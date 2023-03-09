{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Timer.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Generics.Collections, System.Messaging, Winapi.Windows, FMX.Types;

type
  TWinTimerService = class(TInterfacedObject, IFMXTimerService)
  private
    class var FSingleton: TWinTimerService;
  private type
    TTimerInfo = record
      ID: UIntPtr; // the Windows timer ID for this timer
      Handle: TFmxHandle; // the unique FMX Handle for this timer
      Func: TTimerProc; // owner function to handle timer
    end;
  private
    FHandleCounter: TFmxHandle;
    FTimers: TList<TTimerInfo>;
    FPerformanceFrequency: Int64;
    FTerminating: Boolean;
    procedure DestroyTimers;
    { Handlers }
    class procedure TimerCallback(window_hwnd: HWND; Msg: Longint; idEvent: UINT; dwTime: Longint); static; stdcall;
    procedure ApplicationTerminatingHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXTimerService }
    function CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    function GetTick: Double;
  end;

implementation

uses
  System.SysUtils, System.SyncObjs, Winapi.MMSystem, FMX.Platform, FMX.Forms, FMX.Consts;

{ TWinTimerService }

procedure TWinTimerService.ApplicationTerminatingHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  FTerminating := True;
  DestroyTimers;
end;

constructor TWinTimerService.Create;
begin
  // This service uses static TimerCallback, which can be linked only to one instance of service.
  // Therefore we forbid creating two instances of the class.
  if FSingleton = nil then
    FSingleton := Self
  else
    raise Exception.Create('Only one instance of TWinTimerService can exist.');

  inherited;
  FTimers := TList<TTimerInfo>.Create;
  FHandleCounter := 128; // Start counting handles at 128. All valid handles have lower nibble = 0;
  FTerminating := False;
  if not QueryPerformanceFrequency(FPerformanceFrequency) then
    FPerformanceFrequency := 0;

  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationTerminatingMessage, ApplicationTerminatingHandler);
end;

destructor TWinTimerService.Destroy;
begin
  FSingleton := nil;
  TMessageManager.DefaultManager.Unsubscribe(TApplicationTerminatingMessage, ApplicationTerminatingHandler);

  FreeAndNil(FTimers);
  inherited;
end;

function TWinTimerService.CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
var
  TimerInfo: TTimerInfo;
begin
  Result := 0;
  if not FTerminating and (AInterval > 0) and Assigned(ATimerFunc) then
  begin
    TimerInfo.Func := ATimerFunc;
    TimerInfo.ID := Winapi.Windows.SetTimer(0, 0, AInterval, @TimerCallback);
    if TimerInfo.ID <> 0 then
    begin
{$IFDEF CPUX64}
      TimerInfo.Handle := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ENDIF}
{$IFDEF CPUX86}
      TimerInfo.Handle := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
      FTimers.Add(TimerInfo);
      Result := TimerInfo.Handle;
    end
    else
      raise Exception.CreateFmt(SCannotCreateTimer, [GetLastError]);
  end;
end;

function TWinTimerService.DestroyTimer(Timer: TFmxHandle): Boolean;
var
  Index: Integer;
  TimerInfo: TTimerInfo;
begin
  Result := False;
  Index := FTimers.Count;
  while (Index > 0) do
  begin
    Dec(Index);
    TimerInfo := FTimers[Index];
    if TimerInfo.Handle = Timer then
    begin
      Result := Winapi.Windows.KillTimer(0, TimerInfo.ID);
      FTimers.Delete(Index);
    end;
  end;
end;

procedure TWinTimerService.DestroyTimers;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
    try
      DestroyTimer(FTimers[I].Handle);
    except
    end;
end;

function TWinTimerService.GetTick: Double;
var
  PerformanceCounter: Int64;
begin
  if FPerformanceFrequency = 0 then
    Result := timeGetTime / MSecsPerSec
  else
  begin
    QueryPerformanceCounter(PerformanceCounter);
    Result := PerformanceCounter / FPerformanceFrequency;
  end;
end;

class procedure TWinTimerService.TimerCallback(window_hwnd: HWND; Msg: Longint; idEvent: UINT; dwTime: Longint);
var
  Index: Integer;
begin
  if FSingleton = nil then
    Exit;

  try
    Index := FSingleton.FTimers.Count;
    while (Index > 0) do
    begin
      Dec(Index);
      if FSingleton.FTimers[Index].ID = idEvent then
      begin
        FSingleton.FTimers[Index].Func;
        Break;
      end;
    end;
  except
    on E: Exception do
    begin
      if Application <> nil then
        Application.HandleException(nil)
      else
        raise;
    end;
  end;
end;

end.
