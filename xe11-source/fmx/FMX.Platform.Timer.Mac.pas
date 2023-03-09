{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Timer.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo, System.Generics.Collections, System.Messaging, Macapi.ObjectiveC, Macapi.Foundation, FMX.Types;

type
  TMacTimerService = class(TInterfacedObject, IFMXTimerService)
  private
    FHandleCounter: TFmxHandle;
    FHandles: TDictionary<TFmxHandle, IObjectiveC>;
    FTimers: TList<TFmxHandle>;
    FTerminating: Boolean;
    { Handlers }
    procedure ApplicationTerminatingHandler(const Sender: TObject; const Msg: TMessage);
  protected
    function AllocHandle(const Objc: IObjectiveC): TFmxHandle;
    procedure DeleteHandle(const AHandle: TFmxHandle);
    function HandleToObjC(const AHandle: TFmxHandle; const IID: TGUID; out Intf): Boolean; overload;
    function HandleToObjC(const AHandle: TFmxHandle): IObjectiveC; overload;
    procedure ValidateHandle(const AHandle: TFmxHandle);
    function NewFmxHandle: TFmxHandle;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXTimerService }
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    procedure DestroyTimers;
    function GetTick: Double;
  end;

  MacTimer = interface(NSObject)
    ['{337887FF-BA77-4703-BE0E-34DC1CB26276}']
    procedure onTimer; cdecl;
                                                                                  
    procedure release; cdecl;
  end;

  TMacTimer = class(TOCLocal)
  private
    FOnTimer: TTimerProc;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AOnTimer: TTimerProc);

    { MacTimer }
    procedure onTimer; cdecl;
    procedure release; cdecl;
  end;

implementation

uses
  System.SyncObjs, System.SysUtils, Macapi.CocoaTypes, Macapi.ObjCRuntime, Macapi.Mach, FMX.Consts, FMX.Platform,
  FMX.Helpers.Mac, FMX.Forms;

{ TMacTimerService }

function TMacTimerService.AllocHandle(const Objc: IObjectiveC): TFmxHandle;
begin
  Result := NewFmxHandle;
  TMonitor.Enter(FHandles);
  try
    FHandles.Add(Result, Objc);
  finally
    TMonitor.Exit(FHandles);
  end;
end;

procedure TMacTimerService.ApplicationTerminatingHandler(const Sender: TObject; const Msg: TMessage);
begin
  FTerminating := True;
end;

constructor TMacTimerService.Create;
begin
  inherited;
  FTimers := TList<TFmxHandle>.Create;
  FHandles := TDictionary<TFmxHandle, IObjectiveC>.Create;

  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationTerminatingMessage, ApplicationTerminatingHandler);
end;

function TMacTimerService.CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
var
  Timer: NSTimer;
  User: TMacTimer;
  LInterval: NSTimeInterval;
begin
  Result := 0;
  if not FTerminating and (Interval > 0) and Assigned(TimerFunc) then
  begin
    User := TMacTimer.Create(TimerFunc);
    try
      LInterval := Interval / 1000;

      Timer := TNSTimer.Wrap(TNSTimer.OCClass.scheduledTimerWithTimeInterval(LInterval, User.GetObjectID,
        sel_getUid('onTimer'), User.GetObjectID, True));

      TNSRunloop.Wrap(TNSRunLoop.OCClass.mainRunLoop).addTimer(Timer, NSRunLoopCommonModes);
      Result := AllocHandle(Timer);
      FTimers.Add(Result);
    finally
      // User is retained twice (because it's target) by the timer and released twice on timer invalidation.
      NSObject(User.Super).release;
    end;
  end;
end;

procedure TMacTimerService.DeleteHandle(const AHandle: TFmxHandle);
var
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    ValidateHandle(AHandle);
    TMonitor.Enter(FHandles);
    try
      FHandles.Remove(AHandle);
    finally
      TMonitor.Exit(FHandles);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

destructor TMacTimerService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationTerminatingMessage, ApplicationTerminatingHandler);

  DestroyTimers;
  FreeAndNil(FTimers);
  FreeAndNil(FHandles);
  inherited;
end;

function TMacTimerService.DestroyTimer(Timer: TFmxHandle): Boolean;
var
  CocoaTimer: NSTimer;
  I: Integer;
begin
  Result := False;
  if HandleToObjC(Timer, NSTimer, CocoaTimer) then
  begin
    Result := True;
    CocoaTimer.invalidate;
    DeleteHandle(Timer);
    for I := FTimers.Count - 1 downto 0 do
      if FTimers[I] = Timer then
      begin
        FTimers.Delete(I);
        Break;
      end;
  end;
end;

procedure TMacTimerService.DestroyTimers;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
  try
    DestroyTimer(FTimers[I]);
  except
    Continue;
  end;
end;

function TMacTimerService.GetTick: Double;
const
  NanoToSeconds = 1E-9;
begin
  Result := AbsoluteToNanoseconds(mach_absolute_time) * NanoToSeconds;
end;

function TMacTimerService.HandleToObjC(const AHandle: TFmxHandle): IObjectiveC;
begin
  TMonitor.Enter(FHandles);
  try
    ValidateHandle(AHandle);
    if FHandles.ContainsKey(AHandle) then
      Result := FHandles[AHandle]
    else
      Result := nil;
  finally
    TMonitor.Exit(FHandles);
  end;
end;

function TMacTimerService.HandleToObjC(const AHandle: TFmxHandle; const IID: TGUID; out Intf): Boolean;
begin
  Result := Supports(HandleToObjC(AHandle), IID, Intf);
end;

function TMacTimerService.NewFmxHandle: TFmxHandle;
begin
{$IF defined(CPU64BITS)}
  Result := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ELSE}
  Result := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
end;

procedure TMacTimerService.ValidateHandle(const AHandle: TFmxHandle);
begin
  if AHandle and $F <> 0 then
    raise EInvalidFmxHandle.CreateResFmt(@SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(TFmxHandle) * 2, AHandle]);
end;

{ TMacTimer }

constructor TMacTimer.Create(const AOnTimer: TTimerProc);
begin
  inherited Create;
  FOnTimer := AOnTimer;
end;

function TMacTimer.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(MacTimer);
end;

procedure TMacTimer.onTimer;
begin
  if Assigned(@FOnTimer) then
  try
    FOnTimer;
  except
    HandleException(nil);
  end;
end;

procedure TMacTimer.release;
var
  RC: NSUInteger;
begin
  RC := NSObject(Super).retainCount;
  NSObject(Super).release;
  if RC = 1 then
    Destroy;
end;

end.
