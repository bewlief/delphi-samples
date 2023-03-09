{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Timer.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Generics.Collections, System.Messaging, FMX.Types;

type
  /// <summary>Implementation of timer service for iOS</summary>
  TiOSTimerService = class(TInterfacedObject, IFMXTimerService)
  private
    FTimers: TList<TFmxHandle>;
    FObjectMap: TDictionary<TFmxHandle, TObject>;
    FHandleCounter: TFmxHandle;
    FTerminating: Boolean;
    procedure ApplicationTerminatingHandler(const Sender: TObject; const Msg: TMessage);
  protected
    /// <summary>Returns new unique handle</summary>
    function NewFmxHandle: TFmxHandle;
    /// <summary>Creates new unique handle and binds it with specified object.</summary>
    /// <remarks>Use <c>DeleteObjectHandle</c> for removing of created binding.</remarks>
    function AllocObjectHandle(const AObject: TObject): TFmxHandle;
    /// <summary>Removes binding specified handle with object, which was created by <c>AllocObjectHandle</c></summary>
    procedure DeleteObjectHandle(const AHandle: TFmxHandle);
    /// <summary>Finds object, which was bound with specified handle</summary>
    function HandleToObject(const AHandle: TFmxHandle): TObject;
    /// <summary>Checks a validity of a <c>AHandle</c></summary>
    procedure ValidateHandle(const AHandle: TFmxHandle);
    /// <summary>Destroys all allocated timers</summary>
    procedure DestroyTimers;
    /// <summary>Registers timer service in platform</summary>
    procedure RegisterService; virtual;
    /// <summary>Unregisters timer service from platform</summary>
    procedure UnregisterService; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXTimerService }
    function CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(ATimer: TFmxHandle): Boolean;
    function GetTick: Double;
  end;
  TCocoaTouchTimerService = TiOSTimerService;

implementation

uses
  System.TypInfo, System.SysUtils, System.SyncObjs, Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.Mach,
  iOSapi.Foundation, iOSapi.CocoaTypes, FMX.Forms, FMX.Platform, FMX.Consts;

type
  iOSTimer = interface(NSObject)
    ['{B65CD0E6-21EA-4E77-BF5E-981C3B0EE632}']
    procedure onTimer; cdecl;
  end;

  TiOSTimer = class(TOCLocal)
  private
    FTimer: NSTimer;
    FOnTimer: TTimerProc;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AInterval: Integer {msec}; const AOnTimer: TTimerProc);
    destructor Destroy; override;
    { iOSTimer }
    procedure onTimer; cdecl;
  end;

{ TiOSTimerService }

function TiOSTimerService.AllocObjectHandle(const AObject: TObject): TFmxHandle;
begin
  Result := NewFmxHandle;
  TMonitor.Enter(FObjectMap);
  try
    FObjectMap.Add(Result, AObject);
  finally
    TMonitor.Exit(FObjectMap);
  end;
end;

procedure TiOSTimerService.ApplicationTerminatingHandler(const Sender: TObject; const Msg: TMessage);
begin
  FTerminating := True;
  DestroyTimers;
end;

constructor TiOSTimerService.Create;
begin
  inherited;
  FTerminating := False;
  FTimers := TList<TFmxHandle>.Create;
  FObjectMap := TDictionary<TFmxHandle, TObject>.Create;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationTerminatingMessage, ApplicationTerminatingHandler);

  RegisterService;
end;

function TiOSTimerService.CreateTimer(AInterval: Integer; ATimerFunc: TTimerProc): TFmxHandle;
var
  Timer: TiOSTimer;
begin
  Result := 0;
  if FTerminating or (AInterval = 0) or not Assigned(ATimerFunc) then
    Exit;

  Timer := TiOSTimer.Create(AInterval, ATimerFunc);
  try
    Result := AllocObjectHandle(Timer);
    FTimers.Add(Result);
  finally
    {user is retained (twice, because it's target), by the timer and }
    {released (twice) on timer invalidation}
    NSObject(Timer.Super).release;
  end;
end;

procedure TiOSTimerService.DeleteObjectHandle(const AHandle: TFmxHandle);
begin
  TMonitor.Enter(FObjectMap);
  try
    ValidateHandle(AHandle);
    FObjectMap.Remove(AHandle);
  finally
    TMonitor.Exit(FObjectMap);
  end;
end;

destructor TiOSTimerService.Destroy;
begin
  UnregisterService;
  TMessageManager.DefaultManager.Unsubscribe(TApplicationTerminatingMessage, ApplicationTerminatingHandler);
  FreeAndNil(FObjectMap);
  FreeAndNil(FTimers);
  inherited;
end;

function TiOSTimerService.DestroyTimer(ATimer: TFmxHandle): Boolean;
var
  User: TiOSTimer;
begin
  Result := False;
  User := TiOSTimer(HandleToObject(ATimer));
  if User <> nil then
  begin
    try
      User.Free;
    finally
      DeleteObjectHandle(ATimer);
      FTimers.Remove(ATimer);
    end;
    Result := True;
  end;
end;

procedure TiOSTimerService.DestroyTimers;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
    try
      DestroyTimer(FTimers[I]);
    except
    end;
  FTimers.Clear;
end;

function TiOSTimerService.GetTick: Double;
const
  NanoToSeconds = 1E-9;
begin
  Result := AbsoluteToNanoseconds(mach_absolute_time) * NanoToSeconds;
end;

function TiOSTimerService.HandleToObject(const AHandle: TFmxHandle): TObject;
begin
  TMonitor.Enter(FObjectMap);
  try
    ValidateHandle(AHandle);
    if FObjectMap.ContainsKey(AHandle) then
      Result := FObjectMap[AHandle]
    else
      Result := nil;
  finally
    TMonitor.Exit(FObjectMap);
  end;
end;

function TiOSTimerService.NewFmxHandle: TFmxHandle;
begin
{$IF defined(CPU64BITS)}
  Result := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ELSE}
  Result := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
end;

procedure TiOSTimerService.RegisterService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService) then
    TPlatformServices.Current.AddPlatformService(IFMXTimerService, Self);
end;

procedure TiOSTimerService.UnregisterService;
begin
  if TPlatformServices.Current <> nil then
    TPlatformServices.Current.RemovePlatformService(IFMXTimerService);
end;

procedure TiOSTimerService.ValidateHandle(const AHandle: TFmxHandle);
begin
  if AHandle and $F <> 0 then
    raise EInvalidFmxHandle.CreateFMT(SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(AHandle) * 2, AHandle]);
end;

constructor TiOSTimer.Create(const AInterval: Integer; const AOnTimer: TTimerProc);
var
  Interval: NSTimeInterval;
begin
  inherited Create;
  FOnTimer := AOnTimer;
  Interval := AInterval / MSecsPerSec;
  FTimer := TNSTimer.Wrap(TNSTimer.OCClass.scheduledTimerWithTimeInterval(Interval, GetObjectID,
                          sel_getUid('onTimer'), GetObjectID, True));
end;

destructor TiOSTimer.Destroy;
begin
  if FTimer <> nil then
  begin
    FTimer.invalidate;
    FTimer := nil;
  end;
  inherited;
end;

function TiOSTimer.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(iOSTimer);
end;

procedure TiOSTimer.onTimer;
begin
  if Assigned(FOnTimer) then
    try
      FOnTimer;
    except
      if Application <> nil then
        Application.HandleException(nil);
    end;
end;

end.
