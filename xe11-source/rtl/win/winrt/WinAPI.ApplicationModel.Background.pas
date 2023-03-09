{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.ApplicationModel.Background;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.CommonTypes, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.ApplicationModel.Background.IBackgroundTaskProgressEventArgs
  IBackgroundTaskProgressEventArgs = interface;
  PIBackgroundTaskProgressEventArgs = ^IBackgroundTaskProgressEventArgs;

  // Windows.ApplicationModel.Background.BackgroundTaskProgressEventHandler
  BackgroundTaskProgressEventHandler = interface;
  PBackgroundTaskProgressEventHandler = ^BackgroundTaskProgressEventHandler;

  // Windows.ApplicationModel.Background.IBackgroundTaskCompletedEventArgs
  IBackgroundTaskCompletedEventArgs = interface;
  PIBackgroundTaskCompletedEventArgs = ^IBackgroundTaskCompletedEventArgs;

  // Windows.ApplicationModel.Background.BackgroundTaskCompletedEventHandler
  BackgroundTaskCompletedEventHandler = interface;
  PBackgroundTaskCompletedEventHandler = ^BackgroundTaskCompletedEventHandler;

  // Windows.ApplicationModel.Background.IBackgroundTaskRegistration
  IBackgroundTaskRegistration = interface;
  PIBackgroundTaskRegistration = ^IBackgroundTaskRegistration;

  // Windows.ApplicationModel.Background.BackgroundTaskCanceledEventHandler
  BackgroundTaskCanceledEventHandler = interface;
  PBackgroundTaskCanceledEventHandler = ^BackgroundTaskCanceledEventHandler;

  // Windows.ApplicationModel.Background.IBackgroundTaskDeferral
  IBackgroundTaskDeferral = interface;
  PIBackgroundTaskDeferral = ^IBackgroundTaskDeferral;

  // Windows.ApplicationModel.Background.IBackgroundTaskInstance
  IBackgroundTaskInstance = interface;
  PIBackgroundTaskInstance = ^IBackgroundTaskInstance;

  // Windows.ApplicationModel.Background.IBackgroundTrigger
  IBackgroundTrigger = interface;
  PIBackgroundTrigger = ^IBackgroundTrigger;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.AlarmAccessStatus>
  AsyncOperationCompletedHandler_1__AlarmAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__AlarmAccessStatus = ^AsyncOperationCompletedHandler_1__AlarmAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.AlarmAccessStatus>
  IAsyncOperation_1__AlarmAccessStatus = interface;
  PIAsyncOperation_1__AlarmAccessStatus = ^IAsyncOperation_1__AlarmAccessStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.ApplicationTriggerResult>
  AsyncOperationCompletedHandler_1__ApplicationTriggerResult = interface;
  PAsyncOperationCompletedHandler_1__ApplicationTriggerResult = ^AsyncOperationCompletedHandler_1__ApplicationTriggerResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.ApplicationTriggerResult>
  IAsyncOperation_1__ApplicationTriggerResult = interface;
  PIAsyncOperation_1__ApplicationTriggerResult = ^IAsyncOperation_1__ApplicationTriggerResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.BackgroundAccessStatus>
  AsyncOperationCompletedHandler_1__BackgroundAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__BackgroundAccessStatus = ^AsyncOperationCompletedHandler_1__BackgroundAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.BackgroundAccessStatus>
  IAsyncOperation_1__BackgroundAccessStatus = interface;
  PIAsyncOperation_1__BackgroundAccessStatus = ^IAsyncOperation_1__BackgroundAccessStatus;

  // Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>
  IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = interface;
  PIKeyValuePair_2__TGuid__IBackgroundTaskRegistration = ^IKeyValuePair_2__TGuid__IBackgroundTaskRegistration;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>>
  IIterator_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = interface;
  PIIterator_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = ^IIterator_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>>
  IIterable_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = interface;
  PIIterable_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = ^IIterable_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration;

  // Windows.Foundation.Collections.IMapView`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>
  IMapView_2__TGuid__IBackgroundTaskRegistration = interface;
  PIMapView_2__TGuid__IBackgroundTaskRegistration = ^IMapView_2__TGuid__IBackgroundTaskRegistration;

  // Windows.ApplicationModel.Background.IBackgroundTaskInstance2
  IBackgroundTaskInstance2 = interface;
  PIBackgroundTaskInstance2 = ^IBackgroundTaskInstance2;

  // Windows.ApplicationModel.Background.IBackgroundTaskInstance4
  IBackgroundTaskInstance4 = interface;
  PIBackgroundTaskInstance4 = ^IBackgroundTaskInstance4;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.DeviceTriggerResult>
  AsyncOperationCompletedHandler_1__DeviceTriggerResult = interface;
  PAsyncOperationCompletedHandler_1__DeviceTriggerResult = ^AsyncOperationCompletedHandler_1__DeviceTriggerResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.DeviceTriggerResult>
  IAsyncOperation_1__DeviceTriggerResult = interface;
  PIAsyncOperation_1__DeviceTriggerResult = ^IAsyncOperation_1__DeviceTriggerResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.MediaProcessingTriggerResult>
  AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult = interface;
  PAsyncOperationCompletedHandler_1__MediaProcessingTriggerResult = ^AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.MediaProcessingTriggerResult>
  IAsyncOperation_1__MediaProcessingTriggerResult = interface;
  PIAsyncOperation_1__MediaProcessingTriggerResult = ^IAsyncOperation_1__MediaProcessingTriggerResult;

  // Windows.ApplicationModel.Background Enums

  // Windows.ApplicationModel.Background.AlarmAccessStatus
  AlarmAccessStatus = (
    Unspecified = 0,
    AllowedWithWakeupCapability = 1,
    AllowedWithoutWakeupCapability = 2,
    Denied = 3
  );
  PAlarmAccessStatus = ^AlarmAccessStatus;

  // Windows.ApplicationModel.Background.ApplicationTriggerResult
  ApplicationTriggerResult = (
    Allowed = 0,
    CurrentlyRunning = 1,
    DisabledByPolicy = 2,
    UnknownError = 3
  );
  PApplicationTriggerResult = ^ApplicationTriggerResult;

  // Windows.ApplicationModel.Background.BackgroundAccessRequestKind
  BackgroundAccessRequestKind = (
    AlwaysAllowed = 0,
    AllowedSubjectToSystemPolicy = 1
  );
  PBackgroundAccessRequestKind = ^BackgroundAccessRequestKind;

  // Windows.ApplicationModel.Background.BackgroundAccessStatus
  BackgroundAccessStatus = (
    Unspecified = 0,
    AllowedWithAlwaysOnRealTimeConnectivity = 1,
    AllowedMayUseActiveRealTimeConnectivity = 2,
    Denied = 3,
    AlwaysAllowed = 4,
    AllowedSubjectToSystemPolicy = 5,
    DeniedBySystemPolicy = 6,
    DeniedByUser = 7
  );
  PBackgroundAccessStatus = ^BackgroundAccessStatus;

  // Windows.ApplicationModel.Background.BackgroundTaskCancellationReason
  BackgroundTaskCancellationReason = (
    Abort = 0,
    Terminating = 1,
    LoggingOff = 2,
    ServicingUpdate = 3,
    IdleTask = 4,
    Uninstall = 5,
    ConditionLoss = 6,
    SystemPolicy = 7,
    QuietHoursEntered = 8,
    ExecutionTimeExceeded = 9,
    ResourceRevocation = 10,
    EnergySaver = 11
  );
  PBackgroundTaskCancellationReason = ^BackgroundTaskCancellationReason;

  // Windows.ApplicationModel.Background.BackgroundTaskThrottleCounter
  BackgroundTaskThrottleCounter = (
    All = 0,
    Cpu = 1,
    Network = 2
  );
  PBackgroundTaskThrottleCounter = ^BackgroundTaskThrottleCounter;

  // Windows.ApplicationModel.Background.BackgroundWorkCostValue
  BackgroundWorkCostValue = (
    Low = 0,
    Medium = 1,
    High = 2
  );
  PBackgroundWorkCostValue = ^BackgroundWorkCostValue;

  // Windows.ApplicationModel.Background.CustomSystemEventTriggerRecurrence
  CustomSystemEventTriggerRecurrence = (
    Once = 0,
    Always = 1
  );
  PCustomSystemEventTriggerRecurrence = ^CustomSystemEventTriggerRecurrence;

  // Windows.ApplicationModel.Background.DeviceTriggerResult
  DeviceTriggerResult = (
    Allowed = 0,
    DeniedByUser = 1,
    DeniedBySystem = 2,
    LowBattery = 3
  );
  PDeviceTriggerResult = ^DeviceTriggerResult;

  // Windows.ApplicationModel.Background.LocationTriggerType
  LocationTriggerType = (
    Geofence = 0
  );
  PLocationTriggerType = ^LocationTriggerType;

  // Windows.ApplicationModel.Background.MediaProcessingTriggerResult
  MediaProcessingTriggerResult = (
    Allowed = 0,
    CurrentlyRunning = 1,
    DisabledByPolicy = 2,
    UnknownError = 3
  );
  PMediaProcessingTriggerResult = ^MediaProcessingTriggerResult;

  // Windows.ApplicationModel.Background.SystemConditionType
  SystemConditionType = (
    Invalid = 0,
    UserPresent = 1,
    UserNotPresent = 2,
    InternetAvailable = 3,
    InternetNotAvailable = 4,
    SessionConnected = 5,
    SessionDisconnected = 6,
    FreeNetworkAvailable = 7,
    BackgroundWorkCostNotHigh = 8
  );
  PSystemConditionType = ^SystemConditionType;

  // Windows.ApplicationModel.Background.SystemTriggerType
  SystemTriggerType = (
    Invalid = 0,
    SmsReceived = 1,
    UserPresent = 2,
    UserAway = 3,
    NetworkStateChange = 4,
    ControlChannelReset = 5,
    InternetAvailable = 6,
    SessionConnected = 7,
    ServicingComplete = 8,
    LockScreenApplicationAdded = 9,
    LockScreenApplicationRemoved = 10,
    TimeZoneChange = 11,
    OnlineIdConnectedStateChange = 12,
    BackgroundWorkCostChange = 13,
    PowerStateChange = 14,
    DefaultSignInAccountChange = 15
  );
  PSystemTriggerType = ^SystemTriggerType;

  // Windows.ApplicationModel.Background Records
  // Windows.ApplicationModel.Background.BackgroundAlarmApplicationContract
  BackgroundAlarmApplicationContract = record
  end;
  PBackgroundAlarmApplicationContract = ^BackgroundAlarmApplicationContract;

  // Windows.ApplicationModel.Background Interfaces

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.IBackgroundTaskProgressEventArgs
  IBackgroundTaskProgressEventArgs = interface(IInspectable)
  ['{FB1468AC-8332-4D0A-9532-03EAE684DA31}']
    function get_InstanceId: TGuid; safecall;
    function get_Progress: Cardinal; safecall;
    property InstanceId: TGuid read get_InstanceId;
    property Progress: Cardinal read get_Progress;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.BackgroundTaskProgressEventHandler
  BackgroundTaskProgressEventHandler = interface(IUnknown)
  ['{46E0683C-8A88-4C99-804C-76897F6277A6}']
    procedure Invoke(sender: IBackgroundTaskRegistration; args: IBackgroundTaskProgressEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.IBackgroundTaskCompletedEventArgs
  IBackgroundTaskCompletedEventArgs = interface(IInspectable)
  ['{565D25CF-F209-48F4-9967-2B184F7BFBF0}']
    function get_InstanceId: TGuid; safecall;
    procedure CheckResult; safecall;
    property InstanceId: TGuid read get_InstanceId;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.BackgroundTaskCompletedEventHandler
  BackgroundTaskCompletedEventHandler = interface(IUnknown)
  ['{5B38E929-A086-46A7-A678-439135822BCF}']
    procedure Invoke(sender: IBackgroundTaskRegistration; args: IBackgroundTaskCompletedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.IBackgroundTaskRegistration
  IBackgroundTaskRegistration = interface(IInspectable)
  ['{10654CC2-A26E-43BF-8C12-1FB40DBFBFA0}']
    function get_TaskId: TGuid; safecall;
    function get_Name: HSTRING; safecall;
    function add_Progress(handler: BackgroundTaskProgressEventHandler): EventRegistrationToken; safecall;
    procedure remove_Progress(cookie: EventRegistrationToken); safecall;
    function add_Completed(handler: BackgroundTaskCompletedEventHandler): EventRegistrationToken; safecall;
    procedure remove_Completed(cookie: EventRegistrationToken); safecall;
    procedure Unregister(cancelTask: Boolean); safecall;
    property Name: HSTRING read get_Name;
    property TaskId: TGuid read get_TaskId;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.BackgroundTaskCanceledEventHandler
  BackgroundTaskCanceledEventHandler = interface(IUnknown)
  ['{A6C4BAC0-51F8-4C57-AC3F-156DD1680C4F}']
    procedure Invoke(sender: IBackgroundTaskInstance; reason: BackgroundTaskCancellationReason); safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.IBackgroundTaskDeferral
  IBackgroundTaskDeferral = interface(IInspectable)
  ['{93CC156D-AF27-4DD3-846E-24EE40CADD25}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Background.IBackgroundTaskInstance
  IBackgroundTaskInstance = interface(IInspectable)
  ['{865BDA7A-21D8-4573-8F32-928A1B0641F6}']
    function get_InstanceId: TGuid; safecall;
    function get_Task: IBackgroundTaskRegistration; safecall;
    function get_Progress: Cardinal; safecall;
    procedure put_Progress(value: Cardinal); safecall;
    function get_TriggerDetails: IInspectable; safecall;
    function add_Canceled(cancelHandler: BackgroundTaskCanceledEventHandler): EventRegistrationToken; safecall;
    procedure remove_Canceled(cookie: EventRegistrationToken); safecall;
    function get_SuspendedCount: Cardinal; safecall;
    function GetDeferral: IBackgroundTaskDeferral; safecall;
    property InstanceId: TGuid read get_InstanceId;
    property Progress: Cardinal read get_Progress write put_Progress;
    property SuspendedCount: Cardinal read get_SuspendedCount;
    property Task: IBackgroundTaskRegistration read get_Task;
    property TriggerDetails: IInspectable read get_TriggerDetails;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.Background.IBackgroundTrigger
  [WinRTClassNameAttribute(SWindows_ApplicationModel_Background_ConversationalAgentTrigger)]
  IBackgroundTrigger = interface(IInspectable)
  ['{84B3A058-6027-4B87-9790-BDF3F757DBD7}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.AlarmAccessStatus>
  AsyncOperationCompletedHandler_1__AlarmAccessStatus_Delegate_Base = interface(IUnknown)
  ['{84108017-A8E7-5449-B713-DF48503A953E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__AlarmAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.AlarmAccessStatus>
  AsyncOperationCompletedHandler_1__AlarmAccessStatus = interface(AsyncOperationCompletedHandler_1__AlarmAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.AlarmAccessStatus>
  IAsyncOperation_1__AlarmAccessStatus_Base = interface(IInspectable)
  ['{A55A747D-59F6-5CB6-B439-C8AAD670905C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__AlarmAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__AlarmAccessStatus; safecall;
    function GetResults: AlarmAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__AlarmAccessStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.AlarmAccessStatus>
  IAsyncOperation_1__AlarmAccessStatus = interface(IAsyncOperation_1__AlarmAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.ApplicationTriggerResult>
  AsyncOperationCompletedHandler_1__ApplicationTriggerResult_Delegate_Base = interface(IUnknown)
  ['{D0065EF6-EE9D-55F8-AC2B-53A91FF96D2E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ApplicationTriggerResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.ApplicationTriggerResult>
  AsyncOperationCompletedHandler_1__ApplicationTriggerResult = interface(AsyncOperationCompletedHandler_1__ApplicationTriggerResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.ApplicationTriggerResult>
  IAsyncOperation_1__ApplicationTriggerResult_Base = interface(IInspectable)
  ['{47CBD985-0F08-5A3D-92CF-B27960506ED6}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ApplicationTriggerResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ApplicationTriggerResult; safecall;
    function GetResults: ApplicationTriggerResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ApplicationTriggerResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.ApplicationTriggerResult>
  IAsyncOperation_1__ApplicationTriggerResult = interface(IAsyncOperation_1__ApplicationTriggerResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.BackgroundAccessStatus>
  AsyncOperationCompletedHandler_1__BackgroundAccessStatus_Delegate_Base = interface(IUnknown)
  ['{26DD26E3-3F47-5709-B2F2-D6D0AD3288F0}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__BackgroundAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.BackgroundAccessStatus>
  AsyncOperationCompletedHandler_1__BackgroundAccessStatus = interface(AsyncOperationCompletedHandler_1__BackgroundAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.BackgroundAccessStatus>
  IAsyncOperation_1__BackgroundAccessStatus_Base = interface(IInspectable)
  ['{7B44E581-CFA9-5763-BED7-6A65739F0DBF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__BackgroundAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__BackgroundAccessStatus; safecall;
    function GetResults: BackgroundAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__BackgroundAccessStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.BackgroundAccessStatus>
  IAsyncOperation_1__BackgroundAccessStatus = interface(IAsyncOperation_1__BackgroundAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>
  IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = interface(IInspectable)
  ['{5A1F6D75-8678-547C-8FD7-FBCEB6EBF968}']
    function get_Key: TGuid; safecall;
    function get_Value: IBackgroundTaskRegistration; safecall;
    property Key: TGuid read get_Key;
    property Value: IBackgroundTaskRegistration read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>>
  IIterator_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration_Base = interface(IInspectable)
  ['{2001AEA5-1A86-517E-8BE5-11D7FB5935B2}']
    function get_Current: IKeyValuePair_2__TGuid__IBackgroundTaskRegistration; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__TGuid__IBackgroundTaskRegistration): Cardinal; safecall;
    property Current: IKeyValuePair_2__TGuid__IBackgroundTaskRegistration read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>>
  IIterator_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = interface(IIterator_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration_Base)
  ['{2001AEA5-1A86-517E-8BE5-11D7FB5935B2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>>
  IIterable_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration_Base = interface(IInspectable)
  ['{62AE0FDA-B238-554F-A275-1DC16D6CA03A}']
    function First: IIterator_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>>
  IIterable_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration = interface(IIterable_1__IKeyValuePair_2__TGuid__IBackgroundTaskRegistration_Base)
  ['{80FB0327-5A00-55CC-85DB-A852719981B9}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>
  IMapView_2__TGuid__IBackgroundTaskRegistration_Base = interface(IInspectable)
  ['{2C08602F-40B1-5E97-AE21-5C04D7FB829C}']
    function Lookup(key: TGuid): IBackgroundTaskRegistration; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: TGuid): Boolean; safecall;
    procedure Split(out first: IMapView_2__TGuid__IBackgroundTaskRegistration; out second: IMapView_2__TGuid__IBackgroundTaskRegistration); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<Guid,Windows.ApplicationModel.Background.IBackgroundTaskRegistration>
  IMapView_2__TGuid__IBackgroundTaskRegistration = interface(IMapView_2__TGuid__IBackgroundTaskRegistration_Base)
  ['{78C880F6-A7DC-5172-89DA-7749FC82AA82}']
  end;

  // Windows.ApplicationModel.Background.IBackgroundTaskInstance2
  IBackgroundTaskInstance2 = interface(IInspectable)
  ['{4F7D0176-0C76-4FB4-896D-5DE1864122F6}']
    function GetThrottleCount(counter: BackgroundTaskThrottleCounter): Cardinal; safecall;
  end;

  // Windows.ApplicationModel.Background.IBackgroundTaskInstance4
  IBackgroundTaskInstance4 = interface(IInspectable)
  ['{7F29F23C-AA04-4B08-97B0-06D874CDABF5}']
    function get_User: IUser; safecall;
    property User: IUser read get_User;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.DeviceTriggerResult>
  AsyncOperationCompletedHandler_1__DeviceTriggerResult_Delegate_Base = interface(IUnknown)
  ['{D5AA9506-1464-57D4-859D-7EE9B26CB1F9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__DeviceTriggerResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.DeviceTriggerResult>
  AsyncOperationCompletedHandler_1__DeviceTriggerResult = interface(AsyncOperationCompletedHandler_1__DeviceTriggerResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.DeviceTriggerResult>
  IAsyncOperation_1__DeviceTriggerResult_Base = interface(IInspectable)
  ['{B5136C46-2F2E-511D-9E8E-5EF4DECB1DA7}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__DeviceTriggerResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__DeviceTriggerResult; safecall;
    function GetResults: DeviceTriggerResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__DeviceTriggerResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.DeviceTriggerResult>
  IAsyncOperation_1__DeviceTriggerResult = interface(IAsyncOperation_1__DeviceTriggerResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.MediaProcessingTriggerResult>
  AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult_Delegate_Base = interface(IUnknown)
  ['{3814C6A5-2AD1-5875-BED5-5031CD1F50A2}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__MediaProcessingTriggerResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Background.MediaProcessingTriggerResult>
  AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult = interface(AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.MediaProcessingTriggerResult>
  IAsyncOperation_1__MediaProcessingTriggerResult_Base = interface(IInspectable)
  ['{2595482C-1CBF-5691-A30D-2164909C6712}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult; safecall;
    function GetResults: MediaProcessingTriggerResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__MediaProcessingTriggerResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Background.MediaProcessingTriggerResult>
  IAsyncOperation_1__MediaProcessingTriggerResult = interface(IAsyncOperation_1__MediaProcessingTriggerResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Windows.ApplicationModel.Background.ConversationalAgentTrigger
  // DualAPI
  // Implements: Windows.ApplicationModel.Background.IBackgroundTrigger
  // Instantiable: "IBackgroundTrigger"
  TConversationalAgentTrigger = class(TWinRTGenericImportI<IBackgroundTrigger>) end;

implementation

{ TConversationalAgentTrigger }

end.
