{*******************************************************}
{                                                       }
{            CodeGear Delphi Runtime Library            }
{                                                       }
{     Notification Center implementation for MacOS      }
{                                                       }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Mac.Notification;

interface

{$SCOPEDENUMS ON}

uses
  System.Notification;

  /// <summary>Common ancestor used to instantiate platform implementation</summary>
  type TPlatformNotificationCenter = class(TBaseNotificationCenter)
  protected
    class function GetInstance: TBaseNotificationCenter; override;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  System.Messaging,
  System.SysConst,
  Macapi.Foundation,
  Macapi.ObjectiveC,
  Macapi.Helpers,
  Macapi.AppKit,
  Macapi.ObjCRuntime,
  Macapi.UserNotifications;

type

{ TNotificationCenterCocoa }

  TNotificationCenterDelegate = class;

  TNotificationCenterCocoa = class(TPlatformNotificationCenter)
  private type
    TPermissionState = (Unknown, Granted, Denied);
  private
    class var FNotificationCenterSingleton: TNotificationCenterCocoa;
    FNotificationCenterDelegate: TNotificationCenterDelegate;
    FPermissionsState: TPermissionState;
    procedure AddNotificationRequestHandler(error: NSError);
    procedure CheckAuthorization;
    function ConvertNativeToDelphiNotification(const ANotification: UNNotification): TNotification;
    function CreateNotificationRequest(const ANotification: TNotification; const AIsScheduled: Boolean): UNNotificationRequest;
    function GetNotificationSound(const ANotification: TNotification): UNNotificationSound;
    function GetNotificationTrigger(const ANotification: TNotification): UNNotificationTrigger;
    function RepeatIntervalToNSDateComponents(const ADate: TDate; const ARepeatInterval: TRepeatInterval): NSDateComponents;
    procedure RequestAuthorizationHandler(granted: Boolean; error: NSError);
    class destructor Destroy;
    class function GetNotificationCenter: TNotificationCenterCocoa; static;
  protected
    function DoAuthorizationStatus: TAuthorizationStatus; override;
    procedure DoRequestPermission; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReceiveNotification(const ANotification: UNNotification);
    procedure DoScheduleNotification(const ANotification: TNotification); override;
    procedure DoPresentNotification(const ANotification: TNotification); override;
    procedure DoCancelNotification(const AName: string); overload; override;
    procedure DoCancelNotification(const ANotification: TNotification); overload; override;
    procedure DoCancelAllNotifications; override;
    procedure DoSetIconBadgeNumber(const ACount: Integer); override;
    function DoGetIconBadgeNumber: Integer; override;
    procedure DoResetIconBadgeNumber; override;

    class property NotificationCenter: TNotificationCenterCocoa read GetNotificationCenter;
  end;

{ Notification Center Delegate }

  TNotificationCenterDelegate = class(TOCLocal, UNUserNotificationCenterDelegate)
  strict private
    FNotificationCenter: TNotificationCenterCocoa;
    procedure ProcessLocalNotification(notification: UNNotification);
    procedure ProcessNotification(notification: UNNotification);
  public
    { UNUserNotificationCenterDelegate }
    [MethodName('userNotificationCenter:openSettingsForNotification:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse;
      completionHandler: Pointer); overload; cdecl;
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification;
      completionHandler: Pointer); overload; cdecl;
  public
    constructor Create(ANotificationCenter: TNotificationCenterCocoa);
  end;

var
  FUserNotificationCenter: UNUserNotificationCenter;

function UserNotificationCenter: UNUserNotificationCenter;
begin
  if FUserNotificationCenter = nil then
    FUserNotificationCenter := TUNUserNotificationCenter.OCClass.currentNotificationCenter;
  Result := FUserNotificationCenter;
end;

{ TNotificationCenterCocoa }

function TNotificationCenterCocoa.DoAuthorizationStatus: TAuthorizationStatus;
begin
  case FPermissionsState of
    TPermissionState.Granted:
      Result := TAuthorizationStatus.Authorized;
    TPermissionState.Denied:
      Result := TAuthorizationStatus.Denied;
  else
    Result := TAuthorizationStatus.NotDetermined;
  end;
end;

procedure TNotificationCenterCocoa.DoCancelAllNotifications;
begin
  UserNotificationCenter.removeAllPendingNotificationRequests;
  UserNotificationCenter.removeAllDeliveredNotifications;
end;

procedure TNotificationCenterCocoa.DoCancelNotification(const AName: string);
var
  LIdentifiers: NSMutableArray;
begin
  if not AName.IsEmpty then
  begin
    LIdentifiers := TNSMutableArray.Wrap(TNSMutableArray.OCClass.arrayWithObject(StringToId(AName)));
    UserNotificationCenter.removeDeliveredNotificationsWithIdentifiers(LIdentifiers);
    UserNotificationCenter.removePendingNotificationRequestsWithIdentifiers(LIdentifiers);
  end;
end;

procedure TNotificationCenterCocoa.DoCancelNotification(const ANotification: TNotification);
begin
  if ANotification <> nil then
    DoCancelNotification(ANotification.Name);
end;

constructor TNotificationCenterCocoa.Create;
begin
  inherited;
  FNotificationCenterDelegate := TNotificationCenterDelegate.Create(Self);
  UserNotificationCenter.setDelegate(FNotificationCenterDelegate.GetObjectID);
  FPermissionsState := TPermissionState.Unknown;
end;

destructor TNotificationCenterCocoa.Destroy;
begin
  FNotificationCenterDelegate.Free;
  inherited Destroy;
end;

class destructor TNotificationCenterCocoa.Destroy;
begin
  FNotificationCenterSingleton.Free;
end;

function TNotificationCenterCocoa.GetNotificationSound(const ANotification: TNotification): UNNotificationSound;
begin
  if not ANotification.EnableSound then
    Exit(nil);

  if ANotification.SoundName.IsEmpty then
    Result := TUNNotificationSound.OCClass.defaultSound
  else
    Result := TUNNotificationSound.OCClass.soundNamed(StrToNSStr(ANotification.SoundName));
end;

function TNotificationCenterCocoa.GetNotificationTrigger(const ANotification: TNotification): UNNotificationTrigger;
var
  LDateComponents: NSDateComponents;
begin
  LDateComponents := RepeatIntervalToNSDateComponents(ANotification.FireDate, ANotification.RepeatInterval);
  LDateComponents.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.defaultTimeZone));
  Result := TUNCalendarNotificationTrigger.OCClass.triggerWithDateMatchingComponents(LDateComponents, ANotification.RepeatInterval <> TRepeatInterval.None);
end;

function TNotificationCenterCocoa.CreateNotificationRequest(const ANotification: TNotification; const AIsScheduled: Boolean): UNNotificationRequest;
var
  LNotificationContent: UNMutableNotificationContent;
  LTrigger: UNNotificationTrigger;
  LId: string;
begin
  LNotificationContent := TUNMutableNotificationContent.Create;
  LNotificationContent.setTitle(StrToNSStr(ANotification.Title));
  LNotificationContent.setBody(StrToNSStr(ANotification.AlertBody));
  LNotificationContent.setBadge(TNSNumber.Wrap(TNSNumber.OCClass.numberWithInt(ANotification.Number)));
  LNotificationContent.setSound(GetNotificationSound(ANotification));
  if AIsScheduled then
    LTrigger := GetNotificationTrigger(ANotification)
  else
    LTrigger := nil;
  if ANotification.Name.IsEmpty then
    LId := DateTimeToStr(Now)
  else
    LId := ANotification.Name;
  Result := TUNNotificationRequest.OCClass.requestWithIdentifier(StrToNSStr(LId), LNotificationContent, LTrigger);
end;

procedure TNotificationCenterCocoa.CheckAuthorization;
begin
  case AuthorizationStatus of
    TAuthorizationStatus.NotDetermined:
      raise ELocalNotificationAccess.CreateFmt(SNotificationCenterCannotPerformOperation, ['RequestPermission']); // Do not localize
    TAuthorizationStatus.Denied:
      raise ELocalNotificationAccess.CreateFmt(SUserRejectedAccess, ['NotificationCenter']); // Do not localize
  end;
end;

function TNotificationCenterCocoa.ConvertNativeToDelphiNotification(const ANotification: UNNotification): TNotification;
var
  LRequest: UNNotificationRequest;
begin
  LRequest := ANotification.request;
  Result := TNotification.Create;
  Result.Name := NSStrToStr(LRequest.identifier);
  Result.Title := NSStrToStr(LRequest.content.title);
  Result.AlertBody := NSStrToStr(LRequest.content.body);
  if LRequest.content.badge = nil then
    Result.Number := 0
  else
    Result.Number := LRequest.content.badge.intValue;
  Result.EnableSound := LRequest.content.sound <> nil;
  Result.FireDate := NSDateToDateTime(ANotification.date);
  Result.RepeatInterval := TRepeatInterval.None;
end;

procedure TNotificationCenterCocoa.DoPresentNotification(const ANotification: TNotification);
var
  LNotificationRequest: UNNotificationRequest;
begin
  CheckAuthorization;
  LNotificationRequest := CreateNotificationRequest(ANotification, False);
  UserNotificationCenter.addNotificationRequest(LNotificationRequest, AddNotificationRequestHandler);
end;

procedure TNotificationCenterCocoa.DoScheduleNotification(const ANotification: TNotification);
var
  LNotificationRequest: UNNotificationRequest;
begin
  CheckAuthorization;
  DoCancelNotification(ANotification);
  LNotificationRequest := CreateNotificationRequest(ANotification, True);
  UserNotificationCenter.addNotificationRequest(LNotificationRequest, AddNotificationRequestHandler);
end;

procedure TNotificationCenterCocoa.AddNotificationRequestHandler(error: NSError);
begin
  if error <> nil then
    NSLog(NSObjectToID(error.localizedDescription));
end;

procedure TNotificationCenterCocoa.ReceiveNotification(const ANotification: UNNotification);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(ConvertNativeToDelphiNotification(ANotification)));
end;

procedure TNotificationCenterCocoa.DoSetIconBadgeNumber(const ACount: Integer);
var
  NSApp: NSApplication;
begin
  if ACount > 0 then
  begin
    NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
    NSApp.dockTile.setBadgeLabel(StrToNSStr(ACount.ToString));
  end
  else
    DoResetIconBadgeNumber;
end;

function TNotificationCenterCocoa.DoGetIconBadgeNumber: Integer;
var
  NSApp: NSApplication;
  BadgeLabel: string;
begin
  NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
  if NSApp.dockTile.badgeLabel <> nil then
  begin
    BadgeLabel := UTF8ToString(NSApp.dockTile.badgeLabel.UTF8String);
    if not TryStrToInt(BadgeLabel, Result) then
      Result := 0;
  end
  else
    Result := 0;
end;

class function TNotificationCenterCocoa.GetNotificationCenter: TNotificationCenterCocoa;
begin
  if FNotificationCenterSingleton = nil then
    FNotificationCenterSingleton := TNotificationCenterCocoa.Create;
  Result := FNotificationCenterSingleton;
end;

procedure TNotificationCenterCocoa.DoRequestPermission;
begin
  UserNotificationCenter.requestAuthorizationWithOptions(UNAuthorizationOptionBadge or UNAuthorizationOptionSound or UNAuthorizationOptionAlert,
    RequestAuthorizationHandler);
end;

procedure TNotificationCenterCocoa.DoResetIconBadgeNumber;
var
  NSApp: NSApplication;
begin
//  Log.d('XXXX DoResetIconBadgeNumber');
  NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
  NSApp.dockTile.setBadgeLabel(nil);
end;

function TNotificationCenterCocoa.RepeatIntervalToNSDateComponents(const ADate: TDate;
  const ARepeatInterval: TRepeatInterval): NSDateComponents;
begin
  Result := TNSDateComponents.Create;
  case ARepeatInterval of
    TRepeatInterval.None:
    begin
      Result.setNanosecond(MilliSecondOf(ADate));
      Result.setSecond(SecondOf(ADate));
      Result.setMinute(MinuteOf(ADate));
      Result.setHour(HourOf(ADate));
      Result.setDay(DayOf(ADate));
      Result.setMonth(MonthOf(ADate));
      Result.setYear(YearOf(ADate));
    end;
    TRepeatInterval.Second:
      Result.setNanosecond(MilliSecondOf(ADate));
    TRepeatInterval.Minute:
    begin
      Result.setNanosecond(MilliSecondOf(ADate));
      Result.setSecond(SecondOf(ADate));
    end;
    TRepeatInterval.Hour:
    begin
      Result.setNanosecond(MilliSecondOf(ADate));
      Result.setSecond(SecondOf(ADate));
      Result.setMinute(MinuteOf(ADate));
    end;
    TRepeatInterval.Day:
    begin
      Result.setNanosecond(MilliSecondOf(ADate));
      Result.setSecond(SecondOf(ADate));
      Result.setMinute(MinuteOf(ADate));
      Result.setHour(HourOf(ADate));
    end;
    TRepeatInterval.Week:
    begin
      Result.setNanosecond(MilliSecondOf(ADate));
      Result.setSecond(SecondOf(ADate));
      Result.setMinute(MinuteOf(ADate));
      Result.setHour(HourOf(ADate));
      Result.setWeekday(DayOfTheWeek(ADate));
    end;
    TRepeatInterval.Month:
    begin
      Result.setNanosecond(MilliSecondOf(ADate));
      Result.setSecond(SecondOf(ADate));
      Result.setMinute(MinuteOf(ADate));
      Result.setHour(HourOf(ADate));
      Result.setDay(DayOf(ADate));
    end;
    TRepeatInterval.Year:
    begin
      Result.setNanosecond(MilliSecondOf(ADate));
      Result.setSecond(SecondOf(ADate));
      Result.setMinute(MinuteOf(ADate));
      Result.setHour(HourOf(ADate));
      Result.setDay(DayOf(ADate));
      Result.setMonth(MonthOf(ADate));
    end;
    TRepeatInterval.Weekday:
      raise ELocalNotification.CreateFmt(SNotificationCenterFeatureIsNotSupported, ['TRepeatInterval.Weekday']);
    TRepeatInterval.Era:
      raise ELocalNotification.CreateFmt(SNotificationCenterFeatureIsNotSupported, ['TRepeatInterval.Era']);
    TRepeatInterval.Quarter:
      raise ELocalNotification.CreateFmt(SNotificationCenterFeatureIsNotSupported, ['TRepeatInterval.Quarter']);
  end;
end;

procedure TNotificationCenterCocoa.RequestAuthorizationHandler(granted: Boolean; error: NSError);
var
  LError: string;
begin
  if error <> nil then
    LError := NSStrToStr(error.localizedDescription);
  if granted then
    FPermissionsState := TPermissionState.Granted
  else
    FPermissionsState := TPermissionState.Denied;
  NotifyPermissionRequestResult(granted);
end;

{ TNotificationCenterDelegate }

constructor TNotificationCenterDelegate.Create(ANotificationCenter: TNotificationCenterCocoa);
begin
  inherited Create;
  FNotificationCenter := ANotificationCenter;
end;

procedure TNotificationCenterDelegate.ProcessLocalNotification(notification: UNNotification);
var
  LNotification: TNotification;
  LRequest: UNNotificationRequest;
  LRepeatInterval: Pointer;
begin
  LRequest := notification.request;
  LNotification := TNotification.Create;
  LNotification.Name := NSStrToStr(LRequest.identifier);
  LNotification.AlertBody := NSStrToStr(LRequest.content.body);
  LNotification.Title := NSStrToStr(LRequest.content.title);
  LNotification.EnableSound := LRequest.content.sound <> nil;
  LNotification.HasAction := LRequest.content.categoryIdentifier <> nil;
  if LRequest.content.badge = nil then
    LNotification.Number := 0
  else
    LNotification.Number := LRequest.content.badge.intValue;
  LNotification.FireDate := NSDateToDateTime(notification.date);
  LRepeatInterval := LRequest.content.userInfo.objectForKey(StringToID('RepeatInterval')); // Do not localize
  if LRepeatInterval <> nil then
    LNotification.RepeatInterval := TRepeatInterval(TNSNumber.Wrap(LRepeatInterval).intValue)
  else
    LNotification.RepeatInterval := TRepeatInterval.None;
  TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(LNotification));
end;

procedure TNotificationCenterDelegate.ProcessNotification(notification: UNNotification);
begin
  // Allow push implementation (if/when it is implemented) to process the notification
  if (notification.request.trigger <> nil) and notification.request.trigger.isKindOfClass(objc_getClass('UNPushNotificationTrigger')) then // Do not localize
    TMessageManager.DefaultManager.SendMessage(Self, TMessage<UNNotification>.Create(notification))
  else
    ProcessLocalNotification(notification);
end;

procedure TNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse;
  completionHandler: Pointer);
var
  LBlockImp: procedure; cdecl;
begin
  ProcessNotification(response.notification);
  @LBlockImp := imp_implementationWithBlock(completionHandler);
  LBlockImp;
  imp_removeBlock(@LBlockImp);
end;

procedure TNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification);
begin
  // Not implemented
end;

procedure TNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification;
  completionHandler: Pointer);
var
  LBlockImp: procedure(options: UNNotificationPresentationOptions); cdecl;
  LOptions: UNNotificationPresentationOptions;
begin
  ProcessNotification(notification);
  @LBlockImp := imp_implementationWithBlock(completionHandler);
  LOptions := UNNotificationPresentationOptionAlert;
  LBlockImp(LOptions);
  imp_removeBlock(@LBlockImp);
end;

{ TPlatformNotificationCenter }

class function TPlatformNotificationCenter.GetInstance: TBaseNotificationCenter;
begin
  if TOSVersion.Check(10, 8) then
    Result := TBaseNotificationCenter(TNotificationCenterCocoa.NotificationCenter)
  else
    Result := nil;
end;

end.
