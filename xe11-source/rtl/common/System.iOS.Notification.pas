{*******************************************************}
{                                                       }
{            CodeGear Delphi Runtime Library            }
{                                                       }
{      Notification Center implementation for iOS       }
{                                                       }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.iOS.Notification;

interface

{$SCOPEDENUMS ON}

uses
  System.Notification;

type
  /// <summary>Common ancestor used to instantiate platform implementation</summary>
  TPlatformNotificationCenter = class(TBaseNotificationCenter)
  protected
    class function GetInstance: TBaseNotificationCenter; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Messaging,
  System.DateUtils,
  System.SysConst,
  Macapi.ObjectiveC,
  Macapi.Helpers,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  iOSapi.UIKit,
  iOSapi.UserNotifications;

type

  TNotificationCenterIOS = class abstract(TPlatformNotificationCenter)
  private
    FDelayedNotifications: TObjectList<TNotification>;
    FIsApplicationLoaded: Boolean;
    { Global External event }
    procedure ReceiveLocalNotification(const Sender: TObject; const M: TMessage);
    { Delayed notifications }
    procedure NotifyDelayedNotifications;
    procedure ClearDelayedNotifications;
    procedure DidFormsLoad;
  protected
    procedure DoLoaded; override;
    { Application Icon Badge Number }
    procedure DoSetIconBadgeNumber(const ACount: Integer); override;
    function DoGetIconBadgeNumber: Integer; override;
    procedure DoResetIconBadgeNumber; override;

    function ConvertNativeToDelphiNotification(const ANotification: Pointer): TNotification; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function SharedApplication: UIApplication; inline;
  end;

{ TNotificationCenterCocoa4 }

  TNotificationCenterCocoa4 = class(TNotificationCenterIOS)
  private
    class var FInstance: TNotificationCenterCocoa4;
    class function GetDefaultInstance: TNotificationCenterCocoa4; static;
    class destructor Destroy;

    { Creation and manipulation with notifications }
    function CreateNativeNotification(const ANotification: TNotification): UILocalNotification;
    function FindNativeNotification(const AID: string; var ANotification: UILocalNotification): Boolean;
  protected
    procedure DoRequestPermission; override;
    function DoAuthorizationStatus: TAuthorizationStatus; override;

    procedure DoScheduleNotification(const ANotification: TNotification); override;
    procedure DoPresentNotification(const ANotification: TNotification); override;
    procedure DoCancelNotification(const AName: string); overload; override;
    procedure DoCancelNotification(const ANotification: TNotification); overload; override;
    procedure DoCancelAllNotifications; override;

    function ConvertNativeToDelphiNotification(const ANotification: Pointer): TNotification; override;
  public
    class property Instance: TNotificationCenterCocoa4 read GetDefaultInstance;
  end;

  TNotificationCenterCocoa10 = class(TNotificationCenterIOS)
  private type
    TPermissionState = (Unknown, Granted, Denied);
  private
    class var FInstance: TNotificationCenterCocoa10;
    class function GetDefaultInstance: TNotificationCenterCocoa10; static;
    class destructor Destroy;
  private
    FPermissionsState: TPermissionState;

    { Creation and manipulation with notifications }
    function CreateNotificationRequest(const ANotification: TNotification; const AIsScheduled: Boolean): UNNotificationRequest;
    function RepeatIntervalToNSDateComponents(const ADate: TDate; const ARepeatInterval: TRepeatInterval): NSDateComponents;
    function GetNotificationSound(const ANotification: TNotification): UNNotificationSound;
    function GetNotificationTrigger(const ANotification: TNotification): UNNotificationTrigger;

    { Handler }
    procedure RequestAuthorizationHandler(granted: Boolean; error: NSError);
    procedure AddNotificationRequestHandler(error: NSError);
  protected
    procedure DoRequestPermission; override;
    function DoAuthorizationStatus: TAuthorizationStatus; override;

    procedure DoScheduleNotification(const ANotification: TNotification); override;
    procedure DoPresentNotification(const ANotification: TNotification); override;
    procedure DoCancelNotification(const AName: string); overload; override;
    procedure DoCancelNotification(const ANotification: TNotification); overload; override;
    procedure DoCancelAllNotifications; override;
    function ConvertNativeToDelphiNotification(const ANotification: Pointer): TNotification; override;
  public
    constructor Create;

    procedure CheckAuthorization;
    class property Instance: TNotificationCenterCocoa10 read GetDefaultInstance;
  end;

function RepeatIntervalToNSCalendarUnit(const AInterval: TRepeatInterval): NSCalendarUnit;
begin
  case AInterval of
    TRepeatInterval.None:
      Result := 0;
    TRepeatInterval.Second:
      Result := NSSecondCalendarUnit;
    TRepeatInterval.Minute:
      Result := NSMinuteCalendarUnit;
    TRepeatInterval.Hour:
      Result := NSHourCalendarUnit;
    TRepeatInterval.Day:
      Result := NSDayCalendarUnit;
    TRepeatInterval.Weekday:
      Result := NSWeekdayCalendarUnit;
    TRepeatInterval.Week:
      Result := NSWeekCalendarUnit;
    TRepeatInterval.Month:
      Result := NSMonthCalendarUnit;
    TRepeatInterval.Quarter:
      Result := NSQuarterCalendarUnit;
    TRepeatInterval.Year:
      Result := NSYearCalendarUnit;
    TRepeatInterval.Era:
      Result := NSEraCalendarUnit;
  else
    Result := 0;
  end;
end;

function NSCalendarUnitToRepeatInterval(const AInterval: NSCalendarUnit): TRepeatInterval;
begin
  case AInterval of
    NSEraCalendarUnit:
      Result := TRepeatInterval.Era;
    NSYearCalendarUnit:
      Result := TRepeatInterval.Year;
    NSQuarterCalendarUnit:
      Result := TRepeatInterval.Quarter;
    NSMonthCalendarUnit:
      Result := TRepeatInterval.Month;
    NSWeekCalendarUnit:
      Result := TRepeatInterval.Week;
    NSWeekdayCalendarUnit:
      Result := TRepeatInterval.Weekday;
    NSDayCalendarUnit:
      Result := TRepeatInterval.Day;
    NSHourCalendarUnit:
      Result := TRepeatInterval.Hour;
    NSMinuteCalendarUnit:
      Result := TRepeatInterval.Minute;
    NSSecondCalendarUnit:
      Result := TRepeatInterval.Second;
  else
    Result := TRepeatInterval.None;
  end;
end;

{$REGION 'TNotificationCenterCocoa4'}

function TNotificationCenterCocoa4.DoAuthorizationStatus: TAuthorizationStatus;
var
  NotificationSettings: UIUserNotificationSettings;
begin
  NotificationSettings := SharedApplication.currentUserNotificationSettings;
  if NotificationSettings.types and UIUserNotificationTypeNone = UIUserNotificationTypeNone then
    Result := TAuthorizationStatus.Denied
  else
    Result := TAuthorizationStatus.Authorized;
end;

procedure TNotificationCenterCocoa4.DoCancelAllNotifications;
begin
  SharedApplication.cancelAllLocalNotifications;
end;

function TNotificationCenterCocoa4.FindNativeNotification(const AID: string; var ANotification: UILocalNotification): Boolean;

  function FindInScheduledNotifications: UILocalNotification;
  var
    Notifications: NSArray;
    NativeNotification: UILocalNotification;
    Found: Boolean;
    I: NSUInteger;
    UserInfo: NSDictionary;
  begin
    Notifications := SharedApplication.scheduledLocalNotifications;
    Found := False;
    if Notifications <> nil then
    begin
      I := 0;
      while (I < Notifications.count) and not Found do
      begin
        NativeNotification := TUILocalNotification.Wrap(Notifications.objectAtIndex(I));
        UserInfo := NativeNotification.userInfo;
        if (UserInfo <> nil) and (UTF8ToString(TNSString.Wrap(UserInfo.valueForKey(StrToNSStr('id'))).UTF8String) = AID) then
          Found := True
        else
          Inc(I);
      end;
    end;
    if Found then
      Result := NativeNotification
    else
      Result := nil;
  end;

begin
  // We are searching notification in two list:
  //   1. Notifications, which have not displayed in Notification Center
  //   2. Notifications, which already displayed
  ANotification := FindInScheduledNotifications;
  Result := ANotification <> nil;
end;

procedure TNotificationCenterCocoa4.DoCancelNotification(const AName: string);
var
  NativeNotification: UILocalNotification;
begin
  if not AName.IsEmpty and FindNativeNotification(AName, NativeNotification) then
    SharedApplication.cancelLocalNotification(NativeNotification);
end;

procedure TNotificationCenterCocoa4.DoCancelNotification(const ANotification: TNotification);
begin
  if ANotification <> nil then
    DoCancelNotification(ANotification.Name);
end;

function TNotificationCenterCocoa4.CreateNativeNotification(const ANotification: TNotification): UILocalNotification;
var
  NativeNotification: UILocalNotification;
  UserInfo: NSDictionary;
  GMTDateTime: TDateTime;
begin
  NativeNotification := TUILocalNotification.Create;
  if not ANotification.Name.IsEmpty then
  begin
    // Set unique identificator
    UserInfo := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(
      (StrToNSStr(ANotification.Name) as ILocalObject).GetObjectID, (StrToNSStr('id') as ILocalObject).GetObjectID));
    NativeNotification.setUserInfo(UserInfo);
  end;
  // Get GMT time and set notification fired date
  GMTDateTime := GetGMTDateTime(ANotification.FireDate);
  NativeNotification.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.defaultTimeZone));
  NativeNotification.setFireDate(DateTimeToNSDate(GMTDateTime));
  NativeNotification.setApplicationIconBadgeNumber(ANotification.Number);
  NativeNotification.setAlertBody(StrToNSStr(ANotification.AlertBody));
  NativeNotification.setAlertAction(StrToNSStr(ANotification.AlertAction));
  NativeNotification.setHasAction(ANotification.HasAction);
  NativeNotification.setRepeatInterval(RepeatIntervalToNSCalendarUnit(ANotification.RepeatInterval));
  if ANotification.EnableSound then
    if ANotification.SoundName.IsEmpty then
      NativeNotification.setSoundName(UILocalNotificationDefaultSoundName)
    else
      NativeNotification.setSoundName(StrToNSStr(ANotification.SoundName))
  else
    NativeNotification.setSoundName(nil);
  Result := NativeNotification;
end;

class destructor TNotificationCenterCocoa4.Destroy;
begin
  FInstance.Free;
end;

function TNotificationCenterCocoa4.ConvertNativeToDelphiNotification(const ANotification: Pointer): TNotification;
var
  UserInfo: NSDictionary;
  NotificationTmp: TNotification;
  LocalNotification: UILocalNotification;
begin
  NotificationTmp := nil;
  if ANotification <> nil then
  begin
    LocalNotification := TUILocalNotification.Wrap(ANotification);
    NotificationTmp := TNotification.Create;
    UserInfo := LocalNotification.userInfo;
    if UserInfo <> nil then
      NotificationTmp.Name := UTF8ToString(TNSString.Wrap(UserInfo.valueForKey(StrToNSStr('id'))).UTF8String);
    if LocalNotification.AlertBody <> nil then
      NotificationTmp.AlertBody := UTF8ToString(LocalNotification.AlertBody.UTF8String);
    if LocalNotification.AlertAction <> nil then
      NotificationTmp.AlertAction := UTF8ToString(LocalNotification.AlertAction.UTF8String);;
    NotificationTmp.Number := LocalNotification.ApplicationIconBadgeNumber;
    NotificationTmp.FireDate := NSDateToDateTime(LocalNotification.FireDate);
    NotificationTmp.EnableSound := LocalNotification.SoundName <> nil;
    if (LocalNotification.soundName = nil) or (LocalNotification.soundName.compare(UILocalNotificationDefaultSoundName) = NSOrderedSame) then
      NotificationTmp.SoundName := ''
    else
      NotificationTmp.SoundName := NSStrToStr(LocalNotification.soundName);
    NotificationTmp.HasAction := LocalNotification.HasAction;
    NotificationTmp.RepeatInterval := NSCalendarUnitToRepeatInterval(LocalNotification.repeatInterval);
  end;
  Result := NotificationTmp;
end;

procedure TNotificationCenterCocoa4.DoPresentNotification(const ANotification: TNotification);
var
  NativeNotification: UILocalNotification;
begin
  DoCancelNotification(ANotification);
  NativeNotification := CreateNativeNotification(ANotification);
  SharedApplication.presentLocalNotificationNow(NativeNotification);
end;

procedure TNotificationCenterCocoa4.DoRequestPermission;
begin
  // Nothing
  NotifyPermissionRequestResult(True);
end;

procedure TNotificationCenterCocoa4.DoScheduleNotification(const ANotification: TNotification);
var
  NativeNotification: UILocalNotification;
begin
  CancelNotification(ANotification.Name);
  NativeNotification := CreateNativeNotification(ANotification);
  SharedApplication.scheduleLocalNotification(NativeNotification);
end;

class function TNotificationCenterCocoa4.GetDefaultInstance: TNotificationCenterCocoa4;
begin
  if FInstance = nil then
    FInstance := TNotificationCenterCocoa4.Create;
  Result := FInstance;
end;

{$ENDREGION}

{ TPlatformNotificationCenter }

class function TPlatformNotificationCenter.GetInstance: TBaseNotificationCenter;
begin
  if TOsVersion.Check(10) then
    Result := TBaseNotificationCenter(TNotificationCenterCocoa10.Instance)
  else
    Result := TBaseNotificationCenter(TNotificationCenterCocoa4.Instance)
end;

{ TNotificationCenterCocoa10 }

procedure TNotificationCenterCocoa10.AddNotificationRequestHandler(error: NSError);
begin
  if error <> nil then
    NSLog(NSObjectToID(error.localizedDescription));
end;

procedure TNotificationCenterCocoa10.CheckAuthorization;
begin
  case AuthorizationStatus of
    TAuthorizationStatus.NotDetermined:
      raise ELocalNotificationAccess.CreateFmt(SNotificationCenterCannotPerformOperation, ['RequestPermission']); // do not localize
    TAuthorizationStatus.Denied:
      raise ELocalNotificationAccess.CreateFmt(SUserRejectedAccess, ['NotificationCenter']); // do not localize
  end;
end;

function TNotificationCenterCocoa10.ConvertNativeToDelphiNotification(const ANotification: Pointer): TNotification;
var
  Response: UNNotificationResponse;
  Request: UNNotificationRequest;
  NativeNotification: UNNotification;
begin
  Response := TUNNotificationResponse.Wrap(ANotification);
  NativeNotification := Response.notification;
  Request := NativeNotification.request;

  Result := TNotification.Create;
  Result.Name := NSStrToStr(Request.identifier);
  Result.Title := NSStrToStr(Request.content.title);
  Result.AlertBody := NSStrToStr(Request.content.body);
  if Request.content.badge = nil then
    Result.Number := 0
  else
    Result.Number := Request.content.badge.intValue;
  Result.EnableSound := Request.content.sound <> nil;
  Result.FireDate := NSDateToDateTime(NativeNotification.date);
  Result.RepeatInterval := TRepeatInterval.None;
end;

constructor TNotificationCenterCocoa10.Create;
begin
  inherited;
  FPermissionsState := TPermissionState.Unknown;
end;

function TNotificationCenterCocoa10.CreateNotificationRequest(const ANotification: TNotification; const AIsScheduled: Boolean): UNNotificationRequest;
var
  NotificationContent: UNMutableNotificationContent;
  Trigger: UNNotificationTrigger;
  Id: string;
begin
  NotificationContent := TUNMutableNotificationContent.Create;
  NotificationContent.setTitle(StrToNSStr(ANotification.Title));
  NotificationContent.setBody(StrToNSStr(ANotification.AlertBody));
  NotificationContent.setBadge(TNSNumber.Wrap(TNSNumber.OCClass.numberWithInt(ANotification.Number)));
  NotificationContent.setSound(GetNotificationSound(ANotification));

  if AIsScheduled then
    Trigger := GetNotificationTrigger(ANotification)
  else
    Trigger := nil;

  if ANotification.Name.IsEmpty then
    Id := DateTimeToStr(Now)
  else
    Id := ANotification.Name;
  Result := TUNNotificationRequest.OCClass.requestWithIdentifier(StrToNSStr(Id), NotificationContent, Trigger);
end;

class destructor TNotificationCenterCocoa10.Destroy;
begin
  FreeAndNil(FInstance);
end;

function TNotificationCenterCocoa10.DoAuthorizationStatus: TAuthorizationStatus;
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

procedure TNotificationCenterCocoa10.DoCancelAllNotifications;
begin
  CheckAuthorization;

  TUNUserNotificationCenter.OCClass.currentNotificationCenter.removeAllDeliveredNotifications;
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.removeAllPendingNotificationRequests;
end;

procedure TNotificationCenterCocoa10.DoCancelNotification(const ANotification: TNotification);
begin
  if ANotification <> nil then
    DoCancelNotification(ANotification.Name);
end;

procedure TNotificationCenterCocoa10.DoCancelNotification(const AName: string);
var
  NotificationIds: NSMutableArray;
begin
  CheckAuthorization;

  NotificationIds := TNSMutableArray.Wrap(TNSMutableArray.OCClass.arrayWithObject(StringToId(AName)));
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.removeDeliveredNotificationsWithIdentifiers(NotificationIds);
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.removePendingNotificationRequestsWithIdentifiers(NotificationIds);
end;

procedure TNotificationCenterCocoa10.DoPresentNotification(const ANotification: TNotification);
var
  NotificationRequest: UNNotificationRequest;
begin
  CheckAuthorization;

  NotificationRequest := CreateNotificationRequest(ANotification, False);
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.addNotificationRequest(NotificationRequest, AddNotificationRequestHandler);
end;

procedure TNotificationCenterCocoa10.DoRequestPermission;
begin
  case FPermissionsState of
    TNotificationCenterCocoa10.TPermissionState.Unknown:
      TUNUserNotificationCenter.OCClass.currentNotificationCenter.requestAuthorizationWithOptions(
        UNAuthorizationOptionBadge or UNAuthorizationOptionSound or UNAuthorizationOptionAlert, RequestAuthorizationHandler);

    TNotificationCenterCocoa10.TPermissionState.Denied:
      NotifyPermissionRequestResult(False);

    TNotificationCenterCocoa10.TPermissionState.Granted:
      NotifyPermissionRequestResult(True);
  end;
end;

procedure TNotificationCenterCocoa10.DoScheduleNotification(const ANotification: TNotification);
var
  NotificationRequest: UNNotificationRequest;
begin
  CheckAuthorization;

  DoCancelNotification(ANotification);
  NotificationRequest := CreateNotificationRequest(ANotification, True);
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.addNotificationRequest(NotificationRequest, AddNotificationRequestHandler);
end;

class function TNotificationCenterCocoa10.GetDefaultInstance: TNotificationCenterCocoa10;
begin
  if FInstance = nil then
    FInstance := TNotificationCenterCocoa10.Create;
  Result := FInstance;
end;

function TNotificationCenterCocoa10.GetNotificationSound(const ANotification: TNotification): UNNotificationSound;
begin
  if not ANotification.EnableSound then
    Exit(nil);

  if ANotification.SoundName.IsEmpty then
    Result := TUNNotificationSound.OCClass.defaultSound
  else
    Result := TUNNotificationSound.OCClass.soundNamed(StrToNSStr(ANotification.SoundName))
end;

function TNotificationCenterCocoa10.GetNotificationTrigger(const ANotification: TNotification): UNNotificationTrigger;
var
  DateComponents: NSDateComponents;
begin
  DateComponents := RepeatIntervalToNSDateComponents(ANotification.FireDate, ANotification.RepeatInterval);
  DateComponents.setTimeZone(TNSTimeZone.Wrap(TNSTimeZone.OCClass.defaultTimeZone));
  Result := TUNCalendarNotificationTrigger.OCClass.triggerWithDateMatchingComponents(DateComponents, ANotification.RepeatInterval <> TRepeatInterval.None);
end;

function TNotificationCenterCocoa10.RepeatIntervalToNSDateComponents(const ADate: TDate;
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

procedure TNotificationCenterCocoa10.RequestAuthorizationHandler(granted: Boolean; error: NSError);
begin
  if granted then
    FPermissionsState := TPermissionState.Granted
  else
    FPermissionsState := TPermissionState.Denied;

  NotifyPermissionRequestResult(granted);
end;

{ TNotificationCenterIOS }

procedure TNotificationCenterIOS.ClearDelayedNotifications;
begin
  FDelayedNotifications.Clear;
end;

constructor TNotificationCenterIOS.Create;
begin
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, ReceiveLocalNotification);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessage<UNNotificationResponse>, ReceiveLocalNotification);

  FDelayedNotifications := TObjectList<TNotification>.Create;
  FIsApplicationLoaded := False;
end;

destructor TNotificationCenterIOS.Destroy;
begin
  { Unsibscribe }
  TMessageManager.DefaultManager.Unsubscribe(TMessage<UNNotificationResponse>, ReceiveLocalNotification);
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, ReceiveLocalNotification);
  { Destroying }
  ClearDelayedNotifications;
  FDelayedNotifications.Free;
  inherited;
end;

procedure TNotificationCenterIOS.DidFormsLoad;
begin
  FIsApplicationLoaded := True;
  NotifyDelayedNotifications;
end;

function TNotificationCenterIOS.DoGetIconBadgeNumber: Integer;
begin
  Result := SharedApplication.ApplicationIconBadgeNumber;
end;

procedure TNotificationCenterIOS.DoLoaded;
begin
  inherited;
  // DoLoaded is invoked before TForm.OnCreate. However, we have to process receiving of the Local Notification
  // strictly after the form is fully loaded, because we need to invoke TNotificationCenter.OnReceiveLocalNotification
  // after TForm.OnCreate.
  TThread.ForceQueue(nil, procedure begin
    DidFormsLoad;
  end);
end;

procedure TNotificationCenterIOS.DoResetIconBadgeNumber;
begin
  SharedApplication.setApplicationIconBadgeNumber(0);
end;

procedure TNotificationCenterIOS.DoSetIconBadgeNumber(const ACount: Integer);
begin
  SharedApplication.setApplicationIconBadgeNumber(ACount);
end;

procedure TNotificationCenterIOS.ReceiveLocalNotification(const Sender: TObject; const M: TMessage);

  procedure SendNotification(const Notification: TNotification);
  begin
    TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
    // Sending Delayed notifications
    if FDelayedNotifications.Count > 0 then
      NotifyDelayedNotifications;
  end;

var
  NativeNotification: Pointer;
  Notification: TNotification;
begin
  if M is TMessageReceivedNotification then
    NativeNotification := NSObjectToID((M as TMessageReceivedNotification).Value)
  else
    NativeNotification := NSObjectToID((M as TMessage<UNNotificationResponse>).Value);

  // iOS doesn't provide list of presented notification. So we need to store it
  // in our list for cancelling in future with using ID
  Notification := ConvertNativeToDelphiNotification(NativeNotification);

  if not FIsApplicationLoaded then
    FDelayedNotifications.Add(Notification)
  else
    try
      SendNotification(Notification);
    finally
      Notification.Free;
    end;
end;

procedure TNotificationCenterIOS.NotifyDelayedNotifications;
var
  Notification: TNotification;
begin
  for Notification in FDelayedNotifications do
    TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(Notification));
  ClearDelayedNotifications;
end;

function TNotificationCenterIOS.SharedApplication: UIApplication;
var
  App: Pointer;
begin
  Result := nil;
  App := TUIApplication.OCClass.sharedApplication;
  if App <> nil then
    Result := TUIApplication.Wrap(App);
end;

end.
