{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.PushNotification.FCM.iOS;

interface

{$HPPEMIT LINKUNIT}

implementation

uses
  System.SysUtils, System.Classes, System.JSON, System.PushNotification, System.Messaging, System.Notification, System.RTLConsts,
  Macapi.ObjectiveC, Macapi.Helpers, Macapi.ObjCRuntime,
  iOSapi.Foundation, iOSapi.UIKit, iOSapi.UserNotifications, iOSapi.FirebaseCommon, iOSapi.FirebaseMessaging, iOSapi.Helpers,
  FMX.Forms, FMX.Platform;

type
  TFcmPushServiceNotification = class(TPushServiceNotification)
  private
    FRawData: TJSONObject;
  protected
    function GetDataKey: string; override;
    function GetJson: TJSONObject; override;
    function GetDataObject: TJSONObject; override;
  public
    constructor Create(const AJSON: string); overload;
  end;

  TFcmPushService = class;

  TFIRMessagingDelegate = class(TOCLocal, FIRMessagingDelegate)
  private
    FPushService: TFcmPushService;
    procedure ReceivedMessage(remoteMessage: FIRMessagingRemoteMessage);
  public
    { FIRMessagingDelegate }
    procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl;
    [MethodName('messaging:didReceiveMessage:')]
    procedure didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage); cdecl;
    [MethodName('messaging:didRefreshRegistrationToken:')]
    procedure didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
    [MethodName('messaging:didReceiveRegistrationToken:')]
    procedure didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
  public
    constructor Create(const APushService: TFcmPushService);
  end;

  TFcmPushService = class(TPushService)
  private
    FAuthOptions: UNAuthorizationOptions;
    FDeviceID: string;
    FDeviceToken: string;
    FFIRMessagingDelegate: TFIRMessagingDelegate;
    FMessaging: FIRMessaging;
    FStartupError: string;
    FStartupNotification: string;
    FStatus: TPushService.TStatus;
    function GetUserDefaultsTokenKey: NSString;
    procedure MessageReceived(const AJSON: string);
    function Messaging: FIRMessaging;
    procedure PushMessageHandler(const Sender: TObject; const M: TMessage);
    procedure Register;
    procedure RegisterRemoteNotificationsIOS10OrLater;
    procedure RegisterRemoteNotificationsIOS7OrEarlier;
    procedure RegisterRemoteNotificationsIOS8OrLater;
    procedure RequestAuthorization;
    procedure RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
    procedure Unregister;
  protected
    constructor Create; reintroduce;
    function GetDeviceToken: TPushService.TPropArray; override;
    function GetDeviceID: TPushService.TPropArray; override;
    function GetStartupNotifications: TArray<TPushServiceNotification>; override;
    function GetStartupError: string; override;
    function GetStatus: TPushService.TStatus; override;
    procedure SetDeviceToken(const AToken: string);
    procedure StartService; override;
    procedure StopService; override;
  public
    destructor Destroy; override;
  end;

procedure RegisterPushServices;
begin
  TFcmPushService.Create;
end;

function NSDictionaryToJSON(const ADictionary: NSDictionary): string;
var
  LData: NSData;
  LString: NSString;
  LError: NSError;
begin
  LData := TNSJSONSerialization.OCClass.dataWithJSONObject(NSObjectToID(ADictionary), 0, Addr(LError));
  if (LData <> nil) and (LError = nil) then
  begin
    LString := TNSString.Wrap(TNSString.Alloc.initWithData(LData, NSUTF8StringEncoding));
    Result :=  NSStrToStr(LString);
  end
  else
    Result := string.Empty;
end;

function StandardUserDefaults: NSUserDefaults;
begin
  Result := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.StandardUserDefaults);
end;

function UserNotificationCenter: UNUserNotificationCenter;
begin
  Result := TUNUserNotificationCenter.OCClass.currentNotificationCenter;
end;

{ TFcmPushServiceNotification }

constructor TFcmPushServiceNotification.Create(const AJSON: string);
begin
  FRawData := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
end;

function TFcmPushServiceNotification.GetDataKey: string;
begin
  Result := TPushService.TServiceNames.FCM;
end;

function TFcmPushServiceNotification.GetDataObject: TJSONObject;
var
  LValue: TJSONValue;
begin
  Result := FRawData;
  if FRawData <> nil then
  begin
    LValue := FRawData.Values[GetDataKey];
    if LValue <> nil then
      Result := LValue as TJSONObject;
  end;
end;

function TFcmPushServiceNotification.GetJson: TJSONObject;
begin
  Result := FRawData;
end;

{ TFIRMessagingDelegate }

constructor TFIRMessagingDelegate.Create(const APushService: TFcmPushService);
begin
  inherited Create;
  FPushService := APushService;
end;

procedure TFIRMessagingDelegate.applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage);
begin
  ReceivedMessage(remoteMessage);
end;

procedure TFIRMessagingDelegate.didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage);
begin
  ReceivedMessage(remoteMessage);
end;

procedure TFIRMessagingDelegate.didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
begin
  FPushService.SetDeviceToken(NSStrToStr(fcmToken));
end;

procedure TFIRMessagingDelegate.didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
begin
  FPushService.SetDeviceToken(NSStrToStr(fcmToken));
end;

procedure TFIRMessagingDelegate.ReceivedMessage(remoteMessage: FIRMessagingRemoteMessage);
begin
  FPushService.MessageReceived(NSDictionaryToJSON(remoteMessage.appData));
end;

{ TFcmPushService }

constructor TFcmPushService.Create;
begin
  inherited Create(TPushServiceManager.Instance, TPushService.TServiceNames.FCM);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushStartupNotificationMessage, PushMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushRemoteNotificationMessage, PushMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushFailToRegisterMessage, PushMessageHandler);
  FDeviceID := NSStrToStr(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).identifierForVendor.UUIDString);
  FDeviceToken := NSStrToStr(StandardUserDefaults.stringForKey(GetUserDefaultsTokenKey));
end;

destructor TFcmPushService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TPushStartupNotificationMessage, PushMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushRemoteNotificationMessage, PushMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushFailToRegisterMessage, PushMessageHandler);
  inherited;
end;

function TFcmPushService.GetUserDefaultsTokenKey: NSString;
begin
  Result := StrToNSStr(NSStrToStr(TiOSHelper.MainBundle.bundleIdentifier) + '.DeviceToken'); // Do not localize
end;

procedure TFcmPushService.SetDeviceToken(const AToken: string);
var
  LChanges: TPushService.TChanges;
begin
  FDeviceToken := AToken;
  StandardUserDefaults.setObject(StringToID(FDeviceToken), GetUserDefaultsTokenKey);
  LChanges := [TPushService.TChange.DeviceToken];
  if FStatus <> TPushService.TStatus.Started then
  begin
    FStatus := TPushService.TStatus.Started;
    LChanges := LChanges + [TPushService.TChange.Status];
  end;
  DoChange(LChanges);
end;

procedure TFcmPushService.PushMessageHandler(const Sender: TObject; const M: TMessage);
begin
  try
    if M is TPushStartupNotificationMessage then
    begin
      FStartupNotification := TPushStartupNotificationMessage(M).Value.Notification;
      DoChange([TPushService.TChange.StartupNotifications]);
      MessageReceived(FStartupNotification);
    end
    else if M is TPushRemoteNotificationMessage then
      DoReceiveNotification(TFcmPushServiceNotification.Create(TPushRemoteNotificationMessage(M).Value.Notification))
    else if M is TPushFailToRegisterMessage then
    begin
      FStartupError := TPushFailToRegisterMessage(M).Value.ErrorMessage;
      FStatus := TPushService.TStatus.StartupError;
      DoChange([TPushService.TChange.Status]);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TFcmPushService.MessageReceived(const AJSON: string);
begin
  DoReceiveNotification(TFcmPushServiceNotification.Create(AJSON));
end;

procedure TFcmPushService.Register;
begin
  TFIRApp.OCClass.configure;
  FFIRMessagingDelegate := TFIRMessagingDelegate.Create(Self);
  Messaging.setDelegate(FFIRMessagingDelegate.GetObjectID);
end;

procedure TFcmPushService.Unregister;
begin
  FDeviceToken := string.Empty;
  Messaging.setDelegate(nil);
  FFIRMessagingDelegate.Free;
end;

procedure TFcmPushService.StartService;
begin
  FStatus := TPushService.TStatus.Starting;
  DoChange([TChange.Status]);
  Register;
  RequestAuthorization;
end;

procedure TFcmPushService.StopService;
begin
  if not FDeviceToken.IsEmpty then
  begin
    Unregister;
    FDeviceToken := string.Empty;
    FStartupError := string.Empty;
    FStatus := TPushService.TStatus.Stopped;
    DoChange([TChange.Status]);
  end;
end;

function TFcmPushService.GetDeviceID: TPushService.TPropArray;
begin
  Result := TPushService.TPropArray.Create(TPushService.TPropPair.Create(TPushService.TDeviceIDNames.DeviceID, FDeviceID));
end;

function TFcmPushService.GetDeviceToken: TPushService.TPropArray;
begin
  Result := TPushService.TPropArray.Create(TPushService.TPropPair.Create(TPushService.TDeviceTokenNames.DeviceToken, FDeviceToken));
end;

function TFcmPushService.GetStartupError: string;
begin
  Result := FStartupError;
end;

function TFcmPushService.GetStartupNotifications: TArray<TPushServiceNotification>;
begin
  if FStartupNotification = string.Empty then
    Result := nil
  else
    Result := TArray<TPushServiceNotification>.Create(TFcmPushServiceNotification.Create(FStartupNotification));
end;

function TFcmPushService.GetStatus: TPushService.TStatus;
begin
  Result := FStatus;
end;

function TFcmPushService.Messaging: FIRMessaging;
begin
  if FMessaging = nil then
    FMessaging := TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging);
  Result := FMessaging;
end;

procedure TFcmPushService.RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin
  if not TiOSHelper.SharedApplication.isRegisteredForRemoteNotifications then
    TiOSHelper.SharedApplication.registerForRemoteNotifications;
end;

procedure TFcmPushService.RegisterRemoteNotificationsIOS10OrLater;
begin
  UserNotificationCenter.requestAuthorizationWithOptions(FAuthOptions, RequestAuthorizationWithOptionsCompletionHandler);
end;

procedure TFcmPushService.RegisterRemoteNotificationsIOS7OrEarlier;
begin
  TiOSHelper.SharedApplication.registerForRemoteNotificationTypes(Addr(FAuthOptions));
end;

procedure TFcmPushService.RegisterRemoteNotificationsIOS8OrLater;
var
  LSettings: UIUserNotificationSettings;
begin
  LSettings := TUIUserNotificationSettings.Wrap(TUIUserNotificationSettings.OCClass.settingsForTypes(FAuthOptions, nil));
  TiOSHelper.SharedApplication.registerUserNotificationSettings(LSettings);
  if not TiOSHelper.SharedApplication.isRegisteredForRemoteNotifications then
    TiOSHelper.SharedApplication.registerForRemoteNotifications;
end;

procedure TFcmPushService.RequestAuthorization;
begin
  FAuthOptions := UNAuthorizationOptionSound or UNAuthorizationOptionAlert or UNAuthorizationOptionBadge;
  if TOSVersion.Check(10) then
    RegisterRemoteNotificationsIOS10OrLater
  else if TOSVersion.Check(8) then
    RegisterRemoteNotificationsIOS8OrLater
  else
    RegisterRemoteNotificationsIOS7OrEarlier;
end;

initialization
  RegisterPushServices;

end.
