{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
unit FMX.PushNotification.Android;

interface

{$SCOPEDENUMS ON}

{$HPPEMIT LINKUNIT}

implementation

uses
  System.SysUtils, System.Classes, System.JSON, System.PushNotification, System.Messaging, Androidapi.JNI.Embarcadero,
  AndroidApi.JNI.Embarcadero.Firebase, Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.PlayServices.Tasks, Androidapi.JNI.Firebase,
  FMX.Platform.Android, FMX.Consts;

type

{ Firebase implementation }

  TFcmPushServiceNotification = class(TPushServiceNotification)
  private
    FRawData: TJSONObject;
  protected
    function GetDataKey: string; override;
    function GetJson: TJSONObject; override;
    function GetDataObject: TJSONObject; override;
  public
    constructor Create(const ABundle: JBundle); overload;
  end;

  TTaskCompleteCallback = reference to procedure(task: JTask);

  TFcmPushService = class(TPushService)
  private type
    /// <summary>Listener for receiving result of requesting Firebase Token.</summary>
    TTokenTaskCompleteListener = class(TJavaLocal, JOnCompleteListener)
    private
      FCompleteCallback: TTaskCompleteCallback;
    public
      constructor Create(const ACompleteCallback: TTaskCompleteCallback);

      { JOnCompleteListener }
      procedure onComplete(task: JTask); cdecl;
    end;

    // <summary>Listeners for receiving new token and Push notification, when application is in foreground.</summary>
    TAndroidPushNotificationListener = class(TJavaLocal, JMessaging_PushNotificationListener)
    public
      { JMessaging_PushNotificationListener }
      procedure onNotificationReceived(notification: JBundle); cdecl;
      procedure onNewTokenReceived(token: JString); cdecl;
    end;
  private
    FDeviceToken: string;
    FDeviceID: string;
    FStatus: TPushService.TStatus;
    FStartupError: string;
    FTokenTaskCompleteListener: TTokenTaskCompleteListener;
    FPushNotificationListener: TAndroidPushNotificationListener;
    procedure GetToken;
    procedure MessageReceivedListener(const Sender: TObject; const M: TMessage);
  protected
    constructor Create; reintroduce;
    function GetStatus: TPushService.TStatus; override;
    procedure StartService; override;
    procedure StopService; override;
    function GetDeviceToken: TPushService.TPropArray; override;
    function GetDeviceID: TPushService.TPropArray; override;
    function GetStartupNotifications: TArray<TPushServiceNotification>; override;
    function GetStartupError: string; override;
  public
    destructor Destroy; override;
  end;

var
  aFcmPushService : TFcmPushService;

procedure RegisterPushService;
begin
  // TPushService registers itself in TPushService.AfterConstruction So we don't need to have a store referenece at it.
  // TPushServiceManager destroys all registered push services in TPushServiceManager.Destroy.
  aFcmPushService := TFcmPushService.Create;
end;

procedure UnregisterPushService;
begin
  aFcmPushService.Manager.RemoveService(aFcmPushService);
  aFcmPushService := nil;
end;

{ TFcmPushService }

procedure TFcmPushService.GetToken;
var
  FirebaseMessaging: JFirebaseMessaging;
begin
  // In fact, registration of the application in Firebase Cloud is carried out in
  // com.embarcadero.firebase.provider.FirebaseInitProvider. So in this place we just receive device token.
  FTokenTaskCompleteListener := TTokenTaskCompleteListener.Create(
    procedure(task: JTask)
    begin
      if task.isSuccessful then
      begin
        FDeviceToken := JStringToString(TJString.Wrap(task.getResult));
        FStatus := TStatus.Started;

        DoChange([TChange.Status, TChange.DeviceToken]);
      end
      else
      begin
        FStartupError := JStringToString(task.getException.getMessage);
        FStatus := TStatus.StartupError;

        DoChange([TChange.Status]);
      end;
    end);

  try
    FirebaseMessaging := TJFirebaseMessaging.JavaClass.getInstance;
    FirebaseMessaging.getToken.addOnCompleteListener(FTokenTaskCompleteListener);
  except
    on E: EJNIException do
    begin
      FStartupError := E.Message;
      FStatus := TStatus.StartupError;

      DoChange([TChange.Status]);
    end;
  end;
end;

procedure TFcmPushService.StartService;
begin
  FStatus := TStatus.Starting;

  DoChange([TChange.Status]);
  GetToken;
end;

procedure TFcmPushService.StopService;
begin
  // Not reachable.
end;

constructor TFcmPushService.Create;
begin
  inherited Create(TPushServiceManager.Instance, TPushService.TServiceNames.FCM);

  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedListener);

  FPushNotificationListener := TAndroidPushNotificationListener.Create;

  TJmessaging_ProxyFirebaseMessagingService.JavaClass.setListener(FPushNotificationListener);
end;

destructor TFcmPushService.Destroy;
begin
  TJmessaging_ProxyFirebaseMessagingService.JavaClass.setListener(nil);

  FreeAndNil(FPushNotificationListener);
  FreeAndNil(FTokenTaskCompleteListener);

  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedListener);

  inherited;
end;

function TFcmPushService.GetDeviceID: TPushService.TPropArray;
begin
  if FDeviceID.IsEmpty then
    FDeviceID := JStringToString(MainActivity.getDeviceID);
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
var
  LBundle: JBundle;
begin
  LBundle := MainActivity.getStartupFCM;
  if LBundle <> nil then
    Result := TArray<TPushServiceNotification>.Create(TFcmPushServiceNotification.Create(LBundle))
  else
    Result := nil;
end;

function TFcmPushService.GetStatus: TPushService.TStatus;
begin
  Result := FStatus;
end;

procedure TFcmPushService.MessageReceivedListener(const Sender: TObject; const M: TMessage);

  function IsIntentWithNotification(Intent: JIntent): Boolean;
  begin
    Result := (Intent <> nil) and (Intent.getAction <> nil) and
               Intent.getAction.equals(TJFMXNativeActivity.JavaClass.ACTION_FCM_NOTIFICATION);
  end;

  procedure ProcessBundle(const ANotification: JBundle);
  var
    LNotificationObject: TFcmPushServiceNotification;
  begin
    LNotificationObject := TFcmPushServiceNotification.Create(ANotification);
    // Notifications come in on secondary thread
    TThread.Queue(nil,
      procedure
      begin
        // Handle notifications on firemonkey thread
        DoReceiveNotification(LNotificationObject);
      end);
  end;

  procedure ProcessIntent(const AIntent: JIntent);
  var
    LBundle: JBundle;
  begin
    if AIntent <> nil then
    begin
      LBundle := AIntent.getBundleExtra(StringToJString('fcm'));
      if LBundle = nil then
        LBundle := AIntent.getExtras();

      if LBundle <> nil then
        ProcessBundle(LBundle);
    end;
  end;

var
  InputIntent: JIntent;
begin
  if M is TMessageReceivedNotification then
  begin
    InputIntent := TMessageReceivedNotification(M).Value;
    if IsIntentWithNotification(InputIntent) then
      ProcessIntent(InputIntent);
  end;
end;

{ TFcmPushServiceNotification }

function TFcmPushServiceNotification.GetDataObject: TJSONObject;
var
  LValue: TJSONValue;
begin
  // The message /can/ be prefaced with "fcm", but this is not required
  Result := FRawData;  // take raw JSON as default
  if FRawData <> nil then
  begin
    LValue := FRawData.Values[GetDataKey];
    if LValue <> nil then
      Result := LValue as TJSONObject;
  end;
end;

constructor TFcmPushServiceNotification.Create(const ABundle: JBundle);
var
  LJSONObject: TJSONObject;
  LIterator: JIterator;
  LValue: JString;
  LKey: JString;
begin
  LJSONObject := TJSONObject.Create;
  LIterator := ABundle.KeySet.iterator;
  while LIterator.hasNext do
  begin
    LKey := LIterator.next.toString;
    LValue := ABundle.&get(LKey).ToString;
    LJSONObject.AddPair(JStringToString(LKey), JStringToString(LValue));
  end;
  Assert(LJSONObject.Count = ABundle.keySet.size);
  FRawData := LJSONObject;
end;

function TFcmPushServiceNotification.GetDataKey: string;
begin
  Result := 'fcm'; // Do not localize
end;

function TFcmPushServiceNotification.GetJson: TJSONObject;
begin
  Result := FRawData;
end;

{ TFcmPushService.TTokenTaskCompleteListener }

constructor TFcmPushService.TTokenTaskCompleteListener.Create(const ACompleteCallback: TTaskCompleteCallback);
begin
  inherited Create;
  FCompleteCallback := ACompleteCallback;
end;

procedure TFcmPushService.TTokenTaskCompleteListener.onComplete(task: JTask);
begin
  if Assigned(FCompleteCallback) then
    FCompleteCallback(task);
end;

{ TFcmPushService.TAndroidPushNotificationListener }

procedure TFcmPushService.TAndroidPushNotificationListener.onNewTokenReceived(token: JString);
begin
  // Not applicable.
end;

procedure TFcmPushService.TAndroidPushNotificationListener.onNotificationReceived(notification: JBundle);
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(TJFMXNativeActivity.JavaClass.ACTION_FCM_NOTIFICATION);
  Intent.putExtra(StringToJString('fcm'), notification);

  TMessageManager.DefaultManager.SendMessage(nil, TMessageReceivedNotification.Create(Intent));
end;

initialization
  RegisterPushService;

finalization
  UnregisterPushService;

end.
