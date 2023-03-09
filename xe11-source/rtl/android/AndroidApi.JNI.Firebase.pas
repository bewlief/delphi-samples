{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.Firebase;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.PlayServices,
  Androidapi.JNI.PlayServices.Tasks;

type
// ===== Forward declarations =====

  JFirebaseApp = interface;//com.google.firebase.FirebaseApp
  //JFirebaseApp_BackgroundStateChangeListener = interface;//com.google.firebase.FirebaseApp$BackgroundStateChangeListener
  //JFirebaseAppLifecycleListener = interface;//com.google.firebase.FirebaseAppLifecycleListener
  JFirebaseOptions = interface;//com.google.firebase.FirebaseOptions
  //JFirebaseOptions_1 = interface;//com.google.firebase.FirebaseOptions$1
  JEnhancedIntentService = interface;//com.google.firebase.messaging.EnhancedIntentService
  JFirebaseMessaging = interface;//com.google.firebase.messaging.FirebaseMessaging
  JFirebaseMessagingService = interface;//com.google.firebase.messaging.FirebaseMessagingService
  JGmsRpc = interface;//com.google.firebase.messaging.GmsRpc
  Jmessaging_Metadata = interface;//com.google.firebase.messaging.Metadata
  JNotificationParams = interface;//com.google.firebase.messaging.NotificationParams
  JRemoteMessage = interface;//com.google.firebase.messaging.RemoteMessage
  //JRemoteMessage_1 = interface;//com.google.firebase.messaging.RemoteMessage$1
  JRemoteMessage_Notification = interface;//com.google.firebase.messaging.RemoteMessage$Notification
  JStore_Token = interface;//com.google.firebase.messaging.Store$Token

// ===== Interface declarations =====

  JFirebaseAppClass = interface(JObjectClass)
    ['{838A377D-1D26-4E81-A4C0-8155C56B8128}']
    {class} function _GetDEFAULT_APP_NAME: JString; cdecl;
    {class} procedure clearInstancesForTest; cdecl;
    {class} function getApps(context: JContext): JList; cdecl;
    {class} function getInstance: JFirebaseApp; cdecl; overload;
    {class} function getInstance(string_: JString): JFirebaseApp; cdecl; overload;
    {class} function getPersistenceKey(string_: JString; firebaseOptions: JFirebaseOptions): JString; cdecl; overload;
    {class} function initializeApp(context: JContext): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; firebaseOptions: JFirebaseOptions): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; firebaseOptions: JFirebaseOptions; string_: JString): JFirebaseApp; cdecl; overload;
    {class} property DEFAULT_APP_NAME: JString read _GetDEFAULT_APP_NAME;
  end;

  [JavaSignature('com/google/firebase/FirebaseApp')]
  JFirebaseApp = interface(JObject)
    ['{B7D36E0E-987B-4442-BDCC-10A26FE2DA04}']
    //procedure addBackgroundStateChangeListener(backgroundStateChangeListener: JFirebaseApp_BackgroundStateChangeListener); cdecl;
    //procedure addLifecycleEventListener(firebaseAppLifecycleListener: JFirebaseAppLifecycleListener); cdecl;
    procedure delete; cdecl;
    function equals(object_: JObject): Boolean; cdecl;
    function &get(class_: Jlang_Class): JObject; cdecl;
    function getApplicationContext: JContext; cdecl;
    function getName: JString; cdecl;
    function getOptions: JFirebaseOptions; cdecl;
    function getPersistenceKey: JString; cdecl; overload;
    function hashCode: Integer; cdecl;
    function isDataCollectionDefaultEnabled: Boolean; cdecl;
    function isDefaultApp: Boolean; cdecl;
    //procedure removeBackgroundStateChangeListener(backgroundStateChangeListener: JFirebaseApp_BackgroundStateChangeListener); cdecl;
    //procedure removeLifecycleEventListener(firebaseAppLifecycleListener: JFirebaseAppLifecycleListener); cdecl;
    procedure setAutomaticResourceManagementEnabled(b: Boolean); cdecl;
    procedure setDataCollectionDefaultEnabled(boolean: JBoolean); cdecl; overload;
    procedure setDataCollectionDefaultEnabled(b: Boolean); cdecl; overload;
    function toString: JString; cdecl;
  end;
  TJFirebaseApp = class(TJavaGenericImport<JFirebaseAppClass, JFirebaseApp>) end;

  // com.google.firebase.FirebaseApp$BackgroundStateChangeListener
  // com.google.firebase.FirebaseAppLifecycleListener
  JFirebaseOptionsClass = interface(JObjectClass)
    ['{BD98AD54-572A-4962-A9C6-16CD919D3E6E}']
    {class} function fromResource(context: JContext): JFirebaseOptions; cdecl;
  end;

  [JavaSignature('com/google/firebase/FirebaseOptions')]
  JFirebaseOptions = interface(JObject)
    ['{229D49DD-61F6-4A19-9AB8-0BB36F6BC4FF}']
    function equals(object_: JObject): Boolean; cdecl;
    function getApiKey: JString; cdecl;
    function getApplicationId: JString; cdecl;
    function getDatabaseUrl: JString; cdecl;
    function getGaTrackingId: JString; cdecl;
    function getGcmSenderId: JString; cdecl;
    function getProjectId: JString; cdecl;
    function getStorageBucket: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJFirebaseOptions = class(TJavaGenericImport<JFirebaseOptionsClass, JFirebaseOptions>) end;

  // com.google.firebase.FirebaseOptions$1
  JEnhancedIntentServiceClass = interface(JServiceClass)
    ['{D24DCFF9-59DD-4B4E-8948-9C81FD9F6D32}']
    {class} function init: JEnhancedIntentService; cdecl;
  end;

  [JavaSignature('com/google/firebase/messaging/EnhancedIntentService')]
  JEnhancedIntentService = interface(JService)
    ['{2E674278-EE24-408D-BCF7-B1CD80EDC64B}']
    procedure handleIntent(intent: JIntent); cdecl;
    function handleIntentOnMainThread(intent: JIntent): Boolean; cdecl;
    function onBind(intent: JIntent): JIBinder; cdecl;
    procedure onDestroy; cdecl;
    function onStartCommand(intent: JIntent; i: Integer; i1: Integer): Integer; cdecl;
  end;
  TJEnhancedIntentService = class(TJavaGenericImport<JEnhancedIntentServiceClass, JEnhancedIntentService>) end;

  JFirebaseMessagingClass = interface(JObjectClass)
    ['{5E14657A-AF87-404A-8BF2-9B14D1730E90}']
    {class} function _GetINSTANCE_ID_SCOPE: JString; cdecl;
    {class} function getInstance: JFirebaseMessaging; cdecl; overload;
    {class} //function getTransportFactory: JTransportFactory; cdecl;
    {class} property INSTANCE_ID_SCOPE: JString read _GetINSTANCE_ID_SCOPE;
  end;

  [JavaSignature('com/google/firebase/messaging/FirebaseMessaging')]
  JFirebaseMessaging = interface(JObject)
    ['{EE5EEAAC-6BB6-4401-A1A0-69E348580BD0}']
    function deleteToken: JTask; cdecl;
    function deliveryMetricsExportToBigQueryEnabled: Boolean; cdecl;
    function getToken: JTask; cdecl;
    function isAutoInitEnabled: Boolean; cdecl;
    procedure send(remoteMessage: JRemoteMessage); cdecl;
    procedure setAutoInitEnabled(b: Boolean); cdecl;
    procedure setDeliveryMetricsExportToBigQuery(b: Boolean); cdecl;
    function subscribeToTopic(string_: JString): JTask; cdecl;
    function unsubscribeFromTopic(string_: JString): JTask; cdecl;
  end;
  TJFirebaseMessaging = class(TJavaGenericImport<JFirebaseMessagingClass, JFirebaseMessaging>) end;

  JFirebaseMessagingServiceClass = interface(JEnhancedIntentServiceClass)
    ['{77E28384-4E74-44BB-9864-C8548B690E2F}']
    {class} function _GetACTION_DIRECT_BOOT_REMOTE_INTENT: JString; cdecl;
    {class} function init: JFirebaseMessagingService; cdecl;
    {class} property ACTION_DIRECT_BOOT_REMOTE_INTENT: JString read _GetACTION_DIRECT_BOOT_REMOTE_INTENT;
  end;

  [JavaSignature('com/google/firebase/messaging/FirebaseMessagingService')]
  JFirebaseMessagingService = interface(JEnhancedIntentService)
    ['{250F778A-64D2-471C-8535-AF42C3D86975}']
    procedure handleIntent(intent: JIntent); cdecl;
    procedure onDeletedMessages; cdecl;
    procedure onMessageReceived(remoteMessage: JRemoteMessage); cdecl;
    procedure onMessageSent(string_: JString); cdecl;
    procedure onNewToken(string_: JString); cdecl;
    procedure onSendError(string_: JString; exception: JException); cdecl;
  end;
  TJFirebaseMessagingService = class(TJavaGenericImport<JFirebaseMessagingServiceClass, JFirebaseMessagingService>) end;

  JGmsRpcClass = interface(JObjectClass)
    ['{A100B719-D0C9-405D-9BB1-D7C785B18374}']
  end;

  [JavaSignature('com/google/firebase/messaging/GmsRpc')]
  JGmsRpc = interface(JObject)
    ['{FEBB6158-1882-4F57-8AC5-01E5217C0344}']
  end;
  TJGmsRpc = class(TJavaGenericImport<JGmsRpcClass, JGmsRpc>) end;

  Jmessaging_MetadataClass = interface(JObjectClass)
    ['{A47F9F86-A487-4B29-B468-6D2CB0147318}']
  end;

  [JavaSignature('com/google/firebase/messaging/Metadata')]
  Jmessaging_Metadata = interface(JObject)
    ['{FE639378-41E3-497B-B059-FF51E7A540F6}']
  end;
  TJmessaging_Metadata = class(TJavaGenericImport<Jmessaging_MetadataClass, Jmessaging_Metadata>) end;

  JNotificationParamsClass = interface(JObjectClass)
    ['{E1329F85-919D-46DD-B840-26621406B470}']
    {class} function init(bundle: JBundle): JNotificationParams; cdecl;
    {class} function isNotification(bundle: JBundle): Boolean; cdecl; overload;
  end;

  [JavaSignature('com/google/firebase/messaging/NotificationParams')]
  JNotificationParams = interface(JObject)
    ['{4192CD3C-EADD-4BD7-A626-FD87BB544ECA}']
    function getBoolean(string_: JString): Boolean; cdecl;
    function getInteger(string_: JString): JInteger; cdecl;
    function getJSONArray(string_: JString): JJSONArray; cdecl;
    function getLink: Jnet_Uri; cdecl;
    function getLocalizationArgsForKey(string_: JString): TJavaObjectArray<JObject>; cdecl;
    function getLocalizationResourceForKey(string_: JString): JString; cdecl;
    function getLocalizedString(resources: JResources; string_: JString; string_1: JString): JString; cdecl;
    function getLong(string_: JString): JLong; cdecl;
    function getNotificationChannelId: JString; cdecl;
    function getPossiblyLocalizedString(resources: JResources; string_: JString; string_1: JString): JString; cdecl;
    function getSoundResourceName: JString; cdecl;
    function getString(string_: JString): JString; cdecl;
    function getVibrateTimings: TJavaArray<Int64>; cdecl;
    function hasImage: Boolean; cdecl;
    function isNotification: Boolean; cdecl; overload;
    function paramsForAnalyticsIntent: JBundle; cdecl;
    function paramsWithReservedKeysRemoved: JBundle; cdecl;
  end;
  TJNotificationParams = class(TJavaGenericImport<JNotificationParamsClass, JNotificationParams>) end;

  JRemoteMessageClass = interface(JAbstractSafeParcelableClass)
    ['{FF4D52B2-A0D8-47E1-9AB3-AAF95A7E2513}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetPRIORITY_HIGH: Integer; cdecl;
    {class} function _GetPRIORITY_NORMAL: Integer; cdecl;
    {class} function _GetPRIORITY_UNKNOWN: Integer; cdecl;
    {class} function init(bundle: JBundle): JRemoteMessage; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property PRIORITY_HIGH: Integer read _GetPRIORITY_HIGH;
    {class} property PRIORITY_NORMAL: Integer read _GetPRIORITY_NORMAL;
    {class} property PRIORITY_UNKNOWN: Integer read _GetPRIORITY_UNKNOWN;
  end;

  [JavaSignature('com/google/firebase/messaging/RemoteMessage')]
  JRemoteMessage = interface(JAbstractSafeParcelable)
    ['{09E2896C-9538-4EE3-8DB2-C3E5B3E42838}']
    function getCollapseKey: JString; cdecl;
    function getData: JMap; cdecl;
    function getFrom: JString; cdecl;
    function getMessageId: JString; cdecl;
    function getMessageType: JString; cdecl;
    function getNotification: JRemoteMessage_Notification; cdecl;
    function getOriginalPriority: Integer; cdecl;
    function getPriority: Integer; cdecl;
    function getRawData: TJavaArray<Byte>; cdecl;
    function getSenderId: JString; cdecl;
    function getSentTime: Int64; cdecl;
    function getTo: JString; cdecl;
    function getTtl: Integer; cdecl;
    function toIntent: JIntent; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJRemoteMessage = class(TJavaGenericImport<JRemoteMessageClass, JRemoteMessage>) end;

  // com.google.firebase.messaging.RemoteMessage$1
  JRemoteMessage_NotificationClass = interface(JObjectClass)
    ['{517BD0C8-8E40-492A-87DA-FE02C48669C4}']
  end;

  [JavaSignature('com/google/firebase/messaging/RemoteMessage$Notification')]
  JRemoteMessage_Notification = interface(JObject)
    ['{11FA4879-22C9-4075-BCE7-9205083E60EC}']
    function getBody: JString; cdecl;
    function getBodyLocalizationArgs: TJavaObjectArray<JString>; cdecl;
    function getBodyLocalizationKey: JString; cdecl;
    function getChannelId: JString; cdecl;
    function getClickAction: JString; cdecl;
    function getColor: JString; cdecl;
    function getDefaultLightSettings: Boolean; cdecl;
    function getDefaultSound: Boolean; cdecl;
    function getDefaultVibrateSettings: Boolean; cdecl;
    function getEventTime: JLong; cdecl;
    function getIcon: JString; cdecl;
    function getImageUrl: Jnet_Uri; cdecl;
    function getLightSettings: TJavaArray<Integer>; cdecl;
    function getLink: Jnet_Uri; cdecl;
    function getLocalOnly: Boolean; cdecl;
    function getNotificationCount: JInteger; cdecl;
    function getNotificationPriority: JInteger; cdecl;
    function getSound: JString; cdecl;
    function getSticky: Boolean; cdecl;
    function getTag: JString; cdecl;
    function getTicker: JString; cdecl;
    function getTitle: JString; cdecl;
    function getTitleLocalizationArgs: TJavaObjectArray<JString>; cdecl;
    function getTitleLocalizationKey: JString; cdecl;
    function getVibrateTimings: TJavaArray<Int64>; cdecl;
    function getVisibility: JInteger; cdecl;
  end;
  TJRemoteMessage_Notification = class(TJavaGenericImport<JRemoteMessage_NotificationClass, JRemoteMessage_Notification>) end;

  JStore_TokenClass = interface(JObjectClass)
    ['{F98CDFBB-526B-4B96-9791-39FB9C542C02}']
  end;

  [JavaSignature('com/google/firebase/messaging/Store$Token')]
  JStore_Token = interface(JObject)
    ['{EE90DA7B-BD0E-4EFF-8F0C-A2ED584F6554}']
  end;
  TJStore_Token = class(TJavaGenericImport<JStore_TokenClass, JStore_Token>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JFirebaseApp', TypeInfo(Androidapi.JNI.Firebase.JFirebaseApp));
  //TRegTypes.RegisterType('Androidapi.JNI.Firebase.JFirebaseApp_BackgroundStateChangeListener', TypeInfo(Androidapi.JNI.Firebase.JFirebaseApp_BackgroundStateChangeListener));
  //TRegTypes.RegisterType('Androidapi.JNI.Firebase.JFirebaseAppLifecycleListener', TypeInfo(Androidapi.JNI.Firebase.JFirebaseAppLifecycleListener));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JFirebaseOptions', TypeInfo(Androidapi.JNI.Firebase.JFirebaseOptions));
  //TRegTypes.RegisterType('Androidapi.JNI.Firebase.JFirebaseOptions_1', TypeInfo(Androidapi.JNI.Firebase.JFirebaseOptions_1));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JEnhancedIntentService', TypeInfo(Androidapi.JNI.Firebase.JEnhancedIntentService));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JFirebaseMessaging', TypeInfo(Androidapi.JNI.Firebase.JFirebaseMessaging));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JFirebaseMessagingService', TypeInfo(Androidapi.JNI.Firebase.JFirebaseMessagingService));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JGmsRpc', TypeInfo(Androidapi.JNI.Firebase.JGmsRpc));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.Jmessaging_Metadata', TypeInfo(Androidapi.JNI.Firebase.Jmessaging_Metadata));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JNotificationParams', TypeInfo(Androidapi.JNI.Firebase.JNotificationParams));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JRemoteMessage', TypeInfo(Androidapi.JNI.Firebase.JRemoteMessage));
  //TRegTypes.RegisterType('Androidapi.JNI.Firebase.JRemoteMessage_1', TypeInfo(Androidapi.JNI.Firebase.JRemoteMessage_1));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JRemoteMessage_Notification', TypeInfo(Androidapi.JNI.Firebase.JRemoteMessage_Notification));
  TRegTypes.RegisterType('Androidapi.JNI.Firebase.JStore_Token', TypeInfo(Androidapi.JNI.Firebase.JStore_Token));
end;

initialization
  RegisterTypes;
end.


