{***********************************************************}
{                                                           }
{             CodeGear Delphi Runtime Library               }
{                                                           }
{      Copyright(c) 2018-2022 Embarcadero Technologies, Inc.}
{                  All rights reserved                      }
{                                                           }
{***********************************************************}

unit iOSapi.UserNotifications;

interface

uses
  Macapi.ObjectiveC, iOSApi.CocoaTypes, iOSApi.Foundation, iOSApi.CoreLocation;

const
  UNErrorCodeNotificationsNotAllowed = 1;
  UNErrorCodeAttachmentInvalidURL = 100;
  UNErrorCodeAttachmentUnrecognizedType = 101;
  UNErrorCodeAttachmentInvalidFileSize = 102;
  UNErrorCodeAttachmentNotInDataStore = 103;
  UNErrorCodeAttachmentMoveIntoDataStoreFailed = 104;
  UNErrorCodeAttachmentCorrupt = 105;
  UNErrorCodeNotificationInvalidNoDate = 1400;
  UNErrorCodeNotificationInvalidNoContent = 1401;
  UNNotificationActionOptionAuthenticationRequired = 1;
  UNNotificationActionOptionDestructive = 2;
  UNNotificationActionOptionForeground = 4;
  UNNotificationCategoryOptionCustomDismissAction = 1;
  UNNotificationCategoryOptionAllowInCarPlay = 2;
  UNNotificationCategoryOptionHiddenPreviewsShowTitle = 4;
  UNNotificationCategoryOptionHiddenPreviewsShowSubtitle = 8;
  UNAuthorizationStatusNotDetermined = 0;
  UNAuthorizationStatusDenied = 1;
  UNAuthorizationStatusAuthorized = 2;
  UNAuthorizationStatusProvisional = 3;
  UNShowPreviewsSettingAlways = 0;
  UNShowPreviewsSettingWhenAuthenticated = 1;
  UNShowPreviewsSettingNever = 2;
  UNNotificationSettingNotSupported = 0;
  UNNotificationSettingDisabled = 1;
  UNNotificationSettingEnabled = 2;
  UNAlertStyleNone = 0;
  UNAlertStyleBanner = 1;
  UNAlertStyleAlert = 2;
  UNAuthorizationOptionBadge = 1;
  UNAuthorizationOptionSound = 2;
  UNAuthorizationOptionAlert = 4;
  UNAuthorizationOptionCarPlay = 8;
  UNAuthorizationOptionCriticalAlert = 16;
  UNAuthorizationOptionProvidesAppNotificationSettings = 32;
  UNAuthorizationOptionProvisional = 64;
  UNNotificationPresentationOptionBadge = 1;
  UNNotificationPresentationOptionSound = 2;
  UNNotificationPresentationOptionAlert = 4;

type
  UNNotification = interface;
  UNNotificationAction = interface;
  UNTextInputNotificationAction = interface;
  UNNotificationAttachment = interface;
  UNNotificationCategory = interface;
  UNNotificationContent = interface;
  UNMutableNotificationContent = interface;
  UNNotificationRequest = interface;
  UNNotificationResponse = interface;
  UNTextInputNotificationResponse = interface;
  UNNotificationServiceExtension = interface;
  UNNotificationSettings = interface;
  UNNotificationSound = interface;
  UNNotificationTrigger = interface;
  UNPushNotificationTrigger = interface;
  UNTimeIntervalNotificationTrigger = interface;
  UNCalendarNotificationTrigger = interface;
  UNLocationNotificationTrigger = interface;
  UNUserNotificationCenter = interface;
  UNUserNotificationCenterDelegate = interface;

  PNSError = ^NSError; // Added manually
  UNErrorCode = NSInteger;
  UNNotificationActionOptions = NSInteger;
  UNNotificationCategoryOptions = NSInteger;
  UNAuthorizationStatus = NSInteger;
  UNShowPreviewsSetting = NSInteger;
  UNNotificationSetting = NSInteger;
  UNAlertStyle = NSInteger;
  UNNotificationSoundName = NSString;
  UNAuthorizationOptions = NSInteger;
  UNNotificationPresentationOptions = NSInteger;
  TUNNotificationServiceExtensionBlockMethod1 = procedure(contentToDeliver: UNNotificationContent) of object;
  TUNUserNotificationCenterBlockMethod1 = procedure(granted: Boolean; error: NSError) of object;
  TUNUserNotificationCenterBlockMethod2 = procedure(categories: NSSet) of object;
  TUNUserNotificationCenterBlockMethod3 = procedure(settings: UNNotificationSettings) of object;
  TUNUserNotificationCenterBlockMethod4 = procedure(error: NSError) of object;
  TUNUserNotificationCenterBlockMethod5 = procedure(requests: NSArray) of object;
  TUNUserNotificationCenterBlockMethod6 = procedure(notifications: NSArray) of object;
  TUNUserNotificationCenterDelegateBlockMethod1 = procedure(options: UNNotificationPresentationOptions) of object;
  TUNUserNotificationCenterDelegateBlockMethod2 = procedure of object;

  UNNotificationClass = interface(NSObjectClass)
    ['{6D4428D5-18A3-4913-AD3D-99398B6A89CB}']
  end;
  UNNotification = interface(NSObject)
    ['{AFEE6151-57D2-4811-B5E8-3CDF1DDFBE57}']
    function date: NSDate; cdecl;
    function request: UNNotificationRequest; cdecl;
  end;
  TUNNotification = class(TOCGenericImport<UNNotificationClass, UNNotification>) end;

  UNNotificationActionClass = interface(NSObjectClass)
    ['{3A506339-56AB-4579-A1B2-F08E7A2D6C9A}']
    [MethodName('actionWithIdentifier:title:options:')]
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions): UNNotificationAction; cdecl;
  end;
  UNNotificationAction = interface(NSObject)
    ['{5B3E600A-3DB8-4257-9397-A2590D0B134D}']
    function identifier: NSString; cdecl;
    function options: UNNotificationActionOptions; cdecl;
    function title: NSString; cdecl;
  end;
  TUNNotificationAction = class(TOCGenericImport<UNNotificationActionClass, UNNotificationAction>) end;

  UNTextInputNotificationActionClass = interface(UNNotificationActionClass)
    ['{41186088-9940-4F9B-A0AB-7F9C64417876}']
    [MethodName('actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:')]
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions; textInputButtonTitle: NSString; textInputPlaceholder: NSString): UNTextInputNotificationAction; cdecl;
  end;
  UNTextInputNotificationAction = interface(UNNotificationAction)
    ['{CEF16097-1AA7-4853-9BDB-644840CF0003}']
    function textInputButtonTitle: NSString; cdecl;
    function textInputPlaceholder: NSString; cdecl;
  end;
  TUNTextInputNotificationAction = class(TOCGenericImport<UNTextInputNotificationActionClass, UNTextInputNotificationAction>) end;

  UNNotificationAttachmentClass = interface(NSObjectClass)
    ['{F4099CB1-FB60-46EA-BB24-AF96292EEA45}']
    [MethodName('attachmentWithIdentifier:URL:options:error:')]
    {class} function attachmentWithIdentifier(identifier: NSString; URL: NSURL; options: NSDictionary; error: PNSError): UNNotificationAttachment; cdecl;
  end;
  UNNotificationAttachment = interface(NSObject)
    ['{89AABA8D-7E0C-4FA0-806E-0CC1341166D3}']
    function &type: NSString; cdecl;
    function identifier: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TUNNotificationAttachment = class(TOCGenericImport<UNNotificationAttachmentClass, UNNotificationAttachment>) end;

  UNNotificationCategoryClass = interface(NSObjectClass)
    ['{34B7ADD9-3404-436E-82C5-47E368E6DC17}']
    [MethodName('categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:')]
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray; hiddenPreviewsBodyPlaceholder: NSString; categorySummaryFormat: NSString; options: UNNotificationCategoryOptions): UNNotificationCategory; cdecl; overload;
    [MethodName('categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:')]
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray; hiddenPreviewsBodyPlaceholder: NSString; options: UNNotificationCategoryOptions): UNNotificationCategory; cdecl; overload;
    [MethodName('categoryWithIdentifier:actions:intentIdentifiers:options:')]
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray; options: UNNotificationCategoryOptions): UNNotificationCategory; cdecl; overload;
  end;
  UNNotificationCategory = interface(NSObject)
    ['{26B3E2D1-4E43-42CF-8AB3-344E2455D75F}']
    function actions: NSArray; cdecl;
    function categorySummaryFormat: NSString; cdecl;
    function hiddenPreviewsBodyPlaceholder: NSString; cdecl;
    function identifier: NSString; cdecl;
    function intentIdentifiers: NSArray; cdecl;
    function options: UNNotificationCategoryOptions; cdecl;
  end;
  TUNNotificationCategory = class(TOCGenericImport<UNNotificationCategoryClass, UNNotificationCategory>) end;

  UNNotificationContentClass = interface(NSObjectClass)
    ['{51114586-06DF-4F81-89EE-A596F73F170A}']
  end;
  UNNotificationContent = interface(NSObject)
    ['{BDF46729-F7A4-4780-8699-424E5A342426}']
    function attachments: NSArray; cdecl;
    function badge: NSNumber; cdecl;
    function body: NSString; cdecl;
    function categoryIdentifier: NSString; cdecl;
    function launchImageName: NSString; cdecl;
    function sound: UNNotificationSound; cdecl;
    function subtitle: NSString; cdecl;
    function summaryArgument: NSString; cdecl;
    function summaryArgumentCount: NSUInteger; cdecl;
    function threadIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNNotificationContent = class(TOCGenericImport<UNNotificationContentClass, UNNotificationContent>) end;

  UNMutableNotificationContentClass = interface(UNNotificationContentClass)
    ['{D9F80BFC-AE7B-4186-9FC7-63FF96694C99}']
  end;
  UNMutableNotificationContent = interface(UNNotificationContent)
    ['{C97A9AB7-D2BE-479C-A737-B12B05B35496}']
    function attachments: NSArray; cdecl;
    function badge: NSNumber; cdecl;
    function body: NSString; cdecl;
    function categoryIdentifier: NSString; cdecl;
    function launchImageName: NSString; cdecl;
    procedure setAttachments(attachments: NSArray); cdecl;
    procedure setBadge(badge: NSNumber); cdecl;
    procedure setBody(body: NSString); cdecl;
    procedure setCategoryIdentifier(categoryIdentifier: NSString); cdecl;
    procedure setLaunchImageName(launchImageName: NSString); cdecl;
    procedure setSound(sound: UNNotificationSound); cdecl;
    procedure setSubtitle(subtitle: NSString); cdecl;
    procedure setSummaryArgument(summaryArgument: NSString); cdecl;
    procedure setSummaryArgumentCount(summaryArgumentCount: NSUInteger); cdecl;
    procedure setThreadIdentifier(threadIdentifier: NSString); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function sound: UNNotificationSound; cdecl;
    function subtitle: NSString; cdecl;
    function summaryArgument: NSString; cdecl;
    function summaryArgumentCount: NSUInteger; cdecl;
    function threadIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNMutableNotificationContent = class(TOCGenericImport<UNMutableNotificationContentClass, UNMutableNotificationContent>) end;

  UNNotificationRequestClass = interface(NSObjectClass)
    ['{85DD088D-EECD-4F62-B4D4-A77297CB8C68}']
    [MethodName('requestWithIdentifier:content:trigger:')]
    {class} function requestWithIdentifier(identifier: NSString; content: UNNotificationContent; trigger: UNNotificationTrigger): UNNotificationRequest; cdecl;
  end;
  UNNotificationRequest = interface(NSObject)
    ['{77D79B65-2B67-4255-B40E-7E98F2C91D8C}']
    function content: UNNotificationContent; cdecl;
    function identifier: NSString; cdecl;
    function trigger: UNNotificationTrigger; cdecl;
  end;
  TUNNotificationRequest = class(TOCGenericImport<UNNotificationRequestClass, UNNotificationRequest>) end;

  UNNotificationResponseClass = interface(NSObjectClass)
    ['{822F3160-ED9B-401A-B770-ED0E745C67A9}']
  end;
  UNNotificationResponse = interface(NSObject)
    ['{12B346C3-E55B-4A93-81D3-01699EBAEA43}']
    function actionIdentifier: NSString; cdecl;
    function notification: UNNotification; cdecl;
  end;
  TUNNotificationResponse = class(TOCGenericImport<UNNotificationResponseClass, UNNotificationResponse>) end;

  UNTextInputNotificationResponseClass = interface(UNNotificationResponseClass)
    ['{860727E8-2CB8-4A3F-83AD-27D5D4F1F8F7}']
  end;
  UNTextInputNotificationResponse = interface(UNNotificationResponse)
    ['{C2869DC8-9C1D-4D23-93D0-8D869827AB5D}']
    function userText: NSString; cdecl;
  end;
  TUNTextInputNotificationResponse = class(TOCGenericImport<UNTextInputNotificationResponseClass, UNTextInputNotificationResponse>) end;

  UNNotificationServiceExtensionClass = interface(NSObjectClass)
    ['{6554C320-B0B0-4927-A011-1BA871C24140}']
  end;
  UNNotificationServiceExtension = interface(NSObject)
    ['{16B231C8-D680-4943-95C3-8941E7E01A78}']
    [MethodName('didReceiveNotificationRequest:withContentHandler:')]
    procedure didReceiveNotificationRequest(request: UNNotificationRequest; contentHandler: TUNNotificationServiceExtensionBlockMethod1); cdecl;
    procedure serviceExtensionTimeWillExpire; cdecl;
  end;
  TUNNotificationServiceExtension = class(TOCGenericImport<UNNotificationServiceExtensionClass, UNNotificationServiceExtension>) end;

  UNNotificationSettingsClass = interface(NSObjectClass)
    ['{F6CC36AB-6F7D-4F0D-AA01-F604086FD698}']
  end;
  UNNotificationSettings = interface(NSObject)
    ['{98352662-F2B4-443A-83C3-F839AD9FFADC}']
    function alertSetting: UNNotificationSetting; cdecl;
    function alertStyle: UNAlertStyle; cdecl;
    function authorizationStatus: UNAuthorizationStatus; cdecl;
    function badgeSetting: UNNotificationSetting; cdecl;
    function carPlaySetting: UNNotificationSetting; cdecl;
    function criticalAlertSetting: UNNotificationSetting; cdecl;
    function lockScreenSetting: UNNotificationSetting; cdecl;
    function notificationCenterSetting: UNNotificationSetting; cdecl;
    function providesAppNotificationSettings: Boolean; cdecl;
    function showPreviewsSetting: UNShowPreviewsSetting; cdecl;
    function soundSetting: UNNotificationSetting; cdecl;
  end;
  TUNNotificationSettings = class(TOCGenericImport<UNNotificationSettingsClass, UNNotificationSettings>) end;

  UNNotificationSoundClass = interface(NSObjectClass)
    ['{DE409993-FA94-4455-B5E1-E19B9999BD00}']
    [MethodName('criticalSoundNamed:withAudioVolume:')]
    {class} function criticalSoundNamed(name: UNNotificationSoundName; volume: Single): UNNotificationSound; cdecl; overload;
    {class} function criticalSoundNamed(name: UNNotificationSoundName): UNNotificationSound; cdecl; overload;
    {class} function defaultCriticalSound: UNNotificationSound; cdecl;
    {class} function defaultCriticalSoundWithAudioVolume(volume: Single): UNNotificationSound; cdecl;
    {class} function defaultSound: UNNotificationSound; cdecl;
    {class} function soundNamed(name: UNNotificationSoundName): UNNotificationSound; cdecl;
  end;
  UNNotificationSound = interface(NSObject)
    ['{8C29C92A-EE23-423F-8F97-38735D4E2544}']
  end;
  TUNNotificationSound = class(TOCGenericImport<UNNotificationSoundClass, UNNotificationSound>) end;

  UNNotificationTriggerClass = interface(NSObjectClass)
    ['{209BB768-F422-478F-AC5F-F2EB4856F30C}']
  end;
  UNNotificationTrigger = interface(NSObject)
    ['{274B049C-A4B1-4BB2-BD89-57DB452C626A}']
    function repeats: Boolean; cdecl;
  end;
  TUNNotificationTrigger = class(TOCGenericImport<UNNotificationTriggerClass, UNNotificationTrigger>) end;

  UNPushNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{73252166-B928-4E81-A6DE-C0F7E22B93FF}']
  end;
  UNPushNotificationTrigger = interface(UNNotificationTrigger)
    ['{D5E464F5-4FEC-4A26-BE0B-1643B92408BA}']
  end;
  TUNPushNotificationTrigger = class(TOCGenericImport<UNPushNotificationTriggerClass, UNPushNotificationTrigger>) end;

  UNTimeIntervalNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{65B02DB4-95F6-436E-9DE1-DDE82B8B4E9B}']
    [MethodName('triggerWithTimeInterval:repeats:')]
    {class} function triggerWithTimeInterval(timeInterval: NSTimeInterval; repeats: Boolean): UNTimeIntervalNotificationTrigger; cdecl;
  end;
  UNTimeIntervalNotificationTrigger = interface(UNNotificationTrigger)
    ['{52D4250D-9E69-468B-B73C-840099D60361}']
    function nextTriggerDate: NSDate; cdecl;
    function timeInterval: NSTimeInterval; cdecl;
  end;
  TUNTimeIntervalNotificationTrigger = class(TOCGenericImport<UNTimeIntervalNotificationTriggerClass, UNTimeIntervalNotificationTrigger>) end;

  UNCalendarNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{0D70BDA0-5C5C-412D-98A2-80CBBC21AABB}']
    [MethodName('triggerWithDateMatchingComponents:repeats:')]
    {class} function triggerWithDateMatchingComponents(dateComponents: NSDateComponents; repeats: Boolean): UNCalendarNotificationTrigger; cdecl;
  end;
  UNCalendarNotificationTrigger = interface(UNNotificationTrigger)
    ['{59EDD126-D15F-40D9-B632-A249AE77E127}']
    function dateComponents: NSDateComponents; cdecl;
    function nextTriggerDate: NSDate; cdecl;
  end;
  TUNCalendarNotificationTrigger = class(TOCGenericImport<UNCalendarNotificationTriggerClass, UNCalendarNotificationTrigger>) end;

  UNLocationNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{E90E4004-1202-45BF-B20C-3BEDA4AA43F8}']
    [MethodName('triggerWithRegion:repeats:')]
    {class} function triggerWithRegion(region: CLRegion; repeats: Boolean): UNLocationNotificationTrigger; cdecl;
  end;
  UNLocationNotificationTrigger = interface(UNNotificationTrigger)
    ['{DDDAB13D-5499-41A6-BCFC-5A90D65FB4DF}']
    function region: CLRegion; cdecl;
  end;
  TUNLocationNotificationTrigger = class(TOCGenericImport<UNLocationNotificationTriggerClass, UNLocationNotificationTrigger>) end;

  UNUserNotificationCenterClass = interface(NSObjectClass)
    ['{499732CA-F14F-4A91-82F9-903E1B0C125C}']
    {class} function currentNotificationCenter: UNUserNotificationCenter; cdecl;
  end;
  UNUserNotificationCenter = interface(NSObject)
    ['{23C1DA35-7D1A-483F-ACE5-872CA4068EFF}']
    [MethodName('addNotificationRequest:withCompletionHandler:')]
    procedure addNotificationRequest(request: UNNotificationRequest; completionHandler: TUNUserNotificationCenterBlockMethod4); cdecl;
    function delegate: UNUserNotificationCenterDelegate; cdecl;
    procedure getDeliveredNotificationsWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod6); cdecl;
    procedure getNotificationCategoriesWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod2); cdecl;
    procedure getNotificationSettingsWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod3); cdecl;
    procedure getPendingNotificationRequestsWithCompletionHandler(completionHandler: TUNUserNotificationCenterBlockMethod5); cdecl;
    procedure removeAllDeliveredNotifications; cdecl;
    procedure removeAllPendingNotificationRequests; cdecl;
    procedure removeDeliveredNotificationsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure removePendingNotificationRequestsWithIdentifiers(identifiers: NSArray); cdecl;
    [MethodName('requestAuthorizationWithOptions:completionHandler:')]
    procedure requestAuthorizationWithOptions(options: UNAuthorizationOptions; completionHandler: TUNUserNotificationCenterBlockMethod1); cdecl;
    procedure setDelegate(delegate: UNUserNotificationCenterDelegate); cdecl;
    procedure setNotificationCategories(categories: NSSet); cdecl;
    function supportsContentExtensions: Boolean; cdecl;
  end;
  TUNUserNotificationCenter = class(TOCGenericImport<UNUserNotificationCenterClass, UNUserNotificationCenter>) end;

  UNUserNotificationCenterDelegate = interface(IObjectiveC)
    ['{07BBABF7-BB08-44A7-A654-37D3EEE16DA9}']
    [MethodName('userNotificationCenter:openSettingsForNotification:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); overload; cdecl;
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification; completionHandler: Pointer); overload; cdecl;
  end;

const
  libUserNotifications = '/System/Library/Frameworks/UserNotifications.framework/UserNotifications';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  System.SysUtils, Posix.Dlfcn;

var
  UserNotificationsModule: HMODULE;

initialization
  UserNotificationsModule:= dlopen(MarshaledAString(libUserNotifications), RTLD_LAZY);

finalization
  dlclose(UserNotificationsModule);
{$ENDIF IOS}
end.
