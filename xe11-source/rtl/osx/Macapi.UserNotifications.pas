{***********************************************************}
{                                                           }
{             CodeGear Delphi Runtime Library               }
{                                                           }
{      Copyright(c) 2018-2022 Embarcadero Technologies, Inc.}
{                  All rights reserved                      }
{                                                           }
{***********************************************************}

unit Macapi.UserNotifications;

interface

uses
  Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.CoreFoundation, Macapi.Foundation, Macapi.CoreLocation;

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
  UNNotificationCategoryOptionAllowAnnouncement = 16;
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
  UNAuthorizationOptionAnnouncement = 128;
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
    ['{2A76BE6A-9849-46EC-9F83-706167602998}']
  end;

  UNNotification = interface(NSObject)
    ['{032DB651-A557-4C50-8833-C7AE1A452CD3}']
    function date: NSDate; cdecl;
    function request: UNNotificationRequest; cdecl;
  end;
  TUNNotification = class(TOCGenericImport<UNNotificationClass, UNNotification>) end;

  UNNotificationActionClass = interface(NSObjectClass)
    ['{31F10DF6-1CAF-4839-B45D-DD8573499853}']
    [MethodName('actionWithIdentifier:title:options:')]
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions): Pointer; cdecl;
  end;

  UNNotificationAction = interface(NSObject)
    ['{04370B32-96BF-4DD4-B53F-27F92875B7D2}']
    function identifier: NSString; cdecl;
    function options: UNNotificationActionOptions; cdecl;
    function title: NSString; cdecl;
  end;
  TUNNotificationAction = class(TOCGenericImport<UNNotificationActionClass, UNNotificationAction>) end;

  UNTextInputNotificationActionClass = interface(UNNotificationActionClass)
    ['{E4DE3E52-7871-4BC2-B48E-1E3E404BEF4F}']
    [MethodName('actionWithIdentifier:title:options:textInputButtonTitle:textInputPlaceholder:')]
    {class} function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions;
      textInputButtonTitle: NSString; textInputPlaceholder: NSString): Pointer; cdecl;
  end;

  UNTextInputNotificationAction = interface(UNNotificationAction)
    ['{287D38BD-AB90-44F0-8D44-2B9FCF8FEAF8}']
    function textInputButtonTitle: NSString; cdecl;
    function textInputPlaceholder: NSString; cdecl;
  end;
  TUNTextInputNotificationAction = class(TOCGenericImport<UNTextInputNotificationActionClass, UNTextInputNotificationAction>) end;

  UNNotificationAttachmentClass = interface(NSObjectClass)
    ['{2BC1AFAA-AE9A-49A6-AFD0-5A7E512BEC47}']
    [MethodName('attachmentWithIdentifier:URL:options:error:')]
    {class} function attachmentWithIdentifier(identifier: NSString; URL: NSURL; options: NSDictionary; error: PPointer): Pointer; cdecl;
  end;

  UNNotificationAttachment = interface(NSObject)
    ['{A84310B6-9BDE-42CD-9A5D-0FE9B72D1360}']
    function &type: NSString; cdecl;
    function identifier: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TUNNotificationAttachment = class(TOCGenericImport<UNNotificationAttachmentClass, UNNotificationAttachment>) end;

  UNNotificationCategoryClass = interface(NSObjectClass)
    ['{91D765A8-A668-4472-84F8-108EE4CB1ABA}']
    [MethodName('categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:categorySummaryFormat:options:')]
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray;
      hiddenPreviewsBodyPlaceholder: NSString; categorySummaryFormat: NSString; options: UNNotificationCategoryOptions): Pointer; overload; cdecl;
    [MethodName('categoryWithIdentifier:actions:intentIdentifiers:hiddenPreviewsBodyPlaceholder:options:')]
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray;
      hiddenPreviewsBodyPlaceholder: NSString; options: UNNotificationCategoryOptions): Pointer; overload; cdecl;
    [MethodName('categoryWithIdentifier:actions:intentIdentifiers:options:')]
    {class} function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray;
      options: UNNotificationCategoryOptions): Pointer; overload; cdecl;
  end;

  UNNotificationCategory = interface(NSObject)
    ['{AAEF9285-A966-4C60-8E0C-2E764A824115}']
    function actions: NSArray; cdecl;
    function categorySummaryFormat: NSString; cdecl;
    function hiddenPreviewsBodyPlaceholder: NSString; cdecl;
    function identifier: NSString; cdecl;
    function intentIdentifiers: NSArray; cdecl;
    function options: UNNotificationCategoryOptions; cdecl;
  end;
  TUNNotificationCategory = class(TOCGenericImport<UNNotificationCategoryClass, UNNotificationCategory>) end;

  UNNotificationContentClass = interface(NSObjectClass)
    ['{42E6A8E6-A974-400E-8F17-283AAC622E96}']
  end;

  UNNotificationContent = interface(NSObject)
    ['{4A5E625C-3456-4685-8CAE-090F2FD71C32}']
    function attachments: NSArray; cdecl;
    function badge: NSNumber; cdecl;
    function body: NSString; cdecl;
    function categoryIdentifier: NSString; cdecl;
    function launchImageName: NSString; cdecl;
    function sound: UNNotificationSound; cdecl;
    function subtitle: NSString; cdecl;
    function summaryArgument: NSString; cdecl;
    function summaryArgumentCount: NSUInteger; cdecl;
    function targetContentIdentifier: NSString; cdecl;
    function threadIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNNotificationContent = class(TOCGenericImport<UNNotificationContentClass, UNNotificationContent>) end;

  UNMutableNotificationContentClass = interface(UNNotificationContentClass)
    ['{51CB222C-6749-472B-9E10-68097C060C64}']
  end;

  UNMutableNotificationContent = interface(UNNotificationContent)
    ['{8B90E33C-09C1-4CEA-B776-627E57644544}']
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
    procedure setTargetContentIdentifier(targetContentIdentifier: NSString); cdecl;
    procedure setThreadIdentifier(threadIdentifier: NSString); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function sound: UNNotificationSound; cdecl;
    function subtitle: NSString; cdecl;
    function summaryArgument: NSString; cdecl;
    function summaryArgumentCount: NSUInteger; cdecl;
    function targetContentIdentifier: NSString; cdecl;
    function threadIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TUNMutableNotificationContent = class(TOCGenericImport<UNMutableNotificationContentClass, UNMutableNotificationContent>) end;

  UNNotificationRequestClass = interface(NSObjectClass)
    ['{28E55952-F5C4-4055-B442-F32181D5FEB5}']
    [MethodName('requestWithIdentifier:content:trigger:')]
    {class} function requestWithIdentifier(identifier: NSString; content: UNNotificationContent;
      trigger: UNNotificationTrigger): UNNotificationRequest; cdecl;
  end;

  UNNotificationRequest = interface(NSObject)
    ['{0EC94899-9DDF-415B-AA21-92B219F46A93}']
    function content: UNNotificationContent; cdecl;
    function identifier: NSString; cdecl;
    function trigger: UNNotificationTrigger; cdecl;
  end;
  TUNNotificationRequest = class(TOCGenericImport<UNNotificationRequestClass, UNNotificationRequest>) end;

  UNNotificationResponseClass = interface(NSObjectClass)
    ['{731AFE72-3B2E-4B5F-89ED-4C847ADF5086}']
  end;

  UNNotificationResponse = interface(NSObject)
    ['{329F53FE-80F9-4BF6-AF79-2023E492BD69}']
    function actionIdentifier: NSString; cdecl;
    function notification: UNNotification; cdecl;
  end;
  TUNNotificationResponse = class(TOCGenericImport<UNNotificationResponseClass, UNNotificationResponse>) end;

  UNTextInputNotificationResponseClass = interface(UNNotificationResponseClass)
    ['{A5DC235B-EC9F-4069-A11A-388C794A45A3}']
  end;

  UNTextInputNotificationResponse = interface(UNNotificationResponse)
    ['{CB7B2FF6-AD91-4FB3-AD24-9A4740EC1795}']
    function userText: NSString; cdecl;
  end;
  TUNTextInputNotificationResponse = class(TOCGenericImport<UNTextInputNotificationResponseClass, UNTextInputNotificationResponse>) end;

  UNNotificationServiceExtensionClass = interface(NSObjectClass)
    ['{7ADB12A0-3D8A-46AC-B27B-A83CC5146250}']
  end;

  UNNotificationServiceExtension = interface(NSObject)
    ['{107D0838-AD03-4DC5-8702-217F21A90015}']
    [MethodName('didReceiveNotificationRequest:withContentHandler:')]
    procedure didReceiveNotificationRequest(request: UNNotificationRequest; contentHandler: TUNNotificationServiceExtensionBlockMethod1); cdecl;
    procedure serviceExtensionTimeWillExpire; cdecl;
  end;
  TUNNotificationServiceExtension = class(TOCGenericImport<UNNotificationServiceExtensionClass, UNNotificationServiceExtension>) end;

  UNNotificationSettingsClass = interface(NSObjectClass)
    ['{715391B9-C122-4194-84D6-0BFBFAE59959}']
  end;

  UNNotificationSettings = interface(NSObject)
    ['{C4FCDADE-488F-4C5C-91D4-3709E0831CEA}']
    function alertSetting: UNNotificationSetting; cdecl;
    function alertStyle: UNAlertStyle; cdecl;
    function announcementSetting: UNNotificationSetting; cdecl;
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
    ['{8AEA44B7-ACDA-44CE-A447-0348196CEACA}']
    [MethodName('criticalSoundNamed:withAudioVolume:')]
    {class} function criticalSoundNamed(name: UNNotificationSoundName; volume: Single): UNNotificationSound; overload; cdecl;
    {class} function criticalSoundNamed(name: UNNotificationSoundName): UNNotificationSound; overload; cdecl;
    {class} function defaultCriticalSound: UNNotificationSound; cdecl;
    {class} function defaultCriticalSoundWithAudioVolume(volume: Single): UNNotificationSound; cdecl;
    {class} function defaultSound: UNNotificationSound; cdecl;
    {class} function soundNamed(name: UNNotificationSoundName): UNNotificationSound; cdecl;
  end;

  UNNotificationSound = interface(NSObject)
    ['{982F9AEE-565E-4047-BE9B-682113448882}']
  end;
  TUNNotificationSound = class(TOCGenericImport<UNNotificationSoundClass, UNNotificationSound>) end;

  UNNotificationTriggerClass = interface(NSObjectClass)
    ['{FB491D08-5EA4-420F-A137-BD5A07DA3105}']
  end;

  UNNotificationTrigger = interface(NSObject)
    ['{5D6509F5-7F23-45F9-BAC9-0FCEBC9DA447}']
    function repeats: Boolean; cdecl;
  end;
  TUNNotificationTrigger = class(TOCGenericImport<UNNotificationTriggerClass, UNNotificationTrigger>) end;

  UNPushNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{368CA119-FEB9-4F5F-A4A0-E6B58D4CFC65}']
  end;

  UNPushNotificationTrigger = interface(UNNotificationTrigger)
    ['{0AAB0C66-D8FE-4B0E-BB84-709C1D5503EF}']
  end;
  TUNPushNotificationTrigger = class(TOCGenericImport<UNPushNotificationTriggerClass, UNPushNotificationTrigger>) end;

  UNTimeIntervalNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{5286BA59-50A6-418E-96BF-DA4E8241A7C1}']
    [MethodName('triggerWithTimeInterval:repeats:')]
    {class} function triggerWithTimeInterval(timeInterval: NSTimeInterval; repeats: Boolean): Pointer; cdecl;
  end;

  UNTimeIntervalNotificationTrigger = interface(UNNotificationTrigger)
    ['{CAB18695-3FA9-4FA3-9A2C-5570EEB4EA31}']
    function nextTriggerDate: NSDate; cdecl;
    function timeInterval: NSTimeInterval; cdecl;
  end;
  TUNTimeIntervalNotificationTrigger = class(TOCGenericImport<UNTimeIntervalNotificationTriggerClass, UNTimeIntervalNotificationTrigger>) end;

  UNCalendarNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{8D320FDB-2C72-41FB-8D90-79DA3B88EFF5}']
    [MethodName('triggerWithDateMatchingComponents:repeats:')]
    {class} function triggerWithDateMatchingComponents(dateComponents: NSDateComponents; repeats: Boolean): UNCalendarNotificationTrigger; cdecl;
  end;

  UNCalendarNotificationTrigger = interface(UNNotificationTrigger)
    ['{5BA4CD3B-7678-40BE-80A8-D6BB5936C4FE}']
    function dateComponents: NSDateComponents; cdecl;
    function nextTriggerDate: NSDate; cdecl;
  end;
  TUNCalendarNotificationTrigger = class(TOCGenericImport<UNCalendarNotificationTriggerClass, UNCalendarNotificationTrigger>) end;

  UNLocationNotificationTriggerClass = interface(UNNotificationTriggerClass)
    ['{471413A4-3FE8-479A-BFF8-2A4EF46ACDC4}']
    [MethodName('triggerWithRegion:repeats:')]
    {class} function triggerWithRegion(region: CLRegion; repeats: Boolean): Pointer; cdecl;
  end;

  UNLocationNotificationTrigger = interface(UNNotificationTrigger)
    ['{61C126F4-81BC-4A1D-8100-A8FD2718DB69}']
    function region: CLRegion; cdecl;
  end;
  TUNLocationNotificationTrigger = class(TOCGenericImport<UNLocationNotificationTriggerClass, UNLocationNotificationTrigger>) end;

  UNUserNotificationCenterClass = interface(NSObjectClass)
    ['{7CEE4ABB-BD66-473A-85C0-D51918DC4ABD}']
    {class} function currentNotificationCenter: UNUserNotificationCenter; cdecl;
  end;

  UNUserNotificationCenter = interface(NSObject)
    ['{713C253F-716A-483F-9D97-4E7D65C3D284}']
    [MethodName('addNotificationRequest:withCompletionHandler:')]
    procedure addNotificationRequest(request: UNNotificationRequest; completionHandler: TUNUserNotificationCenterBlockMethod4); cdecl;
    function delegate: Pointer; cdecl;
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
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setNotificationCategories(categories: NSSet); cdecl;
    function supportsContentExtensions: Boolean; cdecl;
  end;
  TUNUserNotificationCenter = class(TOCGenericImport<UNUserNotificationCenterClass, UNUserNotificationCenter>) end;

  UNUserNotificationCenterDelegate = interface(IObjectiveC)
    ['{DCAC2E6A-351E-4E9F-BF06-F04E831DEC9D}']
    [MethodName('userNotificationCenter:openSettingsForNotification:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse;
      completionHandler: Pointer); overload; cdecl;
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification;
      completionHandler: Pointer); overload; cdecl;
  end;

function UNErrorDomain: NSString;
function UNNotificationAttachmentOptionsTypeHintKey: NSString;
function UNNotificationAttachmentOptionsThumbnailHiddenKey: NSString;
function UNNotificationAttachmentOptionsThumbnailClippingRectKey: NSString;
function UNNotificationAttachmentOptionsThumbnailTimeKey: NSString;
function UNNotificationDefaultActionIdentifier: NSString;
function UNNotificationDismissActionIdentifier: NSString;

const
  libUserNotifications = '/System/Library/Frameworks/UserNotifications.framework/UserNotifications';

implementation

uses
  System.SysUtils;

var
  UserNotificationsModule: THandle;

function UNErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNErrorDomain');
end;

function UNNotificationAttachmentOptionsTypeHintKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsTypeHintKey');
end;

function UNNotificationAttachmentOptionsThumbnailHiddenKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailHiddenKey');
end;

function UNNotificationAttachmentOptionsThumbnailClippingRectKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailClippingRectKey');
end;

function UNNotificationAttachmentOptionsThumbnailTimeKey: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailTimeKey');
end;

function UNNotificationDefaultActionIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationDefaultActionIdentifier');
end;

function UNNotificationDismissActionIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationDismissActionIdentifier');
end;

initialization
  UserNotificationsModule := LoadLibrary(libUserNotifications);

finalization
  if UserNotificationsModule <> 0 then
    FreeLibrary(UserNotificationsModule);

end.
