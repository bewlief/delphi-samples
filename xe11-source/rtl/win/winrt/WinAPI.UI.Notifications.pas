{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI.Notifications;

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
  // Alias type definitions for types moved from this unit

  IToastDismissedEventArgs = Winapi.CommonTypes.IToastDismissedEventArgs;
  PIToastDismissedEventArgs = Winapi.CommonTypes.PIToastDismissedEventArgs;
  IToastFailedEventArgs = Winapi.CommonTypes.IToastFailedEventArgs;
  PIToastFailedEventArgs = Winapi.CommonTypes.PIToastFailedEventArgs;
  IToastNotification = Winapi.CommonTypes.IToastNotification;
  PIToastNotification = Winapi.CommonTypes.PIToastNotification;
  ToastDismissalReason = Winapi.CommonTypes.ToastDismissalReason;
  PToastDismissalReason = Winapi.CommonTypes.PToastDismissalReason;
  TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs_Delegate_Base;
  TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs;
  PTypedEventHandler_2__IToastNotification__IToastDismissedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IToastNotification__IToastDismissedEventArgs;
  TypedEventHandler_2__IToastNotification__IToastFailedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IToastNotification__IToastFailedEventArgs_Delegate_Base;
  TypedEventHandler_2__IToastNotification__IToastFailedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IToastNotification__IToastFailedEventArgs;
  PTypedEventHandler_2__IToastNotification__IToastFailedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IToastNotification__IToastFailedEventArgs;

  // Forward declarations for interfaces

  // Windows.UI.Notifications.ITileNotification
  ITileNotification = interface;
  PITileNotification = ^ITileNotification;

  // Windows.UI.Notifications.IBadgeNotification
  IBadgeNotification = interface;
  PIBadgeNotification = ^IBadgeNotification;

  // Windows.UI.Notifications.INotificationData
  INotificationData = interface;
  PINotificationData = ^INotificationData;

  // Windows.UI.Notifications.IScheduledToastNotification
  IScheduledToastNotification = interface;
  PIScheduledToastNotification = ^IScheduledToastNotification;

  // Windows.UI.Notifications.IToastActivatedEventArgs
  IToastActivatedEventArgs = interface;
  PIToastActivatedEventArgs = ^IToastActivatedEventArgs;

  // Windows.UI.Notifications.IToastActivatedEventArgs2
  IToastActivatedEventArgs2 = interface;
  PIToastActivatedEventArgs2 = ^IToastActivatedEventArgs2;

  // Windows.UI.Notifications.IToastCollection
  IToastCollection = interface;
  PIToastCollection = ^IToastCollection;

  // Windows.UI.Notifications.IToastCollectionFactory
  IToastCollectionFactory = interface;
  PIToastCollectionFactory = ^IToastCollectionFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IToastCollection>
  IIterator_1__IToastCollection = interface;
  PIIterator_1__IToastCollection = ^IIterator_1__IToastCollection;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IToastCollection>
  IIterable_1__IToastCollection = interface;
  PIIterable_1__IToastCollection = ^IIterable_1__IToastCollection;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>
  IVectorView_1__IToastCollection = interface;
  PIVectorView_1__IToastCollection = ^IVectorView_1__IToastCollection;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection = ^AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>>
  IAsyncOperation_1__IVectorView_1__IToastCollection = interface;
  PIAsyncOperation_1__IVectorView_1__IToastCollection = ^IAsyncOperation_1__IVectorView_1__IToastCollection;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastCollection>
  AsyncOperationCompletedHandler_1__IToastCollection = interface;
  PAsyncOperationCompletedHandler_1__IToastCollection = ^AsyncOperationCompletedHandler_1__IToastCollection;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastCollection>
  IAsyncOperation_1__IToastCollection = interface;
  PIAsyncOperation_1__IToastCollection = ^IAsyncOperation_1__IToastCollection;

  // Windows.UI.Notifications.IToastCollectionManager
  IToastCollectionManager = interface;
  PIToastCollectionManager = ^IToastCollectionManager;

  // Windows.UI.Notifications.IToastNotification2
  IToastNotification2 = interface;
  PIToastNotification2 = ^IToastNotification2;

  // Windows.UI.Notifications.IToastNotification3
  IToastNotification3 = interface;
  PIToastNotification3 = ^IToastNotification3;

  // Windows.UI.Notifications.IToastNotification4
  IToastNotification4 = interface;
  PIToastNotification4 = ^IToastNotification4;

  // Windows.UI.Notifications.IToastNotification6
  IToastNotification6 = interface;
  PIToastNotification6 = ^IToastNotification6;

  // Windows.UI.Notifications.IToastNotificationFactory
  IToastNotificationFactory = interface;
  PIToastNotificationFactory = ^IToastNotificationFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IToastNotification>
  IIterator_1__IToastNotification = interface;
  PIIterator_1__IToastNotification = ^IIterator_1__IToastNotification;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IToastNotification>
  IIterable_1__IToastNotification = interface;
  PIIterable_1__IToastNotification = ^IIterable_1__IToastNotification;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastNotification>
  IVectorView_1__IToastNotification = interface;
  PIVectorView_1__IToastNotification = ^IVectorView_1__IToastNotification;

  // Windows.UI.Notifications.IToastNotificationHistory2
  IToastNotificationHistory2 = interface;
  PIToastNotificationHistory2 = ^IToastNotificationHistory2;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IScheduledToastNotification>
  IIterator_1__IScheduledToastNotification = interface;
  PIIterator_1__IScheduledToastNotification = ^IIterator_1__IScheduledToastNotification;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IScheduledToastNotification>
  IIterable_1__IScheduledToastNotification = interface;
  PIIterable_1__IScheduledToastNotification = ^IIterable_1__IScheduledToastNotification;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IScheduledToastNotification>
  IVectorView_1__IScheduledToastNotification = interface;
  PIVectorView_1__IScheduledToastNotification = ^IVectorView_1__IScheduledToastNotification;

  // Windows.UI.Notifications.IToastNotifier
  IToastNotifier = interface;
  PIToastNotifier = ^IToastNotifier;

  // Windows.UI.Notifications.IToastNotificationManagerForUser
  IToastNotificationManagerForUser = interface;
  PIToastNotificationManagerForUser = ^IToastNotificationManagerForUser;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastNotifier>
  AsyncOperationCompletedHandler_1__IToastNotifier = interface;
  PAsyncOperationCompletedHandler_1__IToastNotifier = ^AsyncOperationCompletedHandler_1__IToastNotifier;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastNotifier>
  IAsyncOperation_1__IToastNotifier = interface;
  PIAsyncOperation_1__IToastNotifier = ^IAsyncOperation_1__IToastNotifier;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastNotificationHistory2>
  AsyncOperationCompletedHandler_1__IToastNotificationHistory2 = interface;
  PAsyncOperationCompletedHandler_1__IToastNotificationHistory2 = ^AsyncOperationCompletedHandler_1__IToastNotificationHistory2;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastNotificationHistory2>
  IAsyncOperation_1__IToastNotificationHistory2 = interface;
  PIAsyncOperation_1__IToastNotificationHistory2 = ^IAsyncOperation_1__IToastNotificationHistory2;

  // Windows.UI.Notifications.IToastNotificationManagerStatics
  IToastNotificationManagerStatics = interface;
  PIToastNotificationManagerStatics = ^IToastNotificationManagerStatics;

  // Windows.UI.Notifications.IToastNotificationManagerStatics2
  IToastNotificationManagerStatics2 = interface;
  PIToastNotificationManagerStatics2 = ^IToastNotificationManagerStatics2;

  // Windows.UI.Notifications.IToastNotificationManagerStatics4
  IToastNotificationManagerStatics4 = interface;
  PIToastNotificationManagerStatics4 = ^IToastNotificationManagerStatics4;

  // Windows.UI.Notifications.IToastNotificationManagerStatics5
  IToastNotificationManagerStatics5 = interface;
  PIToastNotificationManagerStatics5 = ^IToastNotificationManagerStatics5;

  // Windows.UI.Notifications.IToastNotifier2
  IToastNotifier2 = interface;
  PIToastNotifier2 = ^IToastNotifier2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.Management.UserNotificationListenerAccessStatus>
  AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus = ^AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.Management.UserNotificationListenerAccessStatus>
  IAsyncOperation_1__Management_UserNotificationListenerAccessStatus = interface;
  PIAsyncOperation_1__Management_UserNotificationListenerAccessStatus = ^IAsyncOperation_1__Management_UserNotificationListenerAccessStatus;

  // Windows.UI.Notifications Enums

  // Windows.UI.Notifications.AdaptiveNotificationContentKind
  AdaptiveNotificationContentKind = (
    Text = 0
  );
  PAdaptiveNotificationContentKind = ^AdaptiveNotificationContentKind;

  // Windows.UI.Notifications.BadgeTemplateType
  BadgeTemplateType = (
    BadgeGlyph = 0,
    BadgeNumber = 1
  );
  PBadgeTemplateType = ^BadgeTemplateType;

  // Windows.UI.Notifications.Management.UserNotificationListenerAccessStatus
  Management_UserNotificationListenerAccessStatus = (
    Unspecified = 0,
    Allowed = 1,
    Denied = 2
  );
  PManagement_UserNotificationListenerAccessStatus = ^Management_UserNotificationListenerAccessStatus;

  // Windows.UI.Notifications.NotificationKinds
  NotificationKinds = (
    Unknown = 0,
    Toast = 1
  );
  PNotificationKinds = ^NotificationKinds;

  // Windows.UI.Notifications.NotificationMirroring
  NotificationMirroring = (
    Allowed = 0,
    Disabled = 1
  );
  PNotificationMirroring = ^NotificationMirroring;

  // Windows.UI.Notifications.NotificationSetting
  NotificationSetting = (
    Enabled = 0,
    DisabledForApplication = 1,
    DisabledForUser = 2,
    DisabledByGroupPolicy = 3,
    DisabledByManifest = 4
  );
  PNotificationSetting = ^NotificationSetting;

  // Windows.UI.Notifications.NotificationUpdateResult
  NotificationUpdateResult = (
    Succeeded = 0,
    Failed = 1,
    NotificationNotFound = 2
  );
  PNotificationUpdateResult = ^NotificationUpdateResult;

  // Windows.UI.Notifications.PeriodicUpdateRecurrence
  PeriodicUpdateRecurrence = (
    HalfHour = 0,
    Hour = 1,
    SixHours = 2,
    TwelveHours = 3,
    Daily = 4
  );
  PPeriodicUpdateRecurrence = ^PeriodicUpdateRecurrence;

  // Windows.UI.Notifications.TileFlyoutTemplateType
  TileFlyoutTemplateType = (
    TileFlyoutTemplate01 = 0
  );
  PTileFlyoutTemplateType = ^TileFlyoutTemplateType;

  // Windows.UI.Notifications.TileTemplateType
  TileTemplateType = (
    TileSquareImage = 0,
    TileSquareBlock = 1,
    TileSquareText01 = 2,
    TileSquareText02 = 3,
    TileSquareText03 = 4,
    TileSquareText04 = 5,
    TileSquarePeekImageAndText01 = 6,
    TileSquarePeekImageAndText02 = 7,
    TileSquarePeekImageAndText03 = 8,
    TileSquarePeekImageAndText04 = 9,
    TileWideImage = 10,
    TileWideImageCollection = 11,
    TileWideImageAndText01 = 12,
    TileWideImageAndText02 = 13,
    TileWideBlockAndText01 = 14,
    TileWideBlockAndText02 = 15,
    TileWidePeekImageCollection01 = 16,
    TileWidePeekImageCollection02 = 17,
    TileWidePeekImageCollection03 = 18,
    TileWidePeekImageCollection04 = 19,
    TileWidePeekImageCollection05 = 20,
    TileWidePeekImageCollection06 = 21,
    TileWidePeekImageAndText01 = 22,
    TileWidePeekImageAndText02 = 23,
    TileWidePeekImage01 = 24,
    TileWidePeekImage02 = 25,
    TileWidePeekImage03 = 26,
    TileWidePeekImage04 = 27,
    TileWidePeekImage05 = 28,
    TileWidePeekImage06 = 29,
    TileWideSmallImageAndText01 = 30,
    TileWideSmallImageAndText02 = 31,
    TileWideSmallImageAndText03 = 32,
    TileWideSmallImageAndText04 = 33,
    TileWideSmallImageAndText05 = 34,
    TileWideText01 = 35,
    TileWideText02 = 36,
    TileWideText03 = 37,
    TileWideText04 = 38,
    TileWideText05 = 39,
    TileWideText06 = 40,
    TileWideText07 = 41,
    TileWideText08 = 42,
    TileWideText09 = 43,
    TileWideText10 = 44,
    TileWideText11 = 45,
    TileSquare150x150Image = 0,
    TileSquare150x150Block = 1,
    TileSquare150x150Text01 = 2,
    TileSquare150x150Text02 = 3,
    TileSquare150x150Text03 = 4,
    TileSquare150x150Text04 = 5,
    TileSquare150x150PeekImageAndText01 = 6,
    TileSquare150x150PeekImageAndText02 = 7,
    TileSquare150x150PeekImageAndText03 = 8,
    TileSquare150x150PeekImageAndText04 = 9,
    TileWide310x150Image = 10,
    TileWide310x150ImageCollection = 11,
    TileWide310x150ImageAndText01 = 12,
    TileWide310x150ImageAndText02 = 13,
    TileWide310x150BlockAndText01 = 14,
    TileWide310x150BlockAndText02 = 15,
    TileWide310x150PeekImageCollection01 = 16,
    TileWide310x150PeekImageCollection02 = 17,
    TileWide310x150PeekImageCollection03 = 18,
    TileWide310x150PeekImageCollection04 = 19,
    TileWide310x150PeekImageCollection05 = 20,
    TileWide310x150PeekImageCollection06 = 21,
    TileWide310x150PeekImageAndText01 = 22,
    TileWide310x150PeekImageAndText02 = 23,
    TileWide310x150PeekImage01 = 24,
    TileWide310x150PeekImage02 = 25,
    TileWide310x150PeekImage03 = 26,
    TileWide310x150PeekImage04 = 27,
    TileWide310x150PeekImage05 = 28,
    TileWide310x150PeekImage06 = 29,
    TileWide310x150SmallImageAndText01 = 30,
    TileWide310x150SmallImageAndText02 = 31,
    TileWide310x150SmallImageAndText03 = 32,
    TileWide310x150SmallImageAndText04 = 33,
    TileWide310x150SmallImageAndText05 = 34,
    TileWide310x150Text01 = 35,
    TileWide310x150Text02 = 36,
    TileWide310x150Text03 = 37,
    TileWide310x150Text04 = 38,
    TileWide310x150Text05 = 39,
    TileWide310x150Text06 = 40,
    TileWide310x150Text07 = 41,
    TileWide310x150Text08 = 42,
    TileWide310x150Text09 = 43,
    TileWide310x150Text10 = 44,
    TileWide310x150Text11 = 45,
    TileSquare310x310BlockAndText01 = 46,
    TileSquare310x310BlockAndText02 = 47,
    TileSquare310x310Image = 48,
    TileSquare310x310ImageAndText01 = 49,
    TileSquare310x310ImageAndText02 = 50,
    TileSquare310x310ImageAndTextOverlay01 = 51,
    TileSquare310x310ImageAndTextOverlay02 = 52,
    TileSquare310x310ImageAndTextOverlay03 = 53,
    TileSquare310x310ImageCollectionAndText01 = 54,
    TileSquare310x310ImageCollectionAndText02 = 55,
    TileSquare310x310ImageCollection = 56,
    TileSquare310x310SmallImagesAndTextList01 = 57,
    TileSquare310x310SmallImagesAndTextList02 = 58,
    TileSquare310x310SmallImagesAndTextList03 = 59,
    TileSquare310x310SmallImagesAndTextList04 = 60,
    TileSquare310x310Text01 = 61,
    TileSquare310x310Text02 = 62,
    TileSquare310x310Text03 = 63,
    TileSquare310x310Text04 = 64,
    TileSquare310x310Text05 = 65,
    TileSquare310x310Text06 = 66,
    TileSquare310x310Text07 = 67,
    TileSquare310x310Text08 = 68,
    TileSquare310x310TextList01 = 69,
    TileSquare310x310TextList02 = 70,
    TileSquare310x310TextList03 = 71,
    TileSquare310x310SmallImageAndText01 = 72,
    TileSquare310x310SmallImagesAndTextList05 = 73,
    TileSquare310x310Text09 = 74,
    TileSquare71x71IconWithBadge = 75,
    TileSquare150x150IconWithBadge = 76,
    TileWide310x150IconWithBadgeAndText = 77,
    TileSquare71x71Image = 78,
    TileTall150x310Image = 79
  );
  PTileTemplateType = ^TileTemplateType;

  // Windows.UI.Notifications.ToastHistoryChangedType
  ToastHistoryChangedType = (
    Cleared = 0,
    Removed = 1,
    Expired = 2,
    Added = 3
  );
  PToastHistoryChangedType = ^ToastHistoryChangedType;

  // Windows.UI.Notifications.ToastNotificationPriority
  ToastNotificationPriority = (
    Default = 0,
    High = 1
  );
  PToastNotificationPriority = ^ToastNotificationPriority;

  // Windows.UI.Notifications.ToastTemplateType
  ToastTemplateType = (
    ToastImageAndText01 = 0,
    ToastImageAndText02 = 1,
    ToastImageAndText03 = 2,
    ToastImageAndText04 = 3,
    ToastText01 = 4,
    ToastText02 = 5,
    ToastText03 = 6,
    ToastText04 = 7
  );
  PToastTemplateType = ^ToastTemplateType;

  // Windows.UI.Notifications.UserNotificationChangedKind
  UserNotificationChangedKind = (
    Added = 0,
    Removed = 1
  );
  PUserNotificationChangedKind = ^UserNotificationChangedKind;

  // Windows.UI.Notifications Interfaces

  // UsedAPI Interface
  // Windows.UI.Notifications.ITileNotification
  ITileNotification = interface(IInspectable)
  ['{EBAEC8FA-50EC-4C18-B4D0-3AF02E5540AB}']
    function get_Content: Xml_Dom_IXmlDocument; safecall;
    procedure put_ExpirationTime(value: IReference_1__DateTime); safecall;
    function get_ExpirationTime: IReference_1__DateTime; safecall;
    procedure put_Tag(value: HSTRING); safecall;
    function get_Tag: HSTRING; safecall;
    property Content: Xml_Dom_IXmlDocument read get_Content;
    property ExpirationTime: IReference_1__DateTime read get_ExpirationTime write put_ExpirationTime;
    property Tag: HSTRING read get_Tag write put_Tag;
  end;

  // UsedAPI Interface
  // Windows.UI.Notifications.IBadgeNotification
  IBadgeNotification = interface(IInspectable)
  ['{075CB4CA-D08A-4E2F-9233-7E289C1F7722}']
    function get_Content: Xml_Dom_IXmlDocument; safecall;
    procedure put_ExpirationTime(value: IReference_1__DateTime); safecall;
    function get_ExpirationTime: IReference_1__DateTime; safecall;
    property Content: Xml_Dom_IXmlDocument read get_Content;
    property ExpirationTime: IReference_1__DateTime read get_ExpirationTime write put_ExpirationTime;
  end;

  // UsedAPI Interface
  // Windows.UI.Notifications.INotificationData
  INotificationData = interface(IInspectable)
  ['{9FFD2312-9D6A-4AAF-B6AC-FF17F0C1F280}']
    function get_Values: IMap_2__HSTRING__HSTRING; safecall;
    function get_SequenceNumber: Cardinal; safecall;
    procedure put_SequenceNumber(value: Cardinal); safecall;
    property SequenceNumber: Cardinal read get_SequenceNumber write put_SequenceNumber;
    property Values: IMap_2__HSTRING__HSTRING read get_Values;
  end;

  // UsedAPI Interface
  // Windows.UI.Notifications.IScheduledToastNotification
  IScheduledToastNotification = interface(IInspectable)
  ['{79F577F8-0DE7-48CD-9740-9B370490C838}']
    function get_Content: Xml_Dom_IXmlDocument; safecall;
    function get_DeliveryTime: DateTime; safecall;
    function get_SnoozeInterval: IReference_1__TimeSpan; safecall;
    function get_MaximumSnoozeCount: Cardinal; safecall;
    procedure put_Id(value: HSTRING); safecall;
    function get_Id: HSTRING; safecall;
    property Content: Xml_Dom_IXmlDocument read get_Content;
    property DeliveryTime: DateTime read get_DeliveryTime;
    property Id: HSTRING read get_Id write put_Id;
    property MaximumSnoozeCount: Cardinal read get_MaximumSnoozeCount;
    property SnoozeInterval: IReference_1__TimeSpan read get_SnoozeInterval;
  end;

  // Windows.UI.Notifications.IToastActivatedEventArgs
  IToastActivatedEventArgs = interface(IInspectable)
  ['{E3BF92F3-C197-436F-8265-0625824F8DAC}']
    function get_Arguments: HSTRING; safecall;
    property Arguments: HSTRING read get_Arguments;
  end;

  // Windows.UI.Notifications.IToastActivatedEventArgs2
  IToastActivatedEventArgs2 = interface(IInspectable)
  ['{AB7DA512-CC61-568E-81BE-304AC31038FA}']
    function get_UserInput: IPropertySet; safecall;
    property UserInput: IPropertySet read get_UserInput;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastCollection
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastCollection)]
  IToastCollection = interface(IInspectable)
  ['{0A8BC3B0-E0BE-4858-BC2A-89DFE0B32863}']
    function get_Id: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    procedure put_DisplayName(value: HSTRING); safecall;
    function get_LaunchArgs: HSTRING; safecall;
    procedure put_LaunchArgs(value: HSTRING); safecall;
    function get_Icon: IUriRuntimeClass; safecall;
    procedure put_Icon(value: IUriRuntimeClass); safecall;
    property DisplayName: HSTRING read get_DisplayName write put_DisplayName;
    property Icon: IUriRuntimeClass read get_Icon write put_Icon;
    property Id: HSTRING read get_Id;
    property LaunchArgs: HSTRING read get_LaunchArgs write put_LaunchArgs;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastCollectionFactory
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastCollection)]
  IToastCollectionFactory = interface(IInspectable)
  ['{164DD3D7-73C4-44F7-B4FF-FB6D4BF1F4C6}']
    function CreateInstance(collectionId: HSTRING; displayName: HSTRING; launchArgs: HSTRING; iconUri: IUriRuntimeClass): IToastCollection; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IToastCollection>
  IIterator_1__IToastCollection_Base = interface(IInspectable)
  ['{1512ED75-8C74-5520-AC88-134A1403F7AD}']
    function get_Current: IToastCollection; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIToastCollection): Cardinal; safecall;
    property Current: IToastCollection read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IToastCollection>
  IIterator_1__IToastCollection = interface(IIterator_1__IToastCollection_Base)
  ['{C702E18A-98D1-5340-8D67-47A83A883DB4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IToastCollection>
  IIterable_1__IToastCollection_Base = interface(IInspectable)
  ['{8928D527-DB5D-5A10-AE9B-430FA0906E74}']
    function First: IIterator_1__IToastCollection; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IToastCollection>
  IIterable_1__IToastCollection = interface(IIterable_1__IToastCollection_Base)
  ['{0759B5B7-BD0A-539D-9B28-2F5CD7024463}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>
  IVectorView_1__IToastCollection = interface(IInspectable)
  ['{482CB04D-99D2-5D76-92E5-0296B2FA175F}']
    function GetAt(index: Cardinal): IToastCollection; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IToastCollection; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIToastCollection): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection_Delegate_Base = interface(IUnknown)
  ['{4650E069-3052-530E-BC38-93C411773B77}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IToastCollection; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection_Delegate_Base)
  ['{7CFB2450-F6AA-5D5E-A493-4E8CB7A6E050}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>>
  IAsyncOperation_1__IVectorView_1__IToastCollection_Base = interface(IInspectable)
  ['{34D4FA14-252B-5CB4-A7DA-971EE5DAEC7C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection; safecall;
    function GetResults: IVectorView_1__IToastCollection; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IToastCollection read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastCollection>>
  IAsyncOperation_1__IVectorView_1__IToastCollection = interface(IAsyncOperation_1__IVectorView_1__IToastCollection_Base)
  ['{F69FC18A-04D8-58C0-A4FE-9E26E3CB8B1B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastCollection>
  AsyncOperationCompletedHandler_1__IToastCollection_Delegate_Base = interface(IUnknown)
  ['{8D44CA1E-15D7-5BCB-B002-384C87171C74}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IToastCollection; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastCollection>
  AsyncOperationCompletedHandler_1__IToastCollection = interface(AsyncOperationCompletedHandler_1__IToastCollection_Delegate_Base)
  ['{28FF2BAB-98D8-5909-B9AA-9FDBD29B7DC8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastCollection>
  IAsyncOperation_1__IToastCollection_Base = interface(IInspectable)
  ['{9310EC47-9F0A-5999-80C2-4B31E9F77E8E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IToastCollection); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IToastCollection; safecall;
    function GetResults: IToastCollection; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IToastCollection read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastCollection>
  IAsyncOperation_1__IToastCollection = interface(IAsyncOperation_1__IToastCollection_Base)
  ['{962C3201-2826-5C51-BC22-A428E197900F}']
  end;

  // Windows.UI.Notifications.IToastCollectionManager
  IToastCollectionManager = interface(IInspectable)
  ['{2A1821FE-179D-49BC-B79D-A527920D3665}']
    function SaveToastCollectionAsync(collection: IToastCollection): IAsyncAction; safecall;
    function FindAllToastCollectionsAsync: IAsyncOperation_1__IVectorView_1__IToastCollection; safecall;
    function GetToastCollectionAsync(collectionId: HSTRING): IAsyncOperation_1__IToastCollection; safecall;
    function RemoveToastCollectionAsync(collectionId: HSTRING): IAsyncAction; safecall;
    function RemoveAllToastCollectionsAsync: IAsyncAction; safecall;
    function get_User: IUser; safecall;
    function get_AppId: HSTRING; safecall;
    property AppId: HSTRING read get_AppId;
    property User: IUser read get_User;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotification2
  IToastNotification2 = interface(IInspectable)
  ['{9DFB9FD1-143A-490E-90BF-B9FBA7132DE7}']
    procedure put_Tag(value: HSTRING); safecall;
    function get_Tag: HSTRING; safecall;
    procedure put_Group(value: HSTRING); safecall;
    function get_Group: HSTRING; safecall;
    procedure put_SuppressPopup(value: Boolean); safecall;
    function get_SuppressPopup: Boolean; safecall;
    property Group: HSTRING read get_Group write put_Group;
    property SuppressPopup: Boolean read get_SuppressPopup write put_SuppressPopup;
    property Tag: HSTRING read get_Tag write put_Tag;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotification3
  IToastNotification3 = interface(IInspectable)
  ['{31E8AED8-8141-4F99-BC0A-C4ED21297D77}']
    function get_NotificationMirroring: NotificationMirroring; safecall;
    procedure put_NotificationMirroring(value: NotificationMirroring); safecall;
    function get_RemoteId: HSTRING; safecall;
    procedure put_RemoteId(value: HSTRING); safecall;
    property NotificationMirroring_: NotificationMirroring read get_NotificationMirroring write put_NotificationMirroring;
    property RemoteId: HSTRING read get_RemoteId write put_RemoteId;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotification4
  IToastNotification4 = interface(IInspectable)
  ['{15154935-28EA-4727-88E9-C58680E2D118}']
    function get_Data: INotificationData; safecall;
    procedure put_Data(value: INotificationData); safecall;
    function get_Priority: ToastNotificationPriority; safecall;
    procedure put_Priority(value: ToastNotificationPriority); safecall;
    property Data: INotificationData read get_Data write put_Data;
    property Priority: ToastNotificationPriority read get_Priority write put_Priority;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotification6
  IToastNotification6 = interface(IInspectable)
  ['{43EBFE53-89AE-5C1E-A279-3AECFE9B6F54}']
    function get_ExpiresOnReboot: Boolean; safecall;
    procedure put_ExpiresOnReboot(value: Boolean); safecall;
    property ExpiresOnReboot: Boolean read get_ExpiresOnReboot write put_ExpiresOnReboot;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotificationFactory
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastNotification)]
  IToastNotificationFactory = interface(IInspectable)
  ['{04124B20-82C6-4229-B109-FD9ED4662B53}']
    function CreateToastNotification(content: Xml_Dom_IXmlDocument): IToastNotification; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IToastNotification>
  IIterator_1__IToastNotification_Base = interface(IInspectable)
  ['{FE1E726A-3AA9-5D98-B19B-97E3E17EEC7B}']
    function get_Current: IToastNotification; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIToastNotification): Cardinal; safecall;
    property Current: IToastNotification read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IToastNotification>
  IIterator_1__IToastNotification = interface(IIterator_1__IToastNotification_Base)
  ['{ABA5D40A-4EF1-5090-8A7E-B2B779BCCBB7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IToastNotification>
  IIterable_1__IToastNotification_Base = interface(IInspectable)
  ['{52C9428B-D37A-554D-BF55-B8685D5F552D}']
    function First: IIterator_1__IToastNotification; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IToastNotification>
  IIterable_1__IToastNotification = interface(IIterable_1__IToastNotification_Base)
  ['{DD43185C-C276-593B-B076-B598FCD176DB}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IToastNotification>
  IVectorView_1__IToastNotification = interface(IInspectable)
  ['{82CB31B6-357F-581F-A6C9-CBCB308BAEC8}']
    function GetAt(index: Cardinal): IToastNotification; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IToastNotification; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIToastNotification): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotificationHistory2
  IToastNotificationHistory2 = interface(IInspectable)
  ['{3BC3D253-2F31-4092-9129-8AD5ABF067DA}']
    function GetHistory: IVectorView_1__IToastNotification; overload; safecall;
    function GetHistory(applicationId: HSTRING): IVectorView_1__IToastNotification; overload; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IScheduledToastNotification>
  IIterator_1__IScheduledToastNotification_Base = interface(IInspectable)
  ['{304238B6-888A-5DD2-96CD-BFCA8927483B}']
    function get_Current: IScheduledToastNotification; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIScheduledToastNotification): Cardinal; safecall;
    property Current: IScheduledToastNotification read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Notifications.IScheduledToastNotification>
  IIterator_1__IScheduledToastNotification = interface(IIterator_1__IScheduledToastNotification_Base)
  ['{6C34AAF7-B3EF-5E61-ADA9-2024702696C6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IScheduledToastNotification>
  IIterable_1__IScheduledToastNotification_Base = interface(IInspectable)
  ['{7A7B2A51-C182-5846-A861-4F9C036F24AD}']
    function First: IIterator_1__IScheduledToastNotification; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Notifications.IScheduledToastNotification>
  IIterable_1__IScheduledToastNotification = interface(IIterable_1__IScheduledToastNotification_Base)
  ['{92B73B0F-5001-54D8-B43C-AAB6D146FBC9}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Notifications.IScheduledToastNotification>
  IVectorView_1__IScheduledToastNotification = interface(IInspectable)
  ['{762EC9E6-C409-5C27-B61F-C480347C1816}']
    function GetAt(index: Cardinal): IScheduledToastNotification; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IScheduledToastNotification; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIScheduledToastNotification): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotifier
  IToastNotifier = interface(IInspectable)
  ['{75927B93-03F3-41EC-91D3-6E5BAC1B38E7}']
    procedure Show(notification: IToastNotification); safecall;
    procedure Hide(notification: IToastNotification); safecall;
    function get_Setting: NotificationSetting; safecall;
    procedure AddToSchedule(scheduledToast: IScheduledToastNotification); safecall;
    procedure RemoveFromSchedule(scheduledToast: IScheduledToastNotification); safecall;
    function GetScheduledToastNotifications: IVectorView_1__IScheduledToastNotification; safecall;
    property Setting: NotificationSetting read get_Setting;
  end;

  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotificationManagerForUser
  IToastNotificationManagerForUser = interface(IInspectable)
  ['{79AB57F6-43FE-487B-8A7F-99567200AE94}']
    function CreateToastNotifier: IToastNotifier; overload; safecall;
    function CreateToastNotifier(applicationId: HSTRING): IToastNotifier; overload; safecall;
    function get_History: IToastNotificationHistory2; safecall;
    function get_User: IUser; safecall;
    property History: IToastNotificationHistory2 read get_History;
    property User: IUser read get_User;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastNotifier>
  AsyncOperationCompletedHandler_1__IToastNotifier_Delegate_Base = interface(IUnknown)
  ['{FDE26ED7-BC37-5A7C-B3DA-3E41AC97BBA4}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IToastNotifier; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastNotifier>
  AsyncOperationCompletedHandler_1__IToastNotifier = interface(AsyncOperationCompletedHandler_1__IToastNotifier_Delegate_Base)
  ['{CB57B7F4-9EAD-55DD-A970-3734A2B0422F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastNotifier>
  IAsyncOperation_1__IToastNotifier_Base = interface(IInspectable)
  ['{2DDDC10E-38E6-5655-ADF3-820E8FB14DCC}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IToastNotifier); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IToastNotifier; safecall;
    function GetResults: IToastNotifier; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IToastNotifier read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastNotifier>
  IAsyncOperation_1__IToastNotifier = interface(IAsyncOperation_1__IToastNotifier_Base)
  ['{1A0505B2-4770-56F9-8A2D-4BA4CA07FF35}']
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.IToastNotificationHistory2>
  AsyncOperationCompletedHandler_1__IToastNotificationHistory2 = interface(IUnknown)
  ['{788D1165-6B5A-5874-87BC-EC72DC7F4FD0}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IToastNotificationHistory2; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.IToastNotificationHistory2>
  IAsyncOperation_1__IToastNotificationHistory2 = interface(IInspectable)
  ['{22B9CA34-FB6B-541C-9768-FBDD021F06C1}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IToastNotificationHistory2); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IToastNotificationHistory2; safecall;
    function GetResults: IToastNotificationHistory2; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IToastNotificationHistory2 read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotificationManagerStatics
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastNotificationManager)]
  IToastNotificationManagerStatics = interface(IInspectable)
  ['{50AC103F-D235-4598-BBEF-98FE4D1A3AD4}']
    function CreateToastNotifier: IToastNotifier; overload; safecall;
    function CreateToastNotifier(applicationId: HSTRING): IToastNotifier; overload; safecall;
    function GetTemplateContent(&type: ToastTemplateType): Xml_Dom_IXmlDocument; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotificationManagerStatics2
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastNotificationManager)]
  IToastNotificationManagerStatics2 = interface(IInspectable)
  ['{7AB93C52-0E48-4750-BA9D-1A4113981847}']
    function get_History: IToastNotificationHistory2; safecall;
    property History: IToastNotificationHistory2 read get_History;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotificationManagerStatics4
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastNotificationManager)]
  IToastNotificationManagerStatics4 = interface(IInspectable)
  ['{8F993FD3-E516-45FB-8130-398E93FA52C3}']
    function GetForUser(user: IUser): IToastNotificationManagerForUser; safecall;
    procedure ConfigureNotificationMirroring(value: NotificationMirroring); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Notifications.IToastNotificationManagerStatics5
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastNotificationManager)]
  IToastNotificationManagerStatics5 = interface(IInspectable)
  ['{D6F5F569-D40D-407C-8989-88CAB42CFD14}']
    function GetDefault: IToastNotificationManagerForUser; safecall;
  end;

  // Windows.UI.Notifications.IToastNotifier2
  IToastNotifier2 = interface(IInspectable)
  ['{354389C6-7C01-4BD5-9C20-604340CD2B74}']
    function Update(data: INotificationData; tag: HSTRING; group: HSTRING): NotificationUpdateResult; overload; safecall;
    function Update(data: INotificationData; tag: HSTRING): NotificationUpdateResult; overload; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.Management.UserNotificationListenerAccessStatus>
  AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus_Delegate_Base = interface(IUnknown)
  ['{F09E843A-13CB-559B-A9FC-015722C2CD57}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Management_UserNotificationListenerAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Notifications.Management.UserNotificationListenerAccessStatus>
  AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus = interface(AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.Management.UserNotificationListenerAccessStatus>
  IAsyncOperation_1__Management_UserNotificationListenerAccessStatus_Base = interface(IInspectable)
  ['{0FBAD8C7-086F-5BF9-81E2-8D79E7184803}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus; safecall;
    function GetResults: Management_UserNotificationListenerAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Management_UserNotificationListenerAccessStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Notifications.Management.UserNotificationListenerAccessStatus>
  IAsyncOperation_1__Management_UserNotificationListenerAccessStatus = interface(IAsyncOperation_1__Management_UserNotificationListenerAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Windows.UI.Notifications.ToastCollection
  // DualAPI
  // Implements: Windows.UI.Notifications.IToastCollection
  // Factory: "Windows.UI.Notifications.IToastCollectionFactory"
  TToastCollection = class(TWinRTGenericImportF<IToastCollectionFactory>)
  public
    // -> IToastCollectionFactory
    class function CreateInstance(collectionId: HSTRING; displayName: HSTRING; launchArgs: HSTRING; iconUri: IUriRuntimeClass): IToastCollection; static; inline;
  end;

  // Windows.UI.Notifications.ToastNotification
  // DualAPI
  // Implements: Windows.UI.Notifications.IToastNotification
  // Implements: Windows.UI.Notifications.IToastNotification2
  // Implements: Windows.UI.Notifications.IToastNotification3
  // Implements: Windows.UI.Notifications.IToastNotification4
  // Implements: Windows.UI.Notifications.IToastNotification6
  // Factory: "Windows.UI.Notifications.IToastNotificationFactory"
  TToastNotification = class(TWinRTGenericImportF<IToastNotificationFactory>)
  public
    // -> IToastNotificationFactory
    class function CreateToastNotification(content: Xml_Dom_IXmlDocument): IToastNotification; static; inline;
  end;

  // Windows.UI.Notifications.ToastNotificationManager
  // DualAPI
  // Statics: "Windows.UI.Notifications.IToastNotificationManagerStatics"
  // Statics: "Windows.UI.Notifications.IToastNotificationManagerStatics2"
  // Statics: "Windows.UI.Notifications.IToastNotificationManagerStatics4"
  // Statics: "Windows.UI.Notifications.IToastNotificationManagerStatics5"
  TToastNotificationManager = class(TWinRTGenericImportS4<IToastNotificationManagerStatics, IToastNotificationManagerStatics2, IToastNotificationManagerStatics4, IToastNotificationManagerStatics5>)
  public
    // -> IToastNotificationManagerStatics
    class function CreateToastNotifier: IToastNotifier; overload; static; inline;
    class function CreateToastNotifier(applicationId: HSTRING): IToastNotifier; overload; static; inline;
    class function GetTemplateContent(&type: ToastTemplateType): Xml_Dom_IXmlDocument; static; inline;

    // -> IToastNotificationManagerStatics2
    class function get_History: IToastNotificationHistory2; static; inline;
    class property History: IToastNotificationHistory2 read get_History;

    // -> IToastNotificationManagerStatics4
    class function GetForUser(user: IUser): IToastNotificationManagerForUser; static; inline;
    class procedure ConfigureNotificationMirroring(value: NotificationMirroring); static; inline;

    // -> IToastNotificationManagerStatics5
    class function GetDefault: IToastNotificationManagerForUser; static; inline;
  end;

implementation

{ TToastCollection }
// Factories for : "ToastCollection"
// Factory: "Windows.UI.Notifications.IToastCollectionFactory"
// -> IToastCollectionFactory

class function TToastCollection.CreateInstance(collectionId: HSTRING; displayName: HSTRING; launchArgs: HSTRING; iconUri: IUriRuntimeClass): IToastCollection;
begin
  Result := Factory.CreateInstance(collectionId, displayName, launchArgs, iconUri);
end;


{ TToastNotification }
// Factories for : "ToastNotification"
// Factory: "Windows.UI.Notifications.IToastNotificationFactory"
// -> IToastNotificationFactory

class function TToastNotification.CreateToastNotification(content: Xml_Dom_IXmlDocument): IToastNotification;
begin
  Result := Factory.CreateToastNotification(content);
end;


{ TToastNotificationManager }

class function TToastNotificationManager.CreateToastNotifier: IToastNotifier;
begin
  Result := Statics.CreateToastNotifier;
end;

class function TToastNotificationManager.CreateToastNotifier(applicationId: HSTRING): IToastNotifier;
begin
  Result := Statics.CreateToastNotifier(applicationId);
end;

class function TToastNotificationManager.GetTemplateContent(&type: ToastTemplateType): Xml_Dom_IXmlDocument;
begin
  Result := Statics.GetTemplateContent(&type);
end;


class function TToastNotificationManager.get_History: IToastNotificationHistory2;
begin
  Result := Statics2.get_History;
end;


class function TToastNotificationManager.GetForUser(user: IUser): IToastNotificationManagerForUser;
begin
  Result := Statics3.GetForUser(user);
end;

class procedure TToastNotificationManager.ConfigureNotificationMirroring(value: NotificationMirroring);
begin
  Statics3.ConfigureNotificationMirroring(value);
end;


class function TToastNotificationManager.GetDefault: IToastNotificationManagerForUser;
begin
  Result := Statics4.GetDefault;
end;


end.
