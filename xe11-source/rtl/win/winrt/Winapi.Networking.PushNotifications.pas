{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Networking.PushNotifications;

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
  Winapi.UI.Notifications, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.Networking.PushNotifications.IRawNotification
  IRawNotification = interface;
  PIRawNotification = ^IRawNotification;

  // Windows.Networking.PushNotifications.IPushNotificationReceivedEventArgs
  IPushNotificationReceivedEventArgs = interface;
  PIPushNotificationReceivedEventArgs = ^IPushNotificationReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.PushNotifications.IPushNotificationChannel,Windows.Networking.PushNotifications.IPushNotificationReceivedEventArgs>
  TypedEventHandler_2__IPushNotificationChannel__IPushNotificationReceivedEventArgs = interface;
  PTypedEventHandler_2__IPushNotificationChannel__IPushNotificationReceivedEventArgs = ^TypedEventHandler_2__IPushNotificationChannel__IPushNotificationReceivedEventArgs;

  // Windows.Networking.PushNotifications.IPushNotificationChannel
  IPushNotificationChannel = interface;
  PIPushNotificationChannel = ^IPushNotificationChannel;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.PushNotifications.IPushNotificationChannel>
  AsyncOperationCompletedHandler_1__IPushNotificationChannel = interface;
  PAsyncOperationCompletedHandler_1__IPushNotificationChannel = ^AsyncOperationCompletedHandler_1__IPushNotificationChannel;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.PushNotifications.IPushNotificationChannel>
  IAsyncOperation_1__IPushNotificationChannel = interface;
  PIAsyncOperation_1__IPushNotificationChannel = ^IAsyncOperation_1__IPushNotificationChannel;

  // Windows.Networking.PushNotifications.IPushNotificationChannelManagerForUser
  IPushNotificationChannelManagerForUser = interface;
  PIPushNotificationChannelManagerForUser = ^IPushNotificationChannelManagerForUser;

  // Windows.Networking.PushNotifications.IPushNotificationChannelManagerForUser2
  IPushNotificationChannelManagerForUser2 = interface;
  PIPushNotificationChannelManagerForUser2 = ^IPushNotificationChannelManagerForUser2;

  // Windows.Networking.PushNotifications.IPushNotificationChannelsRevokedEventArgs
  IPushNotificationChannelsRevokedEventArgs = interface;
  PIPushNotificationChannelsRevokedEventArgs = ^IPushNotificationChannelsRevokedEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.Networking.PushNotifications.IPushNotificationChannelsRevokedEventArgs>
  EventHandler_1__IPushNotificationChannelsRevokedEventArgs = interface;
  PEventHandler_1__IPushNotificationChannelsRevokedEventArgs = ^EventHandler_1__IPushNotificationChannelsRevokedEventArgs;

  // Windows.Networking.PushNotifications.IRawNotification2
  IRawNotification2 = interface;
  PIRawNotification2 = ^IRawNotification2;

  // Windows.Networking.PushNotifications.IRawNotification3
  IRawNotification3 = interface;
  PIRawNotification3 = ^IRawNotification3;

  // Windows.Networking.PushNotifications Enums

  // Windows.Networking.PushNotifications.PushNotificationType
  PushNotificationType = (
    Toast = 0,
    Tile = 1,
    Badge = 2,
    Raw = 3,
    TileFlyout = 4
  );
  PPushNotificationType = ^PushNotificationType;

  // Windows.Networking.PushNotifications Interfaces

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IRawNotification
  IRawNotification = interface(IInspectable)
  ['{1A227281-3B79-42AC-9963-22AB00D4F0B7}']
    function get_Content: HSTRING; safecall;
    property Content: HSTRING read get_Content;
  end;

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IPushNotificationReceivedEventArgs
  IPushNotificationReceivedEventArgs = interface(IInspectable)
  ['{D1065E0C-36CD-484C-B935-0A99B753CF00}']
    procedure put_Cancel(value: Boolean); safecall;
    function get_Cancel: Boolean; safecall;
    function get_NotificationType: PushNotificationType; safecall;
    function get_ToastNotification: IToastNotification; safecall;
    function get_TileNotification: ITileNotification; safecall;
    function get_BadgeNotification: IBadgeNotification; safecall;
    function get_RawNotification: IRawNotification; safecall;
    property BadgeNotification: IBadgeNotification read get_BadgeNotification;
    property Cancel: Boolean read get_Cancel write put_Cancel;
    property NotificationType: PushNotificationType read get_NotificationType;
    property RawNotification: IRawNotification read get_RawNotification;
    property TileNotification: ITileNotification read get_TileNotification;
    property ToastNotification: IToastNotification read get_ToastNotification;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.PushNotifications.IPushNotificationChannel,Windows.Networking.PushNotifications.IPushNotificationReceivedEventArgs>
  TypedEventHandler_2__IPushNotificationChannel__IPushNotificationReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{55FA217D-1FC3-5863-B980-7094D4379694}']
    procedure Invoke(sender: IPushNotificationChannel; args: IPushNotificationReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.PushNotifications.IPushNotificationChannel,Windows.Networking.PushNotifications.IPushNotificationReceivedEventArgs>
  TypedEventHandler_2__IPushNotificationChannel__IPushNotificationReceivedEventArgs = interface(TypedEventHandler_2__IPushNotificationChannel__IPushNotificationReceivedEventArgs_Delegate_Base)
  ['{FB99BACF-30EA-5C0D-A536-AAA7B7970CA5}']
  end;

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IPushNotificationChannel
  IPushNotificationChannel = interface(IInspectable)
  ['{2B28102E-EF0B-4F39-9B8A-A3C194DE7081}']
    function get_Uri: HSTRING; safecall;
    function get_ExpirationTime: DateTime; safecall;
    procedure Close; safecall;
    function add_PushNotificationReceived(handler: TypedEventHandler_2__IPushNotificationChannel__IPushNotificationReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_PushNotificationReceived(token: EventRegistrationToken); safecall;
    property ExpirationTime: DateTime read get_ExpirationTime;
    property Uri: HSTRING read get_Uri;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.PushNotifications.IPushNotificationChannel>
  AsyncOperationCompletedHandler_1__IPushNotificationChannel_Delegate_Base = interface(IUnknown)
  ['{CF7C902F-0F0D-5B22-90B1-85141B5816CD}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IPushNotificationChannel; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.PushNotifications.IPushNotificationChannel>
  AsyncOperationCompletedHandler_1__IPushNotificationChannel = interface(AsyncOperationCompletedHandler_1__IPushNotificationChannel_Delegate_Base)
  ['{8CA507D2-F89C-5D2B-B320-327877DC7DF7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.PushNotifications.IPushNotificationChannel>
  IAsyncOperation_1__IPushNotificationChannel_Base = interface(IInspectable)
  ['{70945A09-331A-5E40-B854-66B7A3233BAB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IPushNotificationChannel); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IPushNotificationChannel; safecall;
    function GetResults: IPushNotificationChannel; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IPushNotificationChannel read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.PushNotifications.IPushNotificationChannel>
  IAsyncOperation_1__IPushNotificationChannel = interface(IAsyncOperation_1__IPushNotificationChannel_Base)
  ['{9F5E2218-4313-54F9-819F-88AFFCAE1958}']
  end;

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IPushNotificationChannelManagerForUser
  IPushNotificationChannelManagerForUser = interface(IInspectable)
  ['{A4C45704-1182-42C7-8890-F563C4890DC4}']
    function CreatePushNotificationChannelForApplicationAsync: IAsyncOperation_1__IPushNotificationChannel; overload; safecall;
    function CreatePushNotificationChannelForApplicationAsync(applicationId: HSTRING): IAsyncOperation_1__IPushNotificationChannel; overload; safecall;
    function CreatePushNotificationChannelForSecondaryTileAsync(tileId: HSTRING): IAsyncOperation_1__IPushNotificationChannel; safecall;
    function get_User: IUser; safecall;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IPushNotificationChannelManagerForUser2
  IPushNotificationChannelManagerForUser2 = interface(IInspectable)
  ['{C38B066A-7CC1-4DAC-87FD-BE6E920414A4}']
    function CreateRawPushNotificationChannelWithAlternateKeyForApplicationAsync(appServerKey: IBuffer; channelId: HSTRING): IAsyncOperation_1__IPushNotificationChannel; overload; safecall;
    function CreateRawPushNotificationChannelWithAlternateKeyForApplicationAsync(appServerKey: IBuffer; channelId: HSTRING; appId: HSTRING): IAsyncOperation_1__IPushNotificationChannel; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IPushNotificationChannelsRevokedEventArgs
  IPushNotificationChannelsRevokedEventArgs = interface(IInspectable)
  ['{20E1A24C-1A34-5BEB-AAE2-40C232C8C140}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Networking.PushNotifications.IPushNotificationChannelsRevokedEventArgs>
  EventHandler_1__IPushNotificationChannelsRevokedEventArgs = interface(IUnknown)
  ['{0E6A9A63-3055-5937-BF72-F88EAAD614B7}']
    procedure Invoke(sender: IInspectable; args: IPushNotificationChannelsRevokedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IRawNotification2
  IRawNotification2 = interface(IInspectable)
  ['{E6D0CF19-0C6F-4CDD-9424-EEC5BE014D26}']
    function get_Headers: IMapView_2__HSTRING__HSTRING; safecall;
    function get_ChannelId: HSTRING; safecall;
    property ChannelId: HSTRING read get_ChannelId;
    property Headers: IMapView_2__HSTRING__HSTRING read get_Headers;
  end;

  // UsedAPI Interface
  // Windows.Networking.PushNotifications.IRawNotification3
  IRawNotification3 = interface(IInspectable)
  ['{62737DDE-8A73-424C-AB44-5635F40A96E5}']
    function get_ContentBytes: IBuffer; safecall;
    property ContentBytes: IBuffer read get_ContentBytes;
  end;

implementation

end.
