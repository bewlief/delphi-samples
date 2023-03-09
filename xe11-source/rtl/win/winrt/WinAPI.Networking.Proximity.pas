{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Networking.Proximity;

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

  // Windows.Networking.Proximity.IProximityMessage
  IProximityMessage = interface;
  PIProximityMessage = ^IProximityMessage;

  // Windows.Networking.Proximity.MessageReceivedHandler
  MessageReceivedHandler = interface;
  PMessageReceivedHandler = ^MessageReceivedHandler;

  // Windows.Networking.Proximity.MessageTransmittedHandler
  MessageTransmittedHandler = interface;
  PMessageTransmittedHandler = ^MessageTransmittedHandler;

  // Windows.Networking.Proximity.DeviceDepartedEventHandler
  DeviceDepartedEventHandler = interface;
  PDeviceDepartedEventHandler = ^DeviceDepartedEventHandler;

  // Windows.Networking.Proximity.IProximityDevice
  IProximityDevice = interface;
  PIProximityDevice = ^IProximityDevice;

  // Windows.Networking.Proximity.DeviceArrivedEventHandler
  DeviceArrivedEventHandler = interface;
  PDeviceArrivedEventHandler = ^DeviceArrivedEventHandler;

  // Windows.Networking.Proximity.IProximityDeviceStatics
  IProximityDeviceStatics = interface;
  PIProximityDeviceStatics = ^IProximityDeviceStatics;

  // Windows.Networking.Proximity Enums

  // Windows.Networking.Proximity.PeerDiscoveryTypes
  PeerDiscoveryTypes = (
    None = 0,
    Browse = 1,
    Triggered = 2
  );
  PPeerDiscoveryTypes = ^PeerDiscoveryTypes;

  // Windows.Networking.Proximity.PeerRole
  PeerRole = (
    Peer = 0,
    Host = 1,
    Client = 2
  );
  PPeerRole = ^PeerRole;

  // Windows.Networking.Proximity.PeerWatcherStatus
  PeerWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PPeerWatcherStatus = ^PeerWatcherStatus;

  // Windows.Networking.Proximity.TriggeredConnectState
  TriggeredConnectState = (
    PeerFound = 0,
    Listening = 1,
    Connecting = 2,
    Completed = 3,
    Canceled = 4,
    Failed = 5
  );
  PTriggeredConnectState = ^TriggeredConnectState;

  // Windows.Networking.Proximity Interfaces

  // UsedAPI Interface
  // Windows.Networking.Proximity.IProximityMessage
  IProximityMessage = interface(IInspectable)
  ['{EFAB0782-F6E1-4675-A045-D8E320C24808}']
    function get_MessageType: HSTRING; safecall;
    function get_SubscriptionId: Int64; safecall;
    function get_Data: IBuffer; safecall;
    function get_DataAsString: HSTRING; safecall;
    property Data: IBuffer read get_Data;
    property DataAsString: HSTRING read get_DataAsString;
    property MessageType: HSTRING read get_MessageType;
    property SubscriptionId: Int64 read get_SubscriptionId;
  end;

  // UsedAPI Interface
  // Windows.Networking.Proximity.MessageReceivedHandler
  MessageReceivedHandler = interface(IUnknown)
  ['{EFAB0782-F6E2-4675-A045-D8E320C24808}']
    procedure Invoke(sender: IProximityDevice; &message: IProximityMessage); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Proximity.MessageTransmittedHandler
  MessageTransmittedHandler = interface(IUnknown)
  ['{EFAA0B4A-F6E2-4D7D-856C-78FC8EFC021E}']
    procedure Invoke(sender: IProximityDevice; messageId: Int64); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Proximity.DeviceDepartedEventHandler
  DeviceDepartedEventHandler = interface(IUnknown)
  ['{EFA9DA69-F6E2-49C9-A49E-8E0FC58FB911}']
    procedure Invoke(sender: IProximityDevice); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Proximity.IProximityDevice
  [WinRTClassNameAttribute(SWindows_Networking_Proximity_ProximityDevice)]
  IProximityDevice = interface(IInspectable)
  ['{EFA8A552-F6E1-4329-A0FC-AB6B0FD28262}']
    function SubscribeForMessage(messageType: HSTRING; messageReceivedHandler: MessageReceivedHandler): Int64; safecall;
    function PublishMessage(messageType: HSTRING; &message: HSTRING): Int64; overload; safecall;
    function PublishMessage(messageType: HSTRING; &message: HSTRING; messageTransmittedHandler: MessageTransmittedHandler): Int64; overload; safecall;
    function PublishBinaryMessage(messageType: HSTRING; &message: IBuffer): Int64; overload; safecall;
    function PublishBinaryMessage(messageType: HSTRING; &message: IBuffer; messageTransmittedHandler: MessageTransmittedHandler): Int64; overload; safecall;
    function PublishUriMessage(&message: IUriRuntimeClass): Int64; overload; safecall;
    function PublishUriMessage(&message: IUriRuntimeClass; messageTransmittedHandler: MessageTransmittedHandler): Int64; overload; safecall;
    procedure StopSubscribingForMessage(subscriptionId: Int64); safecall;
    procedure StopPublishingMessage(messageId: Int64); safecall;
    function add_DeviceArrived(arrivedHandler: DeviceArrivedEventHandler): EventRegistrationToken; safecall;
    procedure remove_DeviceArrived(cookie: EventRegistrationToken); safecall;
    function add_DeviceDeparted(departedHandler: DeviceDepartedEventHandler): EventRegistrationToken; safecall;
    procedure remove_DeviceDeparted(cookie: EventRegistrationToken); safecall;
    function get_MaxMessageBytes: Cardinal; safecall;
    function get_BitsPerSecond: UInt64; safecall;
    function get_DeviceId: HSTRING; safecall;
    property BitsPerSecond: UInt64 read get_BitsPerSecond;
    property DeviceId: HSTRING read get_DeviceId;
    property MaxMessageBytes: Cardinal read get_MaxMessageBytes;
  end;

  // UsedAPI Interface
  // Windows.Networking.Proximity.DeviceArrivedEventHandler
  DeviceArrivedEventHandler = interface(IUnknown)
  ['{EFA9DA69-F6E1-49C9-A49E-8E0FC58FB911}']
    procedure Invoke(sender: IProximityDevice); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Proximity.IProximityDeviceStatics
  [WinRTClassNameAttribute(SWindows_Networking_Proximity_ProximityDevice)]
  IProximityDeviceStatics = interface(IInspectable)
  ['{914BA01D-F6E1-47C4-A14C-148A1903D0C6}']
    function GetDeviceSelector: HSTRING; safecall;
    function GetDefault: IProximityDevice; safecall;
    function FromId(deviceId: HSTRING): IProximityDevice; safecall;
  end;

  // Windows.Networking.Proximity.ProximityDevice
  // DualAPI
  // Implements: Windows.Networking.Proximity.IProximityDevice
  // Statics: "Windows.Networking.Proximity.IProximityDeviceStatics"
  TProximityDevice = class(TWinRTGenericImportS<IProximityDeviceStatics>)
  public
    // -> IProximityDeviceStatics
    class function GetDeviceSelector: HSTRING; static; inline;
    class function GetDefault: IProximityDevice; static; inline;
    class function FromId(deviceId: HSTRING): IProximityDevice; static; inline;
  end;

implementation

{ TProximityDevice }

class function TProximityDevice.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;

class function TProximityDevice.GetDefault: IProximityDevice;
begin
  Result := Statics.GetDefault;
end;

class function TProximityDevice.FromId(deviceId: HSTRING): IProximityDevice;
begin
  Result := Statics.FromId(deviceId);
end;


end.
