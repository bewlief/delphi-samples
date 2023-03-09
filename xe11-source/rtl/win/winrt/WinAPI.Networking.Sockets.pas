{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Networking.Sockets;

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
  Winapi.Networking, 
  Winapi.Storage.Streams, 
  Winapi.Security.Credentials, 
  Winapi.Security.Cryptography, 
  Winapi.ApplicationModel.Background, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  SocketProtectionLevel = Winapi.CommonTypes.SocketProtectionLevel;
  PSocketProtectionLevel = Winapi.CommonTypes.PSocketProtectionLevel;
  SocketSslErrorSeverity = Winapi.CommonTypes.SocketSslErrorSeverity;
  PSocketSslErrorSeverity = Winapi.CommonTypes.PSocketSslErrorSeverity;

  // Forward declarations for interfaces

  // Windows.Networking.Sockets.IStreamSocketControl
  IStreamSocketControl = interface;
  PIStreamSocketControl = ^IStreamSocketControl;

  // Windows.Networking.Sockets.IStreamSocketInformation
  IStreamSocketInformation = interface;
  PIStreamSocketInformation = ^IStreamSocketInformation;

  // Windows.Networking.Sockets.IStreamSocket
  IStreamSocket = interface;
  PIStreamSocket = ^IStreamSocket;

  // Windows.Networking.Sockets.IStreamSocketListenerControl
  IStreamSocketListenerControl = interface;
  PIStreamSocketListenerControl = ^IStreamSocketListenerControl;

  // Windows.Networking.Sockets.IStreamSocketListenerInformation
  IStreamSocketListenerInformation = interface;
  PIStreamSocketListenerInformation = ^IStreamSocketListenerInformation;

  // Windows.Networking.Sockets.IStreamSocketListenerConnectionReceivedEventArgs
  IStreamSocketListenerConnectionReceivedEventArgs = interface;
  PIStreamSocketListenerConnectionReceivedEventArgs = ^IStreamSocketListenerConnectionReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IStreamSocketListener,Windows.Networking.Sockets.IStreamSocketListenerConnectionReceivedEventArgs>
  TypedEventHandler_2__IStreamSocketListener__IStreamSocketListenerConnectionReceivedEventArgs = interface;
  PTypedEventHandler_2__IStreamSocketListener__IStreamSocketListenerConnectionReceivedEventArgs = ^TypedEventHandler_2__IStreamSocketListener__IStreamSocketListenerConnectionReceivedEventArgs;

  // Windows.Networking.Sockets.IStreamSocketListener
  IStreamSocketListener = interface;
  PIStreamSocketListener = ^IStreamSocketListener;

  // Windows.Networking.Sockets.IDatagramSocketControl
  IDatagramSocketControl = interface;
  PIDatagramSocketControl = ^IDatagramSocketControl;

  // Windows.Networking.Sockets.IDatagramSocketInformation
  IDatagramSocketInformation = interface;
  PIDatagramSocketInformation = ^IDatagramSocketInformation;

  // Windows.Networking.Sockets.IDatagramSocketMessageReceivedEventArgs
  IDatagramSocketMessageReceivedEventArgs = interface;
  PIDatagramSocketMessageReceivedEventArgs = ^IDatagramSocketMessageReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IDatagramSocket,Windows.Networking.Sockets.IDatagramSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IDatagramSocket__IDatagramSocketMessageReceivedEventArgs = interface;
  PTypedEventHandler_2__IDatagramSocket__IDatagramSocketMessageReceivedEventArgs = ^TypedEventHandler_2__IDatagramSocket__IDatagramSocketMessageReceivedEventArgs;

  // Windows.Networking.Sockets.IDatagramSocket
  IDatagramSocket = interface;
  PIDatagramSocket = ^IDatagramSocket;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Sockets.IStreamSocket>
  AsyncOperationCompletedHandler_1__IStreamSocket = interface;
  PAsyncOperationCompletedHandler_1__IStreamSocket = ^AsyncOperationCompletedHandler_1__IStreamSocket;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Sockets.IStreamSocket>
  IAsyncOperation_1__IStreamSocket = interface;
  PIAsyncOperation_1__IStreamSocket = ^IAsyncOperation_1__IStreamSocket;

  // Windows.Networking.Sockets.IControlChannelTriggerResetEventDetails
  IControlChannelTriggerResetEventDetails = interface;
  PIControlChannelTriggerResetEventDetails = ^IControlChannelTriggerResetEventDetails;

  // Windows.Networking.Sockets.IDatagramSocket2
  IDatagramSocket2 = interface;
  PIDatagramSocket2 = ^IDatagramSocket2;

  // Windows.Networking.Sockets.ISocketActivityContext
  ISocketActivityContext = interface;
  PISocketActivityContext = ^ISocketActivityContext;

  // Windows.Networking.Sockets.IDatagramSocket3
  IDatagramSocket3 = interface;
  PIDatagramSocket3 = ^IDatagramSocket3;

  // Windows.Networking.Sockets.IDatagramSocketControl2
  IDatagramSocketControl2 = interface;
  PIDatagramSocketControl2 = ^IDatagramSocketControl2;

  // Windows.Networking.Sockets.IDatagramSocketControl3
  IDatagramSocketControl3 = interface;
  PIDatagramSocketControl3 = ^IDatagramSocketControl3;

  // Windows.Networking.Sockets.IDatagramSocketStatics
  IDatagramSocketStatics = interface;
  PIDatagramSocketStatics = ^IDatagramSocketStatics;

  // Windows.Networking.Sockets.IWebSocketClosedEventArgs
  IWebSocketClosedEventArgs = interface;
  PIWebSocketClosedEventArgs = ^IWebSocketClosedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IWebSocket,Windows.Networking.Sockets.IWebSocketClosedEventArgs>
  TypedEventHandler_2__IWebSocket__IWebSocketClosedEventArgs = interface;
  PTypedEventHandler_2__IWebSocket__IWebSocketClosedEventArgs = ^TypedEventHandler_2__IWebSocket__IWebSocketClosedEventArgs;

  // Windows.Networking.Sockets.IWebSocket
  IWebSocket = interface;
  PIWebSocket = ^IWebSocket;

  // Windows.Networking.Sockets.IWebSocketControl
  IWebSocketControl = interface;
  PIWebSocketControl = ^IWebSocketControl;

  // Windows.Networking.Sockets.IMessageWebSocketControl
  IMessageWebSocketControl = interface;
  PIMessageWebSocketControl = ^IMessageWebSocketControl;

  // Windows.Networking.Sockets.IWebSocketInformation
  IWebSocketInformation = interface;
  PIWebSocketInformation = ^IWebSocketInformation;

  // Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs
  IMessageWebSocketMessageReceivedEventArgs = interface;
  PIMessageWebSocketMessageReceivedEventArgs = ^IMessageWebSocketMessageReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IMessageWebSocket,Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs = interface;
  PTypedEventHandler_2__IMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs = ^TypedEventHandler_2__IMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs;

  // Windows.Networking.Sockets.IMessageWebSocket
  IMessageWebSocket = interface;
  PIMessageWebSocket = ^IMessageWebSocket;

  // Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs
  IWebSocketServerCustomValidationRequestedEventArgs = interface;
  PIWebSocketServerCustomValidationRequestedEventArgs = ^IWebSocketServerCustomValidationRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IMessageWebSocket,Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs>
  TypedEventHandler_2__IMessageWebSocket__IWebSocketServerCustomValidationRequestedEventArgs = interface;
  PTypedEventHandler_2__IMessageWebSocket__IWebSocketServerCustomValidationRequestedEventArgs = ^TypedEventHandler_2__IMessageWebSocket__IWebSocketServerCustomValidationRequestedEventArgs;

  // Windows.Networking.Sockets.IMessageWebSocket2
  IMessageWebSocket2 = interface;
  PIMessageWebSocket2 = ^IMessageWebSocket2;

  // Windows.Networking.Sockets.IMessageWebSocket3
  IMessageWebSocket3 = interface;
  PIMessageWebSocket3 = ^IMessageWebSocket3;

  // Windows.Networking.Sockets.IMessageWebSocketControl2
  IMessageWebSocketControl2 = interface;
  PIMessageWebSocketControl2 = ^IMessageWebSocketControl2;

  // Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs2
  IMessageWebSocketMessageReceivedEventArgs2 = interface;
  PIMessageWebSocketMessageReceivedEventArgs2 = ^IMessageWebSocketMessageReceivedEventArgs2;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IServerMessageWebSocket,Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IServerMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs = interface;
  PTypedEventHandler_2__IServerMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs = ^TypedEventHandler_2__IServerMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs;

  // Windows.Networking.Sockets.IServerMessageWebSocketControl
  IServerMessageWebSocketControl = interface;
  PIServerMessageWebSocketControl = ^IServerMessageWebSocketControl;

  // Windows.Networking.Sockets.IServerMessageWebSocketInformation
  IServerMessageWebSocketInformation = interface;
  PIServerMessageWebSocketInformation = ^IServerMessageWebSocketInformation;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IServerMessageWebSocket,Windows.Networking.Sockets.IWebSocketClosedEventArgs>
  TypedEventHandler_2__IServerMessageWebSocket__IWebSocketClosedEventArgs = interface;
  PTypedEventHandler_2__IServerMessageWebSocket__IWebSocketClosedEventArgs = ^TypedEventHandler_2__IServerMessageWebSocket__IWebSocketClosedEventArgs;

  // Windows.Networking.Sockets.IServerMessageWebSocket
  IServerMessageWebSocket = interface;
  PIServerMessageWebSocket = ^IServerMessageWebSocket;

  // Windows.Networking.Sockets.IServerStreamWebSocketInformation
  IServerStreamWebSocketInformation = interface;
  PIServerStreamWebSocketInformation = ^IServerStreamWebSocketInformation;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IServerStreamWebSocket,Windows.Networking.Sockets.IWebSocketClosedEventArgs>
  TypedEventHandler_2__IServerStreamWebSocket__IWebSocketClosedEventArgs = interface;
  PTypedEventHandler_2__IServerStreamWebSocket__IWebSocketClosedEventArgs = ^TypedEventHandler_2__IServerStreamWebSocket__IWebSocketClosedEventArgs;

  // Windows.Networking.Sockets.IServerStreamWebSocket
  IServerStreamWebSocket = interface;
  PIServerStreamWebSocket = ^IServerStreamWebSocket;

  // Windows.Networking.Sockets.ISocketErrorStatics
  ISocketErrorStatics = interface;
  PISocketErrorStatics = ^ISocketErrorStatics;

  // Windows.Networking.Sockets.IStreamSocket2
  IStreamSocket2 = interface;
  PIStreamSocket2 = ^IStreamSocket2;

  // Windows.Networking.Sockets.IStreamSocket3
  IStreamSocket3 = interface;
  PIStreamSocket3 = ^IStreamSocket3;

  // Windows.Networking.Sockets.IStreamSocketControl2
  IStreamSocketControl2 = interface;
  PIStreamSocketControl2 = ^IStreamSocketControl2;

  // Windows.Networking.Sockets.IStreamSocketControl3
  IStreamSocketControl3 = interface;
  PIStreamSocketControl3 = ^IStreamSocketControl3;

  // Windows.Networking.Sockets.IStreamSocketControl4
  IStreamSocketControl4 = interface;
  PIStreamSocketControl4 = ^IStreamSocketControl4;

  // Windows.Networking.Sockets.IStreamSocketInformation2
  IStreamSocketInformation2 = interface;
  PIStreamSocketInformation2 = ^IStreamSocketInformation2;

  // Windows.Networking.Sockets.IStreamSocketListener2
  IStreamSocketListener2 = interface;
  PIStreamSocketListener2 = ^IStreamSocketListener2;

  // Windows.Networking.Sockets.IStreamSocketListener3
  IStreamSocketListener3 = interface;
  PIStreamSocketListener3 = ^IStreamSocketListener3;

  // Windows.Networking.Sockets.IStreamSocketListenerControl2
  IStreamSocketListenerControl2 = interface;
  PIStreamSocketListenerControl2 = ^IStreamSocketListenerControl2;

  // Windows.Networking.Sockets.IStreamSocketStatics
  IStreamSocketStatics = interface;
  PIStreamSocketStatics = ^IStreamSocketStatics;

  // Windows.Networking.Sockets.IStreamWebSocketControl
  IStreamWebSocketControl = interface;
  PIStreamWebSocketControl = ^IStreamWebSocketControl;

  // Windows.Networking.Sockets.IStreamWebSocket
  IStreamWebSocket = interface;
  PIStreamWebSocket = ^IStreamWebSocket;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IStreamWebSocket,Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs>
  TypedEventHandler_2__IStreamWebSocket__IWebSocketServerCustomValidationRequestedEventArgs = interface;
  PTypedEventHandler_2__IStreamWebSocket__IWebSocketServerCustomValidationRequestedEventArgs = ^TypedEventHandler_2__IStreamWebSocket__IWebSocketServerCustomValidationRequestedEventArgs;

  // Windows.Networking.Sockets.IStreamWebSocket2
  IStreamWebSocket2 = interface;
  PIStreamWebSocket2 = ^IStreamWebSocket2;

  // Windows.Networking.Sockets.IStreamWebSocketControl2
  IStreamWebSocketControl2 = interface;
  PIStreamWebSocketControl2 = ^IStreamWebSocketControl2;

  // Windows.Networking.Sockets.IWebSocketControl2
  IWebSocketControl2 = interface;
  PIWebSocketControl2 = ^IWebSocketControl2;

  // Windows.Networking.Sockets.IWebSocketErrorStatics
  IWebSocketErrorStatics = interface;
  PIWebSocketErrorStatics = ^IWebSocketErrorStatics;

  // Windows.Networking.Sockets.IWebSocketInformation2
  IWebSocketInformation2 = interface;
  PIWebSocketInformation2 = ^IWebSocketInformation2;

  // Windows.Networking.Sockets Enums

  // Windows.Networking.Sockets.ControlChannelTriggerResetReason
  ControlChannelTriggerResetReason = (
    FastUserSwitched = 0,
    LowPowerExit = 1,
    QuietHoursExit = 2,
    ApplicationRestart = 3
  );
  PControlChannelTriggerResetReason = ^ControlChannelTriggerResetReason;

  // Windows.Networking.Sockets.ControlChannelTriggerResourceType
  ControlChannelTriggerResourceType = (
    RequestSoftwareSlot = 0,
    RequestHardwareSlot = 1
  );
  PControlChannelTriggerResourceType = ^ControlChannelTriggerResourceType;

  // Windows.Networking.Sockets.ControlChannelTriggerStatus
  ControlChannelTriggerStatus = (
    HardwareSlotRequested = 0,
    SoftwareSlotAllocated = 1,
    HardwareSlotAllocated = 2,
    PolicyError = 3,
    SystemError = 4,
    TransportDisconnected = 5,
    ServiceUnavailable = 6
  );
  PControlChannelTriggerStatus = ^ControlChannelTriggerStatus;

  // Windows.Networking.Sockets.MessageWebSocketReceiveMode
  MessageWebSocketReceiveMode = (
    FullMessage = 0,
    PartialMessage = 1
  );
  PMessageWebSocketReceiveMode = ^MessageWebSocketReceiveMode;

  // Windows.Networking.Sockets.SocketActivityConnectedStandbyAction
  SocketActivityConnectedStandbyAction = (
    DoNotWake = 0,
    Wake = 1
  );
  PSocketActivityConnectedStandbyAction = ^SocketActivityConnectedStandbyAction;

  // Windows.Networking.Sockets.SocketActivityKind
  SocketActivityKind = (
    None = 0,
    StreamSocketListener = 1,
    DatagramSocket = 2,
    StreamSocket = 3
  );
  PSocketActivityKind = ^SocketActivityKind;

  // Windows.Networking.Sockets.SocketActivityTriggerReason
  SocketActivityTriggerReason = (
    None = 0,
    SocketActivity = 1,
    ConnectionAccepted = 2,
    KeepAliveTimerExpired = 3,
    SocketClosed = 4
  );
  PSocketActivityTriggerReason = ^SocketActivityTriggerReason;

  // Windows.Networking.Sockets.SocketErrorStatus
  SocketErrorStatus = (
    Unknown = 0,
    OperationAborted = 1,
    HttpInvalidServerResponse = 2,
    ConnectionTimedOut = 3,
    AddressFamilyNotSupported = 4,
    SocketTypeNotSupported = 5,
    HostNotFound = 6,
    NoDataRecordOfRequestedType = 7,
    NonAuthoritativeHostNotFound = 8,
    ClassTypeNotFound = 9,
    AddressAlreadyInUse = 10,
    CannotAssignRequestedAddress = 11,
    ConnectionRefused = 12,
    NetworkIsUnreachable = 13,
    UnreachableHost = 14,
    NetworkIsDown = 15,
    NetworkDroppedConnectionOnReset = 16,
    SoftwareCausedConnectionAbort = 17,
    ConnectionResetByPeer = 18,
    HostIsDown = 19,
    NoAddressesFound = 20,
    TooManyOpenFiles = 21,
    MessageTooLong = 22,
    CertificateExpired = 23,
    CertificateUntrustedRoot = 24,
    CertificateCommonNameIsIncorrect = 25,
    CertificateWrongUsage = 26,
    CertificateRevoked = 27,
    CertificateNoRevocationCheck = 28,
    CertificateRevocationServerOffline = 29,
    CertificateIsInvalid = 30
  );
  PSocketErrorStatus = ^SocketErrorStatus;

  // Windows.Networking.Sockets.SocketMessageType
  SocketMessageType = (
    Binary = 0,
    Utf8 = 1
  );
  PSocketMessageType = ^SocketMessageType;

  // Windows.Networking.Sockets.SocketQualityOfService
  SocketQualityOfService = (
    Normal = 0,
    LowLatency = 1
  );
  PSocketQualityOfService = ^SocketQualityOfService;

  // Windows.Networking.Sockets Records
  // Windows.Networking.Sockets.BandwidthStatistics
  BandwidthStatistics = record
    OutboundBitsPerSecond: UInt64;
    InboundBitsPerSecond: UInt64;
    OutboundBitsPerSecondInstability: UInt64;
    InboundBitsPerSecondInstability: UInt64;
    OutboundBandwidthPeaked: Boolean;
    InboundBandwidthPeaked: Boolean;
  end;
  PBandwidthStatistics = ^BandwidthStatistics;

  // Windows.Networking.Sockets.ControlChannelTriggerContract
  ControlChannelTriggerContract = record
  end;
  PControlChannelTriggerContract = ^ControlChannelTriggerContract;

  // Windows.Networking.Sockets.RoundTripTimeStatistics
  RoundTripTimeStatistics = record
    Variance: Cardinal;
    Max: Cardinal;
    Min: Cardinal;
    Sum: Cardinal;
  end;
  PRoundTripTimeStatistics = ^RoundTripTimeStatistics;

  // Windows.Networking.Sockets Interfaces

  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketControl
  IStreamSocketControl = interface(IInspectable)
  ['{FE25ADF1-92AB-4AF3-9992-0F4C85E36CC4}']
    function get_NoDelay: Boolean; safecall;
    procedure put_NoDelay(value: Boolean); safecall;
    function get_KeepAlive: Boolean; safecall;
    procedure put_KeepAlive(value: Boolean); safecall;
    function get_OutboundBufferSizeInBytes: Cardinal; safecall;
    procedure put_OutboundBufferSizeInBytes(value: Cardinal); safecall;
    function get_QualityOfService: SocketQualityOfService; safecall;
    procedure put_QualityOfService(value: SocketQualityOfService); safecall;
    function get_OutboundUnicastHopLimit: Byte; safecall;
    procedure put_OutboundUnicastHopLimit(value: Byte); safecall;
    property KeepAlive: Boolean read get_KeepAlive write put_KeepAlive;
    property NoDelay: Boolean read get_NoDelay write put_NoDelay;
    property OutboundBufferSizeInBytes: Cardinal read get_OutboundBufferSizeInBytes write put_OutboundBufferSizeInBytes;
    property OutboundUnicastHopLimit: Byte read get_OutboundUnicastHopLimit write put_OutboundUnicastHopLimit;
    property QualityOfService: SocketQualityOfService read get_QualityOfService write put_QualityOfService;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketInformation
  IStreamSocketInformation = interface(IInspectable)
  ['{3B80AE30-5E68-4205-88F0-DC85D2E25DED}']
    function get_LocalAddress: IHostName; safecall;
    function get_LocalPort: HSTRING; safecall;
    function get_RemoteHostName: IHostName; safecall;
    function get_RemoteAddress: IHostName; safecall;
    function get_RemoteServiceName: HSTRING; safecall;
    function get_RemotePort: HSTRING; safecall;
    function get_RoundTripTimeStatistics: RoundTripTimeStatistics; safecall;
    function get_BandwidthStatistics: BandwidthStatistics; safecall;
    function get_ProtectionLevel: SocketProtectionLevel; safecall;
    function get_SessionKey: IBuffer; safecall;
    property BandwidthStatistics_: BandwidthStatistics read get_BandwidthStatistics;
    property LocalAddress: IHostName read get_LocalAddress;
    property LocalPort: HSTRING read get_LocalPort;
    property ProtectionLevel: SocketProtectionLevel read get_ProtectionLevel;
    property RemoteAddress: IHostName read get_RemoteAddress;
    property RemoteHostName: IHostName read get_RemoteHostName;
    property RemotePort: HSTRING read get_RemotePort;
    property RemoteServiceName: HSTRING read get_RemoteServiceName;
    property RoundTripTimeStatistics_: RoundTripTimeStatistics read get_RoundTripTimeStatistics;
    property SessionKey: IBuffer read get_SessionKey;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocket
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_StreamSocket)]
  IStreamSocket = interface(IInspectable)
  ['{69A22CF3-FC7B-4857-AF38-F6E7DE6A5B49}']
    function get_Control: IStreamSocketControl; safecall;
    function get_Information: IStreamSocketInformation; safecall;
    function get_InputStream: IInputStream; safecall;
    function get_OutputStream: IOutputStream; safecall;
    function ConnectAsync(endpointPair: IEndpointPair): IAsyncAction; overload; safecall;
    function ConnectAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncAction; overload; safecall;
    function ConnectAsync(endpointPair: IEndpointPair; protectionLevel: SocketProtectionLevel): IAsyncAction; overload; safecall;
    function ConnectAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; protectionLevel: SocketProtectionLevel): IAsyncAction; overload; safecall;
    function UpgradeToSslAsync(protectionLevel: SocketProtectionLevel; validationHostName: IHostName): IAsyncAction; safecall;
    property Control: IStreamSocketControl read get_Control;
    property Information: IStreamSocketInformation read get_Information;
    property InputStream: IInputStream read get_InputStream;
    property OutputStream: IOutputStream read get_OutputStream;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketListenerControl
  IStreamSocketListenerControl = interface(IInspectable)
  ['{20D8C576-8D8A-4DBA-9722-A16C4D984980}']
    function get_QualityOfService: SocketQualityOfService; safecall;
    procedure put_QualityOfService(value: SocketQualityOfService); safecall;
    property QualityOfService: SocketQualityOfService read get_QualityOfService write put_QualityOfService;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketListenerInformation
  IStreamSocketListenerInformation = interface(IInspectable)
  ['{E62BA82F-A63A-430B-BF62-29E93E5633B4}']
    function get_LocalPort: HSTRING; safecall;
    property LocalPort: HSTRING read get_LocalPort;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketListenerConnectionReceivedEventArgs
  IStreamSocketListenerConnectionReceivedEventArgs = interface(IInspectable)
  ['{0C472EA9-373F-447B-85B1-DDD4548803BA}']
    function get_Socket: IStreamSocket; safecall;
    property Socket: IStreamSocket read get_Socket;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IStreamSocketListener,Windows.Networking.Sockets.IStreamSocketListenerConnectionReceivedEventArgs>
  TypedEventHandler_2__IStreamSocketListener__IStreamSocketListenerConnectionReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{33D00D41-C94F-5A61-9AB7-280DCEFA0B08}']
    procedure Invoke(sender: IStreamSocketListener; args: IStreamSocketListenerConnectionReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IStreamSocketListener,Windows.Networking.Sockets.IStreamSocketListenerConnectionReceivedEventArgs>
  TypedEventHandler_2__IStreamSocketListener__IStreamSocketListenerConnectionReceivedEventArgs = interface(TypedEventHandler_2__IStreamSocketListener__IStreamSocketListenerConnectionReceivedEventArgs_Delegate_Base)
  ['{53211835-FF9E-59A6-AB04-69DD8BA5D368}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketListener
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_StreamSocketListener)]
  IStreamSocketListener = interface(IInspectable)
  ['{FF513437-DF9F-4DF0-BF82-0EC5D7B35AAE}']
    function get_Control: IStreamSocketListenerControl; safecall;
    function get_Information: IStreamSocketListenerInformation; safecall;
    function BindServiceNameAsync(localServiceName: HSTRING): IAsyncAction; safecall;
    function BindEndpointAsync(localHostName: IHostName; localServiceName: HSTRING): IAsyncAction; safecall;
    function add_ConnectionReceived(eventHandler: TypedEventHandler_2__IStreamSocketListener__IStreamSocketListenerConnectionReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ConnectionReceived(eventCookie: EventRegistrationToken); safecall;
    property Control: IStreamSocketListenerControl read get_Control;
    property Information: IStreamSocketListenerInformation read get_Information;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IDatagramSocketControl
  IDatagramSocketControl = interface(IInspectable)
  ['{52AC3F2E-349A-4135-BB58-B79B2647D390}']
    function get_QualityOfService: SocketQualityOfService; safecall;
    procedure put_QualityOfService(value: SocketQualityOfService); safecall;
    function get_OutboundUnicastHopLimit: Byte; safecall;
    procedure put_OutboundUnicastHopLimit(value: Byte); safecall;
    property OutboundUnicastHopLimit: Byte read get_OutboundUnicastHopLimit write put_OutboundUnicastHopLimit;
    property QualityOfService: SocketQualityOfService read get_QualityOfService write put_QualityOfService;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IDatagramSocketInformation
  IDatagramSocketInformation = interface(IInspectable)
  ['{5F1A569A-55FB-48CD-9706-7A974F7B1585}']
    function get_LocalAddress: IHostName; safecall;
    function get_LocalPort: HSTRING; safecall;
    function get_RemoteAddress: IHostName; safecall;
    function get_RemotePort: HSTRING; safecall;
    property LocalAddress: IHostName read get_LocalAddress;
    property LocalPort: HSTRING read get_LocalPort;
    property RemoteAddress: IHostName read get_RemoteAddress;
    property RemotePort: HSTRING read get_RemotePort;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IDatagramSocketMessageReceivedEventArgs
  IDatagramSocketMessageReceivedEventArgs = interface(IInspectable)
  ['{9E2DDCA2-1712-4CE4-B179-8C652C6D107E}']
    function get_RemoteAddress: IHostName; safecall;
    function get_RemotePort: HSTRING; safecall;
    function get_LocalAddress: IHostName; safecall;
    function GetDataReader: IDataReader; safecall;
    function GetDataStream: IInputStream; safecall;
    property LocalAddress: IHostName read get_LocalAddress;
    property RemoteAddress: IHostName read get_RemoteAddress;
    property RemotePort: HSTRING read get_RemotePort;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IDatagramSocket,Windows.Networking.Sockets.IDatagramSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IDatagramSocket__IDatagramSocketMessageReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{4482E19B-2389-5767-9B0B-8D7A8EF55743}']
    procedure Invoke(sender: IDatagramSocket; args: IDatagramSocketMessageReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IDatagramSocket,Windows.Networking.Sockets.IDatagramSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IDatagramSocket__IDatagramSocketMessageReceivedEventArgs = interface(TypedEventHandler_2__IDatagramSocket__IDatagramSocketMessageReceivedEventArgs_Delegate_Base)
  ['{0A89BF8D-AD86-5D61-957C-6E8AC88380B4}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IDatagramSocket
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_DatagramSocket)]
  IDatagramSocket = interface(IInspectable)
  ['{7FE25BBB-C3BC-4677-8446-CA28A465A3AF}']
    function get_Control: IDatagramSocketControl; safecall;
    function get_Information: IDatagramSocketInformation; safecall;
    function get_OutputStream: IOutputStream; safecall;
    function ConnectAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncAction; overload; safecall;
    function ConnectAsync(endpointPair: IEndpointPair): IAsyncAction; overload; safecall;
    function BindServiceNameAsync(localServiceName: HSTRING): IAsyncAction; safecall;
    function BindEndpointAsync(localHostName: IHostName; localServiceName: HSTRING): IAsyncAction; safecall;
    procedure JoinMulticastGroup(host: IHostName); safecall;
    function GetOutputStreamAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncOperation_1__IOutputStream; overload; safecall;
    function GetOutputStreamAsync(endpointPair: IEndpointPair): IAsyncOperation_1__IOutputStream; overload; safecall;
    function add_MessageReceived(eventHandler: TypedEventHandler_2__IDatagramSocket__IDatagramSocketMessageReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_MessageReceived(eventCookie: EventRegistrationToken); safecall;
    property Control: IDatagramSocketControl read get_Control;
    property Information: IDatagramSocketInformation read get_Information;
    property OutputStream: IOutputStream read get_OutputStream;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Sockets.IStreamSocket>
  AsyncOperationCompletedHandler_1__IStreamSocket_Delegate_Base = interface(IUnknown)
  ['{71B5D99E-3854-5E9A-B4DC-D1B58BF198FC}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStreamSocket; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Sockets.IStreamSocket>
  AsyncOperationCompletedHandler_1__IStreamSocket = interface(AsyncOperationCompletedHandler_1__IStreamSocket_Delegate_Base)
  ['{A5755FC7-E478-5709-91EB-F4DE6C94B875}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Sockets.IStreamSocket>
  IAsyncOperation_1__IStreamSocket_Base = interface(IInspectable)
  ['{FB3E3D3C-6FE5-5E27-A132-902247E2A93E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStreamSocket); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStreamSocket; safecall;
    function GetResults: IStreamSocket; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStreamSocket read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Sockets.IStreamSocket>
  IAsyncOperation_1__IStreamSocket = interface(IAsyncOperation_1__IStreamSocket_Base)
  ['{673D41A9-FA65-5D68-A1A0-DC2E97A32F4A}']
  end;

  // Windows.Networking.Sockets.IControlChannelTriggerResetEventDetails
  IControlChannelTriggerResetEventDetails = interface(IInspectable)
  ['{6851038E-8EC4-42FE-9BB2-21E91B7BFCB1}']
    function get_ResetReason: ControlChannelTriggerResetReason; safecall;
    function get_HardwareSlotReset: Boolean; safecall;
    function get_SoftwareSlotReset: Boolean; safecall;
    property HardwareSlotReset: Boolean read get_HardwareSlotReset;
    property ResetReason: ControlChannelTriggerResetReason read get_ResetReason;
    property SoftwareSlotReset: Boolean read get_SoftwareSlotReset;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IDatagramSocket2
  IDatagramSocket2 = interface(IInspectable)
  ['{D83BA354-9A9D-4185-A20A-1424C9C2A7CD}']
    function BindServiceNameAsync(localServiceName: HSTRING; adapter: INetworkAdapter): IAsyncAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.ISocketActivityContext
  ISocketActivityContext = interface(IInspectable)
  ['{43B04D64-4C85-4396-A637-1D973F6EBD49}']
    function get_Data: IBuffer; safecall;
    property Data: IBuffer read get_Data;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IDatagramSocket3
  IDatagramSocket3 = interface(IInspectable)
  ['{37544F09-AB92-4306-9AC1-0C381283D9C6}']
    function CancelIOAsync: IAsyncAction; safecall;
    procedure EnableTransferOwnership(taskId: TGuid); overload; safecall;
    procedure EnableTransferOwnership(taskId: TGuid; connectedStandbyAction: SocketActivityConnectedStandbyAction); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING; data: ISocketActivityContext); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING; data: ISocketActivityContext; keepAliveTime: TimeSpan); overload; safecall;
  end;

  // Windows.Networking.Sockets.IDatagramSocketControl2
  IDatagramSocketControl2 = interface(IInspectable)
  ['{33EAD5C2-979C-4415-82A1-3CFAF646C192}']
    function get_InboundBufferSizeInBytes: Cardinal; safecall;
    procedure put_InboundBufferSizeInBytes(value: Cardinal); safecall;
    function get_DontFragment: Boolean; safecall;
    procedure put_DontFragment(value: Boolean); safecall;
    property DontFragment: Boolean read get_DontFragment write put_DontFragment;
    property InboundBufferSizeInBytes: Cardinal read get_InboundBufferSizeInBytes write put_InboundBufferSizeInBytes;
  end;

  // Windows.Networking.Sockets.IDatagramSocketControl3
  IDatagramSocketControl3 = interface(IInspectable)
  ['{D4EB8256-1F6D-4598-9B57-D42A001DF349}']
    function get_MulticastOnly: Boolean; safecall;
    procedure put_MulticastOnly(value: Boolean); safecall;
    property MulticastOnly: Boolean read get_MulticastOnly write put_MulticastOnly;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IDatagramSocketStatics
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_DatagramSocket)]
  IDatagramSocketStatics = interface(IInspectable)
  ['{E9C62AEE-1494-4A21-BB7E-8589FC751D9D}']
    function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; safecall;
    function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; sortOptions: HostNameSortOptions): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IWebSocketClosedEventArgs
  IWebSocketClosedEventArgs = interface(IInspectable)
  ['{CEB78D07-D0A8-4703-A091-C8C2C0915BC3}']
    function get_Code: Word; safecall;
    function get_Reason: HSTRING; safecall;
    property Code: Word read get_Code;
    property Reason: HSTRING read get_Reason;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IWebSocket,Windows.Networking.Sockets.IWebSocketClosedEventArgs>
  TypedEventHandler_2__IWebSocket__IWebSocketClosedEventArgs_Delegate_Base = interface(IUnknown)
  ['{03CF8F90-1669-5F4D-9404-2B784678E6DD}']
    procedure Invoke(sender: IWebSocket; args: IWebSocketClosedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IWebSocket,Windows.Networking.Sockets.IWebSocketClosedEventArgs>
  TypedEventHandler_2__IWebSocket__IWebSocketClosedEventArgs = interface(TypedEventHandler_2__IWebSocket__IWebSocketClosedEventArgs_Delegate_Base)
  ['{90FF48E9-4B0F-595B-A91E-E6669261220C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IWebSocket
  IWebSocket = interface(IInspectable)
  ['{F877396F-99B1-4E18-BC08-850C9ADF156E}']
    function get_OutputStream: IOutputStream; safecall;
    function ConnectAsync(uri: IUriRuntimeClass): IAsyncAction; safecall;
    procedure SetRequestHeader(headerName: HSTRING; headerValue: HSTRING); safecall;
    function add_Closed(eventHandler: TypedEventHandler_2__IWebSocket__IWebSocketClosedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closed(eventCookie: EventRegistrationToken); safecall;
    procedure Close(code: Word; reason: HSTRING); safecall;
    property OutputStream: IOutputStream read get_OutputStream;
  end;

  // Windows.Networking.Sockets.IWebSocketControl
  IWebSocketControl = interface(IInspectable)
  ['{2EC4BDC3-D9A5-455A-9811-DE24D45337E9}']
    function get_OutboundBufferSizeInBytes: Cardinal; safecall;
    procedure put_OutboundBufferSizeInBytes(value: Cardinal); safecall;
    function get_ServerCredential: IPasswordCredential; safecall;
    procedure put_ServerCredential(value: IPasswordCredential); safecall;
    function get_ProxyCredential: IPasswordCredential; safecall;
    procedure put_ProxyCredential(value: IPasswordCredential); safecall;
    function get_SupportedProtocols: IVector_1__HSTRING; safecall;
    property OutboundBufferSizeInBytes: Cardinal read get_OutboundBufferSizeInBytes write put_OutboundBufferSizeInBytes;
    property ProxyCredential: IPasswordCredential read get_ProxyCredential write put_ProxyCredential;
    property ServerCredential: IPasswordCredential read get_ServerCredential write put_ServerCredential;
    property SupportedProtocols: IVector_1__HSTRING read get_SupportedProtocols;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IMessageWebSocketControl
  IMessageWebSocketControl = interface(IInspectable)
  ['{8118388A-C629-4F0A-80FB-81FC05538862}']
    function get_MaxMessageSize: Cardinal; safecall;
    procedure put_MaxMessageSize(value: Cardinal); safecall;
    function get_MessageType: SocketMessageType; safecall;
    procedure put_MessageType(value: SocketMessageType); safecall;
    property MaxMessageSize: Cardinal read get_MaxMessageSize write put_MaxMessageSize;
    property MessageType: SocketMessageType read get_MessageType write put_MessageType;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IWebSocketInformation
  IWebSocketInformation = interface(IInspectable)
  ['{5E01E316-C92A-47A5-B25F-07847639D181}']
    function get_LocalAddress: IHostName; safecall;
    function get_BandwidthStatistics: BandwidthStatistics; safecall;
    function get_Protocol: HSTRING; safecall;
    property BandwidthStatistics_: BandwidthStatistics read get_BandwidthStatistics;
    property LocalAddress: IHostName read get_LocalAddress;
    property Protocol: HSTRING read get_Protocol;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs
  IMessageWebSocketMessageReceivedEventArgs = interface(IInspectable)
  ['{478C22AC-4C4B-42ED-9ED7-1EF9F94FA3D5}']
    function get_MessageType: SocketMessageType; safecall;
    function GetDataReader: IDataReader; safecall;
    function GetDataStream: IInputStream; safecall;
    property MessageType: SocketMessageType read get_MessageType;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IMessageWebSocket,Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{DE980538-6DCD-52B0-802F-4B6CF59A01AB}']
    procedure Invoke(sender: IMessageWebSocket; args: IMessageWebSocketMessageReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IMessageWebSocket,Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs = interface(TypedEventHandler_2__IMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs_Delegate_Base)
  ['{4648E17B-49F1-5F4F-ADCE-761EC022B556}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IMessageWebSocket
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_MessageWebSocket)]
  IMessageWebSocket = interface(IInspectable)
  ['{33727D08-34D5-4746-AD7B-8DDE5BC2EF88}']
    function get_Control: IMessageWebSocketControl; safecall;
    function get_Information: IWebSocketInformation; safecall;
    function add_MessageReceived(eventHandler: TypedEventHandler_2__IMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_MessageReceived(eventCookie: EventRegistrationToken); safecall;
    property Control: IMessageWebSocketControl read get_Control;
    property Information: IWebSocketInformation read get_Information;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs
  IWebSocketServerCustomValidationRequestedEventArgs = interface(IInspectable)
  ['{FFEFFE48-022A-4AB7-8B36-E10AF4640E6B}']
    function get_ServerCertificate: Certificates_ICertificate; safecall;
    function get_ServerCertificateErrorSeverity: SocketSslErrorSeverity; safecall;
    function get_ServerCertificateErrors: IVectorView_1__Certificates_ChainValidationResult; safecall;
    function get_ServerIntermediateCertificates: IVectorView_1__Certificates_ICertificate; safecall;
    procedure Reject; safecall;
    function GetDeferral: IDeferral; safecall;
    property ServerCertificate: Certificates_ICertificate read get_ServerCertificate;
    property ServerCertificateErrorSeverity: SocketSslErrorSeverity read get_ServerCertificateErrorSeverity;
    property ServerCertificateErrors: IVectorView_1__Certificates_ChainValidationResult read get_ServerCertificateErrors;
    property ServerIntermediateCertificates: IVectorView_1__Certificates_ICertificate read get_ServerIntermediateCertificates;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IMessageWebSocket,Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs>
  TypedEventHandler_2__IMessageWebSocket__IWebSocketServerCustomValidationRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{2C34C585-9CF6-56C7-8DD1-5DA26E322078}']
    procedure Invoke(sender: IMessageWebSocket; args: IWebSocketServerCustomValidationRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IMessageWebSocket,Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs>
  TypedEventHandler_2__IMessageWebSocket__IWebSocketServerCustomValidationRequestedEventArgs = interface(TypedEventHandler_2__IMessageWebSocket__IWebSocketServerCustomValidationRequestedEventArgs_Delegate_Base)
  ['{BCDA6FC2-B686-54FF-81E8-235E7421473B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IMessageWebSocket2
  IMessageWebSocket2 = interface(IInspectable)
  ['{BED0CEE7-F9C8-440A-9AD5-737281D9742E}']
    function add_ServerCustomValidationRequested(eventHandler: TypedEventHandler_2__IMessageWebSocket__IWebSocketServerCustomValidationRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ServerCustomValidationRequested(eventCookie: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IMessageWebSocket3
  IMessageWebSocket3 = interface(IInspectable)
  ['{59D9DEFB-71AF-4349-8487-911FCF681597}']
    function SendNonfinalFrameAsync(data: IBuffer): IAsyncOperationWithProgress_2__Cardinal__Cardinal; safecall;
    function SendFinalFrameAsync(data: IBuffer): IAsyncOperationWithProgress_2__Cardinal__Cardinal; safecall;
  end;

  // Windows.Networking.Sockets.IMessageWebSocketControl2
  IMessageWebSocketControl2 = interface(IInspectable)
  ['{E30FD791-080C-400A-A712-27DFA9E744D8}']
    function get_DesiredUnsolicitedPongInterval: TimeSpan; safecall;
    procedure put_DesiredUnsolicitedPongInterval(value: TimeSpan); safecall;
    function get_ActualUnsolicitedPongInterval: TimeSpan; safecall;
    function get_ReceiveMode: MessageWebSocketReceiveMode; safecall;
    procedure put_ReceiveMode(value: MessageWebSocketReceiveMode); safecall;
    function get_ClientCertificate: Certificates_ICertificate; safecall;
    procedure put_ClientCertificate(value: Certificates_ICertificate); safecall;
    property ActualUnsolicitedPongInterval: TimeSpan read get_ActualUnsolicitedPongInterval;
    property ClientCertificate: Certificates_ICertificate read get_ClientCertificate write put_ClientCertificate;
    property DesiredUnsolicitedPongInterval: TimeSpan read get_DesiredUnsolicitedPongInterval write put_DesiredUnsolicitedPongInterval;
    property ReceiveMode: MessageWebSocketReceiveMode read get_ReceiveMode write put_ReceiveMode;
  end;

  // Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs2
  IMessageWebSocketMessageReceivedEventArgs2 = interface(IInspectable)
  ['{89CE06FD-DD6F-4A07-87F9-F9EB4D89D83D}']
    function get_IsMessageComplete: Boolean; safecall;
    property IsMessageComplete: Boolean read get_IsMessageComplete;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IServerMessageWebSocket,Windows.Networking.Sockets.IMessageWebSocketMessageReceivedEventArgs>
  TypedEventHandler_2__IServerMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs = interface(IUnknown)
  ['{8AD19FC8-605D-5E39-9B62-4AF4DA6F63B4}']
    procedure Invoke(sender: IServerMessageWebSocket; args: IMessageWebSocketMessageReceivedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IServerMessageWebSocketControl
  IServerMessageWebSocketControl = interface(IInspectable)
  ['{69C2F051-1C1F-587A-4519-2181610192B7}']
    function get_MessageType: SocketMessageType; safecall;
    procedure put_MessageType(value: SocketMessageType); safecall;
    property MessageType: SocketMessageType read get_MessageType write put_MessageType;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IServerMessageWebSocketInformation
  IServerMessageWebSocketInformation = interface(IInspectable)
  ['{FC32B45F-4448-5505-6CC9-09AFA8915F5D}']
    function get_BandwidthStatistics: BandwidthStatistics; safecall;
    function get_Protocol: HSTRING; safecall;
    function get_LocalAddress: IHostName; safecall;
    property BandwidthStatistics_: BandwidthStatistics read get_BandwidthStatistics;
    property LocalAddress: IHostName read get_LocalAddress;
    property Protocol: HSTRING read get_Protocol;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IServerMessageWebSocket,Windows.Networking.Sockets.IWebSocketClosedEventArgs>
  TypedEventHandler_2__IServerMessageWebSocket__IWebSocketClosedEventArgs = interface(IUnknown)
  ['{77A71B70-3494-5D11-8110-6138371DBF41}']
    procedure Invoke(sender: IServerMessageWebSocket; args: IWebSocketClosedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IServerMessageWebSocket
  IServerMessageWebSocket = interface(IInspectable)
  ['{E3AC9240-813B-5EFD-7E11-AE2305FC77F1}']
    function add_MessageReceived(value: TypedEventHandler_2__IServerMessageWebSocket__IMessageWebSocketMessageReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_MessageReceived(token: EventRegistrationToken); safecall;
    function get_Control: IServerMessageWebSocketControl; safecall;
    function get_Information: IServerMessageWebSocketInformation; safecall;
    function get_OutputStream: IOutputStream; safecall;
    function add_Closed(value: TypedEventHandler_2__IServerMessageWebSocket__IWebSocketClosedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
    procedure Close(code: Word; reason: HSTRING); safecall;
    property Control: IServerMessageWebSocketControl read get_Control;
    property Information: IServerMessageWebSocketInformation read get_Information;
    property OutputStream: IOutputStream read get_OutputStream;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IServerStreamWebSocketInformation
  IServerStreamWebSocketInformation = interface(IInspectable)
  ['{FC32B45F-4448-5505-6CC9-09ABA8915F5D}']
    function get_BandwidthStatistics: BandwidthStatistics; safecall;
    function get_Protocol: HSTRING; safecall;
    function get_LocalAddress: IHostName; safecall;
    property BandwidthStatistics_: BandwidthStatistics read get_BandwidthStatistics;
    property LocalAddress: IHostName read get_LocalAddress;
    property Protocol: HSTRING read get_Protocol;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IServerStreamWebSocket,Windows.Networking.Sockets.IWebSocketClosedEventArgs>
  TypedEventHandler_2__IServerStreamWebSocket__IWebSocketClosedEventArgs = interface(IUnknown)
  ['{6A162D34-CAF7-57F3-92BF-FC684C665EC4}']
    procedure Invoke(sender: IServerStreamWebSocket; args: IWebSocketClosedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IServerStreamWebSocket
  IServerStreamWebSocket = interface(IInspectable)
  ['{2CED5BBF-74F6-55E4-79DF-9132680DFEE8}']
    function get_Information: IServerStreamWebSocketInformation; safecall;
    function get_InputStream: IInputStream; safecall;
    function get_OutputStream: IOutputStream; safecall;
    function add_Closed(value: TypedEventHandler_2__IServerStreamWebSocket__IWebSocketClosedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
    procedure Close(code: Word; reason: HSTRING); safecall;
    property Information: IServerStreamWebSocketInformation read get_Information;
    property InputStream: IInputStream read get_InputStream;
    property OutputStream: IOutputStream read get_OutputStream;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.ISocketErrorStatics
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_SocketError)]
  ISocketErrorStatics = interface(IInspectable)
  ['{828337F4-7D56-4D8E-B7B4-A07DD7C1BCA9}']
    function GetStatus(hresult: Integer): SocketErrorStatus; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocket2
  IStreamSocket2 = interface(IInspectable)
  ['{29D0E575-F314-4D09-ADF0-0FBD967FBD9F}']
    function ConnectAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; protectionLevel: SocketProtectionLevel; adapter: INetworkAdapter): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocket3
  IStreamSocket3 = interface(IInspectable)
  ['{3F430B00-9D28-4854-BAC3-2301941EC223}']
    function CancelIOAsync: IAsyncAction; safecall;
    procedure EnableTransferOwnership(taskId: TGuid); overload; safecall;
    procedure EnableTransferOwnership(taskId: TGuid; connectedStandbyAction: SocketActivityConnectedStandbyAction); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING; data: ISocketActivityContext); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING; data: ISocketActivityContext; keepAliveTime: TimeSpan); overload; safecall;
  end;

  // Windows.Networking.Sockets.IStreamSocketControl2
  IStreamSocketControl2 = interface(IInspectable)
  ['{C2D09A56-060F-44C1-B8E2-1FBF60BD62C5}']
    function get_IgnorableServerCertificateErrors: IVector_1__Certificates_ChainValidationResult; safecall;
    property IgnorableServerCertificateErrors: IVector_1__Certificates_ChainValidationResult read get_IgnorableServerCertificateErrors;
  end;

  // Windows.Networking.Sockets.IStreamSocketControl3
  IStreamSocketControl3 = interface(IInspectable)
  ['{C56A444C-4E74-403E-894C-B31CAE5C7342}']
    function get_SerializeConnectionAttempts: Boolean; safecall;
    procedure put_SerializeConnectionAttempts(value: Boolean); safecall;
    function get_ClientCertificate: Certificates_ICertificate; safecall;
    procedure put_ClientCertificate(value: Certificates_ICertificate); safecall;
    property ClientCertificate: Certificates_ICertificate read get_ClientCertificate write put_ClientCertificate;
    property SerializeConnectionAttempts: Boolean read get_SerializeConnectionAttempts write put_SerializeConnectionAttempts;
  end;

  // Windows.Networking.Sockets.IStreamSocketControl4
  IStreamSocketControl4 = interface(IInspectable)
  ['{964E2B3D-EC27-4888-B3CE-C74B418423AD}']
    function get_MinProtectionLevel: SocketProtectionLevel; safecall;
    procedure put_MinProtectionLevel(value: SocketProtectionLevel); safecall;
    property MinProtectionLevel: SocketProtectionLevel read get_MinProtectionLevel write put_MinProtectionLevel;
  end;

  // Windows.Networking.Sockets.IStreamSocketInformation2
  IStreamSocketInformation2 = interface(IInspectable)
  ['{12C28452-4BDC-4EE4-976A-CF130E9D92E3}']
    function get_ServerCertificateErrorSeverity: SocketSslErrorSeverity; safecall;
    function get_ServerCertificateErrors: IVectorView_1__Certificates_ChainValidationResult; safecall;
    function get_ServerCertificate: Certificates_ICertificate; safecall;
    function get_ServerIntermediateCertificates: IVectorView_1__Certificates_ICertificate; safecall;
    property ServerCertificate: Certificates_ICertificate read get_ServerCertificate;
    property ServerCertificateErrorSeverity: SocketSslErrorSeverity read get_ServerCertificateErrorSeverity;
    property ServerCertificateErrors: IVectorView_1__Certificates_ChainValidationResult read get_ServerCertificateErrors;
    property ServerIntermediateCertificates: IVectorView_1__Certificates_ICertificate read get_ServerIntermediateCertificates;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketListener2
  IStreamSocketListener2 = interface(IInspectable)
  ['{658DC13E-BB3E-4458-B232-ED1088694B98}']
    function BindServiceNameAsync(localServiceName: HSTRING; protectionLevel: SocketProtectionLevel): IAsyncAction; overload; safecall;
    function BindServiceNameAsync(localServiceName: HSTRING; protectionLevel: SocketProtectionLevel; adapter: INetworkAdapter): IAsyncAction; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketListener3
  IStreamSocketListener3 = interface(IInspectable)
  ['{4798201C-BDF8-4919-8542-28D450E74507}']
    function CancelIOAsync: IAsyncAction; safecall;
    procedure EnableTransferOwnership(taskId: TGuid); overload; safecall;
    procedure EnableTransferOwnership(taskId: TGuid; connectedStandbyAction: SocketActivityConnectedStandbyAction); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING); overload; safecall;
    procedure TransferOwnership(socketId: HSTRING; data: ISocketActivityContext); overload; safecall;
  end;

  // Windows.Networking.Sockets.IStreamSocketListenerControl2
  IStreamSocketListenerControl2 = interface(IInspectable)
  ['{948BB665-2C3E-404B-B8B0-8EB249A2B0A1}']
    function get_NoDelay: Boolean; safecall;
    procedure put_NoDelay(value: Boolean); safecall;
    function get_KeepAlive: Boolean; safecall;
    procedure put_KeepAlive(value: Boolean); safecall;
    function get_OutboundBufferSizeInBytes: Cardinal; safecall;
    procedure put_OutboundBufferSizeInBytes(value: Cardinal); safecall;
    function get_OutboundUnicastHopLimit: Byte; safecall;
    procedure put_OutboundUnicastHopLimit(value: Byte); safecall;
    property KeepAlive: Boolean read get_KeepAlive write put_KeepAlive;
    property NoDelay: Boolean read get_NoDelay write put_NoDelay;
    property OutboundBufferSizeInBytes: Cardinal read get_OutboundBufferSizeInBytes write put_OutboundBufferSizeInBytes;
    property OutboundUnicastHopLimit: Byte read get_OutboundUnicastHopLimit write put_OutboundUnicastHopLimit;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamSocketStatics
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_StreamSocket)]
  IStreamSocketStatics = interface(IInspectable)
  ['{A420BC4A-6E2E-4AF5-B556-355AE0CD4F29}']
    function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; safecall;
    function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; sortOptions: HostNameSortOptions): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamWebSocketControl
  IStreamWebSocketControl = interface(IInspectable)
  ['{B4F478B1-A45A-48DB-953A-645B7D964C07}']
    function get_NoDelay: Boolean; safecall;
    procedure put_NoDelay(value: Boolean); safecall;
    property NoDelay: Boolean read get_NoDelay write put_NoDelay;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamWebSocket
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_StreamWebSocket)]
  IStreamWebSocket = interface(IInspectable)
  ['{BD4A49D8-B289-45BB-97EB-C7525205A843}']
    function get_Control: IStreamWebSocketControl; safecall;
    function get_Information: IWebSocketInformation; safecall;
    function get_InputStream: IInputStream; safecall;
    property Control: IStreamWebSocketControl read get_Control;
    property Information: IWebSocketInformation read get_Information;
    property InputStream: IInputStream read get_InputStream;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IStreamWebSocket,Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs>
  TypedEventHandler_2__IStreamWebSocket__IWebSocketServerCustomValidationRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{18B143D3-897E-595E-ACC1-EF35614B4CEC}']
    procedure Invoke(sender: IStreamWebSocket; args: IWebSocketServerCustomValidationRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.Sockets.IStreamWebSocket,Windows.Networking.Sockets.IWebSocketServerCustomValidationRequestedEventArgs>
  TypedEventHandler_2__IStreamWebSocket__IWebSocketServerCustomValidationRequestedEventArgs = interface(TypedEventHandler_2__IStreamWebSocket__IWebSocketServerCustomValidationRequestedEventArgs_Delegate_Base)
  ['{E56170FE-9025-5791-9FD9-BC68D49563E1}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IStreamWebSocket2
  IStreamWebSocket2 = interface(IInspectable)
  ['{AA4D08CB-93F5-4678-8236-57CCE5417ED5}']
    function add_ServerCustomValidationRequested(eventHandler: TypedEventHandler_2__IStreamWebSocket__IWebSocketServerCustomValidationRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ServerCustomValidationRequested(eventCookie: EventRegistrationToken); safecall;
  end;

  // Windows.Networking.Sockets.IStreamWebSocketControl2
  IStreamWebSocketControl2 = interface(IInspectable)
  ['{215D9F7E-FA58-40DA-9F11-A48DAFE95037}']
    function get_DesiredUnsolicitedPongInterval: TimeSpan; safecall;
    procedure put_DesiredUnsolicitedPongInterval(value: TimeSpan); safecall;
    function get_ActualUnsolicitedPongInterval: TimeSpan; safecall;
    function get_ClientCertificate: Certificates_ICertificate; safecall;
    procedure put_ClientCertificate(value: Certificates_ICertificate); safecall;
    property ActualUnsolicitedPongInterval: TimeSpan read get_ActualUnsolicitedPongInterval;
    property ClientCertificate: Certificates_ICertificate read get_ClientCertificate write put_ClientCertificate;
    property DesiredUnsolicitedPongInterval: TimeSpan read get_DesiredUnsolicitedPongInterval write put_DesiredUnsolicitedPongInterval;
  end;

  // Windows.Networking.Sockets.IWebSocketControl2
  IWebSocketControl2 = interface(IInspectable)
  ['{79C3BE03-F2CA-461E-AF4E-9665BC2D0620}']
    function get_IgnorableServerCertificateErrors: IVector_1__Certificates_ChainValidationResult; safecall;
    property IgnorableServerCertificateErrors: IVector_1__Certificates_ChainValidationResult read get_IgnorableServerCertificateErrors;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Sockets.IWebSocketErrorStatics
  [WinRTClassNameAttribute(SWindows_Networking_Sockets_WebSocketError)]
  IWebSocketErrorStatics = interface(IInspectable)
  ['{27CDF35B-1F61-4709-8E02-61283ADA4E9D}']
    function GetStatus(hresult: Integer): WebErrorStatus; safecall;
  end;

  // Windows.Networking.Sockets.IWebSocketInformation2
  IWebSocketInformation2 = interface(IInspectable)
  ['{CE1D39CE-A1B7-4D43-8269-8D5B981BD47A}']
    function get_ServerCertificate: Certificates_ICertificate; safecall;
    function get_ServerCertificateErrorSeverity: SocketSslErrorSeverity; safecall;
    function get_ServerCertificateErrors: IVectorView_1__Certificates_ChainValidationResult; safecall;
    function get_ServerIntermediateCertificates: IVectorView_1__Certificates_ICertificate; safecall;
    property ServerCertificate: Certificates_ICertificate read get_ServerCertificate;
    property ServerCertificateErrorSeverity: SocketSslErrorSeverity read get_ServerCertificateErrorSeverity;
    property ServerCertificateErrors: IVectorView_1__Certificates_ChainValidationResult read get_ServerCertificateErrors;
    property ServerIntermediateCertificates: IVectorView_1__Certificates_ICertificate read get_ServerIntermediateCertificates;
  end;

  // Windows.Networking.Sockets.DatagramSocket
  // DualAPI
  // Implements: Windows.Networking.Sockets.IDatagramSocket
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.Networking.Sockets.IDatagramSocket2
  // Implements: Windows.Networking.Sockets.IDatagramSocket3
  // Statics: "Windows.Networking.Sockets.IDatagramSocketStatics"
  // Instantiable: "IDatagramSocket"
  TDatagramSocket = class(TWinRTGenericImportSI<IDatagramSocketStatics, IDatagramSocket>)
  public
    // -> IDatagramSocketStatics
    class function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; static; inline;
    class function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; sortOptions: HostNameSortOptions): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; static; inline;
  end;

  // Windows.Networking.Sockets.MessageWebSocket
  // DualAPI
  // Implements: Windows.Networking.Sockets.IMessageWebSocket
  // Implements: Windows.Networking.Sockets.IWebSocket
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.Networking.Sockets.IMessageWebSocket2
  // Implements: Windows.Networking.Sockets.IMessageWebSocket3
  // Instantiable: "IMessageWebSocket"
  TMessageWebSocket = class(TWinRTGenericImportI<IMessageWebSocket>) end;

  // Windows.Networking.Sockets.SocketError
  // DualAPI
  // Statics: "Windows.Networking.Sockets.ISocketErrorStatics"
  TSocketError = class(TWinRTGenericImportS<ISocketErrorStatics>)
  public
    // -> ISocketErrorStatics
    class function GetStatus(hresult: Integer): SocketErrorStatus; static; inline;
  end;

  // Windows.Networking.Sockets.StreamSocket
  // DualAPI
  // Implements: Windows.Networking.Sockets.IStreamSocket
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.Networking.Sockets.IStreamSocket2
  // Implements: Windows.Networking.Sockets.IStreamSocket3
  // Statics: "Windows.Networking.Sockets.IStreamSocketStatics"
  // Instantiable: "IStreamSocket"
  TStreamSocket = class(TWinRTGenericImportSI<IStreamSocketStatics, IStreamSocket>)
  public
    // -> IStreamSocketStatics
    class function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; static; inline;
    class function GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; sortOptions: HostNameSortOptions): IAsyncOperation_1__IVectorView_1__IEndpointPair; overload; static; inline;
  end;

  // Windows.Networking.Sockets.StreamSocketListener
  // DualAPI
  // Implements: Windows.Networking.Sockets.IStreamSocketListener
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.Networking.Sockets.IStreamSocketListener2
  // Implements: Windows.Networking.Sockets.IStreamSocketListener3
  // Instantiable: "IStreamSocketListener"
  TStreamSocketListener = class(TWinRTGenericImportI<IStreamSocketListener>) end;

  // Windows.Networking.Sockets.StreamWebSocket
  // DualAPI
  // Implements: Windows.Networking.Sockets.IStreamWebSocket
  // Implements: Windows.Networking.Sockets.IWebSocket
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.Networking.Sockets.IStreamWebSocket2
  // Instantiable: "IStreamWebSocket"
  TStreamWebSocket = class(TWinRTGenericImportI<IStreamWebSocket>) end;

  // Windows.Networking.Sockets.WebSocketError
  // DualAPI
  // Statics: "Windows.Networking.Sockets.IWebSocketErrorStatics"
  TWebSocketError = class(TWinRTGenericImportS<IWebSocketErrorStatics>)
  public
    // -> IWebSocketErrorStatics
    class function GetStatus(hresult: Integer): WebErrorStatus; static; inline;
  end;

implementation

{ TDatagramSocket }

class function TDatagramSocket.GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncOperation_1__IVectorView_1__IEndpointPair;
begin
  Result := Statics.GetEndpointPairsAsync(remoteHostName, remoteServiceName);
end;

class function TDatagramSocket.GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; sortOptions: HostNameSortOptions): IAsyncOperation_1__IVectorView_1__IEndpointPair;
begin
  Result := Statics.GetEndpointPairsAsync(remoteHostName, remoteServiceName, sortOptions);
end;


{ TMessageWebSocket }

{ TSocketError }

class function TSocketError.GetStatus(hresult: Integer): SocketErrorStatus;
begin
  Result := Statics.GetStatus(hresult);
end;


{ TStreamSocket }

class function TStreamSocket.GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING): IAsyncOperation_1__IVectorView_1__IEndpointPair;
begin
  Result := Statics.GetEndpointPairsAsync(remoteHostName, remoteServiceName);
end;

class function TStreamSocket.GetEndpointPairsAsync(remoteHostName: IHostName; remoteServiceName: HSTRING; sortOptions: HostNameSortOptions): IAsyncOperation_1__IVectorView_1__IEndpointPair;
begin
  Result := Statics.GetEndpointPairsAsync(remoteHostName, remoteServiceName, sortOptions);
end;


{ TStreamSocketListener }

{ TStreamWebSocket }

{ TWebSocketError }

class function TWebSocketError.GetStatus(hresult: Integer): WebErrorStatus;
begin
  Result := Statics.GetStatus(hresult);
end;


end.
