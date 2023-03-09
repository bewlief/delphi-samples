{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Networking.Vpn;

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

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Vpn.VpnManagementErrorStatus>
  AsyncOperationCompletedHandler_1__VpnManagementErrorStatus = interface;
  PAsyncOperationCompletedHandler_1__VpnManagementErrorStatus = ^AsyncOperationCompletedHandler_1__VpnManagementErrorStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Vpn.VpnManagementErrorStatus>
  IAsyncOperation_1__VpnManagementErrorStatus = interface;
  PIAsyncOperation_1__VpnManagementErrorStatus = ^IAsyncOperation_1__VpnManagementErrorStatus;

  // Windows.Networking.Vpn Enums

  // Windows.Networking.Vpn.VpnAppIdType
  VpnAppIdType = (
    PackageFamilyName = 0,
    FullyQualifiedBinaryName = 1,
    FilePath = 2
  );
  PVpnAppIdType = ^VpnAppIdType;

  // Windows.Networking.Vpn.VpnAuthenticationMethod
  VpnAuthenticationMethod = (
    Mschapv2 = 0,
    Eap = 1,
    Certificate = 2,
    PresharedKey = 3
  );
  PVpnAuthenticationMethod = ^VpnAuthenticationMethod;

  // Windows.Networking.Vpn.VpnChannelActivityEventType
  VpnChannelActivityEventType = (
    Idle = 0,
    Active = 1
  );
  PVpnChannelActivityEventType = ^VpnChannelActivityEventType;

  // Windows.Networking.Vpn.VpnChannelRequestCredentialsOptions
  VpnChannelRequestCredentialsOptions = (
    None = 0,
    Retrying = 1,
    UseForSingleSignIn = 2
  );
  PVpnChannelRequestCredentialsOptions = ^VpnChannelRequestCredentialsOptions;

  // Windows.Networking.Vpn.VpnCredentialType
  VpnCredentialType = (
    UsernamePassword = 0,
    UsernameOtpPin = 1,
    UsernamePasswordAndPin = 2,
    UsernamePasswordChange = 3,
    SmartCard = 4,
    ProtectedCertificate = 5,
    UnProtectedCertificate = 6
  );
  PVpnCredentialType = ^VpnCredentialType;

  // Windows.Networking.Vpn.VpnDataPathType
  VpnDataPathType = (
    Send = 0,
    Receive = 1
  );
  PVpnDataPathType = ^VpnDataPathType;

  // Windows.Networking.Vpn.VpnDomainNameType
  VpnDomainNameType = (
    Suffix = 0,
    FullyQualified = 1,
    Reserved = 65535
  );
  PVpnDomainNameType = ^VpnDomainNameType;

  // Windows.Networking.Vpn.VpnIPProtocol
  VpnIPProtocol = (
    None = 0,
    Tcp = 6,
    Udp = 17,
    Icmp = 1,
    Ipv6Icmp = 58,
    Igmp = 2,
    Pgm = 113
  );
  PVpnIPProtocol = ^VpnIPProtocol;

  // Windows.Networking.Vpn.VpnManagementConnectionStatus
  VpnManagementConnectionStatus = (
    Disconnected = 0,
    Disconnecting = 1,
    Connected = 2,
    Connecting = 3
  );
  PVpnManagementConnectionStatus = ^VpnManagementConnectionStatus;

  // Windows.Networking.Vpn.VpnManagementErrorStatus
  VpnManagementErrorStatus = (
    Ok = 0,
    Other = 1,
    InvalidXmlSyntax = 2,
    ProfileNameTooLong = 3,
    ProfileInvalidAppId = 4,
    AccessDenied = 5,
    CannotFindProfile = 6,
    AlreadyDisconnecting = 7,
    AlreadyConnected = 8,
    GeneralAuthenticationFailure = 9,
    EapFailure = 10,
    SmartCardFailure = 11,
    CertificateFailure = 12,
    ServerConfiguration = 13,
    NoConnection = 14,
    ServerConnection = 15,
    UserNamePassword = 16,
    DnsNotResolvable = 17,
    InvalidIP = 18
  );
  PVpnManagementErrorStatus = ^VpnManagementErrorStatus;

  // Windows.Networking.Vpn.VpnNativeProtocolType
  VpnNativeProtocolType = (
    Pptp = 0,
    L2tp = 1,
    IpsecIkev2 = 2
  );
  PVpnNativeProtocolType = ^VpnNativeProtocolType;

  // Windows.Networking.Vpn.VpnPacketBufferStatus
  VpnPacketBufferStatus = (
    Ok = 0,
    InvalidBufferSize = 1
  );
  PVpnPacketBufferStatus = ^VpnPacketBufferStatus;

  // Windows.Networking.Vpn.VpnRoutingPolicyType
  VpnRoutingPolicyType = (
    SplitRouting = 0,
    ForceAllTrafficOverVpn = 1
  );
  PVpnRoutingPolicyType = ^VpnRoutingPolicyType;

  // Windows.Networking.Vpn Interfaces

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Vpn.VpnManagementErrorStatus>
  AsyncOperationCompletedHandler_1__VpnManagementErrorStatus_Delegate_Base = interface(IUnknown)
  ['{31229F8C-709D-5017-8629-57EF1289E616}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__VpnManagementErrorStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Vpn.VpnManagementErrorStatus>
  AsyncOperationCompletedHandler_1__VpnManagementErrorStatus = interface(AsyncOperationCompletedHandler_1__VpnManagementErrorStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Vpn.VpnManagementErrorStatus>
  IAsyncOperation_1__VpnManagementErrorStatus_Base = interface(IInspectable)
  ['{E99B86DC-6B65-5F23-9419-90B55852F283}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__VpnManagementErrorStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__VpnManagementErrorStatus; safecall;
    function GetResults: VpnManagementErrorStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__VpnManagementErrorStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Vpn.VpnManagementErrorStatus>
  IAsyncOperation_1__VpnManagementErrorStatus = interface(IAsyncOperation_1__VpnManagementErrorStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

implementation

end.
