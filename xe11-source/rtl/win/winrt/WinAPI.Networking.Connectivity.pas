{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Networking.Connectivity;

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
  Winapi.AI, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  AsyncOperationCompletedHandler_1__IConnectionProfile_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IConnectionProfile_Delegate_Base;
  AsyncOperationCompletedHandler_1__IConnectionProfile = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IConnectionProfile;
  PAsyncOperationCompletedHandler_1__IConnectionProfile = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IConnectionProfile;
  IAsyncOperation_1__IConnectionProfile_Base = Winapi.CommonTypes.IAsyncOperation_1__IConnectionProfile_Base;
  IAsyncOperation_1__IConnectionProfile = Winapi.CommonTypes.IAsyncOperation_1__IConnectionProfile;
  PIAsyncOperation_1__IConnectionProfile = Winapi.CommonTypes.PIAsyncOperation_1__IConnectionProfile;
  IConnectionCost = Winapi.CommonTypes.IConnectionCost;
  PIConnectionCost = Winapi.CommonTypes.PIConnectionCost;
  IConnectionProfile = Winapi.CommonTypes.IConnectionProfile;
  PIConnectionProfile = Winapi.CommonTypes.PIConnectionProfile;
  IDataPlanStatus = Winapi.CommonTypes.IDataPlanStatus;
  PIDataPlanStatus = Winapi.CommonTypes.PIDataPlanStatus;
  IDataPlanUsage = Winapi.CommonTypes.IDataPlanUsage;
  PIDataPlanUsage = Winapi.CommonTypes.PIDataPlanUsage;
  IDataUsage = Winapi.CommonTypes.IDataUsage;
  PIDataUsage = Winapi.CommonTypes.PIDataUsage;
  IIPInformation = Winapi.CommonTypes.IIPInformation;
  PIIPInformation = Winapi.CommonTypes.PIIPInformation;
  INetworkAdapter = Winapi.CommonTypes.INetworkAdapter;
  PINetworkAdapter = Winapi.CommonTypes.PINetworkAdapter;
  INetworkItem = Winapi.CommonTypes.INetworkItem;
  PINetworkItem = Winapi.CommonTypes.PINetworkItem;
  INetworkSecuritySettings = Winapi.CommonTypes.INetworkSecuritySettings;
  PINetworkSecuritySettings = Winapi.CommonTypes.PINetworkSecuritySettings;
  IVectorView_1__IConnectionProfile = Winapi.CommonTypes.IVectorView_1__IConnectionProfile;
  PIVectorView_1__IConnectionProfile = Winapi.CommonTypes.PIVectorView_1__IConnectionProfile;
  NetworkAuthenticationType = Winapi.CommonTypes.NetworkAuthenticationType;
  PNetworkAuthenticationType = Winapi.CommonTypes.PNetworkAuthenticationType;
  NetworkConnectivityLevel = Winapi.CommonTypes.NetworkConnectivityLevel;
  PNetworkConnectivityLevel = Winapi.CommonTypes.PNetworkConnectivityLevel;
  NetworkCostType = Winapi.CommonTypes.NetworkCostType;
  PNetworkCostType = Winapi.CommonTypes.PNetworkCostType;
  NetworkEncryptionType = Winapi.CommonTypes.NetworkEncryptionType;
  PNetworkEncryptionType = Winapi.CommonTypes.PNetworkEncryptionType;
  NetworkTypes = Winapi.CommonTypes.NetworkTypes;
  PNetworkTypes = Winapi.CommonTypes.PNetworkTypes;
  RoamingStates = Winapi.CommonTypes.RoamingStates;
  PRoamingStates = Winapi.CommonTypes.PRoamingStates;

  // Forward declarations for interfaces

  // Windows.Networking.Connectivity.ICellularApnContext
  ICellularApnContext = interface;
  PICellularApnContext = ^ICellularApnContext;

  // Windows.Networking.Connectivity.ICellularApnContext2
  ICellularApnContext2 = interface;
  PICellularApnContext2 = ^ICellularApnContext2;

  // Windows.Networking.Connectivity.IConnectionCost2
  IConnectionCost2 = interface;
  PIConnectionCost2 = ^IConnectionCost2;

  // Windows.Networking.Connectivity.IWwanConnectionProfileDetails
  IWwanConnectionProfileDetails = interface;
  PIWwanConnectionProfileDetails = ^IWwanConnectionProfileDetails;

  // Windows.Networking.Connectivity.IWlanConnectionProfileDetails
  IWlanConnectionProfileDetails = interface;
  PIWlanConnectionProfileDetails = ^IWlanConnectionProfileDetails;

  // Windows.Networking.Connectivity.INetworkUsage
  INetworkUsage = interface;
  PINetworkUsage = ^INetworkUsage;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.INetworkUsage>
  IIterator_1__INetworkUsage = interface;
  PIIterator_1__INetworkUsage = ^IIterator_1__INetworkUsage;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.INetworkUsage>
  IIterable_1__INetworkUsage = interface;
  PIIterable_1__INetworkUsage = ^IIterable_1__INetworkUsage;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>
  IVectorView_1__INetworkUsage = interface;
  PIVectorView_1__INetworkUsage = ^IVectorView_1__INetworkUsage;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>>
  AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage = ^AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>>
  IAsyncOperation_1__IVectorView_1__INetworkUsage = interface;
  PIAsyncOperation_1__IVectorView_1__INetworkUsage = ^IAsyncOperation_1__IVectorView_1__INetworkUsage;

  // Windows.Networking.Connectivity.IConnectivityInterval
  IConnectivityInterval = interface;
  PIConnectivityInterval = ^IConnectivityInterval;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IIterator_1__IConnectivityInterval = interface;
  PIIterator_1__IConnectivityInterval = ^IIterator_1__IConnectivityInterval;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IIterable_1__IConnectivityInterval = interface;
  PIIterable_1__IConnectivityInterval = ^IIterable_1__IConnectivityInterval;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IVectorView_1__IConnectivityInterval = interface;
  PIVectorView_1__IConnectivityInterval = ^IVectorView_1__IConnectivityInterval;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval = ^AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>>
  IAsyncOperation_1__IVectorView_1__IConnectivityInterval = interface;
  PIAsyncOperation_1__IVectorView_1__IConnectivityInterval = ^IAsyncOperation_1__IVectorView_1__IConnectivityInterval;

  // Windows.Networking.Connectivity.IConnectionProfile2
  IConnectionProfile2 = interface;
  PIConnectionProfile2 = ^IConnectionProfile2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.ConnectionProfileDeleteStatus>
  AsyncOperationCompletedHandler_1__ConnectionProfileDeleteStatus = interface;
  PAsyncOperationCompletedHandler_1__ConnectionProfileDeleteStatus = ^AsyncOperationCompletedHandler_1__ConnectionProfileDeleteStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.ConnectionProfileDeleteStatus>
  IAsyncOperation_1__ConnectionProfileDeleteStatus = interface;
  PIAsyncOperation_1__ConnectionProfileDeleteStatus = ^IAsyncOperation_1__ConnectionProfileDeleteStatus;

  // Windows.Networking.Connectivity.IConnectionProfile5
  IConnectionProfile5 = interface;
  PIConnectionProfile5 = ^IConnectionProfile5;

  // Windows.Networking.Connectivity.IConnectionProfileFilter
  IConnectionProfileFilter = interface;
  PIConnectionProfileFilter = ^IConnectionProfileFilter;

  // Windows.Networking.Connectivity.IConnectionProfileFilter2
  IConnectionProfileFilter2 = interface;
  PIConnectionProfileFilter2 = ^IConnectionProfileFilter2;

  // Windows.Networking.Connectivity.IConnectionProfileFilter3
  IConnectionProfileFilter3 = interface;
  PIConnectionProfileFilter3 = ^IConnectionProfileFilter3;

  // Windows.Networking.Connectivity.IConnectionSession
  IConnectionSession = interface;
  PIConnectionSession = ^IConnectionSession;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IConnectionSession>
  AsyncOperationCompletedHandler_1__IConnectionSession = interface;
  PAsyncOperationCompletedHandler_1__IConnectionSession = ^AsyncOperationCompletedHandler_1__IConnectionSession;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IConnectionSession>
  IAsyncOperation_1__IConnectionSession = interface;
  PIAsyncOperation_1__IConnectionSession = ^IAsyncOperation_1__IConnectionSession;

  // Windows.Networking.Connectivity.IRoutePolicy
  IRoutePolicy = interface;
  PIRoutePolicy = ^IRoutePolicy;

  // Windows.Networking.Connectivity.IConnectivityManagerStatics
  IConnectivityManagerStatics = interface;
  PIConnectivityManagerStatics = ^IConnectivityManagerStatics;

  // Windows.Networking.Connectivity.ILanIdentifierData
  ILanIdentifierData = interface;
  PILanIdentifierData = ^ILanIdentifierData;

  // Windows.Networking.Connectivity.ILanIdentifier
  ILanIdentifier = interface;
  PILanIdentifier = ^ILanIdentifier;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.IConnectionProfile>
  IIterator_1__IConnectionProfile = interface;
  PIIterator_1__IConnectionProfile = ^IIterator_1__IConnectionProfile;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.IConnectionProfile>
  IIterable_1__IConnectionProfile = interface;
  PIIterable_1__IConnectionProfile = ^IIterable_1__IConnectionProfile;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.ILanIdentifier>
  IIterator_1__ILanIdentifier = interface;
  PIIterator_1__ILanIdentifier = ^IIterator_1__ILanIdentifier;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.ILanIdentifier>
  IIterable_1__ILanIdentifier = interface;
  PIIterable_1__ILanIdentifier = ^IIterable_1__ILanIdentifier;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.ILanIdentifier>
  IVectorView_1__ILanIdentifier = interface;
  PIVectorView_1__ILanIdentifier = ^IVectorView_1__ILanIdentifier;

  // Windows.Networking.Connectivity.IProxyConfiguration
  IProxyConfiguration = interface;
  PIProxyConfiguration = ^IProxyConfiguration;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IProxyConfiguration>
  AsyncOperationCompletedHandler_1__IProxyConfiguration = interface;
  PAsyncOperationCompletedHandler_1__IProxyConfiguration = ^AsyncOperationCompletedHandler_1__IProxyConfiguration;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IProxyConfiguration>
  IAsyncOperation_1__IProxyConfiguration = interface;
  PIAsyncOperation_1__IProxyConfiguration = ^IAsyncOperation_1__IProxyConfiguration;

  // Windows.Networking.Connectivity.NetworkStatusChangedEventHandler
  NetworkStatusChangedEventHandler = interface;
  PNetworkStatusChangedEventHandler = ^NetworkStatusChangedEventHandler;

  // Windows.Networking.Connectivity.INetworkInformationStatics
  INetworkInformationStatics = interface;
  PINetworkInformationStatics = ^INetworkInformationStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile = ^AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>>
  IAsyncOperation_1__IVectorView_1__IConnectionProfile = interface;
  PIAsyncOperation_1__IVectorView_1__IConnectionProfile = ^IAsyncOperation_1__IVectorView_1__IConnectionProfile;

  // Windows.Networking.Connectivity.INetworkInformationStatics2
  INetworkInformationStatics2 = interface;
  PINetworkInformationStatics2 = ^INetworkInformationStatics2;

  // Windows.Networking.Connectivity.INetworkStateChangeEventDetails
  INetworkStateChangeEventDetails = interface;
  PINetworkStateChangeEventDetails = ^INetworkStateChangeEventDetails;

  // Windows.Networking.Connectivity.INetworkStateChangeEventDetails2
  INetworkStateChangeEventDetails2 = interface;
  PINetworkStateChangeEventDetails2 = ^INetworkStateChangeEventDetails2;

  // Windows.Networking.Connectivity.IRoutePolicyFactory
  IRoutePolicyFactory = interface;
  PIRoutePolicyFactory = ^IRoutePolicyFactory;

  // Windows.Networking.Connectivity.IWwanConnectionProfileDetails2
  IWwanConnectionProfileDetails2 = interface;
  PIWwanConnectionProfileDetails2 = ^IWwanConnectionProfileDetails2;

  // Windows.Networking.Connectivity Enums

  // Windows.Networking.Connectivity.CellularApnAuthenticationType
  CellularApnAuthenticationType = (
    None = 0,
    Pap = 1,
    Chap = 2,
    Mschapv2 = 3
  );
  PCellularApnAuthenticationType = ^CellularApnAuthenticationType;

  // Windows.Networking.Connectivity.ConnectionProfileDeleteStatus
  ConnectionProfileDeleteStatus = (
    Success = 0,
    DeniedByUser = 1,
    DeniedBySystem = 2,
    UnknownError = 3
  );
  PConnectionProfileDeleteStatus = ^ConnectionProfileDeleteStatus;

  // Windows.Networking.Connectivity.DataUsageGranularity
  DataUsageGranularity = (
    PerMinute = 0,
    PerHour = 1,
    PerDay = 2,
    Total = 3
  );
  PDataUsageGranularity = ^DataUsageGranularity;

  // Windows.Networking.Connectivity.DomainConnectivityLevel
  DomainConnectivityLevel = (
    None = 0,
    Unauthenticated = 1,
    Authenticated = 2
  );
  PDomainConnectivityLevel = ^DomainConnectivityLevel;

  // Windows.Networking.Connectivity.TriStates
  TriStates = (
    DoNotCare = 0,
    No = 1,
    Yes = 2
  );
  PTriStates = ^TriStates;

  // Windows.Networking.Connectivity.WwanDataClass
  WwanDataClass = (
    None = 0,
    Gprs = 1,
    Edge = 2,
    Umts = 4,
    Hsdpa = 8,
    Hsupa = 16,
    LteAdvanced = 32,
    Cdma1xRtt = 65536,
    Cdma1xEvdo = 131072,
    Cdma1xEvdoRevA = 262144,
    Cdma1xEvdv = 524288,
    Cdma3xRtt = 1048576,
    Cdma1xEvdoRevB = 2097152,
    CdmaUmb = 4194304,
    Custom = -2147483648
  );
  PWwanDataClass = ^WwanDataClass;

  // Windows.Networking.Connectivity.WwanNetworkIPKind
  WwanNetworkIPKind = (
    None = 0,
    Ipv4 = 1,
    Ipv6 = 2,
    Ipv4v6 = 3,
    Ipv4v6v4Xlat = 4
  );
  PWwanNetworkIPKind = ^WwanNetworkIPKind;

  // Windows.Networking.Connectivity.WwanNetworkRegistrationState
  WwanNetworkRegistrationState = (
    None = 0,
    Deregistered = 1,
    Searching = 2,
    Home = 3,
    Roaming = 4,
    Partner = 5,
    Denied = 6
  );
  PWwanNetworkRegistrationState = ^WwanNetworkRegistrationState;

  // Windows.Networking.Connectivity Records
  // Windows.Networking.Connectivity.NetworkUsageStates
  NetworkUsageStates = record
    Roaming: TriStates;
    Shared: TriStates;
  end;
  PNetworkUsageStates = ^NetworkUsageStates;

  // Windows.Networking.Connectivity.WwanContract
  WwanContract = record
  end;
  PWwanContract = ^WwanContract;

  // Windows.Networking.Connectivity Interfaces

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.ICellularApnContext
  [WinRTClassNameAttribute(SWindows_Networking_Connectivity_CellularApnContext)]
  ICellularApnContext = interface(IInspectable)
  ['{6FA529F4-EFFD-4542-9AB2-705BBF94943A}']
    function get_ProviderId: HSTRING; safecall;
    procedure put_ProviderId(value: HSTRING); safecall;
    function get_AccessPointName: HSTRING; safecall;
    procedure put_AccessPointName(value: HSTRING); safecall;
    function get_UserName: HSTRING; safecall;
    procedure put_UserName(value: HSTRING); safecall;
    function get_Password: HSTRING; safecall;
    procedure put_Password(value: HSTRING); safecall;
    function get_IsCompressionEnabled: Boolean; safecall;
    procedure put_IsCompressionEnabled(value: Boolean); safecall;
    function get_AuthenticationType: CellularApnAuthenticationType; safecall;
    procedure put_AuthenticationType(value: CellularApnAuthenticationType); safecall;
    property AccessPointName: HSTRING read get_AccessPointName write put_AccessPointName;
    property AuthenticationType: CellularApnAuthenticationType read get_AuthenticationType write put_AuthenticationType;
    property IsCompressionEnabled: Boolean read get_IsCompressionEnabled write put_IsCompressionEnabled;
    property Password: HSTRING read get_Password write put_Password;
    property ProviderId: HSTRING read get_ProviderId write put_ProviderId;
    property UserName: HSTRING read get_UserName write put_UserName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.ICellularApnContext2
  ICellularApnContext2 = interface(IInspectable)
  ['{76B0EB1A-AC49-4350-B1E5-DC4763BC69C7}']
    function get_ProfileName: HSTRING; safecall;
    procedure put_ProfileName(value: HSTRING); safecall;
    property ProfileName: HSTRING read get_ProfileName write put_ProfileName;
  end;

  // Windows.Networking.Connectivity.IConnectionCost2
  IConnectionCost2 = interface(IInspectable)
  ['{8E113A05-E209-4549-BB25-5E0DB691CB05}']
    function get_BackgroundDataUsageRestricted: Boolean; safecall;
    property BackgroundDataUsageRestricted: Boolean read get_BackgroundDataUsageRestricted;
  end;

  // Windows.Networking.Connectivity.IWwanConnectionProfileDetails
  IWwanConnectionProfileDetails = interface(IInspectable)
  ['{0E4DA8FE-835F-4DF3-82FD-DF556EBC09EF}']
    function get_HomeProviderId: HSTRING; safecall;
    function get_AccessPointName: HSTRING; safecall;
    function GetNetworkRegistrationState: WwanNetworkRegistrationState; safecall;
    function GetCurrentDataClass: WwanDataClass; safecall;
    property AccessPointName: HSTRING read get_AccessPointName;
    property HomeProviderId: HSTRING read get_HomeProviderId;
  end;

  // Windows.Networking.Connectivity.IWlanConnectionProfileDetails
  IWlanConnectionProfileDetails = interface(IInspectable)
  ['{562098CB-B35A-4BF1-A884-B7557E88FF86}']
    function GetConnectedSsid: HSTRING; safecall;
  end;

  // Windows.Networking.Connectivity.INetworkUsage
  INetworkUsage = interface(IInspectable)
  ['{49DA8FCE-9985-4927-BF5B-072B5C65F8D9}']
    function get_BytesSent: UInt64; safecall;
    function get_BytesReceived: UInt64; safecall;
    function get_ConnectionDuration: TimeSpan; safecall;
    property BytesReceived: UInt64 read get_BytesReceived;
    property BytesSent: UInt64 read get_BytesSent;
    property ConnectionDuration: TimeSpan read get_ConnectionDuration;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.INetworkUsage>
  IIterator_1__INetworkUsage_Base = interface(IInspectable)
  ['{5FAFB57B-9C82-50A1-9970-69F9CB069695}']
    function get_Current: INetworkUsage; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PINetworkUsage): Cardinal; safecall;
    property Current: INetworkUsage read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.INetworkUsage>
  IIterator_1__INetworkUsage = interface(IIterator_1__INetworkUsage_Base)
  ['{932F485D-2603-5593-BAED-C56FEE5A0EE1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.INetworkUsage>
  IIterable_1__INetworkUsage_Base = interface(IInspectable)
  ['{DD2656B1-8360-5772-B272-C47F7F0FC7A6}']
    function First: IIterator_1__INetworkUsage; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.INetworkUsage>
  IIterable_1__INetworkUsage = interface(IIterable_1__INetworkUsage_Base)
  ['{F45D00D7-BCA3-5892-916A-7B9EB87C4FCB}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>
  IVectorView_1__INetworkUsage = interface(IInspectable)
  ['{081441AB-4EA5-5F39-9A7D-3F9124CD22A0}']
    function GetAt(index: Cardinal): INetworkUsage; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: INetworkUsage; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PINetworkUsage): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>>
  AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage_Delegate_Base = interface(IUnknown)
  ['{E31D7E7E-4173-5C71-B04B-A09658002590}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__INetworkUsage; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>>
  AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage = interface(AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage_Delegate_Base)
  ['{34C7CFF2-F4A8-5936-AD75-041DA70D4BBA}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>>
  IAsyncOperation_1__IVectorView_1__INetworkUsage_Base = interface(IInspectable)
  ['{05C9E081-6229-5049-8EEA-A498407C00D5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage; safecall;
    function GetResults: IVectorView_1__INetworkUsage; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__INetworkUsage read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.INetworkUsage>>
  IAsyncOperation_1__IVectorView_1__INetworkUsage = interface(IAsyncOperation_1__IVectorView_1__INetworkUsage_Base)
  ['{F5F7F8A3-E501-5378-A56C-21A4BF9F2118}']
  end;

  // Windows.Networking.Connectivity.IConnectivityInterval
  IConnectivityInterval = interface(IInspectable)
  ['{4FAA3FFF-6746-4824-A964-EED8E87F8709}']
    function get_StartTime: DateTime; safecall;
    function get_ConnectionDuration: TimeSpan; safecall;
    property ConnectionDuration: TimeSpan read get_ConnectionDuration;
    property StartTime: DateTime read get_StartTime;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IIterator_1__IConnectivityInterval_Base = interface(IInspectable)
  ['{741CEA48-651C-5FD9-931E-4F91B521E182}']
    function get_Current: IConnectivityInterval; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIConnectivityInterval): Cardinal; safecall;
    property Current: IConnectivityInterval read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IIterator_1__IConnectivityInterval = interface(IIterator_1__IConnectivityInterval_Base)
  ['{1D8205F6-A78E-533F-9450-1BD5B39E6A63}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IIterable_1__IConnectivityInterval_Base = interface(IInspectable)
  ['{58051A8B-B259-5414-9B9A-CAA0789E833E}']
    function First: IIterator_1__IConnectivityInterval; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IIterable_1__IConnectivityInterval = interface(IIterable_1__IConnectivityInterval_Base)
  ['{CCFAFDA3-D747-52F7-8D78-538C34753194}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>
  IVectorView_1__IConnectivityInterval = interface(IInspectable)
  ['{33332F56-9B64-569F-BA8C-040BA0E8B249}']
    function GetAt(index: Cardinal): IConnectivityInterval; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IConnectivityInterval; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIConnectivityInterval): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval_Delegate_Base = interface(IUnknown)
  ['{B475014C-95F1-5310-B5D1-C2309D944440}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IConnectivityInterval; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval_Delegate_Base)
  ['{85A0B45C-52E3-58C8-81DC-4B1B5335FF42}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>>
  IAsyncOperation_1__IVectorView_1__IConnectivityInterval_Base = interface(IInspectable)
  ['{AF96D70B-41C7-5DC6-9895-EA043A885D8D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval; safecall;
    function GetResults: IVectorView_1__IConnectivityInterval; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IConnectivityInterval read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectivityInterval>>
  IAsyncOperation_1__IVectorView_1__IConnectivityInterval = interface(IAsyncOperation_1__IVectorView_1__IConnectivityInterval_Base)
  ['{2156BF20-8540-5CAB-BFE6-2BC40D70271D}']
  end;

  // Windows.Networking.Connectivity.IConnectionProfile2
  IConnectionProfile2 = interface(IInspectable)
  ['{E2045145-4C9F-400C-9150-7EC7D6E2888A}']
    function get_IsWwanConnectionProfile: Boolean; safecall;
    function get_IsWlanConnectionProfile: Boolean; safecall;
    function get_WwanConnectionProfileDetails: IWwanConnectionProfileDetails; safecall;
    function get_WlanConnectionProfileDetails: IWlanConnectionProfileDetails; safecall;
    function get_ServiceProviderGuid: IReference_1__TGuid; safecall;
    function GetSignalBars: IReference_1__Byte; safecall;
    function GetDomainConnectivityLevel: DomainConnectivityLevel; safecall;
    function GetNetworkUsageAsync(startTime: DateTime; endTime: DateTime; granularity: DataUsageGranularity; states: NetworkUsageStates): IAsyncOperation_1__IVectorView_1__INetworkUsage; safecall;
    function GetConnectivityIntervalsAsync(startTime: DateTime; endTime: DateTime; states: NetworkUsageStates): IAsyncOperation_1__IVectorView_1__IConnectivityInterval; safecall;
    property IsWlanConnectionProfile: Boolean read get_IsWlanConnectionProfile;
    property IsWwanConnectionProfile: Boolean read get_IsWwanConnectionProfile;
    property ServiceProviderGuid: IReference_1__TGuid read get_ServiceProviderGuid;
    property WlanConnectionProfileDetails: IWlanConnectionProfileDetails read get_WlanConnectionProfileDetails;
    property WwanConnectionProfileDetails: IWwanConnectionProfileDetails read get_WwanConnectionProfileDetails;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.ConnectionProfileDeleteStatus>
  AsyncOperationCompletedHandler_1__ConnectionProfileDeleteStatus = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ConnectionProfileDeleteStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.ConnectionProfileDeleteStatus>
  IAsyncOperation_1__ConnectionProfileDeleteStatus = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ConnectionProfileDeleteStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ConnectionProfileDeleteStatus; safecall;
    function GetResults: ConnectionProfileDeleteStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ConnectionProfileDeleteStatus read get_Completed write put_Completed;
  end;

  // Windows.Networking.Connectivity.IConnectionProfile5
  IConnectionProfile5 = interface(IInspectable)
  ['{85361EC7-9C73-4BE0-8F14-578EEC71EE0E}']
    function get_CanDelete: Boolean; safecall;
    function TryDeleteAsync: IAsyncOperation_1__ConnectionProfileDeleteStatus; safecall;
    property CanDelete: Boolean read get_CanDelete;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.IConnectionProfileFilter
  [WinRTClassNameAttribute(SWindows_Networking_Connectivity_ConnectionProfileFilter)]
  IConnectionProfileFilter = interface(IInspectable)
  ['{204C7CC8-BD2D-4E8D-A4B3-455EC337388A}']
    procedure put_IsConnected(value: Boolean); safecall;
    function get_IsConnected: Boolean; safecall;
    procedure put_IsWwanConnectionProfile(value: Boolean); safecall;
    function get_IsWwanConnectionProfile: Boolean; safecall;
    procedure put_IsWlanConnectionProfile(value: Boolean); safecall;
    function get_IsWlanConnectionProfile: Boolean; safecall;
    procedure put_NetworkCostType(value: NetworkCostType); safecall;
    function get_NetworkCostType: NetworkCostType; safecall;
    procedure put_ServiceProviderGuid(value: IReference_1__TGuid); safecall;
    function get_ServiceProviderGuid: IReference_1__TGuid; safecall;
    property IsConnected: Boolean read get_IsConnected write put_IsConnected;
    property IsWlanConnectionProfile: Boolean read get_IsWlanConnectionProfile write put_IsWlanConnectionProfile;
    property IsWwanConnectionProfile: Boolean read get_IsWwanConnectionProfile write put_IsWwanConnectionProfile;
    property NetworkCostType_: NetworkCostType read get_NetworkCostType write put_NetworkCostType;
    property ServiceProviderGuid: IReference_1__TGuid read get_ServiceProviderGuid write put_ServiceProviderGuid;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.IConnectionProfileFilter2
  IConnectionProfileFilter2 = interface(IInspectable)
  ['{CD068EE1-C3FC-4FAD-9DDC-593FAA4B7885}']
    procedure put_IsRoaming(value: IReference_1__Boolean); safecall;
    function get_IsRoaming: IReference_1__Boolean; safecall;
    procedure put_IsOverDataLimit(value: IReference_1__Boolean); safecall;
    function get_IsOverDataLimit: IReference_1__Boolean; safecall;
    procedure put_IsBackgroundDataUsageRestricted(value: IReference_1__Boolean); safecall;
    function get_IsBackgroundDataUsageRestricted: IReference_1__Boolean; safecall;
    function get_RawData: IBuffer; safecall;
    property IsBackgroundDataUsageRestricted: IReference_1__Boolean read get_IsBackgroundDataUsageRestricted write put_IsBackgroundDataUsageRestricted;
    property IsOverDataLimit: IReference_1__Boolean read get_IsOverDataLimit write put_IsOverDataLimit;
    property IsRoaming: IReference_1__Boolean read get_IsRoaming write put_IsRoaming;
    property RawData: IBuffer read get_RawData;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.IConnectionProfileFilter3
  IConnectionProfileFilter3 = interface(IInspectable)
  ['{0AAA09C0-5014-447C-8809-AEE4CB0AF94A}']
    procedure put_PurposeGuid(value: IReference_1__TGuid); safecall;
    function get_PurposeGuid: IReference_1__TGuid; safecall;
    property PurposeGuid: IReference_1__TGuid read get_PurposeGuid write put_PurposeGuid;
  end;

  // UsedAPI Interface
  // Windows.Networking.Connectivity.IConnectionSession
  IConnectionSession = interface(IInspectable)
  ['{FF905D4C-F83B-41B0-8A0C-1462D9C56B73}']
    function get_ConnectionProfile: IConnectionProfile; safecall;
    property ConnectionProfile: IConnectionProfile read get_ConnectionProfile;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IConnectionSession>
  AsyncOperationCompletedHandler_1__IConnectionSession_Delegate_Base = interface(IUnknown)
  ['{3BC680D8-9E83-5086-8F49-7A29BFB1C7E1}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IConnectionSession; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IConnectionSession>
  AsyncOperationCompletedHandler_1__IConnectionSession = interface(AsyncOperationCompletedHandler_1__IConnectionSession_Delegate_Base)
  ['{D1813A09-BE5C-5845-A642-627024C4F8D4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IConnectionSession>
  IAsyncOperation_1__IConnectionSession_Base = interface(IInspectable)
  ['{94FC6211-4702-5D24-81BF-170CA7818995}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IConnectionSession); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IConnectionSession; safecall;
    function GetResults: IConnectionSession; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IConnectionSession read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IConnectionSession>
  IAsyncOperation_1__IConnectionSession = interface(IAsyncOperation_1__IConnectionSession_Base)
  ['{31FC98D3-FD1F-5E31-BBAA-11B2B9707382}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.IRoutePolicy
  [WinRTClassNameAttribute(SWindows_Networking_Connectivity_RoutePolicy)]
  IRoutePolicy = interface(IInspectable)
  ['{11ABC4AC-0FC7-42E4-8742-569923B1CA11}']
    function get_ConnectionProfile: IConnectionProfile; safecall;
    function get_HostName: IHostName; safecall;
    function get_HostNameType: DomainNameType; safecall;
    property ConnectionProfile: IConnectionProfile read get_ConnectionProfile;
    property HostName: IHostName read get_HostName;
    property HostNameType: DomainNameType read get_HostNameType;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.IConnectivityManagerStatics
  [WinRTClassNameAttribute(SWindows_Networking_Connectivity_ConnectivityManager)]
  IConnectivityManagerStatics = interface(IInspectable)
  ['{5120D4B1-4FB1-48B0-AFC9-42E0092A8164}']
    function AcquireConnectionAsync(cellularApnContext: ICellularApnContext): IAsyncOperation_1__IConnectionSession; safecall;
    procedure AddHttpRoutePolicy(routePolicy: IRoutePolicy); safecall;
    procedure RemoveHttpRoutePolicy(routePolicy: IRoutePolicy); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.Connectivity.ILanIdentifierData
  ILanIdentifierData = interface(IInspectable)
  ['{A74E83C3-D639-45BE-A36A-C4E4AEAF6D9B}']
    function get_Type: Cardinal; safecall;
    function get_Value: IVectorView_1__Byte; safecall;
    property &Type: Cardinal read get_Type;
    property Value: IVectorView_1__Byte read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Networking.Connectivity.ILanIdentifier
  ILanIdentifier = interface(IInspectable)
  ['{48AA53AA-1108-4546-A6CB-9A74DA4B7BA0}']
    function get_InfrastructureId: ILanIdentifierData; safecall;
    function get_PortId: ILanIdentifierData; safecall;
    function get_NetworkAdapterId: TGuid; safecall;
    property InfrastructureId: ILanIdentifierData read get_InfrastructureId;
    property NetworkAdapterId: TGuid read get_NetworkAdapterId;
    property PortId: ILanIdentifierData read get_PortId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.IConnectionProfile>
  IIterator_1__IConnectionProfile_Base = interface(IInspectable)
  ['{89913732-A08B-5CB2-AF16-BBBB2223839E}']
    function get_Current: IConnectionProfile; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIConnectionProfile): Cardinal; safecall;
    property Current: IConnectionProfile read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.IConnectionProfile>
  IIterator_1__IConnectionProfile = interface(IIterator_1__IConnectionProfile_Base)
  ['{ECF00CAE-BF0D-5D30-89EF-A3403CB22EAC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.IConnectionProfile>
  IIterable_1__IConnectionProfile_Base = interface(IInspectable)
  ['{34DABEF9-87D0-5B1C-A7AC-9D290ADEB0C8}']
    function First: IIterator_1__IConnectionProfile; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.IConnectionProfile>
  IIterable_1__IConnectionProfile = interface(IIterable_1__IConnectionProfile_Base)
  ['{8BC9CBFB-ADC3-5C9A-902E-CA4F4958113E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.ILanIdentifier>
  IIterator_1__ILanIdentifier_Base = interface(IInspectable)
  ['{2C5D2F7E-CE9C-5253-A0F4-01E5BDC11988}']
    function get_Current: ILanIdentifier; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PILanIdentifier): Cardinal; safecall;
    property Current: ILanIdentifier read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.Connectivity.ILanIdentifier>
  IIterator_1__ILanIdentifier = interface(IIterator_1__ILanIdentifier_Base)
  ['{E50F2506-05FF-5248-B0BF-998C91B0C9A6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.ILanIdentifier>
  IIterable_1__ILanIdentifier_Base = interface(IInspectable)
  ['{ACCEF3CD-5D92-5C01-8AC4-79FE74CD733E}']
    function First: IIterator_1__ILanIdentifier; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.Connectivity.ILanIdentifier>
  IIterable_1__ILanIdentifier = interface(IIterable_1__ILanIdentifier_Base)
  ['{81266A25-E204-54FF-8B8E-3B30DA27BC35}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.ILanIdentifier>
  IVectorView_1__ILanIdentifier = interface(IInspectable)
  ['{43442CEB-9A1C-5799-9C42-8A7FBB36ADB7}']
    function GetAt(index: Cardinal): ILanIdentifier; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ILanIdentifier; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PILanIdentifier): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Networking.Connectivity.IProxyConfiguration
  IProxyConfiguration = interface(IInspectable)
  ['{EF3A60B4-9004-4DD6-B7D8-B3E502F4AAD0}']
    function get_ProxyUris: IVectorView_1__IUriRuntimeClass; safecall;
    function get_CanConnectDirectly: Boolean; safecall;
    property CanConnectDirectly: Boolean read get_CanConnectDirectly;
    property ProxyUris: IVectorView_1__IUriRuntimeClass read get_ProxyUris;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IProxyConfiguration>
  AsyncOperationCompletedHandler_1__IProxyConfiguration_Delegate_Base = interface(IUnknown)
  ['{035B2567-EFB9-5BC3-B609-F9A8C20B7001}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IProxyConfiguration; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IProxyConfiguration>
  AsyncOperationCompletedHandler_1__IProxyConfiguration = interface(AsyncOperationCompletedHandler_1__IProxyConfiguration_Delegate_Base)
  ['{6EB17802-BF19-5CEB-9A91-5D78B23E0D8F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IProxyConfiguration>
  IAsyncOperation_1__IProxyConfiguration_Base = interface(IInspectable)
  ['{1E7651F6-6562-59C7-9AF3-8756636EEEE2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IProxyConfiguration); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IProxyConfiguration; safecall;
    function GetResults: IProxyConfiguration; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IProxyConfiguration read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IProxyConfiguration>
  IAsyncOperation_1__IProxyConfiguration = interface(IAsyncOperation_1__IProxyConfiguration_Base)
  ['{A945C381-57EE-5A60-BC54-8A3729E42737}']
  end;

  // UsedAPI Interface
  // Windows.Networking.Connectivity.NetworkStatusChangedEventHandler
  NetworkStatusChangedEventHandler = interface(IUnknown)
  ['{71BA143F-598E-49D0-84EB-8FEBAEDCC195}']
    procedure Invoke(sender: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.INetworkInformationStatics
  [WinRTClassNameAttribute(SWindows_Networking_Connectivity_NetworkInformation)]
  INetworkInformationStatics = interface(IInspectable)
  ['{5074F851-950D-4165-9C15-365619481EEA}']
    function GetConnectionProfiles: IVectorView_1__IConnectionProfile; safecall;
    function GetInternetConnectionProfile: IConnectionProfile; safecall;
    function GetLanIdentifiers: IVectorView_1__ILanIdentifier; safecall;
    function GetHostNames: IVectorView_1__IHostName; safecall;
    function GetProxyConfigurationAsync(uri: IUriRuntimeClass): IAsyncOperation_1__IProxyConfiguration; safecall;
    function GetSortedEndpointPairs(destinationList: IIterable_1__IEndpointPair; sortOptions: HostNameSortOptions): IVectorView_1__IEndpointPair; safecall;
    function add_NetworkStatusChanged(networkStatusHandler: NetworkStatusChangedEventHandler): EventRegistrationToken; safecall;
    procedure remove_NetworkStatusChanged(eventCookie: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile_Delegate_Base = interface(IUnknown)
  ['{C523D9DD-4EA6-5115-80E9-4E7AD4769798}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IConnectionProfile; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile_Delegate_Base)
  ['{D7DB932C-AF75-5E4A-9689-0320303401DF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>>
  IAsyncOperation_1__IVectorView_1__IConnectionProfile_Base = interface(IInspectable)
  ['{C0023294-C2CB-52F0-A9F4-21916032F69D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile; safecall;
    function GetResults: IVectorView_1__IConnectionProfile; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IConnectionProfile read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>>
  IAsyncOperation_1__IVectorView_1__IConnectionProfile = interface(IAsyncOperation_1__IVectorView_1__IConnectionProfile_Base)
  ['{650097D7-D5A3-559F-B864-B5E1C7576775}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.INetworkInformationStatics2
  [WinRTClassNameAttribute(SWindows_Networking_Connectivity_NetworkInformation)]
  INetworkInformationStatics2 = interface(IInspectable)
  ['{459CED14-2832-49B6-BA6E-E265F04786A8}']
    function FindConnectionProfilesAsync(pProfileFilter: IConnectionProfileFilter): IAsyncOperation_1__IVectorView_1__IConnectionProfile; safecall;
  end;

  // Windows.Networking.Connectivity.INetworkStateChangeEventDetails
  INetworkStateChangeEventDetails = interface(IInspectable)
  ['{1F0CF333-D7A6-44DD-A4E9-687C476B903D}']
    function get_HasNewInternetConnectionProfile: Boolean; safecall;
    function get_HasNewConnectionCost: Boolean; safecall;
    function get_HasNewNetworkConnectivityLevel: Boolean; safecall;
    function get_HasNewDomainConnectivityLevel: Boolean; safecall;
    function get_HasNewHostNameList: Boolean; safecall;
    function get_HasNewWwanRegistrationState: Boolean; safecall;
    property HasNewConnectionCost: Boolean read get_HasNewConnectionCost;
    property HasNewDomainConnectivityLevel: Boolean read get_HasNewDomainConnectivityLevel;
    property HasNewHostNameList: Boolean read get_HasNewHostNameList;
    property HasNewInternetConnectionProfile: Boolean read get_HasNewInternetConnectionProfile;
    property HasNewNetworkConnectivityLevel: Boolean read get_HasNewNetworkConnectivityLevel;
    property HasNewWwanRegistrationState: Boolean read get_HasNewWwanRegistrationState;
  end;

  // Windows.Networking.Connectivity.INetworkStateChangeEventDetails2
  INetworkStateChangeEventDetails2 = interface(IInspectable)
  ['{D643C0E8-30D3-4F6A-AD47-6A1873CEB3C1}']
    function get_HasNewTetheringOperationalState: Boolean; safecall;
    function get_HasNewTetheringClientCount: Boolean; safecall;
    property HasNewTetheringClientCount: Boolean read get_HasNewTetheringClientCount;
    property HasNewTetheringOperationalState: Boolean read get_HasNewTetheringOperationalState;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.Connectivity.IRoutePolicyFactory
  [WinRTClassNameAttribute(SWindows_Networking_Connectivity_RoutePolicy)]
  IRoutePolicyFactory = interface(IInspectable)
  ['{36027933-A18E-4DB5-A697-F58FA7364E44}']
    function CreateRoutePolicy(connectionProfile: IConnectionProfile; hostName: IHostName; &type: DomainNameType): IRoutePolicy; safecall;
  end;

  // Windows.Networking.Connectivity.IWwanConnectionProfileDetails2
  IWwanConnectionProfileDetails2 = interface(IInspectable)
  ['{7A754EDE-A1ED-48B2-8E92-B460033D52E2}']
    function get_IPKind: WwanNetworkIPKind; safecall;
    function get_PurposeGuids: IVectorView_1__TGuid; safecall;
    property IPKind: WwanNetworkIPKind read get_IPKind;
    property PurposeGuids: IVectorView_1__TGuid read get_PurposeGuids;
  end;

  // Windows.Networking.Connectivity.CellularApnContext
  // DualAPI
  // Implements: Windows.Networking.Connectivity.ICellularApnContext
  // Implements: Windows.Networking.Connectivity.ICellularApnContext2
  // Instantiable: "ICellularApnContext"
  TCellularApnContext = class(TWinRTGenericImportI<ICellularApnContext>) end;

  // Windows.Networking.Connectivity.ConnectionProfileFilter
  // DualAPI
  // Implements: Windows.Networking.Connectivity.IConnectionProfileFilter
  // Implements: Windows.Networking.Connectivity.IConnectionProfileFilter2
  // Implements: Windows.Networking.Connectivity.IConnectionProfileFilter3
  // Instantiable: "IConnectionProfileFilter"
  TConnectionProfileFilter = class(TWinRTGenericImportI<IConnectionProfileFilter>) end;

  // Windows.Networking.Connectivity.ConnectivityManager
  // DualAPI
  // Statics: "Windows.Networking.Connectivity.IConnectivityManagerStatics"
  TConnectivityManager = class(TWinRTGenericImportS<IConnectivityManagerStatics>)
  public
    // -> IConnectivityManagerStatics
    class function AcquireConnectionAsync(cellularApnContext: ICellularApnContext): IAsyncOperation_1__IConnectionSession; static; inline;
    class procedure AddHttpRoutePolicy(routePolicy: IRoutePolicy); static; inline;
    class procedure RemoveHttpRoutePolicy(routePolicy: IRoutePolicy); static; inline;
  end;

  // Windows.Networking.Connectivity.NetworkInformation
  // DualAPI
  // Statics: "Windows.Networking.Connectivity.INetworkInformationStatics"
  // Statics: "Windows.Networking.Connectivity.INetworkInformationStatics2"
  TNetworkInformation = class(TWinRTGenericImportS2<INetworkInformationStatics, INetworkInformationStatics2>)
  public
    // -> INetworkInformationStatics
    class function GetConnectionProfiles: IVectorView_1__IConnectionProfile; static; inline;
    class function GetInternetConnectionProfile: IConnectionProfile; static; inline;
    class function GetLanIdentifiers: IVectorView_1__ILanIdentifier; static; inline;
    class function GetHostNames: IVectorView_1__IHostName; static; inline;
    class function GetProxyConfigurationAsync(uri: IUriRuntimeClass): IAsyncOperation_1__IProxyConfiguration; static; inline;
    class function GetSortedEndpointPairs(destinationList: IIterable_1__IEndpointPair; sortOptions: HostNameSortOptions): IVectorView_1__IEndpointPair; static; inline;
    class function add_NetworkStatusChanged(networkStatusHandler: NetworkStatusChangedEventHandler): EventRegistrationToken; static; inline;
    class procedure remove_NetworkStatusChanged(eventCookie: EventRegistrationToken); static; inline;

    // -> INetworkInformationStatics2
    class function FindConnectionProfilesAsync(pProfileFilter: IConnectionProfileFilter): IAsyncOperation_1__IVectorView_1__IConnectionProfile; static; inline;
  end;

  // Windows.Networking.Connectivity.RoutePolicy
  // DualAPI
  // Implements: Windows.Networking.Connectivity.IRoutePolicy
  // Factory: "Windows.Networking.Connectivity.IRoutePolicyFactory"
  TRoutePolicy = class(TWinRTGenericImportF<IRoutePolicyFactory>)
  public
    // -> IRoutePolicyFactory
    class function CreateRoutePolicy(connectionProfile: IConnectionProfile; hostName: IHostName; &type: DomainNameType): IRoutePolicy; static; inline;
  end;

implementation

{ TCellularApnContext }

{ TConnectionProfileFilter }

{ TConnectivityManager }

class function TConnectivityManager.AcquireConnectionAsync(cellularApnContext: ICellularApnContext): IAsyncOperation_1__IConnectionSession;
begin
  Result := Statics.AcquireConnectionAsync(cellularApnContext);
end;

class procedure TConnectivityManager.AddHttpRoutePolicy(routePolicy: IRoutePolicy);
begin
  Statics.AddHttpRoutePolicy(routePolicy);
end;

class procedure TConnectivityManager.RemoveHttpRoutePolicy(routePolicy: IRoutePolicy);
begin
  Statics.RemoveHttpRoutePolicy(routePolicy);
end;


{ TNetworkInformation }

class function TNetworkInformation.GetConnectionProfiles: IVectorView_1__IConnectionProfile;
begin
  Result := Statics.GetConnectionProfiles;
end;

class function TNetworkInformation.GetInternetConnectionProfile: IConnectionProfile;
begin
  Result := Statics.GetInternetConnectionProfile;
end;

class function TNetworkInformation.GetLanIdentifiers: IVectorView_1__ILanIdentifier;
begin
  Result := Statics.GetLanIdentifiers;
end;

class function TNetworkInformation.GetHostNames: IVectorView_1__IHostName;
begin
  Result := Statics.GetHostNames;
end;

class function TNetworkInformation.GetProxyConfigurationAsync(uri: IUriRuntimeClass): IAsyncOperation_1__IProxyConfiguration;
begin
  Result := Statics.GetProxyConfigurationAsync(uri);
end;

class function TNetworkInformation.GetSortedEndpointPairs(destinationList: IIterable_1__IEndpointPair; sortOptions: HostNameSortOptions): IVectorView_1__IEndpointPair;
begin
  Result := Statics.GetSortedEndpointPairs(destinationList, sortOptions);
end;

class function TNetworkInformation.add_NetworkStatusChanged(networkStatusHandler: NetworkStatusChangedEventHandler): EventRegistrationToken;
begin
  Result := Statics.add_NetworkStatusChanged(networkStatusHandler);
end;

class procedure TNetworkInformation.remove_NetworkStatusChanged(eventCookie: EventRegistrationToken);
begin
  Statics.remove_NetworkStatusChanged(eventCookie);
end;


class function TNetworkInformation.FindConnectionProfilesAsync(pProfileFilter: IConnectionProfileFilter): IAsyncOperation_1__IVectorView_1__IConnectionProfile;
begin
  Result := Statics2.FindConnectionProfilesAsync(pProfileFilter);
end;


{ TRoutePolicy }
// Factories for : "RoutePolicy"
// Factory: "Windows.Networking.Connectivity.IRoutePolicyFactory"
// -> IRoutePolicyFactory

class function TRoutePolicy.CreateRoutePolicy(connectionProfile: IConnectionProfile; hostName: IHostName; &type: DomainNameType): IRoutePolicy;
begin
  Result := Factory.CreateRoutePolicy(connectionProfile, hostName, &type);
end;


end.
