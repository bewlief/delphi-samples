{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Devices.Enumeration;

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

  AsyncOperationCompletedHandler_1__IDeviceInformation_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IDeviceInformation_Delegate_Base;
  AsyncOperationCompletedHandler_1__IDeviceInformation = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IDeviceInformation;
  PAsyncOperationCompletedHandler_1__IDeviceInformation = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IDeviceInformation;
  DeviceClass = Winapi.CommonTypes.DeviceClass;
  PDeviceClass = Winapi.CommonTypes.PDeviceClass;
  DevicePickerDisplayStatusOptions = Winapi.CommonTypes.DevicePickerDisplayStatusOptions;
  PDevicePickerDisplayStatusOptions = Winapi.CommonTypes.PDevicePickerDisplayStatusOptions;
  DeviceWatcherStatus = Winapi.CommonTypes.DeviceWatcherStatus;
  PDeviceWatcherStatus = Winapi.CommonTypes.PDeviceWatcherStatus;
  IAsyncOperation_1__IDeviceInformation_Base = Winapi.CommonTypes.IAsyncOperation_1__IDeviceInformation_Base;
  IAsyncOperation_1__IDeviceInformation = Winapi.CommonTypes.IAsyncOperation_1__IDeviceInformation;
  PIAsyncOperation_1__IDeviceInformation = Winapi.CommonTypes.PIAsyncOperation_1__IDeviceInformation;
  IDeviceDisconnectButtonClickedEventArgs = Winapi.CommonTypes.IDeviceDisconnectButtonClickedEventArgs;
  PIDeviceDisconnectButtonClickedEventArgs = Winapi.CommonTypes.PIDeviceDisconnectButtonClickedEventArgs;
  IDeviceInformation = Winapi.CommonTypes.IDeviceInformation;
  PIDeviceInformation = Winapi.CommonTypes.PIDeviceInformation;
  IDeviceInformationUpdate = Winapi.CommonTypes.IDeviceInformationUpdate;
  PIDeviceInformationUpdate = Winapi.CommonTypes.PIDeviceInformationUpdate;
  IDevicePicker = Winapi.CommonTypes.IDevicePicker;
  PIDevicePicker = Winapi.CommonTypes.PIDevicePicker;
  IDevicePickerAppearance = Winapi.CommonTypes.IDevicePickerAppearance;
  PIDevicePickerAppearance = Winapi.CommonTypes.PIDevicePickerAppearance;
  IDevicePickerFilter = Winapi.CommonTypes.IDevicePickerFilter;
  PIDevicePickerFilter = Winapi.CommonTypes.PIDevicePickerFilter;
  IDeviceSelectedEventArgs = Winapi.CommonTypes.IDeviceSelectedEventArgs;
  PIDeviceSelectedEventArgs = Winapi.CommonTypes.PIDeviceSelectedEventArgs;
  IDeviceWatcher = Winapi.CommonTypes.IDeviceWatcher;
  PIDeviceWatcher = Winapi.CommonTypes.PIDeviceWatcher;
  IEnclosureLocation = Winapi.CommonTypes.IEnclosureLocation;
  PIEnclosureLocation = Winapi.CommonTypes.PIEnclosureLocation;
  IVector_1__DeviceClass_Base = Winapi.CommonTypes.IVector_1__DeviceClass_Base;
  IVector_1__DeviceClass = Winapi.CommonTypes.IVector_1__DeviceClass;
  PIVector_1__DeviceClass = Winapi.CommonTypes.PIVector_1__DeviceClass;
  IVectorView_1__DeviceClass = Winapi.CommonTypes.IVectorView_1__DeviceClass;
  PIVectorView_1__DeviceClass = Winapi.CommonTypes.PIVectorView_1__DeviceClass;
  Panel = Winapi.CommonTypes.Panel;
  PPanel = Winapi.CommonTypes.PPanel;
  Pnp_IPnpObject = Winapi.CommonTypes.Pnp_IPnpObject;
  PPnp_IPnpObject = Winapi.CommonTypes.PPnp_IPnpObject;
  Pnp_IPnpObjectUpdate = Winapi.CommonTypes.Pnp_IPnpObjectUpdate;
  PPnp_IPnpObjectUpdate = Winapi.CommonTypes.PPnp_IPnpObjectUpdate;
  Pnp_IPnpObjectWatcher = Winapi.CommonTypes.Pnp_IPnpObjectWatcher;
  PPnp_IPnpObjectWatcher = Winapi.CommonTypes.PPnp_IPnpObjectWatcher;
  Pnp_PnpObjectType = Winapi.CommonTypes.Pnp_PnpObjectType;
  PPnp_PnpObjectType = Winapi.CommonTypes.PPnp_PnpObjectType;
  TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs_Delegate_Base;
  TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs;
  PTypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs;
  TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs_Delegate_Base;
  TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs;
  PTypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs;
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformation_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDeviceWatcher__IDeviceInformation_Delegate_Base;
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformation = Winapi.CommonTypes.TypedEventHandler_2__IDeviceWatcher__IDeviceInformation;
  PTypedEventHandler_2__IDeviceWatcher__IDeviceInformation = Winapi.CommonTypes.PTypedEventHandler_2__IDeviceWatcher__IDeviceInformation;
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate_Delegate_Base;
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate = Winapi.CommonTypes.TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate;
  PTypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate = Winapi.CommonTypes.PTypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate;
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject_Delegate_Base;
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject = Winapi.CommonTypes.TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject;
  PTypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject = Winapi.CommonTypes.PTypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject;
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate_Delegate_Base;
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate = Winapi.CommonTypes.TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate;
  PTypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate = Winapi.CommonTypes.PTypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate;

  // Forward declarations for interfaces

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.DeviceAccessStatus>
  AsyncOperationCompletedHandler_1__DeviceAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__DeviceAccessStatus = ^AsyncOperationCompletedHandler_1__DeviceAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.DeviceAccessStatus>
  IAsyncOperation_1__DeviceAccessStatus = interface;
  PIAsyncOperation_1__DeviceAccessStatus = ^IAsyncOperation_1__DeviceAccessStatus;

  // Windows.Devices.Enumeration.IDeviceAccessChangedEventArgs
  IDeviceAccessChangedEventArgs = interface;
  PIDeviceAccessChangedEventArgs = ^IDeviceAccessChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceAccessInformation,Windows.Devices.Enumeration.IDeviceAccessChangedEventArgs>
  TypedEventHandler_2__IDeviceAccessInformation__IDeviceAccessChangedEventArgs = interface;
  PTypedEventHandler_2__IDeviceAccessInformation__IDeviceAccessChangedEventArgs = ^TypedEventHandler_2__IDeviceAccessInformation__IDeviceAccessChangedEventArgs;

  // Windows.Devices.Enumeration.IDeviceAccessInformation
  IDeviceAccessInformation = interface;
  PIDeviceAccessInformation = ^IDeviceAccessInformation;

  // Windows.Devices.Enumeration.IDeviceAccessChangedEventArgs2
  IDeviceAccessChangedEventArgs2 = interface;
  PIDeviceAccessChangedEventArgs2 = ^IDeviceAccessChangedEventArgs2;

  // Windows.Devices.Enumeration.IDevicePairingResult
  IDevicePairingResult = interface;
  PIDevicePairingResult = ^IDevicePairingResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDevicePairingResult>
  AsyncOperationCompletedHandler_1__IDevicePairingResult = interface;
  PAsyncOperationCompletedHandler_1__IDevicePairingResult = ^AsyncOperationCompletedHandler_1__IDevicePairingResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDevicePairingResult>
  IAsyncOperation_1__IDevicePairingResult = interface;
  PIAsyncOperation_1__IDevicePairingResult = ^IAsyncOperation_1__IDevicePairingResult;

  // Windows.Devices.Enumeration.IDeviceInformationPairing
  IDeviceInformationPairing = interface;
  PIDeviceInformationPairing = ^IDeviceInformationPairing;

  // Windows.Devices.Enumeration.IDeviceInformation2
  IDeviceInformation2 = interface;
  PIDeviceInformation2 = ^IDeviceInformation2;

  // Windows.Devices.Enumeration.IDevicePairingSettings
  IDevicePairingSettings = interface;
  PIDevicePairingSettings = ^IDevicePairingSettings;

  // Windows.Devices.Enumeration.IDevicePairingRequestedEventArgs
  IDevicePairingRequestedEventArgs = interface;
  PIDevicePairingRequestedEventArgs = ^IDevicePairingRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceInformationCustomPairing,Windows.Devices.Enumeration.IDevicePairingRequestedEventArgs>
  TypedEventHandler_2__IDeviceInformationCustomPairing__IDevicePairingRequestedEventArgs = interface;
  PTypedEventHandler_2__IDeviceInformationCustomPairing__IDevicePairingRequestedEventArgs = ^TypedEventHandler_2__IDeviceInformationCustomPairing__IDevicePairingRequestedEventArgs;

  // Windows.Devices.Enumeration.IDeviceInformationCustomPairing
  IDeviceInformationCustomPairing = interface;
  PIDeviceInformationCustomPairing = ^IDeviceInformationCustomPairing;

  // Windows.Devices.Enumeration.IDeviceUnpairingResult
  IDeviceUnpairingResult = interface;
  PIDeviceUnpairingResult = ^IDeviceUnpairingResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDeviceUnpairingResult>
  AsyncOperationCompletedHandler_1__IDeviceUnpairingResult = interface;
  PAsyncOperationCompletedHandler_1__IDeviceUnpairingResult = ^AsyncOperationCompletedHandler_1__IDeviceUnpairingResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDeviceUnpairingResult>
  IAsyncOperation_1__IDeviceUnpairingResult = interface;
  PIAsyncOperation_1__IDeviceUnpairingResult = ^IAsyncOperation_1__IDeviceUnpairingResult;

  // Windows.Devices.Enumeration.IDeviceInformationPairing2
  IDeviceInformationPairing2 = interface;
  PIDeviceInformationPairing2 = ^IDeviceInformationPairing2;

  // Windows.Devices.Enumeration.IDeviceInformationPairingStatics
  IDeviceInformationPairingStatics = interface;
  PIDeviceInformationPairingStatics = ^IDeviceInformationPairingStatics;

  // Windows.Devices.Enumeration.IDeviceInformationPairingStatics2
  IDeviceInformationPairingStatics2 = interface;
  PIDeviceInformationPairingStatics2 = ^IDeviceInformationPairingStatics2;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.IDeviceInformation>
  IIterator_1__IDeviceInformation = interface;
  PIIterator_1__IDeviceInformation = ^IIterator_1__IDeviceInformation;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.IDeviceInformation>
  IIterable_1__IDeviceInformation = interface;
  PIIterable_1__IDeviceInformation = ^IIterable_1__IDeviceInformation;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>
  IVectorView_1__IDeviceInformation = interface;
  PIVectorView_1__IDeviceInformation = ^IVectorView_1__IDeviceInformation;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation = ^AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>>
  IAsyncOperation_1__IVectorView_1__IDeviceInformation = interface;
  PIAsyncOperation_1__IVectorView_1__IDeviceInformation = ^IAsyncOperation_1__IVectorView_1__IDeviceInformation;

  // Windows.Devices.Enumeration.IDeviceInformationStatics
  IDeviceInformationStatics = interface;
  PIDeviceInformationStatics = ^IDeviceInformationStatics;

  // Windows.Devices.Enumeration.IDeviceInformationStatics2
  IDeviceInformationStatics2 = interface;
  PIDeviceInformationStatics2 = ^IDeviceInformationStatics2;

  // Windows.Devices.Enumeration.IDeviceInformationUpdate2
  IDeviceInformationUpdate2 = interface;
  PIDeviceInformationUpdate2 = ^IDeviceInformationUpdate2;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.DeviceClass>
  IIterator_1__DeviceClass = interface;
  PIIterator_1__DeviceClass = ^IIterator_1__DeviceClass;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.DeviceClass>
  IIterable_1__DeviceClass = interface;
  PIIterable_1__DeviceClass = ^IIterable_1__DeviceClass;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.DeviceWatcherEventKind>
  IIterator_1__DeviceWatcherEventKind = interface;
  PIIterator_1__DeviceWatcherEventKind = ^IIterator_1__DeviceWatcherEventKind;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.DeviceWatcherEventKind>
  IIterable_1__DeviceWatcherEventKind = interface;
  PIIterable_1__DeviceWatcherEventKind = ^IIterable_1__DeviceWatcherEventKind;

  // Windows.Devices.Enumeration.IEnclosureLocation2
  IEnclosureLocation2 = interface;
  PIEnclosureLocation2 = ^IEnclosureLocation2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  AsyncOperationCompletedHandler_1__Pnp_IPnpObject = interface;
  PAsyncOperationCompletedHandler_1__Pnp_IPnpObject = ^AsyncOperationCompletedHandler_1__Pnp_IPnpObject;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IAsyncOperation_1__Pnp_IPnpObject = interface;
  PIAsyncOperation_1__Pnp_IPnpObject = ^IAsyncOperation_1__Pnp_IPnpObject;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IIterator_1__Pnp_IPnpObject = interface;
  PIIterator_1__Pnp_IPnpObject = ^IIterator_1__Pnp_IPnpObject;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IIterable_1__Pnp_IPnpObject = interface;
  PIIterable_1__Pnp_IPnpObject = ^IIterable_1__Pnp_IPnpObject;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IVectorView_1__Pnp_IPnpObject = interface;
  PIVectorView_1__Pnp_IPnpObject = ^IVectorView_1__Pnp_IPnpObject;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject = ^AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>>
  IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject = interface;
  PIAsyncOperation_1__IVectorView_1__Pnp_IPnpObject = ^IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject;

  // Windows.Devices.Enumeration.Pnp.IPnpObjectStatics
  Pnp_IPnpObjectStatics = interface;
  PPnp_IPnpObjectStatics = ^Pnp_IPnpObjectStatics;

  // Windows.Devices.Enumeration Enums

  // Windows.Devices.Enumeration.DeviceAccessStatus
  DeviceAccessStatus = (
    Unspecified = 0,
    Allowed = 1,
    DeniedByUser = 2,
    DeniedBySystem = 3
  );
  PDeviceAccessStatus = ^DeviceAccessStatus;

  // Windows.Devices.Enumeration.DeviceInformationKind
  DeviceInformationKind = (
    Unknown = 0,
    DeviceInterface = 1,
    DeviceContainer = 2,
    Device = 3,
    DeviceInterfaceClass = 4,
    AssociationEndpoint = 5,
    AssociationEndpointContainer = 6,
    AssociationEndpointService = 7,
    DevicePanel = 8
  );
  PDeviceInformationKind = ^DeviceInformationKind;

  // Windows.Devices.Enumeration.DevicePairingKinds
  DevicePairingKinds = (
    None = 0,
    ConfirmOnly = 1,
    DisplayPin = 2,
    ProvidePin = 4,
    ConfirmPinMatch = 8,
    ProvidePasswordCredential = 16
  );
  PDevicePairingKinds = ^DevicePairingKinds;

  // Windows.Devices.Enumeration.DevicePairingProtectionLevel
  DevicePairingProtectionLevel = (
    Default = 0,
    None = 1,
    Encryption = 2,
    EncryptionAndAuthentication = 3
  );
  PDevicePairingProtectionLevel = ^DevicePairingProtectionLevel;

  // Windows.Devices.Enumeration.DevicePairingResultStatus
  DevicePairingResultStatus = (
    Paired = 0,
    NotReadyToPair = 1,
    NotPaired = 2,
    AlreadyPaired = 3,
    ConnectionRejected = 4,
    TooManyConnections = 5,
    HardwareFailure = 6,
    AuthenticationTimeout = 7,
    AuthenticationNotAllowed = 8,
    AuthenticationFailure = 9,
    NoSupportedProfiles = 10,
    ProtectionLevelCouldNotBeMet = 11,
    AccessDenied = 12,
    InvalidCeremonyData = 13,
    PairingCanceled = 14,
    OperationAlreadyInProgress = 15,
    RequiredHandlerNotRegistered = 16,
    RejectedByHandler = 17,
    RemoteDeviceHasAssociation = 18,
    Failed = 19
  );
  PDevicePairingResultStatus = ^DevicePairingResultStatus;

  // Windows.Devices.Enumeration.DeviceUnpairingResultStatus
  DeviceUnpairingResultStatus = (
    Unpaired = 0,
    AlreadyUnpaired = 1,
    OperationAlreadyInProgress = 2,
    AccessDenied = 3,
    Failed = 4
  );
  PDeviceUnpairingResultStatus = ^DeviceUnpairingResultStatus;

  // Windows.Devices.Enumeration.DeviceWatcherEventKind
  DeviceWatcherEventKind = (
    Add = 0,
    Update = 1,
    Remove = 2
  );
  PDeviceWatcherEventKind = ^DeviceWatcherEventKind;

  // Windows.Devices.Enumeration Interfaces

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.DeviceAccessStatus>
  AsyncOperationCompletedHandler_1__DeviceAccessStatus_Delegate_Base = interface(IUnknown)
  ['{EE154D83-805B-53E8-8469-90715036D013}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__DeviceAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.DeviceAccessStatus>
  AsyncOperationCompletedHandler_1__DeviceAccessStatus = interface(AsyncOperationCompletedHandler_1__DeviceAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.DeviceAccessStatus>
  IAsyncOperation_1__DeviceAccessStatus_Base = interface(IInspectable)
  ['{C00BC2F2-A7F8-5F3F-80D1-2808EF6BCA10}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__DeviceAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__DeviceAccessStatus; safecall;
    function GetResults: DeviceAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__DeviceAccessStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.DeviceAccessStatus>
  IAsyncOperation_1__DeviceAccessStatus = interface(IAsyncOperation_1__DeviceAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceAccessChangedEventArgs
  IDeviceAccessChangedEventArgs = interface(IInspectable)
  ['{DEDA0BCC-4F9D-4F58-9DBA-A9BC800408D5}']
    function get_Status: DeviceAccessStatus; safecall;
    property Status: DeviceAccessStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceAccessInformation,Windows.Devices.Enumeration.IDeviceAccessChangedEventArgs>
  TypedEventHandler_2__IDeviceAccessInformation__IDeviceAccessChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{4C71D028-B793-5BCE-AE59-FA77F45A40D8}']
    procedure Invoke(sender: IDeviceAccessInformation; args: IDeviceAccessChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceAccessInformation,Windows.Devices.Enumeration.IDeviceAccessChangedEventArgs>
  TypedEventHandler_2__IDeviceAccessInformation__IDeviceAccessChangedEventArgs = interface(TypedEventHandler_2__IDeviceAccessInformation__IDeviceAccessChangedEventArgs_Delegate_Base)
  ['{6A7B811E-C5D4-55BB-BB7B-99CAA2F420E2}']
  end;

  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceAccessInformation
  IDeviceAccessInformation = interface(IInspectable)
  ['{0BAA9A73-6DE5-4915-8DDD-9A0554A6F545}']
    function add_AccessChanged(handler: TypedEventHandler_2__IDeviceAccessInformation__IDeviceAccessChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AccessChanged(cookie: EventRegistrationToken); safecall;
    function get_CurrentStatus: DeviceAccessStatus; safecall;
    property CurrentStatus: DeviceAccessStatus read get_CurrentStatus;
  end;

  // Windows.Devices.Enumeration.IDeviceAccessChangedEventArgs2
  IDeviceAccessChangedEventArgs2 = interface(IInspectable)
  ['{82523262-934B-4B30-A178-ADC39F2F2BE3}']
    function get_Id: HSTRING; safecall;
    property Id: HSTRING read get_Id;
  end;

  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDevicePairingResult
  IDevicePairingResult = interface(IInspectable)
  ['{072B02BF-DD95-4025-9B37-DE51ADBA37B7}']
    function get_Status: DevicePairingResultStatus; safecall;
    function get_ProtectionLevelUsed: DevicePairingProtectionLevel; safecall;
    property ProtectionLevelUsed: DevicePairingProtectionLevel read get_ProtectionLevelUsed;
    property Status: DevicePairingResultStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDevicePairingResult>
  AsyncOperationCompletedHandler_1__IDevicePairingResult_Delegate_Base = interface(IUnknown)
  ['{7EE0247F-5F57-5CB2-B40E-18B5A211D6C3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IDevicePairingResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDevicePairingResult>
  AsyncOperationCompletedHandler_1__IDevicePairingResult = interface(AsyncOperationCompletedHandler_1__IDevicePairingResult_Delegate_Base)
  ['{FEE5AE5D-187F-56A3-AFD2-3C5ABCCCBE14}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDevicePairingResult>
  IAsyncOperation_1__IDevicePairingResult_Base = interface(IInspectable)
  ['{1002DB74-8948-591E-815D-E40B667599A3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IDevicePairingResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IDevicePairingResult; safecall;
    function GetResults: IDevicePairingResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IDevicePairingResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDevicePairingResult>
  IAsyncOperation_1__IDevicePairingResult = interface(IAsyncOperation_1__IDevicePairingResult_Base)
  ['{529C658D-8499-557A-A110-7634DDC46275}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformationPairing
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_DeviceInformationPairing)]
  IDeviceInformationPairing = interface(IInspectable)
  ['{2C4769F5-F684-40D5-8469-E8DBAAB70485}']
    function get_IsPaired: Boolean; safecall;
    function get_CanPair: Boolean; safecall;
    function PairAsync: IAsyncOperation_1__IDevicePairingResult; overload; safecall;
    function PairAsync(minProtectionLevel: DevicePairingProtectionLevel): IAsyncOperation_1__IDevicePairingResult; overload; safecall;
    property CanPair: Boolean read get_CanPair;
    property IsPaired: Boolean read get_IsPaired;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformation2
  IDeviceInformation2 = interface(IInspectable)
  ['{F156A638-7997-48D9-A10C-269D46533F48}']
    function get_Kind: DeviceInformationKind; safecall;
    function get_Pairing: IDeviceInformationPairing; safecall;
    property Kind: DeviceInformationKind read get_Kind;
    property Pairing: IDeviceInformationPairing read get_Pairing;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDevicePairingSettings
  IDevicePairingSettings = interface(IInspectable)
  ['{482CB27C-83BB-420E-BE51-6602B222DE54}']
  end;

  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDevicePairingRequestedEventArgs
  IDevicePairingRequestedEventArgs = interface(IInspectable)
  ['{F717FC56-DE6B-487F-8376-0180ACA69963}']
    function get_DeviceInformation: IDeviceInformation; safecall;
    function get_PairingKind: DevicePairingKinds; safecall;
    function get_Pin: HSTRING; safecall;
    procedure Accept; overload; safecall;
    procedure Accept(pin: HSTRING); overload; safecall;
    function GetDeferral: IDeferral; safecall;
    property DeviceInformation: IDeviceInformation read get_DeviceInformation;
    property PairingKind: DevicePairingKinds read get_PairingKind;
    property Pin: HSTRING read get_Pin;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceInformationCustomPairing,Windows.Devices.Enumeration.IDevicePairingRequestedEventArgs>
  TypedEventHandler_2__IDeviceInformationCustomPairing__IDevicePairingRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{FA65231F-4178-5DE1-B2CC-03E22D7702B4}']
    procedure Invoke(sender: IDeviceInformationCustomPairing; args: IDevicePairingRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceInformationCustomPairing,Windows.Devices.Enumeration.IDevicePairingRequestedEventArgs>
  TypedEventHandler_2__IDeviceInformationCustomPairing__IDevicePairingRequestedEventArgs = interface(TypedEventHandler_2__IDeviceInformationCustomPairing__IDevicePairingRequestedEventArgs_Delegate_Base)
  ['{09195B85-D449-5DF4-AE79-5919FDCB9DED}']
  end;

  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformationCustomPairing
  IDeviceInformationCustomPairing = interface(IInspectable)
  ['{85138C02-4EE6-4914-8370-107A39144C0E}']
    function PairAsync(pairingKindsSupported: DevicePairingKinds): IAsyncOperation_1__IDevicePairingResult; overload; safecall;
    function PairAsync(pairingKindsSupported: DevicePairingKinds; minProtectionLevel: DevicePairingProtectionLevel): IAsyncOperation_1__IDevicePairingResult; overload; safecall;
    function PairAsync(pairingKindsSupported: DevicePairingKinds; minProtectionLevel: DevicePairingProtectionLevel; devicePairingSettings: IDevicePairingSettings): IAsyncOperation_1__IDevicePairingResult; overload; safecall;
    function add_PairingRequested(handler: TypedEventHandler_2__IDeviceInformationCustomPairing__IDevicePairingRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_PairingRequested(token: EventRegistrationToken); safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceUnpairingResult
  IDeviceUnpairingResult = interface(IInspectable)
  ['{66F44AD3-79D9-444B-92CF-A92EF72571C7}']
    function get_Status: DeviceUnpairingResultStatus; safecall;
    property Status: DeviceUnpairingResultStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDeviceUnpairingResult>
  AsyncOperationCompletedHandler_1__IDeviceUnpairingResult_Delegate_Base = interface(IUnknown)
  ['{9BBE6EB9-DB2D-5160-A20C-F0C265F20D8E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IDeviceUnpairingResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDeviceUnpairingResult>
  AsyncOperationCompletedHandler_1__IDeviceUnpairingResult = interface(AsyncOperationCompletedHandler_1__IDeviceUnpairingResult_Delegate_Base)
  ['{32DA3AD5-F202-544C-94F8-2BC574BE3DA8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDeviceUnpairingResult>
  IAsyncOperation_1__IDeviceUnpairingResult_Base = interface(IInspectable)
  ['{2BB4DF3D-BD7E-5FE0-9020-56DC0D30B935}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IDeviceUnpairingResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IDeviceUnpairingResult; safecall;
    function GetResults: IDeviceUnpairingResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IDeviceUnpairingResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDeviceUnpairingResult>
  IAsyncOperation_1__IDeviceUnpairingResult = interface(IAsyncOperation_1__IDeviceUnpairingResult_Base)
  ['{27C0B299-384A-5783-8DC1-60C21161FABB}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformationPairing2
  IDeviceInformationPairing2 = interface(IInspectable)
  ['{F68612FD-0AEE-4328-85CC-1C742BB1790D}']
    function get_ProtectionLevel: DevicePairingProtectionLevel; safecall;
    function get_Custom: IDeviceInformationCustomPairing; safecall;
    function PairAsync(minProtectionLevel: DevicePairingProtectionLevel; devicePairingSettings: IDevicePairingSettings): IAsyncOperation_1__IDevicePairingResult; safecall;
    function UnpairAsync: IAsyncOperation_1__IDeviceUnpairingResult; safecall;
    property Custom: IDeviceInformationCustomPairing read get_Custom;
    property ProtectionLevel: DevicePairingProtectionLevel read get_ProtectionLevel;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformationPairingStatics
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_DeviceInformationPairing)]
  IDeviceInformationPairingStatics = interface(IInspectable)
  ['{E915C408-36D4-49A1-BF13-514173799B6B}']
    function TryRegisterForAllInboundPairingRequests(pairingKindsSupported: DevicePairingKinds): Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformationPairingStatics2
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_DeviceInformationPairing)]
  IDeviceInformationPairingStatics2 = interface(IInspectable)
  ['{04DE5372-B7B7-476B-A74F-C5836A704D98}']
    function TryRegisterForAllInboundPairingRequestsWithProtectionLevel(pairingKindsSupported: DevicePairingKinds; minProtectionLevel: DevicePairingProtectionLevel): Boolean; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.IDeviceInformation>
  IIterator_1__IDeviceInformation_Base = interface(IInspectable)
  ['{6F85D843-E8AB-5B46-85D7-327C58D18712}']
    function get_Current: IDeviceInformation; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIDeviceInformation): Cardinal; safecall;
    property Current: IDeviceInformation read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.IDeviceInformation>
  IIterator_1__IDeviceInformation = interface(IIterator_1__IDeviceInformation_Base)
  ['{F06491A0-6D3D-5A38-B164-AEE6E5B0326C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.IDeviceInformation>
  IIterable_1__IDeviceInformation_Base = interface(IInspectable)
  ['{DD9F8A5D-EC98-5F4B-A3EA-9C8B5AD53C4B}']
    function First: IIterator_1__IDeviceInformation; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.IDeviceInformation>
  IIterable_1__IDeviceInformation = interface(IIterable_1__IDeviceInformation_Base)
  ['{180E4B06-9936-5D42-A5BF-1BDC08713DBD}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>
  IVectorView_1__IDeviceInformation = interface(IInspectable)
  ['{717AA701-AB02-5C8D-AD35-EE564CF8BB0E}']
    function GetAt(index: Cardinal): IDeviceInformation; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IDeviceInformation; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIDeviceInformation): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation_Delegate_Base = interface(IUnknown)
  ['{4A458732-527E-5C73-9A68-A73DA370F782}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IDeviceInformation; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation_Delegate_Base)
  ['{E2375529-DAC5-52E8-B1DE-7AAE0A9202CC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>>
  IAsyncOperation_1__IVectorView_1__IDeviceInformation_Base = interface(IInspectable)
  ['{45180254-082E-5274-B2E7-AC0517F44D07}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation; safecall;
    function GetResults: IVectorView_1__IDeviceInformation; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IDeviceInformation read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.IDeviceInformation>>
  IAsyncOperation_1__IVectorView_1__IDeviceInformation = interface(IAsyncOperation_1__IVectorView_1__IDeviceInformation_Base)
  ['{42F4622B-7AB2-59F9-8C91-8235DC44E8F4}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformationStatics
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_DeviceInformation)]
  IDeviceInformationStatics = interface(IInspectable)
  ['{C17F100E-3A46-4A78-8013-769DC9B97390}']
    function CreateFromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IDeviceInformation; overload; safecall;
    function CreateFromIdAsync(deviceId: HSTRING; additionalProperties: IIterable_1__HSTRING): IAsyncOperation_1__IDeviceInformation; overload; safecall;
    function FindAllAsync: IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; safecall;
    function FindAllAsync(deviceClass: DeviceClass): IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; safecall;
    function FindAllAsync(aqsFilter: HSTRING): IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; safecall;
    function FindAllAsync(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING): IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; safecall;
    function CreateWatcher: IDeviceWatcher; overload; safecall;
    function CreateWatcher(deviceClass: DeviceClass): IDeviceWatcher; overload; safecall;
    function CreateWatcher(aqsFilter: HSTRING): IDeviceWatcher; overload; safecall;
    function CreateWatcher(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING): IDeviceWatcher; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformationStatics2
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_DeviceInformation)]
  IDeviceInformationStatics2 = interface(IInspectable)
  ['{493B4F34-A84F-45FD-9167-15D1CB1BD1F9}']
    function GetAqsFilterFromDeviceClass(deviceClass: DeviceClass): HSTRING; safecall;
    function CreateFromIdAsync(deviceId: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IAsyncOperation_1__IDeviceInformation; safecall;
    function FindAllAsync(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IAsyncOperation_1__IVectorView_1__IDeviceInformation; safecall;
    function CreateWatcher(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IDeviceWatcher; safecall;
  end;

  // Windows.Devices.Enumeration.IDeviceInformationUpdate2
  IDeviceInformationUpdate2 = interface(IInspectable)
  ['{5D9D148C-A873-485E-BAA6-AA620788E3CC}']
    function get_Kind: DeviceInformationKind; safecall;
    property Kind: DeviceInformationKind read get_Kind;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.DeviceClass>
  IIterator_1__DeviceClass_Base = interface(IInspectable)
  ['{C3807283-1416-593C-955C-0B4A286FF7BB}']
    function get_Current: DeviceClass; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDeviceClass): Cardinal; safecall;
    property Current: DeviceClass read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.DeviceClass>
  IIterator_1__DeviceClass = interface(IIterator_1__DeviceClass_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.DeviceClass>
  IIterable_1__DeviceClass_Base = interface(IInspectable)
  ['{47D4BE05-58F1-522E-81C6-975EB4131BB9}']
    function First: IIterator_1__DeviceClass; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.DeviceClass>
  IIterable_1__DeviceClass = interface(IIterable_1__DeviceClass_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.DeviceWatcherEventKind>
  IIterator_1__DeviceWatcherEventKind_Base = interface(IInspectable)
  ['{CB5CA9DB-CCD6-5103-A93D-C925C908838D}']
    function get_Current: DeviceWatcherEventKind; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDeviceWatcherEventKind): Cardinal; safecall;
    property Current: DeviceWatcherEventKind read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.DeviceWatcherEventKind>
  IIterator_1__DeviceWatcherEventKind = interface(IIterator_1__DeviceWatcherEventKind_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.DeviceWatcherEventKind>
  IIterable_1__DeviceWatcherEventKind_Base = interface(IInspectable)
  ['{F04365AB-D3F3-5F85-A7DA-DC19CFF73D86}']
    function First: IIterator_1__DeviceWatcherEventKind; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.DeviceWatcherEventKind>
  IIterable_1__DeviceWatcherEventKind = interface(IIterable_1__DeviceWatcherEventKind_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Devices.Enumeration.IEnclosureLocation2
  IEnclosureLocation2 = interface(IInspectable)
  ['{2885995B-E07D-485D-8A9E-BDF29AEF4F66}']
    function get_RotationAngleInDegreesClockwise: Cardinal; safecall;
    property RotationAngleInDegreesClockwise: Cardinal read get_RotationAngleInDegreesClockwise;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  AsyncOperationCompletedHandler_1__Pnp_IPnpObject_Delegate_Base = interface(IUnknown)
  ['{9D615463-6879-521F-8E97-E66D3DDBC95E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Pnp_IPnpObject; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  AsyncOperationCompletedHandler_1__Pnp_IPnpObject = interface(AsyncOperationCompletedHandler_1__Pnp_IPnpObject_Delegate_Base)
  ['{863BDB04-0585-5767-A64A-B247CE04BE47}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IAsyncOperation_1__Pnp_IPnpObject_Base = interface(IInspectable)
  ['{22B0FB93-30E6-501A-BD3B-9FA3063E9C16}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Pnp_IPnpObject); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Pnp_IPnpObject; safecall;
    function GetResults: Pnp_IPnpObject; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Pnp_IPnpObject read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IAsyncOperation_1__Pnp_IPnpObject = interface(IAsyncOperation_1__Pnp_IPnpObject_Base)
  ['{DCC6CC16-85D7-5A00-8E63-9BC7075C0B08}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IIterator_1__Pnp_IPnpObject_Base = interface(IInspectable)
  ['{6BB6D2F1-B5FB-57F0-8251-F20CDE5A6871}']
    function get_Current: Pnp_IPnpObject; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPnp_IPnpObject): Cardinal; safecall;
    property Current: Pnp_IPnpObject read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IIterator_1__Pnp_IPnpObject = interface(IIterator_1__Pnp_IPnpObject_Base)
  ['{DEA7DBDA-BCCA-58DD-96FB-3FBEA7FAF5D8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IIterable_1__Pnp_IPnpObject_Base = interface(IInspectable)
  ['{30B50092-36EE-53FF-9450-029004436C60}']
    function First: IIterator_1__Pnp_IPnpObject; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IIterable_1__Pnp_IPnpObject = interface(IIterable_1__Pnp_IPnpObject_Base)
  ['{17677D16-E3DB-51B9-AF2E-D4EDFB202D13}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>
  IVectorView_1__Pnp_IPnpObject = interface(IInspectable)
  ['{5B091106-8910-5FE6-8C50-A27C65BD860C}']
    function GetAt(index: Cardinal): Pnp_IPnpObject; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Pnp_IPnpObject; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPnp_IPnpObject): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject_Delegate_Base = interface(IUnknown)
  ['{811D834C-A15E-5522-B7F4-E53004FC58FF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject_Delegate_Base)
  ['{43B26CAC-1267-5096-A034-0C4142CDEDF6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>>
  IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject_Base = interface(IInspectable)
  ['{F383C2CC-F326-5BBE-95D1-CBC24714EF86}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject; safecall;
    function GetResults: IVectorView_1__Pnp_IPnpObject; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Pnp_IPnpObject read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.Pnp.IPnpObject>>
  IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject = interface(IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject_Base)
  ['{E4B7CC1B-E86A-53BF-9BD8-D0505EAA62A5}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Enumeration.Pnp.IPnpObjectStatics
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_Pnp_PnpObject)]
  Pnp_IPnpObjectStatics = interface(IInspectable)
  ['{B3C32A3D-D168-4660-BBF3-A733B14B6E01}']
    function CreateFromIdAsync(&type: Pnp_PnpObjectType; id: HSTRING; requestedProperties: IIterable_1__HSTRING): IAsyncOperation_1__Pnp_IPnpObject; safecall;
    function FindAllAsync(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING): IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject; overload; safecall;
    function FindAllAsync(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING; aqsFilter: HSTRING): IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject; overload; safecall;
    function CreateWatcher(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING): Pnp_IPnpObjectWatcher; overload; safecall;
    function CreateWatcher(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING; aqsFilter: HSTRING): Pnp_IPnpObjectWatcher; overload; safecall;
  end;

  // Windows.Devices.Enumeration.DeviceInformation
  // DualAPI
  // Implements: Windows.Devices.Enumeration.IDeviceInformation
  // Implements: Windows.Devices.Enumeration.IDeviceInformation2
  // Statics: "Windows.Devices.Enumeration.IDeviceInformationStatics"
  // Statics: "Windows.Devices.Enumeration.IDeviceInformationStatics2"
  TDeviceInformation = class(TWinRTGenericImportS2<IDeviceInformationStatics, IDeviceInformationStatics2>)
  public
    // -> IDeviceInformationStatics
    class function CreateFromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IDeviceInformation; overload; static; inline;
    class function CreateFromIdAsync(deviceId: HSTRING; additionalProperties: IIterable_1__HSTRING): IAsyncOperation_1__IDeviceInformation; overload; static; inline;
    class function FindAllAsync: IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; static; inline;
    class function FindAllAsync(deviceClass: DeviceClass): IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; static; inline;
    class function FindAllAsync(aqsFilter: HSTRING): IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; static; inline;
    class function FindAllAsync(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING): IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; static; inline;
    class function CreateWatcher: IDeviceWatcher; overload; static; inline;
    class function CreateWatcher(deviceClass: DeviceClass): IDeviceWatcher; overload; static; inline;
    class function CreateWatcher(aqsFilter: HSTRING): IDeviceWatcher; overload; static; inline;
    class function CreateWatcher(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING): IDeviceWatcher; overload; static; inline;

    // -> IDeviceInformationStatics2
    class function GetAqsFilterFromDeviceClass(deviceClass: DeviceClass): HSTRING; static; inline;
    class function CreateFromIdAsync(deviceId: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IAsyncOperation_1__IDeviceInformation; overload; static; inline;
    class function FindAllAsync(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IAsyncOperation_1__IVectorView_1__IDeviceInformation; overload; static; inline;
    class function CreateWatcher(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IDeviceWatcher; overload; static; inline;
  end;

  // Windows.Devices.Enumeration.DeviceInformationPairing
  // Explicitly imported
  // Implements: Windows.Devices.Enumeration.IDeviceInformationPairing
  // Implements: Windows.Devices.Enumeration.IDeviceInformationPairing2
  // Statics: "Windows.Devices.Enumeration.IDeviceInformationPairingStatics"
  // Statics: "Windows.Devices.Enumeration.IDeviceInformationPairingStatics2"
  TDeviceInformationPairing = class(TWinRTGenericImportS2<IDeviceInformationPairingStatics, IDeviceInformationPairingStatics2>)
  public
    // -> IDeviceInformationPairingStatics
    class function TryRegisterForAllInboundPairingRequests(pairingKindsSupported: DevicePairingKinds): Boolean; static; inline;

    // -> IDeviceInformationPairingStatics2
    class function TryRegisterForAllInboundPairingRequestsWithProtectionLevel(pairingKindsSupported: DevicePairingKinds; minProtectionLevel: DevicePairingProtectionLevel): Boolean; static; inline;
  end;

  // Windows.Devices.Enumeration.DevicePicker
  // DualAPI
  // Implements: Windows.Devices.Enumeration.IDevicePicker
  // Instantiable: "IDevicePicker"
  TDevicePicker = class(TWinRTGenericImportI<IDevicePicker>) end;

  // Windows.Devices.Enumeration.Pnp.PnpObject
  // DualAPI
  // Implements: Windows.Devices.Enumeration.Pnp.IPnpObject
  // Statics: "Windows.Devices.Enumeration.Pnp.IPnpObjectStatics"
  TPnp_PnpObject = class(TWinRTGenericImportS<Pnp_IPnpObjectStatics>)
  public
    // -> Pnp_IPnpObjectStatics
    class function CreateFromIdAsync(&type: Pnp_PnpObjectType; id: HSTRING; requestedProperties: IIterable_1__HSTRING): IAsyncOperation_1__Pnp_IPnpObject; static; inline;
    class function FindAllAsync(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING): IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject; overload; static; inline;
    class function FindAllAsync(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING; aqsFilter: HSTRING): IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject; overload; static; inline;
    class function CreateWatcher(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING): Pnp_IPnpObjectWatcher; overload; static; inline;
    class function CreateWatcher(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING; aqsFilter: HSTRING): Pnp_IPnpObjectWatcher; overload; static; inline;
  end;

implementation

{ TDeviceInformation }

class function TDeviceInformation.CreateFromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IDeviceInformation;
begin
  Result := Statics.CreateFromIdAsync(deviceId);
end;

class function TDeviceInformation.CreateFromIdAsync(deviceId: HSTRING; additionalProperties: IIterable_1__HSTRING): IAsyncOperation_1__IDeviceInformation;
begin
  Result := Statics.CreateFromIdAsync(deviceId, additionalProperties);
end;

class function TDeviceInformation.FindAllAsync: IAsyncOperation_1__IVectorView_1__IDeviceInformation;
begin
  Result := Statics.FindAllAsync;
end;

class function TDeviceInformation.FindAllAsync(deviceClass: DeviceClass): IAsyncOperation_1__IVectorView_1__IDeviceInformation;
begin
  Result := Statics.FindAllAsync(deviceClass);
end;

class function TDeviceInformation.FindAllAsync(aqsFilter: HSTRING): IAsyncOperation_1__IVectorView_1__IDeviceInformation;
begin
  Result := Statics.FindAllAsync(aqsFilter);
end;

class function TDeviceInformation.FindAllAsync(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING): IAsyncOperation_1__IVectorView_1__IDeviceInformation;
begin
  Result := Statics.FindAllAsync(aqsFilter, additionalProperties);
end;

class function TDeviceInformation.CreateWatcher: IDeviceWatcher;
begin
  Result := Statics.CreateWatcher;
end;

class function TDeviceInformation.CreateWatcher(deviceClass: DeviceClass): IDeviceWatcher;
begin
  Result := Statics.CreateWatcher(deviceClass);
end;

class function TDeviceInformation.CreateWatcher(aqsFilter: HSTRING): IDeviceWatcher;
begin
  Result := Statics.CreateWatcher(aqsFilter);
end;

class function TDeviceInformation.CreateWatcher(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING): IDeviceWatcher;
begin
  Result := Statics.CreateWatcher(aqsFilter, additionalProperties);
end;


class function TDeviceInformation.GetAqsFilterFromDeviceClass(deviceClass: DeviceClass): HSTRING;
begin
  Result := Statics2.GetAqsFilterFromDeviceClass(deviceClass);
end;

class function TDeviceInformation.CreateFromIdAsync(deviceId: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IAsyncOperation_1__IDeviceInformation;
begin
  Result := Statics2.CreateFromIdAsync(deviceId, additionalProperties, kind);
end;

class function TDeviceInformation.FindAllAsync(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IAsyncOperation_1__IVectorView_1__IDeviceInformation;
begin
  Result := Statics2.FindAllAsync(aqsFilter, additionalProperties, kind);
end;

class function TDeviceInformation.CreateWatcher(aqsFilter: HSTRING; additionalProperties: IIterable_1__HSTRING; kind: DeviceInformationKind): IDeviceWatcher;
begin
  Result := Statics2.CreateWatcher(aqsFilter, additionalProperties, kind);
end;


{ TDeviceInformationPairing }

class function TDeviceInformationPairing.TryRegisterForAllInboundPairingRequests(pairingKindsSupported: DevicePairingKinds): Boolean;
begin
  Result := Statics.TryRegisterForAllInboundPairingRequests(pairingKindsSupported);
end;


class function TDeviceInformationPairing.TryRegisterForAllInboundPairingRequestsWithProtectionLevel(pairingKindsSupported: DevicePairingKinds; minProtectionLevel: DevicePairingProtectionLevel): Boolean;
begin
  Result := Statics2.TryRegisterForAllInboundPairingRequestsWithProtectionLevel(pairingKindsSupported, minProtectionLevel);
end;


{ TDevicePicker }

{ TPnp_PnpObject }

class function TPnp_PnpObject.CreateFromIdAsync(&type: Pnp_PnpObjectType; id: HSTRING; requestedProperties: IIterable_1__HSTRING): IAsyncOperation_1__Pnp_IPnpObject;
begin
  Result := Statics.CreateFromIdAsync(&type, id, requestedProperties);
end;

class function TPnp_PnpObject.FindAllAsync(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING): IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject;
begin
  Result := Statics.FindAllAsync(&type, requestedProperties);
end;

class function TPnp_PnpObject.FindAllAsync(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING; aqsFilter: HSTRING): IAsyncOperation_1__IVectorView_1__Pnp_IPnpObject;
begin
  Result := Statics.FindAllAsync(&type, requestedProperties, aqsFilter);
end;

class function TPnp_PnpObject.CreateWatcher(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING): Pnp_IPnpObjectWatcher;
begin
  Result := Statics.CreateWatcher(&type, requestedProperties);
end;

class function TPnp_PnpObject.CreateWatcher(&type: Pnp_PnpObjectType; requestedProperties: IIterable_1__HSTRING; aqsFilter: HSTRING): Pnp_IPnpObjectWatcher;
begin
  Result := Statics.CreateWatcher(&type, requestedProperties, aqsFilter);
end;


end.
