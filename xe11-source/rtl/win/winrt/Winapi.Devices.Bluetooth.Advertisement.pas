{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Devices.Bluetooth.Advertisement;

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
  Winapi.ApplicationModel, 
  Winapi.Devices.Bluetooth, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.Foundation.IReference`1<Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementFlags>
  IReference_1__BluetoothLEAdvertisementFlags = interface;
  PIReference_1__BluetoothLEAdvertisementFlags = ^IReference_1__BluetoothLEAdvertisementFlags;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData
  IBluetoothLEManufacturerData = interface;
  PIBluetoothLEManufacturerData = ^IBluetoothLEManufacturerData;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IIterator_1__IBluetoothLEManufacturerData = interface;
  PIIterator_1__IBluetoothLEManufacturerData = ^IIterator_1__IBluetoothLEManufacturerData;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IIterable_1__IBluetoothLEManufacturerData = interface;
  PIIterable_1__IBluetoothLEManufacturerData = ^IIterable_1__IBluetoothLEManufacturerData;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IVectorView_1__IBluetoothLEManufacturerData = interface;
  PIVectorView_1__IBluetoothLEManufacturerData = ^IVectorView_1__IBluetoothLEManufacturerData;

  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IVector_1__IBluetoothLEManufacturerData = interface;
  PIVector_1__IBluetoothLEManufacturerData = ^IVector_1__IBluetoothLEManufacturerData;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection
  IBluetoothLEAdvertisementDataSection = interface;
  PIBluetoothLEAdvertisementDataSection = ^IBluetoothLEAdvertisementDataSection;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IIterator_1__IBluetoothLEAdvertisementDataSection = interface;
  PIIterator_1__IBluetoothLEAdvertisementDataSection = ^IIterator_1__IBluetoothLEAdvertisementDataSection;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IIterable_1__IBluetoothLEAdvertisementDataSection = interface;
  PIIterable_1__IBluetoothLEAdvertisementDataSection = ^IIterable_1__IBluetoothLEAdvertisementDataSection;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IVectorView_1__IBluetoothLEAdvertisementDataSection = interface;
  PIVectorView_1__IBluetoothLEAdvertisementDataSection = ^IVectorView_1__IBluetoothLEAdvertisementDataSection;

  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IVector_1__IBluetoothLEAdvertisementDataSection = interface;
  PIVector_1__IBluetoothLEAdvertisementDataSection = ^IVector_1__IBluetoothLEAdvertisementDataSection;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisement
  IBluetoothLEAdvertisement = interface;
  PIBluetoothLEAdvertisement = ^IBluetoothLEAdvertisement;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern
  IBluetoothLEAdvertisementBytePattern = interface;
  PIBluetoothLEAdvertisementBytePattern = ^IBluetoothLEAdvertisementBytePattern;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IIterator_1__IBluetoothLEAdvertisementBytePattern = interface;
  PIIterator_1__IBluetoothLEAdvertisementBytePattern = ^IIterator_1__IBluetoothLEAdvertisementBytePattern;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IIterable_1__IBluetoothLEAdvertisementBytePattern = interface;
  PIIterable_1__IBluetoothLEAdvertisementBytePattern = ^IIterable_1__IBluetoothLEAdvertisementBytePattern;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IVectorView_1__IBluetoothLEAdvertisementBytePattern = interface;
  PIVectorView_1__IBluetoothLEAdvertisementBytePattern = ^IVectorView_1__IBluetoothLEAdvertisementBytePattern;

  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IVector_1__IBluetoothLEAdvertisementBytePattern = interface;
  PIVector_1__IBluetoothLEAdvertisementBytePattern = ^IVector_1__IBluetoothLEAdvertisementBytePattern;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementFilter
  IBluetoothLEAdvertisementFilter = interface;
  PIBluetoothLEAdvertisementFilter = ^IBluetoothLEAdvertisementFilter;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePatternFactory
  IBluetoothLEAdvertisementBytePatternFactory = interface;
  PIBluetoothLEAdvertisementBytePatternFactory = ^IBluetoothLEAdvertisementBytePatternFactory;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSectionFactory
  IBluetoothLEAdvertisementDataSectionFactory = interface;
  PIBluetoothLEAdvertisementDataSectionFactory = ^IBluetoothLEAdvertisementDataSectionFactory;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataTypesStatics
  IBluetoothLEAdvertisementDataTypesStatics = interface;
  PIBluetoothLEAdvertisementDataTypesStatics = ^IBluetoothLEAdvertisementDataTypesStatics;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherStatusChangedEventArgs
  IBluetoothLEAdvertisementPublisherStatusChangedEventArgs = interface;
  PIBluetoothLEAdvertisementPublisherStatusChangedEventArgs = ^IBluetoothLEAdvertisementPublisherStatusChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherStatusChangedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementPublisher__IBluetoothLEAdvertisementPublisherStatusChangedEventArgs = interface;
  PTypedEventHandler_2__IBluetoothLEAdvertisementPublisher__IBluetoothLEAdvertisementPublisherStatusChangedEventArgs = ^TypedEventHandler_2__IBluetoothLEAdvertisementPublisher__IBluetoothLEAdvertisementPublisherStatusChangedEventArgs;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher
  IBluetoothLEAdvertisementPublisher = interface;
  PIBluetoothLEAdvertisementPublisher = ^IBluetoothLEAdvertisementPublisher;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher2
  IBluetoothLEAdvertisementPublisher2 = interface;
  PIBluetoothLEAdvertisementPublisher2 = ^IBluetoothLEAdvertisementPublisher2;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherFactory
  IBluetoothLEAdvertisementPublisherFactory = interface;
  PIBluetoothLEAdvertisementPublisherFactory = ^IBluetoothLEAdvertisementPublisherFactory;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherStatusChangedEventArgs2
  IBluetoothLEAdvertisementPublisherStatusChangedEventArgs2 = interface;
  PIBluetoothLEAdvertisementPublisherStatusChangedEventArgs2 = ^IBluetoothLEAdvertisementPublisherStatusChangedEventArgs2;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs
  IBluetoothLEAdvertisementReceivedEventArgs = interface;
  PIBluetoothLEAdvertisementReceivedEventArgs = ^IBluetoothLEAdvertisementReceivedEventArgs;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs2
  IBluetoothLEAdvertisementReceivedEventArgs2 = interface;
  PIBluetoothLEAdvertisementReceivedEventArgs2 = ^IBluetoothLEAdvertisementReceivedEventArgs2;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs = interface;
  PTypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs = ^TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherStoppedEventArgs
  IBluetoothLEAdvertisementWatcherStoppedEventArgs = interface;
  PIBluetoothLEAdvertisementWatcherStoppedEventArgs = ^IBluetoothLEAdvertisementWatcherStoppedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherStoppedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementWatcherStoppedEventArgs = interface;
  PTypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementWatcherStoppedEventArgs = ^TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementWatcherStoppedEventArgs;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher
  IBluetoothLEAdvertisementWatcher = interface;
  PIBluetoothLEAdvertisementWatcher = ^IBluetoothLEAdvertisementWatcher;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher2
  IBluetoothLEAdvertisementWatcher2 = interface;
  PIBluetoothLEAdvertisementWatcher2 = ^IBluetoothLEAdvertisementWatcher2;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherFactory
  IBluetoothLEAdvertisementWatcherFactory = interface;
  PIBluetoothLEAdvertisementWatcherFactory = ^IBluetoothLEAdvertisementWatcherFactory;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerDataFactory
  IBluetoothLEManufacturerDataFactory = interface;
  PIBluetoothLEManufacturerDataFactory = ^IBluetoothLEManufacturerDataFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IIterator_1__IBluetoothLEAdvertisementReceivedEventArgs = interface;
  PIIterator_1__IBluetoothLEAdvertisementReceivedEventArgs = ^IIterator_1__IBluetoothLEAdvertisementReceivedEventArgs;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IIterable_1__IBluetoothLEAdvertisementReceivedEventArgs = interface;
  PIIterable_1__IBluetoothLEAdvertisementReceivedEventArgs = ^IIterable_1__IBluetoothLEAdvertisementReceivedEventArgs;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IVectorView_1__IBluetoothLEAdvertisementReceivedEventArgs = interface;
  PIVectorView_1__IBluetoothLEAdvertisementReceivedEventArgs = ^IVectorView_1__IBluetoothLEAdvertisementReceivedEventArgs;

  // Windows.Devices.Bluetooth.Advertisement Enums

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementFlags
  BluetoothLEAdvertisementFlags = (
    None = 0,
    LimitedDiscoverableMode = 1,
    GeneralDiscoverableMode = 2,
    ClassicNotSupported = 4,
    DualModeControllerCapable = 8,
    DualModeHostCapable = 16
  );
  PBluetoothLEAdvertisementFlags = ^BluetoothLEAdvertisementFlags;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementPublisherStatus
  BluetoothLEAdvertisementPublisherStatus = (
    Created = 0,
    Waiting = 1,
    Started = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PBluetoothLEAdvertisementPublisherStatus = ^BluetoothLEAdvertisementPublisherStatus;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementType
  BluetoothLEAdvertisementType = (
    ConnectableUndirected = 0,
    ConnectableDirected = 1,
    ScannableUndirected = 2,
    NonConnectableUndirected = 3,
    ScanResponse = 4,
    Extended = 5
  );
  PBluetoothLEAdvertisementType = ^BluetoothLEAdvertisementType;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementWatcherStatus
  BluetoothLEAdvertisementWatcherStatus = (
    Created = 0,
    Started = 1,
    Stopping = 2,
    Stopped = 3,
    Aborted = 4
  );
  PBluetoothLEAdvertisementWatcherStatus = ^BluetoothLEAdvertisementWatcherStatus;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEScanningMode
  BluetoothLEScanningMode = (
    Passive = 0,
    Active = 1,
    None = 2
  );
  PBluetoothLEScanningMode = ^BluetoothLEScanningMode;

  // Windows.Devices.Bluetooth.Advertisement Interfaces

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementFlags>
  IReference_1__BluetoothLEAdvertisementFlags = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: BluetoothLEAdvertisementFlags; safecall;
    property Value: BluetoothLEAdvertisementFlags read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEManufacturerData)]
  IBluetoothLEManufacturerData = interface(IInspectable)
  ['{912DBA18-6963-4533-B061-4694DAFB34E5}']
    function get_CompanyId: Word; safecall;
    procedure put_CompanyId(value: Word); safecall;
    function get_Data: IBuffer; safecall;
    procedure put_Data(value: IBuffer); safecall;
    property CompanyId: Word read get_CompanyId write put_CompanyId;
    property Data: IBuffer read get_Data write put_Data;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IIterator_1__IBluetoothLEManufacturerData_Base = interface(IInspectable)
  ['{12F158DD-7016-5338-AC5C-7D5503D73274}']
    function get_Current: IBluetoothLEManufacturerData; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIBluetoothLEManufacturerData): Cardinal; safecall;
    property Current: IBluetoothLEManufacturerData read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IIterator_1__IBluetoothLEManufacturerData = interface(IIterator_1__IBluetoothLEManufacturerData_Base)
  ['{1AFC1F61-BE0F-5D68-A1B5-002CA2D6339F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IIterable_1__IBluetoothLEManufacturerData_Base = interface(IInspectable)
  ['{834A4CAC-BB8B-5F0F-9F28-4DBC98C17907}']
    function First: IIterator_1__IBluetoothLEManufacturerData; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IIterable_1__IBluetoothLEManufacturerData = interface(IIterable_1__IBluetoothLEManufacturerData_Base)
  ['{BB452D0A-A575-5A1A-B77F-4938C6FF9A09}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IVectorView_1__IBluetoothLEManufacturerData = interface(IInspectable)
  ['{362D1A87-BBD6-5B43-A0B8-139A1F132948}']
    function GetAt(index: Cardinal): IBluetoothLEManufacturerData; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IBluetoothLEManufacturerData; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBluetoothLEManufacturerData): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IVector_1__IBluetoothLEManufacturerData_Base = interface(IInspectable)
  ['{52D75B45-1D24-5EEB-BABB-65EFFAE45E46}']
    function GetAt(index: Cardinal): IBluetoothLEManufacturerData; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IBluetoothLEManufacturerData; safecall;
    function IndexOf(value: IBluetoothLEManufacturerData; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IBluetoothLEManufacturerData); safecall;
    procedure InsertAt(index: Cardinal; value: IBluetoothLEManufacturerData); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IBluetoothLEManufacturerData); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBluetoothLEManufacturerData): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIBluetoothLEManufacturerData); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData>
  IVector_1__IBluetoothLEManufacturerData = interface(IVector_1__IBluetoothLEManufacturerData_Base)
  ['{A18B4E27-ABEE-502A-B326-87787C366F13}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementDataSection)]
  IBluetoothLEAdvertisementDataSection = interface(IInspectable)
  ['{D7213314-3A43-40F9-B6F0-92BFEFC34AE3}']
    function get_DataType: Byte; safecall;
    procedure put_DataType(value: Byte); safecall;
    function get_Data: IBuffer; safecall;
    procedure put_Data(value: IBuffer); safecall;
    property Data: IBuffer read get_Data write put_Data;
    property DataType: Byte read get_DataType write put_DataType;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IIterator_1__IBluetoothLEAdvertisementDataSection_Base = interface(IInspectable)
  ['{AF5C0E81-788B-52D4-82A2-1ED28C66A05E}']
    function get_Current: IBluetoothLEAdvertisementDataSection; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIBluetoothLEAdvertisementDataSection): Cardinal; safecall;
    property Current: IBluetoothLEAdvertisementDataSection read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IIterator_1__IBluetoothLEAdvertisementDataSection = interface(IIterator_1__IBluetoothLEAdvertisementDataSection_Base)
  ['{28C6C337-33E6-5B6B-A8C0-CB101A838EFE}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IIterable_1__IBluetoothLEAdvertisementDataSection_Base = interface(IInspectable)
  ['{DB98B5D1-897E-59CC-B86A-7B8855AC98AF}']
    function First: IIterator_1__IBluetoothLEAdvertisementDataSection; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IIterable_1__IBluetoothLEAdvertisementDataSection = interface(IIterable_1__IBluetoothLEAdvertisementDataSection_Base)
  ['{F68192BA-2B18-58A5-A3BA-1F874813B0B2}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IVectorView_1__IBluetoothLEAdvertisementDataSection = interface(IInspectable)
  ['{6619200D-D012-530E-A769-1B060E574B9A}']
    function GetAt(index: Cardinal): IBluetoothLEAdvertisementDataSection; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IBluetoothLEAdvertisementDataSection; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBluetoothLEAdvertisementDataSection): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IVector_1__IBluetoothLEAdvertisementDataSection_Base = interface(IInspectable)
  ['{B6F71AD2-E2CF-5D54-B6F1-90964EE5D4DA}']
    function GetAt(index: Cardinal): IBluetoothLEAdvertisementDataSection; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IBluetoothLEAdvertisementDataSection; safecall;
    function IndexOf(value: IBluetoothLEAdvertisementDataSection; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IBluetoothLEAdvertisementDataSection); safecall;
    procedure InsertAt(index: Cardinal; value: IBluetoothLEAdvertisementDataSection); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IBluetoothLEAdvertisementDataSection); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBluetoothLEAdvertisementDataSection): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIBluetoothLEAdvertisementDataSection); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection>
  IVector_1__IBluetoothLEAdvertisementDataSection = interface(IVector_1__IBluetoothLEAdvertisementDataSection_Base)
  ['{27485048-DEEF-57A1-8FF9-90B75BF5E3BD}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisement
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisement)]
  IBluetoothLEAdvertisement = interface(IInspectable)
  ['{066FB2B7-33D1-4E7D-8367-CF81D0F79653}']
    function get_Flags: IReference_1__BluetoothLEAdvertisementFlags; safecall;
    procedure put_Flags(value: IReference_1__BluetoothLEAdvertisementFlags); safecall;
    function get_LocalName: HSTRING; safecall;
    procedure put_LocalName(value: HSTRING); safecall;
    function get_ServiceUuids: IVector_1__TGuid; safecall;
    function get_ManufacturerData: IVector_1__IBluetoothLEManufacturerData; safecall;
    function get_DataSections: IVector_1__IBluetoothLEAdvertisementDataSection; safecall;
    function GetManufacturerDataByCompanyId(companyId: Word): IVectorView_1__IBluetoothLEManufacturerData; safecall;
    function GetSectionsByType(&type: Byte): IVectorView_1__IBluetoothLEAdvertisementDataSection; safecall;
    property DataSections: IVector_1__IBluetoothLEAdvertisementDataSection read get_DataSections;
    property Flags: IReference_1__BluetoothLEAdvertisementFlags read get_Flags write put_Flags;
    property LocalName: HSTRING read get_LocalName write put_LocalName;
    property ManufacturerData: IVector_1__IBluetoothLEManufacturerData read get_ManufacturerData;
    property ServiceUuids: IVector_1__TGuid read get_ServiceUuids;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementBytePattern)]
  IBluetoothLEAdvertisementBytePattern = interface(IInspectable)
  ['{FBFAD7F2-B9C5-4A08-BC51-502F8EF68A79}']
    function get_DataType: Byte; safecall;
    procedure put_DataType(value: Byte); safecall;
    function get_Offset: SmallInt; safecall;
    procedure put_Offset(value: SmallInt); safecall;
    function get_Data: IBuffer; safecall;
    procedure put_Data(value: IBuffer); safecall;
    property Data: IBuffer read get_Data write put_Data;
    property DataType: Byte read get_DataType write put_DataType;
    property Offset: SmallInt read get_Offset write put_Offset;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IIterator_1__IBluetoothLEAdvertisementBytePattern_Base = interface(IInspectable)
  ['{B33E103A-1A61-5107-8813-C0E905C05486}']
    function get_Current: IBluetoothLEAdvertisementBytePattern; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIBluetoothLEAdvertisementBytePattern): Cardinal; safecall;
    property Current: IBluetoothLEAdvertisementBytePattern read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IIterator_1__IBluetoothLEAdvertisementBytePattern = interface(IIterator_1__IBluetoothLEAdvertisementBytePattern_Base)
  ['{780018BB-FD2E-5FC9-AB41-844D7405DB8A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IIterable_1__IBluetoothLEAdvertisementBytePattern_Base = interface(IInspectable)
  ['{1E3FADEE-54AC-538B-8777-351AFB78CB74}']
    function First: IIterator_1__IBluetoothLEAdvertisementBytePattern; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IIterable_1__IBluetoothLEAdvertisementBytePattern = interface(IIterable_1__IBluetoothLEAdvertisementBytePattern_Base)
  ['{EB205564-A2C2-591F-AAB5-82EAEBFBC6DA}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IVectorView_1__IBluetoothLEAdvertisementBytePattern = interface(IInspectable)
  ['{D57CF71B-0B53-5F7B-A0D9-4690FB2588C7}']
    function GetAt(index: Cardinal): IBluetoothLEAdvertisementBytePattern; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IBluetoothLEAdvertisementBytePattern; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBluetoothLEAdvertisementBytePattern): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IVector_1__IBluetoothLEAdvertisementBytePattern_Base = interface(IInspectable)
  ['{8DD461B7-9775-5E82-A0A6-6627ABD0D010}']
    function GetAt(index: Cardinal): IBluetoothLEAdvertisementBytePattern; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IBluetoothLEAdvertisementBytePattern; safecall;
    function IndexOf(value: IBluetoothLEAdvertisementBytePattern; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IBluetoothLEAdvertisementBytePattern); safecall;
    procedure InsertAt(index: Cardinal; value: IBluetoothLEAdvertisementBytePattern); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IBluetoothLEAdvertisementBytePattern); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBluetoothLEAdvertisementBytePattern): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIBluetoothLEAdvertisementBytePattern); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern>
  IVector_1__IBluetoothLEAdvertisementBytePattern = interface(IVector_1__IBluetoothLEAdvertisementBytePattern_Base)
  ['{3E74F04D-A6C9-5DC1-85B1-CE32D80C638A}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementFilter
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementFilter)]
  IBluetoothLEAdvertisementFilter = interface(IInspectable)
  ['{131EB0D3-D04E-47B1-837E-49405BF6F80F}']
    function get_Advertisement: IBluetoothLEAdvertisement; safecall;
    procedure put_Advertisement(value: IBluetoothLEAdvertisement); safecall;
    function get_BytePatterns: IVector_1__IBluetoothLEAdvertisementBytePattern; safecall;
    property Advertisement: IBluetoothLEAdvertisement read get_Advertisement write put_Advertisement;
    property BytePatterns: IVector_1__IBluetoothLEAdvertisementBytePattern read get_BytePatterns;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePatternFactory
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementBytePattern)]
  IBluetoothLEAdvertisementBytePatternFactory = interface(IInspectable)
  ['{C2E24D73-FD5C-4EC3-BE2A-9CA6FA11B7BD}']
    function Create(dataType: Byte; offset: SmallInt; data: IBuffer): IBluetoothLEAdvertisementBytePattern; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSectionFactory
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementDataSection)]
  IBluetoothLEAdvertisementDataSectionFactory = interface(IInspectable)
  ['{E7A40942-A845-4045-BF7E-3E9971DB8A6B}']
    function Create(dataType: Byte; data: IBuffer): IBluetoothLEAdvertisementDataSection; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataTypesStatics
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementDataTypes)]
  IBluetoothLEAdvertisementDataTypesStatics = interface(IInspectable)
  ['{3BB6472F-0606-434B-A76E-74159F0684D3}']
    function get_Flags: Byte; safecall;
    function get_IncompleteService16BitUuids: Byte; safecall;
    function get_CompleteService16BitUuids: Byte; safecall;
    function get_IncompleteService32BitUuids: Byte; safecall;
    function get_CompleteService32BitUuids: Byte; safecall;
    function get_IncompleteService128BitUuids: Byte; safecall;
    function get_CompleteService128BitUuids: Byte; safecall;
    function get_ShortenedLocalName: Byte; safecall;
    function get_CompleteLocalName: Byte; safecall;
    function get_TxPowerLevel: Byte; safecall;
    function get_SlaveConnectionIntervalRange: Byte; safecall;
    function get_ServiceSolicitation16BitUuids: Byte; safecall;
    function get_ServiceSolicitation32BitUuids: Byte; safecall;
    function get_ServiceSolicitation128BitUuids: Byte; safecall;
    function get_ServiceData16BitUuids: Byte; safecall;
    function get_ServiceData32BitUuids: Byte; safecall;
    function get_ServiceData128BitUuids: Byte; safecall;
    function get_PublicTargetAddress: Byte; safecall;
    function get_RandomTargetAddress: Byte; safecall;
    function get_Appearance: Byte; safecall;
    function get_AdvertisingInterval: Byte; safecall;
    function get_ManufacturerSpecificData: Byte; safecall;
    property AdvertisingInterval: Byte read get_AdvertisingInterval;
    property Appearance: Byte read get_Appearance;
    property CompleteLocalName: Byte read get_CompleteLocalName;
    property CompleteService128BitUuids: Byte read get_CompleteService128BitUuids;
    property CompleteService16BitUuids: Byte read get_CompleteService16BitUuids;
    property CompleteService32BitUuids: Byte read get_CompleteService32BitUuids;
    property Flags: Byte read get_Flags;
    property IncompleteService128BitUuids: Byte read get_IncompleteService128BitUuids;
    property IncompleteService16BitUuids: Byte read get_IncompleteService16BitUuids;
    property IncompleteService32BitUuids: Byte read get_IncompleteService32BitUuids;
    property ManufacturerSpecificData: Byte read get_ManufacturerSpecificData;
    property PublicTargetAddress: Byte read get_PublicTargetAddress;
    property RandomTargetAddress: Byte read get_RandomTargetAddress;
    property ServiceData128BitUuids: Byte read get_ServiceData128BitUuids;
    property ServiceData16BitUuids: Byte read get_ServiceData16BitUuids;
    property ServiceData32BitUuids: Byte read get_ServiceData32BitUuids;
    property ServiceSolicitation128BitUuids: Byte read get_ServiceSolicitation128BitUuids;
    property ServiceSolicitation16BitUuids: Byte read get_ServiceSolicitation16BitUuids;
    property ServiceSolicitation32BitUuids: Byte read get_ServiceSolicitation32BitUuids;
    property ShortenedLocalName: Byte read get_ShortenedLocalName;
    property SlaveConnectionIntervalRange: Byte read get_SlaveConnectionIntervalRange;
    property TxPowerLevel: Byte read get_TxPowerLevel;
  end;

  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherStatusChangedEventArgs
  IBluetoothLEAdvertisementPublisherStatusChangedEventArgs = interface(IInspectable)
  ['{09C2BD9F-2DFF-4B23-86EE-0D14FB94AEAE}']
    function get_Status: BluetoothLEAdvertisementPublisherStatus; safecall;
    function get_Error: BluetoothError; safecall;
    property Error: BluetoothError read get_Error;
    property Status: BluetoothLEAdvertisementPublisherStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherStatusChangedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementPublisher__IBluetoothLEAdvertisementPublisherStatusChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{C2FFA4F1-5893-54A8-BD94-AA1198B05D07}']
    procedure Invoke(sender: IBluetoothLEAdvertisementPublisher; args: IBluetoothLEAdvertisementPublisherStatusChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherStatusChangedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementPublisher__IBluetoothLEAdvertisementPublisherStatusChangedEventArgs = interface(TypedEventHandler_2__IBluetoothLEAdvertisementPublisher__IBluetoothLEAdvertisementPublisherStatusChangedEventArgs_Delegate_Base)
  ['{06B838A8-7B0F-536E-8726-131A41D9D97E}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementPublisher)]
  IBluetoothLEAdvertisementPublisher = interface(IInspectable)
  ['{CDE820F9-D9FA-43D6-A264-DDD8B7DA8B78}']
    function get_Status: BluetoothLEAdvertisementPublisherStatus; safecall;
    function get_Advertisement: IBluetoothLEAdvertisement; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    function add_StatusChanged(handler: TypedEventHandler_2__IBluetoothLEAdvertisementPublisher__IBluetoothLEAdvertisementPublisherStatusChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StatusChanged(token: EventRegistrationToken); safecall;
    property Advertisement: IBluetoothLEAdvertisement read get_Advertisement;
    property Status: BluetoothLEAdvertisementPublisherStatus read get_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher2
  IBluetoothLEAdvertisementPublisher2 = interface(IInspectable)
  ['{FBDB545E-56F1-510F-A434-217FBD9E7BD2}']
    function get_PreferredTransmitPowerLevelInDBm: IReference_1__SmallInt; safecall;
    procedure put_PreferredTransmitPowerLevelInDBm(value: IReference_1__SmallInt); safecall;
    function get_UseExtendedAdvertisement: Boolean; safecall;
    procedure put_UseExtendedAdvertisement(value: Boolean); safecall;
    function get_IsAnonymous: Boolean; safecall;
    procedure put_IsAnonymous(value: Boolean); safecall;
    function get_IncludeTransmitPowerLevel: Boolean; safecall;
    procedure put_IncludeTransmitPowerLevel(value: Boolean); safecall;
    property IncludeTransmitPowerLevel: Boolean read get_IncludeTransmitPowerLevel write put_IncludeTransmitPowerLevel;
    property IsAnonymous: Boolean read get_IsAnonymous write put_IsAnonymous;
    property PreferredTransmitPowerLevelInDBm: IReference_1__SmallInt read get_PreferredTransmitPowerLevelInDBm write put_PreferredTransmitPowerLevelInDBm;
    property UseExtendedAdvertisement: Boolean read get_UseExtendedAdvertisement write put_UseExtendedAdvertisement;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherFactory
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementPublisher)]
  IBluetoothLEAdvertisementPublisherFactory = interface(IInspectable)
  ['{5C5F065E-B863-4981-A1AF-1C544D8B0C0D}']
    function Create(advertisement: IBluetoothLEAdvertisement): IBluetoothLEAdvertisementPublisher; safecall;
  end;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherStatusChangedEventArgs2
  IBluetoothLEAdvertisementPublisherStatusChangedEventArgs2 = interface(IInspectable)
  ['{8F62790E-DC88-5C8B-B34E-10B321850F88}']
    function get_SelectedTransmitPowerLevelInDBm: IReference_1__SmallInt; safecall;
    property SelectedTransmitPowerLevelInDBm: IReference_1__SmallInt read get_SelectedTransmitPowerLevelInDBm;
  end;

  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs
  IBluetoothLEAdvertisementReceivedEventArgs = interface(IInspectable)
  ['{27987DDF-E596-41BE-8D43-9E6731D4A913}']
    function get_RawSignalStrengthInDBm: SmallInt; safecall;
    function get_BluetoothAddress: UInt64; safecall;
    function get_AdvertisementType: BluetoothLEAdvertisementType; safecall;
    function get_Timestamp: DateTime; safecall;
    function get_Advertisement: IBluetoothLEAdvertisement; safecall;
    property Advertisement: IBluetoothLEAdvertisement read get_Advertisement;
    property AdvertisementType: BluetoothLEAdvertisementType read get_AdvertisementType;
    property BluetoothAddress: UInt64 read get_BluetoothAddress;
    property RawSignalStrengthInDBm: SmallInt read get_RawSignalStrengthInDBm;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs2
  IBluetoothLEAdvertisementReceivedEventArgs2 = interface(IInspectable)
  ['{12D9C87B-0399-5F0E-A348-53B02B6B162E}']
    function get_BluetoothAddressType: BluetoothAddressType; safecall;
    function get_TransmitPowerLevelInDBm: IReference_1__SmallInt; safecall;
    function get_IsAnonymous: Boolean; safecall;
    function get_IsConnectable: Boolean; safecall;
    function get_IsScannable: Boolean; safecall;
    function get_IsDirected: Boolean; safecall;
    function get_IsScanResponse: Boolean; safecall;
    property BluetoothAddressType_: BluetoothAddressType read get_BluetoothAddressType;
    property IsAnonymous: Boolean read get_IsAnonymous;
    property IsConnectable: Boolean read get_IsConnectable;
    property IsDirected: Boolean read get_IsDirected;
    property IsScanResponse: Boolean read get_IsScanResponse;
    property IsScannable: Boolean read get_IsScannable;
    property TransmitPowerLevelInDBm: IReference_1__SmallInt read get_TransmitPowerLevelInDBm;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{90EB4ECA-D465-5EA0-A61C-033C8C5ECEF2}']
    procedure Invoke(sender: IBluetoothLEAdvertisementWatcher; args: IBluetoothLEAdvertisementReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs = interface(TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs_Delegate_Base)
  ['{FD2452E0-5167-53D2-BADA-405DDF441262}']
  end;

  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherStoppedEventArgs
  IBluetoothLEAdvertisementWatcherStoppedEventArgs = interface(IInspectable)
  ['{DD40F84D-E7B9-43E3-9C04-0685D085FD8C}']
    function get_Error: BluetoothError; safecall;
    property Error: BluetoothError read get_Error;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherStoppedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementWatcherStoppedEventArgs_Delegate_Base = interface(IUnknown)
  ['{9936A4DB-DC99-55C3-9E9B-BF4854BD9EAB}']
    procedure Invoke(sender: IBluetoothLEAdvertisementWatcher; args: IBluetoothLEAdvertisementWatcherStoppedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher,Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherStoppedEventArgs>
  TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementWatcherStoppedEventArgs = interface(TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementWatcherStoppedEventArgs_Delegate_Base)
  ['{859B97EB-2DFE-59AD-AD1B-3C93C7512179}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementWatcher)]
  IBluetoothLEAdvertisementWatcher = interface(IInspectable)
  ['{A6AC336F-F3D3-4297-8D6C-C81EA6623F40}']
    function get_MinSamplingInterval: TimeSpan; safecall;
    function get_MaxSamplingInterval: TimeSpan; safecall;
    function get_MinOutOfRangeTimeout: TimeSpan; safecall;
    function get_MaxOutOfRangeTimeout: TimeSpan; safecall;
    function get_Status: BluetoothLEAdvertisementWatcherStatus; safecall;
    function get_ScanningMode: BluetoothLEScanningMode; safecall;
    procedure put_ScanningMode(value: BluetoothLEScanningMode); safecall;
    function get_SignalStrengthFilter: IBluetoothSignalStrengthFilter; safecall;
    procedure put_SignalStrengthFilter(value: IBluetoothSignalStrengthFilter); safecall;
    function get_AdvertisementFilter: IBluetoothLEAdvertisementFilter; safecall;
    procedure put_AdvertisementFilter(value: IBluetoothLEAdvertisementFilter); safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    function add_Received(handler: TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Received(token: EventRegistrationToken); safecall;
    function add_Stopped(handler: TypedEventHandler_2__IBluetoothLEAdvertisementWatcher__IBluetoothLEAdvertisementWatcherStoppedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Stopped(token: EventRegistrationToken); safecall;
    property AdvertisementFilter: IBluetoothLEAdvertisementFilter read get_AdvertisementFilter write put_AdvertisementFilter;
    property MaxOutOfRangeTimeout: TimeSpan read get_MaxOutOfRangeTimeout;
    property MaxSamplingInterval: TimeSpan read get_MaxSamplingInterval;
    property MinOutOfRangeTimeout: TimeSpan read get_MinOutOfRangeTimeout;
    property MinSamplingInterval: TimeSpan read get_MinSamplingInterval;
    property ScanningMode: BluetoothLEScanningMode read get_ScanningMode write put_ScanningMode;
    property SignalStrengthFilter: IBluetoothSignalStrengthFilter read get_SignalStrengthFilter write put_SignalStrengthFilter;
    property Status: BluetoothLEAdvertisementWatcherStatus read get_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher2
  IBluetoothLEAdvertisementWatcher2 = interface(IInspectable)
  ['{01BF26BC-B164-5805-90A3-E8A7997FF225}']
    function get_AllowExtendedAdvertisements: Boolean; safecall;
    procedure put_AllowExtendedAdvertisements(value: Boolean); safecall;
    property AllowExtendedAdvertisements: Boolean read get_AllowExtendedAdvertisements write put_AllowExtendedAdvertisements;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherFactory
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementWatcher)]
  IBluetoothLEAdvertisementWatcherFactory = interface(IInspectable)
  ['{9AAF2D56-39AC-453E-B32A-85C657E017F1}']
    function Create(advertisementFilter: IBluetoothLEAdvertisementFilter): IBluetoothLEAdvertisementWatcher; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerDataFactory
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Advertisement_BluetoothLEManufacturerData)]
  IBluetoothLEManufacturerDataFactory = interface(IInspectable)
  ['{C09B39F8-319A-441E-8DE5-66A81E877A6C}']
    function Create(companyId: Word; data: IBuffer): IBluetoothLEManufacturerData; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IIterator_1__IBluetoothLEAdvertisementReceivedEventArgs_Base = interface(IInspectable)
  ['{096EDBB8-ECEF-5724-BE62-240DCFF6ACA9}']
    function get_Current: IBluetoothLEAdvertisementReceivedEventArgs; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIBluetoothLEAdvertisementReceivedEventArgs): Cardinal; safecall;
    property Current: IBluetoothLEAdvertisementReceivedEventArgs read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IIterator_1__IBluetoothLEAdvertisementReceivedEventArgs = interface(IIterator_1__IBluetoothLEAdvertisementReceivedEventArgs_Base)
  ['{F0FFB2E4-7E87-598B-9639-93EEEC8D41C6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IIterable_1__IBluetoothLEAdvertisementReceivedEventArgs_Base = interface(IInspectable)
  ['{34F6412F-8314-5205-967C-DB357C9A42A7}']
    function First: IIterator_1__IBluetoothLEAdvertisementReceivedEventArgs; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IIterable_1__IBluetoothLEAdvertisementReceivedEventArgs = interface(IIterable_1__IBluetoothLEAdvertisementReceivedEventArgs_Base)
  ['{DCC7A122-E1B2-5805-8D67-9AF550BC5824}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementReceivedEventArgs>
  IVectorView_1__IBluetoothLEAdvertisementReceivedEventArgs = interface(IInspectable)
  ['{DAAAEAA7-D307-5BF1-A0DB-0A811F07970B}']
    function GetAt(index: Cardinal): IBluetoothLEAdvertisementReceivedEventArgs; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IBluetoothLEAdvertisementReceivedEventArgs; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBluetoothLEAdvertisementReceivedEventArgs): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisement
  // DualAPI
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisement
  // Instantiable: "IBluetoothLEAdvertisement"
  TBluetoothLEAdvertisement = class(TWinRTGenericImportI<IBluetoothLEAdvertisement>) end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementBytePattern
  // DualAPI
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePattern
  // Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePatternFactory"
  // Instantiable: "IBluetoothLEAdvertisementBytePattern"
  TBluetoothLEAdvertisementBytePattern = class(TWinRTGenericImportFI<IBluetoothLEAdvertisementBytePatternFactory, IBluetoothLEAdvertisementBytePattern>)
  public
    // -> IBluetoothLEAdvertisementBytePatternFactory
    class function Create(dataType: Byte; offset: SmallInt; data: IBuffer): IBluetoothLEAdvertisementBytePattern; overload; static; inline;
  end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementDataSection
  // DualAPI
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSection
  // Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSectionFactory"
  // Instantiable: "IBluetoothLEAdvertisementDataSection"
  TBluetoothLEAdvertisementDataSection = class(TWinRTGenericImportFI<IBluetoothLEAdvertisementDataSectionFactory, IBluetoothLEAdvertisementDataSection>)
  public
    // -> IBluetoothLEAdvertisementDataSectionFactory
    class function Create(dataType: Byte; data: IBuffer): IBluetoothLEAdvertisementDataSection; overload; static; inline;
  end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementDataTypes
  // DualAPI
  // Statics: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataTypesStatics"
  TBluetoothLEAdvertisementDataTypes = class(TWinRTGenericImportS<IBluetoothLEAdvertisementDataTypesStatics>)
  public
    // -> IBluetoothLEAdvertisementDataTypesStatics
    class function get_Flags: Byte; static; inline;
    class function get_IncompleteService16BitUuids: Byte; static; inline;
    class function get_CompleteService16BitUuids: Byte; static; inline;
    class function get_IncompleteService32BitUuids: Byte; static; inline;
    class function get_CompleteService32BitUuids: Byte; static; inline;
    class function get_IncompleteService128BitUuids: Byte; static; inline;
    class function get_CompleteService128BitUuids: Byte; static; inline;
    class function get_ShortenedLocalName: Byte; static; inline;
    class function get_CompleteLocalName: Byte; static; inline;
    class function get_TxPowerLevel: Byte; static; inline;
    class function get_SlaveConnectionIntervalRange: Byte; static; inline;
    class function get_ServiceSolicitation16BitUuids: Byte; static; inline;
    class function get_ServiceSolicitation32BitUuids: Byte; static; inline;
    class function get_ServiceSolicitation128BitUuids: Byte; static; inline;
    class function get_ServiceData16BitUuids: Byte; static; inline;
    class function get_ServiceData32BitUuids: Byte; static; inline;
    class function get_ServiceData128BitUuids: Byte; static; inline;
    class function get_PublicTargetAddress: Byte; static; inline;
    class function get_RandomTargetAddress: Byte; static; inline;
    class function get_Appearance: Byte; static; inline;
    class function get_AdvertisingInterval: Byte; static; inline;
    class function get_ManufacturerSpecificData: Byte; static; inline;
    class property AdvertisingInterval: Byte read get_AdvertisingInterval;
    class property Appearance: Byte read get_Appearance;
    class property CompleteLocalName: Byte read get_CompleteLocalName;
    class property CompleteService128BitUuids: Byte read get_CompleteService128BitUuids;
    class property CompleteService16BitUuids: Byte read get_CompleteService16BitUuids;
    class property CompleteService32BitUuids: Byte read get_CompleteService32BitUuids;
    class property Flags: Byte read get_Flags;
    class property IncompleteService128BitUuids: Byte read get_IncompleteService128BitUuids;
    class property IncompleteService16BitUuids: Byte read get_IncompleteService16BitUuids;
    class property IncompleteService32BitUuids: Byte read get_IncompleteService32BitUuids;
    class property ManufacturerSpecificData: Byte read get_ManufacturerSpecificData;
    class property PublicTargetAddress: Byte read get_PublicTargetAddress;
    class property RandomTargetAddress: Byte read get_RandomTargetAddress;
    class property ServiceData128BitUuids: Byte read get_ServiceData128BitUuids;
    class property ServiceData16BitUuids: Byte read get_ServiceData16BitUuids;
    class property ServiceData32BitUuids: Byte read get_ServiceData32BitUuids;
    class property ServiceSolicitation128BitUuids: Byte read get_ServiceSolicitation128BitUuids;
    class property ServiceSolicitation16BitUuids: Byte read get_ServiceSolicitation16BitUuids;
    class property ServiceSolicitation32BitUuids: Byte read get_ServiceSolicitation32BitUuids;
    class property ShortenedLocalName: Byte read get_ShortenedLocalName;
    class property SlaveConnectionIntervalRange: Byte read get_SlaveConnectionIntervalRange;
    class property TxPowerLevel: Byte read get_TxPowerLevel;
  end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementFilter
  // DualAPI
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementFilter
  // Instantiable: "IBluetoothLEAdvertisementFilter"
  TBluetoothLEAdvertisementFilter = class(TWinRTGenericImportI<IBluetoothLEAdvertisementFilter>) end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementPublisher
  // DualAPI
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisher2
  // Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherFactory"
  // Instantiable: "IBluetoothLEAdvertisementPublisher"
  TBluetoothLEAdvertisementPublisher = class(TWinRTGenericImportFI<IBluetoothLEAdvertisementPublisherFactory, IBluetoothLEAdvertisementPublisher>)
  public
    // -> IBluetoothLEAdvertisementPublisherFactory
    class function Create(advertisement: IBluetoothLEAdvertisement): IBluetoothLEAdvertisementPublisher; overload; static; inline;
  end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEAdvertisementWatcher
  // DualAPI
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcher2
  // Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherFactory"
  // Instantiable: "IBluetoothLEAdvertisementWatcher"
  TBluetoothLEAdvertisementWatcher = class(TWinRTGenericImportFI<IBluetoothLEAdvertisementWatcherFactory, IBluetoothLEAdvertisementWatcher>)
  public
    // -> IBluetoothLEAdvertisementWatcherFactory
    class function Create(advertisementFilter: IBluetoothLEAdvertisementFilter): IBluetoothLEAdvertisementWatcher; overload; static; inline;
  end;

  // Windows.Devices.Bluetooth.Advertisement.BluetoothLEManufacturerData
  // DualAPI
  // Implements: Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerData
  // Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerDataFactory"
  // Instantiable: "IBluetoothLEManufacturerData"
  TBluetoothLEManufacturerData = class(TWinRTGenericImportFI<IBluetoothLEManufacturerDataFactory, IBluetoothLEManufacturerData>)
  public
    // -> IBluetoothLEManufacturerDataFactory
    class function Create(companyId: Word; data: IBuffer): IBluetoothLEManufacturerData; overload; static; inline;
  end;

implementation

{ TBluetoothLEAdvertisement }

{ TBluetoothLEAdvertisementBytePattern }
// Factories for : "BluetoothLEAdvertisementBytePattern"
// Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementBytePatternFactory"
// -> IBluetoothLEAdvertisementBytePatternFactory

class function TBluetoothLEAdvertisementBytePattern.Create(dataType: Byte; offset: SmallInt; data: IBuffer): IBluetoothLEAdvertisementBytePattern;
begin
  Result := Factory.Create(dataType, offset, data);
end;


{ TBluetoothLEAdvertisementDataSection }
// Factories for : "BluetoothLEAdvertisementDataSection"
// Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementDataSectionFactory"
// -> IBluetoothLEAdvertisementDataSectionFactory

class function TBluetoothLEAdvertisementDataSection.Create(dataType: Byte; data: IBuffer): IBluetoothLEAdvertisementDataSection;
begin
  Result := Factory.Create(dataType, data);
end;


{ TBluetoothLEAdvertisementDataTypes }

class function TBluetoothLEAdvertisementDataTypes.get_Flags: Byte;
begin
  Result := Statics.get_Flags;
end;

class function TBluetoothLEAdvertisementDataTypes.get_IncompleteService16BitUuids: Byte;
begin
  Result := Statics.get_IncompleteService16BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_CompleteService16BitUuids: Byte;
begin
  Result := Statics.get_CompleteService16BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_IncompleteService32BitUuids: Byte;
begin
  Result := Statics.get_IncompleteService32BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_CompleteService32BitUuids: Byte;
begin
  Result := Statics.get_CompleteService32BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_IncompleteService128BitUuids: Byte;
begin
  Result := Statics.get_IncompleteService128BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_CompleteService128BitUuids: Byte;
begin
  Result := Statics.get_CompleteService128BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ShortenedLocalName: Byte;
begin
  Result := Statics.get_ShortenedLocalName;
end;

class function TBluetoothLEAdvertisementDataTypes.get_CompleteLocalName: Byte;
begin
  Result := Statics.get_CompleteLocalName;
end;

class function TBluetoothLEAdvertisementDataTypes.get_TxPowerLevel: Byte;
begin
  Result := Statics.get_TxPowerLevel;
end;

class function TBluetoothLEAdvertisementDataTypes.get_SlaveConnectionIntervalRange: Byte;
begin
  Result := Statics.get_SlaveConnectionIntervalRange;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ServiceSolicitation16BitUuids: Byte;
begin
  Result := Statics.get_ServiceSolicitation16BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ServiceSolicitation32BitUuids: Byte;
begin
  Result := Statics.get_ServiceSolicitation32BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ServiceSolicitation128BitUuids: Byte;
begin
  Result := Statics.get_ServiceSolicitation128BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ServiceData16BitUuids: Byte;
begin
  Result := Statics.get_ServiceData16BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ServiceData32BitUuids: Byte;
begin
  Result := Statics.get_ServiceData32BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ServiceData128BitUuids: Byte;
begin
  Result := Statics.get_ServiceData128BitUuids;
end;

class function TBluetoothLEAdvertisementDataTypes.get_PublicTargetAddress: Byte;
begin
  Result := Statics.get_PublicTargetAddress;
end;

class function TBluetoothLEAdvertisementDataTypes.get_RandomTargetAddress: Byte;
begin
  Result := Statics.get_RandomTargetAddress;
end;

class function TBluetoothLEAdvertisementDataTypes.get_Appearance: Byte;
begin
  Result := Statics.get_Appearance;
end;

class function TBluetoothLEAdvertisementDataTypes.get_AdvertisingInterval: Byte;
begin
  Result := Statics.get_AdvertisingInterval;
end;

class function TBluetoothLEAdvertisementDataTypes.get_ManufacturerSpecificData: Byte;
begin
  Result := Statics.get_ManufacturerSpecificData;
end;


{ TBluetoothLEAdvertisementFilter }

{ TBluetoothLEAdvertisementPublisher }
// Factories for : "BluetoothLEAdvertisementPublisher"
// Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementPublisherFactory"
// -> IBluetoothLEAdvertisementPublisherFactory

class function TBluetoothLEAdvertisementPublisher.Create(advertisement: IBluetoothLEAdvertisement): IBluetoothLEAdvertisementPublisher;
begin
  Result := Factory.Create(advertisement);
end;


{ TBluetoothLEAdvertisementWatcher }
// Factories for : "BluetoothLEAdvertisementWatcher"
// Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEAdvertisementWatcherFactory"
// -> IBluetoothLEAdvertisementWatcherFactory

class function TBluetoothLEAdvertisementWatcher.Create(advertisementFilter: IBluetoothLEAdvertisementFilter): IBluetoothLEAdvertisementWatcher;
begin
  Result := Factory.Create(advertisementFilter);
end;


{ TBluetoothLEManufacturerData }
// Factories for : "BluetoothLEManufacturerData"
// Factory: "Windows.Devices.Bluetooth.Advertisement.IBluetoothLEManufacturerDataFactory"
// -> IBluetoothLEManufacturerDataFactory

class function TBluetoothLEManufacturerData.Create(companyId: Word; data: IBuffer): IBluetoothLEManufacturerData;
begin
  Result := Factory.Create(companyId, data);
end;


end.
