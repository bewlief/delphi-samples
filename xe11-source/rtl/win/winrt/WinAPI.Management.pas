{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Management;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.ApplicationModel, 
  Winapi.CommonTypes, 
  Winapi.Storage, 
  Winapi.Foundation, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.Management.Deployment.IPackageVolume
  Deployment_IPackageVolume = interface;
  PDeployment_IPackageVolume = ^Deployment_IPackageVolume;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Object>
  IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIKeyValuePair_2__HSTRING__IInspectable = ^IKeyValuePair_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IInspectable = ^IIterator_1__IKeyValuePair_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IInspectable = ^IIterable_1__IKeyValuePair_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IMapView`2<String,Object>
  IMapView_2__HSTRING__IInspectable = interface;
  PIMapView_2__HSTRING__IInspectable = ^IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IMap`2<String,Object>
  IMap_2__HSTRING__IInspectable = interface;
  PIMap_2__HSTRING__IInspectable = ^IMap_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  IMapChangedEventArgs_1__HSTRING = interface;
  PIMapChangedEventArgs_1__HSTRING = ^IMapChangedEventArgs_1__HSTRING;

  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable = interface;
  PMapChangedEventHandler_2__HSTRING__IInspectable = ^MapChangedEventHandler_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IObservableMap`2<String,Object>
  IObservableMap_2__HSTRING__IInspectable = interface;
  PIObservableMap_2__HSTRING__IInspectable = ^IObservableMap_2__HSTRING__IInspectable;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationProgressHandler_2__IBuffer__Cardinal = ^AsyncOperationProgressHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface;
  PIAsyncOperationWithProgress_2__IBuffer__Cardinal = ^IAsyncOperationWithProgress_2__IBuffer__Cardinal;

  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationProgressHandler_2__Cardinal__Cardinal = ^AsyncOperationProgressHandler_2__Cardinal__Cardinal;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal;

  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = interface;
  PIAsyncOperationWithProgress_2__Cardinal__Cardinal = ^IAsyncOperationWithProgress_2__Cardinal__Cardinal;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface;
  PAsyncOperationCompletedHandler_1__Boolean = ^AsyncOperationCompletedHandler_1__Boolean;

  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface;
  PIAsyncOperation_1__Boolean = ^IAsyncOperation_1__Boolean;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.IApplicationData,Object>
  TypedEventHandler_2__IApplicationData__IInspectable = interface;
  PTypedEventHandler_2__IApplicationData__IInspectable = ^TypedEventHandler_2__IApplicationData__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface;
  PIIterator_1__HSTRING = ^IIterator_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface;
  PIIterable_1__HSTRING = ^IIterable_1__HSTRING;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface;
  PIVectorView_1__HSTRING = ^IVectorView_1__HSTRING;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // Windows.Management.Deployment.IAddPackageOptions
  Deployment_IAddPackageOptions = interface;
  PDeployment_IAddPackageOptions = ^Deployment_IAddPackageOptions;

  // Windows.Management.Deployment.IDeploymentResult
  Deployment_IDeploymentResult = interface;
  PDeployment_IDeploymentResult = ^Deployment_IDeploymentResult;

  // Windows.Management.Deployment.IDeploymentResult2
  Deployment_IDeploymentResult2 = interface;
  PDeployment_IDeploymentResult2 = ^Deployment_IDeploymentResult2;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = interface;
  PAsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = ^AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = ^AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = interface;
  PIAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = ^IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress;

  // Windows.Management.Deployment.IPackageUserInformation
  Deployment_IPackageUserInformation = interface;
  PDeployment_IPackageUserInformation = ^Deployment_IPackageUserInformation;

  // Windows.Foundation.Collections.IIterator`1<Windows.Management.Deployment.IPackageUserInformation>
  IIterator_1__Deployment_IPackageUserInformation = interface;
  PIIterator_1__Deployment_IPackageUserInformation = ^IIterator_1__Deployment_IPackageUserInformation;

  // Windows.Foundation.Collections.IIterable`1<Windows.Management.Deployment.IPackageUserInformation>
  IIterable_1__Deployment_IPackageUserInformation = interface;
  PIIterable_1__Deployment_IPackageUserInformation = ^IIterable_1__Deployment_IPackageUserInformation;

  // Windows.Management.Deployment.IPackageManager
  Deployment_IPackageManager = interface;
  PDeployment_IPackageManager = ^Deployment_IPackageManager;

  // Windows.Management.Deployment.IPackageManager2
  Deployment_IPackageManager2 = interface;
  PDeployment_IPackageManager2 = ^Deployment_IPackageManager2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Management.Deployment.IPackageVolume>
  AsyncOperationCompletedHandler_1__Deployment_IPackageVolume = interface;
  PAsyncOperationCompletedHandler_1__Deployment_IPackageVolume = ^AsyncOperationCompletedHandler_1__Deployment_IPackageVolume;

  // Windows.Foundation.IAsyncOperation`1<Windows.Management.Deployment.IPackageVolume>
  IAsyncOperation_1__Deployment_IPackageVolume = interface;
  PIAsyncOperation_1__Deployment_IPackageVolume = ^IAsyncOperation_1__Deployment_IPackageVolume;

  // Windows.Foundation.Collections.IIterator`1<Windows.Management.Deployment.IPackageVolume>
  IIterator_1__Deployment_IPackageVolume = interface;
  PIIterator_1__Deployment_IPackageVolume = ^IIterator_1__Deployment_IPackageVolume;

  // Windows.Foundation.Collections.IIterable`1<Windows.Management.Deployment.IPackageVolume>
  IIterable_1__Deployment_IPackageVolume = interface;
  PIIterable_1__Deployment_IPackageVolume = ^IIterable_1__Deployment_IPackageVolume;

  // Windows.Management.Deployment.IPackageManager3
  Deployment_IPackageManager3 = interface;
  PDeployment_IPackageManager3 = ^Deployment_IPackageManager3;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>
  IVectorView_1__Deployment_IPackageVolume = interface;
  PIVectorView_1__Deployment_IPackageVolume = ^IVectorView_1__Deployment_IPackageVolume;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume = ^AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>>
  IAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume = interface;
  PIAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume = ^IAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume;

  // Windows.Management.Deployment.IPackageManager4
  Deployment_IPackageManager4 = interface;
  PDeployment_IPackageManager4 = ^Deployment_IPackageManager4;

  // Windows.Management.Deployment.IPackageManagerDebugSettings
  Deployment_IPackageManagerDebugSettings = interface;
  PDeployment_IPackageManagerDebugSettings = ^Deployment_IPackageManagerDebugSettings;

  // Windows.Management.Deployment.IPackageManager5
  Deployment_IPackageManager5 = interface;
  PDeployment_IPackageManager5 = ^Deployment_IPackageManager5;

  // Windows.Management.Deployment.IPackageManager6
  Deployment_IPackageManager6 = interface;
  PDeployment_IPackageManager6 = ^Deployment_IPackageManager6;

  // Windows.Management.Deployment.IPackageManager7
  Deployment_IPackageManager7 = interface;
  PDeployment_IPackageManager7 = ^Deployment_IPackageManager7;

  // Windows.Management.Deployment.IPackageManager8
  Deployment_IPackageManager8 = interface;
  PDeployment_IPackageManager8 = ^Deployment_IPackageManager8;

  // Windows.Management.Deployment.IStagePackageOptions
  Deployment_IStagePackageOptions = interface;
  PDeployment_IStagePackageOptions = ^Deployment_IStagePackageOptions;

  // Windows.Management.Deployment.IRegisterPackageOptions
  Deployment_IRegisterPackageOptions = interface;
  PDeployment_IRegisterPackageOptions = ^Deployment_IRegisterPackageOptions;

  // Windows.Management.Deployment.IPackageManager9
  Deployment_IPackageManager9 = interface;
  PDeployment_IPackageManager9 = ^Deployment_IPackageManager9;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt64>
  AsyncOperationCompletedHandler_1__UInt64 = interface;
  PAsyncOperationCompletedHandler_1__UInt64 = ^AsyncOperationCompletedHandler_1__UInt64;

  // Windows.Foundation.IAsyncOperation`1<UInt64>
  IAsyncOperation_1__UInt64 = interface;
  PIAsyncOperation_1__UInt64 = ^IAsyncOperation_1__UInt64;

  // Windows.Management.Deployment.IPackageVolume2
  Deployment_IPackageVolume2 = interface;
  PDeployment_IPackageVolume2 = ^Deployment_IPackageVolume2;

  // Windows.Management.Deployment.Preview.IInstalledClassicAppInfo
  Deployment_Preview_IInstalledClassicAppInfo = interface;
  PDeployment_Preview_IInstalledClassicAppInfo = ^Deployment_Preview_IInstalledClassicAppInfo;

  // Windows.Management.Deployment.Preview.IClassicAppManagerStatics
  Deployment_Preview_IClassicAppManagerStatics = interface;
  PDeployment_Preview_IClassicAppManagerStatics = ^Deployment_Preview_IClassicAppManagerStatics;

  // Windows.Management.IMdmAlert
  IMdmAlert = interface;
  PIMdmAlert = ^IMdmAlert;

  // Windows.Foundation.Collections.IIterator`1<Windows.Management.IMdmAlert>
  IIterator_1__IMdmAlert = interface;
  PIIterator_1__IMdmAlert = ^IIterator_1__IMdmAlert;

  // Windows.Foundation.Collections.IIterable`1<Windows.Management.IMdmAlert>
  IIterable_1__IMdmAlert = interface;
  PIIterable_1__IMdmAlert = ^IIterable_1__IMdmAlert;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Management.IMdmAlert>
  IVectorView_1__IMdmAlert = interface;
  PIVectorView_1__IMdmAlert = ^IVectorView_1__IMdmAlert;

  // Windows.Management.IMdmSession
  IMdmSession = interface;
  PIMdmSession = ^IMdmSession;

  // Windows.Management.IMdmSessionManagerStatics
  IMdmSessionManagerStatics = interface;
  PIMdmSessionManagerStatics = ^IMdmSessionManagerStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  AsyncOperationCompletedHandler_1__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IInspectable = ^AsyncOperationCompletedHandler_1__IInspectable;

  // Windows.Foundation.IAsyncOperation`1<Object>
  IAsyncOperation_1__IInspectable = interface;
  PIAsyncOperation_1__IInspectable = ^IAsyncOperation_1__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.Management.Policies.INamedPolicyData,Object>
  TypedEventHandler_2__Policies_INamedPolicyData__IInspectable = interface;
  PTypedEventHandler_2__Policies_INamedPolicyData__IInspectable = ^TypedEventHandler_2__Policies_INamedPolicyData__IInspectable;

  // Windows.Management.Policies.INamedPolicyData
  Policies_INamedPolicyData = interface;
  PPolicies_INamedPolicyData = ^Policies_INamedPolicyData;

  // Windows.Management.Policies.INamedPolicyStatics
  Policies_INamedPolicyStatics = interface;
  PPolicies_INamedPolicyStatics = ^Policies_INamedPolicyStatics;

  // Windows.Management.Workplace.IMdmAllowPolicyStatics
  Workplace_IMdmAllowPolicyStatics = interface;
  PWorkplace_IMdmAllowPolicyStatics = ^Workplace_IMdmAllowPolicyStatics;

  // Windows.Management.Workplace.IMdmPolicyStatics2
  Workplace_IMdmPolicyStatics2 = interface;
  PWorkplace_IMdmPolicyStatics2 = ^Workplace_IMdmPolicyStatics2;

  // Windows.Management Enums

  // Windows.Management.Deployment.AddPackageByAppInstallerOptions
  Deployment_AddPackageByAppInstallerOptions = (
    None = 0,
    InstallAllResources = 32,
    ForceTargetAppShutdown = 64,
    RequiredContentGroupOnly = 256,
    LimitToExistingPackages = 512
  );
  PDeployment_AddPackageByAppInstallerOptions = ^Deployment_AddPackageByAppInstallerOptions;

  // Windows.Management.Deployment.DeploymentOptions
  Deployment_DeploymentOptions = (
    None = 0,
    ForceApplicationShutdown = 1,
    DevelopmentMode = 2,
    InstallAllResources = 32,
    ForceTargetApplicationShutdown = 64,
    RequiredContentGroupOnly = 256,
    ForceUpdateFromAnyVersion = 262144,
    RetainFilesOnFailure = 2097152,
    StageInPlace = 4194304
  );
  PDeployment_DeploymentOptions = ^Deployment_DeploymentOptions;

  // Windows.Management.Deployment.DeploymentProgressState
  Deployment_DeploymentProgressState = (
    Queued = 0,
    Processing = 1
  );
  PDeployment_DeploymentProgressState = ^Deployment_DeploymentProgressState;

  // Windows.Management.Deployment.PackageInstallState
  Deployment_PackageInstallState = (
    NotInstalled = 0,
    Staged = 1,
    Installed = 2,
    Paused = 6
  );
  PDeployment_PackageInstallState = ^Deployment_PackageInstallState;

  // Windows.Management.Deployment.PackageState
  Deployment_PackageState = (
    Normal = 0,
    LicenseInvalid = 1,
    Modified = 2,
    Tampered = 3
  );
  PDeployment_PackageState = ^Deployment_PackageState;

  // Windows.Management.Deployment.PackageStatus
  Deployment_PackageStatus = (
    OK = 0,
    LicenseIssue = 1,
    Modified = 2,
    Tampered = 4,
    Disabled = 8
  );
  PDeployment_PackageStatus = ^Deployment_PackageStatus;

  // Windows.Management.Deployment.PackageStubPreference
  Deployment_PackageStubPreference = (
    Full = 0,
    Stub = 1
  );
  PDeployment_PackageStubPreference = ^Deployment_PackageStubPreference;

  // Windows.Management.Deployment.PackageTypes
  Deployment_PackageTypes = (
    None = 0,
    Main = 1,
    Framework = 2,
    Resource = 4,
    Bundle = 8,
    Xap = 16,
    Optional = 32,
    All = -1
  );
  PDeployment_PackageTypes = ^Deployment_PackageTypes;

  // Windows.Management.Deployment.RemovalOptions
  Deployment_RemovalOptions = (
    None = 0,
    PreserveApplicationData = 4096,
    PreserveRoamableApplicationData = 128,
    RemoveForAllUsers = 524288
  );
  PDeployment_RemovalOptions = ^Deployment_RemovalOptions;

  // Windows.Management.Deployment.StubPackageOption
  Deployment_StubPackageOption = (
    Default = 0,
    InstallFull = 1,
    InstallStub = 2,
    UsePreference = 3
  );
  PDeployment_StubPackageOption = ^Deployment_StubPackageOption;

  // Windows.Management.MdmAlertDataType
  MdmAlertDataType = (
    &String = 0,
    Base64 = 1,
    Boolean = 2,
    Integer = 3
  );
  PMdmAlertDataType = ^MdmAlertDataType;

  // Windows.Management.MdmAlertMark
  MdmAlertMark = (
    None = 0,
    Fatal = 1,
    Critical = 2,
    Warning = 3,
    Informational = 4
  );
  PMdmAlertMark = ^MdmAlertMark;

  // Windows.Management.MdmSessionState
  MdmSessionState = (
    NotStarted = 0,
    Starting = 1,
    Connecting = 2,
    Communicating = 3,
    AlertStatusAvailable = 4,
    Retrying = 5,
    Completed = 6
  );
  PMdmSessionState = ^MdmSessionState;

  // Windows.Management.Policies.NamedPolicyKind
  Policies_NamedPolicyKind = (
    Invalid = 0,
    Binary = 1,
    Boolean = 2,
    Int32 = 3,
    Int64 = 4,
    &String = 5
  );
  PPolicies_NamedPolicyKind = ^Policies_NamedPolicyKind;

  // Windows.Management.Workplace.MessagingSyncPolicy
  Workplace_MessagingSyncPolicy = (
    Disallowed = 0,
    Allowed = 1,
    Required = 2
  );
  PWorkplace_MessagingSyncPolicy = ^Workplace_MessagingSyncPolicy;

  // Windows.Management Records
  // Windows.Management.Deployment.DeploymentProgress
  Deployment_DeploymentProgress = record
    state: Deployment_DeploymentProgressState;
    percentage: Cardinal;
  end;
  PDeployment_DeploymentProgress = ^Deployment_DeploymentProgress;

  // Windows.Management.Deployment.Preview.DeploymentPreviewContract
  Deployment_Preview_DeploymentPreviewContract = record
  end;
  PDeployment_Preview_DeploymentPreviewContract = ^Deployment_Preview_DeploymentPreviewContract;

  // Windows.Management.Workplace.WorkplaceSettingsContract
  Workplace_WorkplaceSettingsContract = record
  end;
  PWorkplace_WorkplaceSettingsContract = ^Workplace_WorkplaceSettingsContract;

  // Windows.Management Interfaces

  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageVolume
  Deployment_IPackageVolume = interface(IInspectable)
  ['{CF2672C3-1A40-4450-9739-2ACE2E898853}']
    function get_IsOffline: Boolean; safecall;
    function get_IsSystemVolume: Boolean; safecall;
    function get_MountPoint: HSTRING; safecall;
    function get_Name: HSTRING; safecall;
    function get_PackageStorePath: HSTRING; safecall;
    function get_SupportsHardLinks: Boolean; safecall;
    function FindPackages: IVector_1__IPackage; overload; safecall;
    function FindPackages(packageName: HSTRING; packagePublisher: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackages(packageFamilyName: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackagesWithPackageTypes(packageTypes: Deployment_PackageTypes): IVector_1__IPackage; overload; safecall;
    function FindPackagesWithPackageTypes(packageTypes: Deployment_PackageTypes; packageName: HSTRING; packagePublisher: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackagesWithPackageTypes(packageTypes: Deployment_PackageTypes; packageFamilyName: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackage(packageFullName: HSTRING): IVector_1__IPackage; safecall;
    function FindPackagesForUser(userSecurityId: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackagesForUser(userSecurityId: HSTRING; packageName: HSTRING; packagePublisher: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackagesForUser(userSecurityId: HSTRING; packageFamilyName: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackagesForUserWithPackageTypes(userSecurityId: HSTRING; packageTypes: Deployment_PackageTypes): IVector_1__IPackage; overload; safecall;
    function FindPackagesForUserWithPackageTypes(userSecurityId: HSTRING; packageTypes: Deployment_PackageTypes; packageName: HSTRING; packagePublisher: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackagesForUserWithPackageTypes(userSecurityId: HSTRING; packageTypes: Deployment_PackageTypes; packageFamilyName: HSTRING): IVector_1__IPackage; overload; safecall;
    function FindPackageForUser(userSecurityId: HSTRING; packageFullName: HSTRING): IVector_1__IPackage; safecall;
    property IsOffline: Boolean read get_IsOffline;
    property IsSystemVolume: Boolean read get_IsSystemVolume;
    property MountPoint: HSTRING read get_MountPoint;
    property Name: HSTRING read get_Name;
    property PackageStorePath: HSTRING read get_PackageStorePath;
    property SupportsHardLinks: Boolean read get_SupportsHardLinks;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Object>
  IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{09335560-6C6B-5A26-9348-97B781132B20}']
    function get_Key: HSTRING; safecall;
    function get_Value: IInspectable; safecall;
    property Key: HSTRING read get_Key;
    property Value: IInspectable read get_Value;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{5DB5FA32-707C-5849-A06B-91C8EB9D10E8}']
    function get_Current: IKeyValuePair_2__HSTRING__IInspectable; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IInspectable): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IInspectable read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{FE2F3D47-5D47-5499-8374-430C7CDA0204}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IInspectable; safecall;
  end;

  // Windows.Foundation.Collections.IMapView`2<String,Object>
  IMapView_2__HSTRING__IInspectable = interface(IInspectable)
  ['{BB78502A-F79D-54FA-92C9-90C5039FDF7E}']
    function Lookup(key: HSTRING): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IInspectable; out second: IMapView_2__HSTRING__IInspectable); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IMap`2<String,Object>
  IMap_2__HSTRING__IInspectable = interface(IInspectable)
  ['{1B0D3570-0877-5EC2-8A2C-3B9539506ACA}']
    function Lookup(key: HSTRING): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    function GetView: IMapView_2__HSTRING__IInspectable; safecall;
    function Insert(key: HSTRING; value: IInspectable): Boolean; safecall;
    procedure Remove(key: HSTRING); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  IMapChangedEventArgs_1__HSTRING = interface(IInspectable)
  ['{60141EFB-F2F9-5377-96FD-F8C60D9558B5}']
    function get_CollectionChange: CollectionChange; safecall;
    function get_Key: HSTRING; safecall;
    property CollectionChange_: CollectionChange read get_CollectionChange;
    property Key: HSTRING read get_Key;
  end;

  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable = interface(IUnknown)
  ['{24F981E5-DDCA-538D-AADA-A59906084CF1}']
    procedure Invoke(sender: IObservableMap_2__HSTRING__IInspectable; event: IMapChangedEventArgs_1__HSTRING); safecall;
  end;

  // Windows.Foundation.Collections.IObservableMap`2<String,Object>
  IObservableMap_2__HSTRING__IInspectable = interface(IInspectable)
  ['{236AAC9D-FB12-5C4D-A41C-9E445FB4D7EC}']
    function add_MapChanged(vhnd: MapChangedEventHandler_2__HSTRING__IInspectable): EventRegistrationToken; safecall;
    procedure remove_MapChanged(token: EventRegistrationToken); safecall;
  end;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface(IUnknown)
  ['{BF666554-7605-5D9A-B14E-18D8C8472AFE}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; progressInfo: Cardinal); safecall;
  end;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface(IUnknown)
  ['{06386A7A-E009-5B0B-AB68-A8E48B516647}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface(IInspectable)
  ['{D26B2819-897F-5C7D-84D6-56D796561431}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IBuffer__Cardinal); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IBuffer__Cardinal; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal; safecall;
    function GetResults: IBuffer; safecall;
    property Progress: AsyncOperationProgressHandler_2__IBuffer__Cardinal read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal read get_Completed write put_Completed;
  end;

  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface(IUnknown)
  ['{EA0FE405-D432-5AC7-9EF8-5A65E1F97D7E}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; progressInfo: Cardinal); safecall;
  end;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface(IUnknown)
  ['{1E466DC5-840F-54F9-B877-5E3A9F4B6C74}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = interface(IInspectable)
  ['{ECCB574A-C684-5572-A679-6B0842CFB57F}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__Cardinal__Cardinal); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__Cardinal__Cardinal; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal; safecall;
    function GetResults: Cardinal; safecall;
    property Progress: AsyncOperationProgressHandler_2__Cardinal__Cardinal read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal read get_Completed write put_Completed;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface(IUnknown)
  ['{C1D3D1A2-AE17-5A5F-B5A2-BDCC8844889A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Boolean; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface(IInspectable)
  ['{CDB5EFB3-5788-509D-9BE1-71CCB8A3362A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Boolean); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Boolean; safecall;
    function GetResults: Boolean; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Boolean read get_Completed write put_Completed;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.IApplicationData,Object>
  TypedEventHandler_2__IApplicationData__IInspectable = interface(IUnknown)
  ['{4E72D889-3D9A-5E3B-AA8A-3B37D17226DC}']
    procedure Invoke(sender: IApplicationData; args: IInspectable); safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface(IInspectable)
  ['{8C304EBB-6615-50A4-8829-879ECD443236}']
    function get_Current: HSTRING; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    property Current: HSTRING read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface(IInspectable)
  ['{E2FCC7C1-3BFC-5A0B-B2B0-72E769D1CB7E}']
    function First: IIterator_1__HSTRING; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface(IInspectable)
  ['{2F13C006-A03A-5F69-B090-75A43E33423E}']
    function GetAt(index: Cardinal): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: HSTRING; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface(IInspectable)
  ['{98B9ACC1-4B56-532E-AC73-03D5291CCA90}']
    function GetAt(index: Cardinal): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__HSTRING; safecall;
    function IndexOf(value: HSTRING; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: HSTRING); safecall;
    procedure InsertAt(index: Cardinal; value: HSTRING); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: HSTRING); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PHSTRING); safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IAddPackageOptions
  [WinRTClassNameAttribute(SWindows_Management_Deployment_AddPackageOptions)]
  Deployment_IAddPackageOptions = interface(IInspectable)
  ['{05CEE018-F68F-422B-95A4-66679EC77FC0}']
    function get_DependencyPackageUris: IVector_1__IUriRuntimeClass; safecall;
    function get_TargetVolume: Deployment_IPackageVolume; safecall;
    procedure put_TargetVolume(value: Deployment_IPackageVolume); safecall;
    function get_OptionalPackageFamilyNames: IVector_1__HSTRING; safecall;
    function get_OptionalPackageUris: IVector_1__IUriRuntimeClass; safecall;
    function get_RelatedPackageUris: IVector_1__IUriRuntimeClass; safecall;
    function get_ExternalLocationUri: IUriRuntimeClass; safecall;
    procedure put_ExternalLocationUri(value: IUriRuntimeClass); safecall;
    function get_StubPackageOption: Deployment_StubPackageOption; safecall;
    procedure put_StubPackageOption(value: Deployment_StubPackageOption); safecall;
    function get_DeveloperMode: Boolean; safecall;
    procedure put_DeveloperMode(value: Boolean); safecall;
    function get_ForceAppShutdown: Boolean; safecall;
    procedure put_ForceAppShutdown(value: Boolean); safecall;
    function get_ForceTargetAppShutdown: Boolean; safecall;
    procedure put_ForceTargetAppShutdown(value: Boolean); safecall;
    function get_ForceUpdateFromAnyVersion: Boolean; safecall;
    procedure put_ForceUpdateFromAnyVersion(value: Boolean); safecall;
    function get_InstallAllResources: Boolean; safecall;
    procedure put_InstallAllResources(value: Boolean); safecall;
    function get_RequiredContentGroupOnly: Boolean; safecall;
    procedure put_RequiredContentGroupOnly(value: Boolean); safecall;
    function get_RetainFilesOnFailure: Boolean; safecall;
    procedure put_RetainFilesOnFailure(value: Boolean); safecall;
    function get_StageInPlace: Boolean; safecall;
    procedure put_StageInPlace(value: Boolean); safecall;
    function get_AllowUnsigned: Boolean; safecall;
    procedure put_AllowUnsigned(value: Boolean); safecall;
    function get_DeferRegistrationWhenPackagesAreInUse: Boolean; safecall;
    procedure put_DeferRegistrationWhenPackagesAreInUse(value: Boolean); safecall;
    property AllowUnsigned: Boolean read get_AllowUnsigned write put_AllowUnsigned;
    property DeferRegistrationWhenPackagesAreInUse: Boolean read get_DeferRegistrationWhenPackagesAreInUse write put_DeferRegistrationWhenPackagesAreInUse;
    property DependencyPackageUris: IVector_1__IUriRuntimeClass read get_DependencyPackageUris;
    property DeveloperMode: Boolean read get_DeveloperMode write put_DeveloperMode;
    property ExternalLocationUri: IUriRuntimeClass read get_ExternalLocationUri write put_ExternalLocationUri;
    property ForceAppShutdown: Boolean read get_ForceAppShutdown write put_ForceAppShutdown;
    property ForceTargetAppShutdown: Boolean read get_ForceTargetAppShutdown write put_ForceTargetAppShutdown;
    property ForceUpdateFromAnyVersion: Boolean read get_ForceUpdateFromAnyVersion write put_ForceUpdateFromAnyVersion;
    property InstallAllResources: Boolean read get_InstallAllResources write put_InstallAllResources;
    property OptionalPackageFamilyNames: IVector_1__HSTRING read get_OptionalPackageFamilyNames;
    property OptionalPackageUris: IVector_1__IUriRuntimeClass read get_OptionalPackageUris;
    property RelatedPackageUris: IVector_1__IUriRuntimeClass read get_RelatedPackageUris;
    property RequiredContentGroupOnly: Boolean read get_RequiredContentGroupOnly write put_RequiredContentGroupOnly;
    property RetainFilesOnFailure: Boolean read get_RetainFilesOnFailure write put_RetainFilesOnFailure;
    property StageInPlace: Boolean read get_StageInPlace write put_StageInPlace;
    property StubPackageOption: Deployment_StubPackageOption read get_StubPackageOption write put_StubPackageOption;
    property TargetVolume: Deployment_IPackageVolume read get_TargetVolume write put_TargetVolume;
  end;

  // UsedAPI Interface
  // Windows.Management.Deployment.IDeploymentResult
  Deployment_IDeploymentResult = interface(IInspectable)
  ['{2563B9AE-B77D-4C1F-8A7B-20E6AD515EF3}']
    function get_ErrorText: HSTRING; safecall;
    function get_ActivityId: TGuid; safecall;
    function get_ExtendedErrorCode: HRESULT; safecall;
    property ActivityId: TGuid read get_ActivityId;
    property ErrorText: HSTRING read get_ErrorText;
    property ExtendedErrorCode: HRESULT read get_ExtendedErrorCode;
  end;

  // Windows.Management.Deployment.IDeploymentResult2
  Deployment_IDeploymentResult2 = interface(IInspectable)
  ['{FC0E715C-5A01-4BD7-BCF1-381C8C82E04A}']
    function get_IsRegistered: Boolean; safecall;
    property IsRegistered: Boolean read get_IsRegistered;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress_Delegate_Base = interface(IUnknown)
  ['{F1B926D1-1796-597A-9BEA-6C6449D03EEF}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; progressInfo: Deployment_DeploymentProgress); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = interface(AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress_Delegate_Base)
  ['{861AC421-B865-5686-86C4-4917F24750BB}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress_Delegate_Base = interface(IUnknown)
  ['{6E1C7129-61E0-5D88-9FD4-F3CE65A05719}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = interface(AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress_Delegate_Base)
  ['{2ABB4ED1-2F62-5ED0-8D86-755BEEBBB430}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress_Base = interface(IInspectable)
  ['{5A97AAB7-B6EA-55AC-A5DC-D5B164D94E94}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function GetResults: Deployment_IDeploymentResult; safecall;
    property Progress: AsyncOperationProgressHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Management.Deployment.IDeploymentResult,Windows.Management.Deployment.DeploymentProgress>
  IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress = interface(IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress_Base)
  ['{7AF23842-29E8-5C37-BB0B-6561C00ADB92}']
  end;

  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageUserInformation
  Deployment_IPackageUserInformation = interface(IInspectable)
  ['{F6383423-FA09-4CBC-9055-15CA275E2E7E}']
    function get_UserSecurityId: HSTRING; safecall;
    function get_InstallState: Deployment_PackageInstallState; safecall;
    property InstallState: Deployment_PackageInstallState read get_InstallState;
    property UserSecurityId: HSTRING read get_UserSecurityId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Management.Deployment.IPackageUserInformation>
  IIterator_1__Deployment_IPackageUserInformation_Base = interface(IInspectable)
  ['{75660566-AE43-5858-ADA6-D57DDAE90277}']
    function get_Current: Deployment_IPackageUserInformation; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDeployment_IPackageUserInformation): Cardinal; safecall;
    property Current: Deployment_IPackageUserInformation read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Management.Deployment.IPackageUserInformation>
  IIterator_1__Deployment_IPackageUserInformation = interface(IIterator_1__Deployment_IPackageUserInformation_Base)
  ['{BA4EB7C5-E98F-56CE-965B-8E26E0FF88DC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Management.Deployment.IPackageUserInformation>
  IIterable_1__Deployment_IPackageUserInformation_Base = interface(IInspectable)
  ['{341348B9-52C8-5B57-9E91-F19F2A05B188}']
    function First: IIterator_1__Deployment_IPackageUserInformation; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Management.Deployment.IPackageUserInformation>
  IIterable_1__Deployment_IPackageUserInformation = interface(IIterable_1__Deployment_IPackageUserInformation_Base)
  ['{1CD8D83B-9210-50C2-A324-5A8E16954951}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager
  [WinRTClassNameAttribute(SWindows_Management_Deployment_PackageManager)]
  Deployment_IPackageManager = interface(IInspectable)
  ['{9A7D4B65-5E8F-4FC7-A2E5-7F6925CB8B53}']
    function AddPackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function UpdatePackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RemovePackageAsync(packageFullName: HSTRING): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function StagePackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RegisterPackageAsync(manifestUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function FindPackages: IIterable_1__IPackage; overload; safecall;
    function FindPackagesForUser(userSecurityId: HSTRING): IIterable_1__IPackage; overload; safecall;
    function FindPackages(packageName: HSTRING; packagePublisher: HSTRING): IIterable_1__IPackage; overload; safecall;
    function FindPackagesForUser(userSecurityId: HSTRING; packageName: HSTRING; packagePublisher: HSTRING): IIterable_1__IPackage; overload; safecall;
    function FindUsers(packageFullName: HSTRING): IIterable_1__Deployment_IPackageUserInformation; safecall;
    procedure SetPackageState(packageFullName: HSTRING; packageState: Deployment_PackageState); safecall;
    function FindPackage(packageFullName: HSTRING): IPackage; safecall;
    function CleanupPackageForUserAsync(packageName: HSTRING; userSecurityId: HSTRING): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function FindPackages(packageFamilyName: HSTRING): IIterable_1__IPackage; overload; safecall;
    function FindPackagesForUser(userSecurityId: HSTRING; packageFamilyName: HSTRING): IIterable_1__IPackage; overload; safecall;
    function FindPackageForUser(userSecurityId: HSTRING; packageFullName: HSTRING): IPackage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager2
  Deployment_IPackageManager2 = interface(IInspectable)
  ['{F7AAD08D-0840-46F2-B5D8-CAD47693A095}']
    function RemovePackageAsync(packageFullName: HSTRING; removalOptions: Deployment_RemovalOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function StagePackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RegisterPackageByFullNameAsync(mainPackageFullName: HSTRING; dependencyPackageFullNames: IIterable_1__HSTRING; deploymentOptions: Deployment_DeploymentOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function FindPackagesWithPackageTypes(packageTypes: Deployment_PackageTypes): IIterable_1__IPackage; overload; safecall;
    function FindPackagesForUserWithPackageTypes(userSecurityId: HSTRING; packageTypes: Deployment_PackageTypes): IIterable_1__IPackage; overload; safecall;
    function FindPackagesWithPackageTypes(packageName: HSTRING; packagePublisher: HSTRING; packageTypes: Deployment_PackageTypes): IIterable_1__IPackage; overload; safecall;
    function FindPackagesForUserWithPackageTypes(userSecurityId: HSTRING; packageName: HSTRING; packagePublisher: HSTRING; packageTypes: Deployment_PackageTypes): IIterable_1__IPackage; overload; safecall;
    function FindPackagesWithPackageTypes(packageFamilyName: HSTRING; packageTypes: Deployment_PackageTypes): IIterable_1__IPackage; overload; safecall;
    function FindPackagesForUserWithPackageTypes(userSecurityId: HSTRING; packageFamilyName: HSTRING; packageTypes: Deployment_PackageTypes): IIterable_1__IPackage; overload; safecall;
    function StageUserDataAsync(packageFullName: HSTRING): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Management.Deployment.IPackageVolume>
  AsyncOperationCompletedHandler_1__Deployment_IPackageVolume_Delegate_Base = interface(IUnknown)
  ['{35FEE361-6CEA-5E5C-8EDA-34B3F22DF4E7}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Deployment_IPackageVolume; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Management.Deployment.IPackageVolume>
  AsyncOperationCompletedHandler_1__Deployment_IPackageVolume = interface(AsyncOperationCompletedHandler_1__Deployment_IPackageVolume_Delegate_Base)
  ['{6FE7A673-8668-57D3-B7F0-4B020D7ACA77}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Management.Deployment.IPackageVolume>
  IAsyncOperation_1__Deployment_IPackageVolume_Base = interface(IInspectable)
  ['{0315EDB6-DC58-51CC-A519-44901AD2CF15}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Deployment_IPackageVolume); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Deployment_IPackageVolume; safecall;
    function GetResults: Deployment_IPackageVolume; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Deployment_IPackageVolume read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Management.Deployment.IPackageVolume>
  IAsyncOperation_1__Deployment_IPackageVolume = interface(IAsyncOperation_1__Deployment_IPackageVolume_Base)
  ['{EEED3F8D-7911-534D-99E8-F5C0D3968F23}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Management.Deployment.IPackageVolume>
  IIterator_1__Deployment_IPackageVolume_Base = interface(IInspectable)
  ['{A8D5B736-4E68-5EF1-9F07-F06837988C73}']
    function get_Current: Deployment_IPackageVolume; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDeployment_IPackageVolume): Cardinal; safecall;
    property Current: Deployment_IPackageVolume read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Management.Deployment.IPackageVolume>
  IIterator_1__Deployment_IPackageVolume = interface(IIterator_1__Deployment_IPackageVolume_Base)
  ['{6502657F-39D1-51CB-8EFC-A5C8E5249C0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Management.Deployment.IPackageVolume>
  IIterable_1__Deployment_IPackageVolume_Base = interface(IInspectable)
  ['{A6199162-B163-56A1-9980-DB0C3F4E9284}']
    function First: IIterator_1__Deployment_IPackageVolume; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Management.Deployment.IPackageVolume>
  IIterable_1__Deployment_IPackageVolume = interface(IIterable_1__Deployment_IPackageVolume_Base)
  ['{FF5F02E2-4F72-50FC-B792-22550539B8A0}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager3
  Deployment_IPackageManager3 = interface(IInspectable)
  ['{DAAD9948-36F1-41A7-9188-BC263E0DCB72}']
    function AddPackageVolumeAsync(packageStorePath: HSTRING): IAsyncOperation_1__Deployment_IPackageVolume; safecall;
    function AddPackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    procedure ClearPackageStatus(packageFullName: HSTRING; status: Deployment_PackageStatus); safecall;
    function RegisterPackageAsync(manifestUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions; appDataVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function FindPackageVolume(volumeName: HSTRING): Deployment_IPackageVolume; safecall;
    function FindPackageVolumes: IIterable_1__Deployment_IPackageVolume; safecall;
    function GetDefaultPackageVolume: Deployment_IPackageVolume; safecall;
    function MovePackageToVolumeAsync(packageFullName: HSTRING; deploymentOptions: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RemovePackageVolumeAsync(volume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    procedure SetDefaultPackageVolume(volume: Deployment_IPackageVolume); safecall;
    procedure SetPackageStatus(packageFullName: HSTRING; status: Deployment_PackageStatus); safecall;
    function SetPackageVolumeOfflineAsync(packageVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function SetPackageVolumeOnlineAsync(packageVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function StagePackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function StageUserDataAsync(packageFullName: HSTRING; deploymentOptions: Deployment_DeploymentOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>
  IVectorView_1__Deployment_IPackageVolume = interface(IInspectable)
  ['{3A73E178-6A1B-565F-9E4D-5FA259CA687E}']
    function GetAt(index: Cardinal): Deployment_IPackageVolume; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Deployment_IPackageVolume; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDeployment_IPackageVolume): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume_Delegate_Base = interface(IUnknown)
  ['{721241C2-0B83-594A-9B61-CE7F1492C415}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume_Delegate_Base)
  ['{F20E797E-09FA-5BAA-B39F-599C139664E1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>>
  IAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume_Base = interface(IInspectable)
  ['{1E357E07-D337-5C07-AE06-900C1B9A77C1}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume; safecall;
    function GetResults: IVectorView_1__Deployment_IPackageVolume; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Deployment_IPackageVolume read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Management.Deployment.IPackageVolume>>
  IAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume = interface(IAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume_Base)
  ['{6FD8C3AE-E9B7-5052-A4F6-0481394B4685}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager4
  Deployment_IPackageManager4 = interface(IInspectable)
  ['{3C719963-BAB6-46BF-8FF7-DA4719230AE6}']
    function GetPackageVolumesAsync: IAsyncOperation_1__IVectorView_1__Deployment_IPackageVolume; safecall;
  end;

  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManagerDebugSettings
  Deployment_IPackageManagerDebugSettings = interface(IInspectable)
  ['{1A611683-A988-4FCF-8F0F-CE175898E8EB}']
    function SetContentGroupStateAsync(package: IPackage; contentGroupName: HSTRING; state: PackageContentGroupState): IAsyncAction; overload; safecall;
    function SetContentGroupStateAsync(package: IPackage; contentGroupName: HSTRING; state: PackageContentGroupState; completionPercentage: Double): IAsyncAction; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager5
  Deployment_IPackageManager5 = interface(IInspectable)
  ['{711F3117-1AFD-4313-978C-9BB6E1B864A7}']
    function AddPackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume; optionalPackageFamilyNames: IIterable_1__HSTRING; externalPackageUris: IIterable_1__IUriRuntimeClass): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function StagePackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume; optionalPackageFamilyNames: IIterable_1__HSTRING; externalPackageUris: IIterable_1__IUriRuntimeClass): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RegisterPackageByFamilyNameAsync(mainPackageFamilyName: HSTRING; dependencyPackageFamilyNames: IIterable_1__HSTRING; deploymentOptions: Deployment_DeploymentOptions; appDataVolume: Deployment_IPackageVolume; optionalPackageFamilyNames: IIterable_1__HSTRING): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function get_DebugSettings: Deployment_IPackageManagerDebugSettings; safecall;
    property DebugSettings: Deployment_IPackageManagerDebugSettings read get_DebugSettings;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager6
  Deployment_IPackageManager6 = interface(IInspectable)
  ['{0847E909-53CD-4E4F-832E-57D180F6E447}']
    function ProvisionPackageForAllUsersAsync(packageFamilyName: HSTRING): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function AddPackageByAppInstallerFileAsync(appInstallerFileUri: IUriRuntimeClass; options: Deployment_AddPackageByAppInstallerOptions; targetVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RequestAddPackageByAppInstallerFileAsync(appInstallerFileUri: IUriRuntimeClass; options: Deployment_AddPackageByAppInstallerOptions; targetVolume: Deployment_IPackageVolume): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function AddPackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; options: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume; optionalPackageFamilyNames: IIterable_1__HSTRING; packageUrisToInstall: IIterable_1__IUriRuntimeClass; relatedPackageUris: IIterable_1__IUriRuntimeClass): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function StagePackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; options: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume; optionalPackageFamilyNames: IIterable_1__HSTRING; packageUrisToInstall: IIterable_1__IUriRuntimeClass; relatedPackageUris: IIterable_1__IUriRuntimeClass): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RequestAddPackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume; optionalPackageFamilyNames: IIterable_1__HSTRING; relatedPackageUris: IIterable_1__IUriRuntimeClass): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager7
  Deployment_IPackageManager7 = interface(IInspectable)
  ['{F28654F4-2BA7-4B80-88D6-BE15F9A23FBA}']
    function RequestAddPackageAsync(packageUri: IUriRuntimeClass; dependencyPackageUris: IIterable_1__IUriRuntimeClass; deploymentOptions: Deployment_DeploymentOptions; targetVolume: Deployment_IPackageVolume; optionalPackageFamilyNames: IIterable_1__HSTRING; relatedPackageUris: IIterable_1__IUriRuntimeClass; packageUrisToInstall: IIterable_1__IUriRuntimeClass): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager8
  Deployment_IPackageManager8 = interface(IInspectable)
  ['{B8575330-1298-4EE2-80EE-7F659C5D2782}']
    function DeprovisionPackageForAllUsersAsync(packageFamilyName: HSTRING): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IStagePackageOptions
  [WinRTClassNameAttribute(SWindows_Management_Deployment_StagePackageOptions)]
  Deployment_IStagePackageOptions = interface(IInspectable)
  ['{0B110C9C-B95D-4C56-BD36-6D656800D06B}']
    function get_DependencyPackageUris: IVector_1__IUriRuntimeClass; safecall;
    function get_TargetVolume: Deployment_IPackageVolume; safecall;
    procedure put_TargetVolume(value: Deployment_IPackageVolume); safecall;
    function get_OptionalPackageFamilyNames: IVector_1__HSTRING; safecall;
    function get_OptionalPackageUris: IVector_1__IUriRuntimeClass; safecall;
    function get_RelatedPackageUris: IVector_1__IUriRuntimeClass; safecall;
    function get_ExternalLocationUri: IUriRuntimeClass; safecall;
    procedure put_ExternalLocationUri(value: IUriRuntimeClass); safecall;
    function get_StubPackageOption: Deployment_StubPackageOption; safecall;
    procedure put_StubPackageOption(value: Deployment_StubPackageOption); safecall;
    function get_DeveloperMode: Boolean; safecall;
    procedure put_DeveloperMode(value: Boolean); safecall;
    function get_ForceUpdateFromAnyVersion: Boolean; safecall;
    procedure put_ForceUpdateFromAnyVersion(value: Boolean); safecall;
    function get_InstallAllResources: Boolean; safecall;
    procedure put_InstallAllResources(value: Boolean); safecall;
    function get_RequiredContentGroupOnly: Boolean; safecall;
    procedure put_RequiredContentGroupOnly(value: Boolean); safecall;
    function get_StageInPlace: Boolean; safecall;
    procedure put_StageInPlace(value: Boolean); safecall;
    function get_AllowUnsigned: Boolean; safecall;
    procedure put_AllowUnsigned(value: Boolean); safecall;
    property AllowUnsigned: Boolean read get_AllowUnsigned write put_AllowUnsigned;
    property DependencyPackageUris: IVector_1__IUriRuntimeClass read get_DependencyPackageUris;
    property DeveloperMode: Boolean read get_DeveloperMode write put_DeveloperMode;
    property ExternalLocationUri: IUriRuntimeClass read get_ExternalLocationUri write put_ExternalLocationUri;
    property ForceUpdateFromAnyVersion: Boolean read get_ForceUpdateFromAnyVersion write put_ForceUpdateFromAnyVersion;
    property InstallAllResources: Boolean read get_InstallAllResources write put_InstallAllResources;
    property OptionalPackageFamilyNames: IVector_1__HSTRING read get_OptionalPackageFamilyNames;
    property OptionalPackageUris: IVector_1__IUriRuntimeClass read get_OptionalPackageUris;
    property RelatedPackageUris: IVector_1__IUriRuntimeClass read get_RelatedPackageUris;
    property RequiredContentGroupOnly: Boolean read get_RequiredContentGroupOnly write put_RequiredContentGroupOnly;
    property StageInPlace: Boolean read get_StageInPlace write put_StageInPlace;
    property StubPackageOption: Deployment_StubPackageOption read get_StubPackageOption write put_StubPackageOption;
    property TargetVolume: Deployment_IPackageVolume read get_TargetVolume write put_TargetVolume;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IRegisterPackageOptions
  [WinRTClassNameAttribute(SWindows_Management_Deployment_RegisterPackageOptions)]
  Deployment_IRegisterPackageOptions = interface(IInspectable)
  ['{677112A7-50D4-496C-8415-0602B4C6D3BF}']
    function get_DependencyPackageUris: IVector_1__IUriRuntimeClass; safecall;
    function get_AppDataVolume: Deployment_IPackageVolume; safecall;
    procedure put_AppDataVolume(value: Deployment_IPackageVolume); safecall;
    function get_OptionalPackageFamilyNames: IVector_1__HSTRING; safecall;
    function get_ExternalLocationUri: IUriRuntimeClass; safecall;
    procedure put_ExternalLocationUri(value: IUriRuntimeClass); safecall;
    function get_DeveloperMode: Boolean; safecall;
    procedure put_DeveloperMode(value: Boolean); safecall;
    function get_ForceAppShutdown: Boolean; safecall;
    procedure put_ForceAppShutdown(value: Boolean); safecall;
    function get_ForceTargetAppShutdown: Boolean; safecall;
    procedure put_ForceTargetAppShutdown(value: Boolean); safecall;
    function get_ForceUpdateFromAnyVersion: Boolean; safecall;
    procedure put_ForceUpdateFromAnyVersion(value: Boolean); safecall;
    function get_InstallAllResources: Boolean; safecall;
    procedure put_InstallAllResources(value: Boolean); safecall;
    function get_StageInPlace: Boolean; safecall;
    procedure put_StageInPlace(value: Boolean); safecall;
    function get_AllowUnsigned: Boolean; safecall;
    procedure put_AllowUnsigned(value: Boolean); safecall;
    function get_DeferRegistrationWhenPackagesAreInUse: Boolean; safecall;
    procedure put_DeferRegistrationWhenPackagesAreInUse(value: Boolean); safecall;
    property AllowUnsigned: Boolean read get_AllowUnsigned write put_AllowUnsigned;
    property AppDataVolume: Deployment_IPackageVolume read get_AppDataVolume write put_AppDataVolume;
    property DeferRegistrationWhenPackagesAreInUse: Boolean read get_DeferRegistrationWhenPackagesAreInUse write put_DeferRegistrationWhenPackagesAreInUse;
    property DependencyPackageUris: IVector_1__IUriRuntimeClass read get_DependencyPackageUris;
    property DeveloperMode: Boolean read get_DeveloperMode write put_DeveloperMode;
    property ExternalLocationUri: IUriRuntimeClass read get_ExternalLocationUri write put_ExternalLocationUri;
    property ForceAppShutdown: Boolean read get_ForceAppShutdown write put_ForceAppShutdown;
    property ForceTargetAppShutdown: Boolean read get_ForceTargetAppShutdown write put_ForceTargetAppShutdown;
    property ForceUpdateFromAnyVersion: Boolean read get_ForceUpdateFromAnyVersion write put_ForceUpdateFromAnyVersion;
    property InstallAllResources: Boolean read get_InstallAllResources write put_InstallAllResources;
    property OptionalPackageFamilyNames: IVector_1__HSTRING read get_OptionalPackageFamilyNames;
    property StageInPlace: Boolean read get_StageInPlace write put_StageInPlace;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.IPackageManager9
  Deployment_IPackageManager9 = interface(IInspectable)
  ['{1AA79035-CC71-4B2E-80A6-C7041D8579A7}']
    function FindProvisionedPackages: IVector_1__IPackage; safecall;
    function AddPackageByUriAsync(packageUri: IUriRuntimeClass; options: Deployment_IAddPackageOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function StagePackageByUriAsync(packageUri: IUriRuntimeClass; options: Deployment_IStagePackageOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RegisterPackageByUriAsync(manifestUri: IUriRuntimeClass; options: Deployment_IRegisterPackageOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    function RegisterPackagesByFullNameAsync(packageFullNames: IIterable_1__HSTRING; options: Deployment_IRegisterPackageOptions): IAsyncOperationWithProgress_2__Deployment_IDeploymentResult__Deployment_DeploymentProgress; safecall;
    procedure SetPackageStubPreference(packageFamilyName: HSTRING; useStub: Deployment_PackageStubPreference); safecall;
    function GetPackageStubPreference(packageFamilyName: HSTRING): Deployment_PackageStubPreference; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt64>
  AsyncOperationCompletedHandler_1__UInt64_Delegate_Base = interface(IUnknown)
  ['{EE8AEB02-FB00-51FA-8F57-32583EA241F9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__UInt64; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt64>
  AsyncOperationCompletedHandler_1__UInt64 = interface(AsyncOperationCompletedHandler_1__UInt64_Delegate_Base)
  ['{EE8AEB02-FB00-51FA-8F57-32583EA241F9}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<UInt64>
  IAsyncOperation_1__UInt64_Base = interface(IInspectable)
  ['{2A70D630-0767-5F0A-A1C2-DEB08126E26E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__UInt64); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__UInt64; safecall;
    function GetResults: UInt64; safecall;
    property Completed: AsyncOperationCompletedHandler_1__UInt64 read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<UInt64>
  IAsyncOperation_1__UInt64 = interface(IAsyncOperation_1__UInt64_Base)
  ['{2A70D630-0767-5F0A-A1C2-DEB08126E26E}']
  end;

  // Windows.Management.Deployment.IPackageVolume2
  Deployment_IPackageVolume2 = interface(IInspectable)
  ['{46ABCF2E-9DD4-47A2-AB8C-C6408349BCD8}']
    function get_IsFullTrustPackageSupported: Boolean; safecall;
    function get_IsAppxInstallSupported: Boolean; safecall;
    function GetAvailableSpaceAsync: IAsyncOperation_1__UInt64; safecall;
    property IsAppxInstallSupported: Boolean read get_IsAppxInstallSupported;
    property IsFullTrustPackageSupported: Boolean read get_IsFullTrustPackageSupported;
  end;

  // UsedAPI Interface
  // Windows.Management.Deployment.Preview.IInstalledClassicAppInfo
  Deployment_Preview_IInstalledClassicAppInfo = interface(IInspectable)
  ['{0A7D3DA3-65D0-4086-80D6-0610D760207D}']
    function get_DisplayName: HSTRING; safecall;
    function get_DisplayVersion: HSTRING; safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property DisplayVersion: HSTRING read get_DisplayVersion;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Deployment.Preview.IClassicAppManagerStatics
  [WinRTClassNameAttribute(SWindows_Management_Deployment_Preview_ClassicAppManager)]
  Deployment_Preview_IClassicAppManagerStatics = interface(IInspectable)
  ['{E2FAD668-882C-4F33-B035-0DF7B90D67E6}']
    function FindInstalledApp(appUninstallKey: HSTRING): Deployment_Preview_IInstalledClassicAppInfo; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.IMdmAlert
  [WinRTClassNameAttribute(SWindows_Management_MdmAlert)]
  IMdmAlert = interface(IInspectable)
  ['{B0FBC327-28C1-4B52-A548-C5807CAF70B6}']
    function get_Data: HSTRING; safecall;
    procedure put_Data(value: HSTRING); safecall;
    function get_Format: MdmAlertDataType; safecall;
    procedure put_Format(value: MdmAlertDataType); safecall;
    function get_Mark: MdmAlertMark; safecall;
    procedure put_Mark(value: MdmAlertMark); safecall;
    function get_Source: HSTRING; safecall;
    procedure put_Source(value: HSTRING); safecall;
    function get_Status: Cardinal; safecall;
    function get_Target: HSTRING; safecall;
    procedure put_Target(value: HSTRING); safecall;
    function get_Type: HSTRING; safecall;
    procedure put_Type(value: HSTRING); safecall;
    property Data: HSTRING read get_Data write put_Data;
    property Format: MdmAlertDataType read get_Format write put_Format;
    property Mark: MdmAlertMark read get_Mark write put_Mark;
    property Source: HSTRING read get_Source write put_Source;
    property Status: Cardinal read get_Status;
    property Target: HSTRING read get_Target write put_Target;
    property &Type: HSTRING read get_Type write put_Type;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Management.IMdmAlert>
  IIterator_1__IMdmAlert_Base = interface(IInspectable)
  ['{B4A6EBEA-B19F-5DA5-B3D1-E859F1F4DF17}']
    function get_Current: IMdmAlert; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIMdmAlert): Cardinal; safecall;
    property Current: IMdmAlert read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Management.IMdmAlert>
  IIterator_1__IMdmAlert = interface(IIterator_1__IMdmAlert_Base)
  ['{47D70B2D-916D-535B-9E06-D827E3D2ED88}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Management.IMdmAlert>
  IIterable_1__IMdmAlert_Base = interface(IInspectable)
  ['{A0A617DC-210C-529F-B5E9-29AECEEBB5A8}']
    function First: IIterator_1__IMdmAlert; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Management.IMdmAlert>
  IIterable_1__IMdmAlert = interface(IIterable_1__IMdmAlert_Base)
  ['{CF3C3A84-0F6D-59E4-A76C-A14A806A2FD1}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Management.IMdmAlert>
  IVectorView_1__IMdmAlert = interface(IInspectable)
  ['{C964A942-1FBC-5E8F-96E7-8E00139EBD74}']
    function GetAt(index: Cardinal): IMdmAlert; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IMdmAlert; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIMdmAlert): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Management.IMdmSession
  IMdmSession = interface(IInspectable)
  ['{FE89314C-8F64-4797-A9D7-9D88F86AE166}']
    function get_Alerts: IVectorView_1__IMdmAlert; safecall;
    function get_ExtendedError: HRESULT; safecall;
    function get_Id: HSTRING; safecall;
    function get_State: MdmSessionState; safecall;
    function AttachAsync: IAsyncAction; safecall;
    procedure Delete; safecall;
    function StartAsync: IAsyncAction; overload; safecall;
    function StartAsync(alerts: IIterable_1__IMdmAlert): IAsyncAction; overload; safecall;
    property Alerts: IVectorView_1__IMdmAlert read get_Alerts;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Id: HSTRING read get_Id;
    property State: MdmSessionState read get_State;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.IMdmSessionManagerStatics
  [WinRTClassNameAttribute(SWindows_Management_MdmSessionManager)]
  IMdmSessionManagerStatics = interface(IInspectable)
  ['{CF4AD959-F745-4B79-9B5C-DE0BF8EFE44B}']
    function get_SessionIds: IVectorView_1__HSTRING; safecall;
    function TryCreateSession: IMdmSession; safecall;
    procedure DeleteSessionById(sessionId: HSTRING); safecall;
    function GetSessionById(sessionId: HSTRING): IMdmSession; safecall;
    property SessionIds: IVectorView_1__HSTRING read get_SessionIds;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  AsyncOperationCompletedHandler_1__IInspectable = interface(IUnknown)
  ['{3F08262E-A2E1-5134-9297-E9211F481A2D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IInspectable; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Object>
  IAsyncOperation_1__IInspectable = interface(IInspectable)
  ['{ABF53C57-EE50-5342-B52A-26E3B8CC024F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IInspectable); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IInspectable; safecall;
    function GetResults: IInspectable; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IInspectable read get_Completed write put_Completed;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Management.Policies.INamedPolicyData,Object>
  TypedEventHandler_2__Policies_INamedPolicyData__IInspectable_Delegate_Base = interface(IUnknown)
  ['{791A3C00-5AA2-5F0E-BB17-3480BC2D96CC}']
    procedure Invoke(sender: Policies_INamedPolicyData; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Management.Policies.INamedPolicyData,Object>
  TypedEventHandler_2__Policies_INamedPolicyData__IInspectable = interface(TypedEventHandler_2__Policies_INamedPolicyData__IInspectable_Delegate_Base)
  ['{3070A1EC-B134-582C-820B-D3A7609A8DEA}']
  end;

  // UsedAPI Interface
  // Windows.Management.Policies.INamedPolicyData
  Policies_INamedPolicyData = interface(IInspectable)
  ['{38DCB198-95AC-4077-A643-8078CAE26400}']
    function get_Area: HSTRING; safecall;
    function get_Name: HSTRING; safecall;
    function get_Kind: Policies_NamedPolicyKind; safecall;
    function get_IsManaged: Boolean; safecall;
    function get_IsUserPolicy: Boolean; safecall;
    function get_User: IUser; safecall;
    function GetBoolean: Boolean; safecall;
    function GetBinary: IBuffer; safecall;
    function GetInt32: Integer; safecall;
    function GetInt64: Int64; safecall;
    function GetString: HSTRING; safecall;
    function add_Changed(changedHandler: TypedEventHandler_2__Policies_INamedPolicyData__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Changed(cookie: EventRegistrationToken); safecall;
    property Area: HSTRING read get_Area;
    property IsManaged: Boolean read get_IsManaged;
    property IsUserPolicy: Boolean read get_IsUserPolicy;
    property Kind: Policies_NamedPolicyKind read get_Kind;
    property Name: HSTRING read get_Name;
    property User: IUser read get_User;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Policies.INamedPolicyStatics
  [WinRTClassNameAttribute(SWindows_Management_Policies_NamedPolicy)]
  Policies_INamedPolicyStatics = interface(IInspectable)
  ['{7F793BE7-76C4-4058-8CAD-67662CD05F0D}']
    function GetPolicyFromPath(area: HSTRING; name: HSTRING): Policies_INamedPolicyData; safecall;
    function GetPolicyFromPathForUser(user: IUser; area: HSTRING; name: HSTRING): Policies_INamedPolicyData; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Workplace.IMdmAllowPolicyStatics
  [WinRTClassNameAttribute(SWindows_Management_Workplace_MdmPolicy)]
  Workplace_IMdmAllowPolicyStatics = interface(IInspectable)
  ['{C39709E7-741C-41F2-A4B6-314C31502586}']
    function IsBrowserAllowed: Boolean; safecall;
    function IsCameraAllowed: Boolean; safecall;
    function IsMicrosoftAccountAllowed: Boolean; safecall;
    function IsStoreAllowed: Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Management.Workplace.IMdmPolicyStatics2
  [WinRTClassNameAttribute(SWindows_Management_Workplace_MdmPolicy)]
  Workplace_IMdmPolicyStatics2 = interface(IInspectable)
  ['{C99C7526-03D4-49F9-A993-43EFCCD265C4}']
    function GetMessagingSyncPolicy: Workplace_MessagingSyncPolicy; safecall;
  end;

  // Windows.Management.Deployment.AddPackageOptions
  // DualAPI
  // Implements: Windows.Management.Deployment.IAddPackageOptions
  // Instantiable: "Deployment_IAddPackageOptions"
  TDeployment_AddPackageOptions = class(TWinRTGenericImportI<Deployment_IAddPackageOptions>) end;

  // Windows.Management.Deployment.PackageManager
  // DualAPI
  // Explicitly imported
  // Implements: Windows.Management.Deployment.IPackageManager
  // Implements: Windows.Management.Deployment.IPackageManager2
  // Implements: Windows.Management.Deployment.IPackageManager3
  // Implements: Windows.Management.Deployment.IPackageManager4
  // Implements: Windows.Management.Deployment.IPackageManager5
  // Implements: Windows.Management.Deployment.IPackageManager6
  // Implements: Windows.Management.Deployment.IPackageManager7
  // Implements: Windows.Management.Deployment.IPackageManager8
  // Implements: Windows.Management.Deployment.IPackageManager9
  // Instantiable: "Deployment_IPackageManager"
  TDeployment_PackageManager = class(TWinRTGenericImportI<Deployment_IPackageManager>) end;

  // Windows.Management.Deployment.Preview.ClassicAppManager
  // DualAPI
  // Statics: "Windows.Management.Deployment.Preview.IClassicAppManagerStatics"
  TDeployment_Preview_ClassicAppManager = class(TWinRTGenericImportS<Deployment_Preview_IClassicAppManagerStatics>)
  public
    // -> Deployment_Preview_IClassicAppManagerStatics
    class function FindInstalledApp(appUninstallKey: HSTRING): Deployment_Preview_IInstalledClassicAppInfo; static; inline;
  end;

  // Windows.Management.Deployment.RegisterPackageOptions
  // DualAPI
  // Implements: Windows.Management.Deployment.IRegisterPackageOptions
  // Instantiable: "Deployment_IRegisterPackageOptions"
  TDeployment_RegisterPackageOptions = class(TWinRTGenericImportI<Deployment_IRegisterPackageOptions>) end;

  // Windows.Management.Deployment.StagePackageOptions
  // DualAPI
  // Implements: Windows.Management.Deployment.IStagePackageOptions
  // Instantiable: "Deployment_IStagePackageOptions"
  TDeployment_StagePackageOptions = class(TWinRTGenericImportI<Deployment_IStagePackageOptions>) end;

  // Windows.Management.MdmAlert
  // DualAPI
  // Implements: Windows.Management.IMdmAlert
  // Instantiable: "IMdmAlert"
  TMdmAlert = class(TWinRTGenericImportI<IMdmAlert>) end;

  // Windows.Management.MdmSessionManager
  // DualAPI
  // Statics: "Windows.Management.IMdmSessionManagerStatics"
  TMdmSessionManager = class(TWinRTGenericImportS<IMdmSessionManagerStatics>)
  public
    // -> IMdmSessionManagerStatics
    class function get_SessionIds: IVectorView_1__HSTRING; static; inline;
    class function TryCreateSession: IMdmSession; static; inline;
    class procedure DeleteSessionById(sessionId: HSTRING); static; inline;
    class function GetSessionById(sessionId: HSTRING): IMdmSession; static; inline;
    class property SessionIds: IVectorView_1__HSTRING read get_SessionIds;
  end;

  // Windows.Management.Policies.NamedPolicy
  // DualAPI
  // Statics: "Windows.Management.Policies.INamedPolicyStatics"
  TPolicies_NamedPolicy = class(TWinRTGenericImportS<Policies_INamedPolicyStatics>)
  public
    // -> Policies_INamedPolicyStatics
    class function GetPolicyFromPath(area: HSTRING; name: HSTRING): Policies_INamedPolicyData; static; inline;
    class function GetPolicyFromPathForUser(user: IUser; area: HSTRING; name: HSTRING): Policies_INamedPolicyData; static; inline;
  end;

  // Windows.Management.Workplace.MdmPolicy
  // DualAPI
  // Statics: "Windows.Management.Workplace.IMdmAllowPolicyStatics"
  // Statics: "Windows.Management.Workplace.IMdmPolicyStatics2"
  TWorkplace_MdmPolicy = class(TWinRTGenericImportS2<Workplace_IMdmAllowPolicyStatics, Workplace_IMdmPolicyStatics2>)
  public
    // -> Workplace_IMdmAllowPolicyStatics
    class function IsBrowserAllowed: Boolean; static; inline;
    class function IsCameraAllowed: Boolean; static; inline;
    class function IsMicrosoftAccountAllowed: Boolean; static; inline;
    class function IsStoreAllowed: Boolean; static; inline;

    // -> Workplace_IMdmPolicyStatics2
    class function GetMessagingSyncPolicy: Workplace_MessagingSyncPolicy; static; inline;
  end;

implementation

{ TDeployment_AddPackageOptions }

{ TDeployment_PackageManager }

{ TDeployment_Preview_ClassicAppManager }

class function TDeployment_Preview_ClassicAppManager.FindInstalledApp(appUninstallKey: HSTRING): Deployment_Preview_IInstalledClassicAppInfo;
begin
  Result := Statics.FindInstalledApp(appUninstallKey);
end;


{ TDeployment_RegisterPackageOptions }

{ TDeployment_StagePackageOptions }

{ TMdmAlert }

{ TMdmSessionManager }

class function TMdmSessionManager.get_SessionIds: IVectorView_1__HSTRING;
begin
  Result := Statics.get_SessionIds;
end;

class function TMdmSessionManager.TryCreateSession: IMdmSession;
begin
  Result := Statics.TryCreateSession;
end;

class procedure TMdmSessionManager.DeleteSessionById(sessionId: HSTRING);
begin
  Statics.DeleteSessionById(sessionId);
end;

class function TMdmSessionManager.GetSessionById(sessionId: HSTRING): IMdmSession;
begin
  Result := Statics.GetSessionById(sessionId);
end;


{ TPolicies_NamedPolicy }

class function TPolicies_NamedPolicy.GetPolicyFromPath(area: HSTRING; name: HSTRING): Policies_INamedPolicyData;
begin
  Result := Statics.GetPolicyFromPath(area, name);
end;

class function TPolicies_NamedPolicy.GetPolicyFromPathForUser(user: IUser; area: HSTRING; name: HSTRING): Policies_INamedPolicyData;
begin
  Result := Statics.GetPolicyFromPathForUser(user, area, name);
end;


{ TWorkplace_MdmPolicy }

class function TWorkplace_MdmPolicy.IsBrowserAllowed: Boolean;
begin
  Result := Statics.IsBrowserAllowed;
end;

class function TWorkplace_MdmPolicy.IsCameraAllowed: Boolean;
begin
  Result := Statics.IsCameraAllowed;
end;

class function TWorkplace_MdmPolicy.IsMicrosoftAccountAllowed: Boolean;
begin
  Result := Statics.IsMicrosoftAccountAllowed;
end;

class function TWorkplace_MdmPolicy.IsStoreAllowed: Boolean;
begin
  Result := Statics.IsStoreAllowed;
end;


class function TWorkplace_MdmPolicy.GetMessagingSyncPolicy: Workplace_MessagingSyncPolicy;
begin
  Result := Statics2.GetMessagingSyncPolicy;
end;


end.
