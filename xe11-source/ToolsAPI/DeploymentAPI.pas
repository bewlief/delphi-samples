{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit DeploymentAPI;

interface

uses System.SysUtils, System.Classes, ToolsAPI, PlatformAPI, System.Generics.Collections, System.Types;

type

  /// <summary>
  /// IDE Service 
  /// </summary>
  IOTADeploymentService = interface
    ['{658EBC6E-E96F-45FA-934B-505D158852C7}']
    procedure DeployManagerProject(const Project: IOTAProject);
  end;

  /// <summary>
  /// Personality Trait
  /// </summary>
  IProjectSupportsDeploymentTrait230 = interface
    ['{45F5E6B4-CFAE-4C8A-A6CE-D6513E12BE17}']
    function ProjectSupportsDeployment: Boolean;
  end;

  IProjectSupportsDeploymentTrait = interface(IProjectSupportsDeploymentTrait230)
    ['{E58E526C-3D36-4D3D-A2FF-F8D153569F29}']
    function ProjectSupportsDeployment: Boolean; overload; deprecated 'Use the overloaded version with IOTAProject parameter';
    function ProjectSupportsDeployment(const Project: IOTAProject): Boolean; overload;
  end;


  // Project module interface.  Projects that support deployment may implement.

//  IDeployAssemblyReferences = interface;

  IDeployableProject = interface
  ['{F775F090-1966-4C65-B778-DF4CD2F7F9E6}']
//    function GetHasAssemblyReferences: Boolean;
//    function GetAssemblyReferences: IDeployAssemblyReferences;
//    property HasAssemblyReferences: Boolean read GetHasAssemblyReferences;
//    property AssemblyReferences: IDeployAssemblyReferences read GetAssemblyReferences;
  end;

//  IDeployAssemblyReferences = interface
//  ['{68E7820D-5249-4E86-9024-E97A6BDC4176}']
//    procedure Refresh;
//    function GetCount: Integer;
//    function GetFileName(I: Integer): string;
//    property Count: Integer read GetCount;
//    property FileNames[I: Integer]: string read GetFileName;
//  end;

const
  feDeployProj = '.deployproj';
  sDeviceId = 'DeviceId';
  dcFile = 'File';
  dcProfectFile = 'ProjectFile';
  dcProjectOutput = 'ProjectOutput';
  dcDebugSymbols = 'DebugSymbols';
  dcAdditionalDebugSymbols = 'AdditionalDebugSymbols';
  dcDependencyPackage = 'DependencyPackage';
  dcDependencyPackageResource = 'DependencyPackageResource';
  dcDependencyPackageDebugSymbols = 'DependencyPackageDebugSymbols';
  dcDependencyModule = 'DependencyModule';
  dcDependencyFramework = 'DependencyFramework';

  //OSX
  dcProjectMainIcns = 'Icns_MainIcns';
  dcProjectOSXResource = 'ProjectOSXResource';
  dcProjectOSXInfoPList = 'ProjectOSXInfoPList';
  dcProjectOSXEntitlements = 'ProjectOSXEntitlements';
  dcProjectOSXDebug = 'ProjectOSXDebug';


  //iOS
  dcProjectiOSResource = 'ProjectiOSResource';
  dcProjectOutput_iOS32 = 'ProjectOutput_iOS32';

  dcIOS_AppStore1024 = 'iOS_AppStore1024';

  dcIPhone_AppIcon120 = 'iPhone_AppIcon120';
  dcIPhone_AppIcon180 = 'iPhone_AppIcon180';
  dcIPhone_Spotlight80 = 'iPhone_Spotlight80';
  dcIPhone_Spotlight120 = 'iPhone_Spotlight120';
  dcIPhone_Setting58 = 'iPhone_Setting58';
  dcIPhone_Setting87 = 'iPhone_Setting87';
  dcIPhone_Notification40 = 'iPhone_Notification40';
  dcIPhone_Notification60 = 'iPhone_Notification60';

  dcIPhone_Launch2x = 'iPhone_Launch2x';
  dcIPhone_LaunchDark2x = 'iPhone_LaunchDark2x';
  dcIPhone_Launch3x = 'iPhone_Launch3x';
  dcIPhone_LaunchDark3x = 'iPhone_LaunchDark3x';

  dcIPad_AppIcon152 = 'iPad_AppIcon152';
  dcIPad_AppIcon167 = 'iPad_AppIcon167';
  dcIPad_SpotLight80 = 'iPad_SpotLight80';
  dcIPad_Setting58 = 'iPad_Setting58';
  dcIPad_Notification40 = 'iPad_Notification40';

  dcIPad_Launch2x = 'iPad_Launch2x';
  dcIPad_LaunchDark2x = 'iPad_LaunchDark2x';

  dcProjectiOSInfoPList = 'ProjectiOSInfoPList';
  dcProjectiOSDeviceDebug = 'ProjectiOSDeviceDebug';
  dcProjectiOSEntitlements = 'ProjectiOSEntitlements';
  dcProjectiOSLaunchScreen = 'ProjectiOSLaunchScreen';

  //Android
  dcProjectOutput_Android32 = 'ProjectOutput_Android32';
  dcAndroidServiceOutput_Android32 = 'AndroidServiceOutput_Android32';

  dcAndroidDefaultAppIcon = 'Android_DefaultAppIcon';
  dcProjectAndroidManifest = 'ProjectAndroidManifest';
  dcAndroidLauncherIcon36 = 'Android_LauncherIcon36';
  dcAndroidLauncherIcon48 = 'Android_LauncherIcon48';
  dcAndroidLauncherIcon72 = 'Android_LauncherIcon72';
  dcAndroidLauncherIcon96 = 'Android_LauncherIcon96';
  dcAndroidLauncherIcon144 = 'Android_LauncherIcon144';
  dcAndroidLauncherIcon192 = 'Android_LauncherIcon192';
  dcAndroidSplashImage426 = 'Android_SplashImage426';
  dcAndroidSplashImage470 = 'Android_SplashImage470';
  dcAndroidSplashImage640 = 'Android_SplashImage640';
  dcAndroidSplashImage960 = 'Android_SplashImage960';
  dcAndroidGDBServer = 'AndroidGDBServer';
  dcAndroidClassesDexFile = 'AndroidClassesDexFile';
  dcAndroidClasses = 'AndroidClasses';
  dcAndroidLibnativeArmeabiFile = 'AndroidLibnativeArmeabiFile';
  dcAndroidLibnativeArmeabiv7aFile = 'AndroidLibnativeArmeabiv7aFile';
  dcAndroidLibnativeMipsFile = 'AndroidLibnativeMipsFile';
  dcAndroidSplashImageDef = 'AndroidSplashImageDef';
  dcAndroidSplashStyles = 'AndroidSplashStyles';
  dcAndroidSplashStylesV21 = 'AndroidSplashStylesV21';
  dcAndroidServiceOutput = 'AndroidServiceOutput';
  dcAndroidFileProvider = 'AndroidFileProvider';

  dcAndroidNotificationIcon24 = 'Android_NotificationIcon24';
  dcAndroidNotificationIcon36 = 'Android_NotificationIcon36';
  dcAndroidNotificationIcon48 = 'Android_NotificationIcon48';
  dcAndroidNotificationIcon72 = 'Android_NotificationIcon72';
  dcAndroidNotificationIcon96 = 'Android_NotificationIcon96';
  dcAndroidColors = 'Android_Colors';
  dcAndroidStrings = 'Android_Strings';

  //Universal Windows Package
  dcProjectUWPManifest = 'ProjectUWPManifest';
  dcUWPDefaultDelphiLogo44 = 'UWP_DelphiLogo44';
  dcUWPDefaultDelphiLogo150 = 'UWP_DelphiLogo150';
  dcUWPDefaultCppLogo44 = 'UWP_CppLogo44';
  dcUWPDefaultCppLogo150 = 'UWP_CppLogo150';

type
  /// <summary>
  /// Make sure the ordinal values of TDeployOperation match the corresponding
  /// "put" arguments for the remote agent. Make sure doNotSpecified is always
  /// the highest ordinal value 
  /// </summary>
  TDeployOperation = (doCopyOnly, doSetExecBit, doUnArchive, doRun, doUnused, doExecuteScript, doNotSpecified);
  TDeploymentItemAction = (daAdded, daChanged, daRemoved, daReconciled);

  IProjectDeploymentItem = interface
  ['{9D9FC05C-49BD-4FD8-8425-833D16FA55E3}']
    /// <summary>
    /// Condition is an MSBuild condition statement which will be evaluated in the
    /// context of the deployment's project when the Deploy target is run, so that
    /// delivery can be conditionalized based on project settings.
    /// e.g., "'$(DynamicRTL)'=='true'" on the file libcgrtl.dylib will cause it
    /// to be deployed only when the project has linked with the dynamic rtl. Look
    /// in $(TP)\app\design\*Strs.pas for a list of most project setting property names 
    /// </summary>
    function GetCondition: string;
    function GetLocalCommands(const PlatformName: string): TList<string>;
    function GetOperation(const PlatformName: string): TDeployOperation;
    function GetPlatforms: TArray<string>;
    function GetRemoteCommands(const PlatformName: string): TList<string>;
    function GetRemoteDir(const PlatformName: string): string;
    function GetRequired: Boolean;
    procedure RemovePlatform(const PlatformName: string);
    procedure SetCondition(const Value: string);
    procedure SetOperation(const PlatformName: string; const Value: TDeployOperation);
    procedure SetRemoteDir(const PlatformName, Value: string);
    procedure SetRequired(Value: Boolean);

    property Condition: string read GetCondition write SetCondition;
    property LocalCommands[const PlatformName: string]: TList<string> read GetLocalCommands;
    property Operation[const PlatformName: string]: TDeployOperation read GetOperation write SetOperation;
    property RemoteCommands[const PlatformName: string]: TList<string> read GetRemoteCommands;
    property RemoteDir[const PlatformName: string]: string read GetRemoteDir write SetRemoteDir;
    property Required: Boolean read GetRequired write SetRequired;
    property Platforms: TArray<string> read GetPlatforms;
  end;

  IProjectDeploymentFile170 = interface(IProjectDeploymentItem)
    ['{184728B4-5713-4D50-A042-CD027A7802BA}']
    function GetConfiguration: string;
    function GetDeploymentClass: string;
    function GetEnabled(const PlatformName: string): Boolean;
    function GetLocalName: string;
    function GetRemoteName(const PlatformName: string): string;
    procedure SetConfiguration(const Value: string);
    procedure SetDeploymentClass(const Value: string);
    procedure SetEnabled(const PlatformName: string; const Value: Boolean);
    procedure SetLocalName(const Value: string);
    procedure SetRemoteName(const PlatformName, Value: string);

    property Configuration: string read GetConfiguration write SetConfiguration;
    property DeploymentClass: string read GetDeploymentClass write SetDeploymentClass;
    property Enabled[const PlatformName: string]: Boolean read GetEnabled write SetEnabled;
    property LocalName: string read GetLocalName write SetLocalName;
    property RemoteName[const PlatformName: string]: string read GetRemoteName write SetRemoteName;
  end;

  IProjectDeploymentFile190 = interface(IProjectDeploymentFile170)
    ['{51CC1C02-BB0F-4CF1-8F48-68ABAEE511C2}']
    /// <summary>
    /// Methods to specify the platform for each file in order to avoid duplicates when the same file
    /// that is located in a unique directory is required for many platforms. 
    /// </summary>
    function GetFilePlatform: string;
    procedure SetFilePlatform(const Value: string);
    property FilePlatform: string read GetFilePlatform write SetFilePlatform;
  end;

  IProjectDeploymentFile = interface(IProjectDeploymentFile190)
    ['{F8032C03-731C-4A4D-A493-8F119129B07D}']
    /// <summary>
    /// Methods to specify when the remote file should be overwritten in case of the local file
    /// is newer than the remote file. 
    /// </summary>
    function GetOverwrite(const PlatformName: string): Boolean;
    procedure SetOverwrite (const PlatformName: string; Value: Boolean);
    property Overwrite[const PlatformName: string]: Boolean read GetOverwrite write SetOverwrite;
  end;

  IProjectDeploymentClass = interface(IProjectDeploymentItem)
    ['{C1FA0FD1-A0B0-4465-BE24-6C37C75B6D28}']
    function GetClassId: string;
    function GetDescription: string;
    procedure SetClassId(const Value: string);
    procedure SetDescription(const Value: string);
    function ValidFor(const Filename, PlatformName: string): Boolean;

    property ClassId: string read GetClassId write SetClassId;
    property Description: string read GetDescription write SetDescription;
  end;

  IProjectDeploymentNotifier = interface
    ['{E3BBD079-75B6-46F6-AC61-AD4B3B793111}']
    procedure Destroyed;
    procedure ItemChanged(const Item: IProjectDeploymentItem; Action: TDeploymentItemAction);
    procedure Loaded(const ClassId: string);
  end;

  TDeploymentFileArray = array of IProjectDeploymentFile;
  TDeploymentClassArray = array of IProjectDeploymentClass;

const
  cAllPlatforms = 'All';

type
  TReconcileResult = (rrSuccess, rrNotUpToDate, rrCompileRequired, rrFailed, rrNotSupported);

  IProjectDeployment170 = interface
    ['{FF8F9951-52D1-40F1-8A34-A72969BEE0D6}']
    /// <summary>
    /// Register for notifications when project deployment data changes 
    /// </summary>
    function AddNotifier(const Notifier: IProjectDeploymentNotifier): Integer;
    procedure AddClass(const AClass: IProjectDeploymentClass);
    procedure AddFile(const AFile: IProjectDeploymentFile);
    procedure Clear;
    function CreateClass(const ClassId: string): IProjectDeploymentClass;
    function CreateFile(const LocalName: string): IProjectDeploymentFile;
    function FindFiles(const LocalName, Configuration, PlatformName, ClassName: string): TDeploymentFileArray;
    function GetClasses: TDictionary<string, IProjectDeploymentClass>.TValueCollection;
    function GetClass(const ClassId: string): IProjectDeploymentClass;
    /// <summary>
    /// Returns which classes, if any, can be used with a particular file based on it's extension 
    /// </summary>
    function GetDefaultClasses(const Filename, PlatformName: string): TDeploymentClassArray;
    function GetFile(const LocalName: string): IProjectDeploymentFile;
    function GetFiles: TDictionary<string, IProjectDeploymentFile>.TValueCollection;
    /// <summary>
    /// Return an array of all items with ClassId 
    /// </summary>
    function GetFilesOfClass(const ClassId: string): TDeploymentFileArray;
    /// <summary>
    /// The project binary output's remote filename, relative to the remote project directory 
    /// </summary>
    function GetProjectOutputFile(const PlatformName: string = ''; const ConfigurationName: string = ''): string;
    /// <summary>
    /// The root directory on the remote machine, relative to the agent's "scratch" directory,
    /// or a fully qualified path 
    /// </summary>
    function GetRemoteProjectDir(PlatformName: string): string;
    /// <summary>
    /// Does the deployment have any data in it, or has ever been reconciled 
    /// </summary>
    function IsEmpty: Boolean;
    /// <summary>
    /// Updates deployment to reflect current project state 
    /// </summary>
    function Reconcile(Configuration: string = ''; PlatformName: string = ''): TReconcileResult;
    /// <summary>
    /// Remove a deployment class 
    /// </summary>
    procedure RemoveClass(const ClassId: string);
    /// <summary>
    /// Remove all deployment data associated with a local file 
    /// </summary>
    procedure RemoveFile(const LocalName: string);
    /// <summary>
    /// Remove all files with ClassId 
    /// </summary>
    procedure RemoveFilesOfClass(const ClassId: string);
    /// <summary>
    /// Remove previously registered notifier 
    /// </summary>
    procedure RemoveNotifier(const NotifierIndex: Integer);
    /// <summary>
    /// Remove all deployment data, reset to default 
    /// </summary>
    procedure Reset;
    /// <summary>
    /// Write MSBuild script used by the Deployment MSBuild target 
    /// </summary>
    procedure SaveToMSBuild;
    /// <summary>
    /// Set remote project root directory 
    /// </summary>
    procedure SetRemoteProjectDir(PlatformName: string; const Value: string);

    property Classes: TDictionary<string, IProjectDeploymentClass>.TValueCollection read GetClasses;
    property Files: TDictionary<string, IProjectDeploymentFile>.TValueCollection read GetFiles;
    property RemoteProjectDir[PlatformName: string]: string read GetRemoteProjectDir
      write SetRemoteProjectDir;
  end;

  IProjectDeployment180 = interface(IProjectDeployment170)
  ['{76A88D6C-8E6B-460E-8178-C21659E3D3CB}']
    /// <summary>
    /// Overloaded methods to manage files for many platforms. This is necessary in order to avoid duplicates
    /// when the same file that is located in a unique directory is required for many platforms. 
    /// </summary>
    procedure AddFile(const AFile: IProjectDeploymentFile); overload;
    procedure AddFile(const PlatformName: string; const AFile: IProjectDeploymentFile); overload;
    function CreateFile(const LocalName: string): IProjectDeploymentFile; overload; deprecated;
    function CreateFile(const PlatformName: string; const LocalName: string): IProjectDeploymentFile; overload;
    function GetFile(const LocalName: string): IProjectDeploymentFile; overload;
    function GetFile(const PlatformName: string; const LocalName: string): IProjectDeploymentFile; overload;
    procedure RemoveFile(const LocalName: string); overload;
    procedure RemoveFile(const PlatformName: string; const LocalName: string); overload;
  end;

  IProjectDeployment190 = interface(IProjectDeployment180)
    ['{34139979-F793-4B85-8C25-058545542407}']
    /// <summary>
    /// Overloaded methods to perform a selective clearing for all platforms or for a specified platform  
    /// </summary>
    procedure Clear; overload;
    procedure Clear(const PlatformName: string); overload;
    /// <summary>
    /// Method to get the number of deploy classes by platform 
    /// </summary>
    function GetClassCount(const PlatformName: string): Integer;
    /// <summary>
    /// Method to know if there are default deployment classes for the specified platform 
    /// </summary>
    function RequiresDeployClasses(const PlatformName: string): Boolean;
  end;

  IProjectDeployment200 = interface(IProjectDeployment190)
    ['{5EDEAD6D-538B-47F6-B3D6-65405FAEEE1C}']
    /// <summary>
    /// Overloaded methods to perform a selective clearing by removing or not the added files  
    /// </summary>
    procedure Clear(const KeepUserFiles: Boolean); overload;
    procedure Clear(const PlatformName: string; const KeepUserFiles: Boolean); overload;
  end;

  IProjectDeployment220 = interface(IProjectDeployment200)
    ['{5D1B3896-962A-417B-A7B5-EE5FAA36BBB7}']
    /// <summary>
    /// Overloaded methods to handle files for multiple configurations and platforms.
    /// </summary>
    procedure AddFile(const AFile: IProjectDeploymentFile); overload; deprecated 'Use overloaded version of this method with config and platform parameters';
    procedure AddFile(const PlatformName: string; const AFile: IProjectDeploymentFile); overload; deprecated 'Use overloaded version of this method with config and platform parameters';
    function CreateFile(const LocalName: string): IProjectDeploymentFile; overload; deprecated;
    function CreateFile(const PlatformName: string; const LocalName: string): IProjectDeploymentFile; overload; deprecated 'Use overloaded version of this method with config and platform parameters';
    function GetFile(const LocalName: string): IProjectDeploymentFile; overload; deprecated;
    function GetFile(const PlatformName: string; const LocalName: string): IProjectDeploymentFile; overload; deprecated 'Use overloaded version of this method with config and platform parameters';
    procedure RemoveFile(const LocalName: string); overload; deprecated 'Use overloaded version of this method with config and platform parameters';
    procedure RemoveFile(const PlatformName: string; const LocalName: string); overload; deprecated 'Use overloaded version of this method with config and platform parameters';

    procedure AddFile(const Configuration, PlatformName: string; const AFile: IProjectDeploymentFile); overload;
    function CreateFile(const Configuration, PlatformName: string; const LocalName: string): IProjectDeploymentFile; overload;
    function GetFile(const Configuration, PlatformName: string; const LocalName: string): IProjectDeploymentFile; overload;
    procedure RemoveFile(const Configuration, PlatformName: string; const LocalName: string); overload;

    /// <summary>
    /// Updates the deployment file list to reflect the current project state.
    /// Added new parameter ReconcileInternally to don't reconcile packages when we want to update the project state internally 
    /// </summary>
    function Reconcile(Configuration: string = ''; PlatformName: string = ''; const ReconcileInternally: Boolean = False): TReconcileResult; overload;
  end;

  IProjectDeployment230 = interface(IProjectDeployment220)
    ['{55D3B5B8-AE66-48BC-8357-2767E0121F03}']
    procedure SetBlockNotifications(const Value: Boolean);
    function GetBlockNotifications: Boolean;
    property BlockNotifications: Boolean read GetBlockNotifications write SetBlockNotifications;
  end;

  IProjectDeployment = interface(IProjectDeployment230)
    ['{3FF68DBA-FB95-477B-B619-43770365EF11}']
    function GetSortedClasses: TArray<IProjectDeploymentClass>;
    property SortedClasses: TArray<IProjectDeploymentClass> read GetSortedClasses;
  end;

{ Helper functions to resolve unspecified IProjectDeploymentFile fields through the
  underlying Deployment Class }
function GetDeploymentRemoteDir(const AFile: IProjectDeploymentFile;
  const PlatformName: string; const Deployment: IProjectDeployment): string;
function GetDeploymentOperation(const AFile: IProjectDeploymentFile;
  const PlatformName: string; const Deployment: IProjectDeployment): TDeployOperation;
function GetDeploymentClass(const AFile: IProjectDeploymentFile): string;
function GetDeploymentRequired(const AFile: IProjectDeploymentFile;
  const PlatformName: string; const Deployment: IProjectDeployment): Boolean;

implementation

function GetDeploymentRemoteDir(const AFile: IProjectDeploymentFile;
  const PlatformName: string; const Deployment: IProjectDeployment): string;
begin
  Result := AFile.RemoteDir[PlatformName];
  if (Result = '') and (AFile.DeploymentClass <> '')
  and (Deployment.GetClass(AFile.DeploymentClass) <> nil) then
  begin
    Result := Deployment.GetClass(AFile.DeploymentClass).RemoteDir[PlatformName];
    if Result <> '' then
      Result := IncludeTrailingPathDelimiter(Result);
  end;
end;

function GetDeploymentOperation(const AFile: IProjectDeploymentFile;
  const PlatformName: string; const Deployment: IProjectDeployment): TDeployOperation;
begin
  Result := AFile.Operation[PlatformName];
  if (Result = doNotSpecified) and (AFile.DeploymentClass <> '')
  and (Deployment.GetClass(AFile.DeploymentClass) <> nil) then
    Result := Deployment.GetClass(AFile.DeploymentClass).Operation[PlatformName];
  if Result = doNotSpecified then
    Result := doCopyOnly;
end;

function GetDeploymentClass(const AFile: IProjectDeploymentFile): string;
begin
  Result := AFile.DeploymentClass;
  if Result = '' then
    Result := dcFile;
end;

function GetDeploymentRequired(const AFile: IProjectDeploymentFile;
  const PlatformName: string; const Deployment: IProjectDeployment): Boolean;
begin
  Result := AFile.Required;
  if not Result and (AFile.DeploymentClass <> '')
  and (Deployment.GetClass(AFile.DeploymentClass) <> nil) then
    Result := Deployment.GetClass(AFile.DeploymentClass).Required;
end;

end.
