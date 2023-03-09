
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
[!outputoff]
// 1.1
[!outputon]
[!if=(DataSnapConsoleSource)]
[!if=(Diagnostics)]
// DataSnapConsoleSource
[!endif]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
program [!ProjectName];

{$APPTYPE CONSOLE}

uses System.SysUtils;

begin
  try
    RunDSServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end.
[!endif]
[!if=(DataSnapServiceSource)]
[!if=(Diagnostics)]
// DataSnapServiceSource
[!endif]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
program [!ProjectName];

uses Vcl.SvcMgr;

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.Run;
end.
[!endif]
[!if=(DataSnapVCLSource)]
[!if=(Diagnostics)]
// DataSnapVCLSource
[!endif]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
program [!ProjectName];

uses Vcl.Forms;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
[!endif]
[!if=(DataSnapModuleSource)]
[!if=(Diagnostics)]
// DataSnapModuleSource
[!endif]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
[!if=(Customize_CommentModule)]
[!Customize_CommentModuleText]
[!endif]
unit [!ModuleIdent];

interface

uses System.SysUtils, System.Classes,
[!if=(ServiceMethods)]
  Vcl.SvcMgr,
[!endif]
[!if=(TCPIPProtocol)]
  Datasnap.DSTCPServerTransport,
[!endif]
[!if=(HTTPProtocol)]
  Datasnap.DSHTTPCommon, Datasnap.DSHTTP,
[!endif]
  Datasnap.DSServer, Datasnap.DSCommonServer,
[!if=(IncludeDataSnapConnectors)]
  Datasnap.DSClientMetadata, Datasnap.DSHTTPServiceProxyDispatcher,
  Datasnap.DSProxyJavaAndroid, Datasnap.DSProxyJavaBlackBerry,
  Datasnap.DSProxyObjectiveCiOS, Datasnap.DSProxyCsharpSilverlight,
[!else]
[!if=(IncludeJavaScriptFiles)]
  Datasnap.DSClientMetadata, Datasnap.DSProxyJavaScript,
[!endif]
[!endif]
  Datasnap.DSAuth;

type
  T[!FormIdent] = class(T[!AncestorIdent])
    DSServer1: TDSServer;
[!if=(TCPIPProtocol)]
    DSTCPServerTransport1: TDSTCPServerTransport;
[!endif]
[!if=(HTTPProtocol)]
    DSHTTPService1: TDSHTTPService;
[!endif]
[!if=(HTTPSProtocol)]
    DSHTTPService2: TDSHTTPService;
    DSCertFiles1: TDSCertFiles;
[!endif]
[!if=(Authentication)]
    DSAuthenticationManager1: TDSAuthenticationManager;
[!endif]
[!if=(IncludeServerModule, "FALSE")]
[!if=(IncludeDataSnapConnectors)]
    DSHTTPServiceProxyDispatcher1: TDSHTTPServiceProxyDispatcher;
    DSProxyGenerator1: TDSProxyGenerator;
    DSServerMetaDataProvider1: TDSServerMetaDataProvider;
[!else]
[!if=(IncludeJavaScriptFiles)]
    DSProxyGenerator1: TDSProxyGenerator;
    DSServerMetaDataProvider1: TDSServerMetaDataProvider;
[!endif]
[!endif]
[!endif]
[!if=(IncludeJavaScriptFiles)]
[!if=(IncludeServerModule, "FALSE")]
    DSHTTPServiceFileDispatcher1: TDSHTTPServiceFileDispatcher;
[!endif]
[!endif]
[!if=(ServerMethodsClass)]
    DSServerClass1: TDSServerClass;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
[!endif]
[!if=(Authorization)]
    procedure DSAuthenticationManager1UserAuthorize(Sender: TObject;
      EventObject: TDSAuthorizeEventObject; var valid: Boolean);
[!endif]
[!if=(HTTPSProtocol)]
    procedure DSCertFiles1GetPEMFilePasskey(ASender: TObject;
      var APasskey: AnsiString);
[!endif]
[!if=(Authentication)]
    procedure DSAuthenticationManager1UserAuthenticate(Sender: TObject;
      const Protocol, Context, User, Password: string; var valid: Boolean;
      UserRoles: TStrings);
[!endif]
[!if=(ServiceMethods)]
    procedure ServiceStart(Sender: TService; var Started: Boolean);
[!endif]
[!if=(IncludeJavaScriptFiles)]
[!if=(IncludeServerModule, "FALSE")]
    procedure DataModuleCreate(Sender: TObject);
[!endif]
[!endif]
  private
    { Private declarations }
[!if=(ServiceMethods)]
  protected
    function DoStop: Boolean; override;
    function DoPause: Boolean; override;
    function DoContinue: Boolean; override;
    procedure DoInterrogate; override;
[!endif]
  public
[!if=(IncludeServerModule, "TRUE")]
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
[!endif]
[!if=(ServiceMethods)]
    function GetServiceController: TServiceController; override;
[!endif]
  end;

[!if=(RunDSServer)]
procedure RunDSServer;

[!endif]
[!if=(RunDSServer)]
[!else]
[!if=(IncludeServerModule, "FALSE")]
var
  [!FormIdent]: T[!FormIdent];

[!endif]
[!endif]
[!if=(IncludeServerModule, "TRUE")]
function DSServer: TDSServer;
[!if=(Authentication)]
function DSAuthenticationManager: TDSAuthenticationManager;
[!endif]

[!endif]
implementation

[!if=(ServerMethodsClass)]
uses Winapi.Windows, [!ServerMethodsUnitName];
[!else]
uses Winapi.Windows;
[!endif]

{$R *.dfm}

[!if=(IncludeServerModule, "TRUE")]
var
  FModule: TComponent;
  FDSServer: TDSServer;
[!if=(Authentication)]
  FDSAuthenticationManager: TDSAuthenticationManager;
[!endif]

function DSServer: TDSServer;
begin
  Result := FDSServer;
end;
[!if=(Authentication)]

function DSAuthenticationManager: TDSAuthenticationManager;
begin
  Result := FDSAuthenticationManager;
end;
[!endif]

[!endif]
[!if=(IncludeServerModule, "TRUE")]
constructor T[!FormIdent].Create(AOwner: TComponent);
begin
  inherited;
  FDSServer := DSServer1;
[!if=(Authentication)]
  FDSAuthenticationManager := DSAuthenticationManager1;
[!endif]
end;

destructor T[!FormIdent].Destroy;
begin
  inherited;
  FDSServer := nil;
[!if=(Authentication)]
  FDSAuthenticationManager := nil;
[!endif]
end;

[!endif]
[!if=(IncludeJavaScriptFiles)]
[!if=(IncludeServerModule, "FALSE")]
procedure T[!FormIdent].DataModuleCreate(Sender: TObject);
begin
  //write the JavaScript proxy on startup
  DSProxyGenerator1.Write;
end;

[!endif]
[!endif]
[!if=(ServerMethodsClass)]
procedure T[!FormIdent].DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := [!ServerMethodsUnitName].T[!ServerMethodsClassName];
end;

[!endif]
[!if=(Authentication)]
procedure T[!FormIdent].DSAuthenticationManager1UserAuthenticate(
  Sender: TObject; const Protocol, Context, User, Password: string;
  var valid: Boolean; UserRoles: TStrings);
begin
  { TODO : Validate the client user and password.
    If role-based authorization is needed, add role names to the UserRoles parameter  }
  valid := True;
end;

[!endif]
[!if=(Authorization)]
procedure T[!FormIdent].DSAuthenticationManager1UserAuthorize(
  Sender: TObject; EventObject: TDSAuthorizeEventObject;
  var valid: Boolean);
begin
  { TODO : Authorize a user to execute a method.
    Use values from EventObject such as UserName, UserRoles, AuthorizedRoles and DeniedRoles.
    Use DSAuthenticationManager1.Roles to define Authorized and Denied roles
    for particular server methods. }
  valid := True;
end;

[!endif]
[!if=(HTTPSProtocol)]
procedure T[!FormIdent].DSCertFiles1GetPEMFilePasskey(ASender: TObject;
  var APasskey: AnsiString);
begin
  APasskey := '[!KeyFilePassword]';
end;

[!endif]

[!if=(RunDSServer)]
procedure RunDSServer;
var
  LModule: T[!FormIdent];
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
begin
  Writeln(Format('Starting %s', [T[!FormIdent].ClassName]));
  LModule := T[!FormIdent].Create(nil);
  try
    LModule.DSServer1.Start;
    try
      Writeln('Press ESC to stop the server');
      LHandle := GetStdHandle(STD_INPUT_HANDLE);
      while True do
      begin
        Win32Check(ReadConsoleInput(LHandle, LInputRecord, 1, LEvent));
        if (LInputRecord.EventType = KEY_EVENT) and
        LInputRecord.Event.KeyEvent.bKeyDown and
        (LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then
          break;
      end;
    finally
      LModule.DSServer1.Stop;
    end;
  finally
    LModule.Free;
  end;
end;

[!endif]
[!if=(ServiceMethods)]
procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  [!FormIdent].Controller(CtrlCode);
end;

function T[!FormIdent].GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

function T[!FormIdent].DoContinue: Boolean;
begin
  Result := inherited;
  DSServer1.Start;
end;

procedure T[!FormIdent].DoInterrogate;
begin
  inherited;
end;

function T[!FormIdent].DoPause: Boolean;
begin
  DSServer1.Stop;
  Result := inherited;
end;

function T[!FormIdent].DoStop: Boolean;
begin
  DSServer1.Stop;
  Result := inherited;
end;

procedure T[!FormIdent].ServiceStart(Sender: TService; var Started: Boolean);
begin
  DSServer1.Start;
end;
[!endif]
[!if=(IncludeServerModule, "TRUE")]
initialization
  FModule := T[!FormIdent].Create(nil);
finalization
  FModule.Free;
[!endif]
end.
[!endif]
[!if=(DataSnapModuleIntf)]
//$$ -- WebModule Interface -- (stWebModuleIntf)
 // { Placeholder for C++}
[!endif]
[!if=(DataSnapModuleDFMSource)]
object [!FormIdent]: T[!FormIdent]
  OldCreateOrder = False
[!if=(IncludeJavaScriptFiles)]
  OnCreate = DataModuleCreate
[!endif]
[!if=(ServiceMethods)]
  DisplayName = '[!FormIdent]'
  OnStart = ServiceStart
[!endif]
  Left = 271
  Top = 114
  Height = 271
  Width = 415
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
[!if=(TCPIPProtocol)]
  object DSTCPServerTransport1: TDSTCPServerTransport
[!if=(Authentication)]
    AuthenticationManager = DSAuthenticationManager1
[!endif]
    Server = DSServer1
[!if=(IncludeFilters)]
    Filters = <
[!if=(IncludeEncryptionFilters)]
      item
        FilterId = 'PC1'
      end
      item
        FilterId = 'RSA'
      end
[!endif]
[!if=(IncludeCompressionFilter)]
      item
        FilterId = 'ZLibCompression'
      end
[!endif]
    >
[!endif]
    Left = 96
    Top = 73
    Port = [!TCPIPPort]
  end
[!endif]
[!if=(HTTPProtocol)]
  object DSHTTPService1: TDSHTTPService
[!if=(Authentication)]
    AuthenticationManager = DSAuthenticationManager1
[!endif]
    Active = False
    Server = DSServer1
[!if=(IncludeFilters)]
    Filters = <
[!if=(IncludeEncryptionFilters)]
      item
        FilterId = 'PC1'
      end
      item
        FilterId = 'RSA'
      end
[!endif]
[!if=(IncludeCompressionFilter)]
      item
        FilterId = 'ZLibCompression'
      end
[!endif]
    >
[!endif]
    Left = 96
    Top = 135
    HTTPPort = [!HTTPPort]
  end
[!endif]
[!if=(HTTPSProtocol)]
  object DSCertFiles1: TDSCertFiles
    RootCertFile = '[!RootCertFile]'
    CertFile = '[!CertFile]'
    KeyFile = '[!KeyFile]'
    OnGetPEMFilePasskey = DSCertFiles1GetPEMFilePasskey
    Left = 200
    Top = 197
  end
  object DSHTTPService2: TDSHTTPService
[!if=(Authentication)]
    AuthenticationManager = DSAuthenticationManager1
[!endif]
    Active = False
    Server = DSServer1
    CertFiles = DSCertFiles1
[!if=(IncludeFilters)]
    Filters = <
[!if=(IncludeCompressionFilter)]
      item
        FilterId = 'ZLibCompression'
      end
[!endif]
    >
[!endif]
    Left = 200
    Top = 135
    HTTPPort = [!HTTPSPort]
  end
[!endif]
[!if=(Authentication)]
  object DSAuthenticationManager1: TDSAuthenticationManager
    OnUserAuthenticate = DSAuthenticationManager1UserAuthenticate
[!if=(Authorization)]
    OnUserAuthorize = DSAuthenticationManager1UserAuthorize
[!endif]
    Left = 96
    Top = 197
    Roles = <>
  end
[!endif]
[!if=(ServerMethodsClass)]
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    LifeCycle = 'Session'
    Left = 200
    Top = 11
  end
[!endif]
[!if=(IncludeServerModule, "FALSE")]
[!if=(IncludeDataSnapConnectors)]
  object DSHTTPServiceProxyDispatcher1: TDSHTTPServiceProxyDispatcher
[!if=(HTTPProtocol)]
    Service = DSHTTPService1
[!endif]
    WebFileExtensions = <
      item
        MimeType = 'application/x-zip-compressed'
        Extensions = 'zip'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '\proxy\*'
      end
      item
        DirectoryAction = dirExclude
        DirectoryMask = '\proxy\*\*'
      end>
    RootDirectory = '.'
    DSProxyGenerator = DSProxyGenerator1
    RequiredProxyFilesPath = 'proxy'
    Left = 312
    Top = 160
  end
  object DSProxyGenerator1: TDSProxyGenerator
    MetaDataProvider = DSServerMetaDataProvider1
[!if=(IncludeJavaScriptFiles)]
    TargetUnitName = 'ServerFunctions.js'
    TargetDirectory = 'js'
    Writer = 'Java Script REST'
[!endif]
    Left = 320
    Top = 96
  end
  object DSServerMetaDataProvider1: TDSServerMetaDataProvider
    Server = DSServer1
    Left = 320
    Top = 40
  end
[!else]
[!if=(IncludeJavaScriptFiles)]
  object DSServerMetaDataProvider1: TDSServerMetaDataProvider
    Server = DSServer1
    Left = 272
    Top = 88
  end
  object DSProxyGenerator1: TDSProxyGenerator
    MetaDataProvider = DSServerMetaDataProvider1
    TargetUnitName = 'ServerFunctions.js'
    TargetDirectory = 'js'
    Writer = 'Java Script REST'
    Left = 256
    Top = 168
  end
[!endif]
[!endif]
[!endif]
[!if=(IncludeJavaScriptFiles)]
[!if=(IncludeServerModule, "FALSE")]
  object DSHTTPServiceFileDispatcher1: TDSHTTPServiceFileDispatcher
[!if=(HTTPProtocol)]
    Service = DSHTTPService1
[!endif]
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/html'
        Extensions = 'html;htm'
      end
      item
        MimeType = 'text/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpeg;jpg'
      end
      item
        MimeType = 'image/x-png'
        Extensions = 'png'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '*'
      end
      item
        DirectoryAction = dirExclude
        DirectoryMask = '\templates\*'
      end>
    RootDirectory = '.'
    Left = 312
    Top = 216
  end
[!endif]
[!endif]
  end
end
[!endif]
[!if=(DataSnapVCLFormSource)]
[!if=(Diagnostics)]
// DataVCLFormSource
[!endif]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
unit [!ModuleIdent];

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  T[!FormIdent] = class(T[!AncestorIdent])
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  [!FormIdent]: T[!FormIdent];

implementation

{$R *.dfm}

end.
[!endif]
[!if=(DataSnapVCLFormIntf)]
 // {Placeholder for C++}
[!endif]
[!if=(DSServerModuleTemplateIntf)]
// Place holder for C++
[!endif]
[!if=(DSServerModuleTemplate)]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
unit [!ModuleIdent];

interface

uses System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth;

type
[!if=(IncludeComments)]
[!endif]
[!if=(MethodInfoOn)]
{$METHODINFO ON}
[!endif]
  T[!FormIdent] = class(T[!AncestorIdent])
  private
    { Private declarations }
  public
    { Public declarations }
[!if=(IncludeSampleMethods)]
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
[!endif]
  end;
[!if=(MethodInfoOn)]
{$METHODINFO OFF}
[!endif]

implementation

{$R *.dfm}
[!if=(IncludeSampleMethods)]

uses System.StrUtils;

function T[!FormIdent].EchoString(Value: string): string;
begin
  Result := Value;
end;

function T[!FormIdent].ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;
[!endif]

end.
[!endif]
[!if=(DataSnapServerMethodsClassTemplateIntf)]
// Placeholder for C++
[!endif]
[!if=(DataSnapServerMethodsClassTemplate)]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
unit [!ModuleIdent];

interface

uses System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth;

type
[!if=(IncludeComments)]
[!endif]
{$METHODINFO ON}
  T[!FormIdent] = class(T[!AncestorIdent])
  private
    { Private declarations }
  public
    { Public declarations }
[!if=(IncludeSampleMethods)]
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
[!endif]
  end;
{$METHODINFO OFF}

implementation

[!if=(IncludeSampleMethods)]

uses System.StrUtils;

function T[!FormIdent].EchoString(Value: string): string;
begin
  Result := Value;
end;

function T[!FormIdent].ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;
[!endif]
end.
[!endif]
[!if=(DataSnapWebModuleIntf, "TRUE")]
// Placeholder for C++
[!endif]
[!if=(DataSnapWebModuleSource, "TRUE")]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
[!if=(Customize_CommentModule)]
[!Customize_CommentModuleText]
[!endif]
unit [!ModuleIdent];

interface

uses
[!if=(DataSnapREST, "TRUE")]
  System.SysUtils, System.Classes, Web.HTTPApp, Datasnap.DSHTTPCommon,
  Datasnap.DSHTTPWebBroker, Datasnap.DSServer,
  Web.WebFileDispatcher, Web.HTTPProd,
  DSAuth,
[!if=(IncludeDataSnapConnectors)]
  Datasnap.DSProxyDispatcher, Datasnap.DSProxyJavaAndroid,
  Datasnap.DSProxyJavaBlackBerry, Datasnap.DSProxyObjectiveCiOS,
  Datasnap.DSProxyCsharpSilverlight,
[!endif]
  Datasnap.DSProxyJavaScript;
[!else]
  System.SysUtils, System.Classes, Web.HTTPApp, Datasnap.DSHTTPCommon,
  Datasnap.DSHTTPWebBroker, Datasnap.DSServer,
[!if=(IncludeDataSnapConnectors)]
  Datasnap.DSProxyDispatcher, Datasnap.DSProxyJavaAndroid, Datasnap.DSProxyJavaBlackBerry,
  Datasnap.DSProxyObjectiveCiOS, Datasnap.DSProxyCsharpSilverlight,
[!endif]
  Datasnap.DSAuth;
[!endif]

type
  T[!FormIdent] = class(T[!AncestorIdent])
    DSHTTPWebDispatcher1: TDSHTTPWebDispatcher;
[!if=(IncludeServerModule, "FALSE")]
    DSServer1: TDSServer;
[!endif]
[!if=(Authentication)]
[!if=(IncludeServerModule, "FALSE")]
    DSAuthenticationManager1: TDSAuthenticationManager;
[!endif]
[!endif]
[!if=(ServerMethodsClass)]
[!if=(IncludeServerModule, "FALSE")]
    DSServerClass1: TDSServerClass;
[!endif]
[!endif]
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
    ServerFunctionInvoker: TPageProducer;
    ReverseString: TPageProducer;
[!endif]
    WebFileDispatcher1: TWebFileDispatcher;
    DSProxyGenerator1: TDSProxyGenerator;
    DSServerMetaDataProvider1: TDSServerMetaDataProvider;
[!else]
[!if=(IncludeDataSnapConnectors)]
    DSProxyGenerator1: TDSProxyGenerator;
    DSServerMetaDataProvider1: TDSServerMetaDataProvider;
[!endif]
[!endif]
[!if=(IncludeDataSnapConnectors)]
    DSProxyDispatcher1: TDSProxyDispatcher;
[!endif]
[!if=(ServerMethodsClass)]
[!if=(IncludeServerModule, "FALSE")]
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
[!endif]
[!endif]
[!if=(Authorization)]
[!if=(IncludeServerModule, "FALSE")]
    procedure DSAuthenticationManager1UserAuthorize(Sender: TObject;
      EventObject: TDSAuthorizeEventObject; var valid: Boolean);
[!endif]
[!endif]
[!if=(Authentication)]
[!if=(IncludeServerModule, "FALSE")]
    procedure DSAuthenticationManager1UserAuthenticate(Sender: TObject;
      const Protocol, Context, User, Password: string; var valid: Boolean;
      UserRoles: TStrings);
[!endif]
[!endif]
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
    procedure ServerFunctionInvokerHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: string; TagParams: TStrings; var ReplaceText: string);
    procedure WebModuleDefaultAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleBeforeDispatch(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
[!else]
    procedure [!FormIdent]DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
[!endif]
    procedure WebFileDispatcher1BeforeDispatch(Sender: TObject;
      const AFileName: string; Request: TWebRequest; Response: TWebResponse;
      var Handled: Boolean);
[!else]
    procedure [!FormIdent]DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
[!endif]
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
    FServerFunctionInvokerAction: TWebActionItem;
    function AllowServerFunctionInvoker: Boolean;
[!endif]
[!endif]
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = T[!FormIdent];

implementation

[!if=(IncludeServerModule)]
[!if=(ServerMethodsClass)]
uses [!ServerMethodsUnitName], [!ServerModuleUnitName], Web.WebReq;
[!else]
uses [!ServerModuleUnitName], Web.WebReq;
[!endif]
[!else]
[!if=(ServerMethodsClass)]
uses [!ServerMethodsUnitName], Web.WebReq;
[!else]
uses Web.WebReq;
[!endif]
[!endif]

{$R *.dfm}

[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
[!else]
procedure T[!FormIdent].[!FormIdent]DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>xxDataSnap Server</body></html>';
end;

[!endif]
[!else]
procedure T[!FormIdent].[!FormIdent]DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>yyDataSnap Server</body></html>';
end;

[!endif]
[!if=(ServerMethodsClass)]
[!if=(IncludeServerModule, "FALSE")]
procedure T[!FormIdent].DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := [!ServerMethodsUnitName].T[!ServerMethodsClassName];
end;

[!endif]
[!endif]
[!if=(Authentication)]
[!if=(IncludeServerModule, "FALSE")]
procedure T[!FormIdent].DSAuthenticationManager1UserAuthenticate(
  Sender: TObject; const Protocol, Context, User, Password: string;
  var valid: Boolean; UserRoles: TStrings);
begin
  valid := True;
end;

[!endif]
[!endif]
[!if=(Authorization)]
[!if=(IncludeServerModule, "FALSE")]
procedure T[!FormIdent].DSAuthenticationManager1UserAuthorize(
  Sender: TObject; EventObject: TDSAuthorizeEventObject; 
  var valid: Boolean);
begin
  valid := True;
end;

[!endif]
[!endif]
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
procedure T[!FormIdent].ServerFunctionInvokerHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: string; TagParams: TStrings; var ReplaceText: string);
begin
  if SameText(TagString, 'urlpath') then
    ReplaceText := string(Request.InternalScriptName)
  else if SameText(TagString, 'port') then
    ReplaceText := IntToStr(Request.ServerPort)
  else if SameText(TagString, 'host') then
    ReplaceText := string(Request.Host)
[!if=(IncludeSampleMethods)]
  else if SameText(TagString, 'classname') then
    ReplaceText := [!ServerMethodsUnitName].T[!ServerMethodsClassName].ClassName
[!endif]
  else if SameText(TagString, 'loginrequired') then
    if DSHTTPWebDispatcher1.AuthenticationManager <> nil then
      ReplaceText := 'true'
    else
      ReplaceText := 'false'
  else if SameText(TagString, 'serverfunctionsjs') then
    ReplaceText := string(Request.InternalScriptName) + '/js/serverfunctions.js'
  else if SameText(TagString, 'servertime') then
    ReplaceText := DateTimeToStr(Now)
  else if SameText(TagString, 'serverfunctioninvoker') then
    if AllowServerFunctionInvoker then
      ReplaceText :=
      '<div><a href="' + string(Request.InternalScriptName) +
      '/ServerFunctionInvoker" target="_blank">Server Functions</a></div>'
    else
      ReplaceText := '';
end;

procedure T[!FormIdent].WebModuleDefaultAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  if (Request.InternalPathInfo = '') or (Request.InternalPathInfo = '/')then
    Response.Content := ReverseString.Content
  else
    Response.SendRedirect(Request.InternalScriptName + '/');
end;

procedure T[!FormIdent].WebModuleBeforeDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  if FServerFunctionInvokerAction <> nil then
    FServerFunctionInvokerAction.Enabled := AllowServerFunctionInvoker;
end;

function T[!FormIdent].AllowServerFunctionInvoker: Boolean;
begin
  Result := (Request.RemoteAddr = '127.0.0.1') or
    (Request.RemoteAddr = '0:0:0:0:0:0:0:1') or (Request.RemoteAddr = '::1');
end;

[!endif]
procedure T[!FormIdent].WebFileDispatcher1BeforeDispatch(Sender: TObject;
  const AFileName: string; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
var
  D1, D2: TDateTime;
begin
  Handled := False;
  if SameFileName(ExtractFileName(AFileName), 'serverfunctions.js') then
    if not FileExists(AFileName) or (FileAge(AFileName, D1) and FileAge(WebApplicationFileName, D2) and (D1 < D2)) then
    begin
      DSProxyGenerator1.TargetDirectory := ExtractFilePath(AFileName);
      DSProxyGenerator1.TargetUnitName := ExtractFileName(AFileName);
      DSProxyGenerator1.Write;
    end;
end;

[!endif]
procedure T[!FormIdent].WebModuleCreate(Sender: TObject);
begin
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
  FServerFunctionInvokerAction := ActionByName('ServerFunctionInvokerAction');
[!endif]
[!endif]
[!if=(IncludeServerModule)]
[!if=(IncludeDataSnapConnectors)]
  DSServerMetaDataProvider1.Server := DSServer;
[!else]
[!if=(IncludeDataSnapConnectors)]
  DSServerMetaDataProvider1.Server := DSServer;
[!endif]
[!endif]
  DSHTTPWebDispatcher1.Server := DSServer;
[!if=(Authentication)]
  DSHTTPWebDispatcher1.AuthenticationManager := DSAuthenticationManager;
[!endif]
[!endif]
end;

initialization
finalization
  Web.WebReq.FreeWebModules;

end.
[!endif]
[!if=(DataSnapWebModuleDFMSource)]
object [!FormIdent]: T[!FormIdent]
  OldCreateOrder = False
  OnCreate = WebModuleCreate
[!if=(DataSnapREST)]
[!if=(IncludeSampleWebFiles)]
  Actions = <
    item
      Name = 'ReverseStringAction'
      PathInfo = '/ReverseString'
      Producer = ReverseString
    end
    item
      Name = 'ServerFunctionInvokerAction'
      PathInfo = '/ServerFunctionInvoker'
      Producer = ServerFunctionInvoker
    end
    item
      Default = True
      Name = 'DefaultAction'
      PathInfo = '/'
      OnAction = WebModuleDefaultAction
    end>
  BeforeDispatch = WebModuleBeforeDispatch
[!else]
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = [!FormIdent]DefaultHandlerAction
    end>
[!endif]
[!else]
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = [!FormIdent]DefaultHandlerAction
    end>
[!endif]
[!if=(DataSnapREST)]
  Height = 333
  Width = 414
[!else]
  Height = 230
  Width = 415
[!endif]
[!if=(IncludeServerModule, "FALSE")]
  object DSServer1: TDSServer
    AutoStart = True
    HideDSAdmin = False
    Left = 96
    Top = 11
  end
[!endif]
  object DSHTTPWebDispatcher1: TDSHTTPWebDispatcher
[!if=(Authentication)]
[!if=(IncludeServerModule, "FALSE")]
    AuthenticationManager = DSAuthenticationManager1
[!endif]
[!if=(IncludeFilters)]
    Filters = <
[!if=(IncludeEncryptionFilters)]
      item
        FilterId = 'PC1'
      end
      item
        FilterId = 'RSA'
      end
[!endif]
[!if=(IncludeCompressionFilter)]
      item
        FilterId = 'ZLibCompression'
      end
[!endif]
    >
[!endif]
[!endif]
    RESTContext = 'rest'
[!if=(IncludeServerModule, "FALSE")]
    Server = DSServer1
[!endif]
    DSHostname = 'localhost'
    DSPort = 211
    Filters = <>
    WebDispatch.MethodType = mtAny
    WebDispatch.PathInfo = 'datasnap*'
    Left = 96
    Top = 75
  end
[!if=(Authentication)]
[!if=(IncludeServerModule, "FALSE")]
  object DSAuthenticationManager1: TDSAuthenticationManager
    OnUserAuthenticate = DSAuthenticationManager1UserAuthenticate
[!if=(Authorization)]
    OnUserAuthorize = DSAuthenticationManager1UserAuthorize
[!endif]
    Left = 200
    Top = 139
  end
[!endif]
[!endif]
[!if=(ServerMethodsClass)]
[!if=(IncludeServerModule, "FALSE")]
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    LifeCycle = 'Session'
    Left = 200
    Top = 11
  end
[!endif]
[!endif]
[!if=(DataSnapREST)]
[!if=(IncludeSampleWebFiles)]
  object ServerFunctionInvoker: TPageProducer
    HTMLFile = 'Templates\ServerFunctionInvoker.html'
    OnHTMLTag = ServerFunctionInvokerHTMLTag
    Left = 56
    Top = 184
  end
  object ReverseString: TPageProducer
    HTMLFile = 'templates\ReverseString.html'
    OnHTMLTag = ServerFunctionInvokerHTMLTag
    Left = 168
    Top = 184
  end
[!endif]
  object WebFileDispatcher1: TWebFileDispatcher
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/x-png'
        Extensions = 'png'
      end
      item
        MimeType = 'text/html'
        Extensions = 'htm;html'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpg;jpeg;jpe'
      end
      item
        MimeType = 'image/gif'
        Extensions = 'gif'
      end>
    BeforeDispatch = WebFileDispatcher1BeforeDispatch
    Left = 56
    Top = 136
  end
  object DSProxyGenerator1: TDSProxyGenerator
    ExcludeClasses = 'DSMetadata'
    MetaDataProvider = DSServerMetaDataProvider1
    Writer = 'Java Script REST'
    Left = 48
    Top = 248
  end
  object DSServerMetaDataProvider1: TDSServerMetaDataProvider
[!if=(IncludeServerModule, "FALSE")]
    Server = DSServer1
[!endif]
    Left = 208
    Top = 248
  end
[!if=(IncludeDataSnapConnectors)]
  object DSProxyDispatcher1: TDSProxyDispatcher
    RequiredProxyFilesPath = 'proxy'
    DSProxyGenerator = DSProxyGenerator1
    Left = 264
    Top = 128
  end
[!endif]
[!else]
[!if=(IncludeDataSnapConnectors)]
  object DSProxyDispatcher1: TDSProxyDispatcher
    RequiredProxyFilesPath = 'proxy'
    DSProxyGenerator = DSProxyGenerator1
    Left = 320
    Top = 80
  end
  object DSProxyGenerator1: TDSProxyGenerator
    MetaDataProvider = DSServerMetaDataProvider1
    Left = 320
    Top = 16
  end
  object DSServerMetaDataProvider1: TDSServerMetaDataProvider
[!if=(IncludeServerModule, "FALSE")]
    Server = DSServer1
[!endif]
    Left = 320
    Top = 160
  end
[!endif]
[!endif]
end
[!endif]

[!if=(JSProxyWriterSource)]
[!endif]
