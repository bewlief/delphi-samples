
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
[!if=(IsapiSource, "TRUE")]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
library [!ProjectName];

uses Winapi.ActiveX, System.Win.ComObj, Web.WebBroker, Web.Win.ISAPIApp,
[!if=(DBXTerminateThreads, "TRUE")]
  Web.Win.ISAPIThreadPool, Data.DBXCommon, Datasnap.DSService;
[!else]
  Web.Win.ISAPIThreadPool;
[!endif]

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

[!if=(DBXTerminateThreads, "TRUE")]
procedure TerminateThreads;
begin
  TDSSessionManager.Instance.Free;
  Data.DBXCommon.TDBXScheduler.Instance.Free;
end;

[!endif]
begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
[!if=(SetWebModuleClass, "TRUE")]
  Application.WebModuleClass := WebModuleClass;
[!endif]
[!if=(DBXTerminateThreads, "TRUE")]
  TISAPIApplication(Application).OnTerminate := TerminateThreads;
[!endif]
  Application.Run;
end.
[!endif]
[!if=(WinCGISource, "TRUE")]
program [!ProjectName];

{$APPTYPE CONSOLE}

uses Web.WebBroker, CGIApp;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.
[!endif]
[!if=(CGISource, "TRUE")]
program [!ProjectName];

{$APPTYPE CONSOLE}

uses Web.WebBroker, CGIApp;

{$R *.res}

begin
  Application.Initialize;
[!if=(SetWebModuleClass, "TRUE")]
  Application.WebModuleClass := WebModuleClass;
[!endif]
  Application.Run;
end.
[!endif]
[!if=(IndyConsoleProjectSource, "TRUE")]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
program [!ProjectName];
{$APPTYPE CONSOLE}

uses System.SysUtils, Winapi.Windows, IdHTTPWebBrokerBridge,
[!if=(HTTPS, "TRUE")]
  IdSSLOpenSSL,
[!endif]
  Web.WebReq,
[!if=(DBXTerminateThreads, "TRUE")]
  Web.WebBroker, Datasnap.DSService;
[!else]
  Web.WebBroker;
[!endif]

{$R *.res}

[!if=(DBXTerminateThreads, "TRUE")]
procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
    TDSSessionManager.Instance.TerminateAllSessions;
end;

[!endif]
[!if=(HTTPS, "TRUE")]
type
  TGetSSLPassword = class
    procedure OnGetSSLPassword(var APassword: AnsiString);
  end;

procedure TGetSSLPassword.OnGetSSLPassword(var APassword: AnsiString);
begin
  APassword := '[!KeyFilePassword]';
end;

[!endif]
procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
  LServer: TIdHTTPWebBrokerBridge;
[!if=(HTTPS, "TRUE")]
  LGetSSLPassword: TGetSSLPassword;
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
[!endif]
begin
  Writeln(Format('Starting HTTP Server or port %d', [APort]));
[!if=(HTTPS, "TRUE")]
  LGetSSLPassword := nil;
[!endif]
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
[!if=(HTTPS, "TRUE")]
    LGetSSLPassword := TGetSSLPassword.Create;
    LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(LServer);
    LIOHandleSSL.SSLOptions.CertFile := '[!CertFile]';
    LIOHandleSSL.SSLOptions.RootCertFile := '[!RootCertFile]';
    LIOHandleSSL.SSLOptions.KeyFile := '[!KeyFile]';
    LIOHandleSSL.OnGetPassword := LGetSSLPassword.OnGetSSLPassword;
    LServer.IOHandler := LIOHandleSSL;
[!endif]
    LServer.DefaultPort := APort;
    LServer.Active := True;
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
[!if=(DBXTerminateThreads, "TRUE")]
    TerminateThreads();
[!endif]
  finally
    LServer.Free;
[!if=(HTTPS, "TRUE")]
    LGetSSLPassword.Free;
[!endif]
  end;
end;

begin
  try
[!if=(SetWebModuleClass, "TRUE")]
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
[!endif]
    RunServer([!HTTPPort]);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end.
[!endif]
[!if=(WebModuleSource, "TRUE")]
unit [!ModuleIdent];

interface

uses System.SysUtils, System.Classes, Web.HTTPApp;

type
  T[!FormIdent] = class(T[!AncestorIdent])
    procedure [!FormIdent]DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = T[!FormIdent];

implementation

[!if=(Clx, "TRUE")]
{$R *.xfm}
[!else]
{$R *.dfm}
[!endif]

procedure T[!FormIdent].[!FormIdent]DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>Web Server Application</body></html>';
end;

end.
[!endif]
[!if=(WebModuleIntf, "TRUE")]
//$$ -- WebModule Interface -- (stWebModuleIntf)
 // { Placeholder for C++}
[!endif]
[!if=(WebModuleDFMSource)]
object [!FormIdent]: T[!FormIdent]
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = [!FormIdent]DefaultHandlerAction
    end>
  Height = 230
  Width = 415
end
[!endif]
[!if=(COMProjectSource, "TRUE")]
program [!ProjectName];

{$APPTYPE GUI}

uses
[!if=(Clx, "TRUE")]
  QForms,
[!else]
  Vcl.Forms,
[!endif]
  Web.WebReq,
  SockApp;

{$R *.res}

begin
[!if=(SetWebModuleClass, "TRUE")]
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
[!endif]
  Application.Initialize;
  Application.Run;
end.
[!endif]
[!if=(IndyFormProjectSource, "TRUE")]
program [!ProjectName];
{$APPTYPE GUI}

uses Vcl.Forms, Web.WebReq, IdHTTPWebBrokerBridge;

{$R *.res}

begin
[!if=(SetWebModuleClass, "TRUE")]
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
[!endif]
  Application.Initialize;
  Application.Run;
end.
[!endif]
[!if=(COMConsoleSource, "TRUE")]
unit [!ModuleIdent];

interface

uses
[!if=(Clx, "TRUE")]
  SysUtils, Classes, QForms;
[!else]
  System.SysUtils, System.Classes, Vcl.Forms;
[!endif]

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

uses SockApp;

[!if=(Clx, "TRUE")]
{$R *.xfm}
[!else]
{$R *.dfm}
[!endif]

initialization
  TWebAppSockObjectFactory.Create('%3:s')

end.
[!endif]
[!if=(IndyFormConsoleSource, "TRUE")]
[!if=(Customize_TimeStampModule)]
// [!Customize_TimeStampText]
[!endif]
unit [!ModuleIdent];

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.AppEvnts, Vcl.StdCtrls, IdHTTPWebBrokerBridge, Web.HTTPApp;

type
  T[!FormIdent] = class(T[!AncestorIdent])
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditPort: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    ButtonOpenBrowser: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonOpenBrowserClick(Sender: TObject);
  private
    FServer: TIdHTTPWebBrokerBridge;
    procedure StartServer;
[!if=(HTTPS, "TRUE)]
    procedure OnGetSSLPassword(var APassword: AnsiString);
[!endif]
    { Private declarations }
  public
    { Public declarations }
  end;

var
  [!FormIdent]: T[!FormIdent];

implementation

{$R *.dfm}

uses
[!if=(HTTPS, "TRUE")]
  IdSSLOpenSSL,
[!endif]
[!if=(DBXTerminateThreads, "TRUE")]
  Winapi.ShellApi, Datasnap.DSService;
[!else]
  Winapi.ShellApi;
[!endif]

procedure T[!FormIdent].ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  ButtonStart.Enabled := not FServer.Active;
  ButtonStop.Enabled := FServer.Active;
  EditPort.Enabled := not FServer.Active;
end;

procedure T[!FormIdent].ButtonOpenBrowserClick(Sender: TObject);
var
  LURL: string;
begin
  StartServer;
[!if=(HTTPS, "TRUE")]
  LURL := Format('https://localhost:%s', [EditPort.Text]);
[!else]
  LURL := Format('http://localhost:%s', [EditPort.Text]);
[!endif]
  ShellExecute(0,
        nil,
        PChar(LURL), nil, nil, SW_SHOWNOACTIVATE);
end;

procedure T[!FormIdent].ButtonStartClick(Sender: TObject);
begin
  StartServer;
end;

[!if=(DBXTerminateThreads, "TRUE")]
procedure TerminateThreads;
begin
  if TDSSessionManager.Instance <> nil then
    TDSSessionManager.Instance.TerminateAllSessions;
end;

[!endif]
procedure T[!FormIdent].ButtonStopClick(Sender: TObject);
begin
[!if=(DBXTerminateThreads, "TRUE")]
  TerminateThreads;
[!endif]
  FServer.Active := False;
  FServer.Bindings.Clear;
end;

procedure T[!FormIdent].FormCreate(Sender: TObject);
[!if=(HTTPS, "TRUE")]
var
  LIOHandleSSL: TIdServerIOHandlerSSLOpenSSL;
[!endif]
begin
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
[!if=(HTTPS, "TRUE")]
  LIOHandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(FServer);
  LIOHandleSSL.SSLOptions.CertFile := '[!CertFile]';
  LIOHandleSSL.SSLOptions.RootCertFile := '[!RootCertFile]';
  LIOHandleSSL.SSLOptions.KeyFile := '[!KeyFile]';
  LIOHandleSSL.OnGetPassword := OnGetSSLPassword;
  FServer.IOHandler := LIOHandleSSL;
[!endif]
end;
[!if=(HTTPS, "TRUE")]

procedure T[!FormIdent].OnGetSSLPassword(var APassword: AnsiString);
begin
  APassword := '[!KeyFilePassword]';
end;
[!endif]

procedure T[!FormIdent].StartServer;
begin
  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := StrToInt(EditPort.Text);
    FServer.Active := True;
  end;
end;

end.
[!endif]
[!if=(IndyFormConsoleIntf, "TRUE")]
  // {Placeholder for C++}
[!endif]
[!if=(IndyFormConsoleDFMSource, "TRUE")]
object [!FormIdent]: T[!FormIdent]
  Left = 271
  Top = 114
  Caption = '[!FormIdent]'
  ClientHeight = 235
  ClientWidth = 399
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 48
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object ButtonStart: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = ButtonStartClick
  end
  object ButtonStop: TButton
    Left = 105
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = ButtonStopClick
  end
  object EditPort: TEdit
    Left = 24
    Top = 67
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '[!HTTPPort]'
  end
  object ButtonOpenBrowser: TButton
    Left = 24
    Top = 112
    Width = 107
    Height = 25
    Caption = 'Open Browser'
    TabOrder = 3
    OnClick = ButtonOpenBrowserClick
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 288
    Top = 24
  end
end
[!endif]
[!if=(COMConsoleIntf, "TRUE")]
 //$$ -- COMApp Form Interface -- (stCOMConsoleIntf)
 // {Placeholder for C++}
[!endif]
[!if=(Apache, "TRUE")]
library [!ProjectName];

uses Web.WebBroker, ApacheApp;

{$R *.res}

exports
  apache_module name '[!ProjectName]_module';

begin
  Application.Initialize;
  Application.Run;
end.
[!endif]
[!if=(ApacheTwo, "TRUE")]
library [!ProjectName];

uses Web.WebBroker, ApacheTwoApp;

{$R *.res}

exports
  apache_module name '[!ProjectName]_module';

begin
  Application.Initialize;
  Application.Run;
end.
[!endif]
[!if=(SOAPWebModuleSource, "TRUE")]
unit [!ModuleIdent];

interface

uses System.SysUtils, System.Classes, Web.HTTPApp;

type
  T[!FormIdent] = class(T[!AncestorIdent])
    HTTPSoapDispatcher1: THTTPSoapDispatcher;
    HTTPSoapPascalInvoker1: THTTPSoapPascalInvoker;
    WSDLHTMLPublish1: TWSDLHTMLPublish;
    procedure [!FormIdent]DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = T[!FormIdent];

implementation

[!if=(Clx, "TRUE")]
{$R *.xfm}
[!else]
{$R *.dfm}
[!endif]

procedure T[!FormIdent].[!FormIdent]DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  WSDLHTMLPublish1.ServiceInfo(Sender, Request, Response, Handled);
end;

end.
[!endif]
[!if=(SOAPWebModuleIntf, "TRUE")]
 //$$ -- SOAP Web Module Interface -- (stSOAPWebModuleIntf)
 // {Placeholder for C++}
[!endif]
[!if=(SOAPCOMConsoleSource, "TRUE")]
unit [!ModuleIdent];

interface

uses
[!if=(Clx, "TRUE")]
  SysUtils, Classes, QForms;
[!else]
  System.SysUtils, System.Classes, Vcl.Forms;
[!endif]

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

uses SockApp;

[!if=(Clx, "TRUE")]
{$R *.xfm}
[!else]
{$R *.dfm}
[!endif]

initialization
  TWebAppSockObjectFactory.Create('%3:s');

end.
[!endif]
[!if=(SOAPCOMConsoleIntf, "TRUE")]
 //$$ -- SOAP Form Interface -- (stSOAPCOMConsoleIntf)
 // {Placeholder for C++}
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

uses System.SysUtils, System.Classes, Web.HTTPApp, Datasnap.DSHTTPCommon,
  Datasnap.DSHTTPWebBroker, Datasnap.DSServer;

type
  T[!FormIdent] = class(T[!AncestorIdent])
    DSServer1: TDSServer;
    DSHTTPWebDispatcher1: TDSHTTPWebDispatcher;
    procedure [!FormIdent]DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = T[!FormIdent];

implementation

[!if=(Clx, "TRUE")]
{$R *.xfm}
[!else]
{$R *.dfm}
[!endif]

procedure T[!FormIdent].[!FormIdent]DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html><heading/><body>DataSnap Server</body></html>';
end;


