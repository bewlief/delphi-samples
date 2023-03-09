
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
//---------------------------------------------------------------------------
#include <ActiveX.hpp>
#include <ComObj.hpp>
#include <WebBroker.hpp>
#include <ISAPIApp.hpp>
#include <ISAPIThreadPool.hpp>
#include <Isapi2.hpp>
[!if=(DBXTerminateThreads, "TRUE")]
#include <DBXCommon.hpp>
#include <DSService.hpp>
[!endif]
#pragma hdrstop

#define Application Webbroker::Application

#pragma link "isapiapp.obj"
#pragma link "isapithreadpool.obj"
#pragma link "webbroker.obj"
#pragma package(smart_init)

[!if=(SetWebModuleClass, "TRUE")]
extern PACKAGE TComponentClass WebModuleClass;
[!endif]
//---------------------------------------------------------------------------
[!if=(DBXTerminateThreads, "TRUE")]
void _fastcall TerminateThreads()
{
  delete TDSSessionManager::Instance;
  delete TDBXScheduler::Instance;
}

[!endif]
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try
  {
    if (reason == DLL_PROCESS_ATTACH) {
      CoInitFlags = COINIT_MULTITHREADED;
      Application->Initialize();
[!if=(SetWebModuleClass, "TRUE")]
     Application->WebModuleClass = WebModuleClass;
[!endif]
[!if=(DBXTerminateThreads, "TRUE")]
      ((TISAPIApplication*)Application)->OnTerminate = TerminateThreads;
[!endif]
      Application->Run();
    }
  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
extern "C"
{
  BOOL __declspec(dllexport) WINAPI GetExtensionVersion(Isapi2::THSE_VERSION_INFO &Ver)
  {
    return Isapithreadpool::GetExtensionVersion(Ver);
  }
  //---------------------------------------------------------------------------
  unsigned __declspec(dllexport) WINAPI HttpExtensionProc(Isapi2::TEXTENSION_CONTROL_BLOCK &ECB)
  {
    return Isapithreadpool::HttpExtensionProc(ECB);
  }
  //---------------------------------------------------------------------------
  BOOL __declspec(dllexport) WINAPI TerminateExtension(int dwFlags)
  {
    return Isapithreadpool::TerminateExtension(dwFlags);
  }
}
//---------------------------------------------------------------------------
#undef Application
//---------------------------------------------------------------------------
[!endif]
[!if=(WinCGISource, "TRUE")]
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <WebBroker.hpp>
#include <CGIApp.hpp>
#pragma hdrstop
#include <tchar.h>
#define Application Webbroker::Application
#pragma link "CGIApp.obj"
#pragma link "WebBroker.obj"

//---------------------------------------------------------------------------
USERES("[!ProjectName].res");
[!if=(SetWebModuleClass, "TRUE")]
extern PACKAGE TComponentClass WebModuleClass;
[!endif]
//---------------------------------------------------------------------------
WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
  try
  {
    Application->Initialize();
[!if=(SetWebModuleClass, "TRUE")]
    Application->WebModuleClass = WebModuleClass;
[!endif]
    Application->Run();
  }
  catch (Exception &exception)
  {
    Sysutils::ShowException(&exception, System::ExceptAddr());
  }
  return 0;
}
//---------------------------------------------------------------------------
[!endif]
[!if=(CGISource, "TRUE")]
//---------------------------------------------------------------------------
[!if=(Clx, "TRUE")]
#include <SysUtils.hpp>
#include <WebBroker.hpp>
#include <CGIApp.hpp>
[!else]
#include <condefs.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SysUtils.hpp>
#include <WebBroker.hpp>
#include <CGIApp.hpp>
[!endif]
#pragma hdrstop
#include <tchar.h>
#define Application Webbroker::Application
[!if=(Linux, "TRUE")]
#pragma link "CGIApp.o"
#pragma link "WebBroker.o"
[!else]
#pragma link "CGIApp.obj"
#pragma link "WebBroker.obj"
#pragma package(smart_init)
[!endif]

[!if=(SetWebModuleClass, "TRUE")]
extern PACKAGE TComponentClass WebModuleClass;
[!endif]
//---------------------------------------------------------------------------
int _tmain(int argc, _TCHAR* argv[])
{
//  try
//  {
    Application->Initialize();
[!if=(SetWebModuleClass, "TRUE")]
    Application->WebModuleClass = WebModuleClass;
[!endif]
    Application->Run();
//  }
//  catch (Exception &exception)
//  {
//    Sysutils::ShowException(&exception, System::ExceptAddr());
//  }
  return 0;
}
//---------------------------------------------------------------------------
[!endif]
[!if=(IndyConsoleProjectSource, "TRUE")]
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <WebReq.hpp>
#include <IdHTTPWebBrokerBridge.hpp>
[!if=(HTTPS, "TRUE")]
#include <IdSSLOpenSSL.hpp>
[!endif]
#include <tchar.h>
#include <stdio.h>
#include <memory>
//---------------------------------------------------------------------------
#pragma link "WebReq.obj"
#pragma link "IndySystem.lib"
#pragma link "IndyCore.lib"
#pragma link "IndyProtocols.lib"
#pragma link "IdHTTPWebBrokerBridge.obj"
[!if=(DBXTerminateThreads, "TRUE")]
#include <DSService.hpp>

void TerminateThreads(void)
{
  if (TDSSessionManager::Instance != NULL)
  {
    TDSSessionManager::Instance->TerminateAllSessions();
  }
}
[!endif]
//---------------------------------------------------------------------------
[!if=(HTTPS, "TRUE")]
class TGetSSLPassword
{
public:
    void __fastcall OnGetSSLPassword(AnsiString &APassword)
    {
        APassword = "[!KeyFilePassword]";
    }
};
//---------------------------------------------------------------------------
[!endif]
void runServer(int port)
{
    printf ("Starting HTTP Server or port %d\n", port);
    std::auto_ptr<TIdHTTPWebBrokerBridge> server(new TIdHTTPWebBrokerBridge(NULL));
[!if=(HTTPS, "TRUE")]
    TIdServerIOHandlerSSLOpenSSL *iohandler = new TIdServerIOHandlerSSLOpenSSL(server.get());
    iohandler->SSLOptions->CertFile = "[!CertFile]";
    iohandler->SSLOptions->RootCertFile = "[!RootCertFile]";
    iohandler->SSLOptions->KeyFile = "[!KeyFile]";
    std::auto_ptr<TGetSSLPassword> getpassword(new TGetSSLPassword());
    iohandler->OnGetPassword = getpassword->OnGetSSLPassword;
    server->IOHandler = iohandler;
[!endif]
    server->DefaultPort = port;
    server->Active = true;
    printf("Press ESC to stop the server\n");
    TInputRecord inputRecord;
    DWORD event;
    HANDLE stdIn = GetStdHandle(STD_INPUT_HANDLE);
    while (true)
    {
        Win32Check(ReadConsoleInput(stdIn, &inputRecord, 1, &event));
        if (inputRecord.EventType == KEY_EVENT &&
        inputRecord.Event.KeyEvent.bKeyDown &&
        inputRecord.Event.KeyEvent.wVirtualKeyCode == VK_ESCAPE)
          break;
    }
[!if=(DBXTerminateThreads, "TRUE")]
    TerminateThreads();
[!endif]
}
[!if=(SetWebModuleClass, "TRUE")]
//---------------------------------------------------------------------------
extern PACKAGE TComponentClass WebModuleClass;
[!endif]
//---------------------------------------------------------------------------
#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
  try
  {
[!if=(SetWebModuleClass, "TRUE")]
    if (WebRequestHandler() != NULL)
    {
      WebRequestHandler()->WebModuleClass = WebModuleClass;
    }
[!endif]
    runServer([!HTTPPort]);
  }
  catch (Exception &exception)
  {
    printf("%s: %s\n", AnsiString(exception.ClassName()).c_str(), AnsiString(exception.Message).c_str());
  }
  return 0;
}
//---------------------------------------------------------------------------
[!endif]
[!if=(WebModuleSource, "TRUE")]
//---------------------------------------------------------------------------
#include "[!ModuleIdent].h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
[!if=(Clx, "TRUE")]
#pragma resource "*.xfm"
[!else]
#pragma resource "*.dfm"
[!endif]

TComponentClass WebModuleClass = __classid(T[!FormIdent]);
//---------------------------------------------------------------------------
__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: T[!AncestorIdent](Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall T[!FormIdent]::[!FormIdent]DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  Response->Content = "<html><heading/><body>Web Server Application</body></html>";
}
//---------------------------------------------------------------------------
[!endif]
[!if=(WebModuleIntf, "TRUE")]
//---------------------------------------------------------------------------
#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <HTTPApp.hpp>
//---------------------------------------------------------------------------
class T[!FormIdent] : public T[!AncestorIdent]
{
__published:	// IDE-managed Components
	void __fastcall [!FormIdent]DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
private:	// User declarations
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
#endif
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
//---------------------------------------------------------------------------
[!if=(Clx, "TRUE")]
#include <clx.h>
[!else]
#include <vcl.h>
[!endif]
#pragma hdrstop
#include <tchar.h>
[!if=(SetWebModuleClass, "TRUE")]
#include <WebReq.hpp>
[!endif]

[!if=(Linux, "TRUE")]
#pragma link "WebReq.o"
#pragma link "SockAppHlpr.o"
[!else]
#pragma link "WebReq.obj"
#pragma link "SockAppHlpr.obj"
#pragma link "IndySystem.lib"
#pragma link "IndyCore.lib"
[!endif]

//---------------------------------------------------------------------------
USERES("[!ProjectName].res");
//---------------------------------------------------------------------------
[!if=(Linux, "TRUE")]
int main(void)
{
//	try
//	{
                 Application->Initialize();
                 Application->Run();
//	}
//	catch (Exception &exception)
//	{
//		Application->ShowException(&exception);
//	}
//	catch(...)
//	{
//		try
//		{
//			throw Exception("");
//		}
//		catch(Exception &exception)
//		{
//			Application->ShowException(&exception);
//		}
//	}
        return 0;
}
[!else]
[!if=(SetWebModuleClass, "TRUE")]
extern PACKAGE TComponentClass WebModuleClass;
[!endif]
WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
  try
  {
[!if=(SetWebModuleClass, "TRUE")]
    if (WebRequestHandler() != NULL)
    {
      WebRequestHandler()->WebModuleClass = WebModuleClass;
    }
[!endif]
    Application->Initialize();
    Application->Run();
  }
  catch (Exception &exception)
  {
    Sysutils::ShowException(&exception, System::ExceptAddr());
  }
  return 0;
}
[!endif]
//---------------------------------------------------------------------------

[!endif]
[!if=(IndyFormProjectSource, "TRUE")]
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
[!if=(SetWebModuleClass, "TRUE")]
#include <WebReq.hpp>
[!endif]
#pragma link "WebReq.obj"
#pragma link "IndySystem.lib"
#pragma link "IndyCore.lib"
#pragma link "IndyProtocols.lib"
#pragma link "IdHTTPWebBrokerBridge.obj"

//---------------------------------------------------------------------------
USERES("[!ProjectName].res");
//---------------------------------------------------------------------------
[!if=(SetWebModuleClass, "TRUE")]
extern PACKAGE TComponentClass WebModuleClass;
[!endif]
WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
  try
  {
[!if=(SetWebModuleClass, "TRUE")]
    if (WebRequestHandler() != NULL)
    {
      WebRequestHandler()->WebModuleClass = WebModuleClass;
    }
[!endif]
    Application->Initialize();
    Application->Run();
  }
  catch (Exception &exception)
  {
    Sysutils::ShowException(&exception, System::ExceptAddr());
  }
  return 0;
}
//---------------------------------------------------------------------------

[!endif]
[!if=(COMConsoleSource, "TRUE")]
//---------------------------------------------------------------------------
[!if=(Clx, "TRUE")]
#include <clx.h>
[!else]
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
[!endif]

#pragma hdrstop
#include "[!ModuleIdent].h"
//---------------------------------------------------------------------------

#pragma package(smart_init)
[!if=(Clx, "TRUE")]
#pragma resource "*.xfm"
[!else]
#pragma resource "*.dfm"
[!endif]

T[!FormIdent] *[!FormIdent];

__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: T[!AncestorIdent](Owner)
{
}

void initFunc()
{
  InitSockAppFactory("%3:s");
}
#pragma startup initFunc 31

[!endif]
[!if=(IndyFormConsoleSource, "TRUE")]
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "[!ModuleIdent].h"
[!if=(DBXTerminateThreads, "TRUE")]
#include <DSService.hpp>
[!endif]
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::ButtonStartClick(TObject *Sender)
{
  StartServer();
}
[!if=(DBXTerminateThreads, "TRUE")]
//---------------------------------------------------------------------------
void TerminateThreads(void)
{
  if (TDSSessionManager::Instance != NULL)
  {
    TDSSessionManager::Instance->TerminateAllSessions();
  }
}
[!endif]
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::ButtonStopClick(TObject *Sender)
{
[!if=(DBXTerminateThreads, "TRUE")]
  TerminateThreads();
[!endif]
  FServer->Active = false;
  FServer->Bindings->Clear();
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::ButtonOpenBrowserClick(TObject *Sender)
{
  StartServer();
  String url;
[!if=(HTTPS, "TRUE")]
  url.sprintf(L"https://localhost:%s", EditPort->Text.c_str());
[!else]
  url.sprintf(L"http://localhost:%s", EditPort->Text.c_str());
[!endif]
  ShellExecuteW(0,
        NULL,
        url.c_str(),
        NULL, NULL, SW_SHOWNOACTIVATE);
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::ApplicationEvents1Idle(TObject *Sender, bool &Done)
{
  ButtonStart->Enabled = !FServer->Active;
  ButtonStop->Enabled = FServer->Active;
  EditPort->Enabled = !FServer->Active;
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::StartServer()
{
  if (!FServer->Active)
  {
    FServer->Bindings->Clear();
    FServer->DefaultPort = StrToInt(EditPort->Text);
    FServer->Active = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::FormCreate(TObject *Sender)
{
  FServer = new TIdHTTPWebBrokerBridge(this);
[!if=(HTTPS, "TRUE")]
  TIdServerIOHandlerSSLOpenSSL *LIOHandleSSL;
  LIOHandleSSL = new TIdServerIOHandlerSSLOpenSSL(FServer);
  LIOHandleSSL->SSLOptions->CertFile = "[!CertFile]";
  LIOHandleSSL->SSLOptions->RootCertFile = "[!RootCertFile]";
  LIOHandleSSL->SSLOptions->KeyFile = "[!KeyFile]";
  LIOHandleSSL->OnGetPassword = OnGetSSLPassword;
  FServer->IOHandler = LIOHandleSSL;
[!endif]
}
//---------------------------------------------------------------------------
[!if=(HTTPS, "TRUE")]
void __fastcall T[!FormIdent]::OnGetSSLPassword(AnsiString &APassword)
{
  APassword = "[!KeyFilePassword]";
}
//---------------------------------------------------------------------------
[!endif]
[!endif]
[!if=(IndyFormConsoleIntf, "TRUE")]
//---------------------------------------------------------------------------

#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <AppEvnts.hpp>
#include <IdHTTPWebBrokerBridge.hpp>
[!if=(HTTPS, "TRUE")]
#include <IdSSLOpenSSL.hpp>
[!endif]
//---------------------------------------------------------------------------
class T[!FormIdent] : public T[!AncestorIdent]
{
__published:	// IDE-managed Components
	TButton *ButtonOpenBrowser;
	TApplicationEvents *ApplicationEvents1;
	TEdit *EditPort;
	TButton *ButtonStop;
	TButton *ButtonStart;
	TLabel *Label1;
	void __fastcall ButtonStartClick(TObject *Sender);
	void __fastcall ButtonStopClick(TObject *Sender);
	void __fastcall ButtonOpenBrowserClick(TObject *Sender);
	void __fastcall ApplicationEvents1Idle(TObject *Sender, bool &Done);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	TIdHTTPWebBrokerBridge *FServer;
	void __fastcall StartServer();
[!if=(HTTPS, "TRUE")]
        void __fastcall OnGetSSLPassword(AnsiString &APassword);
[!endif]
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
#endif
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
//---------------------------------------------------------------------------
#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------

#include <Classes.hpp>
[!if=(Clx, "TRUE")]
#include <QForms.hpp>
[!else]
#include <Forms.hpp>
[!endif]
#include <SockAppHlpr.hpp>

class T[!FormIdent] : public T[!AncestorIdent]
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner);
};

extern PACKAGE T[!FormIdent] *[!FormIdent];

#endif
[!endif]
[!if=(Apache, "TRUE")]
//---------------------------------------------------------------------------
#include <WebBroker.hpp>
#include <ApacheApp.hpp>
#pragma hdrstop

#define Application Webbroker::Application

[!if=(Linux, "TRUE")]
#pragma link "ApacheApp.o"
#pragma link "WebBroker.o"
[!else]
#pragma link "ApacheApp.obj"
#pragma link "WebBroker.obj"
[!endif]


//---------------------------------------------------------------------------
extern "C"
{
  /* This module is the exported entry point accessed by Apache.
     Use the name of this variable in the httpd.conf file when loading
     this module, but preced it with an underscore:

     #### Start httpd.conf suggestion
     LoadModule _Project1_module "C:\FullPathToTheDll.dll"
     <Location /some_location>
       SetHandler Project1-handler
     </Location>
    #### End httpd.conf suggestion

    Be sure to put the LoadModule directive after the ClearModuleList
    directive in the httpd.conf file.

    To change the default handler name and the ModuleName, do a strcpy in
    the DllEntryPoint function below, such as:

    ...

      if (reason == DLL_PROCESS_ATTACH) {
        strcpy(ContentType, "custom_handler_name-handler");
        strcpy(ModuleName, "unique_module_name");
    ...

    */

  Httpd::module __declspec(dllexport) [!ProjectName]_module;
}

//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try
  {
    if (reason == DLL_PROCESS_ATTACH) {
      Apacheapp::set_module(&[!ProjectName]_module);
		  Application->Initialize();
		  Application->Run();
    }
  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
#undef Application
//---------------------------------------------------------------------------

[!endif]
[!if=(ApacheTwo, "TRUE")]
//---------------------------------------------------------------------------
#include <WebBroker.hpp>
#include <ApacheTwoApp.hpp>
#pragma hdrstop

#define Application Webbroker::Application

[!if=(Linux, "TRUE")]
#pragma link "ApacheTwoApp.o"
#pragma link "WebBroker.o"
[!else]
#pragma link "ApacheTwoApp.obj"
#pragma link "WebBroker.obj"
[!endif]


//---------------------------------------------------------------------------
extern "C"
{
  /* This module is the exported entry point accessed by Apache.
     Use the name of this variable in the httpd.conf file when loading
     this module, but preced it with an underscore:

     #### Start httpd.conf suggestion
     LoadModule _Project1_module "C:\FullPathToTheDll.dll"
     <Location /some_location>
       SetHandler Project1-handler
     </Location>
    #### End httpd.conf suggestion

    Be sure to put the LoadModule directive after the ClearModuleList
    directive in the httpd.conf file.

    To change the default handler name and the ModuleName, do a strcpy in
    the DllEntryPoint function below, such as:

    ...

      if (reason == DLL_PROCESS_ATTACH) {
        strcpy(ContentType, "custom_handler_name-handler");
        strcpy(ModuleName, "unique_module_name");
    ...

    */

  Httpd::module __declspec(dllexport) [!ProjectName]_module;
}

//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  try
  {
    if (reason == DLL_PROCESS_ATTACH) {
      Apacheapp::set_module(&[!ProjectName]_module);
		  Application->Initialize();
		  Application->Run();
    }
  }
  catch (Exception &exception)
  {
  }
  return 1;
}
//---------------------------------------------------------------------------
#undef Application
//---------------------------------------------------------------------------

[!endif]
[!if=(SOAPWebModuleSource, "TRUE")]
//---------------------------------------------------------------------------
#include "[!ModuleIdent].h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
[!if=(Clx, "TRUE")]
#pragma resource "*.xfm"
[!else]
#pragma resource "*.dfm"
[!endif]

TComponentClass WebModuleClass = __classid(T[!FormIdent]);
//---------------------------------------------------------------------------
__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: T[!AncestorIdent](Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall T[!FormIdent]::[!FormIdent]DefaultHandlerAction(TObject *Sender,
      TWebRequest *Request, TWebResponse *Response, bool &Handled)
{
  WSDLHTMLPublish1->ServiceInfo(Sender, Request, Response, Handled);
}
//---------------------------------------------------------------------------
[!endif]
[!if=(SOAPWebModuleIntf, "TRUE")]
// stSOAPWebModuleIntf
//---------------------------------------------------------------------------
#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <HTTPApp.hpp>
//---------------------------------------------------------------------------
class T[!FormIdent] : public T[!AncestorIdent]
{
__published:	// IDE-managed Components
        THTTPSoapDispatcher *HTTPSoapDispatcher1;
        THTTPSoapCppInvoker *HTTPSoapCppInvoker1;
        TWSDLHTMLPublish *WSDLHTMLPublish1;
        void __fastcall [!FormIdent]DefaultHandlerAction(TObject *Sender,
          TWebRequest *Request, TWebResponse *Response, bool &Handled);
private:	// User declarations
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
#endif

[!endif]

[!if=(SOAPCOMConsoleSource, "TRUE")]
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
[!if=(Clx, "TRUE")]
#include <clx.h>
[!else]
#include <vcl.h>
[!endif]
#pragma hdrstop
#include "[!ModuleIdent].h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
[!if=(Clx, "TRUE")]
#pragma resource "*.xfm"
[!else]
#pragma resource "*.dfm"
[!endif]

T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: T[!AncestorIdent](Owner)
{
}

//---------------------------------------------------------------------------
void initWebAppDebugApp()
{
  InitSockAppFactory(appGuid, "%3:s");
}
#pragma startup initWebAppDebugApp 31
//---------------------------------------------------------------------------
[!endif]
[!if=(SOAPCOMConsoleIntf, "TRUE")]
//---------------------------------------------------------------------------
#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <HTTPApp.hpp>
//---------------------------------------------------------------------------
class T[!FormIdent] : public T[!AncestorIdent]
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
#endif
[!endif]

