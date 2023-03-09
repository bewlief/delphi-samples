
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
//----------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
#include <stdio.h>
#include <memory>
//----------------------------------------------------------------------------
extern void runDSServer();
//----------------------------------------------------------------------------
#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
  try
  {
    runDSServer();
  }
  catch (Exception &exception)
  {
    printf("%ls: %ls", exception.ClassName(), exception.Message);
  }
  return 0;
}
//----------------------------------------------------------------------------
[!endif]
[!if=(DataSnapServiceSource)]
[!if=(Diagnostics)]
// DataSnapServiceSource
[!endif]
#include <SysUtils.hpp>
#include <SvcMgr.hpp>
#pragma hdrstop
#define Application Svcmgr::Application
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	try
	{
		// Windows 2003 Server requires StartServiceCtrlDispatcher to be
		// called before CoRegisterClassObject, which can be called indirectly
		// by Application.Initialize. TServiceApplication->DelayInitialize allows
		// Application->Initialize to be called from TService->Main (after
		// StartServiceCtrlDispatcher has been called).
		//
		// Delayed initialization of the Application object may affect
		// events which then occur prior to initialization, such as
		// TService->OnCreate. It is only recommended if the ServiceApplication
		// registers a class object with OLE and is intended for use with
		// Windows 2003 Server.
		//
		// Application->DelayInitialize = true;
		//
		if ((!Application->DelayInitialize) || (Application->Installing()))
		{
			Application->Initialize();
		}
		Application->Run();
	}
	catch (Exception &exception)
	{
		Sysutils::ShowException(&exception, System::ExceptAddr());
	}
        catch(...)
        {
		try
		{
	        	throw Exception("");
		}
		catch(Exception &exception)
		{
			Sysutils::ShowException(&exception, System::ExceptAddr());
		}
        }
	return 0;
}
[!endif]
[!if=(DataSnapVCLSource)]
[!if=(Diagnostics)]
// DataSnapVCLSource
[!endif]
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->Run();
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw Exception("");
		}
		catch (Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
[!endif]
[!if=(DataSnapModuleSource)]
[!if=(Diagnostics)]
// DataSnapModuleSource
[!endif]
//----------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
#include <stdio.h>
#include <memory>
[!if=(ServerMethodsClass)]
#include "[!ServerMethodsUnitName].h"
[!endif]

#include "[!ModuleIdent].h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: T[!AncestorIdent](Owner)
{
}
//----------------------------------------------------------------------------
[!if=(ServerMethodsClass)]
void __fastcall T[!FormIdent]::DSServerClass1GetClass(TDSServerClass *DSServerClass,
          TPersistentClass &PersistentClass)
{
	PersistentClass =  __classid(T[!ServerMethodsClassName]);
}
//----------------------------------------------------------------------------
[!endif]
[!if=(Authentication)]
void __fastcall T[!FormIdent]::DSAuthenticationManager1UserAuthenticate(TObject *Sender,
          const UnicodeString Protocol, const UnicodeString Context, const UnicodeString User,
          const UnicodeString Password, bool &valid, TStrings *UserRoles)

{
	/* TODO : Validate the client user and password.
	If role-based authorization is needed, add role names to the UserRoles parameter  */
	valid = true;
}
//----------------------------------------------------------------------------
[!endif]
[!if=(Authorization)]
void __fastcall T[!FormIdent]::DSAuthenticationManager1UserAuthorize(TObject *Sender,
          const TDSAuthorizeEventObject * eventObject, bool &valid)

{
	/* TODO : Authorize a user to execute a method.
	Use values from eventObject such as UserName, UserRoles, AuthorizedRoles and DeniedRoles.
	Use DSAuthenticationManager1.Roles to define Authorized and Denied roles
	for particular server methods. */
	valid = true;
}
//----------------------------------------------------------------------------
[!endif]
[!if=(HTTPSProtocol)]
void __fastcall T[!FormIdent]::DSCertFiles1GetPEMFilePasskey(TObject *ASender,
          AnsiString &APasskey)
{
	APasskey = "[!KeyFilePassword]";
}
//----------------------------------------------------------------------------
[!endif]
[!if=(RunDSServer)]
void runDSServer()
{
	printf("Starting %ls\n", __classid(T[!FormIdent])->ClassName());
	std::auto_ptr<T[!FormIdent]> module(new T[!FormIdent](NULL));
	module->DSServer1->Start();
	TInputRecord inputRecord;
	DWORD event;
	printf("Press ESC to stop the server");
	HANDLE stdIn = GetStdHandle(STD_INPUT_HANDLE);
	while (true)
	{
		Win32Check(ReadConsoleInput(stdIn, &inputRecord, 1, &event));
		if (inputRecord.EventType == KEY_EVENT &&
		inputRecord.Event.KeyEvent.bKeyDown &&
		inputRecord.Event.KeyEvent.wVirtualKeyCode == VK_ESCAPE)
			break;
	}
	module->DSServer1->Stop();
}
//----------------------------------------------------------------------------
[!endif]
[!if=(ServiceMethods)]
TServiceController __fastcall T[!FormIdent]::GetServiceController(void)
{
	return (TServiceController) ServiceController;
}

void __stdcall ServiceController(unsigned CtrlCode)
{
	[!FormIdent]->Controller(CtrlCode);
}

 bool __fastcall T[!FormIdent]::DoContinue(void)
{
	bool doContinue = TService::DoContinue();
	DSServer1->Start();
	return doContinue;
}

void __fastcall T[!FormIdent]::DoInterrogate(void)
{
	TService::DoInterrogate();
}

bool __fastcall T[!FormIdent]::DoPause(void)
{
	DSServer1->Stop();
	return TService::DoPause();
}

bool __fastcall T[!FormIdent]::DoStop(void)
{
	DSServer1->Stop();
	return TService::DoStop();
}

void __fastcall T[!FormIdent]::ServiceStart(TService* Sender, bool &Started)
{
	DSServer1->Start();
}
//---------------------------------------------------------------------------
[!endif]
[!endif]
[!if=(DataSnapModuleIntf)]
//----------------------------------------------------------------------------

#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <Classes.hpp>
[!if=(ServiceMethods)]
#include <SvcMgr.hpp>
[!endif]
#include <DSCommonServer.hpp>
#include <DSServer.hpp>
[!if=(TCPIPProtocol)]
#include <DSTCPServerTransport.hpp>
[!endif]
#include <DSAuth.hpp>
//----------------------------------------------------------------------------
class T[!FormIdent] : public T[!AncestorIdent]
{
__published:	// IDE-managed Components
	TDSServer *DSServer1;
[!if=(TCPIPProtocol)]
	TDSTCPServerTransport *DSTCPServerTransport1;
[!endif]
[!if=(HTTPProtocol)]
	TDSHTTPService *DSHTTPService1;
[!endif]
[!if=(Authentication)]
	TDSAuthenticationManager *DSAuthenticationManager1;
[!endif]
[!if=(ServerMethodsClass)]
	TDSServerClass *DSServerClass1;
	void __fastcall DSServerClass1GetClass(TDSServerClass *DSServerClass, TPersistentClass &PersistentClass);
[!endif]
[!if=(Authentication)]
	void __fastcall DSAuthenticationManager1UserAuthenticate(TObject *Sender, const UnicodeString Protocol,
          const UnicodeString Context, const UnicodeString User,
          const UnicodeString Password, bool &valid,  TStrings *UserRoles);
[!endif]
[!if=(Authorization)]
	void __fastcall DSAuthenticationManager1UserAuthorize(TObject *Sender, 
          const TDSAuthorizeEventObject * eventObject, bool &valid);
[!endif]
[!if=(HTTPSProtocol)]
	void __fastcall DSCertFiles1GetPEMFilePasskey(TObject *ASender,
          AnsiString &APasskey);
[!endif]
[!if=(ServiceMethods)]
	void __fastcall ServiceStart(TService* Sender, bool &Started);
[!endif]
private:	// User declarations
[!if=(ServiceMethods)]
protected:
	bool __fastcall DoStop(void);
	bool __fastcall DoPause(void);
	bool __fastcall DoContinue(void);
	void __fastcall DoInterrogate(void);
[!endif]
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner);
[!if=(ServiceMethods)]
	TServiceController __fastcall GetServiceController(void);

	friend void __stdcall ServiceController(unsigned CtrlCode);
[!endif]
};
//----------------------------------------------------------------------------
extern PACKAGE T[!FormIdent] *[!FormIdent];
//----------------------------------------------------------------------------
#endif
[!endif]
[!if=(DataSnapModuleDFMSource)]
object [!FormIdent]: T[!FormIdent]
  OldCreateOrder = False
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
  end
end
[!endif]
[!if=(DataSnapVCLFormSource)]
[!if=(Diagnostics)]
// DataVCLFormSource
[!endif]
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "[!ModuleIdent].h"
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
[!endif]
[!if=(DataSnapVCLFormIntf)]
//---------------------------------------------------------------------------

#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
class T[!FormIdent] : public TForm
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
[!if=(DSServerModuleTemplateIntf)]
//---------------------------------------------------------------------------

#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <DSServer.hpp>
//---------------------------------------------------------------------------
[!if=(MethodInfoOn)]
class DECLSPEC_DRTTI T[!FormIdent] : public T[!AncestorIdent]
[!else]
class T[!FormIdent] : public T[!AncestorIdent]
[!endif]
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner); 
[!if=(IncludeSampleMethods)]
	System::UnicodeString EchoString(System::UnicodeString value);
	System::UnicodeString  ReverseString(System::UnicodeString value);
[!endif]
};
//---------------------------------------------------------------------------
extern PACKAGE T[!FormIdent] *[!FormIdent];
//---------------------------------------------------------------------------
#endif
[!endif]
[!if=(DSServerModuleTemplate)]
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "[!ModuleIdent].h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: T[!AncestorIdent](Owner)
{
}
//----------------------------------------------------------------------------
[!if=(IncludeSampleMethods)]
System::UnicodeString T[!FormIdent]::EchoString(System::UnicodeString value)
{
    return value;
}
//----------------------------------------------------------------------------
System::UnicodeString T[!FormIdent]::ReverseString(System::UnicodeString value)
{
    return ::ReverseString(value);
}
//----------------------------------------------------------------------------
[!endif]
[!endif]
[!if=(DataSnapServerMethodsClassTemplateIntf)]
//----------------------------------------------------------------------------

#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//----------------------------------------------------------------------------
#include <Classes.hpp>
#include <DSServer.hpp>
//----------------------------------------------------------------------------
class DECLSPEC_DRTTI T[!FormIdent] : public T[!AncestorIdent]
{
private:	// User declarations
public:		// User declarations
[!if=(IncludeSampleMethods)]
    System::UnicodeString EchoString(System::UnicodeString value);
    System::UnicodeString  ReverseString(System::UnicodeString value);
[!endif]
};
#endif
[!endif]
[!if=(DataSnapServerMethodsClassTemplate)]
//----------------------------------------------------------------------------
#include <SysUtils.hpp>
#pragma hdrstop

#include "[!ModuleIdent].h"
//----------------------------------------------------------------------------
#pragma package(smart_init)
[!if=(IncludeSampleMethods)]
//----------------------------------------------------------------------------
System::UnicodeString T[!FormIdent]::EchoString(System::UnicodeString value)
{
    return value;
}
//----------------------------------------------------------------------------
System::UnicodeString T[!FormIdent]::ReverseString(System::UnicodeString value)
{
    return ::ReverseString(value);
}
//----------------------------------------------------------------------------
[!endif]
[!endif]
[!if=(DataSnapWebModuleIntf, "TRUE")]

//---------------------------------------------------------------------------
#ifndef [!ModuleIdent]H
#define [!ModuleIdent]H
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <HTTPApp.hpp>
#include <DBXCommon.hpp>
#include <DSCommonServer.hpp>
#include <DSHTTPCommon.hpp>
#include <DSHTTPWebBroker.hpp>
#include <DSServer.hpp>
#include <HTTPProd.hpp>
#include <DSAuth.hpp>
//---------------------------------------------------------------------------
class T[!FormIdent] : public TWebModule
{
__published:	// IDE-managed Components
	TDSServer *DSServer1;
	TDSHTTPWebDispatcher *DSHTTPWebDispatcher1;
[!if=(Authentication)]
	TDSAuthenticationManager *DSAuthenticationManager1;
[!endif]
[!if=(ServerMethodsClass)]
	TDSServerClass *DSServerClass1;
[!endif]
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
	TPageProducer *ServerFunctionInvoker;
	TPageProducer *ReverseString;
[!endif]
	TWebFileDispatcher *WebFileDispatcher1;
	TDSProxyGenerator *DSProxyGenerator1;
	TDSServerMetaDataProvider *DSServerMetaDataProvider1;
[!endif]
[!if=(ServerMethodsClass)]
	void __fastcall DSServerClass1GetClass(TDSServerClass *DSServerClass, TPersistentClass &PersistentClass);
[!endif]
[!if=(Authentication)]
	void __fastcall DSAuthenticationManager1UserAuthenticate(TObject *Sender, const UnicodeString Protocol,
          const UnicodeString Context, const UnicodeString User,
          const UnicodeString Password, bool &valid, TStrings *UserRoles);
[!endif]
[!if=(Authorization)]
	void __fastcall DSAuthenticationManager1UserAuthorize(TObject *Sender, 
          const TDSAuthorizeEventObject * eventObject, bool &valid);
[!endif]
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
	void __fastcall ServerFunctionInvokerHTMLTag(TObject *Sender, TTag Tag, const UnicodeString TagString,
		  TStrings *TagParams, UnicodeString &ReplaceText);
	void __fastcall WebModuleDefaultAction(TObject *Sender, TWebRequest *Request, TWebResponse *Response,
		  bool &Handled);
	void __fastcall WebModuleBeforeDispatch(TObject *Sender, TWebRequest *Request, TWebResponse *Response,
		  bool &Handled);
[!else]
	void __fastcall [!FormIdent]DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
[!endif]
	void __fastcall WebFileDispatcher1BeforeDispatch(TObject *Sender, const UnicodeString AFileName,
		  TWebRequest *Request, TWebResponse *Response, bool &Handled);
	void __fastcall WebModuleCreate(TObject *Sender);
[!else]
	void __fastcall [!FormIdent]DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
[!endif]
private:	// User declarations
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
	TWebActionItem* FServerFunctionInvokerAction;
	bool __fastcall AllowServerFunctionInvoker(void);
[!endif]
[!endif]
public:		// User declarations
	__fastcall T[!FormIdent](TComponent* Owner);
};
//---------------------------------------------------------------------------
#endif
[!endif]
[!if=(DataSnapWebModuleSource, "TRUE")]

//---------------------------------------------------------------------------
#include "[!ModuleIdent].h"
[!if=(ServerMethodsClass)]
#include "[!ServerMethodsUnitName].h"
[!endif]
#include <WebReq.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TComponentClass WebModuleClass = __classid(T[!FormIdent]);
//---------------------------------------------------------------------------
__fastcall T[!FormIdent]::T[!FormIdent](TComponent* Owner)
	: T[!AncestorIdent](Owner)
{
}
//---------------------------------------------------------------------------
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
[!else]
//----------------------------------------------------------------------------
void __fastcall T[!FormIdent]::[!FormIdent]DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
		  TWebResponse *Response, bool &Handled)
{
  Response->Content = "<html><heading/><body>Web Server Application</body></html>";
}
//---------------------------------------------------------------------------
[!endif]
[!else]
//----------------------------------------------------------------------------
void __fastcall T[!FormIdent]::[!FormIdent]DefaultHandlerAction(TObject *Sender, TWebRequest *Request,
		  TWebResponse *Response, bool &Handled)
{
  Response->Content = "<html><heading/><body>Web Server Application</body></html>";
}
//---------------------------------------------------------------------------
[!endif]
[!if=(ServerMethodsClass)]
void __fastcall T[!FormIdent]::DSServerClass1GetClass(TDSServerClass *DSServerClass,
          TPersistentClass &PersistentClass)
{
	PersistentClass =  __classid(T[!ServerMethodsClassName]);
}
//---------------------------------------------------------------------------
[!endif]
[!if=(Authentication)]
void __fastcall T[!FormIdent]::DSAuthenticationManager1UserAuthenticate(TObject *Sender,
          const UnicodeString Protocol, const UnicodeString Context, const UnicodeString User,
          const UnicodeString Password, bool &valid, TStrings *UserRoles)

{
	valid = true;
}
//----------------------------------------------------------------------------
[!endif]
[!if=(Authorization)]
void __fastcall T[!FormIdent]::DSAuthenticationManager1UserAuthorize(TObject *Sender,
          const TDSAuthorizeEventObject * eventObject, bool &valid)

{
	valid = true;
}
//----------------------------------------------------------------------------
[!endif]
[!if=(DataSnapREST, "TRUE")]
[!if=(IncludeSampleWebFiles)]
void __fastcall T[!FormIdent]::ServerFunctionInvokerHTMLTag(TObject *Sender, TTag Tag,
          const UnicodeString TagString, TStrings *TagParams, UnicodeString &ReplaceText)

{
	if (SameText(TagString, "urlpath"))
		ReplaceText = (Request->InternalScriptName);
	else if (SameText(TagString, "port"))
		ReplaceText = IntToStr(Request->ServerPort);
	else if (SameText(TagString, "host"))
		ReplaceText = (Request->Host);
[!if=(IncludeSampleMethods)]
	else if (SameText(TagString, "classname"))
		ReplaceText = __classid(T[!ServerMethodsClassName])->ClassName();
[!endif]
	else if (SameText(TagString, "loginrequired"))
	{
		if (DSHTTPWebDispatcher1->AuthenticationManager != NULL)
			ReplaceText = "true";
		else
			ReplaceText = "false";
	}
	else if (SameText(TagString, "serverfunctionsjs"))
		ReplaceText = Request->InternalScriptName + "/js/serverfunctions.js";
	else if (SameText(TagString, "servertime"))
		ReplaceText = DateTimeToStr(Now());
	else if  (SameText(TagString, "serverfunctioninvoker"))
		if (AllowServerFunctionInvoker())
			ReplaceText =
			"<div><a href='" + Request->InternalScriptName +
			"/ServerFunctionInvoker' target='_blank'>Server Functions</a></div>";
	else
		ReplaceText = "";
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::WebModuleDefaultAction(TObject *Sender, TWebRequest *Request,
		  TWebResponse *Response, bool &Handled)
{
  if (Request->InternalPathInfo == "" || Request->InternalPathInfo == "/")
	Response->Content = ReverseString->Content();
  else
	Response->SendRedirect(Request->InternalScriptName + "/");
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::WebModuleBeforeDispatch(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
	if (FServerFunctionInvokerAction != NULL)
	{
		FServerFunctionInvokerAction->Enabled = AllowServerFunctionInvoker();
	}
}
//---------------------------------------------------------------------------
bool __fastcall T[!FormIdent]::AllowServerFunctionInvoker(void)
{
	return Request->RemoteAddr == "127.0.0.1" ||
		Request->RemoteAddr == "0:0:0:0:0:0:0:1" || Request->RemoteAddr == "::1";
}
[!endif]
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::WebFileDispatcher1BeforeDispatch(TObject *Sender, const UnicodeString AFileName,
          TWebRequest *Request, TWebResponse *Response, bool &Handled)

{
	Handled = False;
	if (SameFileName(ExtractFileName(AFileName), "serverfunctions.js"))
	{
		TDateTime D1, D2;
		if (FileAge(AFileName, D1) && FileAge(WebApplicationFileName(), D2) && (D1 < D2))
		{
			DSProxyGenerator1->TargetDirectory = ExtractFilePath(AFileName);
			DSProxyGenerator1->TargetUnitName = ExtractFileName(AFileName);
			DSProxyGenerator1->Write();
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall T[!FormIdent]::WebModuleCreate(TObject *Sender)
{
[!if=(IncludeSampleWebFiles)]
	FServerFunctionInvokerAction = ActionByName("ServerFunctionInvokerAction");
[!endif]
}
//---------------------------------------------------------------------------
static void freeWebModules()
{
	FreeWebModules;
}
#pragma exit freeWebModules 33
[!endif]
[!endif]
[!if=(DataSnapWebModuleDFMSource)]
object [!FormIdent]: T[!FormIdent]
  OldCreateOrder = False
[!if=(DataSnapREST)]
  OnCreate = WebModuleCreate
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
  object DSServer1: TDSServer
    AutoStart = True
    HideDSAdmin = False
    Left = 96
    Top = 11
  end
  object DSHTTPWebDispatcher1: TDSHTTPWebDispatcher
[!if=(Authentication)]
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
    RESTContext = 'rest'
    Server = DSServer1
    DSHostname = 'localhost'
    DSPort = 211
    Filters = <>
    WebDispatch.MethodType = mtAny
    WebDispatch.PathInfo = 'datasnap*'
    Left = 96
    Top = 75
  end
[!if=(Authentication)]
  object DSAuthenticationManager1: TDSAuthenticationManager
    OnUserAuthenticate = DSAuthenticationManager1UserAuthenticate
[!if=(Authorization)]
    OnUserAuthorize = DSAuthenticationManager1UserAuthorize
[!endif]
    Left = 200
    Top = 139
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
    Server = DSServer1
    Left = 208
    Top = 248
  end
[!endif]
end
[!endif]

[!if=(JSProxyWriterSource)]
// Placeholer for C++
[!endif]
