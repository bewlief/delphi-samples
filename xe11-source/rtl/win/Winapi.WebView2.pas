{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Winapi.WebView2;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 98336 $
// File generated on 07/05/2020 23:02:54 from Type Library described below.

// ************************************************************************  //
// Type Lib: WebView2.tlb (1)
// LIBID: {605A1CAD-D490-4B0F-8428-BC2E5BF9C358}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN64
// Errors:
//   Hint: Parameter 'object' of ICoreWebView2.AddHostObjectToScript changed to 'object_'
//   Hint: Symbol 'type' renamed to 'type_'
// Cmdline:
//   tlibimp  -P -Fe- -XR+ WebView2.tlb
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses
  Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Winapi.ActiveX;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WebView2MajorVersion = 1;
  WebView2MinorVersion = 0;

  LIBID_WebView2: TGUID = '{605A1CAD-D490-4B0F-8428-BC2E5BF9C358}';

  IID_ICoreWebView2AcceleratorKeyPressedEventArgs: TGUID = '{9224476E-D8C3-4EB7-BB65-2FD7792B27CE}';
  IID_ICoreWebView2AcceleratorKeyPressedEventHandler: TGUID = '{A7D303F9-503C-4B7E-BC40-5C7CE6CABAAA}';
  IID_ICoreWebView2Controller: TGUID = '{7CCC5C7F-8351-4572-9077-9C1C80913835}';
  IID_ICoreWebView2ZoomFactorChangedEventHandler: TGUID = '{F1828246-8B98-4274-B708-ECDB6BF3843A}';
  IID_ICoreWebView2MoveFocusRequestedEventHandler: TGUID = '{4B21D6DD-3DE7-47B0-8019-7D3ACE6E3631}';
  IID_ICoreWebView2MoveFocusRequestedEventArgs: TGUID = '{71922903-B180-49D0-AED2-C9F9D10064B1}';
  IID_ICoreWebView2FocusChangedEventHandler: TGUID = '{76E67C71-663F-4C17-B71A-9381CCF3B94B}';
  IID_ICoreWebView2: TGUID = '{189B8AAF-0426-4748-B9AD-243F537EB46B}';
  IID_ICoreWebView2Settings: TGUID = '{203FBA37-6850-4DCC-A25A-58A351AC625D}';
  IID_ICoreWebView2NavigationStartingEventHandler: TGUID = '{073337A4-64D2-4C7E-AC9F-987F0F613497}';
  IID_ICoreWebView2NavigationStartingEventArgs: TGUID = '{EE1938CE-D385-4CB0-854B-F498F78C3D88}';
  IID_ICoreWebView2HttpRequestHeaders: TGUID = '{2C1F04DF-C90E-49E4-BD25-4A659300337B}';
  IID_ICoreWebView2HttpHeadersCollectionIterator: TGUID = '{4212F3A7-0FBC-4C9C-8118-17ED6370C1B3}';
  IID_ICoreWebView2ContentLoadingEventHandler: TGUID = '{7AF5CC82-AE19-4964-BD71-B9BC5F03E85D}';
  IID_ICoreWebView2ContentLoadingEventArgs: TGUID = '{2A800835-2179-45D6-A745-6657E9A546B9}';
  IID_ICoreWebView2SourceChangedEventHandler: TGUID = '{8FEDD1A7-3A33-416F-AF81-881EEB001433}';
  IID_ICoreWebView2SourceChangedEventArgs: TGUID = '{BD9A4BFB-BE19-40BD-968B-EBCF0D727EF3}';
  IID_ICoreWebView2HistoryChangedEventHandler: TGUID = '{54C9B7D7-D9E9-4158-861F-F97E1C3C6631}';
  IID_ICoreWebView2NavigationCompletedEventHandler: TGUID = '{9F921239-20C4-455F-9E3F-6047A50E248B}';
  IID_ICoreWebView2NavigationCompletedEventArgs: TGUID = '{361F5621-EA7F-4C55-95EC-3C5E6992EA4A}';
  IID_ICoreWebView2ScriptDialogOpeningEventHandler: TGUID = '{72D93789-2727-4A9B-A4FC-1B2609CBCBE3}';
  IID_ICoreWebView2ScriptDialogOpeningEventArgs: TGUID = '{B8F6356E-24DC-4D74-90FE-AD071E11CB91}';
  IID_ICoreWebView2Deferral: TGUID = '{A7ED8BF0-3EC9-4E39-8427-3D6F157BD285}';
  IID_ICoreWebView2PermissionRequestedEventHandler: TGUID = '{543B4ADE-9B0B-4748-9AB7-D76481B223AA}';
  IID_ICoreWebView2PermissionRequestedEventArgs: TGUID = '{774B5EA1-3FAD-435C-B1FC-A77D1ACD5EAF}';
  IID_ICoreWebView2ProcessFailedEventHandler: TGUID = '{7D2183F9-CCA8-40F2-91A9-EAFAD32C8A9B}';
  IID_ICoreWebView2ProcessFailedEventArgs: TGUID = '{EA45D1F4-75C0-471F-A6E9-803FBFF8FEF2}';
  IID_ICoreWebView2AddScriptToExecuteOnDocumentCreatedCompletedHandler: TGUID = '{7082ABED-0591-428F-A722-60C2F814546B}';
  IID_ICoreWebView2ExecuteScriptCompletedHandler: TGUID = '{3B717C93-3ED5-4450-9B13-7F56AA367AC7}';
  IID_ICoreWebView2CapturePreviewCompletedHandler: TGUID = '{DCED64F8-D9C7-4A3C-B9FD-FBBCA0B43496}';
  IID_ICoreWebView2WebMessageReceivedEventHandler: TGUID = '{199328C8-9964-4F5F-84E6-E875B1B763D6}';
  IID_ICoreWebView2WebMessageReceivedEventArgs: TGUID = '{B263B5AE-9C54-4B75-B632-40AE1A0B6912}';
  IID_ICoreWebView2CallDevToolsProtocolMethodCompletedHandler: TGUID = '{C20CF895-BA7C-493B-AB2E-8A6E3A3602A2}';
  IID_ICoreWebView2DevToolsProtocolEventReceiver: TGUID = '{FE59C48C-540C-4A3C-8898-8E1602E0055D}';
  IID_ICoreWebView2DevToolsProtocolEventReceivedEventHandler: TGUID = '{8E1DED79-A40B-4271-8BE6-57640C167F4A}';
  IID_ICoreWebView2DevToolsProtocolEventReceivedEventArgs: TGUID = '{F661B1C2-5FF5-4700-B723-C439034539B4}';
  IID_ICoreWebView2NewWindowRequestedEventHandler: TGUID = '{ACAA30EF-A40C-47BD-9CB9-D9C2AADC9FCB}';
  IID_ICoreWebView2NewWindowRequestedEventArgs: TGUID = '{9EDC7F5F-C6EA-4F3C-827B-A8880794C0A9}';
  IID_ICoreWebView2DocumentTitleChangedEventHandler: TGUID = '{6423D6B1-5A57-46C5-BA46-DBB3735EE7C9}';
  IID_ICoreWebView2ContainsFullScreenElementChangedEventHandler: TGUID = '{120888E3-4CAD-4EC2-B627-B2016D05612D}';
  IID_ICoreWebView2WebResourceRequestedEventHandler: TGUID = '{F6DC79F2-E1FA-4534-8968-4AFF10BBAA32}';
  IID_ICoreWebView2WebResourceRequestedEventArgs: TGUID = '{2D7B3282-83B1-41CA-8BBF-FF18F6BFE320}';
  IID_ICoreWebView2WebResourceRequest: TGUID = '{11B02254-B827-49F6-8974-30F6E6C55AF6}';
  IID_ICoreWebView2WebResourceResponse: TGUID = '{5953D1FC-B08F-46DD-AFD3-66B172419CD0}';
  IID_ICoreWebView2HttpResponseHeaders: TGUID = '{B5F6D4D5-1BFF-4869-85B8-158153017B04}';
  IID_ICoreWebView2WindowCloseRequestedEventHandler: TGUID = '{63C89928-AD32-4421-A0E4-EC99B34AA97E}';
  IID_ICoreWebView2CreateCoreWebView2ControllerCompletedHandler: TGUID = '{86EF6808-3C3F-4C6F-975E-8CE0B98F70BA}';
  IID_ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler: TGUID = '{8B4F98CE-DB0D-4E71-85FD-C4C4EF1F2630}';
  IID_ICoreWebView2Environment: TGUID = '{DA66D884-6DA8-410E-9630-8C48F8B3A40E}';
  IID_ICoreWebView2NewBrowserVersionAvailableEventHandler: TGUID = '{E82E8242-EE39-4A57-A065-E13256D60342}';
  IID_ICoreWebView2EnvironmentOptions: TGUID = '{97E9FBD9-646A-4B75-8682-149B71DACE59}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum COREWEBVIEW2_KEY_EVENT_KIND
type
  COREWEBVIEW2_KEY_EVENT_KIND = TOleEnum;
const
  COREWEBVIEW2_KEY_EVENT_KIND_KEY_DOWN = $00000000;
  COREWEBVIEW2_KEY_EVENT_KIND_KEY_UP = $00000001;
  COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_DOWN = $00000002;
  COREWEBVIEW2_KEY_EVENT_KIND_SYSTEM_KEY_UP = $00000003;

// Constants for enum COREWEBVIEW2_MOVE_FOCUS_REASON
type
  COREWEBVIEW2_MOVE_FOCUS_REASON = TOleEnum;
const
  COREWEBVIEW2_MOVE_FOCUS_REASON_PROGRAMMATIC = $00000000;
  COREWEBVIEW2_MOVE_FOCUS_REASON_NEXT = $00000001;
  COREWEBVIEW2_MOVE_FOCUS_REASON_PREVIOUS = $00000002;

// Constants for enum COREWEBVIEW2_WEB_ERROR_STATUS
type
  COREWEBVIEW2_WEB_ERROR_STATUS = TOleEnum;
const
  COREWEBVIEW2_WEB_ERROR_STATUS_UNKNOWN = $00000000;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_COMMON_NAME_IS_INCORRECT = $00000001;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_EXPIRED = $00000002;
  COREWEBVIEW2_WEB_ERROR_STATUS_CLIENT_CERTIFICATE_CONTAINS_ERRORS = $00000003;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_REVOKED = $00000004;
  COREWEBVIEW2_WEB_ERROR_STATUS_CERTIFICATE_IS_INVALID = $00000005;
  COREWEBVIEW2_WEB_ERROR_STATUS_SERVER_UNREACHABLE = $00000006;
  COREWEBVIEW2_WEB_ERROR_STATUS_TIMEOUT = $00000007;
  COREWEBVIEW2_WEB_ERROR_STATUS_ERROR_HTTP_INVALID_SERVER_RESPONSE = $00000008;
  COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_ABORTED = $00000009;
  COREWEBVIEW2_WEB_ERROR_STATUS_CONNECTION_RESET = $0000000A;
  COREWEBVIEW2_WEB_ERROR_STATUS_DISCONNECTED = $0000000B;
  COREWEBVIEW2_WEB_ERROR_STATUS_CANNOT_CONNECT = $0000000C;
  COREWEBVIEW2_WEB_ERROR_STATUS_HOST_NAME_NOT_RESOLVED = $0000000D;
  COREWEBVIEW2_WEB_ERROR_STATUS_OPERATION_CANCELED = $0000000E;
  COREWEBVIEW2_WEB_ERROR_STATUS_REDIRECT_FAILED = $0000000F;
  COREWEBVIEW2_WEB_ERROR_STATUS_UNEXPECTED_ERROR = $00000010;

// Constants for enum COREWEBVIEW2_SCRIPT_DIALOG_KIND
type
  COREWEBVIEW2_SCRIPT_DIALOG_KIND = TOleEnum;
const
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_ALERT = $00000000;
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_CONFIRM = $00000001;
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_PROMPT = $00000002;
  COREWEBVIEW2_SCRIPT_DIALOG_KIND_BEFOREUNLOAD = $00000003;

// Constants for enum COREWEBVIEW2_PERMISSION_KIND
type
  COREWEBVIEW2_PERMISSION_KIND = TOleEnum;
const
  COREWEBVIEW2_PERMISSION_KIND_UNKNOWN_PERMISSION = $00000000;
  COREWEBVIEW2_PERMISSION_KIND_MICROPHONE = $00000001;
  COREWEBVIEW2_PERMISSION_KIND_CAMERA = $00000002;
  COREWEBVIEW2_PERMISSION_KIND_GEOLOCATION = $00000003;
  COREWEBVIEW2_PERMISSION_KIND_NOTIFICATIONS = $00000004;
  COREWEBVIEW2_PERMISSION_KIND_OTHER_SENSORS = $00000005;
  COREWEBVIEW2_PERMISSION_KIND_CLIPBOARD_READ = $00000006;

// Constants for enum COREWEBVIEW2_PERMISSION_STATE
type
  COREWEBVIEW2_PERMISSION_STATE = TOleEnum;
const
  COREWEBVIEW2_PERMISSION_STATE_DEFAULT = $00000000;
  COREWEBVIEW2_PERMISSION_STATE_ALLOW = $00000001;
  COREWEBVIEW2_PERMISSION_STATE_DENY = $00000002;

// Constants for enum COREWEBVIEW2_PROCESS_FAILED_KIND
type
  COREWEBVIEW2_PROCESS_FAILED_KIND = TOleEnum;
const
  COREWEBVIEW2_PROCESS_FAILED_KIND_BROWSER_PROCESS_EXITED = $00000000;
  COREWEBVIEW2_PROCESS_FAILED_KIND_RENDER_PROCESS_EXITED = $00000001;
  COREWEBVIEW2_PROCESS_FAILED_KIND_RENDER_PROCESS_UNRESPONSIVE = $00000002;

// Constants for enum COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT
type
  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT = TOleEnum;
const
  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_PNG = $00000000;
  COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_JPEG = $00000001;

// Constants for enum COREWEBVIEW2_WEB_RESOURCE_CONTEXT
type
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT = TOleEnum;
const
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_ALL = $00000000;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_DOCUMENT = $00000001;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_STYLESHEET = $00000002;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE = $00000003;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MEDIA = $00000004;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FONT = $00000005;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SCRIPT = $00000006;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_XML_HTTP_REQUEST = $00000007;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_FETCH = $00000008;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_TEXT_TRACK = $00000009;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_EVENT_SOURCE = $0000000A;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_WEBSOCKET = $0000000B;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MANIFEST = $0000000C;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_SIGNED_EXCHANGE = $0000000D;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_PING = $0000000E;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_CSP_VIOLATION_REPORT = $0000000F;
  COREWEBVIEW2_WEB_RESOURCE_CONTEXT_OTHER = $00000010;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ICoreWebView2AcceleratorKeyPressedEventArgs = interface;
  ICoreWebView2AcceleratorKeyPressedEventHandler = interface;
  ICoreWebView2Controller = interface;
  ICoreWebView2ZoomFactorChangedEventHandler = interface;
  ICoreWebView2MoveFocusRequestedEventHandler = interface;
  ICoreWebView2MoveFocusRequestedEventArgs = interface;
  ICoreWebView2FocusChangedEventHandler = interface;
  ICoreWebView2 = interface;
  ICoreWebView2Settings = interface;
  ICoreWebView2NavigationStartingEventHandler = interface;
  ICoreWebView2NavigationStartingEventArgs = interface;
  ICoreWebView2HttpRequestHeaders = interface;
  ICoreWebView2HttpHeadersCollectionIterator = interface;
  ICoreWebView2ContentLoadingEventHandler = interface;
  ICoreWebView2ContentLoadingEventArgs = interface;
  ICoreWebView2SourceChangedEventHandler = interface;
  ICoreWebView2SourceChangedEventArgs = interface;
  ICoreWebView2HistoryChangedEventHandler = interface;
  ICoreWebView2NavigationCompletedEventHandler = interface;
  ICoreWebView2NavigationCompletedEventArgs = interface;
  ICoreWebView2ScriptDialogOpeningEventHandler = interface;
  ICoreWebView2ScriptDialogOpeningEventArgs = interface;
  ICoreWebView2Deferral = interface;
  ICoreWebView2PermissionRequestedEventHandler = interface;
  ICoreWebView2PermissionRequestedEventArgs = interface;
  ICoreWebView2ProcessFailedEventHandler = interface;
  ICoreWebView2ProcessFailedEventArgs = interface;
  ICoreWebView2AddScriptToExecuteOnDocumentCreatedCompletedHandler = interface;
  ICoreWebView2ExecuteScriptCompletedHandler = interface;
  ICoreWebView2CapturePreviewCompletedHandler = interface;
  ICoreWebView2WebMessageReceivedEventHandler = interface;
  ICoreWebView2WebMessageReceivedEventArgs = interface;
  ICoreWebView2CallDevToolsProtocolMethodCompletedHandler = interface;
  ICoreWebView2DevToolsProtocolEventReceiver = interface;
  ICoreWebView2DevToolsProtocolEventReceivedEventHandler = interface;
  ICoreWebView2DevToolsProtocolEventReceivedEventArgs = interface;
  ICoreWebView2NewWindowRequestedEventHandler = interface;
  ICoreWebView2NewWindowRequestedEventArgs = interface;
  ICoreWebView2DocumentTitleChangedEventHandler = interface;
  ICoreWebView2ContainsFullScreenElementChangedEventHandler = interface;
  ICoreWebView2WebResourceRequestedEventHandler = interface;
  ICoreWebView2WebResourceRequestedEventArgs = interface;
  ICoreWebView2WebResourceRequest = interface;
  ICoreWebView2WebResourceResponse = interface;
  ICoreWebView2HttpResponseHeaders = interface;
  ICoreWebView2WindowCloseRequestedEventHandler = interface;
  ICoreWebView2CreateCoreWebView2ControllerCompletedHandler = interface;
  ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler = interface;
  ICoreWebView2Environment = interface;
  ICoreWebView2NewBrowserVersionAvailableEventHandler = interface;
  ICoreWebView2EnvironmentOptions = interface;

// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  wireHWND = ^_RemotableHandle; 
  PUserType1 = ^GUID; {*}
  POleVariant1 = ^OleVariant; {*}
  PByte1 = ^Byte; {*}

  __MIDL___MIDL_itf_webview2_0005_0001_0001 = record
    Data1: LongWord;
    Data2: Word;
    Data3: Word;
    Data4: array[0..7] of Byte;
  end;

  GUID = __MIDL___MIDL_itf_webview2_0005_0001_0001; 
  {$EXTERNALSYM GUID}

  COREWEBVIEW2_PHYSICAL_KEY_STATUS = record
    RepeatCount: SYSUINT;
    ScanCode: SYSUINT;
    IsExtendedKey: Integer;
    IsMenuKeyDown: Integer;
    WasKeyDown: Integer;
    IsKeyReleased: Integer;
  end;

  tagRECT = record
    left: Integer;
    top: Integer;
    right: Integer;
    bottom: Integer;
  end;
  {$EXTERNALSYM tagRECT}

{$ALIGN 8}
  EventRegistrationToken = record
    value: Int64;
  end;


{$ALIGN 4}
  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

{$ALIGN 8}
  _LARGE_INTEGER = record
    QuadPart: Int64;
  end;

  _ULARGE_INTEGER = record
    QuadPart: Largeuint;
  end;

{$ALIGN 4}
  _FILETIME = record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;
  {$EXTERNALSYM _FILETIME}

{$ALIGN 8}
  tagSTATSTG = record
    pwcsName: PWideChar;
    type_: LongWord;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: LongWord;
    grfLocksSupported: LongWord;
    clsid: GUID;
    grfStateBits: LongWord;
    reserved: LongWord;
  end;
  {$EXTERNALSYM tagSTATSTG}


// *********************************************************************//
// Interface: ICoreWebView2AcceleratorKeyPressedEventArgs
// Flags:     (0)
// GUID:      {9224476E-D8C3-4EB7-BB65-2FD7792B27CE}
// *********************************************************************//
  ICoreWebView2AcceleratorKeyPressedEventArgs = interface(IUnknown)
    ['{9224476E-D8C3-4EB7-BB65-2FD7792B27CE}']
    function Get_KeyEventKind(out KeyEventKind: COREWEBVIEW2_KEY_EVENT_KIND): HResult; stdcall;
    function Get_VirtualKey(out VirtualKey: SYSUINT): HResult; stdcall;
    function Get_KeyEventLParam(out lParam: SYSINT): HResult; stdcall;
    function Get_PhysicalKeyStatus(out PhysicalKeyStatus: COREWEBVIEW2_PHYSICAL_KEY_STATUS): HResult; stdcall;
    function Get_Handled(out Handled: Integer): HResult; stdcall;
    function Set_Handled(Handled: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2AcceleratorKeyPressedEventHandler
// Flags:     (0)
// GUID:      {A7D303F9-503C-4B7E-BC40-5C7CE6CABAAA}
// *********************************************************************//
  ICoreWebView2AcceleratorKeyPressedEventHandler = interface(IUnknown)
    ['{A7D303F9-503C-4B7E-BC40-5C7CE6CABAAA}']
    function Invoke(const sender: ICoreWebView2Controller; 
                    const args: ICoreWebView2AcceleratorKeyPressedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2Controller
// Flags:     (0)
// GUID:      {7CCC5C7F-8351-4572-9077-9C1C80913835}
// *********************************************************************//
  ICoreWebView2Controller = interface(IUnknown)
    ['{7CCC5C7F-8351-4572-9077-9C1C80913835}']
    function Get_IsVisible(out IsVisible: Integer): HResult; stdcall;
    function Set_IsVisible(IsVisible: Integer): HResult; stdcall;
    function Get_Bounds(out Bounds: tagRECT): HResult; stdcall;
    function Set_Bounds(Bounds: tagRECT): HResult; stdcall;
    function Get_ZoomFactor(out ZoomFactor: Double): HResult; stdcall;
    function Set_ZoomFactor(ZoomFactor: Double): HResult; stdcall;
    function add_ZoomFactorChanged(const eventHandler: ICoreWebView2ZoomFactorChangedEventHandler; 
                                   out token: EventRegistrationToken): HResult; stdcall;
    function remove_ZoomFactorChanged(token: EventRegistrationToken): HResult; stdcall;
    function SetBoundsAndZoomFactor(Bounds: tagRECT; ZoomFactor: Double): HResult; stdcall;
    function MoveFocus(reason: COREWEBVIEW2_MOVE_FOCUS_REASON): HResult; stdcall;
    function add_MoveFocusRequested(const eventHandler: ICoreWebView2MoveFocusRequestedEventHandler; 
                                    out token: EventRegistrationToken): HResult; stdcall;
    function remove_MoveFocusRequested(token: EventRegistrationToken): HResult; stdcall;
    function add_GotFocus(const eventHandler: ICoreWebView2FocusChangedEventHandler; 
                          out token: EventRegistrationToken): HResult; stdcall;
    function remove_GotFocus(token: EventRegistrationToken): HResult; stdcall;
    function add_LostFocus(const eventHandler: ICoreWebView2FocusChangedEventHandler; 
                           out token: EventRegistrationToken): HResult; stdcall;
    function remove_LostFocus(token: EventRegistrationToken): HResult; stdcall;
    function add_AcceleratorKeyPressed(const eventHandler: ICoreWebView2AcceleratorKeyPressedEventHandler; 
                                       out token: EventRegistrationToken): HResult; stdcall;
    function remove_AcceleratorKeyPressed(token: EventRegistrationToken): HResult; stdcall;
    function Get_ParentWindow(out topLevelWindow: wireHWND): HResult; stdcall;
    function Set_ParentWindow(topLevelWindow: wireHWND): HResult; stdcall;
    function NotifyParentWindowPositionChanged: HResult; stdcall;
    function Close: HResult; stdcall;
    function Get_CoreWebView2(out CoreWebView2: ICoreWebView2): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ZoomFactorChangedEventHandler
// Flags:     (0)
// GUID:      {F1828246-8B98-4274-B708-ECDB6BF3843A}
// *********************************************************************//
  ICoreWebView2ZoomFactorChangedEventHandler = interface(IUnknown)
    ['{F1828246-8B98-4274-B708-ECDB6BF3843A}']
    function Invoke(const sender: ICoreWebView2Controller; const args: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2MoveFocusRequestedEventHandler
// Flags:     (0)
// GUID:      {4B21D6DD-3DE7-47B0-8019-7D3ACE6E3631}
// *********************************************************************//
  ICoreWebView2MoveFocusRequestedEventHandler = interface(IUnknown)
    ['{4B21D6DD-3DE7-47B0-8019-7D3ACE6E3631}']
    function Invoke(const sender: ICoreWebView2Controller; 
                    const args: ICoreWebView2MoveFocusRequestedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2MoveFocusRequestedEventArgs
// Flags:     (0)
// GUID:      {71922903-B180-49D0-AED2-C9F9D10064B1}
// *********************************************************************//
  ICoreWebView2MoveFocusRequestedEventArgs = interface(IUnknown)
    ['{71922903-B180-49D0-AED2-C9F9D10064B1}']
    function Get_reason(out value: COREWEBVIEW2_MOVE_FOCUS_REASON): HResult; stdcall;
    function Get_Handled(out value: Integer): HResult; stdcall;
    function Set_Handled(value: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2FocusChangedEventHandler
// Flags:     (0)
// GUID:      {76E67C71-663F-4C17-B71A-9381CCF3B94B}
// *********************************************************************//
  ICoreWebView2FocusChangedEventHandler = interface(IUnknown)
    ['{76E67C71-663F-4C17-B71A-9381CCF3B94B}']
    function Invoke(const sender: ICoreWebView2Controller; const args: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2
// Flags:     (0)
// GUID:      {189B8AAF-0426-4748-B9AD-243F537EB46B}
// *********************************************************************//
  ICoreWebView2 = interface(IUnknown)
    ['{189B8AAF-0426-4748-B9AD-243F537EB46B}']
    function Get_Settings(out Settings: ICoreWebView2Settings): HResult; stdcall;
    function Get_Source(out uri: PWideChar): HResult; stdcall;
    function Navigate(uri: PWideChar): HResult; stdcall;
    function NavigateToString(htmlContent: PWideChar): HResult; stdcall;
    function add_NavigationStarting(const eventHandler: ICoreWebView2NavigationStartingEventHandler; 
                                    out token: EventRegistrationToken): HResult; stdcall;
    function remove_NavigationStarting(token: EventRegistrationToken): HResult; stdcall;
    function add_ContentLoading(const eventHandler: ICoreWebView2ContentLoadingEventHandler; 
                                out token: EventRegistrationToken): HResult; stdcall;
    function remove_ContentLoading(token: EventRegistrationToken): HResult; stdcall;
    function add_SourceChanged(const eventHandler: ICoreWebView2SourceChangedEventHandler; 
                               out token: EventRegistrationToken): HResult; stdcall;
    function remove_SourceChanged(token: EventRegistrationToken): HResult; stdcall;
    function add_HistoryChanged(const eventHandler: ICoreWebView2HistoryChangedEventHandler; 
                                out token: EventRegistrationToken): HResult; stdcall;
    function remove_HistoryChanged(token: EventRegistrationToken): HResult; stdcall;
    function add_NavigationCompleted(const eventHandler: ICoreWebView2NavigationCompletedEventHandler; 
                                     out token: EventRegistrationToken): HResult; stdcall;
    function remove_NavigationCompleted(token: EventRegistrationToken): HResult; stdcall;
    function add_FrameNavigationStarting(const eventHandler: ICoreWebView2NavigationStartingEventHandler; 
                                         out token: EventRegistrationToken): HResult; stdcall;
    function remove_FrameNavigationStarting(token: EventRegistrationToken): HResult; stdcall;
    function add_FrameNavigationCompleted(const eventHandler: ICoreWebView2NavigationCompletedEventHandler; 
                                          out token: EventRegistrationToken): HResult; stdcall;
    function remove_FrameNavigationCompleted(token: EventRegistrationToken): HResult; stdcall;
    function add_ScriptDialogOpening(const eventHandler: ICoreWebView2ScriptDialogOpeningEventHandler; 
                                     out token: EventRegistrationToken): HResult; stdcall;
    function remove_ScriptDialogOpening(token: EventRegistrationToken): HResult; stdcall;
    function add_PermissionRequested(const eventHandler: ICoreWebView2PermissionRequestedEventHandler; 
                                     out token: EventRegistrationToken): HResult; stdcall;
    function remove_PermissionRequested(token: EventRegistrationToken): HResult; stdcall;
    function add_ProcessFailed(const eventHandler: ICoreWebView2ProcessFailedEventHandler; 
                               out token: EventRegistrationToken): HResult; stdcall;
    function remove_ProcessFailed(token: EventRegistrationToken): HResult; stdcall;
    function AddScriptToExecuteOnDocumentCreated(javaScript: PWideChar; 
                                                 const handler: ICoreWebView2AddScriptToExecuteOnDocumentCreatedCompletedHandler): HResult; stdcall;
    function RemoveScriptToExecuteOnDocumentCreated(id: PWideChar): HResult; stdcall;
    function ExecuteScript(javaScript: PWideChar; 
                           const handler: ICoreWebView2ExecuteScriptCompletedHandler): HResult; stdcall;
    function CapturePreview(imageFormat: COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT; 
                            const imageStream: IStream; 
                            const handler: ICoreWebView2CapturePreviewCompletedHandler): HResult; stdcall;
    function Reload: HResult; stdcall;
    function PostWebMessageAsJson(webMessageAsJson: PWideChar): HResult; stdcall;
    function PostWebMessageAsString(webMessageAsString: PWideChar): HResult; stdcall;
    function add_WebMessageReceived(const handler: ICoreWebView2WebMessageReceivedEventHandler; 
                                    out token: EventRegistrationToken): HResult; stdcall;
    function remove_WebMessageReceived(token: EventRegistrationToken): HResult; stdcall;
    function CallDevToolsProtocolMethod(methodName: PWideChar; parametersAsJson: PWideChar; 
                                        const handler: ICoreWebView2CallDevToolsProtocolMethodCompletedHandler): HResult; stdcall;
    function Get_BrowserProcessId(out value: SYSUINT): HResult; stdcall;
    function Get_CanGoBack(out CanGoBack: Integer): HResult; stdcall;
    function Get_CanGoForward(out CanGoForward: Integer): HResult; stdcall;
    function GoBack: HResult; stdcall;
    function GoForward: HResult; stdcall;
    function GetDevToolsProtocolEventReceiver(eventName: PWideChar; 
                                              out receiver: ICoreWebView2DevToolsProtocolEventReceiver): HResult; stdcall;
    function Stop: HResult; stdcall;
    function add_NewWindowRequested(const eventHandler: ICoreWebView2NewWindowRequestedEventHandler; 
                                    out token: EventRegistrationToken): HResult; stdcall;
    function remove_NewWindowRequested(token: EventRegistrationToken): HResult; stdcall;
    function add_DocumentTitleChanged(const eventHandler: ICoreWebView2DocumentTitleChangedEventHandler; 
                                      out token: EventRegistrationToken): HResult; stdcall;
    function remove_DocumentTitleChanged(token: EventRegistrationToken): HResult; stdcall;
    function Get_DocumentTitle(out title: PWideChar): HResult; stdcall;
    function AddHostObjectToScript(name: PWideChar; const object_: OleVariant): HResult; stdcall;
    function RemoveHostObjectFromScript(name: PWideChar): HResult; stdcall;
    function OpenDevToolsWindow: HResult; stdcall;
    function add_ContainsFullScreenElementChanged(const eventHandler: ICoreWebView2ContainsFullScreenElementChangedEventHandler; 
                                                  out token: EventRegistrationToken): HResult; stdcall;
    function remove_ContainsFullScreenElementChanged(token: EventRegistrationToken): HResult; stdcall;
    function Get_ContainsFullScreenElement(out ContainsFullScreenElement: Integer): HResult; stdcall;
    function add_WebResourceRequested(const eventHandler: ICoreWebView2WebResourceRequestedEventHandler; 
                                      out token: EventRegistrationToken): HResult; stdcall;
    function remove_WebResourceRequested(token: EventRegistrationToken): HResult; stdcall;
    function AddWebResourceRequestedFilter(uri: PWideChar; 
                                           ResourceContext: COREWEBVIEW2_WEB_RESOURCE_CONTEXT): HResult; stdcall;
    function RemoveWebResourceRequestedFilter(uri: PWideChar; 
                                              ResourceContext: COREWEBVIEW2_WEB_RESOURCE_CONTEXT): HResult; stdcall;
    function add_WindowCloseRequested(const eventHandler: ICoreWebView2WindowCloseRequestedEventHandler; 
                                      out token: EventRegistrationToken): HResult; stdcall;
    function remove_WindowCloseRequested(token: EventRegistrationToken): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2Settings
// Flags:     (0)
// GUID:      {203FBA37-6850-4DCC-A25A-58A351AC625D}
// *********************************************************************//
  ICoreWebView2Settings = interface(IUnknown)
    ['{203FBA37-6850-4DCC-A25A-58A351AC625D}']
    function Get_IsScriptEnabled(out IsScriptEnabled: Integer): HResult; stdcall;
    function Set_IsScriptEnabled(IsScriptEnabled: Integer): HResult; stdcall;
    function Get_IsWebMessageEnabled(out IsWebMessageEnabled: Integer): HResult; stdcall;
    function Set_IsWebMessageEnabled(IsWebMessageEnabled: Integer): HResult; stdcall;
    function Get_AreDefaultScriptDialogsEnabled(out AreDefaultScriptDialogsEnabled: Integer): HResult; stdcall;
    function Set_AreDefaultScriptDialogsEnabled(AreDefaultScriptDialogsEnabled: Integer): HResult; stdcall;
    function Get_IsStatusBarEnabled(out IsStatusBarEnabled: Integer): HResult; stdcall;
    function Set_IsStatusBarEnabled(IsStatusBarEnabled: Integer): HResult; stdcall;
    function Get_AreDevToolsEnabled(out AreDevToolsEnabled: Integer): HResult; stdcall;
    function Set_AreDevToolsEnabled(AreDevToolsEnabled: Integer): HResult; stdcall;
    function Get_AreDefaultContextMenusEnabled(out enabled: Integer): HResult; stdcall;
    function Set_AreDefaultContextMenusEnabled(enabled: Integer): HResult; stdcall;
    function Get_AreRemoteObjectsAllowed(out allowed: Integer): HResult; stdcall;
    function Set_AreRemoteObjectsAllowed(allowed: Integer): HResult; stdcall;
    function Get_IsZoomControlEnabled(out enabled: Integer): HResult; stdcall;
    function Set_IsZoomControlEnabled(enabled: Integer): HResult; stdcall;
    function Get_IsBuiltInErrorPageEnabled(out enabled: Integer): HResult; stdcall;
    function Set_IsBuiltInErrorPageEnabled(enabled: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2NavigationStartingEventHandler
// Flags:     (0)
// GUID:      {073337A4-64D2-4C7E-AC9F-987F0F613497}
// *********************************************************************//
  ICoreWebView2NavigationStartingEventHandler = interface(IUnknown)
    ['{073337A4-64D2-4C7E-AC9F-987F0F613497}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2NavigationStartingEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2NavigationStartingEventArgs
// Flags:     (0)
// GUID:      {EE1938CE-D385-4CB0-854B-F498F78C3D88}
// *********************************************************************//
  ICoreWebView2NavigationStartingEventArgs = interface(IUnknown)
    ['{EE1938CE-D385-4CB0-854B-F498F78C3D88}']
    function Get_uri(out uri: PWideChar): HResult; stdcall;
    function Get_IsUserInitiated(out IsUserInitiated: Integer): HResult; stdcall;
    function Get_IsRedirected(out IsRedirected: Integer): HResult; stdcall;
    function Get_RequestHeaders(out RequestHeaders: ICoreWebView2HttpRequestHeaders): HResult; stdcall;
    function Get_Cancel(out Cancel: Integer): HResult; stdcall;
    function Set_Cancel(Cancel: Integer): HResult; stdcall;
    function Get_NavigationId(out navigation_id: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2HttpRequestHeaders
// Flags:     (0)
// GUID:      {2C1F04DF-C90E-49E4-BD25-4A659300337B}
// *********************************************************************//
  ICoreWebView2HttpRequestHeaders = interface(IUnknown)
    ['{2C1F04DF-C90E-49E4-BD25-4A659300337B}']
    function GetHeader(name: PWideChar; out value: PWideChar): HResult; stdcall;
    function GetHeaders(name: PWideChar; out iterator: ICoreWebView2HttpHeadersCollectionIterator): HResult; stdcall;
    function Contains(name: PWideChar; out Contains: Integer): HResult; stdcall;
    function SetHeader(name: PWideChar; value: PWideChar): HResult; stdcall;
    function RemoveHeader(name: PWideChar): HResult; stdcall;
    function GetIterator(out iterator: ICoreWebView2HttpHeadersCollectionIterator): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2HttpHeadersCollectionIterator
// Flags:     (0)
// GUID:      {4212F3A7-0FBC-4C9C-8118-17ED6370C1B3}
// *********************************************************************//
  ICoreWebView2HttpHeadersCollectionIterator = interface(IUnknown)
    ['{4212F3A7-0FBC-4C9C-8118-17ED6370C1B3}']
    function GetCurrentHeader(out name: PWideChar; out value: PWideChar): HResult; stdcall;
    function Get_HasCurrentHeader(out hasCurrent: Integer): HResult; stdcall;
    function MoveNext(out hasNext: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ContentLoadingEventHandler
// Flags:     (0)
// GUID:      {7AF5CC82-AE19-4964-BD71-B9BC5F03E85D}
// *********************************************************************//
  ICoreWebView2ContentLoadingEventHandler = interface(IUnknown)
    ['{7AF5CC82-AE19-4964-BD71-B9BC5F03E85D}']
    function Invoke(const webview: ICoreWebView2; const args: ICoreWebView2ContentLoadingEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ContentLoadingEventArgs
// Flags:     (0)
// GUID:      {2A800835-2179-45D6-A745-6657E9A546B9}
// *********************************************************************//
  ICoreWebView2ContentLoadingEventArgs = interface(IUnknown)
    ['{2A800835-2179-45D6-A745-6657E9A546B9}']
    function Get_IsErrorPage(out IsErrorPage: Integer): HResult; stdcall;
    function Get_NavigationId(out navigation_id: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2SourceChangedEventHandler
// Flags:     (0)
// GUID:      {8FEDD1A7-3A33-416F-AF81-881EEB001433}
// *********************************************************************//
  ICoreWebView2SourceChangedEventHandler = interface(IUnknown)
    ['{8FEDD1A7-3A33-416F-AF81-881EEB001433}']
    function Invoke(const webview: ICoreWebView2; const args: ICoreWebView2SourceChangedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2SourceChangedEventArgs
// Flags:     (0)
// GUID:      {BD9A4BFB-BE19-40BD-968B-EBCF0D727EF3}
// *********************************************************************//
  ICoreWebView2SourceChangedEventArgs = interface(IUnknown)
    ['{BD9A4BFB-BE19-40BD-968B-EBCF0D727EF3}']
    function Get_IsNewDocument(out IsNewDocument: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2HistoryChangedEventHandler
// Flags:     (0)
// GUID:      {54C9B7D7-D9E9-4158-861F-F97E1C3C6631}
// *********************************************************************//
  ICoreWebView2HistoryChangedEventHandler = interface(IUnknown)
    ['{54C9B7D7-D9E9-4158-861F-F97E1C3C6631}']
    function Invoke(const webview: ICoreWebView2; const args: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2NavigationCompletedEventHandler
// Flags:     (0)
// GUID:      {9F921239-20C4-455F-9E3F-6047A50E248B}
// *********************************************************************//
  ICoreWebView2NavigationCompletedEventHandler = interface(IUnknown)
    ['{9F921239-20C4-455F-9E3F-6047A50E248B}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2NavigationCompletedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2NavigationCompletedEventArgs
// Flags:     (0)
// GUID:      {361F5621-EA7F-4C55-95EC-3C5E6992EA4A}
// *********************************************************************//
  ICoreWebView2NavigationCompletedEventArgs = interface(IUnknown)
    ['{361F5621-EA7F-4C55-95EC-3C5E6992EA4A}']
    function Get_IsSuccess(out IsSuccess: Integer): HResult; stdcall;
    function Get_WebErrorStatus(out COREWEBVIEW2_WEB_ERROR_STATUS: COREWEBVIEW2_WEB_ERROR_STATUS): HResult; stdcall;
    function Get_NavigationId(out navigation_id: Largeuint): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ScriptDialogOpeningEventHandler
// Flags:     (0)
// GUID:      {72D93789-2727-4A9B-A4FC-1B2609CBCBE3}
// *********************************************************************//
  ICoreWebView2ScriptDialogOpeningEventHandler = interface(IUnknown)
    ['{72D93789-2727-4A9B-A4FC-1B2609CBCBE3}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2ScriptDialogOpeningEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ScriptDialogOpeningEventArgs
// Flags:     (0)
// GUID:      {B8F6356E-24DC-4D74-90FE-AD071E11CB91}
// *********************************************************************//
  ICoreWebView2ScriptDialogOpeningEventArgs = interface(IUnknown)
    ['{B8F6356E-24DC-4D74-90FE-AD071E11CB91}']
    function Get_uri(out uri: PWideChar): HResult; stdcall;
    function Get_Kind(out Kind: COREWEBVIEW2_SCRIPT_DIALOG_KIND): HResult; stdcall;
    function Get_Message(out Message: PWideChar): HResult; stdcall;
    function Accept: HResult; stdcall;
    function Get_DefaultText(out DefaultText: PWideChar): HResult; stdcall;
    function Get_ResultText(out ResultText: PWideChar): HResult; stdcall;
    function Set_ResultText(ResultText: PWideChar): HResult; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2Deferral
// Flags:     (0)
// GUID:      {A7ED8BF0-3EC9-4E39-8427-3D6F157BD285}
// *********************************************************************//
  ICoreWebView2Deferral = interface(IUnknown)
    ['{A7ED8BF0-3EC9-4E39-8427-3D6F157BD285}']
    function Complete: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2PermissionRequestedEventHandler
// Flags:     (0)
// GUID:      {543B4ADE-9B0B-4748-9AB7-D76481B223AA}
// *********************************************************************//
  ICoreWebView2PermissionRequestedEventHandler = interface(IUnknown)
    ['{543B4ADE-9B0B-4748-9AB7-D76481B223AA}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2PermissionRequestedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2PermissionRequestedEventArgs
// Flags:     (0)
// GUID:      {774B5EA1-3FAD-435C-B1FC-A77D1ACD5EAF}
// *********************************************************************//
  ICoreWebView2PermissionRequestedEventArgs = interface(IUnknown)
    ['{774B5EA1-3FAD-435C-B1FC-A77D1ACD5EAF}']
    function Get_uri(out uri: PWideChar): HResult; stdcall;
    function Get_PermissionKind(out value: COREWEBVIEW2_PERMISSION_KIND): HResult; stdcall;
    function Get_IsUserInitiated(out IsUserInitiated: Integer): HResult; stdcall;
    function Get_State(out value: COREWEBVIEW2_PERMISSION_STATE): HResult; stdcall;
    function Set_State(value: COREWEBVIEW2_PERMISSION_STATE): HResult; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ProcessFailedEventHandler
// Flags:     (0)
// GUID:      {7D2183F9-CCA8-40F2-91A9-EAFAD32C8A9B}
// *********************************************************************//
  ICoreWebView2ProcessFailedEventHandler = interface(IUnknown)
    ['{7D2183F9-CCA8-40F2-91A9-EAFAD32C8A9B}']
    function Invoke(const sender: ICoreWebView2; const args: ICoreWebView2ProcessFailedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ProcessFailedEventArgs
// Flags:     (0)
// GUID:      {EA45D1F4-75C0-471F-A6E9-803FBFF8FEF2}
// *********************************************************************//
  ICoreWebView2ProcessFailedEventArgs = interface(IUnknown)
    ['{EA45D1F4-75C0-471F-A6E9-803FBFF8FEF2}']
    function Get_ProcessFailedKind(out ProcessFailedKind: COREWEBVIEW2_PROCESS_FAILED_KIND): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2AddScriptToExecuteOnDocumentCreatedCompletedHandler
// Flags:     (0)
// GUID:      {7082ABED-0591-428F-A722-60C2F814546B}
// *********************************************************************//
  ICoreWebView2AddScriptToExecuteOnDocumentCreatedCompletedHandler = interface(IUnknown)
    ['{7082ABED-0591-428F-A722-60C2F814546B}']
    function Invoke(errorCode: HResult; id: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ExecuteScriptCompletedHandler
// Flags:     (0)
// GUID:      {3B717C93-3ED5-4450-9B13-7F56AA367AC7}
// *********************************************************************//
  ICoreWebView2ExecuteScriptCompletedHandler = interface(IUnknown)
    ['{3B717C93-3ED5-4450-9B13-7F56AA367AC7}']
    function Invoke(errorCode: HResult; resultObjectAsJson: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2CapturePreviewCompletedHandler
// Flags:     (0)
// GUID:      {DCED64F8-D9C7-4A3C-B9FD-FBBCA0B43496}
// *********************************************************************//
  ICoreWebView2CapturePreviewCompletedHandler = interface(IUnknown)
    ['{DCED64F8-D9C7-4A3C-B9FD-FBBCA0B43496}']
    function Invoke(result: HResult): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebMessageReceivedEventHandler
// Flags:     (0)
// GUID:      {199328C8-9964-4F5F-84E6-E875B1B763D6}
// *********************************************************************//
  ICoreWebView2WebMessageReceivedEventHandler = interface(IUnknown)
    ['{199328C8-9964-4F5F-84E6-E875B1B763D6}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2WebMessageReceivedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebMessageReceivedEventArgs
// Flags:     (0)
// GUID:      {B263B5AE-9C54-4B75-B632-40AE1A0B6912}
// *********************************************************************//
  ICoreWebView2WebMessageReceivedEventArgs = interface(IUnknown)
    ['{B263B5AE-9C54-4B75-B632-40AE1A0B6912}']
    function Get_Source(out Source: PWideChar): HResult; stdcall;
    function Get_webMessageAsJson(out webMessageAsJson: PWideChar): HResult; stdcall;
    function TryGetWebMessageAsString(out webMessageAsString: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2CallDevToolsProtocolMethodCompletedHandler
// Flags:     (0)
// GUID:      {C20CF895-BA7C-493B-AB2E-8A6E3A3602A2}
// *********************************************************************//
  ICoreWebView2CallDevToolsProtocolMethodCompletedHandler = interface(IUnknown)
    ['{C20CF895-BA7C-493B-AB2E-8A6E3A3602A2}']
    function Invoke(errorCode: HResult; returnObjectAsJson: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2DevToolsProtocolEventReceiver
// Flags:     (0)
// GUID:      {FE59C48C-540C-4A3C-8898-8E1602E0055D}
// *********************************************************************//
  ICoreWebView2DevToolsProtocolEventReceiver = interface(IUnknown)
    ['{FE59C48C-540C-4A3C-8898-8E1602E0055D}']
    function add_DevToolsProtocolEventReceived(const handler: ICoreWebView2DevToolsProtocolEventReceivedEventHandler; 
                                               out token: EventRegistrationToken): HResult; stdcall;
    function remove_DevToolsProtocolEventReceived(token: EventRegistrationToken): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2DevToolsProtocolEventReceivedEventHandler
// Flags:     (0)
// GUID:      {8E1DED79-A40B-4271-8BE6-57640C167F4A}
// *********************************************************************//
  ICoreWebView2DevToolsProtocolEventReceivedEventHandler = interface(IUnknown)
    ['{8E1DED79-A40B-4271-8BE6-57640C167F4A}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2DevToolsProtocolEventReceivedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2DevToolsProtocolEventReceivedEventArgs
// Flags:     (0)
// GUID:      {F661B1C2-5FF5-4700-B723-C439034539B4}
// *********************************************************************//
  ICoreWebView2DevToolsProtocolEventReceivedEventArgs = interface(IUnknown)
    ['{F661B1C2-5FF5-4700-B723-C439034539B4}']
    function Get_ParameterObjectAsJson(out ParameterObjectAsJson: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2NewWindowRequestedEventHandler
// Flags:     (0)
// GUID:      {ACAA30EF-A40C-47BD-9CB9-D9C2AADC9FCB}
// *********************************************************************//
  ICoreWebView2NewWindowRequestedEventHandler = interface(IUnknown)
    ['{ACAA30EF-A40C-47BD-9CB9-D9C2AADC9FCB}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2NewWindowRequestedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2NewWindowRequestedEventArgs
// Flags:     (0)
// GUID:      {9EDC7F5F-C6EA-4F3C-827B-A8880794C0A9}
// *********************************************************************//
  ICoreWebView2NewWindowRequestedEventArgs = interface(IUnknown)
    ['{9EDC7F5F-C6EA-4F3C-827B-A8880794C0A9}']
    function Get_uri(out uri: PWideChar): HResult; stdcall;
    function Set_NewWindow(const NewWindow: ICoreWebView2): HResult; stdcall;
    function Get_NewWindow(out NewWindow: ICoreWebView2): HResult; stdcall;
    function Set_Handled(Handled: Integer): HResult; stdcall;
    function Get_Handled(out Handled: Integer): HResult; stdcall;
    function Get_IsUserInitiated(out IsUserInitiated: Integer): HResult; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2DocumentTitleChangedEventHandler
// Flags:     (0)
// GUID:      {6423D6B1-5A57-46C5-BA46-DBB3735EE7C9}
// *********************************************************************//
  ICoreWebView2DocumentTitleChangedEventHandler = interface(IUnknown)
    ['{6423D6B1-5A57-46C5-BA46-DBB3735EE7C9}']
    function Invoke(const sender: ICoreWebView2; const args: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2ContainsFullScreenElementChangedEventHandler
// Flags:     (0)
// GUID:      {120888E3-4CAD-4EC2-B627-B2016D05612D}
// *********************************************************************//
  ICoreWebView2ContainsFullScreenElementChangedEventHandler = interface(IUnknown)
    ['{120888E3-4CAD-4EC2-B627-B2016D05612D}']
    function Invoke(const sender: ICoreWebView2; const args: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceRequestedEventHandler
// Flags:     (0)
// GUID:      {F6DC79F2-E1FA-4534-8968-4AFF10BBAA32}
// *********************************************************************//
  ICoreWebView2WebResourceRequestedEventHandler = interface(IUnknown)
    ['{F6DC79F2-E1FA-4534-8968-4AFF10BBAA32}']
    function Invoke(const sender: ICoreWebView2; 
                    const args: ICoreWebView2WebResourceRequestedEventArgs): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceRequestedEventArgs
// Flags:     (0)
// GUID:      {2D7B3282-83B1-41CA-8BBF-FF18F6BFE320}
// *********************************************************************//
  ICoreWebView2WebResourceRequestedEventArgs = interface(IUnknown)
    ['{2D7B3282-83B1-41CA-8BBF-FF18F6BFE320}']
    function Get_Request(out Request: ICoreWebView2WebResourceRequest): HResult; stdcall;
    function Get_Response(out Response: ICoreWebView2WebResourceResponse): HResult; stdcall;
    function Set_Response(const Response: ICoreWebView2WebResourceResponse): HResult; stdcall;
    function GetDeferral(out deferral: ICoreWebView2Deferral): HResult; stdcall;
    function Get_ResourceContext(out context: COREWEBVIEW2_WEB_RESOURCE_CONTEXT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceRequest
// Flags:     (0)
// GUID:      {11B02254-B827-49F6-8974-30F6E6C55AF6}
// *********************************************************************//
  ICoreWebView2WebResourceRequest = interface(IUnknown)
    ['{11B02254-B827-49F6-8974-30F6E6C55AF6}']
    function Get_uri(out uri: PWideChar): HResult; stdcall;
    function Set_uri(uri: PWideChar): HResult; stdcall;
    function Get_Method(out Method: PWideChar): HResult; stdcall;
    function Set_Method(Method: PWideChar): HResult; stdcall;
    function Get_Content(out Content: IStream): HResult; stdcall;
    function Set_Content(const Content: IStream): HResult; stdcall;
    function Get_Headers(out Headers: ICoreWebView2HttpRequestHeaders): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WebResourceResponse
// Flags:     (0)
// GUID:      {5953D1FC-B08F-46DD-AFD3-66B172419CD0}
// *********************************************************************//
  ICoreWebView2WebResourceResponse = interface(IUnknown)
    ['{5953D1FC-B08F-46DD-AFD3-66B172419CD0}']
    function Get_Content(out Content: IStream): HResult; stdcall;
    function Set_Content(const Content: IStream): HResult; stdcall;
    function Get_Headers(out Headers: ICoreWebView2HttpResponseHeaders): HResult; stdcall;
    function Get_StatusCode(out StatusCode: SYSINT): HResult; stdcall;
    function Set_StatusCode(StatusCode: SYSINT): HResult; stdcall;
    function Get_ReasonPhrase(out ReasonPhrase: PWideChar): HResult; stdcall;
    function Set_ReasonPhrase(ReasonPhrase: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2HttpResponseHeaders
// Flags:     (0)
// GUID:      {B5F6D4D5-1BFF-4869-85B8-158153017B04}
// *********************************************************************//
  ICoreWebView2HttpResponseHeaders = interface(IUnknown)
    ['{B5F6D4D5-1BFF-4869-85B8-158153017B04}']
    function AppendHeader(name: PWideChar; value: PWideChar): HResult; stdcall;
    function Contains(name: PWideChar; out Contains: Integer): HResult; stdcall;
    function GetHeader(name: PWideChar; out value: PWideChar): HResult; stdcall;
    function GetHeaders(name: PWideChar; out iterator: ICoreWebView2HttpHeadersCollectionIterator): HResult; stdcall;
    function GetIterator(out iterator: ICoreWebView2HttpHeadersCollectionIterator): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2WindowCloseRequestedEventHandler
// Flags:     (0)
// GUID:      {63C89928-AD32-4421-A0E4-EC99B34AA97E}
// *********************************************************************//
  ICoreWebView2WindowCloseRequestedEventHandler = interface(IUnknown)
    ['{63C89928-AD32-4421-A0E4-EC99B34AA97E}']
    function Invoke(const sender: ICoreWebView2; const args: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2CreateCoreWebView2ControllerCompletedHandler
// Flags:     (0)
// GUID:      {86EF6808-3C3F-4C6F-975E-8CE0B98F70BA}
// *********************************************************************//
  ICoreWebView2CreateCoreWebView2ControllerCompletedHandler = interface(IUnknown)
    ['{86EF6808-3C3F-4C6F-975E-8CE0B98F70BA}']
    function Invoke(result: HResult; const createdController: ICoreWebView2Controller): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler
// Flags:     (0)
// GUID:      {8B4F98CE-DB0D-4E71-85FD-C4C4EF1F2630}
// *********************************************************************//
  ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler = interface(IUnknown)
    ['{8B4F98CE-DB0D-4E71-85FD-C4C4EF1F2630}']
    function Invoke(result: HResult; const created_environment: ICoreWebView2Environment): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2Environment
// Flags:     (0)
// GUID:      {DA66D884-6DA8-410E-9630-8C48F8B3A40E}
// *********************************************************************//
  ICoreWebView2Environment = interface(IUnknown)
    ['{DA66D884-6DA8-410E-9630-8C48F8B3A40E}']
    function CreateCoreWebView2Controller(ParentWindow: HWND; 
                                          const handler: ICoreWebView2CreateCoreWebView2ControllerCompletedHandler): HResult; stdcall;
    function CreateWebResourceResponse(const Content: IStream; StatusCode: SYSINT; 
                                       ReasonPhrase: PWideChar; Headers: PWideChar; 
                                       out Response: ICoreWebView2WebResourceResponse): HResult; stdcall;
    function Get_BrowserVersionString(out versionInfo: PWideChar): HResult; stdcall;
    function add_NewBrowserVersionAvailable(const eventHandler: ICoreWebView2NewBrowserVersionAvailableEventHandler; 
                                            out token: EventRegistrationToken): HResult; stdcall;
    function remove_NewBrowserVersionAvailable(token: EventRegistrationToken): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2NewBrowserVersionAvailableEventHandler
// Flags:     (0)
// GUID:      {E82E8242-EE39-4A57-A065-E13256D60342}
// *********************************************************************//
  ICoreWebView2NewBrowserVersionAvailableEventHandler = interface(IUnknown)
    ['{E82E8242-EE39-4A57-A065-E13256D60342}']
    function Invoke(const webviewEnvironment: ICoreWebView2Environment; const args: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICoreWebView2EnvironmentOptions
// Flags:     (0)
// GUID:      {97E9FBD9-646A-4B75-8682-149B71DACE59}
// *********************************************************************//
  ICoreWebView2EnvironmentOptions = interface(IUnknown)
    ['{97E9FBD9-646A-4B75-8682-149B71DACE59}']
    function Get_AdditionalBrowserArguments(out value: PWideChar): HResult; stdcall;
    function Set_AdditionalBrowserArguments(value: PWideChar): HResult; stdcall;
    function Get_Language(out value: PWideChar): HResult; stdcall;
    function Set_Language(value: PWideChar): HResult; stdcall;
    function Get_TargetCompatibleBrowserVersion(out value: PWideChar): HResult; stdcall;
    function Set_TargetCompatibleBrowserVersion(value: PWideChar): HResult; stdcall;
  end;

implementation

uses System.Win.ComObj;

end.
