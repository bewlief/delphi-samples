{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Win;

interface

{$SCOPEDENUMS ON}

procedure RegisterWebBrowserService;
procedure UnRegisterWebBrowserService;

implementation

uses
  System.Classes, System.Win.InternetExplorer, System.UITypes, System.Types, System.Win.IEInterfaces,
  System.SysUtils, System.RTLConsts, System.SyncObjs, System.Generics.Collections, System.Math,
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, Winapi.EdgeUtils, Winapi.WebView2,
  FMX.Platform, FMX.WebBrowser, FMX.Graphics, FMX.Types, FMX.Platform.Win, FMX.Forms, FMX.Controls.Ole, FMX.Surfaces, FMX.Controls.Win, 
  FMX.ZOrder.Win, FMX.Utils, FMX.Controls, FMX.Controls.Model, FMX.Controls.Presentation, FMX.Presentation.Win, FMX.Presentation.Messages,
  FMX.Presentation.Factory, FMX.Consts;

type
  EBrowserEngineException = class(Exception);

  // Helper types used to work with WinRT event interfaces
  PInterface = ^IInterface;
  Callback<T1, T2> = record
    type
      TStdProc1 = reference to function(const P1: T1): HResult stdcall;
      TStdProc2 = reference to function(const P1: T1; const P2: T2): HResult stdcall;
      TStdProc3 = reference to function(P1: T1; P2: T2): HResult stdcall;
      TStdMethod1 = function(P1: T1): HResult of object stdcall;
      TStdMethod2 = function(P1: T1; const P2: T2): HResult of object stdcall;
    class function CreateAs<INTF>(P: TStdProc1): INTF; overload; static;
    class function CreateAs<INTF>(P: TStdProc2): INTF; overload; static;
    class function CreateAs<INTF>(P: TStdProc3): INTF; overload; static;
    class function CreateAs<INTF>(P: TStdMethod1): INTF; overload; static;
    class function CreateAs<INTF>(P: TStdMethod2): INTF; overload; static;
  end;

  TBrowserNavigation = (GoBack, GoForward, GoHome);
  TBrowserControlState = (None, Creating, Created, Failed);

  TWindowsWebBrowserModel = class;

  TWinNativeWebBrowser = class(TWinPresentation)
  private
    FURL: string;
    [Weak] FCustomBrowser: ICustomBrowser;
    FBrowserControlState: TBrowserControlState;
    FCritSec: TCriticalSection;
    FLastErrorCode: HResult;
    FLastURL: string;
    FModel: TWindowsWebBrowserModel;
    FAcceleratorKeyPressedToken: EventRegistrationToken;
    FContainsFullScreenElementChangedToken: EventRegistrationToken;
    FContentLoadingToken: EventRegistrationToken;
    FDocumentTitleChangedToken: EventRegistrationToken;
    FFrameNavigationStartingToken: EventRegistrationToken;
    FFrameNavigationCompletedToken: EventRegistrationToken;
    FGotFocusToken: EventRegistrationToken;
    FHistoryChangedToken: EventRegistrationToken;
    FLostFocusToken: EventRegistrationToken;
    FMoveFocusRequestedToken: EventRegistrationToken;
    FNavigationStartingToken: EventRegistrationToken;
    FNavigationCompletedToken: EventRegistrationToken;
    FNewWindowRequestedToken: EventRegistrationToken;
    FPermissionRequestedToken: EventRegistrationToken;
    FProcessFailedToken: EventRegistrationToken;
    FScriptDialogOpeningToken: EventRegistrationToken;
    FSourceChangedToken: EventRegistrationToken;
    FWebResourceRequestedToken: EventRegistrationToken;
    FWebMessageReceivedToken: EventRegistrationToken;
    FWindowCloseRequestedToken: EventRegistrationToken;
    FZoomFactorChangedToken: EventRegistrationToken;
    FDevToolsProtocolEventReceivedTokenMap: TDictionary<string, EventRegistrationToken>;
    FWebView: ICoreWebView2;
    FWebViewFocusEventActive: Boolean;
    FWebViewController: ICoreWebView2Controller;
    FWebViewEnvironment: ICoreWebView2Environment;
    FWebViewSettings: ICoreWebView2Settings;
    procedure CloseWebView;
    function CreateCoreWebView2ControllerCompleted(AResult: HResult; const ACreatedController: ICoreWebView2Controller): HResult; stdcall;
    function CreateEnvironmentCompleted(AResult: HResult; const AEnvironment: ICoreWebView2Environment): HResult; stdcall;
    procedure CreateWebView;
    procedure DoContentLoading(const ASuccess: Boolean; const ANavigationID: UInt64);
    procedure DoCreateWebViewCompleted(const ASuccess: Boolean);
    procedure DoJavascriptEvaluated(const AErrorCode: LongInt; const AResultObjectAsJSON: string);
    procedure DoNavigationCompleted(const ASuccess: Boolean);
    procedure DoNavigationStarting(const AURL: string);
    procedure DoProcessFailed(const AFailureType: COREWEBVIEW2_PROCESS_FAILED_KIND);
    procedure InitializeWebView;
    procedure Navigate;
    procedure PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>); message PM_SET_SIZE;
    function ProcessHResult(AHResult: HResult): Boolean;
    procedure Resize;
    procedure SelectNextControl(const AForwards: Boolean);
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    function DefineModelClass: TDataModelClass; override;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    function CaptureBitmap: TBitmap;
    procedure EvaluateJavascript(const AJavascript: string);
    procedure NavigateURL(const AURL: string = '');
    procedure NavigateTo(const ANavigation: TBrowserNavigation);
    procedure NavigateToString(const AHTMLContent: string);
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure Refresh;
    procedure Stop;
    property CustomBrowser: ICustomBrowser read FCustomBrowser write FCustomBrowser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Model: TWindowsWebBrowserModel read FModel;
  end;

  TWindowsWBService = class(TWBFactoryService)
  protected
    function DoCreateWebBrowser: ICustomBrowser; override;
  end;

  { TWindowsWebBrowserService }

  TWindowsWebBrowserService = class;

  TWindowsWebBrowser = class;

  TWindowsWebBrowserModel = class(TDataModel)
  private
    FControl: TWindowsWebBrowser;
  protected
    property Control: TWindowsWebBrowser read FControl write FControl;
  public
    procedure DoContentLoading(const ASuccess: Boolean; const ANavigationID: UInt64);
    procedure DoCreateWebViewCompleted(const ASuccess: Boolean);
    procedure DoJavascriptEvaluated(const AErrorCode: LongInt; const AResultObjectAsJSON: string);
    procedure DoNavigationCompleted(const ASuccess: Boolean);
    procedure DoNavigationStarting(const AURL: string);
  end;

  TContentLoadingEvent = procedure(Sender: TObject; const Success: Boolean; const NavigationID: UInt64) of object;
  TCreateVebViewCompletedEvent = procedure(Sender: TObject; const Success: Boolean) of object;
  TJavascriptEvaluatedEvent = procedure(Sender: TObject; const ErrorCode: LongInt; const ResultObjectAsJSON: string) of object;
  TNavigationCompletedEvent = procedure(Sender: TObject; const Success: Boolean) of object;
  TNavigationStartingEvent = procedure(Sender: TObject; const URL: string) of object;

  TWindowsWebBrowser = class(TPresentedControl)
  private
    FCustomBrowser: ICustomBrowser;
    FNativePresentation: TWinNativeWebBrowser;
    FOnCreateWebViewCompleted: TCreateVebViewCompletedEvent;
    FOnContentLoading: TContentLoadingEvent;
    FOnJavascriptEvaluated: TJavascriptEvaluatedEvent;
    FOnNavigationCompleted: TNavigationCompletedEvent;
    FOnNavigationStarting: TNavigationStartingEvent;
    function GetModel: TWindowsWebBrowserModel;
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure DoContentLoading(const ASuccess: Boolean; const ANavigationID: UInt64);
    procedure DoCreateWebViewCompleted(const ASuccess: Boolean);
    procedure DoJavascriptEvaluated(const AErrorCode: LongInt; const AResultObjectAsJSON: string);
    procedure DoNavigationCompleted(const ASuccess: Boolean);
    procedure DoNavigationStarting(const AURL: string);
    procedure LoadPresentation; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
    procedure SetCustomBrowser(const ACustomBrowser: ICustomBrowser);
  public
    constructor Create(AOwner: TComponent); override;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    function CaptureBitmap: TBitmap;
    procedure EvaluateJavascript(const AJavascript: string);
    procedure NavigateURL(const AURL: string = '');
    procedure NavigateTo(const ANavigation: TBrowserNavigation);
    procedure NavigateToString(const AHTMLContent: string);
    procedure Refresh;
    procedure Stop;
    property Model: TWindowsWebBrowserModel read GetModel;
    property NativePresentation: TWinNativeWebBrowser read FNativePresentation;
    property OnCreateWebViewCompleted: TCreateVebViewCompletedEvent read FOnCreateWebViewCompleted write FOnCreateWebViewCompleted;
    property OnContentLoading: TContentLoadingEvent read FOnContentLoading write FOnContentLoading;
    property OnJavascriptEvaluated: TJavascriptEvaluatedEvent read FOnJavascriptEvaluated write FOnJavascriptEvaluated;
    property OnNavigationCompleted: TNavigationCompletedEvent read FOnNavigationCompleted write FOnNavigationCompleted;
    property OnNavigationStarting: TNavigationStartingEvent read FOnNavigationStarting write FOnNavigationStarting;
  end;

  TEdgeBrowserHelper = class helper for TWinNativeWebBrowser
    class var FBrowserExecutableFolders: TDictionary<TWinNativeWebBrowser, string>;
    class var FUserDataFolders: TDictionary<TWinNativeWebBrowser, string>;
    function GetBrowserExecutableFolder: string;
    procedure SetBrowserExecutableFolder(const Value: string);
    function GetUserDataFolder: string;
    procedure SetUserDataFolder(const Value: string);
    /// <summary>
    ///   If set this specifies the location of msedgewebview2.exe, used with WebView2 Fixed Version Distribution mode:
    ///   https://docs.microsoft.com/en-us/microsoft-edge/webview2/concepts/distribution#fixed-version-distribution-mode
    ///   If not set this defaults to looking for an installed version of the WebView2 runtime or alternatively an
    ///   installation of Edge Canary.
    ///   Note that setting this property affects the next creation of the underlying WebView2 control. If this
    ///   component has already created a WebView2 control then setting this property will have no effect unless the
    ///   WebView2 control gets recreated (e.g. by ReinitializeWebView or ReinitializeWebViewWithNewBrowser).
    ///   If the path contains \Edge\Application\ then the WebView2 creation will fail.
    /// </summary>
    property BrowserExecutableFolder: string read GetBrowserExecutableFolder write SetBrowserExecutableFolder;
    /// <summary>
    ///   If set this specifies the location of the user data folder.
    ///   If not set this defaults to the folder {your_exe_name}.WebView2 in the same folder as the executable.
    ///   If folder creation permission is not available to the process where the user data folder needs to be created
    ///   then the creation of the underlying WebView2 control can fail.
    ///   The application will need to take responsibility for cleaning up the user data folder when it is no longer
    ///   required.
    ///   Note that setting this property affects the next creation of the underlying WebView2 control. If this
    ///   component has already created a WebView2 control then setting this property will have no effect unless the
    ///   WebView2 control gets recreated (e.g. by ReinitializeWebView or ReinitializeWebViewWithNewBrowser).
    /// </summary>
    property UserDataFolder: string read GetUserDataFolder write SetUserDataFolder;
  end;

  TWindowsBrowserEngine = class(TInterfacedObject)
  private
    FService: TWindowsWebBrowserService;
  protected
    function CaptureBitmap: TBitmap; virtual; abstract;
    procedure EvaluateJavaScript(const JavaScript: string); virtual; abstract;
    function GetCanGoBack: Boolean; virtual; abstract;
    function GetCanGoForward: Boolean; virtual; abstract;
    function GetHandle: HWND; virtual; abstract;
    function GetParent: TFmxObject; virtual; abstract;
    function GetVisible: Boolean; virtual; abstract;
    procedure GoBack; virtual; abstract;
    procedure GoForward; virtual; abstract;
    procedure GoHome; virtual; abstract;
    procedure LoadDocumentFromStream(const AStream: TStringStream); virtual; abstract;
    procedure Navigate(const AURL: string; const AFlags: OleVariant); virtual; abstract;
    procedure Reload; virtual; abstract;
    procedure SetVisible(const Value: Boolean); virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure WebControlUpdated; virtual; abstract;
    property Service: TWindowsWebBrowserService read FService;
  public
    constructor Create(const AService: TWindowsWebBrowserService); virtual;
  end;

  TWindowsWebBrowserOLEFrameworkDelegate = class;

  TWindowsBrowserEngineIE = class(TWindowsBrowserEngine)
  private
    FCanGoBack: Boolean;
    FCanGoForward: Boolean;
    FInstance: TOleWebBrowser;
    FOLEDelegate: TWindowsWebBrowserOLEFrameworkDelegate;
    procedure WBCommandStateChange(Sender: TObject; Command: Integer; Enable: WordBool);
    procedure WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName,
      PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame,
      StatusCode: OleVariant; var Cancel: WordBool);
  protected
    function CaptureBitmap: TBitmap; override;
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure DoSetFocus;
    procedure EvaluateJavaScript(const JavaScript: string); override;
    function GetCanGoBack: Boolean; override;
    function GetCanGoForward: Boolean; override;
    function GetHandle: HWND; override;
    function GetParent: TFmxObject; override;
    function GetVisible: Boolean; override;
    procedure GoBack; override;
    procedure GoForward; override;
    procedure GoHome; override;
    procedure LoadDocumentFromStream(const AStream: TStringStream); override;
    procedure Navigate(const AURL: string; const AFlags: OleVariant); override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure Reload; override;
    procedure SetVisible(const Value: Boolean); override;
    procedure Stop; override;
    procedure WebControlUpdated; override;
    property Instance: TOleWebBrowser read FInstance;
  public
    constructor Create(const AService: TWindowsWebBrowserService); override;
    destructor Destroy; override;
  end;

  TWindowsBrowserEngineEdge = class(TWindowsBrowserEngine)
  private
    FURL: string;
    FWinBrowserControl: TWindowsWebBrowser;
    procedure EdgeCreateWebViewCompletedHandler(Sender: TObject; const ASuccess: Boolean);
    procedure EdgeContentLoadingHandler(Sender: TObject; const ASuccess: Boolean; const ANavigationID: UInt64);
    procedure EdgeJavascriptEvaluatedHandler(Sender: TObject; const AErrorCode: LongInt; const AResultObjectAsJSON: string);
    procedure EdgeNavigationCompletedHandler(Sender: TObject; const ASuccess: Boolean);
    procedure EdgeNavigationStartingHandler(Sender: TObject; const AURL: string);
  protected
    function CaptureBitmap: TBitmap; override;
    procedure EvaluateJavaScript(const JavaScript: string); override;
    function GetCanGoBack: Boolean; override;
    function GetCanGoForward: Boolean; override;
    function GetHandle: HWND; override;
    function GetParent: TFmxObject; override;
    function GetVisible: Boolean; override;
    procedure GoBack; override;
    procedure GoForward; override;
    procedure GoHome; override;
    procedure LoadDocumentFromStream(const AStream: TStringStream); override;
    procedure Navigate(const AURL: string; const AFlags: OleVariant); override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure Reload; override;
    procedure SetVisible(const Value: Boolean); override;
    procedure Stop; override;
    procedure WebControlUpdated; override;
  public
    constructor Create(const AService: TWindowsWebBrowserService); override;
    destructor Destroy; override;
  end;

  TWindowsWebBrowserOLEFrameworkDelegate = class(TOLEFrameworkDelegate)
  private
    FBrowserEngine: TWindowsBrowserEngineIE;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure DoSetFocus; override;
    function GetHandle: HWND; override;
    procedure Hide; override;
    procedure SetVisible(const Value: Boolean); override;
    procedure Show; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent; const ABrowserEngine: TWindowsBrowserEngineIE); reintroduce;
    procedure DefaultHandler(var Message); override;
  end;

  TWinWBMediator = class(TInterfacedObject, ICustomBrowser)
  private
    FBrowser: TWindowsWebBrowserService;
  public
    constructor Create;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    property WB: TWindowsWebBrowserService read FBrowser implements ICustomBrowser;
  end;

  TWindowsWebBrowserService = class(TInterfacedObject, ICustomBrowser, IWindowsBrowserProperties)
  private
    FBrowserEngine: TWindowsBrowserEngine;
    FCache: Boolean;
    FUrl: string;
    FWebControl: TCustomWebBrowser;
    FWindowsActiveEngine: TWindowsActiveEngine;
    FWindowsEngine: TWindowsEngine;
    procedure InitBrowser;
    function CaptureBitmap: TBitmap;
    procedure RemoveFromZOrderManager;
  protected
    procedure FailLoadingWithError;
    procedure FinishLoading(const AURL: string);
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure SetWindowsActiveEngine(const Value: TWindowsActiveEngine);
    procedure SetVisible(const Value: Boolean);
    procedure ShouldStartLoading(const AURL: string);
    procedure StartLoading(const AURL: string);
    procedure DoSetFocus;
    function GetHandle: HWND;
    function GetParent: TFmxObject;
    property WebControl: TCustomWebBrowser read FWebControl;
  public
    function GetWindowsActiveEngine: TWindowsActiveEngine;
    function GetWindowsEngine: TWindowsEngine;
    procedure SetWindowsEngine(const Value : TWindowsEngine);
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    function GetEnableCaching: Boolean;
    procedure SetEnableCaching(const Value: Boolean);
    procedure SetWebBrowserControl(const AValue: TCustomWebBrowser);
    function GetVisible: Boolean;
    procedure UpdateContentFromControl;
    procedure Navigate;
    procedure Reload;
    procedure Stop;
    procedure EvaluateJavaScript(const JavaScript: string);
    procedure LoadFromStrings(const AContent: string; const ABaseUrl: string); overload;
    procedure LoadFromStrings(const AContent: string; const AContentEncoding: TEncoding; const ABaseUrl: string); overload;
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure Show;
    procedure Hide;
    property URL: string read GetURL write SetURL;
    property EnableCaching: Boolean read GetEnableCaching write SetEnableCaching;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
    procedure ReleaseInstance;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TVCLObjectController = class(TInterfacedObject, IVCLComObject)
  private
    FComponent: TComponent;
  protected
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure FreeOnRelease;
  public
    constructor Create(Component: TComponent);
    destructor Destroy; override;
  end;

type
  TWinBitmap = record
    BitmapInfo: TBitmapInfo;
    DC: HDC;
    Bitmap: HBITMAP;
    Bits: PAlphaColorRecArray;
    Width, Height: Integer;
  end;

function CreateWinBitmap(const Width, Height: Integer): TWinBitmap;
begin
  Result.Width := Width;
  Result.Height := Height;
  Result.DC := CreateCompatibleDC(0);
  ZeroMemory(@(Result.BitmapInfo.bmiHeader), SizeOf(TBitmapInfoHeader));
  Result.BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
  Result.BitmapInfo.bmiHeader.biWidth := Width;
  Result.BitmapInfo.bmiHeader.biHeight := -Height;
  Result.BitmapInfo.bmiHeader.biPlanes := 1;
  Result.BitmapInfo.bmiHeader.biCompression := BI_RGB;
  Result.BitmapInfo.bmiHeader.biBitCount := 32;
  Result.Bitmap := CreateDIBSection(Result.DC, Result.BitmapInfo, DIB_RGB_COLORS, Pointer(Result.Bits), 0, 0);
  SetMapMode(Result.DC, MM_TEXT);
  SelectObject(Result.DC, Result.Bitmap);
end;

procedure DestroyWinBitmap(var Bitmap: TWinBitmap);
begin
  Bitmap.Bits := nil;
  DeleteObject(Bitmap.Bitmap);
  DeleteDC(Bitmap.DC);
end;

var
  WBService: TWindowsWBService;

procedure RegisterWebBrowserService;
begin
  WBService := TWindowsWBService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXWBService, WBService);
end;

procedure UnRegisterWebBrowserService;
begin
  if TPlatformServices.Current <> nil then
    TPlatformServices.Current.RemovePlatformService(IFMXWBService);
end;

{ Callback }

class function Callback<T1, T2>.CreateAs<INTF>(P: TStdProc1): INTF;
type
  PIntf = ^INTF;
begin
  Result := PIntf(@P)^;
end;

class function Callback<T1, T2>.CreateAs<INTF>(P: TStdProc2): INTF;
type
  PIntf = ^INTF;
begin
  Result := PIntf(@P)^;
end;

class function Callback<T1, T2>.CreateAs<INTF>(P: TStdProc3): INTF;
type
  PIntf = ^INTF;
begin
  Result := PIntf(@P)^;
end;

class function Callback<T1, T2>.CreateAs<INTF>(P: TStdMethod1): INTF;
begin
  Result := CreateAs<INTF>(
    function(const P1: T1): HResult stdcall
    begin
      Result := P(P1);
    end);
end;

class function Callback<T1, T2>.CreateAs<INTF>(P: TStdMethod2): INTF;
begin
  Result := CreateAs<INTF>(
    function(const P1: T1; const P2: T2): HResult stdcall
    begin
      Result := P(P1, P2)
    end);
end;

{ TWindowsWebBrowserService }

function TWindowsWebBrowserService.CaptureBitmap: TBitmap;
begin
  if FBrowserEngine <> nil then
    Result := FBrowserEngine.CaptureBitmap
  else
    Result := nil;
end;

procedure TWindowsWebBrowserService.EvaluateJavaScript(const JavaScript: string);
begin
  if FBrowserEngine <> nil then
    FBrowserEngine.EvaluateJavaScript(JavaScript);
end;

procedure TWindowsWebBrowserService.FailLoadingWithError;
begin
  if FWebControl <> nil then
    FWebControl.FailLoadingWithError;
end;

procedure TWindowsWebBrowserService.FinishLoading(const AURL: string);
begin
  SetURL(AURL);
  if FWebControl <> nil then
    FWebControl.FinishLoading;
end;

constructor TWindowsWebBrowserService.Create;
begin
  inherited;
  FWindowsActiveEngine := TWindowsActiveEngine.None;
end;

destructor TWindowsWebBrowserService.Destroy;
begin
  RemoveFromZOrderManager;
  inherited;
end;

procedure TWindowsWebBrowserService.DoSetFocus;
begin
  inherited;
  if FWebControl <> nil then
    FWebControl.SetFocus;
end;

function TWindowsWebBrowserService.GetWindowsActiveEngine: TWindowsActiveEngine;
begin
  Result := FWindowsActiveEngine;
end;

function TWindowsWebBrowserService.GetCanGoBack: Boolean;
begin
  if FBrowserEngine <> nil then
    Result := FBrowserEngine.GetCanGoBack
  else
    Result := False;
end;

function TWindowsWebBrowserService.GetCanGoForward: Boolean;
begin
  if FBrowserEngine <> nil then
    Result := FBrowserEngine.GetCanGoForward
  else
    Result := False;
end;

function TWindowsWebBrowserService.GetEnableCaching: Boolean;
begin
  Result := FCache;
end;

function TWindowsWebBrowserService.GetHandle: HWND;
begin
  if FBrowserEngine <> nil then
    Result := FBrowserEngine.GetHandle
  else
    Result := 0;
end;

function TWindowsWebBrowserService.GetParent: TFmxObject;
begin
  if FBrowserEngine <> nil then
    Result := FBrowserEngine.GetParent
  else
    Result := nil;
end;

function TWindowsWebBrowserService.GetWindowsEngine: TWindowsEngine;
begin
  Result := FWindowsEngine;
end;

function TWindowsWebBrowserService.GetURL: string;
begin
  Result := FUrl;
end;

function TWindowsWebBrowserService.GetVisible: Boolean;
begin
  if FBrowserEngine <> nil then
    Result := FBrowserEngine.GetVisible
  else
    Result := False;
end;

procedure TWindowsWebBrowserService.GoBack;
begin
  if FBrowserEngine <> nil then
    FBrowserEngine.GoBack;
end;

procedure TWindowsWebBrowserService.GoForward;
begin
  if FBrowserEngine <> nil then
    FBrowserEngine.GoForward;
end;

procedure TWindowsWebBrowserService.GoHome;
begin
  if FBrowserEngine <> nil then
    FBrowserEngine.GoHome;
end;

procedure TWindowsWebBrowserService.ShouldStartLoading(const AURL: string);
begin
  if FWebControl <> nil then
    FWebControl.ShouldStartLoading(AURL);
end;

procedure TWindowsWebBrowserService.Show;
begin
  SetVisible(True);
end;

procedure TWindowsWebBrowserService.Hide;
begin
  SetVisible(False);
end;

procedure TWindowsWebBrowserService.InitBrowser;
begin
  if (FWindowsEngine = TWindowsEngine.IEOnly) or ((FWindowsEngine = TWindowsEngine.EdgeIfAvailable) and not IsEdgeAvailable) then
  begin
    if FBrowserEngine = nil then
    begin
      FWindowsActiveEngine := TWindowsActiveEngine.IE;
      FBrowserEngine := TWindowsBrowserEngineIE.Create(Self);
    end;
  end
  else if (FWindowsEngine = TWindowsEngine.EdgeIfAvailable) or (FWindowsEngine = TWindowsEngine.EdgeOnly) then
  begin
    if (FBrowserEngine = nil) and IsEdgeAvailable then
    begin
      FWindowsActiveEngine := TWindowsActiveEngine.NoneYet;
      FBrowserEngine := TWindowsBrowserEngineEdge.Create(Self);
    end
    else if (FWindowsEngine = TWindowsEngine.EdgeOnly) and not IsEdgeAvailable then
      raise EBrowserEngineException.CreateRes(@SEdgeBrowserEngineUnavailable);
  end;
end;

procedure TWindowsWebBrowserService.LoadFromStrings(const AContent: string; const ABaseUrl: string);
begin
  LoadFromStrings(AContent, nil, ABaseUrl);
end;

procedure TWindowsWebBrowserService.LoadFromStrings(const AContent: string; const AContentEncoding: TEncoding; const ABaseUrl: string);
var
  StringStream: TStringStream;
begin
  if FBrowserEngine <> nil then
  begin
    if AContentEncoding <> nil then
      StringStream := TStringStream.Create(AContent, AContentEncoding)
    else
      StringStream := TStringStream.Create(AContent);
    try
      FBrowserEngine.LoadDocumentFromStream(StringStream);
    finally
      StringStream.Free;
    end;
  end;
end;

procedure TWindowsWebBrowserService.Navigate;
var
  Flags: OleVariant;
  NewURL: string;
begin
  if FBrowserEngine <> nil then
  begin
    Flags := 0;
    if not FCache then
      Flags := Flags or navNoReadFromCache or navNoWriteToCache;

    if Pos(TWebBrowser.FilesPref, URL) <> 0 then
    begin
      NewURL := Copy(URL, length(TWebBrowser.FilesPref) + 1, length(URL));
      if not FileExists(NewURL) then
        raise EFileNotFoundException.CreateRes(@SSpecifiedFileNotFound);
    end
    else
      NewURL := URL;
    FBrowserEngine.Navigate(NewURL, Flags);
  end;
end;

function TWindowsWebBrowserService.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FBrowserEngine <> nil) and (Result <> S_OK) then
    Result := FBrowserEngine.QueryInterface(IID, Obj);
end;

procedure TWindowsWebBrowserService.ReleaseInstance;
begin
  RemoveFromZOrderManager;
  FreeAndNil(FBrowserEngine);
end;

procedure TWindowsWebBrowserService.Reload;
begin
  if FBrowserEngine <> nil then
    FBrowserEngine.Reload;
end;

procedure TWindowsWebBrowserService.RemoveFromZOrderManager;
var
  Form: TCommonCustomForm;
begin
  if (FWebControl <> nil) and (FWebControl.Root is TCommonCustomForm) then
  begin
    Form := TCommonCustomForm(FWebControl.Root);
    if Form.IsHandleAllocated then
      WindowHandleToPlatform(Form.Handle).ZOrderManager.RemoveLink(FWebControl);
  end;
end;

procedure TWindowsWebBrowserService.SetWindowsActiveEngine(const Value: TWindowsActiveEngine);
begin
  FWindowsActiveEngine := Value;
end;

procedure TWindowsWebBrowserService.SetEnableCaching(const Value: Boolean);
begin
  FCache := Value;
end;

procedure TWindowsWebBrowserService.SetWindowsEngine(const Value: TWindowsEngine);
begin
  if (FWindowsEngine <> Value) then
  begin
    FWindowsEngine := Value;
    if FWebControl <> nil then
    begin
      ReleaseInstance;
      InitBrowser;
      UpdateContentFromControl;
    end;
  end;
end;

procedure TWindowsWebBrowserService.SetURL(const AValue: string);
begin
  FUrl := AValue;
end;

procedure TWindowsWebBrowserService.SetVisible(const Value: Boolean);
begin
  inherited;
  if FBrowserEngine <> nil then
    FBrowserEngine.SetVisible(Value);
end;

procedure TWindowsWebBrowserService.SetWebBrowserControl(const AValue: TCustomWebBrowser);
begin
  FWebControl := AValue;
  if FBrowserEngine <> nil then
    FBrowserEngine.WebControlUpdated;
end;

procedure TWindowsWebBrowserService.StartLoading(const AURL: string);
begin
  FUrl := AURL;
  if FWebControl <> nil then
    FWebControl.StartLoading;
end;

procedure TWindowsWebBrowserService.Stop;
begin
  if FBrowserEngine <> nil then
    FBrowserEngine.Stop;
end;

procedure TWindowsWebBrowserService.UpdateContentFromControl;
var
  ZOrderManager: TWinZOrderManager;
  Form: TCommonCustomForm;
begin
  if FWebControl.GetParentComponent <> nil then
  begin
    Form := TCommonCustomForm(FWebControl.Root);
    if Form <> nil then
    begin
      ZOrderManager := WindowHandleToPlatform(Form.Handle).ZOrderManager;
      ZOrderManager.UpdateOrderAndBounds(FWebControl);
    end;
  end;
end;

{ TWindowsWBService }

function TWindowsWBService.DoCreateWebBrowser: ICustomBrowser;
begin
  Result := TWinWBMediator.Create;
end;

{ TVCLObjectController }

constructor TVCLObjectController.Create(Component: TComponent);
begin
  inherited Create;
  FComponent := Component;
  FComponent.VCLComObject := Pointer(IVCLComObject(Self));
end;

destructor TVCLObjectController.Destroy;
begin
  if Assigned(FComponent) then
    FComponent.Free;
  inherited;
end;

procedure TVCLObjectController.FreeOnRelease;
begin

end;

function TVCLObjectController.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLObjectController.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLObjectController.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TVCLObjectController.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ TWinWBMediator }

constructor TWinWBMediator.Create;
begin
  FBrowser := TWindowsWebBrowserService.Create;
end;

destructor TWinWBMediator.Destroy;
begin
  FBrowser.ReleaseInstance;
  inherited;
end;

function TWinWBMediator.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FBrowser <> nil) and (Result <> S_OK) then
    Result := FBrowser.QueryInterface(IID, Obj);
end;

{ TWindowsWebBrowserModel }

procedure TWindowsWebBrowserModel.DoContentLoading(const ASuccess: Boolean; const ANavigationID: UInt64);
begin
  FControl.DoContentLoading(ASuccess, ANavigationID);
end;

procedure TWindowsWebBrowserModel.DoCreateWebViewCompleted(const ASuccess: Boolean);
begin
  FControl.DoCreateWebViewCompleted(ASuccess);
end;

procedure TWindowsWebBrowserModel.DoJavascriptEvaluated(const AErrorCode: LongInt; const AResultObjectAsJSON: string);
begin
  FControl.DoJavascriptEvaluated(AErrorCode, AResultObjectAsJSON);
end;

procedure TWindowsWebBrowserModel.DoNavigationCompleted(const ASuccess: Boolean);
begin
  FControl.DoNavigationCompleted(ASuccess);
end;

procedure TWindowsWebBrowserModel.DoNavigationStarting(const AURL: string);
begin
  FControl.DoNavigationStarting(AURL);
end;

{ TWindowsWebBrowser }

constructor TWindowsWebBrowser.Create(AOwner: TComponent);
begin
  inherited;
  Model.Control := Self;
end;

function TWindowsWebBrowser.DefineModelClass: TDataModelClass;
begin
  Result := TWindowsWebBrowserModel;
end;

procedure TWindowsWebBrowser.LoadPresentation;
begin
  inherited;
  if ControlType = TControlType.Platform then
  begin
    FNativePresentation := TWinNativeWebBrowser(Presentation);
    FNativePresentation.CustomBrowser := FCustomBrowser;
  end;
end;

procedure TWindowsWebBrowser.DoContentLoading(const ASuccess: Boolean; const ANavigationID: UInt64);
begin
  if Assigned(FOnContentLoading) then
    FOnContentLoading(Self, ASuccess, ANavigationID);
end;

procedure TWindowsWebBrowser.DoCreateWebViewCompleted(const ASuccess: Boolean);
begin
  if Assigned(FOnCreateWebViewCompleted) then
    FOnCreateWebViewCompleted(Self, ASuccess);
end;

procedure TWindowsWebBrowser.DoJavascriptEvaluated(const AErrorCode: LongInt; const AResultObjectAsJSON: string);
begin
  if Assigned(FOnJavascriptEvaluated) then
    FOnJavascriptEvaluated(Self, AErrorCode, AResultObjectAsJSON);
end;

procedure TWindowsWebBrowser.DoNavigationCompleted(const ASuccess: Boolean);
begin
  if Assigned(FOnNavigationCompleted) then
    FOnNavigationCompleted(Self, ASuccess);
end;

procedure TWindowsWebBrowser.DoNavigationStarting(const AURL: string);
begin
  if Assigned(FOnNavigationStarting) then
    FOnNavigationStarting(Self, AURL);
end;

function TWindowsWebBrowser.GetModel: TWindowsWebBrowserModel;
begin
  Result := inherited GetModel<TWindowsWebBrowserModel>;
end;

function TWindowsWebBrowser.CanGoBack: Boolean;
begin
  Result := FNativePresentation.CanGoBack;
end;

function TWindowsWebBrowser.CanGoForward: Boolean;
begin
  Result := FNativePresentation.CanGoForward;
end;

function TWindowsWebBrowser.CaptureBitmap: TBitmap;
begin
  Result := FNativePresentation.CaptureBitmap;
end;

procedure TWindowsWebBrowser.EvaluateJavascript(const AJavascript: string);
begin
  FNativePresentation.EvaluateJavascript(AJavascript);
end;

procedure TWindowsWebBrowser.NavigateTo(const ANavigation: TBrowserNavigation);
begin
  FNativePresentation.NavigateTo(ANavigation);
end;

procedure TWindowsWebBrowser.NavigateToString(const AHTMLContent: string);
begin
  FNativePresentation.NavigateToString(AHTMLContent);
end;

procedure TWindowsWebBrowser.NavigateURL(const AURL: string = '');
begin
  FNativePresentation.NavigateURL(AURL);
end;

function TWindowsWebBrowser.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

procedure TWindowsWebBrowser.Refresh;
begin
  FNativePresentation.Refresh;
end;

procedure TWindowsWebBrowser.SetCustomBrowser(const ACustomBrowser: ICustomBrowser);
begin
  FCustomBrowser := ACustomBrowser;
end;

procedure TWindowsWebBrowser.Stop;
begin
  FNativePresentation.Stop;
end;

{ TEdgeBrowserHelper }

function TEdgeBrowserHelper.GetBrowserExecutableFolder: string;
begin
  Result := '';
  if FBrowserExecutableFolders <> nil then
    FBrowserExecutableFolders.TryGetValue(Self, Result)
end;

procedure TEdgeBrowserHelper.SetBrowserExecutableFolder(const Value: string);
begin
  if FBrowserExecutableFolders = nil then
    FBrowserExecutableFolders := TDictionary<TWinNativeWebBrowser, string>.Create;
  FBrowserExecutableFolders.AddOrSetValue(Self, Value)
end;

function TEdgeBrowserHelper.GetUserDataFolder: string;
begin
  Result := '';
  if FUserDataFolders <> nil then
    FUserDataFolders.TryGetValue(Self, Result)
end;

procedure TEdgeBrowserHelper.SetUserDataFolder(const Value: string);
begin
  if FUserDataFolders = nil then
    FUserDataFolders := TDictionary<TWinNativeWebBrowser, string>.Create;
  FUserDataFolders.AddOrSetValue(Self, Value)
end;

function EdgeBrowserGetBrowserExecutableFolder(EdgeBrowser: TWinNativeWebBrowser): string;
begin
  Result := EdgeBrowser.BrowserExecutableFolder
end;

function EdgeBrowserGetUserDataFolder(EdgeBrowser: TWinNativeWebBrowser): string;
begin
  Result := EdgeBrowser.UserDataFolder
end;

procedure EdgeBrowserSetBrowserExecutableFolder(EdgeBrowser: TWinNativeWebBrowser; const S: string);
begin
  EdgeBrowser.BrowserExecutableFolder := S
end;

procedure EdgeBrowserSetUserDataFolder(EdgeBrowser: TWinNativeWebBrowser; const S: string);
begin
  EdgeBrowser.UserDataFolder := S
end;

{ TWinNativeWebBrowser }

constructor TWinNativeWebBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FModel := TWindowsWebBrowserModel(inherited Model);
  FBrowserControlState := TBrowserControlState.None;
  FCritSec := TCriticalSection.Create;
  FDevToolsProtocolEventReceivedTokenMap := TDictionary<string, EventRegistrationToken>.Create;
end;

destructor TWinNativeWebBrowser.Destroy;
begin
  FDevToolsProtocolEventReceivedTokenMap.Free;
  FCritSec.Free;
  inherited;
end;

procedure TWinNativeWebBrowser.CreateHandle;
begin
  inherited;
  CreateWebView;
end;

procedure TWinNativeWebBrowser.DestroyHandle;
begin
  inherited;
  // Not implemented
end;

procedure TWinNativeWebBrowser.DoContentLoading(const ASuccess: Boolean; const ANavigationID: UInt64);
begin
  // Not implemented yet
end;

procedure TWinNativeWebBrowser.DoCreateWebViewCompleted(const ASuccess: Boolean);
begin
  Model.DoCreateWebViewCompleted(ASuccess);
end;

procedure TWinNativeWebBrowser.DoJavascriptEvaluated(const AErrorCode: LongInt; const AResultObjectAsJSON: string);
begin
  Model.DoJavascriptEvaluated(AErrorCode, AResultObjectAsJSON);
end;

procedure TWinNativeWebBrowser.DoNavigationCompleted(const ASuccess: Boolean);
begin
  Model.DoNavigationCompleted(ASuccess);
end;

procedure TWinNativeWebBrowser.DoNavigationStarting(const AURL: string);
begin
  Model.DoNavigationStarting(AURL);
end;

procedure TWinNativeWebBrowser.DoProcessFailed(const AFailureType: COREWEBVIEW2_PROCESS_FAILED_KIND);
begin
  // Not implemented yet
end;

procedure TWinNativeWebBrowser.EvaluateJavascript(const AJavascript: string);
begin
  if FWebView <> nil then
  begin
    FWebView.ExecuteScript(PWideChar(AJavascript),
      Callback<HResult, PWideChar>.CreateAs<ICoreWebView2ExecuteScriptCompletedHandler>(
        function(ErrorCode: HResult; ResultObjectAsJson: PWideChar): HResult stdcall
        begin
          Result := S_OK;
          DoJavascriptEvaluated(ErrorCode, string(ResultObjectAsJson));
        end
      )
    );
  end;
end;

function TWinNativeWebBrowser.CreateCoreWebView2ControllerCompleted(AResult: HResult; const ACreatedController: ICoreWebView2Controller): HResult;
begin
  Result := S_OK;

  if AResult = E_ABORT then
    Exit;

  if ProcessHResult(AResult) then
  begin
    FWebViewController := ACreatedController;
    var LHResult := FWebViewController.Get_CoreWebView2(FWebView);
    if ProcessHResult(LHResult) then
    begin
      LHResult := FWebView.Get_Settings(FWebViewSettings);
      if ProcessHResult(LHResult) then
      begin
        FBrowserControlState := TBrowserControlState.Created;
        DoCreateWebViewCompleted(True);
        {$REGION 'WebView2 event handlers'}
        if (FWebView <> nil) and (FWebViewController <> nil) then
        begin
          // Register a handler for the NavigationStarting event.
          // The triggered event could, say, enable a Cancel button.
          FWebView.add_NavigationStarting(
            Callback<ICoreWebView2, ICoreWebView2NavigationStartingEventArgs>.CreateAs<ICoreWebView2NavigationStartingEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2NavigationStartingEventArgs): HResult stdcall
              begin
                Result := S_OK;
                var URI: PChar;
                Args.Get_uri(URI);
                DoNavigationStarting(string(URI));
              end
            ),
            FNavigationStartingToken);
          // Register a handler for the NavigationCompleted event.
          // The triggered event could, say, check whether the navigation succeeded, and if not, do something.
          // It could also update the Back, Forward, and Cancel buttons.
          FWebView.add_NavigationCompleted(
            Callback<ICoreWebView2, ICoreWebView2NavigationCompletedEventArgs>.CreateAs<ICoreWebView2NavigationCompletedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2NavigationCompletedEventArgs): HResult stdcall
              begin
                Result := S_OK;
                var IsSuccess: Integer := 0;
                Args.Get_IsSuccess(IsSuccess);
                DoNavigationCompleted(LongBool(IsSuccess));
              end
            ),
            FNavigationCompletedToken);
          // Register a handler for the SourceChanged event.
          // The triggered event could, say, read the webview's source URI and update an address bar.
          FWebView.add_SourceChanged(
            Callback<ICoreWebView2, ICoreWebView2SourceChangedEventArgs>.CreateAs<ICoreWebView2SourceChangedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2SourceChangedEventArgs): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FSourceChangedToken);
          // Register a handler for the HistoryChanged event.
          // The triggered event could, say, check whether Back or Forward buttons should be enabled.
          FWebView.add_HistoryChanged(
            Callback<ICoreWebView2, IUnknown>.CreateAs<ICoreWebView2HistoryChangedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: IUnknown): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FHistoryChangedToken);
          // Register a handler for the ContentLoading event.
          FWebView.add_ContentLoading(
            Callback<ICoreWebView2, ICoreWebView2ContentLoadingEventArgs>.CreateAs<ICoreWebView2ContentLoadingEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2ContentLoadingEventArgs): HResult stdcall
              begin
                Result := S_OK;
                  var IsErrorPage: Integer := 0;
                  Args.Get_IsErrorPage(IsErrorPage);
                  var NavigationId: UInt64 := 0;
                  Args.Get_NavigationId(NavigationId);
                  DoContentLoading(LongBool(IsErrorPage), NavigationId);
              end
            ),
            FContentLoadingToken);
          // Register a handler for the DocumentTitleChanged event.
          FWebView.add_DocumentTitleChanged(
            Callback<ICoreWebView2, IUnknown>.CreateAs<ICoreWebView2DocumentTitleChangedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: IUnknown): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FDocumentTitleChangedToken);
          // Register a handler for the NewWindowRequested event.
          FWebView.add_NewWindowRequested(
            Callback<ICoreWebView2, ICoreWebView2NewWindowRequestedEventArgs>.CreateAs<ICoreWebView2NewWindowRequestedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2NewWindowRequestedEventArgs): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FNewWindowRequestedToken);
          // Register a handler for the FrameNavigationStarting event.
          FWebView.add_FrameNavigationStarting(
            Callback<ICoreWebView2, ICoreWebView2NavigationStartingEventArgs>.CreateAs<ICoreWebView2NavigationStartingEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2NavigationStartingEventArgs): HResult stdcall
              begin
                Result := S_OK;
                var URI: PChar;
                Args.Get_uri(URI);
                DoNavigationStarting(string(URI));
              end
            ),
            FFrameNavigationStartingToken);
          // Register a handler for the FrameNavigationCompleted event.
          FWebView.add_FrameNavigationCompleted(
            Callback<ICoreWebView2, ICoreWebView2NavigationCompletedEventArgs>.CreateAs<ICoreWebView2NavigationCompletedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2NavigationCompletedEventArgs): HResult stdcall
              begin
                Result := S_OK;
                var IsSuccess: Integer := 0;
                Args.Get_IsSuccess(IsSuccess);
                DoNavigationCompleted(LongBool(IsSuccess));
              end
            ),
            FFrameNavigationStartingToken);
          // Register a handler for the WebResourceRequested event.
          FWebView.add_WebResourceRequested(
            Callback<ICoreWebView2, ICoreWebView2WebResourceRequestedEventArgs>.CreateAs<ICoreWebView2WebResourceRequestedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: ICoreWebView2WebResourceRequestedEventArgs): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FWebResourceRequestedToken);
          // Register a handler for the ScriptDialogOpening event.
          FWebView.add_ScriptDialogOpening(
            Callback<ICoreWebView2, ICoreWebView2ScriptDialogOpeningEventArgs>.CreateAs<ICoreWebView2ScriptDialogOpeningEventHandler>(
              function(const Webview: ICoreWebView2; const Args: ICoreWebView2ScriptDialogOpeningEventArgs): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FScriptDialogOpeningToken);
          // Register a handler for the PermissionRequested event.
          FWebView.add_PermissionRequested(
            Callback<ICoreWebView2, ICoreWebView2PermissionRequestedEventArgs>.CreateAs<ICoreWebView2PermissionRequestedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: ICoreWebView2PermissionRequestedEventArgs): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FPermissionRequestedToken);
          // Register a handler for the ProcessFailed event.
          FWebView.add_ProcessFailed(
            Callback<ICoreWebView2, ICoreWebView2ProcessFailedEventArgs>.CreateAs<ICoreWebView2ProcessFailedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: ICoreWebView2ProcessFailedEventArgs): HResult stdcall
              begin
                Result := S_OK;
                var FailureType: COREWEBVIEW2_PROCESS_FAILED_KIND := COREWEBVIEW2_PROCESS_FAILED_KIND_BROWSER_PROCESS_EXITED;
                Args.Get_ProcessFailedKind(FailureType);
                DoProcessFailed(FailureType);
              end
            ),
            FProcessFailedToken);
          // Register a handler for the WebMessageReceived event.
          FWebView.add_WebMessageReceived(
            Callback<ICoreWebView2, ICoreWebView2WebMessageReceivedEventArgs>.CreateAs<ICoreWebView2WebMessageReceivedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: ICoreWebView2WebMessageReceivedEventArgs): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FWebMessageReceivedToken);
          // Register a handler for the ContainsFullScreenElementChanged event.
          FWebView.add_ContainsFullScreenElementChanged(
            Callback<ICoreWebView2, IUnknown>.CreateAs<ICoreWebView2ContainsFullScreenElementChangedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: IUnknown): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FContainsFullScreenElementChangedToken);
          // Register a handler for the WindowClosedRequested event.
          FWebView.add_WindowCloseRequested(
            Callback<ICoreWebView2, IUnknown>.CreateAs<ICoreWebView2WindowCloseRequestedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: IUnknown): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FWindowCloseRequestedToken);
          // Register a handler for the ZoomFactorChanged event.
          FWebViewController.add_ZoomFactorChanged(
            Callback<ICoreWebView2, IUnknown>.CreateAs<ICoreWebView2ZoomFactorChangedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: IUnknown): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FZoomFactorChangedToken);
          // Register a handler for the MoveFocusRequested event.
          FWebViewController.add_MoveFocusRequested(
            Callback<ICoreWebView2, ICoreWebView2MoveFocusRequestedEventArgs>.CreateAs<ICoreWebView2MoveFocusRequestedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2MoveFocusRequestedEventArgs): HResult stdcall
              begin
                Result := S_OK;
                var Reason: COREWEBVIEW2_MOVE_FOCUS_REASON;
                if ProcessHResult(Args.Get_reason(Reason)) then
                begin
                  case Reason of
                    COREWEBVIEW2_MOVE_FOCUS_REASON_PREVIOUS:
                      SelectNextControl(False);
                    COREWEBVIEW2_MOVE_FOCUS_REASON_NEXT:
                      SelectNextControl(True);
                  end;
                end;
              end
            ),
            FMoveFocusRequestedToken);
          // Register a handler for the AcceleratorKeyPressed event.
          FWebViewController.add_AcceleratorKeyPressed(
            Callback<ICoreWebView2, ICoreWebView2AcceleratorKeyPressedEventArgs>.CreateAs<ICoreWebView2AcceleratorKeyPressedEventHandler>(
              function(const Sender: ICoreWebView2; const Args: ICoreWebView2AcceleratorKeyPressedEventArgs): HResult stdcall
              begin
                Result := S_OK;
              end
            ),
            FAcceleratorKeyPressedToken);
          // Register a handler for the GotFocus event.
          FWebViewController.add_GotFocus(
            Callback<ICoreWebView2, IUnknown>.CreateAs<ICoreWebView2FocusChangedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: IUnknown): HResult stdcall
              begin
                Result := S_OK;
                FWebViewFocusEventActive := True;
                SetFocus;
                FWebViewFocusEventActive := False;
              end
            ),
            FGotFocusToken);
          // Register a handler for the LostFocus event.
          FWebViewController.add_LostFocus(
            Callback<ICoreWebView2, IUnknown>.CreateAs<ICoreWebView2FocusChangedEventHandler>(
              function(const Webview: ICoreWebView2; const Args: IUnknown): HResult stdcall
              begin
                Result := S_OK;
                ResetFocus;
              end
            ),
            FLostFocusToken);
          // Set the initial size of the WebView
          Resize;
        end;
        {$ENDREGION}
        // Navigate to saved URL, if there is one
        if Length(FLastURL) > 0 then
          NavigateURL(FLastURL);
      end
    end;
    if Winapi.Windows.Failed(LHResult) then
      DoCreateWebViewCompleted(False);
  end
  else
  begin
    // A problem occurred along the way
    FBrowserControlState := TBrowserControlState.Failed;
    DoCreateWebViewCompleted(False);
  end;
end;

procedure TWinNativeWebBrowser.SelectNextControl(const AForwards: Boolean);
var
  LTabList: ITabList;
  LParent: TControl;
  LCurrent, LNext: IControl;
begin
  if (Control <> nil) and (Control.Root <> nil) then
  begin
    LCurrent := Control.Root.Focused;
    LParent := TControl(LCurrent.GetObject).ParentControl;
    while LParent <> nil do
    begin
      LTabList := LParent.GetTabList;
      LNext := LTabList.FindNextTabStop(LCurrent, AForwards, True);
      if (LNext = nil) or (LNext = LCurrent) then
      begin
        Supports(LParent, IControl, LCurrent);
        LParent := LParent.ParentControl;
      end
      else
        Break;
    end;
    if LNext <> nil then
      LNext.SetFocus;
  end;
end;

function TWinNativeWebBrowser.CreateEnvironmentCompleted(AResult: HResult; const AEnvironment: ICoreWebView2Environment): HResult;
begin
  if ProcessHResult(AResult) then
  begin
    FWebViewEnvironment := AEnvironment;
    // Add a reference so that after we have cleared up entirely and the
    // WebView tries to tidy up its environment, we don't get a failure
    FWebViewEnvironment._AddRef;
    var AHResult := FWebViewEnvironment.CreateCoreWebView2Controller(Handle,
      Callback<HResult, ICoreWebView2Controller>.CreateAs<ICoreWebView2CreateCoreWebView2ControllerCompletedHandler>(
        CreateCoreWebView2ControllerCompleted
      ));
    if not ProcessHResult(AHResult) then
    begin
      FBrowserControlState := TBrowserControlState.Failed;
      DoCreateWebViewCompleted(False);
    end;
  end;
  Result := S_OK;
end;

procedure TWinNativeWebBrowser.CreateWebView;
begin
  if (FWebView = nil) and (FBrowserControlState <> TBrowserControlState.Creating) then
  begin
    FBrowserControlState := TBrowserControlState.Creating;
    InitializeWebView;
  end;
end;

function TWinNativeWebBrowser.CanGoBack: Boolean;
begin
  Result := False;
  if FWebView <> nil then
  begin
    var ResultInt: Integer;
    if ProcessHResult(FWebView.Get_CanGoBack(ResultInt)) then
      Result := LongBool(ResultInt);
  end;
end;

function TWinNativeWebBrowser.CanGoForward: Boolean;
begin
  Result := False;
  if FWebView <> nil then
  begin
    var ResultInt: Integer;
    if ProcessHResult(FWebView.Get_CanGoForward(ResultInt)) then
      Result := LongBool(ResultInt);
  end;
end;

function TWinNativeWebBrowser.CaptureBitmap: TBitmap;
var
  Stream: TMemoryStream;
begin
  Result := nil;
  if FWebView <> nil then
  begin
    Stream := TMemoryStream.Create;
    try
      var IsCapturing: Boolean;
      var NewPos: LargeUInt;
      var AdapterIntf: IStream := TStreamAdapter.Create(Stream);
      AdapterIntf.Seek(0, STREAM_SEEK_SET, NewPos);
      var Handler :=
        function(AResult: HResult): HResult stdcall
        begin
          Result := S_OK;
          IsCapturing := False;
        end;
      IsCapturing := True;
      if ProcessHResult(FWebView.CapturePreview(COREWEBVIEW2_CAPTURE_PREVIEW_IMAGE_FORMAT_JPEG, AdapterIntf,
        ICoreWebView2CapturePreviewCompletedHandler(PInterface(@Handler)^))) then
      begin
        while IsCapturing do
          Application.ProcessMessages;
        Result := TBitmap.CreateFromStream(Stream);
      end;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TWinNativeWebBrowser.CloseWebView;
begin
  if FWebView <> nil then
  begin
    try
      FWebView.remove_NavigationStarting(FNavigationStartingToken);
      FWebView.remove_NavigationCompleted(FNavigationCompletedToken);
      FWebView.remove_SourceChanged(FSourceChangedToken);
      FWebView.remove_HistoryChanged(FHistoryChangedToken);
      FWebView.remove_ContentLoading(FContentLoadingToken);
      FWebView.remove_DocumentTitleChanged(FDocumentTitleChangedToken);
      FWebView.remove_NewWindowRequested(FNewWindowRequestedToken);
      FWebView.remove_FrameNavigationStarting(FFrameNavigationStartingToken);
      FWebView.remove_FrameNavigationCompleted(FFrameNavigationCompletedToken);
      FWebView.remove_WebResourceRequested(FWebResourceRequestedToken);
      FWebView.remove_ScriptDialogOpening(FScriptDialogOpeningToken);
      FWebView.remove_PermissionRequested(FPermissionRequestedToken);
      FWebView.remove_ProcessFailed(FProcessFailedToken);
      FWebView.remove_WebMessageReceived(FWebMessageReceivedToken);
      FWebView.remove_WindowCloseRequested(FWindowCloseRequestedToken);
      FWebView.remove_ContainsFullScreenElementChanged(FContainsFullScreenElementChangedToken);
      for var CDPEventName in FDevToolsProtocolEventReceivedTokenMap.Keys do
      begin
        var Receiver: ICoreWebView2DevToolsProtocolEventReceiver;
        if ProcessHResult(FWebView.GetDevToolsProtocolEventReceiver(PChar(CDPEventname), Receiver)) then
          Receiver.remove_DevToolsProtocolEventReceived(FDevToolsProtocolEventReceivedTokenMap[CDPEventName]);
      end;
      FDevToolsProtocolEventReceivedTokenMap.Clear;
    finally
      FWebView := nil;
    end;
  end;
  if FWebViewController <> nil then
  begin
    try
      FWebViewController.remove_AcceleratorKeyPressed(FAcceleratorKeyPressedToken);
      FWebViewController.remove_GotFocus(FGotFocusToken);
      FWebViewController.remove_LostFocus(FLostFocusToken);
      FWebViewController.remove_MoveFocusRequested(FMoveFocusRequestedToken);
      FWebViewController.remove_ZoomFactorChanged(FZoomFactorChangedToken);
      FWebViewController.Close;
    finally
      FWebViewController := nil;
    end;
  end;
end;

procedure TWinNativeWebBrowser.InitializeWebView;
begin
  // To ensure browser switches get applied correctly, we need to close
  // the existing WebView. This will result in a new browser process
  // getting created which will apply the browser switches.
  CloseWebView;
  // Initialise any settings here
  FWebViewFocusEventActive := False;
  // Create the environment
  var hr := CreateCoreWebView2EnvironmentWithOptions(PChar(BrowserExecutableFolder), PChar(UserDataFolder), nil,
    Callback<HResult, ICoreWebView2Environment>.CreateAs<ICoreWebView2CreateCoreWebView2EnvironmentCompletedHandler>(
      CreateEnvironmentCompleted));
  if not ProcessHResult(hr) then
  begin
    FBrowserControlState := TBrowserControlState.Failed;
    DoCreateWebViewCompleted(False);
  end;
end;

procedure TWinNativeWebBrowser.Navigate;
var
  URL, NewURL: string;
begin
  URL := FURL;
  if Pos(TWebBrowser.FilesPref, URL) <> 0 then
  begin
    NewURL := Copy(URL, Length(TWebBrowser.FilesPref) + 1, Length(URL));
    if not FileExists(NewURL) then
      raise EFileNotFoundException.CreateRes(@SSpecifiedFileNotFound);
  end
  else
    NewURL := URL;
  if NewURL.Trim.Length > 0 then
    FLastURL := NewURL;
  if FWebView <> nil then
    ProcessHResult(FWebView.Navigate(PChar(NewURL)));
end;

procedure TWinNativeWebBrowser.NavigateTo(const ANavigation: TBrowserNavigation);
begin
  if FWebView <> nil then
  begin
    case ANavigation of
      TBrowserNavigation.GoBack:
        FWebView.GoBack;
      TBrowserNavigation.GoForward:
        FWebView.GoForward;
    end;
    // No Edge equivalent for GoHome
  end;
end;

procedure TWinNativeWebBrowser.NavigateToString(const AHTMLContent: string);
begin
  if FWebView <> nil then
    ProcessHResult(FWebView.NavigateToString(PChar(AHTMLContent)));
end;

procedure TWinNativeWebBrowser.NavigateURL(const AURL: string = '');
begin
  if not AURL.IsEmpty then
    FURL := AURL;
  Navigate;
end;

procedure TWinNativeWebBrowser.PMSetSize(var AMessage: TDispatchMessageWithValue<TSizeF>);
begin
  inherited;
  Resize;
end;

function TWinNativeWebBrowser.ProcessHResult(AHResult: HResult): Boolean;
begin
  FCritSec.Enter;
  try
    FLastErrorCode := AHResult;
    Result := Succeeded(FLastErrorCode);
  finally
    FCritSec.Leave;
  end;
end;

function TWinNativeWebBrowser.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FWebView <> nil) and (Result <> S_OK) then
    Result := FWebView.QueryInterface(IID, Obj);
end;

procedure TWinNativeWebBrowser.Refresh;
begin
  if FWebView <> nil then
    FWebView.Reload;
end;

procedure TWinNativeWebBrowser.Resize;
begin
  if (FWebViewController <> nil) and (Control <> nil) then
  begin
    var DesiredBounds := Control.BoundsRect.Truncate;
    DesiredBounds.Bottom := Trunc((DesiredBounds.Bottom - DesiredBounds.Top) * Scale + DesiredBounds.Top);
    DesiredBounds.Right := Trunc((DesiredBounds.Right - DesiredBounds.Left) * Scale + DesiredBounds.Left);
    ProcessHResult(FWebViewController.Set_Bounds(tagRECT(DesiredBounds)));
  end;
end;

procedure TWinNativeWebBrowser.Stop;
begin
  if FWebView <> nil then
    FWebView.Stop;
end;

function TWinNativeWebBrowser.DefineModelClass: TDataModelClass;
begin
  Result := TWindowsWebBrowserModel;
end;

{ TWindowsWebBrowserOLEFrameworkDelegate }

constructor TWindowsWebBrowserOLEFrameworkDelegate.Create(AOwner: TComponent; const ABrowserEngine: TWindowsBrowserEngineIE);
begin
  inherited Create(AOwner);
  FBrowserEngine := ABrowserEngine;
  TVCLObjectController.Create(Self);
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.CreateHandle;
begin
  inherited;
  FBrowserEngine.CreateHandle;
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.DefaultHandler(var Message);
var
  LMessage: TMessage;
begin
  if HandleAllocated and (FBrowserEngine.Instance.MiscStatus and OLEMISC_SIMPLEFRAME = 0) then
  begin
    LMessage := TMessage(Message);
    LMessage.Result := CallWindowProc(DefWndProc, Handle, LMessage.Msg, LMessage.WParam, LMessage.LParam);
  end
  else
    inherited DefaultHandler(Message);
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.DestroyHandle;
begin
  inherited;
  FBrowserEngine.DestroyHandle;
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.DoSetFocus;
begin
  inherited;
  FBrowserEngine.DoSetFocus;
end;

function TWindowsWebBrowserOLEFrameworkDelegate.GetHandle: HWND;
begin
  Result := WindowHandle;
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.Hide;
begin
  SetVisible(False);
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.SetVisible(const Value: Boolean);
begin
  inherited;
  FBrowserEngine.SetVisible(Value);
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.Show;
begin
  SetVisible(True);
end;

procedure TWindowsWebBrowserOLEFrameworkDelegate.WndProc(var Message: TMessage);
const
  CN_BASE = $BC00;
var
  WinMsg: TMsg;
  FOleInPlaceActiveObject: IOleInPlaceActiveObject;
begin
  if InRange(Message.Msg, CN_BASE + WM_KEYFIRST, CN_BASE + WM_KEYLAST) and
     Supports(FBrowserEngine.FInstance.ControlInterface, IOleInPlaceActiveObject, FOleInPlaceActiveObject) then
  begin
    WinMsg.HWnd := Handle;
    WinMsg.Message := Message.Msg - CN_BASE;
    WinMsg.WParam := Message.WParam;
    WinMsg.LParam := Message.LParam;
    WinMsg.Time := GetMessageTime;
    WinMsg.Pt.X := $115DE1F1;
    WinMsg.Pt.Y := $115DE1F1;
    if FOleInPlaceActiveObject.TranslateAccelerator(WinMsg) = S_OK then
    begin
      Message.Result := 1;
      Exit;
    end;
  end;
  inherited;
end;

{ TWindowsBrowserEngine }

constructor TWindowsBrowserEngine.Create(const AService: TWindowsWebBrowserService);
begin
  inherited Create;
  FService := AService;
end;

{ TWindowsBrowserEngineIE }

constructor TWindowsBrowserEngineIE.Create(const AService: TWindowsWebBrowserService);
begin
  inherited;
  FOLEDelegate := TWindowsWebBrowserOLEFrameworkDelegate.Create(nil, Self);
  FInstance := TOleWebBrowser.Create(FOLEDelegate);
  FInstance.OnCommandStateChange := WBCommandStateChange;
  FInstance.OnBeforeNavigate2 := WebBrowserBeforeNavigate2;
  FInstance.OnNavigateError := WebBrowserNavigateError;
  FInstance.OnNavigateComplete2 := WebBrowserNavigateComplete2;
  FInstance.Silent := True;
  WebControlUpdated;
end;

destructor TWindowsBrowserEngineIE.Destroy;
begin
  FInstance.Free;
  inherited;
end;

procedure TWindowsBrowserEngineIE.CreateHandle;
var
  Form: TCommonCustomForm;
  ZOrderManager: TWinZOrderManager;
begin
  if Service.WebControl <> nil then
  begin
    Form := TCommonCustomForm(Service.WebControl.Root);
    if Form <> nil then
    begin
      ZOrderManager := WindowHandleToPlatform(Form.Handle).ZOrderManager;
      ZOrderManager.AddOrSetLink(Service.WebControl, FOLEDelegate.WindowHandle, 0);
    end;
  end;
end;

procedure TWindowsBrowserEngineIE.DestroyHandle;
begin
  Service.RemoveFromZOrderManager;
end;

procedure TWindowsBrowserEngineIE.DoSetFocus;
begin
  FService.DoSetFocus;
end;

function TWindowsBrowserEngineIE.CaptureBitmap: TBitmap;

  function CreateBitmapFromWinBitmap(const AWBitMap: TWinBitmap): TBitmap;
  var
    BSurface: TBitmapSurface;
    I, J: Integer;
  begin
    Result := TBitmap.Create;
    BSurface := TBitmapSurface.Create;
    try
      BSurface.SetSize(AWBitMap.Width, AWBitMap.Height);
      for I := 0 to AWBitMap.Height - 1 do
        for J := 0 to AWBitMap.Width - 1 do
          BSurface.Pixels[J, I] := AWBitMap.Bits^[I * AWBitMap.Width + J].Color;
      Result.Assign(BSurface);
    finally
      BSurface.Free;
    end;
  end;

var
  ViewObject: IViewObject;
  SourceDrawRect: TRect;
  WBitmap: TWinBitmap;
  Document: IDispatch;
begin
  Result := nil;
  Document := FInstance.DefaultInterface.Document;
  if Supports(Document, IViewObject, ViewObject) then
  begin
    SourceDrawRect := Rect(0, 0, Trunc(Service.WebControl.Size.Width), Trunc(Service.WebControl.Size.Height));
    WBitmap := CreateWinBitmap(SourceDrawRect.Width, SourceDrawRect.Height);
    try
      ViewObject.Draw(DVASPECT_CONTENT, 1, nil, nil, FOLEDelegate.Handle, WBitmap.DC, @SourceDrawRect, nil, nil, 0);
      Result := CreateBitmapFromWinBitmap(WBitmap);
    finally
      DestroyWinBitmap(WBitmap);
    end;
  end;
end;

procedure TWindowsBrowserEngineIE.EvaluateJavaScript(const JavaScript: string);
var
  LDoc: IHTMLDocument2;
  LHTMLWindow: IHTMLWindow2;
begin
  LDoc := FInstance.DefaultInterface.Document as IHTMLDocument2;
  if Assigned(LDoc) then
  begin
    LHTMLWindow := LDoc.parentWindow;
    if Assigned(LHTMLWindow) then
      LHTMLWindow.execScript(JavaScript, 'JavaScript');
  end;
end;

function TWindowsBrowserEngineIE.GetCanGoBack: Boolean;
begin
  Result := FCanGoBack;
end;

function TWindowsBrowserEngineIE.GetCanGoForward: Boolean;
begin
  Result := FCanGoForward;
end;

function TWindowsBrowserEngineIE.GetHandle: HWND;
begin
  Result := FOLEDelegate.WindowHandle;
end;

function TWindowsBrowserEngineIE.GetParent: TFmxObject;
begin
  Result := FOLEDelegate.Parent;
end;

function TWindowsBrowserEngineIE.GetVisible: Boolean;
begin
  Result := FInstance.Visible;
end;

procedure TWindowsBrowserEngineIE.GoBack;
begin
  FInstance.GoBack;
end;

procedure TWindowsBrowserEngineIE.GoForward;
begin
  FInstance.GoForward;
end;

procedure TWindowsBrowserEngineIE.GoHome;
begin
  FInstance.GoHome;
end;

procedure TWindowsBrowserEngineIE.LoadDocumentFromStream(const AStream: TStringStream);
const
  CBlankPage: string = 'about:blank'; // do not localize
var
  PersistStreamInit: IPersistStreamInit;
  StreamAdapter: IStream;
begin
  if FInstance.DefaultInterface.Document = nil then
    FInstance.Navigate(CBlankPage);
  if Supports(FInstance.DefaultInterface.Document, IPersistStreamInit, PersistStreamInit) then
  begin
    if PersistStreamInit.InitNew = S_OK then
    begin
      StreamAdapter:= TStreamAdapter.Create(AStream);
      PersistStreamInit.Load(StreamAdapter);
    end;
  end;
end;

procedure TWindowsBrowserEngineIE.Navigate(const AURL: string; const AFlags: OleVariant);
begin
  FInstance.Navigate(AURL, AFlags);
end;

function TWindowsBrowserEngineIE.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := FInstance.ControlInterface.QueryInterface(IID, Obj);
end;

procedure TWindowsBrowserEngineIE.Reload;
begin
  FInstance.Refresh;
end;

procedure TWindowsBrowserEngineIE.SetVisible(const Value: Boolean);
begin
  FInstance.Visible := Value;
end;

procedure TWindowsBrowserEngineIE.Stop;
begin
  FInstance.Stop;
end;

procedure TWindowsBrowserEngineIE.WBCommandStateChange(Sender: TObject; Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK:
      FCanGoBack := Enable;
    CSC_NAVIGATEFORWARD:
      FCanGoForward := Enable;
  end;
end;

procedure TWindowsBrowserEngineIE.WebBrowserBeforeNavigate2(ASender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  Service.ShouldStartLoading(URL);
  Service.StartLoading(URL);
end;

procedure TWindowsBrowserEngineIE.WebBrowserNavigateComplete2(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
begin
  Service.FinishLoading(URL);
end;

procedure TWindowsBrowserEngineIE.WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch; const URL, Frame, StatusCode: OleVariant;
  var Cancel: WordBool);
begin
  Service.FailLoadingWithError;
end;

procedure TWindowsBrowserEngineIE.WebControlUpdated;
begin
  FInstance.Visible := (Service.WebControl <> nil) and Service.WebControl.Visible and Service.WebControl.ParentedVisible;
  FOLEDelegate.Parent := Service.WebControl.Parent;
end;

{ TWindowsBrowserEngineEdge }

constructor TWindowsBrowserEngineEdge.Create(const AService: TWindowsWebBrowserService);
begin
  inherited;
  FWinBrowserControl := TWindowsWebBrowser.Create(nil);
  FWinBrowserControl.OnCreateWebViewCompleted := EdgeCreateWebViewCompletedHandler;
  FWinBrowserControl.OnContentLoading := EdgeContentLoadingHandler;
  FWinBrowserControl.OnJavascriptEvaluated := EdgeJavascriptEvaluatedHandler;
  FWinBrowserControl.OnNavigationCompleted := EdgeNavigationCompletedHandler;
  FWinBrowserControl.OnNavigationStarting := EdgeNavigationStartingHandler;
  FWinBrowserControl.SetCustomBrowser(Service);
  FWinBrowserControl.Align := TAlignLayout.Client;
  FWinBrowserControl.ControlType := TControlType.Platform;
  WebControlUpdated;
end;

destructor TWindowsBrowserEngineEdge.Destroy;
begin
  FWinBrowserControl.Free;
  inherited;
end;

function TWindowsBrowserEngineEdge.CaptureBitmap: TBitmap;
begin
  Result := FWinBrowserControl.CaptureBitmap;
end;

procedure TWindowsBrowserEngineEdge.EdgeContentLoadingHandler(Sender: TObject; const ASuccess: Boolean; const ANavigationID: UInt64);
begin
  if ASuccess then
    Service.StartLoading(FURL);
end;

procedure TWindowsBrowserEngineEdge.EdgeCreateWebViewCompletedHandler(Sender: TObject; const ASuccess: Boolean);
var
  ActiveEngine: TWindowsActiveEngine;
begin
  if ASuccess then
    ActiveEngine := TWindowsActiveEngine.Edge
  else
    ActiveEngine := TWindowsActiveEngine.None;

  Service.SetWindowsActiveEngine(ActiveEngine);
end;

procedure TWindowsBrowserEngineEdge.EdgeJavascriptEvaluatedHandler(Sender: TObject; const AErrorCode: LongInt; const AResultObjectAsJSON: string);
begin
  // Not implemented
end;

procedure TWindowsBrowserEngineEdge.EdgeNavigationCompletedHandler(Sender: TObject; const ASuccess: Boolean);
begin
  Service.FinishLoading(FURL);
end;

procedure TWindowsBrowserEngineEdge.EdgeNavigationStartingHandler(Sender: TObject; const AURL: string);
begin
  FURL := AURL;
  Service.ShouldStartLoading(FURL);
end;

procedure TWindowsBrowserEngineEdge.EvaluateJavaScript(const JavaScript: string);
begin
  FWinBrowserControl.EvaluateJavascript(JavaScript);
end;

function TWindowsBrowserEngineEdge.GetCanGoBack: Boolean;
begin
  Result := FWinBrowserControl.CanGoBack;
end;

function TWindowsBrowserEngineEdge.GetCanGoForward: Boolean;
begin
  Result := FWinBrowserControl.CanGoForward;
end;

function TWindowsBrowserEngineEdge.GetHandle: HWND;
begin
  Result := FWinBrowserControl.NativePresentation.Handle;
end;

function TWindowsBrowserEngineEdge.GetParent: TFmxObject;
begin
  Result := FWinBrowserControl.GetParent;
end;

function TWindowsBrowserEngineEdge.GetVisible: Boolean;
begin
  Result := FWinBrowserControl.Visible;
end;

procedure TWindowsBrowserEngineEdge.GoBack;
begin
  FWinBrowserControl.NavigateTo(TBrowserNavigation.GoBack);
end;

procedure TWindowsBrowserEngineEdge.GoForward;
begin
  FWinBrowserControl.NavigateTo(TBrowserNavigation.GoForward);
end;

procedure TWindowsBrowserEngineEdge.GoHome;
begin
  FWinBrowserControl.NavigateTo(TBrowserNavigation.GoHome);
end;

procedure TWindowsBrowserEngineEdge.LoadDocumentFromStream(const AStream: TStringStream);
begin
  FWinBrowserControl.NavigateToString(AStream.DataString);
end;

procedure TWindowsBrowserEngineEdge.Navigate(const AURL: string; const AFlags: OleVariant);
begin
  FWinBrowserControl.NavigateURL(AURL);
end;

function TWindowsBrowserEngineEdge.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (FWinBrowserControl.NativePresentation <> nil) and (Result <> S_OK) then
    Result := FWinBrowserControl.NativePresentation.QueryInterface(IID, Obj);
end;

procedure TWindowsBrowserEngineEdge.Reload;
begin
  FWinBrowserControl.Refresh;
end;

procedure TWindowsBrowserEngineEdge.SetVisible(const Value: Boolean);
begin
  FWinBrowserControl.Visible := Value;
end;

procedure TWindowsBrowserEngineEdge.Stop;
begin
  FWinBrowserControl.Stop;
end;

procedure TWindowsBrowserEngineEdge.WebControlUpdated;
begin
  FWinBrowserControl.Parent := Service.WebControl;
end;

initialization
  TPresentationProxyFactory.Current.Register(TWindowsWebBrowser, TControlType.Platform, TWinPresentationProxy<TWinNativeWebBrowser>);

finalization
  TPresentationProxyFactory.Current.Unregister(TWindowsWebBrowser, TControlType.Platform, TWinPresentationProxy<TWinNativeWebBrowser>);

end.
