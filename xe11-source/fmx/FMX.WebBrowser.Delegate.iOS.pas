{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Delegate.iOS;

interface

{$SCOPEDENUMS ON}

uses
  Macapi.ObjectiveC, FMX.WebBrowser, FMX.WebBrowser.Delegate.Cocoa, iOSapi.UIKit, iOSApi.Foundation, System.Types, iOSapi.WebKit,
  FMX.Surfaces;

type
  WKUIDelegateSlim = interface(IObjectiveC)
    ['{A0FEDEBE-6023-4FBE-897A-9C005E83934B}']
    [MethodName('webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:')]
    function webViewCreateWebViewWithConfiguration(webView: WKWebView; configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction;
      windowFeatures: WKWindowFeatures): WKWebView; cdecl;
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptAlertPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
      completionHandler: Pointer); cdecl; // TWKUIDelegateBlockMethod1
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptConfirmPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
      completionHandler: Pointer); cdecl; // TWKUIDelegateBlockMethod2
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptTextInputPanelWithPrompt(webView: WKWebView; prompt: NSString; defaultText: NSString; frame: WKFrameInfo;
      completionHandler: Pointer); cdecl; // TWKUIDelegateBlockMethod3
  end;

  WKNavigationDelegateSlim = interface(IObjectiveC)
    ['{7FDDD993-8024-478B-930D-334AFEF0CA16}']
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
      decisionHandler: Pointer); overload; cdecl; // TWKNavigationDelegateBlockMethod1
    [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
      decisionHandler: Pointer); cdecl; // TWKNavigationDelegateBlockMethod3
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
    procedure webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
      completionHandler: Pointer); cdecl; // TWKNavigationDelegateBlockMethod4
    [MethodName('webView:didStartProvisionalNavigation:')]
    procedure webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
  end;

  /// <summary>Wrapper for UIWebView</summary>
  INativeWebView = WKWebView;
  /// <summary>Wrapper for UIView</summary>
  INativeView = UIView;
  /// <summary>Wrapper for UIWebView</summary>
  IMainFrame = WKWebView;
  /// <summary>Wrapper for NSURLRequestCachePolicy</summary>
  NativeCachePolicy = NSURLRequestCachePolicy;
  WebUIDelegate = WKUIDelegateSlim;
  WebNavigationDelegate = WKNavigationDelegateSlim;

{ TWebViewDelegate }
  /// <summary> Class delegate for reciving call backs from native UIWebView</summary>
  TWebViewDelegate = class(TBaseWebViewDelegate, WKUIDelegateSlim, WKNavigationDelegateSlim)
  private
    FWebBrowser: TCustomWebBrowser;
    [unsafe] FURLSetter: ICustomBrowser;
  public
    { WKUIDelegate }
    [MethodName('webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:')]
    function webViewCreateWebViewWithConfiguration(webView: WKWebView; configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction;
      windowFeatures: WKWindowFeatures): WKWebView; cdecl;
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptAlertPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
      completionHandler: Pointer); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptConfirmPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
      completionHandler: Pointer); cdecl;
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptTextInputPanelWithPrompt(webView: WKWebView; prompt: NSString; defaultText: NSString; frame: WKFrameInfo;
      completionHandler: Pointer); cdecl;
    { WKNavigationDelegate }
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
      decisionHandler: Pointer); overload; cdecl;
    [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
      decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
    procedure webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
      completionHandler: Pointer); cdecl;
    [MethodName('webView:didStartProvisionalNavigation:')]
    procedure webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
  public
    /// <summary>Return a pointer to a native delegate</summary>
    constructor Create;
    /// <summary>Setter for callback's receiver</summary>
    procedure SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
  end;

  /// <summary>The class for the automation of work with native UIWebView</summary>
  TNativeWebViewHelper = class
  public
    /// <summary>Class function for init UIWebView with predefined params</summary>
    class function CreateAndInitWebView: WKWebView;
    /// <summary>Class function for getting working frame</summary>
    class function MainFrame(const ANativeWebView: INativeWebView): IMainFrame; inline;
    /// <summary> Class procedure for setting Bounds/// </summary>
    class procedure SetBounds(const ANativeWebView: INativeWebView; const ABounds: TRectF; const AHeight: Single = 0);
    /// <summary> Class procedure for taking snapshot from web view.</summary>
    class function NativeViewToSurface(const ANativeView: INativeWebView; const ASurface: TBitmapSurface): Boolean;
  end;

implementation

uses
  Macapi.ObjCRuntime, Macapi.Helpers, iOSapi.CoreGraphics, FMX.Helpers.iOS;

{ TWebViewDelegate }

constructor TWebViewDelegate.Create;
begin
  inherited Create;
end;

procedure TWebViewDelegate.SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
begin
  FWebBrowser := AWebBrowser;
  FURLSetter := AURLSetter;
end;

function TWebViewDelegate.webViewCreateWebViewWithConfiguration(webView: WKWebView; configuration: WKWebViewConfiguration;
  navigationAction: WKNavigationAction; windowFeatures: WKWindowFeatures): WKWebView;
var
  Url: NSURL;
begin
  if navigationAction.targetFrame = nil then
  begin
    Url := navigationAction.request.URL;
    SharedApplication.openURL(Url);
  end
  else if not navigationAction.targetFrame.isMainFrame then
    webView.loadRequest(navigationAction.request);
  Result := nil;
end;

procedure TWebViewDelegate.webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
  decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
begin
  if FWebBrowser <> nil then
  begin
    FURLSetter.SetURL(NSStrToStr(navigationAction.request.URL.absoluteString));
    FWebBrowser.ShouldStartLoading(FWebBrowser.URL);
  end;
  @LBlockImp := imp_implementationWithBlock(decisionHandler);
  LBlockImp(WKNavigationActionPolicyAllow);
  imp_removeBlock(@LBlockImp);
end;

procedure TWebViewDelegate.webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
  decisionHandler: Pointer);
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(WKNavigationResponsePolicyAllow);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

procedure TWebViewDelegate.webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
begin
  if FWebBrowser <> nil then
    FWebBrowser.FailLoadingWithError;
end;

procedure TWebViewDelegate.webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
var
  LURLString: NSString;
  LURL: NSURL;
begin
  if error.domain.isEqualToString(NSURLErrorDomain) and (error.code = NSURLErrorUnsupportedURL) then
  begin
    LURLString := TNSString.Wrap(error.userInfo.objectForKey(NSObjectToID(NSURLErrorFailingURLStringErrorKey)));
    LURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(LURLString));
    SharedApplication.openURL(LURL);
  end
  else if error.code = NSURLErrorTimedOut then       
    FWebBrowser.FailLoadingWithError;
end;

procedure TWebViewDelegate.webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
  completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure(disposition: NSURLSessionAuthChallengeDisposition; ignored: Pointer; credential: Pointer); cdecl;
  LAuthMethod: NSString;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  LAuthMethod := challenge.protectionSpace.authenticationMethod;
  if LAuthMethod.isEqualToString(NSURLAuthenticationMethodDefault) or LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPBasic) or
    LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPDigest) then
  begin
    AuthenticateForHost(webView.URL.host,
      procedure(const ACredential: NSURLCredential)
      begin
        LCompletionHandlerBlock(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(ACredential));
        imp_removeBlock(@LCompletionHandlerBlock);
      end
    );
  end
  else if LAuthMethod.isEqualToString(NSURLAuthenticationMethodServerTrust) then
  begin
    LCompletionHandlerBlock(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil);
    imp_removeBlock(@LCompletionHandlerBlock);
  end
  else
  begin
    LCompletionHandlerBlock(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
    imp_removeBlock(@LCompletionHandlerBlock);
  end;
end;

procedure TWebViewDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  if FWebBrowser <> nil then
    FWebBrowser.StartLoading;
end;

procedure TWebViewDelegate.webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  if FURLSetter <> nil then
    FURLSetter.SetURL(NSStrToStr(webView.URL.absoluteString));
  if FWebBrowser <> nil then
    FWebBrowser.FinishLoading;
end;

procedure TWebViewDelegate.webViewRunJavaScriptAlertPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
  completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure; cdecl;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  JavaScriptInformationMessage(message,
    procedure
    begin
      LCompletionHandlerBlock;
      imp_removeBlock(@LCompletionHandlerBlock);
    end
  );
end;

procedure TWebViewDelegate.webViewRunJavaScriptConfirmPanelWithMessage(webView: WKWebView; message: NSString; frame: WKFrameInfo;
  completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure(result: Boolean); cdecl;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  JavaScriptConfirmationMessage(message,
    procedure(const AResponse: Boolean)
    begin
      LCompletionHandlerBlock(AResponse);
      imp_removeBlock(@LCompletionHandlerBlock);
    end
  );
end;

procedure TWebViewDelegate.webViewRunJavaScriptTextInputPanelWithPrompt(webView: WKWebView; prompt: NSString; defaultText: NSString; frame: WKFrameInfo;
  completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure(result: Pointer); cdecl;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  JavaScriptInputQuery(prompt, defaultText,
    procedure(const AResponse: NSString)
    begin
      LCompletionHandlerBlock(NSObjectToID(AResponse));
      imp_removeBlock(@LCompletionHandlerBlock);
    end
  );
end;

{ TNativeWebViewHelper }

class function TNativeWebViewHelper.CreateAndInitWebView: WKWebView;
var
  LConfiguration: WKWebViewConfiguration;
begin
  LConfiguration := TWKWebViewConfiguration.Create;
  LConfiguration.setAllowsInlineMediaPlayback(True);
  Result := TWKWebView.Wrap(TWKWebView.Alloc.initWithFrame(CGRectMake(0, 0, 0, 0), LConfiguration));
end;

class function TNativeWebViewHelper.MainFrame(const ANativeWebView: INativeWebView): IMainFrame;
begin
  Result := ANativeWebView;
end;

class function TNativeWebViewHelper.NativeViewToSurface(const ANativeView: INativeWebView;
  const ASurface: TBitmapSurface): Boolean;
begin
  Result := FMX.Helpers.iOS.NativeViewToSurface(ANativeView, ASurface);
end;

class procedure TNativeWebViewHelper.SetBounds(const ANativeWebView: INativeWebView; const ABounds: TRectF;
  const AHeight: Single);
begin
  ANativeWebView.setFrame(CGRectFromRect(TRectF.Create(ABounds.Left, ABounds.Bottom, ABounds.Right, ABounds.Top)));
end;

end.
