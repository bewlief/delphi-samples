{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.WebBrowser.Delegate.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.SysUtils, Macapi.ObjectiveC, Macapi.AppKit, Macapi.WebKit, Macapi.CocoaTypes, Macapi.Foundation,
  FMX.WebBrowser, FMX.Surfaces, FMX.WebBrowser.Delegate.Cocoa, FMX.Forms;

type
  /// <summary>Wrapper for WKWebView</summary>
  INativeWebView = WKWebView;
  /// <summary>Wrapper for NSView</summary>
  INativeView = NSView;
  /// <summary>Wrapper for WKWebView</summary>
  IMainFrame = WKWebView;
  /// <summary>Wrapper for NSURLRequestCachePolicy</summary>
  NativeCachePolicy = NSURLRequestCachePolicy;
  WebUIDelegate = WKUIDelegate;
  WebNavigationDelegate = WKNavigationDelegate;

  { TWebViewDelegate }
  /// <summary> Class delegate for reciving call backs from native webView</summary>
  TWebViewDelegate = class(TBaseWebViewDelegate, WKUIDelegate, WKNavigationDelegate)
  private
    FWebBrowser: TCustomWebBrowser;
    [unsafe] FURLSetter: ICustomBrowser;
  public
    { WKUIDelegate }
    [MethodName('webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:')]
    function webViewCreateWebViewWithConfigurationForNavigationActionWindowFeatures(webView: WKWebView;
      createWebViewWithConfiguration: WKWebViewConfiguration; forNavigationAction: WKNavigationAction;
      windowFeatures: WKWindowFeatures): WKWebView; cdecl;
    [MethodName('webView:runJavaScriptAlertPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrameCompletionHandler(webView: WKWebView;
      runJavaScriptAlertPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: Pointer); cdecl;
    [MethodName('webView:runJavaScriptConfirmPanelWithMessage:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrameCompletionHandler(webView: WKWebView;
      runJavaScriptConfirmPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo;
      completionHandler: Pointer); cdecl;
    [MethodName('webView:runJavaScriptTextInputPanelWithPrompt:defaultText:initiatedByFrame:completionHandler:')]
    procedure webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrameCompletionHandler
      (webView: WKWebView; runJavaScriptTextInputPanelWithPrompt: NSString; defaultText: NSString;
      initiatedByFrame: WKFrameInfo; completionHandler: Pointer); cdecl;
    { WKNavigationDelegate }
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationActionDecisionHandler(webView: WKWebView;
      decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationResponseDecisionHandler(webView: WKWebView;
      decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didStartProvisionalNavigation:')]
    procedure webViewDidStartProvisionalNavigation(webView: WKWebView;
      didStartProvisionalNavigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveServerRedirectForProvisionalNavigation:')]
    procedure webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
      didReceiveServerRedirectForProvisionalNavigation: WKNavigation); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisionalNavigationWithError(webView: WKWebView;
      didFailProvisionalNavigation: WKNavigation; withError: NSError); cdecl;
    [MethodName('webView:didCommitNavigation:')]
    procedure webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigationWithError(webView: WKWebView; didFailNavigation: WKNavigation;
      withError: NSError); cdecl;
    [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
    procedure webViewDidReceiveAuthenticationChallengeCompletionHandler(webView: WKWebView;
      didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge;
      completionHandler: Pointer); cdecl;
  public
    /// <summary>Setter for callback's receiver</summary>
    procedure SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
    /// <summary>Return a pointer to a native delegate</summary>
    constructor Create;
  end;

  /// <summary>The class for the automation of work with native webView.</summary>
  TNativeWebViewHelper = class
    /// <summary>Class function for init WKWebView with predefined params.</summary>
    class function CreateAndInitWebView: WKWebView;
    /// <summary>Class function for getting working frame.</summary>
    class function MainFrame(const ANativeWebView: INativeWebView): IMainFrame; inline;
    /// <summary> Class procedure for setting Bounds.</summary>
    class procedure SetBounds(const ANativeWebView: INativeWebView; const ABounds: TRectF; const AHeight: Single = 0);
    /// <summary> Class procedure for taking snapshot from web view.</summary>
    class function NativeViewToSurface(const ANativeView: INativeWebView; const ASurface: TBitmapSurface): Boolean;
  end;

implementation

uses
  System.Math, System.TypInfo, Macapi.ObjCRuntime, Macapi.Helpers, MacApi.CoreFoundation ,FMX.Helpers.Mac;

{ TWebViewDelegate }

constructor TWebViewDelegate.Create;
begin
  // In TOCLocal constructor placed in protected section.
  // For calling we should make new one and call inherited.
  inherited Create;
end;

procedure TWebViewDelegate.SetWebBrowser(const AWebBrowser: TCustomWebBrowser; const AURLSetter: ICustomBrowser);
begin
  FWebBrowser := AWebBrowser;
  FURLSetter := AURLSetter;
end;

function TWebViewDelegate.webViewCreateWebViewWithConfigurationForNavigationActionWindowFeatures(webView: WKWebView;
  createWebViewWithConfiguration: WKWebViewConfiguration; forNavigationAction: WKNavigationAction; windowFeatures: WKWindowFeatures): WKWebView;
begin
  if (forNavigationAction.targetFrame = nil) or not forNavigationAction.targetFrame.isMainFrame then
    webView.loadRequest(forNavigationAction.request);
  Result := nil;
end;

procedure TWebViewDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(webView: WKWebView;
  decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer);
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  if FWebBrowser <> nil then
  begin
    FURLSetter.SetURL(NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString));
    FWebBrowser.ShouldStartLoading(FWebBrowser.URL);
  end;
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(WKNavigationResponsePolicyAllow);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

procedure TWebViewDelegate.webViewDecidePolicyForNavigationResponseDecisionHandler(webView: WKWebView;
  decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer);
var
  LDecisionHandlerBlock: procedure(policy: WKNavigationResponsePolicy); cdecl;
begin
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(WKNavigationResponsePolicyAllow);
  imp_removeBlock(@LDecisionHandlerBlock);
end;

procedure TWebViewDelegate.webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation);
begin

end;

procedure TWebViewDelegate.webViewDidFailNavigationWithError(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError);
begin
  if FWebBrowser <> nil then
    FWebBrowser.FailLoadingWithError;
end;

procedure TWebViewDelegate.webViewDidFailProvisionalNavigationWithError(webView: WKWebView; didFailProvisionalNavigation: WKNavigation;
  withError: NSError);
begin

end;

procedure TWebViewDelegate.webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation);
begin
  if FURLSetter <> nil then
    FURLSetter.SetURL(NSStrToStr(webView.URL.absoluteString));
  if FWebBrowser <> nil then
    FWebBrowser.FinishLoading;
end;

procedure TWebViewDelegate.webViewDidReceiveAuthenticationChallengeCompletionHandler(webView: WKWebView;
  didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure(disposition: NSURLSessionAuthChallengeDisposition; ignored: Pointer; credential: Pointer); cdecl;
  LAuthMethod: NSString;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  LAuthMethod := didReceiveAuthenticationChallenge.protectionSpace.authenticationMethod;
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

procedure TWebViewDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
  didReceiveServerRedirectForProvisionalNavigation: WKNavigation);
begin
  if FWebBrowser <> nil then
    FURLSetter.SetURL(NSStrToStr(webView.URL.absoluteString));
end;

procedure TWebViewDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; didStartProvisionalNavigation: WKNavigation);
begin
  if FWebBrowser <> nil then
    FWebBrowser.StartLoading;
end;

procedure TWebViewDelegate.webViewRunJavaScriptAlertPanelWithMessageInitiatedByFrameCompletionHandler(webView: WKWebView;
   runJavaScriptAlertPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo; completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure; cdecl;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  JavaScriptInformationMessage(runJavaScriptAlertPanelWithMessage,
    procedure
    begin
      LCompletionHandlerBlock;
      imp_removeBlock(@LCompletionHandlerBlock);
    end
  );
end;

procedure TWebViewDelegate.webViewRunJavaScriptConfirmPanelWithMessageInitiatedByFrameCompletionHandler(webView: WKWebView;
   runJavaScriptConfirmPanelWithMessage: NSString; initiatedByFrame: WKFrameInfo; completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure(result: Boolean); cdecl;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  JavaScriptConfirmationMessage(runJavaScriptConfirmPanelWithMessage,
    procedure(const AResponse: Boolean)
    begin
      LCompletionHandlerBlock(AResponse);
      imp_removeBlock(@LCompletionHandlerBlock);
    end
  );
end;

procedure TWebViewDelegate.webViewRunJavaScriptTextInputPanelWithPromptDefaultTextInitiatedByFrameCompletionHandler(webView: WKWebView;
  runJavaScriptTextInputPanelWithPrompt: NSString; defaultText: NSString; initiatedByFrame: WKFrameInfo; completionHandler: Pointer);
var
  LCompletionHandlerBlock: procedure(result: Pointer); cdecl;
begin
  @LCompletionHandlerBlock := imp_implementationWithBlock(completionHandler);
  JavaScriptInputQuery(runJavaScriptTextInputPanelWithPrompt, defaultText,
    procedure(const AResponse: NSString)
    begin
      LCompletionHandlerBlock(NSObjectToID(AResponse));
      imp_removeBlock(@LCompletionHandlerBlock);
    end
  );
end;

type
  TMacWebViewScreenShoter = class
  private type
    TState = (Processing, Completed, Failed);
  private
    FState: TState;
    FWebView: INativeWebView;
    FSurface: TBitmapSurface;
    procedure CompleteBlock(param1: Pointer; error: NSError);
  public
    constructor Create(const ANativeView: INativeWebView);
    function TakeSnapshot(const ASurface: TBitmapSurface): Boolean;

    property Surface: TBitmapSurface read FSurface;
  end;

{ TNativeWebViewHelper }

class function TNativeWebViewHelper.CreateAndInitWebView: WKWebView;
begin
  Result := TWKWebView.Create;
  Result.setAutoresizingMask(NSViewMinYMargin);
end;

class function TNativeWebViewHelper.MainFrame(const ANativeWebView: INativeWebView): IMainFrame;
begin
  Result := ANativeWebView;
end;

class function TNativeWebViewHelper.NativeViewToSurface(const ANativeView: INativeWebView;
  const ASurface: TBitmapSurface): Boolean;
begin
  if TOSVersion.Check(10, 13) then
  begin
    var Snapshoter := TMacWebViewScreenShoter.Create(ANativeView);
    try
      Result := Snapshoter.TakeSnapshot(ASurface);
    finally
      Snapshoter.Free;
    end;
  end
  else
    Result := FMX.Helpers.Mac.NativeViewToSurface(ANativeView, ASurface);
end;

class procedure TNativeWebViewHelper.SetBounds(const ANativeWebView: INativeWebView; const ABounds: TRectF;
  const AHeight: Single);
begin
  ANativeWebView.setFrame(CGRectFromRect(TRectF.Create(ABounds.Left, AHeight - ABounds.Bottom, ABounds.Right,
    AHeight - ABounds.Top)));
end;

{ TMacWebViewScreenShoter }

procedure TMacWebViewScreenShoter.CompleteBlock(param1: Pointer; error: NSError);
begin
  Assert(FSurface <> nil);

  if error <> nil then
  begin
    FState := TState.Failed;
    Exit;
  end;

  try
    var Image := TNSImage.Wrap(param1);
    var ImageRef := Image.CGImageForProposedRect(nil, nil, nil);
    CGImageRefToSurface(ImageRef, FSurface);
    FState := TState.Completed;
  except on E: Exception do
    FState := TState.Failed;
  end;
end;

constructor TMacWebViewScreenShoter.Create(const ANativeView: INativeWebView);
begin
  FWebView := ANativeView;
  FState := TState.Completed;
end;

function TMacWebViewScreenShoter.TakeSnapshot(const ASurface: TBitmapSurface): Boolean;
begin
  FState := TState.Processing;
  FSurface := ASurface;
  var Config := TWKSnapshotConfiguration.Create;
  Config.setAfterScreenUpdates(True);
  Config.setRect(FWebView.frame);
  FWebView.takeSnapshotWithConfiguration(Config, CompleteBlock);

  // Async -> Sync
  while FState = TState.Processing do
    Application.ProcessMessages;

  Result := FState = TState.Completed;
end;

end.
