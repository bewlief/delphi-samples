unit iOSapi.WebKit;

{*******************************************************}
{                                                       }
{            CodeGear Delphi Runtime Library            }
{                                                       }
{ Copyright(c) 2010-2022 Embarcadero Technologies, Inc. }
{                  All rights reserved                  }
{                                                       }
{*******************************************************}

interface

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics;

const
  WKErrorUnknown = 1;
  WKErrorWebContentProcessTerminated = 2;
  WKErrorWebViewInvalidated = 3;
  WKErrorJavaScriptExceptionOccurred = 4;
  WKErrorJavaScriptResultTypeIsUnsupported = 5;
  WKErrorContentRuleListStoreCompileFailed = 6;
  WKErrorContentRuleListStoreLookUpFailed = 7;
  WKErrorContentRuleListStoreRemoveFailed = 8;
  WKErrorContentRuleListStoreVersionMismatch = 9;
  WKErrorAttributedStringContentFailedToLoad = 10;
  WKErrorAttributedStringContentLoadTimedOut = 11;
  WKContentModeRecommended = 0;
  WKContentModeMobile = 1;
  WKContentModeDesktop = 2;
  WKNavigationTypeLinkActivated = 0;
  WKNavigationTypeFormSubmitted = 1;
  WKNavigationTypeBackForward = 2;
  WKNavigationTypeReload = 3;
  WKNavigationTypeFormResubmitted = 4;
  WKNavigationTypeOther = -1;
  WKNavigationActionPolicyCancel = 0;
  WKNavigationActionPolicyAllow = 1;
  WKNavigationResponsePolicyCancel = 0;
  WKNavigationResponsePolicyAllow = 1;
  WKUserScriptInjectionTimeAtDocumentStart = 0;
  WKUserScriptInjectionTimeAtDocumentEnd = 1;
  WKDataDetectorTypeNone = 0;
  WKDataDetectorTypePhoneNumber = 1;
  WKDataDetectorTypeLink = 2;
  WKDataDetectorTypeAddress = 4;
  WKDataDetectorTypeCalendarEvent = 8;
  WKDataDetectorTypeTrackingNumber = 16;
  WKDataDetectorTypeFlightNumber = 32;
  WKDataDetectorTypeLookupSuggestion = 64;
  WKDataDetectorTypeAll = -1;
  WKDataDetectorTypeSpotlightSuggestion = WKDataDetectorTypeLookupSuggestion;
  WKSelectionGranularityDynamic = 0;
  WKSelectionGranularityCharacter = 1;
  WKAudiovisualMediaTypeNone = 0;
  WKAudiovisualMediaTypeAudio = 1;
  WKAudiovisualMediaTypeVideo = 2;
  WKAudiovisualMediaTypeAll = -1;

type
  WKBackForwardListItem = interface;
  WKBackForwardList = interface;
  WKContentRuleList = interface;
  WKContentRuleListStore = interface;
  WKContextMenuElementInfo = interface;
  WKSecurityOrigin = interface;
  WKWebView = interface;
  WKFrameInfo = interface;
  WKHTTPCookieStoreObserver = interface;
  WKHTTPCookieStore = interface;
  WKWebpagePreferences = interface;
  WKNavigation = interface;
  WKNavigationAction = interface;
  WKNavigationDelegate = interface;
  WKNavigationResponse = interface;
  WKPreferences = interface;
  WKPreviewActionItem = interface;
  WKPreviewElementInfo = interface;
  WKProcessPool = interface;
  WKScriptMessage = interface;
  WKScriptMessageHandler = interface;
  WKSnapshotConfiguration = interface;
  WKUIDelegate = interface;
  WKURLSchemeHandler = interface;
  WKURLSchemeTask = interface;
  WKUserContentController = interface;
  WKUserScript = interface;
  WKWebViewConfiguration = interface;
  WKWebsiteDataRecord = interface;
  WKWebsiteDataStore = interface;
  WKWindowFeatures = interface;

  NSAttributedStringCompletionHandler = procedure(p1: NSAttributedString; p2: NSDictionary; p3: NSError) of object;
  WKErrorCode = NSInteger;
  WKContentMode = NSInteger;
  WKNavigationType = NSInteger;
  WKNavigationActionPolicy = NSInteger;
  WKNavigationResponsePolicy = NSInteger;
  WKUserScriptInjectionTime = NSInteger;
  WKDataDetectorTypes = NSInteger;
  WKSelectionGranularity = NSInteger;
  WKAudiovisualMediaTypes = NSInteger;
  TWKContentRuleListStoreBlockMethod1 = procedure(param1: WKContentRuleList; param2: NSError) of object;
  TWKContentRuleListStoreBlockMethod2 = procedure(param1: NSError) of object;
  TWKContentRuleListStoreBlockMethod3 = procedure(param1: NSArray) of object;
  TWKWebViewBlockMethod1 = procedure(param1: Pointer; error: NSError) of object;
  TWKWebViewBlockMethod2 = procedure(snapshotImage: UIImage; error: NSError) of object;
  TWKHTTPCookieStoreBlockMethod1 = procedure(param1: NSArray) of object;
  TWKHTTPCookieStoreBlockMethod2 = procedure of object;
  TWKNavigationDelegateBlockMethod1 = procedure(param1: WKNavigationActionPolicy) of object;
  TWKNavigationDelegateBlockMethod2 = procedure(param1: WKNavigationActionPolicy; param2: WKWebpagePreferences) of object;
  TWKNavigationDelegateBlockMethod3 = procedure(param1: WKNavigationResponsePolicy) of object;
  TWKNavigationDelegateBlockMethod4 = procedure(disposition: NSURLSessionAuthChallengeDisposition; credential: NSURLCredential) of object;
  TWKUIDelegateBlockMethod1 = procedure of object;
  TWKUIDelegateBlockMethod2 = procedure(result: Boolean) of object;
  TWKUIDelegateBlockMethod3 = procedure(result: NSString) of object;
  TWKUIDelegateBlockMethod4 = procedure(configuration: UIContextMenuConfiguration) of object;
  TWKWebsiteDataStoreBlockMethod1 = procedure(param1: NSArray) of object;
  TWKWebsiteDataStoreBlockMethod2 = procedure of object;

  WKBackForwardListItemClass = interface(NSObjectClass)
    ['{4E7E1674-03C5-4DF8-8E68-96C2786F6ED4}']
  end;

  WKBackForwardListItem = interface(NSObject)
    ['{02FB445F-863F-43FA-9588-058F7FFA850D}']
    function initialURL: NSURL; cdecl;
    function title: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TWKBackForwardListItem = class(TOCGenericImport<WKBackForwardListItemClass, WKBackForwardListItem>) end;

  WKBackForwardListClass = interface(NSObjectClass)
    ['{EA218791-FF4C-4E0C-9468-EB4CA5F843C4}']
  end;

  WKBackForwardList = interface(NSObject)
    ['{945C8CD5-A5B9-4DB3-8B70-9D45AE928E68}']
    function backItem: WKBackForwardListItem; cdecl;
    function backList: NSArray; cdecl;
    function currentItem: WKBackForwardListItem; cdecl;
    function forwardItem: WKBackForwardListItem; cdecl;
    function forwardList: NSArray; cdecl;
    function itemAtIndex(index: NSInteger): WKBackForwardListItem; cdecl;
  end;
  TWKBackForwardList = class(TOCGenericImport<WKBackForwardListClass, WKBackForwardList>) end;

  WKContentRuleListClass = interface(NSObjectClass)
    ['{4864FEDC-76EF-4D21-9D24-A4D467E2F708}']
  end;

  WKContentRuleList = interface(NSObject)
    ['{4B241548-40D1-4312-B188-88C837F0FBFE}']
    function identifier: NSString; cdecl;
  end;
  TWKContentRuleList = class(TOCGenericImport<WKContentRuleListClass, WKContentRuleList>) end;

  WKContentRuleListStoreClass = interface(NSObjectClass)
    ['{14CE6524-F8D7-416A-8F57-77B185902AD4}']
    {class} function defaultStore: Pointer; cdecl;
    {class} function storeWithURL(url: NSURL): Pointer; cdecl;
  end;

  WKContentRuleListStore = interface(NSObject)
    ['{F02D8647-A72D-4760-BF46-67810D17E10A}']
    [MethodName('compileContentRuleListForIdentifier:encodedContentRuleList:completionHandler:')]
    procedure compileContentRuleListForIdentifier(identifier: NSString; encodedContentRuleList: NSString; completionHandler: TWKContentRuleListStoreBlockMethod1); cdecl;
    procedure getAvailableContentRuleListIdentifiers(completionHandler: TWKContentRuleListStoreBlockMethod3); cdecl;
    [MethodName('lookUpContentRuleListForIdentifier:completionHandler:')]
    procedure lookUpContentRuleListForIdentifier(identifier: NSString; completionHandler: TWKContentRuleListStoreBlockMethod1); cdecl;
    [MethodName('removeContentRuleListForIdentifier:completionHandler:')]
    procedure removeContentRuleListForIdentifier(identifier: NSString; completionHandler: TWKContentRuleListStoreBlockMethod2); cdecl;
  end;
  TWKContentRuleListStore = class(TOCGenericImport<WKContentRuleListStoreClass, WKContentRuleListStore>) end;

  WKContextMenuElementInfoClass = interface(NSObjectClass)
    ['{40D0C71A-041C-4074-A05A-5C655F0558A7}']
  end;

  WKContextMenuElementInfo = interface(NSObject)
    ['{5AAADDF5-76E1-409E-9B55-475EFC16E28A}']
    function linkURL: NSURL; cdecl;
  end;
  TWKContextMenuElementInfo = class(TOCGenericImport<WKContextMenuElementInfoClass, WKContextMenuElementInfo>) end;

  WKSecurityOriginClass = interface(NSObjectClass)
    ['{81DB91C9-EAD4-4961-889F-0FC90F174AD3}']
  end;

  WKSecurityOrigin = interface(NSObject)
    ['{8B8FA506-19D2-41ED-B5BF-05E113231FE0}']
    function host: NSString; cdecl;
    function port: NSInteger; cdecl;
    function protocol: NSString; cdecl;
  end;
  TWKSecurityOrigin = class(TOCGenericImport<WKSecurityOriginClass, WKSecurityOrigin>) end;

  WKWebViewClass = interface(UIViewClass)
    ['{4AD37690-DD68-41D1-AA84-DAB6144162EF}']
    {class} function handlesURLScheme(urlScheme: NSString): Boolean; cdecl;
  end;

  WKWebView = interface(UIView)
    ['{81680B75-DC3C-4AD6-807B-51B5C5E78EAF}']
    function allowsBackForwardNavigationGestures: Boolean; cdecl;
    function allowsLinkPreview: Boolean; cdecl;
    function backForwardList: WKBackForwardList; cdecl;
    function canGoBack: Boolean; cdecl;
    function canGoForward: Boolean; cdecl;
    function certificateChain: NSArray; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("serverTrust", macos(10.11, 10.12), ios(9.0, 10.0))
    function configuration: WKWebViewConfiguration; cdecl;
    function customUserAgent: NSString; cdecl;
    function estimatedProgress: Double; cdecl;
    [MethodName('evaluateJavaScript:completionHandler:')]
    procedure evaluateJavaScript(javaScriptString: NSString; completionHandler: TWKWebViewBlockMethod1); cdecl;
    function goBack: WKNavigation; cdecl;
    function goForward: WKNavigation; cdecl;
    function goToBackForwardListItem(item: WKBackForwardListItem): WKNavigation; cdecl;
    function hasOnlySecureContent: Boolean; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    [MethodName('initWithFrame:configuration:')]
    function initWithFrame(frame: CGRect; configuration: WKWebViewConfiguration): Pointer; cdecl;
    function isLoading: Boolean; cdecl;
    [MethodName('loadData:MIMEType:characterEncodingName:baseURL:')]
    function loadData(data: NSData; MIMEType: NSString; characterEncodingName: NSString; baseURL: NSURL): WKNavigation; cdecl;
    [MethodName('loadFileURL:allowingReadAccessToURL:')]
    function loadFileURL(URL: NSURL; readAccessURL: NSURL): WKNavigation; cdecl;
    [MethodName('loadHTMLString:baseURL:')]
    function loadHTMLString(&string: NSString; baseURL: NSURL): WKNavigation; cdecl;
    function loadRequest(request: NSURLRequest): WKNavigation; cdecl;
    function navigationDelegate: Pointer; cdecl;
    function reload: WKNavigation; cdecl;
    function reloadFromOrigin: WKNavigation; cdecl;
    function scrollView: UIScrollView; cdecl;
    function serverTrust: SecTrustRef; cdecl;
    procedure setAllowsBackForwardNavigationGestures(allowsBackForwardNavigationGestures: Boolean); cdecl;
    procedure setAllowsLinkPreview(allowsLinkPreview: Boolean); cdecl;
    procedure setCustomUserAgent(customUserAgent: NSString); cdecl;
    procedure setNavigationDelegate(navigationDelegate: Pointer); cdecl;
    procedure setUIDelegate(UIDelegate: Pointer); cdecl;
    procedure stopLoading; cdecl;
    [MethodName('takeSnapshotWithConfiguration:completionHandler:')]
    procedure takeSnapshotWithConfiguration(snapshotConfiguration: WKSnapshotConfiguration; completionHandler: TWKWebViewBlockMethod2); cdecl;
    function title: NSString; cdecl;
    function UIDelegate: Pointer; cdecl;
    function URL: NSURL; cdecl;
  end;
  TWKWebView = class(TOCGenericImport<WKWebViewClass, WKWebView>) end;

  WKFrameInfoClass = interface(NSObjectClass)
    ['{90F87597-298D-40D3-84A2-4A9578463B54}']
  end;

  WKFrameInfo = interface(NSObject)
    ['{225153D1-FF3E-4FFF-B184-68765D21AD85}']
    function isMainFrame: Boolean; cdecl;
    function request: NSURLRequest; cdecl;
    function securityOrigin: WKSecurityOrigin; cdecl;
    function webView: WKWebView; cdecl;
  end;
  TWKFrameInfo = class(TOCGenericImport<WKFrameInfoClass, WKFrameInfo>) end;

  WKHTTPCookieStoreObserver = interface(IObjectiveC)
    ['{B9B076DE-F993-4C2A-8D68-F3596C279931}']
    procedure cookiesDidChangeInCookieStore(cookieStore: WKHTTPCookieStore); cdecl;
  end;

  WKHTTPCookieStoreClass = interface(NSObjectClass)
    ['{D5AAC178-DFD1-41D6-86AF-787F58D9A004}']
  end;

  WKHTTPCookieStore = interface(NSObject)
    ['{5A2E9297-6D69-4894-9C60-8173B4096770}']
    procedure addObserver(observer: Pointer); cdecl;
    [MethodName('deleteCookie:completionHandler:')]
    procedure deleteCookie(cookie: NSHTTPCookie; completionHandler: TWKHTTPCookieStoreBlockMethod2); cdecl;
    procedure getAllCookies(completionHandler: TWKHTTPCookieStoreBlockMethod1); cdecl;
    procedure removeObserver(observer: Pointer); cdecl;
    [MethodName('setCookie:completionHandler:')]
    procedure setCookie(cookie: NSHTTPCookie; completionHandler: TWKHTTPCookieStoreBlockMethod2); cdecl;
  end;
  TWKHTTPCookieStore = class(TOCGenericImport<WKHTTPCookieStoreClass, WKHTTPCookieStore>) end;

  WKWebpagePreferencesClass = interface(NSObjectClass)
    ['{4197EBF6-4B05-4FEB-B21A-B2A00FA36ADB}']
  end;

  WKWebpagePreferences = interface(NSObject)
    ['{D0D110E0-5B3D-4B3C-97CE-4DE8FEE261DD}']
    function preferredContentMode: WKContentMode; cdecl;
    procedure setPreferredContentMode(preferredContentMode: WKContentMode); cdecl;
  end;
  TWKWebpagePreferences = class(TOCGenericImport<WKWebpagePreferencesClass, WKWebpagePreferences>) end;

  WKNavigationClass = interface(NSObjectClass)
    ['{BD59C6E8-0DA3-43F4-B4E8-49AD317DE9D9}']
  end;

  WKNavigation = interface(NSObject)
    ['{EA8A5456-55AA-4401-BA03-EC1978F9B276}']
    function effectiveContentMode: WKContentMode; cdecl;
  end;
  TWKNavigation = class(TOCGenericImport<WKNavigationClass, WKNavigation>) end;

  WKNavigationActionClass = interface(NSObjectClass)
    ['{C888C99B-D534-4C5B-BEAE-6C74B61AD87C}']
  end;

  WKNavigationAction = interface(NSObject)
    ['{DC43CB26-69A8-4044-869F-F3A33CA5875B}']
    function navigationType: WKNavigationType; cdecl;
    function request: NSURLRequest; cdecl;
    function sourceFrame: WKFrameInfo; cdecl;
    function targetFrame: WKFrameInfo; cdecl;
  end;
  TWKNavigationAction = class(TOCGenericImport<WKNavigationActionClass, WKNavigationAction>) end;

  WKNavigationDelegate = interface(IObjectiveC)
    ['{FD874B6D-A9C0-4105-AB0D-56F12293565F}']
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
      decisionHandler: Pointer); overload; cdecl; // TWKNavigationDelegateBlockMethod1
    [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
      decisionHandler: Pointer); cdecl; // TWKNavigationDelegateBlockMethod3
    [MethodName('webView:didCommitNavigation:')]
    procedure webViewDidCommitNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
    procedure webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
      completionHandler: Pointer); cdecl; // TWKNavigationDelegateBlockMethod4
    [MethodName('webView:didReceiveServerRedirectForProvisionalNavigation:')]
    procedure webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    [MethodName('webView:didStartProvisionalNavigation:')]
    procedure webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    procedure webViewWebContentProcessDidTerminate(webView: WKWebView); cdecl;
  end;

  WKNavigationResponseClass = interface(NSObjectClass)
    ['{3CBCE722-E1C3-43DE-9CDF-57C295AE041A}']
  end;

  WKNavigationResponse = interface(NSObject)
    ['{7216894F-FAEB-459F-BD67-C53189FC818D}']
    function canShowMIMEType: Boolean; cdecl;
    function isForMainFrame: Boolean; cdecl;
    function response: NSURLResponse; cdecl;
  end;
  TWKNavigationResponse = class(TOCGenericImport<WKNavigationResponseClass, WKNavigationResponse>) end;

  WKPreferencesClass = interface(NSObjectClass)
    ['{B5F711CB-AC02-4CF0-94F7-02A6F978B2F6}']
  end;

  WKPreferences = interface(NSObject)
    ['{996436E3-C8E2-46AF-A1A6-D4BA4AAEAD47}']
    function isFraudulentWebsiteWarningEnabled: Boolean; cdecl;
    function javaEnabled: Boolean; cdecl; // API_DEPRECATED("Java is no longer supported", macos(10.10, 10.15))
    function javaScriptCanOpenWindowsAutomatically: Boolean; cdecl;
    function javaScriptEnabled: Boolean; cdecl;
    function minimumFontSize: CGFloat; cdecl;
    function plugInsEnabled: Boolean; cdecl; // API_DEPRECATED("Plug-ins are no longer supported", macos(10.10, 10.15))
    procedure setFraudulentWebsiteWarningEnabled(fraudulentWebsiteWarningEnabled: Boolean); cdecl;
    procedure setJavaEnabled(javaEnabled: Boolean); cdecl; // API_DEPRECATED("Java is no longer supported", macos(10.10, 10.15))
    procedure setJavaScriptCanOpenWindowsAutomatically(javaScriptCanOpenWindowsAutomatically: Boolean); cdecl;
    procedure setJavaScriptEnabled(javaScriptEnabled: Boolean); cdecl;
    procedure setMinimumFontSize(minimumFontSize: CGFloat); cdecl;
    procedure setPlugInsEnabled(plugInsEnabled: Boolean); cdecl; // API_DEPRECATED("Plug-ins are no longer supported", macos(10.10, 10.15))
  end;
  TWKPreferences = class(TOCGenericImport<WKPreferencesClass, WKPreferences>) end;

  WKPreviewActionItem = interface(IObjectiveC)
    ['{FA424EAB-B3FA-4B89-915B-BF81829A2296}']
    function identifier: NSString; cdecl;
  end;

  WKPreviewElementInfoClass = interface(NSObjectClass)
    ['{8E3B5E08-7A55-445F-92BC-DC572739C542}']
  end;

  WKPreviewElementInfo = interface(NSObject)
    ['{FF56ED61-2EEF-4C71-A17E-32465AA87E11}']
    function linkURL: NSURL; cdecl;
  end;
  TWKPreviewElementInfo = class(TOCGenericImport<WKPreviewElementInfoClass, WKPreviewElementInfo>) end;

  WKProcessPoolClass = interface(NSObjectClass)
    ['{0B25E984-01B3-4D53-BAF0-EA2A21393E0B}']
  end;

  WKProcessPool = interface(NSObject)
    ['{A45D6127-FAD0-4EA4-B4D7-6B7E54004909}']
  end;
  TWKProcessPool = class(TOCGenericImport<WKProcessPoolClass, WKProcessPool>) end;

  WKScriptMessageClass = interface(NSObjectClass)
    ['{943E04FF-5A12-4291-B874-095C6B10D756}']
  end;

  WKScriptMessage = interface(NSObject)
    ['{E44ECD5C-93CA-4F4B-AE6B-5DD4EBAD4E35}']
    function body: Pointer; cdecl;
    function frameInfo: WKFrameInfo; cdecl;
    function name: NSString; cdecl;
    function webView: WKWebView; cdecl;
  end;
  TWKScriptMessage = class(TOCGenericImport<WKScriptMessageClass, WKScriptMessage>) end;

  WKScriptMessageHandler = interface(IObjectiveC)
    ['{093162DF-FC23-4C20-B36A-C0AFC7DEDF18}']
    [MethodName('userContentController:didReceiveScriptMessage:')]
    procedure userContentController(userContentController: WKUserContentController; message: WKScriptMessage); cdecl;
  end;

  WKSnapshotConfigurationClass = interface(NSObjectClass)
    ['{A5EC3C86-A335-455A-90CF-596CC4471A3A}']
  end;

  WKSnapshotConfiguration = interface(NSObject)
    ['{FAD098B9-6FAD-4A36-9131-C2E9AC7D3E8D}']
    function afterScreenUpdates: Boolean; cdecl;
    function rect: CGRect; cdecl;
    procedure setAfterScreenUpdates(afterScreenUpdates: Boolean); cdecl;
    procedure setRect(rect: CGRect); cdecl;
    procedure setSnapshotWidth(snapshotWidth: NSNumber); cdecl;
    function snapshotWidth: NSNumber; cdecl;
  end;
  TWKSnapshotConfiguration = class(TOCGenericImport<WKSnapshotConfigurationClass, WKSnapshotConfiguration>) end;

  WKUIDelegate = interface(IObjectiveC)
    ['{6F7C5692-ED85-4A65-A24D-BEB14164553D}']
    [MethodName('webView:contextMenuConfigurationForElement:completionHandler:')]
    procedure webViewContextMenuConfigurationForElement(webView: WKWebView; elementInfo: WKContextMenuElementInfo;
      completionHandler: Pointer); cdecl; // TWKUIDelegateBlockMethod4
    [MethodName('webView:contextMenuDidEndForElement:')]
    procedure webViewContextMenuDidEndForElement(webView: WKWebView; elementInfo: WKContextMenuElementInfo); cdecl;
    [MethodName('webView:contextMenuForElement:willCommitWithAnimator:')]
    procedure webViewContextMenuForElement(webView: WKWebView; elementInfo: WKContextMenuElementInfo; animator: Pointer); cdecl;
    [MethodName('webView:contextMenuWillPresentForElement:')]
    procedure webViewContextMenuWillPresentForElement(webView: WKWebView; elementInfo: WKContextMenuElementInfo); cdecl;
    [MethodName('webView:createWebViewWithConfiguration:forNavigationAction:windowFeatures:')]
    function webViewCreateWebViewWithConfiguration(webView: WKWebView; configuration: WKWebViewConfiguration; navigationAction: WKNavigationAction;
      windowFeatures: WKWindowFeatures): WKWebView; cdecl;
    procedure webViewDidClose(webView: WKWebView); cdecl;
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

  WKURLSchemeHandler = interface(IObjectiveC)
    ['{2DE0DDFD-D6B7-4DDA-935D-22622C733970}']
    [MethodName('webView:startURLSchemeTask:')]
    procedure webViewStartURLSchemeTask(webView: WKWebView; urlSchemeTask: Pointer); cdecl;
    [MethodName('webView:stopURLSchemeTask:')]
    procedure webViewStopURLSchemeTask(webView: WKWebView; urlSchemeTask: Pointer); cdecl;
  end;

  WKURLSchemeTask = interface(IObjectiveC)
    ['{A33B9673-DBF8-4D2A-B727-617F7973C44A}']
    procedure didFailWithError(error: NSError); cdecl;
    procedure didFinish; cdecl;
    procedure didReceiveData(data: NSData); cdecl;
    procedure didReceiveResponse(response: NSURLResponse); cdecl;
    function request: NSURLRequest; cdecl;
  end;

  WKUserContentControllerClass = interface(NSObjectClass)
    ['{C33C73CC-6567-429D-A83F-4E85DE933023}']
  end;

  WKUserContentController = interface(NSObject)
    ['{A27AFE3D-0935-461F-8A5C-03ABA440C09C}']
    procedure addContentRuleList(contentRuleList: WKContentRuleList); cdecl;
    [MethodName('addScriptMessageHandler:name:')]
    procedure addScriptMessageHandler(scriptMessageHandler: Pointer; name: NSString); cdecl;
    procedure addUserScript(userScript: WKUserScript); cdecl;
    procedure removeAllContentRuleLists; cdecl;
    procedure removeAllUserScripts; cdecl;
    procedure removeContentRuleList(contentRuleList: WKContentRuleList); cdecl;
    procedure removeScriptMessageHandlerForName(name: NSString); cdecl;
    function userScripts: NSArray; cdecl;
  end;
  TWKUserContentController = class(TOCGenericImport<WKUserContentControllerClass, WKUserContentController>) end;

  WKUserScriptClass = interface(NSObjectClass)
    ['{B53EFEFB-377C-4405-8C12-DCDA4B8DF4D7}']
  end;

  WKUserScript = interface(NSObject)
    ['{5F564C8E-B1A4-482A-BB95-B61E26DA6C9A}']
    [MethodName('initWithSource:injectionTime:forMainFrameOnly:')]
    function initWithSource(source: NSString; injectionTime: WKUserScriptInjectionTime; forMainFrameOnly: Boolean): Pointer; cdecl;
    function injectionTime: WKUserScriptInjectionTime; cdecl;
    function isForMainFrameOnly: Boolean; cdecl;
    function source: NSString; cdecl;
  end;
  TWKUserScript = class(TOCGenericImport<WKUserScriptClass, WKUserScript>) end;

  WKWebViewConfigurationClass = interface(NSObjectClass)
    ['{EA300DF9-D9B1-41A5-8318-AD953F932ABF}']
  end;

  WKWebViewConfiguration = interface(NSObject)
    ['{CB2BBB9B-0862-4D35-9FBE-5BB738F3DB40}']
    function allowsAirPlayForMediaPlayback: Boolean; cdecl;
    function allowsInlineMediaPlayback: Boolean; cdecl;
    function allowsPictureInPictureMediaPlayback: Boolean; cdecl;
    function applicationNameForUserAgent: NSString; cdecl;
    function dataDetectorTypes: WKDataDetectorTypes; cdecl;
    function defaultWebpagePreferences: WKWebpagePreferences; cdecl;
    function ignoresViewportScaleLimits: Boolean; cdecl;
    function mediaTypesRequiringUserActionForPlayback: WKAudiovisualMediaTypes; cdecl;
    function preferences: WKPreferences; cdecl;
    function processPool: WKProcessPool; cdecl;
    function selectionGranularity: WKSelectionGranularity; cdecl;
    procedure setAllowsAirPlayForMediaPlayback(allowsAirPlayForMediaPlayback: Boolean); cdecl;
    procedure setAllowsInlineMediaPlayback(allowsInlineMediaPlayback: Boolean); cdecl;
    procedure setAllowsPictureInPictureMediaPlayback(allowsPictureInPictureMediaPlayback: Boolean); cdecl;
    procedure setApplicationNameForUserAgent(applicationNameForUserAgent: NSString); cdecl;
    procedure setDataDetectorTypes(dataDetectorTypes: WKDataDetectorTypes); cdecl;
    procedure setDefaultWebpagePreferences(defaultWebpagePreferences: WKWebpagePreferences); cdecl;
    procedure setIgnoresViewportScaleLimits(ignoresViewportScaleLimits: Boolean); cdecl;
    procedure setMediaTypesRequiringUserActionForPlayback(mediaTypesRequiringUserActionForPlayback: WKAudiovisualMediaTypes); cdecl;
    procedure setPreferences(preferences: WKPreferences); cdecl;
    procedure setProcessPool(processPool: WKProcessPool); cdecl;
    procedure setSelectionGranularity(selectionGranularity: WKSelectionGranularity); cdecl;
    procedure setSuppressesIncrementalRendering(suppressesIncrementalRendering: Boolean); cdecl;
    [MethodName('setURLSchemeHandler:forURLScheme:')]
    procedure setURLSchemeHandler(urlSchemeHandler: Pointer; urlScheme: NSString); cdecl;
    procedure setUserContentController(userContentController: WKUserContentController); cdecl;
    procedure setWebsiteDataStore(websiteDataStore: WKWebsiteDataStore); cdecl;
    function suppressesIncrementalRendering: Boolean; cdecl;
    function urlSchemeHandlerForURLScheme(urlScheme: NSString): Pointer; cdecl;
    function userContentController: WKUserContentController; cdecl;
    function websiteDataStore: WKWebsiteDataStore; cdecl;
  end;
  TWKWebViewConfiguration = class(TOCGenericImport<WKWebViewConfigurationClass, WKWebViewConfiguration>) end;

  WKWebsiteDataRecordClass = interface(NSObjectClass)
    ['{297DD965-60EC-4AC3-9EBC-97F8E4C24140}']
  end;

  WKWebsiteDataRecord = interface(NSObject)
    ['{B1B1D946-8FC7-4F92-A18B-AC3A2D10C480}']
    function dataTypes: NSSet; cdecl;
    function displayName: NSString; cdecl;
  end;
  TWKWebsiteDataRecord = class(TOCGenericImport<WKWebsiteDataRecordClass, WKWebsiteDataRecord>) end;

  WKWebsiteDataStoreClass = interface(NSObjectClass)
    ['{42FE87F5-4889-4C44-AD5A-E9CA068E71F0}']
    {class} function allWebsiteDataTypes: NSSet; cdecl;
    {class} function defaultDataStore: WKWebsiteDataStore; cdecl;
    {class} function nonPersistentDataStore: WKWebsiteDataStore; cdecl;
  end;

  WKWebsiteDataStore = interface(NSObject)
    ['{064078BE-6369-4AB6-906D-54552717210F}']
    [MethodName('fetchDataRecordsOfTypes:completionHandler:')]
    procedure fetchDataRecordsOfTypes(dataTypes: NSSet; completionHandler: TWKWebsiteDataStoreBlockMethod1); cdecl;
    function httpCookieStore: WKHTTPCookieStore; cdecl;
    function isPersistent: Boolean; cdecl;
    [MethodName('removeDataOfTypes:modifiedSince:completionHandler:')]
    procedure removeDataOfTypes(dataTypes: NSSet; date: NSDate; completionHandler: TWKWebsiteDataStoreBlockMethod2); overload; cdecl;
    [MethodName('removeDataOfTypes:forDataRecords:completionHandler:')]
    procedure removeDataOfTypes(dataTypes: NSSet; dataRecords: NSArray; completionHandler: TWKWebsiteDataStoreBlockMethod2); overload; cdecl;
  end;
  TWKWebsiteDataStore = class(TOCGenericImport<WKWebsiteDataStoreClass, WKWebsiteDataStore>) end;

  WKWindowFeaturesClass = interface(NSObjectClass)
    ['{3CDA2D44-1A43-4C9D-8A38-594D735DE40F}']
  end;

  WKWindowFeatures = interface(NSObject)
    ['{0B1B7DAE-9EA4-4359-8536-B0A804CB4155}']
    function allowsResizing: NSNumber; cdecl;
    function height: NSNumber; cdecl;
    function menuBarVisibility: NSNumber; cdecl;
    function statusBarVisibility: NSNumber; cdecl;
    function toolbarsVisibility: NSNumber; cdecl;
    function width: NSNumber; cdecl;
    function x: NSNumber; cdecl;
    function y: NSNumber; cdecl;
  end;
  TWKWindowFeatures = class(TOCGenericImport<WKWindowFeaturesClass, WKWindowFeatures>) end;

function NSReadAccessURLDocumentOption: NSAttributedStringDocumentReadingOptionKey;
function WKErrorDomain: NSString;
function WKPreviewActionItemIdentifierOpen: NSString;
function WKPreviewActionItemIdentifierAddToReadingList: NSString;
function WKPreviewActionItemIdentifierCopy: NSString;
function WKPreviewActionItemIdentifierShare: NSString;
function WKWebsiteDataTypeFetchCache: NSString;
function WKWebsiteDataTypeDiskCache: NSString;
function WKWebsiteDataTypeMemoryCache: NSString;
function WKWebsiteDataTypeOfflineWebApplicationCache: NSString;
function WKWebsiteDataTypeCookies: NSString;
function WKWebsiteDataTypeSessionStorage: NSString;
function WKWebsiteDataTypeLocalStorage: NSString;
function WKWebsiteDataTypeWebSQLDatabases: NSString;
function WKWebsiteDataTypeIndexedDBDatabases: NSString;
function WKWebsiteDataTypeServiceWorkerRegistrations: NSString;

const
  libWebKit = '/System/Library/Frameworks/WebKit.framework/WebKit';

implementation

uses
  Posix.Dlfcn;

var
  WebKitModule: THandle;

function NSReadAccessURLDocumentOption: NSAttributedStringDocumentReadingOptionKey;
begin
  Result := CocoaNSStringConst(libWebKit, 'NSReadAccessURLDocumentOption');
end;

function WKErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKErrorDomain');
end;

function WKPreviewActionItemIdentifierOpen: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKPreviewActionItemIdentifierOpen');
end;

function WKPreviewActionItemIdentifierAddToReadingList: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKPreviewActionItemIdentifierAddToReadingList');
end;

function WKPreviewActionItemIdentifierCopy: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKPreviewActionItemIdentifierCopy');
end;

function WKPreviewActionItemIdentifierShare: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKPreviewActionItemIdentifierShare');
end;

function WKWebsiteDataTypeFetchCache: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeFetchCache');
end;

function WKWebsiteDataTypeDiskCache: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeDiskCache');
end;

function WKWebsiteDataTypeMemoryCache: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeMemoryCache');
end;

function WKWebsiteDataTypeOfflineWebApplicationCache: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeOfflineWebApplicationCache');
end;

function WKWebsiteDataTypeCookies: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeCookies');
end;

function WKWebsiteDataTypeSessionStorage: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeSessionStorage');
end;

function WKWebsiteDataTypeLocalStorage: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeLocalStorage');
end;

function WKWebsiteDataTypeWebSQLDatabases: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeWebSQLDatabases');
end;

function WKWebsiteDataTypeIndexedDBDatabases: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeIndexedDBDatabases');
end;

function WKWebsiteDataTypeServiceWorkerRegistrations: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeServiceWorkerRegistrations');
end;

initialization
  WebKitModule := dlopen(MarshaledAString(libWebKit), RTLD_LAZY);

finalization
  dlclose(WebKitModule)
end.
