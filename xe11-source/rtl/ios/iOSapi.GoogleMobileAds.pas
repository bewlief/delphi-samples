unit iOSapi.GoogleMobileAds;

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
  Macapi.ObjectiveC, Macapi.Dispatch,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.StoreKit;

const
  kGADErrorInvalidRequest = 0;
  kGADErrorNoFill = 1;
  kGADErrorNetworkError = 2;
  kGADErrorServerError = 3;
  kGADErrorOSVersionTooLow = 4;
  kGADErrorTimeout = 5;
  kGADErrorInterstitialAlreadyUsed = 6;
  kGADErrorMediationDataError = 7;
  kGADErrorMediationAdapterError = 8;
  kGADErrorMediationInvalidAdSize = 10;
  kGADErrorInternalError = 11;
  kGADErrorInvalidArgument = 12;
  kGADErrorReceivedInvalidResponse = 13;
  kGADErrorRewardedAdAlreadyUsed = 14;
  kGADErrorMediationNoFill = 9;
  kGADErrorAdAlreadyUsed = 19;
  kGADErrorApplicationIdentifierMissing = 20;
  kGADGenderUnknown = 0;
  kGADGenderMale = 1;
  kGADGenderFemale = 2;
  GADAdValuePrecisionUnknown = 0;
  GADAdValuePrecisionEstimated = 1;
  GADAdValuePrecisionPublisherProvided = 2;
  GADAdValuePrecisionPrecise = 3;
  GADAdFormatBanner = 0;
  GADAdFormatInterstitial = 1;
  GADAdFormatRewarded = 2;
  GADAdFormatNative = 3;
  GADAdapterInitializationStateNotReady = 0;
  GADAdapterInitializationStateReady = 1;
  kGADInAppPurchaseStatusError = 0;
  kGADInAppPurchaseStatusSuccessful = 1;
  kGADInAppPurchaseStatusCancel = 2;
  kGADInAppPurchaseStatusInvalidProduct = 3;
  GADMediaAspectRatioUnknown = 0;
  GADMediaAspectRatioAny = 1;
  GADMediaAspectRatioLandscape = 2;
  GADMediaAspectRatioPortrait = 3;
  GADMediaAspectRatioSquare = 4;
  GADNativeAdImageAdLoaderOptionsOrientationAny = 1;
  GADNativeAdImageAdLoaderOptionsOrientationPortrait = 2;
  GADNativeAdImageAdLoaderOptionsOrientationLandscape = 3;
  GADAdChoicesPositionTopRightCorner = 0;
  GADAdChoicesPositionTopLeftCorner = 1;
  GADAdChoicesPositionBottomRightCorner = 2;
  GADAdChoicesPositionBottomLeftCorner = 3;
  GADPresentationErrorCodeAdNotReady = 15;
  GADPresentationErrorCodeAdTooLarge = 16;
  GADPresentationErrorCodeInternal = 17;
  GADPresentationErrorCodeAdAlreadyUsed = 18;
  kGADMBannerAnimationTypeNone = 0;
  kGADMBannerAnimationTypeFlipFromLeft = 1;
  kGADMBannerAnimationTypeFlipFromRight = 2;
  kGADMBannerAnimationTypeCurlUp = 3;
  kGADMBannerAnimationTypeCurlDown = 4;
  kGADMBannerAnimationTypeSlideFromLeft = 5;
  kGADMBannerAnimationTypeSlideFromRight = 6;
  kGADMBannerAnimationTypeFadeIn = 7;
  kGADMBannerAnimationTypeRandom = 8;

type
  DFPCustomRenderedBannerViewDelegate = interface;
  GADRequestError = interface;
  GADAdLoaderDelegate = interface;
  GADAdNetworkExtras = interface;
  GADRequest = interface;
  GADAdLoaderOptions = interface;
  GADAdLoader = interface;
  GADAppEventDelegate = interface;
  GADAdSizeDelegate = interface;
  GADAdValue = interface;
  GADBannerViewDelegate = interface;
  GADDefaultInAppPurchaseDelegate = interface;
  GADInAppPurchaseDelegate = interface;
  GADResponseInfo = interface;
  GADBannerView = interface;
  GADVideoController = interface;
  DFPBannerAdLoaderDelegate = interface;
  DFPBannerView = interface;
  DFPBannerViewOptions = interface;
  DFPCustomRenderedAd = interface;
  DFPCustomRenderedInterstitialDelegate = interface;
  GADInterstitialDelegate = interface;
  GADInterstitial = interface;
  DFPInterstitial = interface;
  DFPRequest = interface;
  GADAdChoicesView = interface;
  GADAudioVideoManagerDelegate = interface;
  GADAudioVideoManager = interface;
  GADAdapterStatus = interface;
  GADInitializationStatus = interface;
  GADRequestConfiguration = interface;
  GADMobileAds = interface;
  GADAdReward = interface;
  GADAppOpenAd = interface;
  GADAppOpenAdView = interface;
  GADCustomEventBannerDelegate = interface;
  GADCustomEventRequest = interface;
  GADCustomEventBanner = interface;
  GADCustomEventExtras = interface;
  GADCustomEventInterstitialDelegate = interface;
  GADCustomEventInterstitial = interface;
  GADCustomEventNativeAd = interface;
  GADNativeAdImage = interface;
  GADMediatedUnifiedNativeAd = interface;
  GADCustomEventNativeAdDelegate = interface;
  GADDebugOptionsViewControllerDelegate = interface;
  GADDebugOptionsViewController = interface;
  GADDelayedAdRenderingDelegate = interface;
  GADDelayedAdRenderingOptions = interface;
  GADDisplayAdMeasurement = interface;
  GADDynamicHeightSearchRequest = interface;
  GADExtras = interface;
  GADDefaultInAppPurchase = interface;
  GADInAppPurchase = interface;
  GADMediaContent = interface;
  GADInstreamAd = interface;
  GADInstreamAdView = interface;
  GADMediaView = interface;
  GADMultipleAdsAdLoaderOptions = interface;
  GADMuteThisAdReason = interface;
  GADNativeAd = interface;
  GADNativeAdDelegate = interface;
  GADNativeAdImageAdLoaderOptions = interface;
  GADNativeAdMediaAdLoaderOptions = interface;
  GADNativeAdViewAdOptions = interface;
  GADNativeCustomTemplateAd = interface;
  GADNativeCustomTemplateAdLoaderDelegate = interface;
  GADNativeExpressAdViewDelegate = interface;
  GADNativeExpressAdView = interface;
  GADNativeMuteThisAdLoaderOptions = interface;
  GADRewardBasedVideoAdDelegate = interface;
  GADRewardBasedVideoAd = interface;
  GADRewardedAdDelegate = interface;
  GADRewardedAdMetadataDelegate = interface;
  GADServerSideVerificationOptions = interface;
  GADRewardedAd = interface;
  GADSearchBannerView = interface;
  GADUnifiedNativeAdDelegate = interface;
  GADUnifiedNativeAd = interface;
  GADUnifiedNativeAdLoaderDelegate = interface;
  GADUnifiedNativeAdView = interface;
  GADUnifiedNativeAdUnconfirmedClickDelegate = interface;
  GADVideoControllerDelegate = interface;
  GADVideoOptions = interface;
  GADMediationAdRequest = interface;
  GADMAdNetworkConnector = interface;
  GADMAdNetworkAdapter = interface;
  GADMediatedUnifiedNativeAdNotificationSource = interface;
  GADMediationAd = interface;
  GADMediationAdEventDelegate = interface;
  GADMediationBannerAdEventDelegate = interface;
  GADMediationInterstitialAdEventDelegate = interface;
  GADMediationNativeAdEventDelegate = interface;
  GADMediationRewardedAdEventDelegate = interface;
  GADMediationCredentials = interface;
  GADMediationServerConfiguration = interface;
  GADMediationAdConfiguration = interface;
  GADMediationBannerAd = interface;
  GADMediationBannerAdConfiguration = interface;
  GADMediationInterstitialAd = interface;
  GADMediationInterstitialAdConfiguration = interface;
  GADMediationNativeAd = interface;
  GADMediationNativeAdConfiguration = interface;
  GADMediationRewardedAd = interface;
  GADMediationRewardedAdConfiguration = interface;
  GADMediationAdapter = interface;
  GADMRewardBasedVideoAdNetworkAdapter = interface;
  GADMRewardBasedVideoAdNetworkConnector = interface;
  GADRTBMediationSignalsConfiguration = interface;
  GADRTBRequestParameters = interface;
  GADRTBAdapter = interface;

  GADAdapterInitializationState = NSInteger;
  GADAdChoicesPosition = NSInteger;
  GADAdFormat = NSInteger;
  GADAdLoaderAdType = NSString;
  GADAdMetadataKey = NSString;
  GADAdValuePrecision = NSInteger;
  GADErrorCode = NSInteger;
  GADGender = NSInteger;
  GADInAppPurchaseStatus = NSInteger;
  GADMaxAdContentRating = NSString;
  GADMBannerAnimationType = NSInteger;
  GADMediaAspectRatio = NSInteger;
  GADNativeAdImageAdLoaderOptionsOrientation = NSInteger;
  GADPresentationErrorCode = NSInteger;
  GADUnifiedNativeAssetIdentifier = NSString;

  GADAdSize = record
    size: CGSize;
    flags: NSUInteger;
  end;
  PGADAdSize = ^GADAdSize;

  GADVersionNumber = record
    majorVersion: NSInteger;
    minorVersion: NSInteger;
    patchVersion: NSInteger;
  end;
  PGADVersionNumber = ^GADVersionNumber;

  GADAppOpenAdCloseHandler = procedure of object;
  GADAppOpenAdLoadCompletionHandler = procedure(appOpenAd: GADAppOpenAd; error: NSError) of object;
  GADInitializationCompletionHandler = procedure(status: GADInitializationStatus) of object;
  GADInstreamAdLoadCompletionHandler = procedure(instreamAd: GADInstreamAd; error: NSError) of object;
  GADMediationAdapterSetUpCompletionBlock = procedure(error: NSError) of object;
  GADMediationBannerLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;
  GADMediationInterstitialLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;
  GADMediationNativeLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;
  GADMediationRewardedLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;
  GADNativeAdCustomClickHandler = procedure(assetID: NSString) of object;
  GADPaidEventHandler = procedure(value: GADAdValue) of object;
  GADRewardedAdLoadCompletionHandler = procedure(error: GADRequestError) of object;
  GADRTBSignalCompletionHandler = procedure(signals: NSString; error: NSError) of object;

  DFPCustomRenderedBannerViewDelegate = interface(IObjectiveC)
    ['{89F777D3-E359-4B3A-BD45-7D1F1229597C}']
    [MethodName('bannerView:didReceiveCustomRenderedAd:')]
    procedure bannerView(bannerView: DFPBannerView; customRenderedAd: DFPCustomRenderedAd); cdecl;
  end;

  GADRequestErrorClass = interface(NSErrorClass)
    ['{ABF8689E-E9B9-47F3-96A8-C3920BCBB77A}']
  end;

  GADRequestError = interface(NSError)
    ['{40180B03-E23E-4FC9-88F3-C50EAD9D8DD1}']
  end;
  TGADRequestError = class(TOCGenericImport<GADRequestErrorClass, GADRequestError>) end;

  GADAdLoaderDelegate = interface(IObjectiveC)
    ['{67E6B2B9-EFCE-4A17-8793-26CF0F2B6267}']
    procedure adLoader(adLoader: GADAdLoader; error: GADRequestError); cdecl;
    procedure adLoaderDidFinishLoading(adLoader: GADAdLoader); cdecl;
  end;

  GADAdNetworkExtras = interface(IObjectiveC)
    ['{AE29005B-64BE-4615-B576-E7989B4B5273}']
  end;

  GADRequestClass = interface(NSObjectClass)
    ['{C68BC342-D749-4763-8001-903CE1E03326}']
    {class} function request: Pointer; cdecl;
    {class} function sdkVersion: NSString; cdecl;
  end;

  GADRequest = interface(NSObject)
    ['{F1F4A949-AFE9-42C1-99BF-63D619CE8414}']
    function adNetworkExtrasFor(aClass: Pointer): Pointer; cdecl;
    function birthday: NSDate; cdecl;
    function contentURL: NSString; cdecl;
    function gender: GADGender; cdecl;
    function keywords: NSArray; cdecl;
    procedure registerAdNetworkExtras(extras: Pointer); cdecl;
    procedure removeAdNetworkExtrasFor(aClass: Pointer); cdecl;
    function requestAgent: NSString; cdecl;
    function scene: NSObject; cdecl;
    procedure setBirthday(birthday: NSDate); cdecl;
    procedure setBirthdayWithMonth(month: NSInteger; day: NSInteger; year: NSInteger); cdecl;
    procedure setContentURL(contentURL: NSString); cdecl;
    procedure setGender(gender: GADGender); cdecl;
    procedure setKeywords(keywords: NSArray); cdecl;
    procedure setLocationWithDescription(locationDescription: NSString); cdecl;
    procedure setLocationWithLatitude(latitude: CGFloat; longitude: CGFloat; accuracyInMeters: CGFloat); cdecl;
    procedure setRequestAgent(requestAgent: NSString); cdecl;
    procedure setScene(scene: NSObject); cdecl;
    procedure setTestDevices(testDevices: NSArray); cdecl;
    procedure tagForChildDirectedTreatment(childDirectedTreatment: Boolean); cdecl;
    function testDevices: NSArray; cdecl;
  end;
  TGADRequest = class(TOCGenericImport<GADRequestClass, GADRequest>) end;

  GADAdLoaderOptionsClass = interface(NSObjectClass)
    ['{CF8AA16E-456E-41D2-AF44-4006D526797C}']
  end;

  GADAdLoaderOptions = interface(NSObject)
    ['{D4496760-47CB-4F8B-B669-1D40B8B2D721}']
  end;
  TGADAdLoaderOptions = class(TOCGenericImport<GADAdLoaderOptionsClass, GADAdLoaderOptions>) end;

  GADAdLoaderClass = interface(NSObjectClass)
    ['{4D490BA2-011F-4632-8551-F8C6ECB96DD4}']
  end;

  GADAdLoader = interface(NSObject)
    ['{C8C2DF6F-0581-43A0-A8FC-11B3820EFCC4}']
    function adUnitID: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function initWithAdUnitID(adUnitID: NSString; rootViewController: UIViewController; adTypes: NSArray; options: NSArray): Pointer; cdecl;
    function isLoading: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADAdLoader = class(TOCGenericImport<GADAdLoaderClass, GADAdLoader>) end;

  GADAppEventDelegate = interface(IObjectiveC)
    ['{C23D7799-884E-498A-896A-5A712039E015}']
    procedure adView(banner: GADBannerView; name: NSString; info: NSString); cdecl;
    procedure interstitial(interstitial: GADInterstitial; name: NSString; info: NSString); cdecl;
  end;

  GADAdSizeDelegate = interface(IObjectiveC)
    ['{34461A0A-956E-408D-82DB-AE7D569E49F8}']
    [MethodName('adView:willChangeAdSizeTo:')]
    procedure adView(bannerView: GADBannerView; size: GADAdSize); cdecl;
  end;

  GADAdValueClass = interface(NSObjectClass)
    ['{4592E0C6-7C32-465D-9A5B-3999E03BC88D}']
  end;

  GADAdValue = interface(NSObject)
    ['{1B295730-CC28-471B-B0C5-FC47012D3694}']
    function currencyCode: NSString; cdecl;
    function precision: GADAdValuePrecision; cdecl;
    function value: NSDecimalNumber; cdecl;
  end;
  TGADAdValue = class(TOCGenericImport<GADAdValueClass, GADAdValue>) end;

  GADBannerViewDelegate = interface(IObjectiveC)
    ['{CF6B557E-58C2-4DAE-9815-B791E24B1BEF}']
    procedure adView(bannerView: GADBannerView; error: GADRequestError); cdecl;
    procedure adViewDidDismissScreen(bannerView: GADBannerView); cdecl;
    procedure adViewDidReceiveAd(bannerView: GADBannerView); cdecl;
    procedure adViewWillDismissScreen(bannerView: GADBannerView); cdecl;
    procedure adViewWillLeaveApplication(bannerView: GADBannerView); cdecl;
    procedure adViewWillPresentScreen(bannerView: GADBannerView); cdecl;
  end;

  GADDefaultInAppPurchaseDelegate = interface(IObjectiveC)
    ['{EF970370-5B77-48DC-BBD1-CA45AD21A8DA}']
    function shouldStartPurchaseForProductID(productID: NSString; quantity: NSInteger): Boolean; cdecl;
    procedure userDidPayForPurchase(defaultInAppPurchase: GADDefaultInAppPurchase); cdecl;
  end;

  GADInAppPurchaseDelegate = interface(IObjectiveC)
    ['{7FD5836E-7BBF-4D76-8372-93187958449E}']
    procedure didReceiveInAppPurchase(purchase: GADInAppPurchase); cdecl;
  end;

  GADResponseInfoClass = interface(NSObjectClass)
    ['{00A5B14E-D8A6-4CCC-8A0F-A102B665026D}']
  end;

  GADResponseInfo = interface(NSObject)
    ['{FE873DC2-F870-42EB-A6B2-4F0EDCCE8C41}']
    function adNetworkClassName: NSString; cdecl;
    function responseIdentifier: NSString; cdecl;
  end;
  TGADResponseInfo = class(TOCGenericImport<GADResponseInfoClass, GADResponseInfo>) end;

  GADBannerViewClass = interface(UIViewClass)
    ['{F560569A-4AE2-4ED1-B89F-944ECB1762CC}']
  end;

  GADBannerView = interface(UIView)
    ['{470654FF-45A1-426C-9C4D-BDFDC0A659D7}']
    function adNetworkClassName: NSString; cdecl;
    function adSize: GADAdSize; cdecl;
    function adSizeDelegate: Pointer; cdecl;
    function adUnitID: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function hasAutoRefreshed: Boolean; cdecl;
    function inAppPurchaseDelegate: Pointer; cdecl;
    function initWithAdSize(adSize: GADAdSize): Pointer; overload; cdecl;
    [MethodName('initWithAdSize:origin:')]
    function initWithAdSize(adSize: GADAdSize; origin: CGPoint): Pointer; overload; cdecl;
    function isAutoloadEnabled: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    function mediatedAdView: UIView; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setAdSize(adSize: GADAdSize); cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setAutoloadEnabled(autoloadEnabled: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setInAppPurchaseDelegate(inAppPurchaseDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
  end;
  TGADBannerView = class(TOCGenericImport<GADBannerViewClass, GADBannerView>) end;

  GADVideoControllerClass = interface(NSObjectClass)
    ['{2CC1D25D-E9FE-45AB-9B69-486D5F80F342}']
  end;

  GADVideoController = interface(NSObject)
    ['{57FAFF14-FF7C-459F-BEE6-A14B5A5BF74C}']
    function aspectRatio: Double; cdecl;
    function clickToExpandEnabled: Boolean; cdecl;
    function customControlsEnabled: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function hasVideoContent: Boolean; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMute(mute: Boolean); cdecl;
    procedure stop; cdecl;
  end;
  TGADVideoController = class(TOCGenericImport<GADVideoControllerClass, GADVideoController>) end;

  DFPBannerAdLoaderDelegate = interface(IObjectiveC)
    ['{A11F7960-977E-4473-A666-E27FE965666C}']
    procedure adLoader(adLoader: GADAdLoader; bannerView: DFPBannerView); cdecl;
    function validBannerSizesForAdLoader(adLoader: GADAdLoader): NSArray; cdecl;
  end;

  DFPBannerViewClass = interface(GADBannerViewClass)
    ['{52B72AEC-7AFC-48F7-9295-8263515B80EE}']
  end;

  DFPBannerView = interface(GADBannerView)
    ['{D3859CBC-AAC7-43F7-A2DD-FAF5A2AC4304}']
    function adSizeDelegate: Pointer; cdecl;
    function adUnitID: NSString; cdecl;
    function appEventDelegate: Pointer; cdecl;
    function customRenderedBannerViewDelegate: Pointer; cdecl;
    function enableManualImpressions: Boolean; cdecl;
    procedure recordImpression; cdecl;
    procedure resize(size: GADAdSize); cdecl;
    procedure setAdOptions(adOptions: NSArray); cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setAppEventDelegate(appEventDelegate: Pointer); cdecl;
    procedure setCustomRenderedBannerViewDelegate(customRenderedBannerViewDelegate: Pointer); cdecl;
    procedure setEnableManualImpressions(enableManualImpressions: Boolean); cdecl;
    procedure setValidAdSizes(validAdSizes: NSArray); cdecl;
    procedure setValidAdSizesWithSizes(firstSize: PGADAdSize); cdecl;
    function validAdSizes: NSArray; cdecl;
    function videoController: GADVideoController; cdecl;
  end;
  TDFPBannerView = class(TOCGenericImport<DFPBannerViewClass, DFPBannerView>) end;

  DFPBannerViewOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{45492F7C-AEEB-4875-83A3-7973DE2944F1}']
  end;

  DFPBannerViewOptions = interface(GADAdLoaderOptions)
    ['{CAABB91C-2F3A-4ADF-97B1-94BA0A09F15C}']
    function adSizeDelegate: Pointer; cdecl;
    function appEventDelegate: Pointer; cdecl;
    function enableManualImpressions: Boolean; cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
    procedure setAppEventDelegate(appEventDelegate: Pointer); cdecl;
    procedure setEnableManualImpressions(enableManualImpressions: Boolean); cdecl;
  end;
  TDFPBannerViewOptions = class(TOCGenericImport<DFPBannerViewOptionsClass, DFPBannerViewOptions>) end;

  DFPCustomRenderedAdClass = interface(NSObjectClass)
    ['{EBB49244-7A4C-4910-A840-908590B6F667}']
  end;

  DFPCustomRenderedAd = interface(NSObject)
    ['{B12E0D32-15B8-42E0-AD6D-EB67437E98DD}']
    function adBaseURL: NSURL; cdecl;
    function adHTML: NSString; cdecl;
    procedure finishedRenderingAdView(view: UIView); cdecl;
    procedure recordClick; cdecl;
    procedure recordImpression; cdecl;
  end;
  TDFPCustomRenderedAd = class(TOCGenericImport<DFPCustomRenderedAdClass, DFPCustomRenderedAd>) end;

  DFPCustomRenderedInterstitialDelegate = interface(IObjectiveC)
    ['{B502ACB0-4046-4446-8D7F-94D369351C22}']
    procedure interstitial(interstitial: DFPInterstitial; customRenderedAd: DFPCustomRenderedAd); cdecl;
  end;

  GADInterstitialDelegate = interface(IObjectiveC)
    ['{79726360-4247-4FD8-B0B4-6CFF96F60AD8}']
    procedure interstitial(ad: GADInterstitial; error: GADRequestError); cdecl;
    procedure interstitialDidDismissScreen(ad: GADInterstitial); cdecl;
    procedure interstitialDidFailToPresentScreen(ad: GADInterstitial); cdecl;
    procedure interstitialDidReceiveAd(ad: GADInterstitial); cdecl;
    procedure interstitialWillDismissScreen(ad: GADInterstitial); cdecl;
    procedure interstitialWillLeaveApplication(ad: GADInterstitial); cdecl;
    procedure interstitialWillPresentScreen(ad: GADInterstitial); cdecl;
  end;

  GADInterstitialClass = interface(NSObjectClass)
    ['{AB22715C-A80B-4FCB-898F-41852D775E0B}']
  end;

  GADInterstitial = interface(NSObject)
    ['{C9B6FC27-957D-47DA-B0BD-3D643F19E474}']
    function adNetworkClassName: NSString; cdecl;
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PNSError): Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function hasBeenUsed: Boolean; cdecl;
    function inAppPurchaseDelegate: Pointer; cdecl;
    function initWithAdUnitID(adUnitID: NSString): Pointer; cdecl;
    function isReady: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setInAppPurchaseDelegate(inAppPurchaseDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
  end;
  TGADInterstitial = class(TOCGenericImport<GADInterstitialClass, GADInterstitial>) end;

  DFPInterstitialClass = interface(GADInterstitialClass)
    ['{3FB46584-F78F-4120-89EB-9343A949FE31}']
  end;

  DFPInterstitial = interface(GADInterstitial)
    ['{53EBA9DC-87CB-417D-850D-C300B5CC588F}']
    function appEventDelegate: Pointer; cdecl;
    function customRenderedInterstitialDelegate: Pointer; cdecl;
    function initWithAdUnitID(adUnitID: NSString): Pointer; cdecl;
    procedure setAppEventDelegate(appEventDelegate: Pointer); cdecl;
    procedure setCustomRenderedInterstitialDelegate(customRenderedInterstitialDelegate: Pointer); cdecl;
  end;
  TDFPInterstitial = class(TOCGenericImport<DFPInterstitialClass, DFPInterstitial>) end;

  DFPRequestClass = interface(GADRequestClass)
    ['{856161AF-BC55-4021-948C-9E9100E0C242}']
  end;

  DFPRequest = interface(GADRequest)
    ['{0569D11D-88A6-4D3E-BD27-06D8F93774D1}']
    function categoryExclusions: NSArray; cdecl;
    function customTargeting: NSDictionary; cdecl;
    function publisherProvidedID: NSString; cdecl;
    procedure setCategoryExclusions(categoryExclusions: NSArray); cdecl;
    procedure setCustomTargeting(customTargeting: NSDictionary); cdecl;
    procedure setPublisherProvidedID(publisherProvidedID: NSString); cdecl;
  end;
  TDFPRequest = class(TOCGenericImport<DFPRequestClass, DFPRequest>) end;

  GADAdChoicesViewClass = interface(UIViewClass)
    ['{50F7A7B5-45CA-4986-84FB-16D446C86A41}']
  end;

  GADAdChoicesView = interface(UIView)
    ['{021E125A-6B86-44FA-9454-D2AED4839D95}']
  end;
  TGADAdChoicesView = class(TOCGenericImport<GADAdChoicesViewClass, GADAdChoicesView>) end;

  GADAudioVideoManagerDelegate = interface(IObjectiveC)
    ['{16B3A5CE-40B4-40E6-A41A-E841C38DCDBF}']
    procedure audioVideoManagerDidPauseAllVideo(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerDidStopPlayingAudio(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerWillPlayAudio(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerWillPlayVideo(audioVideoManager: GADAudioVideoManager); cdecl;
  end;

  GADAudioVideoManagerClass = interface(NSObjectClass)
    ['{5BBEFFB9-DC4A-4FB8-9832-66958E46EEBA}']
  end;

  GADAudioVideoManager = interface(NSObject)
    ['{865AE32A-A4A8-4166-816E-55E0B2E94536}']
    function audioSessionIsApplicationManaged: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    procedure setAudioSessionIsApplicationManaged(audioSessionIsApplicationManaged: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADAudioVideoManager = class(TOCGenericImport<GADAudioVideoManagerClass, GADAudioVideoManager>) end;

  GADAdapterStatusClass = interface(NSObjectClass)
    ['{B2C5AFFF-F2A3-4D6E-9A1C-C488DEC94629}']
  end;

  GADAdapterStatus = interface(NSObject)
    ['{73EB922A-4A59-46FB-8F45-AF8739603E4B}']
    function description: NSString; cdecl;
    function latency: NSTimeInterval; cdecl;
    function state: GADAdapterInitializationState; cdecl;
  end;
  TGADAdapterStatus = class(TOCGenericImport<GADAdapterStatusClass, GADAdapterStatus>) end;

  GADInitializationStatusClass = interface(NSObjectClass)
    ['{72D1257D-6FDB-4129-92A2-867BC48166E8}']
  end;

  GADInitializationStatus = interface(NSObject)
    ['{67881E70-9B87-481A-9295-F6AE917A1182}']
    function adapterStatusesByClassName: NSDictionary; cdecl;
  end;
  TGADInitializationStatus = class(TOCGenericImport<GADInitializationStatusClass, GADInitializationStatus>) end;

  GADRequestConfigurationClass = interface(NSObjectClass)
    ['{72A3EDB5-BB0E-4051-A5A4-227C5D5B6C7C}']
  end;

  GADRequestConfiguration = interface(NSObject)
    ['{66DF90E0-2623-4554-AFE4-8B96E1B42A76}']
    function maxAdContentRating: GADMaxAdContentRating; cdecl;
    procedure setMaxAdContentRating(maxAdContentRating: GADMaxAdContentRating); cdecl;
    procedure setTestDeviceIdentifiers(testDeviceIdentifiers: NSArray); cdecl;
    procedure tagForChildDirectedTreatment(childDirectedTreatment: Boolean); cdecl;
    procedure tagForUnderAgeOfConsent(underAgeOfConsent: Boolean); cdecl;
    function testDeviceIdentifiers: NSArray; cdecl;
  end;
  TGADRequestConfiguration = class(TOCGenericImport<GADRequestConfigurationClass, GADRequestConfiguration>) end;

  GADMobileAdsClass = interface(NSObjectClass)
    ['{584FD714-49BA-41D0-9A24-00CDE7881FD6}']
    {class} procedure configureWithApplicationID(applicationID: NSString); cdecl;
    {class} procedure disableAutomatedInAppPurchaseReporting; cdecl;
    {class} procedure disableSDKCrashReporting; cdecl;
    {class} function sharedInstance: GADMobileAds; cdecl;
  end;

  GADMobileAds = interface(NSObject)
    ['{27D7E6D6-FF14-4564-9E54-CD76CAEE4A8B}']
    function applicationMuted: Boolean; cdecl;
    function applicationVolume: Single; cdecl;
    function audioVideoManager: GADAudioVideoManager; cdecl;
    function initializationStatus: GADInitializationStatus; cdecl;
    function isSDKVersionAtLeastMajor(major: NSInteger; minor: NSInteger; patch: NSInteger): Boolean; cdecl;
    function requestConfiguration: GADRequestConfiguration; cdecl;
    procedure setApplicationMuted(applicationMuted: Boolean); cdecl;
    procedure setApplicationVolume(applicationVolume: Single); cdecl;
    procedure startWithCompletionHandler(completionHandler: GADInitializationCompletionHandler); cdecl;
  end;
  TGADMobileAds = class(TOCGenericImport<GADMobileAdsClass, GADMobileAds>) end;

  GADAdRewardClass = interface(NSObjectClass)
    ['{A65E8ED6-E4ED-43B8-91F3-1B871F346152}']
  end;

  GADAdReward = interface(NSObject)
    ['{4D251B9B-BA77-46E8-92A7-3541A8FCC326}']
    function &type: NSString; cdecl;
    function amount: NSDecimalNumber; cdecl;
    function initWithRewardType(rewardType: NSString; rewardAmount: NSDecimalNumber): Pointer; cdecl;
  end;
  TGADAdReward = class(TOCGenericImport<GADAdRewardClass, GADAdReward>) end;

  GADAppOpenAdClass = interface(NSObjectClass)
    ['{1EFABB77-CEC1-48A9-B345-4DA7DEF6FE88}']
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; orientation: UIInterfaceOrientation;
      completionHandler: GADAppOpenAdLoadCompletionHandler); cdecl;
  end;

  GADAppOpenAd = interface(NSObject)
    ['{FB4700CB-7938-4550-93F4-FB8F573B44B2}']
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
  end;
  TGADAppOpenAd = class(TOCGenericImport<GADAppOpenAdClass, GADAppOpenAd>) end;

  GADAppOpenAdViewClass = interface(UIViewClass)
    ['{ADAD9001-DADB-4392-AB68-3917E33CF7FC}']
  end;

  GADAppOpenAdView = interface(UIView)
    ['{87D37C0B-CF78-44EB-AD2D-44BF7FF9D8AB}']
    function adCloseHandler: GADAppOpenAdCloseHandler; cdecl;
    function appOpenAd: GADAppOpenAd; cdecl;
    procedure setAdCloseHandler(adCloseHandler: GADAppOpenAdCloseHandler); cdecl;
    procedure setAppOpenAd(appOpenAd: GADAppOpenAd); cdecl;
  end;
  TGADAppOpenAdView = class(TOCGenericImport<GADAppOpenAdViewClass, GADAppOpenAdView>) end;

  GADCustomEventBannerDelegate = interface(IObjectiveC)
    ['{4B34EFCE-9A29-4ABB-8BDC-2843100A7512}']
    [MethodName('customEventBanner:clickDidOccurInAd:')]
    procedure customEventBannerClickDidOccurInAd(customEvent: Pointer; view: UIView); cdecl;
    procedure customEventBannerDidDismissModal(customEvent: Pointer); cdecl;
    [MethodName('customEventBanner:didFailAd:')]
    procedure customEventBannerDidFailAd(customEvent: Pointer; error: NSError); cdecl;
    [MethodName('customEventBanner:didReceiveAd:')]
    procedure customEventBannerDidReceiveAd(customEvent: Pointer; view: UIView); cdecl;
    procedure customEventBannerWasClicked(customEvent: Pointer); cdecl;
    procedure customEventBannerWillDismissModal(customEvent: Pointer); cdecl;
    procedure customEventBannerWillLeaveApplication(customEvent: Pointer); cdecl;
    procedure customEventBannerWillPresentModal(customEvent: Pointer); cdecl;
    function viewControllerForPresentingModalView: UIViewController; cdecl;
  end;

  GADCustomEventRequestClass = interface(NSObjectClass)
    ['{18B27CE2-7889-467A-95A1-774DADA78C97}']
  end;

  GADCustomEventRequest = interface(NSObject)
    ['{C7833651-3E8B-4527-9528-E85650899905}']
    function additionalParameters: NSDictionary; cdecl;
    function isTesting: Boolean; cdecl;
    function userBirthday: NSDate; cdecl;
    function userGender: GADGender; cdecl;
    function userHasLocation: Boolean; cdecl;
    function userKeywords: NSArray; cdecl;
    function userLatitude: CGFloat; cdecl;
    function userLocationAccuracyInMeters: CGFloat; cdecl;
    function userLocationDescription: NSString; cdecl;
    function userLongitude: CGFloat; cdecl;
  end;
  TGADCustomEventRequest = class(TOCGenericImport<GADCustomEventRequestClass, GADCustomEventRequest>) end;

  GADCustomEventBanner = interface(IObjectiveC)
    ['{4BE4624C-190B-402A-94CE-4D4F27E25C96}']
    function delegate: Pointer; cdecl;
    procedure requestBannerAd(adSize: GADAdSize; serverParameter: NSString; serverLabel: NSString; request: GADCustomEventRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADCustomEventExtrasClass = interface(NSObjectClass)
    ['{8F170E04-3BC5-4297-A163-022AB719B2D1}']
  end;

  GADCustomEventExtras = interface(NSObject)
    ['{38DA0BA5-D8AD-45C7-BA11-54543BE0E286}']
    function allExtras: NSDictionary; cdecl;
    function extrasForLabel(&label: NSString): NSDictionary; cdecl;
    procedure removeAllExtras; cdecl;
    procedure setExtras(extras: NSDictionary; &label: NSString); cdecl;
  end;
  TGADCustomEventExtras = class(TOCGenericImport<GADCustomEventExtrasClass, GADCustomEventExtras>) end;

  GADCustomEventInterstitialDelegate = interface(IObjectiveC)
    ['{15A6F037-0CD3-4A00-A826-4F826F87C598}']
    [MethodName('customEventInterstitial:didReceiveAd:')]
    procedure customEventInterstitial(customEvent: Pointer; ad: NSObject); overload; cdecl;
    [MethodName('customEventInterstitial:didFailAd:')]
    procedure customEventInterstitial(customEvent: Pointer; error: NSError); overload; cdecl;
    procedure customEventInterstitialDidDismiss(customEvent: Pointer); cdecl;
    procedure customEventInterstitialDidReceiveAd(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWasClicked(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWillDismiss(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWillLeaveApplication(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWillPresent(customEvent: Pointer); cdecl;
  end;

  GADCustomEventInterstitial = interface(IObjectiveC)
    ['{6120C0A6-2612-443C-B508-9187FB63F41E}']
    function delegate: Pointer; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    procedure requestInterstitialAdWithParameter(serverParameter: NSString; serverLabel: NSString; request: GADCustomEventRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADCustomEventNativeAd = interface(IObjectiveC)
    ['{06BB0CF4-1BA2-41B1-8303-130D7EB31BF9}']
    function delegate: Pointer; cdecl;
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
    procedure requestNativeAdWithParameter(serverParameter: NSString; request: GADCustomEventRequest; adTypes: NSArray; options: NSArray;
      rootViewController: UIViewController); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADNativeAdImageClass = interface(NSObjectClass)
    ['{FEBD08EF-2AB5-46FB-AB7C-3E7F16065ACD}']
  end;

  GADNativeAdImage = interface(NSObject)
    ['{3389B185-4B62-4E16-A245-265012169E30}']
    function image: UIImage; cdecl;
    function imageURL: NSURL; cdecl;
    function initWithImage(image: UIImage): Pointer; cdecl;
    function initWithURL(URL: NSURL; scale: CGFloat): Pointer; cdecl;
    function scale: CGFloat; cdecl;
  end;
  TGADNativeAdImage = class(TOCGenericImport<GADNativeAdImageClass, GADNativeAdImage>) end;

  GADMediatedUnifiedNativeAd = interface(IObjectiveC)
    ['{0BFA45E5-E613-4A90-B976-C0A6155C5864}']
    function adChoicesView: UIView; cdecl;
    function advertiser: NSString; cdecl;
    function body: NSString; cdecl;
    function callToAction: NSString; cdecl;
    function currentTime: NSTimeInterval; cdecl;
    procedure didRecordClickOnAssetWithName(assetName: GADUnifiedNativeAssetIdentifier; view: UIView; viewController: UIViewController); cdecl;
    procedure didRecordImpression; cdecl;
    procedure didRenderInView(view: UIView; clickableAssetViews: NSDictionary; nonclickableAssetViews: NSDictionary;
      viewController: UIViewController); cdecl;
    procedure didUntrackView(view: UIView); cdecl;
    function duration: NSTimeInterval; cdecl;
    function extraAssets: NSDictionary; cdecl;
    function hasVideoContent: Boolean; cdecl;
    function headline: NSString; cdecl;
    function icon: GADNativeAdImage; cdecl;
    function images: NSArray; cdecl;
    function mediaContentAspectRatio: CGFloat; cdecl;
    function mediaView: UIView; cdecl;
    function price: NSString; cdecl;
    function starRating: NSDecimalNumber; cdecl;
    function store: NSString; cdecl;
  end;

  GADCustomEventNativeAdDelegate = interface(IObjectiveC)
    ['{554DDADD-34B4-4CF0-9AE5-430022B80E09}']
    [MethodName('customEventNativeAd:didFailToLoadWithError:')]
    procedure customEventNativeAd(customEventNativeAd: Pointer; error: NSError); overload; cdecl;
    [MethodName('customEventNativeAd:didReceiveMediatedUnifiedNativeAd:')]
    procedure customEventNativeAd(customEventNativeAd: Pointer; mediatedUnifiedNativeAd: Pointer); overload; cdecl;
  end;

  GADDebugOptionsViewControllerDelegate = interface(IObjectiveC)
    ['{79427340-CD5A-4C9C-A4F2-C17BD4D0809C}']
    procedure debugOptionsViewControllerDidDismiss(controller: GADDebugOptionsViewController); cdecl;
  end;

  GADDebugOptionsViewControllerClass = interface(UIViewControllerClass)
    ['{E7909906-BEE2-4A06-A4CA-5C6D1FF67CFA}']
    {class} function debugOptionsViewControllerWithAdUnitID(adUnitID: NSString): Pointer; cdecl;
  end;

  GADDebugOptionsViewController = interface(UIViewController)
    ['{F679D6A6-3834-4F03-B8E5-A10D7049B45B}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADDebugOptionsViewController = class(TOCGenericImport<GADDebugOptionsViewControllerClass, GADDebugOptionsViewController>) end;

  GADDelayedAdRenderingDelegate = interface(IObjectiveC)
    ['{E9B2FAB9-819E-4530-9C8B-93F76E975696}']
    function adLoader(adLoader: GADAdLoader; resumeHandler: dispatch_block_t): Boolean; cdecl;
  end;

  GADDelayedAdRenderingOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{4E6A9CBC-2A54-4EB4-A853-3E36137FE8F7}']
  end;

  GADDelayedAdRenderingOptions = interface(GADAdLoaderOptions)
    ['{261645D1-0B8F-4CBF-8336-22C816264BB0}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADDelayedAdRenderingOptions = class(TOCGenericImport<GADDelayedAdRenderingOptionsClass, GADDelayedAdRenderingOptions>) end;

  GADDisplayAdMeasurementClass = interface(NSObjectClass)
    ['{01969CAC-E87D-4077-B4FA-CECBD286072B}']
  end;

  GADDisplayAdMeasurement = interface(NSObject)
    ['{D508706D-1211-4A55-8BA4-1BA0283DA1D8}']
    procedure setView(view: UIView); cdecl;
    function startWithError(error: PNSError): Boolean; cdecl;
    function view: UIView; cdecl;
  end;
  TGADDisplayAdMeasurement = class(TOCGenericImport<GADDisplayAdMeasurementClass, GADDisplayAdMeasurement>) end;

  GADDynamicHeightSearchRequestClass = interface(GADRequestClass)
    ['{08385F87-520C-4D58-840B-065DF5ECFD6D}']
  end;

  GADDynamicHeightSearchRequest = interface(GADRequest)
    ['{B35CC3A4-4CC7-4AF3-8E82-290E6433ABB2}']
    function adBorderColor: NSString; cdecl;
    function adBorderCSSSelections: NSString; cdecl;
    function adjustableLineHeight: CGFloat; cdecl;
    function adPage: NSInteger; cdecl;
    function adSeparatorColor: NSString; cdecl;
    function adTestEnabled: Boolean; cdecl;
    function annotationFontSize: CGFloat; cdecl;
    function annotationTextColor: NSString; cdecl;
    function attributionBottomSpacing: CGFloat; cdecl;
    function attributionFontFamily: NSString; cdecl;
    function attributionFontSize: CGFloat; cdecl;
    function attributionTextColor: NSString; cdecl;
    function backgroundColor: NSString; cdecl;
    function boldTitleEnabled: Boolean; cdecl;
    function borderColor: NSString; cdecl;
    function borderCSSSelections: NSString; cdecl;
    function channel: NSString; cdecl;
    function clickToCallExtensionEnabled: Boolean; cdecl;
    function CSSWidth: NSString; cdecl;
    function descriptionFontSize: CGFloat; cdecl;
    function detailedAttributionExtensionEnabled: Boolean; cdecl;
    function domainLinkColor: NSString; cdecl;
    function domainLinkFontSize: CGFloat; cdecl;
    function fontFamily: NSString; cdecl;
    function hostLanguage: NSString; cdecl;
    function locationExtensionEnabled: Boolean; cdecl;
    function locationExtensionFontSize: CGFloat; cdecl;
    function locationExtensionTextColor: NSString; cdecl;
    function longerHeadlinesExtensionEnabled: Boolean; cdecl;
    function numberOfAds: NSInteger; cdecl;
    function plusOnesExtensionEnabled: Boolean; cdecl;
    function query: NSString; cdecl;
    function sellerRatingsExtensionEnabled: Boolean; cdecl;
    procedure setAdBorderColor(adBorderColor: NSString); cdecl;
    procedure setAdBorderCSSSelections(adBorderCSSSelections: NSString); cdecl;
    procedure setAdjustableLineHeight(adjustableLineHeight: CGFloat); cdecl;
    procedure setAdPage(adPage: NSInteger); cdecl;
    procedure setAdSeparatorColor(adSeparatorColor: NSString); cdecl;
    procedure setAdTestEnabled(adTestEnabled: Boolean); cdecl;
    procedure setAdvancedOptionValue(value: Pointer; key: NSString); cdecl;
    procedure setAnnotationFontSize(annotationFontSize: CGFloat); cdecl;
    procedure setAnnotationTextColor(annotationTextColor: NSString); cdecl;
    procedure setAttributionBottomSpacing(attributionBottomSpacing: CGFloat); cdecl;
    procedure setAttributionFontFamily(attributionFontFamily: NSString); cdecl;
    procedure setAttributionFontSize(attributionFontSize: CGFloat); cdecl;
    procedure setAttributionTextColor(attributionTextColor: NSString); cdecl;
    procedure setBackgroundColor(backgroundColor: NSString); cdecl;
    procedure setBoldTitleEnabled(boldTitleEnabled: Boolean); cdecl;
    procedure setBorderColor(borderColor: NSString); cdecl;
    procedure setBorderCSSSelections(borderCSSSelections: NSString); cdecl;
    procedure setChannel(channel: NSString); cdecl;
    procedure setClickToCallExtensionEnabled(clickToCallExtensionEnabled: Boolean); cdecl;
    procedure setCSSWidth(CSSWidth: NSString); cdecl;
    procedure setDescriptionFontSize(descriptionFontSize: CGFloat); cdecl;
    procedure setDetailedAttributionExtensionEnabled(detailedAttributionExtensionEnabled: Boolean); cdecl;
    procedure setDomainLinkColor(domainLinkColor: NSString); cdecl;
    procedure setDomainLinkFontSize(domainLinkFontSize: CGFloat); cdecl;
    procedure setFontFamily(fontFamily: NSString); cdecl;
    procedure setHostLanguage(hostLanguage: NSString); cdecl;
    procedure setLocationExtensionEnabled(locationExtensionEnabled: Boolean); cdecl;
    procedure setLocationExtensionFontSize(locationExtensionFontSize: CGFloat); cdecl;
    procedure setLocationExtensionTextColor(locationExtensionTextColor: NSString); cdecl;
    procedure setLongerHeadlinesExtensionEnabled(longerHeadlinesExtensionEnabled: Boolean); cdecl;
    procedure setNumberOfAds(numberOfAds: NSInteger); cdecl;
    procedure setPlusOnesExtensionEnabled(plusOnesExtensionEnabled: Boolean); cdecl;
    procedure setQuery(query: NSString); cdecl;
    procedure setSellerRatingsExtensionEnabled(sellerRatingsExtensionEnabled: Boolean); cdecl;
    procedure setSiteLinksExtensionEnabled(siteLinksExtensionEnabled: Boolean); cdecl;
    procedure setTextColor(textColor: NSString); cdecl;
    procedure setTitleFontSize(titleFontSize: CGFloat); cdecl;
    procedure setTitleLinkColor(titleLinkColor: NSString); cdecl;
    procedure setTitleUnderlineHidden(titleUnderlineHidden: Boolean); cdecl;
    procedure setVerticalSpacing(verticalSpacing: CGFloat); cdecl;
    function siteLinksExtensionEnabled: Boolean; cdecl;
    function textColor: NSString; cdecl;
    function titleFontSize: CGFloat; cdecl;
    function titleLinkColor: NSString; cdecl;
    function titleUnderlineHidden: Boolean; cdecl;
    function verticalSpacing: CGFloat; cdecl;
  end;
  TGADDynamicHeightSearchRequest = class(TOCGenericImport<GADDynamicHeightSearchRequestClass, GADDynamicHeightSearchRequest>) end;

  GADExtrasClass = interface(NSObjectClass)
    ['{1825824A-D34A-4522-837A-0EB47B02E80F}']
  end;

  GADExtras = interface(NSObject)
    ['{E5386B93-16FC-4CE5-B1A4-1D3D2BEE75AF}']
    function additionalParameters: NSDictionary; cdecl;
    procedure setAdditionalParameters(additionalParameters: NSDictionary); cdecl;
  end;
  TGADExtras = class(TOCGenericImport<GADExtrasClass, GADExtras>) end;

  GADDefaultInAppPurchaseClass = interface(NSObjectClass)
    ['{B27B2F69-B46A-4761-B79D-ADE643D9FF3C}']
    {class} procedure disableDefaultPurchaseFlow; cdecl;
    {class} procedure enableDefaultPurchaseFlowWithDelegate(delegate: Pointer); cdecl;
  end;

  GADDefaultInAppPurchase = interface(NSObject)
    ['{25A335AF-939C-421C-9555-BBFFC02F183E}']
    procedure finishTransaction; cdecl;
    function paymentTransaction: SKPaymentTransaction; cdecl;
    function productID: NSString; cdecl;
    function quantity: NSInteger; cdecl;
  end;
  TGADDefaultInAppPurchase = class(TOCGenericImport<GADDefaultInAppPurchaseClass, GADDefaultInAppPurchase>) end;

  GADInAppPurchaseClass = interface(NSObjectClass)
    ['{59B4E6A7-71B7-46FE-B754-FDC76DF4D21C}']
  end;

  GADInAppPurchase = interface(NSObject)
    ['{582D1B91-021B-47B7-8152-11408A645DC6}']
    function productID: NSString; cdecl;
    function quantity: NSInteger; cdecl;
    procedure reportPurchaseStatus(purchaseStatus: GADInAppPurchaseStatus); cdecl;
  end;
  TGADInAppPurchase = class(TOCGenericImport<GADInAppPurchaseClass, GADInAppPurchase>) end;

  GADMediaContentClass = interface(NSObjectClass)
    ['{8892078C-3A1E-405C-9F9C-1E89713C46E4}']
  end;

  GADMediaContent = interface(NSObject)
    ['{C3640842-1BA8-44E7-AEEC-88009C08D679}']
    function aspectRatio: CGFloat; cdecl;
    function currentTime: NSTimeInterval; cdecl;
    function duration: NSTimeInterval; cdecl;
    function hasVideoContent: Boolean; cdecl;
    function mainImage: UIImage; cdecl;
    procedure setMainImage(mainImage: UIImage); cdecl;
    function videoController: GADVideoController; cdecl;
  end;
  TGADMediaContent = class(TOCGenericImport<GADMediaContentClass, GADMediaContent>) end;

  GADInstreamAdClass = interface(NSObjectClass)
    ['{3367BC2E-39B1-40EE-9393-B4A993D379A0}']
    {class} procedure loadAdWithAdTag(adTag: NSString; completionHandler: GADInstreamAdLoadCompletionHandler); cdecl;
    {class} procedure loadAdWithAdUnitID(adUnitID: NSString; request: GADRequest; mediaAspectRatio: GADMediaAspectRatio;
      completionHandler: GADInstreamAdLoadCompletionHandler); cdecl;
  end;

  GADInstreamAd = interface(NSObject)
    ['{BEDACF81-0969-40EF-BAEC-80BEBCDE5ACC}']
    function mediaContent: GADMediaContent; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
  end;
  TGADInstreamAd = class(TOCGenericImport<GADInstreamAdClass, GADInstreamAd>) end;

  GADInstreamAdViewClass = interface(UIViewClass)
    ['{A5F94520-64D4-46FE-93AB-660910F00387}']
  end;

  GADInstreamAdView = interface(UIView)
    ['{D44646D9-EEBD-4EE9-A543-391A2D3DCFE7}']
    function ad: GADInstreamAd; cdecl;
    procedure setAd(ad: GADInstreamAd); cdecl;
  end;
  TGADInstreamAdView = class(TOCGenericImport<GADInstreamAdViewClass, GADInstreamAdView>) end;

  GADMediaViewClass = interface(UIViewClass)
    ['{BEB95E06-03F7-46A7-91AF-6789E026D3BC}']
  end;

  GADMediaView = interface(UIView)
    ['{F6DD72AB-720C-4245-9E73-AF9CC55A1347}']
    function mediaContent: GADMediaContent; cdecl;
    procedure setMediaContent(mediaContent: GADMediaContent); cdecl;
  end;
  TGADMediaView = class(TOCGenericImport<GADMediaViewClass, GADMediaView>) end;

  GADMultipleAdsAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{3C7B5B82-4FE9-4AA5-8FD7-24E759AF12A9}']
  end;

  GADMultipleAdsAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{319EF936-4F4D-4800-A4AB-B2639CFB15CF}']
    function numberOfAds: NSInteger; cdecl;
    procedure setNumberOfAds(numberOfAds: NSInteger); cdecl;
  end;
  TGADMultipleAdsAdLoaderOptions = class(TOCGenericImport<GADMultipleAdsAdLoaderOptionsClass, GADMultipleAdsAdLoaderOptions>) end;

  GADMuteThisAdReasonClass = interface(NSObjectClass)
    ['{CF3D21FD-F113-4EFD-8E03-F4F1DF59007F}']
  end;

  GADMuteThisAdReason = interface(NSObject)
    ['{DFF9763C-0E94-4269-9212-33C9352CC72F}']
    function reasonDescription: NSString; cdecl;
  end;
  TGADMuteThisAdReason = class(TOCGenericImport<GADMuteThisAdReasonClass, GADMuteThisAdReason>) end;

  GADNativeAdClass = interface(NSObjectClass)
    ['{A82DE2CB-533F-497F-9B85-F7E8FF5EC5AD}']
  end;

  GADNativeAd = interface(NSObject)
    ['{5CEBA8DC-370A-4E51-BD7B-F1CBC675131A}']
    function adNetworkClassName: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function extraAssets: NSDictionary; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
  end;
  TGADNativeAd = class(TOCGenericImport<GADNativeAdClass, GADNativeAd>) end;

  GADNativeAdDelegate = interface(IObjectiveC)
    ['{5FF3D639-BB56-496B-AB07-8D4EA46545AC}']
    procedure nativeAdDidDismissScreen(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdDidRecordClick(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdDidRecordImpression(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdWillDismissScreen(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdWillLeaveApplication(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdWillPresentScreen(nativeAd: GADNativeAd); cdecl;
  end;

  GADNativeAdImageAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{AD3DBE85-B81C-49F6-849E-9CF567BE90D8}']
  end;

  GADNativeAdImageAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{4F537345-D0E7-4A31-9D3D-109B04DF0709}']
    function disableImageLoading: Boolean; cdecl;
    function preferredImageOrientation: GADNativeAdImageAdLoaderOptionsOrientation; cdecl;
    procedure setDisableImageLoading(disableImageLoading: Boolean); cdecl;
    procedure setPreferredImageOrientation(preferredImageOrientation: GADNativeAdImageAdLoaderOptionsOrientation); cdecl;
    procedure setShouldRequestMultipleImages(shouldRequestMultipleImages: Boolean); cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
  end;
  TGADNativeAdImageAdLoaderOptions = class(TOCGenericImport<GADNativeAdImageAdLoaderOptionsClass, GADNativeAdImageAdLoaderOptions>) end;

  GADNativeAdMediaAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{64F27F96-2E3B-4055-B8EB-07CE0C233FC6}']
  end;

  GADNativeAdMediaAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{C14BA0E1-ABD8-4DBE-8976-5023B67BC408}']
    function mediaAspectRatio: GADMediaAspectRatio; cdecl;
    procedure setMediaAspectRatio(mediaAspectRatio: GADMediaAspectRatio); cdecl;
  end;
  TGADNativeAdMediaAdLoaderOptions = class(TOCGenericImport<GADNativeAdMediaAdLoaderOptionsClass, GADNativeAdMediaAdLoaderOptions>) end;

  GADNativeAdViewAdOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{CD757401-4CED-4A16-BF8D-26C40BC5AC95}']
  end;

  GADNativeAdViewAdOptions = interface(GADAdLoaderOptions)
    ['{8AFB9591-854A-45B4-8BEA-EE7DAD7E2D06}']
    function preferredAdChoicesPosition: GADAdChoicesPosition; cdecl;
    procedure setPreferredAdChoicesPosition(preferredAdChoicesPosition: GADAdChoicesPosition); cdecl;
  end;
  TGADNativeAdViewAdOptions = class(TOCGenericImport<GADNativeAdViewAdOptionsClass, GADNativeAdViewAdOptions>) end;

  GADNativeCustomTemplateAdClass = interface(GADNativeAdClass)
    ['{F80C57D5-1733-4C8D-A684-F88EA7DA3D78}']
  end;

  GADNativeCustomTemplateAd = interface(GADNativeAd)
    ['{00C93915-BD80-42CE-A0D7-20B8E7FF40E9}']
    function availableAssetKeys: NSArray; cdecl;
    function customClickHandler: GADNativeAdCustomClickHandler; cdecl;
    function displayAdMeasurement: GADDisplayAdMeasurement; cdecl;
    function imageForKey(key: NSString): GADNativeAdImage; cdecl;
    function mediaView: GADMediaView; cdecl;
    [MethodName('performClickOnAssetWithKey:customClickHandler:')]
    procedure performClickOnAssetWithKey(assetKey: NSString; customClickHandler: dispatch_block_t); overload; cdecl;
    procedure performClickOnAssetWithKey(assetKey: NSString); overload; cdecl;
    procedure recordImpression; cdecl;
    procedure setCustomClickHandler(customClickHandler: GADNativeAdCustomClickHandler); cdecl;
    function stringForKey(key: NSString): NSString; cdecl;
    function templateID: NSString; cdecl;
    function videoController: GADVideoController; cdecl;
  end;
  TGADNativeCustomTemplateAd = class(TOCGenericImport<GADNativeCustomTemplateAdClass, GADNativeCustomTemplateAd>) end;

  GADNativeCustomTemplateAdLoaderDelegate = interface(IObjectiveC)
    ['{2D22D736-3A8A-4233-B044-BEAA512C8BA7}']
    procedure adLoader(adLoader: GADAdLoader; nativeCustomTemplateAd: GADNativeCustomTemplateAd); cdecl;
    function nativeCustomTemplateIDsForAdLoader(adLoader: GADAdLoader): NSArray; cdecl;
  end;

  GADNativeExpressAdViewDelegate = interface(IObjectiveC)
    ['{C30B6E89-0E71-444B-9E86-EACAC8AEDE4F}']
    procedure nativeExpressAdView(nativeExpressAdView: GADNativeExpressAdView; error: GADRequestError); cdecl;
    procedure nativeExpressAdViewDidDismissScreen(nativeExpressAdView: GADNativeExpressAdView); cdecl;
    procedure nativeExpressAdViewDidReceiveAd(nativeExpressAdView: GADNativeExpressAdView); cdecl;
    procedure nativeExpressAdViewWillDismissScreen(nativeExpressAdView: GADNativeExpressAdView); cdecl;
    procedure nativeExpressAdViewWillLeaveApplication(nativeExpressAdView: GADNativeExpressAdView); cdecl;
    procedure nativeExpressAdViewWillPresentScreen(nativeExpressAdView: GADNativeExpressAdView); cdecl;
  end;

  GADNativeExpressAdViewClass = interface(UIViewClass)
    ['{88C19579-4C57-4ED0-9B89-BDFCC0539613}']
  end;

  GADNativeExpressAdView = interface(UIView)
    ['{B4B7027C-7909-4D3C-93FD-266323A447D4}']
    function adNetworkClassName: NSString; cdecl;
    function adSize: GADAdSize; cdecl;
    function adUnitID: NSString; cdecl;
    function delegate: Pointer; cdecl;
    [MethodName('initWithAdSize:origin:')]
    function initWithAdSize(adSize: GADAdSize; origin: CGPoint): Pointer; overload; cdecl;
    function initWithAdSize(adSize: GADAdSize): Pointer; overload; cdecl;
    function isAutoloadEnabled: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setAdOptions(adOptions: NSArray); cdecl;
    procedure setAdSize(adSize: GADAdSize); cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setAutoloadEnabled(autoloadEnabled: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    function videoController: GADVideoController; cdecl;
  end;
  TGADNativeExpressAdView = class(TOCGenericImport<GADNativeExpressAdViewClass, GADNativeExpressAdView>) end;

  GADNativeMuteThisAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{47315F4E-066A-4509-92FE-234E9F1A661A}']
  end;

  GADNativeMuteThisAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{14990AD2-CCC2-4FBA-ADE1-7CE6D3516E13}']
    function customMuteThisAdRequested: Boolean; cdecl;
    procedure setCustomMuteThisAdRequested(customMuteThisAdRequested: Boolean); cdecl;
  end;
  TGADNativeMuteThisAdLoaderOptions = class(TOCGenericImport<GADNativeMuteThisAdLoaderOptionsClass, GADNativeMuteThisAdLoaderOptions>) end;

  GADRewardBasedVideoAdDelegate = interface(IObjectiveC)
    ['{CE064833-DDE7-425C-858F-A5EBE9251210}']
    [MethodName('rewardBasedVideoAd:didFailToLoadWithError:')]
    procedure rewardBasedVideoAd(rewardBasedVideoAd: GADRewardBasedVideoAd; error: NSError); overload; cdecl;
    [MethodName('rewardBasedVideoAd:didRewardUserWithReward:')]
    procedure rewardBasedVideoAd(rewardBasedVideoAd: GADRewardBasedVideoAd; reward: GADAdReward); overload; cdecl;
    procedure rewardBasedVideoAdDidClose(rewardBasedVideoAd: GADRewardBasedVideoAd); cdecl;
    procedure rewardBasedVideoAdDidCompletePlaying(rewardBasedVideoAd: GADRewardBasedVideoAd); cdecl;
    procedure rewardBasedVideoAdDidOpen(rewardBasedVideoAd: GADRewardBasedVideoAd); cdecl;
    procedure rewardBasedVideoAdDidReceiveAd(rewardBasedVideoAd: GADRewardBasedVideoAd); cdecl;
    procedure rewardBasedVideoAdDidStartPlaying(rewardBasedVideoAd: GADRewardBasedVideoAd); cdecl;
    procedure rewardBasedVideoAdMetadataDidChange(rewardBasedVideoAd: GADRewardBasedVideoAd); cdecl;
    procedure rewardBasedVideoAdWillLeaveApplication(rewardBasedVideoAd: GADRewardBasedVideoAd); cdecl;
  end;

  GADRewardBasedVideoAdClass = interface(NSObjectClass)
    ['{76BCFD29-71C2-4FED-B1A8-C191774F3FE3}']
    {class} function sharedInstance: GADRewardBasedVideoAd; cdecl;
  end;

  GADRewardBasedVideoAd = interface(NSObject)
    ['{ADE673C2-CA70-494A-8C7F-C041CA9AB1BD}']
    function adMetadata: NSDictionary; cdecl;
    function adNetworkClassName: NSString; cdecl;
    function customRewardString: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function isReady: Boolean; cdecl;
    procedure loadRequest(request: GADRequest; adUnitID: NSString); cdecl;
    procedure presentFromRootViewController(viewController: UIViewController); cdecl;
    procedure setCustomRewardString(customRewardString: NSString); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setUserIdentifier(userIdentifier: NSString); cdecl;
    function userIdentifier: NSString; cdecl;
  end;
  TGADRewardBasedVideoAd = class(TOCGenericImport<GADRewardBasedVideoAdClass, GADRewardBasedVideoAd>) end;

  GADRewardedAdDelegate = interface(IObjectiveC)
    ['{8D866C8F-2F37-4F3C-8673-E608360F7776}']
    [MethodName('rewardedAd:didFailToPresentWithError:')]
    procedure rewardedAd(rewardedAd: GADRewardedAd; error: NSError); overload; cdecl;
    [MethodName('rewardedAd:userDidEarnReward:')]
    procedure rewardedAd(rewardedAd: GADRewardedAd; reward: GADAdReward); overload; cdecl;
    procedure rewardedAdDidDismiss(rewardedAd: GADRewardedAd); cdecl;
    procedure rewardedAdDidPresent(rewardedAd: GADRewardedAd); cdecl;
  end;

  GADRewardedAdMetadataDelegate = interface(IObjectiveC)
    ['{8CB1D739-7C5C-4D38-883B-87EF144B8C43}']
    procedure rewardedAdMetadataDidChange(rewardedAd: GADRewardedAd); cdecl;
  end;

  GADServerSideVerificationOptionsClass = interface(NSObjectClass)
    ['{733A9EFD-60E0-46FE-AD98-FD11B9BCB29B}']
  end;

  GADServerSideVerificationOptions = interface(NSObject)
    ['{CE7CA8C0-6A75-417B-9553-B8703D0AC6AC}']
    function customRewardString: NSString; cdecl;
    procedure setCustomRewardString(customRewardString: NSString); cdecl;
    procedure setUserIdentifier(userIdentifier: NSString); cdecl;
    function userIdentifier: NSString; cdecl;
  end;
  TGADServerSideVerificationOptions = class(TOCGenericImport<GADServerSideVerificationOptionsClass, GADServerSideVerificationOptions>) end;

  GADRewardedAdClass = interface(NSObjectClass)
    ['{480CA3EA-9080-47AB-A943-DA5E794EAA88}']
  end;

  GADRewardedAd = interface(NSObject)
    ['{715FA694-1A34-42BC-85D5-4FC28D7EFA79}']
    function adMetadata: NSDictionary; cdecl;
    function adMetadataDelegate: Pointer; cdecl;
    function adNetworkClassName: NSString; cdecl;
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PNSError): Boolean; cdecl;
    function initWithAdUnitID(adUnitID: NSString): Pointer; cdecl;
    function isReady: Boolean; cdecl;
    procedure loadRequest(request: GADRequest; completionHandler: GADRewardedAdLoadCompletionHandler); cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    procedure presentFromRootViewController(viewController: UIViewController; delegate: Pointer); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function reward: GADAdReward; cdecl;
    function serverSideVerificationOptions: GADServerSideVerificationOptions; cdecl;
    procedure setAdMetadataDelegate(adMetadataDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setServerSideVerificationOptions(serverSideVerificationOptions: GADServerSideVerificationOptions); cdecl;
  end;
  TGADRewardedAd = class(TOCGenericImport<GADRewardedAdClass, GADRewardedAd>) end;

  GADSearchBannerViewClass = interface(GADBannerViewClass)
    ['{9134D93B-3CC1-4AC7-8D6E-05BDF6949A8D}']
  end;

  GADSearchBannerView = interface(GADBannerView)
    ['{AB3C5E96-4396-4018-8FB1-7542AA3680AA}']
    function adSizeDelegate: Pointer; cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
  end;
  TGADSearchBannerView = class(TOCGenericImport<GADSearchBannerViewClass, GADSearchBannerView>) end;

  GADUnifiedNativeAdDelegate = interface(IObjectiveC)
    ['{95C1E044-0CDB-4C28-A04D-A25C1B510E17}']
    procedure nativeAdDidDismissScreen(nativeAd: GADUnifiedNativeAd); cdecl;
    procedure nativeAdDidRecordClick(nativeAd: GADUnifiedNativeAd); cdecl;
    procedure nativeAdDidRecordImpression(nativeAd: GADUnifiedNativeAd); cdecl;
    procedure nativeAdIsMuted(nativeAd: GADUnifiedNativeAd); cdecl;
    procedure nativeAdWillDismissScreen(nativeAd: GADUnifiedNativeAd); cdecl;
    procedure nativeAdWillLeaveApplication(nativeAd: GADUnifiedNativeAd); cdecl;
    procedure nativeAdWillPresentScreen(nativeAd: GADUnifiedNativeAd); cdecl;
  end;

  GADUnifiedNativeAdClass = interface(NSObjectClass)
    ['{E3386CCF-33BE-48CF-AA46-5EC6AA8B8E35}']
  end;

  GADUnifiedNativeAd = interface(NSObject)
    ['{5B66244F-F8FF-4650-A583-7B3AB4848DC1}']
    function adNetworkClassName: NSString; cdecl;
    function advertiser: NSString; cdecl;
    function body: NSString; cdecl;
    function callToAction: NSString; cdecl;
    procedure cancelUnconfirmedClick; cdecl;
    function delegate: Pointer; cdecl;
    procedure enableCustomClickGestures; cdecl;
    function extraAssets: NSDictionary; cdecl;
    function headline: NSString; cdecl;
    function icon: GADNativeAdImage; cdecl;
    function images: NSArray; cdecl;
    function isCustomClickGestureEnabled: Boolean; cdecl;
    function isCustomMuteThisAdAvailable: Boolean; cdecl;
    function mediaContent: GADMediaContent; cdecl;
    function muteThisAdReasons: NSArray; cdecl;
    procedure muteThisAdWithReason(reason: GADMuteThisAdReason); cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function price: NSString; cdecl;
    procedure recordCustomClickGesture; cdecl;
    procedure registerAdView(adView: UIView; clickableAssetViews: NSDictionary; nonclickableAssetViews: NSDictionary); cdecl;
    procedure registerClickConfirmingView(view: UIView); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    procedure setUnconfirmedClickDelegate(unconfirmedClickDelegate: Pointer); cdecl;
    function starRating: NSDecimalNumber; cdecl;
    function store: NSString; cdecl;
    function unconfirmedClickDelegate: Pointer; cdecl;
    procedure unregisterAdView; cdecl;
    function videoController: GADVideoController; cdecl;
  end;
  TGADUnifiedNativeAd = class(TOCGenericImport<GADUnifiedNativeAdClass, GADUnifiedNativeAd>) end;

  GADUnifiedNativeAdLoaderDelegate = interface(IObjectiveC)
    ['{B1A1DD05-93CE-49D8-A823-45DC7AD939D4}']
    procedure adLoader(adLoader: GADAdLoader; nativeAd: GADUnifiedNativeAd); cdecl;
  end;

  GADUnifiedNativeAdViewClass = interface(UIViewClass)
    ['{4D94DF61-9CB9-431D-9571-8AC69BD520C6}']
  end;

  GADUnifiedNativeAdView = interface(UIView)
    ['{EAA78041-17FC-43F8-ACF3-30CF4ED54B24}']
    function adChoicesView: GADAdChoicesView; cdecl;
    function advertiserView: UIView; cdecl;
    function bodyView: UIView; cdecl;
    function callToActionView: UIView; cdecl;
    function headlineView: UIView; cdecl;
    function iconView: UIView; cdecl;
    function imageView: UIView; cdecl;
    function mediaView: GADMediaView; cdecl;
    function nativeAd: GADUnifiedNativeAd; cdecl;
    function priceView: UIView; cdecl;
    procedure setAdChoicesView(adChoicesView: GADAdChoicesView); cdecl;
    procedure setAdvertiserView(advertiserView: UIView); cdecl;
    procedure setBodyView(bodyView: UIView); cdecl;
    procedure setCallToActionView(callToActionView: UIView); cdecl;
    procedure setHeadlineView(headlineView: UIView); cdecl;
    procedure setIconView(iconView: UIView); cdecl;
    procedure setImageView(imageView: UIView); cdecl;
    procedure setMediaView(mediaView: GADMediaView); cdecl;
    procedure setNativeAd(nativeAd: GADUnifiedNativeAd); cdecl;
    procedure setPriceView(priceView: UIView); cdecl;
    procedure setStarRatingView(starRatingView: UIView); cdecl;
    procedure setStoreView(storeView: UIView); cdecl;
    function starRatingView: UIView; cdecl;
    function storeView: UIView; cdecl;
  end;
  TGADUnifiedNativeAdView = class(TOCGenericImport<GADUnifiedNativeAdViewClass, GADUnifiedNativeAdView>) end;

  GADUnifiedNativeAdUnconfirmedClickDelegate = interface(IObjectiveC)
    ['{53E706AF-B573-47B9-85D9-06A8339ABA04}']
    procedure nativeAd(nativeAd: GADUnifiedNativeAd; assetID: GADUnifiedNativeAssetIdentifier); cdecl;
    procedure nativeAdDidCancelUnconfirmedClick(nativeAd: GADUnifiedNativeAd); cdecl;
  end;

  GADVideoControllerDelegate = interface(IObjectiveC)
    ['{89DBDBC5-05AB-4237-84CF-D6760882FB2D}']
    procedure videoControllerDidEndVideoPlayback(videoController: GADVideoController); cdecl;
    procedure videoControllerDidMuteVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidPauseVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidPlayVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidUnmuteVideo(videoController: GADVideoController); cdecl;
  end;

  GADVideoOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{51CA7981-DD16-4522-80FB-2A94D3626DD1}']
  end;

  GADVideoOptions = interface(GADAdLoaderOptions)
    ['{0605D4DE-BB08-4C37-BB1F-3F697AD42615}']
    function clickToExpandRequested: Boolean; cdecl;
    function customControlsRequested: Boolean; cdecl;
    procedure setClickToExpandRequested(clickToExpandRequested: Boolean); cdecl;
    procedure setCustomControlsRequested(customControlsRequested: Boolean); cdecl;
    procedure setStartMuted(startMuted: Boolean); cdecl;
    function startMuted: Boolean; cdecl;
  end;
  TGADVideoOptions = class(TOCGenericImport<GADVideoOptionsClass, GADVideoOptions>) end;

  GADMediationAdRequest = interface(IObjectiveC)
    ['{7F2343DB-46D3-4C44-BC6F-5F72C1C9A052}']
    function childDirectedTreatment: NSNumber; cdecl;
    function credentials: NSDictionary; cdecl;
    function maxAdContentRating: GADMaxAdContentRating; cdecl;
    function networkExtras: Pointer; cdecl;
    function publisherId: NSString; cdecl;
    function testMode: Boolean; cdecl;
    function underAgeOfConsent: NSNumber; cdecl;
    function userBirthday: NSDate; cdecl;
    function userGender: GADGender; cdecl;
    function userHasLocation: Boolean; cdecl;
    function userKeywords: NSArray; cdecl;
    function userLatitude: CGFloat; cdecl;
    function userLocationAccuracyInMeters: CGFloat; cdecl;
    function userLocationDescription: NSString; cdecl;
    function userLongitude: CGFloat; cdecl;
  end;

  GADMAdNetworkConnector = interface(IObjectiveC)
    ['{0360B375-E36F-41FF-A8F8-C253D4090DBD}']
    [MethodName('adapter:clickDidOccurInBanner:')]
    procedure adapterClickDidOccurInBanner(adapter: Pointer; view: UIView); cdecl;
    procedure adapterDidDismissFullScreenModal(adapter: Pointer); cdecl;
    procedure adapterDidDismissInterstitial(adapter: Pointer); cdecl;
    procedure adapterDidFailAd(adapter: Pointer; error: NSError); cdecl;
    procedure adapterDidFailInterstitial(adapter: Pointer; error: NSError); cdecl;
    procedure adapterDidGetAdClick(adapter: Pointer); cdecl;
    procedure adapterDidReceiveAdView(adapter: Pointer; view: UIView); cdecl;
    procedure adapterDidReceiveInterstitial(adapter: Pointer; interstitial: NSObject); overload; cdecl;
    procedure adapterDidReceiveInterstitial(adapter: Pointer); overload; cdecl;
    procedure adapterDidReceiveMediatedUnifiedNativeAd(adapter: Pointer; mediatedUnifiedNativeAd: Pointer); cdecl;
    procedure adapterWillDismissFullScreenModal(adapter: Pointer); cdecl;
    procedure adapterWillDismissInterstitial(adapter: Pointer); cdecl;
    procedure adapterWillLeaveApplication(adapter: Pointer); cdecl;
    procedure adapterWillPresentFullScreenModal(adapter: Pointer); cdecl;
    procedure adapterWillPresentInterstitial(adapter: Pointer); cdecl;
    function adMuted: Boolean; cdecl;
    function adVolume: Single; cdecl;
    function viewControllerForPresentingModalView: UIViewController; cdecl;
  end;

  GADMAdNetworkAdapter = interface(IObjectiveC)
    ['{7BC458BE-3686-448E-9E43-5DC7174AF994}']
    function adapterVersion: NSString; cdecl;
    procedure changeAdSizeTo(adSize: GADAdSize); cdecl;
    procedure getBannerWithSize(adSize: GADAdSize); cdecl;
    procedure getInterstitial; cdecl;
    procedure getNativeAdWithAdTypes(adTypes: NSArray; options: NSArray); cdecl;
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
    function initWithGADMAdNetworkConnector(connector: Pointer): Pointer; cdecl;
    function networkExtrasClass: Pointer; cdecl;
    procedure presentInterstitialFromRootViewController(rootViewController: UIViewController); cdecl;
    procedure stopBeingDelegate; cdecl;
  end;

  GADMediatedUnifiedNativeAdNotificationSourceClass = interface(NSObjectClass)
    ['{7440AE80-8F38-44C3-8A42-FD431837AE97}']
    {class} procedure mediatedNativeAdDidDismissScreen(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidEndVideoPlayback(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidPauseVideo(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidPlayVideo(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidRecordClick(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidRecordImpression(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillDismissScreen(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillLeaveApplication(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillPresentScreen(mediatedNativeAd: Pointer); cdecl;
  end;

  GADMediatedUnifiedNativeAdNotificationSource = interface(NSObject)
    ['{93187CEA-30CD-4B0B-8F18-0810922F0266}']
  end;
  TGADMediatedUnifiedNativeAdNotificationSource = class(TOCGenericImport<GADMediatedUnifiedNativeAdNotificationSourceClass,
    GADMediatedUnifiedNativeAdNotificationSource>) end;

  GADMediationAd = interface(IObjectiveC)
    ['{7F708A01-022A-45C0-85B3-8F26F61C1144}']
  end;

  GADMediationAdEventDelegate = interface(IObjectiveC)
    ['{E3873293-E023-4772-9B7B-8E4200E40CA3}']
    procedure didDismissFullScreenView; cdecl;
    procedure didFailToPresentWithError(error: NSError); cdecl;
    procedure reportClick; cdecl;
    procedure reportImpression; cdecl;
    procedure willDismissFullScreenView; cdecl;
    procedure willPresentFullScreenView; cdecl;
  end;

  GADMediationBannerAdEventDelegate = interface(IObjectiveC)
    ['{AAB805DC-6C65-4992-9624-10327284B221}']
    procedure willBackgroundApplication; cdecl;
  end;

  GADMediationInterstitialAdEventDelegate = interface(IObjectiveC)
    ['{EB6E4D1F-CA60-46CF-855A-09EA29042239}']
    procedure willBackgroundApplication; cdecl;
  end;

  GADMediationNativeAdEventDelegate = interface(IObjectiveC)
    ['{D8488634-4E82-40F2-8395-BB8E6C9B2CAE}']
    procedure didEndVideo; cdecl;
    procedure didMuteVideo; cdecl;
    procedure didPauseVideo; cdecl;
    procedure didPlayVideo; cdecl;
    procedure didUnmuteVideo; cdecl;
    procedure willBackgroundApplication; cdecl;
  end;

  GADMediationRewardedAdEventDelegate = interface(IObjectiveC)
    ['{481B52DD-396C-4113-AFDE-1E12BF5DC769}']
    procedure didEndVideo; cdecl;
    procedure didRewardUserWithReward(reward: GADAdReward); cdecl;
    procedure didStartVideo; cdecl;
  end;

  GADMediationCredentialsClass = interface(NSObjectClass)
    ['{AA3F7D3F-B494-442A-940A-805BBE5D4F5D}']
  end;

  GADMediationCredentials = interface(NSObject)
    ['{8B9BDDBA-B2FD-4677-8818-6A1302CC07AD}']
    function format: GADAdFormat; cdecl;
    function settings: NSDictionary; cdecl;
  end;
  TGADMediationCredentials = class(TOCGenericImport<GADMediationCredentialsClass, GADMediationCredentials>) end;

  GADMediationServerConfigurationClass = interface(NSObjectClass)
    ['{ACD12D69-432D-4E3A-9851-89933C87E7CE}']
  end;

  GADMediationServerConfiguration = interface(NSObject)
    ['{A0BDC528-A319-439C-9A95-BD81E94BDD5A}']
    function credentials: NSArray; cdecl;
  end;
  TGADMediationServerConfiguration = class(TOCGenericImport<GADMediationServerConfigurationClass, GADMediationServerConfiguration>) end;

  GADMediationAdConfigurationClass = interface(NSObjectClass)
    ['{BF240130-34C4-48FA-AB54-3FFB9088A612}']
  end;

  GADMediationAdConfiguration = interface(NSObject)
    ['{26486727-B559-4936-9C44-2C66DE78EC97}']
    function bidResponse: NSString; cdecl;
    function childDirectedTreatment: NSNumber; cdecl;
    function credentials: GADMediationCredentials; cdecl;
    function extras: Pointer; cdecl;
    function hasUserLocation: Boolean; cdecl;
    function isTestRequest: Boolean; cdecl;
    function topViewController: UIViewController; cdecl;
    function userLatitude: CGFloat; cdecl;
    function userLocationAccuracyInMeters: CGFloat; cdecl;
    function userLongitude: CGFloat; cdecl;
    function watermark: NSData; cdecl;
  end;
  TGADMediationAdConfiguration = class(TOCGenericImport<GADMediationAdConfigurationClass, GADMediationAdConfiguration>) end;

  GADMediationBannerAd = interface(IObjectiveC)
    ['{6AAAD9C6-4C4F-4122-92B1-953D1EBC929E}']
    procedure changeAdSizeTo(adSize: GADAdSize); cdecl;
    function view: UIView; cdecl;
  end;

  GADMediationBannerAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{BDF1A673-EC57-4DDD-A830-9F4609A83D7F}']
  end;

  GADMediationBannerAdConfiguration = interface(GADMediationAdConfiguration)
    ['{71F82B1A-9DFE-498B-8057-BBC91E82F515}']
    function adSize: GADAdSize; cdecl;
  end;
  TGADMediationBannerAdConfiguration = class(TOCGenericImport<GADMediationBannerAdConfigurationClass, GADMediationBannerAdConfiguration>) end;

  GADMediationInterstitialAd = interface(IObjectiveC)
    ['{B10D7AE9-4EBF-4779-B59C-B75E29C7A35E}']
    procedure presentFromViewController(viewController: UIViewController); cdecl;
  end;

  GADMediationInterstitialAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{1B8176B4-937A-4013-813B-8CA4C6A302BB}']
  end;

  GADMediationInterstitialAdConfiguration = interface(GADMediationAdConfiguration)
    ['{527209CA-68AD-46D7-858E-02C22C1AD512}']
  end;
  TGADMediationInterstitialAdConfiguration = class(TOCGenericImport<GADMediationInterstitialAdConfigurationClass, GADMediationInterstitialAdConfiguration>) end;

  GADMediationNativeAd = interface(IObjectiveC)
    ['{917168AA-B042-420A-8F2E-85CADE689749}']
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
  end;

  GADMediationNativeAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{0C8B0EAC-D9D6-46DF-BB70-D0E10B4C14FF}']
  end;

  GADMediationNativeAdConfiguration = interface(GADMediationAdConfiguration)
    ['{DABFBD31-84DF-4C22-B658-EDA09C1782C4}']
    function options: NSArray; cdecl;
  end;
  TGADMediationNativeAdConfiguration = class(TOCGenericImport<GADMediationNativeAdConfigurationClass, GADMediationNativeAdConfiguration>) end;

  GADMediationRewardedAd = interface(IObjectiveC)
    ['{18428C2D-54BA-485C-AA76-DB4BCE317CD1}']
    procedure presentFromViewController(viewController: UIViewController); cdecl;
  end;

  GADMediationRewardedAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{4285717C-FAE2-4490-8D23-6C6DFCEC0918}']
  end;

  GADMediationRewardedAdConfiguration = interface(GADMediationAdConfiguration)
    ['{B9DCBAED-6338-4320-865F-7B4AB35A2D85}']
  end;
  TGADMediationRewardedAdConfiguration = class(TOCGenericImport<GADMediationRewardedAdConfigurationClass, GADMediationRewardedAdConfiguration>) end;

  GADMediationAdapter = interface(IObjectiveC)
    ['{F608666C-23B6-4E25-8876-1A835960108A}']
    {class} function adSDKVersion: GADVersionNumber; cdecl;
    procedure loadBannerForAdConfiguration(adConfiguration: GADMediationBannerAdConfiguration; completionHandler: GADMediationBannerLoadCompletionHandler); cdecl;
    procedure loadInterstitialForAdConfiguration(adConfiguration: GADMediationInterstitialAdConfiguration; completionHandler: GADMediationInterstitialLoadCompletionHandler); cdecl;
    procedure loadNativeAdForAdConfiguration(adConfiguration: GADMediationNativeAdConfiguration; completionHandler: GADMediationNativeLoadCompletionHandler); cdecl;
    procedure loadRewardedAdForAdConfiguration(adConfiguration: GADMediationRewardedAdConfiguration; completionHandler: GADMediationRewardedLoadCompletionHandler); cdecl;
    function networkExtrasClass: Pointer; cdecl;
    procedure setUpWithConfiguration(configuration: GADMediationServerConfiguration; completionHandler: GADMediationAdapterSetUpCompletionBlock); cdecl;
    function version: GADVersionNumber; cdecl;
  end;

  GADMRewardBasedVideoAdNetworkAdapter = interface(IObjectiveC)
    ['{46E67DF5-20C3-469D-B654-37DC73FA0406}']
    function adapterVersion: NSString; cdecl;
    function initWithGADMAdNetworkConnector(connector: Pointer): Pointer; cdecl;
    [MethodName('initWithRewardBasedVideoAdNetworkConnector:credentials:')]
    function initWithRewardBasedVideoAdNetworkConnector(connector: Pointer; credentials: NSArray): Pointer; overload; cdecl;
    function initWithRewardBasedVideoAdNetworkConnector(connector: Pointer): Pointer; overload; cdecl;
    function networkExtrasClass: Pointer; cdecl;
    procedure presentRewardBasedVideoAdWithRootViewController(viewController: UIViewController); cdecl;
    procedure requestRewardBasedVideoAd; cdecl;
    procedure setUp; cdecl;
    procedure setUpWithUserID(userID: NSString); cdecl;
    procedure stopBeingDelegate; cdecl;
  end;

  GADMRewardBasedVideoAdNetworkConnector = interface(IObjectiveC)
    ['{22F3F270-CCBB-4421-8ADB-8F23B7EE687B}']
    procedure adapterDidCloseRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidCompletePlayingRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidFailToLoadRewardBasedVideoAdwithError(rewardBasedVideoAdAdapter: Pointer; error: NSError); cdecl;
    procedure adapterDidFailToSetUpRewardBasedVideoAdWithError(rewardBasedVideoAdAdapter: Pointer; error: NSError); cdecl;
    procedure adapterDidGetAdClick(adapter: Pointer); cdecl;
    procedure adapterDidOpenRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidReceiveRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidRewardUserWithReward(rewardBasedVideoAd: Pointer; reward: GADAdReward); cdecl;
    procedure adapterDidSetUpRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidStartPlayingRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterWillLeaveApplication(rewardBasedVideoAdAdapter: Pointer); cdecl;
  end;

  GADRTBMediationSignalsConfigurationClass = interface(NSObjectClass)
    ['{58A0CC75-9AB5-4BE1-A6C9-3DE384A795C4}']
  end;

  GADRTBMediationSignalsConfiguration = interface(NSObject)
    ['{C6222D60-12DB-432C-B39E-157E1766BA4B}']
    function credentials: NSArray; cdecl;
  end;
  TGADRTBMediationSignalsConfiguration = class(TOCGenericImport<GADRTBMediationSignalsConfigurationClass, GADRTBMediationSignalsConfiguration>) end;

  GADRTBRequestParametersClass = interface(NSObjectClass)
    ['{E339E5A0-C2BB-4AFE-8EE9-3DD4DAFDAF7B}']
  end;

  GADRTBRequestParameters = interface(NSObject)
    ['{408B3D98-CFEB-4C97-A098-BE5C5CAAC402}']
    function adSize: GADAdSize; cdecl;
    function configuration: GADRTBMediationSignalsConfiguration; cdecl;
    function extras: Pointer; cdecl;
  end;
  TGADRTBRequestParameters = class(TOCGenericImport<GADRTBRequestParametersClass, GADRTBRequestParameters>) end;

  GADRTBAdapter = interface(IObjectiveC)
    ['{CF250129-7F8A-46A3-8D6D-EB2BE33189EC}']
    procedure collectSignalsForRequestParameters(params: GADRTBRequestParameters; completionHandler: GADRTBSignalCompletionHandler); cdecl;
  end;

function GADPortraitAnchoredAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADPortraitAnchoredAdaptiveBannerAdSizeWithWidth';
function GADLandscapeAnchoredAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADLandscapeAnchoredAdaptiveBannerAdSizeWithWidth';
function GADCurrentOrientationAnchoredAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADCurrentOrientationAnchoredAdaptiveBannerAdSizeWithWidth';
function GADAdSizeFromCGSize(size: CGSize): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADAdSizeFromCGSize';
function GADAdSizeFullWidthPortraitWithHeight(height: CGFloat): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADAdSizeFullWidthPortraitWithHeight';
function GADAdSizeFullWidthLandscapeWithHeight(height: CGFloat): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADAdSizeFullWidthLandscapeWithHeight';
function GADAdSizeEqualToSize(size1: GADAdSize; size2: GADAdSize): Boolean; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADAdSizeEqualToSize';
function CGSizeFromGADAdSize(size: GADAdSize): CGSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'CGSizeFromGADAdSize';
function IsGADAdSizeValid(size: GADAdSize): Boolean; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'IsGADAdSizeValid';
function GADAdSizeIsFluid(size: GADAdSize): Boolean; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADAdSizeIsFluid';
function NSStringFromGADAdSize(size: GADAdSize): NSString; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'NSStringFromGADAdSize';
function NSValueFromGADAdSize(size: GADAdSize): NSValue; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'NSValueFromGADAdSize';
function GADAdSizeFromNSValue(value: NSValue): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADAdSizeFromNSValue';
function GADClosestValidSizeForAdSizes(original: GADAdSize; possibleAdSizes: NSArray): GADAdSize; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleMobileAds' name _PU + 'GADClosestValidSizeForAdSizes';

function kGADAdSizeBanner: GADAdSize;
function kGADAdSizeLargeBanner: GADAdSize;
function kGADAdSizeMediumRectangle: GADAdSize;
function kGADAdSizeFullBanner: GADAdSize;
function kGADAdSizeLeaderboard: GADAdSize;

implementation

function kGADAdSizeBanner: GADAdSize;
begin
  Result.size.width := 320;
  Result.size.height := 50;
end;

function kGADAdSizeLargeBanner: GADAdSize;
begin
  Result.size.width := 320;
  Result.size.height := 100;
end;

function kGADAdSizeMediumRectangle: GADAdSize;
begin
  Result.size.width := 300;
  Result.size.height := 250;
end;

function kGADAdSizeFullBanner: GADAdSize;
begin
  Result.size.width := 468;
  Result.size.height := 60;
end;

function kGADAdSizeLeaderboard: GADAdSize;
begin
  Result.size.width := 728;
  Result.size.height := 90;
end;

end.
