{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.AdMob;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Location,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  Androidapi.JNI.Webkit,
  Androidapi.JNI.Widget;

type
// ===== Forward declarations =====

  JAdError = interface;//com.google.android.gms.ads.AdError
  JAdFormat = interface;//com.google.android.gms.ads.AdFormat
  JAdInspectorError = interface;//com.google.android.gms.ads.AdInspectorError
  JAdListener = interface;//com.google.android.gms.ads.AdListener
  JAdLoadCallback = interface;//com.google.android.gms.ads.AdLoadCallback
  JAdRequest = interface;//com.google.android.gms.ads.AdRequest
  JAdRequest_Builder = interface;//com.google.android.gms.ads.AdRequest$Builder
  JAdSize = interface;//com.google.android.gms.ads.AdSize
  JAdValue = interface;//com.google.android.gms.ads.AdValue
  JBaseAdView = interface;//com.google.android.gms.ads.BaseAdView
  JAdView = interface;//com.google.android.gms.ads.AdView
  JFullScreenContentCallback = interface;//com.google.android.gms.ads.FullScreenContentCallback
  JLoadAdError = interface;//com.google.android.gms.ads.LoadAdError
  JMediaContent = interface;//com.google.android.gms.ads.MediaContent
  JMobileAds = interface;//com.google.android.gms.ads.MobileAds
  JMuteThisAdListener = interface;//com.google.android.gms.ads.MuteThisAdListener
  JMuteThisAdReason = interface;//com.google.android.gms.ads.MuteThisAdReason
  JOnAdInspectorClosedListener = interface;//com.google.android.gms.ads.OnAdInspectorClosedListener
  JOnPaidEventListener = interface;//com.google.android.gms.ads.OnPaidEventListener
  JRequestConfiguration = interface;//com.google.android.gms.ads.RequestConfiguration
  JRequestConfiguration_Builder = interface;//com.google.android.gms.ads.RequestConfiguration$Builder
  JResponseInfo = interface;//com.google.android.gms.ads.ResponseInfo
  JVideoController = interface;//com.google.android.gms.ads.VideoController
  JVideoController_VideoLifecycleCallbacks = interface;//com.google.android.gms.ads.VideoController$VideoLifecycleCallbacks
  JVideoOptions = interface;//com.google.android.gms.ads.VideoOptions
  JVideoOptions_Builder = interface;//com.google.android.gms.ads.VideoOptions$Builder
  Jadmanager_AppEventListener = interface;//com.google.android.gms.ads.admanager.AppEventListener
  JMediaView = interface;//com.google.android.gms.ads.formats.MediaView
  JNativeAd_AdChoicesInfo = interface;//com.google.android.gms.ads.formats.NativeAd$AdChoicesInfo
  JNativeAd_Image = interface;//com.google.android.gms.ads.formats.NativeAd$Image
  JNativeAdOptions = interface;//com.google.android.gms.ads.formats.NativeAdOptions
  JNativeAdOptions_Builder = interface;//com.google.android.gms.ads.formats.NativeAdOptions$Builder
  JNativeCustomTemplateAd = interface;//com.google.android.gms.ads.formats.NativeCustomTemplateAd
  JNativeCustomTemplateAd_DisplayOpenMeasurement = interface;//com.google.android.gms.ads.formats.NativeCustomTemplateAd$DisplayOpenMeasurement
  JUnifiedNativeAd = interface;//com.google.android.gms.ads.formats.UnifiedNativeAd
  JUnifiedNativeAd_UnconfirmedClickListener = interface;//com.google.android.gms.ads.formats.UnifiedNativeAd$UnconfirmedClickListener
  //Jformats_zzd = interface;//com.google.android.gms.ads.formats.zzd
  JInitializationStatus = interface;//com.google.android.gms.ads.initialization.InitializationStatus
  JOnInitializationCompleteListener = interface;//com.google.android.gms.ads.initialization.OnInitializationCompleteListener
  Jinterstitial_InterstitialAd = interface;//com.google.android.gms.ads.interstitial.InterstitialAd
  JInterstitialAdLoadCallback = interface;//com.google.android.gms.ads.interstitial.InterstitialAdLoadCallback
  Jmediation_MediationAdRequest = interface;//com.google.android.gms.ads.mediation.MediationAdRequest
  JMediationExtrasReceiver = interface;//com.google.android.gms.ads.mediation.MediationExtrasReceiver
  Jmediation_MediationAdapter = interface;//com.google.android.gms.ads.mediation.MediationAdapter
  Jmediation_MediationBannerAdapter = interface;//com.google.android.gms.ads.mediation.MediationBannerAdapter
  Jmediation_MediationBannerListener = interface;//com.google.android.gms.ads.mediation.MediationBannerListener
  Jmediation_MediationInterstitialAdapter = interface;//com.google.android.gms.ads.mediation.MediationInterstitialAdapter
  Jmediation_MediationInterstitialListener = interface;//com.google.android.gms.ads.mediation.MediationInterstitialListener
  JMediationNativeAdapter = interface;//com.google.android.gms.ads.mediation.MediationNativeAdapter
  JMediationNativeListener = interface;//com.google.android.gms.ads.mediation.MediationNativeListener
  JNativeMediationAdRequest = interface;//com.google.android.gms.ads.mediation.NativeMediationAdRequest
  JNetworkExtras = interface;//com.google.android.gms.ads.mediation.NetworkExtras
  JUnifiedNativeAdMapper = interface;//com.google.android.gms.ads.mediation.UnifiedNativeAdMapper
  //JCustomEventExtras = interface;//com.google.android.gms.ads.mediation.customevent.CustomEventExtras
  Jnativead_NativeAdOptions = interface;//com.google.android.gms.ads.nativead.NativeAdOptions
  Jnativead_NativeAdOptions_Builder = interface;//com.google.android.gms.ads.nativead.NativeAdOptions$Builder
  //Jnativead_zza = interface;//com.google.android.gms.ads.nativead.zza
  JAdInfo = interface;//com.google.android.gms.ads.query.AdInfo
  JQueryInfo = interface;//com.google.android.gms.ads.query.QueryInfo
  JQueryInfoGenerationCallback = interface;//com.google.android.gms.ads.query.QueryInfoGenerationCallback
  JSearchAdRequest = interface;//com.google.android.gms.ads.search.SearchAdRequest
  //Jsearch_zzb = interface;//com.google.android.gms.ads.search.zzb
  //Jsearch_zzc = interface;//com.google.android.gms.ads.search.zzc
  //Jgms_ads_zzc = interface;//com.google.android.gms.ads.zzc
  //Jgms_ads_zzd = interface;//com.google.android.gms.ads.zzd

// ===== Interface declarations =====

  JAdErrorClass = interface(JObjectClass)
    ['{78903148-2EDD-4C82-A5FA-1F0B605B4532}']
    {class} function _GetUNDEFINED_DOMAIN: JString; cdecl;
    {class} function init(i: Integer; string_: JString; string_1: JString): JAdError; cdecl; overload;
    {class} function init(i: Integer; string_: JString; string_1: JString; adError: JAdError): JAdError; cdecl; overload;
    {class} property UNDEFINED_DOMAIN: JString read _GetUNDEFINED_DOMAIN;
  end;

  [JavaSignature('com/google/android/gms/ads/AdError')]
  JAdError = interface(JObject)
    ['{70FB4B50-3EE1-461B-BAFE-FCAA37E89F06}']
    function getCause: JAdError; cdecl;
    function getCode: Integer; cdecl;
    function getDomain: JString; cdecl;
    function getMessage: JString; cdecl;
    function toString: JString; cdecl;
    //function zza: Jzzym; cdecl;
    function zzb: JJSONObject; cdecl;
  end;
  TJAdError = class(TJavaGenericImport<JAdErrorClass, JAdError>) end;

  JAdFormatClass = interface(JEnumClass)
    ['{DD4B1606-B411-4673-B16B-98ECCA2906B3}']
    {class} function _GetBANNER: JAdFormat; cdecl;
    {class} function _GetINTERSTITIAL: JAdFormat; cdecl;
    {class} function _GetNATIVE: JAdFormat; cdecl;
    {class} function _GetREWARDED: JAdFormat; cdecl;
    {class} function _GetREWARDED_INTERSTITIAL: JAdFormat; cdecl;
    {class} function valueOf(string_: JString): JAdFormat; cdecl;
    {class} function values: TJavaObjectArray<JAdFormat>; cdecl;
    {class} property BANNER: JAdFormat read _GetBANNER;
    {class} property INTERSTITIAL: JAdFormat read _GetINTERSTITIAL;
    {class} property NATIVE: JAdFormat read _GetNATIVE;
    {class} property REWARDED: JAdFormat read _GetREWARDED;
    {class} property REWARDED_INTERSTITIAL: JAdFormat read _GetREWARDED_INTERSTITIAL;
  end;

  [JavaSignature('com/google/android/gms/ads/AdFormat')]
  JAdFormat = interface(JEnum)
    ['{6763C777-688B-4DBD-9BC0-1BB934D17B82}']
  end;
  TJAdFormat = class(TJavaGenericImport<JAdFormatClass, JAdFormat>) end;

  JAdInspectorErrorClass = interface(JAdErrorClass)
    ['{F32319A4-7D63-4F83-9506-7C7D1C1FAB92}']
    {class} function _GetERROR_CODE_ALREADY_OPEN: Integer; cdecl;
    {class} function _GetERROR_CODE_FAILED_TO_LOAD: Integer; cdecl;
    {class} function _GetERROR_CODE_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_NOT_IN_TEST_MODE: Integer; cdecl;
    {class} function init(i: Integer; string_: JString; string_1: JString): JAdInspectorError; cdecl;
    {class} property ERROR_CODE_ALREADY_OPEN: Integer read _GetERROR_CODE_ALREADY_OPEN;
    {class} property ERROR_CODE_FAILED_TO_LOAD: Integer read _GetERROR_CODE_FAILED_TO_LOAD;
    {class} property ERROR_CODE_INTERNAL_ERROR: Integer read _GetERROR_CODE_INTERNAL_ERROR;
    {class} property ERROR_CODE_NOT_IN_TEST_MODE: Integer read _GetERROR_CODE_NOT_IN_TEST_MODE;
  end;

  [JavaSignature('com/google/android/gms/ads/AdInspectorError')]
  JAdInspectorError = interface(JAdError)
    ['{E11C06D2-FEE5-4897-B627-712372862BA9}']
    function getCode: Integer; cdecl;
  end;
  TJAdInspectorError = class(TJavaGenericImport<JAdInspectorErrorClass, JAdInspectorError>) end;

  JAdListenerClass = interface(JObjectClass)
    ['{22D3BB99-7931-49A5-B29C-893667600EFD}']
    {class} function init: JAdListener; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdListener')]
  JAdListener = interface(JObject)
    ['{62EE9CD8-6B80-4046-A01D-5AFDFC095925}']
    procedure onAdClicked; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdOpened; cdecl;
  end;
  TJAdListener = class(TJavaGenericImport<JAdListenerClass, JAdListener>) end;

  JAdLoadCallbackClass = interface(JObjectClass)
    ['{5BE0F043-DEDD-48ED-989F-18719C1D5EC8}']
    {class} function init: JAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdLoadCallback')]
  JAdLoadCallback = interface(JObject)
    ['{D4AC674D-D1BD-47C2-8144-C395D7206C60}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    //procedure onAdLoaded(adT: J); cdecl;
  end;
  TJAdLoadCallback = class(TJavaGenericImport<JAdLoadCallbackClass, JAdLoadCallback>) end;

  JAdRequestClass = interface(JObjectClass)
    ['{83BBA0E5-5A1B-4C4E-8174-BF55FB897504}']
    {class} function _GetDEVICE_ID_EMULATOR: JString; cdecl;
    {class} function _GetERROR_CODE_APP_ID_MISSING: Integer; cdecl;
    {class} function _GetERROR_CODE_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_INVALID_REQUEST: Integer; cdecl;
    {class} function _GetERROR_CODE_MEDIATION_NO_FILL: Integer; cdecl;
    {class} function _GetERROR_CODE_NETWORK_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_NO_FILL: Integer; cdecl;
    {class} function _GetERROR_CODE_REQUEST_ID_MISMATCH: Integer; cdecl;
    {class} function _GetGENDER_FEMALE: Integer; cdecl;
    {class} function _GetGENDER_MALE: Integer; cdecl;
    {class} function _GetGENDER_UNKNOWN: Integer; cdecl;
    {class} function _GetMAX_CONTENT_URL_LENGTH: Integer; cdecl;
    {class} property DEVICE_ID_EMULATOR: JString read _GetDEVICE_ID_EMULATOR;
    {class} property ERROR_CODE_APP_ID_MISSING: Integer read _GetERROR_CODE_APP_ID_MISSING;
    {class} property ERROR_CODE_INTERNAL_ERROR: Integer read _GetERROR_CODE_INTERNAL_ERROR;
    {class} property ERROR_CODE_INVALID_REQUEST: Integer read _GetERROR_CODE_INVALID_REQUEST;
    {class} property ERROR_CODE_MEDIATION_NO_FILL: Integer read _GetERROR_CODE_MEDIATION_NO_FILL;
    {class} property ERROR_CODE_NETWORK_ERROR: Integer read _GetERROR_CODE_NETWORK_ERROR;
    {class} property ERROR_CODE_NO_FILL: Integer read _GetERROR_CODE_NO_FILL;
    {class} property ERROR_CODE_REQUEST_ID_MISMATCH: Integer read _GetERROR_CODE_REQUEST_ID_MISMATCH;
    {class} property GENDER_FEMALE: Integer read _GetGENDER_FEMALE;
    {class} property GENDER_MALE: Integer read _GetGENDER_MALE;
    {class} property GENDER_UNKNOWN: Integer read _GetGENDER_UNKNOWN;
    {class} property MAX_CONTENT_URL_LENGTH: Integer read _GetMAX_CONTENT_URL_LENGTH;
  end;

  [JavaSignature('com/google/android/gms/ads/AdRequest')]
  JAdRequest = interface(JObject)
    ['{6A952D89-493B-4EC3-9B2D-419540C3E919}']
    function getContentUrl: JString; cdecl;
    function getCustomEventExtrasBundle(class_: Jlang_Class): JBundle; cdecl;
    function getCustomTargeting: JBundle; cdecl;
    function getKeywords: JSet; cdecl;
    function getLocation: JLocation; cdecl;
    function getNeighboringContentUrls: JList; cdecl;
    function getNetworkExtrasBundle(class_: Jlang_Class): JBundle; cdecl;
    function isTestDevice(context: JContext): Boolean; cdecl;
    //function zza: Jzzacq; cdecl;
  end;
  TJAdRequest = class(TJavaGenericImport<JAdRequestClass, JAdRequest>) end;

  JAdRequest_BuilderClass = interface(JObjectClass)
    ['{31A61751-C547-4A56-AB61-F867EBA5961D}']
    {class} function init: JAdRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdRequest$Builder')]
  JAdRequest_Builder = interface(JObject)
    ['{983C6775-921E-4A57-B560-F364B7D60ADC}']
    function addCustomEventExtrasBundle(class_: Jlang_Class; bundle: JBundle): JAdRequest_Builder; cdecl;
    function addKeyword(string_: JString): JAdRequest_Builder; cdecl;
    function addNetworkExtras(networkExtras: JNetworkExtras): JAdRequest_Builder; cdecl;
    function addNetworkExtrasBundle(class_: Jlang_Class; bundle: JBundle): JAdRequest_Builder; cdecl;
    function build: JAdRequest; cdecl;
    function setAdInfo(adInfo: JAdInfo): JAdRequest_Builder; cdecl;
    function setContentUrl(string_: JString): JAdRequest_Builder; cdecl;
    function setHttpTimeoutMillis(i: Integer): JAdRequest_Builder; cdecl;
    function setLocation(location: JLocation): JAdRequest_Builder; cdecl;
    function setNeighboringContentUrls(list: JList): JAdRequest_Builder; cdecl;
    function setRequestAgent(string_: JString): JAdRequest_Builder; cdecl;
    function zza(string_: JString): JAdRequest_Builder; cdecl;
    function zzb(date: JDate): JAdRequest_Builder; cdecl;
    function zzc(i: Integer): JAdRequest_Builder; cdecl;
    function zzd(b: Boolean): JAdRequest_Builder; cdecl;
    function zze(b: Boolean): JAdRequest_Builder; cdecl;
  end;
  TJAdRequest_Builder = class(TJavaGenericImport<JAdRequest_BuilderClass, JAdRequest_Builder>) end;

  JAdSizeClass = interface(JObjectClass)
    ['{3A15A6B8-8EB9-49BA-9FD9-6B8D507F7707}']
    {class} function _GetAUTO_HEIGHT: Integer; cdecl;
    {class} function _GetBANNER: JAdSize; cdecl;
    {class} function _GetFLUID: JAdSize; cdecl;
    {class} function _GetFULL_BANNER: JAdSize; cdecl;
    {class} function _GetFULL_WIDTH: Integer; cdecl;
    {class} function _GetINVALID: JAdSize; cdecl;
    {class} function _GetLARGE_BANNER: JAdSize; cdecl;
    {class} function _GetLEADERBOARD: JAdSize; cdecl;
    {class} function _GetMEDIUM_RECTANGLE: JAdSize; cdecl;
    {class} function _GetSEARCH: JAdSize; cdecl;
    {class} function _GetSMART_BANNER: JAdSize; cdecl;
    {class} function _GetWIDE_SKYSCRAPER: JAdSize; cdecl;
    {class} function _Getzza: JAdSize; cdecl;
    {class} function getCurrentOrientationAnchoredAdaptiveBannerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getCurrentOrientationInlineAdaptiveBannerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getCurrentOrientationInterscrollerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getInlineAdaptiveBannerAdSize(i: Integer; i1: Integer): JAdSize; cdecl;
    {class} function getLandscapeAnchoredAdaptiveBannerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getLandscapeInlineAdaptiveBannerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getLandscapeInterscrollerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getPortraitAnchoredAdaptiveBannerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getPortraitInlineAdaptiveBannerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function getPortraitInterscrollerAdSize(context: JContext; i: Integer): JAdSize; cdecl;
    {class} function init(i: Integer; i1: Integer): JAdSize; cdecl; overload;
    {class} property AUTO_HEIGHT: Integer read _GetAUTO_HEIGHT;
    {class} property BANNER: JAdSize read _GetBANNER;
    {class} property FLUID: JAdSize read _GetFLUID;
    {class} property FULL_BANNER: JAdSize read _GetFULL_BANNER;
    {class} property FULL_WIDTH: Integer read _GetFULL_WIDTH;
    {class} property INVALID: JAdSize read _GetINVALID;
    {class} property LARGE_BANNER: JAdSize read _GetLARGE_BANNER;
    {class} property LEADERBOARD: JAdSize read _GetLEADERBOARD;
    {class} property MEDIUM_RECTANGLE: JAdSize read _GetMEDIUM_RECTANGLE;
    {class} property SEARCH: JAdSize read _GetSEARCH;
    {class} property SMART_BANNER: JAdSize read _GetSMART_BANNER;
    {class} property WIDE_SKYSCRAPER: JAdSize read _GetWIDE_SKYSCRAPER;
    {class} property zza: JAdSize read _Getzza;
  end;

  [JavaSignature('com/google/android/gms/ads/AdSize')]
  JAdSize = interface(JObject)
    ['{6A30E519-3C74-464C-B145-C0A6A02B3093}']
    function equals(object_: JObject): Boolean; cdecl;
    function getHeight: Integer; cdecl;
    function getHeightInPixels(context: JContext): Integer; cdecl;
    function getWidth: Integer; cdecl;
    function getWidthInPixels(context: JContext): Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isAutoHeight: Boolean; cdecl;
    function isFluid: Boolean; cdecl;
    function isFullWidth: Boolean; cdecl;
    function toString: JString; cdecl;
  end;
  TJAdSize = class(TJavaGenericImport<JAdSizeClass, JAdSize>) end;

  JAdValueClass = interface(JObjectClass)
    ['{BC8C0529-48A0-45D9-A518-8BC55B64785F}']
    {class} function zza(i: Integer; string_: JString; l: Int64): JAdValue; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdValue')]
  JAdValue = interface(JObject)
    ['{1609B647-B6F3-4275-8B89-3E695A6FA9CE}']
    function getCurrencyCode: JString; cdecl;
    function getPrecisionType: Integer; cdecl;
    function getValueMicros: Int64; cdecl;
  end;
  TJAdValue = class(TJavaGenericImport<JAdValueClass, JAdValue>) end;

  JBaseAdViewClass = interface(JViewGroupClass)
    ['{D9CDE901-0B35-480E-81D0-4A45C7D6C856}']
  end;

  [JavaSignature('com/google/android/gms/ads/BaseAdView')]
  JBaseAdView = interface(JViewGroup)
    ['{B279FD2D-B14F-410F-944A-6464CD214C55}']
    procedure destroy; cdecl;
    function getAdListener: JAdListener; cdecl;
    function getAdSize: JAdSize; cdecl;
    function getAdUnitId: JString; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function isLoading: Boolean; cdecl;
    procedure loadAd(adRequest: JAdRequest); cdecl;
    procedure pause; cdecl;
    procedure resume; cdecl;
    procedure setAdListener(adListener: JAdListener); cdecl;
    procedure setAdSize(adSize: JAdSize); cdecl;
    procedure setAdUnitId(string_: JString); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
  end;
  TJBaseAdView = class(TJavaGenericImport<JBaseAdViewClass, JBaseAdView>) end;

  JAdViewClass = interface(JBaseAdViewClass)
    ['{D2A51E0D-437B-4F72-8826-BCCCBF315647}']
    {class} function init(context: JContext): JAdView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JAdView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet; i: Integer): JAdView; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/ads/AdView')]
  JAdView = interface(JBaseAdView)
    ['{58156621-09A0-4178-81FD-9349FD929B6B}']
    function zza: JVideoController; cdecl;
  end;
  TJAdView = class(TJavaGenericImport<JAdViewClass, JAdView>) end;

  JFullScreenContentCallbackClass = interface(JObjectClass)
    ['{4AE5DFFF-D055-4899-B45C-6E0A3FB46294}']
    {class} function _GetERROR_CODE_AD_REUSED: Integer; cdecl;
    {class} function _GetERROR_CODE_APP_NOT_FOREGROUND: Integer; cdecl;
    {class} function _GetERROR_CODE_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_MEDIATION_SHOW_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_NOT_READY: Integer; cdecl;
    {class} function init: JFullScreenContentCallback; cdecl;
    {class} property ERROR_CODE_AD_REUSED: Integer read _GetERROR_CODE_AD_REUSED;
    {class} property ERROR_CODE_APP_NOT_FOREGROUND: Integer read _GetERROR_CODE_APP_NOT_FOREGROUND;
    {class} property ERROR_CODE_INTERNAL_ERROR: Integer read _GetERROR_CODE_INTERNAL_ERROR;
    {class} property ERROR_CODE_MEDIATION_SHOW_ERROR: Integer read _GetERROR_CODE_MEDIATION_SHOW_ERROR;
    {class} property ERROR_CODE_NOT_READY: Integer read _GetERROR_CODE_NOT_READY;
  end;

  [JavaSignature('com/google/android/gms/ads/FullScreenContentCallback')]
  JFullScreenContentCallback = interface(JObject)
    ['{BC938920-F8A2-4DEF-8AD2-58A25158C597}']
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJFullScreenContentCallback = class(TJavaGenericImport<JFullScreenContentCallbackClass, JFullScreenContentCallback>) end;

  JLoadAdErrorClass = interface(JAdErrorClass)
    ['{7BDC9D6B-A9BF-4BA6-97EC-58E5DA6CC6AE}']
    {class} function init(i: Integer; string_: JString; string_1: JString; adError: JAdError; responseInfo: JResponseInfo): JLoadAdError; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/LoadAdError')]
  JLoadAdError = interface(JAdError)
    ['{5D61E232-CDDB-4DA1-8D01-EF23F3A469BD}']
    function getResponseInfo: JResponseInfo; cdecl;
    function toString: JString; cdecl;
    function zzb: JJSONObject; cdecl;
  end;
  TJLoadAdError = class(TJavaGenericImport<JLoadAdErrorClass, JLoadAdError>) end;

  JMediaContentClass = interface(IJavaClass)
    ['{09E7B007-922A-4461-8B89-1C43B073DCB8}']
  end;

  [JavaSignature('com/google/android/gms/ads/MediaContent')]
  JMediaContent = interface(IJavaInstance)
    ['{512105DF-EF07-412F-B3DE-4BE73B1851F4}']
    function getAspectRatio: Single; cdecl;
    function getCurrentTime: Single; cdecl;
    function getDuration: Single; cdecl;
    function getMainImage: JDrawable; cdecl;
    function getVideoController: JVideoController; cdecl;
    function hasVideoContent: Boolean; cdecl;
    procedure setMainImage(drawable: JDrawable); cdecl;
  end;
  TJMediaContent = class(TJavaGenericImport<JMediaContentClass, JMediaContent>) end;

  JMobileAdsClass = interface(JObjectClass)
    ['{3D93A524-A1F1-463F-9892-40911A24B89F}']
    {class} function _GetERROR_DOMAIN: JString; cdecl;
    {class} procedure disableMediationAdapterInitialization(context: JContext); cdecl;
    {class} function getInitializationStatus: JInitializationStatus; cdecl;
    {class} function getRequestConfiguration: JRequestConfiguration; cdecl;
    {class} function getVersionString: JString; cdecl;
    {class} procedure initialize(context: JContext); cdecl; overload;
    {class} procedure initialize(context: JContext; onInitializationCompleteListener: JOnInitializationCompleteListener); cdecl; overload;
    {class} procedure openAdInspector(context: JContext; onAdInspectorClosedListener: JOnAdInspectorClosedListener); cdecl;
    {class} procedure openDebugMenu(context: JContext; string_: JString); cdecl;
    {class} procedure registerRtbAdapter(class_: Jlang_Class); cdecl;
    {class} procedure registerWebView(webView: JWebView); cdecl;
    {class} procedure setAppMuted(b: Boolean); cdecl;
    {class} procedure setAppVolume(f: Single); cdecl;
    {class} procedure setRequestConfiguration(requestConfiguration: JRequestConfiguration); cdecl;
    {class} property ERROR_DOMAIN: JString read _GetERROR_DOMAIN;
  end;

  [JavaSignature('com/google/android/gms/ads/MobileAds')]
  JMobileAds = interface(JObject)
    ['{A32AC866-E4AD-483B-B67B-AA22020169EE}']
  end;
  TJMobileAds = class(TJavaGenericImport<JMobileAdsClass, JMobileAds>) end;

  JMuteThisAdListenerClass = interface(IJavaClass)
    ['{76E9FA8B-CC8C-4E01-8D4D-6C6C66F29A5F}']
  end;

  [JavaSignature('com/google/android/gms/ads/MuteThisAdListener')]
  JMuteThisAdListener = interface(IJavaInstance)
    ['{D02DD772-9B10-4793-8666-4B6413F3EA20}']
    procedure onAdMuted; cdecl;
  end;
  TJMuteThisAdListener = class(TJavaGenericImport<JMuteThisAdListenerClass, JMuteThisAdListener>) end;

  JMuteThisAdReasonClass = interface(IJavaClass)
    ['{78EC50FA-9BBD-4B65-84F1-8ACBC0A36671}']
  end;

  [JavaSignature('com/google/android/gms/ads/MuteThisAdReason')]
  JMuteThisAdReason = interface(IJavaInstance)
    ['{4A38C1D0-3156-4270-8843-72115092C4CF}']
    function getDescription: JString; cdecl;
  end;
  TJMuteThisAdReason = class(TJavaGenericImport<JMuteThisAdReasonClass, JMuteThisAdReason>) end;

  JOnAdInspectorClosedListenerClass = interface(IJavaClass)
    ['{E252ED93-92AA-4C34-8250-AF40380E1126}']
  end;

  [JavaSignature('com/google/android/gms/ads/OnAdInspectorClosedListener')]
  JOnAdInspectorClosedListener = interface(IJavaInstance)
    ['{B54491B4-5480-4A20-AD23-CCB96CB046A0}']
    procedure onAdInspectorClosed(adInspectorError: JAdInspectorError); cdecl;
  end;
  TJOnAdInspectorClosedListener = class(TJavaGenericImport<JOnAdInspectorClosedListenerClass, JOnAdInspectorClosedListener>) end;

  JOnPaidEventListenerClass = interface(IJavaClass)
    ['{C843CA6F-265F-4FAD-92F2-C53EF38C11E6}']
  end;

  [JavaSignature('com/google/android/gms/ads/OnPaidEventListener')]
  JOnPaidEventListener = interface(IJavaInstance)
    ['{BFC46E7C-7DA3-429A-9FBA-38A99261B5F8}']
    procedure onPaidEvent(adValue: JAdValue); cdecl;
  end;
  TJOnPaidEventListener = class(TJavaGenericImport<JOnPaidEventListenerClass, JOnPaidEventListener>) end;

  JRequestConfigurationClass = interface(JObjectClass)
    ['{D7012D76-D59A-434B-B99D-AC35A122F1D4}']
    {class} function _GetMAX_AD_CONTENT_RATING_G: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_MA: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_PG: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_T: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_UNSPECIFIED: JString; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer; cdecl;
    {class} function _GetTAG_FOR_UNDER_AGE_OF_CONSENT_FALSE: Integer; cdecl;
    {class} function _GetTAG_FOR_UNDER_AGE_OF_CONSENT_TRUE: Integer; cdecl;
    {class} function _GetTAG_FOR_UNDER_AGE_OF_CONSENT_UNSPECIFIED: Integer; cdecl;
    {class} function _Getzza: JList; cdecl;
    {class} property MAX_AD_CONTENT_RATING_G: JString read _GetMAX_AD_CONTENT_RATING_G;
    {class} property MAX_AD_CONTENT_RATING_MA: JString read _GetMAX_AD_CONTENT_RATING_MA;
    {class} property MAX_AD_CONTENT_RATING_PG: JString read _GetMAX_AD_CONTENT_RATING_PG;
    {class} property MAX_AD_CONTENT_RATING_T: JString read _GetMAX_AD_CONTENT_RATING_T;
    {class} property MAX_AD_CONTENT_RATING_UNSPECIFIED: JString read _GetMAX_AD_CONTENT_RATING_UNSPECIFIED;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED;
    {class} property TAG_FOR_UNDER_AGE_OF_CONSENT_FALSE: Integer read _GetTAG_FOR_UNDER_AGE_OF_CONSENT_FALSE;
    {class} property TAG_FOR_UNDER_AGE_OF_CONSENT_TRUE: Integer read _GetTAG_FOR_UNDER_AGE_OF_CONSENT_TRUE;
    {class} property TAG_FOR_UNDER_AGE_OF_CONSENT_UNSPECIFIED: Integer read _GetTAG_FOR_UNDER_AGE_OF_CONSENT_UNSPECIFIED;
    {class} property zza: JList read _Getzza;
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration')]
  JRequestConfiguration = interface(JObject)
    ['{87413AEC-7DB6-4352-AB6D-F7BA1060D46E}']
    function getMaxAdContentRating: JString; cdecl;
    function getTagForChildDirectedTreatment: Integer; cdecl;
    function getTagForUnderAgeOfConsent: Integer; cdecl;
    function getTestDeviceIds: JList; cdecl;
    function toBuilder: JRequestConfiguration_Builder; cdecl;
  end;
  TJRequestConfiguration = class(TJavaGenericImport<JRequestConfigurationClass, JRequestConfiguration>) end;

  JRequestConfiguration_BuilderClass = interface(JObjectClass)
    ['{7502C2A5-9CE7-4393-955F-09AA71D147A9}']
    {class} function init: JRequestConfiguration_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration$Builder')]
  JRequestConfiguration_Builder = interface(JObject)
    ['{CDFD4672-779C-49D4-8132-E59394CB0460}']
    function build: JRequestConfiguration; cdecl;
    function setMaxAdContentRating(string_: JString): JRequestConfiguration_Builder; cdecl;
    function setTagForChildDirectedTreatment(i: Integer): JRequestConfiguration_Builder; cdecl;
    function setTagForUnderAgeOfConsent(i: Integer): JRequestConfiguration_Builder; cdecl;
    function setTestDeviceIds(list: JList): JRequestConfiguration_Builder; cdecl;
  end;
  TJRequestConfiguration_Builder = class(TJavaGenericImport<JRequestConfiguration_BuilderClass, JRequestConfiguration_Builder>) end;

  JResponseInfoClass = interface(JObjectClass)
    ['{DA46688A-5AC8-4B3B-9873-8DA7862C8578}']
    {class} //function zzb(zzacg: Jzzacg): JResponseInfo; cdecl;
    {class} //function zzc(zzacg: Jzzacg): JResponseInfo; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/ResponseInfo')]
  JResponseInfo = interface(JObject)
    ['{5847783B-0E9B-45E3-B1E2-9525462C1F50}']
    function getAdapterResponses: JList; cdecl;
    function getMediationAdapterClassName: JString; cdecl;
    function getResponseId: JString; cdecl;
    function toString: JString; cdecl;
    function zza: JJSONObject; cdecl;
  end;
  TJResponseInfo = class(TJavaGenericImport<JResponseInfoClass, JResponseInfo>) end;

  JVideoControllerClass = interface(JObjectClass)
    ['{B40803A8-D03C-4A3C-B7D3-05594134412C}']
    {class} function _GetPLAYBACK_STATE_ENDED: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_PAUSED: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_PLAYING: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_READY: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_UNKNOWN: Integer; cdecl;
    {class} function init: JVideoController; cdecl;
    {class} property PLAYBACK_STATE_ENDED: Integer read _GetPLAYBACK_STATE_ENDED;
    {class} property PLAYBACK_STATE_PAUSED: Integer read _GetPLAYBACK_STATE_PAUSED;
    {class} property PLAYBACK_STATE_PLAYING: Integer read _GetPLAYBACK_STATE_PLAYING;
    {class} property PLAYBACK_STATE_READY: Integer read _GetPLAYBACK_STATE_READY;
    {class} property PLAYBACK_STATE_UNKNOWN: Integer read _GetPLAYBACK_STATE_UNKNOWN;
  end;

  [JavaSignature('com/google/android/gms/ads/VideoController')]
  JVideoController = interface(JObject)
    ['{DB81E433-4245-4D6B-BA13-5A30A7105695}']
    function getPlaybackState: Integer; cdecl;
    function getVideoLifecycleCallbacks: JVideoController_VideoLifecycleCallbacks; cdecl;
    function hasVideoContent: Boolean; cdecl;
    function isClickToExpandEnabled: Boolean; cdecl;
    function isCustomControlsEnabled: Boolean; cdecl;
    function isMuted: Boolean; cdecl;
    procedure mute(b: Boolean); cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure setVideoLifecycleCallbacks(videoLifecycleCallbacks: JVideoController_VideoLifecycleCallbacks); cdecl;
    procedure stop; cdecl;
    //procedure zza(zzacj: Jzzacj); cdecl;
    //function zzb: Jzzacj; cdecl;
  end;
  TJVideoController = class(TJavaGenericImport<JVideoControllerClass, JVideoController>) end;

  JVideoController_VideoLifecycleCallbacksClass = interface(JObjectClass)
    ['{AA3C3330-33F5-4C9D-92AA-64BCF2D5DB37}']
    {class} function init: JVideoController_VideoLifecycleCallbacks; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/VideoController$VideoLifecycleCallbacks')]
  JVideoController_VideoLifecycleCallbacks = interface(JObject)
    ['{749DC977-CCFE-49E6-A135-6AA507FEF9AA}']
    procedure onVideoEnd; cdecl;
    procedure onVideoMute(b: Boolean); cdecl;
    procedure onVideoPause; cdecl;
    procedure onVideoPlay; cdecl;
    procedure onVideoStart; cdecl;
  end;
  TJVideoController_VideoLifecycleCallbacks = class(TJavaGenericImport<JVideoController_VideoLifecycleCallbacksClass, JVideoController_VideoLifecycleCallbacks>) end;

  JVideoOptionsClass = interface(JObjectClass)
    ['{0BAAEE8B-3D12-48E0-9AE1-DE86CBB30E21}']
    {class} //function init(zzady: Jzzady): JVideoOptions; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/ads/VideoOptions')]
  JVideoOptions = interface(JObject)
    ['{670CDDED-CA2E-4479-B895-BF57C42BAF94}']
    function getClickToExpandRequested: Boolean; cdecl;
    function getCustomControlsRequested: Boolean; cdecl;
    function getStartMuted: Boolean; cdecl;
  end;
  TJVideoOptions = class(TJavaGenericImport<JVideoOptionsClass, JVideoOptions>) end;

  JVideoOptions_BuilderClass = interface(JObjectClass)
    ['{A1B136B1-E4D5-4E96-A3B3-0B7324091EA0}']
    {class} function init: JVideoOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/VideoOptions$Builder')]
  JVideoOptions_Builder = interface(JObject)
    ['{23B5B3BB-4F1A-4B7C-BC68-DAB01142ADA4}']
    function build: JVideoOptions; cdecl;
    function setClickToExpandRequested(b: Boolean): JVideoOptions_Builder; cdecl;
    function setCustomControlsRequested(b: Boolean): JVideoOptions_Builder; cdecl;
    function setStartMuted(b: Boolean): JVideoOptions_Builder; cdecl;
  end;
  TJVideoOptions_Builder = class(TJavaGenericImport<JVideoOptions_BuilderClass, JVideoOptions_Builder>) end;

  Jadmanager_AppEventListenerClass = interface(IJavaClass)
    ['{4F0CB424-EC17-4384-8010-8BCCA56E7FE7}']
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AppEventListener')]
  Jadmanager_AppEventListener = interface(IJavaInstance)
    ['{A5E42D4D-B87F-46C5-8EFE-2E9D38098053}']
    procedure onAppEvent(string_: JString; string_1: JString); cdecl;
  end;
  TJadmanager_AppEventListener = class(TJavaGenericImport<Jadmanager_AppEventListenerClass, Jadmanager_AppEventListener>) end;

  JMediaViewClass = interface(JFrameLayoutClass)
    ['{F43BBF8B-F284-44F2-B820-B65DAA019FD1}']
    {class} function init(context: JContext): JMediaView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JMediaView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet; i: Integer): JMediaView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet; i: Integer; i1: Integer): JMediaView; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/MediaView')]
  JMediaView = interface(JFrameLayout)
    ['{AD776CC1-67B5-4A03-AB58-6BC60C099D1F}']
    procedure setImageScaleType(scaleType: JImageView_ScaleType); cdecl;
    procedure setMediaContent(mediaContent: JMediaContent); cdecl;
  end;
  TJMediaView = class(TJavaGenericImport<JMediaViewClass, JMediaView>) end;

  JNativeAd_AdChoicesInfoClass = interface(JObjectClass)
    ['{C2EF5AD3-7427-42C8-B345-E7BDF4EAED7D}']
    {class} function init: JNativeAd_AdChoicesInfo; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAd$AdChoicesInfo')]
  JNativeAd_AdChoicesInfo = interface(JObject)
    ['{EDE6E626-EDDA-4E52-99B1-83D5FB6A03E3}']
    function getImages: JList; cdecl;
    function getText: JCharSequence; cdecl;
  end;
  TJNativeAd_AdChoicesInfo = class(TJavaGenericImport<JNativeAd_AdChoicesInfoClass, JNativeAd_AdChoicesInfo>) end;

  JNativeAd_ImageClass = interface(JObjectClass)
    ['{D215B653-3257-421A-BB4A-48B80C28E8B3}']
    {class} function init: JNativeAd_Image; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAd$Image')]
  JNativeAd_Image = interface(JObject)
    ['{C4AE857D-F323-4B3B-BB9B-189E124739AA}']
    function getDrawable: JDrawable; cdecl;
    function getScale: Double; cdecl;
    function getUri: Jnet_Uri; cdecl;
    function zza: Integer; cdecl;
    function zzb: Integer; cdecl;
  end;
  TJNativeAd_Image = class(TJavaGenericImport<JNativeAd_ImageClass, JNativeAd_Image>) end;

  JNativeAdOptionsClass = interface(JObjectClass)
    ['{3F41235C-F359-441E-B82E-E3B3771A27CC}']
    {class} function _GetADCHOICES_BOTTOM_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_BOTTOM_RIGHT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_RIGHT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_ANY: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer; cdecl;
    {class} function _GetORIENTATION_ANY: Integer; cdecl;
    {class} function _GetORIENTATION_LANDSCAPE: Integer; cdecl;
    {class} function _GetORIENTATION_PORTRAIT: Integer; cdecl;
    {class} property ADCHOICES_BOTTOM_LEFT: Integer read _GetADCHOICES_BOTTOM_LEFT;
    {class} property ADCHOICES_BOTTOM_RIGHT: Integer read _GetADCHOICES_BOTTOM_RIGHT;
    {class} property ADCHOICES_TOP_LEFT: Integer read _GetADCHOICES_TOP_LEFT;
    {class} property ADCHOICES_TOP_RIGHT: Integer read _GetADCHOICES_TOP_RIGHT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_ANY: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_ANY;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN;
    {class} property ORIENTATION_ANY: Integer read _GetORIENTATION_ANY;
    {class} property ORIENTATION_LANDSCAPE: Integer read _GetORIENTATION_LANDSCAPE;
    {class} property ORIENTATION_PORTRAIT: Integer read _GetORIENTATION_PORTRAIT;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAdOptions')]
  JNativeAdOptions = interface(JObject)
    ['{C0175877-FCF9-4AC7-9EC6-DCA0599B924F}']
    function getAdChoicesPlacement: Integer; cdecl;
    function getImageOrientation: Integer; cdecl;
    function getMediaAspectRatio: Integer; cdecl;
    function getVideoOptions: JVideoOptions; cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
    function shouldReturnUrlsForImageAssets: Boolean; cdecl;
    function zza: Boolean; cdecl;
  end;
  TJNativeAdOptions = class(TJavaGenericImport<JNativeAdOptionsClass, JNativeAdOptions>) end;

  JNativeAdOptions_BuilderClass = interface(JObjectClass)
    ['{1B00FF73-D2D9-4EFE-B9FE-C2D4653654DA}']
    {class} function init: JNativeAdOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAdOptions$Builder')]
  JNativeAdOptions_Builder = interface(JObject)
    ['{6F1BE2EE-1D44-4164-B84F-8691F2C86899}']
    function build: JNativeAdOptions; cdecl;
    function setAdChoicesPlacement(i: Integer): JNativeAdOptions_Builder; cdecl;
    function setImageOrientation(i: Integer): JNativeAdOptions_Builder; cdecl;
    function setMediaAspectRatio(i: Integer): JNativeAdOptions_Builder; cdecl;
    function setRequestCustomMuteThisAd(b: Boolean): JNativeAdOptions_Builder; cdecl;
    function setRequestMultipleImages(b: Boolean): JNativeAdOptions_Builder; cdecl;
    function setReturnUrlsForImageAssets(b: Boolean): JNativeAdOptions_Builder; cdecl;
    function setVideoOptions(videoOptions: JVideoOptions): JNativeAdOptions_Builder; cdecl;
  end;
  TJNativeAdOptions_Builder = class(TJavaGenericImport<JNativeAdOptions_BuilderClass, JNativeAdOptions_Builder>) end;

  JNativeCustomTemplateAdClass = interface(IJavaClass)
    ['{B600216B-B510-4684-9F6E-1BBF6C86C3D2}']
    {class} function _GetASSET_NAME_VIDEO: JString; cdecl;
    {class} property ASSET_NAME_VIDEO: JString read _GetASSET_NAME_VIDEO;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeCustomTemplateAd')]
  JNativeCustomTemplateAd = interface(IJavaInstance)
    ['{92016655-285A-4A5D-830E-9F8184E865F8}']
    procedure destroy; cdecl;
    function getAvailableAssetNames: JList; cdecl;
    function getCustomTemplateId: JString; cdecl;
    function getDisplayOpenMeasurement: JNativeCustomTemplateAd_DisplayOpenMeasurement; cdecl;
    function getImage(string_: JString): JNativeAd_Image; cdecl;
    function getText(string_: JString): JCharSequence; cdecl;
    function getVideoController: JVideoController; cdecl;
    function getVideoMediaView: JMediaView; cdecl;
    procedure performClick(string_: JString); cdecl;
    procedure recordImpression; cdecl;
  end;
  TJNativeCustomTemplateAd = class(TJavaGenericImport<JNativeCustomTemplateAdClass, JNativeCustomTemplateAd>) end;

  JNativeCustomTemplateAd_DisplayOpenMeasurementClass = interface(IJavaClass)
    ['{C8786937-0980-405C-BF0A-AD25FD2A60D1}']
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeCustomTemplateAd$DisplayOpenMeasurement')]
  JNativeCustomTemplateAd_DisplayOpenMeasurement = interface(IJavaInstance)
    ['{CC2B8E3C-BB82-4FC9-A22D-9DD7E89A43C1}']
    procedure setView(view: JView); cdecl;
    function start: Boolean; cdecl;
  end;
  TJNativeCustomTemplateAd_DisplayOpenMeasurement = class(TJavaGenericImport<JNativeCustomTemplateAd_DisplayOpenMeasurementClass, JNativeCustomTemplateAd_DisplayOpenMeasurement>) end;

  JUnifiedNativeAdClass = interface(JObjectClass)
    ['{B74CB80C-AFE8-4604-898D-EC17336205E8}']
    {class} function init: JUnifiedNativeAd; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/UnifiedNativeAd')]
  JUnifiedNativeAd = interface(JObject)
    ['{9724B29E-CDCB-4A77-99A5-26DCA009F0E3}']
    procedure cancelUnconfirmedClick; cdecl;
    procedure destroy; cdecl;
    procedure enableCustomClickGesture; cdecl;
    function getAdChoicesInfo: JNativeAd_AdChoicesInfo; cdecl;
    function getAdvertiser: JString; cdecl;
    function getBody: JString; cdecl;
    function getCallToAction: JString; cdecl;
    function getExtras: JBundle; cdecl;
    function getHeadline: JString; cdecl;
    function getIcon: JNativeAd_Image; cdecl;
    function getImages: JList; cdecl;
    function getMediaContent: JMediaContent; cdecl;
    function getMediationAdapterClassName: JString; cdecl;
    function getMuteThisAdReasons: JList; cdecl;
    function getPrice: JString; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function getStarRating: JDouble; cdecl;
    function getStore: JString; cdecl;
    function getVideoController: JVideoController; cdecl;
    function isCustomClickGestureEnabled: Boolean; cdecl;
    function isCustomMuteThisAdEnabled: Boolean; cdecl;
    procedure muteThisAd(muteThisAdReason: JMuteThisAdReason); cdecl;
    procedure performClick(bundle: JBundle); cdecl;
    procedure recordCustomClickGesture; cdecl;
    function recordImpression(bundle: JBundle): Boolean; cdecl;
    procedure reportTouchEvent(bundle: JBundle); cdecl;
    procedure setMuteThisAdListener(muteThisAdListener: JMuteThisAdListener); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setUnconfirmedClickListener(unconfirmedClickListener: JUnifiedNativeAd_UnconfirmedClickListener); cdecl;
    function zza: JObject; cdecl;
  end;
  TJUnifiedNativeAd = class(TJavaGenericImport<JUnifiedNativeAdClass, JUnifiedNativeAd>) end;

  JUnifiedNativeAd_UnconfirmedClickListenerClass = interface(IJavaClass)
    ['{C5DF4247-E5D5-4315-A122-0E51874D0142}']
  end;

  [JavaSignature('com/google/android/gms/ads/formats/UnifiedNativeAd$UnconfirmedClickListener')]
  JUnifiedNativeAd_UnconfirmedClickListener = interface(IJavaInstance)
    ['{7DF4834D-BB3F-425F-917A-1A7F4D523E94}']
    procedure onUnconfirmedClickCancelled; cdecl;
    procedure onUnconfirmedClickReceived(string_: JString); cdecl;
  end;
  TJUnifiedNativeAd_UnconfirmedClickListener = class(TJavaGenericImport<JUnifiedNativeAd_UnconfirmedClickListenerClass, JUnifiedNativeAd_UnconfirmedClickListener>) end;

  // com.google.android.gms.ads.formats.zzd
  JInitializationStatusClass = interface(IJavaClass)
    ['{F186532A-D415-47D8-AC36-0519660DEB9F}']
  end;

  [JavaSignature('com/google/android/gms/ads/initialization/InitializationStatus')]
  JInitializationStatus = interface(IJavaInstance)
    ['{A4BD598D-20DD-4CBC-899D-979FEF554AB5}']
    function getAdapterStatusMap: JMap; cdecl;
  end;
  TJInitializationStatus = class(TJavaGenericImport<JInitializationStatusClass, JInitializationStatus>) end;

  JOnInitializationCompleteListenerClass = interface(IJavaClass)
    ['{F725AF5C-17D7-46EB-B60C-FA1A737EF965}']
  end;

  [JavaSignature('com/google/android/gms/ads/initialization/OnInitializationCompleteListener')]
  JOnInitializationCompleteListener = interface(IJavaInstance)
    ['{78C48F46-BC9A-4BA1-BC4F-E8D5ED29F7D3}']
    procedure onInitializationComplete(initializationStatus: JInitializationStatus); cdecl;
  end;
  TJOnInitializationCompleteListener = class(TJavaGenericImport<JOnInitializationCompleteListenerClass, JOnInitializationCompleteListener>) end;

  Jinterstitial_InterstitialAdClass = interface(JObjectClass)
    ['{F6EE0193-3B01-4F77-B909-9B7E812C4F40}']
    {class} function init: Jinterstitial_InterstitialAd; cdecl;
    {class} procedure load(context: JContext; string_: JString; adRequest: JAdRequest; interstitialAdLoadCallback: JInterstitialAdLoadCallback); cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/interstitial/InterstitialAd')]
  Jinterstitial_InterstitialAd = interface(JObject)
    ['{7117C82F-1926-44A6-A3C5-3E0EA5612BEF}']
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(b: Boolean); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure show(activity: JActivity); cdecl;
  end;
  TJinterstitial_InterstitialAd = class(TJavaGenericImport<Jinterstitial_InterstitialAdClass, Jinterstitial_InterstitialAd>) end;

  JInterstitialAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{3B052E52-8E13-4AFF-84CE-BDF8EAEA94F0}']
    {class} function init: JInterstitialAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/interstitial/InterstitialAdLoadCallback')]
  JInterstitialAdLoadCallback = interface(JAdLoadCallback)
    ['{EC68D59C-EC2D-4B6D-BE43-01CFE9D8553A}']
  end;
  TJInterstitialAdLoadCallback = class(TJavaGenericImport<JInterstitialAdLoadCallbackClass, JInterstitialAdLoadCallback>) end;

  Jmediation_MediationAdRequestClass = interface(IJavaClass)
    ['{9916C74C-6549-4129-9A68-B0F4FDF9D6E6}']
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer; cdecl;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdRequest')]
  Jmediation_MediationAdRequest = interface(IJavaInstance)
    ['{7875E348-E12C-418F-A96B-89B391B411CD}']
    function getBirthday: JDate; cdecl;
    function getGender: Integer; cdecl;
    function getKeywords: JSet; cdecl;
    function getLocation: JLocation; cdecl;
    function isDesignedForFamilies: Boolean; cdecl;
    function isTesting: Boolean; cdecl;
    function taggedForChildDirectedTreatment: Integer; cdecl;
  end;
  TJmediation_MediationAdRequest = class(TJavaGenericImport<Jmediation_MediationAdRequestClass, Jmediation_MediationAdRequest>) end;

  JMediationExtrasReceiverClass = interface(IJavaClass)
    ['{F109A898-BE9C-49D3-B58A-FF22E2DA7726}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationExtrasReceiver')]
  JMediationExtrasReceiver = interface(IJavaInstance)
    ['{95481BFA-B73F-4ED0-B0F7-220CB61F2618}']
  end;
  TJMediationExtrasReceiver = class(TJavaGenericImport<JMediationExtrasReceiverClass, JMediationExtrasReceiver>) end;

  Jmediation_MediationAdapterClass = interface(JMediationExtrasReceiverClass)
    ['{56434ADF-62C9-4FED-B605-4ED88F4151D9}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdapter')]
  Jmediation_MediationAdapter = interface(JMediationExtrasReceiver)
    ['{CDF50BCB-4148-4DEF-8CE7-7DBD8B5A5A25}']
    procedure onDestroy; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
  end;
  TJmediation_MediationAdapter = class(TJavaGenericImport<Jmediation_MediationAdapterClass, Jmediation_MediationAdapter>) end;

  Jmediation_MediationBannerAdapterClass = interface(Jmediation_MediationAdapterClass)
    ['{895F58F0-3EBC-4ED1-803B-9BE1BD2DC781}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationBannerAdapter')]
  Jmediation_MediationBannerAdapter = interface(Jmediation_MediationAdapter)
    ['{A613CE5E-C4D8-4BDE-A9AC-364D0E553C9A}']
    function getBannerView: JView; cdecl;
    procedure requestBannerAd(context: JContext; mediationBannerListener: Jmediation_MediationBannerListener; bundle: JBundle; adSize: JAdSize; mediationAdRequest: Jmediation_MediationAdRequest; bundle1: JBundle); cdecl;
  end;
  TJmediation_MediationBannerAdapter = class(TJavaGenericImport<Jmediation_MediationBannerAdapterClass, Jmediation_MediationBannerAdapter>) end;

  Jmediation_MediationBannerListenerClass = interface(IJavaClass)
    ['{A0C1233D-2CEF-4135-91F1-F5435230A5E4}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationBannerListener')]
  Jmediation_MediationBannerListener = interface(IJavaInstance)
    ['{B5EB31F0-707E-4935-AA57-6915280B75CC}']
    procedure onAdClicked(mediationBannerAdapter: Jmediation_MediationBannerAdapter); cdecl;
    procedure onAdClosed(mediationBannerAdapter: Jmediation_MediationBannerAdapter); cdecl;
    procedure onAdFailedToLoad(mediationBannerAdapter: Jmediation_MediationBannerAdapter; adError: JAdError); cdecl; overload;
    procedure onAdFailedToLoad(mediationBannerAdapter: Jmediation_MediationBannerAdapter; i: Integer); cdecl; overload;
    procedure onAdLeftApplication(mediationBannerAdapter: Jmediation_MediationBannerAdapter); cdecl;
    procedure onAdLoaded(mediationBannerAdapter: Jmediation_MediationBannerAdapter); cdecl;
    procedure onAdOpened(mediationBannerAdapter: Jmediation_MediationBannerAdapter); cdecl;
    procedure zza(mediationBannerAdapter: Jmediation_MediationBannerAdapter; string_: JString; string_1: JString); cdecl;
  end;
  TJmediation_MediationBannerListener = class(TJavaGenericImport<Jmediation_MediationBannerListenerClass, Jmediation_MediationBannerListener>) end;

  Jmediation_MediationInterstitialAdapterClass = interface(Jmediation_MediationAdapterClass)
    ['{5F4FAC50-91FC-4438-A712-0DE59B68DF0B}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterstitialAdapter')]
  Jmediation_MediationInterstitialAdapter = interface(Jmediation_MediationAdapter)
    ['{287270DB-58DC-4CB0-86FF-CFF530C09F1F}']
    procedure requestInterstitialAd(context: JContext; mediationInterstitialListener: Jmediation_MediationInterstitialListener; bundle: JBundle; mediationAdRequest: Jmediation_MediationAdRequest; bundle1: JBundle); cdecl;
    procedure showInterstitial; cdecl;
  end;
  TJmediation_MediationInterstitialAdapter = class(TJavaGenericImport<Jmediation_MediationInterstitialAdapterClass, Jmediation_MediationInterstitialAdapter>) end;

  Jmediation_MediationInterstitialListenerClass = interface(IJavaClass)
    ['{F6CD7C91-9206-47F0-B83C-72A6470E16FF}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterstitialListener')]
  Jmediation_MediationInterstitialListener = interface(IJavaInstance)
    ['{3BA473E5-0F3D-4DF5-88DB-6502B4886381}']
    procedure onAdClicked(mediationInterstitialAdapter: Jmediation_MediationInterstitialAdapter); cdecl;
    procedure onAdClosed(mediationInterstitialAdapter: Jmediation_MediationInterstitialAdapter); cdecl;
    procedure onAdFailedToLoad(mediationInterstitialAdapter: Jmediation_MediationInterstitialAdapter; adError: JAdError); cdecl; overload;
    procedure onAdFailedToLoad(mediationInterstitialAdapter: Jmediation_MediationInterstitialAdapter; i: Integer); cdecl; overload;
    procedure onAdLeftApplication(mediationInterstitialAdapter: Jmediation_MediationInterstitialAdapter); cdecl;
    procedure onAdLoaded(mediationInterstitialAdapter: Jmediation_MediationInterstitialAdapter); cdecl;
    procedure onAdOpened(mediationInterstitialAdapter: Jmediation_MediationInterstitialAdapter); cdecl;
  end;
  TJmediation_MediationInterstitialListener = class(TJavaGenericImport<Jmediation_MediationInterstitialListenerClass, Jmediation_MediationInterstitialListener>) end;

  JMediationNativeAdapterClass = interface(Jmediation_MediationAdapterClass)
    ['{58DD5DEE-A850-4E8E-A85F-4718C9184750}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationNativeAdapter')]
  JMediationNativeAdapter = interface(Jmediation_MediationAdapter)
    ['{60334DB6-7D57-4D7B-A21A-0B130FAA39B4}']
    procedure requestNativeAd(context: JContext; mediationNativeListener: JMediationNativeListener; bundle: JBundle; nativeMediationAdRequest: JNativeMediationAdRequest; bundle1: JBundle); cdecl;
  end;
  TJMediationNativeAdapter = class(TJavaGenericImport<JMediationNativeAdapterClass, JMediationNativeAdapter>) end;

  JMediationNativeListenerClass = interface(IJavaClass)
    ['{A4516C16-A769-42C0-BF80-F11FC74C1751}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationNativeListener')]
  JMediationNativeListener = interface(IJavaInstance)
    ['{C536554B-C650-4203-BBB2-DE64FBF92177}']
    procedure onAdClicked(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdClosed(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdFailedToLoad(mediationNativeAdapter: JMediationNativeAdapter; adError: JAdError); cdecl; overload;
    procedure onAdFailedToLoad(mediationNativeAdapter: JMediationNativeAdapter; i: Integer); cdecl; overload;
    procedure onAdImpression(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdLeftApplication(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdLoaded(mediationNativeAdapter: JMediationNativeAdapter; unifiedNativeAdMapper: JUnifiedNativeAdMapper); cdecl;
    procedure onAdOpened(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onVideoEnd(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure zzb(mediationNativeAdapter: JMediationNativeAdapter; nativeCustomTemplateAd: JNativeCustomTemplateAd); cdecl;
    procedure zzc(mediationNativeAdapter: JMediationNativeAdapter; nativeCustomTemplateAd: JNativeCustomTemplateAd; string_: JString); cdecl;
  end;
  TJMediationNativeListener = class(TJavaGenericImport<JMediationNativeListenerClass, JMediationNativeListener>) end;

  JNativeMediationAdRequestClass = interface(Jmediation_MediationAdRequestClass)
    ['{F0458E92-FECC-40AA-BBF1-2D9E78B8002A}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/NativeMediationAdRequest')]
  JNativeMediationAdRequest = interface(Jmediation_MediationAdRequest)
    ['{D153DC8C-8E0D-4C0D-B989-DE8E86FF3C70}']
    function getAdVolume: Single; cdecl;
    function getNativeAdOptions: JNativeAdOptions; cdecl;
    function getNativeAdRequestOptions: Jnativead_NativeAdOptions; cdecl;
    function isAdMuted: Boolean; cdecl;
    function isUnifiedNativeAdRequested: Boolean; cdecl;
    function zza: Boolean; cdecl;
    function zzb: JMap; cdecl;
  end;
  TJNativeMediationAdRequest = class(TJavaGenericImport<JNativeMediationAdRequestClass, JNativeMediationAdRequest>) end;

  JNetworkExtrasClass = interface(IJavaClass)
    ['{CD85A32A-AD18-4FEB-9378-775086001CBC}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/NetworkExtras')]
  JNetworkExtras = interface(IJavaInstance)
    ['{87A70156-1EB5-4B91-AB24-47468B2E2FB1}']
  end;
  TJNetworkExtras = class(TJavaGenericImport<JNetworkExtrasClass, JNetworkExtras>) end;

  JUnifiedNativeAdMapperClass = interface(JObjectClass)
    ['{84228FFB-6E69-4419-BF32-207A901064F3}']
    {class} function init: JUnifiedNativeAdMapper; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/UnifiedNativeAdMapper')]
  JUnifiedNativeAdMapper = interface(JObject)
    ['{E2A26653-A7C3-412E-84B6-7CBDD16F8A34}']
    function getAdChoicesContent: JView; cdecl;
    function getAdvertiser: JString; cdecl;
    function getBody: JString; cdecl;
    function getCallToAction: JString; cdecl;
    function getCurrentTime: Single; cdecl;
    function getDuration: Single; cdecl;
    function getExtras: JBundle; cdecl;
    function getHeadline: JString; cdecl;
    function getIcon: JNativeAd_Image; cdecl;
    function getImages: JList; cdecl;
    function getMediaContentAspectRatio: Single; cdecl;
    function getOverrideClickHandling: Boolean; cdecl;
    function getOverrideImpressionRecording: Boolean; cdecl;
    function getPrice: JString; cdecl;
    function getStarRating: JDouble; cdecl;
    function getStore: JString; cdecl;
    procedure handleClick(view: JView); cdecl;
    function hasVideoContent: Boolean; cdecl;
    procedure recordImpression; cdecl;
    procedure setAdChoicesContent(view: JView); cdecl;
    procedure setAdvertiser(string_: JString); cdecl;
    procedure setBody(string_: JString); cdecl;
    procedure setCallToAction(string_: JString); cdecl;
    procedure setExtras(bundle: JBundle); cdecl;
    procedure setHasVideoContent(b: Boolean); cdecl;
    procedure setHeadline(string_: JString); cdecl;
    procedure setIcon(image: JNativeAd_Image); cdecl;
    procedure setImages(list: JList); cdecl;
    procedure setMediaContentAspectRatio(f: Single); cdecl;
    procedure setMediaView(view: JView); cdecl;
    procedure setOverrideClickHandling(b: Boolean); cdecl;
    procedure setOverrideImpressionRecording(b: Boolean); cdecl;
    procedure setPrice(string_: JString); cdecl;
    procedure setStarRating(double: JDouble); cdecl;
    procedure setStore(string_: JString); cdecl;
    procedure trackViews(view: JView; map: JMap; map1: JMap); cdecl;
    procedure untrackView(view: JView); cdecl;
    procedure zza(videoController: JVideoController); cdecl;
    procedure zzb(object_: JObject); cdecl;
    function zzc: JVideoController; cdecl;
    function zzd: JView; cdecl;
    function zze: JObject; cdecl;
  end;
  TJUnifiedNativeAdMapper = class(TJavaGenericImport<JUnifiedNativeAdMapperClass, JUnifiedNativeAdMapper>) end;

  // com.google.android.gms.ads.mediation.customevent.CustomEventExtras
  Jnativead_NativeAdOptionsClass = interface(JObjectClass)
    ['{EC60EF29-BB6D-40FA-9F7D-26D388D2E5C4}']
    {class} function _GetADCHOICES_BOTTOM_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_BOTTOM_RIGHT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_RIGHT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_ANY: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer; cdecl;
    {class} property ADCHOICES_BOTTOM_LEFT: Integer read _GetADCHOICES_BOTTOM_LEFT;
    {class} property ADCHOICES_BOTTOM_RIGHT: Integer read _GetADCHOICES_BOTTOM_RIGHT;
    {class} property ADCHOICES_TOP_LEFT: Integer read _GetADCHOICES_TOP_LEFT;
    {class} property ADCHOICES_TOP_RIGHT: Integer read _GetADCHOICES_TOP_RIGHT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_ANY: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_ANY;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdOptions')]
  Jnativead_NativeAdOptions = interface(JObject)
    ['{76D8EC06-85C9-4B3B-9580-CDCEDD3CCC28}']
    function getAdChoicesPlacement: Integer; cdecl;
    function getMediaAspectRatio: Integer; cdecl;
    function getVideoOptions: JVideoOptions; cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
    function shouldReturnUrlsForImageAssets: Boolean; cdecl;
    function zza: Boolean; cdecl;
  end;
  TJnativead_NativeAdOptions = class(TJavaGenericImport<Jnativead_NativeAdOptionsClass, Jnativead_NativeAdOptions>) end;

  Jnativead_NativeAdOptions_BuilderClass = interface(JObjectClass)
    ['{AE7A5117-7923-4361-9D18-889332329120}']
    {class} function init: Jnativead_NativeAdOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdOptions$Builder')]
  Jnativead_NativeAdOptions_Builder = interface(JObject)
    ['{858BA149-7041-4A34-97B2-E0EA172715E6}']
    function build: Jnativead_NativeAdOptions; cdecl;
    function setAdChoicesPlacement(i: Integer): Jnativead_NativeAdOptions_Builder; cdecl;
    function setMediaAspectRatio(i: Integer): Jnativead_NativeAdOptions_Builder; cdecl;
    function setRequestCustomMuteThisAd(b: Boolean): Jnativead_NativeAdOptions_Builder; cdecl;
    function setRequestMultipleImages(b: Boolean): Jnativead_NativeAdOptions_Builder; cdecl;
    function setReturnUrlsForImageAssets(b: Boolean): Jnativead_NativeAdOptions_Builder; cdecl;
    function setVideoOptions(videoOptions: JVideoOptions): Jnativead_NativeAdOptions_Builder; cdecl;
  end;
  TJnativead_NativeAdOptions_Builder = class(TJavaGenericImport<Jnativead_NativeAdOptions_BuilderClass, Jnativead_NativeAdOptions_Builder>) end;

  // com.google.android.gms.ads.nativead.zza
  JAdInfoClass = interface(JObjectClass)
    ['{24967D3A-A736-4716-9475-35D3FF1AB3BD}']
    {class} function getRequestId(string_: JString): JString; cdecl;
    {class} function init(queryInfo: JQueryInfo; string_: JString): JAdInfo; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/query/AdInfo')]
  JAdInfo = interface(JObject)
    ['{1EA6BC2E-92E4-4C7F-A01C-09E559A813FE}']
    function getAdString: JString; cdecl;
    function getQueryInfo: JQueryInfo; cdecl;
  end;
  TJAdInfo = class(TJavaGenericImport<JAdInfoClass, JAdInfo>) end;

  JQueryInfoClass = interface(JObjectClass)
    ['{E9BE8FEE-B08F-4E0A-870A-0212A0079507}']
    {class} procedure generate(context: JContext; adFormat: JAdFormat; adRequest: JAdRequest; queryInfoGenerationCallback: JQueryInfoGenerationCallback); cdecl;
    {class} //function init(zzadb: Jzzadb): JQueryInfo; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/query/QueryInfo')]
  JQueryInfo = interface(JObject)
    ['{352DAABE-58FC-469B-929F-7D6BA4633E8A}']
    function getQuery: JString; cdecl;
    function getQueryBundle: JBundle; cdecl;
    function getRequestId: JString; cdecl;
  end;
  TJQueryInfo = class(TJavaGenericImport<JQueryInfoClass, JQueryInfo>) end;

  JQueryInfoGenerationCallbackClass = interface(JObjectClass)
    ['{20B5BBE7-8403-4FF4-806B-B792768BC7BB}']
    {class} function init: JQueryInfoGenerationCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/query/QueryInfoGenerationCallback')]
  JQueryInfoGenerationCallback = interface(JObject)
    ['{6B9F60F0-23A6-4A5E-A62B-65F8697403AE}']
    procedure onFailure(string_: JString); cdecl;
    procedure onSuccess(queryInfo: JQueryInfo); cdecl;
  end;
  TJQueryInfoGenerationCallback = class(TJavaGenericImport<JQueryInfoGenerationCallbackClass, JQueryInfoGenerationCallback>) end;

  JSearchAdRequestClass = interface(JObjectClass)
    ['{31CC1220-F7EC-44D3-9805-80A0C42377CE}']
    {class} function _GetBORDER_TYPE_DASHED: Integer; cdecl;
    {class} function _GetBORDER_TYPE_DOTTED: Integer; cdecl;
    {class} function _GetBORDER_TYPE_NONE: Integer; cdecl;
    {class} function _GetBORDER_TYPE_SOLID: Integer; cdecl;
    {class} function _GetCALL_BUTTON_COLOR_DARK: Integer; cdecl;
    {class} function _GetCALL_BUTTON_COLOR_LIGHT: Integer; cdecl;
    {class} function _GetCALL_BUTTON_COLOR_MEDIUM: Integer; cdecl;
    {class} function _GetDEVICE_ID_EMULATOR: JString; cdecl;
    {class} function _GetERROR_CODE_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_INVALID_REQUEST: Integer; cdecl;
    {class} function _GetERROR_CODE_NETWORK_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_NO_FILL: Integer; cdecl;
    {class} property BORDER_TYPE_DASHED: Integer read _GetBORDER_TYPE_DASHED;
    {class} property BORDER_TYPE_DOTTED: Integer read _GetBORDER_TYPE_DOTTED;
    {class} property BORDER_TYPE_NONE: Integer read _GetBORDER_TYPE_NONE;
    {class} property BORDER_TYPE_SOLID: Integer read _GetBORDER_TYPE_SOLID;
    {class} property CALL_BUTTON_COLOR_DARK: Integer read _GetCALL_BUTTON_COLOR_DARK;
    {class} property CALL_BUTTON_COLOR_LIGHT: Integer read _GetCALL_BUTTON_COLOR_LIGHT;
    {class} property CALL_BUTTON_COLOR_MEDIUM: Integer read _GetCALL_BUTTON_COLOR_MEDIUM;
    {class} property DEVICE_ID_EMULATOR: JString read _GetDEVICE_ID_EMULATOR;
    {class} property ERROR_CODE_INTERNAL_ERROR: Integer read _GetERROR_CODE_INTERNAL_ERROR;
    {class} property ERROR_CODE_INVALID_REQUEST: Integer read _GetERROR_CODE_INVALID_REQUEST;
    {class} property ERROR_CODE_NETWORK_ERROR: Integer read _GetERROR_CODE_NETWORK_ERROR;
    {class} property ERROR_CODE_NO_FILL: Integer read _GetERROR_CODE_NO_FILL;
  end;

  [JavaSignature('com/google/android/gms/ads/search/SearchAdRequest')]
  JSearchAdRequest = interface(JObject)
    ['{7105D858-7D2D-4105-A415-4B9C3B187692}']
    function getAnchorTextColor: Integer; cdecl;
    function getBackgroundColor: Integer; cdecl;
    function getBackgroundGradientBottom: Integer; cdecl;
    function getBackgroundGradientTop: Integer; cdecl;
    function getBorderColor: Integer; cdecl;
    function getBorderThickness: Integer; cdecl;
    function getBorderType: Integer; cdecl;
    function getCallButtonColor: Integer; cdecl;
    function getCustomChannels: JString; cdecl;
    function getCustomEventExtrasBundle(class_: Jlang_Class): JBundle; cdecl;
    function getDescriptionTextColor: Integer; cdecl;
    function getFontFace: JString; cdecl;
    function getHeaderTextColor: Integer; cdecl;
    function getHeaderTextSize: Integer; cdecl;
    function getLocation: JLocation; cdecl;
    function getNetworkExtras(class_: Jlang_Class): JNetworkExtras; cdecl;
    function getNetworkExtrasBundle(class_: Jlang_Class): JBundle; cdecl;
    function getQuery: JString; cdecl;
    function isTestDevice(context: JContext): Boolean; cdecl;
  end;
  TJSearchAdRequest = class(TJavaGenericImport<JSearchAdRequestClass, JSearchAdRequest>) end;

  // com.google.android.gms.ads.search.zzb
  // com.google.android.gms.ads.search.zzc
  // com.google.android.gms.ads.zzc
  // com.google.android.gms.ads.zzd
implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdError', TypeInfo(Androidapi.JNI.AdMob.JAdError));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdFormat', TypeInfo(Androidapi.JNI.AdMob.JAdFormat));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdInspectorError', TypeInfo(Androidapi.JNI.AdMob.JAdInspectorError));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdListener', TypeInfo(Androidapi.JNI.AdMob.JAdListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdLoadCallback', TypeInfo(Androidapi.JNI.AdMob.JAdLoadCallback));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdRequest', TypeInfo(Androidapi.JNI.AdMob.JAdRequest));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdRequest_Builder', TypeInfo(Androidapi.JNI.AdMob.JAdRequest_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdSize', TypeInfo(Androidapi.JNI.AdMob.JAdSize));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdValue', TypeInfo(Androidapi.JNI.AdMob.JAdValue));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JBaseAdView', TypeInfo(Androidapi.JNI.AdMob.JBaseAdView));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdView', TypeInfo(Androidapi.JNI.AdMob.JAdView));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JFullScreenContentCallback', TypeInfo(Androidapi.JNI.AdMob.JFullScreenContentCallback));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JLoadAdError', TypeInfo(Androidapi.JNI.AdMob.JLoadAdError));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMediaContent', TypeInfo(Androidapi.JNI.AdMob.JMediaContent));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMobileAds', TypeInfo(Androidapi.JNI.AdMob.JMobileAds));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMuteThisAdListener', TypeInfo(Androidapi.JNI.AdMob.JMuteThisAdListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMuteThisAdReason', TypeInfo(Androidapi.JNI.AdMob.JMuteThisAdReason));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JOnAdInspectorClosedListener', TypeInfo(Androidapi.JNI.AdMob.JOnAdInspectorClosedListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JOnPaidEventListener', TypeInfo(Androidapi.JNI.AdMob.JOnPaidEventListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JRequestConfiguration', TypeInfo(Androidapi.JNI.AdMob.JRequestConfiguration));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JRequestConfiguration_Builder', TypeInfo(Androidapi.JNI.AdMob.JRequestConfiguration_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JResponseInfo', TypeInfo(Androidapi.JNI.AdMob.JResponseInfo));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JVideoController', TypeInfo(Androidapi.JNI.AdMob.JVideoController));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JVideoController_VideoLifecycleCallbacks', TypeInfo(Androidapi.JNI.AdMob.JVideoController_VideoLifecycleCallbacks));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JVideoOptions', TypeInfo(Androidapi.JNI.AdMob.JVideoOptions));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JVideoOptions_Builder', TypeInfo(Androidapi.JNI.AdMob.JVideoOptions_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jadmanager_AppEventListener', TypeInfo(Androidapi.JNI.AdMob.Jadmanager_AppEventListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMediaView', TypeInfo(Androidapi.JNI.AdMob.JMediaView));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNativeAd_AdChoicesInfo', TypeInfo(Androidapi.JNI.AdMob.JNativeAd_AdChoicesInfo));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNativeAd_Image', TypeInfo(Androidapi.JNI.AdMob.JNativeAd_Image));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNativeAdOptions', TypeInfo(Androidapi.JNI.AdMob.JNativeAdOptions));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNativeAdOptions_Builder', TypeInfo(Androidapi.JNI.AdMob.JNativeAdOptions_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNativeCustomTemplateAd', TypeInfo(Androidapi.JNI.AdMob.JNativeCustomTemplateAd));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNativeCustomTemplateAd_DisplayOpenMeasurement', TypeInfo(Androidapi.JNI.AdMob.JNativeCustomTemplateAd_DisplayOpenMeasurement));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JUnifiedNativeAd', TypeInfo(Androidapi.JNI.AdMob.JUnifiedNativeAd));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JUnifiedNativeAd_UnconfirmedClickListener', TypeInfo(Androidapi.JNI.AdMob.JUnifiedNativeAd_UnconfirmedClickListener));
  //TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jformats_zzd', TypeInfo(Androidapi.JNI.AdMob.Jformats_zzd));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JInitializationStatus', TypeInfo(Androidapi.JNI.AdMob.JInitializationStatus));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JOnInitializationCompleteListener', TypeInfo(Androidapi.JNI.AdMob.JOnInitializationCompleteListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jinterstitial_InterstitialAd', TypeInfo(Androidapi.JNI.AdMob.Jinterstitial_InterstitialAd));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JInterstitialAdLoadCallback', TypeInfo(Androidapi.JNI.AdMob.JInterstitialAdLoadCallback));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jmediation_MediationAdRequest', TypeInfo(Androidapi.JNI.AdMob.Jmediation_MediationAdRequest));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMediationExtrasReceiver', TypeInfo(Androidapi.JNI.AdMob.JMediationExtrasReceiver));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jmediation_MediationAdapter', TypeInfo(Androidapi.JNI.AdMob.Jmediation_MediationAdapter));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jmediation_MediationBannerAdapter', TypeInfo(Androidapi.JNI.AdMob.Jmediation_MediationBannerAdapter));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jmediation_MediationBannerListener', TypeInfo(Androidapi.JNI.AdMob.Jmediation_MediationBannerListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jmediation_MediationInterstitialAdapter', TypeInfo(Androidapi.JNI.AdMob.Jmediation_MediationInterstitialAdapter));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jmediation_MediationInterstitialListener', TypeInfo(Androidapi.JNI.AdMob.Jmediation_MediationInterstitialListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMediationNativeAdapter', TypeInfo(Androidapi.JNI.AdMob.JMediationNativeAdapter));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JMediationNativeListener', TypeInfo(Androidapi.JNI.AdMob.JMediationNativeListener));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNativeMediationAdRequest', TypeInfo(Androidapi.JNI.AdMob.JNativeMediationAdRequest));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JNetworkExtras', TypeInfo(Androidapi.JNI.AdMob.JNetworkExtras));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JUnifiedNativeAdMapper', TypeInfo(Androidapi.JNI.AdMob.JUnifiedNativeAdMapper));
  //TRegTypes.RegisterType('Androidapi.JNI.AdMob.JCustomEventExtras', TypeInfo(Androidapi.JNI.AdMob.JCustomEventExtras));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jnativead_NativeAdOptions', TypeInfo(Androidapi.JNI.AdMob.Jnativead_NativeAdOptions));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jnativead_NativeAdOptions_Builder', TypeInfo(Androidapi.JNI.AdMob.Jnativead_NativeAdOptions_Builder));
  //TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jnativead_zza', TypeInfo(Androidapi.JNI.AdMob.Jnativead_zza));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JAdInfo', TypeInfo(Androidapi.JNI.AdMob.JAdInfo));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JQueryInfo', TypeInfo(Androidapi.JNI.AdMob.JQueryInfo));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JQueryInfoGenerationCallback', TypeInfo(Androidapi.JNI.AdMob.JQueryInfoGenerationCallback));
  TRegTypes.RegisterType('Androidapi.JNI.AdMob.JSearchAdRequest', TypeInfo(Androidapi.JNI.AdMob.JSearchAdRequest));
  //TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jsearch_zzb', TypeInfo(Androidapi.JNI.AdMob.Jsearch_zzb));
  //TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jsearch_zzc', TypeInfo(Androidapi.JNI.AdMob.Jsearch_zzc));
  //TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jgms_ads_zzc', TypeInfo(Androidapi.JNI.AdMob.Jgms_ads_zzc));
  //TRegTypes.RegisterType('Androidapi.JNI.AdMob.Jgms_ads_zzd', TypeInfo(Androidapi.JNI.AdMob.Jgms_ads_zzd));
end;

initialization
  RegisterTypes;
end.


