{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.PlayServices;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Support;

type
// ===== Forward declarations =====

  JSafeParcelable = interface;//com.google.android.gms.common.internal.safeparcel.SafeParcelable
  JAbstractSafeParcelable = interface;//com.google.android.gms.common.internal.safeparcel.AbstractSafeParcelable
  JConnectionResult = interface;//com.google.android.gms.common.ConnectionResult
  JGooglePlayServicesUtilLight = interface;//com.google.android.gms.common.GooglePlayServicesUtilLight
  JGooglePlayServicesUtil = interface;//com.google.android.gms.common.GooglePlayServicesUtil
  JLifecycleActivity = interface;//com.google.android.gms.common.api.internal.LifecycleActivity
  JLifecycleCallback = interface;//com.google.android.gms.common.api.internal.LifecycleCallback
  JLifecycleFragment = interface;//com.google.android.gms.common.api.internal.LifecycleFragment
  JIObjectWrapper = interface;//com.google.android.gms.dynamic.IObjectWrapper
  //Jzzaae = interface;//com.google.android.gms.internal.ads.zzaae
  //Jzzaah = interface;//com.google.android.gms.internal.ads.zzaah
  //Jzzaak = interface;//com.google.android.gms.internal.ads.zzaak
  //Jzzaau = interface;//com.google.android.gms.internal.ads.zzaau
  //Jzzaay = interface;//com.google.android.gms.internal.ads.zzaay
  //Jzzabb = interface;//com.google.android.gms.internal.ads.zzabb
  //Jzzabf = interface;//com.google.android.gms.internal.ads.zzabf
  //Jzzabi = interface;//com.google.android.gms.internal.ads.zzabi
  //Jzzacd = interface;//com.google.android.gms.internal.ads.zzacd
  //Jzzacg = interface;//com.google.android.gms.internal.ads.zzacg
  //Jzzacj = interface;//com.google.android.gms.internal.ads.zzacj
  //Jzzacm = interface;//com.google.android.gms.internal.ads.zzacm
  //Jzzacn = interface;//com.google.android.gms.internal.ads.zzacn
  //Jzzacp = interface;//com.google.android.gms.internal.ads.zzacp
  //Jzzacq = interface;//com.google.android.gms.internal.ads.zzacq
  //Jzzacs = interface;//com.google.android.gms.internal.ads.zzacs
  //Jzzadb = interface;//com.google.android.gms.internal.ads.zzadb
  //Jzzadu = interface;//com.google.android.gms.internal.ads.zzadu
  //Jzzady = interface;//com.google.android.gms.internal.ads.zzady
  //Jzzafi = interface;//com.google.android.gms.internal.ads.zzafi
  //Jzzafl = interface;//com.google.android.gms.internal.ads.zzafl
  //Jzzaus = interface;//com.google.android.gms.internal.ads.zzaus
  //Jzzauv = interface;//com.google.android.gms.internal.ads.zzauv
  //Jzzaws = interface;//com.google.android.gms.internal.ads.zzaws
  //Jzzawy = interface;//com.google.android.gms.internal.ads.zzawy
  //Jzztb = interface;//com.google.android.gms.internal.ads.zztb
  //Jzzte = interface;//com.google.android.gms.internal.ads.zzte
  //Jzztf = interface;//com.google.android.gms.internal.ads.zztf
  //Jzzti = interface;//com.google.android.gms.internal.ads.zzti
  //Jzzyi = interface;//com.google.android.gms.internal.ads.zzyi
  //Jzzyk = interface;//com.google.android.gms.internal.ads.zzyk
  //Jzzym = interface;//com.google.android.gms.internal.ads.zzym
  //Jzzys = interface;//com.google.android.gms.internal.ads.zzys
  //Jzzyw = interface;//com.google.android.gms.internal.ads.zzyw
  //Jzzyx = interface;//com.google.android.gms.internal.ads.zzyx
  //Jzzyz = interface;//com.google.android.gms.internal.ads.zzyz
  //Jzzzd = interface;//com.google.android.gms.internal.ads.zzzd
  //Jzzzz = interface;//com.google.android.gms.internal.ads.zzzz
  //Jmaps_zzaa = interface;//com.google.android.gms.internal.maps.zzaa
  //Jmaps_zzad = interface;//com.google.android.gms.internal.maps.zzad
  //Jmaps_zzag = interface;//com.google.android.gms.internal.maps.zzag
  //Jinternal_maps_zzaj = interface;//com.google.android.gms.internal.maps.zzaj
  //Jmaps_zzi = interface;//com.google.android.gms.internal.maps.zzi
  //Jmaps_zzl = interface;//com.google.android.gms.internal.maps.zzl
  //Jmaps_zzo = interface;//com.google.android.gms.internal.maps.zzo
  //Jmaps_zzr = interface;//com.google.android.gms.internal.maps.zzr
  //Jmaps_zzx = interface;//com.google.android.gms.internal.maps.zzx

// ===== Interface declarations =====

  JSafeParcelableClass = interface(JParcelableClass)
    ['{865435AB-F2E1-4DEF-B775-17CC0CAC9AED}']
    {class} function _GetNULL: JString; cdecl;
    {class} 
  end;

  [JavaSignature('com/google/android/gms/common/internal/safeparcel/SafeParcelable')]
  JSafeParcelable = interface(JParcelable)
    ['{0CBA956D-6EA5-41D8-8AFC-42759ADFD37B}']
  end;
  TJSafeParcelable = class(TJavaGenericImport<JSafeParcelableClass, JSafeParcelable>) end;

  JAbstractSafeParcelableClass = interface(JSafeParcelableClass)
    ['{BC85B4E6-5576-4EB2-A665-AC962376DA97}']
    {class} function init: JAbstractSafeParcelable; cdecl;
  end;

  [JavaSignature('com/google/android/gms/common/internal/safeparcel/AbstractSafeParcelable')]
  JAbstractSafeParcelable = interface(JSafeParcelable)
    ['{5D1DC3DA-DE62-426D-A9F1-202B7329489A}']
    function describeContents: Integer; cdecl;
  end;
  TJAbstractSafeParcelable = class(TJavaGenericImport<JAbstractSafeParcelableClass, JAbstractSafeParcelable>) end;

  JConnectionResultClass = interface(JAbstractSafeParcelableClass)
    ['{4A2A7E79-F9D7-4636-A3CF-C291BEF18896}']
    {class} function _GetAPI_DISABLED: Integer; cdecl;
    {class} function _GetAPI_DISABLED_FOR_CONNECTION: Integer; cdecl;
    {class} function _GetAPI_UNAVAILABLE: Integer; cdecl;
    {class} function _GetCANCELED: Integer; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetDEVELOPER_ERROR: Integer; cdecl;
    {class} function _GetDRIVE_EXTERNAL_STORAGE_REQUIRED: Integer; cdecl;
    {class} function _GetINTERNAL_ERROR: Integer; cdecl;
    {class} function _GetINTERRUPTED: Integer; cdecl;
    {class} function _GetINVALID_ACCOUNT: Integer; cdecl;
    {class} function _GetLICENSE_CHECK_FAILED: Integer; cdecl;
    {class} function _GetNETWORK_ERROR: Integer; cdecl;
    {class} function _GetRESOLUTION_ACTIVITY_NOT_FOUND: Integer; cdecl;
    {class} function _GetRESOLUTION_REQUIRED: Integer; cdecl;
    {class} function _GetRESTRICTED_PROFILE: Integer; cdecl;
    {class} function _GetRESULT_SUCCESS: JConnectionResult; cdecl;
    {class} function _GetSERVICE_DISABLED: Integer; cdecl;
    {class} function _GetSERVICE_INVALID: Integer; cdecl;
    {class} function _GetSERVICE_MISSING: Integer; cdecl;
    {class} function _GetSERVICE_MISSING_PERMISSION: Integer; cdecl;
    {class} function _GetSERVICE_UPDATING: Integer; cdecl;
    {class} function _GetSERVICE_VERSION_UPDATE_REQUIRED: Integer; cdecl;
    {class} function _GetSIGN_IN_FAILED: Integer; cdecl;
    {class} function _GetSIGN_IN_REQUIRED: Integer; cdecl;
    {class} function _GetSUCCESS: Integer; cdecl;
    {class} function _GetTIMEOUT: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} function init(i: Integer): JConnectionResult; cdecl; overload;
    {class} function init(i: Integer; pendingIntent: JPendingIntent): JConnectionResult; cdecl; overload;
    {class} function init(i: Integer; pendingIntent: JPendingIntent; string_: JString): JConnectionResult; cdecl; overload;
    {class} property API_DISABLED: Integer read _GetAPI_DISABLED;
    {class} property API_DISABLED_FOR_CONNECTION: Integer read _GetAPI_DISABLED_FOR_CONNECTION;
    {class} property API_UNAVAILABLE: Integer read _GetAPI_UNAVAILABLE;
    {class} property CANCELED: Integer read _GetCANCELED;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property DEVELOPER_ERROR: Integer read _GetDEVELOPER_ERROR;
    {class} property DRIVE_EXTERNAL_STORAGE_REQUIRED: Integer read _GetDRIVE_EXTERNAL_STORAGE_REQUIRED;
    {class} property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
    {class} property INTERRUPTED: Integer read _GetINTERRUPTED;
    {class} property INVALID_ACCOUNT: Integer read _GetINVALID_ACCOUNT;
    {class} property LICENSE_CHECK_FAILED: Integer read _GetLICENSE_CHECK_FAILED;
    {class} property NETWORK_ERROR: Integer read _GetNETWORK_ERROR;
    {class} property RESOLUTION_ACTIVITY_NOT_FOUND: Integer read _GetRESOLUTION_ACTIVITY_NOT_FOUND;
    {class} property RESOLUTION_REQUIRED: Integer read _GetRESOLUTION_REQUIRED;
    {class} property RESTRICTED_PROFILE: Integer read _GetRESTRICTED_PROFILE;
    {class} property RESULT_SUCCESS: JConnectionResult read _GetRESULT_SUCCESS;
    {class} property SERVICE_DISABLED: Integer read _GetSERVICE_DISABLED;
    {class} property SERVICE_INVALID: Integer read _GetSERVICE_INVALID;
    {class} property SERVICE_MISSING: Integer read _GetSERVICE_MISSING;
    {class} property SERVICE_MISSING_PERMISSION: Integer read _GetSERVICE_MISSING_PERMISSION;
    {class} property SERVICE_UPDATING: Integer read _GetSERVICE_UPDATING;
    {class} property SERVICE_VERSION_UPDATE_REQUIRED: Integer read _GetSERVICE_VERSION_UPDATE_REQUIRED;
    {class} property SIGN_IN_FAILED: Integer read _GetSIGN_IN_FAILED;
    {class} property SIGN_IN_REQUIRED: Integer read _GetSIGN_IN_REQUIRED;
    {class} property SUCCESS: Integer read _GetSUCCESS;
    {class} property TIMEOUT: Integer read _GetTIMEOUT;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/gms/common/ConnectionResult')]
  JConnectionResult = interface(JAbstractSafeParcelable)
    ['{A6C69B94-8D31-4C7B-9001-1DDF4A76D784}']
    function equals(object_: JObject): Boolean; cdecl;
    function getErrorCode: Integer; cdecl;
    function getErrorMessage: JString; cdecl;
    function getResolution: JPendingIntent; cdecl;
    function hasResolution: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isSuccess: Boolean; cdecl;
    procedure startResolutionForResult(activity: JActivity; i: Integer); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJConnectionResult = class(TJavaGenericImport<JConnectionResultClass, JConnectionResult>) end;

  JGooglePlayServicesUtilLightClass = interface(JObjectClass)
    ['{C0B8736E-3F12-4B5C-BEA0-E898FD608DEC}']
    {class} function _GetGOOGLE_PLAY_GAMES_PACKAGE: JString; cdecl;
    {class} function _GetGOOGLE_PLAY_SERVICES_PACKAGE: JString; cdecl;
    {class} function _GetGOOGLE_PLAY_SERVICES_VERSION_CODE: Integer; cdecl;
    {class} function _GetGOOGLE_PLAY_STORE_PACKAGE: JString; cdecl;
    {class} procedure cancelAvailabilityErrorNotifications(context: JContext); cdecl;
    {class} procedure enableUsingApkIndependentContext; cdecl;
    {class} procedure ensurePlayServicesAvailable(context: JContext; i: Integer); cdecl;
    {class} function getApkVersion(context: JContext): Integer; cdecl;
    {class} function getClientVersion(context: JContext): Integer; cdecl;
    {class} function getErrorPendingIntent(i: Integer; context: JContext; i1: Integer): JPendingIntent; cdecl;
    {class} function getErrorString(i: Integer): JString; cdecl;
    {class} function getGooglePlayServicesAvailabilityRecoveryIntent(i: Integer): JIntent; cdecl;
    {class} function getRemoteContext(context: JContext): JContext; cdecl;
    {class} function getRemoteResource(context: JContext): JResources; cdecl;
    {class} function honorsDebugCertificates(context: JContext): Boolean; cdecl;
    {class} function isGooglePlayServicesAvailable(context: JContext): Integer; cdecl; overload;
    {class} function isGooglePlayServicesAvailable(context: JContext; i: Integer): Integer; cdecl; overload;
    {class} function isGooglePlayServicesUid(context: JContext; i: Integer): Boolean; cdecl;
    {class} function isPlayServicesPossiblyUpdating(context: JContext; i: Integer): Boolean; cdecl;
    {class} function isPlayStorePossiblyUpdating(context: JContext; i: Integer): Boolean; cdecl;
    {class} function isRestrictedUserProfile(context: JContext): Boolean; cdecl;
    {class} function isSidewinderDevice(context: JContext): Boolean; cdecl;
    {class} function isUserRecoverableError(i: Integer): Boolean; cdecl;
    {class} function uidHasPackageName(context: JContext; i: Integer; string_: JString): Boolean; cdecl;
    {class} property GOOGLE_PLAY_GAMES_PACKAGE: JString read _GetGOOGLE_PLAY_GAMES_PACKAGE;
    {class} property GOOGLE_PLAY_SERVICES_PACKAGE: JString read _GetGOOGLE_PLAY_SERVICES_PACKAGE;
    {class} property GOOGLE_PLAY_SERVICES_VERSION_CODE: Integer read _GetGOOGLE_PLAY_SERVICES_VERSION_CODE;
    {class} property GOOGLE_PLAY_STORE_PACKAGE: JString read _GetGOOGLE_PLAY_STORE_PACKAGE;
  end;

  [JavaSignature('com/google/android/gms/common/GooglePlayServicesUtilLight')]
  JGooglePlayServicesUtilLight = interface(JObject)
    ['{8C3FDCFF-224F-40FD-9A5E-759F427C7A74}']
  end;
  TJGooglePlayServicesUtilLight = class(TJavaGenericImport<JGooglePlayServicesUtilLightClass, JGooglePlayServicesUtilLight>) end;

  JGooglePlayServicesUtilClass = interface(JGooglePlayServicesUtilLightClass)
    ['{DEE7FB7C-B1A2-4098-9E42-67A2CF856C16}']
    {class} function _GetGMS_ERROR_DIALOG: JString; cdecl;
    {class} function _GetGOOGLE_PLAY_SERVICES_PACKAGE: JString; cdecl;
    {class} function _GetGOOGLE_PLAY_SERVICES_VERSION_CODE: Integer; cdecl;
    {class} function _GetGOOGLE_PLAY_STORE_PACKAGE: JString; cdecl;
    {class} function getErrorDialog(i: Integer; activity: JActivity; i1: Integer): JDialog; cdecl; overload;
    {class} function getErrorDialog(i: Integer; activity: JActivity; i1: Integer; onCancelListener: JDialogInterface_OnCancelListener): JDialog; cdecl; overload;
    {class} function getErrorPendingIntent(i: Integer; context: JContext; i1: Integer): JPendingIntent; cdecl;
    {class} function getErrorString(i: Integer): JString; cdecl;
    {class} function getRemoteContext(context: JContext): JContext; cdecl;
    {class} function getRemoteResource(context: JContext): JResources; cdecl;
    {class} function isGooglePlayServicesAvailable(context: JContext): Integer; cdecl; overload;
    {class} function isGooglePlayServicesAvailable(context: JContext; i: Integer): Integer; cdecl; overload;
    {class} function isUserRecoverableError(i: Integer): Boolean; cdecl;
    {class} function showErrorDialogFragment(i: Integer; activity: JActivity; i1: Integer): Boolean; cdecl; overload;
    {class} function showErrorDialogFragment(i: Integer; activity: JActivity; i1: Integer; onCancelListener: JDialogInterface_OnCancelListener): Boolean; cdecl; overload;
    {class} function showErrorDialogFragment(i: Integer; activity: JActivity; fragment: Jfragment_app_Fragment; i1: Integer; onCancelListener: JDialogInterface_OnCancelListener): Boolean; cdecl; overload;
    {class} procedure showErrorNotification(i: Integer; context: JContext); cdecl;
    {class} property GMS_ERROR_DIALOG: JString read _GetGMS_ERROR_DIALOG;
    {class} //GOOGLE_PLAY_SERVICES_PACKAGE is defined in parent interface
    {class} //GOOGLE_PLAY_SERVICES_VERSION_CODE is defined in parent interface
    {class} //GOOGLE_PLAY_STORE_PACKAGE is defined in parent interface
  end;

  [JavaSignature('com/google/android/gms/common/GooglePlayServicesUtil')]
  JGooglePlayServicesUtil = interface(JGooglePlayServicesUtilLight)
    ['{3549FA2E-E8D1-4F37-A21B-B5E10E6169D1}']
  end;
  TJGooglePlayServicesUtil = class(TJavaGenericImport<JGooglePlayServicesUtilClass, JGooglePlayServicesUtil>) end;

  JLifecycleActivityClass = interface(JObjectClass)
    ['{9EC2140E-F848-407F-BB04-A8F831848114}']
    {class} function init(contextWrapper: JContextWrapper): JLifecycleActivity; cdecl; overload;
    {class} function init(activity: JActivity): JLifecycleActivity; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/common/api/internal/LifecycleActivity')]
  JLifecycleActivity = interface(JObject)
    ['{2B68991F-98B8-4B61-9238-3B608D92A7C1}']
    function asActivity: JActivity; cdecl;
    function asFragmentActivity: JFragmentActivity; cdecl;
    function asObject: JObject; cdecl;
    function isChimera: Boolean; cdecl;
    function isSupport: Boolean; cdecl;
    function zza: Boolean; cdecl;
  end;
  TJLifecycleActivity = class(TJavaGenericImport<JLifecycleActivityClass, JLifecycleActivity>) end;

  JLifecycleCallbackClass = interface(JObjectClass)
    ['{D719BE18-D6E4-4FA1-8044-FD27D6891243}']
    {class} function getFragment(contextWrapper: JContextWrapper): JLifecycleFragment; cdecl; overload;
    {class} function getFragment(activity: JActivity): JLifecycleFragment; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/common/api/internal/LifecycleCallback')]
  JLifecycleCallback = interface(JObject)
    ['{DCDAB6FF-1D93-489E-9025-AAE277F255ED}']
    procedure dump(string_: JString; fileDescriptor: JFileDescriptor; printWriter: JPrintWriter; string_1: TJavaObjectArray<JString>); cdecl;
    function getActivity: JActivity; cdecl;
    procedure onActivityResult(i: Integer; i1: Integer; intent: JIntent); cdecl;
    procedure onCreate(bundle: JBundle); cdecl;
    procedure onDestroy; cdecl;
    procedure onResume; cdecl;
    procedure onSaveInstanceState(bundle: JBundle); cdecl;
    procedure onStart; cdecl;
    procedure onStop; cdecl;
  end;
  TJLifecycleCallback = class(TJavaGenericImport<JLifecycleCallbackClass, JLifecycleCallback>) end;

  JLifecycleFragmentClass = interface(IJavaClass)
    ['{30FF17B4-3B40-44A8-B667-198908605577}']
  end;

  [JavaSignature('com/google/android/gms/common/api/internal/LifecycleFragment')]
  JLifecycleFragment = interface(IJavaInstance)
    ['{9EB4F0C2-029D-4CE3-ADF0-40E5CE957115}']
    procedure addCallback(string_: JString; lifecycleCallback: JLifecycleCallback); cdecl;
    function getCallbackOrNull(string_: JString; class_: Jlang_Class): JLifecycleCallback; cdecl;
    function getLifecycleActivity: JActivity; cdecl;
    function isCreated: Boolean; cdecl;
    function isStarted: Boolean; cdecl;
    procedure startActivityForResult(intent: JIntent; i: Integer); cdecl;
  end;
  TJLifecycleFragment = class(TJavaGenericImport<JLifecycleFragmentClass, JLifecycleFragment>) end;

  JIObjectWrapperClass = interface(JIInterfaceClass)
    ['{7658457F-D0AE-4F7E-9742-545C6EABA6B3}']
  end;

  [JavaSignature('com/google/android/gms/dynamic/IObjectWrapper')]
  JIObjectWrapper = interface(JIInterface)
    ['{1BE3436C-8B90-4364-8263-9404B8F4FB72}']
  end;
  TJIObjectWrapper = class(TJavaGenericImport<JIObjectWrapperClass, JIObjectWrapper>) end;

  // com.google.android.gms.internal.ads.zzaae
  // com.google.android.gms.internal.ads.zzaah
  // com.google.android.gms.internal.ads.zzaak
  // com.google.android.gms.internal.ads.zzaau
  // com.google.android.gms.internal.ads.zzaay
  // com.google.android.gms.internal.ads.zzabb
  // com.google.android.gms.internal.ads.zzabf
  // com.google.android.gms.internal.ads.zzabi
  // com.google.android.gms.internal.ads.zzacd
  // com.google.android.gms.internal.ads.zzacg
  // com.google.android.gms.internal.ads.zzacj
  // com.google.android.gms.internal.ads.zzacm
  // com.google.android.gms.internal.ads.zzacn
  // com.google.android.gms.internal.ads.zzacp
  // com.google.android.gms.internal.ads.zzacq
  // com.google.android.gms.internal.ads.zzacs
  // com.google.android.gms.internal.ads.zzadb
  // com.google.android.gms.internal.ads.zzadu
  // com.google.android.gms.internal.ads.zzady
  // com.google.android.gms.internal.ads.zzafi
  // com.google.android.gms.internal.ads.zzafl
  // com.google.android.gms.internal.ads.zzaus
  // com.google.android.gms.internal.ads.zzauv
  // com.google.android.gms.internal.ads.zzaws
  // com.google.android.gms.internal.ads.zzawy
  // com.google.android.gms.internal.ads.zztb
  // com.google.android.gms.internal.ads.zzte
  // com.google.android.gms.internal.ads.zztf
  // com.google.android.gms.internal.ads.zzti
  // com.google.android.gms.internal.ads.zzyi
  // com.google.android.gms.internal.ads.zzyk
  // com.google.android.gms.internal.ads.zzym
  // com.google.android.gms.internal.ads.zzys
  // com.google.android.gms.internal.ads.zzyw
  // com.google.android.gms.internal.ads.zzyx
  // com.google.android.gms.internal.ads.zzyz
  // com.google.android.gms.internal.ads.zzzd
  // com.google.android.gms.internal.ads.zzzz
  // com.google.android.gms.internal.maps.zzaa
  // com.google.android.gms.internal.maps.zzad
  // com.google.android.gms.internal.maps.zzag
  // com.google.android.gms.internal.maps.zzaj
  // com.google.android.gms.internal.maps.zzi
  // com.google.android.gms.internal.maps.zzl
  // com.google.android.gms.internal.maps.zzo
  // com.google.android.gms.internal.maps.zzr
  // com.google.android.gms.internal.maps.zzx
implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JSafeParcelable', TypeInfo(Androidapi.JNI.PlayServices.JSafeParcelable));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JAbstractSafeParcelable', TypeInfo(Androidapi.JNI.PlayServices.JAbstractSafeParcelable));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JConnectionResult', TypeInfo(Androidapi.JNI.PlayServices.JConnectionResult));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JGooglePlayServicesUtilLight', TypeInfo(Androidapi.JNI.PlayServices.JGooglePlayServicesUtilLight));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JGooglePlayServicesUtil', TypeInfo(Androidapi.JNI.PlayServices.JGooglePlayServicesUtil));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JLifecycleActivity', TypeInfo(Androidapi.JNI.PlayServices.JLifecycleActivity));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JLifecycleCallback', TypeInfo(Androidapi.JNI.PlayServices.JLifecycleCallback));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JLifecycleFragment', TypeInfo(Androidapi.JNI.PlayServices.JLifecycleFragment));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.JIObjectWrapper', TypeInfo(Androidapi.JNI.PlayServices.JIObjectWrapper));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzaae', TypeInfo(Androidapi.JNI.PlayServices.Jzzaae));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzaah', TypeInfo(Androidapi.JNI.PlayServices.Jzzaah));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzaak', TypeInfo(Androidapi.JNI.PlayServices.Jzzaak));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzaau', TypeInfo(Androidapi.JNI.PlayServices.Jzzaau));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzaay', TypeInfo(Androidapi.JNI.PlayServices.Jzzaay));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzabb', TypeInfo(Androidapi.JNI.PlayServices.Jzzabb));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzabf', TypeInfo(Androidapi.JNI.PlayServices.Jzzabf));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzabi', TypeInfo(Androidapi.JNI.PlayServices.Jzzabi));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacd', TypeInfo(Androidapi.JNI.PlayServices.Jzzacd));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacg', TypeInfo(Androidapi.JNI.PlayServices.Jzzacg));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacj', TypeInfo(Androidapi.JNI.PlayServices.Jzzacj));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacm', TypeInfo(Androidapi.JNI.PlayServices.Jzzacm));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacn', TypeInfo(Androidapi.JNI.PlayServices.Jzzacn));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacp', TypeInfo(Androidapi.JNI.PlayServices.Jzzacp));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacq', TypeInfo(Androidapi.JNI.PlayServices.Jzzacq));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzacs', TypeInfo(Androidapi.JNI.PlayServices.Jzzacs));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzadb', TypeInfo(Androidapi.JNI.PlayServices.Jzzadb));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzadu', TypeInfo(Androidapi.JNI.PlayServices.Jzzadu));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzady', TypeInfo(Androidapi.JNI.PlayServices.Jzzady));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzafi', TypeInfo(Androidapi.JNI.PlayServices.Jzzafi));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzafl', TypeInfo(Androidapi.JNI.PlayServices.Jzzafl));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzaus', TypeInfo(Androidapi.JNI.PlayServices.Jzzaus));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzauv', TypeInfo(Androidapi.JNI.PlayServices.Jzzauv));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzaws', TypeInfo(Androidapi.JNI.PlayServices.Jzzaws));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzawy', TypeInfo(Androidapi.JNI.PlayServices.Jzzawy));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzztb', TypeInfo(Androidapi.JNI.PlayServices.Jzztb));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzte', TypeInfo(Androidapi.JNI.PlayServices.Jzzte));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzztf', TypeInfo(Androidapi.JNI.PlayServices.Jzztf));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzti', TypeInfo(Androidapi.JNI.PlayServices.Jzzti));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzyi', TypeInfo(Androidapi.JNI.PlayServices.Jzzyi));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzyk', TypeInfo(Androidapi.JNI.PlayServices.Jzzyk));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzym', TypeInfo(Androidapi.JNI.PlayServices.Jzzym));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzys', TypeInfo(Androidapi.JNI.PlayServices.Jzzys));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzyw', TypeInfo(Androidapi.JNI.PlayServices.Jzzyw));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzyx', TypeInfo(Androidapi.JNI.PlayServices.Jzzyx));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzyz', TypeInfo(Androidapi.JNI.PlayServices.Jzzyz));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzzd', TypeInfo(Androidapi.JNI.PlayServices.Jzzzd));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jzzzz', TypeInfo(Androidapi.JNI.PlayServices.Jzzzz));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzaa', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzaa));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzad', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzad));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzag', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzag));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jinternal_maps_zzaj', TypeInfo(Androidapi.JNI.PlayServices.Jinternal_maps_zzaj));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzi', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzi));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzl', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzl));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzo', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzo));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzr', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzr));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Jmaps_zzx', TypeInfo(Androidapi.JNI.PlayServices.Jmaps_zzx));
end;

initialization
  RegisterTypes;
end.


