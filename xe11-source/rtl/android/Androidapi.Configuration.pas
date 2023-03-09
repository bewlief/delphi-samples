{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Configuration;

interface

uses Androidapi.AssetManager;

(*$HPPEMIT '#include <android/configuration.h>' *)

{$I Androidapi.inc}

type
  AConfiguration = record end;
  {$EXTERNALSYM AConfiguration}
  PAConfiguration = ^AConfiguration;

const
  /// <summary>Orientation: not specified.</summary>
  ACONFIGURATION_ORIENTATION_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_ORIENTATION_ANY}

  /// <summary>Orientation: value corresponding to the port resource
  /// qualifier</summary>
  ACONFIGURATION_ORIENTATION_PORT = $0001;
  {$EXTERNALSYM ACONFIGURATION_ORIENTATION_PORT}

  /// <summary>Orientation: value corresponding to the land resource
  /// qualifier</summary>
  ACONFIGURATION_ORIENTATION_LAND = $0002;
  {$EXTERNALSYM ACONFIGURATION_ORIENTATION_LAND}
  
  /// <summary>Not currently supported or used.</summary>
  ACONFIGURATION_ORIENTATION_SQUARE = $0003 deprecated;
  {$EXTERNALSYM ACONFIGURATION_ORIENTATION_SQUARE}

  /// <summary>Touchscreen: not specified.</summary>
  ACONFIGURATION_TOUCHSCREEN_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_TOUCHSCREEN_ANY}

  /// <summary>Touchscreen: value corresponding to the notouch resource
  /// qualifier.</summary>
  ACONFIGURATION_TOUCHSCREEN_NOTOUCH = $0001;
  {$EXTERNALSYM ACONFIGURATION_TOUCHSCREEN_NOTOUCH}

  /// <summary>Not currently supported or used.</summary>
  ACONFIGURATION_TOUCHSCREEN_STYLUS = $0002 deprecated;
  {$EXTERNALSYM ACONFIGURATION_TOUCHSCREEN_STYLUS}

  /// <summary>Touchscreen: value corresponding to the finger resource
  /// qualifier.</summary>
  ACONFIGURATION_TOUCHSCREEN_FINGER = $0003;
  {$EXTERNALSYM ACONFIGURATION_TOUCHSCREEN_FINGER}
  
  /// <summary>Density: default density.</summary>
  ACONFIGURATION_DENSITY_DEFAULT = 0;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_DEFAULT}

  /// <summary>Density: value corresponding to the ldpi resource
  /// qualifier.</summary>
  ACONFIGURATION_DENSITY_LOW = 120;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_LOW}

  /// <summary>Density: value corresponding to the mdpi resource
  /// qualifier.</summary>
  ACONFIGURATION_DENSITY_MEDIUM = 160;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_MEDIUM}

  /// <summary>Density: value corresponding to the tvdpi resource
  /// qualifier.</summary>
  ACONFIGURATION_DENSITY_TV = 213;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_TV}

  /// <summary>Density: value corresponding to the hdpi resource
  /// qualifier.</summary>
  ACONFIGURATION_DENSITY_HIGH = 240;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_HIGH}

  /// <summary>Density: value corresponding to the xhdpi resource
  /// qualifier.</summary>
  ACONFIGURATION_DENSITY_XHIGH = 320;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_XHIGH}

  /// <summary>Density: value corresponding to the xxhdpi resource
  /// qualifier.</summary>
  ACONFIGURATION_DENSITY_XXHIGH = 480;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_XXHIGH}

  /// <summary>Density: value corresponding to the xxxhdpi resource
  /// qualifier.</summary>
  ACONFIGURATION_DENSITY_XXXHIGH = 640;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_XXXHIGH}

  /// <summary>Density: any density.</summary>
  ACONFIGURATION_DENSITY_ANY = $fffe;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_ANY}

  /// <summary>Density: no density specified.</summary>
  ACONFIGURATION_DENSITY_NONE = $ffff;
  {$EXTERNALSYM ACONFIGURATION_DENSITY_NONE}

  /// <summary>Keyboard: not specified.</summary>
  ACONFIGURATION_KEYBOARD_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_KEYBOARD_ANY}

  /// <summary>Keyboard: value corresponding to the nokeys resource
  /// qualifier.</summary>
  ACONFIGURATION_KEYBOARD_NOKEYS = $0001;
  {$EXTERNALSYM ACONFIGURATION_KEYBOARD_NOKEYS}

  /// <summary>Keyboard: value corresponding to the qwerty resource
  /// qualifier.</summary>
  ACONFIGURATION_KEYBOARD_QWERTY = $0002;
  {$EXTERNALSYM ACONFIGURATION_KEYBOARD_QWERTY}

  /// <summary>Keyboard: value corresponding to the 12key resource
  /// qualifier.</summary>
  ACONFIGURATION_KEYBOARD_12KEY = $0003;
  {$EXTERNALSYM ACONFIGURATION_KEYBOARD_12KEY}

  /// <summary>Navigation: not specified.</summary>
  ACONFIGURATION_NAVIGATION_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_NAVIGATION_ANY}

  /// <summary>Navigation: value corresponding to the nonav resource
  /// qualifier.</summary>
  ACONFIGURATION_NAVIGATION_NONAV = $0001;
  {$EXTERNALSYM ACONFIGURATION_NAVIGATION_NONAV}

  /// <summary>Navigation: value corresponding to the dpad resource
  /// qualifier.</summary>
  ACONFIGURATION_NAVIGATION_DPAD = $0002;
  {$EXTERNALSYM ACONFIGURATION_NAVIGATION_DPAD}
  
  /// <summary>Navigation: value corresponding to the trackball resource
  /// qualifier.</summary>
  ACONFIGURATION_NAVIGATION_TRACKBALL = $0003;
  {$EXTERNALSYM ACONFIGURATION_NAVIGATION_TRACKBALL}

  /// <summary>Navigation: value corresponding to the wheel resource
  /// qualifier.</summary>
  ACONFIGURATION_NAVIGATION_WHEEL = $0004;
  {$EXTERNALSYM ACONFIGURATION_NAVIGATION_WHEEL}
  
  /// <summary>Keyboard availability: not specified.</summary>
  ACONFIGURATION_KEYSHIDDEN_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_KEYSHIDDEN_ANY}

  /// <summary>Keyboard availability: value corresponding to the keysexposed
  /// resource qualifier.</summary>
  ACONFIGURATION_KEYSHIDDEN_NO = $0001;
  {$EXTERNALSYM ACONFIGURATION_KEYSHIDDEN_NO}

  /// <summary>Keyboard availability: value corresponding to the keyshidden
  /// resource qualifier.</summary>
  ACONFIGURATION_KEYSHIDDEN_YES = $0002;
  {$EXTERNALSYM ACONFIGURATION_KEYSHIDDEN_YES}

  /// <summary>Keyboard availability: value corresponding to the keyssoft
  /// resource qualifier.</summary>
  ACONFIGURATION_KEYSHIDDEN_SOFT = $0003;
  {$EXTERNALSYM ACONFIGURATION_KEYSHIDDEN_SOFT}

  /// <summary>Navigation availability: not specified.</summary>
  ACONFIGURATION_NAVHIDDEN_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_NAVHIDDEN_ANY}

  /// <summary>Navigation availability: value corresponding to the navexposed
  /// resource qualifier.</summary>
  ACONFIGURATION_NAVHIDDEN_NO = $0001;
  {$EXTERNALSYM ACONFIGURATION_NAVHIDDEN_NO}

  /// <summary>Navigation availability: value corresponding to the navhidden
  /// resource qualifier.</summary>
  ACONFIGURATION_NAVHIDDEN_YES = $0002;
  {$EXTERNALSYM ACONFIGURATION_NAVHIDDEN_YES}
  
  /// <summary>Screen size: not specified.</summary>
  ACONFIGURATION_SCREENSIZE_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_SCREENSIZE_ANY}

  /// <summary>Screen size: value indicating the screen is at least
  /// approximately 320x426 dp units, corresponding to the small resource
  /// qualifier.</summary>
  ACONFIGURATION_SCREENSIZE_SMALL = $01;
  {$EXTERNALSYM ACONFIGURATION_SCREENSIZE_SMALL}

  /// <summary>Screen size: value indicating the screen is at least
  /// approximately 320x470 dp units, corresponding to the normal resource
  /// qualifier.</summary>
  ACONFIGURATION_SCREENSIZE_NORMAL = $02;
  {$EXTERNALSYM ACONFIGURATION_SCREENSIZE_NORMAL}

  /// <summary>Screen size: value indicating the screen is at least
  /// approximately 480x640 dp units, corresponding to the large resource
  /// qualifier.</summary>
  ACONFIGURATION_SCREENSIZE_LARGE = $03;
  {$EXTERNALSYM ACONFIGURATION_SCREENSIZE_LARGE}

  /// <summary>Screen size: value indicating the screen is at least
  /// approximately 720x960 dp units, corresponding to the xlarge resource
  /// qualifier.</summary>
  ACONFIGURATION_SCREENSIZE_XLARGE = $04;
  {$EXTERNALSYM ACONFIGURATION_SCREENSIZE_XLARGE}
  
  /// <summary>Screen layout: not specified.</summary>
  ACONFIGURATION_SCREENLONG_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_SCREENLONG_ANY}

  /// <summary>Screen layout: value that corresponds to the notlong resource
  /// qualifier.</summary>
  ACONFIGURATION_SCREENLONG_NO = $01;
  {$EXTERNALSYM ACONFIGURATION_SCREENLONG_NO}

  /// <summary>Screen layout: value that corresponds to the long resource
  /// qualifier.</summary>
  ACONFIGURATION_SCREENLONG_YES = $02;
  {$EXTERNALSYM ACONFIGURATION_SCREENLONG_YES}

  ACONFIGURATION_SCREENROUND_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_SCREENROUND_ANY}

  ACONFIGURATION_SCREENROUND_NO = $01;
  {$EXTERNALSYM ACONFIGURATION_SCREENROUND_NO}

  ACONFIGURATION_SCREENROUND_YES = $02;
  {$EXTERNALSYM ACONFIGURATION_SCREENROUND_YES}

  /// <summary>Wide color gamut: not specified.</summary>
  ACONFIGURATION_WIDE_COLOR_GAMUT_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_WIDE_COLOR_GAMUT_ANY}

  /// <summary>Wide color gamut: value that corresponds to nowidecg resource
  /// qualifier specified.</summary>
  ACONFIGURATION_WIDE_COLOR_GAMUT_NO = $01;
  {$EXTERNALSYM ACONFIGURATION_WIDE_COLOR_GAMUT_NO}

  /// <summary>Wide color gamut: value that corresponds to widecg resource
  /// qualifier specified.</summary>
  ACONFIGURATION_WIDE_COLOR_GAMUT_YES = $02;
  {$EXTERNALSYM ACONFIGURATION_WIDE_COLOR_GAMUT_YES}

  /// <summary>HDR: not specified.</summary>
  ACONFIGURATION_HDR_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_HDR_ANY}

  /// <summary>HDR: value that corresponds to lowdr resource qualifier
  /// specified.</summary>
  ACONFIGURATION_HDR_NO = $01;
  {$EXTERNALSYM ACONFIGURATION_HDR_NO}

  /// <summary>HDR: value that corresponds to highdr resource qualifier
  /// specified.</summary>
  ACONFIGURATION_HDR_YES = $02;
  {$EXTERNALSYM ACONFIGURATION_HDR_YES}

  /// <summary>UI mode: not specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_ANY}

  /// <summary>UI mode: value that corresponds to no UI mode type resource
  /// qualifier specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_NORMAL = $01;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_NORMAL}

  /// <summary>UI mode: value that corresponds to desk resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_DESK = $02;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_DESK}

  /// <summary>UI mode: value that corresponds to car resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_CAR = $03;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_CAR}

  /// <summary>UI mode: value that corresponds to television resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_TELEVISION = $04;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_TELEVISION}

  /// <summary>UI mode: value that corresponds to appliance resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_APPLIANCE = $05;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_APPLIANCE}

  /// <summary>UI mode: value that corresponds to watch resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_WATCH = $06;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_WATCH}

  /// <summary>UI mode: value that corresponds to vr resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_TYPE_VR_HEADSET = $07;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_TYPE_VR_HEADSET}

  /// <summary>UI night mode: not specified.</summary>
  ACONFIGURATION_UI_MODE_NIGHT_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_NIGHT_ANY}

  /// <summary>UI night mode: value that corresponds to notnight resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_NIGHT_NO = $1;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_NIGHT_NO}
  
  /// <summary>UI night mode: value that corresponds to night resource qualifier
  /// specified.</summary>
  ACONFIGURATION_UI_MODE_NIGHT_YES = $2;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE_NIGHT_YES}
  
  /// <summary>Screen width DPI: not specified.</summary>
  ACONFIGURATION_SCREEN_WIDTH_DP_ANY = $0001;
  {$EXTERNALSYM ACONFIGURATION_SCREEN_WIDTH_DP_ANY}

  /// <summary>Screen height DPI: not specified.</summary>
  ACONFIGURATION_SCREEN_HEIGHT_DP_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_SCREEN_HEIGHT_DP_ANY}

  /// <summary>Smallest screen width DPI: not specified.</summary>
  ACONFIGURATION_SMALLEST_SCREEN_WIDTH_DP_ANY = $0000;
  {$EXTERNALSYM ACONFIGURATION_SMALLEST_SCREEN_WIDTH_DP_ANY}

  /// <summary>Layout direction: not specified.</summary>
  ACONFIGURATION_LAYOUTDIR_ANY = $00;
  {$EXTERNALSYM ACONFIGURATION_LAYOUTDIR_ANY}

  /// <summary>Layout direction: value that corresponds to ldltr resource
  /// qualifier specified.</summary>
  ACONFIGURATION_LAYOUTDIR_LTR = $01;
  {$EXTERNALSYM ACONFIGURATION_LAYOUTDIR_LTR}

  /// <summary>Layout direction: value that corresponds to ldrtl resource
  /// qualifier specified.</summary>
  ACONFIGURATION_LAYOUTDIR_RTL = $02;
  {$EXTERNALSYM ACONFIGURATION_LAYOUTDIR_RTL}

  /// <summary>Bit mask for mcc configuration.</summary>
  ACONFIGURATION_MCC = $0001;
  {$EXTERNALSYM ACONFIGURATION_Mcc}

  /// <summary>Bit mask for mnc configuration.</summary>
  ACONFIGURATION_MNC = $0002;
  {$EXTERNALSYM ACONFIGURATION_Mnc}

  /// <summary>Bit mask for locale configuration.</summary>
  ACONFIGURATION_LOCALE = $0004;
  {$EXTERNALSYM ACONFIGURATION_LOCALE}
  
  /// <summary>Bit mask for touchscreen configuration.</summary>
  ACONFIGURATION_TOUCHSCREEN = $0008;
  {$EXTERNALSYM ACONFIGURATION_TOUCHSCREEN}

  /// <summary>Bit mask for keyboard configuration.</summary>
  ACONFIGURATION_KEYBOARD = $0010;
  {$EXTERNALSYM ACONFIGURATION_KEYBOARD}

  /// <summary>Bit mask for keyboardHidden configuration.</summary>
  ACONFIGURATION_KEYBOARD_HIDDEN = $0020;
  {$EXTERNALSYM ACONFIGURATION_KEYBOARD_HIDDEN}

  /// <summary>Bit mask for navigation configuration.</summary>
  ACONFIGURATION_NAVIGATION = $0040;
  {$EXTERNALSYM ACONFIGURATION_NAVIGATION}

  /// <summary>Bit mask for orientation configuration.</summary>
  ACONFIGURATION_ORIENTATION = $0080;
  {$EXTERNALSYM ACONFIGURATION_ORIENTATION}

  /// <summary>Bit mask for density configuration.</summary>
  ACONFIGURATION_DENSITY = $0100;
  {$EXTERNALSYM ACONFIGURATION_DENSITY}

  /// <summary>Bit mask for screen size configuration.</summary>
  ACONFIGURATION_SCREEN_SIZE = $0200;
  {$EXTERNALSYM ACONFIGURATION_SCREEN_SIZE}

  /// <summary>Bit mask for platform version configuration.</summary>
  ACONFIGURATION_VERSION = $0400;
  {$EXTERNALSYM ACONFIGURATION_VERSION}

  /// <summary>Bit mask for screen layout configuration.</summary>
  ACONFIGURATION_SCREEN_LAYOUT = $0800;
  {$EXTERNALSYM ACONFIGURATION_SCREEN_LAYOUT}

  /// <summary>Bit mask for ui mode configuration.</summary>
  ACONFIGURATION_UI_MODE = $1000;
  {$EXTERNALSYM ACONFIGURATION_UI_MODE}

  /// <summary>Bit mask for smallest screen width configuration.</summary>
  ACONFIGURATION_SMALLEST_SCREEN_SIZE = $2000;
  {$EXTERNALSYM ACONFIGURATION_SMALLEST_SCREEN_SIZE}

  /// <summary>Bit mask for layout direction configuration.</summary>
  ACONFIGURATION_LAYOUTDIR = $4000;
  {$EXTERNALSYM ACONFIGURATION_LAYOUTDIR}

  ACONFIGURATION_SCREEN_ROUND = $8000;
  {$EXTERNALSYM ACONFIGURATION_SCREEN_ROUND}

  /// <summary>Bit mask for wide color gamut configuration.</summary>
  ACONFIGURATION_COLOR_MODE = $10000;
  {$EXTERNALSYM ACONFIGURATION_COLOR_MODE}

  /// <summary>Constant used to to represent MNC (Mobile Network Code) zero.
  /// 0 cannot be used, since it is used to represent an undefined MNC.</summary>
  ACONFIGURATION_MNC_ZERO = $ffff;
  {$EXTERNALSYM ACONFIGURATION_MNC_ZERO}

/// <summary>Create a new AConfiguration, initialized with no values set.</summary>
function AConfiguration_new: PAConfiguration; cdecl;
  external AndroidLib name 'AConfiguration_new';
{$EXTERNALSYM AConfiguration_new}

/// <summary>Free an AConfiguration that was previously created with
/// AConfiguration_new().</summary>
procedure AConfiguration_delete(Config: PAConfiguration); cdecl;
  external AndroidLib name 'AConfiguration_delete';
{$EXTERNALSYM AConfiguration_delete}

/// <summary>Create and return a new AConfiguration based on the current
/// configuration in use in the given AssetManager.</summary>
procedure AConfiguration_fromAssetManager(OutConfig: PAConfiguration;AssetManager: PAAssetManager); cdecl;
  external AndroidLib name 'AConfiguration_fromAssetManager';
{$EXTERNALSYM AConfiguration_fromAssetManager}

/// <summary>Copy the contents of 'SrcConfig' to 'DestConfig'.</summary>
procedure AConfiguration_copy(DestConfig, SrcConfig: PAConfiguration); cdecl;
  external AndroidLib name 'AConfiguration_copy';
{$EXTERNALSYM AConfiguration_copy}

/// <summary>Return the current Mcc set in the configuration. 0 if not set.
/// </summary>
function AConfiguration_getMcc(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getMcc';
{$EXTERNALSYM AConfiguration_getMcc}

/// <summary>Set the current Mcc in the configuration. 0 to clear.</summary>
procedure AConfiguration_setMcc(Config: PAConfiguration; Mcc: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setMcc';
{$EXTERNALSYM AConfiguration_setMcc}

/// <summary>Return the current Mnc set in the configuration. 0 if not set.
/// </summary>
function AConfiguration_getMnc(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getMnc';
{$EXTERNALSYM AConfiguration_getMnc}

/// <summary>Set the current Mnc in the configuration. 0 to clear.</summary>
procedure AConfiguration_setMnc(Config: PAConfiguration; Mnc: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setMnc';
{$EXTERNALSYM AConfiguration_setMnc}

/// <summary>Return the current language code set in the configuration. The
/// output will be filled with an array of two characters. They are not
/// 0-terminated. If a language is not set, they will be 0.</summary>
procedure AConfiguration_getLanguage(Config: PAConfiguration; OutLanguage: MarshaledAString); cdecl;
  external AndroidLib name 'AConfiguration_getLanguage';
{$EXTERNALSYM AConfiguration_getLanguage}

/// <summary>Set the current language code in the configuration, from the first
/// two characters in the string.</summary>
procedure AConfiguration_setLanguage(Config: PAConfiguration; const Language:  MarshaledAString); cdecl;
  external AndroidLib name 'AConfiguration_setLanguage';
{$EXTERNALSYM AConfiguration_setLanguage}

/// <summary>Return the current country code set in the configuration. The
/// output will be filled with an array of two characters. They are not
/// 0-terminated. If a country is not set, they will be 0.</summary>
procedure AConfiguration_getCountry(Config: PAConfiguration; OutCountry:  MarshaledAString); cdecl;
  external AndroidLib name 'AConfiguration_getCountry';
{$EXTERNALSYM AConfiguration_getCountry}

/// <summary>Set the current country code in the configuration, from the first
/// two characters in the string.</summary>
procedure AConfiguration_setCountry(Config: PAConfiguration; const Country:  MarshaledAString); cdecl;
  external AndroidLib name 'AConfiguration_setCountry';
{$EXTERNALSYM AConfiguration_setCountry}

/// <summary>Return the current ACONFIGURATION_ORIENTATION_* set in the
/// configuration.</summary>
function AConfiguration_getOrientation(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getOrientation';
{$EXTERNALSYM AConfiguration_getOrientation}

/// <summary>Set the current orientation in the configuration.</summary>
procedure AConfiguration_setOrientation(Config: PAConfiguration; Orientation: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setOrientation';
{$EXTERNALSYM AConfiguration_setOrientation}

/// <summary>Return the current ACONFIGURATION_TOUCHSCREEN_* set in the
/// configuration.</summary>
function AConfiguration_getTouchscreen(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getTouchscreen';
{$EXTERNALSYM AConfiguration_getTouchscreen}

/// <summary>Set the current touchscreen in the configuration.</summary>
procedure AConfiguration_setTouchscreen(Config: PAConfiguration; TouchScreen: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setTouchscreen';
{$EXTERNALSYM AConfiguration_setTouchscreen}

/// <summary>Return the current ACONFIGURATION_DENSITY_* set in the
/// configuration.</summary>
function AConfiguration_getDensity(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getDensity';
{$EXTERNALSYM AConfiguration_getDensity}

/// <summary>Set the current density in the configuration.</summary>
procedure AConfiguration_setDensity(Config: PAConfiguration; density: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setDensity';
{$EXTERNALSYM AConfiguration_setDensity}

/// <summary>Return the current ACONFIGURATION_KEYBOARD_* set in the
/// configuration.</summary>
function AConfiguration_getKeyboard(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getKeyboard';
{$EXTERNALSYM AConfiguration_getKeyboard}

/// <summary>Set the current keyboard in the configuration.</summary>
procedure AConfiguration_setKeyboard(Config: PAConfiguration; keyboard: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setKeyboard';
{$EXTERNALSYM AConfiguration_setKeyboard}

/// <summary>Return the current ACONFIGURATION_NAVIGATION_* set in the
/// configuration.</summary>
function AConfiguration_getNavigation(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getNavigation';
{$EXTERNALSYM AConfiguration_getNavigation}

/// <summary>Set the current navigation in the configuration.</summary>
procedure AConfiguration_setNavigation(Config: PAConfiguration; navigation: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setNavigation';
{$EXTERNALSYM AConfiguration_setNavigation}

/// <summary>Return the current ACONFIGURATION_KEYSHIDDEN_* set in the
/// configuration.</summary>
function AConfiguration_getKeysHidden(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getKeysHidden';
{$EXTERNALSYM AConfiguration_getKeysHidden}

/// <summary>Set the current keys hidden in the configuration.</summary>
procedure AConfiguration_setKeysHidden(Config: PAConfiguration; keysHidden: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setKeysHidden';
{$EXTERNALSYM AConfiguration_setKeysHidden}

/// <summary>Return the current ACONFIGURATION_NAVHIDDEN_* set in the
/// configuration.</summary>
function AConfiguration_getNavHidden(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getNavHidden';
{$EXTERNALSYM AConfiguration_getNavHidden}

/// <summary>Set the current nav hidden in the configuration.</summary>
procedure AConfiguration_setNavHidden(Config: PAConfiguration; navHidden: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setNavHidden';
{$EXTERNALSYM AConfiguration_setNavHidden}

/// <summary>Return the current SDK (API) version set in the configuration.
/// </summary>
function AConfiguration_getSdkVersion(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getSdkVersion';
{$EXTERNALSYM AConfiguration_getSdkVersion}

/// <summary>Set the current SDK version in the configuration.</summary>
procedure AConfiguration_setSdkVersion(Config: PAConfiguration; sdkVersion: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setSdkVersion';
{$EXTERNALSYM AConfiguration_setSdkVersion}

/// <summary>Return the current ACONFIGURATION_SCREENSIZE_* set in the
/// configuration.</summary>
function AConfiguration_getScreenSize(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getScreenSize';
{$EXTERNALSYM AConfiguration_getScreenSize}

/// <summary>Set the current screen size in the configuration.</summary>
procedure AConfiguration_setScreenSize(Config: PAConfiguration; screenSize: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setScreenSize';
{$EXTERNALSYM AConfiguration_setScreenSize}

/// <summary>Return the current ACONFIGURATION_SCREENLONG_* set in the
/// configuration.</summary>
function AConfiguration_getScreenLong(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getScreenLong';
{$EXTERNALSYM AConfiguration_getScreenLong}

/// <summary>Set the current screen long in the configuration.</summary>
procedure AConfiguration_setScreenLong(Config: PAConfiguration; screenLong: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setScreenLong';
{$EXTERNALSYM AConfiguration_setScreenLong}

/// <summary>Return the current ACONFIGURATION_UI_MODE_TYPE_* set in the
/// configuration.</summary>
function AConfiguration_getUiModeType(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getUiModeType';
{$EXTERNALSYM AConfiguration_getUiModeType}

/// <summary>Set the current UI mode type in the configuration.</summary>
procedure AConfiguration_setUiModeType(Config: PAConfiguration; uiModeType: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setUiModeType';
{$EXTERNALSYM AConfiguration_setUiModeType}

/// <summary>Return the current ACONFIGURATION_UI_MODE_NIGHT_* set in the
/// configuration.</summary>
function AConfiguration_getUiModeNight(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getUiModeNight';
{$EXTERNALSYM AConfiguration_getUiModeNight}

/// <summary>Set the current UI mode night in the configuration.</summary>
procedure AConfiguration_setUiModeNight(Config: PAConfiguration; uiModeNight: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setUiModeNight';
{$EXTERNALSYM AConfiguration_setUiModeNight}

///<remarks>Introduced in API 13</remarks>
/// <summary>Return the current configuration screen width in dp units, or
/// ACONFIGURATION_SCREEN_WIDTH_DP_ANY if not set.</summary>
function AConfiguration_getScreenWidthDp(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getScreenWidthDp';
{$EXTERNALSYM AConfiguration_getScreenWidthDp}

///<remarks>Introduced in API 13</remarks>
/// <summary>Set the configuration's current screen width in dp units.</summary>
procedure AConfiguration_setScreenWidthDp(Config: PAConfiguration; value: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setScreenWidthDp';
{$EXTERNALSYM AConfiguration_setScreenWidthDp}

///<remarks>Introduced in API 13</remarks>
/// <summary>Return the current configuration screen height in dp units, or
/// ACONFIGURATION_SCREEN_HEIGHT_DP_ANY if not set.</summary>
function AConfiguration_getScreenHeightDp(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getScreenHeightDp';
{$EXTERNALSYM AConfiguration_getScreenHeightDp}

///<remarks>Introduced in API 13</remarks>
/// <summary>Set the configuration's current screen height in dp units.</summary>
procedure AConfiguration_setScreenHeightDp(Config: PAConfiguration; value: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setScreenHeightDp';
{$EXTERNALSYM AConfiguration_setScreenHeightDp}

///<remarks>Introduced in API 13</remarks>
/// <summary>Return the current configuration's smallest screen width in dp
/// units, or ACONFIGURATION_SMALLEST_SCREEN_WIDTH_DP_ANY if not set.</summary>
function AConfiguration_getSmallestScreenWidthDp(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getSmallestScreenWidthDp';
{$EXTERNALSYM AConfiguration_getSmallestScreenWidthDp}

///<remarks>Introduced in API 13</remarks>
///<summary>Set the configuration's smallest screen width in dp units.</summary>
procedure AConfiguration_setSmallestScreenWidthDp(Config: PAConfiguration; value: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setSmallestScreenWidthDp';
{$EXTERNALSYM AConfiguration_setSmallestScreenWidthDp}

///<remarks>Introduced in API 17</remarks>
/// <summary>Return the configuration's layout direction, or
/// ACONFIGURATION_LAYOUTDIR_ANY if not set.</summary>
function AConfiguration_getLayoutDirection(Config: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_getLayoutDirection';
{$EXTERNALSYM AConfiguration_getLayoutDirection}

///<remarks>Introduced in API 17</remarks>
///<summary>Set the configuration's layout direction.</summary>
procedure AConfiguration_setLayoutDirection(Config: PAConfiguration; value: Int32); cdecl;
  external AndroidLib name 'AConfiguration_setLayoutDirection';
{$EXTERNALSYM AConfiguration_setLayoutDirection}

/// <summary>Perform a diff between two configurations. Returns a bit mask of
/// ACONFIGURATION_* constants, each bit set meaning that configuration
/// element is different between them.</summary>
function AConfiguration_diff(Config1, Config2: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_diff';
{$EXTERNALSYM AConfiguration_diff}

/// <summary>Determine whether 'BaseConfig' is a valid configuration for use
/// within the environment 'RequestedConfig'. Returns 0 if there are any values
/// in 'BaseConfig' that conflict with 'RequestedConfig'. Returns 1 if it does
/// not conflict.</summary>
function AConfiguration_match(BaseConfig, RequestedConfig: PAConfiguration): Int32; cdecl;
  external AndroidLib name 'AConfiguration_match';
{$EXTERNALSYM AConfiguration_match}

/// <summary>Determine whether the configuration in 'TestConfig' is better than
/// the existing configuration in 'BaseConfig'. If 'RequestedConfig' is non-nil,
/// this decision is based on the overall configuration given there. If it is
/// nil, this decision is simply based on which configuration is more specific.
/// Returns non-0 if 'TestConfig' is better than 'BaseConfig'.<br />
/// This assumes you have already filtered the configurations with
/// AConfiguration_match().</summary>
function AConfiguration_isBetterThan(BaseConfig, TestConfig, RequestedConfig: PAConfiguration) : Int32; cdecl;
  external AndroidLib name 'AConfiguration_isBetterThan';
{$EXTERNALSYM AConfiguration_isBetterThan}

implementation

end.

