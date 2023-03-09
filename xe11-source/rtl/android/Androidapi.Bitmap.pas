{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Bitmap;

interface

uses Androidapi.Jni;

(*$HPPEMIT '#include <android/bitmap.h>' *)

{$I Androidapi.inc}

const
  ANDROID_BITMAP_RESULT_SUCCESS           = 0;
  {$EXTERNALSYM ANDROID_BITMAP_RESULT_SUCCESS}
  { Backward compatibility: this constant used to be misspelled. }
  ANDROID_BITMAP_RESUT_SUCCESS            = ANDROID_BITMAP_RESULT_SUCCESS;

  ANDROID_BITMAP_RESULT_BAD_PARAMETER     =-1;
  {$EXTERNALSYM ANDROID_BITMAP_RESULT_BAD_PARAMETER}
  
  ANDROID_BITMAP_RESULT_JNI_EXCEPTION     =-2;
  {$EXTERNALSYM ANDROID_BITMAP_RESULT_JNI_EXCEPTION}
  
  ANDROID_BITMAP_RESULT_ALLOCATION_FAILED =-3;
  {$EXTERNALSYM ANDROID_BITMAP_RESULT_ALLOCATION_FAILED}

type
  /// <summary>Bitmap pixel format.</summary>
  AndroidBitmapFormat = (
    /// <summary>No format.</summary>
    ANDROID_BITMAP_FORMAT_NONE      = 0,
    /// <summary>Red: 8 bits, Green: 8 bits, Blue: 8 bits, Alpha: 8 bits.</summary>
    ANDROID_BITMAP_FORMAT_RGBA_8888 = 1,
    /// <summary>Red: 5 bits, Green: 6 bits, Blue: 5 bits.</summary>
    ANDROID_BITMAP_FORMAT_RGB_565   = 4,
    /// <summary>Deprecated in API level 13. Because of the poor quality of this
    /// configuration, it is advised to use ARGB_8888 instead.</summary>
    ANDROID_BITMAP_FORMAT_RGBA_4444 = 7,
    /// <summary>Alpha: 8 bits.</summary>
    ANDROID_BITMAP_FORMAT_A_8       = 8
  );
  {$EXTERNALSYM AndroidBitmapFormat}

  /// <summary>Bitmap info, see AndroidBitmap_getInfo().</summary>
  AndroidBitmapInfo = record
    /// <summary>The bitmap width in pixels.</summary>
    width: UInt32;
    /// <summary>The bitmap height in pixels.</summary>
    height: UInt32;
    /// <summary>The number of byte per row.</summary>
    stride: UInt32;
    /// <summary>The bitmap pixel format. See AndroidBitmapFormat</summary>
    format: Int32;
    /// <summary>Unused. 0 for now.</summary>
    flags: UInt32;
  end;
  {$EXTERNALSYM AndroidBitmapInfo}
  
  PAndroidBitmapInfo = ^AndroidBitmapInfo;

/// <summary>Given a java bitmap object, fill out the AndroidBitmap struct for
/// it. If the call fails, the info parameter will be ignored</summary>
function AndroidBitmap_getInfo(PEnv: PJNIEnv; JBitmap: JNIObject; BitmapInfo: PAndroidBitmapInfo): Integer; cdecl;
   external AndroidJniGraphicsLib name 'AndroidBitmap_getInfo';
{$EXTERNALSYM AndroidBitmap_getInfo}

/// <summary>Given a java bitmap object, attempt to lock the pixel address.
/// Locking will ensure that the memory for the pixels will not move
/// until the unlockPixels call, and ensure that, if the pixels had been
/// previously purged, they will have been restored.<br />
/// If this call succeeds, it must be balanced by a call to
/// AndroidBitmap_unlockPixels, after which time the address of the
/// pixels should no longer be used.<br />
/// If this succeeds, AddrPtr^ will be set to the pixel address. If the
/// call fails, AddrPtr will be ignored.</summary>
function AndroidBitmap_lockPixels(PEnv: PJNIEnv; JBitmap: JNIObject; AddrPtr: PPointer): Integer; cdecl;
   external AndroidJniGraphicsLib name 'AndroidBitmap_lockPixels';
{$EXTERNALSYM AndroidBitmap_lockPixels}

/// <summary>Call this to balanace a successful call to AndroidBitmap_lockPixels
/// </summary>
function AndroidBitmap_unlockPixels(PEnv: PJNIEnv; Jbitmap: JNIObject): Integer; cdecl;
   external AndroidJniGraphicsLib name 'AndroidBitmap_unlockPixels';
{$EXTERNALSYM AndroidBitmap_unlockPixels}

implementation

end.
