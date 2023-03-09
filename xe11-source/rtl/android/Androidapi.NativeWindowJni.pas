{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.NativeWindowJni;

interface

uses
  Androidapi.NativeWindow, Androidapi.Jni;

(*$HPPEMIT '#include <android/native_window_jni.h>' *)

{$I Androidapi.inc}

/// <summary>Return the ANativeWindow associated with a Java Surface object, for
/// interacting with it through native code. This acquires a reference on the
/// ANativeWindow that is returned; be sure to use ANativeWindow_release()
/// when done with it so that it doesn't leak.</summary>
function ANativeWindow_fromSurface(PEnv: PJNIEnv; Surface: JNIObject): PANativeWindow; cdecl;
  external AndroidLib name 'ANativeWindow_fromSurface';
{$EXTERNALSYM ANativeWindow_fromSurface}

(*
/// <remarks>Introduced in API 26</remarks>
/// <summary>Return a Java Surface object derived from the ANativeWindow, for
/// interacting with it through Java code. The returned Java object acquires a
/// reference on the ANativeWindow; maintains it through general Java object's
/// life cycle; and will automatically release the reference when the Java
/// object gets garbage collected.</summary>
function ANativeWindow_toSurface(PEnv: PJNIEnv; Window: PANativeWindow): JNIObject; cdecl;
  external AndroidLib name 'ANativeWindow_toSurface';
{$EXTERNALSYM ANativeWindow_toSurface}
*)

implementation

end.
