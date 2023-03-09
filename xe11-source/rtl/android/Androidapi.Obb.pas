{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Obb;

interface

uses Posix.SysTypes;

(*$HPPEMIT '#include <android/obb.h>' *)

{$I Androidapi.inc}

const
  /// <summary>Flag for an obb file, returned by AObbInfo_getFlags().</summary>
  AOBBINFO_OVERLAY = $0001;
  {$EXTERNALSYM AOBBINFO_OVERLAY}

type
  /// <summary>AObbInfo is an opaque type representing information for obb
  /// storage.</summary>
  AObbInfo = record end;
  {$EXTERNALSYM AObbInfo}  
  PAObbInfo = ^AObbInfo;

/// <summary>Scan an OBB and get information about it.</summary>
function AObbScanner_getObbInfo(const FileName: MarshaledAString): PAObbInfo; cdecl;
  external AndroidLib name 'AObbScanner_getObbInfo';
{$EXTERNALSYM AObbScanner_getObbInfo}

/// <summary>Destroy the AObbInfo object. You must call this when finished with
/// the object.</summary>
procedure AObbInfo_delete(ObbInfo: PAObbInfo); cdecl;
  external AndroidLib name 'AObbInfo_delete';
{$EXTERNALSYM AObbInfo_delete}

/// <summary>Get the package name for the OBB.</summary>
function AObbInfo_getPackageName(ObbInfo: PAObbInfo): MarshaledAString; cdecl;
  external AndroidLib name 'AObbInfo_getPackageName';
{$EXTERNALSYM AObbInfo_getPackageName}

/// <summary>Get the version of an OBB file.</summary>
function AObbInfo_getVersion(ObbInfo: PAObbInfo): Int32; cdecl;
  external AndroidLib name 'AObbInfo_getVersion';
{$EXTERNALSYM AObbInfo_getVersion}

/// <summary>Get the flags of an OBB file.</summary>
function AObbInfo_getFlags(ObbInfo: PAObbInfo): Int32; cdecl;
  external AndroidLib name 'AObbInfo_getFlags';
{$EXTERNALSYM AObbInfo_getFlags}

implementation

end.
