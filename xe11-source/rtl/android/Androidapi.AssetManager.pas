{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.AssetManager;

interface

uses Posix.StdDef, Posix.SysTypes;

(*$HPPEMIT '#include <android/asset_manager.h>' *)

{$I Androidapi.inc}

{ Available modes for opening assets with AAssetManager_open }
const
  /// <summary>No specific information about how data will be accessed.</summary>
  AASSET_MODE_UNKNOWN   = 0;
  {$EXTERNALSYM AASSET_MODE_UNKNOWN}

  /// <summary>Read chunks, and seek forward and backward.</summary>
  AASSET_MODE_RANDOM    = 1;
  {$EXTERNALSYM AASSET_MODE_RANDOM}

  /// <summary>Read sequentially, with an occasional forward seek.</summary>
  AASSET_MODE_STREAMING = 2;
  {$EXTERNALSYM AASSET_MODE_STREAMING}

  /// <summary>Caller plans to ask for a read-only buffer with all data.</summary>
  AASSET_MODE_BUFFER    = 3;
  {$EXTERNALSYM AASSET_MODE_BUFFER}

type
  /// <summary>AAssetManager provides access to an application's raw assets by
  /// creating AAsset objects.<br />
  /// AAssetManager is a wrapper to the low-level native implementation
  /// of the java AAssetManager, a pointer can be obtained using
  /// AAssetManager_fromJava().<br />
  /// The asset hierarchy may be examined like a filesystem, using AAssetDir
  /// objects to peruse a single directory.<br />
  /// A native AAssetManager pointer may be shared across multiple threads.
  /// </summary>
  AAssetManager = record end;
  {$EXTERNALSYM AAssetManager}
  PAAssetManager = ^AAssetManager;

  /// <summary>AAssetDir provides access to a chunk of the asset hierarchy as if
  /// it were a single directory. The contents are populated by the
  /// AAssetManager.<br />
  /// The list of files will be sorted in ascending order by ASCII
  /// value.</summary>
  AAssetDir = record end;
  {$EXTERNALSYM AAssetDir}
  PAAssetDir = ^AAssetDir;

  /// <summary>AAsset provides access to a read-only asset.<br />
  /// AAsset objects are NOT thread-safe, and should not be shared across
  /// threads.</summary>
  AAsset = record end;
  {$EXTERNALSYM AAsset}
  PAAsset = ^AAsset;

/// <summary>Open the named directory within the asset hierarchy. The directory
/// can then be inspected with the AAssetDir functions. To open the top-level
/// directory, pass in "" as the dirName.<br />
/// The object returned here should be freed by calling AAssetDir_close().
/// </summary>
function AAssetManager_openDir(AssetManager: PAAssetManager; const DirectoryName: MarshaledAString): PAAssetDir; cdecl;
  external AndroidLib name 'AAssetManager_openDir';
{$EXTERNALSYM AAssetManager_openDir}

/// <summary>Open an asset.<br />
/// The object returned here should be freed by calling AAsset_close().</summary>
function AAssetManager_open(AssetManager: PAAssetManager; const FileName: MarshaledAString; Mode: Integer): PAAsset; cdecl;
  external AndroidLib name 'AAssetManager_open';
{$EXTERNALSYM AAssetManager_open}

/// <summary>Iterate over the files in an asset directory. A NULL string is
/// returned when all the file names have been returned.<br />
/// The returned file name is suitable for passing to AAssetManager_open().<br />
/// The string returned here is owned by the AssetDir implementation and is not
/// guaranteed to remain valid if any other calls are made on this AAssetDir
/// instance.</summary>
function AAssetDir_getNextFileName(AssetDirectory: PAAssetDir): MarshaledAString; cdecl;
  external AndroidLib name 'AAssetDir_getNextFileName';
{$EXTERNALSYM AAssetDir_getNextFileName}

/// <summary>Reset the iteration state of AAssetDir_getNextFileName() to the
/// beginning.</summary>
procedure AAssetDir_rewind(AssetDirectory: PAAssetDir); cdecl;
  external AndroidLib name 'AAssetDir_rewind';
{$EXTERNALSYM AAssetDir_rewind}

/// <summary>Close an opened AAssetDir, freeing any related resources.</summary>
procedure AAssetDir_close(AssetDirectory: PAAssetDir); cdecl;
  external AndroidLib name 'AAssetDir_close';
{$EXTERNALSYM AAssetDir_close}

/// <summary>Attempt to read 'Count' bytes of data from the current offset.<br />
/// Returns the number of bytes read, zero on EOF, or &lt; 0 on error.</summary>
function AAsset_read(Asset: PAAsset; Buffer: Pointer; Count: size_t): Integer; cdecl;
  external AndroidLib name 'AAsset_read';
{$EXTERNALSYM AAsset_read}

/// <summary>Seek to the specified offset within the asset data. 'Whence' uses
/// the same constants as lseek()/fseek().<br />
/// Returns the new position on success, or off_t(-1) on error.</summary>
function AAsset_seek(Asset: PAAsset; Offset: off_t; Whence: Integer): off_t; cdecl;
  external AndroidLib name 'AAsset_seek';
{$EXTERNALSYM AAsset_seek}

/// <summary>Close the asset, freeing all associated resources.</summary>
procedure AAsset_close(Asset: PAAsset); cdecl;
  external AndroidLib name 'AAsset_close';
{$EXTERNALSYM AAsset_close}

/// <summary>Get a pointer to a buffer holding the entire contents of the
/// asset.<br />
/// Returns nil on failure./// </summary>
function AAsset_getBuffer(Asset: PAAsset): Pointer; cdecl;
  external AndroidLib name 'AAsset_getBuffer';
{$EXTERNALSYM AAsset_getBuffer}

/// <summary>Report the total size of the asset data.</summary>
function AAsset_getLength(Asset: PAAsset): off_t; cdecl;
  external AndroidLib name 'AAsset_getLength';
{$EXTERNALSYM AAsset_getLength}

/// <summary>Report the total amount of asset data that can be read from the
/// current position.</summary>
function AAsset_getRemainingLength(Asset: PAAsset): off_t; cdecl;
  external AndroidLib name 'AAsset_getRemainingLength';
{$EXTERNALSYM AAsset_getRemainingLength}

/// <summary>Open a new file descriptor that can be used to read the asset
/// data.<br />
/// Returns &lt; 0 if direct fd access is not possible (for example, if the
/// asset is compressed).</summary>
function AAsset_openFileDescriptor(Asset: PAAsset; OutStart, PutLength: Poff_t): Integer; cdecl;
  external AndroidLib name 'AAsset_openFileDescriptor';
{$EXTERNALSYM AAsset_openFileDescriptor}

/// <summary>Returns whether this asset's internal buffer is allocated in
/// ordinary RAM (i.e. not mmapped).</summary>
function AAsset_isAllocated(asset: PAAsset): Integer; cdecl;
  external AndroidLib name 'AAsset_isAllocated';
{$EXTERNALSYM AAsset_isAllocated}

implementation

end.
