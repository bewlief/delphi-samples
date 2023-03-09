{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.StorageManager;

interface

(*$HPPEMIT '#include <android/storage_manager.h>' *)

{$I Androidapi.inc}

{ The different states of a OBB storage passed to AStorageManager_obbCallbackFunc(). }
const
  /// <summary>The OBB container is now mounted and ready for use. Can be
  /// returned as the status for callbacks made during asynchronous OBB actions.
  /// </summary>
  AOBB_STATE_MOUNTED = 1;
  {$EXTERNALSYM AOBB_STATE_MOUNTED}

  /// <summary>The OBB container is now unmounted and not usable. Can be
  /// returned as the status for callbacks made during asynchronous OBB actions.
  /// </summary>
  AOBB_STATE_UNMOUNTED = 2;
  {$EXTERNALSYM AOBB_STATE_UNMOUNTED}

  /// <summary>There was an internal system error encountered while trying to
  /// mount the OBB. Can be returned as the status for callbacks made during
  /// asynchronous OBB actions.</summary>
  AOBB_STATE_ERROR_INTERNAL = 20;
  {$EXTERNALSYM AOBB_STATE_ERROR_INTERNAL}

  /// <summary>The OBB could not be mounted by the system. Can be returned as
  /// the status for callbacks made during asynchronous OBB actions.</summary>
  AOBB_STATE_ERROR_COULD_NOT_MOUNT = 21;
  {$EXTERNALSYM AOBB_STATE_ERROR_COULD_NOT_MOUNT}

  /// <summary>The OBB could not be unmounted. This most likely indicates that a
  /// file is in use on the OBB. Can be returned as the status for callbacks
  /// made during asynchronous OBB actions.</summary>
  AOBB_STATE_ERROR_COULD_NOT_UNMOUNT = 22;
  {$EXTERNALSYM AOBB_STATE_ERROR_COULD_NOT_UNMOUNT}

  /// <summary>A call was made to unmount the OBB when it was not mounted. Can
  /// be returned as the status for callbacks made during asynchronous OBB
  /// actions.</summary>
  AOBB_STATE_ERROR_NOT_MOUNTED = 23;
  {$EXTERNALSYM AOBB_STATE_ERROR_NOT_MOUNTED}

  /// <summary>The OBB has already been mounted. Can be returned as the status
  /// for callbacks made during asynchronous OBB actions.</summary>
  AOBB_STATE_ERROR_ALREADY_MOUNTED = 24;
  {$EXTERNALSYM AOBB_STATE_ERROR_ALREADY_MOUNTED}

  /// <summary>The current application does not have permission to use this OBB.
  /// This could be because the OBB indicates it's owned by a different package.
  /// Can be returned as the status for callbacks made during asynchronous OBB
  /// actions.</summary>
  AOBB_STATE_ERROR_PERMISSION_DENIED = 25;
  {$EXTERNALSYM AOBB_STATE_ERROR_PERMISSION_DENIED}

type
  /// <summary>AStorageManager manages application OBB storage, a pointer can be
  /// obtained with AStorageManager_new.</summary>
  AStorageManager = record end;
  {$EXTERNALSYM AStorageManager}
  PAStorageManager = ^AStorageManager;

  /// <summary>Callback function for asynchronous calls made on OBB files.<br />
  /// "State" is one of the following constants: AOBB_STATE_MOUNTED,
  /// AOBB_STATE_UNMOUNTED, AOBB_STATE_ERROR_INTERNAL,
  /// AOBB_STATE_ERROR_COULD_NOT_MOUNT, AOBB_STATE_ERROR_COULD_NOT_UNMOUNT,
  /// AOBB_STATE_ERROR_NOT_MOUNTED, AOBB_STATE_ERROR_ALREADY_MOUNTED,
  /// AOBB_STATE_ERROR_PERMISSION_DENIED</summary>
  AStorageManager_obbCallbackFunc = procedure(const Filename: MarshaledAString; const State: Int32; Data: Pointer);
  {$EXTERNALSYM AStorageManager_obbCallbackFunc}
  TAStorageManager_obbCallbackFunc = AStorageManager_obbCallbackFunc;

/// <summary>Obtains a new instance of AStorageManager.</summary>
function AStorageManager_new: PAStorageManager; cdecl;
  external AndroidLib name 'AStorageManager_new';
{$EXTERNALSYM AStorageManager_new}

/// <summary>Release AStorageManager instance.</summary>
procedure AStorageManager_delete(StorageManager: PAStorageManager); cdecl;
  external AndroidLib name 'AStorageManager_delete';
{$EXTERNALSYM AStorageManager_delete}

/// <summary>Attempts to mount an OBB file. This is an asynchronous operation.
/// </summary>
procedure AStorageManager_mountObb(StorageManager: PAStorageManager; const Filename: MarshaledAString; const Key: MarshaledAString;
        CallbackFunction: AStorageManager_obbCallbackFunc; Data: Pointer); cdecl;
  external AndroidLib name 'AStorageManager_mountObb';
{$EXTERNALSYM AStorageManager_mountObb}

/// <summary>Attempts to unmount an OBB file. This is an asynchronous operation.
/// </summary>
procedure AStorageManager_unmountObb(StorageManager: PAStorageManager; const Filename: MarshaledAString; const Force: Integer;
        CallbackFunction: AStorageManager_obbCallbackFunc; Data: Pointer);
  external AndroidLib name 'AStorageManager_unmountObb';
{$EXTERNALSYM AStorageManager_unmountObb}

/// <summary>Check whether an OBB is mounted.</summary>
function AStorageManager_isObbMounted(StorageManager: PAStorageManager; const Filename: MarshaledAString): Integer; cdecl;
  external AndroidLib name 'AStorageManager_isObbMounted';
{$EXTERNALSYM AStorageManager_isObbMounted}

/// <summary>Get the mounted path for an OBB.</summary>
function AStorageManager_getMountedObbPath(StorageManager: PAStorageManager; const Filename: MarshaledAString): MarshaledAString; cdecl;
  external AndroidLib name 'AStorageManager_getMountedObbPath';
{$EXTERNALSYM AStorageManager_getMountedObbPath}

implementation

end.
