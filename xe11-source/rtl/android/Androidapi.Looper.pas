{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.Looper;

interface

(*$HPPEMIT '#include <android/looper.h>' *)

{$I Androidapi.inc}

{ Option for ALooper_prepare(). }
const
  /// <summary>This looper will accept calls to ALooper_addFd() that do not have
  /// a callback (that is provide nil for the callback). In this case the caller
  /// of ALooper_pollOnce() or ALooper_pollAll() MUST check the return from
  /// these functions to discover when data is available on such fds and process
  /// it.</summary>
  ALOOPER_PREPARE_ALLOW_NON_CALLBACKS = 1 shl 0;
  {$EXTERNALSYM ALOOPER_PREPARE_ALLOW_NON_CALLBACKS}

{ Result from ALooper_pollOnce() and ALooper_pollAll() }
const
  /// <summary>The poll was awoken using wake() before the timeout expired and
  /// no callbacks were executed and no other file descriptors were ready.
  /// </summary>
  ALOOPER_POLL_WAKE = -1;
  {$EXTERNALSYM ALOOPER_POLL_WAKE}

  /// <summary>One or more callbacks were executed.</summary>
  ALOOPER_POLL_CALLBACK = -2;
  {$EXTERNALSYM ALOOPER_POLL_CALLBACK}

  /// <summary>The timeout expired.</summary>
  ALOOPER_POLL_TIMEOUT = -3;
  {$EXTERNALSYM ALOOPER_POLL_TIMEOUT}

  /// <summary>An error occurred.</summary>
  ALOOPER_POLL_ERROR = -4;
  {$EXTERNALSYM ALOOPER_POLL_ERROR}

{ Flags for file descriptor events that a looper can monitor.

  These flag bits can be combined to monitor multiple events at once. }

const
  /// <summary>The file descriptor is available for read operations.</summary>
  ALOOPER_EVENT_INPUT = 1 shl 0;
  {$EXTERNALSYM ALOOPER_EVENT_INPUT}

  /// <summary>The file descriptor is available for write operations.</summary>
  ALOOPER_EVENT_OUTPUT = 1 shl 1;
  {$EXTERNALSYM ALOOPER_EVENT_OUTPUT}

  /// <summary>The file descriptor has encountered an error condition.<br />
  /// The looper always sends notifications about errors; it is not necessary to
  /// specify this event flag in the requested event set.</summary>
  ALOOPER_EVENT_ERROR = 1 shl 2;
  {$EXTERNALSYM ALOOPER_EVENT_ERROR}

  /// <summary>The file descriptor was hung up. For example, indicates that the
  /// remote end of a pipe or socket was closed.<br />
  /// The looper always sends notifications about hangups; it is not necessary
  /// to specify this event flag in the requested event set.</summary>
  ALOOPER_EVENT_HANGUP = 1 shl 3;
  {$EXTERNALSYM ALOOPER_EVENT_HANGUP}

  /// <summary>The file descriptor is invalid. For example, the file descriptor
  /// was closed prematurely.<br />
  /// The looper always sends notifications about invalid file descriptors; it
  /// is not necessary to specify this event flag in the requested event set.
  /// </summary>
  ALOOPER_EVENT_INVALID = 1 shl 4;
  {$EXTERNALSYM ALOOPER_EVENT_INVALID}

type
  /// <summary>ALooper<br />
  /// A looper is the state tracking an event loop for a thread.<br />
  /// Loopers do not define event structures or other such things; rather they
  /// are a lower-level facility to attach one or more discrete objects
  /// listening for an event. An "event" here is simply data available on a file
  /// descriptor: each attached object has an associated file descriptor, and
  /// waiting for "events" means (internally) polling on all of these file
  /// descriptors until one or more of them have data available.<br />
  /// A thread can have only one ALooper associated with it.</summary>
  ALooper = record end;
  {$EXTERNALSYM ALooper}

  PALooper = ^ALooper;

/// <summary>Returns the looper associated with the calling thread, or nil if
/// there is not one.</summary>
function ALooper_forThread: PALooper; cdecl;
  external AndroidLib name 'ALooper_forThread';
{$EXTERNALSYM ALooper_forThread}

/// <summary>Prepares a looper associated with the calling thread, and returns
/// it. If the thread already has a looper, it is returned. Otherwise, a new one
/// is created, associated with the thread, and returned.<br />
/// The Options may be ALOOPER_PREPARE_ALLOW_NON_CALLBACKS or 0.</summary>
function ALooper_prepare(Options: Integer): PALooper; cdecl;
  external AndroidLib name 'ALooper_prepare';
{$EXTERNALSYM ALooper_prepare}

/// <summary>Acquire a reference on the given ALooper object. This prevents the
/// object from being deleted until the reference is removed. This is only
/// needed to safely hand an ALooper from one thread to another.</summary>
procedure ALooper_acquire(Looper: PALooper); cdecl;
  external AndroidLib name 'ALooper_acquire';
{$EXTERNALSYM ALooper_acquire}

/// <summary>Remove a reference that was previously acquired with
/// ALooper_acquire().</summary>
procedure ALooper_release(Looper: PALooper); cdecl;
  external AndroidLib name 'ALooper_release';
{$EXTERNALSYM ALooper_release}

type
  /// <summary>For callback-based event loops, this is the prototype of the
  /// function that is called. It is given the file descriptor it is associated
  /// with, a bitmask of the poll events that were triggered (typically
  /// ALOOPER_EVENT_INPUT), and the data pointer that was originally
  /// supplied.<br />
  /// Implementations should return 1 to continue receiving callbacks, or 0 to
  /// have this file descriptor and callback unregistered from the looper.
  /// </summary>
  ALooper_callbackFunc = function(FileDescriptor, Events: Integer; Data: Pointer): Integer; cdecl;
  {$EXTERNALSYM ALooper_callbackFunc}

/// <summary>Waits for events to be available, with optional timeout in
/// milliseconds. Invokes callbacks for all file descriptors on which an event
/// occurred.<br />
/// If the timeout is zero, returns immediately without blocking. If the timeout
/// is negative, waits indefinitely until an event appears.<br />
/// Returns ALOOPER_POLL_WAKE if the poll was awoken using wake() before the
/// timeout expired and no callbacks were invoked and no other file
/// descriptors were ready.<br />
/// Returns ALOOPER_POLL_CALLBACK if one or more callbacks were invoked.<br />
/// Returns ALOOPER_POLL_TIMEOUT if there was no data before the given timeout
/// expired.<br />
/// Returns ALOOPER_POLL_ERROR if an error occurred.<br />
/// Returns a value >= 0 containing an identifier (the same identifier `Ident`
/// passed to ALooper_addFd()) if its file descriptor has data and it has no
/// callback function (requiring the caller here to handle it). In this (and
/// only this) case OutFileDescriptor, OutEvents and OutData will contain the
/// poll events and data associated with the fd, otherwise they will be set to
/// nil.<br />
/// This method does not return until it has finished invoking the appropriate
/// callbacks for all file descriptors that were signalled.</summary>
function ALooper_pollOnce(TimeOutMilliSeconds: Integer; OutFileDescriptor, OutEvents: PInteger; OutData: PPointer): Integer; cdecl;
  external AndroidLib name 'ALooper_pollOnce';
{$EXTERNALSYM ALooper_pollOnce}

/// <summary>Like ALooper_pollOnce(), but performs all pending callbacks until
/// all data has been consumed or a file descriptor is available with no
/// callback. This function will never return ALOOPER_POLL_CALLBACK.</summary>
function ALooper_pollAll(TimeOutMilliSeconds: Integer; OutFileDescriptor, OutEvents: PInteger; OutData: PPointer): Integer; cdecl;
  external AndroidLib name 'ALooper_pollAll';
{$EXTERNALSYM ALooper_pollAll}

/// <summary>Wakes the poll asynchronously.<br />
/// This method can be called on any thread.<br />
/// This method returns immediately.</summary>
procedure ALooper_wake(Looper: PALooper); cdecl;
  external AndroidLib name 'ALooper_wake';
{$EXTERNALSYM ALooper_wake}

/// <summary>Adds a new file descriptor to be polled by the looper. If the same
/// file descriptor was previously added, it is replaced.<br />
/// "FileDescriptor" is the file descriptor to be added.<br />
/// "Ident" is an identifier for this event, which is returned from
///   ALooper_pollOnce(). The identifier must be >= 0, or ALOOPER_POLL_CALLBACK
///   if providing a non-nil callback.<br />
/// "Events" are the poll events to wake up on. Typically this is
///   ALOOPER_EVENT_INPUT.<br />
/// "Callback" is the function to call when there is an event on the file
///   descriptor.<br />
/// "Data" is a private data pointer to supply to the callback.<br />
/// There are two main uses of this function:<br />
/// (1) If "Callback" is non-nil, then this function will be called when there
///   is data on the file descriptor. It should execute any events it has
///   pending, appropriately reading from the file descriptor. The 'Ident' is
///   ignored in this case.<br />
/// (2) If "Callback" is nil, the 'Ident' will be returned by ALooper_pollOnce
///   when its file descriptor has data available, requiring the caller to take
///   care of processing it.<br />
/// Returns 1 if the file descriptor was added or -1 if an error occurred.<br />
/// This method can be called on any thread.<br />
/// This method may block briefly if it needs to wake the poll.</summary>
function ALooper_addFd(Looper: PALooper; FileDescriptor, Ident, Events: Integer; Callback: ALooper_callbackFunc; Data: Pointer): Integer; cdecl;
  external AndroidLib name 'ALooper_addFd';
{$EXTERNALSYM ALooper_addFd}

/// <summary>Removes a previously added file descriptor from the looper.<br />
/// When this method returns, it is safe to close the file descriptor since the
/// looper will no longer have a reference to it. However, it is possible for
/// the callback to already be running or for it to run one last time if the
/// file descriptor was already signalled. Calling code is responsible for
/// ensuring that this case is safely handled. For example, if the callback
/// takes care of removing itself during its own execution either by
/// returning 0 or by calling this method, then it can be guaranteed to not
/// be invoked again at any later time unless registered anew.<br />
/// Returns 1 if the file descriptor was removed, 0 if none was previously
/// registered or -1 if an error occurred.<br />
/// This method can be called on any thread.<br />
/// This method may block briefly if it needs to wake the poll.</summary>
function ALooper_removeFd(Looper: PALooper; FileHandler: Integer): Integer; cdecl;
  external AndroidLib name 'ALooper_removeFd';
{$EXTERNALSYM ALooper_removeFd}

implementation

end.
