{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

type

  timer_t = Pointer;
  {$EXTERNALSYM timer_t}
  Ptimer_t = ^timer_t;

  blkcnt_t = LongWord;
  {$EXTERNALSYM blkcnt_t}
  Pblkcnt_t = ^blkcnt_t;

  blksize_t = LongWord;
  {$EXTERNALSYM blksize_t}
  Pblksize_t = ^blksize_t;

  clock_t = Longint;
  {$EXTERNALSYM clock_t}
  Pclock_t = ^clock_t;

  clockid_t = Int32;
  {$EXTERNALSYM clockid_t}
  Pclockid_t = ^clockid_t;

  dev_t = LongWord;
  {$EXTERNALSYM dev_t}
  Pdev_t = ^dev_t;

  fsblkcnt_t = LongWord;
  {$EXTERNALSYM fsblkcnt_t}
  Pfsblkcnt_t = ^fsblkcnt_t;

  fsfilcnt_t = LongWord;
  {$EXTERNALSYM fsfilcnt_t}
  Pfsfilcnt_t = ^fsfilcnt_t;

  gid_t = UInt32;
  {$EXTERNALSYM gid_t}
  Pgid_t = ^gid_t;

  id_t = UInt32;
  {$EXTERNALSYM id_t}
  _Pid_t = ^id_t;

  ino_t = LongWord;
  {$EXTERNALSYM ino_t}
  Pino_t = ^ino_t;

  key_t = Int32;
  {$EXTERNALSYM key_t}
  Pkey_t = ^key_t;

  {$IFDEF CPU64BITS}
  mode_t = UInt32;
  {$ELSE !CPU64BITS}
  mode_t = UInt16;
  {$ENDIF CPU64BITS}
  {$EXTERNALSYM mode_t}
  Pmode_t = ^mode_t;

  nlink_t = UInt32;
  {$EXTERNALSYM nlink_t}
  Pnlink_t = ^nlink_t;

{$IFNDEF _OFF_T_DEFINED_}
{$DEFINE _OFF_T_DEFINED_}
  off_t = Longint;
  {$EXTERNALSYM off_t}
  Poff_t = ^off_t;

  off64_t = Int64;
  {$EXTERNALSYM off64_t}
  Poff64_t = ^off64_t;
{$ENDIF}

  pid_t = Int32;
  {$EXTERNALSYM pid_t}
  Ppid_t = ^pid_t;

  ssize_t = Longint;
  {$EXTERNALSYM ssize_t}
  Pssize_t = ^ssize_t;

  {$IFDEF CPU64BITS}
  uid_t = UInt32;
  {$ELSE !CPU64BITS}
  uid_t = UInt16;
  {$ENDIF CPU64BITS}
  {$EXTERNALSYM uid_t}
  Puid_t = ^uid_t;

  suseconds_t = NativeInt;
  {$EXTERNALSYM suseconds_t}
  Psuseconds_t = ^suseconds_t;

{ PThreads Support }

type

 uint32_t = UInt32;
 {$EXTERNALSYM uint32_t}
 int32_t = Int32;
 {$EXTERNALSYM int32_t}

{$IFNDEF _PTHREAD_ATTR_T_DEFINED}
  pthread_attr_t = record
    flags: uint32_t;
    stack_base: Pointer;
    stack_size: size_t;
    guard_size: size_t;
    sched_policy: int32_t;
    sched_priority: int32_t;
{$IFDEF CPU64BITS}
    __reserved: array[1..16] of byte;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_attr_t}
{$ENDIF _PTHREAD_ATTR_T_DEFINED}
  Ppthread_attr_t = ^pthread_attr_t;

  pthread_barrier_t = record
{$IFDEF CPU64BITS}
    __private: array[1..4] of Int64;
{$ELSE !CPU64BITS}
    __private: array[1..8] of Int32;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_barrier_t}
  Ppthread_barrier_t = ^pthread_barrier_t;
  {$EXTERNALSYM Ppthread_barrier_t}

  pthread_cond_t = record
{$IFDEF CPU64BITS}
    value: array [1..12] of int32_t;
{$ELSE !CPU64BITS}
    value: int32_t;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_cond_t}

  pthread_condattr_t = NativeInt;
  {$EXTERNALSYM pthread_condattr_t}
  Ppthread_condattr_t = ^pthread_condattr_t;

  pthread_key_t = Int32;
  {$EXTERNALSYM pthread_key_t}

  pthread_mutex_t = record
{$IFDEF CPU64BITS}
    value: array[1..10] of int32_t;
{$ELSE !CPU64BITS}
    value: int32_t;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_mutex_t}

  pthread_once_t = Int32;
  {$EXTERNALSYM pthread_once_t}

  pthread_rwlock_t = record
{$IFDEF CPU64BITS}
    __private: array[1..14] of int32_t;
{$ELSE !CPU64BITS}
    __private: array[1..10] of int32_t;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_rwlock_t}

  pthread_spinlock_t = record
{$IFDEF CPU64BITS}
    __private: Int64;
{$ELSE !CPU64BITS}
    __private: array[1..2] of int32_t;
{$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM pthread_spinlock_t}

  pthread_rwlockattr_t = NativeInt;
  {$EXTERNALSYM pthread_rwlockattr_t}
  Ppthread_rwlockattr_t = ^pthread_rwlockattr_t;

  pthread_t = NativeInt;
  {$EXTERNALSYM pthread_t}

  pthread_mutexattr_t = NativeInt;
  {$EXTERNALSYM pthread_mutexattr_t}
  Ppthread_mutexattr_t = ^pthread_mutexattr_t;

  time_t = NativeInt;
  {$EXTERNALSYM time_t}
  Ptime_t = ^time_t;

  useconds_t = UInt32;
  {$EXTERNALSYM useconds_t}
  Puseconds_t = ^useconds_t;

  caddr_t = MarshaledAString platform;
  {$EXTERNALSYM caddr_t}
