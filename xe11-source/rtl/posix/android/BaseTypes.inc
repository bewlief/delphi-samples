{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

const
{$IFDEF ANDROID32}
  libc = '/system/lib/libc.so';
  libpthread = '/system/lib/libc.so';
  //libiconv = '/system/lib/libc.so';  //not supported on android
  libdl = '/system/lib/libdl.so';
{$ENDIF ANDROID32}
{$IFDEF ANDROID64}
  libc = '/system/lib64/libc.so';
  libpthread = '/system/lib64/libc.so';
  //libiconv = '/system/lib/libc.so';  //not supported on android
  libdl = '/system/lib64/libdl.so';
{$ENDIF ANDROID64}

{$IF not Declared(_PU)}
const
  // On Android, cdecl names don't have a preceeding underscore
  _PU = '';
  {$EXTERNALSYM _PU}
{$ENDIF}

const
  _INODE_SUFFIX = '';
