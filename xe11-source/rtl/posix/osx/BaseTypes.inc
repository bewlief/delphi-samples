{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

const
  libc        = '/usr/lib/libc.dylib';
  libpthread  = '/usr/lib/libpthread.dylib';
  libiconv    = '/usr/lib/libiconv.dylib';
  libdl       = '/usr/lib/libdl.dylib';

{$IF not Declared(_PU)}
const
  // On Mac OSX, cdecl names have a preceeding underscore
  // if x86 native backend.
  {$IF Defined(UNDERSCOREIMPORTNAME)}
  _PU = '_';
  {$ELSE}
  _PU = '';
  {$ENDIF}
  {$EXTERNALSYM _PU}
{$ENDIF}

const
{$IF Defined(OSX) and (Defined(CPUX86) or Defined(CPUX64))}
  _INODE_SUFFIX = '$INODE64';
{$ELSE}
  _INODE_SUFFIX = '';
{$ENDIF}
  {$EXTERNALSYM _INODE_SUFFIX}
