{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.Utime;

{$WEAKPACKAGEUNIT}
{$HPPEMIT NOUSINGNAMESPACE}

interface

uses Posix.Base, Posix.SysTypes;

(*$HPPEMIT '#include <utime.h>' *)

{$IFDEF MACOS}
{$I osx/UtimeTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/UtimeTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/UtimeTypes.inc}
{$ENDIF}

{$I UtimeAPI.inc}

implementation

end.
