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

unit Posix.Grp;

{$WEAKPACKAGEUNIT}
{$HPPEMIT NOUSINGNAMESPACE}

interface

uses Posix.Base, Posix.SysTypes;

{$HPPEMIT '#include <grp.h>' }

{$IFDEF MACOS}
{$I osx/GrpTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/GrpTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/GrpTypes.inc}
{$ENDIF}

{$I GrpAPI.inc}

implementation

end.
