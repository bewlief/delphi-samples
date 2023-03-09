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

unit Posix.Locale;

{$WEAKPACKAGEUNIT}
{$HPPEMIT NOUSINGNAMESPACE}

interface

uses Posix.Base;

{$IFDEF MACOS}
{$I osx/LocaleTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/LocaleTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/LocaleTypes.inc}
{$ENDIF}
{ Locale functions not implement in Android }

{$I LocaleAPI.inc}

(*$HPPEMIT '#include <locale.h>' *)

implementation

end.
