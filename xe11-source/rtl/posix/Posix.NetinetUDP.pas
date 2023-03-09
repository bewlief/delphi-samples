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

unit Posix.NetinetUDP;

{$WEAKPACKAGEUNIT}
{$HPPEMIT NOUSINGNAMESPACE}

interface

uses Posix.Base;

{$IFDEF MACOS}
{$I osx/NetinetUDPTypes.inc}
{$ELSEIF defined(LINUX)}
{$I linux/NetinetUDPTypes.inc}
{$ELSEIF defined(ANDROID)}
{$I android/NetinetUDPTypes.inc}
{$ENDIF}

implementation

end.
