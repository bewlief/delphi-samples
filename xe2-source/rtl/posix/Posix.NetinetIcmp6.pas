{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit Posix.NetinetIcmp6 platform;

{$WEAKPACKAGEUNIT}

interface

uses
  Posix.Base, Posix.SysTypes, Posix.NetinetIn;

{$HPPEMIT '#include <netinet/icmp6.h>' }

{$IFDEF MACOS}
{$I osx/NetinetIcmp6Types.inc}
{$ENDIF MACOS}
{$IFDEF LINUX}
{$I linux/NetinetIcmp6Types.inc}
{$ENDIF LINUX}

function ICMP6_FILTER_WILLPASS(__type: Integer; const filterp: icmp6_filter): Boolean;
{$EXTERNALSYM ICMP6_FILTER_WILLPASS}
function ICMP6_FILTER_WILLBLOCK(__type: Integer; const filterp: icmp6_filter): Boolean;
{$EXTERNALSYM ICMP6_FILTER_WILLBLOCK}
procedure ICMP6_FILTER_SETPASS(__type: Integer; var filterp: icmp6_filter);
{$EXTERNALSYM ICMP6_FILTER_SETPASS}
procedure ICMP6_FILTER_SETBLOCK(__type: Integer; var filterp: icmp6_filter);
{$EXTERNALSYM ICMP6_FILTER_SETBLOCK}
procedure ICMP6_FILTER_SETPASSALL(var filterp: icmp6_filter);
{$EXTERNALSYM ICMP6_FILTER_SETPASSALL}
procedure ICMP6_FILTER_SETBLOCKALL(var filterp: icmp6_filter);
{$EXTERNALSYM ICMP6_FILTER_SETBLOCKALL}


implementation

function ICMP6_FILTER_WILLPASS(__type: Integer; const filterp: icmp6_filter): Boolean;
begin
  Result := (filterp.data[__type shr 5] and (1 shl (__type and 31))) = 0;
end;

function ICMP6_FILTER_WILLBLOCK(__type: Integer; const filterp: icmp6_filter): Boolean;
begin
  Result := (filterp.data[__type shr 5] and (1 shl (__type and 31))) <> 0;
end;

procedure ICMP6_FILTER_SETPASS(__type: Integer; var filterp: icmp6_filter);
begin
  filterp.data[__type shr 5] := filterp.data[__type shr 5] and not
                                      (1 shl (__type and 31));
end;

procedure ICMP6_FILTER_SETBLOCK(__type: Integer; var filterp: icmp6_filter);
begin
  filterp.data[__type shr 5] := filterp.data[__type shr 5] or
                                      (1 shl (__type and 31));
end;

procedure ICMP6_FILTER_SETPASSALL(var filterp: icmp6_filter);
begin
  FillChar(filterp, SizeOf(filterp), 0);
end;

procedure ICMP6_FILTER_SETBLOCKALL(var filterp: icmp6_filter);
begin
  FillChar(filterp, SizeOf(filterp), $FF);
end;

end.
