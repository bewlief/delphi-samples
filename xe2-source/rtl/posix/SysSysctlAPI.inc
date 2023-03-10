{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}


function sysctl(name: PInteger; namelen: cardinal; 
  oldp: Pointer; oldlen: Psize_t;
  newp: Pointer; newlen: size_t): Integer; cdecl;
  external libc name _PU + 'sysctl';
{$EXTERNALSYM sysctl}

function sysctlbyname(Name: PAnsiChar; 
  oldp: Pointer; oldlen: Psize_t;
  newp: Pointer; newlen: size_t): Integer; cdecl;
  external libc name _PU + 'sysctlbyname';
{$EXTERNALSYM sysctlbyname}

function sysctlnametomib(name: PAnsiChar; mibp: PInteger; 
  sizep: Psize_t): Integer; cdecl;
  external libc name _PU + 'sysctlnametomib';
{$EXTERNALSYM sysctlnametomib}
