{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

function __NSGetEnviron: Pointer; cdecl;
  external libc name _PU + '_NSGetEnviron';
