{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{ wchar.h types }

type
  {$IFDEF CPU64BITS}
  wctype_t = Int64;
  {$ELSE !CPU64BITS}
  wctype_t = Int32;
  {$ENDIF CPU64BITS}
  {$EXTERNALSYM wctype_t}
  Pwctype_t = ^wctype_t;

  wint_t = UInt32;
  {$EXTERNALSYM wint_t}
  Pwint_t = ^wint_t;

  mbstate_t = record
    __seq: array[1..4] of Byte;
  {$IFDEF CPU64BITS}
    __reserved: array[1..4] of Byte;
  {$ENDIF CPU64BITS}
  end;
  {$EXTERNALSYM mbstate_t}
  Pmbstate_t = ^mbstate_t;
