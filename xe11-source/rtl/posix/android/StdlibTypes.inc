{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{ stdlib.h }
type
  div_t = record
    quot: Int32;
    rem: Int32;
  end;
  {$EXTERNALSYM div_t}
  Pdiv_t = ^div_t;

  ldiv_t = record
    quot: Longint;
    rem: Longint;
  end;
  {$EXTERNALSYM ldiv_t}
  Pldiv_t = ^ldiv_t;

  lldiv_t = record
    quot: Int64;
    rem: Int64;
  end;
  {$EXTERNALSYM lldiv_t}
  Plldiv_t = ^lldiv_t;

  TCompareProc = function(const p1, p2: Pointer): Integer; cdecl;
  TAtexitFunc = procedure;
const
  RAND_MAX = $7FFFFFFF;
  {$EXTERNALSYM RAND_MAX}
  EXIT_FAILURE = 1;
  {$EXTERNALSYM EXIT_FAILURE}
  EXIT_SUCCESS = 0;
  {$EXTERNALSYM EXIT_SUCCESS}
  MB_CUR_MAX = 1;
  {$EXTERNALSYM MB_CUR_MAX}
