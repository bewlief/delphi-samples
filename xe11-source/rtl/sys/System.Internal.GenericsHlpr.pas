{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{*******************************************************}
{  Helpers for C++ Generics binding.                    }
{*******************************************************}

unit System.Internal.GenericsHlpr;

{$WEAKPACKAGEUNIT}
{$HPPEMIT NOUSINGNAMESPACE}

interface

uses System.Generics.Collections;

procedure InitInstantiations(I: Integer);

implementation

uses System.SysUtils;

type
  TListHlpr = class
    class function HelperGetValue<T>(): T; static;
  end;

class function TListHlpr.HelperGetValue<T>(): T;
var
  Val: T;
  L: TList<T>;
begin
  L := TList<T>.Create;
  try
    L.Add(Val);
    Result := L[0];
  finally
    L.Free;
  end
end;


function ListGetValue(I: Integer): Variant;
begin
  case I of
    0:  Result := TListHlpr.HelperGetValue<string>();
{$IFNDEF NEXTGEN}
    1:  Result := TListHlpr.HelperGetValue<AnsiChar>();
    2:  Result := TListHlpr.HelperGetValue<WideString>();
    3:  Result := TListHlpr.HelperGetValue<AnsiString>();
{$ENDIF}
    4:  Result := TListHlpr.HelperGetValue<UTF8String>();
    5:  Result := TListHlpr.HelperGetValue<Cardinal>();
    6:  Result := TListHlpr.HelperGetValue<Integer>();
    7:  Result := TListHlpr.HelperGetValue<ShortInt>();
    8:  Result := TListHlpr.HelperGetValue<Word>();
    9:  Result := TListHlpr.HelperGetValue<Byte>();
    10: Result := TListHlpr.HelperGetValue<SmallInt>();
    11: Result := TListHlpr.HelperGetValue<Char>();
    12: Result := TListHlpr.HelperGetValue<Boolean>();
    13: Result := TListHlpr.HelperGetValue<WideChar>();
    14: Result := TListHlpr.HelperGetValue<Single>();
    15: Result := TListHlpr.HelperGetValue<Real>();
    16: Result := TListHlpr.HelperGetValue<Double>();
    17: Result := TListHlpr.HelperGetValue<Extended>();
    18: Result := TListHlpr.HelperGetValue<Int64>();
    19: Result := TListHlpr.HelperGetValue<UInt64>();
    20: Result := TListHlpr.HelperGetValue<Currency>();
    21: Result := TListHlpr.HelperGetValue<TDateTime>();
    22:           TListHlpr.HelperGetValue<Pointer>();
  end;
end;

procedure InitInstantiations(I: Integer);
begin
  ListGetValue(I);
end;

end.
