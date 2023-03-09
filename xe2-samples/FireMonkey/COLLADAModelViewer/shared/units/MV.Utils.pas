
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MV.Utils;

interface

uses FMX.Types3D, FMX.Types, System.UITypes, Classes, SysUtils, Types;

const
  SHARED_PATH = '..\..\..\..\..\shared\';
  IMAGES_PATH = SHARED_PATH + 'textures\';
  TEXTURES_PATH = SHARED_PATH + 'textures\';
  MODELS_PATH = SHARED_PATH + 'models\';
  SHADERS_PATH = SHARED_PATH + 'shaders\';



function PointDistance2(const v1, v2: TPoint3D): Single;   overload;
function PointDistance2(const v1, v2: TPointF): Single;   overload;
function PointAdd(const v1: TPoint3D; const v2: TPoint3D): TPoint3D;
function ColorToVector3D(const AColor: TColor):Tvector3D;
function CreateTranslationMatrix3D(const AVec: TVector3D): TMatrix3D;
function CreateScaleMatrix3D(const AScale: TVector3D): TMatrix3D;
function Matrix3D(const m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, m41, m42, m43, m44:Single): TMatrix3D;
function Matrix3DA(const LArr: TSingleDynArray):TMatrix3D;
function QuaternionRot(const R, P, Y: Single): TQuaternion3D;
function CreateRotMatrix3D(const Y, P, R: Single): TMatrix3D;
function SphericalToRect(const ARotation: TPointf; ARadius:Single) : TVector3D;

function FloatStringsToSingleDynArray(const AStr: string): TSingleDynArray;
function IntStringsToIntegerDynArray(const AStr: string): TIntegerDynArray; overload;
function StringsToStringDynArray(const AStr: string): TStringDynArray; overload;

implementation
{------------------------------------------------------------------------------
  Math
}


function SphericalToRect(const ARotation: TPointf; ARadius:Single) : TVector3D;
var
  sinr: single;
const
  DEG_TO_RAD = pi/180;
begin
  sinr := ARadius * Sin(ARotation.y * DEG_TO_RAD);
  result.X := sinr * Cos(ARotation.x * DEG_TO_RAD);
  result.Y := sinr * Sin(ARotation.x * DEG_TO_RAD);
  result.Z := ARadius * Cos(ARotation.y * DEG_TO_RAD);
end;

function ColorToVector3D(const AColor: TColor):Tvector3D;
var
  LColorRec: TColorRec;
begin
  LColorRec.Color := AColor;
  result := Vector3D(LColorRec.R/128,LColorRec.G/128,LColorRec.B/128,1.0);
end;


function QuaternionRot(const R, P, Y: Single): TQuaternion3D;
var
  qp, qy, qR: TQuaternion3D;
begin
  qR := QuaternionFromAngleAxis(R, Vector3D(0, 0, 1));
  qp := QuaternionFromAngleAxis(P, Vector3D(0, 1, 0));
  qy := QuaternionFromAngleAxis(Y, Vector3D(1, 0, 0));
  Result := QuaternionMultiply(qR, QuaternionMultiply(qp, qy));
end;

function CreateRotMatrix3D(const Y, P, R: Single): TMatrix3D;
var
  q: TQuaternion3D;
begin
  q := QuaternionRot(R, P, Y);
  Result := QuaternionToMatrix(q);
end;

function Matrix3D(const
  m11, m12, m13, m14,
  m21, m22, m23, m24,
  m31, m32, m33, m34,
  m41, m42, m43, m44 : Single): TMatrix3D;
begin
  Result.m11 := m11; Result.m12 := m12; Result.m13 := m13; Result.m14 := m14;
  Result.m21 := m21; Result.m22 := m22; Result.m23 := m23; Result.m24 := m24;
  Result.m31 := m31; Result.m32 := m32; Result.m33 := m33; Result.m34 := m34;
  Result.m41 := m41; Result.m42 := m42; Result.m43 := m43; Result.m44 := m44;
end;



function Matrix3DA(const LArr: TSingleDynArray):TMatrix3d;
begin
    result := Matrix3D(
      LArr[0],LArr[4],LArr[8],LArr[12],
      LArr[1],LArr[5],LArr[9],LArr[13],
      LArr[2],LArr[6],LArr[10],LArr[14],
      LArr[3],LArr[7],LArr[11],LArr[15]);
end;


function CreateTranslationMatrix3D(const AVec: TVector3D): TMatrix3D;
begin
{
  Result.m11 := 1;      Result.m21 := 0;      Result.m31 := 0;      Result.m41 := 0;
  Result.m12 := 0;      Result.m22 := 1;      Result.m32 := 0;      Result.m42 := 0;
  Result.m13 := 0;      Result.m23 := 0;      Result.m33 := 1;      Result.m43 := 0;
  Result.m14 := AVec.x; Result.m24 := AVec.y; Result.m34 := AVec.z; Result.m44 := 1;    }

  Result.m11 := 1;      Result.m21 := 0;      Result.m31 := 0;      Result.m41 := AVec.x;
  Result.m12 := 0;      Result.m22 := 1;      Result.m32 := 0;      Result.m42 := AVec.y;
  Result.m13 := 0;      Result.m23 := 0;      Result.m33 := 1;      Result.m43 := AVec.z;
  Result.m14 := 0; Result.m24 := 0; Result.m34 := 0; Result.m44 := 1;

end;


function CreateScaleMatrix3D(const AScale: TVector3D): TMatrix3D;
begin
  Result.M[0].V[0] := AScale.x;
  Result.M[0].V[1] := 0;
  Result.M[0].V[2] := 0;
  Result.M[0].V[3] := 0;

  Result.M[1].V[0] := 0;
  Result.M[1].V[1] := AScale.y;
  Result.M[1].V[2] := 0;
  Result.M[1].V[3] := 0;

  Result.M[2].V[0] := 0;
  Result.M[2].V[1] := 0;
  Result.M[2].V[2] := AScale.z;
  Result.M[2].V[3] := 0;

  Result.M[3].V[0] := 0;
  Result.M[3].V[1] := 0;
  Result.M[3].V[2] := 0;
  Result.M[3].V[3] := 1;
end;


function PointAdd(const v1: TPoint3D; const v2: TPoint3D): TPoint3D;
begin
  Result.x := v1.x + v2.x;
  Result.y := v1.y + v2.y;
  Result.z := v1.z + v2.z;
end;

function PointDistance2(const v1, v2: TPoint3D): Single;
begin
  Result := Sqr(v2.x - v1.x) + Sqr(v2.y - v1.y) + Sqr(v2.z - v1.z);
end;

function PointDistance2(const v1, v2: TPointF): Single;
begin
  Result := Sqr(v2.x - v1.x) + Sqr(v2.y - v1.y);
end;

function FloatStringsToSingleDynArray(const AStr: string): TSingleDynArray;
var
  str: string;
  c: PChar;

  LFormatSettings: TFormatSettings;

  procedure TryAdd;
  begin
    if str = '' then
      Exit;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := StrToFloat(str, LFormatSettings);
    str := '';
  end;

begin
  Result := nil;

  if AStr = '' then
    Exit;

  LFormatSettings := TFormatSettings.Create;

  c := @AStr[1];
  while c^ <> #0 do
  begin
    case c^ of
      #1..#32: TryAdd;
    else
      case c^ of
        '.': LFormatSettings.DecimalSeparator := '.';
        ',': LFormatSettings.DecimalSeparator := ',';
      end;

      str := str + c^;
    end;

    Inc(c);
  end;

  TryAdd;
end;

function IntStringsToIntegerDynArray(const AStr: string): TIntegerDynArray;
var
  str: string;
  c: PChar;

  procedure TryAdd;
  begin
    if str = '' then
      Exit;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := StrToInt(str);
    str := '';
  end;

begin
  Result := nil;

  if AStr = '' then
    Exit;

  c := @AStr[1];
  while c^ <> #0 do
  begin
    case c^ of
      #1..#32: TryAdd;
    else
      str := str + c^;
    end;

    Inc(c);
  end;

  TryAdd;
end;

function StringsToStringDynArray(const AStr: string): TStringDynArray; overload;
var
  str: string;
  c: PChar;

  procedure TryAdd;
  begin
    if str = '' then
      Exit;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := str;
    str := '';
  end;

begin
  Result := nil;

  if AStr = '' then
    Exit;

  c := @AStr[1];
  while c^ <> #0 do
  begin
    case c^ of
      #1..#32: TryAdd;
    else
      str := str + c^;
    end;

    Inc(c);
  end;

  TryAdd;
end;

end.
