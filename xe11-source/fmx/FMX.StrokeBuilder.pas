{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.StrokeBuilder;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Math, System.Math.Vectors, FMX.Graphics, FMX.Canvas.GPU.Helpers;

{.$DEFINE SafeInitArrays}
{.$DEFINE BuildSanityChecks}

type
  TStrokeBuilder = class
  private
    FMatrix: TMatrix;
    FBrush: TStrokeBrush;

    FVertices: TCanvasHelper.TVertexArray;
    FColors: TCanvasHelper.TAlphaColorArray;
    FIndices: TCanvasHelper.TIndexArray;

    FCurrentVertex: Integer;
    FCurrentIndex: Integer;

    FSegmentCount: Integer;
    FLastSegmentFraction: Single;
    FExtraPieces: Integer;
    FLastDashExtend: Boolean;
    FThickness: Single;
    FHalfThickness: Single;
    FStrokeColor: TAlphaColor;

    FEllipseCenter: TPointF;
    FEllipseRadius: TPointF;
    FEllipseCircumf: Single;
    FEllipseTransfCenter: TPointF;
    FUndeterminedMode: Boolean;

    function GetMatrixScale: TPointF;

    procedure ArrayFillCheck;
    procedure InitArrays(const VertexCount, IndexCount: Integer); inline;
    procedure FinalizeArrays;
    procedure InitArrayPointers;

    procedure InsertVertex(const VertexPos: TPointF; const Color: TAlphaColor);
    procedure InsertIndex(const Value: Integer);
    function GetCapDivisions: Integer; inline;
    procedure GetDashEstimate(out VertexCount, IndexCount: Integer);
    procedure InsertDash(SrcPos, DestPos: TPointF; const DashDirVec, ThickPerp: TPointF);
    procedure GetDotEstimate(out VertexCount, IndexCount: Integer);
    procedure InsertDot(const MidPos, DotDirVec, ThickPerp: TPointF);
    function GetPatternStepCount: Integer;
    procedure ComputeBuildEstimates(const TentSegmentCount: Single; out VertexCount, IndexCount: Integer);
    procedure InsertSegment(const SegmentPos, SegDirVec, ThickPerp, DestPos: TPointF; IsLast: Boolean);

    function GetEllipseTransfAt(const Delta: Single): TPointF;
    procedure InsertEllipseSegment(const SegInitDist: Single; IsLast: Boolean);
  public
    procedure BuildLine(const SrcPos, DestPos: TPointF; const Opacity: Single);
    procedure BuildIntermEllipse(const Center, Radius: TPointF; const Opacity: Single);
    procedure BuildSolidEllipse(const Center, Radius: TPointF; const Opacity: Single);

    procedure BuildIntermPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean = False);
    procedure BuildSolidPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean = False);

    procedure BuildIntermPath(const Path: TPathData; const Opacity: Single);
    procedure BuildSolidPath(const Path: TPathData; const Opacity: Single);

    procedure ResetArrays;

    property Matrix: TMatrix read FMatrix write FMatrix;
    property Brush: TStrokeBrush read FBrush write FBrush;

    property Vertices: TCanvasHelper.TVertexArray read FVertices;
    property Colors: TCanvasHelper.TAlphaColorArray read FColors;
    property Indices: TCanvasHelper.TIndexArray read FIndices;
  end;

implementation

uses
  System.UIConsts, System.Generics.Collections, FMX.Types, System.SysUtils;

function PointFDot(const P1, P2: TPointF): Single;
begin
  Result := (P1.X * P2.X) + (P1.Y * P2.Y);
end;

type
  TVertex = record
    P: TPointF;
    I: Integer;
    class operator Implicit(const APoint: TPointF): TVertex;
  end;

  TEndCaps = (Start, Stop);
  TEndCap = set of TEndCaps;

  TStrokeBuilderHelper = class helper for TStrokeBuilder
    procedure StartSegment(const P1, P2: TPointF; var A, B: TVertex);
    procedure EdgePoints(const P, Direction: TPointF; var A, B: TVertex);
    function AddVertex(var V: TVertex): Integer;
    procedure Triangle(var A, B, C: TVertex);
    procedure Quad(var A, B, C, D: TVertex);
    procedure Arc(const O: TPointF; var A, B, C: TVertex; Clockwise: Boolean);
  end;

class operator TVertex.Implicit(const APoint: TPointF): TVertex;
begin
  Result.I := -1;
  Result.P := APoint;
end;

procedure Intersect(const A: TVertex; var B: TVertex; const C, D: TVertex);
var
  D1, D2: TPointF;
  Denom, Numer: Single;
begin
  D1 := B.P - A.P;
  D2 := D.P - C.P;
  Denom := D1.CrossProduct(D2);
  if Abs(Denom) < Epsilon then
    Exit;
  Numer := D2.CrossProduct(A.P - C.P) / Denom;
  B.I := -1;
  B.P := A.P + Numer * D1;
end;

procedure TStrokeBuilderHelper.StartSegment(const P1, P2: TPointF; var A, B: TVertex);
var
  D1: TPointF;
begin
  D1 := (P2 - P1).Normalize;
  EdgePoints(P1, D1, A, B);
end;

procedure TStrokeBuilderHelper.EdgePoints(const P, Direction: TPointF; var A, B: TVertex);
begin
  A.I := -1;
  A.P.X := P.X + FHalfThickness * direction.Y;
  A.P.Y := P.Y - FHalfThickness * direction.X;
  B.I := -1;
  B.P.X := P.X - FHalfThickness * direction.Y;
  B.P.Y := P.Y + FHalfThickness * direction.X;
end;

function TStrokeBuilderHelper.AddVertex(var V: TVertex): Integer;
begin
  if V.I < 0 then
  begin
    V.I := FCurrentVertex;
    InsertVertex(V.P, FStrokeColor);
  end;
  Result := V.I;
end;

procedure TStrokeBuilderHelper.Triangle(var A, B, C: TVertex);
begin
  AddVertex(A);
  AddVertex(B);
  AddVertex(C);
  InsertIndex(A.I);
  InsertIndex(B.I);
  InsertIndex(C.I);
end;

procedure TStrokeBuilderHelper.Quad(var A, B, C, D: TVertex);
begin
  AddVertex(A);
  AddVertex(B);
  AddVertex(D);
  AddVertex(C);
  InsertIndex(B.I);
  InsertIndex(C.I);
  InsertIndex(D.I);
  InsertIndex(D.I);
  InsertIndex(A.I);
  InsertIndex(B.I);
end;

procedure TStrokeBuilderHelper.Arc(const O: TPointF; var A, B, C: TVertex; Clockwise: Boolean);
var
  P1, P2: TPointF;
  A1, A2: Single;
  n: Integer;
  P, Z, V: TVertex;
  Angle, R: Single;
  SN, CS: Single;
begin
  P1 := B.P - O;
  P2 := C.P - O;
  A1 := ArcTan2(P1.Y, P1.X);
  A2 := ArcTan2(P2.Y, P2.X);
  if Clockwise then
  begin
    if A2 > A1 then
      A2 := A2 - 2 * PI;
  end
  else
  begin
    if A1 > A2 then
      A1 := A1 - 2 * PI;
  end;
  angle := A2 - A1;

  n := Round(Abs(angle) / (10 * PI / 180));
  if n < 1 then
    n := 1;
  P := B;
  Z := A;
  for var i := 0 to n - 2 do
  begin
    R := (i + 1) *  angle / n;
    SinCos(R, SN, CS);
    V.I := -1;
    V.P.X := O.X + CS * P1.X - SN * P1.Y;
    V.P.Y := O.Y + SN * P1.X + CS * P1.Y;
    Triangle(Z, P, V);
    P := V;
  end;
  V := C;
  Triangle(Z, P, V);
end;

{$REGION 'Stroke Builder implementation'}

function TStrokeBuilder.GetMatrixScale: TPointF;
const
  BaseVector: TPointF = (X: 0; Y: 0);
begin
  Result.X := (PointF(1, 0) * FMatrix).Distance(BaseVector * FMatrix);
  Result.Y := (PointF(0, 1) * FMatrix).Distance(BaseVector * FMatrix);
end;

procedure TStrokeBuilder.InitArrayPointers();
begin
  FCurrentVertex := 0;
  FCurrentIndex := 0;
end;

procedure TStrokeBuilder.InitArrays(const VertexCount, IndexCount: Integer);
{$IFDEF SafeInitArrays}
var
  Index: Integer;
{$ENDIF}
begin
  SetLength(FVertices, VertexCount);
  SetLength(FColors, VertexCount);
  SetLength(FIndices, IndexCount);

{$IFDEF SafeInitArrays}
  for Index := 0 to IndexCount - 1 do
    FIndices[Index] := -1;

  FillChar(FVertices[0], SizeOf(TPointF) * VertexCount, 0);
  FillChar(FColors[0], SizeOf(TAlphaColor) * VertexCount, 0);
{$ENDIF}
  InitArrayPointers();
end;

procedure TStrokeBuilder.ResetArrays;
begin
  SetLength(FVertices, 0);
  SetLength(FColors, 0);
  SetLength(FIndices, 0);
end;

procedure TStrokeBuilder.ArrayFillCheck;
begin
{$IFDEF BuildSanityChecks}
  Assert(FCurrentVertex = Length(FVertices), 'Vertices have not been filled correctly.');
  Assert(FCurrentIndex = Length(FIndices), 'Indices have not been filled correctly.');
{$ENDIF}
end;

procedure TStrokeBuilder.FinalizeArrays;
begin
  if FUndeterminedMode then
  begin
    SetLength(FVertices, FCurrentVertex);
    SetLength(FColors, FCurrentVertex);
    SetLength(FIndices, FCurrentIndex);
  end;
end;

procedure TStrokeBuilder.InsertVertex(const VertexPos: TPointF; const Color: TAlphaColor);
var
  NewValue: Integer;
begin
  if FUndeterminedMode and (Length(FVertices) <= FCurrentVertex) then
  begin
    NewValue := 8 + Ceil(Length(FVertices) * 1.5);

    SetLength(FVertices, NewValue);
    SetLength(FColors, NewValue);
  end;

{$IFDEF BuildSanityChecks}
  Assert(FCurrentVertex < Length(FVertices), 'Too many vertices.');
{$ENDIF}
  FVertices[FCurrentVertex] := VertexPos;
  FColors[FCurrentVertex] := Color;
  Inc(FCurrentVertex);
end;

procedure TStrokeBuilder.InsertIndex(const Value: Integer);
var
  NewValue: Integer;
begin
  if FUndeterminedMode and (Length(FIndices) <= FCurrentIndex) then
  begin
    NewValue := 12 + Ceil(Length(FIndices) * 1.5);
    SetLength(FIndices, NewValue);
  end;

{$IFDEF BuildSanityChecks}
  Assert(FCurrentIndex < Length(FIndices), 'Too many indices.');
{$ENDIF}
  FIndices[FCurrentIndex] := Value;
  Inc(FCurrentIndex);
end;

function TStrokeBuilder.GetCapDivisions: Integer;
begin
  if FBrush.Cap = TStrokeCap.Round then
    Result := Max(Ceil(FThickness * Pi / 4.0), 2)
  else
    Result := 0;
end;

procedure TStrokeBuilder.GetDashEstimate(out VertexCount, IndexCount: Integer);
var
  Divisions: Integer;
begin
  case FBrush.Cap of
    TStrokeCap.Round:
      begin
        Divisions := GetCapDivisions;

        VertexCount := 6 + Divisions * 2;
        IndexCount := 6 + (Divisions + 1) * 6;
      end;

  else
    begin
      VertexCount := 4;
      IndexCount := 6;
    end;
  end;
end;

procedure TStrokeBuilder.InsertDash(SrcPos, DestPos: TPointF; const DashDirVec, ThickPerp: TPointF);
var
  InitIndex, DivIndex, Divisions: Integer;
  SinValue, CosValue: Single;
  RoundShift: TPointF;
begin
  if FBrush.Cap = TStrokeCap.Round then
  begin
    RoundShift := DashDirVec * FHalfThickness;

    SrcPos := SrcPos + RoundShift;
    DestPos := DestPos - RoundShift;
  end;

  InitIndex := FCurrentVertex;

  InsertVertex(SrcPos + ThickPerp, FStrokeColor);
  InsertVertex(DestPos + ThickPerp, FStrokeColor);
  InsertVertex(DestPos - ThickPerp, FStrokeColor);
  InsertVertex(SrcPos - ThickPerp, FStrokeColor);

  InsertIndex(InitIndex + 0);
  InsertIndex(InitIndex + 1);
  InsertIndex(InitIndex + 2);
  InsertIndex(InitIndex + 2);
  InsertIndex(InitIndex + 3);
  InsertIndex(InitIndex + 0);

  if FBrush.Cap = TStrokeCap.Round then
  begin
    InsertVertex(SrcPos, FStrokeColor);
    InsertVertex(DestPos, FStrokeColor);

    Divisions := GetCapDivisions;

    for DivIndex := 0 to Divisions - 1 do
    begin
      SinCos((DivIndex + 1) * Pi / (Divisions + 1), SinValue, CosValue);

      InsertVertex(PointF(SrcPos.X + ThickPerp.X * CosValue - ThickPerp.Y * SinValue, SrcPos.Y + ThickPerp.X * SinValue + ThickPerp.Y *
        CosValue), FStrokeColor);
    end;

    for DivIndex := 0 to Divisions - 1 do
    begin
      SinCos((DivIndex + 1) * Pi / (Divisions + 1), SinValue, CosValue);

      InsertVertex(PointF(DestPos.X + ThickPerp.Y * SinValue - ThickPerp.X * CosValue,
        DestPos.Y - (ThickPerp.X * SinValue + ThickPerp.Y * CosValue)), FStrokeColor);
    end;

    InsertIndex(InitIndex + 4);
    InsertIndex(InitIndex + 0);
    InsertIndex(InitIndex + 6);

    InsertIndex(InitIndex + 4);
    InsertIndex(InitIndex + 5 + Divisions);
    InsertIndex(InitIndex + 3);

    for DivIndex := 0 to Divisions - 2 do
    begin
      InsertIndex(InitIndex + 4);
      InsertIndex(InitIndex + 6 + DivIndex);
      InsertIndex(InitIndex + 7 + DivIndex);
    end;

    InsertIndex(InitIndex + 2);
    InsertIndex(InitIndex + 6 + Divisions);
    InsertIndex(InitIndex + 5);

    InsertIndex(InitIndex + 5);
    InsertIndex(InitIndex + 5 + Divisions * 2);
    InsertIndex(InitIndex + 1);

    for DivIndex := 0 to Divisions - 2 do
    begin
      InsertIndex(InitIndex + 5);
      InsertIndex(InitIndex + 6 + Divisions + DivIndex);
      InsertIndex(InitIndex + 7 + Divisions + DivIndex);
    end;
  end;
end;

procedure TStrokeBuilder.GetDotEstimate(out VertexCount, IndexCount: Integer);
var
  Divisions: Integer;
begin
  case FBrush.Cap of
    TStrokeCap.Round:
      begin
        Divisions := GetCapDivisions;

        VertexCount := 3 + Divisions * 2;
        IndexCount := (Divisions + 1) * 6;
      end;

  else
    begin
      VertexCount := 4;
      IndexCount := 6;
    end;
  end;
end;

procedure TStrokeBuilder.InsertDot(const MidPos, DotDirVec, ThickPerp: TPointF);
var
  InitIndex, DivIndex, Divisions: Integer;
  SinValue, CosValue: Single;
  DotParShift: TPointF;
begin
  InitIndex := FCurrentVertex;

  if FBrush.Cap = TStrokeCap.Flat then
  begin
    DotParShift := DotDirVec * FHalfThickness;

    InsertVertex(MidPos + ThickPerp - DotParShift, FStrokeColor);
    InsertVertex(MidPos + DotParShift + ThickPerp, FStrokeColor);
    InsertVertex(MidPos + DotParShift - ThickPerp, FStrokeColor);
    InsertVertex(MidPos - (ThickPerp + DotParShift), FStrokeColor);

    InsertIndex(InitIndex + 0);
    InsertIndex(InitIndex + 1);
    InsertIndex(InitIndex + 2);
    InsertIndex(InitIndex + 2);
    InsertIndex(InitIndex + 3);
    InsertIndex(InitIndex + 0);
  end
  else
  begin
    InsertVertex(MidPos, FStrokeColor);

    Divisions := 2 + GetCapDivisions * 2;

    for DivIndex := 0 to Divisions - 1 do
    begin
      SinCos(DivIndex * (Pi * 2.0) / Divisions, SinValue, CosValue);

      InsertVertex(PointF(MidPos.X + ThickPerp.X * CosValue - ThickPerp.Y * SinValue, MidPos.Y + ThickPerp.X * SinValue + ThickPerp.Y *
        CosValue), FStrokeColor);
    end;

    for DivIndex := 0 to Divisions - 1 do
    begin
      InsertIndex(InitIndex);
      InsertIndex(InitIndex + 1 + DivIndex);
      InsertIndex(InitIndex + 1 + ((DivIndex + 1) mod Divisions));
    end;
  end;
end;

function TStrokeBuilder.GetPatternStepCount: Integer;
begin
  case FBrush.Dash of
    TStrokeDash.Solid, TStrokeDash.Custom:
      Result := 1;

    TStrokeDash.Dash:
      Result := 4;

    TStrokeDash.Dot:
      Result := 2;

    TStrokeDash.DashDot:
      Result := 6;

    TStrokeDash.DashDotDot:
      Result := 8;

  else
    Result := 0;
  end;
end;

procedure TStrokeBuilder.ComputeBuildEstimates(const TentSegmentCount: Single; out VertexCount, IndexCount: Integer);
var
  PieceVertices, PieceIndices, FloorSegmentCount, CeilSegmentCount: Integer;
begin
  FExtraPieces := 0;
  FLastDashExtend := False;

  FLastSegmentFraction := Frac(TentSegmentCount);

  FloorSegmentCount := Floor(TentSegmentCount);
  CeilSegmentCount := Ceil(TentSegmentCount);

  case FBrush.Dash of
    TStrokeDash.Solid, TStrokeDash.Custom:
      begin
        FSegmentCount := 1;
        GetDashEstimate(VertexCount, IndexCount);
      end;

    TStrokeDash.Dash:
      begin
        FSegmentCount := FloorSegmentCount;

        if FLastSegmentFraction >= 0.25 then
        begin
          FSegmentCount := CeilSegmentCount;
          FLastDashExtend := (CeilSegmentCount <> FloorSegmentCount) and (FLastSegmentFraction < 0.75);
        end;

        GetDashEstimate(PieceVertices, PieceIndices);

        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;
      end;

    TStrokeDash.Dot:
      begin
        FSegmentCount := Round(TentSegmentCount);

        GetDotEstimate(PieceVertices, PieceIndices);

        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;
      end;

    TStrokeDash.DashDot:
      begin
        FSegmentCount := FloorSegmentCount;

        if FLastSegmentFraction >= 1 / 6 then
        begin
          FSegmentCount := CeilSegmentCount;
          FLastDashExtend := (CeilSegmentCount <> FloorSegmentCount) and (FLastSegmentFraction < 0.5);
        end
        else
          FLastSegmentFraction := 1;

        GetDashEstimate(PieceVertices, PieceIndices);
        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;

        GetDotEstimate(PieceVertices, PieceIndices);

        if FSegmentCount > 1 then
        begin
          Inc(VertexCount, PieceVertices * (FSegmentCount - 1));
          Inc(IndexCount, PieceIndices * (FSegmentCount - 1));
        end;

        if FLastSegmentFraction >= 5 / 6 then
        begin
          Inc(VertexCount, PieceVertices);
          Inc(IndexCount, PieceIndices);
          Inc(FExtraPieces);
        end;
      end;

    TStrokeDash.DashDotDot:
      begin
        FSegmentCount := FloorSegmentCount;

        if FLastSegmentFraction >= 1 / 8 then
        begin
          FSegmentCount := CeilSegmentCount;
          FLastDashExtend := (CeilSegmentCount <> FloorSegmentCount) and (FLastSegmentFraction < 3.0 / 8.0);
        end
        else
          FLastSegmentFraction := 1;

        GetDashEstimate(PieceVertices, PieceIndices);
        VertexCount := PieceVertices * FSegmentCount;
        IndexCount := PieceIndices * FSegmentCount;

        GetDotEstimate(PieceVertices, PieceIndices);

        if FSegmentCount > 1 then
        begin
          Inc(VertexCount, PieceVertices * (FSegmentCount - 1) * 2);
          Inc(IndexCount, PieceIndices * (FSegmentCount - 1) * 2);
        end;

        if FLastSegmentFraction >= 7 / 8 then
        begin
          Inc(VertexCount, PieceVertices * 2);
          Inc(IndexCount, PieceIndices * 2);
          Inc(FExtraPieces, 2);
        end
        else if FLastSegmentFraction >= 5 / 8 then
        begin
          Inc(VertexCount, PieceVertices);
          Inc(IndexCount, PieceIndices);
          Inc(FExtraPieces);
        end;
      end;
  end;
end;

procedure TStrokeBuilder.InsertSegment(const SegmentPos, SegDirVec, ThickPerp, DestPos: TPointF; IsLast: Boolean);
var
  PieceTail: TPointF;
begin
  case FBrush.Dash of
    TStrokeDash.Solid, TStrokeDash.Custom:
      InsertDash(SegmentPos, DestPos, SegDirVec, ThickPerp);

    TStrokeDash.Dash:
      begin
        if IsLast and FLastDashExtend then
          PieceTail := DestPos
        else
          PieceTail := SegmentPos + (SegDirVec * FThickness * 3);

        InsertDash(SegmentPos, PieceTail, SegDirVec, ThickPerp);
      end;

    TStrokeDash.Dot:
      InsertDot(SegmentPos + (SegDirVec * FHalfThickness), SegDirVec, ThickPerp);

    TStrokeDash.DashDot:
      begin
        if IsLast and FLastDashExtend then
          PieceTail := DestPos
        else
          PieceTail := SegmentPos + (SegDirVec * FThickness * 3);

        InsertDash(SegmentPos, PieceTail, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
          InsertDot(SegmentPos + (SegDirVec * FThickness * 4.5), SegDirVec, ThickPerp);
      end;

    TStrokeDash.DashDotDot:
      begin
        if IsLast and FLastDashExtend then
          PieceTail := DestPos
        else
          PieceTail := SegmentPos + (SegDirVec * FThickness * 3);

        InsertDash(SegmentPos, PieceTail, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
          InsertDot(SegmentPos + (SegDirVec * FThickness * 4.5), SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 1) then
          InsertDot(SegmentPos + (SegDirVec * FThickness * 6.5), SegDirVec, ThickPerp);
      end;
  end;
end;

procedure TStrokeBuilder.BuildLine(const SrcPos, DestPos: TPointF; const Opacity: Single);
var
  FinalSrcPos, FinalDestPos, PiecePos, StepDelta, ThickPerp: TPointF;
  PieceDirVec, CurScale: TPointF;
  StepSize, PiecesCount: Single;
  StepIndex, LastStepIndex, PatternStepCount: Integer;
  LineLength: Single;
  TotalVertices, TotalIndices: Integer;
begin
  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));
  FUndeterminedMode := False;

  FinalSrcPos := SrcPos * Matrix;
  FinalDestPos := DestPos * Matrix;
  LineLength := FinalSrcPos.Distance(FinalDestPos);

  PieceDirVec := (FinalDestPos - FinalSrcPos).Normalize;

  PatternStepCount := GetPatternStepCount;

  if PatternStepCount > 0 then
    StepSize := FThickness * PatternStepCount
  else
    StepSize := LineLength;

  StepDelta := PieceDirVec * StepSize;

  PiecePos := FinalSrcPos;
  ThickPerp := PointF(-PieceDirVec.Y, PieceDirVec.X) * (FThickness * 0.5);

  if StepSize <= 0 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  PiecesCount := LineLength / StepSize;

  ComputeBuildEstimates(PiecesCount, TotalVertices, TotalIndices);
  if FSegmentCount < 1 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  LastStepIndex := FSegmentCount - 1;

  InitArrays(TotalVertices, TotalIndices);

  for StepIndex := 0 to LastStepIndex do
  begin
    InsertSegment(PiecePos, PieceDirVec, ThickPerp, FinalDestPos, StepIndex >= LastStepIndex);
    PiecePos := PiecePos + StepDelta;
  end;

  ArrayFillCheck();
end;

function TStrokeBuilder.GetEllipseTransfAt(const Delta: Single): TPointF;
var
  Angle, CosAngle, SinAngle: Single;
  SampleAt: TPointF;
begin
  Angle := Delta * 2.0 * Pi / FEllipseCircumf;

  SinCos(Angle, SinAngle, CosAngle);

  SampleAt.X := FEllipseCenter.X + CosAngle * FEllipseRadius.X;
  SampleAt.Y := FEllipseCenter.Y - SinAngle * FEllipseRadius.Y;

  Result := SampleAt * FMatrix;
end;

procedure TStrokeBuilder.InsertEllipseSegment(const SegInitDist: Single; IsLast: Boolean);
var
  TempDelta: Single;
  SegSrcPos, SegDestPos, SegDirVec, ThickPerp: TPointF;
begin
  case FBrush.Dash of
    TStrokeDash.Dash:
      begin
        SegSrcPos := GetEllipseTransfAt(SegInitDist);

        if IsLast and FLastDashExtend then
          TempDelta := FEllipseCircumf
        else
          TempDelta := SegInitDist + FThickness * 3.0;

        SegDestPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (SegDestPos - SegSrcPos).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDash(SegSrcPos, SegDestPos, SegDirVec, ThickPerp);
      end;

    TStrokeDash.Dot:
      begin
        TempDelta := SegInitDist + FHalfThickness;

        SegSrcPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
          FHalfThickness)).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDot(SegSrcPos, SegDirVec, ThickPerp);
      end;

    TStrokeDash.DashDot:
      begin
        SegSrcPos := GetEllipseTransfAt(SegInitDist);

        if IsLast and FLastDashExtend then
          TempDelta := FEllipseCircumf
        else
          TempDelta := SegInitDist + FThickness * 3.0;

        SegDestPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (SegDestPos - SegSrcPos).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDash(SegSrcPos, SegDestPos, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
        begin
          TempDelta := SegInitDist + FThickness * 4.5;

          SegSrcPos := GetEllipseTransfAt(TempDelta);

          SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
            FHalfThickness)).Normalize;
          ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

          InsertDot(SegSrcPos, SegDirVec, ThickPerp);
        end;
      end;

    TStrokeDash.DashDotDot:
      begin
        SegSrcPos := GetEllipseTransfAt(SegInitDist);

        if IsLast and FLastDashExtend then
          TempDelta := FEllipseCircumf
        else
          TempDelta := SegInitDist + FThickness * 3;

        SegDestPos := GetEllipseTransfAt(TempDelta);

        SegDirVec := (SegDestPos - SegSrcPos).Normalize;
        ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

        InsertDash(SegSrcPos, SegDestPos, SegDirVec, ThickPerp);

        if (not IsLast) or (FExtraPieces > 0) then
        begin
          TempDelta := SegInitDist + FThickness * 4.5;

          SegSrcPos := GetEllipseTransfAt(TempDelta);

          SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
            FHalfThickness)).Normalize;
          ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

          InsertDot(SegSrcPos, SegDirVec, ThickPerp);
        end;

        if (not IsLast) or (FExtraPieces > 1) then
        begin
          TempDelta := SegInitDist + FThickness * 6.5;

          SegSrcPos := GetEllipseTransfAt(TempDelta);

          SegDirVec := (GetEllipseTransfAt(TempDelta + FHalfThickness) - GetEllipseTransfAt(TempDelta -
            FHalfThickness)).Normalize;
          ThickPerp := PointF(-SegDirVec.Y, SegDirVec.X) * FHalfThickness;

          InsertDot(SegSrcPos, SegDirVec, ThickPerp);
        end;
      end;
  end;
end;

procedure TStrokeBuilder.BuildIntermEllipse(const Center, Radius: TPointF; const Opacity: Single);
var
  MajorAxis, MinorAxis, AxisSum, AxisSub, SubDivSumSq3: Single;
  StepSize, TentSegmentCount, SegInitDist: Single;
  CurScale: TPointF;
  PatternStepCount, TotalVertices, TotalIndices, StepIndex: Integer;
begin
  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));
  FUndeterminedMode := False;

  FEllipseCenter := Center;
  FEllipseRadius := Radius;
  FEllipseTransfCenter := Center * FMatrix;

  if Radius.X > Radius.Y then
  begin
    MajorAxis := Radius.X * CurScale.X;
    MinorAxis := Radius.Y * CurScale.Y;
  end
  else
  begin
    MajorAxis := Radius.Y * CurScale.Y;
    MinorAxis := Radius.X * CurScale.X;
  end;

  AxisSum := MajorAxis + MinorAxis;
  AxisSub := MajorAxis - MinorAxis;

  if AxisSum <= 0 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  SubDivSumSq3 := 3 * Sqr(AxisSub / AxisSum);

  FEllipseCircumf := Pi * AxisSum * (1 + (SubDivSumSq3 / (10 + Sqrt(4 - SubDivSumSq3))));

  PatternStepCount := GetPatternStepCount;
  if PatternStepCount < 1 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  StepSize := FThickness * PatternStepCount;
  TentSegmentCount := FEllipseCircumf / StepSize;

  ComputeBuildEstimates(TentSegmentCount, TotalVertices, TotalIndices);
  if (FSegmentCount < 1) or (FEllipseCircumf < FThickness) then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  InitArrays(TotalVertices, TotalIndices);

  SegInitDist := 0.0;

  for StepIndex := 0 to FSegmentCount - 1 do
  begin
    InsertEllipseSegment(SegInitDist, StepIndex = FSegmentCount - 1);
    SegInitDist := SegInitDist + StepSize;
  end;

  ArrayFillCheck;
end;

procedure TStrokeBuilder.BuildSolidEllipse(const Center, Radius: TPointF; const Opacity: Single);
var
  MajorAxis, MinorAxis, AxisSum, AxisSub, SubDivSumSq3: Single;
  StepSize, HalfStepSize, SegInitDist: Single;
  CurScale, SampleAt, SampleDirVec, ThickPerp: TPointF;
  TotalVertices, TotalIndices, StepIndex: Integer;
begin
  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));
  FUndeterminedMode := False;

  FEllipseCenter := Center;
  FEllipseRadius := Radius;
  FEllipseTransfCenter := Center * FMatrix;

  if Radius.X > Radius.Y then
  begin
    MajorAxis := Radius.X * CurScale.X;
    MinorAxis := Radius.Y * CurScale.Y;
  end
  else
  begin
    MajorAxis := Radius.Y * CurScale.Y;
    MinorAxis := Radius.X * CurScale.X;
  end;

  AxisSum := MajorAxis + MinorAxis;
  AxisSub := MajorAxis - MinorAxis;

  if AxisSum <= 0 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  SubDivSumSq3 := 3 * Sqr(AxisSub / AxisSum);

  FEllipseCircumf := Pi * AxisSum * (1 + (SubDivSumSq3 / (10 + Sqrt(4 - SubDivSumSq3))));


  if MajorAxis > 100 then
    StepSize := FEllipseCircumf / (2.0 * Sqrt(FEllipseCircumf))
  else
    if MajorAxis > 50 then
      StepSize := MajorAxis * 0.1
    else
      StepSize := MajorAxis * 0.05;

  HalfStepSize := StepSize * 0.5;

  FSegmentCount := Round(FEllipseCircumf / StepSize);

  if (FSegmentCount < 1) or (FEllipseCircumf < FThickness) then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  TotalVertices := FSegmentCount * 2;
  TotalIndices := FSegmentCount * 6;

  InitArrays(TotalVertices, TotalIndices);

  SegInitDist := 0;

  for StepIndex := 0 to FSegmentCount - 1 do
  begin
    SampleAt := GetEllipseTransfAt(SegInitDist);

    SampleDirVec := (GetEllipseTransfAt(SegInitDist + HalfStepSize) - GetEllipseTransfAt(SegInitDist -
      HalfStepSize)).Normalize;
    ThickPerp := PointF(-SampleDirVec.Y, SampleDirVec.X) * FHalfThickness;

    InsertVertex(SampleAt - ThickPerp, FStrokeColor);
    InsertVertex(SampleAt + ThickPerp, FStrokeColor);

    InsertIndex((StepIndex * 2 + 3) mod TotalVertices);
    InsertIndex((StepIndex * 2 + 1) mod TotalVertices);
    InsertIndex(StepIndex * 2);

    InsertIndex(StepIndex * 2);
    InsertIndex((StepIndex * 2 + 2) mod TotalVertices);
    InsertIndex((StepIndex * 2 + 3) mod TotalVertices);

    SegInitDist := SegInitDist + StepSize;
  end;

  ArrayFillCheck;
end;

procedure TStrokeBuilder.BuildIntermPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean);
const
  COS_170_DEGREE = -0.98;
var
  CurScale: TPointF;
  CurIndex: Integer;
  FirstPoint: Integer;

  procedure PolyLine(Start, Count: Integer);
  var
    Closed: Boolean;
    Loop: Boolean;
    SaveThickness: Single;
    SaveColor: TAlphaColor;
    First, I: Integer;
    P1, P2, P3: TPointF;
    D1: TPointF;
    L1: Single;
    A, B, C, D, E, F: TVertex;

    Dashes: array of Single;
    iDash : Integer;
    DashLen: Single;

    EndCap: TEndCap;

    procedure NextDash;
    begin
      iDash := (iDash + 1) mod Length(Dashes);
      DashLen := Dashes[iDash];
    end;

    procedure EndSegment(var A, B: TVertex; P1, P2: TPointF);
    var
      C, D, O: TVertex;
      D1: TPointF;
      L1: Single;
    begin
      D1 := (P2 - P1).Normalize;
      L1 := (P2 - P1).Length;

      while L1 > DashLen do
      begin
        O.P := P1;
        P1 := P1 + D1 * DashLen;
        L1 := L1 - DashLen;
        EdgePoints(P1, D1, C, D);
        if not Odd(iDash) then
        begin
          if Brush.Cap = TStrokeCap.Round then
          begin
            if TEndCaps.Start in EndCap then
            begin
              O.P := O.P + D1 * FHalfThickness;
              EdgePoints(O.P, D1, A, B);
              O.I := -1;
              Arc(O.P, O, B, A, False);
            end;
            O.P := P1 - D1 * FHalfThickness;
            EdgePoints(O.P, D1, E, F);
            Quad(A, B, F, E);
            O.I := -1;
            Arc(O.P, O, E, F, False);
          end
          else
            Quad(A, B, D, C);
        end;
        A := C;
        B := D;
        NextDash;
        EndCap := [TEndCaps.Start, TEndCaps.Stop];
      end;

      if Odd(iDash) or (L1 < FThickness) then
        Exit;

      if Brush.Cap = TStrokeCap.Round then
      begin
        if TEndCaps.Start in EndCap then
        begin
          P1 := P1 + D1 * FHalfThickness;
          EdgePoints(P1, D1, A, B);
          O := P1;
          Arc(P1, O, B, A, False);
        end;
        P2 := P2 - D1 * FHalfThickness;
      end;
      EdgePoints(P2, D1, C, D);
      Quad(A, B, D, C);
      if Brush.Cap = TStrokeCap.Round then
      begin
        O := P2;
        Arc(P2, O, C, D, False);
      end;
    end;

    procedure DrawJoin(var A, B: TVertex; var P1, P2: TPointF; const P3: TPointF; Visible: Boolean = True);
    const
      COS_170_DEGREE = -0.98;
    var
      D1, D2: TPointF;
      L1: Single;
      C, D: TVertex;                 //    A........C E..........G
      E, F: TVertex;                 //  P1:        :P2          :P3
      G, H: TVertex;                 //    B........D F..........H
      O: TVertex;
      Style: TStrokeJoin;
      CosAngle: Single;
      Clockwise: Boolean;

      procedure Capsule;
      begin
        if Brush.Cap = TStrokeCap.Round then
        begin
          if TEndCaps.Start in EndCap then
          begin
            P1 := P1 + D1 * FHalfThickness;
            EdgePoints(P1, D1, A, B);
            O := P1;
            Arc(P1, O, B, A, False);
          end;
          if (L1 > FHalfThickness) or not (TEndCaps.Start in EndCap) then
            Quad(A, B, D, C)
          else
          begin
            C := A;
            D := B;
          end;
        end else
          Quad(A, B, D, C);
      end;

    begin
      D1 := (P2 - P1).Normalize;
      L1 := (P2 - P1).Length;
      D2 := (P3 - P2).Normalize;

      Style := Brush.Join;
      CosAngle := D1.AngleCosine(D2);
      if (Style = TStrokeJoin.Miter) and (CosAngle < COS_170_DEGREE) then
        Style := TStrokeJoin.Bevel;

      while L1 > DashLen do
      begin
        O.P := P1;
        P1 := P1 + D1 * DashLen;
        L1 := L1 - DashLen;
        EdgePoints(P1, D1, C, D);
        if Visible and not Odd(iDash) then
        begin
          if Brush.Cap = TStrokeCap.Round then
          begin
            if TEndCaps.Start in EndCap then
            begin
              O.P := O.P + D1 * FHalfThickness;
              EdgePoints(O.P, D1, A, B);
              O.I := -1;
              Arc(O.P, O, B, A, False);
            end;
            O.P := P1 - D1 * FHalfThickness;
            EdgePoints(O.P, D1, E, F);
            Quad(A, B, F, E);
            O.I := -1;
            Arc(O.P, O, E, F, False);
          end else
            Quad(A, B, D, C);
        end;
        A := C;
        B := D;
        NextDash;
        EndCap := [TEndCaps.Start, TEndCaps.Stop];
      end;
      DashLen := DashLen - L1;
      if Odd(iDash) then
        Visible := False;

      if Visible then
      begin
        if (L1 < FHalfThickness) and (TEndCaps.Start in EndCap) then
        begin
          P1 := P2 - D2 * L1;
          EdgePoints(P1, D2, A, B);
          D1 := D2;
          Style := TStrokeJoin.Miter;
        end;
        if (DashLen < FHalfThickness) and (TEndCaps.Start in EndCap) then
        begin
          if Brush.Cap = TStrokeCap.Round then
          begin
            if TEndCaps.Start in EndCap then
            begin
              P1 := P1 + D1 * FHalfThickness;
              EdgePoints(P1, D1, A, B);
              O := P1;
              Arc(P1, O, B, A, False);
            end;
            P1 := P1 + D1 * (DashLen + L1 - FThickness);
            EdgePoints(P1, D1, C, D);
            Quad(A, B, D, C);
            O := P1;
            Arc(P1, O, C, D, False);
          end
          else
          begin
            P1 := P1 + D1 * (DashLen + L1);
            EdgePoints(P1, D1, C, D);
            Quad(A, B, D, C);
          end;
          P2 := P2 + D2 * DashLen;
          NextDash;
          Visible := False;
        end;
      end;

      EdgePoints(P2, D1, C, D);
      EdgePoints(P2, D2, E, F);
      EdgePoints(P3, D2, G, H);

      if Style = TStrokeJoin.Miter then
      begin
        Intersect(A, C, E, G);
        Intersect(B, D, F, H);
        if Visible then
          Capsule;
        A := C;
        B := D;
      end
      else
      begin
        if CosAngle >= COS_170_DEGREE then
        begin
          Clockwise := (D1.X * D2.Y - D2.X * D1.Y) < 0;
          if Clockwise then
          begin
            Intersect(A, C, E, G);
            if Visible then
            begin
              Capsule;
              case Style of
                TStrokeJoin.Bevel: Triangle(C, D, F);
                TStrokeJoin.Round: Arc(P2, C, D, F, True);
              end;
            end;
            D := F;
          end
          else
          begin
            Intersect(B, D, F, H);
            if Visible then
            begin
              Capsule;
              case Style of
                TStrokeJoin.Bevel: Triangle(D, C, E);
                TStrokeJoin.Round: Arc(P2, D, C, E, False);
              end;
            end;
            C := E;
          end;
          A := C;
          B := D;
        end
        else
        begin
          if Visible then
            Quad(A, B, D, C);
          A := D;
          B := C;
        end;
      end;

      P1 := P2;
      P2 := P3;
      if Visible then
        EndCap := [TEndCaps.Stop]
      else
        EndCap := [TEndCaps.Start, TEndCaps.Stop];
    end;

  begin
    if Count < 2 then
      Exit;

    Closed := Points[Start].EqualsTo(Points[Start + Count - 1]);
    if Closed then
    begin
      Dec(Count);
      if Count = 2 then
        Exit;
    end
    else
      Closed := not BreakAtEnd;

    case Brush.Dash of
      TStrokeDash.Dash       : Dashes := [3 * FThickness, FThickness];
      TStrokeDash.Dot        : Dashes := [FThickness, FThickness];
      TStrokeDash.DashDot    : Dashes := [3 * FThickness, FThickness, FThickness, FThickness];
      TStrokeDash.DashDotDot : Dashes := [3 * FThickness, FThickness, FThickness, FThickness, FThickness, FThickness];
      TStrokeDash.Custom:
      begin
        SetLength(Dashes, Length(Brush.DashArray));
        for I := 0 to Length(Dashes) - 1 do
          if Odd(I) then
            Dashes[I] := Brush.DashArray[I] * FThickness - FThickness
          else
            Dashes[I] := Brush.DashArray[I] * FThickness + FThickness
      end;
    else
      Dashes := [Single.MaxValue];
    end;
    iDash := 0;
    DashLen := Dashes[iDash];

    if  FThickness < 3 then
    begin
      SaveThickness := FHalfThickness;
      FHalfThickness := 1.75 * FHalfThickness;
      F.I := -1;
      if Closed then
      begin
        First := 0;
        F.P := Points[Start + Count - 1] * FMatrix;
      end
      else
      begin
        First := 1;
        F.P := Points[Start] * FMatrix;
      end;
      for I := First to Count - 1 do
      begin
        E := F;
        F.I := -1;
        F.P := Points[Start + I] * FMatrix;
        D1 := (F.P - E.P);
        L1 := D1.Length;
        D1 := D1/L1;

        while L1 > 0 do
        begin
          if odd(iDash) then
          begin
            if L1 < DashLen then
            begin
              DashLen := DashLen - L1;
              Break;
            end;
            E.I := -1;
            E.P := E.P + DashLen * D1;
            L1 := L1 - DashLen;
            NextDash;
          end
          else
          begin
            if L1 > DashLen then
            begin
              F.I := -1;
              F.P := E.P + DashLen * D1;
              L1 := L1 - DashLen;
              NextDash;
            end
            else
            begin
              F.P := Points[Start + I] * FMatrix;
              DashLen := DashLen - L1;
              L1 := 0;
            end;
            EdgePoints(E.P, D1, A, B);
            EdgePoints(F.P, D1, C, D);
            AddVertex(E);
            AddVertex(F);
            SaveColor := FStrokeColor;
            FStrokeColor := MakeColor(FStrokeColor, 0.25);
            AddVertex(A);
            Quad(A, E, F, C);
            AddVertex(B);
            Quad(B, E, F, D);
            FStrokeColor := SaveColor;
            E := F;
          end;
        end;
      end;
      FHalfThickness := SaveThickness;
      Exit;
    end;

    Loop := Closed;
    if Loop then
    begin
      L1 := (Points[Start + Count - 1] * FMatrix - Points[Start] * FMatrix).Length;
      for I := 0 to Count - 2 do
      begin
        L1 := L1 + (Points[Start + I + 1] * FMatrix - Points[Start + I] * FMatrix).Length;
        while L1 > DashLen do
        begin
          L1 := L1 - DashLen;
          NextDash;
        end;
        DashLen := DashLen - L1;
        L1 := 0;
      end;
      Loop := (DashLen > FThickness) and not Odd(iDash);
      iDash := 0;
      DashLen := Dashes[0];
    end;

    if Loop then
    begin
      P1 := Points[Start + Count - 1] * FMatrix;
      P2 := Points[Start] * FMatrix;
      P3 := Points[Start + 1] * FMatrix;
      StartSegment(P1, P2, A, B);
      EndCap := [TEndCaps.Stop];
      DrawJoin(A, B, P1, P2, P3, False);
      EndCap := [TEndCaps.Stop];
      iDash := 0;
      DashLen := Dashes[0] - FHalfThickness;
    end
    else
    begin
      P1 := Points[Start + 0] * FMatrix;
      P2 := Points[Start + 1] * FMatrix;
      D1 := (P2 - P1).Normalize;
      P1 := P1 - D1 * FHalfThickness;
      StartSegment(P1, P2, A, B);
      EndCap := [TEndCaps.Start, TEndCaps.Stop];
    end;

    for I := 0 to Count - 3 do
    begin
      P3 := Points[Start + I + 2] * FMatrix;
      DrawJoin(A, B, P1, P2, P3);
    end;

    if Closed then
    begin
      P3 := Points[Start] * FMatrix;
      DrawJoin(A, B, P1, P2, P3);
      if Loop then
      begin
        P3 := Points[Start + 1] * FMatrix;
        DrawJoin(A, B, P1, P2, P3);
      end
      else
        EndSegment(A, B, P1, P3);
    end
    else
    begin
      D1 := (P2 - P1).Normalize;
      P2 := P2 + D1 * FHalfThickness;
      EndSegment(A, B, P1, P2);
    end;
  end;

begin
  if Length(Points) < 2 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));

  FUndeterminedMode := True;
  InitArrayPointers;

  FirstPoint := 0;
  CurIndex := 0;
  while CurIndex < Length(Points) do
  begin
    if Points[CurIndex] = PolygonPointBreak then
    begin
      PolyLine(FirstPoint, CurIndex - FirstPoint);
      FirstPoint := CurIndex + 1;
    end;
    Inc(CurIndex);
  end;
  PolyLine(FirstPoint, CurIndex - FirstPoint);

  FinalizeArrays;
end;

procedure TStrokeBuilder.BuildIntermPath(const Path: TPathData; const Opacity: Single);
var
  Points: TPolygon;
begin
  Path.FlattenToPolygon(Points, 1);
  BuildIntermPolygon(Points, Opacity, (Path.Count > 0) and (Path[Path.Count - 1].Kind <> TPathPointKind.Close));
end;

procedure TStrokeBuilder.BuildSolidPolygon(const Points: TPolygon; const Opacity: Single; BreakAtEnd: Boolean);
const
  COS_170_DEGREE = -0.98;
var
  CurScale: TPointF;
  CurIndex: Integer;
  FirstPoint: Integer;

  procedure PolyLine(Start, Count: Integer);
  var
    Closed: Boolean;
    SaveThickness: Single;
    SaveColor: TAlphaColor;
    First, I: Integer;
    P0, P1: TPointF;
    L1: Single;
    D1, D2: TPointF;
    A, B, C, D, E, F, G, H, V: TVertex;
    Style: TStrokeJoin;
    CosAngle: Single;
    Clockwise: Boolean;
    NoCap: Boolean;
  begin
    if Count < 2 then
      Exit;

    Closed := Points[Start].EqualsTo(Points[Start + Count - 1]);
    if Closed then
    begin
      Dec(Count);
      if Count < 2 then
        Exit;
    end
    else
      Closed := not BreakAtEnd;

    if FThickness < 3 then
    begin
      SaveThickness := FHalfThickness;
      FHalfThickness := 1.75 * FHalfThickness;
      F.I := -1;
      if Closed then
      begin
        First := 0;
        F.P := Points[Start + Count - 1] * FMatrix;
      end
      else
      begin
        First := 1;
        F.P := Points[Start] * FMatrix;
      end;
      for I := First to Count - 1 do
      begin
        E := F;
        F.I := -1;
        F.P := Points[Start + I] * FMatrix;
        D1 := (F.P - E.P);
        L1 := D1.Length;
        D1 := D1/L1;
        EdgePoints(E.P, D1, A, B);
        EdgePoints(F.P, D1, C, D);

        AddVertex(E);
        AddVertex(F);
        SaveColor := FStrokeColor;
        FStrokeColor := MakeColor(FStrokeColor, 0.25);
        AddVertex(A);
        Quad(A, E, F, C);
        AddVertex(B);
        Quad(B, E, F, D);
        FStrokeColor := SaveColor;
      end;
      FHalfThickness := SaveThickness;
      Exit;
    end;

    if Closed then
    begin
      NoCap := True;
      P0 := Points[Start + Count - 2] * FMatrix;
      P1 := Points[Start + Count - 1] * FMatrix;
      Dec(Start, 2);
      Inc(Count, 3);
    end
    else
    begin
      NoCap := False;
      P0 := Points[Start] * FMatrix;
      P1 := Points[Start + 1] * FMatrix;
    end;

    D2 := (P1 - P0).Normalize;

    if (Brush.Cap = TStrokeCap.Flat) and not Closed then
      P0 := P0 - D2 * FHalfThickness;

    EdgePoints(P0, D2, A, B);
    EdgePoints(P1, D2, G, H);

    if (Brush.Cap = TStrokeCap.Round) and not Closed then
    begin
      V.I := -1;
      V.P := P0;
      Arc(P0, V, B, A, False);
    end;

    for I := 0 to Count - 3 do
    begin
      P0 := P1;
      if Closed and (I = Count - 3) then
        P1 := Points[Start + 2] * FMatrix
      else
        P1 := Points[Start + I + 2] * FMatrix;
      D1 := D2;
      D2 := (P1 - P0).Normalize;

      CosAngle := D1.AngleCosine(D2);
      Style := Brush.Join;
      if (Style = TStrokeJoin.Miter) and (CosAngle < COS_170_DEGREE) then
        Style := TStrokeJoin.Bevel;

      C := G;
      D := H;
      EdgePoints(P0, D2, E, F);
      EdgePoints(P1, D2, G, H);

      if Style = TStrokeJoin.Miter then
      begin
        Intersect(A, C, E, G);
        Intersect(B, D, F, H);
        if not NoCap then
          Quad(A, B, D, C);
        A := C;
        B := D;
      end
      else
      begin
        if CosAngle >= COS_170_DEGREE then
        begin
          Clockwise := (D1.X * D2.Y - D2.X * D1.Y) < 0;
          if Clockwise then
          begin
            Intersect(A, C, E, G);
            if NoCap = False then
            begin
              Quad(A, B, D, C);
              case Style of
                TStrokeJoin.Bevel: Triangle(C, D, F);
                TStrokeJoin.Round: Arc(P0, C, D, F, True);
              end;
            end;
            A := C;
            B := D;
            D := F;
          end
          else
          begin
            Intersect(B, D, F, H);
            if NoCap = False then
            begin
              Quad(A, B, D, C);
              case Style of
                TStrokeJoin.Bevel: Triangle(D, C, E);
                TStrokeJoin.Round: Arc(P0, D, C, E, False);
              end;
            end;
            A := C;
            B := D;
            C := E;
          end;
          A := C;
          B := D;
        end
        else if not NoCap then
        begin
          if Style = TStrokeJoin.Round then
            Arc(P0, D, C, E, False);
          Quad(A, B, D, C);
          A := D;
          B := C;
        end;
      end;
      NoCap := False;
    end;

    if not Closed then
    begin
      if Brush.Cap = TStrokeCap.Flat then
        P1 := P1 + D2 * FHalfThickness;
      EdgePoints(P1, D2, C, D);
      Quad(A, B, D, C);
      if Brush.Cap = TStrokeCap.Round then
      begin
        V.I := -1;
        V.P := P1;
        Arc(P1, V, C, D, False);
      end;
    end;
  end;

begin
  if Length(Points) < 2 then
  begin
    InitArrays(0, 0);
    Exit;
  end;

  CurScale := GetMatrixScale;

  FThickness := FBrush.Thickness * (CurScale.X + CurScale.Y) * 0.5;
  FHalfThickness := FThickness * 0.5;
  FStrokeColor := PremultiplyAlpha(MakeColor(FBrush.Color, Opacity));

  FUndeterminedMode := True;
  InitArrayPointers;

  FirstPoint := 0;
  CurIndex := 0;
  while CurIndex < Length(Points) do
  begin
    if Points[CurIndex] = PolygonPointBreak then
    begin
      PolyLine(FirstPoint, CurIndex - FirstPoint);
      FirstPoint := CurIndex + 1;
    end;
    Inc(CurIndex);
  end;
  PolyLine(FirstPoint, CurIndex - FirstPoint);

  FinalizeArrays;
end;

procedure TStrokeBuilder.BuildSolidPath(const Path: TPathData; const Opacity: Single);
var
  Points: TPolygon;
begin
  Path.FlattenToPolygon(Points, 1);
  BuildSolidPolygon(Points, Opacity, (Path.Count > 0) and (Path[Path.Count - 1].Kind <> TPathPointKind.Close));
end;

{$ENDREGION}

end.
