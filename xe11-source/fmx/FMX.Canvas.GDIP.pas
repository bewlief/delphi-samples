{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Canvas.GDIP;

interface

{$SCOPEDENUMS ON}

procedure RegisterCanvasClasses;
procedure UnregisterCanvasClasses;

implementation

uses
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX, Winapi.GDIPOBJ, Winapi.GDIPAPI, Winapi.GDIPUTIL,
  System.Classes, System.SysUtils, System.Math, System.Types, System.UITypes, System.UIConsts, System.Generics.Collections,
  FMX.Types, FMX.Consts, FMX.Printer, FMX.Printer.Win, FMX.TextLayout, FMX.Platform, FMX.Platform.Win, System.Character, 
  FMX.Graphics, System.Math.Vectors, FMX.Helpers.Win;

type

{ TGDIPCanvasSaveState }

  TGDIPCanvasSaveState = class(TCanvasSaveState)
  private
    FState: GraphicsState;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

{ TCanvasGdiPlus }

  TCanvasGdiPlus = class(TCanvas)
  private class var
    FLockedBitmapData: TDictionary<THandle, Winapi.GDIPAPI.TBitmapData>;
    class procedure DestroySharedResources; static;
  private
    FGPGraphics: TGPGraphics;
    FGPPen: TGPPen;
    FGPPenBrush: TGPBrush;
    FGPBrush: TGPBrush;
    FMetaBrush: TCanvas.TMetaBrush;
    FMetaStroke: TCanvas.TMetaBrush;
    FMetaStrokeBrush: TCanvas.TMetaStrokeBrush;
    FGPFamily: TGPFontFamily;
    FFontCollection: TGPPrivateFontCollection;
    FFontScale: Single;
    FContextHandle: THandle;
    FClipRects: PClipRects;
    FSmoothingMode: TSmoothingMode;
    procedure DetermineSmoothingMode(const Quality: TCanvasQuality);
    function GetGraphics: TGPGraphics; inline;
    function CreateSaveState: TCanvasSaveState; override;
    procedure SetClipRects(const ARects: array of TRectF);
    function CreateGPGradientBrush(ABrush: TBrush; const ARect: TRectF; const AOpacity: Single): TGPBrush;
    function CreateGPBitmapBrush(ABrush: TBrush; const ARect: TRectF; const AOpacity: Single): TGPBrush;
    procedure ApplyFill(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single);
    procedure ApplyStroke(const AStroke: TStrokeBrush; const ARect: TRectF; const AOpacity: Single);
    function GetPhysicalSize: TSize;
  protected
    procedure FontChanged(Sender: TObject); override;
    { begin and }
    function DoBeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override;
    procedure DoEndScene; override;
    { creation }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    constructor CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    { Bitmaps }
    class function DoInitializeBitmap(const Width, Height: Integer; const Scale: Single;
      var PixelFormat: TPixelFormat): THandle; override;
    class procedure DoFinalizeBitmap(var Bitmap: THandle); override;
    class function DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData); override;
    { drawing }
    procedure DoSetMatrix(const M: TMatrix); override;
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
      const HighSpeed: Boolean = False); override;
    procedure DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    { buffer }
    procedure DoClear(const Color: TAlphaColor); override;
    procedure DoClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    { cliping }
    procedure DoIntersectClipRect(const ARect: TRectF); override;
    procedure DoExcludeClipRect(const ARect: TRectF); override;
  public
    destructor Destroy; override;
    procedure SetSize(const AWidth, AHeight: Integer); override;
    { text }
    function LoadFontFromStream(const AStream: TStream): Boolean; override;
    { mesauring }
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
    property Graphics: TGPGraphics read GetGraphics;
    property PhysicalSize: TSize read GetPhysicalSize;
  end;

  TTextLayoutGDIPlus = class(TTextLayout)
  private
    FGraphics: TGPGraphics;
    FIsGraphicsOwner: Boolean;
    FGPFont: TGPFont;
    FGPBrush: TGPBrush;
    FStringFormat: TGPStringFormat;
    FTextRect: TRectF;
    function DefineTextForGDI: string;
    function MeasureRange(const APos, ALength: Integer): TRegion;
    function GetFontScale: Single;
    function GetOutputArea: TGPRectF;
    procedure InitStringFormat;
    procedure RecreateGPFont;
    procedure RecreateGPBrushIfNeeded;
  protected
    procedure DoRenderLayout; override;
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function GetTextHeight: Single; override;
    function GetTextWidth: Single; override;
    function GetTextRect: TRectF; override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
  public
    constructor Create(const ACanvas: TCanvas = nil); override;
    destructor Destroy; override;
    //
    procedure ConvertToPath(const APath: TPathData); override;

    property OutputArea: TGPRectF read GetOutputArea;
    property FontScale: Single read GetFontScale;
  end;

function GPRectFromRect(const R: TRectF): TGPRectF;
begin
  Result.X := R.Left;
  Result.Y := R.Top;
  Result.Width := R.Width;
  Result.Height := R.Height;
end;

function GPPointFromPoint(const P: TPointF): TGPPointF;
begin
  Result.X := P.X;
  Result.Y := P.Y;
end;

function RectFFromGPRect(const R: TGPRectF): TRectF;
begin
  Result.Left := R.X;
  Result.Top := R.Y;
  Result.Width := R.Width;
  Result.Height := R.Height;
end;

function TextTrimmingToStringTrimming(const AValue: TTextTrimming): TStringTrimming;
begin
  case AValue of
    TTextTrimming.None:
      Result := StringTrimmingNone;
    TTextTrimming.Character:
      Result := StringTrimmingEllipsisCharacter;
    TTextTrimming.Word:
      Result := StringTrimmingEllipsisWord;
  else
    Result := StringTrimmingNone;
  end;
end;

function TextAlignToStringTrimming(const AValue: TTextAlign): TStringAlignment;
begin
  case AValue of
    TTextAlign.Center:
      Result := StringAlignmentCenter;
    TTextAlign.Leading:
      Result := StringAlignmentNear;
    TTextAlign.Trailing:
      Result := StringAlignmentFar;
  else
    Result := StringAlignmentNear;
  end;
end;

{ TCanvasGdiPlus }

const
  ImageColorMatrix: TColorMatrix = ((1, 0, 0, 0, 0), (0, 1, 0, 0, 0), (0, 0, 1, 0, 0), (0, 0, 0, 1, 0), (0, 0, 0, 0, 1));
  TextContrast = 4;
  SmoothingModeUndetermined = SmoothingModeAntiAlias8x8;

var
  ColorArray: array [0 .. 100] of TGPColor;
  OffsetArray: array [0 .. 100] of Single;

function vgStyleToGPStyle(const AStyle: TFontStyleExt): Integer;
begin
  Result := 0;
  if not AStyle.Weight.IsRegular then
    Result := Result or FontStyleBold;
  if not AStyle.Slant.IsRegular then
    Result := Result or FontStyleItalic;
  if TFontStyle.fsUnderline in AStyle.SimpleStyle then
    Result := Result or FontStyleUnderline;
  if TFontStyle.fsStrikeOut in AStyle.SimpleStyle then
    Result := Result or FontStyleStrikeout;
end;

class procedure TCanvasGdiPlus.DestroySharedResources;
begin
  FreeAndNil(FLockedBitmapData);
end;

procedure TCanvasGdiPlus.DetermineSmoothingMode(const Quality: TCanvasQuality);
begin
  if Quality = TCanvasQuality.HighPerformance then
    FSmoothingMode := SmoothingModeHighSpeed
  else if TOSVersion.Check(6, 0) then
    FSmoothingMode := SmoothingModeAntiAlias8x8
  else
    FSmoothingMode := SmoothingModeHighQuality;
end;

constructor TCanvasGdiPlus.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault);
begin
  inherited CreateFromWindow(AParent, AWidth, AHeight, AQuality);
  DetermineSmoothingMode(Quality);
  WindowHandleToPlatform(Parent).CreateBuffer(PhysicalSize.Width, PhysicalSize.Height);
  FGPGraphics := TGPGraphics.Create(WindowHandleToPlatform(Parent).BufferHandle);
  FGPGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
  FGPGraphics.SetSmoothingMode(FSmoothingMode);
  FGPGraphics.SetInterpolationMode(InterpolationModeHighQuality);
  FGPGraphics.SetTextContrast(TextContrast);
  if GlobalUseGDIPlusClearType then
    FGPGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
  else
    FGPGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
  FGPFamily := TGPFontFamily.Create('Tahoma');
  FFontScale := 1;
end;

constructor TCanvasGdiPlus.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault);
begin
  inherited CreateFromBitmap(ABitmap, AQuality);
  DetermineSmoothingMode(Quality);
  FGPGraphics := TGPGraphics.Create(TGPBitmap(Bitmap.Handle));
  FGPGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
  FGPGraphics.SetSmoothingMode(FSmoothingMode);
  FGPGraphics.SetTextContrast(TextContrast);
  if GlobalUseGDIPlusClearType then
    FGPGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
  else
    FGPGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
  FGPFamily := TGPFontFamily.Create('Tahoma');
  if (Width > 0) and (Height > 0) and not SameValue(FGPGraphics.GetDpiX, 0.0, TEpsilon.Scale) then
    FFontScale := 96 / FGPGraphics.GetDpiX
  else
    FFontScale := 1;
end;

constructor TCanvasGdiPlus.CreateFromPrinter(const APrinter: TAbstractPrinter);
var
  PrinterWin: TPrinterWin;
begin
  inherited CreateFromPrinter(APrinter);
  if APrinter is TPrinterWin then
  begin
    // calling the constructor through the canvas instance makes the canvas
    // update the DC handle if APrinter is the same printer for the canvas
    if (FPrinter <> nil) and (APrinter = FPrinter) then
    begin
      FGPGraphics.Free;
      FGPFamily.Free;
    end;
    // initialize the canvas size
    PrinterWin := TPrinterWin(APrinter);
    FWidth := PrinterWin.PageWidth;
    FHeight := PrinterWin.PageHeight;

    // create the underlying GDIPlus canvas object
    FGPGraphics := TGPGraphics.Create(PrinterWin.Handle);
    FGPGraphics.SetPageUnit(UnitPixel);
    FGPGraphics.SetSmoothingMode(FSmoothingMode);
    FGPGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    FGPGraphics.SetInterpolationMode(InterpolationModeHighQuality);
    FGPGraphics.SetTextContrast(TextContrast);
    FGPFamily := TGPFontFamily.Create('Tahoma');
    FFontScale := 1;
  end
  else
    raise EPrinter.CreateResFmt(@SInvalidPrinterClass, [APrinter.ClassName]);
end;

function TCanvasGdiPlus.CreateGPBitmapBrush(ABrush: TBrush; const ARect: TRectF;
  const AOpacity: Single): TGPBrush;
var
  B: TBitmap;
  CM: TColorMatrix;
  ImageAttributes: TGPImageAttributes;
  GPBrushTmp: TGPTextureBrush;
  SX: Single;
  SY: Single;
begin
  B := ABrush.Bitmap.Bitmap;
  if (B <> nil) and not B.IsEmpty then
  begin
    if B.HandleAllocated then
    begin
      CM := ImageColorMatrix;
      CM[3][3] := AOpacity;
      if AOpacity <> 1 then
      begin
        ImageAttributes := TGPImageAttributes.Create;
        ImageAttributes.SetColorMatrix(CM, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
      end
      else
        ImageAttributes := nil;
      if ABrush.Bitmap.WrapMode <> TWrapMode.TileStretch then
      begin
        GPBrushTmp := TGPTextureBrush.Create(TGPBitmap(B.Handle),
          GPRectFromRect(RectF(0, 0, B.Width, B.Height)), ImageAttributes);
        if ABrush.Bitmap.WrapMode = TWrapMode.TileOriginal then
          GPBrushTmp.SetWrapMode(Winapi.GDIPAPI.TWrapMode.WrapModeClamp)
        else
          GPBrushTmp.SetWrapMode(Winapi.GDIPAPI.TWrapMode.WrapModeTile);
      end
      else
      begin
        GPBrushTmp := TGPTextureBrush.Create(TGPBitmap(B.Handle), GPRectFromRect(RectF(0, 0, B.Width, B.Height)),
          ImageAttributes);
        GPBrushTmp.SetWrapMode(WrapModeClamp);
        if Stroke.Kind = TBrushKind.None then
        begin
          SX := ARect.Width / B.Width;
          SY := ARect.Height / B.Height;
        end
        else
        begin
          SX := (ARect.Width + (Stroke.Thickness / 2)) / B.Width;
          SY := (ARect.Height + (Stroke.Thickness / 2)) / B.Height;
        end;
        GPBrushTmp.TranslateTransform(ARect.Left, ARect.Top);
        GPBrushTmp.ScaleTransform(SX, SY);
      end;
      if AOpacity <> 1 then
        ImageAttributes.Free;
      Result := GPBrushTmp;
    end
    else
      Result := TGPSolidBrush.Create($00000000);
  end
  else
    Result := TGPSolidBrush.Create($00000000);
end;

function TCanvasGdiPlus.CreateGPGradientBrush(ABrush: TBrush; const ARect: TRectF; const AOpacity: Single): TGPBrush;
const
  SizeExtrusionMaxValue = 32; // We lose 5 precission bits
var
  Count: Integer;
  I: Integer;
  WorkingPath: TGPGraphicsPath;
  MatrixTmp: TMatrix;
  GDITransformMatrix: TGPMatrix;
  SizeExtrusion: Single;
  ExtrusionThreshold: Single;
begin
  if ABrush.Gradient.Points.Count > 1 then
  begin
    Count := 0;

    if ABrush.Gradient.Points[0].Offset > 0 then
    begin
      ColorArray[Count] := MakeColor(ABrush.Gradient.Points[0].IntColor, AOpacity);
      OffsetArray[Count] := 0;
      Count := Count + 1;
    end;
    for I := 0 to ABrush.Gradient.Points.Count - 1 do
    begin
      ColorArray[I + Count] := MakeColor(ABrush.Gradient.Points[I].IntColor, AOpacity);
      OffsetArray[I + Count] := ABrush.Gradient.Points[I].Offset;
    end;
    if ABrush.Gradient.Points[ABrush.Gradient.Points.Count - 1].Offset < 1 then
    begin
      Count := Count + 1;
      ColorArray[ABrush.Gradient.Points.Count + Count - 1] :=
        MakeColor(ABrush.Gradient.Points[ABrush.Gradient.Points.Count - 1].IntColor, AOpacity);
      OffsetArray[ABrush.Gradient.Points.Count + Count - 1] := 1;
    end;

    if ABrush.Gradient.Style = TGradientStyle.Linear then
    begin
      { Linear }
      Result := TGPLinearGradientBrush.Create
        (MakePoint(ARect.Left + ABrush.Gradient.StartPosition.X * ARect.Width,
        ARect.Top + ABrush.Gradient.StartPosition.y * ARect.Height),
        MakePoint(ARect.Left + ABrush.Gradient.StopPosition.X * ARect.Width,
        ARect.Top + ABrush.Gradient.StopPosition.y * ARect.Height),
        ABrush.Color, ABrush.Color);
      TGPLinearGradientBrush(Result).SetWrapMode(WrapModeTileFlipX);
      TGPLinearGradientBrush(Result).SetInterpolationColors
        (PGPColor(@ColorArray), PSingle(@OffsetArray),
        ABrush.Gradient.Points.Count + Count);
    end
    else
    begin
      SizeExtrusion := SizeExtrusionMaxValue;
      ExtrusionThreshold := 1 - (1 / (SizeExtrusion * 2 + 1));

      for I := ABrush.Gradient.Points.Count + Count downto 1 do
      begin
        OffsetArray[I] := OffsetArray[I - 1];
        ColorArray[I] := ColorArray[I - 1];
      end;
      for I := 1 to ABrush.Gradient.Points.Count + Count - 1 do
        OffsetArray[I] := ExtrusionThreshold + (OffsetArray[I] * (1 - ExtrusionThreshold));
      Inc(Count);

      { Radial }
      WorkingPath := TGPGraphicsPath.Create;
      try
        WorkingPath.AddEllipse(GPRectFromRect(
          TRectF.Create(ARect.Left - ARect.Width * SizeExtrusion, ARect.Top - ARect.Height * SizeExtrusion,
          ARect.Right + ARect.Width * SizeExtrusion, ARect.Bottom + ARect.Height * SizeExtrusion)));
        Result := TGPPathGradientBrush.Create(WorkingPath);
        TGPPathGradientBrush(Result).SetCenterPoint(GPPointFromPoint(ARect.CenterPoint));
      finally
        WorkingPath.Free;
      end;
      MatrixTmp := ABrush.Gradient.RadialTransform.Matrix;
      GDITransformMatrix := TGPMatrix.Create(MatrixTmp.m11, MatrixTmp.m12, MatrixTmp.m21, MatrixTmp.m22, MatrixTmp.m31,
        MatrixTmp.m32);
      try
        TGPPathGradientBrush(Result).SetTransform(GDITransformMatrix);
      finally
        GDITransformMatrix.Free;
      end;
      TGPPathGradientBrush(Result).SetWrapMode(WrapModeClamp);
      TGPPathGradientBrush(Result).SetInterpolationColors(PARGB(@ColorArray), PSingle(@OffsetArray),
        ABrush.Gradient.Points.Count + Count);
    end;
  end
  else
    Result := TGPSolidBrush.Create(MakeColor(ABrush.Color, AOpacity));
end;

function TCanvasGdiPlus.CreateSaveState: TCanvasSaveState;
begin
  Result := TGDIPCanvasSaveState.Create;
end;

destructor TCanvasGdiPlus.Destroy;
begin
  FMetaStrokeBrush.Free;
  FMetaStroke.Free;
  FMetaBrush.Free;
  FreeAndNil(FFontCollection);
  FreeAndNil(FGPFamily);
  FGPBrush.Free;
  FGPPenBrush.Free;
  FGPPen.Free;
  FreeAndNil(FGPGraphics);
  inherited Destroy;
end;

function TCanvasGdiPlus.DoBeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
begin
  FContextHandle := AContextHandle;
  FClipRects := AClipRects;
  Result := inherited DoBeginScene(AClipRects);
  if Result and (AClipRects <> nil) then
    SetClipRects(AClipRects^);
end;

procedure TCanvasGdiPlus.DoEndScene;
var
  I: Integer;
  R: TRect;
  ParentHandle: TWinWindowHandle;
begin
  inherited;
  if (Parent <> nil) and (FContextHandle <> 0) then
  begin
    ParentHandle := WindowHandleToPlatform(Parent);
    if not ParentHandle.Transparency or ((ParentHandle.Form <> nil) and (csDesigning in ParentHandle.Form.ComponentState)) then
    begin
      if FClipRects <> nil then
        for I := 0 to High(FClipRects^) do
        begin
          R := WindowHandleToPlatform(Parent).FormToWnd(FClipRects^[I]).Round;
          Winapi.Windows.BitBlt(FContextHandle, R.Left, R.Top, R.Width, R.Height,
            WindowHandleToPlatform(Parent).BufferHandle, R.Left, R.Top, SRCCOPY);
        end
      else
        Winapi.Windows.BitBlt(FContextHandle, 0, 0, Width, Height, WindowHandleToPlatform(Parent).BufferHandle, 0, 0, SRCCOPY);
    end;
  end;
end;

procedure TCanvasGdiPlus.DoClear(const Color: TAlphaColor);
begin
  FGPGraphics.Clear(Color)
end;

procedure TCanvasGdiPlus.DoClearRect(const ARect: TRectF; const AColor: TAlphaColor);
var
  SaveIndex: Cardinal;
begin
  SaveIndex := FGPGraphics.Save;
  try
    FGPGraphics.IntersectClip(GPRectFromRect(ARect));
    FGPGraphics.Clear(AColor);
  finally
    FGPGraphics.Restore(SaveIndex);
  end;
end;

function TCanvasGdiPlus.GetGraphics: TGPGraphics;
begin
  Result := FGPGraphics;
end;

function TCanvasGdiPlus.GetPhysicalSize: TSize;
begin
  Result := TSize.Create(Trunc(Width * Scale), Trunc(Height * Scale));
end;

procedure TCanvasGdiPlus.DoSetMatrix(const M: TMatrix);
var
  GdiMatrix: TGPMatrix;
  LMatrix: TMatrix;
begin
  LMatrix := M * TMatrix.CreateScaling(Scale, Scale);
  GdiMatrix := TGPMatrix.Create(LMatrix.m11, LMatrix.m12, LMatrix.m21, LMatrix.m22, LMatrix.m31, LMatrix.m32);
  try
    GdiMatrix.Shear(LMatrix.m13, LMatrix.m23);
    FGPGraphics.SetTransform(GdiMatrix);
  finally
    GdiMatrix.Free;
  end;
end;

procedure TCanvasGdiPlus.SetSize(const AWidth, AHeight: Integer);
begin
  if (Parent <> nil) and ((AWidth <> Width) or (AHeight <> Height)) then
  begin
    inherited;
    FreeAndNil(FGPGraphics);
    FreeAndNil(FGPFamily);
    WindowHandleToPlatform(Parent).ResizeBuffer(PhysicalSize.Width, PhysicalSize.Height);
    FGPGraphics := TGPGraphics.Create(WindowHandleToPlatform(Parent).BufferHandle);
    FGPGraphics.SetSmoothingMode(FSmoothingMode);
    FGPGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    FGPGraphics.SetInterpolationMode(InterpolationModeHighQuality);
    FGPGraphics.SetTextContrast(TextContrast);
    if GlobalUseGDIPlusClearType then
      FGPGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
    else
      FGPGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    FGPFamily := TGPFontFamily.Create(FFont.Family);
    FFontScale := 1;
  end
  else
    inherited;
end;

procedure TCanvasGdiPlus.SetClipRects(const ARects: array of TRectF);
var
  I: Integer;
  R: TGPRegion;
begin
  R := TGPRegion.Create;
  try
    R.MakeEmpty;
    for I := 0 to High(ARects) do
      R.Union(GPRectFromRect(ARects[I]));
    FGPGraphics.SetClip(R);
  finally
    R.Free;
  end;
end;

procedure TCanvasGdiPlus.DoIntersectClipRect(const ARect: TRectF);
begin
  FClippingChangeCount := FClippingChangeCount + 1;
  FGPGraphics.IntersectClip(MakeRect(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top));
end;

procedure TCanvasGdiPlus.DoExcludeClipRect(const ARect: TRectF);
begin
  FClippingChangeCount := FClippingChangeCount + 1;
  FGPGraphics.ExcludeClip(GPRectFromRect(ARect));
end;

procedure TCanvasGdiPlus.ApplyFill(const ABrush: TBrush; const ARect: TRectF; const AOpacity: Single);
begin
  if (ABrush.Kind = TBrushKind.Resource) and (ABrush.Resource <> nil) and (ABrush.Resource.Brush <> nil) then
    ABrush.Assign(ABrush.Resource.Brush);

  if FMetaBrush = nil then
    FMetaBrush := TMetaBrush.Create;

  if FMetaBrush.Valid and ((FMetaBrush.Kind <> ABrush.Kind) or (not SameValue(FMetaBrush.Opacity, AOpacity,
    TEpsilon.Scale))) then
    FMetaBrush.Valid := False;

  case ABrush.Kind of
    TBrushKind.Solid:
      if (not FMetaBrush.Valid) or (FMetaBrush.Color <> ABrush.Color) then
      begin
        FGPBrush.Free;
        FGPBrush := TGPSolidBrush.Create(MakeColor(ABrush.Color, AOpacity));
        FMetaBrush.Color := ABrush.Color;
      end;

    TBrushKind.Gradient:
      if (not FMetaBrush.Valid) or (FMetaBrush.Rect <> ARect) or (not FMetaBrush.Gradient.Equal(ABrush.Gradient)) then
      begin
        FGPBrush.Free;
        FGPBrush := CreateGPGradientBrush(ABrush, ARect, AOpacity);
        FMetaBrush.Rect := ARect;
        FMetaBrush.Gradient.Assign(ABrush.Gradient);
      end;

    TBrushKind.Bitmap:
      { Bitmap brush is not cached as it requires creating a copy of bitmap's pixels, which would then need a per-pixel
        comparison to ensure that such bitmap stays current. Overall, such per-pixel comparison would be less efficient
        than just re-creating the bitmap brush each time. }
      begin
        FGPBrush.Free;
        FGPBrush := CreateGPBitmapBrush(ABrush, ARect, AOpacity);
        FMetaBrush.Image := ABrush.Bitmap.Image;
        FMetaBrush.WrapMode := ABrush.Bitmap.WrapMode;
        FMetaBrush.Rect := ARect;
      end;

  else
    begin
      FGPBrush.Free;
      FGPBrush := TGPSolidBrush.Create($00000000);
    end;
  end;

  FMetaBrush.Kind := ABrush.Kind;
  FMetaBrush.Opacity := AOpacity;
  FMetaBrush.Valid := True;
end;

procedure TCanvasGdiPlus.ApplyStroke(const AStroke: TStrokeBrush; const ARect: TRectF; const AOpacity: Single);
begin
  if (AStroke.Kind = TBrushKind.Resource) and (AStroke.Resource <> nil) and (AStroke.Resource.Brush <> nil) then
    AStroke.Assign(AStroke.Resource.Brush);

  if FMetaStroke = nil then
    FMetaStroke := TMetaBrush.Create;

  if FMetaStroke.Valid and ((FMetaStroke.Kind <> AStroke.Kind) or
    (not SameValue(FMetaStroke.Opacity, AOpacity, TEpsilon.Scale))) then
    FMetaStroke.Valid := False;

  case AStroke.Kind of
    TBrushKind.Solid:
      if (not FMetaStroke.Valid) or (FMetaStroke.Color <> AStroke.Color) then
      begin
        FGPPenBrush.Free;
        FGPPenBrush := TGPSolidBrush.Create(MakeColor(AStroke.Color, AOpacity));
        FMetaStroke.Color := AStroke.Color;
      end;

    TBrushKind.Gradient:
      if (not FMetaStroke.Valid) or (FMetaStroke.Rect <> ARect) or
        (not FMetaStroke.Gradient.Equal(AStroke.Gradient)) then
      begin
        FGPPenBrush.Free;
        FGPPenBrush := CreateGPGradientBrush(AStroke, ARect, AOpacity);
        FMetaStroke.Rect := ARect;
        FMetaStroke.Gradient.Assign(AStroke.Gradient);
      end;

    TBrushKind.Bitmap:
      if (not FMetaStroke.Valid) or (FMetaStroke.Image <> AStroke.Bitmap.Image) or (FMetaStroke.Rect <> ARect) or
        (FMetaStroke.WrapMode <> AStroke.Bitmap.WrapMode) then
      begin
        FGPPenBrush.Free;
        FGPPenBrush := CreateGPBitmapBrush(AStroke, ARect, AOpacity);
        FMetaStroke.Image := AStroke.Bitmap.Image;
        FMetaStroke.WrapMode := AStroke.Bitmap.WrapMode;
        FMetaStroke.Rect := ARect;
      end;

  else
    begin
      FGPPenBrush.Free;
      FGPPenBrush := TGPSolidBrush.Create($00000000);
    end;
  end;

  FMetaStroke.Kind := AStroke.Kind;
  FMetaStroke.Opacity := AOpacity;
  FMetaStroke.Valid := True;

  if FMetaStrokeBrush = nil then
    FMetaStrokeBrush := TMetaStrokeBrush.Create;

  if (not FMetaStrokeBrush.Valid) or (FMetaStrokeBrush.Cap <> AStroke.Cap) or
    (FMetaStrokeBrush.Join <> AStroke.Join) or (FMetaStrokeBrush.Dash <> AStroke.Dash) or
    ((AStroke.Dash = TStrokeDash.Custom) and ((FMetaStrokeBrush.DashArray <> AStroke.DashArray) or
    not SameValue(FMetaStrokeBrush.DashOffset, AStroke.DashOffset, TEpsilon.Vector))) then
  begin
    FreeAndNil(FGPPen);
    if AStroke.Thickness > 0 then
    begin
      FGPPen := TGPPen.Create(FGPPenBrush);
      case AStroke.Cap of
        TStrokeCap.Flat:
          FGPPen.SetLineCap(LineCapSquare, LineCapSquare, DashCapFlat);
        TStrokeCap.Round:
          FGPPen.SetLineCap(LineCapRound, LineCapRound, DashCapRound);
      end;
      if Length(AStroke.DashArray) > 0 then
      begin
        FGPPen.SetDashOffset(AStroke.DashOffset);
        FGPPen.SetDashPattern(@AStroke.DashArray[0], Length(AStroke.DashArray));
      end
      else
        FGPPen.SetDashStyle(DashStyleSolid);
      case AStroke.Join of
        TStrokeJoin.Miter:
          FGPPen.SetLineJoin(LineJoinMiter);
        TStrokeJoin.Round:
          FGPPen.SetLineJoin(LineJoinRound);
        TStrokeJoin.Bevel:
          FGPPen.SetLineJoin(LineJoinBevel);
      end;
      FGPPen.SetWidth(AStroke.Thickness);
    end;
  end;
end;

procedure TCanvasGdiPlus.FontChanged(Sender: TObject);
begin
  FreeAndNil(FGPFamily);
  FGPFamily := TGPFontFamily.Create(FFont.Family, FFontCollection);
  if not FGPFamily.IsAvailable then
  begin
    FGPFamily.Free;
    FGPFamily := TGPFontFamily.Create(FFont.Family);
    if not FGPFamily.IsAvailable then
    begin
      FGPFamily.Free;
      FGPFamily := TGPFontFamily.GenericSansSerif.Clone;
    end
  end;
end;

procedure TCanvasGdiPlus.DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  ApplyStroke(ABrush, TRectF.Create(APt1.X, APt1.Y, APt2.X, APt2.Y), AOpacity);
  if FGPPen <> nil then
    FGPGraphics.DrawLine(FGPPen, APt1.X, APt1.y, APt2.X, APt2.y);
end;

procedure TCanvasGdiPlus.DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  ApplyStroke(ABrush, ARect, AOpacity);
  if FGPPen <> nil then
    FGPGraphics.DrawRectangle(FGPPen, GPRectFromRect(ARect));
end;

procedure TCanvasGdiPlus.DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
  ApplyFill(ABrush, ARect, AOpacity);
  FGPGraphics.FillRectangle(FGPBrush, GPRectFromRect(ARect));
end;

procedure TCanvasGdiPlus.DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  ApplyStroke(ABrush, ARect, AOpacity);
  if FGPPen <> nil then
    FGPGraphics.DrawEllipse(FGPPen, GPRectFromRect(ARect));
end;

procedure TCanvasGdiPlus.DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
  ApplyFill(ABrush, ARect, AOpacity);
  FGPGraphics.FillEllipse(FGPBrush, GPRectFromRect(ARect));
end;

function TCanvasGdiPlus.LoadFontFromStream(const AStream: TStream): Boolean;
var
  Stream: TMemoryStream;
begin
  if FFontCollection = nil then
    FFontCollection := TGPPrivateFontCollection.Create;
  Stream := TMemoryStream.Create;
  try
    Stream.CopyFrom(AStream, AStream.Size);
    Result := Stream.Size > 0;
    if Result then
      FFontCollection.AddMemoryFont(Stream.Memory, Stream.Size);
  finally
    Stream.Free;
  end;
end;

{ Bitmaps }

procedure TCanvasGdiPlus.DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
  const HighSpeed: Boolean);
var
  CM: TColorMatrix;
  ImageAttributes: TGPImageAttributes;
  GPBitmap: TGPBitmap;
  M: TBitmapData;
begin
  if HighSpeed then
    FGPGraphics.SetInterpolationMode(InterpolationModeNearestNeighbor)
  else
    FGPGraphics.SetInterpolationMode(InterpolationModeHighQuality);

  if (FPrinter <> nil) and not TCanvasManager.DefaultCanvas.InheritsFrom(TCanvasGdiPlus) then
  begin
    // In printer
    if ABitmap.Map(TMapAccess.Read, M) then
    try
      GPBitmap := TGPBitmap.Create(ABitmap.Width, ABitmap.Height, M.Pitch, PixelFormat32bppPARGB, M.Data);
      try
        if AOpacity < 1 then
        begin
          if SameValue(AOpacity, 0, TEpsilon.Vector) then
            Exit;
          if not ABitmap.HandleAllocated then
            Exit;
          CM := ImageColorMatrix;
          CM[3][3] := AOpacity;
          ImageAttributes := TGPImageAttributes.Create;
          try
            ImageAttributes.SetColorMatrix(CM, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
            FGPGraphics.DrawImage(GPBitmap, MakeRect(DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
              DstRect.Bottom - DstRect.Top), SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom -
              SrcRect.Top, UnitPixel, ImageAttributes);
          finally
            ImageAttributes.Free;
          end;
        end
        else
        begin
          if not ABitmap.HandleAllocated then
            Exit;
          FGPGraphics.DrawImage(GPBitmap, MakeRect(DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
            DstRect.Bottom - DstRect.Top), SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom -
            SrcRect.Top, UnitPixel);
        end;
      finally
        GPBitmap.Free;
      end;
    finally
      ABitmap.Unmap(M);
    end;
    Exit;
  end;

  if AOpacity < 1 then
  begin
    if SameValue(AOpacity, 0, TEpsilon.Vector) then
      Exit;
    if not ABitmap.HandleAllocated then
      Exit;
    CM := ImageColorMatrix;
    CM[3][3] := AOpacity;
    ImageAttributes := TGPImageAttributes.Create;
    try
      ImageAttributes.SetColorMatrix(CM, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
      FGPGraphics.DrawImage(TGPBitmap(ABitmap.Handle), MakeRect(DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
        DstRect.Bottom - DstRect.Top), SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom -
        SrcRect.Top, UnitPixel, ImageAttributes);
    finally
      ImageAttributes.Free;
    end;
  end
  else
  begin
    if not ABitmap.HandleAllocated then
      Exit;
    FGPGraphics.DrawImage(TGPBitmap(ABitmap.Handle), MakeRect(DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top), SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom -
      SrcRect.Top, UnitPixel);
  end;
end;

{ Path }

procedure TCanvasGdiPlus.DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  I: Integer;
  CP, CP1, CP2: TPointF;
  P: TGPGraphicsPath;
begin
  P := TGPGraphicsPath.Create;
  ApplyStroke(ABrush, APath.GetBounds, AOpacity);
  try
    I := 0;
    while I < APath.Count do
    begin
      case APath[I].Kind of
        TPathPointKind.MoveTo:
          begin
            CP := APath[I].Point;
            P.StartFigure;
          end;
        TPathPointKind.LineTo:
          begin
            P.AddLine(CP.X, CP.Y, APath[I].Point.X, APath[I].Point.Y);
            CP := APath[I].Point;
          end;
        TPathPointKind.CurveTo:
          begin
            CP1 := APath[I].Point;
            Inc(I);
            CP2 := APath[I].Point;
            Inc(I);
            P.AddBezier(CP.X, CP.Y, CP1.X, CP1.Y, CP2.X, CP2.Y, APath[I].Point.X, APath[I].Point.Y);
            CP := APath[I].Point;
          end;
        TPathPointKind.Close:
          begin
            P.CloseFigure;
          end;
      end;
      Inc(I);
    end;
    FGPGraphics.DrawPath(FGPPen, P);
  finally
    P.Free;
  end;
end;

procedure TCanvasGdiPlus.DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
var
  I: Integer;
  CP, CP1, CP2: TPointF;
  P: TGPGraphicsPath;
begin
  P := TGPGraphicsPath.Create;
  try
    I := 0;
    while I < APath.Count do
    begin
      case APath[I].Kind of
        TPathPointKind.MoveTo:
          begin
            CP := APath[I].Point;
            P.StartFigure;
          end;
        TPathPointKind.LineTo:
          begin
            P.AddLine(CP.X, CP.Y, APath[I].Point.X, APath[I].Point.Y);
            CP := APath[I].Point;
          end;
        TPathPointKind.CurveTo:
          begin
            CP1 := APath[I].Point;
            Inc(I);
            CP2 := APath[I].Point;
            Inc(I);
            P.AddBezier(CP.X, CP.Y, CP1.X, CP1.Y, CP2.X, CP2.Y, APath[I].Point.X, APath[I].Point.Y);
            CP := APath[I].Point;
          end;
        TPathPointKind.Close:
          begin
            P.CloseFigure;
          end;
      end;
      Inc(I);
    end;
    ApplyFill(ABrush, APath.GetBounds, AOpacity);
    FGPGraphics.FillPath(FGPBrush, P);
  finally
    P.Free;
  end;
end;

function TCanvasGdiPlus.PtInPath(const APoint: TPointF; const APath: TPathData): Boolean;
var
  I: Integer;
  B: TRectF;
  CP, CP1, CP2: TPointF;
  P: TGPGraphicsPath;
begin
  B :=  APath.GetBounds;
  if not B.Contains(APoint) then
    Result := False
  else
  begin
    P := TGPGraphicsPath.Create;
    try
      I := 0;
      while I < APath.Count do
      begin
        case APath[I].Kind of
          TPathPointKind.MoveTo:
            begin
              CP := APath[I].Point;
              P.StartFigure;
            end;
          TPathPointKind.LineTo:
            begin
              P.AddLine(CP.X, CP.Y, APath[I].Point.X, APath[I].Point.Y);
              CP := APath[I].Point;
            end;
          TPathPointKind.CurveTo:
            begin
              CP1 := APath[I].Point;
              Inc(I);
              CP2 := APath[I].Point;
              Inc(I);
              P.AddBezier(CP.X, CP.Y, CP1.X, CP1.Y, CP2.X, CP2.Y, APath[I].Point.X, APath[I].Point.Y);
              CP := APath[I].Point;
            end;
          TPathPointKind.Close:
            begin
              P.CloseFigure;
            end;
        end;
        Inc(I);
      end;
      Result := P.IsVisible(APoint.X, APoint.y);
    finally
      P.Free;
    end;
  end;
end;

class function TCanvasGdiPlus.DoInitializeBitmap(const Width, Height: Integer; const Scale: Single; var PixelFormat: TPixelFormat): THandle;
begin
  PixelFormat := TPixelFormat.BGRA;
  Result := THandle(TGPBitmap.Create(Width, Height, PixelFormat32bppPARGB));
end;

class procedure TCanvasGdiPlus.DoFinalizeBitmap(var Bitmap: THandle);
begin
  TGPBitmap(Bitmap).Free;
end;

class function TCanvasGdiPlus.DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean;
var
  Flags: UInt;
  GPBitmap: TGPBitmap;
  GPBitmapData: Winapi.GDIPAPI.TBitmapData;
begin
  case Access of
    TMapAccess.Read: Flags := ImageLockModeRead;
    TMapAccess.Write: Flags := ImageLockModeWrite;
  else
    Flags := ImageLockModeRead or ImageLockModeWrite;
  end;
  GPBitmap := TGPBitmap(Bitmap);
  if GPBitmap.LockBits(MakeRect(TRect.Create(0, 0, GPBitmap.GetWidth, GPBitmap.GetHeight)), Flags, PixelFormat32bppPARGB, GPBitmapData) = Ok then
  begin
    if FLockedBitmapData = nil then
      FLockedBitmapData := TDictionary<THandle, Winapi.GDIPAPI.TBitmapData>.Create;
    FLockedBitmapData.Add(Bitmap, GPBitmapData);
    Data.Data := GPBitmapData.Scan0;
    Data.Pitch := GPBitmapData.Stride;
    Result := True;
  end
  else
    Result := False;
end;

class procedure TCanvasGdiPlus.DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData);
var
  GPBitmapData: Winapi.GDIPAPI.TBitmapData;
begin
  if FLockedBitmapData.TryGetValue(Bitmap, GPBitmapData) then
  begin
    TGPBitmap(Bitmap).UnlockBits(GPBitmapData);
    FLockedBitmapData.Remove(Bitmap);
  end;
end;

{ TGDIPCanvasSaveState }

procedure TGDIPCanvasSaveState.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCanvasGdiPlus then
    FState := TCanvasGdiPlus(Source).Graphics.Save;
end;

procedure TGDIPCanvasSaveState.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCanvasGdiPlus then
    TCanvasGdiPlus(Dest).Graphics.Restore(FState);
end;

{ TTextLayoutGDIPlus }

procedure TTextLayoutGDIPlus.ConvertToPath(const APath: TPathData);
var
  GPPath: TGPGraphicsPath;
  GPFamily: TGPFontFamily;
  Data: Winapi.GDIPAPI.TPathData;
  SavePoints: PGPPointF;
  i: Integer;
  SP, CP1, CP2: TPointF;
begin
  if Text.IsEmpty then
    Exit;

  GPPath := TGPGraphicsPath.Create;
  // path
  GPFamily := TGPFontFamily.Create(Font.Family);
  if not GPFamily.IsAvailable then
  begin
    FreeAndNil(GPFamily);
    GPFamily := TGPFontFamily.Create(Font.Family);
    if not GPFamily.IsAvailable then
    begin
      FreeAndNil(GPFamily);
      GPFamily := TGPFontFamily.GenericSansSerif.Clone;
    end
  end;
  GPPath.AddString(Text, -1, GPFamily, vgStyleToGPStyle(Font.StyleExt),
    Font.Size, GPRectFromRect(RectF(0, 0,
      MaxSize.X - Padding.Left - Padding.Right,
      MaxSize.Y - Padding.Top - Padding.Bottom)), FStringFormat);
  // expand path
  Data := Winapi.GDIPAPI.TPathData.Create;
  GPPath.GetPathData(Data);
  SavePoints := Data.Points;
  // calc size
  i := 0;
  while i < Data.Count do
  begin
    if PByteArray(Data.Types)[i] = Integer(PathPointTypeStart) then
    begin
      SP := PointF(Data.Points^.X, Data.Points^.y);
      APath.MoveTo(PointF(Data.Points^.X, Data.Points^.y));
    end;
    if PByteArray(Data.Types)[i] and Integer(PathPointTypeBezier) = Integer(PathPointTypeBezier) then
    begin
      CP1 := PointF(Data.Points^.X, Data.Points^.y);
      Inc(i);
      Inc(Data.Points);
      CP2 := PointF(Data.Points^.X, Data.Points^.y);
      Inc(i);
      Inc(Data.Points);
      APath.CurveTo(CP1, CP2, PointF(Data.Points^.X, Data.Points^.Y));
    end;
    if PByteArray(Data.Types)[i] and Integer(PathPointTypeLine) = Integer(PathPointTypeLine) then
    begin
      APath.LineTo(PointF(Data.Points^.X, Data.Points^.Y));
    end;
    if PByteArray(Data.Types)[i] and Integer(PathPointTypeCloseSubpath) = Integer(PathPointTypeCloseSubpath) then
    begin
      APath.ClosePath;
    end;
    Inc(i);
    Inc(Data.Points);
  end;
  //
  Data.Points := SavePoints;
  FreeAndNil(Data);
  FreeAndNil(GPPath);
  FreeAndNil(GPFamily);
end;

constructor TTextLayoutGDIPlus.Create(const ACanvas: TCanvas);
var
  Bitmap: TGPBitmap;
begin
  inherited Create(ACanvas);
  if (LayoutCanvas <> nil) and PrinterAssigned and (LayoutCanvas = Printer.Canvas) then
  begin
    FGraphics := TCanvasGdiPlus(ACanvas).Graphics;
    FIsGraphicsOwner := False;
  end
  else
  begin
    Bitmap := TGPBitmap.Create(1, 1, PixelFormat32bppARGB);
    FGraphics := TGPGraphics.Create(Bitmap);
    FreeAndNil(Bitmap);
    if ACanvas is TCanvasGdiPlus then
      FGraphics.SetSmoothingMode(TCanvasGdiPlus(ACanvas).FSmoothingMode)
    else
      FGraphics.SetSmoothingMode(SmoothingModeUndetermined);
    FGraphics.SetInterpolationMode(InterpolationModeHighQuality);
    FGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    FGraphics.SetTextContrast(TextContrast);
    if GlobalUseGDIPlusClearType then
      FGraphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit)
    else
      FGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    FIsGraphicsOwner := True;
  end;
  FStringFormat := TGPStringFormat.Create(StringFormatFlagsMeasureTrailingSpaces);
  FTextRect := TRectF.Empty;
end;

function TTextLayoutGDIPlus.DefineTextForGDI: string;
begin
  Result := Text;
  if Result.IsEmpty then
    Result := '-'
  else
    if Result.Chars[Result.Length - 1] = ' ' then
      Result[Result.Length] := '-';
end;

destructor TTextLayoutGDIPlus.Destroy;
begin
  if FIsGraphicsOwner then
    FreeAndNil(FGraphics);
  FreeAndNil(FStringFormat);
  FreeAndNil(FGPFont);
  FreeAndNil(FGPBrush);
  inherited;
end;

procedure TTextLayoutGDIPlus.DoDrawLayout(const ACanvas: TCanvas);
var
  LayoutRect: TGPRectF;
begin
  if Text.IsEmpty then
    Exit;

  if FStringFormat = nil then
    Exit;

  if ACanvas is TCanvasGdiPlus then
  begin
    RecreateGPBrushIfNeeded;

    LayoutRect := OutputArea;

    LayoutRect.X := LayoutRect.X + TopLeft.X;
    LayoutRect.Y := LayoutRect.Y + TopLeft.Y;
    TCanvasGdiPlus(ACanvas).FGPGraphics.DrawString(Text, -1, FGPFont, LayoutRect, FStringFormat, FGPBrush);
  end;
end;

procedure TTextLayoutGDIPlus.DoRenderLayout;
var
  LayoutRect: TGPRectF;
  LText: string;
begin
  if (FStringFormat = nil) or (FGraphics = nil) then
    Exit;

  RecreateGPFont;
  InitStringFormat;

  //Measuring text size
  LayoutRect := OutputArea;
  LText := DefineTextForGDI;
  FGraphics.MeasureString(LText, -1, FGPFont, LayoutRect, FStringFormat, LayoutRect);

  FTextRect := RectFFromGPRect(LayoutRect);
end;

function TTextLayoutGDIPlus.GetFontScale: Single;
begin
  if LayoutCanvas = nil then
    Result := 1
  else
    Result := TCanvasGdiPlus(LayoutCanvas).FFontScale;
end;

function TTextLayoutGDIPlus.GetOutputArea: TGPRectF;
begin
  Result.X := Padding.Left;
  Result.Y := Padding.Top;
  Result.Width := MaxSize.X - Padding.Left - Padding.Right;
  Result.Height := MaxSize.Y - Padding.Top - Padding.Bottom;
end;

function TTextLayoutGDIPlus.GetTextHeight: Single;
begin
  Result := FTextRect.Height;
end;

function TTextLayoutGDIPlus.GetTextRect: TRectF;
begin
  Result := FTextRect;
  Result.Offset(TopLeft);
end;

function TTextLayoutGDIPlus.GetTextWidth: Single;
begin
  Result := FTextRect.Width;
end;

procedure TTextLayoutGDIPlus.InitStringFormat;
var
  Flags: Integer;
begin
  Flags := FStringFormat.GetFormatFlags;
  if WordWrap then
    Flags := Flags and not StringFormatFlagsNoWrap
  else
    Flags := Flags or StringFormatFlagsNoWrap;
  if RightToLeft then
    Flags := Flags or StringFormatFlagsDirectionRightToLeft;
  FStringFormat.SetFormatFlags(Flags);
  FStringFormat.SetTrimming(TextTrimmingToStringTrimming(Trimming));
  FStringFormat.SetAlignment(TextAlignToStringTrimming(HorizontalAlign));
  FStringFormat.SetLineAlignment(TextAlignToStringTrimming(VerticalAlign));
end;

function TTextLayoutGDIPlus.MeasureRange(const APos, ALength: Integer): TRegion;
type
  TRegions = array of TGPRegion;

  function AllocateRegions(const ACount: Integer): TRegions;
  var
    I: Integer;
  begin
    SetLength(Result, ACount);
    for I := 0 to ACount - 1 do
      Result[I] := TGPRegion.Create;
  end;

var
  CharRange: TCharacterRange;
  Regions: TRegions;
  I, J, Count: Integer;
{$IFDEF CPUX64}
  SavedExceptionMask: TArithmeticExceptionMask;
{$ENDIF CPUX64}
  M: TGPMatrix;
  RectCount: Integer;
  Rects: array of TGPRectF;
  LText: string;
  LayoutRect: TGPRectF;
begin
  SetLength(Result, 0);
  LText := DefineTextForGDI;
  if (APos < LText.Length) and LText.Chars[APos].IsLowSurrogate then
    CharRange := MakeCharacterRange(APos - 1, ALength + 1)
  else
    CharRange := MakeCharacterRange(APos, ALength);
  FStringFormat.SetMeasurableCharacterRanges(1, @CharRange);
  Count := FStringFormat.GetMeasurableCharacterRangeCount;

  Regions := AllocateRegions(Count);
  // measure
{$IFDEF CPUX64}
  SavedExceptionMask := GetExceptionMask;
  SetExceptionMask(exAllArithmeticExceptions);
  try
{$ENDIF CPUX64}
    LayoutRect := GetOutputArea;
    FGraphics.MeasureCharacterRanges(LText, -1, FGPFont, LayoutRect, FStringFormat, Count, Regions);
{$IFDEF CPUX64}
  finally
    SetExceptionMask(SavedExceptionMask); // restore SSE's exception mask.
  end;
{$ENDIF CPUX64}

  M := TGPMatrix.Create;
  try
    for I := 0 to Count - 1 do
    begin
      RectCount := Regions[I].GetRegionScansCount(M);
      if RectCount > 0 then
      begin
        SetLength(Rects, RectCount);
        Regions[I].GetRegionScans(M, PGPRectF(@Rects[0]), RectCount);
        for J := 0 to RectCount - 1 do
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := RectFFromGPRect(Rects[J]);
          if Text.IsEmpty then
            Result[High(Result)].Width := 0;
        end;
      end;
      FreeAndNil(Regions[I]);
    end;
  finally
    FreeAndNil(M);
  end;
end;

procedure TTextLayoutGDIPlus.RecreateGPBrushIfNeeded;
var
  LOpacity: Single;
  LColor: Cardinal;
begin
  // RS-31314
  if Opacity = 1 then
    LOpacity := 0.99
  else
    LOpacity := Opacity;
  if (FGPBrush = nil) or ((TGPSolidBrush(FGPBrush).GetColor(LColor) = TStatus.Ok) and
     (LColor <> MakeColor(Color, LOpacity))) then
  begin
    FreeAndNil(FGPBrush);
    FGPBrush := TGPSolidBrush.Create(MakeColor(Color, LOpacity));
  end;
end;

procedure TTextLayoutGDIPlus.RecreateGPFont;
var
  LDC: HDC;
  LHFont: HFONT;
begin
  FreeAndNil(FGPFont);
  LDC := GetDC(0);
  try
    LHFont := CreateFont(-Round(Font.Size * FontScale), 0, 0, 0,
      FontWeightToWinapi(Font.StyleExt.Weight),
      DWORD(not Font.StyleExt.Slant.IsRegular),
      DWORD(TFontStyle.fsUnderline in Font.StyleExt.SimpleStyle),
      DWORD(TFontStyle.fsStrikeOut in Font.StyleExt.SimpleStyle), 0, 0, 0, 0, 0, PChar(Font.Family));
    try
      FGPFont := TGPFont.Create(LDC, LHFont);
    finally
      DeleteObject(LHFont);
    end;
  finally
    ReleaseDC(0, LDC);
  end;

  if FGPFont = nil then
    FGPFont := TGPFont.Create(TCanvasGdiPlus(LayoutCanvas).FGPFamily, Font.Size * FontScale,
                              vgStyleToGPStyle(Font.StyleExt), UnitPoint);
end;

function TTextLayoutGDIPlus.DoPositionAtPoint(const APoint: TPointF): Integer;

  function RegionContains(const ARegion: TRegion; const APoint: TPointF): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(ARegion) to High(ARegion) do
      if IsPointInRect(APoint, ARegion[I]) then
        Exit(True);
  end;

  //Check if a point hits region line
  function RegionLinesContain(const ARegion: TRegion; const APoint: TPointF): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := High(ARegion) downto 0 do
      if (APoint.Y >= ARegion[I].Top) and (APoint.Y < ARegion[I].Bottom) and (APoint.X >= ARegion[I].Left) then
        Exit(True);
  end;

var
  RegionL, RegionR: TRegion;
  LPoint: TPointF;
  L, M, R: Integer;
  LRect: TRectF;
begin
  Result := -1;
  LRect := Self.TextRect;
  if not IsPointInRect(APoint, LRect) then
  begin
    if IsPointInRect(APoint, RectF(LRect.Left, LRect.Top, LRect.Left + MaxSize.X, LRect.Bottom)) then
      Result := Text.Length
    else
      if ((APoint.X < LRect.Left) or SameValue(APoint.X, LRect.Left, TEpsilon.Position)) and
         ((APoint.Y > LRect.Top) or SameValue(APoint.Y, LRect.Top, TEpsilon.Position)) and
         ((APoint.Y < LRect.Bottom) or SameValue(APoint.Y, LRect.Bottom, TEpsilon.Position)) then
        Result := 0;
    Exit;
  end;
  if Text.IsEmpty then
    Exit(0);
  LPoint := APoint - TopLeft;
  //Using binary search to find point position
  L := 0;
  R := Text.Length - 1;
  while L <= R do
  begin
    M := (L + R) shr 1;
    RegionL := MeasureRange(L, M - L + 1);
    RegionR := MeasureRange(M + 1, R - M);
    //RegionLinesContain: when there is no intersection check for a click on the last line. This wouldn't break
    //condition because we have layout rectangle condition above (IsPointInRect(APoint, LRect)
    if RegionContains(RegionR, LPoint) or RegionLinesContain(RegionR, LPoint) then
      L := M + 1
    else
    begin
      if (M - L) = 0 then
      begin
        Result := M;
        //The remaining part of the region is one character wide and the click happened on the right-hand half of the
        //region. In this case we need to get the position that follows the region immediately
        if LPoint.X > (RegionL[0].Left + RegionL[0].Width * 3 / 5) then
          Inc(Result);
        Exit;
      end;
      R := M;
    end;
  end;
  if InRange(Result, 0, Text.Length - 1) and Text.Chars[Result].IsLowSurrogate then
    Inc(Result);
end;

function TTextLayoutGDIPlus.DoRegionForRange(const ARange: TTextRange): TRegion;
var
  I: Integer;
begin
  SetLength(Result, 0);
  if (ARange.Pos < 0) or (ARange.Length < 0) then
    Exit;

  if (ARange.Pos = Text.Length) and (ARange.Length = 0) then
    if Text.IsEmpty then
    begin
      SetLength(Result, 1);
      Result[0] := Self.TextRect;
      Exit;
    end
    else
    begin
      Result := MeasureRange(Text.Length - 1, 1);
      for I := Low(Result) to High(Result) do
        Result[I].Left := Result[I].Right;
    end
  else
  begin
    Result := MeasureRange(ARange.Pos, ARange.Length);
    if Length(Result) = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := Self.TextRect;
      Result[0].Left := Result[0].Right;
      Exit;
    end;
  end;
  for I := Low(Result) to High(Result) do
    Result[I].Offset(TopLeft);
end;

procedure RegisterCanvasClasses;
begin
  TCanvasManager.RegisterCanvas(TCanvasGdiPlus, False, True);
end;

procedure UnregisterCanvasClasses;
begin
  TCanvasGdiPlus.DestroySharedResources;
end;

initialization
  TTextLayoutManager.RegisterTextLayout(TTextLayoutGDIPlus, TCanvasGdiPlus);
finalization
  FreeAndNil(TCanvasGdiPlus.FLockedBitmapData);
end.


