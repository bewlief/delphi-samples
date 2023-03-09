{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Screen.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Generics.Collections, Winapi.Windows, FMX.Types, FMX.Platform, FMX.Platform.Win;

type

{ TWinMultiDisplay }

  TDpPxConverter = class;

  TOrderedDictionary<TKey, TValue> = class
  private
    FValues: TList<TValue>;
    FIndex: TDictionary<TKey, Integer>;
    function GetCount: Integer;
    function GetValues(const AIndex: Integer): TValue;
    procedure SetValues(const AIndex: Integer; const Value: TValue);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AKey: TKey; const AValue: TValue);
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    procedure Clear;
    property Values[const AIndex: Integer]: TValue read GetValues write SetValues; default;
    property Count: Integer read GetCount;
  end;

  TWinMultiDisplay = class(TInterfacedObject, IFMXMultiDisplayService, IFMXScreenService, IFMXDeviceMetricsService)
  public const
    StandardDpi = 96;
  private type
    TParameter = (DisplayCount, Displays, WorkareaRect, DesktopRect, PhysicalDesktopRect);
    TParameters = set of TParameter;
  private
    FDisplays: TOrderedDictionary<HMONITOR, TDisplay>;
    FDisplayCount: Integer;
    FWorkAreaRect: TRect;
    FDesktopRect: TRectF;
    FPhysicalDesktopRect: TRect;
    FOutdatedParameters: TParameters;
    FPxDpConverter: TDpPxConverter;
    FPrimaryDisplay: TDisplay;
    FIsPrimaryDisplayDefined: Boolean;
    procedure AddDisplay(const AMonitorHandle: HMONITOR);
    procedure UpdateDisplaysIfNeeded;
    class function GetMonitorScale(const AHandle: HMONITOR): Single;
    class function EnumMonitorsProc(hm: HMONITOR; dc: HDC; R: PRect; Data: Pointer): Boolean; stdcall; static;
  protected
    function FindDisplay(const AMonitorHandle: HMONITOR): TDisplay;
    { Primary display }
    procedure SetNewPrimaryDisplay(const ADisplay: TDisplay);
    procedure ResetPrimaryDisplay;
  public
    constructor Create;
    destructor Destroy; override;
    class function ScaleRect(const ARect: TRect; const AScale: Single): TRect;

    function PxToDp(const AValue: TPoint): TPointF;
    function DpToPx(const AValue: TPointF): TPoint;

    { IFMXMultiDisplayService }
    function GetDisplayCount: Integer;
    function GetWorkAreaRect: TRectF;
    function GetPhysicalWorkAreaRect: TRect;
    function GetDesktopRect: TRectF;
    function GetPhysicalDesktopRect: TRect;
    function GetDisplay(const AIndex: Integer): TDisplay;
    function GetDesktopCenterRect(const ASize: TSizeF): TRectF;
    function DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
    function DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
    procedure UpdateDisplayInformation;

    { IFMXScreenService }
    function GetScreenSize: TPointF;
    function GetScreenScale: Single;
    function GetScreenOrientation: TScreenOrientation;
    procedure SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);

    { IFMXDeviceMetricsService }
    function GetDisplayMetrics: TDeviceDisplayMetrics;
  public
    property IsPrimaryDisplayDefined: Boolean read FIsPrimaryDisplayDefined;
    property PrimaryDisplay: TDisplay read FPrimaryDisplay;
  end;

  /// <summary><para>PX - DP coordinate converter. It takes into account the features of the location of monitors,
  /// their scaling factors.</para>
  /// <para>The converter divides the entire desktop space into sections, each of which defines the rules for
  /// converting coordinates in this section.</para>
  /// </summary>
  TDpPxConverter = class
  private type
    TTable<T> = array of array of T;
    TCell = record
      Scale: Single;
      LocationPX: TPoint;
      LocationDP: TPointF;
      SizePX: TSize;
      SizeDP: TSizeF;
      IsDisplayPart: Boolean;
    end;
  private
    FDisplays: TList<TDisplay>;
    FColumns: TList<Integer>; // px
    FRows: TList<Integer>; // px
    FTable: TTable<TCell>;
    procedure AddColumn(const AValue: Integer);
    procedure AddRow(const AValue: Integer);
    function GetDisplayScaleByPX(const AHitPoint: TPoint): Single;
    function ContainsAnyDisplay(const AHitPoint: TPoint): Boolean;
    procedure SetTableSize(const AColumnCount, ARowCount: Integer);
    function GetCellSizePX(const AColumn, ARow: Integer): TSize;
    procedure SetCellLocationDP(const AColumn, ARow: Integer; const APoint: TPointF);
  protected
    function DefineCellByPX(const APoint: TPoint; out AColumn, ARow: Integer): Boolean;
    function DefineCellByDP(const APoint: TPointF; out AColumn, ARow: Integer): Boolean;
    procedure RebuildTable;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Multiplies <c>ASize</c> value on <c>AScale</c>.</summary>
    class function ScaleSize(const ASize: TSize; const AScale: Single): TSizeF;

    /// <summary>Converts PX to DP.</summary>
    function PXToDP(const AValue: TPoint): TPointF;
    /// <summary>Converts DP to PX.</summary>
    function DPToPX(const AValue: TPointF): TPoint;
    /// <summary>Adds display information into converter.</summary>
    procedure AddDisplay(const ADisplay: TDisplay);
    /// <summary>Clears all added displays.</summary>
    procedure Clear;
    /// <summary>Returns logical size of Desktop rect in dp.</summary>
    function CalculateDesktopRect: TRectF;
    function ToString: string; override;
  end;

implementation

uses
  System.SysUtils, System.Math, System.RTLConsts, System.Generics.Defaults, Winapi.MultiMon, Winapi.ShellScaling,
  FMX.Controls, FMX.Forms, FMX.Helpers.Win;

procedure RaiseIfNil(const AObject: TObject; const AArgumentName: string);
begin
  if AObject = nil then
    raise EArgumentException.CreateFmt(SParamIsNil, [AArgumentName]);
end;

{ TOrderedDictionary<TKey, TValue> }

procedure TOrderedDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  FIndex.Add(AKey, FIndex.Count);
  FValues.Add(AValue);
end;

procedure TOrderedDictionary<TKey, TValue>.Clear;
begin
  FValues.Clear;
  FIndex.Clear;
end;

constructor TOrderedDictionary<TKey, TValue>.Create;
begin
  FValues := TList<TValue>.Create;
  FIndex := TDictionary<TKey, Integer>.Create;
end;

destructor TOrderedDictionary<TKey, TValue>.Destroy;
begin
  FreeAndNil(FIndex);
  FreeAndNil(FValues);
  inherited;
end;

function TOrderedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TOrderedDictionary<TKey, TValue>.GetValues(const AIndex: Integer): TValue;
begin
  Result := FValues[AIndex];
end;

procedure TOrderedDictionary<TKey, TValue>.SetValues(const AIndex: Integer; const Value: TValue);
begin
  FValues[AIndex] := Value;
end;

function TOrderedDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
var
  Index: Integer;
begin
  Result := FIndex.TryGetValue(AKey, Index);
  if Result then
    AValue := FValues[Index];
end;

{ TWinMultiDisplay }

procedure TWinMultiDisplay.AddDisplay(const AMonitorHandle: HMONITOR);
var
  MonInfo: TMonitorInfo;
  Display: TDisplay;
begin
  MonInfo.cbSize := SizeOf(MonInfo);
  if GetMonitorInfo(AMonitorHandle, @MonInfo) then
  begin
    Display.Id := AMonitorHandle;
    Display.Index := FDisplays.Count;
    Display.Primary := (MonInfo.dwFlags and MONITORINFOF_PRIMARY) <> 0;
    Display.PhysicalBounds := MonInfo.rcMonitor; // px
    Display.PhysicalWorkarea := MonInfo.rcWork; // px
    Display.Scale := GetMonitorScale(AMonitorHandle);
    Display.Bounds := TWinMultiDisplay.ScaleRect(MonInfo.rcMonitor, Display.Scale); // dp
    Display.Workarea := TWinMultiDisplay.ScaleRect(MonInfo.rcWork, Display.Scale); // dp

    FDisplays.Add(AMonitorHandle, Display);
  end;
end;

constructor TWinMultiDisplay.Create;
begin
  FDisplays := TOrderedDictionary<HMONITOR, TDisplay>.Create;
  FWorkAreaRect := TRect.Empty;
  FDesktopRect := TRect.Empty;
  FOutdatedParameters := [Low(TParameter)..High(TParameter)];

  FPxDpConverter := TDpPxConverter.Create;
  UpdateDisplaysIfNeeded;
end;

destructor TWinMultiDisplay.Destroy;
begin
  FreeAndNil(FPxDpConverter);
  FreeAndNil(FDisplays);
  inherited;
end;

function TWinMultiDisplay.GetDisplayCount: Integer;
begin
  if TParameter.DisplayCount in FOutdatedParameters then
  begin
    FDisplayCount := GetSystemMetrics(SM_CMONITORS);
    Exclude(FOutdatedParameters, TParameter.DisplayCount);
  end;
  Result := FDisplayCount;
end;

function TWinMultiDisplay.GetDisplayMetrics: TDeviceDisplayMetrics;
var
  R: TRect;
begin
  Winapi.Windows.GetWindowRect(GetDesktopWindow, R);
  Result.PhysicalScreenSize := TSize.Create(R.Width, R.Height);
  Result.RawScreenSize := Result.PhysicalScreenSize;
  Result.LogicalScreenSize := Result.PhysicalScreenSize;
  if Result.PhysicalScreenSize.cx > 0 then
    Result.AspectRatio := Result.PhysicalScreenSize.cy / Result.PhysicalScreenSize.cx
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := StandardDpi;
  Result.ScreenScale := 1;
  Result.FontScale := 1;
end;

class function TWinMultiDisplay.GetMonitorScale(const AHandle: HMONITOR): Single;
var
  DpiX, DpiY: Cardinal;
begin
  if TOSVersion.Check(6, 3) then
  begin
    if GetDPIForMonitor(AHandle, MDT_Default, DpiX, DpiY) = S_OK then
      Result := DpiX / StandardDpi
    else
      Result := 1;
  end
  else
    Result := 1;
end;

function TWinMultiDisplay.GetPhysicalDesktopRect: TRect;
begin
  if TParameter.PhysicalDesktopRect in FOutdatedParameters then
  begin
    FPhysicalDesktopRect.Left := GetSystemMetrics(SM_XVIRTUALSCREEN);
    FPhysicalDesktopRect.Top := GetSystemMetrics(SM_YVIRTUALSCREEN);
    FPhysicalDesktopRect.Width := GetSystemMetrics(SM_CXVIRTUALSCREEN);
    FPhysicalDesktopRect.Height := GetSystemMetrics(SM_CYVIRTUALSCREEN);
    Exclude(FOutdatedParameters, TParameter.PhysicalDesktopRect);
  end;
  Result := FPhysicalDesktopRect;
end;

function TWinMultiDisplay.GetPhysicalWorkAreaRect: TRect;
begin
  if (TParameter.WorkareaRect in FOutdatedParameters) and SystemParametersInfo(SPI_GETWORKAREA, 0, FWorkAreaRect, 0) then
    Exclude(FOutdatedParameters, TParameter.WorkareaRect);
  Result := FWorkAreaRect;
end;

function TWinMultiDisplay.GetScreenOrientation: TScreenOrientation;
begin
  Result := TScreenOrientation.Landscape;
end;

function TWinMultiDisplay.GetScreenScale: Single;

  function GetScaleFromDC: Single;
  var
    DC: HDC;
  begin
    DC := GetDC(0);
    try
      Result := GetDCScale(DC);
    finally
      ReleaseDC(0, DC);
    end;
  end;

begin
  UpdateDisplaysIfNeeded;

  if IsPrimaryDisplayDefined then
    Result := PrimaryDisplay.Scale
  else
    Result := GetScaleFromDC;
end;

function TWinMultiDisplay.GetScreenSize: TPointF;
var
  WR: TRect;
  Scale: Single;
begin
  Winapi.Windows.GetWindowRect(GetDesktopWindow, WR);
  Scale := GetScreenScale;
  Result := TPointF.Create(WR.Width / Scale, WR.Height / Scale);
end;

function TWinMultiDisplay.GetDesktopCenterRect(const ASize: TSizeF): TRectF;

  function PointOutOfDisplay(const P: TPointF): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to GetDisplayCount - 1 do
      if GetDisplay(I).Bounds.Contains(P) then
        Exit(False);
    Result := True;
  end;

  function CornerOutOfDisplay(const R: TRectF): Boolean;
  begin
    Result := PointOutOfDisplay(R.TopLeft) or PointOutOfDisplay(TPointF.Create(R.Right, R.Top)) or
              PointOutOfDisplay(R.BottomRight) or PointOutOfDisplay(TPointF.Create(R.Left, R.Bottom));
  end;

var
  I, MinI: Integer;
  DesktopCenter: TPointF;
  Dist, MinDist: Double;
  WorkArea: TRect;
begin
  DesktopCenter := GetDesktopRect.CenterPoint;
  Result := TRectF.Create(TPointF.Create(DesktopCenter.X - ASize.cx / 2, DesktopCenter.Y - ASize.cy / 2), ASize.cx, ASize.cy);
  MinDist := MaxInt;
  MinI := -1;

  for I := 0 to GetDisplayCount - 1 do
  begin
    Dist := GetDisplay(I).WorkArea.CenterPoint.Distance(DesktopCenter);
    if Dist < MinDist then
    begin
      MinDist := Dist;
      MinI := I;
    end;
  end;
  WorkArea := GetDisplay(MinI).WorkArea.Round;
  if CornerOutOfDisplay(Result) then
  begin
    if Result.Top < WorkArea.Top then
      Result.SetLocation(Result.Left, WorkArea.Top);
    if Result.Bottom > WorkArea.Bottom then
      Result.SetLocation(Result.Left, WorkArea.Bottom - Result.Height);
    if CornerOutOfDisplay(Result) then
    begin
      if Result.Left < WorkArea.Left then
        Result.SetLocation(WorkArea.Left, Result.Top);
      if Result.Right > WorkArea.Right then
        Result.SetLocation(WorkArea.Right - Result.Width, Result.Top);
    end;
  end;
end;

function TWinMultiDisplay.GetDesktopRect: TRectF;
begin
  if TParameter.DesktopRect in FOutdatedParameters then
  begin
    FDesktopRect := FPxDpConverter.CalculateDesktopRect;
    Exclude(FOutdatedParameters, TParameter.DesktopRect);
  end;
  Result := FDesktopRect;
end;

function TWinMultiDisplay.GetWorkAreaRect: TRectF;
begin
  if (TParameter.WorkareaRect in FOutdatedParameters) and SystemParametersInfo(SPI_GETWORKAREA, 0, FWorkAreaRect, 0) then
  begin
    FWorkAreaRect := ScaleRect(FWorkAreaRect, GetScreenScale);
    Exclude(FOutdatedParameters, TParameter.WorkareaRect);
  end;
  Result := FWorkAreaRect;
end;

function TWinMultiDisplay.PxToDp(const AValue: TPoint): TPointF;
begin
  UpdateDisplaysIfNeeded;

  Result := FPxDpConverter.PXToDP(AValue);
end;

procedure TWinMultiDisplay.ResetPrimaryDisplay;
begin
  FIsPrimaryDisplayDefined := False;
end;

procedure TWinMultiDisplay.SetNewPrimaryDisplay(const ADisplay: TDisplay);
begin
  FPrimaryDisplay := ADisplay;
  FIsPrimaryDisplayDefined := True;
end;

class function TWinMultiDisplay.ScaleRect(const ARect: TRect; const AScale: Single): TRect;
begin
  Result.Left := Round(ARect.Left / AScale);
  Result.Right := Round(ARect.Right / AScale);
  Result.Top := Round(ARect.Top / AScale);
  Result.Bottom := Round(ARect.Bottom / AScale);
end;

procedure TWinMultiDisplay.SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);
begin
  // Not needed for Windows
end;

procedure TWinMultiDisplay.UpdateDisplayInformation;
begin
  FOutdatedParameters := [Low(TParameter)..High(TParameter)];
end;

function TWinMultiDisplay.FindDisplay(const AMonitorHandle: HMONITOR): TDisplay;
begin
  UpdateDisplaysIfNeeded;

  if not FDisplays.TryGetValue(AMonitorHandle, Result) then
    raise EInvalidArgument.Create(sArgumentInvalid)
end;

function TWinMultiDisplay.DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
var
  Handle: TWinWindowHandle;
  Form, ParentForm: TCommonCustomForm;
  ParentControl: TFmxObject;
  Control: TControl;
  P: TPointF;
  ScreenPosition: TPoint;
  Scene: IScene;
begin
  RaiseIfNil(AHandle, 'AHandle');

  Handle := WindowHandleToPlatform(AHandle);
  Form := Handle.Form;
  ParentControl := nil;
  if Form.IsPopupForm then
  begin
    if Form is TCustomPopupForm then
    begin
      if GetKeyState(VK_RBUTTON) <> 0 then
      begin
        GetCursorPos(ScreenPosition);
        Result := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
        Exit;
      end;
      ParentControl := TCustomPopupForm(Form).PlacementTarget;
    end;
    ParentForm := Form.ParentForm;
    if (ParentControl = nil) and (ParentForm <> nil) then
    begin
      while TCommonCustomForm.IsPopupForm(ParentForm) and (ParentForm.ParentForm <> nil) do
        ParentForm := ParentForm.ParentForm;
      if ParentControl = nil then
      begin
        ParentControl := Form.Parent;
        while (ParentControl <> nil) and (ParentControl.Root <> nil) and (ParentControl.Root.GetObject <> ParentForm) do
          ParentControl := ParentControl.Parent;
        while (ParentControl <> nil) and not (ParentControl is TControl) do
          ParentControl := ParentControl.Parent;
      end;
      if (ParentControl = nil) and (ParentForm <> nil) and (ParentForm.Focused <> nil) then
        ParentControl := ParentForm.Focused.GetObject;
    end;
    if ParentControl is TControl then
    begin
      Control := TControl(ParentControl);
      P := Control.Padding.Rect.TopLeft;
      P := Control.LocalToAbsolute(P);
      if Supports(Control.Root.GetObject, IScene, Scene) then
        P := Scene.LocalToScreen(P);
      P.Offset((Control.Width - Control.Padding.Right) / 2, (Control.Height - Control.Padding.Bottom) / 2);
      ScreenPosition := TPoint.Create(Round(P.X), Round(P.Y));
      Result := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
      Exit;
    end;
    if ParentForm <> nil then
      Handle := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Handle = nil) or (Handle.Wnd = 0) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Winapi.MultiMon.MonitorFromWindow(Handle.Wnd, MONITOR_DEFAULTTONEAREST));
end;

function TWinMultiDisplay.DpToPx(const AValue: TPointF): TPoint;
begin
  UpdateDisplaysIfNeeded;

  Result := FPxDpConverter.DPToPX(AValue);
end;

class function TWinMultiDisplay.EnumMonitorsProc(hm: HMONITOR; dc: HDC; R: PRect; Data: Pointer): Boolean;
var
  Sender: TWinMultiDisplay;
begin
  Sender := TWinMultiDisplay(Data);
  Sender.AddDisplay(hm);
  Result := True;
end;

function TWinMultiDisplay.DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
var
  Handle: TWinWindowHandle;
  Form: TCommonCustomForm;
  DispByPoint, DispByMouse: TDisplay;
  PointF: TPointF;
  ScreenPosition: TPoint;
begin
  RaiseIfNil(AHandle, 'AHandle');

  Handle := WindowHandleToPlatform(AHandle);
  if (Handle = nil) or (Handle.Wnd = 0) then
    raise EArgumentException.Create(sArgumentInvalid);
  Form := Handle.Form;
  if not Form.IsPopupForm then
  begin
    PointF := TPointF.Create(APoint);
    ScreenPosition := Form.ClientToScreen(PointF).Round;
    DispByPoint := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
    if (GetKeyState(VK_RBUTTON) <> 0) or (GetKeyState(VK_LBUTTON) <> 0) or (GetKeyState(VK_MBUTTON) <> 0) then
    begin
      Result := DisplayFromWindow(AHandle);
      GetCursorPos(ScreenPosition);
      DispByMouse := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
      if DispByMouse.Index <> Result.Index then
        Result := DispByPoint;
    end
    else
      Result := DispByPoint;
  end
  else if Form.Visible and (Form is TCustomPopupForm) then
    Result := FindDisplay(Winapi.MultiMon.MonitorFromWindow(Handle.Wnd, MONITOR_DEFAULTTONEAREST))
  else
    Result := DisplayFromWindow(AHandle);
end;

procedure TWinMultiDisplay.UpdateDisplaysIfNeeded;
var
  I: Integer;
  Display: TDisplay;
begin
  if TParameter.Displays in FOutdatedParameters then
  begin
    FDisplays.Clear;
    ResetPrimaryDisplay;
    EnumDisplayMonitors(0, nil, @TWinMultiDisplay.EnumMonitorsProc, Winapi.Windows.LPARAM(Self));
    Exclude(FOutdatedParameters, TParameter.Displays);

    { Adjust logical rects }
    FPxDpConverter.Clear;
    for I := 0 to FDisplays.Count - 1 do
      FPxDpConverter.AddDisplay(FDisplays[I]);

    for I := 0 to FDisplays.Count - 1 do
    begin
      Display := FDisplays[I];
      Display.Workarea := TRectF.Create(FPxDpConverter.PXToDP(Display.PhysicalWorkarea.TopLeft), Display.Workarea.Width, Display.Workarea.Height);
      Display.Bounds := TRectF.Create(FPxDpConverter.PXToDP(Display.PhysicalBounds.TopLeft), Display.Workarea.Width, Display.Bounds.Height);;
      FDisplays[I] := Display;
      if Display.Primary then
        SetNewPrimaryDisplay(Display);
    end;
  end;
end;

function TWinMultiDisplay.GetDisplay(const AIndex: Integer): TDisplay;
begin
  UpdateDisplaysIfNeeded;

  if (AIndex < 0) or (AIndex >= GetDisplayCount) then
    raise EListError.CreateFMT(SListIndexError, [AIndex]);
  Result := FDisplays[AIndex];
end;

{ TDpPxConverter }

procedure TDpPxConverter.AddColumn(const AValue: Integer);
begin
  if FColumns.Contains(AValue) then
    Exit;

  FColumns.Add(AValue);
  FColumns.Sort;
end;

procedure TDpPxConverter.AddRow(const AValue: Integer);
begin
  if FRows.Contains(AValue) then
    Exit;

  FRows.Add(AValue);
  FRows.Sort;
end;

function TDpPxConverter.CalculateDesktopRect: TRectF;
var
  Col: Integer;
  Row: Integer;
  Cell: TCell;
begin
  Result := TRectF.Empty;
  for Col := Low(FTable) to High(FTable) do
    for Row := Low(FTable[Col]) to High(FTable[Col]) do
    begin
      Cell := FTable[Col, Row];
      // Accounts only display cells.
      if not Cell.IsDisplayPart then
        Continue;
      Result.Left := Min(Result.Left, Cell.LocationDP.X);
      Result.Top := Min(Result.Top, Cell.LocationDP.Y);
      Result.Right := Max(Result.Right, Cell.LocationDP.X + Cell.SizeDP.Width);
      Result.Bottom := Max(Result.Bottom, Cell.LocationDP.Y + Cell.SizeDP.Height);
    end;
end;

function TDpPxConverter.GetDisplayScaleByPX(const AHitPoint: TPoint): Single;
var
  I: Integer;
  Display: TDisplay;
begin
  for I := 0 to FDisplays.Count - 1 do
  begin
    Display := FDisplays[I];
    if Display.PhysicalBounds.Contains(AHitPoint) then
      Exit(Display.Scale);
  end;

  Result := 1;
end;

procedure TDpPxConverter.SetTableSize(const AColumnCount, ARowCount: Integer);
var
  Col: Integer;
begin
  SetLength(FTable, AColumnCount);
  for Col := Low(FTable) to High(FTable) do
    SetLength(FTable[Col], ARowCount);
end;

function TDpPxConverter.GetCellSizePX(const AColumn, ARow: Integer): TSize;
begin
  Assert(InRange(AColumn, 0, FColumns.Count - 2));
  Assert(InRange(ARow, 0, FRows.Count - 2));

  Result := TSize.Create(FColumns[AColumn + 1] - FColumns[AColumn], FRows[ARow + 1] - FRows[ARow]);
end;

procedure TDpPxConverter.SetCellLocationDP(const AColumn, ARow: Integer; const APoint: TPointF);
var
  Cell: TCell;
begin
  Assert(InRange(AColumn, 0, High(FTable)));
  Assert(InRange(ARow, 0, High(FTable[0])));

  Cell := FTable[AColumn, ARow];
  Cell.LocationDP := APoint;
  FTable[AColumn, ARow] := Cell;
end;

function TDpPxConverter.DefineCellByPX(const APoint: TPoint; out AColumn, ARow: Integer): Boolean;
var
  Col: Integer;
  Row: Integer;
begin
  AColumn := -1;
  for Col := 0 to FColumns.Count - 2 do
    if APoint.X >= FColumns[Col] then
      AColumn := Col
    else
      Break;

  ARow := -1;
  for Row := 0 to FRows.Count - 2 do
    if APoint.Y >= FRows[Row] then
      ARow := Row
    else
      Break;

  // if Point is placed out of display area, we are rounding it to nearest right/bottom cell.
  if (ARow = -1) and (FRows.Count > 0) then
     ARow := 0;
  if (AColumn = -1) and (FColumns.Count > 0) then
     AColumn := 0;

  Result := (ARow <> -1) and (AColumn <> -1);
end;

function TDpPxConverter.DefineCellByDP(const APoint: TPointF; out AColumn, ARow: Integer): Boolean;
var
  Col: Integer;
  Row: Integer;
  Cell: TCell;
begin
  AColumn := -1;
  ARow := -1;
  for Col := Low(FTable) to High(FTable) do
    for Row := Low(FTable[Col]) to High(FTable[Col]) do
    begin
      Cell := FTable[Col, Row];
      if InRange(APoint.X, Cell.LocationDP.X, Cell.LocationDP.X + Cell.SizeDP.Width) and
         InRange(APoint.Y, Cell.LocationDP.Y, Cell.LocationDP.Y + Cell.SizeDP.Height) then
      begin
        AColumn := Col;
        ARow := Row;
        Break;
      end;
    end;

  // if Point is placed out of display area, we are rounding it to nearest right/bottom cell.
  if (ARow = -1) and (FRows.Count > 0) then
     ARow := 0;
  if (AColumn = -1) and (FColumns.Count > 0) then
     AColumn := 0;

  Result := (ARow <> -1) and (AColumn <> -1);
end;

procedure TDpPxConverter.RebuildTable;
type
  TDirection = (LeftToRight, RightToLeft, TopToBottom, BottomToTop);

  function GetPreviousCell(const ACol, ARow: Integer; const ADirection: TDirection): TCell;
  var
    PrevCol: Integer;
    PrevRow: Integer;
  begin
    case ADirection of
      TDirection.LeftToRight:
      begin
        PrevCol := ACol - 1;
        PrevRow := ARow;
      end;
      TDirection.RightToLeft:
      begin
        PrevCol := ACol + 1;
        PrevRow := ARow;
      end;
      TDirection.TopToBottom:
      begin
        PrevCol := ACol;
        PrevRow := ARow - 1;
      end;
      TDirection.BottomToTop:
      begin
        PrevCol := ACol;
        PrevRow := ARow + 1;
      end;
    else
      PrevCol := ACol;
      PrevRow := ARow;
    end;

    Assert(InRange(PrevCol, 0, High(FTable)));
    Assert(InRange(PrevRow, 0, High(FTable[0])));

    Result := FTable[PrevCol, PrevRow];
  end;

var
  Cell: TCell;
  Row: Integer;
  Col: Integer;
  PreviousCell: TCell;
  LocationDp: TPointF;
  ZeroCol: Integer;
  ZeroRow: Integer;
  CurrentCell: TCell;
begin
  if (FColumns.Count = 0) or (FRows.Count = 0) then
  begin
    SetTableSize(0, 0);
    Exit;
  end;

  SetTableSize(FColumns.Count - 1, FRows.Count - 1);

  { Initial filling of the table }
  for Col := Low(FTable) to High(FTable) do
    for Row := Low(FTable[Col]) to High(FTable[Col]) do
    begin
      Cell.LocationPX := TPoint.Create(FColumns[Col], FRows[Row]);
      Cell.Scale := GetDisplayScaleByPX(Cell.LocationPX);
      Cell.SizePX := GetCellSizePX(Col, Row);
      Cell.SizeDP := ScaleSize(Cell.SizePX, Cell.Scale);
      Cell.IsDisplayPart := ContainsAnyDisplay(Cell.LocationPX);
      FTable[Col, Row] := Cell;
    end;

  { Calculation cell positions in DP }
  // Starting from the main display section, we calculate the positions of all cells.
  // Defining the primary screen
  if not DefineCellByPX(TPoint.Zero, ZeroCol, ZeroRow) then
  begin
    // Primary display always has (0,0) position. However, if we haven't had primary display here,
    // we select first one.
    ZeroCol := 0;
    ZeroRow := 0;
  end;

  // Center -> Right
  for Col := ZeroCol to High(FTable) do
  begin
    // Center -> Top
    LocationDp := TPointF.Zero;
    for Row := ZeroRow downto Low(FTable[Col]) do
    begin
      CurrentCell := FTable[Col, Row];
      if Col <> ZeroCol then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.LeftToRight);
        LocationDp.X := PreviousCell.LocationDp.X + PreviousCell.SizePX.Width / PreviousCell.Scale;
      end;
      if Row <> ZeroRow then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.BottomToTop);
        LocationDp.Y := PreviousCell.LocationDp.Y - CurrentCell.SizePX.Height / CurrentCell.Scale;
      end;

      SetCellLocationDP(Col, Row, LocationDP);
    end;

    // Center -> Bottom
    LocationDp := TPointF.Zero;
    for Row := ZeroRow to High(FTable[Col]) do
    begin
      if Col <> ZeroCol then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.LeftToRight);
        LocationDp.X := PreviousCell.LocationDP.X + PreviousCell.SizePX.Width / PreviousCell.Scale;
      end;
      if Row <> ZeroRow then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.TopToBottom);
        LocationDp.Y := PreviousCell.LocationDP.Y + PreviousCell.SizePX.Height / PreviousCell.Scale;
      end;

      SetCellLocationDP(Col, Row, LocationDP);
    end;
  end;

  // Center -> Left
  for Col := ZeroCol downto Low(FTable) do
  begin
    // Center -> Top
    LocationDp := TPointF.Zero;
    for Row := ZeroRow downto Low(FTable[Col]) do
    begin
      CurrentCell := FTable[Col, Row];
      if Col <> ZeroCol then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.RightToLeft);
        LocationDp.X := PreviousCell.LocationDP.X - CurrentCell.SizePX.Width / CurrentCell.Scale;
      end;
      if Row <> ZeroRow then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.BottomToTop);
        LocationDp.Y := PreviousCell.LocationDp.Y - CurrentCell.SizePX.Height / CurrentCell.Scale;
      end;

      SetCellLocationDP(Col, Row, LocationDP);
    end;

    // Center -> Bottom
    LocationDp := TPointF.Zero;
    for Row := ZeroRow to High(FTable[Col]) do
    begin
      CurrentCell := FTable[Col, Row];
      if Col <> ZeroCol then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.RightToLeft);
        LocationDp.X := PreviousCell.LocationDP.X - CurrentCell.SizePX.Width / CurrentCell.Scale;
      end;
      if Row <> ZeroRow then
      begin
        PreviousCell := GetPreviousCell(Col, Row, TDirection.TopToBottom);
        LocationDp.Y := PreviousCell.LocationDP.Y + PreviousCell.SizePX.Height / PreviousCell.Scale;
      end;

      SetCellLocationDP(Col, Row, LocationDP);
    end;
  end;
end;

class function TDpPxConverter.ScaleSize(const ASize: TSize; const AScale: Single): TSizeF;
begin
  Result := TSizeF.Create(ASize.Width / AScale, ASize.height / AScale);
end;

constructor TDpPxConverter.Create;
begin
  inherited;
  FDisplays := TList<TDisplay>.Create;
  FColumns := TList<Integer>.Create;
  FRows := TList<Integer>.Create;
end;

destructor TDpPxConverter.Destroy;
begin
  FreeAndNil(FDisplays);
  FreeAndNil(FRows);
  FreeAndNil(FColumns);
  inherited;
end;

function TDpPxConverter.PXToDP(const AValue: TPoint): TPointF;
var
  Col: Integer;
  Row: Integer;
  Cell: TCell;
  CellPixel: TPoint;
begin
  if DefineCellByPX(AValue, Col, Row) then
  begin
    Cell := FTable[Col, Row];
    CellPixel := AValue - Cell.LocationPX;
    Result := Cell.LocationDP + TPointF.Create(CellPixel.X / Cell.Scale, CellPixel.Y / Cell.Scale);
  end
  else
    Result := AValue;
end;

function TDpPxConverter.DPToPX(const AValue: TPointF): TPoint;
var
  Col: Integer;
  Row: Integer;
  Cell: TCell;
  CellPoint: TPointF;
begin
  if DefineCellByDP(AValue, Col, Row) then
  begin
    Cell := FTable[Col, Row];
    CellPoint := AValue - Cell.LocationDP;
    Result.x := Cell.LocationPX.X + Round(CellPoint.X * Cell.Scale);
    Result.y := Cell.LocationPX.Y + Round(CellPoint.Y * Cell.Scale);
  end
  else
    Result := AValue.Truncate;
end;

procedure TDpPxConverter.AddDisplay(const ADisplay: TDisplay);
var
  PhysicalBounds: TRect;
begin
  FDisplays.Add(ADisplay);

  PhysicalBounds := ADisplay.PhysicalBounds;
  AddColumn(PhysicalBounds.Left);
  AddColumn(PhysicalBounds.Right);
  AddRow(PhysicalBounds.Top);
  AddRow(PhysicalBounds.Bottom);
  RebuildTable;
end;

procedure TDpPxConverter.Clear;
begin
  FColumns.Clear;
  FRows.Clear;
  FDisplays.Clear;
  SetTableSize(FColumns.Count, FRows.Count);
end;

function TDpPxConverter.ContainsAnyDisplay(const AHitPoint: TPoint): Boolean;
var
  I: Integer;
  Display: TDisplay;
begin
  Result := False;
  for I := 0 to FDisplays.Count - 1 do
  begin
    Display := FDisplays[I];
    if Display.PhysicalBounds.Contains(AHitPoint) then
      Exit(True);
  end;
end;

function TDpPxConverter.ToString: string;
var
  Col: Integer;
  Row: Integer;
  Cell: TCell;
begin
  Result := string.Empty;
  for Col := Low(FTable) to High(FTable) do
  begin
    for Row := Low(FTable[Col]) to High(FTable[Col]) do
    begin
      Cell := FTable[Col, Row];
      Result := Result + Format('%.2f %.2f |', [Cell.LocationDP.X, Cell.LocationDP.Y]);
    end;
    Result := Result + SLineBreak;
  end;
end;

end.
