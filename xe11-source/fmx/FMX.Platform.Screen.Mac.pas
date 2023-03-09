{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Screen.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, System.Generics.Collections, Macapi.CocoaTypes, Macapi.AppKit, FMX.Types, FMX.Platform;

type
  TMacMultiDisplay = class(TInterfacedObject, IFMXMultiDisplayService, IFMXScreenService, IFMXDeviceMetricsService)
  private
    FDisplayCount: Integer;
    FWorkAreaRect: TRectF;
    FDesktopRect: TRectF;
    FDisplayList: TList<TDisplay>;
    function NSRectToRect(const ANSRect: NSRect): TRect;
    procedure UpdateDisplays;
    function FindDisplay(const screen: NSScreen): TDisplay;
  public
    destructor Destroy; override;

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
  end;

implementation

uses
  System.SysUtils, System.Math, System.RTLConsts, Macapi.Helpers, Macapi.ObjectiveC, Macapi.Foundation, FMX.Controls,
  FMX.Forms, FMX.Helpers.Mac, FMX.Platform.Mac;

{ TMacMultiDisplay }

destructor TMacMultiDisplay.Destroy;
begin
  FreeAndNil(FDisplayList);
  inherited;
end;

function TMacMultiDisplay.DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
begin
  Result := DisplayFromWindow(AHandle);
end;

function TMacMultiDisplay.DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
var
  Wnd: TMacWindowHandle;
  Form, ParentForm: TCommonCustomForm;
begin
  if AHandle = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Wnd := WindowHandleToPlatform(AHandle);
  if Wnd.Wnd <> nil then
    Form := Wnd.FindForm(Wnd.Wnd)
  else
    Form := nil;
  if (Form <> nil) and Form.IsPopupForm then
  begin
    ParentForm := Form.ParentForm;
    while (ParentForm <> nil) and ParentForm.IsPopupForm do
      ParentForm := ParentForm.ParentForm;
    if ParentForm <> nil then
      Wnd := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Wnd = nil) or (Wnd.Wnd = nil) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Wnd.Wnd.screen);
end;

function TMacMultiDisplay.FindDisplay(const screen: NSScreen): TDisplay;

  function DoFind(const R: TRect): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    if FDisplayList <> nil then
      for I := 0 to FDisplayList.Count - 1 do
        if R = FDisplayList[I].Bounds then
        begin
          Result := I;
          Exit;
        end;
  end;

var
  Index: Integer;
  R: TRect;
begin
  if screen = nil then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);
  R := NSRectToRect(screen.Frame);
  Index := DoFind(R);
  if Index = -1 then
  begin
    UpdateDisplays;
    Index := DoFind(R);
  end;
  if Index = -1 then
    raise EInvalidArgument.Create(sArgumentInvalid)
  else
    Result := FDisplayList[Index];
end;

function TMacMultiDisplay.GetDesktopCenterRect(const ASize: TSizeF): TRectF;
var
  I, MinI: Integer;
  DesktopCenter: TPointF;
  Dist, MinDist: Double;
  WorkArea: TRectF;
begin
  DesktopCenter := GetDesktopRect.CenterPoint;
  Result := TRectF.Create(TPointF.Create(DesktopCenter.X - ASize.cx / 2, DesktopCenter.Y - ASize.cy / 2),
                          ASize.cx, ASize.cy);
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
  WorkArea := GetDisplay(MinI).WorkArea;
  if Result.Top < WorkArea.Top then
    Result.SetLocation(Result.Left, WorkArea.Top);
  if Result.Bottom > WorkArea.Bottom then
    Result.SetLocation(Result.Left, WorkArea.Bottom - Result.Height);
  if Result.Left < WorkArea.Left then
    Result.SetLocation(WorkArea.Left, Result.Top);
  if Result.Right > WorkArea.Right then
    Result.SetLocation(WorkArea.Right - Result.Width, Result.Top);
end;

function TMacMultiDisplay.GetDesktopRect: TRectF;
var
  I: Integer;
begin
  if (FDesktopRect.Width <= 0) or (FDesktopRect.Height <= 0) then
  begin
    FDesktopRect := TRect.Empty;
    for I := 0 to GetDisplayCount - 1 do
      FDesktopRect.Union(GetDisplay(I).Bounds);
  end;
  Result := FDesktopRect;
end;

function TMacMultiDisplay.GetDisplay(const AIndex: Integer): TDisplay;
begin
  if AIndex < 0 then
    raise EListError.CreateFmt(SListIndexError, [AIndex]);
  if (FDisplayList = nil) or (FDisplayList.Count <> GetDisplayCount) then
    UpdateDisplays;
  if AIndex >= GetDisplayCount then
    raise EListError.CreateFmt(SListIndexError, [AIndex]);
  Result := FDisplayList[AIndex];
end;

function TMacMultiDisplay.GetDisplayCount: Integer;
begin
  if FDisplayCount = 0 then
    FDisplayCount := TNSScreen.OCClass.screens.count;
  Result := FDisplayCount;
end;

function TMacMultiDisplay.GetDisplayMetrics: TDeviceDisplayMetrics;
const
  MacBasePPI = 110;
var
  Screen: NSScreen;
  ScreenSize: TPointF;
  ScreenScale: Single;
begin
  Screen := MainScreen;
  ScreenSize := Screen.frame.size.ToSizeF;
  ScreenScale := Screen.backingScaleFactor;

  Result.PhysicalScreenSize := TSizeF.Create(ScreenSize.X * ScreenScale, ScreenSize.Y * ScreenScale).Truncate;
  Result.RawScreenSize := Result.PhysicalScreenSize;
  Result.LogicalScreenSize := TSize.Create(Trunc(ScreenSize.X), Trunc(ScreenSize.Y));
  if Abs(ScreenSize.X) > 0 then
    Result.AspectRatio := ScreenSize.Y / ScreenSize.X
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := Trunc(MacBasePPI * GetScreenScale);
  Result.ScreenScale := ScreenScale;
  Result.FontScale := ScreenScale;
end;

function TMacMultiDisplay.GetPhysicalDesktopRect: TRect;
begin
  Result := NSRectToRect(MainScreen.convertRectToBacking(MainScreen.frame)); // px
end;

function TMacMultiDisplay.GetPhysicalWorkAreaRect: TRect;
begin
  Result := NSRectToRect(MainScreen.convertRectToBacking(MainScreen.visibleFrame)); // px
end;

function TMacMultiDisplay.GetScreenOrientation: TScreenOrientation;
begin
  Result := TScreenOrientation.Landscape;
end;

function TMacMultiDisplay.GetScreenScale: Single;
begin
  Result := MainScreen.backingScaleFactor;
end;

function TMacMultiDisplay.GetScreenSize: TPointF;
begin
  Result := MainScreen.Frame.Size.ToPointF;
end;

function TMacMultiDisplay.GetWorkAreaRect: TRectF;
begin
  if (FWorkAreaRect.Width <= 0) or (FWorkAreaRect.Height <= 0) then
    FWorkAreaRect := NSRectToRect(MainScreen.visibleFrame);
  Result := FWorkAreaRect;
end;

function TMacMultiDisplay.NSRectToRect(const ANSRect: NSRect): TRect;
var
  LNSSize: NSSize;
begin
  LNSSize := MainScreen.Frame.Size;
  Result := TRect.Create(TPoint.Create(Round(ANSRect.origin.x),
    Round(LNSSize.height - ANSRect.origin.y - ANSRect.size.height)), Round(ANSRect.size.width),
    Round(ANSRect.size.height));
end;

procedure TMacMultiDisplay.SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);
begin
  // Not needed for MAC
end;

procedure TMacMultiDisplay.UpdateDisplayInformation;
begin
  FDisplayCount := 0;
  FDesktopRect := TRect.Empty;
  FWorkAreaRect := TRect.Empty;
  FreeAndNil(FDisplayList);
end;

procedure TMacMultiDisplay.UpdateDisplays;

  procedure AddInfo(const I: Integer);
  var
    LNSScreen: NSScreen;
    Display: TDisplay;
  begin
    LNSScreen := TNSScreen.Wrap(TNSScreen.OCClass.screens.objectAtIndex(I));
    Display.Id := NativeUInt(NSObjectToId(LNSScreen));
    Display.Index := I;
    Display.Primary := I = 0;
    Display.Bounds := NSRectToRect(LNSScreen.frame); // dp
    Display.Workarea := NSRectToRect(LNSScreen.visibleFrame); // dp
    Display.Scale := LNSScreen.backingScaleFactor;
    Display.PhysicalBounds := NSRectToRect(LNSScreen.convertRectToBacking(LNSScreen.frame)); // px
    Display.PhysicalWorkarea := NSRectToRect(LNSScreen.convertRectToBacking(LNSScreen.visibleFrame)); // px

    FDisplayList.Add(Display);
  end;

var
  I: Integer;
begin
  UpdateDisplayInformation;
  FDisplayCount := TNSScreen.OCClass.screens.count;
  if FDisplayList = nil then
    FDisplayList := TList<TDisplay>.Create
  else
    FDisplayList.Clear;
  for I := 0 to FDisplayCount - 1 do
    AddInfo(I);
end;

end.
