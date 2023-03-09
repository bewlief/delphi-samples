{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Screen.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.TypInfo, System.Generics.Collections, Androidapi.JNIBridge, Androidapi.JNI.Util,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Hardware, Androidapi.JNI.App, AndroidApi.JNI.Embarcadero,
  FMX.Platform, FMX.Types, FMX.Forms;

type
  TAndroidDisplayListener = class;

  /// <summary>Implementations of screen's services: <c>IFMXMultiDisplayService</c>, <c>IFMXScreenService</c>,
  ///  <c>IFMXDeviceMetricsService</c></summary>
  TAndroidScreenServices = class(TInterfacedObject, IFMXMultiDisplayService, IFMXScreenService,
    IFMXDeviceMetricsService, IFMXFullScreenWindowService)
  private type
    TParameter = (DesktopRect, Displays);
    TParameters = set of TParameter;

    TFullScreenStateChangedListener = class(TJavaLocal, JOnFullScreenStateChangedListener)
    private
      FScreenServices: TAndroidScreenServices;
    public
      constructor Create(const AScreenServices: TAndroidScreenServices);
      { JOnFullScreenStateChangedListener }
      procedure stateChanged(oldState: Integer; newState: Integer); cdecl;
    end;

  private
    FDisplayManager: JDisplayManager;
    FDisplayListener: TAndroidDisplayListener;
    FDisplays: TList<TDisplay>;
    FIsPrimaryDisplayDefined: Boolean;
    FPrimaryDisplay: TDisplay;
    FDesktopRect: TRectF;
    FPhysicalDesktopRect: TRect;
    FOutdatedParameters: TParameters;
    { Full Screen }
    FFullScreenStateChangedListener: TFullScreenStateChangedListener;
    FIsFullScreen: Boolean;
    function TryFillDisplayInfo(const ANativeDisplay: JDisplay; var ADisplay: TDisplay): Boolean;
    function FindDisplay(const Activity: JActivity): TDisplay;
  protected
    /// <summary>Registers screen's services in the platform</summary>
    procedure RegisterServices; virtual;
    /// <summary>Unregisters screen's services</summary>
    procedure UnregisterServices; virtual;
    { Primary display }
    procedure SetNewPrimaryDisplay(const ADisplay: TDisplay);
    procedure ResetPrimaryDisplay;
    { Display modifications }
    procedure AddDisplay(const AId: Integer);
    procedure UpdateDisplay(const AId: Integer);
    procedure RemoveDisplay(const AId: Integer);
    function FindDisplayIndex(const AId: Integer): Integer;
    procedure UpdateDisplays;
    procedure UpdateDisplaysIfNeeded;
    procedure UpdateDesktopRect;
  public
    constructor Create;
    destructor Destroy; override;
    class function ScaleByMetrics(const AMetrics: JDisplayMetrics): Single; static;
    class function OrientationByDisplay(const ANativeDisplay: JDisplay): TScreenOrientation; static;
    class function ScaleForDisplay(const ANativeDisplay: JDisplay): Single; static;

    { IFMXMultiDisplayService }
    function GetDisplayCount: Integer;
    function GetDesktopCenterRect(const Size: TSizeF): TRectF;
    function GetWorkAreaRect: TRectF;
    function GetPhysicalWorkAreaRect: TRect;
    function GetDesktopRect: TRectF;
    function GetPhysicalDesktopRect: TRect;
    function GetDisplay(const AIndex: Integer): TDisplay;
    function DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
    function DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay; inline;
    procedure UpdateDisplayInformation;

    { IFMXScreenService }
    function GetScreenSize: TPointF;
    function GetScreenScale: Single;
    function GetScreenOrientation: TScreenOrientation;
    procedure SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);

    { IFMXDeviceMetricsService }
    function GetDisplayMetrics: TDeviceDisplayMetrics;

    { IFMXFullScreenWindowService }
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
  public
    property DisplayManager: JDisplayManager read FDisplayManager;
    property IsPrimaryDisplayDefined: Boolean read FIsPrimaryDisplayDefined;
    property PrimaryDisplay: TDisplay read FPrimaryDisplay;
  end;

  TAndroidDisplayListener = class(TJavaLocal, JDisplayManager_DisplayListener)
  private
    [Weak] FService: TAndroidScreenServices;
  public
    constructor Create(const AScreenService: TAndroidScreenServices);

    { JDisplayManager_DisplayListener }
    procedure onDisplayAdded(displayId: Integer); cdecl;
    procedure onDisplayChanged(displayId: Integer); cdecl;
    procedure onDisplayRemoved(displayId: Integer); cdecl;
  end;

  TScreenScaleOverrideHook = procedure(const UserContext: Pointer; const DensityScale, DensityDPI: Single;
    var ScreenScale: Single);

procedure SetScreenScaleOverrideHook(const UserContext: Pointer; const Hook: TScreenScaleOverrideHook);
procedure UnsetScreenScaleOverrideHook;

implementation

uses
  System.Math, System.Math.Vectors, System.SysUtils, System.RTLConsts, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  Androidapi.JNI, FMX.Platform.Android, FMX.Consts;

var
  ScreenScaleOverrideHook: TScreenScaleOverrideHook;
  ScreenScaleOverrideHookContext: Pointer;

procedure SetScreenScaleOverrideHook(const UserContext: Pointer; const Hook: TScreenScaleOverrideHook);
begin
  ScreenScaleOverrideHook := Hook;
  ScreenScaleOverrideHookContext := UserContext;
end;

procedure UnsetScreenScaleOverrideHook;
begin
  ScreenScaleOverrideHook := nil;
  ScreenScaleOverrideHookContext := nil;
end;

{ TAndroidScreenServices }

constructor TAndroidScreenServices.Create;
var
  DisplayService: JObject;
begin
  inherited;
  DisplayService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.DISPLAY_SERVICE);
  if DisplayService = nil then
    raise EUnsupportedPlatformService.CreateFMT(SUnsupportedPlatformService, ['IFMXMultiDisplayService']);
  FDisplayManager := TJDisplayManager.Wrap(DisplayService);
  FDisplayListener := TAndroidDisplayListener.Create(Self);
  FDisplayManager.registerDisplayListener(FDisplayListener, nil);
  FDisplays := TList<TDisplay>.Create;
  FIsFullScreen := not MainActivity.getFullScreenManager.getSystemUIVisibility;
  FFullScreenStateChangedListener := TFullScreenStateChangedListener.Create(Self);
  MainActivity.getFullScreenManager.setStateCallback(FFullScreenStateChangedListener);
  FOutdatedParameters := [Low(TParameter)..High(TParameter)];

  RegisterServices;
  _AddRef;
end;

destructor TAndroidScreenServices.Destroy;
begin
  FDisplayManager.unregisterDisplayListener(FDisplayListener);
  FreeAndNil(FDisplayListener);
  FreeAndNil(FDisplays);
  MainActivity.getFullScreenManager.setStateCallback(nil);
  FreeAndNil(FFullScreenStateChangedListener);
  UnregisterServices;
  inherited;
end;

function TAndroidScreenServices.DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
begin
  UpdateDisplaysIfNeeded;

  Result := DisplayFromWindow(AHandle);
end;

function TAndroidScreenServices.DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;
begin
  UpdateDisplaysIfNeeded;

  Result := FindDisplay(TAndroidHelper.Activity);
end;

function TAndroidScreenServices.FindDisplay(const Activity: JActivity): TDisplay;
var
  Index, DisplayId: Integer;
begin
  if (Activity = nil) or (Activity.getWindowManager = nil) or (Activity.getWindowManager.getDefaultDisplay = nil) then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);

  DisplayId := Activity.getWindowManager.getDefaultDisplay.getDisplayId;
  Index := FindDisplayIndex(DisplayId);
  if Index = -1 then
    raise EInvalidArgument.Create(sArgumentInvalid)
  else
    Result := FDisplays[Index];
end;

function TAndroidScreenServices.FindDisplayIndex(const AId: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to FDisplays.Count - 1 do
    if FDisplays[I].Id = NativeUInt(AId) then
      Exit(I);
  Result := -1;
end;

function TAndroidScreenServices.GetDesktopCenterRect(const Size: TSizeF): TRectF;
var
  DesktopCenter: TPointF;
begin
  UpdateDisplaysIfNeeded;

  DesktopCenter := GetWorkAreaRect.CenterPoint;
  Result := TRectF.Create(TPointF.Create(DesktopCenter.X - Size.cx / 2, DesktopCenter.Y - Size.cy / 2),
                         Size.cx, Size.cy);
end;

function TAndroidScreenServices.GetDesktopRect: TRectF;
begin
  UpdateDisplaysIfNeeded;

  if TParameter.DesktopRect in FOutdatedParameters then
    UpdateDesktopRect;
  Result := FDesktopRect;
end;

function TAndroidScreenServices.GetDisplay(const AIndex: Integer): TDisplay;
begin
  UpdateDisplaysIfNeeded;

  Result := FDisplays[AIndex];
end;

function TAndroidScreenServices.GetDisplayCount: Integer;
begin
  UpdateDisplaysIfNeeded;

  Result := FDisplays.Count;
end;

function TAndroidScreenServices.TryFillDisplayInfo(const ANativeDisplay: JDisplay; var ADisplay: TDisplay): Boolean;

  function ScaleRect(const ARect: TRect; const AScale: Single): TRect;
  begin
    Result.Left := Trunc(ARect.Left / AScale);
    Result.Right := Trunc(ARect.Right / AScale);
    Result.Top := Trunc(ARect.Top / AScale);
    Result.Bottom := Trunc(ARect.Bottom / AScale);
  end;

  function CalculatePhysicalWorkarea(const ANativeDisplay: JDisplay; const APhysicalBounds: TRect): TRect;
  var
    PW, PH: JPoint;
    W, H: Integer;
  begin
    PW := TJPoint.Create;
    PH := TJPoint.Create;
    ANativeDisplay.getCurrentSizeRange(PW, PH);
    if OrientationByDisplay(ANativeDisplay) in [TScreenOrientation.Portrait, TScreenOrientation.InvertedPortrait] then
    begin
      W := PW.x;
      H := PH.y;
    end
    else
    begin
      W := PH.x;
      H := PW.y;
    end;
    Result := TRect.Create(TPoint.Create(APhysicalBounds.Right - W, APhysicalBounds.Bottom - H), W, H);
  end;

var
  Scale: Single;
  R: JRect;
begin
  if ANativeDisplay = nil then
    raise EArgumentNilException.Create(SArgumentNil);

  Result := False;
  Scale := ScaleForDisplay(ANativeDisplay);
  if Scale > 0 then
  begin
    R := TJRect.Create;
    ANativeDisplay.getRectSize(R);
    if (R.width > 0) and (R.height > 0) then
    begin
      ADisplay.Id := ANativeDisplay.getDisplayId;
      ADisplay.Scale := Scale;
      ADisplay.PhysicalBounds := TRect.Create(R.left, R.top, R.right, R.bottom); // px
      ADisplay.Bounds := ScaleRect(ADisplay.PhysicalBounds, Scale); // dp
      ADisplay.PhysicalWorkarea := CalculatePhysicalWorkarea(ANativeDisplay, ADisplay.PhysicalBounds); // px
      ADisplay.Workarea := ScaleRect(ADisplay.PhysicalWorkarea, Scale); // dp
      ADisplay.Primary := ANativeDisplay.getDisplayId = TJDisplay.JavaClass.DEFAULT_DISPLAY;
      Result := True;
    end;
  end;
end;

function TAndroidScreenServices.GetDisplayMetrics: TDeviceDisplayMetrics;
var
  Metrics: JDisplayMetrics;
  RawScreenSize: JPoint;
  DensityDPI: Single;
begin
  Metrics := TAndroidHelper.DisplayMetrics;
  if Metrics <> nil then
  begin
    Result.PhysicalScreenSize := TSize.Create(Metrics.widthPixels, Metrics.heightPixels);
    DensityDPI := Round((Metrics.xdpi + Metrics.ydpi) / 2);
    if DensityDPI <> 0 then
    begin
      Result.LogicalScreenSize.cx := Trunc(Metrics.widthPixels / DensityDPI);
      Result.LogicalScreenSize.cy := Trunc(Metrics.heightPixels / DensityDPI);
    end
    else
      Result.LogicalScreenSize := Result.PhysicalScreenSize;
    if Metrics.widthPixels <> 0 then
      Result.AspectRatio := Metrics.heightPixels / Metrics.widthPixels
    else
      Result.AspectRatio := 1;
    Result.PixelsPerInch := Round(DensityDPI);
    Result.ScreenScale := Metrics.density;
    Result.FontScale := Metrics.scaledDensity;
  end
  else
    Result := TDeviceDisplayMetrics.Default;
  RawScreenSize := MainActivity.getRawDisplaySize;
  if RawScreenSize <> nil then
    if (Result.PhysicalScreenSize.cx > Result.PhysicalScreenSize.cy) and (RawScreenSize.x > RawScreenSize.y) then
      Result.RawScreenSize := TSize.Create(RawScreenSize.x, RawScreenSize.y)
    else
      Result.RawScreenSize := TSize.Create(RawScreenSize.y, RawScreenSize.x)
  else
    Result.RawScreenSize := Result.PhysicalScreenSize;
end;

function TAndroidScreenServices.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
begin
  Result := FIsFullScreen;
end;

function TAndroidScreenServices.GetPhysicalDesktopRect: TRect;
begin
  UpdateDisplaysIfNeeded;

  if TParameter.DesktopRect in FOutdatedParameters then
    UpdateDesktopRect;
  Result := FPhysicalDesktopRect;
end;

function TAndroidScreenServices.GetPhysicalWorkAreaRect: TRect;
begin
  UpdateDisplaysIfNeeded;

  if FIsPrimaryDisplayDefined then
    Result := PrimaryDisplay.PhysicalWorkarea
  else
    Result := TRect.Empty;
end;

function TAndroidScreenServices.GetScreenOrientation: TScreenOrientation;
begin
  Result := OrientationByDisplay(TAndroidHelper.Display);
end;

function TAndroidScreenServices.GetScreenScale: Single;
begin
  UpdateDisplaysIfNeeded;

  if IsPrimaryDisplayDefined then
    Result := PrimaryDisplay.Scale
  else
    Result := 1;
end;

function TAndroidScreenServices.GetScreenSize: TPointF;
begin
  UpdateDisplaysIfNeeded;

  if IsPrimaryDisplayDefined then
    Result := PrimaryDisplay.Bounds.Size
  else
    Result := TPointF.Zero;
end;

function TAndroidScreenServices.GetWorkAreaRect: TRectF;
begin
  UpdateDisplaysIfNeeded;

  if FIsPrimaryDisplayDefined then
    Result := PrimaryDisplay.Workarea
  else
    Result := TRect.Empty;
end;

class function TAndroidScreenServices.OrientationByDisplay(const ANativeDisplay: JDisplay): TScreenOrientation;

  function IsLandscapeDevice(const Rotation: Integer): Boolean;
  var
    Resources: JResources;
    Configuration: JConfiguration;
    Straight: Boolean;
  begin
    Straight := (Rotation = TJSurface.JavaClass.ROTATION_0) or (Rotation = TJSurface.JavaClass.ROTATION_180);
    if TAndroidHelper.Context <> nil then
    begin
      Resources := TAndroidHelper.Context.getResources;
      if Resources <> nil then
      begin
        Configuration := Resources.getConfiguration;
        if Configuration <> nil then
          Exit(((Configuration.orientation = TJConfiguration.JavaClass.ORIENTATION_LANDSCAPE) and Straight) or
            ((Configuration.orientation = TJConfiguration.JavaClass.ORIENTATION_PORTRAIT) and not Straight));
      end;
    end;
    Result := ((Screen.Width > Screen.Height) and Straight) or ((Screen.Width < Screen.Height) and not Straight);
  end;

var
  Rotation: Integer;
begin
  if ANativeDisplay <> nil then
  begin
    Rotation := ANativeDisplay.getRotation;
    if IsLandscapeDevice(Rotation) then
    begin // landscape device
      if Rotation = TJSurface.JavaClass.ROTATION_180 then
        Result := TScreenOrientation.InvertedLandscape
      else if Rotation = TJSurface.JavaClass.ROTATION_90 then
        Result := TScreenOrientation.InvertedPortrait
      else if Rotation = TJSurface.JavaClass.ROTATION_270 then
        Result := TScreenOrientation.Portrait
      else
        Result := TScreenOrientation.Landscape;
    end
    else
    begin // portrait device
      if Rotation = TJSurface.JavaClass.ROTATION_180 then
        Result := TScreenOrientation.InvertedPortrait
      else if Rotation = TJSurface.JavaClass.ROTATION_90 then
        Result := TScreenOrientation.Landscape
      else if Rotation = TJSurface.JavaClass.ROTATION_270 then
        Result := TScreenOrientation.InvertedLandscape
      else
        Result := TScreenOrientation.Portrait;
    end;
  end
  else
    Result := TScreenOrientation.Portrait;
end;

procedure TAndroidScreenServices.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXMultiDisplayService) then
    TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXScreenService) then
    TPlatformServices.Current.AddPlatformService(IFMXScreenService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDeviceMetricsService) then
    TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXFullScreenWindowService) then
    TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, Self);
end;

procedure TAndroidScreenServices.RemoveDisplay(const AId: Integer);
var
  Display: TDisplay;
  DisplayIndex: Integer;
begin
  DisplayIndex := FindDisplayIndex(AId);

  if DisplayIndex = -1 then
    Exit;

  Display := FDisplays.ExtractAt(DisplayIndex);
  if Display.Primary then
    ResetPrimaryDisplay;
  Include(FOutdatedParameters, TParameter.DesktopRect);
end;

procedure TAndroidScreenServices.ResetPrimaryDisplay;
begin
  FIsPrimaryDisplayDefined := False;
end;

class function TAndroidScreenServices.ScaleByMetrics(const AMetrics: JDisplayMetrics): Single;
const
  // Default values taken from Android SDK reference:
  // http://developer.android.com/reference/android/util/DisplayMetrics.html#density
  DefaultDensityScale = 1;
  DefaultDensityDPI = 160;
var
  DensityScale, DensityDPI: Single;
begin
  if AMetrics <> nil then
  begin
    DensityScale := AMetrics.density; // API level 1
    DensityDPI := AMetrics.densityDpi; // API level 4
  end
  else
  begin
    DensityScale := DefaultDensityScale;
    DensityDPI := DefaultDensityDPI;
  end;

  // Some devices (for example: ZTE Nubia Z9 MAX) can return 0.01 value.
  if DensityScale <= 0.01 then
    if AMetrics.widthPixels > AMetrics.heightPixels then
      DensityScale := RoundTo(AMetrics.widthPixels / Screen.Width, -2)
    else
      DensityScale := RoundTo(AMetrics.heightPixels / Screen.Height, -2);
  if DensityScale <= 0.01 then
    DensityScale := 1;

  Result := DensityScale;

  if Assigned(ScreenScaleOverrideHook) then
  begin
    ScreenScaleOverrideHook(ScreenScaleOverrideHookContext, DensityScale, DensityDPI, Result);
    Result := EnsureRange(Result, 1, 3);
  end;
end;

class function TAndroidScreenServices.ScaleForDisplay(const ANativeDisplay: JDisplay): Single;
var
  Metrics: JDisplayMetrics;
begin
  if ANativeDisplay = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Metrics := TJDisplayMetrics.Create;
  ANativeDisplay.getMetrics(Metrics);
  Result := ScaleByMetrics(Metrics);
end;

procedure TAndroidScreenServices.SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
  MainActivity.getFullScreenManager.setSystemUIVisibility(not AValue);
end;

procedure TAndroidScreenServices.SetNewPrimaryDisplay(const ADisplay: TDisplay);
begin
  FPrimaryDisplay := ADisplay;
  FIsPrimaryDisplayDefined := True;
end;

procedure TAndroidScreenServices.SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);
const
  AllScreenOrientations: TScreenOrientations = [TScreenOrientation.Portrait, TScreenOrientation.Landscape,
    TScreenOrientation.InvertedPortrait, TScreenOrientation.InvertedLandscape];
var
  RequestedOrientation: Integer;
begin
  if AOrientations = AllScreenOrientations then
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_USER
  // Only one orientation is specified
  else if AOrientations = [TScreenOrientation.Portrait]  then
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_PORTRAIT
  else if AOrientations = [TScreenOrientation.InvertedPortrait]  then
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_REVERSE_PORTRAIT
  else if AOrientations = [TScreenOrientation.Landscape]  then
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_LANDSCAPE
  else if AOrientations = [TScreenOrientation.InvertedLandscape]  then
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_REVERSE_LANDSCAPE
  // Only both portrait or only both landscape orientations
  else if AOrientations = [TScreenOrientation.Portrait, TScreenOrientation.InvertedPortrait] then
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_SENSOR_PORTRAIT
  else if AOrientations = [TScreenOrientation.Landscape, TScreenOrientation.InvertedLandscape] then
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_SENSOR_LANDSCAPE
  // The Android doesn't allow a way for blocking only one orientation, so we allow all orientations.
  else
    RequestedOrientation := TJActivityInfo.JavaClass.SCREEN_ORIENTATION_USER;

  TAndroidHelper.Activity.setRequestedOrientation(RequestedOrientation);
end;

procedure TAndroidScreenServices.SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
end;

procedure TAndroidScreenServices.UnregisterServices;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXMultiDisplayService);
    TPlatformServices.Current.RemovePlatformService(IFMXScreenService);
    TPlatformServices.Current.RemovePlatformService(IFMXDeviceMetricsService);
    TPlatformServices.Current.RemovePlatformService(IFMXFullScreenWindowService);
  end;
end;

procedure TAndroidScreenServices.AddDisplay(const AId: Integer);
var
  NativeDisplay: JDisplay;
  Display: TDisplay;
begin
  NativeDisplay := DisplayManager.getDisplay(AId);
  if NativeDisplay = nil then
    Exit;

  if TryFillDisplayInfo(NativeDisplay, Display) then
  begin
    Display.Index := FDisplays.Count;
    FDisplays.Add(Display);
    if Display.Primary then
      SetNewPrimaryDisplay(Display);
    Include(FOutdatedParameters, TParameter.DesktopRect);
  end;
end;

procedure TAndroidScreenServices.UpdateDesktopRect;
var
  I: Integer;
  Display: TDisplay;
begin
  FDesktopRect := TRectF.Empty;
  FPhysicalDesktopRect := TRect.Empty;
  for I := 0 to FDisplays.Count - 1 do
  begin
    Display := FDisplays[I];
    FDesktopRect.Union(Display.Bounds);
    FPhysicalDesktopRect.Union(Display.PhysicalBounds);
  end;
  Exclude(FOutdatedParameters, TParameter.DesktopRect);
end;

procedure TAndroidScreenServices.UpdateDisplay(const AId: Integer);
var
  NativeDisplay: JDisplay;
  Display: TDisplay;
  DisplayIndex: Integer;
begin
  DisplayIndex := FindDisplayIndex(AId);
  if DisplayIndex = -1 then
    Exit;

  NativeDisplay := DisplayManager.getDisplay(AId);
  if TryFillDisplayInfo(NativeDisplay, Display) then
  begin
    Display.Index := DisplayIndex;
    FDisplays[DisplayIndex] := Display;
    if Display.Primary then
      SetNewPrimaryDisplay(Display);
    Include(FOutdatedParameters, TParameter.DesktopRect);
  end;
end;

procedure TAndroidScreenServices.UpdateDisplayInformation;
begin
  FOutdatedParameters := [Low(TParameter)..High(TParameter)];
end;

procedure TAndroidScreenServices.UpdateDisplays;

  procedure MarkFirstDisplayAsPrimary;
  var
    TmpDisplay: TDisplay;
  begin
    TmpDisplay := FDisplays.First;
    TmpDisplay.Primary := True;
    FDisplays[0] := TmpDisplay;
  end;

var
  I: Integer;
  NativeDisplay: JDisplay;
  Display: TDisplay;
  DisplayIndex: Integer;
begin
  FDisplays.Clear;
  ResetPrimaryDisplay;
  DisplayIndex := 0;
  for I := 0 to DisplayManager.getDisplays.Length - 1 do
  begin
    NativeDisplay := DisplayManager.getDisplays[I];
    // We use only displays with the correct information
    if TryFillDisplayInfo(NativeDisplay, Display) then
    begin
      Display.Index := DisplayIndex;
      FDisplays.Add(Display);
      if Display.Primary then
        SetNewPrimaryDisplay(Display);
      Inc(DisplayIndex);
    end;
  end;

  // If there is no primary display use the first on the list
  if (FDisplays.Count > 0) and not IsPrimaryDisplayDefined then
    MarkFirstDisplayAsPrimary;

  Exclude(FOutdatedParameters, TParameter.Displays);
  Include(FOutdatedParameters, TParameter.DesktopRect);
end;

procedure TAndroidScreenServices.UpdateDisplaysIfNeeded;
begin
  if TParameter.Displays in FOutdatedParameters then
    UpdateDisplays;
end;

{ TAndroidScreenServices.TFullScreenStateChangedListener }

constructor TAndroidScreenServices.TFullScreenStateChangedListener.Create(
  const AScreenServices: TAndroidScreenServices);
begin
  inherited Create;
  FScreenServices := AScreenServices;
end;

procedure TAndroidScreenServices.TFullScreenStateChangedListener.stateChanged(oldState, newState: Integer);
begin
  FScreenServices.FIsFullScreen := (newState <> TJFullScreenManager.JavaClass.STATE_NAV) and
                                   (newState <> TJFullScreenManager.JavaClass.STATE_STAT_NAV);
end;

{ TAndroidDisplayListener }

constructor TAndroidDisplayListener.Create(const AScreenService: TAndroidScreenServices);
begin
  inherited Create;
  FService := AScreenService;
end;

procedure TAndroidDisplayListener.onDisplayAdded(displayId: Integer);
begin
  FService.AddDisplay(displayId);
end;

procedure TAndroidDisplayListener.onDisplayChanged(displayId: Integer);
begin
  FService.UpdateDisplay(displayId);
end;

procedure TAndroidDisplayListener.onDisplayRemoved(displayId: Integer);
begin
  FService.RemoveDisplay(displayId);
end;

end.
