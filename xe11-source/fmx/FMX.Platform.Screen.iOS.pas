{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Screen.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.TypInfo, System.Generics.Collections, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation,
  FMX.Platform, FMX.Types;

type
  TInterfaceOrientationChangedListener = class;

  /// <summary>Implementations of screen's services: <c>IFMXMultiDisplayService</c>, <c>IFMXScreenService</c>,
  ///  <c>IFMXDeviceMetricsService</c></summary>
  TiOSScreenServices = class(TInterfacedObject, IFMXMultiDisplayService, IFMXScreenService, IFMXDeviceMetricsService)
  private
    FMainScreen: UIScreen;
    FDisplayCount: Integer;
    FWorkAreaRect: TRectF;
    FPhysicalWorkAreaRect: TRect;
    FDesktopRect: TRectF;
    FPhysicalDesktopRect: TRect;
    FDisplayList: TList<TDisplay>;
    FOrientationListener: TInterfaceOrientationChangedListener;
    function CGRectToRect(const ACGRect: NSRect): TRect;
    procedure UpdateDisplays;
    function FindDisplay(const screen: UIScreen): TDisplay;
  protected
    /// <summary>Registers screen's services in the platform</summary>
    procedure RegisterServices; virtual;
    /// <summary>Unregisters screen's services</summary>
    procedure UnregisterServices; virtual;
  public
    constructor Create;
    destructor Destroy; override;

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
  public
    /// <summary>Returns <c>UIScreen.mainScreen</c></summary>
    property MainScreen: UIScreen read FMainScreen;
  end;
  TCocoaTouchScreenServices = TiOSScreenServices;

  IFMXInterfaceOrientationChanged = interface(NSObject)
  ['{AAF2DF0E-C4BF-4057-8C5F-1B8A832BC39E}']
    procedure DeviceOrientationChanged; cdecl;
  end;

  TInterfaceOrientationChangedListener = class(TOCLocal)
  private
    [Weak] FScreenService: TiOSScreenServices;
  protected
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AService: TiOSScreenServices);
    destructor Destroy; override;
    { IFMXInterfaceOrientationChanged }
    procedure DeviceOrientationChanged; cdecl;
  end;

implementation

uses
  System.Classes, System.SysUtils, System.Math, System.RTLConsts, Macapi.Helpers, Macapi.ObjCRuntime, FMX.Platform.iOS,
  FMX.Forms, FMX.Helpers.iOS, FMX.Controls, FMX.Consts;

function ScaleRect(const ARect: TRectF; const AScale: Single): TRect;
begin
  Result.Left := Trunc(ARect.Left / AScale);
  Result.Right := Trunc(ARect.Right / AScale);
  Result.Top := Trunc(ARect.Top / AScale);
  Result.Bottom := Trunc(ARect.Bottom / AScale);
end;

{ TiOSScreenServices }

procedure TiOSScreenServices.UnregisterServices;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXMultiDisplayService);
    TPlatformServices.Current.RemovePlatformService(IFMXScreenService);
    TPlatformServices.Current.RemovePlatformService(IFMXDeviceMetricsService);
  end;
end;

procedure TiOSScreenServices.UpdateDisplayInformation;
begin
  FDisplayCount := 0;
  FWorkAreaRect := TRectF.Empty;
  FPhysicalWorkAreaRect := TRect.Empty;
  FDesktopRect := TRectF.Empty;
  FPhysicalDesktopRect := TRect.Empty;
  FreeAndNil(FDisplayList);
end;

function TiOSScreenServices.GetDisplayCount: Integer;
begin
  if FDisplayCount = 0 then
    FDisplayCount := TUIScreen.OCClass.screens.count;
  Result := FDisplayCount;
end;

function TiOSScreenServices.GetDisplayMetrics: TDeviceDisplayMetrics;
const
  IOSBasePPI = 163;
var
  ScreenSize: TPointF;
  ScreenScale: Single;
begin
  ScreenSize := GetScreenSize;
  ScreenScale := GetScreenScale;
  Result.PhysicalScreenSize := TSize.Create(Round(ScreenSize.X * ScreenScale), Round(ScreenSize.Y * ScreenScale));
  Result.RawScreenSize := Result.PhysicalScreenSize;
  Result.LogicalScreenSize := TSize.Create(Round(ScreenSize.X), Round(ScreenSize.Y));
  if Abs(ScreenSize.X) > 0 then
    Result.AspectRatio := ScreenSize.Y / ScreenSize.X
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := Round(IOSBasePPI * ScreenScale);
  Result.ScreenScale := ScreenScale;
  Result.FontScale := ScreenScale;
end;

function TiOSScreenServices.GetPhysicalDesktopRect: TRect;
var
  I: Integer;
begin
  if FPhysicalDesktopRect.IsEmpty then
  begin
    FPhysicalDesktopRect := TRect.Empty;
    for I := 0 to GetDisplayCount - 1 do
      FPhysicalDesktopRect.Union(GetDisplay(I).PhysicalBounds);
  end;
  Result := FPhysicalDesktopRect;
end;

function TiOSScreenServices.GetPhysicalWorkAreaRect: TRect;
begin
  if FPhysicalWorkAreaRect.IsEmpty then
    FPhysicalWorkAreaRect := ScaleRect(CGRectToRect(MainScreen.applicationFrame), MainScreen.nativeScale);
  Result := FPhysicalWorkAreaRect;
end;

function TiOSScreenServices.GetDesktopCenterRect(const Size: TSizeF): TRectF;
var
  DesktopCenter: TPointF;
begin
  DesktopCenter := GetWorkAreaRect.CenterPoint;
  Result := TRectF.Create(TPointF.Create(DesktopCenter.X - Size.cx / 2, DesktopCenter.Y - Size.cy / 2),
                          Size.cx, Size.cy);
end;

function TiOSScreenServices.GetDesktopRect: TRectF;
var
  I: Integer;
begin
  if FDesktopRect.IsEmpty then
  begin
    FDesktopRect := TRectF.Empty;
    for I := 0 to GetDisplayCount - 1 do
      FDesktopRect.Union(GetDisplay(I).Bounds);
  end;
  Result := FDesktopRect;
end;

function TiOSScreenServices.CGRectToRect(const ACGRect: NSRect): TRect;
var
  LSize: NSSize;
begin
  if TOSVersion.Check(8) then
    Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x), Round(ACGRect.origin.y)), Round(ACGRect.size.width),
      Round(ACGRect.size.height))
  else
    case GetScreenOrientation of
      TScreenOrientation.Portrait:
      begin
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x), Round(ACGRect.origin.y)), Round(ACGRect.size.width),
          Round(ACGRect.size.height));
      end;
      TScreenOrientation.Landscape:
      begin
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.y),
          Round(ACGRect.origin.x)), Round(ACGRect.size.height), Round(ACGRect.size.width));
      end;
      TScreenOrientation.InvertedPortrait:
      begin
        LSize := MainScreen.bounds.size;
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.x),
          Round(LSize.height - ACGRect.origin.y - ACGRect.size.height)), Round(ACGRect.size.width),
          Round(ACGRect.size.height));
      end;
      TScreenOrientation.InvertedLandscape:
      begin
        LSize := MainScreen.bounds.size;
        Result := TRect.Create(TPoint.Create(Round(ACGRect.origin.y),
          Round(LSize.width - ACGRect.origin.x - ACGRect.size.width)), Round(ACGRect.size.height),
          Round(ACGRect.size.width));
      end;
    end;
end;

procedure TiOSScreenServices.UpdateDisplays;

  procedure AddInfo(const I: Integer);
  var
    Screen: UIScreen;
    Display: TDisplay;
  begin
    Screen := TUIScreen.Wrap(TUIScreen.OCClass.screens.objectAtIndex(I));

    Display.Id := NativeUInt(NSObjectToId(Screen));
    Display.Index := I;
    Display.Primary := I = 0;
    Display.Bounds := CGRectToRect(Screen.bounds); // dp
    Display.Workarea := CGRectToRect(Screen.applicationFrame); // dp
    Display.Scale := Screen.nativeScale;
    Display.PhysicalBounds := CGRectToRect(Screen.nativeBounds); // px
    Display.PhysicalWorkarea := ScaleRect(Display.Workarea, Display.Scale); // px

    FDisplayList.Add(Display);
  end;

var
  I: Integer;
begin
  UpdateDisplayInformation;
  FDisplayCount := TUIScreen.OCClass.screens.count;
  if FDisplayList = nil then
    FDisplayList := TList<TDisplay>.Create
  else
    FDisplayList.Clear;
  for I := 0 to FDisplayCount - 1 do
    AddInfo(I);
end;

function TiOSScreenServices.FindDisplay(const screen: UIScreen): TDisplay;

  function DoFind(const R: TRect): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    if FDisplayList <> nil then
      for I := 0 to FDisplayList.Count - 1 do
        if R = FDisplayList[I].Bounds then
          Exit(I);
  end;

var
  Index: Integer;
  R: TRect;
begin
  if screen = nil then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);
  R := CGRectToRect(screen.bounds);
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

function TiOSScreenServices.DisplayFromWindow(const AHandle: TWindowHandle): TDisplay;

  function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
  begin
    Result := (AForm <> nil) and ((AForm.FormStyle = TFormStyle.Popup) or (AForm.Owner is TPopup) or (AForm is TCustomPopupForm));
  end;

var
  Wnd: TiOSWindowHandle;
  ParentForm: TCommonCustomForm;
begin
  if AHandle = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Wnd := WindowHandleToPlatform(AHandle);
  if IsPopupForm(Wnd.Form) then
  begin
    ParentForm := Wnd.Form.ParentForm;
    while IsPopupForm(ParentForm) do
      ParentForm := ParentForm.ParentForm;
    if ParentForm <> nil then
      Wnd := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Wnd = nil) or (Wnd.Wnd = nil) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Wnd.Wnd.screen);
end;

constructor TiOSScreenServices.Create;
begin
  inherited;
  FMainScreen := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen);
  FOrientationListener := TInterfaceOrientationChangedListener.Create(Self);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(FOrientationListener.GetObjectID,
    sel_getUid('DeviceOrientationChanged'), StringToID('UIDeviceOrientationDidChangeNotification'), nil);
  RegisterServices;
end;

destructor TiOSScreenServices.Destroy;
begin
  UnregisterServices;
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(FOrientationListener.GetObjectID);
  FOrientationListener.Free;
  FMainScreen := nil;
  inherited;
end;

function TiOSScreenServices.DisplayFromPoint(const AHandle: TWindowHandle; const APoint: TPoint): TDisplay;
begin
  Result := DisplayFromWindow(AHandle);
end;

function TiOSScreenServices.GetDisplay(const AIndex: Integer): TDisplay;
begin
  if AIndex < 0 then
    raise EListError.CreateFMT(SListIndexError, [AIndex]);
  if (FDisplayList = nil) or (FDisplayList.Count <> GetDisplayCount) then
    UpdateDisplays;
  if AIndex >= GetDisplayCount then
    raise EListError.CreateFMT(SListIndexError, [AIndex]);
  Result := FDisplayList[AIndex];
end;

function TiOSScreenServices.GetWorkAreaRect: TRectF;
begin
  if FWorkAreaRect.IsEmpty then
    FWorkAreaRect := CGRectToRect(MainScreen.applicationFrame);
  Result := FWorkAreaRect;
end;

procedure TiOSScreenServices.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXMultiDisplayService) then
    TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXScreenService) then
    TPlatformServices.Current.AddPlatformService(IFMXScreenService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDeviceMetricsService) then
    TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, Self);
end;

function TiOSScreenServices.GetScreenSize: TPointF;
var
  ScreenSize: TPointF;
begin
  ScreenSize := MainScreen.bounds.size.ToPointF;
  if not TOSVersion.Check(8) and (GetScreenOrientation in [TScreenOrientation.Landscape, TScreenOrientation.InvertedLandscape]) then
    Result := TPointF.Create(ScreenSize.Y, ScreenSize.X)
  else
    Result := ScreenSize;
end;

procedure TiOSScreenServices.SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);

  function FindDifferentDeviceOrientation(const ADeviceOrieunbation: UIDeviceOrientation): UIDeviceOrientation;
  begin
    case ADeviceOrieunbation of
      UIDeviceOrientationPortrait:
        Result := UIDeviceOrientationLandscapeLeft;
      UIDeviceOrientationPortraitUpsideDown:
        Result := UIDeviceOrientationLandscapeLeft;
      UIDeviceOrientationLandscapeLeft:
        Result := UIDeviceOrientationPortrait;
      UIDeviceOrientationLandscapeRight:
        Result := UIDeviceOrientationPortrait;
      UIDeviceOrientationFaceUp:
        Result := UIDeviceOrientationLandscapeLeft;
      UIDeviceOrientationFaceDown:
        Result := UIDeviceOrientationLandscapeLeft;
    else
      Result := ADeviceOrieunbation;
    end;
  end;

  procedure ForceApplyingSupportedOrientation;
  var
    Device: UIDevice;
    CurrentOrientation: UIDeviceOrientation;
    IntermediateOrientation: UIDeviceOrientation;
  begin
    // Supported interface orientations are being returned in TFMXViewController.supportedInterfaceOrientations
    // However the Apple doesn't provide way to tell iOS, that supportedInterfaceOrientations value was changed.
    // There is only one known way that was found in the community, it's a changing "orientation" param value.
    // So we are changing current device orientation param value on another and returning back.
    Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
    CurrentOrientation := Device.orientation;
    IntermediateOrientation := FindDifferentDeviceOrientation(CurrentOrientation);
    Device.setValueForKey(TNSNumber.OCClass.numberWithInt(IntermediateOrientation), StrToNSStr('orientation'));
    Device.setValueForKey(TNSNumber.OCClass.numberWithInt(CurrentOrientation), StrToNSStr('orientation'));
    TUIViewController.OCClass.attemptRotationToDeviceOrientation;
  end;

begin
  if Application.FormFactor.Orientations <> AOrientations then
  begin
    Application.FormFactor.Orientations := AOrientations;
    ForceApplyingSupportedOrientation;
  end;
end;

function TiOSScreenServices.GetScreenScale: Single;
begin
  if FMainScreen = nil then
    Result := 1.0
  else
    Result := FMainScreen.nativeScale
end;

function TiOSScreenServices.GetScreenOrientation: TScreenOrientation;
var
  InterfaceOrientation: UIInterfaceOrientation;
begin
  InterfaceOrientation := SharedApplication.keyWindow.rootViewController.interfaceOrientation;
  case InterfaceOrientation of
    UIInterfaceOrientationLandscapeLeft:
      Result := TScreenOrientation.Landscape;
    UIInterfaceOrientationLandscapeRight:
      Result := TScreenOrientation.InvertedLandscape;
    UIInterfaceOrientationPortrait:
      Result := TScreenOrientation.Portrait;
    UIInterfaceOrientationPortraitUpsideDown:
      Result := TScreenOrientation.InvertedPortrait;
  else
    Result := TScreenOrientation.Portrait
  end;
end;

{ TInterfaceOrientationChangedListener }

constructor TInterfaceOrientationChangedListener.Create(const AService: TiOSScreenServices);
begin
  inherited Create;
  FScreenService := AService;
end;

destructor TInterfaceOrientationChangedListener.Destroy;
begin
  FScreenService := nil;
  inherited;
end;

procedure TInterfaceOrientationChangedListener.DeviceOrientationChanged;
begin
  FScreenService.UpdateDisplayInformation;
end;

function TInterfaceOrientationChangedListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXInterfaceOrientationChanged);
end;

end.
