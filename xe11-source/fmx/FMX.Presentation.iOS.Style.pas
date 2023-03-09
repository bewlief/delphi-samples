{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Presentation.iOS.Style;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo, System.Types, System.Classes, System.SysUtils, Macapi.ObjectiveC, iOSapi.Foundation, iOSapi.CocoaTypes,
  iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.GLKit, Macapi.MetalKit, FMX.Types, FMX.Controls.Presentation, FMX.Controls,
  FMX.Graphics, FMX.Presentation.iOS, FMX.Platform.iOS, FMX.Forms, FMX.Presentation.Style.Common, FMX.Context.Metal;

type
  TiOSStyledPresentation = class;
  TiOSNativeScene = class;

  /// <summary>Helper class used as root for control's style</summary>
  TiOSNativeStyledControl = class(TNativeStyledControl)
  private
    function GetScene: TiOSNativeScene;
  protected
    function GetDefaultStyleLookupName: string; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure AdjustSize; override;
    property Scene: TiOSNativeScene read GetScene;
  end;

  /// <summary>Non TControl class that used as container for style to break control parenting</summary>
  TiOSNativeScene = class(TNativeScene)
  private
    FCanvas: TCanvas;
    function GetView: UIView;
    function GetHandle: TiOSWindowHandle;
    function GetPresentation: TiOSStyledPresentation;
    function GetStyledControl: TiOSNativeStyledControl;
  protected
    procedure DoAddUpdateRect(R: TRectF); override;
    function DoGetCanvas: TCanvas; override;
    function DoGetSceneScale: Single; override;
    function DoGetStyleBook: TStyleBook; override;
    function DoLocalToScreen(AScenePoint: TPointF): TPointF; override;
    function DoScreenToLocal(AScreenPoint: TPointF): TPointF; override;
    procedure DoResized(const NewSize: TSizeF); override;
    function GetPresentedControl: TControl; override;
  public
    constructor Create(APresentation: TiOSStyledPresentation); reintroduce;
    destructor Destroy; override;
    procedure Paint;
    property Presentation: TiOSStyledPresentation read GetPresentation;
    /// <summary>Link to OS window handle linked with presentation</summary>
    property Handle: TiOSWindowHandle read GetHandle;
    /// <summary>Link to platform UIView used as container for scene</summary>
    property View: UIView read GetView;
    property StyledControl: TiOSNativeStyledControl read GetStyledControl;
  end;

{ TiOSStyledPresentation }

  /// <summary>Objective-C bridge helper for opengl native-styled presentation</summary>
  IiOSGLKSceneControl = interface(GLKView)
  ['{3A907753-FF20-4EB7-A791-E30C62016759}']
    procedure drawRect(R: CGRect); cdecl;
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  /// <summary>Objective-C bridge helper for metal native-styled presentation</summary>
  IiOSMTKSceneControl = interface(MTKView)
  ['{8A835023-0C2E-4BEF-B0C9-40691CF915CD}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  /// <summary>Basic iOS native-styled presentation, which is UIView.</summary>
  TiOSStyledPresentation = class(TiOSNativeView)
  private
    FNativeScene: TiOSNativeScene;
    function GetView: UIView;
    function GetStyledControl: TiOSNativeStyledControl;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    procedure InitView; override;
    procedure SetSize(const ASize: TSizeF); override;
    /// <summary>Bridge from presentation's GetDefaultStyleLookupName to StyledControl.GetDefaultStyleLookupName</summary>
    function GetDefaultStyleLookupName: string; virtual;
    /// <summary>Bridge from presentation's GetParentClassStyleLookupName to StyledControl.GetParentClassStyleLookupName</summary>
    function GetParentClassStyleLookupName: string; virtual;
    /// <summary>Bridge from presentation's ApplyStyle to StyledControl.ApplyStyle</summary>
    procedure ApplyStyle; virtual;
    /// <summary>Bridge from presentation's FreeStyle to StyledControl.FreeStyle</summary>
    procedure FreeStyle; virtual;
    /// <summary>Bridge from presentation's DoApplyStyleLookup to StyledControl.DoApplyStyleLookup</summary>
    procedure DoApplyStyleLookup; virtual;
  public
    constructor Create; overload; override;
    destructor Destroy; override;
    procedure Dispatch(var Message); override;
    /// <summary>Overriden Objective-C method</summary>
    procedure drawRect(R: CGRect); cdecl;
    /// <summary>Link to platform UIView used as container for scene</summary>
    property View: UIView read GetView;
    /// <summary>Link to root styled control of the scene</summary>
    property StyledControl: TiOSNativeStyledControl read GetStyledControl;
  end;

implementation

uses
  Macapi.Helpers, FMX.Presentation.Factory, FMX.Helpers.iOS, FMX.Context.GLES.iOS;

type

{ TViewWindowHandle }

  TViewWindowHandle = class(TiOSWindowHandle)
  private
    [Weak] FPresentation: TiOSStyledPresentation;
  protected
    function GetView: UIView; override;
    function GetGLView: GLKView; override;
    function GetMTView: MTKView; override;
    function GetForm: TCommonCustomForm; override;
    function GetWnd: UIWindow; override;
    function GetScale: Single; override;
  public
    constructor Create(APresentation: TiOSStyledPresentation);
  end;

  TOpenStyledControl = class(TStyledControl);

{ TiOSNativeStyledControl }

procedure TiOSNativeStyledControl.AdjustSize;
begin
end;

procedure TiOSNativeStyledControl.ApplyStyle;
begin
  inherited;
  Scene.Presentation.ApplyStyle;
end;

procedure TiOSNativeStyledControl.FreeStyle;
begin
  Scene.Presentation.FreeStyle;
  inherited;
end;

function TiOSNativeStyledControl.GetDefaultStyleLookupName: string;
begin
  Result := Scene.Presentation.GetDefaultStyleLookupName;
end;

function TiOSNativeStyledControl.GetScene: TiOSNativeScene;
begin
  Result := TiOSNativeScene(inherited Scene);
end;

{ TViewWindowHandle }

constructor TViewWindowHandle.Create(APresentation: TiOSStyledPresentation);
begin
  inherited Create(nil);
  FPresentation := APresentation;
end;

function TViewWindowHandle.GetForm: TCommonCustomForm;
begin
  Result := nil;
end;

function TViewWindowHandle.GetGLView: GLKView;
begin
  Result := GLKView(FPresentation.Super);
end;

function TViewWindowHandle.GetMTView: MTKView;
begin
  Result := MTKView(FPresentation.Super);
end;

function TViewWindowHandle.GetScale: Single;
begin
  Result := MainScreen.scale;
end;

function TViewWindowHandle.GetView: UIView;
begin
  Result := UIView(FPresentation.Super);
end;

function TViewWindowHandle.GetWnd: UIWindow;
begin
  Result := FPresentation.View.window;
end;

constructor TiOSNativeScene.Create(APresentation: TiOSStyledPresentation);
begin
  inherited Create(TViewWindowHandle.Create(APresentation), APresentation, TiOSNativeStyledControl);
  FCanvas := TCanvasManager.CreateFromWindow(Handle, Round(APresentation.Size.Width),
    Round(APresentation.Size.Height));
end;

destructor TiOSNativeScene.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

function TiOSNativeScene.GetView: UIView;
begin
  Result := Presentation.View;
end;

function TiOSNativeScene.DoLocalToScreen(AScenePoint: TPointF): TPointF;
var
  Point: NSPoint;
begin
  Point := View.window.convertPoint(CGPoint.Create(AScenePoint), View);
  Result := Point.ToPointF;
end;

function TiOSNativeScene.DoScreenToLocal(AScreenPoint: TPointF): TPointF;
var
  Point: NSPoint;
begin
  Point := View.convertPoint(CGPoint.Create(AScreenPoint), View.window);
  Result := Point.ToPointF;
end;

procedure TiOSNativeScene.DoAddUpdateRect(R: TRectF);
begin
  if not (csDestroying in ComponentState) and not IsDisableUpdating then
  begin
    R := TRectF.Create(R.TopLeft.Truncate, R.BottomRight.Ceiling);
    if IntersectRect(R, TRectF.Create(0, 0, Presentation.Size.Width, Presentation.Size.Height)) then
      Presentation.View.setNeedsDisplay;
  end;
end;

function TiOSNativeScene.DoGetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TiOSNativeScene.GetHandle: TiOSWindowHandle;
begin
  Result := TiOSWindowHandle(inherited Handle);
end;

function TiOSNativeScene.GetPresentation: TiOSStyledPresentation;
begin
  Result := TiOSStyledPresentation(inherited Presentation);
end;

function TiOSNativeScene.GetPresentedControl: TControl;
begin
  Result := Presentation.Control;
end;

function TiOSNativeScene.DoGetSceneScale: Single;
begin
  Result := View.window.screen.scale;
end;

function TiOSNativeScene.DoGetStyleBook: TStyleBook;
begin
  if (Presentation.Control <> nil) and (Presentation.Control.Scene <> nil) then
    Result := Presentation.Control.Scene.StyleBook
  else
    Result := nil;
end;

function TiOSNativeScene.GetStyledControl: TiOSNativeStyledControl;
begin
  Result := TiOSNativeStyledControl(inherited StyledControl);
end;

procedure TiOSNativeScene.Paint;
begin
  if UpdateRects.Count > 0 then
  begin
    if FCanvas.BeginScene then
    try
      FCanvas.Clear(0);
      PaintControls;
    finally
      FCanvas.EndScene;
    end;
  end;
end;

procedure TiOSNativeScene.DoResized(const NewSize: TSizeF);
begin
  inherited;
  FCanvas.SetSize(Trunc(NewSize.Width), Trunc(NewSize.Height));
end;

{ TiOSStyledPresentation }

constructor TiOSStyledPresentation.Create;
begin
  inherited;
  View.setOpaque(False);
  FNativeScene := TiOSNativeScene.Create(Self);
  Control.InsertObject(0, FNativeScene);
end;

procedure TiOSStyledPresentation.InitView;
var
  V: Pointer;
begin
  if GlobalUseMetal then
    V := NSObjectToID(MTKView(Super).initWithFrame(ViewFrame, TCustomContextMetal.SharedDevice))
  else
    V := GLKView(Super).initWithFrame(ViewFrame, TCustomContextIOS.SharedContext);

  if GetObjectID <> V then
    UpdateObjectID(V);
end;

destructor TiOSStyledPresentation.Destroy;
begin
  FNativeScene.Free;
  inherited;
end;

procedure TiOSStyledPresentation.Dispatch(var Message);
begin
  if FNativeScene <> nil then
    FNativeScene.Dispatch(Message);
  inherited;
end;

procedure TiOSStyledPresentation.DoApplyStyleLookup;
begin
  FNativeScene.StyledControl.DoApplyStyleLookup;
end;

procedure TiOSStyledPresentation.ApplyStyle;
begin
  TOpenStyledControl(Control).ApplyStyle;
end;

procedure TiOSStyledPresentation.FreeStyle;
begin
  TOpenStyledControl(Control).FreeStyle;
end;

function TiOSStyledPresentation.GetDefaultStyleLookupName: string;
begin
  Result := TStyledControl(Control).DefaultStyleLookupName;
end;

function TiOSStyledPresentation.GetParentClassStyleLookupName: string;
begin
  Result := TStyledControl(Control).ParentClassStyleLookupName;
end;

procedure TiOSStyledPresentation.drawRect(R: CGRect);
begin
  if FNativeScene <> nil then
  begin
    FNativeScene.UpdateRects.Add(R.ToRectF);
    FNativeScene.Paint;
  end;
end;

function TiOSStyledPresentation.GetObjectiveCClass: PTypeInfo;
begin
  if GlobalUseMetal then
    Result := TypeInfo(IiOSMTKSceneControl)
  else
    Result := TypeInfo(IiOSGLKSceneControl);
end;

function TiOSStyledPresentation.GetStyledControl: TiOSNativeStyledControl;
begin
  Result := FNativeScene.StyledControl;
end;

function TiOSStyledPresentation.GetView: UIView;
begin
  Result := UIView(Super);
end;

procedure TiOSStyledPresentation.SetSize(const ASize: TSizeF);
begin
  inherited;
  if FNativeScene <> nil then
    FNativeScene.SetSize(Size);
end;

initialization
  TPresentationProxyFactory.Current.RegisterDefault(TControlType.Platform, TiOSPresentationProxy<TiOSStyledPresentation>);

finalization
  TPresentationProxyFactory.Current.UnregisterDefault(TControlType.Platform);

end.
