{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.BufferedLayout;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Math, System.SysUtils, System.Generics.Collections, System.Types, System.UITypes,
  System.Messaging, FMX.Types, FMX.Controls, FMX.Graphics, FMX.Platform, FMX.Layouts;

type

  TCustomBufferedLayout = class;

  TBufferedScene = class(TFMXObject, IScene, IAlignRoot, IContent)
  private class var
    FScreenService: IFMXScreenService;
    class destructor Destroy;
  private
    [Weak] FScene: TCustomBufferedLayout;
    FBuffer: TBitmap;
    FControls: TControlList;
    FWidth: Integer;
    FHeight: Integer;
    FUpdateRects: array of TRectF;
    FLastWidth: Single;
    FLastHeight: Single;
    FDisableAlign: Boolean;
    { IScene }
    procedure AddUpdateRect(const R: TRectF);
    function GetUpdateRectsCount: Integer;
    function GetUpdateRect(const Index: Integer): TRectF;
    function GetObject: TFmxObject;
    function GetCanvas: TCanvas;
    function GetSceneScale: Single;
    function LocalToScreen(const P: TPointF): TPointF;
    function ScreenToLocal(const P: TPointF): TPointF;
    procedure ChangeScrollingState(const AControl: TControl; const Active: Boolean);
    procedure DisableUpdating;
    procedure EnableUpdating;
    function GetStyleBook: TStyleBook;
    procedure SetStyleBook(const Value: TStyleBook);
    { IAlignRoot }
    procedure Realign;
    procedure ChildrenAlignChanged;
    { IContent }
    function GetParent: TFmxObject;
    function GetChildrenCount: Integer;
    procedure Changed;
    procedure Invalidate;
    procedure UpdateBuffer;
  protected
    procedure ScaleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage); virtual;
    procedure DrawTo;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    function ObjectAtPoint(P: TPointF): IControl;
  public
    constructor Create(const AScene: TCustomBufferedLayout); reintroduce;
    destructor Destroy; override;
    procedure SetSize(const AWidth, AHeight: Integer);
    property Buffer: TBitmap read FBuffer;
    property Scene: TCustomBufferedLayout read FScene;
  end;

  TCustomBufferedLayout = class(TLayout)
  private
    FScene: TBufferedScene;
  protected
    procedure Paint; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoResized; override;
    function ObjectAtPoint(P: TPointF): IControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TBufferedLayout = class(TCustomBufferedLayout)
  published
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property HitTest;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    { Events }
    property OnApplyStyleLookup;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

implementation

uses
  FMX.Forms, FMX.Text;

type
  TBufferedSceneHelper = class
  private
    class var FInstance: TBufferedSceneHelper;
    class function GetInstance: TBufferedSceneHelper; static;
  private
    FScenes: TList<TBufferedScene>;
    procedure StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
    procedure ProcessStyleChanges(const AScene: TBufferedScene; const AMessage: TStyleChangedMessage);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Register(const ABufferedScene: TBufferedScene);
    procedure Unregister(const ABufferedScene: TBufferedScene);
  public
    class procedure DestroyCurrent;
    class property Instance: TBufferedSceneHelper read GetInstance;
  end;

{ TBufferedScene }

constructor TBufferedScene.Create(const AScene: TCustomBufferedLayout);
begin
  inherited Create(nil);
  if FScreenService = nil then
    TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, FScreenService);
  FScene := AScene;
  FWidth := Round(AScene.Width);
  FHeight := Round(AScene.Height);
  FBuffer := TBitmap.Create;
  UpdateBuffer;
  FControls := TControlList.Create;
  FControls.Capacity := 10;
  TMessageManager.DefaultManager.SubscribeToMessage(TScaleChangedMessage, ScaleChangedHandler);
  TBufferedSceneHelper.Instance.Register(Self);
end;

destructor TBufferedScene.Destroy;
begin
  TBufferedSceneHelper.Instance.Unregister(Self);
  TMessageManager.DefaultManager.Unsubscribe(TScaleChangedMessage, ScaleChangedHandler);
  DeleteChildren;
  FreeAndNil(FControls);
  FBuffer.Free;
  inherited;
end;

procedure TBufferedScene.AddUpdateRect(const R: TRectF);
var
  AbsoluteRect: TRectF;
begin
  if csDestroying in ComponentState then
    Exit;

  SetLength(FUpdateRects, Length(FUpdateRects) + 1);
  FUpdateRects[High(FUpdateRects)] := R;

  AbsoluteRect := FScene.LocalToAbsolute(R);
  FScene.RepaintRect(AbsoluteRect);
end;

procedure TBufferedScene.ChangeScrollingState(const AControl: TControl; const Active: Boolean);
begin
end;

procedure TBufferedScene.ChildrenAlignChanged;
begin
end;

class destructor TBufferedScene.Destroy;
begin
  FScreenService := nil;
end;

procedure TBufferedScene.DisableUpdating;
begin
end;

procedure TBufferedScene.DoAddObject(const AObject: TFmxObject);
var
  ChildControl: TControl;
begin
  inherited;
  if AObject is TControl then
  begin
    ChildControl := TControl(AObject);
    ChildControl.SetNewScene(Self);
    ChildControl.RecalcOpacity;
    ChildControl.RecalcAbsolute;
    ChildControl.RecalcUpdateRect;
    ChildControl.RecalcHasClipParent;
    ChildControl.RecalcEnabled;

    FControls.Add(ChildControl);

    if ChildControl.Align = TAlignLayout.None then
      ChildControl.Repaint
    else
      Realign;
  end;
end;

procedure TBufferedScene.DoRemoveObject(const AObject: TFmxObject);
var
  ChildControl: TControl;
begin
  inherited;
  if AObject is TControl then
  begin
    ChildControl := TControl(AObject);
    FControls.Remove(ChildControl);
    ChildControl.SetNewScene(nil);
  end;
end;

procedure TBufferedScene.Invalidate;
begin
  AddUpdateRect(TRectF.Create(0, 0, FWidth, FHeight));
end;

type
  TOpenControl = class(TControl);

procedure TBufferedScene.DrawTo;

  function NeedPaintControl(const AControl: TControl): Boolean;
  var
    DrawRect: TRectF;
    I: Integer;
  begin
    DrawRect := UnionRect(AControl.ChildrenRect, AControl.UpdateRect);
    for I := Low(FUpdateRects) to High(FUpdateRects) do
      if IntersectRect(FUpdateRects[I], DrawRect) then
        Exit(True);
    Result := False;
  end;

var
  I: Integer;
  AllowPaint: Boolean;
  Control: TControl;
begin
  if Length(FUpdateRects) = 0 then
    Exit;

  if FBuffer.Canvas.BeginScene(@FUpdateRects) then
  try
    FBuffer.Canvas.Clear(TAlphaColorRec.Null);

    for I := 0 to FControls.Count - 1 do
    begin
      Control := FControls[I];
      if Control.Visible or Control.ShouldTestMouseHits then
      begin
        if Control.UpdateRect.IsEmpty then
          Continue;
        AllowPaint := Control.InPaintTo;
        if not AllowPaint then
          AllowPaint := NeedPaintControl(Control);
        if AllowPaint then
          TOpenControl(Control).PaintInternal;
      end;
    end;
  finally
    FBuffer.Canvas.EndScene;
  end;
  SetLength(FUpdateRects, 0);
end;

procedure TBufferedScene.EnableUpdating;
begin
end;

function TBufferedScene.GetCanvas: TCanvas;
begin
  Result := FBuffer.Canvas;
end;

function TBufferedScene.GetObject: TFmxObject;
begin
  Result := Self;
end;

function TBufferedScene.GetSceneScale: Single;
begin
  Result := FBuffer.BitmapScale;
end;

function TBufferedScene.GetStyleBook: TStyleBook;
begin
  if FScene.Scene = nil then
    Result := nil
  else
    Result := FScene.Scene.StyleBook;
end;

function TBufferedScene.GetUpdateRect(const Index: Integer): TRectF;
begin
  Result := FUpdateRects[Index];
end;

function TBufferedScene.GetUpdateRectsCount: Integer;
begin
  Result := Length(FUpdateRects);
end;

function TBufferedScene.LocalToScreen(const P: TPointF): TPointF;
begin
  Result := FScene.LocalToScreen(P);
end;

function TBufferedScene.ObjectAtPoint(P: TPointF): IControl;
var
  I: Integer;
  Control: TControl;
  NewObj: IControl;
begin
  if FControls.Count = 0 then
    Exit(nil);

  for I := FControls.Count - 1 downto 0 do
  begin
    Control := FControls[I];
    if not Control.Visible then
      Continue;

    NewObj := IControl(Control).ObjectAtPoint(P);
    if NewObj <> nil then
      Exit(NewObj);
  end;
end;

procedure TBufferedScene.ScaleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  UpdateBuffer;
end;

function TBufferedScene.ScreenToLocal(const P: TPointF): TPointF;
begin
  Result := FScene.ScreenToLocal(P);
end;

procedure TBufferedScene.Realign;
var
  Padding: TBounds;
begin
  Padding := TBounds.Create(TRectF.Empty);
  try
    AlignObjects(Self, Padding, FWidth, FHeight, FLastWidth, FLastHeight, FDisableAlign);
  finally
    Padding.Free;
  end;
end;

procedure TBufferedScene.UpdateBuffer;
var
  Scale: Single;
begin
  if FScene.Scene = nil then
    Scale := FScreenService.GetScreenScale
  else
    Scale := FScene.Scene.GetSceneScale;

  FBuffer.BitmapScale := Scale;
  FBuffer.SetSize(Ceil(FWidth * Scale), Ceil(FHeight * Scale));
  Invalidate;
end;

procedure TBufferedScene.SetSize(const AWidth, AHeight: Integer);
begin
  if (FWidth <> AWidth) or (FHeight <> AHeight) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    UpdateBuffer;
    Realign;
  end;
end;

procedure TBufferedScene.SetStyleBook(const Value: TStyleBook);
begin
end;

function TBufferedScene.GetChildrenCount: Integer;
begin
  if Children = nil then
    Result := 0
  else
    Result := Children.Count;
end;

procedure TBufferedScene.Changed;
begin
end;

function TBufferedScene.GetParent: TFmxObject;
begin
  Result := FScene;
end;

{ TCustomBufferedLayout }

constructor TCustomBufferedLayout.Create(AOwner: TComponent);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FScene := TBufferedScene.Create(Self);
    FScene.Parent := Self;
    FScene.Stored := False;
  end;
end;

destructor TCustomBufferedLayout.Destroy;
begin
  if FScene <> nil then
    FScene.DisposeOf;
  inherited;
end;

procedure TCustomBufferedLayout.DoAddObject(const AObject: TFmxObject);
begin
  if (FScene <> nil) and (AObject <> FScene) then
    FScene.AddObject(AObject)
  else
    inherited;
end;

procedure TCustomBufferedLayout.DoResized;
begin
  inherited;
  if FScene <> nil then
    FScene.SetSize(Round(Width), Round(Height));
end;

function TCustomBufferedLayout.ObjectAtPoint(P: TPointF): IControl;
begin
  Result := nil;
  if FScene <> nil then
    Result := FScene.ObjectAtPoint(P);
  if Result = nil then
    Result := inherited ObjectAtPoint(P);
end;

procedure TCustomBufferedLayout.Paint;
begin
  if FScene <> nil then
  begin
    FScene.DrawTo;
    Canvas.DrawBitmap(FScene.Buffer, FScene.Buffer.BoundsF, LocalRect, AbsoluteOpacity, True);
  end;

  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{ TBufferedSceneHelper }

constructor TBufferedSceneHelper.Create;
begin
  FScenes := TList<TBufferedScene>.Create;
  TMessageManager.DefaultManager.SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

destructor TBufferedSceneHelper.Destroy;
begin
  FreeAndNil(FScenes);
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, StyleChangedHandler);
  inherited;
end;

class procedure TBufferedSceneHelper.DestroyCurrent;
begin
  FreeAndNil(FInstance);
end;

class function TBufferedSceneHelper.GetInstance: TBufferedSceneHelper;
begin
  if FInstance = nil then
    FInstance := TBufferedSceneHelper.Create;

  Result := FInstance;
end;

procedure TBufferedSceneHelper.ProcessStyleChanges(const AScene: TBufferedScene; const AMessage: TStyleChangedMessage);

  function IsOurScene(const ANewScene: IScene): Boolean;
  begin
    Result := (ANewScene <> nil) and (ANewScene.GetObject = AScene.FScene.Scene.GetObject);
  end;

  function IsStyleOverriden(const ANewStyleBook: TStyleBook): Boolean;
  begin
    Result := AScene.GetStyleBook <> ANewStyleBook;
  end;

begin
  if not IsOurScene(AMessage.Scene) or IsStyleOverriden(AMessage.Value) then
    Exit;

  try
    TMessageManager.DefaultManager.SendMessage(nil, TStyleChangedMessage.Create(AScene.GetStyleBook, AScene), True);
  except
    Application.HandleException(Self);
  end;
  AScene.UpdateBuffer;
end;

procedure TBufferedSceneHelper.StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
var
  Message: TStyleChangedMessage;
  Scene: TBufferedScene;
begin
  Message := TStyleChangedMessage(Msg);
  for Scene in FScenes do
    ProcessStyleChanges(Scene, Message);
end;

procedure TBufferedSceneHelper.Register(const ABufferedScene: TBufferedScene);
begin
  FScenes.Add(ABufferedScene);
end;

procedure TBufferedSceneHelper.Unregister(const ABufferedScene: TBufferedScene);
begin
  FScenes.Remove(ABufferedScene);
end;

initialization
  RegisterFmxClasses([TBufferedLayout]);
finalization
  TBufferedSceneHelper.DestroyCurrent;
end.
