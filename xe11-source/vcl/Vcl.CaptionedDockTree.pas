{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.CaptionedDockTree;

{$HPPEMIT LEGACYHPP}

interface

uses 
  Winapi.Windows, Vcl.Controls, Vcl.Graphics, Winapi.Messages, Vcl.Themes;

type
  /// <summary>
  /// TParentFormState: stores information about the parent dock form for
  /// use in drawing the dock site
  /// </summary>
  TParentFormState = record
    Caption: string;
    StartColor, EndColor, FontColor: TColor;
    Focused: Boolean;
    Icon: TIcon;
  end;

  TDockCaptionOrientation = (dcoHorizontal, dcoVertical);

  /// <summary>
  /// Hit tests for the caption. Note: The custom values allow you
  /// to return your own hit test results for your own drawer.
  /// </summary>
  TDockCaptionHitTest = Cardinal;
  /// <summary>
  /// The pin button style to draw, if any.
  /// </summary>
  TDockCaptionPinButton = (dcpbNone, dcpbUp, dcpbDown);

  TDockCaptionDrawer = class(TObject)
  private
    FDockCaptionOrientation: TDockCaptionOrientation;
    FDockCaptionPinButton: TDockCaptionPinButton;
    function GetCloseRect(const CaptionRect: TRect): TRect;
    function GetPinRect(const CaptionRect: TRect): TRect;
    function CalcButtonSize(const CaptionRect: TRect): Integer;
  protected
    property DockCaptionOrientation: TDockCaptionOrientation read FDockCaptionOrientation;
  public
    function GetGrabberSize(ADockSite: TWinControl): Integer; virtual;
    procedure DrawDockCaption(const Canvas: TCanvas;
      CaptionRect: TRect; State: TParentFormState); virtual;
    function DockCaptionHitTest(const CaptionRect: TRect;
      const MousePos: TPoint): TDockCaptionHitTest; virtual;
    /// <summary>
    /// Creates an instance of the TDockCaptionDrawer. It is virtual so the
    /// call to TCaptionedDockTree.GetDockCaptionDrawer.Create(..) will
    /// be called on the correct type.
    /// </summary>
    constructor Create(DockCaptionOrientation: TDockCaptionOrientation); virtual;
    property DockCaptionPinButton: TDockCaptionPinButton read FDockCaptionPinButton write FDockCaptionPinButton;
  end;

  TDockCaptionDrawerClass = class of TDockCaptionDrawer;

  TCaptionedDockTree = class(TDockTree)
  private
    FGrabberSize: Integer;
    FDockCaptionOrientation: TDockCaptionOrientation;
    FDockCaptionDrawer: TDockCaptionDrawer;
  protected
    procedure InvalidateDockSite(const Client: TControl);
    function AdjustCaptionRect(const ARect: TRect): TRect; virtual;
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); override;
    function InternalCaptionHitTest(const Zone: TDockZone;
      const MousePos: TPoint): TDockCaptionHitTest;
    procedure PaintDockFrame(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); override;
    function ZoneCaptionHitTest(const Zone: TDockZone;
      const MousePos: TPoint; var HTFlag: Integer): Boolean; override;
    property DockCaptionOrientation: TDockCaptionOrientation read FDockCaptionOrientation;
    property DockCaptionDrawer: TDockCaptionDrawer read FDockCaptionDrawer;
    procedure WndProc(var Message: TMessage); override;
    function GetGrabberSize: Integer; virtual;
  public
    constructor Create(DockSite: TWinControl); overload; override;
    constructor Create(DockSite: TWinControl;
      ADockCaptionOrientation: TDockCaptionOrientation); reintroduce; overload;
    destructor Destroy; override;
    class function GetParentFormState(const Control: TControl): TParentFormState; virtual;
    class function GetDockCaptionDrawer: TDockCaptionDrawerClass; virtual;
  end;

  TCaptionedDockTreeClass = class of TCaptionedDockTree;
  
const
  /// <summary>
  /// TDockCaptionHitTest constant values used. You can use your own values,
  /// but start at the dchtCustom value. Items 4-9 are reserved for future
  /// VCL use, and the value of dchtCustom may change.
  /// </summary>
  dchtNone = 0;
  dchtCaption = 1;
  dchtClose = 2;
  dchtPin = 3;
  dchtCustom = 10;
  cDockCaptionDefaultGrabberSize = 23;

implementation

uses
{$IF DEFINED(CLR)}
  System.Runtime.InteropServices,
{$ENDIF}
  System.Types, System.UITypes, Vcl.Forms, Vcl.GraphUtil;

function GetThemeColor(Style: TCustomStyleServices; Details: TThemedElementDetails; ElementColor: TElementColor; var Color: TColor): Boolean; inline;
begin
  Result := Style.Enabled and
    Style.GetElementColor(Details, ElementColor, Color) and (Color <> clNone);
end;

{ TCaptionedDockTree }

procedure TCaptionedDockTree.AdjustDockRect(Control: TControl;
  var ARect: TRect);
begin
  if FDockCaptionOrientation = dcoHorizontal then
    Inc(ARect.Top, GetGrabberSize)
  else
    Inc(ARect.Left, GetGrabberSize)
end;

constructor TCaptionedDockTree.Create(DockSite: TWinControl);
begin
  inherited;
  FGrabberSize := cDockCaptionDefaultGrabberSize;
  FDockCaptionDrawer := GetDockCaptionDrawer.Create(FDockCaptionOrientation);
end;

constructor TCaptionedDockTree.Create(DockSite: TWinControl;
  ADockCaptionOrientation: TDockCaptionOrientation);
begin
  FDockCaptionOrientation := ADockCaptionOrientation;
  Create(DockSite);
end;

class function TCaptionedDockTree.GetParentFormState(const Control: TControl): TParentFormState;
begin
  if Control is TCustomForm then
  begin
    Result.Caption := TCustomForm(Control).Caption;
    Result.Focused := (Screen.ActiveControl <> nil) and
      Screen.ActiveControl.Focused and
      (TWinControl(Control).ContainsControl(Screen.ActiveControl));
    if Control is TForm then
      Result.Icon := TForm(Control).Icon
    else
      Result.Icon := nil;
  end
  else
  begin
    Result.Caption := '';
    Result.Focused := False;
    Result.Icon := nil;
  end;
  if Result.Focused then
  begin
    Result.StartColor := clActiveBorder;
    Result.EndColor := GetHighlightColor(clActiveBorder, 22);
    Result.FontColor := clCaptionText;
  end
  else
  begin
    Result.StartColor := GetShadowColor(clBtnFace, -25);
    Result.EndColor := GetHighlightColor(clBtnFace, 15);
    Result.FontColor := clBtnText;
  end;
end;

procedure TCaptionedDockTree.InvalidateDockSite(const Client: TControl);
var
  ParentForm: TCustomForm;
  Rect: TRect;
begin
  ParentForm := GetParentForm(Client, False);
  { Just invalidate the parent form's rect in the HostDockSite
    so that we can "follow focus" on docked items. }
  if (ParentForm <> nil) and (ParentForm.HostDockSite <> nil) then
  begin
    with ParentForm.HostDockSite do
      if UseDockManager and (DockManager <> nil) then
      begin
        DockManager.GetControlBounds(ParentForm, Rect);
        InvalidateRect(Handle, Rect, False);
      end;
  end;
end;

function TCaptionedDockTree.AdjustCaptionRect(const ARect: TRect): TRect;
begin
  Result := ARect;
  if FDockCaptionOrientation = dcoHorizontal then
  begin
    Result.Left := Result.Left + 1;
    Result.Bottom := Result.Top + GetGrabberSize - 1;
    Result.Right := Result.Right - 2; { Shrink the rect a little }
  end
  else
  begin
    Result.Right := Result.Left + GetGrabberSize - 1;
    Result.Left := Result.Left + 1;
    Result.Bottom := Result.Bottom - 3;
  end;
end;

procedure TCaptionedDockTree.PaintDockFrame(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  Canvas.Font := Screen.CaptionFont;
  Canvas.Font.Height := MulDiv(Canvas.Font.Height, Control.CurrentPPI, Screen.PixelsPerInch);
  FDockCaptionDrawer.DrawDockCaption(Canvas,
    AdjustCaptionRect(ARect),
    GetParentFormState(Control));
end;

procedure TCaptionedDockTree.WndProc(var Message: TMessage);
{$IF DEFINED(CLR)}
var
  LGCHandle: GCHandle;
{$ENDIF}
begin
  if Message.Msg = CM_DOCKNOTIFICATION then
  begin
{$IF DEFINED(CLR)}
    with TCMDockNotification.Create(Message) do
    begin
      if NotifyRec.ClientMsg = CM_INVALIDATEDOCKHOST then
      begin
        if NotifyRec.MsgWParam <> 0 then
        begin
          LGCHandle := GChandle(IntPtr(NotifyRec.MsgWParam));
          if LGChandle.IsAllocated then
            InvalidateDockSite(TControl(LGCHandle.Target))
        end;
      end
      else
        inherited;
    end;
{$ELSE}
    with TCMDockNotification(Message) do
    begin
      if NotifyRec.ClientMsg = CM_INVALIDATEDOCKHOST then
        InvalidateDockSite(TControl(NotifyRec.MsgWParam))
      else
        inherited;
    end;
{$ENDIF}
  end
  else
    inherited;
end;

function TCaptionedDockTree.InternalCaptionHitTest(const Zone: TDockZone;
  const MousePos: TPoint): TDockCaptionHitTest;
var
  FrameRect, CaptionRect: TRect;
begin
  FrameRect := Zone.ChildControl.BoundsRect;
  AdjustDockRect(Zone.ChildControl, FrameRect);
  AdjustFrameRect(Zone.ChildControl, FrameRect);
  CaptionRect := AdjustCaptionRect(FrameRect);
  Result := FDockCaptionDrawer.DockCaptionHitTest(CaptionRect, MousePos);
end;

function TCaptionedDockTree.ZoneCaptionHitTest(const Zone: TDockZone;
  const MousePos: TPoint; var HTFlag: Integer): Boolean;
var
  HitTest: TDockCaptionHitTest;
begin
  HitTest := InternalCaptionHitTest(Zone, MousePos);
  if HitTest = dchtNone then
    Result := False
  else
  begin
    Result := True;
    if HitTest = dchtClose then
      HTFlag := HTCLOSE
    else if HitTest = dchtCaption then
      HTFlag := HTCAPTION;
  end;
end;

destructor TCaptionedDockTree.Destroy;
begin
  FDockCaptionDrawer.Free;
  inherited;
end;

class function TCaptionedDockTree.GetDockCaptionDrawer: TDockCaptionDrawerClass;
begin
  Result := TDockCaptionDrawer;
end;

function TCaptionedDockTree.GetGrabberSize: Integer;
begin
  Result := FGrabberSize;
  if DockSite <> nil then
    Result := DockSite.ScaleValue(Result);
end;

{ TDockCaptionDrawer }

function TDockCaptionDrawer.CalcButtonSize(
  const CaptionRect: TRect): Integer;
const
  cButtonBuffer = 8;
begin
  if FDockCaptionOrientation = dcoHorizontal then
    Result := CaptionRect.Bottom - CaptionRect.Top - cButtonBuffer
  else
    Result := CaptionRect.Right - CaptionRect.Left - cButtonBuffer;
end;

constructor TDockCaptionDrawer.Create(
  DockCaptionOrientation: TDockCaptionOrientation);
begin
  inherited Create;
  FDockCaptionOrientation := DockCaptionOrientation;
  //FDockCaptionPinButton := dcpbUp; // For testing
end;

function TDockCaptionDrawer.DockCaptionHitTest(const CaptionRect: TRect;
  const MousePos: TPoint): TDockCaptionHitTest;
var
  CloseRect, PinRect: TRect;
begin
  if CaptionRect.Contains(MousePos) then
  begin
    CloseRect := GetCloseRect(CaptionRect);
    { Make the rect vertically the same size as the captionrect }
    if FDockCaptionOrientation = dcoHorizontal then
    begin
      CloseRect.Top := CaptionRect.Top;
      CloseRect.Bottom := CaptionRect.Bottom;
      Inc(CloseRect.Right);
    end
    else
    begin
      CloseRect.Left := CaptionRect.Left;
      CloseRect.Right := CaptionRect.Right;
      Inc(CloseRect.Bottom);
    end;
    if CloseRect.Contains(MousePos) then
      Result := dchtClose
    else if FDockCaptionPinButton <> dcpbNone then
    begin
      { did it hit the pin? }
      if FDockCaptionOrientation = dcoHorizontal then
      begin
        PinRect := GetPinRect(CaptionRect);
        PinRect.Top := CaptionRect.Top;
        PinRect.Bottom := CaptionRect.Bottom;
        Inc(PinRect.Right);
      end
      else
      begin
        PinRect := GetPinRect(CaptionRect);
        PinRect.Left := CaptionRect.Left;
        PinRect.Right := CaptionRect.Right;
        Inc(PinRect.Bottom);
      end;

      if PinRect.Contains(MousePos) then
        Result := dchtPin
      else
        Result := dchtCaption;
    end
    else
      Result := dchtCaption
  end
  else
    Result := dchtNone;
end;

procedure TDockCaptionDrawer.DrawDockCaption(const Canvas: TCanvas;
  CaptionRect: TRect; State: TParentFormState);
const
  cDefaultCaptionSize = 21;
  cDefaultCloseIconSize = 8;
  cDefaultPinIconSize = 5;

var
  LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;

  function GetScaleFactor: Double;
  begin
    if FDockCaptionOrientation = dcoHorizontal then
      Result := CaptionRect.Height / cDefaultCaptionSize
    else
      Result := CaptionRect.Width / cDefaultCaptionSize;
    if Result < 1 then
      Result := 1;
  end;

  function ScaleV(const AValue: Integer): Integer;
  begin
    Result := Round(GetScaleFactor * AValue);
  end;

  procedure PaintCloseX(ARect: TRect);
  var
    X, Y, I: Integer;
    LSize: Integer;
    LScaleFactor: Double;
  begin
    if not GetThemeColor(LStyle, LDetails, ecEdgeFillColor, LColor) then
      LColor := GetShadowColor(clBtnFace, -120);
    LScaleFactor := GetScaleFactor;
    Canvas.Pen.Color := LColor;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 1;
    LSize := Round(cDefaultCloseIconSize * LScaleFactor);
    X := ARect.Left + (ARect.Width - LSize - Trunc(LScaleFactor)) div 2;
    Y := ARect.Top + (ARect.Height - LSize) div 2;
    for I := 1 to Round(LScaleFactor * 2) do
    begin
      Canvas.MoveTo(X, Y);
      Canvas.LineTo(X + LSize + 1 , Y + LSize + 1);
      Canvas.MoveTo(X + LSize, Y);
      Canvas.LineTo(X - 1, Y + LSize + 1);
      Inc(X);
   end;
end;


  procedure PaintPin(ARect: TRect);
  var
    R, R1, R2: TRect;
  begin
    Canvas.Pen.Color := Canvas.Font.Color;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := Trunc(GetScaleFactor);

    if not GetThemeColor(LStyle, LDetails, ecEdgeFillColor, LColor) then
      LColor := GetShadowColor(clBtnFace, -120);
    Canvas.Pen.Color := LColor;

    if FDockCaptionPinButton = dcpbDown then
    begin
      R := Rect(0, 0, ScaleV(cDefaultPinIconSize), ScaleV(cDefaultPinIconSize) * 2);
      R.Offset(ARect.CenterPoint.X - R.Width div 2, ARect.CenterPoint.Y - R.Height div 2 + 1);
      R1 := R;
      R1.Bottom := R.Top + R.Height div 2 + 1;
      Canvas.Rectangle(R1);
      R2 := R1;
      R2.Left := R2.Right - Canvas.Pen.Width - 1;
      Canvas.Rectangle(R2);
      Canvas.MoveTo(R1.Left - Canvas.Pen.Width - Canvas.Pen.Width div 2, R1.Bottom - 1);
      Canvas.LineTo(R1.Right + Canvas.Pen.Width, R1.Bottom - 1);
      Canvas.MoveTo(R1.Left + R1.Width div 2, R1.Bottom - 1);
      Canvas.LineTo(R1.Left + R1.Width div 2, R.Bottom);
    end
    else
    begin
      R := Rect(0, 0, ScaleV(cDefaultPinIconSize) * 2,  ScaleV(cDefaultPinIconSize));
      R.Offset(ARect.CenterPoint.X - R.Width div 2, ARect.CenterPoint.Y - R.Height div 2 + 1);
      R1 := R;
      R1.Left := R.Right - R.Width div 2 - 1;
      Canvas.Rectangle(R1);
      R2 := R1;
      R2.Top := R2.Bottom - Canvas.Pen.Width - 1;
      Canvas.Rectangle(R2);
      Canvas.MoveTo(R1.Left, R1.Top - Canvas.Pen.Width - Canvas.Pen.Width div 2);
      Canvas.LineTo(R1.Left, R1.Bottom + Canvas.Pen.Width);
      Canvas.MoveTo(R.Left - Canvas.Pen.Width, R1.Top + R1.Height div 2);
      Canvas.LineTo(R1.Left, R1.Top + R1.Height div 2);
    end;
    Canvas.Pen.Width := 1;
  end;

  procedure PaintOutline(const ARect: TRect);
  begin
    if not GetThemeColor(LStyle, LDetails, ecEdgeDkShadowColor, LColor) then
      LColor := GetShadowColor(clBtnFace, -75);
    Canvas.Pen.Color := LColor;

    if not GetThemeColor(LStyle, LDetails, ecFillColor, LColor) then
      LColor := clBtnFace;
    Canvas.Brush.Color := LColor;
    Canvas.FillRect(Rect(ARect.Left + 1, ARect.Top + 1,
      ARect.Right - 1, ARect.Bottom - 1));

    // Top left pixel
    Canvas.Pixels[ARect.Left + 1, ARect.Top + 1] := Canvas.Pen.Color;
    // top line
    Canvas.MoveTo(ARect.Left + 2, ARect.Top + 0);
    Canvas.LineTo(ARect.Right - 2, ARect.Top + 0);
    // top right pixel
    Canvas.Pixels[ARect.Right - 2, ARect.Top + 1] := Canvas.Pen.Color;
    // right line
    Canvas.MoveTo(ARect.Right - 1, ARect.Top + 2);
    Canvas.LineTo(ARect.Right - 1, ARect.Bottom - 2);
    // bottom right pixel
    Canvas.Pixels[ARect.Right - 2, ARect.Bottom - 2] := Canvas.Pen.Color;
    // bottom line
    Canvas.MoveTo(ARect.Left + 2, ARect.Bottom - 1);
    Canvas.LineTo(ARect.Right - 2, ARect.Bottom - 1);
    // bottom left pixel
    Canvas.Pixels[ARect.Left + 1, ARect.Bottom - 2] := Canvas.Pen.Color;
    // left line
    Canvas.MoveTo(ARect.Left + 0, ARect.Top + 2);
    Canvas.LineTo(ARect.Left + 0, ARect.Bottom - 2);

    // Lighter 3d'ish look
    // Right side
    if not GetThemeColor(LStyle, LDetails, ecEdgeShadowColor, LColor) then
      LColor := GetShadowColor(clBtnFace, -25);
    Canvas.Pen.Color := LColor;
    Canvas.MoveTo(ARect.Right - 4, ARect.Top + 1);
    Canvas.LineTo(ARect.Right - 3, ARect.Top + 1);
    Canvas.LineTo(ARect.Right - 3, ARect.Top + 2);
    Canvas.LineTo(ARect.Right - 2, ARect.Top + 2);
    // vert line
    Canvas.LineTo(ARect.Right - 2, ARect.Bottom - 3);
    Canvas.LineTo(ARect.Right - 3, ARect.Bottom - 3);
    Canvas.LineTo(ARect.Right - 3, ARect.Bottom - 2);
    // bottom line
    Canvas.LineTo(ARect.Left + 1, ARect.Bottom - 2);
    // last bottom left side pixel
    Canvas.Pixels[ARect.Left + 1, ARect.Bottom - 3] := Canvas.Pen.Color;
  end;

  procedure DrawCloseButton(const ARect: TRect);
  var
    OutlineRect: TRect;
  begin
    OutlineRect.Left := ARect.Left - 1;
    OutlineRect.Top := ARect.Top - 1;
    OutlineRect.Right := ARect.Right + 2;
    OutlineRect.Bottom := ARect.Bottom + 2;
    LDetails := LStyle.GetElementDetails(tpDockPanelCloseNormal);
    PaintOutline(OutlineRect);
    PaintCloseX(ARect);
  end;

  procedure DrawPinButton(const ARect: TRect);
  var
    OutlineRect: TRect;
  begin
    OutlineRect.Left := ARect.Left - 1;
    OutlineRect.Top := ARect.Top - 1;
    OutlineRect.Right := ARect.Right;
    OutlineRect.Bottom := ARect.Bottom + 2;
    LDetails := LStyle.GetElementDetails(tpDockPanelPinNormal);
    PaintOutline(OutlineRect);
    PaintPin(ARect);
  end;

  function RectWidth(const Rect: TRect): Integer;
  begin
    Result := Rect.Right - Rect.Left;
  end;

  procedure DrawIcon;
  var
    FormBitmap: TBitmap;
    DestBitmap: TBitmap;
    ImageSize: Integer;
    X, Y: Integer;
  begin
    if (State.Icon <> nil) and (State.Icon.HandleAllocated) then
    begin
      if FDockCaptionOrientation = dcoHorizontal then
      begin
        ImageSize := CaptionRect.Bottom - CaptionRect.Top - 3;
        X := CaptionRect.Left;
        Y := CaptionRect.Top + 2;
      end
      else
      begin
        ImageSize := CaptionRect.Right - CaptionRect.Left - 3;
        X := CaptionRect.Left + 1;
        Y := CaptionRect.Top;
      end;

      FormBitmap := nil;
      DestBitmap := TBitmap.Create;
      try
        FormBitmap := TBitmap.Create;
        DestBitmap.Width :=  ImageSize;
        DestBitmap.Height := ImageSize;
        DestBitmap.Canvas.Brush.Color := clFuchsia;
        DestBitmap.Canvas.FillRect(Rect(0, 0, DestBitmap.Width, DestBitmap.Height));
        FormBitmap.Width := State.Icon.Width;
        FormBitmap.Height := State.Icon.Height;
        FormBitmap.Canvas.Draw(0, 0, State.Icon);
        ScaleImage(FormBitmap, DestBitmap, DestBitmap.Width / FormBitmap.Width);

        DestBitmap.TransparentColor := DestBitmap.Canvas.Pixels[0, DestBitmap.Height - 1];
        DestBitmap.Transparent := True;

        Canvas.Draw(X, Y, DestBitmap);
      finally
        FormBitmap.Free;
        DestBitmap.Free;
      end;

      if FDockCaptionOrientation = dcoHorizontal then
        CaptionRect.Left := CaptionRect.Left + 6 + ImageSize
      else
        CaptionRect.Top := CaptionRect.Top + 6 + ImageSize;
    end;
  end;

const
  CHorzStates: array[Boolean] of TThemedPanel = (tpDockPanelHorzNormal, tpDockPanelHorzSelected);
  CVertStates: array[Boolean] of TThemedPanel = (tpDockPanelVertNormal, tpDockPanelVertSelected);
var
  ShouldDrawClose: Boolean;
  CloseRect, PinRect: TRect;
begin
  LStyle := StyleServices;
  LDetails := LStyle.GetElementDetails(CHorzStates[State.Focused]);

  if not GetThemeColor(LStyle, LDetails, ecTextColor, LColor) then
    LColor := State.FontColor;
  Canvas.Font.Color := LColor;
  Canvas.Pen.Width := 1;
  if not GetThemeColor(LStyle, LDetails, ecBorderColor, LColor) then
    LColor := State.StartColor;
  Canvas.Pen.Color := LColor;

  if FDockCaptionOrientation = dcoHorizontal then
  begin
    CaptionRect.Top := CaptionRect.Top + 1;

    { Fill the middle }

    if not GetThemeColor(LStyle, LDetails, ecFillColor, LColor) then
      if State.Focused then
        LColor := clActiveCaption
      else
        LColor := clBtnFace;
    Canvas.Brush.Color := LColor;

    Canvas.FillRect(Rect(CaptionRect.Left + 1, CaptionRect.Top + 1,
      CaptionRect.Right, CaptionRect.Bottom));

    { Draw a slight outline }
    Canvas.Pen.Color := GetShadowColor(Canvas.Pen.Color, -20);
    with CaptionRect do
      Canvas.Polyline([Point(Left + 2, Top),
        Point(Right - 2, Top), { Top line }
        Point(Right, Top + 2), { Top right curve }
        Point(Right, Bottom - 2), { Right side line }
        Point(Right - 2, Bottom), { Bottom right curve }
        Point(Left + 2, Bottom), { Bottom line }
        Point(Left, Bottom - 2), { Bottom left curve }
        Point(Left, Top + 2), { Left side line }
        Point(Left + 3, Top)]); { Top left curve }

    { Get the close rect size/position }
    CloseRect := GetCloseRect(CaptionRect);
    { Does it have the pin button? Make some room for it, and draw it. }
    if FDockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);
      if FDockCaptionPinButton = dcpbUp then
        Inc(PinRect.Top); { Down a little further - better looks }
      DrawPinButton(PinRect);
      CaptionRect.Right := PinRect.Right - 2;
    end
    else
    begin
      { Shrink the rect to consider the close button on the right, and
        not draw text in it. }
      CaptionRect.Right := CloseRect.Right - 2;
    end;
    { Move away from the left edge a little before drawing text }
    CaptionRect.Left := CaptionRect.Left + 6;
    { Draw the icon, if found. }
    DrawIcon;
    ShouldDrawClose := CloseRect.Left >= CaptionRect.Left;
  end
  else
  begin
    { Give a rounded effect }
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Top + 1);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Top + 1);

    { fill the middle }
    LDetails := LStyle.GetElementDetails(CVertStates[State.Focused]);
    if not GetThemeColor(LStyle, LDetails, ecFillColor, LColor) then
      if State.Focused then
        LColor := clBtnHighlight
      else
        LColor := clBtnFace;
    Canvas.Brush.Color := LColor;

    Canvas.FillRect(Rect(CaptionRect.Left, CaptionRect.Top + 2,
      CaptionRect.Right, CaptionRect.Bottom));

    Canvas.Pen.Color := State.EndColor;
    Canvas.MoveTo(CaptionRect.Left + 1, CaptionRect.Bottom);
    Canvas.LineTo(CaptionRect.Right - 1, CaptionRect.Bottom);

    Canvas.Font.Orientation := 900; { 90 degrees upwards }
    { Get the close rect size/position }
    CloseRect := GetCloseRect(CaptionRect);

    { Does it have the pin button? Make some room for it, and draw it. }
    if FDockCaptionPinButton <> dcpbNone then
    begin
      PinRect := GetPinRect(CaptionRect);
      DrawPinButton(PinRect);
      CaptionRect.Top := PinRect.Bottom + 2;
    end
    else
      { Add a little spacing between the close button and the text }
      CaptionRect.Top := CloseRect.Bottom + 2;

    ShouldDrawClose := CaptionRect.Top < CaptionRect.Bottom;
    { Make the captionrect horizontal for proper clipping }
    CaptionRect.Right := CaptionRect.Left + (CaptionRect.Bottom - CaptionRect.Top - 2);
    { Position the caption starting position at most at the bottom of the
      rectangle }
    CaptionRect.Top := CaptionRect.Top + Canvas.TextWidth(State.Caption) + 2;

    if CaptionRect.Top > CaptionRect.Bottom then
      CaptionRect.Top := CaptionRect.Bottom;
  end;

  Canvas.Brush.Style := bsClear; { For drawing the font }
  if State.Caption <> '' then
  begin
    if State.Focused then
      Canvas.Font.Style := Canvas.Font.Style + [fsBold]
    else
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];

   if ShouldDrawClose then
     CaptionRect.Right := CaptionRect.Right - (CloseRect.Right - CloseRect.Left) - 4;

    Canvas.TextRect(CaptionRect, State.Caption,
      [tfEndEllipsis, tfVerticalCenter, tfSingleLine]);
  end;

  if ShouldDrawClose then
    DrawCloseButton(CloseRect);
end;

const
  cSideBuffer = 4;

function TDockCaptionDrawer.GetCloseRect(const CaptionRect: TRect): TRect;
var
  CloseSize: Integer;
begin
  CloseSize := CalcButtonSize(CaptionRect);
  if FDockCaptionOrientation = dcoHorizontal then
  begin
    Result.Left := CaptionRect.Right - CloseSize - cSideBuffer;
    Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - CloseSize) div 2;
  end
  else
  begin
    Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - CloseSize) div 2;
    Result.Top := CaptionRect.Top + 2 * cSideBuffer;
  end;
  Result.Right := Result.Left + CloseSize;
  Result.Bottom := Result.Top + CloseSize;
end;

function TDockCaptionDrawer.GetGrabberSize(ADockSite: TWinControl): Integer;
begin
  Result := cDockCaptionDefaultGrabberSize;
end;

function TDockCaptionDrawer.GetPinRect(const CaptionRect: TRect): TRect;
var
  PinSize: Integer;
begin
  PinSize := CalcButtonSize(CaptionRect);
  if FDockCaptionOrientation = dcoHorizontal then
  begin
    Result.Left := CaptionRect.Right - 2*PinSize - 2*cSideBuffer;
    Result.Top := CaptionRect.Top + ((CaptionRect.Bottom - CaptionRect.Top) - PinSize) div 2;
  end
  else
  begin
    Result.Left := CaptionRect.Left + ((CaptionRect.Right - CaptionRect.Left) - PinSize) div 2;
    Result.Top := CaptionRect.Top + 2*cSideBuffer + 2*PinSize;
  end;
  Result.Right := Result.Left + PinSize + 2;
  Result.Bottom := Result.Top + PinSize;
end;

initialization
  DefaultDockTreeClass := TCaptionedDockTree;
end.
