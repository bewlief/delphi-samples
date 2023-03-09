{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.ThemedActnCtrls;

{$HPPEMIT LEGACYHPP}

interface

uses Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Controls, Vcl.Graphics,
  Vcl.Buttons, Vcl.ToolWin, Vcl.ActnMan, Vcl.ActnMenus, Vcl.ActnCtrls,
  Vcl.ActnColorMaps, Vcl.ShadowWnd;

type

{ TThemedMenuItem }

  TThemedMenuItem = class(TCustomMenuItem)
  private
    FCheckRect: TRect;
    FGutterRect: TRect;
    FPaintRect: TRect;
    FSubMenuGlyphRect: TRect;
    FSeparatorHeight: Integer;
    procedure DoDrawMenuCheck;
    procedure DoDrawText(DC: HDC; const Text: string; var Rect: TRect; Flags: Longint);
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
    procedure DrawGlyph(const Location: TPoint); override;
    procedure DrawLargeGlyph(Location: TPoint); override;
    procedure DrawSeparator(const Offset: Integer); override;
    procedure DrawSubMenuGlyph; override;
    procedure DrawText(var Rect: TRect; var Flags: Cardinal; Text: string); override;
    procedure DrawUnusedEdges; override;
    procedure DrawShadowedText(Rect: TRect; Flags: Cardinal; Text: string;
      TextColor: TColor; ShadowColor: TColor); override;
    function GetImageSize: TPoint; override;
  public
    procedure CalcBounds; override;
  end;

{ TThemedMenuButton }

  TThemedMenuButton = class(TCustomMenuButton)
  private
    procedure DoDrawText(const Text: string; var Rect: TRect; Flags: Longint);
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
    procedure DrawText(var ARect: TRect; var Flags: Cardinal;
      Text: string); override;
    procedure DrawShadowedText(Rect: TRect; Flags: Cardinal; Text: string;
      TextColor: TColor; ShadowColor: TColor); override;
  public
    procedure CalcBounds; override;
  end;

{ TThemedPopupMenu }

  TThemedPopupMenu = class(TCustomActionPopupMenu)
  protected
    function GetExpandBtnClass: TCustomMenuExpandBtnClass; override;
    procedure NCPaint(DC: HDC); override;
    procedure PositionPopup(AnOwner: TCustomActionBar;
      ParentItem: TCustomActionControl); override;
    procedure VisibleChanging; override;
    procedure AdjustFont; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TThemedExpandBtn }

  TThemedExpandBtn = class(TCustomMenuExpandBtn)
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
    procedure DrawFrame(ARect: TRect; Down: Boolean); override;
    function GetImageSize: TPoint; override;
  end;

{ TThemedButtonControl }

  TThemedButtonControl = class(TCustomButtonControl)
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
    procedure DrawFrame(ARect: TRect; Down: Boolean); override;
    procedure DrawGlyph(const Location: TPoint); override;
    procedure DrawLargeGlyph(Location: TPoint); override;
    procedure DrawText(var ARect: TRect; var Flags: Cardinal;
      Text: string); override;
    procedure DrawShadowedText(Rect: TRect; Flags: Cardinal; Text: string;
      TextColor, ShadowColor: TColor); override;
  end;

{ TThemedDropDownButton }

  TThemedDropDownButton = class(TCustomDropDownButton)
  protected
    procedure DrawFrame(ARect: TRect; Down: Boolean); override;
    procedure DrawBackground(var PaintRect: TRect); override;
    function GetDropDownButtonWidth: Integer; override;
    function GetPopupClass: TCustomActionBarClass; override;
    procedure DrawGlyph(const Location: TPoint); override;
    procedure DrawLargeGlyph(Location: TPoint); override;
    procedure DrawText(var ARect: TRect; var Flags: Cardinal;
       Text: string); override;
  end;

{ TThemedCustomizePopup }

  TThemedCustomizePopup = class(TCustomizeActionToolBar)
  protected
    procedure NCPaint(DC: HDC); override;
    function GetAddRemoveItemClass: TCustomAddRemoveItemClass; override;
    function GetDefaultColorMapClass: TCustomColorMapClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TThemedToolScrollBtn }

  TThemedToolScrollBtn = class(TCustomToolScrollBtn)
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
  end;

{ TThemedAddRemoveItem }

  TThemedAddRemoveItem = class(TCustomAddRemoveItem)
  private
    FCheckRect: TRect;
    FVisibleCheckRect: TRect;
    FGutterRect: TRect;
    FPaintRect: TRect;
    FSubMenuGlyphRect: TRect;
    FSeparatorHeight: Integer;
    procedure DoDrawText(DC: HDC; const Text: string; var Rect: TRect; Flags: Longint);
  protected
    procedure DrawBackground(var PaintRect: TRect); override;
    procedure DrawGlyph(const Location: TPoint); override;
    procedure DrawSeparator(const Offset: Integer); override;
    procedure DrawText(var Rect: TRect; var Flags: Cardinal; Text: string); override;
    procedure DrawUnusedEdges; override;
    function GetImageSize: TPoint; override;
  public
    procedure CalcBounds; override;
  end;

implementation

uses
{$IF DEFINED(CLR)}
  System.Runtime.InteropServices,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Contnrs, 
  Vcl.Forms, Vcl.ActnList, Vcl.GraphUtil, Vcl.ListActns, Vcl.ImgList,
  Vcl.Themes, Vcl.Menus, Winapi.CommCtrl, Winapi.UxTheme;

function IsMouseButtonPressed: Boolean;
begin
  Result := not (((GetAsyncKeyState(VK_RBUTTON)and $8000)=0) and
    ((GetAsyncKeyState(VK_LBUTTON)and $8000)=0));
end;

{ TThemedMenuItem }

procedure TThemedMenuItem.CalcBounds;
const
  CheckMarkStates: array[Boolean] of TThemedMenu = (tmPopupCheckDisabled, tmPopupCheckNormal);
  SubMenuStates: array[Boolean] of TThemedMenu = (tmPopupSubMenuDisabled, tmPopupSubMenuNormal);
var
  DC: HDC;
  LFont: HFONT;
  LBounds: TRect;
  LImageSize: TPoint;
  LHeight, LWidth, Offset: Integer;
  LGlyphSize, LGutterSize, LSeparatorSize, LSubMenuGlyphSize: TSize;
  LCheckMargins, LGutterMargins, LMenuItemMargins, LSeparatorMargins, LSubMenuGlyphMargins: TElementMargins;
  TempFont: TFont;
  PPI: Integer;
  LStyle: TCustomStyleServices;
begin
  DC := CreateCompatibleDC(0);
  PPI := GetDevicePPI;
  LStyle := StyleServices;
  if PPI <> Screen.PixelsPerInch then
    TempFont := TFont.Create
  else
    TempFont := nil;
  try
    if TempFont <> nil then
    begin
      TempFont.Assign(Screen.MenuFont);
      TempFont.Height := MulDiv(TempFont.Height, PPI, Screen.PixelsPerInch);
      LFont := SelectObject(DC, TempFont.Handle);
    end
    else
      LFont := SelectObject(DC, Screen.MenuFont.Handle);
    try
      inherited;
      LHeight := 0;
      LWidth := 0;

      // Check/Glyph
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(CheckMarkStates[Enabled]), esActual, LGlyphSize, PPI);
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(CheckMarkStates[Enabled]), emContent, LCheckMargins, PPI);

      // Gutter
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(tmPopupGutter), esActual, LGutterSize);
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(tmPopupGutter), emSizing, LGutterMargins);

      // Menu item
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(tmPopupItemNormal), emSizing, LMenuItemMargins, PPI);
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(SubMenuStates[Enabled]), esActual, LSubMenuGlyphSize, PPI);
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(SubMenuStates[Enabled]), emContent, LSubMenuGlyphMargins);

      // Calculate check/glyph size
      LImageSize := GetImageSize;
      if LImageSize.Y > LGlyphSize.cy then
        LGlyphSize.cy := LImageSize.Y;
      if LImageSize.X > LGlyphSize.cx then
        LGlyphSize.cx := LImageSize.X;
      Inc(LHeight, LGlyphSize.cy);
      Inc(LWidth, LGlyphSize.cx);

      // Add margins for check/glyph
      Inc(LHeight, LCheckMargins.cyTopHeight + LCheckMargins.cyBottomHeight);
      Inc(LWidth, LCheckMargins.cxLeftWidth + LCheckMargins.cxRightWidth);
      FCheckRect := Rect(0, 0,
        LGlyphSize.cx + LCheckMargins.cxRightWidth + LCheckMargins.cxRightWidth,
        LGlyphSize.cy + LCheckMargins.cyBottomHeight + LCheckMargins.cyBottomHeight);

      // Add size and margins for gutter
      Inc(LWidth, LGutterMargins.cxLeftWidth);
      FGutterRect.Left := LWidth;
      FGutterRect.Right := FGutterRect.Left + LGutterSize.cx;
      Inc(LWidth, LGutterSize.cx + LGutterMargins.cxRightWidth);

      // Add margins for menu item
      Inc(LWidth, LMenuItemMargins.cxLeftWidth + LMenuItemMargins.cxRightWidth);
      Offset := LWidth - TextBounds.Left - LMenuItemMargins.cxRightWidth;
      LBounds := TextBounds;
      OffsetRect(LBounds, Offset, -1);
      TextBounds := LBounds;

      // Add size of potential submenu glyph
      Inc(LWidth, LSubMenuGlyphSize.cx);
      Inc(LWidth, LSubMenuGlyphMargins.cxLeftWidth);
      Inc(LWidth, LSubMenuGlyphMargins.cxRightWidth);
      // Add Width of menu item to FSubMenuGlyphRect before using
      FSubMenuGlyphRect := Rect(-LSubMenuGlyphMargins.cxRightWidth - LSubMenuGlyphSize.cx,
        (Height - LSubMenuGlyphSize.cy) div 2,
        -LSubMenuGlyphMargins.cxRightWidth,
        ((Height - LSubMenuGlyphSize.cy) div 2) + LSubMenuGlyphSize.cy);

      // Add margins for menu short cut
      if ActionClient <> nil then
      begin
        LBounds := Rect(0, 0, 0, 0);
        DoDrawText(DC, ActionClient.ShortCutText, LBounds, DT_CALCRECT or DT_NOCLIP);
      end
      else
        LBounds := ShortCutBounds;

      Offset := FSubMenuGlyphRect.Left - LBounds.Right -
        LMenuItemMargins.cxRightWidth - LSubMenuGlyphMargins.cxLeftWidth;
      OffsetRect(LBounds, Offset, 0);
      // Add Width of menu item to ShortCutBounds before using
      ShortCutBounds := LBounds;
      Inc(LWidth, LMenuItemMargins.cxLeftWidth + LMenuItemMargins.cxRightWidth);

      // Adjust size if separator
      if Separator then
      begin
        LStyle.GetElementSize(DC, LStyle.GetElementDetails(tmPopupSeparator), esActual, LSeparatorSize);
        LStyle.GetElementMargins(DC, LStyle.GetElementDetails(tmPopupSeparator), emSizing, LSeparatorMargins);

        LHeight := LSeparatorSize.cy + LSeparatorMargins.cyBottomHeight;
        LWidth := LSeparatorSize.cx;
        FSeparatorHeight := LSeparatorSize.cy;
      end
      else
      // adjust the size when then text height is bigger
      if LHeight <= TextBounds.Height then
        LHeight := TextBounds.Height + LCheckMargins.cyTopHeight + LCheckMargins.cyBottomHeight;

      FGutterRect.Top := 0;
      FGutterRect.Bottom := LHeight;
      SetBounds(Left, Top,
        LWidth + TextBounds.Right - TextBounds.Left + ShortCutBounds.Right - ShortCutBounds.Left,
        LHeight);
    finally
      SelectObject(DC, LFont);
    end;
  finally
    DeleteDC(DC);
    if TempFont <> nil then
      TempFont.Free;
  end;
end;

procedure TThemedMenuItem.DoDrawMenuCheck;
const
  CheckMarkBkgStates: array[Boolean] of TThemedMenu = (tmPopupCheckBackgroundDisabled, tmPopupCheckBackgroundNormal);
  CheckMarkStates: array[Boolean] of TThemedMenu = (tmPopupCheckDisabled, tmPopupCheckNormal);
var
  R: TRect;
  LCheckSize: TSize;
  LPPI: Integer;
  LStyle: TCustomStyleServices;
begin
  if IsChecked then
  begin
    LStyle := StyleServices;
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckMarkBkgStates[Enabled]), FCheckRect);
    if not HasGlyph then
    begin
      LPPI := GetDevicePPI;
      LStyle.GetElementSize(Canvas.Handle, LStyle.GetElementDetails(CheckMarkStates[Enabled]), esActual, LCheckSize, LPPI);
      R := Rect(0, 0, LCheckSize.cx, LCheckSize.cy);
      OffsetRect(R, FCheckRect.Left + (FCheckRect.Width - LCheckSize.cx) div 2,
        FCheckRect.Top + (FCheckRect.Height - LCheckSize.cy) div 2);
      LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckMarkStates[Enabled]), R, nil, LPPI);
    end;
  end;
end;

procedure TThemedMenuItem.DoDrawText(DC: HDC; const Text: string;
  var Rect: TRect; Flags: Longint);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmPopupItemDisabled, tmPopupItemNormal);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  // get format and color
  LFormats := TTextFormatFlags(Flags);
  LStyle := StyleServices;
  if Selected and Enabled then
    LDetails := LStyle.GetElementDetails(tmPopupItemHot)
  else
    LDetails := LStyle.GetElementDetails(MenuStates[Enabled or ActionBar.DesignMode]);

  if not LStyle.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := ActionBar.ColorMap.FontColor;

  // tweak text
  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  // Draw menu item text
  if LStyle.IsSystemStyle then
    LStyle.DrawText(DC, LDetails, LCaption, Rect, LFormats, LColor)
  else
  begin
    Winapi.Windows.SetBkMode(DC, Winapi.Windows.TRANSPARENT);
    Winapi.Windows.SetTextColor(DC, LColor);
    Winapi.Windows.DrawText(DC, PChar(LCaption), Length(LCaption), Rect, Flags);
  end;
end;

procedure TThemedMenuItem.DrawBackground(var PaintRect: TRect);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  FPaintRect := PaintRect;
  if LStyle.IsSystemStyle then
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupBackground), FPaintRect);
  LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupGutter), FGutterRect);
end;

procedure TThemedMenuItem.DrawGlyph(const Location: TPoint);
var
  LImageSize, LLocation: TPoint;
begin
  if (Action is TCustomAction) and TCustomAction(Action).Checked then
    DoDrawMenuCheck;
  if HasGlyph then
  begin
    LImageSize := GetImageSize;
    LLocation.X := ((FCheckRect.Right - FCheckRect.Left) - LImageSize.X) div 2;
    LLocation.Y := ((FCheckRect.Bottom - FCheckRect.Top) - LImageSize.Y) div 2;
    inherited DrawGlyph(LLocation);
  end;
end;

procedure TThemedMenuItem.DrawLargeGlyph(Location: TPoint);
var
  LImageSize: TPoint;
  ImageList: TCustomImageList;
  LDisabled, DrawEnabled: Boolean;
begin
  if not HasGlyph and ((Action is TCustomAction) and TCustomAction(Action).Checked) then
    DoDrawMenuCheck
  else
  begin
    ImageList := FindImageList(True, LDisabled, ActionClient.ImageIndex);
    if not Assigned(ImageList) then
      Exit;

    LImageSize := GetImageSize;
    if ImageList.Width = LImageSize.X then
      Location.X := ((FCheckRect.Right - FCheckRect.Left) - LImageSize.X) div 2
    else
      Location.X := (TextBounds.Left - ImageList.Width - Margins.Left - Margins.Right) div 2;

    if ImageList.Height = LImageSize.Y then
      Location.Y := ((FCheckRect.Bottom - FCheckRect.Top) - LImageSize.Y) div 2
    else
      Location.Y := (Height - ImageList.Height - Margins.Top - Margins.Bottom) div 2;

    DrawEnabled := LDisabled or Enabled and (ActionClient.ImageIndex <> -1) or
      (csDesigning in ComponentState);
    ImageList.Draw(Canvas, Location.X, Location.Y, ActionClient.ImageIndex,
      dsTransparent, itImage, DrawEnabled);
  end;
end;

procedure TThemedMenuItem.DrawSeparator(const Offset: Integer);
var
  LRect: TRect;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  LRect := Rect(FGutterRect.Right + 1, 0, Width, FSeparatorHeight);
                                                                               
  LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupSeparator), LRect);
end;

procedure TThemedMenuItem.DrawShadowedText(Rect: TRect; Flags: Cardinal;
  Text: string; TextColor, ShadowColor: TColor);
begin
  DrawText(Rect, Flags, Text)
end;

procedure TThemedMenuItem.DrawSubMenuGlyph;
const
  SubMenuStates: array[Boolean] of TThemedMenu = (tmPopupSubMenuDisabled, tmPopupSubMenuNormal);
var
  LRect: TRect;
  LSize: TSize;
  PPI: Integer;
  LStyle: TCustomStyleServices;
begin
  LRect := FSubMenuGlyphRect;
  LStyle := StyleServices;
  PPI := GetDevicePPI;
  OffsetRect(LRect, Width, 1);
  if LStyle.GetElementSize(Canvas.Handle,
    LStyle.GetElementDetails(tmPopupSubMenuNormal), esActual, LSize, PPI)
  then
  begin
    LRect.Top := (Height - LSize.cy) div 2;
    LRect.Bottom := LRect.Top + LSize.cy;
    LRect.Right := LRect.Left + LSize.cx;
  end
  else
  begin
    LRect.Top := (Height - FSubMenuGlyphRect.Height) div 2;
    LRect.Bottom := LRect.Top + FSubMenuGlyphRect.Height;
  end;

  LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(SubMenuStates[Enabled]), LRect, nil, PPI);
end;

procedure TThemedMenuItem.DrawText(var Rect: TRect; var Flags: Cardinal; Text: string);
var
  LRect: TRect;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  // Draw menu highlight
  if Selected and Enabled then
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupItemHot), FPaintRect)
  else if Selected then
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupItemDisabledHot), FPaintRect);

  // Draw menu item text
  if (Parent is TCustomActionBar) and (not ActionBar.PersistentHotkeys) then
    Text := FNoPrefix;
  Canvas.Font := Screen.MenuFont;
  Canvas.Font.Height := MulDiv(Canvas.Font.Height, GetDevicePPI, Screen.PixelsPerInch);
  if ActionClient.Default then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  LRect := FPaintRect;
  DoDrawText(Canvas.Handle, Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);
  OffsetRect(LRect, Rect.Left,
    ((FPaintRect.Bottom - FPaintRect.Top) - (LRect.Bottom - LRect.Top)) div 2);
  DoDrawText(Canvas.Handle, Text, LRect, Flags);

  // Draw shortcut
  if ShowShortCut and ((ActionClient <> nil) and not ActionClient.HasItems) then
  begin
    Flags := DrawTextBiDiModeFlags(DT_RIGHT);
    LRect := TRect.Create(ShortCutBounds.Left, LRect.Top, ShortCutBounds.Right, LRect.Bottom);
    LRect.Offset(Width, 0);
    DoDrawText(Canvas.Handle, ActionClient.ShortCutText, LRect, Flags);
  end;
end;

procedure TThemedMenuItem.DrawUnusedEdges;
begin
end;

function TThemedMenuItem.GetImageSize: TPoint;
var
  LImages: TCustomImageList;
begin
  Result := Point(0,0);
  if not Assigned(ActionClient) then Exit;

  LImages := FindImageList(not ActionClient.ActionClients.SmallIcons);
  if LImages <> nil then
  begin
    if not ActionClient.ActionClients.SmallIcons and
       (LImages.Width = LImages.Height) and (LImages.Width < 32) then
      Result := Point(32, 32)
    else
      Result := Point(LImages.Width, LImages.Height);
  end;
end;

{ TThemedMenuButton }

procedure TThemedMenuButton.CalcBounds;
var
  TempFont: TFont;
begin
  if (Menu <> nil) and (Menu.GetDevicePPI <> Screen.PixelsPerInch) then
  begin
    TempFont := TFont.Create;
    try
      TempFont.Assign(Screen.MenuFont);
      TempFont.Height := MulDiv(TempFont.Height, Menu.GetDevicePPI, Screen.PixelsPerInch);
      Font.Assign(TempFont);
    finally
      TempFont.Free;
    end;
  end
  else
    Font.Assign(Screen.MenuFont);
  inherited CalcBounds;
  Width := Width + ScaleValue(2);
end;

procedure TThemedMenuButton.DoDrawText(const Text: string;
  var Rect: TRect; Flags: Integer);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmMenuBarItemNormal, tmMenuBarItemHot);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
  LStyle: TCustomStyleServices;
  LParentForm: TCustomForm;
begin
  // get format and color
  LStyle := StyleServices;
  LFormats := TTextFormatFlags(Flags);

  if Enabled then
    LDetails := LStyle.GetElementDetails(MenuStates[Selected or MouseInControl or ActionBar.DesignMode])
  else
    LDetails := LStyle.GetElementDetails(tmMenuBarItemDisabled);

  if not (csDesigning in ComponentState) and (csGlassPaint in ControlState) then
  begin
    LParentForm := GetParentForm(Self);
    if (LParentForm <> nil) and LParentForm.CustomTitleBar.Enabled then
    begin
      if LParentForm.Active then
        LColor := LParentForm.CustomTitleBar.ForegroundColor
      else
        LColor := LParentForm.CustomTitleBar.InactiveForegroundColor;
    end else if not LStyle.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
      LColor := ActionBar.ColorMap.FontColor;
  end else
  begin
    Canvas.Brush.Style := bsClear;
    if Selected then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := clMenuText;
    if not LStyle.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
      LColor := ActionBar.ColorMap.FontColor;
  end;
  // tweak text
  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  // Draw menu item text
  if Enabled then
    LDetails := LStyle.GetElementDetails(MenuStates[Selected or MouseInControl]);
  if LStyle.IsSystemStyle then
    LStyle.DrawText(Canvas.Handle, LDetails, LCaption, Rect, LFormats, LColor)
  else
  begin
    WinApi.Windows.SetBkMode(Canvas.Handle, Winapi.Windows.TRANSPARENT);
    WinApi.Windows.SetTextColor(Canvas.Handle, LColor);
    Winapi.Windows.DrawText(Canvas.Handle, PChar(LCaption), Length(LCaption), Rect, Flags);
  end;
end;

procedure TThemedMenuButton.DrawBackground(var PaintRect: TRect);
const
  MenuStates: array[Boolean {MouseInControl}, Boolean {Selected}] of TThemedMenu =
    ((tmMenuBarItemNormal, tmMenuBarItemPushed), (tmMenuBarItemHot, tmMenuBarItemPushed));
var
  LStyle: TCustomStyleServices;
  LParentForm: TCustomForm;
  LColor: TColor;
begin
  if not (csDesigning in ComponentState) and (csGlassPaint in ControlState) then
  begin
    LStyle := StyleServices;
    LColor := LStyle.GetSystemColor(clMenuBar);
    LParentForm := GetParentForm(Self);
    if (MouseInControl or Selected) then
    begin
      LColor := LStyle.GetSystemColor(clHighlight);
      if (LParentForm <> nil) and LParentForm.CustomTitleBar.Enabled and LParentForm.Active then
        LColor := ColorBlendRGB(LColor, LParentForm.CustomTitleBar.BackgroundColor, 0.6)
      else if (LParentForm <> nil) and LParentForm.CustomTitleBar.Enabled and not LParentForm.Active then
        LColor := ColorBlendRGB(LColor, LParentForm.CustomTitleBar.InactiveBackgroundColor, 0.6);
    end else if (LParentForm <> nil) and LParentForm.CustomTitleBar.Enabled then
    begin
      if LParentForm.Active then
        LColor := LParentForm.CustomTitleBar.BackgroundColor
      else
        LColor := LParentForm.CustomTitleBar.InactiveBackgroundColor;
    end;
    Canvas.Brush.Color := LColor;
    Canvas.FillRect(PaintRect);
  end else
  begin
    Canvas.Brush.Color := ActionBar.ColorMap.Color;
    LStyle := StyleServices;
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(MenuStates[MouseInControl, Selected]), PaintRect);
  end
end;

procedure TThemedMenuButton.DrawShadowedText(Rect: TRect; Flags: Cardinal;
  Text: string; TextColor, ShadowColor: TColor);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  if LStyle.Enabled and not LStyle.IsSystemStyle then
    DrawText(Rect, Flags, Text)
  else
    inherited;
end;

procedure TThemedMenuButton.DrawText(var ARect: TRect; var Flags: Cardinal;
  Text: string);
var
  LRect: TRect;
begin
  if Parent is TCustomActionMainMenuBar then
    if not TCustomActionMainMenuBar(Parent).PersistentHotkeys then
      Text := StripHotkey(Text);

  // Draw menu item text
  LRect := ARect;
  Inc(LRect.Left);
  Canvas.Font := Screen.MenuFont;
  Canvas.Font.Height := MulDiv(Canvas.Font.Height, CurrentPPI, Screen.PixelsPerInch);
  DoDrawText(Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);
  DoDrawText(Text, LRect, Flags);
end;

{ TThemedPopupMenu }

constructor TThemedPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  VertMargin := 0;
end;

function TThemedPopupMenu.GetExpandBtnClass: TCustomMenuExpandBtnClass;
begin
  Result := TThemedExpandBtn;
end;

procedure TThemedPopupMenu.NCPaint(DC: HDC);
var
  RC, RW: TRect;
  OldHandle: THandle;
  LStyle: TCustomStyleServices;
begin
  Winapi.Windows.GetClientRect(Handle, RC);
  GetWindowRect(Handle, RW);
  MapWindowPoints(0, Handle, RW, 2);
  OffsetRect(RC, -RW.Left, -RW.Top);
  ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
  { Draw border in non-client area }
  OffsetRect(RW, -RW.Left, -RW.Top);

  OldHandle := Canvas.Handle;
  LStyle := Self.StyleServices;
  if LStyle.Enabled and not LStyle.IsSystemStyle
  then
    begin
      try
        Canvas.Handle := DC;
        LStyle.DrawElement(DC, LStyle.GetElementDetails(tmPopupBorders),
         Rect(0, 0, RW.Right, RW.Bottom));
      finally
        Canvas.Handle := OldHandle;
      end;
      Exit;
    end
  else
  try
    Canvas.Handle := DC;
    Canvas.Pen.Width := 1;

    Canvas.Pen.Color := ColorMap.FrameTopLeftOuter;
    Canvas.MoveTo(RW.Right, RW.Top);
    Canvas.LineTo(RW.Left, Rw.Top);
    Canvas.LineTo(RW.Left, RW.Bottom);

    Canvas.Pen.Color := ColorMap.FrameTopLeftInner;
    Canvas.MoveTo(RW.Right - 1, RW.Top + 1);
    Canvas.LineTo(RW.Left + 1, Rw.Top + 1);
    Canvas.LineTo(RW.Left + 1, RW.Bottom - 2);

    Canvas.Pen.Color := ColorMap.FrameBottomRightOuter;
    Canvas.MoveTo(RW.Right - 1, RW.Top);
    Canvas.LineTo(RW.Right - 1, RW.Bottom - 1);
    Canvas.LineTo(RW.Left - 1, RW.Bottom - 1);

    Canvas.Pen.Color := ColorMap.FrameBottomRightInner;
    Canvas.MoveTo(RW.Right - 2, RW.Top + 1);
    Canvas.LineTo(RW.Right - 2, RW.Bottom - 2);
    Canvas.LineTo(RW.Left, RW.Bottom - 2);
  finally
    Canvas.Handle := OldHandle;
  end;
end;

procedure TThemedPopupMenu.PositionPopup(AnOwner: TCustomActionBar;
  ParentItem: TCustomActionControl);
begin
  inherited PositionPopup(AnOwner, ParentItem);
  if (ParentItem is TCustomMenuItem) and (Left > ParentItem.Parent.BoundsRect.Left) then
    Left := ParentItem.Parent.BoundsRect.Right - 1
  else if ParentItem is TCustomMenuButton and (Left > ParentItem.BoundsRect.Left) then
    Left := Left - 1;
end;

procedure TThemedPopupMenu.VisibleChanging;
begin
  if ParentControl is TCustomButtonControl then
    ParentControl.Invalidate;
  inherited;
end;

procedure TThemedPopupMenu.AdjustFont;
begin
  Font.Assign(Screen.MenuFont);
  Font.Height := MulDiv(Font.Height, GetDevicePPI, Screen.PixelsPerInch);
end;

{ TThemedExpandBtn }

procedure TThemedExpandBtn.DrawBackground(var PaintRect: TRect);
var
  LRect: TRect;
  LImageSize: TPoint;

  LCheckMargins, LGutterMargins: TElementMargins;
  LGlyphSize, LGutterSize: TSize;
begin
  InflateRect(PaintRect, 1, 1);

  with Canvas, StyleServices do
  begin
    // draw background
    DrawElement(Handle, GetElementDetails(tmPopupBackground), PaintRect);

    // get margins
    GetElementMargins(Handle, GetElementDetails(tmPopupCheckNormal), emContent, LCheckMargins);
    GetElementMargins(Handle, GetElementDetails(tmPopupGutter), emSizing, LGutterMargins);

    // get sizes
    GetElementSize(Handle, GetElementDetails(tmPopupCheckNormal), esActual, LGlyphSize);
    GetElementSize(Handle, GetElementDetails(tmPopupGutter), esActual, LGutterSize);

    LImageSize := GetImageSize;
    if LImageSize.Y > LGlyphSize.cy then
      LGlyphSize.cy := LImageSize.Y;
    if LImageSize.X > LGlyphSize.cx then
      LGlyphSize.cx := LImageSize.X;

    LRect.Left := LCheckMargins.cxLeftWidth + LCheckMargins.cxRightWidth + LGlyphSize.cx;
    LRect.Top := 0;
    LRect.Right := LRect.Left + LGutterSize.cx;
    LRect.Bottom := Height;

    // draw gutter
    DrawElement(Handle, GetElementDetails(tmPopupGutter), LRect);
  end;
end;

procedure TThemedExpandBtn.DrawFrame(ARect: TRect; Down: Boolean);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  if Enabled and ((FState = bsDown) or not Flat or MouseInControl or IsChecked) then
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupItemHot), ARect);
end;

function TThemedExpandBtn.GetImageSize: TPoint;
var
  LMenu: TCustomActionMenuBar;
  LImages: TCustomImageList;
  LLargeImages: Boolean;
begin
  LMenu := Menu;
  LImages := nil;
  Result := Point(0, 0);

  if LMenu = nil then Exit;
  LLargeImages := (LMenu.RootMenu.ActionClient <> nil) and
    (not LMenu.RootMenu.ActionClient.Items.SmallIcons);
  if LMenu.ActionManager <> nil then
  begin
    if LLargeImages and (LMenu.ActionManager.LargeImages <> nil) then
      LImages := LMenu.ActionManager.LargeImages
    else
      LImages := LMenu.ActionManager.Images;
  end;

  if LImages <> nil then
  begin
    if (LMenu.RootMenu.ActionClient <> nil) and
       (not LMenu.RootMenu.ActionClient.Items.SmallIcons and
       (LImages.Width = LImages.Height) and (LImages.Width < 32)) then
      Result := Point(32, 32)
    else
      Result := Point(LImages.Width, LImages.Height);
  end;
end;

{ TThemedButtonControl }

procedure TThemedButtonControl.DrawBackground(var PaintRect: TRect);
const
  DisabledState: array[Boolean] of TThemedToolBar = (ttbButtonDisabled, ttbButtonPressed);
  CheckedState: array[Boolean] of TThemedToolBar = (ttbButtonHot, ttbButtonCheckedHot);
var
  SaveIndex: Integer;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  if not LStyle.IsSystemStyle and ActionClient.Separator then Exit;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    if Enabled and not (ActionBar.DesignMode) then
    begin
      if (MouseInControl or IsChecked) and
         Assigned(ActionClient) {and not ActionClient.Separator)} then
      begin
        LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckedState[IsChecked or (FState = bsDown)]), PaintRect);

        if not MouseInControl then
          LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(ttbButtonPressed), PaintRect);
      end
      else
        LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(ttbButtonNormal), PaintRect);
    end
    else
      LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(DisabledState[IsChecked]), PaintRect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

procedure TThemedButtonControl.DrawFrame(ARect: TRect; Down: Boolean);
begin
  // This button style has no frame
end;

procedure TThemedButtonControl.DrawGlyph(const Location: TPoint);
var
  NewLocation: TPoint;
begin
  NewLocation := Location;
  if not (csDesigning in ComponentState) and ((FState = bsDown) and not IsChecked)
     and IsMouseButtonPressed then
  begin
    Inc(NewLocation.X);
    Inc(NewLocation.Y);
  end;
  inherited DrawGlyph(NewLocation);
end;

procedure TThemedButtonControl.DrawLargeGlyph(Location: TPoint);
var
  ImageList: TCustomImageList;
  DrawEnabled: Boolean;
  LDisabled: Boolean;
begin
  if not HasGlyph then
    Exit;

  ImageList := FindImageList(True, LDisabled, ActionClient.ImageIndex);
  if not Assigned(ImageList) then
    Exit;

  if (ImageList.Width + Margins.Left + Margins.Right) < TextBounds.Left then
    Inc(Location.X, (TextBounds.Left - ImageList.Width - Margins.Left - Margins.Right) div 2);
  if (ImageList.Height + Margins.Top + Margins.Bottom) < Height then
    Inc(Location.Y, (Height - ImageList.Height - Margins.Top - Margins.Bottom) div 2);

  if not (csDesigning in ComponentState) and ((FState = bsDown) and not IsChecked)
     and IsMouseButtonPressed then
  begin
    Inc(Location.X);
    Inc(Location.Y);
  end;

  DrawEnabled := LDisabled or Enabled and (ActionClient.ImageIndex <> -1) or
    (csDesigning in ComponentState);
  ImageList.Draw(Canvas, Location.X, Location.Y, ActionClient.ImageIndex,
    dsTransparent, itImage, DrawEnabled);
end;

procedure TThemedButtonControl.DrawShadowedText(Rect: TRect; Flags: Cardinal;
  Text: string; TextColor, ShadowColor: TColor);
begin
  if IsCustomStyleActive then
    DrawText(Rect, Flags, Text)
  else
    inherited;
end;

procedure TThemedButtonControl.DrawText(var ARect: TRect; var Flags: Cardinal; Text: string);
const
  DisabledState: array[Boolean] of TThemedToolBar = (ttbButtonDisabled, ttbButtonPressed);
  CheckedState: array[Boolean] of TThemedToolBar = (ttbButtonHot, ttbButtonCheckedHot);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  // get format and color
  LStyle := StyleServices;
  LFormats := TTextFormatFlags(Flags);

  if Enabled and not (ActionBar.DesignMode) then
  begin
    if (MouseInControl or IsChecked) and
       Assigned(ActionClient) then
    begin
      LDetails := LStyle.GetElementDetails(CheckedState[IsChecked or (FState = bsDown)]);
      if not MouseInControl then
        LDetails := LStyle.GetElementDetails(ttbButtonPressed);
    end
    else
      LDetails :=  LStyle.GetElementDetails(ttbButtonNormal);
  end
  else
    LDetails := LStyle.GetElementDetails(DisabledState[IsChecked]);

  if not LStyle.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := Canvas.Font.Color;

  // tweak text
  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  // Draw menu item text
 if csGlassPaint in ControlState then
   DrawGlassText(Canvas.Handle, Text, ARect, Flags, 0, Canvas.Font.Color)
 else
   LStyle.DrawText(Canvas.Handle, LDetails, LCaption, ARect, LFormats, LColor);
end;


{ TThemedDropDownButton }

procedure TThemedDropDownButton.DrawBackground(var PaintRect: TRect);
const
  DisabledState: array[Boolean] of TThemedToolBar = (ttbButtonDisabled, ttbButtonPressed);
  CheckedState: array[Boolean] of TThemedToolBar = (ttbButtonHot, ttbButtonCheckedHot);
var
  SaveIndex: Integer;
  LStyle: TCustomStyleServices;
begin
  SaveIndex := SaveDC(Canvas.Handle);
  try
    LStyle := StyleServices;
    if Enabled and not (ActionBar.DesignMode) then
    begin
      if (MouseInControl or IsChecked or DroppedDown) and
         (Assigned(ActionClient) and not ActionClient.Separator) then
      begin
        LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckedState[IsChecked or (FState = bsDown)]), PaintRect);

       if IsChecked and not MouseInControl then
          LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(ttbButtonPressed), PaintRect);
      end
      else
        LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(ttbButtonNormal), PaintRect);
    end
    else
      LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(DisabledState[IsChecked]), PaintRect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

procedure TThemedDropDownButton.DrawFrame(ARect: TRect; Down: Boolean);
var
  SaveIndex: Integer;
  LStyle: TCustomStyleServices;
begin
  if Enabled and not (ActionBar.DesignMode) then
    if (MouseInControl or IsChecked or DroppedDown) and Assigned(ActionClient) then
    begin
      LStyle := StyleServices;
      if not LStyle.IsSystemStyle then
      begin
        ARect.Left := ARect.Right - GetDropDownButtonWidth;
        ARect.Right := ARect.Left + 3;
      end
      else
        ARect.Left := ARect.Right - GetDropDownButtonWidth - 2;
      SaveIndex := SaveDC(Canvas.Handle);
      try
        LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(ttbSeparatorHot), ARect);
      finally
        RestoreDC(Canvas.Handle, SaveIndex);
      end;
    end;
end;

procedure TThemedDropDownButton.DrawGlyph(const Location: TPoint);
var
  NewLocation: TPoint;
begin
  NewLocation := Location;
  if not (csDesigning in ComponentState) and ((FState = bsDown) and not IsChecked)
     and IsMouseButtonPressed then
  begin
    Inc(NewLocation.X);
    Inc(NewLocation.Y);
  end;
  inherited DrawGlyph(NewLocation);
end;

procedure TThemedDropDownButton.DrawLargeGlyph(Location: TPoint);
var
  ImageList: TCustomImageList;
  DrawEnabled: Boolean;
  LDisabled: Boolean;
begin
  if not HasGlyph then
    Exit;

  ImageList := FindImageList(True, LDisabled, ActionClient.ImageIndex);
  if not Assigned(ImageList) then
    Exit;

  if (ImageList.Width + Margins.Left + Margins.Right) < TextBounds.Left then
    Inc(Location.X, (TextBounds.Left - ImageList.Width - Margins.Left - Margins.Right) div 2);
  if (ImageList.Height + Margins.Top + Margins.Bottom) < Height then
    Inc(Location.Y, (Height - ImageList.Height - Margins.Top - Margins.Bottom) div 2);

  if not (csDesigning in ComponentState) and ((FState = bsDown) and not IsChecked)
     and IsMouseButtonPressed then
  begin
    Inc(Location.X);
    Inc(Location.Y);
  end;

  DrawEnabled := LDisabled or Enabled and (ActionClient.ImageIndex <> -1) or
    (csDesigning in ComponentState);
  ImageList.Draw(Canvas, Location.X, Location.Y, ActionClient.ImageIndex,
    dsTransparent, itImage, DrawEnabled);
end;

const
  cDropDownSize = 11;

function TThemedDropDownButton.GetDropDownButtonWidth: Integer;
begin
  Result := ScaleValue(cDropDownSize);
end;

function TThemedDropDownButton.GetPopupClass: TCustomActionBarClass;
begin
  Result := TThemedPopupMenu;
end;

procedure TThemedDropDownButton.DrawText(var ARect: TRect; var Flags: Cardinal; Text: string);
const
  DisabledState: array[Boolean] of TThemedToolBar = (ttbButtonDisabled, ttbButtonPressed);
  CheckedState: array[Boolean] of TThemedToolBar = (ttbButtonHot, ttbButtonCheckedHot);

var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  // get format and color
  LStyle := StyleServices;
  LFormats := TTextFormatFlags(Flags);

  if Enabled and not (ActionBar.DesignMode) then
  begin
    if (MouseInControl or IsChecked) and
       Assigned(ActionClient) then
    begin
      LDetails := LStyle.GetElementDetails(CheckedState[IsChecked or (FState = bsDown)]);
      if not MouseInControl then
        LDetails := LStyle.GetElementDetails(ttbButtonPressed);
    end
    else
      if DroppedDown then
        LDetails := LStyle.GetElementDetails(ttbButtonHot)
      else
        LDetails := LStyle.GetElementDetails(ttbButtonNormal);
  end
  else
    LDetails := LStyle.GetElementDetails(DisabledState[IsChecked]);

  if not LStyle.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := Canvas.Font.Color;

  // tweak text
  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  // Draw menu item text
  LStyle.DrawText(Canvas.Handle, LDetails, LCaption, ARect, LFormats, LColor);
end;

{ TThemedCustomizePopup }

constructor TThemedCustomizePopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  VertMargin := 0;
end;

function TThemedCustomizePopup.GetAddRemoveItemClass: TCustomAddRemoveItemClass;
begin
  Result := TThemedAddRemoveItem;
end;

function TThemedCustomizePopup.GetDefaultColorMapClass: TCustomColorMapClass;
begin
  Result := TThemedColorMap;
end;

procedure TThemedCustomizePopup.NCPaint(DC: HDC);
var
  RC, RW: TRect;
begin
  Winapi.Windows.GetClientRect(Handle, RC);
  GetWindowRect(Handle, RW);
  MapWindowPoints(0, Handle, RW, 2);
  OffsetRect(RC, -RW.Left, -RW.Top);
  ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
  { Draw border in non-client area }
  OffsetRect(RW, -RW.Left, -RW.Top);
  try
    Canvas.Handle := DC;
    Canvas.Pen.Color := ColorMap.FrameTopLeftOuter;
    Canvas.Brush.Color := ColorMap.Color;
    Canvas.Rectangle(RW);
    Canvas.Pen.Color := ColorMap.MenuColor;
    Canvas.MoveTo(1,1);
    Canvas.LineTo(1, Height - 1);
  finally
    IntersectClipRect(Canvas.Handle, RW.Left, RW.Top, RW.Right, RW.Bottom);
    Canvas.Handle := 0;
  end;
end;

{ TThemedAddRemoveItem }

procedure TThemedAddRemoveItem.CalcBounds;
const
  CheckMarkStates: array[Boolean] of TThemedMenu = (tmPopupCheckDisabled, tmPopupCheckNormal);
  SubMenuStates: array[Boolean] of TThemedMenu = (tmPopupSubMenuDisabled, tmPopupSubMenuNormal);
var
  DC: HDC;
  LFont: HFONT;
  LBounds: TRect;
  LImageSize: TPoint;
  LHeight, LWidth, Offset: Integer;
  LGlyphSize, LGutterSize, LSeparatorSize, LSubMenuGlyphSize: TSize;
  LCheckMargins, LGutterMargins, LMenuItemMargins, LSeparatorMargins, LSubMenuGlyphMargins: TElementMargins;
  TempFont: TFont;
  PPI: Integer;
  LStyle: TCustomStyleServices;
begin
  inherited;
  DC := CreateCompatibleDC(0);
  PPI := GetDevicePPI;
  LStyle := StyleServices;
  if PPI <> Screen.PixelsPerInch then
    TempFont := TFont.Create
  else
    TempFont := nil;
  try
    if TempFont <> nil then
    begin
      TempFont.Assign(Screen.MenuFont);
      TempFont.Height := MulDiv(TempFont.Height, PPI, Screen.PixelsPerInch);
      LFont := SelectObject(DC, TempFont.Handle);
    end
    else
      LFont := SelectObject(DC, Screen.MenuFont.Handle);
    try
      if TempFont <> nil then
        Font.Assign(TempFont)
      else
        Font.Assign(Screen.MenuFont);
      inherited;
      LHeight := 0;
      LWidth := 0;

      // Check/Glyph
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(CheckMarkStates[Enabled]), esActual, LGlyphSize, PPI);
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(CheckMarkStates[Enabled]), emContent, LCheckMargins, PPI);

      // Gutter
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(tmPopupGutter), esActual, LGutterSize);
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(tmPopupGutter), emSizing, LGutterMargins);

      // Menu item
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(tmPopupItemNormal), emSizing, LMenuItemMargins);
      LStyle.GetElementSize(DC, LStyle.GetElementDetails(SubMenuStates[Enabled]), esActual, LSubMenuGlyphSize, PPI);
      LStyle.GetElementMargins(DC, LStyle.GetElementDetails(SubMenuStates[Enabled]), emContent, LSubMenuGlyphMargins, PPI);

      // Add "visible on actionbar" checkmark
      Inc(LWidth, LGlyphSize.cx);
      Inc(LWidth, LCheckMargins.cxLeftWidth + LCheckMargins.cxRightWidth);
      Offset := LWidth;
      FVisibleCheckRect := Rect(0, 0,
        LCheckMargins.cxLeftWidth + LGlyphSize.cx + LCheckMargins.cxRightWidth, Offset);

      // Calculate check/glyph size
      LImageSize := GetImageSize;
      if LImageSize.Y > LGlyphSize.cy then
        LGlyphSize.cy := LImageSize.Y;
      if LImageSize.X > LGlyphSize.cx then
        LGlyphSize.cx := LImageSize.X;
      Inc(LHeight, LGlyphSize.cy);
      Inc(LWidth, LGlyphSize.cx);

      // Add margins for check/glyph
      Inc(LHeight, LCheckMargins.cyTopHeight + LCheckMargins.cyBottomHeight);
      Inc(LWidth, LCheckMargins.cxLeftWidth + LCheckMargins.cxRightWidth);
      FCheckRect := Rect(Offset, 0,
        LGlyphSize.cx + LCheckMargins.cxRightWidth + LCheckMargins.cxRightWidth + Offset,
        LGlyphSize.cy + LCheckMargins.cyBottomHeight + LCheckMargins.cyBottomHeight);

      // Add size and margins for gutter
      Inc(LWidth, LGutterMargins.cxLeftWidth);
      FGutterRect.Left := LWidth;
      FGutterRect.Right := FGutterRect.Left + LGutterSize.cx;
      Inc(LWidth, LGutterSize.cx + LGutterMargins.cxRightWidth);

      // Add margins for menu item
      Inc(LWidth, LMenuItemMargins.cxLeftWidth + LMenuItemMargins.cxRightWidth);
      Offset := LWidth - TextBounds.Left - LMenuItemMargins.cxRightWidth;
      LBounds := TextBounds;
      OffsetRect(LBounds, Offset, -1);
      TextBounds := LBounds;

      // Add size of potential submenu glyph
      Inc(LWidth, LSubMenuGlyphSize.cx);
      Inc(LWidth, LSubMenuGlyphMargins.cxLeftWidth);
      Inc(LWidth, LSubMenuGlyphMargins.cxRightWidth);
      // Add Width of menu item to FSubMenuGlyphRect before using
      with LSubMenuGlyphMargins, LSubMenuGlyphSize do
        FSubMenuGlyphRect := Rect(-cxRightWidth - cx, (Height - cy) div 2,
          -cxRightWidth, ((Height - cy) div 2) + cy);

      // Add margins for menu short cut
      if ActionClient <> nil then
      begin
        Canvas.Font := Screen.MenuFont;
        DoDrawText(DC, ActionClient.ShortCutText, LBounds, DT_CALCRECT or DT_NOCLIP);
      end
      else
        LBounds := ShortCutBounds;
      Offset := FSubMenuGlyphRect.Left - LBounds.Right -
        LMenuItemMargins.cxRightWidth - LSubMenuGlyphMargins.cxLeftWidth;
      OffsetRect(LBounds, Offset, 0);
      // Add Width of menu item to ShortCutBounds before using
      ShortCutBounds := LBounds;
      Inc(LWidth, LMenuItemMargins.cxLeftWidth + LMenuItemMargins.cxRightWidth);

      // Adjust size if separator
      if Separator then
      begin
        LStyle.GetElementSize(DC, LStyle.GetElementDetails(tmPopupSeparator), esActual, LSeparatorSize);
        LStyle.GetElementMargins(DC, LStyle.GetElementDetails(tmPopupSeparator), emSizing, LSeparatorMargins);

        LHeight := LSeparatorSize.cy + LSeparatorMargins.cyBottomHeight;
        LWidth := LSeparatorSize.cx;
        FSeparatorHeight := LSeparatorSize.cy;
      end;

      FGutterRect.Top := 0;
      FGutterRect.Bottom := LHeight;
      SetBounds(Left, Top,
        LWidth + TextBounds.Right - TextBounds.Left + ShortCutBounds.Right - ShortCutBounds.Left,
        LHeight);
    finally
      SelectObject(DC, LFont);
    end;
  finally
    DeleteDC(DC);
    if TempFont <> nil then
      TempFont.Free;
  end;
end;

procedure TThemedAddRemoveItem.DoDrawText(DC: HDC; const Text: string;
  var Rect: TRect; Flags: Integer);

const
  MenuStates: array[Boolean] of TThemedMenu = (tmPopupItemDisabled, tmPopupItemNormal);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
  LStyle: TCustomStyleServices;
begin
  // get format and color
  LStyle := StyleServices;
  LFormats := TTextFormatFlags(Flags);
  if Selected and Enabled then
    LDetails := LStyle.GetElementDetails(tmPopupItemHot)
  else
    LDetails := LStyle.GetElementDetails(MenuStates[Enabled or ActionBar.DesignMode]);

  if not LStyle.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := ActionBar.ColorMap.FontColor;

  // tweak text
  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  // Draw menu item text
  LStyle.DrawText(DC, LDetails, LCaption, Rect, LFormats, LColor);
end;


procedure TThemedAddRemoveItem.DrawBackground(var PaintRect: TRect);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  FPaintRect := PaintRect;
  if LStyle.IsSystemStyle then
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupBackground), FPaintRect);
  LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupGutter), FGutterRect);
end;

procedure TThemedAddRemoveItem.DrawGlyph(const Location: TPoint);
const
  CheckMarkBkgStates: array[Boolean] of TThemedMenu = (tmPopupCheckBackgroundDisabled, tmPopupCheckBackgroundNormal);
  CheckMarkStates: array[Boolean] of TThemedMenu = (tmPopupCheckDisabled, tmPopupCheckNormal);
var
  ImageList: TCustomImageList;
  LDisabled: Boolean;
  PPI: Integer;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  PPI := GetDevicePPI;
  if (Assigned(ActionClient) and not ActionClient.HasGlyph) and
     ((Action is TCustomAction) and TCustomAction(Action).Checked) then
  begin
    if IsChecked then
    begin
      LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckMarkBkgStates[Enabled]), FCheckRect);
      LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckMarkStates[Enabled]), FCheckRect, nil, PPI);
    end;
  end
  else
  begin
    ImageList := FindImageList(False, LDisabled, ActionClient.ImageIndex);
    if Assigned(ImageList) and (ActionClient.ImageIndex <> -1) then
      with FCheckRect do
        ImageList.Draw(Canvas, Left + ((Right - Left - ImageList.Width) div 2),
          Top + ((Bottom - Top - ImageList.Height) div 2), ActionClient.ImageIndex,
          Enabled or LDisabled);
  end;

  if IsActionVisible then
  begin
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckMarkBkgStates[Enabled]), FVisibleCheckRect);
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(CheckMarkStates[Enabled]), FVisibleCheckRect, nil, PPI);
  end;
end;

procedure TThemedAddRemoveItem.DrawSeparator(const Offset: Integer);
var
  LRect: TRect;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  LRect := Rect(FGutterRect.Right + 1, 0, Width, FSeparatorHeight);
                                                                               
  LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupSeparator), LRect);
end;

procedure TThemedAddRemoveItem.DrawText(var Rect: TRect; var Flags: Cardinal;
  Text: string);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmPopupItemDisabled, tmPopupItemNormal);
var
  LRect: TRect;
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  // Draw menu highlight
  if Selected then
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tmPopupItemHot), FPaintRect);

  // Draw menu item text
  if (Parent is TCustomActionBar) and (not ActionBar.PersistentHotkeys) then
    Text := FNoPrefix;
  Canvas.Font := Screen.MenuFont;
  LRect := FPaintRect;

  DoDrawText(Canvas.Handle, Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);
  OffsetRect(LRect, Rect.Left,
    ((FPaintRect.Bottom - FPaintRect.Top) - (LRect.Bottom - LRect.Top)) div 2);
  DoDrawText(Canvas.Handle, Text, LRect, Flags);

  // Draw shortcut
  if ShowShortCut and ((ActionClient <> nil) and not ActionClient.HasItems) then
  begin
    Flags := DrawTextBiDiModeFlags(DT_RIGHT);
    LRect := System.Types.Rect(ShortCutBounds.Left, LRect.Top, ShortCutBounds.Right, LRect.Bottom);
    OffsetRect(LRect, Width, 0);
    DoDrawText(Canvas.Handle, ActionClient.ShortCutText, LRect, Flags);
  end;
end;

procedure TThemedAddRemoveItem.DrawUnusedEdges;
begin
  // This style of menu item does not have unused edges
end;

function TThemedAddRemoveItem.GetImageSize: TPoint;
var
  LImages: TCustomImageList;
begin
  Result := Point(0,0);
  if not Assigned(ActionClient) then Exit;

  LImages := FindImageList(not ActionClient.ActionClients.SmallIcons);
  if LImages <> nil then
  begin
    if not ActionClient.ActionClients.SmallIcons and
       (LImages.Width = LImages.Height) and (LImages.Width < 32) then
      Result := Point(32, 32)
    else
      Result := Point(LImages.Width, LImages.Height);
  end;
end;

{ TThemedToolScrollBtn }

procedure TThemedToolScrollBtn.DrawBackground(var PaintRect: TRect);
const
  PressedStates: array[Boolean] of TThemedToolBar = (ttbButtonHot, ttbButtonCheckedHot);
var
  LStyle: TCustomStyleServices;
begin
  LStyle := StyleServices;
  if Enabled and MouseInControl then
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(PressedStates[FState = bsDown]), PaintRect)
  else
    LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(ttbButtonNormal), PaintRect);
end;

end.
