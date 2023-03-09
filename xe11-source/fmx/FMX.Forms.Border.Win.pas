{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Forms.Border.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, System.Generics.Collections, Winapi.Messages, Winapi.Windows, FMX.Forms, FMX.Types,
  FMX.Forms.Border, FMX.Controls, FMX.Menus, FMX.Graphics, FMX.Helpers.Win;

type
  TWinBuffer = class;

  TWinWindowBorder = class(TStyledWindowBorder)
  public type
    TWindowElement = (Caption, LeftBorder, RightBorder, BottomBorder, ClientArea, TopLeftSizingArea, TopSizingArea,
                      TopRightSizingArea, LeftSizingArea, RightSizingArea, BottomleftSizingArea, BottomSizingArea,
                      BottomRightSizingArea);
  private type
    /// <summary>The element of nonclient window area.</summary>
    TNCElement = (Caption, LeftBorder, RightBorder, BottomBorder);
  private
    FBuffers: array [TNCElement] of TWinBuffer;
    FNCClick: Boolean;
    FUpdating: Integer;
    { Style objects }
    FBottomBorder: TControl;
    FLeftBorder: TControl;
    FRightBorder: TControl;
    FSavedCaptionPadding: TRectF;
    { Old style structure }
    FSavedIconPos: TPointF;
    FSavedTitleMargins: TPointF;
    { Painting }
    FCurrentCanvas: TCanvas;
    FIcon: THandle;
    { Menu }
    FMenuBar: TMenuBar;
    FMenuMap: TDictionary<TFmxHandle, TMenuItem>;
    procedure UpdateButtonsState;
    procedure UpdateBuffersSizes;
    function GetBuffer(const AIndex: TNCElement): TWinBuffer;
    function GetWndSize: TSize; inline;
    function GetWnd: HWnd; inline;
    { Window measurements in px }
    function GetWndElementBounds(const AIndex: TWindowElement): TRect;
    /// <summary>Returns thickness of form border which overlap screen workarea in maximized state (px).</summary>
    function GetWndOverlapScreenFormThickness: TRect;
    /// <summary>
    ///   Returns the thickness of the sizing border around the perimeter of a window that can be resized in px.
    /// </summary>
    function GetWndDefaultFrameResizeThickness: TRect;
    function GetWndClientMargins: TRect;
    { Window measurements in dp }
    function GetTopOffset: Single;
    function GetDefaultFrameThickness: TRectF;
    { Rendering frame }
    function GetScale: Single;
    procedure InvalidateRegion;
    procedure RecreateRegion;
    procedure Paint;
    procedure PaintCaption(const DC: HDC);
    procedure PaintBottom(const DC: HDC);
    procedure PaintLeft(const DC: HDC);
    procedure PaintRight(const DC: HDC);
    { WinAPI Messages}
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCActivate(var Message: TWMNCActivate); message WM_NCACTIVATE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMNCMouseLeave(var Message: TMessage); message WM_NCMOUSELEAVE;
    procedure WMNCAddUpdateRect(var Message: TMessage); message WM_NCADDUPDATERECT;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
  protected
    procedure EnableChanged; override;
    { Size }
    function GetFormSize: TSizeF; override;
    procedure Resize; override;
    function GetClientMargins: TRectF; override;
    procedure RealignMenuBar;
    procedure WindowFrameCouldChanged;
    { Style }
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure StyleChanged; override;
    function GetStyleLookup: string; override;
    function IsOldStyleStructure: Boolean;
    { Rendering }
    procedure DoPaint; virtual; deprecated 'Override PaintNC instead.';
    procedure PaintNC; virtual;
    procedure Invalidate; override;
    procedure DoAddUpdateRect(R: TRectF); override;
    function GetCanvas: TCanvas; override;
    { Intermediate buffers of nonclient area }
    property LeftBuffer: TWinBuffer index TNCElement.LeftBorder read GetBuffer;
    property RightBuffer: TWinBuffer index TNCElement.RightBorder read GetBuffer;
    property CaptionBuffer: TWinBuffer index TNCElement.Caption read GetBuffer;
    property BottomBuffer: TWinBuffer index TNCElement.BottomBorder read GetBuffer;
  public
    constructor Create(const AForm: TCommonCustomForm); override;
    destructor Destroy; override;
    { Menu }
    procedure CreateOSMenu(const AMenu: IItemsContainer);
    function HandleExists(const Handle: TFmxHandle): Boolean;
    procedure RemoveHandle(const Handle: TFmxHandle);
    { Updating }
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;
  public
    property NCClick: Boolean read FNCClick;
    /// <summary>Returns current form scale.</summary>
    property Scale: Single read GetScale;
    /// <summary>The window size (nonclient + client areas) in px.</summary>
    property WndSize: TSize read GetWndSize;
    /// <summary>The window handle of owned form.</summary>
    property Wnd: HWnd read GetWnd;
    /// <summary>
    ///   Returns client area margins in px, based on current style (px).
    /// </summary>
    /// <remarks>
    ///   It doesn't account maximaze window state.
    /// </remarks>
    property WndClientMargins: TRect read GetWndClientMargins;
    /// <summary>
    ///   Returns specified element bounds with the clipping area taken into accout in the maximize state of the form (px).
    /// </summary>
    property WndElementBounds[const AIndex: TWindowElement]: TRect read GetWndElementBounds;
  end;
  TWindowBorderWin = TWinWindowBorder;

  TWinBuffer = class
  private
    FBitmap: TBitmap;
    FBitmapDC: HDC;
    FBitmapHandle: HBITMAP;
    FData: Pointer;
    function GetScale: Single;
    procedure SetScale(const Value: Single);
    function GetLogicalWidth: Single;
    function GetLogicalHeight: Single;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetCanvas: TCanvas;
  protected
    procedure FillBitmapInfo(var BitmapInfo: TBitmapInfo; const Width, Height: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(const ASize: TSize {px});
    function IsAllocated: Boolean;
    procedure CopyFMXBitmapToWinBitmap;
    procedure Paint(const DC: HDC; const ADestPos: TPoint; const ASrcPoint: TPoint);
  public
    property LogicalWidth: Single read GetLogicalWidth;
    property LogicalHeight: Single read GetLogicalHeight;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Canvas: TCanvas read GetCanvas;
    property BitmapDC: HDC read FBitmapDC;
    property Size: TSize write SetSize;
    property Scale: Single read GetScale write SetScale;
  end;

function CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder; deprecated 'Use TWinWindowBorder.Create instead';
function WMNCMessages(AForm: TCommonCustomForm; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;

implementation

uses
  System.UITypes, System.Variants, System.Math, System.StrUtils, System.SysUtils, Winapi.UxTheme, FMX.ActnList,
  FMX.Platform.Win, FMX.Utils, FMX.ImgList, FMX.Consts;

type
  TWinWindowBorderEx = class(TWinWindowBorder)
  private
    FMouseLeaveTimer: TTimer;
    { Handlers }
    procedure MouseLeaveCheckHandler(Sender: TObject);
  protected
    { WinAPI Messages}
    procedure WMNCMouseMove(var Message: TWMNCMouseMove); message WM_NCMOUSEMOVE;
  public
    constructor Create(const AForm: TCommonCustomForm); override;
    destructor Destroy; override;
  end;

{ Window }

function SystemHitTest(wParam: WPARAM): Boolean;
begin
  Result := wParam in [HTCAPTION, HTTOP, HTBOTTOM, HTLEFT, HTRIGHT, HTCLIENT, HTTOPLEFT, HTTOPRIGHT, HTBOTTOMLEFT,
                       HTBOTTOMRIGHT, HTSYSMENU];
end;

function ScaleRect(const ARect: TRectF; const AScale: Single): TRect; inline;
begin
  Result.Left := Trunc(ARect.Left * AScale);
  Result.Top := Trunc(ARect.Top * AScale);
  Result.Right := Trunc(ARect.Right * AScale);
  Result.Bottom := Trunc(ARect.Bottom * AScale);
end;

{ Nonclient area }

type
  TOpenControl = class(TControl);
  TOpenCommonCustomForm = class(TCommonCustomForm);

procedure TWinWindowBorder.BeginUpdate;
begin
  Inc(FUpdating);
end;

constructor TWinWindowBorder.Create(const AForm: TCommonCustomForm);
var
   Element: TNCElement;
begin
  inherited Create(AForm);
  for Element := Low(TNCElement) to High(TNCElement) do
    FBuffers[Element] := TWinBuffer.Create;

  FMenuMap := TDictionary<TFmxHandle, TMenuItem>.Create;
end;

destructor TWinWindowBorder.Destroy;
var
  Element: TNCElement;
begin
  for Element := Low(TNCElement) to High(TNCElement) do
    FreeAndNil(FBuffers[Element]);
  FreeAndNil(FMenuMap);
  inherited;
end;

procedure TWinWindowBorder.CreateOSMenu(const AMenu: IItemsContainer);

  procedure InsertItems(Parent: TFmxObject; Item: TMenuItem);
  var
    I: Integer;
    Child: IItemsContainer;
    NewItem: TMenuItem;
    Handle: TFmxHandle;
  begin
    if not Item.Visible then
      Exit;
    NewItem := TMenuItem.Create(Self);
    NewItem.BeginUpdate;
    try
      NewItem.ImageIndex := Item.ImageIndex;
      NewItem.Bitmap := Item.Bitmap;
      NewItem.Text := Item.Text;
      NewItem.ShortCut := Item.ShortCut;
      NewItem.OnClick := Item.OnClick;
      NewItem.Enabled := Item.Enabled;
      NewItem.IsChecked := Item.IsChecked;
      NewItem.Parent := Parent;
      NewItem.Action := Item.Action;
      NewItem.Tag := Item.Tag;
    finally
      NewItem.EndUpdate;
    end;
    Handle := TFmxHandle(NewItem);
    (Item as INativeControl).Handle := Handle;
    FMenuMap.Add(Handle, NewItem);
    if Supports(Item, IItemsContainer, Child) then
      for I := 0 to Child.GetItemsCount - 1 do
        if Child.GetItem(I) is TMenuItem then
          InsertItems(NewItem, TMenuItem(Child.GetItem(I)));
  end;

  function GetAttachedImageList(const AMenu: IItemsContainer): TCustomImageList;
  var
    Glyph: IGlyph;
  begin
    if Supports(AMenu, IGlyph, Glyph) and (Glyph.Images is TCustomImageList) then
      Result := TCustomImageList(Glyph.Images)
    else
      Result := nil;
  end;

var
  I: Integer;
begin
  BeginUpdate;
  try
    if FMenuBar = nil then
    begin
      FMenuBar := TMenuBar.Create(Self);
      FMenuBar.StyleLookup := 'mainmenustyle';  // do not localize
      FMenuBar.Parent := Self;
      FMenuBar.Align := TAlignLayout.None;
      FMenuBar.Stored := False;
      FMenuBar.Locked := True;
      FMenuBar.Images := GetAttachedImageList(AMenu);
    end;
    for I := 0 to AMenu.GetItemsCount - 1 do
      if (AMenu.GetItem(I) is TMenuItem) and TMenuItem(AMenu.GetItem(I)).Visible then
        InsertItems(FMenuBar, TMenuItem(AMenu.GetItem(I)));
  finally
    EndUpdate;
  end;
  WindowFrameCouldChanged;
end;

procedure TWinWindowBorder.RealignMenuBar;
var
  NewBounds: TRectF;
begin
  if FMenuBar = nil then
    Exit;

  NewBounds.Left := LeftBuffer.LogicalWidth;
  NewBounds.Top := CaptionBuffer.LogicalHeight - FMenuBar.Height;
  NewBounds.Width := CaptionBuffer.LogicalWidth - RightBuffer.LogicalWidth - LeftBuffer.LogicalWidth;
  NewBounds.Height := FMenuBar.Height;
  FMenuBar.SetBounds(NewBounds.Left, NewBounds.Top, NewBounds.Width, NewBounds.Height);
end;

function TWinWindowBorder.GetBuffer(const AIndex: TNCElement): TWinBuffer;
begin
  Result := FBuffers[AIndex];
end;

function TWinWindowBorder.GetCanvas: TCanvas;
begin
  Result := FCurrentCanvas;
end;

function TWinWindowBorder.GetTopOffset: Single;
var
  Display: TDisplay;
  WndWorkArea: TRect;
  WndBounds: TRect;
begin
  Assert(Screen <> nil);

  Display := Screen.DisplayFromForm(Form);
  WndWorkArea := Display.PhysicalWorkarea;
  WndBounds := WindowHandleToPlatform(Form.Handle).WndBounds;

  if WndBounds.Top < WndWorkArea.Top then
    Result := (WndWorkArea.Top - WndBounds.Top) / Scale
  else
    Result := 0;
end;

function TWinWindowBorder.GetWnd: HWnd;
begin
  Result := WindowHandleToPlatform(Form.Handle).Wnd;
end;

function TWinWindowBorder.GetWndClientMargins: TRect;
begin
  Result := ScaleRect(ClientMargins, Scale);
end;

function TWinWindowBorder.GetWndSize: TSize;
begin
  Result := WindowHandleToPlatform(Form.Handle).WndBounds.Size;
end;

function TWinWindowBorder.HandleExists(const Handle: TFmxHandle): Boolean;
begin
  Result := FMenuMap.ContainsKey(Handle);
end;

procedure TWinWindowBorder.RemoveHandle(const Handle: TFmxHandle);
begin
  BeginUpdate;
  try
    TFmxObject(Pointer(Handle)).Free
  finally
    EndUpdate;
    FMenuMap.Remove(Handle);
  end;
end;

procedure TWinWindowBorder.Paint;
var
  I: Integer;
  Control: TControl;
begin
  if not IsSupported then
    Exit;

  if GetCanvas.BeginScene then
  try
    GetCanvas.Clear(0);
    for I := 0 to ChildrenCount - 1 do
    begin
      if not (Children[I] is TControl) then
        Continue;

      Control := TControl(Children[I]);
      if Control.Visible or ((csDesigning in ComponentState) and not Control.Locked) then
      begin
        if Control.UpdateRect.IsEmpty then
          Continue;
        TOpenControl(Control).PaintInternal;
      end;
    end;
  finally
    GetCanvas.EndScene;
  end;
end;

procedure TWinWindowBorder.UpdateButtonsState;

  function DefineCorrectiveContentPadding: TRectF;
  var
    ContainerRect: TRectF;
    LClientMargins: TRectF;
    LFrameThickness: TRectF;
  begin
    { Define current position of content }
    ContainerRect := FCaptionContainerObject.AbsoluteRect;
    ContainerRect.Inflate(-FSavedCaptionPadding.Left, -FSavedCaptionPadding.Top, -FSavedCaptionPadding.Right, -FSavedCaptionPadding.Bottom);
    LClientMargins := GetClientMargins;
    LFrameThickness := GetDefaultFrameThickness;

    Result.Left := Max(LClientMargins.Left, LFrameThickness.Left) - ContainerRect.Left;
    Result.Right := Max(LClientMargins.Right, LFrameThickness.Right) - (Form.Width - ContainerRect.Right);
    Result.Top := GetTopOffset;
  end;

  procedure AdjustStyleControlLocationForOldStyle;
  begin
    if Form.WindowState = TWindowState.wsMaximized then
    begin
      if FMinObject <> nil then
      begin
        FMinObject.Margins.Top := 0;
        if FMinObject.AbsoluteRect.Top < GetTopOffset then
        begin
          FMinObject.Margins.Top := GetTopOffset - FMinObject.AbsoluteRect.Top;
          if FMinObject.AbsoluteRect.Bottom > CaptionBuffer.Height then
            FMinObject.Margins.Top := FMinObject.Margins.Top - (FMinObject.AbsoluteRect.Bottom - CaptionBuffer.Height);
        end;
      end;
      if FResObject <> nil then
      begin
        FResObject.Visible := True;
        FResObject.Margins.Top := 0;
        if (FMaxObject <> nil) and FMaxObject.Visible then
          FResObject.Position.X := FMaxObject.Position.X;
        if FResObject.AbsoluteRect.Top < GetTopOffset then
        begin
          FResObject.Margins.Top := GetTopOffset - FResObject.AbsoluteRect.Top;
          if FResObject.AbsoluteRect.Bottom > CaptionBuffer.Height then
            FResObject.Margins.Top := FResObject.Margins.Top - (FResObject.AbsoluteRect.Bottom - CaptionBuffer.Height);
        end;
      end;
      if FMaxObject <> nil then
      begin
        FMaxObject.Margins.Top := 0;
        FMaxObject.Visible := False;
        if FMaxObject.AbsoluteRect.Top < GetTopOffset then
        begin
          FMaxObject.Margins.Top := GetTopOffset - FMaxObject.AbsoluteRect.Top;
          if FMaxObject.AbsoluteRect.Bottom > CaptionBuffer.Height then
            FMaxObject.Margins.Top := FMaxObject.Margins.Top - (FMaxObject.AbsoluteRect.Bottom - CaptionBuffer.Height);
        end;
      end;
      if FCloseObject <> nil then
      begin
        FCloseObject.Margins.Top := 0;
        if FCloseObject.AbsoluteRect.Top < GetTopOffset then
        begin
          FCloseObject.Margins.Top := GetTopOffset - FCloseObject.AbsoluteRect.Top;
          if FCloseObject.AbsoluteRect.Bottom > CaptionBuffer.Height then
            FCloseObject.Margins.Top := FCloseObject.Margins.Top - (FCloseObject.AbsoluteRect.Bottom - CaptionBuffer.Height);
        end;
        // only on most right button
        FCloseObject.Margins.Right := RightBuffer.Width;
      end;
      if FTitleObject <> nil then
      begin
        FTitleObject.Margins.Top := FSavedTitleMargins.Y;
        if FTitleObject.AbsoluteRect.Top < GetTopOffset then
        begin
          FTitleObject.Margins.Top := FSavedTitleMargins.Y + (GetTopOffset - FTitleObject.AbsoluteRect.Top);
          if FTitleObject.AbsoluteRect.Bottom > CaptionBuffer.Height then
            FTitleObject.Margins.Top := FTitleObject.Margins.Top - (FTitleObject.AbsoluteRect.Bottom - CaptionBuffer.Height);
        end;
        FTitleObject.Margins.Left := FSavedTitleMargins.X + LeftBuffer.Width;
        if (FIconObject <> nil) and (FTitleObject <> nil) then
          FTitleObject.Margins.Left := FTitleObject.Margins.Left + FIconObject.Width;
      end;
      if FIconObject <> nil then
      begin
        FIconObject.Position.Point := FSavedIconPos;
        if FIconObject.AbsoluteRect.Top < GetTopOffset then
        begin
          FIconObject.Position.Y := FSavedIconPos.Y + (GetTopOffset - FIconObject.AbsoluteRect.Top);
          if FIconObject.AbsoluteRect.Bottom > CaptionBuffer.Height then
            FIconObject.Position.Y := FSavedIconPos.Y - (FIconObject.AbsoluteRect.Bottom - CaptionBuffer.Height);
        end;
        FIconObject.Position.X := FSavedIconPos.X + LeftBuffer.Width;
      end;
    end
    else
    begin
      if FMinObject <> nil then
        FMinObject.Margins.Top := 0;
      if FMaxObject <> nil then
      begin
        FMaxObject.Visible := True;
        FMaxObject.Margins.Top := 0;
        if (FResObject <> nil) and FResObject.Visible then
          FMaxObject.Position.X := FResObject.Position.X;
      end;
      if FResObject <> nil then
      begin
        FResObject.Visible := False;
        FResObject.Margins.Top := 0;
      end;
      if FCloseObject <> nil then
      begin
        FCloseObject.Margins.Top := 0;
        FCloseObject.Margins.Right := 0;
      end;
      if FTitleObject <> nil then
      begin
        FTitleObject.Margins.Top := FSavedTitleMargins.Y;
        FTitleObject.Margins.Left := FSavedTitleMargins.X;
        if (FIconObject <> nil) and (FTitleObject <> nil) then
          FTitleObject.Margins.Left := FTitleObject.Margins.Left + FIconObject.Width;
      end;
      if FIconObject <> nil then
        FIconObject.Position.Point := FSavedIconPos;
    end;
  end;

  procedure AdjustStyleControlLocationForNewStyle;
  var
    CorrectivePadding: TRectF;
  begin
    if Form.WindowState = TWindowState.wsMaximized then
    begin
      if FCaptionContainerObject <> nil then
      begin
        CorrectivePadding := DefineCorrectiveContentPadding;
        FCaptionContainerObject.Padding.Left := FSavedCaptionPadding.Left + DefineCorrectiveContentPadding.Left;
        FCaptionContainerObject.Padding.Right := FSavedCaptionPadding.Right + DefineCorrectiveContentPadding.Right;
        FCaptionContainerObject.Padding.Top := FSavedCaptionPadding.Top + DefineCorrectiveContentPadding.Top;
      end;
      if FResObject <> nil then
      begin
        FResObject.Visible := True;
        if (FMaxObject <> nil) and FMaxObject.Visible then
          FResObject.Position.X := FMaxObject.Position.X;
      end;
      if FMaxObject <> nil then
        FMaxObject.Visible := False;
    end
    else
    begin
      if FCaptionContainerObject <> nil then
        FCaptionContainerObject.Padding.Rect := FSavedCaptionPadding;
      if FMaxObject <> nil then
      begin
        FMaxObject.Visible := True;
        if (FResObject <> nil) and FResObject.Visible then
          FMaxObject.Position.X := FResObject.Position.X;
      end;
      if FResObject <> nil then
        FResObject.Visible := False;
    end;
  end;

var
  Caption: ICaption;
begin
  if Supports(FTitleObject, ICaption, Caption) then
    Caption.Text := Form.Caption;

  if IsOldStyleStructure then
    { Old style structure }
    AdjustStyleControlLocationForOldStyle
  else
    { New style structure }
    AdjustStyleControlLocationForNewStyle;
end;

procedure TWinWindowBorder.PaintCaption(const DC: HDC);

  function GetFormIcon: THandle;
  begin
    Result := SendMessage(Wnd, WM_GETICON, ICON_SMALL, 0);
    if Result = 0 then
      Result := SendMessage(Wnd, WM_GETICON, ICON_BIG, 0);
    if Result = 0 then
      Result := LoadIcon(MainInstance, 'MAINICON');
    if Result = 0 then
      Result := LoadIcon(0, IDI_APPLICATION);
  end;

  procedure DrawSystemIconTo(const ABuffer: TWinBuffer);
  const
    SystemIconSize = 16; // dp
  var
    IconSize: TSize;
    DestRect: TRect;
    IconRect: TRect;
  begin
    if FIconObject = nil then
      Exit;

    if FIcon = 0 then
      FIcon := GetFormIcon;
    if FIcon <> 0 then
    begin
      IconSize := TSizeF.Create(SystemIconSize * Scale, SystemIconSize * Scale).Round;
      if IsOldStyleStructure then
      begin
        IconRect.TopLeft := (FIconObject.Position.Point * Scale).Round;
        IconRect.Size := IconSize;
      end
      else
      begin
        IconRect := TRect.Create(TPoint.Zero, IconSize.Width, IconSize.Height);
        DestRect := ScaleRect(FIconObject.AbsoluteRect, Scale);
        IconRect := TRectF(IconRect).PlaceInto(DestRect).Round;
      end;
      DrawIconEx(ABuffer.BitmapDC, IconRect.Left, IconRect.Top, FIcon, IconSize.Width, IconSize.Height, 0, 0, DI_NORMAL);
    end;
  end;

var
  SaveLeftWidth, SaveRightWidth: Single;
begin
  FCurrentCanvas := CaptionBuffer.Canvas;
  FResourceLink.Position.Point := TPointF.Zero;
  if FLeftBorder <> nil then
  begin
    SaveLeftWidth := FLeftBorder.Width;
    FLeftBorder.Width := LeftBuffer.LogicalWidth;
  end
  else
    SaveLeftWidth := 0;
  if FRightBorder <> nil then
  begin
    SaveRightWidth := FRightBorder.Width;
    FRightBorder.Width := RightBuffer.LogicalWidth;
  end
  else
    SaveRightWidth := 0;
  try
    TOpenControl(FResourceLink).RecalcUpdateRect;
    Paint;
    CaptionBuffer.CopyFMXBitmapToWinBitmap;
    DrawSystemIconTo(CaptionBuffer);
    CaptionBuffer.Paint(DC, TPoint.Zero, TPoint.Zero);
  finally
    if FLeftBorder <> nil then
      FLeftBorder.Width := SaveLeftWidth;
    if FRightBorder <> nil then
      FRightBorder.Width := SaveRightWidth;
  end;
end;

procedure TWinWindowBorder.PaintLeft(const DC: HDC);
var
  SaveWidth: Single;
begin
  FCurrentCanvas := LeftBuffer.Canvas;
  FResourceLink.Position.Point := TPointF.Zero;
  if FLeftBorder = nil then
    SaveWidth := 0
  else
  begin
    SaveWidth := FLeftBorder.Width;
    FLeftBorder.Width := LeftBuffer.LogicalWidth;
  end;

  try
    TOpenControl(FResourceLink).RecalcUpdateRect;
    Paint;
    LeftBuffer.CopyFMXBitmapToWinBitmap;
    LeftBuffer.Paint(DC, TPoint.Create(0, CaptionBuffer.Height), TPoint.Create(0, CaptionBuffer.Height));
  finally
    if FLeftBorder <> nil then
      FLeftBorder.Width := SaveWidth;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}
procedure TWinWindowBorder.PaintNC;

  procedure ExcludeClientArea(const ADC: HDC);
  begin
    ExcludeClipRect(ADC, LeftBuffer.Width, CaptionBuffer.Height, WndSize.Width - RightBuffer.Width,
                         WndSize.Height - BottomBuffer.Height);
  end;

var
  DC: HDC;
  SaveDisableAlign: Boolean;
begin
  DoPaint; // Invoke for backward compatibility

  if not CaptionBuffer.IsAllocated or (Wnd = 0) or IsUpdating or (FResourceLink = nil) then
    Exit;

  DC := GetWindowDC(Wnd);
  BeginUpdate;
  try
    SaveDisableAlign := FDisableAlign;
    FDisableAlign := True;
    try
      try
        UpdateButtonsState;

        ExcludeClientArea(DC);
        PaintCaption(DC);
        PaintBottom(DC);
        PaintLeft(DC);
        PaintRight(DC);
      finally
        FResourceLink.Position.Point := TPointF.Zero;
        FCurrentCanvas := CaptionBuffer.Canvas;
        ReleaseDC(Wnd, DC);
      end;
    finally
      FDisableAlign := SaveDisableAlign;
    end;
  finally
    EndUpdate;
  end;
end;
{$WARN SYMBOL_DEPRECATED DEFAULT}

procedure TWinWindowBorder.PaintRight(const DC: HDC);
var
  SaveWidth: Single;
begin
  FCurrentCanvas := RightBuffer.Canvas;
  FResourceLink.Position.Point := TPointF.Create(-FormSize.Width + RightBuffer.Width / Scale, 0);
  if FRightBorder = nil then
    SaveWidth := 0
  else
  begin
    SaveWidth := FRightBorder.Width;
    FRightBorder.Width := RightBuffer.LogicalWidth;
  end;

  try
    TOpenControl(FResourceLink).RecalcUpdateRect;
    Paint;
    RightBuffer.CopyFMXBitmapToWinBitmap;
    RightBuffer.Paint(DC, TPoint.Create(WndSize.Width - RightBuffer.Width, CaptionBuffer.Height),
                          TPoint.Create(0, CaptionBuffer.Height));
  finally
    if FRightBorder <> nil then
      FRightBorder.Width := SaveWidth;
  end;
end;

procedure TWinWindowBorder.PaintBottom(const DC: HDC);
var
  SaveHeight: Single;
begin
  FCurrentCanvas := BottomBuffer.Canvas;
  FResourceLink.Position.Point := TPointF.Create(0, -FormSize.Height + BottomBuffer.Height / Scale);
  if FBottomBorder <> nil then
  begin
    SaveHeight := FBottomBorder.Height;
    FBottomBorder.Height := BottomBuffer.LogicalHeight;
  end
  else
    SaveHeight := 0;
  try
    TOpenControl(FResourceLink).RecalcUpdateRect;
    Paint;
    BottomBuffer.CopyFMXBitmapToWinBitmap;
    BottomBuffer.Paint(DC, TPoint.Create(LeftBuffer.Width, WndSize.Height - BottomBuffer.Height),
                           TPoint.Create(LeftBuffer.Width, 0));
  finally
    if FBottomBorder <> nil then
      FBottomBorder.Height := SaveHeight;
  end;
end;

procedure TWinWindowBorder.DoPaint;
begin
end;

procedure TWinWindowBorder.EnableChanged;
begin
  inherited;
  Form.RecreateOSMenu;
  RecreateRegion;
end;

procedure TWinWindowBorder.EndUpdate;
begin
  if IsUpdating then
    Dec(FUpdating);
end;

procedure TWinWindowBorder.InvalidateRegion;

  procedure HideStyleObjects;
  var
    I: Integer;
    Control: IControl;
  begin
    for I := 0 to FResourceLink.ChildrenCount - 1 do
      if Supports(FResourceLink.Children[I], IControl, Control) then
        Control.Visible := False;
  end;

  procedure ShowStyleObjects;
  var
    I: Integer;
    Control: IControl;
  begin
    for I := 0 to FResourceLink.ChildrenCount - 1 do
      if Supports(FResourceLink.Children[I], IControl, Control) then
        Control.Visible := True;
  end;

var
  SaveDisableAlign: Boolean;
begin
  if not CaptionBuffer.IsAllocated then
    Exit;
  if Wnd = 0 then
    Exit;
  if FResourceLink = nil then
    Exit;
  if IsUpdating then
    Exit;

  BeginUpdate;
  try
    SaveDisableAlign := FDisableAlign;
    FDisableAlign := True;
    try
      try
        if FMaskObject <> nil then
        begin
          HideStyleObjects;
          FMaskObject.Visible := True;
          FMaskObject.SetBounds(0, 0, FResourceLink.Width, FResourceLink.Height);
        end;
        // paint caption
        FCurrentCanvas := CaptionBuffer.Canvas;
        FResourceLink.Position.Point := TPointF.Zero;
        TOpenControl(FResourceLink).RecalcUpdateRect;
        Paint;
        // paint left
        FCurrentCanvas := LeftBuffer.Canvas;
        FResourceLink.Position.Point := TPointF.Create(0, -CaptionBuffer.LogicalHeight);
        TOpenControl(FResourceLink).RecalcUpdateRect;
        Paint;
        // paint right
        FCurrentCanvas := RightBuffer.Canvas;
        FResourceLink.Position.Point := TPointF.Create(-FResourceLink.Width + RightBuffer.LogicalWidth, -CaptionBuffer.LogicalHeight);
        TOpenControl(FResourceLink).RecalcUpdateRect;
        Paint;
        // paint bottom
        FCurrentCanvas := BottomBuffer.Canvas;
        FResourceLink.Position.Point := TPointF.Create(0, -FResourceLink.Height + BottomBuffer.LogicalHeight);
        TOpenControl(FResourceLink).RecalcUpdateRect;
        Paint;
      finally
        if FMaskObject <> nil then
        begin
          ShowStyleObjects;
          FMaskObject.Visible := False;
        end;
        FResourceLink.Position.Point := TPointF.Zero;
        FCurrentCanvas := CaptionBuffer.Canvas;
      end;
    finally
      FDisableAlign := SaveDisableAlign;
    end;
  finally
    EndUpdate;
  end;
end;

function TWinWindowBorder.IsOldStyleStructure: Boolean;
begin
  Result := FCaptionContainerObject = nil;
end;

function TWinWindowBorder.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

type
  TWinRegionBuilder = class
  public
    class var Rts: array [0..5000] of TRect;
  private
    FRegion: HRgn;
    function CreateRegionFromBitmap(const ABitmap: TBitmap; const AOriginPoint: TPoint; const ADestRect: TRect): HRgn; overload;
    function CreateRegionDataFromBitmap(const ABitmap: TBitmap; const AOriginPoint: TPoint; const ADestRect: TRect; var RgnData: PRgnData): Integer;
  public
    constructor Create(const ARect: TRect);
    function AddFromBitmap(const ABitmap: TBitmap; const AOriginPoint: TPoint; const ADestRect: TRect): TWinRegionBuilder; overload;
    function Build: HRgn;
  end;

procedure TWinWindowBorder.RecreateRegion;
var
  Region: HRgn;
  RegionBuilder: TWinRegionBuilder;
  CaptionRect: TRect;
  BottomRect: TRect;
  LeftRect: TRect;
  RightRect: TRect;
begin
  if csDesigning in Form.ComponentState then
    Exit;

  if Form.Border.IsSupported then
  begin
    // paint to FM FBuffer
    InvalidateRegion;

    // Creating window region
    RegionBuilder := TWinRegionBuilder.Create(WndElementBounds[TWindowElement.ClientArea]);
    try
      CaptionRect := WndElementBounds[TWindowElement.Caption];
      BottomRect := WndElementBounds[TWindowElement.BottomBorder];
      LeftRect := WndElementBounds[TWindowElement.LeftBorder];
      RightRect := WndElementBounds[TWindowElement.RightBorder];
      Region := RegionBuilder.AddFromBitmap(CaptionBuffer.FBitmap, CaptionRect.TopLeft, CaptionRect)
                             .AddFromBitmap(BottomBuffer.FBitmap, TPoint.Create(BottomRect.Left, 0), BottomRect)
                             .AddFromBitmap(LeftBuffer.FBitmap, TPoint.Create(LeftRect.Left, 0), LeftRect)
                             .AddFromBitmap(RightBuffer.FBitmap, TPoint.Create(0, 0), RightRect)
                             .Build;
    finally
      RegionBuilder.Free;
    end;

    // Sets region based on style
    SetWindowRgn(Wnd, Region, False);
    PaintNC;
  end
  else
    // Reset region to default value
    SetWindowRgn(Wnd, 0, True);
end;

procedure TWinWindowBorder.Resize;
begin
  if not IsSupported then
    Exit;

  Realign;
  UpdateBuffersSizes;
  RealignMenuBar;
  RecreateRegion;
end;

procedure TWinWindowBorder.UpdateBuffersSizes;
var
  LClientMargins: TRect;
begin
  LClientMargins := WndClientMargins;
  CaptionBuffer.Scale := Scale;
  CaptionBuffer.Size := TSize.Create(WndSize.Width, LClientMargins.Top);
  BottomBuffer.Scale := Scale;
  BottomBuffer.Size := TSize.Create(WndSize.Width, LClientMargins.Bottom);
  LeftBuffer.Scale := Scale;
  LeftBuffer.Size := TSize.Create(LClientMargins.Left, WndSize.Height);
  RightBuffer.Scale := Scale;
  RightBuffer.Size := TSize.Create(LClientMargins.Right, WndSize.Height);
  FCurrentCanvas := CaptionBuffer.Canvas;
end;

function TWinWindowBorder.GetClientMargins: TRectF;
begin
  Result := inherited;
  if FMenuBar <> nil then
    Result.Top := Result.Top + FMenuBar.Height;
end;

function TWinWindowBorder.GetDefaultFrameThickness: TRectF;
begin
  Result := ScaleRect(GetWndOverlapScreenFormThickness, 1 / Scale);
end;

function TWinWindowBorder.GetWndDefaultFrameResizeThickness: TRect;
begin
  Result.Left := GetSystemMetricsForWindow(SM_CXSIZEFRAME, Wnd);
  Result.Top := GetSystemMetricsForWindow(SM_CYSIZEFRAME, Wnd);

  Result.Right := Result.Left;
  Result.Bottom := Result.Top;
end;

function TWinWindowBorder.GetWndElementBounds(const AIndex: TWindowElement): TRect;

  function ShouldInflateInMaximizedState: Boolean;
  begin
    Result := (Form.WindowState = TWindowState.wsMaximized) and (Form.BorderStyle <> TFmxFormBorderStyle.None);
  end;

  function GetFrameIndent: TRect;
  var
    Display: TDisplay;
    WA: TRect;
    WndBounds: TRect;
  begin
    Display := Screen.DisplayFromForm(Form);
    WA := Display.PhysicalWorkarea;
    WndBounds := WindowHandleToPlatform(Form.Handle).WndBounds;
    if WndBounds.Top < WA.Top then
      Result.Top := WA.Top - WndBounds.Top
    else
      Result.Top := 0;
    if WndBounds.Left < WA.Left then
      Result.Left := WA.Left - WndBounds.Left
    else
      Result.Left := 0;
    if WndBounds.Bottom > WA.Bottom then
      Result.Bottom := WndBounds.Bottom - WA.Bottom
    else
      Result.Bottom := 0;
    if WndBounds.Right > WA.Right then
      Result.Right := WndBounds.Right - WA.Right
    else
      Result.Right := 0;
  end;

  function CalculateClientAreaBounds: TRect;
  var
    LClientMargins: TRect;
    FrameIndent: TRect;
  begin
    LClientMargins := WndClientMargins;
    if ShouldInflateInMaximizedState then
    begin
      FrameIndent := GetFrameIndent;
      Result := TRect.Create(Max(LClientMargins.Left, FrameIndent.Left), Max(LClientMargins.Top, FrameIndent.Top),
                                 WndSize.Width - Max(LClientMargins.Right, FrameIndent.Right),
                                 WndSize.Height - Max(LClientMargins.Bottom, FrameIndent.Bottom))
    end
    else
      Result := TRect.Create(LClientMargins.Left, LClientMargins.Top,
                             WndSize.Width - LClientMargins.Right, WndSize.Height - LClientMargins.Bottom);
  end;

var
  FrameIndent: TRect;
  LClientMargins: TRect;
  ClientAreaBounds: TRect;
  SizingFrame: TRect;
begin
  LClientMargins := WndClientMargins;
  ClientAreaBounds := CalculateClientAreaBounds;
  case AIndex of
    { General elements }
    TWindowElement.Caption:
      Result := TRect.Create(0, 0, WndSize.Width, ClientAreaBounds.Top);
    TWindowElement.BottomBorder:
      Result := TRect.Create(0, ClientAreaBounds.Bottom, WndSize.Width, WndSize.Height);
    TWindowElement.LeftBorder:
      Result := TRect.Create(0, LClientMargins.Top, LClientMargins.Left, WndSize.Height - LClientMargins.Bottom);
    TWindowElement.RightBorder:
      Result := TRect.Create(WndSize.Width - LClientMargins.Right, LClientMargins.Top,
                             WndSize.Width, WndSize.Height - LClientMargins.Bottom);
    TWindowElement.ClientArea:
      Result := ClientAreaBounds;
    { Sizer }
    TWindowElement.TopLeftSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(0, 0, SizingFrame.Left, SizingFrame.Top);
    end;
    TWindowElement.TopSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(SizingFrame.Left, 0, WndSize.Width - SizingFrame.Right, SizingFrame.Top);
    end;
    TWindowElement.TopRightSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(WndSize.Width - SizingFrame.Right, 0, WndSize.Width, SizingFrame.Top);
    end;
    TWindowElement.LeftSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(0, SizingFrame.Top, SizingFrame.Right, WndSize.Height - SizingFrame.Bottom);
    end;
    TWindowElement.RightSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(WndSize.Width - SizingFrame.Right, SizingFrame.Top, WndSize.Width, WndSize.Height - SizingFrame.Bottom);
    end;
    TWindowElement.BottomleftSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(0, WndSize.Height - SizingFrame.Bottom, SizingFrame.Left, WndSize.Height);
    end;
    TWindowElement.BottomSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(SizingFrame.Left, WndSize.Height - SizingFrame.Bottom, WndSize.Width - SizingFrame.Right, WndSize.Height);
    end;
    TWindowElement.BottomRightSizingArea:
    begin
      SizingFrame := GetWndDefaultFrameResizeThickness;
      Result := TRect.Create(WndSize.Width - SizingFrame.Right, WndSize.Height - SizingFrame.Bottom, WndSize.Width, WndSize.Height);
    end;
  end;

  if ShouldInflateInMaximizedState then
  begin
    FrameIndent := GetFrameIndent;
    case AIndex of
      TWindowElement.Caption:
        Result.Inflate(-FrameIndent.Left, -FrameIndent.Top, -FrameIndent.Right, 0);
      TWindowElement.BottomBorder:
        Result.Inflate(-FrameIndent.Left, 0, -FrameIndent.Right, -Min(FrameIndent.Bottom, Result.Height));
      TWindowElement.LeftBorder:
        Result.Inflate(-FrameIndent.Left, 0, 0, 0);
      TWindowElement.RightBorder:
        Result.Inflate(0, 0, -FrameIndent.Right, 0);
    end;
  end;
end;

function TWinWindowBorder.GetWndOverlapScreenFormThickness: TRect;
var
  Display: TDisplay;
  WA: TRect;
  WndBounds: TRect;
begin
  if Form.WindowState = TWindowState.wsMaximized then
  begin
    Display := Screen.DisplayFromForm(Form);
    WA := Display.PhysicalWorkarea;
    WndBounds := WindowHandleToPlatform(Form.Handle).WndBounds;
    Result.Left := WA.Left - WndBounds.Left;
    Result.Right := WndBounds.Right - WA.Right;
    Result.Top := WA.Top - WndBounds.Top;
    Result.Bottom := WndBounds.Bottom - WA.Bottom;
  end
  else
    Result := TRect.Empty;
end;

function TWinWindowBorder.GetFormSize: TSizeF;
begin
  Result := WindowHandleToPlatform(Form.Handle).Bounds.Size;
end;

function TWinWindowBorder.GetScale: Single;
begin
  Result := Form.Handle.Scale;
end;

function TWinWindowBorder.GetStyleLookup: string;
begin
  case Form.BorderStyle of
    TFmxFormBorderStyle.ToolWindow,
    TFmxFormBorderStyle.SizeToolWin:
      Result := 'toolwindowstyle';
  else
    Result := 'windowborderstyle';
  end;
end;

procedure TWinWindowBorder.WindowFrameCouldChanged;
var
  R: TRect;
begin
  // Frame size depends on buffers sizes.
  UpdateBuffersSizes;

  if InitThemeLibrary and not UseThemes then
    SetWindowLong(Wnd, GWL_STYLE, GetWindowLong(Wnd, GWL_STYLE) and not WS_CAPTION);
  GetWindowRect(Wnd, R);
  SetWindowPos(Wnd, 0, R.Left, R.Top, R.Width, R.Height, SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
               SWP_NOSENDCHANGING or SWP_FRAMECHANGED);
  RealignMenuBar;
end;

procedure TWinWindowBorder.WMNCActivate(var Message: TWMNCActivate);
begin
  Message.Result := 1;
end;

procedure TWinWindowBorder.WMNCAddUpdateRect(var Message: TMessage);

  procedure ProcessNCUpdateMessages;
  var
    Msg: TMsg;
  begin
    while PeekMessage(Msg, Wnd, WM_NCADDUPDATERECT, WM_NCADDUPDATERECT, PM_REMOVE) do
      if Msg.Message = WM_QUIT then
      begin
        { Repost WM_QUIT messages }
        PostQuitMessage(Msg.wParam);
        Break;
      end;
  end;

begin
  ProcessNCUpdateMessages;
  PaintNC;
end;

procedure TWinWindowBorder.WMNCCalcSize(var Message: TWMNCCalcSize);

  function GetBorderSize: TRect;
  var
    DefaultFrameSize: TRect;
  begin
    Result.Left := LeftBuffer.Width;
    Result.Top := CaptionBuffer.Height;
    Result.Right := RightBuffer.Width;
    Result.Bottom := BottomBuffer.Height;
    if (Form.BorderStyle <> TFmxFormBorderStyle.None) and (Form.WindowState = TWindowState.wsMaximized) then
    begin
      DefaultFrameSize := GetWndOverlapScreenFormThickness;
      Result.Left := Max(Result.Left, DefaultFrameSize.Left);
      Result.Right := Max(Result.Right, DefaultFrameSize.Right);
      Result.Bottom := Max(Result.Bottom, DefaultFrameSize.Bottom);
    end;
  end;

  procedure InflateOnStyledBorderSize(var AParams: PNCCalcSizeParams);
  var
    StyledBorderThickness: TRect;
  begin
    StyledBorderThickness := GetBorderSize;

    Inc(AParams.rgrc[0].Left, StyledBorderThickness.Left);
    Inc(AParams.rgrc[0].Top, StyledBorderThickness.Top);
    Dec(AParams.rgrc[0].Right, StyledBorderThickness.Right);
    Dec(AParams.rgrc[0].Bottom, StyledBorderThickness.Bottom);
  end;

var
  Params: PNCCalcSizeParams;
  LMessage: TMessage;
begin
  if Message.CalcValidRects then
  begin
    Params := Message.CalcSize_Params;
    InflateOnStyledBorderSize(Params);
    Message.Result := 0;
  end
  else
  begin
    LMessage := TMessage(Message);
    Message.Result := DefWindowProc(Wnd, LMessage.Msg, LMessage.WParam, LMessage.LParam);
  end;
end;

procedure TWinWindowBorder.WMNCHitTest(var Message: TWMNCHitTest);

  function IsFormResizable: Boolean;
  begin
    Result := Form.BorderStyle in [TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin];
  end;

  function DefineFormElement(const AHitObject: IControl): Integer; overload;
  var
    StyleName: string;
  begin
    StyleName := AHitObject.GetObject.StyleName;
    if SameText(StyleName, 'client') then
      Result := HTCLIENT
    else if SameText(StyleName, 'icon') then
      Result := HTSYSMENU
    else if SameText(StyleName, 'left') and IsFormResizable then
      Result := HTLEFT
    else if SameText(StyleName, 'right') and IsFormResizable then
      Result := HTRIGHT
    else if SameText(StyleName, 'top') and IsFormResizable then
      Result := HTTOP
    else if SameText(StyleName, 'bottom') and IsFormResizable then
      Result := HTBOTTOM
    else if SameText(StyleName, 'caption') then
      Result := HTCAPTION
    else
      Result := HTBORDER;
  end;

  function DefineFormElement(const AHitPoint: TPointF {dp}; const ABorderMargins: TRectF): Integer; overload;
  var
    TopLeftRect, TopRightRect,
    BottomLeftRect, BottomRightRect,
    TopRect, LeftRect, RightRect, BottomRect: TRectF;
    BorderScene: IScene;
    HitPoint: TPointF;
  begin
    TopLeftRect := TRectF.Create(0, 0, ABorderMargins.Left, ABorderMargins.Top);
    TopRightRect := TRectF.Create(Form.Width - ABorderMargins.Right, 0, Form.Width, ABorderMargins.Top);
    BottomLeftRect := TRectF.Create(0, Form.Height - ABorderMargins.Bottom, ABorderMargins.Left, Form.Height);
    BottomRightRect := TRectF.Create(Form.Width - ABorderMargins.Right, Form.Height - ABorderMargins.Bottom, Form.Width, Form.Height);
    TopRect := TRectF.Create(TopLeftRect.Right, 0, TopRightRect.Left, ABorderMargins.Top);
    LeftRect := TRectF.Create(0, TopLeftRect.Bottom, ABorderMargins.Left, BottomLeftRect.Top);
    RightRect := TRectF.Create(Form.Width - ABorderMargins.Right, TopRightRect.Bottom, Form.Width, BottomRightRect.Top);
    BottomRect := TRectF.Create(BottomLeftRect.Right, Form.Height - ABorderMargins.Bottom, BottomRightRect.Left, Form.Height);

    if IsFormResizable  then
    begin
      if Supports(Self, IScene, BorderScene) then
        HitPoint := BorderScene.ScreenToLocal(AHitPoint)
      else
        HitPoint := AHitPoint;

      if TopLeftRect.Contains(HitPoint) then
        Result := HTTOPLEFT
      else if TopRightRect.Contains(HitPoint) then
        Result := HTTOPRIGHT
      else if BottomLeftRect.Contains(HitPoint) then
        Result := HTBOTTOMLEFT
      else if BottomRightRect.Contains(HitPoint) then
        Result := HTBOTTOMRIGHT
      else if LeftRect.Contains(HitPoint) then
        Result := HTLEFT
      else if RightRect.Contains(HitPoint) then
        Result := HTRIGHT
      else if BottomRect.Contains(HitPoint) then
        Result := HTBOTTOM
      else if TopRect.Contains(HitPoint) then
        Result := HTTOP
      else
        Result := 0;
    end
    else
      Result := HTCLIENT;
  end;

  function TryDefineFormSizerElement(const AScreenPoint: TPointF {dp}; var AElementID: LRESULT): Boolean;
  const
    SizingElementsID: array [TWindowElement.TopLeftSizingArea..TWindowElement.BottomRightSizingArea] of LRESULT =
      (HTTOPLEFT, HTTOP, HTTOPRIGHT, HTLEFT, HTRIGHT, HTBOTTOMLEFT, HTBOTTOM, HTBOTTOMRIGHT);
  var
    BorderScene: IScene;
    HitPoint: TPointF;
    WndHitPoint: TPoint;
    WindowElement: TWindowElement;
  begin
    if Supports(Self, IScene, BorderScene) then
      HitPoint := BorderScene.ScreenToLocal(AScreenPoint) // dp
    else
      HitPoint := AScreenPoint; // dp

    WndHitPoint := (HitPoint * Scale).Round; // px
    for WindowElement := TWindowElement.TopLeftSizingArea to TWindowElement.BottomRightSizingArea do
      if WndElementBounds[WindowElement].Contains(WndHitPoint) then
      begin
        AElementID := SizingElementsID[WindowElement];
        Exit(True);
      end;

    Result := False;
  end;

var
  HitObject: IControl;
  ScreenPoint: TPointF;
  ElementID: LRESULT;
begin
  ScreenPoint := PxToDp(Message.Pos); // dp
  HitObject := ObjectAtPoint(ScreenPoint);
  if HitObject = nil then
    ElementID := DefineFormElement(ScreenPoint, ClientMargins)
  else
    ElementID := DefineFormElement(HitObject);

  // When we define window element based on style, it really depends on style structure.
  // For example, if style doesn't have named "left" or "right" style element, than window will not allow to resize.
  // To reduce the dependency on the style, we additionally try to define the resizing area based on default
  if IsFormResizable and (ElementID in [HTBORDER, HTCAPTION]) then
    TryDefineFormSizerElement(ScreenPoint, ElementID);
  Message.Result := ElementID;
end;

procedure TWinWindowBorder.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  ScreenPoint: TPointF;
  LMessage: TMessage;
begin
  if SystemHitTest(Message.HitTest) then
  begin
    LMessage := TMessage(Message);
    Message.Result := DefWindowProc(Wnd, LMessage.Msg, LMessage.WParam, LMessage.LParam);
    Exit;
  end;

  ScreenPoint := PxToDp(TPoint.Create(Message.XCursor, Message.YCursor)); // dp
  FNCClick := True;
  MouseDown(TMouseButton.mbLeft, [ssLeft], ScreenPoint.X, ScreenPoint.Y);
end;

procedure TWinWindowBorder.WMNCLButtonUp(var Message: TWMNCLButtonUp);
var
  ScreenPoint: TPointF;
  LMessage: TMessage;
begin
  FNCClick := False;
  if SystemHitTest(Message.HitTest) then
  begin
    LMessage := TMessage(Message);
    Message.Result := DefWindowProc(Wnd, LMessage.Msg, LMessage.WParam, LMessage.LParam);
    Exit;
  end;

  ScreenPoint := PxToDp(TPoint.Create(Message.XCursor, Message.YCursor)); // dp
  MouseUp(TMouseButton.mbLeft, [ssLeft], ScreenPoint.X, ScreenPoint.Y);
end;

procedure TWinWindowBorder.WMNCMouseLeave(var Message: TMessage);
begin
  MouseLeave;
  MouseUp(TMouseButton.mbLeft, [], -1, -1);
end;

procedure TWinWindowBorder.WMNCMouseMove(var Message: TWMNCMouseMove);
var
  ScreenPoint: TPointF;
  LMessage: TMessage;
begin
  ScreenPoint := PxToDp(TPoint.Create(Message.XCursor, Message.YCursor)); // dp
  if SystemHitTest(Message.HitTest) then
  begin
    MouseLeave;
    LMessage := TMessage(Message);
    Message.Result := DefWindowProc(Wnd, LMessage.Msg, LMessage.WParam, LMessage.LParam);
    Exit;
  end;
  MouseMove([], ScreenPoint.X, ScreenPoint.Y);
end;

procedure TWinWindowBorder.WMNCPaint(var Message: TWMNCPaint);
begin
  PaintNC;
end;

procedure TWinWindowBorder.WMLButtonUp(var Message: TWMLButtonUp);
var
  ScreenPoint: TPointF;
  FormPoint: TPoint;
begin
  FormPoint := TPoint.Create(Message.XPos, Message.YPos); // px
  Winapi.Windows.ClientToScreen(Wnd, FormPoint); // px
  ScreenPoint := PxToDp(FormPoint); // dp
  FNCClick := False;
  MouseUp(TMouseButton.mbLeft, KeysToShiftState(Message.Keys), ScreenPoint.X, ScreenPoint.Y);
end;

procedure TWinWindowBorder.WMMouseLeave(var Message: TMessage);
begin
  MouseLeave;
end;

procedure TWinWindowBorder.WMMouseMove(var Message: TWMMouseMove);
var
  ScreenPoint: TPointF;
  FormPoint: TPoint;
begin
  FormPoint := TPoint.Create(Message.XPos, Message.YPos); // px
  Winapi.Windows.ClientToScreen(Wnd, FormPoint); // px
  ScreenPoint := PxToDp(FormPoint); // dp
  MouseMove(KeysToShiftState(Message.Keys), ScreenPoint.X, ScreenPoint.Y);
end;

procedure TWinWindowBorder.Invalidate;
begin
  DoAddUpdateRect(TRectF.Create(0, 0, Form.Width, Form.Height));
end;

procedure TWinWindowBorder.DoAddUpdateRect(R: TRectF);
var
  Msg: TMsg;
begin
  if not Form.IsHandleAllocated or IsUpdating then
    Exit;

  if not PeekMessage(Msg, Wnd, WM_NCADDUPDATERECT, WM_NCADDUPDATERECT, PM_NOREMOVE) then
    PostMessage(Wnd, WM_NCADDUPDATERECT, 0, 0);
end;

procedure TWinWindowBorder.ApplyStyle;
begin
  inherited;
  { It's only for old style border structure }
  if FIconObject <> nil then
    FSavedIconPos := FIconObject.Position.Point;
  if FTitleObject <> nil then
    FSavedTitleMargins := FTitleObject.Margins.Rect.TopLeft;

  FLeftBorder := TControl(FResourceLink.FindStyleResource('left'));
  FRightBorder := TControl(FResourceLink.FindStyleResource('right'));
  FBottomBorder := TControl(FResourceLink.FindStyleResource('bottom'));
  if FCaptionContainerObject <> nil then
    FSavedCaptionPadding := FCaptionContainerObject.Padding.Rect;

  Resize;
  WindowFrameCouldChanged;
end;

procedure TWinWindowBorder.FreeStyle;
begin
  FSavedIconPos := TPointF.Zero;
  FSavedTitleMargins := TPointF.Zero;

  if FCaptionContainerObject <> nil then
    FCaptionContainerObject.Padding.Rect := FSavedCaptionPadding;

  FBottomBorder := nil;
  FRightBorder := nil;
  FLeftBorder := nil;
  inherited;
end;

procedure TWinWindowBorder.StyleChanged;
begin
  inherited;
  if Form.Border.IsSupported then
    TOpenCommonCustomForm(Form).ResizeHandle
  else if Form.IsHandleAllocated then
    RecreateRegion;

  if FMenuBar <> nil then
  begin
    FMenuBar.NeedStyleLookup;
    FMenuBar.ApplyStyleLookup;
  end;
end;

function WMNCMessages(AForm: TCommonCustomForm; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;

  function IsStyledBorderSupported: Boolean;
  var
    WindowBorder: TWinWindowBorder;
  begin
    Result := AForm.IsHandleAllocated and (AForm.BorderStyle <> TFmxFormBorderStyle.None)
              and not (csDesigning in AForm.ComponentState)
              and (AForm.Border.WindowBorder is TWinWindowBorder);
    if Result then
    begin
      WindowBorder := TWinWindowBorder(AForm.Border.WindowBorder);
      Result := (WindowBorder <> nil) and WindowBorder.IsSupported and AForm.Border.IsSupported;
    end;
  end;

var
  Message: TMessage;
begin
  Assert(AForm <> nil);

  if IsStyledBorderSupported then
  begin
    Message.Msg := uMsg;
    Message.WParam := wParam;
    Message.LParam := lParam;
    Message.Result := 0;
    AForm.Border.WindowBorder.Dispatch(Message);
    Result := Message.Result;
  end
  else
    Result := DefWindowProc(FormToHWND(AForm), uMsg, wParam, lParam);
end;

function CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
begin
  Result := TWinWindowBorderEx.Create(AForm);
end;

{ TWinBuffer }

constructor TWinBuffer.Create;
begin
  FBitmap := TBitmap.Create;
end;

destructor TWinBuffer.Destroy;
begin
  FreeAndNil(FBitmap);
  if FBitmapDC <> 0 then
    DeleteDC(FBitmapDC);
  if FBitmapHandle <> 0 then
    DeleteObject(FBitmapHandle);
  FData := nil;
  inherited;
end;

procedure TWinBuffer.FillBitmapInfo(var BitmapInfo: TBitmapInfo; const Width, Height: Integer);
begin
  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;
  BitmapInfo.bmiHeader.biCompression := BI_RGB;
  BitmapInfo.bmiHeader.biWidth := Width;
  if BitmapInfo.bmiHeader.biWidth <= 0 then
    BitmapInfo.bmiHeader.biWidth := 1;
  BitmapInfo.bmiHeader.biHeight := -Height;
  if BitmapInfo.bmiHeader.biHeight >= 0 then
    BitmapInfo.bmiHeader.biHeight := -1;
end;

function TWinBuffer.GetCanvas: TCanvas;
begin
  Result := FBitmap.Canvas;
end;

function TWinBuffer.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;

function TWinBuffer.GetLogicalHeight: Single;
begin
  Result := FBitmap.Height / FBitmap.BitmapScale;
end;

function TWinBuffer.GetLogicalWidth: Single;
begin
  Result := FBitmap.Width / FBitmap.BitmapScale;
end;

function TWinBuffer.GetScale: Single;
begin
  Result := FBitmap.BitmapScale;
end;

function TWinBuffer.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

function TWinBuffer.IsAllocated: Boolean;
begin
  Result := FData <> nil;
end;

procedure TWinBuffer.Paint(const DC: HDC; const ADestPos: TPoint; const ASrcPoint: TPoint);
begin
  BitBlt(DC, ADestPos.X, ADestPos.Y, Width, Height, FBitmapDC, ASrcPoint.X, ASrcPoint.Y, SRCCOPY);
end;

procedure TWinBuffer.SetScale(const Value: Single);
begin
  FBitmap.BitmapScale := Value;
end;

procedure TWinBuffer.SetSize(const ASize: TSize);
var
  BitmapInfo: TBitmapInfo;
begin
  if FBitmapDC <> 0 then
    DeleteDC(FBitmapDC);
  if FBitmapHandle <> 0 then
    DeleteObject(FBitmapHandle);
  FData := nil;

  FBitmap.SetSize(ASize.Width, ASize.Height);
  FillBitmapInfo(BitmapInfo, FBitmap.Width, FBitmap.Height);
  try
    FBitmapHandle := CreateDIBSection(0, BitmapInfo, DIB_RGB_COLORS, FData, 0, 0);
    FBitmapDC := CreateCompatibleDC(0);
  except
    raise;
  end;
  if FBitmapHandle = 0 then
    RaiseLastOSError;
  if SelectObject(FBitmapDC, FBitmapHandle) = 0 then
    RaiseLastOSError;
end;

procedure TWinBuffer.CopyFMXBitmapToWinBitmap;
var
  Map: TBitmapData;
  I: Integer;
begin
  if FBitmap.Map(TMapAccess.Read, Map) then
  try
    if Map.Pitch = Width * 4 then
      Move(Map.Data^, FData^, Width * Height * 4)
    else
      for I := 0 to Height - 1 do
        Move(PAlphaColorArray(Map.Data)[I * (Map.Pitch div 4)], PAlphaColorArray(FData)[I * Width], Width * 4);
  finally
    FBitmap.Unmap(Map);
  end;
end;

{ TWinRegionBuilder }

function TWinRegionBuilder.AddFromBitmap(const ABitmap: TBitmap; const AOriginPoint: TPoint; const ADestRect: TRect): TWinRegionBuilder;
var
  NewRegion: HRgn;
begin
  Assert(ABitmap <> nil);

  Result := Self;

  if not ADestRect.IsEmpty then
  begin
    NewRegion := CreateRegionFromBitmap(ABitmap, AOriginPoint, ADestRect);
    CombineRgn(FRegion, FRegion, NewRegion, RGN_OR);
    DeleteObject(NewRegion);
  end;
end;

function TWinRegionBuilder.Build: HRgn;
begin
  Result := FRegion;
end;

constructor TWinRegionBuilder.Create(const ARect: TRect);
begin
  FRegion := CreateRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

function TWinRegionBuilder.CreateRegionDataFromBitmap(const ABitmap: TBitmap; const AOriginPoint: TPoint; const ADestRect: TRect; var RgnData: PRgnData): Integer;
var
  Y, X, I1: integer;
  C: TAlphaColorRec;
  Count: Integer;
  Map: TBitmapData;
  RegionRect: TRect;
  SourceRight: Integer;
  SourceBottom: Integer;
  SourceWidth: Integer;
  SourceHeight: Integer;
begin
  Assert(AOriginPoint.X >= 0);
  Assert(AOriginPoint.Y >= 0);

  if ABitmap.IsEmpty then
    Exit(0);

  Assert(AOriginPoint.X < ABitmap.Width);
  Assert(AOriginPoint.Y < ABitmap.Height);

  SourceWidth := Min(ADestRect.Width, ABitmap.Width - AOriginPoint.X);
  SourceHeight := Min(ADestRect.Height, ABitmap.Height - AOriginPoint.Y);
  SourceRight := AOriginPoint.X + SourceWidth;
  SourceBottom := AOriginPoint.Y + SourceHeight;
  Count := 0;
  if ABitmap.Map(TMapAccess.Read, Map) then
  try
    for Y := AOriginPoint.Y to SourceBottom - 1 do
    begin
      X := AOriginPoint.X - 1;
      while X < SourceRight do
      begin
        { Counting transparent pixels }
        repeat
          Inc(X);
          if X >= SourceRight then
            Break;
          C := TAlphaColorRec(Map.GetPixel(X, Y));
        until not (C.A = 0);

        if X >= SourceRight then
          Break;

        { Counting nontransparent pixels}
        I1 := X;
        repeat
          Inc(I1);
          if I1 >= SourceRight then
            Break;
          C := TAlphaColorRec(Map.GetPixel(I1, Y));
        until (C.A = 0);

        { Definition region rect }
        if X <> I1 then
        begin
          RegionRect := Rect(X, Y, I1, Y + 1); // In ABitmap coordinate system
          RegionRect.Offset(-AOriginPoint.X, -AOriginPoint.Y);
          RegionRect.Offset(ADestRect.TopLeft);
          if RegionRect.IntersectsWith(ADestRect) then
          begin
            Rts[Count] := RegionRect;
            Inc(Count);
          end;
        end;
        X := I1;
      end;
    end;
  finally
    ABitmap.Unmap(Map);
  end;
  { Make Region data }
  Result := Count * SizeOf(TRect);
  GetMem(Rgndata, SizeOf(TRgnDataHeader) + Result);
  RgnData^.rdh.dwSize := SizeOf(TRgnDataHeader);
  RgnData^.rdh.iType := RDH_RECTANGLES;
  RgnData^.rdh.nCount := Count;
  RgnData^.rdh.nRgnSize := 0;
  RgnData^.rdh.rcBound := ABitmap.Bounds;
  { Update New Region }
  Move(Rts, RgnData^.Buffer, Result);
  Result := SizeOf(TRgnDataHeader) + Count * SizeOf(TRect);
end;

function TWinRegionBuilder.CreateRegionFromBitmap(const ABitmap: TBitmap; const AOriginPoint: TPoint; const ADestRect: TRect): HRgn;
var
  RgnData: PRgnData;
  Size: Integer;
begin
  RgnData := nil;
  Size := CreateRegionDataFromBitmap(ABitmap, AOriginPoint, ADestRect, RgnData);
  if RgnData <> nil then
    RgnData.rdh.rcBound := ADestRect;
  Result := ExtCreateRegion(nil, Size, RgnData^);
  if RgnData <> nil then
    FreeMem(RgnData, Size);
end;

{ TWinWindowBorderEx }

constructor TWinWindowBorderEx.Create(const AForm: TCommonCustomForm);
begin
  inherited;
  FMouseLeaveTimer := TTimer.Create(nil);
  FMouseLeaveTimer.Interval := 10;
  FMouseLeaveTimer.OnTimer := MouseLeaveCheckHandler;
end;

destructor TWinWindowBorderEx.Destroy;
begin
  FreeAndNil(FMouseLeaveTimer);
  inherited;
end;

procedure TWinWindowBorderEx.MouseLeaveCheckHandler(Sender: TObject);
var
  CursorPixel: TPoint;
begin
  GetCursorPos(&CursorPixel);
  if WindowFromPoint(CursorPixel) <> Wnd then
  begin
    FMouseLeaveTimer.Enabled := False;
    MouseLeave;
  end;
end;

procedure TWinWindowBorderEx.WMNCMouseMove(var Message: TWMNCMouseMove);
begin
  FMouseLeaveTimer.Enabled := True;
  inherited;
end;

end.
