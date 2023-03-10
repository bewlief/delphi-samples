
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdActns, ExtActns, ActnList, ToolWin, ActnMan, ActnCtrls,
  ActnMenus, StdCtrls, ComCtrls, ImgList, StdActnMenus,
  XPStyleActnCtrls, XPActnCtrls;

type
  TForm1 = class(TForm)
    ActionManager1: TActionManager;
    ActionMainMenuBar1: TActionMainMenuBar;
    RichEdit1: TRichEdit;
    ImageList1: TImageList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    RichEditBold1: TRichEditBold;
    RichEditItalic1: TRichEditItalic;
    RichEditUnderline1: TRichEditUnderline;
    RichEditStrikeOut1: TRichEditStrikeOut;
    RichEditBullets1: TRichEditBullets;
    RichEditAlignLeft1: TRichEditAlignLeft;
    RichEditAlignRight1: TRichEditAlignRight;
    RichEditAlignCenter1: TRichEditAlignCenter;
    FileExit1: TFileExit;
    SearchFind1: TSearchFind;
    SearchFindNext1: TSearchFindNext;
    SearchReplace1: TSearchReplace;
    SearchFindFirst1: TSearchFindFirst;
    ActionToolBar1: TActionToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure PopupClass(Sender: TObject; var PopupClass: TCustomPopupClass);
  public
    { Public declarations }
  end;

{ TCustomLayeredActionPopupEx }

  TCustomLayeredActionPopupEx = class(TXPStylePopupMenu)
  private
    FAlphaBlend: Boolean;
    FAlphaBlendValue: Byte;
    FTransparentColor: Boolean;
    FTransparentColorValue: TColor;
    procedure SetAlphaBlend(const Value: Boolean);
    procedure SetAlphaBlendValue(const Value: Byte);
    procedure SetTransparentColor(const Value: Boolean);
    procedure SetTransparentColorValue(const Value: TColor);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure InitAlphaBlending(var Params: TCreateParams);
    procedure SetLayeredAttribs;
    property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue;
    property TransparentColor: Boolean read FTransparentColor write SetTransparentColor;
    property TransparentColorValue: TColor read FTransparentColorValue write SetTransparentColorValue;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TCustomLayeredActionPopup }

procedure TCustomLayeredActionPopupEx.SetAlphaBlend(const Value: Boolean);
begin
  if FAlphaBlend <> Value then
  begin
    FAlphaBlend := Value;
    SetLayeredAttribs;
  end;
end;

procedure TCustomLayeredActionPopupEx.SetAlphaBlendValue(const Value: Byte);
begin
  if FAlphaBlendValue <> Value then
  begin
    FAlphaBlendValue := Value;
    SetLayeredAttribs;
  end;
end;

procedure TCustomLayeredActionPopupEx.SetLayeredAttribs;
const
  cUseAlpha: array [Boolean] of Integer = (0, LWA_ALPHA);
  cUseColorKey: array [Boolean] of Integer = (0, LWA_COLORKEY);
var
  AStyle: Integer;
begin
  if not (csDesigning in ComponentState) and
    (Assigned(SetLayeredWindowAttributes)) and HandleAllocated then
  begin
    AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
    if FAlphaBlend then
    begin
      if (AStyle and WS_EX_LAYERED) = 0 then
        SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
      SetLayeredWindowAttributes(Handle, FTransparentColorValue, FAlphaBlendValue,
        cUseAlpha[FAlphaBlend] or cUseColorKey[FTransparentColor]);
    end
    else
    begin
      SetWindowLong(Handle, GWL_EXSTYLE, AStyle and not WS_EX_LAYERED);
      RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
    end;
  end;
end;

procedure TCustomLayeredActionPopupEx.SetTransparentColorValue(const Value: TColor);
begin
  if FTransparentColorValue <> Value then
  begin
    FTransparentColorValue := Value;
    SetLayeredAttribs;
  end;
end;

procedure TCustomLayeredActionPopupEx.SetTransparentColor(const Value: Boolean);
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    SetLayeredAttribs;
  end;
end;

procedure TCustomLayeredActionPopupEx.InitAlphaBlending(var Params: TCreateParams);
begin
  if not (csDesigning in ComponentState) and (assigned(SetLayeredWindowAttributes)) then
    if FAlphaBlend or FTransparentColor then
      Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
end;

procedure TCustomLayeredActionPopupEx.CreateWindowHandle(
  const Params: TCreateParams);
begin
  inherited;
  SetLayeredAttribs;
end;

constructor TCustomLayeredActionPopupEx.Create(AOwner: TComponent);
begin
  inherited;
  FAlphaBlend := True;
  FAlphaBlendValue := 125;
  FTransparentColorValue := clWhite;
end;

procedure TCustomLayeredActionPopupEx.CreateParams(
  var Params: TCreateParams);
begin
  inherited;
  InitAlphaBlending(Params);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ActionMainMenuBar1.OnGetPopupClass := PopupClass;
end;

procedure TForm1.PopupClass(Sender: TObject;
  var PopupClass: TCustomPopupClass);
begin
  PopupClass := TCustomLayeredActionPopupEx;
end;

end.
