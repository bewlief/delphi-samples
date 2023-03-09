{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                FireDAC GUIx Forms memo                }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$DEFINE FireDAC_SynEdit}

unit FireDAC.VCLUI.SynMemo;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages, System.Win.Registry,
{$ENDIF}
  System.Classes, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.Dialogs,
  emb.SynEdit, emb.SynMemo, emb.SynHighlighterSQL, emb.SynEditHighlighter,
  FireDAC.Stan.Intf, FireDAC.VCLUI.Controls;

type
  {----------------------------------------------------------------------------}
  { TFDGUIxFormsSynMemo                                                        }
  {----------------------------------------------------------------------------}
  TFDGUIxFormsSynMemo = class(TSynMemo, IFDGUIxFormsMemo)
  private
    FFindDialog: TFindDialog;
    FReplaceDialog: TReplaceDialog;
    FCurDialog: TFindDialog;
    function DoFind(ARestart, AForward, ACaseSens, AWholeWord, AReplace, AReplaceAll: Boolean;
      const ASearchString, AReplaceString: String): Boolean;
    procedure DoDialogFind(Sender: TObject);
  protected
    procedure SetBothScrollBars;
    procedure SetOnChange(const AEvent: TNotifyEvent);
    procedure SetOnKeydown(const AEvent: TKeyEvent);
    procedure SetOnKeyUp(const AEvent: TKeyEvent);
    procedure SetOnMouseUp(const AEvent: TMouseEvent);
    procedure SetOnExit(const AEvent: TNotifyEvent);
    function GetFont: TFont;
    function GetLines: TStrings;
    function GetRDBMSKind: TFDRDBMSKind;
    procedure SetRDBMSKind(const AValue: TFDRDBMSKind);
    function GetCaretPos: TPoint;
    procedure SetCaretPos(const AValue: TPoint);
    procedure SetSelStart(AValue: Integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CanFindFirst: Boolean;
    procedure FindFirst;
    function CanFindReplaceFirst: Boolean;
    procedure FindReplaceFirst;
    function CanFindNext: Boolean;
    procedure FindNext;
    function CanSelectAll: Boolean;
    procedure SelectAll;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property RDBMSKind: TFDRDBMSKind read GetRDBMSKind write SetRDBMSKind;
  end;

implementation

uses
  System.Types, System.UITypes, System.SysUtils, System.TypInfo,
  emb.SynEditTypes, emb.SynEditSearch,
  FireDAC.Stan.Util, FireDAC.Stan.ResStrs;

{------------------------------------------------------------------------------}
{ TFDGUIxFormsHilighter                                                        }
{------------------------------------------------------------------------------}
type
  TFDGUIxFormsHilighter = class(TSynSQLSyn)
  private
{$IFDEF MSWINDOWS}
    FReg: TRegistry;
{$ENDIF}
    FFontName: String;
    FFontSize: Integer;
{$IFDEF MSWINDOWS}
    function GetEditorKey(AKind: Integer): String;
    procedure LoadAttr(AAttrs: TSynHighlighterAttributes; const AKind: String);
    procedure LoadSettings;
    function ReadBool(const AIdent: string): Boolean;
{$ENDIF}
    function GetRDBMSKind: TFDRDBMSKind;
    procedure SetRDBMSKind(const AValue: TFDRDBMSKind);
  public
    constructor Create(AOwner: TComponent); override;
    property RDBMSKind: TFDRDBMSKind read GetRDBMSKind write SetRDBMSKind;
  end;

{------------------------------------------------------------------------------}
constructor TFDGUIxFormsHilighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF MSWINDOWS}
  FReg := TRegistry.Create;
  try
    FReg.RootKey := HKEY_CURRENT_USER;
    LoadSettings;
  finally
    FDFree(FReg);
  end;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
{------------------------------------------------------------------------------}
function TFDGUIxFormsHilighter.GetEditorKey(AKind: Integer): String;
begin
  Result := '\SOFTWARE\Embarcadero\';
  if AKind = 0 then
    Result := Result + 'BDS\'
  else
    Result := Result + 'BDS_WORK\';
  Result := Result + IntToStr(Trunc(RTLVersion - 13.0)) + '.0\Editor';
end;

{-------------------------------------------------------------------------------}
function TFDGUIxFormsHilighter.ReadBool(const AIdent: string): Boolean;
const
  C_BoolArr: array [Boolean] of String = ('False', 'True');
begin
  Result := FReg.ReadString(AIdent) = C_BoolArr[True];
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsHilighter.LoadAttr(AAttrs: TSynHighlighterAttributes;
  const AKind: String);
var
  eStyles: TFontStyles;
  s: String;
begin
  if not FReg.OpenKeyReadOnly(GetEditorKey(0) + '\Highlight\' + AKind) then
    if not FReg.OpenKeyReadOnly(GetEditorKey(1) + '\Highlight\' + AKind) then
      Exit;

  if not ReadBool('Default Foreground') then begin
    s := FReg.ReadString('Foreground Color New');
    if s <> '' then
      AAttrs.Foreground := StringToColor(s);
  end;

  if not ReadBool('Default Background') then begin
    s := FReg.ReadString('Background Color New');
    if s <> '' then
      AAttrs.Background := StringToColor(s);
  end;

  eStyles := [];
  if ReadBool('Bold') then
    Include(eStyles, fsBold)
  else
    Exclude(eStyles, fsBold);
  if ReadBool('Italic') then
    Include(eStyles, fsItalic)
  else
    Exclude(eStyles, fsItalic);
  if ReadBool('Underline') then
    Include(eStyles, fsUnderline)
  else
    Exclude(eStyles, fsUnderline);
  AAttrs.Style := eStyles;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsHilighter.LoadSettings;
begin
  if not FReg.OpenKeyReadOnly(GetEditorKey(0) + '\Options') then
    if not FReg.OpenKeyReadOnly(GetEditorKey(1) + '\Options') then
      Exit;
  if FReg.ValueExists('Editor Font') then
    FFontName := FReg.ReadString('Editor Font')
  else
    FFontName := 'Courier New';
  if FReg.ValueExists('Font Size') then
    FFontSize := FReg.ReadInteger('Font Size')
  else
    FFontSize := 10;
  LoadAttr(SpaceAttri, 'Whitespace');
  LoadAttr(CommentAttribute, 'Comment');
  LoadAttr(CommentAttri, 'Comment');
  LoadAttr(ConditionalCommentAttri, 'Preprocessor');
  LoadAttr(KeywordAttribute, 'Reserved word');
  LoadAttr(KeyAttri, 'Reserved word');
  LoadAttr(PLSQLAttri, 'Reserved word');
  LoadAttr(SQLPlusAttri, 'Reserved word');
  LoadAttr(FunctionAttri, 'Reserved word');
  LoadAttr(DataTypeAttri, 'Reserved word');
  LoadAttr(NumberAttri, 'Number');
  LoadAttr(StringAttribute, 'String');
  LoadAttr(StringAttri, 'String');
  LoadAttr(SymbolAttribute, 'Symbol');
  LoadAttr(SymbolAttri, 'Symbol');
  LoadAttr(IdentifierAttribute, 'Identifier');
  LoadAttr(IdentifierAttri, 'Identifier');
  LoadAttr(DelimitedIdentifierAttri, 'Identifier');
  LoadAttr(VariableAttri, 'Identifier');
end;
{$ENDIF}

{------------------------------------------------------------------------------}
function TFDGUIxFormsHilighter.GetRDBMSKind: TFDRDBMSKind;
var
  sSQLDialect: string;
begin
  sSQLDialect := GetEnumName(TypeInfo(TSQLDialect), Ord(SQLDialect));
  if SameText(sSQLDialect, 'sqlInterbase6') then Result := TFDRDBMSKinds.Interbase
  else if SameText(sSQLDialect, 'sqlInterbase6') then Result := TFDRDBMSKinds.Interbase
  else if SameText(sSQLDialect, 'sqlMSSQL7') then Result := TFDRDBMSKinds.MSSQL
  else if SameText(sSQLDialect, 'sqlMySQL') then Result := TFDRDBMSKinds.MySQL
  else if SameText(sSQLDialect, 'sqlOracle') then Result := TFDRDBMSKinds.Oracle
  else if SameText(sSQLDialect, 'sqlSybase') then Result := TFDRDBMSKinds.SQLAnywhere
  else if SameText(sSQLDialect, 'sqlMSSQL2K') then Result := TFDRDBMSKinds.MSSQL
  else if SameText(sSQLDialect, 'sqlPostgres') then Result := TFDRDBMSKinds.PostgreSQL
  else Result := TFDRDBMSKinds.Other;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsHilighter.SetRDBMSKind(const AValue: TFDRDBMSKind);
begin
  case AValue of
    TFDRDBMSKinds.Oracle:      SQLDialect := sqlOracle;
    TFDRDBMSKinds.MSSQL:       SQLDialect := sqlMSSQL2K;
    TFDRDBMSKinds.MySQL:       SQLDialect := sqlMySQL;
    TFDRDBMSKinds.SQLAnywhere: SQLDialect := sqlSybase;
    TFDRDBMSKinds.Interbase:   SQLDialect := sqlInterbase6;
    TFDRDBMSKinds.Firebird:    SQLDialect := sqlInterbase6;
    TFDRDBMSKinds.PostgreSQL:  SQLDialect := sqlPostgres;
    else                       SQLDialect := sqlStandard;
  end;
end;

{------------------------------------------------------------------------------}
{ TFDGUIxFormsSynMemo                                                          }
{------------------------------------------------------------------------------}
constructor TFDGUIxFormsSynMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Highlighter := TFDGUIxFormsHilighter.Create(Self);
  if TFDGUIxFormsHilighter(Highlighter).FFontName <> '' then
    Font.Name := TFDGUIxFormsHilighter(Highlighter).FFontName;
  if TFDGUIxFormsHilighter(Highlighter).FFontSize <> 0 then
    Font.Height := TFDGUIxFormsHilighter(Highlighter).FFontSize
  else
    Font.Size := 9;
  WordWrap := False;
  Gutter.ShowLineNumbers := True;
  SearchEngine := TSynEditSearch.Create(Self);
  FFindDialog := TFindDialog.Create(Self);
  FFindDialog.OnFind := DoDialogFind;
  FReplaceDialog := TReplaceDialog.Create(Self);
  FReplaceDialog.OnFind := DoDialogFind;
  FReplaceDialog.OnReplace := DoDialogFind;
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.GetCaretPos: TPoint;
begin
  Result := Point(CaretX - 1, CaretY - 1);
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetCaretPos(const AValue: TPoint);
begin
  CaretX := AValue.X + 1;
  CaretY := AValue.Y + 1;
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.GetRDBMSKind: TFDRDBMSKind;
begin
  Result := TFDGUIxFormsHilighter(Highlighter).RDBMSKind;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetRDBMSKind(const AValue: TFDRDBMSKind);
begin
  TFDGUIxFormsHilighter(Highlighter).RDBMSKind := AValue;
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.GetFont: TFont;
begin
  Result := Font;
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.GetLines: TStrings;
begin
  Result := Lines;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetBothScrollBars;
begin
  ScrollBars := ssBoth;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetOnChange(const AEvent: TNotifyEvent);
begin
  OnChange := AEvent;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetOnKeydown(const AEvent: TKeyEvent);
begin
  OnKeyDown := AEvent;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetOnKeyUp(const AEvent: TKeyEvent);
begin
  OnKeyUp := AEvent;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetOnMouseUp(const AEvent: TMouseEvent);
begin
  OnMouseUp := AEvent;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetOnExit(const AEvent: TNotifyEvent);
begin
  OnExit := AEvent;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SetSelStart(AValue: Integer);
begin
  SelStart := AValue;
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.DoFind(ARestart, AForward, ACaseSens, AWholeWord,
  AReplace, AReplaceAll: Boolean; const ASearchString, AReplaceString: String): Boolean;
var
  eSSO: TSynSearchOptions;
begin
  eSSO := [];
  if not AForward then
    Include(eSSO, ssoBackwards);
  if ACaseSens then
    Include(eSSO, ssoMatchCase);
  if ARestart then
    Include(eSSO, ssoEntireScope);
  if AWholeWord then
    Include(eSSO, ssoWholeWord);
  if AReplace then
    Include(eSSO, ssoReplace);
  if AReplaceAll then
    Include(eSSO, ssoReplaceAll);
  if SearchReplace(ASearchString, AReplaceString, eSSO) = 0 then begin
//    if ssoBackwards in eSSO then
//      BlockEnd := BlockBegin
//    else
//      BlockBegin := BlockEnd;
//    CaretXY := BlockBegin;
    Result := False;
  end
  else
    Result := True;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.DoDialogFind(Sender: TObject);
var
  lFind: Boolean;
begin
  if not ((FCurDialog = FReplaceDialog) and
          ([frReplace, frReplaceAll] * FCurDialog.Options = [])) then begin
    FCurDialog.CloseDialog;
    SetFocus;
  end;
  lFind := DoFind(False,
    frDown in FCurDialog.Options,
    frMatchCase in FCurDialog.Options,
    frWholeWord in FCurDialog.Options,
    (frReplace in FCurDialog.Options) or (FCurDialog = FReplaceDialog),
    frReplaceAll in FCurDialog.Options,
    FCurDialog.FindText,
    FReplaceDialog.ReplaceText);
  if not lFind and not ((CaretXY.Char = 1) and (CaretXY.Line = 1)) then
    if MessageDlg(S_FD_FindNotFoundRestart, mtConfirmation, mbYesNo, -1) = mrYes then
      lFind := DoFind(True,
        frDown in FCurDialog.Options,
        frMatchCase in FCurDialog.Options,
        frWholeWord in FCurDialog.Options,
        (frReplace in FCurDialog.Options) or (FCurDialog = FReplaceDialog),
        frReplaceAll in FCurDialog.Options,
        FCurDialog.FindText,
        FReplaceDialog.ReplaceText)
    else
      lFind := True;
  if not lFind then
    MessageDlg(S_FD_FindNotFound, mtConfirmation, [mbOk], -1);
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.CanFindFirst: Boolean;
begin
  Result := Lines.Count > 0;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.FindFirst;
var
  s: string;
begin
  FCurDialog := FFindDialog;
  s := GetWordAtRowCol(CaretXY);
  if s <> '' then
    FFindDialog.FindText := s;
  FFindDialog.Execute;
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.CanFindReplaceFirst: Boolean;
begin
  Result := (Lines.Count > 0) and not ReadOnly;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.FindReplaceFirst;
var
  s: string;
begin
  FCurDialog := FReplaceDialog;
  s := GetWordAtRowCol(CaretXY);
  if s <> '' then
    FReplaceDialog.FindText := s;
  FReplaceDialog.Execute;
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.CanFindNext: Boolean;
begin
  Result := (FCurDialog <> nil) and (FCurDialog.FindText <> '') and (Lines.Count > 0);
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.FindNext;
begin
  DoDialogFind(nil);
end;

{------------------------------------------------------------------------------}
function TFDGUIxFormsSynMemo.CanSelectAll: Boolean;
begin
  Result := Lines.Count > 0;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.SelectAll;
begin
  inherited SelectAll;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.KeyDown(var Key: Word; Shift: TShiftState);
var
  s: String;
begin
  inherited KeyDown(Key, Shift);
  if (Shift = [ssCtrl]) and (Key = Ord('F')) then begin
    if CanFindFirst then
      FindFirst;
    Key := 0;
  end
  else if (Shift = [ssCtrl]) and (Key = Ord('R')) then begin
    if CanFindReplaceFirst then
      FindReplaceFirst;
    Key := 0;
  end
  else if Key = VK_F3 then begin
    if CanFindNext then
      FindNext;
    Key := 0;
  end
  else if (Shift = [ssAlt]) and (Key = Ord('G')) then begin
    s := CaretY.ToString;
    if InputQuery(S_FD_GotoCaption, S_FD_GotoPrompt, s) then
      CaretY := StrToIntDef(s, CaretY);
  end;
end;

{------------------------------------------------------------------------------}
procedure TFDGUIxFormsSynMemo.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited ChangeScale(M, D, isDpiChange);
  Font.Size := ScaleValue(Font.Size);
end;

{------------------------------------------------------------------------------}
function CreateSynMemo(AOwner: TComponent): TWinControl;
begin
  Result := TFDGUIxFormsSynMemo.Create(AOwner);
end;

initialization
  FDGUIxCreateMemoFunc := CreateSynMemo;

end.
