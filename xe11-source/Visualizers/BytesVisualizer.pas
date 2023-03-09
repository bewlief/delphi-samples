{*******************************************************}
{                                                       }
{            RadStudio Debugger Visualizer Sample       }
{ Copyright(c) 2009-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit BytesVisualizer;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Messages, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Grids, ToolsAPI, Vcl.ExtCtrls;

type
  TAvailableState = (asAvailable, asProcRunning, asOutOfScope, asNotAvailable);

  TBytesViewerFrame = class(TFrame, IOTADebuggerVisualizerExternalViewerUpdater,
    IOTAThreadNotifier, IOTAThreadNotifier160)
    pcViews: TPageControl;
    tabHex: TTabSheet;
    tabANSI: TTabSheet;
    mmANSI: TMemo;
    grdHex: TStringGrid;
    tabUnicode: TTabSheet;
    mmUnicode: TMemo;
    tabUTF8: TTabSheet;
    mmUTF8: TMemo;
    Panel1: TPanel;
    procedure pcViewsChange(Sender: TObject);
  private
    FOwningForm: TCustomForm;
    FClosedProc: TOTAVisualizerClosedProcedure;
    FExpression: string;
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FDeferredError: Boolean;
    FItems: TStrings;
    FAvailableState: TAvailableState;
    function Evaluate(Expression: string): string;
    procedure SetForm(AForm: TCustomForm);
    procedure AddBytesRegion(const Expression, TypeName, EvalResult: string);
    procedure SetAvailableState(const AState: TAvailableState);
    function GetBytesExps(const ATypeName, AExpression: string; var APtrExp,
      ALenExp, APosExp: string): Boolean;
    procedure WMDPIChangedAfterParent(var Message: TMessage); message WM_DPICHANGED_AFTERPARENT;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    { IOTADebuggerVisualizerExternalViewerUpdater }
    procedure CloseVisualizer;
    procedure MarkUnavailable(Reason: TOTAVisualizerUnavailableReason);
    procedure RefreshVisualizer(const Expression, TypeName, EvalResult: string);
    procedure SetClosedCallback(ClosedProc: TOTAVisualizerClosedProcedure);
    { IOTAThreadNotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress, ResultSize: LongWord; ReturnCode: Integer); overload;
    procedure ModifyComplete(const ExprStr, ResultStr: string; ReturnCode: Integer);
    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr, ResultStr: string; CanModify: Boolean;
      ResultAddress: TOTAAddress; ResultSize: LongWord; ReturnCode: Integer); overload;
  end;

procedure Register;

implementation

uses
  DesignIntf, Vcl.Actnlist, Vcl.ImgList, Vcl.Menus, System.IniFiles, Vcl.GraphUtil, BrandingAPI;

{$R *.dfm}

resourcestring
  sBytesVisualizerName = 'TBytes/TMemoryStream Visualizer for Delphi';
  sBytesVisualizerDescription = 'Displays TBytes/TMemoryStream instances in a human-readable memory dump / string format';
  sMenuText = 'Show Content';
  sFormCaption = 'Memory Visualizer - %s';
  sProcessNotAccessible = 'process not accessible';
  sValueNotAccessible = 'value not accessible';
  sOutOfScope = 'out of scope';
  sTrimmed = 'trimmed at %d bytes';

type

  IFrameFormHelper = interface
    ['{0FD4A98F-CE6B-422A-BF13-14E59707D3B2}']
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Form: TCustomFrame);
  end;

  TBytesVisualizerForm = class(TInterfacedObject, INTACustomDockableForm, IFrameFormHelper)
  private
    FMyFrame: TBytesViewerFrame;
    FMyForm: TCustomForm;
    FExpression: string;
  public
    constructor Create(const Expression: string);
    { INTACustomDockableForm }
    function GetCaption: string;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetIdentifier: string;
    function GetMenuActionList: TCustomActionList;
    function GetMenuImageList: TCustomImageList;
    procedure CustomizePopupMenu(PopupMenu: TPopupMenu);
    function GetToolbarActionList: TCustomActionList;
    function GetToolbarImageList: TCustomImageList;
    procedure CustomizeToolBar(ToolBar: TToolBar);
    procedure LoadWindowState(Desktop: TCustomIniFile; const Section: string);
    procedure SaveWindowState(Desktop: TCustomIniFile; const Section: string; IsProject: Boolean);
    function GetEditState: TEditState;
    function EditAction(Action: TEditAction): Boolean;
    { IFrameFormHelper }
    function GetForm: TCustomForm;
    function GetFrame: TCustomFrame;
    procedure SetForm(Form: TCustomForm);
    procedure SetFrame(Frame: TCustomFrame);
  end;

  TDebuggerBytesVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizer250, IOTADebuggerVisualizerExternalViewer)
  public
    { IOTADebuggerVisualizer }
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean); overload;
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    { IOTADebuggerVisualizer250 }
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean; var IsGeneric: Boolean); overload;
    { IOTADebuggerVisualizerExternalViewer }
    function GetMenuText: string;
    function Show(const Expression, TypeName, EvalResult: string;
      SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
  end;

  TBytesVisualizerType = record
    TypeName: string;
    AllDesc: Boolean;
    IsGen: Boolean;
    PtrExp: string;
    LenExp: string;
    PosExp: string;
  end;

const
  CBpL = 16;
  CMaxBytes = $10000;

  BytesVisualizerTypes: array[0..4] of TBytesVisualizerType =
  (
    (TypeName: 'TArray<System.Byte>';        AllDesc: False; IsGen: True; PtrExp: 'Pointer(%s)'; LenExp: 'Length(%s)'; PosExp: '';),
    (TypeName: 'System.TArray<System.Byte>'; AllDesc: False; IsGen: True; PtrExp: 'Pointer(%s)'; LenExp: 'Length(%s)'; PosExp: '';),
    (TypeName: 'array of Byte';              AllDesc: False; IsGen: False; PtrExp: 'Pointer(%s)'; LenExp: 'Length(%s)'; PosExp: '';),
    (TypeName: 'RawByteString';              AllDesc: False; IsGen: False; PtrExp: 'Pointer(%s)'; LenExp: 'Length(%s)'; PosExp: '';),
    (TypeName: 'TCustomMemoryStream';        AllDesc: True;  IsGen: False; PtrExp: '%s.Memory';   LenExp: '%s.Size';    PosExp: '%s.Position';)
  );

{ TDebuggerBytesVisualizer }

function TDebuggerBytesVisualizer.GetMenuText: string;
begin
  Result := sMenuText;
end;

function TDebuggerBytesVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := Length(BytesVisualizerTypes);
end;

procedure TDebuggerBytesVisualizer.GetSupportedType(Index: Integer; var TypeName: string;
  var AllDescendants: Boolean);
begin
  TypeName := BytesVisualizerTypes[Index].TypeName;
  AllDescendants := BytesVisualizerTypes[Index].AllDesc;
end;

procedure TDebuggerBytesVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants, IsGeneric: Boolean);
begin
  TypeName := BytesVisualizerTypes[Index].TypeName;
  AllDescendants := BytesVisualizerTypes[Index].AllDesc;
  IsGeneric := BytesVisualizerTypes[Index].IsGen;
end;

function TDebuggerBytesVisualizer.GetVisualizerDescription: string;
begin
  Result := sBytesVisualizerDescription;
end;

function TDebuggerBytesVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerBytesVisualizer.GetVisualizerName: string;
begin
  Result := sBytesVisualizerName;
end;

function TDebuggerBytesVisualizer.Show(const Expression, TypeName, EvalResult: string;
  SuggestedLeft, SuggestedTop: Integer): IOTADebuggerVisualizerExternalViewerUpdater;
var
  AForm: TCustomForm;
  AFrame: TBytesViewerFrame;
  VisDockForm: INTACustomDockableForm;
  LThemingServices: IOTAIDEThemingServices;
begin
  VisDockForm := TBytesVisualizerForm.Create(Expression) as INTACustomDockableForm;
  AForm := (BorlandIDEServices as INTAServices).CreateDockableForm(VisDockForm);
  AForm.LockDrawing;
  try
    AForm.Left := SuggestedLeft;
    AForm.Top := SuggestedTop;
    (VisDockForm as IFrameFormHelper).SetForm(AForm);
    AFrame := (VisDockForm as IFrameFormHelper).GetFrame as TBytesViewerFrame;
    AFrame.AddBytesRegion(Expression, TypeName, EvalResult);
    AFrame.pcViewsChange(nil);
    Result := AFrame as IOTADebuggerVisualizerExternalViewerUpdater;
    if Supports(BorlandIDEServices, IOTAIDEThemingServices, LThemingServices) and
      LThemingServices.IDEThemingEnabled then
    begin
      AFrame.Panel1.StyleElements := AFrame.Panel1.StyleElements - [seClient];
      AFrame.Panel1.ParentBackground := False;
      LThemingServices.ApplyTheme(AForm);
      AFrame.Panel1.Color := ColorBlendRGB(LThemingServices.StyleServices.GetSystemColor(clWindowText),
      LThemingServices.StyleServices.GetSystemColor(clWindow), 0.5);
 
      if TIDEThemeMetrics.Font.Enabled then
        AFrame.Font.Assign(TIDEThemeMetrics.Font.GetFont());
    end;
  finally
    AForm.UnlockDrawing;
  end;
end;

{ TBytesViewerFrame }

procedure TBytesViewerFrame.SetAvailableState(const AState: TAvailableState);
var
  s: string;
  I: Integer;
begin
  FAvailableState := AState;
  case FAvailableState of
    asAvailable:
      ;
    asProcRunning:
      s := sProcessNotAccessible;
    asOutOfScope:
      s := sOutOfScope;
    asNotAvailable:
      s := sValueNotAccessible;
  end;
  grdHex.RowCount := 1;
  if FAvailableState <> asAvailable then
  begin
    grdHex.Cells[0, 0] := s;
    for I := 1 to grdHex.ColCount - 1 do
      grdHex.Cells[I, 0] := '';
    mmANSI.Text := s;
    mmUnicode.Text := s;
    mmUTF8.Text := s;
  end
  else
  begin
    grdHex.Cells[0, 0] := '';
    mmANSI.Clear;
    mmUnicode.Clear;
    mmUTF8.Clear;
  end;
end;

function TBytesViewerFrame.GetBytesExps(const ATypeName, AExpression: string;
  var APtrExp, ALenExp, APosExp: string): Boolean;
var
  I, Len: Integer;
  LCurType, LCurExp: string;
begin
  APtrExp := '';
  ALenExp := '';
  APosExp := '';
  Result := False;
  LCurType := ATypeName;
  LCurExp := '(' + AExpression + ')';
  repeat
    Len := LCurType.IndexOf('<') + 1;
    if Len = 0 then
      Len := Length(LCurType);
    for I := Low(BytesVisualizerTypes) to High(BytesVisualizerTypes) do
      if StrLIComp(PChar(BytesVisualizerTypes[I].TypeName), PChar(LCurType), Len) = 0 then
      begin
        APtrExp := BytesVisualizerTypes[I].PtrExp;
        ALenExp := BytesVisualizerTypes[I].LenExp;
        APosExp := BytesVisualizerTypes[I].PosExp;
        Exit(True);
      end;
    LCurExp := LCurExp + '.ClassParent';
    LCurType := Evaluate(LCurExp + '.ClassName');
    LCurType := LCurType.Substring(1, LCurType.Length - 2);
  until (LCurType = '') or SameText(LCurType, 'TObject');
end;

procedure TBytesViewerFrame.AddBytesRegion(const Expression, TypeName,
  EvalResult: string);
var
  DebugSvcs: IOTADebuggerServices;
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  LPtrExp, LLenExp, LPosExp: string;
  LPtrStr, LLenStr: string;
  LPtr, LLen: UInt64;
  LPos: Int64;
  LBytes: TBytes;
  I: Integer;
  LRow, LCol: Integer;
  LStr: string;
  LAnsi: AnsiString;
  LUni: UnicodeString;
  LLenTrimmed: string;
  LOffWidth: Integer;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    CurProcess := DebugSvcs.CurrentProcess;
  if CurProcess = nil then
    Exit;
  CurThread := CurProcess.CurrentThread;
  if CurThread = nil then
    Exit;

  FExpression := Expression;
  if not GetBytesExps(TypeName, Expression, LPtrExp, LLenExp, LPosExp) then
  begin
    SetAvailableState(asNotAvailable);
    Exit;
  end;

  LPtrStr := Evaluate(Format(LPtrExp, [Expression]));
  LLenStr := Evaluate(Format(LLenExp, [Expression]));
  if LPtrStr.IsEmpty or LLenStr.IsEmpty then
  begin
    SetAvailableState(asNotAvailable);
    Exit;
  end;

  SetAvailableState(asAvailable);
  if SameText(LPtrStr, 'nil') then
  begin
    LPtr := 0;
    LLen := 0;
  end
  else
  begin
    LPtr := StrToUInt64(LPtrStr);
    LLen := StrToUInt64(LLenStr);
  end;
  LLenTrimmed := '';
  if LLen > CMaxBytes then
  begin
    LLen := CMaxBytes;
    LLenTrimmed := Format(' (' + sTrimmed + ')', [CMaxBytes]);
  end;
  if LLen > 0 then
  begin
    SetLength(LBytes, LLen);
    CurProcess.ReadProcessMemory(LPtr, LLen, LBytes[0]);
  end;

  if not LPosExp.IsEmpty then
    LPos := StrToInt64Def(Evaluate(Format(LPosExp, [Expression])), 0)
  else
    LPos := 0;
  if LPos > LLen then
    LPos := LLen;

  SetLength(LAnsi, Length(LBytes));
  if Length(LBytes) > 0 then
    for I := Low(LBytes) to High(LBytes) do
      if LBytes[I] = 0 then
        LAnsi[I + 1] := ' '
      else
        LAnsi[I + 1] := AnsiChar(LBytes[I]);
  mmANSI.Text := string(LAnsi) + LLenTrimmed;
  mmANSI.SelStart := LPos;

  SetLength(LUni, (Length(LBytes) + 1) div 2);
  if Length(LBytes) > 0 then
    for I := Low(LBytes) to High(LBytes) div 2 do
      if I * 2 + 1 >= Length(LBytes) then
        if LBytes[I * 2] = 0 then
          LUni[I + 1] := ' '
        else
          LUni[I + 1] := WideChar(LBytes[I * 2])
      else
        if (LBytes[I * 2] = 0) and (LBytes[I * 2 + 1] = 0) then
          LUni[I + 1] := ' '
        else
          LUni[I + 1] := WideChar(LBytes[I * 2] + LBytes[I * 2 + 1] shl 8);
  mmUnicode.Text := LUni + LLenTrimmed;
  mmUnicode.SelStart := LPos div 2;

  try
    mmUTF8.Text := TEncoding.UTF8.GetString(LBytes) + LLenTrimmed;
  except
    on E: Exception do
      mmUTF8.Text := E.Message;
  end;

  grdHex.ColCount := CBpL + 2;
  grdHex.RowCount := 0;
  grdHex.RowCount := (LLen + CBpL - 1) div CBpL;

  LOffWidth := Length(IntToHex(CMaxBytes - 1));
  grdHex.ColWidths[0] := grdHex.TextWidthToColWidth(0, StringOfChar('D', LOffWidth), nil) + 4;
  for I := 0 to CBpL - 1 do
    grdHex.ColWidths[I + 1] := grdHex.TextWidthToColWidth(2, 'DD', nil) + 4;
  grdHex.ColWidths[CBpL + 1] := grdHex.TextWidthToColWidth(0, StringOfChar('W', CBpL), nil) + 4;

  LStr := '';
  LRow := -1;
  for I := 0 to LLen - 1 do
  begin
    LRow := I div CBpL;
    LCol := I mod CBpL;

    if LCol = 0 then
    begin
      grdHex.Cells[0, LRow] := IntToHex(I, LOffWidth);
      if LRow > 0 then
        grdHex.Cells[CBpL + 1, LRow - 1] := LStr;
      LStr := StringOfChar(' ', CBpL);
    end;

    if (I = LLen - 1) and (LLenTrimmed <> '') then
      grdHex.Cells[LCol + 1, LRow] := '..'
    else
      grdHex.Cells[LCol + 1, LRow] := IntToHex(LBytes[I]);

    if LBytes[I] > 0 then
      LStr[LCol + 1] := Char(LBytes[I])
    else
      LStr[LCol + 1] := ' ';
  end;
  if LRow <> -1 then
    grdHex.Cells[CBpL + 1, LRow] := LStr;
  if (LPos = LLen) and (LLen > 0) then
    Dec(LPos);
  grdHex.Row := LPos div CBpL;
  grdHex.Col := 1 + LPos mod CBpL;
end;

procedure TBytesViewerFrame.AfterSave;
begin

end;

procedure TBytesViewerFrame.BeforeSave;
begin

end;

procedure TBytesViewerFrame.CloseVisualizer;
begin
  if FOwningForm <> nil then
    FOwningForm.Close;
end;

procedure TBytesViewerFrame.Destroyed;
begin

end;

function TBytesViewerFrame.Evaluate(Expression: string): string;
var
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  ResultStr: array[0..4095] of Char;
  CanModify: Boolean;
  Done: Boolean;
  ResultAddr, ResultSize, ResultVal: LongWord;
  EvalRes: TOTAEvaluateResult;
  DebugSvcs: IOTADebuggerServices;
begin
  begin
    Result := '';
    if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
      CurProcess := DebugSvcs.CurrentProcess;
    if CurProcess <> nil then
    begin
      CurThread := CurProcess.CurrentThread;
      if CurThread <> nil then
      begin
        repeat
        begin
          Done := True;
          EvalRes := CurThread.Evaluate(Expression, @ResultStr, Length(ResultStr),
            CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
          case EvalRes of
            erOK: Result := ResultStr;
            erDeferred:
              begin
                FCompleted := False;
                FDeferredResult := '';
                FDeferredError := False;
                FNotifierIndex := CurThread.AddNotifier(Self);
                while not FCompleted do
                  DebugSvcs.ProcessDebugEvents;
                CurThread.RemoveNotifier(FNotifierIndex);
                FNotifierIndex := -1;
                if not FDeferredError then
                begin
                  if FDeferredResult <> '' then
                    Result := FDeferredResult
                  else
                    Result := ResultStr;
                end;
              end;
            erBusy:
              begin
                DebugSvcs.ProcessDebugEvents;
                Done := False;
              end;
          end;
        end
        until Done = True;
      end;
    end;
  end;
end;

procedure TBytesViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: LongWord;
  ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress), ResultSize, ReturnCode);
end;

procedure TBytesViewerFrame.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  FDeferredResult := ResultStr;
  FDeferredError := ReturnCode <> 0;
end;

procedure TBytesViewerFrame.MarkUnavailable(
  Reason: TOTAVisualizerUnavailableReason);
begin
  if Reason = ovurProcessRunning then
    SetAvailableState(asProcRunning)
  else if Reason = ovurOutOfScope then
    SetAvailableState(asOutOfScope);
end;

procedure TBytesViewerFrame.Modified;
begin

end;

procedure TBytesViewerFrame.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin

end;

procedure TBytesViewerFrame.RefreshVisualizer(const Expression, TypeName,
  EvalResult: string);
begin
  AddBytesRegion(Expression, TypeName, EvalResult);
end;

procedure TBytesViewerFrame.SetClosedCallback(
  ClosedProc: TOTAVisualizerClosedProcedure);
begin
  FClosedProc := ClosedProc;
end;

procedure TBytesViewerFrame.SetForm(AForm: TCustomForm);
begin
  FOwningForm := AForm;
end;

procedure TBytesViewerFrame.SetParent(AParent: TWinControl);
begin
  if AParent = nil then
  begin
    FreeAndNil(FItems);
    if Assigned(FClosedProc) then
      FClosedProc;
  end;
  inherited;
end;

procedure TBytesViewerFrame.WMDPIChangedAfterParent(var Message: TMessage);
begin
  inherited;
  if TIDEThemeMetrics.Font.Enabled then
    TIDEThemeMetrics.Font.AdjustDPISize(Font, TIDEThemeMetrics.Font.Size, PixelsPerInch);
end;

procedure TBytesViewerFrame.pcViewsChange(Sender: TObject);
begin
  if pcViews.ActivePage = tabHex then
    grdHex.SetFocus
  else if pcViews.ActivePage = tabANSI then
    mmANSI.SetFocus
  else if pcViews.ActivePage = tabUnicode then
    mmUnicode.SetFocus
  else if pcViews.ActivePage = tabUTF8 then
    mmUTF8.SetFocus;
end;

procedure TBytesViewerFrame.ThreadNotify(Reason: TOTANotifyReason);
begin

end;

{ TBytesVisualizerForm }

constructor TBytesVisualizerForm.Create(const Expression: string);
begin
  inherited Create;
  FExpression := Expression;
end;

procedure TBytesVisualizerForm.CustomizePopupMenu(PopupMenu: TPopupMenu);
begin
  // no toolbar
end;

procedure TBytesVisualizerForm.CustomizeToolBar(ToolBar: TToolBar);
begin
 // no toolbar
end;

function TBytesVisualizerForm.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

procedure TBytesVisualizerForm.FrameCreated(AFrame: TCustomFrame);
begin
  FMyFrame := TBytesViewerFrame(AFrame);
end;

function TBytesVisualizerForm.GetCaption: string;
begin
  Result := Format(sFormCaption, [FExpression]);
end;

function TBytesVisualizerForm.GetEditState: TEditState;
begin
  Result := [];
end;

function TBytesVisualizerForm.GetForm: TCustomForm;
begin
  Result := FMyForm;
end;

function TBytesVisualizerForm.GetFrame: TCustomFrame;
begin
  Result := FMyFrame;
end;

function TBytesVisualizerForm.GetFrameClass: TCustomFrameClass;
begin
  Result := TBytesViewerFrame;
end;

function TBytesVisualizerForm.GetIdentifier: string;
begin
  Result := 'BytesDebugVisualizer';
end;

function TBytesVisualizerForm.GetMenuActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBytesVisualizerForm.GetMenuImageList: TCustomImageList;
begin
  Result := nil;
end;

function TBytesVisualizerForm.GetToolbarActionList: TCustomActionList;
begin
  Result := nil;
end;

function TBytesVisualizerForm.GetToolbarImageList: TCustomImageList;
begin
  Result := nil;
end;

procedure TBytesVisualizerForm.LoadWindowState(Desktop: TCustomIniFile;
  const Section: string);
begin
  //no desktop saving
end;

procedure TBytesVisualizerForm.SaveWindowState(Desktop: TCustomIniFile;
  const Section: string; IsProject: Boolean);
begin
  //no desktop saving
end;

procedure TBytesVisualizerForm.SetForm(Form: TCustomForm);
begin
  FMyForm := Form;
  if Assigned(FMyFrame) then
    FMyFrame.SetForm(FMyForm);
end;

procedure TBytesVisualizerForm.SetFrame(Frame: TCustomFrame);
begin
   FMyFrame := TBytesViewerFrame(Frame);
end;

var
  BytesVis: IOTADebuggerVisualizer;

procedure Register;
begin
  BytesVis := TDebuggerBytesVisualizer.Create;
  (BorlandIDEServices as IOTADebuggerServices).RegisterDebugVisualizer(BytesVis);
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    DebuggerServices.UnregisterDebugVisualizer(BytesVis);
    BytesVis := nil;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.

