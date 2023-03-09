{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ScreenReader.Win;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.Layouts, FMX.StdCtrls, FMX.TreeView, FMX.Edit, FMX.Grid, System.Generics.Collections, Winapi.Windows,
  Winapi.Messages, FMX.Platform.Win, Winapi.OleAcc, Winapi.ActiveX;

{$SCOPEDENUMS ON}

type
  TFormAccessibility = class;

  // Form override to handle form's Win proc
  TAccForm = class(TForm)
  protected
    FPrevFocus: string;
    FInitFocus: string;
    FPrevIdx: Integer;
    FPrevSel: TTreeViewItem;
    FPrevExpanded: Boolean;
    FPrevEx: Double;
    FPrevStr: string;
    FPrevCell: TPoint;
    FPrevCaret: Integer;
    FPrevDrop: Boolean;
    OldWinProc: Pointer;
    NewWinProc: Pointer;
    FAccessibility: TFormAccessibility;
    FControlList: TList<TFMXObject>;
    FHWND: HWND;
    FShiftChilds: Boolean;
    procedure CreateHandle; override;
    procedure ReorderChilds;
    function GetFocusedChildID: Integer;
    function IsFocusedList: Boolean;
    function GetChildControlsCount: Integer;
    function GetChildControl(Index: Integer): TFmxObject;
   public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: NativeInt = 0); override;
    destructor Destroy; override;
    procedure UpdateFocusedControl;
  end;

  // Form accessibility handler
  TFormAccessibility = class(TInterfacedObject, IDispatch, IAccessible)
  private
    { IAccessible }
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer; out pcyHeight: Integer;
      varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount: Integer; LocaleID: Integer;
      DispIDs: Pointer): HRESULT; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer; out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult: Pointer;
      ExcepInfo: Pointer; ArgErr: Pointer): HRESULT; stdcall;
  public
    FFmxForm: TAccForm;
    function GetControlValue: string;
    function GetControlName: string;
    function GetControlChecked: Boolean;
    function IsCheckBox: Boolean;
  end;

  // FireMonkey control accessibility handler
  TFmxAccessibility = class(TInterfacedObject, IDispatch, IAccessible)
    { IAccessible }
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer; out pcyHeight: Integer;
      varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount: Integer; LocaleID: Integer;
      DispIDs: Pointer): HRESULT; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer; out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult: Pointer;
      ExcepInfo: Pointer; ArgErr: Pointer): HRESULT; stdcall;
  public
    FFmxObj: TFMXObject;
    FParent: TForm;
    function GetControlValue: string;
    function GetControlName: string;
    function GetControlChecked: boolean;
    function IsCheckBox: boolean;
    function IsTreeView: boolean;
  end;

  // FireMonkey child item accessibility handler
  TFmxChildAccessibility = class(TInterfacedObject, IDispatch, IAccessible)
    { IAccessible }
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer; out pcyHeight: Integer;
      varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount: Integer; LocaleID: Integer;
      DispIDs: Pointer): HRESULT; stdcall;
    function GetTypeInfo(Index: Integer; LocaleID: Integer; out TypeInfo): HRESULT; stdcall;
    function GetTypeInfoCount(out Count: Integer): HRESULT; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult: Pointer;
      ExcepInfo: Pointer; ArgErr: Pointer): HRESULT; stdcall;
  public
    FChildText: string;
    FFmxObj: TFmxObject;
  end;

implementation

{ TScreenReader }

uses
  FMX.ListBox, FMX.Memo, FMX.TabControl, FMX.ScreenReader, FMX.ComboEdit, FMX.SpinBox;

const
  ID_SCREENREADER = $1969;

// Retrieve TForm instance associated with a window handle
function FormForHandle(AHandle: HWND): TForm;
var
  I: integer;
  Wnd: HWND;
begin
  Result := nil;

  for I := 0 to Screen.FormCount - 1 do
  begin
    Wnd := WindowHandleToPlatform(Screen.Forms[I].Handle).Wnd;
    if Wnd = AHandle then
    begin
      Result := Screen.Forms[I] as TForm;
      Break;
    end;
  end;
end;

// Get ACC Role that matches a FireMonkey control class
function GetControlRole(const AObj: TFmxObject): Integer;
var
  Role: TFMXRole;
  FmxAcc: IFMXAccessibility;
begin
  Result := 0;
  if AObj <> nil then
  begin
    if AObj.GetInterface(IFMXAccessibility, FmxAcc) then
    begin
      Role := FmxAcc.GetRole;
      case Role of
        TFMXRole.StaticText: Result := ROLE_SYSTEM_STATICTEXT;
        TFMXRole.Cell: Result := ROLE_SYSTEM_CELL;
        TFMXRole.Text: Result := ROLE_SYSTEM_TEXT;
        TFMXRole.Button: Result := ROLE_SYSTEM_PUSHBUTTON;
        TFMXRole.CheckBox: Result := ROLE_SYSTEM_CHECKBUTTON;
        TFMXRole.RadioButton: Result := ROLE_SYSTEM_RADIOBUTTON;
        TFMXRole.Grid: Result := ROLE_SYSTEM_TABLE;
        TFMXRole.List: Result := ROLE_SYSTEM_LIST;
        TFMXRole.Slider: Result := ROLE_SYSTEM_SLIDER;
        TFMXRole.ComboBox: Result := ROLE_SYSTEM_COMBOBOX;
      else
        Result := ROLE_SYSTEM_TEXT;
      end;
    end
    else if (AObj is TControl) and (AObj.Owner is TColumn) then
      Result := ROLE_SYSTEM_CELL
    else if (AObj is TCustomGrid) then
      Result := ROLE_SYSTEM_CELL
    else if (AObj is TButton) then
      Result := ROLE_SYSTEM_PUSHBUTTON
    else if (AObj is TSpeedButton) then
      Result := ROLE_SYSTEM_PUSHBUTTON
    else if (AObj is TComboEdit) then
      Result := ROLE_SYSTEM_COMBOBOX
    else if (AObj is TListBox) then
      Result := ROLE_SYSTEM_LIST
    else if (AObj is TComboBox) then
      Result := ROLE_SYSTEM_COMBOBOX
    else if (AObj is TEdit) then
      Result := ROLE_SYSTEM_TEXT
    else if (AObj is TSpinBox) then
      Result := ROLE_SYSTEM_SPINBUTTON
    else if (AObj is TTrackBar) then
      Result := ROLE_SYSTEM_SLIDER
    else if (AObj is TLabel) then
      Result := ROLE_SYSTEM_STATICTEXT
    else if (AObj is TCheckBox) then
      Result := ROLE_SYSTEM_CHECKBUTTON
    else if (AObj is TRadioButton) then
      Result := ROLE_SYSTEM_RADIOBUTTON
    else if (AObj is TMemo) then
      Result := ROLE_SYSTEM_TEXT
    else if (AObj is TTreeView) then
      Result := ROLE_SYSTEM_OUTLINEITEM;
  end;
end;

// Windows FMX form window message handler
function ScreenReaderWndProc(AHwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  LAccForm: TFormAccessibility;
  LAccControl: TFmxChildAccessibility;
  LForm: TForm;
  LFmxAccForm: TAccForm;
  LText: string;
  LChildID: Integer;
  I: Integer;
begin
  Result := 0;
  LForm := FormForHandle(AHwnd);
  LFmxAccForm := LForm as TAccForm;
  if (LForm = nil) or (LFmxAccForm = nil) then
    Exit;
  LFmxAccForm.FHWND := AHwnd;

  case uMsg of
  // Main Acc message
    WM_GETOBJECT:
      begin
        LChildID := LFmxAccForm.GetFocusedChildID;

        if (NativeInt(lParam) = NativeInt(LChildID)) then
        begin
          if Assigned(LFmxAccForm.Focused) and (LFmxAccForm.Focused is TListBox) and (TListBox(LFmxAccForm.Focused).ItemIndex >= 0) then
          begin
            // listbox
            LAccControl := TFmxChildAccessibility.Create;
            LAccControl.FChildText := TListBox(LFmxAccForm.Focused).Items[TListBox(LFmxAccForm.Focused).ItemIndex];
            // the parent
            LAccControl.FFmxObj := LFmxAccForm.Focused as TFmxObject;
            Result := LResultFromObject(IID_IAccessible, wParam, LAccControl);
            Exit;
          end;

          if Assigned(LFmxAccForm.Focused) and (LFmxAccForm.Focused is TComboBox) and (TComboBox(LFmxAccForm.Focused).ItemIndex >= 0) then
          begin
            // combobox
            LAccControl := TFmxChildAccessibility.Create;
            LAccControl.FChildText := TComboBox(LFmxAccForm.Focused).Items[TComboBox(LFmxAccForm.Focused).ItemIndex];
            // the parent
            LAccControl.FFmxObj := LFmxAccForm.Focused as TFmxObject;
            Result := LResultFromObject(IID_IAccessible, wParam, LAccControl);
            Exit;
          end;

          if Assigned(LFmxAccForm.Focused) and (LFmxAccForm.Focused is TComboEdit) and (TComboEdit(LFmxAccForm.Focused).ItemIndex >= 0) then
          begin
            // comboedit
            LAccControl := TFmxChildAccessibility.Create;
            LAccControl.FChildText := TComboEdit(LFmxAccForm.Focused).Items[TComboEdit(LFmxAccForm.Focused).ItemIndex];
            // the parent
            LAccControl.FFmxObj := LFmxAccForm.Focused as TFmxObject;
            Result := LresultFromObject(IID_IAccessible, wParam, LAccControl);
            Exit;
          end;
        end;
        // Form accessibility handling object
        if DWORD(lParam) = OBJID_CLIENT then
        begin
          LAccForm := TFormAccessibility.Create;
          LAccForm.FFmxForm := LFmxAccForm;
          Result := LResultFromObject(IID_IAccessible, wParam, LAccForm);
          Exit;
        end;
      end;

    // reset when form looses focus
    WM_KILLFOCUS:
      begin
        LFmxAccForm.FPrevIdx := -1;
        LFmxAccForm.FPrevSel := nil;
        LFmxAccForm.FPrevFocus := '';
        LFmxAccForm.FPrevStr := '';
        LFmxAccForm.FPrevEx := 0;
        LFmxAccForm.FPrevCell := Point(-1,-1);
      end;

    // update focused control on key up & mouse up
    WM_KEYUP, WM_LBUTTONUP:
      begin
        if (LFmxAccForm <> nil) and (LFmxAccForm.Focused <> nil) then
        begin
          Result := CallWindowProc((LFmxAccForm as TAccForm).OldWinProc, AHwnd, uMsg, wParam, lParam);

          LFmxAccForm.UpdateFocusedControl;
          Exit;
        end;
      end;

    // standard window class text retrieval (length + text)
    WM_GETTEXTLENGTH:
      begin
        if LFmxAccForm.Focused <> nil then
          LText := GetFocusControlText(LFmxAccForm)
        else
          LText := LFmxAccForm.Caption;
        Result := Length(LText);
        Exit;
      end;
    WM_GETTEXT:
      begin
        if LFmxAccForm.Focused <> nil then
          LText := GetFocusControlText(LFmxAccForm)
        else
          LText := LFmxAccForm.Caption;
        I := Length(LText);
        Result := StrLen(StrLCopy(PChar(LParam), PChar(LText), I));
        Exit;
      end;
    // Timer method need to detect combobox closing detection
    WM_TIMER:
      begin
        if (LFmxAccForm <> nil) and (LFmxAccForm.Focused <> nil) and (wParam = ID_SCREENREADER) then
        begin
          if (LFmxAccForm.ActiveControl <> nil) and (LFmxAccForm.FInitFocus = '') then
          begin
            LChildID := LFmxAccForm.GetFocusedChildID;

            if (LFmxAccForm.Focused.GetObject is TListBox) then
              NotifyWinEvent(EVENT_OBJECT_FOCUS, AHwnd, LChildID, TListBox(LFmxAccForm.Focused.GetObject).ItemIndex + 1)
            else
              NotifyWinEvent(EVENT_OBJECT_FOCUS, AHwnd, OBJID_CLIENT, LChildID);

            LFmxAccForm.FInitFocus := LFmxAccForm.Focused.GetObject.Name;
          end;

          if (LFmxAccForm.Focused is TComboBox) then
          begin
            if TComboBox(LFmxAccForm.Focused).DroppedDown <> LFmxAccForm.FPrevDrop then
            begin
              LChildID := LFmxAccForm.GetFocusedChildID;
              NotifyWinEvent(EVENT_OBJECT_FOCUS, AHwnd, OBJID_CLIENT, LChildID);
            end;
            LFmxAccForm.FPrevDrop := (LFmxAccForm.Focused as TComboBox).DroppedDown;
          end;

          if (LFmxAccForm.Focused is TComboEdit) then
          begin
            if TComboEdit(LFmxAccForm.Focused).DroppedDown <> LFmxAccForm.FPrevDrop then
            begin
              LChildID := LFmxAccForm.GetFocusedChildID;
              NotifyWinEvent(EVENT_OBJECT_FOCUS, AHwnd, OBJID_CLIENT, LChildID);
            end;
            LFmxAccForm.FPrevDrop := (LFmxAccForm.Focused as TComboEdit).DroppedDown;
          end;
        end;
      end;
    WM_DESTROY:
      begin
        // Destroy the timer again
        KillTimer(AHwnd, ID_SCREENREADER);
      end;
  end;

  Result := CallWindowProc((LFmxAccForm as TAccForm).OldWinProc, AHwnd, uMsg, wParam, lParam);
end;


{ TFormAccessibility }


// Performs the specified object's default action. Not all objects have a default action.
// DISP_E_MEMBERNOTFOUND: The object does not support the method.
// This value is returned for controls that do not perform actions, such as edit fields.

function TFormAccessibility.accDoDefaultAction(varChild: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

//

function TFormAccessibility.accHitTest(xLeft, yTop: Integer; out pvarChild: OleVariant): HResult;
begin
  VariantInit(pvarChild);
  TVarData(pvarChild).VType := VT_I4;
  TVarData(pvarChild).VInteger := 1 + random(20);
  Result := S_OK;
end;

function TFormAccessibility.accLocation(out pxLeft, pyTop, pcxWidth, pcyHeight: Integer; varChild: OleVariant): HResult;
var
  ScaleFactor: single;
begin
  ScaleFactor := FFmxForm.Handle.Scale;
  pxLeft := Round(FFmxForm.Left * ScaleFactor);
  pyTop := Round(FFmxForm.Top * ScaleFactor);
  pcxWidth := Round(FFmxForm.Width * ScaleFactor);
  pcyHeight := Round(FFmxForm.Height * ScaleFactor);
  Result := S_OK;
end;

// No form-level acc navigate control implemented

function TFormAccessibility.accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// No form-level acc select control implemented

function TFormAccessibility.accSelect(flagsSelect: Integer; varChild: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Returns true when control is of type where no text value can be entered

function TFormAccessibility.IsCheckBox: Boolean;
var
  Obj: TFmxObject;
begin
  Result := False;
  if FFmxForm.Focused <> nil then
  begin
    Obj := FFmxForm.Focused.GetObject;
    if Obj <> nil then
      Result := (Obj is TCheckBox) or (Obj is TRadioButton);
  end;
end;

// Returns true when a checkbox or radiobutton is checked

function TFormAccessibility.GetControlChecked: Boolean;
var
  Obj: TFmxObject;
begin
    Result := False;
    if FFmxForm.Focused <> nil then
    begin
      Obj := FFmxForm.Focused.GetObject;
      if Obj <> nil then
        if (Obj is TCheckBox) then
          Result := TCheckbox(Obj).IsChecked
        else if (Obj is TRadioButton) then
          Result := TRadioButton(Obj).IsChecked;
    end;
end;

// Returns the classname of the focused control

function TFormAccessibility.GetControlName: string;
var
  Obj: TFmxObject;
begin
  Result := '';
  if FFmxForm.Focused <> nil then
  begin
    Obj := FFmxForm.Focused.GetObject;
    if Obj <> nil then
      Result := Obj.Name;
  end;
end;

// Returns the text value of the focused control
function TFormAccessibility.GetControlValue: string;
var
  Obj: TFmxObject;
  FmxAcc: IFMXAccessibility;
begin
  Result := '';
  if FFmxForm.Focused <> nil then
  begin
    Obj := FFmxForm.Focused.GetObject;

    if Obj <> nil then
    begin
      if Obj.GetInterface(IFMXAccessibility, FmxAcc) then
        Result := FmxAcc.GetControlText
      else if (Obj is TButton) then
        Result := TButton(Obj).Text
      else if (Obj is TSpeedButton) then
        Result := TSpeedButton(Obj).Text
      else if (Obj is TComboEdit) then
        Result := TComboEdit(Obj).Text
      else if (Obj is TListBox) then
      begin
        if TListBox(Obj).ItemIndex >= 0 then
          Result := TListBox(Obj).Items[TListBox(Obj).ItemIndex];
      end
      else if (Obj is TComboBox) then
      begin
        if TComboBox(Obj).ItemIndex >= 0 then
          Result := TComboBox(Obj).Items[TComboBox(Obj).ItemIndex];
      end
      else if (Obj is TEdit) then
        Result := TEdit(Obj).Text
      else if (Obj is TCustomEdit) and (Obj.Owner is TColumn) then
        Result := TCustomEdit(Obj).Text
      else if (Obj is TCheckBox) then
        Result := TCheckBox(Obj).Text
      else if (Obj is TRadioButton) then
        Result := TRadioButton(Obj).Text
      else if (Obj is TMemo) then
        Result := TMemo(Obj).Lines.Text
      else if (Obj is TTreeView) then
      begin
        if TTreeView(Obj).Selected <> nil then
          Result := TTreeView(Obj).Selected.Text;
      end;
    end;
  end;
end;

// Not used interface method
function TFormAccessibility.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer;
  DispIDs: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not used interface method
function TFormAccessibility.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not used interface method
function TFormAccessibility.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Returns the acc object for a child, based on child ID
function TFormAccessibility.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
var
  FmxAcc: TFmxAccessibility;
  LChildID: Integer;
begin
  Result := S_FALSE;
  ppdispChild := nil;

  if TVarData(varChild).VType <> VT_I4 then
    Result := E_INVALIDARG
  else
  begin
    LChildID := TVarData(varChild).VInteger;

    if (LChildID <= FFmxForm.GetChildControlsCount) and (LChildID > 0) then
    begin
      FmxAcc := TFmxAccessibility.Create;
      FmxAcc.FFmxObj := FFmxForm.GetChildControl(LChildID - 1);
      FmxAcc.FParent := FFmxForm;
      ppdispChild := FmxAcc;
      Result := S_OK;
    end;
  end;
end;

// Returns the number of acc enabled childs in the form
function TFormAccessibility.Get_accChildCount(out pcountChildren: Integer): HResult;
begin
  pcountChildren := FFmxForm.GetChildControlsCount;
  Result := S_OK;
end;

// Not implemented
function TFormAccessibility.Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult;
begin
  // no default action
  Result := S_FALSE;
end;

// Not implemented
function TFormAccessibility.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
begin
  // no description
  pszDescription := '';
  Result := S_FALSE;
end;

// Return the acc object ID of the focused form child
function TFormAccessibility.Get_accFocus(out pvarChild: OleVariant): HResult;
begin
  VariantInit(pvarChild);

  if FFmxForm.Focused <> nil then
  begin
    TVarData(pvarChild).VType := VT_I4;
    // return index
    TVarData(pvarChild).VType := VT_I4;
    TVarData(pvarChild).VInteger := FFmxForm.GetFocusedChildID;
  end
  else
    TVarData(pvarChild).VType := VT_EMPTY;
  Result := S_OK;
end;

// Not implemented
function TFormAccessibility.Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult;
begin
  Result := S_FALSE;
end;

// Not implemented
function TFormAccessibility.Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
  out pidTopic: Integer): HResult;
begin
  Result := S_FALSE;
end;

// Not implemented
function TFormAccessibility.Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult;
begin
  Result := S_FALSE;
end;

// Return the name of the focused child control
function TFormAccessibility.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
begin
  Result := S_OK;
  pszName := GetControlName;
end;

// Form has no parent
function TFormAccessibility.Get_accParent(out ppdispParent: IDispatch): HResult;
begin
  Result := S_FALSE;
end;

// Return the role for the focused form control
function TFormAccessibility.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
var
  Obj: TFmxObject;
  LChildID: Integer;
begin
  VariantInit(pvarRole);

  if TVarData(varChild).VType = VT_I4 then
  begin
    LChildID := TVarData(varChild).VInteger;

    // child is form itself
    if LChildID = CHILDID_SELF then
    begin
      TVarData(pvarRole).VType := VT_I4;
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_WINDOW;
      Result := S_OK;
      Exit;
    end
    else
    begin
      // get the role of the child
      Obj := TFMXObject(FFmxForm.Components[LChildID - 1]);

      TVarData(pvarRole).VType := VT_I4;
      TVarData(pvarRole).VInteger := GetControlRole(Obj);
      Result := S_OK;
      Exit;
    end;
  end
  else
  begin
    TVarData(pvarRole).VType := VT_EMPTY;
    Result := S_OK;
  end;
end;

// Not implemented
function TFormAccessibility.Get_accSelection(out pvarChildren: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Return the state of the focused form control
function TFormAccessibility.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
const
  IsEnabled: array[Boolean] of Integer = (STATE_SYSTEM_UNAVAILABLE, 0);
  HasPopup: array[Boolean] of Integer = (0, STATE_SYSTEM_HASPOPUP);
  IsVisible: array[Boolean] of Integer = (STATE_SYSTEM_INVISIBLE, 0);
  IsChecked: array[Boolean] of Integer = (0, STATE_SYSTEM_CHECKED);
begin
  Result := S_OK;
  VariantInit(pvarState);

  if TVarData(varChild).VType <> VT_I4 then
  begin
    TVarData(pvarState).VType := VT_EMPTY;
    Result := E_INVALIDARG;
    Exit;
  end;

  TVarData(pvarState).VType := VT_I4;

  if (varChild = CHILDID_SELF) then
  begin
    TVarData(pvarState).VInteger := STATE_SYSTEM_FOCUSED or STATE_SYSTEM_FOCUSABLE or STATE_SYSTEM_HOTTRACKED;
    TVarData(pvarState).VInteger := TVarData(pvarState).VInteger or IsVisible[true];
    TVarData(pvarState).VInteger := TVarData(pvarState).VInteger or IsEnabled[true];
  end;
end;

// Return the form text or the text value of the focused child control
function TFormAccessibility.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
var
  LChildID: integer;
  Obj: TFmxObject;
begin
  if TVarData(varChild).VType <> VT_I4 then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  LChildID := TVarData(varChild).VInteger;

  if LChildID = CHILDID_SELF then
  begin
    pszValue := FFmxForm.Caption;
    Result := S_OK;
  end
  else
  begin
    Obj := TFMXObject(FFmxForm.Components[LChildID -1]);
    pszValue := GetControlTextValue(Obj);
    Result := S_OK;
  end;
end;

// Not implemented interface method
function TFormAccessibility.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
  var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method
function TFormAccessibility.Set_accName(varChild: OleVariant; const pszName: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND
end;

function TFormAccessibility.Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult;
var
  LChildID: integer;
begin
  if TVarData(varChild).VType <> VT_I4 then
  begin
    Result := E_INVALIDARG;
    Exit;
  end;

  LChildID := TVarData(varChild).VInteger;

  if LChildID = CHILDID_SELF then
  begin
    FFmxForm.Caption := pszValue;
    Result := S_OK;
  end
  else
    Result := E_FAIL;
end;

{ TFmxAccessibility }

// Performs the specified object's default action. Not all objects have a default action.
// DISP_E_MEMBERNOTFOUND: The object does not support the method.
// This value is returned for controls that do not perform actions, such as edit fields.

function TFmxAccessibility.accDoDefaultAction(varChild: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the child element or child object that is displayed at a specific point on the screen.

function TFmxAccessibility.accHitTest(xLeft, yTop: Integer; out pvarChild: OleVariant): HResult;
begin
  VariantInit(pvarChild);
  TVarData(pvarChild).VType := VT_EMPTY;  {VT_I4}
  Result := S_FALSE;
end;

// Retrieves the specified object's current screen location.
// All visual objects must support this method.

function TFmxAccessibility.accLocation(out pxLeft, pyTop, pcxWidth, pcyHeight: Integer; varChild: OleVariant): HResult;
var
  LPointF: TPointF;
  LScaleFactor: single;
  LControl: TControl;

function GetPoint(const AControl: TControl): TPointF;
begin
  Result := TPointF.Zero;
  if AControl.ParentedVisible then
  begin
    Result := AControl.Position.Point;
    if (AControl.Parent <> nil) and (AControl.Parent is TControl) then
      Result := TControl(AControl.Parent).LocalToAbsolute(AControl.Position.Point);
  end;
end;

begin
  Result := S_FALSE;

  if (FFmxObj is TControl) and (FFmxObj.Parent <> nil) then
  begin
    LControl := TControl(FFmxObj);
    LPointF := GetPoint(LControl);
    if LPointF <> TPointF.Zero then
    begin
      LScaleFactor := FParent.Handle.Scale;
      LPointF := FParent.ClientToScreen(LPointF);
      pxLeft := Round(LPointF.X * LScaleFactor);
      pyTop := Round(LPointF.Y * LScaleFactor);
      pcxWidth := Round(LControl.Width * LScaleFactor);
      pcyHeight := Round(LControl.Height * LScaleFactor);
      Result := S_OK;
    end
    else
      Result := S_FALSE;
  end;
end;
// Traverses to another UI element within a container and retrieves the object.
// This method is optional.

function TFmxAccessibility.accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Modifies the selection or moves the keyboard focus of the specified object.
// All objects that support selection or receive the keyboard focus must support this method.

function TFmxAccessibility.accSelect(flagsSelect: Integer; varChild: OleVariant): HResult;
begin
  Result := S_OK;
end;

// Retrieves an IDispatch for the specified child, if one exists. All objects must support this property.
// S_FALSE : The child is not an accessible object.
// S_OK: varChild = ID of child control, return acc object for this child

function TFmxAccessibility.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
var
  LChildID: Integer;
  FmxAcc: TFmxChildAccessibility;
begin
  Result := S_FALSE;

  if TVarData(varChild).VType <> VT_I4 then
    Result := E_INVALIDARG
  else
  begin
    LChildID := TVarData(varChild).VInteger;

    if (FFmxObj is TListBox) and (LChildID <> CHILDID_SELF) then
    begin
      FmxAcc := TFmxChildAccessibility.Create;
      if (LChildID > 0) and (LChildID <= TListBox(FFmxObj).Items.Count) then
      begin
        FmxAcc.FChildText := TListBox(FFmxObj).Items[LChildID - 1];
        FmxAcc.FFmxObj := FFmxObj;
        ppdispChild := FmxAcc;

        Result := S_OK;
      end;
    end;
  end;
end;

// Retrieves the number of children that belong to this object. All objects must support this property.

function TFmxAccessibility.Get_accChildCount(out pcountChildren: Integer): HResult;
begin
  pcountChildren := 0;

  if (FFmxObj is TListBox)  then
    pcountChildren := TListBox(FFmxObj).Items.Count
  else if (FFmxObj is TComboBox) then
    pcountChildren := TComboBox(FFmxObj).Items.Count
  else if (FFmxObj is TComboEdit) then
    pcountChildren := TComboEdit(FFmxObj).Items.Count
  else if (FFmxObj is TTreeView) and (TTreeView(FFmxObj).Selected <> nil) then
      pcountchildren := TTreeView(FFmxObj).Selected.Count;
  Result := S_OK;
end;

// Retrieves a string that indicates the object's default action. Not all objects have a default action.

function TFmxAccessibility.Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves a string that describes the visual appearance of the specified object. Not all objects have a description.
// The Description property is often used incorrectly and is not supported by Microsoft UI Automation.
// Microsoft Active Accessibility server developers should not use this property.
// If more information is needed for accessibility and automation scenarios, use the properties supported by UI Automation
// elements and control patterns.

function TFmxAccessibility.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the object that has the keyboard focus.
// All objects that may receive the keyboard focus must support this property.
// for lists, should return the ID of the selected item

function TFmxAccessibility.Get_accFocus(out pvarChild: OleVariant): HResult;
begin
  VariantInit(pvarChild);
  TVarData(pvarChild).VType := VT_I4;
  TVarData(pvarChild).VInteger := CHILDID_SELF;
  if (FFmxObj is TListBox) and (TListBox(FFmxObj).ItemIndex >= 0) then
    TVarData(pvarChild).VInteger := TListBox(FFmxObj).ItemIndex;

  Result := S_OK;
end;

// Retrieves the Help property string of an object. Not all objects support this

function TFmxAccessibility.Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the full path of the WinHelp file that is associated with the specified object;
// it also retrieves the identifier of the appropriate topic within that file

function TFmxAccessibility.Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant;
  out pidTopic: Integer): HResult;
begin
  pidTopic := 0;
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Not implemented

function TFmxAccessibility.Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult;
begin
  Result := S_FALSE;
end;

// Method retrieves the name of the specified object. All objects support this property.
// Many objects such as icons, menus, check boxes, combo boxes, and other controls have labels that are displayed to users.
// Any label that is displayed to users is used for the object's name property. For more information, see the Name Property.
// Note to server developers:
// If you are using menu or button text for the Name property, remove any ampersands (&) marking the keyboard access keys

function TFmxAccessibility.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
begin
  Result := S_OK;
  pszName := '';
  if not ((FFmxObj is TEdit) or (FFmxObj is TMemo) or (FFmxObj is TLabel) or (FFmxObj is TListBox) or
    (FFmxObj is TComboEdit) or (FFmxObj is TComboBox))  then
    pszName := GetControlTextValue(FFmxObj)
  else
    Result := S_FALSE;
end;

// Retrieves the IDispatch of the object's parent. All objects support this property.

function TFmxAccessibility.Get_accParent(out ppdispParent: IDispatch): HResult;
var
  FrmAcc: TFormAccessibility;
begin
  Result := S_FALSE;
  if (FParent <> nil) and (FParent is TAccForm) then
  begin
    FrmAcc := TFormAccessibility.Create;
    FrmAcc.FFmxForm := TAccForm(FParent);
    ppdispParent := FrmAcc;
    Result := S_OK;
  end;
end;

// Retrieves information that describes the role of the specified object.
// All objects support this property.

function TFmxAccessibility.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
var
  Obj: TFmxObject;
  FmxAcc: IFMXAccessibility;
begin
  VariantInit(pvarRole);
  TVarData(pvarRole).VType := VT_I4;
  if TVarData(pvarRole).VType <> VT_I4 then
  begin
    TVarData(pvarRole).VType := VT_EMPTY;
    Result := E_INVALIDARG;
    Exit;
  end;

  Result := S_FALSE;

  Obj := FFmxObj;
  if Obj <> nil then
  begin
    Result := S_OK;
    VariantInit(pvarRole);
    TVarData(pvarRole).VType := VT_I4;
    TVarData(pvarRole).VInteger := ROLE_SYSTEM_WINDOW;

    if Obj.GetInterface(IFMXAccessibility, FmxAcc) then
    begin
      TVarData(pvarRole).VInteger := GetControlRole(Obj);
      Exit;
    end;

    if (Obj is TControl) and (Obj.Owner is TColumn) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_CELL
    else if (Obj is TCustomGrid) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_CELL
    else if (Obj is TButton) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_PUSHBUTTON
    else if (Obj is TSpeedButton) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_PUSHBUTTON
    else if (Obj is TComboEdit) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_COMBOBOX
    else if (Obj is TListBox) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_LIST
    else if (Obj is TComboBox) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_COMBOBOX
    else if (Obj is TEdit) then
    begin
      if TEdit(Obj).ReadOnly then
        TVarData(pvarRole).VInteger := ROLE_SYSTEM_STATICTEXT
      else
        TVarData(pvarRole).VInteger := ROLE_SYSTEM_TEXT;
    end
    else if (Obj is TSpinBox) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_SPINBUTTON
    else if (Obj is TTrackBar) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_SLIDER
    else if (Obj is TLabel) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_STATICTEXT
    else if (Obj is TCheckBox) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_CHECKBUTTON
    else if (Obj is TRadioButton) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_RADIOBUTTON
    else if (Obj is TMemo) then
    begin
      if TMemo(Obj).ReadOnly then
        TVarData(pvarRole).VInteger := ROLE_SYSTEM_STATICTEXT
      else
        TVarData(pvarRole).VInteger := ROLE_SYSTEM_TEXT;
    end
    else if (Obj is TTreeView) then
      TVarData(pvarRole).VInteger := ROLE_SYSTEM_OUTLINEITEM;
  end;
end;

// Retrieves the selected children of this object.
// All objects that support selection must support this property.

function TFmxAccessibility.Get_accSelection(out pvarChildren: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the current state of the specified object. All objects support this

function TFmxAccessibility.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
const
  IsEnabled: array[Boolean] of Integer = (STATE_SYSTEM_UNAVAILABLE, 0);
  HasPopup: array[Boolean] of Integer = (0, STATE_SYSTEM_HASPOPUP);
  IsVisible: array[Boolean] of Integer = (STATE_SYSTEM_INVISIBLE, 0);
  IsChecked: array[Boolean] of Integer = (0, STATE_SYSTEM_CHECKED);
begin
  Result := S_OK;

  VariantInit(pvarState);
  TVarData(pvarState).VType := VT_I4;

  if (varChild = CHILDID_SELF) then
  begin
    TVarData(pvarState).VInteger := STATE_SYSTEM_FOCUSED or STATE_SYSTEM_FOCUSABLE;

    if FFmxObj <> nil then
    begin
      if IsTreeView then
      begin
        if TTreeView(FFmxObj).Selected.Count > 0 then
        begin
          if TTreeView(FFmxObj).Selected.IsExpanded then
            TVarData(pvarState).VInteger := TVarData(pvarState).VInteger or STATE_SYSTEM_EXPANDED
          else
            TVarData(pvarState).VInteger := TVarData(pvarState).VInteger or STATE_SYSTEM_COLLAPSED;
        end;
      end;

      // checkbox or radiobutton state
      if IsCheckBox then
      begin
        TVarData(pvarState).VInteger := TVarData(pvarState).VInteger or IsChecked[GetControlChecked];
      end;
    end;
  end;
end;

// Retrieves the value of the specified object.
// Numeric values returned from scroll bar and trackbar accessible objects indicate percentages.
// They are integers between zero (0) and one hundred (100), inclusive, but might also be a limited range for example,
// between one (1) and sixteen (16). Also, some scroll bar and trackbar objects return strings that correspond to settings
// such as screen size or Internet security.

function TFmxAccessibility.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
begin
  Result := s_OK;

  if (FFmxObj is TComboBox) or (FFmxObj is TComboEdit) then
  begin
    pszValue := GetControlTextValue(FFmxObj);
    if pszValue = ''  then
      pszValue := 'No value selected';
  end
  else
  begin
    pszValue := GetControlTextValue(FFmxObj);
  end;

  if (FFmxObj is TCustomGrid) then
    pszValue := '';
end;

// Returns true when a checkbox or radiobutton is checked

function TFmxAccessibility.GetControlChecked: Boolean;
var
  Obj: TFmxObject;
begin
  Result := false;
  Obj := FFmxObj;

  if Obj <> nil then
  begin
    if (Obj is TCheckBox) then
      Result := TCheckBox(Obj).IsChecked
    else if (Obj is TRadioButton) then
      Result := TRadioButton(Obj).IsChecked;
  end;
end;

// Return the name of the focused control

function TFmxAccessibility.GetControlName: string;
begin
    Result := '';
    if FFmxObj <> nil then
      Result := FFmxObj.Name;
end;

// Return the text of the focused control

function TFmxAccessibility.GetControlValue: string;
begin
  Result := GetControlTextValue(FFmxObj);
end;

// Not implemented interface method

function TFmxAccessibility.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer;
  DispIDs: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method

function TFmxAccessibility.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method

function TFmxAccessibility.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TFmxAccessibility.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params;
  VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method

function TFmxAccessibility.IsCheckBox: Boolean;
begin
  Result := (FFmxObj is TCheckBox) or (FFmxObj is TRadioButton);
end;

// Return true when the focused control is a treeview

function TFmxAccessibility.IsTreeView: Boolean;
begin
  Result := (FFmxObj is TTreeView);
end;

// Not implemented interface method

function TFmxAccessibility.Set_accName(varChild: OleVariant; const pszName: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND
end;

function TFmxAccessibility.Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult;
begin
  if SetControlTextValue(FFmxObj, pszValue) then
    Result := s_OK
  else
    Result := E_FAIL;
end;

{ TFmxChildAccessibility }

// Accessibility object that handles with control child elements like listbox items, combobox items

// Performs the specified object's default action. Not all objects have a default action.
// DISP_E_MEMBERNOTFOUND: The object does not support the method.
// This value is returned for controls that do not perform actions, such as edit fields.

function TFmxChildAccessibility.accDoDefaultAction(varChild: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the child element or child object that is displayed at a specific point on the screen.

function TFmxChildAccessibility.accHitTest(xLeft, yTop: Integer; out pvarChild: OleVariant): HResult;
begin
  VariantInit(pvarChild);
  TVarData(pvarChild).VType := VT_EMPTY;
  Result := S_FALSE;
end;

// Retrieves the specified object's current screen location.
// All visual objects must support this method.

function TFmxChildAccessibility.accLocation(out pxLeft, pyTop, pcxWidth, pcyHeight: Integer; varChild: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Traverses to another UI element within a container and retrieves the object.
// This method is optional.

function TFmxChildAccessibility.accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Modifies the selection or moves the keyboard focus of the specified object.
// All objects that support selection or receive the keyboard focus must support this method.

function TFmxChildAccessibility.accSelect(flagsSelect: Integer; varChild: OleVariant): HResult;
begin
  Result := S_OK;
end;

// Retrieves an IDispatch for the specified child, if one exists. All objects must support this property.
// S_FALSE : The child is not an accessible object.
// S_OK: varChild = ID of child control, return acc object for this child

function TFmxChildAccessibility.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
begin
  Result := S_FALSE;
end;

// Retrieves the number of children that belong to this object. All objects must support this property.

function TFmxChildAccessibility.Get_accChildCount(out pcountChildren: Integer): HResult;
begin
  pcountChildren := 0;
  Result := S_OK;
end;

// Retrieves a string that indicates the object's default action. Not all objects have a default action.

function TFmxChildAccessibility.Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves a string that describes the visual appearance of the specified object. Not all objects have a description.
// The Description property is often used incorrectly and is not supported by Microsoft UI Automation.
// Microsoft Active Accessibility server developers should not use this property.
// If more information is needed for accessibility and automation scenarios, use the properties supported by UI Automation
// elements and control patterns.

function TFmxChildAccessibility.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
begin
  pszDescription := '';
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the object that has the keyboard focus.
// All objects that may receive the keyboard focus must support this property.
// for lists, should return the ID of the selected item

function TFmxChildAccessibility.Get_accFocus(out pvarChild: OleVariant): HResult;
begin
  VariantInit(pvarChild);
  TVarData(pvarChild).VType := VT_I4;
  TVarData(pvarChild).VInteger := CHILDID_SELF;
  Result := S_OK;
end;

// Retrieves the Help property string of an object. Not all objects support this

function TFmxChildAccessibility.Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the full path of the WinHelp file that is associated with the specified object;
// it also retrieves the identifier of the appropriate topic within that file

function TFmxChildAccessibility.Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HResult;
begin
  pidTopic := 0;
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Not implemented interface method

function TFmxChildAccessibility.Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult;
begin
  Result := S_FALSE;
end;

// Method retrieves the name of the specified object. All objects support this property.
// Many objects such as icons, menus, check boxes, combo boxes, and other controls have labels that are displayed to users.
// Any label that is displayed to users is used for the object's name property. For more information, see the Name Property.
// Note to server developers:
// If you are using menu or button text for the Name property, remove any ampersands (&) marking the keyboard access keys

function TFmxChildAccessibility.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
begin
  Result := S_OK;
  pszName := '';
end;

// Retrieves the IDispatch of the object's parent. All objects support this property.

function TFmxChildAccessibility.Get_accParent(out ppdispParent: IDispatch): HResult;
var
  FrmAcc: TFmxAccessibility;
begin
  Result := S_FALSE;
  if FFmxObj <> nil then
  begin
    FrmAcc := TFmxAccessibility.Create;
    FrmAcc.FFmxObj := FFmxObj;
    ppdispParent := FrmAcc;
    Result := S_OK;
  end;
end;

// Retrieves information that describes the role of the specified object.
// All objects support this property.

function TFmxChildAccessibility.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
begin
  VariantInit(pvarRole);
  TVarData(pvarRole).VType := VT_I4;
  TVarData(pvarRole).VInteger := ROLE_SYSTEM_CELL;
  Result := S_OK;
end;

// Retrieves the selected children of this object.
// All objects that support selection must support this property.

function TFmxChildAccessibility.Get_accSelection(out pvarChildren: OleVariant): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND;
end;

// Retrieves the current state of the specified object. All objects support this

function TFmxChildAccessibility.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
begin
  VariantInit(pvarState);
  TVarData(pvarState).VType := VT_I4;
  TVarData(pvarState).VInteger := STATE_SYSTEM_FOCUSED or STATE_SYSTEM_FOCUSABLE;
  Result := S_OK;
end;

// Retrieves the value of the specified object.
// Numeric values returned from scroll bar and trackbar accessible objects indicate percentages.
// They are integers between zero (0) and one hundred (100), inclusive, but might also be a limited range for example,
// between one (1) and sixteen (16). Also, some scroll bar and trackbar objects return strings that correspond to settings
// such as screen size or Internet security.

function TFmxChildAccessibility.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
begin
  pszValue := FChildText;
  Result := S_OK;
end;

// Not implemented interface method

function TFmxChildAccessibility.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer;
  DispIDs: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method

function TFmxChildAccessibility.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method

function TFmxChildAccessibility.GetTypeInfoCount(out Count: Integer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method

function TFmxChildAccessibility.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word;
  var Params; VarResult, ExcepInfo, ArgErr: Pointer): HRESULT;
begin
  Result := E_NOTIMPL;
end;

// Not implemented interface method

function TFmxChildAccessibility.Set_accName(varChild: OleVariant; const pszName: WideString): HResult;
begin
  Result := DISP_E_MEMBERNOTFOUND
end;

function TFmxChildAccessibility.Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult;
begin
  if SetControlTextValue(FFmxObj, pszValue) then
    Result := s_OK
  else
    Result := E_FAIL;
end;

{ TAccForm }

constructor TAccForm.Create(AOwner: TComponent);
begin
  inherited;
  if FControlList = nil then
    FControlList := TList<TFMXObject>.Create;
end;

// Override the method where the form window handle is created to replace the main Window proc and install a timer

procedure TAccForm.CreateHandle;
var
  LHandle: HWND;
begin
  inherited;

  // override Window proc handler
  LHandle := WindowHandleToPlatform(Handle).Wnd;
  OldWinProc := Pointer(GetWindowLong(LHandle, GWL_WNDPROC));
  NewWinProc := Pointer(SetWindowLong(LHandle, GWL_WNDPROC, LONG_PTR(@ScreenReaderWndProc)));

  SetTimer(LHandle,ID_SCREENREADER,250,nil);

  // init vars
  FPrevFocus := '';
  FPrevIdx := -1;
  FPrevSel := nil;
  FPrevExpanded := false;
  FPrevEx := 0;
  FPrevCell := Point(-1,-1);
end;

// Returns true when a control has focus that has child items

function TAccForm.IsFocusedList: Boolean;
var
  Obj: TFmxObject;
begin
  Result := False;
  if Focused <> nil then
  begin
    Obj := Focused.GetObject;
    if Obj <> nil then
      Result := (Obj is TComboBox) or (Obj is TComboEdit) or (Obj is TListBox);
  end;
end;

// Trigger acc when the focused control has changed

procedure TAccForm.UpdateFocusedControl;
var
  SCol: TColumn;
  SGrid: TCustomGrid;
  ChildID: Integer;
begin
  if Focused = nil then
    Exit;

  ChildID := GetFocusedChildID;

  if (Focused.GetObject.Name = '') then
  begin
    // Test for grid with inplace editor active
    if Focused.Parent <> nil then
    begin
      if (Focused is TControl) and (TControl(Focused).Owner is TColumn) then
      begin
        SCol := TColumn(TControl(Focused).Owner);
        if SCol.PresentedControl is TCustomGrid then
          SGrid := TCustomGrid(SCol.PresentedControl)
        else
          SGrid := nil;

        if (SGrid.ColumnIndex <> FPrevCell.X) then
        begin
          NotifyWinEvent(EVENT_OBJECT_FOCUS, FHWND, OBJID_CLIENT, ChildID);
          FPrevCell := Point(SGrid.ColumnIndex, SGrid.Selected);
          Exit;
        end
        else
        if (SGrid.Selected <> FPrevCell.Y) then
        begin
          // Force different control ID list so remaining on same column is
          // seen as focus change and control value is reread
          FShiftChilds := not FShiftChilds;
          ChildID := GetFocusedChildID;
          NotifyWinEvent(EVENT_OBJECT_FOCUS, FHWND, OBJID_CLIENT, ChildID);
          FPrevCell := Point(SGrid.ColumnIndex, SGrid.Selected);
          Exit;
        end;
      end;
    end;
  end;

  // Test for readonly grid cell
  if (Focused is TCustomGrid) then
  begin
    SGrid := TCustomGrid(Focused);

    if (SGrid.ColumnIndex <> FPrevCell.X) or (SGrid.Selected <> FPrevCell.Y) then
    begin
      NotifyWinEvent(EVENT_OBJECT_FOCUS, FHWND, OBJID_CLIENT, ChildID);
      FPrevCell := Point(SGrid.ColumnIndex, SGrid.Selected);
      Exit;
    end;
  end;

  // Test for checkbox value change
  if (Focused.GetObject is TCheckBox) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    NotifyWinEvent(EVENT_OBJECT_STATECHANGE, FHWND, OBJID_CLIENT, ChildID);
    Exit;
  end;

  // Test for checkbox value change
  if (Focused.GetObject is TRadioButton) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    NotifyWinEvent(EVENT_OBJECT_STATECHANGE, FHWND, OBJID_CLIENT, ChildID);
    Exit;
  end;

  // Test for trackbar value change
  if (Focused.GetObject is TTrackBar) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    NotifyWinEvent(EVENT_OBJECT_VALUECHANGE, FHWND, OBJID_CLIENT, ChildID);
    Exit;
  end;

  // Test for edit caret
  if (Focused.GetObject is TEdit) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    if TEdit(Focused.GetObject).SelStart <> FPrevCaret then
    begin
      NotifyWinEvent(EVENT_OBJECT_TEXTSELECTIONCHANGED, FHWND, OBJID_CLIENT, ChildID);
      NotifyWinEvent(EVENT_OBJECT_LOCATIONCHANGE, FHWND, OBJID_CLIENT, ChildID);
    end;

    FPrevCaret := TEdit(Focused.GetObject).SelStart;
    Exit;
  end;

  // Test for spinbox value change
  if (Focused.GetObject is TSpinBox) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    if FPrevStr <> TSpinBox(Focused.GetObject).Text then
      NotifyWinEvent(EVENT_OBJECT_VALUECHANGE, FHWND, OBJID_CLIENT, ChildID);

    FPrevStr := TSpinBox(Focused.GetObject).Text;
    Exit;
  end;

  // Test for listbox index change
  if (Focused.GetObject is TListBox) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    if FPrevIdx <> TListBox(Focused.GetObject).ItemIndex then
    begin
      NotifyWinEvent(EVENT_OBJECT_FOCUS, FHWND, ChildID , TListBox(Focused.GetObject).ItemIndex + 1);
      FPrevIdx := TListBox(Focused.GetObject).ItemIndex;
      Exit;
    end;
  end;

  // Test for combobox index change
  if (Focused.GetObject is TComboBox) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    if FPrevIdx <> TComboBox(Focused.GetObject).ItemIndex then
      NotifyWinEvent(EVENT_OBJECT_VALUECHANGE, FHWND, OBJID_CLIENT, ChildID);

    FPrevIdx := TComboBox(Focused.GetObject).ItemIndex;
    Exit;
  end;

  // Test for combobox index or value change
  if (Focused.GetObject is TComboEdit) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    if FPrevIdx <> TComboEdit(Focused.GetObject).ItemIndex then
      NotifyWinEvent(EVENT_OBJECT_FOCUS, FHWND, OBJID_CLIENT, ChildID);

    FPrevIdx := TComboEdit(Focused.GetObject).ItemIndex;
    Exit;
  end;

  // Test for treeview node change
  if (Focused.GetObject is TTreeView) and (Focused.GetObject.Name = FPrevFocus) then
  begin
    if FPrevExpanded <> TTreeView(Focused.GetObject).Selected.IsExpanded then
    begin
      NotifyWinEvent(EVENT_OBJECT_VALUECHANGE, FHWND, OBJID_CLIENT, ChildID);
      FPrevExpanded := TTreeView(Focused.GetObject).Selected.IsExpanded;
    end;

    if FPrevSel <> TTreeView(Focused.GetObject).Selected then
    begin
      NotifyWinEvent(EVENT_OBJECT_FOCUS, FHWND, OBJID_CLIENT, ChildID);
      FPrevSel := TTreeView(Focused.GetObject).Selected;
    end;
    Exit;
  end;

  // Test for general control focus change
  if FPrevFocus <> Focused.GetObject.Name then
  begin
    NotifyWinEvent(EVENT_OBJECT_FOCUS, FHWND, OBJID_CLIENT, ChildID);

    FPrevFocus := Focused.GetObject.Name;
    FPrevIdx := -2;
    FPrevCell := Point(-1,-1);
    FPrevCaret := -1;
    FPrevDrop := false;

    if (IsFocusedList) then
      NotifyWinEvent(EVENT_OBJECT_VALUECHANGE, FHWND, OBJID_CLIENT, ChildID);
  end;
end;

// Return the nr. of child controls in the form

function TAccForm.GetChildControlsCount: integer;

  procedure GetChildsCount(Ctrl: TFmxObject);
  var
    I: Integer;
    Child: TFmxObject;
  begin
    for I := 0 to Ctrl.ChildrenCount - 1 do
    begin
      Child := Ctrl.Children[I];

      if (Child is TFrame) or (Child is TLayout) or (Child is TCustomGrid) or (Child is TColumn) or (Child is TPanel) or
        (Child is TScrollBox) or (Child is TContent) or (Child is TTabControl) or (Child is TTabItem) then
      begin
        FControlList.Add(Child);
        GetChildsCount(Child as TControl);
      end
      else
        FControlList.Add(Child);
    end;
  end;

begin
  FControlList.Clear;
  GetChildsCount(Self);

  if FShiftChilds then
    ReorderChilds;

  Result := FControlList.Count;
end;


constructor TAccForm.CreateNew(AOwner: TComponent; Dummy: NativeInt = 0);
begin
  inherited;
  if FControlList = nil then
    FControlList := TList<TFmxObject>.Create;
end;

destructor TAccForm.Destroy;
begin
  FControlList.Free;
  inherited;
end;

// Return a child control instance based on ID

function TAccForm.GetChildControl(Index: Integer): TFmxObject;
begin
  Result := nil;
  if (Index < FControlList.Count) then
    Result := FControlList.Items[Index];
end;

// Get the ID of the focused form control

function TAccForm.GetFocusedChildID: integer;
var
  I: Integer;
  Count: Integer;
  Obj, Child: TFmxObject;
begin
  Result := 0;

  if Focused = nil then
    Exit;

  Count := GetChildControlsCount;
  Obj := Focused.GetObject;

  for I := 0 to Count - 1  do
  begin
    Child := GetChildControl(I);
    if (Obj = Child) then
    begin
      Result := I + 1;
      Break;
    end;
  end;
end;

procedure TAccForm.ReorderChilds;
var
  Obj: TFmxObject;
begin
  if FControlList.Count = 0 then
    Exit;
  Obj := FControlList.Items[0];
  FControlList.Delete(0);
  FControlList.Add(Obj);
end;

end.
