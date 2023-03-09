{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ScreenReader;

interface

uses
  FMX.Types,
  FMX.Forms;

{$SCOPEDENUMS ON}

type
  TFMXRole = (StaticText, Cell, Text, Button, CheckBox, RadioButton, Grid, List, Slider, ComboBox, TabGroup);

  // Interface for custom controls to add accessibility support
  IFMXAccessibility = interface
  ['{49AF90E5-341A-4869-9D3E-F659670FB4D8}']
    // Return the text to read by the accessibility interface
    function GetControlText: string;
    // Returns the accesibility role of the custom control
    function GetRole: TFMXRole;
  end;

// Returns the text to read for the focused control in the form
function GetFocusControlText(const AForm: TForm): string;

// Returns true when a FireMonkey object is a child control of the form
function IsFormControl(const AForm: TForm; const AObj: TFmxObject): Boolean;

// Retrieve the text to read for various FireMonkey control types
function GetControlTextValue(const AObj: TFmxObject): string;

function SetControlTextValue(const AObj: TFmxObject; const AValue: WideString): Boolean;

implementation

uses
  System.RTTI, System.SysUtils, FMX.StdCtrls, FMX.Grid, FMX.TreeView, FMX.ListBox,
  FMX.Edit;

function IsFormControl(const AForm: TForm; const AObj: TFmxObject): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to AForm.ComponentCount - 1 do
  begin
    if AObj = AForm.Components[I] then
    begin
      Result := True;
      Break;
    end;
  end;
                                
//  if not Result then
//  begin
//    Result := AObj is TTextCell;
//  end;
end;

function GetFocusControlText(const AForm: TForm): string;
var
  Obj: TFmxObject;
begin
  Result := '';
  if AForm.Focused <> nil then
  begin
    Obj := AForm.Focused.GetObject;
    Result := GetControlTextValue(Obj);
  end;
end;

function GetControlTextValue(const AObj: TFmxObject): string;
var
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  RttiPropText: TRttiProperty;
  RttiPropVal: TRttiProperty;
  ValueControl: Boolean;
  ExtValue: Double;
  AccIntf: IFMXAccessibility;
  LGrid: TCustomGrid;
begin
  Result := '';

  if AObj <> nil then
  begin
    if AObj.GetInterface(IFMXAccessibility, AccIntf) then
    begin
      Result := AccIntf.GetControlText;
      Exit;
    end;

    if (AObj is TListBox) and (TListBox(AObj).ItemIndex >= 0) then
        Result := TListBox(AObj).Items[TListBox(AObj).ItemIndex]
    else if (AObj is TComboBox) and ((AObj as TComboBox).ItemIndex >= 0) then
        Result := (AObj as TComboBox).Items[(AObj as TComboBox).ItemIndex]
    else if (AObj is TCustomGrid) then
    begin
      LGrid := TCustomGrid(AObj);
      if (LGrid.Col >= 0) and (LGrid.Col < LGrid.ColumnCount) and (LGrid.Row >= 0) and (LGrid.Row < LGrid.RowCount) then
        Result := LGrid.Columns[LGrid.Col].ValueToString(LGrid.Model.GetValue(LGrid.Col, LGrid.Row, False))
    end
    else if AObj is TCustomEdit then
      Result := TCustomEdit(AObj).Text
    else if (AObj is TTreeView) and (TTreeView(AObj).Selected <> nil) then
        Result := TTreeview(AObj).Selected.Text
    else
    begin
      RttiCtx := TRttiContext.Create;
      try
        RttiType := RttiCtx.GetType(AObj.ClassInfo);
        RttiPropText := RttiType.GetProperty('Text');
        RttiPropVal := RttiType.GetProperty('Value');

        ValueControl := (AObj is TTrackBar) or (AObj is TScrollBar);

        if not ValueControl and (RttiPropText <> nil) then
           Result := RttiPropText.GetValue(AObj).AsString
        else if RttiPropVal <> nil then
        begin
          ExtValue := RttiPropVal.GetValue(AObj).AsExtended;
          Result := Format('%.2g',[ExtValue]);
        end;
      finally
        RttiCtx.Free;
      end;
    end;
  end;
end;

function SetControlTextValue(const AObj: TFmxObject; const AValue: WideString): Boolean;
var
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  RttiProp: TRttiProperty;
begin
  Result := False;

  if AObj <> nil then
  begin
    RttiCtx := TRttiContext.Create;
    RttiType := RttiCtx.GetType(AObj.ClassInfo);
    for RttiProp in RttiType.GetProperties() do
      if RttiProp.IsWritable and (RttiProp.Name = 'Text') then
        RttiProp.SetValue(AObj, TValue.From(AValue));
    Result := True;
  end;
end;

end.
