{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FiltEdit;

interface

uses Windows, Messages, Classes, Graphics, Forms, Controls, Tabs,
  Buttons, DesignIntf, DesignEditors, Grids, StdCtrls, ExtCtrls;

type
  TFilterEditor = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    pnBottom: TPanel;
    pnMain: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    procedure SetFilter(Value: string);
    function GetFilter: string;
    procedure UpdateGridColumns;
  end;

  TFilterProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

uses
  SysUtils, DsnConst, LibHelp, BrandingAPI;

{$R *.dfm}

const
  NameCol = 0;
  FiltCol = 1;
  FirstRow = 1;

var
  FilterGrid: TStringGrid;

{ TFilterEditor }

procedure TFilterEditor.FormCreate(Sender: TObject);
begin
  if TIDEThemeMetrics.Font.Enabled then
    Font.Assign(TIDEThemeMetrics.Font.GetFont());

  if ThemeProperties <> nil then
  begin
    IDEThemeManager.RegisterFormClass(TFilterEditor);
    ThemeProperties.ApplyTheme(Self);
    pnMain.Color := ThemeProperties.StyleServices.GetSystemColor(clWindow);
    pnBottom.Color := ThemeProperties.StyleServices.GetSystemColor(clBtnFace);
  end;

  HelpContext := hcDFilterEditor;
  FilterGrid := TStringGrid.Create(Self);
  with FilterGrid do
  begin
    ColCount := 2;
    FixedCols := 0;
    RowCount := 25;
    ScrollBars := ssVertical;
    Options := [goFixedVertLine, goHorzLine, goVertLine, goEditing, goTabs,
      goAlwaysShowEditor];
    Align := alClient;
    Parent := pnMain;
    TabOrder := 0;
    Cells[NameCol, 0] := SFilterName;
    Cells[FiltCol, 0] := SFilter;
  end;
  UpdateGridColumns;
  ActiveControl := FilterGrid;
end;

procedure TFilterEditor.UpdateGridColumns;
begin
  inherited;
  FilterGrid.DefaultColWidth := FilterGrid.ClientWidth div FilterGrid.ColCount - FilterGrid.GridLineWidth;
  FilterGrid.DefaultRowHeight := TIDEThemeMetrics.AdjustSize(Canvas.TextHeight('W') + ScaleValue(2));
end;

procedure TFilterEditor.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  inherited;
  UpdateGridColumns;
end;

function TFilterEditor.GetFilter: string;
var
  I: Integer;

  function EmptyRow: Boolean;
  begin
    Result := True;
    with FilterGrid do
      if (Cells[NameCol, I] <> '') or (Cells[FiltCol, I] <> '') then
        Result := False;
  end;

begin
  Result := '';
  with FilterGrid do
  begin
    for I := FirstRow to RowCount - 1 do
    begin
      if not EmptyRow then
      begin
        Result := Result+Cells[NameCol, I];
        Result := Result+'|';
        Result := Result+Cells[FiltCol, I];
        Result := Result+'|';
      end;
    end;
  end;
  I := Length(Result);
  if I > 0 then
    while IsDelimiter('|', Result, I) do
    begin
      SetLength(Result, Length(Result) - 1);
      Dec(I);
    end;
end;

procedure TFilterEditor.SetFilter(Value: string);
var
  Index: Byte;
  r, c: Integer;
begin
  if Value <> '' then
  begin
    r := FirstRow;
    c := NameCol;
    Index := AnsiPos('|', Value);
    with FilterGrid do
    begin
      while Index > 0 do
      begin
        Cells[c, r] := Copy(Value, 1, Index - 1);
        if c = FiltCol then
        begin
          c := NameCol;
          if r = RowCount - 1 then
            RowCount := RowCount + 1;
          r := r + 1;
        end
        else c := FiltCol;
        Delete(Value, 1, Index);
        Index := AnsiPos('|', Value);
      end;
      Cells[c, r] := Copy(Value, 1, Length(Value));
    end;
  end;
end;

{ TFilterProperty }

procedure TFilterProperty.Edit;
var
  FilterEditor: TFilterEditor;
begin
  FilterEditor := TFilterEditor.Create(Application);
  try
    FilterEditor.SetFilter(GetValue);
    FilterEditor.ShowModal;
    if FilterEditor.ModalResult = mrOK then
      SetValue(FilterEditor.GetFilter);
  finally
    FilterEditor.Free;
  end;
end;

function TFilterProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect];
end;

procedure TFilterEditor.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.

