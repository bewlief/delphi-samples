{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

{*******************************************************}
{                Strings Editor Dialog                  }
{*******************************************************}

unit StringsEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, StrEdit, Vcl.Menus, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ActnPopup,
  Vcl.PlatformDefaultStyleActnCtrls;

type
  TStringsEditDlg = class(TStrEditDlg)
    GroupBox1: TGroupBox;
    LineCount: TLabel;
    Memo: TRichEdit;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure UpdateStatus(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    SingleLine: string;
    MultipleLines: string;
  protected
    function GetLines: TStrings; override;
    procedure SetLines(const Value: TStrings); override;
    function GetLinesControl: TWinControl; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses DesignConst, BrandingAPI;

function TStringsEditDlg.GetLinesControl: TWinControl;
begin
  Result := Memo;
end;

procedure TStringsEditDlg.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then CancelButton.Click;
end;

procedure TStringsEditDlg.UpdateStatus(Sender: TObject);
var
  Count: Integer;
  LineText: string;
begin
  if Sender = Memo then FModified := True;
  Count := Memo.Lines.Count;
  if Count = 1 then LineText := SingleLine
  else LineText := MultipleLines;
  LineCount.Caption := Format('%d %s', [Count, LineText]);
end;

procedure TStringsEditDlg.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  inherited;
  TIDEThemeMetrics.Font.AdjustDPISize(Font, TIDEThemeMetrics.Font.Size, CurrentPPI);
  TIDEThemeMetrics.Font.AdjustDPISize(Memo.Font, TIDEThemeMetrics.Font.Size, CurrentPPI);
end;

procedure TStringsEditDlg.FormCreate(Sender: TObject);
begin
  inherited;
  if TIDEThemeMetrics.Font.Enabled then
    Font.Assign(TIDEThemeMetrics.Font.GetFont);
  if ThemeProperties <> nil then
  begin
    IDEThemeManager.RegisterFormClass(TStringsEditDlg);
    ThemeProperties.ApplyTheme(Self);
  end;
  SingleLine := srLine;
  MultipleLines := srLines;
end;

function TStringsEditDlg.GetLines: TStrings;
begin
  Result := Memo.Lines;
end;

procedure TStringsEditDlg.SetLines(const Value: TStrings);
begin
  Memo.Lines.Assign(Value);
end;

end.
