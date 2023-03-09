unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ABSMain, ABSTypes, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids;

type
  TfrmMain = class(TForm)
    ABSTable1: TABSTable;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    eMin: TEdit;
    rbByTable: TRadioButton;
    rbBySQL: TRadioButton;
    btCreateTable: TButton;
    DataSource1: TDataSource;
    ABSQuery1: TABSQuery;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Label7: TLabel;
    Label4: TLabel;
    eMax: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure btCreateTableClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  btCreateTableClick(nil);
end;

procedure TfrmMain.btCreateTableClick(Sender: TObject);
begin
  ABSTable1.Close;
  if (rbByTable.Checked) then
    begin
      with ABSTable1 do
        begin
          with AdvFieldDefs do
            begin
              Clear;
              Add('ID', aftAutoinc);
              Add('Name', aftString,15);
              with AddFieldDef do
                begin
                  Name := 'Age';
                  DataType := aftInteger;
                  MinValue.AsInteger := StrToInt(eMin.Text);
                  MaxValue.AsInteger := StrToInt(eMax.Text);
                end;
            end;
          CreateTable;
        end;
    end
  else
    begin
      ABSQuery1.SQL.Text := Format(
         'DROP TABLE test; CREATE TABLE test (ID Autoinc, Name Char(15), Age Integer MINVALUE %d MAXVALUE %d)',
         [StrToInt(eMin.Text), StrToInt(eMax.Text)]);
      ABSQuery1.ExecSQL;
    end;
  ABSTable1.Open;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  ABSTable1.Append;
  ABSTable1.FieldByName('Age').AsInteger := StrToInt(eMax.Text)+1;
  ABSTable1.Post;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  ABSTable1.Append;
  ABSTable1.FieldByName('Age').AsInteger := StrToInt(eMin.Text);
  ABSTable1.Post;
end;

end.
