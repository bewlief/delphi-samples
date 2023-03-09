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
    eDefaultName: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    eDefaultAge: TEdit;
    rbByTable: TRadioButton;
    rbBySQL: TRadioButton;
    btCreateTable: TButton;
    DataSource1: TDataSource;
    ABSQuery1: TABSQuery;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    btInsert: TButton;
    Label7: TLabel;
    procedure btCreateTableClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btInsertClick(Sender: TObject);
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
              with AddFieldDef do
                begin
                  Name := 'Name';
                  DataType := aftString;
                  Size := 15;
                  DefaultValue.AsString := eDefaultName.Text;
                end;
              with AddFieldDef do
                begin
                  Name := 'Age';
                  DataType := aftInteger;
                  DefaultValue.AsInteger := StrToInt(eDefaultAge.Text);
                end;
            end;
          CreateTable;
        end;
    end
  else
    begin
      ABSQuery1.SQL.Text := Format(
         'DROP TABLE test; CREATE TABLE test (ID Autoinc, Name Char(15) DEFAULT ''%s'', Age Integer DEFAULT %d)',
         [eDefaultName.Text, StrToInt(eDefaultAge.Text)]);
      ABSQuery1.ExecSQL;
    end;
  ABSTable1.Open;
end;

procedure TfrmMain.btInsertClick(Sender: TObject);
begin
  ABSTable1.Append;
  ABSTable1.Post;
end;

end.
