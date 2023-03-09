unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB,
  ABSMain,
  ABSSecurity;

const DataBaseFileName = '..\..\Data\Demos.abs';

type
  TfrmMain = class(TForm)
    DBGrid1: TDBGrid;
    btnChange: TButton;
    btnCreate: TButton;
    dbDemos: TABSDatabase;
    db: TABSDatabase;
    tEmployeeDemos: TABSTable;
    btnOpen: TButton;
    tEmployee: TABSTable;
    DataSource1: TDataSource;
    sdDatabase: TSaveDialog;
    odDatabase: TOpenDialog;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure dbPassword(Sender: TObject; var Continue: Boolean);
    procedure btnChangeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses uDatabase;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var CurDir: String;
begin
  CurDir := ExtractFilePath(Application.ExeName);
  odDatabase.InitialDir := CurDir;
  sdDatabase.InitialDir := CurDir;
  dbDemos.DatabaseFileName := CurDir + DataBaseFileName;
  dbDemos.Open;
  tEmployeeDemos.Open;
end;

procedure TfrmMain.btnCreateClick(Sender: TObject);
begin
  if frmDatabase.ShowModal = mrOk then
   begin
    db.Password := frmDatabase.ePassword1.Text;
    db.CryptoAlgorithm := TABSCryptoAlgorithm(frmDatabase.cbCryptoAgorithm.ItemIndex);
    if sdDatabase.Execute then
     begin
      db.Close;
      db.DatabaseFileName := sdDatabase.FileName;
      db.CreateDatabase;
      tEmployeeDemos.CopyTable(tEmployeeDemos.TableName, db.DatabaseFileName, db.Password);
      tEmployee.Open;
      btnChange.Enabled := True;
     end;
   end;
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  if odDatabase.Execute then
   begin
    db.Close;
    db.DatabaseFileName := odDatabase.FileName;
    tEmployee.Open;
    btnChange.Enabled := True;
   end;
end;

procedure TfrmMain.dbPassword(Sender: TObject; var Continue: Boolean);
var pwd: String;
begin
  Continue := InputQuery('Database Password', 'Please enter pasword:', pwd);
  db.Password := pwd;
end;

procedure TfrmMain.btnChangeClick(Sender: TObject);
begin
  if frmDatabase.ShowModal = mrOk then
   begin
    db.Close;
    db.ChangePassword(frmDatabase.ePassword1.Text, TABSCryptoAlgorithm(frmDatabase.cbCryptoAgorithm.ItemIndex));
    tEmployee.Open;
   end;
end;

end.
