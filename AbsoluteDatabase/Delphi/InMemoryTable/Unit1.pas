unit Unit1;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, ABSMain, ABSTypes, Grids, DBGrids, ExtCtrls, DBCtrls;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    mSQL: TMemo;
    btnCreateAndOpen: TButton;
    dbDemos: TABSDatabase;
    Query: TABSQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    btnSave: TButton;
    btnLoad: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    tFriends: TABSTable;
    tFriendsInMemory: TABSTable;
    procedure FormCreate(Sender: TObject);
    procedure btnCreateAndOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure EnableDisableControls;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.EnableDisableControls;
begin
  btnSave.Enabled := tFriendsInMemory.Exists;
  btnLoad.Enabled := tFriends.Exists;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  dbDemos.Open;
  EnableDisableControls;
end;

procedure TForm1.btnCreateAndOpenClick(Sender: TObject);
begin
  Query.Close;
  Query.SQL.Text := mSQL.Text;
  Query.Open;
  EnableDisableControls;
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  if tFriends.Exists then
    tFriends.DeleteTable;
  tFriendsInMemory.CopyTable(tFriends.TableName, dbDemos.DatabaseFileName);
  EnableDisableControls;
end;

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  Query.Close;
  if tFriendsInMemory.Exists then
    tFriendsInMemory.DeleteTable;
  tFriends.CopyTable(tFriendsInMemory.TableName, 'MEMORY');
  mSQL.Text := 'select * from friends';
  btnCreateAndOpenClick(Sender);
  btnCreateAndOpen.Enabled := False;
end;

end.
