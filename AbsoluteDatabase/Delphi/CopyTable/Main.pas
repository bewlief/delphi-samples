unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ABSMain, StdCtrls, ComCtrls, ExtCtrls;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TfrmMain = class(TForm)
    Label1: TLabel;
    lvTables: TListView;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    dbDemos: TABSDatabase;
    ABSTable1: TABSTable;
    Bevel1: TBevel;
    Label2: TLabel;
    eTableName: TEdit;
    rbCurrentDb: TRadioButton;
    rbSelectedDb: TRadioButton;
    eDbFileName: TEdit;
    odOpenDatabase: TOpenDialog;
    btnDbFileSelect: TButton;
    btCopy: TButton;
    ePassword: TEdit;
    cbEncrypted: TCheckBox;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btCopyClick(Sender: TObject);
    procedure btnDbFileSelectClick(Sender: TObject);
    procedure lvTablesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure rbCurrentDbClick(Sender: TObject);
    procedure rbSelectedDbClick(Sender: TObject);
  private
    { Private declarations }
    procedure FillTablesList;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  dbDemos.Open;
  FillTablesList;
end;

procedure TfrmMain.btnDbFileSelectClick(Sender: TObject);
begin
  if odOpenDatabase.Execute then
    eDbFileName.Text := odOpenDatabase.FileName;
end;

procedure TfrmMain.btCopyClick(Sender: TObject);
begin
  if (lvTables.Selected <> nil) then
    begin
      ABSTable1.TableName := lvTables.Selected.Caption;
      if (rbCurrentDb.Checked) then
        begin
          ABSTable1.CopyTable(eTableName.Text);
          FillTablesList;
        end
      else
        if (not cbEncrypted.Checked) then
          ABSTable1.CopyTable(eTableName.Text, eDbFileName.Text)
        else
          ABSTable1.CopyTable(eTableName.Text, eDbFileName.Text, ePassword.Text);
      MessageDlg('Table copied.', mtInformation, [mbOK], 0);
    end
  else
    MessageDlg('Please select table to copy from the list', mtInformation, [mbOK], 0);
end;

procedure TfrmMain.FillTablesList;
var
  tables: TStringList;
  i: integer;
begin
  tables := TStringList.Create;
  try
    lvTables.Items.Clear;
    dbDemos.GetTablesList(tables);
    for i:=0 to tables.Count-1 do
      lvTables.Items.Add.Caption := tables[i];
  finally
    tables.free;
  end;
end;

procedure TfrmMain.lvTablesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (lvTables.Selected <> nil) then
    eTableName.Text := lvTables.Selected.Caption+'_copy';
end;

procedure TfrmMain.rbCurrentDbClick(Sender: TObject);
begin
  eDbFileName.Enabled := False;
  cbEncrypted.Enabled := False;
  ePassword.Enabled := False;
end;

procedure TfrmMain.rbSelectedDbClick(Sender: TObject);
begin
  eDbFileName.Enabled := True;
  cbEncrypted.Enabled := True;
  ePassword.Enabled := True;
end;

end.
