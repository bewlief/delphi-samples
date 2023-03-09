unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  ABSMain,
  ABSSecurity, DB;

const DataBaseFileName = '..\..\Data\Demos.abs';

type
  TfrmMain = class(TForm)
    db: TABSDatabase;
    btnCreateDefaultDatabase: TButton;
    Label1: TLabel;
    btnCreateCustomDatabase: TButton;
    lvTables: TListBox;
    Label2: TLabel;
    btnDropDatabase: TButton;
    sdDatabase: TSaveDialog;
    Label3: TLabel;
    dbDemos: TABSDatabase;
    Table: TABSTable;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    procedure btnCreateCustomDatabaseClick(Sender: TObject);
    procedure btnDropDatabaseClick(Sender: TObject);
    procedure btnCreateDefaultDatabaseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CopyTables;
  end;

var
  frmMain: TfrmMain;

implementation

uses uDatabase;

{$R *.dfm}

procedure TfrmMain.btnCreateCustomDatabaseClick(Sender: TObject);
begin
  if frmDatabase.ShowModal = mrOk then
    begin
     db.PageSize := StrToInt(frmDatabase.cbPageSize.Text);
     db.PageCountInExtent := StrToInt(frmDatabase.cbPageCountInExtent.Text);
     db.MaxConnections := StrToInt(frmDatabase.eMaxConnections.Text);
     if frmDatabase.cbEncrypted.Checked then
      begin
       db.Password := frmDatabase.ePassword1.Text;
       db.CryptoAlgorithm := TABSCryptoAlgorithm(frmDatabase.cbCryptoAgorithm.ItemIndex);
      end
     else
      begin
       db.Password := '';
      end;
      btnCreateDefaultDatabaseClick(Sender);
    end;
end;

procedure TfrmMain.btnCreateDefaultDatabaseClick(Sender: TObject);
begin
  if sdDatabase.Execute then
   begin
    db.Close;
    db.DatabaseFileName := sdDatabase.FileName;
    db.CreateDatabase;
    CopyTables;
    db.GetTablesList(lvTables.Items);
   end;
end;

procedure TfrmMain.CopyTables;
var
  TableList: TStringList;
  i: Integer;
begin
  TableList := TStringList.Create;
  try
    dbDemos.GetTablesList(TableList);
    for i:=0 to TableList.Count-1 do
     begin
      Table.TableName := TableList[i];
      try
        Table.CopyTable(Table.TableName, db.DatabaseFileName, db.Password);
      except
        on e: Exception do
          MessageDlg(e.Message, mtError, [mbOK], 0);
      end;
     end;
  finally
    TableList.Free;
  end;
end;

procedure TfrmMain.btnDropDatabaseClick(Sender: TObject);
var s: String;
begin
  s := 'Delete (Drop) Database ''' + db.DatabaseFileName + '''.'#13#10'Are you sure?';
  if (MessageDlg(s, mtConfirmation, [mbYes,mbNo,mbCancel],0) = mrYes) then
   begin
    db.Close;
    db.DeleteDatabase;
    lvTables.Clear;
   end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  dbDemos.Close;
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  dbDemos.Open;
  sdDatabase.InitialDir := ExtractFilePath(Application.ExeName);
end;

end.
