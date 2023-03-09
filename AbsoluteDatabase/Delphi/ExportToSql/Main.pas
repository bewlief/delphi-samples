unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ABSMain, DB;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    lvTables: TListView;
    dlgSave: TSaveDialog;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    reExportToSql: TRichEdit;
    GroupBox1: TGroupBox;
    cbAddDropTable: TCheckBox;
    cbStructure: TCheckBox;
    cbData: TCheckBox;
    cbBlobSettings: TCheckBox;
    cbFieldNamesInInsert: TCheckBox;
    btnExpotrAsSqlToScreeen: TButton;
    btnExpotrAsSqlToFile: TButton;
    Label2: TLabel;
    Label3: TLabel;
    dbDemos: TABSDatabase;
    tblTable: TABSTable;
    procedure btnExpotrAsSqlToScreeenClick(Sender: TObject);
    procedure btnExpotrAsSqlToFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbStructureClick(Sender: TObject);
    procedure cbDataClick(Sender: TObject);
  private
    procedure ExportSelectedTablesToSQL(SQL: TStrings);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ TForm1 }

procedure TForm1.ExportSelectedTablesToSQL;
var
  I: Integer;
begin
  with tblTable.ExportToSqlOptions do
  begin
    Structure := cbStructure.Checked;
    AddDropTable := cbAddDropTable.Checked;
    BlobSettings := cbBlobSettings.Checked;
    Data := cbData.Checked;
    FieldNamesInInserts := cbFieldNamesInInsert.Checked;
  end;
  SQL.Clear;
  for I := 0 to lvTables.Items.Count - 1 do
    if lvTables.Items[I].Checked then
    begin
      tblTable.TableName := lvTables.Items[I].Caption;
      SQL.Add(tblTable.ExportToSQL);
    end;
end;

procedure TForm1.btnExpotrAsSqlToScreeenClick(Sender: TObject);
begin
  ExportSelectedTablesToSQL(reExportToSql.Lines);
end;

procedure TForm1.btnExpotrAsSqlToFileClick(Sender: TObject);
var
  SQL: TStrings;
begin
  if dlgSave.Execute then
  begin
    SQL := TStringList.Create;
    try
      ExportSelectedTablesToSQL(SQL);
      SQL.SaveToFile(dlgSave.FileName);
    finally
      SQL.Free;
    end;
    reExportToSql.Text := 'Script saved to file: ' + dlgSave.FileName;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Tables: TStrings;
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  Tables := TStringList.Create;
  try
    dbDemos.GetTablesList(Tables);
    lvTables.Items.Clear;
    while Tables.Count > 0 do
    begin
      with lvTables.Items.Add do
        Caption := Tables[0];
      Tables.Delete(0);
    end;
  finally
    Tables.Free;
  end;
end;

procedure TForm1.cbStructureClick(Sender: TObject);
begin
  cbAddDropTable.Enabled := cbStructure.Checked;
  cbBlobSettings.Enabled := cbStructure.Checked;
end;

procedure TForm1.cbDataClick(Sender: TObject);
begin
  cbFieldNamesInInsert.Enabled := cbData.Checked;
end;

end.
