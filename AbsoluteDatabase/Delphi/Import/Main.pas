unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, StdCtrls, DBTables, DB, ABSMain;

type
  TfrmMain = class(TForm)
    ABSDb: TABSDatabase;
    ABSTable: TABSTable;
    Table: TTable;
    Label4: TLabel;
    Label5: TLabel;
    lbImportAliasTables: TListBox;
    lbImportAliases: TListBox;
    Label8: TLabel;
    edImportDestDB: TEdit;
    btBrowseImportDestinationDB: TButton;
    lImportTable: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label9: TLabel;
    mImportDetails: TMemo;
    gTableImport: TGauge;
    gOverallImport: TGauge;
    btStopImport: TButton;
    btStart: TButton;
    odAbsDb: TOpenDialog;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lbImportAliasesClick(Sender: TObject);
    procedure lbImportAliasTablesClick(Sender: TObject);
    procedure btBrowseImportDestinationDBClick(Sender: TObject);
    procedure btStopImportClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure ABSTableBeforeImport(Sender: TObject);
    procedure ABSTableImportProgress(Sender: TObject; PercentDone: Integer;
      var Continue: Boolean);
  private
    { Private declarations }
    IsStopped: Boolean;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DBTables.Session.GetAliasNames(lbImportAliases.Items);
  lbImportAliasTablesClick(nil);
end;

procedure TfrmMain.lbImportAliasesClick(Sender: TObject);
begin
 DBTables.Session.GetTableNames(lbImportAliases.Items[lbImportAliases.ItemIndex],'',True,True,lbImportAliasTables.Items);
 lbImportAliasTablesClick(Sender);
end;

procedure TfrmMain.lbImportAliasTablesClick(Sender: TObject);
begin
  btStart.Enabled := (lbImportAliasTables.SelCount > 0);
end;

procedure TfrmMain.btBrowseImportDestinationDBClick(Sender: TObject);
begin
  if (odAbsDb.Execute) then
    edImportDestDB.Text := odAbsDb.FileName;
end;

procedure TfrmMain.btStopImportClick(Sender: TObject);
begin
  IsStopped := True;
end;

procedure TfrmMain.btStartClick(Sender: TObject);
var
  i, tableCount: Integer;
  tables: TListBox;
  tableName: String;
  PromptOverwrite: Boolean;
  mr: TModalResult;
  Log: String;
begin
  PromptOverwrite := True;
  IsStopped := False;
  AbsDB.Close;
  Table.Close;
  mImportDetails.Clear;
  AbsDB.DatabaseFileName := edImportDestDB.Text;
  mImportDetails.Lines.Add(Format('Import tables from "%s" to "%s"',[lbImportAliases.Items[lbImportAliases.ItemIndex],AbsDB.DatabaseFileName]));
  if (not AbsDB.Exists) then
    AbsDB.CreateDatabase;
  try
    AbsDB.Open;
  except
    MessageDlg(Format('Cannot open "%s" database file',[AbsDB.DatabaseFileName]),mtError,[mbOk],0);
    exit;
  end;
  tableCount := lbImportAliasTables.Items.Count;
  tables := lbImportAliasTables;
  Table.DatabaseName := lbImportAliases.Items[lbImportAliases.ItemIndex];
  gOverallImport.MaxValue := tables.SelCount;
  gOverallImport.Progress := 0;
  // import tables
  for i := 0 to tableCount - 1 do
    if (tables.Selected[i]) then
      begin
        if (IsStopped) then
          break;
        ABSTable.Close;
        Table.Close;
        tableName := tables.Items[i];
        Table.TableName := tableName;
        // SQL Server: dbo.table -> table
        if (Pos('dbo.', LowerCase(tableName)) = 1) then
          ABSTable.TableName := Copy(tableName, 5, Length(tableName)-4)
        else
          if (ExtractFileExt(tableName) <> '') then
            ABSTable.tableName := Copy(tableName, 1,
                           Length(tableName)-Length(ExtractFileExt(tableName)))
          else
            ABSTable.TableName := tableName;
        // overwrite existing table?
        if (ABSTable.Exists and PromptOverwrite) then
          begin
            mr := MessageDlg(Format('Table "%s" exists in "%s" database. Do you want to overwrite it?',[ABSTable.TableName, ABSDb.DatabaseFileName]),
        			               mtConfirmation,[mbYes,mbNo,mbAll],0);
            if (mr = mrNo) then
              begin
                mImportDetails.Lines.Add(Format('Tables "%s" already exists, its import cancelled by user',[ABSTable.TableName]));
                gOverallImport.Progress := gOverallImport.Progress + 1;
                continue;
              end
            else
              if (mr = mrAll) then
                PromptOverwrite := False;
          end;
        // import table
        lImportTable.Caption := Format('Importing table "%s"',[tableName]);
        mImportDetails.Lines.Add(lImportTable.Caption);
        Log := '';
        try
          Table.Open;
          ABSTable.ImportTable(Table, Log, Table.IndexDefs);
          ABSTable.Open;
          if (Log = '') then Log := 'No errors';
          mImportDetails.Lines.Add(Format('Table "%s" imported. %d records transferred, %d records skipped. ErrorLog: %s',
             [tableName, ABSTable.RecordCount, Table.RecordCount-ABSTable.RecordCount, Log]));
        except
          on E: Exception do
             mImportDetails.Lines.Add(Format('Table "%s" import failed. Error: %s. ErrorLog: %s',
                                   [tableName, E.Message, Log]));
        end;
        gOverallImport.Progress := gOverallImport.Progress + 1;
      end;
    if (IsStopped) then
      mImportDetails.Lines.Add('Import stopped by user')
    else
      mImportDetails.Lines.Add('Import finished');
end;

procedure TfrmMain.ABSTableBeforeImport(Sender: TObject);
begin
  gTableImport.Progress := 0;
end;

procedure TfrmMain.ABSTableImportProgress(Sender: TObject;
  PercentDone: Integer; var Continue: Boolean);
begin
  gTableImport.Progress := PercentDone;
  Continue := not IsStopped;
  Application.ProcessMessages;
end;

end.
