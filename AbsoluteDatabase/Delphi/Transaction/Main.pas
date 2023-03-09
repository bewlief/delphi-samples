unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, DBGrids, Db, ABSMain, DBCtrls;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TMainForm = class(TForm)
    DataSource1: TDataSource;
    ABSTable1: TABSTable;
    ABSQuery1: TABSQuery;
    dbDemos: TABSDatabase;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    btTransactionWithTable: TButton;
    btTransactionWithQuery: TButton;
    cbFlush: TCheckBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    procedure btTransactionWithTableClick(Sender: TObject);
    procedure btTransactionWithQueryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation


{$R *.DFM}

procedure TMainForm.btTransactionWithTableClick(Sender: TObject);
begin
  dbDemos.StartTransaction;
  try
    AbsTable1.Insert;
    AbsTable1.FieldByName('name').AsString := 'Kelly';
    AbsTable1.Post;
    dbDemos.Commit(cbFlush.Checked);
  except
    dbDemos.Rollback;
  end;
end;

procedure TMainForm.btTransactionWithQueryClick(Sender: TObject);
begin
  with ABSQuery1 do
    begin
      SQL.Text := 'START TRANSACTION; INSERT INTO friends (name) values ("Junior");';
      if cbFlush.Checked then
        SQL.Text := SQL.Text + 'COMMIT;'
      else
        SQL.Text := SQL.Text + 'COMMIT NOFLUSH;';
      try
        ExecSQL;
      except
        SQL.Text := 'ROLLBACK';
        ExecSQL;
      end;
    end;
  ABSTable1.Refresh;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var q: TABSQuery;
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  dbDemos.Open;

  q := TABSQuery.Create(nil);
  try
    q.DatabaseName := dbDemos.DatabaseName;
    q.SQL.Clear;
    q.SQL.Append('drop table friends;');
    q.SQL.Append('create table friends (id AutoInc, name varchar(20));');
    q.SQL.Append('insert into friends (name) values ("Kim");');
    q.SQL.Append('insert into friends (name) values ("Scott");');
    q.ExecSQL;
  finally
    q.Free;
  end;

  ABSTable1.Open;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ABSTable1.Close;
  ABSTable1.DeleteTable;
end;

end.
