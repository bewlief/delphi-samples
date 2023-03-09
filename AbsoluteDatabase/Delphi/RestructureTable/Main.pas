unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ABSMain, DB, ExtCtrls, DBCtrls, Grids, DBGrids, StdCtrls;

const DataBaseFileName = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    dbDemos: TABSDatabase;
    tEmployeeDemos: TABSTable;
    tEmployeeMemory: TABSTable;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    btnInit: TButton;
    mSQL: TMemo;
    Query: TABSQuery;
    GroupBox2: TGroupBox;
    rbTable: TRadioButton;
    rbQuery: TRadioButton;
    btnRestructure: TButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnInitClick(Sender: TObject);
    procedure btnRestructureClick(Sender: TObject);
    procedure rbTableClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  dbDemos.Open;
  btnInitClick(nil);
end;

procedure TForm1.btnInitClick(Sender: TObject);
begin
  tEmployeeMemory.Close;
  if tEmployeeMemory.Exists then tEmployeeMemory.DeleteTable;
  tEmployeeDemos.CopyTable(tEmployeeDemos.TableName, tEmployeeMemory.DatabaseName);
  // Drop All Indexes
  tEmployeeMemory.Open;
  tEmployeeMemory.Close;
  tEmployeeMemory.RestructureIndexDefs.Clear;
  tEmployeeMemory.RestructureTable;
  tEmployeeMemory.Open;
end;

procedure TForm1.btnRestructureClick(Sender: TObject);
var
 i: Integer;
begin
 tEmployeeMemory.Close;
 try
   if rbQuery.Checked then
    begin
     Query.SQL.Text := mSQL.Text;
     Query.ExecSQL;
    end
   else
    begin
     tEmployeeMemory.RestructureFieldDefs.DeleteFieldDef('LastName');
     tEmployeeMemory.RestructureFieldDefs.Find('FirstName').Name := 'Name';
     tEmployeeMemory.RestructureTable;
    end;
 finally
   tEmployeeMemory.Open;
 end;
end;

procedure TForm1.rbTableClick(Sender: TObject);
begin
  mSQL.Enabled := rbQuery.Checked;
end;

end.
