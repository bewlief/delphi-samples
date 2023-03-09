unit uMain;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, Grids, DBGrids, DB, ABSMain, ComCtrls,
  StdCtrls;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    dbDemos: TABSDatabase;
    lvTables: TListView;
    ABSTable1: TABSTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    lbSelectedTable: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lvTablesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
var
  tables: TStringList;
  i: integer;
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  dbDemos.Open;
  tables := TStringList.Create;
  try
    dbDemos.GetTablesList(tables);
    for i:=0 to tables.Count-1 do
      lvTables.Items.Add.Caption := tables[i];
  finally
    tables.free;
  end;
end;

procedure TForm1.lvTablesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
{$IFNDEF D5H}
  if (AnsiCompareText(Item.Caption, 'Dictionary') <> 0) then
    begin
{$ENDIF}
  if not (csDestroying in Form1.ComponentState) then
  begin
    lbSelectedTable.Caption := Item.Caption;
    ABSTable1.Close;
    ABSTable1.TableName := Item.Caption;
    ABSTable1.Open;
  end;
{$IFNDEF D5H}
    end;
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dbDemos.Close;
end;

end.
