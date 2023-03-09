unit Unit1;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, ABSMain, DB, StdCtrls, DBCtrls;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    dbDemos: TABSDatabase;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    ABSTable1: TABSTable;
    ABSTable2: TABSTable;
    DBImage1: TDBImage;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ABSTable2AfterCancel(DataSet: TDataSet);
    procedure ABSTable2BeforeInsert(DataSet: TDataSet);
  private
    { Private declarations }
    Bookmark: TBookmark;
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
  ABSTable1.Open;
  ABSTable2.Open;
end;

procedure TForm1.ABSTable2AfterCancel(DataSet: TDataSet);
begin
 Dataset.GotoBookmark(Bookmark);
 DataSet.FreeBookmark(Bookmark);
end;

procedure TForm1.ABSTable2BeforeInsert(DataSet: TDataSet);
begin
 Bookmark := Dataset.GetBookmark;
end;

end.
