unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, ABSMain, ExtCtrls, DBCtrls;

const  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TfrmMain = class(TForm)
    dbDemos: TABSDatabase;
    ABSTable1: TABSTable;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses QReport;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  ABSTable1.Open;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  frmReport.QuickRep1.Preview;
end;

end.
