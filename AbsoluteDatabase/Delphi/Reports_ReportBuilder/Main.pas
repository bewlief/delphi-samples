unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Grids, DBGrids, Db, ABSMain, DBCtrls,
  ppCtrls, ppBands, ppPrnabl, ppClass, ppDB, ppCache, ppProd, ppReport,
  ppComm, ppRelatv, ppDBPipe;

const  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TMainForm = class(TForm)
    DataSource1: TDataSource;
    dbDemos: TABSDatabase;
    ABSTable1: TABSTable;
    ppDBPipeline1: TppDBPipeline;
    ppReport1: TppReport;
    ppHeaderBand1: TppHeaderBand;
    ppDetailBand1: TppDetailBand;
    ppFooterBand1: TppFooterBand;
    ppTitleBand1: TppTitleBand;
    ppLabel1: TppLabel;
    DBGrid1: TDBGrid;
    bnReport: TBitBtn;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    ppLabel5: TppLabel;
    ppDBText4: TppDBText;
    ppDBText1: TppDBText;
    ppLabel2: TppLabel;
    ppDBText2: TppDBText;
    ppLabel3: TppLabel;
    ppDBText3: TppDBText;
    ppLabel4: TppLabel;
    procedure bnReportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation


{$R *.DFM}

procedure TMainForm.bnReportClick(Sender: TObject);
begin
 ppReport1.Print;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  ABSTable1.Open;
end;

end.
