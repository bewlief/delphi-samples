unit Unit1;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ABSMain, DBCtrls, StdCtrls, Mask, Grids, DBGrids, DB, ExtCtrls,
  ExtDlgs;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    dbDemos: TABSDatabase;
    tVenues: TABSTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBText1: TDBText;
    DBEdit1: TDBEdit;
    DBMemo1: TDBMemo;
    Map: TDBImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    tVenuesVenueNo: TAutoIncField;
    tVenuesVenue: TStringField;
    tVenuesCapacity: TIntegerField;
    tVenuesVenue_Map: TGraphicField;
    tVenuesRemarks: TMemoField;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure MapDblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  tVenues.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 if (OpenPictureDialog1.Execute) then
  begin
    tVenues.Edit;
    try
      tVenuesVenue_Map.LoadFromFile(OpenPictureDialog1.FileName);
      tVenues.Post;
    except
     tVenues.Cancel;
     raise;
    end;
  end;
end;

procedure TForm1.MapDblClick(Sender: TObject);
begin
  Button1Click(Sender);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 if (SaveDialog1.Execute) then
   tVenuesVenue_Map.SaveToFile(SaveDialog1.FileName);
end;

end.
