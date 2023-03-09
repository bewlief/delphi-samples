unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Grids, DBGrids, ExtCtrls, DBCtrls, DB, ABSMain, ABSTypes;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TfrmMain = class(TForm)
    dbDemos: TABSDatabase;
    tVenues: TABSTable;
    tVenuesVenueNo: TAutoIncField;
    tVenuesVenue: TStringField;
    tVenuesCapacity: TIntegerField;
    tVenuesVenue_Map: TGraphicField;
    tVenuesRemarks: TMemoField;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    btCreate: TButton;
    GroupBox2: TGroupBox;
    cbGraphicAlgorithm: TComboBox;
    Label2: TLabel;
    Label1: TLabel;
    seGraphicMode: TSpinEdit;
    Label4: TLabel;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cbMemoalgorithm: TComboBox;
    seMemoMode: TSpinEdit;
    GroupBox4: TGroupBox;
    Label8: TLabel;
    lbNewDbFileName: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lbVenuesDBSize: TLabel;
    VenuesDB: TABSDatabase;
    tNewVenues: TABSTable;
    DBImage1: TDBImage;
    DBMemo1: TDBMemo;
    procedure FormCreate(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  dbDemos.Open;
  tVenues.Open;
  cbGraphicAlgorithm.ItemIndex := 2;
  cbMemoAlgorithm.ItemIndex := 1;
end;

procedure TfrmMain.btCreateClick(Sender: TObject);
var
  GraphicCompressionAlgorithm: TCompressionAlgorithm;
  MemoCompressionAlgorithm: TCompressionAlgorithm;
  f: TFileStream;
begin
  GraphicCompressionAlgorithm := TCompressionAlgorithm(cbGraphicAlgorithm.ItemIndex);
  MemoCompressionAlgorithm := TCompressionAlgorithm(cbMemoAlgorithm.ItemIndex);
  VenuesDB.Close;
  VenuesDB.PageSize := 512;
  VenuesDB.MaxConnections := 20;
  VenuesDB.CreateDatabase;
  with tNewVenues do
    begin
      with AdvFieldDefs do
        begin
          Clear;
          Add('VenueNo', aftAutoinc);
          Add('Venue', aftString, 30);
          Add('Capacity', aftInteger);
          Add('Venue_Map', aftGraphic, 0, False, GraphicCompressionAlgorithm,
              seGraphicMode.Value);
          Add('Remarks', aftMemo, 0, False, MemoCompressionAlgorithm,
              seMemoMode.Value);
        end;
      CreateTable;
      BatchMove(tVenues, bmtCopy);
    end;
  VenuesDB.Close;
  VenuesDB.CompactDatabase;
  f := TFileStream.Create(VenuesDB.DatabaseFileName, fmOpenRead or fmShareDenyNone);
  lbVenuesDBSize.Caption := IntToStr(Round(f.Size/1024))+' Kb';
  f.Free;
  tNewVenues.Open;
end;

end.
