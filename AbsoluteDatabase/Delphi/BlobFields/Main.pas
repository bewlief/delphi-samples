unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, DB, ABSMain;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TfrmMain = class(TForm)
    GroupBox1: TGroupBox;
    Label3: TLabel;
    dbDemos: TABSDatabase;
    tVenues: TABSTable;
    tVenuesVenueNo: TAutoIncField;
    tVenuesVenue: TStringField;
    tVenuesCapacity: TIntegerField;
    tVenuesVenue_Map: TGraphicField;
    tVenuesRemarks: TMemoField;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBMemo1: TDBMemo;
    DBImage1: TDBImage;
    btLoadMemo: TButton;
    btSaveMemo: TButton;
    btLoadImage: TButton;
    btSaveImage: TButton;
    odBlob: TOpenDialog;
    sdBlob: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btLoadMemoClick(Sender: TObject);
    procedure btSaveMemoClick(Sender: TObject);
    procedure btLoadImageClick(Sender: TObject);
    procedure btSaveImageClick(Sender: TObject);
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
end;

procedure TfrmMain.btLoadMemoClick(Sender: TObject);
var
  FileStream: TFileStream;
  BlobStream: TStream;
begin
 if (odBlob.Execute) then
   begin
     tVenues.Edit;
     try
       BlobStream := tVenues.CreateBlobStream(tVenues.FieldByName('Remarks'),bmWrite);
       FileStream := TFileStream.Create(odBlob.FileName,fmOpenRead or fmShareDenyNone);
       BlobStream.CopyFrom(FileStream,FileStream.Size);
       FileStream.Free;
       BlobStream.Free;
       tVenues.Post;
     except
       tVenues.Cancel;
     end;
   end;
end;

procedure TfrmMain.btSaveMemoClick(Sender: TObject);
var
  FileStream: TFileStream;
  BlobStream: TStream;
begin
  if (sdBlob.Execute) then
    begin
      FileStream := TFileStream.Create(sdBlob.FileName,fmCreate);
      BlobStream := tVenues.CreateBlobStream(tVenues.FieldByName('Remarks'),bmRead);
      FileStream.CopyFrom(BlobStream,BlobStream.Size);
      BlobStream.Free;
      FileStream.Free;
    end;
end;

procedure TfrmMain.btLoadImageClick(Sender: TObject);
var
  FileStream: TFileStream;
  BlobStream: TStream;
begin
 if (odBlob.Execute) then
   begin
     tVenues.Edit;
     try
       BlobStream := tVenues.CreateBlobStream(tVenues.FieldByName('Venue_Map'),bmWrite);
       FileStream := TFileStream.Create(odBlob.FileName,fmOpenRead or fmShareDenyNone);
       BlobStream.CopyFrom(FileStream,FileStream.Size);
       FileStream.Free;
       BlobStream.Free;
       tVenues.Post;
     except
       tVenues.Cancel;
       raise;
     end;
   end;
end;

procedure TfrmMain.btSaveImageClick(Sender: TObject);
var
  FileStream: TFileStream;
  BlobStream: TStream;
begin
  if (sdBlob.Execute) then
    begin
      FileStream := TFileStream.Create(sdBlob.FileName,fmCreate);
      BlobStream := tVenues.CreateBlobStream(tVenues.FieldByName('Venue_Map'),bmRead);
      FileStream.CopyFrom(BlobStream,BlobStream.Size);
      BlobStream.Free;
      FileStream.Free;
    end;
end;

end.
