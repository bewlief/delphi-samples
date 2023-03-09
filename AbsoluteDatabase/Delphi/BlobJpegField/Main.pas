unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, DB, ABSMain, Jpeg;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TfrmMain = class(TForm)
    GroupBox1: TGroupBox;
    Label3: TLabel;
    dbDemos: TABSDatabase;
    tPhotoAlbum: TABSTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    btSaveImage: TButton;
    odBlob: TOpenDialog;
    sdBlob: TSaveDialog;
    btLoadImage: TButton;
    GroupBox2: TGroupBox;
    Photo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btLoadImageClick(Sender: TObject);
    procedure btSaveImageClick(Sender: TObject);
    procedure tPhotoAlbumAfterScroll(DataSet: TDataSet);
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
  tPhotoAlbum.Open;
end;

procedure TfrmMain.btLoadImageClick(Sender: TObject);
var
  FileStream: TFileStream;
  BlobStream: TStream;
begin
 if (odBlob.Execute) then
   begin
     tPhotoAlbum.Edit;
     try
       BlobStream := tPhotoAlbum.CreateBlobStream(tPhotoAlbum.FieldByName('Photo'),bmWrite);
       FileStream := TFileStream.Create(odBlob.FileName,fmOpenRead or fmShareDenyNone);
       BlobStream.CopyFrom(FileStream,FileStream.Size);
       FileStream.Free;
       BlobStream.Free;
       tPhotoAlbum.Post;
       tPhotoAlbumAfterScroll(tPhotoAlbum);
     except
       tPhotoAlbum.Cancel;
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
      BlobStream := tPhotoAlbum.CreateBlobStream(tPhotoAlbum.FieldByName('Photo'),bmRead);
      FileStream.CopyFrom(BlobStream,BlobStream.Size-BlobStream.Position);
      BlobStream.Free;
      FileStream.Free;
    end;
end;

procedure TfrmMain.tPhotoAlbumAfterScroll(DataSet: TDataSet);
var
  JpegImage: TJPEGImage;
  BlobStream: TStream;
begin
  if (not tPhotoAlbum.FieldByName('Photo').IsNull) then
    begin
      BlobStream := tPhotoAlbum.CreateBlobStream(tPhotoAlbum.FieldByName('Photo'),bmRead);
      JpegImage := TJPEGImage.Create;
      try
        JpegImage.LoadFromStream(BlobStream);
        Photo.Picture.Assign(JpegImage);
        Photo.Visible := True;
      finally
        JpegImage.Free;
        BlobStream.Free;
      end;
    end
  else
    Photo.Visible := False;
end;

end.
