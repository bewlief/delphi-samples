unit uMain;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBCtrls, Grids, DBGrids, ABSMain, ABSDecUtil;

type
  TForm1 = class(TForm)
    tblImages: TABSTable;
    DBGrid1: TDBGrid;
    DBImage1: TDBImage;
    dsImages: TDataSource;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    memQuery: TMemo;
    tblImagesID: TAutoIncField;
    tblImagesFilePath: TStringField;
    tblImagesImage: TBlobField;
    Label2: TLabel;
    Label3: TLabel;
    btnCreateQuery: TButton;
    btnExecuteQuery: TButton;
    Label4: TLabel;
    qryAddImage: TABSQuery;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnCreateQueryClick(Sender: TObject);
    procedure btnExecuteQueryClick(Sender: TObject);
    procedure memQueryChange(Sender: TObject);
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
  ImagePath: string;
begin
  dlgOpen.InitialDir := ExtractFilePath(Application.ExeName) + 'Images';
  // create in-memory table using FieldDefs set at design-time
  tblImages.CreateTable;
  // append sample records
  tblImages.Open;
  tblImages.Append;
  ImagePath := ExtractFilePath(Application.ExeName) + 'Images\vampire.bmp';
  tblImagesFilePath.Value := ImagePath;
  tblImagesImage.LoadFromFile(ImagePath);
  tblImages.Post;
  tblImages.Append;
  ImagePath := ExtractFilePath(Application.ExeName) + 'Images\lord.bmp';
  tblImagesFilePath.Value := ImagePath;
  tblImagesImage.LoadFromFile(ImagePath);
  tblImages.Post;
  tblImages.First;

end;

procedure TForm1.btnCreateQueryClick(Sender: TObject);
var
  MimeCoder: TStringFormat_MIME64;
  S: TMemoryStream;
begin
  if dlgOpen.Execute then
  begin
    MimeCoder := TStringFormat_MIME64.Create;
    S := TMemoryStream.Create;
    S.LoadFromFile(dlgOpen.FileName);
    try
      memQuery.Lines.Text := 'INSERT INTO Images (FilePath, Image) ' + #10 + 'VALUES (''' +
        dlgOpen.FileName + ''', ' + #10 +
        'MimeToBin(''' + MimeCoder.StrTo(S.Memory, S.Size) + '''))';
      btnExecuteQuery.Enabled := True;
    finally
      MimeCoder.Free;
      S.Free;
    end;
  end;
end;

procedure TForm1.btnExecuteQueryClick(Sender: TObject);
begin
  qryAddImage.SQL := memQuery.Lines;
  qryAddImage.ExecSQL;
  tblImages.Refresh;
end;

procedure TForm1.memQueryChange(Sender: TObject);
begin
  btnExecuteQuery.Enabled := memQuery.Text <> '';
end;

end.
