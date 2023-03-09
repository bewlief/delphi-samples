unit Unit1;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, StdCtrls, ComCtrls, DB, ABSMain;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    reSQL: TRichEdit;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    cbRequestLive: TCheckBox;
    Button1: TButton;
    DataSource1: TDataSource;
    dbDemos: TABSDatabase;
    Query: TABSQuery;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbRequestLiveClick(Sender: TObject);
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
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Query.Close;
  Query.SQL.Text := reSQL.Text;
  Query.RequestLive := cbRequestLive.Checked;
  Query.Open;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Query.Close;
  Query.SQL.Text := reSQL.Text;
  Query.ExecSQL;
end;

procedure TForm1.cbRequestLiveClick(Sender: TObject);
begin
  Query.Close;
end;

end.
