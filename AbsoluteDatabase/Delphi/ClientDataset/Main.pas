unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ABSMain, Grids, DBGrids, DBClient, Provider, StdCtrls,
  ExtCtrls, DBCtrls;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    dbDemos: TABSDatabase;
    ABSTable: TABSTable;
    ABSQuery: TABSQuery;
    DataSource: TDataSource;
    DataSetProvider: TDataSetProvider;
    ClientDataSet: TClientDataSet;
    rgDataSet: TRadioGroup;
    btnReOpen: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    eUpdateSql: TEdit;
    Button2: TButton;
    Button1: TButton;
    DataSetProviderUpdate: TDataSetProvider;
    ClientDataSetUpdate: TClientDataSet;
    ABSQueryUpdate: TABSQuery;
    DBNavigator1: TDBNavigator;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    procedure btnReOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ApplyUpdates(DataSet: TDataSet);
  private
    { Private declarations }
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
  ClientDataSet.Open;
end;

procedure TForm1.btnReOpenClick(Sender: TObject);
begin
  ClientDataSet.Close;
  if rgDataSet.ItemIndex = 1 then
    DataSetProvider.DataSet := ABSTable
  else
    DataSetProvider.DataSet := ABSQuery;
  ClientDataSet.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ClientDataSet.Close;
end;

procedure TForm1.ApplyUpdates(DataSet: TDataSet);
begin
 ClientDataSet.ApplyUpdates(0);
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
{$IFDEF D5H}
  ClientDataSetUpdate.CommandText := eUpdateSql.Text;
  ClientDataSetUpdate.Execute;
  ClientDataSet.Open;
  ClientDataSet.Refresh;
{$ELSE}
  MessageDlg('Supported in Delphi 5 and higher only', mtInformation, [mbOK], 0);
{$ENDIF}
end;

end.
