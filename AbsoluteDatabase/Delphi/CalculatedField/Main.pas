unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ABSMain, ABSTypes, StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids;

type
  TfrmMain = class(TForm)
    ABSTable1: TABSTable;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    ABSTable1Cost: TCurrencyField;
    ABSTable1ID: TAutoIncField;
    ABSTable1Price: TCurrencyField;
    ABSTable1Quantity: TIntegerField;
    procedure FormCreate(Sender: TObject);
    procedure ABSTable1CalcFields(DataSet: TDataSet);
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
  with ABSTable1 do
    begin
      { field defs are assigned at design-time}
      { calculated field is created by Fields Editor }
      CreateTable;
      Open;
      Insert;
      FieldByName('price').AsFloat := 95;
      FieldByName('quantity').AsInteger := 10;
      Post;
    end;
end;

procedure TfrmMain.ABSTable1CalcFields(DataSet: TDataSet);
begin
  with ABSTable1 do
   FieldByName('cost').AsFloat := FieldByName('price').AsFloat *
                                  FieldByName('quantity').AsInteger;
end;

end.
