unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ABSMain, Grids, DBGrids, StdCtrls;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Table: TABSTable;
    GroupBox1: TGroupBox;
    rbTable: TRadioButton;
    rbQuery: TRadioButton;
    Button1: TButton;
    Label1: TLabel;
    Query: TABSQuery;
    mSQL: TMemo;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure rbQueryClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Table.Close;
  if rbQuery.Checked then
   begin
    Query.SQL.Text := mSQL.Text;
    Query.ExecSQL;
    Table.Open;
   end
  else
   begin
    if Table.Exists then Table.DeleteTable;
    Table.FieldDefs.Clear;
    Table.FieldDefs.Add('id',ftAutoInc,0,False);
    Table.FieldDefs.Add('name',ftString,20,False);
    Table.CreateTable;
    Table.Open;

    Table.Append;
    Table.FieldByName('name').AsString := 'John';
    Table.Post;
   end;
end;

procedure TForm1.rbQueryClick(Sender: TObject);
begin
  mSQL.Enabled := rbQuery.Checked;
end;

end.
