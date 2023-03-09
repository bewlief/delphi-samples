unit uRange;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, ABSMain, ComCtrls, ExtCtrls,
  Buttons;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';
  QueryConst: String = 'select * from employee order by ';

type
  TForm1 = class(TForm)
    dbDemos: TABSDatabase;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Table: TABSTable;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    cmbStartRange: TComboBox;
    cmbEndRange: TComboBox;
    cbStartKeyExclusive: TCheckBox;
    cbEndKeyExclusive: TCheckBox;
    btRange: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure btRangeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  s:string;
  index:string;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
 dbDemos.Open;
 Table.Open;
 Table.IndexName := 'ByVenue';
 Table.First;
 while not Table.Eof do
  begin
   cmbEndRange.Items.Add(Table.FieldByName('Venue').AsString);
   cmbStartRange.Items.Add(Table.FieldByName('Venue').AsString);
   Table.Next;
  end;
   cmbStartRange.ItemIndex := 0;
   cmbEndRange.ItemIndex := 3;
end;

procedure TForm1.btRangeClick(Sender: TObject);
begin
 if btRange.Caption = 'Apply' then
  begin
   Table.IndexName := 'ByVenue';
   Table.SetRangeStart;
   Table.FieldByName('Venue').AsString := cmbStartRange.Text;
   Table.KeyExclusive := cbStartKeyExclusive.Checked;
   Table.SetRangeEnd;
   Table.FieldByName('Venue').AsString := cmbEndRange.Text;
   Table.KeyExclusive := cbEndKeyExclusive.Checked;
   Table.ApplyRange;
   btRange.Kind := bkCancel;
  end
 else
  begin
   Table.CancelRange;
   btRange.Kind := bkOK;
   btRange.Caption := 'Apply';
  end;
end;

end.
