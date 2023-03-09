unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ABSMain, Grids, DBGrids, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Table: TABSTable;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Label1: TLabel;
    lbIndexes: TListBox;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    cbCase: TCheckBox;
    btnCreateIndex: TButton;
    eIndexName: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbAsc: TCheckBox;
    mSQL: TMemo;
    Label6: TLabel;
    btnDropIndex: TButton;
    GroupBox2: TGroupBox;
    rbTable: TRadioButton;
    rbQuery: TRadioButton;
    Bevel1: TBevel;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    cbUnique: TCheckBox;
    Query: TABSQuery;
    procedure FormCreate(Sender: TObject);
    procedure rbTableClick(Sender: TObject);
    procedure btnCreateIndexClick(Sender: TObject);
    procedure btnDropIndexClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure UpdateIndexList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  with Table do
   begin
    InMemory := True;
    FieldDefs.Clear;
    FieldDefs.Add('id',ftAutoInc,0,false);
    FieldDefs.Add('name',ftString,20,false);
    IndexDefs.Clear;
    IndexDefs.Add('pk_Id', 'id', [ixPrimary]);
    CreateTable;
    Open;
    Append;
    FieldByName('name').AsString := 'Billy';
    Post;
    Append;
    FieldByName('name').AsString := 'Mary';
    Post;
   end;
  UpdateIndexList;
end;

procedure TForm1.UpdateIndexList;
var
  i: Integer;
  s: String;
begin
  lbIndexes.Clear;
  for i:=0 to Table.IndexDefs.Count-1 do
   begin
    s := Table.IndexDefs[i].Name;
    s := s + ' (' + Table.IndexDefs[i].Fields + ')';  
    lbIndexes.Items.Add(s);
   end;

end;

procedure TForm1.rbTableClick(Sender: TObject);
begin
  mSQL.Enabled := rbQuery.Checked;
end;

procedure TForm1.btnCreateIndexClick(Sender: TObject);
var
  s,desc,nocase: String;
  Options: TIndexOptions;
begin
  Table.Close;
  try
    if rbQuery.Checked then
     begin
      s := 'create ';
      if cbUnique.Checked then s := s + ' unique ';
      s := s + ' index ' + eIndexName.Text + ' on friends (name ';
      if not cbAsc.Checked then s := s + ' desc ';
      if not cbCase.Checked then
        s := s + ' case'
      else
        s := s + ' nocase';
      s := s + ')';
      mSQL.Text := s;
      Query.SQL.Text := s;
      Query.ExecSQL;
     end
    else
     begin
      Options := [];
      if cbUnique.Checked then Options := [ixUnique];
      desc := '';
      nocase := '';
      if not cbAsc.Checked then desc := 'name';
      if not cbCase.Checked then nocase := 'name';
      Table.RestructureIndexDefs.Add(eIndexName.Text, 'name', Options, false, desc, nocase);
      Table.RestructureTable;
     end;
  finally
    Table.Open;
  end;
  UpdateIndexList;
end;

procedure TForm1.btnDropIndexClick(Sender: TObject);
var
  s: String;
begin
  Table.Close;
  try
    if rbQuery.Checked then
     begin
      s := 'drop index friends.' + eIndexName.Text;
      mSQL.Text := s;
      Query.SQL.Text := s;
      Query.ExecSQL;
     end
    else
     begin
      Table.RestructureIndexDefs.DeleteIndexDef(eIndexName.Text);
      Table.RestructureTable;
     end;
  finally
    Table.Open;
  end;
  UpdateIndexList;
end;

end.
