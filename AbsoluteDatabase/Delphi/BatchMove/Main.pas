unit Main;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ABSMain, StdCtrls, Grids, DBGrids, ExtCtrls, DBTables,
  ComCtrls;

type
  TForm1 = class(TForm)
    tSrc: TABSTable;
    tDst: TABSTable;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Button2: TButton;
    dsSrc: TDataSource;
    dsDst: TDataSource;
    rgType: TRadioGroup;
    pb: TProgressBar;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tDstBeforeBatchMove(Sender: TObject);
    procedure tDstAfterBatchMove(Sender: TObject);
    procedure tDstBatchMoveProgress(Sender: TObject; PercentDone: Integer;
      var Continue: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitTables;
  end;

var
  Form1: TForm1;

implementation
uses ABSTypes;

{$R *.dfm}

procedure TForm1.InitTables;
begin
  with tSrc do
    begin
      Close;
      FieldDefs.Clear;
      FieldDefs.Add('id', ftAutoInc, 0, False);
      FieldDefs.Add('name', ftString, 10, False);
      FieldDefs.Add('code', ftInteger, 0, False);
      FieldDefs.Add('address', ftString, 10, False);
      IndexDefs.Clear;
      IndexDefs.Add('pk_code','code',[ixPrimary]);
      CreateTable;
      Open;

      Insert;
      FieldByName('name').AsString := 'Johnny';
      FieldByName('code').AsInteger := 10;
      FieldByName('address').AsString := 'USA';
      Post;

      Insert;
      FieldByName('name').AsString := 'Piter';
      FieldByName('code').AsInteger := 12;
      FieldByName('address').AsString := 'Germany';
      Post;

    end;

  with tDst do
    begin
      Close;
      FieldDefs.Clear;
      FieldDefs.Add('code', ftInteger, 0, False);
      FieldDefs.Add('name', ftString, 10, False);
      FieldDefs.Add('lastname', ftString, 10, False);
      IndexDefs.Clear;
      IndexDefs.Add('pk_code','code',[ixPrimary]);
      CreateTable;
      Open;

      Insert;
      FieldByName('code').AsInteger := 10;
      FieldByName('name').AsString := 'John';
      FieldByName('lastname').AsString := 'Smith';
      Post;

      Insert;
      FieldByName('code').AsInteger := 11;
      FieldByName('name').AsString := 'Andrew';
      FieldByName('lastname').AsString := 'Harrison';
      Post;

    end;
  pb.Position := 0;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  InitTables;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  InitTables;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  tDst.BatchMove(tSrc, TABSBatchMoveType(rgType.ItemIndex), 'pk_code');
end;

procedure TForm1.tDstBeforeBatchMove(Sender: TObject);
begin
  pb.Position := 0;
end;

procedure TForm1.tDstAfterBatchMove(Sender: TObject);
begin
  pb.Position := 100;
end;

procedure TForm1.tDstBatchMoveProgress(Sender: TObject;
  PercentDone: Integer; var Continue: Boolean);
begin
  pb.Position := PercentDone;
end;

end.
