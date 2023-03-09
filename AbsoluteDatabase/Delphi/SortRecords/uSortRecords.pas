unit uSortRecords;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, ABSMain, ComCtrls, ExtCtrls,
  Buttons;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';
  QueryConst: String = 'select * from employee order by ';

type
  TForm1 = class(TForm)
    Label1: TLabel;
    dbDemos: TABSDatabase;
    Query: TABSQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    mSQL: TMemo;
    Table: TABSTable;
    rgComponent: TRadioGroup;
    rgSorting: TRadioGroup;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure rgComponentClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure rgSortingClick(Sender: TObject);
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
 Table.TableName := 'employee';
 Index := 'ByFirstName';
 s:='';
end;

procedure TForm1.rgComponentClick(Sender: TObject);
begin
 if rgComponent.ItemIndex = 1 then
  begin
   mSQL.Enabled := false;
   mSQL.Text := 'Using ABSTable component for sorting'
  end
 else
  begin
   mSQL.Enabled := true;
   mSQL.Text := s;
   rgSortingClick(Sender);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 case rgComponent.ItemIndex of
  0: begin
      Query.Close;
      DataSource1.DataSet := Query;
      Query.SQL.Text := mSQL.Text;
      Query.Open;
     end;
  1: begin
      Table.Close;
      DataSource1.DataSet := Table;
      Table.IndexName := Index;
      Table.Open;
     end;
 end;
end;

procedure TForm1.rgSortingClick(Sender: TObject);
begin
 case rgComponent.ItemIndex of

  0: case rgSorting.ItemIndex of
      0: begin
          mSQL.Text := QueryConst + 'FirstName ASC';
          Index := 'ByFirstName';
         end;
      1: begin
          mSQL.Text := QueryConst + 'LastName DESC';
          Index := 'ByLastName';
         end;
      2: begin
          mSQL.Text := QueryConst + 'Salary DESC, LastName ASC';
          Index := 'BySalaryLastName';
         end;
     end;

  1: case rgSorting.ItemIndex of
      0: begin
          Index := 'ByFirstName';
          s := QueryConst + 'FirstName ASC';
         end;
      1: begin
          Index := 'ByLastName';
          s := QueryConst + 'LastName DESC';
         end;
      2: begin
          Index := 'BySalaryLastName';
          s := QueryConst + 'Salary DESC, LastName ASC';
         end;
     end;
 end;
end;

end.
