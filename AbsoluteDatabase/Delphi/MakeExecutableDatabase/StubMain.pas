unit StubMain;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, ABSMain, Grids, DBGrids;

type
  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    dbExecutable: TABSDatabase;
    tblEmployee: TABSTable;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    DataSource1: TDataSource;
    procedure FormCreate(Sender: TObject);
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
  dbExecutable.DatabaseFileName := Application.ExeName;
  try
    dbExecutable.Open;
  except
    ShowMessage('This file doesn''t contain a database. Please use MakeExecutableDB project to append a database to this stub application');
    Exit;
  end;
  tblEmployee.Open;
end;

end.
