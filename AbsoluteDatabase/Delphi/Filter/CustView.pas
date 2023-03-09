unit CustView;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBCtrls, Grids, DBGrids, ComCtrls, StdCtrls, Buttons, DB, ABSMain;

const
  DataBaseFileName: String = '..\..\Data\Demos.abs';

type
  TfmCustView = class(TForm)
    rgDataSet: TRadioGroup;
    SpeedButton1: TSpeedButton;
    DBGrid1: TDBGrid;
    EmployeeSource: TDataSource;
    Employee: TABSTable;
    dbDemos: TABSDatabase;
    SQLEmployee: TABSQuery;
    Label1: TLabel;
    EventsSource1: TDataSource;
    DBGrid2: TDBGrid;
    EventsTable1: TABSTable;
    RadioGroup1: TRadioGroup;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    procedure rgDataSetClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EmployeeFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure SQLEmployeeFilterRecord(DataSet: TDataSet;
      var Accept: Boolean);
    procedure RadioGroup1Click(Sender: TObject);
  end;

var
  fmCustView: TfmCustView;

implementation

uses Filter1;

{$R *.dfm}

{ Change the Dataset for the Employee from Query to Table,
  or Table to Query. If a filter is current, set that filter
  to the just-changed Dataset. }
procedure TfmCustView.rgDataSetClick(Sender: TObject);
var
  st: string;
begin
  with fmCustView, EmployeeSource do
  begin
    { Is the other Dataset Filtered? Get its filter. }
    if Dataset.Filtered then
      st := Dataset.Filter;
    case rgDataset.ItemIndex of
      0: if Dataset <> SQLEmployee then
           Dataset := SQLEmployee;
      1: if EmployeeSource.Dataset <> Employee then
           Dataset := Employee;
    end;

    { Set the Filter of the current Dataset. }
    if st <> '' then
    begin
      Dataset.Filter := st;
      Dataset.Filtered := True;
    end;
  end;
end;

procedure TfmCustView.SpeedButton1Click(Sender: TObject);
begin
 if RadioGroup1.ItemIndex = 0 then
  begin
   Label1.Caption := 'Unfiltered';
   fmFilterFrm.Show
  end
 else
  begin
   EmployeeSource.Dataset.Filter := '';
   EmployeeSource.Dataset.Filtered := true;
   Label1.Caption := 'Filtered';
  end; 
end;

procedure TfmCustView.FormCreate(Sender: TObject);
begin
 dbDemos.DatabaseFileName := ExtractFilePath(Application.ExeName) + DataBaseFileName;
 dbDemos.Open;
 Employee.Open;
 SQLEmployee.Open;
end;

procedure TfmCustView.EmployeeFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
(* if RadioGroup1.ItemIndex = 1 then
  if DataSet.FieldByName('HireDate').AsDateTime >=
    EventsTable1.FieldByName('Event_Date').AsDateTime
   then Accept := true
  else Accept := false;*)
  Accept := True;
end;

procedure TfmCustView.SQLEmployeeFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
 if RadioGroup1.ItemIndex = 1 then
  if DataSet.FieldByName('HireDate').AsDateTime >=
    EventsTable1.FieldByName('Event_Date').AsDateTime
   then Accept := true
  else Accept := false;
end;

procedure TfmCustView.RadioGroup1Click(Sender: TObject);
begin
 if RadioGroup1.ItemIndex=0 then
  begin
    EventsTable1.Close;
    DBGrid2.Enabled := false;
  end
 else
  begin
    EventsTable1.Open;
    DBGrid2.Enabled := true;
  end
end;

end.
