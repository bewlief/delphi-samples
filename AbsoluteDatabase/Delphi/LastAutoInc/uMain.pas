unit uMain;

interface
{$I CompVer.inc}

uses
{$IFDEF D6H}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, ABSMain, Grids, DBGrids, Spin, ComCtrls;

type
  TForm1 = class(TForm)
    dbgOrders: TDBGrid;
    DBGrid2: TDBGrid;
    tblOrders: TABSTable;
    tblOrderPositions: TABSTable;
    dsOrders: TDataSource;
    dsOrderPositions: TDataSource;
    Label1: TLabel;
    Label2: TLabel;
    tblOrdersID: TAutoIncField;
    tblOrdersCustomerName: TStringField;
    tblOrdersOrderDate: TDateField;
    tblOrderPositionsOrderID: TIntegerField;
    tblOrderPositionsProductName: TStringField;
    tblOrderPositionsQuantity: TIntegerField;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    edCustomerName: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    lblQuantity1: TLabel;
    Label10: TLabel;
    dtOrderDate: TDateTimePicker;
    edProduct1: TEdit;
    lblProduct2: TLabel;
    lblProduct3: TLabel;
    edProduct2: TEdit;
    lblQuantity2: TLabel;
    edProduct3: TEdit;
    lblQuantity3: TLabel;
    seQuantity1: TSpinEdit;
    seQuantity2: TSpinEdit;
    seQuantity3: TSpinEdit;
    btnAddUsingQuery: TButton;
    btnAddUsingTable: TButton;
    qryAddOrderPosition: TABSQuery;
    qryAddOrder: TABSQuery;
    procedure FormCreate(Sender: TObject);
    procedure btnAddUsingTableClick(Sender: TObject);
    procedure btnAddUsingQueryClick(Sender: TObject);
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
  // create in-memory tables using FieldDefs and IndexDefs set at design-time
  tblOrders.CreateTable;
  tblOrderPositions.CreateTable;
  // add sample data
  tblOrders.Open;
  tblOrderPositions.Open;
  tblOrders.Append;
  tblOrdersCustomerName.Value := 'Bob Robber';
  tblOrdersOrderDate.Value := EncodeDate(1990,06,01);
  tblOrders.Post;
  tblOrderPositions.Append;
  tblOrderPositionsOrderID.Value := tblOrders.LastAutoincValue('ID');
  tblOrderPositionsProductName.Value := 'Sabre';
  tblOrderPositionsQuantity.Value := 1;
  tblOrderPositions.Post;
  tblOrderPositions.Append;
  tblOrderPositionsOrderID.Value := tblOrders.LastAutoincValue('ID');
  tblOrderPositionsProductName.Value := 'Pistol';
  tblOrderPositionsQuantity.Value := 2;
  tblOrderPositions.Post;
  tblOrders.Append;
  tblOrdersCustomerName.Value := 'Atom Ant';
  tblOrdersOrderDate.Value := EncodeDate(1992,11,11);
  tblOrders.Post;
  tblOrderPositions.Append;
  tblOrderPositionsOrderID.Value := tblOrders.LastAutoincValue('ID');
  tblOrderPositionsProductName.Value := 'Pineapple';
  tblOrderPositionsQuantity.Value := 2;
  tblOrderPositions.Post;

end;

procedure TForm1.btnAddUsingTableClick(Sender: TObject);
begin
  tblOrders.Append;
  tblOrdersCustomerName.Value := edCustomerName.Text;
  tblOrdersOrderDate.Value := dtOrderDate.Date;
  tblOrders.Post;

  tblOrderPositions.Append;
  tblOrderPositionsOrderID.Value := tblOrders.LastAutoincValue('ID');
  tblOrderPositionsProductName.Value := edProduct1.Text;
  tblOrderPositionsQuantity.Value := seQuantity1.Value;
  tblOrderPositions.Post;

  tblOrderPositions.Append;
  tblOrderPositionsOrderID.Value := tblOrders.LastAutoincValue('ID');
  tblOrderPositionsProductName.Value := edProduct2.Text;
  tblOrderPositionsQuantity.Value := seQuantity2.Value;
  tblOrderPositions.Post;

  tblOrderPositions.Append;
  tblOrderPositionsOrderID.Value := tblOrders.LastAutoincValue('ID');
  tblOrderPositionsProductName.Value := edProduct3.Text;
  tblOrderPositionsQuantity.Value := seQuantity3.Value;
  tblOrderPositions.Post;

end;

procedure TForm1.btnAddUsingQueryClick(Sender: TObject);
begin
  qryAddOrder.ParamByName('CustomerName').Value := edCustomerName.Text;
  qryAddOrder.ParamByName('OrderDate').Value := dtOrderDate.Date;
  qryAddOrder.ExecSQL;

  qryAddOrderPosition.ParamByName('ProductName').Value := edProduct1.Text;
  qryAddOrderPosition.ParamByName('Quantity').Value := seQuantity1.Value;
  qryAddOrderPosition.ExecSQL;

  qryAddOrderPosition.ParamByName('ProductName').Value := edProduct2.Text;
  qryAddOrderPosition.ParamByName('Quantity').Value := seQuantity2.Value;
  qryAddOrderPosition.ExecSQL;

  qryAddOrderPosition.ParamByName('ProductName').Value := edProduct3.Text;
  qryAddOrderPosition.ParamByName('Quantity').Value := seQuantity3.Value;
  qryAddOrderPosition.ExecSQL;

  tblOrders.Refresh;
  tblOrders.Last;
  tblOrderPositions.Refresh;
end;

end.
