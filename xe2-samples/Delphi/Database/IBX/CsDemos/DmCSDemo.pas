
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit DmCSDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBTables, DB, IBCustomDataSet, IBTable, IBStoredProc, IBDatabase;

type
  TDmEmployee = class(TDataModule)
    SalesSource: TDataSource;
    CustomerSource: TDataSource;
    EmployeeSource: TDataSource;
    SalaryHistorySource: TDataSource;
    EmployeeDatabase: TIBDatabase;
    IBTransaction1: TIBTransaction;
    ShipOrderProc: TIBStoredProc;
    DeleteEmployeeProc: TIBStoredProc;
    SalesTable: TIBTable;
    CustomerTable: TIBTable;
    CustomerTableCUST_NO: TIntegerField;
    CustomerTableCUSTOMER: TIBStringField;
    CustomerTableCONTACT_FIRST: TIBStringField;
    CustomerTableCONTACT_LAST: TIBStringField;
    CustomerTablePHONE_NO: TIBStringField;
    CustomerTableADDRESS_LINE1: TIBStringField;
    CustomerTableADDRESS_LINE2: TIBStringField;
    CustomerTableCITY: TIBStringField;
    CustomerTableSTATE_PROVINCE: TIBStringField;
    CustomerTableCOUNTRY: TIBStringField;
    CustomerTablePOSTAL_CODE: TIBStringField;
    CustomerTableON_HOLD: TIBStringField;
    SalesTablePO_NUMBER: TIBStringField;
    SalesTableCUST_NO: TIntegerField;
    SalesTableSALES_REP: TSmallintField;
    SalesTableORDER_STATUS: TIBStringField;
    SalesTableORDER_DATE: TDateTimeField;
    SalesTableSHIP_DATE: TDateTimeField;
    SalesTableDATE_NEEDED: TDateTimeField;
    SalesTablePAID: TIBStringField;
    SalesTableQTY_ORDERED: TIntegerField;
    SalesTableTOTAL_VALUE: TIBBCDField;
    SalesTableDISCOUNT: TFloatField;
    SalesTableITEM_TYPE: TIBStringField;
    SalesTableAGED: TFloatField;
    EmployeeTable: TIBTable;
    EmployeeTableEMP_NO: TSmallintField;
    EmployeeTableFIRST_NAME: TIBStringField;
    EmployeeTableLAST_NAME: TIBStringField;
    EmployeeTablePHONE_EXT: TIBStringField;
    EmployeeTableHIRE_DATE: TDateTimeField;
    EmployeeTableDEPT_NO: TIBStringField;
    EmployeeTableJOB_CODE: TIBStringField;
    EmployeeTableJOB_GRADE: TSmallintField;
    EmployeeTableJOB_COUNTRY: TIBStringField;
    EmployeeTableSALARY: TIBBCDField;
    EmployeeTableFULL_NAME: TIBStringField;
    SalaryHistoryTable: TIBTable;
    SalaryHistoryTableEMP_NO: TSmallintField;
    SalaryHistoryTableCHANGE_DATE: TDateTimeField;
    SalaryHistoryTableUPDATER_ID: TIBStringField;
    SalaryHistoryTableOLD_SALARY: TIBBCDField;
    SalaryHistoryTablePERCENT_CHANGE: TFloatField;
    SalaryHistoryTableNEW_SALARY: TFloatField;
    EmployeeLookup: TIBTable;
    EmployeeLookupEMP_NO: TSmallintField;
    EmployeeLookupFIRST_NAME: TIBStringField;
    EmployeeLookupLAST_NAME: TIBStringField;
    EmployeeLookupPHONE_EXT: TIBStringField;
    EmployeeLookupHIRE_DATE: TDateTimeField;
    EmployeeLookupDEPT_NO: TIBStringField;
    EmployeeLookupJOB_CODE: TIBStringField;
    EmployeeLookupJOB_GRADE: TSmallintField;
    EmployeeLookupJOB_COUNTRY: TIBStringField;
    EmployeeLookupSALARY: TIBBCDField;
    EmployeeLookupFULL_NAME: TIBStringField;
    procedure EmployeeTableBeforeDelete(DataSet: TDataSet);
    procedure EmployeeTableAfterPost(DataSet: TDataSet);
    procedure DmEmployeeCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DmEmployee: TDmEmployee;

implementation

{$R *.dfm}

{ Note: Business rules go in the data model.  Here is an example, used by
  the transaction editing demo.  Deletes for the employee table are done
  with a stored procedure rather than the normal BDE record delete
  mechanism, so an audit trail could be provided, etc... }

{ The database, EmployeeDatabase, is the InterBase example EMPLOYEE.GDB database
  accessed thru the BDE alias IBLOCAL.  This database contains examples
  of stored procedures, triggers, check constraints, views, etc., many of
  which are used within this demo project. }

procedure TDmEmployee.EmployeeTableBeforeDelete(DataSet: TDataSet);
begin
  { Assign the current employee's id to the stored procedure's parameter }
  DeleteEmployeeProc.Params.ParamValues['EMP_NUM'] := EmployeeTable['EMP_NO'];
  DeleteEmployeeProc.ExecProc;          { Trigger the stored proc }
  EmployeeTable.Refresh;                { Refresh the data }
  { Block the EmployeeTable delete since the stored procedure did the work }
  Abort;
end;

procedure TDmEmployee.EmployeeTableAfterPost(DataSet: TDataSet);
begin
  { A change in an employee salary triggers a change in the salary history,
    so if that table is open, it needs to be refreshed now }
  with SalaryHistoryTable do if Active then Refresh;
end;

procedure TDmEmployee.DmEmployeeCreate(Sender: TObject);
begin
  EmployeeDatabase.Open;
end;

end.
