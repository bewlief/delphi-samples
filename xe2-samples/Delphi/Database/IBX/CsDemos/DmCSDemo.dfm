object DmEmployee: TDmEmployee
  OldCreateOrder = True
  OnCreate = DmEmployeeCreate
  Height = 224
  Width = 414
  object SalesSource: TDataSource
    DataSet = SalesTable
    Left = 40
    Top = 104
  end
  object CustomerSource: TDataSource
    DataSet = CustomerTable
    Left = 128
    Top = 104
  end
  object EmployeeSource: TDataSource
    DataSet = EmployeeTable
    Left = 224
    Top = 104
  end
  object SalaryHistorySource: TDataSource
    DataSet = SalaryHistoryTable
    Left = 328
    Top = 104
  end
  object EmployeeDatabase: TIBDatabase
    Connected = True
    DatabaseName = 
      'C:\Users\Public\Documents\RAD Studio\9.0\Samples\Data\EMPLOYEE.G' +
      'DB'
    Params.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = IBTransaction1
    ServerType = 'IBServer'
    Left = 40
    Top = 8
  end
  object IBTransaction1: TIBTransaction
    Left = 48
    Top = 168
  end
  object ShipOrderProc: TIBStoredProc
    Database = EmployeeDatabase
    Transaction = IBTransaction1
    StoredProcName = 'SHIP_ORDER'
    Left = 128
    Top = 8
    ParamData = <
      item
        DataType = ftString
        Name = 'PO_NUM'
        ParamType = ptInput
      end>
  end
  object DeleteEmployeeProc: TIBStoredProc
    Database = EmployeeDatabase
    Transaction = IBTransaction1
    StoredProcName = 'DELETE_EMPLOYEE'
    Left = 224
    Top = 8
  end
  object SalesTable: TIBTable
    Database = EmployeeDatabase
    Transaction = IBTransaction1
    IndexFieldNames = 'PO_NUMBER'
    TableName = 'SALES'
    Left = 40
    Top = 56
    object SalesTablePO_NUMBER: TIBStringField
      FieldName = 'PO_NUMBER'
      Required = True
      Size = 8
    end
    object SalesTableCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
      Required = True
    end
    object SalesTableSALES_REP: TSmallintField
      FieldName = 'SALES_REP'
    end
    object SalesTableORDER_STATUS: TIBStringField
      FieldName = 'ORDER_STATUS'
      Size = 7
    end
    object SalesTableORDER_DATE: TDateTimeField
      FieldName = 'ORDER_DATE'
    end
    object SalesTableSHIP_DATE: TDateTimeField
      FieldName = 'SHIP_DATE'
    end
    object SalesTableDATE_NEEDED: TDateTimeField
      FieldName = 'DATE_NEEDED'
    end
    object SalesTablePAID: TIBStringField
      FieldName = 'PAID'
      Size = 1
    end
    object SalesTableQTY_ORDERED: TIntegerField
      FieldName = 'QTY_ORDERED'
    end
    object SalesTableTOTAL_VALUE: TIBBCDField
      FieldName = 'TOTAL_VALUE'
      Required = True
      Precision = 9
      Size = 2
    end
    object SalesTableDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
    end
    object SalesTableITEM_TYPE: TIBStringField
      FieldName = 'ITEM_TYPE'
      Size = 12
    end
    object SalesTableAGED: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'AGED'
      ReadOnly = True
    end
  end
  object CustomerTable: TIBTable
    Database = EmployeeDatabase
    Transaction = IBTransaction1
    IndexFieldNames = 'CUST_NO'
    MasterFields = 'CUST_NO'
    MasterSource = SalesSource
    TableName = 'CUSTOMER'
    Left = 128
    Top = 56
    object CustomerTableCUST_NO: TIntegerField
      FieldName = 'CUST_NO'
      Required = True
    end
    object CustomerTableCUSTOMER: TIBStringField
      FieldName = 'CUSTOMER'
      Required = True
      Size = 25
    end
    object CustomerTableCONTACT_FIRST: TIBStringField
      FieldName = 'CONTACT_FIRST'
      Size = 15
    end
    object CustomerTableCONTACT_LAST: TIBStringField
      FieldName = 'CONTACT_LAST'
    end
    object CustomerTablePHONE_NO: TIBStringField
      FieldName = 'PHONE_NO'
    end
    object CustomerTableADDRESS_LINE1: TIBStringField
      FieldName = 'ADDRESS_LINE1'
      Size = 30
    end
    object CustomerTableADDRESS_LINE2: TIBStringField
      FieldName = 'ADDRESS_LINE2'
      Size = 30
    end
    object CustomerTableCITY: TIBStringField
      FieldName = 'CITY'
      Size = 25
    end
    object CustomerTableSTATE_PROVINCE: TIBStringField
      FieldName = 'STATE_PROVINCE'
      Size = 15
    end
    object CustomerTableCOUNTRY: TIBStringField
      FieldName = 'COUNTRY'
      Size = 15
    end
    object CustomerTablePOSTAL_CODE: TIBStringField
      FieldName = 'POSTAL_CODE'
      Size = 12
    end
    object CustomerTableON_HOLD: TIBStringField
      FieldName = 'ON_HOLD'
      Size = 1
    end
  end
  object EmployeeTable: TIBTable
    Database = EmployeeDatabase
    Transaction = IBTransaction1
    IndexFieldNames = 'EMP_NO'
    TableName = 'EMPLOYEE'
    Left = 224
    Top = 56
    object EmployeeTableEMP_NO: TSmallintField
      FieldName = 'EMP_NO'
      Required = True
    end
    object EmployeeTableFIRST_NAME: TIBStringField
      FieldName = 'FIRST_NAME'
      Required = True
      Size = 15
    end
    object EmployeeTableLAST_NAME: TIBStringField
      FieldName = 'LAST_NAME'
      Required = True
    end
    object EmployeeTablePHONE_EXT: TIBStringField
      FieldName = 'PHONE_EXT'
      Size = 4
    end
    object EmployeeTableHIRE_DATE: TDateTimeField
      FieldName = 'HIRE_DATE'
    end
    object EmployeeTableDEPT_NO: TIBStringField
      FieldName = 'DEPT_NO'
      Required = True
      Size = 3
    end
    object EmployeeTableJOB_CODE: TIBStringField
      FieldName = 'JOB_CODE'
      Required = True
      Size = 5
    end
    object EmployeeTableJOB_GRADE: TSmallintField
      FieldName = 'JOB_GRADE'
      Required = True
    end
    object EmployeeTableJOB_COUNTRY: TIBStringField
      FieldName = 'JOB_COUNTRY'
      Required = True
      Size = 15
    end
    object EmployeeTableSALARY: TIBBCDField
      FieldName = 'SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
    object EmployeeTableFULL_NAME: TIBStringField
      FieldKind = fkInternalCalc
      FieldName = 'FULL_NAME'
      ReadOnly = True
      Size = 37
    end
  end
  object SalaryHistoryTable: TIBTable
    Database = EmployeeDatabase
    Transaction = IBTransaction1
    IndexFieldNames = 'EMP_NO'
    MasterFields = 'EMP_NO'
    MasterSource = EmployeeSource
    TableName = 'SALARY_HISTORY'
    Left = 328
    Top = 56
    object SalaryHistoryTableEMP_NO: TSmallintField
      FieldName = 'EMP_NO'
      Required = True
    end
    object SalaryHistoryTableCHANGE_DATE: TDateTimeField
      FieldName = 'CHANGE_DATE'
    end
    object SalaryHistoryTableUPDATER_ID: TIBStringField
      FieldName = 'UPDATER_ID'
      Required = True
    end
    object SalaryHistoryTableOLD_SALARY: TIBBCDField
      FieldName = 'OLD_SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
    object SalaryHistoryTablePERCENT_CHANGE: TFloatField
      FieldName = 'PERCENT_CHANGE'
    end
    object SalaryHistoryTableNEW_SALARY: TFloatField
      FieldKind = fkInternalCalc
      FieldName = 'NEW_SALARY'
      ReadOnly = True
    end
  end
  object EmployeeLookup: TIBTable
    Database = EmployeeDatabase
    Transaction = IBTransaction1
    IndexFieldNames = 'EMP_NO'
    TableName = 'EMPLOYEE'
    Left = 328
    Top = 8
    object EmployeeLookupEMP_NO: TSmallintField
      FieldName = 'EMP_NO'
      Required = True
    end
    object EmployeeLookupFIRST_NAME: TIBStringField
      FieldName = 'FIRST_NAME'
      Required = True
      Size = 15
    end
    object EmployeeLookupLAST_NAME: TIBStringField
      FieldName = 'LAST_NAME'
      Required = True
    end
    object EmployeeLookupPHONE_EXT: TIBStringField
      FieldName = 'PHONE_EXT'
      Size = 4
    end
    object EmployeeLookupHIRE_DATE: TDateTimeField
      FieldName = 'HIRE_DATE'
    end
    object EmployeeLookupDEPT_NO: TIBStringField
      FieldName = 'DEPT_NO'
      Required = True
      Size = 3
    end
    object EmployeeLookupJOB_CODE: TIBStringField
      FieldName = 'JOB_CODE'
      Required = True
      Size = 5
    end
    object EmployeeLookupJOB_GRADE: TSmallintField
      FieldName = 'JOB_GRADE'
      Required = True
    end
    object EmployeeLookupJOB_COUNTRY: TIBStringField
      FieldName = 'JOB_COUNTRY'
      Required = True
      Size = 15
    end
    object EmployeeLookupSALARY: TIBBCDField
      FieldName = 'SALARY'
      Required = True
      Precision = 18
      Size = 2
    end
    object EmployeeLookupFULL_NAME: TIBStringField
      FieldKind = fkInternalCalc
      FieldName = 'FULL_NAME'
      ReadOnly = True
      Size = 37
    end
  end
end
