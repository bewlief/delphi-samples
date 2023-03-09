object MastData: TMastData
  OldCreateOrder = True
  OnCreate = MastDataCreate
  Height = 479
  Width = 741
  object Database: TIBDatabase
    Connected = True
    DatabaseName = 
      'C:\Users\Public\Documents\RAD Studio\9.0\Samples\Data\MASTSQL.GD' +
      'B'
    Params.Strings = (
      'USER_NAME=sysdba'
      'PASSWORD=masterkey')
    LoginPrompt = False
    DefaultTransaction = Transaction
    ServerType = 'IBServer'
    AllowStreamedConnected = False
    Left = 340
    Top = 292
  end
  object NextCust: TIBTable
    Database = Database
    Transaction = Transaction
    FieldDefs = <
      item
        Name = 'NEWCUST'
        Attributes = [faRequired]
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY4'
        Fields = 'NEWCUST'
        Options = [ixPrimary, ixUnique]
      end>
    StoreDefs = True
    TableName = 'NEXTCUST'
    Left = 156
    Top = 60
    object NextCustNewCust: TFloatField
      FieldName = 'NEWCUST'
    end
  end
  object Parts: TIBTable
    Database = Database
    Transaction = Transaction
    BeforeOpen = PartsBeforeOpen
    OnCalcFields = PartsCalcFields
    CachedUpdates = True
    FieldDefs = <
      item
        Name = 'PARTNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'VENDORNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'DESCRIPTION'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'ONHAND'
        DataType = ftFloat
      end
      item
        Name = 'ONORDER'
        DataType = ftFloat
      end
      item
        Name = 'COST'
        DataType = ftFloat
      end
      item
        Name = 'LISTPRICE'
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY7'
        Fields = 'PARTNO'
        Options = [ixPrimary, ixUnique]
      end
      item
        Name = 'DESCRIPTION'
        Fields = 'DESCRIPTION'
      end
      item
        Name = 'VENDORNO'
        Fields = 'VENDORNO'
      end>
    StoreDefs = True
    TableName = 'PARTS'
    Left = 20
    Top = 157
    object PartsPartNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 8
      FieldName = 'PARTNO'
      Required = True
    end
    object PartsDescription: TIBStringField
      DisplayWidth = 21
      FieldName = 'DESCRIPTION'
      Required = True
      Size = 30
    end
    object PartsVendorNo: TFloatField
      DisplayWidth = 9
      FieldName = 'VENDORNO'
    end
    object PartsOnHand: TFloatField
      DisplayWidth = 9
      FieldName = 'ONHAND'
    end
    object PartsOnOrder: TFloatField
      DisplayWidth = 10
      FieldName = 'ONORDER'
    end
    object PartsBackOrd: TBooleanField
      DisplayWidth = 9
      FieldKind = fkCalculated
      FieldName = 'BACKORD'
      DisplayValues = 'Yes;No'
      Calculated = True
    end
    object PartsCost: TCurrencyField
      DisplayWidth = 12
      FieldName = 'COST'
    end
    object PartsListPrice: TCurrencyField
      DisplayWidth = 12
      FieldName = 'LISTPRICE'
    end
  end
  object PartsQuery: TIBQuery
    Database = Database
    Transaction = Transaction
    OnCalcFields = PartsQueryCalcFields
    SQL.Strings = (
      'SELECT * FROM PARTS'
      ' WHERE (PARTS.ONORDER > PARTS.ONHAND)'
      '')
    Left = 156
    Top = 157
    object PartsQueryPartNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 8
      FieldName = 'PARTNO'
    end
    object PartsQueryDescription: TIBStringField
      DisplayWidth = 21
      FieldName = 'DESCRIPTION'
      Size = 30
    end
    object PartsQueryVendorNo: TFloatField
      FieldName = 'VENDORNO'
    end
    object PartsQueryOnHand: TFloatField
      DisplayWidth = 9
      FieldName = 'ONHAND'
    end
    object PartsQueryOnOrder: TFloatField
      DisplayWidth = 10
      FieldName = 'ONORDER'
    end
    object PartsQueryBackOrd: TBooleanField
      DisplayWidth = 9
      FieldKind = fkCalculated
      FieldName = 'BACKORD'
      DisplayValues = 'Yes;No'
      Calculated = True
    end
    object PartsQueryCost: TCurrencyField
      FieldName = 'COST'
    end
    object PartsQueryListPrice: TCurrencyField
      FieldName = 'LISTPRICE'
    end
  end
  object Vendors: TIBTable
    Database = Database
    Transaction = Transaction
    FieldDefs = <
      item
        Name = 'VENDORNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'VENDORNAME'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'ADDRESS1'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'ADDRESS2'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'CITY'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'STATE'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'ZIP'
        DataType = ftString
        Size = 10
      end
      item
        Name = 'COUNTRY'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'PHONE'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'FAX'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'PREFERRED'
        DataType = ftString
        Size = 1
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY8'
        Fields = 'VENDORNO'
        Options = [ixPrimary, ixUnique]
      end>
    ReadOnly = True
    StoreDefs = True
    TableName = 'VENDORS'
    Left = 20
    Top = 252
  end
  object Orders: TIBTable
    Database = Database
    Transaction = Transaction
    AfterCancel = OrdersAfterCancel
    AfterDelete = OrdersAfterDelete
    AfterPost = OrdersAfterPost
    BeforeCancel = OrdersBeforeCancel
    BeforeClose = OrdersBeforeClose
    BeforeDelete = OrdersBeforeDelete
    BeforeEdit = OrdersBeforeEdit
    BeforeInsert = OrdersBeforeInsert
    BeforeOpen = OrdersBeforeOpen
    OnCalcFields = OrdersCalcFields
    OnNewRecord = OrdersNewRecord
    CachedUpdates = True
    FieldDefs = <
      item
        Name = 'ORDERNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'CUSTNO'
        DataType = ftFloat
      end
      item
        Name = 'SALEDATE'
        DataType = ftDateTime
      end
      item
        Name = 'SHIPDATE'
        DataType = ftDateTime
      end
      item
        Name = 'EMPNO'
        DataType = ftInteger
      end
      item
        Name = 'SHIPTOCONTACT'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'SHIPTOADDR1'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'SHIPTOADDR2'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'SHIPTOCITY'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'SHIPTOSTATE'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'SHIPTOZIP'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'SHIPTOCOUNTRY'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'SHIPTOPHONE'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'SHIPVIA'
        DataType = ftWideString
        Size = 7
      end
      item
        Name = 'PO'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'TERMS'
        DataType = ftWideString
        Size = 6
      end
      item
        Name = 'PAYMENTMETHOD'
        DataType = ftWideString
        Size = 7
      end
      item
        Name = 'ITEMSTOTAL'
        DataType = ftFloat
      end
      item
        Name = 'TAXRATE'
        DataType = ftFloat
      end
      item
        Name = 'FREIGHT'
        DataType = ftFloat
      end
      item
        Name = 'AMOUNTPAID'
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY6'
        Fields = 'ORDERNO'
        Options = [ixPrimary, ixUnique]
      end>
    IndexFieldNames = 'ORDERNO'
    StoreDefs = True
    TableName = 'ORDERS'
    Left = 20
    Top = 12
    object OrdersOrderNo: TFloatField
      Alignment = taLeftJustify
      DisplayLabel = 'OrderNo'
      FieldName = 'ORDERNO'
    end
    object OrdersCustNo: TFloatField
      Alignment = taLeftJustify
      DisplayLabel = 'CustNo'
      FieldName = 'CUSTNO'
      Required = True
      OnChange = OrdersCustNoChange
    end
    object OrdersSaleDate: TDateTimeField
      DisplayLabel = 'Sale Date'
      FieldName = 'SALEDATE'
      OnValidate = OrdersSaleDateValidate
    end
    object OrdersShipDate: TDateTimeField
      DisplayLabel = 'Ship Date'
      FieldName = 'SHIPDATE'
    end
    object OrdersShipToContact: TIBStringField
      DisplayLabel = 'Ship to Contact'
      FieldName = 'SHIPTOCONTACT'
    end
    object OrdersShipToAddr1: TIBStringField
      DisplayLabel = 'Ship to Addr1'
      FieldName = 'SHIPTOADDR1'
      Size = 30
    end
    object OrdersShipToAddr2: TIBStringField
      DisplayLabel = 'Ship to Addr2'
      FieldName = 'SHIPTOADDR2'
      Size = 30
    end
    object OrdersShipToCity: TIBStringField
      DisplayLabel = 'Ship to City'
      FieldName = 'SHIPTOCITY'
      Size = 15
    end
    object OrdersShipToState: TIBStringField
      DisplayLabel = 'Ship to State'
      FieldName = 'SHIPTOSTATE'
    end
    object OrdersShipToZip: TIBStringField
      DisplayLabel = 'Ship to Zip'
      FieldName = 'SHIPTOZIP'
      Size = 10
    end
    object OrdersShipToCountry: TIBStringField
      DisplayLabel = 'Ship to Country'
      FieldName = 'SHIPTOCOUNTRY'
    end
    object OrdersShipToPhone: TIBStringField
      DisplayLabel = 'Ship to Phone'
      FieldName = 'SHIPTOPHONE'
      Size = 15
    end
    object OrdersShipVIA: TIBStringField
      DisplayLabel = 'Ship VIA'
      FieldName = 'SHIPVIA'
      Size = 7
    end
    object OrdersPO: TIBStringField
      FieldName = 'PO'
      Size = 15
    end
    object OrdersEmpNo: TIntegerField
      DisplayLabel = 'Emp No'
      FieldName = 'EMPNO'
      Required = True
    end
    object OrdersTerms: TIBStringField
      DisplayLabel = 'Terms'
      FieldName = 'TERMS'
      Size = 6
    end
    object OrdersPaymentMethod: TIBStringField
      DisplayLabel = 'Payment Hod'
      FieldName = 'PAYMENTMETHOD'
      Size = 7
    end
    object OrdersItemsTotal: TCurrencyField
      DisplayLabel = 'Items Total'
      FieldName = 'ITEMSTOTAL'
    end
    object OrdersTaxRate: TFloatField
      DisplayLabel = 'Tax Rate'
      FieldName = 'TAXRATE'
      DisplayFormat = '0.00%;(0.00%);0%'
      MaxValue = 100.000000000000000000
    end
    object OrdersTaxTotal: TCurrencyField
      DisplayLabel = 'Tax Total'
      FieldKind = fkCalculated
      FieldName = 'TAXTOTAL'
      Calculated = True
    end
    object OrdersFreight: TCurrencyField
      DisplayLabel = 'Freight'
      FieldName = 'FREIGHT'
      OnValidate = OrdersFreightValidate
    end
    object OrdersAmountPaid: TCurrencyField
      DisplayLabel = 'Amount Paid'
      FieldName = 'AMOUNTPAID'
    end
    object OrdersAmountDue: TCurrencyField
      DisplayLabel = 'Amount Due'
      FieldKind = fkCalculated
      FieldName = 'AMOUNTDUE'
      Calculated = True
    end
    object OrdersSalesPerson: TIBStringField
      DisplayLabel = 'Sales Person'
      FieldKind = fkLookup
      FieldName = 'SALESPERSON'
      LookupDataSet = Emps
      LookupKeyFields = 'EmpNo'
      LookupResultField = 'FullName'
      KeyFields = 'EmpNo'
      Size = 40
      Lookup = True
    end
  end
  object CustByOrd: TIBTable
    Database = Database
    Transaction = Transaction
    FieldDefs = <
      item
        Name = 'CUSTNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'COMPANY'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'ADDR1'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'ADDR2'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'CITY'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'STATE'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'ZIP'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'COUNTRY'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'PHONE'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'FAX'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'TAXRATE'
        DataType = ftFloat
      end
      item
        Name = 'CONTACT'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'LASTINVOICEDATE'
        DataType = ftDateTime
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY1'
        Fields = 'CUSTNO'
        Options = [ixPrimary, ixUnique]
      end
      item
        Name = 'COMPANY'
        Fields = 'COMPANY'
      end>
    IndexFieldNames = 'CUSTNO'
    MasterFields = 'CUSTNO'
    MasterSource = OrdersSource
    ReadOnly = True
    StoreDefs = True
    TableName = 'CUSTOMER'
    Left = 20
    Top = 60
    object CustByOrdCustNo: TFloatField
      DisplayLabel = 'Customer ID'
      FieldName = 'CUSTNO'
    end
    object CustByOrdCompany: TIBStringField
      DisplayLabel = 'Company'
      FieldName = 'COMPANY'
      Size = 30
    end
    object CustByOrdAddr1: TIBStringField
      DisplayLabel = 'Address 1'
      FieldName = 'ADDR1'
      Size = 30
    end
    object CustByOrdAddr2: TIBStringField
      DisplayLabel = 'Address 2'
      FieldName = 'ADDR2'
      Size = 30
    end
    object CustByOrdCity: TIBStringField
      DisplayLabel = 'City'
      FieldName = 'CITY'
      Size = 15
    end
    object CustByOrdState: TIBStringField
      DisplayLabel = 'State'
      FieldName = 'STATE'
    end
    object CustByOrdZip: TIBStringField
      DisplayLabel = 'Zip'
      FieldName = 'ZIP'
      Size = 10
    end
    object CustByOrdCountry: TIBStringField
      DisplayLabel = 'Country'
      FieldName = 'COUNTRY'
    end
    object CustByOrdPhone: TIBStringField
      DisplayLabel = 'Phone'
      FieldName = 'PHONE'
      Size = 15
    end
    object CustByOrdFAX: TIBStringField
      DisplayLabel = 'Fax'
      FieldName = 'FAX'
      Size = 15
    end
    object CustByOrdTaxRate: TFloatField
      DisplayLabel = 'Tax Rate'
      FieldName = 'TAXRATE'
    end
    object CustByOrdContact: TIBStringField
      DisplayLabel = 'Contact'
      FieldName = 'CONTACT'
    end
    object CustByOrdLastInvoiceDate: TDateTimeField
      DisplayLabel = 'Last Invoice Date'
      FieldName = 'LASTINVOICEDATE'
    end
  end
  object Items: TIBTable
    Database = Database
    Transaction = Transaction
    AfterDelete = ItemsAfterDelete
    AfterPost = ItemsAfterPost
    BeforeDelete = EnsureOrdersEdit
    BeforeEdit = ItemsBeforeEdit
    BeforeInsert = EnsureOrdersEdit
    BeforeOpen = ItemsBeforeOpen
    BeforePost = ItemsBeforePost
    OnCalcFields = ItemsCalcFields
    OnNewRecord = ItemsNewRecord
    CachedUpdates = True
    FieldDefs = <
      item
        Name = 'ITEMNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'ORDERNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'PARTNO'
        DataType = ftFloat
      end
      item
        Name = 'QTY'
        DataType = ftInteger
      end
      item
        Name = 'DISCOUNT'
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY3'
        Fields = 'ORDERNO;ITEMNO'
        Options = [ixPrimary, ixUnique]
      end>
    IndexFieldNames = 'ORDERNO'
    MasterFields = 'ORDERNO'
    MasterSource = OrdersSource
    StoreDefs = True
    TableName = 'ITEMS'
    Left = 20
    Top = 108
    object ItemsItemNo: TFloatField
      DisplayLabel = 'Item No'
      FieldName = 'ITEMNO'
      Visible = False
    end
    object ItemsOrderNo: TFloatField
      DisplayLabel = 'Order No'
      FieldName = 'ORDERNO'
      Visible = False
    end
    object ItemsPartNo: TFloatField
      DisplayLabel = 'Part No'
      FieldName = 'PARTNO'
      OnValidate = ItemsPartNoValidate
    end
    object ItemsDescription: TIBStringField
      DisplayLabel = 'Description'
      DisplayWidth = 28
      FieldKind = fkLookup
      FieldName = 'DESCRIPTION'
      LookupDataSet = Parts
      LookupKeyFields = 'PartNo'
      LookupResultField = 'Description'
      KeyFields = 'PartNo'
      ReadOnly = True
      Size = 30
      Lookup = True
    end
    object ItemsSellPrice: TCurrencyField
      DisplayLabel = 'Sell Price'
      DisplayWidth = 9
      FieldKind = fkLookup
      FieldName = 'SELLPRICE'
      LookupDataSet = Parts
      LookupKeyFields = 'PartNo'
      LookupResultField = 'ListPrice'
      KeyFields = 'PartNo'
      Lookup = True
    end
    object ItemsQty: TIntegerField
      DisplayLabel = 'Qty'
      DisplayWidth = 5
      FieldName = 'QTY'
      OnValidate = ItemsQtyValidate
    end
    object ItemsDiscount: TFloatField
      DisplayLabel = 'Discount'
      DisplayWidth = 7
      FieldName = 'DISCOUNT'
      DisplayFormat = '0.00%'
      MaxValue = 100.000000000000000000
    end
    object ItemsExtPrice: TCurrencyField
      DisplayLabel = 'Ext Price'
      DisplayWidth = 10
      FieldKind = fkCalculated
      FieldName = 'EXTPRICE'
      Calculated = True
    end
  end
  object NextOrd: TIBTable
    Database = Database
    Transaction = Transaction
    FieldDefs = <
      item
        Name = 'NEWKEY'
        Attributes = [faRequired]
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY5'
        Fields = 'NEWKEY'
        Options = [ixPrimary, ixUnique]
      end>
    StoreDefs = True
    TableName = 'NEXTORD'
    Left = 155
    Top = 12
    object NextOrdNewKey: TFloatField
      FieldName = 'NEWKEY'
    end
  end
  object Emps: TIBTable
    Database = Database
    Transaction = Transaction
    OnCalcFields = EmpsCalcFields
    FieldDefs = <
      item
        Name = 'EMPNO'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'LASTNAME'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'FIRSTNAME'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'PHONEEXT'
        DataType = ftWideString
        Size = 4
      end
      item
        Name = 'HIREDATE'
        DataType = ftDateTime
      end
      item
        Name = 'SALARY'
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY2'
        Fields = 'EMPNO'
        Options = [ixPrimary, ixUnique]
      end
      item
        Name = 'LASTNAME'
        Fields = 'LASTNAME;FIRSTNAME'
      end>
    IndexFieldNames = 'EMPNO'
    ReadOnly = True
    StoreDefs = True
    TableName = 'EMPLOYEE'
    Left = 20
    Top = 204
    object EmpsEmpNo: TIntegerField
      FieldName = 'EMPNO'
    end
    object EmpsFullName: TIBStringField
      FieldKind = fkCalculated
      FieldName = 'FULLNAME'
      Calculated = True
    end
    object EmpsLastName: TIBStringField
      FieldName = 'LASTNAME'
    end
    object EmpsFirstName: TIBStringField
      FieldName = 'FIRSTNAME'
      Size = 15
    end
    object EmpsPhoneExt: TIBStringField
      FieldName = 'PHONEEXT'
      Size = 4
    end
    object EmpsHireDate: TDateTimeField
      FieldName = 'HIREDATE'
    end
    object EmpsSalary: TFloatField
      FieldName = 'SALARY'
    end
  end
  object LastItemQuery: TIBQuery
    Database = Database
    Transaction = Transaction
    DataSource = OrdersSource
    SQL.Strings = (
      'SELECT MAX(ITEMNO) FROM ITEMS'
      '  WHERE ORDERNO = :ORDERNO')
    Left = 155
    Top = 108
    ParamData = <
      item
        DataType = ftFloat
        Name = 'OrderNo'
        ParamType = ptUnknown
        Size = 8
      end>
  end
  object Cust: TIBTable
    Database = Database
    Transaction = Transaction
    BeforeOpen = CustBeforeOpen
    BeforePost = CustBeforePost
    CachedUpdates = True
    FieldDefs = <
      item
        Name = 'CUSTNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'COMPANY'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'ADDR1'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'ADDR2'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'CITY'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'STATE'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'ZIP'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'COUNTRY'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'PHONE'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'FAX'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'TAXRATE'
        DataType = ftFloat
      end
      item
        Name = 'CONTACT'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'LASTINVOICEDATE'
        DataType = ftDateTime
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY1'
        Fields = 'CUSTNO'
        Options = [ixPrimary, ixUnique]
      end
      item
        Name = 'COMPANY'
        Fields = 'COMPANY'
      end>
    StoreDefs = True
    TableName = 'CUSTOMER'
    Left = 236
    Top = 13
    object CustCustNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 6
      FieldName = 'CUSTNO'
    end
    object CustCompany: TIBStringField
      DisplayWidth = 26
      FieldName = 'COMPANY'
      Required = True
      Size = 30
    end
    object CustPhone: TIBStringField
      DisplayWidth = 15
      FieldName = 'PHONE'
      Size = 15
    end
    object CustLastInvoiceDate: TDateTimeField
      DisplayLabel = 'LastInvoice'
      DisplayWidth = 10
      FieldName = 'LASTINVOICEDATE'
      DisplayFormat = 'DDDDD'
    end
    object CustAddr1: TIBStringField
      FieldName = 'ADDR1'
      Size = 30
    end
    object CustAddr2: TIBStringField
      FieldName = 'ADDR2'
      Size = 30
    end
    object CustCity: TIBStringField
      FieldName = 'CITY'
      Size = 15
    end
    object CustState: TIBStringField
      FieldName = 'STATE'
    end
    object CustZip: TIBStringField
      FieldName = 'ZIP'
      Size = 10
    end
    object CustCountry: TIBStringField
      FieldName = 'COUNTRY'
    end
    object CustFAX: TIBStringField
      FieldName = 'FAX'
      Size = 15
    end
    object CustTaxRate: TFloatField
      FieldName = 'TAXRATE'
    end
    object CustContact: TIBStringField
      FieldName = 'CONTACT'
    end
  end
  object CustQuery: TIBQuery
    Database = Database
    Transaction = Transaction
    SQL.Strings = (
      'SELECT CUSTOMER.CUSTNO, CUSTOMER.COMPANY,'
      '             CUSTOMER.LASTINVOICEDATE, CUSTOMER.PHONE'
      ' FROM CUSTOMER'
      ' WHERE'
      '   (CUSTOMER.LASTINVOICEDATE >= :FromDate) AND'
      '   (CUSTOMER.LASTINVOICEDATE <= :ToDate)')
    Left = 236
    Top = 61
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'FromDate'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'ToDate'
        ParamType = ptUnknown
      end>
    object CustQueryCustNo: TFloatField
      Alignment = taLeftJustify
      DisplayWidth = 6
      FieldName = 'CUSTNO'
    end
    object CustQueryCompany: TIBStringField
      DisplayWidth = 26
      FieldName = 'COMPANY'
      Size = 30
    end
    object CustQueryPhone: TIBStringField
      DisplayWidth = 15
      FieldName = 'PHONE'
      Size = 15
    end
    object CustQueryLastInvoiceDate: TDateTimeField
      DisplayLabel = 'LastInvoice'
      DisplayWidth = 10
      FieldName = 'LASTINVOICEDATE'
      DisplayFormat = 'DDDDD'
    end
  end
  object OrdByCust: TIBTable
    Database = Database
    Transaction = Transaction
    OnCalcFields = OrdByCustCalcFields
    FieldDefs = <
      item
        Name = 'ORDERNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'CUSTNO'
        DataType = ftFloat
      end
      item
        Name = 'SALEDATE'
        DataType = ftDateTime
      end
      item
        Name = 'SHIPDATE'
        DataType = ftDateTime
      end
      item
        Name = 'EMPNO'
        DataType = ftInteger
      end
      item
        Name = 'SHIPTOCONTACT'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'SHIPTOADDR1'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'SHIPTOADDR2'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'SHIPTOCITY'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'SHIPTOSTATE'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'SHIPTOZIP'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'SHIPTOCOUNTRY'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'SHIPTOPHONE'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'SHIPVIA'
        DataType = ftWideString
        Size = 7
      end
      item
        Name = 'PO'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'TERMS'
        DataType = ftWideString
        Size = 6
      end
      item
        Name = 'PAYMENTMETHOD'
        DataType = ftWideString
        Size = 7
      end
      item
        Name = 'ITEMSTOTAL'
        DataType = ftFloat
      end
      item
        Name = 'TAXRATE'
        DataType = ftFloat
      end
      item
        Name = 'FREIGHT'
        DataType = ftFloat
      end
      item
        Name = 'AMOUNTPAID'
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY6'
        Fields = 'ORDERNO'
        Options = [ixPrimary, ixUnique]
      end>
    IndexFieldNames = 'CUSTNO'
    MasterFields = 'CUSTNO'
    MasterSource = CustMasterSrc
    ReadOnly = True
    StoreDefs = True
    TableName = 'ORDERS'
    Left = 236
    Top = 108
    object OrdByCustOrderNo: TFloatField
      Alignment = taLeftJustify
      DisplayLabel = 'Order No'
      DisplayWidth = 10
      FieldName = 'ORDERNO'
    end
    object OrdByCustCustNo: TFloatField
      DisplayLabel = 'Cust No'
      FieldName = 'CUSTNO'
      Visible = False
    end
    object OrdByCustSaleDate: TDateTimeField
      DisplayLabel = 'Sale Date'
      DisplayWidth = 9
      FieldName = 'SALEDATE'
      DisplayFormat = 'DDDDD'
    end
    object OrdByCustShipDate: TDateTimeField
      DisplayLabel = 'Ship Date'
      DisplayWidth = 9
      FieldName = 'SHIPDATE'
      DisplayFormat = 'DDDDD'
    end
    object OrdByCustItemsTotal: TCurrencyField
      DisplayLabel = 'Items Total'
      FieldName = 'ITEMSTOTAL'
      Visible = False
    end
    object OrdByCustTaxRate: TFloatField
      DisplayLabel = 'Tax Rate'
      FieldName = 'TAXRATE'
      Visible = False
    end
    object OrdByCustFreight: TCurrencyField
      DisplayLabel = 'Freight'
      FieldName = 'FREIGHT'
      Visible = False
    end
    object OrdByCustAmountPaid: TCurrencyField
      DisplayLabel = 'Amount Paid'
      DisplayWidth = 14
      FieldName = 'AMOUNTPAID'
    end
    object OrdByCustAmountDue: TCurrencyField
      DisplayLabel = 'Amont Due'
      DisplayWidth = 14
      FieldKind = fkCalculated
      FieldName = 'AMOUNTDUE'
      Calculated = True
    end
  end
  object CustByComp: TIBTable
    Database = Database
    Transaction = Transaction
    FieldDefs = <
      item
        Name = 'CUSTNO'
        Attributes = [faRequired]
        DataType = ftFloat
      end
      item
        Name = 'COMPANY'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'ADDR1'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'ADDR2'
        DataType = ftWideString
        Size = 30
      end
      item
        Name = 'CITY'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'STATE'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'ZIP'
        DataType = ftWideString
        Size = 10
      end
      item
        Name = 'COUNTRY'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'PHONE'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'FAX'
        DataType = ftWideString
        Size = 15
      end
      item
        Name = 'TAXRATE'
        DataType = ftFloat
      end
      item
        Name = 'CONTACT'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'LASTINVOICEDATE'
        DataType = ftDateTime
      end>
    IndexDefs = <
      item
        Name = 'RDB$PRIMARY1'
        Fields = 'CUSTNO'
        Options = [ixPrimary, ixUnique]
      end
      item
        Name = 'COMPANY'
        Fields = 'COMPANY'
      end>
    IndexFieldNames = 'COMPANY'
    StoreDefs = True
    TableName = 'CUSTOMER'
    Left = 236
    Top = 156
  end
  object CustByLastInvQuery: TIBQuery
    Database = Database
    Transaction = Transaction
    SQL.Strings = (
      'SELECT * FROM CUSTOMER'
      'ORDER BY'
      'LASTINVOICEDATE DESC')
    Left = 240
    Top = 208
    object CustByLastInvQueryCustNo: TFloatField
      FieldName = 'CUSTNO'
      Origin = '"CUSTOMER.DB".CustNo'
    end
    object CustByLastInvQueryCompany: TIBStringField
      FieldName = 'COMPANY'
      Origin = '"CUSTOMER.DB".Company'
      Size = 30
    end
    object CustByLastInvQueryAddr1: TIBStringField
      FieldName = 'ADDR1'
      Origin = '"CUSTOMER.DB".Addr1'
      Size = 30
    end
    object CustByLastInvQueryAddr2: TIBStringField
      FieldName = 'ADDR2'
      Origin = '"CUSTOMER.DB".Addr2'
      Size = 30
    end
    object CustByLastInvQueryCity: TIBStringField
      FieldName = 'CITY'
      Origin = '"CUSTOMER.DB".City'
      Size = 15
    end
    object CustByLastInvQueryState: TIBStringField
      FieldName = 'STATE'
      Origin = '"CUSTOMER.DB".State'
    end
    object CustByLastInvQueryZip: TIBStringField
      FieldName = 'ZIP'
      Origin = '"CUSTOMER.DB".Zip'
      Size = 10
    end
    object CustByLastInvQueryCountry: TIBStringField
      FieldName = 'COUNTRY'
      Origin = '"CUSTOMER.DB".Country'
    end
    object CustByLastInvQueryPhone: TIBStringField
      FieldName = 'PHONE'
      Origin = '"CUSTOMER.DB".Phone'
      Size = 15
    end
    object CustByLastInvQueryFAX: TIBStringField
      FieldName = 'FAX'
      Origin = '"CUSTOMER.DB".FAX'
      Size = 15
    end
    object CustByLastInvQueryTaxRate: TFloatField
      FieldName = 'TAXRATE'
      Origin = '"CUSTOMER.DB".TaxRate'
    end
    object CustByLastInvQueryContact: TIBStringField
      FieldName = 'CONTACT'
      Origin = '"CUSTOMER.DB".Contact'
    end
    object CustByLastInvQueryLastInvoiceDate: TDateTimeField
      FieldName = 'LASTINVOICEDATE'
      Origin = '"CUSTOMER.DB".LastInvoiceDate'
    end
  end
  object OrdersByDateQuery: TIBQuery
    Database = Database
    Transaction = Transaction
    SQL.Strings = (
      'SELECT * FROM ORDERS'
      'WHERE'
      '  (SALEDATE >= :FromDate) AND'
      '  (SALEDATE <= :ToDate)'
      'ORDER BY'
      '  SALEDATE')
    Left = 240
    Top = 256
    ParamData = <
      item
        DataType = ftDateTime
        Name = 'FromDate'
        ParamType = ptUnknown
        Value = 34700d
      end
      item
        DataType = ftDateTime
        Name = 'ToDate'
        ParamType = ptUnknown
        Value = 35488d
      end>
    object OrdersByDateQueryOrderNo: TFloatField
      FieldName = 'ORDERNO'
      Origin = '"ORDERS.DB".OrderNo'
    end
    object OrdersByDateQueryCustNo: TFloatField
      FieldName = 'CUSTNO'
      Origin = '"ORDERS.DB".CustNo'
    end
    object OrdersByDateQuerySaleDate: TDateTimeField
      FieldName = 'SALEDATE'
      Origin = '"ORDERS.DB".SaleDate'
    end
    object OrdersByDateQueryShipDate: TDateTimeField
      FieldName = 'SHIPDATE'
      Origin = '"ORDERS.DB".ShipDate'
    end
    object OrdersByDateQueryEmpNo: TIntegerField
      FieldName = 'EMPNO'
      Origin = '"ORDERS.DB".EmpNo'
    end
    object OrdersByDateQueryShipToContact: TIBStringField
      FieldName = 'SHIPTOCONTACT'
      Origin = '"ORDERS.DB".ShipToContact'
    end
    object OrdersByDateQueryShipToAddr1: TIBStringField
      FieldName = 'SHIPTOADDR1'
      Origin = '"ORDERS.DB".ShipToAddr1'
      Size = 30
    end
    object OrdersByDateQueryShipToAddr2: TIBStringField
      FieldName = 'SHIPTOADDR2'
      Origin = '"ORDERS.DB".ShipToAddr2'
      Size = 30
    end
    object OrdersByDateQueryShipToCity: TIBStringField
      FieldName = 'SHIPTOCITY'
      Origin = '"ORDERS.DB".ShipToCity'
      Size = 15
    end
    object OrdersByDateQueryShipToState: TIBStringField
      FieldName = 'SHIPTOSTATE'
      Origin = '"ORDERS.DB".ShipToState'
    end
    object OrdersByDateQueryShipToZip: TIBStringField
      FieldName = 'SHIPTOZIP'
      Origin = '"ORDERS.DB".ShipToZip'
      Size = 10
    end
    object OrdersByDateQueryShipToCountry: TIBStringField
      FieldName = 'SHIPTOCOUNTRY'
      Origin = '"ORDERS.DB".ShipToCountry'
    end
    object OrdersByDateQueryShipToPhone: TIBStringField
      FieldName = 'SHIPTOPHONE'
      Origin = '"ORDERS.DB".ShipToPhone'
      Size = 15
    end
    object OrdersByDateQueryShipVIA: TIBStringField
      FieldName = 'SHIPVIA'
      Origin = '"ORDERS.DB".ShipVIA'
      Size = 7
    end
    object OrdersByDateQueryPO: TIBStringField
      FieldName = 'PO'
      Origin = '"ORDERS.DB".PO'
      Size = 15
    end
    object OrdersByDateQueryTerms: TIBStringField
      FieldName = 'TERMS'
      Origin = '"ORDERS.DB".Terms'
      Size = 6
    end
    object OrdersByDateQueryPaymentMethod: TIBStringField
      FieldName = 'PAYMENTMETHOD'
      Origin = '"ORDERS.DB".PaymentMethod'
      Size = 7
    end
    object OrdersByDateQueryItemsTotal: TCurrencyField
      FieldName = 'ITEMSTOTAL'
      Origin = '"ORDERS.DB".ItemsTotal'
    end
    object OrdersByDateQueryTaxRate: TFloatField
      FieldName = 'TAXRATE'
      Origin = '"ORDERS.DB".TaxRate'
    end
    object OrdersByDateQueryFreight: TCurrencyField
      FieldName = 'FREIGHT'
      Origin = '"ORDERS.DB".Freight'
    end
    object OrdersByDateQueryAmountPaid: TCurrencyField
      FieldName = 'AMOUNTPAID'
      Origin = '"ORDERS.DB".AmountPaid'
    end
    object OrdersByDateQueryCompany: TIBStringField
      FieldKind = fkLookup
      FieldName = 'COMPANY'
      LookupDataSet = Cust
      LookupKeyFields = 'CustNo'
      LookupResultField = 'Company'
      KeyFields = 'CustNo'
      Size = 40
      Lookup = True
    end
  end
  object Transaction: TIBTransaction
    DefaultDatabase = Database
    Left = 312
    Top = 208
  end
  object OpenDialog: TOpenDialog
    Filter = 'Interbase Files (*.gdb)|*.gdb|All Files (*.*)|*.*'
    Left = 157
    Top = 208
  end
  object OrdersSource: TDataSource
    DataSet = Orders
    Left = 80
    Top = 16
  end
  object CustByOrdSrc: TDataSource
    DataSet = CustByOrd
    Left = 80
    Top = 64
  end
  object ItemsSource: TDataSource
    DataSet = Items
    Left = 56
    Top = 112
  end
  object PartsSource: TDataSource
    DataSet = Parts
    Left = 72
    Top = 160
  end
  object EmpsSource: TDataSource
    DataSet = Emps
    Left = 72
    Top = 216
  end
  object VendorSource: TDataSource
    DataSet = Vendors
    Left = 72
    Top = 272
  end
  object CustSource: TDataSource
    DataSet = Cust
    Left = 304
    Top = 16
  end
  object CustMasterSrc: TDataSource
    DataSet = Cust
    Left = 312
    Top = 72
  end
  object OrdByCustSrc: TDataSource
    DataSet = OrdByCust
    Left = 312
    Top = 128
  end
  object CustByCompSrc: TDataSource
    DataSet = CustByComp
    Left = 352
    Top = 176
  end
end
