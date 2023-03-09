inherited StdDataForm: TStdDataForm
  Caption = 'StdDataForm'
  ClientHeight = 430
  ClientWidth = 611
  OnCreate = FormCreate
  ExplicitWidth = 627
  ExplicitHeight = 466
  PixelsPerInch = 96
  TextHeight = 13
  inherited GDSStdPanel: TPanel
    Width = 611
    TabOrder = 1
  end
  object StdCtrlPanel: TPanel
    Left = 0
    Top = 89
    Width = 611
    Height = 72
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    ExplicitWidth = 460
    object FilterOnRadioGroup: TRadioGroup
      Left = 11
      Top = 7
      Width = 95
      Height = 57
      Caption = 'Filter Field'
      Items.Strings = (
        'Sale &Date'
        'A&mount Due')
      TabOrder = 0
      OnClick = FilterOnRadioGroupClick
    end
    object GroupBox1: TGroupBox
      Left = 119
      Top = 7
      Width = 162
      Height = 57
      Caption = 'Filter/Search Critera'
      TabOrder = 1
      object FilterOnLabel: TLabel
        Left = 8
        Top = 14
        Width = 83
        Height = 13
        Caption = 'Show records >= '
      end
      object FilterCriteria: TEdit
        Left = 6
        Top = 30
        Width = 150
        Height = 21
        TabOrder = 0
        OnExit = FilterCriteriaExit
        OnKeyPress = FilterCriteriaKeyPress
      end
    end
    object FilterCheckBox: TCheckBox
      Left = 314
      Top = 14
      Width = 135
      Height = 17
      Caption = '&Filtered Records Only'
      TabOrder = 2
      OnClick = FilterCheckBoxClick
    end
    object NextBtn: TButton
      Left = 295
      Top = 37
      Width = 70
      Height = 25
      Caption = 'Find &Next'
      TabOrder = 3
      OnClick = NextBtnClick
    end
    object PriorBtn: TButton
      Left = 377
      Top = 37
      Width = 70
      Height = 25
      Caption = 'Find &Prior'
      TabOrder = 4
      OnClick = PriorBtnClick
    end
  end
  object OrdersSource: TDataSource
    DataSet = Orders
    Left = 215
    Top = 278
  end
  object Orders: TIBTable
    Database = Database
    Transaction = IBTransaction1
    OnCalcFields = OrdersCalcFields
    Active = True
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
    ReadOnly = True
    StoreDefs = True
    TableName = 'ORDERS'
    Left = 208
    Top = 224
    object IBTable1ORDERNO: TFloatField
      FieldName = 'ORDERNO'
      Required = True
    end
    object IBTable1CUSTNO: TFloatField
      FieldName = 'CUSTNO'
    end
    object IBTable1SALEDATE: TDateTimeField
      FieldName = 'SALEDATE'
    end
    object IBTable1SHIPDATE: TDateTimeField
      FieldName = 'SHIPDATE'
    end
    object IBTable1EMPNO: TIntegerField
      FieldName = 'EMPNO'
    end
    object IBTable1SHIPTOCONTACT: TIBStringField
      FieldName = 'SHIPTOCONTACT'
    end
    object IBTable1SHIPTOADDR1: TIBStringField
      FieldName = 'SHIPTOADDR1'
      Size = 30
    end
    object IBTable1SHIPTOADDR2: TIBStringField
      FieldName = 'SHIPTOADDR2'
      Size = 30
    end
    object IBTable1SHIPTOCITY: TIBStringField
      FieldName = 'SHIPTOCITY'
      Size = 15
    end
    object IBTable1SHIPTOSTATE: TIBStringField
      FieldName = 'SHIPTOSTATE'
    end
    object IBTable1SHIPTOZIP: TIBStringField
      FieldName = 'SHIPTOZIP'
      Size = 10
    end
    object IBTable1SHIPTOCOUNTRY: TIBStringField
      FieldName = 'SHIPTOCOUNTRY'
    end
    object IBTable1SHIPTOPHONE: TIBStringField
      FieldName = 'SHIPTOPHONE'
      Size = 15
    end
    object IBTable1SHIPVIA: TIBStringField
      FieldName = 'SHIPVIA'
      Size = 7
    end
    object IBTable1PO: TIBStringField
      FieldName = 'PO'
      Size = 15
    end
    object IBTable1TERMS: TIBStringField
      FieldName = 'TERMS'
      Size = 6
    end
    object IBTable1PAYMENTMETHOD: TIBStringField
      FieldName = 'PAYMENTMETHOD'
      Size = 7
    end
    object IBTable1ITEMSTOTAL: TFloatField
      FieldName = 'ITEMSTOTAL'
    end
    object IBTable1TAXRATE: TFloatField
      FieldName = 'TAXRATE'
    end
    object IBTable1FREIGHT: TFloatField
      FieldName = 'FREIGHT'
    end
    object IBTable1AMOUNTPAID: TFloatField
      FieldName = 'AMOUNTPAID'
    end
    object OrdersAmountDue: TFloatField
      FieldKind = fkCalculated
      FieldName = 'AmountDue'
      Calculated = True
    end
    object OrdersTaxAmount: TFloatField
      FieldKind = fkCalculated
      FieldName = 'TaxAmount'
      Calculated = True
    end
    object OrdersCustName: TStringField
      FieldKind = fkLookup
      FieldName = 'CustName'
      LookupDataSet = Cust
      LookupKeyFields = 'CUSTNO'
      LookupResultField = 'COMPANY'
      KeyFields = 'ORDERNO'
      Size = 75
      Lookup = True
    end
  end
  object IBTransaction1: TIBTransaction
    Active = True
    Left = 128
    Top = 232
  end
  object Database: TIBDatabase
    Connected = True
    DatabaseName = 
      'C:\Users\Public\Documents\RAD Studio\9.0\Samples\Data\MASTSQL.GD' +
      'B'
    Params.Strings = (
      'user_name=SYSDBA'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = IBTransaction1
    ServerType = 'IBServer'
    Left = 64
    Top = 232
  end
  object Cust: TIBTable
    Database = Database
    Transaction = IBTransaction1
    Active = True
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
    ReadOnly = True
    StoreDefs = True
    TableName = 'CUSTOMER'
    Left = 264
    Top = 232
    object CustCUSTNO: TFloatField
      FieldName = 'CUSTNO'
      Required = True
    end
    object CustCOMPANY: TIBStringField
      FieldName = 'COMPANY'
      Required = True
      Size = 30
    end
    object CustADDR1: TIBStringField
      FieldName = 'ADDR1'
      Size = 30
    end
    object CustADDR2: TIBStringField
      FieldName = 'ADDR2'
      Size = 30
    end
    object CustCITY: TIBStringField
      FieldName = 'CITY'
      Size = 15
    end
    object CustSTATE: TIBStringField
      FieldName = 'STATE'
    end
    object CustZIP: TIBStringField
      FieldName = 'ZIP'
      Size = 10
    end
    object CustCOUNTRY: TIBStringField
      FieldName = 'COUNTRY'
    end
    object CustPHONE: TIBStringField
      FieldName = 'PHONE'
      Size = 15
    end
    object CustFAX: TIBStringField
      FieldName = 'FAX'
      Size = 15
    end
    object CustTAXRATE: TFloatField
      FieldName = 'TAXRATE'
    end
    object CustCONTACT: TIBStringField
      FieldName = 'CONTACT'
    end
    object CustLASTINVOICEDATE: TDateTimeField
      FieldName = 'LASTINVOICEDATE'
    end
  end
end
