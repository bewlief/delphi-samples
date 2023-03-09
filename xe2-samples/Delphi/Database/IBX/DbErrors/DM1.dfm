object DM: TDM
  OldCreateOrder = True
  Height = 303
  Width = 241
  object CustomerSource: TDataSource
    DataSet = Customer
    Left = 81
    Top = 8
  end
  object OrdersSource: TDataSource
    DataSet = Orders
    Left = 81
    Top = 70
  end
  object ItemsSource: TDataSource
    DataSet = Items
    Left = 81
    Top = 130
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
    Left = 16
    Top = 192
  end
  object IBTransaction1: TIBTransaction
    Active = True
    Left = 80
    Top = 192
  end
  object Customer: TIBTable
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
    StoreDefs = True
    TableName = 'CUSTOMER'
    Left = 16
    Top = 8
    object CustomerCUSTNO: TFloatField
      FieldName = 'CUSTNO'
      Required = True
    end
    object CustomerCOMPANY: TIBStringField
      FieldName = 'COMPANY'
      Required = True
      Size = 30
    end
  end
  object Orders: TIBTable
    Database = Database
    Transaction = IBTransaction1
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
    IndexName = 'RDB$PRIMARY6'
    MasterFields = 'CustNo'
    MasterSource = CustomerSource
    StoreDefs = True
    TableName = 'ORDERS'
    Left = 16
    Top = 72
    object OrdersORDERNO: TFloatField
      FieldName = 'ORDERNO'
      Required = True
    end
    object OrdersCUSTNO: TFloatField
      FieldName = 'CUSTNO'
    end
    object OrdersSALEDATE: TDateTimeField
      FieldName = 'SALEDATE'
    end
    object OrdersSHIPDATE: TDateTimeField
      FieldName = 'SHIPDATE'
    end
    object OrdersEMPNO: TIntegerField
      FieldName = 'EMPNO'
    end
  end
  object Items: TIBTable
    Database = Database
    Transaction = IBTransaction1
    Active = True
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
    MasterFields = 'OrderNo'
    MasterSource = OrdersSource
    StoreDefs = True
    TableName = 'ITEMS'
    Left = 16
    Top = 128
    object ItemsITEMNO: TFloatField
      FieldName = 'ITEMNO'
      Required = True
    end
    object ItemsORDERNO: TFloatField
      FieldName = 'ORDERNO'
      Required = True
    end
    object ItemsPARTNO: TFloatField
      FieldName = 'PARTNO'
    end
    object ItemsQTY: TIntegerField
      FieldName = 'QTY'
    end
    object ItemsDISCOUNT: TFloatField
      FieldName = 'DISCOUNT'
    end
  end
end
