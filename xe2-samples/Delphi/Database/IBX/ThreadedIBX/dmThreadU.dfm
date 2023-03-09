object dmThread: TdmThread
  OldCreateOrder = False
  Height = 479
  Width = 741
  object IBDatabase1: TIBDatabase
    DatabaseName = 
      'C:\Users\Public\Documents\RAD Studio\9.0\Samples\Data\EMPLOYEE.G' +
      'DB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = IBTransaction1
    ServerType = 'IBServer'
    Left = 52
    Top = 84
  end
  object IBTransaction1: TIBTransaction
    DefaultDatabase = IBDatabase1
    Left = 124
    Top = 84
  end
  object IBQuery1: TIBQuery
    Database = IBDatabase1
    Transaction = IBTransaction1
    BufferChunks = 100
    Left = 192
    Top = 84
  end
end
