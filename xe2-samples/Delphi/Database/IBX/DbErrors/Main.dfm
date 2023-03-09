object FmMain: TFmMain
  Left = 263
  Top = 191
  BorderStyle = bsSingle
  Caption = 'Database Errors Demo'
  ClientHeight = 351
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 32
    Width = 74
    Height = 16
    Caption = 'Customers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 121
    Width = 48
    Height = 16
    Caption = 'Orders'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 209
    Width = 38
    Height = 16
    Caption = 'Items'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object DBNavigator1: TDBNavigator
    Left = 5
    Top = 4
    Width = 240
    Height = 25
    TabOrder = 0
  end
  object GridCustomers: TDBGrid
    Left = 8
    Top = 47
    Width = 342
    Height = 73
    DataSource = DM.CustomerSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnEnter = GridCustomersEnter
    OnExit = GridCustomersExit
    Columns = <
      item
        Expanded = False
        FieldName = 'CustNo'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Company'
        Visible = True
      end>
  end
  object GridOrders: TDBGrid
    Left = 8
    Top = 136
    Width = 341
    Height = 75
    DataSource = DM.OrdersSource
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnEnter = GridOrdersEnter
    Columns = <
      item
        Expanded = False
        FieldName = 'OrderNo'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'CustNo'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'SaleDate'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ShipDate'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'EmpNo'
        Width = 47
        Visible = True
      end>
  end
  object GridItems: TDBGrid
    Left = 8
    Top = 224
    Width = 341
    Height = 120
    DataSource = DM.ItemsSource
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnEnter = GridItemsEnter
  end
  object MainMenu1: TMainMenu
    Left = 296
    object About1: TMenuItem
      Caption = '&About'
      OnClick = About1Click
    end
  end
end
