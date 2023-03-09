object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Sample Proxy Generator'
  ClientHeight = 807
  ClientWidth = 707
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    707
    807)
  PixelsPerInch = 120
  TextHeight = 17
  object GroupBox1: TGroupBox
    Left = 10
    Top = 10
    Width = 337
    Height = 202
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Connection'
    TabOrder = 0
    DesignSize = (
      337
      202)
    object Label6: TLabel
      Left = 12
      Top = 22
      Width = 28
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Host'
    end
    object Label7: TLabel
      Left = 12
      Top = 58
      Width = 51
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Protocol'
    end
    object Label8: TLabel
      Left = 12
      Top = 93
      Width = 26
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Port'
    end
    object Label9: TLabel
      Left = 9
      Top = 128
      Width = 53
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'URLPath'
    end
    object EditHost: TEdit
      Left = 89
      Top = 18
      Width = 158
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
      Text = 'localhost'
    end
    object ComboBoxProtocol: TComboBox
      Left = 89
      Top = 54
      Width = 95
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        'tcp/ip'
        'http')
    end
    object EditPort: TEdit
      Left = 89
      Top = 89
      Width = 95
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
      Text = '211'
    end
    object EditURLPath: TEdit
      Left = 89
      Top = 124
      Width = 238
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 354
    Top = 10
    Width = 343
    Height = 202
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Code'
    TabOrder = 1
    DesignSize = (
      343
      202)
    object Label1: TLabel
      Left = 21
      Top = 22
      Width = 95
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Exclude Classes'
    end
    object Label2: TLabel
      Left = 21
      Top = 58
      Width = 104
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Exclude Methods'
    end
    object Label3: TLabel
      Left = 21
      Top = 165
      Width = 63
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Unit Name'
    end
    object Label4: TLabel
      Left = 21
      Top = 128
      Width = 100
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Include Methods'
    end
    object Label5: TLabel
      Left = 21
      Top = 93
      Width = 91
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Include Classes'
    end
    object EditExcludeClasses: TEdit
      Left = 143
      Top = 18
      Width = 185
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object EditExcludeMethods: TEdit
      Left = 144
      Top = 54
      Width = 184
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object EditUnitName: TEdit
      Left = 144
      Top = 161
      Width = 184
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 'Unit1'
    end
    object EditIncludeMethods: TEdit
      Left = 144
      Top = 124
      Width = 184
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object EditIncludeClasses: TEdit
      Left = 143
      Top = 89
      Width = 185
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
  end
  object GroupBox3: TGroupBox
    Left = 10
    Top = 255
    Width = 686
    Height = 146
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Output'
    TabOrder = 2
    DesignSize = (
      686
      146)
    object Directory: TLabel
      Left = 42
      Top = 63
      Width = 57
      Height = 17
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Directory'
    end
    object CheckBoxWriteToFile: TCheckBox
      Left = 21
      Top = 31
      Width = 106
      Height = 23
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Write to File'
      TabOrder = 0
    end
    object EditOutputDirectory: TEdit
      Left = 114
      Top = 61
      Width = 537
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object CheckBoxWriteToMemo: TCheckBox
      Left = 21
      Top = 106
      Width = 127
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Write to Memo'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object ButtonGenerateProxy: TButton
    Left = 9
    Top = 409
    Width = 99
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Generate'
    TabOrder = 3
    OnClick = ButtonGenerateProxyClick
  end
  object MemoCode: TMemo
    Left = 11
    Top = 450
    Width = 687
    Height = 335
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object GroupBox4: TGroupBox
    Left = 10
    Top = 201
    Width = 687
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Language'
    TabOrder = 5
    object ComboBoxLanguages: TComboBox
      Left = 89
      Top = 20
      Width = 238
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      TabOrder = 0
    end
  end
  object SQLConnection1: TSQLConnection
    DriverName = 'Datasnap'
    Params.Strings = (
      'DriverUnit=DbxDataSnap'
      'Filters={}')
    Left = 336
    Top = 184
  end
end
