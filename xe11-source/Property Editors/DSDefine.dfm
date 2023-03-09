object DefineField: TDefineField
  Left = 221
  Top = 137
  BorderStyle = bsDialog
  Caption = 'New Field'
  ClientHeight = 473
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 502
    Height = 424
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    ExplicitHeight = 317
    object FieldGroup: TGroupBox
      AlignWithMargins = True
      Left = 12
      Top = 8
      Width = 482
      Height = 127
      Margins.Left = 12
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Field properties'
      TabOrder = 0
      DesignSize = (
        482
        127)
      object ComponentNameLabel: TLabel
        Left = 8
        Top = 65
        Width = 59
        Height = 13
        Caption = 'C&omponent:'
        FocusControl = ComponentNameEdit
      end
      object FieldNameLabel: TLabel
        Left = 8
        Top = 33
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = FieldNameEdit
      end
      object FieldTypeLabel: TLabel
        Left = 8
        Top = 97
        Width = 28
        Height = 13
        Caption = '&Type:'
        FocusControl = FieldTypeList
      end
      object SizeEditLabel: TLabel
        Left = 240
        Top = 97
        Width = 23
        Height = 13
        Caption = '&Size:'
        FocusControl = SizeEdit
      end
      object ComponentNameEdit: TEdit
        Left = 104
        Top = 62
        Width = 370
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object FieldNameEdit: TEdit
        Left = 104
        Top = 30
        Width = 370
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = FieldNameEditChange
        OnClick = FieldNameEditChange
      end
      object FieldTypeList: TComboBox
        Left = 104
        Top = 94
        Width = 130
        Height = 21
        TabOrder = 2
        OnChange = FieldTypeListChange
      end
      object SizeEdit: TEdit
        Left = 346
        Top = 94
        Width = 57
        Height = 21
        MaxLength = 5
        TabOrder = 3
      end
    end
    object FieldKind: TRadioGroup
      AlignWithMargins = True
      Left = 12
      Top = 143
      Width = 482
      Height = 100
      Margins.Left = 12
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Field type'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        '&Data'
        '&Calculated'
        '&Lookup')
      TabOrder = 1
      OnClick = FieldKindClick
      ExplicitTop = 111
    end
    object LookupGroup: TGroupBox
      AlignWithMargins = True
      Left = 12
      Top = 251
      Width = 482
      Height = 161
      Margins.Left = 12
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = 'Lookup definition'
      TabOrder = 2
      DesignSize = (
        482
        161)
      object DatasetLabel: TLabel
        Left = 8
        Top = 65
        Width = 42
        Height = 13
        Caption = 'D&ataset:'
        Enabled = False
        FocusControl = DatasetList
      end
      object KeyFieldsLabel: TLabel
        Left = 8
        Top = 33
        Width = 52
        Height = 13
        Caption = '&Key Fields:'
        Enabled = False
        FocusControl = KeyFieldsList
      end
      object LookupKeysLabel: TLabel
        Left = 8
        Top = 97
        Width = 64
        Height = 13
        Caption = 'Look&up Keys:'
        Enabled = False
        FocusControl = LookupKeysList
      end
      object ResultFieldLabel: TLabel
        Left = 8
        Top = 129
        Width = 59
        Height = 13
        Caption = '&Result Field:'
        Enabled = False
        FocusControl = ResultFieldList
      end
      object DatasetList: TComboBox
        Left = 104
        Top = 62
        Width = 370
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 1
        OnChange = DatasetListChange
        OnDropDown = DatasetListDropDown
      end
      object KeyFieldsList: TComboBox
        Left = 104
        Top = 30
        Width = 370
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 0
        OnDropDown = KeyFieldsListDropDown
      end
      object LookupKeysList: TComboBox
        Left = 104
        Top = 94
        Width = 370
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 2
        OnDropDown = LookupKeysListDropDown
      end
      object ResultFieldList: TComboBox
        Left = 104
        Top = 126
        Width = 370
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 3
        OnDropDown = ResultFieldListDropDown
      end
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 424
    Width = 502
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 317
    object CancelBtn: TButton
      Left = 328
      Top = 12
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object HelpBtn: TButton
      Left = 411
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 1
      OnClick = HelpBtnClick
    end
    object OkBtn: TButton
      Left = 245
      Top = 12
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
      OnClick = OkBtnClick
    end
  end
end
