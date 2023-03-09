object MaskForm: TMaskForm
  Left = 245
  Top = 153
  BorderStyle = bsDialog
  Caption = 'Input Mask Editor'
  ClientHeight = 281
  ClientWidth = 488
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnBottom: TPanel
    Left = 0
    Top = 232
    Width = 488
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    ExplicitTop = 162
    object CancelButton: TButton
      Left = 305
      Top = 12
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object HelpButton: TButton
      Left = 388
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
    object Masks: TButton
      Left = 8
      Top = 12
      Width = 75
      Height = 25
      Caption = '&Masks...'
      TabOrder = 0
      OnClick = MasksClick
    end
    object OKButton: TButton
      Left = 223
      Top = 12
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 488
    Height = 232
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    ExplicitHeight = 162
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 57
      Height = 13
      Caption = '&Input Mask:'
      FocusControl = InputMask
      IsControl = True
    end
    object Label2: TLabel
      Left = 208
      Top = 8
      Width = 70
      Height = 13
      Caption = '&Sample Masks:'
      FocusControl = ListBox1
      IsControl = True
    end
    object Label3: TLabel
      Left = 8
      Top = 167
      Width = 54
      Height = 13
      Caption = '&Test Input:'
      FocusControl = TestEdit
      IsControl = True
    end
    object Label4: TLabel
      Left = 8
      Top = 55
      Width = 102
      Height = 13
      Caption = 'Character for &Blanks:'
      FocusControl = Blanks
      IsControl = True
    end
    object Blanks: TEdit
      Left = 8
      Top = 74
      Width = 33
      Height = 21
      MaxLength = 1
      TabOrder = 1
      Text = '_'
      OnChange = BlanksChange
      IsControl = True
    end
    object InputMask: TEdit
      Left = 8
      Top = 27
      Width = 192
      Height = 21
      TabOrder = 0
      Text = 'InputMask'
      OnChange = InputMaskChange
      IsControl = True
    end
    object ListBox1: TListBox
      Left = 207
      Top = 27
      Width = 256
      Height = 180
      Style = lbOwnerDrawFixed
      ItemHeight = 20
      Items.Strings = (
        'Phone | 4155551212 | !\(999\)000-0000;1;_'
        'Extension | 15450 | !99999;1;_'
        'Social Security | 555555555 | 000\-00\-0000;1;_'
        'Short Zip Code | 90504 | 00000;1;_'
        'Long Zip Code | 905040000 | 00000\-9999;1;_'
        'Date | 062794 | !99/99/00;1;_'
        'Long Time | 090515PM | !90:00:00>LL;1;_'
        'Short Time | 1345 | !90:00;1;_')
      TabOrder = 4
      OnClick = ListBoxSelect
      OnDrawItem = ListDrawItem
      IsControl = True
    end
    object SaveMaskCheck: TCheckBox
      Left = 8
      Top = 104
      Width = 177
      Height = 20
      Caption = 'Save &Literal Characters'
      TabOrder = 2
      OnClick = BlanksChange
      IsControl = True
    end
    object TestEdit: TMaskEdit
      Left = 8
      Top = 186
      Width = 192
      Height = 21
      TabOrder = 3
      Text = ''
      IsControl = True
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dem'
    Filter = 'Edit Masks (*.dem)|*.dem|All Files (*.*)|*.*'
    Title = 'Open Mask File'
    Left = 296
    Top = 120
  end
end
