object StrEditDlg: TStrEditDlg
  Left = 226
  Top = 123
  BorderIcons = [biSystemMenu]
  Caption = 'String List Editor'
  ClientHeight = 279
  ClientWidth = 429
  Color = clBtnFace
  Constraints.MinHeight = 306
  Constraints.MinWidth = 437
  OldCreateOrder = True
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  PopupMenu = StringEditorMenu
  PopupMode = pmAuto
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 230
    Width = 429
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      429
      49)
    object CancelButton: TButton
      Left = 257
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object CodeWndBtn: TButton
      Left = 12
      Top = 12
      Width = 100
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Code Editor...'
      ModalResult = 6
      TabOrder = 1
      OnClick = CodeWndBtnClick
    end
    object HelpButton: TButton
      Left = 339
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Help'
      TabOrder = 2
      OnClick = HelpButtonClick
    end
    object OKButton: TButton
      Left = 175
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
    end
  end
  object OpenDialog: TOpenDialog
    HelpContext = 27040
    DefaultExt = 'TXT'
    Filter = 
      'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|' +
      'Batch files (*.BAT)|*.BAT|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Load string list'
    Left = 376
  end
  object SaveDialog: TSaveDialog
    HelpContext = 27050
    DefaultExt = 'TXT'
    Filter = 
      'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|' +
      'Batch files (*.BAT)|*.BAT|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist, ofEnableSizing]
    Title = 'Save string list'
    Left = 404
  end
  object StringEditorMenu: TPopupActionBar
    Left = 344
    object LoadItem: TMenuItem
      Caption = '&Load...'
      OnClick = FileOpen
    end
    object SaveItem: TMenuItem
      Caption = '&Save...'
      OnClick = FileSave
    end
    object CodeEditorItem: TMenuItem
      Caption = '&Code Editor...'
      Visible = False
      OnClick = CodeWndBtnClick
    end
  end
end
