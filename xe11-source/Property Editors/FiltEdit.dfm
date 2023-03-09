object FilterEditor: TFilterEditor
  Left = 248
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Filter Editor'
  ClientHeight = 221
  ClientWidth = 367
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  PixelsPerInch = 96
  TextHeight = 13
  object pnBottom: TPanel
    Left = 0
    Top = 172
    Width = 367
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    object CancelButton: TButton
      Left = 202
      Top = 12
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 284
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 2
      OnClick = HelpBtnClick
    end
    object OKButton: TButton
      Left = 120
      Top = 12
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 367
    Height = 172
    Align = alClient
    BevelOuter = bvNone
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
  end
end
