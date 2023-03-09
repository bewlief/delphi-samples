object PictureEditorDlg: TPictureEditorDlg
  Left = 232
  Top = 143
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Picture Editor'
  ClientHeight = 294
  ClientWidth = 344
  Color = clBtnFace
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 257
    Top = 21
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 257
    Top = 54
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object HelpButton: TButton
    Left = 257
    Top = 87
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 2
    OnClick = HelpButtonClick
  end
  object GroupBox1: TGroupBox
    Left = -1
    Top = 0
    Width = 256
    Height = 305
    TabOrder = 3
    object ImagePanel: TPanel
      Left = 8
      Top = 16
      Width = 246
      Height = 235
      BevelOuter = bvNone
      BorderWidth = 5
      Color = clWindow
      TabOrder = 0
      object Shape1: TShape
        Left = 5
        Top = 5
        Width = 236
        Height = 225
        Align = alClient
        Brush.Style = bsClear
        Pen.Style = psDot
        ExplicitWidth = 250
      end
      object ImagePaintBox: TPaintBox
        Left = 5
        Top = 5
        Width = 236
        Height = 225
        Align = alClient
        OnPaint = ImagePaintBoxPaint
        ExplicitLeft = 45
        ExplicitTop = 10
        ExplicitWidth = 240
      end
    end
    object Load: TButton
      Left = 10
      Top = 259
      Width = 75
      Height = 23
      Caption = '&Load...'
      TabOrder = 1
      OnClick = LoadClick
    end
    object Save: TButton
      Left = 93
      Top = 259
      Width = 75
      Height = 23
      Caption = '&Save...'
      TabOrder = 2
      OnClick = SaveClick
    end
    object Clear: TButton
      Left = 176
      Top = 259
      Width = 75
      Height = 23
      Caption = '&Clear'
      TabOrder = 3
      OnClick = ClearClick
    end
  end
  object OpenDialog: TOpenPictureDialog
    Filter = 
      'All (*.bmp;*.ico;*.emf;*.wmf)|*.bmp;*.ico;*.emf;*.wmf|Bitmaps (*' +
      '.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf' +
      '|Metafiles (*.wmf)|*.wmf'
    Left = 140
    Top = 20
  end
  object SaveDialog: TSavePictureDialog
    Filter = 
      'All (*.bmp;*.ico;*.emf;*.wmf)|*.bmp;*.ico;*.emf;*.wmf|Bitmaps (*' +
      '.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf' +
      '|Metafiles (*.wmf)|*.wmf'
    Options = [ofOverwritePrompt, ofEnableSizing]
    Left = 140
    Top = 52
  end
end
