inherited StringsEditDlg: TStringsEditDlg
  HelpContext = 26000
  ActiveControl = Memo
  ClientHeight = 303
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  ExplicitHeight = 342
  DesignSize = (
    429
    303)
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelBottom: TPanel
    Top = 254
    ExplicitTop = 254
    inherited CancelButton: TButton
      Left = 259
      ExplicitLeft = 259
    end
    inherited HelpButton: TButton
      Left = 342
      ExplicitLeft = 342
    end
    inherited OKButton: TButton
      Left = 176
      ExplicitLeft = 176
    end
  end
  object GroupBox1: TGroupBox [1]
    Left = 8
    Top = 8
    Width = 423
    Height = 239
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    DesignSize = (
      423
      239)
    object LineCount: TLabel
      Left = 6
      Top = 1
      Width = 169
      Height = 17
      AutoSize = False
      Caption = '0 lines'
    end
    object Memo: TRichEdit
      Left = 3
      Top = 20
      Width = 406
      Height = 216
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HideScrollBars = False
      ParentFont = False
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      Zoom = 100
      OnChange = UpdateStatus
      OnKeyDown = Memo1KeyDown
    end
  end
end
