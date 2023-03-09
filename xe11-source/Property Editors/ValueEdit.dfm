inherited ValueEditDlg: TValueEditDlg
  HelpContext = 26005
  ActiveControl = ValueListEditor1
  Caption = 'Value List Editor'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelBottom: TPanel
    inherited CancelButton: TButton
      Left = 260
      ExplicitLeft = 260
    end
    inherited HelpButton: TButton
      Left = 343
      ExplicitLeft = 343
    end
    inherited OKButton: TButton
      Left = 177
      ExplicitLeft = 177
    end
  end
  object ValueListEditor1: TValueListEditor [1]
    Left = 12
    Top = 7
    Width = 405
    Height = 211
    Anchors = [akLeft, akTop, akRight, akBottom]
    KeyOptions = [keyEdit, keyAdd, keyDelete]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
    TabOrder = 1
    OnKeyDown = ValueListEditor1KeyDown
    OnStringsChange = ValueListEditor1StringsChange
    ColWidths = (
      150
      249)
  end
end
