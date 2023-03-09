object AddFilesFrame: TAddFilesFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  DesignSize = (
    320
    240)
  object Label1: TLabel
    Left = 3
    Top = 3
    Width = 128
    Height = 13
    Caption = 'Files to add to the project:'
  end
  object Button1: TButton
    Left = 242
    Top = 185
    Width = 75
    Height = 25
    Action = FileOpen1
    Caption = '&Browse...'
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 3
    Top = 22
    Width = 314
    Height = 157
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = Memo1Change
  end
  object ActionList1: TActionList
    Left = 40
    Top = 176
    object FileOpen1: TFileOpen
      Caption = '&Open...'
      Dialog.Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
      Hint = 'Open|Opens an existing file'
      ImageIndex = 7
      ShortCut = 16463
      OnAccept = FileOpen1Accept
    end
  end
end
