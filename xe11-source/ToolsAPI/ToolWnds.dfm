object ToolbarDesignWindow: TToolbarDesignWindow
  Left = 338
  Top = 303
  HorzScrollBar.Increment = 31
  VertScrollBar.Increment = 14
  Caption = 'ToolbarForm'
  ClientHeight = 149
  ClientWidth = 342
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  PopupMenu = PopupMenu1
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  object Splitter1: TSplitter
    Left = 0
    Top = 30
    Width = 342
    Height = 8
    Cursor = crSizeNS
    Align = alTop
    ResizeStyle = rsUpdate
    OnCanResize = Splitter1CanResize
    OnMoved = Splitter1Moved
    ExplicitWidth = 350
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 342
    Height = 30
    BorderWidth = 1
    EdgeBorders = [ebTop, ebBottom]
    Indent = 4
    PopupMenu = PopupMenu2
    TabOrder = 0
    Wrapable = False
  end
  object PopupMenu1: TPopupActionBar
    Left = 8
    Top = 72
    object Toolbar2: TMenuItem
      Action = ToolbarCmd
    end
  end
  object ActionList1: TActionList
    Left = 40
    Top = 72
    object ToolbarCmd: TAction
      Caption = '&Toolbar'
      Checked = True
      OnExecute = ToolbarCmdExecute
      OnUpdate = ToolbarCmdUpdate
    end
    object TextLabelsCmd: TAction
      Caption = 'Text &Labels'
      OnExecute = TextLabelsCmdExecute
      OnUpdate = TextLabelsCmdUpdate
    end
    object HelpCmd: TAction
      Caption = 'Help'
      Hint = 'Show help'
      OnExecute = HelpCmdExecute
      OnUpdate = HelpCmdUpdate
    end
  end
  object PopupMenu2: TPopupActionBar
    Left = 72
    Top = 72
    object TextLabels1: TMenuItem
      Action = TextLabelsCmd
    end
  end
end
