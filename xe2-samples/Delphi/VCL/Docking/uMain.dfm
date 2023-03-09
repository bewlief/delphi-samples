object MainForm: TMainForm
  Left = 454
  Top = 257
  Caption = 'Docking Demo'
  ClientHeight = 440
  ClientWidth = 667
  Color = clWindow
  ParentFont = True
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object VSplitter: TSplitter
    Left = 0
    Top = 48
    Width = 4
    Height = 388
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Visible = False
  end
  object HSplitter: TSplitter
    Left = 0
    Top = 436
    Width = 667
    Height = 4
    Cursor = crVSplit
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    Visible = False
  end
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 667
    Height = 48
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    AutoSize = True
    BandMaximize = bmDblClick
    Bands = <
      item
        Break = False
        Control = ToolBar1
        ImageIndex = -1
        MinHeight = 21
        Width = 661
      end
      item
        Control = ToolBar2
        ImageIndex = -1
        MinHeight = 21
        Width = 661
      end>
    DockSite = True
    OnDockOver = CoolBar1DockOver
    object ToolBar1: TToolBar
      Left = 11
      Top = 0
      Width = 280
      Height = 21
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 52
      Caption = 'ToolBar1'
      Constraints.MaxWidth = 280
      DragKind = dkDock
      DragMode = dmAutomatic
      ShowCaptions = True
      TabOrder = 0
      Transparent = True
      Wrapable = False
      object ToolButton13: TToolButton
        Left = 0
        Top = 0
        Action = ExitAction
      end
      object ToolButton16: TToolButton
        Left = 52
        Top = 0
        Width = 14
        Caption = 'ToolButton16'
        ImageIndex = 7
        Style = tbsSeparator
      end
      object btnToolBar1: TToolButton
        Left = 66
        Top = 0
        Action = ViewToolBar1
        Style = tbsCheck
      end
      object btnToolBar2: TToolButton
        Left = 118
        Top = 0
        Action = ViewToolBar2
        Style = tbsCheck
      end
    end
    object ToolBar2: TToolBar
      Left = 11
      Top = 23
      Width = 390
      Height = 21
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 37
      Caption = 'ToolBar2'
      Constraints.MaxWidth = 390
      DragKind = dkDock
      DragMode = dmAutomatic
      ShowCaptions = True
      TabOrder = 1
      Transparent = True
      Wrapable = False
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = ViewWhiteWindow
      end
      object ToolButton2: TToolButton
        Left = 37
        Top = 0
        Action = ViewBlueWindow
      end
      object ToolButton3: TToolButton
        Left = 74
        Top = 0
        Action = ViewGreenWindow
      end
      object ToolButton5: TToolButton
        Left = 111
        Top = 0
        Action = ViewLimeWindow
      end
      object ToolButton6: TToolButton
        Left = 148
        Top = 0
        Action = ViewPurpleWindow
      end
      object ToolButton7: TToolButton
        Left = 185
        Top = 0
        Action = ViewRedWindow
      end
      object ToolButton4: TToolButton
        Left = 222
        Top = 0
        Action = ViewTealWindow
      end
    end
  end
  object LeftDockPanel: TPanel
    Left = 4
    Top = 48
    Width = 0
    Height = 388
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alLeft
    BevelOuter = bvNone
    DockSite = True
    TabOrder = 1
    OnDockDrop = LeftDockPanelDockDrop
    OnDockOver = LeftDockPanelDockOver
    OnGetSiteInfo = LeftDockPanelGetSiteInfo
    OnUnDock = LeftDockPanelUnDock
  end
  object BottomDockPanel: TPanel
    Left = 0
    Top = 436
    Width = 667
    Height = 0
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    BevelOuter = bvNone
    DockSite = True
    TabOrder = 2
    OnDockDrop = LeftDockPanelDockDrop
    OnDockOver = BottomDockPanelDockOver
    OnGetSiteInfo = LeftDockPanelGetSiteInfo
    OnUnDock = LeftDockPanelUnDock
  end
  object ActionList1: TActionList
    Left = 136
    Top = 80
    object ViewToolBar1: TAction
      Category = 'ViewToolBars'
      Caption = 'ToolBar &1'
      Checked = True
      ImageIndex = 1
      OnExecute = ViewToolBar1Execute
    end
    object ViewToolBar2: TAction
      Category = 'ViewToolBars'
      Caption = 'ToolBar &2'
      Checked = True
      ImageIndex = 2
      OnExecute = ViewToolBar2Execute
    end
    object ViewWhiteWindow: TAction
      Category = 'ViewWindows'
      Caption = '&White'
      Hint = 'View white window'
      OnExecute = ViewWhiteWindowExecute
    end
    object ExitAction: TAction
      Caption = 'E&xit'
      OnExecute = ExitActionExecute
    end
    object ViewBlueWindow: TAction
      Tag = 1
      Category = 'ViewWindows'
      Caption = '&Blue'
      OnExecute = ViewWhiteWindowExecute
    end
    object ViewGreenWindow: TAction
      Tag = 2
      Category = 'ViewWindows'
      Caption = '&Green'
      OnExecute = ViewWhiteWindowExecute
    end
    object ViewRedWindow: TAction
      Tag = 3
      Category = 'ViewWindows'
      Caption = '&Red'
      OnExecute = ViewWhiteWindowExecute
    end
    object ViewTealWindow: TAction
      Tag = 4
      Category = 'ViewWindows'
      Caption = '&Teal'
      OnExecute = ViewWhiteWindowExecute
    end
    object ViewPurpleWindow: TAction
      Tag = 5
      Category = 'ViewWindows'
      Caption = '&Purple'
      OnExecute = ViewWhiteWindowExecute
    end
    object ViewLimeWindow: TAction
      Tag = 6
      Category = 'ViewWindows'
      Caption = '&Lime'
      OnExecute = ViewWhiteWindowExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 176
    Top = 80
    object File2: TMenuItem
      Caption = '&File'
      object Exit2: TMenuItem
        Action = ExitAction
      end
    end
    object View2: TMenuItem
      Caption = '&View'
      object ToolBar11: TMenuItem
        Action = ViewToolBar1
      end
      object ToolBar21: TMenuItem
        Action = ViewToolBar2
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Floatonclosedocked1: TMenuItem
        Caption = 'Float on close docked'
        OnClick = Floatonclosedocked1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object White1: TMenuItem
        Action = ViewWhiteWindow
      end
      object Blue1: TMenuItem
        Action = ViewBlueWindow
      end
      object Green1: TMenuItem
        Action = ViewGreenWindow
      end
      object Lime1: TMenuItem
        Action = ViewLimeWindow
      end
      object Purple1: TMenuItem
        Action = ViewPurpleWindow
      end
      object Red1: TMenuItem
        Action = ViewRedWindow
      end
      object Teal1: TMenuItem
        Action = ViewTealWindow
      end
    end
  end
end
