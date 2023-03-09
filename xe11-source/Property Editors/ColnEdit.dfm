inherited CollectionEditor: TCollectionEditor
  Left = 212
  Top = 178
  HelpContext = 26150
  HorzScrollBar.Increment = 10
  VertScrollBar.Increment = 11
  BorderIcons = [biSystemMenu]
  Caption = 'CollectionEditor'
  ClientHeight = 122
  ClientWidth = 120
  KeyPreview = True
  PopupMode = pmExplicit
  OnBeforeMonitorDpiChanged = FormBeforeMonitorDpiChanged
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  OnShow = FormShow
  ExplicitWidth = 136
  ExplicitHeight = 161
  PixelsPerInch = 96
  TextHeight = 15
  inherited Splitter1: TSplitter
    Width = 120
    ExplicitWidth = 117
  end
  inherited ToolBar1: TToolBar
    Width = 120
    Images = ImageList1
    TabOrder = 1
    ExplicitWidth = 120
    object ToolButton1: TToolButton
      Left = 4
      Top = 0
      Action = AddCmd
    end
    object ToolButton2: TToolButton
      Left = 27
      Top = 0
      Action = DeleteCmd
    end
    object ToolButton3: TToolButton
      Left = 50
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      ImageName = 'Collections\MoveUp'
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 58
      Top = 0
      Action = MoveUpCmd
    end
    object ToolButton5: TToolButton
      Left = 81
      Top = 0
      Action = MoveDownCmd
    end
  end
  object Panel3: TPanel [2]
    Left = 0
    Top = 38
    Width = 120
    Height = 84
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 120
      Height = 84
      Align = alClient
      Columns = <>
      ColumnClick = False
      DragMode = dmAutomatic
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = ListView1Change
      OnClick = ListView1Click
      OnDragDrop = ListView1DragDrop
      OnDragOver = ListView1DragOver
      OnKeyDown = ListView1KeyDown
      OnKeyPress = ListView1KeyPress
    end
  end
  inherited PopupMenu1: TPopupActionBar
    Images = ImageList1
    object N3: TMenuItem [0]
      Action = AddCmd
    end
    object N4: TMenuItem [1]
      Action = DeleteCmd
    end
    object N5: TMenuItem [2]
      Action = MoveUpCmd
    end
    object N6: TMenuItem [3]
      Action = MoveDownCmd
    end
    object N7: TMenuItem [4]
      Action = SelectAllCmd
    end
    object N2: TMenuItem [5]
      Caption = '-'
    end
  end
  inherited ActionList1: TActionList
    Images = ImageList1
    object AddCmd: TAction
      Caption = '&Add'
      Hint = 'Add New'
      ImageIndex = 0
      ImageName = 'Collections\Add'
      ShortCut = 45
      OnExecute = AddClick
    end
    object DeleteCmd: TAction
      Caption = '&Delete'
      Enabled = False
      Hint = 'Delete Selected'
      ImageIndex = 1
      ImageName = 'Collections\Delete'
      ShortCut = 46
      OnExecute = DeleteClick
      OnUpdate = SelectionUpdate
    end
    object MoveUpCmd: TAction
      Caption = 'Move &Up'
      Enabled = False
      Hint = 'Move Selected Up'
      ImageIndex = 2
      ImageName = 'Collections\MoveUp'
      ShortCut = 16422
      OnExecute = MoveUpClick
      OnUpdate = SelectionUpdate
    end
    object MoveDownCmd: TAction
      Caption = 'Move Dow&n'
      Enabled = False
      Hint = 'Move Selected Down'
      ImageIndex = 3
      ImageName = 'Collections\MoveDown'
      ShortCut = 16424
      OnExecute = MoveDownClick
      OnUpdate = SelectionUpdate
    end
    object SelectAllCmd: TAction
      Caption = '&Select All'
      Enabled = False
      OnExecute = SelectAll1Click
      OnUpdate = SelectAllCommandUpdate
    end
  end
  object ImageList1: TVirtualImageList
    Images = <
      item
        CollectionIndex = 402
        CollectionName = 'Collections\Add'
        Name = 'Collections\Add'
      end
      item
        CollectionIndex = 411
        CollectionName = 'Collections\Delete'
        Name = 'Collections\Delete'
      end
      item
        CollectionIndex = 412
        CollectionName = 'Collections\MoveUp'
        Name = 'Collections\MoveUp'
      end
      item
        CollectionIndex = 414
        CollectionName = 'Collections\MoveDown'
        Name = 'Collections\MoveDown'
      end>
    ImageCollection = IDEImageResourcesFrm.IDEImageCollection
    Left = 8
    Top = 40
  end
end
