inherited DataBindingListDesigner: TDataBindingListDesigner
  HelpContext = 16201
  HorzScrollBar.Increment = 19
  VertScrollBar.Increment = 16
  BorderIcons = [biSystemMenu]
  Caption = 'DataBindingListDesigner'
  ClientHeight = 326
  ClientWidth = 555
  KeyPreview = True
  PopupMode = pmExplicit
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  ExplicitWidth = 571
  ExplicitHeight = 364
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter1: TSplitter
    Width = 555
    ExplicitWidth = 328
  end
  object Splitter2: TSplitter [1]
    Left = 123
    Top = 33
    Width = 4
    Height = 293
    ResizeStyle = rsUpdate
    ExplicitLeft = 90
    ExplicitHeight = 236
  end
  inherited ToolBar1: TToolBar
    Width = 555
    Images = ImageList1
    TabOrder = 2
    ExplicitWidth = 555
    object ToolButton1: TToolButton
      Left = 4
      Top = 0
      Action = NewBindCompDialog
      DropdownMenu = NewDataBindingsPopup
      Style = tbsDropDown
    end
    object ToolButton2: TToolButton
      Left = 42
      Top = 0
      Action = RemoveBindComp
    end
    object ToolButton5: TToolButton
      Left = 65
      Top = 0
      Width = 8
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 73
      Top = 0
      Action = MoveUp
    end
    object ToolButton4: TToolButton
      Left = 96
      Top = 0
      Action = MoveDown
    end
  end
  object CategoryPanel: TPanel [3]
    Left = 0
    Top = 33
    Width = 123
    Height = 293
    Align = alLeft
    BevelOuter = bvNone
    Caption = 'CategoryPanel'
    TabOrder = 1
    object ListBox1: TListBox
      Left = 0
      Top = 18
      Width = 123
      Height = 275
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 123
      Height = 18
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 3
        Top = 2
        Width = 56
        Height = 13
        Caption = 'Categor&ies:'
        FocusControl = ListBox1
      end
    end
  end
  object ActionPanel: TPanel [4]
    Left = 127
    Top = 33
    Width = 428
    Height = 293
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 428
      Height = 18
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label2: TLabel
        Left = 3
        Top = 2
        Width = 87
        Height = 13
        Caption = '&Bind Components:'
        FocusControl = ListView1
      end
    end
    object ListPanel: TPanel
      Left = 0
      Top = 18
      Width = 428
      Height = 275
      Align = alClient
      BevelOuter = bvNone
      Caption = 'ListPanel'
      DoubleBuffered = False
      ParentColor = True
      ParentDoubleBuffered = False
      TabOrder = 1
      ExplicitTop = 0
      ExplicitHeight = 18
      object ListView1: TListView
        Left = 0
        Top = 0
        Width = 428
        Height = 275
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 100
          end
          item
            Caption = 'Description'
            Width = 200
          end>
        ColumnClick = False
        DragMode = dmAutomatic
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = ListView1Change
        OnClick = ListView1Click
        OnDblClick = ListView1DblClick
        OnEndDrag = ListView1EndDrag
        OnDragDrop = ListView1DragDrop
        OnDragOver = ListView1DragOver
        OnKeyDown = ListView1KeyDown
        OnKeyPress = ListView1KeyPress
        OnResize = ListView1Resize
        OnStartDrag = ListView1StartDrag
        ExplicitWidth = 419
        ExplicitHeight = 245
      end
    end
  end
  inherited PopupMenu1: TPopupActionBar
    Images = ImageList1
    object AddCommon1: TMenuItem [0]
      Action = NewBindCompDialog
    end
    object NewDataBindingPopupMenuItem1: TMenuItem [1]
      Action = NewCommonDataBinding
    end
    object N1: TMenuItem [2]
      Caption = '-'
    end
    object MoveUp1: TMenuItem [3]
      Action = MoveUp
    end
    object MoveDown1: TMenuItem [4]
      Action = MoveDown
    end
    object ChangeRight1: TMenuItem [5]
      Action = RelationshipsAction
    end
    object NewRelationshipAction1: TMenuItem [6]
      Action = NewRelationshipAction
    end
    object N2: TMenuItem [7]
      Caption = '-'
    end
    object CutItem: TMenuItem [8]
      Caption = 'Cu&t'
      ShortCut = 16472
      OnClick = CutItemClick
    end
    object CopyItem: TMenuItem [9]
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = CopyItemClick
    end
    object PasteItem: TMenuItem [10]
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = PasteItemClick
    end
    object DeleteItem: TMenuItem [11]
      Action = RemoveBindComp
    end
    object SelectAllItem: TMenuItem [12]
      Action = SelectAllAction
    end
    object N3: TMenuItem [13]
      Caption = '-'
    end
    object PanelDescriptions1: TMenuItem [14]
      Action = DescriptionsAction
    end
  end
  inherited ActionList1: TActionList
    Images = ImageList1
    Left = 72
    Top = 136
    object NewCommonDataBinding: TAction
      Caption = 'Common'
      ShortCut = 16429
      Visible = False
      OnExecute = NewCommonDataBindingExecute
    end
    object NewBindCompDialog: TAction
      Caption = 'New Binding...'
      Hint = 'New Binding|Create new binding  from list'
      ImageIndex = 0
      ShortCut = 45
      OnExecute = NewBindCompDialogExecute
    end
    object RemoveBindComp: TAction
      Caption = '&Delete'
      Enabled = False
      Hint = 'Delete|Delete selected bindings'
      ImageIndex = 1
      ShortCut = 46
      OnExecute = DeleteClick
      OnUpdate = SelectedUpdate
    end
    object MoveUp: TAction
      Caption = 'Move &Up'
      Hint = 'Move Up|Move selected upwards'
      ImageIndex = 2
      ShortCut = 16422
      OnExecute = MoveUpClick
      OnUpdate = MoveUpUpdate
    end
    object MoveDown: TAction
      Caption = 'Move Dow&n'
      Hint = 'Move Down|Move selected downwards'
      ImageIndex = 3
      ShortCut = 16424
      OnExecute = MoveDownClick
      OnUpdate = MoveDownUpdate
    end
    object DescriptionsAction: TAction
      Caption = 'Panel D&escriptions'
      Checked = True
      Hint = 'Descriptions|Shows/hides panel descriptions'
      ShortCut = 0
      OnExecute = DescriptionsActionExecute
    end
    object SelectAllAction: TAction
      Caption = 'Select &All'
      Hint = 'Select All|Selects all bindings'
      ShortCut = 0
      OnExecute = SelectAllItemClick
      OnUpdate = SelectAllActionUpdate
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      ShortCut = 16451
      OnExecute = CopyItemClick
      OnUpdate = SelectedUpdate
    end
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      ShortCut = 16472
      OnExecute = CutItemClick
      OnUpdate = SelectedUpdate
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      ShortCut = 16470
      OnExecute = PasteItemClick
      OnUpdate = SelectedUpdate
    end
    object BindCompWizard: TAction
      Caption = 'LiveBindings Wizard...'
      Hint = 'LiveBindings Wizard...|Create new binding using a wizard'
      ImageIndex = 0
      ShortCut = 8237
      OnExecute = BindCompWizardExecute
    end
    object RelationshipsAction: TAction
      Caption = 'Change Relationships...'
      ShortCut = 0
      OnExecute = RelationshipsActionExecute
      OnUpdate = RelationshipsActionUpdate
    end
    object NewRelationshipAction: TAction
      Caption = 'New Relationships...'
      ShortCut = 0
      OnExecute = NewRelationshipActionExecute
      OnUpdate = NewRelationshipActionUpdate
    end
  end
  inherited PopupMenu2: TPopupActionBar
    Left = 80
  end
  object ImageList1: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 402
        CollectionName = 'Collections\Add'
        Disabled = False
        Name = 'Collections\Add'
      end
      item
        CollectionIndex = 411
        CollectionName = 'Collections\Delete'
        Disabled = False
        Name = 'Collections\Delete'
      end
      item
        CollectionIndex = 412
        CollectionName = 'Collections\MoveUp'
        Disabled = False
        Name = 'Collections\MoveUp'
      end
      item
        CollectionIndex = 413
        CollectionName = 'Collections\MoveDown'
        Disabled = False
        Name = 'Collections\MoveDown'
      end>
    ImageCollection = IDEImageResourcesFrm.IDEImageCollection
    Left = 16
    Top = 128
  end
  object NewDataBindingsPopup: TPopupMenu
    OnPopup = NewDataBindingsPopupPopup
    Left = 40
    Top = 200
    object NewStandardDataBinding1: TMenuItem
      Action = NewBindCompDialog
    end
    object Wizard1: TMenuItem
      Action = BindCompWizard
    end
    object NewDataBinding1: TMenuItem
      Action = NewCommonDataBinding
    end
  end
end
