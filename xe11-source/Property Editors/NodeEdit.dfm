object TreeViewItems: TTreeViewItems
  Left = 237
  Top = 142
  HelpContext = 26090
  BorderIcons = [biSystemMenu]
  Caption = 'TreeView Items Editor'
  ClientHeight = 276
  ClientWidth = 527
  Constraints.MinHeight = 236
  Constraints.MinWidth = 423
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 527
    Height = 227
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 293
      Height = 211
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 8
      Align = alLeft
      Caption = '&Items'
      TabOrder = 0
      DesignSize = (
        293
        211)
      object New: TButton
        Left = 159
        Top = 32
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&New Item'
        Default = True
        TabOrder = 1
        OnClick = NewClick
      end
      object Delete: TButton
        Left = 159
        Top = 99
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Delete'
        TabOrder = 3
        OnClick = DeleteClick
      end
      object TreeView: TTreeView
        Left = 8
        Top = 32
        Width = 144
        Height = 169
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragMode = dmAutomatic
        HideSelection = False
        Indent = 19
        TabOrder = 0
        OnChange = TreeViewChange
        OnChanging = TreeViewChanging
        OnDragDrop = TreeViewDragDrop
        OnDragOver = TreeViewDragOver
        OnEdited = TreeViewEdited
      end
      object NewSub: TButton
        Left = 159
        Top = 66
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'N&ew SubItem'
        TabOrder = 2
        OnClick = NewSubClick
      end
      object Load: TButton
        Left = 159
        Top = 132
        Width = 125
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Load'
        TabOrder = 4
        OnClick = LoadClick
      end
    end
    object PropGroupBox: TGroupBox
      AlignWithMargins = True
      Left = 305
      Top = 8
      Width = 222
      Height = 211
      Margins.Left = 4
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 8
      Align = alClient
      Caption = 'Item Properties'
      TabOrder = 1
      DesignSize = (
        222
        211)
      object Label1: TLabel
        Left = 8
        Top = 37
        Width = 26
        Height = 13
        Caption = '&Text:'
        FocusControl = TextEdit
      end
      object Label2: TLabel
        Left = 8
        Top = 64
        Width = 65
        Height = 13
        Caption = 'I&mage Index:'
        FocusControl = Image
      end
      object Label3: TLabel
        Left = 8
        Top = 119
        Width = 61
        Height = 13
        Caption = 'State Inde&x:'
        FocusControl = StateImage
      end
      object Label4: TLabel
        Left = 8
        Top = 92
        Width = 76
        Height = 13
        Caption = '&Selected Index:'
        FocusControl = SelectedIndex
      end
      object Label5: TLabel
        Left = 8
        Top = 146
        Width = 83
        Height = 13
        Caption = '&Expanded Index:'
        FocusControl = ExpandedIndex
      end
      object TextEdit: TEdit
        Left = 124
        Top = 34
        Width = 87
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = ValueChange
        OnExit = TextEditExit
      end
      object Image: TEdit
        Left = 124
        Top = 61
        Width = 47
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = ValueChange
        OnExit = ImageExit
      end
      object StateImage: TEdit
        Left = 124
        Top = 116
        Width = 47
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = ValueChange
        OnExit = StateImageExit
      end
      object SelectedIndex: TEdit
        Left = 124
        Top = 89
        Width = 47
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = ValueChange
        OnExit = SelectedIndexExit
      end
      object ExpandedIndex: TEdit
        Left = 124
        Top = 143
        Width = 47
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = ValueChange
        OnExit = ExpandedIndexExit
      end
      object cbEnabled: TCheckBox
        Left = 10
        Top = 170
        Width = 209
        Height = 17
        Caption = 'Enabled'
        TabOrder = 5
        OnClick = EnabledChanged
      end
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 227
    Width = 527
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    DesignSize = (
      527
      49)
    object Apply: TButton
      Left = 358
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyClick
    end
    object Cancel: TButton
      Left = 275
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Help: TButton
      Left = 441
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpClick
    end
    object OkButton: TButton
      Left = 192
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'All Files(*.*)|*.*'
    Left = 109
    Top = 114
  end
end
