object ListViewItems: TListViewItems
  Left = 190
  Top = 158
  HelpContext = 26100
  BorderIcons = [biSystemMenu]
  Caption = 'ListView Items Editor'
  ClientHeight = 241
  ClientWidth = 576
  KeyPreview = True
  OldCreateOrder = True
  PopupMode = pmAuto
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 576
    Height = 192
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    object PropGroupBox: TGroupBox
      AlignWithMargins = True
      Left = 329
      Top = 8
      Width = 247
      Height = 176
      Margins.Left = 4
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 8
      Align = alClient
      Caption = 'Item Properties'
      TabOrder = 1
      DesignSize = (
        247
        176)
      object Label1: TLabel
        Left = 8
        Top = 35
        Width = 41
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Caption:'
        FocusControl = CaptionEdit
      end
      object Label2: TLabel
        Left = 8
        Top = 64
        Width = 65
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 'I&mage Index:'
        FocusControl = Image
      end
      object Label3: TLabel
        Left = 8
        Top = 93
        Width = 61
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = '&State Index:'
        FocusControl = StateImage
      end
      object Label4: TLabel
        Left = 8
        Top = 119
        Width = 33
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Group:'
      end
      object CaptionEdit: TEdit
        Left = 128
        Top = 32
        Width = 111
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 0
        OnChange = ValueChange
        OnExit = CaptionEditExit
      end
      object Image: TEdit
        Left = 128
        Top = 60
        Width = 45
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChange = ValueChange
        OnExit = ImageExit
      end
      object StateImage: TEdit
        Left = 128
        Top = 89
        Width = 45
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 2
        OnChange = ValueChange
        OnExit = StateImageExit
      end
      object cbGroupID: TComboBox
        Left = 128
        Top = 116
        Width = 111
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        TabOrder = 3
        OnChange = cbGroupIDChange
      end
    end
    object GroupBox1: TGroupBox
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 317
      Height = 176
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 8
      Align = alLeft
      Caption = '&Items'
      TabOrder = 0
      object Panel1: TPanel
        Left = 2
        Top = 15
        Width = 174
        Height = 159
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 0
        ExplicitLeft = 48
        ExplicitTop = 42
        ExplicitWidth = 73
        ExplicitHeight = 91
        object TreeView: TTreeView
          AlignWithMargins = True
          Left = 8
          Top = 18
          Width = 162
          Height = 138
          Margins.Left = 8
          Margins.Top = 18
          Margins.Right = 4
          Align = alClient
          DragMode = dmAutomatic
          HideSelection = False
          Indent = 19
          TabOrder = 0
          OnChange = TreeViewChange
          OnChanging = TreeViewChanging
          OnDragDrop = TreeViewDragDrop
          OnDragOver = TreeViewDragOver
          OnEdited = TreeViewEdited
          OnEditing = TreeViewEditing
          OnKeyDown = TreeViewKeyDown
          ExplicitTop = 17
          ExplicitHeight = 139
        end
      end
      object Panel2: TPanel
        Left = 176
        Top = 15
        Width = 139
        Height = 159
        Align = alRight
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 1
        DesignSize = (
          139
          159)
        object Delete: TButton
          Left = 9
          Top = 88
          Width = 125
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '&Delete'
          TabOrder = 2
          OnClick = DeleteClick
        end
        object New: TButton
          Left = 9
          Top = 27
          Width = 125
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '&New Item'
          Default = True
          TabOrder = 0
          OnClick = NewClick
        end
        object NewSub: TButton
          Left = 9
          Top = 58
          Width = 125
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'N&ew SubItem'
          TabOrder = 1
          OnClick = NewSubClick
        end
      end
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 192
    Width = 576
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    ShowCaption = False
    TabOrder = 1
    StyleElements = [seFont, seBorder]
    DesignSize = (
      576
      49)
    object Apply: TButton
      Left = 406
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Apply'
      TabOrder = 2
      OnClick = ApplyClick
    end
    object Button7: TButton
      Left = 487
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 3
      OnClick = Button7Click
    end
    object Cancel: TButton
      Left = 324
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object OkButton: TButton
      Left = 242
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
  end
end
