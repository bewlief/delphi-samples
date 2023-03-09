object ImageListEditor: TImageListEditor
  Left = 0
  Top = 0
  HelpContext = 26140
  Caption = 'ImageList Editor'
  ClientHeight = 376
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 415
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    584
    376)
  PixelsPerInch = 96
  TextHeight = 13
  object OK: TButton
    Left = 485
    Top = 29
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Cancel: TButton
    Left = 485
    Top = 62
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Apply: TButton
    Left = 485
    Top = 95
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'A&pply'
    TabOrder = 2
    OnClick = ApplyClick
  end
  object Help: TButton
    Left = 485
    Top = 128
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpClick
  end
  object ImageListGroup: TGroupBox
    Left = 17
    Top = 200
    Width = 543
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' &Images '
    TabOrder = 4
    DesignSize = (
      543
      121)
    object ImageView: TListView
      Left = 7
      Top = 40
      Width = 536
      Height = 73
      Margins.Left = 0
      Margins.Top = 12
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'asdf'
          Width = 100
        end>
      DragCursor = crArrow
      DragMode = dmAutomatic
      HideSelection = False
      IconOptions.Arrangement = iaLeft
      IconOptions.AutoArrange = True
      IconOptions.WrapText = False
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnCompare = ImageViewCompare
      OnEndDrag = ImageViewEndDrag
      OnDragDrop = ImageViewDragDrop
      OnDragOver = ImageViewDragOver
      OnSelectItem = ImageViewSelectItem
    end
  end
  object ImageGroup: TGroupBox
    Left = 17
    Top = 8
    Width = 450
    Height = 186
    Caption = ' &Selected Image '
    TabOrder = 5
    DesignSize = (
      450
      186)
    object OptionsPanel: TPanel
      Left = 93
      Top = 35
      Width = 354
      Height = 172
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 366
      object FillLabel: TLabel
        Left = 10
        Top = 50
        Width = 372
        Height = 13
        AutoSize = False
        Caption = '&Fill Color:'
        Transparent = True
      end
      object TransparentLabel: TLabel
        Left = 10
        Top = 0
        Width = 372
        Height = 13
        AutoSize = False
        Caption = '&Transparent Color:'
        Transparent = True
      end
      object OptionsGroup: TRadioGroup
        Left = 0
        Top = 100
        Width = 357
        Height = 69
        Caption = ' Options '
        Columns = 3
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'Cr&op'
          'St&retch'
          'C&enter')
        TabOrder = 0
        OnClick = OptionsGroupClick
      end
      object FillColor: TColorBox
        Left = 10
        Top = 68
        Width = 151
        Height = 22
        DefaultColorColor = clWindow
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        TabOrder = 1
        OnChange = FillColorChange
      end
      object TransparentColor: TColorBox
        Left = 10
        Top = 18
        Width = 151
        Height = 22
        DefaultColorColor = clWindow
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        TabOrder = 2
        OnChange = TransparentColorChange
      end
    end
    object MainPanel: TPanel
      Left = 7
      Top = 35
      Width = 78
      Height = 78
      BevelOuter = bvNone
      BorderWidth = 1
      ParentColor = True
      TabOrder = 0
      object Shape1: TShape
        Left = 1
        Top = 1
        Width = 76
        Height = 76
        Align = alClient
        Brush.Style = bsClear
        Pen.Style = psDot
        ExplicitLeft = 20
        ExplicitTop = 14
        ExplicitWidth = 65
        ExplicitHeight = 65
      end
      object MainImage: TImage
        AlignWithMargins = True
        Left = 2
        Top = 2
        Width = 74
        Height = 74
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Align = alClient
        Stretch = True
        OnMouseDown = MainImageMouseDown
        OnMouseMove = MainImageMouseMove
        OnMouseUp = MainImageMouseUp
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 76
        ExplicitHeight = 75
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 327
    Width = 584
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 6
    DesignSize = (
      584
      49)
    object Add: TButton
      Left = 28
      Top = 12
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Add...'
      TabOrder = 0
      OnClick = AddClick
    end
    object Clear: TButton
      Left = 352
      Top = 12
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Clear'
      Enabled = False
      TabOrder = 1
      OnClick = ClearClick
    end
    object Delete: TButton
      Left = 244
      Top = 12
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Delete'
      Enabled = False
      TabOrder = 2
      OnClick = DeleteClick
    end
    object ExportBtn: TButton
      Left = 460
      Top = 12
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'E&xport...'
      Enabled = False
      TabOrder = 3
      OnClick = ExportBtnClick
    end
    object ReplaceBtn: TButton
      Left = 136
      Top = 12
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Replace...'
      Enabled = False
      TabOrder = 4
      OnClick = AddClick
    end
  end
  object OpenDialog: TOpenPictureDialog
    HelpContext = 27000
    DefaultExt = 'bmp'
    Filter = 
      'All (*.bmp, *.ico)|*.bmp;*.ico|Bitmaps (*.bmp)|*.bmp|Icons (*.ic' +
      'o)|*.ico'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Add Images'
    Left = 32
    Top = 232
  end
  object DragTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = DragTimerTimer
    Left = 32
    Top = 280
  end
  object SaveDialog: TSavePictureDialog
    HelpContext = 27010
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofEnableSizing]
    Title = 'Export Images'
    Left = 32
    Top = 184
  end
end
