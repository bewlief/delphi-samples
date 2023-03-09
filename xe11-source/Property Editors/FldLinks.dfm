object LinkFields: TLinkFields
  Left = 235
  Top = 123
  BorderStyle = bsDialog
  Caption = 'Field Link Designer'
  ClientHeight = 462
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object pnTop: TPanel
    Left = 0
    Top = 0
    Width = 442
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    FullRepaint = False
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      442
      36)
    object IndexLabel: TLabel
      Left = 8
      Top = 10
      Width = 90
      Height = 15
      Caption = 'A&vailable Indexes'
      FocusControl = IndexList
    end
    object IndexList: TComboBox
      Left = 130
      Top = 7
      Width = 304
      Height = 23
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = IndexListChange
      OnClick = IndexListChange
    end
  end
  object pnBottom: TPanel
    Left = 0
    Top = 413
    Width = 442
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    ShowCaption = False
    TabOrder = 2
    DesignSize = (
      442
      49)
    object Button1: TButton
      Left = 187
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = BitBtn1Click
    end
    object Button2: TButton
      Left = 271
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
      Left = 355
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Help'
      TabOrder = 2
      OnClick = HelpClick
    end
  end
  object pnMain: TPanel
    Left = 0
    Top = 36
    Width = 442
    Height = 377
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    ShowCaption = False
    TabOrder = 1
    object pnLink: TPanel
      Left = 0
      Top = 0
      Width = 442
      Height = 260
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      ShowCaption = False
      TabOrder = 0
      object pnLinkCenter: TPanel
        Left = 357
        Top = 0
        Width = 85
        Height = 260
        Align = alRight
        BevelOuter = bvNone
        FullRepaint = False
        ShowCaption = False
        TabOrder = 1
        object AddButton: TButton
          Left = 2
          Top = 121
          Width = 75
          Height = 25
          Caption = '&Add'
          TabOrder = 0
          OnClick = AddButtonClick
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 357
        Height = 260
        Align = alClient
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 0
        object pnLinkLeft: TPanel
          Left = 0
          Top = 130
          Width = 357
          Height = 130
          Align = alTop
          BevelOuter = bvNone
          FullRepaint = False
          ShowCaption = False
          TabOrder = 1
          object Label30: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 0
            Width = 349
            Height = 15
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            Caption = 'D&etail Fields'
            FocusControl = DetailList
            IsControl = True
            ExplicitWidth = 63
          end
          object DetailList: TListBox
            AlignWithMargins = True
            Left = 8
            Top = 23
            Width = 341
            Height = 94
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alClient
            IntegralHeight = True
            ItemHeight = 15
            TabOrder = 0
            OnClick = BindingListClick
            IsControl = True
          end
        end
        object pnLinkRight: TPanel
          Left = 0
          Top = 0
          Width = 357
          Height = 130
          Align = alTop
          BevelOuter = bvNone
          FullRepaint = False
          ShowCaption = False
          TabOrder = 0
          object Label31: TLabel
            AlignWithMargins = True
            Left = 8
            Top = 0
            Width = 349
            Height = 15
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            Caption = '&Master Fields'
            FocusControl = MasterList
            IsControl = True
            ExplicitWidth = 69
          end
          object MasterList: TListBox
            AlignWithMargins = True
            Left = 8
            Top = 23
            Width = 341
            Height = 94
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alClient
            IntegralHeight = True
            ItemHeight = 15
            TabOrder = 0
            OnClick = BindingListClick
            IsControl = True
          end
        end
      end
    end
    object pnJoined: TPanel
      Left = 0
      Top = 260
      Width = 442
      Height = 117
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      ShowCaption = False
      TabOrder = 1
      object pnJoinedRight: TPanel
        Left = 349
        Top = 0
        Width = 93
        Height = 117
        Align = alRight
        BevelOuter = bvNone
        FullRepaint = False
        ShowCaption = False
        TabOrder = 1
        object ClearButton: TButton
          Left = 6
          Top = 21
          Width = 75
          Height = 25
          Caption = '&Clear'
          TabOrder = 0
          OnClick = ClearButtonClick
        end
        object DeleteButton: TButton
          Left = 6
          Top = 52
          Width = 75
          Height = 25
          Caption = '&Delete'
          TabOrder = 1
          OnClick = DeleteButtonClick
        end
      end
      object pnJoinedLeft: TPanel
        Left = 0
        Top = 0
        Width = 349
        Height = 117
        Align = alClient
        BevelOuter = bvNone
        FullRepaint = False
        ShowCaption = False
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 0
          Width = 341
          Height = 15
          Margins.Left = 8
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = '&Joined Fields'
          FocusControl = BindList
          ExplicitWidth = 67
        end
        object BindList: TListBox
          AlignWithMargins = True
          Left = 8
          Top = 23
          Width = 341
          Height = 94
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alClient
          IntegralHeight = True
          ItemHeight = 15
          MultiSelect = True
          TabOrder = 0
          OnClick = BindListClick
          IsControl = True
        end
      end
    end
  end
end
