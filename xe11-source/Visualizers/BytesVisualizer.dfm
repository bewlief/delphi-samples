object BytesViewerFrame: TBytesViewerFrame
  Left = 0
  Top = 0
  Width = 547
  Height = 249
  TabOrder = 0
  PixelsPerInch = 96
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 531
    Height = 233
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 0
    ExplicitLeft = 184
    ExplicitTop = 48
    ExplicitWidth = 185
    ExplicitHeight = 41
    object pcViews: TPageControl
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 529
      Height = 231
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      ActivePage = tabHex
      Align = alClient
      TabHeight = 24
      TabOrder = 0
      OnChange = pcViewsChange
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 531
      ExplicitHeight = 233
      object tabHex: TTabSheet
        Caption = '&Hex'
        object grdHex: TStringGrid
          Left = 0
          Top = 0
          Width = 521
          Height = 197
          Align = alClient
          BorderStyle = bsNone
          ColCount = 18
          FixedRows = 0
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goRangeSelect, goFixedRowDefAlign]
          ParentFont = False
          TabOrder = 0
        end
      end
      object tabANSI: TTabSheet
        Caption = 'ANSI'
        ImageIndex = 1
        object mmANSI: TMemo
          Left = 0
          Top = 0
          Width = 521
          Height = 197
          Align = alClient
          BorderStyle = bsNone
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
          WantReturns = False
        end
      end
      object tabUnicode: TTabSheet
        Caption = '&Unicode'
        ImageIndex = 2
        object mmUnicode: TMemo
          Left = 0
          Top = 0
          Width = 521
          Height = 197
          Align = alClient
          BorderStyle = bsNone
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
          WantReturns = False
        end
      end
      object tabUTF8: TTabSheet
        Caption = 'U&TF8'
        ImageIndex = 3
        object mmUTF8: TMemo
          Left = 0
          Top = 0
          Width = 521
          Height = 197
          Align = alClient
          BorderStyle = bsNone
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
          WantReturns = False
        end
      end
    end
  end
end
