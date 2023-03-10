object StatsFrame: TStatsFrame
  Left = 0
  Top = 0
  Width = 213
  Height = 165
  HorzScrollBar.Range = 209
  VertScrollBar.Range = 163
  AutoScroll = False
  TabOrder = 0
  object GroupBox1: TGroupBox
    Left = 0
    Top = -3
    Width = 209
    Height = 166
    TabOrder = 0
    object lblTotalResponseTime: TLabel
      Left = 132
      Top = 33
      Width = 7
      Height = 15
      Caption = '0'
    end
    object lblAvgResponseTime: TLabel
      Left = 132
      Top = 51
      Width = 7
      Height = 15
      Caption = '0'
    end
    object lblLastResponseTime: TLabel
      Left = 132
      Top = 70
      Width = 7
      Height = 15
      Caption = '0'
    end
    object lblRequestCount: TLabel
      Left = 97
      Top = 14
      Width = 7
      Height = 15
      Caption = '0'
    end
    object Label3: TLabel
      Left = 8
      Top = 14
      Width = 81
      Height = 15
      Caption = 'RequestCount:'
    end
    object Label5: TLabel
      Left = 8
      Top = 33
      Width = 119
      Height = 15
      Caption = 'Total Response Time:'
    end
    object Label7: TLabel
      Left = 8
      Top = 51
      Width = 115
      Height = 15
      Caption = 'Avg Response Time:'
    end
    object Label8: TLabel
      Left = 8
      Top = 70
      Width = 115
      Height = 15
      Caption = 'Last Response Time:'
    end
    object Label1: TLabel
      Left = 8
      Top = 90
      Width = 113
      Height = 15
      Caption = 'Min Response Time:'
    end
    object Label2: TLabel
      Left = 8
      Top = 109
      Width = 116
      Height = 15
      Caption = 'Max Response Time:'
    end
    object lblMinResponseTime: TLabel
      Left = 132
      Top = 90
      Width = 7
      Height = 15
      Caption = '0'
    end
    object lblMaxResponseTime: TLabel
      Left = 132
      Top = 109
      Width = 7
      Height = 15
      Caption = '0'
    end
    object Button1: TButton
      Left = 9
      Top = 133
      Width = 49
      Height = 25
      Action = ResetCountsAction
      Caption = '&Reset'
      TabOrder = 0
    end
  end
  object ActionList1: TActionList
    Left = 248
    Top = 40
    object ResetCountsAction: TAction
      Caption = '&Reset'
      OnExecute = ResetCountsActionExecute
      OnUpdate = ResetCountsActionUpdate
    end
  end
end
