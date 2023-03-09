inherited GridViewForm: TGridViewForm
  Caption = 'Grid View'
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid [2]
    Left = 0
    Top = 161
    Width = 611
    Height = 269
    Align = alClient
    DataSource = OrdersSource
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBlack
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDblClick = DBGrid1DblClick
  end
end
