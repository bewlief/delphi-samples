object ApplicationTypeFrame: TApplicationTypeFrame
  Left = 0
  Top = 0
  Width = 251
  Height = 93
  TabOrder = 0
  object RadioButtonVCLApplication: TRadioButton
    Left = 0
    Top = 3
    Width = 113
    Height = 17
    Caption = 'VCL Application'
    TabOrder = 0
    OnClick = RadioButtonVCLApplicationClick
  end
  object RadioButtonConsoleApplication: TRadioButton
    Left = 0
    Top = 26
    Width = 113
    Height = 17
    Caption = 'Console Application'
    TabOrder = 1
    OnClick = RadioButtonVCLApplicationClick
  end
end
