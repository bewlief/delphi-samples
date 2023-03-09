object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'TTreeVew CheckBox Demo'
  ClientHeight = 1221
  ClientWidth = 1316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 192
  DesignSize = (
    1316
    1221)
  TextHeight = 27
  object Label1: TLabel
    Left = 34
    Top = 608
    Width = 94
    Height = 27
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = 'State log:'
  end
  object SpeedButton1: TSpeedButton
    Left = 555
    Top = 600
    Width = 97
    Height = 44
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Anchors = [akTop, akRight]
    Caption = 'Clear'
    OnClick = SpeedButton1Click
    ExplicitLeft = 574
  end
  object TreeView1: TTreeView
    Left = 34
    Top = 24
    Width = 618
    Height = 564
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Anchors = [akLeft, akTop, akRight]
    AutoExpand = True
    CheckBoxes = True
    CheckStyles = [csPartial, csDimmed, csExclusion]
    Indent = 38
    TabOrder = 0
    OnCheckStateChanged = TreeViewCheckStateChanged
    OnCheckStateChanging = TreeViewCheckStateChanging
    Items.NodeData = {
      0305000000340000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010B45006D00620061007200630061006400650072006F00320000
      000000000000000000FFFFFFFFFFFFFFFF000000000000000002000000010A52
      00410044002000530074007500640069006F002A0000000000000000000000FF
      FFFFFFFFFFFFFF0000000000000000000000000106440065006C007000680069
      00320000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
      00010A43002B002B004200750069006C00640065007200240000000000000000
      000000FFFFFFFFFFFFFFFF0000000000000000020000000103560043004C002C
      0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000200000001
      07570069006E0064006F0077007300240000000000000000000000FFFFFFFFFF
      FFFFFF0000000000000000000000000103760031003000240000000000000000
      000000FFFFFFFFFFFFFFFF000000000000000000000000010376003100310030
      0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000200000001
      0949006E00740065006C002F0041004D0044002A0000000000000000000000FF
      FFFFFFFFFFFFFF0000000000000000000000000106330032002D006200690074
      002A0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
      000106360034002D00620069007400340000000000000000000000FFFFFFFFFF
      FFFFFF000000000000000001000000010B4E0065007700200046006500610074
      00750072006500320000000000000000000000FFFFFFFFFFFFFFFF0000000000
      00000000000000010A560065007200790020004E006900630065002100300000
      000000000000000000FFFFFFFFFFFFFFFF000000000000000001000000010954
      0054007200650065005600690065007700320000000000000000000000FFFFFF
      FFFFFFFFFF000000000000000001000000010A43006800650063006B0062006F
      00780065007300340000000000000000000000FFFFFFFFFFFFFFFF0000000000
      00000000000000010B4D0075006C00740069002D0053007400610074006500}
  end
  object lstLog: TListBox
    Left = 34
    Top = 653
    Width = 618
    Height = 541
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 27
    Items.Strings = (
      'Keeps a log'
      'of changes '
      'in check box '
      'states and '
      'options')
    TabOrder = 1
    ExplicitWidth = 604
    ExplicitHeight = 540
  end
  object Panel1: TPanel
    Left = 691
    Top = 24
    Width = 598
    Height = 1186
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 2
    ExplicitLeft = 677
    object Button1: TButton
      Left = 23
      Top = 8
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Toggle CheckBoxes'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button10: TButton
      Left = 23
      Top = 728
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Top node NodeCheckState = ncsPartial'
      TabOrder = 1
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 23
      Top = 808
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Top node NodeCheckState = ncsDimmed'
      TabOrder = 2
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 23
      Top = 888
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Top node NodeCheckState = ncsExclusion'
      TabOrder = 3
      OnClick = Button12Click
    end
    object Button13: TButton
      Left = 23
      Top = 1048
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Full Expand'
      TabOrder = 4
      OnClick = Button13Click
    end
    object Button14: TButton
      Left = 23
      Top = 1118
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Full Collapse'
      TabOrder = 5
      OnClick = Button14Click
    end
    object Button15: TButton
      Left = 23
      Top = 970
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Random States'
      TabOrder = 6
      OnClick = Button15Click
    end
    object Button2: TButton
      Left = 23
      Top = 88
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'CheckStyles = []'
      TabOrder = 7
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 23
      Top = 168
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'CheckStyles = [csPartial]'
      TabOrder = 8
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 23
      Top = 248
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'CheckStyles = [csPartial, csDimmed]'
      TabOrder = 9
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 23
      Top = 326
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'CheckStyles = [csPartial, csExclusion]'
      TabOrder = 10
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 23
      Top = 408
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'CheckStyles = [csDimmed, csExclusion]'
      TabOrder = 11
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 23
      Top = 488
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'CheckStyles = [csPartial, csDimmed, csExclusion]'
      TabOrder = 12
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 23
      Top = 568
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Top node NodeCheckState = ncsUnchecked'
      TabOrder = 13
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 23
      Top = 648
      Width = 546
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = 'Top node NodeCheckState = ncsChecked'
      TabOrder = 14
      OnClick = Button9Click
    end
  end
  object FDMemTable1: TFDMemTable
    Active = True
    FieldDefs = <
      item
        Name = 'word'
        DataType = ftString
        Size = 1000
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvPersistent, rvSilentMode]
    ResourceOptions.Persistent = True
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 432
    Top = 360
    Content = {
      414442530F0000007B0C0000FF00010001FF02FF03040016000000460044004D
      0065006D005400610062006C0065003100050016000000460044004D0065006D
      005400610062006C0065003100060000000000070000080032000000090000FF
      0AFF0B04000800000077006F007200640005000800000077006F00720064000C
      00010000000E000D000F00E80300001000011100011200011300011400011500
      0116000800000077006F00720064001700E8030000FEFEFF18FEFF19FEFF1AFF
      1B1C0000000000FF1D00000D000000416363656C65726F6D65746572FEFEFF1B
      1C0001000000FF1D0000040000004144534CFEFEFF1B1C0002000000FF1D0000
      07000000416E64726F6964FEFEFF1B1C0003000000FF1D000007000000417263
      68697665FEFEFF1B1C0004000000FF1D000003000000415458FEFEFF1B1C0005
      000000FF1D0000060000004261636B7570FEFEFF1B1C0006000000FF1D000009
      00000042616E647769647468FEFEFF1B1C0007000000FF1D0000040000004261
      7564FEFEFF1B1C0008000000FF1D00000900000042656E63686D61726BFEFEFF
      1B1C0009000000FF1D000007000000426172636F6465FEFEFF1B1C000A000000
      FF1D000007000000426F6F74696E67FEFEFF1B1C000B000000FF1D00000B0000
      00426F6F74206C6F61646572FEFEFF1B1C000C000000FF1D0000040000004249
      4F53FEFEFF1B1C000D000000FF1D0000060000004269746D6170FEFEFF1B1C00
      0E000000FF1D000007000000426974636F696EFEFEFF1B1C000F000000FF1D00
      000A000000426974546F7272656E74FEFEFF1B1C0010000000FF1D0000090000
      00426C61636B6C697374FEFEFF1B1C0011000000FF1D00000A000000426C6F63
      6B636861696EFEFEFF1B1C0012000000FF1D000009000000426C7565746F6F74
      68FEFEFF1B1C0013000000FF1D00000600000042696E617279FEFEFF1B1C0014
      000000FF1D0000080000004261636B6C696E6BFEFEFF1B1C0015000000FF1D00
      0009000000426C6F617477617265FEFEFF1B1C0016000000FF1D000007000000
      426F726C616E64FEFEFF1B1C0017000000FF1D000003000000427573FEFEFF1B
      1C0018000000FF1D000003000000432B2BFEFEFF1B1C0019000000FF1D00000A
      000000432B2B4275696C646572FEFEFF1B1C001A000000FF1D00000500000043
      61636865FEFEFF1B1C001B000000FF1D000008000000436865636B73756DFEFE
      FF1B1C001C000000FF1D00000B000000436F6D7072657373696F6EFEFEFF1B1C
      001D000000FF1D000007000000436F6E74656E74FEFEFF1B1C001E000000FF1D
      000004000000434D4F53FEFEFF1B1C001F000000FF1D000006000000436F6F6B
      6965FEFEFF1B1C0020000000FF1D00000B0000004379626572206372696D65FE
      FEFF1B1C0021000000FF1D00000D00000043796265727365637572697479FEFE
      FF1B1C0022000000FF1D0000060000004461656D6F6EFEFEFF1B1C0023000000
      FF1D0000080000004461746162617365FEFEFF1B1C0024000000FF1D00000700
      00004465762D432B2BFEFEFF1B1C0025000000FF1D0000050000004465627567
      FEFEFF1B1C0026000000FF1D000009000000446576656C6F706572FEFEFF1B1C
      0027000000FF1D00000600000044656C706869FEFEFF1B1C0028000000FF1D00
      0004000000446F636BFEFEFF1B1C0029000000FF1D000006000000446F6E676C
      65FEFEFF1B1C002A000000FF1D000003000000446F73FEFEFF1B1C002B000000
      FF1D000006000000447269766572FEFEFF1B1C002C000000FF1D00000D000000
      44657669636520647269766572FEFEFF1B1C002D000000FF1D00000300000044
      5049FEFEFF1B1C002E000000FF1D00000A000000456E6372797074696F6EFEFE
      FF1B1C002F000000FF1D000008000000456D756C61746F72FEFEFF1B1C003000
      0000FF1D00000800000045746865726E6574FEFEFF1B1C0031000000FF1D0000
      08000000456E642075736572FEFEFF1B1C0032000000FF1D0000050000004641
      543332FEFEFF1B1C0033000000FF1D0000090000004672616D65776F726BFEFE
      FF1B1C0034000000FF1D0000080000004672656577617265FEFEFF1B1C003500
      0000FF1D0000080000004669726577616C6CFEFEFF1B1C0036000000FF1D0000
      03000000465450FEFEFF1B1C0037000000FF1D000003000000474946FEFEFF1B
      1C0038000000FF1D000003000000474954FEFEFF1B1C0039000000FF1D000003
      000000475053FEFEFF1B1C003A000000FF1D000003000000475549FEFEFF1B1C
      003B000000FF1D00000400000048617368FEFEFF1B1C003C000000FF1D000004
      00000048544D4CFEFEFF1B1C003D000000FF1D0000050000004854545053FEFE
      FF1B1C003E000000FF1D00000700000048656164696E67FEFEFF1B1C003F0000
      00FF1D000009000000496E74657262617365FEFEFF1B1C0040000000FF1D0000
      03000000492F4FFEFEFF1B1C0041000000FF1D00000400000049454545FEFEFF
      1B1C0042000000FF1D00000300000049534FFEFEFF1B1C0043000000FF1D0000
      04000000494D4549FEFEFF1B1C0044000000FF1D000003000000495350FEFEFF
      1B1C0045000000FF1D000008000000496E7465726E6574FEFEFF1B1C00460000
      00FF1D0000040000004A504547FEFEFF1B1C0047000000FF1D0000060000004B
      65726E656CFEFEFF1B1C0048000000FF1D0000030000004D7033FEFEFF1B1C00
      49000000FF1D0000070000004D616C77617265FEFEFF1B1C004A000000FF1D00
      00030000004D4D53FEFEFF1B1C004B000000FF1D0000040000004D494449FEFE
      FF1B1C004C000000FF1D0000070000004D616368696E65FEFEFF1B1C004D0000
      00FF1D0000060000004E6577626965FEFEFF1B1C004E000000FF1D0000030000
      004F454DFEFEFF1B1C004F000000FF1D0000020000004F53FEFEFF1B1C005000
      0000FF1D0000030000004F4352FEFEFF1B1C0051000000FF1D0000090000004F
      766572636C6F636BFEFEFF1B1C0052000000FF1D0000080000004F7665726865
      6174FEFEFF1B1C0053000000FF1D00000600000050617363616CFEFEFF1B1C00
      54000000FF1D000003000000504446FEFEFF1B1C0055000000FF1D0000080000
      005068697368696E67FEFEFF1B1C0056000000FF1D000006000000507974686F
      6EFEFEFF1B1C0057000000FF1D000007000000506C75672D696EFEFEFF1B1C00
      58000000FF1D00000900000050726F636573736F72FEFEFF1B1C0059000000FF
      1D000006000000515745525459FEFEFF1B1C005A000000FF1D00000D00000052
      656D6F746520616363657373FEFEFF1B1C005B000000FF1D0000080000005265
      676973747279FEFEFF1B1C005C000000FF1D000009000000526561642D6F6E6C
      79FEFEFF1B1C005D000000FF1D00000400000052414944FEFEFF1B1C005E0000
      00FF1D000007000000526F6F74696E67FEFEFF1B1C005F000000FF1D00000300
      000052414DFEFEFF1B1C0060000000FF1D00000900000053616665206D6F6465
      FEFEFF1B1C0061000000FF1D00000400000053534944FEFEFF1B1C0062000000
      FF1D00000300000053454FFEFEFF1B1C0063000000FF1D00000C000000536572
      76696365207061636BFEFEFF1B1C0064000000FF1D0000060000005365727665
      72FEFEFF1B1C0065000000FF1D00000B000000536F7572636520636F6465FEFE
      FF1B1C0066000000FF1D0000040000005370616DFEFEFF1B1C0067000000FF1D
      00000D00000053656172636820656E67696E65FEFEFF1B1C0068000000FF1D00
      00050000005472617368FEFEFF1B1C0069000000FF1D00000A000000556E6465
      72636C6F636BFEFEFF1B1C006A000000FF1D000004000000556E6978FEFEFF1B
      1C006B000000FF1D0000050000005669727573FEFEFF1B1C006C000000FF1D00
      0003000000564741FEFEFF1B1C006D000000FF1D000004000000564F4950FEFE
      FF1B1C006E000000FF1D000003000000574542FEFEFF1B1C006F000000FF1D00
      000500000057692D4669FEFEFF1B1C0070000000FF1D000007000000486F7473
      706F74FEFEFF1B1C0071000000FF1D00000400000057696B69FEFEFF1B1C0072
      000000FF1D00000900000057696B697065646961FEFEFF1B1C0073000000FF1D
      00000700000057696E646F7773FEFEFF1B1C0074000000FF1D00000C00000057
      6972656C657373204C414EFEFEFF1B1C0075000000FF1D00000E000000576F72
      6C64205769646520576562FEFEFF1B1C0076000000FF1D000007000000575953
      49575947FEFEFF1B1C0077000000FF1D000003000000575041FEFEFF1B1C0078
      000000FF1D00000600000057756D707573FEFEFF1B1C0079000000FF1D000003
      0000005A6970FEFEFEFEFEFF1EFEFF1F20007A000000FF21FEFEFE0E004D0061
      006E0061006700650072001E0055007000640061007400650073005200650067
      006900730074007200790012005400610062006C0065004C006900730074000A
      005400610062006C00650008004E0061006D006500140053006F007500720063
      0065004E0061006D0065000A0054006100620049004400240045006E0066006F
      0072006300650043006F006E00730074007200610069006E00740073001E004D
      0069006E0069006D0075006D0043006100700061006300690074007900180043
      006800650063006B004E006F0074004E0075006C006C00140043006F006C0075
      006D006E004C006900730074000C0043006F006C0075006D006E00100053006F
      0075007200630065004900440018006400740041006E00730069005300740072
      0069006E0067001000440061007400610054007900700065000800530069007A
      0065001400530065006100720063006800610062006C006500120041006C006C
      006F0077004E0075006C006C000800420061007300650014004F0041006C006C
      006F0077004E0075006C006C0012004F0049006E005500700064006100740065
      0010004F0049006E00570068006500720065001A004F0072006900670069006E
      0043006F006C004E0061006D006500140053006F007500720063006500530069
      007A0065001C0043006F006E00730074007200610069006E0074004C00690073
      007400100056006900650077004C006900730074000E0052006F0077004C0069
      0073007400060052006F0077000A0052006F0077004900440010004F00720069
      00670069006E0061006C001800520065006C006100740069006F006E004C0069
      00730074001C0055007000640061007400650073004A006F00750072006E0061
      006C001200530061007600650050006F0069006E0074000E004300680061006E
      00670065007300}
    object FDMemTable1word: TStringField
      FieldName = 'word'
      Size = 1000
    end
  end
end
