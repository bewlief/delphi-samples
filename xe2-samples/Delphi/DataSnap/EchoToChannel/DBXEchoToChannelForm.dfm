object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'DBX EchoToChannel Client'
  ClientHeight = 380
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    482
    380)
  PixelsPerInch = 120
  TextHeight = 16
  object LabelManagerId: TLabel
    Left = 67
    Top = 8
    Width = 91
    Height = 16
    Caption = 'LabelManagerId'
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 16
    Caption = 'Client ID:'
  end
  object Label2: TLabel
    Left = 8
    Top = 112
    Width = 25
    Height = 16
    Caption = 'Log:'
  end
  object ButtonConnect: TButton
    Left = 8
    Top = 33
    Width = 105
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = ButtonConnectClick
  end
  object ButtonDisconnect: TButton
    Left = 126
    Top = 33
    Width = 105
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 1
    OnClick = ButtonDisconnectClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 134
    Width = 466
    Height = 203
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
  end
  object ButtonClearLog: TButton
    Left = 399
    Top = 349
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Clear'
    TabOrder = 5
    OnClick = ButtonClearLogClick
  end
  object ButtonEcho: TButton
    Left = 399
    Top = 73
    Width = 75
    Height = 25
    Caption = 'Echo'
    TabOrder = 3
    OnClick = ButtonEchoClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 73
    Width = 369
    Height = 24
    TabOrder = 2
    Text = 'Text to Echo'
  end
  object DSClientCallbackChannelManager1: TDSClientCallbackChannelManager
    DSHostname = 'localhost'
    DSPort = '211'
    CommunicationProtocol = 'tcp/ip'
    ChannelName = 'Echo'
    ManagerId = '14293.185643.93700'
    Left = 440
    Top = 105
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 432
    Top = 9
  end
end
