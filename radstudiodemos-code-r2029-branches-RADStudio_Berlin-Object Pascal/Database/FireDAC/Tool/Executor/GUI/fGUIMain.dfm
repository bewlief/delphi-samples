object frmExecGUIMain: TfrmExecGUIMain
  Left = 362
  Top = 112
  Caption = 'FireDAC Executor'
  ClientHeight = 623
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 790
    Height = 598
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    ParentBackground = False
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 4
      Top = 440
      Width = 782
      Height = 4
      Cursor = crVSplit
      Align = alBottom
    end
    object Panel10: TPanel
      Left = 4
      Top = 35
      Width = 782
      Height = 4
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 0
    end
    object Panel11: TPanel
      Left = 4
      Top = 39
      Width = 782
      Height = 401
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clBtnShadow
      ParentBackground = False
      TabOrder = 1
      object pnlInstructionName: TPanel
        Left = 1
        Top = 1
        Width = 780
        Height = 23
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Color = clSkyBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
        object lblTitle: TLabel
          Left = 11
          Top = 4
          Width = 57
          Height = 13
          Caption = 'SQL Script'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object pnlTitleBottomLine: TPanel
          Left = 0
          Top = 22
          Width = 780
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = clBtnShadow
          ParentBackground = False
          TabOrder = 0
        end
      end
      object Panel5: TPanel
        Left = 1
        Top = 24
        Width = 780
        Height = 376
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 1
      end
    end
    object pnlTopFrame: TPanel
      Left = 4
      Top = 4
      Width = 782
      Height = 31
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clGray
      Ctl3D = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 2
      object pnlToolbar: TPanel
        Left = 1
        Top = 1
        Width = 187
        Height = 29
        Align = alLeft
        BevelOuter = bvNone
        BorderWidth = 3
        Color = clWhite
        ParentBackground = False
        TabOrder = 0
        object ToolBar1: TToolBar
          Left = 3
          Top = 3
          Width = 181
          Height = 23
          Align = alClient
          ButtonWidth = 26
          Caption = 'ToolBar1'
          Color = clBtnFace
          Images = ilMain
          ParentColor = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Transparent = True
          object tbtOpen: TToolButton
            Left = 0
            Top = 0
            Action = acOpenScript
          end
          object ToolButton6: TToolButton
            Left = 26
            Top = 0
            Action = acSaveScript
          end
          object ToolButton2: TToolButton
            Left = 52
            Top = 0
            Width = 8
            Caption = 'ToolButton2'
            ImageIndex = 3
            Style = tbsSeparator
          end
          object tbtRun: TToolButton
            Left = 60
            Top = 0
            Action = acRun
          end
          object ToolButton1: TToolButton
            Left = 86
            Top = 0
            Action = acRunByStep
          end
          object ToolButton3: TToolButton
            Left = 112
            Top = 0
            Action = acSkipStep
          end
          object ToolButton4: TToolButton
            Left = 138
            Top = 0
            Width = 8
            Caption = 'ToolButton4'
            ImageIndex = 4
            Style = tbsSeparator
          end
          object ToolButton5: TToolButton
            Left = 146
            Top = 0
            Action = acHelp
          end
          object ToolButton8: TToolButton
            Left = 172
            Top = 0
            Width = 8
            Caption = 'ToolButton8'
            ImageIndex = 3
            Style = tbsSeparator
          end
        end
      end
      object Panel1: TPanel
        Left = 188
        Top = 1
        Width = 593
        Height = 29
        Align = alClient
        BevelOuter = bvNone
        Color = clWindow
        ParentBackground = False
        TabOrder = 1
        object Label3: TLabel
          Left = 3
          Top = 7
          Width = 57
          Height = 13
          Caption = '&Connect to:'
          FocusControl = cbConnDef
          Transparent = False
        end
        object cbConnDef: TComboBox
          Left = 68
          Top = 4
          Width = 124
          Height = 21
          BevelInner = bvSpace
          BevelKind = bkFlat
          BevelOuter = bvRaised
          Style = csDropDownList
          Ctl3D = True
          ParentCtl3D = False
          TabOrder = 0
          OnChange = cbConnDefChange
        end
      end
    end
    object Panel15: TPanel
      Left = 4
      Top = 444
      Width = 782
      Height = 150
      Align = alBottom
      Alignment = taLeftJustify
      BevelOuter = bvNone
      BorderWidth = 1
      Color = clBtnShadow
      ParentBackground = False
      TabOrder = 3
      object Panel16: TPanel
        Left = 1
        Top = 1
        Width = 780
        Height = 23
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Color = clSkyBlue
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
        object Label2: TLabel
          Left = 11
          Top = 4
          Width = 20
          Height = 13
          Caption = 'Log'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Panel17: TPanel
          Left = 0
          Top = 22
          Width = 780
          Height = 1
          Align = alBottom
          BevelOuter = bvNone
          Color = clBtnShadow
          ParentBackground = False
          TabOrder = 0
        end
      end
      object Panel18: TPanel
        Left = 1
        Top = 24
        Width = 780
        Height = 125
        Align = alClient
        BevelOuter = bvNone
        Color = clWhite
        ParentBackground = False
        TabOrder = 1
        object mmLog: TMemo
          Left = 0
          Top = 0
          Width = 780
          Height = 125
          Align = alClient
          BevelInner = bvNone
          BorderStyle = bsNone
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssBoth
          ShowHint = True
          TabOrder = 0
          OnDblClick = mmLogDblClick
        end
      end
    end
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 598
    Width = 790
    Height = 25
    BorderWidth = 2
    Panels = <
      item
        Width = 300
      end
      item
        Width = 80
      end
      item
        Width = 80
      end>
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'sql'
    Filter = 'SQL Files (*.sql)|*.sql|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 22
    Top = 104
  end
  object ActionList1: TActionList
    Images = ilMain
    Left = 23
    Top = 72
    object acNewScript: TAction
      Category = 'File'
      Caption = '&New Script'
      Hint = 'Create empty script'
      ShortCut = 16462
      OnExecute = acNewScriptExecute
    end
    object acOpenScript: TAction
      Category = 'File'
      Caption = '&Open Script...'
      Hint = 'Load a script into executor window'
      ImageIndex = 0
      ShortCut = 16463
      OnExecute = acOpenScriptExecute
    end
    object acSaveScript: TAction
      Category = 'File'
      Caption = '&Save Script'
      Hint = 'Save executor window into script file'
      ImageIndex = 1
      ShortCut = 16467
      OnExecute = acSaveScriptExecute
      OnUpdate = acSaveScriptUpdate
    end
    object acSaveScriptAs: TAction
      Category = 'File'
      Caption = 'Save Script &As...'
      Hint = 'Save executor window into script file, ask user for file name'
      ShortCut = 24659
      OnExecute = acSaveScriptAsExecute
      OnUpdate = acSaveScriptAsUpdate
    end
    object acSaveLog: TAction
      Category = 'File'
      Caption = 'Save &Log...'
      Hint = 'Save log content into file'
      OnExecute = acSaveLogExecute
      OnUpdate = acSaveLogUpdate
    end
    object acExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      OnExecute = acExitExecute
    end
    object acRun: TAction
      Category = 'Run'
      Caption = '&Run Script'
      Hint = 'Run the script until it is finished'
      ImageIndex = 3
      ShortCut = 120
      OnExecute = acRunExecute
      OnUpdate = acRunUpdate
    end
    object acRunByStep: TAction
      Category = 'Run'
      Caption = 'Run by &Step'
      Hint = 'Run next script command'
      ImageIndex = 5
      ShortCut = 118
      OnExecute = acRunByStepExecute
      OnUpdate = acRunByStepUpdate
    end
    object acSkipStep: TAction
      Category = 'Run'
      Caption = 'S&kip Command'
      Hint = 'Skip next script command'
      ImageIndex = 4
      ShortCut = 8310
      OnExecute = acSkipStepExecute
      OnUpdate = acSkipStepUpdate
    end
    object acContinueOnError: TAction
      Category = 'Options'
      Caption = '&Continue on Error'
      Hint = 'Stop or continue script execution on error'
      OnExecute = acContinueOnErrorExecute
      OnUpdate = acContinueOnErrorUpdate
    end
    object acDropNonexistObj: TAction
      Category = 'Options'
      Caption = '&Drop Nonexistent Object'
      OnExecute = acDropNonexistObjExecute
      OnUpdate = acDropNonexistObjUpdate
    end
    object acShowMessages: TAction
      Category = 'Options'
      Caption = 'Show &Messages'
      OnExecute = acShowMessagesExecute
      OnUpdate = acShowMessagesUpdate
    end
    object acHelp: TAction
      Category = 'Help'
      Caption = '&Help...'
      Hint = 'Show FDExecutor commands help'
      ImageIndex = 2
      ShortCut = 112
      OnExecute = acHelpExecute
    end
    object acAbout: TAction
      Category = 'Help'
      Caption = '&About ...'
      OnExecute = acAboutExecute
    end
  end
  object MainMenu1: TMainMenu
    Images = ilMain
    Left = 88
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object NewScript1: TMenuItem
        Action = acNewScript
      end
      object Opensqlfile1: TMenuItem
        Action = acOpenScript
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object SaveLogas1: TMenuItem
        Action = acSaveScript
      end
      object SaveScriptAs1: TMenuItem
        Action = acSaveScriptAs
      end
      object SaveLog1: TMenuItem
        Action = acSaveLog
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = acExit
      end
    end
    object Run1: TMenuItem
      Caption = '&Run'
      object Runscript1: TMenuItem
        Action = acRun
      end
      object Runbystep1: TMenuItem
        Action = acRunByStep
      end
      object Dropoutcommand1: TMenuItem
        Action = acSkipStep
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object mniContOnErrors: TMenuItem
        Action = acContinueOnError
      end
      object HideNoObjAtDropErrors1: TMenuItem
        Action = acDropNonexistObj
      end
      object mniShowMessages: TMenuItem
        Action = acShowMessages
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Help2: TMenuItem
        Action = acHelp
      end
      object About1: TMenuItem
        Action = acAbout
      end
    end
  end
  object FDGUIxErrorDialog1: TFDGUIxErrorDialog
    Provider = 'Forms'
    Caption = 'FireDAC Executor Error'
    Left = 21
    Top = 138
  end
  object dlgSaveScript: TSaveDialog
    DefaultExt = 'sql'
    Filter = 'SQL Files (*.sql)|*.sql|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 53
    Top = 104
  end
  object FDGUIxLoginDialog1: TFDGUIxLoginDialog
    Provider = 'Forms'
    Caption = 'FireDAC Executor Login'
    VisibleItems.Strings = (
      '*')
    Left = 53
    Top = 138
  end
  object FDGUIxAsyncExecuteDialog1: TFDGUIxAsyncExecuteDialog
    Provider = 'Forms'
    Caption = 'FireDAC Executor Working'
    Left = 85
    Top = 138
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 117
    Top = 138
  end
  object ilMain: TImageList
    Left = 54
    Top = 72
    Bitmap = {
      494C010106000900080010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CB8C4400A65400006336000063360000A654000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00CB8C4400CB8C4400CB8C440063360000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000CB8C4400A65400006336000063360000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00D9A77D00CB8C4400CB8C4400A3760000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00CB8C4400CB8C4400CB8C44006336
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00D9A77D00D9A77D00CB8C4400A3760000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00D9A77D00CB8C4400CB8C4400A376
      0000A65400000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A3760000D9A77D00D9A77D00D9A77D00D9A77D00A3760000A65400000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00D9A77D00D9A77D00CB8C4400A376
      0000A65400000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A3760000FFFFCC00FFFFCC00FFFFCC00FFFFCC00A37600000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A3760000D9A77D00D9A77D00D9A77D00D9A77D00A376
      0000A65400000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A3760000A3760000A3760000A3760000CB8C44000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A3760000FFFFCC00FFFFCC00FFFFCC00FFFF
      CC00A37600000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CB8C44000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3760000A3760000A3760000A376
      0000CB8C44000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CB8C
      4400CB8C4400CB8C440000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CB8C44000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CB8C4400CB8C
      4400CB8C4400CB8C4400CB8C4400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CB8C4400CB8C4400CB8C
      4400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CB8C4400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CB8C44000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CB8C4400CB8C4400CB8C4400CB8C
      4400CB8C44000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CB8C44000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CB8C44000000
      000000000000000000000000000000000000000000000000000000000000CB8C
      4400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CB8C44000000000000000000CB8C440000000000CB8C4400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CB8C4400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CB8C440000000000CB8C44000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AD5A5A00AD52
      5200A54A4A00AD949400C6CEC600CECEC600CECEC600C6CEC600C6CEC600B59C
      9C009C4242009C424200A5525200000000000000000000000000000000000000
      00009C9C9C006B6B6B00525252004A4A4A004A4A4A004A4A4A00525252009C9C
      9C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD60031B5DE0021AD
      D600000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD7B7300CE636300CE6B
      6B00B55A5A009C848400BDA5A500E7CECE00FFF7F700FFFFF700F7F7F700CEB5
      B500942929009C313100C65A5A00AD5A5A00000000000000000000000000ADAD
      FD00E7CEC600EFDED600F7E7D600F7E7D600EFDED600EFDED600CEBDB5005A5A
      5A00737373009C9C9C0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD6009CDEEF0084EF
      FF004AC6E70021ADD60018A5C60018A5C60018A5C60000000000000000000000
      00000000000000000000000000000000000000000000BD7B7300CE636300CE63
      6300B55A5A009C7B7B009C424200B5737300E7DEDE00FFF7F700FFFFFF00D6B5
      B500943131009C313100BD5A5A00AD5A5A000000000000000000CEC6BD00F7E7
      D600F7F7F700E7E7E700CEB5A500D6AD9400DEC6BD00EFF7F700F7EFEF00EFDE
      D6008C847B006B6B6B009C9C9C00000000000000000000000000000000000000
      000000000000004A000000630000004A00000000000000000000000000000000
      0000000000000000000000000000000000000000000029ADD60052BDE7009CFF
      FF0094FFFF0073DEF70073DEF70073DEF70073DEF7004AC6E70021ADD60018A5
      C6000000000000000000000000000000000000000000BD7B7300CE636300CE63
      6300B55A5A00AD8484009C3939009C393900CEBDBD00EFEFEF00FFFFFF00E7C6
      C6009429290094313100BD5A5A00AD5A5A0000000000E7D6CE00F7E7DE00F7FF
      FF00CE9C8400B54A1000BD633900D6AD9C00C65A2100BD522100D6AD9C00F7FF
      FF00F7DED6007B736B0073737300000000000000000000000000000000000000
      00000000000000630000089C210000630000004A000000000000000000000000
      0000000000000000000000000000000000000000000029ADD60052BDE700ADFF
      FF008CF7FF008CEFFF008CEFFF008CEFFF0073DEF70073DEF70073DEF7004AC6
      EF0021ADD60000000000000000000000000000000000BD7B7300CE636300CE63
      6300B55A5A00B58C8C009C4A4A0094313100A59C9C00D6D6D600FFFFFF00E7C6
      C6009429290094313100BD5A5A00AD5A5A0000000000EFDED600F7FFFF00C67B
      5A00BD4A1000C6521800C6A59400FFFFFF00DE947300BD4A1000B54A1000CE9C
      8400F7FFFF00EFDED6005A5A5A009C9C9C000000000000000000000000000000
      00000000000000630000109C2100089C210000630000004A0000000000000000
      0000000000000000000000000000000000000000000029ADD60029ADD600ADDE
      EF0094F7FF0094F7FF008CEFFF008CEFFF008CEFFF008CEFFF0073DEF70073DE
      F7004AC6EF0000000000000000000000000000000000BD7B7300CE636300CE63
      6300BD5A5A00C6948C00C6949400B5848400AD8C8C00BDA5A500E7C6C600DEAD
      FD00A5393900A5393900C65A5A00AD5A5A00F7E7E700F7F7F700D6AD9400BD4A
      1000CE633100CE632900CE6B3900DE8C6B00CE632900CE633100C65A2900B54A
      1000DEC6BD00F7EFE700A59C9400636363000000000000000000000000000000
      0000000000000063000010A5290010A52900089C210000630000004A00000000
      0000000000000000000000000000000000000000000029ADD60073DEF70029AD
      D6009CFFFF008CF7FF008CF7FF008CF7FF008CEFFF008CEFFF008CEFFF0073DE
      F70073DEF70018A5C600000000000000000000000000BD7B7300CE636300CE63
      6300CE636300CE636300CE636300CE636300CE636300C65A5A00C65A5A00CE63
      6300CE636300CE636300CE6B6B00AD525A00F7EFE700F7F7F700C6633100C65A
      2900CE6B3100CE5A2100CE8C6B00F7E7DE00CE6B3900C65A2100CE633100C652
      1800C67B5200F7FFFF00DECEC600525252000000000000000000000000000000
      0000000000000063000010B5390010A52900089C2100089C210000630000004A
      0000000000000000000000000000000000000000000029ADD60094F7FF0029AD
      D600ADDEEF00A5EFF700A5EFF700A5F7FF008CEFFF008CEFFF008CEFFF0073DE
      F7000073080018A5C600000000000000000000000000BD7B7300B5525200B55A
      5A00C6848400D6A5A500D6ADAD00D6ADA500D6ADAD00D6A5A500D6A5A500D6AD
      A500D6ADAD00D69C9C00CE636300AD525200F7EFE700EFDED600C65A2100CE63
      3100CE633100CE5A2100C6846B00FFFFFF00EFAD9400C64A1000CE633100CE63
      2900C65A2900F7EFEF00EFDED600525252000000000000000000000000000000
      0000000000000063000018B54A0010AD390010AD390010AD390010A529000063
      0000005200000000000000000000000000000000000029ADD6009CFFFF0073DE
      F70029ADD60018A5C60018A5C60018A5C600ADDEEF008CF7FF0084EFFF000073
      08005AE78C000073080018A5C6000000000000000000BD7B7300AD524A00E7CE
      CE00F7F7F700F7F7EF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F700DEBDBD00C65A5A00AD525A00F7EFE700F7DECE00CE5A2100CE63
      3100CE633100CE632900C6522100CEB5A500FFFFFF00E79C7B00C6521800CE63
      2900C65A2900F7EFEF00EFDED600525252000000000000000000000000000000
      0000000000000063000052BD52005ABD52004ABD52004ABD52004ABD52000063
      0000005200000000000000000000000000000000000029ADD6009CFFFF0094F7
      FF0073DEF70073DEF70073DEF7006BDEF70029ADD600ADDEEF000073080052D6
      7B0042D66B0031C64A00007308000000000000000000BD7B7300B5524A00EFD6
      D600FFF7F700F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EF
      EF00F7F7F700DEBDBD00C65A5A00AD525A00F7EFE700F7EFE700DE6B3100D66B
      3100CE632900C65A2100C6521800BD4A1000DECEC600FFFFFF00D6734200CE5A
      2100CE6B3900FFF7F700EFDED6006B6B6B000000000000000000000000000000
      000000000000006300005AC65A006BC66B006BC663004ABD520000630000004A
      0000000000000000000000000000000000000000000029ADD6009CFFFF0094F7
      FF0094F7FF0094F7FF0094F7FF0073DEF70073DEF70029ADD60018A5C600108C
      210031C64A00109C210018A5C6000000000000000000BD7B7300B5524A00EFD6
      D600EFEFEF00D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600EFEFEF00DEBDBD00C65A5A00AD525A00F7E7E700FFFFFF00F79C6B00E763
      2900CE8C6B00EFE7DE00D67B5200BD310000D69C7B00FFFFFF00DE8C6300CE52
      1800E79C7300FFFFFF00DEC6BD00ADADAD000000000000000000000000000000
      000000000000006300006BC66B007BD67B007BD67B0000630000004A00000000
      0000000000000000000000000000000000000000000029ADD600C6FFFF0094FF
      FF009CFFFF00D6FFFF00D6FFFF008CEFFF0094EFFF0073DEF70073DEF7000884
      100018AD290008841000000000000000000000000000BD7B7300B5524A00EFD6
      D600EFEFEF00DED6D600DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DED6
      D600EFEFEF00DEBDBD00C65A5A00AD525A00F7EFE700F7EFEF00FFEFDE00FF8C
      4A00DE845A00EFFFFF00FFFFFF00E7BDA500F7FFFF00EFFFFF00E7733900E773
      3900FFEFEF00F7E7DE00A59C9400000000000000000000000000000000000000
      0000000000000063000063C663008CD68C0000630000004A0000000000000000
      0000000000000000000000000000000000000000000021ADD6009CDEEF00C6FF
      FF00C6FFFF009CDEEF0018ADD60018A5C60018A5C60018A5C60018A5C600088C
      100008A5180000000000000000000000000000000000BD7B7300B5524A00EFD6
      D600F7F7EF00E7DEDE00E7DEDE00E7DEDE00E7DEDE00E7DEDE00E7DEDE00E7DE
      DE00EFEFEF00DEBDBD00C65A5A00AD525A0000000000EFDED600FFFFFF00FFEF
      CE00FFB57300EFAD8400EFE7DE00EFF7F700EFE7DE00F7A57B00FF8C4A00FFDE
      CE00FFFFFF00EFDED600CECEC600000000000000000000000000000000000000
      0000000000000063000039AD390000630000004A000000000000000000000000
      000000000000000000000000000000000000000000000000000031B5DE0029AD
      D60018A5C60018A5C60000000000000000000000000000000000088C100008A5
      18000884100000000000000000000000000000000000BD7B7300B5524A00EFD6
      D600EFEFEF00D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6D600D6D6
      D600EFEFEF00DEBDBD00C65A5A00AD525A0000000000F7EFEF00EFDED600FFFF
      FF00FFFFFF00FFF7C600FFDEAD00FFCE9400FFCE9400FFD6AD00FFF7F700FFFF
      FF00EFDED600CECEC60000000000000000000000000000000000000000000000
      000000000000004A000000630000004A00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000730800087B0800088C1000088C1000087B
      08000000000000000000000000000000000000000000BD7B7300B5524A00E7D6
      CE00FFF7F700F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EF
      EF00FFF7F700DEBDBD00C65A5A00AD525A000000000000000000F7EFEF00EFDE
      D600F7E7E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFE7DE00EFDE
      D600DEDEDE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AD524A00CEB5
      B500D6D6D600CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECE
      CE00D6D6D600CEADAD00A54A4A0000000000000000000000000000000000F7EF
      E700F7F7EF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EFEF00F7EF
      E700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000F07FFFFF00000000
      F03FFC1F00000000F01FFC0F00000000F01FFC0700000000F01FFC0700000000
      F81FFC0700000000FC1FFE0700000000FFF7FF0700000000FFE3FFDF00000000
      FFC1FF8F00000000BFF7FF0700000000FFFFFFDF00000000DFEFFFFF00000000
      F6BFFFBF00000000FFFFFAFF00000000FFFFC001F00FFFFF8FFF8000E003FFFF
      807F8000C001F8FF800F80008001F87F800780008000F83F800780000000F81F
      800380000000F80F800380000000F807800180000000F807800180000000F80F
      800180000000F81F800380000001F83F800780008001F87FC3C780008003F8FF
      FE0F8000C007FFFFFFFFC001E00FFFFF00000000000000000000000000000000
      000000000000}
  end
  object dlgSaveLog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text File (.txt)|*.txt|Log File (.log)|*.log|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 85
    Top = 104
  end
  object FDGUIxScriptDialog1: TFDGUIxScriptDialog
    Provider = 'Forms'
    Options = [ssCallstack, ssAutoHide]
    Left = 149
    Top = 138
  end
end
