object SampleExpertsModule: TSampleExpertsModule
  OldCreateOrder = False
  Height = 285
  Width = 544
  object ExpertsProject1: TExpertsProject
    ProjectType = ptApplication
    ProjectTemplateFile = ExpertsProjectFile1
    OnCreateModules = ExpertsProject1CreateModules
    Left = 64
    Top = 48
  end
  object ExpertsProjectFile1: TExpertsTemplateFile
    TemplateDoc.Strings = (
      '[!outputoff]'
      '[!if=(ProjectType, "Application")]'
      '[!outputon]'
      '// Sample Application Project'
      '// Created by SampleProjectWizard'
      'program [!ProjectName];'
      ''
      'uses'
      '  Forms;'
      ''
      '{$R *.res}'
      ''
      'begin'
      '  Application.Initialize;'
      '  Application.MainFormOnTaskbar := True;'
      '  Application.Run;'
      'end.'
      '[!outputoff]'
      '[!endif]'
      ''
      ''
      '[!if=(ProjectType, "Console")]'
      '[!outputon]'
      '// Sample Console Project'
      '// Created by SampleProjectWizard'
      'program [!ProjectName];'
      ''
      '{$APPTYPE CONSOLE}'
      ''
      'uses'
      '  SysUtils;'
      ''
      'begin'
      '  try'
      '    { TODO -oUser -cConsole Main : Insert code here }'
      '  except'
      '    on E: Exception do'
      '      Writeln(E.ClassName, '#39': '#39', E.Message);'
      '  end;'
      'end.'
      '[!outputoff]'
      '[!endif]'
      ''
      '')
    TemplateProperties = ExpertsProperties1
    Left = 232
    Top = 16
  end
  object ExpertsProperties1: TExpertsTemplateProperties
    Properties.Strings = (
      'ProjectType=Application')
    Left = 408
    Top = 64
  end
  object ExpertsFormModule1: TExpertsModule
    FormTemplateFile = ExpertsFormFile1
    AncestorName = 'Form'
    Left = 64
    Top = 184
  end
  object ExpertsDataModule1: TExpertsModule
    AncestorName = 'DataModule'
    Left = 200
    Top = 168
  end
  object ExpertsUnit1: TExpertsUnit
    Left = 344
    Top = 168
  end
  object ExpertsTextFile1: TExpertsTextFile
    FileName = 'text'
    FileNameExt = '.txt'
    Left = 432
    Top = 208
  end
  object ExpertsFormFile1: TExpertsTemplateFile
    TemplateDoc.Strings = (
      '[!outputoff]'
      ''
      ''
      '[!if=(FileType, "Source")]'
      '[!outputon]'
      '// Sample Form'
      'unit [!ModuleIdent];'
      ''
      'interface'
      ''
      'uses'
      
        '  Windows, Messages, SysUtils, Variants, Classes, Graphics, Cont' +
        'rols, Forms,'
      '  Dialogs, StdCtrls;'
      ''
      'type'
      '  T[!FormIdent] = class(T[!AncestorIdent])'
      '[!if=(AddControls)]'
      '    ListBox1: TListBox;'
      '    Edit1: TEdit;'
      '    Button1: TButton;'
      '    procedure Button1Click(Sender: TObject);'
      '[!endif]'
      '  private'
      '    { Private declarations }'
      '  public'
      '    { Public declarations }'
      '  end;'
      ''
      'var'
      '  [!FormIdent]: T[!FormIdent];'
      ''
      'implementation'
      ''
      '{$R *.dfm}'
      ''
      '[!if=(AddControls)]'
      'procedure T[!FormIdent].Button1Click(Sender: TObject);'
      'begin'
      '  ListBox1.Items.Add(Edit1.Text)'
      'end;'
      ''
      '[!endif]'
      ''
      'end.'
      '[!outputoff]'
      '[!endif]'
      ''
      '[!if=(FileType, "Form")]'
      '[!outputon]'
      'object [!FormIdent]: T[!FormIdent]'
      '  Left = 0'
      '  Top = 0'
      '  Caption = '#39'[!FormCaption]'#39
      '  ClientHeight = 337'
      '  ClientWidth = 527'
      '  Color = clBtnFace'
      '  Font.Charset = DEFAULT_CHARSET'
      '  Font.Color = clWindowText'
      '  Font.Height = -11'
      '  Font.Name = '#39'Tahoma'#39
      '  Font.Style = []'
      '  OldCreateOrder = False'
      '  PixelsPerInch = 96'
      '  TextHeight = 13'
      '[!if=(AddControls)]'
      '  object ListBox1: TListBox'
      '    Left = 16'
      '    Top = 24'
      '    Width = 121'
      '    Height = 137'
      '    ItemHeight = 13'
      '    TabOrder = 0'
      '  end'
      '  object Edit1: TEdit'
      '    Left = 16'
      '    Top = 167'
      '    Width = 121'
      '    Height = 21'
      '    TabOrder = 1'
      '    Text = '#39'Edit1'#39
      '  end'
      '  object Button1: TButton'
      '    Left = 16'
      '    Top = 194'
      '    Width = 75'
      '    Height = 25'
      '    Caption = '#39'Add'#39
      '    TabOrder = 2'
      '    OnClick = Button1Click'
      '  end'
      '[!endif]'
      'end'
      '[!outputoff]'
      '[!endif]'
      ''
      ''
      '')
    TemplateProperties = ExpertsProperties1
    Left = 192
    Top = 96
  end
end
