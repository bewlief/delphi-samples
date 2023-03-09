
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SampleExpertsCreatorsUnit;

interface

uses
  SysUtils, Classes, ExpertsProject, ExpertsTemplates, ExpertsModules, SampleExpertTypes;

type
  TSampleExpertsModule = class(TDataModule)
    ExpertsProject1: TExpertsProject;
    ExpertsProjectFile1: TExpertsTemplateFile;
    ExpertsProperties1: TExpertsTemplateProperties;
    ExpertsFormModule1: TExpertsModule;
    ExpertsDataModule1: TExpertsModule;
    ExpertsUnit1: TExpertsUnit;
    ExpertsTextFile1: TExpertsTextFile;
    ExpertsFormFile1: TExpertsTemplateFile;
    procedure ExpertsProject1CreateModules(
      Sender: TCustomExpertsProject; const APersonality: string);
  private
    FSelectedFeatures: TFeatures;
    FAddControls: Boolean;
    FFormCaption: string;
    procedure SetSelectedFeatures(const Value: TFeatures);
    procedure SetAddControls(const Value: Boolean);
    procedure SetFormCaption(const Value: string);
    procedure UpdateProperties;
    { Private declarations }
  public
    { Public declarations }
    property SelectedFeatures: TFeatures read FSelectedFeatures write SetSelectedFeatures;
    property FormCaption: string read FFormCaption write SetFormCaption;
    property AddControls: Boolean read FAddControls write SetAddControls;
  end;

var
  SampleExpertsModule: TSampleExpertsModule;

implementation

{$R *.dfm}

procedure TSampleExpertsModule.ExpertsProject1CreateModules(
  Sender: TCustomExpertsProject; const APersonality: string);
begin
  UpdateProperties;
  if TFeature.feForm in FSelectedFeatures then
    ExpertsFormModule1.CreateModule(APersonality);
  if TFeature.feDataModule in FSelectedFeatures then
    ExpertsDataModule1.CreateModule(APersonality);
  if TFeature.feUnit in FSelectedFeatures then
    ExpertsUnit1.CreateModule(APersonality);
  if TFeature.feTextFile in FSelectedFeatures then
    ExpertsTextFile1.CreateModule;
end;



procedure TSampleExpertsModule.SetAddControls(const Value: Boolean);
begin
  FAddControls := Value;
  UpdateProperties;
end;

procedure TSampleExpertsModule.SetFormCaption(const Value: string);
begin
  FFormCaption := Value;
  UpdateProperties;
end;

procedure TSampleExpertsModule.SetSelectedFeatures(const Value: TFeatures);
begin
  FSelectedFeatures := Value;
end;

procedure  TSampleExpertsModule.UpdateProperties;
const
  sFalseTrue: array[false..true] of string = ('false', 'true');
begin
  ExpertsProperties1.Properties.Values['AddControls'] := sFalseTrue[AddControls];
  ExpertsProperties1.Properties.Values['FormCaption'] := FFormCaption;
end;

end.
