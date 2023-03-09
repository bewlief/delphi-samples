
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
// Manage list of project features
unit SampleExpertsFeatures;

interface

uses
  SysUtils, Classes, SampleExpertTypes, WizardAPI, ExpertsUIWizard;

type
  TFeatureDescription = record
    Group: TFeature;
    Feature: TFeature;
    Name: string;
    Description: string;
    ApplicationTypes: TSampleApplicationTypes;
  end;

  const FFeatureDescriptions: array[0..4] of TFeatureDescription =
    ((Group: feFiles; Feature: feNone; Name: 'Files'; Description: 'Files description'; ApplicationTypes: [appVCL, appConsole]),
    (Group: feFiles; Feature: feUnit; Name: 'Unit'; Description: 'Unit description'; ApplicationTypes: [appVCL, appConsole]),
    (Group: feFiles; Feature: feForm; Name: 'Form'; Description: 'Form description'; ApplicationTypes: [appVCL]),
    (Group: feFiles; Feature: feDataModule; Name: 'DataModule'; Description: 'DataModule description'; ApplicationTypes: [appVCL, appConsole]),
    (Group: feFiles; Feature: feTextFile; Name: 'Text File'; Description: 'Text File description'; ApplicationTypes: [appVCL, appConsole])
    );

procedure AddFeature(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AFeatureDescription: TFeatureDescription);
procedure EnumFeatures(AFilter: TFunc<TFeatureDescription, Boolean>; AEnum: TProc<TFeatureDescription>);
procedure EnumApplicationFeatures(AType: TSampleApplicationType; AEnum: TProc<TFeatureDescription>);
procedure CheckFeature(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AFeature: TFeature; AChecked: Boolean);
procedure CheckFeatures(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AType: TSampleApplicationType; AFeatures: TFeatures);
procedure AddFeatures(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AType: TSampleApplicationType);
procedure UpdateFeatures(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AType: TSampleApplicationType;
  var AFeatures: TFeatures);

implementation

procedure AddFeature(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AFeatureDescription: TFeatureDescription);
begin
  if AFeatureDescription.Feature = feNone then
    with AFeatureDescription do
      AFeaturesPage.AddFeatureGroup(Integer(Group), Name, Description)
  else if AFeatureDescription.Group = feNone then
    with AFeatureDescription do
      AFeaturesPage.AddFeature(Integer(Feature), Name, Description)
  else
    with AFeatureDescription do
      AFeaturesPage.AddFeature(Integer(Group), Integer(Feature), Name, Description)
end;

procedure EnumFeatures(AFilter: TFunc<TFeatureDescription, Boolean>; AEnum: TProc<TFeatureDescription>);
var
  LDescription: TFeatureDescription;
begin
  for LDescription in FFeatureDescriptions do
    if AFilter(LDescription) then
    begin
      AEnum(LDescription);
    end;
end;

procedure EnumApplicationFeatures(AType: TSampleApplicationType; AEnum: TProc<TFeatureDescription>);
begin
  EnumFeatures(
    function(ADescription: TFeatureDescription): Boolean
    begin
      Result := (ADescription.ApplicationTypes * [AType]) <> []
    end, AEnum);
end;

procedure CheckFeature(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AFeature: TFeature; AChecked: Boolean);
begin
  AFeaturesPage.Checked[Integer(AFeature)] := AChecked;
end;

procedure CheckFeatures(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AType: TSampleApplicationType; AFeatures: TFeatures);
begin
  EnumApplicationFeatures(AType,
   procedure(ADescription: TFeatureDescription)
   begin
      if ADescription.Feature <> feNone then
        CheckFeature(AFeaturesPage, ADescription.Feature,
          (AFeatures * [ADescription.Feature]) <> []);
   end)
end;

procedure AddFeatures(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AType: TSampleApplicationType);
begin
  EnumApplicationFeatures(AType,
   procedure(ADescription: TFeatureDescription)
   begin
      AddFeature(AFeaturesPage, ADescription);
   end)
end;

procedure UpdateFeatures(AFeaturesPage: TCustomExpertsFeaturesWizardPage; AType: TSampleApplicationType;
  var AFeatures: TFeatures);
var
  LFeatures: TFeatures;
begin
  LFeatures := AFeatures;
  EnumApplicationFeatures(AType,
    procedure(ADescription: TFeatureDescription)
    begin
      if ADescription.Feature <> feNone then
      begin
        if AFeaturesPage.Checked[Integer(ADescription.Feature)] then
          Include(LFeatures, ADescription.Feature)
        else
          Exclude(LFeatures, ADescription.Feature)
      end;
    end);
  AFeatures := LFeatures;
end;


end.
