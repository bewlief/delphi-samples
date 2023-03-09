
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SampleExpertsReg;

interface

uses ToolsAPI, SysUtils;

procedure Register;

implementation

uses
  Classes, Windows, SampleExpertsCreatorsUnit, ExpertsRepository, SampleExpertsUIUnit,
  SampleExpertTypes, ExpertsProject, PlatformAPI
  ;


const
  sSampleProjectWizardID = 'Embarcadero.SampleProjectWizard';
  sAuthor = 'Embarcadero';
  sSampleProjectIcon = 'IconOne';
  sSampleFormIcon = 'IconTwo';
  sSampleTextFileIcon = 'IconThree';

  sSampleFormWizardID = 'Embarcadero.SampleFormWizard';
  sSampleTextFileWizardID = 'Embarcadero.SampleTextFileWizard';

resourcestring
  rsSampleWizardPage = 'SampleWizardPage';

  rsSampleProjectWizardName = 'SampleProjectWizardName';
  rsSampleProjectWizardComment = 'SampleProjectWizardComment';

  rsSampleFormWizardName = 'SampleFormWizardName';
  rsSampleFormWizardComment = 'SampleFormWizardComment';

  rsSampleTextFileWizardName = 'SampleTextFileWizardName';
  rsSampleTextFileWizardComment = 'SampleTextFileWizardComment';

  rsNotImplemented = 'Not implemented';

procedure Register;
var
  LProjectTypes: Array[TSampleApplicationType] of TExpertsProjectType;
begin
  Assert(Length(LProjectTypes) = 2);
  LProjectTypes[appVCl] := ptApplication;
  LProjectTypes[appConsole] := ptConsole;
  // Project wizards
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(sDelphiPersonality,
    rsSampleProjectWizardComment, rsSampleProjectWizardName, sSampleProjectWizardID, rsSampleWizardPage, sAuthor,
    procedure
    var
      LUIModule: TSampleExpertsUIModule;
    begin
      LUIModule := TSampleExpertsUIModule.Create(nil);
      try
        // TODO: flag to indicate that wizard should close or stay open when there is an error
        LUIModule.ExpertsProjectWizard1.Execute(
          procedure
          var
            LModule: TSampleExpertsModule;
          begin
            LModule := TSampleExpertsModule.Create(nil);
            try
              // Indicate which type of project to create
              LModule.ExpertsProject1.ProjectType := LProjectTypes[LUIModule.ApplicationType];
              LModule.SelectedFeatures := LUIModule.SelectedFeatures;
              LModule.FormCaption := LUIModule.FormCaption;
              LModule.AddControls := LUIModule.AddControls;
              try
                LModule.ExpertsProject1.CreateProject(sDelphiPersonality);
              except
                ApplicationHandleException(nil);
              end;
            finally
              LModule.Free;
            end;
          end);
      finally
        LUIModule.Free;
      end;
    end,
    function: Cardinal
    begin
      Result := LoadIcon(HInstance, sSampleProjectIcon)
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform),
    TArray<string>.Create()
    ) as IOTAWizard);



  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(sDelphiPersonality,
    rsSampleFormWizardComment, rsSampleFormWizardName, sSampleFormWizardID, rsSampleWizardPage, sAuthor,
    procedure
    var
      LUIModule: TSampleExpertsUIModule;
    begin
      LUIModule := TSampleExpertsUIModule.Create(nil);
      try
        // TODO: flag to indicate that wizard should close or stay open when there is an error
        LUIModule.ExpertsFormWizard1.Execute(
          procedure
          var
            LModule: TSampleExpertsModule;
          begin
            LModule := TSampleExpertsModule.Create(nil);
            try
              LModule.FormCaption := LUIModule.FormCaption;
              LModule.AddControls := LUIModule.AddControls;
              try
                LModule.ExpertsFormModule1.CreateModule(sDelphiPersonality);
              except
                ApplicationHandleException(nil);
              end;
            finally
              LModule.Free;
            end;
          end);
      finally
        LUIModule.Free;
      end;
    end,
    function: Cardinal
    begin
      Result := LoadIcon(hInstance, sSampleFormIcon)
    end
    ) as IOTAWizard);

  RegisterPackageWizard(TExpertsRepositoryModuleWizardWithProc.Create(sDelphiPersonality,
    rsSampleTextFileWizardComment, rsSampleTextFileWizardName, sSampleTextFileWizardID, rsSampleWizardPage, sAuthor,
    procedure
    var
      LModule: TSampleExpertsModule;
    begin
      LModule := TSampleExpertsModule.Create(nil);
      try
        LModule.ExpertsTextFile1.CreateModule;
      finally
        LModule.Free;
      end;
    end,
    function: Cardinal
    begin
      Result := LoadIcon(hInstance, sSampleTextFileIcon)
    end
    ) as IOTAWizard);

end;


end.
