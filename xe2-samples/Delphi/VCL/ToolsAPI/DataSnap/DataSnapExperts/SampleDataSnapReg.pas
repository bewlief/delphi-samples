
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SampleDataSnapReg;

// Register wizards for creating datasnap servers.

interface

uses ToolsAPI, SysUtils;

procedure Register;

implementation

uses
  Classes, Windows, ExpertsRepository,
  ExpertsProject, PlatformAPI,
  StandardDataSnapCreatorsUnit, StandardDataSnapUIUnit,
  CustomizedDataSnapCreatorsUnit, CustomizedDataSnapUIUnit,
  StandardDSRESTCreatorsUnit, StandardDSRESTUIUnit,
  CustomizedDSRESTCreatorsUnit, CustomizedDSRESTUIUnit
  ;


const
  sAuthor = 'Embarcadero';
  sStandardDSStandAloneProjectWizardID = sAuthor + '.StandardDSStandAloneProjectWizard';
  sStandardDSStandAloneProjectIcon = 'IconOne';
  sCustomizedDSStandAloneProjectWizardID = sAuthor + '.CustomizedDSStandAloneProjectWizard';
  sCustomizedDSStandAloneProjectIcon = 'IconTwo';

  sStandardDSRESTProjectWizardID = sAuthor + '.StandardRESTServerProjectWizard';
  sStandardDSRESTProjectIcon = 'IconOne';
  sCustomizedDSRESTProjectWizardID = sAuthor + '.CustomizedRESTServerProjectWizard';
  sCustomizedDSRESTProjectIcon = 'IconTwo';

resourcestring
  rsSampleDSStandAloneWizardPage = 'DS Standalone samples';
  rsSampleDSRESTWizardPage = 'DS REST Samples';

  rsStandardDSStandAloneProjectWizardName = 'StandardDSStandAloneProjectWizardName';
  rsStandardDSStandAloneProjectWizardComment = 'StandardDSStandAloneProjectWizardComment';
  rsCustomizedDSStandAloneProjectWizardName = 'CustomizedDSStandAloneProjectWizardName';
  rsCustomizedDSStandAloneProjectWizardComment = 'CustomizedDSStandAloneProjectWizardComment';

  rsStandardDSRESTProjectWizardName = 'StandardDSRESTProjectWizardName';
  rsStandardDSRESTProjectWizardComment = 'StandardDSRESTProjectWizardComment';
  rsCustomizedDSRESTProjectWizardName = 'CustomizedDSRESTProjectWizardName';
  rsCustomizedDSRESTProjectWizardComment = 'CustomizedDSRESTProjectWizardComment';



procedure RegisterStandardDSStandAloneProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
    rsStandardDSStandAloneProjectWizardComment, rsStandardDSStandAloneProjectWizardName, sStandardDSStandAloneProjectWizardID, rsSampleDSStandAloneWizardPage, sAuthor,
    procedure
    var
      LUIModule: TStandardDataSnapUIModule;
    begin
      LUIModule := TStandardDataSnapUIModule.Create(nil);
      try
        LUIModule.DSStandAloneAppWizard.Execute(
          procedure
          var
            LModule: TStandardDataSnapCreatorsModule;
          begin
            LModule := TStandardDataSnapCreatorsModule.Create(nil);
            try
              // Set personality so the correct template files are used
                LModule.Personality := APersonality;
                // Indicate which type of project to create
                LModule.ProjectType := LUIModule.ProjectType;
                // Indicate port
                LModule.HTTPPort := LUIModule.HTTPPort;
                LModule.HTTPSPort := LUIModule.HTTPSPort;
                LModule.TCPIPPort := LUIModule.TCPIPPort;
                LModule.Features := LUIModule.Features;
                LModule.SelectedClassName := LUIModule.SelectedClassName;
                LModule.CertFileInfo := LUIModule.CertFileInfo;
                LModule.ProjectLocation := LUIModule.ProjectLocation;

               try
                 LModule.DSStandAloneProject.CreateProject(APersonality);
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
      Result := LoadIcon(HInstance, sStandardDSStandAloneProjectIcon)
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform),
    TArray<string>.Create()
    ) as IOTAWizard);
end;


procedure RegisterCustomizedDSStandAloneProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
    rsCustomizedDSStandAloneProjectWizardComment, rsCustomizedDSStandAloneProjectWizardName, sCustomizedDSStandAloneProjectWizardID, rsSampleDSStandAloneWizardPage, sAuthor,
    procedure
    var
      LUIModule: TCustomizedDataSnapUIModule;
    begin
      LUIModule := TCustomizedDataSnapUIModule.Create(nil);
      try
        LUIModule.DSStandAloneAppWizard.Execute(
          procedure
          var
            LModule: TCustomizedDataSnapCreatorsModule;
          begin
            LModule := TCustomizedDataSnapCreatorsModule.Create(nil);
            try
              // Set personality so the correct template files are used
                LModule.Personality := APersonality;
                // Indicate which type of project to create
                LModule.ProjectType := LUIModule.ProjectType;
                // Indicate port
                LModule.HTTPPort := LUIModule.HTTPPort;
                LModule.HTTPSPort := LUIModule.HTTPSPort;
                LModule.TCPIPPort := LUIModule.TCPIPPort;
                LModule.Features := LUIModule.Features;
                LModule.SelectedClassName := LUIModule.SelectedClassName;
                LModule.CertFileInfo := LUIModule.CertFileInfo;
                LModule.ProjectLocation := LUIModule.ProjectLocation;
               // Customization: comment text
                LModule.Comments := LUIModule.Comments;
              try
                LModule.DSStandAloneProject.CreateProject(APersonality);
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
      Result := LoadIcon(HInstance, sStandardDSStandAloneProjectIcon)
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform),
    TArray<string>.Create()
    ) as IOTAWizard);
end;

procedure RegisterStandardDSRESTProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
    rsStandardDSRESTProjectWizardComment, rsStandardDSRESTProjectWizardName, sStandardDSRESTProjectWizardID, rsSampleDSRESTWizardPage, sAuthor,
    procedure
    var
      LUIModule: TStandardDSRESTUIModule;
    begin
      LUIModule := TStandardDSRESTUIModule.Create(nil);
      try
        LUIModule.WebServerProjectWizard.Execute(
          procedure
          var
            LModule: TStandardDSRESTCreatorsModule;
          begin
            LModule := TStandardDSRESTCreatorsModule.Create(nil);
            try
              // Set personality so the correct template files are used
              LModule.Personality := APersonality;
              // Indicate which type of project to create
              LModule.ProjectType := LUIModule.ProjectType;
              // Indicate port
              LModule.HTTPPort := LUIModule.HTTPPort;
              LModule.HTTPS := LUIModule.HTTPS;
              LModule.Features := LUIModule.Features;
              LModule.SelectedClassName := LUIModule.SelectedClassName;
              LModule.CertFileInfo := LUIModule.CertFileInfo;
              LModule.ProjectLocation := LUIModule.ProjectLocation;
              LModule.WebProject.CreateProject(APersonality);
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
      Result := LoadIcon(HInstance, sStandardDSRESTProjectIcon)
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform),
    TArray<string>.Create()
    ) as IOTAWizard);
end;


procedure RegisterCustomizedDSRESTProjectWizard(const APersonality: string);
begin
  RegisterPackageWizard(TExpertsRepositoryProjectWizardWithProc.Create(APersonality,
    rsCustomizedDSRESTProjectWizardComment, rsCustomizedDSRESTProjectWizardName, sCustomizedDSRESTProjectWizardID, rsSampleDSRESTWizardPage, sAuthor,
    procedure
    var
      LUIModule: TCustomizedDSRESTUIModule;
    begin
      LUIModule := TCustomizedDSRESTUIModule.Create(nil);
      try
        LUIModule.WebServerProjectWizard.Execute(
          procedure
          var
            LModule: TCustomizedDSRESTCreatorsModule;
          begin
            LModule := TCustomizedDSRESTCreatorsModule.Create(nil);
            try
              // Set personality so the correct template files are used
              LModule.Personality := APersonality;
              // Indicate which type of project to create
              LModule.ProjectType := LUIModule.ProjectType;
              // Indicate port
              LModule.HTTPPort := LUIModule.HTTPPort;
              LModule.HTTPS := LUIModule.HTTPS;
              LModule.Features := LUIModule.Features;
              LModule.SelectedClassName := LUIModule.SelectedClassName;
              LModule.CertFileInfo := LUIModule.CertFileInfo;
              LModule.ProjectLocation := LUIModule.ProjectLocation;
              // Customizations
              LModule.Comments := LUIModule.Comments;
              LModule.FilesToAdd := LUIModule.FilesToAdd;
              LModule.WebProject.CreateProject(APersonality);
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
      Result := LoadIcon(HInstance, sStandardDSRESTProjectIcon)
    end,
    TArray<string>.Create(cWin32Platform, cWin64Platform),
    TArray<string>.Create()
    ) as IOTAWizard);
end;

procedure Register;
begin
  RegisterStandardDSStandAloneProjectWizard(sDelphiPersonality);
  RegisterStandardDSStandAloneProjectWizard(sCBuilderPersonality);
  RegisterCustomizedDSStandAloneProjectWizard(sDelphiPersonality);
  RegisterCustomizedDSStandAloneProjectWizard(sCBuilderPersonality);

  RegisterStandardDSRESTProjectWizard(sDelphiPersonality);
  RegisterStandardDSRESTProjectWizard(sCBuilderPersonality);
  RegisterCustomizedDSRESTProjectWizard(sDelphiPersonality);
  RegisterCustomizedDSRESTProjectWizard(sCBuilderPersonality);
end;





end.
