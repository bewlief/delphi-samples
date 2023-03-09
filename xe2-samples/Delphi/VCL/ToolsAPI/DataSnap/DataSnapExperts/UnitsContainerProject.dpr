
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program UnitsContainerProject;

// This project is just a unit container so that ancestor forms can be resolved.
// Do not compile

uses
  Vcl.Forms,
  DSServerExpertsCreators in '$(BDS)\source\experts\DSServerExpertsCreators.pas' {DSServerExpertsCreatorsModule: TDataModule},
  DSServerExpertsUI in '$(BDS)\source\experts\DSServerExpertsUI.pas' {DSServerExpertsUIModule: TDataModule},
  InetExpertsUI  in '$(BDS)\source\experts\InetExpertsUI.pas' {InetExpertsUIModule: TDataModule},
  InetExpertsCreators in '$(BDS)\source\experts\InetExpertsCreators.pas' {InetExpertsCreatorsModule: TDataModule},
  DSServerFeatures in '$(BDS)\source\experts\DSServerFeatures.pas',
  DSServerFeatureManager in '$(BDS)\source\experts\DSServerFeatureManager.pas',
  InetCertFilesWizardPage in '$(BDS)\source\experts\InetCertFilesWizardPage.pas' {InetCertFilesWizardFrame: TFrame},
  DSProjectLocationWizardPage in '$(BDS)\source\experts\DSProjectLocationWizardPage.pas' {DSProjectLocationWizardFrame: TFrame},
  DSServerWebBrokerExpertsCreators in '$(BDS)\source\experts\DSServerWebBrokerExpertsCreators.pas' {DSServerWebBrokerExpertsCreatorsModule: TDataModule},
  DSRESTExpertsCreators in '$(BDS)\source\experts\DSRESTExpertsCreators.pas' {DSRESTExpertsCreatorsModule: TDataModule},
  DSServerWebBrokerExpertsUI in '$(BDS)\source\experts\DSServerWebBrokerExpertsUI.pas' {DSServerWebBrokerExpertsUIModule: TDataModule},
  DSRESTExpertsUI in '$(BDS)\source\experts\DSRESTExpertsUI.pas' {DSRESTExpertsUIModule: TDataModule},
  DSServerMethodsExpertsCreators in '$(BDS)\source\experts\DSServerMethodsExpertsCreators.pas' {DSServerMethodsCreatorModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
