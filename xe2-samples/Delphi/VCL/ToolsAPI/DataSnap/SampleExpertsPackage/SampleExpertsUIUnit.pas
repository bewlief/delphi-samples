
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit SampleExpertsUIUnit;

interface

uses
  SysUtils, Classes, ExpertsUIWizard, Vcl.Forms, SampleExpertTypes, Windows, Vcl.Graphics,
    SampleFormOptionsFrame;

type

  TSampleExpertsUIModule = class(TDataModule)
    ExpertsProjectWizard1: TExpertsWizard;
    ApplicationTypeWizardPage1: TExpertsFrameWizardPage;
    FormOptionsFrameWizardPage: TExpertsFrameWizardPage;
    ConsoleFeaturesWizardPage: TExpertsFeaturesWizardPage;
    VCLFeaturesWizardPage: TExpertsFeaturesWizardPage;
    ExpertsFormWizard1: TExpertsWizard;
    procedure ApplicationTypeWizardPage1CreateFrame(Sender: TCustomExpertsFrameWizardPage;
      AOwner: TComponent; out AFrame: TFrame);
    procedure ConsoleFeaturesWizardPageWizardPageCreated(
      Sender: TCustomExpertsWizardPage);
    procedure FormOptionsFrameWizardPageCreateFrame(
      Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
      out AFrame: TFrame);
    procedure DataModuleCreate(Sender: TObject);
    procedure VCLFeaturesWizardPageWizardPageCreated(
      Sender: TCustomExpertsWizardPage);
    procedure ConsoleFeaturesWizardPageFeatureChecked(
      AFeaturesWizardPage: TCustomExpertsFeaturesWizardPage);
    procedure VCLFeaturesWizardPageFeatureChecked(
      AFeaturesWizardPage: TCustomExpertsFeaturesWizardPage);
    procedure ConsoleFeaturesWizardPageEnterPage(
      Sender: TCustomExpertsWizardPage;
      PageTransition: TExpertsWizardPageTransition);
    procedure VCLFeaturesWizardPageEnterPage(Sender: TCustomExpertsWizardPage;
      PageTransition: TExpertsWizardPageTransition);
    procedure ExpertsProjectWizard1LoadImage(Sender: TCustomExpertsWizard;
      var AImage: TBitmap);
    procedure ExpertsProjectWizard1EnterPage(Sender: TCustomExpertsWizard; LastPage,
      CurrentPage: TCustomExpertsWizardPage;
      PageTransition: TExpertsWizardPageTransition);
    procedure ApplicationTypeWizardPage1UpdateInfo(
      Sender: TCustomExpertsWizardPage);
  private
    FApplicationType: TSampleApplicationType;
    FFeatures: TFeatures;
    procedure OnApplicationTypeChange(Sender: TObject);
    procedure EnablePages;
    function GetSelectedFeatures: TFeatures;
    function GetAddControls: Boolean;
    function GetFormCaption: string;
    function GetSampleFrame: TFormOptionsFrame;
    { Private declarations }
  public
    { Public declarations }
    property ApplicationType: TSampleApplicationType read FApplicationType;
    property SelectedFeatures: TFeatures read GetSelectedFeatures;
    property FormCaption: string read GetFormCaption;
    property AddControls: Boolean read GetAddControls;
  end;

var
  SampleExpertsUIModule: TSampleExpertsUIModule;

implementation

{$R *.dfm}

uses  SampleAppTypeFrameUnit, SampleExpertsFeatures;


procedure TSampleExpertsUIModule.DataModuleCreate(Sender: TObject);
begin
  FApplicationType := appVCl;
  FFeatures := [feUnit, feForm, feDataModule];
  EnablePages;
end;

// Indicate pages can be shown
procedure TSampleExpertsUIModule.EnablePages;
begin
  ExpertsProjectWizard1.PageEnabled[VCLFeaturesWizardPage] := FApplicationType = appVCl;
  ExpertsProjectWizard1.PageEnabled[ConsoleFeaturesWizardPage] := FApplicationType = appConsole;
  ExpertsProjectWizard1.PageEnabled[FormOptionsFrameWizardPage] := feForm in SelectedFeatures;
end;

procedure TSampleExpertsUIModule.ApplicationTypeWizardPage1UpdateInfo(
  Sender: TCustomExpertsWizardPage);
begin
  case FApplicationType of
    appVCL:
      Sender.WizardInfo := 'Create VCL Application';
    appConsole:
      Sender.WizardInfo := 'Create Console Application';
  else
    Sender.WizardInfo := '';
  end;
end;

procedure TSampleExpertsUIModule.ConsoleFeaturesWizardPageEnterPage(
  Sender: TCustomExpertsWizardPage;
  PageTransition: TExpertsWizardPageTransition);
begin
  CheckFeatures(TExpertsFeaturesWizardPage(Sender),
    appConsole, FFeatures);
end;

procedure TSampleExpertsUIModule.ConsoleFeaturesWizardPageFeatureChecked(
  AFeaturesWizardPage: TCustomExpertsFeaturesWizardPage);
begin
  UpdateFeatures(AFeaturesWizardPage, appConsole, FFeatures);
end;

procedure TSampleExpertsUIModule.ConsoleFeaturesWizardPageWizardPageCreated(
  Sender: TCustomExpertsWizardPage);
begin
  AddFeatures(TExpertsFeaturesWizardPage(Sender), appConsole);
  CheckFeatures(TExpertsFeaturesWizardPage(Sender),
    appConsole, FFeatures);
end;

procedure TSampleExpertsUIModule.ApplicationTypeWizardPage1CreateFrame(
  Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent; out AFrame: TFrame);
begin
  AFrame := TApplicationTypeFrame.Create(AOwner);
  TApplicationTypeFrame(AFrame).ApplicationType := FApplicationType;
  TApplicationTypeFrame(AFrame).OnApplicationTypeChange := OnApplicationTypeChange;
end;

procedure TSampleExpertsUIModule.OnApplicationTypeChange(Sender: TObject);
begin
  FApplicationType := TApplicationTypeFrame(ApplicationTypeWizardPage1.Frame).ApplicationType;
  EnablePages;
  ApplicationTypeWizardPage1.UpdateInfo;
end;

procedure TSampleExpertsUIModule.VCLFeaturesWizardPageEnterPage(
  Sender: TCustomExpertsWizardPage;
  PageTransition: TExpertsWizardPageTransition);
begin
  // TODO; Never called in feature page, remove this event
  CheckFeatures(TExpertsFeaturesWizardPage(Sender),
    appVCL, FFeatures);
end;

procedure TSampleExpertsUIModule.VCLFeaturesWizardPageFeatureChecked(
  AFeaturesWizardPage: TCustomExpertsFeaturesWizardPage);
begin
  UpdateFeatures(AFeaturesWizardPage, appVCL, FFeatures);
  EnablePages;
end;

procedure TSampleExpertsUIModule.VCLFeaturesWizardPageWizardPageCreated(
  Sender: TCustomExpertsWizardPage);
begin
  AddFeatures(TExpertsFeaturesWizardPage(Sender), appVCL);
  CheckFeatures(TExpertsFeaturesWizardPage(Sender),
    appVCL, FFeatures);
end;

procedure TSampleExpertsUIModule.FormOptionsFrameWizardPageCreateFrame(
  Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
  out AFrame: TFrame);
begin
  //Create the same frame (just for testing)
  AFrame := TFormOptionsFrame.Create(AOwner);
end;

procedure TSampleExpertsUIModule.ExpertsProjectWizard1EnterPage(
  Sender: TCustomExpertsWizard; LastPage, CurrentPage: TCustomExpertsWizardPage;
  PageTransition: TExpertsWizardPageTransition);
begin
  if CurrentPage = ConsoleFeaturesWizardPage then
    CheckFeatures(TExpertsFeaturesWizardPage(CurrentPage), appConsole,
      FFeatures);
  if CurrentPage = VCLFeaturesWizardPage then
    CheckFeatures(TExpertsFeaturesWizardPage(CurrentPage), appVCL,
      FFeatures);
end;

function TSampleExpertsUIModule.GetSampleFrame: TFormOptionsFrame;
begin
  Result := TFormOptionsFrame(FormOptionsFrameWizardPage.Frame);
  Assert((Result = nil) or Result.InheritsFrom(TFormOptionsFrame));
end;

procedure TSampleExpertsUIModule.ExpertsProjectWizard1LoadImage(
  Sender: TCustomExpertsWizard; var AImage: TBitmap);
begin
  AImage := TBitmap.Create;
  try
    AImage.LoadFromResourceName(HInstance, 'SampleWizardPanel');
  except
    FreeAndNil(AImage);
  end;
end;

function TSampleExpertsUIModule.GetAddControls: Boolean;
begin
  if GetSampleFrame <> nil then
    Result := GetSampleFrame.CheckBoxAddControls.Checked
  else
    Result := False;
end;

function TSampleExpertsUIModule.GetFormCaption: string;
begin
  if GetSampleFrame <> nil then
    Result := GetSampleFrame.EditFormCaption.Text;

end;

function TSampleExpertsUIModule.GetSelectedFeatures: TFeatures;
var
  LFeatures: TFeatures;
begin
  LFeatures := [];
  EnumApplicationFeatures(ApplicationType,
   procedure(ADescription: TFeatureDescription)
   begin
      if ADescription.Feature <> feNone then
        if ADescription.Feature in FFeatures then
          Include(LFeatures, ADescription.Feature);
   end);
   Result := LFeatures;
end;

end.
