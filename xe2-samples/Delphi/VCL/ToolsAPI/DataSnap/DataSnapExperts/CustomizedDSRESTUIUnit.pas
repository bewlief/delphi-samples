
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomizedDSRESTUIUnit;

interface

uses
  System.SysUtils, System.Classes, DSRestExpertsUI, DSServerWebBrokerExpertsUI,
  ExpertsUIWizard, VCL.Forms;

type
  TCustomizedDSRESTUIModule = class(TDSRESTExpertsUIModule)
    CommentsWizardPage1: TExpertsFrameWizardPage;
    WelcomeWizardsPage1: TExpertsFrameWizardPage;
    AddFilesWizardPage: TExpertsFrameWizardPage;
    procedure CommentsWizardPage1FrameCreate(
      Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
      out AFrame: TFrame);
    procedure WelcomeWizardsPage1FrameCreate(
      Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
      out AFrame: TFrame);
    procedure AddFilesWizardPageFrameCreate(
      Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
      out AFrame: TFrame);
    procedure AddFilesWizardPageLeavingPage(Sender: TCustomExpertsWizardPage;
      PageTransition: TExpertsWizardPageTransition; var Allow: Boolean);
  private
    function GetComments: string;
    function GetAddFiles: TArray<string>;
  protected
    procedure SetDefaults; override;
    procedure EnablePages; override;
  public
    { Public declarations }
    property Comments: string read GetComments;
    property FilesToAdd: TArray<string> read GetAddFiles;
  end;

var
  CustomizedDSRESTUIModule: TCustomizedDSRESTUIModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses DSServerFeatures, DSServerFeatureManager,
  CustomizedFeatures,
  WelcomeFrameUnit,
  CommentFrameUnit,
  AddFilesFrameUnit,
  InetWiz, Dialogs;

procedure TCustomizedDSRESTUIModule.SetDefaults;
begin
  // Modify wizard defaults
  inherited;
  WebServerProjectWizard.Caption := 'New Customized DataSnap REST Application';
  // Indicate which features are check by default
  Features := Features + [cCustomFeatureCommentModule,  cCustomFeatureTimeStampModule,
    cCustomFeatureSampleMethods, cCustomFeatureAddFiles];
  // Customization:  Populate features page with custom features
  FeatureDescriptions := CustomFeatureDescriptions;
  SelectedClassName := TDSServerClassName.scComponent;
end;

procedure TCustomizedDSRESTUIModule.WelcomeWizardsPage1FrameCreate(
  Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
  out AFrame: TFrame);
begin
  AFrame := TWelcomeFrame.Create(AOwner);
  TWelcomeFrame(AFrame).Label1.Caption :=
  'Welcome to the "' + WebServerProjectWizard.Caption + '" Wizard';
end;

procedure TCustomizedDSRESTUIModule.AddFilesWizardPageFrameCreate(
  Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
  out AFrame: TFrame);
begin
  AFrame := TAddFilesFrame.Create(AOwner);

end;

procedure TCustomizedDSRESTUIModule.AddFilesWizardPageLeavingPage(
  Sender: TCustomExpertsWizardPage;
  PageTransition: TExpertsWizardPageTransition; var Allow: Boolean);
begin
  inherited;
  try
    case PageTransition of
      eptNext,
      eptFinish:
        (AddFilesWizardPage.Frame as TAddFilesFrame).Validate;
    end;
  except
    on E: Exception do
    begin
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Allow := False;
    end;

  end;

end;

procedure TCustomizedDSRESTUIModule.CommentsWizardPage1FrameCreate(
  Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
  out AFrame: TFrame);
begin
  AFrame := TCommentFrame.Create(AOwner);
end;

procedure TCustomizedDSRESTUIModule.EnablePages;
begin
  inherited;
  // Customization: Hide or show comments page
  WebServerProjectWizard.PageEnabled[CommentsWizardPage1] :=
    (Features * [cCustomFeatureCommentModule] <> []);
  WebServerProjectWizard.PageEnabled[AddFilesWizardPage] :=
    (Features * [cCustomFeatureAddFiles] <> []);
end;

function TCustomizedDSRESTUIModule.GetAddFiles: TArray<string>;
begin
  if AddFilesWizardPage.Frame <> nil then
    Result := TAddFilesFrame(AddFilesWizardPage.Frame).FileNames
  else
    SetLength(Result, 0);
end;

function TCustomizedDSRESTUIModule.GetComments: string;
begin
  if CommentsWizardPage1.Frame <> nil then
    Result := TCommentFrame(CommentsWizardPage1.Frame).Memo1.Lines.Text
  else
    Result := '';

end;

end.
