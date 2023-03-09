
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomizedDataSnapUIUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DSServerExpertsUI, ExpertsUIWizard;

type
  TCustomizedDataSnapUIModule = class(TDSServerExpertsUIModule)
    CommentsWizardPage1: TExpertsFrameWizardPage;
    WelcomeWizardsPage1: TExpertsFrameWizardPage;
    procedure CommentsWizardPage1FrameCreate(
      Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
      out AFrame: TFrame);
    procedure WelcomeWizardsPage1FrameCreate(
      Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
      out AFrame: TFrame);
  private
    function GetComments: string;
  protected
    procedure EnablePages; override;
    procedure SetDefaults; override;
    { Private declarations }
  public
    { Public declarations }
    property Comments: string read GetComments;
  end;

var
  CustomizedDataSnapUIModule: TCustomizedDataSnapUIModule;

implementation

{$R *.dfm}

uses DSServerFeatures, DSServerFeatureManager,
  CustomizedFeatures,
  WelcomeFrameUnit,
  CommentFrameUnit;


procedure TCustomizedDataSnapUIModule.SetDefaults;
begin
  // Modify wizard defaults
  inherited;
  DSStandAloneAppWizard.Caption := 'New Customized DataSnap Server';
  // Specify which features to check by default
  Features := Features + [cCustomFeatureCommentModule,
      cCustomFeatureTimeStampModule,
      cCustomFeatureSampleMethods];
  // Uncomment to uncheck TCPProtocol by default
  // Features := Features - [dsTCPProtocol];

  // Populate features page with custom features
  FeatureDescriptions := CustomFeatureDescriptions;
end;

procedure TCustomizedDataSnapUIModule.WelcomeWizardsPage1FrameCreate(
  Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
  out AFrame: TFrame);
begin
  AFrame := TWelcomeFrame.Create(AOwner);
  TWelcomeFrame(AFrame).Label1.Caption :=
  'Welcome to the "' + DSStandAloneAppWizard.Caption + '" Wizard'

end;

procedure TCustomizedDataSnapUIModule.CommentsWizardPage1FrameCreate(
  Sender: TCustomExpertsFrameWizardPage; AOwner: TComponent;
  out AFrame: TFrame);
begin
  AFrame := TCommentFrame.Create(AOwner);
end;

procedure TCustomizedDataSnapUIModule.EnablePages;
begin
  inherited;
  // Customization: Hide or show comments page
  DSStandAloneAppWizard.PageEnabled[CommentsWizardPage1] :=
    (Features * [cCustomFeatureCommentModule] <> []);
end;

function TCustomizedDataSnapUIModule.GetComments: string;
begin
  if CommentsWizardPage1.Frame <> nil then
    Result := TCommentFrame(CommentsWizardPage1.Frame).Memo1.Lines.Text
  else
    Result := '';

end;

end.
