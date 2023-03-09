inherited CustomizedDSRESTUIModule: TCustomizedDSRESTUIModule
  OldCreateOrder = False
  Height = 353
  Width = 542
  inherited WebServerProjectWizard: TExpertsWizard
    WizardPages = <
      item
        WizardPage = WelcomeWizardsPage1
      end
      item
        WizardPage = ApplicationTypeWizardPage1
      end
      item
        WizardPage = PortsWizardPage
      end
      item
        WizardPage = CertFilesWizardPage
      end
      item
        WizardPage = FeaturesWizardPage
      end
      item
        WizardPage = CommentsWizardPage1
      end
      item
        WizardPage = AddFilesWizardPage
      end
      item
        WizardPage = ServerClassWizardPage
      end
      item
        WizardPage = LocationWizardPage
      end>
  end
  object CommentsWizardPage1: TExpertsFrameWizardPage
    OnFrameCreate = CommentsWizardPage1FrameCreate
    Description = 'Comments Page'
    Left = 424
    Top = 192
  end
  object WelcomeWizardsPage1: TExpertsFrameWizardPage
    OnFrameCreate = WelcomeWizardsPage1FrameCreate
    Description = 'Welcome Page'
    Left = 424
    Top = 240
  end
  object AddFilesWizardPage: TExpertsFrameWizardPage
    OnFrameCreate = AddFilesWizardPageFrameCreate
    Description = 'Select files to add to the project'
    OnLeavingPage = AddFilesWizardPageLeavingPage
    Left = 424
    Top = 296
  end
end
