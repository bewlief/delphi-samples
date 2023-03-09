inherited CustomizedDataSnapUIModule: TCustomizedDataSnapUIModule
  OldCreateOrder = True
  inherited DSStandAloneAppWizard: TExpertsWizard
    WizardPages = <
      item
        WizardPage = WelcomeWizardsPage1
      end
      item
        WizardPage = ApplicationTypeWizardPage1
      end
      item
        WizardPage = DSStandAloneFeaturesWizardPage
      end
      item
        WizardPage = CommentsWizardPage1
      end
      item
        WizardPage = PortsWizardPage
      end
      item
        WizardPage = CertFilesWizardPage
      end
      item
        WizardPage = ServerClassWizardPage
      end
      item
        WizardPage = LocationWizardPage
      end>
  end
  inherited LocationWizardPage: TExpertsFrameWizardPage
    Left = 352
    Top = 120
  end
  object CommentsWizardPage1: TExpertsFrameWizardPage
    OnFrameCreate = CommentsWizardPage1FrameCreate
    Description = 'CommentsPage'
    Left = 288
    Top = 320
  end
  object WelcomeWizardsPage1: TExpertsFrameWizardPage
    OnFrameCreate = WelcomeWizardsPage1FrameCreate
    Description = 'Welcome Page'
    Left = 288
    Top = 272
  end
end
