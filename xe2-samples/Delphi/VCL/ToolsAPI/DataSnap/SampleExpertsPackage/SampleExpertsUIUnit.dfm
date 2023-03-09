object SampleExpertsUIModule: TSampleExpertsUIModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 254
  Width = 331
  object ExpertsProjectWizard1: TExpertsWizard
    WizardPages = <
      item
        WizardPage = ApplicationTypeWizardPage1
      end
      item
        WizardPage = ConsoleFeaturesWizardPage
      end
      item
        WizardPage = VCLFeaturesWizardPage
      end
      item
        WizardPage = FormOptionsFrameWizardPage
      end
      item
      end
      item
      end>
    Caption = 'Sample Project Wizard'
    HelpContext = 0
    OnEnterPage = ExpertsProjectWizard1EnterPage
    OnLoadImage = ExpertsProjectWizard1LoadImage
    Left = 48
    Top = 24
  end
  object ApplicationTypeWizardPage1: TExpertsFrameWizardPage
    OnFrameCreate = ApplicationTypeWizardPage1CreateFrame
    Title = 'Application Type'
    Description = 'Choose an application type'
    OnUpdateInfo = ApplicationTypeWizardPage1UpdateInfo
    Left = 64
    Top = 88
  end
  object FormOptionsFrameWizardPage: TExpertsFrameWizardPage
    OnFrameCreate = FormOptionsFrameWizardPageCreateFrame
    Title = 'Form Options Page'
    Description = 'Form Options Page Description'
    Left = 184
    Top = 16
  end
  object ConsoleFeaturesWizardPage: TExpertsFeaturesWizardPage
    Title = 'Console Features Page'
    Description = 'Console Features Page Description'
    OnFeatureChecked = ConsoleFeaturesWizardPageFeatureChecked
    OnWizardPageCreated = ConsoleFeaturesWizardPageWizardPageCreated
    OnEnterPage = ConsoleFeaturesWizardPageEnterPage
    Left = 232
    Top = 80
  end
  object VCLFeaturesWizardPage: TExpertsFeaturesWizardPage
    Title = 'VCL Features Page'
    Description = 'VCL Features Page Description'
    OnFeatureChecked = VCLFeaturesWizardPageFeatureChecked
    OnWizardPageCreated = VCLFeaturesWizardPageWizardPageCreated
    OnEnterPage = VCLFeaturesWizardPageEnterPage
    Left = 240
    Top = 136
  end
  object ExpertsFormWizard1: TExpertsWizard
    WizardPages = <
      item
        WizardPage = FormOptionsFrameWizardPage
      end>
    Caption = 'Sample Form Wizard'
    HelpContext = 0
    Left = 48
    Top = 168
  end
end
