
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit StandardDSRESTUIUnit;

interface

uses
  System.SysUtils, System.Classes, DSRestExpertsUI, DSServerWebBrokerExpertsUI,
  ExpertsUIWizard;

type
  TStandardDSRESTUIModule = class(TDSRESTExpertsUIModule)
  private
    { Private declarations }
  protected
    procedure SetDefaults; override;
  public
    { Public declarations }
  end;

var
  StandardDSRESTUIModule: TStandardDSRESTUIModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TStandardDSRESTUIModule.SetDefaults;
begin
  // Modify wizard defaults
  inherited;
  WebServerProjectWizard.Caption := 'New Standard DataSnap REST Application';
end;

end.
