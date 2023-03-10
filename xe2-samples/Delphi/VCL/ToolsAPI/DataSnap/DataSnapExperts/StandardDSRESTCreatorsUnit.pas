
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit StandardDSRESTCreatorsUnit;

interface

uses
  System.SysUtils, System.Classes, DSRESTExpertsCreators,
  DSServerWebBrokerExpertsCreators, ExpertsTemplates, ExpertsProject,
  ExpertsModules;

type
  TStandardDSRESTCreatorsModule = class(TDSRESTExpertsCreatorsModule)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StandardDSRESTCreatorsModule: TStandardDSRESTCreatorsModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
