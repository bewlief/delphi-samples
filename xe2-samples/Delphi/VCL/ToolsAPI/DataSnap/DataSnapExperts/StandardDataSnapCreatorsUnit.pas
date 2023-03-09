
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit StandardDataSnapCreatorsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, DSServerExpertsCreators,
  ExpertsTemplates, ExpertsModules, ExpertsProject;

type
  TStandardDataSnapCreatorsModule = class(TDSServerExpertsCreatorsModule)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StandardDataSnapCreatorsModule: TStandardDataSnapCreatorsModule;

implementation

{$R *.dfm}

end.
