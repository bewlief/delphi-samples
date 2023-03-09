
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit StandardDataSnapUIUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DSServerExpertsUI, ExpertsUIWizard;

type
  TStandardDataSnapUIModule = class(TDSServerExpertsUIModule)
  private
    { Private declarations }
  public
    procedure SetDefaults; override;
    { Public declarations }
  end;

var
  StandardDataSnapUIModule: TStandardDataSnapUIModule;

implementation

{$R *.dfm}

procedure TStandardDataSnapUIModule.SetDefaults;
begin
  // Modify wizard defaults
  inherited;
  DSStandAloneAppWizard.Caption := 'New Standard DataSnap Server';
end;

end.
