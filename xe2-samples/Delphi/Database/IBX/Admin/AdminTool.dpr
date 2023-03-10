
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program AdminTool;

uses
  Forms,
  frmAdminToolU in 'frmAdminToolU.pas' {frmAdminTool},
  frmUserInformationU in 'frmUserInformationU.pas' {frmUserInformation},
  frmLoginU in 'frmLoginU.pas' {frmLogin},
  frmAddCertificateU in 'frmAddCertificateU.pas' {frmAddCertificate},
  frmVerboseU in 'frmVerboseU.pas' {frmVerbose},
  Vcl.Themes in 'Vcl.Themes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAdminTool, frmAdminTool);
  Application.CreateForm(TfrmUserInformation, frmUserInformation);
  Application.Run;
end.
