
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program Clients;

uses
  FireDAC.Stan.Factory,
  Forms,
  dmMainBase in '..\..\dmMainBase.pas' {dmlMainBase: TDataModule},
  fClients in 'fClients.pas' {frmClients},
  fMainBase in '..\..\fMainBase.pas' {frmMainBase},
  fMainConnectionDefBase in '..\..\fMainConnectionDefBase.pas' {frmMainConnectionDefBase},
  fMainLayers in '..\..\fMainLayers.pas' {frmMainLayers};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmlMainBase, dmlMainBase);
  Application.CreateForm(TfrmClients, frmClients);
  Application.Run;
end.
