
//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program TetherDBClient;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  System.Tether.AppProfile in '..\..\..\System.Tether.AppProfile.pas',
  System.Tether.Consts in '..\..\..\System.Tether.Consts.pas',
  System.Tether.Manager in '..\..\..\System.Tether.Manager.pas',
  System.Tether.NetworkAdapter in '..\..\..\System.Tether.NetworkAdapter.pas',
  System.Tether.TCPProtocol in '..\..\..\System.Tether.TCPProtocol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
