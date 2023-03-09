
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program LowLevel3DDemo;

uses
  FMX.Forms,
  LowLevel3DDemoMainFrm in 'LowLevel3DDemoMainFrm.pas' {fmxLowLevel3DDemoMain};

begin
  Application.Initialize;
  Application.CreateForm(TfmxLowLevel3DDemoMain, fmxLowLevel3DDemoMain);
  Application.Run;
end.
