
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program AnimationDemo3D;

uses
  FMX.Forms,
  anidemofrm in 'anidemofrm.pas' {frmAniDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAniDemo, frmAniDemo);
  Application.Run;
end.
