program RibbonDemo;
// Copyright (c) 1995-2010 Embarcadero Technologies, Inc.

// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.
uses
  Forms,
  RibbonDemoMainForm in 'RibbonDemoMainForm.pas' {frmRibbonDemo},
  Vcl.Themes in 'Vcl.Themes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRibbonDemo, frmRibbonDemo);
  Application.Run;
end.
