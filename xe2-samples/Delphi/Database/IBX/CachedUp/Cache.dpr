
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program Cache;

uses
  Forms,
  CachedUp in 'CachedUp.pas' {CacheDemoForm},
  About in 'About.pas' {AboutDialog},
  Errform in 'Errform.pas' {UpdateErrorForm},
  DataMod in 'DataMod.pas' {CacheData: TDataModule},
  Vcl.Themes in 'Vcl.Themes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCacheData, CacheData);
  Application.CreateForm(TCacheDemoForm, CacheDemoForm);
  Application.CreateForm(TUpdateErrorForm, UpdateErrorForm);
  Application.Run;
end.
