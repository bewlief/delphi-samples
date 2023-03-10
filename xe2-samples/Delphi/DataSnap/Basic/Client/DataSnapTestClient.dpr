{*******************************************************}
{                                                       }
{       CodeGear Delphi Visual Component Library        }
{                                                       }
// Copyright (c) 1995-2010 Embarcadero Technologies, Inc.

// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.
{                                                       }
{*******************************************************}

program DataSnapTestClient;

uses
  SysUtils,
  Forms,
  ClientForm in 'ClientForm.pas' {Form13},
  ClientClasses in 'ClientClasses.pas',
  DataSnapTestData in '..\examples\DataSnapTestData.pas',
  Vcl.Themes in 'Vcl.Themes.pas';

{$R *.res}

begin
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TForm13, Form13);
  Application.Run;
  except on Ex: Exception do
    Application.ShowException(Ex);
  end;
end.
