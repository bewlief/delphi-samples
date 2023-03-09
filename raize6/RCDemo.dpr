program RCDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain};

{$R *.res}
{$R RCDemoResources.res}  // Link in Manifest and Version Information

begin
  Application.Title := 'Raize Components 6 Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
