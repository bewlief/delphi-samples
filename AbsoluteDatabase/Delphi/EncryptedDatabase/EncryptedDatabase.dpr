program EncryptedDatabase;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  uDatabase in 'uDatabase.pas' {frmDatabase};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDatabase, frmDatabase);
  Application.Run;
end.
