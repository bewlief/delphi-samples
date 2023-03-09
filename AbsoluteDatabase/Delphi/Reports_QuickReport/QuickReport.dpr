program QuickReport;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  QReport in 'QReport.pas' {frmReport};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmReport, frmReport);
  Application.Run;
end.
