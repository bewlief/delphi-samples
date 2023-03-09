program AbsSqlConsoleCGI;
{$I CompVer.inc}

{$APPTYPE CONSOLE}  // CGI application

uses
{$IFDEF D5H}
  WebBroker,
{$ENDIF}
  HTTPApp,
  CGIApp,
  Main in 'Main.pas' {WebModule1: TWebModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TWebModule1, WebModule1);
  Application.Run;
end.
