library AbsSqlConsole;
{$I CompVer.inc}

uses
  ActiveX,
  ComObj,
{$IFDEF D5H}
  WebBroker,
  ISAPIThreadPool,
{$ENDIF}
  HTTPApp,
  ISAPIApp,
  Main in 'Main.pas' {WebModule1: TWebModule};

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.CreateForm(TWebModule1, WebModule1);
  Application.Run;
end.
