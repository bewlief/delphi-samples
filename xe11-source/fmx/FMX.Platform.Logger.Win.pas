{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Logger.Win;

interface

{$SCOPEDENUMS ON}

uses
  FMX.Platform;

type
  TWinLoggerService = class(TInterfacedObject, IFMXLoggingService)
  public
    { IFMXLoggingService }
    procedure Log(const AFormat: string; const AParams: array of const);
  end;

implementation

uses
  System.SysUtils, Winapi.Windows;

{ TWinLoggerService }

procedure TWinLoggerService.Log(const AFormat: string; const AParams: array of const);
begin
  if Length(AParams) = 0 then
    OutputDebugString(PChar(AFormat))
  else
    OutputDebugString(PChar(Format(AFormat, AParams)));
end;

end.
