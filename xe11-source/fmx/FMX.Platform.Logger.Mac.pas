{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Logger.Mac;

interface

{$SCOPEDENUMS ON}

uses
  FMX.Platform;

type
  TMacLoggerService = class(TInterfacedObject, IFMXLoggingService)
  public
    { IFMXLoggingService }
    procedure Log(const AFormat: string; const AParams: array of const);
  end;

implementation

uses
  System.SysUtils;

{ TWinLoggerService }

procedure TMacLoggerService.Log(const AFormat: string; const AParams: array of const);
begin
  if Length(AParams) = 0 then
    WriteLn(AFormat)
  else
    WriteLn(Format(AFormat, AParams));
end;

end.
