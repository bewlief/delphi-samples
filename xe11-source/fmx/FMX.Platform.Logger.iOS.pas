{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Logger.iOS;

interface

{$SCOPEDENUMS ON}

uses
  FMX.Platform;

type
  /// <summary>Logger service implementation for iOS</summary>
  TiOSLoggerService = class(TInterfacedObject, IFMXLoggingService)
  private
    procedure RegisterService;
    procedure UnregisterService;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXLoggingService }
    procedure Log(const AFormat: string; const AParams: array of const);
  end;
  TCocoaTouchLoggerService = TiOSLoggerService;

implementation

uses
  System.SysUtils, Macapi.Helpers, Macapi.ObjectiveC, iOSapi.Foundation;

{ TiOSLoggerService }

constructor TiOSLoggerService.Create;
begin
  inherited;
  RegisterService;
end;

destructor TiOSLoggerService.Destroy;
begin
  UnregisterService;
  inherited;
end;

procedure TiOSLoggerService.Log(const AFormat: string; const AParams: array of const);
var
  Message: string;
begin
  if Length(AParams) = 0 then
    Message := AFormat
  else
    Message := Format(AFormat, AParams);
  NSLog(StringToId(Message));
end;

procedure TiOSLoggerService.RegisterService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXLoggingService) then
    TPlatformServices.Current.AddPlatformService(IFMXLoggingService, Self);
end;

procedure TiOSLoggerService.UnregisterService;
begin
  if TPlatformServices.Current <> nil then
    TPlatformServices.Current.RemovePlatformService(IFMXLoggingService);
end;

end.
