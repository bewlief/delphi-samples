{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Logger.Android;

interface

{$SCOPEDENUMS ON}

uses
  FMX.Platform;

type
  /// <summary>Logger service implementation for Android</summary>
  TAndroidLoggerService = class(TInterfacedObject, IFMXLoggingService)
  public const
    PrefixFormat = 'FMX: %s: ';
  private
    FLogPrefix: string;
    procedure RegisterService;
    procedure UnregisterService;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXLoggingService }
    procedure Log(const AFormat: string; const AParams: array of const);
  end;

implementation

uses
  System.SysUtils, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.Log, Androidapi.Helpers;

{ TAndroidLoggerService }

constructor TAndroidLoggerService.Create;
begin
  inherited;
  FLogPrefix := Format(PrefixFormat, [TAndroidHelper.ApplicationTitle]);
  RegisterService;
  _AddRef;
end;

destructor TAndroidLoggerService.Destroy;
begin
  UnregisterService;
  inherited;
end;

procedure TAndroidLoggerService.Log(const AFormat: string; const AParams: array of const);
var
  Msg: string;
  M: TMarshaller;
begin
  if Length(AParams) = 0 then
    Msg := FLogPrefix + AFormat
  else
    Msg := Format(FLogPrefix + AFormat, AParams);
  LOGI(M.AsUtf8(Msg).ToPointer);
end;

procedure TAndroidLoggerService.RegisterService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXLoggingService) then
    TPlatformServices.Current.AddPlatformService(IFMXLoggingService, Self);
end;

procedure TAndroidLoggerService.UnregisterService;
begin
  if TPlatformServices.Current <> nil then
    TPlatformServices.Current.RemovePlatformService(IFMXLoggingService);
end;

end.
