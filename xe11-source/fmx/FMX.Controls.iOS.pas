{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Controls.iOS;

interface

{$SCOPEDENUMS ON}

implementation

uses
  System.SysUtils, FMX.Types, FMX.Styles{$IFDEF IOS}, iOSapi.UIKit, FMX.Helpers.iOS{$ENDIF};

const
  iOSLightStyle = 'iosstyle';
  iOSDarkStyle = 'ios13darkstyle';

{$R *.res}

function iOSStyleSelection(const APlatform: TOSPlatform): string;
begin
{$IFDEF WIN32}
  Result := iOSLightStyle;
{$ELSE}
  if GetUserInterfaceStyle = UIUserInterfaceStyleDark then
    Result := iOSDarkStyle
  else
    Result := iOSLightStyle;
{$ENDIF}
end;

initialization
  TStyleManager.RegisterPlatformStyleResource(TOSPlatform.iOS, iOSLightStyle);
  TStyleManager.RegisterPlatformStyleResource(TOSPlatform.iOS, iOSDarkStyle);
  TStyleManager.RegisterPlatformStyleSelection(TOSPlatform.iOS, iOSStyleSelection);
end.

