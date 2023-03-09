{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Device.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Devices, FMX.Platform;

type
  /// <summary>Implementation of <c>IFMXDeviceService</c> for Windows.</summary>
  TWinDeviceServices = class(TInterfacedObject, IFMXDeviceService)
  public
    { IFMXDeviceService }
    function GetModel: string;
    function GetFeatures: TDeviceFeatures;
    function GetDeviceClass: TDeviceInfo.TDeviceClass;
  end;

implementation

uses
  System.SysUtils, WinApi.Windows;

{ TWinDeviceServices }

function TWinDeviceServices.GetDeviceClass: TDeviceInfo.TDeviceClass;
begin
  if (GetSystemMetrics(SM_TABLETPC) <> 0) and (TDeviceFeature.HasTouchScreen in GetFeatures) then
    Result := TDeviceInfo.TDeviceClass.Tablet
  else
    Result := TDeviceInfo.TDeviceClass.Desktop;
end;

function TWinDeviceServices.GetFeatures: TDeviceFeatures;
var
  Value: Integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
  if ((Value and NID_READY) = NID_READY) and ((Value and NID_MULTI_INPUT) = NID_MULTI_INPUT) then
    Result := [TDeviceFeature.HasTouchScreen]
  else
    Result := [];
end;

function TWinDeviceServices.GetModel: string;
begin
  Result := string.Empty;
end;

end.
