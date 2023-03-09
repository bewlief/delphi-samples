{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Metrics.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, FMX.Platform, FMX.Graphics;

type
  TWinMetricsServices = class(TInterfacedObject, IFMXDefaultMetricsService, IFMXSystemInformationService,
                              IFMXSystemFontService, IFMXLocaleService, IFMXListingService)
  public const
    DefaultFontSize = 12;
  private
    FDefaultFontFamilyName: string;
    function IFMXListingService.GetHeaderBehaviors = GetListingHeaderBehaviors;
    function IFMXListingService.GetSearchFeatures = GetListingSearchFeatures;
    function IFMXListingService.GetTransitionFeatures = GetListingTransitionFeatures;
    function IFMXListingService.GetEditModeFeatures = GetListingEditModeFeatures;
  public
    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;

    { IFMXSystemInformationService }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    function GetCaretWidth: Integer;
    function GetMenuShowDelay: Integer;

    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: string;
    function GetDefaultFontSize: Single;

    { IFMXLocaleService }
    function GetCurrentLangID: string;
    function GetLocaleFirstDayOfWeek: string;
    function GetFirstWeekday: Byte;

    { IFMXListingService }
    function GetListingHeaderBehaviors: TListingHeaderBehaviors;
    function GetListingSearchFeatures: TListingSearchFeatures;
    function GetListingTransitionFeatures: TListingTransitionFeatures;
    function GetListingEditModeFeatures: TListingEditModeFeatures;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, Winapi.Windows;

{ TWinMetricsServices }

function TWinMetricsServices.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  Result := AComponent = TComponentKind.Calendar;
end;

function TWinMetricsServices.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  if AComponent = TComponentKind.Calendar then
    Result := TSize.Create(202, 180)
  else
    Result := TSize.Create(80, 22);
end;

function TWinMetricsServices.GetScrollingBehaviour: TScrollingBehaviours;
var
  Value: Integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
  if ((Value and NID_READY) = NID_READY) and (((Value and NID_MULTI_INPUT) = NID_MULTI_INPUT)) then
    Result := [TScrollingBehaviour.Animation, TScrollingBehaviour.TouchTracking]
  else
    Result := [];
end;

function TWinMetricsServices.GetMinScrollThumbSize: Single;
begin
  Result := 15;
end;

function TWinMetricsServices.GetCaretWidth: Integer;
begin
  Result := 1;
end;

function TWinMetricsServices.GetMenuShowDelay: Integer;
begin
  if SystemParametersInfo(SPI_GETMENUSHOWDELAY, 0, @Result, 0) then
    Result := Result div 2
  else
    Result := 0;
end;

function TWinMetricsServices.GetDefaultFontFamilyName: string;
var
  NonClientMetrics: TNonClientMetrics;
begin
  if FDefaultFontFamilyName.IsEmpty then
  begin
    NonClientMetrics.cbSize := SizeOf(TNonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(TNonClientMetrics), @NonClientMetrics, 0) then
      FDefaultFontFamilyName := NonClientMetrics.lfMessageFont.lfFaceName
    else if TOSVersion.Check(6) then
      FDefaultFontFamilyName := 'Segoe UI'
    else
      FDefaultFontFamilyName := 'Tahoma';
  end;

  Result := FDefaultFontFamilyName;
end;

function TWinMetricsServices.GetDefaultFontSize: Single;
begin
  Result := DefaultFontSize;
end;

function TWinMetricsServices.GetCurrentLangID: string;
var
  buffer: MarshaledString;
  UserLCID: LCID;
  buflen: Integer;
begin
  UserLCID := GetUserDefaultLCID;
  buflen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
  buffer := StrAlloc(buflen);
  if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, buflen) <> 0 then
    Result := buffer
  else
    Result := 'en';
  StrDispose(buffer);
end;

function TWinMetricsServices.GetLocaleFirstDayOfWeek: string;
var
  buffer: DWORD;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, @buffer, SizeOf(buffer) div SizeOf(Char));
  Result := Chr(buffer);
end;

function TWinMetricsServices.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  buffer: DWORD;
begin
  // On Windows zero index corresponds to Monday, so we need to add offset to match DayMonday = 1 in RTL
  if GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, @buffer, SizeOf(buffer) div SizeOf(Char)) > 0 then
    Result := buffer - Ord('0') + MondayOffset
  else
    Result := DayMonday;
end;

function TWinMetricsServices.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

function TWinMetricsServices.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TWinMetricsServices.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [];
end;

function TWinMetricsServices.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [];
end;

end.
