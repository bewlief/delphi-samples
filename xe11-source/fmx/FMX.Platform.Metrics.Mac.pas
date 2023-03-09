{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Metrics.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, FMX.Platform, FMX.Graphics;

type
  TMacMetricsServices = class(TInterfacedObject, IFMXDefaultMetricsService, IFMXSystemInformationService,
                              IFMXSystemFontService, IFMXLocaleService, IFMXListingService)
  public const
    DefaultFontSize = 13;
  private
    FDefaultFontName: string;
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
  System.SysUtils, Macapi.Helpers, Macapi.AppKit, Macapi.Foundation, Macapi.CocoaTypes;

{ TMacMetricsServices }

function TMacMetricsServices.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  case AComponent of
    TComponentKind.Calendar:
      Result := True;
  else
    Result := False;
  end;
end;

function TMacMetricsServices.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.Calendar: Result := TSize.Create(230, 204);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TMacMetricsServices.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.BoundsAnimation, TScrollingBehaviour.AutoShowing];
end;

function TMacMetricsServices.GetCaretWidth: Integer;
begin
  Result := 1;
end;

function TMacMetricsServices.GetMinScrollThumbSize: Single;
begin
  Result := 25;
end;

function TMacMetricsServices.GetMenuShowDelay: Integer;
begin
  Result := 150;
end;

function TMacMetricsServices.GetDefaultFontFamilyName: string;
begin
  if FDefaultFontName.IsEmpty then
    FDefaultFontName := NSStrToStr(TNSFont.Wrap(TNSFont.OCClass.systemFontOfSize(DefaultFontSize)).fontName);
  Result := FDefaultFontName;
end;

function TMacMetricsServices.GetDefaultFontSize: Single;
begin
  Result := DefaultFontSize;
end;

function TMacMetricsServices.GetCurrentLangID: string;
begin
  Result := NSStrToStr(TNSLocale.Wrap(TNSLocale.OCClass.currentLocale).localeIdentifier); // "en_GB", "es_ES_PREEURO"
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

function TMacMetricsServices.GetLocaleFirstDayOfWeek: string;
var
  cal: NSCalendar;
  firstDay: NSUInteger;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    cal:= TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
    firstDay:= Cal.firstWeekday;
    Result:= IntToStr(firstDay);
  finally
    AutoReleasePool.release;
  end;
end;

function TMacMetricsServices.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  Calendar: NSCalendar;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Calendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
    // On the OSX Zero index corresponds Sunday, so we need to add offset. Because in RTL DayMonday = 1
    Result := Calendar.firstWeekday - MondayOffset;
  finally
    AutoReleasePool.release;
  end;
end;

function TMacMetricsServices.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TMacMetricsServices.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [TListingTransitionFeature.EditMode, TListingTransitionFeature.DeleteButtonSlide,
    TListingTransitionFeature.PullToRefresh];
end;

function TMacMetricsServices.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [];
end;

function TMacMetricsServices.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

end.
