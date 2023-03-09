{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Metrics.iOS;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Rtti, FMX.Platform, FMX.Graphics;

type
  /// <summary>This class represents all interfaces for getting metrics</summary>
  TiOSMetricsServices = class(TInterfacedObject, IFMXDefaultMetricsService, IFMXDefaultPropertyValueService,
                              IFMXSystemInformationService, IFMXTextEditingService, IFMXSystemFontService,
                              IFMXLocaleService, IFMXListingService)
  public const
    DefaultiOSFontSize = 14;
  private
    FDefaultiOSFontName: string;
    function IFMXListingService.GetHeaderBehaviors = GetListingHeaderBehaviors;
    function IFMXListingService.GetSearchFeatures = GetListingSearchFeatures;
    function IFMXListingService.GetTransitionFeatures = GetListingTransitionFeatures;
    function IFMXListingService.GetEditModeFeatures = GetListingEditModeFeatures;
  protected
    /// <summary>Registers all metrics services in platform</summary>
    procedure RegisterServices; virtual;
    /// <summary>Unregisters all metrics service</summary>
    procedure UnregisterServices; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;

    { IFMXDefaultPropertyValueService }
    function GetDefaultPropertyValue(const AClassName, APropertyName: string): TValue;

    { IFMXSystemInformationService }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    function GetCaretWidth: Integer;
    function GetMenuShowDelay: Integer;

    { IFMXTextEditingService }
    function GetCaretBehaviors: TCaretBehaviors;

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
  TCocoaTouchMetricsServices = TiOSMetricsServices;

implementation

uses
  System.SysUtils, Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Helpers, iOSapi.CocoaTypes, iOSapi.UIKit, 
  iOSapi.Foundation, FMX.Pickers;

function TiOSMetricsServices.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  case AComponent of
    TComponentKind.Button: Result := True;
    TComponentKind.Label: Result := True;
    TComponentKind.Edit: Result := True;
    TComponentKind.ScrollBar: Result := True;
    TComponentKind.ListBoxItem: Result := True;
    TComponentKind.Calendar: Result := True;
  else
    Result := False;
  end;
end;

procedure TiOSMetricsServices.UnregisterServices;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
    TPlatformServices.Current.RemovePlatformService(IFMXDefaultPropertyValueService);
    TPlatformServices.Current.RemovePlatformService(IFMXSystemInformationService);
    TPlatformServices.Current.RemovePlatformService(IFMXTextEditingService);
    TPlatformServices.Current.RemovePlatformService(IFMXSystemFontService);
    TPlatformServices.Current.RemovePlatformService(IFMXLocaleService);
    TPlatformServices.Current.RemovePlatformService(IFMXListingService);
  end;
end;

function TiOSMetricsServices.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.Button: Result := TSize.Create(73, 44);
    TComponentKind.Label: Result := TSize.Create(82, 21);
    TComponentKind.Edit: Result := TSize.Create(97, 30);
    TComponentKind.ScrollBar: Result := TSize.Create(7, 7);
    TComponentKind.ListBoxItem: Result := TSize.Create(44, 44);
    TComponentKind.Calendar: Result := TSize.Create(320, 280);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TiOSMetricsServices.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  Calendar: NSCalendar;
begin
  Calendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
  // On the iOS Zero index corresponds Sunday, so we need to add offset. Because in RTL DayMonday = 1
  Result := Calendar.firstWeekday - MondayOffset;
end;

function TiOSMetricsServices.GetLocaleFirstDayOfWeek: string;
var
  Calendar: NSCalendar;
  FirstDay: NSUInteger;
begin
  Calendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
  FirstDay := Calendar.firstWeekday;
  Result := IntToStr(FirstDay);
end;

function TiOSMetricsServices.GetDefaultFontFamilyName: string;
begin
  if FDefaultiOSFontName.IsEmpty then
    FDefaultiOSFontName := NSStrToStr(TUIFont.Wrap(TUIFont.OCClass.systemFontOfSize(DefaultiOSFontSize)).fontName);
  Result := FDefaultiOSFontName;
end;

function TiOSMetricsServices.GetDefaultFontSize: Single;
begin
  Result := DefaultiOSFontSize;
end;

function TiOSMetricsServices.GetDefaultPropertyValue(const AClassName, APropertyName: string): TValue;

  function GetSpinBoxPropertyDefaultValue: TValue;
  begin
    Result := TValue.Empty;
    if string.Compare(APropertyName, 'CanFocusOnPlusMinus', True) = 0 then
      Result := False;
  end;

begin
  Result := TValue.Empty;

  if string.Compare(AClassName, 'tcolorcombobox', True) = 0 then
    Result := TValue.From<TDropDownKind>(TDropDownKind.Native)
  else if string.Compare(AClassName, 'tspinbox', True) = 0 then
    Result := GetSpinBoxPropertyDefaultValue
  else
    Result := False;
end;

function TiOSMetricsServices.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.BoundsAnimation, TScrollingBehaviour.Animation, TScrollingBehaviour.AutoShowing, TScrollingBehaviour.TouchTracking];
end;

procedure TiOSMetricsServices.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDefaultMetricsService) then
    TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDefaultPropertyValueService) then
    TPlatformServices.Current.AddPlatformService(IFMXDefaultPropertyValueService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXSystemInformationService) then
    TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTextEditingService) then
    TPlatformServices.Current.AddPlatformService(IFMXTextEditingService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXSystemFontService) then
    TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService) then
    TPlatformServices.Current.AddPlatformService(IFMXLocaleService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXListingService) then
    TPlatformServices.Current.AddPlatformService(IFMXListingService, Self);
end;

function TiOSMetricsServices.GetMinScrollThumbSize: Single;
begin
  Result := 30;
end;

constructor TiOSMetricsServices.Create;
begin
  inherited;
  RegisterServices;
end;

destructor TiOSMetricsServices.Destroy;
begin
  UnregisterServices;
  inherited;
end;

function TiOSMetricsServices.GetCaretBehaviors: TCaretBehaviors;
begin
  Result := [TCaretBehavior.DisableCaretInsideWords];
end;

function TiOSMetricsServices.GetCaretWidth: Integer;
begin
  Result := 2;
end;

function TiOSMetricsServices.GetCurrentLangID: string;
var
  CurrentLocale: NSLocale;
  LanguageISO: NSString;
begin
  CurrentLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
  LanguageISO := TNSString.Wrap(CurrentLocale.objectForKey((NSLocaleLanguageCode as ILocalObject).GetObjectID));
  Result := UTF8ToString(LanguageISO.UTF8String);
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

function TiOSMetricsServices.GetMenuShowDelay: Integer;
begin
  Result := 0;
end;

function TiOSMetricsServices.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [TListingHeaderBehavior.Sticky];
end;

function TiOSMetricsServices.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop, TListingSearchFeature.AsFirstItem];
end;

function TiOSMetricsServices.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [TListingTransitionFeature.EditMode, TListingTransitionFeature.DeleteButtonSlide,
    TListingTransitionFeature.PullToRefresh];
end;

function TiOSMetricsServices.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [TListingEditModeFeature.Delete];
end;

end.
