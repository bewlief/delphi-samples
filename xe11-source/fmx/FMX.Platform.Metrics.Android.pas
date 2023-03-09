{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Metrics.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Rtti, FMX.Platform, FMX.Graphics;

type
  /// <summary>This class represents all interfaces for getting metrics</summary>
  TAndroidMetricsServices = class(TInterfacedObject, IFMXDefaultMetricsService, IFMXDefaultPropertyValueService,
                                  IFMXSystemInformationService, IFMXSystemFontService, IFMXLocaleService,
                                  IFMXListingService)
  public const
    DefaultAndroidFontSize = 14;
    DefaultAndroidFontName = 'Roboto';
  private
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
  System.SysUtils, Androidapi.JNI.JavaTypes, Androidapi.Helpers, FMX.Pickers;

{ TAndroidMetricsServices }

constructor TAndroidMetricsServices.Create;
begin
  inherited;
  RegisterServices;
  _AddRef;
end;

destructor TAndroidMetricsServices.Destroy;
begin
  UnregisterServices;
  inherited;
end;

function TAndroidMetricsServices.GetCaretWidth: Integer;
begin
  Result := 2;
end;

function TAndroidMetricsServices.GetCurrentLangID: string;
var
  Locale: JLocale;
begin
  Locale := TJLocale.JavaClass.getDefault;
  Result := JStringToString(Locale.getISO3Language);
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

function TAndroidMetricsServices.GetDefaultFontFamilyName: string;
begin
  Result := DefaultAndroidFontName;
end;

function TAndroidMetricsServices.GetDefaultFontSize: Single;
begin
  Result := DefaultAndroidFontSize;
end;

function TAndroidMetricsServices.GetDefaultPropertyValue(const AClassName, APropertyName: string): TValue;

  function GetSpinBoxPropertyDefaultValue: TValue;
  const
    SpinBoxDefaultPropName = 'CanFocusOnPlusMinus'; //Do not localize
  begin
    Result := TValue.Empty;
    if string.Compare(APropertyName, SpinBoxDefaultPropName, True) = 0 then
      Result := False;
  end;

  function GetComboEditPropertyDefaultValue: TValue;
  const
    ComboEditDefaultPropName = 'NeedSetFocusAfterButtonClick'; //Do not localize
  begin
    Result := TValue.Empty;
    if string.Compare(APropertyName, ComboEditDefaultPropName, True) = 0 then
      Result := False;
  end;

const
  ColorComboBoxClassName = 'tcolorcombobox'; //Do not localize
  SpibBoxClassName = 'tspinbox'; //Do not localize
  ComboExitClassName = 'tcomboeditbox'; //Do not localize
begin
  Result := TValue.Empty;

  if string.Compare(AClassName, ColorComboBoxClassName, True) = 0 then
    Result := TValue.From<TDropDownKind>(TDropDownKind.Native)
  else if string.Compare(AClassName, SpibBoxClassName, True) = 0 then
    Result := GetSpinBoxPropertyDefaultValue
  else if string.Compare(AClassName, ComboExitClassName, True) = 0 then
    Result := GetComboEditPropertyDefaultValue
  else
    Result := False;
end;

function TAndroidMetricsServices.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.Button: Result := TSize.Create(73, 44);
    TComponentKind.Label: Result := TSize.Create(82, 23);
    TComponentKind.Edit: Result := TSize.Create(97, 32);
    TComponentKind.ScrollBar: Result := TSize.Create(7, 7);
    TComponentKind.ListBoxItem: Result := TSize.Create(44, 44);
    TComponentKind.Calendar: Result := TSize.Create(346, 300);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TAndroidMetricsServices.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  Calendar: JCalendar;
begin
  Calendar := TJCalendar.JavaClass.getInstance;
  // On the Android Zero index corresponds Sunday, so we need to add offset. Because in RTL DayMonday = 1
  Result := Calendar.getFirstDayOfWeek - MondayOffset;
end;

function TAndroidMetricsServices.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [];
end;

function TAndroidMetricsServices.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

function TAndroidMetricsServices.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TAndroidMetricsServices.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [TListingTransitionFeature.ScrollGlow];
end;

function TAndroidMetricsServices.GetLocaleFirstDayOfWeek: string;
var
  Calendar: JCalendar;
begin
  Calendar := TJCalendar.JavaClass.getInstance;
  Result := IntToStr(Calendar.getFirstDayOfWeek);
end;

function TAndroidMetricsServices.GetMenuShowDelay: Integer;
begin
  Result := 0;
end;

function TAndroidMetricsServices.GetMinScrollThumbSize: Single;
begin
  Result := 30;
end;

function TAndroidMetricsServices.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.Animation, TScrollingBehaviour.AutoShowing, TScrollingBehaviour.TouchTracking];
end;

procedure TAndroidMetricsServices.RegisterServices;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDefaultMetricsService) then
    TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXDefaultPropertyValueService) then
    TPlatformServices.Current.AddPlatformService(IFMXDefaultPropertyValueService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXSystemInformationService) then
    TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXSystemFontService) then
    TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService) then
    TPlatformServices.Current.AddPlatformService(IFMXLocaleService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXListingService) then
    TPlatformServices.Current.AddPlatformService(IFMXListingService, Self);
end;

function TAndroidMetricsServices.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
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

procedure TAndroidMetricsServices.UnregisterServices;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
    TPlatformServices.Current.RemovePlatformService(IFMXDefaultPropertyValueService);
    TPlatformServices.Current.RemovePlatformService(IFMXSystemInformationService);
    TPlatformServices.Current.RemovePlatformService(IFMXSystemFontService);
    TPlatformServices.Current.RemovePlatformService(IFMXLocaleService);
    TPlatformServices.Current.RemovePlatformService(IFMXListingService);
  end;
end;

end.
