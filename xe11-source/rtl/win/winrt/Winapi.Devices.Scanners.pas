{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Devices.Scanners;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.CommonTypes, 
  Winapi.GraphicsRT, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  IImageScannerScanResult = Winapi.CommonTypes.IImageScannerScanResult;
  PIImageScannerScanResult = Winapi.CommonTypes.PIImageScannerScanResult;

  // Forward declarations for interfaces

  // Windows.Devices.Scanners.IImageScannerFormatConfiguration
  IImageScannerFormatConfiguration = interface;
  PIImageScannerFormatConfiguration = ^IImageScannerFormatConfiguration;

  // Windows.Devices.Scanners.IImageScannerPreviewResult
  IImageScannerPreviewResult = interface;
  PIImageScannerPreviewResult = ^IImageScannerPreviewResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Scanners.IImageScannerPreviewResult>
  AsyncOperationCompletedHandler_1__IImageScannerPreviewResult = interface;
  PAsyncOperationCompletedHandler_1__IImageScannerPreviewResult = ^AsyncOperationCompletedHandler_1__IImageScannerPreviewResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Scanners.IImageScannerPreviewResult>
  IAsyncOperation_1__IImageScannerPreviewResult = interface;
  PIAsyncOperation_1__IImageScannerPreviewResult = ^IAsyncOperation_1__IImageScannerPreviewResult;

  // Windows.Devices.Scanners.IImageScanner
  IImageScanner = interface;
  PIImageScanner = ^IImageScanner;

  // Windows.Devices.Scanners.IImageScannerFeederConfiguration
  IImageScannerFeederConfiguration = interface;
  PIImageScannerFeederConfiguration = ^IImageScannerFeederConfiguration;

  // Windows.Devices.Scanners.IImageScannerSourceConfiguration
  IImageScannerSourceConfiguration = interface;
  PIImageScannerSourceConfiguration = ^IImageScannerSourceConfiguration;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Scanners.IImageScanner>
  AsyncOperationCompletedHandler_1__IImageScanner = interface;
  PAsyncOperationCompletedHandler_1__IImageScanner = ^AsyncOperationCompletedHandler_1__IImageScanner;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Scanners.IImageScanner>
  IAsyncOperation_1__IImageScanner = interface;
  PIAsyncOperation_1__IImageScanner = ^IAsyncOperation_1__IImageScanner;

  // Windows.Devices.Scanners.IImageScannerStatics
  IImageScannerStatics = interface;
  PIImageScannerStatics = ^IImageScannerStatics;

  // Windows.Devices.Scanners Enums

  // Windows.Devices.Scanners.ImageScannerAutoCroppingMode
  ImageScannerAutoCroppingMode = (
    Disabled = 0,
    SingleRegion = 1,
    MultipleRegion = 2
  );
  PImageScannerAutoCroppingMode = ^ImageScannerAutoCroppingMode;

  // Windows.Devices.Scanners.ImageScannerColorMode
  ImageScannerColorMode = (
    Color = 0,
    Grayscale = 1,
    Monochrome = 2,
    AutoColor = 3
  );
  PImageScannerColorMode = ^ImageScannerColorMode;

  // Windows.Devices.Scanners.ImageScannerFormat
  ImageScannerFormat = (
    Jpeg = 0,
    Png = 1,
    DeviceIndependentBitmap = 2,
    Tiff = 3,
    Xps = 4,
    OpenXps = 5,
    Pdf = 6
  );
  PImageScannerFormat = ^ImageScannerFormat;

  // Windows.Devices.Scanners.ImageScannerScanSource
  ImageScannerScanSource = (
    Default = 0,
    Flatbed = 1,
    Feeder = 2,
    AutoConfigured = 3
  );
  PImageScannerScanSource = ^ImageScannerScanSource;

  // Windows.Devices.Scanners Records
  // Windows.Devices.Scanners.ImageScannerResolution
  ImageScannerResolution = record
    DpiX: Single;
    DpiY: Single;
  end;
  PImageScannerResolution = ^ImageScannerResolution;

  // Windows.Devices.Scanners.ScannerDeviceContract
  ScannerDeviceContract = record
  end;
  PScannerDeviceContract = ^ScannerDeviceContract;

  // Windows.Devices.Scanners Interfaces

  // UsedAPI Interface
  // Windows.Devices.Scanners.IImageScannerFormatConfiguration
  IImageScannerFormatConfiguration = interface(IInspectable)
  ['{AE275D11-DADF-4010-BF10-CCA5C83DCBB0}']
    function get_DefaultFormat: ImageScannerFormat; safecall;
    function get_Format: ImageScannerFormat; safecall;
    procedure put_Format(value: ImageScannerFormat); safecall;
    function IsFormatSupported(value: ImageScannerFormat): Boolean; safecall;
    property DefaultFormat: ImageScannerFormat read get_DefaultFormat;
    property Format: ImageScannerFormat read get_Format write put_Format;
  end;

  // UsedAPI Interface
  // Windows.Devices.Scanners.IImageScannerPreviewResult
  IImageScannerPreviewResult = interface(IInspectable)
  ['{08B7FE8E-8891-441D-BE9C-176FA109C8BB}']
    function get_Succeeded: Boolean; safecall;
    function get_Format: ImageScannerFormat; safecall;
    property Format: ImageScannerFormat read get_Format;
    property Succeeded: Boolean read get_Succeeded;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Scanners.IImageScannerPreviewResult>
  AsyncOperationCompletedHandler_1__IImageScannerPreviewResult_Delegate_Base = interface(IUnknown)
  ['{C054A410-AC3C-5353-B1EE-E85E78FAF3F1}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IImageScannerPreviewResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Scanners.IImageScannerPreviewResult>
  AsyncOperationCompletedHandler_1__IImageScannerPreviewResult = interface(AsyncOperationCompletedHandler_1__IImageScannerPreviewResult_Delegate_Base)
  ['{96C784BC-119C-557A-8D28-405AF877D7B2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Scanners.IImageScannerPreviewResult>
  IAsyncOperation_1__IImageScannerPreviewResult_Base = interface(IInspectable)
  ['{2F74576F-0498-5348-BC3B-A70D1A771718}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IImageScannerPreviewResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IImageScannerPreviewResult; safecall;
    function GetResults: IImageScannerPreviewResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IImageScannerPreviewResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Scanners.IImageScannerPreviewResult>
  IAsyncOperation_1__IImageScannerPreviewResult = interface(IAsyncOperation_1__IImageScannerPreviewResult_Base)
  ['{2EE5CD44-9985-5C27-9C6A-32A92405B896}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Scanners.IImageScanner
  [WinRTClassNameAttribute(SWindows_Devices_Scanners_ImageScanner)]
  IImageScanner = interface(IInspectable)
  ['{53A88F78-5298-48A0-8DA3-8087519665E0}']
    function get_DeviceId: HSTRING; safecall;
    function get_DefaultScanSource: ImageScannerScanSource; safecall;
    function IsScanSourceSupported(value: ImageScannerScanSource): Boolean; safecall;
    function get_FlatbedConfiguration: IImageScannerFormatConfiguration; safecall;
    function get_FeederConfiguration: IImageScannerFormatConfiguration; safecall;
    function get_AutoConfiguration: IImageScannerFormatConfiguration; safecall;
    function IsPreviewSupported(scanSource: ImageScannerScanSource): Boolean; safecall;
    function ScanPreviewToStreamAsync(scanSource: ImageScannerScanSource; targetStream: IRandomAccessStream): IAsyncOperation_1__IImageScannerPreviewResult; safecall;
    function ScanFilesToFolderAsync(scanSource: ImageScannerScanSource; storageFolder: IStorageFolder): IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal; safecall;
    property AutoConfiguration: IImageScannerFormatConfiguration read get_AutoConfiguration;
    property DefaultScanSource: ImageScannerScanSource read get_DefaultScanSource;
    property DeviceId: HSTRING read get_DeviceId;
    property FeederConfiguration: IImageScannerFormatConfiguration read get_FeederConfiguration;
    property FlatbedConfiguration: IImageScannerFormatConfiguration read get_FlatbedConfiguration;
  end;

  // Windows.Devices.Scanners.IImageScannerFeederConfiguration
  IImageScannerFeederConfiguration = interface(IInspectable)
  ['{74BDACEE-FA97-4C17-8280-40E39C6DCC67}']
    function get_CanAutoDetectPageSize: Boolean; safecall;
    function get_AutoDetectPageSize: Boolean; safecall;
    procedure put_AutoDetectPageSize(value: Boolean); safecall;
    function get_PageSize: Printing_PrintMediaSize; safecall;
    procedure put_PageSize(value: Printing_PrintMediaSize); safecall;
    function get_PageOrientation: Printing_PrintOrientation; safecall;
    procedure put_PageOrientation(value: Printing_PrintOrientation); safecall;
    function get_PageSizeDimensions: TSizeF; safecall;
    function IsPageSizeSupported(pageSize: Printing_PrintMediaSize; pageOrientation: Printing_PrintOrientation): Boolean; safecall;
    function get_MaxNumberOfPages: Cardinal; safecall;
    procedure put_MaxNumberOfPages(value: Cardinal); safecall;
    function get_CanScanDuplex: Boolean; safecall;
    function get_Duplex: Boolean; safecall;
    procedure put_Duplex(value: Boolean); safecall;
    function get_CanScanAhead: Boolean; safecall;
    function get_ScanAhead: Boolean; safecall;
    procedure put_ScanAhead(value: Boolean); safecall;
    property AutoDetectPageSize: Boolean read get_AutoDetectPageSize write put_AutoDetectPageSize;
    property CanAutoDetectPageSize: Boolean read get_CanAutoDetectPageSize;
    property CanScanAhead: Boolean read get_CanScanAhead;
    property CanScanDuplex: Boolean read get_CanScanDuplex;
    property Duplex: Boolean read get_Duplex write put_Duplex;
    property MaxNumberOfPages: Cardinal read get_MaxNumberOfPages write put_MaxNumberOfPages;
    property PageOrientation: Printing_PrintOrientation read get_PageOrientation write put_PageOrientation;
    property PageSize: Printing_PrintMediaSize read get_PageSize write put_PageSize;
    property PageSizeDimensions: TSizeF read get_PageSizeDimensions;
    property ScanAhead: Boolean read get_ScanAhead write put_ScanAhead;
  end;

  // Windows.Devices.Scanners.IImageScannerSourceConfiguration
  IImageScannerSourceConfiguration = interface(IInspectable)
  ['{BFB50055-0B44-4C82-9E89-205F9C234E59}']
    function get_MinScanArea: TSizeF; safecall;
    function get_MaxScanArea: TSizeF; safecall;
    function get_SelectedScanRegion: TRectF; safecall;
    procedure put_SelectedScanRegion(value: TRectF); safecall;
    function get_AutoCroppingMode: ImageScannerAutoCroppingMode; safecall;
    procedure put_AutoCroppingMode(value: ImageScannerAutoCroppingMode); safecall;
    function IsAutoCroppingModeSupported(value: ImageScannerAutoCroppingMode): Boolean; safecall;
    function get_MinResolution: ImageScannerResolution; safecall;
    function get_MaxResolution: ImageScannerResolution; safecall;
    function get_OpticalResolution: ImageScannerResolution; safecall;
    function get_DesiredResolution: ImageScannerResolution; safecall;
    procedure put_DesiredResolution(value: ImageScannerResolution); safecall;
    function get_ActualResolution: ImageScannerResolution; safecall;
    function get_DefaultColorMode: ImageScannerColorMode; safecall;
    function get_ColorMode: ImageScannerColorMode; safecall;
    procedure put_ColorMode(value: ImageScannerColorMode); safecall;
    function IsColorModeSupported(value: ImageScannerColorMode): Boolean; safecall;
    function get_MinBrightness: Integer; safecall;
    function get_MaxBrightness: Integer; safecall;
    function get_BrightnessStep: Cardinal; safecall;
    function get_DefaultBrightness: Integer; safecall;
    function get_Brightness: Integer; safecall;
    procedure put_Brightness(value: Integer); safecall;
    function get_MinContrast: Integer; safecall;
    function get_MaxContrast: Integer; safecall;
    function get_ContrastStep: Cardinal; safecall;
    function get_DefaultContrast: Integer; safecall;
    function get_Contrast: Integer; safecall;
    procedure put_Contrast(value: Integer); safecall;
    property ActualResolution: ImageScannerResolution read get_ActualResolution;
    property AutoCroppingMode: ImageScannerAutoCroppingMode read get_AutoCroppingMode write put_AutoCroppingMode;
    property Brightness: Integer read get_Brightness write put_Brightness;
    property BrightnessStep: Cardinal read get_BrightnessStep;
    property ColorMode: ImageScannerColorMode read get_ColorMode write put_ColorMode;
    property Contrast: Integer read get_Contrast write put_Contrast;
    property ContrastStep: Cardinal read get_ContrastStep;
    property DefaultBrightness: Integer read get_DefaultBrightness;
    property DefaultColorMode: ImageScannerColorMode read get_DefaultColorMode;
    property DefaultContrast: Integer read get_DefaultContrast;
    property DesiredResolution: ImageScannerResolution read get_DesiredResolution write put_DesiredResolution;
    property MaxBrightness: Integer read get_MaxBrightness;
    property MaxContrast: Integer read get_MaxContrast;
    property MaxResolution: ImageScannerResolution read get_MaxResolution;
    property MaxScanArea: TSizeF read get_MaxScanArea;
    property MinBrightness: Integer read get_MinBrightness;
    property MinContrast: Integer read get_MinContrast;
    property MinResolution: ImageScannerResolution read get_MinResolution;
    property MinScanArea: TSizeF read get_MinScanArea;
    property OpticalResolution: ImageScannerResolution read get_OpticalResolution;
    property SelectedScanRegion: TRectF read get_SelectedScanRegion write put_SelectedScanRegion;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Scanners.IImageScanner>
  AsyncOperationCompletedHandler_1__IImageScanner_Delegate_Base = interface(IUnknown)
  ['{B35AD6B4-0DA0-5241-87FF-EEF3A1883243}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IImageScanner; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Scanners.IImageScanner>
  AsyncOperationCompletedHandler_1__IImageScanner = interface(AsyncOperationCompletedHandler_1__IImageScanner_Delegate_Base)
  ['{60E91BF4-CA9E-57D7-AC8C-BA5180FFAECD}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Scanners.IImageScanner>
  IAsyncOperation_1__IImageScanner_Base = interface(IInspectable)
  ['{75D78736-6C52-551E-AB5F-50674F323431}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IImageScanner); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IImageScanner; safecall;
    function GetResults: IImageScanner; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IImageScanner read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Scanners.IImageScanner>
  IAsyncOperation_1__IImageScanner = interface(IAsyncOperation_1__IImageScanner_Base)
  ['{279D0851-470C-54D1-8873-F2C5FD62A1DB}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Scanners.IImageScannerStatics
  [WinRTClassNameAttribute(SWindows_Devices_Scanners_ImageScanner)]
  IImageScannerStatics = interface(IInspectable)
  ['{BC57E70E-D804-4477-9FB5-B911B5473897}']
    function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IImageScanner; safecall;
    function GetDeviceSelector: HSTRING; safecall;
  end;

  // Windows.Devices.Scanners.ImageScanner
  // DualAPI
  // Implements: Windows.Devices.Scanners.IImageScanner
  // Statics: "Windows.Devices.Scanners.IImageScannerStatics"
  TImageScanner = class(TWinRTGenericImportS<IImageScannerStatics>)
  public
    // -> IImageScannerStatics
    class function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IImageScanner; static; inline;
    class function GetDeviceSelector: HSTRING; static; inline;
  end;

implementation

{ TImageScanner }

class function TImageScanner.FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IImageScanner;
begin
  Result := Statics.FromIdAsync(deviceId);
end;

class function TImageScanner.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;


end.
