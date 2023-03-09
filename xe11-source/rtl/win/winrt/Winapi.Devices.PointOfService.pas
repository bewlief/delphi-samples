{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Devices.PointOfService;

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
  Winapi.Foundation, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  ICashDrawerCloseAlarm = Winapi.CommonTypes.ICashDrawerCloseAlarm;
  PICashDrawerCloseAlarm = Winapi.CommonTypes.PICashDrawerCloseAlarm;
  IClaimedCashDrawer = Winapi.CommonTypes.IClaimedCashDrawer;
  PIClaimedCashDrawer = Winapi.CommonTypes.PIClaimedCashDrawer;
  IClaimedLineDisplay = Winapi.CommonTypes.IClaimedLineDisplay;
  PIClaimedLineDisplay = Winapi.CommonTypes.PIClaimedLineDisplay;
  ILineDisplayCapabilities = Winapi.CommonTypes.ILineDisplayCapabilities;
  PILineDisplayCapabilities = Winapi.CommonTypes.PILineDisplayCapabilities;
  ILineDisplayWindow = Winapi.CommonTypes.ILineDisplayWindow;
  PILineDisplayWindow = Winapi.CommonTypes.PILineDisplayWindow;
  LineDisplayScrollDirection = Winapi.CommonTypes.LineDisplayScrollDirection;
  PLineDisplayScrollDirection = Winapi.CommonTypes.PLineDisplayScrollDirection;
  LineDisplayTextAttribute = Winapi.CommonTypes.LineDisplayTextAttribute;
  PLineDisplayTextAttribute = Winapi.CommonTypes.PLineDisplayTextAttribute;
  LineDisplayTextAttributeGranularity = Winapi.CommonTypes.LineDisplayTextAttributeGranularity;
  PLineDisplayTextAttributeGranularity = Winapi.CommonTypes.PLineDisplayTextAttributeGranularity;
  UnifiedPosPowerReportingType = Winapi.CommonTypes.UnifiedPosPowerReportingType;
  PUnifiedPosPowerReportingType = Winapi.CommonTypes.PUnifiedPosPowerReportingType;

  // Forward declarations for interfaces

  // Windows.Devices.PointOfService.IBarcodeSymbologyAttributes
  IBarcodeSymbologyAttributes = interface;
  PIBarcodeSymbologyAttributes = ^IBarcodeSymbologyAttributes;

  // Windows.Devices.PointOfService.ICashDrawerCapabilities
  ICashDrawerCapabilities = interface;
  PICashDrawerCapabilities = ^ICashDrawerCapabilities;

  // Windows.Devices.PointOfService.ICashDrawerStatus
  ICashDrawerStatus = interface;
  PICashDrawerStatus = ^ICashDrawerStatus;

  // Windows.Devices.PointOfService.ICashDrawerEventSourceEventArgs
  ICashDrawerEventSourceEventArgs = interface;
  PICashDrawerEventSourceEventArgs = ^ICashDrawerEventSourceEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawerEventSource,Windows.Devices.PointOfService.ICashDrawerEventSourceEventArgs>
  TypedEventHandler_2__ICashDrawerEventSource__ICashDrawerEventSourceEventArgs = interface;
  PTypedEventHandler_2__ICashDrawerEventSource__ICashDrawerEventSourceEventArgs = ^TypedEventHandler_2__ICashDrawerEventSource__ICashDrawerEventSourceEventArgs;

  // Windows.Devices.PointOfService.ICashDrawerEventSource
  ICashDrawerEventSource = interface;
  PICashDrawerEventSource = ^ICashDrawerEventSource;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedCashDrawer>
  AsyncOperationCompletedHandler_1__IClaimedCashDrawer = interface;
  PAsyncOperationCompletedHandler_1__IClaimedCashDrawer = ^AsyncOperationCompletedHandler_1__IClaimedCashDrawer;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedCashDrawer>
  IAsyncOperation_1__IClaimedCashDrawer = interface;
  PIAsyncOperation_1__IClaimedCashDrawer = ^IAsyncOperation_1__IClaimedCashDrawer;

  // Windows.Devices.PointOfService.ICashDrawerStatusUpdatedEventArgs
  ICashDrawerStatusUpdatedEventArgs = interface;
  PICashDrawerStatusUpdatedEventArgs = ^ICashDrawerStatusUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawer,Windows.Devices.PointOfService.ICashDrawerStatusUpdatedEventArgs>
  TypedEventHandler_2__ICashDrawer__ICashDrawerStatusUpdatedEventArgs = interface;
  PTypedEventHandler_2__ICashDrawer__ICashDrawerStatusUpdatedEventArgs = ^TypedEventHandler_2__ICashDrawer__ICashDrawerStatusUpdatedEventArgs;

  // Windows.Devices.PointOfService.ICashDrawer
  ICashDrawer = interface;
  PICashDrawer = ^ICashDrawer;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ICashDrawer>
  AsyncOperationCompletedHandler_1__ICashDrawer = interface;
  PAsyncOperationCompletedHandler_1__ICashDrawer = ^AsyncOperationCompletedHandler_1__ICashDrawer;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ICashDrawer>
  IAsyncOperation_1__ICashDrawer = interface;
  PIAsyncOperation_1__ICashDrawer = ^IAsyncOperation_1__ICashDrawer;

  // Windows.Devices.PointOfService.ICashDrawerStatics
  ICashDrawerStatics = interface;
  PICashDrawerStatics = ^ICashDrawerStatics;

  // Windows.Devices.PointOfService.ICashDrawerStatics2
  ICashDrawerStatics2 = interface;
  PICashDrawerStatics2 = ^ICashDrawerStatics2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IBarcodeSymbologyAttributes>
  AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes = interface;
  PAsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes = ^AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IBarcodeSymbologyAttributes>
  IAsyncOperation_1__IBarcodeSymbologyAttributes = interface;
  PIAsyncOperation_1__IBarcodeSymbologyAttributes = ^IAsyncOperation_1__IBarcodeSymbologyAttributes;

  // Windows.Devices.PointOfService.IPosPrinterPrintOptions
  IPosPrinterPrintOptions = interface;
  PIPosPrinterPrintOptions = ^IPosPrinterPrintOptions;

  // Windows.Devices.PointOfService.IJournalPrintJob
  IJournalPrintJob = interface;
  PIJournalPrintJob = ^IJournalPrintJob;

  // Windows.Devices.PointOfService.IClaimedJournalPrinter
  IClaimedJournalPrinter = interface;
  PIClaimedJournalPrinter = ^IClaimedJournalPrinter;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.LineDisplayPowerStatus>
  AsyncOperationCompletedHandler_1__LineDisplayPowerStatus = interface;
  PAsyncOperationCompletedHandler_1__LineDisplayPowerStatus = ^AsyncOperationCompletedHandler_1__LineDisplayPowerStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.LineDisplayPowerStatus>
  IAsyncOperation_1__LineDisplayPowerStatus = interface;
  PIAsyncOperation_1__LineDisplayPowerStatus = ^IAsyncOperation_1__LineDisplayPowerStatus;

  // Windows.Devices.PointOfService.ILineDisplayStatusUpdatedEventArgs
  ILineDisplayStatusUpdatedEventArgs = interface;
  PILineDisplayStatusUpdatedEventArgs = ^ILineDisplayStatusUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Windows.Devices.PointOfService.ILineDisplayStatusUpdatedEventArgs>
  TypedEventHandler_2__IClaimedLineDisplay__ILineDisplayStatusUpdatedEventArgs = interface;
  PTypedEventHandler_2__IClaimedLineDisplay__ILineDisplayStatusUpdatedEventArgs = ^TypedEventHandler_2__IClaimedLineDisplay__ILineDisplayStatusUpdatedEventArgs;

  // Windows.Devices.PointOfService.ILineDisplayCustomGlyphs
  ILineDisplayCustomGlyphs = interface;
  PILineDisplayCustomGlyphs = ^ILineDisplayCustomGlyphs;

  // Windows.Devices.PointOfService.ILineDisplayAttributes
  ILineDisplayAttributes = interface;
  PILineDisplayAttributes = ^ILineDisplayAttributes;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplayWindow>
  AsyncOperationCompletedHandler_1__ILineDisplayWindow = interface;
  PAsyncOperationCompletedHandler_1__ILineDisplayWindow = ^AsyncOperationCompletedHandler_1__ILineDisplayWindow;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplayWindow>
  IAsyncOperation_1__ILineDisplayWindow = interface;
  PIAsyncOperation_1__ILineDisplayWindow = ^IAsyncOperation_1__ILineDisplayWindow;

  // Windows.Devices.PointOfService.ILineDisplayStoredBitmap
  ILineDisplayStoredBitmap = interface;
  PILineDisplayStoredBitmap = ^ILineDisplayStoredBitmap;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplayStoredBitmap>
  AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap = interface;
  PAsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap = ^AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplayStoredBitmap>
  IAsyncOperation_1__ILineDisplayStoredBitmap = interface;
  PIAsyncOperation_1__ILineDisplayStoredBitmap = ^IAsyncOperation_1__ILineDisplayStoredBitmap;

  // Windows.Devices.PointOfService.IClaimedLineDisplay2
  IClaimedLineDisplay2 = interface;
  PIClaimedLineDisplay2 = ^IClaimedLineDisplay2;

  // Windows.Devices.PointOfService.IClaimedLineDisplayClosedEventArgs
  IClaimedLineDisplayClosedEventArgs = interface;
  PIClaimedLineDisplayClosedEventArgs = ^IClaimedLineDisplayClosedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Windows.Devices.PointOfService.IClaimedLineDisplayClosedEventArgs>
  TypedEventHandler_2__IClaimedLineDisplay__IClaimedLineDisplayClosedEventArgs = interface;
  PTypedEventHandler_2__IClaimedLineDisplay__IClaimedLineDisplayClosedEventArgs = ^TypedEventHandler_2__IClaimedLineDisplay__IClaimedLineDisplayClosedEventArgs;

  // Windows.Devices.PointOfService.IClaimedLineDisplay3
  IClaimedLineDisplay3 = interface;
  PIClaimedLineDisplay3 = ^IClaimedLineDisplay3;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedLineDisplay>
  AsyncOperationCompletedHandler_1__IClaimedLineDisplay = interface;
  PAsyncOperationCompletedHandler_1__IClaimedLineDisplay = ^AsyncOperationCompletedHandler_1__IClaimedLineDisplay;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedLineDisplay>
  IAsyncOperation_1__IClaimedLineDisplay = interface;
  PIAsyncOperation_1__IClaimedLineDisplay = ^IAsyncOperation_1__IClaimedLineDisplay;

  // Windows.Devices.PointOfService.IClaimedLineDisplayStatics
  IClaimedLineDisplayStatics = interface;
  PIClaimedLineDisplayStatics = ^IClaimedLineDisplayStatics;

  // Windows.Devices.PointOfService.IReceiptPrintJob
  IReceiptPrintJob = interface;
  PIReceiptPrintJob = ^IReceiptPrintJob;

  // Windows.Devices.PointOfService.IClaimedReceiptPrinter
  IClaimedReceiptPrinter = interface;
  PIClaimedReceiptPrinter = ^IClaimedReceiptPrinter;

  // Windows.Devices.PointOfService.ISlipPrintJob
  ISlipPrintJob = interface;
  PISlipPrintJob = ^ISlipPrintJob;

  // Windows.Devices.PointOfService.IClaimedSlipPrinter
  IClaimedSlipPrinter = interface;
  PIClaimedSlipPrinter = ^IClaimedSlipPrinter;

  // Windows.Devices.PointOfService.IPosPrinterReleaseDeviceRequestedEventArgs
  IPosPrinterReleaseDeviceRequestedEventArgs = interface;
  PIPosPrinterReleaseDeviceRequestedEventArgs = ^IPosPrinterReleaseDeviceRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedPosPrinter,Windows.Devices.PointOfService.IPosPrinterReleaseDeviceRequestedEventArgs>
  TypedEventHandler_2__IClaimedPosPrinter__IPosPrinterReleaseDeviceRequestedEventArgs = interface;
  PTypedEventHandler_2__IClaimedPosPrinter__IPosPrinterReleaseDeviceRequestedEventArgs = ^TypedEventHandler_2__IClaimedPosPrinter__IPosPrinterReleaseDeviceRequestedEventArgs;

  // Windows.Devices.PointOfService.IClaimedPosPrinter
  IClaimedPosPrinter = interface;
  PIClaimedPosPrinter = ^IClaimedPosPrinter;

  // Windows.Devices.PointOfService.ICommonClaimedPosPrinterStation
  ICommonClaimedPosPrinterStation = interface;
  PICommonClaimedPosPrinterStation = ^ICommonClaimedPosPrinterStation;

  // Windows.Devices.PointOfService.ICommonPosPrintStationCapabilities
  ICommonPosPrintStationCapabilities = interface;
  PICommonPosPrintStationCapabilities = ^ICommonPosPrintStationCapabilities;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IIterator_1__PosPrinterRotation = interface;
  PIIterator_1__PosPrinterRotation = ^IIterator_1__PosPrinterRotation;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IIterable_1__PosPrinterRotation = interface;
  PIIterable_1__PosPrinterRotation = ^IIterable_1__PosPrinterRotation;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IVectorView_1__PosPrinterRotation = interface;
  PIVectorView_1__PosPrinterRotation = ^IVectorView_1__PosPrinterRotation;

  // Windows.Devices.PointOfService.ICommonReceiptSlipCapabilities
  ICommonReceiptSlipCapabilities = interface;
  PICommonReceiptSlipCapabilities = ^ICommonReceiptSlipCapabilities;

  // Windows.Devices.PointOfService.IJournalPrinterCapabilities
  IJournalPrinterCapabilities = interface;
  PIJournalPrinterCapabilities = ^IJournalPrinterCapabilities;

  // Windows.Devices.PointOfService.IJournalPrinterCapabilities2
  IJournalPrinterCapabilities2 = interface;
  PIJournalPrinterCapabilities2 = ^IJournalPrinterCapabilities2;

  // Windows.Devices.PointOfService.ILineDisplay
  ILineDisplay = interface;
  PILineDisplay = ^ILineDisplay;

  // Windows.Devices.PointOfService.ILineDisplay2
  ILineDisplay2 = interface;
  PILineDisplay2 = ^ILineDisplay2;

  // Windows.Devices.PointOfService.ILineDisplayCursorAttributes
  ILineDisplayCursorAttributes = interface;
  PILineDisplayCursorAttributes = ^ILineDisplayCursorAttributes;

  // Windows.Devices.PointOfService.ILineDisplayCursor
  ILineDisplayCursor = interface;
  PILineDisplayCursor = ^ILineDisplayCursor;

  // Windows.Devices.PointOfService.ILineDisplayMarquee
  ILineDisplayMarquee = interface;
  PILineDisplayMarquee = ^ILineDisplayMarquee;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplay>
  AsyncOperationCompletedHandler_1__ILineDisplay = interface;
  PAsyncOperationCompletedHandler_1__ILineDisplay = ^AsyncOperationCompletedHandler_1__ILineDisplay;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplay>
  IAsyncOperation_1__ILineDisplay = interface;
  PIAsyncOperation_1__ILineDisplay = ^IAsyncOperation_1__ILineDisplay;

  // Windows.Devices.PointOfService.ILineDisplayStatics
  ILineDisplayStatics = interface;
  PILineDisplayStatics = ^ILineDisplayStatics;

  // Windows.Devices.PointOfService.ILineDisplayStatisticsCategorySelector
  ILineDisplayStatisticsCategorySelector = interface;
  PILineDisplayStatisticsCategorySelector = ^ILineDisplayStatisticsCategorySelector;

  // Windows.Devices.PointOfService.ILineDisplayStatics2
  ILineDisplayStatics2 = interface;
  PILineDisplayStatics2 = ^ILineDisplayStatics2;

  // Windows.Devices.PointOfService.ILineDisplayWindow2
  ILineDisplayWindow2 = interface;
  PILineDisplayWindow2 = ^ILineDisplayWindow2;

  // Windows.Devices.PointOfService.IReceiptPrinterCapabilities
  IReceiptPrinterCapabilities = interface;
  PIReceiptPrinterCapabilities = ^IReceiptPrinterCapabilities;

  // Windows.Devices.PointOfService.ISlipPrinterCapabilities
  ISlipPrinterCapabilities = interface;
  PISlipPrinterCapabilities = ^ISlipPrinterCapabilities;

  // Windows.Devices.PointOfService.IPosPrinterCapabilities
  IPosPrinterCapabilities = interface;
  PIPosPrinterCapabilities = ^IPosPrinterCapabilities;

  // Windows.Devices.PointOfService.IPosPrinterStatus
  IPosPrinterStatus = interface;
  PIPosPrinterStatus = ^IPosPrinterStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedPosPrinter>
  AsyncOperationCompletedHandler_1__IClaimedPosPrinter = interface;
  PAsyncOperationCompletedHandler_1__IClaimedPosPrinter = ^AsyncOperationCompletedHandler_1__IClaimedPosPrinter;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedPosPrinter>
  IAsyncOperation_1__IClaimedPosPrinter = interface;
  PIAsyncOperation_1__IClaimedPosPrinter = ^IAsyncOperation_1__IClaimedPosPrinter;

  // Windows.Devices.PointOfService.IPosPrinterStatusUpdatedEventArgs
  IPosPrinterStatusUpdatedEventArgs = interface;
  PIPosPrinterStatusUpdatedEventArgs = ^IPosPrinterStatusUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IPosPrinter,Windows.Devices.PointOfService.IPosPrinterStatusUpdatedEventArgs>
  TypedEventHandler_2__IPosPrinter__IPosPrinterStatusUpdatedEventArgs = interface;
  PTypedEventHandler_2__IPosPrinter__IPosPrinterStatusUpdatedEventArgs = ^TypedEventHandler_2__IPosPrinter__IPosPrinterStatusUpdatedEventArgs;

  // Windows.Devices.PointOfService.IPosPrinter
  IPosPrinter = interface;
  PIPosPrinter = ^IPosPrinter;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.PointOfService.SizeUInt32>
  IIterator_1__SizeUInt32 = interface;
  PIIterator_1__SizeUInt32 = ^IIterator_1__SizeUInt32;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.PointOfService.SizeUInt32>
  IIterable_1__SizeUInt32 = interface;
  PIIterable_1__SizeUInt32 = ^IIterable_1__SizeUInt32;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.PointOfService.SizeUInt32>
  IVectorView_1__SizeUInt32 = interface;
  PIVectorView_1__SizeUInt32 = ^IVectorView_1__SizeUInt32;

  // Windows.Devices.PointOfService.IPosPrinterFontProperty
  IPosPrinterFontProperty = interface;
  PIPosPrinterFontProperty = ^IPosPrinterFontProperty;

  // Windows.Devices.PointOfService.IPosPrinter2
  IPosPrinter2 = interface;
  PIPosPrinter2 = ^IPosPrinter2;

  // Windows.Devices.PointOfService.IPosPrinterCharacterSetIdsStatics
  IPosPrinterCharacterSetIdsStatics = interface;
  PIPosPrinterCharacterSetIdsStatics = ^IPosPrinterCharacterSetIdsStatics;

  // Windows.Devices.PointOfService.IPosPrinterJob
  IPosPrinterJob = interface;
  PIPosPrinterJob = ^IPosPrinterJob;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IPosPrinter>
  AsyncOperationCompletedHandler_1__IPosPrinter = interface;
  PAsyncOperationCompletedHandler_1__IPosPrinter = ^AsyncOperationCompletedHandler_1__IPosPrinter;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IPosPrinter>
  IAsyncOperation_1__IPosPrinter = interface;
  PIAsyncOperation_1__IPosPrinter = ^IAsyncOperation_1__IPosPrinter;

  // Windows.Devices.PointOfService.IPosPrinterStatics
  IPosPrinterStatics = interface;
  PIPosPrinterStatics = ^IPosPrinterStatics;

  // Windows.Devices.PointOfService.IPosPrinterStatics2
  IPosPrinterStatics2 = interface;
  PIPosPrinterStatics2 = ^IPosPrinterStatics2;

  // Windows.Devices.PointOfService.IReceiptPrintJob2
  IReceiptPrintJob2 = interface;
  PIReceiptPrintJob2 = ^IReceiptPrintJob2;

  // Windows.Devices.PointOfService.IReceiptPrinterCapabilities2
  IReceiptPrinterCapabilities2 = interface;
  PIReceiptPrinterCapabilities2 = ^IReceiptPrinterCapabilities2;

  // Windows.Devices.PointOfService.ISlipPrinterCapabilities2
  ISlipPrinterCapabilities2 = interface;
  PISlipPrinterCapabilities2 = ^ISlipPrinterCapabilities2;

  // Windows.Devices.PointOfService Enums

  // Windows.Devices.PointOfService.BarcodeScannerStatus
  BarcodeScannerStatus = (
    Online = 0,
    Off = 1,
    Offline = 2,
    OffOrOffline = 3,
    Extended = 4
  );
  PBarcodeScannerStatus = ^BarcodeScannerStatus;

  // Windows.Devices.PointOfService.BarcodeSymbologyDecodeLengthKind
  BarcodeSymbologyDecodeLengthKind = (
    AnyLength = 0,
    Discrete = 1,
    Range = 2
  );
  PBarcodeSymbologyDecodeLengthKind = ^BarcodeSymbologyDecodeLengthKind;

  // Windows.Devices.PointOfService.CashDrawerStatusKind
  CashDrawerStatusKind = (
    Online = 0,
    Off = 1,
    Offline = 2,
    OffOrOffline = 3,
    Extended = 4
  );
  PCashDrawerStatusKind = ^CashDrawerStatusKind;

  // Windows.Devices.PointOfService.LineDisplayCursorType
  LineDisplayCursorType = (
    None = 0,
    Block = 1,
    HalfBlock = 2,
    Underline = 3,
    Reverse = 4,
    Other = 5
  );
  PLineDisplayCursorType = ^LineDisplayCursorType;

  // Windows.Devices.PointOfService.LineDisplayDescriptorState
  LineDisplayDescriptorState = (
    Off = 0,
    &On = 1,
    Blink = 2
  );
  PLineDisplayDescriptorState = ^LineDisplayDescriptorState;

  // Windows.Devices.PointOfService.LineDisplayHorizontalAlignment
  LineDisplayHorizontalAlignment = (
    Left = 0,
    Center = 1,
    Right = 2
  );
  PLineDisplayHorizontalAlignment = ^LineDisplayHorizontalAlignment;

  // Windows.Devices.PointOfService.LineDisplayMarqueeFormat
  LineDisplayMarqueeFormat = (
    None = 0,
    Walk = 1,
    Place = 2
  );
  PLineDisplayMarqueeFormat = ^LineDisplayMarqueeFormat;

  // Windows.Devices.PointOfService.LineDisplayPowerStatus
  LineDisplayPowerStatus = (
    Unknown = 0,
    Online = 1,
    Off = 2,
    Offline = 3,
    OffOrOffline = 4
  );
  PLineDisplayPowerStatus = ^LineDisplayPowerStatus;

  // Windows.Devices.PointOfService.LineDisplayVerticalAlignment
  LineDisplayVerticalAlignment = (
    Top = 0,
    Center = 1,
    Bottom = 2
  );
  PLineDisplayVerticalAlignment = ^LineDisplayVerticalAlignment;

  // Windows.Devices.PointOfService.MagneticStripeReaderAuthenticationLevel
  MagneticStripeReaderAuthenticationLevel = (
    NotSupported = 0,
    Optional = 1,
    Required = 2
  );
  PMagneticStripeReaderAuthenticationLevel = ^MagneticStripeReaderAuthenticationLevel;

  // Windows.Devices.PointOfService.MagneticStripeReaderAuthenticationProtocol
  MagneticStripeReaderAuthenticationProtocol = (
    None = 0,
    ChallengeResponse = 1
  );
  PMagneticStripeReaderAuthenticationProtocol = ^MagneticStripeReaderAuthenticationProtocol;

  // Windows.Devices.PointOfService.MagneticStripeReaderErrorReportingType
  MagneticStripeReaderErrorReportingType = (
    CardLevel = 0,
    TrackLevel = 1
  );
  PMagneticStripeReaderErrorReportingType = ^MagneticStripeReaderErrorReportingType;

  // Windows.Devices.PointOfService.MagneticStripeReaderStatus
  MagneticStripeReaderStatus = (
    Unauthenticated = 0,
    Authenticated = 1,
    Extended = 2
  );
  PMagneticStripeReaderStatus = ^MagneticStripeReaderStatus;

  // Windows.Devices.PointOfService.MagneticStripeReaderTrackErrorType
  MagneticStripeReaderTrackErrorType = (
    None = 0,
    StartSentinelError = 1,
    EndSentinelError = 2,
    ParityError = 3,
    LrcError = 4,
    Unknown = -1
  );
  PMagneticStripeReaderTrackErrorType = ^MagneticStripeReaderTrackErrorType;

  // Windows.Devices.PointOfService.MagneticStripeReaderTrackIds
  MagneticStripeReaderTrackIds = (
    None = 0,
    Track1 = 1,
    Track2 = 2,
    Track3 = 4,
    Track4 = 8
  );
  PMagneticStripeReaderTrackIds = ^MagneticStripeReaderTrackIds;

  // Windows.Devices.PointOfService.PosConnectionTypes
  PosConnectionTypes = (
    Local = 1,
    IP = 2,
    Bluetooth = 4,
    All = -1
  );
  PPosConnectionTypes = ^PosConnectionTypes;

  // Windows.Devices.PointOfService.PosPrinterAlignment
  PosPrinterAlignment = (
    Left = 0,
    Center = 1,
    Right = 2
  );
  PPosPrinterAlignment = ^PosPrinterAlignment;

  // Windows.Devices.PointOfService.PosPrinterBarcodeTextPosition
  PosPrinterBarcodeTextPosition = (
    None = 0,
    Above = 1,
    Below = 2
  );
  PPosPrinterBarcodeTextPosition = ^PosPrinterBarcodeTextPosition;

  // Windows.Devices.PointOfService.PosPrinterCartridgeSensors
  PosPrinterCartridgeSensors = (
    None = 0,
    Removed = 1,
    Empty = 2,
    HeadCleaning = 4,
    NearEnd = 8
  );
  PPosPrinterCartridgeSensors = ^PosPrinterCartridgeSensors;

  // Windows.Devices.PointOfService.PosPrinterColorCapabilities
  PosPrinterColorCapabilities = (
    None = 0,
    Primary = 1,
    Custom1 = 2,
    Custom2 = 4,
    Custom3 = 8,
    Custom4 = 16,
    Custom5 = 32,
    Custom6 = 64,
    Cyan = 128,
    Magenta = 256,
    Yellow = 512,
    Full = 1024
  );
  PPosPrinterColorCapabilities = ^PosPrinterColorCapabilities;

  // Windows.Devices.PointOfService.PosPrinterColorCartridge
  PosPrinterColorCartridge = (
    Unknown = 0,
    Primary = 1,
    Custom1 = 2,
    Custom2 = 3,
    Custom3 = 4,
    Custom4 = 5,
    Custom5 = 6,
    Custom6 = 7,
    Cyan = 8,
    Magenta = 9,
    Yellow = 10
  );
  PPosPrinterColorCartridge = ^PosPrinterColorCartridge;

  // Windows.Devices.PointOfService.PosPrinterLineDirection
  PosPrinterLineDirection = (
    Horizontal = 0,
    Vertical = 1
  );
  PPosPrinterLineDirection = ^PosPrinterLineDirection;

  // Windows.Devices.PointOfService.PosPrinterLineStyle
  PosPrinterLineStyle = (
    SingleSolid = 0,
    DoubleSolid = 1,
    Broken = 2,
    Chain = 3
  );
  PPosPrinterLineStyle = ^PosPrinterLineStyle;

  // Windows.Devices.PointOfService.PosPrinterMapMode
  PosPrinterMapMode = (
    Dots = 0,
    Twips = 1,
    English = 2,
    Metric = 3
  );
  PPosPrinterMapMode = ^PosPrinterMapMode;

  // Windows.Devices.PointOfService.PosPrinterMarkFeedCapabilities
  PosPrinterMarkFeedCapabilities = (
    None = 0,
    ToTakeUp = 1,
    ToCutter = 2,
    ToCurrentTopOfForm = 4,
    ToNextTopOfForm = 8
  );
  PPosPrinterMarkFeedCapabilities = ^PosPrinterMarkFeedCapabilities;

  // Windows.Devices.PointOfService.PosPrinterMarkFeedKind
  PosPrinterMarkFeedKind = (
    ToTakeUp = 0,
    ToCutter = 1,
    ToCurrentTopOfForm = 2,
    ToNextTopOfForm = 3
  );
  PPosPrinterMarkFeedKind = ^PosPrinterMarkFeedKind;

  // Windows.Devices.PointOfService.PosPrinterPrintSide
  PosPrinterPrintSide = (
    Unknown = 0,
    Side1 = 1,
    Side2 = 2
  );
  PPosPrinterPrintSide = ^PosPrinterPrintSide;

  // Windows.Devices.PointOfService.PosPrinterRotation
  PosPrinterRotation = (
    Normal = 0,
    Right90 = 1,
    Left90 = 2,
    Rotate180 = 3
  );
  PPosPrinterRotation = ^PosPrinterRotation;

  // Windows.Devices.PointOfService.PosPrinterRuledLineCapabilities
  PosPrinterRuledLineCapabilities = (
    None = 0,
    Horizontal = 1,
    Vertical = 2
  );
  PPosPrinterRuledLineCapabilities = ^PosPrinterRuledLineCapabilities;

  // Windows.Devices.PointOfService.PosPrinterStatusKind
  PosPrinterStatusKind = (
    Online = 0,
    Off = 1,
    Offline = 2,
    OffOrOffline = 3,
    Extended = 4
  );
  PPosPrinterStatusKind = ^PosPrinterStatusKind;

  // Windows.Devices.PointOfService.Provider.BarcodeScannerTriggerState
  Provider_BarcodeScannerTriggerState = (
    Released = 0,
    Pressed = 1
  );
  PProvider_BarcodeScannerTriggerState = ^Provider_BarcodeScannerTriggerState;

  // Windows.Devices.PointOfService.UnifiedPosErrorReason
  UnifiedPosErrorReason = (
    UnknownErrorReason = 0,
    NoService = 1,
    Disabled = 2,
    Illegal = 3,
    NoHardware = 4,
    Closed = 5,
    Offline = 6,
    Failure = 7,
    Timeout = 8,
    Busy = 9,
    Extended = 10
  );
  PUnifiedPosErrorReason = ^UnifiedPosErrorReason;

  // Windows.Devices.PointOfService.UnifiedPosErrorSeverity
  UnifiedPosErrorSeverity = (
    UnknownErrorSeverity = 0,
    Warning = 1,
    Recoverable = 2,
    Unrecoverable = 3,
    AssistanceRequired = 4,
    Fatal = 5
  );
  PUnifiedPosErrorSeverity = ^UnifiedPosErrorSeverity;

  // Windows.Devices.PointOfService.UnifiedPosHealthCheckLevel
  UnifiedPosHealthCheckLevel = (
    UnknownHealthCheckLevel = 0,
    POSInternal = 1,
    &External = 2,
    Interactive = 3
  );
  PUnifiedPosHealthCheckLevel = ^UnifiedPosHealthCheckLevel;

  // Windows.Devices.PointOfService Records
  // Windows.Devices.PointOfService.SizeUInt32
  SizeUInt32 = record
    Width: Cardinal;
    Height: Cardinal;
  end;
  PSizeUInt32 = ^SizeUInt32;

  // Windows.Devices.PointOfService Interfaces

  // Windows.Devices.PointOfService.IBarcodeSymbologyAttributes
  IBarcodeSymbologyAttributes = interface(IInspectable)
  ['{66413A78-AB7A-4ADA-8ECE-936014B2EAD7}']
    function get_IsCheckDigitValidationEnabled: Boolean; safecall;
    procedure put_IsCheckDigitValidationEnabled(value: Boolean); safecall;
    function get_IsCheckDigitValidationSupported: Boolean; safecall;
    function get_IsCheckDigitTransmissionEnabled: Boolean; safecall;
    procedure put_IsCheckDigitTransmissionEnabled(value: Boolean); safecall;
    function get_IsCheckDigitTransmissionSupported: Boolean; safecall;
    function get_DecodeLength1: Cardinal; safecall;
    procedure put_DecodeLength1(value: Cardinal); safecall;
    function get_DecodeLength2: Cardinal; safecall;
    procedure put_DecodeLength2(value: Cardinal); safecall;
    function get_DecodeLengthKind: BarcodeSymbologyDecodeLengthKind; safecall;
    procedure put_DecodeLengthKind(value: BarcodeSymbologyDecodeLengthKind); safecall;
    function get_IsDecodeLengthSupported: Boolean; safecall;
    property DecodeLength1: Cardinal read get_DecodeLength1 write put_DecodeLength1;
    property DecodeLength2: Cardinal read get_DecodeLength2 write put_DecodeLength2;
    property DecodeLengthKind: BarcodeSymbologyDecodeLengthKind read get_DecodeLengthKind write put_DecodeLengthKind;
    property IsCheckDigitTransmissionEnabled: Boolean read get_IsCheckDigitTransmissionEnabled write put_IsCheckDigitTransmissionEnabled;
    property IsCheckDigitTransmissionSupported: Boolean read get_IsCheckDigitTransmissionSupported;
    property IsCheckDigitValidationEnabled: Boolean read get_IsCheckDigitValidationEnabled write put_IsCheckDigitValidationEnabled;
    property IsCheckDigitValidationSupported: Boolean read get_IsCheckDigitValidationSupported;
    property IsDecodeLengthSupported: Boolean read get_IsDecodeLengthSupported;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawerCapabilities
  ICashDrawerCapabilities = interface(IInspectable)
  ['{0BC6DE0B-E8E7-4B1F-B1D1-3E501AD08247}']
    function get_PowerReportingType: UnifiedPosPowerReportingType; safecall;
    function get_IsStatisticsReportingSupported: Boolean; safecall;
    function get_IsStatisticsUpdatingSupported: Boolean; safecall;
    function get_IsStatusReportingSupported: Boolean; safecall;
    function get_IsStatusMultiDrawerDetectSupported: Boolean; safecall;
    function get_IsDrawerOpenSensorAvailable: Boolean; safecall;
    property IsDrawerOpenSensorAvailable: Boolean read get_IsDrawerOpenSensorAvailable;
    property IsStatisticsReportingSupported: Boolean read get_IsStatisticsReportingSupported;
    property IsStatisticsUpdatingSupported: Boolean read get_IsStatisticsUpdatingSupported;
    property IsStatusMultiDrawerDetectSupported: Boolean read get_IsStatusMultiDrawerDetectSupported;
    property IsStatusReportingSupported: Boolean read get_IsStatusReportingSupported;
    property PowerReportingType: UnifiedPosPowerReportingType read get_PowerReportingType;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawerStatus
  ICashDrawerStatus = interface(IInspectable)
  ['{6BBD78BF-DCA1-4E06-99EB-5AF6A5AEC108}']
    function get_StatusKind: CashDrawerStatusKind; safecall;
    function get_ExtendedStatus: Cardinal; safecall;
    property ExtendedStatus: Cardinal read get_ExtendedStatus;
    property StatusKind: CashDrawerStatusKind read get_StatusKind;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawerEventSourceEventArgs
  ICashDrawerEventSourceEventArgs = interface(IInspectable)
  ['{69CB3BC1-147F-421C-9C23-090123BB786C}']
    function get_CashDrawer: ICashDrawer; safecall;
    property CashDrawer: ICashDrawer read get_CashDrawer;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawerEventSource,Windows.Devices.PointOfService.ICashDrawerEventSourceEventArgs>
  TypedEventHandler_2__ICashDrawerEventSource__ICashDrawerEventSourceEventArgs = interface(IUnknown)
  ['{D1995B24-A22E-5BB0-8C77-00AE74F2AB29}']
    procedure Invoke(sender: ICashDrawerEventSource; args: ICashDrawerEventSourceEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawerEventSource
  ICashDrawerEventSource = interface(IInspectable)
  ['{E006E46C-F2F9-442F-8DD6-06C10A4227BA}']
    function add_DrawerClosed(handler: TypedEventHandler_2__ICashDrawerEventSource__ICashDrawerEventSourceEventArgs): EventRegistrationToken; safecall;
    procedure remove_DrawerClosed(token: EventRegistrationToken); safecall;
    function add_DrawerOpened(handler: TypedEventHandler_2__ICashDrawerEventSource__ICashDrawerEventSourceEventArgs): EventRegistrationToken; safecall;
    procedure remove_DrawerOpened(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedCashDrawer>
  AsyncOperationCompletedHandler_1__IClaimedCashDrawer_Delegate_Base = interface(IUnknown)
  ['{E68C3736-FDE7-5CFB-B22F-92119723E729}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IClaimedCashDrawer; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedCashDrawer>
  AsyncOperationCompletedHandler_1__IClaimedCashDrawer = interface(AsyncOperationCompletedHandler_1__IClaimedCashDrawer_Delegate_Base)
  ['{E8192C0D-7D61-53CB-AC73-B8619B1C0121}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedCashDrawer>
  IAsyncOperation_1__IClaimedCashDrawer_Base = interface(IInspectable)
  ['{9230E7AA-20A0-5752-9C20-4BF44934A87E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IClaimedCashDrawer); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IClaimedCashDrawer; safecall;
    function GetResults: IClaimedCashDrawer; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IClaimedCashDrawer read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedCashDrawer>
  IAsyncOperation_1__IClaimedCashDrawer = interface(IAsyncOperation_1__IClaimedCashDrawer_Base)
  ['{DD547670-8167-5132-B38F-2B2590B3D847}']
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawerStatusUpdatedEventArgs
  ICashDrawerStatusUpdatedEventArgs = interface(IInspectable)
  ['{30AAE98A-0D70-459C-9553-87E124C52488}']
    function get_Status: ICashDrawerStatus; safecall;
    property Status: ICashDrawerStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawer,Windows.Devices.PointOfService.ICashDrawerStatusUpdatedEventArgs>
  TypedEventHandler_2__ICashDrawer__ICashDrawerStatusUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{64662EF4-CB0E-5C6F-A820-7D0A769554C9}']
    procedure Invoke(sender: ICashDrawer; args: ICashDrawerStatusUpdatedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawer,Windows.Devices.PointOfService.ICashDrawerStatusUpdatedEventArgs>
  TypedEventHandler_2__ICashDrawer__ICashDrawerStatusUpdatedEventArgs = interface(TypedEventHandler_2__ICashDrawer__ICashDrawerStatusUpdatedEventArgs_Delegate_Base)
  ['{518BE39A-2654-5D10-8426-B50C2B52B087}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawer
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_CashDrawer)]
  ICashDrawer = interface(IInspectable)
  ['{9F88F5C8-DE54-4AEE-A890-920BCBFE30FC}']
    function get_DeviceId: HSTRING; safecall;
    function get_Capabilities: ICashDrawerCapabilities; safecall;
    function get_Status: ICashDrawerStatus; safecall;
    function get_IsDrawerOpen: Boolean; safecall;
    function get_DrawerEventSource: ICashDrawerEventSource; safecall;
    function ClaimDrawerAsync: IAsyncOperation_1__IClaimedCashDrawer; safecall;
    function CheckHealthAsync(level: UnifiedPosHealthCheckLevel): IAsyncOperation_1__HSTRING; safecall;
    function GetStatisticsAsync(statisticsCategories: IIterable_1__HSTRING): IAsyncOperation_1__HSTRING; safecall;
    function add_StatusUpdated(handler: TypedEventHandler_2__ICashDrawer__ICashDrawerStatusUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StatusUpdated(token: EventRegistrationToken); safecall;
    property Capabilities: ICashDrawerCapabilities read get_Capabilities;
    property DeviceId: HSTRING read get_DeviceId;
    property DrawerEventSource: ICashDrawerEventSource read get_DrawerEventSource;
    property IsDrawerOpen: Boolean read get_IsDrawerOpen;
    property Status: ICashDrawerStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ICashDrawer>
  AsyncOperationCompletedHandler_1__ICashDrawer_Delegate_Base = interface(IUnknown)
  ['{57836710-F186-5636-891D-F8C5398EA6DF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ICashDrawer; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ICashDrawer>
  AsyncOperationCompletedHandler_1__ICashDrawer = interface(AsyncOperationCompletedHandler_1__ICashDrawer_Delegate_Base)
  ['{2B5A22BA-CB98-5A1C-BED6-09440AC4C5D9}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ICashDrawer>
  IAsyncOperation_1__ICashDrawer_Base = interface(IInspectable)
  ['{45007467-92F2-5BFF-B191-AA5000FEDD9E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ICashDrawer); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ICashDrawer; safecall;
    function GetResults: ICashDrawer; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ICashDrawer read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ICashDrawer>
  IAsyncOperation_1__ICashDrawer = interface(IAsyncOperation_1__ICashDrawer_Base)
  ['{F0126938-62B0-590E-AE08-DA21554EAD22}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawerStatics
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_CashDrawer)]
  ICashDrawerStatics = interface(IInspectable)
  ['{DFA0955A-D437-4FFF-B547-DDA969A4F883}']
    function GetDefaultAsync: IAsyncOperation_1__ICashDrawer; safecall;
    function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__ICashDrawer; safecall;
    function GetDeviceSelector: HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.ICashDrawerStatics2
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_CashDrawer)]
  ICashDrawerStatics2 = interface(IInspectable)
  ['{3E818121-8C42-40E8-9C0E-40297048104C}']
    function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IBarcodeSymbologyAttributes>
  AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes_Delegate_Base = interface(IUnknown)
  ['{F8FC5A52-2F45-5E46-A82E-3DA009573B5C}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IBarcodeSymbologyAttributes; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IBarcodeSymbologyAttributes>
  AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes = interface(AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes_Delegate_Base)
  ['{6AF8902C-E82F-5911-BB12-560BE52961F7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IBarcodeSymbologyAttributes>
  IAsyncOperation_1__IBarcodeSymbologyAttributes_Base = interface(IInspectable)
  ['{461550CA-7BB3-5ADE-A642-61B8FEF2E35C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes; safecall;
    function GetResults: IBarcodeSymbologyAttributes; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IBarcodeSymbologyAttributes read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IBarcodeSymbologyAttributes>
  IAsyncOperation_1__IBarcodeSymbologyAttributes = interface(IAsyncOperation_1__IBarcodeSymbologyAttributes_Base)
  ['{F101BC12-1822-5F55-8F20-4CEEA844E880}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterPrintOptions
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_PosPrinterPrintOptions)]
  IPosPrinterPrintOptions = interface(IInspectable)
  ['{0A2E16FD-1D02-5A58-9D59-BFCDE76FDE86}']
    function get_TypeFace: HSTRING; safecall;
    procedure put_TypeFace(value: HSTRING); safecall;
    function get_CharacterHeight: Cardinal; safecall;
    procedure put_CharacterHeight(value: Cardinal); safecall;
    function get_Bold: Boolean; safecall;
    procedure put_Bold(value: Boolean); safecall;
    function get_Italic: Boolean; safecall;
    procedure put_Italic(value: Boolean); safecall;
    function get_Underline: Boolean; safecall;
    procedure put_Underline(value: Boolean); safecall;
    function get_ReverseVideo: Boolean; safecall;
    procedure put_ReverseVideo(value: Boolean); safecall;
    function get_Strikethrough: Boolean; safecall;
    procedure put_Strikethrough(value: Boolean); safecall;
    function get_Superscript: Boolean; safecall;
    procedure put_Superscript(value: Boolean); safecall;
    function get_Subscript: Boolean; safecall;
    procedure put_Subscript(value: Boolean); safecall;
    function get_DoubleWide: Boolean; safecall;
    procedure put_DoubleWide(value: Boolean); safecall;
    function get_DoubleHigh: Boolean; safecall;
    procedure put_DoubleHigh(value: Boolean); safecall;
    function get_Alignment: PosPrinterAlignment; safecall;
    procedure put_Alignment(value: PosPrinterAlignment); safecall;
    function get_CharacterSet: Cardinal; safecall;
    procedure put_CharacterSet(value: Cardinal); safecall;
    property Alignment: PosPrinterAlignment read get_Alignment write put_Alignment;
    property Bold: Boolean read get_Bold write put_Bold;
    property CharacterHeight: Cardinal read get_CharacterHeight write put_CharacterHeight;
    property CharacterSet: Cardinal read get_CharacterSet write put_CharacterSet;
    property DoubleHigh: Boolean read get_DoubleHigh write put_DoubleHigh;
    property DoubleWide: Boolean read get_DoubleWide write put_DoubleWide;
    property Italic: Boolean read get_Italic write put_Italic;
    property ReverseVideo: Boolean read get_ReverseVideo write put_ReverseVideo;
    property Strikethrough: Boolean read get_Strikethrough write put_Strikethrough;
    property Subscript: Boolean read get_Subscript write put_Subscript;
    property Superscript: Boolean read get_Superscript write put_Superscript;
    property TypeFace: HSTRING read get_TypeFace write put_TypeFace;
    property Underline: Boolean read get_Underline write put_Underline;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IJournalPrintJob
  IJournalPrintJob = interface(IInspectable)
  ['{9F4F2864-F3F0-55D0-8C39-74CC91783EED}']
    procedure Print(data: HSTRING; printOptions: IPosPrinterPrintOptions); safecall;
    procedure FeedPaperByLine(lineCount: Integer); safecall;
    procedure FeedPaperByMapModeUnit(distance: Integer); safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedJournalPrinter
  IClaimedJournalPrinter = interface(IInspectable)
  ['{67EA0630-517D-487F-9FDF-D2E0A0A264A5}']
    function CreateJob: IJournalPrintJob; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.LineDisplayPowerStatus>
  AsyncOperationCompletedHandler_1__LineDisplayPowerStatus_Delegate_Base = interface(IUnknown)
  ['{ABBA6D19-D81F-5D85-A900-66B96B6D2B32}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__LineDisplayPowerStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.LineDisplayPowerStatus>
  AsyncOperationCompletedHandler_1__LineDisplayPowerStatus = interface(AsyncOperationCompletedHandler_1__LineDisplayPowerStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.LineDisplayPowerStatus>
  IAsyncOperation_1__LineDisplayPowerStatus_Base = interface(IInspectable)
  ['{363EEDCD-7922-5503-9009-1C631C9E3653}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__LineDisplayPowerStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__LineDisplayPowerStatus; safecall;
    function GetResults: LineDisplayPowerStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__LineDisplayPowerStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.LineDisplayPowerStatus>
  IAsyncOperation_1__LineDisplayPowerStatus = interface(IAsyncOperation_1__LineDisplayPowerStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplayStatusUpdatedEventArgs
  ILineDisplayStatusUpdatedEventArgs = interface(IInspectable)
  ['{DDD57C1A-86FB-4EBA-93D1-6F5EDA52B752}']
    function get_Status: LineDisplayPowerStatus; safecall;
    property Status: LineDisplayPowerStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Windows.Devices.PointOfService.ILineDisplayStatusUpdatedEventArgs>
  TypedEventHandler_2__IClaimedLineDisplay__ILineDisplayStatusUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{25D178FF-3069-536C-A0C2-88E0250E8A29}']
    procedure Invoke(sender: IClaimedLineDisplay; args: ILineDisplayStatusUpdatedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Windows.Devices.PointOfService.ILineDisplayStatusUpdatedEventArgs>
  TypedEventHandler_2__IClaimedLineDisplay__ILineDisplayStatusUpdatedEventArgs = interface(TypedEventHandler_2__IClaimedLineDisplay__ILineDisplayStatusUpdatedEventArgs_Delegate_Base)
  ['{675A25E3-19A3-55E0-990E-CFBD632632F5}']
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplayCustomGlyphs
  ILineDisplayCustomGlyphs = interface(IInspectable)
  ['{2257F63C-F263-44F1-A1A0-E750A6A0EC54}']
    function get_SizeInPixels: TSizeF; safecall;
    function get_SupportedGlyphCodes: IVectorView_1__Cardinal; safecall;
    function TryRedefineAsync(glyphCode: Cardinal; glyphData: IBuffer): IAsyncOperation_1__Boolean; safecall;
    property SizeInPixels: TSizeF read get_SizeInPixels;
    property SupportedGlyphCodes: IVectorView_1__Cardinal read get_SupportedGlyphCodes;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplayAttributes
  ILineDisplayAttributes = interface(IInspectable)
  ['{C17DE99C-229A-4C14-A6F1-B4E4B1FEAD92}']
    function get_IsPowerNotifyEnabled: Boolean; safecall;
    procedure put_IsPowerNotifyEnabled(value: Boolean); safecall;
    function get_Brightness: Integer; safecall;
    procedure put_Brightness(value: Integer); safecall;
    function get_BlinkRate: TimeSpan; safecall;
    procedure put_BlinkRate(value: TimeSpan); safecall;
    function get_ScreenSizeInCharacters: TSizeF; safecall;
    procedure put_ScreenSizeInCharacters(value: TSizeF); safecall;
    function get_CharacterSet: Integer; safecall;
    procedure put_CharacterSet(value: Integer); safecall;
    function get_IsCharacterSetMappingEnabled: Boolean; safecall;
    procedure put_IsCharacterSetMappingEnabled(value: Boolean); safecall;
    function get_CurrentWindow: ILineDisplayWindow; safecall;
    procedure put_CurrentWindow(value: ILineDisplayWindow); safecall;
    property BlinkRate: TimeSpan read get_BlinkRate write put_BlinkRate;
    property Brightness: Integer read get_Brightness write put_Brightness;
    property CharacterSet: Integer read get_CharacterSet write put_CharacterSet;
    property CurrentWindow: ILineDisplayWindow read get_CurrentWindow write put_CurrentWindow;
    property IsCharacterSetMappingEnabled: Boolean read get_IsCharacterSetMappingEnabled write put_IsCharacterSetMappingEnabled;
    property IsPowerNotifyEnabled: Boolean read get_IsPowerNotifyEnabled write put_IsPowerNotifyEnabled;
    property ScreenSizeInCharacters: TSizeF read get_ScreenSizeInCharacters write put_ScreenSizeInCharacters;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplayWindow>
  AsyncOperationCompletedHandler_1__ILineDisplayWindow_Delegate_Base = interface(IUnknown)
  ['{E4D37B02-B65A-5AEC-A219-D1E0B7F3F912}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ILineDisplayWindow; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplayWindow>
  AsyncOperationCompletedHandler_1__ILineDisplayWindow = interface(AsyncOperationCompletedHandler_1__ILineDisplayWindow_Delegate_Base)
  ['{112D7900-B341-5267-ABCA-9A98C793E3ED}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplayWindow>
  IAsyncOperation_1__ILineDisplayWindow_Base = interface(IInspectable)
  ['{9755F05B-64CC-5051-8350-4ACF1FFCBE58}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ILineDisplayWindow); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ILineDisplayWindow; safecall;
    function GetResults: ILineDisplayWindow; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ILineDisplayWindow read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplayWindow>
  IAsyncOperation_1__ILineDisplayWindow = interface(IAsyncOperation_1__ILineDisplayWindow_Base)
  ['{CCD6F7A8-3DA3-5281-AFF2-38347C529B11}']
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplayStoredBitmap
  ILineDisplayStoredBitmap = interface(IInspectable)
  ['{F621515B-D81E-43BA-BF1B-BCFA3C785BA0}']
    function get_EscapeSequence: HSTRING; safecall;
    function TryDeleteAsync: IAsyncOperation_1__Boolean; safecall;
    property EscapeSequence: HSTRING read get_EscapeSequence;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplayStoredBitmap>
  AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap_Delegate_Base = interface(IUnknown)
  ['{A576FA69-9988-5A23-844C-F8A69F48A429}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ILineDisplayStoredBitmap; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplayStoredBitmap>
  AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap = interface(AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap_Delegate_Base)
  ['{1E9170AB-D6FE-5ED2-A6BC-3F00D962A9E8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplayStoredBitmap>
  IAsyncOperation_1__ILineDisplayStoredBitmap_Base = interface(IInspectable)
  ['{DDA5D77D-B7A1-541D-A480-3D46BED98E9D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap; safecall;
    function GetResults: ILineDisplayStoredBitmap; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ILineDisplayStoredBitmap read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplayStoredBitmap>
  IAsyncOperation_1__ILineDisplayStoredBitmap = interface(IAsyncOperation_1__ILineDisplayStoredBitmap_Base)
  ['{06E90F61-7265-5591-ADFE-DB01986232F7}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedLineDisplay2
  IClaimedLineDisplay2 = interface(IInspectable)
  ['{A31C75ED-41F5-4E76-A074-795E47A46E97}']
    function GetStatisticsAsync(statisticsCategories: IIterable_1__HSTRING): IAsyncOperation_1__HSTRING; safecall;
    function CheckHealthAsync(level: UnifiedPosHealthCheckLevel): IAsyncOperation_1__HSTRING; safecall;
    function CheckPowerStatusAsync: IAsyncOperation_1__LineDisplayPowerStatus; safecall;
    function add_StatusUpdated(handler: TypedEventHandler_2__IClaimedLineDisplay__ILineDisplayStatusUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StatusUpdated(token: EventRegistrationToken); safecall;
    function get_SupportedScreenSizesInCharacters: IVectorView_1__Size; safecall;
    function get_MaxBitmapSizeInPixels: TSizeF; safecall;
    function get_SupportedCharacterSets: IVectorView_1__Integer; safecall;
    function get_CustomGlyphs: ILineDisplayCustomGlyphs; safecall;
    function GetAttributes: ILineDisplayAttributes; safecall;
    function TryUpdateAttributesAsync(attributes: ILineDisplayAttributes): IAsyncOperation_1__Boolean; safecall;
    function TrySetDescriptorAsync(descriptor: Cardinal; descriptorState: LineDisplayDescriptorState): IAsyncOperation_1__Boolean; safecall;
    function TryClearDescriptorsAsync: IAsyncOperation_1__Boolean; safecall;
    function TryCreateWindowAsync(viewport: TRectF; windowSize: TSizeF): IAsyncOperation_1__ILineDisplayWindow; safecall;
    function TryStoreStorageFileBitmapAsync(bitmap: IStorageFile): IAsyncOperation_1__ILineDisplayStoredBitmap; overload; safecall;
    function TryStoreStorageFileBitmapAsync(bitmap: IStorageFile; horizontalAlignment: LineDisplayHorizontalAlignment; verticalAlignment: LineDisplayVerticalAlignment): IAsyncOperation_1__ILineDisplayStoredBitmap; overload; safecall;
    function TryStoreStorageFileBitmapAsync(bitmap: IStorageFile; horizontalAlignment: LineDisplayHorizontalAlignment; verticalAlignment: LineDisplayVerticalAlignment; widthInPixels: Integer): IAsyncOperation_1__ILineDisplayStoredBitmap; overload; safecall;
    property CustomGlyphs: ILineDisplayCustomGlyphs read get_CustomGlyphs;
    property MaxBitmapSizeInPixels: TSizeF read get_MaxBitmapSizeInPixels;
    property SupportedCharacterSets: IVectorView_1__Integer read get_SupportedCharacterSets;
    property SupportedScreenSizesInCharacters: IVectorView_1__Size read get_SupportedScreenSizesInCharacters;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedLineDisplayClosedEventArgs
  IClaimedLineDisplayClosedEventArgs = interface(IInspectable)
  ['{F915F364-D3D5-4F10-B511-90939EDFACD8}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Windows.Devices.PointOfService.IClaimedLineDisplayClosedEventArgs>
  TypedEventHandler_2__IClaimedLineDisplay__IClaimedLineDisplayClosedEventArgs = interface(IUnknown)
  ['{A2AA517D-717A-5A40-A8F5-BC47834C27C3}']
    procedure Invoke(sender: IClaimedLineDisplay; args: IClaimedLineDisplayClosedEventArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedLineDisplay3
  IClaimedLineDisplay3 = interface(IInspectable)
  ['{642ECD92-E9D4-4ECC-AF75-329C274CD18F}']
    function add_Closed(handler: TypedEventHandler_2__IClaimedLineDisplay__IClaimedLineDisplayClosedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedLineDisplay>
  AsyncOperationCompletedHandler_1__IClaimedLineDisplay_Delegate_Base = interface(IUnknown)
  ['{4E1A79F1-DFF2-5B18-BEBE-2ACA010BBFCC}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IClaimedLineDisplay; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedLineDisplay>
  AsyncOperationCompletedHandler_1__IClaimedLineDisplay = interface(AsyncOperationCompletedHandler_1__IClaimedLineDisplay_Delegate_Base)
  ['{E6E53C79-74CE-5295-ACD8-26F252E31920}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedLineDisplay>
  IAsyncOperation_1__IClaimedLineDisplay_Base = interface(IInspectable)
  ['{4BD0A904-1AAA-545A-8CBC-1D45C3E80E5E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IClaimedLineDisplay); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IClaimedLineDisplay; safecall;
    function GetResults: IClaimedLineDisplay; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IClaimedLineDisplay read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedLineDisplay>
  IAsyncOperation_1__IClaimedLineDisplay = interface(IAsyncOperation_1__IClaimedLineDisplay_Base)
  ['{E22B8EFE-315E-5EFD-A8B9-62A55C61450C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedLineDisplayStatics
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_ClaimedLineDisplay)]
  IClaimedLineDisplayStatics = interface(IInspectable)
  ['{78CA98FB-8B6B-4973-86F0-3E570C351825}']
    function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IClaimedLineDisplay; safecall;
    function GetDeviceSelector: HSTRING; overload; safecall;
    function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IReceiptPrintJob
  IReceiptPrintJob = interface(IInspectable)
  ['{AA96066E-ACAD-4B79-9D0F-C0CFC08DC77B}']
    procedure MarkFeed(kind: PosPrinterMarkFeedKind); safecall;
    procedure CutPaper(percentage: Double); overload; safecall;
    procedure CutPaper; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedReceiptPrinter
  IClaimedReceiptPrinter = interface(IInspectable)
  ['{9AD27A74-DD61-4EE2-9837-5B5D72D538B9}']
    function get_SidewaysMaxLines: Cardinal; safecall;
    function get_SidewaysMaxChars: Cardinal; safecall;
    function get_LinesToPaperCut: Cardinal; safecall;
    function get_PageSize: TSizeF; safecall;
    function get_PrintArea: TRectF; safecall;
    function CreateJob: IReceiptPrintJob; safecall;
    property LinesToPaperCut: Cardinal read get_LinesToPaperCut;
    property PageSize: TSizeF read get_PageSize;
    property PrintArea: TRectF read get_PrintArea;
    property SidewaysMaxChars: Cardinal read get_SidewaysMaxChars;
    property SidewaysMaxLines: Cardinal read get_SidewaysMaxLines;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ISlipPrintJob
  ISlipPrintJob = interface(IInspectable)
  ['{5D88F95D-6131-5A4B-B7D5-8EF2DA7B4165}']
    procedure Print(data: HSTRING; printOptions: IPosPrinterPrintOptions); safecall;
    procedure FeedPaperByLine(lineCount: Integer); safecall;
    procedure FeedPaperByMapModeUnit(distance: Integer); safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedSlipPrinter
  IClaimedSlipPrinter = interface(IInspectable)
  ['{BD5DEFF2-AF90-4E8A-B77B-E3AE9CA63A7F}']
    function get_SidewaysMaxLines: Cardinal; safecall;
    function get_SidewaysMaxChars: Cardinal; safecall;
    function get_MaxLines: Cardinal; safecall;
    function get_LinesNearEndToEnd: Cardinal; safecall;
    function get_PrintSide: PosPrinterPrintSide; safecall;
    function get_PageSize: TSizeF; safecall;
    function get_PrintArea: TRectF; safecall;
    procedure OpenJaws; safecall;
    procedure CloseJaws; safecall;
    function InsertSlipAsync(timeout: TimeSpan): IAsyncOperation_1__Boolean; safecall;
    function RemoveSlipAsync(timeout: TimeSpan): IAsyncOperation_1__Boolean; safecall;
    procedure ChangePrintSide(printSide: PosPrinterPrintSide); safecall;
    function CreateJob: ISlipPrintJob; safecall;
    property LinesNearEndToEnd: Cardinal read get_LinesNearEndToEnd;
    property MaxLines: Cardinal read get_MaxLines;
    property PageSize: TSizeF read get_PageSize;
    property PrintArea: TRectF read get_PrintArea;
    property PrintSide: PosPrinterPrintSide read get_PrintSide;
    property SidewaysMaxChars: Cardinal read get_SidewaysMaxChars;
    property SidewaysMaxLines: Cardinal read get_SidewaysMaxLines;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterReleaseDeviceRequestedEventArgs
  IPosPrinterReleaseDeviceRequestedEventArgs = interface(IInspectable)
  ['{2BCBA359-1CEF-40B2-9ECB-F927F856AE3C}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedPosPrinter,Windows.Devices.PointOfService.IPosPrinterReleaseDeviceRequestedEventArgs>
  TypedEventHandler_2__IClaimedPosPrinter__IPosPrinterReleaseDeviceRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{31424F6F-CFEB-5031-8A95-BEA59B09E584}']
    procedure Invoke(sender: IClaimedPosPrinter; args: IPosPrinterReleaseDeviceRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedPosPrinter,Windows.Devices.PointOfService.IPosPrinterReleaseDeviceRequestedEventArgs>
  TypedEventHandler_2__IClaimedPosPrinter__IPosPrinterReleaseDeviceRequestedEventArgs = interface(TypedEventHandler_2__IClaimedPosPrinter__IPosPrinterReleaseDeviceRequestedEventArgs_Delegate_Base)
  ['{2651A22A-9DCF-598E-B807-BE46B471C78C}']
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IClaimedPosPrinter
  IClaimedPosPrinter = interface(IInspectable)
  ['{6D64CE0C-E03E-4B14-A38E-C28C34B86353}']
    function get_DeviceId: HSTRING; safecall;
    function get_IsEnabled: Boolean; safecall;
    procedure put_CharacterSet(value: Cardinal); safecall;
    function get_CharacterSet: Cardinal; safecall;
    function get_IsCoverOpen: Boolean; safecall;
    procedure put_IsCharacterSetMappingEnabled(value: Boolean); safecall;
    function get_IsCharacterSetMappingEnabled: Boolean; safecall;
    procedure put_MapMode(value: PosPrinterMapMode); safecall;
    function get_MapMode: PosPrinterMapMode; safecall;
    function get_Receipt: IClaimedReceiptPrinter; safecall;
    function get_Slip: IClaimedSlipPrinter; safecall;
    function get_Journal: IClaimedJournalPrinter; safecall;
    function EnableAsync: IAsyncOperation_1__Boolean; safecall;
    function DisableAsync: IAsyncOperation_1__Boolean; safecall;
    function RetainDeviceAsync: IAsyncOperation_1__Boolean; safecall;
    function ResetStatisticsAsync(statisticsCategories: IIterable_1__HSTRING): IAsyncOperation_1__Boolean; safecall;
    function UpdateStatisticsAsync(statistics: IIterable_1__IKeyValuePair_2__HSTRING__HSTRING): IAsyncOperation_1__Boolean; safecall;
    function add_ReleaseDeviceRequested(handler: TypedEventHandler_2__IClaimedPosPrinter__IPosPrinterReleaseDeviceRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ReleaseDeviceRequested(token: EventRegistrationToken); safecall;
    property CharacterSet: Cardinal read get_CharacterSet write put_CharacterSet;
    property DeviceId: HSTRING read get_DeviceId;
    property IsCharacterSetMappingEnabled: Boolean read get_IsCharacterSetMappingEnabled write put_IsCharacterSetMappingEnabled;
    property IsCoverOpen: Boolean read get_IsCoverOpen;
    property IsEnabled: Boolean read get_IsEnabled;
    property Journal: IClaimedJournalPrinter read get_Journal;
    property MapMode: PosPrinterMapMode read get_MapMode write put_MapMode;
    property Receipt: IClaimedReceiptPrinter read get_Receipt;
    property Slip: IClaimedSlipPrinter read get_Slip;
  end;

  // Windows.Devices.PointOfService.ICommonClaimedPosPrinterStation
  ICommonClaimedPosPrinterStation = interface(IInspectable)
  ['{B7EB66A8-FE8A-4CFB-8B42-E35B280CB27C}']
    procedure put_CharactersPerLine(value: Cardinal); safecall;
    function get_CharactersPerLine: Cardinal; safecall;
    procedure put_LineHeight(value: Cardinal); safecall;
    function get_LineHeight: Cardinal; safecall;
    procedure put_LineSpacing(value: Cardinal); safecall;
    function get_LineSpacing: Cardinal; safecall;
    function get_LineWidth: Cardinal; safecall;
    procedure put_IsLetterQuality(value: Boolean); safecall;
    function get_IsLetterQuality: Boolean; safecall;
    function get_IsPaperNearEnd: Boolean; safecall;
    procedure put_ColorCartridge(value: PosPrinterColorCartridge); safecall;
    function get_ColorCartridge: PosPrinterColorCartridge; safecall;
    function get_IsCoverOpen: Boolean; safecall;
    function get_IsCartridgeRemoved: Boolean; safecall;
    function get_IsCartridgeEmpty: Boolean; safecall;
    function get_IsHeadCleaning: Boolean; safecall;
    function get_IsPaperEmpty: Boolean; safecall;
    function get_IsReadyToPrint: Boolean; safecall;
    function ValidateData(data: HSTRING): Boolean; safecall;
    property CharactersPerLine: Cardinal read get_CharactersPerLine write put_CharactersPerLine;
    property ColorCartridge: PosPrinterColorCartridge read get_ColorCartridge write put_ColorCartridge;
    property IsCartridgeEmpty: Boolean read get_IsCartridgeEmpty;
    property IsCartridgeRemoved: Boolean read get_IsCartridgeRemoved;
    property IsCoverOpen: Boolean read get_IsCoverOpen;
    property IsHeadCleaning: Boolean read get_IsHeadCleaning;
    property IsLetterQuality: Boolean read get_IsLetterQuality write put_IsLetterQuality;
    property IsPaperEmpty: Boolean read get_IsPaperEmpty;
    property IsPaperNearEnd: Boolean read get_IsPaperNearEnd;
    property IsReadyToPrint: Boolean read get_IsReadyToPrint;
    property LineHeight: Cardinal read get_LineHeight write put_LineHeight;
    property LineSpacing: Cardinal read get_LineSpacing write put_LineSpacing;
    property LineWidth: Cardinal read get_LineWidth;
  end;

  // Windows.Devices.PointOfService.ICommonPosPrintStationCapabilities
  ICommonPosPrintStationCapabilities = interface(IInspectable)
  ['{DE5B52CA-E02E-40E9-9E5E-1B488E6AACFC}']
    function get_IsPrinterPresent: Boolean; safecall;
    function get_IsDualColorSupported: Boolean; safecall;
    function get_ColorCartridgeCapabilities: PosPrinterColorCapabilities; safecall;
    function get_CartridgeSensors: PosPrinterCartridgeSensors; safecall;
    function get_IsBoldSupported: Boolean; safecall;
    function get_IsItalicSupported: Boolean; safecall;
    function get_IsUnderlineSupported: Boolean; safecall;
    function get_IsDoubleHighPrintSupported: Boolean; safecall;
    function get_IsDoubleWidePrintSupported: Boolean; safecall;
    function get_IsDoubleHighDoubleWidePrintSupported: Boolean; safecall;
    function get_IsPaperEmptySensorSupported: Boolean; safecall;
    function get_IsPaperNearEndSensorSupported: Boolean; safecall;
    function get_SupportedCharactersPerLine: IVectorView_1__Cardinal; safecall;
    property CartridgeSensors: PosPrinterCartridgeSensors read get_CartridgeSensors;
    property ColorCartridgeCapabilities: PosPrinterColorCapabilities read get_ColorCartridgeCapabilities;
    property IsBoldSupported: Boolean read get_IsBoldSupported;
    property IsDoubleHighDoubleWidePrintSupported: Boolean read get_IsDoubleHighDoubleWidePrintSupported;
    property IsDoubleHighPrintSupported: Boolean read get_IsDoubleHighPrintSupported;
    property IsDoubleWidePrintSupported: Boolean read get_IsDoubleWidePrintSupported;
    property IsDualColorSupported: Boolean read get_IsDualColorSupported;
    property IsItalicSupported: Boolean read get_IsItalicSupported;
    property IsPaperEmptySensorSupported: Boolean read get_IsPaperEmptySensorSupported;
    property IsPaperNearEndSensorSupported: Boolean read get_IsPaperNearEndSensorSupported;
    property IsPrinterPresent: Boolean read get_IsPrinterPresent;
    property IsUnderlineSupported: Boolean read get_IsUnderlineSupported;
    property SupportedCharactersPerLine: IVectorView_1__Cardinal read get_SupportedCharactersPerLine;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IIterator_1__PosPrinterRotation_Base = interface(IInspectable)
  ['{F687EF7C-B11E-56C3-9158-E6BA15BD521B}']
    function get_Current: PosPrinterRotation; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPosPrinterRotation): Cardinal; safecall;
    property Current: PosPrinterRotation read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IIterator_1__PosPrinterRotation = interface(IIterator_1__PosPrinterRotation_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IIterable_1__PosPrinterRotation_Base = interface(IInspectable)
  ['{1B1E4D8D-15F5-5802-9B23-8B75CE2A58C5}']
    function First: IIterator_1__PosPrinterRotation; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IIterable_1__PosPrinterRotation = interface(IIterable_1__PosPrinterRotation_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.PointOfService.PosPrinterRotation>
  IVectorView_1__PosPrinterRotation = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): PosPrinterRotation; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: PosPrinterRotation; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPosPrinterRotation): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Devices.PointOfService.ICommonReceiptSlipCapabilities
  ICommonReceiptSlipCapabilities = interface(IInspectable)
  ['{09286B8B-9873-4D05-BFBE-4727A6038F69}']
    function get_IsBarcodeSupported: Boolean; safecall;
    function get_IsBitmapSupported: Boolean; safecall;
    function get_IsLeft90RotationSupported: Boolean; safecall;
    function get_IsRight90RotationSupported: Boolean; safecall;
    function get_Is180RotationSupported: Boolean; safecall;
    function get_IsPrintAreaSupported: Boolean; safecall;
    function get_RuledLineCapabilities: PosPrinterRuledLineCapabilities; safecall;
    function get_SupportedBarcodeRotations: IVectorView_1__PosPrinterRotation; safecall;
    function get_SupportedBitmapRotations: IVectorView_1__PosPrinterRotation; safecall;
    property Is180RotationSupported: Boolean read get_Is180RotationSupported;
    property IsBarcodeSupported: Boolean read get_IsBarcodeSupported;
    property IsBitmapSupported: Boolean read get_IsBitmapSupported;
    property IsLeft90RotationSupported: Boolean read get_IsLeft90RotationSupported;
    property IsPrintAreaSupported: Boolean read get_IsPrintAreaSupported;
    property IsRight90RotationSupported: Boolean read get_IsRight90RotationSupported;
    property RuledLineCapabilities: PosPrinterRuledLineCapabilities read get_RuledLineCapabilities;
    property SupportedBarcodeRotations: IVectorView_1__PosPrinterRotation read get_SupportedBarcodeRotations;
    property SupportedBitmapRotations: IVectorView_1__PosPrinterRotation read get_SupportedBitmapRotations;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IJournalPrinterCapabilities
  IJournalPrinterCapabilities = interface(IInspectable)
  ['{3B5CCC43-E047-4463-BB58-17B5BA1D8056}']
  end;

  // Windows.Devices.PointOfService.IJournalPrinterCapabilities2
  IJournalPrinterCapabilities2 = interface(IInspectable)
  ['{03B0B645-33B8-533B-BAAA-A4389283AB0A}']
    function get_IsReverseVideoSupported: Boolean; safecall;
    function get_IsStrikethroughSupported: Boolean; safecall;
    function get_IsSuperscriptSupported: Boolean; safecall;
    function get_IsSubscriptSupported: Boolean; safecall;
    function get_IsReversePaperFeedByLineSupported: Boolean; safecall;
    function get_IsReversePaperFeedByMapModeUnitSupported: Boolean; safecall;
    property IsReversePaperFeedByLineSupported: Boolean read get_IsReversePaperFeedByLineSupported;
    property IsReversePaperFeedByMapModeUnitSupported: Boolean read get_IsReversePaperFeedByMapModeUnitSupported;
    property IsReverseVideoSupported: Boolean read get_IsReverseVideoSupported;
    property IsStrikethroughSupported: Boolean read get_IsStrikethroughSupported;
    property IsSubscriptSupported: Boolean read get_IsSubscriptSupported;
    property IsSuperscriptSupported: Boolean read get_IsSuperscriptSupported;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplay
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_LineDisplay)]
  ILineDisplay = interface(IInspectable)
  ['{24F5DF4E-3C99-44E2-B73F-E51BE3637A8C}']
    function get_DeviceId: HSTRING; safecall;
    function get_Capabilities: ILineDisplayCapabilities; safecall;
    function get_PhysicalDeviceName: HSTRING; safecall;
    function get_PhysicalDeviceDescription: HSTRING; safecall;
    function get_DeviceControlDescription: HSTRING; safecall;
    function get_DeviceControlVersion: HSTRING; safecall;
    function get_DeviceServiceVersion: HSTRING; safecall;
    function ClaimAsync: IAsyncOperation_1__IClaimedLineDisplay; safecall;
    property Capabilities: ILineDisplayCapabilities read get_Capabilities;
    property DeviceControlDescription: HSTRING read get_DeviceControlDescription;
    property DeviceControlVersion: HSTRING read get_DeviceControlVersion;
    property DeviceId: HSTRING read get_DeviceId;
    property DeviceServiceVersion: HSTRING read get_DeviceServiceVersion;
    property PhysicalDeviceDescription: HSTRING read get_PhysicalDeviceDescription;
    property PhysicalDeviceName: HSTRING read get_PhysicalDeviceName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplay2
  ILineDisplay2 = interface(IInspectable)
  ['{C296A628-EF44-40F3-BD1C-B04C6A5CDC7D}']
    function CheckPowerStatusAsync: IAsyncOperation_1__LineDisplayPowerStatus; safecall;
  end;

  // Windows.Devices.PointOfService.ILineDisplayCursorAttributes
  ILineDisplayCursorAttributes = interface(IInspectable)
  ['{4E2D54FE-4FFD-4190-AAE1-CE285F20C896}']
    function get_IsBlinkEnabled: Boolean; safecall;
    procedure put_IsBlinkEnabled(value: Boolean); safecall;
    function get_CursorType: LineDisplayCursorType; safecall;
    procedure put_CursorType(value: LineDisplayCursorType); safecall;
    function get_IsAutoAdvanceEnabled: Boolean; safecall;
    procedure put_IsAutoAdvanceEnabled(value: Boolean); safecall;
    function get_Position: TPointF; safecall;
    procedure put_Position(value: TPointF); safecall;
    property CursorType: LineDisplayCursorType read get_CursorType write put_CursorType;
    property IsAutoAdvanceEnabled: Boolean read get_IsAutoAdvanceEnabled write put_IsAutoAdvanceEnabled;
    property IsBlinkEnabled: Boolean read get_IsBlinkEnabled write put_IsBlinkEnabled;
    property Position: TPointF read get_Position write put_Position;
  end;

  // Windows.Devices.PointOfService.ILineDisplayCursor
  ILineDisplayCursor = interface(IInspectable)
  ['{ECDFFC45-754A-4E3B-AB2B-151181085605}']
    function get_CanCustomize: Boolean; safecall;
    function get_IsBlinkSupported: Boolean; safecall;
    function get_IsBlockSupported: Boolean; safecall;
    function get_IsHalfBlockSupported: Boolean; safecall;
    function get_IsUnderlineSupported: Boolean; safecall;
    function get_IsReverseSupported: Boolean; safecall;
    function get_IsOtherSupported: Boolean; safecall;
    function GetAttributes: ILineDisplayCursorAttributes; safecall;
    function TryUpdateAttributesAsync(attributes: ILineDisplayCursorAttributes): IAsyncOperation_1__Boolean; safecall;
    property CanCustomize: Boolean read get_CanCustomize;
    property IsBlinkSupported: Boolean read get_IsBlinkSupported;
    property IsBlockSupported: Boolean read get_IsBlockSupported;
    property IsHalfBlockSupported: Boolean read get_IsHalfBlockSupported;
    property IsOtherSupported: Boolean read get_IsOtherSupported;
    property IsReverseSupported: Boolean read get_IsReverseSupported;
    property IsUnderlineSupported: Boolean read get_IsUnderlineSupported;
  end;

  // Windows.Devices.PointOfService.ILineDisplayMarquee
  ILineDisplayMarquee = interface(IInspectable)
  ['{A3D33E3E-F46A-4B7A-BC21-53EB3B57F8B4}']
    function get_Format: LineDisplayMarqueeFormat; safecall;
    procedure put_Format(value: LineDisplayMarqueeFormat); safecall;
    function get_RepeatWaitInterval: TimeSpan; safecall;
    procedure put_RepeatWaitInterval(value: TimeSpan); safecall;
    function get_ScrollWaitInterval: TimeSpan; safecall;
    procedure put_ScrollWaitInterval(value: TimeSpan); safecall;
    function TryStartScrollingAsync(direction: LineDisplayScrollDirection): IAsyncOperation_1__Boolean; safecall;
    function TryStopScrollingAsync: IAsyncOperation_1__Boolean; safecall;
    property Format: LineDisplayMarqueeFormat read get_Format write put_Format;
    property RepeatWaitInterval: TimeSpan read get_RepeatWaitInterval write put_RepeatWaitInterval;
    property ScrollWaitInterval: TimeSpan read get_ScrollWaitInterval write put_ScrollWaitInterval;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplay>
  AsyncOperationCompletedHandler_1__ILineDisplay_Delegate_Base = interface(IUnknown)
  ['{B5C4D476-4F46-53C4-8A45-89DBE6D6F286}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ILineDisplay; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.ILineDisplay>
  AsyncOperationCompletedHandler_1__ILineDisplay = interface(AsyncOperationCompletedHandler_1__ILineDisplay_Delegate_Base)
  ['{248C7CFC-53A4-5693-ADF6-1BF1EF697D41}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplay>
  IAsyncOperation_1__ILineDisplay_Base = interface(IInspectable)
  ['{40FFDAE9-E7C1-5C44-91B4-BD84EBF8539B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ILineDisplay); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ILineDisplay; safecall;
    function GetResults: ILineDisplay; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ILineDisplay read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.ILineDisplay>
  IAsyncOperation_1__ILineDisplay = interface(IAsyncOperation_1__ILineDisplay_Base)
  ['{E873119A-5068-5970-BFFD-410AB56DF875}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplayStatics
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_LineDisplay)]
  ILineDisplayStatics = interface(IInspectable)
  ['{022DC0B6-11B0-4690-9547-0B39C5AF2114}']
    function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__ILineDisplay; safecall;
    function GetDefaultAsync: IAsyncOperation_1__ILineDisplay; safecall;
    function GetDeviceSelector: HSTRING; overload; safecall;
    function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplayStatisticsCategorySelector
  ILineDisplayStatisticsCategorySelector = interface(IInspectable)
  ['{B521C46B-9274-4D24-94F3-B6017B832444}']
    function get_AllStatistics: HSTRING; safecall;
    function get_UnifiedPosStatistics: HSTRING; safecall;
    function get_ManufacturerStatistics: HSTRING; safecall;
    property AllStatistics: HSTRING read get_AllStatistics;
    property ManufacturerStatistics: HSTRING read get_ManufacturerStatistics;
    property UnifiedPosStatistics: HSTRING read get_UnifiedPosStatistics;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.ILineDisplayStatics2
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_LineDisplay)]
  ILineDisplayStatics2 = interface(IInspectable)
  ['{600C3F1C-77AB-4968-A7DE-C02FF169F2CC}']
    function get_StatisticsCategorySelector: ILineDisplayStatisticsCategorySelector; safecall;
    property StatisticsCategorySelector: ILineDisplayStatisticsCategorySelector read get_StatisticsCategorySelector;
  end;

  // Windows.Devices.PointOfService.ILineDisplayWindow2
  ILineDisplayWindow2 = interface(IInspectable)
  ['{A95CE2E6-BDD8-4365-8E11-DE94DE8DFF02}']
    function get_Cursor: ILineDisplayCursor; safecall;
    function get_Marquee: ILineDisplayMarquee; safecall;
    function ReadCharacterAtCursorAsync: IAsyncOperation_1__Cardinal; safecall;
    function TryDisplayStoredBitmapAtCursorAsync(bitmap: ILineDisplayStoredBitmap): IAsyncOperation_1__Boolean; safecall;
    function TryDisplayStorageFileBitmapAtCursorAsync(bitmap: IStorageFile): IAsyncOperation_1__Boolean; overload; safecall;
    function TryDisplayStorageFileBitmapAtCursorAsync(bitmap: IStorageFile; horizontalAlignment: LineDisplayHorizontalAlignment; verticalAlignment: LineDisplayVerticalAlignment): IAsyncOperation_1__Boolean; overload; safecall;
    function TryDisplayStorageFileBitmapAtCursorAsync(bitmap: IStorageFile; horizontalAlignment: LineDisplayHorizontalAlignment; verticalAlignment: LineDisplayVerticalAlignment; widthInPixels: Integer): IAsyncOperation_1__Boolean; overload; safecall;
    function TryDisplayStorageFileBitmapAtPointAsync(bitmap: IStorageFile; offsetInPixels: TPointF): IAsyncOperation_1__Boolean; overload; safecall;
    function TryDisplayStorageFileBitmapAtPointAsync(bitmap: IStorageFile; offsetInPixels: TPointF; widthInPixels: Integer): IAsyncOperation_1__Boolean; overload; safecall;
    property Cursor: ILineDisplayCursor read get_Cursor;
    property Marquee: ILineDisplayMarquee read get_Marquee;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IReceiptPrinterCapabilities
  IReceiptPrinterCapabilities = interface(IInspectable)
  ['{B8F0B58F-51A8-43FC-9BD5-8DE272A6415B}']
    function get_CanCutPaper: Boolean; safecall;
    function get_IsStampSupported: Boolean; safecall;
    function get_MarkFeedCapabilities: PosPrinterMarkFeedCapabilities; safecall;
    property CanCutPaper: Boolean read get_CanCutPaper;
    property IsStampSupported: Boolean read get_IsStampSupported;
    property MarkFeedCapabilities: PosPrinterMarkFeedCapabilities read get_MarkFeedCapabilities;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.ISlipPrinterCapabilities
  ISlipPrinterCapabilities = interface(IInspectable)
  ['{99B16399-488C-4157-8AC2-9F57F708D3DB}']
    function get_IsFullLengthSupported: Boolean; safecall;
    function get_IsBothSidesPrintingSupported: Boolean; safecall;
    property IsBothSidesPrintingSupported: Boolean read get_IsBothSidesPrintingSupported;
    property IsFullLengthSupported: Boolean read get_IsFullLengthSupported;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterCapabilities
  IPosPrinterCapabilities = interface(IInspectable)
  ['{CDE95721-4380-4985-ADC5-39DB30CD93BC}']
    function get_PowerReportingType: UnifiedPosPowerReportingType; safecall;
    function get_IsStatisticsReportingSupported: Boolean; safecall;
    function get_IsStatisticsUpdatingSupported: Boolean; safecall;
    function get_DefaultCharacterSet: Cardinal; safecall;
    function get_HasCoverSensor: Boolean; safecall;
    function get_CanMapCharacterSet: Boolean; safecall;
    function get_IsTransactionSupported: Boolean; safecall;
    function get_Receipt: IReceiptPrinterCapabilities; safecall;
    function get_Slip: ISlipPrinterCapabilities; safecall;
    function get_Journal: IJournalPrinterCapabilities; safecall;
    property CanMapCharacterSet: Boolean read get_CanMapCharacterSet;
    property DefaultCharacterSet: Cardinal read get_DefaultCharacterSet;
    property HasCoverSensor: Boolean read get_HasCoverSensor;
    property IsStatisticsReportingSupported: Boolean read get_IsStatisticsReportingSupported;
    property IsStatisticsUpdatingSupported: Boolean read get_IsStatisticsUpdatingSupported;
    property IsTransactionSupported: Boolean read get_IsTransactionSupported;
    property Journal: IJournalPrinterCapabilities read get_Journal;
    property PowerReportingType: UnifiedPosPowerReportingType read get_PowerReportingType;
    property Receipt: IReceiptPrinterCapabilities read get_Receipt;
    property Slip: ISlipPrinterCapabilities read get_Slip;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterStatus
  IPosPrinterStatus = interface(IInspectable)
  ['{D1F0C730-DA40-4328-BF76-5156FA33B747}']
    function get_StatusKind: PosPrinterStatusKind; safecall;
    function get_ExtendedStatus: Cardinal; safecall;
    property ExtendedStatus: Cardinal read get_ExtendedStatus;
    property StatusKind: PosPrinterStatusKind read get_StatusKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedPosPrinter>
  AsyncOperationCompletedHandler_1__IClaimedPosPrinter_Delegate_Base = interface(IUnknown)
  ['{01EB0DC3-3C30-5EEA-9FCE-EFB398E0BE34}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IClaimedPosPrinter; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IClaimedPosPrinter>
  AsyncOperationCompletedHandler_1__IClaimedPosPrinter = interface(AsyncOperationCompletedHandler_1__IClaimedPosPrinter_Delegate_Base)
  ['{DCC034BC-9B93-5E70-B7F3-5E11F87E91C7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedPosPrinter>
  IAsyncOperation_1__IClaimedPosPrinter_Base = interface(IInspectable)
  ['{B4476F95-355A-503D-B844-1756C8CFDA98}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IClaimedPosPrinter); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IClaimedPosPrinter; safecall;
    function GetResults: IClaimedPosPrinter; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IClaimedPosPrinter read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IClaimedPosPrinter>
  IAsyncOperation_1__IClaimedPosPrinter = interface(IAsyncOperation_1__IClaimedPosPrinter_Base)
  ['{2A715D99-81DD-5476-AC64-BD33873E6B59}']
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterStatusUpdatedEventArgs
  IPosPrinterStatusUpdatedEventArgs = interface(IInspectable)
  ['{2EDB87DF-13A6-428D-BA81-B0E7C3E5A3CD}']
    function get_Status: IPosPrinterStatus; safecall;
    property Status: IPosPrinterStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IPosPrinter,Windows.Devices.PointOfService.IPosPrinterStatusUpdatedEventArgs>
  TypedEventHandler_2__IPosPrinter__IPosPrinterStatusUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{20B0C66A-5F41-5A32-B45A-344F12E70A34}']
    procedure Invoke(sender: IPosPrinter; args: IPosPrinterStatusUpdatedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IPosPrinter,Windows.Devices.PointOfService.IPosPrinterStatusUpdatedEventArgs>
  TypedEventHandler_2__IPosPrinter__IPosPrinterStatusUpdatedEventArgs = interface(TypedEventHandler_2__IPosPrinter__IPosPrinterStatusUpdatedEventArgs_Delegate_Base)
  ['{B8F0D464-440B-5A73-A838-BCF474C8C9B2}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinter
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_PosPrinter)]
  IPosPrinter = interface(IInspectable)
  ['{2A03C10E-9A19-4A01-994F-12DFAD6ADCBF}']
    function get_DeviceId: HSTRING; safecall;
    function get_Capabilities: IPosPrinterCapabilities; safecall;
    function get_SupportedCharacterSets: IVectorView_1__Cardinal; safecall;
    function get_SupportedTypeFaces: IVectorView_1__HSTRING; safecall;
    function get_Status: IPosPrinterStatus; safecall;
    function ClaimPrinterAsync: IAsyncOperation_1__IClaimedPosPrinter; safecall;
    function CheckHealthAsync(level: UnifiedPosHealthCheckLevel): IAsyncOperation_1__HSTRING; safecall;
    function GetStatisticsAsync(statisticsCategories: IIterable_1__HSTRING): IAsyncOperation_1__HSTRING; safecall;
    function add_StatusUpdated(handler: TypedEventHandler_2__IPosPrinter__IPosPrinterStatusUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StatusUpdated(token: EventRegistrationToken); safecall;
    property Capabilities: IPosPrinterCapabilities read get_Capabilities;
    property DeviceId: HSTRING read get_DeviceId;
    property Status: IPosPrinterStatus read get_Status;
    property SupportedCharacterSets: IVectorView_1__Cardinal read get_SupportedCharacterSets;
    property SupportedTypeFaces: IVectorView_1__HSTRING read get_SupportedTypeFaces;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.PointOfService.SizeUInt32>
  IIterator_1__SizeUInt32 = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: SizeUInt32; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSizeUInt32): Cardinal; safecall;
    property Current: SizeUInt32 read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.PointOfService.SizeUInt32>
  IIterable_1__SizeUInt32 = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__SizeUInt32; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.PointOfService.SizeUInt32>
  IVectorView_1__SizeUInt32 = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): SizeUInt32; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: SizeUInt32; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSizeUInt32): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterFontProperty
  IPosPrinterFontProperty = interface(IInspectable)
  ['{A7F4E93A-F8AC-5F04-84D2-29B16D8A633C}']
    function get_TypeFace: HSTRING; safecall;
    function get_IsScalableToAnySize: Boolean; safecall;
    function get_CharacterSizes: IVectorView_1__SizeUInt32; safecall;
    property CharacterSizes: IVectorView_1__SizeUInt32 read get_CharacterSizes;
    property IsScalableToAnySize: Boolean read get_IsScalableToAnySize;
    property TypeFace: HSTRING read get_TypeFace;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinter2
  IPosPrinter2 = interface(IInspectable)
  ['{248475E8-8B98-5517-8E48-760E86F68987}']
    function get_SupportedBarcodeSymbologies: IVectorView_1__Cardinal; safecall;
    function GetFontProperty(typeface: HSTRING): IPosPrinterFontProperty; safecall;
    property SupportedBarcodeSymbologies: IVectorView_1__Cardinal read get_SupportedBarcodeSymbologies;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterCharacterSetIdsStatics
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_PosPrinterCharacterSetIds)]
  IPosPrinterCharacterSetIdsStatics = interface(IInspectable)
  ['{5C709EFF-709A-4FE7-B215-06A748A38B39}']
    function get_Utf16LE: Cardinal; safecall;
    function get_Ascii: Cardinal; safecall;
    function get_Ansi: Cardinal; safecall;
    property Ansi: Cardinal read get_Ansi;
    property Ascii: Cardinal read get_Ascii;
    property Utf16LE: Cardinal read get_Utf16LE;
  end;

  // Windows.Devices.PointOfService.IPosPrinterJob
  IPosPrinterJob = interface(IInspectable)
  ['{9A94005C-0615-4591-A58F-30F87EDFE2E4}']
    procedure Print(data: HSTRING); safecall;
    procedure PrintLine(data: HSTRING); overload; safecall;
    procedure PrintLine; overload; safecall;
    function ExecuteAsync: IAsyncOperation_1__Boolean; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IPosPrinter>
  AsyncOperationCompletedHandler_1__IPosPrinter_Delegate_Base = interface(IUnknown)
  ['{5E8DBBC8-8B60-5881-8B6E-F699B4949DBA}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IPosPrinter; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.PointOfService.IPosPrinter>
  AsyncOperationCompletedHandler_1__IPosPrinter = interface(AsyncOperationCompletedHandler_1__IPosPrinter_Delegate_Base)
  ['{4DE49EF9-FF09-5C67-85BE-85D97904662B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IPosPrinter>
  IAsyncOperation_1__IPosPrinter_Base = interface(IInspectable)
  ['{024F77CE-51C3-5AFC-9F30-24B3C0F3B25A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IPosPrinter); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IPosPrinter; safecall;
    function GetResults: IPosPrinter; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IPosPrinter read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.PointOfService.IPosPrinter>
  IAsyncOperation_1__IPosPrinter = interface(IAsyncOperation_1__IPosPrinter_Base)
  ['{57F34381-BA47-5A19-B7FE-8E940C7C0FCB}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterStatics
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_PosPrinter)]
  IPosPrinterStatics = interface(IInspectable)
  ['{8CE0D4EA-132F-4CDF-A64A-2D0D7C96A85B}']
    function GetDefaultAsync: IAsyncOperation_1__IPosPrinter; safecall;
    function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IPosPrinter; safecall;
    function GetDeviceSelector: HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.PointOfService.IPosPrinterStatics2
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_PosPrinter)]
  IPosPrinterStatics2 = interface(IInspectable)
  ['{EECD2C1C-B0D0-42E7-B137-B89B16244D41}']
    function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; safecall;
  end;

  // Windows.Devices.PointOfService.IReceiptPrintJob2
  IReceiptPrintJob2 = interface(IInspectable)
  ['{0CBC12E3-9E29-5179-BCD8-1811D3B9A10E}']
    procedure StampPaper; safecall;
    procedure Print(data: HSTRING; printOptions: IPosPrinterPrintOptions); safecall;
    procedure FeedPaperByLine(lineCount: Integer); safecall;
    procedure FeedPaperByMapModeUnit(distance: Integer); safecall;
  end;

  // Windows.Devices.PointOfService.IReceiptPrinterCapabilities2
  IReceiptPrinterCapabilities2 = interface(IInspectable)
  ['{20030638-8A2C-55AC-9A7B-7576D8869E99}']
    function get_IsReverseVideoSupported: Boolean; safecall;
    function get_IsStrikethroughSupported: Boolean; safecall;
    function get_IsSuperscriptSupported: Boolean; safecall;
    function get_IsSubscriptSupported: Boolean; safecall;
    function get_IsReversePaperFeedByLineSupported: Boolean; safecall;
    function get_IsReversePaperFeedByMapModeUnitSupported: Boolean; safecall;
    property IsReversePaperFeedByLineSupported: Boolean read get_IsReversePaperFeedByLineSupported;
    property IsReversePaperFeedByMapModeUnitSupported: Boolean read get_IsReversePaperFeedByMapModeUnitSupported;
    property IsReverseVideoSupported: Boolean read get_IsReverseVideoSupported;
    property IsStrikethroughSupported: Boolean read get_IsStrikethroughSupported;
    property IsSubscriptSupported: Boolean read get_IsSubscriptSupported;
    property IsSuperscriptSupported: Boolean read get_IsSuperscriptSupported;
  end;

  // Windows.Devices.PointOfService.ISlipPrinterCapabilities2
  ISlipPrinterCapabilities2 = interface(IInspectable)
  ['{6FF89671-2D1A-5000-87C2-B0851BFDF07E}']
    function get_IsReverseVideoSupported: Boolean; safecall;
    function get_IsStrikethroughSupported: Boolean; safecall;
    function get_IsSuperscriptSupported: Boolean; safecall;
    function get_IsSubscriptSupported: Boolean; safecall;
    function get_IsReversePaperFeedByLineSupported: Boolean; safecall;
    function get_IsReversePaperFeedByMapModeUnitSupported: Boolean; safecall;
    property IsReversePaperFeedByLineSupported: Boolean read get_IsReversePaperFeedByLineSupported;
    property IsReversePaperFeedByMapModeUnitSupported: Boolean read get_IsReversePaperFeedByMapModeUnitSupported;
    property IsReverseVideoSupported: Boolean read get_IsReverseVideoSupported;
    property IsStrikethroughSupported: Boolean read get_IsStrikethroughSupported;
    property IsSubscriptSupported: Boolean read get_IsSubscriptSupported;
    property IsSuperscriptSupported: Boolean read get_IsSuperscriptSupported;
  end;

  // Windows.Devices.PointOfService.CashDrawer
  // DualAPI
  // Implements: Windows.Devices.PointOfService.ICashDrawer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Devices.PointOfService.ICashDrawerStatics"
  // Statics: "Windows.Devices.PointOfService.ICashDrawerStatics2"
  TCashDrawer = class(TWinRTGenericImportS2<ICashDrawerStatics, ICashDrawerStatics2>)
  public
    // -> ICashDrawerStatics
    class function GetDefaultAsync: IAsyncOperation_1__ICashDrawer; static; inline;
    class function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__ICashDrawer; static; inline;
    class function GetDeviceSelector: HSTRING; overload; static; inline;

    // -> ICashDrawerStatics2
    class function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; overload; static; inline;
  end;

  // Windows.Devices.PointOfService.ClaimedLineDisplay
  // DualAPI
  // Implements: Windows.Devices.PointOfService.IClaimedLineDisplay
  // Implements: Windows.Devices.PointOfService.IClaimedLineDisplay2
  // Implements: Windows.Devices.PointOfService.IClaimedLineDisplay3
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Devices.PointOfService.IClaimedLineDisplayStatics"
  TClaimedLineDisplay = class(TWinRTGenericImportS<IClaimedLineDisplayStatics>)
  public
    // -> IClaimedLineDisplayStatics
    class function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IClaimedLineDisplay; static; inline;
    class function GetDeviceSelector: HSTRING; overload; static; inline;
    class function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; overload; static; inline;
  end;

  // Windows.Devices.PointOfService.LineDisplay
  // DualAPI
  // Implements: Windows.Devices.PointOfService.ILineDisplay
  // Implements: Windows.Devices.PointOfService.ILineDisplay2
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Devices.PointOfService.ILineDisplayStatics"
  // Statics: "Windows.Devices.PointOfService.ILineDisplayStatics2"
  TLineDisplay = class(TWinRTGenericImportS2<ILineDisplayStatics, ILineDisplayStatics2>)
  public
    // -> ILineDisplayStatics
    class function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__ILineDisplay; static; inline;
    class function GetDefaultAsync: IAsyncOperation_1__ILineDisplay; static; inline;
    class function GetDeviceSelector: HSTRING; overload; static; inline;
    class function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; overload; static; inline;

    // -> ILineDisplayStatics2
    class function get_StatisticsCategorySelector: ILineDisplayStatisticsCategorySelector; static; inline;
    class property StatisticsCategorySelector: ILineDisplayStatisticsCategorySelector read get_StatisticsCategorySelector;
  end;

  // Windows.Devices.PointOfService.PosPrinter
  // DualAPI
  // Implements: Windows.Devices.PointOfService.IPosPrinter
  // Implements: Windows.Devices.PointOfService.IPosPrinter2
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Devices.PointOfService.IPosPrinterStatics"
  // Statics: "Windows.Devices.PointOfService.IPosPrinterStatics2"
  TPosPrinter = class(TWinRTGenericImportS2<IPosPrinterStatics, IPosPrinterStatics2>)
  public
    // -> IPosPrinterStatics
    class function GetDefaultAsync: IAsyncOperation_1__IPosPrinter; static; inline;
    class function FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IPosPrinter; static; inline;
    class function GetDeviceSelector: HSTRING; overload; static; inline;

    // -> IPosPrinterStatics2
    class function GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING; overload; static; inline;
  end;

  // Windows.Devices.PointOfService.PosPrinterCharacterSetIds
  // DualAPI
  // Statics: "Windows.Devices.PointOfService.IPosPrinterCharacterSetIdsStatics"
  TPosPrinterCharacterSetIds = class(TWinRTGenericImportS<IPosPrinterCharacterSetIdsStatics>)
  public
    // -> IPosPrinterCharacterSetIdsStatics
    class function get_Utf16LE: Cardinal; static; inline;
    class function get_Ascii: Cardinal; static; inline;
    class function get_Ansi: Cardinal; static; inline;
    class property Ansi: Cardinal read get_Ansi;
    class property Ascii: Cardinal read get_Ascii;
    class property Utf16LE: Cardinal read get_Utf16LE;
  end;

  // Windows.Devices.PointOfService.PosPrinterPrintOptions
  // DualAPI
  // Implements: Windows.Devices.PointOfService.IPosPrinterPrintOptions
  // Instantiable: "IPosPrinterPrintOptions"
  TPosPrinterPrintOptions = class(TWinRTGenericImportI<IPosPrinterPrintOptions>) end;

implementation

{ TCashDrawer }

class function TCashDrawer.GetDefaultAsync: IAsyncOperation_1__ICashDrawer;
begin
  Result := Statics.GetDefaultAsync;
end;

class function TCashDrawer.FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__ICashDrawer;
begin
  Result := Statics.FromIdAsync(deviceId);
end;

class function TCashDrawer.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;


class function TCashDrawer.GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING;
begin
  Result := Statics2.GetDeviceSelector(connectionTypes);
end;


{ TClaimedLineDisplay }

class function TClaimedLineDisplay.FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IClaimedLineDisplay;
begin
  Result := Statics.FromIdAsync(deviceId);
end;

class function TClaimedLineDisplay.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;

class function TClaimedLineDisplay.GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING;
begin
  Result := Statics.GetDeviceSelector(connectionTypes);
end;


{ TLineDisplay }

class function TLineDisplay.FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__ILineDisplay;
begin
  Result := Statics.FromIdAsync(deviceId);
end;

class function TLineDisplay.GetDefaultAsync: IAsyncOperation_1__ILineDisplay;
begin
  Result := Statics.GetDefaultAsync;
end;

class function TLineDisplay.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;

class function TLineDisplay.GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING;
begin
  Result := Statics.GetDeviceSelector(connectionTypes);
end;


class function TLineDisplay.get_StatisticsCategorySelector: ILineDisplayStatisticsCategorySelector;
begin
  Result := Statics2.get_StatisticsCategorySelector;
end;


{ TPosPrinter }

class function TPosPrinter.GetDefaultAsync: IAsyncOperation_1__IPosPrinter;
begin
  Result := Statics.GetDefaultAsync;
end;

class function TPosPrinter.FromIdAsync(deviceId: HSTRING): IAsyncOperation_1__IPosPrinter;
begin
  Result := Statics.FromIdAsync(deviceId);
end;

class function TPosPrinter.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;


class function TPosPrinter.GetDeviceSelector(connectionTypes: PosConnectionTypes): HSTRING;
begin
  Result := Statics2.GetDeviceSelector(connectionTypes);
end;


{ TPosPrinterCharacterSetIds }

class function TPosPrinterCharacterSetIds.get_Utf16LE: Cardinal;
begin
  Result := Statics.get_Utf16LE;
end;

class function TPosPrinterCharacterSetIds.get_Ascii: Cardinal;
begin
  Result := Statics.get_Ascii;
end;

class function TPosPrinterCharacterSetIds.get_Ansi: Cardinal;
begin
  Result := Statics.get_Ansi;
end;


{ TPosPrinterPrintOptions }

end.
