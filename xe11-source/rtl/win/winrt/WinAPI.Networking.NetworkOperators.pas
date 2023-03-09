{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Networking.NetworkOperators;

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
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult;
  AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration_Delegate_Base;
  AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration;
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult;
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult;
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult;
  DataClasses = Winapi.CommonTypes.DataClasses;
  PDataClasses = Winapi.CommonTypes.PDataClasses;
  IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult_Base;
  IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult;
  PIAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult = Winapi.CommonTypes.PIAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult;
  IAsyncOperation_1__IMobileBroadbandModemConfiguration_Base = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandModemConfiguration_Base;
  IAsyncOperation_1__IMobileBroadbandModemConfiguration = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandModemConfiguration;
  PIAsyncOperation_1__IMobileBroadbandModemConfiguration = Winapi.CommonTypes.PIAsyncOperation_1__IMobileBroadbandModemConfiguration;
  IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult_Base;
  IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult;
  PIAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult = Winapi.CommonTypes.PIAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult;
  IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult_Base;
  IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult;
  PIAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult = Winapi.CommonTypes.PIAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult;
  IAsyncOperation_1__IMobileBroadbandUiccAppsResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandUiccAppsResult_Base;
  IAsyncOperation_1__IMobileBroadbandUiccAppsResult = Winapi.CommonTypes.IAsyncOperation_1__IMobileBroadbandUiccAppsResult;
  PIAsyncOperation_1__IMobileBroadbandUiccAppsResult = Winapi.CommonTypes.PIAsyncOperation_1__IMobileBroadbandUiccAppsResult;
  IMobileBroadbandAccount = Winapi.CommonTypes.IMobileBroadbandAccount;
  PIMobileBroadbandAccount = Winapi.CommonTypes.PIMobileBroadbandAccount;
  IMobileBroadbandAccountEventArgs = Winapi.CommonTypes.IMobileBroadbandAccountEventArgs;
  PIMobileBroadbandAccountEventArgs = Winapi.CommonTypes.PIMobileBroadbandAccountEventArgs;
  IMobileBroadbandAccountUpdatedEventArgs = Winapi.CommonTypes.IMobileBroadbandAccountUpdatedEventArgs;
  PIMobileBroadbandAccountUpdatedEventArgs = Winapi.CommonTypes.PIMobileBroadbandAccountUpdatedEventArgs;
  IMobileBroadbandAccountWatcher = Winapi.CommonTypes.IMobileBroadbandAccountWatcher;
  PIMobileBroadbandAccountWatcher = Winapi.CommonTypes.PIMobileBroadbandAccountWatcher;
  IMobileBroadbandDeviceInformation = Winapi.CommonTypes.IMobileBroadbandDeviceInformation;
  PIMobileBroadbandDeviceInformation = Winapi.CommonTypes.PIMobileBroadbandDeviceInformation;
  IMobileBroadbandDeviceService = Winapi.CommonTypes.IMobileBroadbandDeviceService;
  PIMobileBroadbandDeviceService = Winapi.CommonTypes.PIMobileBroadbandDeviceService;
  IMobileBroadbandDeviceServiceCommandResult = Winapi.CommonTypes.IMobileBroadbandDeviceServiceCommandResult;
  PIMobileBroadbandDeviceServiceCommandResult = Winapi.CommonTypes.PIMobileBroadbandDeviceServiceCommandResult;
  IMobileBroadbandDeviceServiceCommandSession = Winapi.CommonTypes.IMobileBroadbandDeviceServiceCommandSession;
  PIMobileBroadbandDeviceServiceCommandSession = Winapi.CommonTypes.PIMobileBroadbandDeviceServiceCommandSession;
  IMobileBroadbandDeviceServiceDataReceivedEventArgs = Winapi.CommonTypes.IMobileBroadbandDeviceServiceDataReceivedEventArgs;
  PIMobileBroadbandDeviceServiceDataReceivedEventArgs = Winapi.CommonTypes.PIMobileBroadbandDeviceServiceDataReceivedEventArgs;
  IMobileBroadbandDeviceServiceDataSession = Winapi.CommonTypes.IMobileBroadbandDeviceServiceDataSession;
  PIMobileBroadbandDeviceServiceDataSession = Winapi.CommonTypes.PIMobileBroadbandDeviceServiceDataSession;
  IMobileBroadbandDeviceServiceInformation = Winapi.CommonTypes.IMobileBroadbandDeviceServiceInformation;
  PIMobileBroadbandDeviceServiceInformation = Winapi.CommonTypes.PIMobileBroadbandDeviceServiceInformation;
  IMobileBroadbandModem = Winapi.CommonTypes.IMobileBroadbandModem;
  PIMobileBroadbandModem = Winapi.CommonTypes.PIMobileBroadbandModem;
  IMobileBroadbandModemConfiguration = Winapi.CommonTypes.IMobileBroadbandModemConfiguration;
  PIMobileBroadbandModemConfiguration = Winapi.CommonTypes.PIMobileBroadbandModemConfiguration;
  IMobileBroadbandNetwork = Winapi.CommonTypes.IMobileBroadbandNetwork;
  PIMobileBroadbandNetwork = Winapi.CommonTypes.PIMobileBroadbandNetwork;
  IMobileBroadbandUicc = Winapi.CommonTypes.IMobileBroadbandUicc;
  PIMobileBroadbandUicc = Winapi.CommonTypes.PIMobileBroadbandUicc;
  IMobileBroadbandUiccApp = Winapi.CommonTypes.IMobileBroadbandUiccApp;
  PIMobileBroadbandUiccApp = Winapi.CommonTypes.PIMobileBroadbandUiccApp;
  IMobileBroadbandUiccAppReadRecordResult = Winapi.CommonTypes.IMobileBroadbandUiccAppReadRecordResult;
  PIMobileBroadbandUiccAppReadRecordResult = Winapi.CommonTypes.PIMobileBroadbandUiccAppReadRecordResult;
  IMobileBroadbandUiccAppRecordDetailsResult = Winapi.CommonTypes.IMobileBroadbandUiccAppRecordDetailsResult;
  PIMobileBroadbandUiccAppRecordDetailsResult = Winapi.CommonTypes.PIMobileBroadbandUiccAppRecordDetailsResult;
  IMobileBroadbandUiccAppsResult = Winapi.CommonTypes.IMobileBroadbandUiccAppsResult;
  PIMobileBroadbandUiccAppsResult = Winapi.CommonTypes.PIMobileBroadbandUiccAppsResult;
  IVectorView_1__IMobileBroadbandDeviceServiceInformation = Winapi.CommonTypes.IVectorView_1__IMobileBroadbandDeviceServiceInformation;
  PIVectorView_1__IMobileBroadbandDeviceServiceInformation = Winapi.CommonTypes.PIVectorView_1__IMobileBroadbandDeviceServiceInformation;
  IVectorView_1__IMobileBroadbandUiccApp = Winapi.CommonTypes.IVectorView_1__IMobileBroadbandUiccApp;
  PIVectorView_1__IMobileBroadbandUiccApp = Winapi.CommonTypes.PIVectorView_1__IMobileBroadbandUiccApp;
  MobileBroadbandAccountWatcherStatus = Winapi.CommonTypes.MobileBroadbandAccountWatcherStatus;
  PMobileBroadbandAccountWatcherStatus = Winapi.CommonTypes.PMobileBroadbandAccountWatcherStatus;
  MobileBroadbandDeviceType = Winapi.CommonTypes.MobileBroadbandDeviceType;
  PMobileBroadbandDeviceType = Winapi.CommonTypes.PMobileBroadbandDeviceType;
  MobileBroadbandRadioState = Winapi.CommonTypes.MobileBroadbandRadioState;
  PMobileBroadbandRadioState = Winapi.CommonTypes.PMobileBroadbandRadioState;
  MobileBroadbandUiccAppOperationStatus = Winapi.CommonTypes.MobileBroadbandUiccAppOperationStatus;
  PMobileBroadbandUiccAppOperationStatus = Winapi.CommonTypes.PMobileBroadbandUiccAppOperationStatus;
  NetworkDeviceStatus = Winapi.CommonTypes.NetworkDeviceStatus;
  PNetworkDeviceStatus = Winapi.CommonTypes.PNetworkDeviceStatus;
  NetworkRegistrationState = Winapi.CommonTypes.NetworkRegistrationState;
  PNetworkRegistrationState = Winapi.CommonTypes.PNetworkRegistrationState;
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs_Delegate_Base;
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs;
  PTypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs;
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs_Delegate_Base;
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs;
  PTypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs;
  TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs_Delegate_Base;
  TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs;
  PTypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs;
  UiccAccessCondition = Winapi.CommonTypes.UiccAccessCondition;
  PUiccAccessCondition = Winapi.CommonTypes.PUiccAccessCondition;
  UiccAppKind = Winapi.CommonTypes.UiccAppKind;
  PUiccAppKind = Winapi.CommonTypes.PUiccAppKind;
  UiccAppRecordKind = Winapi.CommonTypes.UiccAppRecordKind;
  PUiccAppRecordKind = Winapi.CommonTypes.PUiccAppRecordKind;

  // Forward declarations for interfaces

  // Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult
  IHotspotCredentialsAuthenticationResult = interface;
  PIHotspotCredentialsAuthenticationResult = ^IHotspotCredentialsAuthenticationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult>
  AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult = interface;
  PAsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult = ^AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult>
  IAsyncOperation_1__IHotspotCredentialsAuthenticationResult = interface;
  PIAsyncOperation_1__IHotspotCredentialsAuthenticationResult = ^IAsyncOperation_1__IHotspotCredentialsAuthenticationResult;

  // Windows.Networking.NetworkOperators.IKnownCSimFilePathsStatics
  IKnownCSimFilePathsStatics = interface;
  PIKnownCSimFilePathsStatics = ^IKnownCSimFilePathsStatics;

  // Windows.Networking.NetworkOperators.IKnownRuimFilePathsStatics
  IKnownRuimFilePathsStatics = interface;
  PIKnownRuimFilePathsStatics = ^IKnownRuimFilePathsStatics;

  // Windows.Networking.NetworkOperators.IKnownSimFilePathsStatics
  IKnownSimFilePathsStatics = interface;
  PIKnownSimFilePathsStatics = ^IKnownSimFilePathsStatics;

  // Windows.Networking.NetworkOperators.IKnownUSimFilePathsStatics
  IKnownUSimFilePathsStatics = interface;
  PIKnownUSimFilePathsStatics = ^IKnownUSimFilePathsStatics;

  // Windows.Networking.NetworkOperators.IMobileBroadbandAccount2
  IMobileBroadbandAccount2 = interface;
  PIMobileBroadbandAccount2 = ^IMobileBroadbandAccount2;

  // Windows.Networking.NetworkOperators.IMobileBroadbandAccount3
  IMobileBroadbandAccount3 = interface;
  PIMobileBroadbandAccount3 = ^IMobileBroadbandAccount3;

  // Windows.Networking.NetworkOperators.IMobileBroadbandAccountStatics
  IMobileBroadbandAccountStatics = interface;
  PIMobileBroadbandAccountStatics = ^IMobileBroadbandAccountStatics;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IIterator_1__MobileBroadbandPinType = interface;
  PIIterator_1__MobileBroadbandPinType = ^IIterator_1__MobileBroadbandPinType;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IIterable_1__MobileBroadbandPinType = interface;
  PIIterable_1__MobileBroadbandPinType = ^IIterable_1__MobileBroadbandPinType;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IVectorView_1__MobileBroadbandPinType = interface;
  PIVectorView_1__MobileBroadbandPinType = ^IVectorView_1__MobileBroadbandPinType;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult
  IMobileBroadbandPinOperationResult = interface;
  PIMobileBroadbandPinOperationResult = ^IMobileBroadbandPinOperationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult = interface;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult = ^AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult>
  IAsyncOperation_1__IMobileBroadbandPinOperationResult = interface;
  PIAsyncOperation_1__IMobileBroadbandPinOperationResult = ^IAsyncOperation_1__IMobileBroadbandPinOperationResult;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPin
  IMobileBroadbandPin = interface;
  PIMobileBroadbandPin = ^IMobileBroadbandPin;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinManager
  IMobileBroadbandPinManager = interface;
  PIMobileBroadbandPinManager = ^IMobileBroadbandPinManager;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceInformation2
  IMobileBroadbandDeviceInformation2 = interface;
  PIMobileBroadbandDeviceInformation2 = ^IMobileBroadbandDeviceInformation2;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceInformation3
  IMobileBroadbandDeviceInformation3 = interface;
  PIMobileBroadbandDeviceInformation3 = ^IMobileBroadbandDeviceInformation3;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceTriggerDetails
  IMobileBroadbandDeviceServiceTriggerDetails = interface;
  PIMobileBroadbandDeviceServiceTriggerDetails = ^IMobileBroadbandDeviceServiceTriggerDetails;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  IIterator_1__IMobileBroadbandDeviceServiceInformation = interface;
  PIIterator_1__IMobileBroadbandDeviceServiceInformation = ^IIterator_1__IMobileBroadbandDeviceServiceInformation;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  IIterable_1__IMobileBroadbandDeviceServiceInformation = interface;
  PIIterable_1__IMobileBroadbandDeviceServiceInformation = ^IIterable_1__IMobileBroadbandDeviceServiceInformation;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  IIterator_1__IMobileBroadbandUiccApp = interface;
  PIIterator_1__IMobileBroadbandUiccApp = ^IIterator_1__IMobileBroadbandUiccApp;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  IIterable_1__IMobileBroadbandUiccApp = interface;
  PIIterable_1__IMobileBroadbandUiccApp = ^IIterable_1__IMobileBroadbandUiccApp;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.MobileBroadbandModemStatus>
  AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus = interface;
  PAsyncOperationCompletedHandler_1__MobileBroadbandModemStatus = ^AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.MobileBroadbandModemStatus>
  IAsyncOperation_1__MobileBroadbandModemStatus = interface;
  PIAsyncOperation_1__MobileBroadbandModemStatus = ^IAsyncOperation_1__MobileBroadbandModemStatus;

  // Windows.Networking.NetworkOperators.IMobileBroadbandModem2
  IMobileBroadbandModem2 = interface;
  PIMobileBroadbandModem2 = ^IMobileBroadbandModem2;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPco
  IMobileBroadbandPco = interface;
  PIMobileBroadbandPco = ^IMobileBroadbandPco;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandPco>
  AsyncOperationCompletedHandler_1__IMobileBroadbandPco = interface;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandPco = ^AsyncOperationCompletedHandler_1__IMobileBroadbandPco;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandPco>
  IAsyncOperation_1__IMobileBroadbandPco = interface;
  PIAsyncOperation_1__IMobileBroadbandPco = ^IAsyncOperation_1__IMobileBroadbandPco;

  // Windows.Networking.NetworkOperators.IMobileBroadbandModem3
  IMobileBroadbandModem3 = interface;
  PIMobileBroadbandModem3 = ^IMobileBroadbandModem3;

  // Windows.Networking.NetworkOperators.IMobileBroadbandModemStatics
  IMobileBroadbandModemStatics = interface;
  PIMobileBroadbandModemStatics = ^IMobileBroadbandModemStatics;

  // Windows.Networking.NetworkOperators.IMobileBroadbandNetwork2
  IMobileBroadbandNetwork2 = interface;
  PIMobileBroadbandNetwork2 = ^IMobileBroadbandNetwork2;

  // Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange
  IMobileBroadbandNetworkRegistrationStateChange = interface;
  PIMobileBroadbandNetworkRegistrationStateChange = ^IMobileBroadbandNetworkRegistrationStateChange;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IIterator_1__IMobileBroadbandNetworkRegistrationStateChange = interface;
  PIIterator_1__IMobileBroadbandNetworkRegistrationStateChange = ^IIterator_1__IMobileBroadbandNetworkRegistrationStateChange;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IIterable_1__IMobileBroadbandNetworkRegistrationStateChange = interface;
  PIIterable_1__IMobileBroadbandNetworkRegistrationStateChange = ^IIterable_1__IMobileBroadbandNetworkRegistrationStateChange;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IVectorView_1__IMobileBroadbandNetworkRegistrationStateChange = interface;
  PIVectorView_1__IMobileBroadbandNetworkRegistrationStateChange = ^IVectorView_1__IMobileBroadbandNetworkRegistrationStateChange;

  // Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChangeTriggerDetails
  IMobileBroadbandNetworkRegistrationStateChangeTriggerDetails = interface;
  PIMobileBroadbandNetworkRegistrationStateChangeTriggerDetails = ^IMobileBroadbandNetworkRegistrationStateChangeTriggerDetails;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange
  IMobileBroadbandPinLockStateChange = interface;
  PIMobileBroadbandPinLockStateChange = ^IMobileBroadbandPinLockStateChange;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IIterator_1__IMobileBroadbandPinLockStateChange = interface;
  PIIterator_1__IMobileBroadbandPinLockStateChange = ^IIterator_1__IMobileBroadbandPinLockStateChange;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IIterable_1__IMobileBroadbandPinLockStateChange = interface;
  PIIterable_1__IMobileBroadbandPinLockStateChange = ^IIterable_1__IMobileBroadbandPinLockStateChange;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IVectorView_1__IMobileBroadbandPinLockStateChange = interface;
  PIVectorView_1__IMobileBroadbandPinLockStateChange = ^IVectorView_1__IMobileBroadbandPinLockStateChange;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChangeTriggerDetails
  IMobileBroadbandPinLockStateChangeTriggerDetails = interface;
  PIMobileBroadbandPinLockStateChangeTriggerDetails = ^IMobileBroadbandPinLockStateChangeTriggerDetails;

  // Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange
  IMobileBroadbandRadioStateChange = interface;
  PIMobileBroadbandRadioStateChange = ^IMobileBroadbandRadioStateChange;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IIterator_1__IMobileBroadbandRadioStateChange = interface;
  PIIterator_1__IMobileBroadbandRadioStateChange = ^IIterator_1__IMobileBroadbandRadioStateChange;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IIterable_1__IMobileBroadbandRadioStateChange = interface;
  PIIterable_1__IMobileBroadbandRadioStateChange = ^IIterable_1__IMobileBroadbandRadioStateChange;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IVectorView_1__IMobileBroadbandRadioStateChange = interface;
  PIVectorView_1__IMobileBroadbandRadioStateChange = ^IVectorView_1__IMobileBroadbandRadioStateChange;

  // Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChangeTriggerDetails
  IMobileBroadbandRadioStateChangeTriggerDetails = interface;
  PIMobileBroadbandRadioStateChangeTriggerDetails = ^IMobileBroadbandRadioStateChangeTriggerDetails;

  // Windows.Networking.NetworkOperators.INetworkOperatorNotificationEventDetails
  INetworkOperatorNotificationEventDetails = interface;
  PINetworkOperatorNotificationEventDetails = ^INetworkOperatorNotificationEventDetails;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringAccessPointConfiguration
  INetworkOperatorTetheringAccessPointConfiguration = interface;
  PINetworkOperatorTetheringAccessPointConfiguration = ^INetworkOperatorTetheringAccessPointConfiguration;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringAccessPointConfiguration2
  INetworkOperatorTetheringAccessPointConfiguration2 = interface;
  PINetworkOperatorTetheringAccessPointConfiguration2 = ^INetworkOperatorTetheringAccessPointConfiguration2;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient
  INetworkOperatorTetheringClient = interface;
  PINetworkOperatorTetheringClient = ^INetworkOperatorTetheringClient;

  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IIterator_1__INetworkOperatorTetheringClient = interface;
  PIIterator_1__INetworkOperatorTetheringClient = ^IIterator_1__INetworkOperatorTetheringClient;

  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IIterable_1__INetworkOperatorTetheringClient = interface;
  PIIterable_1__INetworkOperatorTetheringClient = ^IIterable_1__INetworkOperatorTetheringClient;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IVectorView_1__INetworkOperatorTetheringClient = interface;
  PIVectorView_1__INetworkOperatorTetheringClient = ^IVectorView_1__INetworkOperatorTetheringClient;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringClientManager
  INetworkOperatorTetheringClientManager = interface;
  PINetworkOperatorTetheringClientManager = ^INetworkOperatorTetheringClientManager;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringEntitlementCheck
  INetworkOperatorTetheringEntitlementCheck = interface;
  PINetworkOperatorTetheringEntitlementCheck = ^INetworkOperatorTetheringEntitlementCheck;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult
  INetworkOperatorTetheringOperationResult = interface;
  PINetworkOperatorTetheringOperationResult = ^INetworkOperatorTetheringOperationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult>
  AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult = interface;
  PAsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult = ^AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult>
  IAsyncOperation_1__INetworkOperatorTetheringOperationResult = interface;
  PIAsyncOperation_1__INetworkOperatorTetheringOperationResult = ^IAsyncOperation_1__INetworkOperatorTetheringOperationResult;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManager
  INetworkOperatorTetheringManager = interface;
  PINetworkOperatorTetheringManager = ^INetworkOperatorTetheringManager;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics
  INetworkOperatorTetheringManagerStatics = interface;
  PINetworkOperatorTetheringManagerStatics = ^INetworkOperatorTetheringManagerStatics;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics2
  INetworkOperatorTetheringManagerStatics2 = interface;
  PINetworkOperatorTetheringManagerStatics2 = ^INetworkOperatorTetheringManagerStatics2;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics3
  INetworkOperatorTetheringManagerStatics3 = interface;
  PINetworkOperatorTetheringManagerStatics3 = ^INetworkOperatorTetheringManagerStatics3;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics4
  INetworkOperatorTetheringManagerStatics4 = interface;
  PINetworkOperatorTetheringManagerStatics4 = ^INetworkOperatorTetheringManagerStatics4;

  // Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults
  IProvisionFromXmlDocumentResults = interface;
  PIProvisionFromXmlDocumentResults = ^IProvisionFromXmlDocumentResults;

  // Windows.Networking.NetworkOperators.IProvisionedProfile
  IProvisionedProfile = interface;
  PIProvisionedProfile = ^IProvisionedProfile;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults>
  AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults = interface;
  PAsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults = ^AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults>
  IAsyncOperation_1__IProvisionFromXmlDocumentResults = interface;
  PIAsyncOperation_1__IProvisionFromXmlDocumentResults = ^IAsyncOperation_1__IProvisionFromXmlDocumentResults;

  // Windows.Networking.NetworkOperators.IProvisioningAgent
  IProvisioningAgent = interface;
  PIProvisioningAgent = ^IProvisioningAgent;

  // Windows.Networking.NetworkOperators.IProvisioningAgentStaticMethods
  IProvisioningAgentStaticMethods = interface;
  PIProvisioningAgentStaticMethods = ^IProvisioningAgentStaticMethods;

  // Windows.Networking.NetworkOperators.IUssdMessage
  IUssdMessage = interface;
  PIUssdMessage = ^IUssdMessage;

  // Windows.Networking.NetworkOperators.IUssdMessageFactory
  IUssdMessageFactory = interface;
  PIUssdMessageFactory = ^IUssdMessageFactory;

  // Windows.Networking.NetworkOperators.IUssdReply
  IUssdReply = interface;
  PIUssdReply = ^IUssdReply;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IUssdReply>
  AsyncOperationCompletedHandler_1__IUssdReply = interface;
  PAsyncOperationCompletedHandler_1__IUssdReply = ^AsyncOperationCompletedHandler_1__IUssdReply;

  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IUssdReply>
  IAsyncOperation_1__IUssdReply = interface;
  PIAsyncOperation_1__IUssdReply = ^IAsyncOperation_1__IUssdReply;

  // Windows.Networking.NetworkOperators.IUssdSession
  IUssdSession = interface;
  PIUssdSession = ^IUssdSession;

  // Windows.Networking.NetworkOperators.IUssdSessionStatics
  IUssdSessionStatics = interface;
  PIUssdSessionStatics = ^IUssdSessionStatics;

  // Windows.Networking.NetworkOperators Enums

  // Windows.Networking.NetworkOperators.ESimAuthenticationPreference
  ESimAuthenticationPreference = (
    OnEntry = 0,
    OnAction = 1,
    Never = 2
  );
  PESimAuthenticationPreference = ^ESimAuthenticationPreference;

  // Windows.Networking.NetworkOperators.ESimDiscoverResultKind
  ESimDiscoverResultKind = (
    None = 0,
    Events = 1,
    ProfileMetadata = 2
  );
  PESimDiscoverResultKind = ^ESimDiscoverResultKind;

  // Windows.Networking.NetworkOperators.ESimOperationStatus
  ESimOperationStatus = (
    Success = 0,
    NotAuthorized = 1,
    NotFound = 2,
    PolicyViolation = 3,
    InsufficientSpaceOnCard = 4,
    ServerFailure = 5,
    ServerNotReachable = 6,
    TimeoutWaitingForUserConsent = 7,
    IncorrectConfirmationCode = 8,
    ConfirmationCodeMaxRetriesExceeded = 9,
    CardRemoved = 10,
    CardBusy = 11,
    Other = 12,
    CardGeneralFailure = 13,
    ConfirmationCodeMissing = 14,
    InvalidMatchingId = 15,
    NoEligibleProfileForThisDevice = 16,
    OperationAborted = 17,
    EidMismatch = 18,
    ProfileNotAvailableForNewBinding = 19,
    ProfileNotReleasedByOperator = 20,
    OperationProhibitedByProfileClass = 21,
    ProfileNotPresent = 22,
    NoCorrespondingRequest = 23
  );
  PESimOperationStatus = ^ESimOperationStatus;

  // Windows.Networking.NetworkOperators.ESimProfileClass
  ESimProfileClass = (
    Operational = 0,
    Test = 1,
    Provisioning = 2
  );
  PESimProfileClass = ^ESimProfileClass;

  // Windows.Networking.NetworkOperators.ESimProfileMetadataState
  ESimProfileMetadataState = (
    Unknown = 0,
    WaitingForInstall = 1,
    Downloading = 2,
    Installing = 3,
    Expired = 4,
    RejectingDownload = 5,
    NoLongerAvailable = 6,
    DeniedByPolicy = 7
  );
  PESimProfileMetadataState = ^ESimProfileMetadataState;

  // Windows.Networking.NetworkOperators.ESimProfileState
  ESimProfileState = (
    Unknown = 0,
    Disabled = 1,
    Enabled = 2,
    Deleted = 3
  );
  PESimProfileState = ^ESimProfileState;

  // Windows.Networking.NetworkOperators.ESimState
  ESimState = (
    Unknown = 0,
    Idle = 1,
    Removed = 2,
    Busy = 3
  );
  PESimState = ^ESimState;

  // Windows.Networking.NetworkOperators.ESimWatcherStatus
  ESimWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4
  );
  PESimWatcherStatus = ^ESimWatcherStatus;

  // Windows.Networking.NetworkOperators.HotspotAuthenticationResponseCode
  HotspotAuthenticationResponseCode = (
    NoError = 0,
    LoginSucceeded = 50,
    LoginFailed = 100,
    RadiusServerError = 102,
    NetworkAdministratorError = 105,
    LoginAborted = 151,
    AccessGatewayInternalError = 255
  );
  PHotspotAuthenticationResponseCode = ^HotspotAuthenticationResponseCode;

  // Windows.Networking.NetworkOperators.MobileBroadbandModemStatus
  MobileBroadbandModemStatus = (
    Success = 0,
    OtherFailure = 1,
    Busy = 2,
    NoDeviceSupport = 3
  );
  PMobileBroadbandModemStatus = ^MobileBroadbandModemStatus;

  // Windows.Networking.NetworkOperators.MobileBroadbandPinFormat
  MobileBroadbandPinFormat = (
    Unknown = 0,
    Numeric = 1,
    Alphanumeric = 2
  );
  PMobileBroadbandPinFormat = ^MobileBroadbandPinFormat;

  // Windows.Networking.NetworkOperators.MobileBroadbandPinLockState
  MobileBroadbandPinLockState = (
    Unknown = 0,
    Unlocked = 1,
    PinRequired = 2,
    PinUnblockKeyRequired = 3
  );
  PMobileBroadbandPinLockState = ^MobileBroadbandPinLockState;

  // Windows.Networking.NetworkOperators.MobileBroadbandPinType
  MobileBroadbandPinType = (
    None = 0,
    Custom = 1,
    Pin1 = 2,
    Pin2 = 3,
    SimPin = 4,
    FirstSimPin = 5,
    NetworkPin = 6,
    NetworkSubsetPin = 7,
    ServiceProviderPin = 8,
    CorporatePin = 9,
    SubsidyLock = 10
  );
  PMobileBroadbandPinType = ^MobileBroadbandPinType;

  // Windows.Networking.NetworkOperators.NetworkOperatorDataUsageNotificationKind
  NetworkOperatorDataUsageNotificationKind = (
    DataUsageProgress = 0
  );
  PNetworkOperatorDataUsageNotificationKind = ^NetworkOperatorDataUsageNotificationKind;

  // Windows.Networking.NetworkOperators.NetworkOperatorEventMessageType
  NetworkOperatorEventMessageType = (
    Gsm = 0,
    Cdma = 1,
    Ussd = 2,
    DataPlanThresholdReached = 3,
    DataPlanReset = 4,
    DataPlanDeleted = 5,
    ProfileConnected = 6,
    ProfileDisconnected = 7,
    RegisteredRoaming = 8,
    RegisteredHome = 9,
    TetheringEntitlementCheck = 10,
    TetheringOperationalStateChanged = 11,
    TetheringNumberOfClientsChanged = 12
  );
  PNetworkOperatorEventMessageType = ^NetworkOperatorEventMessageType;

  // Windows.Networking.NetworkOperators.ProfileMediaType
  ProfileMediaType = (
    Wlan = 0,
    Wwan = 1
  );
  PProfileMediaType = ^ProfileMediaType;

  // Windows.Networking.NetworkOperators.TetheringCapability
  TetheringCapability = (
    Enabled = 0,
    DisabledByGroupPolicy = 1,
    DisabledByHardwareLimitation = 2,
    DisabledByOperator = 3,
    DisabledBySku = 4,
    DisabledByRequiredAppNotInstalled = 5,
    DisabledDueToUnknownCause = 6,
    DisabledBySystemCapability = 7
  );
  PTetheringCapability = ^TetheringCapability;

  // Windows.Networking.NetworkOperators.TetheringOperationStatus
  TetheringOperationStatus = (
    Success = 0,
    Unknown = 1,
    MobileBroadbandDeviceOff = 2,
    WiFiDeviceOff = 3,
    EntitlementCheckTimeout = 4,
    EntitlementCheckFailure = 5,
    OperationInProgress = 6,
    BluetoothDeviceOff = 7,
    NetworkLimitedConnectivity = 8
  );
  PTetheringOperationStatus = ^TetheringOperationStatus;

  // Windows.Networking.NetworkOperators.TetheringOperationalState
  TetheringOperationalState = (
    Unknown = 0,
    &On = 1,
    Off = 2,
    InTransition = 3
  );
  PTetheringOperationalState = ^TetheringOperationalState;

  // Windows.Networking.NetworkOperators.TetheringWiFiBand
  TetheringWiFiBand = (
    Auto = 0,
    TwoPointFourGigahertz = 1,
    FiveGigahertz = 2
  );
  PTetheringWiFiBand = ^TetheringWiFiBand;

  // Windows.Networking.NetworkOperators.UssdResultCode
  UssdResultCode = (
    NoActionRequired = 0,
    ActionRequired = 1,
    Terminated = 2,
    OtherLocalClient = 3,
    OperationNotSupported = 4,
    NetworkTimeout = 5
  );
  PUssdResultCode = ^UssdResultCode;

  // Windows.Networking.NetworkOperators Records
  // Windows.Networking.NetworkOperators.ESimProfileInstallProgress
  ESimProfileInstallProgress = record
    TotalSizeInBytes: Integer;
    InstalledSizeInBytes: Integer;
  end;
  PESimProfileInstallProgress = ^ESimProfileInstallProgress;

  // Windows.Networking.NetworkOperators.LegacyNetworkOperatorsContract
  LegacyNetworkOperatorsContract = record
  end;
  PLegacyNetworkOperatorsContract = ^LegacyNetworkOperatorsContract;

  // Windows.Networking.NetworkOperators.ProfileUsage
  ProfileUsage = record
    UsageInMegabytes: Cardinal;
    LastSyncTime: DateTime;
  end;
  PProfileUsage = ^ProfileUsage;

  // Windows.Networking.NetworkOperators Interfaces

  // Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult
  IHotspotCredentialsAuthenticationResult = interface(IInspectable)
  ['{E756C791-1005-4DE5-83C7-DE61D88831D0}']
    function get_HasNetworkErrorOccurred: Boolean; safecall;
    function get_ResponseCode: HotspotAuthenticationResponseCode; safecall;
    function get_LogoffUrl: IUriRuntimeClass; safecall;
    function get_AuthenticationReplyXml: Xml_Dom_IXmlDocument; safecall;
    property AuthenticationReplyXml: Xml_Dom_IXmlDocument read get_AuthenticationReplyXml;
    property HasNetworkErrorOccurred: Boolean read get_HasNetworkErrorOccurred;
    property LogoffUrl: IUriRuntimeClass read get_LogoffUrl;
    property ResponseCode: HotspotAuthenticationResponseCode read get_ResponseCode;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult>
  AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult_Delegate_Base = interface(IUnknown)
  ['{7F254BEB-471F-5000-94CE-102CC333055F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IHotspotCredentialsAuthenticationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult>
  AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult = interface(AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult_Delegate_Base)
  ['{100A520E-981A-57A0-91FC-3F0B8D5D6ADB}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult>
  IAsyncOperation_1__IHotspotCredentialsAuthenticationResult_Base = interface(IInspectable)
  ['{522781D8-29C8-5D89-8937-1D1C2032F0C8}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult; safecall;
    function GetResults: IHotspotCredentialsAuthenticationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IHotspotCredentialsAuthenticationResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IHotspotCredentialsAuthenticationResult>
  IAsyncOperation_1__IHotspotCredentialsAuthenticationResult = interface(IAsyncOperation_1__IHotspotCredentialsAuthenticationResult_Base)
  ['{19D48783-66B3-54E3-A880-38E143C588FA}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IKnownCSimFilePathsStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_KnownCSimFilePaths)]
  IKnownCSimFilePathsStatics = interface(IInspectable)
  ['{B458AEED-49F1-4C22-B073-96D511BF9C35}']
    function get_EFSpn: IVectorView_1__Cardinal; safecall;
    function get_Gid1: IVectorView_1__Cardinal; safecall;
    function get_Gid2: IVectorView_1__Cardinal; safecall;
    property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    property Gid1: IVectorView_1__Cardinal read get_Gid1;
    property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IKnownRuimFilePathsStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_KnownRuimFilePaths)]
  IKnownRuimFilePathsStatics = interface(IInspectable)
  ['{3883C8B9-FF24-4571-A867-09F960426E14}']
    function get_EFSpn: IVectorView_1__Cardinal; safecall;
    function get_Gid1: IVectorView_1__Cardinal; safecall;
    function get_Gid2: IVectorView_1__Cardinal; safecall;
    property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    property Gid1: IVectorView_1__Cardinal read get_Gid1;
    property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IKnownSimFilePathsStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_KnownSimFilePaths)]
  IKnownSimFilePathsStatics = interface(IInspectable)
  ['{80CD1A63-37A5-43D3-80A3-CCD23E8FECEE}']
    function get_EFOns: IVectorView_1__Cardinal; safecall;
    function get_EFSpn: IVectorView_1__Cardinal; safecall;
    function get_Gid1: IVectorView_1__Cardinal; safecall;
    function get_Gid2: IVectorView_1__Cardinal; safecall;
    property EFOns: IVectorView_1__Cardinal read get_EFOns;
    property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    property Gid1: IVectorView_1__Cardinal read get_Gid1;
    property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IKnownUSimFilePathsStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_KnownUSimFilePaths)]
  IKnownUSimFilePathsStatics = interface(IInspectable)
  ['{7C34E581-1F1B-43F4-9530-8B092D32D71F}']
    function get_EFSpn: IVectorView_1__Cardinal; safecall;
    function get_EFOpl: IVectorView_1__Cardinal; safecall;
    function get_EFPnn: IVectorView_1__Cardinal; safecall;
    function get_Gid1: IVectorView_1__Cardinal; safecall;
    function get_Gid2: IVectorView_1__Cardinal; safecall;
    property EFOpl: IVectorView_1__Cardinal read get_EFOpl;
    property EFPnn: IVectorView_1__Cardinal read get_EFPnn;
    property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    property Gid1: IVectorView_1__Cardinal read get_Gid1;
    property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandAccount2
  IMobileBroadbandAccount2 = interface(IInspectable)
  ['{38F52F1C-1136-4257-959F-B658A352B6D4}']
    function GetConnectionProfiles: IVectorView_1__IConnectionProfile; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandAccount3
  IMobileBroadbandAccount3 = interface(IInspectable)
  ['{092A1E21-9379-4B9B-AD31-D5FEE2F748C6}']
    function get_AccountExperienceUrl: IUriRuntimeClass; safecall;
    property AccountExperienceUrl: IUriRuntimeClass read get_AccountExperienceUrl;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandAccountStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_MobileBroadbandAccount)]
  IMobileBroadbandAccountStatics = interface(IInspectable)
  ['{AA7F4D24-AFC1-4FC8-AE9A-A9175310FAAD}']
    function get_AvailableNetworkAccountIds: IVectorView_1__HSTRING; safecall;
    function CreateFromNetworkAccountId(networkAccountId: HSTRING): IMobileBroadbandAccount; safecall;
    property AvailableNetworkAccountIds: IVectorView_1__HSTRING read get_AvailableNetworkAccountIds;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IIterator_1__MobileBroadbandPinType_Base = interface(IInspectable)
  ['{23EFCF0C-1F8E-5BD9-8B57-F0850121201C}']
    function get_Current: MobileBroadbandPinType; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PMobileBroadbandPinType): Cardinal; safecall;
    property Current: MobileBroadbandPinType read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IIterator_1__MobileBroadbandPinType = interface(IIterator_1__MobileBroadbandPinType_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IIterable_1__MobileBroadbandPinType_Base = interface(IInspectable)
  ['{9D55726D-813E-50FB-9498-87AA872DD6CA}']
    function First: IIterator_1__MobileBroadbandPinType; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IIterable_1__MobileBroadbandPinType = interface(IIterable_1__MobileBroadbandPinType_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.MobileBroadbandPinType>
  IVectorView_1__MobileBroadbandPinType = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): MobileBroadbandPinType; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: MobileBroadbandPinType; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMobileBroadbandPinType): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult
  IMobileBroadbandPinOperationResult = interface(IInspectable)
  ['{11DDDC32-31E7-49F5-B663-123D3BEF0362}']
    function get_IsSuccessful: Boolean; safecall;
    function get_AttemptsRemaining: Cardinal; safecall;
    property AttemptsRemaining: Cardinal read get_AttemptsRemaining;
    property IsSuccessful: Boolean read get_IsSuccessful;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult_Delegate_Base = interface(IUnknown)
  ['{595AD094-60E3-5349-8FE6-EA8ECBBB2541}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMobileBroadbandPinOperationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult = interface(AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult_Delegate_Base)
  ['{5376FE24-4843-598E-9423-B7A8FDE0AF79}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult>
  IAsyncOperation_1__IMobileBroadbandPinOperationResult_Base = interface(IInspectable)
  ['{2F76661C-2F74-5CE2-99F9-47D1A3A13633}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult; safecall;
    function GetResults: IMobileBroadbandPinOperationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandPinOperationResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinOperationResult>
  IAsyncOperation_1__IMobileBroadbandPinOperationResult = interface(IAsyncOperation_1__IMobileBroadbandPinOperationResult_Base)
  ['{251B212E-0F07-5D6C-8BE4-C75257773102}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPin
  IMobileBroadbandPin = interface(IInspectable)
  ['{E661D709-E779-45BF-8281-75323DF9E321}']
    function get_Type: MobileBroadbandPinType; safecall;
    function get_LockState: MobileBroadbandPinLockState; safecall;
    function get_Format: MobileBroadbandPinFormat; safecall;
    function get_Enabled: Boolean; safecall;
    function get_MaxLength: Cardinal; safecall;
    function get_MinLength: Cardinal; safecall;
    function get_AttemptsRemaining: Cardinal; safecall;
    function EnableAsync(currentPin: HSTRING): IAsyncOperation_1__IMobileBroadbandPinOperationResult; safecall;
    function DisableAsync(currentPin: HSTRING): IAsyncOperation_1__IMobileBroadbandPinOperationResult; safecall;
    function EnterAsync(currentPin: HSTRING): IAsyncOperation_1__IMobileBroadbandPinOperationResult; safecall;
    function ChangeAsync(currentPin: HSTRING; newPin: HSTRING): IAsyncOperation_1__IMobileBroadbandPinOperationResult; safecall;
    function UnblockAsync(pinUnblockKey: HSTRING; newPin: HSTRING): IAsyncOperation_1__IMobileBroadbandPinOperationResult; safecall;
    property AttemptsRemaining: Cardinal read get_AttemptsRemaining;
    property Enabled: Boolean read get_Enabled;
    property Format: MobileBroadbandPinFormat read get_Format;
    property LockState: MobileBroadbandPinLockState read get_LockState;
    property MaxLength: Cardinal read get_MaxLength;
    property MinLength: Cardinal read get_MinLength;
    property &Type: MobileBroadbandPinType read get_Type;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinManager
  IMobileBroadbandPinManager = interface(IInspectable)
  ['{83567EDD-6E1F-4B9B-A413-2B1F50CC36DF}']
    function get_SupportedPins: IVectorView_1__MobileBroadbandPinType; safecall;
    function GetPin(pinType: MobileBroadbandPinType): IMobileBroadbandPin; safecall;
    property SupportedPins: IVectorView_1__MobileBroadbandPinType read get_SupportedPins;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceInformation2
  IMobileBroadbandDeviceInformation2 = interface(IInspectable)
  ['{2E467AF1-F932-4737-A722-03BA72370CB8}']
    function get_PinManager: IMobileBroadbandPinManager; safecall;
    function get_Revision: HSTRING; safecall;
    function get_SerialNumber: HSTRING; safecall;
    property PinManager: IMobileBroadbandPinManager read get_PinManager;
    property Revision: HSTRING read get_Revision;
    property SerialNumber: HSTRING read get_SerialNumber;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceInformation3
  IMobileBroadbandDeviceInformation3 = interface(IInspectable)
  ['{E08BB4BD-5D30-4B5A-92CC-D54DF881D49E}']
    function get_SimSpn: HSTRING; safecall;
    function get_SimPnn: HSTRING; safecall;
    function get_SimGid1: HSTRING; safecall;
    property SimGid1: HSTRING read get_SimGid1;
    property SimPnn: HSTRING read get_SimPnn;
    property SimSpn: HSTRING read get_SimSpn;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceTriggerDetails
  IMobileBroadbandDeviceServiceTriggerDetails = interface(IInspectable)
  ['{4A055B70-B9AE-4458-9241-A6A5FBF18A0C}']
    function get_DeviceId: HSTRING; safecall;
    function get_DeviceServiceId: TGuid; safecall;
    function get_ReceivedData: IBuffer; safecall;
    property DeviceId: HSTRING read get_DeviceId;
    property DeviceServiceId: TGuid read get_DeviceServiceId;
    property ReceivedData: IBuffer read get_ReceivedData;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  IIterator_1__IMobileBroadbandDeviceServiceInformation_Base = interface(IInspectable)
  ['{D8D776F6-4692-5461-9155-816E63BAC874}']
    function get_Current: IMobileBroadbandDeviceServiceInformation; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIMobileBroadbandDeviceServiceInformation): Cardinal; safecall;
    property Current: IMobileBroadbandDeviceServiceInformation read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  IIterator_1__IMobileBroadbandDeviceServiceInformation = interface(IIterator_1__IMobileBroadbandDeviceServiceInformation_Base)
  ['{05415FF7-5FDE-51DC-A0D5-4F704E3651A3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  IIterable_1__IMobileBroadbandDeviceServiceInformation_Base = interface(IInspectable)
  ['{88511855-6FE6-5694-83A7-991E29033DE5}']
    function First: IIterator_1__IMobileBroadbandDeviceServiceInformation; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  IIterable_1__IMobileBroadbandDeviceServiceInformation = interface(IIterable_1__IMobileBroadbandDeviceServiceInformation_Base)
  ['{C06A2C8F-A6D4-5C58-937B-A9DDBB8FBBD1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  IIterator_1__IMobileBroadbandUiccApp_Base = interface(IInspectable)
  ['{4BB2066F-1B75-57CF-A722-1E58BFC5AE50}']
    function get_Current: IMobileBroadbandUiccApp; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIMobileBroadbandUiccApp): Cardinal; safecall;
    property Current: IMobileBroadbandUiccApp read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  IIterator_1__IMobileBroadbandUiccApp = interface(IIterator_1__IMobileBroadbandUiccApp_Base)
  ['{776CC781-A67D-5518-A04C-F787677835C1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  IIterable_1__IMobileBroadbandUiccApp_Base = interface(IInspectable)
  ['{AF538114-BD14-53B0-B1D1-841DCAA451AD}']
    function First: IIterator_1__IMobileBroadbandUiccApp; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  IIterable_1__IMobileBroadbandUiccApp = interface(IIterable_1__IMobileBroadbandUiccApp_Base)
  ['{1E7B803F-BB49-5260-B98C-719667953BA7}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.MobileBroadbandModemStatus>
  AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus_Delegate_Base = interface(IUnknown)
  ['{B8628318-EE4F-5AF4-9E3B-AF994FA96C51}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__MobileBroadbandModemStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.MobileBroadbandModemStatus>
  AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus = interface(AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.MobileBroadbandModemStatus>
  IAsyncOperation_1__MobileBroadbandModemStatus_Base = interface(IInspectable)
  ['{AB0D25AB-68CD-54AB-B19C-624711659D3D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus; safecall;
    function GetResults: MobileBroadbandModemStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__MobileBroadbandModemStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.MobileBroadbandModemStatus>
  IAsyncOperation_1__MobileBroadbandModemStatus = interface(IAsyncOperation_1__MobileBroadbandModemStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandModem2
  IMobileBroadbandModem2 = interface(IInspectable)
  ['{12862B28-B9EB-4EE2-BBE3-711F53EEA373}']
    function GetIsPassthroughEnabledAsync: IAsyncOperation_1__Boolean; safecall;
    function SetIsPassthroughEnabledAsync(value: Boolean): IAsyncOperation_1__MobileBroadbandModemStatus; safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandPco
  IMobileBroadbandPco = interface(IInspectable)
  ['{D4E4FCBE-E3A3-43C5-A87B-6C86D229D7FA}']
    function get_Data: IBuffer; safecall;
    function get_IsComplete: Boolean; safecall;
    function get_DeviceId: HSTRING; safecall;
    property Data: IBuffer read get_Data;
    property DeviceId: HSTRING read get_DeviceId;
    property IsComplete: Boolean read get_IsComplete;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandPco>
  AsyncOperationCompletedHandler_1__IMobileBroadbandPco = interface(IUnknown)
  ['{A77FCB2E-97AF-5C65-AA80-2B4E749FF659}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMobileBroadbandPco; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandPco>
  IAsyncOperation_1__IMobileBroadbandPco = interface(IInspectable)
  ['{9124665C-3B7F-5CBC-B055-38808EB5DDEC}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMobileBroadbandPco); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandPco; safecall;
    function GetResults: IMobileBroadbandPco; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandPco read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandModem3
  IMobileBroadbandModem3 = interface(IInspectable)
  ['{E9FEC6EA-2F34-4582-9102-C314D2A87EEC}']
    function TryGetPcoAsync: IAsyncOperation_1__IMobileBroadbandPco; safecall;
    function get_IsInEmergencyCallMode: Boolean; safecall;
    function add_IsInEmergencyCallModeChanged(handler: TypedEventHandler_2__IMobileBroadbandModem__IInspectable): EventRegistrationToken; safecall;
    procedure remove_IsInEmergencyCallModeChanged(token: EventRegistrationToken); safecall;
    property IsInEmergencyCallMode: Boolean read get_IsInEmergencyCallMode;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandModemStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_MobileBroadbandModem)]
  IMobileBroadbandModemStatics = interface(IInspectable)
  ['{F99ED637-D6F1-4A78-8CBC-6421A65063C8}']
    function GetDeviceSelector: HSTRING; safecall;
    function FromId(deviceId: HSTRING): IMobileBroadbandModem; safecall;
    function GetDefault: IMobileBroadbandModem; safecall;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandNetwork2
  IMobileBroadbandNetwork2 = interface(IInspectable)
  ['{5A55DB22-62F7-4BDD-BA1D-477441960BA0}']
    function GetVoiceCallSupportAsync: IAsyncOperation_1__Boolean; safecall;
    function get_RegistrationUiccApps: IVectorView_1__IMobileBroadbandUiccApp; safecall;
    property RegistrationUiccApps: IVectorView_1__IMobileBroadbandUiccApp read get_RegistrationUiccApps;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange
  IMobileBroadbandNetworkRegistrationStateChange = interface(IInspectable)
  ['{BEAF94E1-960F-49B4-A08D-7D85E968C7EC}']
    function get_DeviceId: HSTRING; safecall;
    function get_Network: IMobileBroadbandNetwork; safecall;
    property DeviceId: HSTRING read get_DeviceId;
    property Network: IMobileBroadbandNetwork read get_Network;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IIterator_1__IMobileBroadbandNetworkRegistrationStateChange_Base = interface(IInspectable)
  ['{9CB0F858-E589-57A7-9D01-2C6291567CC7}']
    function get_Current: IMobileBroadbandNetworkRegistrationStateChange; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIMobileBroadbandNetworkRegistrationStateChange): Cardinal; safecall;
    property Current: IMobileBroadbandNetworkRegistrationStateChange read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IIterator_1__IMobileBroadbandNetworkRegistrationStateChange = interface(IIterator_1__IMobileBroadbandNetworkRegistrationStateChange_Base)
  ['{3D7E7450-0C2A-537D-9F77-937B4856757A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IIterable_1__IMobileBroadbandNetworkRegistrationStateChange_Base = interface(IInspectable)
  ['{0B90BB30-660C-51C6-9B8C-31DD8486E10E}']
    function First: IIterator_1__IMobileBroadbandNetworkRegistrationStateChange; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IIterable_1__IMobileBroadbandNetworkRegistrationStateChange = interface(IIterable_1__IMobileBroadbandNetworkRegistrationStateChange_Base)
  ['{E0E40C22-CFC9-5DF3-8EBE-C551D691287E}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChange>
  IVectorView_1__IMobileBroadbandNetworkRegistrationStateChange = interface(IInspectable)
  ['{BD95AB62-44B2-5ADD-B16E-03D48253F162}']
    function GetAt(index: Cardinal): IMobileBroadbandNetworkRegistrationStateChange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IMobileBroadbandNetworkRegistrationStateChange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIMobileBroadbandNetworkRegistrationStateChange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandNetworkRegistrationStateChangeTriggerDetails
  IMobileBroadbandNetworkRegistrationStateChangeTriggerDetails = interface(IInspectable)
  ['{89135CFF-28B8-46AA-B137-1C4B0F21EDFE}']
    function get_NetworkRegistrationStateChanges: IVectorView_1__IMobileBroadbandNetworkRegistrationStateChange; safecall;
    property NetworkRegistrationStateChanges: IVectorView_1__IMobileBroadbandNetworkRegistrationStateChange read get_NetworkRegistrationStateChanges;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange
  IMobileBroadbandPinLockStateChange = interface(IInspectable)
  ['{BE16673E-1F04-4F95-8B90-E7F559DDE7E5}']
    function get_DeviceId: HSTRING; safecall;
    function get_PinType: MobileBroadbandPinType; safecall;
    function get_PinLockState: MobileBroadbandPinLockState; safecall;
    property DeviceId: HSTRING read get_DeviceId;
    property PinLockState: MobileBroadbandPinLockState read get_PinLockState;
    property PinType: MobileBroadbandPinType read get_PinType;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IIterator_1__IMobileBroadbandPinLockStateChange_Base = interface(IInspectable)
  ['{E61B479F-7BD9-5550-BC69-F9C2F71C6A05}']
    function get_Current: IMobileBroadbandPinLockStateChange; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIMobileBroadbandPinLockStateChange): Cardinal; safecall;
    property Current: IMobileBroadbandPinLockStateChange read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IIterator_1__IMobileBroadbandPinLockStateChange = interface(IIterator_1__IMobileBroadbandPinLockStateChange_Base)
  ['{9F6CDEEA-0EF0-5BCE-BEBA-F8667FEFEC97}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IIterable_1__IMobileBroadbandPinLockStateChange_Base = interface(IInspectable)
  ['{AA4A8700-9943-59A3-8647-D373FD5E0E2B}']
    function First: IIterator_1__IMobileBroadbandPinLockStateChange; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IIterable_1__IMobileBroadbandPinLockStateChange = interface(IIterable_1__IMobileBroadbandPinLockStateChange_Base)
  ['{3876476B-DF73-551E-A695-2F70A4292032}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChange>
  IVectorView_1__IMobileBroadbandPinLockStateChange = interface(IInspectable)
  ['{8AAE04EC-BB01-53AD-A97D-612EEE2A0D2A}']
    function GetAt(index: Cardinal): IMobileBroadbandPinLockStateChange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IMobileBroadbandPinLockStateChange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIMobileBroadbandPinLockStateChange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandPinLockStateChangeTriggerDetails
  IMobileBroadbandPinLockStateChangeTriggerDetails = interface(IInspectable)
  ['{D338C091-3E91-4D38-9036-AEE83A6E79AD}']
    function get_PinLockStateChanges: IVectorView_1__IMobileBroadbandPinLockStateChange; safecall;
    property PinLockStateChanges: IVectorView_1__IMobileBroadbandPinLockStateChange read get_PinLockStateChanges;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange
  IMobileBroadbandRadioStateChange = interface(IInspectable)
  ['{B054A561-9833-4AED-9717-4348B21A24B3}']
    function get_DeviceId: HSTRING; safecall;
    function get_RadioState: MobileBroadbandRadioState; safecall;
    property DeviceId: HSTRING read get_DeviceId;
    property RadioState: MobileBroadbandRadioState read get_RadioState;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IIterator_1__IMobileBroadbandRadioStateChange_Base = interface(IInspectable)
  ['{C088CEC3-08E5-5F35-A2B9-0900D028C83B}']
    function get_Current: IMobileBroadbandRadioStateChange; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIMobileBroadbandRadioStateChange): Cardinal; safecall;
    property Current: IMobileBroadbandRadioStateChange read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IIterator_1__IMobileBroadbandRadioStateChange = interface(IIterator_1__IMobileBroadbandRadioStateChange_Base)
  ['{C47426E0-8EBF-523A-BD9A-E23C0C7E5D7F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IIterable_1__IMobileBroadbandRadioStateChange_Base = interface(IInspectable)
  ['{C385ADAA-574C-5AD8-98C2-61309525132D}']
    function First: IIterator_1__IMobileBroadbandRadioStateChange; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IIterable_1__IMobileBroadbandRadioStateChange = interface(IIterable_1__IMobileBroadbandRadioStateChange_Base)
  ['{E4E62A7B-F718-58A8-9FFA-062BD7E51E20}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChange>
  IVectorView_1__IMobileBroadbandRadioStateChange = interface(IInspectable)
  ['{D1CB8E13-2478-5488-A9F6-863309D9DF35}']
    function GetAt(index: Cardinal): IMobileBroadbandRadioStateChange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IMobileBroadbandRadioStateChange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIMobileBroadbandRadioStateChange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandRadioStateChangeTriggerDetails
  IMobileBroadbandRadioStateChangeTriggerDetails = interface(IInspectable)
  ['{71301ACE-093C-42C6-B0DB-AD1F75A65445}']
    function get_RadioStateChanges: IVectorView_1__IMobileBroadbandRadioStateChange; safecall;
    property RadioStateChanges: IVectorView_1__IMobileBroadbandRadioStateChange read get_RadioStateChanges;
  end;

  // Windows.Networking.NetworkOperators.INetworkOperatorNotificationEventDetails
  INetworkOperatorNotificationEventDetails = interface(IInspectable)
  ['{BC68A9D1-82E1-4488-9F2C-1276C2468FAC}']
    function get_NotificationType: NetworkOperatorEventMessageType; safecall;
    function get_NetworkAccountId: HSTRING; safecall;
    function get_EncodingType: Byte; safecall;
    function get_Message: HSTRING; safecall;
    function get_RuleId: HSTRING; safecall;
    function get_SmsMessage: ISmsMessage; safecall;
    property EncodingType: Byte read get_EncodingType;
    property &Message: HSTRING read get_Message;
    property NetworkAccountId: HSTRING read get_NetworkAccountId;
    property NotificationType: NetworkOperatorEventMessageType read get_NotificationType;
    property RuleId: HSTRING read get_RuleId;
    property SmsMessage: ISmsMessage read get_SmsMessage;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringAccessPointConfiguration
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_NetworkOperatorTetheringAccessPointConfiguration)]
  INetworkOperatorTetheringAccessPointConfiguration = interface(IInspectable)
  ['{0BCC0284-412E-403D-ACC6-B757E34774A4}']
    function get_Ssid: HSTRING; safecall;
    procedure put_Ssid(value: HSTRING); safecall;
    function get_Passphrase: HSTRING; safecall;
    procedure put_Passphrase(value: HSTRING); safecall;
    property Passphrase: HSTRING read get_Passphrase write put_Passphrase;
    property Ssid: HSTRING read get_Ssid write put_Ssid;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringAccessPointConfiguration2
  INetworkOperatorTetheringAccessPointConfiguration2 = interface(IInspectable)
  ['{B1809142-7238-59A0-928B-74AB46FD64B6}']
    function IsBandSupported(band: TetheringWiFiBand): Boolean; safecall;
    function IsBandSupportedAsync(band: TetheringWiFiBand): IAsyncOperation_1__Boolean; safecall;
    function get_Band: TetheringWiFiBand; safecall;
    procedure put_Band(value: TetheringWiFiBand); safecall;
    property Band: TetheringWiFiBand read get_Band write put_Band;
  end;

  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient
  INetworkOperatorTetheringClient = interface(IInspectable)
  ['{709D254C-595F-4847-BB30-646935542918}']
    function get_MacAddress: HSTRING; safecall;
    function get_HostNames: IVectorView_1__IHostName; safecall;
    property HostNames: IVectorView_1__IHostName read get_HostNames;
    property MacAddress: HSTRING read get_MacAddress;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IIterator_1__INetworkOperatorTetheringClient_Base = interface(IInspectable)
  ['{5653D065-C708-5341-BC05-D3B9CECD2AC7}']
    function get_Current: INetworkOperatorTetheringClient; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PINetworkOperatorTetheringClient): Cardinal; safecall;
    property Current: INetworkOperatorTetheringClient read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IIterator_1__INetworkOperatorTetheringClient = interface(IIterator_1__INetworkOperatorTetheringClient_Base)
  ['{52FC26D5-75EF-527D-AB08-C10685F93A66}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IIterable_1__INetworkOperatorTetheringClient_Base = interface(IInspectable)
  ['{4762ECB3-AF48-5B63-89B7-78A42056549F}']
    function First: IIterator_1__INetworkOperatorTetheringClient; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IIterable_1__INetworkOperatorTetheringClient = interface(IIterable_1__INetworkOperatorTetheringClient_Base)
  ['{443E479B-46B8-5D56-B4EA-13101020B6A6}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringClient>
  IVectorView_1__INetworkOperatorTetheringClient = interface(IInspectable)
  ['{23AA5A28-AD35-5C47-976A-53269CDC5293}']
    function GetAt(index: Cardinal): INetworkOperatorTetheringClient; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: INetworkOperatorTetheringClient; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PINetworkOperatorTetheringClient): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringClientManager
  INetworkOperatorTetheringClientManager = interface(IInspectable)
  ['{91B14016-8DCA-4225-BBED-EEF8B8D718D7}']
    function GetTetheringClients: IVectorView_1__INetworkOperatorTetheringClient; safecall;
  end;

  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringEntitlementCheck
  INetworkOperatorTetheringEntitlementCheck = interface(IInspectable)
  ['{0108916D-9E9A-4AF6-8DA3-60493B19C204}']
    procedure AuthorizeTethering(allow: Boolean; entitlementFailureReason: HSTRING); safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult
  INetworkOperatorTetheringOperationResult = interface(IInspectable)
  ['{EBD203A1-01BA-476D-B4B3-BF3D12C8F80C}']
    function get_Status: TetheringOperationStatus; safecall;
    function get_AdditionalErrorMessage: HSTRING; safecall;
    property AdditionalErrorMessage: HSTRING read get_AdditionalErrorMessage;
    property Status: TetheringOperationStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult>
  AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult_Delegate_Base = interface(IUnknown)
  ['{A936B927-7537-59C6-89DE-33F36A9725AB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__INetworkOperatorTetheringOperationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult>
  AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult = interface(AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult_Delegate_Base)
  ['{E8DB81F2-A338-5D47-89B0-F26A42FEE582}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult>
  IAsyncOperation_1__INetworkOperatorTetheringOperationResult_Base = interface(IInspectable)
  ['{601B30ED-9B7F-54B6-B61B-24A09BC56304}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult; safecall;
    function GetResults: INetworkOperatorTetheringOperationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__INetworkOperatorTetheringOperationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.INetworkOperatorTetheringOperationResult>
  IAsyncOperation_1__INetworkOperatorTetheringOperationResult = interface(IAsyncOperation_1__INetworkOperatorTetheringOperationResult_Base)
  ['{3112BC41-BE86-53A8-81C0-D0BA85CB5CE2}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManager
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_NetworkOperatorTetheringManager)]
  INetworkOperatorTetheringManager = interface(IInspectable)
  ['{D45A8DA0-0E86-4D98-8BA4-DD70D4B764D3}']
    function get_MaxClientCount: Cardinal; safecall;
    function get_ClientCount: Cardinal; safecall;
    function get_TetheringOperationalState: TetheringOperationalState; safecall;
    function GetCurrentAccessPointConfiguration: INetworkOperatorTetheringAccessPointConfiguration; safecall;
    function ConfigureAccessPointAsync(configuration: INetworkOperatorTetheringAccessPointConfiguration): IAsyncAction; safecall;
    function StartTetheringAsync: IAsyncOperation_1__INetworkOperatorTetheringOperationResult; safecall;
    function StopTetheringAsync: IAsyncOperation_1__INetworkOperatorTetheringOperationResult; safecall;
    property ClientCount: Cardinal read get_ClientCount;
    property MaxClientCount: Cardinal read get_MaxClientCount;
    property TetheringOperationalState_: TetheringOperationalState read get_TetheringOperationalState;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_NetworkOperatorTetheringManager)]
  INetworkOperatorTetheringManagerStatics = interface(IInspectable)
  ['{3EBCBACC-F8C3-405C-9964-70A1EEABE194}']
    function GetTetheringCapability(networkAccountId: HSTRING): TetheringCapability; safecall;
    function CreateFromNetworkAccountId(networkAccountId: HSTRING): INetworkOperatorTetheringManager; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics2
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_NetworkOperatorTetheringManager)]
  INetworkOperatorTetheringManagerStatics2 = interface(IInspectable)
  ['{5B235412-35F0-49E7-9B08-16D278FBAA42}']
    function GetTetheringCapabilityFromConnectionProfile(profile: IConnectionProfile): TetheringCapability; safecall;
    function CreateFromConnectionProfile(profile: IConnectionProfile): INetworkOperatorTetheringManager; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics3
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_NetworkOperatorTetheringManager)]
  INetworkOperatorTetheringManagerStatics3 = interface(IInspectable)
  ['{8FDAADB6-4AF9-4F21-9B58-D53E9F24231E}']
    function CreateFromConnectionProfile(profile: IConnectionProfile; adapter: INetworkAdapter): INetworkOperatorTetheringManager; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics4
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_NetworkOperatorTetheringManager)]
  INetworkOperatorTetheringManagerStatics4 = interface(IInspectable)
  ['{B3B9F9D0-EBFF-46A4-A847-D663D8B0977E}']
    function IsNoConnectionsTimeoutEnabled: Boolean; safecall;
    procedure EnableNoConnectionsTimeout; safecall;
    function EnableNoConnectionsTimeoutAsync: IAsyncAction; safecall;
    procedure DisableNoConnectionsTimeout; safecall;
    function DisableNoConnectionsTimeoutAsync: IAsyncAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults
  IProvisionFromXmlDocumentResults = interface(IInspectable)
  ['{217700E0-8203-11DF-ADB9-F4CE462D9137}']
    function get_AllElementsProvisioned: Boolean; safecall;
    function get_ProvisionResultsXml: HSTRING; safecall;
    property AllElementsProvisioned: Boolean read get_AllElementsProvisioned;
    property ProvisionResultsXml: HSTRING read get_ProvisionResultsXml;
  end;

  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IProvisionedProfile
  IProvisionedProfile = interface(IInspectable)
  ['{217700E0-8202-11DF-ADB9-F4CE462D9137}']
    procedure UpdateCost(value: NetworkCostType); safecall;
    procedure UpdateUsage(value: ProfileUsage); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults>
  AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults_Delegate_Base = interface(IUnknown)
  ['{7A7EEE1B-17F9-5A41-861B-C30EA127D0F1}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IProvisionFromXmlDocumentResults; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults>
  AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults = interface(AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults_Delegate_Base)
  ['{77FF2BE3-9CFD-5034-89B9-FF78EFAFCB68}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults>
  IAsyncOperation_1__IProvisionFromXmlDocumentResults_Base = interface(IInspectable)
  ['{9F8FE338-C6B1-5614-A14F-8977A77E17F2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults; safecall;
    function GetResults: IProvisionFromXmlDocumentResults; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IProvisionFromXmlDocumentResults read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IProvisionFromXmlDocumentResults>
  IAsyncOperation_1__IProvisionFromXmlDocumentResults = interface(IAsyncOperation_1__IProvisionFromXmlDocumentResults_Base)
  ['{68AFF662-59AD-5B99-81EE-9AA2C780B858}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IProvisioningAgent
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_ProvisioningAgent)]
  IProvisioningAgent = interface(IInspectable)
  ['{217700E0-8201-11DF-ADB9-F4CE462D9137}']
    function ProvisionFromXmlDocumentAsync(provisioningXmlDocument: HSTRING): IAsyncOperation_1__IProvisionFromXmlDocumentResults; safecall;
    function GetProvisionedProfile(mediaType: ProfileMediaType; profileName: HSTRING): IProvisionedProfile; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IProvisioningAgentStaticMethods
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_ProvisioningAgent)]
  IProvisioningAgentStaticMethods = interface(IInspectable)
  ['{217700E0-8101-11DF-ADB9-F4CE462D9137}']
    function CreateFromNetworkAccountId(networkAccountId: HSTRING): IProvisioningAgent; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IUssdMessage
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_UssdMessage)]
  IUssdMessage = interface(IInspectable)
  ['{2F9ACF82-2004-4D5D-BF81-2ABA1B4BE4A8}']
    function get_DataCodingScheme: Byte; safecall;
    procedure put_DataCodingScheme(value: Byte); safecall;
    function GetPayload(resultSize: Cardinal; resultValue: PByte): HRESULT; stdcall;
    procedure SetPayload(valueSize: Cardinal; value: PByte); safecall;
    function get_PayloadAsText: HSTRING; safecall;
    procedure put_PayloadAsText(value: HSTRING); safecall;
    property DataCodingScheme: Byte read get_DataCodingScheme write put_DataCodingScheme;
    property PayloadAsText: HSTRING read get_PayloadAsText write put_PayloadAsText;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IUssdMessageFactory
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_UssdMessage)]
  IUssdMessageFactory = interface(IInspectable)
  ['{2F9ACF82-1003-4D5D-BF81-2ABA1B4BE4A8}']
    function CreateMessage(messageText: HSTRING): IUssdMessage; safecall;
  end;

  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IUssdReply
  IUssdReply = interface(IInspectable)
  ['{2F9ACF82-2005-4D5D-BF81-2ABA1B4BE4A8}']
    function get_ResultCode: UssdResultCode; safecall;
    function get_Message: IUssdMessage; safecall;
    property &Message: IUssdMessage read get_Message;
    property ResultCode: UssdResultCode read get_ResultCode;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IUssdReply>
  AsyncOperationCompletedHandler_1__IUssdReply_Delegate_Base = interface(IUnknown)
  ['{9385BD8F-5E14-557A-A7F1-63F33D9ECACF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IUssdReply; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IUssdReply>
  AsyncOperationCompletedHandler_1__IUssdReply = interface(AsyncOperationCompletedHandler_1__IUssdReply_Delegate_Base)
  ['{354DAF3D-8EB1-5BEE-8710-2DF30BF322A0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IUssdReply>
  IAsyncOperation_1__IUssdReply_Base = interface(IInspectable)
  ['{9DD0A063-6153-5AFD-8288-8770DDCFF2DB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IUssdReply); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IUssdReply; safecall;
    function GetResults: IUssdReply; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IUssdReply read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IUssdReply>
  IAsyncOperation_1__IUssdReply = interface(IAsyncOperation_1__IUssdReply_Base)
  ['{032A3426-9808-53E4-8012-2C0A747442DA}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IUssdSession
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_UssdSession)]
  IUssdSession = interface(IInspectable)
  ['{2F9ACF82-2002-4D5D-BF81-2ABA1B4BE4A8}']
    function SendMessageAndGetReplyAsync(&message: IUssdMessage): IAsyncOperation_1__IUssdReply; safecall;
    procedure Close; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Networking.NetworkOperators.IUssdSessionStatics
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_UssdSession)]
  IUssdSessionStatics = interface(IInspectable)
  ['{2F9ACF82-1001-4D5D-BF81-2ABA1B4BE4A8}']
    function CreateFromNetworkAccountId(networkAccountId: HSTRING): IUssdSession; safecall;
    function CreateFromNetworkInterfaceId(networkInterfaceId: HSTRING): IUssdSession; safecall;
  end;

  // Windows.Networking.NetworkOperators.KnownCSimFilePaths
  // DualAPI
  // Statics: "Windows.Networking.NetworkOperators.IKnownCSimFilePathsStatics"
  TKnownCSimFilePaths = class(TWinRTGenericImportS<IKnownCSimFilePathsStatics>)
  public
    // -> IKnownCSimFilePathsStatics
    class function get_EFSpn: IVectorView_1__Cardinal; static; inline;
    class function get_Gid1: IVectorView_1__Cardinal; static; inline;
    class function get_Gid2: IVectorView_1__Cardinal; static; inline;
    class property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    class property Gid1: IVectorView_1__Cardinal read get_Gid1;
    class property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // Windows.Networking.NetworkOperators.KnownRuimFilePaths
  // DualAPI
  // Statics: "Windows.Networking.NetworkOperators.IKnownRuimFilePathsStatics"
  TKnownRuimFilePaths = class(TWinRTGenericImportS<IKnownRuimFilePathsStatics>)
  public
    // -> IKnownRuimFilePathsStatics
    class function get_EFSpn: IVectorView_1__Cardinal; static; inline;
    class function get_Gid1: IVectorView_1__Cardinal; static; inline;
    class function get_Gid2: IVectorView_1__Cardinal; static; inline;
    class property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    class property Gid1: IVectorView_1__Cardinal read get_Gid1;
    class property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // Windows.Networking.NetworkOperators.KnownSimFilePaths
  // DualAPI
  // Statics: "Windows.Networking.NetworkOperators.IKnownSimFilePathsStatics"
  TKnownSimFilePaths = class(TWinRTGenericImportS<IKnownSimFilePathsStatics>)
  public
    // -> IKnownSimFilePathsStatics
    class function get_EFOns: IVectorView_1__Cardinal; static; inline;
    class function get_EFSpn: IVectorView_1__Cardinal; static; inline;
    class function get_Gid1: IVectorView_1__Cardinal; static; inline;
    class function get_Gid2: IVectorView_1__Cardinal; static; inline;
    class property EFOns: IVectorView_1__Cardinal read get_EFOns;
    class property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    class property Gid1: IVectorView_1__Cardinal read get_Gid1;
    class property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // Windows.Networking.NetworkOperators.KnownUSimFilePaths
  // DualAPI
  // Statics: "Windows.Networking.NetworkOperators.IKnownUSimFilePathsStatics"
  TKnownUSimFilePaths = class(TWinRTGenericImportS<IKnownUSimFilePathsStatics>)
  public
    // -> IKnownUSimFilePathsStatics
    class function get_EFSpn: IVectorView_1__Cardinal; static; inline;
    class function get_EFOpl: IVectorView_1__Cardinal; static; inline;
    class function get_EFPnn: IVectorView_1__Cardinal; static; inline;
    class function get_Gid1: IVectorView_1__Cardinal; static; inline;
    class function get_Gid2: IVectorView_1__Cardinal; static; inline;
    class property EFOpl: IVectorView_1__Cardinal read get_EFOpl;
    class property EFPnn: IVectorView_1__Cardinal read get_EFPnn;
    class property EFSpn: IVectorView_1__Cardinal read get_EFSpn;
    class property Gid1: IVectorView_1__Cardinal read get_Gid1;
    class property Gid2: IVectorView_1__Cardinal read get_Gid2;
  end;

  // Windows.Networking.NetworkOperators.MobileBroadbandAccount
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.IMobileBroadbandAccount
  // Implements: Windows.Networking.NetworkOperators.IMobileBroadbandAccount2
  // Implements: Windows.Networking.NetworkOperators.IMobileBroadbandAccount3
  // Statics: "Windows.Networking.NetworkOperators.IMobileBroadbandAccountStatics"
  TMobileBroadbandAccount = class(TWinRTGenericImportS<IMobileBroadbandAccountStatics>)
  public
    // -> IMobileBroadbandAccountStatics
    class function get_AvailableNetworkAccountIds: IVectorView_1__HSTRING; static; inline;
    class function CreateFromNetworkAccountId(networkAccountId: HSTRING): IMobileBroadbandAccount; static; inline;
    class property AvailableNetworkAccountIds: IVectorView_1__HSTRING read get_AvailableNetworkAccountIds;
  end;

  // Windows.Networking.NetworkOperators.MobileBroadbandAccountWatcher
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher
  // Instantiable: "IMobileBroadbandAccountWatcher"
  TMobileBroadbandAccountWatcher = class(TWinRTGenericImportI<IMobileBroadbandAccountWatcher>) end;

  // Windows.Networking.NetworkOperators.MobileBroadbandModem
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.IMobileBroadbandModem
  // Implements: Windows.Networking.NetworkOperators.IMobileBroadbandModem2
  // Implements: Windows.Networking.NetworkOperators.IMobileBroadbandModem3
  // Statics: "Windows.Networking.NetworkOperators.IMobileBroadbandModemStatics"
  TMobileBroadbandModem = class(TWinRTGenericImportS<IMobileBroadbandModemStatics>)
  public
    // -> IMobileBroadbandModemStatics
    class function GetDeviceSelector: HSTRING; static; inline;
    class function FromId(deviceId: HSTRING): IMobileBroadbandModem; static; inline;
    class function GetDefault: IMobileBroadbandModem; static; inline;
  end;

  // Windows.Networking.NetworkOperators.NetworkOperatorTetheringAccessPointConfiguration
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.INetworkOperatorTetheringAccessPointConfiguration
  // Implements: Windows.Networking.NetworkOperators.INetworkOperatorTetheringAccessPointConfiguration2
  // Instantiable: "INetworkOperatorTetheringAccessPointConfiguration"
  TNetworkOperatorTetheringAccessPointConfiguration = class(TWinRTGenericImportI<INetworkOperatorTetheringAccessPointConfiguration>) end;

  // Windows.Networking.NetworkOperators.NetworkOperatorTetheringManager
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.INetworkOperatorTetheringManager
  // Implements: Windows.Networking.NetworkOperators.INetworkOperatorTetheringClientManager
  // Statics: "Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics"
  // Statics: "Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics2"
  // Statics: "Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics3"
  // Statics: "Windows.Networking.NetworkOperators.INetworkOperatorTetheringManagerStatics4"
  TNetworkOperatorTetheringManager = class(TWinRTGenericImportS4<INetworkOperatorTetheringManagerStatics, INetworkOperatorTetheringManagerStatics2, INetworkOperatorTetheringManagerStatics3, INetworkOperatorTetheringManagerStatics4>)
  public
    // -> INetworkOperatorTetheringManagerStatics
    class function GetTetheringCapability(networkAccountId: HSTRING): TetheringCapability; static; inline;
    class function CreateFromNetworkAccountId(networkAccountId: HSTRING): INetworkOperatorTetheringManager; static; inline;

    // -> INetworkOperatorTetheringManagerStatics2
    class function GetTetheringCapabilityFromConnectionProfile(profile: IConnectionProfile): TetheringCapability; static; inline;
    class function CreateFromConnectionProfile(profile: IConnectionProfile): INetworkOperatorTetheringManager; overload; static; inline;

    // -> INetworkOperatorTetheringManagerStatics3
    class function CreateFromConnectionProfile(profile: IConnectionProfile; adapter: INetworkAdapter): INetworkOperatorTetheringManager; overload; static; inline;

    // -> INetworkOperatorTetheringManagerStatics4
    class function IsNoConnectionsTimeoutEnabled: Boolean; static; inline;
    class procedure EnableNoConnectionsTimeout; static; inline;
    class function EnableNoConnectionsTimeoutAsync: IAsyncAction; static; inline;
    class procedure DisableNoConnectionsTimeout; static; inline;
    class function DisableNoConnectionsTimeoutAsync: IAsyncAction; static; inline;
  end;

  // Windows.Networking.NetworkOperators.ProvisioningAgent
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.IProvisioningAgent
  // Statics: "Windows.Networking.NetworkOperators.IProvisioningAgentStaticMethods"
  // Instantiable: "IProvisioningAgent"
  TProvisioningAgent = class(TWinRTGenericImportSI<IProvisioningAgentStaticMethods, IProvisioningAgent>)
  public
    // -> IProvisioningAgentStaticMethods
    class function CreateFromNetworkAccountId(networkAccountId: HSTRING): IProvisioningAgent; static; inline;
  end;

  // Windows.Networking.NetworkOperators.UssdMessage
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.IUssdMessage
  // Factory: "Windows.Networking.NetworkOperators.IUssdMessageFactory"
  TUssdMessage = class(TWinRTGenericImportF<IUssdMessageFactory>)
  public
    // -> IUssdMessageFactory
    class function CreateMessage(messageText: HSTRING): IUssdMessage; static; inline;
  end;

  // Windows.Networking.NetworkOperators.UssdSession
  // DualAPI
  // Implements: Windows.Networking.NetworkOperators.IUssdSession
  // Statics: "Windows.Networking.NetworkOperators.IUssdSessionStatics"
  TUssdSession = class(TWinRTGenericImportS<IUssdSessionStatics>)
  public
    // -> IUssdSessionStatics
    class function CreateFromNetworkAccountId(networkAccountId: HSTRING): IUssdSession; static; inline;
    class function CreateFromNetworkInterfaceId(networkInterfaceId: HSTRING): IUssdSession; static; inline;
  end;

implementation

{ TKnownCSimFilePaths }

class function TKnownCSimFilePaths.get_EFSpn: IVectorView_1__Cardinal;
begin
  Result := Statics.get_EFSpn;
end;

class function TKnownCSimFilePaths.get_Gid1: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid1;
end;

class function TKnownCSimFilePaths.get_Gid2: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid2;
end;


{ TKnownRuimFilePaths }

class function TKnownRuimFilePaths.get_EFSpn: IVectorView_1__Cardinal;
begin
  Result := Statics.get_EFSpn;
end;

class function TKnownRuimFilePaths.get_Gid1: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid1;
end;

class function TKnownRuimFilePaths.get_Gid2: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid2;
end;


{ TKnownSimFilePaths }

class function TKnownSimFilePaths.get_EFOns: IVectorView_1__Cardinal;
begin
  Result := Statics.get_EFOns;
end;

class function TKnownSimFilePaths.get_EFSpn: IVectorView_1__Cardinal;
begin
  Result := Statics.get_EFSpn;
end;

class function TKnownSimFilePaths.get_Gid1: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid1;
end;

class function TKnownSimFilePaths.get_Gid2: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid2;
end;


{ TKnownUSimFilePaths }

class function TKnownUSimFilePaths.get_EFSpn: IVectorView_1__Cardinal;
begin
  Result := Statics.get_EFSpn;
end;

class function TKnownUSimFilePaths.get_EFOpl: IVectorView_1__Cardinal;
begin
  Result := Statics.get_EFOpl;
end;

class function TKnownUSimFilePaths.get_EFPnn: IVectorView_1__Cardinal;
begin
  Result := Statics.get_EFPnn;
end;

class function TKnownUSimFilePaths.get_Gid1: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid1;
end;

class function TKnownUSimFilePaths.get_Gid2: IVectorView_1__Cardinal;
begin
  Result := Statics.get_Gid2;
end;


{ TMobileBroadbandAccount }

class function TMobileBroadbandAccount.get_AvailableNetworkAccountIds: IVectorView_1__HSTRING;
begin
  Result := Statics.get_AvailableNetworkAccountIds;
end;

class function TMobileBroadbandAccount.CreateFromNetworkAccountId(networkAccountId: HSTRING): IMobileBroadbandAccount;
begin
  Result := Statics.CreateFromNetworkAccountId(networkAccountId);
end;


{ TMobileBroadbandAccountWatcher }

{ TMobileBroadbandModem }

class function TMobileBroadbandModem.GetDeviceSelector: HSTRING;
begin
  Result := Statics.GetDeviceSelector;
end;

class function TMobileBroadbandModem.FromId(deviceId: HSTRING): IMobileBroadbandModem;
begin
  Result := Statics.FromId(deviceId);
end;

class function TMobileBroadbandModem.GetDefault: IMobileBroadbandModem;
begin
  Result := Statics.GetDefault;
end;


{ TNetworkOperatorTetheringAccessPointConfiguration }

{ TNetworkOperatorTetheringManager }

class function TNetworkOperatorTetheringManager.GetTetheringCapability(networkAccountId: HSTRING): TetheringCapability;
begin
  Result := Statics.GetTetheringCapability(networkAccountId);
end;

class function TNetworkOperatorTetheringManager.CreateFromNetworkAccountId(networkAccountId: HSTRING): INetworkOperatorTetheringManager;
begin
  Result := Statics.CreateFromNetworkAccountId(networkAccountId);
end;


class function TNetworkOperatorTetheringManager.GetTetheringCapabilityFromConnectionProfile(profile: IConnectionProfile): TetheringCapability;
begin
  Result := Statics2.GetTetheringCapabilityFromConnectionProfile(profile);
end;

class function TNetworkOperatorTetheringManager.CreateFromConnectionProfile(profile: IConnectionProfile): INetworkOperatorTetheringManager;
begin
  Result := Statics2.CreateFromConnectionProfile(profile);
end;


class function TNetworkOperatorTetheringManager.CreateFromConnectionProfile(profile: IConnectionProfile; adapter: INetworkAdapter): INetworkOperatorTetheringManager;
begin
  Result := Statics3.CreateFromConnectionProfile(profile, adapter);
end;


class function TNetworkOperatorTetheringManager.IsNoConnectionsTimeoutEnabled: Boolean;
begin
  Result := Statics4.IsNoConnectionsTimeoutEnabled;
end;

class procedure TNetworkOperatorTetheringManager.EnableNoConnectionsTimeout;
begin
  Statics4.EnableNoConnectionsTimeout;
end;

class function TNetworkOperatorTetheringManager.EnableNoConnectionsTimeoutAsync: IAsyncAction;
begin
  Result := Statics4.EnableNoConnectionsTimeoutAsync;
end;

class procedure TNetworkOperatorTetheringManager.DisableNoConnectionsTimeout;
begin
  Statics4.DisableNoConnectionsTimeout;
end;

class function TNetworkOperatorTetheringManager.DisableNoConnectionsTimeoutAsync: IAsyncAction;
begin
  Result := Statics4.DisableNoConnectionsTimeoutAsync;
end;


{ TProvisioningAgent }

class function TProvisioningAgent.CreateFromNetworkAccountId(networkAccountId: HSTRING): IProvisioningAgent;
begin
  Result := Statics.CreateFromNetworkAccountId(networkAccountId);
end;


{ TUssdMessage }
// Factories for : "UssdMessage"
// Factory: "Windows.Networking.NetworkOperators.IUssdMessageFactory"
// -> IUssdMessageFactory

class function TUssdMessage.CreateMessage(messageText: HSTRING): IUssdMessage;
begin
  Result := Factory.CreateMessage(messageText);
end;


{ TUssdSession }

class function TUssdSession.CreateFromNetworkAccountId(networkAccountId: HSTRING): IUssdSession;
begin
  Result := Statics.CreateFromNetworkAccountId(networkAccountId);
end;

class function TUssdSession.CreateFromNetworkInterfaceId(networkInterfaceId: HSTRING): IUssdSession;
begin
  Result := Statics.CreateFromNetworkInterfaceId(networkInterfaceId);
end;


end.
