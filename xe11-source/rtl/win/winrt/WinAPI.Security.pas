{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Security;

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
  Winapi.Security.Credentials, 
  Winapi.WebRT, 
  Winapi.Security.Cryptography, 
  Winapi.Storage.Streams, 
  Winapi.Networking, 
  Winapi.Foundation, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  IVectorView_1__IInspectable = Winapi.CommonTypes.IVectorView_1__IInspectable;
  PIVectorView_1__IInspectable = Winapi.CommonTypes.PIVectorView_1__IInspectable;

  // Forward declarations for interfaces

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderOperation
  Authentication_Web_Provider_IWebAccountProviderOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderOperation = ^Authentication_Web_Provider_IWebAccountProviderOperation;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.ProtectionPolicyEvaluationResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult = ^AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.ProtectionPolicyEvaluationResult>
  IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult = interface;
  PIAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult = ^IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;

  // Windows.Security.Authentication.Web.Core.IWebProviderError
  Authentication_Web_Core_IWebProviderError = interface;
  PAuthentication_Web_Core_IWebProviderError = ^Authentication_Web_Core_IWebProviderError;

  // Windows.Security.Authentication.Web.Core.IWebTokenResponse
  Authentication_Web_Core_IWebTokenResponse = interface;
  PAuthentication_Web_Core_IWebTokenResponse = ^Authentication_Web_Core_IWebTokenResponse;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IIterator_1__Authentication_Web_Core_IWebTokenResponse = interface;
  PIIterator_1__Authentication_Web_Core_IWebTokenResponse = ^IIterator_1__Authentication_Web_Core_IWebTokenResponse;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IIterable_1__Authentication_Web_Core_IWebTokenResponse = interface;
  PIIterable_1__Authentication_Web_Core_IWebTokenResponse = ^IIterable_1__Authentication_Web_Core_IWebTokenResponse;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IVectorView_1__Authentication_Web_Core_IWebTokenResponse = interface;
  PIVectorView_1__Authentication_Web_Core_IWebTokenResponse = ^IVectorView_1__Authentication_Web_Core_IWebTokenResponse;

  // Windows.Security.Authentication.Web.Core.IWebTokenRequestResult
  Authentication_Web_Core_IWebTokenRequestResult = interface;
  PAuthentication_Web_Core_IWebTokenRequestResult = ^Authentication_Web_Core_IWebTokenRequestResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Web.Core.IWebTokenRequestResult>
  AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult = ^AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Web.Core.IWebTokenRequestResult>
  IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult = interface;
  PIAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult = ^IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult;

  // Windows.Security.Authentication.Web.Core.IWebTokenRequest
  Authentication_Web_Core_IWebTokenRequest = interface;
  PAuthentication_Web_Core_IWebTokenRequest = ^Authentication_Web_Core_IWebTokenRequest;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorServiceResponse>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = ^AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorServiceResponse>
  IAsyncOperation_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = interface;
  PIAsyncOperation_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = ^IAsyncOperation_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse;

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface;
  PIIterator_1__HSTRING = ^IIterator_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface;
  PIIterable_1__HSTRING = ^IIterable_1__HSTRING;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface;
  PIVectorView_1__HSTRING = ^IVectorView_1__HSTRING;

  // Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo
  Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface;
  PAuthentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = ^Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IIterator_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface;
  PIIterator_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = ^IIterator_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IIterable_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface;
  PIIterable_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = ^IIterable_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface;
  PIVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = ^IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = ^AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>>
  IAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface;
  PIAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = ^IAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo;

  // Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationManager
  Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager = interface;
  PAuthentication_Identity_IEnterpriseKeyCredentialRegistrationManager = ^Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager;

  // Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationManagerStatics
  Authentication_Identity_IEnterpriseKeyCredentialRegistrationManagerStatics = interface;
  PAuthentication_Identity_IEnterpriseKeyCredentialRegistrationManagerStatics = ^Authentication_Identity_IEnterpriseKeyCredentialRegistrationManagerStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorFinishAuthenticationStatus>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = ^AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorFinishAuthenticationStatus>
  IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = interface;
  PIAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = ^IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthentication
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthentication = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorAuthentication = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthentication;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = ^AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = interface;
  PIAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = ^IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult;

  // Windows.Foundation.EventHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs>
  EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs = interface;
  PEventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs = ^EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = ^AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = interface;
  PIAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = ^IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStatics
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStatics = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStatics = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = ^AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus>
  IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = interface;
  PIAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = ^IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo2
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo2 = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorInfo2 = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo2;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistration
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistration = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorRegistration = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistration;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = ^AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = interface;
  PIAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = ^IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IIterator_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface;
  PIIterator_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = ^IIterator_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IIterable_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface;
  PIIterable_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = ^IIterable_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface;
  PIVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = ^IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = ^AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>>
  IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface;
  PIAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = ^IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationStatics
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationStatics = interface;
  PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationStatics = ^Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  AsyncOperationCompletedHandler_1__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IInspectable = ^AsyncOperationCompletedHandler_1__IInspectable;

  // Windows.Foundation.IAsyncOperation`1<Object>
  IAsyncOperation_1__IInspectable = interface;
  PIAsyncOperation_1__IInspectable = ^IAsyncOperation_1__IInspectable;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Object>
  IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIKeyValuePair_2__HSTRING__IInspectable = ^IKeyValuePair_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IInspectable = ^IIterator_1__IKeyValuePair_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IInspectable = ^IIterable_1__IKeyValuePair_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IMapView`2<String,Object>
  IMapView_2__HSTRING__IInspectable = interface;
  PIMapView_2__HSTRING__IInspectable = ^IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IMap`2<String,Object>
  IMap_2__HSTRING__IInspectable = interface;
  PIMap_2__HSTRING__IInspectable = ^IMap_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  IMapChangedEventArgs_1__HSTRING = interface;
  PIMapChangedEventArgs_1__HSTRING = ^IMapChangedEventArgs_1__HSTRING;

  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable = interface;
  PMapChangedEventHandler_2__HSTRING__IInspectable = ^MapChangedEventHandler_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IObservableMap`2<String,Object>
  IObservableMap_2__HSTRING__IInspectable = interface;
  PIObservableMap_2__HSTRING__IInspectable = ^IObservableMap_2__HSTRING__IInspectable;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationProgressHandler_2__IBuffer__Cardinal = ^AsyncOperationProgressHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface;
  PIAsyncOperationWithProgress_2__IBuffer__Cardinal = ^IAsyncOperationWithProgress_2__IBuffer__Cardinal;

  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationProgressHandler_2__Cardinal__Cardinal = ^AsyncOperationProgressHandler_2__Cardinal__Cardinal;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal;

  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = interface;
  PIAsyncOperationWithProgress_2__Cardinal__Cardinal = ^IAsyncOperationWithProgress_2__Cardinal__Cardinal;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface;
  PAsyncOperationCompletedHandler_1__Boolean = ^AsyncOperationCompletedHandler_1__Boolean;

  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface;
  PIAsyncOperation_1__Boolean = ^IAsyncOperation_1__Boolean;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,String>
  IKeyValuePair_2__HSTRING__HSTRING = interface;
  PIKeyValuePair_2__HSTRING__HSTRING = ^IKeyValuePair_2__HSTRING__HSTRING;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterator_1__IKeyValuePair_2__HSTRING__HSTRING = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__HSTRING = ^IIterator_1__IKeyValuePair_2__HSTRING__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterable_1__IKeyValuePair_2__HSTRING__HSTRING = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__HSTRING = ^IIterable_1__IKeyValuePair_2__HSTRING__HSTRING;

  // Windows.Foundation.Collections.IMapView`2<String,String>
  IMapView_2__HSTRING__HSTRING = interface;
  PIMapView_2__HSTRING__HSTRING = ^IMapView_2__HSTRING__HSTRING;

  // Windows.Foundation.Collections.IMap`2<String,String>
  IMap_2__HSTRING__HSTRING = interface;
  PIMap_2__HSTRING__HSTRING = ^IMap_2__HSTRING__HSTRING;

  // Windows.Security.Authentication.Web.Core.IFindAllAccountsResult
  Authentication_Web_Core_IFindAllAccountsResult = interface;
  PAuthentication_Web_Core_IFindAllAccountsResult = ^Authentication_Web_Core_IFindAllAccountsResult;

  // Windows.Security.Authentication.Web.Core.IWebAccountEventArgs
  Authentication_Web_Core_IWebAccountEventArgs = interface;
  PAuthentication_Web_Core_IWebAccountEventArgs = ^Authentication_Web_Core_IWebAccountEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Security.Authentication.Web.Core.IWebAccountMonitor,Windows.Security.Authentication.Web.Core.IWebAccountEventArgs>
  TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs = interface;
  PTypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs = ^TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Security.Authentication.Web.Core.IWebAccountMonitor,Object>
  TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__IInspectable = interface;
  PTypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__IInspectable = ^TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__IInspectable;

  // Windows.Security.Authentication.Web.Core.IWebAccountMonitor
  Authentication_Web_Core_IWebAccountMonitor = interface;
  PAuthentication_Web_Core_IWebAccountMonitor = ^Authentication_Web_Core_IWebAccountMonitor;

  // Windows.Security.Authentication.Web.Core.IWebAccountMonitor2
  Authentication_Web_Core_IWebAccountMonitor2 = interface;
  PAuthentication_Web_Core_IWebAccountMonitor2 = ^Authentication_Web_Core_IWebAccountMonitor2;

  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics = interface;
  PAuthentication_Web_Core_IWebAuthenticationCoreManagerStatics = ^Authentication_Web_Core_IWebAuthenticationCoreManagerStatics;

  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics2
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics2 = interface;
  PAuthentication_Web_Core_IWebAuthenticationCoreManagerStatics2 = ^Authentication_Web_Core_IWebAuthenticationCoreManagerStatics2;

  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics3
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics3 = interface;
  PAuthentication_Web_Core_IWebAuthenticationCoreManagerStatics3 = ^Authentication_Web_Core_IWebAuthenticationCoreManagerStatics3;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Web.Core.IFindAllAccountsResult>
  AsyncOperationCompletedHandler_1__Authentication_Web_Core_IFindAllAccountsResult = interface;
  PAsyncOperationCompletedHandler_1__Authentication_Web_Core_IFindAllAccountsResult = ^AsyncOperationCompletedHandler_1__Authentication_Web_Core_IFindAllAccountsResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Web.Core.IFindAllAccountsResult>
  IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult = interface;
  PIAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult = ^IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult;

  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics4
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics4 = interface;
  PAuthentication_Web_Core_IWebAuthenticationCoreManagerStatics4 = ^Authentication_Web_Core_IWebAuthenticationCoreManagerStatics4;

  // Windows.Security.Authentication.Web.Core.IWebProviderErrorFactory
  Authentication_Web_Core_IWebProviderErrorFactory = interface;
  PAuthentication_Web_Core_IWebProviderErrorFactory = ^Authentication_Web_Core_IWebProviderErrorFactory;

  // Windows.Security.Authentication.Web.Core.IWebTokenRequest2
  Authentication_Web_Core_IWebTokenRequest2 = interface;
  PAuthentication_Web_Core_IWebTokenRequest2 = ^Authentication_Web_Core_IWebTokenRequest2;

  // Windows.Security.Authentication.Web.Core.IWebTokenRequest3
  Authentication_Web_Core_IWebTokenRequest3 = interface;
  PAuthentication_Web_Core_IWebTokenRequest3 = ^Authentication_Web_Core_IWebTokenRequest3;

  // Windows.Security.Authentication.Web.Core.IWebTokenRequestFactory
  Authentication_Web_Core_IWebTokenRequestFactory = interface;
  PAuthentication_Web_Core_IWebTokenRequestFactory = ^Authentication_Web_Core_IWebTokenRequestFactory;

  // Windows.Security.Authentication.Web.Core.IWebTokenResponseFactory
  Authentication_Web_Core_IWebTokenResponseFactory = interface;
  PAuthentication_Web_Core_IWebTokenResponseFactory = ^Authentication_Web_Core_IWebTokenResponseFactory;

  // Windows.Security.Authentication.Web.Provider.IWebAccountClientView
  Authentication_Web_Provider_IWebAccountClientView = interface;
  PAuthentication_Web_Provider_IWebAccountClientView = ^Authentication_Web_Provider_IWebAccountClientView;

  // Windows.Security.Authentication.Web.Provider.IWebAccountClientViewFactory
  Authentication_Web_Provider_IWebAccountClientViewFactory = interface;
  PAuthentication_Web_Provider_IWebAccountClientViewFactory = ^Authentication_Web_Provider_IWebAccountClientViewFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IIterator_1__Authentication_Web_Provider_IWebAccountClientView = interface;
  PIIterator_1__Authentication_Web_Provider_IWebAccountClientView = ^IIterator_1__Authentication_Web_Provider_IWebAccountClientView;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IIterable_1__Authentication_Web_Provider_IWebAccountClientView = interface;
  PIIterable_1__Authentication_Web_Provider_IWebAccountClientView = ^IIterable_1__Authentication_Web_Provider_IWebAccountClientView;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = interface;
  PIVectorView_1__Authentication_Web_Provider_IWebAccountClientView = ^IVectorView_1__Authentication_Web_Provider_IWebAccountClientView;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = ^AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>>
  IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = interface;
  PIAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = ^IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView;

  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics
  Authentication_Web_Provider_IWebAccountManagerStatics = interface;
  PAuthentication_Web_Provider_IWebAccountManagerStatics = ^Authentication_Web_Provider_IWebAccountManagerStatics;

  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics2
  Authentication_Web_Provider_IWebAccountManagerStatics2 = interface;
  PAuthentication_Web_Provider_IWebAccountManagerStatics2 = ^Authentication_Web_Provider_IWebAccountManagerStatics2;

  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics3
  Authentication_Web_Provider_IWebAccountManagerStatics3 = interface;
  PAuthentication_Web_Provider_IWebAccountManagerStatics3 = ^Authentication_Web_Provider_IWebAccountManagerStatics3;

  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics4
  Authentication_Web_Provider_IWebAccountManagerStatics4 = interface;
  PAuthentication_Web_Provider_IWebAccountManagerStatics4 = ^Authentication_Web_Provider_IWebAccountManagerStatics4;

  // Windows.Security.Authentication.Web.Provider.IWebAccountMapManagerStatics
  Authentication_Web_Provider_IWebAccountMapManagerStatics = interface;
  PAuthentication_Web_Provider_IWebAccountMapManagerStatics = ^Authentication_Web_Provider_IWebAccountMapManagerStatics;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderAddAccountOperation
  Authentication_Web_Provider_IWebAccountProviderAddAccountOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderAddAccountOperation = ^Authentication_Web_Provider_IWebAccountProviderAddAccountOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderBaseReportOperation
  Authentication_Web_Provider_IWebAccountProviderBaseReportOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderBaseReportOperation = ^Authentication_Web_Provider_IWebAccountProviderBaseReportOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderDeleteAccountOperation
  Authentication_Web_Provider_IWebAccountProviderDeleteAccountOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderDeleteAccountOperation = ^Authentication_Web_Provider_IWebAccountProviderDeleteAccountOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderManageAccountOperation
  Authentication_Web_Provider_IWebAccountProviderManageAccountOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderManageAccountOperation = ^Authentication_Web_Provider_IWebAccountProviderManageAccountOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderRetrieveCookiesOperation
  Authentication_Web_Provider_IWebAccountProviderRetrieveCookiesOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderRetrieveCookiesOperation = ^Authentication_Web_Provider_IWebAccountProviderRetrieveCookiesOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderSignOutAccountOperation
  Authentication_Web_Provider_IWebAccountProviderSignOutAccountOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderSignOutAccountOperation = ^Authentication_Web_Provider_IWebAccountProviderSignOutAccountOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderSilentReportOperation
  Authentication_Web_Provider_IWebAccountProviderSilentReportOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderSilentReportOperation = ^Authentication_Web_Provider_IWebAccountProviderSilentReportOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderTokenObjects
  Authentication_Web_Provider_IWebAccountProviderTokenObjects = interface;
  PAuthentication_Web_Provider_IWebAccountProviderTokenObjects = ^Authentication_Web_Provider_IWebAccountProviderTokenObjects;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderTokenObjects2
  Authentication_Web_Provider_IWebAccountProviderTokenObjects2 = interface;
  PAuthentication_Web_Provider_IWebAccountProviderTokenObjects2 = ^Authentication_Web_Provider_IWebAccountProviderTokenObjects2;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenRequest
  Authentication_Web_Provider_IWebProviderTokenRequest = interface;
  PAuthentication_Web_Provider_IWebProviderTokenRequest = ^Authentication_Web_Provider_IWebProviderTokenRequest;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse
  Authentication_Web_Provider_IWebProviderTokenResponse = interface;
  PAuthentication_Web_Provider_IWebProviderTokenResponse = ^Authentication_Web_Provider_IWebProviderTokenResponse;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IIterator_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface;
  PIIterator_1__Authentication_Web_Provider_IWebProviderTokenResponse = ^IIterator_1__Authentication_Web_Provider_IWebProviderTokenResponse;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IIterable_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface;
  PIIterable_1__Authentication_Web_Provider_IWebProviderTokenResponse = ^IIterable_1__Authentication_Web_Provider_IWebProviderTokenResponse;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IVectorView_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface;
  PIVectorView_1__Authentication_Web_Provider_IWebProviderTokenResponse = ^IVectorView_1__Authentication_Web_Provider_IWebProviderTokenResponse;

  // Windows.Foundation.Collections.IVector`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IVector_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface;
  PIVector_1__Authentication_Web_Provider_IWebProviderTokenResponse = ^IVector_1__Authentication_Web_Provider_IWebProviderTokenResponse;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderTokenOperation
  Authentication_Web_Provider_IWebAccountProviderTokenOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderTokenOperation = ^Authentication_Web_Provider_IWebAccountProviderTokenOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderUIReportOperation
  Authentication_Web_Provider_IWebAccountProviderUIReportOperation = interface;
  PAuthentication_Web_Provider_IWebAccountProviderUIReportOperation = ^Authentication_Web_Provider_IWebAccountProviderUIReportOperation;

  // Windows.Security.Authentication.Web.Provider.IWebAccountScopeManagerStatics
  Authentication_Web_Provider_IWebAccountScopeManagerStatics = interface;
  PAuthentication_Web_Provider_IWebAccountScopeManagerStatics = ^Authentication_Web_Provider_IWebAccountScopeManagerStatics;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenRequest2
  Authentication_Web_Provider_IWebProviderTokenRequest2 = interface;
  PAuthentication_Web_Provider_IWebProviderTokenRequest2 = ^Authentication_Web_Provider_IWebProviderTokenRequest2;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenRequest3
  Authentication_Web_Provider_IWebProviderTokenRequest3 = interface;
  PAuthentication_Web_Provider_IWebProviderTokenRequest3 = ^Authentication_Web_Provider_IWebProviderTokenRequest3;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponseFactory
  Authentication_Web_Provider_IWebProviderTokenResponseFactory = interface;
  PAuthentication_Web_Provider_IWebProviderTokenResponseFactory = ^Authentication_Web_Provider_IWebProviderTokenResponseFactory;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  AsyncOperationCompletedHandler_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^AsyncOperationCompletedHandler_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  IAsyncOperation_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PIAsyncOperation_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^IAsyncOperation_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PIKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  IIterator_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^IIterator_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  IIterable_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^IIterable_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PIMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  IAsyncOperation_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface;
  PIAsyncOperation_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^IAsyncOperation_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Foundation.IReference`1<UInt32>
  IReference_1__Cardinal = interface;
  PIReference_1__Cardinal = ^IReference_1__Cardinal;

  // Windows.Foundation.IReference`1<UInt64>
  IReference_1__UInt64 = interface;
  PIReference_1__UInt64 = ^IReference_1__UInt64;

  // Windows.Foundation.IReference`1<UInt8>
  IReference_1__Byte = interface;
  PIReference_1__Byte = ^IReference_1__Byte;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  AsyncOperationCompletedHandler_1__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__HSTRING = ^AsyncOperationCompletedHandler_1__HSTRING;

  // Windows.Foundation.IAsyncOperation`1<String>
  IAsyncOperation_1__HSTRING = interface;
  PIAsyncOperation_1__HSTRING = ^IAsyncOperation_1__HSTRING;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.DataProtection.UserDataStorageItemProtectionStatus>
  AsyncOperationCompletedHandler_1__DataProtection_UserDataStorageItemProtectionStatus = interface;
  PAsyncOperationCompletedHandler_1__DataProtection_UserDataStorageItemProtectionStatus = ^AsyncOperationCompletedHandler_1__DataProtection_UserDataStorageItemProtectionStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.DataProtection.UserDataStorageItemProtectionStatus>
  IAsyncOperation_1__DataProtection_UserDataStorageItemProtectionStatus = interface;
  PIAsyncOperation_1__DataProtection_UserDataStorageItemProtectionStatus = ^IAsyncOperation_1__DataProtection_UserDataStorageItemProtectionStatus;

  // Windows.Security.EnterpriseData.IDataProtectionInfo
  EnterpriseData_IDataProtectionInfo = interface;
  PEnterpriseData_IDataProtectionInfo = ^EnterpriseData_IDataProtectionInfo;

  // Windows.Security.EnterpriseData.IBufferProtectUnprotectResult
  EnterpriseData_IBufferProtectUnprotectResult = interface;
  PEnterpriseData_IBufferProtectUnprotectResult = ^EnterpriseData_IBufferProtectUnprotectResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IBufferProtectUnprotectResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult = ^AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IBufferProtectUnprotectResult>
  IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult = interface;
  PIAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult = ^IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IDataProtectionInfo>
  AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo = ^AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IDataProtectionInfo>
  IAsyncOperation_1__EnterpriseData_IDataProtectionInfo = interface;
  PIAsyncOperation_1__EnterpriseData_IDataProtectionInfo = ^IAsyncOperation_1__EnterpriseData_IDataProtectionInfo;

  // Windows.Security.EnterpriseData.IDataProtectionManagerStatics
  EnterpriseData_IDataProtectionManagerStatics = interface;
  PEnterpriseData_IDataProtectionManagerStatics = ^EnterpriseData_IDataProtectionManagerStatics;

  // Windows.Security.EnterpriseData.IFileProtectionInfo
  EnterpriseData_IFileProtectionInfo = interface;
  PEnterpriseData_IFileProtectionInfo = ^EnterpriseData_IFileProtectionInfo;

  // Windows.Security.EnterpriseData.IFileProtectionInfo2
  EnterpriseData_IFileProtectionInfo2 = interface;
  PEnterpriseData_IFileProtectionInfo2 = ^EnterpriseData_IFileProtectionInfo2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IFileProtectionInfo>
  AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo = ^AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IFileProtectionInfo>
  IAsyncOperation_1__EnterpriseData_IFileProtectionInfo = interface;
  PIAsyncOperation_1__EnterpriseData_IFileProtectionInfo = ^IAsyncOperation_1__EnterpriseData_IFileProtectionInfo;

  // Windows.Security.EnterpriseData.IProtectedContainerExportResult
  EnterpriseData_IProtectedContainerExportResult = interface;
  PEnterpriseData_IProtectedContainerExportResult = ^EnterpriseData_IProtectedContainerExportResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedContainerExportResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult = ^AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedContainerExportResult>
  IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult = interface;
  PIAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult = ^IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult;

  // Windows.Security.EnterpriseData.IProtectedContainerImportResult
  EnterpriseData_IProtectedContainerImportResult = interface;
  PEnterpriseData_IProtectedContainerImportResult = ^EnterpriseData_IProtectedContainerImportResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedContainerImportResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult = ^AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedContainerImportResult>
  IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult = interface;
  PIAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult = ^IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult;

  // Windows.Security.EnterpriseData.IProtectedFileCreateResult
  EnterpriseData_IProtectedFileCreateResult = interface;
  PEnterpriseData_IProtectedFileCreateResult = ^EnterpriseData_IProtectedFileCreateResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedFileCreateResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult = ^AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedFileCreateResult>
  IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult = interface;
  PIAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult = ^IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult;

  // Windows.Security.EnterpriseData.IFileProtectionManagerStatics
  EnterpriseData_IFileProtectionManagerStatics = interface;
  PEnterpriseData_IFileProtectionManagerStatics = ^EnterpriseData_IFileProtectionManagerStatics;

  // Windows.Security.EnterpriseData.IFileProtectionManagerStatics2
  EnterpriseData_IFileProtectionManagerStatics2 = interface;
  PEnterpriseData_IFileProtectionManagerStatics2 = ^EnterpriseData_IFileProtectionManagerStatics2;

  // Windows.Security.EnterpriseData.IFileUnprotectOptions
  EnterpriseData_IFileUnprotectOptions = interface;
  PEnterpriseData_IFileUnprotectOptions = ^EnterpriseData_IFileUnprotectOptions;

  // Windows.Security.EnterpriseData.IFileProtectionManagerStatics3
  EnterpriseData_IFileProtectionManagerStatics3 = interface;
  PEnterpriseData_IFileProtectionManagerStatics3 = ^EnterpriseData_IFileProtectionManagerStatics3;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.FileProtectionStatus>
  AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus = interface;
  PAsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus = ^AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.FileProtectionStatus>
  IAsyncOperation_1__EnterpriseData_FileProtectionStatus = interface;
  PIAsyncOperation_1__EnterpriseData_FileProtectionStatus = ^IAsyncOperation_1__EnterpriseData_FileProtectionStatus;

  // Windows.Security.EnterpriseData.IFileRevocationManagerStatics
  EnterpriseData_IFileRevocationManagerStatics = interface;
  PEnterpriseData_IFileRevocationManagerStatics = ^EnterpriseData_IFileRevocationManagerStatics;

  // Windows.Security.EnterpriseData.IFileUnprotectOptionsFactory
  EnterpriseData_IFileUnprotectOptionsFactory = interface;
  PEnterpriseData_IFileUnprotectOptionsFactory = ^EnterpriseData_IFileUnprotectOptionsFactory;

  // Windows.Security.EnterpriseData.IProtectedAccessResumedEventArgs
  EnterpriseData_IProtectedAccessResumedEventArgs = interface;
  PEnterpriseData_IProtectedAccessResumedEventArgs = ^EnterpriseData_IProtectedAccessResumedEventArgs;

  // Windows.Security.EnterpriseData.IProtectedAccessSuspendingEventArgs
  EnterpriseData_IProtectedAccessSuspendingEventArgs = interface;
  PEnterpriseData_IProtectedAccessSuspendingEventArgs = ^EnterpriseData_IProtectedAccessSuspendingEventArgs;

  // Windows.Security.EnterpriseData.IProtectedContentRevokedEventArgs
  EnterpriseData_IProtectedContentRevokedEventArgs = interface;
  PEnterpriseData_IProtectedContentRevokedEventArgs = ^EnterpriseData_IProtectedContentRevokedEventArgs;

  // Windows.Security.EnterpriseData.IProtectionPolicyAuditInfo
  EnterpriseData_IProtectionPolicyAuditInfo = interface;
  PEnterpriseData_IProtectionPolicyAuditInfo = ^EnterpriseData_IProtectionPolicyAuditInfo;

  // Windows.Security.EnterpriseData.IProtectionPolicyAuditInfoFactory
  EnterpriseData_IProtectionPolicyAuditInfoFactory = interface;
  PEnterpriseData_IProtectionPolicyAuditInfoFactory = ^EnterpriseData_IProtectionPolicyAuditInfoFactory;

  // Windows.Security.EnterpriseData.IProtectionPolicyManager
  EnterpriseData_IProtectionPolicyManager = interface;
  PEnterpriseData_IProtectionPolicyManager = ^EnterpriseData_IProtectionPolicyManager;

  // Windows.Security.EnterpriseData.IProtectionPolicyManager2
  EnterpriseData_IProtectionPolicyManager2 = interface;
  PEnterpriseData_IProtectionPolicyManager2 = ^EnterpriseData_IProtectionPolicyManager2;

  // Windows.Security.EnterpriseData.IThreadNetworkContext
  EnterpriseData_IThreadNetworkContext = interface;
  PEnterpriseData_IThreadNetworkContext = ^EnterpriseData_IThreadNetworkContext;

  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedAccessSuspendingEventArgs>
  EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs = interface;
  PEventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs = ^EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedAccessResumedEventArgs>
  EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs = interface;
  PEventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs = ^EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedContentRevokedEventArgs>
  EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs = interface;
  PEventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs = ^EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs;

  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics
  EnterpriseData_IProtectionPolicyManagerStatics = interface;
  PEnterpriseData_IProtectionPolicyManagerStatics = ^EnterpriseData_IProtectionPolicyManagerStatics;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface;
  PEventHandler_1__IInspectable = ^EventHandler_1__IInspectable;

  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics2
  EnterpriseData_IProtectionPolicyManagerStatics2 = interface;
  PEnterpriseData_IProtectionPolicyManagerStatics2 = ^EnterpriseData_IProtectionPolicyManagerStatics2;

  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics3
  EnterpriseData_IProtectionPolicyManagerStatics3 = interface;
  PEnterpriseData_IProtectionPolicyManagerStatics3 = ^EnterpriseData_IProtectionPolicyManagerStatics3;

  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics4
  EnterpriseData_IProtectionPolicyManagerStatics4 = interface;
  PEnterpriseData_IProtectionPolicyManagerStatics4 = ^EnterpriseData_IProtectionPolicyManagerStatics4;

  // Windows.Foundation.Collections.IIterator`1<Object>
  IIterator_1__IInspectable = interface;
  PIIterator_1__IInspectable = ^IIterator_1__IInspectable;

  // Windows.Foundation.Collections.IIterable`1<Object>
  IIterable_1__IInspectable = interface;
  PIIterable_1__IInspectable = ^IIterable_1__IInspectable;

  // Windows.Security.Isolation.HostMessageReceivedCallback
  Isolation_HostMessageReceivedCallback = interface;
  PIsolation_HostMessageReceivedCallback = ^Isolation_HostMessageReceivedCallback;

  // Windows.Security.Isolation.MessageReceivedCallback
  Isolation_MessageReceivedCallback = interface;
  PIsolation_MessageReceivedCallback = ^Isolation_MessageReceivedCallback;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Isolation.IsolatedWindowsEnvironmentHostError>
  IIterator_1__Isolation_IsolatedWindowsEnvironmentHostError = interface;
  PIIterator_1__Isolation_IsolatedWindowsEnvironmentHostError = ^IIterator_1__Isolation_IsolatedWindowsEnvironmentHostError;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Isolation.IsolatedWindowsEnvironmentHostError>
  IIterable_1__Isolation_IsolatedWindowsEnvironmentHostError = interface;
  PIIterable_1__Isolation_IsolatedWindowsEnvironmentHostError = ^IIterable_1__Isolation_IsolatedWindowsEnvironmentHostError;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Isolation.IsolatedWindowsEnvironmentHostError>
  IVectorView_1__Isolation_IsolatedWindowsEnvironmentHostError = interface;
  PIVectorView_1__Isolation_IsolatedWindowsEnvironmentHostError = ^IVectorView_1__Isolation_IsolatedWindowsEnvironmentHostError;

  // Windows.Security Enums

  // Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorAuthenticationType
  Authentication_Identity_Core_MicrosoftAccountMultiFactorAuthenticationType = (
    User = 0,
    Device = 1
  );
  PAuthentication_Identity_Core_MicrosoftAccountMultiFactorAuthenticationType = ^Authentication_Identity_Core_MicrosoftAccountMultiFactorAuthenticationType;

  // Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorServiceResponse
  Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = (
    Success = 0,
    Error = 1,
    NoNetworkConnection = 2,
    ServiceUnavailable = 3,
    TotpSetupDenied = 4,
    NgcNotSetup = 5,
    SessionAlreadyDenied = 6,
    SessionAlreadyApproved = 7,
    SessionExpired = 8,
    NgcNonceExpired = 9,
    InvalidSessionId = 10,
    InvalidSessionType = 11,
    InvalidOperation = 12,
    InvalidStateTransition = 13,
    DeviceNotFound = 14,
    FlowDisabled = 15,
    SessionNotApproved = 16,
    OperationCanceledByUser = 17,
    NgcDisabledByServer = 18,
    NgcKeyNotFoundOnServer = 19,
    UIRequired = 20,
    DeviceIdChanged = 21
  );
  PAuthentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = ^Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse;

  // Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorSessionApprovalStatus
  Authentication_Identity_Core_MicrosoftAccountMultiFactorSessionApprovalStatus = (
    Pending = 0,
    Approved = 1,
    Denied = 2
  );
  PAuthentication_Identity_Core_MicrosoftAccountMultiFactorSessionApprovalStatus = ^Authentication_Identity_Core_MicrosoftAccountMultiFactorSessionApprovalStatus;

  // Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorSessionAuthenticationStatus
  Authentication_Identity_Core_MicrosoftAccountMultiFactorSessionAuthenticationStatus = (
    Authenticated = 0,
    Unauthenticated = 1
  );
  PAuthentication_Identity_Core_MicrosoftAccountMultiFactorSessionAuthenticationStatus = ^Authentication_Identity_Core_MicrosoftAccountMultiFactorSessionAuthenticationStatus;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorAuthenticationMessage
  Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationMessage = (
    Invalid = 0,
    SwipeUpWelcome = 1,
    TapWelcome = 2,
    DeviceNeedsAttention = 3,
    LookingForDevice = 4,
    LookingForDevicePluggedin = 5,
    BluetoothIsDisabled = 6,
    NfcIsDisabled = 7,
    WiFiIsDisabled = 8,
    ExtraTapIsRequired = 9,
    DisabledByPolicy = 10,
    TapOnDeviceRequired = 11,
    HoldFinger = 12,
    ScanFinger = 13,
    UnauthorizedUser = 14,
    ReregisterRequired = 15,
    TryAgain = 16,
    SayPassphrase = 17,
    ReadyToSignIn = 18,
    UseAnotherSignInOption = 19,
    ConnectionRequired = 20,
    TimeLimitExceeded = 21,
    CanceledByUser = 22,
    CenterHand = 23,
    MoveHandCloser = 24,
    MoveHandFarther = 25,
    PlaceHandAbove = 26,
    RecognitionFailed = 27,
    DeviceUnavailable = 28
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationMessage = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationMessage;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorAuthenticationScenario
  Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationScenario = (
    SignIn = 0,
    CredentialPrompt = 1
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationScenario = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationScenario;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorAuthenticationStage
  Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStage = (
    NotStarted = 0,
    WaitingForUserConfirmation = 1,
    CollectingCredential = 2,
    SuspendingAuthentication = 3,
    CredentialCollected = 4,
    CredentialAuthenticated = 5,
    StoppingAuthentication = 6,
    ReadyForLock = 7,
    CheckingDevicePresence = 8
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStage = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStage;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorAuthenticationStatus
  Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStatus = (
    Failed = 0,
    Started = 1,
    UnknownDevice = 2,
    DisabledByPolicy = 3,
    InvalidAuthenticationStage = 4
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStatus = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStatus;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDeviceCapabilities
  Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceCapabilities = (
    None = 0,
    SecureStorage = 1,
    StoreKeys = 2,
    ConfirmUserIntentToAuthenticate = 4,
    SupportSecureUserPresenceCheck = 8,
    TransmittedDataIsEncrypted = 16,
    HMacSha256 = 32,
    CloseRangeDataTransmission = 64
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorDeviceCapabilities = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceCapabilities;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDeviceFindScope
  Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceFindScope = (
    User = 0,
    AllUsers = 1
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorDeviceFindScope = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceFindScope;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresence
  Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresence = (
    Absent = 0,
    Present = 1
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresence = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresence;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringMode
  Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode = (
    Unsupported = 0,
    AppManaged = 1,
    SystemManaged = 2
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus
  Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = (
    Unsupported = 0,
    Succeeded = 1,
    DisabledByPolicy = 2
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorFinishAuthenticationStatus
  Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = (
    Failed = 0,
    Completed = 1,
    NonceExpired = 2
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorRegistrationStatus
  Authentication_Identity_Provider_SecondaryAuthenticationFactorRegistrationStatus = (
    Failed = 0,
    Started = 1,
    CanceledByUser = 2,
    PinSetupRequired = 3,
    DisabledByPolicy = 4
  );
  PAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistrationStatus = ^Authentication_Identity_Provider_SecondaryAuthenticationFactorRegistrationStatus;

  // Windows.Security.Authentication.OnlineId.CredentialPromptType
  Authentication_OnlineId_CredentialPromptType = (
    PromptIfNeeded = 0,
    RetypeCredentials = 1,
    DoNotPrompt = 2
  );
  PAuthentication_OnlineId_CredentialPromptType = ^Authentication_OnlineId_CredentialPromptType;

  // Windows.Security.Authentication.OnlineId.OnlineIdSystemTicketStatus
  Authentication_OnlineId_OnlineIdSystemTicketStatus = (
    Success = 0,
    Error = 1,
    ServiceConnectionError = 2
  );
  PAuthentication_OnlineId_OnlineIdSystemTicketStatus = ^Authentication_OnlineId_OnlineIdSystemTicketStatus;

  // Windows.Security.Authentication.Web.Core.FindAllWebAccountsStatus
  Authentication_Web_Core_FindAllWebAccountsStatus = (
    Success = 0,
    NotAllowedByProvider = 1,
    NotSupportedByProvider = 2,
    ProviderError = 3
  );
  PAuthentication_Web_Core_FindAllWebAccountsStatus = ^Authentication_Web_Core_FindAllWebAccountsStatus;

  // Windows.Security.Authentication.Web.Core.WebTokenRequestPromptType
  Authentication_Web_Core_WebTokenRequestPromptType = (
    Default = 0,
    ForceAuthentication = 1
  );
  PAuthentication_Web_Core_WebTokenRequestPromptType = ^Authentication_Web_Core_WebTokenRequestPromptType;

  // Windows.Security.Authentication.Web.Core.WebTokenRequestStatus
  Authentication_Web_Core_WebTokenRequestStatus = (
    Success = 0,
    UserCancel = 1,
    AccountSwitch = 2,
    UserInteractionRequired = 3,
    AccountProviderNotAvailable = 4,
    ProviderError = 5
  );
  PAuthentication_Web_Core_WebTokenRequestStatus = ^Authentication_Web_Core_WebTokenRequestStatus;

  // Windows.Security.Authentication.Web.Provider.WebAccountClientViewType
  Authentication_Web_Provider_WebAccountClientViewType = (
    IdOnly = 0,
    IdAndProperties = 1
  );
  PAuthentication_Web_Provider_WebAccountClientViewType = ^Authentication_Web_Provider_WebAccountClientViewType;

  // Windows.Security.Authentication.Web.Provider.WebAccountProviderOperationKind
  Authentication_Web_Provider_WebAccountProviderOperationKind = (
    RequestToken = 0,
    GetTokenSilently = 1,
    AddAccount = 2,
    ManageAccount = 3,
    DeleteAccount = 4,
    RetrieveCookies = 5,
    SignOutAccount = 6
  );
  PAuthentication_Web_Provider_WebAccountProviderOperationKind = ^Authentication_Web_Provider_WebAccountProviderOperationKind;

  // Windows.Security.Authentication.Web.Provider.WebAccountScope
  Authentication_Web_Provider_WebAccountScope = (
    PerUser = 0,
    PerApplication = 1
  );
  PAuthentication_Web_Provider_WebAccountScope = ^Authentication_Web_Provider_WebAccountScope;

  // Windows.Security.Authentication.Web.Provider.WebAccountSelectionOptions
  Authentication_Web_Provider_WebAccountSelectionOptions = (
    Default = 0,
    New = 1
  );
  PAuthentication_Web_Provider_WebAccountSelectionOptions = ^Authentication_Web_Provider_WebAccountSelectionOptions;

  // Windows.Security.Authentication.Web.TokenBindingKeyType
  Authentication_Web_TokenBindingKeyType = (
    Rsa2048 = 0,
    EcdsaP256 = 1,
    AnyExisting = 2
  );
  PAuthentication_Web_TokenBindingKeyType = ^Authentication_Web_TokenBindingKeyType;

  // Windows.Security.Authentication.Web.WebAuthenticationOptions
  Authentication_Web_WebAuthenticationOptions = (
    None = 0,
    SilentMode = 1,
    UseTitle = 2,
    UseHttpPost = 4,
    UseCorporateNetwork = 8
  );
  PAuthentication_Web_WebAuthenticationOptions = ^Authentication_Web_WebAuthenticationOptions;

  // Windows.Security.Authentication.Web.WebAuthenticationStatus
  Authentication_Web_WebAuthenticationStatus = (
    Success = 0,
    UserCancel = 1,
    ErrorHttp = 2
  );
  PAuthentication_Web_WebAuthenticationStatus = ^Authentication_Web_WebAuthenticationStatus;

  // Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus
  Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = (
    DeniedBySystem = 0,
    NotDeclaredByApp = 1,
    DeniedByUser = 2,
    UserPromptRequired = 3,
    Allowed = 4
  );
  PAuthorization_AppCapabilityAccess_AppCapabilityAccessStatus = ^Authorization_AppCapabilityAccess_AppCapabilityAccessStatus;

  // Windows.Security.DataProtection.UserDataAvailability
  DataProtection_UserDataAvailability = (
    Always = 0,
    AfterFirstUnlock = 1,
    WhileUnlocked = 2
  );
  PDataProtection_UserDataAvailability = ^DataProtection_UserDataAvailability;

  // Windows.Security.DataProtection.UserDataBufferUnprotectStatus
  DataProtection_UserDataBufferUnprotectStatus = (
    Succeeded = 0,
    Unavailable = 1
  );
  PDataProtection_UserDataBufferUnprotectStatus = ^DataProtection_UserDataBufferUnprotectStatus;

  // Windows.Security.DataProtection.UserDataStorageItemProtectionStatus
  DataProtection_UserDataStorageItemProtectionStatus = (
    Succeeded = 0,
    NotProtectable = 1,
    DataUnavailable = 2
  );
  PDataProtection_UserDataStorageItemProtectionStatus = ^DataProtection_UserDataStorageItemProtectionStatus;

  // Windows.Security.EnterpriseData.DataProtectionStatus
  EnterpriseData_DataProtectionStatus = (
    ProtectedToOtherIdentity = 0,
    &Protected = 1,
    Revoked = 2,
    Unprotected = 3,
    LicenseExpired = 4,
    AccessSuspended = 5
  );
  PEnterpriseData_DataProtectionStatus = ^EnterpriseData_DataProtectionStatus;

  // Windows.Security.EnterpriseData.EnforcementLevel
  EnterpriseData_EnforcementLevel = (
    NoProtection = 0,
    Silent = 1,
    &Override = 2,
    Block = 3
  );
  PEnterpriseData_EnforcementLevel = ^EnterpriseData_EnforcementLevel;

  // Windows.Security.EnterpriseData.FileProtectionStatus
  EnterpriseData_FileProtectionStatus = (
    Undetermined = 0,
    Unknown = 0,
    Unprotected = 1,
    Revoked = 2,
    &Protected = 3,
    ProtectedByOtherUser = 4,
    ProtectedToOtherEnterprise = 5,
    NotProtectable = 6,
    ProtectedToOtherIdentity = 7,
    LicenseExpired = 8,
    AccessSuspended = 9,
    FileInUse = 10
  );
  PEnterpriseData_FileProtectionStatus = ^EnterpriseData_FileProtectionStatus;

  // Windows.Security.EnterpriseData.ProtectedImportExportStatus
  EnterpriseData_ProtectedImportExportStatus = (
    Ok = 0,
    Undetermined = 1,
    Unprotected = 2,
    Revoked = 3,
    NotRoamable = 4,
    ProtectedToOtherIdentity = 5,
    LicenseExpired = 6,
    AccessSuspended = 7
  );
  PEnterpriseData_ProtectedImportExportStatus = ^EnterpriseData_ProtectedImportExportStatus;

  // Windows.Security.EnterpriseData.ProtectionPolicyAuditAction
  EnterpriseData_ProtectionPolicyAuditAction = (
    Decrypt = 0,
    CopyToLocation = 1,
    SendToRecipient = 2,
    Other = 3
  );
  PEnterpriseData_ProtectionPolicyAuditAction = ^EnterpriseData_ProtectionPolicyAuditAction;

  // Windows.Security.EnterpriseData.ProtectionPolicyEvaluationResult
  EnterpriseData_ProtectionPolicyEvaluationResult = (
    Allowed = 0,
    Blocked = 1,
    ConsentRequired = 2
  );
  PEnterpriseData_ProtectionPolicyEvaluationResult = ^EnterpriseData_ProtectionPolicyEvaluationResult;

  // Windows.Security.EnterpriseData.ProtectionPolicyRequestAccessBehavior
  EnterpriseData_ProtectionPolicyRequestAccessBehavior = (
    Decrypt = 0,
    TreatOverridePolicyAsBlock = 1
  );
  PEnterpriseData_ProtectionPolicyRequestAccessBehavior = ^EnterpriseData_ProtectionPolicyRequestAccessBehavior;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasDisallowConvenienceLogonResult
  ExchangeActiveSyncProvisioning_EasDisallowConvenienceLogonResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    RequestedPolicyIsStricter = 3
  );
  PExchangeActiveSyncProvisioning_EasDisallowConvenienceLogonResult = ^ExchangeActiveSyncProvisioning_EasDisallowConvenienceLogonResult;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasEncryptionProviderType
  ExchangeActiveSyncProvisioning_EasEncryptionProviderType = (
    NotEvaluated = 0,
    WindowsEncryption = 1,
    OtherEncryption = 2
  );
  PExchangeActiveSyncProvisioning_EasEncryptionProviderType = ^ExchangeActiveSyncProvisioning_EasEncryptionProviderType;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasMaxInactivityTimeLockResult
  ExchangeActiveSyncProvisioning_EasMaxInactivityTimeLockResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    RequestedPolicyIsStricter = 3,
    InvalidParameter = 4
  );
  PExchangeActiveSyncProvisioning_EasMaxInactivityTimeLockResult = ^ExchangeActiveSyncProvisioning_EasMaxInactivityTimeLockResult;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasMaxPasswordFailedAttemptsResult
  ExchangeActiveSyncProvisioning_EasMaxPasswordFailedAttemptsResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    RequestedPolicyIsStricter = 3,
    InvalidParameter = 4
  );
  PExchangeActiveSyncProvisioning_EasMaxPasswordFailedAttemptsResult = ^ExchangeActiveSyncProvisioning_EasMaxPasswordFailedAttemptsResult;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasMinPasswordComplexCharactersResult
  ExchangeActiveSyncProvisioning_EasMinPasswordComplexCharactersResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    RequestedPolicyIsStricter = 3,
    RequestedPolicyNotEnforceable = 4,
    InvalidParameter = 5,
    CurrentUserHasBlankPassword = 6,
    AdminsHaveBlankPassword = 7,
    UserCannotChangePassword = 8,
    AdminsCannotChangePassword = 9,
    LocalControlledUsersCannotChangePassword = 10,
    ConnectedAdminsProviderPolicyIsWeak = 11,
    ConnectedUserProviderPolicyIsWeak = 12,
    ChangeConnectedAdminsPassword = 13,
    ChangeConnectedUserPassword = 14
  );
  PExchangeActiveSyncProvisioning_EasMinPasswordComplexCharactersResult = ^ExchangeActiveSyncProvisioning_EasMinPasswordComplexCharactersResult;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasMinPasswordLengthResult
  ExchangeActiveSyncProvisioning_EasMinPasswordLengthResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    RequestedPolicyIsStricter = 3,
    RequestedPolicyNotEnforceable = 4,
    InvalidParameter = 5,
    CurrentUserHasBlankPassword = 6,
    AdminsHaveBlankPassword = 7,
    UserCannotChangePassword = 8,
    AdminsCannotChangePassword = 9,
    LocalControlledUsersCannotChangePassword = 10,
    ConnectedAdminsProviderPolicyIsWeak = 11,
    ConnectedUserProviderPolicyIsWeak = 12,
    ChangeConnectedAdminsPassword = 13,
    ChangeConnectedUserPassword = 14
  );
  PExchangeActiveSyncProvisioning_EasMinPasswordLengthResult = ^ExchangeActiveSyncProvisioning_EasMinPasswordLengthResult;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasPasswordExpirationResult
  ExchangeActiveSyncProvisioning_EasPasswordExpirationResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    RequestedPolicyIsStricter = 3,
    RequestedExpirationIncompatible = 4,
    InvalidParameter = 5,
    UserCannotChangePassword = 6,
    AdminsCannotChangePassword = 7,
    LocalControlledUsersCannotChangePassword = 8
  );
  PExchangeActiveSyncProvisioning_EasPasswordExpirationResult = ^ExchangeActiveSyncProvisioning_EasPasswordExpirationResult;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasPasswordHistoryResult
  ExchangeActiveSyncProvisioning_EasPasswordHistoryResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    RequestedPolicyIsStricter = 3,
    InvalidParameter = 4
  );
  PExchangeActiveSyncProvisioning_EasPasswordHistoryResult = ^ExchangeActiveSyncProvisioning_EasPasswordHistoryResult;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasRequireEncryptionResult
  ExchangeActiveSyncProvisioning_EasRequireEncryptionResult = (
    NotEvaluated = 0,
    Compliant = 1,
    CanBeCompliant = 2,
    NotProvisionedOnAllVolumes = 3,
    DeFixedDataNotSupported = 4,
    FixedDataNotSupported = 4,
    DeHardwareNotCompliant = 5,
    HardwareNotCompliant = 5,
    DeWinReNotConfigured = 6,
    LockNotConfigured = 6,
    DeProtectionSuspended = 7,
    ProtectionSuspended = 7,
    DeOsVolumeNotProtected = 8,
    OsVolumeNotProtected = 8,
    DeProtectionNotYetEnabled = 9,
    ProtectionNotYetEnabled = 9,
    NoFeatureLicense = 10,
    OsNotProtected = 11,
    UnexpectedFailure = 12
  );
  PExchangeActiveSyncProvisioning_EasRequireEncryptionResult = ^ExchangeActiveSyncProvisioning_EasRequireEncryptionResult;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentActivator
  Isolation_IsolatedWindowsEnvironmentActivator = (
    System = 0,
    User = 1
  );
  PIsolation_IsolatedWindowsEnvironmentActivator = ^Isolation_IsolatedWindowsEnvironmentActivator;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentAllowedClipboardFormats
  Isolation_IsolatedWindowsEnvironmentAllowedClipboardFormats = (
    None = 0,
    Text = 1,
    Image = 2
  );
  PIsolation_IsolatedWindowsEnvironmentAllowedClipboardFormats = ^Isolation_IsolatedWindowsEnvironmentAllowedClipboardFormats;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentAvailablePrinters
  Isolation_IsolatedWindowsEnvironmentAvailablePrinters = (
    None = 0,
    Local = 1,
    Network = 2,
    SystemPrintToPdf = 4,
    SystemPrintToXps = 8
  );
  PIsolation_IsolatedWindowsEnvironmentAvailablePrinters = ^Isolation_IsolatedWindowsEnvironmentAvailablePrinters;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentClipboardCopyPasteDirections
  Isolation_IsolatedWindowsEnvironmentClipboardCopyPasteDirections = (
    None = 0,
    HostToIsolatedWindowsEnvironment = 1,
    IsolatedWindowsEnvironmentToHost = 2
  );
  PIsolation_IsolatedWindowsEnvironmentClipboardCopyPasteDirections = ^Isolation_IsolatedWindowsEnvironmentClipboardCopyPasteDirections;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentCreateStatus
  Isolation_IsolatedWindowsEnvironmentCreateStatus = (
    Success = 0,
    FailureByPolicy = 1,
    UnknownFailure = 2
  );
  PIsolation_IsolatedWindowsEnvironmentCreateStatus = ^Isolation_IsolatedWindowsEnvironmentCreateStatus;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentHostError
  Isolation_IsolatedWindowsEnvironmentHostError = (
    AdminPolicyIsDisabledOrNotPresent = 0,
    FeatureNotInstalled = 1,
    HardwareRequirementsNotMet = 2,
    RebootRequired = 3,
    UnknownError = 4
  );
  PIsolation_IsolatedWindowsEnvironmentHostError = ^Isolation_IsolatedWindowsEnvironmentHostError;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentLaunchFileStatus
  Isolation_IsolatedWindowsEnvironmentLaunchFileStatus = (
    Success = 0,
    UnknownFailure = 1,
    EnvironmentUnavailable = 2,
    FileNotFound = 3,
    TimedOut = 4
  );
  PIsolation_IsolatedWindowsEnvironmentLaunchFileStatus = ^Isolation_IsolatedWindowsEnvironmentLaunchFileStatus;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentOwnerRegistrationStatus
  Isolation_IsolatedWindowsEnvironmentOwnerRegistrationStatus = (
    Success = 0,
    InvalidArgument = 1,
    AccessDenied = 2,
    InsufficientMemory = 3,
    UnknownFailure = 4
  );
  PIsolation_IsolatedWindowsEnvironmentOwnerRegistrationStatus = ^Isolation_IsolatedWindowsEnvironmentOwnerRegistrationStatus;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentPostMessageStatus
  Isolation_IsolatedWindowsEnvironmentPostMessageStatus = (
    Success = 0,
    UnknownFailure = 1,
    EnvironmentUnavailable = 2
  );
  PIsolation_IsolatedWindowsEnvironmentPostMessageStatus = ^Isolation_IsolatedWindowsEnvironmentPostMessageStatus;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentProcessState
  Isolation_IsolatedWindowsEnvironmentProcessState = (
    Running = 1,
    Aborted = 2,
    Completed = 3
  );
  PIsolation_IsolatedWindowsEnvironmentProcessState = ^Isolation_IsolatedWindowsEnvironmentProcessState;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentProgressState
  Isolation_IsolatedWindowsEnvironmentProgressState = (
    Queued = 0,
    Processing = 1,
    Completed = 2
  );
  PIsolation_IsolatedWindowsEnvironmentProgressState = ^Isolation_IsolatedWindowsEnvironmentProgressState;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentShareFolderStatus
  Isolation_IsolatedWindowsEnvironmentShareFolderStatus = (
    Success = 0,
    UnknownFailure = 1,
    EnvironmentUnavailable = 2,
    FolderNotFound = 3,
    AccessDenied = 4
  );
  PIsolation_IsolatedWindowsEnvironmentShareFolderStatus = ^Isolation_IsolatedWindowsEnvironmentShareFolderStatus;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentStartProcessStatus
  Isolation_IsolatedWindowsEnvironmentStartProcessStatus = (
    Success = 0,
    UnknownFailure = 1,
    EnvironmentUnavailable = 2,
    FileNotFound = 3,
    AppNotRegistered = 4
  );
  PIsolation_IsolatedWindowsEnvironmentStartProcessStatus = ^Isolation_IsolatedWindowsEnvironmentStartProcessStatus;

  // Windows.Security Records
  // Windows.Security.EnterpriseData.EnterpriseDataContract
  EnterpriseData_EnterpriseDataContract = record
  end;
  PEnterpriseData_EnterpriseDataContract = ^EnterpriseData_EnterpriseDataContract;

  // Windows.Security.ExchangeActiveSyncProvisioning.EasContract
  ExchangeActiveSyncProvisioning_EasContract = record
  end;
  PExchangeActiveSyncProvisioning_EasContract = ^ExchangeActiveSyncProvisioning_EasContract;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentContract
  Isolation_IsolatedWindowsEnvironmentContract = record
  end;
  PIsolation_IsolatedWindowsEnvironmentContract = ^Isolation_IsolatedWindowsEnvironmentContract;

  // Windows.Security.Isolation.IsolatedWindowsEnvironmentCreateProgress
  Isolation_IsolatedWindowsEnvironmentCreateProgress = record
    State: Isolation_IsolatedWindowsEnvironmentProgressState;
    PercentComplete: Cardinal;
  end;
  PIsolation_IsolatedWindowsEnvironmentCreateProgress = ^Isolation_IsolatedWindowsEnvironmentCreateProgress;

  // Windows.Security Interfaces

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderOperation
  Authentication_Web_Provider_IWebAccountProviderOperation = interface(IInspectable)
  ['{6D5D2426-10B1-419A-A44E-F9C5161574E6}']
    function get_Kind: Authentication_Web_Provider_WebAccountProviderOperationKind; safecall;
    property Kind: Authentication_Web_Provider_WebAccountProviderOperationKind read get_Kind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.ProtectionPolicyEvaluationResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult_Delegate_Base = interface(IUnknown)
  ['{2833BA54-A4E1-5C2D-8A7A-136E8510C78B}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.ProtectionPolicyEvaluationResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult = interface(AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.ProtectionPolicyEvaluationResult>
  IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult_Base = interface(IInspectable)
  ['{E8D81715-C56C-5A6B-B738-5DF6C2775B7B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
    function GetResults: EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_ProtectionPolicyEvaluationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.ProtectionPolicyEvaluationResult>
  IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult = interface(IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebProviderError
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebProviderError)]
  Authentication_Web_Core_IWebProviderError = interface(IInspectable)
  ['{DB191BB1-50C5-4809-8DCA-09C99410245C}']
    function get_ErrorCode: Cardinal; safecall;
    function get_ErrorMessage: HSTRING; safecall;
    function get_Properties: IMap_2__HSTRING__HSTRING; safecall;
    property ErrorCode: Cardinal read get_ErrorCode;
    property ErrorMessage: HSTRING read get_ErrorMessage;
    property Properties: IMap_2__HSTRING__HSTRING read get_Properties;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebTokenResponse
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebTokenResponse)]
  Authentication_Web_Core_IWebTokenResponse = interface(IInspectable)
  ['{67A7C5CA-83F6-44C6-A3B1-0EB69E41FA8A}']
    function get_Token: HSTRING; safecall;
    function get_ProviderError: Authentication_Web_Core_IWebProviderError; safecall;
    function get_WebAccount: IWebAccount; safecall;
    function get_Properties: IMap_2__HSTRING__HSTRING; safecall;
    property Properties: IMap_2__HSTRING__HSTRING read get_Properties;
    property ProviderError: Authentication_Web_Core_IWebProviderError read get_ProviderError;
    property Token: HSTRING read get_Token;
    property WebAccount: IWebAccount read get_WebAccount;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IIterator_1__Authentication_Web_Core_IWebTokenResponse_Base = interface(IInspectable)
  ['{F080B0C9-A095-5B3A-A1DC-D17E7D2982C7}']
    function get_Current: Authentication_Web_Core_IWebTokenResponse; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAuthentication_Web_Core_IWebTokenResponse): Cardinal; safecall;
    property Current: Authentication_Web_Core_IWebTokenResponse read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IIterator_1__Authentication_Web_Core_IWebTokenResponse = interface(IIterator_1__Authentication_Web_Core_IWebTokenResponse_Base)
  ['{3AF93454-0FA5-5B50-9FA6-4CDE1AA49885}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IIterable_1__Authentication_Web_Core_IWebTokenResponse_Base = interface(IInspectable)
  ['{7E5BB7EC-BBD7-5575-9A61-F5815FA22A0E}']
    function First: IIterator_1__Authentication_Web_Core_IWebTokenResponse; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IIterable_1__Authentication_Web_Core_IWebTokenResponse = interface(IIterable_1__Authentication_Web_Core_IWebTokenResponse_Base)
  ['{12223EBA-B793-5936-A23F-894F7F41CB0A}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Core.IWebTokenResponse>
  IVectorView_1__Authentication_Web_Core_IWebTokenResponse = interface(IInspectable)
  ['{DD754C27-54D4-5AA1-88C2-619EB1D36B2F}']
    function GetAt(index: Cardinal): Authentication_Web_Core_IWebTokenResponse; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Authentication_Web_Core_IWebTokenResponse; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAuthentication_Web_Core_IWebTokenResponse): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebTokenRequestResult
  Authentication_Web_Core_IWebTokenRequestResult = interface(IInspectable)
  ['{C12A8305-D1F8-4483-8D54-38FE292784FF}']
    function get_ResponseData: IVectorView_1__Authentication_Web_Core_IWebTokenResponse; safecall;
    function get_ResponseStatus: Authentication_Web_Core_WebTokenRequestStatus; safecall;
    function get_ResponseError: Authentication_Web_Core_IWebProviderError; safecall;
    function InvalidateCacheAsync: IAsyncAction; safecall;
    property ResponseData: IVectorView_1__Authentication_Web_Core_IWebTokenResponse read get_ResponseData;
    property ResponseError: Authentication_Web_Core_IWebProviderError read get_ResponseError;
    property ResponseStatus: Authentication_Web_Core_WebTokenRequestStatus read get_ResponseStatus;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Web.Core.IWebTokenRequestResult>
  AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult_Delegate_Base = interface(IUnknown)
  ['{DEB54B22-70F2-55AB-97C0-6CBDC5DDB6F0}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Web.Core.IWebTokenRequestResult>
  AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult = interface(AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult_Delegate_Base)
  ['{410483D4-EF99-5BB4-A63E-9F89DE125364}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Web.Core.IWebTokenRequestResult>
  IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult_Base = interface(IInspectable)
  ['{0A815852-7C44-5674-B3D2-FA2E4C1E46C9}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult; safecall;
    function GetResults: Authentication_Web_Core_IWebTokenRequestResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Web_Core_IWebTokenRequestResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Web.Core.IWebTokenRequestResult>
  IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult = interface(IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult_Base)
  ['{D749599B-BAAE-5B22-A217-362B28FE19E4}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebTokenRequest
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebTokenRequest)]
  Authentication_Web_Core_IWebTokenRequest = interface(IInspectable)
  ['{B77B4D68-ADCB-4673-B364-0CF7B35CAF97}']
    function get_WebAccountProvider: IWebAccountProvider; safecall;
    function get_Scope: HSTRING; safecall;
    function get_ClientId: HSTRING; safecall;
    function get_PromptType: Authentication_Web_Core_WebTokenRequestPromptType; safecall;
    function get_Properties: IMap_2__HSTRING__HSTRING; safecall;
    property ClientId: HSTRING read get_ClientId;
    property PromptType: Authentication_Web_Core_WebTokenRequestPromptType read get_PromptType;
    property Properties: IMap_2__HSTRING__HSTRING read get_Properties;
    property Scope: HSTRING read get_Scope;
    property WebAccountProvider: IWebAccountProvider read get_WebAccountProvider;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorServiceResponse>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse_Delegate_Base = interface(IUnknown)
  ['{8D7F8240-81CF-5896-95FA-E7B223F769F9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorServiceResponse>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = interface(AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorServiceResponse>
  IAsyncOperation_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse_Base = interface(IInspectable)
  ['{05953B8E-5ADB-51B9-A94A-AD030030B8E3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse; safecall;
    function GetResults: Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Core.MicrosoftAccountMultiFactorServiceResponse>
  IAsyncOperation_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse = interface(IAsyncOperation_1__Authentication_Identity_Core_MicrosoftAccountMultiFactorServiceResponse_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface(IInspectable)
  ['{8C304EBB-6615-50A4-8829-879ECD443236}']
    function get_Current: HSTRING; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    property Current: HSTRING read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface(IInspectable)
  ['{E2FCC7C1-3BFC-5A0B-B2B0-72E769D1CB7E}']
    function First: IIterator_1__HSTRING; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface(IInspectable)
  ['{2F13C006-A03A-5F69-B090-75A43E33423E}']
    function GetAt(index: Cardinal): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: HSTRING; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo
  Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface(IInspectable)
  ['{38321ACC-672B-4823-B603-6B3C753DAF97}']
    function get_TenantId: HSTRING; safecall;
    function get_TenantName: HSTRING; safecall;
    function get_Subject: HSTRING; safecall;
    function get_KeyId: HSTRING; safecall;
    function get_KeyName: HSTRING; safecall;
    property KeyId: HSTRING read get_KeyId;
    property KeyName: HSTRING read get_KeyName;
    property Subject: HSTRING read get_Subject;
    property TenantId: HSTRING read get_TenantId;
    property TenantName: HSTRING read get_TenantName;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IIterator_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Base = interface(IInspectable)
  ['{E3497259-1737-5FAD-803B-9D2D29273E3B}']
    function get_Current: Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAuthentication_Identity_IEnterpriseKeyCredentialRegistrationInfo): Cardinal; safecall;
    property Current: Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IIterator_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface(IIterator_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Base)
  ['{6FE49BFB-B688-528E-A883-1021525D9BD2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IIterable_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Base = interface(IInspectable)
  ['{E7EEA796-77F9-5473-A913-734EA0E3FF46}']
    function First: IIterator_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IIterable_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface(IIterable_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Base)
  ['{2547378B-BA24-5881-93AF-930ECF5DB0B9}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>
  IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface(IInspectable)
  ['{1973E8DE-FB97-510A-B752-B0C33FB68099}']
    function GetAt(index: Cardinal): Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAuthentication_Identity_IEnterpriseKeyCredentialRegistrationInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Delegate_Base = interface(IUnknown)
  ['{67746C40-ADE0-5981-AE23-104891748853}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Delegate_Base)
  ['{2BF818B1-8C9D-5010-B808-F655AD79AA15}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>>
  IAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Base = interface(IInspectable)
  ['{0BD64C2F-8B1D-56D4-A707-FAB5315E7278}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; safecall;
    function GetResults: IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationInfo>>
  IAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo = interface(IAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo_Base)
  ['{FCDA40FF-5D68-5D52-8F1C-3103BB6FD0B7}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationManager
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Identity_EnterpriseKeyCredentialRegistrationManager)]
  Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager = interface(IInspectable)
  ['{83F3BE3F-A25F-4CBA-BB8E-BDC32D03C297}']
    function GetRegistrationsAsync: IAsyncOperation_1__IVectorView_1__Authentication_Identity_IEnterpriseKeyCredentialRegistrationInfo; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Identity_EnterpriseKeyCredentialRegistrationManager)]
  Authentication_Identity_IEnterpriseKeyCredentialRegistrationManagerStatics = interface(IInspectable)
  ['{77B85E9E-ACF4-4BC0-BAC2-40BB46EFBB3F}']
    function get_Current: Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager; safecall;
    property Current: Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager read get_Current;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorFinishAuthenticationStatus>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus_Delegate_Base = interface(IUnknown)
  ['{AE1D7146-3D91-50E3-8F13-613CF2801207}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorFinishAuthenticationStatus>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = interface(AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorFinishAuthenticationStatus>
  IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus_Base = interface(IInspectable)
  ['{9159437A-4397-546E-BE61-2EF161717E06}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus; safecall;
    function GetResults: Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorFinishAuthenticationStatus>
  IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus = interface(IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthentication
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication)]
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthentication = interface(IInspectable)
  ['{020A16E5-6A25-40A3-8C00-50A023F619D1}']
    function get_ServiceAuthenticationHmac: IBuffer; safecall;
    function get_SessionNonce: IBuffer; safecall;
    function get_DeviceNonce: IBuffer; safecall;
    function get_DeviceConfigurationData: IBuffer; safecall;
    function FinishAuthenticationAsync(deviceHmac: IBuffer; sessionHmac: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorFinishAuthenticationStatus; safecall;
    function AbortAuthenticationAsync(errorLogMessage: HSTRING): IAsyncAction; safecall;
    property DeviceConfigurationData: IBuffer read get_DeviceConfigurationData;
    property DeviceNonce: IBuffer read get_DeviceNonce;
    property ServiceAuthenticationHmac: IBuffer read get_ServiceAuthenticationHmac;
    property SessionNonce: IBuffer read get_SessionNonce;
  end deprecated;

  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = interface(IInspectable)
  ['{9CBB5987-EF6D-4BC2-BF49-4617515A0F9A}']
    function get_Status: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStatus; safecall;
    function get_Authentication: Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthentication; safecall;
    property Authentication: Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthentication read get_Authentication;
    property Status: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStatus read get_Status;
  end deprecated;

  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = interface(IInspectable)
  ['{56FEC28B-E8AA-4C0F-8E4C-A559E73ADD88}']
    function get_Stage: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStage; safecall;
    function get_Scenario: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationScenario; safecall;
    function get_DeviceId: HSTRING; safecall;
    property DeviceId: HSTRING read get_DeviceId;
    property Scenario: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationScenario read get_Scenario;
    property Stage: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationStage read get_Stage;
  end deprecated;

  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs = interface(IInspectable)
  ['{D4A5EE56-7291-4073-BC1F-CCB8F5AFDF96}']
    function get_StageInfo: Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo; safecall;
    property StageInfo: Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo read get_StageInfo;
  end deprecated;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult_Delegate_Base = interface(IUnknown)
  ['{2547373D-9684-5E5B-A9B8-A6F90CE632AD}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = interface(AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult_Delegate_Base)
  ['{136622DE-6513-5453-ABA2-ADF5718C1CA2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult_Base = interface(IInspectable)
  ['{18B0A73C-DB59-5279-A76D-02416B2D90B6}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult; safecall;
    function GetResults: Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationResult>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult = interface(IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult_Base)
  ['{4297B2B0-B7FC-5572-B959-83298BCDEEBC}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs>
  EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{10409B3C-42E4-586F-84C1-803DA23765AF}']
    procedure Invoke(sender: IInspectable; args: Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs>
  EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs = interface(EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs_Delegate_Base)
  ['{FD8238F4-6BAF-5D84-B1AA-6F2B9DBEF3EF}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo_Delegate_Base = interface(IUnknown)
  ['{7811D384-2EB8-58F1-AFED-4B4B888F4357}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = interface(AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo_Delegate_Base)
  ['{F17881CD-CAD2-50B1-B3BA-575E09E174F5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo_Base = interface(IInspectable)
  ['{864A2317-B440-5E9E-AE55-4550BB6307DF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo; safecall;
    function GetResults: Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStageInfo>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo = interface(IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo_Base)
  ['{F2EB2A7B-B5DD-58AB-A49B-90F4BC5A370A}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication)]
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStatics = interface(IInspectable)
  ['{3F582656-28F8-4E0F-AE8C-5898B9AE2469}']
    function ShowNotificationMessageAsync(deviceName: HSTRING; &message: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationMessage): IAsyncAction; safecall;
    function StartAuthenticationAsync(deviceId: HSTRING; serviceAuthenticationNonce: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult; safecall;
    function add_AuthenticationStageChanged(handler: EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AuthenticationStageChanged(token: EventRegistrationToken); safecall;
    function GetAuthenticationStageInfoAsync: IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo; safecall;
  end deprecated;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus_Delegate_Base = interface(IUnknown)
  ['{2294A212-9061-5E99-A226-A44AC8F8F4DD}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = interface(AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus>
  IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus_Base = interface(IInspectable)
  ['{7F37ECEA-E3E8-53FC-B0E5-7AA471970EDD}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus; safecall;
    function GetResults: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus>
  IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus = interface(IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Identity_Provider_SecondaryAuthenticationFactorRegistration)]
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics = interface(IInspectable)
  ['{90499A19-7EF2-4523-951C-A417A24ACF93}']
    function RegisterDevicePresenceMonitoringAsync(deviceId: HSTRING; deviceInstancePath: HSTRING; monitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode): IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus; overload; safecall;
    function RegisterDevicePresenceMonitoringAsync(deviceId: HSTRING; deviceInstancePath: HSTRING; monitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode; deviceFriendlyName: HSTRING; deviceModelNumber: HSTRING; deviceConfigurationData: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus; overload; safecall;
    function UnregisterDevicePresenceMonitoringAsync(deviceId: HSTRING): IAsyncAction; safecall;
    function IsDevicePresenceMonitoringSupported: Boolean; safecall;
  end deprecated;

  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface(IInspectable)
  ['{1E2BA861-8533-4FCE-839B-ECB72410AC14}']
    function get_DeviceId: HSTRING; safecall;
    function get_DeviceFriendlyName: HSTRING; safecall;
    function get_DeviceModelNumber: HSTRING; safecall;
    function get_DeviceConfigurationData: IBuffer; safecall;
    property DeviceConfigurationData: IBuffer read get_DeviceConfigurationData;
    property DeviceFriendlyName: HSTRING read get_DeviceFriendlyName;
    property DeviceId: HSTRING read get_DeviceId;
    property DeviceModelNumber: HSTRING read get_DeviceModelNumber;
  end deprecated;

  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo2
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo2 = interface(IInspectable)
  ['{14D981A3-FC26-4FF7-ABC3-48E82A512A0A}']
    function get_PresenceMonitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode; safecall;
    function UpdateDevicePresenceAsync(presenceState: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresence): IAsyncAction; safecall;
    function get_IsAuthenticationSupported: Boolean; safecall;
    property IsAuthenticationSupported: Boolean read get_IsAuthenticationSupported;
    property PresenceMonitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode read get_PresenceMonitoringMode;
  end deprecated;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistration
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Identity_Provider_SecondaryAuthenticationFactorRegistration)]
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistration = interface(IInspectable)
  ['{9F4CBBB4-8CBA-48B0-840D-DBB22A54C678}']
    function FinishRegisteringDeviceAsync(deviceConfigurationData: IBuffer): IAsyncAction; safecall;
    function AbortRegisteringDeviceAsync(errorLogMessage: HSTRING): IAsyncAction; safecall;
  end deprecated;

  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = interface(IInspectable)
  ['{A4FE35F0-ADE3-4981-AF6B-EC195921682A}']
    function get_Status: Authentication_Identity_Provider_SecondaryAuthenticationFactorRegistrationStatus; safecall;
    function get_Registration: Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistration; safecall;
    property Registration: Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistration read get_Registration;
    property Status: Authentication_Identity_Provider_SecondaryAuthenticationFactorRegistrationStatus read get_Status;
  end deprecated;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult_Delegate_Base = interface(IUnknown)
  ['{A04902E8-F830-50EA-89EA-96E2A6FB9453}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult>
  AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = interface(AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult_Delegate_Base)
  ['{99DBF96A-A7C7-55DC-8DD6-8587B90FA644}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult_Base = interface(IInspectable)
  ['{05DA520C-ABA4-584C-BC08-19C5389A70E2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult; safecall;
    function GetResults: Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationResult>
  IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult = interface(IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult_Base)
  ['{8619201A-7EC2-5F94-9AC9-AEFDE88C169D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IIterator_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Base = interface(IInspectable)
  ['{0EEDBDA6-2DE0-50AF-ABC4-46073245FB2D}']
    function get_Current: Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorInfo): Cardinal; safecall;
    property Current: Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IIterator_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface(IIterator_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Base)
  ['{F8B0A1FD-6C68-52B5-97BE-F990D0A7DC71}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IIterable_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Base = interface(IInspectable)
  ['{43B7BBE4-F096-53DD-8C16-1FAA4B468C86}']
    function First: IIterator_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IIterable_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface(IIterable_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Base)
  ['{43EBA77C-FA51-5A82-9937-2C412CD0053F}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>
  IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface(IInspectable)
  ['{AEDC9B77-E59A-5598-878F-D6F0D53D9C55}']
    function GetAt(index: Cardinal): Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAuthentication_Identity_Provider_ISecondaryAuthenticationFactorInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Delegate_Base = interface(IUnknown)
  ['{06752D25-D43E-5D2E-A305-4E1576846FEE}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Delegate_Base)
  ['{57F82FD8-CFFB-5DF8-B5C2-3238113EC69D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>>
  IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Base = interface(IInspectable)
  ['{47EB155B-ABE0-55A5-9310-FEB1DD57DCA5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; safecall;
    function GetResults: IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorInfo>>
  IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo = interface(IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo_Base)
  ['{575738B8-554D-55C4-86F9-5E97BA5D7C70}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Identity_Provider_SecondaryAuthenticationFactorRegistration)]
  Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationStatics = interface(IInspectable)
  ['{1ADF0F65-E3B7-4155-997F-B756EF65BEBA}']
    function RequestStartRegisteringDeviceAsync(deviceId: HSTRING; capabilities: Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceCapabilities; deviceFriendlyName: HSTRING; deviceModelNumber: HSTRING; deviceKey: IBuffer; mutualAuthenticationKey: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult; safecall;
    function FindAllRegisteredDeviceInfoAsync(queryType: Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceFindScope): IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; safecall;
    function UnregisterDeviceAsync(deviceId: HSTRING): IAsyncAction; safecall;
    function UpdateDeviceConfigurationDataAsync(deviceId: HSTRING; deviceConfigurationData: IBuffer): IAsyncAction; safecall;
  end deprecated;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  AsyncOperationCompletedHandler_1__IInspectable = interface(IUnknown)
  ['{3F08262E-A2E1-5134-9297-E9211F481A2D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IInspectable; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Object>
  IAsyncOperation_1__IInspectable = interface(IInspectable)
  ['{ABF53C57-EE50-5342-B52A-26E3B8CC024F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IInspectable); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IInspectable; safecall;
    function GetResults: IInspectable; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IInspectable read get_Completed write put_Completed;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Object>
  IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{09335560-6C6B-5A26-9348-97B781132B20}']
    function get_Key: HSTRING; safecall;
    function get_Value: IInspectable; safecall;
    property Key: HSTRING read get_Key;
    property Value: IInspectable read get_Value;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{5DB5FA32-707C-5849-A06B-91C8EB9D10E8}']
    function get_Current: IKeyValuePair_2__HSTRING__IInspectable; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IInspectable): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IInspectable read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{FE2F3D47-5D47-5499-8374-430C7CDA0204}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IInspectable; safecall;
  end;

  // Windows.Foundation.Collections.IMapView`2<String,Object>
  IMapView_2__HSTRING__IInspectable = interface(IInspectable)
  ['{BB78502A-F79D-54FA-92C9-90C5039FDF7E}']
    function Lookup(key: HSTRING): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IInspectable; out second: IMapView_2__HSTRING__IInspectable); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IMap`2<String,Object>
  IMap_2__HSTRING__IInspectable = interface(IInspectable)
  ['{1B0D3570-0877-5EC2-8A2C-3B9539506ACA}']
    function Lookup(key: HSTRING): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    function GetView: IMapView_2__HSTRING__IInspectable; safecall;
    function Insert(key: HSTRING; value: IInspectable): Boolean; safecall;
    procedure Remove(key: HSTRING); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  IMapChangedEventArgs_1__HSTRING = interface(IInspectable)
  ['{60141EFB-F2F9-5377-96FD-F8C60D9558B5}']
    function get_CollectionChange: CollectionChange; safecall;
    function get_Key: HSTRING; safecall;
    property CollectionChange_: CollectionChange read get_CollectionChange;
    property Key: HSTRING read get_Key;
  end;

  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable = interface(IUnknown)
  ['{24F981E5-DDCA-538D-AADA-A59906084CF1}']
    procedure Invoke(sender: IObservableMap_2__HSTRING__IInspectable; event: IMapChangedEventArgs_1__HSTRING); safecall;
  end;

  // Windows.Foundation.Collections.IObservableMap`2<String,Object>
  IObservableMap_2__HSTRING__IInspectable = interface(IInspectable)
  ['{236AAC9D-FB12-5C4D-A41C-9E445FB4D7EC}']
    function add_MapChanged(vhnd: MapChangedEventHandler_2__HSTRING__IInspectable): EventRegistrationToken; safecall;
    procedure remove_MapChanged(token: EventRegistrationToken); safecall;
  end;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface(IUnknown)
  ['{BF666554-7605-5D9A-B14E-18D8C8472AFE}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; progressInfo: Cardinal); safecall;
  end;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface(IUnknown)
  ['{06386A7A-E009-5B0B-AB68-A8E48B516647}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface(IInspectable)
  ['{D26B2819-897F-5C7D-84D6-56D796561431}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IBuffer__Cardinal); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IBuffer__Cardinal; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal; safecall;
    function GetResults: IBuffer; safecall;
    property Progress: AsyncOperationProgressHandler_2__IBuffer__Cardinal read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal read get_Completed write put_Completed;
  end;

  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface(IUnknown)
  ['{EA0FE405-D432-5AC7-9EF8-5A65E1F97D7E}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; progressInfo: Cardinal); safecall;
  end;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface(IUnknown)
  ['{1E466DC5-840F-54F9-B877-5E3A9F4B6C74}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = interface(IInspectable)
  ['{ECCB574A-C684-5572-A679-6B0842CFB57F}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__Cardinal__Cardinal); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__Cardinal__Cardinal; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal; safecall;
    function GetResults: Cardinal; safecall;
    property Progress: AsyncOperationProgressHandler_2__Cardinal__Cardinal read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal read get_Completed write put_Completed;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface(IUnknown)
  ['{C1D3D1A2-AE17-5A5F-B5A2-BDCC8844889A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Boolean; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface(IInspectable)
  ['{CDB5EFB3-5788-509D-9BE1-71CCB8A3362A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Boolean); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Boolean; safecall;
    function GetResults: Boolean; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Boolean read get_Completed write put_Completed;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,String>
  IKeyValuePair_2__HSTRING__HSTRING = interface(IInspectable)
  ['{60310303-49C5-52E6-ABC6-A9B36ECCC716}']
    function get_Key: HSTRING; safecall;
    function get_Value: HSTRING; safecall;
    property Key: HSTRING read get_Key;
    property Value: HSTRING read get_Value;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterator_1__IKeyValuePair_2__HSTRING__HSTRING = interface(IInspectable)
  ['{05EB86F1-7140-5517-B88D-CBAEBE57E6B1}']
    function get_Current: IKeyValuePair_2__HSTRING__HSTRING; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__HSTRING): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__HSTRING read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterable_1__IKeyValuePair_2__HSTRING__HSTRING = interface(IInspectable)
  ['{E9BDAAF0-CBF6-5C72-BE90-29CBF3A1319B}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__HSTRING; safecall;
  end;

  // Windows.Foundation.Collections.IMapView`2<String,String>
  IMapView_2__HSTRING__HSTRING = interface(IInspectable)
  ['{AC7F26F2-FEB7-5B2A-8AC4-345BC62CAEDE}']
    function Lookup(key: HSTRING): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__HSTRING; out second: IMapView_2__HSTRING__HSTRING); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IMap`2<String,String>
  IMap_2__HSTRING__HSTRING = interface(IInspectable)
  ['{F6D1F700-49C2-52AE-8154-826F9908773C}']
    function Lookup(key: HSTRING): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    function GetView: IMapView_2__HSTRING__HSTRING; safecall;
    function Insert(key: HSTRING; value: HSTRING): Boolean; safecall;
    procedure Remove(key: HSTRING); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IFindAllAccountsResult
  Authentication_Web_Core_IFindAllAccountsResult = interface(IInspectable)
  ['{A5812B5D-B72E-420C-86AB-AAC0D7B7261F}']
    function get_Accounts: IVectorView_1__IWebAccount; safecall;
    function get_Status: Authentication_Web_Core_FindAllWebAccountsStatus; safecall;
    function get_ProviderError: Authentication_Web_Core_IWebProviderError; safecall;
    property Accounts: IVectorView_1__IWebAccount read get_Accounts;
    property ProviderError: Authentication_Web_Core_IWebProviderError read get_ProviderError;
    property Status: Authentication_Web_Core_FindAllWebAccountsStatus read get_Status;
  end;

  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebAccountEventArgs
  Authentication_Web_Core_IWebAccountEventArgs = interface(IInspectable)
  ['{6FB7037D-424E-44EC-977C-EF2415462A5A}']
    function get_Account: IWebAccount; safecall;
    property Account: IWebAccount read get_Account;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Security.Authentication.Web.Core.IWebAccountMonitor,Windows.Security.Authentication.Web.Core.IWebAccountEventArgs>
  TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs_Delegate_Base = interface(IUnknown)
  ['{FA704F04-87B6-516B-9596-CD7CC092169B}']
    procedure Invoke(sender: Authentication_Web_Core_IWebAccountMonitor; args: Authentication_Web_Core_IWebAccountEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Security.Authentication.Web.Core.IWebAccountMonitor,Windows.Security.Authentication.Web.Core.IWebAccountEventArgs>
  TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs = interface(TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs_Delegate_Base)
  ['{DA1CD638-5E24-53AD-A2F3-0631A0E98D17}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Security.Authentication.Web.Core.IWebAccountMonitor,Object>
  TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__IInspectable_Delegate_Base = interface(IUnknown)
  ['{C8CB498D-E0DA-52A1-ABF9-7198C7F5CB42}']
    procedure Invoke(sender: Authentication_Web_Core_IWebAccountMonitor; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Security.Authentication.Web.Core.IWebAccountMonitor,Object>
  TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__IInspectable = interface(TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__IInspectable_Delegate_Base)
  ['{C51133FD-5556-510B-8C8B-3DB8EF495345}']
  end;

  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebAccountMonitor
  Authentication_Web_Core_IWebAccountMonitor = interface(IInspectable)
  ['{7445F5FD-AA9D-4619-8D5D-C138A4EDE3E5}']
    function add_Updated(handler: TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs): EventRegistrationToken; safecall;
    procedure remove_Updated(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    function add_DefaultSignInAccountChanged(handler: TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__IInspectable): EventRegistrationToken; safecall;
    procedure remove_DefaultSignInAccountChanged(token: EventRegistrationToken); safecall;
  end;

  // Windows.Security.Authentication.Web.Core.IWebAccountMonitor2
  Authentication_Web_Core_IWebAccountMonitor2 = interface(IInspectable)
  ['{A7ADC1F8-24B8-4F01-9AE5-24545E71233A}']
    function add_AccountPictureUpdated(handler: TypedEventHandler_2__Authentication_Web_Core_IWebAccountMonitor__Authentication_Web_Core_IWebAccountEventArgs): EventRegistrationToken; safecall;
    procedure remove_AccountPictureUpdated(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebAuthenticationCoreManager)]
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics = interface(IInspectable)
  ['{6ACA7C92-A581-4479-9C10-752EFF44FD34}']
    function GetTokenSilentlyAsync(request: Authentication_Web_Core_IWebTokenRequest): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; safecall;
    function GetTokenSilentlyAsync(request: Authentication_Web_Core_IWebTokenRequest; webAccount: IWebAccount): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; safecall;
    function RequestTokenAsync(request: Authentication_Web_Core_IWebTokenRequest): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; safecall;
    function RequestTokenAsync(request: Authentication_Web_Core_IWebTokenRequest; webAccount: IWebAccount): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; safecall;
    function FindAccountAsync(provider: IWebAccountProvider; webAccountId: HSTRING): IAsyncOperation_1__IWebAccount; safecall;
    function FindAccountProviderAsync(webAccountProviderId: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; safecall;
    function FindAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics2
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebAuthenticationCoreManager)]
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics2 = interface(IInspectable)
  ['{F584184A-8B57-4820-B6A4-70A5B6FCF44A}']
    function FindAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING; user: IUser): IAsyncOperation_1__IWebAccountProvider; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics3
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebAuthenticationCoreManager)]
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics3 = interface(IInspectable)
  ['{2404EEB2-8924-4D93-AB3A-99688B419D56}']
    function CreateWebAccountMonitor(webAccounts: IIterable_1__IWebAccount): Authentication_Web_Core_IWebAccountMonitor; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authentication.Web.Core.IFindAllAccountsResult>
  AsyncOperationCompletedHandler_1__Authentication_Web_Core_IFindAllAccountsResult = interface(IUnknown)
  ['{1F4E8443-1CEB-5DCF-833F-4C681C26E7E6}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authentication.Web.Core.IFindAllAccountsResult>
  IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult = interface(IInspectable)
  ['{51FFE0C6-10CD-5FD1-AA10-BC587D416CED}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authentication_Web_Core_IFindAllAccountsResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authentication_Web_Core_IFindAllAccountsResult; safecall;
    function GetResults: Authentication_Web_Core_IFindAllAccountsResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authentication_Web_Core_IFindAllAccountsResult read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics4
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebAuthenticationCoreManager)]
  Authentication_Web_Core_IWebAuthenticationCoreManagerStatics4 = interface(IInspectable)
  ['{54E633FE-96E0-41E8-9832-1298897C2AAF}']
    function FindAllAccountsAsync(provider: IWebAccountProvider): IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult; overload; safecall;
    function FindAllAccountsAsync(provider: IWebAccountProvider; clientId: HSTRING): IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult; overload; safecall;
    function FindSystemAccountProviderAsync(webAccountProviderId: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; safecall;
    function FindSystemAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; safecall;
    function FindSystemAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING; user: IUser): IAsyncOperation_1__IWebAccountProvider; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebProviderErrorFactory
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebProviderError)]
  Authentication_Web_Core_IWebProviderErrorFactory = interface(IInspectable)
  ['{E3C40A2D-89EF-4E37-847F-A8B9D5A32910}']
    function Create(errorCode: Cardinal; errorMessage: HSTRING): Authentication_Web_Core_IWebProviderError; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebTokenRequest2
  Authentication_Web_Core_IWebTokenRequest2 = interface(IInspectable)
  ['{D700C079-30C8-4397-9654-961C3BE8B855}']
    function get_AppProperties: IMap_2__HSTRING__HSTRING; safecall;
    property AppProperties: IMap_2__HSTRING__HSTRING read get_AppProperties;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebTokenRequest3
  Authentication_Web_Core_IWebTokenRequest3 = interface(IInspectable)
  ['{5A755B51-3BB1-41A5-A63D-90BC32C7DB9A}']
    function get_CorrelationId: HSTRING; safecall;
    procedure put_CorrelationId(value: HSTRING); safecall;
    property CorrelationId: HSTRING read get_CorrelationId write put_CorrelationId;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebTokenRequestFactory
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebTokenRequest)]
  Authentication_Web_Core_IWebTokenRequestFactory = interface(IInspectable)
  ['{6CF2141C-0FF0-4C67-B84F-99DDBE4A72C9}']
    function Create(provider: IWebAccountProvider; scope: HSTRING; clientId: HSTRING): Authentication_Web_Core_IWebTokenRequest; safecall;
    function CreateWithPromptType(provider: IWebAccountProvider; scope: HSTRING; clientId: HSTRING; promptType: Authentication_Web_Core_WebTokenRequestPromptType): Authentication_Web_Core_IWebTokenRequest; safecall;
    function CreateWithProvider(provider: IWebAccountProvider): Authentication_Web_Core_IWebTokenRequest; safecall;
    function CreateWithScope(provider: IWebAccountProvider; scope: HSTRING): Authentication_Web_Core_IWebTokenRequest; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Core.IWebTokenResponseFactory
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Core_WebTokenResponse)]
  Authentication_Web_Core_IWebTokenResponseFactory = interface(IInspectable)
  ['{AB6BF7F8-5450-4EF6-97F7-052B0431C0F0}']
    function CreateWithToken(token: HSTRING): Authentication_Web_Core_IWebTokenResponse; safecall;
    function CreateWithTokenAndAccount(token: HSTRING; webAccount: IWebAccount): Authentication_Web_Core_IWebTokenResponse; safecall;
    function CreateWithTokenAccountAndError(token: HSTRING; webAccount: IWebAccount; error: Authentication_Web_Core_IWebProviderError): Authentication_Web_Core_IWebTokenResponse; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountClientView
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountClientView)]
  Authentication_Web_Provider_IWebAccountClientView = interface(IInspectable)
  ['{E7BD66BA-0BC7-4C66-BFD4-65D3082CBCA8}']
    function get_ApplicationCallbackUri: IUriRuntimeClass; safecall;
    function get_Type: Authentication_Web_Provider_WebAccountClientViewType; safecall;
    function get_AccountPairwiseId: HSTRING; safecall;
    property AccountPairwiseId: HSTRING read get_AccountPairwiseId;
    property ApplicationCallbackUri: IUriRuntimeClass read get_ApplicationCallbackUri;
    property &Type: Authentication_Web_Provider_WebAccountClientViewType read get_Type;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountClientViewFactory
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountClientView)]
  Authentication_Web_Provider_IWebAccountClientViewFactory = interface(IInspectable)
  ['{616D16A4-DE22-4855-A326-06CEBF2A3F23}']
    function Create(viewType: Authentication_Web_Provider_WebAccountClientViewType; applicationCallbackUri: IUriRuntimeClass): Authentication_Web_Provider_IWebAccountClientView; safecall;
    function CreateWithPairwiseId(viewType: Authentication_Web_Provider_WebAccountClientViewType; applicationCallbackUri: IUriRuntimeClass; accountPairwiseId: HSTRING): Authentication_Web_Provider_IWebAccountClientView; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IIterator_1__Authentication_Web_Provider_IWebAccountClientView_Base = interface(IInspectable)
  ['{A5984607-661D-5E9C-A0BA-5C7D5F41AF1C}']
    function get_Current: Authentication_Web_Provider_IWebAccountClientView; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAuthentication_Web_Provider_IWebAccountClientView): Cardinal; safecall;
    property Current: Authentication_Web_Provider_IWebAccountClientView read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IIterator_1__Authentication_Web_Provider_IWebAccountClientView = interface(IIterator_1__Authentication_Web_Provider_IWebAccountClientView_Base)
  ['{DE310216-A3AB-5BE0-B9E5-12C407459359}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IIterable_1__Authentication_Web_Provider_IWebAccountClientView_Base = interface(IInspectable)
  ['{610E0F9D-AAE4-54E1-BB0B-1ACA14110ABF}']
    function First: IIterator_1__Authentication_Web_Provider_IWebAccountClientView; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IIterable_1__Authentication_Web_Provider_IWebAccountClientView = interface(IIterable_1__Authentication_Web_Provider_IWebAccountClientView_Base)
  ['{7A564D5E-AAC6-57AE-9236-9992EB17F58A}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>
  IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = interface(IInspectable)
  ['{06CBF02D-8929-5BCD-8398-70BDB051148B}']
    function GetAt(index: Cardinal): Authentication_Web_Provider_IWebAccountClientView; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Authentication_Web_Provider_IWebAccountClientView; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAuthentication_Web_Provider_IWebAccountClientView): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView_Delegate_Base = interface(IUnknown)
  ['{3FA6536F-7E7A-5BC9-B20F-D866CACAF81C}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView_Delegate_Base)
  ['{C679A94A-DDDD-58C8-B07A-1544CBBCB60E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>>
  IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView_Base = interface(IInspectable)
  ['{116827C1-187E-5095-A14B-DF4111C638C2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView; safecall;
    function GetResults: IVectorView_1__Authentication_Web_Provider_IWebAccountClientView; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebAccountClientView>>
  IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView = interface(IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView_Base)
  ['{05EC3908-83ED-58E6-9D7A-4B7E602837A9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountManager)]
  Authentication_Web_Provider_IWebAccountManagerStatics = interface(IInspectable)
  ['{B2E8E1A6-D49A-4032-84BF-1A2847747BF1}']
    function UpdateWebAccountPropertiesAsync(webAccount: IWebAccount; webAccountUserName: HSTRING; additionalProperties: IMapView_2__HSTRING__HSTRING): IAsyncAction; safecall;
    function AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING): IAsyncOperation_1__IWebAccount; safecall;
    function DeleteWebAccountAsync(webAccount: IWebAccount): IAsyncAction; safecall;
    function FindAllProviderWebAccountsAsync: IAsyncOperation_1__IVectorView_1__IWebAccount; safecall;
    function PushCookiesAsync(uri: IUriRuntimeClass; cookies: IVectorView_1__Http_IHttpCookie): IAsyncAction; safecall;
    function SetViewAsync(webAccount: IWebAccount; view: Authentication_Web_Provider_IWebAccountClientView): IAsyncAction; safecall;
    function ClearViewAsync(webAccount: IWebAccount; applicationCallbackUri: IUriRuntimeClass): IAsyncAction; safecall;
    function GetViewsAsync(webAccount: IWebAccount): IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView; safecall;
    function SetWebAccountPictureAsync(webAccount: IWebAccount; webAccountPicture: IRandomAccessStream): IAsyncAction; safecall;
    function ClearWebAccountPictureAsync(webAccount: IWebAccount): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics2
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountManager)]
  Authentication_Web_Provider_IWebAccountManagerStatics2 = interface(IInspectable)
  ['{68A7A829-2D5F-4653-8BB0-BD2FA6BD2D87}']
    function PullCookiesAsync(uriString: HSTRING; callerPFN: HSTRING): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics3
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountManager)]
  Authentication_Web_Provider_IWebAccountManagerStatics3 = interface(IInspectable)
  ['{DD4523A6-8A4F-4AA2-B15E-03F550AF1359}']
    function FindAllProviderWebAccountsForUserAsync(user: IUser): IAsyncOperation_1__IVectorView_1__IWebAccount; safecall;
    function AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING): IAsyncOperation_1__IWebAccount; overload; safecall;
    function AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope): IAsyncOperation_1__IWebAccount; overload; safecall;
    function AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope; perUserWebAccountId: HSTRING): IAsyncOperation_1__IWebAccount; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics4
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountManager)]
  Authentication_Web_Provider_IWebAccountManagerStatics4 = interface(IInspectable)
  ['{59EBC2D2-F7DB-412F-BC3F-F2FEA04430B4}']
    function InvalidateAppCacheForAllAccountsAsync: IAsyncAction; safecall;
    function InvalidateAppCacheForAccountAsync(webAccount: IWebAccount): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountMapManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountManager)]
  Authentication_Web_Provider_IWebAccountMapManagerStatics = interface(IInspectable)
  ['{E8FA446F-3A1B-48A4-8E90-1E59CA6F54DB}']
    function AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope; perUserWebAccountId: HSTRING): IAsyncOperation_1__IWebAccount; safecall;
    function SetPerAppToPerUserAccountAsync(perAppAccount: IWebAccount; perUserWebAccountId: HSTRING): IAsyncAction; safecall;
    function GetPerUserFromPerAppAccountAsync(perAppAccount: IWebAccount): IAsyncOperation_1__IWebAccount; safecall;
    function ClearPerUserFromPerAppAccountAsync(perAppAccount: IWebAccount): IAsyncAction; safecall;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderAddAccountOperation
  Authentication_Web_Provider_IWebAccountProviderAddAccountOperation = interface(IInspectable)
  ['{73EBDCCF-4378-4C79-9335-A5D7AB81594E}']
    procedure ReportCompleted; safecall;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderBaseReportOperation
  Authentication_Web_Provider_IWebAccountProviderBaseReportOperation = interface(IInspectable)
  ['{BBA4ACBB-993B-4D57-BBE4-1421E3668B4C}']
    procedure ReportCompleted; safecall;
    procedure ReportError(value: Authentication_Web_Core_IWebProviderError); safecall;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderDeleteAccountOperation
  Authentication_Web_Provider_IWebAccountProviderDeleteAccountOperation = interface(IInspectable)
  ['{0ABB48B8-9E01-49C9-A355-7D48CAF7D6CA}']
    function get_WebAccount: IWebAccount; safecall;
    property WebAccount: IWebAccount read get_WebAccount;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderManageAccountOperation
  Authentication_Web_Provider_IWebAccountProviderManageAccountOperation = interface(IInspectable)
  ['{ED20DC5C-D21B-463E-A9B7-C1FD0EDAE978}']
    function get_WebAccount: IWebAccount; safecall;
    procedure ReportCompleted; safecall;
    property WebAccount: IWebAccount read get_WebAccount;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderRetrieveCookiesOperation
  Authentication_Web_Provider_IWebAccountProviderRetrieveCookiesOperation = interface(IInspectable)
  ['{5A040441-0FA3-4AB1-A01C-20B110358594}']
    function get_Context: IUriRuntimeClass; safecall;
    function get_Cookies: IVector_1__Http_IHttpCookie; safecall;
    procedure put_Uri(uri: IUriRuntimeClass); safecall;
    function get_Uri: IUriRuntimeClass; safecall;
    function get_ApplicationCallbackUri: IUriRuntimeClass; safecall;
    property ApplicationCallbackUri: IUriRuntimeClass read get_ApplicationCallbackUri;
    property Context: IUriRuntimeClass read get_Context;
    property Cookies: IVector_1__Http_IHttpCookie read get_Cookies;
    property Uri: IUriRuntimeClass read get_Uri write put_Uri;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderSignOutAccountOperation
  Authentication_Web_Provider_IWebAccountProviderSignOutAccountOperation = interface(IInspectable)
  ['{B890E21D-0C55-47BC-8C72-04A6FC7CAC07}']
    function get_WebAccount: IWebAccount; safecall;
    function get_ApplicationCallbackUri: IUriRuntimeClass; safecall;
    function get_ClientId: HSTRING; safecall;
    property ApplicationCallbackUri: IUriRuntimeClass read get_ApplicationCallbackUri;
    property ClientId: HSTRING read get_ClientId;
    property WebAccount: IWebAccount read get_WebAccount;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderSilentReportOperation
  Authentication_Web_Provider_IWebAccountProviderSilentReportOperation = interface(IInspectable)
  ['{E0B545F8-3B0F-44DA-924C-7B18BAAA62A9}']
    procedure ReportUserInteractionRequired; overload; safecall;
    procedure ReportUserInteractionRequired(value: Authentication_Web_Core_IWebProviderError); overload; safecall;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderTokenObjects
  Authentication_Web_Provider_IWebAccountProviderTokenObjects = interface(IInspectable)
  ['{408F284B-1328-42DB-89A4-0BCE7A717D8E}']
    function get_Operation: Authentication_Web_Provider_IWebAccountProviderOperation; safecall;
    property Operation: Authentication_Web_Provider_IWebAccountProviderOperation read get_Operation;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderTokenObjects2
  Authentication_Web_Provider_IWebAccountProviderTokenObjects2 = interface(IInspectable)
  ['{1020B893-5CA5-4FFF-95FB-B820273FC395}']
    function get_User: IUser; safecall;
    property User: IUser read get_User;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenRequest
  Authentication_Web_Provider_IWebProviderTokenRequest = interface(IInspectable)
  ['{1E18778B-8805-454B-9F11-468D2AF1095A}']
    function get_ClientRequest: Authentication_Web_Core_IWebTokenRequest; safecall;
    function get_WebAccounts: IVectorView_1__IWebAccount; safecall;
    function get_WebAccountSelectionOptions: Authentication_Web_Provider_WebAccountSelectionOptions; safecall;
    function get_ApplicationCallbackUri: IUriRuntimeClass; safecall;
    function GetApplicationTokenBindingKeyAsync(keyType: Authentication_Web_TokenBindingKeyType; target: IUriRuntimeClass): IAsyncOperation_1__Core_ICryptographicKey; safecall;
    property ApplicationCallbackUri: IUriRuntimeClass read get_ApplicationCallbackUri;
    property ClientRequest: Authentication_Web_Core_IWebTokenRequest read get_ClientRequest;
    property WebAccountSelectionOptions: Authentication_Web_Provider_WebAccountSelectionOptions read get_WebAccountSelectionOptions;
    property WebAccounts: IVectorView_1__IWebAccount read get_WebAccounts;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebProviderTokenResponse)]
  Authentication_Web_Provider_IWebProviderTokenResponse = interface(IInspectable)
  ['{EF213793-EF55-4186-B7CE-8CB2E7F9849E}']
    function get_ClientResponse: Authentication_Web_Core_IWebTokenResponse; safecall;
    property ClientResponse: Authentication_Web_Core_IWebTokenResponse read get_ClientResponse;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IIterator_1__Authentication_Web_Provider_IWebProviderTokenResponse_Base = interface(IInspectable)
  ['{EB57825D-5AD6-5EE0-8DC6-A53C1E82E3AB}']
    function get_Current: Authentication_Web_Provider_IWebProviderTokenResponse; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAuthentication_Web_Provider_IWebProviderTokenResponse): Cardinal; safecall;
    property Current: Authentication_Web_Provider_IWebProviderTokenResponse read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IIterator_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface(IIterator_1__Authentication_Web_Provider_IWebProviderTokenResponse_Base)
  ['{D4F07AD8-A677-5846-BFE8-035699DF4227}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IIterable_1__Authentication_Web_Provider_IWebProviderTokenResponse_Base = interface(IInspectable)
  ['{E9BAC236-C077-553A-B4AE-B58FB0B89918}']
    function First: IIterator_1__Authentication_Web_Provider_IWebProviderTokenResponse; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IIterable_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface(IIterable_1__Authentication_Web_Provider_IWebProviderTokenResponse_Base)
  ['{A11FFD59-F667-536C-8505-232BCC190D06}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IVectorView_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface(IInspectable)
  ['{DFFEEDD9-AC45-5F45-9034-9546DE605FE9}']
    function GetAt(index: Cardinal): Authentication_Web_Provider_IWebProviderTokenResponse; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Authentication_Web_Provider_IWebProviderTokenResponse; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAuthentication_Web_Provider_IWebProviderTokenResponse): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IVector_1__Authentication_Web_Provider_IWebProviderTokenResponse_Base = interface(IInspectable)
  ['{4E7AD5CF-390F-5ECD-B714-3C654B84CBBA}']
    function GetAt(index: Cardinal): Authentication_Web_Provider_IWebProviderTokenResponse; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Authentication_Web_Provider_IWebProviderTokenResponse; safecall;
    function IndexOf(value: Authentication_Web_Provider_IWebProviderTokenResponse; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Authentication_Web_Provider_IWebProviderTokenResponse); safecall;
    procedure InsertAt(index: Cardinal; value: Authentication_Web_Provider_IWebProviderTokenResponse); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Authentication_Web_Provider_IWebProviderTokenResponse); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAuthentication_Web_Provider_IWebProviderTokenResponse): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PAuthentication_Web_Provider_IWebProviderTokenResponse); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse>
  IVector_1__Authentication_Web_Provider_IWebProviderTokenResponse = interface(IVector_1__Authentication_Web_Provider_IWebProviderTokenResponse_Base)
  ['{F5CDB8F8-CDF3-56BA-81A6-9CF52E23870D}']
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderTokenOperation
  Authentication_Web_Provider_IWebAccountProviderTokenOperation = interface(IInspectable)
  ['{95C613BE-2034-4C38-9434-D26C14B2B4B2}']
    function get_ProviderRequest: Authentication_Web_Provider_IWebProviderTokenRequest; safecall;
    function get_ProviderResponses: IVector_1__Authentication_Web_Provider_IWebProviderTokenResponse; safecall;
    procedure put_CacheExpirationTime(value: DateTime); safecall;
    function get_CacheExpirationTime: DateTime; safecall;
    property CacheExpirationTime: DateTime read get_CacheExpirationTime write put_CacheExpirationTime;
    property ProviderRequest: Authentication_Web_Provider_IWebProviderTokenRequest read get_ProviderRequest;
    property ProviderResponses: IVector_1__Authentication_Web_Provider_IWebProviderTokenResponse read get_ProviderResponses;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebAccountProviderUIReportOperation
  Authentication_Web_Provider_IWebAccountProviderUIReportOperation = interface(IInspectable)
  ['{28FF92D3-8F80-42FB-944F-B2107BBD42E6}']
    procedure ReportUserCanceled; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebAccountScopeManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebAccountManager)]
  Authentication_Web_Provider_IWebAccountScopeManagerStatics = interface(IInspectable)
  ['{5C6CE37C-12B2-423A-BF3D-85B8D7E53656}']
    function AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope): IAsyncOperation_1__IWebAccount; safecall;
    function SetScopeAsync(webAccount: IWebAccount; scope: Authentication_Web_Provider_WebAccountScope): IAsyncAction; safecall;
    function GetScope(webAccount: IWebAccount): Authentication_Web_Provider_WebAccountScope; safecall;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenRequest2
  Authentication_Web_Provider_IWebProviderTokenRequest2 = interface(IInspectable)
  ['{B5D72E4C-10B1-4AA6-88B1-0B6C9E0C1E46}']
    function GetApplicationTokenBindingKeyIdAsync(keyType: Authentication_Web_TokenBindingKeyType; target: IUriRuntimeClass): IAsyncOperation_1__IBuffer; safecall;
  end;

  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenRequest3
  Authentication_Web_Provider_IWebProviderTokenRequest3 = interface(IInspectable)
  ['{1B2716AA-4289-446E-9256-DAFB6F66A51E}']
    function get_ApplicationPackageFamilyName: HSTRING; safecall;
    function get_ApplicationProcessName: HSTRING; safecall;
    function CheckApplicationForCapabilityAsync(capabilityName: HSTRING): IAsyncOperation_1__Boolean; safecall;
    property ApplicationPackageFamilyName: HSTRING read get_ApplicationPackageFamilyName;
    property ApplicationProcessName: HSTRING read get_ApplicationProcessName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponseFactory
  [WinRTClassNameAttribute(SWindows_Security_Authentication_Web_Provider_WebProviderTokenResponse)]
  Authentication_Web_Provider_IWebProviderTokenResponseFactory = interface(IInspectable)
  ['{FA49D99A-25BA-4077-9CFA-9DB4DEA7B71A}']
    function Create(webTokenResponse: Authentication_Web_Core_IWebTokenResponse): Authentication_Web_Provider_IWebProviderTokenResponse; safecall;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  AsyncOperationCompletedHandler_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  IAsyncOperation_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
    function GetResults: Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus read get_Completed write put_Completed;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IInspectable)
  ['{3E882181-FFAB-529C-B56F-3704D4E76A37}']
    function get_Key: HSTRING; safecall;
    function get_Value: Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
    property Key: HSTRING read get_Key;
    property Value: Authorization_AppCapabilityAccess_AppCapabilityAccessStatus read get_Value;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  IIterator_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IInspectable)
  ['{223DA751-0A6C-55A6-A771-0400E1D0C302}']
    function get_Current: IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  IIterable_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IInspectable)
  ['{AEA35BCF-2498-50FE-9FB2-6CEF59B30B2E}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
  end;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>
  IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IInspectable)
  ['{8A182EDF-78BB-553A-B6B7-9112F3CC65BF}']
    function Lookup(key: HSTRING): Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; out second: IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IUnknown)
  ['{A5B8EFED-D254-5D1E-B3E9-63479EDD102D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Security.Authorization.AppCapabilityAccess.AppCapabilityAccessStatus>>
  IAsyncOperation_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus = interface(IInspectable)
  ['{2512D5B8-AF1A-5059-B1DB-B614E707C245}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
    function GetResults: IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Authorization_AppCapabilityAccess_AppCapabilityAccessStatus read get_Completed write put_Completed;
  end;

  // Windows.Foundation.IReference`1<UInt32>
  IReference_1__Cardinal = interface(IInspectable)
  ['{513EF3AF-E784-5325-A91E-97C2B8111CF3}']
    function get_Value: Cardinal; safecall;
    property Value: Cardinal read get_Value;
  end;

  // Windows.Foundation.IReference`1<UInt64>
  IReference_1__UInt64 = interface(IInspectable)
  ['{6755E376-53BB-568B-A11D-17239868309E}']
    function get_Value: UInt64; safecall;
    property Value: UInt64 read get_Value;
  end;

  // Windows.Foundation.IReference`1<UInt8>
  IReference_1__Byte = interface(IInspectable)
  ['{E5198CC8-2873-55F5-B0A1-84FF9E4AAD62}']
    function get_Value: Byte; safecall;
    property Value: Byte read get_Value;
  end;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface(IInspectable)
  ['{98B9ACC1-4B56-532E-AC73-03D5291CCA90}']
    function GetAt(index: Cardinal): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__HSTRING; safecall;
    function IndexOf(value: HSTRING; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: HSTRING); safecall;
    procedure InsertAt(index: Cardinal; value: HSTRING); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: HSTRING); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PHSTRING); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  AsyncOperationCompletedHandler_1__HSTRING = interface(IUnknown)
  ['{B79A741F-7FB5-50AE-9E99-911201EC3D41}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__HSTRING; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<String>
  IAsyncOperation_1__HSTRING = interface(IInspectable)
  ['{3E1FE603-F897-5263-B328-0806426B8A79}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__HSTRING); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__HSTRING; safecall;
    function GetResults: HSTRING; safecall;
    property Completed: AsyncOperationCompletedHandler_1__HSTRING read get_Completed write put_Completed;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.DataProtection.UserDataStorageItemProtectionStatus>
  AsyncOperationCompletedHandler_1__DataProtection_UserDataStorageItemProtectionStatus = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__DataProtection_UserDataStorageItemProtectionStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.Security.DataProtection.UserDataStorageItemProtectionStatus>
  IAsyncOperation_1__DataProtection_UserDataStorageItemProtectionStatus = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__DataProtection_UserDataStorageItemProtectionStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__DataProtection_UserDataStorageItemProtectionStatus; safecall;
    function GetResults: DataProtection_UserDataStorageItemProtectionStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__DataProtection_UserDataStorageItemProtectionStatus read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IDataProtectionInfo
  EnterpriseData_IDataProtectionInfo = interface(IInspectable)
  ['{8420B0C1-5E31-4405-9540-3F943AF0CB26}']
    function get_Status: EnterpriseData_DataProtectionStatus; safecall;
    function get_Identity: HSTRING; safecall;
    property Identity: HSTRING read get_Identity;
    property Status: EnterpriseData_DataProtectionStatus read get_Status;
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IBufferProtectUnprotectResult
  EnterpriseData_IBufferProtectUnprotectResult = interface(IInspectable)
  ['{47995EDC-6CEC-4E3A-B251-9E7485D79E7A}']
    function get_Buffer: IBuffer; safecall;
    function get_ProtectionInfo: EnterpriseData_IDataProtectionInfo; safecall;
    property Buffer: IBuffer read get_Buffer;
    property ProtectionInfo: EnterpriseData_IDataProtectionInfo read get_ProtectionInfo;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IBufferProtectUnprotectResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult_Delegate_Base = interface(IUnknown)
  ['{73D780CB-AD38-59E6-A236-CC0DF69F15A8}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IBufferProtectUnprotectResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult = interface(AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult_Delegate_Base)
  ['{D1341AEB-476E-5F19-BE77-32BA3EF0C3B5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IBufferProtectUnprotectResult>
  IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult_Base = interface(IInspectable)
  ['{11F95A2E-2E87-5449-BCA3-DCDF2AE6E17A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult; safecall;
    function GetResults: EnterpriseData_IBufferProtectUnprotectResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IBufferProtectUnprotectResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IBufferProtectUnprotectResult>
  IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult = interface(IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult_Base)
  ['{7482F267-5E6F-5F4D-AEEA-6E782AAF3980}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IDataProtectionInfo>
  AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo_Delegate_Base = interface(IUnknown)
  ['{E12BB475-3F2B-51C9-83C6-A5661086B6C3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IDataProtectionInfo>
  AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo = interface(AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo_Delegate_Base)
  ['{84BFF12D-AD5A-53DE-8DA4-8F2F213C7C76}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IDataProtectionInfo>
  IAsyncOperation_1__EnterpriseData_IDataProtectionInfo_Base = interface(IInspectable)
  ['{C8EF3F16-DD70-5E88-9323-85DDDDCD2CA5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo; safecall;
    function GetResults: EnterpriseData_IDataProtectionInfo; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IDataProtectionInfo read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IDataProtectionInfo>
  IAsyncOperation_1__EnterpriseData_IDataProtectionInfo = interface(IAsyncOperation_1__EnterpriseData_IDataProtectionInfo_Base)
  ['{76A96925-B906-5D95-A98E-84BA5D758258}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IDataProtectionManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_DataProtectionManager)]
  EnterpriseData_IDataProtectionManagerStatics = interface(IInspectable)
  ['{B6149B74-9144-4EE4-8A8A-30B5F361430E}']
    function ProtectAsync(data: IBuffer; identity: HSTRING): IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult; safecall;
    function UnprotectAsync(data: IBuffer): IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult; safecall;
    function ProtectStreamAsync(unprotectedStream: IInputStream; identity: HSTRING; protectedStream: IOutputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; safecall;
    function UnprotectStreamAsync(protectedStream: IInputStream; unprotectedStream: IOutputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; safecall;
    function GetProtectionInfoAsync(protectedData: IBuffer): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; safecall;
    function GetStreamProtectionInfoAsync(protectedStream: IInputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; safecall;
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IFileProtectionInfo
  EnterpriseData_IFileProtectionInfo = interface(IInspectable)
  ['{4EE96486-147E-4DD0-8FAF-5253ED91AD0C}']
    function get_Status: EnterpriseData_FileProtectionStatus; safecall;
    function get_IsRoamable: Boolean; safecall;
    function get_Identity: HSTRING; safecall;
    property Identity: HSTRING read get_Identity;
    property IsRoamable: Boolean read get_IsRoamable;
    property Status: EnterpriseData_FileProtectionStatus read get_Status;
  end;

  // Windows.Security.EnterpriseData.IFileProtectionInfo2
  EnterpriseData_IFileProtectionInfo2 = interface(IInspectable)
  ['{82123A4C-557A-498D-8E94-944CD5836432}']
    function get_IsProtectWhileOpenSupported: Boolean; safecall;
    property IsProtectWhileOpenSupported: Boolean read get_IsProtectWhileOpenSupported;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IFileProtectionInfo>
  AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo_Delegate_Base = interface(IUnknown)
  ['{2918FFEA-E091-53E2-BDE5-617C9BB3D8FE}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IFileProtectionInfo>
  AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo = interface(AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo_Delegate_Base)
  ['{47807413-C271-59C5-A2F4-08C8A2AEC22D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IFileProtectionInfo>
  IAsyncOperation_1__EnterpriseData_IFileProtectionInfo_Base = interface(IInspectable)
  ['{67E04591-2D11-5018-9FF0-0B85520B888B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo; safecall;
    function GetResults: EnterpriseData_IFileProtectionInfo; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IFileProtectionInfo read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IFileProtectionInfo>
  IAsyncOperation_1__EnterpriseData_IFileProtectionInfo = interface(IAsyncOperation_1__EnterpriseData_IFileProtectionInfo_Base)
  ['{989A307F-4554-565E-A161-D20734D4522E}']
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectedContainerExportResult
  EnterpriseData_IProtectedContainerExportResult = interface(IInspectable)
  ['{3948EF95-F7FB-4B42-AFB0-DF70B41543C1}']
    function get_Status: EnterpriseData_ProtectedImportExportStatus; safecall;
    function get_File: IStorageFile; safecall;
    property &File: IStorageFile read get_File;
    property Status: EnterpriseData_ProtectedImportExportStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedContainerExportResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult_Delegate_Base = interface(IUnknown)
  ['{560C5521-5008-5272-A766-941F70718BC6}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedContainerExportResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult = interface(AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult_Delegate_Base)
  ['{B3C8B728-2CDF-5BC6-B75E-5F2BA085988D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedContainerExportResult>
  IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult_Base = interface(IInspectable)
  ['{C99F0B2E-67AD-518A-866F-E39B4567B110}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult; safecall;
    function GetResults: EnterpriseData_IProtectedContainerExportResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerExportResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedContainerExportResult>
  IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult = interface(IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult_Base)
  ['{30D5918D-F3DB-5249-B81E-D369047AF8A8}']
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectedContainerImportResult
  EnterpriseData_IProtectedContainerImportResult = interface(IInspectable)
  ['{CDB780D1-E7BB-4D1A-9339-34DC41149F9B}']
    function get_Status: EnterpriseData_ProtectedImportExportStatus; safecall;
    function get_File: IStorageFile; safecall;
    property &File: IStorageFile read get_File;
    property Status: EnterpriseData_ProtectedImportExportStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedContainerImportResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult_Delegate_Base = interface(IUnknown)
  ['{CFEA00F8-D2AD-5AD2-A396-E4F71B9A7C3F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedContainerImportResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult = interface(AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult_Delegate_Base)
  ['{6216C4E3-8995-531A-8629-149D190A7C37}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedContainerImportResult>
  IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult_Base = interface(IInspectable)
  ['{384E9499-D491-5297-BEBA-B33A3D67F207}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult; safecall;
    function GetResults: EnterpriseData_IProtectedContainerImportResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedContainerImportResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedContainerImportResult>
  IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult = interface(IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult_Base)
  ['{9B7EBD1A-6A2E-5D10-B342-B658A0039154}']
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectedFileCreateResult
  EnterpriseData_IProtectedFileCreateResult = interface(IInspectable)
  ['{28E3ED6A-E9E7-4A03-9F53-BDB16172699B}']
    function get_File: IStorageFile; safecall;
    function get_Stream: IRandomAccessStream; safecall;
    function get_ProtectionInfo: EnterpriseData_IFileProtectionInfo; safecall;
    property &File: IStorageFile read get_File;
    property ProtectionInfo: EnterpriseData_IFileProtectionInfo read get_ProtectionInfo;
    property Stream: IRandomAccessStream read get_Stream;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedFileCreateResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult_Delegate_Base = interface(IUnknown)
  ['{6C44A868-7C80-5187-A08D-379BE41909F6}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.IProtectedFileCreateResult>
  AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult = interface(AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult_Delegate_Base)
  ['{432F8879-BB8B-5DE7-8003-B4588DCAD2E6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedFileCreateResult>
  IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult_Base = interface(IInspectable)
  ['{D7F98A9C-738E-50F6-93E3-2608BAF54BE9}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult; safecall;
    function GetResults: EnterpriseData_IProtectedFileCreateResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_IProtectedFileCreateResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.IProtectedFileCreateResult>
  IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult = interface(IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult_Base)
  ['{D2A15153-2B9D-5F4B-A384-365C1B8973EA}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IFileProtectionManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_FileProtectionManager)]
  EnterpriseData_IFileProtectionManagerStatics = interface(IInspectable)
  ['{5846FC9B-E613-426B-BB38-88CBA1DC9ADB}']
    function ProtectAsync(target: IStorageItem; identity: HSTRING): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; safecall;
    function CopyProtectionAsync(source: IStorageItem; target: IStorageItem): IAsyncOperation_1__Boolean; safecall;
    function GetProtectionInfoAsync(source: IStorageItem): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; safecall;
    function SaveFileAsContainerAsync(protectedFile: IStorageFile): IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult; safecall;
    function LoadFileFromContainerAsync(containerFile: IStorageFile): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult; overload; safecall;
    function LoadFileFromContainerAsync(containerFile: IStorageFile; target: IStorageItem): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult; overload; safecall;
    function CreateProtectedAndOpenAsync(parentFolder: IStorageFolder; desiredName: HSTRING; identity: HSTRING; collisionOption: CreationCollisionOption): IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IFileProtectionManagerStatics2
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_FileProtectionManager)]
  EnterpriseData_IFileProtectionManagerStatics2 = interface(IInspectable)
  ['{83D2A745-0483-41AB-B2D5-BC7F23D74EBB}']
    function IsContainerAsync(&file: IStorageFile): IAsyncOperation_1__Boolean; safecall;
    function LoadFileFromContainerAsync(containerFile: IStorageFile; target: IStorageItem; collisionOption: NameCollisionOption): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult; safecall;
    function SaveFileAsContainerAsync(protectedFile: IStorageFile; sharedWithIdentities: IIterable_1__HSTRING): IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IFileUnprotectOptions
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_FileUnprotectOptions)]
  EnterpriseData_IFileUnprotectOptions = interface(IInspectable)
  ['{7D1312F1-3B0D-4DD8-A1F8-1EC53822E2F3}']
    procedure put_Audit(value: Boolean); safecall;
    function get_Audit: Boolean; safecall;
    property Audit: Boolean read get_Audit write put_Audit;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IFileProtectionManagerStatics3
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_FileProtectionManager)]
  EnterpriseData_IFileProtectionManagerStatics3 = interface(IInspectable)
  ['{6918849A-624F-46D6-B241-E9CD5FDF3E3F}']
    function UnprotectAsync(target: IStorageItem): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; overload; safecall;
    function UnprotectAsync(target: IStorageItem; options: EnterpriseData_IFileUnprotectOptions): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; overload; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.FileProtectionStatus>
  AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus_Delegate_Base = interface(IUnknown)
  ['{E104F25B-B957-5ED4-B1C5-1993604CFEAE}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__EnterpriseData_FileProtectionStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Security.EnterpriseData.FileProtectionStatus>
  AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus = interface(AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.FileProtectionStatus>
  IAsyncOperation_1__EnterpriseData_FileProtectionStatus_Base = interface(IInspectable)
  ['{033EFE7C-709E-53A8-8E64-CDAB6BD1ED59}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus; safecall;
    function GetResults: EnterpriseData_FileProtectionStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__EnterpriseData_FileProtectionStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Security.EnterpriseData.FileProtectionStatus>
  IAsyncOperation_1__EnterpriseData_FileProtectionStatus = interface(IAsyncOperation_1__EnterpriseData_FileProtectionStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IFileRevocationManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_FileRevocationManager)]
  EnterpriseData_IFileRevocationManagerStatics = interface(IInspectable)
  ['{256BBC3D-1C5D-4260-8C75-9144CFB78BA9}']
    function ProtectAsync(storageItem: IStorageItem; enterpriseIdentity: HSTRING): IAsyncOperation_1__EnterpriseData_FileProtectionStatus; safecall;
    function CopyProtectionAsync(sourceStorageItem: IStorageItem; targetStorageItem: IStorageItem): IAsyncOperation_1__Boolean; safecall;
    procedure Revoke(enterpriseIdentity: HSTRING); safecall;
    function GetStatusAsync(storageItem: IStorageItem): IAsyncOperation_1__EnterpriseData_FileProtectionStatus; safecall;
  end deprecated;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IFileUnprotectOptionsFactory
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_FileUnprotectOptions)]
  EnterpriseData_IFileUnprotectOptionsFactory = interface(IInspectable)
  ['{51AEB39C-DA8C-4C3F-9BFB-CB73A7CCE0DD}']
    function Create(audit: Boolean): EnterpriseData_IFileUnprotectOptions; safecall;
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectedAccessResumedEventArgs
  EnterpriseData_IProtectedAccessResumedEventArgs = interface(IInspectable)
  ['{AC4DCA59-5D80-4E95-8C5F-8539450EEBE0}']
    function get_Identities: IVectorView_1__HSTRING; safecall;
    property Identities: IVectorView_1__HSTRING read get_Identities;
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectedAccessSuspendingEventArgs
  EnterpriseData_IProtectedAccessSuspendingEventArgs = interface(IInspectable)
  ['{75A193E0-A344-429F-B975-04FC1F88C185}']
    function get_Identities: IVectorView_1__HSTRING; safecall;
    function get_Deadline: DateTime; safecall;
    function GetDeferral: IDeferral; safecall;
    property Deadline: DateTime read get_Deadline;
    property Identities: IVectorView_1__HSTRING read get_Identities;
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectedContentRevokedEventArgs
  EnterpriseData_IProtectedContentRevokedEventArgs = interface(IInspectable)
  ['{63686821-58B9-47EE-93D9-F0F741CF43F0}']
    function get_Identities: IVectorView_1__HSTRING; safecall;
    property Identities: IVectorView_1__HSTRING read get_Identities;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyAuditInfo
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_ProtectionPolicyAuditInfo)]
  EnterpriseData_IProtectionPolicyAuditInfo = interface(IInspectable)
  ['{425AB7E4-FEB7-44FC-B3BB-C3C4D7ECBEBB}']
    procedure put_Action(value: EnterpriseData_ProtectionPolicyAuditAction); safecall;
    function get_Action: EnterpriseData_ProtectionPolicyAuditAction; safecall;
    procedure put_DataDescription(value: HSTRING); safecall;
    function get_DataDescription: HSTRING; safecall;
    procedure put_SourceDescription(value: HSTRING); safecall;
    function get_SourceDescription: HSTRING; safecall;
    procedure put_TargetDescription(value: HSTRING); safecall;
    function get_TargetDescription: HSTRING; safecall;
    property Action: EnterpriseData_ProtectionPolicyAuditAction read get_Action write put_Action;
    property DataDescription: HSTRING read get_DataDescription write put_DataDescription;
    property SourceDescription: HSTRING read get_SourceDescription write put_SourceDescription;
    property TargetDescription: HSTRING read get_TargetDescription write put_TargetDescription;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyAuditInfoFactory
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_ProtectionPolicyAuditInfo)]
  EnterpriseData_IProtectionPolicyAuditInfoFactory = interface(IInspectable)
  ['{7ED4180B-92E8-42D5-83D4-25440B423549}']
    function Create(action: EnterpriseData_ProtectionPolicyAuditAction; dataDescription: HSTRING; sourceDescription: HSTRING; targetDescription: HSTRING): EnterpriseData_IProtectionPolicyAuditInfo; safecall;
    function CreateWithActionAndDataDescription(action: EnterpriseData_ProtectionPolicyAuditAction; dataDescription: HSTRING): EnterpriseData_IProtectionPolicyAuditInfo; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyManager
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_ProtectionPolicyManager)]
  EnterpriseData_IProtectionPolicyManager = interface(IInspectable)
  ['{D5703E18-A08D-47E6-A240-9934D7165EB5}']
    procedure put_Identity(value: HSTRING); safecall;
    function get_Identity: HSTRING; safecall;
    property Identity: HSTRING read get_Identity write put_Identity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyManager2
  EnterpriseData_IProtectionPolicyManager2 = interface(IInspectable)
  ['{ABF7527A-8435-417F-99B6-51BEAF365888}']
    procedure put_ShowEnterpriseIndicator(value: Boolean); safecall;
    function get_ShowEnterpriseIndicator: Boolean; safecall;
    property ShowEnterpriseIndicator: Boolean read get_ShowEnterpriseIndicator write put_ShowEnterpriseIndicator;
  end;

  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IThreadNetworkContext
  EnterpriseData_IThreadNetworkContext = interface(IInspectable)
  ['{FA4EA8E9-EF13-405A-B12C-D7348C6F41FC}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedAccessSuspendingEventArgs>
  EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs_Delegate_Base = interface(IUnknown)
  ['{FE8BB4D8-0D3D-5416-B390-9EAD5DD9B384}']
    procedure Invoke(sender: IInspectable; args: EnterpriseData_IProtectedAccessSuspendingEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedAccessSuspendingEventArgs>
  EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs = interface(EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs_Delegate_Base)
  ['{D8E44E77-2784-5E85-8F85-E53D60557020}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedAccessResumedEventArgs>
  EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs_Delegate_Base = interface(IUnknown)
  ['{9EFE4D36-6549-5222-9BC2-4D5D929D005C}']
    procedure Invoke(sender: IInspectable; args: EnterpriseData_IProtectedAccessResumedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedAccessResumedEventArgs>
  EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs = interface(EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs_Delegate_Base)
  ['{1496933D-E4C9-5242-BA9D-3AE36BE32697}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedContentRevokedEventArgs>
  EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs_Delegate_Base = interface(IUnknown)
  ['{4FF76357-6805-573E-A67D-2C594F5004B7}']
    procedure Invoke(sender: IInspectable; args: EnterpriseData_IProtectedContentRevokedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Security.EnterpriseData.IProtectedContentRevokedEventArgs>
  EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs = interface(EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs_Delegate_Base)
  ['{D4BE21D1-DC3D-526C-B8FF-009700227E82}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_ProtectionPolicyManager)]
  EnterpriseData_IProtectionPolicyManagerStatics = interface(IInspectable)
  ['{C0BFFC66-8C3D-4D56-8804-C68F0AD32EC5}']
    function IsIdentityManaged(identity: HSTRING): Boolean; safecall;
    function TryApplyProcessUIPolicy(identity: HSTRING): Boolean; safecall;
    procedure ClearProcessUIPolicy; safecall;
    function CreateCurrentThreadNetworkContext(identity: HSTRING): EnterpriseData_IThreadNetworkContext; safecall;
    function GetPrimaryManagedIdentityForNetworkEndpointAsync(endpointHost: IHostName): IAsyncOperation_1__HSTRING; safecall;
    procedure RevokeContent(identity: HSTRING); safecall;
    function GetForCurrentView: EnterpriseData_IProtectionPolicyManager; safecall;
    function add_ProtectedAccessSuspending(handler: EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs): EventRegistrationToken; safecall;
    procedure remove_ProtectedAccessSuspending(token: EventRegistrationToken); safecall;
    function add_ProtectedAccessResumed(handler: EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ProtectedAccessResumed(token: EventRegistrationToken); safecall;
    function add_ProtectedContentRevoked(handler: EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ProtectedContentRevoked(token: EventRegistrationToken); safecall;
    function CheckAccess(sourceIdentity: HSTRING; targetIdentity: HSTRING): EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
    function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
  end;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface(IUnknown)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
    procedure Invoke(sender: IInspectable; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics2
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_ProtectionPolicyManager)]
  EnterpriseData_IProtectionPolicyManagerStatics2 = interface(IInspectable)
  ['{B68F9A8C-39E0-4649-B2E4-070AB8A579B3}']
    function HasContentBeenRevokedSince(identity: HSTRING; since: DateTime): Boolean; safecall;
    function CheckAccessForApp(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING): EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
    function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
    function GetEnforcementLevel(identity: HSTRING): EnterpriseData_EnforcementLevel; safecall;
    function IsUserDecryptionAllowed(identity: HSTRING): Boolean; safecall;
    function IsProtectionUnderLockRequired(identity: HSTRING): Boolean; safecall;
    function add_PolicyChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_PolicyChanged(token: EventRegistrationToken); safecall;
    function get_IsProtectionEnabled: Boolean; safecall;
    property IsProtectionEnabled: Boolean read get_IsProtectionEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics3
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_ProtectionPolicyManager)]
  EnterpriseData_IProtectionPolicyManagerStatics3 = interface(IInspectable)
  ['{48FF9E8C-6A6F-4D9F-BCED-18AB537AA015}']
    function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    procedure LogAuditEvent(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics4
  [WinRTClassNameAttribute(SWindows_Security_EnterpriseData_ProtectionPolicyManager)]
  EnterpriseData_IProtectionPolicyManagerStatics4 = interface(IInspectable)
  ['{20B794DB-CCBD-490F-8C83-49CCB77AEA6C}']
    function IsRoamableProtectionEnabled(identity: HSTRING): Boolean; safecall;
    function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
    function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
    function RequestAccessToFilesForAppAsync(sourceItemList: IIterable_1__IStorageItem; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function RequestAccessToFilesForAppAsync(sourceItemList: IIterable_1__IStorageItem; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function RequestAccessToFilesForProcessAsync(sourceItemList: IIterable_1__IStorageItem; processId: Cardinal; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function RequestAccessToFilesForProcessAsync(sourceItemList: IIterable_1__IStorageItem; processId: Cardinal; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function IsFileProtectionRequiredAsync(target: IStorageItem; identity: HSTRING): IAsyncOperation_1__Boolean; safecall;
    function IsFileProtectionRequiredForNewFileAsync(parentFolder: IStorageFolder; identity: HSTRING; desiredName: HSTRING): IAsyncOperation_1__Boolean; safecall;
    function get_PrimaryManagedIdentity: HSTRING; safecall;
    function GetPrimaryManagedIdentityForIdentity(identity: HSTRING): HSTRING; safecall;
    property PrimaryManagedIdentity: HSTRING read get_PrimaryManagedIdentity;
  end;

  // Windows.Foundation.Collections.IIterator`1<Object>
  IIterator_1__IInspectable = interface(IInspectable)
  ['{44A94F2D-04F8-5091-B336-BE7892DD10BE}']
    function get_Current: IInspectable; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIInspectable): Cardinal; safecall;
    property Current: IInspectable read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Object>
  IIterable_1__IInspectable = interface(IInspectable)
  ['{092B849B-60B1-52BE-A44A-6FE8E933CBE4}']
    function First: IIterator_1__IInspectable; safecall;
  end;

  // Windows.Security.Isolation.HostMessageReceivedCallback
  Isolation_HostMessageReceivedCallback = interface(IUnknown)
  ['{FAF26FFA-8CE1-4CC1-B278-322D31A5E4A3}']
    procedure Invoke(receiverId: TGuid; &message: IVectorView_1__IInspectable); safecall;
  end;

  // Windows.Security.Isolation.MessageReceivedCallback
  Isolation_MessageReceivedCallback = interface(IUnknown)
  ['{F5B4C8FF-1D9D-4995-9FEA-4D15257C0757}']
    procedure Invoke(receiverId: TGuid; &message: IVectorView_1__IInspectable); safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Security.Isolation.IsolatedWindowsEnvironmentHostError>
  IIterator_1__Isolation_IsolatedWindowsEnvironmentHostError = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: Isolation_IsolatedWindowsEnvironmentHostError; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIsolation_IsolatedWindowsEnvironmentHostError): Cardinal; safecall;
    property Current: Isolation_IsolatedWindowsEnvironmentHostError read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Security.Isolation.IsolatedWindowsEnvironmentHostError>
  IIterable_1__Isolation_IsolatedWindowsEnvironmentHostError = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__Isolation_IsolatedWindowsEnvironmentHostError; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Security.Isolation.IsolatedWindowsEnvironmentHostError>
  IVectorView_1__Isolation_IsolatedWindowsEnvironmentHostError = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Isolation_IsolatedWindowsEnvironmentHostError; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Isolation_IsolatedWindowsEnvironmentHostError; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIsolation_IsolatedWindowsEnvironmentHostError): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Security.Authentication.Identity.EnterpriseKeyCredentialRegistrationManager
  // DualAPI
  // Implements: Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationManager
  // Statics: "Windows.Security.Authentication.Identity.IEnterpriseKeyCredentialRegistrationManagerStatics"
  TAuthentication_Identity_EnterpriseKeyCredentialRegistrationManager = class(TWinRTGenericImportS<Authentication_Identity_IEnterpriseKeyCredentialRegistrationManagerStatics>)
  public
    // -> Authentication_Identity_IEnterpriseKeyCredentialRegistrationManagerStatics
    class function get_Current: Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager; static; inline;
    class property Current: Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager read get_Current;
  end;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorAuthentication
  // DualAPI
  // Implements: Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthentication
  // Statics: "Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorAuthenticationStatics"
  TAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication = class(TWinRTGenericImportS<Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStatics>)
  public
    // -> Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStatics
    class function ShowNotificationMessageAsync(deviceName: HSTRING; &message: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationMessage): IAsyncAction; static; inline;
    class function StartAuthenticationAsync(deviceId: HSTRING; serviceAuthenticationNonce: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult; static; inline;
    class function add_AuthenticationStageChanged(handler: EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_AuthenticationStageChanged(token: EventRegistrationToken); static; inline;
    class function GetAuthenticationStageInfoAsync: IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo; static; inline;
  end;

  // Windows.Security.Authentication.Identity.Provider.SecondaryAuthenticationFactorRegistration
  // DualAPI
  // Implements: Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistration
  // Statics: "Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics"
  // Statics: "Windows.Security.Authentication.Identity.Provider.ISecondaryAuthenticationFactorRegistrationStatics"
  TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration = class(TWinRTGenericImportS2<Authentication_Identity_Provider_ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics, Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationStatics>)
  public
    // -> Authentication_Identity_Provider_ISecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatics
    class function RegisterDevicePresenceMonitoringAsync(deviceId: HSTRING; deviceInstancePath: HSTRING; monitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode): IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus; overload; static; inline;
    class function RegisterDevicePresenceMonitoringAsync(deviceId: HSTRING; deviceInstancePath: HSTRING; monitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode; deviceFriendlyName: HSTRING; deviceModelNumber: HSTRING; deviceConfigurationData: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus; overload; static; inline;
    class function UnregisterDevicePresenceMonitoringAsync(deviceId: HSTRING): IAsyncAction; static; inline;
    class function IsDevicePresenceMonitoringSupported: Boolean; static; inline;

    // -> Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationStatics
    class function RequestStartRegisteringDeviceAsync(deviceId: HSTRING; capabilities: Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceCapabilities; deviceFriendlyName: HSTRING; deviceModelNumber: HSTRING; deviceKey: IBuffer; mutualAuthenticationKey: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult; static; inline;
    class function FindAllRegisteredDeviceInfoAsync(queryType: Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceFindScope): IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo; static; inline;
    class function UnregisterDeviceAsync(deviceId: HSTRING): IAsyncAction; static; inline;
    class function UpdateDeviceConfigurationDataAsync(deviceId: HSTRING; deviceConfigurationData: IBuffer): IAsyncAction; static; inline;
  end;

  // Windows.Security.Authentication.Web.Core.WebAuthenticationCoreManager
  // DualAPI
  // Statics: "Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics"
  // Statics: "Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics2"
  // Statics: "Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics3"
  // Statics: "Windows.Security.Authentication.Web.Core.IWebAuthenticationCoreManagerStatics4"
  // Interop Intf: "IWebAuthenticationCoreManagerInterop"
  IWebAuthenticationCoreManagerInterop = interface(IInspectable)
  ['{F4B8E804-811E-4436-B69C-44CB67B72084}']
    function RequestTokenForWindowAsync(appWindow: THandle; request: IInspectable; const riid: TGUID): IAsyncInfo; safecall;
    function RequestTokenWithWebAccountForWindowAsync(appWindow: THandle; request, webAccount: IInspectable; const riid: TGUID): IAsyncInfo; safecall;
  end;
  TAuthentication_Web_Core_WebAuthenticationCoreManager = class(TWinRTGenericImportS4O<Authentication_Web_Core_IWebAuthenticationCoreManagerStatics, Authentication_Web_Core_IWebAuthenticationCoreManagerStatics2, Authentication_Web_Core_IWebAuthenticationCoreManagerStatics3, Authentication_Web_Core_IWebAuthenticationCoreManagerStatics4, IWebAuthenticationCoreManagerInterop>)
  public
    // -> Authentication_Web_Core_IWebAuthenticationCoreManagerStatics
    class function GetTokenSilentlyAsync(request: Authentication_Web_Core_IWebTokenRequest): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; static; inline;
    class function GetTokenSilentlyAsync(request: Authentication_Web_Core_IWebTokenRequest; webAccount: IWebAccount): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; static; inline;
    class function RequestTokenAsync(request: Authentication_Web_Core_IWebTokenRequest): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; static; inline;
    class function RequestTokenAsync(request: Authentication_Web_Core_IWebTokenRequest; webAccount: IWebAccount): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult; overload; static; inline;
    class function FindAccountAsync(provider: IWebAccountProvider; webAccountId: HSTRING): IAsyncOperation_1__IWebAccount; static; inline;
    class function FindAccountProviderAsync(webAccountProviderId: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; static; inline;
    class function FindAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; static; inline;

    // -> Authentication_Web_Core_IWebAuthenticationCoreManagerStatics2
    class function FindAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING; user: IUser): IAsyncOperation_1__IWebAccountProvider; overload; static; inline;

    // -> Authentication_Web_Core_IWebAuthenticationCoreManagerStatics3
    class function CreateWebAccountMonitor(webAccounts: IIterable_1__IWebAccount): Authentication_Web_Core_IWebAccountMonitor; static; inline;

    // -> Authentication_Web_Core_IWebAuthenticationCoreManagerStatics4
    class function FindAllAccountsAsync(provider: IWebAccountProvider): IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult; overload; static; inline;
    class function FindAllAccountsAsync(provider: IWebAccountProvider; clientId: HSTRING): IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult; overload; static; inline;
    class function FindSystemAccountProviderAsync(webAccountProviderId: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; static; inline;
    class function FindSystemAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING): IAsyncOperation_1__IWebAccountProvider; overload; static; inline;
    class function FindSystemAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING; user: IUser): IAsyncOperation_1__IWebAccountProvider; overload; static; inline;
  end;

  // Windows.Security.Authentication.Web.Core.WebProviderError
  // DualAPI
  // Implements: Windows.Security.Authentication.Web.Core.IWebProviderError
  // Factory: "Windows.Security.Authentication.Web.Core.IWebProviderErrorFactory"
  TAuthentication_Web_Core_WebProviderError = class(TWinRTGenericImportF<Authentication_Web_Core_IWebProviderErrorFactory>)
  public
    // -> Authentication_Web_Core_IWebProviderErrorFactory
    class function Create(errorCode: Cardinal; errorMessage: HSTRING): Authentication_Web_Core_IWebProviderError; static; inline;
  end;

  // Windows.Security.Authentication.Web.Core.WebTokenRequest
  // DualAPI
  // Implements: Windows.Security.Authentication.Web.Core.IWebTokenRequest
  // Implements: Windows.Security.Authentication.Web.Core.IWebTokenRequest2
  // Implements: Windows.Security.Authentication.Web.Core.IWebTokenRequest3
  // Factory: "Windows.Security.Authentication.Web.Core.IWebTokenRequestFactory"
  TAuthentication_Web_Core_WebTokenRequest = class(TWinRTGenericImportF<Authentication_Web_Core_IWebTokenRequestFactory>)
  public
    // -> Authentication_Web_Core_IWebTokenRequestFactory
    class function Create(provider: IWebAccountProvider; scope: HSTRING; clientId: HSTRING): Authentication_Web_Core_IWebTokenRequest; static; inline;
    class function CreateWithPromptType(provider: IWebAccountProvider; scope: HSTRING; clientId: HSTRING; promptType: Authentication_Web_Core_WebTokenRequestPromptType): Authentication_Web_Core_IWebTokenRequest; static; inline;
    class function CreateWithProvider(provider: IWebAccountProvider): Authentication_Web_Core_IWebTokenRequest; static; inline;
    class function CreateWithScope(provider: IWebAccountProvider; scope: HSTRING): Authentication_Web_Core_IWebTokenRequest; static; inline;
  end;

  // Windows.Security.Authentication.Web.Core.WebTokenResponse
  // DualAPI
  // Implements: Windows.Security.Authentication.Web.Core.IWebTokenResponse
  // Factory: "Windows.Security.Authentication.Web.Core.IWebTokenResponseFactory"
  // Instantiable: "Authentication_Web_Core_IWebTokenResponse"
  TAuthentication_Web_Core_WebTokenResponse = class(TWinRTGenericImportFI<Authentication_Web_Core_IWebTokenResponseFactory, Authentication_Web_Core_IWebTokenResponse>)
  public
    // -> Authentication_Web_Core_IWebTokenResponseFactory
    class function CreateWithToken(token: HSTRING): Authentication_Web_Core_IWebTokenResponse; static; inline;
    class function CreateWithTokenAndAccount(token: HSTRING; webAccount: IWebAccount): Authentication_Web_Core_IWebTokenResponse; static; inline;
    class function CreateWithTokenAccountAndError(token: HSTRING; webAccount: IWebAccount; error: Authentication_Web_Core_IWebProviderError): Authentication_Web_Core_IWebTokenResponse; static; inline;
  end;

  // Windows.Security.Authentication.Web.Provider.WebAccountClientView
  // DualAPI
  // Implements: Windows.Security.Authentication.Web.Provider.IWebAccountClientView
  // Factory: "Windows.Security.Authentication.Web.Provider.IWebAccountClientViewFactory"
  TAuthentication_Web_Provider_WebAccountClientView = class(TWinRTGenericImportF<Authentication_Web_Provider_IWebAccountClientViewFactory>)
  public
    // -> Authentication_Web_Provider_IWebAccountClientViewFactory
    class function Create(viewType: Authentication_Web_Provider_WebAccountClientViewType; applicationCallbackUri: IUriRuntimeClass): Authentication_Web_Provider_IWebAccountClientView; static; inline;
    class function CreateWithPairwiseId(viewType: Authentication_Web_Provider_WebAccountClientViewType; applicationCallbackUri: IUriRuntimeClass; accountPairwiseId: HSTRING): Authentication_Web_Provider_IWebAccountClientView; static; inline;
  end;

  // Windows.Security.Authentication.Web.Provider.WebAccountManager
  // DualAPI
  // Statics: "Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics"
  // Statics: "Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics2"
  // Statics: "Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics3"
  // Statics: "Windows.Security.Authentication.Web.Provider.IWebAccountManagerStatics4"
  // Statics: "Windows.Security.Authentication.Web.Provider.IWebAccountMapManagerStatics"
  // Statics: "Windows.Security.Authentication.Web.Provider.IWebAccountScopeManagerStatics"
  TAuthentication_Web_Provider_WebAccountManager = class(TWinRTGenericImportS6<Authentication_Web_Provider_IWebAccountManagerStatics, Authentication_Web_Provider_IWebAccountManagerStatics2, Authentication_Web_Provider_IWebAccountManagerStatics3, Authentication_Web_Provider_IWebAccountManagerStatics4, Authentication_Web_Provider_IWebAccountMapManagerStatics, Authentication_Web_Provider_IWebAccountScopeManagerStatics>)
  public
    // -> Authentication_Web_Provider_IWebAccountManagerStatics
    class function UpdateWebAccountPropertiesAsync(webAccount: IWebAccount; webAccountUserName: HSTRING; additionalProperties: IMapView_2__HSTRING__HSTRING): IAsyncAction; static; inline;
    class function AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING): IAsyncOperation_1__IWebAccount; overload; static; inline;
    class function DeleteWebAccountAsync(webAccount: IWebAccount): IAsyncAction; static; inline;
    class function FindAllProviderWebAccountsAsync: IAsyncOperation_1__IVectorView_1__IWebAccount; static; inline;
    class function PushCookiesAsync(uri: IUriRuntimeClass; cookies: IVectorView_1__Http_IHttpCookie): IAsyncAction; static; inline;
    class function SetViewAsync(webAccount: IWebAccount; view: Authentication_Web_Provider_IWebAccountClientView): IAsyncAction; static; inline;
    class function ClearViewAsync(webAccount: IWebAccount; applicationCallbackUri: IUriRuntimeClass): IAsyncAction; static; inline;
    class function GetViewsAsync(webAccount: IWebAccount): IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView; static; inline;
    class function SetWebAccountPictureAsync(webAccount: IWebAccount; webAccountPicture: IRandomAccessStream): IAsyncAction; static; inline;
    class function ClearWebAccountPictureAsync(webAccount: IWebAccount): IAsyncAction; static; inline;

    // -> Authentication_Web_Provider_IWebAccountManagerStatics2
    class function PullCookiesAsync(uriString: HSTRING; callerPFN: HSTRING): IAsyncAction; static; inline;

    // -> Authentication_Web_Provider_IWebAccountManagerStatics3
    class function FindAllProviderWebAccountsForUserAsync(user: IUser): IAsyncOperation_1__IVectorView_1__IWebAccount; static; inline;
    class function AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING): IAsyncOperation_1__IWebAccount; overload; static; inline;
    class function AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope): IAsyncOperation_1__IWebAccount; overload; static; inline;
    class function AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope; perUserWebAccountId: HSTRING): IAsyncOperation_1__IWebAccount; overload; static; inline;

    // -> Authentication_Web_Provider_IWebAccountManagerStatics4
    class function InvalidateAppCacheForAllAccountsAsync: IAsyncAction; static; inline;
    class function InvalidateAppCacheForAccountAsync(webAccount: IWebAccount): IAsyncAction; static; inline;

    // -> Authentication_Web_Provider_IWebAccountMapManagerStatics
    class function AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope; perUserWebAccountId: HSTRING): IAsyncOperation_1__IWebAccount; overload; static; inline;
    class function SetPerAppToPerUserAccountAsync(perAppAccount: IWebAccount; perUserWebAccountId: HSTRING): IAsyncAction; static; inline;
    class function GetPerUserFromPerAppAccountAsync(perAppAccount: IWebAccount): IAsyncOperation_1__IWebAccount; static; inline;
    class function ClearPerUserFromPerAppAccountAsync(perAppAccount: IWebAccount): IAsyncAction; static; inline;

    // -> Authentication_Web_Provider_IWebAccountScopeManagerStatics
    class function AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope): IAsyncOperation_1__IWebAccount; overload; static; inline;
    class function SetScopeAsync(webAccount: IWebAccount; scope: Authentication_Web_Provider_WebAccountScope): IAsyncAction; static; inline;
    class function GetScope(webAccount: IWebAccount): Authentication_Web_Provider_WebAccountScope; static; inline;
  end;

  // Windows.Security.Authentication.Web.Provider.WebProviderTokenResponse
  // DualAPI
  // Implements: Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponse
  // Factory: "Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponseFactory"
  TAuthentication_Web_Provider_WebProviderTokenResponse = class(TWinRTGenericImportF<Authentication_Web_Provider_IWebProviderTokenResponseFactory>)
  public
    // -> Authentication_Web_Provider_IWebProviderTokenResponseFactory
    class function Create(webTokenResponse: Authentication_Web_Core_IWebTokenResponse): Authentication_Web_Provider_IWebProviderTokenResponse; static; inline;
  end;

  // Windows.Security.EnterpriseData.DataProtectionManager
  // DualAPI
  // Statics: "Windows.Security.EnterpriseData.IDataProtectionManagerStatics"
  TEnterpriseData_DataProtectionManager = class(TWinRTGenericImportS<EnterpriseData_IDataProtectionManagerStatics>)
  public
    // -> EnterpriseData_IDataProtectionManagerStatics
    class function ProtectAsync(data: IBuffer; identity: HSTRING): IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult; static; inline;
    class function UnprotectAsync(data: IBuffer): IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult; static; inline;
    class function ProtectStreamAsync(unprotectedStream: IInputStream; identity: HSTRING; protectedStream: IOutputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; static; inline;
    class function UnprotectStreamAsync(protectedStream: IInputStream; unprotectedStream: IOutputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; static; inline;
    class function GetProtectionInfoAsync(protectedData: IBuffer): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; static; inline;
    class function GetStreamProtectionInfoAsync(protectedStream: IInputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo; static; inline;
  end;

  // Windows.Security.EnterpriseData.FileProtectionManager
  // DualAPI
  // Statics: "Windows.Security.EnterpriseData.IFileProtectionManagerStatics"
  // Statics: "Windows.Security.EnterpriseData.IFileProtectionManagerStatics2"
  // Statics: "Windows.Security.EnterpriseData.IFileProtectionManagerStatics3"
  TEnterpriseData_FileProtectionManager = class(TWinRTGenericImportS3<EnterpriseData_IFileProtectionManagerStatics, EnterpriseData_IFileProtectionManagerStatics2, EnterpriseData_IFileProtectionManagerStatics3>)
  public
    // -> EnterpriseData_IFileProtectionManagerStatics
    class function ProtectAsync(target: IStorageItem; identity: HSTRING): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; static; inline;
    class function CopyProtectionAsync(source: IStorageItem; target: IStorageItem): IAsyncOperation_1__Boolean; static; inline;
    class function GetProtectionInfoAsync(source: IStorageItem): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; static; inline;
    class function SaveFileAsContainerAsync(protectedFile: IStorageFile): IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult; overload; static; inline;
    class function LoadFileFromContainerAsync(containerFile: IStorageFile): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult; overload; static; inline;
    class function LoadFileFromContainerAsync(containerFile: IStorageFile; target: IStorageItem): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult; overload; static; inline;
    class function CreateProtectedAndOpenAsync(parentFolder: IStorageFolder; desiredName: HSTRING; identity: HSTRING; collisionOption: CreationCollisionOption): IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult; static; inline;

    // -> EnterpriseData_IFileProtectionManagerStatics2
    class function IsContainerAsync(&file: IStorageFile): IAsyncOperation_1__Boolean; static; inline;
    class function LoadFileFromContainerAsync(containerFile: IStorageFile; target: IStorageItem; collisionOption: NameCollisionOption): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult; overload; static; inline;
    class function SaveFileAsContainerAsync(protectedFile: IStorageFile; sharedWithIdentities: IIterable_1__HSTRING): IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult; overload; static; inline;

    // -> EnterpriseData_IFileProtectionManagerStatics3
    class function UnprotectAsync(target: IStorageItem): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; overload; static; inline;
    class function UnprotectAsync(target: IStorageItem; options: EnterpriseData_IFileUnprotectOptions): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo; overload; static; inline;
  end;

  // Windows.Security.EnterpriseData.FileRevocationManager
  // DualAPI
  // Statics: "Windows.Security.EnterpriseData.IFileRevocationManagerStatics"
  TEnterpriseData_FileRevocationManager = class(TWinRTGenericImportS<EnterpriseData_IFileRevocationManagerStatics>)
  public
    // -> EnterpriseData_IFileRevocationManagerStatics
    class function ProtectAsync(storageItem: IStorageItem; enterpriseIdentity: HSTRING): IAsyncOperation_1__EnterpriseData_FileProtectionStatus; static; inline;
    class function CopyProtectionAsync(sourceStorageItem: IStorageItem; targetStorageItem: IStorageItem): IAsyncOperation_1__Boolean; static; inline;
    class procedure Revoke(enterpriseIdentity: HSTRING); static; inline;
    class function GetStatusAsync(storageItem: IStorageItem): IAsyncOperation_1__EnterpriseData_FileProtectionStatus; static; inline;
  end;

  // Windows.Security.EnterpriseData.FileUnprotectOptions
  // DualAPI
  // Implements: Windows.Security.EnterpriseData.IFileUnprotectOptions
  // Factory: "Windows.Security.EnterpriseData.IFileUnprotectOptionsFactory"
  TEnterpriseData_FileUnprotectOptions = class(TWinRTGenericImportF<EnterpriseData_IFileUnprotectOptionsFactory>)
  public
    // -> EnterpriseData_IFileUnprotectOptionsFactory
    class function Create(audit: Boolean): EnterpriseData_IFileUnprotectOptions; static; inline;
  end;

  // Windows.Security.EnterpriseData.ProtectionPolicyAuditInfo
  // DualAPI
  // Implements: Windows.Security.EnterpriseData.IProtectionPolicyAuditInfo
  // Factory: "Windows.Security.EnterpriseData.IProtectionPolicyAuditInfoFactory"
  TEnterpriseData_ProtectionPolicyAuditInfo = class(TWinRTGenericImportF<EnterpriseData_IProtectionPolicyAuditInfoFactory>)
  public
    // -> EnterpriseData_IProtectionPolicyAuditInfoFactory
    class function Create(action: EnterpriseData_ProtectionPolicyAuditAction; dataDescription: HSTRING; sourceDescription: HSTRING; targetDescription: HSTRING): EnterpriseData_IProtectionPolicyAuditInfo; static; inline;
    class function CreateWithActionAndDataDescription(action: EnterpriseData_ProtectionPolicyAuditAction; dataDescription: HSTRING): EnterpriseData_IProtectionPolicyAuditInfo; static; inline;
  end;

  // Windows.Security.EnterpriseData.ProtectionPolicyManager
  // DualAPI
  // Implements: Windows.Security.EnterpriseData.IProtectionPolicyManager
  // Implements: Windows.Security.EnterpriseData.IProtectionPolicyManager2
  // Statics: "Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics"
  // Statics: "Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics2"
  // Statics: "Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics3"
  // Statics: "Windows.Security.EnterpriseData.IProtectionPolicyManagerStatics4"
  TEnterpriseData_ProtectionPolicyManager = class(TWinRTGenericImportS4<EnterpriseData_IProtectionPolicyManagerStatics, EnterpriseData_IProtectionPolicyManagerStatics2, EnterpriseData_IProtectionPolicyManagerStatics3, EnterpriseData_IProtectionPolicyManagerStatics4>)
  public
    // -> EnterpriseData_IProtectionPolicyManagerStatics
    class function IsIdentityManaged(identity: HSTRING): Boolean; static; inline;
    class function TryApplyProcessUIPolicy(identity: HSTRING): Boolean; static; inline;
    class procedure ClearProcessUIPolicy; static; inline;
    class function CreateCurrentThreadNetworkContext(identity: HSTRING): EnterpriseData_IThreadNetworkContext; static; inline;
    class function GetPrimaryManagedIdentityForNetworkEndpointAsync(endpointHost: IHostName): IAsyncOperation_1__HSTRING; static; inline;
    class procedure RevokeContent(identity: HSTRING); static; inline;
    class function GetForCurrentView: EnterpriseData_IProtectionPolicyManager; static; inline;
    class function add_ProtectedAccessSuspending(handler: EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_ProtectedAccessSuspending(token: EventRegistrationToken); static; inline;
    class function add_ProtectedAccessResumed(handler: EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_ProtectedAccessResumed(token: EventRegistrationToken); static; inline;
    class function add_ProtectedContentRevoked(handler: EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_ProtectedContentRevoked(token: EventRegistrationToken); static; inline;
    class function CheckAccess(sourceIdentity: HSTRING; targetIdentity: HSTRING): EnterpriseData_ProtectionPolicyEvaluationResult; static; inline;
    class function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;

    // -> EnterpriseData_IProtectionPolicyManagerStatics2
    class function HasContentBeenRevokedSince(identity: HSTRING; since: DateTime): Boolean; static; inline;
    class function CheckAccessForApp(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING): EnterpriseData_ProtectionPolicyEvaluationResult; static; inline;
    class function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function GetEnforcementLevel(identity: HSTRING): EnterpriseData_EnforcementLevel; static; inline;
    class function IsUserDecryptionAllowed(identity: HSTRING): Boolean; static; inline;
    class function IsProtectionUnderLockRequired(identity: HSTRING): Boolean; static; inline;
    class function add_PolicyChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_PolicyChanged(token: EventRegistrationToken); static; inline;
    class function get_IsProtectionEnabled: Boolean; static; inline;
    class property IsProtectionEnabled: Boolean read get_IsProtectionEnabled;

    // -> EnterpriseData_IProtectionPolicyManagerStatics3
    class function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class procedure LogAuditEvent(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo); static; inline;

    // -> EnterpriseData_IProtectionPolicyManagerStatics4
    class function IsRoamableProtectionEnabled(identity: HSTRING): Boolean; static; inline;
    class function RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessToFilesForAppAsync(sourceItemList: IIterable_1__IStorageItem; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessToFilesForAppAsync(sourceItemList: IIterable_1__IStorageItem; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessToFilesForProcessAsync(sourceItemList: IIterable_1__IStorageItem; processId: Cardinal; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function RequestAccessToFilesForProcessAsync(sourceItemList: IIterable_1__IStorageItem; processId: Cardinal; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; static; inline;
    class function IsFileProtectionRequiredAsync(target: IStorageItem; identity: HSTRING): IAsyncOperation_1__Boolean; static; inline;
    class function IsFileProtectionRequiredForNewFileAsync(parentFolder: IStorageFolder; identity: HSTRING; desiredName: HSTRING): IAsyncOperation_1__Boolean; static; inline;
    class function get_PrimaryManagedIdentity: HSTRING; static; inline;
    class function GetPrimaryManagedIdentityForIdentity(identity: HSTRING): HSTRING; static; inline;
    class property PrimaryManagedIdentity: HSTRING read get_PrimaryManagedIdentity;
  end;

implementation

{ TAuthentication_Identity_EnterpriseKeyCredentialRegistrationManager }

class function TAuthentication_Identity_EnterpriseKeyCredentialRegistrationManager.get_Current: Authentication_Identity_IEnterpriseKeyCredentialRegistrationManager;
begin
  Result := Statics.get_Current;
end;


{ TAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication }

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication.ShowNotificationMessageAsync(deviceName: HSTRING; &message: Authentication_Identity_Provider_SecondaryAuthenticationFactorAuthenticationMessage): IAsyncAction;
begin
  Result := Statics.ShowNotificationMessageAsync(deviceName, &message);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication.StartAuthenticationAsync(deviceId: HSTRING; serviceAuthenticationNonce: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationResult;
begin
  Result := Statics.StartAuthenticationAsync(deviceId, serviceAuthenticationNonce);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication.add_AuthenticationStageChanged(handler: EventHandler_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageChangedEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_AuthenticationStageChanged(handler);
end;

class procedure TAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication.remove_AuthenticationStageChanged(token: EventRegistrationToken);
begin
  Statics.remove_AuthenticationStageChanged(token);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorAuthentication.GetAuthenticationStageInfoAsync: IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorAuthenticationStageInfo;
begin
  Result := Statics.GetAuthenticationStageInfoAsync;
end;


{ TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration }

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.RegisterDevicePresenceMonitoringAsync(deviceId: HSTRING; deviceInstancePath: HSTRING; monitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode): IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus;
begin
  Result := Statics.RegisterDevicePresenceMonitoringAsync(deviceId, deviceInstancePath, monitoringMode);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.RegisterDevicePresenceMonitoringAsync(deviceId: HSTRING; deviceInstancePath: HSTRING; monitoringMode: Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringMode; deviceFriendlyName: HSTRING; deviceModelNumber: HSTRING; deviceConfigurationData: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_SecondaryAuthenticationFactorDevicePresenceMonitoringRegistrationStatus;
begin
  Result := Statics.RegisterDevicePresenceMonitoringAsync(deviceId, deviceInstancePath, monitoringMode, deviceFriendlyName, deviceModelNumber, deviceConfigurationData);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.UnregisterDevicePresenceMonitoringAsync(deviceId: HSTRING): IAsyncAction;
begin
  Result := Statics.UnregisterDevicePresenceMonitoringAsync(deviceId);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.IsDevicePresenceMonitoringSupported: Boolean;
begin
  Result := Statics.IsDevicePresenceMonitoringSupported;
end;


class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.RequestStartRegisteringDeviceAsync(deviceId: HSTRING; capabilities: Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceCapabilities; deviceFriendlyName: HSTRING; deviceModelNumber: HSTRING; deviceKey: IBuffer; mutualAuthenticationKey: IBuffer): IAsyncOperation_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorRegistrationResult;
begin
  Result := Statics2.RequestStartRegisteringDeviceAsync(deviceId, capabilities, deviceFriendlyName, deviceModelNumber, deviceKey, mutualAuthenticationKey);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.FindAllRegisteredDeviceInfoAsync(queryType: Authentication_Identity_Provider_SecondaryAuthenticationFactorDeviceFindScope): IAsyncOperation_1__IVectorView_1__Authentication_Identity_Provider_ISecondaryAuthenticationFactorInfo;
begin
  Result := Statics2.FindAllRegisteredDeviceInfoAsync(queryType);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.UnregisterDeviceAsync(deviceId: HSTRING): IAsyncAction;
begin
  Result := Statics2.UnregisterDeviceAsync(deviceId);
end;

class function TAuthentication_Identity_Provider_SecondaryAuthenticationFactorRegistration.UpdateDeviceConfigurationDataAsync(deviceId: HSTRING; deviceConfigurationData: IBuffer): IAsyncAction;
begin
  Result := Statics2.UpdateDeviceConfigurationDataAsync(deviceId, deviceConfigurationData);
end;


{ TAuthentication_Web_Core_WebAuthenticationCoreManager }

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.GetTokenSilentlyAsync(request: Authentication_Web_Core_IWebTokenRequest): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult;
begin
  Result := Statics.GetTokenSilentlyAsync(request);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.GetTokenSilentlyAsync(request: Authentication_Web_Core_IWebTokenRequest; webAccount: IWebAccount): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult;
begin
  Result := Statics.GetTokenSilentlyAsync(request, webAccount);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.RequestTokenAsync(request: Authentication_Web_Core_IWebTokenRequest): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult;
begin
  Result := Statics.RequestTokenAsync(request);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.RequestTokenAsync(request: Authentication_Web_Core_IWebTokenRequest; webAccount: IWebAccount): IAsyncOperation_1__Authentication_Web_Core_IWebTokenRequestResult;
begin
  Result := Statics.RequestTokenAsync(request, webAccount);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindAccountAsync(provider: IWebAccountProvider; webAccountId: HSTRING): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics.FindAccountAsync(provider, webAccountId);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindAccountProviderAsync(webAccountProviderId: HSTRING): IAsyncOperation_1__IWebAccountProvider;
begin
  Result := Statics.FindAccountProviderAsync(webAccountProviderId);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING): IAsyncOperation_1__IWebAccountProvider;
begin
  Result := Statics.FindAccountProviderAsync(webAccountProviderId, authority);
end;


class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING; user: IUser): IAsyncOperation_1__IWebAccountProvider;
begin
  Result := Statics2.FindAccountProviderAsync(webAccountProviderId, authority, user);
end;


class function TAuthentication_Web_Core_WebAuthenticationCoreManager.CreateWebAccountMonitor(webAccounts: IIterable_1__IWebAccount): Authentication_Web_Core_IWebAccountMonitor;
begin
  Result := Statics3.CreateWebAccountMonitor(webAccounts);
end;


class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindAllAccountsAsync(provider: IWebAccountProvider): IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult;
begin
  Result := Statics4.FindAllAccountsAsync(provider);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindAllAccountsAsync(provider: IWebAccountProvider; clientId: HSTRING): IAsyncOperation_1__Authentication_Web_Core_IFindAllAccountsResult;
begin
  Result := Statics4.FindAllAccountsAsync(provider, clientId);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindSystemAccountProviderAsync(webAccountProviderId: HSTRING): IAsyncOperation_1__IWebAccountProvider;
begin
  Result := Statics4.FindSystemAccountProviderAsync(webAccountProviderId);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindSystemAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING): IAsyncOperation_1__IWebAccountProvider;
begin
  Result := Statics4.FindSystemAccountProviderAsync(webAccountProviderId, authority);
end;

class function TAuthentication_Web_Core_WebAuthenticationCoreManager.FindSystemAccountProviderAsync(webAccountProviderId: HSTRING; authority: HSTRING; user: IUser): IAsyncOperation_1__IWebAccountProvider;
begin
  Result := Statics4.FindSystemAccountProviderAsync(webAccountProviderId, authority, user);
end;


{ TAuthentication_Web_Core_WebProviderError }
// Factories for : "Authentication_Web_Core_WebProviderError"
// Factory: "Windows.Security.Authentication.Web.Core.IWebProviderErrorFactory"
// -> Authentication_Web_Core_IWebProviderErrorFactory

class function TAuthentication_Web_Core_WebProviderError.Create(errorCode: Cardinal; errorMessage: HSTRING): Authentication_Web_Core_IWebProviderError;
begin
  Result := Factory.Create(errorCode, errorMessage);
end;


{ TAuthentication_Web_Core_WebTokenRequest }
// Factories for : "Authentication_Web_Core_WebTokenRequest"
// Factory: "Windows.Security.Authentication.Web.Core.IWebTokenRequestFactory"
// -> Authentication_Web_Core_IWebTokenRequestFactory

class function TAuthentication_Web_Core_WebTokenRequest.Create(provider: IWebAccountProvider; scope: HSTRING; clientId: HSTRING): Authentication_Web_Core_IWebTokenRequest;
begin
  Result := Factory.Create(provider, scope, clientId);
end;

class function TAuthentication_Web_Core_WebTokenRequest.CreateWithPromptType(provider: IWebAccountProvider; scope: HSTRING; clientId: HSTRING; promptType: Authentication_Web_Core_WebTokenRequestPromptType): Authentication_Web_Core_IWebTokenRequest;
begin
  Result := Factory.CreateWithPromptType(provider, scope, clientId, promptType);
end;

class function TAuthentication_Web_Core_WebTokenRequest.CreateWithProvider(provider: IWebAccountProvider): Authentication_Web_Core_IWebTokenRequest;
begin
  Result := Factory.CreateWithProvider(provider);
end;

class function TAuthentication_Web_Core_WebTokenRequest.CreateWithScope(provider: IWebAccountProvider; scope: HSTRING): Authentication_Web_Core_IWebTokenRequest;
begin
  Result := Factory.CreateWithScope(provider, scope);
end;


{ TAuthentication_Web_Core_WebTokenResponse }
// Factories for : "Authentication_Web_Core_WebTokenResponse"
// Factory: "Windows.Security.Authentication.Web.Core.IWebTokenResponseFactory"
// -> Authentication_Web_Core_IWebTokenResponseFactory

class function TAuthentication_Web_Core_WebTokenResponse.CreateWithToken(token: HSTRING): Authentication_Web_Core_IWebTokenResponse;
begin
  Result := Factory.CreateWithToken(token);
end;

class function TAuthentication_Web_Core_WebTokenResponse.CreateWithTokenAndAccount(token: HSTRING; webAccount: IWebAccount): Authentication_Web_Core_IWebTokenResponse;
begin
  Result := Factory.CreateWithTokenAndAccount(token, webAccount);
end;

class function TAuthentication_Web_Core_WebTokenResponse.CreateWithTokenAccountAndError(token: HSTRING; webAccount: IWebAccount; error: Authentication_Web_Core_IWebProviderError): Authentication_Web_Core_IWebTokenResponse;
begin
  Result := Factory.CreateWithTokenAccountAndError(token, webAccount, error);
end;


{ TAuthentication_Web_Provider_WebAccountClientView }
// Factories for : "Authentication_Web_Provider_WebAccountClientView"
// Factory: "Windows.Security.Authentication.Web.Provider.IWebAccountClientViewFactory"
// -> Authentication_Web_Provider_IWebAccountClientViewFactory

class function TAuthentication_Web_Provider_WebAccountClientView.Create(viewType: Authentication_Web_Provider_WebAccountClientViewType; applicationCallbackUri: IUriRuntimeClass): Authentication_Web_Provider_IWebAccountClientView;
begin
  Result := Factory.Create(viewType, applicationCallbackUri);
end;

class function TAuthentication_Web_Provider_WebAccountClientView.CreateWithPairwiseId(viewType: Authentication_Web_Provider_WebAccountClientViewType; applicationCallbackUri: IUriRuntimeClass; accountPairwiseId: HSTRING): Authentication_Web_Provider_IWebAccountClientView;
begin
  Result := Factory.CreateWithPairwiseId(viewType, applicationCallbackUri, accountPairwiseId);
end;


{ TAuthentication_Web_Provider_WebAccountManager }

class function TAuthentication_Web_Provider_WebAccountManager.UpdateWebAccountPropertiesAsync(webAccount: IWebAccount; webAccountUserName: HSTRING; additionalProperties: IMapView_2__HSTRING__HSTRING): IAsyncAction;
begin
  Result := Statics.UpdateWebAccountPropertiesAsync(webAccount, webAccountUserName, additionalProperties);
end;

class function TAuthentication_Web_Provider_WebAccountManager.AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics.AddWebAccountAsync(webAccountId, webAccountUserName, props);
end;

class function TAuthentication_Web_Provider_WebAccountManager.DeleteWebAccountAsync(webAccount: IWebAccount): IAsyncAction;
begin
  Result := Statics.DeleteWebAccountAsync(webAccount);
end;

class function TAuthentication_Web_Provider_WebAccountManager.FindAllProviderWebAccountsAsync: IAsyncOperation_1__IVectorView_1__IWebAccount;
begin
  Result := Statics.FindAllProviderWebAccountsAsync;
end;

class function TAuthentication_Web_Provider_WebAccountManager.PushCookiesAsync(uri: IUriRuntimeClass; cookies: IVectorView_1__Http_IHttpCookie): IAsyncAction;
begin
  Result := Statics.PushCookiesAsync(uri, cookies);
end;

class function TAuthentication_Web_Provider_WebAccountManager.SetViewAsync(webAccount: IWebAccount; view: Authentication_Web_Provider_IWebAccountClientView): IAsyncAction;
begin
  Result := Statics.SetViewAsync(webAccount, view);
end;

class function TAuthentication_Web_Provider_WebAccountManager.ClearViewAsync(webAccount: IWebAccount; applicationCallbackUri: IUriRuntimeClass): IAsyncAction;
begin
  Result := Statics.ClearViewAsync(webAccount, applicationCallbackUri);
end;

class function TAuthentication_Web_Provider_WebAccountManager.GetViewsAsync(webAccount: IWebAccount): IAsyncOperation_1__IVectorView_1__Authentication_Web_Provider_IWebAccountClientView;
begin
  Result := Statics.GetViewsAsync(webAccount);
end;

class function TAuthentication_Web_Provider_WebAccountManager.SetWebAccountPictureAsync(webAccount: IWebAccount; webAccountPicture: IRandomAccessStream): IAsyncAction;
begin
  Result := Statics.SetWebAccountPictureAsync(webAccount, webAccountPicture);
end;

class function TAuthentication_Web_Provider_WebAccountManager.ClearWebAccountPictureAsync(webAccount: IWebAccount): IAsyncAction;
begin
  Result := Statics.ClearWebAccountPictureAsync(webAccount);
end;


class function TAuthentication_Web_Provider_WebAccountManager.PullCookiesAsync(uriString: HSTRING; callerPFN: HSTRING): IAsyncAction;
begin
  Result := Statics2.PullCookiesAsync(uriString, callerPFN);
end;


class function TAuthentication_Web_Provider_WebAccountManager.FindAllProviderWebAccountsForUserAsync(user: IUser): IAsyncOperation_1__IVectorView_1__IWebAccount;
begin
  Result := Statics3.FindAllProviderWebAccountsForUserAsync(user);
end;

class function TAuthentication_Web_Provider_WebAccountManager.AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics3.AddWebAccountForUserAsync(user, webAccountId, webAccountUserName, props);
end;

class function TAuthentication_Web_Provider_WebAccountManager.AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics3.AddWebAccountForUserAsync(user, webAccountId, webAccountUserName, props, scope);
end;

class function TAuthentication_Web_Provider_WebAccountManager.AddWebAccountForUserAsync(user: IUser; webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope; perUserWebAccountId: HSTRING): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics3.AddWebAccountForUserAsync(user, webAccountId, webAccountUserName, props, scope, perUserWebAccountId);
end;


class function TAuthentication_Web_Provider_WebAccountManager.InvalidateAppCacheForAllAccountsAsync: IAsyncAction;
begin
  Result := Statics4.InvalidateAppCacheForAllAccountsAsync;
end;

class function TAuthentication_Web_Provider_WebAccountManager.InvalidateAppCacheForAccountAsync(webAccount: IWebAccount): IAsyncAction;
begin
  Result := Statics4.InvalidateAppCacheForAccountAsync(webAccount);
end;


class function TAuthentication_Web_Provider_WebAccountManager.AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope; perUserWebAccountId: HSTRING): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics5.AddWebAccountAsync(webAccountId, webAccountUserName, props, scope, perUserWebAccountId);
end;

class function TAuthentication_Web_Provider_WebAccountManager.SetPerAppToPerUserAccountAsync(perAppAccount: IWebAccount; perUserWebAccountId: HSTRING): IAsyncAction;
begin
  Result := Statics5.SetPerAppToPerUserAccountAsync(perAppAccount, perUserWebAccountId);
end;

class function TAuthentication_Web_Provider_WebAccountManager.GetPerUserFromPerAppAccountAsync(perAppAccount: IWebAccount): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics5.GetPerUserFromPerAppAccountAsync(perAppAccount);
end;

class function TAuthentication_Web_Provider_WebAccountManager.ClearPerUserFromPerAppAccountAsync(perAppAccount: IWebAccount): IAsyncAction;
begin
  Result := Statics5.ClearPerUserFromPerAppAccountAsync(perAppAccount);
end;


class function TAuthentication_Web_Provider_WebAccountManager.AddWebAccountAsync(webAccountId: HSTRING; webAccountUserName: HSTRING; props: IMapView_2__HSTRING__HSTRING; scope: Authentication_Web_Provider_WebAccountScope): IAsyncOperation_1__IWebAccount;
begin
  Result := Statics6.AddWebAccountAsync(webAccountId, webAccountUserName, props, scope);
end;

class function TAuthentication_Web_Provider_WebAccountManager.SetScopeAsync(webAccount: IWebAccount; scope: Authentication_Web_Provider_WebAccountScope): IAsyncAction;
begin
  Result := Statics6.SetScopeAsync(webAccount, scope);
end;

class function TAuthentication_Web_Provider_WebAccountManager.GetScope(webAccount: IWebAccount): Authentication_Web_Provider_WebAccountScope;
begin
  Result := Statics6.GetScope(webAccount);
end;


{ TAuthentication_Web_Provider_WebProviderTokenResponse }
// Factories for : "Authentication_Web_Provider_WebProviderTokenResponse"
// Factory: "Windows.Security.Authentication.Web.Provider.IWebProviderTokenResponseFactory"
// -> Authentication_Web_Provider_IWebProviderTokenResponseFactory

class function TAuthentication_Web_Provider_WebProviderTokenResponse.Create(webTokenResponse: Authentication_Web_Core_IWebTokenResponse): Authentication_Web_Provider_IWebProviderTokenResponse;
begin
  Result := Factory.Create(webTokenResponse);
end;


{ TEnterpriseData_DataProtectionManager }

class function TEnterpriseData_DataProtectionManager.ProtectAsync(data: IBuffer; identity: HSTRING): IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult;
begin
  Result := Statics.ProtectAsync(data, identity);
end;

class function TEnterpriseData_DataProtectionManager.UnprotectAsync(data: IBuffer): IAsyncOperation_1__EnterpriseData_IBufferProtectUnprotectResult;
begin
  Result := Statics.UnprotectAsync(data);
end;

class function TEnterpriseData_DataProtectionManager.ProtectStreamAsync(unprotectedStream: IInputStream; identity: HSTRING; protectedStream: IOutputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo;
begin
  Result := Statics.ProtectStreamAsync(unprotectedStream, identity, protectedStream);
end;

class function TEnterpriseData_DataProtectionManager.UnprotectStreamAsync(protectedStream: IInputStream; unprotectedStream: IOutputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo;
begin
  Result := Statics.UnprotectStreamAsync(protectedStream, unprotectedStream);
end;

class function TEnterpriseData_DataProtectionManager.GetProtectionInfoAsync(protectedData: IBuffer): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo;
begin
  Result := Statics.GetProtectionInfoAsync(protectedData);
end;

class function TEnterpriseData_DataProtectionManager.GetStreamProtectionInfoAsync(protectedStream: IInputStream): IAsyncOperation_1__EnterpriseData_IDataProtectionInfo;
begin
  Result := Statics.GetStreamProtectionInfoAsync(protectedStream);
end;


{ TEnterpriseData_FileProtectionManager }

class function TEnterpriseData_FileProtectionManager.ProtectAsync(target: IStorageItem; identity: HSTRING): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo;
begin
  Result := Statics.ProtectAsync(target, identity);
end;

class function TEnterpriseData_FileProtectionManager.CopyProtectionAsync(source: IStorageItem; target: IStorageItem): IAsyncOperation_1__Boolean;
begin
  Result := Statics.CopyProtectionAsync(source, target);
end;

class function TEnterpriseData_FileProtectionManager.GetProtectionInfoAsync(source: IStorageItem): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo;
begin
  Result := Statics.GetProtectionInfoAsync(source);
end;

class function TEnterpriseData_FileProtectionManager.SaveFileAsContainerAsync(protectedFile: IStorageFile): IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult;
begin
  Result := Statics.SaveFileAsContainerAsync(protectedFile);
end;

class function TEnterpriseData_FileProtectionManager.LoadFileFromContainerAsync(containerFile: IStorageFile): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult;
begin
  Result := Statics.LoadFileFromContainerAsync(containerFile);
end;

class function TEnterpriseData_FileProtectionManager.LoadFileFromContainerAsync(containerFile: IStorageFile; target: IStorageItem): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult;
begin
  Result := Statics.LoadFileFromContainerAsync(containerFile, target);
end;

class function TEnterpriseData_FileProtectionManager.CreateProtectedAndOpenAsync(parentFolder: IStorageFolder; desiredName: HSTRING; identity: HSTRING; collisionOption: CreationCollisionOption): IAsyncOperation_1__EnterpriseData_IProtectedFileCreateResult;
begin
  Result := Statics.CreateProtectedAndOpenAsync(parentFolder, desiredName, identity, collisionOption);
end;


class function TEnterpriseData_FileProtectionManager.IsContainerAsync(&file: IStorageFile): IAsyncOperation_1__Boolean;
begin
  Result := Statics2.IsContainerAsync(&file);
end;

class function TEnterpriseData_FileProtectionManager.LoadFileFromContainerAsync(containerFile: IStorageFile; target: IStorageItem; collisionOption: NameCollisionOption): IAsyncOperation_1__EnterpriseData_IProtectedContainerImportResult;
begin
  Result := Statics2.LoadFileFromContainerAsync(containerFile, target, collisionOption);
end;

class function TEnterpriseData_FileProtectionManager.SaveFileAsContainerAsync(protectedFile: IStorageFile; sharedWithIdentities: IIterable_1__HSTRING): IAsyncOperation_1__EnterpriseData_IProtectedContainerExportResult;
begin
  Result := Statics2.SaveFileAsContainerAsync(protectedFile, sharedWithIdentities);
end;


class function TEnterpriseData_FileProtectionManager.UnprotectAsync(target: IStorageItem): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo;
begin
  Result := Statics3.UnprotectAsync(target);
end;

class function TEnterpriseData_FileProtectionManager.UnprotectAsync(target: IStorageItem; options: EnterpriseData_IFileUnprotectOptions): IAsyncOperation_1__EnterpriseData_IFileProtectionInfo;
begin
  Result := Statics3.UnprotectAsync(target, options);
end;


{ TEnterpriseData_FileRevocationManager }

class function TEnterpriseData_FileRevocationManager.ProtectAsync(storageItem: IStorageItem; enterpriseIdentity: HSTRING): IAsyncOperation_1__EnterpriseData_FileProtectionStatus;
begin
  Result := Statics.ProtectAsync(storageItem, enterpriseIdentity);
end;

class function TEnterpriseData_FileRevocationManager.CopyProtectionAsync(sourceStorageItem: IStorageItem; targetStorageItem: IStorageItem): IAsyncOperation_1__Boolean;
begin
  Result := Statics.CopyProtectionAsync(sourceStorageItem, targetStorageItem);
end;

class procedure TEnterpriseData_FileRevocationManager.Revoke(enterpriseIdentity: HSTRING);
begin
  Statics.Revoke(enterpriseIdentity);
end;

class function TEnterpriseData_FileRevocationManager.GetStatusAsync(storageItem: IStorageItem): IAsyncOperation_1__EnterpriseData_FileProtectionStatus;
begin
  Result := Statics.GetStatusAsync(storageItem);
end;


{ TEnterpriseData_FileUnprotectOptions }
// Factories for : "EnterpriseData_FileUnprotectOptions"
// Factory: "Windows.Security.EnterpriseData.IFileUnprotectOptionsFactory"
// -> EnterpriseData_IFileUnprotectOptionsFactory

class function TEnterpriseData_FileUnprotectOptions.Create(audit: Boolean): EnterpriseData_IFileUnprotectOptions;
begin
  Result := Factory.Create(audit);
end;


{ TEnterpriseData_ProtectionPolicyAuditInfo }
// Factories for : "EnterpriseData_ProtectionPolicyAuditInfo"
// Factory: "Windows.Security.EnterpriseData.IProtectionPolicyAuditInfoFactory"
// -> EnterpriseData_IProtectionPolicyAuditInfoFactory

class function TEnterpriseData_ProtectionPolicyAuditInfo.Create(action: EnterpriseData_ProtectionPolicyAuditAction; dataDescription: HSTRING; sourceDescription: HSTRING; targetDescription: HSTRING): EnterpriseData_IProtectionPolicyAuditInfo;
begin
  Result := Factory.Create(action, dataDescription, sourceDescription, targetDescription);
end;

class function TEnterpriseData_ProtectionPolicyAuditInfo.CreateWithActionAndDataDescription(action: EnterpriseData_ProtectionPolicyAuditAction; dataDescription: HSTRING): EnterpriseData_IProtectionPolicyAuditInfo;
begin
  Result := Factory.CreateWithActionAndDataDescription(action, dataDescription);
end;


{ TEnterpriseData_ProtectionPolicyManager }

class function TEnterpriseData_ProtectionPolicyManager.IsIdentityManaged(identity: HSTRING): Boolean;
begin
  Result := Statics.IsIdentityManaged(identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.TryApplyProcessUIPolicy(identity: HSTRING): Boolean;
begin
  Result := Statics.TryApplyProcessUIPolicy(identity);
end;

class procedure TEnterpriseData_ProtectionPolicyManager.ClearProcessUIPolicy;
begin
  Statics.ClearProcessUIPolicy;
end;

class function TEnterpriseData_ProtectionPolicyManager.CreateCurrentThreadNetworkContext(identity: HSTRING): EnterpriseData_IThreadNetworkContext;
begin
  Result := Statics.CreateCurrentThreadNetworkContext(identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.GetPrimaryManagedIdentityForNetworkEndpointAsync(endpointHost: IHostName): IAsyncOperation_1__HSTRING;
begin
  Result := Statics.GetPrimaryManagedIdentityForNetworkEndpointAsync(endpointHost);
end;

class procedure TEnterpriseData_ProtectionPolicyManager.RevokeContent(identity: HSTRING);
begin
  Statics.RevokeContent(identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.GetForCurrentView: EnterpriseData_IProtectionPolicyManager;
begin
  Result := Statics.GetForCurrentView;
end;

class function TEnterpriseData_ProtectionPolicyManager.add_ProtectedAccessSuspending(handler: EventHandler_1__EnterpriseData_IProtectedAccessSuspendingEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_ProtectedAccessSuspending(handler);
end;

class procedure TEnterpriseData_ProtectionPolicyManager.remove_ProtectedAccessSuspending(token: EventRegistrationToken);
begin
  Statics.remove_ProtectedAccessSuspending(token);
end;

class function TEnterpriseData_ProtectionPolicyManager.add_ProtectedAccessResumed(handler: EventHandler_1__EnterpriseData_IProtectedAccessResumedEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_ProtectedAccessResumed(handler);
end;

class procedure TEnterpriseData_ProtectionPolicyManager.remove_ProtectedAccessResumed(token: EventRegistrationToken);
begin
  Statics.remove_ProtectedAccessResumed(token);
end;

class function TEnterpriseData_ProtectionPolicyManager.add_ProtectedContentRevoked(handler: EventHandler_1__EnterpriseData_IProtectedContentRevokedEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_ProtectedContentRevoked(handler);
end;

class procedure TEnterpriseData_ProtectionPolicyManager.remove_ProtectedContentRevoked(token: EventRegistrationToken);
begin
  Statics.remove_ProtectedContentRevoked(token);
end;

class function TEnterpriseData_ProtectionPolicyManager.CheckAccess(sourceIdentity: HSTRING; targetIdentity: HSTRING): EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics.CheckAccess(sourceIdentity, targetIdentity);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics.RequestAccessAsync(sourceIdentity, targetIdentity);
end;


class function TEnterpriseData_ProtectionPolicyManager.HasContentBeenRevokedSince(identity: HSTRING; since: DateTime): Boolean;
begin
  Result := Statics2.HasContentBeenRevokedSince(identity, since);
end;

class function TEnterpriseData_ProtectionPolicyManager.CheckAccessForApp(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING): EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics2.CheckAccessForApp(sourceIdentity, appPackageFamilyName);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics2.RequestAccessForAppAsync(sourceIdentity, appPackageFamilyName);
end;

class function TEnterpriseData_ProtectionPolicyManager.GetEnforcementLevel(identity: HSTRING): EnterpriseData_EnforcementLevel;
begin
  Result := Statics2.GetEnforcementLevel(identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.IsUserDecryptionAllowed(identity: HSTRING): Boolean;
begin
  Result := Statics2.IsUserDecryptionAllowed(identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.IsProtectionUnderLockRequired(identity: HSTRING): Boolean;
begin
  Result := Statics2.IsProtectionUnderLockRequired(identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.add_PolicyChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics2.add_PolicyChanged(handler);
end;

class procedure TEnterpriseData_ProtectionPolicyManager.remove_PolicyChanged(token: EventRegistrationToken);
begin
  Statics2.remove_PolicyChanged(token);
end;

class function TEnterpriseData_ProtectionPolicyManager.get_IsProtectionEnabled: Boolean;
begin
  Result := Statics2.get_IsProtectionEnabled;
end;


class function TEnterpriseData_ProtectionPolicyManager.RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics3.RequestAccessAsync(sourceIdentity, targetIdentity, auditInfo);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics3.RequestAccessAsync(sourceIdentity, targetIdentity, auditInfo, messageFromApp);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics3.RequestAccessForAppAsync(sourceIdentity, appPackageFamilyName, auditInfo);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics3.RequestAccessForAppAsync(sourceIdentity, appPackageFamilyName, auditInfo, messageFromApp);
end;

class procedure TEnterpriseData_ProtectionPolicyManager.LogAuditEvent(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo);
begin
  Statics3.LogAuditEvent(sourceIdentity, targetIdentity, auditInfo);
end;


class function TEnterpriseData_ProtectionPolicyManager.IsRoamableProtectionEnabled(identity: HSTRING): Boolean;
begin
  Result := Statics4.IsRoamableProtectionEnabled(identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessAsync(sourceIdentity: HSTRING; targetIdentity: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics4.RequestAccessAsync(sourceIdentity, targetIdentity, auditInfo, messageFromApp, behavior);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessForAppAsync(sourceIdentity: HSTRING; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics4.RequestAccessForAppAsync(sourceIdentity, appPackageFamilyName, auditInfo, messageFromApp, behavior);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessToFilesForAppAsync(sourceItemList: IIterable_1__IStorageItem; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics4.RequestAccessToFilesForAppAsync(sourceItemList, appPackageFamilyName, auditInfo);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessToFilesForAppAsync(sourceItemList: IIterable_1__IStorageItem; appPackageFamilyName: HSTRING; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics4.RequestAccessToFilesForAppAsync(sourceItemList, appPackageFamilyName, auditInfo, messageFromApp, behavior);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessToFilesForProcessAsync(sourceItemList: IIterable_1__IStorageItem; processId: Cardinal; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics4.RequestAccessToFilesForProcessAsync(sourceItemList, processId, auditInfo);
end;

class function TEnterpriseData_ProtectionPolicyManager.RequestAccessToFilesForProcessAsync(sourceItemList: IIterable_1__IStorageItem; processId: Cardinal; auditInfo: EnterpriseData_IProtectionPolicyAuditInfo; messageFromApp: HSTRING; behavior: EnterpriseData_ProtectionPolicyRequestAccessBehavior): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult;
begin
  Result := Statics4.RequestAccessToFilesForProcessAsync(sourceItemList, processId, auditInfo, messageFromApp, behavior);
end;

class function TEnterpriseData_ProtectionPolicyManager.IsFileProtectionRequiredAsync(target: IStorageItem; identity: HSTRING): IAsyncOperation_1__Boolean;
begin
  Result := Statics4.IsFileProtectionRequiredAsync(target, identity);
end;

class function TEnterpriseData_ProtectionPolicyManager.IsFileProtectionRequiredForNewFileAsync(parentFolder: IStorageFolder; identity: HSTRING; desiredName: HSTRING): IAsyncOperation_1__Boolean;
begin
  Result := Statics4.IsFileProtectionRequiredForNewFileAsync(parentFolder, identity, desiredName);
end;

class function TEnterpriseData_ProtectionPolicyManager.get_PrimaryManagedIdentity: HSTRING;
begin
  Result := Statics4.get_PrimaryManagedIdentity;
end;

class function TEnterpriseData_ProtectionPolicyManager.GetPrimaryManagedIdentityForIdentity(identity: HSTRING): HSTRING;
begin
  Result := Statics4.GetPrimaryManagedIdentityForIdentity(identity);
end;


end.
