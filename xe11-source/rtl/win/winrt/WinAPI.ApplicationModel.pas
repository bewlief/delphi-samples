{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.ApplicationModel;

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
  Winapi.ApplicationModel.DataTransfer, 
  Winapi.Storage, 
  Winapi.ApplicationModel.Background, 
  Winapi.Foundation, 
  Winapi.ApplicationModel.Core, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  Activation_ActivationKind = Winapi.CommonTypes.Activation_ActivationKind;
  PActivation_ActivationKind = Winapi.CommonTypes.PActivation_ActivationKind;
  Activation_ApplicationExecutionState = Winapi.CommonTypes.Activation_ApplicationExecutionState;
  PActivation_ApplicationExecutionState = Winapi.CommonTypes.PActivation_ApplicationExecutionState;
  Activation_IActivatedEventArgs = Winapi.CommonTypes.Activation_IActivatedEventArgs;
  PActivation_IActivatedEventArgs = Winapi.CommonTypes.PActivation_IActivatedEventArgs;
  Activation_ISplashScreen = Winapi.CommonTypes.Activation_ISplashScreen;
  PActivation_ISplashScreen = Winapi.CommonTypes.PActivation_ISplashScreen;
  IAppDisplayInfo = Winapi.CommonTypes.IAppDisplayInfo;
  PIAppDisplayInfo = Winapi.CommonTypes.PIAppDisplayInfo;
  IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING_Base = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING_Base;
  IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING;
  PIIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = Winapi.CommonTypes.PIIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING;
  IIterator_1__IKeyValuePair_2__TGuid__IInspectable_Base = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__TGuid__IInspectable_Base;
  IIterator_1__IKeyValuePair_2__TGuid__IInspectable = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__TGuid__IInspectable;
  PIIterator_1__IKeyValuePair_2__TGuid__IInspectable = Winapi.CommonTypes.PIIterator_1__IKeyValuePair_2__TGuid__IInspectable;
  IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = Winapi.CommonTypes.IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING;
  PIKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = Winapi.CommonTypes.PIKeyValuePair_2__HSTRING__IVectorView_1__HSTRING;
  IKeyValuePair_2__TGuid__IInspectable = Winapi.CommonTypes.IKeyValuePair_2__TGuid__IInspectable;
  PIKeyValuePair_2__TGuid__IInspectable = Winapi.CommonTypes.PIKeyValuePair_2__TGuid__IInspectable;
  IMap_2__TGuid__IInspectable = Winapi.CommonTypes.IMap_2__TGuid__IInspectable;
  PIMap_2__TGuid__IInspectable = Winapi.CommonTypes.PIMap_2__TGuid__IInspectable;
  IMapView_2__HSTRING__IVectorView_1__HSTRING_Base = Winapi.CommonTypes.IMapView_2__HSTRING__IVectorView_1__HSTRING_Base;
  IMapView_2__HSTRING__IVectorView_1__HSTRING = Winapi.CommonTypes.IMapView_2__HSTRING__IVectorView_1__HSTRING;
  PIMapView_2__HSTRING__IVectorView_1__HSTRING = Winapi.CommonTypes.PIMapView_2__HSTRING__IVectorView_1__HSTRING;
  IMapView_2__TGuid__IInspectable_Base = Winapi.CommonTypes.IMapView_2__TGuid__IInspectable_Base;
  IMapView_2__TGuid__IInspectable = Winapi.CommonTypes.IMapView_2__TGuid__IInspectable;
  PIMapView_2__TGuid__IInspectable = Winapi.CommonTypes.PIMapView_2__TGuid__IInspectable;
  IPackage = Winapi.CommonTypes.IPackage;
  PIPackage = Winapi.CommonTypes.PIPackage;
  IPackageId = Winapi.CommonTypes.IPackageId;
  PIPackageId = Winapi.CommonTypes.PIPackageId;
  IReference_1__SmallInt = Winapi.CommonTypes.IReference_1__SmallInt;
  PIReference_1__SmallInt = Winapi.CommonTypes.PIReference_1__SmallInt;
  IVectorView_1__IPackage = Winapi.CommonTypes.IVectorView_1__IPackage;
  PIVectorView_1__IPackage = Winapi.CommonTypes.PIVectorView_1__IPackage;
  PackageVersion = Winapi.CommonTypes.PackageVersion;
  PPackageVersion = Winapi.CommonTypes.PPackageVersion;
  Search_ILocalContentSuggestionSettings = Winapi.CommonTypes.Search_ILocalContentSuggestionSettings;
  PSearch_ILocalContentSuggestionSettings = Winapi.CommonTypes.PSearch_ILocalContentSuggestionSettings;
  Search_ISearchQueryLinguisticDetails = Winapi.CommonTypes.Search_ISearchQueryLinguisticDetails;
  PSearch_ISearchQueryLinguisticDetails = Winapi.CommonTypes.PSearch_ISearchQueryLinguisticDetails;
  Search_ISearchSuggestionCollection = Winapi.CommonTypes.Search_ISearchSuggestionCollection;
  PSearch_ISearchSuggestionCollection = Winapi.CommonTypes.PSearch_ISearchSuggestionCollection;
  Search_ISearchSuggestionsRequest = Winapi.CommonTypes.Search_ISearchSuggestionsRequest;
  PSearch_ISearchSuggestionsRequest = Winapi.CommonTypes.PSearch_ISearchSuggestionsRequest;
  Search_ISearchSuggestionsRequestDeferral = Winapi.CommonTypes.Search_ISearchSuggestionsRequestDeferral;
  PSearch_ISearchSuggestionsRequestDeferral = Winapi.CommonTypes.PSearch_ISearchSuggestionsRequestDeferral;
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Activation_ISplashScreen__IInspectable_Delegate_Base;
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__Activation_ISplashScreen__IInspectable;
  PTypedEventHandler_2__Activation_ISplashScreen__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__Activation_ISplashScreen__IInspectable;
  TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable_Delegate_Base;
  TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable;
  PTypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable;
  TypedEventHandler_2__Audio_IAudioGraph__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Audio_IAudioGraph__IInspectable_Delegate_Base;
  TypedEventHandler_2__Audio_IAudioGraph__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__Audio_IAudioGraph__IInspectable;
  PTypedEventHandler_2__Audio_IAudioGraph__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__Audio_IAudioGraph__IInspectable;
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable_Delegate_Base;
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable;
  PTypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable;
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable_Delegate_Base;
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable;
  PTypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable;
  TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable_Delegate_Base;
  TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable;
  PTypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable;
  TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs;
  PTypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs;

  // Forward declarations for interfaces

  // Windows.ApplicationModel.IEnteredBackgroundEventArgs
  IEnteredBackgroundEventArgs = interface;
  PIEnteredBackgroundEventArgs = ^IEnteredBackgroundEventArgs;

  // Windows.ApplicationModel.ISuspendingDeferral
  ISuspendingDeferral = interface;
  PISuspendingDeferral = ^ISuspendingDeferral;

  // Windows.ApplicationModel.ISuspendingOperation
  ISuspendingOperation = interface;
  PISuspendingOperation = ^ISuspendingOperation;

  // Windows.ApplicationModel.ISuspendingEventArgs
  ISuspendingEventArgs = interface;
  PISuspendingEventArgs = ^ISuspendingEventArgs;

  // Windows.ApplicationModel.ILeavingBackgroundEventArgs
  ILeavingBackgroundEventArgs = interface;
  PILeavingBackgroundEventArgs = ^ILeavingBackgroundEventArgs;

  // Windows.ApplicationModel.Activation.ILaunchActivatedEventArgs
  Activation_ILaunchActivatedEventArgs = interface;
  PActivation_ILaunchActivatedEventArgs = ^Activation_ILaunchActivatedEventArgs;

  // Windows.ApplicationModel.Activation.IFileActivatedEventArgs
  Activation_IFileActivatedEventArgs = interface;
  PActivation_IFileActivatedEventArgs = ^Activation_IFileActivatedEventArgs;

  // Windows.ApplicationModel.Activation.ISearchActivatedEventArgs
  Activation_ISearchActivatedEventArgs = interface;
  PActivation_ISearchActivatedEventArgs = ^Activation_ISearchActivatedEventArgs;

  // Windows.ApplicationModel.Activation.IShareTargetActivatedEventArgs
  Activation_IShareTargetActivatedEventArgs = interface;
  PActivation_IShareTargetActivatedEventArgs = ^Activation_IShareTargetActivatedEventArgs;

  // Windows.ApplicationModel.Activation.IFileOpenPickerActivatedEventArgs
  Activation_IFileOpenPickerActivatedEventArgs = interface;
  PActivation_IFileOpenPickerActivatedEventArgs = ^Activation_IFileOpenPickerActivatedEventArgs;

  // Windows.ApplicationModel.Activation.IFileSavePickerActivatedEventArgs
  Activation_IFileSavePickerActivatedEventArgs = interface;
  PActivation_IFileSavePickerActivatedEventArgs = ^Activation_IFileSavePickerActivatedEventArgs;

  // Windows.ApplicationModel.Activation.ICachedFileUpdaterActivatedEventArgs
  Activation_ICachedFileUpdaterActivatedEventArgs = interface;
  PActivation_ICachedFileUpdaterActivatedEventArgs = ^Activation_ICachedFileUpdaterActivatedEventArgs;

  // Windows.ApplicationModel.Activation.IBackgroundActivatedEventArgs
  Activation_IBackgroundActivatedEventArgs = interface;
  PActivation_IBackgroundActivatedEventArgs = ^Activation_IBackgroundActivatedEventArgs;

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

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface;
  PIIterator_1__HSTRING = ^IIterator_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface;
  PIIterable_1__HSTRING = ^IIterable_1__HSTRING;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface;
  PIVectorView_1__HSTRING = ^IVectorView_1__HSTRING;

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

  // Windows.Foundation.IReference`1<UInt32>
  IReference_1__Cardinal = interface;
  PIReference_1__Cardinal = ^IReference_1__Cardinal;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Object>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable = interface;
  PTypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable = ^TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable;

  // Windows.ApplicationModel.Activation.IContactsProviderActivatedEventArgs
  Activation_IContactsProviderActivatedEventArgs = interface;
  PActivation_IContactsProviderActivatedEventArgs = ^Activation_IContactsProviderActivatedEventArgs;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  AsyncOperationCompletedHandler_1__Cardinal = interface;
  PAsyncOperationCompletedHandler_1__Cardinal = ^AsyncOperationCompletedHandler_1__Cardinal;

  // Windows.Foundation.IAsyncOperation`1<UInt32>
  IAsyncOperation_1__Cardinal = interface;
  PIAsyncOperation_1__Cardinal = ^IAsyncOperation_1__Cardinal;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Search.IStorageQueryResultBase,Object>
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = interface;
  PTypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = ^TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Object>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable = interface;
  PTypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable = ^TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  AsyncOperationCompletedHandler_1__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__HSTRING = ^AsyncOperationCompletedHandler_1__HSTRING;

  // Windows.Foundation.IAsyncOperation`1<String>
  IAsyncOperation_1__HSTRING = interface;
  PIAsyncOperation_1__HSTRING = ^IAsyncOperation_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>>
  IIterable_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = ^IIterable_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IPackage>
  IIterator_1__IPackage = interface;
  PIIterator_1__IPackage = ^IIterator_1__IPackage;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IPackage>
  IIterable_1__IPackage = interface;
  PIIterable_1__IPackage = ^IIterable_1__IPackage;

  // Windows.ApplicationModel.IAppInfo
  IAppInfo = interface;
  PIAppInfo = ^IAppInfo;

  // Windows.ApplicationModel.AppExtensions.IAppExtension
  AppExtensions_IAppExtension = interface;
  PAppExtensions_IAppExtension = ^AppExtensions_IAppExtension;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IIterator_1__AppExtensions_IAppExtension = interface;
  PIIterator_1__AppExtensions_IAppExtension = ^IIterator_1__AppExtensions_IAppExtension;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IIterable_1__AppExtensions_IAppExtension = interface;
  PIIterable_1__AppExtensions_IAppExtension = ^IIterable_1__AppExtensions_IAppExtension;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IVectorView_1__AppExtensions_IAppExtension = interface;
  PIVectorView_1__AppExtensions_IAppExtension = ^IVectorView_1__AppExtensions_IAppExtension;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>>
  AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension = ^AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>>
  IAsyncOperation_1__IVectorView_1__AppExtensions_IAppExtension = interface;
  PIAsyncOperation_1__IVectorView_1__AppExtensions_IAppExtension = ^IAsyncOperation_1__IVectorView_1__AppExtensions_IAppExtension;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageInstalledEventArgs
  AppExtensions_IAppExtensionPackageInstalledEventArgs = interface;
  PAppExtensions_IAppExtensionPackageInstalledEventArgs = ^AppExtensions_IAppExtensionPackageInstalledEventArgs;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageUpdatingEventArgs
  AppExtensions_IAppExtensionPackageUpdatingEventArgs = interface;
  PAppExtensions_IAppExtensionPackageUpdatingEventArgs = ^AppExtensions_IAppExtensionPackageUpdatingEventArgs;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageUpdatedEventArgs
  AppExtensions_IAppExtensionPackageUpdatedEventArgs = interface;
  PAppExtensions_IAppExtensionPackageUpdatedEventArgs = ^AppExtensions_IAppExtensionPackageUpdatedEventArgs;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageUninstallingEventArgs
  AppExtensions_IAppExtensionPackageUninstallingEventArgs = interface;
  PAppExtensions_IAppExtensionPackageUninstallingEventArgs = ^AppExtensions_IAppExtensionPackageUninstallingEventArgs;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageStatusChangedEventArgs
  AppExtensions_IAppExtensionPackageStatusChangedEventArgs = interface;
  PAppExtensions_IAppExtensionPackageStatusChangedEventArgs = ^AppExtensions_IAppExtensionPackageStatusChangedEventArgs;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IAppInfo>
  IIterator_1__IAppInfo = interface;
  PIIterator_1__IAppInfo = ^IIterator_1__IAppInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IAppInfo>
  IIterable_1__IAppInfo = interface;
  PIIterable_1__IAppInfo = ^IIterable_1__IAppInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>
  IVectorView_1__IAppInfo = interface;
  PIVectorView_1__IAppInfo = ^IVectorView_1__IAppInfo;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo = ^AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>>
  IAsyncOperation_1__IVectorView_1__IAppInfo = interface;
  PIAsyncOperation_1__IVectorView_1__IAppInfo = ^IAsyncOperation_1__IVectorView_1__IAppInfo;

  // Windows.ApplicationModel.AppService.IAppServiceClosedEventArgs
  AppService_IAppServiceClosedEventArgs = interface;
  PAppService_IAppServiceClosedEventArgs = ^AppService_IAppServiceClosedEventArgs;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.AppServiceConnectionStatus>
  AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus = interface;
  PAsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus = ^AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.AppServiceConnectionStatus>
  IAsyncOperation_1__AppService_AppServiceConnectionStatus = interface;
  PIAsyncOperation_1__AppService_AppServiceConnectionStatus = ^IAsyncOperation_1__AppService_AppServiceConnectionStatus;

  // Windows.ApplicationModel.AppService.IAppServiceResponse
  AppService_IAppServiceResponse = interface;
  PAppService_IAppServiceResponse = ^AppService_IAppServiceResponse;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.IAppServiceResponse>
  AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse = interface;
  PAsyncOperationCompletedHandler_1__AppService_IAppServiceResponse = ^AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.IAppServiceResponse>
  IAsyncOperation_1__AppService_IAppServiceResponse = interface;
  PIAsyncOperation_1__AppService_IAppServiceResponse = ^IAsyncOperation_1__AppService_IAppServiceResponse;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.AppServiceResponseStatus>
  AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus = interface;
  PAsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus = ^AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.AppServiceResponseStatus>
  IAsyncOperation_1__AppService_AppServiceResponseStatus = interface;
  PIAsyncOperation_1__AppService_AppServiceResponseStatus = ^IAsyncOperation_1__AppService_AppServiceResponseStatus;

  // Windows.ApplicationModel.AppService.IAppServiceRequest
  AppService_IAppServiceRequest = interface;
  PAppService_IAppServiceRequest = ^AppService_IAppServiceRequest;

  // Windows.ApplicationModel.AppService.IAppServiceDeferral
  AppService_IAppServiceDeferral = interface;
  PAppService_IAppServiceDeferral = ^AppService_IAppServiceDeferral;

  // Windows.ApplicationModel.AppService.IAppServiceRequestReceivedEventArgs
  AppService_IAppServiceRequestReceivedEventArgs = interface;
  PAppService_IAppServiceRequestReceivedEventArgs = ^AppService_IAppServiceRequestReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.AppService.IAppServiceConnection,Windows.ApplicationModel.AppService.IAppServiceRequestReceivedEventArgs>
  TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceRequestReceivedEventArgs = interface;
  PTypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceRequestReceivedEventArgs = ^TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceRequestReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.AppService.IAppServiceConnection,Windows.ApplicationModel.AppService.IAppServiceClosedEventArgs>
  TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceClosedEventArgs = interface;
  PTypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceClosedEventArgs = ^TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceClosedEventArgs;

  // Windows.ApplicationModel.AppService.IAppServiceConnection
  AppService_IAppServiceConnection = interface;
  PAppService_IAppServiceConnection = ^AppService_IAppServiceConnection;

  // Windows.ApplicationModel.AppService.IAppServiceConnection2
  AppService_IAppServiceConnection2 = interface;
  PAppService_IAppServiceConnection2 = ^AppService_IAppServiceConnection2;

  // Windows.ApplicationModel.AppService.IStatelessAppServiceResponse
  AppService_IStatelessAppServiceResponse = interface;
  PAppService_IStatelessAppServiceResponse = ^AppService_IStatelessAppServiceResponse;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.IStatelessAppServiceResponse>
  AsyncOperationCompletedHandler_1__AppService_IStatelessAppServiceResponse = interface;
  PAsyncOperationCompletedHandler_1__AppService_IStatelessAppServiceResponse = ^AsyncOperationCompletedHandler_1__AppService_IStatelessAppServiceResponse;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.IStatelessAppServiceResponse>
  IAsyncOperation_1__AppService_IStatelessAppServiceResponse = interface;
  PIAsyncOperation_1__AppService_IStatelessAppServiceResponse = ^IAsyncOperation_1__AppService_IStatelessAppServiceResponse;

  // Windows.ApplicationModel.AppService.IAppServiceConnectionStatics
  AppService_IAppServiceConnectionStatics = interface;
  PAppService_IAppServiceConnectionStatics = ^AppService_IAppServiceConnectionStatics;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails
  AppService_IAppServiceTriggerDetails = interface;
  PAppService_IAppServiceTriggerDetails = ^AppService_IAppServiceTriggerDetails;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails2
  AppService_IAppServiceTriggerDetails2 = interface;
  PAppService_IAppServiceTriggerDetails2 = ^AppService_IAppServiceTriggerDetails2;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails3
  AppService_IAppServiceTriggerDetails3 = interface;
  PAppService_IAppServiceTriggerDetails3 = ^AppService_IAppServiceTriggerDetails3;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails4
  AppService_IAppServiceTriggerDetails4 = interface;
  PAppService_IAppServiceTriggerDetails4 = ^AppService_IAppServiceTriggerDetails4;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<String>>
  AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__HSTRING = ^AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<String>>
  IAsyncOperation_1__IVectorView_1__HSTRING = interface;
  PIAsyncOperation_1__IVectorView_1__HSTRING = ^IAsyncOperation_1__IVectorView_1__HSTRING;

  // Windows.Foundation.Collections.IIterator`1<Guid>
  IIterator_1__TGuid = interface;
  PIIterator_1__TGuid = ^IIterator_1__TGuid;

  // Windows.Foundation.Collections.IIterable`1<Guid>
  IIterable_1__TGuid = interface;
  PIIterable_1__TGuid = ^IIterable_1__TGuid;

  // Windows.Foundation.Collections.IVectorView`1<Guid>
  IVectorView_1__TGuid = interface;
  PIVectorView_1__TGuid = ^IVectorView_1__TGuid;

  // Windows.Foundation.Collections.IVector`1<Guid>
  IVector_1__TGuid = interface;
  PIVector_1__TGuid = ^IVector_1__TGuid;

  // Windows.Foundation.IReference`1<UInt8>
  IReference_1__Byte = interface;
  PIReference_1__Byte = ^IReference_1__Byte;

  // Windows.Foundation.IReference`1<UInt64>
  IReference_1__UInt64 = interface;
  PIReference_1__UInt64 = ^IReference_1__UInt64;

  // Windows.Foundation.Collections.IIterator`1<Int32>
  IIterator_1__Integer = interface;
  PIIterator_1__Integer = ^IIterator_1__Integer;

  // Windows.Foundation.Collections.IIterable`1<Int32>
  IIterable_1__Integer = interface;
  PIIterable_1__Integer = ^IIterable_1__Integer;

  // Windows.Foundation.Collections.IVectorView`1<Int32>
  IVectorView_1__Integer = interface;
  PIVectorView_1__Integer = ^IVectorView_1__Integer;

  // Windows.Foundation.Collections.IVector`1<Int32>
  IVector_1__Integer = interface;
  PIVector_1__Integer = ^IVector_1__Integer;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface;
  PEventHandler_1__IInspectable = ^EventHandler_1__IInspectable;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Guid>
  AsyncOperationCompletedHandler_1__TGuid = interface;
  PAsyncOperationCompletedHandler_1__TGuid = ^AsyncOperationCompletedHandler_1__TGuid;

  // Windows.Foundation.IAsyncOperation`1<Guid>
  IAsyncOperation_1__TGuid = interface;
  PIAsyncOperation_1__TGuid = ^IAsyncOperation_1__TGuid;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Calls.VoipPhoneCallResourceReservationStatus>
  AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus = interface;
  PAsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus = ^AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Calls.VoipPhoneCallResourceReservationStatus>
  IAsyncOperation_1__Calls_VoipPhoneCallResourceReservationStatus = interface;
  PIAsyncOperation_1__Calls_VoipPhoneCallResourceReservationStatus = ^IAsyncOperation_1__Calls_VoipPhoneCallResourceReservationStatus;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>
  IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = interface;
  PIKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = ^IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>>
  IIterator_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = ^IIterator_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>>
  IIterable_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = ^IIterable_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>
  IMapView_2__HSTRING__Chat_ChatMessageStatus = interface;
  PIMapView_2__HSTRING__Chat_ChatMessageStatus = ^IMapView_2__HSTRING__Chat_ChatMessageStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Int32>
  AsyncOperationCompletedHandler_1__Integer = interface;
  PAsyncOperationCompletedHandler_1__Integer = ^AsyncOperationCompletedHandler_1__Integer;

  // Windows.Foundation.IAsyncOperation`1<Int32>
  IAsyncOperation_1__Integer = interface;
  PIAsyncOperation_1__Integer = ^IAsyncOperation_1__Integer;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>>
  IIterable_1__IKeyValuePair_2__TGuid__IInspectable = interface;
  PIIterable_1__IKeyValuePair_2__TGuid__IInspectable = ^IIterable_1__IKeyValuePair_2__TGuid__IInspectable;

  // Windows.Foundation.IReference`1<Int32>
  IReference_1__Integer = interface;
  PIReference_1__Integer = ^IReference_1__Integer;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ConversationalAgent.DetectionConfigurationTrainingStatus>
  AsyncOperationCompletedHandler_1__ConversationalAgent_DetectionConfigurationTrainingStatus = interface;
  PAsyncOperationCompletedHandler_1__ConversationalAgent_DetectionConfigurationTrainingStatus = ^AsyncOperationCompletedHandler_1__ConversationalAgent_DetectionConfigurationTrainingStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ConversationalAgent.DetectionConfigurationTrainingStatus>
  IAsyncOperation_1__ConversationalAgent_DetectionConfigurationTrainingStatus = interface;
  PIAsyncOperation_1__ConversationalAgent_DetectionConfigurationTrainingStatus = ^IAsyncOperation_1__ConversationalAgent_DetectionConfigurationTrainingStatus;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectionTrainingDataFormat>
  IIterator_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = interface;
  PIIterator_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = ^IIterator_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectionTrainingDataFormat>
  IIterable_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = interface;
  PIIterable_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = ^IIterable_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectionTrainingDataFormat>
  IVectorView_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = interface;
  PIVectorView_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = ^IVectorView_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorPowerState>
  IIterator_1__ConversationalAgent_ActivationSignalDetectorPowerState = interface;
  PIIterator_1__ConversationalAgent_ActivationSignalDetectorPowerState = ^IIterator_1__ConversationalAgent_ActivationSignalDetectorPowerState;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorPowerState>
  IIterable_1__ConversationalAgent_ActivationSignalDetectorPowerState = interface;
  PIIterable_1__ConversationalAgent_ActivationSignalDetectorPowerState = ^IIterable_1__ConversationalAgent_ActivationSignalDetectorPowerState;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorPowerState>
  IVectorView_1__ConversationalAgent_ActivationSignalDetectorPowerState = interface;
  PIVectorView_1__ConversationalAgent_ActivationSignalDetectorPowerState = ^IVectorView_1__ConversationalAgent_ActivationSignalDetectorPowerState;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ConversationalAgent.ConversationalAgentSessionUpdateResponse>
  AsyncOperationCompletedHandler_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse = interface;
  PAsyncOperationCompletedHandler_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse = ^AsyncOperationCompletedHandler_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ConversationalAgent.ConversationalAgentSessionUpdateResponse>
  IAsyncOperation_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse = interface;
  PIAsyncOperation_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse = ^IAsyncOperation_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface;
  PTypedEventHandler_2__IMemoryBufferReference__IInspectable = ^TypedEventHandler_2__IMemoryBufferReference__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<UInt32>
  IIterator_1__Cardinal = interface;
  PIIterator_1__Cardinal = ^IIterator_1__Cardinal;

  // Windows.Foundation.Collections.IIterable`1<UInt32>
  IIterable_1__Cardinal = interface;
  PIIterable_1__Cardinal = ^IIterable_1__Cardinal;

  // Windows.Foundation.Collections.IVectorView`1<UInt32>
  IVectorView_1__Cardinal = interface;
  PIVectorView_1__Cardinal = ^IVectorView_1__Cardinal;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<UInt32>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Cardinal = ^AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<UInt32>>
  IAsyncOperation_1__IVectorView_1__Cardinal = interface;
  PIAsyncOperation_1__IVectorView_1__Cardinal = ^IAsyncOperation_1__IVectorView_1__Cardinal;

  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.ISuspendingEventArgs>
  EventHandler_1__ISuspendingEventArgs = interface;
  PEventHandler_1__ISuspendingEventArgs = ^EventHandler_1__ISuspendingEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.Activation.IBackgroundActivatedEventArgs>
  EventHandler_1__Activation_IBackgroundActivatedEventArgs = interface;
  PEventHandler_1__Activation_IBackgroundActivatedEventArgs = ^EventHandler_1__Activation_IBackgroundActivatedEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.ILeavingBackgroundEventArgs>
  EventHandler_1__ILeavingBackgroundEventArgs = interface;
  PEventHandler_1__ILeavingBackgroundEventArgs = ^EventHandler_1__ILeavingBackgroundEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.IEnteredBackgroundEventArgs>
  EventHandler_1__IEnteredBackgroundEventArgs = interface;
  PEventHandler_1__IEnteredBackgroundEventArgs = ^EventHandler_1__IEnteredBackgroundEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueueTimer,Object>
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueueTimer__IInspectable = ^TypedEventHandler_2__IDispatcherQueueTimer__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Object>
  TypedEventHandler_2__IDispatcherQueue__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueue__IInspectable = ^TypedEventHandler_2__IDispatcherQueue__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Object>
  TypedEventHandler_2__IDataPackage__IInspectable = interface;
  PTypedEventHandler_2__IDataPackage__IInspectable = ^TypedEventHandler_2__IDataPackage__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IIterator_1__Email_EmailCertificateValidationStatus = interface;
  PIIterator_1__Email_EmailCertificateValidationStatus = ^IIterator_1__Email_EmailCertificateValidationStatus;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IIterable_1__Email_EmailCertificateValidationStatus = interface;
  PIIterable_1__Email_EmailCertificateValidationStatus = ^IIterable_1__Email_EmailCertificateValidationStatus;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.Email.EmailMailboxSmimeEncryptionAlgorithm>
  IReference_1__Email_EmailMailboxSmimeEncryptionAlgorithm = interface;
  PIReference_1__Email_EmailMailboxSmimeEncryptionAlgorithm = ^IReference_1__Email_EmailMailboxSmimeEncryptionAlgorithm;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.Email.EmailMailboxSmimeSigningAlgorithm>
  IReference_1__Email_EmailMailboxSmimeSigningAlgorithm = interface;
  PIReference_1__Email_EmailMailboxSmimeSigningAlgorithm = ^IReference_1__Email_EmailMailboxSmimeSigningAlgorithm;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IVectorView_1__Email_EmailCertificateValidationStatus = interface;
  PIVectorView_1__Email_EmailCertificateValidationStatus = ^IVectorView_1__Email_EmailCertificateValidationStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus = ^AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>>
  IAsyncOperation_1__IVectorView_1__Email_EmailCertificateValidationStatus = interface;
  PIAsyncOperation_1__IVectorView_1__Email_EmailCertificateValidationStatus = ^IAsyncOperation_1__IVectorView_1__Email_EmailCertificateValidationStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Email.EmailMailboxEmptyFolderStatus>
  AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus = interface;
  PAsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus = ^AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Email.EmailMailboxEmptyFolderStatus>
  IAsyncOperation_1__Email_EmailMailboxEmptyFolderStatus = interface;
  PIAsyncOperation_1__Email_EmailMailboxEmptyFolderStatus = ^IAsyncOperation_1__Email_EmailMailboxEmptyFolderStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Email.EmailMailboxDeleteFolderStatus>
  AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus = interface;
  PAsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus = ^AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Email.EmailMailboxDeleteFolderStatus>
  IAsyncOperation_1__Email_EmailMailboxDeleteFolderStatus = interface;
  PIAsyncOperation_1__Email_EmailMailboxDeleteFolderStatus = ^IAsyncOperation_1__Email_EmailMailboxDeleteFolderStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundResult>
  AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult = interface;
  PAsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult = ^AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundResult>
  IAsyncOperation_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult = interface;
  PIAsyncOperation_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult = ^IAsyncOperation_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionResult>
  AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult = interface;
  PAsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult = ^AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionResult>
  IAsyncOperation_1__ExtendedExecution_ExtendedExecutionResult = interface;
  PIAsyncOperation_1__ExtendedExecution_ExtendedExecutionResult = ^IAsyncOperation_1__ExtendedExecution_ExtendedExecutionResult;

  // Windows.ApplicationModel.IAppInfo2
  IAppInfo2 = interface;
  PIAppInfo2 = ^IAppInfo2;

  // Windows.ApplicationModel.IAppInfoStatics
  IAppInfoStatics = interface;
  PIAppInfoStatics = ^IAppInfoStatics;

  // Windows.ApplicationModel.IAppInstallerInfo
  IAppInstallerInfo = interface;
  PIAppInstallerInfo = ^IAppInstallerInfo;

  // Windows.ApplicationModel.IPackage2
  IPackage2 = interface;
  PIPackage2 = ^IPackage2;

  // Windows.ApplicationModel.IPackageStatus
  IPackageStatus = interface;
  PIPackageStatus = ^IPackageStatus;

  // Windows.ApplicationModel.IPackage3
  IPackage3 = interface;
  PIPackage3 = ^IPackage3;

  // Windows.ApplicationModel.IPackage4
  IPackage4 = interface;
  PIPackage4 = ^IPackage4;

  // Windows.ApplicationModel.IPackageContentGroup
  IPackageContentGroup = interface;
  PIPackageContentGroup = ^IPackageContentGroup;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IPackageContentGroup>
  IIterator_1__IPackageContentGroup = interface;
  PIIterator_1__IPackageContentGroup = ^IIterator_1__IPackageContentGroup;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IPackageContentGroup>
  IIterable_1__IPackageContentGroup = interface;
  PIIterable_1__IPackageContentGroup = ^IIterable_1__IPackageContentGroup;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IPackageContentGroup>
  IVectorView_1__IPackageContentGroup = interface;
  PIVectorView_1__IPackageContentGroup = ^IVectorView_1__IPackageContentGroup;

  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>
  IVector_1__IPackageContentGroup = interface;
  PIVector_1__IPackageContentGroup = ^IVector_1__IPackageContentGroup;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>>
  AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup = interface;
  PAsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup = ^AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>>
  IAsyncOperation_1__IVector_1__IPackageContentGroup = interface;
  PIAsyncOperation_1__IVector_1__IPackageContentGroup = ^IAsyncOperation_1__IVector_1__IPackageContentGroup;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.IPackageContentGroup>
  AsyncOperationCompletedHandler_1__IPackageContentGroup = interface;
  PAsyncOperationCompletedHandler_1__IPackageContentGroup = ^AsyncOperationCompletedHandler_1__IPackageContentGroup;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.IPackageContentGroup>
  IAsyncOperation_1__IPackageContentGroup = interface;
  PIAsyncOperation_1__IPackageContentGroup = ^IAsyncOperation_1__IPackageContentGroup;

  // Windows.ApplicationModel.IPackage5
  IPackage5 = interface;
  PIPackage5 = ^IPackage5;

  // Windows.ApplicationModel.IPackageUpdateAvailabilityResult
  IPackageUpdateAvailabilityResult = interface;
  PIPackageUpdateAvailabilityResult = ^IPackageUpdateAvailabilityResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.IPackageUpdateAvailabilityResult>
  AsyncOperationCompletedHandler_1__IPackageUpdateAvailabilityResult = interface;
  PAsyncOperationCompletedHandler_1__IPackageUpdateAvailabilityResult = ^AsyncOperationCompletedHandler_1__IPackageUpdateAvailabilityResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.IPackageUpdateAvailabilityResult>
  IAsyncOperation_1__IPackageUpdateAvailabilityResult = interface;
  PIAsyncOperation_1__IPackageUpdateAvailabilityResult = ^IAsyncOperation_1__IPackageUpdateAvailabilityResult;

  // Windows.ApplicationModel.IPackage6
  IPackage6 = interface;
  PIPackage6 = ^IPackage6;

  // Windows.ApplicationModel.IPackage7
  IPackage7 = interface;
  PIPackage7 = ^IPackage7;

  // Windows.ApplicationModel.IPackage8
  IPackage8 = interface;
  PIPackage8 = ^IPackage8;

  // Windows.ApplicationModel.IPackageStagingEventArgs
  IPackageStagingEventArgs = interface;
  PIPackageStagingEventArgs = ^IPackageStagingEventArgs;

  // Windows.ApplicationModel.IPackageInstallingEventArgs
  IPackageInstallingEventArgs = interface;
  PIPackageInstallingEventArgs = ^IPackageInstallingEventArgs;

  // Windows.ApplicationModel.IPackageUpdatingEventArgs
  IPackageUpdatingEventArgs = interface;
  PIPackageUpdatingEventArgs = ^IPackageUpdatingEventArgs;

  // Windows.ApplicationModel.IPackageUninstallingEventArgs
  IPackageUninstallingEventArgs = interface;
  PIPackageUninstallingEventArgs = ^IPackageUninstallingEventArgs;

  // Windows.ApplicationModel.IPackageStatusChangedEventArgs
  IPackageStatusChangedEventArgs = interface;
  PIPackageStatusChangedEventArgs = ^IPackageStatusChangedEventArgs;

  // Windows.ApplicationModel.IPackageContentGroupStagingEventArgs
  IPackageContentGroupStagingEventArgs = interface;
  PIPackageContentGroupStagingEventArgs = ^IPackageContentGroupStagingEventArgs;

  // Windows.ApplicationModel.IPackageCatalogRemoveResourcePackagesResult
  IPackageCatalogRemoveResourcePackagesResult = interface;
  PIPackageCatalogRemoveResourcePackagesResult = ^IPackageCatalogRemoveResourcePackagesResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.IPackageCatalogRemoveResourcePackagesResult>
  AsyncOperationCompletedHandler_1__IPackageCatalogRemoveResourcePackagesResult = interface;
  PAsyncOperationCompletedHandler_1__IPackageCatalogRemoveResourcePackagesResult = ^AsyncOperationCompletedHandler_1__IPackageCatalogRemoveResourcePackagesResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.IPackageCatalogRemoveResourcePackagesResult>
  IAsyncOperation_1__IPackageCatalogRemoveResourcePackagesResult = interface;
  PIAsyncOperation_1__IPackageCatalogRemoveResourcePackagesResult = ^IAsyncOperation_1__IPackageCatalogRemoveResourcePackagesResult;

  // Windows.ApplicationModel.IPackageIdWithMetadata
  IPackageIdWithMetadata = interface;
  PIPackageIdWithMetadata = ^IPackageIdWithMetadata;

  // Windows.ApplicationModel.IPackageStatics
  IPackageStatics = interface;
  PIPackageStatics = ^IPackageStatics;

  // Windows.ApplicationModel.IPackageStatus2
  IPackageStatus2 = interface;
  PIPackageStatus2 = ^IPackageStatus2;

  // Windows.ApplicationModel.IPackageWithMetadata
  IPackageWithMetadata = interface;
  PIPackageWithMetadata = ^IPackageWithMetadata;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.StartupTaskState>
  AsyncOperationCompletedHandler_1__StartupTaskState = interface;
  PAsyncOperationCompletedHandler_1__StartupTaskState = ^AsyncOperationCompletedHandler_1__StartupTaskState;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.StartupTaskState>
  IAsyncOperation_1__StartupTaskState = interface;
  PIAsyncOperation_1__StartupTaskState = ^IAsyncOperation_1__StartupTaskState;

  // Windows.ApplicationModel.LockScreen.ILockScreenUnlockingDeferral
  LockScreen_ILockScreenUnlockingDeferral = interface;
  PLockScreen_ILockScreenUnlockingDeferral = ^LockScreen_ILockScreenUnlockingDeferral;

  // Windows.ApplicationModel.LockScreen.ILockScreenUnlockingEventArgs
  LockScreen_ILockScreenUnlockingEventArgs = interface;
  PLockScreen_ILockScreenUnlockingEventArgs = ^LockScreen_ILockScreenUnlockingEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.LockScreen.ILockApplicationHost,Windows.ApplicationModel.LockScreen.ILockScreenUnlockingEventArgs>
  TypedEventHandler_2__LockScreen_ILockApplicationHost__LockScreen_ILockScreenUnlockingEventArgs = interface;
  PTypedEventHandler_2__LockScreen_ILockApplicationHost__LockScreen_ILockScreenUnlockingEventArgs = ^TypedEventHandler_2__LockScreen_ILockApplicationHost__LockScreen_ILockScreenUnlockingEventArgs;

  // Windows.ApplicationModel.LockScreen.ILockApplicationHost
  LockScreen_ILockApplicationHost = interface;
  PLockScreen_ILockApplicationHost = ^LockScreen_ILockApplicationHost;

  // Windows.ApplicationModel.LockScreen.ILockApplicationHostStatics
  LockScreen_ILockApplicationHostStatics = interface;
  PLockScreen_ILockApplicationHostStatics = ^LockScreen_ILockApplicationHostStatics;

  // Windows.ApplicationModel.LockScreen.ILockScreenBadge
  LockScreen_ILockScreenBadge = interface;
  PLockScreen_ILockScreenBadge = ^LockScreen_ILockScreenBadge;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.LockScreen.ILockScreenInfo,Object>
  TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable = interface;
  PTypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable = ^TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IIterator_1__LockScreen_ILockScreenBadge = interface;
  PIIterator_1__LockScreen_ILockScreenBadge = ^IIterator_1__LockScreen_ILockScreenBadge;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IIterable_1__LockScreen_ILockScreenBadge = interface;
  PIIterable_1__LockScreen_ILockScreenBadge = ^IIterable_1__LockScreen_ILockScreenBadge;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IVectorView_1__LockScreen_ILockScreenBadge = interface;
  PIVectorView_1__LockScreen_ILockScreenBadge = ^IVectorView_1__LockScreen_ILockScreenBadge;

  // Windows.ApplicationModel.LockScreen.ILockScreenInfo
  LockScreen_ILockScreenInfo = interface;
  PLockScreen_ILockScreenInfo = ^LockScreen_ILockScreenInfo;

  // Windows.ApplicationModel.Payments.IPaymentAddress
  Payments_IPaymentAddress = interface;
  PPayments_IPaymentAddress = ^Payments_IPaymentAddress;

  // Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult
  Payments_IPaymentCanMakePaymentResult = interface;
  PPayments_IPaymentCanMakePaymentResult = ^Payments_IPaymentCanMakePaymentResult;

  // Windows.ApplicationModel.Payments.IPaymentCurrencyAmount
  Payments_IPaymentCurrencyAmount = interface;
  PPayments_IPaymentCurrencyAmount = ^Payments_IPaymentCurrencyAmount;

  // Windows.ApplicationModel.Payments.IPaymentItem
  Payments_IPaymentItem = interface;
  PPayments_IPaymentItem = ^Payments_IPaymentItem;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IIterator_1__Payments_IPaymentItem = interface;
  PIIterator_1__Payments_IPaymentItem = ^IIterator_1__Payments_IPaymentItem;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IIterable_1__Payments_IPaymentItem = interface;
  PIIterable_1__Payments_IPaymentItem = ^IIterable_1__Payments_IPaymentItem;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IVectorView_1__Payments_IPaymentItem = interface;
  PIVectorView_1__Payments_IPaymentItem = ^IVectorView_1__Payments_IPaymentItem;

  // Windows.ApplicationModel.Payments.IPaymentShippingOption
  Payments_IPaymentShippingOption = interface;
  PPayments_IPaymentShippingOption = ^Payments_IPaymentShippingOption;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IIterator_1__Payments_IPaymentShippingOption = interface;
  PIIterator_1__Payments_IPaymentShippingOption = ^IIterator_1__Payments_IPaymentShippingOption;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IIterable_1__Payments_IPaymentShippingOption = interface;
  PIIterable_1__Payments_IPaymentShippingOption = ^IIterable_1__Payments_IPaymentShippingOption;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IVectorView_1__Payments_IPaymentShippingOption = interface;
  PIVectorView_1__Payments_IPaymentShippingOption = ^IVectorView_1__Payments_IPaymentShippingOption;

  // Windows.ApplicationModel.Payments.IPaymentDetailsModifier
  Payments_IPaymentDetailsModifier = interface;
  PPayments_IPaymentDetailsModifier = ^Payments_IPaymentDetailsModifier;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IIterator_1__Payments_IPaymentDetailsModifier = interface;
  PIIterator_1__Payments_IPaymentDetailsModifier = ^IIterator_1__Payments_IPaymentDetailsModifier;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IIterable_1__Payments_IPaymentDetailsModifier = interface;
  PIIterable_1__Payments_IPaymentDetailsModifier = ^IIterable_1__Payments_IPaymentDetailsModifier;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IVectorView_1__Payments_IPaymentDetailsModifier = interface;
  PIVectorView_1__Payments_IPaymentDetailsModifier = ^IVectorView_1__Payments_IPaymentDetailsModifier;

  // Windows.ApplicationModel.Payments.IPaymentDetails
  Payments_IPaymentDetails = interface;
  PPayments_IPaymentDetails = ^Payments_IPaymentDetails;

  // Windows.ApplicationModel.Payments.IPaymentToken
  Payments_IPaymentToken = interface;
  PPayments_IPaymentToken = ^Payments_IPaymentToken;

  // Windows.ApplicationModel.Payments.IPaymentResponse
  Payments_IPaymentResponse = interface;
  PPayments_IPaymentResponse = ^Payments_IPaymentResponse;

  // Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult
  Payments_IPaymentRequestSubmitResult = interface;
  PPayments_IPaymentRequestSubmitResult = ^Payments_IPaymentRequestSubmitResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult = interface;
  PAsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult = ^AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult>
  IAsyncOperation_1__Payments_IPaymentRequestSubmitResult = interface;
  PIAsyncOperation_1__Payments_IPaymentRequestSubmitResult = ^IAsyncOperation_1__Payments_IPaymentRequestSubmitResult;

  // Windows.ApplicationModel.Payments.IPaymentMerchantInfo
  Payments_IPaymentMerchantInfo = interface;
  PPayments_IPaymentMerchantInfo = ^Payments_IPaymentMerchantInfo;

  // Windows.ApplicationModel.Payments.IPaymentMethodData
  Payments_IPaymentMethodData = interface;
  PPayments_IPaymentMethodData = ^Payments_IPaymentMethodData;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IIterator_1__Payments_IPaymentMethodData = interface;
  PIIterator_1__Payments_IPaymentMethodData = ^IIterator_1__Payments_IPaymentMethodData;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IIterable_1__Payments_IPaymentMethodData = interface;
  PIIterable_1__Payments_IPaymentMethodData = ^IIterable_1__Payments_IPaymentMethodData;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IVectorView_1__Payments_IPaymentMethodData = interface;
  PIVectorView_1__Payments_IPaymentMethodData = ^IVectorView_1__Payments_IPaymentMethodData;

  // Windows.ApplicationModel.Payments.IPaymentOptions
  Payments_IPaymentOptions = interface;
  PPayments_IPaymentOptions = ^Payments_IPaymentOptions;

  // Windows.ApplicationModel.Payments.IPaymentRequest
  Payments_IPaymentRequest = interface;
  PPayments_IPaymentRequest = ^Payments_IPaymentRequest;

  // Windows.ApplicationModel.Payments.IPaymentRequestChangedResult
  Payments_IPaymentRequestChangedResult = interface;
  PPayments_IPaymentRequestChangedResult = ^Payments_IPaymentRequestChangedResult;

  // Windows.ApplicationModel.Payments.IPaymentRequestChangedArgs
  Payments_IPaymentRequestChangedArgs = interface;
  PPayments_IPaymentRequestChangedArgs = ^Payments_IPaymentRequestChangedArgs;

  // Windows.ApplicationModel.Payments.PaymentRequestChangedHandler
  Payments_PaymentRequestChangedHandler = interface;
  PPayments_PaymentRequestChangedHandler = ^Payments_PaymentRequestChangedHandler;

  // Windows.ApplicationModel.Payments.IPaymentMediator
  Payments_IPaymentMediator = interface;
  PPayments_IPaymentMediator = ^Payments_IPaymentMediator;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult = interface;
  PAsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult = ^AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult>
  IAsyncOperation_1__Payments_IPaymentCanMakePaymentResult = interface;
  PIAsyncOperation_1__Payments_IPaymentCanMakePaymentResult = ^IAsyncOperation_1__Payments_IPaymentCanMakePaymentResult;

  // Windows.ApplicationModel.Payments.IPaymentMediator2
  Payments_IPaymentMediator2 = interface;
  PPayments_IPaymentMediator2 = ^Payments_IPaymentMediator2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentRequestChangedResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult = interface;
  PAsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult = ^AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentRequestChangedResult>
  IAsyncOperation_1__Payments_IPaymentRequestChangedResult = interface;
  PIAsyncOperation_1__Payments_IPaymentRequestChangedResult = ^IAsyncOperation_1__Payments_IPaymentRequestChangedResult;

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

  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,String>
  MapChangedEventHandler_2__HSTRING__HSTRING = interface;
  PMapChangedEventHandler_2__HSTRING__HSTRING = ^MapChangedEventHandler_2__HSTRING__HSTRING;

  // Windows.Foundation.Collections.IObservableMap`2<String,String>
  IObservableMap_2__HSTRING__HSTRING = interface;
  PIObservableMap_2__HSTRING__HSTRING = ^IObservableMap_2__HSTRING__HSTRING;

  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.ApplicationModel.Search.Core.ISearchSuggestion>
  VectorChangedEventHandler_1__Search_Core_ISearchSuggestion = interface;
  PVectorChangedEventHandler_1__Search_Core_ISearchSuggestion = ^VectorChangedEventHandler_1__Search_Core_ISearchSuggestion;

  // Windows.Foundation.Collections.IObservableVector`1<Windows.ApplicationModel.Search.Core.ISearchSuggestion>
  IObservableVector_1__Search_Core_ISearchSuggestion = interface;
  PIObservableVector_1__Search_Core_ISearchSuggestion = ^IObservableVector_1__Search_Core_ISearchSuggestion;

  // Windows.ApplicationModel.Store.LicenseChangedEventHandler
  Store_LicenseChangedEventHandler = interface;
  PStore_LicenseChangedEventHandler = ^Store_LicenseChangedEventHandler;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Store.FulfillmentResult>
  AsyncOperationCompletedHandler_1__Store_FulfillmentResult = interface;
  PAsyncOperationCompletedHandler_1__Store_FulfillmentResult = ^AsyncOperationCompletedHandler_1__Store_FulfillmentResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Store.FulfillmentResult>
  IAsyncOperation_1__Store_FulfillmentResult = interface;
  PIAsyncOperation_1__Store_FulfillmentResult = ^IAsyncOperation_1__Store_FulfillmentResult;

  // Windows.ApplicationModel.Store.IProductListingWithConsumables
  Store_IProductListingWithConsumables = interface;
  PStore_IProductListingWithConsumables = ^Store_IProductListingWithConsumables;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IIterator_1__Store_Preview_StoreSystemFeature = interface;
  PIIterator_1__Store_Preview_StoreSystemFeature = ^IIterator_1__Store_Preview_StoreSystemFeature;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IIterable_1__Store_Preview_StoreSystemFeature = interface;
  PIIterable_1__Store_Preview_StoreSystemFeature = ^IIterable_1__Store_Preview_StoreSystemFeature;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IVectorView_1__Store_Preview_StoreSystemFeature = interface;
  PIVectorView_1__Store_Preview_StoreSystemFeature = ^IVectorView_1__Store_Preview_StoreSystemFeature;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature = ^AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>>
  IAsyncOperation_1__IVectorView_1__Store_Preview_StoreSystemFeature = interface;
  PIAsyncOperation_1__IVectorView_1__Store_Preview_StoreSystemFeature = ^IAsyncOperation_1__IVectorView_1__Store_Preview_StoreSystemFeature;

  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackage>
  IVector_1__IPackage = interface;
  PIVector_1__IPackage = ^IVector_1__IPackage;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.UserDataTasks.UserDataTaskDaysOfWeek>
  IReference_1__UserDataTasks_UserDataTaskDaysOfWeek = interface;
  PIReference_1__UserDataTasks_UserDataTaskDaysOfWeek = ^IReference_1__UserDataTasks_UserDataTaskDaysOfWeek;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.UserDataTasks.UserDataTaskWeekOfMonth>
  IReference_1__UserDataTasks_UserDataTaskWeekOfMonth = interface;
  PIReference_1__UserDataTasks_UserDataTaskWeekOfMonth = ^IReference_1__UserDataTasks_UserDataTaskWeekOfMonth;

  // Windows.ApplicationModel Enums

  // Windows.ApplicationModel.AddResourcePackageOptions
  AddResourcePackageOptions = (
    None = 0,
    ForceTargetAppShutdown = 1,
    ApplyUpdateIfAvailable = 2
  );
  PAddResourcePackageOptions = ^AddResourcePackageOptions;

  // Windows.ApplicationModel.AppService.AppServiceClosedStatus
  AppService_AppServiceClosedStatus = (
    Completed = 0,
    Canceled = 1,
    ResourceLimitsExceeded = 2,
    Unknown = 3
  );
  PAppService_AppServiceClosedStatus = ^AppService_AppServiceClosedStatus;

  // Windows.ApplicationModel.AppService.AppServiceConnectionStatus
  AppService_AppServiceConnectionStatus = (
    Success = 0,
    AppNotInstalled = 1,
    AppUnavailable = 2,
    AppServiceUnavailable = 3,
    Unknown = 4,
    RemoteSystemUnavailable = 5,
    RemoteSystemNotSupportedByApp = 6,
    NotAuthorized = 7,
    AuthenticationError = 8,
    NetworkNotAvailable = 9,
    DisabledByPolicy = 10,
    WebServiceUnavailable = 11
  );
  PAppService_AppServiceConnectionStatus = ^AppService_AppServiceConnectionStatus;

  // Windows.ApplicationModel.AppService.AppServiceResponseStatus
  AppService_AppServiceResponseStatus = (
    Success = 0,
    Failure = 1,
    ResourceLimitsExceeded = 2,
    Unknown = 3,
    RemoteSystemUnavailable = 4,
    MessageSizeTooLarge = 5,
    AppUnavailable = 6,
    AuthenticationError = 7,
    NetworkNotAvailable = 8,
    DisabledByPolicy = 9,
    WebServiceUnavailable = 10
  );
  PAppService_AppServiceResponseStatus = ^AppService_AppServiceResponseStatus;

  // Windows.ApplicationModel.AppService.StatelessAppServiceResponseStatus
  AppService_StatelessAppServiceResponseStatus = (
    Success = 0,
    AppNotInstalled = 1,
    AppUnavailable = 2,
    AppServiceUnavailable = 3,
    RemoteSystemUnavailable = 4,
    RemoteSystemNotSupportedByApp = 5,
    NotAuthorized = 6,
    ResourceLimitsExceeded = 7,
    MessageSizeTooLarge = 8,
    Failure = 9,
    Unknown = 10,
    AuthenticationError = 11,
    NetworkNotAvailable = 12,
    DisabledByPolicy = 13,
    WebServiceUnavailable = 14
  );
  PAppService_StatelessAppServiceResponseStatus = ^AppService_StatelessAppServiceResponseStatus;

  // Windows.ApplicationModel.Appointments.AppointmentBusyStatus
  Appointments_AppointmentBusyStatus = (
    Busy = 0,
    Tentative = 1,
    Free = 2,
    OutOfOffice = 3,
    WorkingElsewhere = 4
  );
  PAppointments_AppointmentBusyStatus = ^Appointments_AppointmentBusyStatus;

  // Windows.ApplicationModel.Appointments.AppointmentCalendarOtherAppReadAccess
  Appointments_AppointmentCalendarOtherAppReadAccess = (
    SystemOnly = 0,
    Limited = 1,
    Full = 2,
    None = 3
  );
  PAppointments_AppointmentCalendarOtherAppReadAccess = ^Appointments_AppointmentCalendarOtherAppReadAccess;

  // Windows.ApplicationModel.Appointments.AppointmentCalendarOtherAppWriteAccess
  Appointments_AppointmentCalendarOtherAppWriteAccess = (
    None = 0,
    SystemOnly = 1,
    Limited = 2
  );
  PAppointments_AppointmentCalendarOtherAppWriteAccess = ^Appointments_AppointmentCalendarOtherAppWriteAccess;

  // Windows.ApplicationModel.Appointments.AppointmentCalendarSyncStatus
  Appointments_AppointmentCalendarSyncStatus = (
    Idle = 0,
    Syncing = 1,
    UpToDate = 2,
    AuthenticationError = 3,
    PolicyError = 4,
    UnknownError = 5,
    ManualAccountRemovalRequired = 6
  );
  PAppointments_AppointmentCalendarSyncStatus = ^Appointments_AppointmentCalendarSyncStatus;

  // Windows.ApplicationModel.Appointments.AppointmentConflictType
  Appointments_AppointmentConflictType = (
    None = 0,
    Adjacent = 1,
    Overlap = 2
  );
  PAppointments_AppointmentConflictType = ^Appointments_AppointmentConflictType;

  // Windows.ApplicationModel.Appointments.AppointmentDaysOfWeek
  Appointments_AppointmentDaysOfWeek = (
    None = 0,
    Sunday = 1,
    Monday = 2,
    Tuesday = 4,
    Wednesday = 8,
    Thursday = 16,
    Friday = 32,
    Saturday = 64
  );
  PAppointments_AppointmentDaysOfWeek = ^Appointments_AppointmentDaysOfWeek;

  // Windows.ApplicationModel.Appointments.AppointmentDetailsKind
  Appointments_AppointmentDetailsKind = (
    PlainText = 0,
    Html = 1
  );
  PAppointments_AppointmentDetailsKind = ^Appointments_AppointmentDetailsKind;

  // Windows.ApplicationModel.Appointments.AppointmentParticipantResponse
  Appointments_AppointmentParticipantResponse = (
    None = 0,
    Tentative = 1,
    Accepted = 2,
    Declined = 3,
    Unknown = 4
  );
  PAppointments_AppointmentParticipantResponse = ^Appointments_AppointmentParticipantResponse;

  // Windows.ApplicationModel.Appointments.AppointmentParticipantRole
  Appointments_AppointmentParticipantRole = (
    RequiredAttendee = 0,
    OptionalAttendee = 1,
    Resource = 2
  );
  PAppointments_AppointmentParticipantRole = ^Appointments_AppointmentParticipantRole;

  // Windows.ApplicationModel.Appointments.AppointmentRecurrenceUnit
  Appointments_AppointmentRecurrenceUnit = (
    Daily = 0,
    Weekly = 1,
    Monthly = 2,
    MonthlyOnDay = 3,
    Yearly = 4,
    YearlyOnDay = 5
  );
  PAppointments_AppointmentRecurrenceUnit = ^Appointments_AppointmentRecurrenceUnit;

  // Windows.ApplicationModel.Appointments.AppointmentSensitivity
  Appointments_AppointmentSensitivity = (
    &Public = 0,
    &Private = 1
  );
  PAppointments_AppointmentSensitivity = ^Appointments_AppointmentSensitivity;

  // Windows.ApplicationModel.Appointments.AppointmentStoreAccessType
  Appointments_AppointmentStoreAccessType = (
    AppCalendarsReadWrite = 0,
    AllCalendarsReadOnly = 1,
    AllCalendarsReadWrite = 2
  );
  PAppointments_AppointmentStoreAccessType = ^Appointments_AppointmentStoreAccessType;

  // Windows.ApplicationModel.Appointments.AppointmentStoreChangeType
  Appointments_AppointmentStoreChangeType = (
    AppointmentCreated = 0,
    AppointmentModified = 1,
    AppointmentDeleted = 2,
    ChangeTrackingLost = 3,
    CalendarCreated = 4,
    CalendarModified = 5,
    CalendarDeleted = 6
  );
  PAppointments_AppointmentStoreChangeType = ^Appointments_AppointmentStoreChangeType;

  // Windows.ApplicationModel.Appointments.AppointmentSummaryCardView
  Appointments_AppointmentSummaryCardView = (
    System = 0,
    App = 1
  );
  PAppointments_AppointmentSummaryCardView = ^Appointments_AppointmentSummaryCardView;

  // Windows.ApplicationModel.Appointments.AppointmentWeekOfMonth
  Appointments_AppointmentWeekOfMonth = (
    First = 0,
    Second = 1,
    Third = 2,
    Fourth = 3,
    Last = 4
  );
  PAppointments_AppointmentWeekOfMonth = ^Appointments_AppointmentWeekOfMonth;

  // Windows.ApplicationModel.Appointments.FindAppointmentCalendarsOptions
  Appointments_FindAppointmentCalendarsOptions = (
    None = 0,
    IncludeHidden = 1
  );
  PAppointments_FindAppointmentCalendarsOptions = ^Appointments_FindAppointmentCalendarsOptions;

  // Windows.ApplicationModel.Appointments.RecurrenceType
  Appointments_RecurrenceType = (
    Master = 0,
    Instance = 1,
    ExceptionInstance = 2
  );
  PAppointments_RecurrenceType = ^Appointments_RecurrenceType;

  // Windows.ApplicationModel.Calls.Background.PhoneCallBlockedReason
  Calls_Background_PhoneCallBlockedReason = (
    InCallBlockingList = 0,
    PrivateNumber = 1,
    UnknownNumber = 2
  );
  PCalls_Background_PhoneCallBlockedReason = ^Calls_Background_PhoneCallBlockedReason;

  // Windows.ApplicationModel.Calls.Background.PhoneIncomingCallDismissedReason
  Calls_Background_PhoneIncomingCallDismissedReason = (
    Unknown = 0,
    CallRejected = 1,
    TextReply = 2,
    ConnectionLost = 3
  );
  PCalls_Background_PhoneIncomingCallDismissedReason = ^Calls_Background_PhoneIncomingCallDismissedReason;

  // Windows.ApplicationModel.Calls.Background.PhoneLineChangeKind
  Calls_Background_PhoneLineChangeKind = (
    Added = 0,
    Removed = 1,
    PropertiesChanged = 2
  );
  PCalls_Background_PhoneLineChangeKind = ^Calls_Background_PhoneLineChangeKind;

  // Windows.ApplicationModel.Calls.Background.PhoneLineProperties
  Calls_Background_PhoneLineProperties = (
    None = 0,
    BrandingOptions = 1,
    CanDial = 2,
    CellularDetails = 4,
    DisplayColor = 8,
    DisplayName = 16,
    NetworkName = 32,
    NetworkState = 64,
    Transport = 128,
    Voicemail = 256
  );
  PCalls_Background_PhoneLineProperties = ^Calls_Background_PhoneLineProperties;

  // Windows.ApplicationModel.Calls.Background.PhoneTriggerType
  Calls_Background_PhoneTriggerType = (
    NewVoicemailMessage = 0,
    CallHistoryChanged = 1,
    LineChanged = 2,
    AirplaneModeDisabledForEmergencyCall = 3,
    CallOriginDataRequest = 4,
    CallBlocked = 5,
    IncomingCallDismissed = 6
  );
  PCalls_Background_PhoneTriggerType = ^Calls_Background_PhoneTriggerType;

  // Windows.ApplicationModel.Calls.CellularDtmfMode
  Calls_CellularDtmfMode = (
    Continuous = 0,
    Burst = 1
  );
  PCalls_CellularDtmfMode = ^Calls_CellularDtmfMode;

  // Windows.ApplicationModel.Calls.PhoneAudioRoutingEndpoint
  Calls_PhoneAudioRoutingEndpoint = (
    Default = 0,
    Bluetooth = 1,
    Speakerphone = 2
  );
  PCalls_PhoneAudioRoutingEndpoint = ^Calls_PhoneAudioRoutingEndpoint;

  // Windows.ApplicationModel.Calls.PhoneCallHistoryEntryMedia
  Calls_PhoneCallHistoryEntryMedia = (
    Audio = 0,
    Video = 1
  );
  PCalls_PhoneCallHistoryEntryMedia = ^Calls_PhoneCallHistoryEntryMedia;

  // Windows.ApplicationModel.Calls.PhoneCallHistoryEntryOtherAppReadAccess
  Calls_PhoneCallHistoryEntryOtherAppReadAccess = (
    Full = 0,
    SystemOnly = 1
  );
  PCalls_PhoneCallHistoryEntryOtherAppReadAccess = ^Calls_PhoneCallHistoryEntryOtherAppReadAccess;

  // Windows.ApplicationModel.Calls.PhoneCallHistoryEntryQueryDesiredMedia
  Calls_PhoneCallHistoryEntryQueryDesiredMedia = (
    None = 0,
    Audio = 1,
    Video = 2,
    All = -1
  );
  PCalls_PhoneCallHistoryEntryQueryDesiredMedia = ^Calls_PhoneCallHistoryEntryQueryDesiredMedia;

  // Windows.ApplicationModel.Calls.PhoneCallHistoryEntryRawAddressKind
  Calls_PhoneCallHistoryEntryRawAddressKind = (
    PhoneNumber = 0,
    Custom = 1
  );
  PCalls_PhoneCallHistoryEntryRawAddressKind = ^Calls_PhoneCallHistoryEntryRawAddressKind;

  // Windows.ApplicationModel.Calls.PhoneCallHistorySourceIdKind
  Calls_PhoneCallHistorySourceIdKind = (
    CellularPhoneLineId = 0,
    PackageFamilyName = 1
  );
  PCalls_PhoneCallHistorySourceIdKind = ^Calls_PhoneCallHistorySourceIdKind;

  // Windows.ApplicationModel.Calls.PhoneCallHistoryStoreAccessType
  Calls_PhoneCallHistoryStoreAccessType = (
    AppEntriesReadWrite = 0,
    AllEntriesLimitedReadWrite = 1,
    AllEntriesReadWrite = 2
  );
  PCalls_PhoneCallHistoryStoreAccessType = ^Calls_PhoneCallHistoryStoreAccessType;

  // Windows.ApplicationModel.Calls.PhoneCallMedia
  Calls_PhoneCallMedia = (
    Audio = 0,
    AudioAndVideo = 1,
    AudioAndRealTimeText = 2
  );
  PCalls_PhoneCallMedia = ^Calls_PhoneCallMedia;

  // Windows.ApplicationModel.Calls.PhoneLineNetworkOperatorDisplayTextLocation
  Calls_PhoneLineNetworkOperatorDisplayTextLocation = (
    Default = 0,
    Tile = 1,
    Dialer = 2,
    InCallUI = 3
  );
  PCalls_PhoneLineNetworkOperatorDisplayTextLocation = ^Calls_PhoneLineNetworkOperatorDisplayTextLocation;

  // Windows.ApplicationModel.Calls.PhoneLineTransport
  Calls_PhoneLineTransport = (
    Cellular = 0,
    VoipApp = 1,
    Bluetooth = 2
  );
  PCalls_PhoneLineTransport = ^Calls_PhoneLineTransport;

  // Windows.ApplicationModel.Calls.PhoneLineWatcherStatus
  Calls_PhoneLineWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopped = 3
  );
  PCalls_PhoneLineWatcherStatus = ^Calls_PhoneLineWatcherStatus;

  // Windows.ApplicationModel.Calls.PhoneNetworkState
  Calls_PhoneNetworkState = (
    Unknown = 0,
    NoSignal = 1,
    Deregistered = 2,
    Denied = 3,
    Searching = 4,
    Home = 5,
    RoamingInternational = 6,
    RoamingDomestic = 7
  );
  PCalls_PhoneNetworkState = ^Calls_PhoneNetworkState;

  // Windows.ApplicationModel.Calls.PhoneSimState
  Calls_PhoneSimState = (
    Unknown = 0,
    PinNotRequired = 1,
    PinUnlocked = 2,
    PinLocked = 3,
    PukLocked = 4,
    NotInserted = 5,
    Invalid = 6,
    Disabled = 7
  );
  PCalls_PhoneSimState = ^Calls_PhoneSimState;

  // Windows.ApplicationModel.Calls.PhoneVoicemailType
  Calls_PhoneVoicemailType = (
    None = 0,
    Traditional = 1,
    Visual = 2
  );
  PCalls_PhoneVoicemailType = ^Calls_PhoneVoicemailType;

  // Windows.ApplicationModel.Calls.VoipPhoneCallMedia
  Calls_VoipPhoneCallMedia = (
    None = 0,
    Audio = 1,
    Video = 2
  );
  PCalls_VoipPhoneCallMedia = ^Calls_VoipPhoneCallMedia;

  // Windows.ApplicationModel.Calls.VoipPhoneCallRejectReason
  Calls_VoipPhoneCallRejectReason = (
    UserIgnored = 0,
    TimedOut = 1,
    OtherIncomingCall = 2,
    EmergencyCallExists = 3,
    InvalidCallState = 4
  );
  PCalls_VoipPhoneCallRejectReason = ^Calls_VoipPhoneCallRejectReason;

  // Windows.ApplicationModel.Calls.VoipPhoneCallResourceReservationStatus
  Calls_VoipPhoneCallResourceReservationStatus = (
    Success = 0,
    ResourcesNotAvailable = 1
  );
  PCalls_VoipPhoneCallResourceReservationStatus = ^Calls_VoipPhoneCallResourceReservationStatus;

  // Windows.ApplicationModel.Calls.VoipPhoneCallState
  Calls_VoipPhoneCallState = (
    Ended = 0,
    Held = 1,
    Active = 2,
    Incoming = 3,
    Outgoing = 4
  );
  PCalls_VoipPhoneCallState = ^Calls_VoipPhoneCallState;

  // Windows.ApplicationModel.Chat.ChatConversationThreadingKind
  Chat_ChatConversationThreadingKind = (
    Participants = 0,
    ContactId = 1,
    ConversationId = 2,
    Custom = 3
  );
  PChat_ChatConversationThreadingKind = ^Chat_ChatConversationThreadingKind;

  // Windows.ApplicationModel.Chat.ChatItemKind
  Chat_ChatItemKind = (
    &Message = 0,
    Conversation = 1
  );
  PChat_ChatItemKind = ^Chat_ChatItemKind;

  // Windows.ApplicationModel.Chat.ChatMessageChangeType
  Chat_ChatMessageChangeType = (
    MessageCreated = 0,
    MessageModified = 1,
    MessageDeleted = 2,
    ChangeTrackingLost = 3
  );
  PChat_ChatMessageChangeType = ^Chat_ChatMessageChangeType;

  // Windows.ApplicationModel.Chat.ChatMessageKind
  Chat_ChatMessageKind = (
    Standard = 0,
    FileTransferRequest = 1,
    TransportCustom = 2,
    JoinedConversation = 3,
    LeftConversation = 4,
    OtherParticipantJoinedConversation = 5,
    OtherParticipantLeftConversation = 6
  );
  PChat_ChatMessageKind = ^Chat_ChatMessageKind;

  // Windows.ApplicationModel.Chat.ChatMessageOperatorKind
  Chat_ChatMessageOperatorKind = (
    Unspecified = 0,
    Sms = 1,
    Mms = 2,
    Rcs = 3
  );
  PChat_ChatMessageOperatorKind = ^Chat_ChatMessageOperatorKind;

  // Windows.ApplicationModel.Chat.ChatMessageStatus
  Chat_ChatMessageStatus = (
    Draft = 0,
    Sending = 1,
    Sent = 2,
    SendRetryNeeded = 3,
    SendFailed = 4,
    Received = 5,
    ReceiveDownloadNeeded = 6,
    ReceiveDownloadFailed = 7,
    ReceiveDownloading = 8,
    Deleted = 9,
    Declined = 10,
    Cancelled = 11,
    Recalled = 12,
    ReceiveRetryNeeded = 13
  );
  PChat_ChatMessageStatus = ^Chat_ChatMessageStatus;

  // Windows.ApplicationModel.Chat.ChatMessageTransportKind
  Chat_ChatMessageTransportKind = (
    Text = 0,
    Untriaged = 1,
    Blocked = 2,
    Custom = 3
  );
  PChat_ChatMessageTransportKind = ^Chat_ChatMessageTransportKind;

  // Windows.ApplicationModel.Chat.ChatMessageValidationStatus
  Chat_ChatMessageValidationStatus = (
    Valid = 0,
    NoRecipients = 1,
    InvalidData = 2,
    MessageTooLarge = 3,
    TooManyRecipients = 4,
    TransportInactive = 5,
    TransportNotFound = 6,
    TooManyAttachments = 7,
    InvalidRecipients = 8,
    InvalidBody = 9,
    InvalidOther = 10,
    ValidWithLargeMessage = 11,
    VoiceRoamingRestriction = 12,
    DataRoamingRestriction = 13
  );
  PChat_ChatMessageValidationStatus = ^Chat_ChatMessageValidationStatus;

  // Windows.ApplicationModel.Chat.ChatRestoreHistorySpan
  Chat_ChatRestoreHistorySpan = (
    LastMonth = 0,
    LastYear = 1,
    AnyTime = 2
  );
  PChat_ChatRestoreHistorySpan = ^Chat_ChatRestoreHistorySpan;

  // Windows.ApplicationModel.Chat.ChatStoreChangedEventKind
  Chat_ChatStoreChangedEventKind = (
    NotificationsMissed = 0,
    StoreModified = 1,
    MessageCreated = 2,
    MessageModified = 3,
    MessageDeleted = 4,
    ConversationModified = 5,
    ConversationDeleted = 6,
    ConversationTransportDeleted = 7
  );
  PChat_ChatStoreChangedEventKind = ^Chat_ChatStoreChangedEventKind;

  // Windows.ApplicationModel.Chat.ChatTransportErrorCodeCategory
  Chat_ChatTransportErrorCodeCategory = (
    None = 0,
    Http = 1,
    Network = 2,
    MmsServer = 3
  );
  PChat_ChatTransportErrorCodeCategory = ^Chat_ChatTransportErrorCodeCategory;

  // Windows.ApplicationModel.Chat.ChatTransportInterpretedErrorCode
  Chat_ChatTransportInterpretedErrorCode = (
    None = 0,
    Unknown = 1,
    InvalidRecipientAddress = 2,
    NetworkConnectivity = 3,
    ServiceDenied = 4,
    Timeout = 5
  );
  PChat_ChatTransportInterpretedErrorCode = ^Chat_ChatTransportInterpretedErrorCode;

  // Windows.ApplicationModel.Chat.RcsServiceKind
  Chat_RcsServiceKind = (
    Chat = 0,
    GroupChat = 1,
    FileTransfer = 2,
    Capability = 3
  );
  PChat_RcsServiceKind = ^Chat_RcsServiceKind;

  // Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectionTrainingDataFormat
  ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = (
    Voice8kHz8BitMono = 0,
    Voice8kHz16BitMono = 1,
    Voice16kHz8BitMono = 2,
    Voice16kHz16BitMono = 3,
    VoiceOEMDefined = 4,
    Audio44kHz8BitMono = 5,
    Audio44kHz16BitMono = 6,
    Audio48kHz8BitMono = 7,
    Audio48kHz16BitMono = 8,
    AudioOEMDefined = 9,
    OtherOEMDefined = 10
  );
  PConversationalAgent_ActivationSignalDetectionTrainingDataFormat = ^ConversationalAgent_ActivationSignalDetectionTrainingDataFormat;

  // Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorKind
  ConversationalAgent_ActivationSignalDetectorKind = (
    AudioPattern = 0,
    AudioImpulse = 1,
    HardwareEvent = 2
  );
  PConversationalAgent_ActivationSignalDetectorKind = ^ConversationalAgent_ActivationSignalDetectorKind;

  // Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorPowerState
  ConversationalAgent_ActivationSignalDetectorPowerState = (
    HighPower = 0,
    ConnectedLowPower = 1,
    DisconnectedLowPower = 2
  );
  PConversationalAgent_ActivationSignalDetectorPowerState = ^ConversationalAgent_ActivationSignalDetectorPowerState;

  // Windows.ApplicationModel.ConversationalAgent.ConversationalAgentSessionUpdateResponse
  ConversationalAgent_ConversationalAgentSessionUpdateResponse = (
    Success = 0,
    Failed = 1
  );
  PConversationalAgent_ConversationalAgentSessionUpdateResponse = ^ConversationalAgent_ConversationalAgentSessionUpdateResponse;

  // Windows.ApplicationModel.ConversationalAgent.ConversationalAgentState
  ConversationalAgent_ConversationalAgentState = (
    Inactive = 0,
    Detecting = 1,
    Listening = 2,
    Working = 3,
    Speaking = 4,
    ListeningAndSpeaking = 5
  );
  PConversationalAgent_ConversationalAgentState = ^ConversationalAgent_ConversationalAgentState;

  // Windows.ApplicationModel.ConversationalAgent.ConversationalAgentSystemStateChangeType
  ConversationalAgent_ConversationalAgentSystemStateChangeType = (
    UserAuthentication = 0,
    ScreenAvailability = 1,
    IndicatorLightAvailability = 2,
    VoiceActivationAvailability = 3
  );
  PConversationalAgent_ConversationalAgentSystemStateChangeType = ^ConversationalAgent_ConversationalAgentSystemStateChangeType;

  // Windows.ApplicationModel.ConversationalAgent.DetectionConfigurationAvailabilityChangeKind
  ConversationalAgent_DetectionConfigurationAvailabilityChangeKind = (
    SystemResourceAccess = 0,
    Permission = 1,
    LockScreenPermission = 2
  );
  PConversationalAgent_DetectionConfigurationAvailabilityChangeKind = ^ConversationalAgent_DetectionConfigurationAvailabilityChangeKind;

  // Windows.ApplicationModel.ConversationalAgent.DetectionConfigurationTrainingStatus
  ConversationalAgent_DetectionConfigurationTrainingStatus = (
    Success = 0,
    FormatNotSupported = 1,
    VoiceTooQuiet = 2,
    VoiceTooLoud = 3,
    VoiceTooFast = 4,
    VoiceTooSlow = 5,
    VoiceQualityProblem = 6,
    TrainingSystemInternalError = 7
  );
  PConversationalAgent_DetectionConfigurationTrainingStatus = ^ConversationalAgent_DetectionConfigurationTrainingStatus;

  // Windows.ApplicationModel.Email.EmailAttachmentDownloadState
  Email_EmailAttachmentDownloadState = (
    NotDownloaded = 0,
    Downloading = 1,
    Downloaded = 2,
    Failed = 3
  );
  PEmail_EmailAttachmentDownloadState = ^Email_EmailAttachmentDownloadState;

  // Windows.ApplicationModel.Email.EmailBatchStatus
  Email_EmailBatchStatus = (
    Success = 0,
    ServerSearchSyncManagerError = 1,
    ServerSearchUnknownError = 2
  );
  PEmail_EmailBatchStatus = ^Email_EmailBatchStatus;

  // Windows.ApplicationModel.Email.EmailCertificateValidationStatus
  Email_EmailCertificateValidationStatus = (
    Success = 0,
    NoMatch = 1,
    InvalidUsage = 2,
    InvalidCertificate = 3,
    Revoked = 4,
    ChainRevoked = 5,
    RevocationServerFailure = 6,
    Expired = 7,
    Untrusted = 8,
    ServerError = 9,
    UnknownFailure = 10
  );
  PEmail_EmailCertificateValidationStatus = ^Email_EmailCertificateValidationStatus;

  // Windows.ApplicationModel.Email.EmailFlagState
  Email_EmailFlagState = (
    Unflagged = 0,
    Flagged = 1,
    Completed = 2,
    Cleared = 3
  );
  PEmail_EmailFlagState = ^Email_EmailFlagState;

  // Windows.ApplicationModel.Email.EmailImportance
  Email_EmailImportance = (
    Normal = 0,
    High = 1,
    Low = 2
  );
  PEmail_EmailImportance = ^Email_EmailImportance;

  // Windows.ApplicationModel.Email.EmailMailboxActionKind
  Email_EmailMailboxActionKind = (
    MarkMessageAsSeen = 0,
    MarkMessageRead = 1,
    ChangeMessageFlagState = 2,
    MoveMessage = 3,
    SaveDraft = 4,
    SendMessage = 5,
    CreateResponseReplyMessage = 6,
    CreateResponseReplyAllMessage = 7,
    CreateResponseForwardMessage = 8,
    MoveFolder = 9,
    MarkFolderForSyncEnabled = 10
  );
  PEmail_EmailMailboxActionKind = ^Email_EmailMailboxActionKind;

  // Windows.ApplicationModel.Email.EmailMailboxAllowedSmimeEncryptionAlgorithmNegotiation
  Email_EmailMailboxAllowedSmimeEncryptionAlgorithmNegotiation = (
    None = 0,
    StrongAlgorithm = 1,
    AnyAlgorithm = 2
  );
  PEmail_EmailMailboxAllowedSmimeEncryptionAlgorithmNegotiation = ^Email_EmailMailboxAllowedSmimeEncryptionAlgorithmNegotiation;

  // Windows.ApplicationModel.Email.EmailMailboxAutoReplyMessageResponseKind
  Email_EmailMailboxAutoReplyMessageResponseKind = (
    Html = 0,
    PlainText = 1
  );
  PEmail_EmailMailboxAutoReplyMessageResponseKind = ^Email_EmailMailboxAutoReplyMessageResponseKind;

  // Windows.ApplicationModel.Email.EmailMailboxChangeType
  Email_EmailMailboxChangeType = (
    MessageCreated = 0,
    MessageModified = 1,
    MessageDeleted = 2,
    FolderCreated = 3,
    FolderModified = 4,
    FolderDeleted = 5,
    ChangeTrackingLost = 6
  );
  PEmail_EmailMailboxChangeType = ^Email_EmailMailboxChangeType;

  // Windows.ApplicationModel.Email.EmailMailboxCreateFolderStatus
  Email_EmailMailboxCreateFolderStatus = (
    Success = 0,
    NetworkError = 1,
    PermissionsError = 2,
    ServerError = 3,
    UnknownFailure = 4,
    NameCollision = 5,
    ServerRejected = 6
  );
  PEmail_EmailMailboxCreateFolderStatus = ^Email_EmailMailboxCreateFolderStatus;

  // Windows.ApplicationModel.Email.EmailMailboxDeleteFolderStatus
  Email_EmailMailboxDeleteFolderStatus = (
    Success = 0,
    NetworkError = 1,
    PermissionsError = 2,
    ServerError = 3,
    UnknownFailure = 4,
    CouldNotDeleteEverything = 5
  );
  PEmail_EmailMailboxDeleteFolderStatus = ^Email_EmailMailboxDeleteFolderStatus;

  // Windows.ApplicationModel.Email.EmailMailboxEmptyFolderStatus
  Email_EmailMailboxEmptyFolderStatus = (
    Success = 0,
    NetworkError = 1,
    PermissionsError = 2,
    ServerError = 3,
    UnknownFailure = 4,
    CouldNotDeleteEverything = 5
  );
  PEmail_EmailMailboxEmptyFolderStatus = ^Email_EmailMailboxEmptyFolderStatus;

  // Windows.ApplicationModel.Email.EmailMailboxOtherAppReadAccess
  Email_EmailMailboxOtherAppReadAccess = (
    SystemOnly = 0,
    Full = 1,
    None = 2
  );
  PEmail_EmailMailboxOtherAppReadAccess = ^Email_EmailMailboxOtherAppReadAccess;

  // Windows.ApplicationModel.Email.EmailMailboxOtherAppWriteAccess
  Email_EmailMailboxOtherAppWriteAccess = (
    None = 0,
    Limited = 1
  );
  PEmail_EmailMailboxOtherAppWriteAccess = ^Email_EmailMailboxOtherAppWriteAccess;

  // Windows.ApplicationModel.Email.EmailMailboxSmimeEncryptionAlgorithm
  Email_EmailMailboxSmimeEncryptionAlgorithm = (
    Any = 0,
    TripleDes = 1,
    Des = 2,
    RC2128Bit = 3,
    RC264Bit = 4,
    RC240Bit = 5
  );
  PEmail_EmailMailboxSmimeEncryptionAlgorithm = ^Email_EmailMailboxSmimeEncryptionAlgorithm;

  // Windows.ApplicationModel.Email.EmailMailboxSmimeSigningAlgorithm
  Email_EmailMailboxSmimeSigningAlgorithm = (
    Any = 0,
    Sha1 = 1,
    MD5 = 2
  );
  PEmail_EmailMailboxSmimeSigningAlgorithm = ^Email_EmailMailboxSmimeSigningAlgorithm;

  // Windows.ApplicationModel.Email.EmailMailboxSyncStatus
  Email_EmailMailboxSyncStatus = (
    Idle = 0,
    Syncing = 1,
    UpToDate = 2,
    AuthenticationError = 3,
    PolicyError = 4,
    UnknownError = 5,
    ManualAccountRemovalRequired = 6
  );
  PEmail_EmailMailboxSyncStatus = ^Email_EmailMailboxSyncStatus;

  // Windows.ApplicationModel.Email.EmailMeetingResponseType
  Email_EmailMeetingResponseType = (
    Accept = 0,
    Decline = 1,
    Tentative = 2
  );
  PEmail_EmailMeetingResponseType = ^Email_EmailMeetingResponseType;

  // Windows.ApplicationModel.Email.EmailMessageBodyKind
  Email_EmailMessageBodyKind = (
    Html = 0,
    PlainText = 1
  );
  PEmail_EmailMessageBodyKind = ^Email_EmailMessageBodyKind;

  // Windows.ApplicationModel.Email.EmailMessageDownloadState
  Email_EmailMessageDownloadState = (
    PartiallyDownloaded = 0,
    Downloading = 1,
    Downloaded = 2,
    Failed = 3
  );
  PEmail_EmailMessageDownloadState = ^Email_EmailMessageDownloadState;

  // Windows.ApplicationModel.Email.EmailMessageResponseKind
  Email_EmailMessageResponseKind = (
    None = 0,
    Reply = 1,
    ReplyAll = 2,
    Forward = 3
  );
  PEmail_EmailMessageResponseKind = ^Email_EmailMessageResponseKind;

  // Windows.ApplicationModel.Email.EmailMessageSmimeKind
  Email_EmailMessageSmimeKind = (
    None = 0,
    ClearSigned = 1,
    OpaqueSigned = 2,
    Encrypted = 3
  );
  PEmail_EmailMessageSmimeKind = ^Email_EmailMessageSmimeKind;

  // Windows.ApplicationModel.Email.EmailQueryKind
  Email_EmailQueryKind = (
    All = 0,
    Important = 1,
    Flagged = 2,
    Unread = 3,
    Read = 4,
    Unseen = 5
  );
  PEmail_EmailQueryKind = ^Email_EmailQueryKind;

  // Windows.ApplicationModel.Email.EmailQuerySearchFields
  Email_EmailQuerySearchFields = (
    None = 0,
    Subject = 1,
    Sender = 2,
    Preview = 4,
    Recipients = 8,
    All = -1
  );
  PEmail_EmailQuerySearchFields = ^Email_EmailQuerySearchFields;

  // Windows.ApplicationModel.Email.EmailQuerySearchScope
  Email_EmailQuerySearchScope = (
    Local = 0,
    Server = 1
  );
  PEmail_EmailQuerySearchScope = ^Email_EmailQuerySearchScope;

  // Windows.ApplicationModel.Email.EmailQuerySortDirection
  Email_EmailQuerySortDirection = (
    Descending = 0,
    Ascending = 1
  );
  PEmail_EmailQuerySortDirection = ^Email_EmailQuerySortDirection;

  // Windows.ApplicationModel.Email.EmailQuerySortProperty
  Email_EmailQuerySortProperty = (
    Date = 0
  );
  PEmail_EmailQuerySortProperty = ^Email_EmailQuerySortProperty;

  // Windows.ApplicationModel.Email.EmailRecipientResolutionStatus
  Email_EmailRecipientResolutionStatus = (
    Success = 0,
    RecipientNotFound = 1,
    AmbiguousRecipient = 2,
    NoCertificate = 3,
    CertificateRequestLimitReached = 4,
    CannotResolveDistributionList = 5,
    ServerError = 6,
    UnknownFailure = 7
  );
  PEmail_EmailRecipientResolutionStatus = ^Email_EmailRecipientResolutionStatus;

  // Windows.ApplicationModel.Email.EmailSpecialFolderKind
  Email_EmailSpecialFolderKind = (
    None = 0,
    Root = 1,
    Inbox = 2,
    Outbox = 3,
    Drafts = 4,
    DeletedItems = 5,
    Sent = 6
  );
  PEmail_EmailSpecialFolderKind = ^Email_EmailSpecialFolderKind;

  // Windows.ApplicationModel.Email.EmailStoreAccessType
  Email_EmailStoreAccessType = (
    AppMailboxesReadWrite = 0,
    AllMailboxesLimitedReadWrite = 1
  );
  PEmail_EmailStoreAccessType = ^Email_EmailStoreAccessType;

  // Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionReason
  ExtendedExecution_ExtendedExecutionReason = (
    Unspecified = 0,
    LocationTracking = 1,
    SavingData = 2
  );
  PExtendedExecution_ExtendedExecutionReason = ^ExtendedExecution_ExtendedExecutionReason;

  // Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionResult
  ExtendedExecution_ExtendedExecutionResult = (
    Allowed = 0,
    Denied = 1
  );
  PExtendedExecution_ExtendedExecutionResult = ^ExtendedExecution_ExtendedExecutionResult;

  // Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionRevokedReason
  ExtendedExecution_ExtendedExecutionRevokedReason = (
    Resumed = 0,
    SystemPolicy = 1
  );
  PExtendedExecution_ExtendedExecutionRevokedReason = ^ExtendedExecution_ExtendedExecutionRevokedReason;

  // Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundReason
  ExtendedExecution_Foreground_ExtendedExecutionForegroundReason = (
    Unspecified = 0,
    SavingData = 1,
    BackgroundAudio = 2,
    Unconstrained = 3
  );
  PExtendedExecution_Foreground_ExtendedExecutionForegroundReason = ^ExtendedExecution_Foreground_ExtendedExecutionForegroundReason;

  // Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundResult
  ExtendedExecution_Foreground_ExtendedExecutionForegroundResult = (
    Allowed = 0,
    Denied = 1
  );
  PExtendedExecution_Foreground_ExtendedExecutionForegroundResult = ^ExtendedExecution_Foreground_ExtendedExecutionForegroundResult;

  // Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundRevokedReason
  ExtendedExecution_Foreground_ExtendedExecutionForegroundRevokedReason = (
    Resumed = 0,
    SystemPolicy = 1
  );
  PExtendedExecution_Foreground_ExtendedExecutionForegroundRevokedReason = ^ExtendedExecution_Foreground_ExtendedExecutionForegroundRevokedReason;

  // Windows.ApplicationModel.LimitedAccessFeatureStatus
  LimitedAccessFeatureStatus = (
    Unavailable = 0,
    Available = 1,
    AvailableWithoutToken = 2,
    Unknown = 3
  );
  PLimitedAccessFeatureStatus = ^LimitedAccessFeatureStatus;

  // Windows.ApplicationModel.PackageContentGroupState
  PackageContentGroupState = (
    NotStaged = 0,
    Queued = 1,
    Staging = 2,
    Staged = 3
  );
  PPackageContentGroupState = ^PackageContentGroupState;

  // Windows.ApplicationModel.PackageSignatureKind
  PackageSignatureKind = (
    None = 0,
    Developer = 1,
    Enterprise = 2,
    Store = 3,
    System = 4
  );
  PPackageSignatureKind = ^PackageSignatureKind;

  // Windows.ApplicationModel.PackageUpdateAvailability
  PackageUpdateAvailability = (
    Unknown = 0,
    NoUpdates = 1,
    Available = 2,
    Required = 3,
    Error = 4
  );
  PPackageUpdateAvailability = ^PackageUpdateAvailability;

  // Windows.ApplicationModel.Payments.PaymentCanMakePaymentResultStatus
  Payments_PaymentCanMakePaymentResultStatus = (
    Unknown = 0,
    Yes = 1,
    No = 2,
    NotAllowed = 3,
    UserNotSignedIn = 4,
    SpecifiedPaymentMethodIdsNotSupported = 5,
    NoQualifyingCardOnFile = 6
  );
  PPayments_PaymentCanMakePaymentResultStatus = ^Payments_PaymentCanMakePaymentResultStatus;

  // Windows.ApplicationModel.Payments.PaymentOptionPresence
  Payments_PaymentOptionPresence = (
    None = 0,
    Optional = 1,
    Required = 2
  );
  PPayments_PaymentOptionPresence = ^Payments_PaymentOptionPresence;

  // Windows.ApplicationModel.Payments.PaymentRequestChangeKind
  Payments_PaymentRequestChangeKind = (
    ShippingOption = 0,
    ShippingAddress = 1
  );
  PPayments_PaymentRequestChangeKind = ^Payments_PaymentRequestChangeKind;

  // Windows.ApplicationModel.Payments.PaymentRequestCompletionStatus
  Payments_PaymentRequestCompletionStatus = (
    Succeeded = 0,
    Failed = 1,
    Unknown = 2
  );
  PPayments_PaymentRequestCompletionStatus = ^Payments_PaymentRequestCompletionStatus;

  // Windows.ApplicationModel.Payments.PaymentRequestStatus
  Payments_PaymentRequestStatus = (
    Succeeded = 0,
    Failed = 1,
    Canceled = 2
  );
  PPayments_PaymentRequestStatus = ^Payments_PaymentRequestStatus;

  // Windows.ApplicationModel.Payments.PaymentShippingType
  Payments_PaymentShippingType = (
    Shipping = 0,
    Delivery = 1,
    Pickup = 2
  );
  PPayments_PaymentShippingType = ^Payments_PaymentShippingType;

  // Windows.ApplicationModel.Resources.Core.ResourceCandidateKind
  Resources_Core_ResourceCandidateKind = (
    &String = 0,
    &File = 1,
    EmbeddedData = 2
  );
  PResources_Core_ResourceCandidateKind = ^Resources_Core_ResourceCandidateKind;

  // Windows.ApplicationModel.Resources.Core.ResourceQualifierPersistence
  Resources_Core_ResourceQualifierPersistence = (
    None = 0,
    LocalMachine = 1
  );
  PResources_Core_ResourceQualifierPersistence = ^Resources_Core_ResourceQualifierPersistence;

  // Windows.ApplicationModel.Resources.Management.IndexedResourceType
  Resources_Management_IndexedResourceType = (
    &String = 0,
    Path = 1,
    EmbeddedData = 2
  );
  PResources_Management_IndexedResourceType = ^Resources_Management_IndexedResourceType;

  // Windows.ApplicationModel.Search.Core.SearchSuggestionKind
  Search_Core_SearchSuggestionKind = (
    Query = 0,
    Result = 1,
    Separator = 2
  );
  PSearch_Core_SearchSuggestionKind = ^Search_Core_SearchSuggestionKind;

  // Windows.ApplicationModel.SocialInfo.SocialFeedItemStyle
  SocialInfo_SocialFeedItemStyle = (
    Default = 0,
    Photo = 1
  );
  PSocialInfo_SocialFeedItemStyle = ^SocialInfo_SocialFeedItemStyle;

  // Windows.ApplicationModel.SocialInfo.SocialFeedKind
  SocialInfo_SocialFeedKind = (
    HomeFeed = 0,
    ContactFeed = 1,
    Dashboard = 2
  );
  PSocialInfo_SocialFeedKind = ^SocialInfo_SocialFeedKind;

  // Windows.ApplicationModel.SocialInfo.SocialFeedUpdateMode
  SocialInfo_SocialFeedUpdateMode = (
    Append = 0,
    Replace = 1
  );
  PSocialInfo_SocialFeedUpdateMode = ^SocialInfo_SocialFeedUpdateMode;

  // Windows.ApplicationModel.SocialInfo.SocialItemBadgeStyle
  SocialInfo_SocialItemBadgeStyle = (
    Hidden = 0,
    Visible = 1,
    VisibleWithCount = 2
  );
  PSocialInfo_SocialItemBadgeStyle = ^SocialInfo_SocialItemBadgeStyle;

  // Windows.ApplicationModel.StartupTaskState
  StartupTaskState = (
    Disabled = 0,
    DisabledByUser = 1,
    Enabled = 2,
    DisabledByPolicy = 3,
    EnabledByPolicy = 4
  );
  PStartupTaskState = ^StartupTaskState;

  // Windows.ApplicationModel.Store.FulfillmentResult
  Store_FulfillmentResult = (
    Succeeded = 0,
    NothingToFulfill = 1,
    PurchasePending = 2,
    PurchaseReverted = 3,
    ServerError = 4
  );
  PStore_FulfillmentResult = ^Store_FulfillmentResult;

  // Windows.ApplicationModel.Store.LicenseManagement.LicenseRefreshOption
  Store_LicenseManagement_LicenseRefreshOption = (
    RunningLicenses = 0,
    AllLicenses = 1
  );
  PStore_LicenseManagement_LicenseRefreshOption = ^Store_LicenseManagement_LicenseRefreshOption;

  // Windows.ApplicationModel.Store.Preview.DeliveryOptimizationDownloadMode
  Store_Preview_DeliveryOptimizationDownloadMode = (
    Simple = 0,
    HttpOnly = 1,
    Lan = 2,
    Group = 3,
    Internet = 4,
    Bypass = 5
  );
  PStore_Preview_DeliveryOptimizationDownloadMode = ^Store_Preview_DeliveryOptimizationDownloadMode;

  // Windows.ApplicationModel.Store.Preview.DeliveryOptimizationDownloadModeSource
  Store_Preview_DeliveryOptimizationDownloadModeSource = (
    Default = 0,
    Policy = 1
  );
  PStore_Preview_DeliveryOptimizationDownloadModeSource = ^Store_Preview_DeliveryOptimizationDownloadModeSource;

  // Windows.ApplicationModel.Store.Preview.InstallControl.AppInstallState
  Store_Preview_InstallControl_AppInstallState = (
    Pending = 0,
    Starting = 1,
    AcquiringLicense = 2,
    Downloading = 3,
    RestoringData = 4,
    Installing = 5,
    Completed = 6,
    Canceled = 7,
    Paused = 8,
    Error = 9,
    PausedLowBattery = 10,
    PausedWiFiRecommended = 11,
    PausedWiFiRequired = 12,
    ReadyToDownload = 13
  );
  PStore_Preview_InstallControl_AppInstallState = ^Store_Preview_InstallControl_AppInstallState;

  // Windows.ApplicationModel.Store.Preview.InstallControl.AppInstallType
  Store_Preview_InstallControl_AppInstallType = (
    Install = 0,
    Update = 1,
    Repair = 2
  );
  PStore_Preview_InstallControl_AppInstallType = ^Store_Preview_InstallControl_AppInstallType;

  // Windows.ApplicationModel.Store.Preview.InstallControl.AppInstallationToastNotificationMode
  Store_Preview_InstallControl_AppInstallationToastNotificationMode = (
    Default = 0,
    Toast = 1,
    ToastWithoutPopup = 2,
    NoToast = 3
  );
  PStore_Preview_InstallControl_AppInstallationToastNotificationMode = ^Store_Preview_InstallControl_AppInstallationToastNotificationMode;

  // Windows.ApplicationModel.Store.Preview.InstallControl.AutoUpdateSetting
  Store_Preview_InstallControl_AutoUpdateSetting = (
    Disabled = 0,
    Enabled = 1,
    DisabledByPolicy = 2,
    EnabledByPolicy = 3
  );
  PStore_Preview_InstallControl_AutoUpdateSetting = ^Store_Preview_InstallControl_AutoUpdateSetting;

  // Windows.ApplicationModel.Store.Preview.InstallControl.GetEntitlementStatus
  Store_Preview_InstallControl_GetEntitlementStatus = (
    Succeeded = 0,
    NoStoreAccount = 1,
    NetworkError = 2,
    ServerError = 3
  );
  PStore_Preview_InstallControl_GetEntitlementStatus = ^Store_Preview_InstallControl_GetEntitlementStatus;

  // Windows.ApplicationModel.Store.Preview.StoreLogOptions
  Store_Preview_StoreLogOptions = (
    None = 0,
    TryElevate = 1
  );
  PStore_Preview_StoreLogOptions = ^Store_Preview_StoreLogOptions;

  // Windows.ApplicationModel.Store.Preview.StorePreviewProductPurchaseStatus
  Store_Preview_StorePreviewProductPurchaseStatus = (
    Succeeded = 0,
    AlreadyPurchased = 1,
    NotFulfilled = 2,
    NotPurchased = 3
  );
  PStore_Preview_StorePreviewProductPurchaseStatus = ^Store_Preview_StorePreviewProductPurchaseStatus;

  // Windows.ApplicationModel.Store.Preview.StoreSystemFeature
  Store_Preview_StoreSystemFeature = (
    ArchitectureX86 = 0,
    ArchitectureX64 = 1,
    ArchitectureArm = 2,
    DirectX9 = 3,
    DirectX10 = 4,
    DirectX11 = 5,
    D3D12HardwareFL11 = 6,
    D3D12HardwareFL12 = 7,
    Memory300MB = 8,
    Memory750MB = 9,
    Memory1GB = 10,
    Memory2GB = 11,
    CameraFront = 12,
    CameraRear = 13,
    Gyroscope = 14,
    Hover = 15,
    Magnetometer = 16,
    Nfc = 17,
    Resolution720P = 18,
    ResolutionWvga = 19,
    ResolutionWvgaOr720P = 20,
    ResolutionWxga = 21,
    ResolutionWvgaOrWxga = 22,
    ResolutionWxgaOr720P = 23,
    Memory4GB = 24,
    Memory6GB = 25,
    Memory8GB = 26,
    Memory12GB = 27,
    Memory16GB = 28,
    Memory20GB = 29,
    VideoMemory2GB = 30,
    VideoMemory4GB = 31,
    VideoMemory6GB = 32,
    VideoMemory1GB = 33,
    ArchitectureArm64 = 34
  );
  PStore_Preview_StoreSystemFeature = ^Store_Preview_StoreSystemFeature;

  // Windows.ApplicationModel.Store.ProductPurchaseStatus
  Store_ProductPurchaseStatus = (
    Succeeded = 0,
    AlreadyPurchased = 1,
    NotFulfilled = 2,
    NotPurchased = 3
  );
  PStore_ProductPurchaseStatus = ^Store_ProductPurchaseStatus;

  // Windows.ApplicationModel.Store.ProductType
  Store_ProductType = (
    Unknown = 0,
    Durable = 1,
    Consumable = 2
  );
  PStore_ProductType = ^Store_ProductType;

  // Windows.ApplicationModel.UserActivities.UserActivityState
  UserActivities_UserActivityState = (
    New = 0,
    &Published = 1
  );
  PUserActivities_UserActivityState = ^UserActivities_UserActivityState;

  // Windows.ApplicationModel.UserDataAccounts.Provider.UserDataAccountProviderOperationKind
  UserDataAccounts_Provider_UserDataAccountProviderOperationKind = (
    AddAccount = 0,
    Settings = 1,
    ResolveErrors = 2
  );
  PUserDataAccounts_Provider_UserDataAccountProviderOperationKind = ^UserDataAccounts_Provider_UserDataAccountProviderOperationKind;

  // Windows.ApplicationModel.UserDataAccounts.Provider.UserDataAccountProviderPartnerAccountKind
  UserDataAccounts_Provider_UserDataAccountProviderPartnerAccountKind = (
    Exchange = 0,
    PopOrImap = 1
  );
  PUserDataAccounts_Provider_UserDataAccountProviderPartnerAccountKind = ^UserDataAccounts_Provider_UserDataAccountProviderPartnerAccountKind;

  // Windows.ApplicationModel.UserDataAccounts.SystemAccess.DeviceAccountAuthenticationType
  UserDataAccounts_SystemAccess_DeviceAccountAuthenticationType = (
    Basic = 0,
    OAuth = 1,
    SingleSignOn = 2
  );
  PUserDataAccounts_SystemAccess_DeviceAccountAuthenticationType = ^UserDataAccounts_SystemAccess_DeviceAccountAuthenticationType;

  // Windows.ApplicationModel.UserDataAccounts.SystemAccess.DeviceAccountIconId
  UserDataAccounts_SystemAccess_DeviceAccountIconId = (
    Exchange = 0,
    Msa = 1,
    Outlook = 2,
    Generic = 3
  );
  PUserDataAccounts_SystemAccess_DeviceAccountIconId = ^UserDataAccounts_SystemAccess_DeviceAccountIconId;

  // Windows.ApplicationModel.UserDataAccounts.SystemAccess.DeviceAccountMailAgeFilter
  UserDataAccounts_SystemAccess_DeviceAccountMailAgeFilter = (
    All = 0,
    Last1Day = 1,
    Last3Days = 2,
    Last7Days = 3,
    Last14Days = 4,
    Last30Days = 5,
    Last90Days = 6
  );
  PUserDataAccounts_SystemAccess_DeviceAccountMailAgeFilter = ^UserDataAccounts_SystemAccess_DeviceAccountMailAgeFilter;

  // Windows.ApplicationModel.UserDataAccounts.SystemAccess.DeviceAccountServerType
  UserDataAccounts_SystemAccess_DeviceAccountServerType = (
    Exchange = 0,
    Pop = 1,
    Imap = 2
  );
  PUserDataAccounts_SystemAccess_DeviceAccountServerType = ^UserDataAccounts_SystemAccess_DeviceAccountServerType;

  // Windows.ApplicationModel.UserDataAccounts.SystemAccess.DeviceAccountSyncScheduleKind
  UserDataAccounts_SystemAccess_DeviceAccountSyncScheduleKind = (
    Manual = 0,
    Every15Minutes = 1,
    Every30Minutes = 2,
    Every60Minutes = 3,
    Every2Hours = 4,
    Daily = 5,
    AsItemsArrive = 6
  );
  PUserDataAccounts_SystemAccess_DeviceAccountSyncScheduleKind = ^UserDataAccounts_SystemAccess_DeviceAccountSyncScheduleKind;

  // Windows.ApplicationModel.UserDataAccounts.UserDataAccountContentKinds
  UserDataAccounts_UserDataAccountContentKinds = (
    Email = 1,
    Contact = 2,
    Appointment = 4
  );
  PUserDataAccounts_UserDataAccountContentKinds = ^UserDataAccounts_UserDataAccountContentKinds;

  // Windows.ApplicationModel.UserDataAccounts.UserDataAccountOtherAppReadAccess
  UserDataAccounts_UserDataAccountOtherAppReadAccess = (
    SystemOnly = 0,
    Full = 1,
    None = 2
  );
  PUserDataAccounts_UserDataAccountOtherAppReadAccess = ^UserDataAccounts_UserDataAccountOtherAppReadAccess;

  // Windows.ApplicationModel.UserDataAccounts.UserDataAccountStoreAccessType
  UserDataAccounts_UserDataAccountStoreAccessType = (
    AllAccountsReadOnly = 0,
    AppAccountsReadWrite = 1
  );
  PUserDataAccounts_UserDataAccountStoreAccessType = ^UserDataAccounts_UserDataAccountStoreAccessType;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskDaysOfWeek
  UserDataTasks_UserDataTaskDaysOfWeek = (
    None = 0,
    Sunday = 1,
    Monday = 2,
    Tuesday = 4,
    Wednesday = 8,
    Thursday = 16,
    Friday = 32,
    Saturday = 64
  );
  PUserDataTasks_UserDataTaskDaysOfWeek = ^UserDataTasks_UserDataTaskDaysOfWeek;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskDetailsKind
  UserDataTasks_UserDataTaskDetailsKind = (
    PlainText = 0,
    Html = 1
  );
  PUserDataTasks_UserDataTaskDetailsKind = ^UserDataTasks_UserDataTaskDetailsKind;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskKind
  UserDataTasks_UserDataTaskKind = (
    Single = 0,
    Recurring = 1,
    Regenerating = 2
  );
  PUserDataTasks_UserDataTaskKind = ^UserDataTasks_UserDataTaskKind;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskListOtherAppReadAccess
  UserDataTasks_UserDataTaskListOtherAppReadAccess = (
    Full = 0,
    SystemOnly = 1,
    None = 2
  );
  PUserDataTasks_UserDataTaskListOtherAppReadAccess = ^UserDataTasks_UserDataTaskListOtherAppReadAccess;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskListOtherAppWriteAccess
  UserDataTasks_UserDataTaskListOtherAppWriteAccess = (
    Limited = 0,
    None = 1
  );
  PUserDataTasks_UserDataTaskListOtherAppWriteAccess = ^UserDataTasks_UserDataTaskListOtherAppWriteAccess;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskListSyncStatus
  UserDataTasks_UserDataTaskListSyncStatus = (
    Idle = 0,
    Syncing = 1,
    UpToDate = 2,
    AuthenticationError = 3,
    PolicyError = 4,
    UnknownError = 5
  );
  PUserDataTasks_UserDataTaskListSyncStatus = ^UserDataTasks_UserDataTaskListSyncStatus;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskPriority
  UserDataTasks_UserDataTaskPriority = (
    Normal = 0,
    Low = -1,
    High = 1
  );
  PUserDataTasks_UserDataTaskPriority = ^UserDataTasks_UserDataTaskPriority;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskQueryKind
  UserDataTasks_UserDataTaskQueryKind = (
    All = 0,
    Incomplete = 1,
    Complete = 2
  );
  PUserDataTasks_UserDataTaskQueryKind = ^UserDataTasks_UserDataTaskQueryKind;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskQuerySortProperty
  UserDataTasks_UserDataTaskQuerySortProperty = (
    DueDate = 0
  );
  PUserDataTasks_UserDataTaskQuerySortProperty = ^UserDataTasks_UserDataTaskQuerySortProperty;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskRecurrenceUnit
  UserDataTasks_UserDataTaskRecurrenceUnit = (
    Daily = 0,
    Weekly = 1,
    Monthly = 2,
    MonthlyOnDay = 3,
    Yearly = 4,
    YearlyOnDay = 5
  );
  PUserDataTasks_UserDataTaskRecurrenceUnit = ^UserDataTasks_UserDataTaskRecurrenceUnit;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskRegenerationUnit
  UserDataTasks_UserDataTaskRegenerationUnit = (
    Daily = 0,
    Weekly = 1,
    Monthly = 2,
    Yearly = 4
  );
  PUserDataTasks_UserDataTaskRegenerationUnit = ^UserDataTasks_UserDataTaskRegenerationUnit;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskSensitivity
  UserDataTasks_UserDataTaskSensitivity = (
    &Public = 0,
    &Private = 1
  );
  PUserDataTasks_UserDataTaskSensitivity = ^UserDataTasks_UserDataTaskSensitivity;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskStoreAccessType
  UserDataTasks_UserDataTaskStoreAccessType = (
    AppTasksReadWrite = 0,
    AllTasksLimitedReadWrite = 1
  );
  PUserDataTasks_UserDataTaskStoreAccessType = ^UserDataTasks_UserDataTaskStoreAccessType;

  // Windows.ApplicationModel.UserDataTasks.UserDataTaskWeekOfMonth
  UserDataTasks_UserDataTaskWeekOfMonth = (
    First = 0,
    Second = 1,
    Third = 2,
    Fourth = 3,
    Last = 4
  );
  PUserDataTasks_UserDataTaskWeekOfMonth = ^UserDataTasks_UserDataTaskWeekOfMonth;

  // Windows.ApplicationModel.VoiceCommands.VoiceCommandCompletionReason
  VoiceCommands_VoiceCommandCompletionReason = (
    Unknown = 0,
    CommunicationFailed = 1,
    ResourceLimitsExceeded = 2,
    Canceled = 3,
    TimeoutExceeded = 4,
    AppLaunched = 5,
    Completed = 6
  );
  PVoiceCommands_VoiceCommandCompletionReason = ^VoiceCommands_VoiceCommandCompletionReason;

  // Windows.ApplicationModel.VoiceCommands.VoiceCommandContentTileType
  VoiceCommands_VoiceCommandContentTileType = (
    TitleOnly = 0,
    TitleWithText = 1,
    TitleWith68x68Icon = 2,
    TitleWith68x68IconAndText = 3,
    TitleWith68x92Icon = 4,
    TitleWith68x92IconAndText = 5,
    TitleWith280x140Icon = 6,
    TitleWith280x140IconAndText = 7
  );
  PVoiceCommands_VoiceCommandContentTileType = ^VoiceCommands_VoiceCommandContentTileType;

  // Windows.ApplicationModel.Wallet.System.WalletItemAppAssociation
  Wallet_System_WalletItemAppAssociation = (
    None = 0,
    AppInstalled = 1,
    AppNotInstalled = 2
  );
  PWallet_System_WalletItemAppAssociation = ^Wallet_System_WalletItemAppAssociation;

  // Windows.ApplicationModel.Wallet.WalletActionKind
  Wallet_WalletActionKind = (
    OpenItem = 0,
    Transaction = 1,
    MoreTransactions = 2,
    &Message = 3,
    Verb = 4
  );
  PWallet_WalletActionKind = ^Wallet_WalletActionKind;

  // Windows.ApplicationModel.Wallet.WalletBarcodeSymbology
  Wallet_WalletBarcodeSymbology = (
    Invalid = 0,
    Upca = 1,
    Upce = 2,
    Ean13 = 3,
    Ean8 = 4,
    Itf = 5,
    Code39 = 6,
    Code128 = 7,
    Qr = 8,
    Pdf417 = 9,
    Aztec = 10,
    Custom = 100000
  );
  PWallet_WalletBarcodeSymbology = ^Wallet_WalletBarcodeSymbology;

  // Windows.ApplicationModel.Wallet.WalletDetailViewPosition
  Wallet_WalletDetailViewPosition = (
    Hidden = 0,
    HeaderField1 = 1,
    HeaderField2 = 2,
    PrimaryField1 = 3,
    PrimaryField2 = 4,
    SecondaryField1 = 5,
    SecondaryField2 = 6,
    SecondaryField3 = 7,
    SecondaryField4 = 8,
    SecondaryField5 = 9,
    CenterField1 = 10,
    FooterField1 = 11,
    FooterField2 = 12,
    FooterField3 = 13,
    FooterField4 = 14
  );
  PWallet_WalletDetailViewPosition = ^Wallet_WalletDetailViewPosition;

  // Windows.ApplicationModel.Wallet.WalletItemKind
  Wallet_WalletItemKind = (
    Invalid = 0,
    Deal = 1,
    General = 2,
    PaymentInstrument = 3,
    Ticket = 4,
    BoardingPass = 5,
    MembershipCard = 6
  );
  PWallet_WalletItemKind = ^Wallet_WalletItemKind;

  // Windows.ApplicationModel.Wallet.WalletSummaryViewPosition
  Wallet_WalletSummaryViewPosition = (
    Hidden = 0,
    Field1 = 1,
    Field2 = 2
  );
  PWallet_WalletSummaryViewPosition = ^Wallet_WalletSummaryViewPosition;

  // Windows.ApplicationModel Records
  // Windows.ApplicationModel.Activation.ActivatedEventsContract
  Activation_ActivatedEventsContract = record
  end;
  PActivation_ActivatedEventsContract = ^Activation_ActivatedEventsContract;

  // Windows.ApplicationModel.Activation.ActivationCameraSettingsContract
  Activation_ActivationCameraSettingsContract = record
  end;
  PActivation_ActivationCameraSettingsContract = ^Activation_ActivationCameraSettingsContract;

  // Windows.ApplicationModel.Activation.ContactActivatedEventsContract
  Activation_ContactActivatedEventsContract = record
  end;
  PActivation_ContactActivatedEventsContract = ^Activation_ContactActivatedEventsContract;

  // Windows.ApplicationModel.Activation.WebUISearchActivatedEventsContract
  Activation_WebUISearchActivatedEventsContract = record
  end;
  PActivation_WebUISearchActivatedEventsContract = ^Activation_WebUISearchActivatedEventsContract;

  // Windows.ApplicationModel.Calls.Background.CallsBackgroundContract
  Calls_Background_CallsBackgroundContract = record
  end;
  PCalls_Background_CallsBackgroundContract = ^Calls_Background_CallsBackgroundContract;

  // Windows.ApplicationModel.Calls.CallsPhoneContract
  Calls_CallsPhoneContract = record
  end;
  PCalls_CallsPhoneContract = ^Calls_CallsPhoneContract;

  // Windows.ApplicationModel.Calls.CallsVoipContract
  Calls_CallsVoipContract = record
  end;
  PCalls_CallsVoipContract = ^Calls_CallsVoipContract;

  // Windows.ApplicationModel.Calls.LockScreenCallContract
  Calls_LockScreenCallContract = record
  end;
  PCalls_LockScreenCallContract = ^Calls_LockScreenCallContract;

  // Windows.ApplicationModel.CommunicationBlocking.CommunicationBlockingContract
  CommunicationBlocking_CommunicationBlockingContract = record
  end;
  PCommunicationBlocking_CommunicationBlockingContract = ^CommunicationBlocking_CommunicationBlockingContract;

  // Windows.ApplicationModel.FullTrustAppContract
  FullTrustAppContract = record
  end;
  PFullTrustAppContract = ^FullTrustAppContract;

  // Windows.ApplicationModel.PackageInstallProgress
  PackageInstallProgress = record
    PercentComplete: Cardinal;
  end;
  PPackageInstallProgress = ^PackageInstallProgress;

  // Windows.ApplicationModel.Preview.InkWorkspace.PreviewInkWorkspaceContract
  Preview_InkWorkspace_PreviewInkWorkspaceContract = record
  end;
  PPreview_InkWorkspace_PreviewInkWorkspaceContract = ^Preview_InkWorkspace_PreviewInkWorkspaceContract;

  // Windows.ApplicationModel.Preview.Notes.PreviewNotesContract
  Preview_Notes_PreviewNotesContract = record
  end;
  PPreview_Notes_PreviewNotesContract = ^Preview_Notes_PreviewNotesContract;

  // Windows.ApplicationModel.Resources.Core.ResourceLayoutInfo
  Resources_Core_ResourceLayoutInfo = record
    MajorVersion: Cardinal;
    MinorVersion: Cardinal;
    ResourceSubtreeCount: Cardinal;
    NamedResourceCount: Cardinal;
    Checksum: Integer;
  end;
  PResources_Core_ResourceLayoutInfo = ^Resources_Core_ResourceLayoutInfo;

  // Windows.ApplicationModel.Resources.Management.ResourceIndexerContract
  Resources_Management_ResourceIndexerContract = record
  end;
  PResources_Management_ResourceIndexerContract = ^Resources_Management_ResourceIndexerContract;

  // Windows.ApplicationModel.Search.Core.SearchCoreContract
  Search_Core_SearchCoreContract = record
  end;
  PSearch_Core_SearchCoreContract = ^Search_Core_SearchCoreContract;

  // Windows.ApplicationModel.Search.SearchContract
  Search_SearchContract = record
  end;
  PSearch_SearchContract = ^Search_SearchContract;

  // Windows.ApplicationModel.SocialInfo.SocialInfoContract
  SocialInfo_SocialInfoContract = record
  end;
  PSocialInfo_SocialInfoContract = ^SocialInfo_SocialInfoContract;

  // Windows.ApplicationModel.StartupTaskContract
  StartupTaskContract = record
  end;
  PStartupTaskContract = ^StartupTaskContract;

  // Windows.ApplicationModel.Wallet.WalletContract
  Wallet_WalletContract = record
  end;
  PWallet_WalletContract = ^Wallet_WalletContract;

  // Windows.ApplicationModel Interfaces

  // UsedAPI Interface
  // Windows.ApplicationModel.IEnteredBackgroundEventArgs
  IEnteredBackgroundEventArgs = interface(IInspectable)
  ['{F722DCC2-9827-403D-AAED-ECCA9AC17398}']
    function GetDeferral: IDeferral; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.ISuspendingDeferral
  ISuspendingDeferral = interface(IInspectable)
  ['{59140509-8BC9-4EB4-B636-DABDC4F46F66}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.ISuspendingOperation
  ISuspendingOperation = interface(IInspectable)
  ['{9DA4CA41-20E1-4E9B-9F65-A9F435340C3A}']
    function GetDeferral: ISuspendingDeferral; safecall;
    function get_Deadline: DateTime; safecall;
    property Deadline: DateTime read get_Deadline;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.ISuspendingEventArgs
  ISuspendingEventArgs = interface(IInspectable)
  ['{96061C05-2DBA-4D08-B0BD-2B30A131C6AA}']
    function get_SuspendingOperation: ISuspendingOperation; safecall;
    property SuspendingOperation: ISuspendingOperation read get_SuspendingOperation;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.ILeavingBackgroundEventArgs
  ILeavingBackgroundEventArgs = interface(IInspectable)
  ['{39C6EC9A-AE6E-46F9-A07A-CFC23F88733E}']
    function GetDeferral: IDeferral; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.ILaunchActivatedEventArgs
  Activation_ILaunchActivatedEventArgs = interface(IInspectable)
  ['{FBC93E26-A14A-4B4F-82B0-33BED920AF52}']
    function get_Arguments: HSTRING; safecall;
    function get_TileId: HSTRING; safecall;
    property Arguments: HSTRING read get_Arguments;
    property TileId: HSTRING read get_TileId;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.IFileActivatedEventArgs
  Activation_IFileActivatedEventArgs = interface(IInspectable)
  ['{BB2AFC33-93B1-42ED-8B26-236DD9C78496}']
    function get_Files: IVectorView_1__IStorageItem; safecall;
    function get_Verb: HSTRING; safecall;
    property Files: IVectorView_1__IStorageItem read get_Files;
    property Verb: HSTRING read get_Verb;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.ISearchActivatedEventArgs
  Activation_ISearchActivatedEventArgs = interface(IInspectable)
  ['{8CB36951-58C8-43E3-94BC-41D33F8B630E}']
    function get_QueryText: HSTRING; safecall;
    function get_Language: HSTRING; safecall;
    property Language: HSTRING read get_Language;
    property QueryText: HSTRING read get_QueryText;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.IShareTargetActivatedEventArgs
  Activation_IShareTargetActivatedEventArgs = interface(IInspectable)
  ['{4BDAF9C8-CDB2-4ACB-BFC3-6648563378EC}']
    function get_ShareOperation: ShareTarget_IShareOperation; safecall;
    property ShareOperation: ShareTarget_IShareOperation read get_ShareOperation;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.IFileOpenPickerActivatedEventArgs
  Activation_IFileOpenPickerActivatedEventArgs = interface(IInspectable)
  ['{72827082-5525-4BF2-BC09-1F5095D4964D}']
    function get_FileOpenPickerUI: Pickers_Provider_IFileOpenPickerUI; safecall;
    property FileOpenPickerUI: Pickers_Provider_IFileOpenPickerUI read get_FileOpenPickerUI;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.IFileSavePickerActivatedEventArgs
  Activation_IFileSavePickerActivatedEventArgs = interface(IInspectable)
  ['{81C19CF1-74E6-4387-82EB-BB8FD64B4346}']
    function get_FileSavePickerUI: Pickers_Provider_IFileSavePickerUI; safecall;
    property FileSavePickerUI: Pickers_Provider_IFileSavePickerUI read get_FileSavePickerUI;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.ICachedFileUpdaterActivatedEventArgs
  Activation_ICachedFileUpdaterActivatedEventArgs = interface(IInspectable)
  ['{D06EB1C7-3805-4ECB-B757-6CF15E26FEF3}']
    function get_CachedFileUpdaterUI: Provider_ICachedFileUpdaterUI; safecall;
    property CachedFileUpdaterUI: Provider_ICachedFileUpdaterUI read get_CachedFileUpdaterUI;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Activation.IBackgroundActivatedEventArgs
  Activation_IBackgroundActivatedEventArgs = interface(IInspectable)
  ['{AB14BEE0-E760-440E-A91C-44796DE3A92D}']
    function get_TaskInstance: IBackgroundTaskInstance; safecall;
    property TaskInstance: IBackgroundTaskInstance read get_TaskInstance;
  end;

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

  // Windows.Foundation.IReference`1<UInt32>
  IReference_1__Cardinal = interface(IInspectable)
  ['{513EF3AF-E784-5325-A91E-97C2B8111CF3}']
    function get_Value: Cardinal; safecall;
    property Value: Cardinal read get_Value;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Object>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable = interface(IUnknown)
  ['{24C1E235-49C9-57D0-AAB2-00275EE76994}']
    procedure Invoke(sender: Provider_ICachedFileUpdaterUI; args: IInspectable); safecall;
  end;

  // Windows.ApplicationModel.Activation.IContactsProviderActivatedEventArgs
  Activation_IContactsProviderActivatedEventArgs = interface(IInspectable)
  ['{4580DCA8-5750-4916-AA52-C0829521EB94}']
    function get_Verb: HSTRING; safecall;
    property Verb: HSTRING read get_Verb;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  AsyncOperationCompletedHandler_1__Cardinal = interface(IUnknown)
  ['{9343B6E7-E3D2-5E4A-AB2D-2BCE4919A6A4}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<UInt32>
  IAsyncOperation_1__Cardinal = interface(IInspectable)
  ['{EF60385F-BE78-584B-AAEF-7829ADA2B0DE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Cardinal); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Cardinal; safecall;
    function GetResults: Cardinal; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Cardinal read get_Completed write put_Completed;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Search.IStorageQueryResultBase,Object>
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = interface(IUnknown)
  ['{4BA22861-00C4-597F-B6BF-3AF516F3B870}']
    procedure Invoke(sender: Search_IStorageQueryResultBase; args: IInspectable); safecall;
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

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Object>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable = interface(IUnknown)
  ['{BB54A5EA-3A8B-57FC-81E5-AE9EB9864FAE}']
    procedure Invoke(sender: Pickers_Provider_IFileSavePickerUI; args: IInspectable); safecall;
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

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>>
  IIterable_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING_Base = interface(IInspectable)
  ['{A4CD6151-2CC1-56F1-9014-DF6BA3410BEB}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>>
  IIterable_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = interface(IIterable_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING_Base)
  ['{A4CD6151-2CC1-56F1-9014-DF6BA3410BEB}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IPackage>
  IIterator_1__IPackage_Base = interface(IInspectable)
  ['{0217F069-025C-5EE6-A87F-E782E3B623AE}']
    function get_Current: IPackage; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIPackage): Cardinal; safecall;
    property Current: IPackage read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IPackage>
  IIterator_1__IPackage = interface(IIterator_1__IPackage_Base)
  ['{33045CC3-8892-5AAA-AB26-83F5614F6792}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IPackage>
  IIterable_1__IPackage_Base = interface(IInspectable)
  ['{69AD6AA7-0C49-5F27-A5EB-EF4D59467B6D}']
    function First: IIterator_1__IPackage; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IPackage>
  IIterable_1__IPackage = interface(IIterable_1__IPackage_Base)
  ['{9ECA5389-C150-5A23-98C4-E5A26309B5B3}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IAppInfo
  [WinRTClassNameAttribute(SWindows_ApplicationModel_AppInfo)]
  IAppInfo = interface(IInspectable)
  ['{CF7F59B3-6A09-4DE8-A6C0-5792D56880D1}']
    function get_Id: HSTRING; safecall;
    function get_AppUserModelId: HSTRING; safecall;
    function get_DisplayInfo: IAppDisplayInfo; safecall;
    function get_PackageFamilyName: HSTRING; safecall;
    property AppUserModelId: HSTRING read get_AppUserModelId;
    property DisplayInfo: IAppDisplayInfo read get_DisplayInfo;
    property Id: HSTRING read get_Id;
    property PackageFamilyName: HSTRING read get_PackageFamilyName;
  end;

  // Windows.ApplicationModel.AppExtensions.IAppExtension
  AppExtensions_IAppExtension = interface(IInspectable)
  ['{8450902C-15ED-4FAF-93EA-2237BBF8CBD6}']
    function get_Id: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_Package: IPackage; safecall;
    function get_AppInfo: IAppInfo; safecall;
    function GetExtensionPropertiesAsync: IAsyncOperation_1__IPropertySet; safecall;
    function GetPublicFolderAsync: IAsyncOperation_1__IStorageFolder; safecall;
    property AppInfo: IAppInfo read get_AppInfo;
    property Description: HSTRING read get_Description;
    property DisplayName: HSTRING read get_DisplayName;
    property Id: HSTRING read get_Id;
    property Package: IPackage read get_Package;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IIterator_1__AppExtensions_IAppExtension_Base = interface(IInspectable)
  ['{8E80CA83-3CD3-5F9C-83E4-84347CA5498C}']
    function get_Current: AppExtensions_IAppExtension; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAppExtensions_IAppExtension): Cardinal; safecall;
    property Current: AppExtensions_IAppExtension read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IIterator_1__AppExtensions_IAppExtension = interface(IIterator_1__AppExtensions_IAppExtension_Base)
  ['{4A8EAC9C-606C-5772-80DA-AB77456837A5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IIterable_1__AppExtensions_IAppExtension_Base = interface(IInspectable)
  ['{3B4FE356-1B13-59CB-AB1F-C4667A74756B}']
    function First: IIterator_1__AppExtensions_IAppExtension; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IIterable_1__AppExtensions_IAppExtension = interface(IIterable_1__AppExtensions_IAppExtension_Base)
  ['{ACFFA2EC-084E-51F1-A9CF-E17E266D3332}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>
  IVectorView_1__AppExtensions_IAppExtension = interface(IInspectable)
  ['{5F917E50-6D70-56D4-BE5F-AFD72E2799D5}']
    function GetAt(index: Cardinal): AppExtensions_IAppExtension; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: AppExtensions_IAppExtension; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAppExtensions_IAppExtension): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>>
  AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension_Delegate_Base = interface(IUnknown)
  ['{CBD3EA3B-1275-5688-8610-0AB1F83442FC}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__AppExtensions_IAppExtension; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>>
  AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension = interface(AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension_Delegate_Base)
  ['{A5F0FF22-A276-5DB2-A4E4-DE758B4C2CD9}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>>
  IAsyncOperation_1__IVectorView_1__AppExtensions_IAppExtension_Base = interface(IInspectable)
  ['{83295BB9-10DF-530F-A0D7-BE05BA80CB18}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension; safecall;
    function GetResults: IVectorView_1__AppExtensions_IAppExtension; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__AppExtensions_IAppExtension read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.AppExtensions.IAppExtension>>
  IAsyncOperation_1__IVectorView_1__AppExtensions_IAppExtension = interface(IAsyncOperation_1__IVectorView_1__AppExtensions_IAppExtension_Base)
  ['{75C04410-5E1A-59A9-B030-943B4F74093C}']
  end;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageInstalledEventArgs
  AppExtensions_IAppExtensionPackageInstalledEventArgs = interface(IInspectable)
  ['{39E59234-3351-4A8D-9745-E7D3DD45BC48}']
    function get_AppExtensionName: HSTRING; safecall;
    function get_Package: IPackage; safecall;
    function get_Extensions: IVectorView_1__AppExtensions_IAppExtension; safecall;
    property AppExtensionName: HSTRING read get_AppExtensionName;
    property Extensions: IVectorView_1__AppExtensions_IAppExtension read get_Extensions;
    property Package: IPackage read get_Package;
  end;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageUpdatingEventArgs
  AppExtensions_IAppExtensionPackageUpdatingEventArgs = interface(IInspectable)
  ['{7ED59329-1A65-4800-A700-B321009E306A}']
    function get_AppExtensionName: HSTRING; safecall;
    function get_Package: IPackage; safecall;
    property AppExtensionName: HSTRING read get_AppExtensionName;
    property Package: IPackage read get_Package;
  end;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageUpdatedEventArgs
  AppExtensions_IAppExtensionPackageUpdatedEventArgs = interface(IInspectable)
  ['{3A83C43F-797E-44B5-BA24-A4C8B5A543D7}']
    function get_AppExtensionName: HSTRING; safecall;
    function get_Package: IPackage; safecall;
    function get_Extensions: IVectorView_1__AppExtensions_IAppExtension; safecall;
    property AppExtensionName: HSTRING read get_AppExtensionName;
    property Extensions: IVectorView_1__AppExtensions_IAppExtension read get_Extensions;
    property Package: IPackage read get_Package;
  end;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageUninstallingEventArgs
  AppExtensions_IAppExtensionPackageUninstallingEventArgs = interface(IInspectable)
  ['{60F160C5-171E-40FF-AE98-AB2C20DD4D75}']
    function get_AppExtensionName: HSTRING; safecall;
    function get_Package: IPackage; safecall;
    property AppExtensionName: HSTRING read get_AppExtensionName;
    property Package: IPackage read get_Package;
  end;

  // Windows.ApplicationModel.AppExtensions.IAppExtensionPackageStatusChangedEventArgs
  AppExtensions_IAppExtensionPackageStatusChangedEventArgs = interface(IInspectable)
  ['{1CE17433-1153-44FD-87B1-8AE1050303DF}']
    function get_AppExtensionName: HSTRING; safecall;
    function get_Package: IPackage; safecall;
    property AppExtensionName: HSTRING read get_AppExtensionName;
    property Package: IPackage read get_Package;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IAppInfo>
  IIterator_1__IAppInfo_Base = interface(IInspectable)
  ['{69CEC62C-41EB-5D69-A475-29EE22323DD8}']
    function get_Current: IAppInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIAppInfo): Cardinal; safecall;
    property Current: IAppInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IAppInfo>
  IIterator_1__IAppInfo = interface(IIterator_1__IAppInfo_Base)
  ['{9CA3E755-877F-5FA6-9C68-CEB20FF87804}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IAppInfo>
  IIterable_1__IAppInfo_Base = interface(IInspectable)
  ['{63D0BFFE-0E34-55B3-83D5-314CAFF2B137}']
    function First: IIterator_1__IAppInfo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IAppInfo>
  IIterable_1__IAppInfo = interface(IIterable_1__IAppInfo_Base)
  ['{F5021AB5-F6AC-59C9-9632-6E82797B6196}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>
  IVectorView_1__IAppInfo = interface(IInspectable)
  ['{A7320DC1-90CE-5568-98B9-707AC47E5C33}']
    function GetAt(index: Cardinal): IAppInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IAppInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo_Delegate_Base = interface(IUnknown)
  ['{07F25B6F-F054-5649-A5CE-B348DDC618B6}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IAppInfo; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo_Delegate_Base)
  ['{3FA954FE-6B27-52B7-A97F-E691E1BC3B53}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>>
  IAsyncOperation_1__IVectorView_1__IAppInfo_Base = interface(IInspectable)
  ['{07543D91-8610-5152-B0E4-43D6E4CDD0CB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo; safecall;
    function GetResults: IVectorView_1__IAppInfo; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IAppInfo read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IAppInfo>>
  IAsyncOperation_1__IVectorView_1__IAppInfo = interface(IAsyncOperation_1__IVectorView_1__IAppInfo_Base)
  ['{74E17AC2-5438-594C-9B2B-4D85BB307A73}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceClosedEventArgs
  AppService_IAppServiceClosedEventArgs = interface(IInspectable)
  ['{DE6016F6-CB03-4D35-AC8D-CC6303239731}']
    function get_Status: AppService_AppServiceClosedStatus; safecall;
    property Status: AppService_AppServiceClosedStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.AppServiceConnectionStatus>
  AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus_Delegate_Base = interface(IUnknown)
  ['{B6C6BBF2-72CA-5799-A651-D1990670097B}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__AppService_AppServiceConnectionStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.AppServiceConnectionStatus>
  AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus = interface(AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.AppServiceConnectionStatus>
  IAsyncOperation_1__AppService_AppServiceConnectionStatus_Base = interface(IInspectable)
  ['{0D0E6663-2639-5A9A-9CBC-30D7D4CE533B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus; safecall;
    function GetResults: AppService_AppServiceConnectionStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__AppService_AppServiceConnectionStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.AppServiceConnectionStatus>
  IAsyncOperation_1__AppService_AppServiceConnectionStatus = interface(IAsyncOperation_1__AppService_AppServiceConnectionStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceResponse
  AppService_IAppServiceResponse = interface(IInspectable)
  ['{8D503CEC-9AA3-4E68-9559-9DE63E372CE4}']
    function get_Message: IPropertySet; safecall;
    function get_Status: AppService_AppServiceResponseStatus; safecall;
    property &Message: IPropertySet read get_Message;
    property Status: AppService_AppServiceResponseStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.IAppServiceResponse>
  AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse_Delegate_Base = interface(IUnknown)
  ['{7EA7D7EC-E164-52C3-8E32-BBA7126D9028}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__AppService_IAppServiceResponse; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.IAppServiceResponse>
  AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse = interface(AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse_Delegate_Base)
  ['{5A4BFBFF-4CE1-5E2F-B758-8FADCC08AA8C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.IAppServiceResponse>
  IAsyncOperation_1__AppService_IAppServiceResponse_Base = interface(IInspectable)
  ['{48755A7C-C88F-5EF0-9B4C-876FCC2610B4}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse; safecall;
    function GetResults: AppService_IAppServiceResponse; safecall;
    property Completed: AsyncOperationCompletedHandler_1__AppService_IAppServiceResponse read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.IAppServiceResponse>
  IAsyncOperation_1__AppService_IAppServiceResponse = interface(IAsyncOperation_1__AppService_IAppServiceResponse_Base)
  ['{D75C9CFD-0669-5A8E-AE3C-6DAEF08F6377}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.AppServiceResponseStatus>
  AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus_Delegate_Base = interface(IUnknown)
  ['{B824383D-32E0-5579-8670-A06A61457F20}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__AppService_AppServiceResponseStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.AppServiceResponseStatus>
  AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus = interface(AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.AppServiceResponseStatus>
  IAsyncOperation_1__AppService_AppServiceResponseStatus_Base = interface(IInspectable)
  ['{98FDB842-5A0B-539A-A35C-55AC5F228612}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus; safecall;
    function GetResults: AppService_AppServiceResponseStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__AppService_AppServiceResponseStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.AppServiceResponseStatus>
  IAsyncOperation_1__AppService_AppServiceResponseStatus = interface(IAsyncOperation_1__AppService_AppServiceResponseStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceRequest
  AppService_IAppServiceRequest = interface(IInspectable)
  ['{20E58D9D-18DE-4B01-80BA-90A76204E3C8}']
    function get_Message: IPropertySet; safecall;
    function SendResponseAsync(&message: IPropertySet): IAsyncOperation_1__AppService_AppServiceResponseStatus; safecall;
    property &Message: IPropertySet read get_Message;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceDeferral
  AppService_IAppServiceDeferral = interface(IInspectable)
  ['{7E1B5322-EAB0-4248-AE04-FDF93838E472}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceRequestReceivedEventArgs
  AppService_IAppServiceRequestReceivedEventArgs = interface(IInspectable)
  ['{6E122360-FF65-44AE-9E45-857FE4180681}']
    function get_Request: AppService_IAppServiceRequest; safecall;
    function GetDeferral: AppService_IAppServiceDeferral; safecall;
    property Request: AppService_IAppServiceRequest read get_Request;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.AppService.IAppServiceConnection,Windows.ApplicationModel.AppService.IAppServiceRequestReceivedEventArgs>
  TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceRequestReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{18C67D61-4176-5553-B18D-D8F57FE79552}']
    procedure Invoke(sender: AppService_IAppServiceConnection; args: AppService_IAppServiceRequestReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.AppService.IAppServiceConnection,Windows.ApplicationModel.AppService.IAppServiceRequestReceivedEventArgs>
  TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceRequestReceivedEventArgs = interface(TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceRequestReceivedEventArgs_Delegate_Base)
  ['{6AE5027F-820F-553A-9187-32660AA33A35}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.AppService.IAppServiceConnection,Windows.ApplicationModel.AppService.IAppServiceClosedEventArgs>
  TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceClosedEventArgs_Delegate_Base = interface(IUnknown)
  ['{E4EFA98D-4BFC-5E61-A233-688F5F06521F}']
    procedure Invoke(sender: AppService_IAppServiceConnection; args: AppService_IAppServiceClosedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.AppService.IAppServiceConnection,Windows.ApplicationModel.AppService.IAppServiceClosedEventArgs>
  TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceClosedEventArgs = interface(TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceClosedEventArgs_Delegate_Base)
  ['{71776122-4988-5F38-8637-D8876D35AD61}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceConnection
  [WinRTClassNameAttribute(SWindows_ApplicationModel_AppService_AppServiceConnection)]
  AppService_IAppServiceConnection = interface(IInspectable)
  ['{9DD474A2-871F-4D52-89A9-9E090531BD27}']
    function get_AppServiceName: HSTRING; safecall;
    procedure put_AppServiceName(value: HSTRING); safecall;
    function get_PackageFamilyName: HSTRING; safecall;
    procedure put_PackageFamilyName(value: HSTRING); safecall;
    function OpenAsync: IAsyncOperation_1__AppService_AppServiceConnectionStatus; safecall;
    function SendMessageAsync(&message: IPropertySet): IAsyncOperation_1__AppService_IAppServiceResponse; safecall;
    function add_RequestReceived(handler: TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceRequestReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RequestReceived(token: EventRegistrationToken); safecall;
    function add_ServiceClosed(handler: TypedEventHandler_2__AppService_IAppServiceConnection__AppService_IAppServiceClosedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ServiceClosed(token: EventRegistrationToken); safecall;
    property AppServiceName: HSTRING read get_AppServiceName write put_AppServiceName;
    property PackageFamilyName: HSTRING read get_PackageFamilyName write put_PackageFamilyName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceConnection2
  AppService_IAppServiceConnection2 = interface(IInspectable)
  ['{8BDFCD5F-2302-4FBD-8061-52511C2F8BF9}']
    function OpenRemoteAsync(remoteSystemConnectionRequest: RemoteSystems_IRemoteSystemConnectionRequest): IAsyncOperation_1__AppService_AppServiceConnectionStatus; safecall;
    function get_User: IUser; safecall;
    procedure put_User(value: IUser); safecall;
    property User: IUser read get_User write put_User;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IStatelessAppServiceResponse
  AppService_IStatelessAppServiceResponse = interface(IInspectable)
  ['{43754AF7-A9EC-52FE-82E7-939B68DC9388}']
    function get_Message: IPropertySet; safecall;
    function get_Status: AppService_StatelessAppServiceResponseStatus; safecall;
    property &Message: IPropertySet read get_Message;
    property Status: AppService_StatelessAppServiceResponseStatus read get_Status;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.AppService.IStatelessAppServiceResponse>
  AsyncOperationCompletedHandler_1__AppService_IStatelessAppServiceResponse = interface(IUnknown)
  ['{A7ADC0D9-78CD-5109-8E9E-87873A303FF3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__AppService_IStatelessAppServiceResponse; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.AppService.IStatelessAppServiceResponse>
  IAsyncOperation_1__AppService_IStatelessAppServiceResponse = interface(IInspectable)
  ['{2EEA0E7A-1CF0-5AD9-9199-9A6879F45E10}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__AppService_IStatelessAppServiceResponse); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__AppService_IStatelessAppServiceResponse; safecall;
    function GetResults: AppService_IStatelessAppServiceResponse; safecall;
    property Completed: AsyncOperationCompletedHandler_1__AppService_IStatelessAppServiceResponse read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.AppService.IAppServiceConnectionStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_AppService_AppServiceConnection)]
  AppService_IAppServiceConnectionStatics = interface(IInspectable)
  ['{ADC56CE9-D408-5673-8637-827A4B274168}']
    function SendStatelessMessageAsync(connection: AppService_IAppServiceConnection; connectionRequest: RemoteSystems_IRemoteSystemConnectionRequest; &message: IPropertySet): IAsyncOperation_1__AppService_IStatelessAppServiceResponse; safecall;
  end;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails
  AppService_IAppServiceTriggerDetails = interface(IInspectable)
  ['{88A2DCAC-AD28-41B8-80BB-BDF1B2169E19}']
    function get_Name: HSTRING; safecall;
    function get_CallerPackageFamilyName: HSTRING; safecall;
    function get_AppServiceConnection: AppService_IAppServiceConnection; safecall;
    property AppServiceConnection: AppService_IAppServiceConnection read get_AppServiceConnection;
    property CallerPackageFamilyName: HSTRING read get_CallerPackageFamilyName;
    property Name: HSTRING read get_Name;
  end;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails2
  AppService_IAppServiceTriggerDetails2 = interface(IInspectable)
  ['{E83D54B2-28CC-43F2-B465-C0482E59E2DC}']
    function get_IsRemoteSystemConnection: Boolean; safecall;
    property IsRemoteSystemConnection: Boolean read get_IsRemoteSystemConnection;
  end;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails3
  AppService_IAppServiceTriggerDetails3 = interface(IInspectable)
  ['{FBD71E21-7939-4E68-9E3C-7780147AABB6}']
    function CheckCallerForCapabilityAsync(capabilityName: HSTRING): IAsyncOperation_1__Boolean; safecall;
  end;

  // Windows.ApplicationModel.AppService.IAppServiceTriggerDetails4
  AppService_IAppServiceTriggerDetails4 = interface(IInspectable)
  ['{1185B180-8861-5E30-AB55-1CF4D08BBF6D}']
    function get_CallerRemoteConnectionToken: HSTRING; safecall;
    property CallerRemoteConnectionToken: HSTRING read get_CallerRemoteConnectionToken;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<String>>
  AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING_Delegate_Base = interface(IUnknown)
  ['{7C7899BE-5F2E-5BF3-ADE5-AD98B772C7CD}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__HSTRING; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<String>>
  AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING = interface(AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING_Delegate_Base)
  ['{7C7899BE-5F2E-5BF3-ADE5-AD98B772C7CD}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<String>>
  IAsyncOperation_1__IVectorView_1__HSTRING_Base = interface(IInspectable)
  ['{2F92B529-119B-575A-A419-3904B4E41AF2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING; safecall;
    function GetResults: IVectorView_1__HSTRING; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__HSTRING read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<String>>
  IAsyncOperation_1__IVectorView_1__HSTRING = interface(IAsyncOperation_1__IVectorView_1__HSTRING_Base)
  ['{2F92B529-119B-575A-A419-3904B4E41AF2}']
  end;

  // Windows.Foundation.Collections.IIterator`1<Guid>
  IIterator_1__TGuid = interface(IInspectable)
  ['{D3D64048-82B3-53C7-9285-B0BE18368482}']
    function get_Current: TGuid; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PGuid): Cardinal; safecall;
    property Current: TGuid read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Guid>
  IIterable_1__TGuid = interface(IInspectable)
  ['{F4CA3045-5DD7-54BE-982E-D88D8CA0876E}']
    function First: IIterator_1__TGuid; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Guid>
  IVectorView_1__TGuid = interface(IInspectable)
  ['{9520E64B-15B2-52A6-98ED-3191FA6CF68A}']
    function GetAt(index: Cardinal): TGuid; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TGuid; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGuid): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Guid>
  IVector_1__TGuid_Base = interface(IInspectable)
  ['{482E676D-B913-5EC1-AFA8-5F96922E94AE}']
    function GetAt(index: Cardinal): TGuid; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__TGuid; safecall;
    function IndexOf(value: TGuid; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: TGuid); safecall;
    procedure InsertAt(index: Cardinal; value: TGuid); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: TGuid); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGuid): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PGuid); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Guid>
  IVector_1__TGuid = interface(IVector_1__TGuid_Base)
  ['{482E676D-B913-5EC1-AFA8-5F96922E94AE}']
  end;

  // Windows.Foundation.IReference`1<UInt8>
  IReference_1__Byte = interface(IInspectable)
  ['{E5198CC8-2873-55F5-B0A1-84FF9E4AAD62}']
    function get_Value: Byte; safecall;
    property Value: Byte read get_Value;
  end;

  // Windows.Foundation.IReference`1<UInt64>
  IReference_1__UInt64 = interface(IInspectable)
  ['{6755E376-53BB-568B-A11D-17239868309E}']
    function get_Value: UInt64; safecall;
    property Value: UInt64 read get_Value;
  end;

  // Windows.Foundation.Collections.IIterator`1<Int32>
  IIterator_1__Integer = interface(IInspectable)
  ['{BFEA7F78-50C2-5F1D-A6EA-9E978D2699FF}']
    function get_Current: Integer; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInteger): Cardinal; safecall;
    property Current: Integer read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Int32>
  IIterable_1__Integer = interface(IInspectable)
  ['{81A643FB-F51C-5565-83C4-F96425777B66}']
    function First: IIterator_1__Integer; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Int32>
  IVectorView_1__Integer = interface(IInspectable)
  ['{8D720CDF-3934-5D3F-9A55-40E8063B086A}']
    function GetAt(index: Cardinal): Integer; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Integer; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInteger): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Int32>
  IVector_1__Integer_Base = interface(IInspectable)
  ['{B939AF5B-B45D-5489-9149-61442C1905FE}']
    function GetAt(index: Cardinal): Integer; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Integer; safecall;
    function IndexOf(value: Integer; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Integer); safecall;
    procedure InsertAt(index: Cardinal; value: Integer); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Integer); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInteger): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PInteger); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Int32>
  IVector_1__Integer = interface(IVector_1__Integer_Base)
  ['{B939AF5B-B45D-5489-9149-61442C1905FE}']
  end;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface(IUnknown)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
    procedure Invoke(sender: IInspectable; args: IInspectable); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Guid>
  AsyncOperationCompletedHandler_1__TGuid_Delegate_Base = interface(IUnknown)
  ['{5233899B-BA7E-504F-BB83-CEEBAC62DECF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__TGuid; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Guid>
  AsyncOperationCompletedHandler_1__TGuid = interface(AsyncOperationCompletedHandler_1__TGuid_Delegate_Base)
  ['{5233899B-BA7E-504F-BB83-CEEBAC62DECF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Guid>
  IAsyncOperation_1__TGuid_Base = interface(IInspectable)
  ['{6607BC41-294B-5975-9C3F-4B49836D0916}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__TGuid); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__TGuid; safecall;
    function GetResults: TGuid; safecall;
    property Completed: AsyncOperationCompletedHandler_1__TGuid read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Guid>
  IAsyncOperation_1__TGuid = interface(IAsyncOperation_1__TGuid_Base)
  ['{6607BC41-294B-5975-9C3F-4B49836D0916}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Calls.VoipPhoneCallResourceReservationStatus>
  AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus_Delegate_Base = interface(IUnknown)
  ['{7A27B20F-647A-53FC-80F0-A79D083CE531}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Calls_VoipPhoneCallResourceReservationStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Calls.VoipPhoneCallResourceReservationStatus>
  AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus = interface(AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Calls.VoipPhoneCallResourceReservationStatus>
  IAsyncOperation_1__Calls_VoipPhoneCallResourceReservationStatus_Base = interface(IInspectable)
  ['{8528BE80-7CE9-5668-8E48-469AE5BA9EAD}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus; safecall;
    function GetResults: Calls_VoipPhoneCallResourceReservationStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Calls_VoipPhoneCallResourceReservationStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Calls.VoipPhoneCallResourceReservationStatus>
  IAsyncOperation_1__Calls_VoipPhoneCallResourceReservationStatus = interface(IAsyncOperation_1__Calls_VoipPhoneCallResourceReservationStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>
  IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = interface(IInspectable)
  ['{3E882181-FFAB-529C-B56F-3704D4E76A37}']
    function get_Key: HSTRING; safecall;
    function get_Value: Chat_ChatMessageStatus; safecall;
    property Key: HSTRING read get_Key;
    property Value: Chat_ChatMessageStatus read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>>
  IIterator_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus_Base = interface(IInspectable)
  ['{5B099E05-07E2-5346-B075-F4297B3E308B}']
    function get_Current: IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__Chat_ChatMessageStatus): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>>
  IIterator_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = interface(IIterator_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus_Base)
  ['{223DA751-0A6C-55A6-A771-0400E1D0C302}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>>
  IIterable_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus_Base = interface(IInspectable)
  ['{57D87C13-48E9-546F-9B4E-A3906E1E7C24}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>>
  IIterable_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus = interface(IIterable_1__IKeyValuePair_2__HSTRING__Chat_ChatMessageStatus_Base)
  ['{AEA35BCF-2498-50FE-9FB2-6CEF59B30B2E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>
  IMapView_2__HSTRING__Chat_ChatMessageStatus_Base = interface(IInspectable)
  ['{6A44FF0F-A172-5285-9BA5-B9FDD699A0FE}']
    function Lookup(key: HSTRING): Chat_ChatMessageStatus; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__Chat_ChatMessageStatus; out second: IMapView_2__HSTRING__Chat_ChatMessageStatus); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,Windows.ApplicationModel.Chat.ChatMessageStatus>
  IMapView_2__HSTRING__Chat_ChatMessageStatus = interface(IMapView_2__HSTRING__Chat_ChatMessageStatus_Base)
  ['{8A182EDF-78BB-553A-B6B7-9112F3CC65BF}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Int32>
  AsyncOperationCompletedHandler_1__Integer_Delegate_Base = interface(IUnknown)
  ['{D60CAE9D-88CB-59F1-8576-3FBA44796BE8}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Integer; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Int32>
  AsyncOperationCompletedHandler_1__Integer = interface(AsyncOperationCompletedHandler_1__Integer_Delegate_Base)
  ['{D60CAE9D-88CB-59F1-8576-3FBA44796BE8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Int32>
  IAsyncOperation_1__Integer_Base = interface(IInspectable)
  ['{968B9665-06ED-5774-8F53-8EDEABD5F7B5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Integer); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Integer; safecall;
    function GetResults: Integer; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Integer read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Int32>
  IAsyncOperation_1__Integer = interface(IAsyncOperation_1__Integer_Base)
  ['{968B9665-06ED-5774-8F53-8EDEABD5F7B5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>>
  IIterable_1__IKeyValuePair_2__TGuid__IInspectable_Base = interface(IInspectable)
  ['{F3B20528-E3B3-5331-B2D0-0C2623AEE785}']
    function First: IIterator_1__IKeyValuePair_2__TGuid__IInspectable; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>>
  IIterable_1__IKeyValuePair_2__TGuid__IInspectable = interface(IIterable_1__IKeyValuePair_2__TGuid__IInspectable_Base)
  ['{F3B20528-E3B3-5331-B2D0-0C2623AEE785}']
  end;

  // Windows.Foundation.IReference`1<Int32>
  IReference_1__Integer = interface(IInspectable)
  ['{548CEFBD-BC8A-5FA0-8DF2-957440FC8BF4}']
    function get_Value: Integer; safecall;
    property Value: Integer read get_Value;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ConversationalAgent.DetectionConfigurationTrainingStatus>
  AsyncOperationCompletedHandler_1__ConversationalAgent_DetectionConfigurationTrainingStatus = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ConversationalAgent_DetectionConfigurationTrainingStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ConversationalAgent.DetectionConfigurationTrainingStatus>
  IAsyncOperation_1__ConversationalAgent_DetectionConfigurationTrainingStatus = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ConversationalAgent_DetectionConfigurationTrainingStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ConversationalAgent_DetectionConfigurationTrainingStatus; safecall;
    function GetResults: ConversationalAgent_DetectionConfigurationTrainingStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ConversationalAgent_DetectionConfigurationTrainingStatus read get_Completed write put_Completed;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectionTrainingDataFormat>
  IIterator_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: ConversationalAgent_ActivationSignalDetectionTrainingDataFormat; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PConversationalAgent_ActivationSignalDetectionTrainingDataFormat): Cardinal; safecall;
    property Current: ConversationalAgent_ActivationSignalDetectionTrainingDataFormat read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectionTrainingDataFormat>
  IIterable_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectionTrainingDataFormat>
  IVectorView_1__ConversationalAgent_ActivationSignalDetectionTrainingDataFormat = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): ConversationalAgent_ActivationSignalDetectionTrainingDataFormat; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ConversationalAgent_ActivationSignalDetectionTrainingDataFormat; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PConversationalAgent_ActivationSignalDetectionTrainingDataFormat): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorPowerState>
  IIterator_1__ConversationalAgent_ActivationSignalDetectorPowerState = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: ConversationalAgent_ActivationSignalDetectorPowerState; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PConversationalAgent_ActivationSignalDetectorPowerState): Cardinal; safecall;
    property Current: ConversationalAgent_ActivationSignalDetectorPowerState read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorPowerState>
  IIterable_1__ConversationalAgent_ActivationSignalDetectorPowerState = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__ConversationalAgent_ActivationSignalDetectorPowerState; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.ConversationalAgent.ActivationSignalDetectorPowerState>
  IVectorView_1__ConversationalAgent_ActivationSignalDetectorPowerState = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): ConversationalAgent_ActivationSignalDetectorPowerState; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ConversationalAgent_ActivationSignalDetectorPowerState; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PConversationalAgent_ActivationSignalDetectorPowerState): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ConversationalAgent.ConversationalAgentSessionUpdateResponse>
  AsyncOperationCompletedHandler_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ConversationalAgent.ConversationalAgentSessionUpdateResponse>
  IAsyncOperation_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse; safecall;
    function GetResults: ConversationalAgent_ConversationalAgentSessionUpdateResponse; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ConversationalAgent_ConversationalAgentSessionUpdateResponse read get_Completed write put_Completed;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface(IUnknown)
  ['{F4637D4A-0760-5431-BFC0-24EB1D4F6C4F}']
    procedure Invoke(sender: IMemoryBufferReference; args: IInspectable); safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<UInt32>
  IIterator_1__Cardinal = interface(IInspectable)
  ['{F06A2739-9443-5EF0-B284-DC5AFF3E7D10}']
    function get_Current: Cardinal; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCardinal): Cardinal; safecall;
    property Current: Cardinal read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<UInt32>
  IIterable_1__Cardinal = interface(IInspectable)
  ['{421D4B91-B13B-5F37-AE54-B5249BD80539}']
    function First: IIterator_1__Cardinal; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<UInt32>
  IVectorView_1__Cardinal = interface(IInspectable)
  ['{E5CE1A07-8D33-5007-BA64-7D2508CCF85C}']
    function GetAt(index: Cardinal): Cardinal; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Cardinal; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCardinal): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<UInt32>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal_Delegate_Base = interface(IUnknown)
  ['{55772F29-DA64-5C87-871C-074337A84573}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<UInt32>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal_Delegate_Base)
  ['{55772F29-DA64-5C87-871C-074337A84573}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<UInt32>>
  IAsyncOperation_1__IVectorView_1__Cardinal_Base = interface(IInspectable)
  ['{52C56F3C-713A-5162-9E62-362CE7ED53BE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal; safecall;
    function GetResults: IVectorView_1__Cardinal; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Cardinal read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<UInt32>>
  IAsyncOperation_1__IVectorView_1__Cardinal = interface(IAsyncOperation_1__IVectorView_1__Cardinal_Base)
  ['{52C56F3C-713A-5162-9E62-362CE7ED53BE}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.ISuspendingEventArgs>
  EventHandler_1__ISuspendingEventArgs_Delegate_Base = interface(IUnknown)
  ['{338579BF-1A35-5CC4-A622-A6F384FD892C}']
    procedure Invoke(sender: IInspectable; args: ISuspendingEventArgs); safecall;
  end;
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.ISuspendingEventArgs>
  EventHandler_1__ISuspendingEventArgs = interface(EventHandler_1__ISuspendingEventArgs_Delegate_Base)
  ['{1B90A705-3216-5FC2-BDDF-9CFC59042477}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.Activation.IBackgroundActivatedEventArgs>
  EventHandler_1__Activation_IBackgroundActivatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{49A07732-E7B8-5C5B-9DE7-22E33CB97004}']
    procedure Invoke(sender: IInspectable; args: Activation_IBackgroundActivatedEventArgs); safecall;
  end;
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.Activation.IBackgroundActivatedEventArgs>
  EventHandler_1__Activation_IBackgroundActivatedEventArgs = interface(EventHandler_1__Activation_IBackgroundActivatedEventArgs_Delegate_Base)
  ['{3D7805F5-B26F-52AA-83DF-B266FCFD9C5D}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.ILeavingBackgroundEventArgs>
  EventHandler_1__ILeavingBackgroundEventArgs_Delegate_Base = interface(IUnknown)
  ['{9B6171C2-ABB2-5194-AFC0-CEF167C424EB}']
    procedure Invoke(sender: IInspectable; args: ILeavingBackgroundEventArgs); safecall;
  end;
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.ILeavingBackgroundEventArgs>
  EventHandler_1__ILeavingBackgroundEventArgs = interface(EventHandler_1__ILeavingBackgroundEventArgs_Delegate_Base)
  ['{278D6311-A655-5533-9B0F-384C57BCF939}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.IEnteredBackgroundEventArgs>
  EventHandler_1__IEnteredBackgroundEventArgs_Delegate_Base = interface(IUnknown)
  ['{E0739C32-FC14-5361-A8B3-0809699FBCBD}']
    procedure Invoke(sender: IInspectable; args: IEnteredBackgroundEventArgs); safecall;
  end;
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.IEnteredBackgroundEventArgs>
  EventHandler_1__IEnteredBackgroundEventArgs = interface(EventHandler_1__IEnteredBackgroundEventArgs_Delegate_Base)
  ['{57FA658D-7CE6-5020-87BD-1A0D1887E41A}']
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueueTimer,Object>
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable = interface(IUnknown)
  ['{8A13AE56-7643-5F25-A347-5C9F548273DC}']
    procedure Invoke(sender: IDispatcherQueueTimer; args: IInspectable); safecall;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Object>
  TypedEventHandler_2__IDispatcherQueue__IInspectable = interface(IUnknown)
  ['{1ECC7D76-D5F1-5514-8DA3-343E7A82F842}']
    procedure Invoke(sender: IDispatcherQueue; args: IInspectable); safecall;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Object>
  TypedEventHandler_2__IDataPackage__IInspectable = interface(IUnknown)
  ['{FFA86A6A-1BEE-540E-9911-7272C487A1ED}']
    procedure Invoke(sender: IDataPackage; args: IInspectable); safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IIterator_1__Email_EmailCertificateValidationStatus_Base = interface(IInspectable)
  ['{1CFE3D41-16A5-5026-A6FE-2CB0A303A605}']
    function get_Current: Email_EmailCertificateValidationStatus; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PEmail_EmailCertificateValidationStatus): Cardinal; safecall;
    property Current: Email_EmailCertificateValidationStatus read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IIterator_1__Email_EmailCertificateValidationStatus = interface(IIterator_1__Email_EmailCertificateValidationStatus_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IIterable_1__Email_EmailCertificateValidationStatus_Base = interface(IInspectable)
  ['{7E326530-7449-51A7-B1BC-C43533A78E06}']
    function First: IIterator_1__Email_EmailCertificateValidationStatus; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IIterable_1__Email_EmailCertificateValidationStatus = interface(IIterable_1__Email_EmailCertificateValidationStatus_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.Email.EmailMailboxSmimeEncryptionAlgorithm>
  IReference_1__Email_EmailMailboxSmimeEncryptionAlgorithm = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Email_EmailMailboxSmimeEncryptionAlgorithm; safecall;
    property Value: Email_EmailMailboxSmimeEncryptionAlgorithm read get_Value;
  end;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.Email.EmailMailboxSmimeSigningAlgorithm>
  IReference_1__Email_EmailMailboxSmimeSigningAlgorithm = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Email_EmailMailboxSmimeSigningAlgorithm; safecall;
    property Value: Email_EmailMailboxSmimeSigningAlgorithm read get_Value;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>
  IVectorView_1__Email_EmailCertificateValidationStatus = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Email_EmailCertificateValidationStatus; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Email_EmailCertificateValidationStatus; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PEmail_EmailCertificateValidationStatus): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus_Delegate_Base = interface(IUnknown)
  ['{7DB1B498-0944-5B7F-8653-45D0D35DDFF1}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Email_EmailCertificateValidationStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus_Delegate_Base)
  ['{5A9F94D2-879A-565B-91C7-6CD2248BA109}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>>
  IAsyncOperation_1__IVectorView_1__Email_EmailCertificateValidationStatus_Base = interface(IInspectable)
  ['{F3E7EF5B-8894-5488-97EF-029CA4913947}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus; safecall;
    function GetResults: IVectorView_1__Email_EmailCertificateValidationStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Email_EmailCertificateValidationStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Email.EmailCertificateValidationStatus>>
  IAsyncOperation_1__IVectorView_1__Email_EmailCertificateValidationStatus = interface(IAsyncOperation_1__IVectorView_1__Email_EmailCertificateValidationStatus_Base)
  ['{DF78340B-B47F-50E2-9EC5-530CB35D7BE7}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Email.EmailMailboxEmptyFolderStatus>
  AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus_Delegate_Base = interface(IUnknown)
  ['{50333005-DAED-567E-BB88-B1BC4663B075}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Email_EmailMailboxEmptyFolderStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Email.EmailMailboxEmptyFolderStatus>
  AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus = interface(AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Email.EmailMailboxEmptyFolderStatus>
  IAsyncOperation_1__Email_EmailMailboxEmptyFolderStatus_Base = interface(IInspectable)
  ['{0033A74E-9BAA-5F50-8D6E-238635E6FDD3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus; safecall;
    function GetResults: Email_EmailMailboxEmptyFolderStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Email_EmailMailboxEmptyFolderStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Email.EmailMailboxEmptyFolderStatus>
  IAsyncOperation_1__Email_EmailMailboxEmptyFolderStatus = interface(IAsyncOperation_1__Email_EmailMailboxEmptyFolderStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Email.EmailMailboxDeleteFolderStatus>
  AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus_Delegate_Base = interface(IUnknown)
  ['{52604DA6-485B-5445-8E6F-64CC13056045}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Email_EmailMailboxDeleteFolderStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Email.EmailMailboxDeleteFolderStatus>
  AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus = interface(AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Email.EmailMailboxDeleteFolderStatus>
  IAsyncOperation_1__Email_EmailMailboxDeleteFolderStatus_Base = interface(IInspectable)
  ['{6860F87F-0297-5ADF-AA16-9F1F08E2D950}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus; safecall;
    function GetResults: Email_EmailMailboxDeleteFolderStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Email_EmailMailboxDeleteFolderStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Email.EmailMailboxDeleteFolderStatus>
  IAsyncOperation_1__Email_EmailMailboxDeleteFolderStatus = interface(IAsyncOperation_1__Email_EmailMailboxDeleteFolderStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundResult>
  AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult_Delegate_Base = interface(IUnknown)
  ['{07E1DC01-18BA-596A-B745-79F9CDE44CCC}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundResult>
  AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult = interface(AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundResult>
  IAsyncOperation_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult_Base = interface(IInspectable)
  ['{B18EA00F-8C20-5AC2-9246-3EF405271F1A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult; safecall;
    function GetResults: ExtendedExecution_Foreground_ExtendedExecutionForegroundResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ExtendedExecution.Foreground.ExtendedExecutionForegroundResult>
  IAsyncOperation_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult = interface(IAsyncOperation_1__ExtendedExecution_Foreground_ExtendedExecutionForegroundResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionResult>
  AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult_Delegate_Base = interface(IUnknown)
  ['{873C5C7A-C638-5A33-9B03-215C72471663}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ExtendedExecution_ExtendedExecutionResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionResult>
  AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult = interface(AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionResult>
  IAsyncOperation_1__ExtendedExecution_ExtendedExecutionResult_Base = interface(IInspectable)
  ['{1DBB1BC9-6CD7-5947-8CD1-29632B5AA950}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult; safecall;
    function GetResults: ExtendedExecution_ExtendedExecutionResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ExtendedExecution_ExtendedExecutionResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.ExtendedExecution.ExtendedExecutionResult>
  IAsyncOperation_1__ExtendedExecution_ExtendedExecutionResult = interface(IAsyncOperation_1__ExtendedExecution_ExtendedExecutionResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IAppInfo2
  IAppInfo2 = interface(IInspectable)
  ['{BE4B1F5A-2098-431B-BD25-B30878748D47}']
    function get_Package: IPackage; safecall;
    property Package: IPackage read get_Package;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IAppInfoStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_AppInfo)]
  IAppInfoStatics = interface(IInspectable)
  ['{CF1F782A-E48B-4F0C-9B0B-79C3F8957DD7}']
    function get_Current: IAppInfo; safecall;
    function GetFromAppUserModelId(appUserModelId: HSTRING): IAppInfo; safecall;
    function GetFromAppUserModelIdForUser(user: IUser; appUserModelId: HSTRING): IAppInfo; safecall;
    property Current: IAppInfo read get_Current;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.IAppInstallerInfo
  IAppInstallerInfo = interface(IInspectable)
  ['{29AB2AC0-D4F6-42A3-ADCD-D6583C659508}']
    function get_Uri: IUriRuntimeClass; safecall;
    property Uri: IUriRuntimeClass read get_Uri;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackage2
  IPackage2 = interface(IInspectable)
  ['{A6612FB6-7688-4ACE-95FB-359538E7AA01}']
    function get_DisplayName: HSTRING; safecall;
    function get_PublisherDisplayName: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_Logo: IUriRuntimeClass; safecall;
    function get_IsResourcePackage: Boolean; safecall;
    function get_IsBundle: Boolean; safecall;
    function get_IsDevelopmentMode: Boolean; safecall;
    property Description: HSTRING read get_Description;
    property DisplayName: HSTRING read get_DisplayName;
    property IsBundle: Boolean read get_IsBundle;
    property IsDevelopmentMode: Boolean read get_IsDevelopmentMode;
    property IsResourcePackage: Boolean read get_IsResourcePackage;
    property Logo: IUriRuntimeClass read get_Logo;
    property PublisherDisplayName: HSTRING read get_PublisherDisplayName;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.IPackageStatus
  IPackageStatus = interface(IInspectable)
  ['{5FE74F71-A365-4C09-A02D-046D525EA1DA}']
    function VerifyIsOK: Boolean; safecall;
    function get_NotAvailable: Boolean; safecall;
    function get_PackageOffline: Boolean; safecall;
    function get_DataOffline: Boolean; safecall;
    function get_Disabled: Boolean; safecall;
    function get_NeedsRemediation: Boolean; safecall;
    function get_LicenseIssue: Boolean; safecall;
    function get_Modified: Boolean; safecall;
    function get_Tampered: Boolean; safecall;
    function get_DependencyIssue: Boolean; safecall;
    function get_Servicing: Boolean; safecall;
    function get_DeploymentInProgress: Boolean; safecall;
    property DataOffline: Boolean read get_DataOffline;
    property DependencyIssue: Boolean read get_DependencyIssue;
    property DeploymentInProgress: Boolean read get_DeploymentInProgress;
    property Disabled: Boolean read get_Disabled;
    property LicenseIssue: Boolean read get_LicenseIssue;
    property Modified: Boolean read get_Modified;
    property NeedsRemediation: Boolean read get_NeedsRemediation;
    property NotAvailable: Boolean read get_NotAvailable;
    property PackageOffline: Boolean read get_PackageOffline;
    property Servicing: Boolean read get_Servicing;
    property Tampered: Boolean read get_Tampered;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackage3
  IPackage3 = interface(IInspectable)
  ['{5F738B61-F86A-4917-93D1-F1EE9D3B35D9}']
    function get_Status: IPackageStatus; safecall;
    function get_InstalledDate: DateTime; safecall;
    function GetAppListEntriesAsync: IAsyncOperation_1__IVectorView_1__IAppListEntry; safecall;
    property InstalledDate: DateTime read get_InstalledDate;
    property Status: IPackageStatus read get_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackage4
  IPackage4 = interface(IInspectable)
  ['{65AED1AE-B95B-450C-882B-6255187F397E}']
    function get_SignatureKind: PackageSignatureKind; safecall;
    function get_IsOptional: Boolean; safecall;
    function VerifyContentIntegrityAsync: IAsyncOperation_1__Boolean; safecall;
    property IsOptional: Boolean read get_IsOptional;
    property SignatureKind: PackageSignatureKind read get_SignatureKind;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.IPackageContentGroup
  IPackageContentGroup = interface(IInspectable)
  ['{8F62695D-120A-4798-B5E1-5800DDA8F2E1}']
    function get_Package: IPackage; safecall;
    function get_Name: HSTRING; safecall;
    function get_State: PackageContentGroupState; safecall;
    function get_IsRequired: Boolean; safecall;
    property IsRequired: Boolean read get_IsRequired;
    property Name: HSTRING read get_Name;
    property Package: IPackage read get_Package;
    property State: PackageContentGroupState read get_State;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IPackageContentGroup>
  IIterator_1__IPackageContentGroup_Base = interface(IInspectable)
  ['{5F23D323-28F5-560F-A40E-6F3827F82E9F}']
    function get_Current: IPackageContentGroup; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIPackageContentGroup): Cardinal; safecall;
    property Current: IPackageContentGroup read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.IPackageContentGroup>
  IIterator_1__IPackageContentGroup = interface(IIterator_1__IPackageContentGroup_Base)
  ['{FBD3B150-A8CF-5061-8186-E72C2EB0569A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IPackageContentGroup>
  IIterable_1__IPackageContentGroup_Base = interface(IInspectable)
  ['{D7DD1456-4805-5768-A25D-99641B096491}']
    function First: IIterator_1__IPackageContentGroup; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.IPackageContentGroup>
  IIterable_1__IPackageContentGroup = interface(IIterable_1__IPackageContentGroup_Base)
  ['{DCF0DE3A-AFC2-5CB4-82D5-083EB242E820}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IPackageContentGroup>
  IVectorView_1__IPackageContentGroup = interface(IInspectable)
  ['{B80F5A11-40E9-5694-998F-F8F16BBD6E78}']
    function GetAt(index: Cardinal): IPackageContentGroup; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IPackageContentGroup; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPackageContentGroup): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>
  IVector_1__IPackageContentGroup_Base = interface(IInspectable)
  ['{29ADC699-5848-5A98-A516-23FEB0FA2C4B}']
    function GetAt(index: Cardinal): IPackageContentGroup; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IPackageContentGroup; safecall;
    function IndexOf(value: IPackageContentGroup; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IPackageContentGroup); safecall;
    procedure InsertAt(index: Cardinal; value: IPackageContentGroup); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IPackageContentGroup); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPackageContentGroup): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIPackageContentGroup); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>
  IVector_1__IPackageContentGroup = interface(IVector_1__IPackageContentGroup_Base)
  ['{97EF46B2-B1E4-5150-A8BB-BB4FE6A45B7A}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>>
  AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup_Delegate_Base = interface(IUnknown)
  ['{52465BF3-3CA6-5681-A7B4-91847757B5FD}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVector_1__IPackageContentGroup; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>>
  AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup = interface(AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup_Delegate_Base)
  ['{9DF4403A-879F-5481-BDB6-712E26321F23}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>>
  IAsyncOperation_1__IVector_1__IPackageContentGroup_Base = interface(IInspectable)
  ['{929E3C29-BF29-5594-BC63-67DB43A539EA}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup; safecall;
    function GetResults: IVector_1__IPackageContentGroup; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVector_1__IPackageContentGroup read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackageContentGroup>>
  IAsyncOperation_1__IVector_1__IPackageContentGroup = interface(IAsyncOperation_1__IVector_1__IPackageContentGroup_Base)
  ['{48E9F438-96F7-54A4-9239-DCE346EE2580}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.IPackageContentGroup>
  AsyncOperationCompletedHandler_1__IPackageContentGroup_Delegate_Base = interface(IUnknown)
  ['{2253DC38-9A1A-5364-9A3B-03A7DA615499}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IPackageContentGroup; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.IPackageContentGroup>
  AsyncOperationCompletedHandler_1__IPackageContentGroup = interface(AsyncOperationCompletedHandler_1__IPackageContentGroup_Delegate_Base)
  ['{B22BEECF-1BFC-5D65-9148-9B49796B0511}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.IPackageContentGroup>
  IAsyncOperation_1__IPackageContentGroup_Base = interface(IInspectable)
  ['{BBD292E3-DB9F-5802-A488-40F156332C04}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IPackageContentGroup); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IPackageContentGroup; safecall;
    function GetResults: IPackageContentGroup; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IPackageContentGroup read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.IPackageContentGroup>
  IAsyncOperation_1__IPackageContentGroup = interface(IAsyncOperation_1__IPackageContentGroup_Base)
  ['{422F6877-7C41-5240-A94C-B1A9E7C1DD19}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackage5
  IPackage5 = interface(IInspectable)
  ['{0E842DD4-D9AC-45ED-9A1E-74CE056B2635}']
    function GetContentGroupsAsync: IAsyncOperation_1__IVector_1__IPackageContentGroup; safecall;
    function GetContentGroupAsync(name: HSTRING): IAsyncOperation_1__IPackageContentGroup; safecall;
    function StageContentGroupsAsync(names: IIterable_1__HSTRING): IAsyncOperation_1__IVector_1__IPackageContentGroup; overload; safecall;
    function StageContentGroupsAsync(names: IIterable_1__HSTRING; moveToHeadOfQueue: Boolean): IAsyncOperation_1__IVector_1__IPackageContentGroup; overload; safecall;
    function SetInUseAsync(inUse: Boolean): IAsyncOperation_1__Boolean; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.IPackageUpdateAvailabilityResult
  IPackageUpdateAvailabilityResult = interface(IInspectable)
  ['{114E5009-199A-48A1-A079-313C45634A71}']
    function get_Availability: PackageUpdateAvailability; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property Availability: PackageUpdateAvailability read get_Availability;
    property ExtendedError: HRESULT read get_ExtendedError;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.IPackageUpdateAvailabilityResult>
  AsyncOperationCompletedHandler_1__IPackageUpdateAvailabilityResult = interface(IUnknown)
  ['{4AE0D402-6563-5F18-9A87-D9703D9712D3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IPackageUpdateAvailabilityResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.IPackageUpdateAvailabilityResult>
  IAsyncOperation_1__IPackageUpdateAvailabilityResult = interface(IInspectable)
  ['{E4A3BC60-38D8-5FA3-9F29-A3AACBCA0543}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IPackageUpdateAvailabilityResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IPackageUpdateAvailabilityResult; safecall;
    function GetResults: IPackageUpdateAvailabilityResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IPackageUpdateAvailabilityResult read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackage6
  IPackage6 = interface(IInspectable)
  ['{8B1AD942-12D7-4754-AE4E-638CBC0E3A2E}']
    function GetAppInstallerInfo: IAppInstallerInfo; safecall;
    function CheckUpdateAvailabilityAsync: IAsyncOperation_1__IPackageUpdateAvailabilityResult; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackage7
  IPackage7 = interface(IInspectable)
  ['{86FF8D31-A2E4-45E0-9732-283A6D88FDE1}']
    function get_MutableLocation: IStorageFolder; safecall;
    function get_EffectiveLocation: IStorageFolder; safecall;
    property EffectiveLocation: IStorageFolder read get_EffectiveLocation;
    property MutableLocation: IStorageFolder read get_MutableLocation;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackage8
  IPackage8 = interface(IInspectable)
  ['{2C584F7B-CE2A-4BE6-A093-77CFBB2A7EA1}']
    function get_EffectiveExternalLocation: IStorageFolder; safecall;
    function get_MachineExternalLocation: IStorageFolder; safecall;
    function get_UserExternalLocation: IStorageFolder; safecall;
    function get_InstalledPath: HSTRING; safecall;
    function get_MutablePath: HSTRING; safecall;
    function get_EffectivePath: HSTRING; safecall;
    function get_EffectiveExternalPath: HSTRING; safecall;
    function get_MachineExternalPath: HSTRING; safecall;
    function get_UserExternalPath: HSTRING; safecall;
    function GetLogoAsRandomAccessStreamReference(size: TSizeF): IRandomAccessStreamReference; safecall;
    function GetAppListEntries: IVectorView_1__IAppListEntry; safecall;
    function get_IsStub: Boolean; safecall;
    property EffectiveExternalLocation: IStorageFolder read get_EffectiveExternalLocation;
    property EffectiveExternalPath: HSTRING read get_EffectiveExternalPath;
    property EffectivePath: HSTRING read get_EffectivePath;
    property InstalledPath: HSTRING read get_InstalledPath;
    property IsStub: Boolean read get_IsStub;
    property MachineExternalLocation: IStorageFolder read get_MachineExternalLocation;
    property MachineExternalPath: HSTRING read get_MachineExternalPath;
    property MutablePath: HSTRING read get_MutablePath;
    property UserExternalLocation: IStorageFolder read get_UserExternalLocation;
    property UserExternalPath: HSTRING read get_UserExternalPath;
  end;

  // Windows.ApplicationModel.IPackageStagingEventArgs
  IPackageStagingEventArgs = interface(IInspectable)
  ['{1041682D-54E2-4F51-B828-9EF7046C210F}']
    function get_ActivityId: TGuid; safecall;
    function get_Package: IPackage; safecall;
    function get_Progress: Double; safecall;
    function get_IsComplete: Boolean; safecall;
    function get_ErrorCode: HRESULT; safecall;
    property ActivityId: TGuid read get_ActivityId;
    property ErrorCode: HRESULT read get_ErrorCode;
    property IsComplete: Boolean read get_IsComplete;
    property Package: IPackage read get_Package;
    property Progress: Double read get_Progress;
  end;

  // Windows.ApplicationModel.IPackageInstallingEventArgs
  IPackageInstallingEventArgs = interface(IInspectable)
  ['{97741EB7-AB7A-401A-8B61-EB0E7FAFF237}']
    function get_ActivityId: TGuid; safecall;
    function get_Package: IPackage; safecall;
    function get_Progress: Double; safecall;
    function get_IsComplete: Boolean; safecall;
    function get_ErrorCode: HRESULT; safecall;
    property ActivityId: TGuid read get_ActivityId;
    property ErrorCode: HRESULT read get_ErrorCode;
    property IsComplete: Boolean read get_IsComplete;
    property Package: IPackage read get_Package;
    property Progress: Double read get_Progress;
  end;

  // Windows.ApplicationModel.IPackageUpdatingEventArgs
  IPackageUpdatingEventArgs = interface(IInspectable)
  ['{CD7B4228-FD74-443E-B114-23E677B0E86F}']
    function get_ActivityId: TGuid; safecall;
    function get_SourcePackage: IPackage; safecall;
    function get_TargetPackage: IPackage; safecall;
    function get_Progress: Double; safecall;
    function get_IsComplete: Boolean; safecall;
    function get_ErrorCode: HRESULT; safecall;
    property ActivityId: TGuid read get_ActivityId;
    property ErrorCode: HRESULT read get_ErrorCode;
    property IsComplete: Boolean read get_IsComplete;
    property Progress: Double read get_Progress;
    property SourcePackage: IPackage read get_SourcePackage;
    property TargetPackage: IPackage read get_TargetPackage;
  end;

  // Windows.ApplicationModel.IPackageUninstallingEventArgs
  IPackageUninstallingEventArgs = interface(IInspectable)
  ['{4443AA52-AB22-44CD-82BB-4EC9B827367A}']
    function get_ActivityId: TGuid; safecall;
    function get_Package: IPackage; safecall;
    function get_Progress: Double; safecall;
    function get_IsComplete: Boolean; safecall;
    function get_ErrorCode: HRESULT; safecall;
    property ActivityId: TGuid read get_ActivityId;
    property ErrorCode: HRESULT read get_ErrorCode;
    property IsComplete: Boolean read get_IsComplete;
    property Package: IPackage read get_Package;
    property Progress: Double read get_Progress;
  end;

  // Windows.ApplicationModel.IPackageStatusChangedEventArgs
  IPackageStatusChangedEventArgs = interface(IInspectable)
  ['{437D714D-BD80-4A70-BC50-F6E796509575}']
    function get_Package: IPackage; safecall;
    property Package: IPackage read get_Package;
  end;

  // Windows.ApplicationModel.IPackageContentGroupStagingEventArgs
  IPackageContentGroupStagingEventArgs = interface(IInspectable)
  ['{3D7BC27E-6F27-446C-986E-D4733D4D9113}']
    function get_ActivityId: TGuid; safecall;
    function get_Package: IPackage; safecall;
    function get_Progress: Double; safecall;
    function get_IsComplete: Boolean; safecall;
    function get_ErrorCode: HRESULT; safecall;
    function get_ContentGroupName: HSTRING; safecall;
    function get_IsContentGroupRequired: Boolean; safecall;
    property ActivityId: TGuid read get_ActivityId;
    property ContentGroupName: HSTRING read get_ContentGroupName;
    property ErrorCode: HRESULT read get_ErrorCode;
    property IsComplete: Boolean read get_IsComplete;
    property IsContentGroupRequired: Boolean read get_IsContentGroupRequired;
    property Package: IPackage read get_Package;
    property Progress: Double read get_Progress;
  end;

  // Windows.ApplicationModel.IPackageCatalogRemoveResourcePackagesResult
  IPackageCatalogRemoveResourcePackagesResult = interface(IInspectable)
  ['{AE719709-1A52-4321-87B3-E5A1A17981A7}']
    function get_PackagesRemoved: IVectorView_1__IPackage; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property PackagesRemoved: IVectorView_1__IPackage read get_PackagesRemoved;
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.IPackageCatalogRemoveResourcePackagesResult>
  AsyncOperationCompletedHandler_1__IPackageCatalogRemoveResourcePackagesResult = interface(IUnknown)
  ['{B3EFAED5-9EBA-5C3E-8E32-FC85B2E6812C}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IPackageCatalogRemoveResourcePackagesResult; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.IPackageCatalogRemoveResourcePackagesResult>
  IAsyncOperation_1__IPackageCatalogRemoveResourcePackagesResult = interface(IInspectable)
  ['{8D09D8B5-FA3A-5A07-8B73-F987025E36DF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IPackageCatalogRemoveResourcePackagesResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IPackageCatalogRemoveResourcePackagesResult; safecall;
    function GetResults: IPackageCatalogRemoveResourcePackagesResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IPackageCatalogRemoveResourcePackagesResult read get_Completed write put_Completed;
  end;

  // Windows.ApplicationModel.IPackageIdWithMetadata
  IPackageIdWithMetadata = interface(IInspectable)
  ['{40577A7C-0C9E-443D-9074-855F5CE0A08D}']
    function get_ProductId: HSTRING; safecall;
    function get_Author: HSTRING; safecall;
    property Author: HSTRING read get_Author;
    property ProductId: HSTRING read get_ProductId;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackageStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_PackageRT)]
  IPackageStatics = interface(IInspectable)
  ['{4E534BDF-2960-4878-97A4-9624DEB72F2D}']
    function get_Current: IPackage; safecall;
    property Current: IPackage read get_Current;
  end;

  // Windows.ApplicationModel.IPackageStatus2
  IPackageStatus2 = interface(IInspectable)
  ['{F428FA93-7C56-4862-ACFA-ABAEDCC0694D}']
    function get_IsPartiallyStaged: Boolean; safecall;
    property IsPartiallyStaged: Boolean read get_IsPartiallyStaged;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.IPackageWithMetadata
  IPackageWithMetadata = interface(IInspectable)
  ['{95949780-1DE9-40F2-B452-0DE9F1910012}']
    function get_InstallDate: DateTime; safecall;
    function GetThumbnailToken: HSTRING; safecall;
    procedure Launch(parameters: HSTRING); safecall;
    property InstallDate: DateTime read get_InstallDate;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.StartupTaskState>
  AsyncOperationCompletedHandler_1__StartupTaskState_Delegate_Base = interface(IUnknown)
  ['{70A0BF67-19E8-5A86-A32E-3C9863825A04}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__StartupTaskState; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.StartupTaskState>
  AsyncOperationCompletedHandler_1__StartupTaskState = interface(AsyncOperationCompletedHandler_1__StartupTaskState_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.StartupTaskState>
  IAsyncOperation_1__StartupTaskState_Base = interface(IInspectable)
  ['{5239A934-80E2-518F-B819-1F316F379A3F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__StartupTaskState); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__StartupTaskState; safecall;
    function GetResults: StartupTaskState; safecall;
    property Completed: AsyncOperationCompletedHandler_1__StartupTaskState read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.StartupTaskState>
  IAsyncOperation_1__StartupTaskState = interface(IAsyncOperation_1__StartupTaskState_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.LockScreen.ILockScreenUnlockingDeferral
  LockScreen_ILockScreenUnlockingDeferral = interface(IInspectable)
  ['{7E7D1AD6-5203-43E7-9BD6-7C3947D1E3FE}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.LockScreen.ILockScreenUnlockingEventArgs
  LockScreen_ILockScreenUnlockingEventArgs = interface(IInspectable)
  ['{44E6C007-75FB-4ABB-9F8B-824748900C71}']
    function GetDeferral: LockScreen_ILockScreenUnlockingDeferral; safecall;
    function get_Deadline: DateTime; safecall;
    property Deadline: DateTime read get_Deadline;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.LockScreen.ILockApplicationHost,Windows.ApplicationModel.LockScreen.ILockScreenUnlockingEventArgs>
  TypedEventHandler_2__LockScreen_ILockApplicationHost__LockScreen_ILockScreenUnlockingEventArgs_Delegate_Base = interface(IUnknown)
  ['{002E5776-8A5B-5B93-8C6C-9C4C8788F5B4}']
    procedure Invoke(sender: LockScreen_ILockApplicationHost; args: LockScreen_ILockScreenUnlockingEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.LockScreen.ILockApplicationHost,Windows.ApplicationModel.LockScreen.ILockScreenUnlockingEventArgs>
  TypedEventHandler_2__LockScreen_ILockApplicationHost__LockScreen_ILockScreenUnlockingEventArgs = interface(TypedEventHandler_2__LockScreen_ILockApplicationHost__LockScreen_ILockScreenUnlockingEventArgs_Delegate_Base)
  ['{08F45A4A-0029-5309-A6E5-B4554E404F8F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.LockScreen.ILockApplicationHost
  [WinRTClassNameAttribute(SWindows_ApplicationModel_LockScreen_LockApplicationHost)]
  LockScreen_ILockApplicationHost = interface(IInspectable)
  ['{38EE31AD-D94F-4E7C-81FA-4F4436506281}']
    procedure RequestUnlock; safecall;
    function add_Unlocking(handler: TypedEventHandler_2__LockScreen_ILockApplicationHost__LockScreen_ILockScreenUnlockingEventArgs): EventRegistrationToken; safecall;
    procedure remove_Unlocking(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.LockScreen.ILockApplicationHostStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_LockScreen_LockApplicationHost)]
  LockScreen_ILockApplicationHostStatics = interface(IInspectable)
  ['{F48FAB8E-23D7-4E63-96A1-666FF52D3B2C}']
    function GetForCurrentView: LockScreen_ILockApplicationHost; safecall;
  end;

  // Windows.ApplicationModel.LockScreen.ILockScreenBadge
  LockScreen_ILockScreenBadge = interface(IInspectable)
  ['{E95105D9-2BFF-4DB0-9B4F-3824778B9C9A}']
    function get_Logo: IRandomAccessStream; safecall;
    function get_Glyph: IRandomAccessStream; safecall;
    function get_Number: IReference_1__Cardinal; safecall;
    function get_AutomationName: HSTRING; safecall;
    procedure LaunchApp; safecall;
    property AutomationName: HSTRING read get_AutomationName;
    property Glyph: IRandomAccessStream read get_Glyph;
    property Logo: IRandomAccessStream read get_Logo;
    property Number: IReference_1__Cardinal read get_Number;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.LockScreen.ILockScreenInfo,Object>
  TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable_Delegate_Base = interface(IUnknown)
  ['{8BE9E6A3-F88A-5429-8DA3-676B7D4F1A5B}']
    procedure Invoke(sender: LockScreen_ILockScreenInfo; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.LockScreen.ILockScreenInfo,Object>
  TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable = interface(TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable_Delegate_Base)
  ['{A7F4A3EF-0F52-51A1-B9B9-05A40776EA01}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IIterator_1__LockScreen_ILockScreenBadge_Base = interface(IInspectable)
  ['{8D38F924-154D-5705-8F0B-ED61353F6CE2}']
    function get_Current: LockScreen_ILockScreenBadge; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PLockScreen_ILockScreenBadge): Cardinal; safecall;
    property Current: LockScreen_ILockScreenBadge read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IIterator_1__LockScreen_ILockScreenBadge = interface(IIterator_1__LockScreen_ILockScreenBadge_Base)
  ['{088B8AF0-7137-5D3F-ACD7-2294E05A4237}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IIterable_1__LockScreen_ILockScreenBadge_Base = interface(IInspectable)
  ['{6E82DEDC-B74E-503A-B00B-9C6F47F12A0F}']
    function First: IIterator_1__LockScreen_ILockScreenBadge; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IIterable_1__LockScreen_ILockScreenBadge = interface(IIterable_1__LockScreen_ILockScreenBadge_Base)
  ['{3BDF34B0-B187-5268-990D-9D6BF3AE507A}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.LockScreen.ILockScreenBadge>
  IVectorView_1__LockScreen_ILockScreenBadge = interface(IInspectable)
  ['{AEFD5B55-DB8D-5E08-93F9-8AD48C511F09}']
    function GetAt(index: Cardinal): LockScreen_ILockScreenBadge; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: LockScreen_ILockScreenBadge; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PLockScreen_ILockScreenBadge): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.ApplicationModel.LockScreen.ILockScreenInfo
  LockScreen_ILockScreenInfo = interface(IInspectable)
  ['{F59AA65C-9711-4DC9-A630-95B6CB8CDAD0}']
    function add_LockScreenImageChanged(handler: TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable): EventRegistrationToken; safecall;
    procedure remove_LockScreenImageChanged(token: EventRegistrationToken); safecall;
    function get_LockScreenImage: IRandomAccessStream; safecall;
    function add_BadgesChanged(handler: TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable): EventRegistrationToken; safecall;
    procedure remove_BadgesChanged(token: EventRegistrationToken); safecall;
    function get_Badges: IVectorView_1__LockScreen_ILockScreenBadge; safecall;
    function add_DetailTextChanged(handler: TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable): EventRegistrationToken; safecall;
    procedure remove_DetailTextChanged(token: EventRegistrationToken); safecall;
    function get_DetailText: IVectorView_1__HSTRING; safecall;
    function add_AlarmIconChanged(handler: TypedEventHandler_2__LockScreen_ILockScreenInfo__IInspectable): EventRegistrationToken; safecall;
    procedure remove_AlarmIconChanged(token: EventRegistrationToken); safecall;
    function get_AlarmIcon: IRandomAccessStream; safecall;
    property AlarmIcon: IRandomAccessStream read get_AlarmIcon;
    property Badges: IVectorView_1__LockScreen_ILockScreenBadge read get_Badges;
    property DetailText: IVectorView_1__HSTRING read get_DetailText;
    property LockScreenImage: IRandomAccessStream read get_LockScreenImage;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentAddress
  Payments_IPaymentAddress = interface(IInspectable)
  ['{5F2264E9-6F3A-4166-A018-0A0B06BB32B5}']
    function get_Country: HSTRING; safecall;
    procedure put_Country(value: HSTRING); safecall;
    function get_AddressLines: IVectorView_1__HSTRING; safecall;
    procedure put_AddressLines(value: IVectorView_1__HSTRING); safecall;
    function get_Region: HSTRING; safecall;
    procedure put_Region(value: HSTRING); safecall;
    function get_City: HSTRING; safecall;
    procedure put_City(value: HSTRING); safecall;
    function get_DependentLocality: HSTRING; safecall;
    procedure put_DependentLocality(value: HSTRING); safecall;
    function get_PostalCode: HSTRING; safecall;
    procedure put_PostalCode(value: HSTRING); safecall;
    function get_SortingCode: HSTRING; safecall;
    procedure put_SortingCode(value: HSTRING); safecall;
    function get_LanguageCode: HSTRING; safecall;
    procedure put_LanguageCode(value: HSTRING); safecall;
    function get_Organization: HSTRING; safecall;
    procedure put_Organization(value: HSTRING); safecall;
    function get_Recipient: HSTRING; safecall;
    procedure put_Recipient(value: HSTRING); safecall;
    function get_PhoneNumber: HSTRING; safecall;
    procedure put_PhoneNumber(value: HSTRING); safecall;
    function get_Properties: IPropertySet; safecall;
    property AddressLines: IVectorView_1__HSTRING read get_AddressLines write put_AddressLines;
    property City: HSTRING read get_City write put_City;
    property Country: HSTRING read get_Country write put_Country;
    property DependentLocality: HSTRING read get_DependentLocality write put_DependentLocality;
    property LanguageCode: HSTRING read get_LanguageCode write put_LanguageCode;
    property Organization: HSTRING read get_Organization write put_Organization;
    property PhoneNumber: HSTRING read get_PhoneNumber write put_PhoneNumber;
    property PostalCode: HSTRING read get_PostalCode write put_PostalCode;
    property Properties: IPropertySet read get_Properties;
    property Recipient: HSTRING read get_Recipient write put_Recipient;
    property Region: HSTRING read get_Region write put_Region;
    property SortingCode: HSTRING read get_SortingCode write put_SortingCode;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult
  Payments_IPaymentCanMakePaymentResult = interface(IInspectable)
  ['{7696FE55-D5D3-4D3D-B345-45591759C510}']
    function get_Status: Payments_PaymentCanMakePaymentResultStatus; safecall;
    property Status: Payments_PaymentCanMakePaymentResultStatus read get_Status;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentCurrencyAmount
  Payments_IPaymentCurrencyAmount = interface(IInspectable)
  ['{E3A3E9E0-B41F-4987-BDCB-071331F2DAA4}']
    function get_Currency: HSTRING; safecall;
    procedure put_Currency(value: HSTRING); safecall;
    function get_CurrencySystem: HSTRING; safecall;
    procedure put_CurrencySystem(value: HSTRING); safecall;
    function get_Value: HSTRING; safecall;
    procedure put_Value(value: HSTRING); safecall;
    property Currency: HSTRING read get_Currency write put_Currency;
    property CurrencySystem: HSTRING read get_CurrencySystem write put_CurrencySystem;
    property Value: HSTRING read get_Value write put_Value;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentItem
  Payments_IPaymentItem = interface(IInspectable)
  ['{685AC88B-79B2-4B76-9E03-A876223DFE72}']
    function get_Label: HSTRING; safecall;
    procedure put_Label(value: HSTRING); safecall;
    function get_Amount: Payments_IPaymentCurrencyAmount; safecall;
    procedure put_Amount(value: Payments_IPaymentCurrencyAmount); safecall;
    function get_Pending: Boolean; safecall;
    procedure put_Pending(value: Boolean); safecall;
    property Amount: Payments_IPaymentCurrencyAmount read get_Amount write put_Amount;
    property &Label: HSTRING read get_Label write put_Label;
    property Pending: Boolean read get_Pending write put_Pending;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IIterator_1__Payments_IPaymentItem_Base = interface(IInspectable)
  ['{93C12CC3-6A0B-5F02-AC74-056007472731}']
    function get_Current: Payments_IPaymentItem; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPayments_IPaymentItem): Cardinal; safecall;
    property Current: Payments_IPaymentItem read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IIterator_1__Payments_IPaymentItem = interface(IIterator_1__Payments_IPaymentItem_Base)
  ['{FD90DE59-18D1-5AA8-95F4-8C36690EF85F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IIterable_1__Payments_IPaymentItem_Base = interface(IInspectable)
  ['{B61B704C-E5FA-5524-8B95-7D03F5D36AE9}']
    function First: IIterator_1__Payments_IPaymentItem; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IIterable_1__Payments_IPaymentItem = interface(IIterable_1__Payments_IPaymentItem_Base)
  ['{F75248ED-55D4-5CDC-80C8-F4584B015FE9}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentItem>
  IVectorView_1__Payments_IPaymentItem = interface(IInspectable)
  ['{DFCB5863-B26C-56CF-9DC7-9D811C215A21}']
    function GetAt(index: Cardinal): Payments_IPaymentItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Payments_IPaymentItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPayments_IPaymentItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentShippingOption
  Payments_IPaymentShippingOption = interface(IInspectable)
  ['{13372ADA-9753-4574-8966-93145A76C7F9}']
    function get_Label: HSTRING; safecall;
    procedure put_Label(value: HSTRING); safecall;
    function get_Amount: Payments_IPaymentCurrencyAmount; safecall;
    procedure put_Amount(value: Payments_IPaymentCurrencyAmount); safecall;
    function get_Tag: HSTRING; safecall;
    procedure put_Tag(value: HSTRING); safecall;
    function get_IsSelected: Boolean; safecall;
    procedure put_IsSelected(value: Boolean); safecall;
    property Amount: Payments_IPaymentCurrencyAmount read get_Amount write put_Amount;
    property IsSelected: Boolean read get_IsSelected write put_IsSelected;
    property &Label: HSTRING read get_Label write put_Label;
    property Tag: HSTRING read get_Tag write put_Tag;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IIterator_1__Payments_IPaymentShippingOption_Base = interface(IInspectable)
  ['{49EDC9F4-2CE6-534C-B529-5CEEC705DEF5}']
    function get_Current: Payments_IPaymentShippingOption; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPayments_IPaymentShippingOption): Cardinal; safecall;
    property Current: Payments_IPaymentShippingOption read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IIterator_1__Payments_IPaymentShippingOption = interface(IIterator_1__Payments_IPaymentShippingOption_Base)
  ['{EEF23E99-D8A7-5E62-BD76-5417EE084074}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IIterable_1__Payments_IPaymentShippingOption_Base = interface(IInspectable)
  ['{DE881C69-6642-54DE-A8F7-D1A88B2404CF}']
    function First: IIterator_1__Payments_IPaymentShippingOption; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IIterable_1__Payments_IPaymentShippingOption = interface(IIterable_1__Payments_IPaymentShippingOption_Base)
  ['{EA4D6388-6D88-5FE0-B6EA-AFDDFC41E88B}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentShippingOption>
  IVectorView_1__Payments_IPaymentShippingOption = interface(IInspectable)
  ['{01ABA80A-C376-56F9-8191-79E5081F918B}']
    function GetAt(index: Cardinal): Payments_IPaymentShippingOption; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Payments_IPaymentShippingOption; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPayments_IPaymentShippingOption): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentDetailsModifier
  Payments_IPaymentDetailsModifier = interface(IInspectable)
  ['{BE1C7D65-4323-41D7-B305-DFCB765F69DE}']
    function get_JsonData: HSTRING; safecall;
    function get_SupportedMethodIds: IVectorView_1__HSTRING; safecall;
    function get_Total: Payments_IPaymentItem; safecall;
    function get_AdditionalDisplayItems: IVectorView_1__Payments_IPaymentItem; safecall;
    property AdditionalDisplayItems: IVectorView_1__Payments_IPaymentItem read get_AdditionalDisplayItems;
    property JsonData: HSTRING read get_JsonData;
    property SupportedMethodIds: IVectorView_1__HSTRING read get_SupportedMethodIds;
    property Total: Payments_IPaymentItem read get_Total;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IIterator_1__Payments_IPaymentDetailsModifier_Base = interface(IInspectable)
  ['{FAFB6774-B665-5B8B-B1EF-95038C3AABE1}']
    function get_Current: Payments_IPaymentDetailsModifier; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPayments_IPaymentDetailsModifier): Cardinal; safecall;
    property Current: Payments_IPaymentDetailsModifier read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IIterator_1__Payments_IPaymentDetailsModifier = interface(IIterator_1__Payments_IPaymentDetailsModifier_Base)
  ['{0D68A9E4-8321-5A50-8BFE-C3C690B75688}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IIterable_1__Payments_IPaymentDetailsModifier_Base = interface(IInspectable)
  ['{585D2B3D-CB34-58C4-81F4-1EA157996DEF}']
    function First: IIterator_1__Payments_IPaymentDetailsModifier; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IIterable_1__Payments_IPaymentDetailsModifier = interface(IIterable_1__Payments_IPaymentDetailsModifier_Base)
  ['{ECFEA4BD-ADED-5A8B-A607-5D9015631128}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentDetailsModifier>
  IVectorView_1__Payments_IPaymentDetailsModifier = interface(IInspectable)
  ['{205CEF2F-53D2-5A33-A7FF-5D8C8DA2AFC8}']
    function GetAt(index: Cardinal): Payments_IPaymentDetailsModifier; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Payments_IPaymentDetailsModifier; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPayments_IPaymentDetailsModifier): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentDetails
  Payments_IPaymentDetails = interface(IInspectable)
  ['{53BB2D7D-E0EB-4053-8EAE-CE7C48E02945}']
    function get_Total: Payments_IPaymentItem; safecall;
    procedure put_Total(value: Payments_IPaymentItem); safecall;
    function get_DisplayItems: IVectorView_1__Payments_IPaymentItem; safecall;
    procedure put_DisplayItems(value: IVectorView_1__Payments_IPaymentItem); safecall;
    function get_ShippingOptions: IVectorView_1__Payments_IPaymentShippingOption; safecall;
    procedure put_ShippingOptions(value: IVectorView_1__Payments_IPaymentShippingOption); safecall;
    function get_Modifiers: IVectorView_1__Payments_IPaymentDetailsModifier; safecall;
    procedure put_Modifiers(value: IVectorView_1__Payments_IPaymentDetailsModifier); safecall;
    property DisplayItems: IVectorView_1__Payments_IPaymentItem read get_DisplayItems write put_DisplayItems;
    property Modifiers: IVectorView_1__Payments_IPaymentDetailsModifier read get_Modifiers write put_Modifiers;
    property ShippingOptions: IVectorView_1__Payments_IPaymentShippingOption read get_ShippingOptions write put_ShippingOptions;
    property Total: Payments_IPaymentItem read get_Total write put_Total;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentToken
  Payments_IPaymentToken = interface(IInspectable)
  ['{BBCAC013-CCD0-41F2-B2A1-0A2E4B5DCE25}']
    function get_PaymentMethodId: HSTRING; safecall;
    function get_JsonDetails: HSTRING; safecall;
    property JsonDetails: HSTRING read get_JsonDetails;
    property PaymentMethodId: HSTRING read get_PaymentMethodId;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentResponse
  Payments_IPaymentResponse = interface(IInspectable)
  ['{E1389457-8BD2-4888-9FA8-97985545108E}']
    function get_PaymentToken: Payments_IPaymentToken; safecall;
    function get_ShippingOption: Payments_IPaymentShippingOption; safecall;
    function get_ShippingAddress: Payments_IPaymentAddress; safecall;
    function get_PayerEmail: HSTRING; safecall;
    function get_PayerName: HSTRING; safecall;
    function get_PayerPhoneNumber: HSTRING; safecall;
    function CompleteAsync(status: Payments_PaymentRequestCompletionStatus): IAsyncAction; safecall;
    property PayerEmail: HSTRING read get_PayerEmail;
    property PayerName: HSTRING read get_PayerName;
    property PayerPhoneNumber: HSTRING read get_PayerPhoneNumber;
    property PaymentToken: Payments_IPaymentToken read get_PaymentToken;
    property ShippingAddress: Payments_IPaymentAddress read get_ShippingAddress;
    property ShippingOption: Payments_IPaymentShippingOption read get_ShippingOption;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult
  Payments_IPaymentRequestSubmitResult = interface(IInspectable)
  ['{7B9C3912-30F2-4E90-B249-8CE7D78FFE56}']
    function get_Status: Payments_PaymentRequestStatus; safecall;
    function get_Response: Payments_IPaymentResponse; safecall;
    property Response: Payments_IPaymentResponse read get_Response;
    property Status: Payments_PaymentRequestStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult_Delegate_Base = interface(IUnknown)
  ['{CBCD07A6-AE2A-5A70-BC0B-9120560825D1}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Payments_IPaymentRequestSubmitResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult = interface(AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult_Delegate_Base)
  ['{9B9E95FF-2F0D-5120-A5F5-25368049D3D3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult>
  IAsyncOperation_1__Payments_IPaymentRequestSubmitResult_Base = interface(IInspectable)
  ['{CF290DEB-5549-57C3-8ABD-53B76C643CCA}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult; safecall;
    function GetResults: Payments_IPaymentRequestSubmitResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Payments_IPaymentRequestSubmitResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentRequestSubmitResult>
  IAsyncOperation_1__Payments_IPaymentRequestSubmitResult = interface(IAsyncOperation_1__Payments_IPaymentRequestSubmitResult_Base)
  ['{CA6E042F-DB8E-53BA-8178-F710B91FB420}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentMerchantInfo
  Payments_IPaymentMerchantInfo = interface(IInspectable)
  ['{63445050-0E94-4ED6-AACB-E6012BD327A7}']
    function get_PackageFullName: HSTRING; safecall;
    function get_Uri: IUriRuntimeClass; safecall;
    property PackageFullName: HSTRING read get_PackageFullName;
    property Uri: IUriRuntimeClass read get_Uri;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentMethodData
  Payments_IPaymentMethodData = interface(IInspectable)
  ['{D1D3CAF4-DE98-4129-B1B7-C3AD86237BF4}']
    function get_SupportedMethodIds: IVectorView_1__HSTRING; safecall;
    function get_JsonData: HSTRING; safecall;
    property JsonData: HSTRING read get_JsonData;
    property SupportedMethodIds: IVectorView_1__HSTRING read get_SupportedMethodIds;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IIterator_1__Payments_IPaymentMethodData_Base = interface(IInspectable)
  ['{9666522E-E5AC-5374-A5D9-5CF57C4BF689}']
    function get_Current: Payments_IPaymentMethodData; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPayments_IPaymentMethodData): Cardinal; safecall;
    property Current: Payments_IPaymentMethodData read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IIterator_1__Payments_IPaymentMethodData = interface(IIterator_1__Payments_IPaymentMethodData_Base)
  ['{E9B48910-E831-5A98-A88F-BBF38F80C834}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IIterable_1__Payments_IPaymentMethodData_Base = interface(IInspectable)
  ['{8C9ED87E-4ADD-58A8-AD9A-9DFA48CA250A}']
    function First: IIterator_1__Payments_IPaymentMethodData; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IIterable_1__Payments_IPaymentMethodData = interface(IIterable_1__Payments_IPaymentMethodData_Base)
  ['{9A12292A-4C45-5D4A-A598-A8462A6E7BB8}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Payments.IPaymentMethodData>
  IVectorView_1__Payments_IPaymentMethodData = interface(IInspectable)
  ['{D4EB6D93-3C91-50AB-9E50-46FC93402229}']
    function GetAt(index: Cardinal): Payments_IPaymentMethodData; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Payments_IPaymentMethodData; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPayments_IPaymentMethodData): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentOptions
  Payments_IPaymentOptions = interface(IInspectable)
  ['{AAA30854-1F2B-4365-8251-01B58915A5BC}']
    function get_RequestPayerEmail: Payments_PaymentOptionPresence; safecall;
    procedure put_RequestPayerEmail(value: Payments_PaymentOptionPresence); safecall;
    function get_RequestPayerName: Payments_PaymentOptionPresence; safecall;
    procedure put_RequestPayerName(value: Payments_PaymentOptionPresence); safecall;
    function get_RequestPayerPhoneNumber: Payments_PaymentOptionPresence; safecall;
    procedure put_RequestPayerPhoneNumber(value: Payments_PaymentOptionPresence); safecall;
    function get_RequestShipping: Boolean; safecall;
    procedure put_RequestShipping(value: Boolean); safecall;
    function get_ShippingType: Payments_PaymentShippingType; safecall;
    procedure put_ShippingType(value: Payments_PaymentShippingType); safecall;
    property RequestPayerEmail: Payments_PaymentOptionPresence read get_RequestPayerEmail write put_RequestPayerEmail;
    property RequestPayerName: Payments_PaymentOptionPresence read get_RequestPayerName write put_RequestPayerName;
    property RequestPayerPhoneNumber: Payments_PaymentOptionPresence read get_RequestPayerPhoneNumber write put_RequestPayerPhoneNumber;
    property RequestShipping: Boolean read get_RequestShipping write put_RequestShipping;
    property ShippingType: Payments_PaymentShippingType read get_ShippingType write put_ShippingType;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentRequest
  Payments_IPaymentRequest = interface(IInspectable)
  ['{B74942E1-ED7B-47EB-BC08-78CC5D6896B6}']
    function get_MerchantInfo: Payments_IPaymentMerchantInfo; safecall;
    function get_Details: Payments_IPaymentDetails; safecall;
    function get_MethodData: IVectorView_1__Payments_IPaymentMethodData; safecall;
    function get_Options: Payments_IPaymentOptions; safecall;
    property Details: Payments_IPaymentDetails read get_Details;
    property MerchantInfo: Payments_IPaymentMerchantInfo read get_MerchantInfo;
    property MethodData: IVectorView_1__Payments_IPaymentMethodData read get_MethodData;
    property Options: Payments_IPaymentOptions read get_Options;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentRequestChangedResult
  Payments_IPaymentRequestChangedResult = interface(IInspectable)
  ['{DF699E5C-16C4-47AD-9401-8440EC0757DB}']
    function get_ChangeAcceptedByMerchant: Boolean; safecall;
    procedure put_ChangeAcceptedByMerchant(value: Boolean); safecall;
    function get_Message: HSTRING; safecall;
    procedure put_Message(value: HSTRING); safecall;
    function get_UpdatedPaymentDetails: Payments_IPaymentDetails; safecall;
    procedure put_UpdatedPaymentDetails(value: Payments_IPaymentDetails); safecall;
    property ChangeAcceptedByMerchant: Boolean read get_ChangeAcceptedByMerchant write put_ChangeAcceptedByMerchant;
    property &Message: HSTRING read get_Message write put_Message;
    property UpdatedPaymentDetails: Payments_IPaymentDetails read get_UpdatedPaymentDetails write put_UpdatedPaymentDetails;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentRequestChangedArgs
  Payments_IPaymentRequestChangedArgs = interface(IInspectable)
  ['{C6145E44-CD8B-4BE4-B555-27C99194C0C5}']
    function get_ChangeKind: Payments_PaymentRequestChangeKind; safecall;
    function get_ShippingAddress: Payments_IPaymentAddress; safecall;
    function get_SelectedShippingOption: Payments_IPaymentShippingOption; safecall;
    procedure Acknowledge(changeResult: Payments_IPaymentRequestChangedResult); safecall;
    property ChangeKind: Payments_PaymentRequestChangeKind read get_ChangeKind;
    property SelectedShippingOption: Payments_IPaymentShippingOption read get_SelectedShippingOption;
    property ShippingAddress: Payments_IPaymentAddress read get_ShippingAddress;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.PaymentRequestChangedHandler
  Payments_PaymentRequestChangedHandler = interface(IUnknown)
  ['{5078B9E1-F398-4F2C-A27E-94D371CF6C7D}']
    procedure Invoke(paymentRequest: Payments_IPaymentRequest; args: Payments_IPaymentRequestChangedArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentMediator
  [WinRTClassNameAttribute(SWindows_ApplicationModel_Payments_PaymentMediator)]
  Payments_IPaymentMediator = interface(IInspectable)
  ['{FB0EE829-EC0C-449A-83DA-7AE3073365A2}']
    function GetSupportedMethodIdsAsync: IAsyncOperation_1__IVectorView_1__HSTRING; safecall;
    function SubmitPaymentRequestAsync(paymentRequest: Payments_IPaymentRequest): IAsyncOperation_1__Payments_IPaymentRequestSubmitResult; overload; safecall;
    function SubmitPaymentRequestAsync(paymentRequest: Payments_IPaymentRequest; changeHandler: Payments_PaymentRequestChangedHandler): IAsyncOperation_1__Payments_IPaymentRequestSubmitResult; overload; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult_Delegate_Base = interface(IUnknown)
  ['{89AE5B89-6D05-5842-9CDF-F4CBF706DC5E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Payments_IPaymentCanMakePaymentResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult = interface(AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult_Delegate_Base)
  ['{4F0B1A8D-A80A-5EC5-B418-1C561AB31D2B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult>
  IAsyncOperation_1__Payments_IPaymentCanMakePaymentResult_Base = interface(IInspectable)
  ['{A467410A-11DE-5090-B905-96A562D85DE5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult; safecall;
    function GetResults: Payments_IPaymentCanMakePaymentResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Payments_IPaymentCanMakePaymentResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentCanMakePaymentResult>
  IAsyncOperation_1__Payments_IPaymentCanMakePaymentResult = interface(IAsyncOperation_1__Payments_IPaymentCanMakePaymentResult_Base)
  ['{40D26067-7810-5D27-860B-8E8D5E6A06A5}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.Payments.IPaymentMediator2
  Payments_IPaymentMediator2 = interface(IInspectable)
  ['{CEEF98F1-E407-4128-8E73-D93D5F822786}']
    function CanMakePaymentAsync(paymentRequest: Payments_IPaymentRequest): IAsyncOperation_1__Payments_IPaymentCanMakePaymentResult; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentRequestChangedResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult_Delegate_Base = interface(IUnknown)
  ['{BEC8B726-9056-5E47-B22A-0DA09AA84AFE}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Payments_IPaymentRequestChangedResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Payments.IPaymentRequestChangedResult>
  AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult = interface(AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult_Delegate_Base)
  ['{80510216-7791-5D33-B04B-94D78F32CABC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentRequestChangedResult>
  IAsyncOperation_1__Payments_IPaymentRequestChangedResult_Base = interface(IInspectable)
  ['{0CC32025-AC67-57E2-A0F6-3A8E116CEF4C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult; safecall;
    function GetResults: Payments_IPaymentRequestChangedResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Payments_IPaymentRequestChangedResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Payments.IPaymentRequestChangedResult>
  IAsyncOperation_1__Payments_IPaymentRequestChangedResult = interface(IAsyncOperation_1__Payments_IPaymentRequestChangedResult_Base)
  ['{F2EAE227-35BE-5AFA-BC94-912567CFB700}']
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

  // Generic Delegate for:
  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,String>
  MapChangedEventHandler_2__HSTRING__HSTRING_Delegate_Base = interface(IUnknown)
  ['{E2663F37-2E1B-500C-AD68-C3ED7A8F74C8}']
    procedure Invoke(sender: IObservableMap_2__HSTRING__HSTRING; event: IMapChangedEventArgs_1__HSTRING); safecall;
  end;
  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,String>
  MapChangedEventHandler_2__HSTRING__HSTRING = interface(MapChangedEventHandler_2__HSTRING__HSTRING_Delegate_Base)
  ['{E2663F37-2E1B-500C-AD68-C3ED7A8F74C8}']
  end;

  // Windows.Foundation.Collections.IObservableMap`2<String,String>
  IObservableMap_2__HSTRING__HSTRING = interface(IInspectable)
  ['{1E036276-2F60-55F6-B7F3-F86079E6900B}']
    function add_MapChanged(vhnd: MapChangedEventHandler_2__HSTRING__HSTRING): EventRegistrationToken; safecall;
    procedure remove_MapChanged(token: EventRegistrationToken); safecall;
  end;

  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.ApplicationModel.Search.Core.ISearchSuggestion>
  VectorChangedEventHandler_1__Search_Core_ISearchSuggestion = interface(IUnknown)
  ['{3528538E-1115-54FD-8943-A340AE8FEAB1}']
    procedure Invoke(sender: IObservableVector_1__Search_Core_ISearchSuggestion; event: IVectorChangedEventArgs); safecall;
  end;

  // Windows.Foundation.Collections.IObservableVector`1<Windows.ApplicationModel.Search.Core.ISearchSuggestion>
  IObservableVector_1__Search_Core_ISearchSuggestion = interface(IInspectable)
  ['{944A3DA2-8D2E-5EAE-8E5A-99D6B12F6043}']
    function add_VectorChanged(vhnd: VectorChangedEventHandler_1__Search_Core_ISearchSuggestion): EventRegistrationToken; safecall;
    procedure remove_VectorChanged(token: EventRegistrationToken); safecall;
  end;

  // Windows.ApplicationModel.Store.LicenseChangedEventHandler
  Store_LicenseChangedEventHandler = interface(IUnknown)
  ['{D4A50255-1369-4C36-832F-6F2D88E3659B}']
    procedure Invoke; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Store.FulfillmentResult>
  AsyncOperationCompletedHandler_1__Store_FulfillmentResult_Delegate_Base = interface(IUnknown)
  ['{8775ACC9-B9AE-5CCE-895C-971BF9270892}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Store_FulfillmentResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.Store.FulfillmentResult>
  AsyncOperationCompletedHandler_1__Store_FulfillmentResult = interface(AsyncOperationCompletedHandler_1__Store_FulfillmentResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Store.FulfillmentResult>
  IAsyncOperation_1__Store_FulfillmentResult_Base = interface(IInspectable)
  ['{5C8531AC-5D8D-5E07-B6EE-7CAB96930E8A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Store_FulfillmentResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Store_FulfillmentResult; safecall;
    function GetResults: Store_FulfillmentResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Store_FulfillmentResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.Store.FulfillmentResult>
  IAsyncOperation_1__Store_FulfillmentResult = interface(IAsyncOperation_1__Store_FulfillmentResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Windows.ApplicationModel.Store.IProductListingWithConsumables
  Store_IProductListingWithConsumables = interface(IInspectable)
  ['{EB9E9790-8F6B-481F-93A7-5C3A63068149}']
    function get_ProductType: Store_ProductType; safecall;
    property ProductType: Store_ProductType read get_ProductType;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IIterator_1__Store_Preview_StoreSystemFeature_Base = interface(IInspectable)
  ['{D0455B2D-D8AA-557E-89A3-63C33E8CEE99}']
    function get_Current: Store_Preview_StoreSystemFeature; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PStore_Preview_StoreSystemFeature): Cardinal; safecall;
    property Current: Store_Preview_StoreSystemFeature read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IIterator_1__Store_Preview_StoreSystemFeature = interface(IIterator_1__Store_Preview_StoreSystemFeature_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IIterable_1__Store_Preview_StoreSystemFeature_Base = interface(IInspectable)
  ['{98A8577A-B128-5400-8D3D-58654EAAF957}']
    function First: IIterator_1__Store_Preview_StoreSystemFeature; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IIterable_1__Store_Preview_StoreSystemFeature = interface(IIterable_1__Store_Preview_StoreSystemFeature_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>
  IVectorView_1__Store_Preview_StoreSystemFeature = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Store_Preview_StoreSystemFeature; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Store_Preview_StoreSystemFeature; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PStore_Preview_StoreSystemFeature): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature_Delegate_Base = interface(IUnknown)
  ['{7E7946EF-F8F0-53FD-9613-7261CB35DAF4}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Store_Preview_StoreSystemFeature; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature = interface(AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature_Delegate_Base)
  ['{5A9F94D2-879A-565B-91C7-6CD2248BA109}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>>
  IAsyncOperation_1__IVectorView_1__Store_Preview_StoreSystemFeature_Base = interface(IInspectable)
  ['{B1713163-EE1B-5290-8316-F7EBB9D53163}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature; safecall;
    function GetResults: IVectorView_1__Store_Preview_StoreSystemFeature; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Store_Preview_StoreSystemFeature read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.Store.Preview.StoreSystemFeature>>
  IAsyncOperation_1__IVectorView_1__Store_Preview_StoreSystemFeature = interface(IAsyncOperation_1__IVectorView_1__Store_Preview_StoreSystemFeature_Base)
  ['{DF78340B-B47F-50E2-9EC5-530CB35D7BE7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackage>
  IVector_1__IPackage_Base = interface(IInspectable)
  ['{D1BB509E-6989-5C69-B1FF-D1702FE8ACA3}']
    function GetAt(index: Cardinal): IPackage; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IPackage; safecall;
    function IndexOf(value: IPackage; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IPackage); safecall;
    procedure InsertAt(index: Cardinal; value: IPackage); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IPackage); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPackage): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIPackage); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.IPackage>
  IVector_1__IPackage = interface(IVector_1__IPackage_Base)
  ['{7704985F-8D81-5C7F-9B57-870C324DA741}']
  end;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.UserDataTasks.UserDataTaskDaysOfWeek>
  IReference_1__UserDataTasks_UserDataTaskDaysOfWeek = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: UserDataTasks_UserDataTaskDaysOfWeek; safecall;
    property Value: UserDataTasks_UserDataTaskDaysOfWeek read get_Value;
  end;

  // Windows.Foundation.IReference`1<Windows.ApplicationModel.UserDataTasks.UserDataTaskWeekOfMonth>
  IReference_1__UserDataTasks_UserDataTaskWeekOfMonth = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: UserDataTasks_UserDataTaskWeekOfMonth; safecall;
    property Value: UserDataTasks_UserDataTaskWeekOfMonth read get_Value;
  end;

  // Windows.ApplicationModel.AppInfo
  // DualAPI
  // Implements: Windows.ApplicationModel.IAppInfo
  // Implements: Windows.ApplicationModel.IAppInfo2
  // Statics: "Windows.ApplicationModel.IAppInfoStatics"
  TAppInfo = class(TWinRTGenericImportS<IAppInfoStatics>)
  public
    // -> IAppInfoStatics
    class function get_Current: IAppInfo; static; inline;
    class function GetFromAppUserModelId(appUserModelId: HSTRING): IAppInfo; static; inline;
    class function GetFromAppUserModelIdForUser(user: IUser; appUserModelId: HSTRING): IAppInfo; static; inline;
    class property Current: IAppInfo read get_Current;
  end;

  // Windows.ApplicationModel.AppService.AppServiceConnection
  // DualAPI
  // Implements: Windows.ApplicationModel.AppService.IAppServiceConnection
  // Implements: Windows.ApplicationModel.AppService.IAppServiceConnection2
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.ApplicationModel.AppService.IAppServiceConnectionStatics"
  // Instantiable: "AppService_IAppServiceConnection"
  TAppService_AppServiceConnection = class(TWinRTGenericImportSI<AppService_IAppServiceConnectionStatics, AppService_IAppServiceConnection>)
  public
    // -> AppService_IAppServiceConnectionStatics
    class function SendStatelessMessageAsync(connection: AppService_IAppServiceConnection; connectionRequest: RemoteSystems_IRemoteSystemConnectionRequest; &message: IPropertySet): IAsyncOperation_1__AppService_IStatelessAppServiceResponse; static; inline;
  end;

  // Windows.ApplicationModel.LockScreen.LockApplicationHost
  // DualAPI
  // Implements: Windows.ApplicationModel.LockScreen.ILockApplicationHost
  // Statics: "Windows.ApplicationModel.LockScreen.ILockApplicationHostStatics"
  TLockScreen_LockApplicationHost = class(TWinRTGenericImportS<LockScreen_ILockApplicationHostStatics>)
  public
    // -> LockScreen_ILockApplicationHostStatics
    class function GetForCurrentView: LockScreen_ILockApplicationHost; static; inline;
  end;

  // Windows.ApplicationModel.Package
  // DualAPI
  // Implements: Windows.ApplicationModel.IPackage
  // Implements: Windows.ApplicationModel.IPackage2
  // Implements: Windows.ApplicationModel.IPackage3
  // Implements: Windows.ApplicationModel.IPackageWithMetadata
  // Implements: Windows.ApplicationModel.IPackage4
  // Implements: Windows.ApplicationModel.IPackage5
  // Implements: Windows.ApplicationModel.IPackage6
  // Implements: Windows.ApplicationModel.IPackage7
  // Implements: Windows.ApplicationModel.IPackage8
  // Statics: "Windows.ApplicationModel.IPackageStatics"
  TPackageRT = class(TWinRTGenericImportS<IPackageStatics>)
  public
    // -> IPackageStatics
    class function get_Current: IPackage; static; inline;
    class property Current: IPackage read get_Current;
  end;

  // Windows.ApplicationModel.Payments.PaymentMediator
  // DualAPI
  // Implements: Windows.ApplicationModel.Payments.IPaymentMediator
  // Implements: Windows.ApplicationModel.Payments.IPaymentMediator2
  // Instantiable: "Payments_IPaymentMediator"
  TPayments_PaymentMediator = class(TWinRTGenericImportI<Payments_IPaymentMediator>) end;

implementation

{ TAppInfo }

class function TAppInfo.get_Current: IAppInfo;
begin
  Result := Statics.get_Current;
end;

class function TAppInfo.GetFromAppUserModelId(appUserModelId: HSTRING): IAppInfo;
begin
  Result := Statics.GetFromAppUserModelId(appUserModelId);
end;

class function TAppInfo.GetFromAppUserModelIdForUser(user: IUser; appUserModelId: HSTRING): IAppInfo;
begin
  Result := Statics.GetFromAppUserModelIdForUser(user, appUserModelId);
end;


{ TAppService_AppServiceConnection }

class function TAppService_AppServiceConnection.SendStatelessMessageAsync(connection: AppService_IAppServiceConnection; connectionRequest: RemoteSystems_IRemoteSystemConnectionRequest; &message: IPropertySet): IAsyncOperation_1__AppService_IStatelessAppServiceResponse;
begin
  Result := Statics.SendStatelessMessageAsync(connection, connectionRequest, &message);
end;


{ TLockScreen_LockApplicationHost }

class function TLockScreen_LockApplicationHost.GetForCurrentView: LockScreen_ILockApplicationHost;
begin
  Result := Statics.GetForCurrentView;
end;


{ TPackageRT }

class function TPackageRT.get_Current: IPackage;
begin
  Result := Statics.get_Current;
end;


{ TPayments_PaymentMediator }

end.
