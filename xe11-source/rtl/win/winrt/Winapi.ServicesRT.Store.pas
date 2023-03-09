{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.ServicesRT.Store;

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

  AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult;
  PAsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult;
  AsyncOperationCompletedHandler_1__IStoreAppLicense_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreAppLicense_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStoreAppLicense = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreAppLicense;
  PAsyncOperationCompletedHandler_1__IStoreAppLicense = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStoreAppLicense;
  AsyncOperationCompletedHandler_1__IStoreConsumableResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreConsumableResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStoreConsumableResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreConsumableResult;
  PAsyncOperationCompletedHandler_1__IStoreConsumableResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStoreConsumableResult;
  AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult;
  PAsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult;
  AsyncOperationCompletedHandler_1__IStoreProductQueryResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreProductQueryResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStoreProductQueryResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreProductQueryResult;
  PAsyncOperationCompletedHandler_1__IStoreProductQueryResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStoreProductQueryResult;
  AsyncOperationCompletedHandler_1__IStoreProductResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreProductResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStoreProductResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStoreProductResult;
  PAsyncOperationCompletedHandler_1__IStoreProductResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStoreProductResult;
  AsyncOperationCompletedHandler_1__IStorePurchaseResult_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorePurchaseResult_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStorePurchaseResult = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorePurchaseResult;
  PAsyncOperationCompletedHandler_1__IStorePurchaseResult = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStorePurchaseResult;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate_Delegate_Base;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate;
  AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base;
  AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus;
  PAsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = Winapi.CommonTypes.PAsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus;
  AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base;
  AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus;
  PAsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = Winapi.CommonTypes.PAsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus;
  IAsyncOperation_1__IStoreAcquireLicenseResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IStoreAcquireLicenseResult_Base;
  IAsyncOperation_1__IStoreAcquireLicenseResult = Winapi.CommonTypes.IAsyncOperation_1__IStoreAcquireLicenseResult;
  PIAsyncOperation_1__IStoreAcquireLicenseResult = Winapi.CommonTypes.PIAsyncOperation_1__IStoreAcquireLicenseResult;
  IAsyncOperation_1__IStoreAppLicense_Base = Winapi.CommonTypes.IAsyncOperation_1__IStoreAppLicense_Base;
  IAsyncOperation_1__IStoreAppLicense = Winapi.CommonTypes.IAsyncOperation_1__IStoreAppLicense;
  PIAsyncOperation_1__IStoreAppLicense = Winapi.CommonTypes.PIAsyncOperation_1__IStoreAppLicense;
  IAsyncOperation_1__IStoreConsumableResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IStoreConsumableResult_Base;
  IAsyncOperation_1__IStoreConsumableResult = Winapi.CommonTypes.IAsyncOperation_1__IStoreConsumableResult;
  PIAsyncOperation_1__IStoreConsumableResult = Winapi.CommonTypes.PIAsyncOperation_1__IStoreConsumableResult;
  IAsyncOperation_1__IStoreProductPagedQueryResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IStoreProductPagedQueryResult_Base;
  IAsyncOperation_1__IStoreProductPagedQueryResult = Winapi.CommonTypes.IAsyncOperation_1__IStoreProductPagedQueryResult;
  PIAsyncOperation_1__IStoreProductPagedQueryResult = Winapi.CommonTypes.PIAsyncOperation_1__IStoreProductPagedQueryResult;
  IAsyncOperation_1__IStoreProductQueryResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IStoreProductQueryResult_Base;
  IAsyncOperation_1__IStoreProductQueryResult = Winapi.CommonTypes.IAsyncOperation_1__IStoreProductQueryResult;
  PIAsyncOperation_1__IStoreProductQueryResult = Winapi.CommonTypes.PIAsyncOperation_1__IStoreProductQueryResult;
  IAsyncOperation_1__IStoreProductResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IStoreProductResult_Base;
  IAsyncOperation_1__IStoreProductResult = Winapi.CommonTypes.IAsyncOperation_1__IStoreProductResult;
  PIAsyncOperation_1__IStoreProductResult = Winapi.CommonTypes.PIAsyncOperation_1__IStoreProductResult;
  IAsyncOperation_1__IStorePurchaseResult_Base = Winapi.CommonTypes.IAsyncOperation_1__IStorePurchaseResult_Base;
  IAsyncOperation_1__IStorePurchaseResult = Winapi.CommonTypes.IAsyncOperation_1__IStorePurchaseResult;
  PIAsyncOperation_1__IStorePurchaseResult = Winapi.CommonTypes.PIAsyncOperation_1__IStorePurchaseResult;
  IAsyncOperation_1__IVectorView_1__IStorePackageUpdate_Base = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorePackageUpdate_Base;
  IAsyncOperation_1__IVectorView_1__IStorePackageUpdate = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorePackageUpdate;
  PIAsyncOperation_1__IVectorView_1__IStorePackageUpdate = Winapi.CommonTypes.PIAsyncOperation_1__IVectorView_1__IStorePackageUpdate;
  IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Base = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Base;
  IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus;
  PIAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus = Winapi.CommonTypes.PIAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus;
  IIterable_1__IStorePackageUpdate_Base = Winapi.CommonTypes.IIterable_1__IStorePackageUpdate_Base;
  IIterable_1__IStorePackageUpdate = Winapi.CommonTypes.IIterable_1__IStorePackageUpdate;
  PIIterable_1__IStorePackageUpdate = Winapi.CommonTypes.PIIterable_1__IStorePackageUpdate;
  IIterator_1__IStorePackageUpdate_Base = Winapi.CommonTypes.IIterator_1__IStorePackageUpdate_Base;
  IIterator_1__IStorePackageUpdate = Winapi.CommonTypes.IIterator_1__IStorePackageUpdate;
  PIIterator_1__IStorePackageUpdate = Winapi.CommonTypes.PIIterator_1__IStorePackageUpdate;
  IMapView_2__HSTRING__IStoreLicense_Base = Winapi.CommonTypes.IMapView_2__HSTRING__IStoreLicense_Base;
  IMapView_2__HSTRING__IStoreLicense = Winapi.CommonTypes.IMapView_2__HSTRING__IStoreLicense;
  PIMapView_2__HSTRING__IStoreLicense = Winapi.CommonTypes.PIMapView_2__HSTRING__IStoreLicense;
  IMapView_2__HSTRING__IStoreProduct_Base = Winapi.CommonTypes.IMapView_2__HSTRING__IStoreProduct_Base;
  IMapView_2__HSTRING__IStoreProduct = Winapi.CommonTypes.IMapView_2__HSTRING__IStoreProduct;
  PIMapView_2__HSTRING__IStoreProduct = Winapi.CommonTypes.PIMapView_2__HSTRING__IStoreProduct;
  IStoreAcquireLicenseResult = Winapi.CommonTypes.IStoreAcquireLicenseResult;
  PIStoreAcquireLicenseResult = Winapi.CommonTypes.PIStoreAcquireLicenseResult;
  IStoreAppLicense = Winapi.CommonTypes.IStoreAppLicense;
  PIStoreAppLicense = Winapi.CommonTypes.PIStoreAppLicense;
  IStoreAvailability = Winapi.CommonTypes.IStoreAvailability;
  PIStoreAvailability = Winapi.CommonTypes.PIStoreAvailability;
  IStoreCollectionData = Winapi.CommonTypes.IStoreCollectionData;
  PIStoreCollectionData = Winapi.CommonTypes.PIStoreCollectionData;
  IStoreConsumableResult = Winapi.CommonTypes.IStoreConsumableResult;
  PIStoreConsumableResult = Winapi.CommonTypes.PIStoreConsumableResult;
  IStoreContext = Winapi.CommonTypes.IStoreContext;
  PIStoreContext = Winapi.CommonTypes.PIStoreContext;
  IStoreImage = Winapi.CommonTypes.IStoreImage;
  PIStoreImage = Winapi.CommonTypes.PIStoreImage;
  IStoreLicense = Winapi.CommonTypes.IStoreLicense;
  PIStoreLicense = Winapi.CommonTypes.PIStoreLicense;
  IStorePackageLicense = Winapi.CommonTypes.IStorePackageLicense;
  PIStorePackageLicense = Winapi.CommonTypes.PIStorePackageLicense;
  IStorePackageUpdate = Winapi.CommonTypes.IStorePackageUpdate;
  PIStorePackageUpdate = Winapi.CommonTypes.PIStorePackageUpdate;
  IStorePackageUpdateResult = Winapi.CommonTypes.IStorePackageUpdateResult;
  PIStorePackageUpdateResult = Winapi.CommonTypes.PIStorePackageUpdateResult;
  IStorePrice = Winapi.CommonTypes.IStorePrice;
  PIStorePrice = Winapi.CommonTypes.PIStorePrice;
  IStoreProduct = Winapi.CommonTypes.IStoreProduct;
  PIStoreProduct = Winapi.CommonTypes.PIStoreProduct;
  IStoreProductPagedQueryResult = Winapi.CommonTypes.IStoreProductPagedQueryResult;
  PIStoreProductPagedQueryResult = Winapi.CommonTypes.PIStoreProductPagedQueryResult;
  IStoreProductQueryResult = Winapi.CommonTypes.IStoreProductQueryResult;
  PIStoreProductQueryResult = Winapi.CommonTypes.PIStoreProductQueryResult;
  IStoreProductResult = Winapi.CommonTypes.IStoreProductResult;
  PIStoreProductResult = Winapi.CommonTypes.PIStoreProductResult;
  IStorePurchaseProperties = Winapi.CommonTypes.IStorePurchaseProperties;
  PIStorePurchaseProperties = Winapi.CommonTypes.PIStorePurchaseProperties;
  IStorePurchaseResult = Winapi.CommonTypes.IStorePurchaseResult;
  PIStorePurchaseResult = Winapi.CommonTypes.PIStorePurchaseResult;
  IStoreQueueItem = Winapi.CommonTypes.IStoreQueueItem;
  PIStoreQueueItem = Winapi.CommonTypes.PIStoreQueueItem;
  IStoreQueueItemCompletedEventArgs = Winapi.CommonTypes.IStoreQueueItemCompletedEventArgs;
  PIStoreQueueItemCompletedEventArgs = Winapi.CommonTypes.PIStoreQueueItemCompletedEventArgs;
  IStoreQueueItemStatus = Winapi.CommonTypes.IStoreQueueItemStatus;
  PIStoreQueueItemStatus = Winapi.CommonTypes.PIStoreQueueItemStatus;
  IStoreSku = Winapi.CommonTypes.IStoreSku;
  PIStoreSku = Winapi.CommonTypes.PIStoreSku;
  IStoreSubscriptionInfo = Winapi.CommonTypes.IStoreSubscriptionInfo;
  PIStoreSubscriptionInfo = Winapi.CommonTypes.PIStoreSubscriptionInfo;
  IStoreVideo = Winapi.CommonTypes.IStoreVideo;
  PIStoreVideo = Winapi.CommonTypes.PIStoreVideo;
  IVectorView_1__IStoreAvailability = Winapi.CommonTypes.IVectorView_1__IStoreAvailability;
  PIVectorView_1__IStoreAvailability = Winapi.CommonTypes.PIVectorView_1__IStoreAvailability;
  IVectorView_1__IStoreImage = Winapi.CommonTypes.IVectorView_1__IStoreImage;
  PIVectorView_1__IStoreImage = Winapi.CommonTypes.PIVectorView_1__IStoreImage;
  IVectorView_1__IStorePackageUpdate = Winapi.CommonTypes.IVectorView_1__IStorePackageUpdate;
  PIVectorView_1__IStorePackageUpdate = Winapi.CommonTypes.PIVectorView_1__IStorePackageUpdate;
  IVectorView_1__IStoreSku = Winapi.CommonTypes.IVectorView_1__IStoreSku;
  PIVectorView_1__IStoreSku = Winapi.CommonTypes.PIVectorView_1__IStoreSku;
  IVectorView_1__IStoreVideo = Winapi.CommonTypes.IVectorView_1__IStoreVideo;
  PIVectorView_1__IStoreVideo = Winapi.CommonTypes.PIVectorView_1__IStoreVideo;
  IVectorView_1__StorePackageUpdateStatus = Winapi.CommonTypes.IVectorView_1__StorePackageUpdateStatus;
  PIVectorView_1__StorePackageUpdateStatus = Winapi.CommonTypes.PIVectorView_1__StorePackageUpdateStatus;
  StoreConsumableStatus = Winapi.CommonTypes.StoreConsumableStatus;
  PStoreConsumableStatus = Winapi.CommonTypes.PStoreConsumableStatus;
  StoreDurationUnit = Winapi.CommonTypes.StoreDurationUnit;
  PStoreDurationUnit = Winapi.CommonTypes.PStoreDurationUnit;
  StorePackageUpdateState = Winapi.CommonTypes.StorePackageUpdateState;
  PStorePackageUpdateState = Winapi.CommonTypes.PStorePackageUpdateState;
  StorePackageUpdateStatus = Winapi.CommonTypes.StorePackageUpdateStatus;
  PStorePackageUpdateStatus = Winapi.CommonTypes.PStorePackageUpdateStatus;
  StorePurchaseStatus = Winapi.CommonTypes.StorePurchaseStatus;
  PStorePurchaseStatus = Winapi.CommonTypes.PStorePurchaseStatus;
  StoreQueueItemExtendedState = Winapi.CommonTypes.StoreQueueItemExtendedState;
  PStoreQueueItemExtendedState = Winapi.CommonTypes.PStoreQueueItemExtendedState;
  StoreQueueItemKind = Winapi.CommonTypes.StoreQueueItemKind;
  PStoreQueueItemKind = Winapi.CommonTypes.PStoreQueueItemKind;
  StoreQueueItemState = Winapi.CommonTypes.StoreQueueItemState;
  PStoreQueueItemState = Winapi.CommonTypes.PStoreQueueItemState;
  TypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs;
  PTypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs;

  // Forward declarations for interfaces

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>
  IKeyValuePair_2__HSTRING__IStoreLicense = interface;
  PIKeyValuePair_2__HSTRING__IStoreLicense = ^IKeyValuePair_2__HSTRING__IStoreLicense;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>>
  IIterator_1__IKeyValuePair_2__HSTRING__IStoreLicense = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IStoreLicense = ^IIterator_1__IKeyValuePair_2__HSTRING__IStoreLicense;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>>
  IIterable_1__IKeyValuePair_2__HSTRING__IStoreLicense = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IStoreLicense = ^IIterable_1__IKeyValuePair_2__HSTRING__IStoreLicense;

  // Windows.Services.Store.IStoreCanAcquireLicenseResult
  IStoreCanAcquireLicenseResult = interface;
  PIStoreCanAcquireLicenseResult = ^IStoreCanAcquireLicenseResult;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreImage>
  IIterator_1__IStoreImage = interface;
  PIIterator_1__IStoreImage = ^IIterator_1__IStoreImage;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreImage>
  IIterable_1__IStoreImage = interface;
  PIIterable_1__IStoreImage = ^IIterable_1__IStoreImage;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreVideo>
  IIterator_1__IStoreVideo = interface;
  PIIterator_1__IStoreVideo = ^IIterator_1__IStoreVideo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreVideo>
  IIterable_1__IStoreVideo = interface;
  PIIterable_1__IStoreVideo = ^IIterable_1__IStoreVideo;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreAvailability>
  IIterator_1__IStoreAvailability = interface;
  PIIterator_1__IStoreAvailability = ^IIterator_1__IStoreAvailability;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreAvailability>
  IIterable_1__IStoreAvailability = interface;
  PIIterable_1__IStoreAvailability = ^IIterable_1__IStoreAvailability;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreSku>
  IIterator_1__IStoreSku = interface;
  PIIterator_1__IStoreSku = ^IIterator_1__IStoreSku;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreSku>
  IIterable_1__IStoreSku = interface;
  PIIterable_1__IStoreSku = ^IIterable_1__IStoreSku;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>
  IKeyValuePair_2__HSTRING__IStoreProduct = interface;
  PIKeyValuePair_2__HSTRING__IStoreProduct = ^IKeyValuePair_2__HSTRING__IStoreProduct;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>>
  IIterator_1__IKeyValuePair_2__HSTRING__IStoreProduct = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IStoreProduct = ^IIterator_1__IKeyValuePair_2__HSTRING__IStoreProduct;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>>
  IIterable_1__IKeyValuePair_2__HSTRING__IStoreProduct = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IStoreProduct = ^IIterable_1__IKeyValuePair_2__HSTRING__IStoreProduct;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.StorePackageUpdateStatus>
  IIterator_1__StorePackageUpdateStatus = interface;
  PIIterator_1__StorePackageUpdateStatus = ^IIterator_1__StorePackageUpdateStatus;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.StorePackageUpdateStatus>
  IIterable_1__StorePackageUpdateStatus = interface;
  PIIterable_1__StorePackageUpdateStatus = ^IIterable_1__StorePackageUpdateStatus;

  // Windows.Services.Store.IStoreContext2
  IStoreContext2 = interface;
  PIStoreContext2 = ^IStoreContext2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreCanAcquireLicenseResult>
  AsyncOperationCompletedHandler_1__IStoreCanAcquireLicenseResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreCanAcquireLicenseResult = ^AsyncOperationCompletedHandler_1__IStoreCanAcquireLicenseResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreCanAcquireLicenseResult>
  IAsyncOperation_1__IStoreCanAcquireLicenseResult = interface;
  PIAsyncOperation_1__IStoreCanAcquireLicenseResult = ^IAsyncOperation_1__IStoreCanAcquireLicenseResult;

  // Windows.Services.Store.IStoreProductOptions
  IStoreProductOptions = interface;
  PIStoreProductOptions = ^IStoreProductOptions;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreQueueItem>
  IIterator_1__IStoreQueueItem = interface;
  PIIterator_1__IStoreQueueItem = ^IIterator_1__IStoreQueueItem;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreQueueItem>
  IIterable_1__IStoreQueueItem = interface;
  PIIterable_1__IStoreQueueItem = ^IIterable_1__IStoreQueueItem;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreQueueItem>
  IVectorView_1__IStoreQueueItem = interface;
  PIVectorView_1__IStoreQueueItem = ^IVectorView_1__IStoreQueueItem;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreQueueItem>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStoreQueueItem = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStoreQueueItem = ^AsyncOperationCompletedHandler_1__IVectorView_1__IStoreQueueItem;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreQueueItem>>
  IAsyncOperation_1__IVectorView_1__IStoreQueueItem = interface;
  PIAsyncOperation_1__IVectorView_1__IStoreQueueItem = ^IAsyncOperation_1__IVectorView_1__IStoreQueueItem;

  // Windows.Services.Store.IStorePackageInstallOptions
  IStorePackageInstallOptions = interface;
  PIStorePackageInstallOptions = ^IStorePackageInstallOptions;

  // Windows.Services.Store.IStoreUninstallStorePackageResult
  IStoreUninstallStorePackageResult = interface;
  PIStoreUninstallStorePackageResult = ^IStoreUninstallStorePackageResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreUninstallStorePackageResult>
  AsyncOperationCompletedHandler_1__IStoreUninstallStorePackageResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreUninstallStorePackageResult = ^AsyncOperationCompletedHandler_1__IStoreUninstallStorePackageResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreUninstallStorePackageResult>
  IAsyncOperation_1__IStoreUninstallStorePackageResult = interface;
  PIAsyncOperation_1__IStoreUninstallStorePackageResult = ^IAsyncOperation_1__IStoreUninstallStorePackageResult;

  // Windows.Services.Store.IStoreContext3
  IStoreContext3 = interface;
  PIStoreContext3 = ^IStoreContext3;

  // Windows.Services.Store.IStoreRateAndReviewResult
  IStoreRateAndReviewResult = interface;
  PIStoreRateAndReviewResult = ^IStoreRateAndReviewResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreRateAndReviewResult>
  AsyncOperationCompletedHandler_1__IStoreRateAndReviewResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreRateAndReviewResult = ^AsyncOperationCompletedHandler_1__IStoreRateAndReviewResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreRateAndReviewResult>
  IAsyncOperation_1__IStoreRateAndReviewResult = interface;
  PIAsyncOperation_1__IStoreRateAndReviewResult = ^IAsyncOperation_1__IStoreRateAndReviewResult;

  // Windows.Services.Store.IStoreContext4
  IStoreContext4 = interface;
  PIStoreContext4 = ^IStoreContext4;

  // Windows.Services.Store.IStoreContextStatics
  IStoreContextStatics = interface;
  PIStoreContextStatics = ^IStoreContextStatics;

  // Windows.Services.Store Enums

  // Windows.Services.Store.StoreCanLicenseStatus
  StoreCanLicenseStatus = (
    NotLicensableToUser = 0,
    Licensable = 1,
    LicenseActionNotApplicableToProduct = 2,
    NetworkError = 3,
    ServerError = 4
  );
  PStoreCanLicenseStatus = ^StoreCanLicenseStatus;

  // Windows.Services.Store.StoreRateAndReviewStatus
  StoreRateAndReviewStatus = (
    Succeeded = 0,
    CanceledByUser = 1,
    NetworkError = 2,
    Error = 3
  );
  PStoreRateAndReviewStatus = ^StoreRateAndReviewStatus;

  // Windows.Services.Store.StoreUninstallStorePackageStatus
  StoreUninstallStorePackageStatus = (
    Succeeded = 0,
    CanceledByUser = 1,
    NetworkError = 2,
    UninstallNotApplicable = 3,
    Error = 4
  );
  PStoreUninstallStorePackageStatus = ^StoreUninstallStorePackageStatus;

  // Windows.Services.Store Records
  // Windows.Services.Store.StoreContract
  StoreContract = record
  end;
  PStoreContract = ^StoreContract;

  // Windows.Services.Store Interfaces

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>
  IKeyValuePair_2__HSTRING__IStoreLicense = interface(IInspectable)
  ['{CEB25B50-83EF-5774-B16E-4BB96C6E4C65}']
    function get_Key: HSTRING; safecall;
    function get_Value: IStoreLicense; safecall;
    property Key: HSTRING read get_Key;
    property Value: IStoreLicense read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>>
  IIterator_1__IKeyValuePair_2__HSTRING__IStoreLicense_Base = interface(IInspectable)
  ['{A1509348-6522-5062-AE86-CF595475926D}']
    function get_Current: IKeyValuePair_2__HSTRING__IStoreLicense; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IStoreLicense): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IStoreLicense read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>>
  IIterator_1__IKeyValuePair_2__HSTRING__IStoreLicense = interface(IIterator_1__IKeyValuePair_2__HSTRING__IStoreLicense_Base)
  ['{204D492F-1E5B-50CD-AF8A-CEADCE5B568D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>>
  IIterable_1__IKeyValuePair_2__HSTRING__IStoreLicense_Base = interface(IInspectable)
  ['{CA8BA445-6F4D-5DA9-95EE-42CF118DEF63}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IStoreLicense; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreLicense>>
  IIterable_1__IKeyValuePair_2__HSTRING__IStoreLicense = interface(IIterable_1__IKeyValuePair_2__HSTRING__IStoreLicense_Base)
  ['{2641BCF5-1955-560D-9665-96894F3EB16C}']
  end;

  // UsedAPI Interface
  // Windows.Services.Store.IStoreCanAcquireLicenseResult
  IStoreCanAcquireLicenseResult = interface(IInspectable)
  ['{3A693DB3-0088-482F-86D5-BD46522663AD}']
    function get_ExtendedError: HRESULT; safecall;
    function get_LicensableSku: HSTRING; safecall;
    function get_Status: StoreCanLicenseStatus; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property LicensableSku: HSTRING read get_LicensableSku;
    property Status: StoreCanLicenseStatus read get_Status;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreImage>
  IIterator_1__IStoreImage_Base = interface(IInspectable)
  ['{FBE076F6-C3D2-5DF7-839F-012AC0F951C5}']
    function get_Current: IStoreImage; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStoreImage): Cardinal; safecall;
    property Current: IStoreImage read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreImage>
  IIterator_1__IStoreImage = interface(IIterator_1__IStoreImage_Base)
  ['{C172A6FB-2505-5706-BCB1-9D75E14AC85C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreImage>
  IIterable_1__IStoreImage_Base = interface(IInspectable)
  ['{B2DA6DE8-AD55-56CE-8754-2C96F4FE1C2E}']
    function First: IIterator_1__IStoreImage; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreImage>
  IIterable_1__IStoreImage = interface(IIterable_1__IStoreImage_Base)
  ['{01D22578-8616-5728-A0FF-7E486F3A9C4D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreVideo>
  IIterator_1__IStoreVideo_Base = interface(IInspectable)
  ['{91B0B554-A537-5E22-B39F-6A935D0BEF45}']
    function get_Current: IStoreVideo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStoreVideo): Cardinal; safecall;
    property Current: IStoreVideo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreVideo>
  IIterator_1__IStoreVideo = interface(IIterator_1__IStoreVideo_Base)
  ['{8AAEEAB6-7651-5658-8601-6553F357697F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreVideo>
  IIterable_1__IStoreVideo_Base = interface(IInspectable)
  ['{46BE61E4-7173-565E-AED5-4A2152F1CE69}']
    function First: IIterator_1__IStoreVideo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreVideo>
  IIterable_1__IStoreVideo = interface(IIterable_1__IStoreVideo_Base)
  ['{9D74B863-34A8-5D61-8782-4F2AB3528D08}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreAvailability>
  IIterator_1__IStoreAvailability_Base = interface(IInspectable)
  ['{50511CCB-089E-5B73-BB4A-4D1EF77B8F0F}']
    function get_Current: IStoreAvailability; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStoreAvailability): Cardinal; safecall;
    property Current: IStoreAvailability read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreAvailability>
  IIterator_1__IStoreAvailability = interface(IIterator_1__IStoreAvailability_Base)
  ['{381E2DFF-80C3-5CC8-AD28-2C0F87720026}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreAvailability>
  IIterable_1__IStoreAvailability_Base = interface(IInspectable)
  ['{C58DE1A0-25DE-578B-BB69-E0A26D67B203}']
    function First: IIterator_1__IStoreAvailability; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreAvailability>
  IIterable_1__IStoreAvailability = interface(IIterable_1__IStoreAvailability_Base)
  ['{9EA2B48F-FF9D-5850-876D-CEE55741FA56}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreSku>
  IIterator_1__IStoreSku_Base = interface(IInspectable)
  ['{365CDE92-7FE3-59D5-B8F8-8F05ACF50947}']
    function get_Current: IStoreSku; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStoreSku): Cardinal; safecall;
    property Current: IStoreSku read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreSku>
  IIterator_1__IStoreSku = interface(IIterator_1__IStoreSku_Base)
  ['{85F25C19-9E41-5A04-8324-E7B0F04595F0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreSku>
  IIterable_1__IStoreSku_Base = interface(IInspectable)
  ['{522D34EF-4B5A-5B44-A046-7A16051D011E}']
    function First: IIterator_1__IStoreSku; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreSku>
  IIterable_1__IStoreSku = interface(IIterable_1__IStoreSku_Base)
  ['{0A9BDF4A-25BF-5C9C-A798-121849885E96}']
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>
  IKeyValuePair_2__HSTRING__IStoreProduct = interface(IInspectable)
  ['{7649AE79-029E-51E4-AE99-7E3AB8D9EC53}']
    function get_Key: HSTRING; safecall;
    function get_Value: IStoreProduct; safecall;
    property Key: HSTRING read get_Key;
    property Value: IStoreProduct read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>>
  IIterator_1__IKeyValuePair_2__HSTRING__IStoreProduct_Base = interface(IInspectable)
  ['{0EDB142C-9D04-532B-81E9-4C84AB09B34B}']
    function get_Current: IKeyValuePair_2__HSTRING__IStoreProduct; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IStoreProduct): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IStoreProduct read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>>
  IIterator_1__IKeyValuePair_2__HSTRING__IStoreProduct = interface(IIterator_1__IKeyValuePair_2__HSTRING__IStoreProduct_Base)
  ['{9EDA8AB5-7E61-5DF9-9D15-E8AED01A7EB6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>>
  IIterable_1__IKeyValuePair_2__HSTRING__IStoreProduct_Base = interface(IInspectable)
  ['{78A33722-ABFB-57C0-853F-5616A3AB8D57}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IStoreProduct; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.Store.IStoreProduct>>
  IIterable_1__IKeyValuePair_2__HSTRING__IStoreProduct = interface(IIterable_1__IKeyValuePair_2__HSTRING__IStoreProduct_Base)
  ['{F0188E6C-4198-535A-9600-DF68157D5718}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.StorePackageUpdateStatus>
  IIterator_1__StorePackageUpdateStatus_Base = interface(IInspectable)
  ['{6181D84F-F731-57B8-9A6B-8A12FCD58D04}']
    function get_Current: StorePackageUpdateStatus; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PStorePackageUpdateStatus): Cardinal; safecall;
    property Current: StorePackageUpdateStatus read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.StorePackageUpdateStatus>
  IIterator_1__StorePackageUpdateStatus = interface(IIterator_1__StorePackageUpdateStatus_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.StorePackageUpdateStatus>
  IIterable_1__StorePackageUpdateStatus_Base = interface(IInspectable)
  ['{60832223-7EB4-5CD7-8340-F5077173D364}']
    function First: IIterator_1__StorePackageUpdateStatus; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.StorePackageUpdateStatus>
  IIterable_1__StorePackageUpdateStatus = interface(IIterable_1__StorePackageUpdateStatus_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.Store.IStoreContext2
  IStoreContext2 = interface(IInspectable)
  ['{18BC54DA-7BD9-452C-9116-3BBD06FFC63A}']
    function FindStoreProductForPackageAsync(productKinds: IIterable_1__HSTRING; package: IPackage): IAsyncOperation_1__IStoreProductResult; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreCanAcquireLicenseResult>
  AsyncOperationCompletedHandler_1__IStoreCanAcquireLicenseResult = interface(IUnknown)
  ['{F1D1367C-D253-574E-839F-88CBDD6139E7}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreCanAcquireLicenseResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreCanAcquireLicenseResult>
  IAsyncOperation_1__IStoreCanAcquireLicenseResult = interface(IInspectable)
  ['{7E65037C-8A35-52DC-986E-9B252362F0A4}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreCanAcquireLicenseResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreCanAcquireLicenseResult; safecall;
    function GetResults: IStoreCanAcquireLicenseResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreCanAcquireLicenseResult read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.Services.Store.IStoreProductOptions
  IStoreProductOptions = interface(IInspectable)
  ['{5B34A0F9-A113-4811-8326-16199C927F31}']
    function get_ActionFilters: IVector_1__HSTRING; safecall;
    property ActionFilters: IVector_1__HSTRING read get_ActionFilters;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStoreQueueItem>
  IIterator_1__IStoreQueueItem = interface(IInspectable)
  ['{615A5179-EEF6-5B2A-AD8E-AD4E717F039F}']
    function get_Current: IStoreQueueItem; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStoreQueueItem): Cardinal; safecall;
    property Current: IStoreQueueItem read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStoreQueueItem>
  IIterable_1__IStoreQueueItem = interface(IInspectable)
  ['{9021967E-4077-5C9C-B98D-C75265FAB86A}']
    function First: IIterator_1__IStoreQueueItem; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreQueueItem>
  IVectorView_1__IStoreQueueItem = interface(IInspectable)
  ['{C5D060F0-58CE-51DB-98C4-C9B6608CE6AB}']
    function GetAt(index: Cardinal): IStoreQueueItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStoreQueueItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStoreQueueItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreQueueItem>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStoreQueueItem = interface(IUnknown)
  ['{4AF9988F-3D25-5C3B-9B5E-562DB60C3D18}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IStoreQueueItem; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreQueueItem>>
  IAsyncOperation_1__IVectorView_1__IStoreQueueItem = interface(IInspectable)
  ['{15EC24DE-523C-581D-B78C-FC0B89DB34AD}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IStoreQueueItem); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStoreQueueItem; safecall;
    function GetResults: IVectorView_1__IStoreQueueItem; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStoreQueueItem read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.Services.Store.IStorePackageInstallOptions
  IStorePackageInstallOptions = interface(IInspectable)
  ['{1D3D630C-0CCD-44DD-8C59-80810A729973}']
    function get_AllowForcedAppRestart: Boolean; safecall;
    procedure put_AllowForcedAppRestart(value: Boolean); safecall;
    property AllowForcedAppRestart: Boolean read get_AllowForcedAppRestart write put_AllowForcedAppRestart;
  end;

  // UsedAPI Interface
  // Windows.Services.Store.IStoreUninstallStorePackageResult
  IStoreUninstallStorePackageResult = interface(IInspectable)
  ['{9FCA39FD-126F-4CDA-B801-1346B8D0A260}']
    function get_ExtendedError: HRESULT; safecall;
    function get_Status: StoreUninstallStorePackageStatus; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Status: StoreUninstallStorePackageStatus read get_Status;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreUninstallStorePackageResult>
  AsyncOperationCompletedHandler_1__IStoreUninstallStorePackageResult = interface(IUnknown)
  ['{D8F71ABF-74CB-5A01-A9F5-D3F0157191DA}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreUninstallStorePackageResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreUninstallStorePackageResult>
  IAsyncOperation_1__IStoreUninstallStorePackageResult = interface(IInspectable)
  ['{1E895215-D26E-5CF7-B2B4-B551EDF273A9}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreUninstallStorePackageResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreUninstallStorePackageResult; safecall;
    function GetResults: IStoreUninstallStorePackageResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreUninstallStorePackageResult read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.Store.IStoreContext3
  IStoreContext3 = interface(IInspectable)
  ['{E26226CA-1A01-4730-85A6-ECC896E4AE38}']
    function get_CanSilentlyDownloadStorePackageUpdates: Boolean; safecall;
    function TrySilentDownloadStorePackageUpdatesAsync(storePackageUpdates: IIterable_1__IStorePackageUpdate): IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    function TrySilentDownloadAndInstallStorePackageUpdatesAsync(storePackageUpdates: IIterable_1__IStorePackageUpdate): IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    function CanAcquireStoreLicenseForOptionalPackageAsync(optionalPackage: IPackage): IAsyncOperation_1__IStoreCanAcquireLicenseResult; safecall;
    function CanAcquireStoreLicenseAsync(productStoreId: HSTRING): IAsyncOperation_1__IStoreCanAcquireLicenseResult; safecall;
    function GetStoreProductsAsync(productKinds: IIterable_1__HSTRING; storeIds: IIterable_1__HSTRING; storeProductOptions: IStoreProductOptions): IAsyncOperation_1__IStoreProductQueryResult; safecall;
    function GetAssociatedStoreQueueItemsAsync: IAsyncOperation_1__IVectorView_1__IStoreQueueItem; safecall;
    function GetStoreQueueItemsAsync(storeIds: IIterable_1__HSTRING): IAsyncOperation_1__IVectorView_1__IStoreQueueItem; safecall;
    function RequestDownloadAndInstallStorePackagesAsync(storeIds: IIterable_1__HSTRING; storePackageInstallOptions: IStorePackageInstallOptions): IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    function DownloadAndInstallStorePackagesAsync(storeIds: IIterable_1__HSTRING): IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    function RequestUninstallStorePackageAsync(package: IPackage): IAsyncOperation_1__IStoreUninstallStorePackageResult; safecall;
    function RequestUninstallStorePackageByStoreIdAsync(storeId: HSTRING): IAsyncOperation_1__IStoreUninstallStorePackageResult; safecall;
    function UninstallStorePackageAsync(package: IPackage): IAsyncOperation_1__IStoreUninstallStorePackageResult; safecall;
    function UninstallStorePackageByStoreIdAsync(storeId: HSTRING): IAsyncOperation_1__IStoreUninstallStorePackageResult; safecall;
    property CanSilentlyDownloadStorePackageUpdates: Boolean read get_CanSilentlyDownloadStorePackageUpdates;
  end;

  // UsedAPI Interface
  // Windows.Services.Store.IStoreRateAndReviewResult
  IStoreRateAndReviewResult = interface(IInspectable)
  ['{9D209D56-A6B5-4121-9B61-EE6D0FBDBDBB}']
    function get_ExtendedError: HRESULT; safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    function get_WasUpdated: Boolean; safecall;
    function get_Status: StoreRateAndReviewStatus; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData;
    property Status: StoreRateAndReviewStatus read get_Status;
    property WasUpdated: Boolean read get_WasUpdated;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreRateAndReviewResult>
  AsyncOperationCompletedHandler_1__IStoreRateAndReviewResult = interface(IUnknown)
  ['{CEE3E1DC-378D-52F3-964E-1168D2D5A581}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreRateAndReviewResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreRateAndReviewResult>
  IAsyncOperation_1__IStoreRateAndReviewResult = interface(IInspectable)
  ['{961500DC-A2D0-5D4F-9074-B7C41909B0BB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreRateAndReviewResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreRateAndReviewResult; safecall;
    function GetResults: IStoreRateAndReviewResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreRateAndReviewResult read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.Store.IStoreContext4
  IStoreContext4 = interface(IInspectable)
  ['{AF9C6F69-BEA1-4BF4-8E74-AE03E206C6B0}']
    function RequestRateAndReviewAppAsync: IAsyncOperation_1__IStoreRateAndReviewResult; safecall;
    function SetInstallOrderForAssociatedStoreQueueItemsAsync(items: IIterable_1__IStoreQueueItem): IAsyncOperation_1__IVectorView_1__IStoreQueueItem; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.Store.IStoreContextStatics
  [WinRTClassNameAttribute(SWindows_Services_Store_StoreContext)]
  IStoreContextStatics = interface(IInspectable)
  ['{9C06EE5F-15C0-4E72-9330-D6191CEBD19C}']
    function GetDefault: IStoreContext; safecall;
    function GetForUser(user: IUser): IStoreContext; safecall;
  end;

  // Windows.Services.Store.StoreContext
  // Explicitly imported
  // Implements: Windows.Services.Store.IStoreContext
  // Implements: Windows.Services.Store.IStoreContext2
  // Implements: Windows.Services.Store.IStoreContext3
  // Implements: Windows.Services.Store.IStoreContext4
  // Statics: "Windows.Services.Store.IStoreContextStatics"
  TStoreContext = class(TWinRTGenericImportS<IStoreContextStatics>)
  public
    // -> IStoreContextStatics
    class function GetDefault: IStoreContext; static; inline;
    class function GetForUser(user: IUser): IStoreContext; static; inline;
  end;

implementation

{ TStoreContext }

class function TStoreContext.GetDefault: IStoreContext;
begin
  Result := Statics.GetDefault;
end;

class function TStoreContext.GetForUser(user: IUser): IStoreContext;
begin
  Result := Statics.GetForUser(user);
end;


end.
