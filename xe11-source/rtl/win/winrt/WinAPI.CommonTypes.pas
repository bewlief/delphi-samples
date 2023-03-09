{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.CommonTypes;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IPropertySet>
  IAsyncOperation_1__IPropertySet = interface;
  PIAsyncOperation_1__IPropertySet = ^IAsyncOperation_1__IPropertySet;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IPropertySet>
  AsyncOperationCompletedHandler_1__IPropertySet = interface;
  PAsyncOperationCompletedHandler_1__IPropertySet = ^AsyncOperationCompletedHandler_1__IPropertySet;

  // External: Windows.Foundation.Collections.IPropertySet
  IPropertySet = interface;
  PIPropertySet = ^IPropertySet;

  // External: Windows.Foundation.IDeferral
  IDeferral = interface;
  PIDeferral = ^IDeferral;

  // External: Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface;
  PIAsyncOperation_1__Boolean = ^IAsyncOperation_1__Boolean;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface;
  PAsyncOperationCompletedHandler_1__Boolean = ^AsyncOperationCompletedHandler_1__Boolean;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = interface;
  PIAsyncOperationWithProgress_2__Cardinal__Cardinal = ^IAsyncOperationWithProgress_2__Cardinal__Cardinal;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationProgressHandler_2__Cardinal__Cardinal = ^AsyncOperationProgressHandler_2__Cardinal__Cardinal;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal;

  // External: Windows.Foundation.Collections.IKeyValuePair`2<String,Object>
  IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIKeyValuePair_2__HSTRING__IInspectable = ^IKeyValuePair_2__HSTRING__IInspectable;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IInspectable = ^IIterator_1__IKeyValuePair_2__HSTRING__IInspectable;

  // External: Windows.Foundation.Collections.IMapView`2<String,Object>
  IMapView_2__HSTRING__IInspectable = interface;
  PIMapView_2__HSTRING__IInspectable = ^IMapView_2__HSTRING__IInspectable;

  // External: Windows.Foundation.Collections.IObservableMap`2<String,Object>
  IObservableMap_2__HSTRING__IInspectable = interface;
  PIObservableMap_2__HSTRING__IInspectable = ^IObservableMap_2__HSTRING__IInspectable;

  // External: Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable = interface;
  PMapChangedEventHandler_2__HSTRING__IInspectable = ^MapChangedEventHandler_2__HSTRING__IInspectable;

  // External: Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  IMapChangedEventArgs_1__HSTRING = interface;
  PIMapChangedEventArgs_1__HSTRING = ^IMapChangedEventArgs_1__HSTRING;

  // External: Windows.Foundation.IAsyncAction
  IAsyncAction = interface;
  PIAsyncAction = ^IAsyncAction;

  // External: Windows.Foundation.AsyncActionCompletedHandler
  AsyncActionCompletedHandler = interface;
  PAsyncActionCompletedHandler = ^AsyncActionCompletedHandler;

  // External: Windows.Foundation.IReference`1<Windows.Foundation.Point>
  IReference_1__Point = interface;
  PIReference_1__Point = ^IReference_1__Point;

  // External: Windows.Foundation.IReference`1<Windows.Foundation.Rect>
  IReference_1__Rect = interface;
  PIReference_1__Rect = ^IReference_1__Rect;

  // External: Windows.Foundation.IUriRuntimeClass
  IUriRuntimeClass = interface;
  PIUriRuntimeClass = ^IUriRuntimeClass;

  // External: Windows.Foundation.IWwwFormUrlDecoderRuntimeClass
  IWwwFormUrlDecoderRuntimeClass = interface;
  PIWwwFormUrlDecoderRuntimeClass = ^IWwwFormUrlDecoderRuntimeClass;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IUriRuntimeClass>
  IAsyncOperation_1__IUriRuntimeClass = interface;
  PIAsyncOperation_1__IUriRuntimeClass = ^IAsyncOperation_1__IUriRuntimeClass;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IUriRuntimeClass>
  AsyncOperationCompletedHandler_1__IUriRuntimeClass = interface;
  PAsyncOperationCompletedHandler_1__IUriRuntimeClass = ^AsyncOperationCompletedHandler_1__IUriRuntimeClass;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStorePackageLicense,Object>
  TypedEventHandler_2__IStorePackageLicense__IInspectable = interface;
  PTypedEventHandler_2__IStorePackageLicense__IInspectable = ^TypedEventHandler_2__IStorePackageLicense__IInspectable;

  // External: Windows.Services.Store.IStorePackageLicense
  IStorePackageLicense = interface;
  PIStorePackageLicense = ^IStorePackageLicense;

  // External: Windows.ApplicationModel.IPackage
  IPackage = interface;
  PIPackage = ^IPackage;

  // External: Windows.ApplicationModel.IPackageId
  IPackageId = interface;
  PIPackageId = ^IPackageId;

  // External: Windows.Storage.IStorageFolder
  IStorageFolder = interface;
  PIStorageFolder = ^IStorageFolder;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFile>
  IAsyncOperation_1__IStorageFile = interface;
  PIAsyncOperation_1__IStorageFile = ^IAsyncOperation_1__IStorageFile;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageFile>
  AsyncOperationCompletedHandler_1__IStorageFile = interface;
  PAsyncOperationCompletedHandler_1__IStorageFile = ^AsyncOperationCompletedHandler_1__IStorageFile;

  // External: Windows.Storage.IStorageFile
  IStorageFile = interface;
  PIStorageFile = ^IStorageFile;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStream>
  IAsyncOperation_1__IRandomAccessStream = interface;
  PIAsyncOperation_1__IRandomAccessStream = ^IAsyncOperation_1__IRandomAccessStream;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStream>
  AsyncOperationCompletedHandler_1__IRandomAccessStream = interface;
  PAsyncOperationCompletedHandler_1__IRandomAccessStream = ^AsyncOperationCompletedHandler_1__IRandomAccessStream;

  // External: Windows.Storage.Streams.IRandomAccessStream
  IRandomAccessStream = interface;
  PIRandomAccessStream = ^IRandomAccessStream;

  // External: Windows.Storage.Streams.IInputStream
  IInputStream = interface;
  PIInputStream = ^IInputStream;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface;
  PIAsyncOperationWithProgress_2__IBuffer__Cardinal = ^IAsyncOperationWithProgress_2__IBuffer__Cardinal;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationProgressHandler_2__IBuffer__Cardinal = ^AsyncOperationProgressHandler_2__IBuffer__Cardinal;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal;

  // External: Windows.Storage.Streams.IBuffer
  IBuffer = interface;
  PIBuffer = ^IBuffer;

  // External: Windows.Storage.Streams.IOutputStream
  IOutputStream = interface;
  PIOutputStream = ^IOutputStream;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageStreamTransaction>
  IAsyncOperation_1__IStorageStreamTransaction = interface;
  PIAsyncOperation_1__IStorageStreamTransaction = ^IAsyncOperation_1__IStorageStreamTransaction;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageStreamTransaction>
  AsyncOperationCompletedHandler_1__IStorageStreamTransaction = interface;
  PAsyncOperationCompletedHandler_1__IStorageStreamTransaction = ^AsyncOperationCompletedHandler_1__IStorageStreamTransaction;

  // External: Windows.Storage.IStorageStreamTransaction
  IStorageStreamTransaction = interface;
  PIStorageStreamTransaction = ^IStorageStreamTransaction;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFolder>
  IAsyncOperation_1__IStorageFolder = interface;
  PIAsyncOperation_1__IStorageFolder = ^IAsyncOperation_1__IStorageFolder;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageFolder>
  AsyncOperationCompletedHandler_1__IStorageFolder = interface;
  PAsyncOperationCompletedHandler_1__IStorageFolder = ^AsyncOperationCompletedHandler_1__IStorageFolder;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageItem>
  IAsyncOperation_1__IStorageItem = interface;
  PIAsyncOperation_1__IStorageItem = ^IAsyncOperation_1__IStorageItem;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageItem>
  AsyncOperationCompletedHandler_1__IStorageItem = interface;
  PAsyncOperationCompletedHandler_1__IStorageItem = ^AsyncOperationCompletedHandler_1__IStorageItem;

  // External: Windows.Storage.IStorageItem
  IStorageItem = interface;
  PIStorageItem = ^IStorageItem;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IBasicProperties>
  IAsyncOperation_1__FileProperties_IBasicProperties = interface;
  PIAsyncOperation_1__FileProperties_IBasicProperties = ^IAsyncOperation_1__FileProperties_IBasicProperties;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IBasicProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties = interface;
  PAsyncOperationCompletedHandler_1__FileProperties_IBasicProperties = ^AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties;

  // External: Windows.Storage.FileProperties.IBasicProperties
  FileProperties_IBasicProperties = interface;
  PFileProperties_IBasicProperties = ^FileProperties_IBasicProperties;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>>
  IAsyncOperation_1__IVectorView_1__IStorageFile = interface;
  PIAsyncOperation_1__IVectorView_1__IStorageFile = ^IAsyncOperation_1__IVectorView_1__IStorageFile;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile = ^AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>
  IVectorView_1__IStorageFile = interface;
  PIVectorView_1__IStorageFile = ^IVectorView_1__IStorageFile;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>>
  IAsyncOperation_1__IVectorView_1__IStorageFolder = interface;
  PIAsyncOperation_1__IVectorView_1__IStorageFolder = ^IAsyncOperation_1__IVectorView_1__IStorageFolder;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder = ^AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>
  IVectorView_1__IStorageFolder = interface;
  PIVectorView_1__IStorageFolder = ^IVectorView_1__IStorageFolder;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>>
  IAsyncOperation_1__IVectorView_1__IStorageItem = interface;
  PIAsyncOperation_1__IVectorView_1__IStorageItem = ^IAsyncOperation_1__IVectorView_1__IStorageItem;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem = ^AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>
  IVectorView_1__IStorageItem = interface;
  PIVectorView_1__IStorageItem = ^IVectorView_1__IStorageItem;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IPackage>
  IVectorView_1__IPackage = interface;
  PIVectorView_1__IPackage = ^IVectorView_1__IPackage;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStoreContext,Object>
  TypedEventHandler_2__IStoreContext__IInspectable = interface;
  PTypedEventHandler_2__IStoreContext__IInspectable = ^TypedEventHandler_2__IStoreContext__IInspectable;

  // External: Windows.Services.Store.IStoreContext
  IStoreContext = interface;
  PIStoreContext = ^IStoreContext;

  // External: Windows.System.IUser
  IUser = interface;
  PIUser = ^IUser;

  // External: Windows.Foundation.IAsyncOperation`1<Object>
  IAsyncOperation_1__IInspectable = interface;
  PIAsyncOperation_1__IInspectable = ^IAsyncOperation_1__IInspectable;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  AsyncOperationCompletedHandler_1__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IInspectable = ^AsyncOperationCompletedHandler_1__IInspectable;

  // External: Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface;
  PIVectorView_1__HSTRING = ^IVectorView_1__HSTRING;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IAsyncOperation_1__IRandomAccessStreamReference = interface;
  PIAsyncOperation_1__IRandomAccessStreamReference = ^IAsyncOperation_1__IRandomAccessStreamReference;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  AsyncOperationCompletedHandler_1__IRandomAccessStreamReference = interface;
  PAsyncOperationCompletedHandler_1__IRandomAccessStreamReference = ^AsyncOperationCompletedHandler_1__IRandomAccessStreamReference;

  // External: Windows.Storage.Streams.IRandomAccessStreamReference
  IRandomAccessStreamReference = interface;
  PIRandomAccessStreamReference = ^IRandomAccessStreamReference;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IAsyncOperation_1__IRandomAccessStreamWithContentType = interface;
  PIAsyncOperation_1__IRandomAccessStreamWithContentType = ^IAsyncOperation_1__IRandomAccessStreamWithContentType;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType = interface;
  PAsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType = ^AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType;

  // External: Windows.Storage.Streams.IRandomAccessStreamWithContentType
  IRandomAccessStreamWithContentType = interface;
  PIRandomAccessStreamWithContentType = ^IRandomAccessStreamWithContentType;

  // External: Windows.Foundation.IAsyncOperation`1<String>
  IAsyncOperation_1__HSTRING = interface;
  PIAsyncOperation_1__HSTRING = ^IAsyncOperation_1__HSTRING;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  AsyncOperationCompletedHandler_1__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__HSTRING = ^AsyncOperationCompletedHandler_1__HSTRING;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreAppLicense>
  IAsyncOperation_1__IStoreAppLicense = interface;
  PIAsyncOperation_1__IStoreAppLicense = ^IAsyncOperation_1__IStoreAppLicense;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreAppLicense>
  AsyncOperationCompletedHandler_1__IStoreAppLicense = interface;
  PAsyncOperationCompletedHandler_1__IStoreAppLicense = ^AsyncOperationCompletedHandler_1__IStoreAppLicense;

  // External: Windows.Services.Store.IStoreAppLicense
  IStoreAppLicense = interface;
  PIStoreAppLicense = ^IStoreAppLicense;

  // External: Windows.Foundation.Collections.IMapView`2<String,Windows.Services.Store.IStoreLicense>
  IMapView_2__HSTRING__IStoreLicense = interface;
  PIMapView_2__HSTRING__IStoreLicense = ^IMapView_2__HSTRING__IStoreLicense;

  // External: Windows.Services.Store.IStoreLicense
  IStoreLicense = interface;
  PIStoreLicense = ^IStoreLicense;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductResult>
  IAsyncOperation_1__IStoreProductResult = interface;
  PIAsyncOperation_1__IStoreProductResult = ^IAsyncOperation_1__IStoreProductResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductResult>
  AsyncOperationCompletedHandler_1__IStoreProductResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreProductResult = ^AsyncOperationCompletedHandler_1__IStoreProductResult;

  // External: Windows.Services.Store.IStoreProductResult
  IStoreProductResult = interface;
  PIStoreProductResult = ^IStoreProductResult;

  // External: Windows.Services.Store.IStoreProduct
  IStoreProduct = interface;
  PIStoreProduct = ^IStoreProduct;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreImage>
  IVectorView_1__IStoreImage = interface;
  PIVectorView_1__IStoreImage = ^IVectorView_1__IStoreImage;

  // External: Windows.Services.Store.IStoreImage
  IStoreImage = interface;
  PIStoreImage = ^IStoreImage;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreVideo>
  IVectorView_1__IStoreVideo = interface;
  PIVectorView_1__IStoreVideo = ^IVectorView_1__IStoreVideo;

  // External: Windows.Services.Store.IStoreVideo
  IStoreVideo = interface;
  PIStoreVideo = ^IStoreVideo;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreSku>
  IVectorView_1__IStoreSku = interface;
  PIVectorView_1__IStoreSku = ^IVectorView_1__IStoreSku;

  // External: Windows.Services.Store.IStoreSku
  IStoreSku = interface;
  PIStoreSku = ^IStoreSku;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreAvailability>
  IVectorView_1__IStoreAvailability = interface;
  PIVectorView_1__IStoreAvailability = ^IVectorView_1__IStoreAvailability;

  // External: Windows.Services.Store.IStoreAvailability
  IStoreAvailability = interface;
  PIStoreAvailability = ^IStoreAvailability;

  // External: Windows.Services.Store.IStorePrice
  IStorePrice = interface;
  PIStorePrice = ^IStorePrice;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStorePurchaseResult>
  IAsyncOperation_1__IStorePurchaseResult = interface;
  PIAsyncOperation_1__IStorePurchaseResult = ^IAsyncOperation_1__IStorePurchaseResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStorePurchaseResult>
  AsyncOperationCompletedHandler_1__IStorePurchaseResult = interface;
  PAsyncOperationCompletedHandler_1__IStorePurchaseResult = ^AsyncOperationCompletedHandler_1__IStorePurchaseResult;

  // External: Windows.Services.Store.IStorePurchaseResult
  IStorePurchaseResult = interface;
  PIStorePurchaseResult = ^IStorePurchaseResult;

  // External: Windows.Services.Store.IStorePurchaseProperties
  IStorePurchaseProperties = interface;
  PIStorePurchaseProperties = ^IStorePurchaseProperties;

  // External: Windows.Services.Store.IStoreCollectionData
  IStoreCollectionData = interface;
  PIStoreCollectionData = ^IStoreCollectionData;

  // External: Windows.Services.Store.IStoreSubscriptionInfo
  IStoreSubscriptionInfo = interface;
  PIStoreSubscriptionInfo = ^IStoreSubscriptionInfo;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductQueryResult>
  IAsyncOperation_1__IStoreProductQueryResult = interface;
  PIAsyncOperation_1__IStoreProductQueryResult = ^IAsyncOperation_1__IStoreProductQueryResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductQueryResult>
  AsyncOperationCompletedHandler_1__IStoreProductQueryResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreProductQueryResult = ^AsyncOperationCompletedHandler_1__IStoreProductQueryResult;

  // External: Windows.Services.Store.IStoreProductQueryResult
  IStoreProductQueryResult = interface;
  PIStoreProductQueryResult = ^IStoreProductQueryResult;

  // External: Windows.Foundation.Collections.IMapView`2<String,Windows.Services.Store.IStoreProduct>
  IMapView_2__HSTRING__IStoreProduct = interface;
  PIMapView_2__HSTRING__IStoreProduct = ^IMapView_2__HSTRING__IStoreProduct;

  // External: Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface;
  PIIterable_1__HSTRING = ^IIterable_1__HSTRING;

  // External: Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface;
  PIIterator_1__HSTRING = ^IIterator_1__HSTRING;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductPagedQueryResult>
  IAsyncOperation_1__IStoreProductPagedQueryResult = interface;
  PIAsyncOperation_1__IStoreProductPagedQueryResult = ^IAsyncOperation_1__IStoreProductPagedQueryResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductPagedQueryResult>
  AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult = ^AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult;

  // External: Windows.Services.Store.IStoreProductPagedQueryResult
  IStoreProductPagedQueryResult = interface;
  PIStoreProductPagedQueryResult = ^IStoreProductPagedQueryResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreConsumableResult>
  IAsyncOperation_1__IStoreConsumableResult = interface;
  PIAsyncOperation_1__IStoreConsumableResult = ^IAsyncOperation_1__IStoreConsumableResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreConsumableResult>
  AsyncOperationCompletedHandler_1__IStoreConsumableResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreConsumableResult = ^AsyncOperationCompletedHandler_1__IStoreConsumableResult;

  // External: Windows.Services.Store.IStoreConsumableResult
  IStoreConsumableResult = interface;
  PIStoreConsumableResult = ^IStoreConsumableResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreAcquireLicenseResult>
  IAsyncOperation_1__IStoreAcquireLicenseResult = interface;
  PIAsyncOperation_1__IStoreAcquireLicenseResult = ^IAsyncOperation_1__IStoreAcquireLicenseResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreAcquireLicenseResult>
  AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult = interface;
  PAsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult = ^AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult;

  // External: Windows.Services.Store.IStoreAcquireLicenseResult
  IStoreAcquireLicenseResult = interface;
  PIStoreAcquireLicenseResult = ^IStoreAcquireLicenseResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>>
  IAsyncOperation_1__IVectorView_1__IStorePackageUpdate = interface;
  PIAsyncOperation_1__IVectorView_1__IStorePackageUpdate = ^IAsyncOperation_1__IVectorView_1__IStorePackageUpdate;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate = ^AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>
  IVectorView_1__IStorePackageUpdate = interface;
  PIVectorView_1__IStorePackageUpdate = ^IVectorView_1__IStorePackageUpdate;

  // External: Windows.Services.Store.IStorePackageUpdate
  IStorePackageUpdate = interface;
  PIStorePackageUpdate = ^IStorePackageUpdate;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus = interface;
  PIAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus = ^IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = interface;
  PAsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = ^AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = ^AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus;

  // External: Windows.Services.Store.IStorePackageUpdateResult
  IStorePackageUpdateResult = interface;
  PIStorePackageUpdateResult = ^IStorePackageUpdateResult;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.StorePackageUpdateStatus>
  IVectorView_1__StorePackageUpdateStatus = interface;
  PIVectorView_1__StorePackageUpdateStatus = ^IVectorView_1__StorePackageUpdateStatus;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStorePackageUpdate>
  IIterable_1__IStorePackageUpdate = interface;
  PIIterable_1__IStorePackageUpdate = ^IIterable_1__IStorePackageUpdate;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStorePackageUpdate>
  IIterator_1__IStorePackageUpdate = interface;
  PIIterator_1__IStorePackageUpdate = ^IIterator_1__IStorePackageUpdate;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStoreQueueItem,Object>
  TypedEventHandler_2__IStoreQueueItem__IInspectable = interface;
  PTypedEventHandler_2__IStoreQueueItem__IInspectable = ^TypedEventHandler_2__IStoreQueueItem__IInspectable;

  // External: Windows.Services.Store.IStoreQueueItem
  IStoreQueueItem = interface;
  PIStoreQueueItem = ^IStoreQueueItem;

  // External: Windows.Services.Store.IStoreQueueItemStatus
  IStoreQueueItemStatus = interface;
  PIStoreQueueItemStatus = ^IStoreQueueItemStatus;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStoreQueueItem,Windows.Services.Store.IStoreQueueItemCompletedEventArgs>
  TypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs = interface;
  PTypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs = ^TypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs;

  // External: Windows.Services.Store.IStoreQueueItemCompletedEventArgs
  IStoreQueueItemCompletedEventArgs = interface;
  PIStoreQueueItemCompletedEventArgs = ^IStoreQueueItemCompletedEventArgs;

  // External: Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // External: Windows.Foundation.IAsyncOperation`1<UInt32>
  IAsyncOperation_1__Cardinal = interface;
  PIAsyncOperation_1__Cardinal = ^IAsyncOperation_1__Cardinal;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  AsyncOperationCompletedHandler_1__Cardinal = interface;
  PAsyncOperationCompletedHandler_1__Cardinal = ^AsyncOperationCompletedHandler_1__Cardinal;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Storage.Search.IStorageQueryResultBase,Object>
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = interface;
  PTypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = ^TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable;

  // External: Windows.Storage.Search.IStorageQueryResultBase
  Search_IStorageQueryResultBase = interface;
  PSearch_IStorageQueryResultBase = ^Search_IStorageQueryResultBase;

  // External: Windows.Storage.Search.IQueryOptions
  Search_IQueryOptions = interface;
  PSearch_IQueryOptions = ^Search_IQueryOptions;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Storage.Search.SortEntry>
  IVector_1__Search_SortEntry = interface;
  PIVector_1__Search_SortEntry = ^IVector_1__Search_SortEntry;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Search.SortEntry>
  IVectorView_1__Search_SortEntry = interface;
  PIVectorView_1__Search_SortEntry = ^IVectorView_1__Search_SortEntry;

  // External: Windows.Foundation.Collections.IVectorChangedEventArgs
  IVectorChangedEventArgs = interface;
  PIVectorChangedEventArgs = ^IVectorChangedEventArgs;

  // External: Windows.Foundation.IReference`1<Double>
  IReference_1__Double = interface;
  PIReference_1__Double = ^IReference_1__Double;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<UInt64,UInt64>
  IAsyncOperationWithProgress_2__UInt64__UInt64 = interface;
  PIAsyncOperationWithProgress_2__UInt64__UInt64 = ^IAsyncOperationWithProgress_2__UInt64__UInt64;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<UInt64,UInt64>
  AsyncOperationProgressHandler_2__UInt64__UInt64 = interface;
  PAsyncOperationProgressHandler_2__UInt64__UInt64 = ^AsyncOperationProgressHandler_2__UInt64__UInt64;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt64,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = interface;
  PAsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = ^AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64;

  // External: Windows.System.RemoteSystems.IRemoteSystemConnectionRequest
  RemoteSystems_IRemoteSystemConnectionRequest = interface;
  PRemoteSystems_IRemoteSystemConnectionRequest = ^RemoteSystems_IRemoteSystemConnectionRequest;

  // External: Windows.System.RemoteSystems.IRemoteSystem
  RemoteSystems_IRemoteSystem = interface;
  PRemoteSystems_IRemoteSystem = ^RemoteSystems_IRemoteSystem;

  // External: Windows.Foundation.Collections.IIterator`1<Guid>
  IIterator_1__TGuid = interface;
  PIIterator_1__TGuid = ^IIterator_1__TGuid;

  // External: Windows.Foundation.Collections.IVectorView`1<Guid>
  IVectorView_1__TGuid = interface;
  PIVectorView_1__TGuid = ^IVectorView_1__TGuid;

  // External: Windows.Foundation.Collections.IIterator`1<Int32>
  IIterator_1__Integer = interface;
  PIIterator_1__Integer = ^IIterator_1__Integer;

  // External: Windows.Foundation.Collections.IVectorView`1<Int32>
  IVectorView_1__Integer = interface;
  PIVectorView_1__Integer = ^IVectorView_1__Integer;

  // External: Windows.Foundation.Collections.IIterator`1<UInt32>
  IIterator_1__Cardinal = interface;
  PIIterator_1__Cardinal = ^IIterator_1__Cardinal;

  // External: Windows.Foundation.Collections.IVectorView`1<UInt32>
  IVectorView_1__Cardinal = interface;
  PIVectorView_1__Cardinal = ^IVectorView_1__Cardinal;

  // External: Windows.System.IDispatcherQueueTimer
  IDispatcherQueueTimer = interface;
  PIDispatcherQueueTimer = ^IDispatcherQueueTimer;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueueTimer,Object>
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueueTimer__IInspectable = ^TypedEventHandler_2__IDispatcherQueueTimer__IInspectable;

  // External: Windows.System.IDispatcherQueue
  IDispatcherQueue = interface;
  PIDispatcherQueue = ^IDispatcherQueue;

  // External: Windows.System.DispatcherQueueHandler
  DispatcherQueueHandler = interface;
  PDispatcherQueueHandler = ^DispatcherQueueHandler;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Windows.System.IDispatcherQueueShutdownStartingEventArgs>
  TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs = interface;
  PTypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs = ^TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs;

  // External: Windows.System.IDispatcherQueueShutdownStartingEventArgs
  IDispatcherQueueShutdownStartingEventArgs = interface;
  PIDispatcherQueueShutdownStartingEventArgs = ^IDispatcherQueueShutdownStartingEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Object>
  TypedEventHandler_2__IDispatcherQueue__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueue__IInspectable = ^TypedEventHandler_2__IDispatcherQueue__IInspectable;

  // External: Windows.Foundation.IReference`1<UInt32>
  IReference_1__Cardinal = interface;
  PIReference_1__Cardinal = ^IReference_1__Cardinal;

  // External: Windows.Foundation.Collections.IKeyValuePair`2<String,String>
  IKeyValuePair_2__HSTRING__HSTRING = interface;
  PIKeyValuePair_2__HSTRING__HSTRING = ^IKeyValuePair_2__HSTRING__HSTRING;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterator_1__IKeyValuePair_2__HSTRING__HSTRING = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__HSTRING = ^IIterator_1__IKeyValuePair_2__HSTRING__HSTRING;

  // External: Windows.Foundation.Collections.IMapView`2<String,String>
  IMapView_2__HSTRING__HSTRING = interface;
  PIMapView_2__HSTRING__HSTRING = ^IMapView_2__HSTRING__HSTRING;

  // External: Windows.ApplicationModel.IAppDisplayInfo
  IAppDisplayInfo = interface;
  PIAppDisplayInfo = ^IAppDisplayInfo;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Core.ICoreApplicationView,Windows.ApplicationModel.Activation.IActivatedEventArgs>
  TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs = interface;
  PTypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs = ^TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs;

  // External: Windows.ApplicationModel.Core.ICoreApplicationView
  ICoreApplicationView = interface;
  PICoreApplicationView = ^ICoreApplicationView;

  // External: Windows.UI.Core.ICoreWindow
  ICoreWindow = interface;
  PICoreWindow = ^ICoreWindow;

  // External: Windows.UI.Core.ICoreDispatcher
  ICoreDispatcher = interface;
  PICoreDispatcher = ^ICoreDispatcher;

  // External: Windows.UI.Core.DispatchedHandler
  DispatchedHandler = interface;
  PDispatchedHandler = ^DispatchedHandler;

  // External: Windows.UI.Core.IdleDispatchedHandler
  IdleDispatchedHandler = interface;
  PIdleDispatchedHandler = ^IdleDispatchedHandler;

  // External: Windows.UI.Core.IIdleDispatchedHandlerArgs
  IIdleDispatchedHandlerArgs = interface;
  PIIdleDispatchedHandlerArgs = ^IIdleDispatchedHandlerArgs;

  // External: Windows.UI.Core.ICoreCursor
  ICoreCursor = interface;
  PICoreCursor = ^ICoreCursor;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IWindowActivatedEventArgs>
  TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs = ^TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs;

  // External: Windows.UI.Core.IWindowActivatedEventArgs
  IWindowActivatedEventArgs = interface;
  PIWindowActivatedEventArgs = ^IWindowActivatedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IAutomationProviderRequestedEventArgs>
  TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs = ^TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs;

  // External: Windows.UI.Core.IAutomationProviderRequestedEventArgs
  IAutomationProviderRequestedEventArgs = interface;
  PIAutomationProviderRequestedEventArgs = ^IAutomationProviderRequestedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ICharacterReceivedEventArgs>
  TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs = ^TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs;

  // External: Windows.UI.Core.ICharacterReceivedEventArgs
  ICharacterReceivedEventArgs = interface;
  PICharacterReceivedEventArgs = ^ICharacterReceivedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ICoreWindowEventArgs>
  TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs = ^TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs;

  // External: Windows.UI.Core.ICoreWindowEventArgs
  ICoreWindowEventArgs = interface;
  PICoreWindowEventArgs = ^ICoreWindowEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IInputEnabledEventArgs>
  TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs = ^TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs;

  // External: Windows.UI.Core.IInputEnabledEventArgs
  IInputEnabledEventArgs = interface;
  PIInputEnabledEventArgs = ^IInputEnabledEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IKeyEventArgs>
  TypedEventHandler_2__ICoreWindow__IKeyEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__IKeyEventArgs = ^TypedEventHandler_2__ICoreWindow__IKeyEventArgs;

  // External: Windows.UI.Core.IKeyEventArgs
  IKeyEventArgs = interface;
  PIKeyEventArgs = ^IKeyEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__ICoreWindow__IPointerEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__IPointerEventArgs = ^TypedEventHandler_2__ICoreWindow__IPointerEventArgs;

  // External: Windows.UI.Core.IPointerEventArgs
  IPointerEventArgs = interface;
  PIPointerEventArgs = ^IPointerEventArgs;

  // External: Windows.UI.Input.IPointerPoint
  IPointerPoint = interface;
  PIPointerPoint = ^IPointerPoint;

  // External: Windows.Devices.Input.IPointerDevice
  Input_IPointerDevice = interface;
  PInput_IPointerDevice = ^Input_IPointerDevice;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Input.PointerDeviceUsage>
  IVectorView_1__Input_PointerDeviceUsage = interface;
  PIVectorView_1__Input_PointerDeviceUsage = ^IVectorView_1__Input_PointerDeviceUsage;

  // External: Windows.UI.Input.IPointerPointProperties
  IPointerPointProperties = interface;
  PIPointerPointProperties = ^IPointerPointProperties;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Input.IPointerPoint>
  IVector_1__IPointerPoint = interface;
  PIVector_1__IPointerPoint = ^IVector_1__IPointerPoint;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.IPointerPoint>
  IVectorView_1__IPointerPoint = interface;
  PIVectorView_1__IPointerPoint = ^IVectorView_1__IPointerPoint;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ITouchHitTestingEventArgs>
  TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs = ^TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs;

  // External: Windows.UI.Core.ITouchHitTestingEventArgs
  ITouchHitTestingEventArgs = interface;
  PITouchHitTestingEventArgs = ^ITouchHitTestingEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IWindowSizeChangedEventArgs>
  TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs = ^TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs;

  // External: Windows.UI.Core.IWindowSizeChangedEventArgs
  IWindowSizeChangedEventArgs = interface;
  PIWindowSizeChangedEventArgs = ^IWindowSizeChangedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IVisibilityChangedEventArgs>
  TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs = interface;
  PTypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs = ^TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs;

  // External: Windows.UI.Core.IVisibilityChangedEventArgs
  IVisibilityChangedEventArgs = interface;
  PIVisibilityChangedEventArgs = ^IVisibilityChangedEventArgs;

  // External: Windows.ApplicationModel.Activation.IActivatedEventArgs
  Activation_IActivatedEventArgs = interface;
  PActivation_IActivatedEventArgs = ^Activation_IActivatedEventArgs;

  // External: Windows.ApplicationModel.Activation.ISplashScreen
  Activation_ISplashScreen = interface;
  PActivation_ISplashScreen = ^Activation_ISplashScreen;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface;
  PTypedEventHandler_2__Activation_ISplashScreen__IInspectable = ^TypedEventHandler_2__Activation_ISplashScreen__IInspectable;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Object>
  TypedEventHandler_2__IDataPackage__IInspectable = interface;
  PTypedEventHandler_2__IDataPackage__IInspectable = ^TypedEventHandler_2__IDataPackage__IInspectable;

  // External: Windows.ApplicationModel.DataTransfer.IDataPackage
  IDataPackage = interface;
  PIDataPackage = ^IDataPackage;

  // External: Windows.ApplicationModel.DataTransfer.IDataPackageView
  IDataPackageView = interface;
  PIDataPackageView = ^IDataPackageView;

  // External: Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView
  IDataPackagePropertySetView = interface;
  PIDataPackagePropertySetView = ^IDataPackagePropertySetView;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference = interface;
  PIAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference = ^IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference = interface;
  PAsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference = ^AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference;

  // External: Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>
  IMapView_2__HSTRING__IRandomAccessStreamReference = interface;
  PIMapView_2__HSTRING__IRandomAccessStreamReference = ^IMapView_2__HSTRING__IRandomAccessStreamReference;

  // External: Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet
  IDataPackagePropertySet = interface;
  PIDataPackagePropertySet = ^IDataPackagePropertySet;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Windows.ApplicationModel.DataTransfer.IOperationCompletedEventArgs>
  TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs = interface;
  PTypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs = ^TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs;

  // External: Windows.ApplicationModel.DataTransfer.IOperationCompletedEventArgs
  IOperationCompletedEventArgs = interface;
  PIOperationCompletedEventArgs = ^IOperationCompletedEventArgs;

  // External: Windows.ApplicationModel.DataTransfer.DataProviderHandler
  DataProviderHandler = interface;
  PDataProviderHandler = ^DataProviderHandler;

  // External: Windows.ApplicationModel.DataTransfer.IDataProviderRequest
  IDataProviderRequest = interface;
  PIDataProviderRequest = ^IDataProviderRequest;

  // External: Windows.ApplicationModel.DataTransfer.IDataProviderDeferral
  IDataProviderDeferral = interface;
  PIDataProviderDeferral = ^IDataProviderDeferral;

  // External: Windows.Foundation.Collections.IMap`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>
  IMap_2__HSTRING__IRandomAccessStreamReference = interface;
  PIMap_2__HSTRING__IRandomAccessStreamReference = ^IMap_2__HSTRING__IRandomAccessStreamReference;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageItem>
  IIterable_1__IStorageItem = interface;
  PIIterable_1__IStorageItem = ^IIterable_1__IStorageItem;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageItem>
  IIterator_1__IStorageItem = interface;
  PIIterator_1__IStorageItem = ^IIterator_1__IStorageItem;

  // External: Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface;
  PEventHandler_1__IInspectable = ^EventHandler_1__IInspectable;

  // External: Windows.Foundation.IReference`1<Int16>
  IReference_1__SmallInt = interface;
  PIReference_1__SmallInt = ^IReference_1__SmallInt;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession,Object>
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable = ^TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession
  GenericAttributeProfile_IGattSession = interface;
  PGenericAttributeProfile_IGattSession = ^GenericAttributeProfile_IGattSession;

  // External: Windows.Devices.Bluetooth.IBluetoothDeviceId
  IBluetoothDeviceId = interface;
  PIBluetoothDeviceId = ^IBluetoothDeviceId;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSessionStatusChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__GenericAttributeProfile_IGattSessionStatusChangedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattSession__GenericAttributeProfile_IGattSessionStatusChangedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattSession__GenericAttributeProfile_IGattSessionStatusChangedEventArgs;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSessionStatusChangedEventArgs
  GenericAttributeProfile_IGattSessionStatusChangedEventArgs = interface;
  PGenericAttributeProfile_IGattSessionStatusChangedEventArgs = ^GenericAttributeProfile_IGattSessionStatusChangedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient,Object>
  TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable = ^TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient
  GenericAttributeProfile_IGattSubscribedClient = interface;
  PGenericAttributeProfile_IGattSubscribedClient = ^GenericAttributeProfile_IGattSubscribedClient;

  // External: Windows.Foundation.IReference`1<UInt8>
  IReference_1__Byte = interface;
  PIReference_1__Byte = ^IReference_1__Byte;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Object>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable = ^TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic
  GenericAttributeProfile_IGattLocalCharacteristic = interface;
  PGenericAttributeProfile_IGattLocalCharacteristic = ^GenericAttributeProfile_IGattLocalCharacteristic;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult = interface;
  PIAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult = ^IAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult = interface;
  PAsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult = ^AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult
  GenericAttributeProfile_IGattLocalDescriptorResult = interface;
  PGenericAttributeProfile_IGattLocalDescriptorResult = ^GenericAttributeProfile_IGattLocalDescriptorResult;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor
  GenericAttributeProfile_IGattLocalDescriptor = interface;
  PGenericAttributeProfile_IGattLocalDescriptor = ^GenericAttributeProfile_IGattLocalDescriptor;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattReadRequestedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattReadRequestedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattReadRequestedEventArgs;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs
  GenericAttributeProfile_IGattReadRequestedEventArgs = interface;
  PGenericAttributeProfile_IGattReadRequestedEventArgs = ^GenericAttributeProfile_IGattReadRequestedEventArgs;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest>
  IAsyncOperation_1__GenericAttributeProfile_IGattReadRequest = interface;
  PIAsyncOperation_1__GenericAttributeProfile_IGattReadRequest = ^IAsyncOperation_1__GenericAttributeProfile_IGattReadRequest;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest = interface;
  PAsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest = ^AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest
  GenericAttributeProfile_IGattReadRequest = interface;
  PGenericAttributeProfile_IGattReadRequest = ^GenericAttributeProfile_IGattReadRequest;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattReadRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattReadRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattReadRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs
  GenericAttributeProfile_IGattRequestStateChangedEventArgs = interface;
  PGenericAttributeProfile_IGattRequestStateChangedEventArgs = ^GenericAttributeProfile_IGattRequestStateChangedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattWriteRequestedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattWriteRequestedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattWriteRequestedEventArgs;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs
  GenericAttributeProfile_IGattWriteRequestedEventArgs = interface;
  PGenericAttributeProfile_IGattWriteRequestedEventArgs = ^GenericAttributeProfile_IGattWriteRequestedEventArgs;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest>
  IAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest = interface;
  PIAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest = ^IAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest = interface;
  PAsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest = ^AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest
  GenericAttributeProfile_IGattWriteRequest = interface;
  PGenericAttributeProfile_IGattWriteRequest = ^GenericAttributeProfile_IGattWriteRequest;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattWriteRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattWriteRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattWriteRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorParameters
  GenericAttributeProfile_IGattLocalDescriptorParameters = interface;
  PGenericAttributeProfile_IGattLocalDescriptorParameters = ^GenericAttributeProfile_IGattLocalDescriptorParameters;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor>
  IVectorView_1__GenericAttributeProfile_IGattLocalDescriptor = interface;
  PIVectorView_1__GenericAttributeProfile_IGattLocalDescriptor = ^IVectorView_1__GenericAttributeProfile_IGattLocalDescriptor;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattPresentationFormat>
  IVectorView_1__GenericAttributeProfile_IGattPresentationFormat = interface;
  PIVectorView_1__GenericAttributeProfile_IGattPresentationFormat = ^IVectorView_1__GenericAttributeProfile_IGattPresentationFormat;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattPresentationFormat
  GenericAttributeProfile_IGattPresentationFormat = interface;
  PGenericAttributeProfile_IGattPresentationFormat = ^GenericAttributeProfile_IGattPresentationFormat;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient>
  IVectorView_1__GenericAttributeProfile_IGattSubscribedClient = interface;
  PIVectorView_1__GenericAttributeProfile_IGattSubscribedClient = ^IVectorView_1__GenericAttributeProfile_IGattSubscribedClient;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattReadRequestedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattReadRequestedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattReadRequestedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattWriteRequestedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattWriteRequestedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattWriteRequestedEventArgs;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>>
  IAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = interface;
  PIAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = ^IAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>>
  AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = ^AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = interface;
  PIVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = ^IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult
  GenericAttributeProfile_IGattClientNotificationResult = interface;
  PGenericAttributeProfile_IGattClientNotificationResult = ^GenericAttributeProfile_IGattClientNotificationResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult = interface;
  PIAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult = ^IAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult = interface;
  PAsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult = ^AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.IBluetoothDevice,Object>
  TypedEventHandler_2__IBluetoothDevice__IInspectable = interface;
  PTypedEventHandler_2__IBluetoothDevice__IInspectable = ^TypedEventHandler_2__IBluetoothDevice__IInspectable;

  // External: Windows.Devices.Bluetooth.IBluetoothDevice
  IBluetoothDevice = interface;
  PIBluetoothDevice = ^IBluetoothDevice;

  // External: Windows.Networking.IHostName
  IHostName = interface;
  PIHostName = ^IHostName;

  // External: Windows.Networking.Connectivity.IIPInformation
  IIPInformation = interface;
  PIIPInformation = ^IIPInformation;

  // External: Windows.Networking.Connectivity.INetworkAdapter
  INetworkAdapter = interface;
  PINetworkAdapter = ^INetworkAdapter;

  // External: Windows.Networking.Connectivity.INetworkItem
  INetworkItem = interface;
  PINetworkItem = ^INetworkItem;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IConnectionProfile>
  IAsyncOperation_1__IConnectionProfile = interface;
  PIAsyncOperation_1__IConnectionProfile = ^IAsyncOperation_1__IConnectionProfile;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IConnectionProfile>
  AsyncOperationCompletedHandler_1__IConnectionProfile = interface;
  PAsyncOperationCompletedHandler_1__IConnectionProfile = ^AsyncOperationCompletedHandler_1__IConnectionProfile;

  // External: Windows.Networking.Connectivity.IConnectionProfile
  IConnectionProfile = interface;
  PIConnectionProfile = ^IConnectionProfile;

  // External: Windows.Networking.Connectivity.IConnectionCost
  IConnectionCost = interface;
  PIConnectionCost = ^IConnectionCost;

  // External: Windows.Networking.Connectivity.IDataPlanStatus
  IDataPlanStatus = interface;
  PIDataPlanStatus = ^IDataPlanStatus;

  // External: Windows.Networking.Connectivity.IDataPlanUsage
  IDataPlanUsage = interface;
  PIDataPlanUsage = ^IDataPlanUsage;

  // External: Windows.Foundation.IReference`1<UInt64>
  IReference_1__UInt64 = interface;
  PIReference_1__UInt64 = ^IReference_1__UInt64;

  // External: Windows.Foundation.IReference`1<Windows.Foundation.DateTime>
  IReference_1__DateTime = interface;
  PIReference_1__DateTime = ^IReference_1__DateTime;

  // External: Windows.Networking.Connectivity.IDataUsage
  IDataUsage = interface;
  PIDataUsage = ^IDataUsage;

  // External: Windows.Networking.Connectivity.INetworkSecuritySettings
  INetworkSecuritySettings = interface;
  PINetworkSecuritySettings = ^INetworkSecuritySettings;

  // External: Windows.Devices.Bluetooth.IBluetoothClassOfDevice
  IBluetoothClassOfDevice = interface;
  PIBluetoothClassOfDevice = ^IBluetoothClassOfDevice;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IBuffer>
  IVectorView_1__IBuffer = interface;
  PIVectorView_1__IBuffer = ^IVectorView_1__IBuffer;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Rfcomm.IRfcommDeviceService>
  IVectorView_1__Rfcomm_IRfcommDeviceService = interface;
  PIVectorView_1__Rfcomm_IRfcommDeviceService = ^IVectorView_1__Rfcomm_IRfcommDeviceService;

  // External: Windows.Devices.Bluetooth.Rfcomm.IRfcommDeviceService
  Rfcomm_IRfcommDeviceService = interface;
  PRfcomm_IRfcommDeviceService = ^Rfcomm_IRfcommDeviceService;

  // External: Windows.Devices.Bluetooth.Rfcomm.IRfcommServiceId
  Rfcomm_IRfcommServiceId = interface;
  PRfcomm_IRfcommServiceId = ^Rfcomm_IRfcommServiceId;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IAsyncOperation_1__IMapView_2__Cardinal__IBuffer = interface;
  PIAsyncOperation_1__IMapView_2__Cardinal__IBuffer = ^IAsyncOperation_1__IMapView_2__Cardinal__IBuffer;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>>
  AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer = interface;
  PAsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer = ^AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer;

  // External: Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>
  IMapView_2__Cardinal__IBuffer = interface;
  PIMapView_2__Cardinal__IBuffer = ^IMapView_2__Cardinal__IBuffer;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.IBluetoothLEDevice,Object>
  TypedEventHandler_2__IBluetoothLEDevice__IInspectable = interface;
  PTypedEventHandler_2__IBluetoothLEDevice__IInspectable = ^TypedEventHandler_2__IBluetoothLEDevice__IInspectable;

  // External: Windows.Devices.Bluetooth.IBluetoothLEDevice
  IBluetoothLEDevice = interface;
  PIBluetoothLEDevice = ^IBluetoothLEDevice;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDeviceService>
  IVectorView_1__GenericAttributeProfile_IGattDeviceService = interface;
  PIVectorView_1__GenericAttributeProfile_IGattDeviceService = ^IVectorView_1__GenericAttributeProfile_IGattDeviceService;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDeviceService
  GenericAttributeProfile_IGattDeviceService = interface;
  PGenericAttributeProfile_IGattDeviceService = ^GenericAttributeProfile_IGattDeviceService;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic>
  IVectorView_1__GenericAttributeProfile_IGattCharacteristic = interface;
  PIVectorView_1__GenericAttributeProfile_IGattCharacteristic = ^IVectorView_1__GenericAttributeProfile_IGattCharacteristic;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic
  GenericAttributeProfile_IGattCharacteristic = interface;
  PGenericAttributeProfile_IGattCharacteristic = ^GenericAttributeProfile_IGattCharacteristic;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDescriptor>
  IVectorView_1__GenericAttributeProfile_IGattDescriptor = interface;
  PIVectorView_1__GenericAttributeProfile_IGattDescriptor = ^IVectorView_1__GenericAttributeProfile_IGattDescriptor;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDescriptor
  GenericAttributeProfile_IGattDescriptor = interface;
  PGenericAttributeProfile_IGattDescriptor = ^GenericAttributeProfile_IGattDescriptor;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattReadResult = interface;
  PIAsyncOperation_1__GenericAttributeProfile_IGattReadResult = ^IAsyncOperation_1__GenericAttributeProfile_IGattReadResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult = interface;
  PAsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult = ^AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult
  GenericAttributeProfile_IGattReadResult = interface;
  PGenericAttributeProfile_IGattReadResult = ^GenericAttributeProfile_IGattReadResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus>
  IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus = interface;
  PIAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus = ^IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus = interface;
  PAsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus = ^AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = interface;
  PIAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = ^IAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = interface;
  PAsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = ^AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult
  GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = interface;
  PGenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = ^GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattValueChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs = interface;
  PTypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs = ^TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs;

  // External: Windows.Devices.Bluetooth.GenericAttributeProfile.IGattValueChangedEventArgs
  GenericAttributeProfile_IGattValueChangedEventArgs = interface;
  PGenericAttributeProfile_IGattValueChangedEventArgs = ^GenericAttributeProfile_IGattValueChangedEventArgs;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Radios.IRadio>
  IAsyncOperation_1__Radios_IRadio = interface;
  PIAsyncOperation_1__Radios_IRadio = ^IAsyncOperation_1__Radios_IRadio;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Radios.IRadio>
  AsyncOperationCompletedHandler_1__Radios_IRadio = interface;
  PAsyncOperationCompletedHandler_1__Radios_IRadio = ^AsyncOperationCompletedHandler_1__Radios_IRadio;

  // External: Windows.Devices.Radios.IRadio
  Radios_IRadio = interface;
  PRadios_IRadio = ^Radios_IRadio;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Radios.RadioAccessStatus>
  IAsyncOperation_1__Radios_RadioAccessStatus = interface;
  PIAsyncOperation_1__Radios_RadioAccessStatus = ^IAsyncOperation_1__Radios_RadioAccessStatus;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Radios.RadioAccessStatus>
  AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__Radios_RadioAccessStatus = ^AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Radios.IRadio,Object>
  TypedEventHandler_2__Radios_IRadio__IInspectable = interface;
  PTypedEventHandler_2__Radios_IRadio__IInspectable = ^TypedEventHandler_2__Radios_IRadio__IInspectable;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Object>
  TypedEventHandler_2__IDeviceWatcher__IInspectable = interface;
  PTypedEventHandler_2__IDeviceWatcher__IInspectable = ^TypedEventHandler_2__IDeviceWatcher__IInspectable;

  // External: Windows.Devices.Enumeration.IDeviceWatcher
  IDeviceWatcher = interface;
  PIDeviceWatcher = ^IDeviceWatcher;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Windows.Devices.Enumeration.IDeviceInformation>
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformation = interface;
  PTypedEventHandler_2__IDeviceWatcher__IDeviceInformation = ^TypedEventHandler_2__IDeviceWatcher__IDeviceInformation;

  // External: Windows.Devices.Enumeration.IDeviceInformation
  IDeviceInformation = interface;
  PIDeviceInformation = ^IDeviceInformation;

  // External: Windows.Devices.Enumeration.IEnclosureLocation
  IEnclosureLocation = interface;
  PIEnclosureLocation = ^IEnclosureLocation;

  // External: Windows.Devices.Enumeration.IDeviceInformationUpdate
  IDeviceInformationUpdate = interface;
  PIDeviceInformationUpdate = ^IDeviceInformationUpdate;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Windows.Devices.Enumeration.IDeviceInformationUpdate>
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate = interface;
  PTypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate = ^TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Object>
  TypedEventHandler_2__IDevicePicker__IInspectable = interface;
  PTypedEventHandler_2__IDevicePicker__IInspectable = ^TypedEventHandler_2__IDevicePicker__IInspectable;

  // External: Windows.Devices.Enumeration.IDevicePicker
  IDevicePicker = interface;
  PIDevicePicker = ^IDevicePicker;

  // External: Windows.Devices.Enumeration.IDevicePickerFilter
  IDevicePickerFilter = interface;
  PIDevicePickerFilter = ^IDevicePickerFilter;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Devices.Enumeration.DeviceClass>
  IVector_1__DeviceClass = interface;
  PIVector_1__DeviceClass = ^IVector_1__DeviceClass;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.DeviceClass>
  IVectorView_1__DeviceClass = interface;
  PIVectorView_1__DeviceClass = ^IVectorView_1__DeviceClass;

  // External: Windows.Devices.Enumeration.IDevicePickerAppearance
  IDevicePickerAppearance = interface;
  PIDevicePickerAppearance = ^IDevicePickerAppearance;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Windows.Devices.Enumeration.IDeviceSelectedEventArgs>
  TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs = interface;
  PTypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs = ^TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs;

  // External: Windows.Devices.Enumeration.IDeviceSelectedEventArgs
  IDeviceSelectedEventArgs = interface;
  PIDeviceSelectedEventArgs = ^IDeviceSelectedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Windows.Devices.Enumeration.IDeviceDisconnectButtonClickedEventArgs>
  TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs = interface;
  PTypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs = ^TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs;

  // External: Windows.Devices.Enumeration.IDeviceDisconnectButtonClickedEventArgs
  IDeviceDisconnectButtonClickedEventArgs = interface;
  PIDeviceDisconnectButtonClickedEventArgs = ^IDeviceDisconnectButtonClickedEventArgs;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDeviceInformation>
  IAsyncOperation_1__IDeviceInformation = interface;
  PIAsyncOperation_1__IDeviceInformation = ^IAsyncOperation_1__IDeviceInformation;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDeviceInformation>
  AsyncOperationCompletedHandler_1__IDeviceInformation = interface;
  PAsyncOperationCompletedHandler_1__IDeviceInformation = ^AsyncOperationCompletedHandler_1__IDeviceInformation;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Object>
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable = interface;
  PTypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable = ^TypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable;

  // External: Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher
  Pnp_IPnpObjectWatcher = interface;
  PPnp_IPnpObjectWatcher = ^Pnp_IPnpObjectWatcher;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Windows.Devices.Enumeration.Pnp.IPnpObject>
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject = interface;
  PTypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject = ^TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject;

  // External: Windows.Devices.Enumeration.Pnp.IPnpObject
  Pnp_IPnpObject = interface;
  PPnp_IPnpObject = ^Pnp_IPnpObject;

  // External: Windows.Devices.Enumeration.Pnp.IPnpObjectUpdate
  Pnp_IPnpObjectUpdate = interface;
  PPnp_IPnpObjectUpdate = ^Pnp_IPnpObjectUpdate;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Windows.Devices.Enumeration.Pnp.IPnpObjectUpdate>
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate = interface;
  PTypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate = ^TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawerCloseAlarm,Object>
  TypedEventHandler_2__ICashDrawerCloseAlarm__IInspectable = interface;
  PTypedEventHandler_2__ICashDrawerCloseAlarm__IInspectable = ^TypedEventHandler_2__ICashDrawerCloseAlarm__IInspectable;

  // External: Windows.Devices.PointOfService.ICashDrawerCloseAlarm
  ICashDrawerCloseAlarm = interface;
  PICashDrawerCloseAlarm = ^ICashDrawerCloseAlarm;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Object>
  TypedEventHandler_2__IClaimedLineDisplay__IInspectable = interface;
  PTypedEventHandler_2__IClaimedLineDisplay__IInspectable = ^TypedEventHandler_2__IClaimedLineDisplay__IInspectable;

  // External: Windows.Devices.PointOfService.IClaimedLineDisplay
  IClaimedLineDisplay = interface;
  PIClaimedLineDisplay = ^IClaimedLineDisplay;

  // External: Windows.Devices.PointOfService.ILineDisplayCapabilities
  ILineDisplayCapabilities = interface;
  PILineDisplayCapabilities = ^ILineDisplayCapabilities;

  // External: Windows.Devices.PointOfService.ILineDisplayWindow
  ILineDisplayWindow = interface;
  PILineDisplayWindow = ^ILineDisplayWindow;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedCashDrawer,Object>
  TypedEventHandler_2__IClaimedCashDrawer__IInspectable = interface;
  PTypedEventHandler_2__IClaimedCashDrawer__IInspectable = ^TypedEventHandler_2__IClaimedCashDrawer__IInspectable;

  // External: Windows.Devices.PointOfService.IClaimedCashDrawer
  IClaimedCashDrawer = interface;
  PIClaimedCashDrawer = ^IClaimedCashDrawer;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterable_1__IKeyValuePair_2__HSTRING__HSTRING = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__HSTRING = ^IIterable_1__IKeyValuePair_2__HSTRING__HSTRING;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal = interface;
  PIAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal = ^IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal = interface;
  PAsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal = ^AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal;

  // External: Windows.Devices.Scanners.IImageScannerScanResult
  IImageScannerScanResult = interface;
  PIImageScannerScanResult = ^IImageScannerScanResult;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  IAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer = interface;
  PIAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer = ^IAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer = interface;
  PAsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer = ^AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer = ^AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>
  IVectorView_1__ISmsMessage = interface;
  PIVectorView_1__ISmsMessage = ^IVectorView_1__ISmsMessage;

  // External: Windows.Devices.Sms.ISmsMessage
  ISmsMessage = interface;
  PISmsMessage = ^ISmsMessage;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Devices.Sms.ISmsDevice2,Object>
  TypedEventHandler_2__ISmsDevice2__IInspectable = interface;
  PTypedEventHandler_2__ISmsDevice2__IInspectable = ^TypedEventHandler_2__ISmsDevice2__IInspectable;

  // External: Windows.Devices.Sms.ISmsDevice2
  ISmsDevice2 = interface;
  PISmsDevice2 = ^ISmsDevice2;

  // External: Windows.Devices.Sms.ISmsMessageBase
  ISmsMessageBase = interface;
  PISmsMessageBase = ^ISmsMessageBase;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Devices.Sms.ISmsSendMessageResult>
  IAsyncOperation_1__ISmsSendMessageResult = interface;
  PIAsyncOperation_1__ISmsSendMessageResult = ^IAsyncOperation_1__ISmsSendMessageResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Sms.ISmsSendMessageResult>
  AsyncOperationCompletedHandler_1__ISmsSendMessageResult = interface;
  PAsyncOperationCompletedHandler_1__ISmsSendMessageResult = ^AsyncOperationCompletedHandler_1__ISmsSendMessageResult;

  // External: Windows.Devices.Sms.ISmsSendMessageResult
  ISmsSendMessageResult = interface;
  PISmsSendMessageResult = ^ISmsSendMessageResult;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Gaming.Input.IGameController,Windows.System.IUserChangedEventArgs>
  TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs = interface;
  PTypedEventHandler_2__Input_IGameController__IUserChangedEventArgs = ^TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs;

  // External: Windows.Gaming.Input.IGameController
  Input_IGameController = interface;
  PInput_IGameController = ^Input_IGameController;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Gaming.Input.IGameController,Windows.Gaming.Input.IHeadset>
  TypedEventHandler_2__Input_IGameController__Input_IHeadset = interface;
  PTypedEventHandler_2__Input_IGameController__Input_IHeadset = ^TypedEventHandler_2__Input_IGameController__Input_IHeadset;

  // External: Windows.Gaming.Input.IHeadset
  Input_IHeadset = interface;
  PInput_IHeadset = ^Input_IHeadset;

  // External: Windows.System.IUserChangedEventArgs
  IUserChangedEventArgs = interface;
  PIUserChangedEventArgs = ^IUserChangedEventArgs;

  // External: Windows.Devices.Power.IBatteryReport
  Power_IBatteryReport = interface;
  PPower_IBatteryReport = ^Power_IBatteryReport;

  // External: Windows.Foundation.IReference`1<Int32>
  IReference_1__Integer = interface;
  PIReference_1__Integer = ^IReference_1__Integer;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Haptics.ISimpleHapticsController>
  IVectorView_1__Haptics_ISimpleHapticsController = interface;
  PIVectorView_1__Haptics_ISimpleHapticsController = ^IVectorView_1__Haptics_ISimpleHapticsController;

  // External: Windows.Devices.Haptics.ISimpleHapticsController
  Haptics_ISimpleHapticsController = interface;
  PHaptics_ISimpleHapticsController = ^Haptics_ISimpleHapticsController;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Haptics.ISimpleHapticsControllerFeedback>
  IVectorView_1__Haptics_ISimpleHapticsControllerFeedback = interface;
  PIVectorView_1__Haptics_ISimpleHapticsControllerFeedback = ^IVectorView_1__Haptics_ISimpleHapticsControllerFeedback;

  // External: Windows.Devices.Haptics.ISimpleHapticsControllerFeedback
  Haptics_ISimpleHapticsControllerFeedback = interface;
  PHaptics_ISimpleHapticsControllerFeedback = ^Haptics_ISimpleHapticsControllerFeedback;

  // External: Windows.Foundation.Collections.IMapView`2<String,Windows.Foundation.Collections.IVectorView`1<String>>
  IMapView_2__HSTRING__IVectorView_1__HSTRING = interface;
  PIMapView_2__HSTRING__IVectorView_1__HSTRING = ^IMapView_2__HSTRING__IVectorView_1__HSTRING;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFileInputNode,Object>
  TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable = interface;
  PTypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable = ^TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable;

  // External: Windows.Media.Audio.IAudioFileInputNode
  Audio_IAudioFileInputNode = interface;
  PAudio_IAudioFileInputNode = ^Audio_IAudioFileInputNode;

  // External: Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>
  IReference_1__TimeSpan = interface;
  PIReference_1__TimeSpan = ^IReference_1__TimeSpan;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioGraph,Object>
  TypedEventHandler_2__Audio_IAudioGraph__IInspectable = interface;
  PTypedEventHandler_2__Audio_IAudioGraph__IInspectable = ^TypedEventHandler_2__Audio_IAudioGraph__IInspectable;

  // External: Windows.Media.Audio.IAudioGraph
  Audio_IAudioGraph = interface;
  PAudio_IAudioGraph = ^Audio_IAudioGraph;

  // External: Windows.Media.Audio.IAudioFrameInputNode
  Audio_IAudioFrameInputNode = interface;
  PAudio_IAudioFrameInputNode = ^Audio_IAudioFrameInputNode;

  // External: Windows.Media.IAudioFrame
  IAudioFrame = interface;
  PIAudioFrame = ^IAudioFrame;

  // External: Windows.Media.IAudioBuffer
  IAudioBuffer = interface;
  PIAudioBuffer = ^IAudioBuffer;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFrameInputNode,Windows.Media.Audio.IAudioFrameCompletedEventArgs>
  TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IAudioFrameCompletedEventArgs = interface;
  PTypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IAudioFrameCompletedEventArgs = ^TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IAudioFrameCompletedEventArgs;

  // External: Windows.Media.Audio.IAudioFrameCompletedEventArgs
  Audio_IAudioFrameCompletedEventArgs = interface;
  PAudio_IAudioFrameCompletedEventArgs = ^Audio_IAudioFrameCompletedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFrameInputNode,Windows.Media.Audio.IFrameInputNodeQuantumStartedEventArgs>
  TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IFrameInputNodeQuantumStartedEventArgs = interface;
  PTypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IFrameInputNodeQuantumStartedEventArgs = ^TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IFrameInputNodeQuantumStartedEventArgs;

  // External: Windows.Media.Audio.IFrameInputNodeQuantumStartedEventArgs
  Audio_IFrameInputNodeQuantumStartedEventArgs = interface;
  PAudio_IFrameInputNodeQuantumStartedEventArgs = ^Audio_IFrameInputNodeQuantumStartedEventArgs;

  // External: Windows.Media.MediaProperties.IAudioEncodingProperties
  IAudioEncodingProperties = interface;
  PIAudioEncodingProperties = ^IAudioEncodingProperties;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioDeviceInputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult = interface;
  PIAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult = ^IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioDeviceInputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult = interface;
  PAsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult = ^AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult;

  // External: Windows.Media.Audio.ICreateAudioDeviceInputNodeResult
  Audio_ICreateAudioDeviceInputNodeResult = interface;
  PAudio_ICreateAudioDeviceInputNodeResult = ^Audio_ICreateAudioDeviceInputNodeResult;

  // External: Windows.Media.Audio.IAudioDeviceInputNode
  Audio_IAudioDeviceInputNode = interface;
  PAudio_IAudioDeviceInputNode = ^Audio_IAudioDeviceInputNode;

  // External: Windows.Media.Audio.IAudioFrameOutputNode
  Audio_IAudioFrameOutputNode = interface;
  PAudio_IAudioFrameOutputNode = ^Audio_IAudioFrameOutputNode;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult = interface;
  PIAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult = ^IAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult = interface;
  PAsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult = ^AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult;

  // External: Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult
  Audio_ICreateAudioDeviceOutputNodeResult = interface;
  PAudio_ICreateAudioDeviceOutputNodeResult = ^Audio_ICreateAudioDeviceOutputNodeResult;

  // External: Windows.Media.Audio.IAudioDeviceOutputNode
  Audio_IAudioDeviceOutputNode = interface;
  PAudio_IAudioDeviceOutputNode = ^Audio_IAudioDeviceOutputNode;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioFileInputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult = interface;
  PIAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult = ^IAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioFileInputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult = interface;
  PAsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult = ^AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult;

  // External: Windows.Media.Audio.ICreateAudioFileInputNodeResult
  Audio_ICreateAudioFileInputNodeResult = interface;
  PAudio_ICreateAudioFileInputNodeResult = ^Audio_ICreateAudioFileInputNodeResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioFileOutputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult = interface;
  PIAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult = ^IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioFileOutputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult = interface;
  PAsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult = ^AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult;

  // External: Windows.Media.Audio.ICreateAudioFileOutputNodeResult
  Audio_ICreateAudioFileOutputNodeResult = interface;
  PAudio_ICreateAudioFileOutputNodeResult = ^Audio_ICreateAudioFileOutputNodeResult;

  // External: Windows.Media.Audio.IAudioFileOutputNode
  Audio_IAudioFileOutputNode = interface;
  PAudio_IAudioFileOutputNode = ^Audio_IAudioFileOutputNode;

  // External: Windows.Media.MediaProperties.IMediaEncodingProfile
  IMediaEncodingProfile = interface;
  PIMediaEncodingProfile = ^IMediaEncodingProfile;

  // External: Windows.Media.MediaProperties.IVideoEncodingProperties
  IVideoEncodingProperties = interface;
  PIVideoEncodingProperties = ^IVideoEncodingProperties;

  // External: Windows.Media.MediaProperties.IMediaRatio
  IMediaRatio = interface;
  PIMediaRatio = ^IMediaRatio;

  // External: Windows.Media.MediaProperties.IContainerEncodingProperties
  IContainerEncodingProperties = interface;
  PIContainerEncodingProperties = ^IContainerEncodingProperties;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Media.Transcoding.TranscodeFailureReason>
  IAsyncOperation_1__Transcoding_TranscodeFailureReason = interface;
  PIAsyncOperation_1__Transcoding_TranscodeFailureReason = ^IAsyncOperation_1__Transcoding_TranscodeFailureReason;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Transcoding.TranscodeFailureReason>
  AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason = interface;
  PAsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason = ^AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason;

  // External: Windows.Media.Audio.IAudioInputNode
  Audio_IAudioInputNode = interface;
  PAudio_IAudioInputNode = ^Audio_IAudioInputNode;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Audio.IAudioGraphConnection>
  IVectorView_1__Audio_IAudioGraphConnection = interface;
  PIVectorView_1__Audio_IAudioGraphConnection = ^IVectorView_1__Audio_IAudioGraphConnection;

  // External: Windows.Media.Audio.IAudioGraphConnection
  Audio_IAudioGraphConnection = interface;
  PAudio_IAudioGraphConnection = ^Audio_IAudioGraphConnection;

  // External: Windows.Media.Audio.IAudioNode
  Audio_IAudioNode = interface;
  PAudio_IAudioNode = ^Audio_IAudioNode;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Media.Effects.IAudioEffectDefinition>
  IVector_1__Effects_IAudioEffectDefinition = interface;
  PIVector_1__Effects_IAudioEffectDefinition = ^IVector_1__Effects_IAudioEffectDefinition;

  // External: Windows.Media.Effects.IAudioEffectDefinition
  Effects_IAudioEffectDefinition = interface;
  PEffects_IAudioEffectDefinition = ^Effects_IAudioEffectDefinition;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Effects.IAudioEffectDefinition>
  IVectorView_1__Effects_IAudioEffectDefinition = interface;
  PIVectorView_1__Effects_IAudioEffectDefinition = ^IVectorView_1__Effects_IAudioEffectDefinition;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioGraph,Windows.Media.Audio.IAudioGraphUnrecoverableErrorOccurredEventArgs>
  TypedEventHandler_2__Audio_IAudioGraph__Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs = interface;
  PTypedEventHandler_2__Audio_IAudioGraph__Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs = ^TypedEventHandler_2__Audio_IAudioGraph__Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs;

  // External: Windows.Media.Audio.IAudioGraphUnrecoverableErrorOccurredEventArgs
  Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs = interface;
  PAudio_IAudioGraphUnrecoverableErrorOccurredEventArgs = ^Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs;

  // External: Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>
  IKeyValuePair_2__TGuid__IInspectable = interface;
  PIKeyValuePair_2__TGuid__IInspectable = ^IKeyValuePair_2__TGuid__IInspectable;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>>
  IIterator_1__IKeyValuePair_2__TGuid__IInspectable = interface;
  PIIterator_1__IKeyValuePair_2__TGuid__IInspectable = ^IIterator_1__IKeyValuePair_2__TGuid__IInspectable;

  // External: Windows.Foundation.Collections.IMapView`2<Guid,Object>
  IMapView_2__TGuid__IInspectable = interface;
  PIMapView_2__TGuid__IInspectable = ^IMapView_2__TGuid__IInspectable;

  // External: Windows.Foundation.IReference`1<Boolean>
  IReference_1__Boolean = interface;
  PIReference_1__Boolean = ^IReference_1__Boolean;

  // External: Windows.Foundation.Collections.IMap`2<Guid,Object>
  IMap_2__TGuid__IInspectable = interface;
  PIMap_2__TGuid__IInspectable = ^IMap_2__TGuid__IInspectable;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Playback.IMediaPlaybackItem,Windows.Foundation.Collections.IVectorChangedEventArgs>
  TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs = interface;
  PTypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs = ^TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs;

  // External: Windows.Media.Playback.IMediaPlaybackItem
  Playback_IMediaPlaybackItem = interface;
  PPlayback_IMediaPlaybackItem = ^Playback_IMediaPlaybackItem;

  // External: Windows.Media.Core.IMediaSource2
  Core_IMediaSource2 = interface;
  PCore_IMediaSource2 = ^Core_IMediaSource2;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.IMediaSource2,Windows.Media.Core.IMediaSourceOpenOperationCompletedEventArgs>
  TypedEventHandler_2__Core_IMediaSource2__Core_IMediaSourceOpenOperationCompletedEventArgs = interface;
  PTypedEventHandler_2__Core_IMediaSource2__Core_IMediaSourceOpenOperationCompletedEventArgs = ^TypedEventHandler_2__Core_IMediaSource2__Core_IMediaSourceOpenOperationCompletedEventArgs;

  // External: Windows.Media.Core.IMediaSourceOpenOperationCompletedEventArgs
  Core_IMediaSourceOpenOperationCompletedEventArgs = interface;
  PCore_IMediaSourceOpenOperationCompletedEventArgs = ^Core_IMediaSourceOpenOperationCompletedEventArgs;

  // External: Windows.Media.Core.IMediaSourceError
  Core_IMediaSourceError = interface;
  PCore_IMediaSourceError = ^Core_IMediaSourceError;

  // External: Windows.Foundation.Collections.IObservableVector`1<Windows.Media.Core.ITimedTextSource>
  IObservableVector_1__Core_ITimedTextSource = interface;
  PIObservableVector_1__Core_ITimedTextSource = ^IObservableVector_1__Core_ITimedTextSource;

  // External: Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Media.Core.ITimedTextSource>
  VectorChangedEventHandler_1__Core_ITimedTextSource = interface;
  PVectorChangedEventHandler_1__Core_ITimedTextSource = ^VectorChangedEventHandler_1__Core_ITimedTextSource;

  // External: Windows.Foundation.Collections.IObservableVector`1<Windows.Media.Core.ITimedMetadataTrack>
  IObservableVector_1__Core_ITimedMetadataTrack = interface;
  PIObservableVector_1__Core_ITimedMetadataTrack = ^IObservableVector_1__Core_ITimedMetadataTrack;

  // External: Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Media.Core.ITimedMetadataTrack>
  VectorChangedEventHandler_1__Core_ITimedMetadataTrack = interface;
  PVectorChangedEventHandler_1__Core_ITimedMetadataTrack = ^VectorChangedEventHandler_1__Core_ITimedMetadataTrack;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IMediaTrack>
  IVectorView_1__Core_IMediaTrack = interface;
  PIVectorView_1__Core_IMediaTrack = ^IVectorView_1__Core_IMediaTrack;

  // External: Windows.Media.Core.IMediaTrack
  Core_IMediaTrack = interface;
  PCore_IMediaTrack = ^Core_IMediaTrack;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.ITimedMetadataTrack>
  IVectorView_1__Core_ITimedMetadataTrack = interface;
  PIVectorView_1__Core_ITimedMetadataTrack = ^IVectorView_1__Core_ITimedMetadataTrack;

  // External: Windows.Media.Core.ITimedMetadataTrack
  Core_ITimedMetadataTrack = interface;
  PCore_ITimedMetadataTrack = ^Core_ITimedMetadataTrack;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.ITimedMetadataTrack,Windows.Media.Core.IMediaCueEventArgs>
  TypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs = interface;
  PTypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs = ^TypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs;

  // External: Windows.Media.Core.IMediaCueEventArgs
  Core_IMediaCueEventArgs = interface;
  PCore_IMediaCueEventArgs = ^Core_IMediaCueEventArgs;

  // External: Windows.Media.Core.IMediaCue
  Core_IMediaCue = interface;
  PCore_IMediaCue = ^Core_IMediaCue;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.ITimedMetadataTrack,Windows.Media.Core.ITimedMetadataTrackFailedEventArgs>
  TypedEventHandler_2__Core_ITimedMetadataTrack__Core_ITimedMetadataTrackFailedEventArgs = interface;
  PTypedEventHandler_2__Core_ITimedMetadataTrack__Core_ITimedMetadataTrackFailedEventArgs = ^TypedEventHandler_2__Core_ITimedMetadataTrack__Core_ITimedMetadataTrackFailedEventArgs;

  // External: Windows.Media.Core.ITimedMetadataTrackFailedEventArgs
  Core_ITimedMetadataTrackFailedEventArgs = interface;
  PCore_ITimedMetadataTrackFailedEventArgs = ^Core_ITimedMetadataTrackFailedEventArgs;

  // External: Windows.Media.Core.ITimedMetadataTrackError
  Core_ITimedMetadataTrackError = interface;
  PCore_ITimedMetadataTrackError = ^Core_ITimedMetadataTrackError;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IMediaCue>
  IVectorView_1__Core_IMediaCue = interface;
  PIVectorView_1__Core_IMediaCue = ^IVectorView_1__Core_IMediaCue;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt64>
  IAsyncOperationWithProgress_2__IBuffer__UInt64 = interface;
  PIAsyncOperationWithProgress_2__IBuffer__UInt64 = ^IAsyncOperationWithProgress_2__IBuffer__UInt64;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt64>
  AsyncOperationProgressHandler_2__IBuffer__UInt64 = interface;
  PAsyncOperationProgressHandler_2__IBuffer__UInt64 = ^AsyncOperationProgressHandler_2__IBuffer__UInt64;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64 = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64 = ^AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IInputStream,UInt64>
  IAsyncOperationWithProgress_2__IInputStream__UInt64 = interface;
  PIAsyncOperationWithProgress_2__IInputStream__UInt64 = ^IAsyncOperationWithProgress_2__IInputStream__UInt64;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IInputStream,UInt64>
  AsyncOperationProgressHandler_2__IInputStream__UInt64 = interface;
  PAsyncOperationProgressHandler_2__IInputStream__UInt64 = ^AsyncOperationProgressHandler_2__IInputStream__UInt64;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IInputStream,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64 = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64 = ^AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64;

  // External: Windows.Foundation.IAsyncOperationWithProgress`2<String,UInt64>
  IAsyncOperationWithProgress_2__HSTRING__UInt64 = interface;
  PIAsyncOperationWithProgress_2__HSTRING__UInt64 = ^IAsyncOperationWithProgress_2__HSTRING__UInt64;

  // External: Windows.Foundation.AsyncOperationProgressHandler`2<String,UInt64>
  AsyncOperationProgressHandler_2__HSTRING__UInt64 = interface;
  PAsyncOperationProgressHandler_2__HSTRING__UInt64 = ^AsyncOperationProgressHandler_2__HSTRING__UInt64;

  // External: Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<String,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64 = interface;
  PAsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64 = ^AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64;

  // External: Windows.Foundation.Collections.IMap`2<String,String>
  IMap_2__HSTRING__HSTRING = interface;
  PIMap_2__HSTRING__HSTRING = ^IMap_2__HSTRING__HSTRING;

  // External: Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>
  IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = interface;
  PIKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = ^IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>>
  IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = ^IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING;

  // External: Windows.Foundation.IReference`1<Single>
  IReference_1__Single = interface;
  PIReference_1__Single = ^IReference_1__Single;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.IAudioStreamDescriptor>
  IIterable_1__Core_IAudioStreamDescriptor = interface;
  PIIterable_1__Core_IAudioStreamDescriptor = ^IIterable_1__Core_IAudioStreamDescriptor;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.IAudioStreamDescriptor>
  IIterator_1__Core_IAudioStreamDescriptor = interface;
  PIIterator_1__Core_IAudioStreamDescriptor = ^IIterator_1__Core_IAudioStreamDescriptor;

  // External: Windows.Media.Core.IAudioStreamDescriptor
  Core_IAudioStreamDescriptor = interface;
  PCore_IAudioStreamDescriptor = ^Core_IAudioStreamDescriptor;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Media.Core.IAudioStreamDescriptor>
  IVector_1__Core_IAudioStreamDescriptor = interface;
  PIVector_1__Core_IAudioStreamDescriptor = ^IVector_1__Core_IAudioStreamDescriptor;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IAudioStreamDescriptor>
  IVectorView_1__Core_IAudioStreamDescriptor = interface;
  PIVectorView_1__Core_IAudioStreamDescriptor = ^IVectorView_1__Core_IAudioStreamDescriptor;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.IVideoStreamDescriptor>
  IIterable_1__Core_IVideoStreamDescriptor = interface;
  PIIterable_1__Core_IVideoStreamDescriptor = ^IIterable_1__Core_IVideoStreamDescriptor;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.IVideoStreamDescriptor>
  IIterator_1__Core_IVideoStreamDescriptor = interface;
  PIIterator_1__Core_IVideoStreamDescriptor = ^IIterator_1__Core_IVideoStreamDescriptor;

  // External: Windows.Media.Core.IVideoStreamDescriptor
  Core_IVideoStreamDescriptor = interface;
  PCore_IVideoStreamDescriptor = ^Core_IVideoStreamDescriptor;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Media.Core.IVideoStreamDescriptor>
  IVector_1__Core_IVideoStreamDescriptor = interface;
  PIVector_1__Core_IVideoStreamDescriptor = ^IVector_1__Core_IVideoStreamDescriptor;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IVideoStreamDescriptor>
  IVectorView_1__Core_IVideoStreamDescriptor = interface;
  PIVectorView_1__Core_IVideoStreamDescriptor = ^IVectorView_1__Core_IVideoStreamDescriptor;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  IIterable_1__Core_ITimedMetadataStreamDescriptor = interface;
  PIIterable_1__Core_ITimedMetadataStreamDescriptor = ^IIterable_1__Core_ITimedMetadataStreamDescriptor;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  IIterator_1__Core_ITimedMetadataStreamDescriptor = interface;
  PIIterator_1__Core_ITimedMetadataStreamDescriptor = ^IIterator_1__Core_ITimedMetadataStreamDescriptor;

  // External: Windows.Media.Core.ITimedMetadataStreamDescriptor
  Core_ITimedMetadataStreamDescriptor = interface;
  PCore_ITimedMetadataStreamDescriptor = ^Core_ITimedMetadataStreamDescriptor;

  // External: Windows.Media.MediaProperties.ITimedMetadataEncodingProperties
  ITimedMetadataEncodingProperties = interface;
  PITimedMetadataEncodingProperties = ^ITimedMetadataEncodingProperties;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  IVector_1__Core_ITimedMetadataStreamDescriptor = interface;
  PIVector_1__Core_ITimedMetadataStreamDescriptor = ^IVector_1__Core_ITimedMetadataStreamDescriptor;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  IVectorView_1__Core_ITimedMetadataStreamDescriptor = interface;
  PIVectorView_1__Core_ITimedMetadataStreamDescriptor = ^IVectorView_1__Core_ITimedMetadataStreamDescriptor;

  // External: Windows.Foundation.Collections.IIterator`1<UInt8>
  IIterator_1__Byte = interface;
  PIIterator_1__Byte = ^IIterator_1__Byte;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>
  IVectorView_1__IConnectionProfile = interface;
  PIVectorView_1__IConnectionProfile = ^IVectorView_1__IConnectionProfile;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Object>
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable = interface;
  PTypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable = ^TypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher
  IMobileBroadbandAccountWatcher = interface;
  PIMobileBroadbandAccountWatcher = ^IMobileBroadbandAccountWatcher;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Windows.Networking.NetworkOperators.IMobileBroadbandAccountEventArgs>
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs = interface;
  PTypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs = ^TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandAccountEventArgs
  IMobileBroadbandAccountEventArgs = interface;
  PIMobileBroadbandAccountEventArgs = ^IMobileBroadbandAccountEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Windows.Networking.NetworkOperators.IMobileBroadbandAccountUpdatedEventArgs>
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs = interface;
  PTypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs = ^TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandAccountUpdatedEventArgs
  IMobileBroadbandAccountUpdatedEventArgs = interface;
  PIMobileBroadbandAccountUpdatedEventArgs = ^IMobileBroadbandAccountUpdatedEventArgs;

  // External: Windows.Foundation.Collections.IIterable`1<UInt32>
  IIterable_1__Cardinal = interface;
  PIIterable_1__Cardinal = ^IIterable_1__Cardinal;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandModem,Object>
  TypedEventHandler_2__IMobileBroadbandModem__IInspectable = interface;
  PTypedEventHandler_2__IMobileBroadbandModem__IInspectable = ^TypedEventHandler_2__IMobileBroadbandModem__IInspectable;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandModem
  IMobileBroadbandModem = interface;
  PIMobileBroadbandModem = ^IMobileBroadbandModem;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandAccount
  IMobileBroadbandAccount = interface;
  PIMobileBroadbandAccount = ^IMobileBroadbandAccount;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandNetwork
  IMobileBroadbandNetwork = interface;
  PIMobileBroadbandNetwork = ^IMobileBroadbandNetwork;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandDeviceInformation
  IMobileBroadbandDeviceInformation = interface;
  PIMobileBroadbandDeviceInformation = ^IMobileBroadbandDeviceInformation;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  IVectorView_1__IMobileBroadbandDeviceServiceInformation = interface;
  PIVectorView_1__IMobileBroadbandDeviceServiceInformation = ^IVectorView_1__IMobileBroadbandDeviceServiceInformation;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation
  IMobileBroadbandDeviceServiceInformation = interface;
  PIMobileBroadbandDeviceServiceInformation = ^IMobileBroadbandDeviceServiceInformation;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandDeviceService
  IMobileBroadbandDeviceService = interface;
  PIMobileBroadbandDeviceService = ^IMobileBroadbandDeviceService;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataSession
  IMobileBroadbandDeviceServiceDataSession = interface;
  PIMobileBroadbandDeviceServiceDataSession = ^IMobileBroadbandDeviceServiceDataSession;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataSession,Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataReceivedEventArgs>
  TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs = interface;
  PTypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs = ^TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataReceivedEventArgs
  IMobileBroadbandDeviceServiceDataReceivedEventArgs = interface;
  PIMobileBroadbandDeviceServiceDataReceivedEventArgs = ^IMobileBroadbandDeviceServiceDataReceivedEventArgs;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandSession
  IMobileBroadbandDeviceServiceCommandSession = interface;
  PIMobileBroadbandDeviceServiceCommandSession = ^IMobileBroadbandDeviceServiceCommandSession;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult>
  IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult = interface;
  PIAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult = ^IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult = interface;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult = ^AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult
  IMobileBroadbandDeviceServiceCommandResult = interface;
  PIMobileBroadbandDeviceServiceCommandResult = ^IMobileBroadbandDeviceServiceCommandResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration>
  IAsyncOperation_1__IMobileBroadbandModemConfiguration = interface;
  PIAsyncOperation_1__IMobileBroadbandModemConfiguration = ^IAsyncOperation_1__IMobileBroadbandModemConfiguration;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration>
  AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration = interface;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration = ^AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration
  IMobileBroadbandModemConfiguration = interface;
  PIMobileBroadbandModemConfiguration = ^IMobileBroadbandModemConfiguration;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandUicc
  IMobileBroadbandUicc = interface;
  PIMobileBroadbandUicc = ^IMobileBroadbandUicc;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult>
  IAsyncOperation_1__IMobileBroadbandUiccAppsResult = interface;
  PIAsyncOperation_1__IMobileBroadbandUiccAppsResult = ^IAsyncOperation_1__IMobileBroadbandUiccAppsResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult = interface;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult = ^AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult
  IMobileBroadbandUiccAppsResult = interface;
  PIMobileBroadbandUiccAppsResult = ^IMobileBroadbandUiccAppsResult;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  IVectorView_1__IMobileBroadbandUiccApp = interface;
  PIVectorView_1__IMobileBroadbandUiccApp = ^IVectorView_1__IMobileBroadbandUiccApp;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp
  IMobileBroadbandUiccApp = interface;
  PIMobileBroadbandUiccApp = ^IMobileBroadbandUiccApp;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult>
  IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult = interface;
  PIAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult = ^IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult = interface;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult = ^AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult
  IMobileBroadbandUiccAppRecordDetailsResult = interface;
  PIMobileBroadbandUiccAppRecordDetailsResult = ^IMobileBroadbandUiccAppRecordDetailsResult;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult>
  IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult = interface;
  PIAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult = ^IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult = interface;
  PAsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult = ^AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult;

  // External: Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult
  IMobileBroadbandUiccAppReadRecordResult = interface;
  PIMobileBroadbandUiccAppReadRecordResult = ^IMobileBroadbandUiccAppReadRecordResult;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Networking.IHostName>
  IVectorView_1__IHostName = interface;
  PIVectorView_1__IHostName = ^IVectorView_1__IHostName;

  // External: Windows.Data.Xml.Dom.IXmlDocument
  Xml_Dom_IXmlDocument = interface;
  PXml_Dom_IXmlDocument = ^Xml_Dom_IXmlDocument;

  // External: Windows.Data.Xml.Dom.IXmlDocumentType
  Xml_Dom_IXmlDocumentType = interface;
  PXml_Dom_IXmlDocumentType = ^Xml_Dom_IXmlDocumentType;

  // External: Windows.Data.Xml.Dom.IXmlNamedNodeMap
  Xml_Dom_IXmlNamedNodeMap = interface;
  PXml_Dom_IXmlNamedNodeMap = ^Xml_Dom_IXmlNamedNodeMap;

  // External: Windows.Data.Xml.Dom.IXmlNode
  Xml_Dom_IXmlNode = interface;
  PXml_Dom_IXmlNode = ^Xml_Dom_IXmlNode;

  // External: Windows.Data.Xml.Dom.IXmlNodeList
  Xml_Dom_IXmlNodeList = interface;
  PXml_Dom_IXmlNodeList = ^Xml_Dom_IXmlNodeList;

  // External: Windows.Data.Xml.Dom.IXmlDomImplementation
  Xml_Dom_IXmlDomImplementation = interface;
  PXml_Dom_IXmlDomImplementation = ^Xml_Dom_IXmlDomImplementation;

  // External: Windows.Data.Xml.Dom.IXmlElement
  Xml_Dom_IXmlElement = interface;
  PXml_Dom_IXmlElement = ^Xml_Dom_IXmlElement;

  // External: Windows.Data.Xml.Dom.IXmlAttribute
  Xml_Dom_IXmlAttribute = interface;
  PXml_Dom_IXmlAttribute = ^Xml_Dom_IXmlAttribute;

  // External: Windows.Data.Xml.Dom.IXmlDocumentFragment
  Xml_Dom_IXmlDocumentFragment = interface;
  PXml_Dom_IXmlDocumentFragment = ^Xml_Dom_IXmlDocumentFragment;

  // External: Windows.Data.Xml.Dom.IXmlText
  Xml_Dom_IXmlText = interface;
  PXml_Dom_IXmlText = ^Xml_Dom_IXmlText;

  // External: Windows.Data.Xml.Dom.IXmlComment
  Xml_Dom_IXmlComment = interface;
  PXml_Dom_IXmlComment = ^Xml_Dom_IXmlComment;

  // External: Windows.Data.Xml.Dom.IXmlProcessingInstruction
  Xml_Dom_IXmlProcessingInstruction = interface;
  PXml_Dom_IXmlProcessingInstruction = ^Xml_Dom_IXmlProcessingInstruction;

  // External: Windows.Data.Xml.Dom.IXmlEntityReference
  Xml_Dom_IXmlEntityReference = interface;
  PXml_Dom_IXmlEntityReference = ^Xml_Dom_IXmlEntityReference;

  // External: Windows.Data.Xml.Dom.IXmlCDataSection
  Xml_Dom_IXmlCDataSection = interface;
  PXml_Dom_IXmlCDataSection = ^Xml_Dom_IXmlCDataSection;

  // External: Windows.Foundation.Collections.IVector`1<UInt32>
  IVector_1__Cardinal = interface;
  PIVector_1__Cardinal = ^IVector_1__Cardinal;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IDisplayRegion,Object>
  TypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable = interface;
  PTypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable = ^TypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable;

  // External: Windows.UI.WindowManagement.IDisplayRegion
  WindowManagement_IDisplayRegion = interface;
  PWindowManagement_IDisplayRegion = ^WindowManagement_IDisplayRegion;

  // External: Windows.UI.WindowManagement.IWindowingEnvironment
  WindowManagement_IWindowingEnvironment = interface;
  PWindowManagement_IWindowingEnvironment = ^WindowManagement_IWindowingEnvironment;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.WindowManagement.IDisplayRegion>
  IVectorView_1__WindowManagement_IDisplayRegion = interface;
  PIVectorView_1__WindowManagement_IDisplayRegion = ^IVectorView_1__WindowManagement_IDisplayRegion;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IWindowingEnvironment,Windows.UI.WindowManagement.IWindowingEnvironmentChangedEventArgs>
  TypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs = interface;
  PTypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs = ^TypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs;

  // External: Windows.UI.WindowManagement.IWindowingEnvironmentChangedEventArgs
  WindowManagement_IWindowingEnvironmentChangedEventArgs = interface;
  PWindowManagement_IWindowingEnvironmentChangedEventArgs = ^WindowManagement_IWindowingEnvironmentChangedEventArgs;

  // External: Windows.Foundation.Collections.IIterator`1<Single>
  IIterator_1__Single = interface;
  PIIterator_1__Single = ^IIterator_1__Single;

  // External: Windows.Foundation.Collections.IVectorView`1<Single>
  IVectorView_1__Single = interface;
  PIVectorView_1__Single = ^IVectorView_1__Single;

  // External: Windows.Graphics.Effects.IGraphicsEffect
  Effects_IGraphicsEffect = interface;
  PEffects_IGraphicsEffect = ^Effects_IGraphicsEffect;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.Core.ICompositorController,Object>
  TypedEventHandler_2__Core_ICompositorController__IInspectable = interface;
  PTypedEventHandler_2__Core_ICompositorController__IInspectable = ^TypedEventHandler_2__Core_ICompositorController__IInspectable;

  // External: Windows.UI.Composition.Core.ICompositorController
  Core_ICompositorController = interface;
  PCore_ICompositorController = ^Core_ICompositorController;

  // External: Windows.UI.Composition.ICompositor
  ICompositor = interface;
  PICompositor = ^ICompositor;

  // External: Windows.UI.Composition.IColorKeyFrameAnimation
  IColorKeyFrameAnimation = interface;
  PIColorKeyFrameAnimation = ^IColorKeyFrameAnimation;

  // External: Windows.UI.Composition.ICompositionEasingFunction
  ICompositionEasingFunction = interface;
  PICompositionEasingFunction = ^ICompositionEasingFunction;

  // External: Windows.UI.Composition.ICompositionColorBrush
  ICompositionColorBrush = interface;
  PICompositionColorBrush = ^ICompositionColorBrush;

  // External: Windows.UI.Composition.IContainerVisual
  IContainerVisual = interface;
  PIContainerVisual = ^IContainerVisual;

  // External: Windows.UI.Composition.IVisualCollection
  IVisualCollection = interface;
  PIVisualCollection = ^IVisualCollection;

  // External: Windows.UI.Composition.IVisual
  IVisual = interface;
  PIVisual = ^IVisual;

  // External: Windows.UI.Composition.ICompositionClip
  ICompositionClip = interface;
  PICompositionClip = ^ICompositionClip;

  // External: Windows.UI.Composition.ICubicBezierEasingFunction
  ICubicBezierEasingFunction = interface;
  PICubicBezierEasingFunction = ^ICubicBezierEasingFunction;

  // External: Windows.UI.Composition.ICompositionEffectFactory
  ICompositionEffectFactory = interface;
  PICompositionEffectFactory = ^ICompositionEffectFactory;

  // External: Windows.UI.Composition.ICompositionEffectBrush
  ICompositionEffectBrush = interface;
  PICompositionEffectBrush = ^ICompositionEffectBrush;

  // External: Windows.UI.Composition.ICompositionBrush
  ICompositionBrush = interface;
  PICompositionBrush = ^ICompositionBrush;

  // External: Windows.UI.Composition.IExpressionAnimation
  IExpressionAnimation = interface;
  PIExpressionAnimation = ^IExpressionAnimation;

  // External: Windows.UI.Composition.IInsetClip
  IInsetClip = interface;
  PIInsetClip = ^IInsetClip;

  // External: Windows.UI.Composition.ILinearEasingFunction
  ILinearEasingFunction = interface;
  PILinearEasingFunction = ^ILinearEasingFunction;

  // External: Windows.UI.Composition.ICompositionPropertySet
  ICompositionPropertySet = interface;
  PICompositionPropertySet = ^ICompositionPropertySet;

  // External: Windows.UI.Composition.IQuaternionKeyFrameAnimation
  IQuaternionKeyFrameAnimation = interface;
  PIQuaternionKeyFrameAnimation = ^IQuaternionKeyFrameAnimation;

  // External: Windows.UI.Composition.IScalarKeyFrameAnimation
  IScalarKeyFrameAnimation = interface;
  PIScalarKeyFrameAnimation = ^IScalarKeyFrameAnimation;

  // External: Windows.UI.Composition.ICompositionScopedBatch
  ICompositionScopedBatch = interface;
  PICompositionScopedBatch = ^ICompositionScopedBatch;

  // External: Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Composition.ICompositionBatchCompletedEventArgs>
  TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs = interface;
  PTypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs = ^TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs;

  // External: Windows.UI.Composition.ICompositionBatchCompletedEventArgs
  ICompositionBatchCompletedEventArgs = interface;
  PICompositionBatchCompletedEventArgs = ^ICompositionBatchCompletedEventArgs;

  // External: Windows.UI.Composition.ISpriteVisual
  ISpriteVisual = interface;
  PISpriteVisual = ^ISpriteVisual;

  // External: Windows.UI.Composition.ICompositionSurfaceBrush
  ICompositionSurfaceBrush = interface;
  PICompositionSurfaceBrush = ^ICompositionSurfaceBrush;

  // External: Windows.UI.Composition.ICompositionSurface
  ICompositionSurface = interface;
  PICompositionSurface = ^ICompositionSurface;

  // External: Windows.UI.Composition.ICompositionTarget
  ICompositionTarget = interface;
  PICompositionTarget = ^ICompositionTarget;

  // External: Windows.UI.Composition.IVector2KeyFrameAnimation
  IVector2KeyFrameAnimation = interface;
  PIVector2KeyFrameAnimation = ^IVector2KeyFrameAnimation;

  // External: Windows.UI.Composition.IVector3KeyFrameAnimation
  IVector3KeyFrameAnimation = interface;
  PIVector3KeyFrameAnimation = ^IVector3KeyFrameAnimation;

  // External: Windows.UI.Composition.IVector4KeyFrameAnimation
  IVector4KeyFrameAnimation = interface;
  PIVector4KeyFrameAnimation = ^IVector4KeyFrameAnimation;

  // External: Windows.UI.Composition.ICompositionCommitBatch
  ICompositionCommitBatch = interface;
  PICompositionCommitBatch = ^ICompositionCommitBatch;

  // External: Windows.Graphics.IGeometrySource2D
  IGeometrySource2D = interface;
  PIGeometrySource2D = ^IGeometrySource2D;

  // External: Windows.Foundation.Collections.IVector`1<Single>
  IVector_1__Single = interface;
  PIVector_1__Single = ^IVector_1__Single;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.ICompositionCapabilities,Object>
  TypedEventHandler_2__ICompositionCapabilities__IInspectable = interface;
  PTypedEventHandler_2__ICompositionCapabilities__IInspectable = ^TypedEventHandler_2__ICompositionCapabilities__IInspectable;

  // External: Windows.UI.Composition.ICompositionCapabilities
  ICompositionCapabilities = interface;
  PICompositionCapabilities = ^ICompositionCapabilities;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkUnprocessedInput,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs = interface;
  PTypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs = ^TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs;

  // External: Windows.UI.Input.Inking.IInkUnprocessedInput
  IInkUnprocessedInput = interface;
  PIInkUnprocessedInput = ^IInkUnprocessedInput;

  // External: Windows.UI.Input.Inking.IInkPresenter
  IInkPresenter = interface;
  PIInkPresenter = ^IInkPresenter;

  // External: Windows.UI.Input.Inking.IInkStrokeInput
  IInkStrokeInput = interface;
  PIInkStrokeInput = ^IInkStrokeInput;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkStrokeInput,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs = interface;
  PTypedEventHandler_2__IInkStrokeInput__IPointerEventArgs = ^TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs;

  // External: Windows.UI.Input.Inking.IInkInputProcessingConfiguration
  IInkInputProcessingConfiguration = interface;
  PIInkInputProcessingConfiguration = ^IInkInputProcessingConfiguration;

  // External: Windows.UI.Input.Inking.IInkStrokeContainer
  IInkStrokeContainer = interface;
  PIInkStrokeContainer = ^IInkStrokeContainer;

  // External: Windows.UI.Input.Inking.IInkStroke
  IInkStroke = interface;
  PIInkStroke = ^IInkStroke;

  // External: Windows.UI.Input.Inking.IInkDrawingAttributes
  IInkDrawingAttributes = interface;
  PIInkDrawingAttributes = ^IInkDrawingAttributes;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Inking.IInkStrokeRenderingSegment>
  IVectorView_1__IInkStrokeRenderingSegment = interface;
  PIVectorView_1__IInkStrokeRenderingSegment = ^IVectorView_1__IInkStrokeRenderingSegment;

  // External: Windows.UI.Input.Inking.IInkStrokeRenderingSegment
  IInkStrokeRenderingSegment = interface;
  PIInkStrokeRenderingSegment = ^IInkStrokeRenderingSegment;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>
  IIterable_1__Point = interface;
  PIIterable_1__Point = ^IIterable_1__Point;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Point>
  IIterator_1__Point = interface;
  PIIterator_1__Point = ^IIterator_1__Point;

  // External: Windows.Foundation.IAsyncActionWithProgress`1<UInt64>
  IAsyncActionWithProgress_1__UInt64 = interface;
  PIAsyncActionWithProgress_1__UInt64 = ^IAsyncActionWithProgress_1__UInt64;

  // External: Windows.Foundation.AsyncActionProgressHandler`1<UInt64>
  AsyncActionProgressHandler_1__UInt64 = interface;
  PAsyncActionProgressHandler_1__UInt64 = ^AsyncActionProgressHandler_1__UInt64;

  // External: Windows.Foundation.AsyncActionWithProgressCompletedHandler`1<UInt64>
  AsyncActionWithProgressCompletedHandler_1__UInt64 = interface;
  PAsyncActionWithProgressCompletedHandler_1__UInt64 = ^AsyncActionWithProgressCompletedHandler_1__UInt64;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Inking.IInkRecognitionResult>
  IVectorView_1__IInkRecognitionResult = interface;
  PIVectorView_1__IInkRecognitionResult = ^IVectorView_1__IInkRecognitionResult;

  // External: Windows.UI.Input.Inking.IInkRecognitionResult
  IInkRecognitionResult = interface;
  PIInkRecognitionResult = ^IInkRecognitionResult;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Inking.IInkStroke>
  IVectorView_1__IInkStroke = interface;
  PIVectorView_1__IInkStroke = ^IVectorView_1__IInkStroke;

  // External: Windows.UI.Input.Inking.IInkSynchronizer
  IInkSynchronizer = interface;
  PIInkSynchronizer = ^IInkSynchronizer;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkPresenter,Windows.UI.Input.Inking.IInkStrokesCollectedEventArgs>
  TypedEventHandler_2__IInkPresenter__IInkStrokesCollectedEventArgs = interface;
  PTypedEventHandler_2__IInkPresenter__IInkStrokesCollectedEventArgs = ^TypedEventHandler_2__IInkPresenter__IInkStrokesCollectedEventArgs;

  // External: Windows.UI.Input.Inking.IInkStrokesCollectedEventArgs
  IInkStrokesCollectedEventArgs = interface;
  PIInkStrokesCollectedEventArgs = ^IInkStrokesCollectedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkPresenter,Windows.UI.Input.Inking.IInkStrokesErasedEventArgs>
  TypedEventHandler_2__IInkPresenter__IInkStrokesErasedEventArgs = interface;
  PTypedEventHandler_2__IInkPresenter__IInkStrokesErasedEventArgs = ^TypedEventHandler_2__IInkPresenter__IInkStrokesErasedEventArgs;

  // External: Windows.UI.Input.Inking.IInkStrokesErasedEventArgs
  IInkStrokesErasedEventArgs = interface;
  PIInkStrokesErasedEventArgs = ^IInkStrokesErasedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.Core.ICoreInkIndependentInputSource,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs = interface;
  PTypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs = ^TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs;

  // External: Windows.UI.Input.Inking.Core.ICoreInkIndependentInputSource
  Core_ICoreInkIndependentInputSource = interface;
  PCore_ICoreInkIndependentInputSource = ^Core_ICoreInkIndependentInputSource;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialControllerMenuItem,Object>
  TypedEventHandler_2__IRadialControllerMenuItem__IInspectable = interface;
  PTypedEventHandler_2__IRadialControllerMenuItem__IInspectable = ^TypedEventHandler_2__IRadialControllerMenuItem__IInspectable;

  // External: Windows.UI.Input.IRadialControllerMenuItem
  IRadialControllerMenuItem = interface;
  PIRadialControllerMenuItem = ^IRadialControllerMenuItem;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Object>
  TypedEventHandler_2__IRadialController__IInspectable = interface;
  PTypedEventHandler_2__IRadialController__IInspectable = ^TypedEventHandler_2__IRadialController__IInspectable;

  // External: Windows.UI.Input.IRadialController
  IRadialController = interface;
  PIRadialController = ^IRadialController;

  // External: Windows.UI.Input.IRadialControllerMenu
  IRadialControllerMenu = interface;
  PIRadialControllerMenu = ^IRadialControllerMenu;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Input.IRadialControllerMenuItem>
  IVector_1__IRadialControllerMenuItem = interface;
  PIVector_1__IRadialControllerMenuItem = ^IVector_1__IRadialControllerMenuItem;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.IRadialControllerMenuItem>
  IVectorView_1__IRadialControllerMenuItem = interface;
  PIVectorView_1__IRadialControllerMenuItem = ^IVectorView_1__IRadialControllerMenuItem;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerScreenContactStartedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs;

  // External: Windows.UI.Input.IRadialControllerScreenContactStartedEventArgs
  IRadialControllerScreenContactStartedEventArgs = interface;
  PIRadialControllerScreenContactStartedEventArgs = ^IRadialControllerScreenContactStartedEventArgs;

  // External: Windows.UI.Input.IRadialControllerScreenContact
  IRadialControllerScreenContact = interface;
  PIRadialControllerScreenContact = ^IRadialControllerScreenContact;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerScreenContactContinuedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs;

  // External: Windows.UI.Input.IRadialControllerScreenContactContinuedEventArgs
  IRadialControllerScreenContactContinuedEventArgs = interface;
  PIRadialControllerScreenContactContinuedEventArgs = ^IRadialControllerScreenContactContinuedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerRotationChangedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs;

  // External: Windows.UI.Input.IRadialControllerRotationChangedEventArgs
  IRadialControllerRotationChangedEventArgs = interface;
  PIRadialControllerRotationChangedEventArgs = ^IRadialControllerRotationChangedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonClickedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs;

  // External: Windows.UI.Input.IRadialControllerButtonClickedEventArgs
  IRadialControllerButtonClickedEventArgs = interface;
  PIRadialControllerButtonClickedEventArgs = ^IRadialControllerButtonClickedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerControlAcquiredEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs;

  // External: Windows.UI.Input.IRadialControllerControlAcquiredEventArgs
  IRadialControllerControlAcquiredEventArgs = interface;
  PIRadialControllerControlAcquiredEventArgs = ^IRadialControllerControlAcquiredEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Object>
  TypedEventHandler_2__IToastNotification__IInspectable = interface;
  PTypedEventHandler_2__IToastNotification__IInspectable = ^TypedEventHandler_2__IToastNotification__IInspectable;

  // External: Windows.UI.Notifications.IToastNotification
  IToastNotification = interface;
  PIToastNotification = ^IToastNotification;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Windows.UI.Notifications.IToastDismissedEventArgs>
  TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs = interface;
  PTypedEventHandler_2__IToastNotification__IToastDismissedEventArgs = ^TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs;

  // External: Windows.UI.Notifications.IToastDismissedEventArgs
  IToastDismissedEventArgs = interface;
  PIToastDismissedEventArgs = ^IToastDismissedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Windows.UI.Notifications.IToastFailedEventArgs>
  TypedEventHandler_2__IToastNotification__IToastFailedEventArgs = interface;
  PTypedEventHandler_2__IToastNotification__IToastFailedEventArgs = ^TypedEventHandler_2__IToastNotification__IToastFailedEventArgs;

  // External: Windows.UI.Notifications.IToastFailedEventArgs
  IToastFailedEventArgs = interface;
  PIToastFailedEventArgs = ^IToastFailedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.ViewManagement.Core.ICoreInputView,Object>
  TypedEventHandler_2__Core_ICoreInputView__IInspectable = interface;
  PTypedEventHandler_2__Core_ICoreInputView__IInspectable = ^TypedEventHandler_2__Core_ICoreInputView__IInspectable;

  // External: Windows.UI.ViewManagement.Core.ICoreInputView
  Core_ICoreInputView = interface;
  PCore_ICoreInputView = ^Core_ICoreInputView;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.ViewManagement.Core.ICoreInputView,Windows.UI.ViewManagement.Core.ICoreInputViewOcclusionsChangedEventArgs>
  TypedEventHandler_2__Core_ICoreInputView__Core_ICoreInputViewOcclusionsChangedEventArgs = interface;
  PTypedEventHandler_2__Core_ICoreInputView__Core_ICoreInputViewOcclusionsChangedEventArgs = ^TypedEventHandler_2__Core_ICoreInputView__Core_ICoreInputViewOcclusionsChangedEventArgs;

  // External: Windows.UI.ViewManagement.Core.ICoreInputViewOcclusionsChangedEventArgs
  Core_ICoreInputViewOcclusionsChangedEventArgs = interface;
  PCore_ICoreInputViewOcclusionsChangedEventArgs = ^Core_ICoreInputViewOcclusionsChangedEventArgs;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.ViewManagement.Core.ICoreInputViewOcclusion>
  IVectorView_1__Core_ICoreInputViewOcclusion = interface;
  PIVectorView_1__Core_ICoreInputViewOcclusion = ^IVectorView_1__Core_ICoreInputViewOcclusion;

  // External: Windows.UI.ViewManagement.Core.ICoreInputViewOcclusion
  Core_ICoreInputViewOcclusion = interface;
  PCore_ICoreInputViewOcclusion = ^Core_ICoreInputViewOcclusion;

  // External: Windows.UI.IUIContext
  IUIContext = interface;
  PIUIContext = ^IUIContext;

  // External: Windows.UI.Xaml.IStyle
  IStyle = interface;
  PIStyle = ^IStyle;

  // External: Windows.UI.Xaml.ISetterBaseCollection
  ISetterBaseCollection = interface;
  PISetterBaseCollection = ^ISetterBaseCollection;

  // External: Windows.UI.Xaml.Input.ICommand
  Input_ICommand = interface;
  PInput_ICommand = ^Input_ICommand;

  // External: Windows.UI.Xaml.RoutedEventHandler
  RoutedEventHandler = interface;
  PRoutedEventHandler = ^RoutedEventHandler;

  // External: Windows.UI.Xaml.IRoutedEventArgs
  IRoutedEventArgs = interface;
  PIRoutedEventArgs = ^IRoutedEventArgs;

  // External: Windows.UI.Xaml.IDataTemplate
  IDataTemplate = interface;
  PIDataTemplate = ^IDataTemplate;

  // External: Windows.UI.Xaml.IDependencyObject
  IDependencyObject = interface;
  PIDependencyObject = ^IDependencyObject;

  // External: Windows.UI.Xaml.IDependencyProperty
  IDependencyProperty = interface;
  PIDependencyProperty = ^IDependencyProperty;

  // External: Windows.UI.Xaml.IPropertyMetadata
  IPropertyMetadata = interface;
  PIPropertyMetadata = ^IPropertyMetadata;

  // External: Windows.UI.Xaml.CreateDefaultValueCallback
  CreateDefaultValueCallback = interface;
  PCreateDefaultValueCallback = ^CreateDefaultValueCallback;

  // External: Windows.Foundation.Collections.IObservableVector`1<Object>
  IObservableVector_1__IInspectable = interface;
  PIObservableVector_1__IInspectable = ^IObservableVector_1__IInspectable;

  // External: Windows.Foundation.Collections.VectorChangedEventHandler`1<Object>
  VectorChangedEventHandler_1__IInspectable = interface;
  PVectorChangedEventHandler_1__IInspectable = ^VectorChangedEventHandler_1__IInspectable;

  // External: Windows.UI.Xaml.ExceptionRoutedEventHandler
  ExceptionRoutedEventHandler = interface;
  PExceptionRoutedEventHandler = ^ExceptionRoutedEventHandler;

  // External: Windows.UI.Xaml.IExceptionRoutedEventArgs
  IExceptionRoutedEventArgs = interface;
  PIExceptionRoutedEventArgs = ^IExceptionRoutedEventArgs;

  // External: Windows.Foundation.Collections.IVector`1<Object>
  IVector_1__IInspectable = interface;
  PIVector_1__IInspectable = ^IVector_1__IInspectable;

  // External: Windows.Foundation.Collections.IVectorView`1<Object>
  IVectorView_1__IInspectable = interface;
  PIVectorView_1__IInspectable = ^IVectorView_1__IInspectable;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.UI.Xaml.Data.LoadMoreItemsResult>
  IAsyncOperation_1__Data_LoadMoreItemsResult = interface;
  PIAsyncOperation_1__Data_LoadMoreItemsResult = ^IAsyncOperation_1__Data_LoadMoreItemsResult;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Xaml.Data.LoadMoreItemsResult>
  AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult = interface;
  PAsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult = ^AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IRatingControl,Object>
  TypedEventHandler_2__IRatingControl__IInspectable = interface;
  PTypedEventHandler_2__IRatingControl__IInspectable = ^TypedEventHandler_2__IRatingControl__IInspectable;

  // External: Windows.UI.Xaml.Controls.IRatingControl
  IRatingControl = interface;
  PIRatingControl = ^IRatingControl;

  // External: Windows.UI.Xaml.Controls.IRatingItemInfo
  IRatingItemInfo = interface;
  PIRatingItemInfo = ^IRatingItemInfo;

  // External: Windows.UI.Xaml.Documents.ITextPointer
  Documents_ITextPointer = interface;
  PDocuments_ITextPointer = ^Documents_ITextPointer;

  // External: Windows.UI.Xaml.IFrameworkElement
  IFrameworkElement = interface;
  PIFrameworkElement = ^IFrameworkElement;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.ITriggerBase>
  IVector_1__ITriggerBase = interface;
  PIVector_1__ITriggerBase = ^IVector_1__ITriggerBase;

  // External: Windows.UI.Xaml.ITriggerBase
  ITriggerBase = interface;
  PITriggerBase = ^ITriggerBase;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.ITriggerBase>
  IVectorView_1__ITriggerBase = interface;
  PIVectorView_1__ITriggerBase = ^IVectorView_1__ITriggerBase;

  // External: Windows.UI.Xaml.IResourceDictionary
  IResourceDictionary = interface;
  PIResourceDictionary = ^IResourceDictionary;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IResourceDictionary>
  IVector_1__IResourceDictionary = interface;
  PIVector_1__IResourceDictionary = ^IVector_1__IResourceDictionary;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.IResourceDictionary>
  IVectorView_1__IResourceDictionary = interface;
  PIVectorView_1__IResourceDictionary = ^IVectorView_1__IResourceDictionary;

  // External: Windows.Foundation.Collections.IMap`2<Object,Object>
  IMap_2__IInspectable__IInspectable = interface;
  PIMap_2__IInspectable__IInspectable = ^IMap_2__IInspectable__IInspectable;

  // External: Windows.Foundation.Collections.IMapView`2<Object,Object>
  IMapView_2__IInspectable__IInspectable = interface;
  PIMapView_2__IInspectable__IInspectable = ^IMapView_2__IInspectable__IInspectable;

  // External: Windows.UI.Xaml.SizeChangedEventHandler
  SizeChangedEventHandler = interface;
  PSizeChangedEventHandler = ^SizeChangedEventHandler;

  // External: Windows.UI.Xaml.ISizeChangedEventArgs
  ISizeChangedEventArgs = interface;
  PISizeChangedEventArgs = ^ISizeChangedEventArgs;

  // External: Windows.UI.Xaml.Data.IBindingBase
  Data_IBindingBase = interface;
  PData_IBindingBase = ^Data_IBindingBase;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.IBlock>
  IVector_1__Documents_IBlock = interface;
  PIVector_1__Documents_IBlock = ^IVector_1__Documents_IBlock;

  // External: Windows.UI.Xaml.Documents.IBlock
  Documents_IBlock = interface;
  PDocuments_IBlock = ^Documents_IBlock;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.IBlock>
  IVectorView_1__Documents_IBlock = interface;
  PIVectorView_1__Documents_IBlock = ^IVectorView_1__Documents_IBlock;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.IRoutedEventArgs>
  TypedEventHandler_2__ISearchBox__IRoutedEventArgs = interface;
  PTypedEventHandler_2__ISearchBox__IRoutedEventArgs = ^TypedEventHandler_2__ISearchBox__IRoutedEventArgs;

  // External: Windows.UI.Xaml.Controls.ISearchBox
  ISearchBox = interface;
  PISearchBox = ^ISearchBox;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxQueryChangedEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxQueryChangedEventArgs = interface;
  PTypedEventHandler_2__ISearchBox__ISearchBoxQueryChangedEventArgs = ^TypedEventHandler_2__ISearchBox__ISearchBoxQueryChangedEventArgs;

  // External: Windows.UI.Xaml.Controls.ISearchBoxQueryChangedEventArgs
  ISearchBoxQueryChangedEventArgs = interface;
  PISearchBoxQueryChangedEventArgs = ^ISearchBoxQueryChangedEventArgs;

  // External: Windows.ApplicationModel.Search.ISearchQueryLinguisticDetails
  Search_ISearchQueryLinguisticDetails = interface;
  PSearch_ISearchQueryLinguisticDetails = ^Search_ISearchQueryLinguisticDetails;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxSuggestionsRequestedEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxSuggestionsRequestedEventArgs = interface;
  PTypedEventHandler_2__ISearchBox__ISearchBoxSuggestionsRequestedEventArgs = ^TypedEventHandler_2__ISearchBox__ISearchBoxSuggestionsRequestedEventArgs;

  // External: Windows.UI.Xaml.Controls.ISearchBoxSuggestionsRequestedEventArgs
  ISearchBoxSuggestionsRequestedEventArgs = interface;
  PISearchBoxSuggestionsRequestedEventArgs = ^ISearchBoxSuggestionsRequestedEventArgs;

  // External: Windows.ApplicationModel.Search.ISearchSuggestionsRequest
  Search_ISearchSuggestionsRequest = interface;
  PSearch_ISearchSuggestionsRequest = ^Search_ISearchSuggestionsRequest;

  // External: Windows.ApplicationModel.Search.ISearchSuggestionCollection
  Search_ISearchSuggestionCollection = interface;
  PSearch_ISearchSuggestionCollection = ^Search_ISearchSuggestionCollection;

  // External: Windows.ApplicationModel.Search.ISearchSuggestionsRequestDeferral
  Search_ISearchSuggestionsRequestDeferral = interface;
  PSearch_ISearchSuggestionsRequestDeferral = ^Search_ISearchSuggestionsRequestDeferral;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxQuerySubmittedEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxQuerySubmittedEventArgs = interface;
  PTypedEventHandler_2__ISearchBox__ISearchBoxQuerySubmittedEventArgs = ^TypedEventHandler_2__ISearchBox__ISearchBoxQuerySubmittedEventArgs;

  // External: Windows.UI.Xaml.Controls.ISearchBoxQuerySubmittedEventArgs
  ISearchBoxQuerySubmittedEventArgs = interface;
  PISearchBoxQuerySubmittedEventArgs = ^ISearchBoxQuerySubmittedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxResultSuggestionChosenEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxResultSuggestionChosenEventArgs = interface;
  PTypedEventHandler_2__ISearchBox__ISearchBoxResultSuggestionChosenEventArgs = ^TypedEventHandler_2__ISearchBox__ISearchBoxResultSuggestionChosenEventArgs;

  // External: Windows.UI.Xaml.Controls.ISearchBoxResultSuggestionChosenEventArgs
  ISearchBoxResultSuggestionChosenEventArgs = interface;
  PISearchBoxResultSuggestionChosenEventArgs = ^ISearchBoxResultSuggestionChosenEventArgs;

  // External: Windows.ApplicationModel.Search.ILocalContentSuggestionSettings
  Search_ILocalContentSuggestionSettings = interface;
  PSearch_ILocalContentSuggestionSettings = ^Search_ILocalContentSuggestionSettings;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageFolder>
  IVector_1__IStorageFolder = interface;
  PIVector_1__IStorageFolder = ^IVector_1__IStorageFolder;

  // External: Windows.UI.Xaml.Data.IValueConverter
  Data_IValueConverter = interface;
  PData_IValueConverter = ^Data_IValueConverter;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.IInline>
  IVector_1__Documents_IInline = interface;
  PIVector_1__Documents_IInline = ^IVector_1__Documents_IInline;

  // External: Windows.UI.Xaml.Documents.IInline
  Documents_IInline = interface;
  PDocuments_IInline = ^Documents_IInline;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.IInline>
  IVectorView_1__Documents_IInline = interface;
  PIVectorView_1__Documents_IInline = ^IVectorView_1__Documents_IInline;

  // External: Windows.UI.Xaml.Input.IInputScope
  Input_IInputScope = interface;
  PInput_IInputScope = ^Input_IInputScope;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Input.IInputScopeName>
  IVector_1__Input_IInputScopeName = interface;
  PIVector_1__Input_IInputScopeName = ^IVector_1__Input_IInputScopeName;

  // External: Windows.UI.Xaml.Input.IInputScopeName
  Input_IInputScopeName = interface;
  PInput_IInputScopeName = ^Input_IInputScopeName;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Input.IInputScopeName>
  IVectorView_1__Input_IInputScopeName = interface;
  PIVectorView_1__Input_IInputScopeName = ^IVectorView_1__Input_IInputScopeName;

  // External: Windows.UI.Xaml.IUIElement
  IUIElement = interface;
  PIUIElement = ^IUIElement;

  // External: Windows.UI.Xaml.Media.IRectangleGeometry
  IRectangleGeometry = interface;
  PIRectangleGeometry = ^IRectangleGeometry;

  // External: Windows.UI.Xaml.Media.ITransform
  ITransform = interface;
  PITransform = ^ITransform;

  // External: Windows.UI.Xaml.Media.IProjection
  IProjection = interface;
  PIProjection = ^IProjection;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IVector_1__Animation_ITransition = interface;
  PIVector_1__Animation_ITransition = ^IVector_1__Animation_ITransition;

  // External: Windows.UI.Xaml.Media.Animation.ITransition
  Animation_ITransition = interface;
  PAnimation_ITransition = ^Animation_ITransition;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IVectorView_1__Animation_ITransition = interface;
  PIVectorView_1__Animation_ITransition = ^IVectorView_1__Animation_ITransition;

  // External: Windows.UI.Xaml.Media.ICacheMode
  ICacheMode = interface;
  PICacheMode = ^ICacheMode;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Input.IPointer>
  IVectorView_1__Input_IPointer = interface;
  PIVectorView_1__Input_IPointer = ^IVectorView_1__Input_IPointer;

  // External: Windows.UI.Xaml.Input.IPointer
  Input_IPointer = interface;
  PInput_IPointer = ^Input_IPointer;

  // External: Windows.UI.Xaml.Input.KeyEventHandler
  Input_KeyEventHandler = interface;
  PInput_KeyEventHandler = ^Input_KeyEventHandler;

  // External: Windows.UI.Xaml.Input.IKeyRoutedEventArgs
  Input_IKeyRoutedEventArgs = interface;
  PInput_IKeyRoutedEventArgs = ^Input_IKeyRoutedEventArgs;

  // External: Windows.UI.Xaml.DragEventHandler
  DragEventHandler = interface;
  PDragEventHandler = ^DragEventHandler;

  // External: Windows.UI.Xaml.IDragEventArgs
  IDragEventArgs = interface;
  PIDragEventArgs = ^IDragEventArgs;

  // External: Windows.UI.Xaml.Input.PointerEventHandler
  Input_PointerEventHandler = interface;
  PInput_PointerEventHandler = ^Input_PointerEventHandler;

  // External: Windows.UI.Xaml.Input.IPointerRoutedEventArgs
  Input_IPointerRoutedEventArgs = interface;
  PInput_IPointerRoutedEventArgs = ^Input_IPointerRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.TappedEventHandler
  Input_TappedEventHandler = interface;
  PInput_TappedEventHandler = ^Input_TappedEventHandler;

  // External: Windows.UI.Xaml.Input.ITappedRoutedEventArgs
  Input_ITappedRoutedEventArgs = interface;
  PInput_ITappedRoutedEventArgs = ^Input_ITappedRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.DoubleTappedEventHandler
  Input_DoubleTappedEventHandler = interface;
  PInput_DoubleTappedEventHandler = ^Input_DoubleTappedEventHandler;

  // External: Windows.UI.Xaml.Input.IDoubleTappedRoutedEventArgs
  Input_IDoubleTappedRoutedEventArgs = interface;
  PInput_IDoubleTappedRoutedEventArgs = ^Input_IDoubleTappedRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.HoldingEventHandler
  Input_HoldingEventHandler = interface;
  PInput_HoldingEventHandler = ^Input_HoldingEventHandler;

  // External: Windows.UI.Xaml.Input.IHoldingRoutedEventArgs
  Input_IHoldingRoutedEventArgs = interface;
  PInput_IHoldingRoutedEventArgs = ^Input_IHoldingRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.RightTappedEventHandler
  Input_RightTappedEventHandler = interface;
  PInput_RightTappedEventHandler = ^Input_RightTappedEventHandler;

  // External: Windows.UI.Xaml.Input.IRightTappedRoutedEventArgs
  Input_IRightTappedRoutedEventArgs = interface;
  PInput_IRightTappedRoutedEventArgs = ^Input_IRightTappedRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.ManipulationStartingEventHandler
  Input_ManipulationStartingEventHandler = interface;
  PInput_ManipulationStartingEventHandler = ^Input_ManipulationStartingEventHandler;

  // External: Windows.UI.Xaml.Input.IManipulationStartingRoutedEventArgs
  Input_IManipulationStartingRoutedEventArgs = interface;
  PInput_IManipulationStartingRoutedEventArgs = ^Input_IManipulationStartingRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.IManipulationPivot
  Input_IManipulationPivot = interface;
  PInput_IManipulationPivot = ^Input_IManipulationPivot;

  // External: Windows.UI.Xaml.Input.ManipulationInertiaStartingEventHandler
  Input_ManipulationInertiaStartingEventHandler = interface;
  PInput_ManipulationInertiaStartingEventHandler = ^Input_ManipulationInertiaStartingEventHandler;

  // External: Windows.UI.Xaml.Input.IManipulationInertiaStartingRoutedEventArgs
  Input_IManipulationInertiaStartingRoutedEventArgs = interface;
  PInput_IManipulationInertiaStartingRoutedEventArgs = ^Input_IManipulationInertiaStartingRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.IInertiaExpansionBehavior
  Input_IInertiaExpansionBehavior = interface;
  PInput_IInertiaExpansionBehavior = ^Input_IInertiaExpansionBehavior;

  // External: Windows.UI.Xaml.Input.IInertiaRotationBehavior
  Input_IInertiaRotationBehavior = interface;
  PInput_IInertiaRotationBehavior = ^Input_IInertiaRotationBehavior;

  // External: Windows.UI.Xaml.Input.IInertiaTranslationBehavior
  Input_IInertiaTranslationBehavior = interface;
  PInput_IInertiaTranslationBehavior = ^Input_IInertiaTranslationBehavior;

  // External: Windows.UI.Xaml.Input.ManipulationStartedEventHandler
  Input_ManipulationStartedEventHandler = interface;
  PInput_ManipulationStartedEventHandler = ^Input_ManipulationStartedEventHandler;

  // External: Windows.UI.Xaml.Input.IManipulationStartedRoutedEventArgs
  Input_IManipulationStartedRoutedEventArgs = interface;
  PInput_IManipulationStartedRoutedEventArgs = ^Input_IManipulationStartedRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.ManipulationDeltaEventHandler
  Input_ManipulationDeltaEventHandler = interface;
  PInput_ManipulationDeltaEventHandler = ^Input_ManipulationDeltaEventHandler;

  // External: Windows.UI.Xaml.Input.IManipulationDeltaRoutedEventArgs
  Input_IManipulationDeltaRoutedEventArgs = interface;
  PInput_IManipulationDeltaRoutedEventArgs = ^Input_IManipulationDeltaRoutedEventArgs;

  // External: Windows.UI.Xaml.Input.ManipulationCompletedEventHandler
  Input_ManipulationCompletedEventHandler = interface;
  PInput_ManipulationCompletedEventHandler = ^Input_ManipulationCompletedEventHandler;

  // External: Windows.UI.Xaml.Input.IManipulationCompletedRoutedEventArgs
  Input_IManipulationCompletedRoutedEventArgs = interface;
  PInput_IManipulationCompletedRoutedEventArgs = ^Input_IManipulationCompletedRoutedEventArgs;

  // External: Windows.UI.Xaml.IRoutedEvent
  IRoutedEvent = interface;
  PIRoutedEvent = ^IRoutedEvent;

  // External: Windows.UI.Xaml.Media.IGeneralTransform
  IGeneralTransform = interface;
  PIGeneralTransform = ^IGeneralTransform;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IUIElement>
  IVector_1__IUIElement = interface;
  PIVector_1__IUIElement = ^IVector_1__IUIElement;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.IUIElement>
  IVectorView_1__IUIElement = interface;
  PIVectorView_1__IUIElement = ^IVectorView_1__IUIElement;

  // External: Windows.UI.Xaml.IBrushTransition
  IBrushTransition = interface;
  PIBrushTransition = ^IBrushTransition;

  // External: Windows.UI.Xaml.DependencyPropertyChangedEventHandler
  DependencyPropertyChangedEventHandler = interface;
  PDependencyPropertyChangedEventHandler = ^DependencyPropertyChangedEventHandler;

  // External: Windows.UI.Xaml.IDependencyPropertyChangedEventArgs
  IDependencyPropertyChangedEventArgs = interface;
  PIDependencyPropertyChangedEventArgs = ^IDependencyPropertyChangedEventArgs;

  // External: Windows.UI.Xaml.Input.ICharacterReceivedRoutedEventArgs
  Input_ICharacterReceivedRoutedEventArgs = interface;
  PInput_ICharacterReceivedRoutedEventArgs = ^Input_ICharacterReceivedRoutedEventArgs;

  // External: Windows.UI.Xaml.Navigation.NavigatedEventHandler
  Navigation_NavigatedEventHandler = interface;
  PNavigation_NavigatedEventHandler = ^Navigation_NavigatedEventHandler;

  // External: Windows.UI.Xaml.Navigation.INavigationEventArgs
  Navigation_INavigationEventArgs = interface;
  PNavigation_INavigationEventArgs = ^Navigation_INavigationEventArgs;

  // External: Windows.UI.Xaml.Navigation.NavigatingCancelEventHandler
  Navigation_NavigatingCancelEventHandler = interface;
  PNavigation_NavigatingCancelEventHandler = ^Navigation_NavigatingCancelEventHandler;

  // External: Windows.UI.Xaml.Navigation.INavigatingCancelEventArgs
  Navigation_INavigatingCancelEventArgs = interface;
  PNavigation_INavigatingCancelEventArgs = ^Navigation_INavigatingCancelEventArgs;

  // External: Windows.UI.Xaml.Navigation.NavigationFailedEventHandler
  Navigation_NavigationFailedEventHandler = interface;
  PNavigation_NavigationFailedEventHandler = ^Navigation_NavigationFailedEventHandler;

  // External: Windows.UI.Xaml.Navigation.INavigationFailedEventArgs
  Navigation_INavigationFailedEventArgs = interface;
  PNavigation_INavigationFailedEventArgs = ^Navigation_INavigationFailedEventArgs;

  // External: Windows.UI.Xaml.Navigation.NavigationStoppedEventHandler
  Navigation_NavigationStoppedEventHandler = interface;
  PNavigation_NavigationStoppedEventHandler = ^Navigation_NavigationStoppedEventHandler;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Navigation.IPageStackEntry>
  IVector_1__Navigation_IPageStackEntry = interface;
  PIVector_1__Navigation_IPageStackEntry = ^IVector_1__Navigation_IPageStackEntry;

  // External: Windows.UI.Xaml.Navigation.IPageStackEntry
  Navigation_IPageStackEntry = interface;
  PNavigation_IPageStackEntry = ^Navigation_IPageStackEntry;

  // External: Windows.UI.Xaml.Media.Animation.INavigationTransitionInfo
  Animation_INavigationTransitionInfo = interface;
  PAnimation_INavigationTransitionInfo = ^Animation_INavigationTransitionInfo;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Navigation.IPageStackEntry>
  IVectorView_1__Navigation_IPageStackEntry = interface;
  PIVectorView_1__Navigation_IPageStackEntry = ^IVectorView_1__Navigation_IPageStackEntry;

  // External: Windows.UI.Xaml.Navigation.IFrameNavigationOptions
  Navigation_IFrameNavigationOptions = interface;
  PNavigation_IFrameNavigationOptions = ^Navigation_IFrameNavigationOptions;

  // External: Windows.Foundation.Collections.IObservableVector`1<Windows.UI.Xaml.IDependencyObject>
  IObservableVector_1__IDependencyObject = interface;
  PIObservableVector_1__IDependencyObject = ^IObservableVector_1__IDependencyObject;

  // External: Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.UI.Xaml.IDependencyObject>
  VectorChangedEventHandler_1__IDependencyObject = interface;
  PVectorChangedEventHandler_1__IDependencyObject = ^VectorChangedEventHandler_1__IDependencyObject;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IInkToolbar,Object>
  TypedEventHandler_2__IInkToolbar__IInspectable = interface;
  PTypedEventHandler_2__IInkToolbar__IInspectable = ^TypedEventHandler_2__IInkToolbar__IInspectable;

  // External: Windows.UI.Xaml.Controls.IInkToolbar
  IInkToolbar = interface;
  PIInkToolbar = ^IInkToolbar;

  // External: Windows.UI.Xaml.Controls.IInkToolbarToolButton
  IInkToolbarToolButton = interface;
  PIInkToolbarToolButton = ^IInkToolbarToolButton;

  // External: Windows.UI.Xaml.Controls.IInkCanvas
  IInkCanvas = interface;
  PIInkCanvas = ^IInkCanvas;

  // External: Windows.UI.Xaml.Controls.IInkToolbarToggleButton
  IInkToolbarToggleButton = interface;
  PIInkToolbarToggleButton = ^IInkToolbarToggleButton;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IInkToolbarFlyoutItem,Object>
  TypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable = interface;
  PTypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable = ^TypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable;

  // External: Windows.UI.Xaml.Controls.IInkToolbarFlyoutItem
  IInkToolbarFlyoutItem = interface;
  PIInkToolbarFlyoutItem = ^IInkToolbarFlyoutItem;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Object>>
  IAsyncOperation_1__IVectorView_1__IInspectable = interface;
  PIAsyncOperation_1__IVectorView_1__IInspectable = ^IAsyncOperation_1__IVectorView_1__IInspectable;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Object>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IInspectable = ^AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Data.IItemIndexRange>
  IVectorView_1__Data_IItemIndexRange = interface;
  PIVectorView_1__Data_IItemIndexRange = ^IVectorView_1__Data_IItemIndexRange;

  // External: Windows.UI.Xaml.Data.IItemIndexRange
  Data_IItemIndexRange = interface;
  PData_IItemIndexRange = ^Data_IItemIndexRange;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Object>
  TypedEventHandler_2__INavigationView__IInspectable = interface;
  PTypedEventHandler_2__INavigationView__IInspectable = ^TypedEventHandler_2__INavigationView__IInspectable;

  // External: Windows.UI.Xaml.Controls.INavigationView
  INavigationView = interface;
  PINavigationView = ^INavigationView;

  // External: Windows.UI.Xaml.Controls.IAutoSuggestBox
  IAutoSuggestBox = interface;
  PIAutoSuggestBox = ^IAutoSuggestBox;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IAutoSuggestBox,Windows.UI.Xaml.Controls.IAutoSuggestBoxSuggestionChosenEventArgs>
  TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxSuggestionChosenEventArgs = interface;
  PTypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxSuggestionChosenEventArgs = ^TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxSuggestionChosenEventArgs;

  // External: Windows.UI.Xaml.Controls.IAutoSuggestBoxSuggestionChosenEventArgs
  IAutoSuggestBoxSuggestionChosenEventArgs = interface;
  PIAutoSuggestBoxSuggestionChosenEventArgs = ^IAutoSuggestBoxSuggestionChosenEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IAutoSuggestBox,Windows.UI.Xaml.Controls.IAutoSuggestBoxTextChangedEventArgs>
  TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxTextChangedEventArgs = interface;
  PTypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxTextChangedEventArgs = ^TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxTextChangedEventArgs;

  // External: Windows.UI.Xaml.Controls.IAutoSuggestBoxTextChangedEventArgs
  IAutoSuggestBoxTextChangedEventArgs = interface;
  PIAutoSuggestBoxTextChangedEventArgs = ^IAutoSuggestBoxTextChangedEventArgs;

  // External: Windows.UI.Xaml.Controls.IDataTemplateSelector
  IDataTemplateSelector = interface;
  PIDataTemplateSelector = ^IDataTemplateSelector;

  // External: Windows.UI.Xaml.Controls.IStyleSelector
  IStyleSelector = interface;
  PIStyleSelector = ^IStyleSelector;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewSelectionChangedEventArgs>
  TypedEventHandler_2__INavigationView__INavigationViewSelectionChangedEventArgs = interface;
  PTypedEventHandler_2__INavigationView__INavigationViewSelectionChangedEventArgs = ^TypedEventHandler_2__INavigationView__INavigationViewSelectionChangedEventArgs;

  // External: Windows.UI.Xaml.Controls.INavigationViewSelectionChangedEventArgs
  INavigationViewSelectionChangedEventArgs = interface;
  PINavigationViewSelectionChangedEventArgs = ^INavigationViewSelectionChangedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewItemInvokedEventArgs>
  TypedEventHandler_2__INavigationView__INavigationViewItemInvokedEventArgs = interface;
  PTypedEventHandler_2__INavigationView__INavigationViewItemInvokedEventArgs = ^TypedEventHandler_2__INavigationView__INavigationViewItemInvokedEventArgs;

  // External: Windows.UI.Xaml.Controls.INavigationViewItemInvokedEventArgs
  INavigationViewItemInvokedEventArgs = interface;
  PINavigationViewItemInvokedEventArgs = ^INavigationViewItemInvokedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewDisplayModeChangedEventArgs>
  TypedEventHandler_2__INavigationView__INavigationViewDisplayModeChangedEventArgs = interface;
  PTypedEventHandler_2__INavigationView__INavigationViewDisplayModeChangedEventArgs = ^TypedEventHandler_2__INavigationView__INavigationViewDisplayModeChangedEventArgs;

  // External: Windows.UI.Xaml.Controls.INavigationViewDisplayModeChangedEventArgs
  INavigationViewDisplayModeChangedEventArgs = interface;
  PINavigationViewDisplayModeChangedEventArgs = ^INavigationViewDisplayModeChangedEventArgs;

  // External: Windows.UI.Xaml.Documents.IContentLinkProviderCollection
  Documents_IContentLinkProviderCollection = interface;
  PDocuments_IContentLinkProviderCollection = ^Documents_IContentLinkProviderCollection;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IRichEditBox,Windows.UI.Xaml.Documents.IContentLinkInvokedEventArgs>
  TypedEventHandler_2__IRichEditBox__Documents_IContentLinkInvokedEventArgs = interface;
  PTypedEventHandler_2__IRichEditBox__Documents_IContentLinkInvokedEventArgs = ^TypedEventHandler_2__IRichEditBox__Documents_IContentLinkInvokedEventArgs;

  // External: Windows.UI.Xaml.Controls.IRichEditBox
  IRichEditBox = interface;
  PIRichEditBox = ^IRichEditBox;

  // External: Windows.UI.Text.ITextDocument
  ITextDocument = interface;
  PITextDocument = ^ITextDocument;

  // External: Windows.UI.Text.ITextSelection
  ITextSelection = interface;
  PITextSelection = ^ITextSelection;

  // External: Windows.UI.Text.ITextCharacterFormat
  ITextCharacterFormat = interface;
  PITextCharacterFormat = ^ITextCharacterFormat;

  // External: Windows.UI.Text.ITextParagraphFormat
  ITextParagraphFormat = interface;
  PITextParagraphFormat = ^ITextParagraphFormat;

  // External: Windows.UI.Text.ITextRange
  ITextRange = interface;
  PITextRange = ^ITextRange;

  // External: Windows.UI.Xaml.Controls.ContextMenuOpeningEventHandler
  ContextMenuOpeningEventHandler = interface;
  PContextMenuOpeningEventHandler = ^ContextMenuOpeningEventHandler;

  // External: Windows.UI.Xaml.Controls.IContextMenuEventArgs
  IContextMenuEventArgs = interface;
  PIContextMenuEventArgs = ^IContextMenuEventArgs;

  // External: Windows.UI.Xaml.Documents.IContentLinkInvokedEventArgs
  Documents_IContentLinkInvokedEventArgs = interface;
  PDocuments_IContentLinkInvokedEventArgs = ^Documents_IContentLinkInvokedEventArgs;

  // External: Windows.UI.Text.IContentLinkInfo
  IContentLinkInfo = interface;
  PIContentLinkInfo = ^IContentLinkInfo;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.ITextHighlighter>
  IVector_1__Documents_ITextHighlighter = interface;
  PIVector_1__Documents_ITextHighlighter = ^IVector_1__Documents_ITextHighlighter;

  // External: Windows.UI.Xaml.Documents.ITextHighlighter
  Documents_ITextHighlighter = interface;
  PDocuments_ITextHighlighter = ^Documents_ITextHighlighter;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.TextRange>
  IVector_1__Documents_TextRange = interface;
  PIVector_1__Documents_TextRange = ^IVector_1__Documents_TextRange;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.TextRange>
  IVectorView_1__Documents_TextRange = interface;
  PIVectorView_1__Documents_TextRange = ^IVectorView_1__Documents_TextRange;

  // External: Windows.UI.Xaml.Media.IBrush
  IBrush = interface;
  PIBrush = ^IBrush;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.ITextHighlighter>
  IVectorView_1__Documents_ITextHighlighter = interface;
  PIVectorView_1__Documents_ITextHighlighter = ^IVectorView_1__Documents_ITextHighlighter;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISplitView,Object>
  TypedEventHandler_2__ISplitView__IInspectable = interface;
  PTypedEventHandler_2__ISplitView__IInspectable = ^TypedEventHandler_2__ISplitView__IInspectable;

  // External: Windows.UI.Xaml.Controls.ISplitView
  ISplitView = interface;
  PISplitView = ^ISplitView;

  // External: Windows.UI.Xaml.Controls.Primitives.ISplitViewTemplateSettings
  Primitives_ISplitViewTemplateSettings = interface;
  PPrimitives_ISplitViewTemplateSettings = ^Primitives_ISplitViewTemplateSettings;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISplitView,Windows.UI.Xaml.Controls.ISplitViewPaneClosingEventArgs>
  TypedEventHandler_2__ISplitView__ISplitViewPaneClosingEventArgs = interface;
  PTypedEventHandler_2__ISplitView__ISplitViewPaneClosingEventArgs = ^TypedEventHandler_2__ISplitView__ISplitViewPaneClosingEventArgs;

  // External: Windows.UI.Xaml.Controls.ISplitViewPaneClosingEventArgs
  ISplitViewPaneClosingEventArgs = interface;
  PISplitViewPaneClosingEventArgs = ^ISplitViewPaneClosingEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISwapChainPanel,Object>
  TypedEventHandler_2__ISwapChainPanel__IInspectable = interface;
  PTypedEventHandler_2__ISwapChainPanel__IInspectable = ^TypedEventHandler_2__ISwapChainPanel__IInspectable;

  // External: Windows.UI.Xaml.Controls.ISwapChainPanel
  ISwapChainPanel = interface;
  PISwapChainPanel = ^ISwapChainPanel;

  // External: Windows.UI.Core.ICoreInputSourceBase
  ICoreInputSourceBase = interface;
  PICoreInputSourceBase = ^ICoreInputSourceBase;

  // External: Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IInputEnabledEventArgs>
  TypedEventHandler_2__IInspectable__IInputEnabledEventArgs = interface;
  PTypedEventHandler_2__IInspectable__IInputEnabledEventArgs = ^TypedEventHandler_2__IInspectable__IInputEnabledEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ITwoPaneView,Object>
  TypedEventHandler_2__ITwoPaneView__IInspectable = interface;
  PTypedEventHandler_2__ITwoPaneView__IInspectable = ^TypedEventHandler_2__ITwoPaneView__IInspectable;

  // External: Windows.UI.Xaml.Controls.ITwoPaneView
  ITwoPaneView = interface;
  PITwoPaneView = ^ITwoPaneView;

  // External: Windows.UI.Xaml.Navigation.LoadCompletedEventHandler
  Navigation_LoadCompletedEventHandler = interface;
  PNavigation_LoadCompletedEventHandler = ^Navigation_LoadCompletedEventHandler;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IWebView,Object>
  TypedEventHandler_2__IWebView__IInspectable = interface;
  PTypedEventHandler_2__IWebView__IInspectable = ^TypedEventHandler_2__IWebView__IInspectable;

  // External: Windows.UI.Xaml.Controls.IWebView
  IWebView = interface;
  PIWebView = ^IWebView;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Foundation.IUriRuntimeClass>
  IVector_1__IUriRuntimeClass = interface;
  PIVector_1__IUriRuntimeClass = ^IVector_1__IUriRuntimeClass;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.IUriRuntimeClass>
  IVectorView_1__IUriRuntimeClass = interface;
  PIVectorView_1__IUriRuntimeClass = ^IVectorView_1__IUriRuntimeClass;

  // External: Windows.UI.Xaml.Controls.NotifyEventHandler
  NotifyEventHandler = interface;
  PNotifyEventHandler = ^NotifyEventHandler;

  // External: Windows.UI.Xaml.Controls.INotifyEventArgs
  INotifyEventArgs = interface;
  PINotifyEventArgs = ^INotifyEventArgs;

  // External: Windows.UI.Xaml.Controls.WebViewNavigationFailedEventHandler
  WebViewNavigationFailedEventHandler = interface;
  PWebViewNavigationFailedEventHandler = ^WebViewNavigationFailedEventHandler;

  // External: Windows.UI.Xaml.Controls.IWebViewNavigationFailedEventArgs
  IWebViewNavigationFailedEventArgs = interface;
  PIWebViewNavigationFailedEventArgs = ^IWebViewNavigationFailedEventArgs;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IDependencyObject>
  IVector_1__IDependencyObject = interface;
  PIVector_1__IDependencyObject = ^IVector_1__IDependencyObject;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.IDependencyObject>
  IVectorView_1__IDependencyObject = interface;
  PIVectorView_1__IDependencyObject = ^IVectorView_1__IDependencyObject;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.Maps.IMapControl,Object>
  TypedEventHandler_2__Maps_IMapControl__IInspectable = interface;
  PTypedEventHandler_2__Maps_IMapControl__IInspectable = ^TypedEventHandler_2__Maps_IMapControl__IInspectable;

  // External: Windows.UI.Xaml.Controls.Maps.IMapControl
  Maps_IMapControl = interface;
  PMaps_IMapControl = ^Maps_IMapControl;

  // External: Windows.Devices.Geolocation.IGeopoint
  IGeopoint = interface;
  PIGeopoint = ^IGeopoint;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapElement>
  IVector_1__Maps_IMapElement = interface;
  PIVector_1__Maps_IMapElement = ^IVector_1__Maps_IMapElement;

  // External: Windows.UI.Xaml.Controls.Maps.IMapElement
  Maps_IMapElement = interface;
  PMaps_IMapElement = ^Maps_IMapElement;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Maps.IMapElement>
  IVectorView_1__Maps_IMapElement = interface;
  PIVectorView_1__Maps_IMapElement = ^IVectorView_1__Maps_IMapElement;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapRouteView>
  IVector_1__Maps_IMapRouteView = interface;
  PIVector_1__Maps_IMapRouteView = ^IVector_1__Maps_IMapRouteView;

  // External: Windows.UI.Xaml.Controls.Maps.IMapRouteView
  Maps_IMapRouteView = interface;
  PMaps_IMapRouteView = ^Maps_IMapRouteView;

  // External: Windows.Services.Maps.IMapRoute
  Maps_IMapRoute = interface;
  PMaps_IMapRoute = ^Maps_IMapRoute;

  // External: Windows.Devices.Geolocation.IGeoboundingBox
  IGeoboundingBox = interface;
  PIGeoboundingBox = ^IGeoboundingBox;

  // External: Windows.Devices.Geolocation.IGeopath
  IGeopath = interface;
  PIGeopath = ^IGeopath;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.BasicGeoposition>
  IVectorView_1__BasicGeoposition = interface;
  PIVectorView_1__BasicGeoposition = ^IVectorView_1__BasicGeoposition;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.IMapRouteLeg>
  IVectorView_1__Maps_IMapRouteLeg = interface;
  PIVectorView_1__Maps_IMapRouteLeg = ^IVectorView_1__Maps_IMapRouteLeg;

  // External: Windows.Services.Maps.IMapRouteLeg
  Maps_IMapRouteLeg = interface;
  PMaps_IMapRouteLeg = ^Maps_IMapRouteLeg;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.IMapRouteManeuver>
  IVectorView_1__Maps_IMapRouteManeuver = interface;
  PIVectorView_1__Maps_IMapRouteManeuver = ^IVectorView_1__Maps_IMapRouteManeuver;

  // External: Windows.Services.Maps.IMapRouteManeuver
  Maps_IMapRouteManeuver = interface;
  PMaps_IMapRouteManeuver = ^Maps_IMapRouteManeuver;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Maps.IMapRouteView>
  IVectorView_1__Maps_IMapRouteView = interface;
  PIVectorView_1__Maps_IMapRouteView = ^IVectorView_1__Maps_IMapRouteView;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapTileSource>
  IVector_1__Maps_IMapTileSource = interface;
  PIVector_1__Maps_IMapTileSource = ^IVector_1__Maps_IMapTileSource;

  // External: Windows.UI.Xaml.Controls.Maps.IMapTileSource
  Maps_IMapTileSource = interface;
  PMaps_IMapTileSource = ^Maps_IMapTileSource;

  // External: Windows.UI.Xaml.Controls.Maps.IMapTileDataSource
  Maps_IMapTileDataSource = interface;
  PMaps_IMapTileDataSource = ^Maps_IMapTileDataSource;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Maps.IMapTileSource>
  IVectorView_1__Maps_IMapTileSource = interface;
  PIVectorView_1__Maps_IMapTileSource = ^IVectorView_1__Maps_IMapTileSource;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.Maps.IMapControl,Windows.UI.Xaml.Controls.Maps.IMapInputEventArgs>
  TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs = interface;
  PTypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs = ^TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs;

  // External: Windows.UI.Xaml.Controls.Maps.IMapInputEventArgs
  Maps_IMapInputEventArgs = interface;
  PMaps_IMapInputEventArgs = ^Maps_IMapInputEventArgs;

  // External: Windows.Foundation.IReference`1<Windows.UI.Xaml.Thickness>
  IReference_1__Thickness = interface;
  PIReference_1__Thickness = ^IReference_1__Thickness;

  // External: Windows.UI.Xaml.Input.IProcessKeyboardAcceleratorEventArgs
  Input_IProcessKeyboardAcceleratorEventArgs = interface;
  PInput_IProcessKeyboardAcceleratorEventArgs = ^Input_IProcessKeyboardAcceleratorEventArgs;

  // External: Windows.UI.Xaml.IXamlRoot
  IXamlRoot = interface;
  PIXamlRoot = ^IXamlRoot;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.IXamlRoot,Windows.UI.Xaml.IXamlRootChangedEventArgs>
  TypedEventHandler_2__IXamlRoot__IXamlRootChangedEventArgs = interface;
  PTypedEventHandler_2__IXamlRoot__IXamlRootChangedEventArgs = ^TypedEventHandler_2__IXamlRoot__IXamlRootChangedEventArgs;

  // External: Windows.UI.Xaml.IXamlRootChangedEventArgs
  IXamlRootChangedEventArgs = interface;
  PIXamlRootChangedEventArgs = ^IXamlRootChangedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Animation.IConnectedAnimation,Object>
  TypedEventHandler_2__Animation_IConnectedAnimation__IInspectable = interface;
  PTypedEventHandler_2__Animation_IConnectedAnimation__IInspectable = ^TypedEventHandler_2__Animation_IConnectedAnimation__IInspectable;

  // External: Windows.UI.Xaml.Media.Animation.IConnectedAnimation
  Animation_IConnectedAnimation = interface;
  PAnimation_IConnectedAnimation = ^Animation_IConnectedAnimation;

  // External: Windows.UI.Xaml.Controls.IMediaElement
  IMediaElement = interface;
  PIMediaElement = ^IMediaElement;

  // External: Windows.UI.Xaml.Media.IImageSource
  IImageSource = interface;
  PIImageSource = ^IImageSource;

  // External: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IVector_1__ITimelineMarker = interface;
  PIVector_1__ITimelineMarker = ^IVector_1__ITimelineMarker;

  // External: Windows.UI.Xaml.Media.ITimelineMarker
  ITimelineMarker = interface;
  PITimelineMarker = ^ITimelineMarker;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IVectorView_1__ITimelineMarker = interface;
  PIVectorView_1__ITimelineMarker = ^IVectorView_1__ITimelineMarker;

  // External: Windows.Media.PlayTo.IPlayToSource
  PlayTo_IPlayToSource = interface;
  PPlayTo_IPlayToSource = ^PlayTo_IPlayToSource;

  // External: Windows.Media.PlayTo.IPlayToConnection
  PlayTo_IPlayToConnection = interface;
  PPlayTo_IPlayToConnection = ^PlayTo_IPlayToConnection;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionStateChangedEventArgs>
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionStateChangedEventArgs = interface;
  PTypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionStateChangedEventArgs = ^TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionStateChangedEventArgs;

  // External: Windows.Media.PlayTo.IPlayToConnectionStateChangedEventArgs
  PlayTo_IPlayToConnectionStateChangedEventArgs = interface;
  PPlayTo_IPlayToConnectionStateChangedEventArgs = ^PlayTo_IPlayToConnectionStateChangedEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionTransferredEventArgs>
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionTransferredEventArgs = interface;
  PTypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionTransferredEventArgs = ^TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionTransferredEventArgs;

  // External: Windows.Media.PlayTo.IPlayToConnectionTransferredEventArgs
  PlayTo_IPlayToConnectionTransferredEventArgs = interface;
  PPlayTo_IPlayToConnectionTransferredEventArgs = ^PlayTo_IPlayToConnectionTransferredEventArgs;

  // External: Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionErrorEventArgs>
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionErrorEventArgs = interface;
  PTypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionErrorEventArgs = ^TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionErrorEventArgs;

  // External: Windows.Media.PlayTo.IPlayToConnectionErrorEventArgs
  PlayTo_IPlayToConnectionErrorEventArgs = interface;
  PPlayTo_IPlayToConnectionErrorEventArgs = ^PlayTo_IPlayToConnectionErrorEventArgs;

  // External: Windows.Media.Protection.IMediaProtectionManager
  Protection_IMediaProtectionManager = interface;
  PProtection_IMediaProtectionManager = ^Protection_IMediaProtectionManager;

  // External: Windows.Media.Protection.ServiceRequestedEventHandler
  Protection_ServiceRequestedEventHandler = interface;
  PProtection_ServiceRequestedEventHandler = ^Protection_ServiceRequestedEventHandler;

  // External: Windows.Media.Protection.IServiceRequestedEventArgs
  Protection_IServiceRequestedEventArgs = interface;
  PProtection_IServiceRequestedEventArgs = ^Protection_IServiceRequestedEventArgs;

  // External: Windows.Media.Protection.IMediaProtectionServiceRequest
  Protection_IMediaProtectionServiceRequest = interface;
  PProtection_IMediaProtectionServiceRequest = ^Protection_IMediaProtectionServiceRequest;

  // External: Windows.Media.Protection.IMediaProtectionServiceCompletion
  Protection_IMediaProtectionServiceCompletion = interface;
  PProtection_IMediaProtectionServiceCompletion = ^Protection_IMediaProtectionServiceCompletion;

  // External: Windows.Media.Protection.RebootNeededEventHandler
  Protection_RebootNeededEventHandler = interface;
  PProtection_RebootNeededEventHandler = ^Protection_RebootNeededEventHandler;

  // External: Windows.Media.Protection.ComponentLoadFailedEventHandler
  Protection_ComponentLoadFailedEventHandler = interface;
  PProtection_ComponentLoadFailedEventHandler = ^Protection_ComponentLoadFailedEventHandler;

  // External: Windows.Media.Protection.IComponentLoadFailedEventArgs
  Protection_IComponentLoadFailedEventArgs = interface;
  PProtection_IComponentLoadFailedEventArgs = ^Protection_IComponentLoadFailedEventArgs;

  // External: Windows.Media.Protection.IRevocationAndRenewalInformation
  Protection_IRevocationAndRenewalInformation = interface;
  PProtection_IRevocationAndRenewalInformation = ^Protection_IRevocationAndRenewalInformation;

  // External: Windows.Foundation.Collections.IVector`1<Windows.Media.Protection.IRevocationAndRenewalItem>
  IVector_1__Protection_IRevocationAndRenewalItem = interface;
  PIVector_1__Protection_IRevocationAndRenewalItem = ^IVector_1__Protection_IRevocationAndRenewalItem;

  // External: Windows.Media.Protection.IRevocationAndRenewalItem
  Protection_IRevocationAndRenewalItem = interface;
  PProtection_IRevocationAndRenewalItem = ^Protection_IRevocationAndRenewalItem;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.Media.Protection.IRevocationAndRenewalItem>
  IVectorView_1__Protection_IRevocationAndRenewalItem = interface;
  PIVectorView_1__Protection_IRevocationAndRenewalItem = ^IVectorView_1__Protection_IRevocationAndRenewalItem;

  // External: Windows.UI.Xaml.Media.TimelineMarkerRoutedEventHandler
  TimelineMarkerRoutedEventHandler = interface;
  PTimelineMarkerRoutedEventHandler = ^TimelineMarkerRoutedEventHandler;

  // External: Windows.UI.Xaml.Media.ITimelineMarkerRoutedEventArgs
  ITimelineMarkerRoutedEventArgs = interface;
  PITimelineMarkerRoutedEventArgs = ^ITimelineMarkerRoutedEventArgs;

  // External: Windows.UI.Xaml.Media.RateChangedRoutedEventHandler
  RateChangedRoutedEventHandler = interface;
  PRateChangedRoutedEventHandler = ^RateChangedRoutedEventHandler;

  // External: Windows.UI.Xaml.Media.IRateChangedRoutedEventArgs
  IRateChangedRoutedEventArgs = interface;
  PIRateChangedRoutedEventArgs = ^IRateChangedRoutedEventArgs;

  // External: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.IUIElement>
  IIterable_1__IUIElement = interface;
  PIIterable_1__IUIElement = ^IIterable_1__IUIElement;

  // External: Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.IUIElement>
  IIterator_1__IUIElement = interface;
  PIIterator_1__IUIElement = ^IIterator_1__IUIElement;

  // External: Windows.UI.Xaml.Controls.IListViewBase
  IListViewBase = interface;
  PIListViewBase = ^IListViewBase;

  // External: Windows.UI.Xaml.Controls.ItemClickEventHandler
  ItemClickEventHandler = interface;
  PItemClickEventHandler = ^ItemClickEventHandler;

  // External: Windows.UI.Xaml.Controls.IItemClickEventArgs
  IItemClickEventArgs = interface;
  PIItemClickEventArgs = ^IItemClickEventArgs;

  // External: Windows.UI.Xaml.Controls.DragItemsStartingEventHandler
  DragItemsStartingEventHandler = interface;
  PDragItemsStartingEventHandler = ^DragItemsStartingEventHandler;

  // External: Windows.UI.Xaml.Controls.IDragItemsStartingEventArgs
  IDragItemsStartingEventArgs = interface;
  PIDragItemsStartingEventArgs = ^IDragItemsStartingEventArgs;

  // External: Windows.UI.Xaml.IUIElementWeakCollection
  IUIElementWeakCollection = interface;
  PIUIElementWeakCollection = ^IUIElementWeakCollection;

  // External: Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Primitives.IPopup>
  IVectorView_1__Primitives_IPopup = interface;
  PIVectorView_1__Primitives_IPopup = ^IVectorView_1__Primitives_IPopup;

  // External: Windows.UI.Xaml.Controls.Primitives.IPopup
  Primitives_IPopup = interface;
  PPrimitives_IPopup = ^Primitives_IPopup;

  // External: Windows.UI.Xaml.IWindow
  IWindow = interface;
  PIWindow = ^IWindow;

  // External: Windows.UI.Xaml.WindowActivatedEventHandler
  WindowActivatedEventHandler = interface;
  PWindowActivatedEventHandler = ^WindowActivatedEventHandler;

  // External: Windows.UI.Xaml.WindowClosedEventHandler
  WindowClosedEventHandler = interface;
  PWindowClosedEventHandler = ^WindowClosedEventHandler;

  // External: Windows.UI.Xaml.WindowSizeChangedEventHandler
  WindowSizeChangedEventHandler = interface;
  PWindowSizeChangedEventHandler = ^WindowSizeChangedEventHandler;

  // External: Windows.UI.Xaml.WindowVisibilityChangedEventHandler
  WindowVisibilityChangedEventHandler = interface;
  PWindowVisibilityChangedEventHandler = ^WindowVisibilityChangedEventHandler;

  // External: Windows.UI.Xaml.Controls.IMediaTransportControls
  IMediaTransportControls = interface;
  PIMediaTransportControls = ^IMediaTransportControls;

  // External: Windows.Graphics.Imaging.ISoftwareBitmap
  Imaging_ISoftwareBitmap = interface;
  PImaging_ISoftwareBitmap = ^Imaging_ISoftwareBitmap;

  // External: Windows.Graphics.Imaging.IBitmapBuffer
  Imaging_IBitmapBuffer = interface;
  PImaging_IBitmapBuffer = ^Imaging_IBitmapBuffer;

  // External: Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.DataPackageOperation>
  IAsyncOperation_1__DataPackageOperation = interface;
  PIAsyncOperation_1__DataPackageOperation = ^IAsyncOperation_1__DataPackageOperation;

  // External: Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.DataPackageOperation>
  AsyncOperationCompletedHandler_1__DataPackageOperation = interface;
  PAsyncOperationCompletedHandler_1__DataPackageOperation = ^AsyncOperationCompletedHandler_1__DataPackageOperation;

  // External: Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Vector2>
  IReference_1__Numerics_Vector2 = interface;
  PIReference_1__Numerics_Vector2 = ^IReference_1__Numerics_Vector2;

  // External: Windows.Media.Playback.IMediaPlaybackSource
  Playback_IMediaPlaybackSource = interface;
  PPlayback_IMediaPlaybackSource = ^Playback_IMediaPlaybackSource;

  // External: Windows.Media.Core.IMediaSource
  Core_IMediaSource = interface;
  PCore_IMediaSource = ^Core_IMediaSource;

  // External: Windows.Media.Casting.ICastingSource
  Casting_ICastingSource = interface;
  PCasting_ICastingSource = ^Casting_ICastingSource;

  // Windows.Media.Playback.FailedMediaStreamKind
  Playback_FailedMediaStreamKind = (
    Unknown = 0,
    Audio = 1,
    Video = 2
  );
  PPlayback_FailedMediaStreamKind = ^Playback_FailedMediaStreamKind;

  // Windows.UI.Xaml.Interop.NotifyCollectionChangedAction
  Interop_NotifyCollectionChangedAction = (
    Add = 0,
    Remove = 1,
    Replace = 2,
    Move = 3,
    Reset = 4
  );
  PInterop_NotifyCollectionChangedAction = ^Interop_NotifyCollectionChangedAction;

  // Windows.ApplicationModel.DataTransfer.DragDrop.DragDropModifiers
  DragDrop_DragDropModifiers = (
    None = 0,
    Shift = 1,
    Control = 2,
    Alt = 4,
    LeftButton = 8,
    MiddleButton = 16,
    RightButton = 32
  );
  PDragDrop_DragDropModifiers = ^DragDrop_DragDropModifiers;

  // Windows.Graphics.Imaging.BitmapBufferAccessMode
  Imaging_BitmapBufferAccessMode = (
    Read = 0,
    ReadWrite = 1,
    Write = 2
  );
  PImaging_BitmapBufferAccessMode = ^Imaging_BitmapBufferAccessMode;

  // Windows.Graphics.Imaging.BitmapAlphaMode
  Imaging_BitmapAlphaMode = (
    Premultiplied = 0,
    Straight = 1,
    Ignore = 2
  );
  PImaging_BitmapAlphaMode = ^Imaging_BitmapAlphaMode;

  // Windows.Graphics.Imaging.BitmapPixelFormat
  Imaging_BitmapPixelFormat = (
    Unknown = 0,
    Rgba16 = 12,
    Rgba8 = 30,
    Gray16 = 57,
    Gray8 = 62,
    Bgra8 = 87,
    Nv12 = 103,
    P010 = 104,
    Yuy2 = 107
  );
  PImaging_BitmapPixelFormat = ^Imaging_BitmapPixelFormat;

  // Windows.UI.Xaml.ApplicationTheme
  ApplicationTheme = (
    Light = 0,
    Dark = 1
  );
  PApplicationTheme = ^ApplicationTheme;

  // Windows.UI.Xaml.Controls.Primitives.EdgeTransitionLocation
  Primitives_EdgeTransitionLocation = (
    Left = 0,
    Top = 1,
    Right = 2,
    Bottom = 3
  );
  PPrimitives_EdgeTransitionLocation = ^Primitives_EdgeTransitionLocation;

  // Windows.UI.Xaml.Controls.Primitives.AnimationDirection
  Primitives_AnimationDirection = (
    Left = 0,
    Top = 1,
    Right = 2,
    Bottom = 3
  );
  PPrimitives_AnimationDirection = ^Primitives_AnimationDirection;

  // Windows.UI.Xaml.Controls.ScrollIntoViewAlignment
  ScrollIntoViewAlignment = (
    Default = 0,
    Leading = 1
  );
  PScrollIntoViewAlignment = ^ScrollIntoViewAlignment;

  // Windows.UI.Xaml.Controls.IncrementalLoadingTrigger
  IncrementalLoadingTrigger = (
    None = 0,
    Edge = 1
  );
  PIncrementalLoadingTrigger = ^IncrementalLoadingTrigger;

  // Windows.UI.Xaml.Controls.ListViewSelectionMode
  ListViewSelectionMode = (
    None = 0,
    Single = 1,
    Multiple = 2,
    Extended = 3
  );
  PListViewSelectionMode = ^ListViewSelectionMode;

  // Windows.UI.Xaml.Media.MediaCanPlayResponse
  MediaCanPlayResponse = (
    NotSupported = 0,
    Maybe = 1,
    Probably = 2
  );
  PMediaCanPlayResponse = ^MediaCanPlayResponse;

  // Windows.UI.Xaml.Media.Stereo3DVideoRenderMode
  Stereo3DVideoRenderMode = (
    Mono = 0,
    Stereo = 1
  );
  PStereo3DVideoRenderMode = ^Stereo3DVideoRenderMode;

  // Windows.UI.Xaml.Media.Stereo3DVideoPackingMode
  Stereo3DVideoPackingMode = (
    None = 0,
    SideBySide = 1,
    TopBottom = 2
  );
  PStereo3DVideoPackingMode = ^Stereo3DVideoPackingMode;

  // Windows.Media.Protection.RevocationAndRenewalReasons
  Protection_RevocationAndRenewalReasons = (
    UserModeComponentLoad = 1,
    KernelModeComponentLoad = 2,
    AppComponent = 4,
    GlobalRevocationListLoadFailed = 16,
    InvalidGlobalRevocationListSignature = 32,
    GlobalRevocationListAbsent = 4096,
    ComponentRevoked = 8192,
    InvalidComponentCertificateExtendedKeyUse = 16384,
    ComponentCertificateRevoked = 32768,
    InvalidComponentCertificateRoot = 65536,
    ComponentHighSecurityCertificateRevoked = 131072,
    ComponentLowSecurityCertificateRevoked = 262144,
    BootDriverVerificationFailed = 1048576,
    ComponentSignedWithTestCertificate = 16777216,
    EncryptionFailure = 268435456
  );
  PProtection_RevocationAndRenewalReasons = ^Protection_RevocationAndRenewalReasons;

  // Windows.UI.Xaml.Media.AudioDeviceType
  AudioDeviceType = (
    Console = 0,
    Multimedia = 1,
    Communications = 2
  );
  PAudioDeviceType = ^AudioDeviceType;

  // Windows.UI.Xaml.Media.AudioCategory
  AudioCategory = (
    Other = 0,
    ForegroundOnlyMedia = 1,
    BackgroundCapableMedia = 2,
    Communications = 3,
    Alerts = 4,
    SoundEffects = 5,
    GameEffects = 6,
    GameMedia = 7,
    GameChat = 8,
    Speech = 9,
    Movie = 10,
    Media = 11
  );
  PAudioCategory = ^AudioCategory;

  // Windows.Media.PlayTo.PlayToConnectionError
  PlayTo_PlayToConnectionError = (
    None = 0,
    DeviceNotResponding = 1,
    DeviceError = 2,
    DeviceLocked = 3,
    ProtectedPlaybackFailed = 4
  );
  PPlayTo_PlayToConnectionError = ^PlayTo_PlayToConnectionError;

  // Windows.Media.PlayTo.PlayToConnectionState
  PlayTo_PlayToConnectionState = (
    Disconnected = 0,
    Connected = 1,
    Rendering = 2
  );
  PPlayTo_PlayToConnectionState = ^PlayTo_PlayToConnectionState;

  // Windows.UI.Xaml.Media.MediaElementState
  MediaElementState = (
    Closed = 0,
    Opening = 1,
    Buffering = 2,
    Playing = 3,
    Paused = 4,
    Stopped = 5
  );
  PMediaElementState = ^MediaElementState;

  // Windows.UI.Xaml.Controls.Maps.MapAnimationKind
  Maps_MapAnimationKind = (
    Default = 0,
    None = 1,
    Linear = 2,
    Bow = 3
  );
  PMaps_MapAnimationKind = ^Maps_MapAnimationKind;

  // Windows.UI.Xaml.Controls.Maps.MapTileLayer
  Maps_MapTileLayer = (
    LabelOverlay = 0,
    RoadOverlay = 1,
    AreaOverlay = 2,
    BackgroundOverlay = 3,
    BackgroundReplacement = 4
  );
  PMaps_MapTileLayer = ^Maps_MapTileLayer;

  // Windows.Services.Maps.MapManeuverNotices
  Maps_MapManeuverNotices = (
    None = 0,
    Toll = 1,
    Unpaved = 2
  );
  PMaps_MapManeuverNotices = ^Maps_MapManeuverNotices;

  // Windows.Services.Maps.MapRouteManeuverKind
  Maps_MapRouteManeuverKind = (
    None = 0,
    Start = 1,
    Stopover = 2,
    StopoverResume = 3,
    &End = 4,
    GoStraight = 5,
    UTurnLeft = 6,
    UTurnRight = 7,
    TurnKeepLeft = 8,
    TurnKeepRight = 9,
    TurnLightLeft = 10,
    TurnLightRight = 11,
    TurnLeft = 12,
    TurnRight = 13,
    TurnHardLeft = 14,
    TurnHardRight = 15,
    FreewayEnterLeft = 16,
    FreewayEnterRight = 17,
    FreewayLeaveLeft = 18,
    FreewayLeaveRight = 19,
    FreewayContinueLeft = 20,
    FreewayContinueRight = 21,
    TrafficCircleLeft = 22,
    TrafficCircleRight = 23,
    TakeFerry = 24
  );
  PMaps_MapRouteManeuverKind = ^Maps_MapRouteManeuverKind;

  // Windows.UI.Xaml.Controls.Maps.MapWatermarkMode
  Maps_MapWatermarkMode = (
    Automatic = 0,
    &On = 1
  );
  PMaps_MapWatermarkMode = ^Maps_MapWatermarkMode;

  // Windows.UI.Xaml.Controls.Maps.MapStyle
  Maps_MapStyle = (
    None = 0,
    Road = 1,
    Aerial = 2,
    AerialWithRoads = 3,
    Terrain = 4,
    Aerial3D = 5,
    Aerial3DWithRoads = 6,
    Custom = 7
  );
  PMaps_MapStyle = ^Maps_MapStyle;

  // Windows.UI.Xaml.Controls.Maps.MapLoadingStatus
  Maps_MapLoadingStatus = (
    Loading = 0,
    Loaded = 1,
    DataUnavailable = 2,
    DownloadedMapsManagerUnavailable = 3
  );
  PMaps_MapLoadingStatus = ^Maps_MapLoadingStatus;

  // Windows.UI.Xaml.Controls.Maps.MapColorScheme
  Maps_MapColorScheme = (
    Light = 0,
    Dark = 1
  );
  PMaps_MapColorScheme = ^Maps_MapColorScheme;

  // Windows.Web.WebErrorStatus
  WebErrorStatus = (
    Unknown = 0,
    CertificateCommonNameIsIncorrect = 1,
    CertificateExpired = 2,
    CertificateContainsErrors = 3,
    CertificateRevoked = 4,
    CertificateIsInvalid = 5,
    ServerUnreachable = 6,
    Timeout = 7,
    ErrorHttpInvalidServerResponse = 8,
    ConnectionAborted = 9,
    ConnectionReset = 10,
    Disconnected = 11,
    HttpToHttpsOnRedirection = 12,
    HttpsToHttpOnRedirection = 13,
    CannotConnect = 14,
    HostNameNotResolved = 15,
    OperationCanceled = 16,
    RedirectFailed = 17,
    UnexpectedStatusCode = 18,
    UnexpectedRedirection = 19,
    UnexpectedClientError = 20,
    UnexpectedServerError = 21,
    InsufficientRangeSupport = 22,
    MissingContentLengthSupport = 23,
    MultipleChoices = 300,
    MovedPermanently = 301,
    Found = 302,
    SeeOther = 303,
    NotModified = 304,
    UseProxy = 305,
    TemporaryRedirect = 307,
    BadRequest = 400,
    Unauthorized = 401,
    PaymentRequired = 402,
    Forbidden = 403,
    NotFound = 404,
    MethodNotAllowed = 405,
    NotAcceptable = 406,
    ProxyAuthenticationRequired = 407,
    RequestTimeout = 408,
    Conflict = 409,
    Gone = 410,
    LengthRequired = 411,
    PreconditionFailed = 412,
    RequestEntityTooLarge = 413,
    RequestUriTooLong = 414,
    UnsupportedMediaType = 415,
    RequestedRangeNotSatisfiable = 416,
    ExpectationFailed = 417,
    InternalServerError = 500,
    NotImplemented = 501,
    BadGateway = 502,
    ServiceUnavailable = 503,
    GatewayTimeout = 504,
    HttpVersionNotSupported = 505
  );
  PWebErrorStatus = ^WebErrorStatus;

  // Windows.UI.Xaml.Controls.TwoPaneViewTallModeConfiguration
  TwoPaneViewTallModeConfiguration = (
    SinglePane = 0,
    TopBottom = 1,
    BottomTop = 2
  );
  PTwoPaneViewTallModeConfiguration = ^TwoPaneViewTallModeConfiguration;

  // Windows.UI.Xaml.Controls.TwoPaneViewWideModeConfiguration
  TwoPaneViewWideModeConfiguration = (
    SinglePane = 0,
    LeftRight = 1,
    RightLeft = 2
  );
  PTwoPaneViewWideModeConfiguration = ^TwoPaneViewWideModeConfiguration;

  // Windows.UI.Xaml.Controls.TwoPaneViewMode
  TwoPaneViewMode = (
    SinglePane = 0,
    Wide = 1,
    Tall = 2
  );
  PTwoPaneViewMode = ^TwoPaneViewMode;

  // Windows.UI.Xaml.Controls.TwoPaneViewPriority
  TwoPaneViewPriority = (
    Pane1 = 0,
    Pane2 = 1
  );
  PTwoPaneViewPriority = ^TwoPaneViewPriority;

  // Windows.UI.Xaml.Controls.SplitViewDisplayMode
  SplitViewDisplayMode = (
    Overlay = 0,
    &Inline = 1,
    CompactOverlay = 2,
    CompactInline = 3
  );
  PSplitViewDisplayMode = ^SplitViewDisplayMode;

  // Windows.UI.Xaml.Controls.SplitViewPanePlacement
  SplitViewPanePlacement = (
    Left = 0,
    Right = 1
  );
  PSplitViewPanePlacement = ^SplitViewPanePlacement;

  // Windows.UI.Text.TextSetOptions
  TextSetOptions = (
    None = 0,
    UnicodeBidi = 1,
    Unlink = 8,
    Unhide = 16,
    CheckTextLimit = 32,
    FormatRtf = 8192,
    ApplyRtfDocumentDefaults = 16384
  );
  PTextSetOptions = ^TextSetOptions;

  // Windows.UI.Text.TextGetOptions
  TextGetOptions = (
    None = 0,
    AdjustCrlf = 1,
    UseCrlf = 2,
    UseObjectText = 4,
    AllowFinalEop = 8,
    NoHidden = 32,
    IncludeNumbering = 64,
    FormatRtf = 8192,
    UseLf = 16777216
  );
  PTextGetOptions = ^TextGetOptions;

  // Windows.UI.Text.PointOptions
  PointOptions = (
    None = 0,
    IncludeInset = 1,
    Start = 32,
    ClientCoordinates = 256,
    AllowOffClient = 512,
    Transform = 1024,
    NoHorizontalScroll = 65536,
    NoVerticalScroll = 262144
  );
  PPointOptions = ^PointOptions;

  // Windows.UI.Text.VerticalCharacterAlignment
  VerticalCharacterAlignment = (
    Top = 0,
    Baseline = 1,
    Bottom = 2
  );
  PVerticalCharacterAlignment = ^VerticalCharacterAlignment;

  // Windows.UI.Text.HorizontalCharacterAlignment
  HorizontalCharacterAlignment = (
    Left = 0,
    Right = 1,
    Center = 2
  );
  PHorizontalCharacterAlignment = ^HorizontalCharacterAlignment;

  // Windows.UI.Text.FindOptions
  FindOptions = (
    None = 0,
    Word = 2,
    &Case = 4
  );
  PFindOptions = ^FindOptions;

  // Windows.UI.Text.LetterCase
  LetterCase = (
    Lower = 0,
    Upper = 1
  );
  PLetterCase = ^LetterCase;

  // Windows.UI.Text.RangeGravity
  RangeGravity = (
    UIBehavior = 0,
    Backward = 1,
    Forward = 2,
    Inward = 3,
    Outward = 4
  );
  PRangeGravity = ^RangeGravity;

  // Windows.UI.Text.TabLeader
  TabLeader = (
    Spaces = 0,
    Dots = 1,
    Dashes = 2,
    Lines = 3,
    ThickLines = 4,
    Equals = 5
  );
  PTabLeader = ^TabLeader;

  // Windows.UI.Text.TabAlignment
  TabAlignment = (
    Left = 0,
    Center = 1,
    Right = 2,
    Decimal = 3,
    Bar = 4
  );
  PTabAlignment = ^TabAlignment;

  // Windows.UI.Text.ParagraphStyle
  ParagraphStyle = (
    Undefined = 0,
    None = 1,
    Normal = 2,
    Heading1 = 3,
    Heading2 = 4,
    Heading3 = 5,
    Heading4 = 6,
    Heading5 = 7,
    Heading6 = 8,
    Heading7 = 9,
    Heading8 = 10,
    Heading9 = 11
  );
  PParagraphStyle = ^ParagraphStyle;

  // Windows.UI.Text.MarkerType
  MarkerType = (
    Undefined = 0,
    None = 1,
    Bullet = 2,
    Arabic = 3,
    LowercaseEnglishLetter = 4,
    UppercaseEnglishLetter = 5,
    LowercaseRoman = 6,
    UppercaseRoman = 7,
    UnicodeSequence = 8,
    CircledNumber = 9,
    BlackCircleWingding = 10,
    WhiteCircleWingding = 11,
    ArabicWide = 12,
    SimplifiedChinese = 13,
    TraditionalChinese = 14,
    JapanSimplifiedChinese = 15,
    JapanKorea = 16,
    ArabicDictionary = 17,
    ArabicAbjad = 18,
    Hebrew = 19,
    ThaiAlphabetic = 20,
    ThaiNumeric = 21,
    DevanagariVowel = 22,
    DevanagariConsonant = 23,
    DevanagariNumeric = 24
  );
  PMarkerType = ^MarkerType;

  // Windows.UI.Text.MarkerStyle
  MarkerStyle = (
    Undefined = 0,
    Parenthesis = 1,
    Parentheses = 2,
    Period = 3,
    Plain = 4,
    Minus = 5,
    NoNumber = 6
  );
  PMarkerStyle = ^MarkerStyle;

  // Windows.UI.Text.MarkerAlignment
  MarkerAlignment = (
    Undefined = 0,
    Left = 1,
    Center = 2,
    Right = 3
  );
  PMarkerAlignment = ^MarkerAlignment;

  // Windows.UI.Text.LineSpacingRule
  LineSpacingRule = (
    Undefined = 0,
    Single = 1,
    OneAndHalf = 2,
    Double = 3,
    AtLeast = 4,
    Exactly = 5,
    Multiple = 6,
    Percent = 7
  );
  PLineSpacingRule = ^LineSpacingRule;

  // Windows.UI.Text.ParagraphAlignment
  ParagraphAlignment = (
    Undefined = 0,
    Left = 1,
    Center = 2,
    Right = 3,
    Justify = 4
  );
  PParagraphAlignment = ^ParagraphAlignment;

  // Windows.UI.Text.UnderlineType
  UnderlineType = (
    Undefined = 0,
    None = 1,
    Single = 2,
    Words = 3,
    Double = 4,
    Dotted = 5,
    Dash = 6,
    DashDot = 7,
    DashDotDot = 8,
    Wave = 9,
    Thick = 10,
    Thin = 11,
    DoubleWave = 12,
    HeavyWave = 13,
    LongDash = 14,
    ThickDash = 15,
    ThickDashDot = 16,
    ThickDashDotDot = 17,
    ThickDotted = 18,
    ThickLongDash = 19
  );
  PUnderlineType = ^UnderlineType;

  // Windows.UI.Text.TextScript
  TextScript = (
    Undefined = 0,
    Ansi = 1,
    EastEurope = 2,
    Cyrillic = 3,
    Greek = 4,
    Turkish = 5,
    Hebrew = 6,
    Arabic = 7,
    Baltic = 8,
    Vietnamese = 9,
    Default = 10,
    Symbol = 11,
    Thai = 12,
    ShiftJis = 13,
    GB2312 = 14,
    Hangul = 15,
    Big5 = 16,
    PC437 = 17,
    Oem = 18,
    Mac = 19,
    Armenian = 20,
    Syriac = 21,
    Thaana = 22,
    Devanagari = 23,
    Bengali = 24,
    Gurmukhi = 25,
    Gujarati = 26,
    Oriya = 27,
    Tamil = 28,
    Telugu = 29,
    Kannada = 30,
    Malayalam = 31,
    Sinhala = 32,
    Lao = 33,
    Tibetan = 34,
    Myanmar = 35,
    Georgian = 36,
    Jamo = 37,
    Ethiopic = 38,
    Cherokee = 39,
    Aboriginal = 40,
    Ogham = 41,
    Runic = 42,
    Khmer = 43,
    Mongolian = 44,
    Braille = 45,
    Yi = 46,
    Limbu = 47,
    TaiLe = 48,
    NewTaiLue = 49,
    SylotiNagri = 50,
    Kharoshthi = 51,
    Kayahli = 52,
    UnicodeSymbol = 53,
    Emoji = 54,
    Glagolitic = 55,
    Lisu = 56,
    Vai = 57,
    NKo = 58,
    Osmanya = 59,
    PhagsPa = 60,
    Gothic = 61,
    Deseret = 62,
    Tifinagh = 63
  );
  PTextScript = ^TextScript;

  // Windows.UI.Text.LinkType
  LinkType = (
    Undefined = 0,
    NotALink = 1,
    ClientLink = 2,
    FriendlyLinkName = 3,
    FriendlyLinkAddress = 4,
    AutoLink = 5,
    AutoLinkEmail = 6,
    AutoLinkPhone = 7,
    AutoLinkPath = 8
  );
  PLinkType = ^LinkType;

  // Windows.UI.Text.FontStyle
  FontStyle = (
    Normal = 0,
    Oblique = 1,
    Italic = 2
  );
  PFontStyle = ^FontStyle;

  // Windows.UI.Text.FontStretch
  FontStretch = (
    Undefined = 0,
    UltraCondensed = 1,
    ExtraCondensed = 2,
    Condensed = 3,
    SemiCondensed = 4,
    Normal = 5,
    SemiExpanded = 6,
    Expanded = 7,
    ExtraExpanded = 8,
    UltraExpanded = 9
  );
  PFontStretch = ^FontStretch;

  // Windows.UI.Text.FormatEffect
  FormatEffect = (
    Off = 0,
    &On = 1,
    Toggle = 2,
    Undefined = 3
  );
  PFormatEffect = ^FormatEffect;

  // Windows.UI.Text.TextRangeUnit
  TextRangeUnit = (
    Character = 0,
    Word = 1,
    Sentence = 2,
    Paragraph = 3,
    Line = 4,
    Story = 5,
    Screen = 6,
    Section = 7,
    Window = 8,
    CharacterFormat = 9,
    ParagraphFormat = 10,
    &Object = 11,
    HardParagraph = 12,
    Cluster = 13,
    Bold = 14,
    Italic = 15,
    Underline = 16,
    Strikethrough = 17,
    ProtectedText = 18,
    Link = 19,
    SmallCaps = 20,
    AllCaps = 21,
    Hidden = 22,
    Outline = 23,
    Shadow = 24,
    Imprint = 25,
    Disabled = 26,
    Revised = 27,
    Subscript = 28,
    Superscript = 29,
    FontBound = 30,
    LinkProtected = 31,
    ContentLink = 32
  );
  PTextRangeUnit = ^TextRangeUnit;

  // Windows.UI.Text.SelectionType
  SelectionType = (
    None = 0,
    InsertionPoint = 1,
    Normal = 2,
    InlineShape = 7,
    Shape = 8
  );
  PSelectionType = ^SelectionType;

  // Windows.UI.Text.SelectionOptions
  SelectionOptions = (
    StartActive = 1,
    AtEndOfLine = 2,
    Overtype = 4,
    Active = 8,
    Replace = 16
  );
  PSelectionOptions = ^SelectionOptions;

  // Windows.UI.Text.CaretType
  CaretType = (
    Normal = 0,
    Null = 1
  );
  PCaretType = ^CaretType;

  // Windows.UI.Xaml.TextReadingOrder
  TextReadingOrder = (
    Default = 0,
    UseFlowDirection = 0,
    DetectFromContent = 1
  );
  PTextReadingOrder = ^TextReadingOrder;

  // Windows.UI.Xaml.Navigation.NavigationCacheMode
  Navigation_NavigationCacheMode = (
    Disabled = 0,
    Required = 1,
    Enabled = 2
  );
  PNavigation_NavigationCacheMode = ^Navigation_NavigationCacheMode;

  // Windows.UI.Xaml.Controls.AutoSuggestionBoxTextChangeReason
  AutoSuggestionBoxTextChangeReason = (
    UserInput = 0,
    ProgrammaticChange = 1,
    SuggestionChosen = 2
  );
  PAutoSuggestionBoxTextChangeReason = ^AutoSuggestionBoxTextChangeReason;

  // Windows.UI.Xaml.Controls.NavigationViewDisplayMode
  NavigationViewDisplayMode = (
    Minimal = 0,
    Compact = 1,
    Expanded = 2
  );
  PNavigationViewDisplayMode = ^NavigationViewDisplayMode;

  // Windows.UI.Xaml.Controls.InkToolbarFlyoutItemKind
  InkToolbarFlyoutItemKind = (
    Simple = 0,
    Radio = 1,
    Check = 2,
    RadioCheck = 3
  );
  PInkToolbarFlyoutItemKind = ^InkToolbarFlyoutItemKind;

  // Windows.UI.Xaml.Controls.InkToolbarToggle
  InkToolbarToggle = (
    Ruler = 0,
    Custom = 1
  );
  PInkToolbarToggle = ^InkToolbarToggle;

  // Windows.UI.Xaml.Controls.InkToolbarTool
  InkToolbarTool = (
    BallpointPen = 0,
    Pencil = 1,
    Highlighter = 2,
    Eraser = 3,
    CustomPen = 4,
    CustomTool = 5
  );
  PInkToolbarTool = ^InkToolbarTool;

  // Windows.UI.Xaml.Controls.InkToolbarInitialControls
  InkToolbarInitialControls = (
    All = 0,
    None = 1,
    PensOnly = 2,
    AllExceptPens = 3
  );
  PInkToolbarInitialControls = ^InkToolbarInitialControls;

  // Windows.UI.Xaml.Navigation.NavigationMode
  Navigation_NavigationMode = (
    New = 0,
    Back = 1,
    Forward = 2,
    Refresh = 3
  );
  PNavigation_NavigationMode = ^Navigation_NavigationMode;

  // Windows.UI.Xaml.ElementSoundMode
  ElementSoundMode = (
    Default = 0,
    FocusOnly = 1,
    Off = 2
  );
  PElementSoundMode = ^ElementSoundMode;

  // Windows.UI.Xaml.Input.KeyboardNavigationMode
  Input_KeyboardNavigationMode = (
    Local = 0,
    Cycle = 1,
    Once = 2
  );
  PInput_KeyboardNavigationMode = ^Input_KeyboardNavigationMode;

  // Windows.UI.Xaml.TextLineBounds
  TextLineBounds = (
    Full = 0,
    TrimToCapHeight = 1,
    TrimToBaseline = 2,
    Tight = 3
  );
  PTextLineBounds = ^TextLineBounds;

  // Windows.UI.Xaml.OpticalMarginAlignment
  OpticalMarginAlignment = (
    None = 0,
    TrimSideBearings = 1
  );
  POpticalMarginAlignment = ^OpticalMarginAlignment;

  // Windows.UI.Xaml.GridUnitType
  GridUnitType = (
    Auto = 0,
    Pixel = 1,
    Star = 2
  );
  PGridUnitType = ^GridUnitType;

  // Windows.UI.Input.HoldingState
  HoldingState = (
    Started = 0,
    Completed = 1,
    Canceled = 2
  );
  PHoldingState = ^HoldingState;

  // Windows.UI.Xaml.Input.ManipulationModes
  Input_ManipulationModes = (
    None = 0,
    TranslateX = 1,
    TranslateY = 2,
    TranslateRailsX = 4,
    TranslateRailsY = 8,
    Rotate = 16,
    Scale = 32,
    TranslateInertia = 64,
    RotateInertia = 128,
    ScaleInertia = 256,
    All = 65535,
    System = 65536
  );
  PInput_ManipulationModes = ^Input_ManipulationModes;

  // Windows.UI.Xaml.Input.InputScopeNameValue
  Input_InputScopeNameValue = (
    Default = 0,
    Url = 1,
    EmailSmtpAddress = 5,
    PersonalFullName = 7,
    CurrencyAmountAndSymbol = 20,
    CurrencyAmount = 21,
    DateMonthNumber = 23,
    DateDayNumber = 24,
    DateYear = 25,
    Digits = 28,
    Number = 29,
    Password = 31,
    TelephoneNumber = 32,
    TelephoneCountryCode = 33,
    TelephoneAreaCode = 34,
    TelephoneLocalNumber = 35,
    TimeHour = 37,
    TimeMinutesOrSeconds = 38,
    NumberFullWidth = 39,
    AlphanumericHalfWidth = 40,
    AlphanumericFullWidth = 41,
    Hiragana = 44,
    KatakanaHalfWidth = 45,
    KatakanaFullWidth = 46,
    Hanja = 47,
    HangulHalfWidth = 48,
    HangulFullWidth = 49,
    Search = 50,
    Formula = 51,
    SearchIncremental = 52,
    ChineseHalfWidth = 53,
    ChineseFullWidth = 54,
    NativeScript = 55,
    Text = 57,
    Chat = 58,
    NameOrPhoneNumber = 59,
    EmailNameOrAddress = 60,
    Maps = 62,
    NumericPassword = 63,
    NumericPin = 64,
    AlphanumericPin = 65,
    FormulaNumber = 67,
    ChatWithoutEmoji = 68
  );
  PInput_InputScopeNameValue = ^Input_InputScopeNameValue;

  // Windows.UI.Xaml.Visibility
  Visibility = (
    Visible = 0,
    Collapsed = 1
  );
  PVisibility = ^Visibility;

  // Windows.UI.Xaml.LineStackingStrategy
  LineStackingStrategy = (
    MaxHeight = 0,
    BlockLineHeight = 1,
    BaselineToBaseline = 2
  );
  PLineStackingStrategy = ^LineStackingStrategy;

  // Windows.UI.Xaml.TextAlignment
  TextAlignment = (
    Center = 0,
    Left = 1,
    Start = 1,
    Right = 2,
    &End = 2,
    Justify = 3,
    DetectFromContent = 4
  );
  PTextAlignment = ^TextAlignment;

  // Windows.UI.Xaml.TextTrimming
  TextTrimming = (
    None = 0,
    CharacterEllipsis = 1,
    WordEllipsis = 2,
    Clip = 3
  );
  PTextTrimming = ^TextTrimming;

  // Windows.UI.Xaml.TextWrapping
  TextWrapping = (
    NoWrap = 1,
    Wrap = 2,
    WrapWholeWords = 3
  );
  PTextWrapping = ^TextWrapping;

  // Windows.UI.Xaml.FocusState
  FocusState = (
    Unfocused = 0,
    Pointer = 1,
    Keyboard = 2,
    Programmatic = 3
  );
  PFocusState = ^FocusState;

  // Windows.UI.Xaml.Documents.LogicalDirection
  Documents_LogicalDirection = (
    Backward = 0,
    Forward = 1
  );
  PDocuments_LogicalDirection = ^Documents_LogicalDirection;

  // Windows.UI.Xaml.FlowDirection
  FlowDirection = (
    LeftToRight = 0,
    RightToLeft = 1
  );
  PFlowDirection = ^FlowDirection;

  // Windows.UI.Xaml.DurationType
  DurationType = (
    Automatic = 0,
    TimeSpan = 1,
    Forever = 2
  );
  PDurationType = ^DurationType;

  // Windows.UI.Xaml.VerticalAlignment
  VerticalAlignment = (
    Top = 0,
    Center = 1,
    Bottom = 2,
    Stretch = 3
  );
  PVerticalAlignment = ^VerticalAlignment;

  // Windows.UI.Xaml.HorizontalAlignment
  HorizontalAlignment = (
    Left = 0,
    Center = 1,
    Right = 2,
    Stretch = 3
  );
  PHorizontalAlignment = ^HorizontalAlignment;

  // Windows.UI.Xaml.Interop.TypeKind
  Interop_TypeKind = (
    Primitive = 0,
    Metadata = 1,
    Custom = 2
  );
  PInterop_TypeKind = ^Interop_TypeKind;

  // Windows.UI.ViewManagement.Core.CoreInputViewOcclusionKind
  Core_CoreInputViewOcclusionKind = (
    Docked = 0,
    Floating = 1,
    Overlay = 2
  );
  PCore_CoreInputViewOcclusionKind = ^Core_CoreInputViewOcclusionKind;

  // Windows.UI.Notifications.ToastDismissalReason
  ToastDismissalReason = (
    UserCanceled = 0,
    ApplicationHidden = 1,
    TimedOut = 2
  );
  PToastDismissalReason = ^ToastDismissalReason;

  // Windows.UI.Input.Inking.InkPresenterPredefinedConfiguration
  InkPresenterPredefinedConfiguration = (
    SimpleSinglePointer = 0,
    SimpleMultiplePointer = 1
  );
  PInkPresenterPredefinedConfiguration = ^InkPresenterPredefinedConfiguration;

  // Windows.UI.Input.Inking.PenTipShape
  PenTipShape = (
    Circle = 0,
    Rectangle = 1
  );
  PPenTipShape = ^PenTipShape;

  // Windows.UI.Input.Inking.InkInputRightDragAction
  InkInputRightDragAction = (
    LeaveUnprocessed = 0,
    AllowProcessing = 1
  );
  PInkInputRightDragAction = ^InkInputRightDragAction;

  // Windows.UI.Input.Inking.InkInputProcessingMode
  InkInputProcessingMode = (
    None = 0,
    Inking = 1,
    Erasing = 2
  );
  PInkInputProcessingMode = ^InkInputProcessingMode;

  // Windows.UI.Core.CoreInputDeviceTypes
  CoreInputDeviceTypes = (
    None = 0,
    Touch = 1,
    Pen = 2,
    Mouse = 4
  );
  PCoreInputDeviceTypes = ^CoreInputDeviceTypes;

  // Windows.Graphics.DirectX.DirectXPrimitiveTopology
  DirectX_DirectXPrimitiveTopology = (
    Undefined = 0,
    PointList = 1,
    LineList = 2,
    LineStrip = 3,
    TriangleList = 4,
    TriangleStrip = 5
  );
  PDirectX_DirectXPrimitiveTopology = ^DirectX_DirectXPrimitiveTopology;

  // Windows.Graphics.DirectX.DirectXPixelFormat
  DirectX_DirectXPixelFormat = (
    Unknown = 0,
    R32G32B32A32Typeless = 1,
    R32G32B32A32Float = 2,
    R32G32B32A32UInt = 3,
    R32G32B32A32Int = 4,
    R32G32B32Typeless = 5,
    R32G32B32Float = 6,
    R32G32B32UInt = 7,
    R32G32B32Int = 8,
    R16G16B16A16Typeless = 9,
    R16G16B16A16Float = 10,
    R16G16B16A16UIntNormalized = 11,
    R16G16B16A16UInt = 12,
    R16G16B16A16IntNormalized = 13,
    R16G16B16A16Int = 14,
    R32G32Typeless = 15,
    R32G32Float = 16,
    R32G32UInt = 17,
    R32G32Int = 18,
    R32G8X24Typeless = 19,
    D32FloatS8X24UInt = 20,
    R32FloatX8X24Typeless = 21,
    X32TypelessG8X24UInt = 22,
    R10G10B10A2Typeless = 23,
    R10G10B10A2UIntNormalized = 24,
    R10G10B10A2UInt = 25,
    R11G11B10Float = 26,
    R8G8B8A8Typeless = 27,
    R8G8B8A8UIntNormalized = 28,
    R8G8B8A8UIntNormalizedSrgb = 29,
    R8G8B8A8UInt = 30,
    R8G8B8A8IntNormalized = 31,
    R8G8B8A8Int = 32,
    R16G16Typeless = 33,
    R16G16Float = 34,
    R16G16UIntNormalized = 35,
    R16G16UInt = 36,
    R16G16IntNormalized = 37,
    R16G16Int = 38,
    R32Typeless = 39,
    D32Float = 40,
    R32Float = 41,
    R32UInt = 42,
    R32Int = 43,
    R24G8Typeless = 44,
    D24UIntNormalizedS8UInt = 45,
    R24UIntNormalizedX8Typeless = 46,
    X24TypelessG8UInt = 47,
    R8G8Typeless = 48,
    R8G8UIntNormalized = 49,
    R8G8UInt = 50,
    R8G8IntNormalized = 51,
    R8G8Int = 52,
    R16Typeless = 53,
    R16Float = 54,
    D16UIntNormalized = 55,
    R16UIntNormalized = 56,
    R16UInt = 57,
    R16IntNormalized = 58,
    R16Int = 59,
    R8Typeless = 60,
    R8UIntNormalized = 61,
    R8UInt = 62,
    R8IntNormalized = 63,
    R8Int = 64,
    A8UIntNormalized = 65,
    R1UIntNormalized = 66,
    R9G9B9E5SharedExponent = 67,
    R8G8B8G8UIntNormalized = 68,
    G8R8G8B8UIntNormalized = 69,
    BC1Typeless = 70,
    BC1UIntNormalized = 71,
    BC1UIntNormalizedSrgb = 72,
    BC2Typeless = 73,
    BC2UIntNormalized = 74,
    BC2UIntNormalizedSrgb = 75,
    BC3Typeless = 76,
    BC3UIntNormalized = 77,
    BC3UIntNormalizedSrgb = 78,
    BC4Typeless = 79,
    BC4UIntNormalized = 80,
    BC4IntNormalized = 81,
    BC5Typeless = 82,
    BC5UIntNormalized = 83,
    BC5IntNormalized = 84,
    B5G6R5UIntNormalized = 85,
    B5G5R5A1UIntNormalized = 86,
    B8G8R8A8UIntNormalized = 87,
    B8G8R8X8UIntNormalized = 88,
    R10G10B10XRBiasA2UIntNormalized = 89,
    B8G8R8A8Typeless = 90,
    B8G8R8A8UIntNormalizedSrgb = 91,
    B8G8R8X8Typeless = 92,
    B8G8R8X8UIntNormalizedSrgb = 93,
    BC6HTypeless = 94,
    BC6H16UnsignedFloat = 95,
    BC6H16Float = 96,
    BC7Typeless = 97,
    BC7UIntNormalized = 98,
    BC7UIntNormalizedSrgb = 99,
    Ayuv = 100,
    Y410 = 101,
    Y416 = 102,
    NV12 = 103,
    P010 = 104,
    P016 = 105,
    Opaque420 = 106,
    Yuy2 = 107,
    Y210 = 108,
    Y216 = 109,
    NV11 = 110,
    AI44 = 111,
    IA44 = 112,
    P8 = 113,
    A8P8 = 114,
    B4G4R4A4UIntNormalized = 115,
    P208 = 130,
    V208 = 131,
    V408 = 132,
    SamplerFeedbackMinMipOpaque = 189,
    SamplerFeedbackMipRegionUsedOpaque = 190
  );
  PDirectX_DirectXPixelFormat = ^DirectX_DirectXPixelFormat;

  // Windows.Graphics.DirectX.DirectXAlphaMode
  DirectX_DirectXAlphaMode = (
    Unspecified = 0,
    Premultiplied = 1,
    Straight = 2,
    Ignore = 3
  );
  PDirectX_DirectXAlphaMode = ^DirectX_DirectXAlphaMode;

  // Windows.UI.Composition.CompositionStretch
  CompositionStretch = (
    None = 0,
    Fill = 1,
    Uniform = 2,
    UniformToFill = 3
  );
  PCompositionStretch = ^CompositionStretch;

  // Windows.UI.Composition.CompositionBitmapInterpolationMode
  CompositionBitmapInterpolationMode = (
    NearestNeighbor = 0,
    Linear = 1,
    MagLinearMinLinearMipLinear = 2,
    MagLinearMinLinearMipNearest = 3,
    MagLinearMinNearestMipLinear = 4,
    MagLinearMinNearestMipNearest = 5,
    MagNearestMinLinearMipLinear = 6,
    MagNearestMinLinearMipNearest = 7,
    MagNearestMinNearestMipLinear = 8,
    MagNearestMinNearestMipNearest = 9
  );
  PCompositionBitmapInterpolationMode = ^CompositionBitmapInterpolationMode;

  // Windows.UI.Composition.CompositionBatchTypes
  CompositionBatchTypes = (
    None = 0,
    Animation = 1,
    Effect = 2,
    InfiniteAnimation = 4,
    AllAnimations = 5
  );
  PCompositionBatchTypes = ^CompositionBatchTypes;

  // Windows.UI.Composition.CompositionGetValueStatus
  CompositionGetValueStatus = (
    Succeeded = 0,
    TypeMismatch = 1,
    NotFound = 2
  );
  PCompositionGetValueStatus = ^CompositionGetValueStatus;

  // Windows.UI.Composition.CompositionEffectFactoryLoadStatus
  CompositionEffectFactoryLoadStatus = (
    Success = 0,
    EffectTooComplex = 1,
    Pending = 2,
    Other = -1
  );
  PCompositionEffectFactoryLoadStatus = ^CompositionEffectFactoryLoadStatus;

  // Windows.UI.Composition.CompositionCompositeMode
  CompositionCompositeMode = (
    Inherit = 0,
    SourceOver = 1,
    DestinationInvert = 2,
    MinBlend = 3
  );
  PCompositionCompositeMode = ^CompositionCompositeMode;

  // Windows.UI.Composition.CompositionBorderMode
  CompositionBorderMode = (
    Inherit = 0,
    Soft = 1,
    Hard = 2
  );
  PCompositionBorderMode = ^CompositionBorderMode;

  // Windows.UI.Composition.CompositionBackfaceVisibility
  CompositionBackfaceVisibility = (
    Inherit = 0,
    Visible = 1,
    Hidden = 2
  );
  PCompositionBackfaceVisibility = ^CompositionBackfaceVisibility;

  // Windows.UI.Composition.CompositionColorSpace
  CompositionColorSpace = (
    Auto = 0,
    Hsl = 1,
    Rgb = 2,
    HslLinear = 3,
    RgbLinear = 4
  );
  PCompositionColorSpace = ^CompositionColorSpace;

  // Windows.UI.WindowManagement.WindowingEnvironmentKind
  WindowManagement_WindowingEnvironmentKind = (
    Unknown = 0,
    Overlapped = 1,
    Tiled = 2
  );
  PWindowManagement_WindowingEnvironmentKind = ^WindowManagement_WindowingEnvironmentKind;

  // Windows.Data.Xml.Dom.NodeType
  Xml_Dom_NodeType = (
    Invalid = 0,
    ElementNode = 1,
    AttributeNode = 2,
    TextNode = 3,
    DataSectionNode = 4,
    EntityReferenceNode = 5,
    EntityNode = 6,
    ProcessingInstructionNode = 7,
    CommentNode = 8,
    DocumentNode = 9,
    DocumentTypeNode = 10,
    DocumentFragmentNode = 11,
    NotationNode = 12
  );
  PXml_Dom_NodeType = ^Xml_Dom_NodeType;

  // Windows.Networking.Sockets.SocketSslErrorSeverity
  SocketSslErrorSeverity = (
    None = 0,
    Ignorable = 1,
    Fatal = 2
  );
  PSocketSslErrorSeverity = ^SocketSslErrorSeverity;

  // Windows.Networking.NetworkOperators.UiccAccessCondition
  UiccAccessCondition = (
    AlwaysAllowed = 0,
    Pin1 = 1,
    Pin2 = 2,
    Pin3 = 3,
    Pin4 = 4,
    Administrative5 = 5,
    Administrative6 = 6,
    NeverAllowed = 7
  );
  PUiccAccessCondition = ^UiccAccessCondition;

  // Windows.Networking.NetworkOperators.UiccAppRecordKind
  UiccAppRecordKind = (
    Unknown = 0,
    Transparent = 1,
    RecordOriented = 2
  );
  PUiccAppRecordKind = ^UiccAppRecordKind;

  // Windows.Networking.NetworkOperators.UiccAppKind
  UiccAppKind = (
    Unknown = 0,
    MF = 1,
    MFSim = 2,
    MFRuim = 3,
    USim = 4,
    CSim = 5,
    ISim = 6
  );
  PUiccAppKind = ^UiccAppKind;

  // Windows.Networking.NetworkOperators.MobileBroadbandUiccAppOperationStatus
  MobileBroadbandUiccAppOperationStatus = (
    Success = 0,
    InvalidUiccFilePath = 1,
    AccessConditionNotHeld = 2,
    UiccBusy = 3
  );
  PMobileBroadbandUiccAppOperationStatus = ^MobileBroadbandUiccAppOperationStatus;

  // Windows.Networking.NetworkOperators.MobileBroadbandRadioState
  MobileBroadbandRadioState = (
    Off = 0,
    &On = 1
  );
  PMobileBroadbandRadioState = ^MobileBroadbandRadioState;

  // Windows.Networking.NetworkOperators.MobileBroadbandDeviceType
  MobileBroadbandDeviceType = (
    Unknown = 0,
    Embedded = 1,
    Removable = 2,
    Remote = 3
  );
  PMobileBroadbandDeviceType = ^MobileBroadbandDeviceType;

  // Windows.Networking.NetworkOperators.NetworkDeviceStatus
  NetworkDeviceStatus = (
    DeviceNotReady = 0,
    DeviceReady = 1,
    SimNotInserted = 2,
    BadSim = 3,
    DeviceHardwareFailure = 4,
    AccountNotActivated = 5,
    DeviceLocked = 6,
    DeviceBlocked = 7
  );
  PNetworkDeviceStatus = ^NetworkDeviceStatus;

  // Windows.Networking.NetworkOperators.DataClasses
  DataClasses = (
    None = 0,
    Gprs = 1,
    Edge = 2,
    Umts = 4,
    Hsdpa = 8,
    Hsupa = 16,
    LteAdvanced = 32,
    Cdma1xRtt = 65536,
    Cdma1xEvdo = 131072,
    Cdma1xEvdoRevA = 262144,
    Cdma1xEvdv = 524288,
    Cdma3xRtt = 1048576,
    Cdma1xEvdoRevB = 2097152,
    CdmaUmb = 4194304,
    Custom = -2147483648
  );
  PDataClasses = ^DataClasses;

  // Windows.Networking.NetworkOperators.NetworkRegistrationState
  NetworkRegistrationState = (
    None = 0,
    Deregistered = 1,
    Searching = 2,
    Home = 3,
    Roaming = 4,
    Partner = 5,
    Denied = 6
  );
  PNetworkRegistrationState = ^NetworkRegistrationState;

  // Windows.Networking.NetworkOperators.MobileBroadbandAccountWatcherStatus
  MobileBroadbandAccountWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopped = 3,
    Aborted = 4
  );
  PMobileBroadbandAccountWatcherStatus = ^MobileBroadbandAccountWatcherStatus;

  // Windows.Media.Capture.PowerlineFrequency
  Capture_PowerlineFrequency = (
    Disabled = 0,
    FiftyHertz = 1,
    SixtyHertz = 2,
    Auto = 3
  );
  PCapture_PowerlineFrequency = ^Capture_PowerlineFrequency;

  // Windows.Media.Capture.MediaStreamType
  Capture_MediaStreamType = (
    VideoPreview = 0,
    VideoRecord = 1,
    Audio = 2,
    Photo = 3
  );
  PCapture_MediaStreamType = ^Capture_MediaStreamType;

  // Windows.Media.Core.TimedMetadataKind
  Core_TimedMetadataKind = (
    Caption = 0,
    Chapter = 1,
    Custom = 2,
    Data = 3,
    Description = 4,
    Subtitle = 5,
    ImageSubtitle = 6,
    Speech = 7
  );
  PCore_TimedMetadataKind = ^Core_TimedMetadataKind;

  // Windows.Media.Core.TimedMetadataTrackErrorCode
  Core_TimedMetadataTrackErrorCode = (
    None = 0,
    DataFormatError = 1,
    NetworkError = 2,
    InternalError = 3
  );
  PCore_TimedMetadataTrackErrorCode = ^Core_TimedMetadataTrackErrorCode;

  // Windows.Media.Core.MediaTrackKind
  Core_MediaTrackKind = (
    Audio = 0,
    Video = 1,
    TimedMetadata = 2
  );
  PCore_MediaTrackKind = ^Core_MediaTrackKind;

  // Windows.Media.AudioProcessing
  AudioProcessing = (
    Default = 0,
    Raw = 1
  );
  PAudioProcessing = ^AudioProcessing;

  // Windows.Media.Audio.AudioGraphUnrecoverableError
  Audio_AudioGraphUnrecoverableError = (
    None = 0,
    AudioDeviceLost = 1,
    AudioSessionDisconnected = 2,
    UnknownFailure = 3
  );
  PAudio_AudioGraphUnrecoverableError = ^Audio_AudioGraphUnrecoverableError;

  // Windows.Media.Transcoding.TranscodeFailureReason
  Transcoding_TranscodeFailureReason = (
    None = 0,
    Unknown = 1,
    InvalidProfile = 2,
    CodecNotFound = 3
  );
  PTranscoding_TranscodeFailureReason = ^Transcoding_TranscodeFailureReason;

  // Windows.Media.Audio.AudioFileNodeCreationStatus
  Audio_AudioFileNodeCreationStatus = (
    Success = 0,
    FileNotFound = 1,
    InvalidFileType = 2,
    FormatNotSupported = 3,
    UnknownFailure = 4
  );
  PAudio_AudioFileNodeCreationStatus = ^Audio_AudioFileNodeCreationStatus;

  // Windows.Media.Capture.MediaCategory
  Capture_MediaCategory = (
    Other = 0,
    Communications = 1,
    Media = 2,
    GameChat = 3,
    Speech = 4
  );
  PCapture_MediaCategory = ^Capture_MediaCategory;

  // Windows.Media.Audio.AudioDeviceNodeCreationStatus
  Audio_AudioDeviceNodeCreationStatus = (
    Success = 0,
    DeviceNotAvailable = 1,
    FormatNotSupported = 2,
    UnknownFailure = 3,
    AccessDenied = 4
  );
  PAudio_AudioDeviceNodeCreationStatus = ^Audio_AudioDeviceNodeCreationStatus;

  // Windows.Media.AudioBufferAccessMode
  AudioBufferAccessMode = (
    Read = 0,
    ReadWrite = 1,
    Write = 2
  );
  PAudioBufferAccessMode = ^AudioBufferAccessMode;

  // Windows.System.Power.BatteryStatus
  Power_BatteryStatus = (
    NotPresent = 0,
    Discharging = 1,
    Idle = 2,
    Charging = 3
  );
  PPower_BatteryStatus = ^Power_BatteryStatus;

  // Windows.Devices.Sms.SmsModemErrorCode
  SmsModemErrorCode = (
    Other = 0,
    MessagingNetworkError = 1,
    SmsOperationNotSupportedByDevice = 2,
    SmsServiceNotSupportedByNetwork = 3,
    DeviceFailure = 4,
    MessageNotEncodedProperly = 5,
    MessageTooLarge = 6,
    DeviceNotReady = 7,
    NetworkNotReady = 8,
    InvalidSmscAddress = 9,
    NetworkFailure = 10,
    FixedDialingNumberRestricted = 11
  );
  PSmsModemErrorCode = ^SmsModemErrorCode;

  // Windows.Devices.Sms.SmsMessageType
  SmsMessageType = (
    Binary = 0,
    Text = 1,
    Wap = 2,
    App = 3,
    Broadcast = 4,
    Voicemail = 5,
    Status = 6
  );
  PSmsMessageType = ^SmsMessageType;

  // Windows.Devices.Sms.SmsDeviceStatus
  SmsDeviceStatus = (
    Off = 0,
    Ready = 1,
    SimNotInserted = 2,
    BadSim = 3,
    DeviceFailure = 4,
    SubscriptionNotActivated = 5,
    DeviceLocked = 6,
    DeviceBlocked = 7
  );
  PSmsDeviceStatus = ^SmsDeviceStatus;

  // Windows.Devices.Sms.CellularClass
  CellularClass = (
    None = 0,
    Gsm = 1,
    Cdma = 2
  );
  PCellularClass = ^CellularClass;

  // Windows.Devices.Sms.SmsMessageClass
  SmsMessageClass = (
    None = 0,
    Class0 = 1,
    Class1 = 2,
    Class2 = 3,
    Class3 = 4
  );
  PSmsMessageClass = ^SmsMessageClass;

  // Windows.Devices.PointOfService.LineDisplayScrollDirection
  LineDisplayScrollDirection = (
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3
  );
  PLineDisplayScrollDirection = ^LineDisplayScrollDirection;

  // Windows.Devices.PointOfService.LineDisplayTextAttribute
  LineDisplayTextAttribute = (
    Normal = 0,
    Blink = 1,
    Reverse = 2,
    ReverseBlink = 3
  );
  PLineDisplayTextAttribute = ^LineDisplayTextAttribute;

  // Windows.Devices.PointOfService.LineDisplayTextAttributeGranularity
  LineDisplayTextAttributeGranularity = (
    NotSupported = 0,
    EntireDisplay = 1,
    PerCharacter = 2
  );
  PLineDisplayTextAttributeGranularity = ^LineDisplayTextAttributeGranularity;

  // Windows.Devices.PointOfService.UnifiedPosPowerReportingType
  UnifiedPosPowerReportingType = (
    UnknownPowerReportingType = 0,
    Standard = 1,
    Advanced = 2
  );
  PUnifiedPosPowerReportingType = ^UnifiedPosPowerReportingType;

  // Windows.Devices.Enumeration.Pnp.PnpObjectType
  Pnp_PnpObjectType = (
    Unknown = 0,
    DeviceInterface = 1,
    DeviceContainer = 2,
    Device = 3,
    DeviceInterfaceClass = 4,
    AssociationEndpoint = 5,
    AssociationEndpointContainer = 6,
    AssociationEndpointService = 7,
    DevicePanel = 8
  );
  PPnp_PnpObjectType = ^Pnp_PnpObjectType;

  // Windows.Devices.Enumeration.DevicePickerDisplayStatusOptions
  DevicePickerDisplayStatusOptions = (
    None = 0,
    ShowProgress = 1,
    ShowDisconnectButton = 2,
    ShowRetryButton = 4
  );
  PDevicePickerDisplayStatusOptions = ^DevicePickerDisplayStatusOptions;

  // Windows.UI.Popups.Placement
  Popups_Placement = (
    Default = 0,
    Above = 1,
    Below = 2,
    Left = 3,
    Right = 4
  );
  PPopups_Placement = ^Popups_Placement;

  // Windows.Devices.Enumeration.DeviceClass
  DeviceClass = (
    All = 0,
    AudioCapture = 1,
    AudioRender = 2,
    PortableStorageDevice = 3,
    VideoCapture = 4,
    ImageScanner = 5,
    Location = 6
  );
  PDeviceClass = ^DeviceClass;

  // Windows.Devices.Enumeration.DeviceWatcherStatus
  DeviceWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PDeviceWatcherStatus = ^DeviceWatcherStatus;

  // Windows.Devices.Enumeration.Panel
  Panel = (
    Unknown = 0,
    Front = 1,
    Back = 2,
    Top = 3,
    Bottom = 4,
    Left = 5,
    Right = 6
  );
  PPanel = ^Panel;

  // Windows.Devices.Radios.RadioKind
  Radios_RadioKind = (
    Other = 0,
    WiFi = 1,
    MobileBroadband = 2,
    Bluetooth = 3,
    FM = 4
  );
  PRadios_RadioKind = ^Radios_RadioKind;

  // Windows.Devices.Radios.RadioState
  Radios_RadioState = (
    Unknown = 0,
    &On = 1,
    Off = 2,
    Disabled = 3
  );
  PRadios_RadioState = ^Radios_RadioState;

  // Windows.Devices.Radios.RadioAccessStatus
  Radios_RadioAccessStatus = (
    Unspecified = 0,
    Allowed = 1,
    DeniedByUser = 2,
    DeniedBySystem = 3
  );
  PRadios_RadioAccessStatus = ^Radios_RadioAccessStatus;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.GattClientCharacteristicConfigurationDescriptorValue
  GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue = (
    None = 0,
    Notify = 1,
    Indicate = 2
  );
  PGenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue = ^GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue;

  // Windows.Devices.Bluetooth.BluetoothConnectionStatus
  BluetoothConnectionStatus = (
    Disconnected = 0,
    Connected = 1
  );
  PBluetoothConnectionStatus = ^BluetoothConnectionStatus;

  // Windows.Devices.Bluetooth.BluetoothCacheMode
  BluetoothCacheMode = (
    Cached = 0,
    Uncached = 1
  );
  PBluetoothCacheMode = ^BluetoothCacheMode;

  // Windows.Networking.Sockets.SocketProtectionLevel
  SocketProtectionLevel = (
    PlainSocket = 0,
    Ssl = 1,
    SslAllowNullEncryption = 2,
    BluetoothEncryptionAllowNullAuthentication = 3,
    BluetoothEncryptionWithAuthentication = 4,
    Ssl3AllowWeakEncryption = 5,
    Tls10 = 6,
    Tls11 = 7,
    Tls12 = 8,
    Unspecified = 9
  );
  PSocketProtectionLevel = ^SocketProtectionLevel;

  // Windows.Devices.Bluetooth.BluetoothServiceCapabilities
  BluetoothServiceCapabilities = (
    None = 0,
    LimitedDiscoverableMode = 1,
    PositioningService = 8,
    NetworkingService = 16,
    RenderingService = 32,
    CapturingService = 64,
    ObjectTransferService = 128,
    AudioService = 256,
    TelephoneService = 512,
    InformationService = 1024
  );
  PBluetoothServiceCapabilities = ^BluetoothServiceCapabilities;

  // Windows.Devices.Bluetooth.BluetoothMinorClass
  BluetoothMinorClass = (
    Uncategorized = 0,
    ComputerDesktop = 1,
    ComputerServer = 2,
    ComputerLaptop = 3,
    ComputerHandheld = 4,
    ComputerPalmSize = 5,
    ComputerWearable = 6,
    ComputerTablet = 7,
    PhoneCellular = 1,
    PhoneCordless = 2,
    PhoneSmartPhone = 3,
    PhoneWired = 4,
    PhoneIsdn = 5,
    NetworkFullyAvailable = 0,
    NetworkUsed01To17Percent = 8,
    NetworkUsed17To33Percent = 16,
    NetworkUsed33To50Percent = 24,
    NetworkUsed50To67Percent = 32,
    NetworkUsed67To83Percent = 40,
    NetworkUsed83To99Percent = 48,
    NetworkNoServiceAvailable = 56,
    AudioVideoWearableHeadset = 1,
    AudioVideoHandsFree = 2,
    AudioVideoMicrophone = 4,
    AudioVideoLoudspeaker = 5,
    AudioVideoHeadphones = 6,
    AudioVideoPortableAudio = 7,
    AudioVideoCarAudio = 8,
    AudioVideoSetTopBox = 9,
    AudioVideoHifiAudioDevice = 10,
    AudioVideoVcr = 11,
    AudioVideoVideoCamera = 12,
    AudioVideoCamcorder = 13,
    AudioVideoVideoMonitor = 14,
    AudioVideoVideoDisplayAndLoudspeaker = 15,
    AudioVideoVideoConferencing = 16,
    AudioVideoGamingOrToy = 18,
    PeripheralJoystick = 1,
    PeripheralGamepad = 2,
    PeripheralRemoteControl = 3,
    PeripheralSensing = 4,
    PeripheralDigitizerTablet = 5,
    PeripheralCardReader = 6,
    PeripheralDigitalPen = 7,
    PeripheralHandheldScanner = 8,
    PeripheralHandheldGesture = 9,
    WearableWristwatch = 1,
    WearablePager = 2,
    WearableJacket = 3,
    WearableHelmet = 4,
    WearableGlasses = 5,
    ToyRobot = 1,
    ToyVehicle = 2,
    ToyDoll = 3,
    ToyController = 4,
    ToyGame = 5,
    HealthBloodPressureMonitor = 1,
    HealthThermometer = 2,
    HealthWeighingScale = 3,
    HealthGlucoseMeter = 4,
    HealthPulseOximeter = 5,
    HealthHeartRateMonitor = 6,
    HealthHealthDataDisplay = 7,
    HealthStepCounter = 8,
    HealthBodyCompositionAnalyzer = 9,
    HealthPeakFlowMonitor = 10,
    HealthMedicationMonitor = 11,
    HealthKneeProsthesis = 12,
    HealthAnkleProsthesis = 13,
    HealthGenericHealthManager = 14,
    HealthPersonalMobilityDevice = 15
  );
  PBluetoothMinorClass = ^BluetoothMinorClass;

  // Windows.Devices.Bluetooth.BluetoothMajorClass
  BluetoothMajorClass = (
    Miscellaneous = 0,
    Computer = 1,
    Phone = 2,
    NetworkAccessPoint = 3,
    AudioVideo = 4,
    Peripheral = 5,
    Imaging = 6,
    Wearable = 7,
    Toy = 8,
    Health = 9
  );
  PBluetoothMajorClass = ^BluetoothMajorClass;

  // Windows.Networking.HostNameType
  HostNameType = (
    DomainName = 0,
    Ipv4 = 1,
    Ipv6 = 2,
    Bluetooth = 3
  );
  PHostNameType = ^HostNameType;

  // Windows.Networking.Connectivity.NetworkEncryptionType
  NetworkEncryptionType = (
    None = 0,
    Unknown = 1,
    Wep = 2,
    Wep40 = 3,
    Wep104 = 4,
    Tkip = 5,
    Ccmp = 6,
    WpaUseGroup = 7,
    RsnUseGroup = 8,
    Ihv = 9
  );
  PNetworkEncryptionType = ^NetworkEncryptionType;

  // Windows.Networking.Connectivity.NetworkAuthenticationType
  NetworkAuthenticationType = (
    None = 0,
    Unknown = 1,
    Open80211 = 2,
    SharedKey80211 = 3,
    Wpa = 4,
    WpaPsk = 5,
    WpaNone = 6,
    Rsna = 7,
    RsnaPsk = 8,
    Ihv = 9,
    Wpa3 = 10,
    Wpa3Sae = 11,
    Owe = 12
  );
  PNetworkAuthenticationType = ^NetworkAuthenticationType;

  // Windows.Networking.Connectivity.RoamingStates
  RoamingStates = (
    None = 0,
    NotRoaming = 1,
    Roaming = 2
  );
  PRoamingStates = ^RoamingStates;

  // Windows.Networking.Connectivity.NetworkCostType
  NetworkCostType = (
    Unknown = 0,
    Unrestricted = 1,
    Fixed = 2,
    Variable = 3
  );
  PNetworkCostType = ^NetworkCostType;

  // Windows.Networking.Connectivity.NetworkConnectivityLevel
  NetworkConnectivityLevel = (
    None = 0,
    LocalAccess = 1,
    ConstrainedInternetAccess = 2,
    InternetAccess = 3
  );
  PNetworkConnectivityLevel = ^NetworkConnectivityLevel;

  // Windows.Networking.Connectivity.NetworkTypes
  NetworkTypes = (
    None = 0,
    Internet = 1,
    PrivateNetwork = 2
  );
  PNetworkTypes = ^NetworkTypes;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus
  GenericAttributeProfile_GattCommunicationStatus = (
    Success = 0,
    Unreachable = 1,
    ProtocolError = 2,
    AccessDenied = 3
  );
  PGenericAttributeProfile_GattCommunicationStatus = ^GenericAttributeProfile_GattCommunicationStatus;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.GattWriteOption
  GenericAttributeProfile_GattWriteOption = (
    WriteWithResponse = 0,
    WriteWithoutResponse = 1
  );
  PGenericAttributeProfile_GattWriteOption = ^GenericAttributeProfile_GattWriteOption;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.GattRequestState
  GenericAttributeProfile_GattRequestState = (
    Pending = 0,
    Completed = 1,
    Canceled = 2
  );
  PGenericAttributeProfile_GattRequestState = ^GenericAttributeProfile_GattRequestState;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.GattProtectionLevel
  GenericAttributeProfile_GattProtectionLevel = (
    Plain = 0,
    AuthenticationRequired = 1,
    EncryptionRequired = 2,
    EncryptionAndAuthenticationRequired = 3
  );
  PGenericAttributeProfile_GattProtectionLevel = ^GenericAttributeProfile_GattProtectionLevel;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.GattCharacteristicProperties
  GenericAttributeProfile_GattCharacteristicProperties = (
    None = 0,
    Broadcast = 1,
    Read = 2,
    WriteWithoutResponse = 4,
    Write = 8,
    Notify = 16,
    Indicate = 32,
    AuthenticatedSignedWrites = 64,
    ExtendedProperties = 128,
    ReliableWrites = 256,
    WritableAuxiliaries = 512
  );
  PGenericAttributeProfile_GattCharacteristicProperties = ^GenericAttributeProfile_GattCharacteristicProperties;

  // Windows.Devices.Bluetooth.BluetoothError
  BluetoothError = (
    Success = 0,
    RadioNotAvailable = 1,
    ResourceInUse = 2,
    DeviceNotConnected = 3,
    OtherError = 4,
    DisabledByPolicy = 5,
    NotSupported = 6,
    DisabledByUser = 7,
    ConsentRequired = 8,
    TransportNotSupported = 9
  );
  PBluetoothError = ^BluetoothError;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.GattSessionStatus
  GenericAttributeProfile_GattSessionStatus = (
    Closed = 0,
    Active = 1
  );
  PGenericAttributeProfile_GattSessionStatus = ^GenericAttributeProfile_GattSessionStatus;

  // Windows.ApplicationModel.DataTransfer.DataPackageOperation
  DataPackageOperation = (
    None = 0,
    Copy = 1,
    Move = 2,
    Link = 4
  );
  PDataPackageOperation = ^DataPackageOperation;

  // Windows.ApplicationModel.Activation.ApplicationExecutionState
  Activation_ApplicationExecutionState = (
    NotRunning = 0,
    Running = 1,
    Suspended = 2,
    Terminated = 3,
    ClosedByUser = 4
  );
  PActivation_ApplicationExecutionState = ^Activation_ApplicationExecutionState;

  // Windows.ApplicationModel.Activation.ActivationKind
  Activation_ActivationKind = (
    Launch = 0,
    Search = 1,
    ShareTarget = 2,
    &File = 3,
    Protocol = 4,
    FileOpenPicker = 5,
    FileSavePicker = 6,
    CachedFileUpdater = 7,
    ContactPicker = 8,
    Device = 9,
    PrintTaskSettings = 10,
    CameraSettings = 11,
    RestrictedLaunch = 12,
    AppointmentsProvider = 13,
    Contact = 14,
    LockScreenCall = 15,
    VoiceCommand = 16,
    LockScreen = 17,
    PickerReturned = 1000,
    WalletAction = 1001,
    PickFileContinuation = 1002,
    PickSaveFileContinuation = 1003,
    PickFolderContinuation = 1004,
    WebAuthenticationBrokerContinuation = 1005,
    WebAccountProvider = 1006,
    ComponentUI = 1007,
    ProtocolForResults = 1009,
    ToastNotification = 1010,
    Print3DWorkflow = 1011,
    DialReceiver = 1012,
    DevicePairing = 1013,
    UserDataAccountsProvider = 1014,
    FilePickerExperience = 1015,
    LockScreenComponent = 1016,
    ContactPanel = 1017,
    PrintWorkflowForegroundTask = 1018,
    GameUIProvider = 1019,
    StartupTask = 1020,
    CommandLineLaunch = 1021,
    BarcodeScannerProvider = 1022
  );
  PActivation_ActivationKind = ^Activation_ActivationKind;

  // Windows.System.VirtualKeyModifiers
  VirtualKeyModifiers = (
    None = 0,
    Control = 1,
    Menu = 2,
    Shift = 4,
    Windows = 8
  );
  PVirtualKeyModifiers = ^VirtualKeyModifiers;

  // Windows.UI.Input.PointerUpdateKind
  PointerUpdateKind = (
    Other = 0,
    LeftButtonPressed = 1,
    LeftButtonReleased = 2,
    RightButtonPressed = 3,
    RightButtonReleased = 4,
    MiddleButtonPressed = 5,
    MiddleButtonReleased = 6,
    XButton1Pressed = 7,
    XButton1Released = 8,
    XButton2Pressed = 9,
    XButton2Released = 10
  );
  PPointerUpdateKind = ^PointerUpdateKind;

  // Windows.Devices.Input.PointerDeviceType
  Input_PointerDeviceType = (
    Touch = 0,
    Pen = 1,
    Mouse = 2
  );
  PInput_PointerDeviceType = ^Input_PointerDeviceType;

  // Windows.UI.Core.CoreWindowActivationState
  CoreWindowActivationState = (
    CodeActivated = 0,
    Deactivated = 1,
    PointerActivated = 2
  );
  PCoreWindowActivationState = ^CoreWindowActivationState;

  // Windows.System.VirtualKey
  VirtualKey = (
    None = 0,
    LeftButton = 1,
    RightButton = 2,
    Cancel = 3,
    MiddleButton = 4,
    XButton1 = 5,
    XButton2 = 6,
    Back = 8,
    Tab = 9,
    Clear = 12,
    Enter = 13,
    Shift = 16,
    Control = 17,
    Menu = 18,
    Pause = 19,
    CapitalLock = 20,
    Kana = 21,
    Hangul = 21,
    ImeOn = 22,
    Junja = 23,
    Final = 24,
    Hanja = 25,
    Kanji = 25,
    ImeOff = 26,
    Escape = 27,
    Convert = 28,
    NonConvert = 29,
    Accept = 30,
    ModeChange = 31,
    Space = 32,
    PageUp = 33,
    PageDown = 34,
    &End = 35,
    Home = 36,
    Left = 37,
    Up = 38,
    Right = 39,
    Down = 40,
    Select = 41,
    Print = 42,
    Execute = 43,
    Snapshot = 44,
    Insert = 45,
    Delete = 46,
    Help = 47,
    Number0 = 48,
    Number1 = 49,
    Number2 = 50,
    Number3 = 51,
    Number4 = 52,
    Number5 = 53,
    Number6 = 54,
    Number7 = 55,
    Number8 = 56,
    Number9 = 57,
    A = 65,
    B = 66,
    C = 67,
    D = 68,
    E = 69,
    F = 70,
    G = 71,
    H = 72,
    I = 73,
    J = 74,
    K = 75,
    L = 76,
    M = 77,
    N = 78,
    O = 79,
    P = 80,
    Q = 81,
    R = 82,
    S = 83,
    T = 84,
    U = 85,
    V = 86,
    W = 87,
    X = 88,
    Y = 89,
    Z = 90,
    LeftWindows = 91,
    RightWindows = 92,
    Application = 93,
    Sleep = 95,
    NumberPad0 = 96,
    NumberPad1 = 97,
    NumberPad2 = 98,
    NumberPad3 = 99,
    NumberPad4 = 100,
    NumberPad5 = 101,
    NumberPad6 = 102,
    NumberPad7 = 103,
    NumberPad8 = 104,
    NumberPad9 = 105,
    Multiply = 106,
    Add = 107,
    Separator = 108,
    Subtract = 109,
    Decimal = 110,
    Divide = 111,
    F1 = 112,
    F2 = 113,
    F3 = 114,
    F4 = 115,
    F5 = 116,
    F6 = 117,
    F7 = 118,
    F8 = 119,
    F9 = 120,
    F10 = 121,
    F11 = 122,
    F12 = 123,
    F13 = 124,
    F14 = 125,
    F15 = 126,
    F16 = 127,
    F17 = 128,
    F18 = 129,
    F19 = 130,
    F20 = 131,
    F21 = 132,
    F22 = 133,
    F23 = 134,
    F24 = 135,
    NavigationView = 136,
    NavigationMenu = 137,
    NavigationUp = 138,
    NavigationDown = 139,
    NavigationLeft = 140,
    NavigationRight = 141,
    NavigationAccept = 142,
    NavigationCancel = 143,
    NumberKeyLock = 144,
    Scroll = 145,
    LeftShift = 160,
    RightShift = 161,
    LeftControl = 162,
    RightControl = 163,
    LeftMenu = 164,
    RightMenu = 165,
    GoBack = 166,
    GoForward = 167,
    Refresh = 168,
    Stop = 169,
    Search = 170,
    Favorites = 171,
    GoHome = 172,
    GamepadA = 195,
    GamepadB = 196,
    GamepadX = 197,
    GamepadY = 198,
    GamepadRightShoulder = 199,
    GamepadLeftShoulder = 200,
    GamepadLeftTrigger = 201,
    GamepadRightTrigger = 202,
    GamepadDPadUp = 203,
    GamepadDPadDown = 204,
    GamepadDPadLeft = 205,
    GamepadDPadRight = 206,
    GamepadMenu = 207,
    GamepadView = 208,
    GamepadLeftThumbstickButton = 209,
    GamepadRightThumbstickButton = 210,
    GamepadLeftThumbstickUp = 211,
    GamepadLeftThumbstickDown = 212,
    GamepadLeftThumbstickRight = 213,
    GamepadLeftThumbstickLeft = 214,
    GamepadRightThumbstickUp = 215,
    GamepadRightThumbstickDown = 216,
    GamepadRightThumbstickRight = 217,
    GamepadRightThumbstickLeft = 218
  );
  PVirtualKey = ^VirtualKey;

  // Windows.UI.Core.CoreVirtualKeyStates
  CoreVirtualKeyStates = (
    None = 0,
    Down = 1,
    Locked = 2
  );
  PCoreVirtualKeyStates = ^CoreVirtualKeyStates;

  // Windows.UI.Core.CoreCursorType
  CoreCursorType = (
    Arrow = 0,
    Cross = 1,
    Custom = 2,
    Hand = 3,
    Help = 4,
    IBeam = 5,
    SizeAll = 6,
    SizeNortheastSouthwest = 7,
    SizeNorthSouth = 8,
    SizeNorthwestSoutheast = 9,
    SizeWestEast = 10,
    UniversalNo = 11,
    UpArrow = 12,
    Wait = 13,
    Pin = 14,
    Person = 15
  );
  PCoreCursorType = ^CoreCursorType;

  // Windows.UI.Core.CoreWindowFlowDirection
  CoreWindowFlowDirection = (
    LeftToRight = 0,
    RightToLeft = 1
  );
  PCoreWindowFlowDirection = ^CoreWindowFlowDirection;

  // Windows.UI.Core.CoreDispatcherPriority
  CoreDispatcherPriority = (
    Idle = -2,
    Low = -1,
    Normal = 0,
    High = 1
  );
  PCoreDispatcherPriority = ^CoreDispatcherPriority;

  // Windows.UI.Core.CoreProcessEventsOption
  CoreProcessEventsOption = (
    ProcessOneAndAllPending = 0,
    ProcessOneIfPresent = 1,
    ProcessUntilQuit = 2,
    ProcessAllIfPresent = 3
  );
  PCoreProcessEventsOption = ^CoreProcessEventsOption;

  // Windows.System.DispatcherQueuePriority
  DispatcherQueuePriority = (
    Low = -10,
    Normal = 0,
    High = 10
  );
  PDispatcherQueuePriority = ^DispatcherQueuePriority;

  // Windows.System.RemoteSystems.RemoteSystemStatus
  RemoteSystems_RemoteSystemStatus = (
    Unavailable = 0,
    DiscoveringAvailability = 1,
    Available = 2,
    Unknown = 3
  );
  PRemoteSystems_RemoteSystemStatus = ^RemoteSystems_RemoteSystemStatus;

  // Windows.Storage.FileProperties.PropertyPrefetchOptions
  FileProperties_PropertyPrefetchOptions = (
    None = 0,
    MusicProperties = 1,
    VideoProperties = 2,
    ImageProperties = 4,
    DocumentProperties = 8,
    BasicProperties = 16
  );
  PFileProperties_PropertyPrefetchOptions = ^FileProperties_PropertyPrefetchOptions;

  // Windows.Storage.FileProperties.ThumbnailOptions
  FileProperties_ThumbnailOptions = (
    None = 0,
    ReturnOnlyIfCached = 1,
    ResizeThumbnail = 2,
    UseCurrentScale = 4
  );
  PFileProperties_ThumbnailOptions = ^FileProperties_ThumbnailOptions;

  // Windows.Storage.FileProperties.ThumbnailMode
  FileProperties_ThumbnailMode = (
    PicturesView = 0,
    VideosView = 1,
    MusicView = 2,
    DocumentsView = 3,
    ListView = 4,
    SingleItem = 5
  );
  PFileProperties_ThumbnailMode = ^FileProperties_ThumbnailMode;

  // Windows.Storage.Search.DateStackOption
  Search_DateStackOption = (
    None = 0,
    Year = 1,
    Month = 2
  );
  PSearch_DateStackOption = ^Search_DateStackOption;

  // Windows.Storage.Search.IndexerOption
  Search_IndexerOption = (
    UseIndexerWhenAvailable = 0,
    OnlyUseIndexer = 1,
    DoNotUseIndexer = 2,
    OnlyUseIndexerAndOptimizeForIndexedProperties = 3
  );
  PSearch_IndexerOption = ^Search_IndexerOption;

  // Windows.Storage.Search.FolderDepth
  Search_FolderDepth = (
    Shallow = 0,
    Deep = 1
  );
  PSearch_FolderDepth = ^Search_FolderDepth;

  // Windows.Services.Store.StoreQueueItemExtendedState
  StoreQueueItemExtendedState = (
    ActivePending = 0,
    ActiveStarting = 1,
    ActiveAcquiringLicense = 2,
    ActiveDownloading = 3,
    ActiveRestoringData = 4,
    ActiveInstalling = 5,
    Completed = 6,
    Canceled = 7,
    Paused = 8,
    Error = 9,
    PausedPackagesInUse = 10,
    PausedLowBattery = 11,
    PausedWiFiRecommended = 12,
    PausedWiFiRequired = 13,
    PausedReadyToInstall = 14
  );
  PStoreQueueItemExtendedState = ^StoreQueueItemExtendedState;

  // Windows.Services.Store.StoreQueueItemState
  StoreQueueItemState = (
    Active = 0,
    Completed = 1,
    Canceled = 2,
    Error = 3,
    Paused = 4
  );
  PStoreQueueItemState = ^StoreQueueItemState;

  // Windows.Services.Store.StoreQueueItemKind
  StoreQueueItemKind = (
    Install = 0,
    Update = 1,
    Repair = 2
  );
  PStoreQueueItemKind = ^StoreQueueItemKind;

  // Windows.Services.Store.StorePackageUpdateState
  StorePackageUpdateState = (
    Pending = 0,
    Downloading = 1,
    Deploying = 2,
    Completed = 3,
    Canceled = 4,
    OtherError = 5,
    ErrorLowBattery = 6,
    ErrorWiFiRecommended = 7,
    ErrorWiFiRequired = 8
  );
  PStorePackageUpdateState = ^StorePackageUpdateState;

  // Windows.Services.Store.StoreConsumableStatus
  StoreConsumableStatus = (
    Succeeded = 0,
    InsufficentQuantity = 1,
    NetworkError = 2,
    ServerError = 3
  );
  PStoreConsumableStatus = ^StoreConsumableStatus;

  // Windows.Services.Store.StoreDurationUnit
  StoreDurationUnit = (
    Minute = 0,
    Hour = 1,
    Day = 2,
    Week = 3,
    Month = 4,
    Year = 5
  );
  PStoreDurationUnit = ^StoreDurationUnit;

  // Windows.Services.Store.StorePurchaseStatus
  StorePurchaseStatus = (
    Succeeded = 0,
    AlreadyPurchased = 1,
    NotPurchased = 2,
    NetworkError = 3,
    ServerError = 4
  );
  PStorePurchaseStatus = ^StorePurchaseStatus;

  // Windows.System.UserPictureSize
  UserPictureSize = (
    Size64x64 = 0,
    Size208x208 = 1,
    Size424x424 = 2,
    Size1080x1080 = 3
  );
  PUserPictureSize = ^UserPictureSize;

  // Windows.System.UserType
  UserType = (
    LocalUser = 0,
    RemoteUser = 1,
    LocalGuest = 2,
    RemoteGuest = 3
  );
  PUserType = ^UserType;

  // Windows.System.UserAuthenticationStatus
  UserAuthenticationStatus = (
    Unauthenticated = 0,
    LocallyAuthenticated = 1,
    RemotelyAuthenticated = 2
  );
  PUserAuthenticationStatus = ^UserAuthenticationStatus;

  // Windows.Storage.StorageItemTypes
  StorageItemTypes = (
    None = 0,
    &File = 1,
    Folder = 2
  );
  PStorageItemTypes = ^StorageItemTypes;

  // Windows.Storage.FileAttributes
  FileAttributes = (
    Normal = 0,
    ReadOnly = 1,
    Directory = 16,
    Archive = 32,
    Temporary = 256,
    LocallyIncomplete = 512
  );
  PFileAttributes = ^FileAttributes;

  // Windows.Storage.StorageDeleteOption
  StorageDeleteOption = (
    Default = 0,
    PermanentDelete = 1
  );
  PStorageDeleteOption = ^StorageDeleteOption;

  // Windows.Storage.CreationCollisionOption
  CreationCollisionOption = (
    GenerateUniqueName = 0,
    ReplaceExisting = 1,
    FailIfExists = 2,
    OpenIfExists = 3
  );
  PCreationCollisionOption = ^CreationCollisionOption;

  // Windows.Storage.NameCollisionOption
  NameCollisionOption = (
    GenerateUniqueName = 0,
    ReplaceExisting = 1,
    FailIfExists = 2
  );
  PNameCollisionOption = ^NameCollisionOption;

  // Windows.Storage.FileAccessMode
  FileAccessMode = (
    Read = 0,
    ReadWrite = 1
  );
  PFileAccessMode = ^FileAccessMode;

  // Windows.Storage.Streams.InputStreamOptions
  InputStreamOptions = (
    None = 0,
    Partial = 1,
    ReadAhead = 2
  );
  PInputStreamOptions = ^InputStreamOptions;

  // Windows.System.ProcessorArchitecture
  ProcessorArchitecture = (
    X86 = 0,
    Arm = 5,
    X64 = 9,
    Neutral = 11,
    Arm64 = 12,
    X86OnArm64 = 14,
    Unknown = 65535
  );
  PProcessorArchitecture = ^ProcessorArchitecture;

  // Windows.Foundation.Collections.CollectionChange
  CollectionChange = (
    Reset = 0,
    ItemInserted = 1,
    ItemRemoved = 2,
    ItemChanged = 3
  );
  PCollectionChange = ^CollectionChange;

  // Windows.Foundation.AsyncStatus
  AsyncStatus = (
    Canceled = 2,
    Completed = 1,
    Error = 3,
    Started = 0
  );
  PAsyncStatus = ^AsyncStatus;

  //  Records
  // Windows.Graphics.Imaging.BitmapPlaneDescription
  Imaging_BitmapPlaneDescription = record
    StartIndex: Integer;
    Width: Integer;
    Height: Integer;
    Stride: Integer;
  end;
  PImaging_BitmapPlaneDescription = ^Imaging_BitmapPlaneDescription;

  // Windows.UI.Xaml.Controls.Maps.MapZoomLevelRange
  Maps_MapZoomLevelRange = record
    Min: Double;
    Max: Double;
  end;
  PMaps_MapZoomLevelRange = ^Maps_MapZoomLevelRange;

  // Windows.Devices.Geolocation.BasicGeoposition
  BasicGeoposition = record
    Latitude: Double;
    Longitude: Double;
    Altitude: Double;
  end;
  PBasicGeoposition = ^BasicGeoposition;

  // Windows.UI.Xaml.Documents.TextRange
  Documents_TextRange = record
    StartIndex: Integer;
    Length: Integer;
  end;
  PDocuments_TextRange = ^Documents_TextRange;

  // Windows.UI.Xaml.GridLength
  GridLength = record
    Value: Double;
    GridUnitType: GridUnitType;
  end;
  PGridLength = ^GridLength;

  // Windows.UI.Xaml.CornerRadius
  CornerRadius = record
    TopLeft: Double;
    TopRight: Double;
    BottomRight: Double;
    BottomLeft: Double;
  end;
  PCornerRadius = ^CornerRadius;

  // Windows.UI.Input.ManipulationVelocities
  ManipulationVelocities = record
    Linear: TPointF;
    Angular: Single;
    Expansion: Single;
  end;
  PManipulationVelocities = ^ManipulationVelocities;

  // Windows.UI.Input.ManipulationDelta
  ManipulationDelta = record
    Translation: TPointF;
    Scale: Single;
    Rotation: Single;
    Expansion: Single;
  end;
  PManipulationDelta = ^ManipulationDelta;

  // Windows.UI.Xaml.Data.LoadMoreItemsResult
  Data_LoadMoreItemsResult = record
    Count: Cardinal;
  end;
  PData_LoadMoreItemsResult = ^Data_LoadMoreItemsResult;

  // Windows.UI.Xaml.Thickness
  Thickness = record
    Left: Double;
    Top: Double;
    Right: Double;
    Bottom: Double;
  end;
  PThickness = ^Thickness;

  // Windows.UI.Xaml.Interop.TypeName
  Interop_TypeName = record
    Name: HSTRING;
    Kind: Interop_TypeKind;
  end;
  PInterop_TypeName = ^Interop_TypeName;

  // Windows.Graphics.RectInt32
  RectInt32 = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;
  PRectInt32 = ^RectInt32;

  // Windows.Graphics.PointInt32
  PointInt32 = record
    X: Integer;
    Y: Integer;
  end;
  PPointInt32 = ^PointInt32;

  // Windows.Graphics.SizeInt32
  SizeInt32 = record
    Width: Integer;
    Height: Integer;
  end;
  PSizeInt32 = ^SizeInt32;

  // Windows.Foundation.Numerics.Vector4
  Numerics_Vector4 = record
    X: Single;
    Y: Single;
    Z: Single;
    W: Single;
  end;
  PNumerics_Vector4 = ^Numerics_Vector4;

  // Windows.Foundation.Numerics.Matrix3x2
  Numerics_Matrix3x2 = record
    M11: Single;
    M12: Single;
    M21: Single;
    M22: Single;
    M31: Single;
    M32: Single;
  end;
  PNumerics_Matrix3x2 = ^Numerics_Matrix3x2;

  // Windows.Foundation.Numerics.Matrix4x4
  Numerics_Matrix4x4 = record
    M11: Single;
    M12: Single;
    M13: Single;
    M14: Single;
    M21: Single;
    M22: Single;
    M23: Single;
    M24: Single;
    M31: Single;
    M32: Single;
    M33: Single;
    M34: Single;
    M41: Single;
    M42: Single;
    M43: Single;
    M44: Single;
  end;
  PNumerics_Matrix4x4 = ^Numerics_Matrix4x4;

  // Windows.Foundation.Numerics.Quaternion
  Numerics_Quaternion = record
    X: Single;
    Y: Single;
    Z: Single;
    W: Single;
  end;
  PNumerics_Quaternion = ^Numerics_Quaternion;

  // Windows.Foundation.Numerics.Vector3
  Numerics_Vector3 = record
    X: Single;
    Y: Single;
    Z: Single;
  end;
  PNumerics_Vector3 = ^Numerics_Vector3;

  // Windows.Foundation.Numerics.Vector2
  Numerics_Vector2 = record
    X: Single;
    Y: Single;
  end;
  PNumerics_Vector2 = ^Numerics_Vector2;

  // Windows.Devices.Sms.SmsEncodedLength
  SmsEncodedLength = record
    SegmentCount: Cardinal;
    CharacterCountLastSegment: Cardinal;
    CharactersPerSegment: Cardinal;
    ByteCountLastSegment: Cardinal;
    BytesPerSegment: Cardinal;
  end;
  PSmsEncodedLength = ^SmsEncodedLength;

  // Windows.UI.Color
  Color = record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;
  PColor = ^Color;

  // Windows.UI.Core.CoreProximityEvaluation
  CoreProximityEvaluation = record
    Score: Integer;
    AdjustedPoint: TPointF;
  end;
  PCoreProximityEvaluation = ^CoreProximityEvaluation;

  // Windows.Devices.Input.PointerDeviceUsage
  Input_PointerDeviceUsage = record
    UsagePage: Cardinal;
    Usage: Cardinal;
    MinLogical: Integer;
    MaxLogical: Integer;
    MinPhysical: Integer;
    MaxPhysical: Integer;
    &Unit: Cardinal;
    PhysicalMultiplier: Single;
  end;
  PInput_PointerDeviceUsage = ^Input_PointerDeviceUsage;

  // Windows.UI.Core.CorePhysicalKeyStatus
  CorePhysicalKeyStatus = record
    RepeatCount: Cardinal;
    ScanCode: Cardinal;
    IsExtendedKey: Boolean;
    IsMenuKeyDown: Boolean;
    WasKeyDown: Boolean;
    IsKeyReleased: Boolean;
  end;
  PCorePhysicalKeyStatus = ^CorePhysicalKeyStatus;

  // Windows.Storage.Search.SortEntry
  Search_SortEntry = record
    PropertyName: HSTRING;
    AscendingOrder: Boolean;
  end;
  PSearch_SortEntry = ^Search_SortEntry;

  // Windows.Services.Store.StorePackageUpdateStatus
  StorePackageUpdateStatus = record
    PackageFamilyName: HSTRING;
    PackageDownloadSizeInBytes: UInt64;
    PackageBytesDownloaded: UInt64;
    PackageDownloadProgress: Double;
    TotalDownloadProgress: Double;
    PackageUpdateState: StorePackageUpdateState;
  end;
  PStorePackageUpdateStatus = ^StorePackageUpdateStatus;

  // Windows.ApplicationModel.PackageVersion
  PackageVersion = record
    Major: Word;
    Minor: Word;
    Build: Word;
    Revision: Word;
  end;
  PPackageVersion = ^PackageVersion;

  // Windows.Foundation.DateTime
  DateTime = record
    UniversalTime: Int64;
  end;
  PDateTime = ^DateTime;

  // Windows.Foundation.EventRegistrationToken
  EventRegistrationToken = record
    Value: Int64;
  end;
  PEventRegistrationToken = ^EventRegistrationToken;

  // Windows.Foundation.TimeSpan
  TimeSpan = record
    Duration: Int64;
  end;
  PTimeSpan = ^TimeSpan;

  // Windows.UI.Xaml.Duration
  Duration = record
    TimeSpan: TimeSpan;
    &Type: DurationType;
  end;
  PDuration = ^Duration;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IPropertySet>
  IAsyncOperation_1__IPropertySet_Base = interface(IInspectable)
  ['{490B0686-AFD7-5037-9647-D8FE248F182C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IPropertySet); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IPropertySet; safecall;
    function GetResults: IPropertySet; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IPropertySet read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IPropertySet>
  // External 
  IAsyncOperation_1__IPropertySet = interface(IAsyncOperation_1__IPropertySet_Base)
  ['{490B0686-AFD7-5037-9647-D8FE248F182C}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IPropertySet>
  AsyncOperationCompletedHandler_1__IPropertySet_Delegate_Base = interface(IUnknown)
  ['{5075A55F-68BA-56F2-97E6-9B1CBFA2C5F2}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IPropertySet; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IPropertySet>
  // External 
  AsyncOperationCompletedHandler_1__IPropertySet = interface(AsyncOperationCompletedHandler_1__IPropertySet_Delegate_Base)
  ['{5075A55F-68BA-56F2-97E6-9B1CBFA2C5F2}']
  end;

  // DualAPI Interface
  // Windows.Foundation.Collections.IPropertySet
  [WinRTClassNameAttribute(SWindows_Security_Credentials_PasswordCredentialPropertyStore)]
  IPropertySet = interface(IInspectable)
  ['{8A43ED9F-F4E6-4421-ACF9-1DAB2986820C}']
  end;

  // DualAPI Interface
  // Windows.Foundation.IDeferral
  [WinRTClassNameAttribute(SWindows_Foundation_Deferral)]
  IDeferral = interface(IInspectable)
  ['{D6269732-3B7F-46A7-B40B-4FDCA2A2C693}']
    procedure Complete; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean_Base = interface(IInspectable)
  ['{CDB5EFB3-5788-509D-9BE1-71CCB8A3362A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Boolean); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Boolean; safecall;
    function GetResults: Boolean; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Boolean read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Boolean>
  // External 
  IAsyncOperation_1__Boolean = interface(IAsyncOperation_1__Boolean_Base)
  ['{CDB5EFB3-5788-509D-9BE1-71CCB8A3362A}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean_Delegate_Base = interface(IUnknown)
  ['{C1D3D1A2-AE17-5A5F-B5A2-BDCC8844889A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Boolean; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  // External 
  AsyncOperationCompletedHandler_1__Boolean = interface(AsyncOperationCompletedHandler_1__Boolean_Delegate_Base)
  ['{C1D3D1A2-AE17-5A5F-B5A2-BDCC8844889A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  IAsyncOperationWithProgress_2__Cardinal__Cardinal_Base = interface(IInspectable)
  ['{ECCB574A-C684-5572-A679-6B0842CFB57F}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__Cardinal__Cardinal); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__Cardinal__Cardinal; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal; safecall;
    function GetResults: Cardinal; safecall;
    property Progress: AsyncOperationProgressHandler_2__Cardinal__Cardinal read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  // External 
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = interface(IAsyncOperationWithProgress_2__Cardinal__Cardinal_Base)
  ['{ECCB574A-C684-5572-A679-6B0842CFB57F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal_Delegate_Base = interface(IUnknown)
  ['{EA0FE405-D432-5AC7-9EF8-5A65E1F97D7E}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; progressInfo: Cardinal); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  // External 
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface(AsyncOperationProgressHandler_2__Cardinal__Cardinal_Delegate_Base)
  ['{EA0FE405-D432-5AC7-9EF8-5A65E1F97D7E}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal_Delegate_Base = interface(IUnknown)
  ['{1E466DC5-840F-54F9-B877-5E3A9F4B6C74}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface(AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal_Delegate_Base)
  ['{1E466DC5-840F-54F9-B877-5E3A9F4B6C74}']
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Object>
  // External 
  IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{09335560-6C6B-5A26-9348-97B781132B20}']
    function get_Key: HSTRING; safecall;
    function get_Value: IInspectable; safecall;
    property Key: HSTRING read get_Key;
    property Value: IInspectable read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{5DB5FA32-707C-5849-A06B-91C8EB9D10E8}']
    function get_Current: IKeyValuePair_2__HSTRING__IInspectable; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IInspectable): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IInspectable read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  // External 
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable = interface(IIterator_1__IKeyValuePair_2__HSTRING__IInspectable_Base)
  ['{5DB5FA32-707C-5849-A06B-91C8EB9D10E8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Object>
  IMapView_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{BB78502A-F79D-54FA-92C9-90C5039FDF7E}']
    function Lookup(key: HSTRING): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IInspectable; out second: IMapView_2__HSTRING__IInspectable); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,Object>
  // External 
  IMapView_2__HSTRING__IInspectable = interface(IMapView_2__HSTRING__IInspectable_Base)
  ['{BB78502A-F79D-54FA-92C9-90C5039FDF7E}']
  end;

  // Windows.Foundation.Collections.IObservableMap`2<String,Object>
  // External 
  IObservableMap_2__HSTRING__IInspectable = interface(IInspectable)
  ['{236AAC9D-FB12-5C4D-A41C-9E445FB4D7EC}']
    function add_MapChanged(vhnd: MapChangedEventHandler_2__HSTRING__IInspectable): EventRegistrationToken; safecall;
    procedure remove_MapChanged(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable_Delegate_Base = interface(IUnknown)
  ['{24F981E5-DDCA-538D-AADA-A59906084CF1}']
    procedure Invoke(sender: IObservableMap_2__HSTRING__IInspectable; event: IMapChangedEventArgs_1__HSTRING); safecall;
  end;
  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  // External 
  MapChangedEventHandler_2__HSTRING__IInspectable = interface(MapChangedEventHandler_2__HSTRING__IInspectable_Delegate_Base)
  ['{24F981E5-DDCA-538D-AADA-A59906084CF1}']
  end;

  // Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  // External 
  IMapChangedEventArgs_1__HSTRING = interface(IInspectable)
  ['{60141EFB-F2F9-5377-96FD-F8C60D9558B5}']
    function get_CollectionChange: CollectionChange; safecall;
    function get_Key: HSTRING; safecall;
    property CollectionChange_: CollectionChange read get_CollectionChange;
    property Key: HSTRING read get_Key;
  end;

  // Windows.Foundation.IAsyncAction
  // External 
  IAsyncAction = interface(IInspectable)
  ['{5A648006-843A-4DA9-865B-9D26E5DFAD7B}']
    procedure put_Completed(handler: AsyncActionCompletedHandler); safecall;
    function get_Completed: AsyncActionCompletedHandler; safecall;
    procedure GetResults; safecall;
    property Completed: AsyncActionCompletedHandler read get_Completed write put_Completed;
  end;

  // Windows.Foundation.AsyncActionCompletedHandler
  // External 
  AsyncActionCompletedHandler = interface(IUnknown)
  ['{A4ED5C81-76C9-40BD-8BE6-B1D90FB20AE7}']
    procedure Invoke(asyncInfo: IAsyncAction; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IReference`1<Windows.Foundation.Point>
  // External 
  IReference_1__Point = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: TPointF; safecall;
    property Value: TPointF read get_Value;
  end;

  // Windows.Foundation.IReference`1<Windows.Foundation.Rect>
  // External 
  IReference_1__Rect = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: TRectF; safecall;
    property Value: TRectF read get_Value;
  end;

  // DualAPI Interface
  // Windows.Foundation.IUriRuntimeClass
  [WinRTClassNameAttribute(SWindows_Foundation_Uri)]
  IUriRuntimeClass = interface(IInspectable)
  ['{9E365E57-48B2-4160-956F-C7385120BBFC}']
    function get_AbsoluteUri: HSTRING; safecall;
    function get_DisplayUri: HSTRING; safecall;
    function get_Domain: HSTRING; safecall;
    function get_Extension: HSTRING; safecall;
    function get_Fragment: HSTRING; safecall;
    function get_Host: HSTRING; safecall;
    function get_Password: HSTRING; safecall;
    function get_Path: HSTRING; safecall;
    function get_Query: HSTRING; safecall;
    function get_QueryParsed: IWwwFormUrlDecoderRuntimeClass; safecall;
    function get_RawUri: HSTRING; safecall;
    function get_SchemeName: HSTRING; safecall;
    function get_UserName: HSTRING; safecall;
    function get_Port: Integer; safecall;
    function get_Suspicious: Boolean; safecall;
    function Equals(pUri: IUriRuntimeClass): Boolean; safecall;
    function CombineUri(relativeUri: HSTRING): IUriRuntimeClass; safecall;
    property AbsoluteUri: HSTRING read get_AbsoluteUri;
    property DisplayUri: HSTRING read get_DisplayUri;
    property Domain: HSTRING read get_Domain;
    property Extension: HSTRING read get_Extension;
    property Fragment: HSTRING read get_Fragment;
    property Host: HSTRING read get_Host;
    property Password: HSTRING read get_Password;
    property Path: HSTRING read get_Path;
    property Port: Integer read get_Port;
    property Query: HSTRING read get_Query;
    property QueryParsed: IWwwFormUrlDecoderRuntimeClass read get_QueryParsed;
    property RawUri: HSTRING read get_RawUri;
    property SchemeName: HSTRING read get_SchemeName;
    property Suspicious: Boolean read get_Suspicious;
    property UserName: HSTRING read get_UserName;
  end;

  // DualAPI Interface
  // Windows.Foundation.IWwwFormUrlDecoderRuntimeClass
  [WinRTClassNameAttribute(SWindows_Foundation_WwwFormUrlDecoder)]
  IWwwFormUrlDecoderRuntimeClass = interface(IInspectable)
  ['{D45A0451-F225-4542-9296-0E1DF5D254DF}']
    function GetFirstValueByName(name: HSTRING): HSTRING; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IUriRuntimeClass>
  IAsyncOperation_1__IUriRuntimeClass_Base = interface(IInspectable)
  ['{641CB9DD-A28D-59E2-B8DB-A227EDA6CF2E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IUriRuntimeClass); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IUriRuntimeClass; safecall;
    function GetResults: IUriRuntimeClass; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IUriRuntimeClass read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IUriRuntimeClass>
  // External 
  IAsyncOperation_1__IUriRuntimeClass = interface(IAsyncOperation_1__IUriRuntimeClass_Base)
  ['{45BD84D5-E60D-5525-BC8F-B568B5538D2D}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IUriRuntimeClass>
  AsyncOperationCompletedHandler_1__IUriRuntimeClass_Delegate_Base = interface(IUnknown)
  ['{AD46F1CC-2BB0-585C-9885-03C2780D4D58}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IUriRuntimeClass; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IUriRuntimeClass>
  // External 
  AsyncOperationCompletedHandler_1__IUriRuntimeClass = interface(AsyncOperationCompletedHandler_1__IUriRuntimeClass_Delegate_Base)
  ['{A25BFF34-DC7C-5FBA-9850-5D6996615BF5}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStorePackageLicense,Object>
  TypedEventHandler_2__IStorePackageLicense__IInspectable_Delegate_Base = interface(IUnknown)
  ['{6C59D637-2970-5F64-9511-D39AC245BC94}']
    procedure Invoke(sender: IStorePackageLicense; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStorePackageLicense,Object>
  // External 
  TypedEventHandler_2__IStorePackageLicense__IInspectable = interface(TypedEventHandler_2__IStorePackageLicense__IInspectable_Delegate_Base)
  ['{A60D9EA4-F3BA-5685-8F84-04DAC0F5300B}']
  end;

  // Windows.Services.Store.IStorePackageLicense
  // External 
  IStorePackageLicense = interface(IInspectable)
  ['{0C465714-14E1-4973-BD14-F77724271E99}']
    function add_LicenseLost(handler: TypedEventHandler_2__IStorePackageLicense__IInspectable): EventRegistrationToken; safecall;
    procedure remove_LicenseLost(token: EventRegistrationToken); safecall;
    function get_Package: IPackage; safecall;
    function get_IsValid: Boolean; safecall;
    procedure ReleaseLicense; safecall;
    property IsValid: Boolean read get_IsValid;
    property Package: IPackage read get_Package;
  end;

  // DualAPI Interface
  // Windows.ApplicationModel.IPackage
  [WinRTClassNameAttribute(SWindows_ApplicationModel_PackageRT)]
  IPackage = interface(IInspectable)
  ['{163C792F-BD75-413C-BF23-B1FE7B95D825}']
    function get_Id: IPackageId; safecall;
    function get_InstalledLocation: IStorageFolder; safecall;
    function get_IsFramework: Boolean; safecall;
    function get_Dependencies: IVectorView_1__IPackage; safecall;
    property Dependencies: IVectorView_1__IPackage read get_Dependencies;
    property Id: IPackageId read get_Id;
    property InstalledLocation: IStorageFolder read get_InstalledLocation;
    property IsFramework: Boolean read get_IsFramework;
  end;

  // Windows.ApplicationModel.IPackageId
  // External 
  IPackageId = interface(IInspectable)
  ['{1ADB665E-37C7-4790-9980-DD7AE74E8BB2}']
    function get_Name: HSTRING; safecall;
    function get_Version: PackageVersion; safecall;
    function get_Architecture: ProcessorArchitecture; safecall;
    function get_ResourceId: HSTRING; safecall;
    function get_Publisher: HSTRING; safecall;
    function get_PublisherId: HSTRING; safecall;
    function get_FullName: HSTRING; safecall;
    function get_FamilyName: HSTRING; safecall;
    property Architecture: ProcessorArchitecture read get_Architecture;
    property FamilyName: HSTRING read get_FamilyName;
    property FullName: HSTRING read get_FullName;
    property Name: HSTRING read get_Name;
    property Publisher: HSTRING read get_Publisher;
    property PublisherId: HSTRING read get_PublisherId;
    property ResourceId: HSTRING read get_ResourceId;
    property Version: PackageVersion read get_Version;
  end;

  // DualAPI Interface
  // Windows.Storage.IStorageFolder
  [WinRTClassNameAttribute(SWindows_Storage_StorageFolder)]
  IStorageFolder = interface(IInspectable)
  ['{72D1CB78-B3EF-4F75-A80B-6FD9DAE2944B}']
    function CreateFileAsync(desiredName: HSTRING): IAsyncOperation_1__IStorageFile; overload; safecall;
    function CreateFileAsync(desiredName: HSTRING; options: CreationCollisionOption): IAsyncOperation_1__IStorageFile; overload; safecall;
    function CreateFolderAsync(desiredName: HSTRING): IAsyncOperation_1__IStorageFolder; overload; safecall;
    function CreateFolderAsync(desiredName: HSTRING; options: CreationCollisionOption): IAsyncOperation_1__IStorageFolder; overload; safecall;
    function GetFileAsync(name: HSTRING): IAsyncOperation_1__IStorageFile; safecall;
    function GetFolderAsync(name: HSTRING): IAsyncOperation_1__IStorageFolder; safecall;
    function GetItemAsync(name: HSTRING): IAsyncOperation_1__IStorageItem; safecall;
    function GetFilesAsync: IAsyncOperation_1__IVectorView_1__IStorageFile; safecall;
    function GetFoldersAsync: IAsyncOperation_1__IVectorView_1__IStorageFolder; safecall;
    function GetItemsAsync: IAsyncOperation_1__IVectorView_1__IStorageItem; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFile>
  IAsyncOperation_1__IStorageFile_Base = interface(IInspectable)
  ['{5E52F8CE-ACED-5A42-95B4-F674DD84885E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStorageFile); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStorageFile; safecall;
    function GetResults: IStorageFile; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStorageFile read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFile>
  // External 
  IAsyncOperation_1__IStorageFile = interface(IAsyncOperation_1__IStorageFile_Base)
  ['{31C5C3AB-4BF6-51D1-B590-C6EFC00E9FF2}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageFile>
  AsyncOperationCompletedHandler_1__IStorageFile_Delegate_Base = interface(IUnknown)
  ['{E521C894-2C26-5946-9E61-2B5E188D01ED}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStorageFile; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageFile>
  // External 
  AsyncOperationCompletedHandler_1__IStorageFile = interface(AsyncOperationCompletedHandler_1__IStorageFile_Delegate_Base)
  ['{1247300D-7973-53D5-889F-5279D9322114}']
  end;

  // DualAPI Interface
  // Windows.Storage.IStorageFile
  [WinRTClassNameAttribute(SWindows_Storage_StorageFile)]
  IStorageFile = interface(IInspectable)
  ['{FA3F6186-4214-428C-A64C-14C9AC7315EA}']
    function get_FileType: HSTRING; safecall;
    function get_ContentType: HSTRING; safecall;
    function OpenAsync(accessMode: FileAccessMode): IAsyncOperation_1__IRandomAccessStream; safecall;
    function OpenTransactedWriteAsync: IAsyncOperation_1__IStorageStreamTransaction; safecall;
    function CopyAsync(destinationFolder: IStorageFolder): IAsyncOperation_1__IStorageFile; overload; safecall;
    function CopyAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING): IAsyncOperation_1__IStorageFile; overload; safecall;
    function CopyAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING; option: NameCollisionOption): IAsyncOperation_1__IStorageFile; overload; safecall;
    function CopyAndReplaceAsync(fileToReplace: IStorageFile): IAsyncAction; safecall;
    function MoveAsync(destinationFolder: IStorageFolder): IAsyncAction; overload; safecall;
    function MoveAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING): IAsyncAction; overload; safecall;
    function MoveAsync(destinationFolder: IStorageFolder; desiredNewName: HSTRING; option: NameCollisionOption): IAsyncAction; overload; safecall;
    function MoveAndReplaceAsync(fileToReplace: IStorageFile): IAsyncAction; safecall;
    property ContentType: HSTRING read get_ContentType;
    property FileType: HSTRING read get_FileType;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStream>
  IAsyncOperation_1__IRandomAccessStream_Base = interface(IInspectable)
  ['{430ECECE-1418-5D19-81B2-5DDB381603CC}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IRandomAccessStream); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IRandomAccessStream; safecall;
    function GetResults: IRandomAccessStream; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IRandomAccessStream read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStream>
  // External 
  IAsyncOperation_1__IRandomAccessStream = interface(IAsyncOperation_1__IRandomAccessStream_Base)
  ['{430ECECE-1418-5D19-81B2-5DDB381603CC}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStream>
  AsyncOperationCompletedHandler_1__IRandomAccessStream_Delegate_Base = interface(IUnknown)
  ['{398C4183-793D-5B00-819B-4AEF92485E94}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IRandomAccessStream; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStream>
  // External 
  AsyncOperationCompletedHandler_1__IRandomAccessStream = interface(AsyncOperationCompletedHandler_1__IRandomAccessStream_Delegate_Base)
  ['{398C4183-793D-5B00-819B-4AEF92485E94}']
  end;

  // Windows.Storage.Streams.IRandomAccessStream
  // External 
  IRandomAccessStream = interface(IInspectable)
  ['{905A0FE1-BC53-11DF-8C49-001E4FC686DA}']
    function get_Size: UInt64; safecall;
    procedure put_Size(value: UInt64); safecall;
    function GetInputStreamAt(position: UInt64): IInputStream; safecall;
    function GetOutputStreamAt(position: UInt64): IOutputStream; safecall;
    function get_Position: UInt64; safecall;
    procedure Seek(position: UInt64); safecall;
    function CloneStream: IRandomAccessStream; safecall;
    function get_CanRead: Boolean; safecall;
    function get_CanWrite: Boolean; safecall;
    property CanRead: Boolean read get_CanRead;
    property CanWrite: Boolean read get_CanWrite;
    property Position: UInt64 read get_Position;
    property Size: UInt64 read get_Size write put_Size;
  end;

  // DualAPI Interface
  // Windows.Storage.Streams.IInputStream
  // External 
  IInputStream = interface(IInspectable)
  ['{905A0FE2-BC53-11DF-8C49-001E4FC686DA}']
    function ReadAsync(buffer: IBuffer; count: Cardinal; options: InputStreamOptions): IAsyncOperationWithProgress_2__IBuffer__Cardinal; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal_Base = interface(IInspectable)
  ['{D26B2819-897F-5C7D-84D6-56D796561431}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IBuffer__Cardinal); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IBuffer__Cardinal; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal; safecall;
    function GetResults: IBuffer; safecall;
    property Progress: AsyncOperationProgressHandler_2__IBuffer__Cardinal read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  // External 
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface(IAsyncOperationWithProgress_2__IBuffer__Cardinal_Base)
  ['{D26B2819-897F-5C7D-84D6-56D796561431}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal_Delegate_Base = interface(IUnknown)
  ['{BF666554-7605-5D9A-B14E-18D8C8472AFE}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; progressInfo: Cardinal); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  // External 
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface(AsyncOperationProgressHandler_2__IBuffer__Cardinal_Delegate_Base)
  ['{BF666554-7605-5D9A-B14E-18D8C8472AFE}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal_Delegate_Base = interface(IUnknown)
  ['{06386A7A-E009-5B0B-AB68-A8E48B516647}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface(AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal_Delegate_Base)
  ['{06386A7A-E009-5B0B-AB68-A8E48B516647}']
  end;

  // DualAPI Interface
  // Windows.Storage.Streams.IBuffer
  [WinRTClassNameAttribute(SWindows_Storage_Streams_Buffer)]
  IBuffer = interface(IInspectable)
  ['{905A0FE0-BC53-11DF-8C49-001E4FC686DA}']
    function get_Capacity: Cardinal; safecall;
    function get_Length: Cardinal; safecall;
    procedure put_Length(value: Cardinal); safecall;
    property Capacity: Cardinal read get_Capacity;
    property Length: Cardinal read get_Length write put_Length;
  end;

  // DualAPI Interface
  // Windows.Storage.Streams.IOutputStream
  // External 
  IOutputStream = interface(IInspectable)
  ['{905A0FE6-BC53-11DF-8C49-001E4FC686DA}']
    function WriteAsync(buffer: IBuffer): IAsyncOperationWithProgress_2__Cardinal__Cardinal; safecall;
    function FlushAsync: IAsyncOperation_1__Boolean; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageStreamTransaction>
  IAsyncOperation_1__IStorageStreamTransaction_Base = interface(IInspectable)
  ['{0D81405A-9BD3-5E87-82F4-9B4128A887EB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStorageStreamTransaction); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStorageStreamTransaction; safecall;
    function GetResults: IStorageStreamTransaction; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStorageStreamTransaction read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageStreamTransaction>
  // External 
  IAsyncOperation_1__IStorageStreamTransaction = interface(IAsyncOperation_1__IStorageStreamTransaction_Base)
  ['{C5926392-4ECC-526D-BBC1-725AE3F7C449}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageStreamTransaction>
  AsyncOperationCompletedHandler_1__IStorageStreamTransaction_Delegate_Base = interface(IUnknown)
  ['{D11739E6-2995-5D33-BFFF-51B6041F68C1}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStorageStreamTransaction; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageStreamTransaction>
  // External 
  AsyncOperationCompletedHandler_1__IStorageStreamTransaction = interface(AsyncOperationCompletedHandler_1__IStorageStreamTransaction_Delegate_Base)
  ['{4F03213E-DFDA-5E67-98AC-0D139B467B19}']
  end;

  // Windows.Storage.IStorageStreamTransaction
  // External 
  IStorageStreamTransaction = interface(IInspectable)
  ['{F67CF363-A53D-4D94-AE2C-67232D93ACDD}']
    function get_Stream: IRandomAccessStream; safecall;
    function CommitAsync: IAsyncAction; safecall;
    property Stream: IRandomAccessStream read get_Stream;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFolder>
  IAsyncOperation_1__IStorageFolder_Base = interface(IInspectable)
  ['{6BE9E7D7-E83A-5CBC-802C-1768960B52C3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStorageFolder); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStorageFolder; safecall;
    function GetResults: IStorageFolder; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStorageFolder read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageFolder>
  // External 
  IAsyncOperation_1__IStorageFolder = interface(IAsyncOperation_1__IStorageFolder_Base)
  ['{68EA25EA-D88C-5DB0-A24A-BBCBDFCD3ECC}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageFolder>
  AsyncOperationCompletedHandler_1__IStorageFolder_Delegate_Base = interface(IUnknown)
  ['{C211026E-9E63-5452-BA54-3A07D6A96874}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStorageFolder; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageFolder>
  // External 
  AsyncOperationCompletedHandler_1__IStorageFolder = interface(AsyncOperationCompletedHandler_1__IStorageFolder_Delegate_Base)
  ['{8254DF53-F8CD-570A-AEB9-F2C0AD5368AC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageItem>
  IAsyncOperation_1__IStorageItem_Base = interface(IInspectable)
  ['{5FC9C137-EBB7-5E6C-9CBA-686F2EC2B0BB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStorageItem); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStorageItem; safecall;
    function GetResults: IStorageItem; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStorageItem read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IStorageItem>
  // External 
  IAsyncOperation_1__IStorageItem = interface(IAsyncOperation_1__IStorageItem_Base)
  ['{5FC9C137-EBB7-5E6C-9CBA-686F2EC2B0BB}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageItem>
  AsyncOperationCompletedHandler_1__IStorageItem_Delegate_Base = interface(IUnknown)
  ['{92C3102F-A327-5318-A6C1-76F6B2A0ABFB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStorageItem; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IStorageItem>
  // External 
  AsyncOperationCompletedHandler_1__IStorageItem = interface(AsyncOperationCompletedHandler_1__IStorageItem_Delegate_Base)
  ['{92C3102F-A327-5318-A6C1-76F6B2A0ABFB}']
  end;

  // DualAPI Interface
  // Windows.Storage.IStorageItem
  // External 
  IStorageItem = interface(IInspectable)
  ['{4207A996-CA2F-42F7-BDE8-8B10457A7F30}']
    function RenameAsync(desiredName: HSTRING): IAsyncAction; overload; safecall;
    function RenameAsync(desiredName: HSTRING; option: NameCollisionOption): IAsyncAction; overload; safecall;
    function DeleteAsync: IAsyncAction; overload; safecall;
    function DeleteAsync(option: StorageDeleteOption): IAsyncAction; overload; safecall;
    function GetBasicPropertiesAsync: IAsyncOperation_1__FileProperties_IBasicProperties; safecall;
    function get_Name: HSTRING; safecall;
    function get_Path: HSTRING; safecall;
    function get_Attributes: FileAttributes; safecall;
    function get_DateCreated: DateTime; safecall;
    function IsOfType(&type: StorageItemTypes): Boolean; safecall;
    property Attributes: FileAttributes read get_Attributes;
    property DateCreated: DateTime read get_DateCreated;
    property Name: HSTRING read get_Name;
    property Path: HSTRING read get_Path;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IBasicProperties>
  IAsyncOperation_1__FileProperties_IBasicProperties_Base = interface(IInspectable)
  ['{5186131A-4467-504B-977A-0785A8230485}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties; safecall;
    function GetResults: FileProperties_IBasicProperties; safecall;
    property Completed: AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IBasicProperties>
  // External 
  IAsyncOperation_1__FileProperties_IBasicProperties = interface(IAsyncOperation_1__FileProperties_IBasicProperties_Base)
  ['{ADA07FD2-C777-5490-BD12-C0E6D722EAD7}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IBasicProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties_Delegate_Base = interface(IUnknown)
  ['{C8659AAE-4926-52AD-8F60-D89FE5A8DF5F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__FileProperties_IBasicProperties; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IBasicProperties>
  // External 
  AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties = interface(AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties_Delegate_Base)
  ['{129B0F62-E176-5FB1-A3B9-69FEEE1BFB29}']
  end;

  // Windows.Storage.FileProperties.IBasicProperties
  // External 
  FileProperties_IBasicProperties = interface(IInspectable)
  ['{D05D55DB-785E-4A66-BE02-9BEEC58AEA81}']
    function get_Size: UInt64; safecall;
    function get_DateModified: DateTime; safecall;
    function get_ItemDate: DateTime; safecall;
    property DateModified: DateTime read get_DateModified;
    property ItemDate: DateTime read get_ItemDate;
    property Size: UInt64 read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>>
  IAsyncOperation_1__IVectorView_1__IStorageFile_Base = interface(IInspectable)
  ['{03362E33-E413-5F29-97D0-48A4780935F9}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile; safecall;
    function GetResults: IVectorView_1__IStorageFile; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>>
  // External 
  IAsyncOperation_1__IVectorView_1__IStorageFile = interface(IAsyncOperation_1__IVectorView_1__IStorageFile_Base)
  ['{9751F304-CBA2-5A44-890D-0DBD9F173C43}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile_Delegate_Base = interface(IUnknown)
  ['{CB4206C5-0988-5104-AFA9-253C298F86FD}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IStorageFile; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>>
  // External 
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile_Delegate_Base)
  ['{3B203E3A-6A0E-5FA3-A300-EAF350152625}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFile>
  // External 
  IVectorView_1__IStorageFile = interface(IInspectable)
  ['{4296B6E8-E219-5E74-8DDE-196F57200A82}']
    function GetAt(index: Cardinal): IStorageFile; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStorageFile; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorageFile): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>>
  IAsyncOperation_1__IVectorView_1__IStorageFolder_Base = interface(IInspectable)
  ['{CA40B21B-AEB1-5A61-9E08-3BD5D9594023}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder; safecall;
    function GetResults: IVectorView_1__IStorageFolder; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>>
  // External 
  IAsyncOperation_1__IVectorView_1__IStorageFolder = interface(IAsyncOperation_1__IVectorView_1__IStorageFolder_Base)
  ['{A4D504A3-0D7B-5497-B35F-0889A2AF16FD}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder_Delegate_Base = interface(IUnknown)
  ['{ED2D1D9B-26EC-5BE7-A8A3-56458933D25F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IStorageFolder; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>>
  // External 
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder_Delegate_Base)
  ['{AEF07EE0-9203-543D-90C9-088DE0DF94E4}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageFolder>
  // External 
  IVectorView_1__IStorageFolder = interface(IInspectable)
  ['{6A8C9209-6392-5B08-AEE9-F2ADD7DECC49}']
    function GetAt(index: Cardinal): IStorageFolder; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStorageFolder; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorageFolder): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>>
  IAsyncOperation_1__IVectorView_1__IStorageItem_Base = interface(IInspectable)
  ['{4B1C0FD7-7A01-5E7A-A6FE-BE4500283F23}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem; safecall;
    function GetResults: IVectorView_1__IStorageItem; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>>
  // External 
  IAsyncOperation_1__IVectorView_1__IStorageItem = interface(IAsyncOperation_1__IVectorView_1__IStorageItem_Base)
  ['{4B1C0FD7-7A01-5E7A-A6FE-BE4500283F23}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem_Delegate_Base = interface(IUnknown)
  ['{51436E75-ACE1-5A68-B260-F843B846F0DB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IStorageItem; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>>
  // External 
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem_Delegate_Base)
  ['{51436E75-ACE1-5A68-B260-F843B846F0DB}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageItem>
  // External 
  IVectorView_1__IStorageItem = interface(IInspectable)
  ['{85575A41-06CB-58D0-B98A-7C8F06E6E9D7}']
    function GetAt(index: Cardinal): IStorageItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStorageItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorageItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.IPackage>
  // External 
  IVectorView_1__IPackage = interface(IInspectable)
  ['{77191FA5-6652-5BC7-82EB-CDDF0E2F7ECD}']
    function GetAt(index: Cardinal): IPackage; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IPackage; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPackage): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStoreContext,Object>
  TypedEventHandler_2__IStoreContext__IInspectable_Delegate_Base = interface(IUnknown)
  ['{D5A00AC7-082D-547C-A04B-2540C1CDE97A}']
    procedure Invoke(sender: IStoreContext; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStoreContext,Object>
  // External 
  TypedEventHandler_2__IStoreContext__IInspectable = interface(TypedEventHandler_2__IStoreContext__IInspectable_Delegate_Base)
  ['{B1B19242-A891-5BEA-AD67-2F755CE676BA}']
  end;

  // DualAPI Interface
  // Windows.Services.Store.IStoreContext
  [WinRTClassNameAttribute(SWindows_Services_Store_StoreContext)]
  IStoreContext = interface(IInspectable)
  ['{AC98B6BE-F4FD-4912-BABD-5035E5E8BCAB}']
    function get_User: IUser; safecall;
    function add_OfflineLicensesChanged(handler: TypedEventHandler_2__IStoreContext__IInspectable): EventRegistrationToken; safecall;
    procedure remove_OfflineLicensesChanged(token: EventRegistrationToken); safecall;
    function GetCustomerPurchaseIdAsync(serviceTicket: HSTRING; publisherUserId: HSTRING): IAsyncOperation_1__HSTRING; safecall;
    function GetCustomerCollectionsIdAsync(serviceTicket: HSTRING; publisherUserId: HSTRING): IAsyncOperation_1__HSTRING; safecall;
    function GetAppLicenseAsync: IAsyncOperation_1__IStoreAppLicense; safecall;
    function GetStoreProductForCurrentAppAsync: IAsyncOperation_1__IStoreProductResult; safecall;
    function GetStoreProductsAsync(productKinds: IIterable_1__HSTRING; storeIds: IIterable_1__HSTRING): IAsyncOperation_1__IStoreProductQueryResult; safecall;
    function GetAssociatedStoreProductsAsync(productKinds: IIterable_1__HSTRING): IAsyncOperation_1__IStoreProductQueryResult; safecall;
    function GetAssociatedStoreProductsWithPagingAsync(productKinds: IIterable_1__HSTRING; maxItemsToRetrievePerPage: Cardinal): IAsyncOperation_1__IStoreProductPagedQueryResult; safecall;
    function GetUserCollectionAsync(productKinds: IIterable_1__HSTRING): IAsyncOperation_1__IStoreProductQueryResult; safecall;
    function GetUserCollectionWithPagingAsync(productKinds: IIterable_1__HSTRING; maxItemsToRetrievePerPage: Cardinal): IAsyncOperation_1__IStoreProductPagedQueryResult; safecall;
    function ReportConsumableFulfillmentAsync(productStoreId: HSTRING; quantity: Cardinal; trackingId: TGuid): IAsyncOperation_1__IStoreConsumableResult; safecall;
    function GetConsumableBalanceRemainingAsync(productStoreId: HSTRING): IAsyncOperation_1__IStoreConsumableResult; safecall;
    function AcquireStoreLicenseForOptionalPackageAsync(optionalPackage: IPackage): IAsyncOperation_1__IStoreAcquireLicenseResult; safecall;
    function RequestPurchaseAsync(storeId: HSTRING): IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    function RequestPurchaseAsync(storeId: HSTRING; storePurchaseProperties: IStorePurchaseProperties): IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    function GetAppAndOptionalStorePackageUpdatesAsync: IAsyncOperation_1__IVectorView_1__IStorePackageUpdate; safecall;
    function RequestDownloadStorePackageUpdatesAsync(storePackageUpdates: IIterable_1__IStorePackageUpdate): IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    function RequestDownloadAndInstallStorePackageUpdatesAsync(storePackageUpdates: IIterable_1__IStorePackageUpdate): IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    function RequestDownloadAndInstallStorePackagesAsync(storeIds: IIterable_1__HSTRING): IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    property User: IUser read get_User;
  end;

  // DualAPI Interface
  // Windows.System.IUser
  [WinRTClassNameAttribute(SWindows_System_User)]
  IUser = interface(IInspectable)
  ['{DF9A26C6-E746-4BCD-B5D4-120103C4209B}']
    function get_NonRoamableId: HSTRING; safecall;
    function get_AuthenticationStatus: UserAuthenticationStatus; safecall;
    function get_Type: UserType; safecall;
    function GetPropertyAsync(value: HSTRING): IAsyncOperation_1__IInspectable; safecall;
    function GetPropertiesAsync(values: IVectorView_1__HSTRING): IAsyncOperation_1__IPropertySet; safecall;
    function GetPictureAsync(desiredSize: UserPictureSize): IAsyncOperation_1__IRandomAccessStreamReference; safecall;
    property AuthenticationStatus: UserAuthenticationStatus read get_AuthenticationStatus;
    property NonRoamableId: HSTRING read get_NonRoamableId;
    property &Type: UserType read get_Type;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Object>
  IAsyncOperation_1__IInspectable_Base = interface(IInspectable)
  ['{ABF53C57-EE50-5342-B52A-26E3B8CC024F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IInspectable); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IInspectable; safecall;
    function GetResults: IInspectable; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IInspectable read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Object>
  // External 
  IAsyncOperation_1__IInspectable = interface(IAsyncOperation_1__IInspectable_Base)
  ['{ABF53C57-EE50-5342-B52A-26E3B8CC024F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  AsyncOperationCompletedHandler_1__IInspectable_Delegate_Base = interface(IUnknown)
  ['{3F08262E-A2E1-5134-9297-E9211F481A2D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IInspectable; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  // External 
  AsyncOperationCompletedHandler_1__IInspectable = interface(AsyncOperationCompletedHandler_1__IInspectable_Delegate_Base)
  ['{3F08262E-A2E1-5134-9297-E9211F481A2D}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<String>
  // External 
  IVectorView_1__HSTRING = interface(IInspectable)
  ['{2F13C006-A03A-5F69-B090-75A43E33423E}']
    function GetAt(index: Cardinal): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: HSTRING; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IAsyncOperation_1__IRandomAccessStreamReference_Base = interface(IInspectable)
  ['{65178D50-E6A2-5D16-B244-65E9725E5A0C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IRandomAccessStreamReference); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IRandomAccessStreamReference; safecall;
    function GetResults: IRandomAccessStreamReference; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IRandomAccessStreamReference read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  // External 
  IAsyncOperation_1__IRandomAccessStreamReference = interface(IAsyncOperation_1__IRandomAccessStreamReference_Base)
  ['{65178D50-E6A2-5D16-B244-65E9725E5A0C}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  AsyncOperationCompletedHandler_1__IRandomAccessStreamReference_Delegate_Base = interface(IUnknown)
  ['{60847289-EA0B-5DF6-89DF-F2C62CBA9693}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IRandomAccessStreamReference; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  // External 
  AsyncOperationCompletedHandler_1__IRandomAccessStreamReference = interface(AsyncOperationCompletedHandler_1__IRandomAccessStreamReference_Delegate_Base)
  ['{60847289-EA0B-5DF6-89DF-F2C62CBA9693}']
  end;

  // DualAPI Interface
  // Windows.Storage.Streams.IRandomAccessStreamReference
  [WinRTClassNameAttribute(SWindows_Storage_Streams_RandomAccessStreamReference)]
  IRandomAccessStreamReference = interface(IInspectable)
  ['{33EE3134-1DD6-4E3A-8067-D1C162E8642B}']
    function OpenReadAsync: IAsyncOperation_1__IRandomAccessStreamWithContentType; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IAsyncOperation_1__IRandomAccessStreamWithContentType_Base = interface(IInspectable)
  ['{C4A57C5E-32B0-55B3-AD13-CE1C23041ED6}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType; safecall;
    function GetResults: IRandomAccessStreamWithContentType; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  // External 
  IAsyncOperation_1__IRandomAccessStreamWithContentType = interface(IAsyncOperation_1__IRandomAccessStreamWithContentType_Base)
  ['{C4A57C5E-32B0-55B3-AD13-CE1C23041ED6}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType_Delegate_Base = interface(IUnknown)
  ['{86D455B2-D795-554C-9C31-BF6539349C19}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IRandomAccessStreamWithContentType; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  // External 
  AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType = interface(AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType_Delegate_Base)
  ['{3DDDECF4-1D39-58E8-83B1-DBED541C7F35}']
  end;

  // Windows.Storage.Streams.IRandomAccessStreamWithContentType
  // External 
  IRandomAccessStreamWithContentType = interface(IInspectable)
  ['{CC254827-4B3D-438F-9232-10C76BC7E038}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<String>
  IAsyncOperation_1__HSTRING_Base = interface(IInspectable)
  ['{3E1FE603-F897-5263-B328-0806426B8A79}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__HSTRING); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__HSTRING; safecall;
    function GetResults: HSTRING; safecall;
    property Completed: AsyncOperationCompletedHandler_1__HSTRING read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<String>
  // External 
  IAsyncOperation_1__HSTRING = interface(IAsyncOperation_1__HSTRING_Base)
  ['{3E1FE603-F897-5263-B328-0806426B8A79}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  AsyncOperationCompletedHandler_1__HSTRING_Delegate_Base = interface(IUnknown)
  ['{B79A741F-7FB5-50AE-9E99-911201EC3D41}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__HSTRING; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  // External 
  AsyncOperationCompletedHandler_1__HSTRING = interface(AsyncOperationCompletedHandler_1__HSTRING_Delegate_Base)
  ['{B79A741F-7FB5-50AE-9E99-911201EC3D41}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreAppLicense>
  IAsyncOperation_1__IStoreAppLicense_Base = interface(IInspectable)
  ['{3866370B-AFC6-5D01-84C2-4574628DE539}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreAppLicense); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreAppLicense; safecall;
    function GetResults: IStoreAppLicense; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreAppLicense read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreAppLicense>
  // External 
  IAsyncOperation_1__IStoreAppLicense = interface(IAsyncOperation_1__IStoreAppLicense_Base)
  ['{CA226C25-BABF-5F1E-8618-2C283333A3BD}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreAppLicense>
  AsyncOperationCompletedHandler_1__IStoreAppLicense_Delegate_Base = interface(IUnknown)
  ['{CEFF1E09-E506-50AD-A908-52038C256552}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreAppLicense; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreAppLicense>
  // External 
  AsyncOperationCompletedHandler_1__IStoreAppLicense = interface(AsyncOperationCompletedHandler_1__IStoreAppLicense_Delegate_Base)
  ['{87F9166A-AB97-5E61-8BCE-FBAF8AFB06A3}']
  end;

  // Windows.Services.Store.IStoreAppLicense
  // External 
  IStoreAppLicense = interface(IInspectable)
  ['{F389F9DE-73C0-45CE-9BAB-B2FE3E5EAFD3}']
    function get_SkuStoreId: HSTRING; safecall;
    function get_IsActive: Boolean; safecall;
    function get_IsTrial: Boolean; safecall;
    function get_ExpirationDate: DateTime; safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    function get_AddOnLicenses: IMapView_2__HSTRING__IStoreLicense; safecall;
    function get_TrialTimeRemaining: TimeSpan; safecall;
    function get_IsTrialOwnedByThisUser: Boolean; safecall;
    function get_TrialUniqueId: HSTRING; safecall;
    property AddOnLicenses: IMapView_2__HSTRING__IStoreLicense read get_AddOnLicenses;
    property ExpirationDate: DateTime read get_ExpirationDate;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData;
    property IsActive: Boolean read get_IsActive;
    property IsTrial: Boolean read get_IsTrial;
    property IsTrialOwnedByThisUser: Boolean read get_IsTrialOwnedByThisUser;
    property SkuStoreId: HSTRING read get_SkuStoreId;
    property TrialTimeRemaining: TimeSpan read get_TrialTimeRemaining;
    property TrialUniqueId: HSTRING read get_TrialUniqueId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Services.Store.IStoreLicense>
  IMapView_2__HSTRING__IStoreLicense_Base = interface(IInspectable)
  ['{7D0D74F5-4020-54AA-9F3D-0F17127ACDDF}']
    function Lookup(key: HSTRING): IStoreLicense; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IStoreLicense; out second: IMapView_2__HSTRING__IStoreLicense); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Services.Store.IStoreLicense>
  // External 
  IMapView_2__HSTRING__IStoreLicense = interface(IMapView_2__HSTRING__IStoreLicense_Base)
  ['{877D23DB-AF94-5D7C-B9D5-659B7FD285C5}']
  end;

  // Windows.Services.Store.IStoreLicense
  // External 
  IStoreLicense = interface(IInspectable)
  ['{26DC9579-4C4F-4F30-BC89-649F60E36055}']
    function get_SkuStoreId: HSTRING; safecall;
    function get_IsActive: Boolean; safecall;
    function get_ExpirationDate: DateTime; safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    function get_InAppOfferToken: HSTRING; safecall;
    property ExpirationDate: DateTime read get_ExpirationDate;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData;
    property InAppOfferToken: HSTRING read get_InAppOfferToken;
    property IsActive: Boolean read get_IsActive;
    property SkuStoreId: HSTRING read get_SkuStoreId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductResult>
  IAsyncOperation_1__IStoreProductResult_Base = interface(IInspectable)
  ['{9E61E86B-6AFB-50AE-AFC1-C59F545108DD}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreProductResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreProductResult; safecall;
    function GetResults: IStoreProductResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreProductResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductResult>
  // External 
  IAsyncOperation_1__IStoreProductResult = interface(IAsyncOperation_1__IStoreProductResult_Base)
  ['{C071B5BC-BFF2-59D1-84FA-8ECBE3B73F8A}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductResult>
  AsyncOperationCompletedHandler_1__IStoreProductResult_Delegate_Base = interface(IUnknown)
  ['{EB93E936-D515-5414-9D15-F050C0B8F521}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreProductResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductResult>
  // External 
  AsyncOperationCompletedHandler_1__IStoreProductResult = interface(AsyncOperationCompletedHandler_1__IStoreProductResult_Delegate_Base)
  ['{318BEF1D-DC4E-5919-AB41-BA3C8898B4B2}']
  end;

  // Windows.Services.Store.IStoreProductResult
  // External 
  IStoreProductResult = interface(IInspectable)
  ['{B7674F73-3C87-4EE1-8201-F428359BD3AF}']
    function get_Product: IStoreProduct; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Product: IStoreProduct read get_Product;
  end;

  // Windows.Services.Store.IStoreProduct
  // External 
  IStoreProduct = interface(IInspectable)
  ['{320E2C52-D760-450A-A42B-67D1E901AC90}']
    function get_StoreId: HSTRING; safecall;
    function get_Language: HSTRING; safecall;
    function get_Title: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_ProductKind: HSTRING; safecall;
    function get_HasDigitalDownload: Boolean; safecall;
    function get_Keywords: IVectorView_1__HSTRING; safecall;
    function get_Images: IVectorView_1__IStoreImage; safecall;
    function get_Videos: IVectorView_1__IStoreVideo; safecall;
    function get_Skus: IVectorView_1__IStoreSku; safecall;
    function get_IsInUserCollection: Boolean; safecall;
    function get_Price: IStorePrice; safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    function get_LinkUri: IUriRuntimeClass; safecall;
    function GetIsAnySkuInstalledAsync: IAsyncOperation_1__Boolean; safecall;
    function RequestPurchaseAsync: IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    function RequestPurchaseAsync(storePurchaseProperties: IStorePurchaseProperties): IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    function get_InAppOfferToken: HSTRING; safecall;
    property Description: HSTRING read get_Description;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData;
    property HasDigitalDownload: Boolean read get_HasDigitalDownload;
    property Images: IVectorView_1__IStoreImage read get_Images;
    property InAppOfferToken: HSTRING read get_InAppOfferToken;
    property IsInUserCollection: Boolean read get_IsInUserCollection;
    property Keywords: IVectorView_1__HSTRING read get_Keywords;
    property Language: HSTRING read get_Language;
    property LinkUri: IUriRuntimeClass read get_LinkUri;
    property Price: IStorePrice read get_Price;
    property ProductKind: HSTRING read get_ProductKind;
    property Skus: IVectorView_1__IStoreSku read get_Skus;
    property StoreId: HSTRING read get_StoreId;
    property Title: HSTRING read get_Title;
    property Videos: IVectorView_1__IStoreVideo read get_Videos;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreImage>
  // External 
  IVectorView_1__IStoreImage = interface(IInspectable)
  ['{641B279E-E8BE-5755-BF5F-058F31ED3A9C}']
    function GetAt(index: Cardinal): IStoreImage; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStoreImage; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStoreImage): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Services.Store.IStoreImage
  // External 
  IStoreImage = interface(IInspectable)
  ['{081FD248-ADB4-4B64-A993-784789926ED5}']
    function get_Uri: IUriRuntimeClass; safecall;
    function get_ImagePurposeTag: HSTRING; safecall;
    function get_Width: Cardinal; safecall;
    function get_Height: Cardinal; safecall;
    function get_Caption: HSTRING; safecall;
    property Caption: HSTRING read get_Caption;
    property Height: Cardinal read get_Height;
    property ImagePurposeTag: HSTRING read get_ImagePurposeTag;
    property Uri: IUriRuntimeClass read get_Uri;
    property Width: Cardinal read get_Width;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreVideo>
  // External 
  IVectorView_1__IStoreVideo = interface(IInspectable)
  ['{B240054D-573B-589D-9037-E5EC156D3F2C}']
    function GetAt(index: Cardinal): IStoreVideo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStoreVideo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStoreVideo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Services.Store.IStoreVideo
  // External 
  IStoreVideo = interface(IInspectable)
  ['{F26CB184-6F5E-4DC2-886C-3C63083C2F94}']
    function get_Uri: IUriRuntimeClass; safecall;
    function get_VideoPurposeTag: HSTRING; safecall;
    function get_Width: Cardinal; safecall;
    function get_Height: Cardinal; safecall;
    function get_Caption: HSTRING; safecall;
    function get_PreviewImage: IStoreImage; safecall;
    property Caption: HSTRING read get_Caption;
    property Height: Cardinal read get_Height;
    property PreviewImage: IStoreImage read get_PreviewImage;
    property Uri: IUriRuntimeClass read get_Uri;
    property VideoPurposeTag: HSTRING read get_VideoPurposeTag;
    property Width: Cardinal read get_Width;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreSku>
  // External 
  IVectorView_1__IStoreSku = interface(IInspectable)
  ['{C66AA27F-F6F7-53FB-A2A2-1C7CB2E12F78}']
    function GetAt(index: Cardinal): IStoreSku; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStoreSku; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStoreSku): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Services.Store.IStoreSku
  // External 
  IStoreSku = interface(IInspectable)
  ['{397E6F55-4440-4F03-863C-91F3FEC83D79}']
    function get_StoreId: HSTRING; safecall;
    function get_Language: HSTRING; safecall;
    function get_Title: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_IsTrial: Boolean; safecall;
    function get_CustomDeveloperData: HSTRING; safecall;
    function get_Images: IVectorView_1__IStoreImage; safecall;
    function get_Videos: IVectorView_1__IStoreVideo; safecall;
    function get_Availabilities: IVectorView_1__IStoreAvailability; safecall;
    function get_Price: IStorePrice; safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    function get_IsInUserCollection: Boolean; safecall;
    function get_BundledSkus: IVectorView_1__HSTRING; safecall;
    function get_CollectionData: IStoreCollectionData; safecall;
    function GetIsInstalledAsync: IAsyncOperation_1__Boolean; safecall;
    function RequestPurchaseAsync: IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    function RequestPurchaseAsync(storePurchaseProperties: IStorePurchaseProperties): IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    function get_IsSubscription: Boolean; safecall;
    function get_SubscriptionInfo: IStoreSubscriptionInfo; safecall;
    property Availabilities: IVectorView_1__IStoreAvailability read get_Availabilities;
    property BundledSkus: IVectorView_1__HSTRING read get_BundledSkus;
    property CollectionData: IStoreCollectionData read get_CollectionData;
    property CustomDeveloperData: HSTRING read get_CustomDeveloperData;
    property Description: HSTRING read get_Description;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData;
    property Images: IVectorView_1__IStoreImage read get_Images;
    property IsInUserCollection: Boolean read get_IsInUserCollection;
    property IsSubscription: Boolean read get_IsSubscription;
    property IsTrial: Boolean read get_IsTrial;
    property Language: HSTRING read get_Language;
    property Price: IStorePrice read get_Price;
    property StoreId: HSTRING read get_StoreId;
    property SubscriptionInfo: IStoreSubscriptionInfo read get_SubscriptionInfo;
    property Title: HSTRING read get_Title;
    property Videos: IVectorView_1__IStoreVideo read get_Videos;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStoreAvailability>
  // External 
  IVectorView_1__IStoreAvailability = interface(IInspectable)
  ['{831B70EA-807A-5FFD-9C1F-FD60E46387D5}']
    function GetAt(index: Cardinal): IStoreAvailability; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStoreAvailability; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStoreAvailability): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Services.Store.IStoreAvailability
  // External 
  IStoreAvailability = interface(IInspectable)
  ['{FA060325-0FFD-4493-AD43-F1F9918F69FA}']
    function get_StoreId: HSTRING; safecall;
    function get_EndDate: DateTime; safecall;
    function get_Price: IStorePrice; safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    function RequestPurchaseAsync: IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    function RequestPurchaseAsync(storePurchaseProperties: IStorePurchaseProperties): IAsyncOperation_1__IStorePurchaseResult; overload; safecall;
    property EndDate: DateTime read get_EndDate;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData;
    property Price: IStorePrice read get_Price;
    property StoreId: HSTRING read get_StoreId;
  end;

  // Windows.Services.Store.IStorePrice
  // External 
  IStorePrice = interface(IInspectable)
  ['{55BA94C4-15F1-407C-8F06-006380F4DF0B}']
    function get_FormattedBasePrice: HSTRING; safecall;
    function get_FormattedPrice: HSTRING; safecall;
    function get_IsOnSale: Boolean; safecall;
    function get_SaleEndDate: DateTime; safecall;
    function get_CurrencyCode: HSTRING; safecall;
    function get_FormattedRecurrencePrice: HSTRING; safecall;
    property CurrencyCode: HSTRING read get_CurrencyCode;
    property FormattedBasePrice: HSTRING read get_FormattedBasePrice;
    property FormattedPrice: HSTRING read get_FormattedPrice;
    property FormattedRecurrencePrice: HSTRING read get_FormattedRecurrencePrice;
    property IsOnSale: Boolean read get_IsOnSale;
    property SaleEndDate: DateTime read get_SaleEndDate;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStorePurchaseResult>
  IAsyncOperation_1__IStorePurchaseResult_Base = interface(IInspectable)
  ['{33D8CC30-78F5-5F81-AA2D-A4FA2A3B1C68}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStorePurchaseResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStorePurchaseResult; safecall;
    function GetResults: IStorePurchaseResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStorePurchaseResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStorePurchaseResult>
  // External 
  IAsyncOperation_1__IStorePurchaseResult = interface(IAsyncOperation_1__IStorePurchaseResult_Base)
  ['{CB0E05EF-67B6-5CE5-A236-E1D42A8E596D}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStorePurchaseResult>
  AsyncOperationCompletedHandler_1__IStorePurchaseResult_Delegate_Base = interface(IUnknown)
  ['{1D9F89EE-2FCE-54E6-A0A9-52D00C52CC3A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStorePurchaseResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStorePurchaseResult>
  // External 
  AsyncOperationCompletedHandler_1__IStorePurchaseResult = interface(AsyncOperationCompletedHandler_1__IStorePurchaseResult_Delegate_Base)
  ['{2A34AE20-48B6-571D-B3E7-221126420352}']
  end;

  // Windows.Services.Store.IStorePurchaseResult
  // External 
  IStorePurchaseResult = interface(IInspectable)
  ['{ADD28552-F96A-463D-A7BB-C20B4FCA6952}']
    function get_Status: StorePurchaseStatus; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Status: StorePurchaseStatus read get_Status;
  end;

  // Windows.Services.Store.IStorePurchaseProperties
  // External 
  IStorePurchaseProperties = interface(IInspectable)
  ['{836278F3-FF87-4364-A5B4-FD2153EBE43B}']
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    procedure put_ExtendedJsonData(value: HSTRING); safecall;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData write put_ExtendedJsonData;
    property Name: HSTRING read get_Name write put_Name;
  end;

  // Windows.Services.Store.IStoreCollectionData
  // External 
  IStoreCollectionData = interface(IInspectable)
  ['{8AA4C3B3-5BB3-441A-2AB4-4DAB73D5CE67}']
    function get_IsTrial: Boolean; safecall;
    function get_CampaignId: HSTRING; safecall;
    function get_DeveloperOfferId: HSTRING; safecall;
    function get_AcquiredDate: DateTime; safecall;
    function get_StartDate: DateTime; safecall;
    function get_EndDate: DateTime; safecall;
    function get_TrialTimeRemaining: TimeSpan; safecall;
    function get_ExtendedJsonData: HSTRING; safecall;
    property AcquiredDate: DateTime read get_AcquiredDate;
    property CampaignId: HSTRING read get_CampaignId;
    property DeveloperOfferId: HSTRING read get_DeveloperOfferId;
    property EndDate: DateTime read get_EndDate;
    property ExtendedJsonData: HSTRING read get_ExtendedJsonData;
    property IsTrial: Boolean read get_IsTrial;
    property StartDate: DateTime read get_StartDate;
    property TrialTimeRemaining: TimeSpan read get_TrialTimeRemaining;
  end;

  // Windows.Services.Store.IStoreSubscriptionInfo
  // External 
  IStoreSubscriptionInfo = interface(IInspectable)
  ['{4189776A-0559-43AC-A9C6-3AB0011FB8EB}']
    function get_BillingPeriod: Cardinal; safecall;
    function get_BillingPeriodUnit: StoreDurationUnit; safecall;
    function get_HasTrialPeriod: Boolean; safecall;
    function get_TrialPeriod: Cardinal; safecall;
    function get_TrialPeriodUnit: StoreDurationUnit; safecall;
    property BillingPeriod: Cardinal read get_BillingPeriod;
    property BillingPeriodUnit: StoreDurationUnit read get_BillingPeriodUnit;
    property HasTrialPeriod: Boolean read get_HasTrialPeriod;
    property TrialPeriod: Cardinal read get_TrialPeriod;
    property TrialPeriodUnit: StoreDurationUnit read get_TrialPeriodUnit;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductQueryResult>
  IAsyncOperation_1__IStoreProductQueryResult_Base = interface(IInspectable)
  ['{9699E7BB-EA1F-5E03-9439-C80E6977B711}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreProductQueryResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreProductQueryResult; safecall;
    function GetResults: IStoreProductQueryResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreProductQueryResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductQueryResult>
  // External 
  IAsyncOperation_1__IStoreProductQueryResult = interface(IAsyncOperation_1__IStoreProductQueryResult_Base)
  ['{7F680236-A321-51CB-839B-804B9FD215E2}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductQueryResult>
  AsyncOperationCompletedHandler_1__IStoreProductQueryResult_Delegate_Base = interface(IUnknown)
  ['{02F4A42C-0458-58D6-923C-B44BA8EF2222}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreProductQueryResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductQueryResult>
  // External 
  AsyncOperationCompletedHandler_1__IStoreProductQueryResult = interface(AsyncOperationCompletedHandler_1__IStoreProductQueryResult_Delegate_Base)
  ['{292E4706-ABCB-5774-A057-55448902142C}']
  end;

  // Windows.Services.Store.IStoreProductQueryResult
  // External 
  IStoreProductQueryResult = interface(IInspectable)
  ['{D805E6C5-D456-4FF6-8049-9076D5165F73}']
    function get_Products: IMapView_2__HSTRING__IStoreProduct; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Products: IMapView_2__HSTRING__IStoreProduct read get_Products;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Services.Store.IStoreProduct>
  IMapView_2__HSTRING__IStoreProduct_Base = interface(IInspectable)
  ['{DBAAC6E5-61A4-5C88-B5D8-3A3E161C3E4A}']
    function Lookup(key: HSTRING): IStoreProduct; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IStoreProduct; out second: IMapView_2__HSTRING__IStoreProduct); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Services.Store.IStoreProduct>
  // External 
  IMapView_2__HSTRING__IStoreProduct = interface(IMapView_2__HSTRING__IStoreProduct_Base)
  ['{7C28626B-F200-5355-8574-02005573BC63}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING_Base = interface(IInspectable)
  ['{E2FCC7C1-3BFC-5A0B-B2B0-72E769D1CB7E}']
    function First: IIterator_1__HSTRING; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<String>
  // External 
  IIterable_1__HSTRING = interface(IIterable_1__HSTRING_Base)
  ['{E2FCC7C1-3BFC-5A0B-B2B0-72E769D1CB7E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING_Base = interface(IInspectable)
  ['{8C304EBB-6615-50A4-8829-879ECD443236}']
    function get_Current: HSTRING; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PHSTRING): Cardinal; safecall;
    property Current: HSTRING read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<String>
  // External 
  IIterator_1__HSTRING = interface(IIterator_1__HSTRING_Base)
  ['{8C304EBB-6615-50A4-8829-879ECD443236}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductPagedQueryResult>
  IAsyncOperation_1__IStoreProductPagedQueryResult_Base = interface(IInspectable)
  ['{3079E7DB-1BA4-5B9E-856A-6576BF7F9C8A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult; safecall;
    function GetResults: IStoreProductPagedQueryResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreProductPagedQueryResult>
  // External 
  IAsyncOperation_1__IStoreProductPagedQueryResult = interface(IAsyncOperation_1__IStoreProductPagedQueryResult_Base)
  ['{F878308C-4FDD-554E-AB2C-83466B5D778B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductPagedQueryResult>
  AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult_Delegate_Base = interface(IUnknown)
  ['{E786321F-B791-5E38-8BC4-98CB287D1085}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreProductPagedQueryResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreProductPagedQueryResult>
  // External 
  AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult = interface(AsyncOperationCompletedHandler_1__IStoreProductPagedQueryResult_Delegate_Base)
  ['{10FB362F-7495-5DE3-BA40-0BD9550F744E}']
  end;

  // Windows.Services.Store.IStoreProductPagedQueryResult
  // External 
  IStoreProductPagedQueryResult = interface(IInspectable)
  ['{C92718C5-4DD5-4869-A462-ECC6872E43C5}']
    function get_Products: IMapView_2__HSTRING__IStoreProduct; safecall;
    function get_HasMoreResults: Boolean; safecall;
    function get_ExtendedError: HRESULT; safecall;
    function GetNextAsync: IAsyncOperation_1__IStoreProductPagedQueryResult; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property HasMoreResults: Boolean read get_HasMoreResults;
    property Products: IMapView_2__HSTRING__IStoreProduct read get_Products;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreConsumableResult>
  IAsyncOperation_1__IStoreConsumableResult_Base = interface(IInspectable)
  ['{873C497B-C3F7-5657-B921-3E58CE48EE50}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreConsumableResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreConsumableResult; safecall;
    function GetResults: IStoreConsumableResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreConsumableResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreConsumableResult>
  // External 
  IAsyncOperation_1__IStoreConsumableResult = interface(IAsyncOperation_1__IStoreConsumableResult_Base)
  ['{AD6D53A4-7EBC-5E8B-8B0A-0153E7E5695F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreConsumableResult>
  AsyncOperationCompletedHandler_1__IStoreConsumableResult_Delegate_Base = interface(IUnknown)
  ['{3F2BB178-3C4E-56ED-86A5-AD13797CFBFD}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreConsumableResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreConsumableResult>
  // External 
  AsyncOperationCompletedHandler_1__IStoreConsumableResult = interface(AsyncOperationCompletedHandler_1__IStoreConsumableResult_Delegate_Base)
  ['{A0822E34-F573-574A-A5F8-38618D5411EC}']
  end;

  // Windows.Services.Store.IStoreConsumableResult
  // External 
  IStoreConsumableResult = interface(IInspectable)
  ['{EA5DAB72-6A00-4052-BE5B-BFDAB4433352}']
    function get_Status: StoreConsumableStatus; safecall;
    function get_TrackingId: TGuid; safecall;
    function get_BalanceRemaining: Cardinal; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property BalanceRemaining: Cardinal read get_BalanceRemaining;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Status: StoreConsumableStatus read get_Status;
    property TrackingId: TGuid read get_TrackingId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreAcquireLicenseResult>
  IAsyncOperation_1__IStoreAcquireLicenseResult_Base = interface(IInspectable)
  ['{DD6C4705-A76C-528E-99A5-CDD13197D4CF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult; safecall;
    function GetResults: IStoreAcquireLicenseResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Store.IStoreAcquireLicenseResult>
  // External 
  IAsyncOperation_1__IStoreAcquireLicenseResult = interface(IAsyncOperation_1__IStoreAcquireLicenseResult_Base)
  ['{952A4A93-DB35-518D-82AD-F8FB6D84AE29}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreAcquireLicenseResult>
  AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult_Delegate_Base = interface(IUnknown)
  ['{6987C97C-2C19-5F44-B5AC-37393F3C1A4A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IStoreAcquireLicenseResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Store.IStoreAcquireLicenseResult>
  // External 
  AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult = interface(AsyncOperationCompletedHandler_1__IStoreAcquireLicenseResult_Delegate_Base)
  ['{9DD2E89D-B648-5F66-BD9A-E5FBBA66B2FE}']
  end;

  // Windows.Services.Store.IStoreAcquireLicenseResult
  // External 
  IStoreAcquireLicenseResult = interface(IInspectable)
  ['{FBD7946D-F040-4CB3-9A39-29BCECDBE22D}']
    function get_StorePackageLicense: IStorePackageLicense; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property StorePackageLicense: IStorePackageLicense read get_StorePackageLicense;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>>
  IAsyncOperation_1__IVectorView_1__IStorePackageUpdate_Base = interface(IInspectable)
  ['{0AC66C33-45B8-546B-AAAF-D58D62A4C5C5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate; safecall;
    function GetResults: IVectorView_1__IStorePackageUpdate; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>>
  // External 
  IAsyncOperation_1__IVectorView_1__IStorePackageUpdate = interface(IAsyncOperation_1__IVectorView_1__IStorePackageUpdate_Base)
  ['{603DCCA3-2B6D-50E7-AAAA-7DDFCEAF169E}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate_Delegate_Base = interface(IUnknown)
  ['{F8491BCD-2DB5-58E0-8C47-44E6EB10C12D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IStorePackageUpdate; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>>
  // External 
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IStorePackageUpdate_Delegate_Base)
  ['{EC684BE8-7BAA-5335-934F-E647E407C1A6}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.IStorePackageUpdate>
  // External 
  IVectorView_1__IStorePackageUpdate = interface(IInspectable)
  ['{8E067DA9-4FAA-560F-BCE3-D974664BC4AC}']
    function GetAt(index: Cardinal): IStorePackageUpdate; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStorePackageUpdate; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorePackageUpdate): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Services.Store.IStorePackageUpdate
  // External 
  IStorePackageUpdate = interface(IInspectable)
  ['{140FA150-3CBF-4A35-B91F-48271C31B072}']
    function get_Package: IPackage; safecall;
    function get_Mandatory: Boolean; safecall;
    property Mandatory: Boolean read get_Mandatory;
    property Package: IPackage read get_Package;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Base = interface(IInspectable)
  ['{42C436CA-51F7-50B2-8FE4-7B754062E6EB}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus; safecall;
    function GetResults: IStorePackageUpdateResult; safecall;
    property Progress: AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  // External 
  IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus = interface(IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Base)
  ['{7B9F1EF8-E09D-55D9-BA11-6C868BDA3430}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base = interface(IUnknown)
  ['{961260F1-7352-5EDF-9666-1F9A0A8EE477}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; progressInfo: StorePackageUpdateStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  // External 
  AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = interface(AsyncOperationProgressHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base)
  ['{F9DD63B7-E13B-51CF-B017-963980CE0191}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base = interface(IUnknown)
  ['{B3BE0C8B-EF1D-56DC-8547-4DA06EA563DF}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IStorePackageUpdateResult__StorePackageUpdateStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Services.Store.IStorePackageUpdateResult,Windows.Services.Store.StorePackageUpdateStatus>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus = interface(AsyncOperationWithProgressCompletedHandler_2__IStorePackageUpdateResult__StorePackageUpdateStatus_Delegate_Base)
  ['{4FF3F43E-8B85-58FF-8DCC-5A8BC6EC699B}']
  end;

  // Windows.Services.Store.IStorePackageUpdateResult
  // External 
  IStorePackageUpdateResult = interface(IInspectable)
  ['{E79142ED-61F9-4893-B4FE-CF191603AF7B}']
    function get_OverallState: StorePackageUpdateState; safecall;
    function get_StorePackageUpdateStatuses: IVectorView_1__StorePackageUpdateStatus; safecall;
    property OverallState: StorePackageUpdateState read get_OverallState;
    property StorePackageUpdateStatuses: IVectorView_1__StorePackageUpdateStatus read get_StorePackageUpdateStatuses;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Store.StorePackageUpdateStatus>
  // External 
  IVectorView_1__StorePackageUpdateStatus = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): StorePackageUpdateStatus; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: StorePackageUpdateStatus; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PStorePackageUpdateStatus): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStorePackageUpdate>
  IIterable_1__IStorePackageUpdate_Base = interface(IInspectable)
  ['{6B076C51-849E-5EC5-AED5-9B0585591902}']
    function First: IIterator_1__IStorePackageUpdate; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Store.IStorePackageUpdate>
  // External 
  IIterable_1__IStorePackageUpdate = interface(IIterable_1__IStorePackageUpdate_Base)
  ['{E7B20971-0E80-5A2D-8C77-A71A68EEE18B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStorePackageUpdate>
  IIterator_1__IStorePackageUpdate_Base = interface(IInspectable)
  ['{B75DD77B-87CA-5956-8902-84E9FFC97D83}']
    function get_Current: IStorePackageUpdate; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStorePackageUpdate): Cardinal; safecall;
    property Current: IStorePackageUpdate read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Store.IStorePackageUpdate>
  // External 
  IIterator_1__IStorePackageUpdate = interface(IIterator_1__IStorePackageUpdate_Base)
  ['{C2BCB285-D3C8-5F2A-8215-EE0C81093298}']
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStoreQueueItem,Object>
  // External 
  TypedEventHandler_2__IStoreQueueItem__IInspectable = interface(IUnknown)
  ['{6B3A03D4-D264-5FE3-AB73-5705942DB1B8}']
    procedure Invoke(sender: IStoreQueueItem; args: IInspectable); safecall;
  end;

  // Windows.Services.Store.IStoreQueueItem
  // External 
  IStoreQueueItem = interface(IInspectable)
  ['{56D5C32B-F830-4293-9188-CAD2DCDE7357}']
    function get_ProductId: HSTRING; safecall;
    function get_PackageFamilyName: HSTRING; safecall;
    function get_InstallKind: StoreQueueItemKind; safecall;
    function GetCurrentStatus: IStoreQueueItemStatus; safecall;
    function add_Completed(handler: TypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Completed(token: EventRegistrationToken); safecall;
    function add_StatusChanged(handler: TypedEventHandler_2__IStoreQueueItem__IInspectable): EventRegistrationToken; safecall;
    procedure remove_StatusChanged(token: EventRegistrationToken); safecall;
    property InstallKind: StoreQueueItemKind read get_InstallKind;
    property PackageFamilyName: HSTRING read get_PackageFamilyName;
    property ProductId: HSTRING read get_ProductId;
  end;

  // Windows.Services.Store.IStoreQueueItemStatus
  // External 
  IStoreQueueItemStatus = interface(IInspectable)
  ['{9BD6796F-9CC3-4EC3-B2EF-7BE433B30174}']
    function get_PackageInstallState: StoreQueueItemState; safecall;
    function get_PackageInstallExtendedState: StoreQueueItemExtendedState; safecall;
    function get_UpdateStatus: StorePackageUpdateStatus; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property PackageInstallExtendedState: StoreQueueItemExtendedState read get_PackageInstallExtendedState;
    property PackageInstallState: StoreQueueItemState read get_PackageInstallState;
    property UpdateStatus: StorePackageUpdateStatus read get_UpdateStatus;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Services.Store.IStoreQueueItem,Windows.Services.Store.IStoreQueueItemCompletedEventArgs>
  // External 
  TypedEventHandler_2__IStoreQueueItem__IStoreQueueItemCompletedEventArgs = interface(IUnknown)
  ['{E9584C9A-4439-5CB9-A842-86D61CEF668E}']
    procedure Invoke(sender: IStoreQueueItem; args: IStoreQueueItemCompletedEventArgs); safecall;
  end;

  // Windows.Services.Store.IStoreQueueItemCompletedEventArgs
  // External 
  IStoreQueueItemCompletedEventArgs = interface(IInspectable)
  ['{1247DF6C-B44A-439B-BB07-1D3003D005C2}']
    function get_Status: IStoreQueueItemStatus; safecall;
    property Status: IStoreQueueItemStatus read get_Status;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING_Base = interface(IInspectable)
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
  // Windows.Foundation.Collections.IVector`1<String>
  // External 
  IVector_1__HSTRING = interface(IVector_1__HSTRING_Base)
  ['{98B9ACC1-4B56-532E-AC73-03D5291CCA90}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<UInt32>
  IAsyncOperation_1__Cardinal_Base = interface(IInspectable)
  ['{EF60385F-BE78-584B-AAEF-7829ADA2B0DE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Cardinal); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Cardinal; safecall;
    function GetResults: Cardinal; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Cardinal read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<UInt32>
  // External 
  IAsyncOperation_1__Cardinal = interface(IAsyncOperation_1__Cardinal_Base)
  ['{EF60385F-BE78-584B-AAEF-7829ADA2B0DE}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  AsyncOperationCompletedHandler_1__Cardinal_Delegate_Base = interface(IUnknown)
  ['{9343B6E7-E3D2-5E4A-AB2D-2BCE4919A6A4}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  // External 
  AsyncOperationCompletedHandler_1__Cardinal = interface(AsyncOperationCompletedHandler_1__Cardinal_Delegate_Base)
  ['{9343B6E7-E3D2-5E4A-AB2D-2BCE4919A6A4}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Search.IStorageQueryResultBase,Object>
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable_Delegate_Base = interface(IUnknown)
  ['{4BA22861-00C4-597F-B6BF-3AF516F3B870}']
    procedure Invoke(sender: Search_IStorageQueryResultBase; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Search.IStorageQueryResultBase,Object>
  // External 
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = interface(TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable_Delegate_Base)
  ['{4BA22861-00C4-597F-B6BF-3AF516F3B870}']
  end;

  // Windows.Storage.Search.IStorageQueryResultBase
  // External 
  Search_IStorageQueryResultBase = interface(IInspectable)
  ['{C297D70D-7353-47AB-BA58-8C61425DC54B}']
    function GetItemCountAsync: IAsyncOperation_1__Cardinal; safecall;
    function get_Folder: IStorageFolder; safecall;
    function add_ContentsChanged(handler: TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ContentsChanged(eventCookie: EventRegistrationToken); safecall;
    function add_OptionsChanged(changedHandler: TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable): EventRegistrationToken; safecall;
    procedure remove_OptionsChanged(eventCookie: EventRegistrationToken); safecall;
    function FindStartIndexAsync(value: IInspectable): IAsyncOperation_1__Cardinal; safecall;
    function GetCurrentQueryOptions: Search_IQueryOptions; safecall;
    procedure ApplyNewQueryOptions(newQueryOptions: Search_IQueryOptions); safecall;
    property Folder: IStorageFolder read get_Folder;
  end;

  // Windows.Storage.Search.IQueryOptions
  // External 
  Search_IQueryOptions = interface(IInspectable)
  ['{1E5E46EE-0F45-4838-A8E9-D0479D446C30}']
    function get_FileTypeFilter: IVector_1__HSTRING; safecall;
    function get_FolderDepth: Search_FolderDepth; safecall;
    procedure put_FolderDepth(value: Search_FolderDepth); safecall;
    function get_ApplicationSearchFilter: HSTRING; safecall;
    procedure put_ApplicationSearchFilter(value: HSTRING); safecall;
    function get_UserSearchFilter: HSTRING; safecall;
    procedure put_UserSearchFilter(value: HSTRING); safecall;
    function get_Language: HSTRING; safecall;
    procedure put_Language(value: HSTRING); safecall;
    function get_IndexerOption: Search_IndexerOption; safecall;
    procedure put_IndexerOption(value: Search_IndexerOption); safecall;
    function get_SortOrder: IVector_1__Search_SortEntry; safecall;
    function get_GroupPropertyName: HSTRING; safecall;
    function get_DateStackOption: Search_DateStackOption; safecall;
    function SaveToString: HSTRING; safecall;
    procedure LoadFromString(value: HSTRING); safecall;
    procedure SetThumbnailPrefetch(mode: FileProperties_ThumbnailMode; requestedSize: Cardinal; options: FileProperties_ThumbnailOptions); safecall;
    procedure SetPropertyPrefetch(options: FileProperties_PropertyPrefetchOptions; propertiesToRetrieve: IIterable_1__HSTRING); safecall;
    property ApplicationSearchFilter: HSTRING read get_ApplicationSearchFilter write put_ApplicationSearchFilter;
    property DateStackOption: Search_DateStackOption read get_DateStackOption;
    property FileTypeFilter: IVector_1__HSTRING read get_FileTypeFilter;
    property FolderDepth: Search_FolderDepth read get_FolderDepth write put_FolderDepth;
    property GroupPropertyName: HSTRING read get_GroupPropertyName;
    property IndexerOption: Search_IndexerOption read get_IndexerOption write put_IndexerOption;
    property Language: HSTRING read get_Language write put_Language;
    property SortOrder: IVector_1__Search_SortEntry read get_SortOrder;
    property UserSearchFilter: HSTRING read get_UserSearchFilter write put_UserSearchFilter;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Search.SortEntry>
  IVector_1__Search_SortEntry_Base = interface(IInspectable)
  ['{D8EA401B-47B3-5254-84F4-EEA10C4CF068}']
    function GetAt(index: Cardinal): Search_SortEntry; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Search_SortEntry; safecall;
    function IndexOf(value: Search_SortEntry; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Search_SortEntry); safecall;
    procedure InsertAt(index: Cardinal; value: Search_SortEntry); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Search_SortEntry); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSearch_SortEntry): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PSearch_SortEntry); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Search.SortEntry>
  // External 
  IVector_1__Search_SortEntry = interface(IVector_1__Search_SortEntry_Base)
  ['{A4739064-B54E-55D4-8012-317E2B6A807B}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Search.SortEntry>
  // External 
  IVectorView_1__Search_SortEntry = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Search_SortEntry; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Search_SortEntry; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSearch_SortEntry): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVectorChangedEventArgs
  // External 
  IVectorChangedEventArgs = interface(IInspectable)
  ['{575933DF-34FE-4480-AF15-07691F3D5D9B}']
    function get_CollectionChange: CollectionChange; safecall;
    function get_Index: Cardinal; safecall;
    property CollectionChange_: CollectionChange read get_CollectionChange;
    property Index: Cardinal read get_Index;
  end;

  // Windows.Foundation.IReference`1<Double>
  // External 
  IReference_1__Double = interface(IInspectable)
  ['{2F2D6C29-5473-5F3E-92E7-96572BB990E2}']
    function get_Value: Double; safecall;
    property Value: Double read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt64,UInt64>
  IAsyncOperationWithProgress_2__UInt64__UInt64_Base = interface(IInspectable)
  ['{8F1DB6E3-6556-5516-825C-1021EE27CD0C}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__UInt64__UInt64); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__UInt64__UInt64; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64; safecall;
    function GetResults: UInt64; safecall;
    property Progress: AsyncOperationProgressHandler_2__UInt64__UInt64 read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt64,UInt64>
  // External 
  IAsyncOperationWithProgress_2__UInt64__UInt64 = interface(IAsyncOperationWithProgress_2__UInt64__UInt64_Base)
  ['{8F1DB6E3-6556-5516-825C-1021EE27CD0C}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt64,UInt64>
  AsyncOperationProgressHandler_2__UInt64__UInt64_Delegate_Base = interface(IUnknown)
  ['{FFB2B65D-4120-5D13-826D-107851E6BB1C}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__UInt64__UInt64; progressInfo: UInt64); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt64,UInt64>
  // External 
  AsyncOperationProgressHandler_2__UInt64__UInt64 = interface(AsyncOperationProgressHandler_2__UInt64__UInt64_Delegate_Base)
  ['{FFB2B65D-4120-5D13-826D-107851E6BB1C}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt64,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64_Delegate_Base = interface(IUnknown)
  ['{D2024E41-5500-5B5A-BA46-CB7009596A2F}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__UInt64__UInt64; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt64,UInt64>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = interface(AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64_Delegate_Base)
  ['{D2024E41-5500-5B5A-BA46-CB7009596A2F}']
  end;

  // Windows.System.RemoteSystems.IRemoteSystemConnectionRequest
  // External 
  RemoteSystems_IRemoteSystemConnectionRequest = interface(IInspectable)
  ['{84ED4104-8D5E-4D72-8238-7621576C7A67}']
    function get_RemoteSystem: RemoteSystems_IRemoteSystem; safecall;
    property RemoteSystem: RemoteSystems_IRemoteSystem read get_RemoteSystem;
  end;

  // Windows.System.RemoteSystems.IRemoteSystem
  // External 
  RemoteSystems_IRemoteSystem = interface(IInspectable)
  ['{ED5838CD-1E10-4A8C-B4A6-4E5FD6F97721}']
    function get_DisplayName: HSTRING; safecall;
    function get_Id: HSTRING; safecall;
    function get_Kind: HSTRING; safecall;
    function get_Status: RemoteSystems_RemoteSystemStatus; safecall;
    function get_IsAvailableByProximity: Boolean; safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property Id: HSTRING read get_Id;
    property IsAvailableByProximity: Boolean read get_IsAvailableByProximity;
    property Kind: HSTRING read get_Kind;
    property Status: RemoteSystems_RemoteSystemStatus read get_Status;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Guid>
  IIterator_1__TGuid_Base = interface(IInspectable)
  ['{D3D64048-82B3-53C7-9285-B0BE18368482}']
    function get_Current: TGuid; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PGuid): Cardinal; safecall;
    property Current: TGuid read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Guid>
  // External 
  IIterator_1__TGuid = interface(IIterator_1__TGuid_Base)
  ['{D3D64048-82B3-53C7-9285-B0BE18368482}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Guid>
  // External 
  IVectorView_1__TGuid = interface(IInspectable)
  ['{9520E64B-15B2-52A6-98ED-3191FA6CF68A}']
    function GetAt(index: Cardinal): TGuid; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TGuid; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGuid): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Int32>
  IIterator_1__Integer_Base = interface(IInspectable)
  ['{BFEA7F78-50C2-5F1D-A6EA-9E978D2699FF}']
    function get_Current: Integer; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInteger): Cardinal; safecall;
    property Current: Integer read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Int32>
  // External 
  IIterator_1__Integer = interface(IIterator_1__Integer_Base)
  ['{BFEA7F78-50C2-5F1D-A6EA-9E978D2699FF}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Int32>
  // External 
  IVectorView_1__Integer = interface(IInspectable)
  ['{8D720CDF-3934-5D3F-9A55-40E8063B086A}']
    function GetAt(index: Cardinal): Integer; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Integer; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInteger): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<UInt32>
  IIterator_1__Cardinal_Base = interface(IInspectable)
  ['{F06A2739-9443-5EF0-B284-DC5AFF3E7D10}']
    function get_Current: Cardinal; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCardinal): Cardinal; safecall;
    property Current: Cardinal read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<UInt32>
  // External 
  IIterator_1__Cardinal = interface(IIterator_1__Cardinal_Base)
  ['{F06A2739-9443-5EF0-B284-DC5AFF3E7D10}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<UInt32>
  // External 
  IVectorView_1__Cardinal = interface(IInspectable)
  ['{E5CE1A07-8D33-5007-BA64-7D2508CCF85C}']
    function GetAt(index: Cardinal): Cardinal; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Cardinal; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCardinal): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.System.IDispatcherQueueTimer
  // External 
  IDispatcherQueueTimer = interface(IInspectable)
  ['{5FEABB1D-A31C-4727-B1AC-37454649D56A}']
    function get_Interval: TimeSpan; safecall;
    procedure put_Interval(value: TimeSpan); safecall;
    function get_IsRunning: Boolean; safecall;
    function get_IsRepeating: Boolean; safecall;
    procedure put_IsRepeating(value: Boolean); safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    function add_Tick(handler: TypedEventHandler_2__IDispatcherQueueTimer__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Tick(token: EventRegistrationToken); safecall;
    property Interval: TimeSpan read get_Interval write put_Interval;
    property IsRepeating: Boolean read get_IsRepeating write put_IsRepeating;
    property IsRunning: Boolean read get_IsRunning;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueueTimer,Object>
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable_Delegate_Base = interface(IUnknown)
  ['{8B5644C8-8B57-50CE-8933-7AB2CC5A14EF}']
    procedure Invoke(sender: IDispatcherQueueTimer; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueueTimer,Object>
  // External 
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable = interface(TypedEventHandler_2__IDispatcherQueueTimer__IInspectable_Delegate_Base)
  ['{8A13AE56-7643-5F25-A347-5C9F548273DC}']
  end;

  // DualAPI Interface
  // Windows.System.IDispatcherQueue
  [WinRTClassNameAttribute(SWindows_System_DispatcherQueue)]
  IDispatcherQueue = interface(IInspectable)
  ['{603E88E4-A338-4FFE-A457-A5CFB9CEB899}']
    function CreateTimer: IDispatcherQueueTimer; safecall;
    function TryEnqueue(callback: DispatcherQueueHandler): Boolean; overload; safecall;
    function TryEnqueue(priority: DispatcherQueuePriority; callback: DispatcherQueueHandler): Boolean; overload; safecall;
    function add_ShutdownStarting(handler: TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs): EventRegistrationToken; safecall;
    procedure remove_ShutdownStarting(token: EventRegistrationToken); safecall;
    function add_ShutdownCompleted(handler: TypedEventHandler_2__IDispatcherQueue__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ShutdownCompleted(token: EventRegistrationToken); safecall;
  end;

  // Windows.System.DispatcherQueueHandler
  // External 
  DispatcherQueueHandler = interface(IUnknown)
  ['{DFA2DC9C-1A2D-4917-98F2-939AF1D6E0C8}']
    procedure Invoke; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Windows.System.IDispatcherQueueShutdownStartingEventArgs>
  TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs_Delegate_Base = interface(IUnknown)
  ['{B58B5E24-E1C6-528E-9D99-07EC8829DEA5}']
    procedure Invoke(sender: IDispatcherQueue; args: IDispatcherQueueShutdownStartingEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Windows.System.IDispatcherQueueShutdownStartingEventArgs>
  // External 
  TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs = interface(TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs_Delegate_Base)
  ['{88AA433B-5865-5CFC-B469-07EEC9762F68}']
  end;

  // Windows.System.IDispatcherQueueShutdownStartingEventArgs
  // External 
  IDispatcherQueueShutdownStartingEventArgs = interface(IInspectable)
  ['{C4724C4C-FF97-40C0-A226-CC0AAA545E89}']
    function GetDeferral: IDeferral; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Object>
  TypedEventHandler_2__IDispatcherQueue__IInspectable_Delegate_Base = interface(IUnknown)
  ['{FE79F855-2F40-5B88-A0C3-4C042A05DD05}']
    procedure Invoke(sender: IDispatcherQueue; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Object>
  // External 
  TypedEventHandler_2__IDispatcherQueue__IInspectable = interface(TypedEventHandler_2__IDispatcherQueue__IInspectable_Delegate_Base)
  ['{1ECC7D76-D5F1-5514-8DA3-343E7A82F842}']
  end;

  // Windows.Foundation.IReference`1<UInt32>
  // External 
  IReference_1__Cardinal = interface(IInspectable)
  ['{513EF3AF-E784-5325-A91E-97C2B8111CF3}']
    function get_Value: Cardinal; safecall;
    property Value: Cardinal read get_Value;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,String>
  // External 
  IKeyValuePair_2__HSTRING__HSTRING = interface(IInspectable)
  ['{60310303-49C5-52E6-ABC6-A9B36ECCC716}']
    function get_Key: HSTRING; safecall;
    function get_Value: HSTRING; safecall;
    property Key: HSTRING read get_Key;
    property Value: HSTRING read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterator_1__IKeyValuePair_2__HSTRING__HSTRING_Base = interface(IInspectable)
  ['{05EB86F1-7140-5517-B88D-CBAEBE57E6B1}']
    function get_Current: IKeyValuePair_2__HSTRING__HSTRING; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__HSTRING): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__HSTRING read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  // External 
  IIterator_1__IKeyValuePair_2__HSTRING__HSTRING = interface(IIterator_1__IKeyValuePair_2__HSTRING__HSTRING_Base)
  ['{05EB86F1-7140-5517-B88D-CBAEBE57E6B1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,String>
  IMapView_2__HSTRING__HSTRING_Base = interface(IInspectable)
  ['{AC7F26F2-FEB7-5B2A-8AC4-345BC62CAEDE}']
    function Lookup(key: HSTRING): HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__HSTRING; out second: IMapView_2__HSTRING__HSTRING); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,String>
  // External 
  IMapView_2__HSTRING__HSTRING = interface(IMapView_2__HSTRING__HSTRING_Base)
  ['{AC7F26F2-FEB7-5B2A-8AC4-345BC62CAEDE}']
  end;

  // Windows.ApplicationModel.IAppDisplayInfo
  // External 
  IAppDisplayInfo = interface(IInspectable)
  ['{1AEB1103-E4D4-41AA-A4F6-C4A276E79EAC}']
    function get_DisplayName: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function GetLogo(size: TSizeF): IRandomAccessStreamReference; safecall;
    property Description: HSTRING read get_Description;
    property DisplayName: HSTRING read get_DisplayName;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Core.ICoreApplicationView,Windows.ApplicationModel.Activation.IActivatedEventArgs>
  TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{CF193A96-EB13-5E3B-8BDF-87B6EFAE8339}']
    procedure Invoke(sender: ICoreApplicationView; args: Activation_IActivatedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Core.ICoreApplicationView,Windows.ApplicationModel.Activation.IActivatedEventArgs>
  // External 
  TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs = interface(TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs_Delegate_Base)
  ['{11DE1090-7AC7-59D2-AE00-981268883D85}']
  end;

  // Windows.ApplicationModel.Core.ICoreApplicationView
  // External 
  ICoreApplicationView = interface(IInspectable)
  ['{638BB2DB-451D-4661-B099-414F34FFB9F1}']
    function get_CoreWindow: ICoreWindow; safecall;
    function add_Activated(handler: TypedEventHandler_2__ICoreApplicationView__Activation_IActivatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Activated(token: EventRegistrationToken); safecall;
    function get_IsMain: Boolean; safecall;
    function get_IsHosted: Boolean; safecall;
    property CoreWindow: ICoreWindow read get_CoreWindow;
    property IsHosted: Boolean read get_IsHosted;
    property IsMain: Boolean read get_IsMain;
  end;

  // Windows.UI.Core.ICoreWindow
  // External 
  ICoreWindow = interface(IInspectable)
  ['{79B9D5F2-879E-4B89-B798-79E47598030C}']
    function get_AutomationHostProvider: IInspectable; safecall;
    function get_Bounds: TRectF; safecall;
    function get_CustomProperties: IPropertySet; safecall;
    function get_Dispatcher: ICoreDispatcher; safecall;
    function get_FlowDirection: CoreWindowFlowDirection; safecall;
    procedure put_FlowDirection(value: CoreWindowFlowDirection); safecall;
    function get_IsInputEnabled: Boolean; safecall;
    procedure put_IsInputEnabled(value: Boolean); safecall;
    function get_PointerCursor: ICoreCursor; safecall;
    procedure put_PointerCursor(value: ICoreCursor); safecall;
    function get_PointerPosition: TPointF; safecall;
    function get_Visible: Boolean; safecall;
    procedure Activate; safecall;
    procedure Close; safecall;
    function GetAsyncKeyState(virtualKey: VirtualKey): CoreVirtualKeyStates; safecall;
    function GetKeyState(virtualKey: VirtualKey): CoreVirtualKeyStates; safecall;
    procedure ReleasePointerCapture; safecall;
    procedure SetPointerCapture; safecall;
    function add_Activated(handler: TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Activated(cookie: EventRegistrationToken); safecall;
    function add_AutomationProviderRequested(handler: TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AutomationProviderRequested(cookie: EventRegistrationToken); safecall;
    function add_CharacterReceived(handler: TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_CharacterReceived(cookie: EventRegistrationToken); safecall;
    function add_Closed(handler: TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closed(cookie: EventRegistrationToken); safecall;
    function add_InputEnabled(handler: TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs): EventRegistrationToken; safecall;
    procedure remove_InputEnabled(cookie: EventRegistrationToken); safecall;
    function add_KeyDown(handler: TypedEventHandler_2__ICoreWindow__IKeyEventArgs): EventRegistrationToken; safecall;
    procedure remove_KeyDown(cookie: EventRegistrationToken); safecall;
    function add_KeyUp(handler: TypedEventHandler_2__ICoreWindow__IKeyEventArgs): EventRegistrationToken; safecall;
    procedure remove_KeyUp(cookie: EventRegistrationToken); safecall;
    function add_PointerCaptureLost(handler: TypedEventHandler_2__ICoreWindow__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerCaptureLost(cookie: EventRegistrationToken); safecall;
    function add_PointerEntered(handler: TypedEventHandler_2__ICoreWindow__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerEntered(cookie: EventRegistrationToken); safecall;
    function add_PointerExited(handler: TypedEventHandler_2__ICoreWindow__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerExited(cookie: EventRegistrationToken); safecall;
    function add_PointerMoved(handler: TypedEventHandler_2__ICoreWindow__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerMoved(cookie: EventRegistrationToken); safecall;
    function add_PointerPressed(handler: TypedEventHandler_2__ICoreWindow__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerPressed(cookie: EventRegistrationToken); safecall;
    function add_PointerReleased(handler: TypedEventHandler_2__ICoreWindow__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerReleased(cookie: EventRegistrationToken); safecall;
    function add_TouchHitTesting(handler: TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs): EventRegistrationToken; safecall;
    procedure remove_TouchHitTesting(cookie: EventRegistrationToken); safecall;
    function add_PointerWheelChanged(handler: TypedEventHandler_2__ICoreWindow__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerWheelChanged(cookie: EventRegistrationToken); safecall;
    function add_SizeChanged(handler: TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_SizeChanged(cookie: EventRegistrationToken); safecall;
    function add_VisibilityChanged(handler: TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_VisibilityChanged(cookie: EventRegistrationToken); safecall;
    property AutomationHostProvider: IInspectable read get_AutomationHostProvider;
    property Bounds: TRectF read get_Bounds;
    property CustomProperties: IPropertySet read get_CustomProperties;
    property Dispatcher: ICoreDispatcher read get_Dispatcher;
    property FlowDirection: CoreWindowFlowDirection read get_FlowDirection write put_FlowDirection;
    property IsInputEnabled: Boolean read get_IsInputEnabled write put_IsInputEnabled;
    property PointerCursor: ICoreCursor read get_PointerCursor write put_PointerCursor;
    property PointerPosition: TPointF read get_PointerPosition;
    property Visible: Boolean read get_Visible;
  end;

  // Windows.UI.Core.ICoreDispatcher
  // External 
  ICoreDispatcher = interface(IInspectable)
  ['{60DB2FA8-B705-4FDE-A7D6-EBBB1891D39E}']
    function get_HasThreadAccess: Boolean; safecall;
    procedure ProcessEvents(options: CoreProcessEventsOption); safecall;
    function RunAsync(priority: CoreDispatcherPriority; agileCallback: DispatchedHandler): IAsyncAction; safecall;
    function RunIdleAsync(agileCallback: IdleDispatchedHandler): IAsyncAction; safecall;
    property HasThreadAccess: Boolean read get_HasThreadAccess;
  end;

  // Windows.UI.Core.DispatchedHandler
  // External 
  DispatchedHandler = interface(IUnknown)
  ['{D1F276C4-98D8-4636-BF49-EB79507548E9}']
    procedure Invoke; safecall;
  end;

  // Windows.UI.Core.IdleDispatchedHandler
  // External 
  IdleDispatchedHandler = interface(IUnknown)
  ['{A42B0C24-7F21-4ABC-99C1-8F01007F0880}']
    procedure Invoke(e: IIdleDispatchedHandlerArgs); safecall;
  end;

  // Windows.UI.Core.IIdleDispatchedHandlerArgs
  // External 
  IIdleDispatchedHandlerArgs = interface(IInspectable)
  ['{98BB6A24-DC1C-43CB-B4ED-D1C0EB2391F3}']
    function get_IsDispatcherIdle: Boolean; safecall;
    property IsDispatcherIdle: Boolean read get_IsDispatcherIdle;
  end;

  // Windows.UI.Core.ICoreCursor
  // External 
  ICoreCursor = interface(IInspectable)
  ['{96893ACF-111D-442C-8A77-B87992F8E2D6}']
    function get_Id: Cardinal; safecall;
    function get_Type: CoreCursorType; safecall;
    property Id: Cardinal read get_Id;
    property &Type: CoreCursorType read get_Type;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IWindowActivatedEventArgs>
  TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{26A73B35-A7F9-52DB-88D6-15726DEB2523}']
    procedure Invoke(sender: ICoreWindow; args: IWindowActivatedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IWindowActivatedEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs = interface(TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs_Delegate_Base)
  ['{DEBE997D-E6F5-55A7-B1ED-DE68DBBF7499}']
  end;

  // Windows.UI.Core.IWindowActivatedEventArgs
  // External 
  IWindowActivatedEventArgs = interface(IInspectable)
  ['{179D65E7-4658-4CB6-AA13-41D094EA255E}']
    function get_WindowActivationState: CoreWindowActivationState; safecall;
    property WindowActivationState: CoreWindowActivationState read get_WindowActivationState;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IAutomationProviderRequestedEventArgs>
  TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{54DB5C04-81F7-5F46-9FB8-E49BEEC70A24}']
    procedure Invoke(sender: ICoreWindow; args: IAutomationProviderRequestedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IAutomationProviderRequestedEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs = interface(TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs_Delegate_Base)
  ['{4C101D07-8BE3-599C-962E-E0E5172AEB71}']
  end;

  // Windows.UI.Core.IAutomationProviderRequestedEventArgs
  // External 
  IAutomationProviderRequestedEventArgs = interface(IInspectable)
  ['{961FF258-21BF-4B42-A298-FA479D4C52E2}']
    function get_AutomationProvider: IInspectable; safecall;
    procedure put_AutomationProvider(value: IInspectable); safecall;
    property AutomationProvider: IInspectable read get_AutomationProvider write put_AutomationProvider;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ICharacterReceivedEventArgs>
  TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{73F846A8-0AF6-5872-8FB8-AE2F56D8553E}']
    procedure Invoke(sender: ICoreWindow; args: ICharacterReceivedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ICharacterReceivedEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs = interface(TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs_Delegate_Base)
  ['{BCCC3656-64A2-59C5-BBD6-30222BBA45E2}']
  end;

  // Windows.UI.Core.ICharacterReceivedEventArgs
  // External 
  ICharacterReceivedEventArgs = interface(IInspectable)
  ['{C584659F-99B2-4BCC-BD33-04E63F42902E}']
    function get_KeyCode: Cardinal; safecall;
    function get_KeyStatus: CorePhysicalKeyStatus; safecall;
    property KeyCode: Cardinal read get_KeyCode;
    property KeyStatus: CorePhysicalKeyStatus read get_KeyStatus;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ICoreWindowEventArgs>
  TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs_Delegate_Base = interface(IUnknown)
  ['{D08E4F66-3457-57F2-BA0C-CB347133BD15}']
    procedure Invoke(sender: ICoreWindow; args: ICoreWindowEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ICoreWindowEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs = interface(TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs_Delegate_Base)
  ['{8313079D-940D-5497-B414-EAEFB4BFB117}']
  end;

  // Windows.UI.Core.ICoreWindowEventArgs
  // External 
  ICoreWindowEventArgs = interface(IInspectable)
  ['{272B1EF3-C633-4DA5-A26C-C6D0F56B29DA}']
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    property Handled: Boolean read get_Handled write put_Handled;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IInputEnabledEventArgs>
  TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs_Delegate_Base = interface(IUnknown)
  ['{E230A64A-506A-59C3-BB61-5559FF995663}']
    procedure Invoke(sender: ICoreWindow; args: IInputEnabledEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IInputEnabledEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs = interface(TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs_Delegate_Base)
  ['{612B1DC7-D539-508E-BA99-EA96D28C773C}']
  end;

  // Windows.UI.Core.IInputEnabledEventArgs
  // External 
  IInputEnabledEventArgs = interface(IInspectable)
  ['{80371D4F-2FD8-4C24-AA86-3163A87B4E5A}']
    function get_InputEnabled: Boolean; safecall;
    property InputEnabled: Boolean read get_InputEnabled;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IKeyEventArgs>
  TypedEventHandler_2__ICoreWindow__IKeyEventArgs_Delegate_Base = interface(IUnknown)
  ['{A3EC0774-55AC-5D61-8232-B35C5D35C93C}']
    procedure Invoke(sender: ICoreWindow; args: IKeyEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IKeyEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__IKeyEventArgs = interface(TypedEventHandler_2__ICoreWindow__IKeyEventArgs_Delegate_Base)
  ['{6F418E8D-1111-5A06-B7E2-357E0F99D693}']
  end;

  // Windows.UI.Core.IKeyEventArgs
  // External 
  IKeyEventArgs = interface(IInspectable)
  ['{5FF5E930-2544-4A17-BD78-1F2FDEBB106B}']
    function get_VirtualKey: VirtualKey; safecall;
    function get_KeyStatus: CorePhysicalKeyStatus; safecall;
    property KeyStatus: CorePhysicalKeyStatus read get_KeyStatus;
    property VirtualKey_: VirtualKey read get_VirtualKey;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__ICoreWindow__IPointerEventArgs_Delegate_Base = interface(IUnknown)
  ['{420E1BB6-E99D-5E64-8E25-07467E3CAE9E}']
    procedure Invoke(sender: ICoreWindow; args: IPointerEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IPointerEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__IPointerEventArgs = interface(TypedEventHandler_2__ICoreWindow__IPointerEventArgs_Delegate_Base)
  ['{CA41B9D7-6784-528C-B771-3BD8AEBA67DB}']
  end;

  // Windows.UI.Core.IPointerEventArgs
  // External 
  IPointerEventArgs = interface(IInspectable)
  ['{920D9CB1-A5FC-4A21-8C09-49DFE6FFE25F}']
    function get_CurrentPoint: IPointerPoint; safecall;
    function get_KeyModifiers: VirtualKeyModifiers; safecall;
    function GetIntermediatePoints: IVector_1__IPointerPoint; safecall;
    property CurrentPoint: IPointerPoint read get_CurrentPoint;
    property KeyModifiers: VirtualKeyModifiers read get_KeyModifiers;
  end;

  // Windows.UI.Input.IPointerPoint
  // External 
  IPointerPoint = interface(IInspectable)
  ['{E995317D-7296-42D9-8233-C5BE73B74A4A}']
    function get_PointerDevice: Input_IPointerDevice; safecall;
    function get_Position: TPointF; safecall;
    function get_RawPosition: TPointF; safecall;
    function get_PointerId: Cardinal; safecall;
    function get_FrameId: Cardinal; safecall;
    function get_Timestamp: UInt64; safecall;
    function get_IsInContact: Boolean; safecall;
    function get_Properties: IPointerPointProperties; safecall;
    property FrameId: Cardinal read get_FrameId;
    property IsInContact: Boolean read get_IsInContact;
    property PointerDevice: Input_IPointerDevice read get_PointerDevice;
    property PointerId: Cardinal read get_PointerId;
    property Position: TPointF read get_Position;
    property Properties: IPointerPointProperties read get_Properties;
    property RawPosition: TPointF read get_RawPosition;
    property Timestamp: UInt64 read get_Timestamp;
  end;

  // Windows.Devices.Input.IPointerDevice
  // External 
  Input_IPointerDevice = interface(IInspectable)
  ['{93C9BAFC-EBCB-467E-82C6-276FEAE36B5A}']
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_IsIntegrated: Boolean; safecall;
    function get_MaxContacts: Cardinal; safecall;
    function get_PhysicalDeviceRect: TRectF; safecall;
    function get_ScreenRect: TRectF; safecall;
    function get_SupportedUsages: IVectorView_1__Input_PointerDeviceUsage; safecall;
    property IsIntegrated: Boolean read get_IsIntegrated;
    property MaxContacts: Cardinal read get_MaxContacts;
    property PhysicalDeviceRect: TRectF read get_PhysicalDeviceRect;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
    property ScreenRect: TRectF read get_ScreenRect;
    property SupportedUsages: IVectorView_1__Input_PointerDeviceUsage read get_SupportedUsages;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Input.PointerDeviceUsage>
  // External 
  IVectorView_1__Input_PointerDeviceUsage = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Input_PointerDeviceUsage; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_PointerDeviceUsage; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_PointerDeviceUsage): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Input.IPointerPointProperties
  // External 
  IPointerPointProperties = interface(IInspectable)
  ['{C79D8A4B-C163-4EE7-803F-67CE79F9972D}']
    function get_Pressure: Single; safecall;
    function get_IsInverted: Boolean; safecall;
    function get_IsEraser: Boolean; safecall;
    function get_Orientation: Single; safecall;
    function get_XTilt: Single; safecall;
    function get_YTilt: Single; safecall;
    function get_Twist: Single; safecall;
    function get_ContactRect: TRectF; safecall;
    function get_ContactRectRaw: TRectF; safecall;
    function get_TouchConfidence: Boolean; safecall;
    function get_IsLeftButtonPressed: Boolean; safecall;
    function get_IsRightButtonPressed: Boolean; safecall;
    function get_IsMiddleButtonPressed: Boolean; safecall;
    function get_MouseWheelDelta: Integer; safecall;
    function get_IsHorizontalMouseWheel: Boolean; safecall;
    function get_IsPrimary: Boolean; safecall;
    function get_IsInRange: Boolean; safecall;
    function get_IsCanceled: Boolean; safecall;
    function get_IsBarrelButtonPressed: Boolean; safecall;
    function get_IsXButton1Pressed: Boolean; safecall;
    function get_IsXButton2Pressed: Boolean; safecall;
    function get_PointerUpdateKind: PointerUpdateKind; safecall;
    function HasUsage(usagePage: Cardinal; usageId: Cardinal): Boolean; safecall;
    function GetUsageValue(usagePage: Cardinal; usageId: Cardinal): Integer; safecall;
    property ContactRect: TRectF read get_ContactRect;
    property ContactRectRaw: TRectF read get_ContactRectRaw;
    property IsBarrelButtonPressed: Boolean read get_IsBarrelButtonPressed;
    property IsCanceled: Boolean read get_IsCanceled;
    property IsEraser: Boolean read get_IsEraser;
    property IsHorizontalMouseWheel: Boolean read get_IsHorizontalMouseWheel;
    property IsInRange: Boolean read get_IsInRange;
    property IsInverted: Boolean read get_IsInverted;
    property IsLeftButtonPressed: Boolean read get_IsLeftButtonPressed;
    property IsMiddleButtonPressed: Boolean read get_IsMiddleButtonPressed;
    property IsPrimary: Boolean read get_IsPrimary;
    property IsRightButtonPressed: Boolean read get_IsRightButtonPressed;
    property IsXButton1Pressed: Boolean read get_IsXButton1Pressed;
    property IsXButton2Pressed: Boolean read get_IsXButton2Pressed;
    property MouseWheelDelta: Integer read get_MouseWheelDelta;
    property Orientation: Single read get_Orientation;
    property PointerUpdateKind_: PointerUpdateKind read get_PointerUpdateKind;
    property Pressure: Single read get_Pressure;
    property TouchConfidence: Boolean read get_TouchConfidence;
    property Twist: Single read get_Twist;
    property XTilt: Single read get_XTilt;
    property YTilt: Single read get_YTilt;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Input.IPointerPoint>
  IVector_1__IPointerPoint_Base = interface(IInspectable)
  ['{DFA655CF-FDE7-5048-B4BF-C909231B7EDB}']
    function GetAt(index: Cardinal): IPointerPoint; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IPointerPoint; safecall;
    function IndexOf(value: IPointerPoint; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IPointerPoint); safecall;
    procedure InsertAt(index: Cardinal; value: IPointerPoint); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IPointerPoint); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPointerPoint): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIPointerPoint); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Input.IPointerPoint>
  // External 
  IVector_1__IPointerPoint = interface(IVector_1__IPointerPoint_Base)
  ['{73154191-695C-5F04-9D43-911CB8336411}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.IPointerPoint>
  // External 
  IVectorView_1__IPointerPoint = interface(IInspectable)
  ['{1697E0A0-DFE4-5BAE-AC51-F4C43FCEF82B}']
    function GetAt(index: Cardinal): IPointerPoint; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IPointerPoint; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPointerPoint): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ITouchHitTestingEventArgs>
  TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs_Delegate_Base = interface(IUnknown)
  ['{197654C9-0C47-502B-9AA1-0DEB03ED9702}']
    procedure Invoke(sender: ICoreWindow; args: ITouchHitTestingEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.ITouchHitTestingEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs = interface(TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs_Delegate_Base)
  ['{4A977150-92AB-5A61-9150-21F08B3AEF81}']
  end;

  // Windows.UI.Core.ITouchHitTestingEventArgs
  // External 
  ITouchHitTestingEventArgs = interface(IInspectable)
  ['{22F3B823-0B7C-424E-9DF7-33D4F962931B}']
    function get_ProximityEvaluation: CoreProximityEvaluation; safecall;
    procedure put_ProximityEvaluation(value: CoreProximityEvaluation); safecall;
    function get_Point: TPointF; safecall;
    function get_BoundingBox: TRectF; safecall;
    function EvaluateProximity(controlBoundingBox: TRectF): CoreProximityEvaluation; overload; safecall;
    function EvaluateProximity(controlVerticesSize: Cardinal; controlVertices: PPointF): CoreProximityEvaluation; overload; safecall;
    property BoundingBox: TRectF read get_BoundingBox;
    property Point: TPointF read get_Point;
    property ProximityEvaluation: CoreProximityEvaluation read get_ProximityEvaluation write put_ProximityEvaluation;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IWindowSizeChangedEventArgs>
  TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{318DBB67-4089-5690-9040-1D454FB2F686}']
    procedure Invoke(sender: ICoreWindow; args: IWindowSizeChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IWindowSizeChangedEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs = interface(TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs_Delegate_Base)
  ['{44420CB4-1853-5BB7-A73B-2701EF06DC36}']
  end;

  // Windows.UI.Core.IWindowSizeChangedEventArgs
  // External 
  IWindowSizeChangedEventArgs = interface(IInspectable)
  ['{5A200EC7-0426-47DC-B86C-6F475915E451}']
    function get_Size: TSizeF; safecall;
    property Size: TSizeF read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IVisibilityChangedEventArgs>
  TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{14B7F692-5583-52A1-AA42-FC1843C0F748}']
    procedure Invoke(sender: ICoreWindow; args: IVisibilityChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Windows.UI.Core.IVisibilityChangedEventArgs>
  // External 
  TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs = interface(TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs_Delegate_Base)
  ['{C57E132B-9D39-58F7-A15C-2F12895CF858}']
  end;

  // Windows.UI.Core.IVisibilityChangedEventArgs
  // External 
  IVisibilityChangedEventArgs = interface(IInspectable)
  ['{BF9918EA-D801-4564-A495-B1E84F8AD085}']
    function get_Visible: Boolean; safecall;
    property Visible: Boolean read get_Visible;
  end;

  // Windows.ApplicationModel.Activation.IActivatedEventArgs
  // External 
  Activation_IActivatedEventArgs = interface(IInspectable)
  ['{CF651713-CD08-4FD8-B697-A281B6544E2E}']
    function get_Kind: Activation_ActivationKind; safecall;
    function get_PreviousExecutionState: Activation_ApplicationExecutionState; safecall;
    function get_SplashScreen: Activation_ISplashScreen; safecall;
    property Kind: Activation_ActivationKind read get_Kind;
    property PreviousExecutionState: Activation_ApplicationExecutionState read get_PreviousExecutionState;
    property SplashScreen: Activation_ISplashScreen read get_SplashScreen;
  end;

  // Windows.ApplicationModel.Activation.ISplashScreen
  // External 
  Activation_ISplashScreen = interface(IInspectable)
  ['{CA4D975C-D4D6-43F0-97C0-0833C6391C24}']
    function get_ImageLocation: TRectF; safecall;
    function add_Dismissed(handler: TypedEventHandler_2__Activation_ISplashScreen__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Dismissed(cookie: EventRegistrationToken); safecall;
    property ImageLocation: TRectF read get_ImageLocation;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable_Delegate_Base = interface(IUnknown)
  ['{7725B2A5-287D-5ED2-A789-2A6A2673C7FE}']
    procedure Invoke(sender: Activation_ISplashScreen; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  // External 
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface(TypedEventHandler_2__Activation_ISplashScreen__IInspectable_Delegate_Base)
  ['{359B8887-2FA6-5405-A4AF-642C9FDACC93}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Object>
  TypedEventHandler_2__IDataPackage__IInspectable_Delegate_Base = interface(IUnknown)
  ['{C156B0C3-1CBC-5CA4-901C-62C5A8CA5CB5}']
    procedure Invoke(sender: IDataPackage; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Object>
  // External 
  TypedEventHandler_2__IDataPackage__IInspectable = interface(TypedEventHandler_2__IDataPackage__IInspectable_Delegate_Base)
  ['{FFA86A6A-1BEE-540E-9911-7272C487A1ED}']
  end;

  // DualAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataPackage
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DataPackage)]
  IDataPackage = interface(IInspectable)
  ['{61EBF5C7-EFEA-4346-9554-981D7E198FFE}']
    function GetView: IDataPackageView; safecall;
    function get_Properties: IDataPackagePropertySet; safecall;
    function get_RequestedOperation: DataPackageOperation; safecall;
    procedure put_RequestedOperation(value: DataPackageOperation); safecall;
    function add_OperationCompleted(handler: TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_OperationCompleted(token: EventRegistrationToken); safecall;
    function add_Destroyed(handler: TypedEventHandler_2__IDataPackage__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Destroyed(token: EventRegistrationToken); safecall;
    procedure SetData(formatId: HSTRING; value: IInspectable); safecall;
    procedure SetDataProvider(formatId: HSTRING; delayRenderer: DataProviderHandler); safecall;
    procedure SetText(value: HSTRING); safecall;
    procedure SetUri(value: IUriRuntimeClass); safecall;
    procedure SetHtmlFormat(value: HSTRING); safecall;
    function get_ResourceMap: IMap_2__HSTRING__IRandomAccessStreamReference; safecall;
    procedure SetRtf(value: HSTRING); safecall;
    procedure SetBitmap(value: IRandomAccessStreamReference); safecall;
    procedure SetStorageItems(value: IIterable_1__IStorageItem); overload; safecall;
    procedure SetStorageItems(value: IIterable_1__IStorageItem; readOnly: Boolean); overload; safecall;
    property Properties: IDataPackagePropertySet read get_Properties;
    property RequestedOperation: DataPackageOperation read get_RequestedOperation write put_RequestedOperation;
    property ResourceMap: IMap_2__HSTRING__IRandomAccessStreamReference read get_ResourceMap;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackageView
  // External 
  IDataPackageView = interface(IInspectable)
  ['{7B840471-5900-4D85-A90B-10CB85FE3552}']
    function get_Properties: IDataPackagePropertySetView; safecall;
    function get_RequestedOperation: DataPackageOperation; safecall;
    procedure ReportOperationCompleted(value: DataPackageOperation); safecall;
    function get_AvailableFormats: IVectorView_1__HSTRING; safecall;
    function Contains(formatId: HSTRING): Boolean; safecall;
    function GetDataAsync(formatId: HSTRING): IAsyncOperation_1__IInspectable; safecall;
    function GetTextAsync: IAsyncOperation_1__HSTRING; overload; safecall;
    function GetTextAsync(formatId: HSTRING): IAsyncOperation_1__HSTRING; overload; safecall;
    function GetUriAsync: IAsyncOperation_1__IUriRuntimeClass; safecall;
    function GetHtmlFormatAsync: IAsyncOperation_1__HSTRING; safecall;
    function GetResourceMapAsync: IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference; safecall;
    function GetRtfAsync: IAsyncOperation_1__HSTRING; safecall;
    function GetBitmapAsync: IAsyncOperation_1__IRandomAccessStreamReference; safecall;
    function GetStorageItemsAsync: IAsyncOperation_1__IVectorView_1__IStorageItem; safecall;
    property AvailableFormats: IVectorView_1__HSTRING read get_AvailableFormats;
    property Properties: IDataPackagePropertySetView read get_Properties;
    property RequestedOperation: DataPackageOperation read get_RequestedOperation;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView
  // External 
  IDataPackagePropertySetView = interface(IInspectable)
  ['{B94CEC01-0C1A-4C57-BE55-75D01289735D}']
    function get_Title: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_Thumbnail: IRandomAccessStreamReference; safecall;
    function get_FileTypes: IVectorView_1__HSTRING; safecall;
    function get_ApplicationName: HSTRING; safecall;
    function get_ApplicationListingUri: IUriRuntimeClass; safecall;
    property ApplicationListingUri: IUriRuntimeClass read get_ApplicationListingUri;
    property ApplicationName: HSTRING read get_ApplicationName;
    property Description: HSTRING read get_Description;
    property FileTypes: IVectorView_1__HSTRING read get_FileTypes;
    property Thumbnail: IRandomAccessStreamReference read get_Thumbnail;
    property Title: HSTRING read get_Title;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Base = interface(IInspectable)
  ['{FC012D44-2DCF-5162-BE9A-7668675AA590}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference; safecall;
    function GetResults: IMapView_2__HSTRING__IRandomAccessStreamReference; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  // External 
  IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference = interface(IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Base)
  ['{D2D20F27-DBCC-571C-9ADF-DDE7A5AF53CA}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Delegate_Base = interface(IUnknown)
  ['{D4CB6B80-821A-5A7B-898D-D58917B31A36}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  // External 
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference = interface(AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Delegate_Base)
  ['{2D6EFF82-24CB-5680-9184-AD687A705A8C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>
  IMapView_2__HSTRING__IRandomAccessStreamReference_Base = interface(IInspectable)
  ['{0A4CE7A5-DFE0-5796-A438-EFFDFAA31F1B}']
    function Lookup(key: HSTRING): IRandomAccessStreamReference; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IRandomAccessStreamReference; out second: IMapView_2__HSTRING__IRandomAccessStreamReference); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>
  // External 
  IMapView_2__HSTRING__IRandomAccessStreamReference = interface(IMapView_2__HSTRING__IRandomAccessStreamReference_Base)
  ['{20784CBD-B6FB-530A-B732-5375919F30B8}']
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet
  // External 
  IDataPackagePropertySet = interface(IInspectable)
  ['{CD1C93EB-4C4C-443A-A8D3-F5C241E91689}']
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_Description: HSTRING; safecall;
    procedure put_Description(value: HSTRING); safecall;
    function get_Thumbnail: IRandomAccessStreamReference; safecall;
    procedure put_Thumbnail(value: IRandomAccessStreamReference); safecall;
    function get_FileTypes: IVector_1__HSTRING; safecall;
    function get_ApplicationName: HSTRING; safecall;
    procedure put_ApplicationName(value: HSTRING); safecall;
    function get_ApplicationListingUri: IUriRuntimeClass; safecall;
    procedure put_ApplicationListingUri(value: IUriRuntimeClass); safecall;
    property ApplicationListingUri: IUriRuntimeClass read get_ApplicationListingUri write put_ApplicationListingUri;
    property ApplicationName: HSTRING read get_ApplicationName write put_ApplicationName;
    property Description: HSTRING read get_Description write put_Description;
    property FileTypes: IVector_1__HSTRING read get_FileTypes;
    property Thumbnail: IRandomAccessStreamReference read get_Thumbnail write put_Thumbnail;
    property Title: HSTRING read get_Title write put_Title;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Windows.ApplicationModel.DataTransfer.IOperationCompletedEventArgs>
  TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{DD48AF6C-EF9A-59CB-B326-57D9E2411F21}']
    procedure Invoke(sender: IDataPackage; args: IOperationCompletedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Windows.ApplicationModel.DataTransfer.IOperationCompletedEventArgs>
  // External 
  TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs = interface(TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs_Delegate_Base)
  ['{DE9E577F-9562-5D55-A33B-9EE39B3E5435}']
  end;

  // Windows.ApplicationModel.DataTransfer.IOperationCompletedEventArgs
  // External 
  IOperationCompletedEventArgs = interface(IInspectable)
  ['{E7AF329D-051D-4FAB-B1A9-47FD77F70A41}']
    function get_Operation: DataPackageOperation; safecall;
    property Operation: DataPackageOperation read get_Operation;
  end;

  // Windows.ApplicationModel.DataTransfer.DataProviderHandler
  // External 
  DataProviderHandler = interface(IUnknown)
  ['{E7ECD720-F2F4-4A2D-920E-170A2F482A27}']
    procedure Invoke(request: IDataProviderRequest); safecall;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataProviderRequest
  // External 
  IDataProviderRequest = interface(IInspectable)
  ['{EBBC7157-D3C8-47DA-ACDE-F82388D5F716}']
    function get_FormatId: HSTRING; safecall;
    function get_Deadline: DateTime; safecall;
    function GetDeferral: IDataProviderDeferral; safecall;
    procedure SetData(value: IInspectable); safecall;
    property Deadline: DateTime read get_Deadline;
    property FormatId: HSTRING read get_FormatId;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataProviderDeferral
  // External 
  IDataProviderDeferral = interface(IInspectable)
  ['{C2CF2373-2D26-43D9-B69D-DCB86D03F6DA}']
    procedure Complete; safecall;
  end;

  // Windows.Foundation.Collections.IMap`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>
  // External 
  IMap_2__HSTRING__IRandomAccessStreamReference = interface(IInspectable)
  ['{6C5B38B7-9D17-5B5F-A04F-3730A82FD9B8}']
    function Lookup(key: HSTRING): IRandomAccessStreamReference; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    function GetView: IMapView_2__HSTRING__IRandomAccessStreamReference; safecall;
    function Insert(key: HSTRING; value: IRandomAccessStreamReference): Boolean; safecall;
    procedure Remove(key: HSTRING); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageItem>
  IIterable_1__IStorageItem_Base = interface(IInspectable)
  ['{BB8B8418-65D1-544B-B083-6D172F568C73}']
    function First: IIterator_1__IStorageItem; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageItem>
  // External 
  IIterable_1__IStorageItem = interface(IIterable_1__IStorageItem_Base)
  ['{BB8B8418-65D1-544B-B083-6D172F568C73}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageItem>
  IIterator_1__IStorageItem_Base = interface(IInspectable)
  ['{05B487C2-3830-5D3C-98DA-25FA11542DBD}']
    function get_Current: IStorageItem; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStorageItem): Cardinal; safecall;
    property Current: IStorageItem read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageItem>
  // External 
  IIterator_1__IStorageItem = interface(IIterator_1__IStorageItem_Base)
  ['{05B487C2-3830-5D3C-98DA-25FA11542DBD}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable_Delegate_Base = interface(IUnknown)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
    procedure Invoke(sender: IInspectable; args: IInspectable); safecall;
  end;
  // Windows.Foundation.EventHandler`1<Object>
  // External 
  EventHandler_1__IInspectable = interface(EventHandler_1__IInspectable_Delegate_Base)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
  end;

  // Windows.Foundation.IReference`1<Int16>
  // External 
  IReference_1__SmallInt = interface(IInspectable)
  ['{6EC9E41B-6709-5647-9918-A1270110FC4E}']
    function get_Value: SmallInt; safecall;
    property Value: SmallInt read get_Value;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession,Object>
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable_Delegate_Base = interface(IUnknown)
  ['{6C7EC2EC-9F00-5EA0-9A08-60E5070BCF03}']
    procedure Invoke(sender: GenericAttributeProfile_IGattSession; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession,Object>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable = interface(TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable_Delegate_Base)
  ['{36D17BB4-1E2A-5A74-A1F1-A9D1347B7702}']
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_GenericAttributeProfile_GattSession)]
  GenericAttributeProfile_IGattSession = interface(IInspectable)
  ['{D23B5143-E04E-4C24-999C-9C256F9856B1}']
    function get_DeviceId: IBluetoothDeviceId; safecall;
    function get_CanMaintainConnection: Boolean; safecall;
    procedure put_MaintainConnection(value: Boolean); safecall;
    function get_MaintainConnection: Boolean; safecall;
    function get_MaxPduSize: Word; safecall;
    function get_SessionStatus: GenericAttributeProfile_GattSessionStatus; safecall;
    function add_MaxPduSizeChanged(handler: TypedEventHandler_2__GenericAttributeProfile_IGattSession__IInspectable): EventRegistrationToken; safecall;
    procedure remove_MaxPduSizeChanged(token: EventRegistrationToken); safecall;
    function add_SessionStatusChanged(handler: TypedEventHandler_2__GenericAttributeProfile_IGattSession__GenericAttributeProfile_IGattSessionStatusChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_SessionStatusChanged(token: EventRegistrationToken); safecall;
    property CanMaintainConnection: Boolean read get_CanMaintainConnection;
    property DeviceId: IBluetoothDeviceId read get_DeviceId;
    property MaintainConnection: Boolean read get_MaintainConnection write put_MaintainConnection;
    property MaxPduSize: Word read get_MaxPduSize;
    property SessionStatus: GenericAttributeProfile_GattSessionStatus read get_SessionStatus;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.IBluetoothDeviceId
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_BluetoothDeviceId)]
  IBluetoothDeviceId = interface(IInspectable)
  ['{C17949AF-57C1-4642-BCCE-E6C06B20AE76}']
    function get_Id: HSTRING; safecall;
    function get_IsClassicDevice: Boolean; safecall;
    function get_IsLowEnergyDevice: Boolean; safecall;
    property Id: HSTRING read get_Id;
    property IsClassicDevice: Boolean read get_IsClassicDevice;
    property IsLowEnergyDevice: Boolean read get_IsLowEnergyDevice;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSessionStatusChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__GenericAttributeProfile_IGattSessionStatusChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{0E1210F2-7B6F-543E-8ADB-A61D34AB535D}']
    procedure Invoke(sender: GenericAttributeProfile_IGattSession; args: GenericAttributeProfile_IGattSessionStatusChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSession,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSessionStatusChangedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattSession__GenericAttributeProfile_IGattSessionStatusChangedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattSession__GenericAttributeProfile_IGattSessionStatusChangedEventArgs_Delegate_Base)
  ['{D8E88F7A-28D3-54CD-A320-F0FE6F40A999}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSessionStatusChangedEventArgs
  // External 
  GenericAttributeProfile_IGattSessionStatusChangedEventArgs = interface(IInspectable)
  ['{7605B72E-837F-404C-AB34-3163F39DDF32}']
    function get_Error: BluetoothError; safecall;
    function get_Status: GenericAttributeProfile_GattSessionStatus; safecall;
    property Error: BluetoothError read get_Error;
    property Status: GenericAttributeProfile_GattSessionStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient,Object>
  TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable_Delegate_Base = interface(IUnknown)
  ['{9C17A110-806D-594B-B33D-ED280BBF27E5}']
    procedure Invoke(sender: GenericAttributeProfile_IGattSubscribedClient; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient,Object>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable = interface(TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable_Delegate_Base)
  ['{996904B7-B6DC-5C98-996D-4A3E110EFDA5}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient
  // External 
  GenericAttributeProfile_IGattSubscribedClient = interface(IInspectable)
  ['{736E9001-15A4-4EC2-9248-E3F20D463BE9}']
    function get_Session: GenericAttributeProfile_IGattSession; safecall;
    function get_MaxNotificationSize: Word; safecall;
    function add_MaxNotificationSizeChanged(handler: TypedEventHandler_2__GenericAttributeProfile_IGattSubscribedClient__IInspectable): EventRegistrationToken; safecall;
    procedure remove_MaxNotificationSizeChanged(token: EventRegistrationToken); safecall;
    property MaxNotificationSize: Word read get_MaxNotificationSize;
    property Session: GenericAttributeProfile_IGattSession read get_Session;
  end;

  // Windows.Foundation.IReference`1<UInt8>
  // External 
  IReference_1__Byte = interface(IInspectable)
  ['{E5198CC8-2873-55F5-B0A1-84FF9E4AAD62}']
    function get_Value: Byte; safecall;
    property Value: Byte read get_Value;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Object>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable_Delegate_Base = interface(IUnknown)
  ['{8087ACD6-AED7-53EB-9B23-4808BB910C17}']
    procedure Invoke(sender: GenericAttributeProfile_IGattLocalCharacteristic; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Object>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable = interface(TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable_Delegate_Base)
  ['{6FD0FBA3-A5CA-51A7-938F-310EDDB72631}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic
  // External 
  GenericAttributeProfile_IGattLocalCharacteristic = interface(IInspectable)
  ['{AEDE376D-5412-4D74-92A8-8DEB8526829C}']
    function get_Uuid: TGuid; safecall;
    function get_StaticValue: IBuffer; safecall;
    function get_CharacteristicProperties: GenericAttributeProfile_GattCharacteristicProperties; safecall;
    function get_ReadProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    function get_WriteProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    function CreateDescriptorAsync(descriptorUuid: TGuid; parameters: GenericAttributeProfile_IGattLocalDescriptorParameters): IAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult; safecall;
    function get_Descriptors: IVectorView_1__GenericAttributeProfile_IGattLocalDescriptor; safecall;
    function get_UserDescription: HSTRING; safecall;
    function get_PresentationFormats: IVectorView_1__GenericAttributeProfile_IGattPresentationFormat; safecall;
    function get_SubscribedClients: IVectorView_1__GenericAttributeProfile_IGattSubscribedClient; safecall;
    function add_SubscribedClientsChanged(handler: TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__IInspectable): EventRegistrationToken; safecall;
    procedure remove_SubscribedClientsChanged(token: EventRegistrationToken); safecall;
    function add_ReadRequested(handler: TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattReadRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ReadRequested(token: EventRegistrationToken); safecall;
    function add_WriteRequested(handler: TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattWriteRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_WriteRequested(token: EventRegistrationToken); safecall;
    function NotifyValueAsync(value: IBuffer): IAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult; overload; safecall;
    function NotifyValueAsync(value: IBuffer; subscribedClient: GenericAttributeProfile_IGattSubscribedClient): IAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult; overload; safecall;
    property CharacteristicProperties: GenericAttributeProfile_GattCharacteristicProperties read get_CharacteristicProperties;
    property Descriptors: IVectorView_1__GenericAttributeProfile_IGattLocalDescriptor read get_Descriptors;
    property PresentationFormats: IVectorView_1__GenericAttributeProfile_IGattPresentationFormat read get_PresentationFormats;
    property ReadProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_ReadProtectionLevel;
    property StaticValue: IBuffer read get_StaticValue;
    property SubscribedClients: IVectorView_1__GenericAttributeProfile_IGattSubscribedClient read get_SubscribedClients;
    property UserDescription: HSTRING read get_UserDescription;
    property Uuid: TGuid read get_Uuid;
    property WriteProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_WriteProtectionLevel;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult_Base = interface(IInspectable)
  ['{3EF6D808-754F-5040-97AC-0703309C574F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult; safecall;
    function GetResults: GenericAttributeProfile_IGattLocalDescriptorResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult>
  // External 
  IAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult = interface(IAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult_Base)
  ['{5AA981DA-EB28-5CE4-A874-3B103786518A}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult_Delegate_Base = interface(IUnknown)
  ['{F2927EEC-47D9-5338-9BA5-5BE8461AD410}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GenericAttributeProfile_IGattLocalDescriptorResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult>
  // External 
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult = interface(AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattLocalDescriptorResult_Delegate_Base)
  ['{CC95F9AF-E0FC-543C-8E84-61730CC57C28}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorResult
  // External 
  GenericAttributeProfile_IGattLocalDescriptorResult = interface(IInspectable)
  ['{375791BE-321F-4366-BFC1-3BC6B82C79F8}']
    function get_Descriptor: GenericAttributeProfile_IGattLocalDescriptor; safecall;
    function get_Error: BluetoothError; safecall;
    property Descriptor: GenericAttributeProfile_IGattLocalDescriptor read get_Descriptor;
    property Error: BluetoothError read get_Error;
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor
  // External 
  GenericAttributeProfile_IGattLocalDescriptor = interface(IInspectable)
  ['{F48EBE06-789D-4A4B-8652-BD017B5D2FC6}']
    function get_Uuid: TGuid; safecall;
    function get_StaticValue: IBuffer; safecall;
    function get_ReadProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    function get_WriteProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    function add_ReadRequested(handler: TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattReadRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ReadRequested(token: EventRegistrationToken); safecall;
    function add_WriteRequested(handler: TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattWriteRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_WriteRequested(token: EventRegistrationToken); safecall;
    property ReadProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_ReadProtectionLevel;
    property StaticValue: IBuffer read get_StaticValue;
    property Uuid: TGuid read get_Uuid;
    property WriteProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_WriteProtectionLevel;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattReadRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{252DCA5C-86E7-5BE1-AEED-F78C6ED466AB}']
    procedure Invoke(sender: GenericAttributeProfile_IGattLocalDescriptor; args: GenericAttributeProfile_IGattReadRequestedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattReadRequestedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattReadRequestedEventArgs_Delegate_Base)
  ['{563959F3-47CA-5E09-891E-C70420861446}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs
  // External 
  GenericAttributeProfile_IGattReadRequestedEventArgs = interface(IInspectable)
  ['{93497243-F39C-484B-8AB6-996BA486CFA3}']
    function get_Session: GenericAttributeProfile_IGattSession; safecall;
    function GetDeferral: IDeferral; safecall;
    function GetRequestAsync: IAsyncOperation_1__GenericAttributeProfile_IGattReadRequest; safecall;
    property Session: GenericAttributeProfile_IGattSession read get_Session;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest>
  IAsyncOperation_1__GenericAttributeProfile_IGattReadRequest_Base = interface(IInspectable)
  ['{4732CEC2-D943-5CEB-8281-8D54A21B9A45}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest; safecall;
    function GetResults: GenericAttributeProfile_IGattReadRequest; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest>
  // External 
  IAsyncOperation_1__GenericAttributeProfile_IGattReadRequest = interface(IAsyncOperation_1__GenericAttributeProfile_IGattReadRequest_Base)
  ['{15F1BC63-05AD-5B59-8749-F13D89A4D107}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest_Delegate_Base = interface(IUnknown)
  ['{31823848-3AB2-547A-8303-964DCC377C9C}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GenericAttributeProfile_IGattReadRequest; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest>
  // External 
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest = interface(AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadRequest_Delegate_Base)
  ['{79E10863-9602-5F2D-9C60-7ADD53B24F77}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest
  // External 
  GenericAttributeProfile_IGattReadRequest = interface(IInspectable)
  ['{F1DD6535-6ACD-42A6-A4BB-D789DAE0043E}']
    function get_Offset: Cardinal; safecall;
    function get_Length: Cardinal; safecall;
    function get_State: GenericAttributeProfile_GattRequestState; safecall;
    function add_StateChanged(handler: TypedEventHandler_2__GenericAttributeProfile_IGattReadRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StateChanged(token: EventRegistrationToken); safecall;
    procedure RespondWithValue(value: IBuffer); safecall;
    procedure RespondWithProtocolError(protocolError: Byte); safecall;
    property Length: Cardinal read get_Length;
    property Offset: Cardinal read get_Offset;
    property State: GenericAttributeProfile_GattRequestState read get_State;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattReadRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{0246E199-5199-5BDB-919D-8544CE30FD71}']
    procedure Invoke(sender: GenericAttributeProfile_IGattReadRequest; args: GenericAttributeProfile_IGattRequestStateChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequest,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattReadRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattReadRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs_Delegate_Base)
  ['{DA58BBDC-A307-5052-B47C-D1614AE89712}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs
  // External 
  GenericAttributeProfile_IGattRequestStateChangedEventArgs = interface(IInspectable)
  ['{E834D92C-27BE-44B3-9D0D-4FC6E808DD3F}']
    function get_State: GenericAttributeProfile_GattRequestState; safecall;
    function get_Error: BluetoothError; safecall;
    property Error: BluetoothError read get_Error;
    property State: GenericAttributeProfile_GattRequestState read get_State;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattWriteRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{6CF5B169-3731-591B-AE7C-D939FAAA8A71}']
    procedure Invoke(sender: GenericAttributeProfile_IGattLocalDescriptor; args: GenericAttributeProfile_IGattWriteRequestedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattWriteRequestedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattLocalDescriptor__GenericAttributeProfile_IGattWriteRequestedEventArgs_Delegate_Base)
  ['{CB1ABF9C-5B64-5941-86B8-475239221291}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs
  // External 
  GenericAttributeProfile_IGattWriteRequestedEventArgs = interface(IInspectable)
  ['{2DEC8BBE-A73A-471A-94D5-037DEADD0806}']
    function get_Session: GenericAttributeProfile_IGattSession; safecall;
    function GetDeferral: IDeferral; safecall;
    function GetRequestAsync: IAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest; safecall;
    property Session: GenericAttributeProfile_IGattSession read get_Session;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest>
  IAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest_Base = interface(IInspectable)
  ['{FB8B3C18-2F60-5B43-B773-146045816E03}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest; safecall;
    function GetResults: GenericAttributeProfile_IGattWriteRequest; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest>
  // External 
  IAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest = interface(IAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest_Base)
  ['{E82BD98A-0FE5-5AC6-9505-9B93DD1617C1}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest_Delegate_Base = interface(IUnknown)
  ['{25B737F6-30FF-558E-BA16-B564C45FDC06}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GenericAttributeProfile_IGattWriteRequest; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest>
  // External 
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest = interface(AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattWriteRequest_Delegate_Base)
  ['{3475725E-666D-5629-B747-163468E77F7D}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest
  // External 
  GenericAttributeProfile_IGattWriteRequest = interface(IInspectable)
  ['{AEB6A9ED-DE2F-4FC2-A9A8-94EA7844F13D}']
    function get_Value: IBuffer; safecall;
    function get_Offset: Cardinal; safecall;
    function get_Option: GenericAttributeProfile_GattWriteOption; safecall;
    function get_State: GenericAttributeProfile_GattRequestState; safecall;
    function add_StateChanged(handler: TypedEventHandler_2__GenericAttributeProfile_IGattWriteRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StateChanged(token: EventRegistrationToken); safecall;
    procedure Respond; safecall;
    procedure RespondWithProtocolError(protocolError: Byte); safecall;
    property Offset: Cardinal read get_Offset;
    property Option: GenericAttributeProfile_GattWriteOption read get_Option;
    property State: GenericAttributeProfile_GattRequestState read get_State;
    property Value: IBuffer read get_Value;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattWriteRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{7744C6BC-CDCD-5283-9E4F-2E21B49A2EF2}']
    procedure Invoke(sender: GenericAttributeProfile_IGattWriteRequest; args: GenericAttributeProfile_IGattRequestStateChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequest,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattRequestStateChangedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattWriteRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattWriteRequest__GenericAttributeProfile_IGattRequestStateChangedEventArgs_Delegate_Base)
  ['{58BE9D7F-7B15-56CD-B5FF-8D3CD0CDD9BB}']
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptorParameters
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_GenericAttributeProfile_GattLocalDescriptorParameters)]
  GenericAttributeProfile_IGattLocalDescriptorParameters = interface(IInspectable)
  ['{5FDEDE6A-F3C1-4B66-8C4B-E3D2293B40E9}']
    procedure put_StaticValue(value: IBuffer); safecall;
    function get_StaticValue: IBuffer; safecall;
    procedure put_ReadProtectionLevel(value: GenericAttributeProfile_GattProtectionLevel); safecall;
    function get_ReadProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    procedure put_WriteProtectionLevel(value: GenericAttributeProfile_GattProtectionLevel); safecall;
    function get_WriteProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    property ReadProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_ReadProtectionLevel write put_ReadProtectionLevel;
    property StaticValue: IBuffer read get_StaticValue write put_StaticValue;
    property WriteProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_WriteProtectionLevel write put_WriteProtectionLevel;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalDescriptor>
  // External 
  IVectorView_1__GenericAttributeProfile_IGattLocalDescriptor = interface(IInspectable)
  ['{F28FF66D-95D2-5DFE-B92C-E222DC58A527}']
    function GetAt(index: Cardinal): GenericAttributeProfile_IGattLocalDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: GenericAttributeProfile_IGattLocalDescriptor; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGenericAttributeProfile_IGattLocalDescriptor): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattPresentationFormat>
  // External 
  IVectorView_1__GenericAttributeProfile_IGattPresentationFormat = interface(IInspectable)
  ['{E606B52C-CE92-5CAC-A4C3-6BED39EBB7D2}']
    function GetAt(index: Cardinal): GenericAttributeProfile_IGattPresentationFormat; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: GenericAttributeProfile_IGattPresentationFormat; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGenericAttributeProfile_IGattPresentationFormat): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattPresentationFormat
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_GenericAttributeProfile_GattPresentationFormat)]
  GenericAttributeProfile_IGattPresentationFormat = interface(IInspectable)
  ['{196D0021-FAAD-45DC-AE5B-2AC3184E84DB}']
    function get_FormatType: Byte; safecall;
    function get_Exponent: Integer; safecall;
    function get_Unit: Word; safecall;
    function get_Namespace: Byte; safecall;
    function get_Description: Word; safecall;
    property Description: Word read get_Description;
    property Exponent: Integer read get_Exponent;
    property FormatType: Byte read get_FormatType;
    property Namespace: Byte read get_Namespace;
    property &Unit: Word read get_Unit;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattSubscribedClient>
  // External 
  IVectorView_1__GenericAttributeProfile_IGattSubscribedClient = interface(IInspectable)
  ['{AC454392-B7E9-5F81-B490-8C22736E8FAF}']
    function GetAt(index: Cardinal): GenericAttributeProfile_IGattSubscribedClient; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: GenericAttributeProfile_IGattSubscribedClient; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGenericAttributeProfile_IGattSubscribedClient): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattReadRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{218A3E4A-AA9D-500E-BCA7-047751BB10A3}']
    procedure Invoke(sender: GenericAttributeProfile_IGattLocalCharacteristic; args: GenericAttributeProfile_IGattReadRequestedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadRequestedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattReadRequestedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattReadRequestedEventArgs_Delegate_Base)
  ['{76E55EC5-0FA9-57DC-9A19-393D973B71D9}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattWriteRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{ED61A2FB-7D2A-5BA3-8EBF-8AD878E539A9}']
    procedure Invoke(sender: GenericAttributeProfile_IGattLocalCharacteristic; args: GenericAttributeProfile_IGattWriteRequestedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattLocalCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattWriteRequestedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattWriteRequestedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattLocalCharacteristic__GenericAttributeProfile_IGattWriteRequestedEventArgs_Delegate_Base)
  ['{6648D851-ECC6-5BD1-B013-0F76FE46A7B4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>>
  IAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult_Base = interface(IInspectable)
  ['{B6FA5848-ACCD-536B-A37E-2444D86F2C1F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult; safecall;
    function GetResults: IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>>
  // External 
  IAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = interface(IAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult_Base)
  ['{77864A29-6D27-53FB-ACB3-5D0A63014C25}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>>
  AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult_Delegate_Base = interface(IUnknown)
  ['{2F6C4343-667F-5D74-8EE7-B39DE335A960}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>>
  // External 
  AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = interface(AsyncOperationCompletedHandler_1__IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult_Delegate_Base)
  ['{43D6F81B-134C-5EBB-9FBC-DC7726A3E392}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  // External 
  IVectorView_1__GenericAttributeProfile_IGattClientNotificationResult = interface(IInspectable)
  ['{4865DCB2-E33A-5D5E-9256-8825E5354F91}']
    function GetAt(index: Cardinal): GenericAttributeProfile_IGattClientNotificationResult; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: GenericAttributeProfile_IGattClientNotificationResult; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGenericAttributeProfile_IGattClientNotificationResult): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult
  // External 
  GenericAttributeProfile_IGattClientNotificationResult = interface(IInspectable)
  ['{506D5599-0112-419A-8E3B-AE21AFABD2C2}']
    function get_SubscribedClient: GenericAttributeProfile_IGattSubscribedClient; safecall;
    function get_Status: GenericAttributeProfile_GattCommunicationStatus; safecall;
    function get_ProtocolError: IReference_1__Byte; safecall;
    property ProtocolError: IReference_1__Byte read get_ProtocolError;
    property Status: GenericAttributeProfile_GattCommunicationStatus read get_Status;
    property SubscribedClient: GenericAttributeProfile_IGattSubscribedClient read get_SubscribedClient;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult_Base = interface(IInspectable)
  ['{DE27C5CF-6227-5829-B997-88E575AD0680}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult; safecall;
    function GetResults: GenericAttributeProfile_IGattClientNotificationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  // External 
  IAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult = interface(IAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult_Base)
  ['{F965BFC2-BE41-536F-97E7-C537FE92B218}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult_Delegate_Base = interface(IUnknown)
  ['{9783FEF1-1B62-5418-9898-933138C2BD14}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GenericAttributeProfile_IGattClientNotificationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattClientNotificationResult>
  // External 
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult = interface(AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattClientNotificationResult_Delegate_Base)
  ['{130F9942-D7D7-5FBA-9990-DF2B2E3ED58A}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.IBluetoothDevice,Object>
  TypedEventHandler_2__IBluetoothDevice__IInspectable_Delegate_Base = interface(IUnknown)
  ['{DB56CE1C-5E9F-5138-9227-B1A66D60BC1B}']
    procedure Invoke(sender: IBluetoothDevice; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.IBluetoothDevice,Object>
  // External 
  TypedEventHandler_2__IBluetoothDevice__IInspectable = interface(TypedEventHandler_2__IBluetoothDevice__IInspectable_Delegate_Base)
  ['{E3343193-1080-53E0-9056-4FDA6A561704}']
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.IBluetoothDevice
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_BluetoothDevice)]
  IBluetoothDevice = interface(IInspectable)
  ['{2335B156-90D2-4A04-AEF5-0E20B9E6B707}']
    function get_DeviceId: HSTRING; safecall;
    function get_HostName: IHostName; safecall;
    function get_Name: HSTRING; safecall;
    function get_ClassOfDevice: IBluetoothClassOfDevice; safecall;
    function get_SdpRecords: IVectorView_1__IBuffer; safecall;
    function get_RfcommServices: IVectorView_1__Rfcomm_IRfcommDeviceService; safecall;
    function get_ConnectionStatus: BluetoothConnectionStatus; safecall;
    function get_BluetoothAddress: UInt64; safecall;
    function add_NameChanged(handler: TypedEventHandler_2__IBluetoothDevice__IInspectable): EventRegistrationToken; safecall;
    procedure remove_NameChanged(token: EventRegistrationToken); safecall;
    function add_SdpRecordsChanged(handler: TypedEventHandler_2__IBluetoothDevice__IInspectable): EventRegistrationToken; safecall;
    procedure remove_SdpRecordsChanged(token: EventRegistrationToken); safecall;
    function add_ConnectionStatusChanged(handler: TypedEventHandler_2__IBluetoothDevice__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ConnectionStatusChanged(token: EventRegistrationToken); safecall;
    property BluetoothAddress: UInt64 read get_BluetoothAddress;
    property ClassOfDevice: IBluetoothClassOfDevice read get_ClassOfDevice;
    property ConnectionStatus: BluetoothConnectionStatus read get_ConnectionStatus;
    property DeviceId: HSTRING read get_DeviceId;
    property HostName: IHostName read get_HostName;
    property Name: HSTRING read get_Name;
    property RfcommServices: IVectorView_1__Rfcomm_IRfcommDeviceService read get_RfcommServices;
    property SdpRecords: IVectorView_1__IBuffer read get_SdpRecords;
  end;

  // DualAPI Interface
  // Windows.Networking.IHostName
  [WinRTClassNameAttribute(SWindows_Networking_HostName)]
  IHostName = interface(IInspectable)
  ['{BF8ECAAD-ED96-49A7-9084-D416CAE88DCB}']
    function get_IPInformation: IIPInformation; safecall;
    function get_RawName: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_CanonicalName: HSTRING; safecall;
    function get_Type: HostNameType; safecall;
    function IsEqual(hostName: IHostName): Boolean; safecall;
    property CanonicalName: HSTRING read get_CanonicalName;
    property DisplayName: HSTRING read get_DisplayName;
    property IPInformation: IIPInformation read get_IPInformation;
    property RawName: HSTRING read get_RawName;
    property &Type: HostNameType read get_Type;
  end;

  // Windows.Networking.Connectivity.IIPInformation
  // External 
  IIPInformation = interface(IInspectable)
  ['{D85145E0-138F-47D7-9B3A-36BB488CEF33}']
    function get_NetworkAdapter: INetworkAdapter; safecall;
    function get_PrefixLength: IReference_1__Byte; safecall;
    property NetworkAdapter: INetworkAdapter read get_NetworkAdapter;
    property PrefixLength: IReference_1__Byte read get_PrefixLength;
  end;

  // Windows.Networking.Connectivity.INetworkAdapter
  // External 
  INetworkAdapter = interface(IInspectable)
  ['{3B542E03-5388-496C-A8A3-AFFD39AEC2E6}']
    function get_OutboundMaxBitsPerSecond: UInt64; safecall;
    function get_InboundMaxBitsPerSecond: UInt64; safecall;
    function get_IanaInterfaceType: Cardinal; safecall;
    function get_NetworkItem: INetworkItem; safecall;
    function get_NetworkAdapterId: TGuid; safecall;
    function GetConnectedProfileAsync: IAsyncOperation_1__IConnectionProfile; safecall;
    property IanaInterfaceType: Cardinal read get_IanaInterfaceType;
    property InboundMaxBitsPerSecond: UInt64 read get_InboundMaxBitsPerSecond;
    property NetworkAdapterId: TGuid read get_NetworkAdapterId;
    property NetworkItem: INetworkItem read get_NetworkItem;
    property OutboundMaxBitsPerSecond: UInt64 read get_OutboundMaxBitsPerSecond;
  end;

  // Windows.Networking.Connectivity.INetworkItem
  // External 
  INetworkItem = interface(IInspectable)
  ['{01BC4D39-F5E0-4567-A28C-42080C831B2B}']
    function get_NetworkId: TGuid; safecall;
    function GetNetworkTypes: NetworkTypes; safecall;
    property NetworkId: TGuid read get_NetworkId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IConnectionProfile>
  IAsyncOperation_1__IConnectionProfile_Base = interface(IInspectable)
  ['{5BF519CA-8ADB-5AB5-ABB8-FF1BBE5D2DE8}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IConnectionProfile); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IConnectionProfile; safecall;
    function GetResults: IConnectionProfile; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IConnectionProfile read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.Connectivity.IConnectionProfile>
  // External 
  IAsyncOperation_1__IConnectionProfile = interface(IAsyncOperation_1__IConnectionProfile_Base)
  ['{76F83AB3-8473-573F-929B-CB63C31C2217}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IConnectionProfile>
  AsyncOperationCompletedHandler_1__IConnectionProfile_Delegate_Base = interface(IUnknown)
  ['{E4F0C96A-0571-59F4-A9A9-AFAC3E61CAA0}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IConnectionProfile; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.Connectivity.IConnectionProfile>
  // External 
  AsyncOperationCompletedHandler_1__IConnectionProfile = interface(AsyncOperationCompletedHandler_1__IConnectionProfile_Delegate_Base)
  ['{A3EA214F-1B9F-5581-AE53-9B2B1C552EC5}']
  end;

  // Windows.Networking.Connectivity.IConnectionProfile
  // External 
  IConnectionProfile = interface(IInspectable)
  ['{71BA143C-598E-49D0-84EB-8FEBAEDCC195}']
    function get_ProfileName: HSTRING; safecall;
    function GetNetworkConnectivityLevel: NetworkConnectivityLevel; safecall;
    function GetNetworkNames: IVectorView_1__HSTRING; safecall;
    function GetConnectionCost: IConnectionCost; safecall;
    function GetDataPlanStatus: IDataPlanStatus; safecall;
    function get_NetworkAdapter: INetworkAdapter; safecall;
    function GetLocalUsage(StartTime: DateTime; EndTime: DateTime): IDataUsage; overload; safecall;
    function GetLocalUsage(StartTime: DateTime; EndTime: DateTime; States: RoamingStates): IDataUsage; overload; safecall;
    function get_NetworkSecuritySettings: INetworkSecuritySettings; safecall;
    property NetworkAdapter: INetworkAdapter read get_NetworkAdapter;
    property NetworkSecuritySettings: INetworkSecuritySettings read get_NetworkSecuritySettings;
    property ProfileName: HSTRING read get_ProfileName;
  end;

  // Windows.Networking.Connectivity.IConnectionCost
  // External 
  IConnectionCost = interface(IInspectable)
  ['{BAD7D829-3416-4B10-A202-BAC0B075BDAE}']
    function get_NetworkCostType: NetworkCostType; safecall;
    function get_Roaming: Boolean; safecall;
    function get_OverDataLimit: Boolean; safecall;
    function get_ApproachingDataLimit: Boolean; safecall;
    property ApproachingDataLimit: Boolean read get_ApproachingDataLimit;
    property NetworkCostType_: NetworkCostType read get_NetworkCostType;
    property OverDataLimit: Boolean read get_OverDataLimit;
    property Roaming: Boolean read get_Roaming;
  end;

  // Windows.Networking.Connectivity.IDataPlanStatus
  // External 
  IDataPlanStatus = interface(IInspectable)
  ['{977A8B8C-3885-40F3-8851-42CD2BD568BB}']
    function get_DataPlanUsage: IDataPlanUsage; safecall;
    function get_DataLimitInMegabytes: IReference_1__Cardinal; safecall;
    function get_InboundBitsPerSecond: IReference_1__UInt64; safecall;
    function get_OutboundBitsPerSecond: IReference_1__UInt64; safecall;
    function get_NextBillingCycle: IReference_1__DateTime; safecall;
    function get_MaxTransferSizeInMegabytes: IReference_1__Cardinal; safecall;
    property DataLimitInMegabytes: IReference_1__Cardinal read get_DataLimitInMegabytes;
    property DataPlanUsage: IDataPlanUsage read get_DataPlanUsage;
    property InboundBitsPerSecond: IReference_1__UInt64 read get_InboundBitsPerSecond;
    property MaxTransferSizeInMegabytes: IReference_1__Cardinal read get_MaxTransferSizeInMegabytes;
    property NextBillingCycle: IReference_1__DateTime read get_NextBillingCycle;
    property OutboundBitsPerSecond: IReference_1__UInt64 read get_OutboundBitsPerSecond;
  end;

  // Windows.Networking.Connectivity.IDataPlanUsage
  // External 
  IDataPlanUsage = interface(IInspectable)
  ['{B921492D-3B44-47FF-B361-BE59E69ED1B0}']
    function get_MegabytesUsed: Cardinal; safecall;
    function get_LastSyncTime: DateTime; safecall;
    property LastSyncTime: DateTime read get_LastSyncTime;
    property MegabytesUsed: Cardinal read get_MegabytesUsed;
  end;

  // Windows.Foundation.IReference`1<UInt64>
  // External 
  IReference_1__UInt64 = interface(IInspectable)
  ['{6755E376-53BB-568B-A11D-17239868309E}']
    function get_Value: UInt64; safecall;
    property Value: UInt64 read get_Value;
  end;

  // Windows.Foundation.IReference`1<Windows.Foundation.DateTime>
  // External 
  IReference_1__DateTime = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: DateTime; safecall;
    property Value: DateTime read get_Value;
  end;

  // Windows.Networking.Connectivity.IDataUsage
  // External 
  IDataUsage = interface(IInspectable)
  ['{C1431DD3-B146-4D39-B959-0C69B096C512}']
    function get_BytesSent: UInt64; safecall;
    function get_BytesReceived: UInt64; safecall;
    property BytesReceived: UInt64 read get_BytesReceived;
    property BytesSent: UInt64 read get_BytesSent;
  end;

  // Windows.Networking.Connectivity.INetworkSecuritySettings
  // External 
  INetworkSecuritySettings = interface(IInspectable)
  ['{7CA07E8D-917B-4B5F-B84D-28F7A5AC5402}']
    function get_NetworkAuthenticationType: NetworkAuthenticationType; safecall;
    function get_NetworkEncryptionType: NetworkEncryptionType; safecall;
    property NetworkAuthenticationType_: NetworkAuthenticationType read get_NetworkAuthenticationType;
    property NetworkEncryptionType_: NetworkEncryptionType read get_NetworkEncryptionType;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.IBluetoothClassOfDevice
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_BluetoothClassOfDevice)]
  IBluetoothClassOfDevice = interface(IInspectable)
  ['{D640227E-D7D7-4661-9454-65039CA17A2B}']
    function get_RawValue: Cardinal; safecall;
    function get_MajorClass: BluetoothMajorClass; safecall;
    function get_MinorClass: BluetoothMinorClass; safecall;
    function get_ServiceCapabilities: BluetoothServiceCapabilities; safecall;
    property MajorClass: BluetoothMajorClass read get_MajorClass;
    property MinorClass: BluetoothMinorClass read get_MinorClass;
    property RawValue: Cardinal read get_RawValue;
    property ServiceCapabilities: BluetoothServiceCapabilities read get_ServiceCapabilities;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IBuffer>
  // External 
  IVectorView_1__IBuffer = interface(IInspectable)
  ['{FD944562-11D6-5EAB-BD72-701993B68FAC}']
    function GetAt(index: Cardinal): IBuffer; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IBuffer; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBuffer): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.Rfcomm.IRfcommDeviceService>
  // External 
  IVectorView_1__Rfcomm_IRfcommDeviceService = interface(IInspectable)
  ['{DB646C4B-561D-568C-96C9-A904005738D7}']
    function GetAt(index: Cardinal): Rfcomm_IRfcommDeviceService; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Rfcomm_IRfcommDeviceService; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PRfcomm_IRfcommDeviceService): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.Rfcomm.IRfcommDeviceService
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Rfcomm_RfcommDeviceService)]
  Rfcomm_IRfcommDeviceService = interface(IInspectable)
  ['{AE81FF1F-C5A1-4C40-8C28-F3EFD69062F3}']
    function get_ConnectionHostName: IHostName; safecall;
    function get_ConnectionServiceName: HSTRING; safecall;
    function get_ServiceId: Rfcomm_IRfcommServiceId; safecall;
    function get_ProtectionLevel: SocketProtectionLevel; safecall;
    function get_MaxProtectionLevel: SocketProtectionLevel; safecall;
    function GetSdpRawAttributesAsync: IAsyncOperation_1__IMapView_2__Cardinal__IBuffer; overload; safecall;
    function GetSdpRawAttributesAsync(cacheMode: BluetoothCacheMode): IAsyncOperation_1__IMapView_2__Cardinal__IBuffer; overload; safecall;
    property ConnectionHostName: IHostName read get_ConnectionHostName;
    property ConnectionServiceName: HSTRING read get_ConnectionServiceName;
    property MaxProtectionLevel: SocketProtectionLevel read get_MaxProtectionLevel;
    property ProtectionLevel: SocketProtectionLevel read get_ProtectionLevel;
    property ServiceId: Rfcomm_IRfcommServiceId read get_ServiceId;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.Rfcomm.IRfcommServiceId
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_Rfcomm_RfcommServiceId)]
  Rfcomm_IRfcommServiceId = interface(IInspectable)
  ['{22629204-7E02-4017-8136-DA1B6A1B9BBF}']
    function get_Uuid: TGuid; safecall;
    function AsShortId: Cardinal; safecall;
    function AsString: HSTRING; safecall;
    property Uuid: TGuid read get_Uuid;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IAsyncOperation_1__IMapView_2__Cardinal__IBuffer_Base = interface(IInspectable)
  ['{D4904DED-BC1D-5933-AECF-E42C5D465BFF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer; safecall;
    function GetResults: IMapView_2__Cardinal__IBuffer; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>>
  // External 
  IAsyncOperation_1__IMapView_2__Cardinal__IBuffer = interface(IAsyncOperation_1__IMapView_2__Cardinal__IBuffer_Base)
  ['{D4904DED-BC1D-5933-AECF-E42C5D465BFF}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>>
  AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer_Delegate_Base = interface(IUnknown)
  ['{92C2E4D0-7C25-596B-9135-10D1472E6968}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMapView_2__Cardinal__IBuffer; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>>
  // External 
  AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer = interface(AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer_Delegate_Base)
  ['{92C2E4D0-7C25-596B-9135-10D1472E6968}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>
  IMapView_2__Cardinal__IBuffer_Base = interface(IInspectable)
  ['{57DC41E6-8B4D-5910-9703-D7C668436852}']
    function Lookup(key: Cardinal): IBuffer; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: Cardinal): Boolean; safecall;
    procedure Split(out first: IMapView_2__Cardinal__IBuffer; out second: IMapView_2__Cardinal__IBuffer); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<UInt32,Windows.Storage.Streams.IBuffer>
  // External 
  IMapView_2__Cardinal__IBuffer = interface(IMapView_2__Cardinal__IBuffer_Base)
  ['{57DC41E6-8B4D-5910-9703-D7C668436852}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.IBluetoothLEDevice,Object>
  TypedEventHandler_2__IBluetoothLEDevice__IInspectable_Delegate_Base = interface(IUnknown)
  ['{A90661E2-372E-5D1E-BBBB-B8A2CE0E7C4D}']
    procedure Invoke(sender: IBluetoothLEDevice; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.IBluetoothLEDevice,Object>
  // External 
  TypedEventHandler_2__IBluetoothLEDevice__IInspectable = interface(TypedEventHandler_2__IBluetoothLEDevice__IInspectable_Delegate_Base)
  ['{68B7E600-528B-5EE6-BEE1-1A4B917B2AA8}']
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.IBluetoothLEDevice
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_BluetoothLEDevice)]
  IBluetoothLEDevice = interface(IInspectable)
  ['{B5EE2F7B-4AD8-4642-AC48-80A0B500E887}']
    function get_DeviceId: HSTRING; safecall;
    function get_Name: HSTRING; safecall;
    function get_GattServices: IVectorView_1__GenericAttributeProfile_IGattDeviceService; safecall;
    function get_ConnectionStatus: BluetoothConnectionStatus; safecall;
    function get_BluetoothAddress: UInt64; safecall;
    function GetGattService(serviceUuid: TGuid): GenericAttributeProfile_IGattDeviceService; safecall;
    function add_NameChanged(handler: TypedEventHandler_2__IBluetoothLEDevice__IInspectable): EventRegistrationToken; safecall;
    procedure remove_NameChanged(token: EventRegistrationToken); safecall;
    function add_GattServicesChanged(handler: TypedEventHandler_2__IBluetoothLEDevice__IInspectable): EventRegistrationToken; safecall;
    procedure remove_GattServicesChanged(token: EventRegistrationToken); safecall;
    function add_ConnectionStatusChanged(handler: TypedEventHandler_2__IBluetoothLEDevice__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ConnectionStatusChanged(token: EventRegistrationToken); safecall;
    property BluetoothAddress: UInt64 read get_BluetoothAddress;
    property ConnectionStatus: BluetoothConnectionStatus read get_ConnectionStatus;
    property DeviceId: HSTRING read get_DeviceId;
    property GattServices: IVectorView_1__GenericAttributeProfile_IGattDeviceService read get_GattServices;
    property Name: HSTRING read get_Name;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDeviceService>
  // External 
  IVectorView_1__GenericAttributeProfile_IGattDeviceService = interface(IInspectable)
  ['{C44B7FD0-1C4C-56D8-A640-C159C17F1CF9}']
    function GetAt(index: Cardinal): GenericAttributeProfile_IGattDeviceService; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: GenericAttributeProfile_IGattDeviceService; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGenericAttributeProfile_IGattDeviceService): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDeviceService
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_GenericAttributeProfile_GattDeviceService)]
  GenericAttributeProfile_IGattDeviceService = interface(IInspectable)
  ['{AC7B7C05-B33C-47CF-990F-6B8F5577DF71}']
    function GetCharacteristics(characteristicUuid: TGuid): IVectorView_1__GenericAttributeProfile_IGattCharacteristic; safecall;
    function GetIncludedServices(serviceUuid: TGuid): IVectorView_1__GenericAttributeProfile_IGattDeviceService; safecall;
    function get_DeviceId: HSTRING; safecall;
    function get_Uuid: TGuid; safecall;
    function get_AttributeHandle: Word; safecall;
    property AttributeHandle: Word read get_AttributeHandle;
    property DeviceId: HSTRING read get_DeviceId;
    property Uuid: TGuid read get_Uuid;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic>
  // External 
  IVectorView_1__GenericAttributeProfile_IGattCharacteristic = interface(IInspectable)
  ['{F5A70A0F-15F4-57EE-9FE7-0CE390AE530B}']
    function GetAt(index: Cardinal): GenericAttributeProfile_IGattCharacteristic; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: GenericAttributeProfile_IGattCharacteristic; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGenericAttributeProfile_IGattCharacteristic): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_GenericAttributeProfile_GattCharacteristic)]
  GenericAttributeProfile_IGattCharacteristic = interface(IInspectable)
  ['{59CB50C1-5934-4F68-A198-EB864FA44E6B}']
    function GetDescriptors(descriptorUuid: TGuid): IVectorView_1__GenericAttributeProfile_IGattDescriptor; safecall;
    function get_CharacteristicProperties: GenericAttributeProfile_GattCharacteristicProperties; safecall;
    function get_ProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    procedure put_ProtectionLevel(value: GenericAttributeProfile_GattProtectionLevel); safecall;
    function get_UserDescription: HSTRING; safecall;
    function get_Uuid: TGuid; safecall;
    function get_AttributeHandle: Word; safecall;
    function get_PresentationFormats: IVectorView_1__GenericAttributeProfile_IGattPresentationFormat; safecall;
    function ReadValueAsync: IAsyncOperation_1__GenericAttributeProfile_IGattReadResult; overload; safecall;
    function ReadValueAsync(cacheMode: BluetoothCacheMode): IAsyncOperation_1__GenericAttributeProfile_IGattReadResult; overload; safecall;
    function WriteValueAsync(value: IBuffer): IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus; overload; safecall;
    function WriteValueAsync(value: IBuffer; writeOption: GenericAttributeProfile_GattWriteOption): IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus; overload; safecall;
    function ReadClientCharacteristicConfigurationDescriptorAsync: IAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult; safecall;
    function WriteClientCharacteristicConfigurationDescriptorAsync(clientCharacteristicConfigurationDescriptorValue: GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue): IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus; safecall;
    function add_ValueChanged(valueChangedHandler: TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ValueChanged(valueChangedEventCookie: EventRegistrationToken); safecall;
    property AttributeHandle: Word read get_AttributeHandle;
    property CharacteristicProperties: GenericAttributeProfile_GattCharacteristicProperties read get_CharacteristicProperties;
    property PresentationFormats: IVectorView_1__GenericAttributeProfile_IGattPresentationFormat read get_PresentationFormats;
    property ProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_ProtectionLevel write put_ProtectionLevel;
    property UserDescription: HSTRING read get_UserDescription;
    property Uuid: TGuid read get_Uuid;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDescriptor>
  // External 
  IVectorView_1__GenericAttributeProfile_IGattDescriptor = interface(IInspectable)
  ['{F23D1B8A-27DF-547D-9482-1DD9089D8A99}']
    function GetAt(index: Cardinal): GenericAttributeProfile_IGattDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: GenericAttributeProfile_IGattDescriptor; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PGenericAttributeProfile_IGattDescriptor): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattDescriptor
  [WinRTClassNameAttribute(SWindows_Devices_Bluetooth_GenericAttributeProfile_GattDescriptor)]
  GenericAttributeProfile_IGattDescriptor = interface(IInspectable)
  ['{92055F2B-8084-4344-B4C2-284DE19A8506}']
    function get_ProtectionLevel: GenericAttributeProfile_GattProtectionLevel; safecall;
    procedure put_ProtectionLevel(value: GenericAttributeProfile_GattProtectionLevel); safecall;
    function get_Uuid: TGuid; safecall;
    function get_AttributeHandle: Word; safecall;
    function ReadValueAsync: IAsyncOperation_1__GenericAttributeProfile_IGattReadResult; overload; safecall;
    function ReadValueAsync(cacheMode: BluetoothCacheMode): IAsyncOperation_1__GenericAttributeProfile_IGattReadResult; overload; safecall;
    function WriteValueAsync(value: IBuffer): IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus; safecall;
    property AttributeHandle: Word read get_AttributeHandle;
    property ProtectionLevel: GenericAttributeProfile_GattProtectionLevel read get_ProtectionLevel write put_ProtectionLevel;
    property Uuid: TGuid read get_Uuid;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattReadResult_Base = interface(IInspectable)
  ['{D40432A8-1E14-51D0-B49B-AE2CE1AA05E5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult; safecall;
    function GetResults: GenericAttributeProfile_IGattReadResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult>
  // External 
  IAsyncOperation_1__GenericAttributeProfile_IGattReadResult = interface(IAsyncOperation_1__GenericAttributeProfile_IGattReadResult_Base)
  ['{55FC83CB-542E-5E30-AF71-8E8CD0BD3DEB}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult_Delegate_Base = interface(IUnknown)
  ['{D8992AA0-EAC2-55B7-92C5-894886BEB0CA}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GenericAttributeProfile_IGattReadResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult>
  // External 
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult = interface(AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadResult_Delegate_Base)
  ['{303B29E1-492A-5B34-8342-81B827B67051}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadResult
  // External 
  GenericAttributeProfile_IGattReadResult = interface(IInspectable)
  ['{63A66F08-1AEA-4C4C-A50F-97BAE474B348}']
    function get_Status: GenericAttributeProfile_GattCommunicationStatus; safecall;
    function get_Value: IBuffer; safecall;
    property Status: GenericAttributeProfile_GattCommunicationStatus read get_Status;
    property Value: IBuffer read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus>
  IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus_Base = interface(IInspectable)
  ['{3FF69516-1BFB-52E9-9EE6-E5CDB78E1683}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus; safecall;
    function GetResults: GenericAttributeProfile_GattCommunicationStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus>
  // External 
  IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus = interface(IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus_Delegate_Base = interface(IUnknown)
  ['{2154117A-978D-59DB-99CF-6B690CB3389B}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GenericAttributeProfile_GattCommunicationStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.GattCommunicationStatus>
  // External 
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus = interface(AsyncOperationCompletedHandler_1__GenericAttributeProfile_GattCommunicationStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult>
  IAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult_Base = interface(IInspectable)
  ['{CF4444CC-4077-5719-8366-46E86B983685}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult; safecall;
    function GetResults: GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult>
  // External 
  IAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = interface(IAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult_Base)
  ['{8B916488-861B-5537-A189-0239B2B84D64}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult>
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult_Delegate_Base = interface(IUnknown)
  ['{98F9A6F3-4D29-5351-8B12-751DC977A331}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult>
  // External 
  AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = interface(AsyncOperationCompletedHandler_1__GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult_Delegate_Base)
  ['{A42294DB-768D-50EC-AA7C-F4AB7815980F}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattReadClientCharacteristicConfigurationDescriptorResult
  // External 
  GenericAttributeProfile_IGattReadClientCharacteristicConfigurationDescriptorResult = interface(IInspectable)
  ['{63A66F09-1AEA-4C4C-A50F-97BAE474B348}']
    function get_Status: GenericAttributeProfile_GattCommunicationStatus; safecall;
    function get_ClientCharacteristicConfigurationDescriptor: GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue; safecall;
    property ClientCharacteristicConfigurationDescriptor: GenericAttributeProfile_GattClientCharacteristicConfigurationDescriptorValue read get_ClientCharacteristicConfigurationDescriptor;
    property Status: GenericAttributeProfile_GattCommunicationStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattValueChangedEventArgs>
  TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{C1F420F6-6292-5760-A2C9-9DDF98683CFC}']
    procedure Invoke(sender: GenericAttributeProfile_IGattCharacteristic; args: GenericAttributeProfile_IGattValueChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Bluetooth.GenericAttributeProfile.IGattCharacteristic,Windows.Devices.Bluetooth.GenericAttributeProfile.IGattValueChangedEventArgs>
  // External 
  TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs = interface(TypedEventHandler_2__GenericAttributeProfile_IGattCharacteristic__GenericAttributeProfile_IGattValueChangedEventArgs_Delegate_Base)
  ['{42B52484-A834-51A1-A02F-AC8EC8535CA6}']
  end;

  // Windows.Devices.Bluetooth.GenericAttributeProfile.IGattValueChangedEventArgs
  // External 
  GenericAttributeProfile_IGattValueChangedEventArgs = interface(IInspectable)
  ['{D21BDB54-06E3-4ED8-A263-ACFAC8BA7313}']
    function get_CharacteristicValue: IBuffer; safecall;
    function get_Timestamp: DateTime; safecall;
    property CharacteristicValue: IBuffer read get_CharacteristicValue;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Radios.IRadio>
  IAsyncOperation_1__Radios_IRadio_Base = interface(IInspectable)
  ['{EAC62C40-8DBC-5854-8BA0-B7B9940E7389}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Radios_IRadio); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Radios_IRadio; safecall;
    function GetResults: Radios_IRadio; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Radios_IRadio read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Radios.IRadio>
  // External 
  IAsyncOperation_1__Radios_IRadio = interface(IAsyncOperation_1__Radios_IRadio_Base)
  ['{21F4F6AA-B8B3-541E-8C56-6B7DDA0081F4}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Radios.IRadio>
  AsyncOperationCompletedHandler_1__Radios_IRadio_Delegate_Base = interface(IUnknown)
  ['{8A5C7E3A-80E2-585B-8630-7A8E777F0354}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Radios_IRadio; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Radios.IRadio>
  // External 
  AsyncOperationCompletedHandler_1__Radios_IRadio = interface(AsyncOperationCompletedHandler_1__Radios_IRadio_Delegate_Base)
  ['{8178318F-1B1A-5DAE-BD78-77EA625EFAB0}']
  end;

  // DualAPI Interface
  // Windows.Devices.Radios.IRadio
  [WinRTClassNameAttribute(SWindows_Devices_Radios_Radio)]
  Radios_IRadio = interface(IInspectable)
  ['{252118DF-B33E-416A-875F-1CF38AE2D83E}']
    function SetStateAsync(value: Radios_RadioState): IAsyncOperation_1__Radios_RadioAccessStatus; safecall;
    function add_StateChanged(handler: TypedEventHandler_2__Radios_IRadio__IInspectable): EventRegistrationToken; safecall;
    procedure remove_StateChanged(eventCookie: EventRegistrationToken); safecall;
    function get_State: Radios_RadioState; safecall;
    function get_Name: HSTRING; safecall;
    function get_Kind: Radios_RadioKind; safecall;
    property Kind: Radios_RadioKind read get_Kind;
    property Name: HSTRING read get_Name;
    property State: Radios_RadioState read get_State;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Radios.RadioAccessStatus>
  IAsyncOperation_1__Radios_RadioAccessStatus_Base = interface(IInspectable)
  ['{21FB30EF-072F-502C-9898-D0C3B2CD9AC5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus; safecall;
    function GetResults: Radios_RadioAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Radios.RadioAccessStatus>
  // External 
  IAsyncOperation_1__Radios_RadioAccessStatus = interface(IAsyncOperation_1__Radios_RadioAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Radios.RadioAccessStatus>
  AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus_Delegate_Base = interface(IUnknown)
  ['{BD248E73-F05F-574C-AE3D-9B95C4BF282A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Radios_RadioAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Radios.RadioAccessStatus>
  // External 
  AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus = interface(AsyncOperationCompletedHandler_1__Radios_RadioAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Radios.IRadio,Object>
  TypedEventHandler_2__Radios_IRadio__IInspectable_Delegate_Base = interface(IUnknown)
  ['{FC6AA329-B586-5EBB-9E85-3F6B84EBDF18}']
    procedure Invoke(sender: Radios_IRadio; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Radios.IRadio,Object>
  // External 
  TypedEventHandler_2__Radios_IRadio__IInspectable = interface(TypedEventHandler_2__Radios_IRadio__IInspectable_Delegate_Base)
  ['{207C98EF-4610-5402-ADF3-7C412F7B6380}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Object>
  TypedEventHandler_2__IDeviceWatcher__IInspectable_Delegate_Base = interface(IUnknown)
  ['{9234630F-1FF4-54F6-9E3F-AC20369B7725}']
    procedure Invoke(sender: IDeviceWatcher; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Object>
  // External 
  TypedEventHandler_2__IDeviceWatcher__IInspectable = interface(TypedEventHandler_2__IDeviceWatcher__IInspectable_Delegate_Base)
  ['{2E39B58C-3F86-5F12-B142-FABFA589D8FF}']
  end;

  // Windows.Devices.Enumeration.IDeviceWatcher
  // External 
  IDeviceWatcher = interface(IInspectable)
  ['{C9EAB97D-8F6B-4F96-A9F4-ABC814E22271}']
    function add_Added(handler: TypedEventHandler_2__IDeviceWatcher__IDeviceInformation): EventRegistrationToken; safecall;
    procedure remove_Added(token: EventRegistrationToken); safecall;
    function add_Updated(handler: TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate): EventRegistrationToken; safecall;
    procedure remove_Updated(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    function add_EnumerationCompleted(handler: TypedEventHandler_2__IDeviceWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(token: EventRegistrationToken); safecall;
    function add_Stopped(handler: TypedEventHandler_2__IDeviceWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Stopped(token: EventRegistrationToken); safecall;
    function get_Status: DeviceWatcherStatus; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    property Status: DeviceWatcherStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Windows.Devices.Enumeration.IDeviceInformation>
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformation_Delegate_Base = interface(IUnknown)
  ['{03C5A07B-990C-5D09-B0B8-5734EAA38222}']
    procedure Invoke(sender: IDeviceWatcher; args: IDeviceInformation); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Windows.Devices.Enumeration.IDeviceInformation>
  // External 
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformation = interface(TypedEventHandler_2__IDeviceWatcher__IDeviceInformation_Delegate_Base)
  ['{45B8F772-CDC9-5498-8951-1E65FB72796C}']
  end;

  // DualAPI Interface
  // Windows.Devices.Enumeration.IDeviceInformation
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_DeviceInformation)]
  IDeviceInformation = interface(IInspectable)
  ['{ABA0FB95-4398-489D-8E44-E6130927011F}']
    function get_Id: HSTRING; safecall;
    function get_Name: HSTRING; safecall;
    function get_IsEnabled: Boolean; safecall;
    function get_IsDefault: Boolean; safecall;
    function get_EnclosureLocation: IEnclosureLocation; safecall;
    function get_Properties: IMapView_2__HSTRING__IInspectable; safecall;
    procedure Update(updateInfo: IDeviceInformationUpdate); safecall;
    function GetThumbnailAsync: IAsyncOperation_1__IRandomAccessStreamWithContentType; safecall;
    function GetGlyphThumbnailAsync: IAsyncOperation_1__IRandomAccessStreamWithContentType; safecall;
    property EnclosureLocation: IEnclosureLocation read get_EnclosureLocation;
    property Id: HSTRING read get_Id;
    property IsDefault: Boolean read get_IsDefault;
    property IsEnabled: Boolean read get_IsEnabled;
    property Name: HSTRING read get_Name;
    property Properties: IMapView_2__HSTRING__IInspectable read get_Properties;
  end;

  // Windows.Devices.Enumeration.IEnclosureLocation
  // External 
  IEnclosureLocation = interface(IInspectable)
  ['{42340A27-5810-459C-AABB-C65E1F813ECF}']
    function get_InDock: Boolean; safecall;
    function get_InLid: Boolean; safecall;
    function get_Panel: Panel; safecall;
    property InDock: Boolean read get_InDock;
    property InLid: Boolean read get_InLid;
    property Panel_: Panel read get_Panel;
  end;

  // Windows.Devices.Enumeration.IDeviceInformationUpdate
  // External 
  IDeviceInformationUpdate = interface(IInspectable)
  ['{8F315305-D972-44B7-A37E-9E822C78213B}']
    function get_Id: HSTRING; safecall;
    function get_Properties: IMapView_2__HSTRING__IInspectable; safecall;
    property Id: HSTRING read get_Id;
    property Properties: IMapView_2__HSTRING__IInspectable read get_Properties;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Windows.Devices.Enumeration.IDeviceInformationUpdate>
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate_Delegate_Base = interface(IUnknown)
  ['{906F1254-79AD-54FC-93C4-CDB99B437899}']
    procedure Invoke(sender: IDeviceWatcher; args: IDeviceInformationUpdate); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDeviceWatcher,Windows.Devices.Enumeration.IDeviceInformationUpdate>
  // External 
  TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate = interface(TypedEventHandler_2__IDeviceWatcher__IDeviceInformationUpdate_Delegate_Base)
  ['{E974B937-20D2-5428-86F4-7464244D7744}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Object>
  TypedEventHandler_2__IDevicePicker__IInspectable_Delegate_Base = interface(IUnknown)
  ['{62C6D98C-57EE-5BB8-A41C-958D20C3F3E8}']
    procedure Invoke(sender: IDevicePicker; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Object>
  // External 
  TypedEventHandler_2__IDevicePicker__IInspectable = interface(TypedEventHandler_2__IDevicePicker__IInspectable_Delegate_Base)
  ['{461CF512-3561-51C5-9C98-2972FF59D0B7}']
  end;

  // DualAPI Interface
  // Windows.Devices.Enumeration.IDevicePicker
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_DevicePicker)]
  IDevicePicker = interface(IInspectable)
  ['{84997AA2-034A-4440-8813-7D0BD479BF5A}']
    function get_Filter: IDevicePickerFilter; safecall;
    function get_Appearance: IDevicePickerAppearance; safecall;
    function get_RequestedProperties: IVector_1__HSTRING; safecall;
    function add_DeviceSelected(handler: TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs): EventRegistrationToken; safecall;
    procedure remove_DeviceSelected(token: EventRegistrationToken); safecall;
    function add_DisconnectButtonClicked(handler: TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs): EventRegistrationToken; safecall;
    procedure remove_DisconnectButtonClicked(token: EventRegistrationToken); safecall;
    function add_DevicePickerDismissed(handler: TypedEventHandler_2__IDevicePicker__IInspectable): EventRegistrationToken; safecall;
    procedure remove_DevicePickerDismissed(token: EventRegistrationToken); safecall;
    procedure Show(selection: TRectF); overload; safecall;
    procedure Show(selection: TRectF; placement: Popups_Placement); overload; safecall;
    function PickSingleDeviceAsync(selection: TRectF): IAsyncOperation_1__IDeviceInformation; overload; safecall;
    function PickSingleDeviceAsync(selection: TRectF; placement: Popups_Placement): IAsyncOperation_1__IDeviceInformation; overload; safecall;
    procedure Hide; safecall;
    procedure SetDisplayStatus(device: IDeviceInformation; status: HSTRING; options: DevicePickerDisplayStatusOptions); safecall;
    property Appearance: IDevicePickerAppearance read get_Appearance;
    property Filter: IDevicePickerFilter read get_Filter;
    property RequestedProperties: IVector_1__HSTRING read get_RequestedProperties;
  end;

  // Windows.Devices.Enumeration.IDevicePickerFilter
  // External 
  IDevicePickerFilter = interface(IInspectable)
  ['{91DB92A2-57CB-48F1-9B59-A59B7A1F02A2}']
    function get_SupportedDeviceClasses: IVector_1__DeviceClass; safecall;
    function get_SupportedDeviceSelectors: IVector_1__HSTRING; safecall;
    property SupportedDeviceClasses: IVector_1__DeviceClass read get_SupportedDeviceClasses;
    property SupportedDeviceSelectors: IVector_1__HSTRING read get_SupportedDeviceSelectors;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Enumeration.DeviceClass>
  IVector_1__DeviceClass_Base = interface(IInspectable)
  ['{EE662D37-B0EB-5729-9832-156FD2889D48}']
    function GetAt(index: Cardinal): DeviceClass; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__DeviceClass; safecall;
    function IndexOf(value: DeviceClass; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: DeviceClass); safecall;
    procedure InsertAt(index: Cardinal; value: DeviceClass); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: DeviceClass); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDeviceClass): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDeviceClass); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Enumeration.DeviceClass>
  // External 
  IVector_1__DeviceClass = interface(IVector_1__DeviceClass_Base)
  ['{A4739064-B54E-55D4-8012-317E2B6A807B}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Enumeration.DeviceClass>
  // External 
  IVectorView_1__DeviceClass = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): DeviceClass; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: DeviceClass; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDeviceClass): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Devices.Enumeration.IDevicePickerAppearance
  // External 
  IDevicePickerAppearance = interface(IInspectable)
  ['{E69A12C6-E627-4ED8-9B6C-460AF445E56D}']
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_ForegroundColor: Color; safecall;
    procedure put_ForegroundColor(value: Color); safecall;
    function get_BackgroundColor: Color; safecall;
    procedure put_BackgroundColor(value: Color); safecall;
    function get_AccentColor: Color; safecall;
    procedure put_AccentColor(value: Color); safecall;
    function get_SelectedForegroundColor: Color; safecall;
    procedure put_SelectedForegroundColor(value: Color); safecall;
    function get_SelectedBackgroundColor: Color; safecall;
    procedure put_SelectedBackgroundColor(value: Color); safecall;
    function get_SelectedAccentColor: Color; safecall;
    procedure put_SelectedAccentColor(value: Color); safecall;
    property AccentColor: Color read get_AccentColor write put_AccentColor;
    property BackgroundColor: Color read get_BackgroundColor write put_BackgroundColor;
    property ForegroundColor: Color read get_ForegroundColor write put_ForegroundColor;
    property SelectedAccentColor: Color read get_SelectedAccentColor write put_SelectedAccentColor;
    property SelectedBackgroundColor: Color read get_SelectedBackgroundColor write put_SelectedBackgroundColor;
    property SelectedForegroundColor: Color read get_SelectedForegroundColor write put_SelectedForegroundColor;
    property Title: HSTRING read get_Title write put_Title;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Windows.Devices.Enumeration.IDeviceSelectedEventArgs>
  TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs_Delegate_Base = interface(IUnknown)
  ['{47E48C88-1C56-5B58-96A2-8E813D25077A}']
    procedure Invoke(sender: IDevicePicker; args: IDeviceSelectedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Windows.Devices.Enumeration.IDeviceSelectedEventArgs>
  // External 
  TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs = interface(TypedEventHandler_2__IDevicePicker__IDeviceSelectedEventArgs_Delegate_Base)
  ['{1E62D931-39AF-5E34-93A0-8500F98C53C8}']
  end;

  // Windows.Devices.Enumeration.IDeviceSelectedEventArgs
  // External 
  IDeviceSelectedEventArgs = interface(IInspectable)
  ['{269EDADE-1D2F-4940-8402-4156B81D3C77}']
    function get_SelectedDevice: IDeviceInformation; safecall;
    property SelectedDevice: IDeviceInformation read get_SelectedDevice;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Windows.Devices.Enumeration.IDeviceDisconnectButtonClickedEventArgs>
  TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs_Delegate_Base = interface(IUnknown)
  ['{35DD0319-5723-506C-8896-1A28B82BE798}']
    procedure Invoke(sender: IDevicePicker; args: IDeviceDisconnectButtonClickedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.IDevicePicker,Windows.Devices.Enumeration.IDeviceDisconnectButtonClickedEventArgs>
  // External 
  TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs = interface(TypedEventHandler_2__IDevicePicker__IDeviceDisconnectButtonClickedEventArgs_Delegate_Base)
  ['{9B11002B-6A57-54AD-91FB-2B572A19150C}']
  end;

  // Windows.Devices.Enumeration.IDeviceDisconnectButtonClickedEventArgs
  // External 
  IDeviceDisconnectButtonClickedEventArgs = interface(IInspectable)
  ['{8E44B56D-F902-4A00-B536-F37992E6A2A7}']
    function get_Device: IDeviceInformation; safecall;
    property Device: IDeviceInformation read get_Device;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDeviceInformation>
  IAsyncOperation_1__IDeviceInformation_Base = interface(IInspectable)
  ['{07FAA053-EB2F-5CBA-B25B-D9D57BE6715F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IDeviceInformation); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IDeviceInformation; safecall;
    function GetResults: IDeviceInformation; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IDeviceInformation read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Enumeration.IDeviceInformation>
  // External 
  IAsyncOperation_1__IDeviceInformation = interface(IAsyncOperation_1__IDeviceInformation_Base)
  ['{F6CE39E7-E062-5F4E-9663-1883AB9707DA}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDeviceInformation>
  AsyncOperationCompletedHandler_1__IDeviceInformation_Delegate_Base = interface(IUnknown)
  ['{BB483DF2-7BB6-5923-A28D-8342EC30046B}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IDeviceInformation; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Enumeration.IDeviceInformation>
  // External 
  AsyncOperationCompletedHandler_1__IDeviceInformation = interface(AsyncOperationCompletedHandler_1__IDeviceInformation_Delegate_Base)
  ['{33A901B6-95BD-5152-B320-56644152046D}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Object>
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable_Delegate_Base = interface(IUnknown)
  ['{2EE2B4C9-B696-5ECC-B29B-F1E0EF5FE1F7}']
    procedure Invoke(sender: Pnp_IPnpObjectWatcher; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Object>
  // External 
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable = interface(TypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable_Delegate_Base)
  ['{B3E4D6DF-3B83-5CEA-895C-5B2CA60321AC}']
  end;

  // Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher
  // External 
  Pnp_IPnpObjectWatcher = interface(IInspectable)
  ['{83C95CA8-4772-4A7A-ACA8-E48C42A89C44}']
    function add_Added(handler: TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject): EventRegistrationToken; safecall;
    procedure remove_Added(token: EventRegistrationToken); safecall;
    function add_Updated(handler: TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate): EventRegistrationToken; safecall;
    procedure remove_Updated(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    function add_EnumerationCompleted(handler: TypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(token: EventRegistrationToken); safecall;
    function add_Stopped(handler: TypedEventHandler_2__Pnp_IPnpObjectWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Stopped(token: EventRegistrationToken); safecall;
    function get_Status: DeviceWatcherStatus; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    property Status: DeviceWatcherStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Windows.Devices.Enumeration.Pnp.IPnpObject>
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject_Delegate_Base = interface(IUnknown)
  ['{D578EED2-58E5-5825-8AF2-12F89387B656}']
    procedure Invoke(sender: Pnp_IPnpObjectWatcher; args: Pnp_IPnpObject); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Windows.Devices.Enumeration.Pnp.IPnpObject>
  // External 
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject = interface(TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObject_Delegate_Base)
  ['{22A619AD-2356-5234-B2AC-0D26B29D6915}']
  end;

  // DualAPI Interface
  // Windows.Devices.Enumeration.Pnp.IPnpObject
  [WinRTClassNameAttribute(SWindows_Devices_Enumeration_Pnp_PnpObject)]
  Pnp_IPnpObject = interface(IInspectable)
  ['{95C66258-733B-4A8F-93A3-DB078AC870C1}']
    function get_Type: Pnp_PnpObjectType; safecall;
    function get_Id: HSTRING; safecall;
    function get_Properties: IMapView_2__HSTRING__IInspectable; safecall;
    procedure Update(updateInfo: Pnp_IPnpObjectUpdate); safecall;
    property Id: HSTRING read get_Id;
    property Properties: IMapView_2__HSTRING__IInspectable read get_Properties;
    property &Type: Pnp_PnpObjectType read get_Type;
  end;

  // Windows.Devices.Enumeration.Pnp.IPnpObjectUpdate
  // External 
  Pnp_IPnpObjectUpdate = interface(IInspectable)
  ['{6F59E812-001E-4844-BCC6-432886856A17}']
    function get_Type: Pnp_PnpObjectType; safecall;
    function get_Id: HSTRING; safecall;
    function get_Properties: IMapView_2__HSTRING__IInspectable; safecall;
    property Id: HSTRING read get_Id;
    property Properties: IMapView_2__HSTRING__IInspectable read get_Properties;
    property &Type: Pnp_PnpObjectType read get_Type;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Windows.Devices.Enumeration.Pnp.IPnpObjectUpdate>
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate_Delegate_Base = interface(IUnknown)
  ['{AF8F929D-8058-5C38-A3D8-30AA7A08B588}']
    procedure Invoke(sender: Pnp_IPnpObjectWatcher; args: Pnp_IPnpObjectUpdate); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Enumeration.Pnp.IPnpObjectWatcher,Windows.Devices.Enumeration.Pnp.IPnpObjectUpdate>
  // External 
  TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate = interface(TypedEventHandler_2__Pnp_IPnpObjectWatcher__Pnp_IPnpObjectUpdate_Delegate_Base)
  ['{6ED742E7-1453-5E28-97C5-73FD081CAEF3}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawerCloseAlarm,Object>
  TypedEventHandler_2__ICashDrawerCloseAlarm__IInspectable_Delegate_Base = interface(IUnknown)
  ['{C54FBDA4-5E0B-54C3-94F2-83351E41C46F}']
    procedure Invoke(sender: ICashDrawerCloseAlarm; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.ICashDrawerCloseAlarm,Object>
  // External 
  TypedEventHandler_2__ICashDrawerCloseAlarm__IInspectable = interface(TypedEventHandler_2__ICashDrawerCloseAlarm__IInspectable_Delegate_Base)
  ['{5D7EA46E-5DE1-5491-98E1-E554C6EDE1EC}']
  end;

  // Windows.Devices.PointOfService.ICashDrawerCloseAlarm
  // External 
  ICashDrawerCloseAlarm = interface(IInspectable)
  ['{6BF88CC7-6F63-430E-AB3B-95D75FFBE87F}']
    procedure put_AlarmTimeout(value: TimeSpan); safecall;
    function get_AlarmTimeout: TimeSpan; safecall;
    procedure put_BeepFrequency(value: Cardinal); safecall;
    function get_BeepFrequency: Cardinal; safecall;
    procedure put_BeepDuration(value: TimeSpan); safecall;
    function get_BeepDuration: TimeSpan; safecall;
    procedure put_BeepDelay(value: TimeSpan); safecall;
    function get_BeepDelay: TimeSpan; safecall;
    function add_AlarmTimeoutExpired(handler: TypedEventHandler_2__ICashDrawerCloseAlarm__IInspectable): EventRegistrationToken; safecall;
    procedure remove_AlarmTimeoutExpired(token: EventRegistrationToken); safecall;
    function StartAsync: IAsyncOperation_1__Boolean; safecall;
    property AlarmTimeout: TimeSpan read get_AlarmTimeout write put_AlarmTimeout;
    property BeepDelay: TimeSpan read get_BeepDelay write put_BeepDelay;
    property BeepDuration: TimeSpan read get_BeepDuration write put_BeepDuration;
    property BeepFrequency: Cardinal read get_BeepFrequency write put_BeepFrequency;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Object>
  TypedEventHandler_2__IClaimedLineDisplay__IInspectable_Delegate_Base = interface(IUnknown)
  ['{C997782B-46E9-5D92-AC84-EE9D7D073AB5}']
    procedure Invoke(sender: IClaimedLineDisplay; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedLineDisplay,Object>
  // External 
  TypedEventHandler_2__IClaimedLineDisplay__IInspectable = interface(TypedEventHandler_2__IClaimedLineDisplay__IInspectable_Delegate_Base)
  ['{218B3209-AFA0-5469-841D-5F4F6C95C9D4}']
  end;

  // DualAPI Interface
  // Windows.Devices.PointOfService.IClaimedLineDisplay
  [WinRTClassNameAttribute(SWindows_Devices_PointOfService_ClaimedLineDisplay)]
  IClaimedLineDisplay = interface(IInspectable)
  ['{120AC970-9A75-4ACF-AAE7-09972BCF8794}']
    function get_DeviceId: HSTRING; safecall;
    function get_Capabilities: ILineDisplayCapabilities; safecall;
    function get_PhysicalDeviceName: HSTRING; safecall;
    function get_PhysicalDeviceDescription: HSTRING; safecall;
    function get_DeviceControlDescription: HSTRING; safecall;
    function get_DeviceControlVersion: HSTRING; safecall;
    function get_DeviceServiceVersion: HSTRING; safecall;
    function get_DefaultWindow: ILineDisplayWindow; safecall;
    procedure RetainDevice; safecall;
    function add_ReleaseDeviceRequested(handler: TypedEventHandler_2__IClaimedLineDisplay__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ReleaseDeviceRequested(token: EventRegistrationToken); safecall;
    property Capabilities: ILineDisplayCapabilities read get_Capabilities;
    property DefaultWindow: ILineDisplayWindow read get_DefaultWindow;
    property DeviceControlDescription: HSTRING read get_DeviceControlDescription;
    property DeviceControlVersion: HSTRING read get_DeviceControlVersion;
    property DeviceId: HSTRING read get_DeviceId;
    property DeviceServiceVersion: HSTRING read get_DeviceServiceVersion;
    property PhysicalDeviceDescription: HSTRING read get_PhysicalDeviceDescription;
    property PhysicalDeviceName: HSTRING read get_PhysicalDeviceName;
  end;

  // Windows.Devices.PointOfService.ILineDisplayCapabilities
  // External 
  ILineDisplayCapabilities = interface(IInspectable)
  ['{5A15B5D1-8DC5-4B9C-9172-303E47B70C55}']
    function get_IsStatisticsReportingSupported: Boolean; safecall;
    function get_IsStatisticsUpdatingSupported: Boolean; safecall;
    function get_PowerReportingType: UnifiedPosPowerReportingType; safecall;
    function get_CanChangeScreenSize: Boolean; safecall;
    function get_CanDisplayBitmaps: Boolean; safecall;
    function get_CanReadCharacterAtCursor: Boolean; safecall;
    function get_CanMapCharacterSets: Boolean; safecall;
    function get_CanDisplayCustomGlyphs: Boolean; safecall;
    function get_CanReverse: LineDisplayTextAttributeGranularity; safecall;
    function get_CanBlink: LineDisplayTextAttributeGranularity; safecall;
    function get_CanChangeBlinkRate: Boolean; safecall;
    function get_IsBrightnessSupported: Boolean; safecall;
    function get_IsCursorSupported: Boolean; safecall;
    function get_IsHorizontalMarqueeSupported: Boolean; safecall;
    function get_IsVerticalMarqueeSupported: Boolean; safecall;
    function get_IsInterCharacterWaitSupported: Boolean; safecall;
    function get_SupportedDescriptors: Cardinal; safecall;
    function get_SupportedWindows: Cardinal; safecall;
    property CanBlink: LineDisplayTextAttributeGranularity read get_CanBlink;
    property CanChangeBlinkRate: Boolean read get_CanChangeBlinkRate;
    property CanChangeScreenSize: Boolean read get_CanChangeScreenSize;
    property CanDisplayBitmaps: Boolean read get_CanDisplayBitmaps;
    property CanDisplayCustomGlyphs: Boolean read get_CanDisplayCustomGlyphs;
    property CanMapCharacterSets: Boolean read get_CanMapCharacterSets;
    property CanReadCharacterAtCursor: Boolean read get_CanReadCharacterAtCursor;
    property CanReverse: LineDisplayTextAttributeGranularity read get_CanReverse;
    property IsBrightnessSupported: Boolean read get_IsBrightnessSupported;
    property IsCursorSupported: Boolean read get_IsCursorSupported;
    property IsHorizontalMarqueeSupported: Boolean read get_IsHorizontalMarqueeSupported;
    property IsInterCharacterWaitSupported: Boolean read get_IsInterCharacterWaitSupported;
    property IsStatisticsReportingSupported: Boolean read get_IsStatisticsReportingSupported;
    property IsStatisticsUpdatingSupported: Boolean read get_IsStatisticsUpdatingSupported;
    property IsVerticalMarqueeSupported: Boolean read get_IsVerticalMarqueeSupported;
    property PowerReportingType: UnifiedPosPowerReportingType read get_PowerReportingType;
    property SupportedDescriptors: Cardinal read get_SupportedDescriptors;
    property SupportedWindows: Cardinal read get_SupportedWindows;
  end;

  // Windows.Devices.PointOfService.ILineDisplayWindow
  // External 
  ILineDisplayWindow = interface(IInspectable)
  ['{D21FEEF4-2364-4BE5-BEE1-851680AF4964}']
    function get_SizeInCharacters: TSizeF; safecall;
    function get_InterCharacterWaitInterval: TimeSpan; safecall;
    procedure put_InterCharacterWaitInterval(value: TimeSpan); safecall;
    function TryRefreshAsync: IAsyncOperation_1__Boolean; safecall;
    function TryDisplayTextAsync(text: HSTRING; displayAttribute: LineDisplayTextAttribute): IAsyncOperation_1__Boolean; overload; safecall;
    function TryDisplayTextAsync(text: HSTRING; displayAttribute: LineDisplayTextAttribute; startPosition: TPointF): IAsyncOperation_1__Boolean; overload; safecall;
    function TryDisplayTextAsync(text: HSTRING): IAsyncOperation_1__Boolean; overload; safecall;
    function TryScrollTextAsync(direction: LineDisplayScrollDirection; numberOfColumnsOrRows: Cardinal): IAsyncOperation_1__Boolean; safecall;
    function TryClearTextAsync: IAsyncOperation_1__Boolean; safecall;
    property InterCharacterWaitInterval: TimeSpan read get_InterCharacterWaitInterval write put_InterCharacterWaitInterval;
    property SizeInCharacters: TSizeF read get_SizeInCharacters;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedCashDrawer,Object>
  TypedEventHandler_2__IClaimedCashDrawer__IInspectable_Delegate_Base = interface(IUnknown)
  ['{DB886581-2462-5C81-880C-06112CA70012}']
    procedure Invoke(sender: IClaimedCashDrawer; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.PointOfService.IClaimedCashDrawer,Object>
  // External 
  TypedEventHandler_2__IClaimedCashDrawer__IInspectable = interface(TypedEventHandler_2__IClaimedCashDrawer__IInspectable_Delegate_Base)
  ['{A39ABF81-03ED-5140-831E-33396C6B2E7C}']
  end;

  // Windows.Devices.PointOfService.IClaimedCashDrawer
  // External 
  IClaimedCashDrawer = interface(IInspectable)
  ['{CA3F99AF-ABB8-42C1-8A84-5C66512F5A75}']
    function get_DeviceId: HSTRING; safecall;
    function get_IsEnabled: Boolean; safecall;
    function get_IsDrawerOpen: Boolean; safecall;
    function get_CloseAlarm: ICashDrawerCloseAlarm; safecall;
    function OpenDrawerAsync: IAsyncOperation_1__Boolean; safecall;
    function EnableAsync: IAsyncOperation_1__Boolean; safecall;
    function DisableAsync: IAsyncOperation_1__Boolean; safecall;
    function RetainDeviceAsync: IAsyncOperation_1__Boolean; safecall;
    function ResetStatisticsAsync(statisticsCategories: IIterable_1__HSTRING): IAsyncOperation_1__Boolean; safecall;
    function UpdateStatisticsAsync(statistics: IIterable_1__IKeyValuePair_2__HSTRING__HSTRING): IAsyncOperation_1__Boolean; safecall;
    function add_ReleaseDeviceRequested(handler: TypedEventHandler_2__IClaimedCashDrawer__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ReleaseDeviceRequested(token: EventRegistrationToken); safecall;
    property CloseAlarm: ICashDrawerCloseAlarm read get_CloseAlarm;
    property DeviceId: HSTRING read get_DeviceId;
    property IsDrawerOpen: Boolean read get_IsDrawerOpen;
    property IsEnabled: Boolean read get_IsEnabled;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  IIterable_1__IKeyValuePair_2__HSTRING__HSTRING_Base = interface(IInspectable)
  ['{E9BDAAF0-CBF6-5C72-BE90-29CBF3A1319B}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__HSTRING; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  // External 
  IIterable_1__IKeyValuePair_2__HSTRING__HSTRING = interface(IIterable_1__IKeyValuePair_2__HSTRING__HSTRING_Base)
  ['{E9BDAAF0-CBF6-5C72-BE90-29CBF3A1319B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal_Base = interface(IInspectable)
  ['{6E6E228A-F618-5D33-8523-02D16672665B}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal; safecall;
    function GetResults: IImageScannerScanResult; safecall;
    property Progress: AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  // External 
  IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal = interface(IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal_Base)
  ['{F6E8107E-475B-577F-8806-8B6B046E5833}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal_Delegate_Base = interface(IUnknown)
  ['{D1662BAA-4F20-5D18-97F1-A01A6D0DD980}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal; progressInfo: Cardinal); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  // External 
  AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal = interface(AsyncOperationProgressHandler_2__IImageScannerScanResult__Cardinal_Delegate_Base)
  ['{55F1599D-3490-5BF9-95C5-320EF1652B30}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal_Delegate_Base = interface(IUnknown)
  ['{BD8BDBD8-459A-52DC-B101-75B398A61AEF}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IImageScannerScanResult__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Devices.Scanners.IImageScannerScanResult,UInt32>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal = interface(AsyncOperationWithProgressCompletedHandler_2__IImageScannerScanResult__Cardinal_Delegate_Base)
  ['{0089CC7A-21E3-5AED-B565-F2FB72A84119}']
  end;

  // Windows.Devices.Scanners.IImageScannerScanResult
  // External 
  IImageScannerScanResult = interface(IInspectable)
  ['{C91624CD-9037-4E48-84C1-AC0975076BC5}']
    function get_ScannedFiles: IVectorView_1__IStorageFile; safecall;
    property ScannedFiles: IVectorView_1__IStorageFile read get_ScannedFiles;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  IAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer_Base = interface(IInspectable)
  ['{12F85589-415D-5B5D-B0D0-FDA3B0295ADC}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer; safecall;
    function GetResults: IVectorView_1__ISmsMessage; safecall;
    property Progress: AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  // External 
  IAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer = interface(IAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer_Base)
  ['{12F85589-415D-5B5D-B0D0-FDA3B0295ADC}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer_Delegate_Base = interface(IUnknown)
  ['{3F9D1255-EBF8-569F-91C3-49740D5944CE}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer; progressInfo: Integer); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  // External 
  AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer = interface(AsyncOperationProgressHandler_2__IVectorView_1__ISmsMessage__Integer_Delegate_Base)
  ['{3F9D1255-EBF8-569F-91C3-49740D5944CE}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer_Delegate_Base = interface(IUnknown)
  ['{C0454CFC-2F2F-5E0C-8DE9-58B9E82A03BA}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IVectorView_1__ISmsMessage__Integer; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>,Int32>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer = interface(AsyncOperationWithProgressCompletedHandler_2__IVectorView_1__ISmsMessage__Integer_Delegate_Base)
  ['{C0454CFC-2F2F-5E0C-8DE9-58B9E82A03BA}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Sms.ISmsMessage>
  // External 
  IVectorView_1__ISmsMessage = interface(IInspectable)
  ['{D3ACC5B1-6F85-507E-B40A-6950749B426F}']
    function GetAt(index: Cardinal): ISmsMessage; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ISmsMessage; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PISmsMessage): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Devices.Sms.ISmsMessage
  // External 
  ISmsMessage = interface(IInspectable)
  ['{ED3C5E28-6984-4B07-811D-8D5906ED3CEA}']
    function get_Id: Cardinal; safecall;
    function get_MessageClass: SmsMessageClass; safecall;
    property Id: Cardinal read get_Id;
    property MessageClass: SmsMessageClass read get_MessageClass;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Sms.ISmsDevice2,Object>
  TypedEventHandler_2__ISmsDevice2__IInspectable_Delegate_Base = interface(IUnknown)
  ['{3F3808E6-3DEE-57A6-A88D-BACFB066C7FB}']
    procedure Invoke(sender: ISmsDevice2; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Sms.ISmsDevice2,Object>
  // External 
  TypedEventHandler_2__ISmsDevice2__IInspectable = interface(TypedEventHandler_2__ISmsDevice2__IInspectable_Delegate_Base)
  ['{1FBD11F1-2F5F-569E-8544-6BE9B6E92197}']
  end;

  // DualAPI Interface
  // Windows.Devices.Sms.ISmsDevice2
  [WinRTClassNameAttribute(SWindows_Devices_Sms_SmsDevice2)]
  ISmsDevice2 = interface(IInspectable)
  ['{BD8A5C13-E522-46CB-B8D5-9EAD30FB6C47}']
    function get_SmscAddress: HSTRING; safecall;
    procedure put_SmscAddress(value: HSTRING); safecall;
    function get_DeviceId: HSTRING; safecall;
    function get_ParentDeviceId: HSTRING; safecall;
    function get_AccountPhoneNumber: HSTRING; safecall;
    function get_CellularClass: CellularClass; safecall;
    function get_DeviceStatus: SmsDeviceStatus; safecall;
    function CalculateLength(&message: ISmsMessageBase): SmsEncodedLength; safecall;
    function SendMessageAndGetResultAsync(&message: ISmsMessageBase): IAsyncOperation_1__ISmsSendMessageResult; safecall;
    function add_DeviceStatusChanged(eventHandler: TypedEventHandler_2__ISmsDevice2__IInspectable): EventRegistrationToken; safecall;
    procedure remove_DeviceStatusChanged(eventCookie: EventRegistrationToken); safecall;
    property AccountPhoneNumber: HSTRING read get_AccountPhoneNumber;
    property CellularClass_: CellularClass read get_CellularClass;
    property DeviceId: HSTRING read get_DeviceId;
    property DeviceStatus: SmsDeviceStatus read get_DeviceStatus;
    property ParentDeviceId: HSTRING read get_ParentDeviceId;
    property SmscAddress: HSTRING read get_SmscAddress write put_SmscAddress;
  end;

  // DualAPI Interface
  // Windows.Devices.Sms.ISmsMessageBase
  // External 
  ISmsMessageBase = interface(IInspectable)
  ['{2CF0FE30-FE50-4FC6-AA88-4CCFE27A29EA}']
    function get_MessageType: SmsMessageType; safecall;
    function get_DeviceId: HSTRING; safecall;
    function get_CellularClass: CellularClass; safecall;
    function get_MessageClass: SmsMessageClass; safecall;
    function get_SimIccId: HSTRING; safecall;
    property CellularClass_: CellularClass read get_CellularClass;
    property DeviceId: HSTRING read get_DeviceId;
    property MessageClass: SmsMessageClass read get_MessageClass;
    property MessageType: SmsMessageType read get_MessageType;
    property SimIccId: HSTRING read get_SimIccId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Sms.ISmsSendMessageResult>
  IAsyncOperation_1__ISmsSendMessageResult_Base = interface(IInspectable)
  ['{FC0A0B0F-4DCC-5257-BC61-3435E302CE1F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ISmsSendMessageResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ISmsSendMessageResult; safecall;
    function GetResults: ISmsSendMessageResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ISmsSendMessageResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Sms.ISmsSendMessageResult>
  // External 
  IAsyncOperation_1__ISmsSendMessageResult = interface(IAsyncOperation_1__ISmsSendMessageResult_Base)
  ['{C64F411D-53A8-580B-B467-4722D857F737}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Sms.ISmsSendMessageResult>
  AsyncOperationCompletedHandler_1__ISmsSendMessageResult_Delegate_Base = interface(IUnknown)
  ['{C7D5C6FE-9206-5EB1-ABC1-C1BC21804EEB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ISmsSendMessageResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Sms.ISmsSendMessageResult>
  // External 
  AsyncOperationCompletedHandler_1__ISmsSendMessageResult = interface(AsyncOperationCompletedHandler_1__ISmsSendMessageResult_Delegate_Base)
  ['{3263560A-CB14-50E2-9FFA-6A6FC1405030}']
  end;

  // Windows.Devices.Sms.ISmsSendMessageResult
  // External 
  ISmsSendMessageResult = interface(IInspectable)
  ['{DB139AF2-78C9-4FEB-9622-452328088D62}']
    function get_IsSuccessful: Boolean; safecall;
    function get_MessageReferenceNumbers: IVectorView_1__Integer; safecall;
    function get_CellularClass: CellularClass; safecall;
    function get_ModemErrorCode: SmsModemErrorCode; safecall;
    function get_IsErrorTransient: Boolean; safecall;
    function get_NetworkCauseCode: Integer; safecall;
    function get_TransportFailureCause: Integer; safecall;
    property CellularClass_: CellularClass read get_CellularClass;
    property IsErrorTransient: Boolean read get_IsErrorTransient;
    property IsSuccessful: Boolean read get_IsSuccessful;
    property MessageReferenceNumbers: IVectorView_1__Integer read get_MessageReferenceNumbers;
    property ModemErrorCode: SmsModemErrorCode read get_ModemErrorCode;
    property NetworkCauseCode: Integer read get_NetworkCauseCode;
    property TransportFailureCause: Integer read get_TransportFailureCause;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Gaming.Input.IGameController,Windows.System.IUserChangedEventArgs>
  TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{CB753F2C-2F36-5A8F-ADAD-057BEAE73AA4}']
    procedure Invoke(sender: Input_IGameController; args: IUserChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Gaming.Input.IGameController,Windows.System.IUserChangedEventArgs>
  // External 
  TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs = interface(TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs_Delegate_Base)
  ['{705B706B-C815-51C1-AD7E-B93FEF3BE10C}']
  end;

  // DualAPI Interface
  // Windows.Gaming.Input.IGameController
  // External 
  Input_IGameController = interface(IInspectable)
  ['{1BAF6522-5F64-42C5-8267-B9FE2215BFBD}']
    function add_HeadsetConnected(value: TypedEventHandler_2__Input_IGameController__Input_IHeadset): EventRegistrationToken; safecall;
    procedure remove_HeadsetConnected(token: EventRegistrationToken); safecall;
    function add_HeadsetDisconnected(value: TypedEventHandler_2__Input_IGameController__Input_IHeadset): EventRegistrationToken; safecall;
    procedure remove_HeadsetDisconnected(token: EventRegistrationToken); safecall;
    function add_UserChanged(value: TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_UserChanged(token: EventRegistrationToken); safecall;
    function get_Headset: Input_IHeadset; safecall;
    function get_IsWireless: Boolean; safecall;
    function get_User: IUser; safecall;
    property Headset: Input_IHeadset read get_Headset;
    property IsWireless: Boolean read get_IsWireless;
    property User: IUser read get_User;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Gaming.Input.IGameController,Windows.Gaming.Input.IHeadset>
  TypedEventHandler_2__Input_IGameController__Input_IHeadset_Delegate_Base = interface(IUnknown)
  ['{07B2F2B7-8825-5C4E-A052-FCFEDF3AEEA1}']
    procedure Invoke(sender: Input_IGameController; args: Input_IHeadset); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Gaming.Input.IGameController,Windows.Gaming.Input.IHeadset>
  // External 
  TypedEventHandler_2__Input_IGameController__Input_IHeadset = interface(TypedEventHandler_2__Input_IGameController__Input_IHeadset_Delegate_Base)
  ['{824670C2-00A1-537F-AD0A-E3086418DE81}']
  end;

  // Windows.Gaming.Input.IHeadset
  // External 
  Input_IHeadset = interface(IInspectable)
  ['{3FD156EF-6925-3FA8-9181-029C5223AE3B}']
    function get_CaptureDeviceId: HSTRING; safecall;
    function get_RenderDeviceId: HSTRING; safecall;
    property CaptureDeviceId: HSTRING read get_CaptureDeviceId;
    property RenderDeviceId: HSTRING read get_RenderDeviceId;
  end;

  // Windows.System.IUserChangedEventArgs
  // External 
  IUserChangedEventArgs = interface(IInspectable)
  ['{086459DC-18C6-48DB-BC99-724FB9203CCC}']
    function get_User: IUser; safecall;
    property User: IUser read get_User;
  end;

  // Windows.Devices.Power.IBatteryReport
  // External 
  Power_IBatteryReport = interface(IInspectable)
  ['{C9858C3A-4E13-420A-A8D0-24F18F395401}']
    function get_ChargeRateInMilliwatts: IReference_1__Integer; safecall;
    function get_DesignCapacityInMilliwattHours: IReference_1__Integer; safecall;
    function get_FullChargeCapacityInMilliwattHours: IReference_1__Integer; safecall;
    function get_RemainingCapacityInMilliwattHours: IReference_1__Integer; safecall;
    function get_Status: Power_BatteryStatus; safecall;
    property ChargeRateInMilliwatts: IReference_1__Integer read get_ChargeRateInMilliwatts;
    property DesignCapacityInMilliwattHours: IReference_1__Integer read get_DesignCapacityInMilliwattHours;
    property FullChargeCapacityInMilliwattHours: IReference_1__Integer read get_FullChargeCapacityInMilliwattHours;
    property RemainingCapacityInMilliwattHours: IReference_1__Integer read get_RemainingCapacityInMilliwattHours;
    property Status: Power_BatteryStatus read get_Status;
  end;

  // Windows.Foundation.IReference`1<Int32>
  // External 
  IReference_1__Integer = interface(IInspectable)
  ['{548CEFBD-BC8A-5FA0-8DF2-957440FC8BF4}']
    function get_Value: Integer; safecall;
    property Value: Integer read get_Value;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Haptics.ISimpleHapticsController>
  // External 
  IVectorView_1__Haptics_ISimpleHapticsController = interface(IInspectable)
  ['{1B1C9184-A6EB-52D4-BCBE-5DCBE538DD7F}']
    function GetAt(index: Cardinal): Haptics_ISimpleHapticsController; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Haptics_ISimpleHapticsController; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHaptics_ISimpleHapticsController): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Devices.Haptics.ISimpleHapticsController
  // External 
  Haptics_ISimpleHapticsController = interface(IInspectable)
  ['{3D577EF9-4CEE-11E6-B535-001BDC06AB3B}']
    function get_Id: HSTRING; safecall;
    function get_SupportedFeedback: IVectorView_1__Haptics_ISimpleHapticsControllerFeedback; safecall;
    function get_IsIntensitySupported: Boolean; safecall;
    function get_IsPlayCountSupported: Boolean; safecall;
    function get_IsPlayDurationSupported: Boolean; safecall;
    function get_IsReplayPauseIntervalSupported: Boolean; safecall;
    procedure StopFeedback; safecall;
    procedure SendHapticFeedback(feedback: Haptics_ISimpleHapticsControllerFeedback); overload; safecall;
    procedure SendHapticFeedback(feedback: Haptics_ISimpleHapticsControllerFeedback; intensity: Double); overload; safecall;
    procedure SendHapticFeedbackForDuration(feedback: Haptics_ISimpleHapticsControllerFeedback; intensity: Double; playDuration: TimeSpan); safecall;
    procedure SendHapticFeedbackForPlayCount(feedback: Haptics_ISimpleHapticsControllerFeedback; intensity: Double; playCount: Integer; replayPauseInterval: TimeSpan); safecall;
    property Id: HSTRING read get_Id;
    property IsIntensitySupported: Boolean read get_IsIntensitySupported;
    property IsPlayCountSupported: Boolean read get_IsPlayCountSupported;
    property IsPlayDurationSupported: Boolean read get_IsPlayDurationSupported;
    property IsReplayPauseIntervalSupported: Boolean read get_IsReplayPauseIntervalSupported;
    property SupportedFeedback: IVectorView_1__Haptics_ISimpleHapticsControllerFeedback read get_SupportedFeedback;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Haptics.ISimpleHapticsControllerFeedback>
  // External 
  IVectorView_1__Haptics_ISimpleHapticsControllerFeedback = interface(IInspectable)
  ['{FE0D7BA7-AF19-52A8-A31C-5678ACB7F93A}']
    function GetAt(index: Cardinal): Haptics_ISimpleHapticsControllerFeedback; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Haptics_ISimpleHapticsControllerFeedback; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHaptics_ISimpleHapticsControllerFeedback): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Devices.Haptics.ISimpleHapticsControllerFeedback
  // External 
  Haptics_ISimpleHapticsControllerFeedback = interface(IInspectable)
  ['{3D577EF8-4CEE-11E6-B535-001BDC06AB3B}']
    function get_Waveform: Word; safecall;
    function get_Duration: TimeSpan; safecall;
    property Duration: TimeSpan read get_Duration;
    property Waveform: Word read get_Waveform;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Foundation.Collections.IVectorView`1<String>>
  IMapView_2__HSTRING__IVectorView_1__HSTRING_Base = interface(IInspectable)
  ['{2843D34F-D3E5-5FCA-9FDC-B568DD5C1E64}']
    function Lookup(key: HSTRING): IVectorView_1__HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IVectorView_1__HSTRING; out second: IMapView_2__HSTRING__IVectorView_1__HSTRING); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Foundation.Collections.IVectorView`1<String>>
  // External 
  IMapView_2__HSTRING__IVectorView_1__HSTRING = interface(IMapView_2__HSTRING__IVectorView_1__HSTRING_Base)
  ['{2843D34F-D3E5-5FCA-9FDC-B568DD5C1E64}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFileInputNode,Object>
  TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable_Delegate_Base = interface(IUnknown)
  ['{4481085B-8B8B-5520-9825-E9671DA2A89F}']
    procedure Invoke(sender: Audio_IAudioFileInputNode; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFileInputNode,Object>
  // External 
  TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable = interface(TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable_Delegate_Base)
  ['{C3299F12-9086-5F50-86EB-06FEDA10DF6E}']
  end;

  // Windows.Media.Audio.IAudioFileInputNode
  // External 
  Audio_IAudioFileInputNode = interface(IInspectable)
  ['{905B67C8-6F65-4CD4-8890-4694843C276D}']
    procedure put_PlaybackSpeedFactor(value: Double); safecall;
    function get_PlaybackSpeedFactor: Double; safecall;
    function get_Position: TimeSpan; safecall;
    procedure Seek(position: TimeSpan); safecall;
    function get_StartTime: IReference_1__TimeSpan; safecall;
    procedure put_StartTime(value: IReference_1__TimeSpan); safecall;
    function get_EndTime: IReference_1__TimeSpan; safecall;
    procedure put_EndTime(value: IReference_1__TimeSpan); safecall;
    function get_LoopCount: IReference_1__Integer; safecall;
    procedure put_LoopCount(value: IReference_1__Integer); safecall;
    function get_Duration: TimeSpan; safecall;
    function get_SourceFile: IStorageFile; safecall;
    function add_FileCompleted(handler: TypedEventHandler_2__Audio_IAudioFileInputNode__IInspectable): EventRegistrationToken; safecall;
    procedure remove_FileCompleted(token: EventRegistrationToken); safecall;
    property Duration: TimeSpan read get_Duration;
    property EndTime: IReference_1__TimeSpan read get_EndTime write put_EndTime;
    property LoopCount: IReference_1__Integer read get_LoopCount write put_LoopCount;
    property PlaybackSpeedFactor: Double read get_PlaybackSpeedFactor write put_PlaybackSpeedFactor;
    property Position: TimeSpan read get_Position;
    property SourceFile: IStorageFile read get_SourceFile;
    property StartTime: IReference_1__TimeSpan read get_StartTime write put_StartTime;
  end;

  // Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>
  // External 
  IReference_1__TimeSpan = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: TimeSpan; safecall;
    property Value: TimeSpan read get_Value;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioGraph,Object>
  TypedEventHandler_2__Audio_IAudioGraph__IInspectable_Delegate_Base = interface(IUnknown)
  ['{E1407134-09E7-53DE-B54C-8A0659397B88}']
    procedure Invoke(sender: Audio_IAudioGraph; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioGraph,Object>
  // External 
  TypedEventHandler_2__Audio_IAudioGraph__IInspectable = interface(TypedEventHandler_2__Audio_IAudioGraph__IInspectable_Delegate_Base)
  ['{AD7722FD-63CE-595F-9867-3695871F2152}']
  end;

  // DualAPI Interface
  // Windows.Media.Audio.IAudioGraph
  [WinRTClassNameAttribute(SWindows_Media_Audio_AudioGraph)]
  Audio_IAudioGraph = interface(IInspectable)
  ['{1AD46EED-E48C-4E14-9660-2C4F83E9CDD8}']
    function CreateFrameInputNode: Audio_IAudioFrameInputNode; overload; safecall;
    function CreateFrameInputNode(encodingProperties: IAudioEncodingProperties): Audio_IAudioFrameInputNode; overload; safecall;
    function CreateDeviceInputNodeAsync(category: Capture_MediaCategory): IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult; overload; safecall;
    function CreateDeviceInputNodeAsync(category: Capture_MediaCategory; encodingProperties: IAudioEncodingProperties): IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult; overload; safecall;
    function CreateDeviceInputNodeAsync(category: Capture_MediaCategory; encodingProperties: IAudioEncodingProperties; device: IDeviceInformation): IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult; overload; safecall;
    function CreateFrameOutputNode: Audio_IAudioFrameOutputNode; overload; safecall;
    function CreateFrameOutputNode(encodingProperties: IAudioEncodingProperties): Audio_IAudioFrameOutputNode; overload; safecall;
    function CreateDeviceOutputNodeAsync: IAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult; safecall;
    function CreateFileInputNodeAsync(&file: IStorageFile): IAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult; safecall;
    function CreateFileOutputNodeAsync(&file: IStorageFile): IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult; overload; safecall;
    function CreateFileOutputNodeAsync(&file: IStorageFile; fileEncodingProfile: IMediaEncodingProfile): IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult; overload; safecall;
    function CreateSubmixNode: Audio_IAudioInputNode; overload; safecall;
    function CreateSubmixNode(encodingProperties: IAudioEncodingProperties): Audio_IAudioInputNode; overload; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    procedure ResetAllNodes; safecall;
    function add_QuantumStarted(handler: TypedEventHandler_2__Audio_IAudioGraph__IInspectable): EventRegistrationToken; safecall;
    procedure remove_QuantumStarted(token: EventRegistrationToken); safecall;
    function add_QuantumProcessed(handler: TypedEventHandler_2__Audio_IAudioGraph__IInspectable): EventRegistrationToken; safecall;
    procedure remove_QuantumProcessed(token: EventRegistrationToken); safecall;
    function add_UnrecoverableErrorOccurred(handler: TypedEventHandler_2__Audio_IAudioGraph__Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs): EventRegistrationToken; safecall;
    procedure remove_UnrecoverableErrorOccurred(token: EventRegistrationToken); safecall;
    function get_CompletedQuantumCount: UInt64; safecall;
    function get_EncodingProperties: IAudioEncodingProperties; safecall;
    function get_LatencyInSamples: Integer; safecall;
    function get_PrimaryRenderDevice: IDeviceInformation; safecall;
    function get_RenderDeviceAudioProcessing: AudioProcessing; safecall;
    function get_SamplesPerQuantum: Integer; safecall;
    property CompletedQuantumCount: UInt64 read get_CompletedQuantumCount;
    property EncodingProperties: IAudioEncodingProperties read get_EncodingProperties;
    property LatencyInSamples: Integer read get_LatencyInSamples;
    property PrimaryRenderDevice: IDeviceInformation read get_PrimaryRenderDevice;
    property RenderDeviceAudioProcessing: AudioProcessing read get_RenderDeviceAudioProcessing;
    property SamplesPerQuantum: Integer read get_SamplesPerQuantum;
  end;

  // Windows.Media.Audio.IAudioFrameInputNode
  // External 
  Audio_IAudioFrameInputNode = interface(IInspectable)
  ['{01B266C7-FD96-4FF5-A3C5-D27A9BF44237}']
    procedure put_PlaybackSpeedFactor(value: Double); safecall;
    function get_PlaybackSpeedFactor: Double; safecall;
    procedure AddFrame(frame: IAudioFrame); safecall;
    procedure DiscardQueuedFrames; safecall;
    function get_QueuedSampleCount: UInt64; safecall;
    function add_AudioFrameCompleted(handler: TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IAudioFrameCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AudioFrameCompleted(token: EventRegistrationToken); safecall;
    function add_QuantumStarted(handler: TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IFrameInputNodeQuantumStartedEventArgs): EventRegistrationToken; safecall;
    procedure remove_QuantumStarted(token: EventRegistrationToken); safecall;
    property PlaybackSpeedFactor: Double read get_PlaybackSpeedFactor write put_PlaybackSpeedFactor;
    property QueuedSampleCount: UInt64 read get_QueuedSampleCount;
  end;

  // DualAPI Interface
  // Windows.Media.IAudioFrame
  [WinRTClassNameAttribute(SWindows_Media_AudioFrame)]
  IAudioFrame = interface(IInspectable)
  ['{E36AC304-AAB2-4277-9ED0-43CEDF8E29C6}']
    function LockBuffer(mode: AudioBufferAccessMode): IAudioBuffer; safecall;
  end;

  // Windows.Media.IAudioBuffer
  // External 
  IAudioBuffer = interface(IInspectable)
  ['{35175827-724B-4C6A-B130-F6537F9AE0D0}']
    function get_Capacity: Cardinal; safecall;
    function get_Length: Cardinal; safecall;
    procedure put_Length(value: Cardinal); safecall;
    property Capacity: Cardinal read get_Capacity;
    property Length: Cardinal read get_Length write put_Length;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFrameInputNode,Windows.Media.Audio.IAudioFrameCompletedEventArgs>
  TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IAudioFrameCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{AD59DCFE-71B0-5E16-99C2-CD90644D8EE8}']
    procedure Invoke(sender: Audio_IAudioFrameInputNode; args: Audio_IAudioFrameCompletedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFrameInputNode,Windows.Media.Audio.IAudioFrameCompletedEventArgs>
  // External 
  TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IAudioFrameCompletedEventArgs = interface(TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IAudioFrameCompletedEventArgs_Delegate_Base)
  ['{87136DC3-7179-5D76-AB37-AE1936C73E61}']
  end;

  // Windows.Media.Audio.IAudioFrameCompletedEventArgs
  // External 
  Audio_IAudioFrameCompletedEventArgs = interface(IInspectable)
  ['{DC7C829E-0208-4504-A5A8-F0F268920A65}']
    function get_Frame: IAudioFrame; safecall;
    property Frame: IAudioFrame read get_Frame;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFrameInputNode,Windows.Media.Audio.IFrameInputNodeQuantumStartedEventArgs>
  TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IFrameInputNodeQuantumStartedEventArgs_Delegate_Base = interface(IUnknown)
  ['{4530D121-BB9A-57FE-922F-A98EEEDF59AF}']
    procedure Invoke(sender: Audio_IAudioFrameInputNode; args: Audio_IFrameInputNodeQuantumStartedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioFrameInputNode,Windows.Media.Audio.IFrameInputNodeQuantumStartedEventArgs>
  // External 
  TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IFrameInputNodeQuantumStartedEventArgs = interface(TypedEventHandler_2__Audio_IAudioFrameInputNode__Audio_IFrameInputNodeQuantumStartedEventArgs_Delegate_Base)
  ['{E82D8A90-3812-5930-A62C-BAD415076BE6}']
  end;

  // Windows.Media.Audio.IFrameInputNodeQuantumStartedEventArgs
  // External 
  Audio_IFrameInputNodeQuantumStartedEventArgs = interface(IInspectable)
  ['{3D9BD498-A306-4F06-BD9F-E9EFC8226304}']
    function get_RequiredSamples: Integer; safecall;
    property RequiredSamples: Integer read get_RequiredSamples;
  end;

  // DualAPI Interface
  // Windows.Media.MediaProperties.IAudioEncodingProperties
  [WinRTClassNameAttribute(SWindows_Media_MediaProperties_AudioEncodingProperties)]
  IAudioEncodingProperties = interface(IInspectable)
  ['{62BC7A16-005C-4B3B-8A0B-0A090E9687F3}']
    procedure put_Bitrate(value: Cardinal); safecall;
    function get_Bitrate: Cardinal; safecall;
    procedure put_ChannelCount(value: Cardinal); safecall;
    function get_ChannelCount: Cardinal; safecall;
    procedure put_SampleRate(value: Cardinal); safecall;
    function get_SampleRate: Cardinal; safecall;
    procedure put_BitsPerSample(value: Cardinal); safecall;
    function get_BitsPerSample: Cardinal; safecall;
    property Bitrate: Cardinal read get_Bitrate write put_Bitrate;
    property BitsPerSample: Cardinal read get_BitsPerSample write put_BitsPerSample;
    property ChannelCount: Cardinal read get_ChannelCount write put_ChannelCount;
    property SampleRate: Cardinal read get_SampleRate write put_SampleRate;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioDeviceInputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult_Base = interface(IInspectable)
  ['{71AB4481-EC4A-5EE9-A342-3A31747829B8}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult; safecall;
    function GetResults: Audio_ICreateAudioDeviceInputNodeResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioDeviceInputNodeResult>
  // External 
  IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult = interface(IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult_Base)
  ['{BE15A1DC-4FBB-5A22-8452-881A590E3ACA}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioDeviceInputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult_Delegate_Base = interface(IUnknown)
  ['{6CC56450-E4E8-59C9-83D8-63E46EACB20B}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Audio_ICreateAudioDeviceInputNodeResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioDeviceInputNodeResult>
  // External 
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult = interface(AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceInputNodeResult_Delegate_Base)
  ['{D3FBAF3D-B1BE-5447-A6B8-A24B9266B30E}']
  end;

  // Windows.Media.Audio.ICreateAudioDeviceInputNodeResult
  // External 
  Audio_ICreateAudioDeviceInputNodeResult = interface(IInspectable)
  ['{16EEC7A8-1CA7-40EF-91A4-D346E0AA1BBA}']
    function get_Status: Audio_AudioDeviceNodeCreationStatus; safecall;
    function get_DeviceInputNode: Audio_IAudioDeviceInputNode; safecall;
    property DeviceInputNode: Audio_IAudioDeviceInputNode read get_DeviceInputNode;
    property Status: Audio_AudioDeviceNodeCreationStatus read get_Status;
  end;

  // Windows.Media.Audio.IAudioDeviceInputNode
  // External 
  Audio_IAudioDeviceInputNode = interface(IInspectable)
  ['{B01B6BE1-6F4E-49E2-AC01-559D62BEB3A9}']
    function get_Device: IDeviceInformation; safecall;
    property Device: IDeviceInformation read get_Device;
  end;

  // Windows.Media.Audio.IAudioFrameOutputNode
  // External 
  Audio_IAudioFrameOutputNode = interface(IInspectable)
  ['{B847371B-3299-45F5-88B3-C9D12A3F1CC8}']
    function GetFrame: IAudioFrame; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult_Base = interface(IInspectable)
  ['{F810D730-DE15-58E0-A5F4-C159F73669ED}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult; safecall;
    function GetResults: Audio_ICreateAudioDeviceOutputNodeResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult>
  // External 
  IAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult = interface(IAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult_Base)
  ['{4CB6F751-D460-5037-AA59-E0B2314F948F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult_Delegate_Base = interface(IUnknown)
  ['{EDBC9B59-7CAE-513F-B0DC-17666D37BA77}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Audio_ICreateAudioDeviceOutputNodeResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult>
  // External 
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult = interface(AsyncOperationCompletedHandler_1__Audio_ICreateAudioDeviceOutputNodeResult_Delegate_Base)
  ['{8350138E-5DD6-5DC0-AEF7-2AB7C058CF3E}']
  end;

  // Windows.Media.Audio.ICreateAudioDeviceOutputNodeResult
  // External 
  Audio_ICreateAudioDeviceOutputNodeResult = interface(IInspectable)
  ['{F7776D27-1D9A-47F7-9CD4-2859CC1B7BFF}']
    function get_Status: Audio_AudioDeviceNodeCreationStatus; safecall;
    function get_DeviceOutputNode: Audio_IAudioDeviceOutputNode; safecall;
    property DeviceOutputNode: Audio_IAudioDeviceOutputNode read get_DeviceOutputNode;
    property Status: Audio_AudioDeviceNodeCreationStatus read get_Status;
  end;

  // Windows.Media.Audio.IAudioDeviceOutputNode
  // External 
  Audio_IAudioDeviceOutputNode = interface(IInspectable)
  ['{362EDBFF-FF1C-4434-9E0F-BD2EF522AC82}']
    function get_Device: IDeviceInformation; safecall;
    property Device: IDeviceInformation read get_Device;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioFileInputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult_Base = interface(IInspectable)
  ['{473B06BF-387B-56CA-BEE1-527480272B0F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult; safecall;
    function GetResults: Audio_ICreateAudioFileInputNodeResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioFileInputNodeResult>
  // External 
  IAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult = interface(IAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult_Base)
  ['{09523B34-2247-58D1-ADDE-D388D76B966B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioFileInputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult_Delegate_Base = interface(IUnknown)
  ['{504D1EFD-C11C-506E-B8C9-AF17C771EFB5}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Audio_ICreateAudioFileInputNodeResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioFileInputNodeResult>
  // External 
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult = interface(AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileInputNodeResult_Delegate_Base)
  ['{74969811-B1BE-55AF-BE81-9F9280C90FB8}']
  end;

  // Windows.Media.Audio.ICreateAudioFileInputNodeResult
  // External 
  Audio_ICreateAudioFileInputNodeResult = interface(IInspectable)
  ['{CE83D61C-E297-4C50-9CE7-1C7A69D6BD09}']
    function get_Status: Audio_AudioFileNodeCreationStatus; safecall;
    function get_FileInputNode: Audio_IAudioFileInputNode; safecall;
    property FileInputNode: Audio_IAudioFileInputNode read get_FileInputNode;
    property Status: Audio_AudioFileNodeCreationStatus read get_Status;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioFileOutputNodeResult>
  IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult_Base = interface(IInspectable)
  ['{1164517D-E953-5415-A5B3-4249A969BE7B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult; safecall;
    function GetResults: Audio_ICreateAudioFileOutputNodeResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Audio.ICreateAudioFileOutputNodeResult>
  // External 
  IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult = interface(IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult_Base)
  ['{4D516665-1D50-592C-9E86-75AF69A44B55}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioFileOutputNodeResult>
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult_Delegate_Base = interface(IUnknown)
  ['{A7A95713-A08F-5FDF-89C6-9627BCF5D80A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Audio_ICreateAudioFileOutputNodeResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Audio.ICreateAudioFileOutputNodeResult>
  // External 
  AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult = interface(AsyncOperationCompletedHandler_1__Audio_ICreateAudioFileOutputNodeResult_Delegate_Base)
  ['{7A7D880E-AFF4-5211-951C-D11439458145}']
  end;

  // Windows.Media.Audio.ICreateAudioFileOutputNodeResult
  // External 
  Audio_ICreateAudioFileOutputNodeResult = interface(IInspectable)
  ['{47D6BA7B-E909-453F-866E-5540CDA734FF}']
    function get_Status: Audio_AudioFileNodeCreationStatus; safecall;
    function get_FileOutputNode: Audio_IAudioFileOutputNode; safecall;
    property FileOutputNode: Audio_IAudioFileOutputNode read get_FileOutputNode;
    property Status: Audio_AudioFileNodeCreationStatus read get_Status;
  end;

  // Windows.Media.Audio.IAudioFileOutputNode
  // External 
  Audio_IAudioFileOutputNode = interface(IInspectable)
  ['{50E01980-5166-4093-80F8-ADA00089E9CF}']
    function get_File: IStorageFile; safecall;
    function get_FileEncodingProfile: IMediaEncodingProfile; safecall;
    function FinalizeAsync: IAsyncOperation_1__Transcoding_TranscodeFailureReason; safecall;
    property &File: IStorageFile read get_File;
    property FileEncodingProfile: IMediaEncodingProfile read get_FileEncodingProfile;
  end;

  // DualAPI Interface
  // Windows.Media.MediaProperties.IMediaEncodingProfile
  [WinRTClassNameAttribute(SWindows_Media_MediaProperties_MediaEncodingProfile)]
  IMediaEncodingProfile = interface(IInspectable)
  ['{E7DBF5A8-1DB9-4783-876B-3DFE12ACFDB3}']
    procedure put_Audio(value: IAudioEncodingProperties); safecall;
    function get_Audio: IAudioEncodingProperties; safecall;
    procedure put_Video(value: IVideoEncodingProperties); safecall;
    function get_Video: IVideoEncodingProperties; safecall;
    procedure put_Container(value: IContainerEncodingProperties); safecall;
    function get_Container: IContainerEncodingProperties; safecall;
    property Audio: IAudioEncodingProperties read get_Audio write put_Audio;
    property Container: IContainerEncodingProperties read get_Container write put_Container;
    property Video: IVideoEncodingProperties read get_Video write put_Video;
  end;

  // DualAPI Interface
  // Windows.Media.MediaProperties.IVideoEncodingProperties
  [WinRTClassNameAttribute(SWindows_Media_MediaProperties_VideoEncodingProperties)]
  IVideoEncodingProperties = interface(IInspectable)
  ['{76EE6C9A-37C2-4F2A-880A-1282BBB4373D}']
    procedure put_Bitrate(value: Cardinal); safecall;
    function get_Bitrate: Cardinal; safecall;
    procedure put_Width(value: Cardinal); safecall;
    function get_Width: Cardinal; safecall;
    procedure put_Height(value: Cardinal); safecall;
    function get_Height: Cardinal; safecall;
    function get_FrameRate: IMediaRatio; safecall;
    function get_PixelAspectRatio: IMediaRatio; safecall;
    property Bitrate: Cardinal read get_Bitrate write put_Bitrate;
    property FrameRate: IMediaRatio read get_FrameRate;
    property Height: Cardinal read get_Height write put_Height;
    property PixelAspectRatio: IMediaRatio read get_PixelAspectRatio;
    property Width: Cardinal read get_Width write put_Width;
  end;

  // Windows.Media.MediaProperties.IMediaRatio
  // External 
  IMediaRatio = interface(IInspectable)
  ['{D2D0FEE5-8929-401D-AC78-7D357E378163}']
    procedure put_Numerator(value: Cardinal); safecall;
    function get_Numerator: Cardinal; safecall;
    procedure put_Denominator(value: Cardinal); safecall;
    function get_Denominator: Cardinal; safecall;
    property Denominator: Cardinal read get_Denominator write put_Denominator;
    property Numerator: Cardinal read get_Numerator write put_Numerator;
  end;

  // DualAPI Interface
  // Windows.Media.MediaProperties.IContainerEncodingProperties
  [WinRTClassNameAttribute(SWindows_Media_MediaProperties_ContainerEncodingProperties)]
  IContainerEncodingProperties = interface(IInspectable)
  ['{59AC2A57-B32A-479E-8A61-4B7F2E9E7EA0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Transcoding.TranscodeFailureReason>
  IAsyncOperation_1__Transcoding_TranscodeFailureReason_Base = interface(IInspectable)
  ['{02132510-3899-5257-BED9-A43E5149D28C}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason; safecall;
    function GetResults: Transcoding_TranscodeFailureReason; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Transcoding.TranscodeFailureReason>
  // External 
  IAsyncOperation_1__Transcoding_TranscodeFailureReason = interface(IAsyncOperation_1__Transcoding_TranscodeFailureReason_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Transcoding.TranscodeFailureReason>
  AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason_Delegate_Base = interface(IUnknown)
  ['{C42AE2BF-E194-5179-B8AD-03B51C04E1DA}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Transcoding_TranscodeFailureReason; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Transcoding.TranscodeFailureReason>
  // External 
  AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason = interface(AsyncOperationCompletedHandler_1__Transcoding_TranscodeFailureReason_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Windows.Media.Audio.IAudioInputNode
  // External 
  Audio_IAudioInputNode = interface(IInspectable)
  ['{D148005C-8428-4784-B7FD-A99D468C5D20}']
    function get_OutgoingConnections: IVectorView_1__Audio_IAudioGraphConnection; safecall;
    procedure AddOutgoingConnection(destination: Audio_IAudioNode); overload; safecall;
    procedure AddOutgoingConnection(destination: Audio_IAudioNode; gain: Double); overload; safecall;
    procedure RemoveOutgoingConnection(destination: Audio_IAudioNode); safecall;
    property OutgoingConnections: IVectorView_1__Audio_IAudioGraphConnection read get_OutgoingConnections;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Audio.IAudioGraphConnection>
  // External 
  IVectorView_1__Audio_IAudioGraphConnection = interface(IInspectable)
  ['{56CA9249-0753-5D4C-B9AC-581BFD70E8C0}']
    function GetAt(index: Cardinal): Audio_IAudioGraphConnection; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Audio_IAudioGraphConnection; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAudio_IAudioGraphConnection): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Audio.IAudioGraphConnection
  // External 
  Audio_IAudioGraphConnection = interface(IInspectable)
  ['{763070ED-D04E-4FAC-B233-600B42EDD469}']
    function get_Destination: Audio_IAudioNode; safecall;
    procedure put_Gain(value: Double); safecall;
    function get_Gain: Double; safecall;
    property Destination: Audio_IAudioNode read get_Destination;
    property Gain: Double read get_Gain write put_Gain;
  end;

  // Windows.Media.Audio.IAudioNode
  // External 
  Audio_IAudioNode = interface(IInspectable)
  ['{15389D7F-DBD8-4819-BF03-668E9357CD6D}']
    function get_EffectDefinitions: IVector_1__Effects_IAudioEffectDefinition; safecall;
    procedure put_OutgoingGain(value: Double); safecall;
    function get_OutgoingGain: Double; safecall;
    function get_EncodingProperties: IAudioEncodingProperties; safecall;
    function get_ConsumeInput: Boolean; safecall;
    procedure put_ConsumeInput(value: Boolean); safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    procedure Reset; safecall;
    procedure DisableEffectsByDefinition(definition: Effects_IAudioEffectDefinition); safecall;
    procedure EnableEffectsByDefinition(definition: Effects_IAudioEffectDefinition); safecall;
    property ConsumeInput: Boolean read get_ConsumeInput write put_ConsumeInput;
    property EffectDefinitions: IVector_1__Effects_IAudioEffectDefinition read get_EffectDefinitions;
    property EncodingProperties: IAudioEncodingProperties read get_EncodingProperties;
    property OutgoingGain: Double read get_OutgoingGain write put_OutgoingGain;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Effects.IAudioEffectDefinition>
  IVector_1__Effects_IAudioEffectDefinition_Base = interface(IInspectable)
  ['{2828A982-D849-5FC9-84CE-F9A4B3B4D341}']
    function GetAt(index: Cardinal): Effects_IAudioEffectDefinition; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Effects_IAudioEffectDefinition; safecall;
    function IndexOf(value: Effects_IAudioEffectDefinition; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Effects_IAudioEffectDefinition); safecall;
    procedure InsertAt(index: Cardinal; value: Effects_IAudioEffectDefinition); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Effects_IAudioEffectDefinition); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PEffects_IAudioEffectDefinition): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PEffects_IAudioEffectDefinition); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Effects.IAudioEffectDefinition>
  // External 
  IVector_1__Effects_IAudioEffectDefinition = interface(IVector_1__Effects_IAudioEffectDefinition_Base)
  ['{2828A982-D849-5FC9-84CE-F9A4B3B4D341}']
  end;

  // DualAPI Interface
  // Windows.Media.Effects.IAudioEffectDefinition
  // External 
  Effects_IAudioEffectDefinition = interface(IInspectable)
  ['{E4D7F974-7D80-4F73-9089-E31C9DB9C294}']
    function get_ActivatableClassId: HSTRING; safecall;
    function get_Properties: IPropertySet; safecall;
    property ActivatableClassId: HSTRING read get_ActivatableClassId;
    property Properties: IPropertySet read get_Properties;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Effects.IAudioEffectDefinition>
  // External 
  IVectorView_1__Effects_IAudioEffectDefinition = interface(IInspectable)
  ['{DE9E6A7F-D28E-5EF1-916A-EFA880B489D1}']
    function GetAt(index: Cardinal): Effects_IAudioEffectDefinition; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Effects_IAudioEffectDefinition; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PEffects_IAudioEffectDefinition): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioGraph,Windows.Media.Audio.IAudioGraphUnrecoverableErrorOccurredEventArgs>
  TypedEventHandler_2__Audio_IAudioGraph__Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs_Delegate_Base = interface(IUnknown)
  ['{899670C9-DD7F-5F12-98CB-8B17FE80A47F}']
    procedure Invoke(sender: Audio_IAudioGraph; args: Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Audio.IAudioGraph,Windows.Media.Audio.IAudioGraphUnrecoverableErrorOccurredEventArgs>
  // External 
  TypedEventHandler_2__Audio_IAudioGraph__Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs = interface(TypedEventHandler_2__Audio_IAudioGraph__Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs_Delegate_Base)
  ['{097D4FD7-937E-5B2E-BB98-38E78447F22B}']
  end;

  // Windows.Media.Audio.IAudioGraphUnrecoverableErrorOccurredEventArgs
  // External 
  Audio_IAudioGraphUnrecoverableErrorOccurredEventArgs = interface(IInspectable)
  ['{C3D9CBE0-3FF6-4FB3-B262-50D435C55423}']
    function get_Error: Audio_AudioGraphUnrecoverableError; safecall;
    property Error: Audio_AudioGraphUnrecoverableError read get_Error;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>
  // External 
  IKeyValuePair_2__TGuid__IInspectable = interface(IInspectable)
  ['{3BDA1540-D089-5A1A-8F0D-94EBA8068E58}']
    function get_Key: TGuid; safecall;
    function get_Value: IInspectable; safecall;
    property Key: TGuid read get_Key;
    property Value: IInspectable read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>>
  IIterator_1__IKeyValuePair_2__TGuid__IInspectable_Base = interface(IInspectable)
  ['{4F25059A-0B9A-5F25-9B9E-4B9F1D22FF65}']
    function get_Current: IKeyValuePair_2__TGuid__IInspectable; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__TGuid__IInspectable): Cardinal; safecall;
    property Current: IKeyValuePair_2__TGuid__IInspectable read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Object>>
  // External 
  IIterator_1__IKeyValuePair_2__TGuid__IInspectable = interface(IIterator_1__IKeyValuePair_2__TGuid__IInspectable_Base)
  ['{4F25059A-0B9A-5F25-9B9E-4B9F1D22FF65}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<Guid,Object>
  IMapView_2__TGuid__IInspectable_Base = interface(IInspectable)
  ['{E4D2C732-BBC1-5EF4-869F-5007CEB55F6E}']
    function Lookup(key: TGuid): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: TGuid): Boolean; safecall;
    procedure Split(out first: IMapView_2__TGuid__IInspectable; out second: IMapView_2__TGuid__IInspectable); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<Guid,Object>
  // External 
  IMapView_2__TGuid__IInspectable = interface(IMapView_2__TGuid__IInspectable_Base)
  ['{E4D2C732-BBC1-5EF4-869F-5007CEB55F6E}']
  end;

  // Windows.Foundation.IReference`1<Boolean>
  // External 
  IReference_1__Boolean = interface(IInspectable)
  ['{3C00FD60-2950-5939-A21A-2D12C5A01B8A}']
    function get_Value: Boolean; safecall;
    property Value: Boolean read get_Value;
  end;

  // Windows.Foundation.Collections.IMap`2<Guid,Object>
  [WinRTClassNameAttribute(SWindows_Media_MediaProperties_MediaPropertySet)]
  IMap_2__TGuid__IInspectable = interface(IInspectable)
  ['{5EE3189C-7DBF-5998-AD07-5414FB82567C}']
    function Lookup(key: TGuid): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: TGuid): Boolean; safecall;
    function GetView: IMapView_2__TGuid__IInspectable; safecall;
    function Insert(key: TGuid; value: IInspectable): Boolean; safecall;
    procedure Remove(key: TGuid); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Playback.IMediaPlaybackItem,Windows.Foundation.Collections.IVectorChangedEventArgs>
  TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{2DD51918-4525-5BE5-A6C1-409BC72863A6}']
    procedure Invoke(sender: Playback_IMediaPlaybackItem; args: IVectorChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Playback.IMediaPlaybackItem,Windows.Foundation.Collections.IVectorChangedEventArgs>
  // External 
  TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs = interface(TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs_Delegate_Base)
  ['{782906DD-5011-5A36-9331-14044DF71A56}']
  end;

  // DualAPI Interface
  // Windows.Media.Playback.IMediaPlaybackItem
  [WinRTClassNameAttribute(SWindows_Media_Playback_MediaPlaybackItem)]
  Playback_IMediaPlaybackItem = interface(IInspectable)
  ['{047097D2-E4AF-48AB-B283-6929E674ECE2}']
    function add_AudioTracksChanged(handler: TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AudioTracksChanged(token: EventRegistrationToken); safecall;
    function add_VideoTracksChanged(handler: TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_VideoTracksChanged(token: EventRegistrationToken); safecall;
    function add_TimedMetadataTracksChanged(handler: TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_TimedMetadataTracksChanged(token: EventRegistrationToken); safecall;
    function get_Source: Core_IMediaSource2; safecall;
    function get_AudioTracks: IVectorView_1__Core_IMediaTrack; safecall;
    function get_VideoTracks: IVectorView_1__Core_IMediaTrack; safecall;
    function get_TimedMetadataTracks: IVectorView_1__Core_ITimedMetadataTrack; safecall;
    property AudioTracks: IVectorView_1__Core_IMediaTrack read get_AudioTracks;
    property Source: Core_IMediaSource2 read get_Source;
    property TimedMetadataTracks: IVectorView_1__Core_ITimedMetadataTrack read get_TimedMetadataTracks;
    property VideoTracks: IVectorView_1__Core_IMediaTrack read get_VideoTracks;
  end;

  // DualAPI Interface
  // Windows.Media.Core.IMediaSource2
  // External 
  Core_IMediaSource2 = interface(IInspectable)
  ['{2EB61048-655F-4C37-B813-B4E45DFA0ABE}']
    function add_OpenOperationCompleted(handler: TypedEventHandler_2__Core_IMediaSource2__Core_IMediaSourceOpenOperationCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_OpenOperationCompleted(token: EventRegistrationToken); safecall;
    function get_CustomProperties: IPropertySet; safecall;
    function get_Duration: IReference_1__TimeSpan; safecall;
    function get_IsOpen: Boolean; safecall;
    function get_ExternalTimedTextSources: IObservableVector_1__Core_ITimedTextSource; safecall;
    function get_ExternalTimedMetadataTracks: IObservableVector_1__Core_ITimedMetadataTrack; safecall;
    property CustomProperties: IPropertySet read get_CustomProperties;
    property Duration: IReference_1__TimeSpan read get_Duration;
    property ExternalTimedMetadataTracks: IObservableVector_1__Core_ITimedMetadataTrack read get_ExternalTimedMetadataTracks;
    property ExternalTimedTextSources: IObservableVector_1__Core_ITimedTextSource read get_ExternalTimedTextSources;
    property IsOpen: Boolean read get_IsOpen;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.IMediaSource2,Windows.Media.Core.IMediaSourceOpenOperationCompletedEventArgs>
  // External 
  TypedEventHandler_2__Core_IMediaSource2__Core_IMediaSourceOpenOperationCompletedEventArgs = interface(IUnknown)
  ['{2292B367-0CE9-58B0-978A-9149FFB031FB}']
    procedure Invoke(sender: Core_IMediaSource2; args: Core_IMediaSourceOpenOperationCompletedEventArgs); safecall;
  end;

  // Windows.Media.Core.IMediaSourceOpenOperationCompletedEventArgs
  // External 
  Core_IMediaSourceOpenOperationCompletedEventArgs = interface(IInspectable)
  ['{FC682CEB-E281-477C-A8E0-1ACD654114C8}']
    function get_Error: Core_IMediaSourceError; safecall;
    property Error: Core_IMediaSourceError read get_Error;
  end;

  // Windows.Media.Core.IMediaSourceError
  // External 
  Core_IMediaSourceError = interface(IInspectable)
  ['{5C0A8965-37C5-4E9D-8D21-1CDEE90CECC6}']
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
  end;

  // Windows.Foundation.Collections.IObservableVector`1<Windows.Media.Core.ITimedTextSource>
  // External 
  IObservableVector_1__Core_ITimedTextSource = interface(IInspectable)
  ['{0C98C465-6D21-5D0F-9682-7337B29F0B91}']
    function add_VectorChanged(vhnd: VectorChangedEventHandler_1__Core_ITimedTextSource): EventRegistrationToken; safecall;
    procedure remove_VectorChanged(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Media.Core.ITimedTextSource>
  VectorChangedEventHandler_1__Core_ITimedTextSource_Delegate_Base = interface(IUnknown)
  ['{E12E6261-B198-56E0-9822-BA2D3BB6F8FE}']
    procedure Invoke(sender: IObservableVector_1__Core_ITimedTextSource; event: IVectorChangedEventArgs); safecall;
  end;
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Media.Core.ITimedTextSource>
  // External 
  VectorChangedEventHandler_1__Core_ITimedTextSource = interface(VectorChangedEventHandler_1__Core_ITimedTextSource_Delegate_Base)
  ['{3B4D630C-11EC-51AD-8B8C-CD2979D73A87}']
  end;

  // Windows.Foundation.Collections.IObservableVector`1<Windows.Media.Core.ITimedMetadataTrack>
  // External 
  IObservableVector_1__Core_ITimedMetadataTrack = interface(IInspectable)
  ['{19EAC50E-BEBF-58B6-B8B7-DB6B157EEAEC}']
    function add_VectorChanged(vhnd: VectorChangedEventHandler_1__Core_ITimedMetadataTrack): EventRegistrationToken; safecall;
    procedure remove_VectorChanged(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Media.Core.ITimedMetadataTrack>
  VectorChangedEventHandler_1__Core_ITimedMetadataTrack_Delegate_Base = interface(IUnknown)
  ['{BD1DC81C-23C4-5B4B-AC38-C5D34CDF34E4}']
    procedure Invoke(sender: IObservableVector_1__Core_ITimedMetadataTrack; event: IVectorChangedEventArgs); safecall;
  end;
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Media.Core.ITimedMetadataTrack>
  // External 
  VectorChangedEventHandler_1__Core_ITimedMetadataTrack = interface(VectorChangedEventHandler_1__Core_ITimedMetadataTrack_Delegate_Base)
  ['{A2A777D5-776B-5806-A334-2193F4ECF158}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IMediaTrack>
  // External 
  IVectorView_1__Core_IMediaTrack = interface(IInspectable)
  ['{1CE05BEC-9E9B-5108-9D24-80E1C8C2866E}']
    function GetAt(index: Cardinal): Core_IMediaTrack; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_IMediaTrack; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IMediaTrack): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Media.Core.IMediaTrack
  // External 
  Core_IMediaTrack = interface(IInspectable)
  ['{03E1FAFC-C931-491A-B46B-C10EE8C256B7}']
    function get_Id: HSTRING; safecall;
    function get_Language: HSTRING; safecall;
    function get_TrackKind: Core_MediaTrackKind; safecall;
    procedure put_Label(value: HSTRING); safecall;
    function get_Label: HSTRING; safecall;
    property Id: HSTRING read get_Id;
    property &Label: HSTRING read get_Label write put_Label;
    property Language: HSTRING read get_Language;
    property TrackKind: Core_MediaTrackKind read get_TrackKind;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.ITimedMetadataTrack>
  // External 
  IVectorView_1__Core_ITimedMetadataTrack = interface(IInspectable)
  ['{DD588DAC-C2FE-523F-B519-6DDEAA5281A9}']
    function GetAt(index: Cardinal): Core_ITimedMetadataTrack; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_ITimedMetadataTrack; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_ITimedMetadataTrack): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Media.Core.ITimedMetadataTrack
  [WinRTClassNameAttribute(SWindows_Media_Core_TimedMetadataTrack)]
  Core_ITimedMetadataTrack = interface(IInspectable)
  ['{9E6AED9E-F67A-49A9-B330-CF03B0E9CF07}']
    function add_CueEntered(handler: TypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs): EventRegistrationToken; safecall;
    procedure remove_CueEntered(token: EventRegistrationToken); safecall;
    function add_CueExited(handler: TypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs): EventRegistrationToken; safecall;
    procedure remove_CueExited(token: EventRegistrationToken); safecall;
    function add_TrackFailed(handler: TypedEventHandler_2__Core_ITimedMetadataTrack__Core_ITimedMetadataTrackFailedEventArgs): EventRegistrationToken; safecall;
    procedure remove_TrackFailed(token: EventRegistrationToken); safecall;
    function get_Cues: IVectorView_1__Core_IMediaCue; safecall;
    function get_ActiveCues: IVectorView_1__Core_IMediaCue; safecall;
    function get_TimedMetadataKind: Core_TimedMetadataKind; safecall;
    function get_DispatchType: HSTRING; safecall;
    procedure AddCue(cue: Core_IMediaCue); safecall;
    procedure RemoveCue(cue: Core_IMediaCue); safecall;
    property ActiveCues: IVectorView_1__Core_IMediaCue read get_ActiveCues;
    property Cues: IVectorView_1__Core_IMediaCue read get_Cues;
    property DispatchType: HSTRING read get_DispatchType;
    property TimedMetadataKind: Core_TimedMetadataKind read get_TimedMetadataKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.ITimedMetadataTrack,Windows.Media.Core.IMediaCueEventArgs>
  TypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs_Delegate_Base = interface(IUnknown)
  ['{4AAC9411-C355-5C95-8C78-5A0F5CA1A54D}']
    procedure Invoke(sender: Core_ITimedMetadataTrack; args: Core_IMediaCueEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.ITimedMetadataTrack,Windows.Media.Core.IMediaCueEventArgs>
  // External 
  TypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs = interface(TypedEventHandler_2__Core_ITimedMetadataTrack__Core_IMediaCueEventArgs_Delegate_Base)
  ['{8994D194-4D58-5DD5-87F6-12576E1F75B5}']
  end;

  // Windows.Media.Core.IMediaCueEventArgs
  // External 
  Core_IMediaCueEventArgs = interface(IInspectable)
  ['{D12F47F7-5FA4-4E68-9FE5-32160DCEE57E}']
    function get_Cue: Core_IMediaCue; safecall;
    property Cue: Core_IMediaCue read get_Cue;
  end;

  // DualAPI Interface
  // Windows.Media.Core.IMediaCue
  // External 
  Core_IMediaCue = interface(IInspectable)
  ['{C7D15E5D-59DC-431F-A0EE-27744323B36D}']
    procedure put_StartTime(value: TimeSpan); safecall;
    function get_StartTime: TimeSpan; safecall;
    procedure put_Duration(value: TimeSpan); safecall;
    function get_Duration: TimeSpan; safecall;
    procedure put_Id(value: HSTRING); safecall;
    function get_Id: HSTRING; safecall;
    property Duration: TimeSpan read get_Duration write put_Duration;
    property Id: HSTRING read get_Id write put_Id;
    property StartTime: TimeSpan read get_StartTime write put_StartTime;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.ITimedMetadataTrack,Windows.Media.Core.ITimedMetadataTrackFailedEventArgs>
  TypedEventHandler_2__Core_ITimedMetadataTrack__Core_ITimedMetadataTrackFailedEventArgs_Delegate_Base = interface(IUnknown)
  ['{8DC73162-255B-532E-B0C7-9A6D70B4BB9E}']
    procedure Invoke(sender: Core_ITimedMetadataTrack; args: Core_ITimedMetadataTrackFailedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Core.ITimedMetadataTrack,Windows.Media.Core.ITimedMetadataTrackFailedEventArgs>
  // External 
  TypedEventHandler_2__Core_ITimedMetadataTrack__Core_ITimedMetadataTrackFailedEventArgs = interface(TypedEventHandler_2__Core_ITimedMetadataTrack__Core_ITimedMetadataTrackFailedEventArgs_Delegate_Base)
  ['{93A88A48-6D62-56F5-AB42-286248198B55}']
  end;

  // Windows.Media.Core.ITimedMetadataTrackFailedEventArgs
  // External 
  Core_ITimedMetadataTrackFailedEventArgs = interface(IInspectable)
  ['{A57FC9D1-6789-4D4D-B07F-84B4F31ACB70}']
    function get_Error: Core_ITimedMetadataTrackError; safecall;
    property Error: Core_ITimedMetadataTrackError read get_Error;
  end;

  // Windows.Media.Core.ITimedMetadataTrackError
  // External 
  Core_ITimedMetadataTrackError = interface(IInspectable)
  ['{B3767915-4114-4819-B9D9-DD76089E72F8}']
    function get_ErrorCode: Core_TimedMetadataTrackErrorCode; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property ErrorCode: Core_TimedMetadataTrackErrorCode read get_ErrorCode;
    property ExtendedError: HRESULT read get_ExtendedError;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IMediaCue>
  // External 
  IVectorView_1__Core_IMediaCue = interface(IInspectable)
  ['{996421A1-625C-5D92-AD68-8B3911D294B0}']
    function GetAt(index: Cardinal): Core_IMediaCue; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_IMediaCue; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IMediaCue): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt64>
  IAsyncOperationWithProgress_2__IBuffer__UInt64_Base = interface(IInspectable)
  ['{AD960E7F-D73B-56E4-A58C-6EC7678CFD88}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IBuffer__UInt64); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IBuffer__UInt64; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64; safecall;
    function GetResults: IBuffer; safecall;
    property Progress: AsyncOperationProgressHandler_2__IBuffer__UInt64 read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64 read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt64>
  // External 
  IAsyncOperationWithProgress_2__IBuffer__UInt64 = interface(IAsyncOperationWithProgress_2__IBuffer__UInt64_Base)
  ['{AD960E7F-D73B-56E4-A58C-6EC7678CFD88}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt64>
  AsyncOperationProgressHandler_2__IBuffer__UInt64_Delegate_Base = interface(IUnknown)
  ['{D17F5EB6-B422-5E26-A817-7E0FD08F75D5}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__UInt64; progressInfo: UInt64); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt64>
  // External 
  AsyncOperationProgressHandler_2__IBuffer__UInt64 = interface(AsyncOperationProgressHandler_2__IBuffer__UInt64_Delegate_Base)
  ['{D17F5EB6-B422-5E26-A817-7E0FD08F75D5}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64_Delegate_Base = interface(IUnknown)
  ['{ABC81235-39C7-59BF-9948-2D14A93D40FD}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__UInt64; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt64>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64 = interface(AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64_Delegate_Base)
  ['{ABC81235-39C7-59BF-9948-2D14A93D40FD}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IInputStream,UInt64>
  IAsyncOperationWithProgress_2__IInputStream__UInt64_Base = interface(IInspectable)
  ['{455AA601-F13E-5DEE-B9CB-16B531996327}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__IInputStream__UInt64); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__IInputStream__UInt64; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64; safecall;
    function GetResults: IInputStream; safecall;
    property Progress: AsyncOperationProgressHandler_2__IInputStream__UInt64 read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64 read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IInputStream,UInt64>
  // External 
  IAsyncOperationWithProgress_2__IInputStream__UInt64 = interface(IAsyncOperationWithProgress_2__IInputStream__UInt64_Base)
  ['{455AA601-F13E-5DEE-B9CB-16B531996327}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IInputStream,UInt64>
  AsyncOperationProgressHandler_2__IInputStream__UInt64_Delegate_Base = interface(IUnknown)
  ['{F9B2E7F6-762F-50DB-95DD-7F6C6EC47090}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IInputStream__UInt64; progressInfo: UInt64); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IInputStream,UInt64>
  // External 
  AsyncOperationProgressHandler_2__IInputStream__UInt64 = interface(AsyncOperationProgressHandler_2__IInputStream__UInt64_Delegate_Base)
  ['{F9B2E7F6-762F-50DB-95DD-7F6C6EC47090}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IInputStream,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64_Delegate_Base = interface(IUnknown)
  ['{8DB69706-3DD1-5A28-986A-93BE0776D9C3}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IInputStream__UInt64; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IInputStream,UInt64>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64 = interface(AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64_Delegate_Base)
  ['{8DB69706-3DD1-5A28-986A-93BE0776D9C3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<String,UInt64>
  IAsyncOperationWithProgress_2__HSTRING__UInt64_Base = interface(IInspectable)
  ['{C8BBCB29-6B64-5CE2-A831-038F6E02199E}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__HSTRING__UInt64); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__HSTRING__UInt64; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64; safecall;
    function GetResults: HSTRING; safecall;
    property Progress: AsyncOperationProgressHandler_2__HSTRING__UInt64 read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64 read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<String,UInt64>
  // External 
  IAsyncOperationWithProgress_2__HSTRING__UInt64 = interface(IAsyncOperationWithProgress_2__HSTRING__UInt64_Base)
  ['{C8BBCB29-6B64-5CE2-A831-038F6E02199E}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<String,UInt64>
  AsyncOperationProgressHandler_2__HSTRING__UInt64_Delegate_Base = interface(IUnknown)
  ['{14DA7DE7-40DF-5D4C-823F-CF310625AD39}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__HSTRING__UInt64; progressInfo: UInt64); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<String,UInt64>
  // External 
  AsyncOperationProgressHandler_2__HSTRING__UInt64 = interface(AsyncOperationProgressHandler_2__HSTRING__UInt64_Delegate_Base)
  ['{14DA7DE7-40DF-5D4C-823F-CF310625AD39}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<String,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64_Delegate_Base = interface(IUnknown)
  ['{BD75EEBE-E7B5-5AF6-8415-A4B9C9045202}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__HSTRING__UInt64; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<String,UInt64>
  // External 
  AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64 = interface(AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64_Delegate_Base)
  ['{BD75EEBE-E7B5-5AF6-8415-A4B9C9045202}']
  end;

  // Windows.Foundation.Collections.IMap`2<String,String>
  // External 
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

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>
  // External 
  IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = interface(IInspectable)
  ['{BCDE03AD-EA71-5077-A961-1C0ECFF57202}']
    function get_Key: HSTRING; safecall;
    function get_Value: IVectorView_1__HSTRING; safecall;
    property Key: HSTRING read get_Key;
    property Value: IVectorView_1__HSTRING read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>>
  IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING_Base = interface(IInspectable)
  ['{643B6D83-457E-5A43-800F-B0449F91D96B}']
    function get_Current: IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IVectorView_1__HSTRING): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVectorView`1<String>>>
  // External 
  IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING = interface(IIterator_1__IKeyValuePair_2__HSTRING__IVectorView_1__HSTRING_Base)
  ['{643B6D83-457E-5A43-800F-B0449F91D96B}']
  end;

  // Windows.Foundation.IReference`1<Single>
  // External 
  IReference_1__Single = interface(IInspectable)
  ['{719CC2BA-3E76-5DEF-9F1A-38D85A145EA8}']
    function get_Value: Single; safecall;
    property Value: Single read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.IAudioStreamDescriptor>
  IIterable_1__Core_IAudioStreamDescriptor_Base = interface(IInspectable)
  ['{A3E2C972-A171-5B94-8389-E983EBC3F3B9}']
    function First: IIterator_1__Core_IAudioStreamDescriptor; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.IAudioStreamDescriptor>
  // External 
  IIterable_1__Core_IAudioStreamDescriptor = interface(IIterable_1__Core_IAudioStreamDescriptor_Base)
  ['{93E7C43E-0D19-5EEA-B110-02B24E6E007F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.IAudioStreamDescriptor>
  IIterator_1__Core_IAudioStreamDescriptor_Base = interface(IInspectable)
  ['{A61A11CD-B32E-518B-A6A7-5472CBE00E83}']
    function get_Current: Core_IAudioStreamDescriptor; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCore_IAudioStreamDescriptor): Cardinal; safecall;
    property Current: Core_IAudioStreamDescriptor read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.IAudioStreamDescriptor>
  // External 
  IIterator_1__Core_IAudioStreamDescriptor = interface(IIterator_1__Core_IAudioStreamDescriptor_Base)
  ['{676BE182-2B2B-5755-94CB-DD512AD111BE}']
  end;

  // DualAPI Interface
  // Windows.Media.Core.IAudioStreamDescriptor
  [WinRTClassNameAttribute(SWindows_Media_Core_AudioStreamDescriptor)]
  Core_IAudioStreamDescriptor = interface(IInspectable)
  ['{1E3692E4-4027-4847-A70B-DF1D9A2A7B04}']
    function get_EncodingProperties: IAudioEncodingProperties; safecall;
    property EncodingProperties: IAudioEncodingProperties read get_EncodingProperties;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Core.IAudioStreamDescriptor>
  IVector_1__Core_IAudioStreamDescriptor_Base = interface(IInspectable)
  ['{45AFC129-988C-5F1E-9C17-6E34B917CD1B}']
    function GetAt(index: Cardinal): Core_IAudioStreamDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Core_IAudioStreamDescriptor; safecall;
    function IndexOf(value: Core_IAudioStreamDescriptor; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Core_IAudioStreamDescriptor); safecall;
    procedure InsertAt(index: Cardinal; value: Core_IAudioStreamDescriptor); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Core_IAudioStreamDescriptor); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IAudioStreamDescriptor): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCore_IAudioStreamDescriptor); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Core.IAudioStreamDescriptor>
  // External 
  IVector_1__Core_IAudioStreamDescriptor = interface(IVector_1__Core_IAudioStreamDescriptor_Base)
  ['{4AA8CDE4-787E-5075-AF19-B5E29A5B4A75}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IAudioStreamDescriptor>
  // External 
  IVectorView_1__Core_IAudioStreamDescriptor = interface(IInspectable)
  ['{A97548F5-13ED-5B5D-86C2-D9853F89CBD3}']
    function GetAt(index: Cardinal): Core_IAudioStreamDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_IAudioStreamDescriptor; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IAudioStreamDescriptor): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.IVideoStreamDescriptor>
  IIterable_1__Core_IVideoStreamDescriptor_Base = interface(IInspectable)
  ['{3ACBF03C-0A79-5823-AAA9-D88BC3F8F594}']
    function First: IIterator_1__Core_IVideoStreamDescriptor; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.IVideoStreamDescriptor>
  // External 
  IIterable_1__Core_IVideoStreamDescriptor = interface(IIterable_1__Core_IVideoStreamDescriptor_Base)
  ['{279FFF16-8217-56BF-8117-B3BCA6705D0B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.IVideoStreamDescriptor>
  IIterator_1__Core_IVideoStreamDescriptor_Base = interface(IInspectable)
  ['{DA51AB3C-3C64-545C-A3F4-F9B055AAF7D9}']
    function get_Current: Core_IVideoStreamDescriptor; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCore_IVideoStreamDescriptor): Cardinal; safecall;
    property Current: Core_IVideoStreamDescriptor read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.IVideoStreamDescriptor>
  // External 
  IIterator_1__Core_IVideoStreamDescriptor = interface(IIterator_1__Core_IVideoStreamDescriptor_Base)
  ['{360EA333-DC3F-5682-986B-4C807136494A}']
  end;

  // DualAPI Interface
  // Windows.Media.Core.IVideoStreamDescriptor
  [WinRTClassNameAttribute(SWindows_Media_Core_VideoStreamDescriptor)]
  Core_IVideoStreamDescriptor = interface(IInspectable)
  ['{12EE0D55-9C2B-4440-8057-2C7A90F0CBEC}']
    function get_EncodingProperties: IVideoEncodingProperties; safecall;
    property EncodingProperties: IVideoEncodingProperties read get_EncodingProperties;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Core.IVideoStreamDescriptor>
  IVector_1__Core_IVideoStreamDescriptor_Base = interface(IInspectable)
  ['{1FB064B3-636C-5988-9C97-02A9B76150F6}']
    function GetAt(index: Cardinal): Core_IVideoStreamDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Core_IVideoStreamDescriptor; safecall;
    function IndexOf(value: Core_IVideoStreamDescriptor; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Core_IVideoStreamDescriptor); safecall;
    procedure InsertAt(index: Cardinal; value: Core_IVideoStreamDescriptor); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Core_IVideoStreamDescriptor); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IVideoStreamDescriptor): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCore_IVideoStreamDescriptor); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Core.IVideoStreamDescriptor>
  // External 
  IVector_1__Core_IVideoStreamDescriptor = interface(IVector_1__Core_IVideoStreamDescriptor_Base)
  ['{F4E5A134-CFFB-52DE-B2A9-5FA3AC96885E}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.IVideoStreamDescriptor>
  // External 
  IVectorView_1__Core_IVideoStreamDescriptor = interface(IInspectable)
  ['{169BE92A-80D0-5ABF-A578-524970C2FEF3}']
    function GetAt(index: Cardinal): Core_IVideoStreamDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_IVideoStreamDescriptor; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IVideoStreamDescriptor): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  // External 
  IIterable_1__Core_ITimedMetadataStreamDescriptor = interface(IInspectable)
  ['{EF661E8D-8E71-506E-BB69-71EC4DF1331B}']
    function First: IIterator_1__Core_ITimedMetadataStreamDescriptor; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  // External 
  IIterator_1__Core_ITimedMetadataStreamDescriptor = interface(IInspectable)
  ['{4A3B011F-E9E1-553F-BB4D-44940E3AC0BA}']
    function get_Current: Core_ITimedMetadataStreamDescriptor; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCore_ITimedMetadataStreamDescriptor): Cardinal; safecall;
    property Current: Core_ITimedMetadataStreamDescriptor read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Media.Core.ITimedMetadataStreamDescriptor
  // External 
  Core_ITimedMetadataStreamDescriptor = interface(IInspectable)
  ['{133336BF-296A-463E-9FF9-01CD25691408}']
    function get_EncodingProperties: ITimedMetadataEncodingProperties; safecall;
    function Copy: Core_ITimedMetadataStreamDescriptor; safecall;
    property EncodingProperties: ITimedMetadataEncodingProperties read get_EncodingProperties;
  end;

  // Windows.Media.MediaProperties.ITimedMetadataEncodingProperties
  // External 
  ITimedMetadataEncodingProperties = interface(IInspectable)
  ['{51CD30D3-D690-4CFA-97F4-4A398E9DB420}']
    procedure SetFormatUserData(valueSize: Cardinal; value: PByte); safecall;
    procedure GetFormatUserData(valueSize: Cardinal; value: PByte); safecall;
    function Copy: ITimedMetadataEncodingProperties; safecall;
  end;

  // Windows.Foundation.Collections.IVector`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  // External 
  IVector_1__Core_ITimedMetadataStreamDescriptor = interface(IInspectable)
  ['{5E399634-ACF7-549D-8C16-242E1988DC1C}']
    function GetAt(index: Cardinal): Core_ITimedMetadataStreamDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Core_ITimedMetadataStreamDescriptor; safecall;
    function IndexOf(value: Core_ITimedMetadataStreamDescriptor; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Core_ITimedMetadataStreamDescriptor); safecall;
    procedure InsertAt(index: Cardinal; value: Core_ITimedMetadataStreamDescriptor); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Core_ITimedMetadataStreamDescriptor); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_ITimedMetadataStreamDescriptor): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCore_ITimedMetadataStreamDescriptor); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Core.ITimedMetadataStreamDescriptor>
  // External 
  IVectorView_1__Core_ITimedMetadataStreamDescriptor = interface(IInspectable)
  ['{5727E975-BF2B-5175-8A15-BB2E99094563}']
    function GetAt(index: Cardinal): Core_ITimedMetadataStreamDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_ITimedMetadataStreamDescriptor; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_ITimedMetadataStreamDescriptor): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<UInt8>
  IIterator_1__Byte_Base = interface(IInspectable)
  ['{40556131-A2A1-5FAB-AAEE-5F35268CA26B}']
    function get_Current: Byte; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PByte): Cardinal; safecall;
    property Current: Byte read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<UInt8>
  // External 
  IIterator_1__Byte = interface(IIterator_1__Byte_Base)
  ['{40556131-A2A1-5FAB-AAEE-5F35268CA26B}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.Connectivity.IConnectionProfile>
  // External 
  IVectorView_1__IConnectionProfile = interface(IInspectable)
  ['{3C724EDA-2AC8-5E49-945F-E242F6EE16D4}']
    function GetAt(index: Cardinal): IConnectionProfile; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IConnectionProfile; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIConnectionProfile): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Object>
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable_Delegate_Base = interface(IUnknown)
  ['{E4DC9CFC-F462-5AFD-856D-04ACE229D00E}']
    procedure Invoke(sender: IMobileBroadbandAccountWatcher; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Object>
  // External 
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable = interface(TypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable_Delegate_Base)
  ['{86F13482-3275-5928-80CD-6A6B72B82CCD}']
  end;

  // DualAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_MobileBroadbandAccountWatcher)]
  IMobileBroadbandAccountWatcher = interface(IInspectable)
  ['{6BF3335E-23B5-449F-928D-5E0D3E04471D}']
    function add_AccountAdded(handler: TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs): EventRegistrationToken; safecall;
    procedure remove_AccountAdded(cookie: EventRegistrationToken); safecall;
    function add_AccountUpdated(handler: TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AccountUpdated(cookie: EventRegistrationToken); safecall;
    function add_AccountRemoved(handler: TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs): EventRegistrationToken; safecall;
    procedure remove_AccountRemoved(cookie: EventRegistrationToken); safecall;
    function add_EnumerationCompleted(handler: TypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(cookie: EventRegistrationToken); safecall;
    function add_Stopped(handler: TypedEventHandler_2__IMobileBroadbandAccountWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Stopped(cookie: EventRegistrationToken); safecall;
    function get_Status: MobileBroadbandAccountWatcherStatus; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    property Status: MobileBroadbandAccountWatcherStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Windows.Networking.NetworkOperators.IMobileBroadbandAccountEventArgs>
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs_Delegate_Base = interface(IUnknown)
  ['{423CC41E-FE8C-5A7D-9FEE-AAE04EF85700}']
    procedure Invoke(sender: IMobileBroadbandAccountWatcher; args: IMobileBroadbandAccountEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Windows.Networking.NetworkOperators.IMobileBroadbandAccountEventArgs>
  // External 
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs = interface(TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountEventArgs_Delegate_Base)
  ['{5475B308-13DA-55FA-A4E6-32B15DCD409C}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandAccountEventArgs
  // External 
  IMobileBroadbandAccountEventArgs = interface(IInspectable)
  ['{3853C880-77DE-4C04-BEAD-A123B08C9F59}']
    function get_NetworkAccountId: HSTRING; safecall;
    property NetworkAccountId: HSTRING read get_NetworkAccountId;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Windows.Networking.NetworkOperators.IMobileBroadbandAccountUpdatedEventArgs>
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{0E865096-1FFA-5792-8D4F-8623E3C77F56}']
    procedure Invoke(sender: IMobileBroadbandAccountWatcher; args: IMobileBroadbandAccountUpdatedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandAccountWatcher,Windows.Networking.NetworkOperators.IMobileBroadbandAccountUpdatedEventArgs>
  // External 
  TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs = interface(TypedEventHandler_2__IMobileBroadbandAccountWatcher__IMobileBroadbandAccountUpdatedEventArgs_Delegate_Base)
  ['{732723B5-733F-57AE-BAB8-65EFC90209EC}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandAccountUpdatedEventArgs
  // External 
  IMobileBroadbandAccountUpdatedEventArgs = interface(IInspectable)
  ['{7BC31D88-A6BD-49E1-80AB-6B91354A57D4}']
    function get_NetworkAccountId: HSTRING; safecall;
    function get_HasDeviceInformationChanged: Boolean; safecall;
    function get_HasNetworkChanged: Boolean; safecall;
    property HasDeviceInformationChanged: Boolean read get_HasDeviceInformationChanged;
    property HasNetworkChanged: Boolean read get_HasNetworkChanged;
    property NetworkAccountId: HSTRING read get_NetworkAccountId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<UInt32>
  IIterable_1__Cardinal_Base = interface(IInspectable)
  ['{421D4B91-B13B-5F37-AE54-B5249BD80539}']
    function First: IIterator_1__Cardinal; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<UInt32>
  // External 
  IIterable_1__Cardinal = interface(IIterable_1__Cardinal_Base)
  ['{421D4B91-B13B-5F37-AE54-B5249BD80539}']
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandModem,Object>
  // External 
  TypedEventHandler_2__IMobileBroadbandModem__IInspectable = interface(IUnknown)
  ['{2084C5BA-53EE-502D-817A-F40F6DBBDB87}']
    procedure Invoke(sender: IMobileBroadbandModem; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandModem
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_MobileBroadbandModem)]
  IMobileBroadbandModem = interface(IInspectable)
  ['{D0356912-E9F9-4F67-A03D-43189A316BF1}']
    function get_CurrentAccount: IMobileBroadbandAccount; safecall;
    function get_DeviceInformation: IMobileBroadbandDeviceInformation; safecall;
    function get_MaxDeviceServiceCommandSizeInBytes: Cardinal; safecall;
    function get_MaxDeviceServiceDataSizeInBytes: Cardinal; safecall;
    function get_DeviceServices: IVectorView_1__IMobileBroadbandDeviceServiceInformation; safecall;
    function GetDeviceService(deviceServiceId: TGuid): IMobileBroadbandDeviceService; safecall;
    function get_IsResetSupported: Boolean; safecall;
    function ResetAsync: IAsyncAction; safecall;
    function GetCurrentConfigurationAsync: IAsyncOperation_1__IMobileBroadbandModemConfiguration; safecall;
    function get_CurrentNetwork: IMobileBroadbandNetwork; safecall;
    property CurrentAccount: IMobileBroadbandAccount read get_CurrentAccount;
    property CurrentNetwork: IMobileBroadbandNetwork read get_CurrentNetwork;
    property DeviceInformation: IMobileBroadbandDeviceInformation read get_DeviceInformation;
    property DeviceServices: IVectorView_1__IMobileBroadbandDeviceServiceInformation read get_DeviceServices;
    property IsResetSupported: Boolean read get_IsResetSupported;
    property MaxDeviceServiceCommandSizeInBytes: Cardinal read get_MaxDeviceServiceCommandSizeInBytes;
    property MaxDeviceServiceDataSizeInBytes: Cardinal read get_MaxDeviceServiceDataSizeInBytes;
  end;

  // DualAPI Interface
  // Windows.Networking.NetworkOperators.IMobileBroadbandAccount
  [WinRTClassNameAttribute(SWindows_Networking_NetworkOperators_MobileBroadbandAccount)]
  IMobileBroadbandAccount = interface(IInspectable)
  ['{36C24CCD-CEE2-43E0-A603-EE86A36D6570}']
    function get_NetworkAccountId: HSTRING; safecall;
    function get_ServiceProviderGuid: TGuid; safecall;
    function get_ServiceProviderName: HSTRING; safecall;
    function get_CurrentNetwork: IMobileBroadbandNetwork; safecall;
    function get_CurrentDeviceInformation: IMobileBroadbandDeviceInformation; safecall;
    property CurrentDeviceInformation: IMobileBroadbandDeviceInformation read get_CurrentDeviceInformation;
    property CurrentNetwork: IMobileBroadbandNetwork read get_CurrentNetwork;
    property NetworkAccountId: HSTRING read get_NetworkAccountId;
    property ServiceProviderGuid: TGuid read get_ServiceProviderGuid;
    property ServiceProviderName: HSTRING read get_ServiceProviderName;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandNetwork
  // External 
  IMobileBroadbandNetwork = interface(IInspectable)
  ['{CB63928C-0309-4CB6-A8C1-6A5A3C8E1FF6}']
    function get_NetworkAdapter: INetworkAdapter; safecall;
    function get_NetworkRegistrationState: NetworkRegistrationState; safecall;
    function get_RegistrationNetworkError: Cardinal; safecall;
    function get_PacketAttachNetworkError: Cardinal; safecall;
    function get_ActivationNetworkError: Cardinal; safecall;
    function get_AccessPointName: HSTRING; safecall;
    function get_RegisteredDataClass: DataClasses; safecall;
    function get_RegisteredProviderId: HSTRING; safecall;
    function get_RegisteredProviderName: HSTRING; safecall;
    procedure ShowConnectionUI; safecall;
    property AccessPointName: HSTRING read get_AccessPointName;
    property ActivationNetworkError: Cardinal read get_ActivationNetworkError;
    property NetworkAdapter: INetworkAdapter read get_NetworkAdapter;
    property NetworkRegistrationState_: NetworkRegistrationState read get_NetworkRegistrationState;
    property PacketAttachNetworkError: Cardinal read get_PacketAttachNetworkError;
    property RegisteredDataClass: DataClasses read get_RegisteredDataClass;
    property RegisteredProviderId: HSTRING read get_RegisteredProviderId;
    property RegisteredProviderName: HSTRING read get_RegisteredProviderName;
    property RegistrationNetworkError: Cardinal read get_RegistrationNetworkError;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceInformation
  // External 
  IMobileBroadbandDeviceInformation = interface(IInspectable)
  ['{E6D08168-E381-4C6E-9BE8-FE156969A446}']
    function get_NetworkDeviceStatus: NetworkDeviceStatus; safecall;
    function get_Manufacturer: HSTRING; safecall;
    function get_Model: HSTRING; safecall;
    function get_FirmwareInformation: HSTRING; safecall;
    function get_CellularClass: CellularClass; safecall;
    function get_DataClasses: DataClasses; safecall;
    function get_CustomDataClass: HSTRING; safecall;
    function get_MobileEquipmentId: HSTRING; safecall;
    function get_TelephoneNumbers: IVectorView_1__HSTRING; safecall;
    function get_SubscriberId: HSTRING; safecall;
    function get_SimIccId: HSTRING; safecall;
    function get_DeviceType: MobileBroadbandDeviceType; safecall;
    function get_DeviceId: HSTRING; safecall;
    function get_CurrentRadioState: MobileBroadbandRadioState; safecall;
    property CellularClass_: CellularClass read get_CellularClass;
    property CurrentRadioState: MobileBroadbandRadioState read get_CurrentRadioState;
    property CustomDataClass: HSTRING read get_CustomDataClass;
    property DataClasses_: DataClasses read get_DataClasses;
    property DeviceId: HSTRING read get_DeviceId;
    property DeviceType: MobileBroadbandDeviceType read get_DeviceType;
    property FirmwareInformation: HSTRING read get_FirmwareInformation;
    property Manufacturer: HSTRING read get_Manufacturer;
    property MobileEquipmentId: HSTRING read get_MobileEquipmentId;
    property Model: HSTRING read get_Model;
    property NetworkDeviceStatus_: NetworkDeviceStatus read get_NetworkDeviceStatus;
    property SimIccId: HSTRING read get_SimIccId;
    property SubscriberId: HSTRING read get_SubscriberId;
    property TelephoneNumbers: IVectorView_1__HSTRING read get_TelephoneNumbers;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation>
  // External 
  IVectorView_1__IMobileBroadbandDeviceServiceInformation = interface(IInspectable)
  ['{52C008C3-0273-53EC-80FE-EAF6B34CE03F}']
    function GetAt(index: Cardinal): IMobileBroadbandDeviceServiceInformation; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IMobileBroadbandDeviceServiceInformation; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIMobileBroadbandDeviceServiceInformation): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceInformation
  // External 
  IMobileBroadbandDeviceServiceInformation = interface(IInspectable)
  ['{53D69B5B-C4ED-45F0-803A-D9417A6D9846}']
    function get_DeviceServiceId: TGuid; safecall;
    function get_IsDataReadSupported: Boolean; safecall;
    function get_IsDataWriteSupported: Boolean; safecall;
    property DeviceServiceId: TGuid read get_DeviceServiceId;
    property IsDataReadSupported: Boolean read get_IsDataReadSupported;
    property IsDataWriteSupported: Boolean read get_IsDataWriteSupported;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceService
  // External 
  IMobileBroadbandDeviceService = interface(IInspectable)
  ['{22BE1A52-BD80-40AC-8E1F-2E07836A3DBD}']
    function get_DeviceServiceId: TGuid; safecall;
    function get_SupportedCommands: IVectorView_1__Cardinal; safecall;
    function OpenDataSession: IMobileBroadbandDeviceServiceDataSession; safecall;
    function OpenCommandSession: IMobileBroadbandDeviceServiceCommandSession; safecall;
    property DeviceServiceId: TGuid read get_DeviceServiceId;
    property SupportedCommands: IVectorView_1__Cardinal read get_SupportedCommands;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataSession
  // External 
  IMobileBroadbandDeviceServiceDataSession = interface(IInspectable)
  ['{DAD62333-8BCF-4289-8A37-045C2169486A}']
    function WriteDataAsync(value: IBuffer): IAsyncAction; safecall;
    procedure CloseSession; safecall;
    function add_DataReceived(eventHandler: TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_DataReceived(eventCookie: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataSession,Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataReceivedEventArgs>
  TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{31F89CA6-DD7F-5325-9020-627D8B47EA02}']
    procedure Invoke(sender: IMobileBroadbandDeviceServiceDataSession; args: IMobileBroadbandDeviceServiceDataReceivedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataSession,Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataReceivedEventArgs>
  // External 
  TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs = interface(TypedEventHandler_2__IMobileBroadbandDeviceServiceDataSession__IMobileBroadbandDeviceServiceDataReceivedEventArgs_Delegate_Base)
  ['{1A79C740-7A77-57DA-BBE8-9C068B8E0853}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceDataReceivedEventArgs
  // External 
  IMobileBroadbandDeviceServiceDataReceivedEventArgs = interface(IInspectable)
  ['{B6AA13DE-1380-40E3-8618-73CBCA48138C}']
    function get_ReceivedData: IBuffer; safecall;
    property ReceivedData: IBuffer read get_ReceivedData;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandSession
  // External 
  IMobileBroadbandDeviceServiceCommandSession = interface(IInspectable)
  ['{FC098A45-913B-4914-B6C3-AE6304593E75}']
    function SendQueryCommandAsync(commandId: Cardinal; data: IBuffer): IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult; safecall;
    function SendSetCommandAsync(commandId: Cardinal; data: IBuffer): IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult; safecall;
    procedure CloseSession; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult>
  IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult_Base = interface(IInspectable)
  ['{2C673AA8-6A35-50FD-9422-3615A1C28CCB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult; safecall;
    function GetResults: IMobileBroadbandDeviceServiceCommandResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult>
  // External 
  IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult = interface(IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult_Base)
  ['{0943E466-3D80-5767-827C-BEADC11B3BC9}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult_Delegate_Base = interface(IUnknown)
  ['{21F0CE4F-8F33-5E71-A457-DDA553B0D6BB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMobileBroadbandDeviceServiceCommandResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult>
  // External 
  AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult = interface(AsyncOperationCompletedHandler_1__IMobileBroadbandDeviceServiceCommandResult_Delegate_Base)
  ['{D7D3031C-A595-52C3-9179-7C74B197645D}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandDeviceServiceCommandResult
  // External 
  IMobileBroadbandDeviceServiceCommandResult = interface(IInspectable)
  ['{B0F46ABB-94D6-44B9-A538-F0810B645389}']
    function get_StatusCode: Cardinal; safecall;
    function get_ResponseData: IBuffer; safecall;
    property ResponseData: IBuffer read get_ResponseData;
    property StatusCode: Cardinal read get_StatusCode;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration>
  IAsyncOperation_1__IMobileBroadbandModemConfiguration_Base = interface(IInspectable)
  ['{CDBE0003-DAAA-5C89-92E6-A47FFC2418A2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration; safecall;
    function GetResults: IMobileBroadbandModemConfiguration; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration>
  // External 
  IAsyncOperation_1__IMobileBroadbandModemConfiguration = interface(IAsyncOperation_1__IMobileBroadbandModemConfiguration_Base)
  ['{A1DA01E4-C772-5CCB-9C5E-F67D9EDAA903}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration>
  AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration_Delegate_Base = interface(IUnknown)
  ['{C11E0649-8237-5C93-BBDB-2EDA5216FD3F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMobileBroadbandModemConfiguration; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration>
  // External 
  AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration = interface(AsyncOperationCompletedHandler_1__IMobileBroadbandModemConfiguration_Delegate_Base)
  ['{C6FCAF27-08E3-52FF-B470-BFF281F7EB51}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandModemConfiguration
  // External 
  IMobileBroadbandModemConfiguration = interface(IInspectable)
  ['{FCE035A3-D6CD-4320-B982-BE9D3EC7890F}']
    function get_Uicc: IMobileBroadbandUicc; safecall;
    function get_HomeProviderId: HSTRING; safecall;
    function get_HomeProviderName: HSTRING; safecall;
    property HomeProviderId: HSTRING read get_HomeProviderId;
    property HomeProviderName: HSTRING read get_HomeProviderName;
    property Uicc: IMobileBroadbandUicc read get_Uicc;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandUicc
  // External 
  IMobileBroadbandUicc = interface(IInspectable)
  ['{E634F691-525A-4CE2-8FCE-AA4162579154}']
    function get_SimIccId: HSTRING; safecall;
    function GetUiccAppsAsync: IAsyncOperation_1__IMobileBroadbandUiccAppsResult; safecall;
    property SimIccId: HSTRING read get_SimIccId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult>
  IAsyncOperation_1__IMobileBroadbandUiccAppsResult_Base = interface(IInspectable)
  ['{CF1CE97F-1A81-5CE6-8AD5-55FF8B0E8D1B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult; safecall;
    function GetResults: IMobileBroadbandUiccAppsResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult>
  // External 
  IAsyncOperation_1__IMobileBroadbandUiccAppsResult = interface(IAsyncOperation_1__IMobileBroadbandUiccAppsResult_Base)
  ['{FC71A253-3C12-51B6-93E0-A6E61C50A26E}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult_Delegate_Base = interface(IUnknown)
  ['{A12BED56-C672-595E-A67B-49ABC285ADDC}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMobileBroadbandUiccAppsResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult>
  // External 
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult = interface(AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppsResult_Delegate_Base)
  ['{A9553FA4-EEEC-5CF5-8791-3743312C16E1}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppsResult
  // External 
  IMobileBroadbandUiccAppsResult = interface(IInspectable)
  ['{744930EB-8157-4A41-8494-6BF54C9B1D2B}']
    function get_Status: MobileBroadbandUiccAppOperationStatus; safecall;
    function get_UiccApps: IVectorView_1__IMobileBroadbandUiccApp; safecall;
    property Status: MobileBroadbandUiccAppOperationStatus read get_Status;
    property UiccApps: IVectorView_1__IMobileBroadbandUiccApp read get_UiccApps;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp>
  // External 
  IVectorView_1__IMobileBroadbandUiccApp = interface(IInspectable)
  ['{61E0715E-E1F9-522E-8BD9-B1D946F964AE}']
    function GetAt(index: Cardinal): IMobileBroadbandUiccApp; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IMobileBroadbandUiccApp; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIMobileBroadbandUiccApp): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandUiccApp
  // External 
  IMobileBroadbandUiccApp = interface(IInspectable)
  ['{4D170556-98A1-43DD-B2EC-50C90CF248DF}']
    function get_Id: IBuffer; safecall;
    function get_Kind: UiccAppKind; safecall;
    function GetRecordDetailsAsync(uiccFilePath: IIterable_1__Cardinal): IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult; safecall;
    function ReadRecordAsync(uiccFilePath: IIterable_1__Cardinal; recordIndex: Integer): IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult; safecall;
    property Id: IBuffer read get_Id;
    property Kind: UiccAppKind read get_Kind;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult>
  IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult_Base = interface(IInspectable)
  ['{0774F4A6-BDBE-59FF-AA1C-A62E3C6F9D37}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult; safecall;
    function GetResults: IMobileBroadbandUiccAppRecordDetailsResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult>
  // External 
  IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult = interface(IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult_Base)
  ['{A82D52D6-E249-502B-B158-1C5B47B33840}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult_Delegate_Base = interface(IUnknown)
  ['{D0B53858-0E54-5791-82ED-3313DC75DA45}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMobileBroadbandUiccAppRecordDetailsResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult>
  // External 
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult = interface(AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppRecordDetailsResult_Delegate_Base)
  ['{27D58B7A-136A-5311-9339-3EFA8B4C4673}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppRecordDetailsResult
  // External 
  IMobileBroadbandUiccAppRecordDetailsResult = interface(IInspectable)
  ['{D919682F-BE14-4934-981D-2F57B9ED83E6}']
    function get_Status: MobileBroadbandUiccAppOperationStatus; safecall;
    function get_Kind: UiccAppRecordKind; safecall;
    function get_RecordCount: Integer; safecall;
    function get_RecordSize: Integer; safecall;
    function get_ReadAccessCondition: UiccAccessCondition; safecall;
    function get_WriteAccessCondition: UiccAccessCondition; safecall;
    property Kind: UiccAppRecordKind read get_Kind;
    property ReadAccessCondition: UiccAccessCondition read get_ReadAccessCondition;
    property RecordCount: Integer read get_RecordCount;
    property RecordSize: Integer read get_RecordSize;
    property Status: MobileBroadbandUiccAppOperationStatus read get_Status;
    property WriteAccessCondition: UiccAccessCondition read get_WriteAccessCondition;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult>
  IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult_Base = interface(IInspectable)
  ['{27FC8483-30D8-5BE3-BC1E-8CCA0B241DF3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult; safecall;
    function GetResults: IMobileBroadbandUiccAppReadRecordResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult>
  // External 
  IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult = interface(IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult_Base)
  ['{04D99A02-299A-56ED-B6EA-18C5DC2DE9EC}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult>
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult_Delegate_Base = interface(IUnknown)
  ['{B81892B3-4CA9-5EC4-8971-2FBC19B56CA9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMobileBroadbandUiccAppReadRecordResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult>
  // External 
  AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult = interface(AsyncOperationCompletedHandler_1__IMobileBroadbandUiccAppReadRecordResult_Delegate_Base)
  ['{8A6A0DF5-24F6-50DA-87A6-9460D7A5603B}']
  end;

  // Windows.Networking.NetworkOperators.IMobileBroadbandUiccAppReadRecordResult
  // External 
  IMobileBroadbandUiccAppReadRecordResult = interface(IInspectable)
  ['{64C95285-358E-47C5-8249-695F383B2BDB}']
    function get_Status: MobileBroadbandUiccAppOperationStatus; safecall;
    function get_Data: IBuffer; safecall;
    property Data: IBuffer read get_Data;
    property Status: MobileBroadbandUiccAppOperationStatus read get_Status;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Networking.IHostName>
  // External 
  IVectorView_1__IHostName = interface(IInspectable)
  ['{FB26F077-BEFF-523E-81C2-5A710F18359D}']
    function GetAt(index: Cardinal): IHostName; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IHostName; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIHostName): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Data.Xml.Dom.IXmlDocument
  [WinRTClassNameAttribute(SWindows_Data_Xml_Dom_XmlDocument)]
  Xml_Dom_IXmlDocument = interface(IInspectable)
  ['{F7F3A506-1E87-42D6-BCFB-B8C809FA5494}']
    function get_Doctype: Xml_Dom_IXmlDocumentType; safecall;
    function get_Implementation: Xml_Dom_IXmlDomImplementation; safecall;
    function get_DocumentElement: Xml_Dom_IXmlElement; safecall;
    function CreateElement(tagName: HSTRING): Xml_Dom_IXmlElement; safecall;
    function CreateDocumentFragment: Xml_Dom_IXmlDocumentFragment; safecall;
    function CreateTextNode(data: HSTRING): Xml_Dom_IXmlText; safecall;
    function CreateComment(data: HSTRING): Xml_Dom_IXmlComment; safecall;
    function CreateProcessingInstruction(target: HSTRING; data: HSTRING): Xml_Dom_IXmlProcessingInstruction; safecall;
    function CreateAttribute(name: HSTRING): Xml_Dom_IXmlAttribute; safecall;
    function CreateEntityReference(name: HSTRING): Xml_Dom_IXmlEntityReference; safecall;
    function GetElementsByTagName(tagName: HSTRING): Xml_Dom_IXmlNodeList; safecall;
    function CreateCDataSection(data: HSTRING): Xml_Dom_IXmlCDataSection; safecall;
    function get_DocumentUri: HSTRING; safecall;
    function CreateAttributeNS(namespaceUri: IInspectable; qualifiedName: HSTRING): Xml_Dom_IXmlAttribute; safecall;
    function CreateElementNS(namespaceUri: IInspectable; qualifiedName: HSTRING): Xml_Dom_IXmlElement; safecall;
    function GetElementById(elementId: HSTRING): Xml_Dom_IXmlElement; safecall;
    function ImportNode(node: Xml_Dom_IXmlNode; deep: Boolean): Xml_Dom_IXmlNode; safecall;
    property Doctype: Xml_Dom_IXmlDocumentType read get_Doctype;
    property DocumentElement: Xml_Dom_IXmlElement read get_DocumentElement;
    property DocumentUri: HSTRING read get_DocumentUri;
    property &Implementation: Xml_Dom_IXmlDomImplementation read get_Implementation;
  end;

  // Windows.Data.Xml.Dom.IXmlDocumentType
  // External 
  Xml_Dom_IXmlDocumentType = interface(IInspectable)
  ['{F7342425-9781-4964-8E94-9B1C6DFC9BC7}']
    function get_Name: HSTRING; safecall;
    function get_Entities: Xml_Dom_IXmlNamedNodeMap; safecall;
    function get_Notations: Xml_Dom_IXmlNamedNodeMap; safecall;
    property Entities: Xml_Dom_IXmlNamedNodeMap read get_Entities;
    property Name: HSTRING read get_Name;
    property Notations: Xml_Dom_IXmlNamedNodeMap read get_Notations;
  end;

  // Windows.Data.Xml.Dom.IXmlNamedNodeMap
  // External 
  Xml_Dom_IXmlNamedNodeMap = interface(IInspectable)
  ['{B3A69EB0-AAB0-4B82-A6FA-B1453F7C021B}']
    function get_Length: Cardinal; safecall;
    function Item(index: Cardinal): Xml_Dom_IXmlNode; safecall;
    function GetNamedItem(name: HSTRING): Xml_Dom_IXmlNode; safecall;
    function SetNamedItem(node: Xml_Dom_IXmlNode): Xml_Dom_IXmlNode; safecall;
    function RemoveNamedItem(name: HSTRING): Xml_Dom_IXmlNode; safecall;
    function GetNamedItemNS(namespaceUri: IInspectable; name: HSTRING): Xml_Dom_IXmlNode; safecall;
    function RemoveNamedItemNS(namespaceUri: IInspectable; name: HSTRING): Xml_Dom_IXmlNode; safecall;
    function SetNamedItemNS(node: Xml_Dom_IXmlNode): Xml_Dom_IXmlNode; safecall;
    property Length: Cardinal read get_Length;
  end;

  // DualAPI Interface
  // Windows.Data.Xml.Dom.IXmlNode
  // External 
  Xml_Dom_IXmlNode = interface(IInspectable)
  ['{1C741D59-2122-47D5-A856-83F3D4214875}']
    function get_NodeValue: IInspectable; safecall;
    procedure put_NodeValue(value: IInspectable); safecall;
    function get_NodeType: Xml_Dom_NodeType; safecall;
    function get_NodeName: HSTRING; safecall;
    function get_ParentNode: Xml_Dom_IXmlNode; safecall;
    function get_ChildNodes: Xml_Dom_IXmlNodeList; safecall;
    function get_FirstChild: Xml_Dom_IXmlNode; safecall;
    function get_LastChild: Xml_Dom_IXmlNode; safecall;
    function get_PreviousSibling: Xml_Dom_IXmlNode; safecall;
    function get_NextSibling: Xml_Dom_IXmlNode; safecall;
    function get_Attributes: Xml_Dom_IXmlNamedNodeMap; safecall;
    function HasChildNodes: Boolean; safecall;
    function get_OwnerDocument: Xml_Dom_IXmlDocument; safecall;
    function InsertBefore(newChild: Xml_Dom_IXmlNode; referenceChild: Xml_Dom_IXmlNode): Xml_Dom_IXmlNode; safecall;
    function ReplaceChild(newChild: Xml_Dom_IXmlNode; referenceChild: Xml_Dom_IXmlNode): Xml_Dom_IXmlNode; safecall;
    function RemoveChild(childNode: Xml_Dom_IXmlNode): Xml_Dom_IXmlNode; safecall;
    function AppendChild(newChild: Xml_Dom_IXmlNode): Xml_Dom_IXmlNode; safecall;
    function CloneNode(deep: Boolean): Xml_Dom_IXmlNode; safecall;
    function get_NamespaceUri: IInspectable; safecall;
    function get_LocalName: IInspectable; safecall;
    function get_Prefix: IInspectable; safecall;
    procedure Normalize; safecall;
    procedure put_Prefix(value: IInspectable); safecall;
    property Attributes: Xml_Dom_IXmlNamedNodeMap read get_Attributes;
    property ChildNodes: Xml_Dom_IXmlNodeList read get_ChildNodes;
    property FirstChild: Xml_Dom_IXmlNode read get_FirstChild;
    property LastChild: Xml_Dom_IXmlNode read get_LastChild;
    property LocalName: IInspectable read get_LocalName;
    property NamespaceUri: IInspectable read get_NamespaceUri;
    property NextSibling: Xml_Dom_IXmlNode read get_NextSibling;
    property NodeName: HSTRING read get_NodeName;
    property NodeType: Xml_Dom_NodeType read get_NodeType;
    property NodeValue: IInspectable read get_NodeValue write put_NodeValue;
    property OwnerDocument: Xml_Dom_IXmlDocument read get_OwnerDocument;
    property ParentNode: Xml_Dom_IXmlNode read get_ParentNode;
    property Prefix: IInspectable read get_Prefix write put_Prefix;
    property PreviousSibling: Xml_Dom_IXmlNode read get_PreviousSibling;
  end;

  // Windows.Data.Xml.Dom.IXmlNodeList
  // External 
  Xml_Dom_IXmlNodeList = interface(IInspectable)
  ['{8C60AD77-83A4-4EC1-9C54-7BA429E13DA6}']
    function get_Length: Cardinal; safecall;
    function Item(index: Cardinal): Xml_Dom_IXmlNode; safecall;
    property Length: Cardinal read get_Length;
  end;

  // Windows.Data.Xml.Dom.IXmlDomImplementation
  // External 
  Xml_Dom_IXmlDomImplementation = interface(IInspectable)
  ['{6DE58132-F11D-4FBB-8CC6-583CBA93112F}']
    function HasFeature(feature: HSTRING; version: IInspectable): Boolean; safecall;
  end;

  // Windows.Data.Xml.Dom.IXmlElement
  // External 
  Xml_Dom_IXmlElement = interface(IInspectable)
  ['{2DFB8A1F-6B10-4EF8-9F83-EFCCE8FAEC37}']
    function get_TagName: HSTRING; safecall;
    function GetAttribute(attributeName: HSTRING): HSTRING; safecall;
    procedure SetAttribute(attributeName: HSTRING; attributeValue: HSTRING); safecall;
    procedure RemoveAttribute(attributeName: HSTRING); safecall;
    function GetAttributeNode(attributeName: HSTRING): Xml_Dom_IXmlAttribute; safecall;
    function SetAttributeNode(newAttribute: Xml_Dom_IXmlAttribute): Xml_Dom_IXmlAttribute; safecall;
    function RemoveAttributeNode(attributeNode: Xml_Dom_IXmlAttribute): Xml_Dom_IXmlAttribute; safecall;
    function GetElementsByTagName(tagName: HSTRING): Xml_Dom_IXmlNodeList; safecall;
    procedure SetAttributeNS(namespaceUri: IInspectable; qualifiedName: HSTRING; value: HSTRING); safecall;
    function GetAttributeNS(namespaceUri: IInspectable; localName: HSTRING): HSTRING; safecall;
    procedure RemoveAttributeNS(namespaceUri: IInspectable; localName: HSTRING); safecall;
    function SetAttributeNodeNS(newAttribute: Xml_Dom_IXmlAttribute): Xml_Dom_IXmlAttribute; safecall;
    function GetAttributeNodeNS(namespaceUri: IInspectable; localName: HSTRING): Xml_Dom_IXmlAttribute; safecall;
    property TagName: HSTRING read get_TagName;
  end;

  // Windows.Data.Xml.Dom.IXmlAttribute
  // External 
  Xml_Dom_IXmlAttribute = interface(IInspectable)
  ['{AC144AA4-B4F1-4DB6-B206-8A22C308DB0A}']
    function get_Name: HSTRING; safecall;
    function get_Specified: Boolean; safecall;
    function get_Value: HSTRING; safecall;
    procedure put_Value(value: HSTRING); safecall;
    property Name: HSTRING read get_Name;
    property Specified: Boolean read get_Specified;
    property Value: HSTRING read get_Value write put_Value;
  end;

  // Windows.Data.Xml.Dom.IXmlDocumentFragment
  // External 
  Xml_Dom_IXmlDocumentFragment = interface(IInspectable)
  ['{E2EA6A96-0C21-44A5-8BC9-9E4A262708EC}']
  end;

  // Windows.Data.Xml.Dom.IXmlText
  // External 
  Xml_Dom_IXmlText = interface(IInspectable)
  ['{F931A4CB-308D-4760-A1D5-43B67450AC7E}']
    function SplitText(offset: Cardinal): Xml_Dom_IXmlText; safecall;
  end;

  // Windows.Data.Xml.Dom.IXmlComment
  // External 
  Xml_Dom_IXmlComment = interface(IInspectable)
  ['{BCA474D5-B61F-4611-9CAC-2E92E3476D47}']
  end;

  // Windows.Data.Xml.Dom.IXmlProcessingInstruction
  // External 
  Xml_Dom_IXmlProcessingInstruction = interface(IInspectable)
  ['{2707FD1E-1E92-4ECE-B6F4-26F069078DDC}']
    function get_Target: HSTRING; safecall;
    function get_Data: HSTRING; safecall;
    procedure put_Data(value: HSTRING); safecall;
    property Data: HSTRING read get_Data write put_Data;
    property Target: HSTRING read get_Target;
  end;

  // Windows.Data.Xml.Dom.IXmlEntityReference
  // External 
  Xml_Dom_IXmlEntityReference = interface(IInspectable)
  ['{2E2F47BC-C3D0-4CCF-BB86-0AB8C36A61CF}']
  end;

  // Windows.Data.Xml.Dom.IXmlCDataSection
  // External 
  Xml_Dom_IXmlCDataSection = interface(IInspectable)
  ['{4D04B46F-C8BD-45B4-8899-0400D7C2C60F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<UInt32>
  IVector_1__Cardinal_Base = interface(IInspectable)
  ['{534832ED-2A03-5604-890D-5A928CD427B9}']
    function GetAt(index: Cardinal): Cardinal; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Cardinal; safecall;
    function IndexOf(value: Cardinal; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Cardinal); safecall;
    procedure InsertAt(index: Cardinal; value: Cardinal); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Cardinal); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCardinal): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCardinal); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<UInt32>
  // External 
  IVector_1__Cardinal = interface(IVector_1__Cardinal_Base)
  ['{534832ED-2A03-5604-890D-5A928CD427B9}']
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IDisplayRegion,Object>
  // External 
  TypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable = interface(IUnknown)
  ['{8FF84463-41EF-50C0-B82E-A713DC7EDF18}']
    procedure Invoke(sender: WindowManagement_IDisplayRegion; args: IInspectable); safecall;
  end;

  // Windows.UI.WindowManagement.IDisplayRegion
  // External 
  WindowManagement_IDisplayRegion = interface(IInspectable)
  ['{DB50C3A2-4094-5F47-8CB1-EA01DDAFAA94}']
    function get_DisplayMonitorDeviceId: HSTRING; safecall;
    function get_IsVisible: Boolean; safecall;
    function get_WorkAreaOffset: TPointF; safecall;
    function get_WorkAreaSize: TSizeF; safecall;
    function get_WindowingEnvironment: WindowManagement_IWindowingEnvironment; safecall;
    function add_Changed(handler: TypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Changed(token: EventRegistrationToken); safecall;
    property DisplayMonitorDeviceId: HSTRING read get_DisplayMonitorDeviceId;
    property IsVisible: Boolean read get_IsVisible;
    property WindowingEnvironment: WindowManagement_IWindowingEnvironment read get_WindowingEnvironment;
    property WorkAreaOffset: TPointF read get_WorkAreaOffset;
    property WorkAreaSize: TSizeF read get_WorkAreaSize;
  end;

  // Windows.UI.WindowManagement.IWindowingEnvironment
  // External 
  WindowManagement_IWindowingEnvironment = interface(IInspectable)
  ['{264363C0-2A49-5417-B3AE-48A71C63A3BD}']
    function get_IsEnabled: Boolean; safecall;
    function get_Kind: WindowManagement_WindowingEnvironmentKind; safecall;
    function GetDisplayRegions: IVectorView_1__WindowManagement_IDisplayRegion; safecall;
    function add_Changed(handler: TypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Changed(token: EventRegistrationToken); safecall;
    property IsEnabled: Boolean read get_IsEnabled;
    property Kind: WindowManagement_WindowingEnvironmentKind read get_Kind;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.WindowManagement.IDisplayRegion>
  // External 
  IVectorView_1__WindowManagement_IDisplayRegion = interface(IInspectable)
  ['{77539DCC-E8FF-5A29-A5EA-97D2EF32E33B}']
    function GetAt(index: Cardinal): WindowManagement_IDisplayRegion; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: WindowManagement_IDisplayRegion; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PWindowManagement_IDisplayRegion): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IWindowingEnvironment,Windows.UI.WindowManagement.IWindowingEnvironmentChangedEventArgs>
  // External 
  TypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs = interface(IUnknown)
  ['{69BF4FD0-2B2D-54B7-898B-78C757079A4F}']
    procedure Invoke(sender: WindowManagement_IWindowingEnvironment; args: WindowManagement_IWindowingEnvironmentChangedEventArgs); safecall;
  end;

  // Windows.UI.WindowManagement.IWindowingEnvironmentChangedEventArgs
  // External 
  WindowManagement_IWindowingEnvironmentChangedEventArgs = interface(IInspectable)
  ['{4160CFC6-023D-5E9A-B431-350E67DC978A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Single>
  IIterator_1__Single_Base = interface(IInspectable)
  ['{42614E61-B0AA-5E72-9354-2771DB20B7A8}']
    function get_Current: Single; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSingle): Cardinal; safecall;
    property Current: Single read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Single>
  // External 
  IIterator_1__Single = interface(IIterator_1__Single_Base)
  ['{42614E61-B0AA-5E72-9354-2771DB20B7A8}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Single>
  // External 
  IVectorView_1__Single = interface(IInspectable)
  ['{7BCA64FD-150C-5D50-B56B-9F4F474C5930}']
    function GetAt(index: Cardinal): Single; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Single; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSingle): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.Graphics.Effects.IGraphicsEffect
  // External 
  Effects_IGraphicsEffect = interface(IInspectable)
  ['{CB51C0CE-8FE6-4636-B202-861FAA07D8F3}']
    function get_Name: HSTRING; safecall;
    procedure put_Name(name: HSTRING); safecall;
    property Name: HSTRING read get_Name write put_Name;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.Core.ICompositorController,Object>
  // External 
  TypedEventHandler_2__Core_ICompositorController__IInspectable = interface(IUnknown)
  ['{1559D785-01D0-5CE3-8850-7E2B47CC326E}']
    procedure Invoke(sender: Core_ICompositorController; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Composition.Core.ICompositorController
  [WinRTClassNameAttribute(SWindows_UI_Composition_Core_CompositorController)]
  Core_ICompositorController = interface(IInspectable)
  ['{2D75F35A-70A7-4395-BA2D-CEF0B18399F9}']
    function get_Compositor: ICompositor; safecall;
    procedure Commit; safecall;
    function EnsurePreviousCommitCompletedAsync: IAsyncAction; safecall;
    function add_CommitNeeded(handler: TypedEventHandler_2__Core_ICompositorController__IInspectable): EventRegistrationToken; safecall;
    procedure remove_CommitNeeded(token: EventRegistrationToken); safecall;
    property Compositor: ICompositor read get_Compositor;
  end;

  // DualAPI Interface
  // Windows.UI.Composition.ICompositor
  [WinRTClassNameAttribute(SWindows_UI_Composition_Compositor)]
  ICompositor = interface(IInspectable)
  ['{B403CA50-7F8C-4E83-985F-CC45060036D8}']
    function CreateColorKeyFrameAnimation: IColorKeyFrameAnimation; safecall;
    function CreateColorBrush: ICompositionColorBrush; overload; safecall;
    function CreateColorBrush(color: Color): ICompositionColorBrush; overload; safecall;
    function CreateContainerVisual: IContainerVisual; safecall;
    function CreateCubicBezierEasingFunction(controlPoint1: Numerics_Vector2; controlPoint2: Numerics_Vector2): ICubicBezierEasingFunction; safecall;
    function CreateEffectFactory(graphicsEffect: Effects_IGraphicsEffect): ICompositionEffectFactory; overload; safecall;
    function CreateEffectFactory(graphicsEffect: Effects_IGraphicsEffect; animatableProperties: IIterable_1__HSTRING): ICompositionEffectFactory; overload; safecall;
    function CreateExpressionAnimation: IExpressionAnimation; overload; safecall;
    function CreateExpressionAnimation(expression: HSTRING): IExpressionAnimation; overload; safecall;
    function CreateInsetClip: IInsetClip; overload; safecall;
    function CreateInsetClip(leftInset: Single; topInset: Single; rightInset: Single; bottomInset: Single): IInsetClip; overload; safecall;
    function CreateLinearEasingFunction: ILinearEasingFunction; safecall;
    function CreatePropertySet: ICompositionPropertySet; safecall;
    function CreateQuaternionKeyFrameAnimation: IQuaternionKeyFrameAnimation; safecall;
    function CreateScalarKeyFrameAnimation: IScalarKeyFrameAnimation; safecall;
    function CreateScopedBatch(batchType: CompositionBatchTypes): ICompositionScopedBatch; safecall;
    function CreateSpriteVisual: ISpriteVisual; safecall;
    function CreateSurfaceBrush: ICompositionSurfaceBrush; overload; safecall;
    function CreateSurfaceBrush(surface: ICompositionSurface): ICompositionSurfaceBrush; overload; safecall;
    function CreateTargetForCurrentView: ICompositionTarget; safecall;
    function CreateVector2KeyFrameAnimation: IVector2KeyFrameAnimation; safecall;
    function CreateVector3KeyFrameAnimation: IVector3KeyFrameAnimation; safecall;
    function CreateVector4KeyFrameAnimation: IVector4KeyFrameAnimation; safecall;
    function GetCommitBatch(batchType: CompositionBatchTypes): ICompositionCommitBatch; safecall;
  end;

  // Windows.UI.Composition.IColorKeyFrameAnimation
  // External 
  IColorKeyFrameAnimation = interface(IInspectable)
  ['{93ADB5E9-8E05-4593-84A3-DCA152781E56}']
    function get_InterpolationColorSpace: CompositionColorSpace; safecall;
    procedure put_InterpolationColorSpace(value: CompositionColorSpace); safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Color); overload; safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Color; easingFunction: ICompositionEasingFunction); overload; safecall;
    property InterpolationColorSpace: CompositionColorSpace read get_InterpolationColorSpace write put_InterpolationColorSpace;
  end;

  // Windows.UI.Composition.ICompositionEasingFunction
  // External 
  ICompositionEasingFunction = interface(IInspectable)
  ['{5145E356-BF79-4EA8-8CC2-6B5B472E6C9A}']
  end;

  // Windows.UI.Composition.ICompositionColorBrush
  // External 
  ICompositionColorBrush = interface(IInspectable)
  ['{2B264C5E-BF35-4831-8642-CF70C20FFF2F}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    property Color_: Color read get_Color write put_Color;
  end;

  // Windows.UI.Composition.IContainerVisual
  // External 
  IContainerVisual = interface(IInspectable)
  ['{02F6BC74-ED20-4773-AFE6-D49B4A93DB32}']
    function get_Children: IVisualCollection; safecall;
    property Children: IVisualCollection read get_Children;
  end;

  // Windows.UI.Composition.IVisualCollection
  // External 
  IVisualCollection = interface(IInspectable)
  ['{8B745505-FD3E-4A98-84A8-E949468C6BCB}']
    function get_Count: Integer; safecall;
    procedure InsertAbove(newChild: IVisual; sibling: IVisual); safecall;
    procedure InsertAtBottom(newChild: IVisual); safecall;
    procedure InsertAtTop(newChild: IVisual); safecall;
    procedure InsertBelow(newChild: IVisual; sibling: IVisual); safecall;
    procedure Remove(child: IVisual); safecall;
    procedure RemoveAll; safecall;
    property Count: Integer read get_Count;
  end;

  // Windows.UI.Composition.IVisual
  // External 
  IVisual = interface(IInspectable)
  ['{117E202D-A859-4C89-873B-C2AA566788E3}']
    function get_AnchorPoint: Numerics_Vector2; safecall;
    procedure put_AnchorPoint(value: Numerics_Vector2); safecall;
    function get_BackfaceVisibility: CompositionBackfaceVisibility; safecall;
    procedure put_BackfaceVisibility(value: CompositionBackfaceVisibility); safecall;
    function get_BorderMode: CompositionBorderMode; safecall;
    procedure put_BorderMode(value: CompositionBorderMode); safecall;
    function get_CenterPoint: Numerics_Vector3; safecall;
    procedure put_CenterPoint(value: Numerics_Vector3); safecall;
    function get_Clip: ICompositionClip; safecall;
    procedure put_Clip(value: ICompositionClip); safecall;
    function get_CompositeMode: CompositionCompositeMode; safecall;
    procedure put_CompositeMode(value: CompositionCompositeMode); safecall;
    function get_IsVisible: Boolean; safecall;
    procedure put_IsVisible(value: Boolean); safecall;
    function get_Offset: Numerics_Vector3; safecall;
    procedure put_Offset(value: Numerics_Vector3); safecall;
    function get_Opacity: Single; safecall;
    procedure put_Opacity(value: Single); safecall;
    function get_Orientation: Numerics_Quaternion; safecall;
    procedure put_Orientation(value: Numerics_Quaternion); safecall;
    function get_Parent: IContainerVisual; safecall;
    function get_RotationAngle: Single; safecall;
    procedure put_RotationAngle(value: Single); safecall;
    function get_RotationAngleInDegrees: Single; safecall;
    procedure put_RotationAngleInDegrees(value: Single); safecall;
    function get_RotationAxis: Numerics_Vector3; safecall;
    procedure put_RotationAxis(value: Numerics_Vector3); safecall;
    function get_Scale: Numerics_Vector3; safecall;
    procedure put_Scale(value: Numerics_Vector3); safecall;
    function get_Size: Numerics_Vector2; safecall;
    procedure put_Size(value: Numerics_Vector2); safecall;
    function get_TransformMatrix: Numerics_Matrix4x4; safecall;
    procedure put_TransformMatrix(value: Numerics_Matrix4x4); safecall;
    property AnchorPoint: Numerics_Vector2 read get_AnchorPoint write put_AnchorPoint;
    property BackfaceVisibility: CompositionBackfaceVisibility read get_BackfaceVisibility write put_BackfaceVisibility;
    property BorderMode: CompositionBorderMode read get_BorderMode write put_BorderMode;
    property CenterPoint: Numerics_Vector3 read get_CenterPoint write put_CenterPoint;
    property Clip: ICompositionClip read get_Clip write put_Clip;
    property CompositeMode: CompositionCompositeMode read get_CompositeMode write put_CompositeMode;
    property IsVisible: Boolean read get_IsVisible write put_IsVisible;
    property Offset: Numerics_Vector3 read get_Offset write put_Offset;
    property Opacity: Single read get_Opacity write put_Opacity;
    property Orientation: Numerics_Quaternion read get_Orientation write put_Orientation;
    property Parent: IContainerVisual read get_Parent;
    property RotationAngle: Single read get_RotationAngle write put_RotationAngle;
    property RotationAngleInDegrees: Single read get_RotationAngleInDegrees write put_RotationAngleInDegrees;
    property RotationAxis: Numerics_Vector3 read get_RotationAxis write put_RotationAxis;
    property Scale: Numerics_Vector3 read get_Scale write put_Scale;
    property Size: Numerics_Vector2 read get_Size write put_Size;
    property TransformMatrix: Numerics_Matrix4x4 read get_TransformMatrix write put_TransformMatrix;
  end;

  // Windows.UI.Composition.ICompositionClip
  // External 
  ICompositionClip = interface(IInspectable)
  ['{1CCD2A52-CFC7-4ACE-9983-146BB8EB6A3C}']
  end;

  // Windows.UI.Composition.ICubicBezierEasingFunction
  // External 
  ICubicBezierEasingFunction = interface(IInspectable)
  ['{32350666-C1E8-44F9-96B8-C98ACF0AE698}']
    function get_ControlPoint1: Numerics_Vector2; safecall;
    function get_ControlPoint2: Numerics_Vector2; safecall;
    property ControlPoint1: Numerics_Vector2 read get_ControlPoint1;
    property ControlPoint2: Numerics_Vector2 read get_ControlPoint2;
  end;

  // Windows.UI.Composition.ICompositionEffectFactory
  // External 
  ICompositionEffectFactory = interface(IInspectable)
  ['{BE5624AF-BA7E-4510-9850-41C0B4FF74DF}']
    function CreateBrush: ICompositionEffectBrush; safecall;
    function get_ExtendedError: HRESULT; safecall;
    function get_LoadStatus: CompositionEffectFactoryLoadStatus; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property LoadStatus: CompositionEffectFactoryLoadStatus read get_LoadStatus;
  end;

  // Windows.UI.Composition.ICompositionEffectBrush
  // External 
  ICompositionEffectBrush = interface(IInspectable)
  ['{BF7F795E-83CC-44BF-A447-3E3C071789EC}']
    function GetSourceParameter(name: HSTRING): ICompositionBrush; safecall;
    procedure SetSourceParameter(name: HSTRING; source: ICompositionBrush); safecall;
  end;

  // Windows.UI.Composition.ICompositionBrush
  // External 
  ICompositionBrush = interface(IInspectable)
  ['{AB0D7608-30C0-40E9-B568-B60A6BD1FB46}']
  end;

  // Windows.UI.Composition.IExpressionAnimation
  // External 
  IExpressionAnimation = interface(IInspectable)
  ['{6ACC5431-7D3D-4BF3-ABB6-F44BDC4888C1}']
    function get_Expression: HSTRING; safecall;
    procedure put_Expression(value: HSTRING); safecall;
    property Expression: HSTRING read get_Expression write put_Expression;
  end;

  // Windows.UI.Composition.IInsetClip
  // External 
  IInsetClip = interface(IInspectable)
  ['{1E73E647-84C7-477A-B474-5880E0442E15}']
    function get_BottomInset: Single; safecall;
    procedure put_BottomInset(value: Single); safecall;
    function get_LeftInset: Single; safecall;
    procedure put_LeftInset(value: Single); safecall;
    function get_RightInset: Single; safecall;
    procedure put_RightInset(value: Single); safecall;
    function get_TopInset: Single; safecall;
    procedure put_TopInset(value: Single); safecall;
    property BottomInset: Single read get_BottomInset write put_BottomInset;
    property LeftInset: Single read get_LeftInset write put_LeftInset;
    property RightInset: Single read get_RightInset write put_RightInset;
    property TopInset: Single read get_TopInset write put_TopInset;
  end;

  // Windows.UI.Composition.ILinearEasingFunction
  // External 
  ILinearEasingFunction = interface(IInspectable)
  ['{9400975A-C7A6-46B3-ACF7-1A268A0A117D}']
  end;

  // Windows.UI.Composition.ICompositionPropertySet
  // External 
  ICompositionPropertySet = interface(IInspectable)
  ['{C9D6D202-5F67-4453-9117-9EADD430D3C2}']
    procedure InsertColor(propertyName: HSTRING; value: Color); safecall;
    procedure InsertMatrix3x2(propertyName: HSTRING; value: Numerics_Matrix3x2); safecall;
    procedure InsertMatrix4x4(propertyName: HSTRING; value: Numerics_Matrix4x4); safecall;
    procedure InsertQuaternion(propertyName: HSTRING; value: Numerics_Quaternion); safecall;
    procedure InsertScalar(propertyName: HSTRING; value: Single); safecall;
    procedure InsertVector2(propertyName: HSTRING; value: Numerics_Vector2); safecall;
    procedure InsertVector3(propertyName: HSTRING; value: Numerics_Vector3); safecall;
    procedure InsertVector4(propertyName: HSTRING; value: Numerics_Vector4); safecall;
    function TryGetColor(propertyName: HSTRING; out value: Color): CompositionGetValueStatus; safecall;
    function TryGetMatrix3x2(propertyName: HSTRING; out value: Numerics_Matrix3x2): CompositionGetValueStatus; safecall;
    function TryGetMatrix4x4(propertyName: HSTRING; out value: Numerics_Matrix4x4): CompositionGetValueStatus; safecall;
    function TryGetQuaternion(propertyName: HSTRING; out value: Numerics_Quaternion): CompositionGetValueStatus; safecall;
    function TryGetScalar(propertyName: HSTRING; out value: Single): CompositionGetValueStatus; safecall;
    function TryGetVector2(propertyName: HSTRING; out value: Numerics_Vector2): CompositionGetValueStatus; safecall;
    function TryGetVector3(propertyName: HSTRING; out value: Numerics_Vector3): CompositionGetValueStatus; safecall;
    function TryGetVector4(propertyName: HSTRING; out value: Numerics_Vector4): CompositionGetValueStatus; safecall;
  end;

  // Windows.UI.Composition.IQuaternionKeyFrameAnimation
  // External 
  IQuaternionKeyFrameAnimation = interface(IInspectable)
  ['{404E5835-ECF6-4240-8520-671279CF36BC}']
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Quaternion); overload; safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Quaternion; easingFunction: ICompositionEasingFunction); overload; safecall;
  end;

  // Windows.UI.Composition.IScalarKeyFrameAnimation
  // External 
  IScalarKeyFrameAnimation = interface(IInspectable)
  ['{AE288FA9-252C-4B95-A725-BF85E38000A1}']
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Single); overload; safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Single; easingFunction: ICompositionEasingFunction); overload; safecall;
  end;

  // Windows.UI.Composition.ICompositionScopedBatch
  // External 
  ICompositionScopedBatch = interface(IInspectable)
  ['{0D00DAD0-FB07-46FD-8C72-6280D1A3D1DD}']
    function get_IsActive: Boolean; safecall;
    function get_IsEnded: Boolean; safecall;
    procedure &End; safecall;
    procedure Resume; safecall;
    procedure Suspend; safecall;
    function add_Completed(handler: TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Completed(token: EventRegistrationToken); safecall;
    property IsActive: Boolean read get_IsActive;
    property IsEnded: Boolean read get_IsEnded;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Composition.ICompositionBatchCompletedEventArgs>
  TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{9DF03456-3383-508B-9C75-EE840A7E1A39}']
    procedure Invoke(sender: IInspectable; args: ICompositionBatchCompletedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Composition.ICompositionBatchCompletedEventArgs>
  // External 
  TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs = interface(TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs_Delegate_Base)
  ['{7C8E0B74-2AC8-5AE5-98BE-5D8F40CF6B4E}']
  end;

  // Windows.UI.Composition.ICompositionBatchCompletedEventArgs
  // External 
  ICompositionBatchCompletedEventArgs = interface(IInspectable)
  ['{0D00DAD0-9464-450A-A562-2E2698B0A812}']
  end;

  // Windows.UI.Composition.ISpriteVisual
  // External 
  ISpriteVisual = interface(IInspectable)
  ['{08E05581-1AD1-4F97-9757-402D76E4233B}']
    function get_Brush: ICompositionBrush; safecall;
    procedure put_Brush(value: ICompositionBrush); safecall;
    property Brush: ICompositionBrush read get_Brush write put_Brush;
  end;

  // Windows.UI.Composition.ICompositionSurfaceBrush
  // External 
  ICompositionSurfaceBrush = interface(IInspectable)
  ['{AD016D79-1E4C-4C0D-9C29-83338C87C162}']
    function get_BitmapInterpolationMode: CompositionBitmapInterpolationMode; safecall;
    procedure put_BitmapInterpolationMode(value: CompositionBitmapInterpolationMode); safecall;
    function get_HorizontalAlignmentRatio: Single; safecall;
    procedure put_HorizontalAlignmentRatio(value: Single); safecall;
    function get_Stretch: CompositionStretch; safecall;
    procedure put_Stretch(value: CompositionStretch); safecall;
    function get_Surface: ICompositionSurface; safecall;
    procedure put_Surface(value: ICompositionSurface); safecall;
    function get_VerticalAlignmentRatio: Single; safecall;
    procedure put_VerticalAlignmentRatio(value: Single); safecall;
    property BitmapInterpolationMode: CompositionBitmapInterpolationMode read get_BitmapInterpolationMode write put_BitmapInterpolationMode;
    property HorizontalAlignmentRatio: Single read get_HorizontalAlignmentRatio write put_HorizontalAlignmentRatio;
    property Stretch: CompositionStretch read get_Stretch write put_Stretch;
    property Surface: ICompositionSurface read get_Surface write put_Surface;
    property VerticalAlignmentRatio: Single read get_VerticalAlignmentRatio write put_VerticalAlignmentRatio;
  end;

  // DualAPI Interface
  // Windows.UI.Composition.ICompositionSurface
  // External 
  ICompositionSurface = interface(IInspectable)
  ['{1527540D-42C7-47A6-A408-668F79A90DFB}']
  end;

  // Windows.UI.Composition.ICompositionTarget
  // External 
  ICompositionTarget = interface(IInspectable)
  ['{A1BEA8BA-D726-4663-8129-6B5E7927FFA6}']
    function get_Root: IVisual; safecall;
    procedure put_Root(value: IVisual); safecall;
    property Root: IVisual read get_Root write put_Root;
  end;

  // Windows.UI.Composition.IVector2KeyFrameAnimation
  // External 
  IVector2KeyFrameAnimation = interface(IInspectable)
  ['{DF414515-4E29-4F11-B55E-BF2A6EB36294}']
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Vector2); overload; safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Vector2; easingFunction: ICompositionEasingFunction); overload; safecall;
  end;

  // Windows.UI.Composition.IVector3KeyFrameAnimation
  // External 
  IVector3KeyFrameAnimation = interface(IInspectable)
  ['{C8039DAA-A281-43C2-A73D-B68E3C533C40}']
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Vector3); overload; safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Vector3; easingFunction: ICompositionEasingFunction); overload; safecall;
  end;

  // Windows.UI.Composition.IVector4KeyFrameAnimation
  // External 
  IVector4KeyFrameAnimation = interface(IInspectable)
  ['{2457945B-ADDD-4385-9606-B6A3D5E4E1B9}']
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Vector4); overload; safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Numerics_Vector4; easingFunction: ICompositionEasingFunction); overload; safecall;
  end;

  // Windows.UI.Composition.ICompositionCommitBatch
  // External 
  ICompositionCommitBatch = interface(IInspectable)
  ['{0D00DAD0-CA07-4400-8C8E-CB5DB08559CC}']
    function get_IsActive: Boolean; safecall;
    function get_IsEnded: Boolean; safecall;
    function add_Completed(handler: TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Completed(token: EventRegistrationToken); safecall;
    property IsActive: Boolean read get_IsActive;
    property IsEnded: Boolean read get_IsEnded;
  end;

  // DualAPI Interface
  // Windows.Graphics.IGeometrySource2D
  // External 
  IGeometrySource2D = interface(IInspectable)
  ['{CAFF7902-670C-4181-A624-DA977203B845}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Single>
  IVector_1__Single_Base = interface(IInspectable)
  ['{61CF693F-DB4C-579F-B905-5DD3D23CFD4D}']
    function GetAt(index: Cardinal): Single; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Single; safecall;
    function IndexOf(value: Single; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Single); safecall;
    procedure InsertAt(index: Cardinal; value: Single); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Single); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSingle): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PSingle); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Single>
  // External 
  IVector_1__Single = interface(IVector_1__Single_Base)
  ['{61CF693F-DB4C-579F-B905-5DD3D23CFD4D}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.ICompositionCapabilities,Object>
  TypedEventHandler_2__ICompositionCapabilities__IInspectable_Delegate_Base = interface(IUnknown)
  ['{6E8CC6F4-6CF5-5994-9447-726171236EA8}']
    procedure Invoke(sender: ICompositionCapabilities; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.ICompositionCapabilities,Object>
  // External 
  TypedEventHandler_2__ICompositionCapabilities__IInspectable = interface(TypedEventHandler_2__ICompositionCapabilities__IInspectable_Delegate_Base)
  ['{A97C10D7-8BE1-57F7-89A8-BC1D9B5D0C28}']
  end;

  // DualAPI Interface
  // Windows.UI.Composition.ICompositionCapabilities
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionCapabilities)]
  ICompositionCapabilities = interface(IInspectable)
  ['{8253353E-B517-48BC-B1E8-4B3561A2E181}']
    function AreEffectsSupported: Boolean; safecall;
    function AreEffectsFast: Boolean; safecall;
    function add_Changed(handler: TypedEventHandler_2__ICompositionCapabilities__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Changed(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkUnprocessedInput,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs_Delegate_Base = interface(IUnknown)
  ['{4A86BD78-5BCF-5EDE-8F65-A8C65235055C}']
    procedure Invoke(sender: IInkUnprocessedInput; args: IPointerEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkUnprocessedInput,Windows.UI.Core.IPointerEventArgs>
  // External 
  TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs = interface(TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs_Delegate_Base)
  ['{E5E83F48-D96A-5622-81AD-4BA59E3DCF06}']
  end;

  // Windows.UI.Input.Inking.IInkUnprocessedInput
  // External 
  IInkUnprocessedInput = interface(IInspectable)
  ['{DB4445E0-8398-4921-AC3B-AB978C5BA256}']
    function add_PointerEntered(handler: TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerEntered(cookie: EventRegistrationToken); safecall;
    function add_PointerHovered(handler: TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerHovered(cookie: EventRegistrationToken); safecall;
    function add_PointerExited(handler: TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerExited(cookie: EventRegistrationToken); safecall;
    function add_PointerPressed(handler: TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerPressed(cookie: EventRegistrationToken); safecall;
    function add_PointerMoved(handler: TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerMoved(cookie: EventRegistrationToken); safecall;
    function add_PointerReleased(handler: TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerReleased(cookie: EventRegistrationToken); safecall;
    function add_PointerLost(handler: TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerLost(cookie: EventRegistrationToken); safecall;
    function get_InkPresenter: IInkPresenter; safecall;
    property InkPresenter: IInkPresenter read get_InkPresenter;
  end;

  // Windows.UI.Input.Inking.IInkPresenter
  // External 
  IInkPresenter = interface(IInspectable)
  ['{A69B70E2-887B-458F-B173-4FE4438930A3}']
    function get_IsInputEnabled: Boolean; safecall;
    procedure put_IsInputEnabled(value: Boolean); safecall;
    function get_InputDeviceTypes: CoreInputDeviceTypes; safecall;
    procedure put_InputDeviceTypes(value: CoreInputDeviceTypes); safecall;
    function get_UnprocessedInput: IInkUnprocessedInput; safecall;
    function get_StrokeInput: IInkStrokeInput; safecall;
    function get_InputProcessingConfiguration: IInkInputProcessingConfiguration; safecall;
    function get_StrokeContainer: IInkStrokeContainer; safecall;
    procedure put_StrokeContainer(value: IInkStrokeContainer); safecall;
    function CopyDefaultDrawingAttributes: IInkDrawingAttributes; safecall;
    procedure UpdateDefaultDrawingAttributes(value: IInkDrawingAttributes); safecall;
    function ActivateCustomDrying: IInkSynchronizer; safecall;
    procedure SetPredefinedConfiguration(value: InkPresenterPredefinedConfiguration); safecall;
    function add_StrokesCollected(handler: TypedEventHandler_2__IInkPresenter__IInkStrokesCollectedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StrokesCollected(cookie: EventRegistrationToken); safecall;
    function add_StrokesErased(handler: TypedEventHandler_2__IInkPresenter__IInkStrokesErasedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StrokesErased(cookie: EventRegistrationToken); safecall;
    property InputDeviceTypes: CoreInputDeviceTypes read get_InputDeviceTypes write put_InputDeviceTypes;
    property InputProcessingConfiguration: IInkInputProcessingConfiguration read get_InputProcessingConfiguration;
    property IsInputEnabled: Boolean read get_IsInputEnabled write put_IsInputEnabled;
    property StrokeContainer: IInkStrokeContainer read get_StrokeContainer write put_StrokeContainer;
    property StrokeInput: IInkStrokeInput read get_StrokeInput;
    property UnprocessedInput: IInkUnprocessedInput read get_UnprocessedInput;
  end;

  // Windows.UI.Input.Inking.IInkStrokeInput
  // External 
  IInkStrokeInput = interface(IInspectable)
  ['{CF2FFE7B-5E10-43C6-A080-88F26E1DC67D}']
    function add_StrokeStarted(handler: TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_StrokeStarted(cookie: EventRegistrationToken); safecall;
    function add_StrokeContinued(handler: TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_StrokeContinued(cookie: EventRegistrationToken); safecall;
    function add_StrokeEnded(handler: TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_StrokeEnded(cookie: EventRegistrationToken); safecall;
    function add_StrokeCanceled(handler: TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_StrokeCanceled(cookie: EventRegistrationToken); safecall;
    function get_InkPresenter: IInkPresenter; safecall;
    property InkPresenter: IInkPresenter read get_InkPresenter;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkStrokeInput,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs_Delegate_Base = interface(IUnknown)
  ['{BF66B962-702D-5C07-A2D5-15F21583C43A}']
    procedure Invoke(sender: IInkStrokeInput; args: IPointerEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkStrokeInput,Windows.UI.Core.IPointerEventArgs>
  // External 
  TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs = interface(TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs_Delegate_Base)
  ['{8D3570C6-D204-57E3-9A3D-07B8A0D5E873}']
  end;

  // Windows.UI.Input.Inking.IInkInputProcessingConfiguration
  // External 
  IInkInputProcessingConfiguration = interface(IInspectable)
  ['{2778D85E-33CA-4B06-A6D3-AC3945116D37}']
    function get_Mode: InkInputProcessingMode; safecall;
    procedure put_Mode(value: InkInputProcessingMode); safecall;
    function get_RightDragAction: InkInputRightDragAction; safecall;
    procedure put_RightDragAction(value: InkInputRightDragAction); safecall;
    property Mode: InkInputProcessingMode read get_Mode write put_Mode;
    property RightDragAction: InkInputRightDragAction read get_RightDragAction write put_RightDragAction;
  end;

  // DualAPI Interface
  // Windows.UI.Input.Inking.IInkStrokeContainer
  [WinRTClassNameAttribute(SWindows_UI_Input_Inking_InkStrokeContainer)]
  IInkStrokeContainer = interface(IInspectable)
  ['{22ACCBC6-FAA9-4F14-B68C-F6CEE670AE16}']
    function get_BoundingRect: TRectF; safecall;
    procedure AddStroke(stroke: IInkStroke); safecall;
    function DeleteSelected: TRectF; safecall;
    function MoveSelected(translation: TPointF): TRectF; safecall;
    function SelectWithPolyLine(polyline: IIterable_1__Point): TRectF; safecall;
    function SelectWithLine(from: TPointF; &to: TPointF): TRectF; safecall;
    procedure CopySelectedToClipboard; safecall;
    function PasteFromClipboard(position: TPointF): TRectF; safecall;
    function CanPasteFromClipboard: Boolean; safecall;
    function LoadAsync(inputStream: IInputStream): IAsyncActionWithProgress_1__UInt64; safecall;
    function SaveAsync(outputStream: IOutputStream): IAsyncOperationWithProgress_2__Cardinal__Cardinal; safecall;
    procedure UpdateRecognitionResults(recognitionResults: IVectorView_1__IInkRecognitionResult); safecall;
    function GetStrokes: IVectorView_1__IInkStroke; safecall;
    function GetRecognitionResults: IVectorView_1__IInkRecognitionResult; safecall;
    property BoundingRect: TRectF read get_BoundingRect;
  end;

  // Windows.UI.Input.Inking.IInkStroke
  // External 
  IInkStroke = interface(IInspectable)
  ['{15144D60-CCE3-4FCF-9D52-11518AB6AFD4}']
    function get_DrawingAttributes: IInkDrawingAttributes; safecall;
    procedure put_DrawingAttributes(value: IInkDrawingAttributes); safecall;
    function get_BoundingRect: TRectF; safecall;
    function get_Selected: Boolean; safecall;
    procedure put_Selected(value: Boolean); safecall;
    function get_Recognized: Boolean; safecall;
    function GetRenderingSegments: IVectorView_1__IInkStrokeRenderingSegment; safecall;
    function Clone: IInkStroke; safecall;
    property BoundingRect: TRectF read get_BoundingRect;
    property DrawingAttributes: IInkDrawingAttributes read get_DrawingAttributes write put_DrawingAttributes;
    property Recognized: Boolean read get_Recognized;
    property Selected: Boolean read get_Selected write put_Selected;
  end;

  // DualAPI Interface
  // Windows.UI.Input.Inking.IInkDrawingAttributes
  [WinRTClassNameAttribute(SWindows_UI_Input_Inking_InkDrawingAttributes)]
  IInkDrawingAttributes = interface(IInspectable)
  ['{97A2176C-6774-48AD-84F0-48F5A9BE74F9}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    function get_PenTip: PenTipShape; safecall;
    procedure put_PenTip(value: PenTipShape); safecall;
    function get_Size: TSizeF; safecall;
    procedure put_Size(value: TSizeF); safecall;
    function get_IgnorePressure: Boolean; safecall;
    procedure put_IgnorePressure(value: Boolean); safecall;
    function get_FitToCurve: Boolean; safecall;
    procedure put_FitToCurve(value: Boolean); safecall;
    property Color_: Color read get_Color write put_Color;
    property FitToCurve: Boolean read get_FitToCurve write put_FitToCurve;
    property IgnorePressure: Boolean read get_IgnorePressure write put_IgnorePressure;
    property PenTip: PenTipShape read get_PenTip write put_PenTip;
    property Size: TSizeF read get_Size write put_Size;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Inking.IInkStrokeRenderingSegment>
  // External 
  IVectorView_1__IInkStrokeRenderingSegment = interface(IInspectable)
  ['{533D1D1F-D4DE-5B1B-8AD2-33127C4637DD}']
    function GetAt(index: Cardinal): IInkStrokeRenderingSegment; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IInkStrokeRenderingSegment; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIInkStrokeRenderingSegment): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Input.Inking.IInkStrokeRenderingSegment
  // External 
  IInkStrokeRenderingSegment = interface(IInspectable)
  ['{68510F1F-88E3-477A-A2FA-569F5F1F9BD5}']
    function get_Position: TPointF; safecall;
    function get_BezierControlPoint1: TPointF; safecall;
    function get_BezierControlPoint2: TPointF; safecall;
    function get_Pressure: Single; safecall;
    function get_TiltX: Single; safecall;
    function get_TiltY: Single; safecall;
    function get_Twist: Single; safecall;
    property BezierControlPoint1: TPointF read get_BezierControlPoint1;
    property BezierControlPoint2: TPointF read get_BezierControlPoint2;
    property Position: TPointF read get_Position;
    property Pressure: Single read get_Pressure;
    property TiltX: Single read get_TiltX;
    property TiltY: Single read get_TiltY;
    property Twist: Single read get_Twist;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>
  IIterable_1__Point_Base = interface(IInspectable)
  ['{C192280D-3A09-5423-9DC5-67B83EBDE41D}']
    function First: IIterator_1__Point; safecall;
  end;
  // DualAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>
  // External 
  IIterable_1__Point = interface(IIterable_1__Point_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Point>
  IIterator_1__Point_Base = interface(IInspectable)
  ['{C602B59E-0A8E-5E99-B478-2B564585278D}']
    function get_Current: TPointF; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPointF): Cardinal; safecall;
    property Current: TPointF read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Point>
  // External 
  IIterator_1__Point = interface(IIterator_1__Point_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Windows.Foundation.IAsyncActionWithProgress`1<UInt64>
  // External 
  IAsyncActionWithProgress_1__UInt64 = interface(IInspectable)
  ['{43F713D0-C49D-5E55-AEBF-AF395768351E}']
    procedure put_Progress(handler: AsyncActionProgressHandler_1__UInt64); safecall;
    function get_Progress: AsyncActionProgressHandler_1__UInt64; safecall;
    procedure put_Completed(handler: AsyncActionWithProgressCompletedHandler_1__UInt64); safecall;
    function get_Completed: AsyncActionWithProgressCompletedHandler_1__UInt64; safecall;
    procedure GetResults; safecall;
    property Progress: AsyncActionProgressHandler_1__UInt64 read get_Progress write put_Progress;
    property Completed: AsyncActionWithProgressCompletedHandler_1__UInt64 read get_Completed write put_Completed;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncActionProgressHandler`1<UInt64>
  AsyncActionProgressHandler_1__UInt64_Delegate_Base = interface(IUnknown)
  ['{55E233CA-F243-5AE2-853B-F9CC7C0AE0CF}']
    procedure Invoke(asyncInfo: IAsyncActionWithProgress_1__UInt64; progressInfo: UInt64); safecall;
  end;
  // Windows.Foundation.AsyncActionProgressHandler`1<UInt64>
  // External 
  AsyncActionProgressHandler_1__UInt64 = interface(AsyncActionProgressHandler_1__UInt64_Delegate_Base)
  ['{55E233CA-F243-5AE2-853B-F9CC7C0AE0CF}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncActionWithProgressCompletedHandler`1<UInt64>
  AsyncActionWithProgressCompletedHandler_1__UInt64_Delegate_Base = interface(IUnknown)
  ['{E6FF857B-F160-571A-A934-2C61F98C862D}']
    procedure Invoke(asyncInfo: IAsyncActionWithProgress_1__UInt64; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncActionWithProgressCompletedHandler`1<UInt64>
  // External 
  AsyncActionWithProgressCompletedHandler_1__UInt64 = interface(AsyncActionWithProgressCompletedHandler_1__UInt64_Delegate_Base)
  ['{E6FF857B-F160-571A-A934-2C61F98C862D}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Inking.IInkRecognitionResult>
  // External 
  IVectorView_1__IInkRecognitionResult = interface(IInspectable)
  ['{3C16D87C-E0C0-5689-A3D8-87D11003DEA9}']
    function GetAt(index: Cardinal): IInkRecognitionResult; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IInkRecognitionResult; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIInkRecognitionResult): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Input.Inking.IInkRecognitionResult
  // External 
  IInkRecognitionResult = interface(IInspectable)
  ['{36461A94-5068-40EF-8A05-2C2FB60908A2}']
    function get_BoundingRect: TRectF; safecall;
    function GetTextCandidates: IVectorView_1__HSTRING; safecall;
    function GetStrokes: IVectorView_1__IInkStroke; safecall;
    property BoundingRect: TRectF read get_BoundingRect;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Inking.IInkStroke>
  // External 
  IVectorView_1__IInkStroke = interface(IInspectable)
  ['{C3FC26F7-5323-55A2-90F5-5EBE01DAF672}']
    function GetAt(index: Cardinal): IInkStroke; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IInkStroke; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIInkStroke): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Input.Inking.IInkSynchronizer
  // External 
  IInkSynchronizer = interface(IInspectable)
  ['{9B9EA160-AE9B-45F9-8407-4B493B163661}']
    function BeginDry: IVectorView_1__IInkStroke; safecall;
    procedure EndDry; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkPresenter,Windows.UI.Input.Inking.IInkStrokesCollectedEventArgs>
  TypedEventHandler_2__IInkPresenter__IInkStrokesCollectedEventArgs_Delegate_Base = interface(IUnknown)
  ['{176BFA8F-C0DE-5B3A-B28C-0F3931CA52D3}']
    procedure Invoke(sender: IInkPresenter; args: IInkStrokesCollectedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkPresenter,Windows.UI.Input.Inking.IInkStrokesCollectedEventArgs>
  // External 
  TypedEventHandler_2__IInkPresenter__IInkStrokesCollectedEventArgs = interface(TypedEventHandler_2__IInkPresenter__IInkStrokesCollectedEventArgs_Delegate_Base)
  ['{EAEA1A9B-FCE4-51A5-B952-C4037CF4291E}']
  end;

  // Windows.UI.Input.Inking.IInkStrokesCollectedEventArgs
  // External 
  IInkStrokesCollectedEventArgs = interface(IInspectable)
  ['{C4F3F229-1938-495C-B4D9-6DE4B08D4811}']
    function get_Strokes: IVectorView_1__IInkStroke; safecall;
    property Strokes: IVectorView_1__IInkStroke read get_Strokes;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkPresenter,Windows.UI.Input.Inking.IInkStrokesErasedEventArgs>
  TypedEventHandler_2__IInkPresenter__IInkStrokesErasedEventArgs_Delegate_Base = interface(IUnknown)
  ['{30FEC929-14D0-550F-84F2-137FC6A9F08F}']
    procedure Invoke(sender: IInkPresenter; args: IInkStrokesErasedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.IInkPresenter,Windows.UI.Input.Inking.IInkStrokesErasedEventArgs>
  // External 
  TypedEventHandler_2__IInkPresenter__IInkStrokesErasedEventArgs = interface(TypedEventHandler_2__IInkPresenter__IInkStrokesErasedEventArgs_Delegate_Base)
  ['{78213819-AC61-5AC9-987D-45069CDCB496}']
  end;

  // Windows.UI.Input.Inking.IInkStrokesErasedEventArgs
  // External 
  IInkStrokesErasedEventArgs = interface(IInspectable)
  ['{A4216A22-1503-4EBF-8FF5-2DE84584A8AA}']
    function get_Strokes: IVectorView_1__IInkStroke; safecall;
    property Strokes: IVectorView_1__IInkStroke read get_Strokes;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.Core.ICoreInkIndependentInputSource,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs_Delegate_Base = interface(IUnknown)
  ['{B83FBE98-882A-5B69-BD1C-C66690707FEF}']
    procedure Invoke(sender: Core_ICoreInkIndependentInputSource; args: IPointerEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Inking.Core.ICoreInkIndependentInputSource,Windows.UI.Core.IPointerEventArgs>
  // External 
  TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs = interface(TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs_Delegate_Base)
  ['{D5E0528D-E040-5453-B8AE-4AABC229B374}']
  end;

  // DualAPI Interface
  // Windows.UI.Input.Inking.Core.ICoreInkIndependentInputSource
  [WinRTClassNameAttribute(SWindows_UI_Input_Inking_Core_CoreInkIndependentInputSource)]
  Core_ICoreInkIndependentInputSource = interface(IInspectable)
  ['{39B38DA9-7639-4499-A5B5-191D00E35B16}']
    function add_PointerEntering(handler: TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerEntering(cookie: EventRegistrationToken); safecall;
    function add_PointerHovering(handler: TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerHovering(cookie: EventRegistrationToken); safecall;
    function add_PointerExiting(handler: TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerExiting(cookie: EventRegistrationToken); safecall;
    function add_PointerPressing(handler: TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerPressing(cookie: EventRegistrationToken); safecall;
    function add_PointerMoving(handler: TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerMoving(cookie: EventRegistrationToken); safecall;
    function add_PointerReleasing(handler: TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerReleasing(cookie: EventRegistrationToken); safecall;
    function add_PointerLost(handler: TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs): EventRegistrationToken; safecall;
    procedure remove_PointerLost(cookie: EventRegistrationToken); safecall;
    function get_InkPresenter: IInkPresenter; safecall;
    property InkPresenter: IInkPresenter read get_InkPresenter;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialControllerMenuItem,Object>
  TypedEventHandler_2__IRadialControllerMenuItem__IInspectable_Delegate_Base = interface(IUnknown)
  ['{5C90D6FB-E4F8-5BE2-8544-9DE33F82C41A}']
    procedure Invoke(sender: IRadialControllerMenuItem; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialControllerMenuItem,Object>
  // External 
  TypedEventHandler_2__IRadialControllerMenuItem__IInspectable = interface(TypedEventHandler_2__IRadialControllerMenuItem__IInspectable_Delegate_Base)
  ['{2B1E4F55-8DA4-5328-9A67-2DE9E63DAFEB}']
  end;

  // DualAPI Interface
  // Windows.UI.Input.IRadialControllerMenuItem
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialControllerMenuItem)]
  IRadialControllerMenuItem = interface(IInspectable)
  ['{C80FC98D-AD0B-4C9C-8F2F-136A2373A6BA}']
    function get_DisplayText: HSTRING; safecall;
    function get_Tag: IInspectable; safecall;
    procedure put_Tag(value: IInspectable); safecall;
    function add_Invoked(handler: TypedEventHandler_2__IRadialControllerMenuItem__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Invoked(token: EventRegistrationToken); safecall;
    property DisplayText: HSTRING read get_DisplayText;
    property Tag: IInspectable read get_Tag write put_Tag;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Object>
  TypedEventHandler_2__IRadialController__IInspectable_Delegate_Base = interface(IUnknown)
  ['{5E0F93B1-C2F2-5351-82AA-6CF5F4C2D068}']
    procedure Invoke(sender: IRadialController; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Object>
  // External 
  TypedEventHandler_2__IRadialController__IInspectable = interface(TypedEventHandler_2__IRadialController__IInspectable_Delegate_Base)
  ['{4DA17D78-6F80-5F2E-A1DB-83F8EC01736B}']
  end;

  // DualAPI Interface
  // Windows.UI.Input.IRadialController
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialController)]
  IRadialController = interface(IInspectable)
  ['{3055D1C8-DF51-43D4-B23B-0E1037467A09}']
    function get_Menu: IRadialControllerMenu; safecall;
    function get_RotationResolutionInDegrees: Double; safecall;
    procedure put_RotationResolutionInDegrees(value: Double); safecall;
    function get_UseAutomaticHapticFeedback: Boolean; safecall;
    procedure put_UseAutomaticHapticFeedback(value: Boolean); safecall;
    function add_ScreenContactStarted(handler: TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ScreenContactStarted(cookie: EventRegistrationToken); safecall;
    function add_ScreenContactEnded(handler: TypedEventHandler_2__IRadialController__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ScreenContactEnded(cookie: EventRegistrationToken); safecall;
    function add_ScreenContactContinued(handler: TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ScreenContactContinued(cookie: EventRegistrationToken); safecall;
    function add_ControlLost(handler: TypedEventHandler_2__IRadialController__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ControlLost(cookie: EventRegistrationToken); safecall;
    function add_RotationChanged(handler: TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RotationChanged(token: EventRegistrationToken); safecall;
    function add_ButtonClicked(handler: TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ButtonClicked(token: EventRegistrationToken); safecall;
    function add_ControlAcquired(handler: TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs): EventRegistrationToken; safecall;
    procedure remove_ControlAcquired(cookie: EventRegistrationToken); safecall;
    property Menu: IRadialControllerMenu read get_Menu;
    property RotationResolutionInDegrees: Double read get_RotationResolutionInDegrees write put_RotationResolutionInDegrees;
    property UseAutomaticHapticFeedback: Boolean read get_UseAutomaticHapticFeedback write put_UseAutomaticHapticFeedback;
  end;

  // Windows.UI.Input.IRadialControllerMenu
  // External 
  IRadialControllerMenu = interface(IInspectable)
  ['{8506B35D-F640-4412-ABA0-BAD077E5EA8A}']
    function get_Items: IVector_1__IRadialControllerMenuItem; safecall;
    function get_IsEnabled: Boolean; safecall;
    procedure put_IsEnabled(value: Boolean); safecall;
    function GetSelectedMenuItem: IRadialControllerMenuItem; safecall;
    procedure SelectMenuItem(menuItem: IRadialControllerMenuItem); safecall;
    function TrySelectPreviouslySelectedMenuItem: Boolean; safecall;
    property IsEnabled: Boolean read get_IsEnabled write put_IsEnabled;
    property Items: IVector_1__IRadialControllerMenuItem read get_Items;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Input.IRadialControllerMenuItem>
  IVector_1__IRadialControllerMenuItem_Base = interface(IInspectable)
  ['{BFDE94E7-70F8-5CC0-98E2-8C0F8CE524AB}']
    function GetAt(index: Cardinal): IRadialControllerMenuItem; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IRadialControllerMenuItem; safecall;
    function IndexOf(value: IRadialControllerMenuItem; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IRadialControllerMenuItem); safecall;
    procedure InsertAt(index: Cardinal; value: IRadialControllerMenuItem); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IRadialControllerMenuItem); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIRadialControllerMenuItem): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIRadialControllerMenuItem); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Input.IRadialControllerMenuItem>
  // External 
  IVector_1__IRadialControllerMenuItem = interface(IVector_1__IRadialControllerMenuItem_Base)
  ['{700BA2AD-8D5A-5481-8B7C-1AAA3277697B}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.IRadialControllerMenuItem>
  // External 
  IVectorView_1__IRadialControllerMenuItem = interface(IInspectable)
  ['{EFD7C099-1361-58A2-819C-F7A8E94FAD5F}']
    function GetAt(index: Cardinal): IRadialControllerMenuItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IRadialControllerMenuItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIRadialControllerMenuItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerScreenContactStartedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs_Delegate_Base = interface(IUnknown)
  ['{1F2D584E-3AD8-5049-B451-3A44A102FA82}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerScreenContactStartedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerScreenContactStartedEventArgs>
  // External 
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs_Delegate_Base)
  ['{1DCA074B-CD0F-5F3F-93FE-4E34ED408A39}']
  end;

  // Windows.UI.Input.IRadialControllerScreenContactStartedEventArgs
  // External 
  IRadialControllerScreenContactStartedEventArgs = interface(IInspectable)
  ['{206AA436-E651-11E5-BF62-2C27D7404E85}']
    function get_Contact: IRadialControllerScreenContact; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
  end;

  // Windows.UI.Input.IRadialControllerScreenContact
  // External 
  IRadialControllerScreenContact = interface(IInspectable)
  ['{206AA434-E651-11E5-BF62-2C27D7404E85}']
    function get_Bounds: TRectF; safecall;
    function get_Position: TPointF; safecall;
    property Bounds: TRectF read get_Bounds;
    property Position: TPointF read get_Position;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerScreenContactContinuedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs_Delegate_Base = interface(IUnknown)
  ['{30579E67-FB4F-5D38-83B4-9CB610090DEF}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerScreenContactContinuedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerScreenContactContinuedEventArgs>
  // External 
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs_Delegate_Base)
  ['{5D23DB15-EE4E-5430-95EE-33FE7C4C9B11}']
  end;

  // Windows.UI.Input.IRadialControllerScreenContactContinuedEventArgs
  // External 
  IRadialControllerScreenContactContinuedEventArgs = interface(IInspectable)
  ['{206AA437-E651-11E5-BF62-2C27D7404E85}']
    function get_Contact: IRadialControllerScreenContact; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerRotationChangedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{8FE73AC9-8A36-5152-815D-03310EE8BF85}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerRotationChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerRotationChangedEventArgs>
  // External 
  TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs_Delegate_Base)
  ['{4D94E51C-1735-5608-A356-6272EC47F5A0}']
  end;

  // Windows.UI.Input.IRadialControllerRotationChangedEventArgs
  // External 
  IRadialControllerRotationChangedEventArgs = interface(IInspectable)
  ['{206AA435-E651-11E5-BF62-2C27D7404E85}']
    function get_RotationDeltaInDegrees: Double; safecall;
    function get_Contact: IRadialControllerScreenContact; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
    property RotationDeltaInDegrees: Double read get_RotationDeltaInDegrees;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonClickedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs_Delegate_Base = interface(IUnknown)
  ['{D09D9FCC-EDB8-56C0-856D-70E477A9DDF3}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerButtonClickedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonClickedEventArgs>
  // External 
  TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs_Delegate_Base)
  ['{B0128F18-A071-55B3-A456-74AFFFA9BCEF}']
  end;

  // Windows.UI.Input.IRadialControllerButtonClickedEventArgs
  // External 
  IRadialControllerButtonClickedEventArgs = interface(IInspectable)
  ['{206AA438-E651-11E5-BF62-2C27D7404E85}']
    function get_Contact: IRadialControllerScreenContact; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerControlAcquiredEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs_Delegate_Base = interface(IUnknown)
  ['{4EC5F3FD-B217-5452-A2BD-9725CE9F6675}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerControlAcquiredEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerControlAcquiredEventArgs>
  // External 
  TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs_Delegate_Base)
  ['{F6423CA5-444F-50D3-A999-D97B66AFAF4A}']
  end;

  // Windows.UI.Input.IRadialControllerControlAcquiredEventArgs
  // External 
  IRadialControllerControlAcquiredEventArgs = interface(IInspectable)
  ['{206AA439-E651-11E5-BF62-2C27D7404E85}']
    function get_Contact: IRadialControllerScreenContact; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Object>
  TypedEventHandler_2__IToastNotification__IInspectable_Delegate_Base = interface(IUnknown)
  ['{AB54DE2D-97D9-5528-B6AD-105AFE156530}']
    procedure Invoke(sender: IToastNotification; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Object>
  // External 
  TypedEventHandler_2__IToastNotification__IInspectable = interface(TypedEventHandler_2__IToastNotification__IInspectable_Delegate_Base)
  ['{93621AAC-6E87-5F7A-AA83-927B2D905518}']
  end;

  // DualAPI Interface
  // Windows.UI.Notifications.IToastNotification
  [WinRTClassNameAttribute(SWindows_UI_Notifications_ToastNotification)]
  IToastNotification = interface(IInspectable)
  ['{997E2675-059E-4E60-8B06-1760917C8B80}']
    function get_Content: Xml_Dom_IXmlDocument; safecall;
    procedure put_ExpirationTime(value: IReference_1__DateTime); safecall;
    function get_ExpirationTime: IReference_1__DateTime; safecall;
    function add_Dismissed(handler: TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Dismissed(token: EventRegistrationToken); safecall;
    function add_Activated(handler: TypedEventHandler_2__IToastNotification__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Activated(token: EventRegistrationToken); safecall;
    function add_Failed(handler: TypedEventHandler_2__IToastNotification__IToastFailedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Failed(token: EventRegistrationToken); safecall;
    property Content: Xml_Dom_IXmlDocument read get_Content;
    property ExpirationTime: IReference_1__DateTime read get_ExpirationTime write put_ExpirationTime;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Windows.UI.Notifications.IToastDismissedEventArgs>
  TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs_Delegate_Base = interface(IUnknown)
  ['{61C2402F-0ED0-5A18-AB69-59F4AA99A368}']
    procedure Invoke(sender: IToastNotification; args: IToastDismissedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Windows.UI.Notifications.IToastDismissedEventArgs>
  // External 
  TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs = interface(TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs_Delegate_Base)
  ['{52B78A8E-3D0A-5C4D-BBBA-24FAF495B9D4}']
  end;

  // Windows.UI.Notifications.IToastDismissedEventArgs
  // External 
  IToastDismissedEventArgs = interface(IInspectable)
  ['{3F89D935-D9CB-4538-A0F0-FFE7659938F8}']
    function get_Reason: ToastDismissalReason; safecall;
    property Reason: ToastDismissalReason read get_Reason;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Windows.UI.Notifications.IToastFailedEventArgs>
  TypedEventHandler_2__IToastNotification__IToastFailedEventArgs_Delegate_Base = interface(IUnknown)
  ['{95E3E803-C969-5E3A-9753-EA2AD22A9A33}']
    procedure Invoke(sender: IToastNotification; args: IToastFailedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Windows.UI.Notifications.IToastFailedEventArgs>
  // External 
  TypedEventHandler_2__IToastNotification__IToastFailedEventArgs = interface(TypedEventHandler_2__IToastNotification__IToastFailedEventArgs_Delegate_Base)
  ['{DB5E9BE6-9CDD-589F-96DA-A4A5D3697DFD}']
  end;

  // Windows.UI.Notifications.IToastFailedEventArgs
  // External 
  IToastFailedEventArgs = interface(IInspectable)
  ['{35176862-CFD4-44F8-AD64-F500FD896C3B}']
    function get_ErrorCode: HRESULT; safecall;
    property ErrorCode: HRESULT read get_ErrorCode;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.ViewManagement.Core.ICoreInputView,Object>
  // External 
  TypedEventHandler_2__Core_ICoreInputView__IInspectable = interface(IUnknown)
  ['{FF71F333-DBCC-54F2-8C61-0031720ED943}']
    procedure Invoke(sender: Core_ICoreInputView; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.ViewManagement.Core.ICoreInputView
  [WinRTClassNameAttribute(SWindows_UI_ViewManagement_Core_CoreInputView)]
  Core_ICoreInputView = interface(IInspectable)
  ['{C770CD7A-7001-4C32-BF94-25C1F554CBF1}']
    function add_OcclusionsChanged(handler: TypedEventHandler_2__Core_ICoreInputView__Core_ICoreInputViewOcclusionsChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_OcclusionsChanged(token: EventRegistrationToken); safecall;
    function GetCoreInputViewOcclusions: IVectorView_1__Core_ICoreInputViewOcclusion; safecall;
    function TryShowPrimaryView: Boolean; safecall;
    function TryHidePrimaryView: Boolean; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.ViewManagement.Core.ICoreInputView,Windows.UI.ViewManagement.Core.ICoreInputViewOcclusionsChangedEventArgs>
  TypedEventHandler_2__Core_ICoreInputView__Core_ICoreInputViewOcclusionsChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{5ADECF04-EDD1-5133-ABC7-582A027F09BB}']
    procedure Invoke(sender: Core_ICoreInputView; args: Core_ICoreInputViewOcclusionsChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.ViewManagement.Core.ICoreInputView,Windows.UI.ViewManagement.Core.ICoreInputViewOcclusionsChangedEventArgs>
  // External 
  TypedEventHandler_2__Core_ICoreInputView__Core_ICoreInputViewOcclusionsChangedEventArgs = interface(TypedEventHandler_2__Core_ICoreInputView__Core_ICoreInputViewOcclusionsChangedEventArgs_Delegate_Base)
  ['{A830315D-49C0-56DD-A0BC-D84200DC0AEB}']
  end;

  // Windows.UI.ViewManagement.Core.ICoreInputViewOcclusionsChangedEventArgs
  // External 
  Core_ICoreInputViewOcclusionsChangedEventArgs = interface(IInspectable)
  ['{BE1027E8-B3EE-4DF7-9554-89CDC66082C2}']
    function get_Occlusions: IVectorView_1__Core_ICoreInputViewOcclusion; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property Occlusions: IVectorView_1__Core_ICoreInputViewOcclusion read get_Occlusions;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.ViewManagement.Core.ICoreInputViewOcclusion>
  // External 
  IVectorView_1__Core_ICoreInputViewOcclusion = interface(IInspectable)
  ['{35DCF2CC-1E4B-5AE3-BF90-AB5702CB71D4}']
    function GetAt(index: Cardinal): Core_ICoreInputViewOcclusion; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_ICoreInputViewOcclusion; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_ICoreInputViewOcclusion): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.ViewManagement.Core.ICoreInputViewOcclusion
  // External 
  Core_ICoreInputViewOcclusion = interface(IInspectable)
  ['{CC36CE06-3865-4177-B5F5-8B65E0B9CE84}']
    function get_OccludingRect: TRectF; safecall;
    function get_OcclusionKind: Core_CoreInputViewOcclusionKind; safecall;
    property OccludingRect: TRectF read get_OccludingRect;
    property OcclusionKind: Core_CoreInputViewOcclusionKind read get_OcclusionKind;
  end;

  // Windows.UI.IUIContext
  // External 
  IUIContext = interface(IInspectable)
  ['{BB5CFACD-5BD8-59D0-A59E-1C17A4D6D243}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IStyle
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Style)]
  IStyle = interface(IInspectable)
  ['{C4A9F225-9DB7-4A7D-B6D1-F74EDB9293C2}']
    function get_IsSealed: Boolean; safecall;
    function get_Setters: ISetterBaseCollection; safecall;
    function get_TargetType: Interop_TypeName; safecall;
    procedure put_TargetType(value: Interop_TypeName); safecall;
    function get_BasedOn: IStyle; safecall;
    procedure put_BasedOn(value: IStyle); safecall;
    procedure Seal; safecall;
    property BasedOn: IStyle read get_BasedOn write put_BasedOn;
    property IsSealed: Boolean read get_IsSealed;
    property Setters: ISetterBaseCollection read get_Setters;
    property TargetType: Interop_TypeName read get_TargetType write put_TargetType;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.ISetterBaseCollection
  [WinRTClassNameAttribute(SWindows_UI_Xaml_SetterBaseCollection)]
  ISetterBaseCollection = interface(IInspectable)
  ['{03C40CA8-909E-4117-811C-A4529496BDF1}']
    function get_IsSealed: Boolean; safecall;
    property IsSealed: Boolean read get_IsSealed;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.ICommand
  // External 
  Input_ICommand = interface(IInspectable)
  ['{E5AF3542-CA67-4081-995B-709DD13792DF}']
    function add_CanExecuteChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_CanExecuteChanged(token: EventRegistrationToken); safecall;
    function CanExecute(parameter: IInspectable): Boolean; safecall;
    procedure Execute(parameter: IInspectable); safecall;
  end;

  // Windows.UI.Xaml.RoutedEventHandler
  // External 
  RoutedEventHandler = interface(IUnknown)
  ['{A856E674-B0B6-4BC3-BBA8-1BA06E40D4B5}']
    procedure Invoke(sender: IInspectable; e: IRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_RoutedEventArgs)]
  IRoutedEventArgs = interface(IInspectable)
  ['{5C985AC6-D802-4B38-A223-BF070C43FEDF}']
    function get_OriginalSource: IInspectable; safecall;
    property OriginalSource: IInspectable read get_OriginalSource;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IDataTemplate
  [WinRTClassNameAttribute(SWindows_UI_Xaml_DataTemplate)]
  IDataTemplate = interface(IInspectable)
  ['{9910AEC7-8AB5-4118-9BC6-09F45A35073D}']
    function LoadContent: IDependencyObject; safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IDependencyObject
  [WinRTClassNameAttribute(SWindows_UI_Xaml_DependencyObject)]
  IDependencyObject = interface(IInspectable)
  ['{5C526665-F60E-4912-AF59-5FE0680F089D}']
    function GetValue(dp: IDependencyProperty): IInspectable; safecall;
    procedure SetValue(dp: IDependencyProperty; value: IInspectable); safecall;
    procedure ClearValue(dp: IDependencyProperty); safecall;
    function ReadLocalValue(dp: IDependencyProperty): IInspectable; safecall;
    function GetAnimationBaseValue(dp: IDependencyProperty): IInspectable; safecall;
    function get_Dispatcher: ICoreDispatcher; safecall;
    property Dispatcher: ICoreDispatcher read get_Dispatcher;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IDependencyProperty
  [WinRTClassNameAttribute(SWindows_UI_Xaml_DependencyProperty)]
  IDependencyProperty = interface(IInspectable)
  ['{85B13970-9BC4-4E96-ACF1-30C8FD3D55C8}']
    function GetMetadata(forType: Interop_TypeName): IPropertyMetadata; safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IPropertyMetadata
  [WinRTClassNameAttribute(SWindows_UI_Xaml_PropertyMetadata)]
  IPropertyMetadata = interface(IInspectable)
  ['{814EF30D-8D18-448A-8644-F2CB51E70380}']
    function get_DefaultValue: IInspectable; safecall;
    function get_CreateDefaultValueCallback: CreateDefaultValueCallback; safecall;
    property CreateDefaultValueCallback_: CreateDefaultValueCallback read get_CreateDefaultValueCallback;
    property DefaultValue: IInspectable read get_DefaultValue;
  end;

  // Windows.UI.Xaml.CreateDefaultValueCallback
  // External 
  CreateDefaultValueCallback = interface(IUnknown)
  ['{D6ECB12C-15B5-4EC8-B95C-CDD208F08153}']
    function Invoke: IInspectable; safecall;
  end;

  // Windows.Foundation.Collections.IObservableVector`1<Object>
  // External 
  IObservableVector_1__IInspectable = interface(IInspectable)
  ['{7B81C56A-0985-518D-BAA9-0DA9AE009F65}']
    function add_VectorChanged(vhnd: VectorChangedEventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_VectorChanged(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Object>
  VectorChangedEventHandler_1__IInspectable_Delegate_Base = interface(IUnknown)
  ['{B423A801-D35E-56B9-813B-00889536CB98}']
    procedure Invoke(sender: IObservableVector_1__IInspectable; event: IVectorChangedEventArgs); safecall;
  end;
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Object>
  // External 
  VectorChangedEventHandler_1__IInspectable = interface(VectorChangedEventHandler_1__IInspectable_Delegate_Base)
  ['{B423A801-D35E-56B9-813B-00889536CB98}']
  end;

  // Windows.UI.Xaml.ExceptionRoutedEventHandler
  // External 
  ExceptionRoutedEventHandler = interface(IUnknown)
  ['{68E0E810-F6EA-42BC-855B-5D9B67E6A262}']
    procedure Invoke(sender: IInspectable; e: IExceptionRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IExceptionRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_ExceptionRoutedEventArgs)]
  IExceptionRoutedEventArgs = interface(IInspectable)
  ['{DD9FF16A-4B62-4A6C-A49D-0671EF6136BE}']
    function get_ErrorMessage: HSTRING; safecall;
    property ErrorMessage: HSTRING read get_ErrorMessage;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Object>
  IVector_1__IInspectable_Base = interface(IInspectable)
  ['{B32BDCA4-5E52-5B27-BC5D-D66A1A268C2A}']
    function GetAt(index: Cardinal): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IInspectable; safecall;
    function IndexOf(value: IInspectable; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IInspectable); safecall;
    procedure InsertAt(index: Cardinal; value: IInspectable); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IInspectable); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIInspectable): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIInspectable); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Object>
  // External 
  IVector_1__IInspectable = interface(IVector_1__IInspectable_Base)
  ['{B32BDCA4-5E52-5B27-BC5D-D66A1A268C2A}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Object>
  // External 
  IVectorView_1__IInspectable = interface(IInspectable)
  ['{A6487363-B074-5C60-AB16-866DCE4EE54D}']
    function GetAt(index: Cardinal): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IInspectable; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIInspectable): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Xaml.Data.LoadMoreItemsResult>
  IAsyncOperation_1__Data_LoadMoreItemsResult_Base = interface(IInspectable)
  ['{C788089D-37AB-5BA2-B865-5A309ACDFC4D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult; safecall;
    function GetResults: Data_LoadMoreItemsResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Xaml.Data.LoadMoreItemsResult>
  // External 
  IAsyncOperation_1__Data_LoadMoreItemsResult = interface(IAsyncOperation_1__Data_LoadMoreItemsResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Xaml.Data.LoadMoreItemsResult>
  AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult_Delegate_Base = interface(IUnknown)
  ['{10FB738B-A63B-506E-9ED7-2EAB37915221}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Data_LoadMoreItemsResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Xaml.Data.LoadMoreItemsResult>
  // External 
  AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult = interface(AsyncOperationCompletedHandler_1__Data_LoadMoreItemsResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IRatingControl,Object>
  TypedEventHandler_2__IRatingControl__IInspectable_Delegate_Base = interface(IUnknown)
  ['{180BE111-C600-5E5D-8266-FB29B9656AF4}']
    procedure Invoke(sender: IRatingControl; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IRatingControl,Object>
  // External 
  TypedEventHandler_2__IRatingControl__IInspectable = interface(TypedEventHandler_2__IRatingControl__IInspectable_Delegate_Base)
  ['{01971082-B764-5E1B-9982-50C2EBDC8487}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IRatingControl
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_RatingControl)]
  IRatingControl = interface(IInspectable)
  ['{A7D91CA7-E5CF-4963-A24E-9673FE5FFDD5}']
    function get_Caption: HSTRING; safecall;
    procedure put_Caption(value: HSTRING); safecall;
    function get_InitialSetValue: Integer; safecall;
    procedure put_InitialSetValue(value: Integer); safecall;
    function get_IsClearEnabled: Boolean; safecall;
    procedure put_IsClearEnabled(value: Boolean); safecall;
    function get_IsReadOnly: Boolean; safecall;
    procedure put_IsReadOnly(value: Boolean); safecall;
    function get_MaxRating: Integer; safecall;
    procedure put_MaxRating(value: Integer); safecall;
    function get_PlaceholderValue: Double; safecall;
    procedure put_PlaceholderValue(value: Double); safecall;
    function get_ItemInfo: IRatingItemInfo; safecall;
    procedure put_ItemInfo(value: IRatingItemInfo); safecall;
    function get_Value: Double; safecall;
    procedure put_Value(value: Double); safecall;
    function add_ValueChanged(handler: TypedEventHandler_2__IRatingControl__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ValueChanged(token: EventRegistrationToken); safecall;
    property Caption: HSTRING read get_Caption write put_Caption;
    property InitialSetValue: Integer read get_InitialSetValue write put_InitialSetValue;
    property IsClearEnabled: Boolean read get_IsClearEnabled write put_IsClearEnabled;
    property IsReadOnly: Boolean read get_IsReadOnly write put_IsReadOnly;
    property ItemInfo: IRatingItemInfo read get_ItemInfo write put_ItemInfo;
    property MaxRating: Integer read get_MaxRating write put_MaxRating;
    property PlaceholderValue: Double read get_PlaceholderValue write put_PlaceholderValue;
    property Value: Double read get_Value write put_Value;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IRatingItemInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_RatingItemInfo)]
  IRatingItemInfo = interface(IInspectable)
  ['{9CCBE6A2-591E-47A0-A318-6A1F7947DA2D}']
  end;

  // Windows.UI.Xaml.Documents.ITextPointer
  // External 
  Documents_ITextPointer = interface(IInspectable)
  ['{AC687AA1-6A41-43FF-851E-45348AA2CF7B}']
    function get_Parent: IDependencyObject; safecall;
    function get_VisualParent: IFrameworkElement; safecall;
    function get_LogicalDirection: Documents_LogicalDirection; safecall;
    function get_Offset: Integer; safecall;
    function GetCharacterRect(direction: Documents_LogicalDirection): TRectF; safecall;
    function GetPositionAtOffset(offset: Integer; direction: Documents_LogicalDirection): Documents_ITextPointer; safecall;
    property LogicalDirection: Documents_LogicalDirection read get_LogicalDirection;
    property Offset: Integer read get_Offset;
    property Parent: IDependencyObject read get_Parent;
    property VisualParent: IFrameworkElement read get_VisualParent;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IFrameworkElement
  [WinRTClassNameAttribute(SWindows_UI_Xaml_FrameworkElement)]
  IFrameworkElement = interface(IInspectable)
  ['{A391D09B-4A99-4B7C-9D8D-6FA5D01F6FBF}']
    function get_Triggers: IVector_1__ITriggerBase; safecall;
    function get_Resources: IResourceDictionary; safecall;
    procedure put_Resources(value: IResourceDictionary); safecall;
    function get_Tag: IInspectable; safecall;
    procedure put_Tag(value: IInspectable); safecall;
    function get_Language: HSTRING; safecall;
    procedure put_Language(value: HSTRING); safecall;
    function get_ActualWidth: Double; safecall;
    function get_ActualHeight: Double; safecall;
    function get_Width: Double; safecall;
    procedure put_Width(value: Double); safecall;
    function get_Height: Double; safecall;
    procedure put_Height(value: Double); safecall;
    function get_MinWidth: Double; safecall;
    procedure put_MinWidth(value: Double); safecall;
    function get_MaxWidth: Double; safecall;
    procedure put_MaxWidth(value: Double); safecall;
    function get_MinHeight: Double; safecall;
    procedure put_MinHeight(value: Double); safecall;
    function get_MaxHeight: Double; safecall;
    procedure put_MaxHeight(value: Double); safecall;
    function get_HorizontalAlignment: HorizontalAlignment; safecall;
    procedure put_HorizontalAlignment(value: HorizontalAlignment); safecall;
    function get_VerticalAlignment: VerticalAlignment; safecall;
    procedure put_VerticalAlignment(value: VerticalAlignment); safecall;
    function get_Margin: Thickness; safecall;
    procedure put_Margin(value: Thickness); safecall;
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    function get_BaseUri: IUriRuntimeClass; safecall;
    function get_DataContext: IInspectable; safecall;
    procedure put_DataContext(value: IInspectable); safecall;
    function get_Style: IStyle; safecall;
    procedure put_Style(value: IStyle); safecall;
    function get_Parent: IDependencyObject; safecall;
    function get_FlowDirection: FlowDirection; safecall;
    procedure put_FlowDirection(value: FlowDirection); safecall;
    function add_Loaded(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_Loaded(token: EventRegistrationToken); safecall;
    function add_Unloaded(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_Unloaded(token: EventRegistrationToken); safecall;
    function add_SizeChanged(handler: SizeChangedEventHandler): EventRegistrationToken; safecall;
    procedure remove_SizeChanged(token: EventRegistrationToken); safecall;
    function add_LayoutUpdated(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_LayoutUpdated(token: EventRegistrationToken); safecall;
    function FindName(name: HSTRING): IInspectable; safecall;
    procedure SetBinding(dp: IDependencyProperty; binding: Data_IBindingBase); safecall;
    property ActualHeight: Double read get_ActualHeight;
    property ActualWidth: Double read get_ActualWidth;
    property BaseUri: IUriRuntimeClass read get_BaseUri;
    property DataContext: IInspectable read get_DataContext write put_DataContext;
    property FlowDirection_: FlowDirection read get_FlowDirection write put_FlowDirection;
    property Height: Double read get_Height write put_Height;
    property HorizontalAlignment_: HorizontalAlignment read get_HorizontalAlignment write put_HorizontalAlignment;
    property Language: HSTRING read get_Language write put_Language;
    property Margin: Thickness read get_Margin write put_Margin;
    property MaxHeight: Double read get_MaxHeight write put_MaxHeight;
    property MaxWidth: Double read get_MaxWidth write put_MaxWidth;
    property MinHeight: Double read get_MinHeight write put_MinHeight;
    property MinWidth: Double read get_MinWidth write put_MinWidth;
    property Name: HSTRING read get_Name write put_Name;
    property Parent: IDependencyObject read get_Parent;
    property Resources: IResourceDictionary read get_Resources write put_Resources;
    property Style: IStyle read get_Style write put_Style;
    property Tag: IInspectable read get_Tag write put_Tag;
    property Triggers: IVector_1__ITriggerBase read get_Triggers;
    property VerticalAlignment_: VerticalAlignment read get_VerticalAlignment write put_VerticalAlignment;
    property Width: Double read get_Width write put_Width;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.ITriggerBase>
  IVector_1__ITriggerBase_Base = interface(IInspectable)
  ['{9149863B-B78B-5FB6-A0A1-9EBF6BBC3407}']
    function GetAt(index: Cardinal): ITriggerBase; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ITriggerBase; safecall;
    function IndexOf(value: ITriggerBase; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ITriggerBase); safecall;
    procedure InsertAt(index: Cardinal; value: ITriggerBase); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ITriggerBase); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PITriggerBase): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PITriggerBase); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.ITriggerBase>
  // External 
  IVector_1__ITriggerBase = interface(IVector_1__ITriggerBase_Base)
  ['{D2807252-3DB0-59BC-9E62-833B48C910AE}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.ITriggerBase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_TriggerBase)]
  ITriggerBase = interface(IInspectable)
  ['{E7EA222F-DEE6-4393-A8B2-8923D641F395}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.ITriggerBase>
  // External 
  IVectorView_1__ITriggerBase = interface(IInspectable)
  ['{1F02B86D-8C50-56B1-A2E6-4F0DCE5DC7D6}']
    function GetAt(index: Cardinal): ITriggerBase; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ITriggerBase; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PITriggerBase): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IResourceDictionary
  [WinRTClassNameAttribute(SWindows_UI_Xaml_ResourceDictionary)]
  IResourceDictionary = interface(IInspectable)
  ['{C1EA4F24-D6DE-4191-8E3A-F48601F7489C}']
    function get_Source: IUriRuntimeClass; safecall;
    procedure put_Source(value: IUriRuntimeClass); safecall;
    function get_MergedDictionaries: IVector_1__IResourceDictionary; safecall;
    function get_ThemeDictionaries: IMap_2__IInspectable__IInspectable; safecall;
    property MergedDictionaries: IVector_1__IResourceDictionary read get_MergedDictionaries;
    property Source: IUriRuntimeClass read get_Source write put_Source;
    property ThemeDictionaries: IMap_2__IInspectable__IInspectable read get_ThemeDictionaries;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IResourceDictionary>
  IVector_1__IResourceDictionary_Base = interface(IInspectable)
  ['{FE820A0C-694D-518B-8EC5-372993F6CEAF}']
    function GetAt(index: Cardinal): IResourceDictionary; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IResourceDictionary; safecall;
    function IndexOf(value: IResourceDictionary; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IResourceDictionary); safecall;
    procedure InsertAt(index: Cardinal; value: IResourceDictionary); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IResourceDictionary); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIResourceDictionary): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIResourceDictionary); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IResourceDictionary>
  // External 
  IVector_1__IResourceDictionary = interface(IVector_1__IResourceDictionary_Base)
  ['{BB19B4F1-5825-5778-960D-16E25A65201C}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.IResourceDictionary>
  // External 
  IVectorView_1__IResourceDictionary = interface(IInspectable)
  ['{7EA5E3DC-1E1E-5DA2-9355-AEE6CFB63CCF}']
    function GetAt(index: Cardinal): IResourceDictionary; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IResourceDictionary; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIResourceDictionary): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IMap`2<Object,Object>
  // External 
  IMap_2__IInspectable__IInspectable = interface(IInspectable)
  ['{F5F69427-55ED-5512-8429-D4F6626DFCDD}']
    function Lookup(key: IInspectable): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: IInspectable): Boolean; safecall;
    function GetView: IMapView_2__IInspectable__IInspectable; safecall;
    function Insert(key: IInspectable; value: IInspectable): Boolean; safecall;
    procedure Remove(key: IInspectable); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<Object,Object>
  IMapView_2__IInspectable__IInspectable_Base = interface(IInspectable)
  ['{EFE76D10-CB60-50AD-8A4F-6885CD6212A1}']
    function Lookup(key: IInspectable): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: IInspectable): Boolean; safecall;
    procedure Split(out first: IMapView_2__IInspectable__IInspectable; out second: IMapView_2__IInspectable__IInspectable); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<Object,Object>
  // External 
  IMapView_2__IInspectable__IInspectable = interface(IMapView_2__IInspectable__IInspectable_Base)
  ['{EFE76D10-CB60-50AD-8A4F-6885CD6212A1}']
  end;

  // Windows.UI.Xaml.SizeChangedEventHandler
  // External 
  SizeChangedEventHandler = interface(IUnknown)
  ['{1115B13C-25D2-480B-89DC-EB3DCBD6B7FA}']
    procedure Invoke(sender: IInspectable; e: ISizeChangedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.ISizeChangedEventArgs
  // External 
  ISizeChangedEventArgs = interface(IInspectable)
  ['{D5312E60-5CC1-42A1-920C-1AF46BE2F986}']
    function get_PreviousSize: TSizeF; safecall;
    function get_NewSize: TSizeF; safecall;
    property NewSize: TSizeF read get_NewSize;
    property PreviousSize: TSizeF read get_PreviousSize;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Data.IBindingBase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Data_BindingBase)]
  Data_IBindingBase = interface(IInspectable)
  ['{1589A2AB-3D15-49BC-A447-8A5448E58870}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.IBlock>
  IVector_1__Documents_IBlock_Base = interface(IInspectable)
  ['{3EE78A34-160E-50FF-B5AA-09F263A669F8}']
    function GetAt(index: Cardinal): Documents_IBlock; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Documents_IBlock; safecall;
    function IndexOf(value: Documents_IBlock; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Documents_IBlock); safecall;
    procedure InsertAt(index: Cardinal; value: Documents_IBlock); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Documents_IBlock); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_IBlock): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDocuments_IBlock); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.IBlock>
  // External 
  IVector_1__Documents_IBlock = interface(IVector_1__Documents_IBlock_Base)
  ['{4A2909A0-A23A-5D49-8B04-456995429EB0}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Documents.IBlock
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Documents_Block)]
  Documents_IBlock = interface(IInspectable)
  ['{4BCE0016-DD47-4350-8CB0-E171600AC896}']
    function get_TextAlignment: TextAlignment; safecall;
    procedure put_TextAlignment(value: TextAlignment); safecall;
    function get_LineHeight: Double; safecall;
    procedure put_LineHeight(value: Double); safecall;
    function get_LineStackingStrategy: LineStackingStrategy; safecall;
    procedure put_LineStackingStrategy(value: LineStackingStrategy); safecall;
    function get_Margin: Thickness; safecall;
    procedure put_Margin(value: Thickness); safecall;
    property LineHeight: Double read get_LineHeight write put_LineHeight;
    property LineStackingStrategy_: LineStackingStrategy read get_LineStackingStrategy write put_LineStackingStrategy;
    property Margin: Thickness read get_Margin write put_Margin;
    property TextAlignment_: TextAlignment read get_TextAlignment write put_TextAlignment;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.IBlock>
  // External 
  IVectorView_1__Documents_IBlock = interface(IInspectable)
  ['{FF7F1640-984C-5779-BCAD-563D20A384B5}']
    function GetAt(index: Cardinal): Documents_IBlock; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Documents_IBlock; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_IBlock): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.IRoutedEventArgs>
  TypedEventHandler_2__ISearchBox__IRoutedEventArgs_Delegate_Base = interface(IUnknown)
  ['{A16EFF3C-CC37-554C-8F10-0F7E6E2BCAAD}']
    procedure Invoke(sender: ISearchBox; args: IRoutedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.IRoutedEventArgs>
  // External 
  TypedEventHandler_2__ISearchBox__IRoutedEventArgs = interface(TypedEventHandler_2__ISearchBox__IRoutedEventArgs_Delegate_Base)
  ['{85C667C4-D0D4-52A6-B167-A35937B43A03}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.ISearchBox
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_SearchBox)]
  ISearchBox = interface(IInspectable)
  ['{F89ECC5A-99BA-4BD4-966C-F11FA443D13C}']
    function get_SearchHistoryEnabled: Boolean; safecall;
    procedure put_SearchHistoryEnabled(value: Boolean); safecall;
    function get_SearchHistoryContext: HSTRING; safecall;
    procedure put_SearchHistoryContext(value: HSTRING); safecall;
    function get_PlaceholderText: HSTRING; safecall;
    procedure put_PlaceholderText(value: HSTRING); safecall;
    function get_QueryText: HSTRING; safecall;
    procedure put_QueryText(value: HSTRING); safecall;
    function get_FocusOnKeyboardInput: Boolean; safecall;
    procedure put_FocusOnKeyboardInput(value: Boolean); safecall;
    function get_ChooseSuggestionOnEnter: Boolean; safecall;
    procedure put_ChooseSuggestionOnEnter(value: Boolean); safecall;
    function add_QueryChanged(handler: TypedEventHandler_2__ISearchBox__ISearchBoxQueryChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_QueryChanged(token: EventRegistrationToken); safecall;
    function add_SuggestionsRequested(handler: TypedEventHandler_2__ISearchBox__ISearchBoxSuggestionsRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_SuggestionsRequested(token: EventRegistrationToken); safecall;
    function add_QuerySubmitted(handler: TypedEventHandler_2__ISearchBox__ISearchBoxQuerySubmittedEventArgs): EventRegistrationToken; safecall;
    procedure remove_QuerySubmitted(token: EventRegistrationToken); safecall;
    function add_ResultSuggestionChosen(handler: TypedEventHandler_2__ISearchBox__ISearchBoxResultSuggestionChosenEventArgs): EventRegistrationToken; safecall;
    procedure remove_ResultSuggestionChosen(token: EventRegistrationToken); safecall;
    function add_PrepareForFocusOnKeyboardInput(handler: TypedEventHandler_2__ISearchBox__IRoutedEventArgs): EventRegistrationToken; safecall;
    procedure remove_PrepareForFocusOnKeyboardInput(token: EventRegistrationToken); safecall;
    procedure SetLocalContentSuggestionSettings(settings: Search_ILocalContentSuggestionSettings); safecall;
    property ChooseSuggestionOnEnter: Boolean read get_ChooseSuggestionOnEnter write put_ChooseSuggestionOnEnter;
    property FocusOnKeyboardInput: Boolean read get_FocusOnKeyboardInput write put_FocusOnKeyboardInput;
    property PlaceholderText: HSTRING read get_PlaceholderText write put_PlaceholderText;
    property QueryText: HSTRING read get_QueryText write put_QueryText;
    property SearchHistoryContext: HSTRING read get_SearchHistoryContext write put_SearchHistoryContext;
    property SearchHistoryEnabled: Boolean read get_SearchHistoryEnabled write put_SearchHistoryEnabled;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxQueryChangedEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxQueryChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{B15CCC28-40F3-52DA-9011-87621E9C60EA}']
    procedure Invoke(sender: ISearchBox; args: ISearchBoxQueryChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxQueryChangedEventArgs>
  // External 
  TypedEventHandler_2__ISearchBox__ISearchBoxQueryChangedEventArgs = interface(TypedEventHandler_2__ISearchBox__ISearchBoxQueryChangedEventArgs_Delegate_Base)
  ['{FB1E3F1A-524E-53CC-B220-19DEDEAEE9AC}']
  end;

  // Windows.UI.Xaml.Controls.ISearchBoxQueryChangedEventArgs
  // External 
  ISearchBoxQueryChangedEventArgs = interface(IInspectable)
  ['{A9A70F8F-0CB0-4BD2-9998-2FB57AD5E731}']
    function get_QueryText: HSTRING; safecall;
    function get_Language: HSTRING; safecall;
    function get_LinguisticDetails: Search_ISearchQueryLinguisticDetails; safecall;
    property Language: HSTRING read get_Language;
    property LinguisticDetails: Search_ISearchQueryLinguisticDetails read get_LinguisticDetails;
    property QueryText: HSTRING read get_QueryText;
  end;

  // Windows.ApplicationModel.Search.ISearchQueryLinguisticDetails
  // External 
  Search_ISearchQueryLinguisticDetails = interface(IInspectable)
  ['{46A1205B-69C9-4745-B72F-A8A4FC8F24AE}']
    function get_QueryTextAlternatives: IVectorView_1__HSTRING; safecall;
    function get_QueryTextCompositionStart: Cardinal; safecall;
    function get_QueryTextCompositionLength: Cardinal; safecall;
    property QueryTextAlternatives: IVectorView_1__HSTRING read get_QueryTextAlternatives;
    property QueryTextCompositionLength: Cardinal read get_QueryTextCompositionLength;
    property QueryTextCompositionStart: Cardinal read get_QueryTextCompositionStart;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxSuggestionsRequestedEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxSuggestionsRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{287DD4FE-12FC-5289-AB8A-7C7217024A6B}']
    procedure Invoke(sender: ISearchBox; args: ISearchBoxSuggestionsRequestedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxSuggestionsRequestedEventArgs>
  // External 
  TypedEventHandler_2__ISearchBox__ISearchBoxSuggestionsRequestedEventArgs = interface(TypedEventHandler_2__ISearchBox__ISearchBoxSuggestionsRequestedEventArgs_Delegate_Base)
  ['{3C64FAE4-38CC-5BCD-A4A2-A9742188A883}']
  end;

  // Windows.UI.Xaml.Controls.ISearchBoxSuggestionsRequestedEventArgs
  // External 
  ISearchBoxSuggestionsRequestedEventArgs = interface(IInspectable)
  ['{DA15170E-E566-48CB-BD11-FE4B0F30A44D}']
    function get_QueryText: HSTRING; safecall;
    function get_Language: HSTRING; safecall;
    function get_LinguisticDetails: Search_ISearchQueryLinguisticDetails; safecall;
    function get_Request: Search_ISearchSuggestionsRequest; safecall;
    property Language: HSTRING read get_Language;
    property LinguisticDetails: Search_ISearchQueryLinguisticDetails read get_LinguisticDetails;
    property QueryText: HSTRING read get_QueryText;
    property Request: Search_ISearchSuggestionsRequest read get_Request;
  end;

  // Windows.ApplicationModel.Search.ISearchSuggestionsRequest
  // External 
  Search_ISearchSuggestionsRequest = interface(IInspectable)
  ['{4E4E26A7-44E5-4039-9099-6000EAD1F0C6}']
    function get_IsCanceled: Boolean; safecall;
    function get_SearchSuggestionCollection: Search_ISearchSuggestionCollection; safecall;
    function GetDeferral: Search_ISearchSuggestionsRequestDeferral; safecall;
    property IsCanceled: Boolean read get_IsCanceled;
    property SearchSuggestionCollection: Search_ISearchSuggestionCollection read get_SearchSuggestionCollection;
  end;

  // Windows.ApplicationModel.Search.ISearchSuggestionCollection
  // External 
  Search_ISearchSuggestionCollection = interface(IInspectable)
  ['{323A8A4B-FBEA-4446-ABBC-3DA7915FDD3A}']
    function get_Size: Cardinal; safecall;
    procedure AppendQuerySuggestion(text: HSTRING); safecall;
    procedure AppendQuerySuggestions(suggestions: IIterable_1__HSTRING); safecall;
    procedure AppendResultSuggestion(text: HSTRING; detailText: HSTRING; tag: HSTRING; image: IRandomAccessStreamReference; imageAlternateText: HSTRING); safecall;
    procedure AppendSearchSeparator(&label: HSTRING); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.ApplicationModel.Search.ISearchSuggestionsRequestDeferral
  // External 
  Search_ISearchSuggestionsRequestDeferral = interface(IInspectable)
  ['{B71598A9-C065-456D-A845-1ECCEC5DC28B}']
    procedure Complete; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxQuerySubmittedEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxQuerySubmittedEventArgs_Delegate_Base = interface(IUnknown)
  ['{56D6C824-A5B2-51F4-8AF7-7B8EE582C029}']
    procedure Invoke(sender: ISearchBox; args: ISearchBoxQuerySubmittedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxQuerySubmittedEventArgs>
  // External 
  TypedEventHandler_2__ISearchBox__ISearchBoxQuerySubmittedEventArgs = interface(TypedEventHandler_2__ISearchBox__ISearchBoxQuerySubmittedEventArgs_Delegate_Base)
  ['{345A868F-B400-5967-819C-277131C5B3CE}']
  end;

  // Windows.UI.Xaml.Controls.ISearchBoxQuerySubmittedEventArgs
  // External 
  ISearchBoxQuerySubmittedEventArgs = interface(IInspectable)
  ['{126E90FD-3C4E-4CCB-9AEF-4705D19FE548}']
    function get_QueryText: HSTRING; safecall;
    function get_Language: HSTRING; safecall;
    function get_LinguisticDetails: Search_ISearchQueryLinguisticDetails; safecall;
    function get_KeyModifiers: VirtualKeyModifiers; safecall;
    property KeyModifiers: VirtualKeyModifiers read get_KeyModifiers;
    property Language: HSTRING read get_Language;
    property LinguisticDetails: Search_ISearchQueryLinguisticDetails read get_LinguisticDetails;
    property QueryText: HSTRING read get_QueryText;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxResultSuggestionChosenEventArgs>
  TypedEventHandler_2__ISearchBox__ISearchBoxResultSuggestionChosenEventArgs_Delegate_Base = interface(IUnknown)
  ['{8D18767B-A4B8-52FC-8767-F87D05B5172E}']
    procedure Invoke(sender: ISearchBox; args: ISearchBoxResultSuggestionChosenEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISearchBox,Windows.UI.Xaml.Controls.ISearchBoxResultSuggestionChosenEventArgs>
  // External 
  TypedEventHandler_2__ISearchBox__ISearchBoxResultSuggestionChosenEventArgs = interface(TypedEventHandler_2__ISearchBox__ISearchBoxResultSuggestionChosenEventArgs_Delegate_Base)
  ['{3082D9E4-AB39-5299-8DC1-4F969DF0EF53}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.ISearchBoxResultSuggestionChosenEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_SearchBoxResultSuggestionChosenEventArgs)]
  ISearchBoxResultSuggestionChosenEventArgs = interface(IInspectable)
  ['{18918C23-E4C3-4662-A03B-D054FFD0F905}']
    function get_Tag: HSTRING; safecall;
    function get_KeyModifiers: VirtualKeyModifiers; safecall;
    property KeyModifiers: VirtualKeyModifiers read get_KeyModifiers;
    property Tag: HSTRING read get_Tag;
  end;

  // Windows.ApplicationModel.Search.ILocalContentSuggestionSettings
  // External 
  Search_ILocalContentSuggestionSettings = interface(IInspectable)
  ['{EEAEB062-743D-456E-84A3-23F06F2D15D7}']
    procedure put_Enabled(value: Boolean); safecall;
    function get_Enabled: Boolean; safecall;
    function get_Locations: IVector_1__IStorageFolder; safecall;
    procedure put_AqsFilter(value: HSTRING); safecall;
    function get_AqsFilter: HSTRING; safecall;
    function get_PropertiesToMatch: IVector_1__HSTRING; safecall;
    property AqsFilter: HSTRING read get_AqsFilter write put_AqsFilter;
    property Enabled: Boolean read get_Enabled write put_Enabled;
    property Locations: IVector_1__IStorageFolder read get_Locations;
    property PropertiesToMatch: IVector_1__HSTRING read get_PropertiesToMatch;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageFolder>
  IVector_1__IStorageFolder_Base = interface(IInspectable)
  ['{6C26B7BE-5F01-5A60-9DD7-FD17BE3A9DD6}']
    function GetAt(index: Cardinal): IStorageFolder; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IStorageFolder; safecall;
    function IndexOf(value: IStorageFolder; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IStorageFolder); safecall;
    procedure InsertAt(index: Cardinal; value: IStorageFolder); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IStorageFolder); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorageFolder): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIStorageFolder); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageFolder>
  // External 
  IVector_1__IStorageFolder = interface(IVector_1__IStorageFolder_Base)
  ['{AD736464-7886-5872-88E3-395643C94759}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Data.IValueConverter
  // External 
  Data_IValueConverter = interface(IInspectable)
  ['{E6F2FEF0-0712-487F-B313-F300B8D79AA1}']
    function Convert(value: IInspectable; targetType: Interop_TypeName; parameter: IInspectable; language: HSTRING): IInspectable; safecall;
    function ConvertBack(value: IInspectable; targetType: Interop_TypeName; parameter: IInspectable; language: HSTRING): IInspectable; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.IInline>
  IVector_1__Documents_IInline_Base = interface(IInspectable)
  ['{92EC9252-8EE3-55D6-84B4-30B635077778}']
    function GetAt(index: Cardinal): Documents_IInline; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Documents_IInline; safecall;
    function IndexOf(value: Documents_IInline; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Documents_IInline); safecall;
    procedure InsertAt(index: Cardinal; value: Documents_IInline); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Documents_IInline); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_IInline): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDocuments_IInline); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.IInline>
  // External 
  IVector_1__Documents_IInline = interface(IVector_1__Documents_IInline_Base)
  ['{B2A0B20E-7494-5B14-A88C-1D538E537822}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Documents.IInline
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Documents_Inline)]
  Documents_IInline = interface(IInspectable)
  ['{0C92712D-1BC9-4931-8CB1-1AEADF1CC685}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.IInline>
  // External 
  IVectorView_1__Documents_IInline = interface(IInspectable)
  ['{F268A1C3-C8AE-539A-9C3A-4444DAE728EB}']
    function GetAt(index: Cardinal): Documents_IInline; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Documents_IInline; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_IInline): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IInputScope
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_InputScope)]
  Input_IInputScope = interface(IInspectable)
  ['{5C0F85F3-F9D8-4220-B666-045D074D9BFA}']
    function get_Names: IVector_1__Input_IInputScopeName; safecall;
    property Names: IVector_1__Input_IInputScopeName read get_Names;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Input.IInputScopeName>
  IVector_1__Input_IInputScopeName_Base = interface(IInspectable)
  ['{703FE123-D766-562F-B210-1980BB2A0D33}']
    function GetAt(index: Cardinal): Input_IInputScopeName; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Input_IInputScopeName; safecall;
    function IndexOf(value: Input_IInputScopeName; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Input_IInputScopeName); safecall;
    procedure InsertAt(index: Cardinal; value: Input_IInputScopeName); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Input_IInputScopeName); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IInputScopeName): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PInput_IInputScopeName); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Input.IInputScopeName>
  // External 
  IVector_1__Input_IInputScopeName = interface(IVector_1__Input_IInputScopeName_Base)
  ['{74289E29-6B7A-56FE-A74C-C982C4F498D6}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IInputScopeName
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_InputScopeName)]
  Input_IInputScopeName = interface(IInspectable)
  ['{FD3E6997-08FB-4CBA-A021-792D7589FD5A}']
    function get_NameValue: Input_InputScopeNameValue; safecall;
    procedure put_NameValue(value: Input_InputScopeNameValue); safecall;
    property NameValue: Input_InputScopeNameValue read get_NameValue write put_NameValue;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Input.IInputScopeName>
  // External 
  IVectorView_1__Input_IInputScopeName = interface(IInspectable)
  ['{56229622-D986-549F-B23A-A88C6939C5B9}']
    function GetAt(index: Cardinal): Input_IInputScopeName; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IInputScopeName; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IInputScopeName): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IUIElement
  [WinRTClassNameAttribute(SWindows_UI_Xaml_UIElement)]
  IUIElement = interface(IInspectable)
  ['{676D0BE9-B65C-41C6-BA40-58CF87F201C1}']
    function get_DesiredSize: TSizeF; safecall;
    function get_AllowDrop: Boolean; safecall;
    procedure put_AllowDrop(value: Boolean); safecall;
    function get_Opacity: Double; safecall;
    procedure put_Opacity(value: Double); safecall;
    function get_Clip: IRectangleGeometry; safecall;
    procedure put_Clip(value: IRectangleGeometry); safecall;
    function get_RenderTransform: ITransform; safecall;
    procedure put_RenderTransform(value: ITransform); safecall;
    function get_Projection: IProjection; safecall;
    procedure put_Projection(value: IProjection); safecall;
    function get_RenderTransformOrigin: TPointF; safecall;
    procedure put_RenderTransformOrigin(value: TPointF); safecall;
    function get_IsHitTestVisible: Boolean; safecall;
    procedure put_IsHitTestVisible(value: Boolean); safecall;
    function get_Visibility: Visibility; safecall;
    procedure put_Visibility(value: Visibility); safecall;
    function get_RenderSize: TSizeF; safecall;
    function get_UseLayoutRounding: Boolean; safecall;
    procedure put_UseLayoutRounding(value: Boolean); safecall;
    function get_Transitions: IVector_1__Animation_ITransition; safecall;
    procedure put_Transitions(value: IVector_1__Animation_ITransition); safecall;
    function get_CacheMode: ICacheMode; safecall;
    procedure put_CacheMode(value: ICacheMode); safecall;
    function get_IsTapEnabled: Boolean; safecall;
    procedure put_IsTapEnabled(value: Boolean); safecall;
    function get_IsDoubleTapEnabled: Boolean; safecall;
    procedure put_IsDoubleTapEnabled(value: Boolean); safecall;
    function get_IsRightTapEnabled: Boolean; safecall;
    procedure put_IsRightTapEnabled(value: Boolean); safecall;
    function get_IsHoldingEnabled: Boolean; safecall;
    procedure put_IsHoldingEnabled(value: Boolean); safecall;
    function get_ManipulationMode: Input_ManipulationModes; safecall;
    procedure put_ManipulationMode(value: Input_ManipulationModes); safecall;
    function get_PointerCaptures: IVectorView_1__Input_IPointer; safecall;
    function add_KeyUp(handler: Input_KeyEventHandler): EventRegistrationToken; safecall;
    procedure remove_KeyUp(token: EventRegistrationToken); safecall;
    function add_KeyDown(handler: Input_KeyEventHandler): EventRegistrationToken; safecall;
    procedure remove_KeyDown(token: EventRegistrationToken); safecall;
    function add_GotFocus(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_GotFocus(token: EventRegistrationToken); safecall;
    function add_LostFocus(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_LostFocus(token: EventRegistrationToken); safecall;
    function add_DragEnter(handler: DragEventHandler): EventRegistrationToken; safecall;
    procedure remove_DragEnter(token: EventRegistrationToken); safecall;
    function add_DragLeave(handler: DragEventHandler): EventRegistrationToken; safecall;
    procedure remove_DragLeave(token: EventRegistrationToken); safecall;
    function add_DragOver(handler: DragEventHandler): EventRegistrationToken; safecall;
    procedure remove_DragOver(token: EventRegistrationToken); safecall;
    function add_Drop(handler: DragEventHandler): EventRegistrationToken; safecall;
    procedure remove_Drop(token: EventRegistrationToken); safecall;
    function add_PointerPressed(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerPressed(token: EventRegistrationToken); safecall;
    function add_PointerMoved(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerMoved(token: EventRegistrationToken); safecall;
    function add_PointerReleased(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerReleased(token: EventRegistrationToken); safecall;
    function add_PointerEntered(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerEntered(token: EventRegistrationToken); safecall;
    function add_PointerExited(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerExited(token: EventRegistrationToken); safecall;
    function add_PointerCaptureLost(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerCaptureLost(token: EventRegistrationToken); safecall;
    function add_PointerCanceled(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerCanceled(token: EventRegistrationToken); safecall;
    function add_PointerWheelChanged(handler: Input_PointerEventHandler): EventRegistrationToken; safecall;
    procedure remove_PointerWheelChanged(token: EventRegistrationToken); safecall;
    function add_Tapped(handler: Input_TappedEventHandler): EventRegistrationToken; safecall;
    procedure remove_Tapped(token: EventRegistrationToken); safecall;
    function add_DoubleTapped(handler: Input_DoubleTappedEventHandler): EventRegistrationToken; safecall;
    procedure remove_DoubleTapped(token: EventRegistrationToken); safecall;
    function add_Holding(handler: Input_HoldingEventHandler): EventRegistrationToken; safecall;
    procedure remove_Holding(token: EventRegistrationToken); safecall;
    function add_RightTapped(handler: Input_RightTappedEventHandler): EventRegistrationToken; safecall;
    procedure remove_RightTapped(token: EventRegistrationToken); safecall;
    function add_ManipulationStarting(handler: Input_ManipulationStartingEventHandler): EventRegistrationToken; safecall;
    procedure remove_ManipulationStarting(token: EventRegistrationToken); safecall;
    function add_ManipulationInertiaStarting(handler: Input_ManipulationInertiaStartingEventHandler): EventRegistrationToken; safecall;
    procedure remove_ManipulationInertiaStarting(token: EventRegistrationToken); safecall;
    function add_ManipulationStarted(handler: Input_ManipulationStartedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ManipulationStarted(token: EventRegistrationToken); safecall;
    function add_ManipulationDelta(handler: Input_ManipulationDeltaEventHandler): EventRegistrationToken; safecall;
    procedure remove_ManipulationDelta(token: EventRegistrationToken); safecall;
    function add_ManipulationCompleted(handler: Input_ManipulationCompletedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ManipulationCompleted(token: EventRegistrationToken); safecall;
    procedure Measure(availableSize: TSizeF); safecall;
    procedure Arrange(finalRect: TRectF); safecall;
    function CapturePointer(value: Input_IPointer): Boolean; safecall;
    procedure ReleasePointerCapture(value: Input_IPointer); safecall;
    procedure ReleasePointerCaptures; safecall;
    procedure AddHandler(routedEvent: IRoutedEvent; handler: IInspectable; handledEventsToo: Boolean); safecall;
    procedure RemoveHandler(routedEvent: IRoutedEvent; handler: IInspectable); safecall;
    function TransformToVisual(visual: IUIElement): IGeneralTransform; safecall;
    procedure InvalidateMeasure; safecall;
    procedure InvalidateArrange; safecall;
    procedure UpdateLayout; safecall;
    property AllowDrop: Boolean read get_AllowDrop write put_AllowDrop;
    property CacheMode: ICacheMode read get_CacheMode write put_CacheMode;
    property Clip: IRectangleGeometry read get_Clip write put_Clip;
    property DesiredSize: TSizeF read get_DesiredSize;
    property IsDoubleTapEnabled: Boolean read get_IsDoubleTapEnabled write put_IsDoubleTapEnabled;
    property IsHitTestVisible: Boolean read get_IsHitTestVisible write put_IsHitTestVisible;
    property IsHoldingEnabled: Boolean read get_IsHoldingEnabled write put_IsHoldingEnabled;
    property IsRightTapEnabled: Boolean read get_IsRightTapEnabled write put_IsRightTapEnabled;
    property IsTapEnabled: Boolean read get_IsTapEnabled write put_IsTapEnabled;
    property ManipulationMode: Input_ManipulationModes read get_ManipulationMode write put_ManipulationMode;
    property Opacity: Double read get_Opacity write put_Opacity;
    property PointerCaptures: IVectorView_1__Input_IPointer read get_PointerCaptures;
    property Projection: IProjection read get_Projection write put_Projection;
    property RenderSize: TSizeF read get_RenderSize;
    property RenderTransform: ITransform read get_RenderTransform write put_RenderTransform;
    property RenderTransformOrigin: TPointF read get_RenderTransformOrigin write put_RenderTransformOrigin;
    property Transitions: IVector_1__Animation_ITransition read get_Transitions write put_Transitions;
    property UseLayoutRounding: Boolean read get_UseLayoutRounding write put_UseLayoutRounding;
    property Visibility_: Visibility read get_Visibility write put_Visibility;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.IRectangleGeometry
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RectangleGeometry)]
  IRectangleGeometry = interface(IInspectable)
  ['{A25A1F58-C575-4196-91CF-9FDFB10445C3}']
    function get_Rect: TRectF; safecall;
    procedure put_Rect(value: TRectF); safecall;
    property Rect: TRectF read get_Rect write put_Rect;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.ITransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Transform)]
  ITransform = interface(IInspectable)
  ['{4DF74078-BFD6-4ED1-9682-D2FD8BF2FE6F}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.IProjection
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Projection)]
  IProjection = interface(IInspectable)
  ['{B3443557-7F39-4D04-A89C-844338CAC897}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IVector_1__Animation_ITransition_Base = interface(IInspectable)
  ['{E798571F-7E3E-5E1A-AA55-CBC93B83F821}']
    function GetAt(index: Cardinal): Animation_ITransition; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Animation_ITransition; safecall;
    function IndexOf(value: Animation_ITransition; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Animation_ITransition); safecall;
    procedure InsertAt(index: Cardinal; value: Animation_ITransition); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Animation_ITransition); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_ITransition): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PAnimation_ITransition); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITransition>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_TransitionCollection)]
  IVector_1__Animation_ITransition = interface(IVector_1__Animation_ITransition_Base)
  ['{7F1E9E51-5413-5039-8B37-34D9B8E8C125}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.Animation.ITransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_Transition)]
  Animation_ITransition = interface(IInspectable)
  ['{3C677C7C-01D0-4DCE-B333-976F93312B08}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.ITransition>
  // External 
  IVectorView_1__Animation_ITransition = interface(IInspectable)
  ['{B072CE78-3433-54DD-BD01-A738B73CAD2B}']
    function GetAt(index: Cardinal): Animation_ITransition; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Animation_ITransition; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_ITransition): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.ICacheMode
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_CacheMode)]
  ICacheMode = interface(IInspectable)
  ['{98DC8B11-C6F9-4DAB-B838-5FD5EC8C7350}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Input.IPointer>
  // External 
  IVectorView_1__Input_IPointer = interface(IInspectable)
  ['{879A97D2-041F-5B03-A17D-213B7030EDED}']
    function GetAt(index: Cardinal): Input_IPointer; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IPointer; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IPointer): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Xaml.Input.IPointer
  // External 
  Input_IPointer = interface(IInspectable)
  ['{5EE8F39F-747D-4171-90E6-CD37A9DFFB11}']
    function get_PointerId: Cardinal; safecall;
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_IsInContact: Boolean; safecall;
    function get_IsInRange: Boolean; safecall;
    property IsInContact: Boolean read get_IsInContact;
    property IsInRange: Boolean read get_IsInRange;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
    property PointerId: Cardinal read get_PointerId;
  end;

  // Windows.UI.Xaml.Input.KeyEventHandler
  // External 
  Input_KeyEventHandler = interface(IUnknown)
  ['{7C63D2E5-7A0E-4E12-B96A-7715AA6FF1C8}']
    procedure Invoke(sender: IInspectable; e: Input_IKeyRoutedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Input.IKeyRoutedEventArgs
  // External 
  Input_IKeyRoutedEventArgs = interface(IInspectable)
  ['{D4CD3DFE-4079-42E9-A39A-3095D3F049C6}']
    function get_Key: VirtualKey; safecall;
    function get_KeyStatus: CorePhysicalKeyStatus; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property Key: VirtualKey read get_Key;
    property KeyStatus: CorePhysicalKeyStatus read get_KeyStatus;
  end;

  // Windows.UI.Xaml.DragEventHandler
  // External 
  DragEventHandler = interface(IUnknown)
  ['{2AB1A205-1E73-4BCF-AABC-57B97E21961D}']
    procedure Invoke(sender: IInspectable; e: IDragEventArgs); safecall;
  end;

  // Windows.UI.Xaml.IDragEventArgs
  // External 
  IDragEventArgs = interface(IInspectable)
  ['{B440C7C3-02B4-4980-9342-25DAE1C0F188}']
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function get_Data: IDataPackage; safecall;
    procedure put_Data(value: IDataPackage); safecall;
    function GetPosition(relativeTo: IUIElement): TPointF; safecall;
    property Data: IDataPackage read get_Data write put_Data;
    property Handled: Boolean read get_Handled write put_Handled;
  end;

  // Windows.UI.Xaml.Input.PointerEventHandler
  // External 
  Input_PointerEventHandler = interface(IUnknown)
  ['{E4385929-C004-4BCF-8970-359486E39F88}']
    procedure Invoke(sender: IInspectable; e: Input_IPointerRoutedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Input.IPointerRoutedEventArgs
  // External 
  Input_IPointerRoutedEventArgs = interface(IInspectable)
  ['{DA628F0A-9752-49E2-BDE2-49ECCAB9194D}']
    function get_Pointer: Input_IPointer; safecall;
    function get_KeyModifiers: VirtualKeyModifiers; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function GetCurrentPoint(relativeTo: IUIElement): IPointerPoint; safecall;
    function GetIntermediatePoints(relativeTo: IUIElement): IVector_1__IPointerPoint; safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property KeyModifiers: VirtualKeyModifiers read get_KeyModifiers;
    property Pointer: Input_IPointer read get_Pointer;
  end;

  // Windows.UI.Xaml.Input.TappedEventHandler
  // External 
  Input_TappedEventHandler = interface(IUnknown)
  ['{68D940CC-9FF0-49CE-B141-3F07EC477B97}']
    procedure Invoke(sender: IInspectable; e: Input_ITappedRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.ITappedRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_TappedRoutedEventArgs)]
  Input_ITappedRoutedEventArgs = interface(IInspectable)
  ['{A099E6BE-E624-459A-BB1D-E05C73E2CC66}']
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function GetPosition(relativeTo: IUIElement): TPointF; safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
  end;

  // Windows.UI.Xaml.Input.DoubleTappedEventHandler
  // External 
  Input_DoubleTappedEventHandler = interface(IUnknown)
  ['{3124D025-04A7-4D45-825E-8204A624DBF4}']
    procedure Invoke(sender: IInspectable; e: Input_IDoubleTappedRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IDoubleTappedRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_DoubleTappedRoutedEventArgs)]
  Input_IDoubleTappedRoutedEventArgs = interface(IInspectable)
  ['{AF404424-26DF-44F4-8714-9359249B62D3}']
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function GetPosition(relativeTo: IUIElement): TPointF; safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
  end;

  // Windows.UI.Xaml.Input.HoldingEventHandler
  // External 
  Input_HoldingEventHandler = interface(IUnknown)
  ['{ECAE8CCD-8E5E-4FBE-9846-30A6370AFCDF}']
    procedure Invoke(sender: IInspectable; e: Input_IHoldingRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IHoldingRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_HoldingRoutedEventArgs)]
  Input_IHoldingRoutedEventArgs = interface(IInspectable)
  ['{C246FF23-D80D-44DE-8DB9-0D815E269AC0}']
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_HoldingState: HoldingState; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function GetPosition(relativeTo: IUIElement): TPointF; safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property HoldingState_: HoldingState read get_HoldingState;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
  end;

  // Windows.UI.Xaml.Input.RightTappedEventHandler
  // External 
  Input_RightTappedEventHandler = interface(IUnknown)
  ['{2532A062-F447-4950-9C46-F1E34A2C2238}']
    procedure Invoke(sender: IInspectable; e: Input_IRightTappedRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IRightTappedRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_RightTappedRoutedEventArgs)]
  Input_IRightTappedRoutedEventArgs = interface(IInspectable)
  ['{6834869D-7BD5-4033-B237-172F79ABE393}']
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function GetPosition(relativeTo: IUIElement): TPointF; safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
  end;

  // Windows.UI.Xaml.Input.ManipulationStartingEventHandler
  // External 
  Input_ManipulationStartingEventHandler = interface(IUnknown)
  ['{10D0B04E-BFE4-42CB-823C-3FECD8770EF8}']
    procedure Invoke(sender: IInspectable; e: Input_IManipulationStartingRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IManipulationStartingRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_ManipulationStartingRoutedEventArgs)]
  Input_IManipulationStartingRoutedEventArgs = interface(IInspectable)
  ['{18D636B7-53A4-4C15-A498-F3A9CA212A42}']
    function get_Mode: Input_ManipulationModes; safecall;
    procedure put_Mode(value: Input_ManipulationModes); safecall;
    function get_Container: IUIElement; safecall;
    procedure put_Container(value: IUIElement); safecall;
    function get_Pivot: Input_IManipulationPivot; safecall;
    procedure put_Pivot(value: Input_IManipulationPivot); safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    property Container: IUIElement read get_Container write put_Container;
    property Handled: Boolean read get_Handled write put_Handled;
    property Mode: Input_ManipulationModes read get_Mode write put_Mode;
    property Pivot: Input_IManipulationPivot read get_Pivot write put_Pivot;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IManipulationPivot
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_ManipulationPivot)]
  Input_IManipulationPivot = interface(IInspectable)
  ['{2E3838A5-E6C2-4998-82AC-18748B141666}']
    function get_Center: TPointF; safecall;
    procedure put_Center(value: TPointF); safecall;
    function get_Radius: Double; safecall;
    procedure put_Radius(value: Double); safecall;
    property Center: TPointF read get_Center write put_Center;
    property Radius: Double read get_Radius write put_Radius;
  end;

  // Windows.UI.Xaml.Input.ManipulationInertiaStartingEventHandler
  // External 
  Input_ManipulationInertiaStartingEventHandler = interface(IUnknown)
  ['{D39D6322-7C9C-481B-827B-C8B2D9BB6FC7}']
    procedure Invoke(sender: IInspectable; e: Input_IManipulationInertiaStartingRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IManipulationInertiaStartingRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_ManipulationInertiaStartingRoutedEventArgs)]
  Input_IManipulationInertiaStartingRoutedEventArgs = interface(IInspectable)
  ['{246A91A9-CA43-4C0B-ACEF-81E8B8147520}']
    function get_Container: IUIElement; safecall;
    function get_ExpansionBehavior: Input_IInertiaExpansionBehavior; safecall;
    procedure put_ExpansionBehavior(value: Input_IInertiaExpansionBehavior); safecall;
    function get_RotationBehavior: Input_IInertiaRotationBehavior; safecall;
    procedure put_RotationBehavior(value: Input_IInertiaRotationBehavior); safecall;
    function get_TranslationBehavior: Input_IInertiaTranslationBehavior; safecall;
    procedure put_TranslationBehavior(value: Input_IInertiaTranslationBehavior); safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_Delta: ManipulationDelta; safecall;
    function get_Cumulative: ManipulationDelta; safecall;
    function get_Velocities: ManipulationVelocities; safecall;
    property Container: IUIElement read get_Container;
    property Cumulative: ManipulationDelta read get_Cumulative;
    property Delta: ManipulationDelta read get_Delta;
    property ExpansionBehavior: Input_IInertiaExpansionBehavior read get_ExpansionBehavior write put_ExpansionBehavior;
    property Handled: Boolean read get_Handled write put_Handled;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
    property RotationBehavior: Input_IInertiaRotationBehavior read get_RotationBehavior write put_RotationBehavior;
    property TranslationBehavior: Input_IInertiaTranslationBehavior read get_TranslationBehavior write put_TranslationBehavior;
    property Velocities: ManipulationVelocities read get_Velocities;
  end;

  // Windows.UI.Xaml.Input.IInertiaExpansionBehavior
  // External 
  Input_IInertiaExpansionBehavior = interface(IInspectable)
  ['{751D87E5-8D42-44C5-965E-3CD30CC9D6F7}']
    function get_DesiredDeceleration: Double; safecall;
    procedure put_DesiredDeceleration(value: Double); safecall;
    function get_DesiredExpansion: Double; safecall;
    procedure put_DesiredExpansion(value: Double); safecall;
    property DesiredDeceleration: Double read get_DesiredDeceleration write put_DesiredDeceleration;
    property DesiredExpansion: Double read get_DesiredExpansion write put_DesiredExpansion;
  end;

  // Windows.UI.Xaml.Input.IInertiaRotationBehavior
  // External 
  Input_IInertiaRotationBehavior = interface(IInspectable)
  ['{424CFB2E-BBFD-4625-AE78-20C65BF1EFAF}']
    function get_DesiredDeceleration: Double; safecall;
    procedure put_DesiredDeceleration(value: Double); safecall;
    function get_DesiredRotation: Double; safecall;
    procedure put_DesiredRotation(value: Double); safecall;
    property DesiredDeceleration: Double read get_DesiredDeceleration write put_DesiredDeceleration;
    property DesiredRotation: Double read get_DesiredRotation write put_DesiredRotation;
  end;

  // Windows.UI.Xaml.Input.IInertiaTranslationBehavior
  // External 
  Input_IInertiaTranslationBehavior = interface(IInspectable)
  ['{45D3A512-3B32-4882-A4C2-ECFA2D4B6DF0}']
    function get_DesiredDeceleration: Double; safecall;
    procedure put_DesiredDeceleration(value: Double); safecall;
    function get_DesiredDisplacement: Double; safecall;
    procedure put_DesiredDisplacement(value: Double); safecall;
    property DesiredDeceleration: Double read get_DesiredDeceleration write put_DesiredDeceleration;
    property DesiredDisplacement: Double read get_DesiredDisplacement write put_DesiredDisplacement;
  end;

  // Windows.UI.Xaml.Input.ManipulationStartedEventHandler
  // External 
  Input_ManipulationStartedEventHandler = interface(IUnknown)
  ['{F88345F8-E0A3-4BE2-B90C-DC20E6D8BEB0}']
    procedure Invoke(sender: IInspectable; e: Input_IManipulationStartedRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IManipulationStartedRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_ManipulationStartedRoutedEventArgs)]
  Input_IManipulationStartedRoutedEventArgs = interface(IInspectable)
  ['{5DB1AA05-9F80-48B6-AE6C-4F119DE8FF13}']
    function get_Container: IUIElement; safecall;
    function get_Position: TPointF; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    function get_Cumulative: ManipulationDelta; safecall;
    procedure Complete; safecall;
    property Container: IUIElement read get_Container;
    property Cumulative: ManipulationDelta read get_Cumulative;
    property Handled: Boolean read get_Handled write put_Handled;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
    property Position: TPointF read get_Position;
  end;

  // Windows.UI.Xaml.Input.ManipulationDeltaEventHandler
  // External 
  Input_ManipulationDeltaEventHandler = interface(IUnknown)
  ['{AA1160CB-DFB9-4C56-ABDC-711B63C8EB94}']
    procedure Invoke(sender: IInspectable; e: Input_IManipulationDeltaRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IManipulationDeltaRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_ManipulationDeltaRoutedEventArgs)]
  Input_IManipulationDeltaRoutedEventArgs = interface(IInspectable)
  ['{400D5794-4C6F-491D-82D6-3517109399C6}']
    function get_Container: IUIElement; safecall;
    function get_Position: TPointF; safecall;
    function get_IsInertial: Boolean; safecall;
    function get_Delta: ManipulationDelta; safecall;
    function get_Cumulative: ManipulationDelta; safecall;
    function get_Velocities: ManipulationVelocities; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    procedure Complete; safecall;
    property Container: IUIElement read get_Container;
    property Cumulative: ManipulationDelta read get_Cumulative;
    property Delta: ManipulationDelta read get_Delta;
    property Handled: Boolean read get_Handled write put_Handled;
    property IsInertial: Boolean read get_IsInertial;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
    property Position: TPointF read get_Position;
    property Velocities: ManipulationVelocities read get_Velocities;
  end;

  // Windows.UI.Xaml.Input.ManipulationCompletedEventHandler
  // External 
  Input_ManipulationCompletedEventHandler = interface(IUnknown)
  ['{38EF4B0F-14F8-42DF-9A1E-A4BCC4AF77F4}']
    procedure Invoke(sender: IInspectable; e: Input_IManipulationCompletedRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Input.IManipulationCompletedRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Input_ManipulationCompletedRoutedEventArgs)]
  Input_IManipulationCompletedRoutedEventArgs = interface(IInspectable)
  ['{B5AD9B23-2F41-498E-8319-015EE8A75346}']
    function get_Container: IUIElement; safecall;
    function get_Position: TPointF; safecall;
    function get_IsInertial: Boolean; safecall;
    function get_Cumulative: ManipulationDelta; safecall;
    function get_Velocities: ManipulationVelocities; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function get_PointerDeviceType: Input_PointerDeviceType; safecall;
    property Container: IUIElement read get_Container;
    property Cumulative: ManipulationDelta read get_Cumulative;
    property Handled: Boolean read get_Handled write put_Handled;
    property IsInertial: Boolean read get_IsInertial;
    property PointerDeviceType: Input_PointerDeviceType read get_PointerDeviceType;
    property Position: TPointF read get_Position;
    property Velocities: ManipulationVelocities read get_Velocities;
  end;

  // Windows.UI.Xaml.IRoutedEvent
  // External 
  IRoutedEvent = interface(IInspectable)
  ['{A6B25818-43C1-4C70-865C-7BDD5A32E327}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.IGeneralTransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GeneralTransform)]
  IGeneralTransform = interface(IInspectable)
  ['{A06798B7-A2EC-415F-ADE2-EADE9333F2C7}']
    function get_Inverse: IGeneralTransform; safecall;
    function TransformPoint(point: TPointF): TPointF; safecall;
    function TryTransform(inPoint: TPointF; out outPoint: TPointF): Boolean; safecall;
    function TransformBounds(rect: TRectF): TRectF; safecall;
    property Inverse: IGeneralTransform read get_Inverse;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IUIElement>
  IVector_1__IUIElement_Base = interface(IInspectable)
  ['{B4C1E3AC-8768-5B9D-A661-F63330B8507B}']
    function GetAt(index: Cardinal): IUIElement; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IUIElement; safecall;
    function IndexOf(value: IUIElement; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IUIElement); safecall;
    procedure InsertAt(index: Cardinal; value: IUIElement); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IUIElement); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIUIElement): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIUIElement); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IUIElement>
  // External 
  IVector_1__IUIElement = interface(IVector_1__IUIElement_Base)
  ['{8C8AC2C2-21BB-52E2-BFE0-0A7F96B3841F}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.IUIElement>
  // External 
  IVectorView_1__IUIElement = interface(IInspectable)
  ['{8FF941EA-49F4-5F0A-A951-F83F03D3B29B}']
    function GetAt(index: Cardinal): IUIElement; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IUIElement; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIUIElement): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IBrushTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_BrushTransition)]
  IBrushTransition = interface(IInspectable)
  ['{1116972C-9DAD-5429-A7DD-B2B7D061AB8E}']
    function get_Duration: TimeSpan; safecall;
    procedure put_Duration(value: TimeSpan); safecall;
    property Duration: TimeSpan read get_Duration write put_Duration;
  end;

  // Windows.UI.Xaml.DependencyPropertyChangedEventHandler
  // External 
  DependencyPropertyChangedEventHandler = interface(IUnknown)
  ['{09223E5A-75BE-4499-8180-1DDC005421C0}']
    procedure Invoke(sender: IInspectable; e: IDependencyPropertyChangedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.IDependencyPropertyChangedEventArgs
  // External 
  IDependencyPropertyChangedEventArgs = interface(IInspectable)
  ['{81212C2B-24D0-4957-ABC3-224470A93A4E}']
    function get_Property: IDependencyProperty; safecall;
    function get_OldValue: IInspectable; safecall;
    function get_NewValue: IInspectable; safecall;
    property NewValue: IInspectable read get_NewValue;
    property OldValue: IInspectable read get_OldValue;
    property &Property: IDependencyProperty read get_Property;
  end;

  // Windows.UI.Xaml.Input.ICharacterReceivedRoutedEventArgs
  // External 
  Input_ICharacterReceivedRoutedEventArgs = interface(IInspectable)
  ['{7849FD82-48E4-444D-9419-93AB8892C107}']
    function get_Character: Char; safecall;
    function get_KeyStatus: CorePhysicalKeyStatus; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    property Character: Char read get_Character;
    property Handled: Boolean read get_Handled write put_Handled;
    property KeyStatus: CorePhysicalKeyStatus read get_KeyStatus;
  end;

  // Windows.UI.Xaml.Navigation.NavigatedEventHandler
  // External 
  Navigation_NavigatedEventHandler = interface(IUnknown)
  ['{7BD1CF54-23CF-4CCE-B2F5-4CE78D96896E}']
    procedure Invoke(sender: IInspectable; e: Navigation_INavigationEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Navigation.INavigationEventArgs
  // External 
  Navigation_INavigationEventArgs = interface(IInspectable)
  ['{B6AA9834-6691-44D1-BDF7-58820C27B0D0}']
    function get_Content: IInspectable; safecall;
    function get_Parameter: IInspectable; safecall;
    function get_SourcePageType: Interop_TypeName; safecall;
    function get_NavigationMode: Navigation_NavigationMode; safecall;
    function get_Uri: IUriRuntimeClass; safecall;
    procedure put_Uri(value: IUriRuntimeClass); safecall;
    property Content: IInspectable read get_Content;
    property NavigationMode: Navigation_NavigationMode read get_NavigationMode;
    property Parameter: IInspectable read get_Parameter;
    property SourcePageType: Interop_TypeName read get_SourcePageType;
    property Uri: IUriRuntimeClass read get_Uri write put_Uri;
  end;

  // Windows.UI.Xaml.Navigation.NavigatingCancelEventHandler
  // External 
  Navigation_NavigatingCancelEventHandler = interface(IUnknown)
  ['{75D6A78F-A302-4489-9898-24EA49182910}']
    procedure Invoke(sender: IInspectable; e: Navigation_INavigatingCancelEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Navigation.INavigatingCancelEventArgs
  // External 
  Navigation_INavigatingCancelEventArgs = interface(IInspectable)
  ['{FD1D67AE-EAFB-4079-BE80-6DC92A03AEDF}']
    function get_Cancel: Boolean; safecall;
    procedure put_Cancel(value: Boolean); safecall;
    function get_NavigationMode: Navigation_NavigationMode; safecall;
    function get_SourcePageType: Interop_TypeName; safecall;
    property Cancel: Boolean read get_Cancel write put_Cancel;
    property NavigationMode: Navigation_NavigationMode read get_NavigationMode;
    property SourcePageType: Interop_TypeName read get_SourcePageType;
  end;

  // Windows.UI.Xaml.Navigation.NavigationFailedEventHandler
  // External 
  Navigation_NavigationFailedEventHandler = interface(IUnknown)
  ['{4DAB4671-12B2-43C7-B892-9BE2DCD3E88D}']
    procedure Invoke(sender: IInspectable; e: Navigation_INavigationFailedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Navigation.INavigationFailedEventArgs
  // External 
  Navigation_INavigationFailedEventArgs = interface(IInspectable)
  ['{11C1DFF7-36C2-4102-B2EF-0217A97289B3}']
    function get_Exception: HRESULT; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function get_SourcePageType: Interop_TypeName; safecall;
    property Exception: HRESULT read get_Exception;
    property Handled: Boolean read get_Handled write put_Handled;
    property SourcePageType: Interop_TypeName read get_SourcePageType;
  end;

  // Windows.UI.Xaml.Navigation.NavigationStoppedEventHandler
  // External 
  Navigation_NavigationStoppedEventHandler = interface(IUnknown)
  ['{F0117DDB-12FA-4D8D-8B26-B383D09C2B3C}']
    procedure Invoke(sender: IInspectable; e: Navigation_INavigationEventArgs); safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Navigation.IPageStackEntry>
  IVector_1__Navigation_IPageStackEntry_Base = interface(IInspectable)
  ['{E561A13F-89A1-5EF2-A3FE-EBA1B4539B46}']
    function GetAt(index: Cardinal): Navigation_IPageStackEntry; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Navigation_IPageStackEntry; safecall;
    function IndexOf(value: Navigation_IPageStackEntry; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Navigation_IPageStackEntry); safecall;
    procedure InsertAt(index: Cardinal; value: Navigation_IPageStackEntry); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Navigation_IPageStackEntry); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PNavigation_IPageStackEntry): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PNavigation_IPageStackEntry); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Navigation.IPageStackEntry>
  // External 
  IVector_1__Navigation_IPageStackEntry = interface(IVector_1__Navigation_IPageStackEntry_Base)
  ['{12138C7C-8700-5EC2-B68F-51BD59B08BEC}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Navigation.IPageStackEntry
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Navigation_PageStackEntry)]
  Navigation_IPageStackEntry = interface(IInspectable)
  ['{EF8814A6-9388-4ACA-8572-405194069080}']
    function get_SourcePageType: Interop_TypeName; safecall;
    function get_Parameter: IInspectable; safecall;
    function get_NavigationTransitionInfo: Animation_INavigationTransitionInfo; safecall;
    property NavigationTransitionInfo: Animation_INavigationTransitionInfo read get_NavigationTransitionInfo;
    property Parameter: IInspectable read get_Parameter;
    property SourcePageType: Interop_TypeName read get_SourcePageType;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.Animation.INavigationTransitionInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_NavigationTransitionInfo)]
  Animation_INavigationTransitionInfo = interface(IInspectable)
  ['{A9B05091-AE4A-4372-8625-21B7A8B98CA4}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Navigation.IPageStackEntry>
  // External 
  IVectorView_1__Navigation_IPageStackEntry = interface(IInspectable)
  ['{D34746AC-D201-5C5F-A7AF-5B04B3CC1A6E}']
    function GetAt(index: Cardinal): Navigation_IPageStackEntry; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Navigation_IPageStackEntry; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PNavigation_IPageStackEntry): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Navigation.IFrameNavigationOptions
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Navigation_FrameNavigationOptions)]
  Navigation_IFrameNavigationOptions = interface(IInspectable)
  ['{B539AD2A-9FB7-520A-8F41-57A50C59CF92}']
    function get_IsNavigationStackEnabled: Boolean; safecall;
    procedure put_IsNavigationStackEnabled(value: Boolean); safecall;
    function get_TransitionInfoOverride: Animation_INavigationTransitionInfo; safecall;
    procedure put_TransitionInfoOverride(value: Animation_INavigationTransitionInfo); safecall;
    property IsNavigationStackEnabled: Boolean read get_IsNavigationStackEnabled write put_IsNavigationStackEnabled;
    property TransitionInfoOverride: Animation_INavigationTransitionInfo read get_TransitionInfoOverride write put_TransitionInfoOverride;
  end;

  // DualAPI Interface
  // Windows.Foundation.Collections.IObservableVector`1<Windows.UI.Xaml.IDependencyObject>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_DependencyObjectCollection)]
  IObservableVector_1__IDependencyObject = interface(IInspectable)
  ['{1BBADE0C-F703-5AC5-ACDE-448DE801D127}']
    function add_VectorChanged(vhnd: VectorChangedEventHandler_1__IDependencyObject): EventRegistrationToken; safecall;
    procedure remove_VectorChanged(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.UI.Xaml.IDependencyObject>
  VectorChangedEventHandler_1__IDependencyObject_Delegate_Base = interface(IUnknown)
  ['{B62DEC93-A7A5-5FF5-B2D2-6BD20CA0BD4D}']
    procedure Invoke(sender: IObservableVector_1__IDependencyObject; event: IVectorChangedEventArgs); safecall;
  end;
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.UI.Xaml.IDependencyObject>
  // External 
  VectorChangedEventHandler_1__IDependencyObject = interface(VectorChangedEventHandler_1__IDependencyObject_Delegate_Base)
  ['{788D252F-3474-59E3-BFD5-5E7D68D9C605}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IInkToolbar,Object>
  TypedEventHandler_2__IInkToolbar__IInspectable_Delegate_Base = interface(IUnknown)
  ['{EEA8B7BE-9CBA-5129-9B75-4930DC0C2705}']
    procedure Invoke(sender: IInkToolbar; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IInkToolbar,Object>
  // External 
  TypedEventHandler_2__IInkToolbar__IInspectable = interface(TypedEventHandler_2__IInkToolbar__IInspectable_Delegate_Base)
  ['{EB3F0795-E556-5964-A4D0-FCE945CC9A26}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IInkToolbar
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_InkToolbar)]
  IInkToolbar = interface(IInspectable)
  ['{3DDD0CCA-51F0-486F-A03E-4EE13DC12BCB}']
    function get_InitialControls: InkToolbarInitialControls; safecall;
    procedure put_InitialControls(value: InkToolbarInitialControls); safecall;
    function get_Children: IObservableVector_1__IDependencyObject; safecall;
    function get_ActiveTool: IInkToolbarToolButton; safecall;
    procedure put_ActiveTool(value: IInkToolbarToolButton); safecall;
    function get_InkDrawingAttributes: IInkDrawingAttributes; safecall;
    function get_IsRulerButtonChecked: Boolean; safecall;
    procedure put_IsRulerButtonChecked(value: Boolean); safecall;
    function get_TargetInkCanvas: IInkCanvas; safecall;
    procedure put_TargetInkCanvas(value: IInkCanvas); safecall;
    function add_ActiveToolChanged(handler: TypedEventHandler_2__IInkToolbar__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ActiveToolChanged(token: EventRegistrationToken); safecall;
    function add_InkDrawingAttributesChanged(handler: TypedEventHandler_2__IInkToolbar__IInspectable): EventRegistrationToken; safecall;
    procedure remove_InkDrawingAttributesChanged(token: EventRegistrationToken); safecall;
    function add_EraseAllClicked(handler: TypedEventHandler_2__IInkToolbar__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EraseAllClicked(token: EventRegistrationToken); safecall;
    function add_IsRulerButtonCheckedChanged(handler: TypedEventHandler_2__IInkToolbar__IInspectable): EventRegistrationToken; safecall;
    procedure remove_IsRulerButtonCheckedChanged(token: EventRegistrationToken); safecall;
    function GetToolButton(tool: InkToolbarTool): IInkToolbarToolButton; safecall;
    function GetToggleButton(tool: InkToolbarToggle): IInkToolbarToggleButton; safecall;
    property ActiveTool: IInkToolbarToolButton read get_ActiveTool write put_ActiveTool;
    property Children: IObservableVector_1__IDependencyObject read get_Children;
    property InitialControls: InkToolbarInitialControls read get_InitialControls write put_InitialControls;
    property InkDrawingAttributes: IInkDrawingAttributes read get_InkDrawingAttributes;
    property IsRulerButtonChecked: Boolean read get_IsRulerButtonChecked write put_IsRulerButtonChecked;
    property TargetInkCanvas: IInkCanvas read get_TargetInkCanvas write put_TargetInkCanvas;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IInkToolbarToolButton
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_InkToolbarToolButton)]
  IInkToolbarToolButton = interface(IInspectable)
  ['{5C5AF41E-CCB7-4458-8064-A9849D31561B}']
    function get_ToolKind: InkToolbarTool; safecall;
    function get_IsExtensionGlyphShown: Boolean; safecall;
    procedure put_IsExtensionGlyphShown(value: Boolean); safecall;
    property IsExtensionGlyphShown: Boolean read get_IsExtensionGlyphShown write put_IsExtensionGlyphShown;
    property ToolKind: InkToolbarTool read get_ToolKind;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IInkCanvas
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_InkCanvas)]
  IInkCanvas = interface(IInspectable)
  ['{29443C28-8E48-4FC8-A473-35B0BA12ACEA}']
    function get_InkPresenter: IInkPresenter; safecall;
    property InkPresenter: IInkPresenter read get_InkPresenter;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IInkToolbarToggleButton
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_InkToolbarToggleButton)]
  IInkToolbarToggleButton = interface(IInspectable)
  ['{B4A278FA-F5F0-4B1F-BEB0-0B8A29905A4A}']
    function get_ToggleKind: InkToolbarToggle; safecall;
    property ToggleKind: InkToolbarToggle read get_ToggleKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IInkToolbarFlyoutItem,Object>
  TypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable_Delegate_Base = interface(IUnknown)
  ['{127A421C-AE3F-53DF-B9AD-176847F32CDE}']
    procedure Invoke(sender: IInkToolbarFlyoutItem; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IInkToolbarFlyoutItem,Object>
  // External 
  TypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable = interface(TypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable_Delegate_Base)
  ['{0E799DCB-D2A3-5345-A425-C2389C94DFFF}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IInkToolbarFlyoutItem
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_InkToolbarFlyoutItem)]
  IInkToolbarFlyoutItem = interface(IInspectable)
  ['{92B68786-37EE-4915-9E89-E187564A889A}']
    function get_Kind: InkToolbarFlyoutItemKind; safecall;
    procedure put_Kind(value: InkToolbarFlyoutItemKind); safecall;
    function get_IsChecked: Boolean; safecall;
    procedure put_IsChecked(value: Boolean); safecall;
    function add_Checked(handler: TypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Checked(token: EventRegistrationToken); safecall;
    function add_Unchecked(handler: TypedEventHandler_2__IInkToolbarFlyoutItem__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Unchecked(token: EventRegistrationToken); safecall;
    property IsChecked: Boolean read get_IsChecked write put_IsChecked;
    property Kind: InkToolbarFlyoutItemKind read get_Kind write put_Kind;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Object>>
  IAsyncOperation_1__IVectorView_1__IInspectable_Base = interface(IInspectable)
  ['{D671D332-22AA-5597-8DCC-2459EAB49418}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable; safecall;
    function GetResults: IVectorView_1__IInspectable; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Object>>
  // External 
  IAsyncOperation_1__IVectorView_1__IInspectable = interface(IAsyncOperation_1__IVectorView_1__IInspectable_Base)
  ['{D671D332-22AA-5597-8DCC-2459EAB49418}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Object>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable_Delegate_Base = interface(IUnknown)
  ['{261A9D81-F58F-5283-9461-CA3E31C1123C}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IInspectable; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Object>>
  // External 
  AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IInspectable_Delegate_Base)
  ['{261A9D81-F58F-5283-9461-CA3E31C1123C}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Data.IItemIndexRange>
  // External 
  IVectorView_1__Data_IItemIndexRange = interface(IInspectable)
  ['{01AA146D-06B8-5B7B-BF72-DB079CA98B45}']
    function GetAt(index: Cardinal): Data_IItemIndexRange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Data_IItemIndexRange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PData_IItemIndexRange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Data.IItemIndexRange
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Data_ItemIndexRange)]
  Data_IItemIndexRange = interface(IInspectable)
  ['{83B834BE-0583-4A26-9B64-8BF4A2F65704}']
    function get_FirstIndex: Integer; safecall;
    function get_Length: Cardinal; safecall;
    function get_LastIndex: Integer; safecall;
    property FirstIndex: Integer read get_FirstIndex;
    property LastIndex: Integer read get_LastIndex;
    property Length: Cardinal read get_Length;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Object>
  // External 
  TypedEventHandler_2__INavigationView__IInspectable = interface(IUnknown)
  ['{BAAD3795-61D2-54CC-96BA-C475B39F685C}']
    procedure Invoke(sender: INavigationView; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.INavigationView
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_NavigationView)]
  INavigationView = interface(IInspectable)
  ['{F209CE15-391A-42CA-9FC6-F79DA65ACA32}']
    function get_IsPaneOpen: Boolean; safecall;
    procedure put_IsPaneOpen(value: Boolean); safecall;
    function get_CompactModeThresholdWidth: Double; safecall;
    procedure put_CompactModeThresholdWidth(value: Double); safecall;
    function get_ExpandedModeThresholdWidth: Double; safecall;
    procedure put_ExpandedModeThresholdWidth(value: Double); safecall;
    function get_PaneFooter: IUIElement; safecall;
    procedure put_PaneFooter(value: IUIElement); safecall;
    function get_Header: IInspectable; safecall;
    procedure put_Header(value: IInspectable); safecall;
    function get_HeaderTemplate: IDataTemplate; safecall;
    procedure put_HeaderTemplate(value: IDataTemplate); safecall;
    function get_DisplayMode: NavigationViewDisplayMode; safecall;
    function get_IsSettingsVisible: Boolean; safecall;
    procedure put_IsSettingsVisible(value: Boolean); safecall;
    function get_IsPaneToggleButtonVisible: Boolean; safecall;
    procedure put_IsPaneToggleButtonVisible(value: Boolean); safecall;
    function get_AlwaysShowHeader: Boolean; safecall;
    procedure put_AlwaysShowHeader(value: Boolean); safecall;
    function get_CompactPaneLength: Double; safecall;
    procedure put_CompactPaneLength(value: Double); safecall;
    function get_OpenPaneLength: Double; safecall;
    procedure put_OpenPaneLength(value: Double); safecall;
    function get_PaneToggleButtonStyle: IStyle; safecall;
    procedure put_PaneToggleButtonStyle(value: IStyle); safecall;
    function get_SelectedItem: IInspectable; safecall;
    procedure put_SelectedItem(value: IInspectable); safecall;
    function get_MenuItems: IVector_1__IInspectable; safecall;
    function get_MenuItemsSource: IInspectable; safecall;
    procedure put_MenuItemsSource(value: IInspectable); safecall;
    function get_SettingsItem: IInspectable; safecall;
    function get_AutoSuggestBox: IAutoSuggestBox; safecall;
    procedure put_AutoSuggestBox(value: IAutoSuggestBox); safecall;
    function get_MenuItemTemplate: IDataTemplate; safecall;
    procedure put_MenuItemTemplate(value: IDataTemplate); safecall;
    function get_MenuItemTemplateSelector: IDataTemplateSelector; safecall;
    procedure put_MenuItemTemplateSelector(value: IDataTemplateSelector); safecall;
    function get_MenuItemContainerStyle: IStyle; safecall;
    procedure put_MenuItemContainerStyle(value: IStyle); safecall;
    function get_MenuItemContainerStyleSelector: IStyleSelector; safecall;
    procedure put_MenuItemContainerStyleSelector(value: IStyleSelector); safecall;
    function MenuItemFromContainer(container: IDependencyObject): IInspectable; safecall;
    function ContainerFromMenuItem(item: IInspectable): IDependencyObject; safecall;
    function add_SelectionChanged(handler: TypedEventHandler_2__INavigationView__INavigationViewSelectionChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_SelectionChanged(token: EventRegistrationToken); safecall;
    function add_ItemInvoked(handler: TypedEventHandler_2__INavigationView__INavigationViewItemInvokedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ItemInvoked(token: EventRegistrationToken); safecall;
    function add_DisplayModeChanged(handler: TypedEventHandler_2__INavigationView__INavigationViewDisplayModeChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_DisplayModeChanged(token: EventRegistrationToken); safecall;
    property AlwaysShowHeader: Boolean read get_AlwaysShowHeader write put_AlwaysShowHeader;
    property AutoSuggestBox: IAutoSuggestBox read get_AutoSuggestBox write put_AutoSuggestBox;
    property CompactModeThresholdWidth: Double read get_CompactModeThresholdWidth write put_CompactModeThresholdWidth;
    property CompactPaneLength: Double read get_CompactPaneLength write put_CompactPaneLength;
    property DisplayMode: NavigationViewDisplayMode read get_DisplayMode;
    property ExpandedModeThresholdWidth: Double read get_ExpandedModeThresholdWidth write put_ExpandedModeThresholdWidth;
    property Header: IInspectable read get_Header write put_Header;
    property HeaderTemplate: IDataTemplate read get_HeaderTemplate write put_HeaderTemplate;
    property IsPaneOpen: Boolean read get_IsPaneOpen write put_IsPaneOpen;
    property IsPaneToggleButtonVisible: Boolean read get_IsPaneToggleButtonVisible write put_IsPaneToggleButtonVisible;
    property IsSettingsVisible: Boolean read get_IsSettingsVisible write put_IsSettingsVisible;
    property MenuItemContainerStyle: IStyle read get_MenuItemContainerStyle write put_MenuItemContainerStyle;
    property MenuItemContainerStyleSelector: IStyleSelector read get_MenuItemContainerStyleSelector write put_MenuItemContainerStyleSelector;
    property MenuItemTemplate: IDataTemplate read get_MenuItemTemplate write put_MenuItemTemplate;
    property MenuItemTemplateSelector: IDataTemplateSelector read get_MenuItemTemplateSelector write put_MenuItemTemplateSelector;
    property MenuItems: IVector_1__IInspectable read get_MenuItems;
    property MenuItemsSource: IInspectable read get_MenuItemsSource write put_MenuItemsSource;
    property OpenPaneLength: Double read get_OpenPaneLength write put_OpenPaneLength;
    property PaneFooter: IUIElement read get_PaneFooter write put_PaneFooter;
    property PaneToggleButtonStyle: IStyle read get_PaneToggleButtonStyle write put_PaneToggleButtonStyle;
    property SelectedItem: IInspectable read get_SelectedItem write put_SelectedItem;
    property SettingsItem: IInspectable read get_SettingsItem;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IAutoSuggestBox
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_AutoSuggestBox)]
  IAutoSuggestBox = interface(IInspectable)
  ['{103E9B13-3400-4A16-90B9-6912BF06974F}']
    function get_MaxSuggestionListHeight: Double; safecall;
    procedure put_MaxSuggestionListHeight(value: Double); safecall;
    function get_IsSuggestionListOpen: Boolean; safecall;
    procedure put_IsSuggestionListOpen(value: Boolean); safecall;
    function get_TextMemberPath: HSTRING; safecall;
    procedure put_TextMemberPath(value: HSTRING); safecall;
    function get_Text: HSTRING; safecall;
    procedure put_Text(value: HSTRING); safecall;
    function get_UpdateTextOnSelect: Boolean; safecall;
    procedure put_UpdateTextOnSelect(value: Boolean); safecall;
    function get_PlaceholderText: HSTRING; safecall;
    procedure put_PlaceholderText(value: HSTRING); safecall;
    function get_Header: IInspectable; safecall;
    procedure put_Header(value: IInspectable); safecall;
    function get_AutoMaximizeSuggestionArea: Boolean; safecall;
    procedure put_AutoMaximizeSuggestionArea(value: Boolean); safecall;
    function get_TextBoxStyle: IStyle; safecall;
    procedure put_TextBoxStyle(value: IStyle); safecall;
    function add_SuggestionChosen(handler: TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxSuggestionChosenEventArgs): EventRegistrationToken; safecall;
    procedure remove_SuggestionChosen(token: EventRegistrationToken); safecall;
    function add_TextChanged(handler: TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxTextChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_TextChanged(token: EventRegistrationToken); safecall;
    property AutoMaximizeSuggestionArea: Boolean read get_AutoMaximizeSuggestionArea write put_AutoMaximizeSuggestionArea;
    property Header: IInspectable read get_Header write put_Header;
    property IsSuggestionListOpen: Boolean read get_IsSuggestionListOpen write put_IsSuggestionListOpen;
    property MaxSuggestionListHeight: Double read get_MaxSuggestionListHeight write put_MaxSuggestionListHeight;
    property PlaceholderText: HSTRING read get_PlaceholderText write put_PlaceholderText;
    property Text: HSTRING read get_Text write put_Text;
    property TextBoxStyle: IStyle read get_TextBoxStyle write put_TextBoxStyle;
    property TextMemberPath: HSTRING read get_TextMemberPath write put_TextMemberPath;
    property UpdateTextOnSelect: Boolean read get_UpdateTextOnSelect write put_UpdateTextOnSelect;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IAutoSuggestBox,Windows.UI.Xaml.Controls.IAutoSuggestBoxSuggestionChosenEventArgs>
  TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxSuggestionChosenEventArgs_Delegate_Base = interface(IUnknown)
  ['{7996A97E-615E-5FF5-BE3D-0FF9FE43451C}']
    procedure Invoke(sender: IAutoSuggestBox; args: IAutoSuggestBoxSuggestionChosenEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IAutoSuggestBox,Windows.UI.Xaml.Controls.IAutoSuggestBoxSuggestionChosenEventArgs>
  // External 
  TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxSuggestionChosenEventArgs = interface(TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxSuggestionChosenEventArgs_Delegate_Base)
  ['{8DCE7CA8-5F9F-5C9D-8769-D2877749E5FD}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IAutoSuggestBoxSuggestionChosenEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_AutoSuggestBoxSuggestionChosenEventArgs)]
  IAutoSuggestBoxSuggestionChosenEventArgs = interface(IInspectable)
  ['{396F7254-1ED5-4BC5-A060-655530BCA6BA}']
    function get_SelectedItem: IInspectable; safecall;
    property SelectedItem: IInspectable read get_SelectedItem;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IAutoSuggestBox,Windows.UI.Xaml.Controls.IAutoSuggestBoxTextChangedEventArgs>
  TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxTextChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{7E2DC943-5F2F-54FB-816F-8E65E893F90A}']
    procedure Invoke(sender: IAutoSuggestBox; args: IAutoSuggestBoxTextChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IAutoSuggestBox,Windows.UI.Xaml.Controls.IAutoSuggestBoxTextChangedEventArgs>
  // External 
  TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxTextChangedEventArgs = interface(TypedEventHandler_2__IAutoSuggestBox__IAutoSuggestBoxTextChangedEventArgs_Delegate_Base)
  ['{299721D6-CF8C-57EB-9805-D4F37BB75985}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IAutoSuggestBoxTextChangedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_AutoSuggestBoxTextChangedEventArgs)]
  IAutoSuggestBoxTextChangedEventArgs = interface(IInspectable)
  ['{3A6F7254-1ED5-4BC5-A060-655530BCA6BA}']
    function get_Reason: AutoSuggestionBoxTextChangeReason; safecall;
    procedure put_Reason(value: AutoSuggestionBoxTextChangeReason); safecall;
    function CheckCurrent: Boolean; safecall;
    property Reason: AutoSuggestionBoxTextChangeReason read get_Reason write put_Reason;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IDataTemplateSelector
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_DataTemplateSelector)]
  IDataTemplateSelector = interface(IInspectable)
  ['{A907D496-46A0-4CD7-8DBE-F9A581DF60B1}']
    function SelectTemplate(item: IInspectable; container: IDependencyObject): IDataTemplate; safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IStyleSelector
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_StyleSelector)]
  IStyleSelector = interface(IInspectable)
  ['{D1113F67-D8C1-4AE4-98F0-D8504502F08B}']
    function SelectStyle(item: IInspectable; container: IDependencyObject): IStyle; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewSelectionChangedEventArgs>
  TypedEventHandler_2__INavigationView__INavigationViewSelectionChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{17C78C17-2850-5DD4-83D6-D470323C21C7}']
    procedure Invoke(sender: INavigationView; args: INavigationViewSelectionChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewSelectionChangedEventArgs>
  // External 
  TypedEventHandler_2__INavigationView__INavigationViewSelectionChangedEventArgs = interface(TypedEventHandler_2__INavigationView__INavigationViewSelectionChangedEventArgs_Delegate_Base)
  ['{33D42E4C-7920-50C6-B5D6-600B578668F6}']
  end;

  // Windows.UI.Xaml.Controls.INavigationViewSelectionChangedEventArgs
  // External 
  INavigationViewSelectionChangedEventArgs = interface(IInspectable)
  ['{5AA509A0-3A5E-4F54-896C-98B85F819508}']
    function get_SelectedItem: IInspectable; safecall;
    function get_IsSettingsSelected: Boolean; safecall;
    property IsSettingsSelected: Boolean read get_IsSettingsSelected;
    property SelectedItem: IInspectable read get_SelectedItem;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewItemInvokedEventArgs>
  TypedEventHandler_2__INavigationView__INavigationViewItemInvokedEventArgs_Delegate_Base = interface(IUnknown)
  ['{15EC8EC2-C8B3-53E7-893C-0CFB68549B77}']
    procedure Invoke(sender: INavigationView; args: INavigationViewItemInvokedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewItemInvokedEventArgs>
  // External 
  TypedEventHandler_2__INavigationView__INavigationViewItemInvokedEventArgs = interface(TypedEventHandler_2__INavigationView__INavigationViewItemInvokedEventArgs_Delegate_Base)
  ['{7E414136-EF72-5917-A183-89170FE40FAE}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.INavigationViewItemInvokedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_NavigationViewItemInvokedEventArgs)]
  INavigationViewItemInvokedEventArgs = interface(IInspectable)
  ['{29496822-55D2-49FA-964B-F1DBAFEF85C4}']
    function get_InvokedItem: IInspectable; safecall;
    function get_IsSettingsInvoked: Boolean; safecall;
    property InvokedItem: IInspectable read get_InvokedItem;
    property IsSettingsInvoked: Boolean read get_IsSettingsInvoked;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewDisplayModeChangedEventArgs>
  TypedEventHandler_2__INavigationView__INavigationViewDisplayModeChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{10A54AC4-84CF-580B-A63D-51AA3A6A3C0A}']
    procedure Invoke(sender: INavigationView; args: INavigationViewDisplayModeChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.INavigationView,Windows.UI.Xaml.Controls.INavigationViewDisplayModeChangedEventArgs>
  // External 
  TypedEventHandler_2__INavigationView__INavigationViewDisplayModeChangedEventArgs = interface(TypedEventHandler_2__INavigationView__INavigationViewDisplayModeChangedEventArgs_Delegate_Base)
  ['{96E5CEB2-1DD3-5B27-9CB7-9A1074E6CF2F}']
  end;

  // Windows.UI.Xaml.Controls.INavigationViewDisplayModeChangedEventArgs
  // External 
  INavigationViewDisplayModeChangedEventArgs = interface(IInspectable)
  ['{B7C1AD35-5544-40C1-9B33-ACFE1D6C8094}']
    function get_DisplayMode: NavigationViewDisplayMode; safecall;
    property DisplayMode: NavigationViewDisplayMode read get_DisplayMode;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Documents.IContentLinkProviderCollection
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Documents_ContentLinkProviderCollection)]
  Documents_IContentLinkProviderCollection = interface(IInspectable)
  ['{F5B84D0C-A9F4-4D1A-A13C-10DEF1843734}']
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IRichEditBox,Windows.UI.Xaml.Documents.IContentLinkInvokedEventArgs>
  // External 
  TypedEventHandler_2__IRichEditBox__Documents_IContentLinkInvokedEventArgs = interface(IUnknown)
  ['{5E719759-EDFE-5EEC-8035-5B1D56301DE0}']
    procedure Invoke(sender: IRichEditBox; args: Documents_IContentLinkInvokedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IRichEditBox
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_RichEditBox)]
  IRichEditBox = interface(IInspectable)
  ['{90A57A40-80B6-4FCE-B1EC-E3C616284B6A}']
    function get_IsReadOnly: Boolean; safecall;
    procedure put_IsReadOnly(value: Boolean); safecall;
    function get_AcceptsReturn: Boolean; safecall;
    procedure put_AcceptsReturn(value: Boolean); safecall;
    function get_TextAlignment: TextAlignment; safecall;
    procedure put_TextAlignment(value: TextAlignment); safecall;
    function get_TextWrapping: TextWrapping; safecall;
    procedure put_TextWrapping(value: TextWrapping); safecall;
    function get_IsSpellCheckEnabled: Boolean; safecall;
    procedure put_IsSpellCheckEnabled(value: Boolean); safecall;
    function get_IsTextPredictionEnabled: Boolean; safecall;
    procedure put_IsTextPredictionEnabled(value: Boolean); safecall;
    function get_Document: ITextDocument; safecall;
    function get_InputScope: Input_IInputScope; safecall;
    procedure put_InputScope(value: Input_IInputScope); safecall;
    function add_TextChanged(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_TextChanged(token: EventRegistrationToken); safecall;
    function add_SelectionChanged(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_SelectionChanged(token: EventRegistrationToken); safecall;
    function add_ContextMenuOpening(handler: ContextMenuOpeningEventHandler): EventRegistrationToken; safecall;
    procedure remove_ContextMenuOpening(token: EventRegistrationToken); safecall;
    property AcceptsReturn: Boolean read get_AcceptsReturn write put_AcceptsReturn;
    property Document: ITextDocument read get_Document;
    property InputScope: Input_IInputScope read get_InputScope write put_InputScope;
    property IsReadOnly: Boolean read get_IsReadOnly write put_IsReadOnly;
    property IsSpellCheckEnabled: Boolean read get_IsSpellCheckEnabled write put_IsSpellCheckEnabled;
    property IsTextPredictionEnabled: Boolean read get_IsTextPredictionEnabled write put_IsTextPredictionEnabled;
    property TextAlignment_: TextAlignment read get_TextAlignment write put_TextAlignment;
    property TextWrapping_: TextWrapping read get_TextWrapping write put_TextWrapping;
  end;

  // Windows.UI.Text.ITextDocument
  // External 
  ITextDocument = interface(IInspectable)
  ['{BEEE4DDB-90B2-408C-A2F6-0A0AC31E33E4}']
    function get_CaretType: CaretType; safecall;
    procedure put_CaretType(value: CaretType); safecall;
    function get_DefaultTabStop: Single; safecall;
    procedure put_DefaultTabStop(value: Single); safecall;
    function get_Selection: ITextSelection; safecall;
    function get_UndoLimit: Cardinal; safecall;
    procedure put_UndoLimit(value: Cardinal); safecall;
    function CanCopy: Boolean; safecall;
    function CanPaste: Boolean; safecall;
    function CanRedo: Boolean; safecall;
    function CanUndo: Boolean; safecall;
    function ApplyDisplayUpdates: Integer; safecall;
    function BatchDisplayUpdates: Integer; safecall;
    procedure BeginUndoGroup; safecall;
    procedure EndUndoGroup; safecall;
    function GetDefaultCharacterFormat: ITextCharacterFormat; safecall;
    function GetDefaultParagraphFormat: ITextParagraphFormat; safecall;
    function GetRange(startPosition: Integer; endPosition: Integer): ITextRange; safecall;
    function GetRangeFromPoint(point: TPointF; options: PointOptions): ITextRange; safecall;
    procedure GetText(options: TextGetOptions; out value: HSTRING); safecall;
    procedure LoadFromStream(options: TextSetOptions; value: IRandomAccessStream); safecall;
    procedure Redo; safecall;
    procedure SaveToStream(options: TextGetOptions; value: IRandomAccessStream); safecall;
    procedure SetDefaultCharacterFormat(value: ITextCharacterFormat); safecall;
    procedure SetDefaultParagraphFormat(value: ITextParagraphFormat); safecall;
    procedure SetText(options: TextSetOptions; value: HSTRING); safecall;
    procedure Undo; safecall;
    property CaretType_: CaretType read get_CaretType write put_CaretType;
    property DefaultTabStop: Single read get_DefaultTabStop write put_DefaultTabStop;
    property Selection: ITextSelection read get_Selection;
    property UndoLimit: Cardinal read get_UndoLimit write put_UndoLimit;
  end;

  // Windows.UI.Text.ITextSelection
  // External 
  ITextSelection = interface(IInspectable)
  ['{A6D36724-F28F-430A-B2CF-C343671EC0E9}']
    function get_Options: SelectionOptions; safecall;
    procedure put_Options(value: SelectionOptions); safecall;
    function get_Type: SelectionType; safecall;
    function EndKey(&unit: TextRangeUnit; extend: Boolean): Integer; safecall;
    function HomeKey(&unit: TextRangeUnit; extend: Boolean): Integer; safecall;
    function MoveDown(&unit: TextRangeUnit; count: Integer; extend: Boolean): Integer; safecall;
    function MoveLeft(&unit: TextRangeUnit; count: Integer; extend: Boolean): Integer; safecall;
    function MoveRight(&unit: TextRangeUnit; count: Integer; extend: Boolean): Integer; safecall;
    function MoveUp(&unit: TextRangeUnit; count: Integer; extend: Boolean): Integer; safecall;
    procedure TypeText(value: HSTRING); safecall;
    property Options: SelectionOptions read get_Options write put_Options;
    property &Type: SelectionType read get_Type;
  end;

  // Windows.UI.Text.ITextCharacterFormat
  // External 
  ITextCharacterFormat = interface(IInspectable)
  ['{5ADEF3DB-05FB-442D-8065-642AFEA02CED}']
    function get_AllCaps: FormatEffect; safecall;
    procedure put_AllCaps(value: FormatEffect); safecall;
    function get_BackgroundColor: Color; safecall;
    procedure put_BackgroundColor(value: Color); safecall;
    function get_Bold: FormatEffect; safecall;
    procedure put_Bold(value: FormatEffect); safecall;
    function get_FontStretch: FontStretch; safecall;
    procedure put_FontStretch(value: FontStretch); safecall;
    function get_FontStyle: FontStyle; safecall;
    procedure put_FontStyle(value: FontStyle); safecall;
    function get_ForegroundColor: Color; safecall;
    procedure put_ForegroundColor(value: Color); safecall;
    function get_Hidden: FormatEffect; safecall;
    procedure put_Hidden(value: FormatEffect); safecall;
    function get_Italic: FormatEffect; safecall;
    procedure put_Italic(value: FormatEffect); safecall;
    function get_Kerning: Single; safecall;
    procedure put_Kerning(value: Single); safecall;
    function get_LanguageTag: HSTRING; safecall;
    procedure put_LanguageTag(value: HSTRING); safecall;
    function get_LinkType: LinkType; safecall;
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    function get_Outline: FormatEffect; safecall;
    procedure put_Outline(value: FormatEffect); safecall;
    function get_Position: Single; safecall;
    procedure put_Position(value: Single); safecall;
    function get_ProtectedText: FormatEffect; safecall;
    procedure put_ProtectedText(value: FormatEffect); safecall;
    function get_Size: Single; safecall;
    procedure put_Size(value: Single); safecall;
    function get_SmallCaps: FormatEffect; safecall;
    procedure put_SmallCaps(value: FormatEffect); safecall;
    function get_Spacing: Single; safecall;
    procedure put_Spacing(value: Single); safecall;
    function get_Strikethrough: FormatEffect; safecall;
    procedure put_Strikethrough(value: FormatEffect); safecall;
    function get_Subscript: FormatEffect; safecall;
    procedure put_Subscript(value: FormatEffect); safecall;
    function get_Superscript: FormatEffect; safecall;
    procedure put_Superscript(value: FormatEffect); safecall;
    function get_TextScript: TextScript; safecall;
    procedure put_TextScript(value: TextScript); safecall;
    function get_Underline: UnderlineType; safecall;
    procedure put_Underline(value: UnderlineType); safecall;
    function get_Weight: Integer; safecall;
    procedure put_Weight(value: Integer); safecall;
    procedure SetClone(value: ITextCharacterFormat); safecall;
    function GetClone: ITextCharacterFormat; safecall;
    function IsEqual(format: ITextCharacterFormat): Boolean; safecall;
    property AllCaps: FormatEffect read get_AllCaps write put_AllCaps;
    property BackgroundColor: Color read get_BackgroundColor write put_BackgroundColor;
    property Bold: FormatEffect read get_Bold write put_Bold;
    property FontStretch_: FontStretch read get_FontStretch write put_FontStretch;
    property FontStyle_: FontStyle read get_FontStyle write put_FontStyle;
    property ForegroundColor: Color read get_ForegroundColor write put_ForegroundColor;
    property Hidden: FormatEffect read get_Hidden write put_Hidden;
    property Italic: FormatEffect read get_Italic write put_Italic;
    property Kerning: Single read get_Kerning write put_Kerning;
    property LanguageTag: HSTRING read get_LanguageTag write put_LanguageTag;
    property LinkType_: LinkType read get_LinkType;
    property Name: HSTRING read get_Name write put_Name;
    property Outline: FormatEffect read get_Outline write put_Outline;
    property Position: Single read get_Position write put_Position;
    property ProtectedText: FormatEffect read get_ProtectedText write put_ProtectedText;
    property Size: Single read get_Size write put_Size;
    property SmallCaps: FormatEffect read get_SmallCaps write put_SmallCaps;
    property Spacing: Single read get_Spacing write put_Spacing;
    property Strikethrough: FormatEffect read get_Strikethrough write put_Strikethrough;
    property Subscript: FormatEffect read get_Subscript write put_Subscript;
    property Superscript: FormatEffect read get_Superscript write put_Superscript;
    property TextScript_: TextScript read get_TextScript write put_TextScript;
    property Underline: UnderlineType read get_Underline write put_Underline;
    property Weight: Integer read get_Weight write put_Weight;
  end;

  // Windows.UI.Text.ITextParagraphFormat
  // External 
  ITextParagraphFormat = interface(IInspectable)
  ['{2CF8CFA6-4676-498A-93F5-BBDBFC0BD883}']
    function get_Alignment: ParagraphAlignment; safecall;
    procedure put_Alignment(value: ParagraphAlignment); safecall;
    function get_FirstLineIndent: Single; safecall;
    function get_KeepTogether: FormatEffect; safecall;
    procedure put_KeepTogether(value: FormatEffect); safecall;
    function get_KeepWithNext: FormatEffect; safecall;
    procedure put_KeepWithNext(value: FormatEffect); safecall;
    function get_LeftIndent: Single; safecall;
    function get_LineSpacing: Single; safecall;
    function get_LineSpacingRule: LineSpacingRule; safecall;
    function get_ListAlignment: MarkerAlignment; safecall;
    procedure put_ListAlignment(value: MarkerAlignment); safecall;
    function get_ListLevelIndex: Integer; safecall;
    procedure put_ListLevelIndex(value: Integer); safecall;
    function get_ListStart: Integer; safecall;
    procedure put_ListStart(value: Integer); safecall;
    function get_ListStyle: MarkerStyle; safecall;
    procedure put_ListStyle(value: MarkerStyle); safecall;
    function get_ListTab: Single; safecall;
    procedure put_ListTab(value: Single); safecall;
    function get_ListType: MarkerType; safecall;
    procedure put_ListType(value: MarkerType); safecall;
    function get_NoLineNumber: FormatEffect; safecall;
    procedure put_NoLineNumber(value: FormatEffect); safecall;
    function get_PageBreakBefore: FormatEffect; safecall;
    procedure put_PageBreakBefore(value: FormatEffect); safecall;
    function get_RightIndent: Single; safecall;
    procedure put_RightIndent(value: Single); safecall;
    function get_RightToLeft: FormatEffect; safecall;
    procedure put_RightToLeft(value: FormatEffect); safecall;
    function get_Style: ParagraphStyle; safecall;
    procedure put_Style(value: ParagraphStyle); safecall;
    function get_SpaceAfter: Single; safecall;
    procedure put_SpaceAfter(value: Single); safecall;
    function get_SpaceBefore: Single; safecall;
    procedure put_SpaceBefore(value: Single); safecall;
    function get_WidowControl: FormatEffect; safecall;
    procedure put_WidowControl(value: FormatEffect); safecall;
    function get_TabCount: Integer; safecall;
    procedure AddTab(position: Single; align: TabAlignment; leader: TabLeader); safecall;
    procedure ClearAllTabs; safecall;
    procedure DeleteTab(position: Single); safecall;
    function GetClone: ITextParagraphFormat; safecall;
    procedure GetTab(index: Integer; out position: Single; out align: TabAlignment; out leader: TabLeader); safecall;
    function IsEqual(format: ITextParagraphFormat): Boolean; safecall;
    procedure SetClone(format: ITextParagraphFormat); safecall;
    procedure SetIndents(start: Single; left: Single; right: Single); safecall;
    procedure SetLineSpacing(rule: LineSpacingRule; spacing: Single); safecall;
    property Alignment: ParagraphAlignment read get_Alignment write put_Alignment;
    property FirstLineIndent: Single read get_FirstLineIndent;
    property KeepTogether: FormatEffect read get_KeepTogether write put_KeepTogether;
    property KeepWithNext: FormatEffect read get_KeepWithNext write put_KeepWithNext;
    property LeftIndent: Single read get_LeftIndent;
    property LineSpacing: Single read get_LineSpacing;
    property LineSpacingRule_: LineSpacingRule read get_LineSpacingRule;
    property ListAlignment: MarkerAlignment read get_ListAlignment write put_ListAlignment;
    property ListLevelIndex: Integer read get_ListLevelIndex write put_ListLevelIndex;
    property ListStart: Integer read get_ListStart write put_ListStart;
    property ListStyle: MarkerStyle read get_ListStyle write put_ListStyle;
    property ListTab: Single read get_ListTab write put_ListTab;
    property ListType: MarkerType read get_ListType write put_ListType;
    property NoLineNumber: FormatEffect read get_NoLineNumber write put_NoLineNumber;
    property PageBreakBefore: FormatEffect read get_PageBreakBefore write put_PageBreakBefore;
    property RightIndent: Single read get_RightIndent write put_RightIndent;
    property RightToLeft: FormatEffect read get_RightToLeft write put_RightToLeft;
    property SpaceAfter: Single read get_SpaceAfter write put_SpaceAfter;
    property SpaceBefore: Single read get_SpaceBefore write put_SpaceBefore;
    property Style: ParagraphStyle read get_Style write put_Style;
    property TabCount: Integer read get_TabCount;
    property WidowControl: FormatEffect read get_WidowControl write put_WidowControl;
  end;

  // Windows.UI.Text.ITextRange
  // External 
  ITextRange = interface(IInspectable)
  ['{5B9E4E57-C072-42A0-8945-AF503EE54768}']
    function get_Character: Char; safecall;
    procedure put_Character(value: Char); safecall;
    function get_CharacterFormat: ITextCharacterFormat; safecall;
    procedure put_CharacterFormat(value: ITextCharacterFormat); safecall;
    function get_FormattedText: ITextRange; safecall;
    procedure put_FormattedText(value: ITextRange); safecall;
    function get_EndPosition: Integer; safecall;
    procedure put_EndPosition(value: Integer); safecall;
    function get_Gravity: RangeGravity; safecall;
    procedure put_Gravity(value: RangeGravity); safecall;
    function get_Length: Integer; safecall;
    function get_Link: HSTRING; safecall;
    procedure put_Link(value: HSTRING); safecall;
    function get_ParagraphFormat: ITextParagraphFormat; safecall;
    procedure put_ParagraphFormat(value: ITextParagraphFormat); safecall;
    function get_StartPosition: Integer; safecall;
    procedure put_StartPosition(value: Integer); safecall;
    function get_StoryLength: Integer; safecall;
    function get_Text: HSTRING; safecall;
    procedure put_Text(value: HSTRING); safecall;
    function CanPaste(format: Integer): Boolean; safecall;
    procedure ChangeCase(value: LetterCase); safecall;
    procedure Collapse(value: Boolean); safecall;
    procedure Copy; safecall;
    procedure Cut; safecall;
    function Delete(&unit: TextRangeUnit; count: Integer): Integer; safecall;
    function EndOf(&unit: TextRangeUnit; extend: Boolean): Integer; safecall;
    function Expand(&unit: TextRangeUnit): Integer; safecall;
    function FindText(value: HSTRING; scanLength: Integer; options: FindOptions): Integer; safecall;
    procedure GetCharacterUtf32(out value: Cardinal; offset: Integer); safecall;
    function GetClone: ITextRange; safecall;
    function GetIndex(&unit: TextRangeUnit): Integer; safecall;
    procedure GetPoint(horizontalAlign: HorizontalCharacterAlignment; verticalAlign: VerticalCharacterAlignment; options: PointOptions; out point: TPointF); safecall;
    procedure GetRect(options: PointOptions; out rect: TRectF; out hit: Integer); safecall;
    procedure GetText(options: TextGetOptions; out value: HSTRING); safecall;
    procedure GetTextViaStream(options: TextGetOptions; value: IRandomAccessStream); safecall;
    function InRange(range: ITextRange): Boolean; safecall;
    procedure InsertImage(width: Integer; height: Integer; ascent: Integer; verticalAlign: VerticalCharacterAlignment; alternateText: HSTRING; value: IRandomAccessStream); safecall;
    function InStory(range: ITextRange): Boolean; safecall;
    function IsEqual(range: ITextRange): Boolean; safecall;
    function Move(&unit: TextRangeUnit; count: Integer): Integer; safecall;
    function MoveEnd(&unit: TextRangeUnit; count: Integer): Integer; safecall;
    function MoveStart(&unit: TextRangeUnit; count: Integer): Integer; safecall;
    procedure Paste(format: Integer); safecall;
    procedure ScrollIntoView(value: PointOptions); safecall;
    procedure MatchSelection; safecall;
    procedure SetIndex(&unit: TextRangeUnit; index: Integer; extend: Boolean); safecall;
    procedure SetPoint(point: TPointF; options: PointOptions; extend: Boolean); safecall;
    procedure SetRange(startPosition: Integer; endPosition: Integer); safecall;
    procedure SetText(options: TextSetOptions; value: HSTRING); safecall;
    procedure SetTextViaStream(options: TextSetOptions; value: IRandomAccessStream); safecall;
    function StartOf(&unit: TextRangeUnit; extend: Boolean): Integer; safecall;
    property Character: Char read get_Character write put_Character;
    property CharacterFormat: ITextCharacterFormat read get_CharacterFormat write put_CharacterFormat;
    property EndPosition: Integer read get_EndPosition write put_EndPosition;
    property FormattedText: ITextRange read get_FormattedText write put_FormattedText;
    property Gravity: RangeGravity read get_Gravity write put_Gravity;
    property Length: Integer read get_Length;
    property Link: HSTRING read get_Link write put_Link;
    property ParagraphFormat: ITextParagraphFormat read get_ParagraphFormat write put_ParagraphFormat;
    property StartPosition: Integer read get_StartPosition write put_StartPosition;
    property StoryLength: Integer read get_StoryLength;
    property Text: HSTRING read get_Text write put_Text;
  end;

  // Windows.UI.Xaml.Controls.ContextMenuOpeningEventHandler
  // External 
  ContextMenuOpeningEventHandler = interface(IUnknown)
  ['{DF945151-745C-4446-B2FC-216D765847A0}']
    procedure Invoke(sender: IInspectable; e: IContextMenuEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Controls.IContextMenuEventArgs
  // External 
  IContextMenuEventArgs = interface(IInspectable)
  ['{1F7DF263-C14B-4528-B6F0-637999D83CC6}']
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    function get_CursorLeft: Double; safecall;
    function get_CursorTop: Double; safecall;
    property CursorLeft: Double read get_CursorLeft;
    property CursorTop: Double read get_CursorTop;
    property Handled: Boolean read get_Handled write put_Handled;
  end;

  // Windows.UI.Xaml.Documents.IContentLinkInvokedEventArgs
  // External 
  Documents_IContentLinkInvokedEventArgs = interface(IInspectable)
  ['{546717C1-E8DF-4593-9639-97595FDF8310}']
    function get_ContentLinkInfo: IContentLinkInfo; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    property ContentLinkInfo: IContentLinkInfo read get_ContentLinkInfo;
    property Handled: Boolean read get_Handled write put_Handled;
  end;

  // Windows.UI.Text.IContentLinkInfo
  // External 
  IContentLinkInfo = interface(IInspectable)
  ['{1ED52525-1C5F-48CB-B335-78B50A2EE642}']
    function get_Id: Cardinal; safecall;
    procedure put_Id(value: Cardinal); safecall;
    function get_DisplayText: HSTRING; safecall;
    procedure put_DisplayText(value: HSTRING); safecall;
    function get_SecondaryText: HSTRING; safecall;
    procedure put_SecondaryText(value: HSTRING); safecall;
    function get_Uri: IUriRuntimeClass; safecall;
    procedure put_Uri(value: IUriRuntimeClass); safecall;
    function get_LinkContentKind: HSTRING; safecall;
    procedure put_LinkContentKind(value: HSTRING); safecall;
    property DisplayText: HSTRING read get_DisplayText write put_DisplayText;
    property Id: Cardinal read get_Id write put_Id;
    property LinkContentKind: HSTRING read get_LinkContentKind write put_LinkContentKind;
    property SecondaryText: HSTRING read get_SecondaryText write put_SecondaryText;
    property Uri: IUriRuntimeClass read get_Uri write put_Uri;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.ITextHighlighter>
  IVector_1__Documents_ITextHighlighter_Base = interface(IInspectable)
  ['{64905558-D062-5F31-84AF-4A5FA896AE50}']
    function GetAt(index: Cardinal): Documents_ITextHighlighter; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Documents_ITextHighlighter; safecall;
    function IndexOf(value: Documents_ITextHighlighter; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Documents_ITextHighlighter); safecall;
    procedure InsertAt(index: Cardinal; value: Documents_ITextHighlighter); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Documents_ITextHighlighter); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_ITextHighlighter): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDocuments_ITextHighlighter); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.ITextHighlighter>
  // External 
  IVector_1__Documents_ITextHighlighter = interface(IVector_1__Documents_ITextHighlighter_Base)
  ['{B5232EE7-35E6-55A9-965D-83B0E8DF4CB7}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Documents.ITextHighlighter
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Documents_TextHighlighter)]
  Documents_ITextHighlighter = interface(IInspectable)
  ['{BA6CB54B-7D75-4535-B30D-A81A00B637A4}']
    function get_Ranges: IVector_1__Documents_TextRange; safecall;
    function get_Foreground: IBrush; safecall;
    procedure put_Foreground(value: IBrush); safecall;
    function get_Background: IBrush; safecall;
    procedure put_Background(value: IBrush); safecall;
    property Background: IBrush read get_Background write put_Background;
    property Foreground: IBrush read get_Foreground write put_Foreground;
    property Ranges: IVector_1__Documents_TextRange read get_Ranges;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.TextRange>
  IVector_1__Documents_TextRange_Base = interface(IInspectable)
  ['{EE9D4CDA-0750-5C1F-93AA-59ADD8C1421B}']
    function GetAt(index: Cardinal): Documents_TextRange; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Documents_TextRange; safecall;
    function IndexOf(value: Documents_TextRange; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Documents_TextRange); safecall;
    procedure InsertAt(index: Cardinal; value: Documents_TextRange); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Documents_TextRange); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_TextRange): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDocuments_TextRange); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Documents.TextRange>
  // External 
  IVector_1__Documents_TextRange = interface(IVector_1__Documents_TextRange_Base)
  ['{A4739064-B54E-55D4-8012-317E2B6A807B}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.TextRange>
  // External 
  IVectorView_1__Documents_TextRange = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Documents_TextRange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Documents_TextRange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_TextRange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.IBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Brush)]
  IBrush = interface(IInspectable)
  ['{8806A321-1E06-422C-A1CC-01696559E021}']
    function get_Opacity: Double; safecall;
    procedure put_Opacity(value: Double); safecall;
    function get_Transform: ITransform; safecall;
    procedure put_Transform(value: ITransform); safecall;
    function get_RelativeTransform: ITransform; safecall;
    procedure put_RelativeTransform(value: ITransform); safecall;
    property Opacity: Double read get_Opacity write put_Opacity;
    property RelativeTransform: ITransform read get_RelativeTransform write put_RelativeTransform;
    property Transform: ITransform read get_Transform write put_Transform;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Documents.ITextHighlighter>
  // External 
  IVectorView_1__Documents_ITextHighlighter = interface(IInspectable)
  ['{6B4C9285-E325-5E45-8BCB-9A633033D063}']
    function GetAt(index: Cardinal): Documents_ITextHighlighter; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Documents_ITextHighlighter; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDocuments_ITextHighlighter): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISplitView,Object>
  TypedEventHandler_2__ISplitView__IInspectable_Delegate_Base = interface(IUnknown)
  ['{E277BFE5-10C1-5472-9BC6-1AE39AEBFC86}']
    procedure Invoke(sender: ISplitView; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISplitView,Object>
  // External 
  TypedEventHandler_2__ISplitView__IInspectable = interface(TypedEventHandler_2__ISplitView__IInspectable_Delegate_Base)
  ['{19BD7D65-53E9-5DAC-9BDF-93EBC539D03D}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.ISplitView
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_SplitView)]
  ISplitView = interface(IInspectable)
  ['{97222F31-3844-429E-939C-1673155322A1}']
    function get_Content: IUIElement; safecall;
    procedure put_Content(value: IUIElement); safecall;
    function get_Pane: IUIElement; safecall;
    procedure put_Pane(value: IUIElement); safecall;
    function get_IsPaneOpen: Boolean; safecall;
    procedure put_IsPaneOpen(value: Boolean); safecall;
    function get_OpenPaneLength: Double; safecall;
    procedure put_OpenPaneLength(value: Double); safecall;
    function get_CompactPaneLength: Double; safecall;
    procedure put_CompactPaneLength(value: Double); safecall;
    function get_PanePlacement: SplitViewPanePlacement; safecall;
    procedure put_PanePlacement(value: SplitViewPanePlacement); safecall;
    function get_DisplayMode: SplitViewDisplayMode; safecall;
    procedure put_DisplayMode(value: SplitViewDisplayMode); safecall;
    function get_TemplateSettings: Primitives_ISplitViewTemplateSettings; safecall;
    function get_PaneBackground: IBrush; safecall;
    procedure put_PaneBackground(value: IBrush); safecall;
    function add_PaneClosing(handler: TypedEventHandler_2__ISplitView__ISplitViewPaneClosingEventArgs): EventRegistrationToken; safecall;
    procedure remove_PaneClosing(token: EventRegistrationToken); safecall;
    function add_PaneClosed(handler: TypedEventHandler_2__ISplitView__IInspectable): EventRegistrationToken; safecall;
    procedure remove_PaneClosed(token: EventRegistrationToken); safecall;
    property CompactPaneLength: Double read get_CompactPaneLength write put_CompactPaneLength;
    property Content: IUIElement read get_Content write put_Content;
    property DisplayMode: SplitViewDisplayMode read get_DisplayMode write put_DisplayMode;
    property IsPaneOpen: Boolean read get_IsPaneOpen write put_IsPaneOpen;
    property OpenPaneLength: Double read get_OpenPaneLength write put_OpenPaneLength;
    property Pane: IUIElement read get_Pane write put_Pane;
    property PaneBackground: IBrush read get_PaneBackground write put_PaneBackground;
    property PanePlacement: SplitViewPanePlacement read get_PanePlacement write put_PanePlacement;
    property TemplateSettings: Primitives_ISplitViewTemplateSettings read get_TemplateSettings;
  end;

  // Windows.UI.Xaml.Controls.Primitives.ISplitViewTemplateSettings
  // External 
  Primitives_ISplitViewTemplateSettings = interface(IInspectable)
  ['{C16AB5A7-4996-4443-B199-6B6B89124EAB}']
    function get_OpenPaneLength: Double; safecall;
    function get_NegativeOpenPaneLength: Double; safecall;
    function get_OpenPaneLengthMinusCompactLength: Double; safecall;
    function get_NegativeOpenPaneLengthMinusCompactLength: Double; safecall;
    function get_OpenPaneGridLength: GridLength; safecall;
    function get_CompactPaneGridLength: GridLength; safecall;
    property CompactPaneGridLength: GridLength read get_CompactPaneGridLength;
    property NegativeOpenPaneLength: Double read get_NegativeOpenPaneLength;
    property NegativeOpenPaneLengthMinusCompactLength: Double read get_NegativeOpenPaneLengthMinusCompactLength;
    property OpenPaneGridLength: GridLength read get_OpenPaneGridLength;
    property OpenPaneLength: Double read get_OpenPaneLength;
    property OpenPaneLengthMinusCompactLength: Double read get_OpenPaneLengthMinusCompactLength;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISplitView,Windows.UI.Xaml.Controls.ISplitViewPaneClosingEventArgs>
  TypedEventHandler_2__ISplitView__ISplitViewPaneClosingEventArgs_Delegate_Base = interface(IUnknown)
  ['{C674A1DE-C3D9-5E39-BC39-121FC3CC7D41}']
    procedure Invoke(sender: ISplitView; args: ISplitViewPaneClosingEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISplitView,Windows.UI.Xaml.Controls.ISplitViewPaneClosingEventArgs>
  // External 
  TypedEventHandler_2__ISplitView__ISplitViewPaneClosingEventArgs = interface(TypedEventHandler_2__ISplitView__ISplitViewPaneClosingEventArgs_Delegate_Base)
  ['{35B9572C-B65A-5127-BD69-32BB2344AAD8}']
  end;

  // Windows.UI.Xaml.Controls.ISplitViewPaneClosingEventArgs
  // External 
  ISplitViewPaneClosingEventArgs = interface(IInspectable)
  ['{93CF494E-7A95-44D8-9562-1B348248DA9F}']
    function get_Cancel: Boolean; safecall;
    procedure put_Cancel(value: Boolean); safecall;
    property Cancel: Boolean read get_Cancel write put_Cancel;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISwapChainPanel,Object>
  TypedEventHandler_2__ISwapChainPanel__IInspectable_Delegate_Base = interface(IUnknown)
  ['{A8BBF146-B687-5C03-9A42-2AE2D55BFBB4}']
    procedure Invoke(sender: ISwapChainPanel; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ISwapChainPanel,Object>
  // External 
  TypedEventHandler_2__ISwapChainPanel__IInspectable = interface(TypedEventHandler_2__ISwapChainPanel__IInspectable_Delegate_Base)
  ['{0BE097F3-10F7-5455-B6BC-7F85EFE29FBC}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.ISwapChainPanel
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_SwapChainPanel)]
  ISwapChainPanel = interface(IInspectable)
  ['{C589644F-EBA8-427A-B75A-9F1F93A11AE9}']
    function get_CompositionScaleX: Single; safecall;
    function get_CompositionScaleY: Single; safecall;
    function add_CompositionScaleChanged(handler: TypedEventHandler_2__ISwapChainPanel__IInspectable): EventRegistrationToken; safecall;
    procedure remove_CompositionScaleChanged(token: EventRegistrationToken); safecall;
    function CreateCoreIndependentInputSource(deviceTypes: CoreInputDeviceTypes): ICoreInputSourceBase; safecall;
    property CompositionScaleX: Single read get_CompositionScaleX;
    property CompositionScaleY: Single read get_CompositionScaleY;
  end;

  // Windows.UI.Core.ICoreInputSourceBase
  // External 
  ICoreInputSourceBase = interface(IInspectable)
  ['{9F488807-4580-4BE8-BE68-92A9311713BB}']
    function get_Dispatcher: ICoreDispatcher; safecall;
    function get_IsInputEnabled: Boolean; safecall;
    procedure put_IsInputEnabled(value: Boolean); safecall;
    function add_InputEnabled(handler: TypedEventHandler_2__IInspectable__IInputEnabledEventArgs): EventRegistrationToken; safecall;
    procedure remove_InputEnabled(cookie: EventRegistrationToken); safecall;
    property Dispatcher: ICoreDispatcher read get_Dispatcher;
    property IsInputEnabled: Boolean read get_IsInputEnabled write put_IsInputEnabled;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IInputEnabledEventArgs>
  TypedEventHandler_2__IInspectable__IInputEnabledEventArgs_Delegate_Base = interface(IUnknown)
  ['{C9965F1C-3641-51B6-B823-048EC3628FB0}']
    procedure Invoke(sender: IInspectable; args: IInputEnabledEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IInputEnabledEventArgs>
  // External 
  TypedEventHandler_2__IInspectable__IInputEnabledEventArgs = interface(TypedEventHandler_2__IInspectable__IInputEnabledEventArgs_Delegate_Base)
  ['{CAEE5D54-F45C-56BC-8F24-FFB2A20A00B5}']
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.ITwoPaneView,Object>
  // External 
  TypedEventHandler_2__ITwoPaneView__IInspectable = interface(IUnknown)
  ['{0684C4A1-ACFD-5285-BBE9-3AF4BFA10DBE}']
    procedure Invoke(sender: ITwoPaneView; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.ITwoPaneView
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_TwoPaneView)]
  ITwoPaneView = interface(IInspectable)
  ['{1B4D0DB5-14AD-5926-BB8A-5B0A5C0085F0}']
    function get_Pane1: IUIElement; safecall;
    procedure put_Pane1(value: IUIElement); safecall;
    function get_Pane2: IUIElement; safecall;
    procedure put_Pane2(value: IUIElement); safecall;
    function get_Pane1Length: GridLength; safecall;
    procedure put_Pane1Length(value: GridLength); safecall;
    function get_Pane2Length: GridLength; safecall;
    procedure put_Pane2Length(value: GridLength); safecall;
    function get_PanePriority: TwoPaneViewPriority; safecall;
    procedure put_PanePriority(value: TwoPaneViewPriority); safecall;
    function get_Mode: TwoPaneViewMode; safecall;
    function get_WideModeConfiguration: TwoPaneViewWideModeConfiguration; safecall;
    procedure put_WideModeConfiguration(value: TwoPaneViewWideModeConfiguration); safecall;
    function get_TallModeConfiguration: TwoPaneViewTallModeConfiguration; safecall;
    procedure put_TallModeConfiguration(value: TwoPaneViewTallModeConfiguration); safecall;
    function get_MinWideModeWidth: Double; safecall;
    procedure put_MinWideModeWidth(value: Double); safecall;
    function get_MinTallModeHeight: Double; safecall;
    procedure put_MinTallModeHeight(value: Double); safecall;
    function add_ModeChanged(handler: TypedEventHandler_2__ITwoPaneView__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ModeChanged(token: EventRegistrationToken); safecall;
    property MinTallModeHeight: Double read get_MinTallModeHeight write put_MinTallModeHeight;
    property MinWideModeWidth: Double read get_MinWideModeWidth write put_MinWideModeWidth;
    property Mode: TwoPaneViewMode read get_Mode;
    property Pane1: IUIElement read get_Pane1 write put_Pane1;
    property Pane1Length: GridLength read get_Pane1Length write put_Pane1Length;
    property Pane2: IUIElement read get_Pane2 write put_Pane2;
    property Pane2Length: GridLength read get_Pane2Length write put_Pane2Length;
    property PanePriority: TwoPaneViewPriority read get_PanePriority write put_PanePriority;
    property TallModeConfiguration: TwoPaneViewTallModeConfiguration read get_TallModeConfiguration write put_TallModeConfiguration;
    property WideModeConfiguration: TwoPaneViewWideModeConfiguration read get_WideModeConfiguration write put_WideModeConfiguration;
  end;

  // Windows.UI.Xaml.Navigation.LoadCompletedEventHandler
  // External 
  Navigation_LoadCompletedEventHandler = interface(IUnknown)
  ['{AEBAF785-43FC-4E2C-95C3-97AE84EABC8E}']
    procedure Invoke(sender: IInspectable; e: Navigation_INavigationEventArgs); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IWebView,Object>
  TypedEventHandler_2__IWebView__IInspectable_Delegate_Base = interface(IUnknown)
  ['{D9F52E0D-21FB-5A0B-B4C6-7D162AF7FB9C}']
    procedure Invoke(sender: IWebView; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IWebView,Object>
  // External 
  TypedEventHandler_2__IWebView__IInspectable = interface(TypedEventHandler_2__IWebView__IInspectable_Delegate_Base)
  ['{7E8FB66B-41AF-5766-8339-A61B07F94F8C}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IWebView
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_WebView)]
  IWebView = interface(IInspectable)
  ['{5862CC46-1F7D-479B-92A6-DE7858FE8D54}']
    function get_Source: IUriRuntimeClass; safecall;
    procedure put_Source(value: IUriRuntimeClass); safecall;
    function get_AllowedScriptNotifyUris: IVector_1__IUriRuntimeClass; safecall;
    procedure put_AllowedScriptNotifyUris(value: IVector_1__IUriRuntimeClass); safecall;
    function get_DataTransferPackage: IDataPackage; safecall;
    function add_LoadCompleted(handler: Navigation_LoadCompletedEventHandler): EventRegistrationToken; safecall;
    procedure remove_LoadCompleted(token: EventRegistrationToken); safecall;
    function add_ScriptNotify(handler: NotifyEventHandler): EventRegistrationToken; safecall;
    procedure remove_ScriptNotify(token: EventRegistrationToken); safecall;
    function add_NavigationFailed(handler: WebViewNavigationFailedEventHandler): EventRegistrationToken; safecall;
    procedure remove_NavigationFailed(token: EventRegistrationToken); safecall;
    function InvokeScript(scriptName: HSTRING; argumentsSize: Cardinal; arguments: PHSTRING): HSTRING; safecall;
    procedure Navigate(source: IUriRuntimeClass); safecall;
    procedure NavigateToString(text: HSTRING); safecall;
    property AllowedScriptNotifyUris: IVector_1__IUriRuntimeClass read get_AllowedScriptNotifyUris write put_AllowedScriptNotifyUris;
    property DataTransferPackage: IDataPackage read get_DataTransferPackage;
    property Source: IUriRuntimeClass read get_Source write put_Source;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.IUriRuntimeClass>
  IVector_1__IUriRuntimeClass_Base = interface(IInspectable)
  ['{0D82BD8D-FE62-5D67-A7B9-7886DD75BC4E}']
    function GetAt(index: Cardinal): IUriRuntimeClass; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IUriRuntimeClass; safecall;
    function IndexOf(value: IUriRuntimeClass; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IUriRuntimeClass); safecall;
    procedure InsertAt(index: Cardinal; value: IUriRuntimeClass); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IUriRuntimeClass); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIUriRuntimeClass): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIUriRuntimeClass); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.IUriRuntimeClass>
  // External 
  IVector_1__IUriRuntimeClass = interface(IVector_1__IUriRuntimeClass_Base)
  ['{B13E7C58-8A01-5524-A397-45B4629C84C1}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.IUriRuntimeClass>
  // External 
  IVectorView_1__IUriRuntimeClass = interface(IInspectable)
  ['{D38FF558-9620-5253-B2C2-7B2F4B27AF6F}']
    function GetAt(index: Cardinal): IUriRuntimeClass; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IUriRuntimeClass; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIUriRuntimeClass): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Xaml.Controls.NotifyEventHandler
  // External 
  NotifyEventHandler = interface(IUnknown)
  ['{C2FDD1F8-7105-4A74-A109-DE29DFF56B98}']
    procedure Invoke(sender: IInspectable; e: INotifyEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Controls.INotifyEventArgs
  // External 
  INotifyEventArgs = interface(IInspectable)
  ['{AF0E05F7-C4B7-44C5-B09D-5CB7052B3A97}']
    function get_Value: HSTRING; safecall;
    property Value: HSTRING read get_Value;
  end;

  // Windows.UI.Xaml.Controls.WebViewNavigationFailedEventHandler
  // External 
  WebViewNavigationFailedEventHandler = interface(IUnknown)
  ['{A31EAFE1-41DC-47F8-AE22-9706C8F143D4}']
    procedure Invoke(sender: IInspectable; e: IWebViewNavigationFailedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.Controls.IWebViewNavigationFailedEventArgs
  // External 
  IWebViewNavigationFailedEventArgs = interface(IInspectable)
  ['{AF09609A-129C-4170-9E9C-E2CDF025DCA4}']
    function get_Uri: IUriRuntimeClass; safecall;
    function get_WebErrorStatus: WebErrorStatus; safecall;
    property Uri: IUriRuntimeClass read get_Uri;
    property WebErrorStatus_: WebErrorStatus read get_WebErrorStatus;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IDependencyObject>
  IVector_1__IDependencyObject_Base = interface(IInspectable)
  ['{771B857E-AB5C-5DB8-A021-397C92CDC44C}']
    function GetAt(index: Cardinal): IDependencyObject; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IDependencyObject; safecall;
    function IndexOf(value: IDependencyObject; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IDependencyObject); safecall;
    procedure InsertAt(index: Cardinal; value: IDependencyObject); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IDependencyObject); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIDependencyObject): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIDependencyObject); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.IDependencyObject>
  // External 
  IVector_1__IDependencyObject = interface(IVector_1__IDependencyObject_Base)
  ['{124D6648-5A01-567F-AEB6-71F8054793C2}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.IDependencyObject>
  // External 
  IVectorView_1__IDependencyObject = interface(IInspectable)
  ['{724BD94E-A094-5E1D-9BDA-8DE976BFB46D}']
    function GetAt(index: Cardinal): IDependencyObject; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IDependencyObject; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIDependencyObject): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.Maps.IMapControl,Object>
  TypedEventHandler_2__Maps_IMapControl__IInspectable_Delegate_Base = interface(IUnknown)
  ['{F250DD93-E636-51EF-BC93-EE78D431A6BD}']
    procedure Invoke(sender: Maps_IMapControl; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.Maps.IMapControl,Object>
  // External 
  TypedEventHandler_2__Maps_IMapControl__IInspectable = interface(TypedEventHandler_2__Maps_IMapControl__IInspectable_Delegate_Base)
  ['{56AB9FE2-9881-5BAC-9494-5B473FFDE5C9}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.Maps.IMapControl
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_Maps_MapControl)]
  Maps_IMapControl = interface(IInspectable)
  ['{42D0B851-5256-4747-9E6C-0D11E966141E}']
    function get_Center: IGeopoint; safecall;
    procedure put_Center(value: IGeopoint); safecall;
    function get_Children: IVector_1__IDependencyObject; safecall;
    function get_ColorScheme: Maps_MapColorScheme; safecall;
    procedure put_ColorScheme(value: Maps_MapColorScheme); safecall;
    function get_DesiredPitch: Double; safecall;
    procedure put_DesiredPitch(value: Double); safecall;
    function get_Heading: Double; safecall;
    procedure put_Heading(value: Double); safecall;
    function get_LandmarksVisible: Boolean; safecall;
    procedure put_LandmarksVisible(value: Boolean); safecall;
    function get_LoadingStatus: Maps_MapLoadingStatus; safecall;
    function get_MapServiceToken: HSTRING; safecall;
    procedure put_MapServiceToken(value: HSTRING); safecall;
    function get_MaxZoomLevel: Double; safecall;
    function get_MinZoomLevel: Double; safecall;
    function get_PedestrianFeaturesVisible: Boolean; safecall;
    procedure put_PedestrianFeaturesVisible(value: Boolean); safecall;
    function get_Pitch: Double; safecall;
    function get_Style: Maps_MapStyle; safecall;
    procedure put_Style(value: Maps_MapStyle); safecall;
    function get_TrafficFlowVisible: Boolean; safecall;
    procedure put_TrafficFlowVisible(value: Boolean); safecall;
    function get_TransformOrigin: TPointF; safecall;
    procedure put_TransformOrigin(value: TPointF); safecall;
    function get_WatermarkMode: Maps_MapWatermarkMode; safecall;
    procedure put_WatermarkMode(value: Maps_MapWatermarkMode); safecall;
    function get_ZoomLevel: Double; safecall;
    procedure put_ZoomLevel(value: Double); safecall;
    function get_MapElements: IVector_1__Maps_IMapElement; safecall;
    function get_Routes: IVector_1__Maps_IMapRouteView; safecall;
    function get_TileSources: IVector_1__Maps_IMapTileSource; safecall;
    function add_CenterChanged(handler: TypedEventHandler_2__Maps_IMapControl__IInspectable): EventRegistrationToken; safecall;
    procedure remove_CenterChanged(token: EventRegistrationToken); safecall;
    function add_HeadingChanged(handler: TypedEventHandler_2__Maps_IMapControl__IInspectable): EventRegistrationToken; safecall;
    procedure remove_HeadingChanged(token: EventRegistrationToken); safecall;
    function add_LoadingStatusChanged(handler: TypedEventHandler_2__Maps_IMapControl__IInspectable): EventRegistrationToken; safecall;
    procedure remove_LoadingStatusChanged(token: EventRegistrationToken); safecall;
    function add_MapDoubleTapped(handler: TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs): EventRegistrationToken; safecall;
    procedure remove_MapDoubleTapped(token: EventRegistrationToken); safecall;
    function add_MapHolding(handler: TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs): EventRegistrationToken; safecall;
    procedure remove_MapHolding(token: EventRegistrationToken); safecall;
    function add_MapTapped(handler: TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs): EventRegistrationToken; safecall;
    procedure remove_MapTapped(token: EventRegistrationToken); safecall;
    function add_PitchChanged(handler: TypedEventHandler_2__Maps_IMapControl__IInspectable): EventRegistrationToken; safecall;
    procedure remove_PitchChanged(token: EventRegistrationToken); safecall;
    function add_TransformOriginChanged(handler: TypedEventHandler_2__Maps_IMapControl__IInspectable): EventRegistrationToken; safecall;
    procedure remove_TransformOriginChanged(token: EventRegistrationToken); safecall;
    function add_ZoomLevelChanged(handler: TypedEventHandler_2__Maps_IMapControl__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ZoomLevelChanged(token: EventRegistrationToken); safecall;
    function FindMapElementsAtOffset(offset: TPointF): IVectorView_1__Maps_IMapElement; safecall;
    procedure GetLocationFromOffset(offset: TPointF; out location: IGeopoint); safecall;
    procedure GetOffsetFromLocation(location: IGeopoint; out offset: TPointF); safecall;
    procedure IsLocationInView(location: IGeopoint; out isInView: Boolean); safecall;
    function TrySetViewBoundsAsync(bounds: IGeoboundingBox; margin: IReference_1__Thickness; animation: Maps_MapAnimationKind): IAsyncOperation_1__Boolean; safecall;
    function TrySetViewAsync(center: IGeopoint): IAsyncOperation_1__Boolean; overload; safecall;
    function TrySetViewAsync(center: IGeopoint; zoomLevel: IReference_1__Double): IAsyncOperation_1__Boolean; overload; safecall;
    function TrySetViewAsync(center: IGeopoint; zoomLevel: IReference_1__Double; heading: IReference_1__Double; desiredPitch: IReference_1__Double): IAsyncOperation_1__Boolean; overload; safecall;
    function TrySetViewAsync(center: IGeopoint; zoomLevel: IReference_1__Double; heading: IReference_1__Double; desiredPitch: IReference_1__Double; animation: Maps_MapAnimationKind): IAsyncOperation_1__Boolean; overload; safecall;
    property Center: IGeopoint read get_Center write put_Center;
    property Children: IVector_1__IDependencyObject read get_Children;
    property ColorScheme: Maps_MapColorScheme read get_ColorScheme write put_ColorScheme;
    property DesiredPitch: Double read get_DesiredPitch write put_DesiredPitch;
    property Heading: Double read get_Heading write put_Heading;
    property LandmarksVisible: Boolean read get_LandmarksVisible write put_LandmarksVisible;
    property LoadingStatus: Maps_MapLoadingStatus read get_LoadingStatus;
    property MapElements: IVector_1__Maps_IMapElement read get_MapElements;
    property MapServiceToken: HSTRING read get_MapServiceToken write put_MapServiceToken;
    property MaxZoomLevel: Double read get_MaxZoomLevel;
    property MinZoomLevel: Double read get_MinZoomLevel;
    property PedestrianFeaturesVisible: Boolean read get_PedestrianFeaturesVisible write put_PedestrianFeaturesVisible;
    property Pitch: Double read get_Pitch;
    property Routes: IVector_1__Maps_IMapRouteView read get_Routes;
    property Style: Maps_MapStyle read get_Style write put_Style;
    property TileSources: IVector_1__Maps_IMapTileSource read get_TileSources;
    property TrafficFlowVisible: Boolean read get_TrafficFlowVisible write put_TrafficFlowVisible;
    property TransformOrigin: TPointF read get_TransformOrigin write put_TransformOrigin;
    property WatermarkMode: Maps_MapWatermarkMode read get_WatermarkMode write put_WatermarkMode;
    property ZoomLevel: Double read get_ZoomLevel write put_ZoomLevel;
  end;

  // DualAPI Interface
  // Windows.Devices.Geolocation.IGeopoint
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geopoint)]
  IGeopoint = interface(IInspectable)
  ['{6BFA00EB-E56E-49BB-9CAF-CBAA78A8BCEF}']
    function get_Position: BasicGeoposition; safecall;
    property Position: BasicGeoposition read get_Position;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapElement>
  IVector_1__Maps_IMapElement_Base = interface(IInspectable)
  ['{02773F2D-BB17-56FD-96CC-89F1C47F9E11}']
    function GetAt(index: Cardinal): Maps_IMapElement; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Maps_IMapElement; safecall;
    function IndexOf(value: Maps_IMapElement; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Maps_IMapElement); safecall;
    procedure InsertAt(index: Cardinal; value: Maps_IMapElement); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Maps_IMapElement); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapElement): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PMaps_IMapElement); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapElement>
  // External 
  IVector_1__Maps_IMapElement = interface(IVector_1__Maps_IMapElement_Base)
  ['{DA270E58-4B84-51E5-8BB7-C71840B130CD}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.Maps.IMapElement
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_Maps_MapElement)]
  Maps_IMapElement = interface(IInspectable)
  ['{D61FC4DF-B245-47F2-9AC2-43C058B1C903}']
    function get_ZIndex: Integer; safecall;
    procedure put_ZIndex(value: Integer); safecall;
    function get_Visible: Boolean; safecall;
    procedure put_Visible(value: Boolean); safecall;
    property Visible: Boolean read get_Visible write put_Visible;
    property ZIndex: Integer read get_ZIndex write put_ZIndex;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Maps.IMapElement>
  // External 
  IVectorView_1__Maps_IMapElement = interface(IInspectable)
  ['{0CDBEDE3-F142-5E92-9179-C2CB9F4486D5}']
    function GetAt(index: Cardinal): Maps_IMapElement; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Maps_IMapElement; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapElement): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapRouteView>
  IVector_1__Maps_IMapRouteView_Base = interface(IInspectable)
  ['{8CE513B4-0A7D-5553-B735-79CD5A7FEE3F}']
    function GetAt(index: Cardinal): Maps_IMapRouteView; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Maps_IMapRouteView; safecall;
    function IndexOf(value: Maps_IMapRouteView; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Maps_IMapRouteView); safecall;
    procedure InsertAt(index: Cardinal; value: Maps_IMapRouteView); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Maps_IMapRouteView); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapRouteView): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PMaps_IMapRouteView); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapRouteView>
  // External 
  IVector_1__Maps_IMapRouteView = interface(IVector_1__Maps_IMapRouteView_Base)
  ['{B9C6421E-5469-5DB5-A9DD-068380A831CF}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.Maps.IMapRouteView
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_Maps_MapRouteView)]
  Maps_IMapRouteView = interface(IInspectable)
  ['{740EAEC5-BACC-41E1-A67E-DD6513832049}']
    function get_RouteColor: Color; safecall;
    procedure put_RouteColor(value: Color); safecall;
    function get_OutlineColor: Color; safecall;
    procedure put_OutlineColor(value: Color); safecall;
    function get_Route: Maps_IMapRoute; safecall;
    property OutlineColor: Color read get_OutlineColor write put_OutlineColor;
    property Route: Maps_IMapRoute read get_Route;
    property RouteColor: Color read get_RouteColor write put_RouteColor;
  end;

  // Windows.Services.Maps.IMapRoute
  // External 
  Maps_IMapRoute = interface(IInspectable)
  ['{FB07B732-584D-4583-9C60-641FEA274349}']
    function get_BoundingBox: IGeoboundingBox; safecall;
    function get_LengthInMeters: Double; safecall;
    function get_EstimatedDuration: TimeSpan; safecall;
    function get_Path: IGeopath; safecall;
    function get_Legs: IVectorView_1__Maps_IMapRouteLeg; safecall;
    function get_IsTrafficBased: Boolean; safecall;
    property BoundingBox: IGeoboundingBox read get_BoundingBox;
    property EstimatedDuration: TimeSpan read get_EstimatedDuration;
    property IsTrafficBased: Boolean read get_IsTrafficBased;
    property Legs: IVectorView_1__Maps_IMapRouteLeg read get_Legs;
    property LengthInMeters: Double read get_LengthInMeters;
    property Path: IGeopath read get_Path;
  end;

  // DualAPI Interface
  // Windows.Devices.Geolocation.IGeoboundingBox
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_GeoboundingBox)]
  IGeoboundingBox = interface(IInspectable)
  ['{0896C80B-274F-43DA-9A06-CBFCDAEB4EC2}']
    function get_NorthwestCorner: BasicGeoposition; safecall;
    function get_SoutheastCorner: BasicGeoposition; safecall;
    function get_Center: BasicGeoposition; safecall;
    function get_MinAltitude: Double; safecall;
    function get_MaxAltitude: Double; safecall;
    property Center: BasicGeoposition read get_Center;
    property MaxAltitude: Double read get_MaxAltitude;
    property MinAltitude: Double read get_MinAltitude;
    property NorthwestCorner: BasicGeoposition read get_NorthwestCorner;
    property SoutheastCorner: BasicGeoposition read get_SoutheastCorner;
  end;

  // DualAPI Interface
  // Windows.Devices.Geolocation.IGeopath
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geopath)]
  IGeopath = interface(IInspectable)
  ['{E53FD7B9-2DA4-4714-A652-DE8593289898}']
    function get_Positions: IVectorView_1__BasicGeoposition; safecall;
    property Positions: IVectorView_1__BasicGeoposition read get_Positions;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.BasicGeoposition>
  // External 
  IVectorView_1__BasicGeoposition = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): BasicGeoposition; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: BasicGeoposition; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PBasicGeoposition): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.IMapRouteLeg>
  // External 
  IVectorView_1__Maps_IMapRouteLeg = interface(IInspectable)
  ['{AAFAE329-57EE-5FD2-B0B9-C971A3E143EB}']
    function GetAt(index: Cardinal): Maps_IMapRouteLeg; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Maps_IMapRouteLeg; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapRouteLeg): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Services.Maps.IMapRouteLeg
  // External 
  Maps_IMapRouteLeg = interface(IInspectable)
  ['{96F8B2F6-5BBA-4D17-9DB6-1A263FEC7471}']
    function get_BoundingBox: IGeoboundingBox; safecall;
    function get_Path: IGeopath; safecall;
    function get_LengthInMeters: Double; safecall;
    function get_EstimatedDuration: TimeSpan; safecall;
    function get_Maneuvers: IVectorView_1__Maps_IMapRouteManeuver; safecall;
    property BoundingBox: IGeoboundingBox read get_BoundingBox;
    property EstimatedDuration: TimeSpan read get_EstimatedDuration;
    property LengthInMeters: Double read get_LengthInMeters;
    property Maneuvers: IVectorView_1__Maps_IMapRouteManeuver read get_Maneuvers;
    property Path: IGeopath read get_Path;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.IMapRouteManeuver>
  // External 
  IVectorView_1__Maps_IMapRouteManeuver = interface(IInspectable)
  ['{9DA15A6F-DDEA-5945-9863-77745321D47D}']
    function GetAt(index: Cardinal): Maps_IMapRouteManeuver; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Maps_IMapRouteManeuver; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapRouteManeuver): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Services.Maps.IMapRouteManeuver
  // External 
  Maps_IMapRouteManeuver = interface(IInspectable)
  ['{ED5C17F0-A6AB-4D65-A086-FA8A7E340DF2}']
    function get_StartingPoint: IGeopoint; safecall;
    function get_LengthInMeters: Double; safecall;
    function get_InstructionText: HSTRING; safecall;
    function get_Kind: Maps_MapRouteManeuverKind; safecall;
    function get_ExitNumber: HSTRING; safecall;
    function get_ManeuverNotices: Maps_MapManeuverNotices; safecall;
    property ExitNumber: HSTRING read get_ExitNumber;
    property InstructionText: HSTRING read get_InstructionText;
    property Kind: Maps_MapRouteManeuverKind read get_Kind;
    property LengthInMeters: Double read get_LengthInMeters;
    property ManeuverNotices: Maps_MapManeuverNotices read get_ManeuverNotices;
    property StartingPoint: IGeopoint read get_StartingPoint;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Maps.IMapRouteView>
  // External 
  IVectorView_1__Maps_IMapRouteView = interface(IInspectable)
  ['{BA2B23BC-DD45-5822-8BA2-4828B7712FF1}']
    function GetAt(index: Cardinal): Maps_IMapRouteView; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Maps_IMapRouteView; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapRouteView): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapTileSource>
  IVector_1__Maps_IMapTileSource_Base = interface(IInspectable)
  ['{19F78A46-2C65-5F94-BF5B-963347A0A318}']
    function GetAt(index: Cardinal): Maps_IMapTileSource; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Maps_IMapTileSource; safecall;
    function IndexOf(value: Maps_IMapTileSource; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Maps_IMapTileSource); safecall;
    procedure InsertAt(index: Cardinal; value: Maps_IMapTileSource); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Maps_IMapTileSource); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapTileSource): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PMaps_IMapTileSource); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Controls.Maps.IMapTileSource>
  // External 
  IVector_1__Maps_IMapTileSource = interface(IVector_1__Maps_IMapTileSource_Base)
  ['{4126C75F-4265-515A-B4A8-4A93F2FD3330}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.Maps.IMapTileSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_Maps_MapTileSource)]
  Maps_IMapTileSource = interface(IInspectable)
  ['{88A76E4E-2FDF-4567-9255-1100519C8D62}']
    function get_DataSource: Maps_IMapTileDataSource; safecall;
    procedure put_DataSource(value: Maps_IMapTileDataSource); safecall;
    function get_Layer: Maps_MapTileLayer; safecall;
    procedure put_Layer(value: Maps_MapTileLayer); safecall;
    function get_ZoomLevelRange: Maps_MapZoomLevelRange; safecall;
    procedure put_ZoomLevelRange(value: Maps_MapZoomLevelRange); safecall;
    function get_Bounds: IGeoboundingBox; safecall;
    procedure put_Bounds(value: IGeoboundingBox); safecall;
    function get_AllowOverstretch: Boolean; safecall;
    procedure put_AllowOverstretch(value: Boolean); safecall;
    function get_IsFadingEnabled: Boolean; safecall;
    procedure put_IsFadingEnabled(value: Boolean); safecall;
    function get_IsTransparencyEnabled: Boolean; safecall;
    procedure put_IsTransparencyEnabled(value: Boolean); safecall;
    function get_IsRetryEnabled: Boolean; safecall;
    procedure put_IsRetryEnabled(value: Boolean); safecall;
    function get_ZIndex: Integer; safecall;
    procedure put_ZIndex(value: Integer); safecall;
    function get_TilePixelSize: Integer; safecall;
    procedure put_TilePixelSize(value: Integer); safecall;
    function get_Visible: Boolean; safecall;
    procedure put_Visible(value: Boolean); safecall;
    property AllowOverstretch: Boolean read get_AllowOverstretch write put_AllowOverstretch;
    property Bounds: IGeoboundingBox read get_Bounds write put_Bounds;
    property DataSource: Maps_IMapTileDataSource read get_DataSource write put_DataSource;
    property IsFadingEnabled: Boolean read get_IsFadingEnabled write put_IsFadingEnabled;
    property IsRetryEnabled: Boolean read get_IsRetryEnabled write put_IsRetryEnabled;
    property IsTransparencyEnabled: Boolean read get_IsTransparencyEnabled write put_IsTransparencyEnabled;
    property Layer: Maps_MapTileLayer read get_Layer write put_Layer;
    property TilePixelSize: Integer read get_TilePixelSize write put_TilePixelSize;
    property Visible: Boolean read get_Visible write put_Visible;
    property ZIndex: Integer read get_ZIndex write put_ZIndex;
    property ZoomLevelRange: Maps_MapZoomLevelRange read get_ZoomLevelRange write put_ZoomLevelRange;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.Maps.IMapTileDataSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_Maps_MapTileDataSource)]
  Maps_IMapTileDataSource = interface(IInspectable)
  ['{C03D9F5E-BE1F-4C69-9969-79467A513C38}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Maps.IMapTileSource>
  // External 
  IVectorView_1__Maps_IMapTileSource = interface(IInspectable)
  ['{5DF6D860-7B19-56DE-8809-AF37F8D7CCC9}']
    function GetAt(index: Cardinal): Maps_IMapTileSource; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Maps_IMapTileSource; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapTileSource): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.Maps.IMapControl,Windows.UI.Xaml.Controls.Maps.IMapInputEventArgs>
  TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs_Delegate_Base = interface(IUnknown)
  ['{B92F6816-4D0A-52C2-9868-94B5C942007E}']
    procedure Invoke(sender: Maps_IMapControl; args: Maps_IMapInputEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.Maps.IMapControl,Windows.UI.Xaml.Controls.Maps.IMapInputEventArgs>
  // External 
  TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs = interface(TypedEventHandler_2__Maps_IMapControl__Maps_IMapInputEventArgs_Delegate_Base)
  ['{0A7591D5-4651-54C4-B11B-13070121199B}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.Maps.IMapInputEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_Maps_MapInputEventArgs)]
  Maps_IMapInputEventArgs = interface(IInspectable)
  ['{9FC503A0-A8A2-4394-92E9-2247764F2F49}']
    function get_Position: TPointF; safecall;
    function get_Location: IGeopoint; safecall;
    property Location: IGeopoint read get_Location;
    property Position: TPointF read get_Position;
  end;

  // Windows.Foundation.IReference`1<Windows.UI.Xaml.Thickness>
  // External 
  IReference_1__Thickness = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Thickness; safecall;
    property Value: Thickness read get_Value;
  end;

  // Windows.UI.Xaml.Input.IProcessKeyboardAcceleratorEventArgs
  // External 
  Input_IProcessKeyboardAcceleratorEventArgs = interface(IInspectable)
  ['{FB79C216-972B-440C-9B83-2B4198DCF09D}']
    function get_Key: VirtualKey; safecall;
    function get_Modifiers: VirtualKeyModifiers; safecall;
    function get_Handled: Boolean; safecall;
    procedure put_Handled(value: Boolean); safecall;
    property Handled: Boolean read get_Handled write put_Handled;
    property Key: VirtualKey read get_Key;
    property Modifiers: VirtualKeyModifiers read get_Modifiers;
  end;

  // Windows.UI.Xaml.IXamlRoot
  // External 
  IXamlRoot = interface(IInspectable)
  ['{34B50756-1696-5B6D-8E9B-C71464CCAD5A}']
    function get_Content: IUIElement; safecall;
    function get_Size: TSizeF; safecall;
    function get_RasterizationScale: Double; safecall;
    function get_IsHostVisible: Boolean; safecall;
    function get_UIContext: IUIContext; safecall;
    function add_Changed(handler: TypedEventHandler_2__IXamlRoot__IXamlRootChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Changed(token: EventRegistrationToken); safecall;
    property Content: IUIElement read get_Content;
    property IsHostVisible: Boolean read get_IsHostVisible;
    property RasterizationScale: Double read get_RasterizationScale;
    property Size: TSizeF read get_Size;
    property UIContext: IUIContext read get_UIContext;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.IXamlRoot,Windows.UI.Xaml.IXamlRootChangedEventArgs>
  // External 
  TypedEventHandler_2__IXamlRoot__IXamlRootChangedEventArgs = interface(IUnknown)
  ['{53CE076B-F659-5427-8924-F263E278F556}']
    procedure Invoke(sender: IXamlRoot; args: IXamlRootChangedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.IXamlRootChangedEventArgs
  // External 
  IXamlRootChangedEventArgs = interface(IInspectable)
  ['{92D71C21-D23C-5A17-BCB8-001504B6BB19}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Animation.IConnectedAnimation,Object>
  TypedEventHandler_2__Animation_IConnectedAnimation__IInspectable_Delegate_Base = interface(IUnknown)
  ['{44CAA9EA-7598-517A-B78E-ABD20D93D587}']
    procedure Invoke(sender: Animation_IConnectedAnimation; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Animation.IConnectedAnimation,Object>
  // External 
  TypedEventHandler_2__Animation_IConnectedAnimation__IInspectable = interface(TypedEventHandler_2__Animation_IConnectedAnimation__IInspectable_Delegate_Base)
  ['{630C7634-8A39-5835-8193-FF2AE7E94FA3}']
  end;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimation
  // External 
  Animation_IConnectedAnimation = interface(IInspectable)
  ['{3518628C-F387-4C25-AC98-44E86C3CADF0}']
    function add_Completed(handler: TypedEventHandler_2__Animation_IConnectedAnimation__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Completed(token: EventRegistrationToken); safecall;
    function TryStart(destination: IUIElement): Boolean; safecall;
    procedure Cancel; safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IMediaElement
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_MediaElement)]
  IMediaElement = interface(IInspectable)
  ['{A38ED2CF-13DE-4299-ADE2-AE18F74ED353}']
    function get_PosterSource: IImageSource; safecall;
    procedure put_PosterSource(value: IImageSource); safecall;
    function get_Source: IUriRuntimeClass; safecall;
    procedure put_Source(value: IUriRuntimeClass); safecall;
    function get_IsMuted: Boolean; safecall;
    procedure put_IsMuted(value: Boolean); safecall;
    function get_IsAudioOnly: Boolean; safecall;
    function get_AutoPlay: Boolean; safecall;
    procedure put_AutoPlay(value: Boolean); safecall;
    function get_Volume: Double; safecall;
    procedure put_Volume(value: Double); safecall;
    function get_Balance: Double; safecall;
    procedure put_Balance(value: Double); safecall;
    function get_NaturalVideoHeight: Integer; safecall;
    function get_NaturalVideoWidth: Integer; safecall;
    function get_NaturalDuration: Duration; safecall;
    function get_Position: TimeSpan; safecall;
    procedure put_Position(value: TimeSpan); safecall;
    function get_DownloadProgress: Double; safecall;
    function get_BufferingProgress: Double; safecall;
    function get_DownloadProgressOffset: Double; safecall;
    function get_CurrentState: MediaElementState; safecall;
    function get_Markers: IVector_1__ITimelineMarker; safecall;
    function get_CanSeek: Boolean; safecall;
    function get_CanPause: Boolean; safecall;
    function get_AudioStreamCount: Integer; safecall;
    function get_AudioStreamIndex: IReference_1__Integer; safecall;
    procedure put_AudioStreamIndex(value: IReference_1__Integer); safecall;
    function get_PlaybackRate: Double; safecall;
    procedure put_PlaybackRate(value: Double); safecall;
    function get_IsLooping: Boolean; safecall;
    procedure put_IsLooping(value: Boolean); safecall;
    function get_PlayToSource: PlayTo_IPlayToSource; safecall;
    function get_DefaultPlaybackRate: Double; safecall;
    procedure put_DefaultPlaybackRate(value: Double); safecall;
    function get_AspectRatioWidth: Integer; safecall;
    function get_AspectRatioHeight: Integer; safecall;
    function get_RealTimePlayback: Boolean; safecall;
    procedure put_RealTimePlayback(value: Boolean); safecall;
    function get_AudioCategory: AudioCategory; safecall;
    procedure put_AudioCategory(value: AudioCategory); safecall;
    function get_AudioDeviceType: AudioDeviceType; safecall;
    procedure put_AudioDeviceType(value: AudioDeviceType); safecall;
    function get_ProtectionManager: Protection_IMediaProtectionManager; safecall;
    procedure put_ProtectionManager(value: Protection_IMediaProtectionManager); safecall;
    function get_Stereo3DVideoPackingMode: Stereo3DVideoPackingMode; safecall;
    procedure put_Stereo3DVideoPackingMode(value: Stereo3DVideoPackingMode); safecall;
    function get_Stereo3DVideoRenderMode: Stereo3DVideoRenderMode; safecall;
    procedure put_Stereo3DVideoRenderMode(value: Stereo3DVideoRenderMode); safecall;
    function get_IsStereo3DVideo: Boolean; safecall;
    function add_MediaOpened(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_MediaOpened(token: EventRegistrationToken); safecall;
    function add_MediaEnded(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_MediaEnded(token: EventRegistrationToken); safecall;
    function add_MediaFailed(handler: ExceptionRoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_MediaFailed(token: EventRegistrationToken); safecall;
    function add_DownloadProgressChanged(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_DownloadProgressChanged(token: EventRegistrationToken); safecall;
    function add_BufferingProgressChanged(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_BufferingProgressChanged(token: EventRegistrationToken); safecall;
    function add_CurrentStateChanged(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_CurrentStateChanged(token: EventRegistrationToken); safecall;
    function add_MarkerReached(handler: TimelineMarkerRoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_MarkerReached(token: EventRegistrationToken); safecall;
    function add_RateChanged(handler: RateChangedRoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_RateChanged(token: EventRegistrationToken); safecall;
    function add_VolumeChanged(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_VolumeChanged(token: EventRegistrationToken); safecall;
    function add_SeekCompleted(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_SeekCompleted(token: EventRegistrationToken); safecall;
    procedure Stop; safecall;
    procedure Play; safecall;
    procedure Pause; safecall;
    function CanPlayType(&type: HSTRING): MediaCanPlayResponse; safecall;
    procedure SetSource(stream: IRandomAccessStream; mimeType: HSTRING); safecall;
    function GetAudioStreamLanguage(index: IReference_1__Integer): HSTRING; safecall;
    procedure AddAudioEffect(effectID: HSTRING; effectOptional: Boolean; effectConfiguration: IPropertySet); safecall;
    procedure AddVideoEffect(effectID: HSTRING; effectOptional: Boolean; effectConfiguration: IPropertySet); safecall;
    procedure RemoveAllEffects; safecall;
    function get_ActualStereo3DVideoPackingMode: Stereo3DVideoPackingMode; safecall;
    property ActualStereo3DVideoPackingMode: Stereo3DVideoPackingMode read get_ActualStereo3DVideoPackingMode;
    property AspectRatioHeight: Integer read get_AspectRatioHeight;
    property AspectRatioWidth: Integer read get_AspectRatioWidth;
    property AudioCategory_: AudioCategory read get_AudioCategory write put_AudioCategory;
    property AudioDeviceType_: AudioDeviceType read get_AudioDeviceType write put_AudioDeviceType;
    property AudioStreamCount: Integer read get_AudioStreamCount;
    property AudioStreamIndex: IReference_1__Integer read get_AudioStreamIndex write put_AudioStreamIndex;
    property AutoPlay: Boolean read get_AutoPlay write put_AutoPlay;
    property Balance: Double read get_Balance write put_Balance;
    property BufferingProgress: Double read get_BufferingProgress;
    property CanPause: Boolean read get_CanPause;
    property CanSeek: Boolean read get_CanSeek;
    property CurrentState: MediaElementState read get_CurrentState;
    property DefaultPlaybackRate: Double read get_DefaultPlaybackRate write put_DefaultPlaybackRate;
    property DownloadProgress: Double read get_DownloadProgress;
    property DownloadProgressOffset: Double read get_DownloadProgressOffset;
    property IsAudioOnly: Boolean read get_IsAudioOnly;
    property IsLooping: Boolean read get_IsLooping write put_IsLooping;
    property IsMuted: Boolean read get_IsMuted write put_IsMuted;
    property IsStereo3DVideo: Boolean read get_IsStereo3DVideo;
    property Markers: IVector_1__ITimelineMarker read get_Markers;
    property NaturalDuration: Duration read get_NaturalDuration;
    property NaturalVideoHeight: Integer read get_NaturalVideoHeight;
    property NaturalVideoWidth: Integer read get_NaturalVideoWidth;
    property PlayToSource: PlayTo_IPlayToSource read get_PlayToSource;
    property PlaybackRate: Double read get_PlaybackRate write put_PlaybackRate;
    property Position: TimeSpan read get_Position write put_Position;
    property PosterSource: IImageSource read get_PosterSource write put_PosterSource;
    property ProtectionManager: Protection_IMediaProtectionManager read get_ProtectionManager write put_ProtectionManager;
    property RealTimePlayback: Boolean read get_RealTimePlayback write put_RealTimePlayback;
    property Source: IUriRuntimeClass read get_Source write put_Source;
    property Stereo3DVideoPackingMode_: Stereo3DVideoPackingMode read get_Stereo3DVideoPackingMode write put_Stereo3DVideoPackingMode;
    property Stereo3DVideoRenderMode_: Stereo3DVideoRenderMode read get_Stereo3DVideoRenderMode write put_Stereo3DVideoRenderMode;
    property Volume: Double read get_Volume write put_Volume;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.IImageSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ImageSource)]
  IImageSource = interface(IInspectable)
  ['{737EF309-EA41-4D96-A71C-98E98EFCAB07}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IVector_1__ITimelineMarker_Base = interface(IInspectable)
  ['{B2D8DDB4-D2B8-5930-942D-26118756BD46}']
    function GetAt(index: Cardinal): ITimelineMarker; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ITimelineMarker; safecall;
    function IndexOf(value: ITimelineMarker; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ITimelineMarker); safecall;
    procedure InsertAt(index: Cardinal; value: ITimelineMarker); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ITimelineMarker); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PITimelineMarker): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PITimelineMarker); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITimelineMarker>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TimelineMarkerCollection)]
  IVector_1__ITimelineMarker = interface(IVector_1__ITimelineMarker_Base)
  ['{A3702B57-22DD-577D-9127-3FFC83541E72}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.ITimelineMarker
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TimelineMarker)]
  ITimelineMarker = interface(IInspectable)
  ['{A68EF02D-45BA-4E50-8CAD-AAEA3A227AF5}']
    function get_Time: TimeSpan; safecall;
    procedure put_Time(value: TimeSpan); safecall;
    function get_Type: HSTRING; safecall;
    procedure put_Type(value: HSTRING); safecall;
    function get_Text: HSTRING; safecall;
    procedure put_Text(value: HSTRING); safecall;
    property Text: HSTRING read get_Text write put_Text;
    property Time: TimeSpan read get_Time write put_Time;
    property &Type: HSTRING read get_Type write put_Type;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.ITimelineMarker>
  // External 
  IVectorView_1__ITimelineMarker = interface(IInspectable)
  ['{3610381A-2CA3-5419-A8E5-90617DAB6DA2}']
    function GetAt(index: Cardinal): ITimelineMarker; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ITimelineMarker; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PITimelineMarker): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.PlayTo.IPlayToSource
  // External 
  PlayTo_IPlayToSource = interface(IInspectable)
  ['{7F138A08-FBB7-4B09-8356-AA5F4E335C31}']
    function get_Connection: PlayTo_IPlayToConnection; safecall;
    function get_Next: PlayTo_IPlayToSource; safecall;
    procedure put_Next(value: PlayTo_IPlayToSource); safecall;
    procedure PlayNext; safecall;
    property Connection: PlayTo_IPlayToConnection read get_Connection;
    property Next: PlayTo_IPlayToSource read get_Next write put_Next;
  end;

  // Windows.Media.PlayTo.IPlayToConnection
  // External 
  PlayTo_IPlayToConnection = interface(IInspectable)
  ['{112FBFC8-F235-4FDE-8D41-9BF27C9E9A40}']
    function get_State: PlayTo_PlayToConnectionState; safecall;
    function add_StateChanged(handler: TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionStateChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StateChanged(token: EventRegistrationToken); safecall;
    function add_Transferred(handler: TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionTransferredEventArgs): EventRegistrationToken; safecall;
    procedure remove_Transferred(token: EventRegistrationToken); safecall;
    function add_Error(handler: TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionErrorEventArgs): EventRegistrationToken; safecall;
    procedure remove_Error(token: EventRegistrationToken); safecall;
    property State: PlayTo_PlayToConnectionState read get_State;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionStateChangedEventArgs>
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionStateChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{DEF77C1C-9363-5B47-B9F8-A236C50D372E}']
    procedure Invoke(sender: PlayTo_IPlayToConnection; args: PlayTo_IPlayToConnectionStateChangedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionStateChangedEventArgs>
  // External 
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionStateChangedEventArgs = interface(TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionStateChangedEventArgs_Delegate_Base)
  ['{C246A35A-7FAB-5866-919C-39A7E63F106D}']
  end;

  // Windows.Media.PlayTo.IPlayToConnectionStateChangedEventArgs
  // External 
  PlayTo_IPlayToConnectionStateChangedEventArgs = interface(IInspectable)
  ['{68C4B50F-0C20-4980-8602-58C62238D423}']
    function get_PreviousState: PlayTo_PlayToConnectionState; safecall;
    function get_CurrentState: PlayTo_PlayToConnectionState; safecall;
    property CurrentState: PlayTo_PlayToConnectionState read get_CurrentState;
    property PreviousState: PlayTo_PlayToConnectionState read get_PreviousState;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionTransferredEventArgs>
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionTransferredEventArgs_Delegate_Base = interface(IUnknown)
  ['{7278E7F2-0360-5571-8C3E-EA032767C473}']
    procedure Invoke(sender: PlayTo_IPlayToConnection; args: PlayTo_IPlayToConnectionTransferredEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionTransferredEventArgs>
  // External 
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionTransferredEventArgs = interface(TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionTransferredEventArgs_Delegate_Base)
  ['{601EB42D-31E7-531E-B27B-2CDB9DF21F17}']
  end;

  // Windows.Media.PlayTo.IPlayToConnectionTransferredEventArgs
  // External 
  PlayTo_IPlayToConnectionTransferredEventArgs = interface(IInspectable)
  ['{FAE3193A-0683-47D9-8DF0-18CBB48984D8}']
    function get_PreviousSource: PlayTo_IPlayToSource; safecall;
    function get_CurrentSource: PlayTo_IPlayToSource; safecall;
    property CurrentSource: PlayTo_IPlayToSource read get_CurrentSource;
    property PreviousSource: PlayTo_IPlayToSource read get_PreviousSource;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionErrorEventArgs>
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionErrorEventArgs_Delegate_Base = interface(IUnknown)
  ['{8F599F3D-3655-5993-A119-741EC4EE6E42}']
    procedure Invoke(sender: PlayTo_IPlayToConnection; args: PlayTo_IPlayToConnectionErrorEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.PlayTo.IPlayToConnection,Windows.Media.PlayTo.IPlayToConnectionErrorEventArgs>
  // External 
  TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionErrorEventArgs = interface(TypedEventHandler_2__PlayTo_IPlayToConnection__PlayTo_IPlayToConnectionErrorEventArgs_Delegate_Base)
  ['{B880DB65-D565-59F9-A257-B428B2529F6D}']
  end;

  // Windows.Media.PlayTo.IPlayToConnectionErrorEventArgs
  // External 
  PlayTo_IPlayToConnectionErrorEventArgs = interface(IInspectable)
  ['{BF5EADA6-88E6-445F-9D40-D9B9F8939896}']
    function get_Code: PlayTo_PlayToConnectionError; safecall;
    function get_Message: HSTRING; safecall;
    property Code: PlayTo_PlayToConnectionError read get_Code;
    property &Message: HSTRING read get_Message;
  end;

  // Windows.Media.Protection.IMediaProtectionManager
  // External 
  Protection_IMediaProtectionManager = interface(IInspectable)
  ['{45694947-C741-434B-A79E-474C12D93D2F}']
    function add_ServiceRequested(handler: Protection_ServiceRequestedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ServiceRequested(cookie: EventRegistrationToken); safecall;
    function add_RebootNeeded(handler: Protection_RebootNeededEventHandler): EventRegistrationToken; safecall;
    procedure remove_RebootNeeded(cookie: EventRegistrationToken); safecall;
    function add_ComponentLoadFailed(handler: Protection_ComponentLoadFailedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ComponentLoadFailed(cookie: EventRegistrationToken); safecall;
    function get_Properties: IPropertySet; safecall;
    property Properties: IPropertySet read get_Properties;
  end;

  // Windows.Media.Protection.ServiceRequestedEventHandler
  // External 
  Protection_ServiceRequestedEventHandler = interface(IUnknown)
  ['{D2D690BA-CAC9-48E1-95C0-D38495A84055}']
    procedure Invoke(sender: Protection_IMediaProtectionManager; e: Protection_IServiceRequestedEventArgs); safecall;
  end;

  // Windows.Media.Protection.IServiceRequestedEventArgs
  // External 
  Protection_IServiceRequestedEventArgs = interface(IInspectable)
  ['{34283BAF-ABB4-4FC1-BD89-93F106573A49}']
    function get_Request: Protection_IMediaProtectionServiceRequest; safecall;
    function get_Completion: Protection_IMediaProtectionServiceCompletion; safecall;
    property Completion: Protection_IMediaProtectionServiceCompletion read get_Completion;
    property Request: Protection_IMediaProtectionServiceRequest read get_Request;
  end;

  // Windows.Media.Protection.IMediaProtectionServiceRequest
  // External 
  Protection_IMediaProtectionServiceRequest = interface(IInspectable)
  ['{B1DE0EA6-2094-478D-87A4-8B95200F85C6}']
    function get_ProtectionSystem: TGuid; safecall;
    function get_Type: TGuid; safecall;
    property ProtectionSystem: TGuid read get_ProtectionSystem;
    property &Type: TGuid read get_Type;
  end;

  // Windows.Media.Protection.IMediaProtectionServiceCompletion
  // External 
  Protection_IMediaProtectionServiceCompletion = interface(IInspectable)
  ['{8B5CCA18-CFD5-44EE-A2ED-DF76010C14B5}']
    procedure Complete(success: Boolean); safecall;
  end;

  // Windows.Media.Protection.RebootNeededEventHandler
  // External 
  Protection_RebootNeededEventHandler = interface(IUnknown)
  ['{64E12A45-973B-4A3A-B260-91898A49A82C}']
    procedure Invoke(sender: Protection_IMediaProtectionManager); safecall;
  end;

  // Windows.Media.Protection.ComponentLoadFailedEventHandler
  // External 
  Protection_ComponentLoadFailedEventHandler = interface(IUnknown)
  ['{95DA643C-6DB9-424B-86CA-091AF432081C}']
    procedure Invoke(sender: Protection_IMediaProtectionManager; e: Protection_IComponentLoadFailedEventArgs); safecall;
  end;

  // Windows.Media.Protection.IComponentLoadFailedEventArgs
  // External 
  Protection_IComponentLoadFailedEventArgs = interface(IInspectable)
  ['{95972E93-7746-417E-8495-F031BBC5862C}']
    function get_Information: Protection_IRevocationAndRenewalInformation; safecall;
    function get_Completion: Protection_IMediaProtectionServiceCompletion; safecall;
    property Completion: Protection_IMediaProtectionServiceCompletion read get_Completion;
    property Information: Protection_IRevocationAndRenewalInformation read get_Information;
  end;

  // Windows.Media.Protection.IRevocationAndRenewalInformation
  // External 
  Protection_IRevocationAndRenewalInformation = interface(IInspectable)
  ['{F3A1937B-2501-439E-A6E7-6FC95E175FCF}']
    function get_Items: IVector_1__Protection_IRevocationAndRenewalItem; safecall;
    property Items: IVector_1__Protection_IRevocationAndRenewalItem read get_Items;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Protection.IRevocationAndRenewalItem>
  IVector_1__Protection_IRevocationAndRenewalItem_Base = interface(IInspectable)
  ['{3623CC0C-C765-57FB-967D-C7CB6097BD78}']
    function GetAt(index: Cardinal): Protection_IRevocationAndRenewalItem; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Protection_IRevocationAndRenewalItem; safecall;
    function IndexOf(value: Protection_IRevocationAndRenewalItem; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Protection_IRevocationAndRenewalItem); safecall;
    procedure InsertAt(index: Cardinal; value: Protection_IRevocationAndRenewalItem); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Protection_IRevocationAndRenewalItem); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProtection_IRevocationAndRenewalItem): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PProtection_IRevocationAndRenewalItem); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Protection.IRevocationAndRenewalItem>
  // External 
  IVector_1__Protection_IRevocationAndRenewalItem = interface(IVector_1__Protection_IRevocationAndRenewalItem_Base)
  ['{982757F8-7839-5622-B031-72D79B5C1684}']
  end;

  // Windows.Media.Protection.IRevocationAndRenewalItem
  // External 
  Protection_IRevocationAndRenewalItem = interface(IInspectable)
  ['{3099C20C-3CF0-49EA-902D-CAF32D2DDE2C}']
    function get_Reasons: Protection_RevocationAndRenewalReasons; safecall;
    function get_HeaderHash: HSTRING; safecall;
    function get_PublicKeyHash: HSTRING; safecall;
    function get_Name: HSTRING; safecall;
    function get_RenewalId: HSTRING; safecall;
    property HeaderHash: HSTRING read get_HeaderHash;
    property Name: HSTRING read get_Name;
    property PublicKeyHash: HSTRING read get_PublicKeyHash;
    property Reasons: Protection_RevocationAndRenewalReasons read get_Reasons;
    property RenewalId: HSTRING read get_RenewalId;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Protection.IRevocationAndRenewalItem>
  // External 
  IVectorView_1__Protection_IRevocationAndRenewalItem = interface(IInspectable)
  ['{D4BED0AA-0377-58AB-829C-201E1283ABBE}']
    function GetAt(index: Cardinal): Protection_IRevocationAndRenewalItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Protection_IRevocationAndRenewalItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProtection_IRevocationAndRenewalItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Xaml.Media.TimelineMarkerRoutedEventHandler
  // External 
  TimelineMarkerRoutedEventHandler = interface(IUnknown)
  ['{72E2FA9C-6DEA-4CBE-A159-06CE95FBECED}']
    procedure Invoke(sender: IInspectable; e: ITimelineMarkerRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.ITimelineMarkerRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TimelineMarkerRoutedEventArgs)]
  ITimelineMarkerRoutedEventArgs = interface(IInspectable)
  ['{7C3B3EF3-2C88-4D9C-99B6-46CDBD48D4C1}']
    function get_Marker: ITimelineMarker; safecall;
    procedure put_Marker(value: ITimelineMarker); safecall;
    property Marker: ITimelineMarker read get_Marker write put_Marker;
  end;

  // Windows.UI.Xaml.Media.RateChangedRoutedEventHandler
  // External 
  RateChangedRoutedEventHandler = interface(IUnknown)
  ['{08E9A257-AE05-489B-8839-28C6225D2349}']
    procedure Invoke(sender: IInspectable; e: IRateChangedRoutedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Media.IRateChangedRoutedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RateChangedRoutedEventArgs)]
  IRateChangedRoutedEventArgs = interface(IInspectable)
  ['{9016AA6F-3CA8-4C80-8E2F-8851A68F131F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.IUIElement>
  IIterable_1__IUIElement_Base = interface(IInspectable)
  ['{42E26AE1-D357-57E8-BB48-F75C9FF69D91}']
    function First: IIterator_1__IUIElement; safecall;
  end;
  // DualAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.IUIElement>
  // External 
  IIterable_1__IUIElement = interface(IIterable_1__IUIElement_Base)
  ['{F685720D-571B-56A5-AA22-80702EAD5D5C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.IUIElement>
  IIterator_1__IUIElement_Base = interface(IInspectable)
  ['{1D1F9D60-D53B-57F7-B144-8F7C487846E8}']
    function get_Current: IUIElement; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIUIElement): Cardinal; safecall;
    property Current: IUIElement read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.IUIElement>
  // External 
  IIterator_1__IUIElement = interface(IIterator_1__IUIElement_Base)
  ['{C9E7BD02-C114-5BAF-904B-7D58C4B49CBF}']
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IListViewBase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_ListViewBase)]
  IListViewBase = interface(IInspectable)
  ['{3D0813BA-6890-4537-BFE5-796D9458EDD6}']
    function get_SelectedItems: IVector_1__IInspectable; safecall;
    function get_SelectionMode: ListViewSelectionMode; safecall;
    procedure put_SelectionMode(value: ListViewSelectionMode); safecall;
    function get_IsSwipeEnabled: Boolean; safecall;
    procedure put_IsSwipeEnabled(value: Boolean); safecall;
    function get_CanDragItems: Boolean; safecall;
    procedure put_CanDragItems(value: Boolean); safecall;
    function get_CanReorderItems: Boolean; safecall;
    procedure put_CanReorderItems(value: Boolean); safecall;
    function get_IsItemClickEnabled: Boolean; safecall;
    procedure put_IsItemClickEnabled(value: Boolean); safecall;
    function get_DataFetchSize: Double; safecall;
    procedure put_DataFetchSize(value: Double); safecall;
    function get_IncrementalLoadingThreshold: Double; safecall;
    procedure put_IncrementalLoadingThreshold(value: Double); safecall;
    function get_IncrementalLoadingTrigger: IncrementalLoadingTrigger; safecall;
    procedure put_IncrementalLoadingTrigger(value: IncrementalLoadingTrigger); safecall;
    function add_ItemClick(handler: ItemClickEventHandler): EventRegistrationToken; safecall;
    procedure remove_ItemClick(token: EventRegistrationToken); safecall;
    function add_DragItemsStarting(handler: DragItemsStartingEventHandler): EventRegistrationToken; safecall;
    procedure remove_DragItemsStarting(token: EventRegistrationToken); safecall;
    procedure ScrollIntoView(item: IInspectable); overload; safecall;
    procedure SelectAll; safecall;
    function LoadMoreItemsAsync: IAsyncOperation_1__Data_LoadMoreItemsResult; safecall;
    procedure ScrollIntoView(item: IInspectable; alignment: ScrollIntoViewAlignment); overload; safecall;
    function get_Header: IInspectable; safecall;
    procedure put_Header(value: IInspectable); safecall;
    function get_HeaderTemplate: IDataTemplate; safecall;
    procedure put_HeaderTemplate(value: IDataTemplate); safecall;
    function get_HeaderTransitions: IVector_1__Animation_ITransition; safecall;
    procedure put_HeaderTransitions(value: IVector_1__Animation_ITransition); safecall;
    property CanDragItems: Boolean read get_CanDragItems write put_CanDragItems;
    property CanReorderItems: Boolean read get_CanReorderItems write put_CanReorderItems;
    property DataFetchSize: Double read get_DataFetchSize write put_DataFetchSize;
    property Header: IInspectable read get_Header write put_Header;
    property HeaderTemplate: IDataTemplate read get_HeaderTemplate write put_HeaderTemplate;
    property HeaderTransitions: IVector_1__Animation_ITransition read get_HeaderTransitions write put_HeaderTransitions;
    property IncrementalLoadingThreshold: Double read get_IncrementalLoadingThreshold write put_IncrementalLoadingThreshold;
    property IncrementalLoadingTrigger_: IncrementalLoadingTrigger read get_IncrementalLoadingTrigger write put_IncrementalLoadingTrigger;
    property IsItemClickEnabled: Boolean read get_IsItemClickEnabled write put_IsItemClickEnabled;
    property IsSwipeEnabled: Boolean read get_IsSwipeEnabled write put_IsSwipeEnabled;
    property SelectedItems: IVector_1__IInspectable read get_SelectedItems;
    property SelectionMode: ListViewSelectionMode read get_SelectionMode write put_SelectionMode;
  end;

  // Windows.UI.Xaml.Controls.ItemClickEventHandler
  // External 
  ItemClickEventHandler = interface(IUnknown)
  ['{3DF6D14E-E18A-4A75-9395-627C5F3CD489}']
    procedure Invoke(sender: IInspectable; e: IItemClickEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IItemClickEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_ItemClickEventArgs)]
  IItemClickEventArgs = interface(IInspectable)
  ['{9C314242-F29A-494D-A3A5-D4C7B2A46852}']
    function get_ClickedItem: IInspectable; safecall;
    property ClickedItem: IInspectable read get_ClickedItem;
  end;

  // Windows.UI.Xaml.Controls.DragItemsStartingEventHandler
  // External 
  DragItemsStartingEventHandler = interface(IUnknown)
  ['{3AAEAB4C-14CB-4434-BECC-88A8585C2F89}']
    procedure Invoke(sender: IInspectable; e: IDragItemsStartingEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IDragItemsStartingEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_DragItemsStartingEventArgs)]
  IDragItemsStartingEventArgs = interface(IInspectable)
  ['{71CF215C-DAED-4783-AA11-DC574D2713E9}']
    function get_Cancel: Boolean; safecall;
    procedure put_Cancel(value: Boolean); safecall;
    function get_Items: IVector_1__IInspectable; safecall;
    function get_Data: IDataPackage; safecall;
    property Cancel: Boolean read get_Cancel write put_Cancel;
    property Data: IDataPackage read get_Data;
    property Items: IVector_1__IInspectable read get_Items;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IUIElementWeakCollection
  [WinRTClassNameAttribute(SWindows_UI_Xaml_UIElementWeakCollection)]
  IUIElementWeakCollection = interface(IInspectable)
  ['{10341223-E66D-519E-ACF8-556BD244EAC3}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Controls.Primitives.IPopup>
  // External 
  IVectorView_1__Primitives_IPopup = interface(IInspectable)
  ['{722B1286-A012-51AD-BD6E-CBF8C42FCFAA}']
    function GetAt(index: Cardinal): Primitives_IPopup; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Primitives_IPopup; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrimitives_IPopup): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.Primitives.IPopup
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_Primitives_Popup)]
  Primitives_IPopup = interface(IInspectable)
  ['{62418240-E6D3-4705-A1DC-39156456EE29}']
    function get_Child: IUIElement; safecall;
    procedure put_Child(value: IUIElement); safecall;
    function get_IsOpen: Boolean; safecall;
    procedure put_IsOpen(value: Boolean); safecall;
    function get_HorizontalOffset: Double; safecall;
    procedure put_HorizontalOffset(value: Double); safecall;
    function get_VerticalOffset: Double; safecall;
    procedure put_VerticalOffset(value: Double); safecall;
    function get_ChildTransitions: IVector_1__Animation_ITransition; safecall;
    procedure put_ChildTransitions(value: IVector_1__Animation_ITransition); safecall;
    function get_IsLightDismissEnabled: Boolean; safecall;
    procedure put_IsLightDismissEnabled(value: Boolean); safecall;
    function add_Opened(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Opened(token: EventRegistrationToken); safecall;
    function add_Closed(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
    property Child: IUIElement read get_Child write put_Child;
    property ChildTransitions: IVector_1__Animation_ITransition read get_ChildTransitions write put_ChildTransitions;
    property HorizontalOffset: Double read get_HorizontalOffset write put_HorizontalOffset;
    property IsLightDismissEnabled: Boolean read get_IsLightDismissEnabled write put_IsLightDismissEnabled;
    property IsOpen: Boolean read get_IsOpen write put_IsOpen;
    property VerticalOffset: Double read get_VerticalOffset write put_VerticalOffset;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.IWindow
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Window)]
  IWindow = interface(IInspectable)
  ['{3276167D-C9F6-462D-9DE2-AE4C1FD8C2E5}']
    function get_Bounds: TRectF; safecall;
    function get_Visible: Boolean; safecall;
    function get_Content: IUIElement; safecall;
    procedure put_Content(value: IUIElement); safecall;
    function get_CoreWindow: ICoreWindow; safecall;
    function get_Dispatcher: ICoreDispatcher; safecall;
    function add_Activated(handler: WindowActivatedEventHandler): EventRegistrationToken; safecall;
    procedure remove_Activated(token: EventRegistrationToken); safecall;
    function add_Closed(handler: WindowClosedEventHandler): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
    function add_SizeChanged(handler: WindowSizeChangedEventHandler): EventRegistrationToken; safecall;
    procedure remove_SizeChanged(token: EventRegistrationToken); safecall;
    function add_VisibilityChanged(handler: WindowVisibilityChangedEventHandler): EventRegistrationToken; safecall;
    procedure remove_VisibilityChanged(token: EventRegistrationToken); safecall;
    procedure Activate; safecall;
    procedure Close; safecall;
    property Bounds: TRectF read get_Bounds;
    property Content: IUIElement read get_Content write put_Content;
    property CoreWindow: ICoreWindow read get_CoreWindow;
    property Dispatcher: ICoreDispatcher read get_Dispatcher;
    property Visible: Boolean read get_Visible;
  end;

  // Windows.UI.Xaml.WindowActivatedEventHandler
  // External 
  WindowActivatedEventHandler = interface(IUnknown)
  ['{18026348-8619-4C7B-B534-CED45D9DE219}']
    procedure Invoke(sender: IInspectable; e: IWindowActivatedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.WindowClosedEventHandler
  // External 
  WindowClosedEventHandler = interface(IUnknown)
  ['{0DB89161-20D7-45DF-9122-BA89576703BA}']
    procedure Invoke(sender: IInspectable; e: ICoreWindowEventArgs); safecall;
  end;

  // Windows.UI.Xaml.WindowSizeChangedEventHandler
  // External 
  WindowSizeChangedEventHandler = interface(IUnknown)
  ['{5C21C742-2CED-4FD9-BA38-7118D40E966B}']
    procedure Invoke(sender: IInspectable; e: IWindowSizeChangedEventArgs); safecall;
  end;

  // Windows.UI.Xaml.WindowVisibilityChangedEventHandler
  // External 
  WindowVisibilityChangedEventHandler = interface(IUnknown)
  ['{10406AD6-B090-4A4A-B2AD-D682DF27130F}']
    procedure Invoke(sender: IInspectable; e: IVisibilityChangedEventArgs); safecall;
  end;

  // DualAPI Interface
  // Windows.UI.Xaml.Controls.IMediaTransportControls
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Controls_MediaTransportControls)]
  IMediaTransportControls = interface(IInspectable)
  ['{D6F69E7D-0825-49A9-9FCE-5586D8694F0C}']
    function get_IsFullWindowButtonVisible: Boolean; safecall;
    procedure put_IsFullWindowButtonVisible(value: Boolean); safecall;
    function get_IsFullWindowEnabled: Boolean; safecall;
    procedure put_IsFullWindowEnabled(value: Boolean); safecall;
    function get_IsZoomButtonVisible: Boolean; safecall;
    procedure put_IsZoomButtonVisible(value: Boolean); safecall;
    function get_IsZoomEnabled: Boolean; safecall;
    procedure put_IsZoomEnabled(value: Boolean); safecall;
    function get_IsFastForwardButtonVisible: Boolean; safecall;
    procedure put_IsFastForwardButtonVisible(value: Boolean); safecall;
    function get_IsFastForwardEnabled: Boolean; safecall;
    procedure put_IsFastForwardEnabled(value: Boolean); safecall;
    function get_IsFastRewindButtonVisible: Boolean; safecall;
    procedure put_IsFastRewindButtonVisible(value: Boolean); safecall;
    function get_IsFastRewindEnabled: Boolean; safecall;
    procedure put_IsFastRewindEnabled(value: Boolean); safecall;
    function get_IsStopButtonVisible: Boolean; safecall;
    procedure put_IsStopButtonVisible(value: Boolean); safecall;
    function get_IsStopEnabled: Boolean; safecall;
    procedure put_IsStopEnabled(value: Boolean); safecall;
    function get_IsVolumeButtonVisible: Boolean; safecall;
    procedure put_IsVolumeButtonVisible(value: Boolean); safecall;
    function get_IsVolumeEnabled: Boolean; safecall;
    procedure put_IsVolumeEnabled(value: Boolean); safecall;
    function get_IsPlaybackRateButtonVisible: Boolean; safecall;
    procedure put_IsPlaybackRateButtonVisible(value: Boolean); safecall;
    function get_IsPlaybackRateEnabled: Boolean; safecall;
    procedure put_IsPlaybackRateEnabled(value: Boolean); safecall;
    function get_IsSeekBarVisible: Boolean; safecall;
    procedure put_IsSeekBarVisible(value: Boolean); safecall;
    function get_IsSeekEnabled: Boolean; safecall;
    procedure put_IsSeekEnabled(value: Boolean); safecall;
    function get_IsCompact: Boolean; safecall;
    procedure put_IsCompact(value: Boolean); safecall;
    property IsCompact: Boolean read get_IsCompact write put_IsCompact;
    property IsFastForwardButtonVisible: Boolean read get_IsFastForwardButtonVisible write put_IsFastForwardButtonVisible;
    property IsFastForwardEnabled: Boolean read get_IsFastForwardEnabled write put_IsFastForwardEnabled;
    property IsFastRewindButtonVisible: Boolean read get_IsFastRewindButtonVisible write put_IsFastRewindButtonVisible;
    property IsFastRewindEnabled: Boolean read get_IsFastRewindEnabled write put_IsFastRewindEnabled;
    property IsFullWindowButtonVisible: Boolean read get_IsFullWindowButtonVisible write put_IsFullWindowButtonVisible;
    property IsFullWindowEnabled: Boolean read get_IsFullWindowEnabled write put_IsFullWindowEnabled;
    property IsPlaybackRateButtonVisible: Boolean read get_IsPlaybackRateButtonVisible write put_IsPlaybackRateButtonVisible;
    property IsPlaybackRateEnabled: Boolean read get_IsPlaybackRateEnabled write put_IsPlaybackRateEnabled;
    property IsSeekBarVisible: Boolean read get_IsSeekBarVisible write put_IsSeekBarVisible;
    property IsSeekEnabled: Boolean read get_IsSeekEnabled write put_IsSeekEnabled;
    property IsStopButtonVisible: Boolean read get_IsStopButtonVisible write put_IsStopButtonVisible;
    property IsStopEnabled: Boolean read get_IsStopEnabled write put_IsStopEnabled;
    property IsVolumeButtonVisible: Boolean read get_IsVolumeButtonVisible write put_IsVolumeButtonVisible;
    property IsVolumeEnabled: Boolean read get_IsVolumeEnabled write put_IsVolumeEnabled;
    property IsZoomButtonVisible: Boolean read get_IsZoomButtonVisible write put_IsZoomButtonVisible;
    property IsZoomEnabled: Boolean read get_IsZoomEnabled write put_IsZoomEnabled;
  end;

  // DualAPI Interface
  // Windows.Graphics.Imaging.ISoftwareBitmap
  [WinRTClassNameAttribute(SWindows_Graphics_Imaging_SoftwareBitmap)]
  Imaging_ISoftwareBitmap = interface(IInspectable)
  ['{689E0708-7EEF-483F-963F-DA938818E073}']
    function get_BitmapPixelFormat: Imaging_BitmapPixelFormat; safecall;
    function get_BitmapAlphaMode: Imaging_BitmapAlphaMode; safecall;
    function get_PixelWidth: Integer; safecall;
    function get_PixelHeight: Integer; safecall;
    function get_IsReadOnly: Boolean; safecall;
    procedure put_DpiX(value: Double); safecall;
    function get_DpiX: Double; safecall;
    procedure put_DpiY(value: Double); safecall;
    function get_DpiY: Double; safecall;
    function LockBuffer(mode: Imaging_BitmapBufferAccessMode): Imaging_IBitmapBuffer; safecall;
    procedure CopyTo(bitmap: Imaging_ISoftwareBitmap); safecall;
    procedure CopyFromBuffer(buffer: IBuffer); safecall;
    procedure CopyToBuffer(buffer: IBuffer); safecall;
    function GetReadOnlyView: Imaging_ISoftwareBitmap; safecall;
    property BitmapAlphaMode: Imaging_BitmapAlphaMode read get_BitmapAlphaMode;
    property BitmapPixelFormat: Imaging_BitmapPixelFormat read get_BitmapPixelFormat;
    property DpiX: Double read get_DpiX write put_DpiX;
    property DpiY: Double read get_DpiY write put_DpiY;
    property IsReadOnly: Boolean read get_IsReadOnly;
    property PixelHeight: Integer read get_PixelHeight;
    property PixelWidth: Integer read get_PixelWidth;
  end;

  // Windows.Graphics.Imaging.IBitmapBuffer
  // External 
  Imaging_IBitmapBuffer = interface(IInspectable)
  ['{A53E04C4-399C-438C-B28F-A63A6B83D1A1}']
    function GetPlaneCount: Integer; safecall;
    function GetPlaneDescription(index: Integer): Imaging_BitmapPlaneDescription; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.DataPackageOperation>
  IAsyncOperation_1__DataPackageOperation_Base = interface(IInspectable)
  ['{8B98AEA9-64F0-5672-B30E-DFD9C2E4F6FE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__DataPackageOperation); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__DataPackageOperation; safecall;
    function GetResults: DataPackageOperation; safecall;
    property Completed: AsyncOperationCompletedHandler_1__DataPackageOperation read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.DataPackageOperation>
  // External 
  IAsyncOperation_1__DataPackageOperation = interface(IAsyncOperation_1__DataPackageOperation_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.DataPackageOperation>
  AsyncOperationCompletedHandler_1__DataPackageOperation_Delegate_Base = interface(IUnknown)
  ['{ADD21D46-17DF-5A43-A685-3262FCE84643}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__DataPackageOperation; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.DataPackageOperation>
  // External 
  AsyncOperationCompletedHandler_1__DataPackageOperation = interface(AsyncOperationCompletedHandler_1__DataPackageOperation_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Vector2>
  // External 
  IReference_1__Numerics_Vector2 = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Numerics_Vector2; safecall;
    property Value: Numerics_Vector2 read get_Value;
  end;

  // DualAPI Interface
  // Windows.Media.Playback.IMediaPlaybackSource
  // External 
  Playback_IMediaPlaybackSource = interface(IInspectable)
  ['{EF9DC2BC-9317-4696-B051-2BAD643177B5}']
  end;

  // DualAPI Interface
  // Windows.Media.Core.IMediaSource
  [WinRTClassNameAttribute(SWindows_Media_Core_MediaSource)]
  Core_IMediaSource = interface(IInspectable)
  ['{E7BFB599-A09D-4C21-BCDF-20AF4F86B3D9}']
  end;

  // Windows.Media.Casting.ICastingSource
  // External 
  Casting_ICastingSource = interface(IInspectable)
  ['{F429EA72-3467-47E6-A027-522923E9D727}']
    function get_PreferredSourceUri: IUriRuntimeClass; safecall;
    procedure put_PreferredSourceUri(value: IUriRuntimeClass); safecall;
    property PreferredSourceUri: IUriRuntimeClass read get_PreferredSourceUri write put_PreferredSourceUri;
  end;

implementation

end.
