{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Storage;

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
  Winapi.Devices.Geolocation, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties_Delegate_Base;
  AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__FileProperties_IBasicProperties;
  PAsyncOperationCompletedHandler_1__FileProperties_IBasicProperties = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__FileProperties_IBasicProperties;
  AsyncOperationCompletedHandler_1__IStorageFile_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageFile_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStorageFile = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageFile;
  PAsyncOperationCompletedHandler_1__IStorageFile = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStorageFile;
  AsyncOperationCompletedHandler_1__IStorageFolder_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageFolder_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStorageFolder = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageFolder;
  PAsyncOperationCompletedHandler_1__IStorageFolder = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStorageFolder;
  AsyncOperationCompletedHandler_1__IStorageItem_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageItem_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStorageItem = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageItem;
  PAsyncOperationCompletedHandler_1__IStorageItem = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStorageItem;
  AsyncOperationCompletedHandler_1__IStorageStreamTransaction_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageStreamTransaction_Delegate_Base;
  AsyncOperationCompletedHandler_1__IStorageStreamTransaction = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IStorageStreamTransaction;
  PAsyncOperationCompletedHandler_1__IStorageStreamTransaction = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IStorageStreamTransaction;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile_Delegate_Base;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageFile;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder_Delegate_Base;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageFolder;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem_Delegate_Base;
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageItem;
  CreationCollisionOption = Winapi.CommonTypes.CreationCollisionOption;
  PCreationCollisionOption = Winapi.CommonTypes.PCreationCollisionOption;
  FileAccessMode = Winapi.CommonTypes.FileAccessMode;
  PFileAccessMode = Winapi.CommonTypes.PFileAccessMode;
  FileAttributes = Winapi.CommonTypes.FileAttributes;
  PFileAttributes = Winapi.CommonTypes.PFileAttributes;
  FileProperties_IBasicProperties = Winapi.CommonTypes.FileProperties_IBasicProperties;
  PFileProperties_IBasicProperties = Winapi.CommonTypes.PFileProperties_IBasicProperties;
  FileProperties_PropertyPrefetchOptions = Winapi.CommonTypes.FileProperties_PropertyPrefetchOptions;
  PFileProperties_PropertyPrefetchOptions = Winapi.CommonTypes.PFileProperties_PropertyPrefetchOptions;
  FileProperties_ThumbnailMode = Winapi.CommonTypes.FileProperties_ThumbnailMode;
  PFileProperties_ThumbnailMode = Winapi.CommonTypes.PFileProperties_ThumbnailMode;
  FileProperties_ThumbnailOptions = Winapi.CommonTypes.FileProperties_ThumbnailOptions;
  PFileProperties_ThumbnailOptions = Winapi.CommonTypes.PFileProperties_ThumbnailOptions;
  IAsyncOperation_1__FileProperties_IBasicProperties_Base = Winapi.CommonTypes.IAsyncOperation_1__FileProperties_IBasicProperties_Base;
  IAsyncOperation_1__FileProperties_IBasicProperties = Winapi.CommonTypes.IAsyncOperation_1__FileProperties_IBasicProperties;
  PIAsyncOperation_1__FileProperties_IBasicProperties = Winapi.CommonTypes.PIAsyncOperation_1__FileProperties_IBasicProperties;
  IAsyncOperation_1__IStorageFile_Base = Winapi.CommonTypes.IAsyncOperation_1__IStorageFile_Base;
  IAsyncOperation_1__IStorageFile = Winapi.CommonTypes.IAsyncOperation_1__IStorageFile;
  PIAsyncOperation_1__IStorageFile = Winapi.CommonTypes.PIAsyncOperation_1__IStorageFile;
  IAsyncOperation_1__IStorageFolder_Base = Winapi.CommonTypes.IAsyncOperation_1__IStorageFolder_Base;
  IAsyncOperation_1__IStorageFolder = Winapi.CommonTypes.IAsyncOperation_1__IStorageFolder;
  PIAsyncOperation_1__IStorageFolder = Winapi.CommonTypes.PIAsyncOperation_1__IStorageFolder;
  IAsyncOperation_1__IStorageItem_Base = Winapi.CommonTypes.IAsyncOperation_1__IStorageItem_Base;
  IAsyncOperation_1__IStorageItem = Winapi.CommonTypes.IAsyncOperation_1__IStorageItem;
  PIAsyncOperation_1__IStorageItem = Winapi.CommonTypes.PIAsyncOperation_1__IStorageItem;
  IAsyncOperation_1__IStorageStreamTransaction_Base = Winapi.CommonTypes.IAsyncOperation_1__IStorageStreamTransaction_Base;
  IAsyncOperation_1__IStorageStreamTransaction = Winapi.CommonTypes.IAsyncOperation_1__IStorageStreamTransaction;
  PIAsyncOperation_1__IStorageStreamTransaction = Winapi.CommonTypes.PIAsyncOperation_1__IStorageStreamTransaction;
  IAsyncOperation_1__IVectorView_1__IStorageFile_Base = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorageFile_Base;
  IAsyncOperation_1__IVectorView_1__IStorageFile = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorageFile;
  PIAsyncOperation_1__IVectorView_1__IStorageFile = Winapi.CommonTypes.PIAsyncOperation_1__IVectorView_1__IStorageFile;
  IAsyncOperation_1__IVectorView_1__IStorageFolder_Base = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorageFolder_Base;
  IAsyncOperation_1__IVectorView_1__IStorageFolder = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorageFolder;
  PIAsyncOperation_1__IVectorView_1__IStorageFolder = Winapi.CommonTypes.PIAsyncOperation_1__IVectorView_1__IStorageFolder;
  IAsyncOperation_1__IVectorView_1__IStorageItem_Base = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorageItem_Base;
  IAsyncOperation_1__IVectorView_1__IStorageItem = Winapi.CommonTypes.IAsyncOperation_1__IVectorView_1__IStorageItem;
  PIAsyncOperation_1__IVectorView_1__IStorageItem = Winapi.CommonTypes.PIAsyncOperation_1__IVectorView_1__IStorageItem;
  IIterable_1__IStorageItem_Base = Winapi.CommonTypes.IIterable_1__IStorageItem_Base;
  IIterable_1__IStorageItem = Winapi.CommonTypes.IIterable_1__IStorageItem;
  PIIterable_1__IStorageItem = Winapi.CommonTypes.PIIterable_1__IStorageItem;
  IIterator_1__IStorageItem_Base = Winapi.CommonTypes.IIterator_1__IStorageItem_Base;
  IIterator_1__IStorageItem = Winapi.CommonTypes.IIterator_1__IStorageItem;
  PIIterator_1__IStorageItem = Winapi.CommonTypes.PIIterator_1__IStorageItem;
  IStorageFile = Winapi.CommonTypes.IStorageFile;
  PIStorageFile = Winapi.CommonTypes.PIStorageFile;
  IStorageFolder = Winapi.CommonTypes.IStorageFolder;
  PIStorageFolder = Winapi.CommonTypes.PIStorageFolder;
  IStorageItem = Winapi.CommonTypes.IStorageItem;
  PIStorageItem = Winapi.CommonTypes.PIStorageItem;
  IStorageStreamTransaction = Winapi.CommonTypes.IStorageStreamTransaction;
  PIStorageStreamTransaction = Winapi.CommonTypes.PIStorageStreamTransaction;
  IVector_1__IStorageFolder_Base = Winapi.CommonTypes.IVector_1__IStorageFolder_Base;
  IVector_1__IStorageFolder = Winapi.CommonTypes.IVector_1__IStorageFolder;
  PIVector_1__IStorageFolder = Winapi.CommonTypes.PIVector_1__IStorageFolder;
  IVector_1__Search_SortEntry_Base = Winapi.CommonTypes.IVector_1__Search_SortEntry_Base;
  IVector_1__Search_SortEntry = Winapi.CommonTypes.IVector_1__Search_SortEntry;
  PIVector_1__Search_SortEntry = Winapi.CommonTypes.PIVector_1__Search_SortEntry;
  IVectorView_1__IStorageFile = Winapi.CommonTypes.IVectorView_1__IStorageFile;
  PIVectorView_1__IStorageFile = Winapi.CommonTypes.PIVectorView_1__IStorageFile;
  IVectorView_1__IStorageFolder = Winapi.CommonTypes.IVectorView_1__IStorageFolder;
  PIVectorView_1__IStorageFolder = Winapi.CommonTypes.PIVectorView_1__IStorageFolder;
  IVectorView_1__IStorageItem = Winapi.CommonTypes.IVectorView_1__IStorageItem;
  PIVectorView_1__IStorageItem = Winapi.CommonTypes.PIVectorView_1__IStorageItem;
  IVectorView_1__Search_SortEntry = Winapi.CommonTypes.IVectorView_1__Search_SortEntry;
  PIVectorView_1__Search_SortEntry = Winapi.CommonTypes.PIVectorView_1__Search_SortEntry;
  NameCollisionOption = Winapi.CommonTypes.NameCollisionOption;
  PNameCollisionOption = Winapi.CommonTypes.PNameCollisionOption;
  Search_DateStackOption = Winapi.CommonTypes.Search_DateStackOption;
  PSearch_DateStackOption = Winapi.CommonTypes.PSearch_DateStackOption;
  Search_FolderDepth = Winapi.CommonTypes.Search_FolderDepth;
  PSearch_FolderDepth = Winapi.CommonTypes.PSearch_FolderDepth;
  Search_IndexerOption = Winapi.CommonTypes.Search_IndexerOption;
  PSearch_IndexerOption = Winapi.CommonTypes.PSearch_IndexerOption;
  Search_IQueryOptions = Winapi.CommonTypes.Search_IQueryOptions;
  PSearch_IQueryOptions = Winapi.CommonTypes.PSearch_IQueryOptions;
  Search_IStorageQueryResultBase = Winapi.CommonTypes.Search_IStorageQueryResultBase;
  PSearch_IStorageQueryResultBase = Winapi.CommonTypes.PSearch_IStorageQueryResultBase;
  Search_SortEntry = Winapi.CommonTypes.Search_SortEntry;
  PSearch_SortEntry = Winapi.CommonTypes.PSearch_SortEntry;
  StorageDeleteOption = Winapi.CommonTypes.StorageDeleteOption;
  PStorageDeleteOption = Winapi.CommonTypes.PStorageDeleteOption;
  StorageItemTypes = Winapi.CommonTypes.StorageItemTypes;
  PStorageItemTypes = Winapi.CommonTypes.PStorageItemTypes;

  // Forward declarations for interfaces

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageFile>
  IIterator_1__IStorageFile = interface;
  PIIterator_1__IStorageFile = ^IIterator_1__IStorageFile;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageFolder>
  IIterator_1__IStorageFolder = interface;
  PIIterator_1__IStorageFolder = ^IIterator_1__IStorageFolder;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageFolder>
  IIterable_1__IStorageFolder = interface;
  PIIterable_1__IStorageFolder = ^IIterable_1__IStorageFolder;

  // Windows.Storage.Pickers.Provider.IFileRemovedEventArgs
  Pickers_Provider_IFileRemovedEventArgs = interface;
  PPickers_Provider_IFileRemovedEventArgs = ^Pickers_Provider_IFileRemovedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileOpenPickerUI,Windows.Storage.Pickers.Provider.IFileRemovedEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IFileRemovedEventArgs = interface;
  PTypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IFileRemovedEventArgs = ^TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IFileRemovedEventArgs;

  // Windows.Storage.Pickers.Provider.IPickerClosingDeferral
  Pickers_Provider_IPickerClosingDeferral = interface;
  PPickers_Provider_IPickerClosingDeferral = ^Pickers_Provider_IPickerClosingDeferral;

  // Windows.Storage.Pickers.Provider.IPickerClosingOperation
  Pickers_Provider_IPickerClosingOperation = interface;
  PPickers_Provider_IPickerClosingOperation = ^Pickers_Provider_IPickerClosingOperation;

  // Windows.Storage.Pickers.Provider.IPickerClosingEventArgs
  Pickers_Provider_IPickerClosingEventArgs = interface;
  PPickers_Provider_IPickerClosingEventArgs = ^Pickers_Provider_IPickerClosingEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileOpenPickerUI,Windows.Storage.Pickers.Provider.IPickerClosingEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IPickerClosingEventArgs = interface;
  PTypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IPickerClosingEventArgs = ^TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IPickerClosingEventArgs;

  // Windows.Storage.Pickers.Provider.IFileOpenPickerUI
  Pickers_Provider_IFileOpenPickerUI = interface;
  PPickers_Provider_IFileOpenPickerUI = ^Pickers_Provider_IFileOpenPickerUI;

  // Windows.Storage.Pickers.Provider.ITargetFileRequestDeferral
  Pickers_Provider_ITargetFileRequestDeferral = interface;
  PPickers_Provider_ITargetFileRequestDeferral = ^Pickers_Provider_ITargetFileRequestDeferral;

  // Windows.Storage.Pickers.Provider.ITargetFileRequest
  Pickers_Provider_ITargetFileRequest = interface;
  PPickers_Provider_ITargetFileRequest = ^Pickers_Provider_ITargetFileRequest;

  // Windows.Storage.Pickers.Provider.ITargetFileRequestedEventArgs
  Pickers_Provider_ITargetFileRequestedEventArgs = interface;
  PPickers_Provider_ITargetFileRequestedEventArgs = ^Pickers_Provider_ITargetFileRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Windows.Storage.Pickers.Provider.ITargetFileRequestedEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__Pickers_Provider_ITargetFileRequestedEventArgs = interface;
  PTypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__Pickers_Provider_ITargetFileRequestedEventArgs = ^TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__Pickers_Provider_ITargetFileRequestedEventArgs;

  // Windows.Storage.Pickers.Provider.IFileSavePickerUI
  Pickers_Provider_IFileSavePickerUI = interface;
  PPickers_Provider_IFileSavePickerUI = ^Pickers_Provider_IFileSavePickerUI;

  // Windows.Storage.Provider.IFileUpdateRequestDeferral
  Provider_IFileUpdateRequestDeferral = interface;
  PProvider_IFileUpdateRequestDeferral = ^Provider_IFileUpdateRequestDeferral;

  // Windows.Storage.Provider.IFileUpdateRequest
  Provider_IFileUpdateRequest = interface;
  PProvider_IFileUpdateRequest = ^Provider_IFileUpdateRequest;

  // Windows.Storage.Provider.IFileUpdateRequestedEventArgs
  Provider_IFileUpdateRequestedEventArgs = interface;
  PProvider_IFileUpdateRequestedEventArgs = ^Provider_IFileUpdateRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Windows.Storage.Provider.IFileUpdateRequestedEventArgs>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__Provider_IFileUpdateRequestedEventArgs = interface;
  PTypedEventHandler_2__Provider_ICachedFileUpdaterUI__Provider_IFileUpdateRequestedEventArgs = ^TypedEventHandler_2__Provider_ICachedFileUpdaterUI__Provider_IFileUpdateRequestedEventArgs;

  // Windows.Storage.Provider.ICachedFileUpdaterUI
  Provider_ICachedFileUpdaterUI = interface;
  PProvider_ICachedFileUpdaterUI = ^Provider_ICachedFileUpdaterUI;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Search.SortEntry>
  IIterator_1__Search_SortEntry = interface;
  PIIterator_1__Search_SortEntry = ^IIterator_1__Search_SortEntry;

  // Windows.Storage.Search.IStorageFileQueryResult
  Search_IStorageFileQueryResult = interface;
  PSearch_IStorageFileQueryResult = ^Search_IStorageFileQueryResult;

  // Windows.Storage.IStorageLibraryChange
  IStorageLibraryChange = interface;
  PIStorageLibraryChange = ^IStorageLibraryChange;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageLibraryChange>
  IIterator_1__IStorageLibraryChange = interface;
  PIIterator_1__IStorageLibraryChange = ^IIterator_1__IStorageLibraryChange;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageLibraryChange>
  IIterable_1__IStorageLibraryChange = interface;
  PIIterable_1__IStorageLibraryChange = ^IIterable_1__IStorageLibraryChange;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>
  IVectorView_1__IStorageLibraryChange = interface;
  PIVectorView_1__IStorageLibraryChange = ^IVectorView_1__IStorageLibraryChange;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange = ^AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>>
  IAsyncOperation_1__IVectorView_1__IStorageLibraryChange = interface;
  PIAsyncOperation_1__IVectorView_1__IStorageLibraryChange = ^IAsyncOperation_1__IVectorView_1__IStorageLibraryChange;

  // Windows.Storage.IStorageLibraryChangeReader
  IStorageLibraryChangeReader = interface;
  PIStorageLibraryChangeReader = ^IStorageLibraryChangeReader;

  // Windows.Storage.IStorageLibraryChangeTracker
  IStorageLibraryChangeTracker = interface;
  PIStorageLibraryChangeTracker = ^IStorageLibraryChangeTracker;

  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Storage.IStorageFolder>
  VectorChangedEventHandler_1__IStorageFolder = interface;
  PVectorChangedEventHandler_1__IStorageFolder = ^VectorChangedEventHandler_1__IStorageFolder;

  // Windows.Foundation.Collections.IObservableVector`1<Windows.Storage.IStorageFolder>
  IObservableVector_1__IStorageFolder = interface;
  PIObservableVector_1__IStorageFolder = ^IObservableVector_1__IStorageFolder;

  // Windows.Storage.ISetVersionDeferral
  ISetVersionDeferral = interface;
  PISetVersionDeferral = ^ISetVersionDeferral;

  // Windows.Storage.ISetVersionRequest
  ISetVersionRequest = interface;
  PISetVersionRequest = ^ISetVersionRequest;

  // Windows.Storage.ApplicationDataSetVersionHandler
  ApplicationDataSetVersionHandler = interface;
  PApplicationDataSetVersionHandler = ^ApplicationDataSetVersionHandler;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>
  IKeyValuePair_2__HSTRING__IApplicationDataContainer = interface;
  PIKeyValuePair_2__HSTRING__IApplicationDataContainer = ^IKeyValuePair_2__HSTRING__IApplicationDataContainer;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>>
  IIterator_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer = ^IIterator_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>>
  IIterable_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer = ^IIterable_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.IApplicationDataContainer>
  IMapView_2__HSTRING__IApplicationDataContainer = interface;
  PIMapView_2__HSTRING__IApplicationDataContainer = ^IMapView_2__HSTRING__IApplicationDataContainer;

  // Windows.Storage.IApplicationDataContainer
  IApplicationDataContainer = interface;
  PIApplicationDataContainer = ^IApplicationDataContainer;

  // Windows.Storage.IApplicationData
  IApplicationData = interface;
  PIApplicationData = ^IApplicationData;

  // Windows.Storage.FileProperties.IMusicProperties
  FileProperties_IMusicProperties = interface;
  PFileProperties_IMusicProperties = ^FileProperties_IMusicProperties;

  // Windows.Storage.FileProperties.IVideoProperties
  FileProperties_IVideoProperties = interface;
  PFileProperties_IVideoProperties = ^FileProperties_IVideoProperties;

  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageFile>
  IVector_1__IStorageFile = interface;
  PIVector_1__IStorageFile = ^IVector_1__IStorageFile;

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

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.AccessCache.AccessListEntry>
  IIterator_1__AccessCache_AccessListEntry = interface;
  PIIterator_1__AccessCache_AccessListEntry = ^IIterator_1__AccessCache_AccessListEntry;

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

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMap`2<String,Object>>
  AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable = ^AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMap`2<String,Object>>
  IAsyncOperation_1__IMap_2__HSTRING__IInspectable = interface;
  PIAsyncOperation_1__IMap_2__HSTRING__IInspectable = ^IAsyncOperation_1__IMap_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // Windows.Foundation.IReference`1<Double>
  IReference_1__Double = interface;
  PIReference_1__Double = ^IReference_1__Double;

  // Windows.Storage.FileProperties.IImageProperties
  FileProperties_IImageProperties = interface;
  PFileProperties_IImageProperties = ^FileProperties_IImageProperties;

  // Windows.Storage.FileProperties.IDocumentProperties
  FileProperties_IDocumentProperties = interface;
  PFileProperties_IDocumentProperties = ^FileProperties_IDocumentProperties;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  AsyncOperationCompletedHandler_1__Cardinal = interface;
  PAsyncOperationCompletedHandler_1__Cardinal = ^AsyncOperationCompletedHandler_1__Cardinal;

  // Windows.Foundation.IAsyncOperation`1<UInt32>
  IAsyncOperation_1__Cardinal = interface;
  PIAsyncOperation_1__Cardinal = ^IAsyncOperation_1__Cardinal;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Search.IStorageQueryResultBase,Object>
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = interface;
  PTypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = ^TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable;

  // Windows.Storage.Compression.ICompressor
  Compression_ICompressor = interface;
  PCompression_ICompressor = ^Compression_ICompressor;

  // Windows.Storage.Compression.ICompressorFactory
  Compression_ICompressorFactory = interface;
  PCompression_ICompressorFactory = ^Compression_ICompressorFactory;

  // Windows.Storage.Compression.IDecompressor
  Compression_IDecompressor = interface;
  PCompression_IDecompressor = ^Compression_IDecompressor;

  // Windows.Storage.Compression.IDecompressorFactory
  Compression_IDecompressorFactory = interface;
  PCompression_IDecompressorFactory = ^Compression_IDecompressorFactory;

  // Windows.Storage.FileProperties.IGeotagHelperStatics
  FileProperties_IGeotagHelperStatics = interface;
  PFileProperties_IGeotagHelperStatics = ^FileProperties_IGeotagHelperStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IMusicProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties = interface;
  PAsyncOperationCompletedHandler_1__FileProperties_IMusicProperties = ^AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IMusicProperties>
  IAsyncOperation_1__FileProperties_IMusicProperties = interface;
  PIAsyncOperation_1__FileProperties_IMusicProperties = ^IAsyncOperation_1__FileProperties_IMusicProperties;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IVideoProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties = interface;
  PAsyncOperationCompletedHandler_1__FileProperties_IVideoProperties = ^AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IVideoProperties>
  IAsyncOperation_1__FileProperties_IVideoProperties = interface;
  PIAsyncOperation_1__FileProperties_IVideoProperties = ^IAsyncOperation_1__FileProperties_IVideoProperties;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IImageProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IImageProperties = interface;
  PAsyncOperationCompletedHandler_1__FileProperties_IImageProperties = ^AsyncOperationCompletedHandler_1__FileProperties_IImageProperties;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IImageProperties>
  IAsyncOperation_1__FileProperties_IImageProperties = interface;
  PIAsyncOperation_1__FileProperties_IImageProperties = ^IAsyncOperation_1__FileProperties_IImageProperties;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IDocumentProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties = interface;
  PAsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties = ^AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IDocumentProperties>
  IAsyncOperation_1__FileProperties_IDocumentProperties = interface;
  PIAsyncOperation_1__FileProperties_IDocumentProperties = ^IAsyncOperation_1__FileProperties_IDocumentProperties;

  // Windows.Storage.FileProperties.IStorageItemContentProperties
  FileProperties_IStorageItemContentProperties = interface;
  PFileProperties_IStorageItemContentProperties = ^FileProperties_IStorageItemContentProperties;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.IApplicationData,Object>
  TypedEventHandler_2__IApplicationData__IInspectable = interface;
  PTypedEventHandler_2__IApplicationData__IInspectable = ^TypedEventHandler_2__IApplicationData__IInspectable;

  // Windows.Storage.IApplicationData2
  IApplicationData2 = interface;
  PIApplicationData2 = ^IApplicationData2;

  // Windows.Storage.IApplicationData3
  IApplicationData3 = interface;
  PIApplicationData3 = ^IApplicationData3;

  // Windows.Storage.IApplicationDataStatics
  IApplicationDataStatics = interface;
  PIApplicationDataStatics = ^IApplicationDataStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IApplicationData>
  AsyncOperationCompletedHandler_1__IApplicationData = interface;
  PAsyncOperationCompletedHandler_1__IApplicationData = ^AsyncOperationCompletedHandler_1__IApplicationData;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IApplicationData>
  IAsyncOperation_1__IApplicationData = interface;
  PIAsyncOperation_1__IApplicationData = ^IAsyncOperation_1__IApplicationData;

  // Windows.Storage.IApplicationDataStatics2
  IApplicationDataStatics2 = interface;
  PIApplicationDataStatics2 = ^IApplicationDataStatics2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Provider.FileUpdateStatus>
  AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus = interface;
  PAsyncOperationCompletedHandler_1__Provider_FileUpdateStatus = ^AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Provider.FileUpdateStatus>
  IAsyncOperation_1__Provider_FileUpdateStatus = interface;
  PIAsyncOperation_1__Provider_FileUpdateStatus = ^IAsyncOperation_1__Provider_FileUpdateStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  AsyncOperationCompletedHandler_1__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__HSTRING = ^AsyncOperationCompletedHandler_1__HSTRING;

  // Windows.Foundation.IAsyncOperation`1<String>
  IAsyncOperation_1__HSTRING = interface;
  PIAsyncOperation_1__HSTRING = ^IAsyncOperation_1__HSTRING;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<String>>
  AsyncOperationCompletedHandler_1__IVector_1__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__IVector_1__HSTRING = ^AsyncOperationCompletedHandler_1__IVector_1__HSTRING;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<String>>
  IAsyncOperation_1__IVector_1__HSTRING = interface;
  PIAsyncOperation_1__IVector_1__HSTRING = ^IAsyncOperation_1__IVector_1__HSTRING;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.KnownFoldersAccessStatus>
  AsyncOperationCompletedHandler_1__KnownFoldersAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__KnownFoldersAccessStatus = ^AsyncOperationCompletedHandler_1__KnownFoldersAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.KnownFoldersAccessStatus>
  IAsyncOperation_1__KnownFoldersAccessStatus = interface;
  PIAsyncOperation_1__KnownFoldersAccessStatus = ^IAsyncOperation_1__KnownFoldersAccessStatus;

  // Windows.Storage.IStorageFile2
  IStorageFile2 = interface;
  PIStorageFile2 = ^IStorageFile2;

  // Windows.Storage.IStorageFilePropertiesWithAvailability
  IStorageFilePropertiesWithAvailability = interface;
  PIStorageFilePropertiesWithAvailability = ^IStorageFilePropertiesWithAvailability;

  // Windows.Storage.StreamedFileDataRequestedHandler
  StreamedFileDataRequestedHandler = interface;
  PStreamedFileDataRequestedHandler = ^StreamedFileDataRequestedHandler;

  // Windows.Storage.IStorageFileStatics
  IStorageFileStatics = interface;
  PIStorageFileStatics = ^IStorageFileStatics;

  // Windows.Storage.IStorageFileStatics2
  IStorageFileStatics2 = interface;
  PIStorageFileStatics2 = ^IStorageFileStatics2;

  // Windows.Storage.IStorageFolder2
  IStorageFolder2 = interface;
  PIStorageFolder2 = ^IStorageFolder2;

  // Windows.Storage.IStorageFolder3
  IStorageFolder3 = interface;
  PIStorageFolder3 = ^IStorageFolder3;

  // Windows.Storage.IStorageFolderStatics
  IStorageFolderStatics = interface;
  PIStorageFolderStatics = ^IStorageFolderStatics;

  // Windows.Storage.IStorageFolderStatics2
  IStorageFolderStatics2 = interface;
  PIStorageFolderStatics2 = ^IStorageFolderStatics2;

  // Windows.Storage.IStorageItem2
  IStorageItem2 = interface;
  PIStorageItem2 = ^IStorageItem2;

  // Windows.Storage.IStorageItemProperties
  IStorageItemProperties = interface;
  PIStorageItemProperties = ^IStorageItemProperties;

  // Windows.Storage.IStorageItemProperties2
  IStorageItemProperties2 = interface;
  PIStorageItemProperties2 = ^IStorageItemProperties2;

  // Windows.Storage.IStorageProvider
  IStorageProvider = interface;
  PIStorageProvider = ^IStorageProvider;

  // Windows.Storage.IStorageItemPropertiesWithProvider
  IStorageItemPropertiesWithProvider = interface;
  PIStorageItemPropertiesWithProvider = ^IStorageItemPropertiesWithProvider;

  // Windows.Storage.ISystemDataPaths
  ISystemDataPaths = interface;
  PISystemDataPaths = ^ISystemDataPaths;

  // Windows.Storage.ISystemDataPathsStatics
  ISystemDataPathsStatics = interface;
  PISystemDataPathsStatics = ^ISystemDataPathsStatics;

  // Windows.Storage.IUserDataPaths
  IUserDataPaths = interface;
  PIUserDataPaths = ^IUserDataPaths;

  // Windows.Storage.IUserDataPathsStatics
  IUserDataPathsStatics = interface;
  PIUserDataPathsStatics = ^IUserDataPathsStatics;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>
  IKeyValuePair_2__HSTRING__IVector_1__HSTRING = interface;
  PIKeyValuePair_2__HSTRING__IVector_1__HSTRING = ^IKeyValuePair_2__HSTRING__IVector_1__HSTRING;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>>
  IIterator_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING = ^IIterator_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>>
  IIterable_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING = ^IIterable_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.Foundation.Collections.IVector`1<String>>
  IMapView_2__HSTRING__IVector_1__HSTRING = interface;
  PIMapView_2__HSTRING__IVector_1__HSTRING = ^IMapView_2__HSTRING__IVector_1__HSTRING;

  // Windows.Foundation.Collections.IMap`2<String,Windows.Foundation.Collections.IVector`1<String>>
  IMap_2__HSTRING__IVector_1__HSTRING = interface;
  PIMap_2__HSTRING__IVector_1__HSTRING = ^IMap_2__HSTRING__IVector_1__HSTRING;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Object>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable = interface;
  PTypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable = ^TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Object>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable = interface;
  PTypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable = ^TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable;

  // Windows.Storage.Provider.IStorageProviderFileTypeInfo
  Provider_IStorageProviderFileTypeInfo = interface;
  PProvider_IStorageProviderFileTypeInfo = ^Provider_IStorageProviderFileTypeInfo;

  // Windows.Storage.Provider.IStorageProviderFileTypeInfoFactory
  Provider_IStorageProviderFileTypeInfoFactory = interface;
  PProvider_IStorageProviderFileTypeInfoFactory = ^Provider_IStorageProviderFileTypeInfoFactory;

  // Windows.Storage.Provider.IStorageProviderGetContentInfoForPathResult
  Provider_IStorageProviderGetContentInfoForPathResult = interface;
  PProvider_IStorageProviderGetContentInfoForPathResult = ^Provider_IStorageProviderGetContentInfoForPathResult;

  // Windows.Storage.Provider.IStorageProviderGetPathForContentUriResult
  Provider_IStorageProviderGetPathForContentUriResult = interface;
  PProvider_IStorageProviderGetPathForContentUriResult = ^Provider_IStorageProviderGetPathForContentUriResult;

  // Windows.Storage.Provider.IStorageProviderItemProperty
  Provider_IStorageProviderItemProperty = interface;
  PProvider_IStorageProviderItemProperty = ^Provider_IStorageProviderItemProperty;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderItemProperty>
  IIterator_1__Provider_IStorageProviderItemProperty = interface;
  PIIterator_1__Provider_IStorageProviderItemProperty = ^IIterator_1__Provider_IStorageProviderItemProperty;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderItemProperty>
  IIterable_1__Provider_IStorageProviderItemProperty = interface;
  PIIterable_1__Provider_IStorageProviderItemProperty = ^IIterable_1__Provider_IStorageProviderItemProperty;

  // Windows.Storage.Provider.IStorageProviderItemPropertiesStatics
  Provider_IStorageProviderItemPropertiesStatics = interface;
  PProvider_IStorageProviderItemPropertiesStatics = ^Provider_IStorageProviderItemPropertiesStatics;

  // Windows.Storage.Provider.IStorageProviderItemPropertyDefinition
  Provider_IStorageProviderItemPropertyDefinition = interface;
  PProvider_IStorageProviderItemPropertyDefinition = ^Provider_IStorageProviderItemPropertyDefinition;

  // Windows.Storage.Provider.IStorageProviderItemPropertySource
  Provider_IStorageProviderItemPropertySource = interface;
  PProvider_IStorageProviderItemPropertySource = ^Provider_IStorageProviderItemPropertySource;

  // Windows.Storage.Provider.IStorageProviderPropertyCapabilities
  Provider_IStorageProviderPropertyCapabilities = interface;
  PProvider_IStorageProviderPropertyCapabilities = ^Provider_IStorageProviderPropertyCapabilities;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IIterator_1__Provider_IStorageProviderItemPropertyDefinition = interface;
  PIIterator_1__Provider_IStorageProviderItemPropertyDefinition = ^IIterator_1__Provider_IStorageProviderItemPropertyDefinition;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IIterable_1__Provider_IStorageProviderItemPropertyDefinition = interface;
  PIIterable_1__Provider_IStorageProviderItemPropertyDefinition = ^IIterable_1__Provider_IStorageProviderItemPropertyDefinition;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IVectorView_1__Provider_IStorageProviderItemPropertyDefinition = interface;
  PIVectorView_1__Provider_IStorageProviderItemPropertyDefinition = ^IVectorView_1__Provider_IStorageProviderItemPropertyDefinition;

  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IVector_1__Provider_IStorageProviderItemPropertyDefinition = interface;
  PIVector_1__Provider_IStorageProviderItemPropertyDefinition = ^IVector_1__Provider_IStorageProviderItemPropertyDefinition;

  // Windows.Storage.Provider.IStorageProviderSyncRootInfo
  Provider_IStorageProviderSyncRootInfo = interface;
  PProvider_IStorageProviderSyncRootInfo = ^Provider_IStorageProviderSyncRootInfo;

  // Windows.Storage.Provider.IStorageProviderSyncRootInfo2
  Provider_IStorageProviderSyncRootInfo2 = interface;
  PProvider_IStorageProviderSyncRootInfo2 = ^Provider_IStorageProviderSyncRootInfo2;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IIterator_1__Provider_IStorageProviderFileTypeInfo = interface;
  PIIterator_1__Provider_IStorageProviderFileTypeInfo = ^IIterator_1__Provider_IStorageProviderFileTypeInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IIterable_1__Provider_IStorageProviderFileTypeInfo = interface;
  PIIterable_1__Provider_IStorageProviderFileTypeInfo = ^IIterable_1__Provider_IStorageProviderFileTypeInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IVectorView_1__Provider_IStorageProviderFileTypeInfo = interface;
  PIVectorView_1__Provider_IStorageProviderFileTypeInfo = ^IVectorView_1__Provider_IStorageProviderFileTypeInfo;

  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IVector_1__Provider_IStorageProviderFileTypeInfo = interface;
  PIVector_1__Provider_IStorageProviderFileTypeInfo = ^IVector_1__Provider_IStorageProviderFileTypeInfo;

  // Windows.Storage.Provider.IStorageProviderSyncRootInfo3
  Provider_IStorageProviderSyncRootInfo3 = interface;
  PProvider_IStorageProviderSyncRootInfo3 = ^Provider_IStorageProviderSyncRootInfo3;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IIterator_1__Provider_IStorageProviderSyncRootInfo = interface;
  PIIterator_1__Provider_IStorageProviderSyncRootInfo = ^IIterator_1__Provider_IStorageProviderSyncRootInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IIterable_1__Provider_IStorageProviderSyncRootInfo = interface;
  PIIterable_1__Provider_IStorageProviderSyncRootInfo = ^IIterable_1__Provider_IStorageProviderSyncRootInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IVectorView_1__Provider_IStorageProviderSyncRootInfo = interface;
  PIVectorView_1__Provider_IStorageProviderSyncRootInfo = ^IVectorView_1__Provider_IStorageProviderSyncRootInfo;

  // Windows.Storage.Provider.IStorageProviderSyncRootManagerStatics
  Provider_IStorageProviderSyncRootManagerStatics = interface;
  PProvider_IStorageProviderSyncRootManagerStatics = ^Provider_IStorageProviderSyncRootManagerStatics;

  // Windows.Storage.Provider.IStorageProviderSyncRootManagerStatics2
  Provider_IStorageProviderSyncRootManagerStatics2 = interface;
  PProvider_IStorageProviderSyncRootManagerStatics2 = ^Provider_IStorageProviderSyncRootManagerStatics2;

  // Windows.Storage.Provider.IStorageProviderUriSource
  Provider_IStorageProviderUriSource = interface;
  PProvider_IStorageProviderUriSource = ^Provider_IStorageProviderUriSource;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable = ^AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IAsyncOperation_1__IMapView_2__HSTRING__IInspectable = interface;
  PIAsyncOperation_1__IMapView_2__HSTRING__IInspectable = ^IAsyncOperation_1__IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IIterator_1__IMapView_2__HSTRING__IInspectable = interface;
  PIIterator_1__IMapView_2__HSTRING__IInspectable = ^IIterator_1__IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IIterable_1__IMapView_2__HSTRING__IInspectable = interface;
  PIIterable_1__IMapView_2__HSTRING__IInspectable = ^IIterable_1__IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IVectorView_1__IMapView_2__HSTRING__IInspectable = interface;
  PIVectorView_1__IMapView_2__HSTRING__IInspectable = ^IVectorView_1__IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable = ^AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>>
  IAsyncOperation_1__IVectorView_1__IMapView_2__HSTRING__IInspectable = interface;
  PIAsyncOperation_1__IVectorView_1__IMapView_2__HSTRING__IInspectable = ^IAsyncOperation_1__IVectorView_1__IMapView_2__HSTRING__IInspectable;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Search.IndexedState>
  AsyncOperationCompletedHandler_1__Search_IndexedState = interface;
  PAsyncOperationCompletedHandler_1__Search_IndexedState = ^AsyncOperationCompletedHandler_1__Search_IndexedState;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Search.IndexedState>
  IAsyncOperation_1__Search_IndexedState = interface;
  PIAsyncOperation_1__Search_IndexedState = ^IAsyncOperation_1__Search_IndexedState;

  // Windows.Storage.Search.IStorageFolderQueryResult
  Search_IStorageFolderQueryResult = interface;
  PSearch_IStorageFolderQueryResult = ^Search_IStorageFolderQueryResult;

  // Windows.Storage.Search.IStorageItemQueryResult
  Search_IStorageItemQueryResult = interface;
  PSearch_IStorageItemQueryResult = ^Search_IStorageItemQueryResult;

  // Windows.Storage.Search.IStorageFolderQueryOperations
  Search_IStorageFolderQueryOperations = interface;
  PSearch_IStorageFolderQueryOperations = ^Search_IStorageFolderQueryOperations;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface;
  PTypedEventHandler_2__IMemoryBufferReference__IInspectable = ^TypedEventHandler_2__IMemoryBufferReference__IInspectable;

  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt64,UInt64>
  AsyncOperationProgressHandler_2__UInt64__UInt64 = interface;
  PAsyncOperationProgressHandler_2__UInt64__UInt64 = ^AsyncOperationProgressHandler_2__UInt64__UInt64;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt64,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = interface;
  PAsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = ^AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64;

  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt64,UInt64>
  IAsyncOperationWithProgress_2__UInt64__UInt64 = interface;
  PIAsyncOperationWithProgress_2__UInt64__UInt64 = ^IAsyncOperationWithProgress_2__UInt64__UInt64;

  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageItem>
  IVector_1__IStorageItem = interface;
  PIVector_1__IStorageItem = ^IVector_1__IStorageItem;

  // Windows.Storage Enums

  // Windows.Storage.AccessCache.AccessCacheOptions
  AccessCache_AccessCacheOptions = (
    None = 0,
    DisallowUserInput = 1,
    FastLocationsOnly = 2,
    UseReadOnlyCachedCopy = 4,
    SuppressAccessTimeUpdate = 8
  );
  PAccessCache_AccessCacheOptions = ^AccessCache_AccessCacheOptions;

  // Windows.Storage.AccessCache.RecentStorageItemVisibility
  AccessCache_RecentStorageItemVisibility = (
    AppOnly = 0,
    AppAndSystem = 1
  );
  PAccessCache_RecentStorageItemVisibility = ^AccessCache_RecentStorageItemVisibility;

  // Windows.Storage.ApplicationDataCreateDisposition
  ApplicationDataCreateDisposition = (
    Always = 0,
    Existing = 1
  );
  PApplicationDataCreateDisposition = ^ApplicationDataCreateDisposition;

  // Windows.Storage.ApplicationDataLocality
  ApplicationDataLocality = (
    Local = 0,
    Roaming = 1,
    Temporary = 2,
    LocalCache = 3
  );
  PApplicationDataLocality = ^ApplicationDataLocality;

  // Windows.Storage.Compression.CompressAlgorithm
  Compression_CompressAlgorithm = (
    InvalidAlgorithm = 0,
    NullAlgorithm = 1,
    Mszip = 2,
    Xpress = 3,
    XpressHuff = 4,
    Lzms = 5
  );
  PCompression_CompressAlgorithm = ^Compression_CompressAlgorithm;

  // Windows.Storage.FileProperties.PhotoOrientation
  FileProperties_PhotoOrientation = (
    Unspecified = 0,
    Normal = 1,
    FlipHorizontal = 2,
    Rotate180 = 3,
    FlipVertical = 4,
    Transpose = 5,
    Rotate270 = 6,
    Transverse = 7,
    Rotate90 = 8
  );
  PFileProperties_PhotoOrientation = ^FileProperties_PhotoOrientation;

  // Windows.Storage.FileProperties.ThumbnailType
  FileProperties_ThumbnailType = (
    Image = 0,
    Icon = 1
  );
  PFileProperties_ThumbnailType = ^FileProperties_ThumbnailType;

  // Windows.Storage.FileProperties.VideoOrientation
  FileProperties_VideoOrientation = (
    Normal = 0,
    Rotate90 = 90,
    Rotate180 = 180,
    Rotate270 = 270
  );
  PFileProperties_VideoOrientation = ^FileProperties_VideoOrientation;

  // Windows.Storage.KnownFolderId
  KnownFolderId = (
    AppCaptures = 0,
    CameraRoll = 1,
    DocumentsLibrary = 2,
    HomeGroup = 3,
    MediaServerDevices = 4,
    MusicLibrary = 5,
    Objects3D = 6,
    PicturesLibrary = 7,
    Playlists = 8,
    RecordedCalls = 9,
    RemovableDevices = 10,
    SavedPictures = 11,
    Screenshots = 12,
    VideosLibrary = 13,
    AllAppMods = 14,
    CurrentAppMods = 15
  );
  PKnownFolderId = ^KnownFolderId;

  // Windows.Storage.KnownFoldersAccessStatus
  KnownFoldersAccessStatus = (
    DeniedBySystem = 0,
    NotDeclaredByApp = 1,
    DeniedByUser = 2,
    UserPromptRequired = 3,
    Allowed = 4
  );
  PKnownFoldersAccessStatus = ^KnownFoldersAccessStatus;

  // Windows.Storage.KnownLibraryId
  KnownLibraryId = (
    Music = 0,
    Pictures = 1,
    Videos = 2,
    Documents = 3
  );
  PKnownLibraryId = ^KnownLibraryId;

  // Windows.Storage.Pickers.PickerLocationId
  Pickers_PickerLocationId = (
    DocumentsLibrary = 0,
    ComputerFolder = 1,
    Desktop = 2,
    Downloads = 3,
    HomeGroup = 4,
    MusicLibrary = 5,
    PicturesLibrary = 6,
    VideosLibrary = 7,
    Objects3D = 8,
    Unspecified = 9
  );
  PPickers_PickerLocationId = ^Pickers_PickerLocationId;

  // Windows.Storage.Pickers.PickerViewMode
  Pickers_PickerViewMode = (
    List = 0,
    Thumbnail = 1
  );
  PPickers_PickerViewMode = ^Pickers_PickerViewMode;

  // Windows.Storage.Pickers.Provider.AddFileResult
  Pickers_Provider_AddFileResult = (
    Added = 0,
    AlreadyAdded = 1,
    NotAllowed = 2,
    Unavailable = 3
  );
  PPickers_Provider_AddFileResult = ^Pickers_Provider_AddFileResult;

  // Windows.Storage.Pickers.Provider.FileSelectionMode
  Pickers_Provider_FileSelectionMode = (
    Single = 0,
    Multiple = 1
  );
  PPickers_Provider_FileSelectionMode = ^Pickers_Provider_FileSelectionMode;

  // Windows.Storage.Pickers.Provider.SetFileNameResult
  Pickers_Provider_SetFileNameResult = (
    Succeeded = 0,
    NotAllowed = 1,
    Unavailable = 2
  );
  PPickers_Provider_SetFileNameResult = ^Pickers_Provider_SetFileNameResult;

  // Windows.Storage.Provider.CachedFileOptions
  Provider_CachedFileOptions = (
    None = 0,
    RequireUpdateOnAccess = 1,
    UseCachedFileWhenOffline = 2,
    DenyAccessWhenOffline = 4
  );
  PProvider_CachedFileOptions = ^Provider_CachedFileOptions;

  // Windows.Storage.Provider.CachedFileTarget
  Provider_CachedFileTarget = (
    Local = 0,
    Remote = 1
  );
  PProvider_CachedFileTarget = ^Provider_CachedFileTarget;

  // Windows.Storage.Provider.FileUpdateStatus
  Provider_FileUpdateStatus = (
    Incomplete = 0,
    Complete = 1,
    UserInputNeeded = 2,
    CurrentlyUnavailable = 3,
    Failed = 4,
    CompleteAndRenamed = 5
  );
  PProvider_FileUpdateStatus = ^Provider_FileUpdateStatus;

  // Windows.Storage.Provider.ReadActivationMode
  Provider_ReadActivationMode = (
    NotNeeded = 0,
    BeforeAccess = 1
  );
  PProvider_ReadActivationMode = ^Provider_ReadActivationMode;

  // Windows.Storage.Provider.StorageProviderHardlinkPolicy
  Provider_StorageProviderHardlinkPolicy = (
    None = 0,
    Allowed = 1
  );
  PProvider_StorageProviderHardlinkPolicy = ^Provider_StorageProviderHardlinkPolicy;

  // Windows.Storage.Provider.StorageProviderHydrationPolicy
  Provider_StorageProviderHydrationPolicy = (
    Partial = 0,
    Progressive = 1,
    Full = 2,
    AlwaysFull = 3
  );
  PProvider_StorageProviderHydrationPolicy = ^Provider_StorageProviderHydrationPolicy;

  // Windows.Storage.Provider.StorageProviderHydrationPolicyModifier
  Provider_StorageProviderHydrationPolicyModifier = (
    None = 0,
    ValidationRequired = 1,
    StreamingAllowed = 2,
    AutoDehydrationAllowed = 4
  );
  PProvider_StorageProviderHydrationPolicyModifier = ^Provider_StorageProviderHydrationPolicyModifier;

  // Windows.Storage.Provider.StorageProviderInSyncPolicy
  Provider_StorageProviderInSyncPolicy = (
    Default = 0,
    FileCreationTime = 1,
    FileReadOnlyAttribute = 2,
    FileHiddenAttribute = 4,
    FileSystemAttribute = 8,
    DirectoryCreationTime = 16,
    DirectoryReadOnlyAttribute = 32,
    DirectoryHiddenAttribute = 64,
    DirectorySystemAttribute = 128,
    FileLastWriteTime = 256,
    DirectoryLastWriteTime = 512,
    PreserveInsyncForSyncEngine = -2147483648
  );
  PProvider_StorageProviderInSyncPolicy = ^Provider_StorageProviderInSyncPolicy;

  // Windows.Storage.Provider.StorageProviderPopulationPolicy
  Provider_StorageProviderPopulationPolicy = (
    Full = 1,
    AlwaysFull = 2
  );
  PProvider_StorageProviderPopulationPolicy = ^Provider_StorageProviderPopulationPolicy;

  // Windows.Storage.Provider.StorageProviderProtectionMode
  Provider_StorageProviderProtectionMode = (
    Unknown = 0,
    Personal = 1
  );
  PProvider_StorageProviderProtectionMode = ^Provider_StorageProviderProtectionMode;

  // Windows.Storage.Provider.StorageProviderUriSourceStatus
  Provider_StorageProviderUriSourceStatus = (
    Success = 0,
    NoSyncRoot = 1,
    FileNotFound = 2
  );
  PProvider_StorageProviderUriSourceStatus = ^Provider_StorageProviderUriSourceStatus;

  // Windows.Storage.Provider.UIStatus
  Provider_UIStatus = (
    Unavailable = 0,
    Hidden = 1,
    Visible = 2,
    Complete = 3
  );
  PProvider_UIStatus = ^Provider_UIStatus;

  // Windows.Storage.Provider.WriteActivationMode
  Provider_WriteActivationMode = (
    ReadOnly = 0,
    NotNeeded = 1,
    AfterWrite = 2
  );
  PProvider_WriteActivationMode = ^Provider_WriteActivationMode;

  // Windows.Storage.Search.CommonFileQuery
  Search_CommonFileQuery = (
    DefaultQuery = 0,
    OrderByName = 1,
    OrderByTitle = 2,
    OrderByMusicProperties = 3,
    OrderBySearchRank = 4,
    OrderByDate = 5
  );
  PSearch_CommonFileQuery = ^Search_CommonFileQuery;

  // Windows.Storage.Search.CommonFolderQuery
  Search_CommonFolderQuery = (
    DefaultQuery = 0,
    GroupByYear = 100,
    GroupByMonth = 101,
    GroupByArtist = 102,
    GroupByAlbum = 103,
    GroupByAlbumArtist = 104,
    GroupByComposer = 105,
    GroupByGenre = 106,
    GroupByPublishedYear = 107,
    GroupByRating = 108,
    GroupByTag = 109,
    GroupByAuthor = 110,
    GroupByType = 111
  );
  PSearch_CommonFolderQuery = ^Search_CommonFolderQuery;

  // Windows.Storage.Search.IndexedState
  Search_IndexedState = (
    Unknown = 0,
    NotIndexed = 1,
    PartiallyIndexed = 2,
    FullyIndexed = 3
  );
  PSearch_IndexedState = ^Search_IndexedState;

  // Windows.Storage.StorageLibraryChangeType
  StorageLibraryChangeType = (
    Created = 0,
    Deleted = 1,
    MovedOrRenamed = 2,
    ContentsChanged = 3,
    MovedOutOfLibrary = 4,
    MovedIntoLibrary = 5,
    ContentsReplaced = 6,
    IndexingStatusChanged = 7,
    EncryptionChanged = 8,
    ChangeTrackingLost = 9
  );
  PStorageLibraryChangeType = ^StorageLibraryChangeType;

  // Windows.Storage.StorageOpenOptions
  StorageOpenOptions = (
    None = 0,
    AllowOnlyReaders = 1,
    AllowReadersAndWriters = 2
  );
  PStorageOpenOptions = ^StorageOpenOptions;

  // Windows.Storage.StreamedFileFailureMode
  StreamedFileFailureMode = (
    Failed = 0,
    CurrentlyUnavailable = 1,
    Incomplete = 2
  );
  PStreamedFileFailureMode = ^StreamedFileFailureMode;

  // Windows.Storage Records
  // Windows.Storage.AccessCache.AccessListEntry
  AccessCache_AccessListEntry = record
    Token: HSTRING;
    Metadata: HSTRING;
  end;
  PAccessCache_AccessListEntry = ^AccessCache_AccessListEntry;

  // Windows.Storage.Provider.CloudFilesContract
  Provider_CloudFilesContract = record
  end;
  PProvider_CloudFilesContract = ^Provider_CloudFilesContract;

  // Windows.Storage Interfaces

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageFile>
  IIterator_1__IStorageFile_Base = interface(IInspectable)
  ['{314D2318-74EE-535C-B361-2144DBC573A0}']
    function get_Current: IStorageFile; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStorageFile): Cardinal; safecall;
    property Current: IStorageFile read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageFile>
  IIterator_1__IStorageFile = interface(IIterator_1__IStorageFile_Base)
  ['{314D2318-74EE-535C-B361-2144DBC573A0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageFolder>
  IIterator_1__IStorageFolder_Base = interface(IInspectable)
  ['{5AAC96FB-B3B9-5A7F-A920-4B5A8DF81168}']
    function get_Current: IStorageFolder; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStorageFolder): Cardinal; safecall;
    property Current: IStorageFolder read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageFolder>
  IIterator_1__IStorageFolder = interface(IIterator_1__IStorageFolder_Base)
  ['{73E42360-10FC-513C-9C38-D7FB007ABBC4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageFolder>
  IIterable_1__IStorageFolder_Base = interface(IInspectable)
  ['{4669BEFC-AE5C-52B1-8A97-5466CE61E94E}']
    function First: IIterator_1__IStorageFolder; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageFolder>
  IIterable_1__IStorageFolder = interface(IIterable_1__IStorageFolder_Base)
  ['{D09B688F-402A-5B3F-B552-1FFA23116952}']
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.IFileRemovedEventArgs
  Pickers_Provider_IFileRemovedEventArgs = interface(IInspectable)
  ['{13043DA7-7FCA-4C2B-9ECA-6890F9F00185}']
    function get_Id: HSTRING; safecall;
    property Id: HSTRING read get_Id;
  end deprecated;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileOpenPickerUI,Windows.Storage.Pickers.Provider.IFileRemovedEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IFileRemovedEventArgs_Delegate_Base = interface(IUnknown)
  ['{F1FB2939-695B-5F56-841A-A52A7D148572}']
    procedure Invoke(sender: Pickers_Provider_IFileOpenPickerUI; args: Pickers_Provider_IFileRemovedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileOpenPickerUI,Windows.Storage.Pickers.Provider.IFileRemovedEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IFileRemovedEventArgs = interface(TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IFileRemovedEventArgs_Delegate_Base)
  ['{9062BA3B-52D6-5970-A152-CA057E32A9C4}']
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.IPickerClosingDeferral
  Pickers_Provider_IPickerClosingDeferral = interface(IInspectable)
  ['{7AF7F71E-1A67-4A31-AE80-E907708A619B}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.IPickerClosingOperation
  Pickers_Provider_IPickerClosingOperation = interface(IInspectable)
  ['{4CE9FB84-BEEE-4E39-A773-FC5F0EAE328D}']
    function GetDeferral: Pickers_Provider_IPickerClosingDeferral; safecall;
    function get_Deadline: DateTime; safecall;
    property Deadline: DateTime read get_Deadline;
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.IPickerClosingEventArgs
  Pickers_Provider_IPickerClosingEventArgs = interface(IInspectable)
  ['{7E59F224-B332-4F12-8B9F-A8C2F06B32CD}']
    function get_ClosingOperation: Pickers_Provider_IPickerClosingOperation; safecall;
    function get_IsCanceled: Boolean; safecall;
    property ClosingOperation: Pickers_Provider_IPickerClosingOperation read get_ClosingOperation;
    property IsCanceled: Boolean read get_IsCanceled;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileOpenPickerUI,Windows.Storage.Pickers.Provider.IPickerClosingEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IPickerClosingEventArgs_Delegate_Base = interface(IUnknown)
  ['{2B06BDAC-983B-5552-B5C9-B0990A2DB3A1}']
    procedure Invoke(sender: Pickers_Provider_IFileOpenPickerUI; args: Pickers_Provider_IPickerClosingEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileOpenPickerUI,Windows.Storage.Pickers.Provider.IPickerClosingEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IPickerClosingEventArgs = interface(TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IPickerClosingEventArgs_Delegate_Base)
  ['{48264217-467D-5D3F-9321-B49DC3C6CE4C}']
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.IFileOpenPickerUI
  Pickers_Provider_IFileOpenPickerUI = interface(IInspectable)
  ['{DDA45A10-F9D4-40C4-8AF5-C5B6B5A61D1D}']
    function AddFile(id: HSTRING; &file: IStorageFile): Pickers_Provider_AddFileResult; safecall;
    procedure RemoveFile(id: HSTRING); safecall;
    function ContainsFile(id: HSTRING): Boolean; safecall;
    function CanAddFile(&file: IStorageFile): Boolean; safecall;
    function get_AllowedFileTypes: IVectorView_1__HSTRING; safecall;
    function get_SelectionMode: Pickers_Provider_FileSelectionMode; safecall;
    function get_SettingsIdentifier: HSTRING; safecall;
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function add_FileRemoved(handler: TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IFileRemovedEventArgs): EventRegistrationToken; safecall;
    procedure remove_FileRemoved(token: EventRegistrationToken); safecall;
    function add_Closing(handler: TypedEventHandler_2__Pickers_Provider_IFileOpenPickerUI__Pickers_Provider_IPickerClosingEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closing(token: EventRegistrationToken); safecall;
    property AllowedFileTypes: IVectorView_1__HSTRING read get_AllowedFileTypes;
    property SelectionMode: Pickers_Provider_FileSelectionMode read get_SelectionMode;
    property SettingsIdentifier: HSTRING read get_SettingsIdentifier;
    property Title: HSTRING read get_Title write put_Title;
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.ITargetFileRequestDeferral
  Pickers_Provider_ITargetFileRequestDeferral = interface(IInspectable)
  ['{4AEE9D91-BF15-4DA9-95F6-F6B7D558225B}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.ITargetFileRequest
  Pickers_Provider_ITargetFileRequest = interface(IInspectable)
  ['{42BD3355-7F88-478B-8E81-690B20340678}']
    function get_TargetFile: IStorageFile; safecall;
    procedure put_TargetFile(value: IStorageFile); safecall;
    function GetDeferral: Pickers_Provider_ITargetFileRequestDeferral; safecall;
    property TargetFile: IStorageFile read get_TargetFile write put_TargetFile;
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.ITargetFileRequestedEventArgs
  Pickers_Provider_ITargetFileRequestedEventArgs = interface(IInspectable)
  ['{B163DBC1-1B51-4C89-A591-0FD40B3C57C9}']
    function get_Request: Pickers_Provider_ITargetFileRequest; safecall;
    property Request: Pickers_Provider_ITargetFileRequest read get_Request;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Windows.Storage.Pickers.Provider.ITargetFileRequestedEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__Pickers_Provider_ITargetFileRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{D3E1F8C7-45C5-5249-B336-A111BFAA985B}']
    procedure Invoke(sender: Pickers_Provider_IFileSavePickerUI; args: Pickers_Provider_ITargetFileRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Windows.Storage.Pickers.Provider.ITargetFileRequestedEventArgs>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__Pickers_Provider_ITargetFileRequestedEventArgs = interface(TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__Pickers_Provider_ITargetFileRequestedEventArgs_Delegate_Base)
  ['{87064D8A-E13C-537D-BD5D-DF5B0CB866CC}']
  end;

  // UsedAPI Interface
  // Windows.Storage.Pickers.Provider.IFileSavePickerUI
  Pickers_Provider_IFileSavePickerUI = interface(IInspectable)
  ['{9656C1E7-3E56-43CC-8A39-33C73D9D542B}']
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_AllowedFileTypes: IVectorView_1__HSTRING; safecall;
    function get_SettingsIdentifier: HSTRING; safecall;
    function get_FileName: HSTRING; safecall;
    function TrySetFileName(value: HSTRING): Pickers_Provider_SetFileNameResult; safecall;
    function add_FileNameChanged(handler: TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable): EventRegistrationToken; safecall;
    procedure remove_FileNameChanged(token: EventRegistrationToken); safecall;
    function add_TargetFileRequested(handler: TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__Pickers_Provider_ITargetFileRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_TargetFileRequested(token: EventRegistrationToken); safecall;
    property AllowedFileTypes: IVectorView_1__HSTRING read get_AllowedFileTypes;
    property FileName: HSTRING read get_FileName;
    property SettingsIdentifier: HSTRING read get_SettingsIdentifier;
    property Title: HSTRING read get_Title write put_Title;
  end;

  // UsedAPI Interface
  // Windows.Storage.Provider.IFileUpdateRequestDeferral
  Provider_IFileUpdateRequestDeferral = interface(IInspectable)
  ['{FFCEDB2B-8ADE-44A5-BB00-164C4E72F13A}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.Provider.IFileUpdateRequest
  Provider_IFileUpdateRequest = interface(IInspectable)
  ['{40C82536-C1FE-4D93-A792-1E736BC70837}']
    function get_ContentId: HSTRING; safecall;
    function get_File: IStorageFile; safecall;
    function get_Status: Provider_FileUpdateStatus; safecall;
    procedure put_Status(value: Provider_FileUpdateStatus); safecall;
    function GetDeferral: Provider_IFileUpdateRequestDeferral; safecall;
    procedure UpdateLocalFile(value: IStorageFile); safecall;
    property ContentId: HSTRING read get_ContentId;
    property &File: IStorageFile read get_File;
    property Status: Provider_FileUpdateStatus read get_Status write put_Status;
  end;

  // UsedAPI Interface
  // Windows.Storage.Provider.IFileUpdateRequestedEventArgs
  Provider_IFileUpdateRequestedEventArgs = interface(IInspectable)
  ['{7B0A9342-3905-438D-AAEF-78AE265F8DD2}']
    function get_Request: Provider_IFileUpdateRequest; safecall;
    property Request: Provider_IFileUpdateRequest read get_Request;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Windows.Storage.Provider.IFileUpdateRequestedEventArgs>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__Provider_IFileUpdateRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{ED56AB72-EBD3-52C8-B0AC-987D30090351}']
    procedure Invoke(sender: Provider_ICachedFileUpdaterUI; args: Provider_IFileUpdateRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Windows.Storage.Provider.IFileUpdateRequestedEventArgs>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__Provider_IFileUpdateRequestedEventArgs = interface(TypedEventHandler_2__Provider_ICachedFileUpdaterUI__Provider_IFileUpdateRequestedEventArgs_Delegate_Base)
  ['{79F8EA77-4833-55EF-9EF5-E1DE8A55D466}']
  end;

  // UsedAPI Interface
  // Windows.Storage.Provider.ICachedFileUpdaterUI
  Provider_ICachedFileUpdaterUI = interface(IInspectable)
  ['{9E6F41E6-BAF2-4A97-B600-9333F5DF80FD}']
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_UpdateTarget: Provider_CachedFileTarget; safecall;
    function add_FileUpdateRequested(handler: TypedEventHandler_2__Provider_ICachedFileUpdaterUI__Provider_IFileUpdateRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_FileUpdateRequested(token: EventRegistrationToken); safecall;
    function add_UIRequested(handler: TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable): EventRegistrationToken; safecall;
    procedure remove_UIRequested(token: EventRegistrationToken); safecall;
    function get_UIStatus: Provider_UIStatus; safecall;
    property Title: HSTRING read get_Title write put_Title;
    property UIStatus: Provider_UIStatus read get_UIStatus;
    property UpdateTarget: Provider_CachedFileTarget read get_UpdateTarget;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Search.SortEntry>
  IIterator_1__Search_SortEntry_Base = interface(IInspectable)
  ['{520434A2-ACF7-58C9-B47A-2741F2FAC2C2}']
    function get_Current: Search_SortEntry; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSearch_SortEntry): Cardinal; safecall;
    property Current: Search_SortEntry read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Search.SortEntry>
  IIterator_1__Search_SortEntry = interface(IIterator_1__Search_SortEntry_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // UsedAPI Interface
  // Windows.Storage.Search.IStorageFileQueryResult
  Search_IStorageFileQueryResult = interface(IInspectable)
  ['{52FDA447-2BAA-412C-B29F-D4B1778EFA1E}']
    function GetFilesAsync(startIndex: Cardinal; maxNumberOfItems: Cardinal): IAsyncOperation_1__IVectorView_1__IStorageFile; overload; safecall;
    function GetFilesAsync: IAsyncOperation_1__IVectorView_1__IStorageFile; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.IStorageLibraryChange
  IStorageLibraryChange = interface(IInspectable)
  ['{00980B23-2BE2-4909-AA48-159F5203A51E}']
    function get_ChangeType: StorageLibraryChangeType; safecall;
    function get_Path: HSTRING; safecall;
    function get_PreviousPath: HSTRING; safecall;
    function IsOfType(&type: StorageItemTypes): Boolean; safecall;
    function GetStorageItemAsync: IAsyncOperation_1__IStorageItem; safecall;
    property ChangeType: StorageLibraryChangeType read get_ChangeType;
    property Path: HSTRING read get_Path;
    property PreviousPath: HSTRING read get_PreviousPath;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageLibraryChange>
  IIterator_1__IStorageLibraryChange_Base = interface(IInspectable)
  ['{C48A1103-56E6-5398-84FE-92EDAD7FC111}']
    function get_Current: IStorageLibraryChange; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIStorageLibraryChange): Cardinal; safecall;
    property Current: IStorageLibraryChange read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.IStorageLibraryChange>
  IIterator_1__IStorageLibraryChange = interface(IIterator_1__IStorageLibraryChange_Base)
  ['{30C1A965-C656-5672-9BE2-1DC86984A70A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageLibraryChange>
  IIterable_1__IStorageLibraryChange_Base = interface(IInspectable)
  ['{87C15DFC-0C5E-518B-9206-97D3D9823C61}']
    function First: IIterator_1__IStorageLibraryChange; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.IStorageLibraryChange>
  IIterable_1__IStorageLibraryChange = interface(IIterable_1__IStorageLibraryChange_Base)
  ['{BF45177C-DC90-58C4-91DC-081E21406782}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>
  IVectorView_1__IStorageLibraryChange = interface(IInspectable)
  ['{9851C198-5896-5B2D-A52D-1A721428E511}']
    function GetAt(index: Cardinal): IStorageLibraryChange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IStorageLibraryChange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorageLibraryChange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange_Delegate_Base = interface(IUnknown)
  ['{AB9CEA41-6DF8-535D-8171-46AFF187158F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IStorageLibraryChange; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange_Delegate_Base)
  ['{1FDA1C60-48AF-5CF2-B304-82417ACA5F26}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>>
  IAsyncOperation_1__IVectorView_1__IStorageLibraryChange_Base = interface(IInspectable)
  ['{66E11B8A-9003-52C9-84A8-AE5CCEBE8CF9}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange; safecall;
    function GetResults: IVectorView_1__IStorageLibraryChange; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IStorageLibraryChange read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.IStorageLibraryChange>>
  IAsyncOperation_1__IVectorView_1__IStorageLibraryChange = interface(IAsyncOperation_1__IVectorView_1__IStorageLibraryChange_Base)
  ['{48707D46-94D7-5B58-AD34-EDEB602EE805}']
  end;

  // UsedAPI Interface
  // Windows.Storage.IStorageLibraryChangeReader
  IStorageLibraryChangeReader = interface(IInspectable)
  ['{F205BC83-FCA2-41F9-8954-EE2E991EB96F}']
    function ReadBatchAsync: IAsyncOperation_1__IVectorView_1__IStorageLibraryChange; safecall;
    function AcceptChangesAsync: IAsyncAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.IStorageLibraryChangeTracker
  IStorageLibraryChangeTracker = interface(IInspectable)
  ['{9E157316-6073-44F6-9681-7492D1286C90}']
    function GetChangeReader: IStorageLibraryChangeReader; safecall;
    procedure Enable; safecall;
    procedure Reset; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Storage.IStorageFolder>
  VectorChangedEventHandler_1__IStorageFolder_Delegate_Base = interface(IUnknown)
  ['{2057B641-4B9B-5338-A19A-E9A951916775}']
    procedure Invoke(sender: IObservableVector_1__IStorageFolder; event: IVectorChangedEventArgs); safecall;
  end;
  // Windows.Foundation.Collections.VectorChangedEventHandler`1<Windows.Storage.IStorageFolder>
  VectorChangedEventHandler_1__IStorageFolder = interface(VectorChangedEventHandler_1__IStorageFolder_Delegate_Base)
  ['{02B1C206-D4FA-5E88-BD76-9C158F6CFAFF}']
  end;

  // Windows.Foundation.Collections.IObservableVector`1<Windows.Storage.IStorageFolder>
  IObservableVector_1__IStorageFolder = interface(IInspectable)
  ['{772A1791-7293-589A-A10E-5F888E78FA05}']
    function add_VectorChanged(vhnd: VectorChangedEventHandler_1__IStorageFolder): EventRegistrationToken; safecall;
    procedure remove_VectorChanged(token: EventRegistrationToken); safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.ISetVersionDeferral
  ISetVersionDeferral = interface(IInspectable)
  ['{033508A2-781A-437A-B078-3F32BADCFE47}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.ISetVersionRequest
  ISetVersionRequest = interface(IInspectable)
  ['{B9C76B9B-1056-4E69-8330-162619956F9B}']
    function get_CurrentVersion: Cardinal; safecall;
    function get_DesiredVersion: Cardinal; safecall;
    function GetDeferral: ISetVersionDeferral; safecall;
    property CurrentVersion: Cardinal read get_CurrentVersion;
    property DesiredVersion: Cardinal read get_DesiredVersion;
  end;

  // UsedAPI Interface
  // Windows.Storage.ApplicationDataSetVersionHandler
  ApplicationDataSetVersionHandler = interface(IUnknown)
  ['{A05791E6-CC9F-4687-ACAB-A364FD785463}']
    procedure Invoke(setVersionRequest: ISetVersionRequest); safecall;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>
  IKeyValuePair_2__HSTRING__IApplicationDataContainer = interface(IInspectable)
  ['{9C29CDD1-F5FA-5E4B-BC39-A1FDB2E48959}']
    function get_Key: HSTRING; safecall;
    function get_Value: IApplicationDataContainer; safecall;
    property Key: HSTRING read get_Key;
    property Value: IApplicationDataContainer read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>>
  IIterator_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer_Base = interface(IInspectable)
  ['{AF3C131D-67AA-5C8D-AE0E-272BA24AE74F}']
    function get_Current: IKeyValuePair_2__HSTRING__IApplicationDataContainer; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IApplicationDataContainer): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IApplicationDataContainer read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>>
  IIterator_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer = interface(IIterator_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer_Base)
  ['{C00495D1-304D-5005-8F60-35610C7A70A1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>>
  IIterable_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer_Base = interface(IInspectable)
  ['{A785BE1D-159E-53AD-9553-598B03DCA048}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.IApplicationDataContainer>>
  IIterable_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer = interface(IIterable_1__IKeyValuePair_2__HSTRING__IApplicationDataContainer_Base)
  ['{BE784D1D-41C6-557E-8776-DFDEF79D733E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.IApplicationDataContainer>
  IMapView_2__HSTRING__IApplicationDataContainer_Base = interface(IInspectable)
  ['{13624F8D-85CC-5780-A78D-64DBA58F2C3C}']
    function Lookup(key: HSTRING): IApplicationDataContainer; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IApplicationDataContainer; out second: IMapView_2__HSTRING__IApplicationDataContainer); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.IApplicationDataContainer>
  IMapView_2__HSTRING__IApplicationDataContainer = interface(IMapView_2__HSTRING__IApplicationDataContainer_Base)
  ['{EB2E8596-3B90-592B-83DE-3A1AFD8B48A7}']
  end;

  // UsedAPI Interface
  // Windows.Storage.IApplicationDataContainer
  IApplicationDataContainer = interface(IInspectable)
  ['{C5AEFD1E-F467-40BA-8566-AB640A441E1D}']
    function get_Name: HSTRING; safecall;
    function get_Locality: ApplicationDataLocality; safecall;
    function get_Values: IPropertySet; safecall;
    function get_Containers: IMapView_2__HSTRING__IApplicationDataContainer; safecall;
    function CreateContainer(name: HSTRING; disposition: ApplicationDataCreateDisposition): IApplicationDataContainer; safecall;
    procedure DeleteContainer(name: HSTRING); safecall;
    property Containers: IMapView_2__HSTRING__IApplicationDataContainer read get_Containers;
    property Locality: ApplicationDataLocality read get_Locality;
    property Name: HSTRING read get_Name;
    property Values: IPropertySet read get_Values;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IApplicationData
  [WinRTClassNameAttribute(SWindows_Storage_ApplicationData)]
  IApplicationData = interface(IInspectable)
  ['{C3DA6FB7-B744-4B45-B0B8-223A0938D0DC}']
    function get_Version: Cardinal; safecall;
    function SetVersionAsync(desiredVersion: Cardinal; handler: ApplicationDataSetVersionHandler): IAsyncAction; safecall;
    function ClearAsync: IAsyncAction; overload; safecall;
    function ClearAsync(locality: ApplicationDataLocality): IAsyncAction; overload; safecall;
    function get_LocalSettings: IApplicationDataContainer; safecall;
    function get_RoamingSettings: IApplicationDataContainer; safecall;
    function get_LocalFolder: IStorageFolder; safecall;
    function get_RoamingFolder: IStorageFolder; safecall;
    function get_TemporaryFolder: IStorageFolder; safecall;
    function add_DataChanged(handler: TypedEventHandler_2__IApplicationData__IInspectable): EventRegistrationToken; safecall;
    procedure remove_DataChanged(token: EventRegistrationToken); safecall;
    procedure SignalDataChanged; safecall;
    function get_RoamingStorageQuota: UInt64; safecall;
    property LocalFolder: IStorageFolder read get_LocalFolder;
    property LocalSettings: IApplicationDataContainer read get_LocalSettings;
    property RoamingFolder: IStorageFolder read get_RoamingFolder;
    property RoamingSettings: IApplicationDataContainer read get_RoamingSettings;
    property RoamingStorageQuota: UInt64 read get_RoamingStorageQuota;
    property TemporaryFolder: IStorageFolder read get_TemporaryFolder;
    property Version: Cardinal read get_Version;
  end;

  // UsedAPI Interface
  // Windows.Storage.FileProperties.IMusicProperties
  FileProperties_IMusicProperties = interface(IInspectable)
  ['{BC8AAB62-66EC-419A-BC5D-CA65A4CB46DA}']
    function get_Album: HSTRING; safecall;
    procedure put_Album(value: HSTRING); safecall;
    function get_Artist: HSTRING; safecall;
    procedure put_Artist(value: HSTRING); safecall;
    function get_Genre: IVector_1__HSTRING; safecall;
    function get_TrackNumber: Cardinal; safecall;
    procedure put_TrackNumber(value: Cardinal); safecall;
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_Rating: Cardinal; safecall;
    procedure put_Rating(value: Cardinal); safecall;
    function get_Duration: TimeSpan; safecall;
    function get_Bitrate: Cardinal; safecall;
    function get_AlbumArtist: HSTRING; safecall;
    procedure put_AlbumArtist(value: HSTRING); safecall;
    function get_Composers: IVector_1__HSTRING; safecall;
    function get_Conductors: IVector_1__HSTRING; safecall;
    function get_Subtitle: HSTRING; safecall;
    procedure put_Subtitle(value: HSTRING); safecall;
    function get_Producers: IVector_1__HSTRING; safecall;
    function get_Publisher: HSTRING; safecall;
    procedure put_Publisher(value: HSTRING); safecall;
    function get_Writers: IVector_1__HSTRING; safecall;
    function get_Year: Cardinal; safecall;
    procedure put_Year(value: Cardinal); safecall;
    property Album: HSTRING read get_Album write put_Album;
    property AlbumArtist: HSTRING read get_AlbumArtist write put_AlbumArtist;
    property Artist: HSTRING read get_Artist write put_Artist;
    property Bitrate: Cardinal read get_Bitrate;
    property Composers: IVector_1__HSTRING read get_Composers;
    property Conductors: IVector_1__HSTRING read get_Conductors;
    property Duration: TimeSpan read get_Duration;
    property Genre: IVector_1__HSTRING read get_Genre;
    property Producers: IVector_1__HSTRING read get_Producers;
    property Publisher: HSTRING read get_Publisher write put_Publisher;
    property Rating: Cardinal read get_Rating write put_Rating;
    property Subtitle: HSTRING read get_Subtitle write put_Subtitle;
    property Title: HSTRING read get_Title write put_Title;
    property TrackNumber: Cardinal read get_TrackNumber write put_TrackNumber;
    property Writers: IVector_1__HSTRING read get_Writers;
    property Year: Cardinal read get_Year write put_Year;
  end;

  // UsedAPI Interface
  // Windows.Storage.FileProperties.IVideoProperties
  FileProperties_IVideoProperties = interface(IInspectable)
  ['{719AE507-68DE-4DB8-97DE-49998C059F2F}']
    function get_Rating: Cardinal; safecall;
    procedure put_Rating(value: Cardinal); safecall;
    function get_Keywords: IVector_1__HSTRING; safecall;
    function get_Width: Cardinal; safecall;
    function get_Height: Cardinal; safecall;
    function get_Duration: TimeSpan; safecall;
    function get_Latitude: IReference_1__Double; safecall;
    function get_Longitude: IReference_1__Double; safecall;
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_Subtitle: HSTRING; safecall;
    procedure put_Subtitle(value: HSTRING); safecall;
    function get_Producers: IVector_1__HSTRING; safecall;
    function get_Publisher: HSTRING; safecall;
    procedure put_Publisher(value: HSTRING); safecall;
    function get_Writers: IVector_1__HSTRING; safecall;
    function get_Year: Cardinal; safecall;
    procedure put_Year(value: Cardinal); safecall;
    function get_Bitrate: Cardinal; safecall;
    function get_Directors: IVector_1__HSTRING; safecall;
    function get_Orientation: FileProperties_VideoOrientation; safecall;
    property Bitrate: Cardinal read get_Bitrate;
    property Directors: IVector_1__HSTRING read get_Directors;
    property Duration: TimeSpan read get_Duration;
    property Height: Cardinal read get_Height;
    property Keywords: IVector_1__HSTRING read get_Keywords;
    property Latitude: IReference_1__Double read get_Latitude;
    property Longitude: IReference_1__Double read get_Longitude;
    property Orientation: FileProperties_VideoOrientation read get_Orientation;
    property Producers: IVector_1__HSTRING read get_Producers;
    property Publisher: HSTRING read get_Publisher write put_Publisher;
    property Rating: Cardinal read get_Rating write put_Rating;
    property Subtitle: HSTRING read get_Subtitle write put_Subtitle;
    property Title: HSTRING read get_Title write put_Title;
    property Width: Cardinal read get_Width;
    property Writers: IVector_1__HSTRING read get_Writers;
    property Year: Cardinal read get_Year write put_Year;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageFile>
  IVector_1__IStorageFile_Base = interface(IInspectable)
  ['{FCBC8B8B-6103-5B4E-BA00-4BC2CEDB6A35}']
    function GetAt(index: Cardinal): IStorageFile; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IStorageFile; safecall;
    function IndexOf(value: IStorageFile; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IStorageFile); safecall;
    procedure InsertAt(index: Cardinal; value: IStorageFile); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IStorageFile); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorageFile): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIStorageFile); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageFile>
  IVector_1__IStorageFile = interface(IVector_1__IStorageFile_Base)
  ['{021A3F69-AD1E-5FB9-978A-056984AE2B2D}']
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

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.AccessCache.AccessListEntry>
  IIterator_1__AccessCache_AccessListEntry_Base = interface(IInspectable)
  ['{D1A0A6C4-889D-519B-8508-26241B329B7E}']
    function get_Current: AccessCache_AccessListEntry; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAccessCache_AccessListEntry): Cardinal; safecall;
    property Current: AccessCache_AccessListEntry read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.AccessCache.AccessListEntry>
  IIterator_1__AccessCache_AccessListEntry = interface(IIterator_1__AccessCache_AccessListEntry_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
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

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMap`2<String,Object>>
  AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable_Delegate_Base = interface(IUnknown)
  ['{7344F356-8399-5756-A2F8-ABD50C4146FF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMap_2__HSTRING__IInspectable; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMap`2<String,Object>>
  AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable = interface(AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable_Delegate_Base)
  ['{7344F356-8399-5756-A2F8-ABD50C4146FF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMap`2<String,Object>>
  IAsyncOperation_1__IMap_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{127E39C7-07C1-58E5-B48E-3A4729839FEC}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable; safecall;
    function GetResults: IMap_2__HSTRING__IInspectable; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMap_2__HSTRING__IInspectable read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMap`2<String,Object>>
  IAsyncOperation_1__IMap_2__HSTRING__IInspectable = interface(IAsyncOperation_1__IMap_2__HSTRING__IInspectable_Base)
  ['{127E39C7-07C1-58E5-B48E-3A4729839FEC}']
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

  // Windows.Foundation.IReference`1<Double>
  IReference_1__Double = interface(IInspectable)
  ['{2F2D6C29-5473-5F3E-92E7-96572BB990E2}']
    function get_Value: Double; safecall;
    property Value: Double read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Storage.FileProperties.IImageProperties
  FileProperties_IImageProperties = interface(IInspectable)
  ['{523C9424-FCFF-4275-AFEE-ECDB9AB47973}']
    function get_Rating: Cardinal; safecall;
    procedure put_Rating(value: Cardinal); safecall;
    function get_Keywords: IVector_1__HSTRING; safecall;
    function get_DateTaken: DateTime; safecall;
    procedure put_DateTaken(value: DateTime); safecall;
    function get_Width: Cardinal; safecall;
    function get_Height: Cardinal; safecall;
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_Latitude: IReference_1__Double; safecall;
    function get_Longitude: IReference_1__Double; safecall;
    function get_CameraManufacturer: HSTRING; safecall;
    procedure put_CameraManufacturer(value: HSTRING); safecall;
    function get_CameraModel: HSTRING; safecall;
    procedure put_CameraModel(value: HSTRING); safecall;
    function get_Orientation: FileProperties_PhotoOrientation; safecall;
    function get_PeopleNames: IVectorView_1__HSTRING; safecall;
    property CameraManufacturer: HSTRING read get_CameraManufacturer write put_CameraManufacturer;
    property CameraModel: HSTRING read get_CameraModel write put_CameraModel;
    property DateTaken: DateTime read get_DateTaken write put_DateTaken;
    property Height: Cardinal read get_Height;
    property Keywords: IVector_1__HSTRING read get_Keywords;
    property Latitude: IReference_1__Double read get_Latitude;
    property Longitude: IReference_1__Double read get_Longitude;
    property Orientation: FileProperties_PhotoOrientation read get_Orientation;
    property PeopleNames: IVectorView_1__HSTRING read get_PeopleNames;
    property Rating: Cardinal read get_Rating write put_Rating;
    property Title: HSTRING read get_Title write put_Title;
    property Width: Cardinal read get_Width;
  end;

  // UsedAPI Interface
  // Windows.Storage.FileProperties.IDocumentProperties
  FileProperties_IDocumentProperties = interface(IInspectable)
  ['{7EAB19BC-1821-4923-B4A9-0AEA404D0070}']
    function get_Author: IVector_1__HSTRING; safecall;
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_Keywords: IVector_1__HSTRING; safecall;
    function get_Comment: HSTRING; safecall;
    procedure put_Comment(value: HSTRING); safecall;
    property Author: IVector_1__HSTRING read get_Author;
    property Comment: HSTRING read get_Comment write put_Comment;
    property Keywords: IVector_1__HSTRING read get_Keywords;
    property Title: HSTRING read get_Title write put_Title;
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

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Search.IStorageQueryResultBase,Object>
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = interface(IUnknown)
  ['{4BA22861-00C4-597F-B6BF-3AF516F3B870}']
    procedure Invoke(sender: Search_IStorageQueryResultBase; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Compression.ICompressor
  [WinRTClassNameAttribute(SWindows_Storage_Compression_Compressor)]
  Compression_ICompressor = interface(IInspectable)
  ['{0AC3645A-57AC-4EE1-B702-84D39D5424E0}']
    function FinishAsync: IAsyncOperation_1__Boolean; safecall;
    function DetachStream: IOutputStream; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Compression.ICompressorFactory
  [WinRTClassNameAttribute(SWindows_Storage_Compression_Compressor)]
  Compression_ICompressorFactory = interface(IInspectable)
  ['{5F3D96A4-2CFB-442C-A8BA-D7D11B039DA0}']
    function CreateCompressor(underlyingStream: IOutputStream): Compression_ICompressor; safecall;
    function CreateCompressorEx(underlyingStream: IOutputStream; algorithm: Compression_CompressAlgorithm; blockSize: Cardinal): Compression_ICompressor; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Compression.IDecompressor
  [WinRTClassNameAttribute(SWindows_Storage_Compression_Decompressor)]
  Compression_IDecompressor = interface(IInspectable)
  ['{B883FE46-D68A-4C8B-ADA0-4EE813FC5283}']
    function DetachStream: IInputStream; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Compression.IDecompressorFactory
  [WinRTClassNameAttribute(SWindows_Storage_Compression_Decompressor)]
  Compression_IDecompressorFactory = interface(IInspectable)
  ['{5337E252-1DA2-42E1-8834-0379D28D742F}']
    function CreateDecompressor(underlyingStream: IInputStream): Compression_IDecompressor; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.FileProperties.IGeotagHelperStatics
  [WinRTClassNameAttribute(SWindows_Storage_FileProperties_GeotagHelper)]
  FileProperties_IGeotagHelperStatics = interface(IInspectable)
  ['{41493244-2524-4655-86A6-ED16F5FC716B}']
    function GetGeotagAsync(&file: IStorageFile): IAsyncOperation_1__IGeopoint; safecall;
    function SetGeotagFromGeolocatorAsync(&file: IStorageFile; geolocator: IGeolocator): IAsyncAction; safecall;
    function SetGeotagAsync(&file: IStorageFile; geopoint: IGeopoint): IAsyncAction; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IMusicProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties_Delegate_Base = interface(IUnknown)
  ['{D84E1312-D661-5B7F-9566-7421BDEDC1EA}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__FileProperties_IMusicProperties; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IMusicProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties = interface(AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties_Delegate_Base)
  ['{AE688CD9-E302-5657-82B5-1B974D1FA81F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IMusicProperties>
  IAsyncOperation_1__FileProperties_IMusicProperties_Base = interface(IInspectable)
  ['{0D023B76-20A7-56F3-84AB-CE31E6544B71}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties; safecall;
    function GetResults: FileProperties_IMusicProperties; safecall;
    property Completed: AsyncOperationCompletedHandler_1__FileProperties_IMusicProperties read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IMusicProperties>
  IAsyncOperation_1__FileProperties_IMusicProperties = interface(IAsyncOperation_1__FileProperties_IMusicProperties_Base)
  ['{D3A7B974-3A90-55E8-82D0-D22F2A7B4856}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IVideoProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties_Delegate_Base = interface(IUnknown)
  ['{43401D34-61AB-5CF2-921F-55B616631D1D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__FileProperties_IVideoProperties; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IVideoProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties = interface(AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties_Delegate_Base)
  ['{75953071-FA00-5044-9240-B48C6CEF57B0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IVideoProperties>
  IAsyncOperation_1__FileProperties_IVideoProperties_Base = interface(IInspectable)
  ['{447D4590-D3F9-58BF-AC58-6F9A50839EFE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties; safecall;
    function GetResults: FileProperties_IVideoProperties; safecall;
    property Completed: AsyncOperationCompletedHandler_1__FileProperties_IVideoProperties read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IVideoProperties>
  IAsyncOperation_1__FileProperties_IVideoProperties = interface(IAsyncOperation_1__FileProperties_IVideoProperties_Base)
  ['{C5B61E97-459B-5AA8-B350-2CF55E7C7D47}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IImageProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IImageProperties_Delegate_Base = interface(IUnknown)
  ['{C63729BC-E4C3-564C-B137-2CB4F5966A83}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__FileProperties_IImageProperties; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IImageProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IImageProperties = interface(AsyncOperationCompletedHandler_1__FileProperties_IImageProperties_Delegate_Base)
  ['{3E47A7AF-FC5E-5D68-A93B-98CEA696089D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IImageProperties>
  IAsyncOperation_1__FileProperties_IImageProperties_Base = interface(IInspectable)
  ['{FCD07511-E7F8-5BDA-8C04-795A639DAE8F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__FileProperties_IImageProperties); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__FileProperties_IImageProperties; safecall;
    function GetResults: FileProperties_IImageProperties; safecall;
    property Completed: AsyncOperationCompletedHandler_1__FileProperties_IImageProperties read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IImageProperties>
  IAsyncOperation_1__FileProperties_IImageProperties = interface(IAsyncOperation_1__FileProperties_IImageProperties_Base)
  ['{6248A622-38DA-5FC3-AA6B-374D26DAC6BA}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IDocumentProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties_Delegate_Base = interface(IUnknown)
  ['{4452ED4C-642B-501B-9617-7D68B4AC3C66}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__FileProperties_IDocumentProperties; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.FileProperties.IDocumentProperties>
  AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties = interface(AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties_Delegate_Base)
  ['{E4B7DD5B-B84A-5EF6-884A-5383529B637B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IDocumentProperties>
  IAsyncOperation_1__FileProperties_IDocumentProperties_Base = interface(IInspectable)
  ['{6C86E97C-5699-5700-8D35-D350AD3E4DF2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties; safecall;
    function GetResults: FileProperties_IDocumentProperties; safecall;
    property Completed: AsyncOperationCompletedHandler_1__FileProperties_IDocumentProperties read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.FileProperties.IDocumentProperties>
  IAsyncOperation_1__FileProperties_IDocumentProperties = interface(IAsyncOperation_1__FileProperties_IDocumentProperties_Base)
  ['{A861F227-13B9-5FA3-A62E-01F9C918BA48}']
  end;

  // UsedAPI Interface
  // Windows.Storage.FileProperties.IStorageItemContentProperties
  FileProperties_IStorageItemContentProperties = interface(IInspectable)
  ['{05294BAD-BC38-48BF-85D7-770E0E2AE0BA}']
    function GetMusicPropertiesAsync: IAsyncOperation_1__FileProperties_IMusicProperties; safecall;
    function GetVideoPropertiesAsync: IAsyncOperation_1__FileProperties_IVideoProperties; safecall;
    function GetImagePropertiesAsync: IAsyncOperation_1__FileProperties_IImageProperties; safecall;
    function GetDocumentPropertiesAsync: IAsyncOperation_1__FileProperties_IDocumentProperties; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.IApplicationData,Object>
  TypedEventHandler_2__IApplicationData__IInspectable_Delegate_Base = interface(IUnknown)
  ['{B5348B3B-5081-5AE9-8FA3-4D22D68FB0EA}']
    procedure Invoke(sender: IApplicationData; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.IApplicationData,Object>
  TypedEventHandler_2__IApplicationData__IInspectable = interface(TypedEventHandler_2__IApplicationData__IInspectable_Delegate_Base)
  ['{4E72D889-3D9A-5E3B-AA8A-3B37D17226DC}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IApplicationData2
  IApplicationData2 = interface(IInspectable)
  ['{9E65CD69-0BA3-4E32-BE29-B02DE6607638}']
    function get_LocalCacheFolder: IStorageFolder; safecall;
    property LocalCacheFolder: IStorageFolder read get_LocalCacheFolder;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IApplicationData3
  IApplicationData3 = interface(IInspectable)
  ['{DC222CF4-2772-4C1D-AA2C-C9F743ADE8D1}']
    function GetPublisherCacheFolder(folderName: HSTRING): IStorageFolder; safecall;
    function ClearPublisherCacheFolderAsync(folderName: HSTRING): IAsyncAction; safecall;
    function get_SharedLocalFolder: IStorageFolder; safecall;
    property SharedLocalFolder: IStorageFolder read get_SharedLocalFolder;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IApplicationDataStatics
  [WinRTClassNameAttribute(SWindows_Storage_ApplicationData)]
  IApplicationDataStatics = interface(IInspectable)
  ['{5612147B-E843-45E3-94D8-06169E3C8E17}']
    function get_Current: IApplicationData; safecall;
    property Current: IApplicationData read get_Current;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IApplicationData>
  AsyncOperationCompletedHandler_1__IApplicationData_Delegate_Base = interface(IUnknown)
  ['{ABAFE590-65FE-520A-9D7C-6AB5F1882237}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IApplicationData; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.IApplicationData>
  AsyncOperationCompletedHandler_1__IApplicationData = interface(AsyncOperationCompletedHandler_1__IApplicationData_Delegate_Base)
  ['{C30AEE19-A1A1-5CBD-9B91-DCC5DB72F16B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IApplicationData>
  IAsyncOperation_1__IApplicationData_Base = interface(IInspectable)
  ['{31456B58-A5CB-5C5B-BD6E-CCCE3A7BF4B4}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IApplicationData); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IApplicationData; safecall;
    function GetResults: IApplicationData; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IApplicationData read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.IApplicationData>
  IAsyncOperation_1__IApplicationData = interface(IAsyncOperation_1__IApplicationData_Base)
  ['{141FA633-8D79-53C8-991A-60A5DA990CAE}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IApplicationDataStatics2
  [WinRTClassNameAttribute(SWindows_Storage_ApplicationData)]
  IApplicationDataStatics2 = interface(IInspectable)
  ['{CD606211-CF49-40A4-A47C-C7F0DBBA8107}']
    function GetForUserAsync(user: IUser): IAsyncOperation_1__IApplicationData; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Provider.FileUpdateStatus>
  AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus_Delegate_Base = interface(IUnknown)
  ['{BB185A07-0285-5F37-9C7D-2FC6A3E0E6E5}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Provider_FileUpdateStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Provider.FileUpdateStatus>
  AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus = interface(AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Provider.FileUpdateStatus>
  IAsyncOperation_1__Provider_FileUpdateStatus_Base = interface(IInspectable)
  ['{8F0F439E-87D0-531F-85B1-54F4528F29C3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus; safecall;
    function GetResults: Provider_FileUpdateStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Provider_FileUpdateStatus read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Provider.FileUpdateStatus>
  IAsyncOperation_1__Provider_FileUpdateStatus = interface(IAsyncOperation_1__Provider_FileUpdateStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
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

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<String>>
  AsyncOperationCompletedHandler_1__IVector_1__HSTRING_Delegate_Base = interface(IUnknown)
  ['{FAE4B396-97C8-5CC3-BF88-EA3098EDF6B2}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVector_1__HSTRING; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<String>>
  AsyncOperationCompletedHandler_1__IVector_1__HSTRING = interface(AsyncOperationCompletedHandler_1__IVector_1__HSTRING_Delegate_Base)
  ['{FAE4B396-97C8-5CC3-BF88-EA3098EDF6B2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<String>>
  IAsyncOperation_1__IVector_1__HSTRING_Base = interface(IInspectable)
  ['{92B02CD3-AA6E-573D-BC03-8D2309CBA3EB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVector_1__HSTRING); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVector_1__HSTRING; safecall;
    function GetResults: IVector_1__HSTRING; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVector_1__HSTRING read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<String>>
  IAsyncOperation_1__IVector_1__HSTRING = interface(IAsyncOperation_1__IVector_1__HSTRING_Base)
  ['{92B02CD3-AA6E-573D-BC03-8D2309CBA3EB}']
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.KnownFoldersAccessStatus>
  AsyncOperationCompletedHandler_1__KnownFoldersAccessStatus = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__KnownFoldersAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.KnownFoldersAccessStatus>
  IAsyncOperation_1__KnownFoldersAccessStatus = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__KnownFoldersAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__KnownFoldersAccessStatus; safecall;
    function GetResults: KnownFoldersAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__KnownFoldersAccessStatus read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFile2
  IStorageFile2 = interface(IInspectable)
  ['{954E4BCF-0A77-42FB-B777-C2ED58A52E44}']
    function OpenAsync(accessMode: FileAccessMode; options: StorageOpenOptions): IAsyncOperation_1__IRandomAccessStream; safecall;
    function OpenTransactedWriteAsync(options: StorageOpenOptions): IAsyncOperation_1__IStorageStreamTransaction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFilePropertiesWithAvailability
  IStorageFilePropertiesWithAvailability = interface(IInspectable)
  ['{AFCBBE9B-582B-4133-9648-E44CA46EE491}']
    function get_IsAvailable: Boolean; safecall;
    property IsAvailable: Boolean read get_IsAvailable;
  end;

  // UsedAPI Interface
  // Windows.Storage.StreamedFileDataRequestedHandler
  StreamedFileDataRequestedHandler = interface(IUnknown)
  ['{FEF6A824-2FE1-4D07-A35B-B77C50B5F4CC}']
    procedure Invoke(stream: IOutputStream); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFileStatics
  [WinRTClassNameAttribute(SWindows_Storage_StorageFile)]
  IStorageFileStatics = interface(IInspectable)
  ['{5984C710-DAF2-43C8-8BB4-A4D3EACFD03F}']
    function GetFileFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFile; safecall;
    function GetFileFromApplicationUriAsync(uri: IUriRuntimeClass): IAsyncOperation_1__IStorageFile; safecall;
    function CreateStreamedFileAsync(displayNameWithExtension: HSTRING; dataRequested: StreamedFileDataRequestedHandler; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; safecall;
    function ReplaceWithStreamedFileAsync(fileToReplace: IStorageFile; dataRequested: StreamedFileDataRequestedHandler; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; safecall;
    function CreateStreamedFileFromUriAsync(displayNameWithExtension: HSTRING; uri: IUriRuntimeClass; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; safecall;
    function ReplaceWithStreamedFileFromUriAsync(fileToReplace: IStorageFile; uri: IUriRuntimeClass; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFileStatics2
  [WinRTClassNameAttribute(SWindows_Storage_StorageFile)]
  IStorageFileStatics2 = interface(IInspectable)
  ['{5C76A781-212E-4AF9-8F04-740CAE108974}']
    function GetFileFromPathForUserAsync(user: IUser; path: HSTRING): IAsyncOperation_1__IStorageFile; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFolder2
  IStorageFolder2 = interface(IInspectable)
  ['{E827E8B9-08D9-4A8E-A0AC-FE5ED3CBBBD3}']
    function TryGetItemAsync(name: HSTRING): IAsyncOperation_1__IStorageItem; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFolder3
  IStorageFolder3 = interface(IInspectable)
  ['{9F617899-BDE1-4124-AEB3-B06AD96F98D4}']
    function TryGetChangeTracker: IStorageLibraryChangeTracker; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFolderStatics
  [WinRTClassNameAttribute(SWindows_Storage_StorageFolder)]
  IStorageFolderStatics = interface(IInspectable)
  ['{08F327FF-85D5-48B9-AEE9-28511E339F9F}']
    function GetFolderFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFolder; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageFolderStatics2
  [WinRTClassNameAttribute(SWindows_Storage_StorageFolder)]
  IStorageFolderStatics2 = interface(IInspectable)
  ['{B4656DC3-71D2-467D-8B29-371F0F62BF6F}']
    function GetFolderFromPathForUserAsync(user: IUser; path: HSTRING): IAsyncOperation_1__IStorageFolder; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageItem2
  IStorageItem2 = interface(IInspectable)
  ['{53F926D2-083C-4283-B45B-81C007237E44}']
    function GetParentAsync: IAsyncOperation_1__IStorageFolder; safecall;
    function IsEqual(item: IStorageItem): Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageItemProperties
  IStorageItemProperties = interface(IInspectable)
  ['{86664478-8029-46FE-A789-1C2F3E2FFB5C}']
    function GetThumbnailAsync(mode: FileProperties_ThumbnailMode): IAsyncOperation_1__IRandomAccessStreamWithContentType; overload; safecall;
    function GetThumbnailAsync(mode: FileProperties_ThumbnailMode; requestedSize: Cardinal): IAsyncOperation_1__IRandomAccessStreamWithContentType; overload; safecall;
    function GetThumbnailAsync(mode: FileProperties_ThumbnailMode; requestedSize: Cardinal; options: FileProperties_ThumbnailOptions): IAsyncOperation_1__IRandomAccessStreamWithContentType; overload; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_DisplayType: HSTRING; safecall;
    function get_FolderRelativeId: HSTRING; safecall;
    function get_Properties: FileProperties_IStorageItemContentProperties; safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property DisplayType: HSTRING read get_DisplayType;
    property FolderRelativeId: HSTRING read get_FolderRelativeId;
    property Properties: FileProperties_IStorageItemContentProperties read get_Properties;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageItemProperties2
  IStorageItemProperties2 = interface(IInspectable)
  ['{8E86A951-04B9-4BD2-929D-FEF3F71621D0}']
    function GetScaledImageAsThumbnailAsync(mode: FileProperties_ThumbnailMode): IAsyncOperation_1__IRandomAccessStreamWithContentType; overload; safecall;
    function GetScaledImageAsThumbnailAsync(mode: FileProperties_ThumbnailMode; requestedSize: Cardinal): IAsyncOperation_1__IRandomAccessStreamWithContentType; overload; safecall;
    function GetScaledImageAsThumbnailAsync(mode: FileProperties_ThumbnailMode; requestedSize: Cardinal; options: FileProperties_ThumbnailOptions): IAsyncOperation_1__IRandomAccessStreamWithContentType; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.IStorageProvider
  IStorageProvider = interface(IInspectable)
  ['{E705EED4-D478-47D6-BA46-1A8EBE114A20}']
    function get_Id: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property Id: HSTRING read get_Id;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IStorageItemPropertiesWithProvider
  IStorageItemPropertiesWithProvider = interface(IInspectable)
  ['{861BF39B-6368-4DEE-B40E-74684A5CE714}']
    function get_Provider: IStorageProvider; safecall;
    property Provider: IStorageProvider read get_Provider;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.ISystemDataPaths
  [WinRTClassNameAttribute(SWindows_Storage_SystemDataPaths)]
  ISystemDataPaths = interface(IInspectable)
  ['{E32ABF70-D8FA-45EC-A942-D2E26FB60BA5}']
    function get_Fonts: HSTRING; safecall;
    function get_ProgramData: HSTRING; safecall;
    function get_Public: HSTRING; safecall;
    function get_PublicDesktop: HSTRING; safecall;
    function get_PublicDocuments: HSTRING; safecall;
    function get_PublicDownloads: HSTRING; safecall;
    function get_PublicMusic: HSTRING; safecall;
    function get_PublicPictures: HSTRING; safecall;
    function get_PublicVideos: HSTRING; safecall;
    function get_System: HSTRING; safecall;
    function get_SystemHost: HSTRING; safecall;
    function get_SystemX86: HSTRING; safecall;
    function get_SystemX64: HSTRING; safecall;
    function get_SystemArm: HSTRING; safecall;
    function get_UserProfiles: HSTRING; safecall;
    function get_Windows: HSTRING; safecall;
    property Fonts: HSTRING read get_Fonts;
    property ProgramData: HSTRING read get_ProgramData;
    property &Public: HSTRING read get_Public;
    property PublicDesktop: HSTRING read get_PublicDesktop;
    property PublicDocuments: HSTRING read get_PublicDocuments;
    property PublicDownloads: HSTRING read get_PublicDownloads;
    property PublicMusic: HSTRING read get_PublicMusic;
    property PublicPictures: HSTRING read get_PublicPictures;
    property PublicVideos: HSTRING read get_PublicVideos;
    property System: HSTRING read get_System;
    property SystemArm: HSTRING read get_SystemArm;
    property SystemHost: HSTRING read get_SystemHost;
    property SystemX64: HSTRING read get_SystemX64;
    property SystemX86: HSTRING read get_SystemX86;
    property UserProfiles: HSTRING read get_UserProfiles;
    property Windows: HSTRING read get_Windows;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.ISystemDataPathsStatics
  [WinRTClassNameAttribute(SWindows_Storage_SystemDataPaths)]
  ISystemDataPathsStatics = interface(IInspectable)
  ['{E0F96FD0-9920-4BCA-B379-F96FDF7CAAD8}']
    function GetDefault: ISystemDataPaths; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IUserDataPaths
  [WinRTClassNameAttribute(SWindows_Storage_UserDataPaths)]
  IUserDataPaths = interface(IInspectable)
  ['{F9C53912-ABC4-46FF-8A2B-DC9D7FA6E52F}']
    function get_CameraRoll: HSTRING; safecall;
    function get_Cookies: HSTRING; safecall;
    function get_Desktop: HSTRING; safecall;
    function get_Documents: HSTRING; safecall;
    function get_Downloads: HSTRING; safecall;
    function get_Favorites: HSTRING; safecall;
    function get_History: HSTRING; safecall;
    function get_InternetCache: HSTRING; safecall;
    function get_LocalAppData: HSTRING; safecall;
    function get_LocalAppDataLow: HSTRING; safecall;
    function get_Music: HSTRING; safecall;
    function get_Pictures: HSTRING; safecall;
    function get_Profile: HSTRING; safecall;
    function get_Recent: HSTRING; safecall;
    function get_RoamingAppData: HSTRING; safecall;
    function get_SavedPictures: HSTRING; safecall;
    function get_Screenshots: HSTRING; safecall;
    function get_Templates: HSTRING; safecall;
    function get_Videos: HSTRING; safecall;
    property CameraRoll: HSTRING read get_CameraRoll;
    property Cookies: HSTRING read get_Cookies;
    property Desktop: HSTRING read get_Desktop;
    property Documents: HSTRING read get_Documents;
    property Downloads: HSTRING read get_Downloads;
    property Favorites: HSTRING read get_Favorites;
    property History: HSTRING read get_History;
    property InternetCache: HSTRING read get_InternetCache;
    property LocalAppData: HSTRING read get_LocalAppData;
    property LocalAppDataLow: HSTRING read get_LocalAppDataLow;
    property Music: HSTRING read get_Music;
    property Pictures: HSTRING read get_Pictures;
    property Profile: HSTRING read get_Profile;
    property Recent: HSTRING read get_Recent;
    property RoamingAppData: HSTRING read get_RoamingAppData;
    property SavedPictures: HSTRING read get_SavedPictures;
    property Screenshots: HSTRING read get_Screenshots;
    property Templates: HSTRING read get_Templates;
    property Videos: HSTRING read get_Videos;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.IUserDataPathsStatics
  [WinRTClassNameAttribute(SWindows_Storage_UserDataPaths)]
  IUserDataPathsStatics = interface(IInspectable)
  ['{01B29DEF-E062-48A1-8B0C-F2C7A9CA56C0}']
    function GetForUser(user: IUser): IUserDataPaths; safecall;
    function GetDefault: IUserDataPaths; safecall;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>
  IKeyValuePair_2__HSTRING__IVector_1__HSTRING = interface(IInspectable)
  ['{174F26C7-79EA-5F7C-BD70-AC4457F2CAC8}']
    function get_Key: HSTRING; safecall;
    function get_Value: IVector_1__HSTRING; safecall;
    property Key: HSTRING read get_Key;
    property Value: IVector_1__HSTRING read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>>
  IIterator_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING_Base = interface(IInspectable)
  ['{A11824C9-E458-502A-AFD8-CE3CE0ABD6FE}']
    function get_Current: IKeyValuePair_2__HSTRING__IVector_1__HSTRING; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IVector_1__HSTRING): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IVector_1__HSTRING read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>>
  IIterator_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING = interface(IIterator_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING_Base)
  ['{A11824C9-E458-502A-AFD8-CE3CE0ABD6FE}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>>
  IIterable_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING_Base = interface(IInspectable)
  ['{4FED2669-D0D3-59F6-91D9-95902D728D6A}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Foundation.Collections.IVector`1<String>>>
  IIterable_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING = interface(IIterable_1__IKeyValuePair_2__HSTRING__IVector_1__HSTRING_Base)
  ['{4FED2669-D0D3-59F6-91D9-95902D728D6A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Foundation.Collections.IVector`1<String>>
  IMapView_2__HSTRING__IVector_1__HSTRING_Base = interface(IInspectable)
  ['{153F9C9C-D22A-5C9E-9C74-8B85C908B000}']
    function Lookup(key: HSTRING): IVector_1__HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IVector_1__HSTRING; out second: IMapView_2__HSTRING__IVector_1__HSTRING); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Foundation.Collections.IVector`1<String>>
  IMapView_2__HSTRING__IVector_1__HSTRING = interface(IMapView_2__HSTRING__IVector_1__HSTRING_Base)
  ['{153F9C9C-D22A-5C9E-9C74-8B85C908B000}']
  end;

  // Windows.Foundation.Collections.IMap`2<String,Windows.Foundation.Collections.IVector`1<String>>
  IMap_2__HSTRING__IVector_1__HSTRING = interface(IInspectable)
  ['{E475CA9D-6AFB-5992-993E-53E6EF7A9ECD}']
    function Lookup(key: HSTRING): IVector_1__HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    function GetView: IMapView_2__HSTRING__IVector_1__HSTRING; safecall;
    function Insert(key: HSTRING; value: IVector_1__HSTRING): Boolean; safecall;
    procedure Remove(key: HSTRING); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Object>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable_Delegate_Base = interface(IUnknown)
  ['{F1E6A632-F97F-54D9-9E1B-A714EDC3FF06}']
    procedure Invoke(sender: Pickers_Provider_IFileSavePickerUI; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Pickers.Provider.IFileSavePickerUI,Object>
  TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable = interface(TypedEventHandler_2__Pickers_Provider_IFileSavePickerUI__IInspectable_Delegate_Base)
  ['{BB54A5EA-3A8B-57FC-81E5-AE9EB9864FAE}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Object>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable_Delegate_Base = interface(IUnknown)
  ['{45FDD443-C0B9-57B0-A84F-7D876EDC3149}']
    procedure Invoke(sender: Provider_ICachedFileUpdaterUI; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Storage.Provider.ICachedFileUpdaterUI,Object>
  TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable = interface(TypedEventHandler_2__Provider_ICachedFileUpdaterUI__IInspectable_Delegate_Base)
  ['{24C1E235-49C9-57D0-AAB2-00275EE76994}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderFileTypeInfo
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderFileTypeInfo)]
  Provider_IStorageProviderFileTypeInfo = interface(IInspectable)
  ['{1955B9C1-0184-5A88-87DF-4544F464365D}']
    function get_FileExtension: HSTRING; safecall;
    function get_IconResource: HSTRING; safecall;
    property FileExtension: HSTRING read get_FileExtension;
    property IconResource: HSTRING read get_IconResource;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderFileTypeInfoFactory
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderFileTypeInfo)]
  Provider_IStorageProviderFileTypeInfoFactory = interface(IInspectable)
  ['{3FA12C6F-CCE6-5D5D-80B1-389E7CF92DBF}']
    function CreateInstance(fileExtension: HSTRING; iconResource: HSTRING): Provider_IStorageProviderFileTypeInfo; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderGetContentInfoForPathResult
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderGetContentInfoForPathResult)]
  Provider_IStorageProviderGetContentInfoForPathResult = interface(IInspectable)
  ['{2564711D-AA89-4D12-82E3-F72A92E33966}']
    function get_Status: Provider_StorageProviderUriSourceStatus; safecall;
    procedure put_Status(value: Provider_StorageProviderUriSourceStatus); safecall;
    function get_ContentUri: HSTRING; safecall;
    procedure put_ContentUri(value: HSTRING); safecall;
    function get_ContentId: HSTRING; safecall;
    procedure put_ContentId(value: HSTRING); safecall;
    property ContentId: HSTRING read get_ContentId write put_ContentId;
    property ContentUri: HSTRING read get_ContentUri write put_ContentUri;
    property Status: Provider_StorageProviderUriSourceStatus read get_Status write put_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderGetPathForContentUriResult
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderGetPathForContentUriResult)]
  Provider_IStorageProviderGetPathForContentUriResult = interface(IInspectable)
  ['{63711A9D-4118-45A6-ACB6-22C49D019F40}']
    function get_Status: Provider_StorageProviderUriSourceStatus; safecall;
    procedure put_Status(value: Provider_StorageProviderUriSourceStatus); safecall;
    function get_Path: HSTRING; safecall;
    procedure put_Path(value: HSTRING); safecall;
    property Path: HSTRING read get_Path write put_Path;
    property Status: Provider_StorageProviderUriSourceStatus read get_Status write put_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderItemProperty
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderItemProperty)]
  Provider_IStorageProviderItemProperty = interface(IInspectable)
  ['{476CB558-730B-4188-B7B5-63B716ED476D}']
    procedure put_Id(value: Integer); safecall;
    function get_Id: Integer; safecall;
    procedure put_Value(value: HSTRING); safecall;
    function get_Value: HSTRING; safecall;
    procedure put_IconResource(value: HSTRING); safecall;
    function get_IconResource: HSTRING; safecall;
    property IconResource: HSTRING read get_IconResource write put_IconResource;
    property Id: Integer read get_Id write put_Id;
    property Value: HSTRING read get_Value write put_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderItemProperty>
  IIterator_1__Provider_IStorageProviderItemProperty_Base = interface(IInspectable)
  ['{0C6DDDDE-1AA3-54F5-B139-E4A237DC1C5F}']
    function get_Current: Provider_IStorageProviderItemProperty; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PProvider_IStorageProviderItemProperty): Cardinal; safecall;
    property Current: Provider_IStorageProviderItemProperty read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderItemProperty>
  IIterator_1__Provider_IStorageProviderItemProperty = interface(IIterator_1__Provider_IStorageProviderItemProperty_Base)
  ['{BB037BEF-95D6-5868-B5AC-4398D6EFC162}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderItemProperty>
  IIterable_1__Provider_IStorageProviderItemProperty_Base = interface(IInspectable)
  ['{4584CB69-EE26-59E0-B05D-C9A7851A7317}']
    function First: IIterator_1__Provider_IStorageProviderItemProperty; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderItemProperty>
  IIterable_1__Provider_IStorageProviderItemProperty = interface(IIterable_1__Provider_IStorageProviderItemProperty_Base)
  ['{CF281CBB-1B32-591A-9346-6444C59E1D72}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderItemPropertiesStatics
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderItemProperties)]
  Provider_IStorageProviderItemPropertiesStatics = interface(IInspectable)
  ['{2D2C1C97-2704-4729-8FA9-7E6B8E158C2F}']
    function SetAsync(item: IStorageItem; itemProperties: IIterable_1__Provider_IStorageProviderItemProperty): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderItemPropertyDefinition
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderItemPropertyDefinition)]
  Provider_IStorageProviderItemPropertyDefinition = interface(IInspectable)
  ['{C5B383BB-FF1F-4298-831E-FF1C08089690}']
    function get_Id: Integer; safecall;
    procedure put_Id(value: Integer); safecall;
    function get_DisplayNameResource: HSTRING; safecall;
    procedure put_DisplayNameResource(value: HSTRING); safecall;
    property DisplayNameResource: HSTRING read get_DisplayNameResource write put_DisplayNameResource;
    property Id: Integer read get_Id write put_Id;
  end;

  // Windows.Storage.Provider.IStorageProviderItemPropertySource
  Provider_IStorageProviderItemPropertySource = interface(IInspectable)
  ['{8F6F9C3E-F632-4A9B-8D99-D2D7A11DF56A}']
    function GetItemProperties(itemPath: HSTRING): IIterable_1__Provider_IStorageProviderItemProperty; safecall;
  end;

  // Windows.Storage.Provider.IStorageProviderPropertyCapabilities
  Provider_IStorageProviderPropertyCapabilities = interface(IInspectable)
  ['{658D2F0E-63B7-4567-ACF9-51ABE301DDA5}']
    function IsPropertySupported(propertyCanonicalName: HSTRING): Boolean; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IIterator_1__Provider_IStorageProviderItemPropertyDefinition_Base = interface(IInspectable)
  ['{55E5719D-2BDA-521F-8C60-6921D90B0BB1}']
    function get_Current: Provider_IStorageProviderItemPropertyDefinition; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PProvider_IStorageProviderItemPropertyDefinition): Cardinal; safecall;
    property Current: Provider_IStorageProviderItemPropertyDefinition read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IIterator_1__Provider_IStorageProviderItemPropertyDefinition = interface(IIterator_1__Provider_IStorageProviderItemPropertyDefinition_Base)
  ['{3427A4E8-5546-57AC-891A-B0FF7CF8EA9E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IIterable_1__Provider_IStorageProviderItemPropertyDefinition_Base = interface(IInspectable)
  ['{41E78B90-1A7F-5DAB-A123-7D5F5011DFEB}']
    function First: IIterator_1__Provider_IStorageProviderItemPropertyDefinition; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IIterable_1__Provider_IStorageProviderItemPropertyDefinition = interface(IIterable_1__Provider_IStorageProviderItemPropertyDefinition_Base)
  ['{A21B7E41-0D3E-57F7-B529-726665A04F54}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IVectorView_1__Provider_IStorageProviderItemPropertyDefinition = interface(IInspectable)
  ['{A550E99C-4754-5A2D-91A4-F09EBF454C95}']
    function GetAt(index: Cardinal): Provider_IStorageProviderItemPropertyDefinition; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Provider_IStorageProviderItemPropertyDefinition; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProvider_IStorageProviderItemPropertyDefinition): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IVector_1__Provider_IStorageProviderItemPropertyDefinition_Base = interface(IInspectable)
  ['{F839FCFF-87DF-53A7-94D4-B507101E7E63}']
    function GetAt(index: Cardinal): Provider_IStorageProviderItemPropertyDefinition; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Provider_IStorageProviderItemPropertyDefinition; safecall;
    function IndexOf(value: Provider_IStorageProviderItemPropertyDefinition; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Provider_IStorageProviderItemPropertyDefinition); safecall;
    procedure InsertAt(index: Cardinal; value: Provider_IStorageProviderItemPropertyDefinition); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Provider_IStorageProviderItemPropertyDefinition); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProvider_IStorageProviderItemPropertyDefinition): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PProvider_IStorageProviderItemPropertyDefinition); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Provider.IStorageProviderItemPropertyDefinition>
  IVector_1__Provider_IStorageProviderItemPropertyDefinition = interface(IVector_1__Provider_IStorageProviderItemPropertyDefinition_Base)
  ['{3963ACFA-643D-5006-A90F-037D698C4387}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderSyncRootInfo
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderSyncRootInfo)]
  Provider_IStorageProviderSyncRootInfo = interface(IInspectable)
  ['{7C1305C4-99F9-41AC-8904-AB055D654926}']
    function get_Id: HSTRING; safecall;
    procedure put_Id(value: HSTRING); safecall;
    function get_Context: IBuffer; safecall;
    procedure put_Context(value: IBuffer); safecall;
    function get_Path: IStorageFolder; safecall;
    procedure put_Path(value: IStorageFolder); safecall;
    function get_DisplayNameResource: HSTRING; safecall;
    procedure put_DisplayNameResource(value: HSTRING); safecall;
    function get_IconResource: HSTRING; safecall;
    procedure put_IconResource(value: HSTRING); safecall;
    function get_HydrationPolicy: Provider_StorageProviderHydrationPolicy; safecall;
    procedure put_HydrationPolicy(value: Provider_StorageProviderHydrationPolicy); safecall;
    function get_HydrationPolicyModifier: Provider_StorageProviderHydrationPolicyModifier; safecall;
    procedure put_HydrationPolicyModifier(value: Provider_StorageProviderHydrationPolicyModifier); safecall;
    function get_PopulationPolicy: Provider_StorageProviderPopulationPolicy; safecall;
    procedure put_PopulationPolicy(value: Provider_StorageProviderPopulationPolicy); safecall;
    function get_InSyncPolicy: Provider_StorageProviderInSyncPolicy; safecall;
    procedure put_InSyncPolicy(value: Provider_StorageProviderInSyncPolicy); safecall;
    function get_HardlinkPolicy: Provider_StorageProviderHardlinkPolicy; safecall;
    procedure put_HardlinkPolicy(value: Provider_StorageProviderHardlinkPolicy); safecall;
    function get_ShowSiblingsAsGroup: Boolean; safecall;
    procedure put_ShowSiblingsAsGroup(value: Boolean); safecall;
    function get_Version: HSTRING; safecall;
    procedure put_Version(value: HSTRING); safecall;
    function get_ProtectionMode: Provider_StorageProviderProtectionMode; safecall;
    procedure put_ProtectionMode(value: Provider_StorageProviderProtectionMode); safecall;
    function get_AllowPinning: Boolean; safecall;
    procedure put_AllowPinning(value: Boolean); safecall;
    function get_StorageProviderItemPropertyDefinitions: IVector_1__Provider_IStorageProviderItemPropertyDefinition; safecall;
    function get_RecycleBinUri: IUriRuntimeClass; safecall;
    procedure put_RecycleBinUri(value: IUriRuntimeClass); safecall;
    property AllowPinning: Boolean read get_AllowPinning write put_AllowPinning;
    property Context: IBuffer read get_Context write put_Context;
    property DisplayNameResource: HSTRING read get_DisplayNameResource write put_DisplayNameResource;
    property HardlinkPolicy: Provider_StorageProviderHardlinkPolicy read get_HardlinkPolicy write put_HardlinkPolicy;
    property HydrationPolicy: Provider_StorageProviderHydrationPolicy read get_HydrationPolicy write put_HydrationPolicy;
    property HydrationPolicyModifier: Provider_StorageProviderHydrationPolicyModifier read get_HydrationPolicyModifier write put_HydrationPolicyModifier;
    property IconResource: HSTRING read get_IconResource write put_IconResource;
    property Id: HSTRING read get_Id write put_Id;
    property InSyncPolicy: Provider_StorageProviderInSyncPolicy read get_InSyncPolicy write put_InSyncPolicy;
    property Path: IStorageFolder read get_Path write put_Path;
    property PopulationPolicy: Provider_StorageProviderPopulationPolicy read get_PopulationPolicy write put_PopulationPolicy;
    property ProtectionMode: Provider_StorageProviderProtectionMode read get_ProtectionMode write put_ProtectionMode;
    property RecycleBinUri: IUriRuntimeClass read get_RecycleBinUri write put_RecycleBinUri;
    property ShowSiblingsAsGroup: Boolean read get_ShowSiblingsAsGroup write put_ShowSiblingsAsGroup;
    property StorageProviderItemPropertyDefinitions: IVector_1__Provider_IStorageProviderItemPropertyDefinition read get_StorageProviderItemPropertyDefinitions;
    property Version: HSTRING read get_Version write put_Version;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderSyncRootInfo2
  Provider_IStorageProviderSyncRootInfo2 = interface(IInspectable)
  ['{CF51B023-7CF1-5166-BDBA-EFD95F529E31}']
    function get_ProviderId: TGuid; safecall;
    procedure put_ProviderId(value: TGuid); safecall;
    property ProviderId: TGuid read get_ProviderId write put_ProviderId;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IIterator_1__Provider_IStorageProviderFileTypeInfo = interface(IInspectable)
  ['{D0BD2F7E-B8BE-5AE2-B490-440D80493E42}']
    function get_Current: Provider_IStorageProviderFileTypeInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PProvider_IStorageProviderFileTypeInfo): Cardinal; safecall;
    property Current: Provider_IStorageProviderFileTypeInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IIterable_1__Provider_IStorageProviderFileTypeInfo = interface(IInspectable)
  ['{B87D13A2-BE96-526A-A56C-74DF1B52BA39}']
    function First: IIterator_1__Provider_IStorageProviderFileTypeInfo; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IVectorView_1__Provider_IStorageProviderFileTypeInfo = interface(IInspectable)
  ['{A66BE115-E6A6-5FB0-A288-F8054B629A28}']
    function GetAt(index: Cardinal): Provider_IStorageProviderFileTypeInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Provider_IStorageProviderFileTypeInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProvider_IStorageProviderFileTypeInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Provider.IStorageProviderFileTypeInfo>
  IVector_1__Provider_IStorageProviderFileTypeInfo = interface(IInspectable)
  ['{FF720A2D-4F40-5AFA-B2E4-3D863301691B}']
    function GetAt(index: Cardinal): Provider_IStorageProviderFileTypeInfo; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Provider_IStorageProviderFileTypeInfo; safecall;
    function IndexOf(value: Provider_IStorageProviderFileTypeInfo; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Provider_IStorageProviderFileTypeInfo); safecall;
    procedure InsertAt(index: Cardinal; value: Provider_IStorageProviderFileTypeInfo); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Provider_IStorageProviderFileTypeInfo); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProvider_IStorageProviderFileTypeInfo): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PProvider_IStorageProviderFileTypeInfo); safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderSyncRootInfo3
  Provider_IStorageProviderSyncRootInfo3 = interface(IInspectable)
  ['{507A6617-BEF6-56FD-855E-75ACE2E45CF5}']
    function get_FallbackFileTypeInfo: IVector_1__Provider_IStorageProviderFileTypeInfo; safecall;
    property FallbackFileTypeInfo: IVector_1__Provider_IStorageProviderFileTypeInfo read get_FallbackFileTypeInfo;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IIterator_1__Provider_IStorageProviderSyncRootInfo_Base = interface(IInspectable)
  ['{F73F72C9-6BF9-5F24-95AF-7264E5516423}']
    function get_Current: Provider_IStorageProviderSyncRootInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PProvider_IStorageProviderSyncRootInfo): Cardinal; safecall;
    property Current: Provider_IStorageProviderSyncRootInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IIterator_1__Provider_IStorageProviderSyncRootInfo = interface(IIterator_1__Provider_IStorageProviderSyncRootInfo_Base)
  ['{EC16ACE5-8024-5247-9513-CD3D7CA3C12C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IIterable_1__Provider_IStorageProviderSyncRootInfo_Base = interface(IInspectable)
  ['{62BA69A0-F65C-502C-9782-B4BC25194D11}']
    function First: IIterator_1__Provider_IStorageProviderSyncRootInfo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IIterable_1__Provider_IStorageProviderSyncRootInfo = interface(IIterable_1__Provider_IStorageProviderSyncRootInfo_Base)
  ['{B13B7ACA-9C63-50A1-8555-64E420EA59F1}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Provider.IStorageProviderSyncRootInfo>
  IVectorView_1__Provider_IStorageProviderSyncRootInfo = interface(IInspectable)
  ['{2B4BBBA2-1C96-5417-988E-9F2A39BA0596}']
    function GetAt(index: Cardinal): Provider_IStorageProviderSyncRootInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Provider_IStorageProviderSyncRootInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProvider_IStorageProviderSyncRootInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderSyncRootManagerStatics
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderSyncRootManager)]
  Provider_IStorageProviderSyncRootManagerStatics = interface(IInspectable)
  ['{3E99FBBF-8FE3-4B40-ABC7-F6FC3D74C98E}']
    procedure Register(syncRootInformation: Provider_IStorageProviderSyncRootInfo); safecall;
    procedure Unregister(id: HSTRING); safecall;
    function GetSyncRootInformationForFolder(folder: IStorageFolder): Provider_IStorageProviderSyncRootInfo; safecall;
    function GetSyncRootInformationForId(id: HSTRING): Provider_IStorageProviderSyncRootInfo; safecall;
    function GetCurrentSyncRoots: IVectorView_1__Provider_IStorageProviderSyncRootInfo; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Provider.IStorageProviderSyncRootManagerStatics2
  [WinRTClassNameAttribute(SWindows_Storage_Provider_StorageProviderSyncRootManager)]
  Provider_IStorageProviderSyncRootManagerStatics2 = interface(IInspectable)
  ['{EFB6CFEE-1374-544E-9DF1-5598D2E9CFDD}']
    function IsSupported: Boolean; safecall;
  end;

  // Windows.Storage.Provider.IStorageProviderUriSource
  Provider_IStorageProviderUriSource = interface(IInspectable)
  ['{B29806D1-8BE0-4962-8BB6-0D4C2E14D47A}']
    procedure GetPathForContentUri(contentUri: HSTRING; a_result: Provider_IStorageProviderGetPathForContentUriResult); safecall;
    procedure GetContentInfoForPath(path: HSTRING; a_result: Provider_IStorageProviderGetContentInfoForPathResult); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable_Delegate_Base = interface(IUnknown)
  ['{89981889-1207-5AE6-9B28-CCC58F3AAC6E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMapView_2__HSTRING__IInspectable; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable = interface(AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable_Delegate_Base)
  ['{89981889-1207-5AE6-9B28-CCC58F3AAC6E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IAsyncOperation_1__IMapView_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{5DCBEE48-9965-51DA-A461-177C885BE7E5}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable; safecall;
    function GetResults: IMapView_2__HSTRING__IInspectable; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IInspectable read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IAsyncOperation_1__IMapView_2__HSTRING__IInspectable = interface(IAsyncOperation_1__IMapView_2__HSTRING__IInspectable_Base)
  ['{5DCBEE48-9965-51DA-A461-177C885BE7E5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IIterator_1__IMapView_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{53A2E825-9BF1-5083-8A7B-9D94F312DADE}']
    function get_Current: IMapView_2__HSTRING__IInspectable; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIMapView_2__HSTRING__IInspectable): Cardinal; safecall;
    property Current: IMapView_2__HSTRING__IInspectable read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IIterator_1__IMapView_2__HSTRING__IInspectable = interface(IIterator_1__IMapView_2__HSTRING__IInspectable_Base)
  ['{53A2E825-9BF1-5083-8A7B-9D94F312DADE}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IIterable_1__IMapView_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{E1670FAE-49CD-5C47-A8C8-F6FA2C650C6C}']
    function First: IIterator_1__IMapView_2__HSTRING__IInspectable; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IIterable_1__IMapView_2__HSTRING__IInspectable = interface(IIterable_1__IMapView_2__HSTRING__IInspectable_Base)
  ['{E1670FAE-49CD-5C47-A8C8-F6FA2C650C6C}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>
  IVectorView_1__IMapView_2__HSTRING__IInspectable = interface(IInspectable)
  ['{172A655B-B3B8-5EAE-BC2E-A6A1F1708B4B}']
    function GetAt(index: Cardinal): IMapView_2__HSTRING__IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IMapView_2__HSTRING__IInspectable; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIMapView_2__HSTRING__IInspectable): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable_Delegate_Base = interface(IUnknown)
  ['{A782A13A-16A0-5326-B985-C4CA49E54E77}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IMapView_2__HSTRING__IInspectable; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable_Delegate_Base)
  ['{A782A13A-16A0-5326-B985-C4CA49E54E77}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>>
  IAsyncOperation_1__IVectorView_1__IMapView_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{FC227365-219D-5D59-8B5B-58EB0A91CA0A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable; safecall;
    function GetResults: IVectorView_1__IMapView_2__HSTRING__IInspectable; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IMapView_2__HSTRING__IInspectable read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Collections.IMapView`2<String,Object>>>
  IAsyncOperation_1__IVectorView_1__IMapView_2__HSTRING__IInspectable = interface(IAsyncOperation_1__IVectorView_1__IMapView_2__HSTRING__IInspectable_Base)
  ['{FC227365-219D-5D59-8B5B-58EB0A91CA0A}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Search.IndexedState>
  AsyncOperationCompletedHandler_1__Search_IndexedState_Delegate_Base = interface(IUnknown)
  ['{B67A3CBA-F5F7-5E51-968A-385126D1F918}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Search_IndexedState; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Search.IndexedState>
  AsyncOperationCompletedHandler_1__Search_IndexedState = interface(AsyncOperationCompletedHandler_1__Search_IndexedState_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Search.IndexedState>
  IAsyncOperation_1__Search_IndexedState_Base = interface(IInspectable)
  ['{88694B1F-F380-574D-8A05-4F67BD52CD11}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Search_IndexedState); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Search_IndexedState; safecall;
    function GetResults: Search_IndexedState; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Search_IndexedState read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Search.IndexedState>
  IAsyncOperation_1__Search_IndexedState = interface(IAsyncOperation_1__Search_IndexedState_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.Storage.Search.IStorageFolderQueryResult
  Search_IStorageFolderQueryResult = interface(IInspectable)
  ['{6654C911-7D66-46FA-AECF-E4A4BAA93AB8}']
    function GetFoldersAsync(startIndex: Cardinal; maxNumberOfItems: Cardinal): IAsyncOperation_1__IVectorView_1__IStorageFolder; overload; safecall;
    function GetFoldersAsync: IAsyncOperation_1__IVectorView_1__IStorageFolder; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Storage.Search.IStorageItemQueryResult
  Search_IStorageItemQueryResult = interface(IInspectable)
  ['{E8948079-9D58-47B8-B2B2-41B07F4795F9}']
    function GetItemsAsync(startIndex: Cardinal; maxNumberOfItems: Cardinal): IAsyncOperation_1__IVectorView_1__IStorageItem; overload; safecall;
    function GetItemsAsync: IAsyncOperation_1__IVectorView_1__IStorageItem; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Search.IStorageFolderQueryOperations
  Search_IStorageFolderQueryOperations = interface(IInspectable)
  ['{CB43CCC9-446B-4A4F-BE97-757771BE5203}']
    function GetIndexedStateAsync: IAsyncOperation_1__Search_IndexedState; safecall;
    function CreateFileQuery: Search_IStorageFileQueryResult; overload; safecall;
    function CreateFileQuery(query: Search_CommonFileQuery): Search_IStorageFileQueryResult; overload; safecall;
    function CreateFileQueryWithOptions(queryOptions: Search_IQueryOptions): Search_IStorageFileQueryResult; safecall;
    function CreateFolderQuery: Search_IStorageFolderQueryResult; overload; safecall;
    function CreateFolderQuery(query: Search_CommonFolderQuery): Search_IStorageFolderQueryResult; overload; safecall;
    function CreateFolderQueryWithOptions(queryOptions: Search_IQueryOptions): Search_IStorageFolderQueryResult; safecall;
    function CreateItemQuery: Search_IStorageItemQueryResult; safecall;
    function CreateItemQueryWithOptions(queryOptions: Search_IQueryOptions): Search_IStorageItemQueryResult; safecall;
    function GetFilesAsync(query: Search_CommonFileQuery; startIndex: Cardinal; maxItemsToRetrieve: Cardinal): IAsyncOperation_1__IVectorView_1__IStorageFile; overload; safecall;
    function GetFilesAsync(query: Search_CommonFileQuery): IAsyncOperation_1__IVectorView_1__IStorageFile; overload; safecall;
    function GetFoldersAsync(query: Search_CommonFolderQuery; startIndex: Cardinal; maxItemsToRetrieve: Cardinal): IAsyncOperation_1__IVectorView_1__IStorageFolder; overload; safecall;
    function GetFoldersAsync(query: Search_CommonFolderQuery): IAsyncOperation_1__IVectorView_1__IStorageFolder; overload; safecall;
    function GetItemsAsync(startIndex: Cardinal; maxItemsToRetrieve: Cardinal): IAsyncOperation_1__IVectorView_1__IStorageItem; safecall;
    function AreQueryOptionsSupported(queryOptions: Search_IQueryOptions): Boolean; safecall;
    function IsCommonFolderQuerySupported(query: Search_CommonFolderQuery): Boolean; safecall;
    function IsCommonFileQuerySupported(query: Search_CommonFileQuery): Boolean; safecall;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface(IUnknown)
  ['{F4637D4A-0760-5431-BFC0-24EB1D4F6C4F}']
    procedure Invoke(sender: IMemoryBufferReference; args: IInspectable); safecall;
  end;

  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt64,UInt64>
  AsyncOperationProgressHandler_2__UInt64__UInt64 = interface(IUnknown)
  ['{FFB2B65D-4120-5D13-826D-107851E6BB1C}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__UInt64__UInt64; progressInfo: UInt64); safecall;
  end;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt64,UInt64>
  AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = interface(IUnknown)
  ['{D2024E41-5500-5B5A-BA46-CB7009596A2F}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__UInt64__UInt64; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt64,UInt64>
  IAsyncOperationWithProgress_2__UInt64__UInt64 = interface(IInspectable)
  ['{8F1DB6E3-6556-5516-825C-1021EE27CD0C}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__UInt64__UInt64); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__UInt64__UInt64; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64; safecall;
    function GetResults: UInt64; safecall;
    property Progress: AsyncOperationProgressHandler_2__UInt64__UInt64 read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 read get_Completed write put_Completed;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageItem>
  IVector_1__IStorageItem_Base = interface(IInspectable)
  ['{802508E2-9C2C-5B91-89A8-39BCF7223344}']
    function GetAt(index: Cardinal): IStorageItem; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IStorageItem; safecall;
    function IndexOf(value: IStorageItem; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IStorageItem); safecall;
    procedure InsertAt(index: Cardinal; value: IStorageItem); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IStorageItem); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIStorageItem): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIStorageItem); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.IStorageItem>
  IVector_1__IStorageItem = interface(IVector_1__IStorageItem_Base)
  ['{802508E2-9C2C-5B91-89A8-39BCF7223344}']
  end;

  // Windows.Storage.ApplicationData
  // DualAPI
  // Implements: Windows.Storage.IApplicationData
  // Implements: Windows.Storage.IApplicationData2
  // Implements: Windows.Storage.IApplicationData3
  // Statics: "Windows.Storage.IApplicationDataStatics"
  // Statics: "Windows.Storage.IApplicationDataStatics2"
  TApplicationData = class(TWinRTGenericImportS2<IApplicationDataStatics, IApplicationDataStatics2>)
  public
    // -> IApplicationDataStatics
    class function get_Current: IApplicationData; static; inline;
    class property Current: IApplicationData read get_Current;

    // -> IApplicationDataStatics2
    class function GetForUserAsync(user: IUser): IAsyncOperation_1__IApplicationData; static; inline;
  end;

  // Windows.Storage.ApplicationDataCompositeValue
  // DualAPI
  // Implements: Windows.Foundation.Collections.IPropertySet
  // Implements: Windows.Foundation.Collections.IObservableMap`2<String,Object>
  // Implements: Windows.Foundation.Collections.IMap`2<String,Object>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  // Instantiable: "IPropertySet"
  TApplicationDataCompositeValue = class(TWinRTGenericImportI<IPropertySet>) end;

  // Windows.Storage.Compression.Compressor
  // DualAPI
  // Implements: Windows.Storage.Compression.ICompressor
  // Implements: Windows.Storage.Streams.IOutputStream
  // Implements: Windows.Foundation.IClosable
  // Factory: "Windows.Storage.Compression.ICompressorFactory"
  TCompression_Compressor = class(TWinRTGenericImportF<Compression_ICompressorFactory>)
  public
    // -> Compression_ICompressorFactory
    class function CreateCompressor(underlyingStream: IOutputStream): Compression_ICompressor; static; inline;
    class function CreateCompressorEx(underlyingStream: IOutputStream; algorithm: Compression_CompressAlgorithm; blockSize: Cardinal): Compression_ICompressor; static; inline;
  end;

  // Windows.Storage.Compression.Decompressor
  // DualAPI
  // Implements: Windows.Storage.Compression.IDecompressor
  // Implements: Windows.Storage.Streams.IInputStream
  // Implements: Windows.Foundation.IClosable
  // Factory: "Windows.Storage.Compression.IDecompressorFactory"
  TCompression_Decompressor = class(TWinRTGenericImportF<Compression_IDecompressorFactory>)
  public
    // -> Compression_IDecompressorFactory
    class function CreateDecompressor(underlyingStream: IInputStream): Compression_IDecompressor; static; inline;
  end;

  // Windows.Storage.FileProperties.GeotagHelper
  // DualAPI
  // Statics: "Windows.Storage.FileProperties.IGeotagHelperStatics"
  TFileProperties_GeotagHelper = class(TWinRTGenericImportS<FileProperties_IGeotagHelperStatics>)
  public
    // -> FileProperties_IGeotagHelperStatics
    class function GetGeotagAsync(&file: IStorageFile): IAsyncOperation_1__IGeopoint; static; inline;
    class function SetGeotagFromGeolocatorAsync(&file: IStorageFile; geolocator: IGeolocator): IAsyncAction; static; inline;
    class function SetGeotagAsync(&file: IStorageFile; geopoint: IGeopoint): IAsyncAction; static; inline;
  end;

  // Windows.Storage.Provider.StorageProviderFileTypeInfo
  // DualAPI
  // Implements: Windows.Storage.Provider.IStorageProviderFileTypeInfo
  // Factory: "Windows.Storage.Provider.IStorageProviderFileTypeInfoFactory"
  TProvider_StorageProviderFileTypeInfo = class(TWinRTGenericImportF<Provider_IStorageProviderFileTypeInfoFactory>)
  public
    // -> Provider_IStorageProviderFileTypeInfoFactory
    class function CreateInstance(fileExtension: HSTRING; iconResource: HSTRING): Provider_IStorageProviderFileTypeInfo; static; inline;
  end;

  // Windows.Storage.Provider.StorageProviderGetContentInfoForPathResult
  // DualAPI
  // Implements: Windows.Storage.Provider.IStorageProviderGetContentInfoForPathResult
  // Instantiable: "Provider_IStorageProviderGetContentInfoForPathResult"
  TProvider_StorageProviderGetContentInfoForPathResult = class(TWinRTGenericImportI<Provider_IStorageProviderGetContentInfoForPathResult>) end;

  // Windows.Storage.Provider.StorageProviderGetPathForContentUriResult
  // DualAPI
  // Implements: Windows.Storage.Provider.IStorageProviderGetPathForContentUriResult
  // Instantiable: "Provider_IStorageProviderGetPathForContentUriResult"
  TProvider_StorageProviderGetPathForContentUriResult = class(TWinRTGenericImportI<Provider_IStorageProviderGetPathForContentUriResult>) end;

  // Windows.Storage.Provider.StorageProviderItemProperties
  // DualAPI
  // Statics: "Windows.Storage.Provider.IStorageProviderItemPropertiesStatics"
  TProvider_StorageProviderItemProperties = class(TWinRTGenericImportS<Provider_IStorageProviderItemPropertiesStatics>)
  public
    // -> Provider_IStorageProviderItemPropertiesStatics
    class function SetAsync(item: IStorageItem; itemProperties: IIterable_1__Provider_IStorageProviderItemProperty): IAsyncAction; static; inline;
  end;

  // Windows.Storage.Provider.StorageProviderItemProperty
  // DualAPI
  // Implements: Windows.Storage.Provider.IStorageProviderItemProperty
  // Instantiable: "Provider_IStorageProviderItemProperty"
  TProvider_StorageProviderItemProperty = class(TWinRTGenericImportI<Provider_IStorageProviderItemProperty>) end;

  // Windows.Storage.Provider.StorageProviderItemPropertyDefinition
  // DualAPI
  // Implements: Windows.Storage.Provider.IStorageProviderItemPropertyDefinition
  // Instantiable: "Provider_IStorageProviderItemPropertyDefinition"
  TProvider_StorageProviderItemPropertyDefinition = class(TWinRTGenericImportI<Provider_IStorageProviderItemPropertyDefinition>) end;

  // Windows.Storage.Provider.StorageProviderSyncRootInfo
  // DualAPI
  // Implements: Windows.Storage.Provider.IStorageProviderSyncRootInfo
  // Implements: Windows.Storage.Provider.IStorageProviderSyncRootInfo2
  // Implements: Windows.Storage.Provider.IStorageProviderSyncRootInfo3
  // Instantiable: "Provider_IStorageProviderSyncRootInfo"
  TProvider_StorageProviderSyncRootInfo = class(TWinRTGenericImportI<Provider_IStorageProviderSyncRootInfo>) end;

  // Windows.Storage.Provider.StorageProviderSyncRootManager
  // DualAPI
  // Statics: "Windows.Storage.Provider.IStorageProviderSyncRootManagerStatics"
  // Statics: "Windows.Storage.Provider.IStorageProviderSyncRootManagerStatics2"
  TProvider_StorageProviderSyncRootManager = class(TWinRTGenericImportS2<Provider_IStorageProviderSyncRootManagerStatics, Provider_IStorageProviderSyncRootManagerStatics2>)
  public
    // -> Provider_IStorageProviderSyncRootManagerStatics
    class procedure Register(syncRootInformation: Provider_IStorageProviderSyncRootInfo); static; inline;
    class procedure Unregister(id: HSTRING); static; inline;
    class function GetSyncRootInformationForFolder(folder: IStorageFolder): Provider_IStorageProviderSyncRootInfo; static; inline;
    class function GetSyncRootInformationForId(id: HSTRING): Provider_IStorageProviderSyncRootInfo; static; inline;
    class function GetCurrentSyncRoots: IVectorView_1__Provider_IStorageProviderSyncRootInfo; static; inline;

    // -> Provider_IStorageProviderSyncRootManagerStatics2
    class function IsSupported: Boolean; static; inline;
  end;

  // Windows.Storage.StorageFile
  // Explicitly imported
  // Implements: Windows.Storage.IStorageFile
  // Implements: Windows.Storage.Streams.IInputStreamReference
  // Implements: Windows.Storage.Streams.IRandomAccessStreamReference
  // Implements: Windows.Storage.IStorageItem
  // Implements: Windows.Storage.IStorageItemProperties
  // Implements: Windows.Storage.IStorageItemProperties2
  // Implements: Windows.Storage.IStorageItem2
  // Implements: Windows.Storage.IStorageItemPropertiesWithProvider
  // Implements: Windows.Storage.IStorageFilePropertiesWithAvailability
  // Implements: Windows.Storage.IStorageFile2
  // Statics: "Windows.Storage.IStorageFileStatics"
  // Statics: "Windows.Storage.IStorageFileStatics2"
  TStorageFile = class(TWinRTGenericImportS2<IStorageFileStatics, IStorageFileStatics2>)
  public
    // -> IStorageFileStatics
    class function GetFileFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFile; static; inline;
    class function GetFileFromApplicationUriAsync(uri: IUriRuntimeClass): IAsyncOperation_1__IStorageFile; static; inline;
    class function CreateStreamedFileAsync(displayNameWithExtension: HSTRING; dataRequested: StreamedFileDataRequestedHandler; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; static; inline;
    class function ReplaceWithStreamedFileAsync(fileToReplace: IStorageFile; dataRequested: StreamedFileDataRequestedHandler; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; static; inline;
    class function CreateStreamedFileFromUriAsync(displayNameWithExtension: HSTRING; uri: IUriRuntimeClass; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; static; inline;
    class function ReplaceWithStreamedFileFromUriAsync(fileToReplace: IStorageFile; uri: IUriRuntimeClass; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile; static; inline;

    // -> IStorageFileStatics2
    class function GetFileFromPathForUserAsync(user: IUser; path: HSTRING): IAsyncOperation_1__IStorageFile; static; inline;
  end;

  // Windows.Storage.StorageFolder
  // Explicitly imported
  // Implements: Windows.Storage.IStorageFolder
  // Implements: Windows.Storage.IStorageItem
  // Implements: Windows.Storage.Search.IStorageFolderQueryOperations
  // Implements: Windows.Storage.IStorageItemProperties
  // Implements: Windows.Storage.IStorageItemProperties2
  // Implements: Windows.Storage.IStorageItem2
  // Implements: Windows.Storage.IStorageFolder2
  // Implements: Windows.Storage.IStorageItemPropertiesWithProvider
  // Implements: Windows.Storage.IStorageFolder3
  // Statics: "Windows.Storage.IStorageFolderStatics"
  // Statics: "Windows.Storage.IStorageFolderStatics2"
  TStorageFolder = class(TWinRTGenericImportS2<IStorageFolderStatics, IStorageFolderStatics2>)
  public
    // -> IStorageFolderStatics
    class function GetFolderFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFolder; static; inline;

    // -> IStorageFolderStatics2
    class function GetFolderFromPathForUserAsync(user: IUser; path: HSTRING): IAsyncOperation_1__IStorageFolder; static; inline;
  end;

  // Windows.Storage.SystemDataPaths
  // Explicitly imported
  // Implements: Windows.Storage.ISystemDataPaths
  // Statics: "Windows.Storage.ISystemDataPathsStatics"
  TSystemDataPaths = class(TWinRTGenericImportS<ISystemDataPathsStatics>)
  public
    // -> ISystemDataPathsStatics
    class function GetDefault: ISystemDataPaths; static; inline;
  end;

  // Windows.Storage.UserDataPaths
  // Explicitly imported
  // Implements: Windows.Storage.IUserDataPaths
  // Statics: "Windows.Storage.IUserDataPathsStatics"
  TUserDataPaths = class(TWinRTGenericImportS<IUserDataPathsStatics>)
  public
    // -> IUserDataPathsStatics
    class function GetForUser(user: IUser): IUserDataPaths; static; inline;
    class function GetDefault: IUserDataPaths; static; inline;
  end;

implementation

{ TApplicationData }

class function TApplicationData.get_Current: IApplicationData;
begin
  Result := Statics.get_Current;
end;


class function TApplicationData.GetForUserAsync(user: IUser): IAsyncOperation_1__IApplicationData;
begin
  Result := Statics2.GetForUserAsync(user);
end;


{ TApplicationDataCompositeValue }

{ TCompression_Compressor }
// Factories for : "Compression_Compressor"
// Factory: "Windows.Storage.Compression.ICompressorFactory"
// -> Compression_ICompressorFactory

class function TCompression_Compressor.CreateCompressor(underlyingStream: IOutputStream): Compression_ICompressor;
begin
  Result := Factory.CreateCompressor(underlyingStream);
end;

class function TCompression_Compressor.CreateCompressorEx(underlyingStream: IOutputStream; algorithm: Compression_CompressAlgorithm; blockSize: Cardinal): Compression_ICompressor;
begin
  Result := Factory.CreateCompressorEx(underlyingStream, algorithm, blockSize);
end;


{ TCompression_Decompressor }
// Factories for : "Compression_Decompressor"
// Factory: "Windows.Storage.Compression.IDecompressorFactory"
// -> Compression_IDecompressorFactory

class function TCompression_Decompressor.CreateDecompressor(underlyingStream: IInputStream): Compression_IDecompressor;
begin
  Result := Factory.CreateDecompressor(underlyingStream);
end;


{ TFileProperties_GeotagHelper }

class function TFileProperties_GeotagHelper.GetGeotagAsync(&file: IStorageFile): IAsyncOperation_1__IGeopoint;
begin
  Result := Statics.GetGeotagAsync(&file);
end;

class function TFileProperties_GeotagHelper.SetGeotagFromGeolocatorAsync(&file: IStorageFile; geolocator: IGeolocator): IAsyncAction;
begin
  Result := Statics.SetGeotagFromGeolocatorAsync(&file, geolocator);
end;

class function TFileProperties_GeotagHelper.SetGeotagAsync(&file: IStorageFile; geopoint: IGeopoint): IAsyncAction;
begin
  Result := Statics.SetGeotagAsync(&file, geopoint);
end;


{ TProvider_StorageProviderFileTypeInfo }
// Factories for : "Provider_StorageProviderFileTypeInfo"
// Factory: "Windows.Storage.Provider.IStorageProviderFileTypeInfoFactory"
// -> Provider_IStorageProviderFileTypeInfoFactory

class function TProvider_StorageProviderFileTypeInfo.CreateInstance(fileExtension: HSTRING; iconResource: HSTRING): Provider_IStorageProviderFileTypeInfo;
begin
  Result := Factory.CreateInstance(fileExtension, iconResource);
end;


{ TProvider_StorageProviderGetContentInfoForPathResult }

{ TProvider_StorageProviderGetPathForContentUriResult }

{ TProvider_StorageProviderItemProperties }

class function TProvider_StorageProviderItemProperties.SetAsync(item: IStorageItem; itemProperties: IIterable_1__Provider_IStorageProviderItemProperty): IAsyncAction;
begin
  Result := Statics.SetAsync(item, itemProperties);
end;


{ TProvider_StorageProviderItemProperty }

{ TProvider_StorageProviderItemPropertyDefinition }

{ TProvider_StorageProviderSyncRootInfo }

{ TProvider_StorageProviderSyncRootManager }

class procedure TProvider_StorageProviderSyncRootManager.Register(syncRootInformation: Provider_IStorageProviderSyncRootInfo);
begin
  Statics.Register(syncRootInformation);
end;

class procedure TProvider_StorageProviderSyncRootManager.Unregister(id: HSTRING);
begin
  Statics.Unregister(id);
end;

class function TProvider_StorageProviderSyncRootManager.GetSyncRootInformationForFolder(folder: IStorageFolder): Provider_IStorageProviderSyncRootInfo;
begin
  Result := Statics.GetSyncRootInformationForFolder(folder);
end;

class function TProvider_StorageProviderSyncRootManager.GetSyncRootInformationForId(id: HSTRING): Provider_IStorageProviderSyncRootInfo;
begin
  Result := Statics.GetSyncRootInformationForId(id);
end;

class function TProvider_StorageProviderSyncRootManager.GetCurrentSyncRoots: IVectorView_1__Provider_IStorageProviderSyncRootInfo;
begin
  Result := Statics.GetCurrentSyncRoots;
end;


class function TProvider_StorageProviderSyncRootManager.IsSupported: Boolean;
begin
  Result := Statics2.IsSupported;
end;


{ TStorageFile }

class function TStorageFile.GetFileFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics.GetFileFromPathAsync(path);
end;

class function TStorageFile.GetFileFromApplicationUriAsync(uri: IUriRuntimeClass): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics.GetFileFromApplicationUriAsync(uri);
end;

class function TStorageFile.CreateStreamedFileAsync(displayNameWithExtension: HSTRING; dataRequested: StreamedFileDataRequestedHandler; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics.CreateStreamedFileAsync(displayNameWithExtension, dataRequested, thumbnail);
end;

class function TStorageFile.ReplaceWithStreamedFileAsync(fileToReplace: IStorageFile; dataRequested: StreamedFileDataRequestedHandler; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics.ReplaceWithStreamedFileAsync(fileToReplace, dataRequested, thumbnail);
end;

class function TStorageFile.CreateStreamedFileFromUriAsync(displayNameWithExtension: HSTRING; uri: IUriRuntimeClass; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics.CreateStreamedFileFromUriAsync(displayNameWithExtension, uri, thumbnail);
end;

class function TStorageFile.ReplaceWithStreamedFileFromUriAsync(fileToReplace: IStorageFile; uri: IUriRuntimeClass; thumbnail: IRandomAccessStreamReference): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics.ReplaceWithStreamedFileFromUriAsync(fileToReplace, uri, thumbnail);
end;


class function TStorageFile.GetFileFromPathForUserAsync(user: IUser; path: HSTRING): IAsyncOperation_1__IStorageFile;
begin
  Result := Statics2.GetFileFromPathForUserAsync(user, path);
end;


{ TStorageFolder }

class function TStorageFolder.GetFolderFromPathAsync(path: HSTRING): IAsyncOperation_1__IStorageFolder;
begin
  Result := Statics.GetFolderFromPathAsync(path);
end;


class function TStorageFolder.GetFolderFromPathForUserAsync(user: IUser; path: HSTRING): IAsyncOperation_1__IStorageFolder;
begin
  Result := Statics2.GetFolderFromPathForUserAsync(user, path);
end;


{ TSystemDataPaths }

class function TSystemDataPaths.GetDefault: ISystemDataPaths;
begin
  Result := Statics.GetDefault;
end;


{ TUserDataPaths }

class function TUserDataPaths.GetForUser(user: IUser): IUserDataPaths;
begin
  Result := Statics.GetForUser(user);
end;

class function TUserDataPaths.GetDefault: IUserDataPaths;
begin
  Result := Statics.GetDefault;
end;


end.
