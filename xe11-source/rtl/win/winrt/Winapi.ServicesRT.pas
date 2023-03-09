{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.ServicesRT;

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
  Winapi.Storage.Streams, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  IReference_1__Double = Winapi.CommonTypes.IReference_1__Double;
  PIReference_1__Double = Winapi.CommonTypes.PIReference_1__Double;
  IReference_1__Integer = Winapi.CommonTypes.IReference_1__Integer;
  PIReference_1__Integer = Winapi.CommonTypes.PIReference_1__Integer;
  IVectorView_1__Maps_IMapRouteLeg = Winapi.CommonTypes.IVectorView_1__Maps_IMapRouteLeg;
  PIVectorView_1__Maps_IMapRouteLeg = Winapi.CommonTypes.PIVectorView_1__Maps_IMapRouteLeg;
  IVectorView_1__Maps_IMapRouteManeuver = Winapi.CommonTypes.IVectorView_1__Maps_IMapRouteManeuver;
  PIVectorView_1__Maps_IMapRouteManeuver = Winapi.CommonTypes.PIVectorView_1__Maps_IMapRouteManeuver;
  Maps_IMapRoute = Winapi.CommonTypes.Maps_IMapRoute;
  PMaps_IMapRoute = Winapi.CommonTypes.PMaps_IMapRoute;
  Maps_IMapRouteLeg = Winapi.CommonTypes.Maps_IMapRouteLeg;
  PMaps_IMapRouteLeg = Winapi.CommonTypes.PMaps_IMapRouteLeg;
  Maps_IMapRouteManeuver = Winapi.CommonTypes.Maps_IMapRouteManeuver;
  PMaps_IMapRouteManeuver = Winapi.CommonTypes.PMaps_IMapRouteManeuver;
  Maps_MapManeuverNotices = Winapi.CommonTypes.Maps_MapManeuverNotices;
  PMaps_MapManeuverNotices = Winapi.CommonTypes.PMaps_MapManeuverNotices;
  Maps_MapRouteManeuverKind = Winapi.CommonTypes.Maps_MapRouteManeuverKind;
  PMaps_MapRouteManeuverKind = Winapi.CommonTypes.PMaps_MapRouteManeuverKind;
  TypedEventHandler_2__IDataPackage__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDataPackage__IInspectable_Delegate_Base;
  TypedEventHandler_2__IDataPackage__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IDataPackage__IInspectable;
  PTypedEventHandler_2__IDataPackage__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IDataPackage__IInspectable;
  TypedEventHandler_2__IStoreContext__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IStoreContext__IInspectable_Delegate_Base;
  TypedEventHandler_2__IStoreContext__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IStoreContext__IInspectable;
  PTypedEventHandler_2__IStoreContext__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IStoreContext__IInspectable;
  TypedEventHandler_2__IStorePackageLicense__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IStorePackageLicense__IInspectable_Delegate_Base;
  TypedEventHandler_2__IStorePackageLicense__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IStorePackageLicense__IInspectable;
  PTypedEventHandler_2__IStorePackageLicense__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IStorePackageLicense__IInspectable;
  TypedEventHandler_2__IStoreQueueItem__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IStoreQueueItem__IInspectable;
  PTypedEventHandler_2__IStoreQueueItem__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IStoreQueueItem__IInspectable;

  // Forward declarations for interfaces

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

  // Windows.Foundation.AsyncOperationCompletedHandler`1<String>
  AsyncOperationCompletedHandler_1__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__HSTRING = ^AsyncOperationCompletedHandler_1__HSTRING;

  // Windows.Foundation.IAsyncOperation`1<String>
  IAsyncOperation_1__HSTRING = interface;
  PIAsyncOperation_1__HSTRING = ^IAsyncOperation_1__HSTRING;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Cortana.CortanaPermission>
  IIterator_1__Cortana_CortanaPermission = interface;
  PIIterator_1__Cortana_CortanaPermission = ^IIterator_1__Cortana_CortanaPermission;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Cortana.CortanaPermission>
  IIterable_1__Cortana_CortanaPermission = interface;
  PIIterable_1__Cortana_CortanaPermission = ^IIterable_1__Cortana_CortanaPermission;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Cortana.CortanaPermissionsChangeResult>
  AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult = interface;
  PAsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult = ^AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Cortana.CortanaPermissionsChangeResult>
  IAsyncOperation_1__Cortana_CortanaPermissionsChangeResult = interface;
  PIAsyncOperation_1__Cortana_CortanaPermissionsChangeResult = ^IAsyncOperation_1__Cortana_CortanaPermissionsChangeResult;

  // Windows.Services.Cortana.ICortanaSettings
  Cortana_ICortanaSettings = interface;
  PCortana_ICortanaSettings = ^Cortana_ICortanaSettings;

  // Windows.Services.Cortana.ICortanaSettingsStatics
  Cortana_ICortanaSettingsStatics = interface;
  PCortana_ICortanaSettingsStatics = ^Cortana_ICortanaSettingsStatics;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRouteManeuver>
  IIterator_1__Maps_IMapRouteManeuver = interface;
  PIIterator_1__Maps_IMapRouteManeuver = ^IIterator_1__Maps_IMapRouteManeuver;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRouteManeuver>
  IIterable_1__Maps_IMapRouteManeuver = interface;
  PIIterable_1__Maps_IMapRouteManeuver = ^IIterable_1__Maps_IMapRouteManeuver;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRouteLeg>
  IIterator_1__Maps_IMapRouteLeg = interface;
  PIIterator_1__Maps_IMapRouteLeg = ^IIterator_1__Maps_IMapRouteLeg;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRouteLeg>
  IIterable_1__Maps_IMapRouteLeg = interface;
  PIIterable_1__Maps_IMapRouteLeg = ^IIterable_1__Maps_IMapRouteLeg;

  // Windows.Services.Maps.IMapAddress
  Maps_IMapAddress = interface;
  PMaps_IMapAddress = ^Maps_IMapAddress;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRoute>
  IIterator_1__Maps_IMapRoute = interface;
  PIIterator_1__Maps_IMapRoute = ^IIterator_1__Maps_IMapRoute;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRoute>
  IIterable_1__Maps_IMapRoute = interface;
  PIIterable_1__Maps_IMapRoute = ^IIterable_1__Maps_IMapRoute;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.IMapRoute>
  IVectorView_1__Maps_IMapRoute = interface;
  PIVectorView_1__Maps_IMapRoute = ^IVectorView_1__Maps_IMapRoute;

  // Windows.Services.Maps.LocalSearch.ILocalLocation
  Maps_LocalSearch_ILocalLocation = interface;
  PMaps_LocalSearch_ILocalLocation = ^Maps_LocalSearch_ILocalLocation;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IIterator_1__Maps_LocalSearch_ILocalLocation = interface;
  PIIterator_1__Maps_LocalSearch_ILocalLocation = ^IIterator_1__Maps_LocalSearch_ILocalLocation;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IIterable_1__Maps_LocalSearch_ILocalLocation = interface;
  PIIterable_1__Maps_LocalSearch_ILocalLocation = ^IIterable_1__Maps_LocalSearch_ILocalLocation;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IVectorView_1__Maps_LocalSearch_ILocalLocation = interface;
  PIVectorView_1__Maps_LocalSearch_ILocalLocation = ^IVectorView_1__Maps_LocalSearch_ILocalLocation;

  // Windows.Services.TargetedContent.ITargetedContentAction
  TargetedContent_ITargetedContentAction = interface;
  PTargetedContent_ITargetedContentAction = ^TargetedContent_ITargetedContentAction;

  // Windows.Services.TargetedContent.ITargetedContentAvailabilityChangedEventArgs
  TargetedContent_ITargetedContentAvailabilityChangedEventArgs = interface;
  PTargetedContent_ITargetedContentAvailabilityChangedEventArgs = ^TargetedContent_ITargetedContentAvailabilityChangedEventArgs;

  // Windows.Services.TargetedContent.ITargetedContentChangedEventArgs
  TargetedContent_ITargetedContentChangedEventArgs = interface;
  PTargetedContent_ITargetedContentChangedEventArgs = ^TargetedContent_ITargetedContentChangedEventArgs;

  // Windows.Services.TargetedContent.ITargetedContentImage
  TargetedContent_ITargetedContentImage = interface;
  PTargetedContent_ITargetedContentImage = ^TargetedContent_ITargetedContentImage;

  // Windows.Foundation.Collections.IIterator`1<Double>
  IIterator_1__Double = interface;
  PIIterator_1__Double = ^IIterator_1__Double;

  // Windows.Foundation.Collections.IIterable`1<Double>
  IIterable_1__Double = interface;
  PIIterable_1__Double = ^IIterable_1__Double;

  // Windows.Foundation.Collections.IVectorView`1<Double>
  IVectorView_1__Double = interface;
  PIVectorView_1__Double = ^IVectorView_1__Double;

  // Windows.Foundation.Collections.IIterator`1<Boolean>
  IIterator_1__Boolean = interface;
  PIIterator_1__Boolean = ^IIterator_1__Boolean;

  // Windows.Foundation.Collections.IIterable`1<Boolean>
  IIterable_1__Boolean = interface;
  PIIterable_1__Boolean = ^IIterable_1__Boolean;

  // Windows.Foundation.Collections.IVectorView`1<Boolean>
  IVectorView_1__Boolean = interface;
  PIVectorView_1__Boolean = ^IVectorView_1__Boolean;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentImage>
  IIterator_1__TargetedContent_ITargetedContentImage = interface;
  PIIterator_1__TargetedContent_ITargetedContentImage = ^IIterator_1__TargetedContent_ITargetedContentImage;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentImage>
  IIterable_1__TargetedContent_ITargetedContentImage = interface;
  PIIterable_1__TargetedContent_ITargetedContentImage = ^IIterable_1__TargetedContent_ITargetedContentImage;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentImage>
  IVectorView_1__TargetedContent_ITargetedContentImage = interface;
  PIVectorView_1__TargetedContent_ITargetedContentImage = ^IVectorView_1__TargetedContent_ITargetedContentImage;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentAction>
  IIterator_1__TargetedContent_ITargetedContentAction = interface;
  PIIterator_1__TargetedContent_ITargetedContentAction = ^IIterator_1__TargetedContent_ITargetedContentAction;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentAction>
  IIterable_1__TargetedContent_ITargetedContentAction = interface;
  PIIterable_1__TargetedContent_ITargetedContentAction = ^IIterable_1__TargetedContent_ITargetedContentAction;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentAction>
  IVectorView_1__TargetedContent_ITargetedContentAction = interface;
  PIVectorView_1__TargetedContent_ITargetedContentAction = ^IVectorView_1__TargetedContent_ITargetedContentAction;

  // Windows.Services.TargetedContent.ITargetedContentValue
  TargetedContent_ITargetedContentValue = interface;
  PTargetedContent_ITargetedContentValue = ^TargetedContent_ITargetedContentValue;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.TargetedContent.ITargetedContentValue>
  IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = interface;
  PIKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = ^IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.TargetedContent.ITargetedContentValue>>
  IIterator_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = ^IIterator_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.TargetedContent.ITargetedContentValue>>
  IIterable_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = ^IIterable_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.Services.TargetedContent.ITargetedContentValue>
  IMapView_2__HSTRING__TargetedContent_ITargetedContentValue = interface;
  PIMapView_2__HSTRING__TargetedContent_ITargetedContentValue = ^IMapView_2__HSTRING__TargetedContent_ITargetedContentValue;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentCollection>
  IIterator_1__TargetedContent_ITargetedContentCollection = interface;
  PIIterator_1__TargetedContent_ITargetedContentCollection = ^IIterator_1__TargetedContent_ITargetedContentCollection;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentCollection>
  IIterable_1__TargetedContent_ITargetedContentCollection = interface;
  PIIterable_1__TargetedContent_ITargetedContentCollection = ^IIterable_1__TargetedContent_ITargetedContentCollection;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentCollection>
  IVectorView_1__TargetedContent_ITargetedContentCollection = interface;
  PIVectorView_1__TargetedContent_ITargetedContentCollection = ^IVectorView_1__TargetedContent_ITargetedContentCollection;

  // Windows.Services.TargetedContent.ITargetedContentItemState
  TargetedContent_ITargetedContentItemState = interface;
  PTargetedContent_ITargetedContentItemState = ^TargetedContent_ITargetedContentItemState;

  // Windows.Services.TargetedContent.ITargetedContentItem
  TargetedContent_ITargetedContentItem = interface;
  PTargetedContent_ITargetedContentItem = ^TargetedContent_ITargetedContentItem;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentItem>
  IIterator_1__TargetedContent_ITargetedContentItem = interface;
  PIIterator_1__TargetedContent_ITargetedContentItem = ^IIterator_1__TargetedContent_ITargetedContentItem;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentItem>
  IIterable_1__TargetedContent_ITargetedContentItem = interface;
  PIIterable_1__TargetedContent_ITargetedContentItem = ^IIterable_1__TargetedContent_ITargetedContentItem;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentItem>
  IVectorView_1__TargetedContent_ITargetedContentItem = interface;
  PIVectorView_1__TargetedContent_ITargetedContentItem = ^IVectorView_1__TargetedContent_ITargetedContentItem;

  // Windows.Services.TargetedContent.ITargetedContentCollection
  TargetedContent_ITargetedContentCollection = interface;
  PTargetedContent_ITargetedContentCollection = ^TargetedContent_ITargetedContentCollection;

  // Windows.Services.TargetedContent.ITargetedContentObject
  TargetedContent_ITargetedContentObject = interface;
  PTargetedContent_ITargetedContentObject = ^TargetedContent_ITargetedContentObject;

  // Windows.Services.TargetedContent.ITargetedContentContainer
  TargetedContent_ITargetedContentContainer = interface;
  PTargetedContent_ITargetedContentContainer = ^TargetedContent_ITargetedContentContainer;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.TargetedContent.ITargetedContentContainer>
  AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentContainer = interface;
  PAsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentContainer = ^AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentContainer;

  // Windows.Foundation.IAsyncOperation`1<Windows.Services.TargetedContent.ITargetedContentContainer>
  IAsyncOperation_1__TargetedContent_ITargetedContentContainer = interface;
  PIAsyncOperation_1__TargetedContent_ITargetedContentContainer = ^IAsyncOperation_1__TargetedContent_ITargetedContentContainer;

  // Windows.Services.TargetedContent.ITargetedContentContainerStatics
  TargetedContent_ITargetedContentContainerStatics = interface;
  PTargetedContent_ITargetedContentContainerStatics = ^TargetedContent_ITargetedContentContainerStatics;

  // Windows.Services.TargetedContent.ITargetedContentStateChangedEventArgs
  TargetedContent_ITargetedContentStateChangedEventArgs = interface;
  PTargetedContent_ITargetedContentStateChangedEventArgs = ^TargetedContent_ITargetedContentStateChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Services.TargetedContent.ITargetedContentSubscription,Windows.Services.TargetedContent.ITargetedContentChangedEventArgs>
  TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentChangedEventArgs = interface;
  PTypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentChangedEventArgs = ^TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Services.TargetedContent.ITargetedContentSubscription,Windows.Services.TargetedContent.ITargetedContentAvailabilityChangedEventArgs>
  TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentAvailabilityChangedEventArgs = interface;
  PTypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentAvailabilityChangedEventArgs = ^TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentAvailabilityChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Services.TargetedContent.ITargetedContentSubscription,Windows.Services.TargetedContent.ITargetedContentStateChangedEventArgs>
  TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentStateChangedEventArgs = interface;
  PTypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentStateChangedEventArgs = ^TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentStateChangedEventArgs;

  // Windows.Services.TargetedContent.ITargetedContentSubscription
  TargetedContent_ITargetedContentSubscription = interface;
  PTargetedContent_ITargetedContentSubscription = ^TargetedContent_ITargetedContentSubscription;

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

  // Windows.Services.TargetedContent.ITargetedContentSubscriptionOptions
  TargetedContent_ITargetedContentSubscriptionOptions = interface;
  PTargetedContent_ITargetedContentSubscriptionOptions = ^TargetedContent_ITargetedContentSubscriptionOptions;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.TargetedContent.ITargetedContentSubscription>
  AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentSubscription = interface;
  PAsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentSubscription = ^AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentSubscription;

  // Windows.Foundation.IAsyncOperation`1<Windows.Services.TargetedContent.ITargetedContentSubscription>
  IAsyncOperation_1__TargetedContent_ITargetedContentSubscription = interface;
  PIAsyncOperation_1__TargetedContent_ITargetedContentSubscription = ^IAsyncOperation_1__TargetedContent_ITargetedContentSubscription;

  // Windows.Services.TargetedContent.ITargetedContentSubscriptionStatics
  TargetedContent_ITargetedContentSubscriptionStatics = interface;
  PTargetedContent_ITargetedContentSubscriptionStatics = ^TargetedContent_ITargetedContentSubscriptionStatics;

  // Windows.Services Enums

  // Windows.Services.Cortana.CortanaPermission
  Cortana_CortanaPermission = (
    BrowsingHistory = 0,
    Calendar = 1,
    CallHistory = 2,
    Contacts = 3,
    Email = 4,
    InputPersonalization = 5,
    Location = 6,
    Messaging = 7,
    Microphone = 8,
    Personalization = 9,
    PhoneCall = 10
  );
  PCortana_CortanaPermission = ^Cortana_CortanaPermission;

  // Windows.Services.Cortana.CortanaPermissionsChangeResult
  Cortana_CortanaPermissionsChangeResult = (
    Success = 0,
    Unavailable = 1,
    DisabledByPolicy = 2
  );
  PCortana_CortanaPermissionsChangeResult = ^Cortana_CortanaPermissionsChangeResult;

  // Windows.Services.Maps.Guidance.GuidanceAudioMeasurementSystem
  Maps_Guidance_GuidanceAudioMeasurementSystem = (
    Meters = 0,
    MilesAndYards = 1,
    MilesAndFeet = 2
  );
  PMaps_Guidance_GuidanceAudioMeasurementSystem = ^Maps_Guidance_GuidanceAudioMeasurementSystem;

  // Windows.Services.Maps.Guidance.GuidanceAudioNotificationKind
  Maps_Guidance_GuidanceAudioNotificationKind = (
    Maneuver = 0,
    Route = 1,
    Gps = 2,
    SpeedLimit = 3,
    Traffic = 4,
    TrafficCamera = 5
  );
  PMaps_Guidance_GuidanceAudioNotificationKind = ^Maps_Guidance_GuidanceAudioNotificationKind;

  // Windows.Services.Maps.Guidance.GuidanceAudioNotifications
  Maps_Guidance_GuidanceAudioNotifications = (
    None = 0,
    Maneuver = 1,
    Route = 2,
    Gps = 4,
    SpeedLimit = 8,
    Traffic = 16,
    TrafficCamera = 32
  );
  PMaps_Guidance_GuidanceAudioNotifications = ^Maps_Guidance_GuidanceAudioNotifications;

  // Windows.Services.Maps.Guidance.GuidanceLaneMarkers
  Maps_Guidance_GuidanceLaneMarkers = (
    None = 0,
    LightRight = 1,
    Right = 2,
    HardRight = 4,
    Straight = 8,
    UTurnLeft = 16,
    HardLeft = 32,
    Left = 64,
    LightLeft = 128,
    UTurnRight = 256,
    Unknown = -1
  );
  PMaps_Guidance_GuidanceLaneMarkers = ^Maps_Guidance_GuidanceLaneMarkers;

  // Windows.Services.Maps.Guidance.GuidanceManeuverKind
  Maps_Guidance_GuidanceManeuverKind = (
    None = 0,
    GoStraight = 1,
    UTurnRight = 2,
    UTurnLeft = 3,
    TurnKeepRight = 4,
    TurnLightRight = 5,
    TurnRight = 6,
    TurnHardRight = 7,
    KeepMiddle = 8,
    TurnKeepLeft = 9,
    TurnLightLeft = 10,
    TurnLeft = 11,
    TurnHardLeft = 12,
    FreewayEnterRight = 13,
    FreewayEnterLeft = 14,
    FreewayLeaveRight = 15,
    FreewayLeaveLeft = 16,
    FreewayKeepRight = 17,
    FreewayKeepLeft = 18,
    TrafficCircleRight1 = 19,
    TrafficCircleRight2 = 20,
    TrafficCircleRight3 = 21,
    TrafficCircleRight4 = 22,
    TrafficCircleRight5 = 23,
    TrafficCircleRight6 = 24,
    TrafficCircleRight7 = 25,
    TrafficCircleRight8 = 26,
    TrafficCircleRight9 = 27,
    TrafficCircleRight10 = 28,
    TrafficCircleRight11 = 29,
    TrafficCircleRight12 = 30,
    TrafficCircleLeft1 = 31,
    TrafficCircleLeft2 = 32,
    TrafficCircleLeft3 = 33,
    TrafficCircleLeft4 = 34,
    TrafficCircleLeft5 = 35,
    TrafficCircleLeft6 = 36,
    TrafficCircleLeft7 = 37,
    TrafficCircleLeft8 = 38,
    TrafficCircleLeft9 = 39,
    TrafficCircleLeft10 = 40,
    TrafficCircleLeft11 = 41,
    TrafficCircleLeft12 = 42,
    Start = 43,
    &End = 44,
    TakeFerry = 45,
    PassTransitStation = 46,
    LeaveTransitStation = 47
  );
  PMaps_Guidance_GuidanceManeuverKind = ^Maps_Guidance_GuidanceManeuverKind;

  // Windows.Services.Maps.Guidance.GuidanceMode
  Maps_Guidance_GuidanceMode = (
    None = 0,
    Simulation = 1,
    Navigation = 2,
    Tracking = 3
  );
  PMaps_Guidance_GuidanceMode = ^Maps_Guidance_GuidanceMode;

  // Windows.Services.Maps.LocalSearch.LocalLocationFinderStatus
  Maps_LocalSearch_LocalLocationFinderStatus = (
    Success = 0,
    UnknownError = 1,
    InvalidCredentials = 2,
    InvalidCategory = 3,
    InvalidSearchTerm = 4,
    InvalidSearchArea = 5,
    NetworkFailure = 6,
    NotSupported = 7
  );
  PMaps_LocalSearch_LocalLocationFinderStatus = ^Maps_LocalSearch_LocalLocationFinderStatus;

  // Windows.Services.Maps.ManeuverWarningKind
  Maps_ManeuverWarningKind = (
    None = 0,
    Accident = 1,
    AdministrativeDivisionChange = 2,
    Alert = 3,
    BlockedRoad = 4,
    CheckTimetable = 5,
    Congestion = 6,
    Construction = 7,
    CountryChange = 8,
    DisabledVehicle = 9,
    GateAccess = 10,
    GetOffTransit = 11,
    GetOnTransit = 12,
    IllegalUTurn = 13,
    MassTransit = 14,
    Miscellaneous = 15,
    NoIncident = 16,
    Other = 17,
    OtherNews = 18,
    OtherTrafficIncidents = 19,
    PlannedEvent = 20,
    PrivateRoad = 21,
    RestrictedTurn = 22,
    RoadClosures = 23,
    RoadHazard = 24,
    ScheduledConstruction = 25,
    SeasonalClosures = 26,
    Tollbooth = 27,
    TollRoad = 28,
    TollZoneEnter = 29,
    TollZoneExit = 30,
    TrafficFlow = 31,
    TransitLineChange = 32,
    UnpavedRoad = 33,
    UnscheduledConstruction = 34,
    Weather = 35
  );
  PMaps_ManeuverWarningKind = ^Maps_ManeuverWarningKind;

  // Windows.Services.Maps.ManeuverWarningSeverity
  Maps_ManeuverWarningSeverity = (
    None = 0,
    LowImpact = 1,
    Minor = 2,
    Moderate = 3,
    Serious = 4
  );
  PMaps_ManeuverWarningSeverity = ^Maps_ManeuverWarningSeverity;

  // Windows.Services.Maps.MapLocationDesiredAccuracy
  Maps_MapLocationDesiredAccuracy = (
    High = 0,
    Low = 1
  );
  PMaps_MapLocationDesiredAccuracy = ^Maps_MapLocationDesiredAccuracy;

  // Windows.Services.Maps.MapLocationFinderStatus
  Maps_MapLocationFinderStatus = (
    Success = 0,
    UnknownError = 1,
    InvalidCredentials = 2,
    BadLocation = 3,
    IndexFailure = 4,
    NetworkFailure = 5,
    NotSupported = 6
  );
  PMaps_MapLocationFinderStatus = ^Maps_MapLocationFinderStatus;

  // Windows.Services.Maps.MapRouteFinderStatus
  Maps_MapRouteFinderStatus = (
    Success = 0,
    UnknownError = 1,
    InvalidCredentials = 2,
    NoRouteFound = 3,
    NoRouteFoundWithGivenOptions = 4,
    StartPointNotFound = 5,
    EndPointNotFound = 6,
    NoPedestrianRouteFound = 7,
    NetworkFailure = 8,
    NotSupported = 9
  );
  PMaps_MapRouteFinderStatus = ^Maps_MapRouteFinderStatus;

  // Windows.Services.Maps.MapRouteOptimization
  Maps_MapRouteOptimization = (
    Time = 0,
    Distance = 1,
    TimeWithTraffic = 2,
    Scenic = 3
  );
  PMaps_MapRouteOptimization = ^Maps_MapRouteOptimization;

  // Windows.Services.Maps.MapRouteRestrictions
  Maps_MapRouteRestrictions = (
    None = 0,
    Highways = 1,
    TollRoads = 2,
    Ferries = 4,
    Tunnels = 8,
    DirtRoads = 16,
    Motorail = 32
  );
  PMaps_MapRouteRestrictions = ^Maps_MapRouteRestrictions;

  // Windows.Services.Maps.MapServiceDataUsagePreference
  Maps_MapServiceDataUsagePreference = (
    Default = 0,
    OfflineMapDataOnly = 1
  );
  PMaps_MapServiceDataUsagePreference = ^Maps_MapServiceDataUsagePreference;

  // Windows.Services.Maps.OfflineMaps.OfflineMapPackageQueryStatus
  Maps_OfflineMaps_OfflineMapPackageQueryStatus = (
    Success = 0,
    UnknownError = 1,
    InvalidCredentials = 2,
    NetworkFailure = 3
  );
  PMaps_OfflineMaps_OfflineMapPackageQueryStatus = ^Maps_OfflineMaps_OfflineMapPackageQueryStatus;

  // Windows.Services.Maps.OfflineMaps.OfflineMapPackageStartDownloadStatus
  Maps_OfflineMaps_OfflineMapPackageStartDownloadStatus = (
    Success = 0,
    UnknownError = 1,
    InvalidCredentials = 2,
    DeniedWithoutCapability = 3
  );
  PMaps_OfflineMaps_OfflineMapPackageStartDownloadStatus = ^Maps_OfflineMaps_OfflineMapPackageStartDownloadStatus;

  // Windows.Services.Maps.OfflineMaps.OfflineMapPackageStatus
  Maps_OfflineMaps_OfflineMapPackageStatus = (
    NotDownloaded = 0,
    Downloading = 1,
    Downloaded = 2,
    Deleting = 3
  );
  PMaps_OfflineMaps_OfflineMapPackageStatus = ^Maps_OfflineMaps_OfflineMapPackageStatus;

  // Windows.Services.Maps.TrafficCongestion
  Maps_TrafficCongestion = (
    Unknown = 0,
    Light = 1,
    Mild = 2,
    Medium = 3,
    Heavy = 4
  );
  PMaps_TrafficCongestion = ^Maps_TrafficCongestion;

  // Windows.Services.Maps.WaypointKind
  Maps_WaypointKind = (
    Stop = 0,
    Via = 1
  );
  PMaps_WaypointKind = ^Maps_WaypointKind;

  // Windows.Services.TargetedContent.TargetedContentAppInstallationState
  TargetedContent_TargetedContentAppInstallationState = (
    NotApplicable = 0,
    NotInstalled = 1,
    Installed = 2
  );
  PTargetedContent_TargetedContentAppInstallationState = ^TargetedContent_TargetedContentAppInstallationState;

  // Windows.Services.TargetedContent.TargetedContentAvailability
  TargetedContent_TargetedContentAvailability = (
    None = 0,
    Partial = 1,
    All = 2
  );
  PTargetedContent_TargetedContentAvailability = ^TargetedContent_TargetedContentAvailability;

  // Windows.Services.TargetedContent.TargetedContentInteraction
  TargetedContent_TargetedContentInteraction = (
    Impression = 0,
    ClickThrough = 1,
    Hover = 2,
    Like = 3,
    Dislike = 4,
    Dismiss = 5,
    Ineligible = 6,
    Accept = 7,
    Decline = 8,
    Defer = 9,
    Canceled = 10,
    Conversion = 11,
    Opportunity = 12
  );
  PTargetedContent_TargetedContentInteraction = ^TargetedContent_TargetedContentInteraction;

  // Windows.Services.TargetedContent.TargetedContentObjectKind
  TargetedContent_TargetedContentObjectKind = (
    Collection = 0,
    Item = 1,
    Value = 2
  );
  PTargetedContent_TargetedContentObjectKind = ^TargetedContent_TargetedContentObjectKind;

  // Windows.Services.TargetedContent.TargetedContentValueKind
  TargetedContent_TargetedContentValueKind = (
    &String = 0,
    Uri = 1,
    Number = 2,
    Boolean = 3,
    &File = 4,
    ImageFile = 5,
    Action = 6,
    Strings = 7,
    Uris = 8,
    Numbers = 9,
    Booleans = 10,
    Files = 11,
    ImageFiles = 12,
    Actions = 13
  );
  PTargetedContent_TargetedContentValueKind = ^TargetedContent_TargetedContentValueKind;

  // Windows.Services Records
  // Windows.Services.Maps.GuidanceContract
  Maps_GuidanceContract = record
  end;
  PMaps_GuidanceContract = ^Maps_GuidanceContract;

  // Windows.Services.Maps.LocalSearchContract
  Maps_LocalSearchContract = record
  end;
  PMaps_LocalSearchContract = ^Maps_LocalSearchContract;

  // Windows.Services.TargetedContent.TargetedContentContract
  TargetedContent_TargetedContentContract = record
  end;
  PTargetedContent_TargetedContentContract = ^TargetedContent_TargetedContentContract;

  // Windows.Services Interfaces

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

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Cortana.CortanaPermission>
  IIterator_1__Cortana_CortanaPermission_Base = interface(IInspectable)
  ['{0F1AC33C-511A-52E8-AF09-D89F7004E8C5}']
    function get_Current: Cortana_CortanaPermission; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCortana_CortanaPermission): Cardinal; safecall;
    property Current: Cortana_CortanaPermission read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Cortana.CortanaPermission>
  IIterator_1__Cortana_CortanaPermission = interface(IIterator_1__Cortana_CortanaPermission_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Cortana.CortanaPermission>
  IIterable_1__Cortana_CortanaPermission_Base = interface(IInspectable)
  ['{36A12EAE-2E24-5E07-BFD0-344A92990916}']
    function First: IIterator_1__Cortana_CortanaPermission; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Cortana.CortanaPermission>
  IIterable_1__Cortana_CortanaPermission = interface(IIterable_1__Cortana_CortanaPermission_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Cortana.CortanaPermissionsChangeResult>
  AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult_Delegate_Base = interface(IUnknown)
  ['{EC1C6586-5E0D-5BC0-B84F-20052C5AC7A9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Cortana_CortanaPermissionsChangeResult; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.Cortana.CortanaPermissionsChangeResult>
  AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult = interface(AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Cortana.CortanaPermissionsChangeResult>
  IAsyncOperation_1__Cortana_CortanaPermissionsChangeResult_Base = interface(IInspectable)
  ['{838A3DD0-F0A3-508F-846A-D3C19E4FE7A0}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult; safecall;
    function GetResults: Cortana_CortanaPermissionsChangeResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Cortana_CortanaPermissionsChangeResult read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.Cortana.CortanaPermissionsChangeResult>
  IAsyncOperation_1__Cortana_CortanaPermissionsChangeResult = interface(IAsyncOperation_1__Cortana_CortanaPermissionsChangeResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.Cortana.ICortanaSettings
  [WinRTClassNameAttribute(SWindows_Services_Cortana_CortanaSettings)]
  Cortana_ICortanaSettings = interface(IInspectable)
  ['{54D571A7-8062-40F4-ABE7-DEDFD697B019}']
    function get_HasUserConsentToVoiceActivation: Boolean; safecall;
    function get_IsVoiceActivationEnabled: Boolean; safecall;
    procedure put_IsVoiceActivationEnabled(value: Boolean); safecall;
    property HasUserConsentToVoiceActivation: Boolean read get_HasUserConsentToVoiceActivation;
    property IsVoiceActivationEnabled: Boolean read get_IsVoiceActivationEnabled write put_IsVoiceActivationEnabled;
  end deprecated;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.Cortana.ICortanaSettingsStatics
  [WinRTClassNameAttribute(SWindows_Services_Cortana_CortanaSettings)]
  Cortana_ICortanaSettingsStatics = interface(IInspectable)
  ['{8B2CCD7E-2EC0-446D-9285-33F07CE8AC04}']
    function IsSupported: Boolean; safecall;
    function GetDefault: Cortana_ICortanaSettings; safecall;
  end deprecated;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRouteManeuver>
  IIterator_1__Maps_IMapRouteManeuver_Base = interface(IInspectable)
  ['{A7AB048B-A6DC-5E4C-9321-71B0E465DFE8}']
    function get_Current: Maps_IMapRouteManeuver; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PMaps_IMapRouteManeuver): Cardinal; safecall;
    property Current: Maps_IMapRouteManeuver read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRouteManeuver>
  IIterator_1__Maps_IMapRouteManeuver = interface(IIterator_1__Maps_IMapRouteManeuver_Base)
  ['{66D6B40E-02BB-5614-8A88-3EAC4B56C7D8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRouteManeuver>
  IIterable_1__Maps_IMapRouteManeuver_Base = interface(IInspectable)
  ['{DE9015FB-91D7-556E-BB4D-200B6F58FAD4}']
    function First: IIterator_1__Maps_IMapRouteManeuver; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRouteManeuver>
  IIterable_1__Maps_IMapRouteManeuver = interface(IIterable_1__Maps_IMapRouteManeuver_Base)
  ['{E9F4F95F-E385-5F90-A528-2790D862AC27}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRouteLeg>
  IIterator_1__Maps_IMapRouteLeg_Base = interface(IInspectable)
  ['{DD1BE7D2-DE62-5752-B2E0-A2B08723B787}']
    function get_Current: Maps_IMapRouteLeg; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PMaps_IMapRouteLeg): Cardinal; safecall;
    property Current: Maps_IMapRouteLeg read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRouteLeg>
  IIterator_1__Maps_IMapRouteLeg = interface(IIterator_1__Maps_IMapRouteLeg_Base)
  ['{4FAC7C35-4A8D-535D-866F-D0A62D187CF9}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRouteLeg>
  IIterable_1__Maps_IMapRouteLeg_Base = interface(IInspectable)
  ['{8FF98759-78CD-56E8-877B-83CE846D6F8B}']
    function First: IIterator_1__Maps_IMapRouteLeg; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRouteLeg>
  IIterable_1__Maps_IMapRouteLeg = interface(IIterable_1__Maps_IMapRouteLeg_Base)
  ['{D2228F7B-EC86-55A1-BB53-0BB9CAD09F14}']
  end;

  // UsedAPI Interface
  // Windows.Services.Maps.IMapAddress
  Maps_IMapAddress = interface(IInspectable)
  ['{CFA7A973-A3B4-4494-B3FF-CBA94DB69699}']
    function get_BuildingName: HSTRING; safecall;
    function get_BuildingFloor: HSTRING; safecall;
    function get_BuildingRoom: HSTRING; safecall;
    function get_BuildingWing: HSTRING; safecall;
    function get_StreetNumber: HSTRING; safecall;
    function get_Street: HSTRING; safecall;
    function get_Neighborhood: HSTRING; safecall;
    function get_District: HSTRING; safecall;
    function get_Town: HSTRING; safecall;
    function get_Region: HSTRING; safecall;
    function get_RegionCode: HSTRING; safecall;
    function get_Country: HSTRING; safecall;
    function get_CountryCode: HSTRING; safecall;
    function get_PostCode: HSTRING; safecall;
    function get_Continent: HSTRING; safecall;
    property BuildingFloor: HSTRING read get_BuildingFloor;
    property BuildingName: HSTRING read get_BuildingName;
    property BuildingRoom: HSTRING read get_BuildingRoom;
    property BuildingWing: HSTRING read get_BuildingWing;
    property Continent: HSTRING read get_Continent;
    property Country: HSTRING read get_Country;
    property CountryCode: HSTRING read get_CountryCode;
    property District: HSTRING read get_District;
    property Neighborhood: HSTRING read get_Neighborhood;
    property PostCode: HSTRING read get_PostCode;
    property Region: HSTRING read get_Region;
    property RegionCode: HSTRING read get_RegionCode;
    property Street: HSTRING read get_Street;
    property StreetNumber: HSTRING read get_StreetNumber;
    property Town: HSTRING read get_Town;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRoute>
  IIterator_1__Maps_IMapRoute_Base = interface(IInspectable)
  ['{97E8485A-79C0-5343-93D1-47CDFB55246B}']
    function get_Current: Maps_IMapRoute; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PMaps_IMapRoute): Cardinal; safecall;
    property Current: Maps_IMapRoute read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.IMapRoute>
  IIterator_1__Maps_IMapRoute = interface(IIterator_1__Maps_IMapRoute_Base)
  ['{56B8F0FF-4704-52AF-98E0-F03A04FA0941}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRoute>
  IIterable_1__Maps_IMapRoute_Base = interface(IInspectable)
  ['{D88A62A2-0EDF-5312-97A8-10AEAEA80B99}']
    function First: IIterator_1__Maps_IMapRoute; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.IMapRoute>
  IIterable_1__Maps_IMapRoute = interface(IIterable_1__Maps_IMapRoute_Base)
  ['{52B08F5A-C6C8-53D3-AC5D-22F911E430F8}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.IMapRoute>
  IVectorView_1__Maps_IMapRoute = interface(IInspectable)
  ['{1E6FCAD9-EA40-524D-ACF1-5E394278E1BC}']
    function GetAt(index: Cardinal): Maps_IMapRoute; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Maps_IMapRoute; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_IMapRoute): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Services.Maps.LocalSearch.ILocalLocation
  Maps_LocalSearch_ILocalLocation = interface(IInspectable)
  ['{BB0FE9AB-4502-4F2C-94A9-0D60DE0E2163}']
    function get_Address: Maps_IMapAddress; safecall;
    function get_Identifier: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_Point: IGeopoint; safecall;
    function get_PhoneNumber: HSTRING; safecall;
    function get_DataAttribution: HSTRING; safecall;
    property Address: Maps_IMapAddress read get_Address;
    property DataAttribution: HSTRING read get_DataAttribution;
    property Description: HSTRING read get_Description;
    property DisplayName: HSTRING read get_DisplayName;
    property Identifier: HSTRING read get_Identifier;
    property PhoneNumber: HSTRING read get_PhoneNumber;
    property Point: IGeopoint read get_Point;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IIterator_1__Maps_LocalSearch_ILocalLocation_Base = interface(IInspectable)
  ['{A23C2E87-A8E7-568F-96A1-69E98F86B9D3}']
    function get_Current: Maps_LocalSearch_ILocalLocation; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PMaps_LocalSearch_ILocalLocation): Cardinal; safecall;
    property Current: Maps_LocalSearch_ILocalLocation read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IIterator_1__Maps_LocalSearch_ILocalLocation = interface(IIterator_1__Maps_LocalSearch_ILocalLocation_Base)
  ['{C2395F06-B5D9-5B7F-94CE-5E5B9A8BD90A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IIterable_1__Maps_LocalSearch_ILocalLocation_Base = interface(IInspectable)
  ['{5474EEFB-60F4-58AC-89E2-6E83F79FA76C}']
    function First: IIterator_1__Maps_LocalSearch_ILocalLocation; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IIterable_1__Maps_LocalSearch_ILocalLocation = interface(IIterable_1__Maps_LocalSearch_ILocalLocation_Base)
  ['{2ED58A59-15CF-5CEA-8357-C021C5567BCF}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.Maps.LocalSearch.ILocalLocation>
  IVectorView_1__Maps_LocalSearch_ILocalLocation = interface(IInspectable)
  ['{4E62950D-5A21-5B97-BB8E-D41C7C051DC7}']
    function GetAt(index: Cardinal): Maps_LocalSearch_ILocalLocation; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Maps_LocalSearch_ILocalLocation; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMaps_LocalSearch_ILocalLocation): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentAction
  TargetedContent_ITargetedContentAction = interface(IInspectable)
  ['{D75B691E-6CD6-4CA0-9D8F-4728B0B7E6B6}']
    function InvokeAsync: IAsyncAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentAvailabilityChangedEventArgs
  TargetedContent_ITargetedContentAvailabilityChangedEventArgs = interface(IInspectable)
  ['{E0F59D26-5927-4450-965C-1CEB7BECDE65}']
    function GetDeferral: IDeferral; safecall;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentChangedEventArgs
  TargetedContent_ITargetedContentChangedEventArgs = interface(IInspectable)
  ['{99D488C9-587E-4586-8EF7-B54CA9453A16}']
    function GetDeferral: IDeferral; safecall;
    function get_HasPreviousContentExpired: Boolean; safecall;
    property HasPreviousContentExpired: Boolean read get_HasPreviousContentExpired;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentImage
  TargetedContent_ITargetedContentImage = interface(IInspectable)
  ['{A7A585D9-779F-4B1E-BBB1-8EAF53FBEAB2}']
    function get_Height: Cardinal; safecall;
    function get_Width: Cardinal; safecall;
    property Height: Cardinal read get_Height;
    property Width: Cardinal read get_Width;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Double>
  IIterator_1__Double_Base = interface(IInspectable)
  ['{638A2CF4-F474-5318-9055-141CB909AC4B}']
    function get_Current: Double; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDouble): Cardinal; safecall;
    property Current: Double read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Double>
  IIterator_1__Double = interface(IIterator_1__Double_Base)
  ['{638A2CF4-F474-5318-9055-141CB909AC4B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Double>
  IIterable_1__Double_Base = interface(IInspectable)
  ['{C738964E-9C64-5BCE-B5CE-61E9A282EC4A}']
    function First: IIterator_1__Double; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Double>
  IIterable_1__Double = interface(IIterable_1__Double_Base)
  ['{C738964E-9C64-5BCE-B5CE-61E9A282EC4A}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Double>
  IVectorView_1__Double = interface(IInspectable)
  ['{AF7586A8-6B21-5F61-BFF1-1B682293AD96}']
    function GetAt(index: Cardinal): Double; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Double; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDouble): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Boolean>
  IIterator_1__Boolean_Base = interface(IInspectable)
  ['{740A0296-A535-572A-BF0B-17C18FF71FE6}']
    function get_Current: Boolean; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PBoolean): Cardinal; safecall;
    property Current: Boolean read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Boolean>
  IIterator_1__Boolean = interface(IIterator_1__Boolean_Base)
  ['{740A0296-A535-572A-BF0B-17C18FF71FE6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Boolean>
  IIterable_1__Boolean_Base = interface(IInspectable)
  ['{30160817-1D7D-54E9-99DB-D7636266A476}']
    function First: IIterator_1__Boolean; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Boolean>
  IIterable_1__Boolean = interface(IIterable_1__Boolean_Base)
  ['{30160817-1D7D-54E9-99DB-D7636266A476}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Boolean>
  IVectorView_1__Boolean = interface(IInspectable)
  ['{243A09CB-6F40-56AF-A442-FE81431FBEF5}']
    function GetAt(index: Cardinal): Boolean; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Boolean; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PBoolean): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentImage>
  IIterator_1__TargetedContent_ITargetedContentImage = interface(IInspectable)
  ['{1C83E55A-3608-5AB1-943C-E33B44B1E90D}']
    function get_Current: TargetedContent_ITargetedContentImage; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PTargetedContent_ITargetedContentImage): Cardinal; safecall;
    property Current: TargetedContent_ITargetedContentImage read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentImage>
  IIterable_1__TargetedContent_ITargetedContentImage = interface(IInspectable)
  ['{9A43508E-4B30-551B-8E9F-CCC976B881DA}']
    function First: IIterator_1__TargetedContent_ITargetedContentImage; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentImage>
  IVectorView_1__TargetedContent_ITargetedContentImage = interface(IInspectable)
  ['{0D42E297-EAB2-5574-B73F-C81C029C4FC7}']
    function GetAt(index: Cardinal): TargetedContent_ITargetedContentImage; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TargetedContent_ITargetedContentImage; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PTargetedContent_ITargetedContentImage): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentAction>
  IIterator_1__TargetedContent_ITargetedContentAction = interface(IInspectable)
  ['{92E3EADE-F4FF-58AE-A4A8-AB377B72AB53}']
    function get_Current: TargetedContent_ITargetedContentAction; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PTargetedContent_ITargetedContentAction): Cardinal; safecall;
    property Current: TargetedContent_ITargetedContentAction read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentAction>
  IIterable_1__TargetedContent_ITargetedContentAction = interface(IInspectable)
  ['{14C2653E-03F5-5EE6-B439-C5A8EA794DC5}']
    function First: IIterator_1__TargetedContent_ITargetedContentAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentAction>
  IVectorView_1__TargetedContent_ITargetedContentAction = interface(IInspectable)
  ['{C3C1FD9C-D409-5CAD-9DBB-F8E6D4663D36}']
    function GetAt(index: Cardinal): TargetedContent_ITargetedContentAction; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TargetedContent_ITargetedContentAction; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PTargetedContent_ITargetedContentAction): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentValue
  TargetedContent_ITargetedContentValue = interface(IInspectable)
  ['{AAFDE4B3-4215-4BF8-867F-43F04865F9BF}']
    function get_ValueKind: TargetedContent_TargetedContentValueKind; safecall;
    function get_Path: HSTRING; safecall;
    function get_String: HSTRING; safecall;
    function get_Uri: IUriRuntimeClass; safecall;
    function get_Number: Double; safecall;
    function get_Boolean: Boolean; safecall;
    function get_File: IRandomAccessStreamReference; safecall;
    function get_ImageFile: TargetedContent_ITargetedContentImage; safecall;
    function get_Action: TargetedContent_ITargetedContentAction; safecall;
    function get_Strings: IVectorView_1__HSTRING; safecall;
    function get_Uris: IVectorView_1__IUriRuntimeClass; safecall;
    function get_Numbers: IVectorView_1__Double; safecall;
    function get_Booleans: IVectorView_1__Boolean; safecall;
    function get_Files: IVectorView_1__IRandomAccessStreamReference; safecall;
    function get_ImageFiles: IVectorView_1__TargetedContent_ITargetedContentImage; safecall;
    function get_Actions: IVectorView_1__TargetedContent_ITargetedContentAction; safecall;
    property Action: TargetedContent_ITargetedContentAction read get_Action;
    property Actions: IVectorView_1__TargetedContent_ITargetedContentAction read get_Actions;
    property Boolean_: Boolean read get_Boolean;
    property Booleans: IVectorView_1__Boolean read get_Booleans;
    property &File: IRandomAccessStreamReference read get_File;
    property Files: IVectorView_1__IRandomAccessStreamReference read get_Files;
    property ImageFile: TargetedContent_ITargetedContentImage read get_ImageFile;
    property ImageFiles: IVectorView_1__TargetedContent_ITargetedContentImage read get_ImageFiles;
    property Number: Double read get_Number;
    property Numbers: IVectorView_1__Double read get_Numbers;
    property Path: HSTRING read get_Path;
    property &String: HSTRING read get_String;
    property Strings: IVectorView_1__HSTRING read get_Strings;
    property Uri: IUriRuntimeClass read get_Uri;
    property Uris: IVectorView_1__IUriRuntimeClass read get_Uris;
    property ValueKind: TargetedContent_TargetedContentValueKind read get_ValueKind;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.TargetedContent.ITargetedContentValue>
  IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = interface(IInspectable)
  ['{5EE8132B-C17A-5E9C-A5B8-50046EFA47B2}']
    function get_Key: HSTRING; safecall;
    function get_Value: TargetedContent_ITargetedContentValue; safecall;
    property Key: HSTRING read get_Key;
    property Value: TargetedContent_ITargetedContentValue read get_Value;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.TargetedContent.ITargetedContentValue>>
  IIterator_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = interface(IInspectable)
  ['{D7033487-BEE9-52AA-9301-F44E20E22419}']
    function get_Current: IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Services.TargetedContent.ITargetedContentValue>>
  IIterable_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue = interface(IInspectable)
  ['{FE39EE98-B2BC-51D4-A65C-92FC53E14A1D}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__TargetedContent_ITargetedContentValue; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Services.TargetedContent.ITargetedContentValue>
  IMapView_2__HSTRING__TargetedContent_ITargetedContentValue = interface(IInspectable)
  ['{912F2A9E-4709-5963-8FDB-FA84BEDBD648}']
    function Lookup(key: HSTRING): TargetedContent_ITargetedContentValue; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__TargetedContent_ITargetedContentValue; out second: IMapView_2__HSTRING__TargetedContent_ITargetedContentValue); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentCollection>
  IIterator_1__TargetedContent_ITargetedContentCollection = interface(IInspectable)
  ['{8B466069-8C41-57F4-8D92-B7E375C8D3D9}']
    function get_Current: TargetedContent_ITargetedContentCollection; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PTargetedContent_ITargetedContentCollection): Cardinal; safecall;
    property Current: TargetedContent_ITargetedContentCollection read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentCollection>
  IIterable_1__TargetedContent_ITargetedContentCollection = interface(IInspectable)
  ['{E7CFDBAD-0551-59B8-881F-4448B45E57D8}']
    function First: IIterator_1__TargetedContent_ITargetedContentCollection; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentCollection>
  IVectorView_1__TargetedContent_ITargetedContentCollection = interface(IInspectable)
  ['{95C4EE43-C628-5A3B-94FD-7A1E33C25E7B}']
    function GetAt(index: Cardinal): TargetedContent_ITargetedContentCollection; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TargetedContent_ITargetedContentCollection; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PTargetedContent_ITargetedContentCollection): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentItemState
  TargetedContent_ITargetedContentItemState = interface(IInspectable)
  ['{73935454-4C65-4B47-A441-472DE53C79B6}']
    function get_ShouldDisplay: Boolean; safecall;
    function get_AppInstallationState: TargetedContent_TargetedContentAppInstallationState; safecall;
    property AppInstallationState: TargetedContent_TargetedContentAppInstallationState read get_AppInstallationState;
    property ShouldDisplay: Boolean read get_ShouldDisplay;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentItem
  TargetedContent_ITargetedContentItem = interface(IInspectable)
  ['{38168DC4-276C-4C32-96BA-565C6E406E74}']
    function get_Path: HSTRING; safecall;
    procedure ReportInteraction(interaction: TargetedContent_TargetedContentInteraction); safecall;
    procedure ReportCustomInteraction(customInteractionName: HSTRING); safecall;
    function get_State: TargetedContent_ITargetedContentItemState; safecall;
    function get_Properties: IMapView_2__HSTRING__TargetedContent_ITargetedContentValue; safecall;
    function get_Collections: IVectorView_1__TargetedContent_ITargetedContentCollection; safecall;
    property Collections: IVectorView_1__TargetedContent_ITargetedContentCollection read get_Collections;
    property Path: HSTRING read get_Path;
    property Properties: IMapView_2__HSTRING__TargetedContent_ITargetedContentValue read get_Properties;
    property State: TargetedContent_ITargetedContentItemState read get_State;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Services.TargetedContent.ITargetedContentItem>
  IIterator_1__TargetedContent_ITargetedContentItem = interface(IInspectable)
  ['{30DC72C3-92DB-5061-A737-41CD0B30CBD9}']
    function get_Current: TargetedContent_ITargetedContentItem; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PTargetedContent_ITargetedContentItem): Cardinal; safecall;
    property Current: TargetedContent_ITargetedContentItem read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Services.TargetedContent.ITargetedContentItem>
  IIterable_1__TargetedContent_ITargetedContentItem = interface(IInspectable)
  ['{285C6A83-EE35-520C-BD25-FF69B60C9142}']
    function First: IIterator_1__TargetedContent_ITargetedContentItem; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Services.TargetedContent.ITargetedContentItem>
  IVectorView_1__TargetedContent_ITargetedContentItem = interface(IInspectable)
  ['{7D1E349F-F3DB-52DD-8648-0F95A9E2ADBC}']
    function GetAt(index: Cardinal): TargetedContent_ITargetedContentItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TargetedContent_ITargetedContentItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PTargetedContent_ITargetedContentItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentCollection
  TargetedContent_ITargetedContentCollection = interface(IInspectable)
  ['{2D4B66C5-F163-44BA-9F6E-E1A4C2BB559D}']
    function get_Id: HSTRING; safecall;
    procedure ReportInteraction(interaction: TargetedContent_TargetedContentInteraction); safecall;
    procedure ReportCustomInteraction(customInteractionName: HSTRING); safecall;
    function get_Path: HSTRING; safecall;
    function get_Properties: IMapView_2__HSTRING__TargetedContent_ITargetedContentValue; safecall;
    function get_Collections: IVectorView_1__TargetedContent_ITargetedContentCollection; safecall;
    function get_Items: IVectorView_1__TargetedContent_ITargetedContentItem; safecall;
    property Collections: IVectorView_1__TargetedContent_ITargetedContentCollection read get_Collections;
    property Id: HSTRING read get_Id;
    property Items: IVectorView_1__TargetedContent_ITargetedContentItem read get_Items;
    property Path: HSTRING read get_Path;
    property Properties: IMapView_2__HSTRING__TargetedContent_ITargetedContentValue read get_Properties;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentObject
  TargetedContent_ITargetedContentObject = interface(IInspectable)
  ['{041D7969-2212-42D1-9DFA-88A8E3033AA3}']
    function get_ObjectKind: TargetedContent_TargetedContentObjectKind; safecall;
    function get_Collection: TargetedContent_ITargetedContentCollection; safecall;
    function get_Item: TargetedContent_ITargetedContentItem; safecall;
    function get_Value: TargetedContent_ITargetedContentValue; safecall;
    property Collection: TargetedContent_ITargetedContentCollection read get_Collection;
    property Item: TargetedContent_ITargetedContentItem read get_Item;
    property ObjectKind: TargetedContent_TargetedContentObjectKind read get_ObjectKind;
    property Value: TargetedContent_ITargetedContentValue read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentContainer
  [WinRTClassNameAttribute(SWindows_Services_TargetedContent_TargetedContentContainer)]
  TargetedContent_ITargetedContentContainer = interface(IInspectable)
  ['{BC2494C9-8837-47C2-850F-D79D64595926}']
    function get_Id: HSTRING; safecall;
    function get_Timestamp: DateTime; safecall;
    function get_Availability: TargetedContent_TargetedContentAvailability; safecall;
    function get_Content: TargetedContent_ITargetedContentCollection; safecall;
    function SelectSingleObject(path: HSTRING): TargetedContent_ITargetedContentObject; safecall;
    property Availability: TargetedContent_TargetedContentAvailability read get_Availability;
    property Content: TargetedContent_ITargetedContentCollection read get_Content;
    property Id: HSTRING read get_Id;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.TargetedContent.ITargetedContentContainer>
  AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentContainer = interface(IUnknown)
  ['{FED1B264-C32E-532C-A751-020C559F2EDE}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__TargetedContent_ITargetedContentContainer; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.TargetedContent.ITargetedContentContainer>
  IAsyncOperation_1__TargetedContent_ITargetedContentContainer = interface(IInspectable)
  ['{E1123519-91E3-5624-B9D3-D89427E6DECC}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentContainer); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentContainer; safecall;
    function GetResults: TargetedContent_ITargetedContentContainer; safecall;
    property Completed: AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentContainer read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentContainerStatics
  [WinRTClassNameAttribute(SWindows_Services_TargetedContent_TargetedContentContainer)]
  TargetedContent_ITargetedContentContainerStatics = interface(IInspectable)
  ['{5B47E7FB-2140-4C1F-A736-C59583F227D8}']
    function GetAsync(contentId: HSTRING): IAsyncOperation_1__TargetedContent_ITargetedContentContainer; safecall;
  end;

  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentStateChangedEventArgs
  TargetedContent_ITargetedContentStateChangedEventArgs = interface(IInspectable)
  ['{9A1CEF3D-8073-4416-8DF2-546835A6414F}']
    function GetDeferral: IDeferral; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Services.TargetedContent.ITargetedContentSubscription,Windows.Services.TargetedContent.ITargetedContentChangedEventArgs>
  TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentChangedEventArgs = interface(IUnknown)
  ['{F783CE54-8F3E-532E-9D4B-64747270FB07}']
    procedure Invoke(sender: TargetedContent_ITargetedContentSubscription; args: TargetedContent_ITargetedContentChangedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Services.TargetedContent.ITargetedContentSubscription,Windows.Services.TargetedContent.ITargetedContentAvailabilityChangedEventArgs>
  TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentAvailabilityChangedEventArgs = interface(IUnknown)
  ['{967DFE2E-B4FE-5C8A-BDF2-C5F578917B11}']
    procedure Invoke(sender: TargetedContent_ITargetedContentSubscription; args: TargetedContent_ITargetedContentAvailabilityChangedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Services.TargetedContent.ITargetedContentSubscription,Windows.Services.TargetedContent.ITargetedContentStateChangedEventArgs>
  TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentStateChangedEventArgs = interface(IUnknown)
  ['{3DF9C7F4-A4B2-564F-AA7F-06C5D9689C6C}']
    procedure Invoke(sender: TargetedContent_ITargetedContentSubscription; args: TargetedContent_ITargetedContentStateChangedEventArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentSubscription
  [WinRTClassNameAttribute(SWindows_Services_TargetedContent_TargetedContentSubscription)]
  TargetedContent_ITargetedContentSubscription = interface(IInspectable)
  ['{882C2C49-C652-4C7A-ACAD-1F7FA2986C73}']
    function get_Id: HSTRING; safecall;
    function GetContentContainerAsync: IAsyncOperation_1__TargetedContent_ITargetedContentContainer; safecall;
    function add_ContentChanged(handler: TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ContentChanged(cookie: EventRegistrationToken); safecall;
    function add_AvailabilityChanged(handler: TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentAvailabilityChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AvailabilityChanged(cookie: EventRegistrationToken); safecall;
    function add_StateChanged(handler: TypedEventHandler_2__TargetedContent_ITargetedContentSubscription__TargetedContent_ITargetedContentStateChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StateChanged(cookie: EventRegistrationToken); safecall;
    property Id: HSTRING read get_Id;
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
  // Windows.Services.TargetedContent.ITargetedContentSubscriptionOptions
  TargetedContent_ITargetedContentSubscriptionOptions = interface(IInspectable)
  ['{61EE6AD0-2C83-421B-8467-413EAF1AEB97}']
    function get_SubscriptionId: HSTRING; safecall;
    function get_AllowPartialContentAvailability: Boolean; safecall;
    procedure put_AllowPartialContentAvailability(value: Boolean); safecall;
    function get_CloudQueryParameters: IMap_2__HSTRING__HSTRING; safecall;
    function get_LocalFilters: IVector_1__HSTRING; safecall;
    procedure Update; safecall;
    property AllowPartialContentAvailability: Boolean read get_AllowPartialContentAvailability write put_AllowPartialContentAvailability;
    property CloudQueryParameters: IMap_2__HSTRING__HSTRING read get_CloudQueryParameters;
    property LocalFilters: IVector_1__HSTRING read get_LocalFilters;
    property SubscriptionId: HSTRING read get_SubscriptionId;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Services.TargetedContent.ITargetedContentSubscription>
  AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentSubscription = interface(IUnknown)
  ['{D2B7262E-1BEA-57BB-B2AD-57ABA1E8D6B9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__TargetedContent_ITargetedContentSubscription; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Services.TargetedContent.ITargetedContentSubscription>
  IAsyncOperation_1__TargetedContent_ITargetedContentSubscription = interface(IInspectable)
  ['{1BFE512A-FF67-5732-859C-5D1FA236EBAB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentSubscription); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentSubscription; safecall;
    function GetResults: TargetedContent_ITargetedContentSubscription; safecall;
    property Completed: AsyncOperationCompletedHandler_1__TargetedContent_ITargetedContentSubscription read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Services.TargetedContent.ITargetedContentSubscriptionStatics
  [WinRTClassNameAttribute(SWindows_Services_TargetedContent_TargetedContentSubscription)]
  TargetedContent_ITargetedContentSubscriptionStatics = interface(IInspectable)
  ['{FADDFE80-360D-4916-B53C-7EA27090D02A}']
    function GetAsync(subscriptionId: HSTRING): IAsyncOperation_1__TargetedContent_ITargetedContentSubscription; safecall;
    function GetOptions(subscriptionId: HSTRING): TargetedContent_ITargetedContentSubscriptionOptions; safecall;
  end;

  // Windows.Services.Cortana.CortanaSettings
  // DualAPI
  // Implements: Windows.Services.Cortana.ICortanaSettings
  // Statics: "Windows.Services.Cortana.ICortanaSettingsStatics"
  TCortana_CortanaSettings = class(TWinRTGenericImportS<Cortana_ICortanaSettingsStatics>)
  public
    // -> Cortana_ICortanaSettingsStatics
    class function IsSupported: Boolean; static; inline;
    class function GetDefault: Cortana_ICortanaSettings; static; inline;
  end;

  // Windows.Services.TargetedContent.TargetedContentContainer
  // DualAPI
  // Implements: Windows.Services.TargetedContent.ITargetedContentContainer
  // Statics: "Windows.Services.TargetedContent.ITargetedContentContainerStatics"
  TTargetedContent_TargetedContentContainer = class(TWinRTGenericImportS<TargetedContent_ITargetedContentContainerStatics>)
  public
    // -> TargetedContent_ITargetedContentContainerStatics
    class function GetAsync(contentId: HSTRING): IAsyncOperation_1__TargetedContent_ITargetedContentContainer; static; inline;
  end;

  // Windows.Services.TargetedContent.TargetedContentSubscription
  // DualAPI
  // Implements: Windows.Services.TargetedContent.ITargetedContentSubscription
  // Statics: "Windows.Services.TargetedContent.ITargetedContentSubscriptionStatics"
  TTargetedContent_TargetedContentSubscription = class(TWinRTGenericImportS<TargetedContent_ITargetedContentSubscriptionStatics>)
  public
    // -> TargetedContent_ITargetedContentSubscriptionStatics
    class function GetAsync(subscriptionId: HSTRING): IAsyncOperation_1__TargetedContent_ITargetedContentSubscription; static; inline;
    class function GetOptions(subscriptionId: HSTRING): TargetedContent_ITargetedContentSubscriptionOptions; static; inline;
  end;

implementation

{ TCortana_CortanaSettings }

class function TCortana_CortanaSettings.IsSupported: Boolean;
begin
  Result := Statics.IsSupported;
end;

class function TCortana_CortanaSettings.GetDefault: Cortana_ICortanaSettings;
begin
  Result := Statics.GetDefault;
end;


{ TTargetedContent_TargetedContentContainer }

class function TTargetedContent_TargetedContentContainer.GetAsync(contentId: HSTRING): IAsyncOperation_1__TargetedContent_ITargetedContentContainer;
begin
  Result := Statics.GetAsync(contentId);
end;


{ TTargetedContent_TargetedContentSubscription }

class function TTargetedContent_TargetedContentSubscription.GetAsync(subscriptionId: HSTRING): IAsyncOperation_1__TargetedContent_ITargetedContentSubscription;
begin
  Result := Statics.GetAsync(subscriptionId);
end;

class function TTargetedContent_TargetedContentSubscription.GetOptions(subscriptionId: HSTRING): TargetedContent_ITargetedContentSubscriptionOptions;
begin
  Result := Statics.GetOptions(subscriptionId);
end;


end.
