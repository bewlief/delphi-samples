{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.SystemRT;

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
  Winapi.WebRT, 
  Winapi.Networking.Sockets, 
  Winapi.DataRT, 
  Winapi.ApplicationModel, 
  Winapi.Storage, 
  Winapi.UI.ViewManagement, 
  Winapi.Devices.Sensors, 
  Winapi.Security.Credentials, 
  Winapi.Globalization, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  AsyncOperationCompletedHandler_1__Cardinal_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__Cardinal_Delegate_Base;
  AsyncOperationCompletedHandler_1__Cardinal = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__Cardinal;
  PAsyncOperationCompletedHandler_1__Cardinal = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__Cardinal;
  AsyncOperationCompletedHandler_1__HSTRING_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__HSTRING_Delegate_Base;
  AsyncOperationCompletedHandler_1__HSTRING = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__HSTRING;
  PAsyncOperationCompletedHandler_1__HSTRING = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__HSTRING;
  AsyncOperationCompletedHandler_1__IInspectable_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IInspectable_Delegate_Base;
  AsyncOperationCompletedHandler_1__IInspectable = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IInspectable;
  PAsyncOperationCompletedHandler_1__IInspectable = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IInspectable;
  AsyncOperationProgressHandler_2__HSTRING__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationProgressHandler_2__HSTRING__UInt64_Delegate_Base;
  AsyncOperationProgressHandler_2__HSTRING__UInt64 = Winapi.CommonTypes.AsyncOperationProgressHandler_2__HSTRING__UInt64;
  PAsyncOperationProgressHandler_2__HSTRING__UInt64 = Winapi.CommonTypes.PAsyncOperationProgressHandler_2__HSTRING__UInt64;
  AsyncOperationProgressHandler_2__IBuffer__Cardinal_Delegate_Base = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IBuffer__Cardinal_Delegate_Base;
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IBuffer__Cardinal;
  PAsyncOperationProgressHandler_2__IBuffer__Cardinal = Winapi.CommonTypes.PAsyncOperationProgressHandler_2__IBuffer__Cardinal;
  AsyncOperationProgressHandler_2__IBuffer__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IBuffer__UInt64_Delegate_Base;
  AsyncOperationProgressHandler_2__IBuffer__UInt64 = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IBuffer__UInt64;
  PAsyncOperationProgressHandler_2__IBuffer__UInt64 = Winapi.CommonTypes.PAsyncOperationProgressHandler_2__IBuffer__UInt64;
  AsyncOperationProgressHandler_2__IInputStream__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IInputStream__UInt64_Delegate_Base;
  AsyncOperationProgressHandler_2__IInputStream__UInt64 = Winapi.CommonTypes.AsyncOperationProgressHandler_2__IInputStream__UInt64;
  PAsyncOperationProgressHandler_2__IInputStream__UInt64 = Winapi.CommonTypes.PAsyncOperationProgressHandler_2__IInputStream__UInt64;
  AsyncOperationProgressHandler_2__UInt64__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationProgressHandler_2__UInt64__UInt64_Delegate_Base;
  AsyncOperationProgressHandler_2__UInt64__UInt64 = Winapi.CommonTypes.AsyncOperationProgressHandler_2__UInt64__UInt64;
  PAsyncOperationProgressHandler_2__UInt64__UInt64 = Winapi.CommonTypes.PAsyncOperationProgressHandler_2__UInt64__UInt64;
  AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64_Delegate_Base;
  AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64 = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64;
  PAsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64 = Winapi.CommonTypes.PAsyncOperationWithProgressCompletedHandler_2__HSTRING__UInt64;
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal_Delegate_Base = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal_Delegate_Base;
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = Winapi.CommonTypes.PAsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal;
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64_Delegate_Base;
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64 = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64 = Winapi.CommonTypes.PAsyncOperationWithProgressCompletedHandler_2__IBuffer__UInt64;
  AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64_Delegate_Base;
  AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64 = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64;
  PAsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64 = Winapi.CommonTypes.PAsyncOperationWithProgressCompletedHandler_2__IInputStream__UInt64;
  AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64_Delegate_Base;
  AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64;
  PAsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64 = Winapi.CommonTypes.PAsyncOperationWithProgressCompletedHandler_2__UInt64__UInt64;
  DispatcherQueueHandler = Winapi.CommonTypes.DispatcherQueueHandler;
  PDispatcherQueueHandler = Winapi.CommonTypes.PDispatcherQueueHandler;
  DispatcherQueuePriority = Winapi.CommonTypes.DispatcherQueuePriority;
  PDispatcherQueuePriority = Winapi.CommonTypes.PDispatcherQueuePriority;
  EventHandler_1__IInspectable_Delegate_Base = Winapi.CommonTypes.EventHandler_1__IInspectable_Delegate_Base;
  EventHandler_1__IInspectable = Winapi.CommonTypes.EventHandler_1__IInspectable;
  PEventHandler_1__IInspectable = Winapi.CommonTypes.PEventHandler_1__IInspectable;
  IAsyncOperation_1__Cardinal_Base = Winapi.CommonTypes.IAsyncOperation_1__Cardinal_Base;
  IAsyncOperation_1__Cardinal = Winapi.CommonTypes.IAsyncOperation_1__Cardinal;
  PIAsyncOperation_1__Cardinal = Winapi.CommonTypes.PIAsyncOperation_1__Cardinal;
  IAsyncOperation_1__HSTRING_Base = Winapi.CommonTypes.IAsyncOperation_1__HSTRING_Base;
  IAsyncOperation_1__HSTRING = Winapi.CommonTypes.IAsyncOperation_1__HSTRING;
  PIAsyncOperation_1__HSTRING = Winapi.CommonTypes.PIAsyncOperation_1__HSTRING;
  IAsyncOperation_1__IInspectable_Base = Winapi.CommonTypes.IAsyncOperation_1__IInspectable_Base;
  IAsyncOperation_1__IInspectable = Winapi.CommonTypes.IAsyncOperation_1__IInspectable;
  PIAsyncOperation_1__IInspectable = Winapi.CommonTypes.PIAsyncOperation_1__IInspectable;
  IAsyncOperationWithProgress_2__HSTRING__UInt64_Base = Winapi.CommonTypes.IAsyncOperationWithProgress_2__HSTRING__UInt64_Base;
  IAsyncOperationWithProgress_2__HSTRING__UInt64 = Winapi.CommonTypes.IAsyncOperationWithProgress_2__HSTRING__UInt64;
  PIAsyncOperationWithProgress_2__HSTRING__UInt64 = Winapi.CommonTypes.PIAsyncOperationWithProgress_2__HSTRING__UInt64;
  IAsyncOperationWithProgress_2__IBuffer__Cardinal_Base = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IBuffer__Cardinal_Base;
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IBuffer__Cardinal;
  PIAsyncOperationWithProgress_2__IBuffer__Cardinal = Winapi.CommonTypes.PIAsyncOperationWithProgress_2__IBuffer__Cardinal;
  IAsyncOperationWithProgress_2__IBuffer__UInt64_Base = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IBuffer__UInt64_Base;
  IAsyncOperationWithProgress_2__IBuffer__UInt64 = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IBuffer__UInt64;
  PIAsyncOperationWithProgress_2__IBuffer__UInt64 = Winapi.CommonTypes.PIAsyncOperationWithProgress_2__IBuffer__UInt64;
  IAsyncOperationWithProgress_2__IInputStream__UInt64_Base = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IInputStream__UInt64_Base;
  IAsyncOperationWithProgress_2__IInputStream__UInt64 = Winapi.CommonTypes.IAsyncOperationWithProgress_2__IInputStream__UInt64;
  PIAsyncOperationWithProgress_2__IInputStream__UInt64 = Winapi.CommonTypes.PIAsyncOperationWithProgress_2__IInputStream__UInt64;
  IAsyncOperationWithProgress_2__UInt64__UInt64_Base = Winapi.CommonTypes.IAsyncOperationWithProgress_2__UInt64__UInt64_Base;
  IAsyncOperationWithProgress_2__UInt64__UInt64 = Winapi.CommonTypes.IAsyncOperationWithProgress_2__UInt64__UInt64;
  PIAsyncOperationWithProgress_2__UInt64__UInt64 = Winapi.CommonTypes.PIAsyncOperationWithProgress_2__UInt64__UInt64;
  IDispatcherQueue = Winapi.CommonTypes.IDispatcherQueue;
  PIDispatcherQueue = Winapi.CommonTypes.PIDispatcherQueue;
  IDispatcherQueueShutdownStartingEventArgs = Winapi.CommonTypes.IDispatcherQueueShutdownStartingEventArgs;
  PIDispatcherQueueShutdownStartingEventArgs = Winapi.CommonTypes.PIDispatcherQueueShutdownStartingEventArgs;
  IDispatcherQueueTimer = Winapi.CommonTypes.IDispatcherQueueTimer;
  PIDispatcherQueueTimer = Winapi.CommonTypes.PIDispatcherQueueTimer;
  IIterable_1__HSTRING_Base = Winapi.CommonTypes.IIterable_1__HSTRING_Base;
  IIterable_1__HSTRING = Winapi.CommonTypes.IIterable_1__HSTRING;
  PIIterable_1__HSTRING = Winapi.CommonTypes.PIIterable_1__HSTRING;
  IIterable_1__IKeyValuePair_2__HSTRING__HSTRING_Base = Winapi.CommonTypes.IIterable_1__IKeyValuePair_2__HSTRING__HSTRING_Base;
  IIterable_1__IKeyValuePair_2__HSTRING__HSTRING = Winapi.CommonTypes.IIterable_1__IKeyValuePair_2__HSTRING__HSTRING;
  PIIterable_1__IKeyValuePair_2__HSTRING__HSTRING = Winapi.CommonTypes.PIIterable_1__IKeyValuePair_2__HSTRING__HSTRING;
  IIterator_1__HSTRING_Base = Winapi.CommonTypes.IIterator_1__HSTRING_Base;
  IIterator_1__HSTRING = Winapi.CommonTypes.IIterator_1__HSTRING;
  PIIterator_1__HSTRING = Winapi.CommonTypes.PIIterator_1__HSTRING;
  IIterator_1__IKeyValuePair_2__HSTRING__HSTRING_Base = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__HSTRING__HSTRING_Base;
  IIterator_1__IKeyValuePair_2__HSTRING__HSTRING = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__HSTRING__HSTRING;
  PIIterator_1__IKeyValuePair_2__HSTRING__HSTRING = Winapi.CommonTypes.PIIterator_1__IKeyValuePair_2__HSTRING__HSTRING;
  IIterator_1__TGuid_Base = Winapi.CommonTypes.IIterator_1__TGuid_Base;
  IIterator_1__TGuid = Winapi.CommonTypes.IIterator_1__TGuid;
  PIIterator_1__TGuid = Winapi.CommonTypes.PIIterator_1__TGuid;
  IKeyValuePair_2__HSTRING__HSTRING = Winapi.CommonTypes.IKeyValuePair_2__HSTRING__HSTRING;
  PIKeyValuePair_2__HSTRING__HSTRING = Winapi.CommonTypes.PIKeyValuePair_2__HSTRING__HSTRING;
  IMap_2__HSTRING__HSTRING = Winapi.CommonTypes.IMap_2__HSTRING__HSTRING;
  PIMap_2__HSTRING__HSTRING = Winapi.CommonTypes.PIMap_2__HSTRING__HSTRING;
  IMapView_2__HSTRING__HSTRING_Base = Winapi.CommonTypes.IMapView_2__HSTRING__HSTRING_Base;
  IMapView_2__HSTRING__HSTRING = Winapi.CommonTypes.IMapView_2__HSTRING__HSTRING;
  PIMapView_2__HSTRING__HSTRING = Winapi.CommonTypes.PIMapView_2__HSTRING__HSTRING;
  IReference_1__Byte = Winapi.CommonTypes.IReference_1__Byte;
  PIReference_1__Byte = Winapi.CommonTypes.PIReference_1__Byte;
  IReference_1__Cardinal = Winapi.CommonTypes.IReference_1__Cardinal;
  PIReference_1__Cardinal = Winapi.CommonTypes.PIReference_1__Cardinal;
  IReference_1__UInt64 = Winapi.CommonTypes.IReference_1__UInt64;
  PIReference_1__UInt64 = Winapi.CommonTypes.PIReference_1__UInt64;
  IUser = Winapi.CommonTypes.IUser;
  PIUser = Winapi.CommonTypes.PIUser;
  IUserChangedEventArgs = Winapi.CommonTypes.IUserChangedEventArgs;
  PIUserChangedEventArgs = Winapi.CommonTypes.PIUserChangedEventArgs;
  IVector_1__HSTRING_Base = Winapi.CommonTypes.IVector_1__HSTRING_Base;
  IVector_1__HSTRING = Winapi.CommonTypes.IVector_1__HSTRING;
  PIVector_1__HSTRING = Winapi.CommonTypes.PIVector_1__HSTRING;
  IVectorView_1__HSTRING = Winapi.CommonTypes.IVectorView_1__HSTRING;
  PIVectorView_1__HSTRING = Winapi.CommonTypes.PIVectorView_1__HSTRING;
  IVectorView_1__TGuid = Winapi.CommonTypes.IVectorView_1__TGuid;
  PIVectorView_1__TGuid = Winapi.CommonTypes.PIVectorView_1__TGuid;
  Power_BatteryStatus = Winapi.CommonTypes.Power_BatteryStatus;
  PPower_BatteryStatus = Winapi.CommonTypes.PPower_BatteryStatus;
  ProcessorArchitecture = Winapi.CommonTypes.ProcessorArchitecture;
  PProcessorArchitecture = Winapi.CommonTypes.PProcessorArchitecture;
  RemoteSystems_IRemoteSystem = Winapi.CommonTypes.RemoteSystems_IRemoteSystem;
  PRemoteSystems_IRemoteSystem = Winapi.CommonTypes.PRemoteSystems_IRemoteSystem;
  RemoteSystems_IRemoteSystemConnectionRequest = Winapi.CommonTypes.RemoteSystems_IRemoteSystemConnectionRequest;
  PRemoteSystems_IRemoteSystemConnectionRequest = Winapi.CommonTypes.PRemoteSystems_IRemoteSystemConnectionRequest;
  RemoteSystems_RemoteSystemStatus = Winapi.CommonTypes.RemoteSystems_RemoteSystemStatus;
  PRemoteSystems_RemoteSystemStatus = Winapi.CommonTypes.PRemoteSystems_RemoteSystemStatus;
  TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs_Delegate_Base;
  TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs;
  PTypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IDispatcherQueue__IDispatcherQueueShutdownStartingEventArgs;
  TypedEventHandler_2__IDispatcherQueue__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDispatcherQueue__IInspectable_Delegate_Base;
  TypedEventHandler_2__IDispatcherQueue__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IDispatcherQueue__IInspectable;
  PTypedEventHandler_2__IDispatcherQueue__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IDispatcherQueue__IInspectable;
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDispatcherQueueTimer__IInspectable_Delegate_Base;
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IDispatcherQueueTimer__IInspectable;
  PTypedEventHandler_2__IDispatcherQueueTimer__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IDispatcherQueueTimer__IInspectable;
  TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs_Delegate_Base;
  TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__Input_IGameController__IUserChangedEventArgs;
  PTypedEventHandler_2__Input_IGameController__IUserChangedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__Input_IGameController__IUserChangedEventArgs;
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable_Delegate_Base;
  TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable;
  PTypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__Search_IStorageQueryResultBase__IInspectable;
  UserAuthenticationStatus = Winapi.CommonTypes.UserAuthenticationStatus;
  PUserAuthenticationStatus = Winapi.CommonTypes.PUserAuthenticationStatus;
  UserPictureSize = Winapi.CommonTypes.UserPictureSize;
  PUserPictureSize = Winapi.CommonTypes.PUserPictureSize;
  UserType = Winapi.CommonTypes.UserType;
  PUserType = Winapi.CommonTypes.PUserType;
  VirtualKey = Winapi.CommonTypes.VirtualKey;
  PVirtualKey = Winapi.CommonTypes.PVirtualKey;
  VirtualKeyModifiers = Winapi.CommonTypes.VirtualKeyModifiers;
  PVirtualKeyModifiers = Winapi.CommonTypes.PVirtualKeyModifiers;

  // Forward declarations for interfaces

  // Windows.System.IProtocolForResultsOperation
  IProtocolForResultsOperation = interface;
  PIProtocolForResultsOperation = ^IProtocolForResultsOperation;

  // Windows.System.RemoteSystems.IRemoteSystemSessionDisconnectedEventArgs
  RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionDisconnectedEventArgs = ^RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSession,Windows.System.RemoteSystems.IRemoteSystemSessionDisconnectedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSession__RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSession__RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSession__RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipant
  RemoteSystems_IRemoteSystemSessionParticipant = interface;
  PRemoteSystems_IRemoteSystemSessionParticipant = ^RemoteSystems_IRemoteSystemSessionParticipant;

  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipantAddedEventArgs
  RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs = ^RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionParticipantAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipantRemovedEventArgs
  RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs = ^RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionParticipantRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher
  RemoteSystems_IRemoteSystemSessionParticipantWatcher = interface;
  PRemoteSystems_IRemoteSystemSessionParticipantWatcher = ^RemoteSystems_IRemoteSystemSessionParticipantWatcher;

  // Windows.System.RemoteSystems.IRemoteSystemSession
  RemoteSystems_IRemoteSystemSession = interface;
  PRemoteSystems_IRemoteSystemSession = ^RemoteSystems_IRemoteSystemSession;

  // Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionClosedEventArgs
  Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs = interface;
  PDiagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs = ^Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection,Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionClosedEventArgs>
  TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs = interface;
  PTypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs = ^TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs;

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

  // Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionRequestReceivedEventArgs
  Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs = interface;
  PDiagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs = ^Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection,Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionRequestReceivedEventArgs>
  TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs = interface;
  PTypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs = ^TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs;

  // Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection
  Diagnostics_DevicePortal_IDevicePortalConnection = interface;
  PDiagnostics_DevicePortal_IDevicePortalConnection = ^Diagnostics_DevicePortal_IDevicePortalConnection;

  // Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  IMapChangedEventArgs_1__HSTRING = interface;
  PIMapChangedEventArgs_1__HSTRING = ^IMapChangedEventArgs_1__HSTRING;

  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable = interface;
  PMapChangedEventHandler_2__HSTRING__IInspectable = ^MapChangedEventHandler_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IObservableMap`2<String,Object>
  IObservableMap_2__HSTRING__IInspectable = interface;
  PIObservableMap_2__HSTRING__IInspectable = ^IObservableMap_2__HSTRING__IInspectable;

  // Windows.System.Diagnostics.DevicePortal.IDevicePortalWebSocketConnection
  Diagnostics_DevicePortal_IDevicePortalWebSocketConnection = interface;
  PDiagnostics_DevicePortal_IDevicePortalWebSocketConnection = ^Diagnostics_DevicePortal_IDevicePortalWebSocketConnection;

  // Windows.System.Diagnostics.DevicePortal.IDevicePortalWebSocketConnectionRequestReceivedEventArgs
  Diagnostics_DevicePortal_IDevicePortalWebSocketConnectionRequestReceivedEventArgs = interface;
  PDiagnostics_DevicePortal_IDevicePortalWebSocketConnectionRequestReceivedEventArgs = ^Diagnostics_DevicePortal_IDevicePortalWebSocketConnectionRequestReceivedEventArgs;

  // Windows.System.Diagnostics.IDiagnosticActionResult
  Diagnostics_IDiagnosticActionResult = interface;
  PDiagnostics_IDiagnosticActionResult = ^Diagnostics_IDiagnosticActionResult;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = interface;
  PAsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = ^AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = ^AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = interface;
  PIAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = ^IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState;

  // Windows.System.Diagnostics.IDiagnosticInvoker
  Diagnostics_IDiagnosticInvoker = interface;
  PDiagnostics_IDiagnosticInvoker = ^Diagnostics_IDiagnosticInvoker;

  // Windows.System.Diagnostics.IDiagnosticInvoker2
  Diagnostics_IDiagnosticInvoker2 = interface;
  PDiagnostics_IDiagnosticInvoker2 = ^Diagnostics_IDiagnosticInvoker2;

  // Windows.System.Diagnostics.IProcessCpuUsageReport
  Diagnostics_IProcessCpuUsageReport = interface;
  PDiagnostics_IProcessCpuUsageReport = ^Diagnostics_IProcessCpuUsageReport;

  // Windows.System.Diagnostics.IProcessCpuUsage
  Diagnostics_IProcessCpuUsage = interface;
  PDiagnostics_IProcessCpuUsage = ^Diagnostics_IProcessCpuUsage;

  // Windows.System.Diagnostics.IProcessDiskUsageReport
  Diagnostics_IProcessDiskUsageReport = interface;
  PDiagnostics_IProcessDiskUsageReport = ^Diagnostics_IProcessDiskUsageReport;

  // Windows.System.Diagnostics.IProcessDiskUsage
  Diagnostics_IProcessDiskUsage = interface;
  PDiagnostics_IProcessDiskUsage = ^Diagnostics_IProcessDiskUsage;

  // Windows.System.Diagnostics.IProcessMemoryUsageReport
  Diagnostics_IProcessMemoryUsageReport = interface;
  PDiagnostics_IProcessMemoryUsageReport = ^Diagnostics_IProcessMemoryUsageReport;

  // Windows.System.Diagnostics.IProcessMemoryUsage
  Diagnostics_IProcessMemoryUsage = interface;
  PDiagnostics_IProcessMemoryUsage = ^Diagnostics_IProcessMemoryUsage;

  // Windows.System.Diagnostics.IProcessDiagnosticInfo
  Diagnostics_IProcessDiagnosticInfo = interface;
  PDiagnostics_IProcessDiagnosticInfo = ^Diagnostics_IProcessDiagnosticInfo;

  // Windows.System.IAppDiagnosticInfo
  IAppDiagnosticInfo = interface;
  PIAppDiagnosticInfo = ^IAppDiagnosticInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppDiagnosticInfo>
  IIterator_1__IAppDiagnosticInfo = interface;
  PIIterator_1__IAppDiagnosticInfo = ^IIterator_1__IAppDiagnosticInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppDiagnosticInfo>
  IIterable_1__IAppDiagnosticInfo = interface;
  PIIterable_1__IAppDiagnosticInfo = ^IIterable_1__IAppDiagnosticInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppDiagnosticInfo>
  IVectorView_1__IAppDiagnosticInfo = interface;
  PIVectorView_1__IAppDiagnosticInfo = ^IVectorView_1__IAppDiagnosticInfo;

  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>
  IVector_1__IAppDiagnosticInfo = interface;
  PIVector_1__IAppDiagnosticInfo = ^IVector_1__IAppDiagnosticInfo;

  // Windows.System.Diagnostics.IProcessDiagnosticInfo2
  Diagnostics_IProcessDiagnosticInfo2 = interface;
  PDiagnostics_IProcessDiagnosticInfo2 = ^Diagnostics_IProcessDiagnosticInfo2;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IIterator_1__Diagnostics_IProcessDiagnosticInfo = interface;
  PIIterator_1__Diagnostics_IProcessDiagnosticInfo = ^IIterator_1__Diagnostics_IProcessDiagnosticInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IIterable_1__Diagnostics_IProcessDiagnosticInfo = interface;
  PIIterable_1__Diagnostics_IProcessDiagnosticInfo = ^IIterable_1__Diagnostics_IProcessDiagnosticInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IVectorView_1__Diagnostics_IProcessDiagnosticInfo = interface;
  PIVectorView_1__Diagnostics_IProcessDiagnosticInfo = ^IVectorView_1__Diagnostics_IProcessDiagnosticInfo;

  // Windows.System.Diagnostics.IProcessDiagnosticInfoStatics
  Diagnostics_IProcessDiagnosticInfoStatics = interface;
  PDiagnostics_IProcessDiagnosticInfoStatics = ^Diagnostics_IProcessDiagnosticInfoStatics;

  // Windows.System.Diagnostics.IProcessDiagnosticInfoStatics2
  Diagnostics_IProcessDiagnosticInfoStatics2 = interface;
  PDiagnostics_IProcessDiagnosticInfoStatics2 = ^Diagnostics_IProcessDiagnosticInfoStatics2;

  // Windows.System.Diagnostics.ISystemCpuUsageReport
  Diagnostics_ISystemCpuUsageReport = interface;
  PDiagnostics_ISystemCpuUsageReport = ^Diagnostics_ISystemCpuUsageReport;

  // Windows.System.Diagnostics.ISystemCpuUsage
  Diagnostics_ISystemCpuUsage = interface;
  PDiagnostics_ISystemCpuUsage = ^Diagnostics_ISystemCpuUsage;

  // Windows.System.Diagnostics.ISystemMemoryUsageReport
  Diagnostics_ISystemMemoryUsageReport = interface;
  PDiagnostics_ISystemMemoryUsageReport = ^Diagnostics_ISystemMemoryUsageReport;

  // Windows.System.Diagnostics.ISystemMemoryUsage
  Diagnostics_ISystemMemoryUsage = interface;
  PDiagnostics_ISystemMemoryUsage = ^Diagnostics_ISystemMemoryUsage;

  // Windows.System.Diagnostics.ISystemDiagnosticInfo
  Diagnostics_ISystemDiagnosticInfo = interface;
  PDiagnostics_ISystemDiagnosticInfo = ^Diagnostics_ISystemDiagnosticInfo;

  // Windows.System.Diagnostics.ISystemDiagnosticInfoStatics
  Diagnostics_ISystemDiagnosticInfoStatics = interface;
  PDiagnostics_ISystemDiagnosticInfoStatics = ^Diagnostics_ISystemDiagnosticInfoStatics;

  // Windows.System.Diagnostics.Telemetry.IPlatformTelemetryRegistrationResult
  Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult = interface;
  PDiagnostics_Telemetry_IPlatformTelemetryRegistrationResult = ^Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult;

  // Windows.System.Diagnostics.Telemetry.IPlatformTelemetryRegistrationSettings
  Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings = interface;
  PDiagnostics_Telemetry_IPlatformTelemetryRegistrationSettings = ^Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings;

  // Windows.System.Diagnostics.Telemetry.IPlatformTelemetryClientStatics
  Diagnostics_Telemetry_IPlatformTelemetryClientStatics = interface;
  PDiagnostics_Telemetry_IPlatformTelemetryClientStatics = ^Diagnostics_Telemetry_IPlatformTelemetryClientStatics;

  // Windows.Foundation.Collections.IIterable`1<Guid>
  IIterable_1__TGuid = interface;
  PIIterable_1__TGuid = ^IIterable_1__TGuid;

  // Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceRuntimeInfo
  Diagnostics_TraceReporting_IPlatformDiagnosticTraceRuntimeInfo = interface;
  PDiagnostics_TraceReporting_IPlatformDiagnosticTraceRuntimeInfo = ^Diagnostics_TraceReporting_IPlatformDiagnosticTraceRuntimeInfo;

  // Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo
  Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface;
  PDiagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = ^Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IIterator_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface;
  PIIterator_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = ^IIterator_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IIterable_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface;
  PIIterable_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = ^IIterable_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IVectorView_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface;
  PIVectorView_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = ^IVectorView_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo;

  // Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticActionsStatics
  Diagnostics_TraceReporting_IPlatformDiagnosticActionsStatics = interface;
  PDiagnostics_TraceReporting_IPlatformDiagnosticActionsStatics = ^Diagnostics_TraceReporting_IPlatformDiagnosticActionsStatics;

  // Windows.System.Display.IDisplayRequest
  Display_IDisplayRequest = interface;
  PDisplay_IDisplayRequest = ^Display_IDisplayRequest;

  // Windows.System.IAppResourceGroupBackgroundTaskReport
  IAppResourceGroupBackgroundTaskReport = interface;
  PIAppResourceGroupBackgroundTaskReport = ^IAppResourceGroupBackgroundTaskReport;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IIterator_1__IAppResourceGroupBackgroundTaskReport = interface;
  PIIterator_1__IAppResourceGroupBackgroundTaskReport = ^IIterator_1__IAppResourceGroupBackgroundTaskReport;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IIterable_1__IAppResourceGroupBackgroundTaskReport = interface;
  PIIterable_1__IAppResourceGroupBackgroundTaskReport = ^IIterable_1__IAppResourceGroupBackgroundTaskReport;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IVectorView_1__IAppResourceGroupBackgroundTaskReport = interface;
  PIVectorView_1__IAppResourceGroupBackgroundTaskReport = ^IVectorView_1__IAppResourceGroupBackgroundTaskReport;

  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IVector_1__IAppResourceGroupBackgroundTaskReport = interface;
  PIVector_1__IAppResourceGroupBackgroundTaskReport = ^IVector_1__IAppResourceGroupBackgroundTaskReport;

  // Windows.System.IAppResourceGroupMemoryReport
  IAppResourceGroupMemoryReport = interface;
  PIAppResourceGroupMemoryReport = ^IAppResourceGroupMemoryReport;

  // Windows.Foundation.Collections.IVector`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IVector_1__Diagnostics_IProcessDiagnosticInfo = interface;
  PIVector_1__Diagnostics_IProcessDiagnosticInfo = ^IVector_1__Diagnostics_IProcessDiagnosticInfo;

  // Windows.System.IAppResourceGroupStateReport
  IAppResourceGroupStateReport = interface;
  PIAppResourceGroupStateReport = ^IAppResourceGroupStateReport;

  // Windows.System.IAppResourceGroupInfo
  IAppResourceGroupInfo = interface;
  PIAppResourceGroupInfo = ^IAppResourceGroupInfo;

  // Windows.System.IAppActivationResult
  IAppActivationResult = interface;
  PIAppActivationResult = ^IAppActivationResult;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppResourceGroupInfo>
  IIterator_1__IAppResourceGroupInfo = interface;
  PIIterator_1__IAppResourceGroupInfo = ^IIterator_1__IAppResourceGroupInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppResourceGroupInfo>
  IIterable_1__IAppResourceGroupInfo = interface;
  PIIterable_1__IAppResourceGroupInfo = ^IIterable_1__IAppResourceGroupInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppResourceGroupInfo>
  IVectorView_1__IAppResourceGroupInfo = interface;
  PIVectorView_1__IAppResourceGroupInfo = ^IVectorView_1__IAppResourceGroupInfo;

  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppResourceGroupInfo>
  IVector_1__IAppResourceGroupInfo = interface;
  PIVector_1__IAppResourceGroupInfo = ^IVector_1__IAppResourceGroupInfo;

  // Windows.System.IAppResourceGroupInfoWatcherEventArgs
  IAppResourceGroupInfoWatcherEventArgs = interface;
  PIAppResourceGroupInfoWatcherEventArgs = ^IAppResourceGroupInfoWatcherEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Windows.System.IAppResourceGroupInfoWatcherEventArgs>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs = interface;
  PTypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs = ^TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Object>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable = interface;
  PTypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable = ^TypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable;

  // Windows.System.IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs
  IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs = interface;
  PIAppResourceGroupInfoWatcherExecutionStateChangedEventArgs = ^IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Windows.System.IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs = interface;
  PTypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs = ^TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs;

  // Windows.System.IAppResourceGroupInfoWatcher
  IAppResourceGroupInfoWatcher = interface;
  PIAppResourceGroupInfoWatcher = ^IAppResourceGroupInfoWatcher;

  // Windows.System.IAppDiagnosticInfo2
  IAppDiagnosticInfo2 = interface;
  PIAppDiagnosticInfo2 = ^IAppDiagnosticInfo2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IAppActivationResult>
  AsyncOperationCompletedHandler_1__IAppActivationResult = interface;
  PAsyncOperationCompletedHandler_1__IAppActivationResult = ^AsyncOperationCompletedHandler_1__IAppActivationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.IAppActivationResult>
  IAsyncOperation_1__IAppActivationResult = interface;
  PIAsyncOperation_1__IAppActivationResult = ^IAsyncOperation_1__IAppActivationResult;

  // Windows.System.IAppDiagnosticInfo3
  IAppDiagnosticInfo3 = interface;
  PIAppDiagnosticInfo3 = ^IAppDiagnosticInfo3;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>>
  AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo = interface;
  PAsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo = ^AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>>
  IAsyncOperation_1__IVector_1__IAppDiagnosticInfo = interface;
  PIAsyncOperation_1__IVector_1__IAppDiagnosticInfo = ^IAsyncOperation_1__IVector_1__IAppDiagnosticInfo;

  // Windows.System.IAppDiagnosticInfoStatics
  IAppDiagnosticInfoStatics = interface;
  PIAppDiagnosticInfoStatics = ^IAppDiagnosticInfoStatics;

  // Windows.System.IAppDiagnosticInfoWatcherEventArgs
  IAppDiagnosticInfoWatcherEventArgs = interface;
  PIAppDiagnosticInfoWatcherEventArgs = ^IAppDiagnosticInfoWatcherEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppDiagnosticInfoWatcher,Windows.System.IAppDiagnosticInfoWatcherEventArgs>
  TypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs = interface;
  PTypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs = ^TypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppDiagnosticInfoWatcher,Object>
  TypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable = interface;
  PTypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable = ^TypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable;

  // Windows.System.IAppDiagnosticInfoWatcher
  IAppDiagnosticInfoWatcher = interface;
  PIAppDiagnosticInfoWatcher = ^IAppDiagnosticInfoWatcher;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.DiagnosticAccessStatus>
  AsyncOperationCompletedHandler_1__DiagnosticAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__DiagnosticAccessStatus = ^AsyncOperationCompletedHandler_1__DiagnosticAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.DiagnosticAccessStatus>
  IAsyncOperation_1__DiagnosticAccessStatus = interface;
  PIAsyncOperation_1__DiagnosticAccessStatus = ^IAsyncOperation_1__DiagnosticAccessStatus;

  // Windows.System.IAppDiagnosticInfoStatics2
  IAppDiagnosticInfoStatics2 = interface;
  PIAppDiagnosticInfoStatics2 = ^IAppDiagnosticInfoStatics2;

  // Windows.System.IAppExecutionStateChangeResult
  IAppExecutionStateChangeResult = interface;
  PIAppExecutionStateChangeResult = ^IAppExecutionStateChangeResult;

  // Windows.System.IAppMemoryReport
  IAppMemoryReport = interface;
  PIAppMemoryReport = ^IAppMemoryReport;

  // Windows.System.IAppMemoryReport2
  IAppMemoryReport2 = interface;
  PIAppMemoryReport2 = ^IAppMemoryReport2;

  // Windows.System.IAppMemoryUsageLimitChangingEventArgs
  IAppMemoryUsageLimitChangingEventArgs = interface;
  PIAppMemoryUsageLimitChangingEventArgs = ^IAppMemoryUsageLimitChangingEventArgs;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IAppExecutionStateChangeResult>
  AsyncOperationCompletedHandler_1__IAppExecutionStateChangeResult = interface;
  PAsyncOperationCompletedHandler_1__IAppExecutionStateChangeResult = ^AsyncOperationCompletedHandler_1__IAppExecutionStateChangeResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.IAppExecutionStateChangeResult>
  IAsyncOperation_1__IAppExecutionStateChangeResult = interface;
  PIAsyncOperation_1__IAppExecutionStateChangeResult = ^IAsyncOperation_1__IAppExecutionStateChangeResult;

  // Windows.System.IAppResourceGroupInfo2
  IAppResourceGroupInfo2 = interface;
  PIAppResourceGroupInfo2 = ^IAppResourceGroupInfo2;

  // Windows.System.IAppUriHandlerHost
  IAppUriHandlerHost = interface;
  PIAppUriHandlerHost = ^IAppUriHandlerHost;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppUriHandlerHost>
  IIterator_1__IAppUriHandlerHost = interface;
  PIIterator_1__IAppUriHandlerHost = ^IIterator_1__IAppUriHandlerHost;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppUriHandlerHost>
  IIterable_1__IAppUriHandlerHost = interface;
  PIIterable_1__IAppUriHandlerHost = ^IIterable_1__IAppUriHandlerHost;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppUriHandlerHost>
  IVectorView_1__IAppUriHandlerHost = interface;
  PIVectorView_1__IAppUriHandlerHost = ^IVectorView_1__IAppUriHandlerHost;

  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppUriHandlerHost>
  IVector_1__IAppUriHandlerHost = interface;
  PIVector_1__IAppUriHandlerHost = ^IVector_1__IAppUriHandlerHost;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppUriHandlerHost>>
  AsyncOperationCompletedHandler_1__IVector_1__IAppUriHandlerHost = interface;
  PAsyncOperationCompletedHandler_1__IVector_1__IAppUriHandlerHost = ^AsyncOperationCompletedHandler_1__IVector_1__IAppUriHandlerHost;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppUriHandlerHost>>
  IAsyncOperation_1__IVector_1__IAppUriHandlerHost = interface;
  PIAsyncOperation_1__IVector_1__IAppUriHandlerHost = ^IAsyncOperation_1__IVector_1__IAppUriHandlerHost;

  // Windows.System.IAppUriHandlerRegistration
  IAppUriHandlerRegistration = interface;
  PIAppUriHandlerRegistration = ^IAppUriHandlerRegistration;

  // Windows.System.IAppUriHandlerRegistrationManager
  IAppUriHandlerRegistrationManager = interface;
  PIAppUriHandlerRegistrationManager = ^IAppUriHandlerRegistrationManager;

  // Windows.System.IDateTimeSettingsStatics
  IDateTimeSettingsStatics = interface;
  PIDateTimeSettingsStatics = ^IDateTimeSettingsStatics;

  // Windows.System.IDispatcherQueue2
  IDispatcherQueue2 = interface;
  PIDispatcherQueue2 = ^IDispatcherQueue2;

  // Windows.System.IDispatcherQueueController
  IDispatcherQueueController = interface;
  PIDispatcherQueueController = ^IDispatcherQueueController;

  // Windows.System.IDispatcherQueueControllerStatics
  IDispatcherQueueControllerStatics = interface;
  PIDispatcherQueueControllerStatics = ^IDispatcherQueueControllerStatics;

  // Windows.System.IDispatcherQueueStatics
  IDispatcherQueueStatics = interface;
  PIDispatcherQueueStatics = ^IDispatcherQueueStatics;

  // Windows.System.IFolderLauncherOptions
  IFolderLauncherOptions = interface;
  PIFolderLauncherOptions = ^IFolderLauncherOptions;

  // Windows.System.IKnownUserPropertiesStatics
  IKnownUserPropertiesStatics = interface;
  PIKnownUserPropertiesStatics = ^IKnownUserPropertiesStatics;

  // Windows.System.ILaunchUriResult
  ILaunchUriResult = interface;
  PILaunchUriResult = ^ILaunchUriResult;

  // Windows.System.ILauncherUIOptions
  ILauncherUIOptions = interface;
  PILauncherUIOptions = ^ILauncherUIOptions;

  // Windows.System.ILauncherOptions
  ILauncherOptions = interface;
  PILauncherOptions = ^ILauncherOptions;

  // Windows.System.ILauncherOptions2
  ILauncherOptions2 = interface;
  PILauncherOptions2 = ^ILauncherOptions2;

  // Windows.System.ILauncherOptions3
  ILauncherOptions3 = interface;
  PILauncherOptions3 = ^ILauncherOptions3;

  // Windows.System.ILauncherOptions4
  ILauncherOptions4 = interface;
  PILauncherOptions4 = ^ILauncherOptions4;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.ILaunchUriResult>
  AsyncOperationCompletedHandler_1__ILaunchUriResult = interface;
  PAsyncOperationCompletedHandler_1__ILaunchUriResult = ^AsyncOperationCompletedHandler_1__ILaunchUriResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.ILaunchUriResult>
  IAsyncOperation_1__ILaunchUriResult = interface;
  PIAsyncOperation_1__ILaunchUriResult = ^IAsyncOperation_1__ILaunchUriResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.LaunchQuerySupportStatus>
  AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus = interface;
  PAsyncOperationCompletedHandler_1__LaunchQuerySupportStatus = ^AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.LaunchQuerySupportStatus>
  IAsyncOperation_1__LaunchQuerySupportStatus = interface;
  PIAsyncOperation_1__LaunchQuerySupportStatus = ^IAsyncOperation_1__LaunchQuerySupportStatus;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.LaunchUriStatus>
  AsyncOperationCompletedHandler_1__LaunchUriStatus = interface;
  PAsyncOperationCompletedHandler_1__LaunchUriStatus = ^AsyncOperationCompletedHandler_1__LaunchUriStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.LaunchUriStatus>
  IAsyncOperation_1__LaunchUriStatus = interface;
  PIAsyncOperation_1__LaunchUriStatus = ^IAsyncOperation_1__LaunchUriStatus;

  // Windows.System.ILauncherViewOptions
  ILauncherViewOptions = interface;
  PILauncherViewOptions = ^ILauncherViewOptions;

  // Windows.Foundation.EventHandler`1<Windows.System.IAppMemoryUsageLimitChangingEventArgs>
  EventHandler_1__IAppMemoryUsageLimitChangingEventArgs = interface;
  PEventHandler_1__IAppMemoryUsageLimitChangingEventArgs = ^EventHandler_1__IAppMemoryUsageLimitChangingEventArgs;

  // Windows.System.IProcessMemoryReport
  IProcessMemoryReport = interface;
  PIProcessMemoryReport = ^IProcessMemoryReport;

  // Windows.System.IProcessLauncherOptions
  IProcessLauncherOptions = interface;
  PIProcessLauncherOptions = ^IProcessLauncherOptions;

  // Windows.System.IProcessLauncherResult
  IProcessLauncherResult = interface;
  PIProcessLauncherResult = ^IProcessLauncherResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IProcessLauncherResult>
  AsyncOperationCompletedHandler_1__IProcessLauncherResult = interface;
  PAsyncOperationCompletedHandler_1__IProcessLauncherResult = ^AsyncOperationCompletedHandler_1__IProcessLauncherResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.IProcessLauncherResult>
  IAsyncOperation_1__IProcessLauncherResult = interface;
  PIAsyncOperation_1__IProcessLauncherResult = ^IAsyncOperation_1__IProcessLauncherResult;

  // Windows.System.IProcessLauncherStatics
  IProcessLauncherStatics = interface;
  PIProcessLauncherStatics = ^IProcessLauncherStatics;

  // Windows.System.IRemoteLauncherOptions
  IRemoteLauncherOptions = interface;
  PIRemoteLauncherOptions = ^IRemoteLauncherOptions;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteLaunchUriStatus>
  AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus = interface;
  PAsyncOperationCompletedHandler_1__RemoteLaunchUriStatus = ^AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteLaunchUriStatus>
  IAsyncOperation_1__RemoteLaunchUriStatus = interface;
  PIAsyncOperation_1__RemoteLaunchUriStatus = ^IAsyncOperation_1__RemoteLaunchUriStatus;

  // Windows.System.IShutdownManagerStatics
  IShutdownManagerStatics = interface;
  PIShutdownManagerStatics = ^IShutdownManagerStatics;

  // Windows.System.IShutdownManagerStatics2
  IShutdownManagerStatics2 = interface;
  PIShutdownManagerStatics2 = ^IShutdownManagerStatics2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.AutoUpdateTimeZoneStatus>
  AsyncOperationCompletedHandler_1__AutoUpdateTimeZoneStatus = interface;
  PAsyncOperationCompletedHandler_1__AutoUpdateTimeZoneStatus = ^AsyncOperationCompletedHandler_1__AutoUpdateTimeZoneStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.AutoUpdateTimeZoneStatus>
  IAsyncOperation_1__AutoUpdateTimeZoneStatus = interface;
  PIAsyncOperation_1__AutoUpdateTimeZoneStatus = ^IAsyncOperation_1__AutoUpdateTimeZoneStatus;

  // Windows.System.IUserAuthenticationStatusChangeDeferral
  IUserAuthenticationStatusChangeDeferral = interface;
  PIUserAuthenticationStatusChangeDeferral = ^IUserAuthenticationStatusChangeDeferral;

  // Windows.System.IUserAuthenticationStatusChangingEventArgs
  IUserAuthenticationStatusChangingEventArgs = interface;
  PIUserAuthenticationStatusChangingEventArgs = ^IUserAuthenticationStatusChangingEventArgs;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.UserWatcherUpdateKind>
  IIterator_1__UserWatcherUpdateKind = interface;
  PIIterator_1__UserWatcherUpdateKind = ^IIterator_1__UserWatcherUpdateKind;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.UserWatcherUpdateKind>
  IIterable_1__UserWatcherUpdateKind = interface;
  PIIterable_1__UserWatcherUpdateKind = ^IIterable_1__UserWatcherUpdateKind;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.UserWatcherUpdateKind>
  IVectorView_1__UserWatcherUpdateKind = interface;
  PIVectorView_1__UserWatcherUpdateKind = ^IVectorView_1__UserWatcherUpdateKind;

  // Windows.System.IUserChangedEventArgs2
  IUserChangedEventArgs2 = interface;
  PIUserChangedEventArgs2 = ^IUserChangedEventArgs2;

  // Windows.System.IUserDeviceAssociationChangedEventArgs
  IUserDeviceAssociationChangedEventArgs = interface;
  PIUserDeviceAssociationChangedEventArgs = ^IUserDeviceAssociationChangedEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.System.IUserDeviceAssociationChangedEventArgs>
  EventHandler_1__IUserDeviceAssociationChangedEventArgs = interface;
  PEventHandler_1__IUserDeviceAssociationChangedEventArgs = ^EventHandler_1__IUserDeviceAssociationChangedEventArgs;

  // Windows.System.IUserDeviceAssociationStatics
  IUserDeviceAssociationStatics = interface;
  PIUserDeviceAssociationStatics = ^IUserDeviceAssociationStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IUser>
  AsyncOperationCompletedHandler_1__IUser = interface;
  PAsyncOperationCompletedHandler_1__IUser = ^AsyncOperationCompletedHandler_1__IUser;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.IUser>
  IAsyncOperation_1__IUser = interface;
  PIAsyncOperation_1__IUser = ^IAsyncOperation_1__IUser;

  // Windows.System.IUserPicker
  IUserPicker = interface;
  PIUserPicker = ^IUserPicker;

  // Windows.System.IUserPickerStatics
  IUserPickerStatics = interface;
  PIUserPickerStatics = ^IUserPickerStatics;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Windows.System.IUserChangedEventArgs>
  TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs = interface;
  PTypedEventHandler_2__IUserWatcher__IUserChangedEventArgs = ^TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Windows.System.IUserAuthenticationStatusChangingEventArgs>
  TypedEventHandler_2__IUserWatcher__IUserAuthenticationStatusChangingEventArgs = interface;
  PTypedEventHandler_2__IUserWatcher__IUserAuthenticationStatusChangingEventArgs = ^TypedEventHandler_2__IUserWatcher__IUserAuthenticationStatusChangingEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Object>
  TypedEventHandler_2__IUserWatcher__IInspectable = interface;
  PTypedEventHandler_2__IUserWatcher__IInspectable = ^TypedEventHandler_2__IUserWatcher__IInspectable;

  // Windows.System.IUserWatcher
  IUserWatcher = interface;
  PIUserWatcher = ^IUserWatcher;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.IUser>
  IIterator_1__IUser = interface;
  PIIterator_1__IUser = ^IIterator_1__IUser;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.IUser>
  IIterable_1__IUser = interface;
  PIIterable_1__IUser = ^IIterable_1__IUser;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>
  IVectorView_1__IUser = interface;
  PIVectorView_1__IUser = ^IVectorView_1__IUser;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IUser = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IUser = ^AsyncOperationCompletedHandler_1__IVectorView_1__IUser;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>>
  IAsyncOperation_1__IVectorView_1__IUser = interface;
  PIAsyncOperation_1__IVectorView_1__IUser = ^IAsyncOperation_1__IVectorView_1__IUser;

  // Windows.System.IUserStatics
  IUserStatics = interface;
  PIUserStatics = ^IUserStatics;

  // Windows.System.Inventory.IInstalledDesktopApp
  Inventory_IInstalledDesktopApp = interface;
  PInventory_IInstalledDesktopApp = ^Inventory_IInstalledDesktopApp;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.Inventory.IInstalledDesktopApp>
  IIterator_1__Inventory_IInstalledDesktopApp = interface;
  PIIterator_1__Inventory_IInstalledDesktopApp = ^IIterator_1__Inventory_IInstalledDesktopApp;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.Inventory.IInstalledDesktopApp>
  IIterable_1__Inventory_IInstalledDesktopApp = interface;
  PIIterable_1__Inventory_IInstalledDesktopApp = ^IIterable_1__Inventory_IInstalledDesktopApp;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Inventory.IInstalledDesktopApp>
  IVectorView_1__Inventory_IInstalledDesktopApp = interface;
  PIVectorView_1__Inventory_IInstalledDesktopApp = ^IVectorView_1__Inventory_IInstalledDesktopApp;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.Inventory.IInstalledDesktopApp>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Inventory_IInstalledDesktopApp = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__Inventory_IInstalledDesktopApp = ^AsyncOperationCompletedHandler_1__IVectorView_1__Inventory_IInstalledDesktopApp;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.Inventory.IInstalledDesktopApp>>
  IAsyncOperation_1__IVectorView_1__Inventory_IInstalledDesktopApp = interface;
  PIAsyncOperation_1__IVectorView_1__Inventory_IInstalledDesktopApp = ^IAsyncOperation_1__IVectorView_1__Inventory_IInstalledDesktopApp;

  // Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReading
  Preview_ITwoPanelHingedDevicePosturePreviewReading = interface;
  PPreview_ITwoPanelHingedDevicePosturePreviewReading = ^Preview_ITwoPanelHingedDevicePosturePreviewReading;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReading>
  AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreviewReading = interface;
  PAsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreviewReading = ^AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreviewReading;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReading>
  IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreviewReading = interface;
  PIAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreviewReading = ^IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreviewReading;

  // Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs
  Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs = interface;
  PPreview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs = ^Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.Preview.ITwoPanelHingedDevicePosturePreview,Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs>
  TypedEventHandler_2__Preview_ITwoPanelHingedDevicePosturePreview__Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs = interface;
  PTypedEventHandler_2__Preview_ITwoPanelHingedDevicePosturePreview__Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs = ^TypedEventHandler_2__Preview_ITwoPanelHingedDevicePosturePreview__Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs;

  // Windows.System.Preview.ITwoPanelHingedDevicePosturePreview
  Preview_ITwoPanelHingedDevicePosturePreview = interface;
  PPreview_ITwoPanelHingedDevicePosturePreview = ^Preview_ITwoPanelHingedDevicePosturePreview;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreview>
  AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreview = interface;
  PAsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreview = ^AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreview;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreview>
  IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreview = interface;
  PIAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreview = ^IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreview;

  // Windows.System.Profile.IAnalyticsVersionInfo
  Profile_IAnalyticsVersionInfo = interface;
  PProfile_IAnalyticsVersionInfo = ^Profile_IAnalyticsVersionInfo;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,String>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__HSTRING = interface;
  PAsyncOperationCompletedHandler_1__IMapView_2__HSTRING__HSTRING = ^AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__HSTRING;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,String>>
  IAsyncOperation_1__IMapView_2__HSTRING__HSTRING = interface;
  PIAsyncOperation_1__IMapView_2__HSTRING__HSTRING = ^IAsyncOperation_1__IMapView_2__HSTRING__HSTRING;

  // Windows.System.Profile.IUnsupportedAppRequirement
  Profile_IUnsupportedAppRequirement = interface;
  PProfile_IUnsupportedAppRequirement = ^Profile_IUnsupportedAppRequirement;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.Profile.IUnsupportedAppRequirement>
  IIterator_1__Profile_IUnsupportedAppRequirement = interface;
  PIIterator_1__Profile_IUnsupportedAppRequirement = ^IIterator_1__Profile_IUnsupportedAppRequirement;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.Profile.IUnsupportedAppRequirement>
  IIterable_1__Profile_IUnsupportedAppRequirement = interface;
  PIIterable_1__Profile_IUnsupportedAppRequirement = ^IIterable_1__Profile_IUnsupportedAppRequirement;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Profile.IUnsupportedAppRequirement>
  IVectorView_1__Profile_IUnsupportedAppRequirement = interface;
  PIVectorView_1__Profile_IUnsupportedAppRequirement = ^IVectorView_1__Profile_IUnsupportedAppRequirement;

  // Windows.System.Profile.IHardwareToken
  Profile_IHardwareToken = interface;
  PProfile_IHardwareToken = ^Profile_IHardwareToken;

  // Windows.System.Profile.IPlatformDiagnosticsAndUsageDataSettingsStatics
  Profile_IPlatformDiagnosticsAndUsageDataSettingsStatics = interface;
  PProfile_IPlatformDiagnosticsAndUsageDataSettingsStatics = ^Profile_IPlatformDiagnosticsAndUsageDataSettingsStatics;

  // Windows.System.Profile.ISharedModeSettingsStatics
  Profile_ISharedModeSettingsStatics = interface;
  PProfile_ISharedModeSettingsStatics = ^Profile_ISharedModeSettingsStatics;

  // Windows.System.Profile.ISharedModeSettingsStatics2
  Profile_ISharedModeSettingsStatics2 = interface;
  PProfile_ISharedModeSettingsStatics2 = ^Profile_ISharedModeSettingsStatics2;

  // Windows.System.Profile.ISystemIdentificationInfo
  Profile_ISystemIdentificationInfo = interface;
  PProfile_ISystemIdentificationInfo = ^Profile_ISystemIdentificationInfo;

  // Windows.System.Profile.ISystemSetupInfoStatics
  Profile_ISystemSetupInfoStatics = interface;
  PProfile_ISystemSetupInfoStatics = ^Profile_ISystemSetupInfoStatics;

  // Windows.System.Profile.SystemManufacturers.IOemSupportInfo
  Profile_SystemManufacturers_IOemSupportInfo = interface;
  PProfile_SystemManufacturers_IOemSupportInfo = ^Profile_SystemManufacturers_IOemSupportInfo;

  // Windows.System.Profile.SystemManufacturers.ISystemSupportDeviceInfo
  Profile_SystemManufacturers_ISystemSupportDeviceInfo = interface;
  PProfile_SystemManufacturers_ISystemSupportDeviceInfo = ^Profile_SystemManufacturers_ISystemSupportDeviceInfo;

  // Windows.System.Profile.SystemManufacturers.ISystemSupportInfoStatics
  Profile_SystemManufacturers_ISystemSupportInfoStatics = interface;
  PProfile_SystemManufacturers_ISystemSupportInfoStatics = ^Profile_SystemManufacturers_ISystemSupportInfoStatics;

  // Windows.System.Profile.SystemManufacturers.ISystemSupportInfoStatics2
  Profile_SystemManufacturers_ISystemSupportInfoStatics2 = interface;
  PProfile_SystemManufacturers_ISystemSupportInfoStatics2 = ^Profile_SystemManufacturers_ISystemSupportInfoStatics2;

  // Windows.System.RemoteSystems.IRemoteSystem2
  RemoteSystems_IRemoteSystem2 = interface;
  PRemoteSystems_IRemoteSystem2 = ^RemoteSystems_IRemoteSystem2;

  // Windows.System.RemoteSystems.IRemoteSystem3
  RemoteSystems_IRemoteSystem3 = interface;
  PRemoteSystems_IRemoteSystem3 = ^RemoteSystems_IRemoteSystem3;

  // Windows.System.RemoteSystems.IRemoteSystem4
  RemoteSystems_IRemoteSystem4 = interface;
  PRemoteSystems_IRemoteSystem4 = ^RemoteSystems_IRemoteSystem4;

  // Windows.System.RemoteSystems.IRemoteSystemApp
  RemoteSystems_IRemoteSystemApp = interface;
  PRemoteSystems_IRemoteSystemApp = ^RemoteSystems_IRemoteSystemApp;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemApp>
  IIterator_1__RemoteSystems_IRemoteSystemApp = interface;
  PIIterator_1__RemoteSystems_IRemoteSystemApp = ^IIterator_1__RemoteSystems_IRemoteSystemApp;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemApp>
  IIterable_1__RemoteSystems_IRemoteSystemApp = interface;
  PIIterable_1__RemoteSystems_IRemoteSystemApp = ^IIterable_1__RemoteSystems_IRemoteSystemApp;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.RemoteSystems.IRemoteSystemApp>
  IVectorView_1__RemoteSystems_IRemoteSystemApp = interface;
  PIVectorView_1__RemoteSystems_IRemoteSystemApp = ^IVectorView_1__RemoteSystems_IRemoteSystemApp;

  // Windows.System.RemoteSystems.IRemoteSystem5
  RemoteSystems_IRemoteSystem5 = interface;
  PRemoteSystems_IRemoteSystem5 = ^RemoteSystems_IRemoteSystem5;

  // Windows.System.RemoteSystems.IRemoteSystem6
  RemoteSystems_IRemoteSystem6 = interface;
  PRemoteSystems_IRemoteSystem6 = ^RemoteSystems_IRemoteSystem6;

  // Windows.System.RemoteSystems.IRemoteSystemAddedEventArgs
  RemoteSystems_IRemoteSystemAddedEventArgs = interface;
  PRemoteSystems_IRemoteSystemAddedEventArgs = ^RemoteSystems_IRemoteSystemAddedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemApp2
  RemoteSystems_IRemoteSystemApp2 = interface;
  PRemoteSystems_IRemoteSystemApp2 = ^RemoteSystems_IRemoteSystemApp2;

  // Windows.System.RemoteSystems.IRemoteSystemAppRegistration
  RemoteSystems_IRemoteSystemAppRegistration = interface;
  PRemoteSystems_IRemoteSystemAppRegistration = ^RemoteSystems_IRemoteSystemAppRegistration;

  // Windows.System.RemoteSystems.IRemoteSystemAuthorizationKindFilter
  RemoteSystems_IRemoteSystemAuthorizationKindFilter = interface;
  PRemoteSystems_IRemoteSystemAuthorizationKindFilter = ^RemoteSystems_IRemoteSystemAuthorizationKindFilter;

  // Windows.System.RemoteSystems.IRemoteSystemConnectionInfo
  RemoteSystems_IRemoteSystemConnectionInfo = interface;
  PRemoteSystems_IRemoteSystemConnectionInfo = ^RemoteSystems_IRemoteSystemConnectionInfo;

  // Windows.System.RemoteSystems.IRemoteSystemConnectionRequest2
  RemoteSystems_IRemoteSystemConnectionRequest2 = interface;
  PRemoteSystems_IRemoteSystemConnectionRequest2 = ^RemoteSystems_IRemoteSystemConnectionRequest2;

  // Windows.System.RemoteSystems.IRemoteSystemConnectionRequest3
  RemoteSystems_IRemoteSystemConnectionRequest3 = interface;
  PRemoteSystems_IRemoteSystemConnectionRequest3 = ^RemoteSystems_IRemoteSystemConnectionRequest3;

  // Windows.System.RemoteSystems.IRemoteSystemDiscoveryTypeFilter
  RemoteSystems_IRemoteSystemDiscoveryTypeFilter = interface;
  PRemoteSystems_IRemoteSystemDiscoveryTypeFilter = ^RemoteSystems_IRemoteSystemDiscoveryTypeFilter;

  // Windows.System.RemoteSystems.IRemoteSystemEnumerationCompletedEventArgs
  RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs = interface;
  PRemoteSystems_IRemoteSystemEnumerationCompletedEventArgs = ^RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemFilter
  RemoteSystems_IRemoteSystemFilter = interface;
  PRemoteSystems_IRemoteSystemFilter = ^RemoteSystems_IRemoteSystemFilter;

  // Windows.System.RemoteSystems.IRemoteSystemKindFilter
  RemoteSystems_IRemoteSystemKindFilter = interface;
  PRemoteSystems_IRemoteSystemKindFilter = ^RemoteSystems_IRemoteSystemKindFilter;

  // Windows.System.RemoteSystems.IRemoteSystemRemovedEventArgs
  RemoteSystems_IRemoteSystemRemovedEventArgs = interface;
  PRemoteSystems_IRemoteSystemRemovedEventArgs = ^RemoteSystems_IRemoteSystemRemovedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Object>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable;

  // Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult
  RemoteSystems_IRemoteSystemSessionJoinResult = interface;
  PRemoteSystems_IRemoteSystemSessionJoinResult = ^RemoteSystems_IRemoteSystemSessionJoinResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult = interface;
  PAsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult = ^AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult>
  IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult = interface;
  PIAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult = ^IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult;

  // Windows.System.RemoteSystems.IRemoteSystemSessionInfo
  RemoteSystems_IRemoteSystemSessionInfo = interface;
  PRemoteSystems_IRemoteSystemSessionInfo = ^RemoteSystems_IRemoteSystemSessionInfo;

  // Windows.System.RemoteSystems.IRemoteSystemSessionAddedEventArgs
  RemoteSystems_IRemoteSystemSessionAddedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionAddedEventArgs = ^RemoteSystems_IRemoteSystemSessionAddedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionJoinRequest
  RemoteSystems_IRemoteSystemSessionJoinRequest = interface;
  PRemoteSystems_IRemoteSystemSessionJoinRequest = ^RemoteSystems_IRemoteSystemSessionJoinRequest;

  // Windows.System.RemoteSystems.IRemoteSystemSessionJoinRequestedEventArgs
  RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs = ^RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionController,Windows.System.RemoteSystems.IRemoteSystemSessionJoinRequestedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionController__RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionController__RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionController__RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult
  RemoteSystems_IRemoteSystemSessionCreationResult = interface;
  PRemoteSystems_IRemoteSystemSessionCreationResult = ^RemoteSystems_IRemoteSystemSessionCreationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult = interface;
  PAsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult = ^AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult>
  IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult = interface;
  PIAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult = ^IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult;

  // Windows.System.RemoteSystems.IRemoteSystemSessionController
  RemoteSystems_IRemoteSystemSessionController = interface;
  PRemoteSystems_IRemoteSystemSessionController = ^RemoteSystems_IRemoteSystemSessionController;

  // Windows.System.RemoteSystems.IRemoteSystemSessionOptions
  RemoteSystems_IRemoteSystemSessionOptions = interface;
  PRemoteSystems_IRemoteSystemSessionOptions = ^RemoteSystems_IRemoteSystemSessionOptions;

  // Windows.System.RemoteSystems.IRemoteSystemSessionInvitation
  RemoteSystems_IRemoteSystemSessionInvitation = interface;
  PRemoteSystems_IRemoteSystemSessionInvitation = ^RemoteSystems_IRemoteSystemSessionInvitation;

  // Windows.System.RemoteSystems.IRemoteSystemSessionInvitationReceivedEventArgs
  RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs = ^RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionInvitationListener,Windows.System.RemoteSystems.IRemoteSystemSessionInvitationReceivedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionInvitationListener__RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionInvitationListener__RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionInvitationListener__RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionInvitationListener
  RemoteSystems_IRemoteSystemSessionInvitationListener = interface;
  PRemoteSystems_IRemoteSystemSessionInvitationListener = ^RemoteSystems_IRemoteSystemSessionInvitationListener;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemSessionParticipant>
  IIterator_1__RemoteSystems_IRemoteSystemSessionParticipant = interface;
  PIIterator_1__RemoteSystems_IRemoteSystemSessionParticipant = ^IIterator_1__RemoteSystems_IRemoteSystemSessionParticipant;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemSessionParticipant>
  IIterable_1__RemoteSystems_IRemoteSystemSessionParticipant = interface;
  PIIterable_1__RemoteSystems_IRemoteSystemSessionParticipant = ^IIterable_1__RemoteSystems_IRemoteSystemSessionParticipant;

  // Windows.System.RemoteSystems.IRemoteSystemSessionValueSetReceivedEventArgs
  RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs = ^RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionMessageChannel,Windows.System.RemoteSystems.IRemoteSystemSessionValueSetReceivedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionMessageChannel__RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionMessageChannel__RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionMessageChannel__RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionMessageChannel
  RemoteSystems_IRemoteSystemSessionMessageChannel = interface;
  PRemoteSystems_IRemoteSystemSessionMessageChannel = ^RemoteSystems_IRemoteSystemSessionMessageChannel;

  // Windows.System.RemoteSystems.IRemoteSystemSessionRemovedEventArgs
  RemoteSystems_IRemoteSystemSessionRemovedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionRemovedEventArgs = ^RemoteSystems_IRemoteSystemSessionRemovedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionAddedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionAddedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionAddedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionUpdatedEventArgs
  RemoteSystems_IRemoteSystemSessionUpdatedEventArgs = interface;
  PRemoteSystems_IRemoteSystemSessionUpdatedEventArgs = ^RemoteSystems_IRemoteSystemSessionUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionUpdatedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionUpdatedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionUpdatedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionRemovedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionRemovedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionRemovedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemSessionWatcher
  RemoteSystems_IRemoteSystemSessionWatcher = interface;
  PRemoteSystems_IRemoteSystemSessionWatcher = ^RemoteSystems_IRemoteSystemSessionWatcher;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystem>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem = interface;
  PAsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem = ^AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystem>
  IAsyncOperation_1__RemoteSystems_IRemoteSystem = interface;
  PIAsyncOperation_1__RemoteSystems_IRemoteSystem = ^IAsyncOperation_1__RemoteSystems_IRemoteSystem;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemAddedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemAddedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemAddedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemUpdatedEventArgs
  RemoteSystems_IRemoteSystemUpdatedEventArgs = interface;
  PRemoteSystems_IRemoteSystemUpdatedEventArgs = ^RemoteSystems_IRemoteSystemUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemUpdatedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemUpdatedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemUpdatedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemRemovedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemRemovedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemRemovedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemWatcher
  RemoteSystems_IRemoteSystemWatcher = interface;
  PRemoteSystems_IRemoteSystemWatcher = ^RemoteSystems_IRemoteSystemWatcher;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemFilter>
  IIterator_1__RemoteSystems_IRemoteSystemFilter = interface;
  PIIterator_1__RemoteSystems_IRemoteSystemFilter = ^IIterator_1__RemoteSystems_IRemoteSystemFilter;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemFilter>
  IIterable_1__RemoteSystems_IRemoteSystemFilter = interface;
  PIIterable_1__RemoteSystems_IRemoteSystemFilter = ^IIterable_1__RemoteSystems_IRemoteSystemFilter;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.RemoteSystemAccessStatus>
  AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus = ^AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.RemoteSystemAccessStatus>
  IAsyncOperation_1__RemoteSystems_RemoteSystemAccessStatus = interface;
  PIAsyncOperation_1__RemoteSystems_RemoteSystemAccessStatus = ^IAsyncOperation_1__RemoteSystems_RemoteSystemAccessStatus;

  // Windows.System.RemoteSystems.IRemoteSystemStatusTypeFilter
  RemoteSystems_IRemoteSystemStatusTypeFilter = interface;
  PRemoteSystems_IRemoteSystemStatusTypeFilter = ^RemoteSystems_IRemoteSystemStatusTypeFilter;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemEnumerationCompletedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemWatcherErrorOccurredEventArgs
  RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs = interface;
  PRemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs = ^RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemWatcherErrorOccurredEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs;

  // Windows.System.RemoteSystems.IRemoteSystemWatcher2
  RemoteSystems_IRemoteSystemWatcher2 = interface;
  PRemoteSystems_IRemoteSystemWatcher2 = ^RemoteSystems_IRemoteSystemWatcher2;

  // Windows.System.RemoteSystems.IRemoteSystemWatcher3
  RemoteSystems_IRemoteSystemWatcher3 = interface;
  PRemoteSystems_IRemoteSystemWatcher3 = ^RemoteSystems_IRemoteSystemWatcher3;

  // Windows.System.RemoteSystems.IRemoteSystemWebAccountFilter
  RemoteSystems_IRemoteSystemWebAccountFilter = interface;
  PRemoteSystems_IRemoteSystemWebAccountFilter = ^RemoteSystems_IRemoteSystemWebAccountFilter;

  // Windows.System.Threading.Core.IPreallocatedWorkItem
  Threading_Core_IPreallocatedWorkItem = interface;
  PThreading_Core_IPreallocatedWorkItem = ^Threading_Core_IPreallocatedWorkItem;

  // Windows.System.Threading.WorkItemHandler
  Threading_WorkItemHandler = interface;
  PThreading_WorkItemHandler = ^Threading_WorkItemHandler;

  // Windows.System.Threading.Core.ISignalNotifier
  Threading_Core_ISignalNotifier = interface;
  PThreading_Core_ISignalNotifier = ^Threading_Core_ISignalNotifier;

  // Windows.System.Threading.Core.SignalHandler
  Threading_Core_SignalHandler = interface;
  PThreading_Core_SignalHandler = ^Threading_Core_SignalHandler;

  // Windows.System.Threading.IThreadPoolTimer
  Threading_IThreadPoolTimer = interface;
  PThreading_IThreadPoolTimer = ^Threading_IThreadPoolTimer;

  // Windows.System.Threading.TimerElapsedHandler
  Threading_TimerElapsedHandler = interface;
  PThreading_TimerElapsedHandler = ^Threading_TimerElapsedHandler;

  // Windows.System.Threading.TimerDestroyedHandler
  Threading_TimerDestroyedHandler = interface;
  PThreading_TimerDestroyedHandler = ^Threading_TimerDestroyedHandler;

  // Windows.System.Update.ISystemUpdateItem
  Update_ISystemUpdateItem = interface;
  PUpdate_ISystemUpdateItem = ^Update_ISystemUpdateItem;

  // Windows.System.Update.ISystemUpdateLastErrorInfo
  Update_ISystemUpdateLastErrorInfo = interface;
  PUpdate_ISystemUpdateLastErrorInfo = ^Update_ISystemUpdateLastErrorInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.System.Update.ISystemUpdateItem>
  IIterator_1__Update_ISystemUpdateItem = interface;
  PIIterator_1__Update_ISystemUpdateItem = ^IIterator_1__Update_ISystemUpdateItem;

  // Windows.Foundation.Collections.IIterable`1<Windows.System.Update.ISystemUpdateItem>
  IIterable_1__Update_ISystemUpdateItem = interface;
  PIIterable_1__Update_ISystemUpdateItem = ^IIterable_1__Update_ISystemUpdateItem;

  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Update.ISystemUpdateItem>
  IVectorView_1__Update_ISystemUpdateItem = interface;
  PIVectorView_1__Update_ISystemUpdateItem = ^IVectorView_1__Update_ISystemUpdateItem;

  // Windows.System.UserProfile.IAdvertisingManagerForUser
  UserProfile_IAdvertisingManagerForUser = interface;
  PUserProfile_IAdvertisingManagerForUser = ^UserProfile_IAdvertisingManagerForUser;

  // Windows.System.UserProfile.IAssignedAccessSettings
  UserProfile_IAssignedAccessSettings = interface;
  PUserProfile_IAssignedAccessSettings = ^UserProfile_IAssignedAccessSettings;

  // Windows.System.UserProfile.IDiagnosticsSettings
  UserProfile_IDiagnosticsSettings = interface;
  PUserProfile_IDiagnosticsSettings = ^UserProfile_IDiagnosticsSettings;

  // Windows.System.UserProfile.IDiagnosticsSettingsStatics
  UserProfile_IDiagnosticsSettingsStatics = interface;
  PUserProfile_IDiagnosticsSettingsStatics = ^UserProfile_IDiagnosticsSettingsStatics;

  // Windows.System.UserProfile.IFirstSignInSettings
  UserProfile_IFirstSignInSettings = interface;
  PUserProfile_IFirstSignInSettings = ^UserProfile_IFirstSignInSettings;

  // Windows.System.UserProfile.IFirstSignInSettingsStatics
  UserProfile_IFirstSignInSettingsStatics = interface;
  PUserProfile_IFirstSignInSettingsStatics = ^UserProfile_IFirstSignInSettingsStatics;

  // Windows.System.UserProfile.IGlobalizationPreferencesForUser
  UserProfile_IGlobalizationPreferencesForUser = interface;
  PUserProfile_IGlobalizationPreferencesForUser = ^UserProfile_IGlobalizationPreferencesForUser;

  // Windows.System.UserProfile.IGlobalizationPreferencesStatics
  UserProfile_IGlobalizationPreferencesStatics = interface;
  PUserProfile_IGlobalizationPreferencesStatics = ^UserProfile_IGlobalizationPreferencesStatics;

  // Windows.System.UserProfile.IGlobalizationPreferencesStatics2
  UserProfile_IGlobalizationPreferencesStatics2 = interface;
  PUserProfile_IGlobalizationPreferencesStatics2 = ^UserProfile_IGlobalizationPreferencesStatics2;

  // Windows.System.UserProfile.IGlobalizationPreferencesStatics3
  UserProfile_IGlobalizationPreferencesStatics3 = interface;
  PUserProfile_IGlobalizationPreferencesStatics3 = ^UserProfile_IGlobalizationPreferencesStatics3;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.UserProfile.SetImageFeedResult>
  AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult = interface;
  PAsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult = ^AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.UserProfile.SetImageFeedResult>
  IAsyncOperation_1__UserProfile_SetImageFeedResult = interface;
  PIAsyncOperation_1__UserProfile_SetImageFeedResult = ^IAsyncOperation_1__UserProfile_SetImageFeedResult;

  // Windows.System.UserProfile.ILockScreenImageFeedStatics
  UserProfile_ILockScreenImageFeedStatics = interface;
  PUserProfile_ILockScreenImageFeedStatics = ^UserProfile_ILockScreenImageFeedStatics;

  // Windows.System.UserProfile.ILockScreenStatics
  UserProfile_ILockScreenStatics = interface;
  PUserProfile_ILockScreenStatics = ^UserProfile_ILockScreenStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.UserProfile.SetAccountPictureResult>
  AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult = interface;
  PAsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult = ^AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.System.UserProfile.SetAccountPictureResult>
  IAsyncOperation_1__UserProfile_SetAccountPictureResult = interface;
  PIAsyncOperation_1__UserProfile_SetAccountPictureResult = ^IAsyncOperation_1__UserProfile_SetAccountPictureResult;

  // Windows.System.UserProfile.IUserInformationStatics
  UserProfile_IUserInformationStatics = interface;
  PUserProfile_IUserInformationStatics = ^UserProfile_IUserInformationStatics;

  // Windows.System.UserProfile.IUserProfilePersonalizationSettings
  UserProfile_IUserProfilePersonalizationSettings = interface;
  PUserProfile_IUserProfilePersonalizationSettings = ^UserProfile_IUserProfilePersonalizationSettings;

  // Windows.System.UserProfile.IUserProfilePersonalizationSettingsStatics
  UserProfile_IUserProfilePersonalizationSettingsStatics = interface;
  PUserProfile_IUserProfilePersonalizationSettingsStatics = ^UserProfile_IUserProfilePersonalizationSettingsStatics;

  // Windows.System Enums

  // Windows.System.AppDiagnosticInfoWatcherStatus
  AppDiagnosticInfoWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PAppDiagnosticInfoWatcherStatus = ^AppDiagnosticInfoWatcherStatus;

  // Windows.System.AppMemoryUsageLevel
  AppMemoryUsageLevel = (
    Low = 0,
    Medium = 1,
    High = 2,
    OverLimit = 3
  );
  PAppMemoryUsageLevel = ^AppMemoryUsageLevel;

  // Windows.System.AppResourceGroupEnergyQuotaState
  AppResourceGroupEnergyQuotaState = (
    Unknown = 0,
    Over = 1,
    Under = 2
  );
  PAppResourceGroupEnergyQuotaState = ^AppResourceGroupEnergyQuotaState;

  // Windows.System.AppResourceGroupExecutionState
  AppResourceGroupExecutionState = (
    Unknown = 0,
    Running = 1,
    Suspending = 2,
    Suspended = 3,
    NotRunning = 4
  );
  PAppResourceGroupExecutionState = ^AppResourceGroupExecutionState;

  // Windows.System.AppResourceGroupInfoWatcherStatus
  AppResourceGroupInfoWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PAppResourceGroupInfoWatcherStatus = ^AppResourceGroupInfoWatcherStatus;

  // Windows.System.AutoUpdateTimeZoneStatus
  AutoUpdateTimeZoneStatus = (
    Attempted = 0,
    TimedOut = 1,
    Failed = 2
  );
  PAutoUpdateTimeZoneStatus = ^AutoUpdateTimeZoneStatus;

  // Windows.System.DiagnosticAccessStatus
  DiagnosticAccessStatus = (
    Unspecified = 0,
    Denied = 1,
    Limited = 2,
    Allowed = 3
  );
  PDiagnosticAccessStatus = ^DiagnosticAccessStatus;

  // Windows.System.Diagnostics.DevicePortal.DevicePortalConnectionClosedReason
  Diagnostics_DevicePortal_DevicePortalConnectionClosedReason = (
    Unknown = 0,
    ResourceLimitsExceeded = 1,
    ProtocolError = 2,
    NotAuthorized = 3,
    UserNotPresent = 4,
    ServiceTerminated = 5
  );
  PDiagnostics_DevicePortal_DevicePortalConnectionClosedReason = ^Diagnostics_DevicePortal_DevicePortalConnectionClosedReason;

  // Windows.System.Diagnostics.DiagnosticActionState
  Diagnostics_DiagnosticActionState = (
    Initializing = 0,
    Downloading = 1,
    VerifyingTrust = 2,
    Detecting = 3,
    Resolving = 4,
    VerifyingResolution = 5
  );
  PDiagnostics_DiagnosticActionState = ^Diagnostics_DiagnosticActionState;

  // Windows.System.Diagnostics.Telemetry.PlatformTelemetryRegistrationStatus
  Diagnostics_Telemetry_PlatformTelemetryRegistrationStatus = (
    Success = 0,
    SettingsOutOfRange = 1,
    UnknownFailure = 2
  );
  PDiagnostics_Telemetry_PlatformTelemetryRegistrationStatus = ^Diagnostics_Telemetry_PlatformTelemetryRegistrationStatus;

  // Windows.System.Diagnostics.TraceReporting.PlatformDiagnosticActionState
  Diagnostics_TraceReporting_PlatformDiagnosticActionState = (
    Success = 0,
    FreeNetworkNotAvailable = 1,
    ACPowerNotAvailable = 2
  );
  PDiagnostics_TraceReporting_PlatformDiagnosticActionState = ^Diagnostics_TraceReporting_PlatformDiagnosticActionState;

  // Windows.System.Diagnostics.TraceReporting.PlatformDiagnosticEscalationType
  Diagnostics_TraceReporting_PlatformDiagnosticEscalationType = (
    OnCompletion = 0,
    OnFailure = 1
  );
  PDiagnostics_TraceReporting_PlatformDiagnosticEscalationType = ^Diagnostics_TraceReporting_PlatformDiagnosticEscalationType;

  // Windows.System.Diagnostics.TraceReporting.PlatformDiagnosticEventBufferLatencies
  Diagnostics_TraceReporting_PlatformDiagnosticEventBufferLatencies = (
    Normal = 1,
    CostDeferred = 2,
    Realtime = 4
  );
  PDiagnostics_TraceReporting_PlatformDiagnosticEventBufferLatencies = ^Diagnostics_TraceReporting_PlatformDiagnosticEventBufferLatencies;

  // Windows.System.Diagnostics.TraceReporting.PlatformDiagnosticTracePriority
  Diagnostics_TraceReporting_PlatformDiagnosticTracePriority = (
    Normal = 0,
    UserElevated = 1
  );
  PDiagnostics_TraceReporting_PlatformDiagnosticTracePriority = ^Diagnostics_TraceReporting_PlatformDiagnosticTracePriority;

  // Windows.System.Diagnostics.TraceReporting.PlatformDiagnosticTraceSlotState
  Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotState = (
    NotRunning = 0,
    Running = 1,
    Throttled = 2
  );
  PDiagnostics_TraceReporting_PlatformDiagnosticTraceSlotState = ^Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotState;

  // Windows.System.Diagnostics.TraceReporting.PlatformDiagnosticTraceSlotType
  Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType = (
    Alternative = 0,
    AlwaysOn = 1,
    Mini = 2
  );
  PDiagnostics_TraceReporting_PlatformDiagnosticTraceSlotType = ^Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType;

  // Windows.System.LaunchFileStatus
  LaunchFileStatus = (
    Success = 0,
    AppUnavailable = 1,
    DeniedByPolicy = 2,
    FileTypeNotSupported = 3,
    Unknown = 4
  );
  PLaunchFileStatus = ^LaunchFileStatus;

  // Windows.System.LaunchQuerySupportStatus
  LaunchQuerySupportStatus = (
    Available = 0,
    AppNotInstalled = 1,
    AppUnavailable = 2,
    NotSupported = 3,
    Unknown = 4
  );
  PLaunchQuerySupportStatus = ^LaunchQuerySupportStatus;

  // Windows.System.LaunchQuerySupportType
  LaunchQuerySupportType = (
    Uri = 0,
    UriForResults = 1
  );
  PLaunchQuerySupportType = ^LaunchQuerySupportType;

  // Windows.System.LaunchUriStatus
  LaunchUriStatus = (
    Success = 0,
    AppUnavailable = 1,
    ProtocolUnavailable = 2,
    Unknown = 3
  );
  PLaunchUriStatus = ^LaunchUriStatus;

  // Windows.System.Power.EnergySaverStatus
  Power_EnergySaverStatus = (
    Disabled = 0,
    Off = 1,
    &On = 2
  );
  PPower_EnergySaverStatus = ^Power_EnergySaverStatus;

  // Windows.System.Power.PowerSupplyStatus
  Power_PowerSupplyStatus = (
    NotPresent = 0,
    Inadequate = 1,
    Adequate = 2
  );
  PPower_PowerSupplyStatus = ^Power_PowerSupplyStatus;

  // Windows.System.PowerState
  PowerState = (
    ConnectedStandby = 0,
    SleepS3 = 1
  );
  PPowerState = ^PowerState;

  // Windows.System.Preview.HingeState
  Preview_HingeState = (
    Unknown = 0,
    Closed = 1,
    Concave = 2,
    Flat = 3,
    Convex = 4,
    Full = 5
  );
  PPreview_HingeState = ^Preview_HingeState;

  // Windows.System.Profile.PlatformDataCollectionLevel
  Profile_PlatformDataCollectionLevel = (
    Security = 0,
    Basic = 1,
    Enhanced = 2,
    Full = 3
  );
  PProfile_PlatformDataCollectionLevel = ^Profile_PlatformDataCollectionLevel;

  // Windows.System.Profile.SystemIdentificationSource
  Profile_SystemIdentificationSource = (
    None = 0,
    Tpm = 1,
    Uefi = 2,
    Registry = 3
  );
  PProfile_SystemIdentificationSource = ^Profile_SystemIdentificationSource;

  // Windows.System.Profile.SystemOutOfBoxExperienceState
  Profile_SystemOutOfBoxExperienceState = (
    NotStarted = 0,
    InProgress = 1,
    Completed = 2
  );
  PProfile_SystemOutOfBoxExperienceState = ^Profile_SystemOutOfBoxExperienceState;

  // Windows.System.Profile.UnsupportedAppRequirementReasons
  Profile_UnsupportedAppRequirementReasons = (
    Unknown = 0,
    DeniedBySystem = 1
  );
  PProfile_UnsupportedAppRequirementReasons = ^Profile_UnsupportedAppRequirementReasons;

  // Windows.System.RemoteLaunchUriStatus
  RemoteLaunchUriStatus = (
    Unknown = 0,
    Success = 1,
    AppUnavailable = 2,
    ProtocolUnavailable = 3,
    RemoteSystemUnavailable = 4,
    ValueSetTooLarge = 5,
    DeniedByLocalSystem = 6,
    DeniedByRemoteSystem = 7
  );
  PRemoteLaunchUriStatus = ^RemoteLaunchUriStatus;

  // Windows.System.RemoteSystems.RemoteSystemAccessStatus
  RemoteSystems_RemoteSystemAccessStatus = (
    Unspecified = 0,
    Allowed = 1,
    DeniedByUser = 2,
    DeniedBySystem = 3
  );
  PRemoteSystems_RemoteSystemAccessStatus = ^RemoteSystems_RemoteSystemAccessStatus;

  // Windows.System.RemoteSystems.RemoteSystemAuthorizationKind
  RemoteSystems_RemoteSystemAuthorizationKind = (
    SameUser = 0,
    Anonymous = 1
  );
  PRemoteSystems_RemoteSystemAuthorizationKind = ^RemoteSystems_RemoteSystemAuthorizationKind;

  // Windows.System.RemoteSystems.RemoteSystemDiscoveryType
  RemoteSystems_RemoteSystemDiscoveryType = (
    Any = 0,
    Proximal = 1,
    Cloud = 2,
    SpatiallyProximal = 3
  );
  PRemoteSystems_RemoteSystemDiscoveryType = ^RemoteSystems_RemoteSystemDiscoveryType;

  // Windows.System.RemoteSystems.RemoteSystemPlatform
  RemoteSystems_RemoteSystemPlatform = (
    Unknown = 0,
    Windows = 1,
    Android = 2,
    Ios = 3,
    Linux = 4
  );
  PRemoteSystems_RemoteSystemPlatform = ^RemoteSystems_RemoteSystemPlatform;

  // Windows.System.RemoteSystems.RemoteSystemSessionCreationStatus
  RemoteSystems_RemoteSystemSessionCreationStatus = (
    Success = 0,
    SessionLimitsExceeded = 1,
    OperationAborted = 2
  );
  PRemoteSystems_RemoteSystemSessionCreationStatus = ^RemoteSystems_RemoteSystemSessionCreationStatus;

  // Windows.System.RemoteSystems.RemoteSystemSessionDisconnectedReason
  RemoteSystems_RemoteSystemSessionDisconnectedReason = (
    SessionUnavailable = 0,
    RemovedByController = 1,
    SessionClosed = 2
  );
  PRemoteSystems_RemoteSystemSessionDisconnectedReason = ^RemoteSystems_RemoteSystemSessionDisconnectedReason;

  // Windows.System.RemoteSystems.RemoteSystemSessionJoinStatus
  RemoteSystems_RemoteSystemSessionJoinStatus = (
    Success = 0,
    SessionLimitsExceeded = 1,
    OperationAborted = 2,
    SessionUnavailable = 3,
    RejectedByController = 4
  );
  PRemoteSystems_RemoteSystemSessionJoinStatus = ^RemoteSystems_RemoteSystemSessionJoinStatus;

  // Windows.System.RemoteSystems.RemoteSystemSessionMessageChannelReliability
  RemoteSystems_RemoteSystemSessionMessageChannelReliability = (
    Reliable = 0,
    Unreliable = 1
  );
  PRemoteSystems_RemoteSystemSessionMessageChannelReliability = ^RemoteSystems_RemoteSystemSessionMessageChannelReliability;

  // Windows.System.RemoteSystems.RemoteSystemSessionParticipantWatcherStatus
  RemoteSystems_RemoteSystemSessionParticipantWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PRemoteSystems_RemoteSystemSessionParticipantWatcherStatus = ^RemoteSystems_RemoteSystemSessionParticipantWatcherStatus;

  // Windows.System.RemoteSystems.RemoteSystemSessionWatcherStatus
  RemoteSystems_RemoteSystemSessionWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PRemoteSystems_RemoteSystemSessionWatcherStatus = ^RemoteSystems_RemoteSystemSessionWatcherStatus;

  // Windows.System.RemoteSystems.RemoteSystemStatusType
  RemoteSystems_RemoteSystemStatusType = (
    Any = 0,
    Available = 1
  );
  PRemoteSystems_RemoteSystemStatusType = ^RemoteSystems_RemoteSystemStatusType;

  // Windows.System.RemoteSystems.RemoteSystemWatcherError
  RemoteSystems_RemoteSystemWatcherError = (
    Unknown = 0,
    InternetNotAvailable = 1,
    AuthenticationError = 2
  );
  PRemoteSystems_RemoteSystemWatcherError = ^RemoteSystems_RemoteSystemWatcherError;

  // Windows.System.ShutdownKind
  ShutdownKind = (
    Shutdown = 0,
    Restart = 1
  );
  PShutdownKind = ^ShutdownKind;

  // Windows.System.Threading.WorkItemOptions
  Threading_WorkItemOptions = (
    None = 0,
    TimeSliced = 1
  );
  PThreading_WorkItemOptions = ^Threading_WorkItemOptions;

  // Windows.System.Threading.WorkItemPriority
  Threading_WorkItemPriority = (
    Low = -1,
    Normal = 0,
    High = 1
  );
  PThreading_WorkItemPriority = ^Threading_WorkItemPriority;

  // Windows.System.Update.SystemUpdateAttentionRequiredReason
  Update_SystemUpdateAttentionRequiredReason = (
    None = 0,
    NetworkRequired = 1,
    InsufficientDiskSpace = 2,
    InsufficientBattery = 3,
    UpdateBlocked = 4
  );
  PUpdate_SystemUpdateAttentionRequiredReason = ^Update_SystemUpdateAttentionRequiredReason;

  // Windows.System.Update.SystemUpdateItemState
  Update_SystemUpdateItemState = (
    NotStarted = 0,
    Initializing = 1,
    Preparing = 2,
    Calculating = 3,
    Downloading = 4,
    Installing = 5,
    Completed = 6,
    RebootRequired = 7,
    Error = 8
  );
  PUpdate_SystemUpdateItemState = ^Update_SystemUpdateItemState;

  // Windows.System.Update.SystemUpdateManagerState
  Update_SystemUpdateManagerState = (
    Idle = 0,
    Detecting = 1,
    ReadyToDownload = 2,
    Downloading = 3,
    ReadyToInstall = 4,
    Installing = 5,
    RebootRequired = 6,
    ReadyToFinalize = 7,
    Finalizing = 8,
    Completed = 9,
    AttentionRequired = 10,
    Error = 11
  );
  PUpdate_SystemUpdateManagerState = ^Update_SystemUpdateManagerState;

  // Windows.System.Update.SystemUpdateStartInstallAction
  Update_SystemUpdateStartInstallAction = (
    UpToReboot = 0,
    AllowReboot = 1
  );
  PUpdate_SystemUpdateStartInstallAction = ^Update_SystemUpdateStartInstallAction;

  // Windows.System.UserProfile.AccountPictureKind
  UserProfile_AccountPictureKind = (
    SmallImage = 0,
    LargeImage = 1,
    Video = 2
  );
  PUserProfile_AccountPictureKind = ^UserProfile_AccountPictureKind;

  // Windows.System.UserProfile.SetAccountPictureResult
  UserProfile_SetAccountPictureResult = (
    Success = 0,
    ChangeDisabled = 1,
    LargeOrDynamicError = 2,
    VideoFrameSizeError = 3,
    FileSizeError = 4,
    Failure = 5
  );
  PUserProfile_SetAccountPictureResult = ^UserProfile_SetAccountPictureResult;

  // Windows.System.UserProfile.SetImageFeedResult
  UserProfile_SetImageFeedResult = (
    Success = 0,
    ChangeDisabled = 1,
    UserCanceled = 2
  );
  PUserProfile_SetImageFeedResult = ^UserProfile_SetImageFeedResult;

  // Windows.System.UserWatcherStatus
  UserWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PUserWatcherStatus = ^UserWatcherStatus;

  // Windows.System.UserWatcherUpdateKind
  UserWatcherUpdateKind = (
    Properties = 0,
    Picture = 1
  );
  PUserWatcherUpdateKind = ^UserWatcherUpdateKind;

  // Windows.System Records
  // Windows.System.Profile.ProfileHardwareTokenContract
  Profile_ProfileHardwareTokenContract = record
  end;
  PProfile_ProfileHardwareTokenContract = ^Profile_ProfileHardwareTokenContract;

  // Windows.System.Profile.ProfileRetailInfoContract
  Profile_ProfileRetailInfoContract = record
  end;
  PProfile_ProfileRetailInfoContract = ^Profile_ProfileRetailInfoContract;

  // Windows.System.Profile.ProfileSharedModeContract
  Profile_ProfileSharedModeContract = record
  end;
  PProfile_ProfileSharedModeContract = ^Profile_ProfileSharedModeContract;

  // Windows.System.Profile.SystemManufacturers.SystemManufacturersContract
  Profile_SystemManufacturers_SystemManufacturersContract = record
  end;
  PProfile_SystemManufacturers_SystemManufacturersContract = ^Profile_SystemManufacturers_SystemManufacturersContract;

  // Windows.System.SystemManagementContract
  SystemManagementContract = record
  end;
  PSystemManagementContract = ^SystemManagementContract;

  // Windows.System.UserProfile.UserProfileContract
  UserProfile_UserProfileContract = record
  end;
  PUserProfile_UserProfileContract = ^UserProfile_UserProfileContract;

  // Windows.System.UserProfile.UserProfileLockScreenContract
  UserProfile_UserProfileLockScreenContract = record
  end;
  PUserProfile_UserProfileLockScreenContract = ^UserProfile_UserProfileLockScreenContract;

  // Windows.System Interfaces

  // UsedAPI Interface
  // Windows.System.IProtocolForResultsOperation
  IProtocolForResultsOperation = interface(IInspectable)
  ['{D581293A-6DE9-4D28-9378-F86782E182BB}']
    procedure ReportCompleted(data: IPropertySet); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionDisconnectedEventArgs
  RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs = interface(IInspectable)
  ['{DE0BC69B-77C5-461C-8209-7C6C5D3111AB}']
    function get_Reason: RemoteSystems_RemoteSystemSessionDisconnectedReason; safecall;
    property Reason: RemoteSystems_RemoteSystemSessionDisconnectedReason read get_Reason;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSession,Windows.System.RemoteSystems.IRemoteSystemSessionDisconnectedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSession__RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs_Delegate_Base = interface(IUnknown)
  ['{FBA14773-5038-511A-95A3-4BA45349100A}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSession; args: RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSession,Windows.System.RemoteSystems.IRemoteSystemSessionDisconnectedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSession__RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSession__RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs_Delegate_Base)
  ['{FA2DF6EF-2BC7-513C-B451-4EEBE73F3565}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipant
  RemoteSystems_IRemoteSystemSessionParticipant = interface(IInspectable)
  ['{7E90058C-ACF9-4729-8A17-44E7BAED5DCC}']
    function get_RemoteSystem: RemoteSystems_IRemoteSystem; safecall;
    function GetHostNames: IVectorView_1__IHostName; safecall;
    property RemoteSystem: RemoteSystems_IRemoteSystem read get_RemoteSystem;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipantAddedEventArgs
  RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs = interface(IInspectable)
  ['{D35A57D8-C9A1-4BB7-B6B0-79BB91ADF93D}']
    function get_Participant: RemoteSystems_IRemoteSystemSessionParticipant; safecall;
    property Participant: RemoteSystems_IRemoteSystemSessionParticipant read get_Participant;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionParticipantAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs_Delegate_Base = interface(IUnknown)
  ['{7D42FFF3-FD21-5E15-B21A-75E1BBCD13C7}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionParticipantWatcher; args: RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionParticipantAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs_Delegate_Base)
  ['{8050242C-8736-5D00-8922-F57E2C4619CA}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipantRemovedEventArgs
  RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs = interface(IInspectable)
  ['{866EF088-DE68-4ABF-88A1-F90D16274192}']
    function get_Participant: RemoteSystems_IRemoteSystemSessionParticipant; safecall;
    property Participant: RemoteSystems_IRemoteSystemSessionParticipant read get_Participant;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionParticipantRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs_Delegate_Base = interface(IUnknown)
  ['{73D7E8B3-7D44-50C8-AFAD-450DE59FD0AE}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionParticipantWatcher; args: RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionParticipantRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs_Delegate_Base)
  ['{ED891AAF-359D-5082-A50F-786496D6DAAB}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher
  RemoteSystems_IRemoteSystemSessionParticipantWatcher = interface(IInspectable)
  ['{DCDD02CC-AA87-4D79-B6CC-4459B3E92075}']
    procedure Start; safecall;
    procedure Stop; safecall;
    function get_Status: RemoteSystems_RemoteSystemSessionParticipantWatcherStatus; safecall;
    function add_Added(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantAddedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Added(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__RemoteSystems_IRemoteSystemSessionParticipantRemovedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    function add_EnumerationCompleted(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(token: EventRegistrationToken); safecall;
    property Status: RemoteSystems_RemoteSystemSessionParticipantWatcherStatus read get_Status;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSession
  RemoteSystems_IRemoteSystemSession = interface(IInspectable)
  ['{69476A01-9ADA-490F-9549-D31CB14C9E95}']
    function get_Id: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_ControllerDisplayName: HSTRING; safecall;
    function add_Disconnected(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSession__RemoteSystems_IRemoteSystemSessionDisconnectedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Disconnected(token: EventRegistrationToken); safecall;
    function CreateParticipantWatcher: RemoteSystems_IRemoteSystemSessionParticipantWatcher; safecall;
    function SendInvitationAsync(invitee: RemoteSystems_IRemoteSystem): IAsyncOperation_1__Boolean; safecall;
    property ControllerDisplayName: HSTRING read get_ControllerDisplayName;
    property DisplayName: HSTRING read get_DisplayName;
    property Id: HSTRING read get_Id;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionClosedEventArgs
  Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs = interface(IInspectable)
  ['{FCF70E38-7032-428C-9F50-945C15A9F0CB}']
    function get_Reason: Diagnostics_DevicePortal_DevicePortalConnectionClosedReason; safecall;
    property Reason: Diagnostics_DevicePortal_DevicePortalConnectionClosedReason read get_Reason;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection,Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionClosedEventArgs>
  TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs_Delegate_Base = interface(IUnknown)
  ['{2AAD93A8-52FA-54B3-9556-15D651208B3F}']
    procedure Invoke(sender: Diagnostics_DevicePortal_IDevicePortalConnection; args: Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection,Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionClosedEventArgs>
  TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs = interface(TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs_Delegate_Base)
  ['{E9F86B9E-CF54-5113-B75C-6A33CAD94BF7}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface(IUnknown)
  ['{EA0FE405-D432-5AC7-9EF8-5A65E1F97D7E}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; progressInfo: Cardinal); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface(IUnknown)
  ['{1E466DC5-840F-54F9-B877-5E3A9F4B6C74}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Cardinal__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
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

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface(IUnknown)
  ['{C1D3D1A2-AE17-5A5F-B5A2-BDCC8844889A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Boolean; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface(IInspectable)
  ['{CDB5EFB3-5788-509D-9BE1-71CCB8A3362A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Boolean); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Boolean; safecall;
    function GetResults: Boolean; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Boolean read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IKeyValuePair`2<String,Object>
  IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{09335560-6C6B-5A26-9348-97B781132B20}']
    function get_Key: HSTRING; safecall;
    function get_Value: IInspectable; safecall;
    property Key: HSTRING read get_Key;
    property Value: IInspectable read get_Value;
  end;

  // UsedAPI Interface
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

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable = interface(IInspectable)
  ['{FE2F3D47-5D47-5499-8374-430C7CDA0204}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IInspectable; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IMapView`2<String,Object>
  IMapView_2__HSTRING__IInspectable = interface(IInspectable)
  ['{BB78502A-F79D-54FA-92C9-90C5039FDF7E}']
    function Lookup(key: HSTRING): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IInspectable; out second: IMapView_2__HSTRING__IInspectable); safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
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

  // UsedAPI Interface
  // Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionRequestReceivedEventArgs
  Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs = interface(IInspectable)
  ['{64DAE045-6FDA-4459-9EBD-ECCE22E38559}']
    function get_RequestMessage: Http_IHttpRequestMessage; safecall;
    function get_ResponseMessage: Http_IHttpResponseMessage; safecall;
    property RequestMessage: Http_IHttpRequestMessage read get_RequestMessage;
    property ResponseMessage: Http_IHttpResponseMessage read get_ResponseMessage;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection,Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionRequestReceivedEventArgs>
  TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{D8E33FF8-8AC4-5FD9-B184-8AE87D828EB9}']
    procedure Invoke(sender: Diagnostics_DevicePortal_IDevicePortalConnection; args: Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection,Windows.System.Diagnostics.DevicePortal.IDevicePortalConnectionRequestReceivedEventArgs>
  TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs = interface(TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs_Delegate_Base)
  ['{310F708B-99D6-56B5-B9FB-16EE3829BF30}']
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.DevicePortal.IDevicePortalConnection
  Diagnostics_DevicePortal_IDevicePortalConnection = interface(IInspectable)
  ['{0F447F51-1198-4DA1-8D54-BDEF393E09B6}']
    function add_Closed(handler: TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionClosedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
    function add_RequestReceived(handler: TypedEventHandler_2__Diagnostics_DevicePortal_IDevicePortalConnection__Diagnostics_DevicePortal_IDevicePortalConnectionRequestReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RequestReceived(token: EventRegistrationToken); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IMapChangedEventArgs`1<String>
  IMapChangedEventArgs_1__HSTRING = interface(IInspectable)
  ['{60141EFB-F2F9-5377-96FD-F8C60D9558B5}']
    function get_CollectionChange: CollectionChange; safecall;
    function get_Key: HSTRING; safecall;
    property CollectionChange_: CollectionChange read get_CollectionChange;
    property Key: HSTRING read get_Key;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.MapChangedEventHandler`2<String,Object>
  MapChangedEventHandler_2__HSTRING__IInspectable = interface(IUnknown)
  ['{24F981E5-DDCA-538D-AADA-A59906084CF1}']
    procedure Invoke(sender: IObservableMap_2__HSTRING__IInspectable; event: IMapChangedEventArgs_1__HSTRING); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IObservableMap`2<String,Object>
  IObservableMap_2__HSTRING__IInspectable = interface(IInspectable)
  ['{236AAC9D-FB12-5C4D-A41C-9E445FB4D7EC}']
    function add_MapChanged(vhnd: MapChangedEventHandler_2__HSTRING__IInspectable): EventRegistrationToken; safecall;
    procedure remove_MapChanged(token: EventRegistrationToken); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.DevicePortal.IDevicePortalWebSocketConnection
  Diagnostics_DevicePortal_IDevicePortalWebSocketConnection = interface(IInspectable)
  ['{67657920-D65A-42F0-AEF4-787808098B7B}']
    function GetServerMessageWebSocketForRequest(request: Http_IHttpRequestMessage): IServerMessageWebSocket; overload; safecall;
    function GetServerMessageWebSocketForRequest(request: Http_IHttpRequestMessage; messageType: SocketMessageType; protocol: HSTRING): IServerMessageWebSocket; overload; safecall;
    function GetServerMessageWebSocketForRequest(request: Http_IHttpRequestMessage; messageType: SocketMessageType; protocol: HSTRING; outboundBufferSizeInBytes: Cardinal; maxMessageSize: Cardinal; receiveMode: MessageWebSocketReceiveMode): IServerMessageWebSocket; overload; safecall;
    function GetServerStreamWebSocketForRequest(request: Http_IHttpRequestMessage): IServerStreamWebSocket; overload; safecall;
    function GetServerStreamWebSocketForRequest(request: Http_IHttpRequestMessage; protocol: HSTRING; outboundBufferSizeInBytes: Cardinal; noDelay: Boolean): IServerStreamWebSocket; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.DevicePortal.IDevicePortalWebSocketConnectionRequestReceivedEventArgs
  Diagnostics_DevicePortal_IDevicePortalWebSocketConnectionRequestReceivedEventArgs = interface(IInspectable)
  ['{79FDCABA-175C-4739-9F74-DDA797C35B3F}']
    function get_IsWebSocketUpgradeRequest: Boolean; safecall;
    function get_WebSocketProtocolsRequested: IVectorView_1__HSTRING; safecall;
    function GetDeferral: IDeferral; safecall;
    property IsWebSocketUpgradeRequest: Boolean read get_IsWebSocketUpgradeRequest;
    property WebSocketProtocolsRequested: IVectorView_1__HSTRING read get_WebSocketProtocolsRequested;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IDiagnosticActionResult
  Diagnostics_IDiagnosticActionResult = interface(IInspectable)
  ['{C265A296-E73B-4097-B28F-3442F03DD831}']
    function get_ExtendedError: HRESULT; safecall;
    function get_Results: IPropertySet; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Results: IPropertySet read get_Results;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState_Delegate_Base = interface(IUnknown)
  ['{A0422898-B50A-52E3-B461-53989308BE12}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState; progressInfo: Diagnostics_DiagnosticActionState); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = interface(AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState_Delegate_Base)
  ['{FCB602D1-427A-5444-8BF6-0869255DF4BE}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState_Delegate_Base = interface(IUnknown)
  ['{390B0091-CAF7-5B64-839D-4990AE7F753C}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = interface(AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState_Delegate_Base)
  ['{3567B618-0206-58EC-B7E8-CA14C1D88A47}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState_Base = interface(IInspectable)
  ['{BB5D493E-74E9-57A1-8C4C-923E0DC4565B}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState; safecall;
    function GetResults: Diagnostics_IDiagnosticActionResult; safecall;
    property Progress: AsyncOperationProgressHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.System.Diagnostics.IDiagnosticActionResult,Windows.System.Diagnostics.DiagnosticActionState>
  IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState = interface(IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState_Base)
  ['{AD7AF5A3-0017-5C89-BF4E-B8C15E0E77A0}']
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IDiagnosticInvoker
  Diagnostics_IDiagnosticInvoker = interface(IInspectable)
  ['{187B270A-02E3-4F86-84FC-FDD892B5940F}']
    function RunDiagnosticActionAsync(context: Json_IJsonObject): IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IDiagnosticInvoker2
  Diagnostics_IDiagnosticInvoker2 = interface(IInspectable)
  ['{E3BF945C-155A-4B52-A8EC-070C44F95000}']
    function RunDiagnosticActionFromStringAsync(context: HSTRING): IAsyncOperationWithProgress_2__Diagnostics_IDiagnosticActionResult__Diagnostics_DiagnosticActionState; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessCpuUsageReport
  Diagnostics_IProcessCpuUsageReport = interface(IInspectable)
  ['{8A6D9CAC-3987-4E2F-A119-6B5FA214F1B4}']
    function get_KernelTime: TimeSpan; safecall;
    function get_UserTime: TimeSpan; safecall;
    property KernelTime: TimeSpan read get_KernelTime;
    property UserTime: TimeSpan read get_UserTime;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessCpuUsage
  Diagnostics_IProcessCpuUsage = interface(IInspectable)
  ['{0BBB2472-C8BF-423A-A810-B559AE4354E2}']
    function GetReport: Diagnostics_IProcessCpuUsageReport; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessDiskUsageReport
  Diagnostics_IProcessDiskUsageReport = interface(IInspectable)
  ['{401627FD-535D-4C1F-81B8-DA54E1BE635E}']
    function get_ReadOperationCount: Int64; safecall;
    function get_WriteOperationCount: Int64; safecall;
    function get_OtherOperationCount: Int64; safecall;
    function get_BytesReadCount: Int64; safecall;
    function get_BytesWrittenCount: Int64; safecall;
    function get_OtherBytesCount: Int64; safecall;
    property BytesReadCount: Int64 read get_BytesReadCount;
    property BytesWrittenCount: Int64 read get_BytesWrittenCount;
    property OtherBytesCount: Int64 read get_OtherBytesCount;
    property OtherOperationCount: Int64 read get_OtherOperationCount;
    property ReadOperationCount: Int64 read get_ReadOperationCount;
    property WriteOperationCount: Int64 read get_WriteOperationCount;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessDiskUsage
  Diagnostics_IProcessDiskUsage = interface(IInspectable)
  ['{5AD78BFD-7E51-4E53-BFAA-5A6EE1AABBF8}']
    function GetReport: Diagnostics_IProcessDiskUsageReport; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessMemoryUsageReport
  Diagnostics_IProcessMemoryUsageReport = interface(IInspectable)
  ['{C2C77CBA-1951-4685-8532-7E749ECF8EEB}']
    function get_NonPagedPoolSizeInBytes: UInt64; safecall;
    function get_PageFaultCount: Cardinal; safecall;
    function get_PageFileSizeInBytes: UInt64; safecall;
    function get_PagedPoolSizeInBytes: UInt64; safecall;
    function get_PeakNonPagedPoolSizeInBytes: UInt64; safecall;
    function get_PeakPageFileSizeInBytes: UInt64; safecall;
    function get_PeakPagedPoolSizeInBytes: UInt64; safecall;
    function get_PeakVirtualMemorySizeInBytes: UInt64; safecall;
    function get_PeakWorkingSetSizeInBytes: UInt64; safecall;
    function get_PrivatePageCount: UInt64; safecall;
    function get_VirtualMemorySizeInBytes: UInt64; safecall;
    function get_WorkingSetSizeInBytes: UInt64; safecall;
    property NonPagedPoolSizeInBytes: UInt64 read get_NonPagedPoolSizeInBytes;
    property PageFaultCount: Cardinal read get_PageFaultCount;
    property PageFileSizeInBytes: UInt64 read get_PageFileSizeInBytes;
    property PagedPoolSizeInBytes: UInt64 read get_PagedPoolSizeInBytes;
    property PeakNonPagedPoolSizeInBytes: UInt64 read get_PeakNonPagedPoolSizeInBytes;
    property PeakPageFileSizeInBytes: UInt64 read get_PeakPageFileSizeInBytes;
    property PeakPagedPoolSizeInBytes: UInt64 read get_PeakPagedPoolSizeInBytes;
    property PeakVirtualMemorySizeInBytes: UInt64 read get_PeakVirtualMemorySizeInBytes;
    property PeakWorkingSetSizeInBytes: UInt64 read get_PeakWorkingSetSizeInBytes;
    property PrivatePageCount: UInt64 read get_PrivatePageCount;
    property VirtualMemorySizeInBytes: UInt64 read get_VirtualMemorySizeInBytes;
    property WorkingSetSizeInBytes: UInt64 read get_WorkingSetSizeInBytes;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessMemoryUsage
  Diagnostics_IProcessMemoryUsage = interface(IInspectable)
  ['{F50B229B-827C-42B7-B07C-0E32627E6B3E}']
    function GetReport: Diagnostics_IProcessMemoryUsageReport; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessDiagnosticInfo
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_ProcessDiagnosticInfo)]
  Diagnostics_IProcessDiagnosticInfo = interface(IInspectable)
  ['{E830B04B-300E-4EE6-A0AB-5B5F5231B434}']
    function get_ProcessId: Cardinal; safecall;
    function get_ExecutableFileName: HSTRING; safecall;
    function get_Parent: Diagnostics_IProcessDiagnosticInfo; safecall;
    function get_ProcessStartTime: DateTime; safecall;
    function get_DiskUsage: Diagnostics_IProcessDiskUsage; safecall;
    function get_MemoryUsage: Diagnostics_IProcessMemoryUsage; safecall;
    function get_CpuUsage: Diagnostics_IProcessCpuUsage; safecall;
    property CpuUsage: Diagnostics_IProcessCpuUsage read get_CpuUsage;
    property DiskUsage: Diagnostics_IProcessDiskUsage read get_DiskUsage;
    property ExecutableFileName: HSTRING read get_ExecutableFileName;
    property MemoryUsage: Diagnostics_IProcessMemoryUsage read get_MemoryUsage;
    property Parent: Diagnostics_IProcessDiagnosticInfo read get_Parent;
    property ProcessId: Cardinal read get_ProcessId;
    property ProcessStartTime: DateTime read get_ProcessStartTime;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IAppDiagnosticInfo
  [WinRTClassNameAttribute(SWindows_System_AppDiagnosticInfo)]
  IAppDiagnosticInfo = interface(IInspectable)
  ['{E348A69A-8889-4CA3-BE07-D5FFFF5F0804}']
    function get_AppInfo: IAppInfo; safecall;
    property AppInfo: IAppInfo read get_AppInfo;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppDiagnosticInfo>
  IIterator_1__IAppDiagnosticInfo_Base = interface(IInspectable)
  ['{183F1E4A-2224-5FE4-B064-68869C53E361}']
    function get_Current: IAppDiagnosticInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIAppDiagnosticInfo): Cardinal; safecall;
    property Current: IAppDiagnosticInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppDiagnosticInfo>
  IIterator_1__IAppDiagnosticInfo = interface(IIterator_1__IAppDiagnosticInfo_Base)
  ['{DF568DD5-94EA-58BE-B7D0-A548D9A07CBF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppDiagnosticInfo>
  IIterable_1__IAppDiagnosticInfo_Base = interface(IInspectable)
  ['{8118DE8F-3AE3-55E1-80A8-25453CDBA894}']
    function First: IIterator_1__IAppDiagnosticInfo; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppDiagnosticInfo>
  IIterable_1__IAppDiagnosticInfo = interface(IIterable_1__IAppDiagnosticInfo_Base)
  ['{C9F3E100-91FF-5C51-919A-78099F112FEB}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppDiagnosticInfo>
  IVectorView_1__IAppDiagnosticInfo = interface(IInspectable)
  ['{111C345B-2F9D-5733-A270-3177DBFB6B20}']
    function GetAt(index: Cardinal): IAppDiagnosticInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IAppDiagnosticInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppDiagnosticInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>
  IVector_1__IAppDiagnosticInfo_Base = interface(IInspectable)
  ['{9CFFA2C3-7EEB-599C-B94D-C794B11F807F}']
    function GetAt(index: Cardinal): IAppDiagnosticInfo; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IAppDiagnosticInfo; safecall;
    function IndexOf(value: IAppDiagnosticInfo; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IAppDiagnosticInfo); safecall;
    procedure InsertAt(index: Cardinal; value: IAppDiagnosticInfo); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IAppDiagnosticInfo); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppDiagnosticInfo): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIAppDiagnosticInfo); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>
  IVector_1__IAppDiagnosticInfo = interface(IVector_1__IAppDiagnosticInfo_Base)
  ['{9A0235E7-9D87-5D5D-8DF4-578F8AD4CEA6}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessDiagnosticInfo2
  Diagnostics_IProcessDiagnosticInfo2 = interface(IInspectable)
  ['{9558CB1A-3D0B-49EC-AB70-4F7A112805DE}']
    function GetAppDiagnosticInfos: IVector_1__IAppDiagnosticInfo; safecall;
    function get_IsPackaged: Boolean; safecall;
    property IsPackaged: Boolean read get_IsPackaged;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IIterator_1__Diagnostics_IProcessDiagnosticInfo_Base = interface(IInspectable)
  ['{A89B4418-4C3B-5F49-B957-785697C99ABF}']
    function get_Current: Diagnostics_IProcessDiagnosticInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDiagnostics_IProcessDiagnosticInfo): Cardinal; safecall;
    property Current: Diagnostics_IProcessDiagnosticInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IIterator_1__Diagnostics_IProcessDiagnosticInfo = interface(IIterator_1__Diagnostics_IProcessDiagnosticInfo_Base)
  ['{E762FF68-C456-5132-8750-6EE48B937BD2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IIterable_1__Diagnostics_IProcessDiagnosticInfo_Base = interface(IInspectable)
  ['{97B73627-B296-5076-B8CD-6BD8A240E966}']
    function First: IIterator_1__Diagnostics_IProcessDiagnosticInfo; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IIterable_1__Diagnostics_IProcessDiagnosticInfo = interface(IIterable_1__Diagnostics_IProcessDiagnosticInfo_Base)
  ['{957BC743-9FE0-5719-856A-BEFC3CDF91FC}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IVectorView_1__Diagnostics_IProcessDiagnosticInfo = interface(IInspectable)
  ['{BE0A1D23-95D6-50B7-B4B4-7BDAAB89D3C0}']
    function GetAt(index: Cardinal): Diagnostics_IProcessDiagnosticInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Diagnostics_IProcessDiagnosticInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDiagnostics_IProcessDiagnosticInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessDiagnosticInfoStatics
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_ProcessDiagnosticInfo)]
  Diagnostics_IProcessDiagnosticInfoStatics = interface(IInspectable)
  ['{2F41B260-B49F-428C-AA0E-84744F49CA95}']
    function GetForProcesses: IVectorView_1__Diagnostics_IProcessDiagnosticInfo; safecall;
    function GetForCurrentProcess: Diagnostics_IProcessDiagnosticInfo; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.IProcessDiagnosticInfoStatics2
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_ProcessDiagnosticInfo)]
  Diagnostics_IProcessDiagnosticInfoStatics2 = interface(IInspectable)
  ['{4A869897-9899-4A44-A29B-091663BE09B6}']
    function TryGetForProcessId(processId: Cardinal): Diagnostics_IProcessDiagnosticInfo; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.ISystemCpuUsageReport
  Diagnostics_ISystemCpuUsageReport = interface(IInspectable)
  ['{2C26D0B2-9483-4F62-AB57-82B29D9719B8}']
    function get_KernelTime: TimeSpan; safecall;
    function get_UserTime: TimeSpan; safecall;
    function get_IdleTime: TimeSpan; safecall;
    property IdleTime: TimeSpan read get_IdleTime;
    property KernelTime: TimeSpan read get_KernelTime;
    property UserTime: TimeSpan read get_UserTime;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.ISystemCpuUsage
  Diagnostics_ISystemCpuUsage = interface(IInspectable)
  ['{6037B3AC-02D6-4234-8362-7FE3ADC81F5F}']
    function GetReport: Diagnostics_ISystemCpuUsageReport; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.ISystemMemoryUsageReport
  Diagnostics_ISystemMemoryUsageReport = interface(IInspectable)
  ['{38663C87-2A9F-403A-BD19-2CF3E8169500}']
    function get_TotalPhysicalSizeInBytes: UInt64; safecall;
    function get_AvailableSizeInBytes: UInt64; safecall;
    function get_CommittedSizeInBytes: UInt64; safecall;
    property AvailableSizeInBytes: UInt64 read get_AvailableSizeInBytes;
    property CommittedSizeInBytes: UInt64 read get_CommittedSizeInBytes;
    property TotalPhysicalSizeInBytes: UInt64 read get_TotalPhysicalSizeInBytes;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.ISystemMemoryUsage
  Diagnostics_ISystemMemoryUsage = interface(IInspectable)
  ['{17FFC595-1702-49CF-AA27-2F0A32591404}']
    function GetReport: Diagnostics_ISystemMemoryUsageReport; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.ISystemDiagnosticInfo
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_SystemDiagnosticInfo)]
  Diagnostics_ISystemDiagnosticInfo = interface(IInspectable)
  ['{A290FE05-DFF3-407F-9A1B-0B2B317CA800}']
    function get_MemoryUsage: Diagnostics_ISystemMemoryUsage; safecall;
    function get_CpuUsage: Diagnostics_ISystemCpuUsage; safecall;
    property CpuUsage: Diagnostics_ISystemCpuUsage read get_CpuUsage;
    property MemoryUsage: Diagnostics_ISystemMemoryUsage read get_MemoryUsage;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.ISystemDiagnosticInfoStatics
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_SystemDiagnosticInfo)]
  Diagnostics_ISystemDiagnosticInfoStatics = interface(IInspectable)
  ['{D404AC21-FC7D-45F0-9A3F-39203AED9F7E}']
    function GetForCurrentSystem: Diagnostics_ISystemDiagnosticInfo; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.Telemetry.IPlatformTelemetryRegistrationResult
  Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult = interface(IInspectable)
  ['{4D8518AB-2292-49BD-A15A-3D71D2145112}']
    function get_Status: Diagnostics_Telemetry_PlatformTelemetryRegistrationStatus; safecall;
    property Status: Diagnostics_Telemetry_PlatformTelemetryRegistrationStatus read get_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.Telemetry.IPlatformTelemetryRegistrationSettings
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_Telemetry_PlatformTelemetryRegistrationSettings)]
  Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings = interface(IInspectable)
  ['{819A8582-CA19-415E-BB79-9C224BFA3A73}']
    function get_StorageSize: Cardinal; safecall;
    procedure put_StorageSize(value: Cardinal); safecall;
    function get_UploadQuotaSize: Cardinal; safecall;
    procedure put_UploadQuotaSize(value: Cardinal); safecall;
    property StorageSize: Cardinal read get_StorageSize write put_StorageSize;
    property UploadQuotaSize: Cardinal read get_UploadQuotaSize write put_UploadQuotaSize;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.Telemetry.IPlatformTelemetryClientStatics
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_Telemetry_PlatformTelemetryClient)]
  Diagnostics_Telemetry_IPlatformTelemetryClientStatics = interface(IInspectable)
  ['{9BF3F25D-D5C3-4EEA-8DBE-9C8DBB0D9D8F}']
    function Register(id: HSTRING): Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult; overload; safecall;
    function Register(id: HSTRING; settings: Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings): Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult; overload; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Guid>
  IIterable_1__TGuid_Base = interface(IInspectable)
  ['{F4CA3045-5DD7-54BE-982E-D88D8CA0876E}']
    function First: IIterator_1__TGuid; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Guid>
  IIterable_1__TGuid = interface(IIterable_1__TGuid_Base)
  ['{F4CA3045-5DD7-54BE-982E-D88D8CA0876E}']
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceRuntimeInfo
  Diagnostics_TraceReporting_IPlatformDiagnosticTraceRuntimeInfo = interface(IInspectable)
  ['{3D4D5E2D-01D8-4768-8554-1EB1CA610986}']
    function get_RuntimeFileTime: Int64; safecall;
    function get_EtwRuntimeFileTime: Int64; safecall;
    property EtwRuntimeFileTime: Int64 read get_EtwRuntimeFileTime;
    property RuntimeFileTime: Int64 read get_RuntimeFileTime;
  end;

  // UsedAPI Interface
  // Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo
  Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface(IInspectable)
  ['{F870ED97-D597-4BF7-88DC-CF5C7DC2A1D2}']
    function get_ScenarioId: TGuid; safecall;
    function get_ProfileHash: UInt64; safecall;
    function get_IsExclusive: Boolean; safecall;
    function get_IsAutoLogger: Boolean; safecall;
    function get_MaxTraceDurationFileTime: Int64; safecall;
    function get_Priority: Diagnostics_TraceReporting_PlatformDiagnosticTracePriority; safecall;
    property IsAutoLogger: Boolean read get_IsAutoLogger;
    property IsExclusive: Boolean read get_IsExclusive;
    property MaxTraceDurationFileTime: Int64 read get_MaxTraceDurationFileTime;
    property Priority: Diagnostics_TraceReporting_PlatformDiagnosticTracePriority read get_Priority;
    property ProfileHash: UInt64 read get_ProfileHash;
    property ScenarioId: TGuid read get_ScenarioId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IIterator_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo_Base = interface(IInspectable)
  ['{1AF4598D-98BB-5E51-842B-CF691925B6C2}']
    function get_Current: Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDiagnostics_TraceReporting_IPlatformDiagnosticTraceInfo): Cardinal; safecall;
    property Current: Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IIterator_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface(IIterator_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo_Base)
  ['{7562E3E2-C8A6-51DD-BD63-8B2C09552F81}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IIterable_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo_Base = interface(IInspectable)
  ['{ECB0C107-C97B-52FE-A5E6-A33E93493769}']
    function First: IIterator_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IIterable_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface(IIterable_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo_Base)
  ['{7DFED320-640F-5C13-8250-0BAA9D96750C}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticTraceInfo>
  IVectorView_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo = interface(IInspectable)
  ['{23E1B446-6D86-59EA-BEF4-56662F7A131C}']
    function GetAt(index: Cardinal): Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDiagnostics_TraceReporting_IPlatformDiagnosticTraceInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticActionsStatics
  [WinRTClassNameAttribute(SWindows_System_Diagnostics_TraceReporting_PlatformDiagnosticActions)]
  Diagnostics_TraceReporting_IPlatformDiagnosticActionsStatics = interface(IInspectable)
  ['{C1145CFA-9292-4267-890A-9EA3ED072312}']
    function IsScenarioEnabled(scenarioId: TGuid): Boolean; safecall;
    function TryEscalateScenario(scenarioId: TGuid; escalationType: Diagnostics_TraceReporting_PlatformDiagnosticEscalationType; outputDirectory: HSTRING; timestampOutputDirectory: Boolean; forceEscalationUpload: Boolean; triggers: IMapView_2__HSTRING__HSTRING): Boolean; safecall;
    function DownloadLatestSettingsForNamespace(partner: HSTRING; feature: HSTRING; isScenarioNamespace: Boolean; downloadOverCostedNetwork: Boolean; downloadOverBattery: Boolean): Diagnostics_TraceReporting_PlatformDiagnosticActionState; safecall;
    function GetActiveScenarioList: IVectorView_1__TGuid; safecall;
    function ForceUpload(latency: Diagnostics_TraceReporting_PlatformDiagnosticEventBufferLatencies; uploadOverCostedNetwork: Boolean; uploadOverBattery: Boolean): Diagnostics_TraceReporting_PlatformDiagnosticActionState; safecall;
    function IsTraceRunning(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType; scenarioId: TGuid; traceProfileHash: UInt64): Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotState; safecall;
    function GetActiveTraceRuntime(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType): Diagnostics_TraceReporting_IPlatformDiagnosticTraceRuntimeInfo; safecall;
    function GetKnownTraceList(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType): IVectorView_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Display.IDisplayRequest
  Display_IDisplayRequest = interface(IInspectable)
  ['{E5732044-F49F-4B60-8DD4-5E7E3A632AC0}']
    procedure RequestActive; safecall;
    procedure RequestRelease; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupBackgroundTaskReport
  IAppResourceGroupBackgroundTaskReport = interface(IInspectable)
  ['{2566E74E-B05D-40C2-9DC1-1A4F039EA120}']
    function get_TaskId: TGuid; safecall;
    function get_Name: HSTRING; safecall;
    function get_Trigger: HSTRING; safecall;
    function get_EntryPoint: HSTRING; safecall;
    property EntryPoint: HSTRING read get_EntryPoint;
    property Name: HSTRING read get_Name;
    property TaskId: TGuid read get_TaskId;
    property Trigger: HSTRING read get_Trigger;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IIterator_1__IAppResourceGroupBackgroundTaskReport_Base = interface(IInspectable)
  ['{00C2180A-08E5-5EED-A08D-56A356CC004F}']
    function get_Current: IAppResourceGroupBackgroundTaskReport; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIAppResourceGroupBackgroundTaskReport): Cardinal; safecall;
    property Current: IAppResourceGroupBackgroundTaskReport read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IIterator_1__IAppResourceGroupBackgroundTaskReport = interface(IIterator_1__IAppResourceGroupBackgroundTaskReport_Base)
  ['{C4E4BA89-4E75-517B-AB55-604D817607BC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IIterable_1__IAppResourceGroupBackgroundTaskReport_Base = interface(IInspectable)
  ['{3E7DCBCA-1804-5672-AD3B-58D944BB044C}']
    function First: IIterator_1__IAppResourceGroupBackgroundTaskReport; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IIterable_1__IAppResourceGroupBackgroundTaskReport = interface(IIterable_1__IAppResourceGroupBackgroundTaskReport_Base)
  ['{791F83F7-F902-5D88-9467-3B9F82BA82E9}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IVectorView_1__IAppResourceGroupBackgroundTaskReport = interface(IInspectable)
  ['{8A469227-CBCA-5739-A5FC-6FDE622585BE}']
    function GetAt(index: Cardinal): IAppResourceGroupBackgroundTaskReport; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IAppResourceGroupBackgroundTaskReport; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppResourceGroupBackgroundTaskReport): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IVector_1__IAppResourceGroupBackgroundTaskReport_Base = interface(IInspectable)
  ['{80F1820A-DB02-5CB2-A128-5172151D1444}']
    function GetAt(index: Cardinal): IAppResourceGroupBackgroundTaskReport; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IAppResourceGroupBackgroundTaskReport; safecall;
    function IndexOf(value: IAppResourceGroupBackgroundTaskReport; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IAppResourceGroupBackgroundTaskReport); safecall;
    procedure InsertAt(index: Cardinal; value: IAppResourceGroupBackgroundTaskReport); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IAppResourceGroupBackgroundTaskReport); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppResourceGroupBackgroundTaskReport): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIAppResourceGroupBackgroundTaskReport); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppResourceGroupBackgroundTaskReport>
  IVector_1__IAppResourceGroupBackgroundTaskReport = interface(IVector_1__IAppResourceGroupBackgroundTaskReport_Base)
  ['{84EEBBBD-2840-534E-9069-537987B62DF6}']
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupMemoryReport
  IAppResourceGroupMemoryReport = interface(IInspectable)
  ['{2C8C06B1-7DB1-4C51-A225-7FAE2D49E431}']
    function get_CommitUsageLimit: UInt64; safecall;
    function get_CommitUsageLevel: AppMemoryUsageLevel; safecall;
    function get_PrivateCommitUsage: UInt64; safecall;
    function get_TotalCommitUsage: UInt64; safecall;
    property CommitUsageLevel: AppMemoryUsageLevel read get_CommitUsageLevel;
    property CommitUsageLimit: UInt64 read get_CommitUsageLimit;
    property PrivateCommitUsage: UInt64 read get_PrivateCommitUsage;
    property TotalCommitUsage: UInt64 read get_TotalCommitUsage;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IVector_1__Diagnostics_IProcessDiagnosticInfo_Base = interface(IInspectable)
  ['{F62E2D01-C1DD-5B60-B5DA-16518CBA0BB0}']
    function GetAt(index: Cardinal): Diagnostics_IProcessDiagnosticInfo; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Diagnostics_IProcessDiagnosticInfo; safecall;
    function IndexOf(value: Diagnostics_IProcessDiagnosticInfo; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Diagnostics_IProcessDiagnosticInfo); safecall;
    procedure InsertAt(index: Cardinal; value: Diagnostics_IProcessDiagnosticInfo); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Diagnostics_IProcessDiagnosticInfo); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDiagnostics_IProcessDiagnosticInfo): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDiagnostics_IProcessDiagnosticInfo); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.System.Diagnostics.IProcessDiagnosticInfo>
  IVector_1__Diagnostics_IProcessDiagnosticInfo = interface(IVector_1__Diagnostics_IProcessDiagnosticInfo_Base)
  ['{8411BB34-D0B6-5D65-A543-D1C3F6EE8DB1}']
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupStateReport
  IAppResourceGroupStateReport = interface(IInspectable)
  ['{52849F18-2F70-4236-AB40-D04DB0C7B931}']
    function get_ExecutionState: AppResourceGroupExecutionState; safecall;
    function get_EnergyQuotaState: AppResourceGroupEnergyQuotaState; safecall;
    property EnergyQuotaState: AppResourceGroupEnergyQuotaState read get_EnergyQuotaState;
    property ExecutionState: AppResourceGroupExecutionState read get_ExecutionState;
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupInfo
  IAppResourceGroupInfo = interface(IInspectable)
  ['{B913F77A-E807-49F4-845E-7B8BDCFE8EE7}']
    function get_InstanceId: TGuid; safecall;
    function get_IsShared: Boolean; safecall;
    function GetBackgroundTaskReports: IVector_1__IAppResourceGroupBackgroundTaskReport; safecall;
    function GetMemoryReport: IAppResourceGroupMemoryReport; safecall;
    function GetProcessDiagnosticInfos: IVector_1__Diagnostics_IProcessDiagnosticInfo; safecall;
    function GetStateReport: IAppResourceGroupStateReport; safecall;
    property InstanceId: TGuid read get_InstanceId;
    property IsShared: Boolean read get_IsShared;
  end;

  // UsedAPI Interface
  // Windows.System.IAppActivationResult
  IAppActivationResult = interface(IInspectable)
  ['{6B528900-F46E-4EB0-AA6C-38AF557CF9ED}']
    function get_ExtendedError: HRESULT; safecall;
    function get_AppResourceGroupInfo: IAppResourceGroupInfo; safecall;
    property AppResourceGroupInfo: IAppResourceGroupInfo read get_AppResourceGroupInfo;
    property ExtendedError: HRESULT read get_ExtendedError;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppResourceGroupInfo>
  IIterator_1__IAppResourceGroupInfo_Base = interface(IInspectable)
  ['{E44D5851-E4BC-50B9-A898-6903137D8A99}']
    function get_Current: IAppResourceGroupInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIAppResourceGroupInfo): Cardinal; safecall;
    property Current: IAppResourceGroupInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppResourceGroupInfo>
  IIterator_1__IAppResourceGroupInfo = interface(IIterator_1__IAppResourceGroupInfo_Base)
  ['{3DA7A949-D470-566C-86B4-F7BA4F4F52BA}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppResourceGroupInfo>
  IIterable_1__IAppResourceGroupInfo_Base = interface(IInspectable)
  ['{8B640948-C0D3-5B7E-A99C-5956190D5408}']
    function First: IIterator_1__IAppResourceGroupInfo; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppResourceGroupInfo>
  IIterable_1__IAppResourceGroupInfo = interface(IIterable_1__IAppResourceGroupInfo_Base)
  ['{814734E8-4E80-5E9C-B35F-1739D873CEF1}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppResourceGroupInfo>
  IVectorView_1__IAppResourceGroupInfo = interface(IInspectable)
  ['{E7985B3F-72DA-5EC5-A5F9-9E60EFB9F95D}']
    function GetAt(index: Cardinal): IAppResourceGroupInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IAppResourceGroupInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppResourceGroupInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppResourceGroupInfo>
  IVector_1__IAppResourceGroupInfo_Base = interface(IInspectable)
  ['{BD7D69A0-0D57-5148-8B23-49A08654F0C7}']
    function GetAt(index: Cardinal): IAppResourceGroupInfo; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IAppResourceGroupInfo; safecall;
    function IndexOf(value: IAppResourceGroupInfo; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IAppResourceGroupInfo); safecall;
    procedure InsertAt(index: Cardinal; value: IAppResourceGroupInfo); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IAppResourceGroupInfo); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppResourceGroupInfo): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIAppResourceGroupInfo); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppResourceGroupInfo>
  IVector_1__IAppResourceGroupInfo = interface(IVector_1__IAppResourceGroupInfo_Base)
  ['{066D7BEA-9A12-57CA-B4C6-3B82F19202D4}']
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupInfoWatcherEventArgs
  IAppResourceGroupInfoWatcherEventArgs = interface(IInspectable)
  ['{7A787637-6302-4D2F-BF89-1C12D0B2A6B9}']
    function get_AppDiagnosticInfos: IVectorView_1__IAppDiagnosticInfo; safecall;
    function get_AppResourceGroupInfo: IAppResourceGroupInfo; safecall;
    property AppDiagnosticInfos: IVectorView_1__IAppDiagnosticInfo read get_AppDiagnosticInfos;
    property AppResourceGroupInfo: IAppResourceGroupInfo read get_AppResourceGroupInfo;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Windows.System.IAppResourceGroupInfoWatcherEventArgs>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs_Delegate_Base = interface(IUnknown)
  ['{A7E14BAE-C778-5661-A41C-1AC3AC635F79}']
    procedure Invoke(sender: IAppResourceGroupInfoWatcher; args: IAppResourceGroupInfoWatcherEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Windows.System.IAppResourceGroupInfoWatcherEventArgs>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs = interface(TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs_Delegate_Base)
  ['{35349A4B-786E-5C09-93E7-A96B6ACC4FEC}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Object>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable_Delegate_Base = interface(IUnknown)
  ['{E23A2901-197A-5867-8728-9C9DB9498D76}']
    procedure Invoke(sender: IAppResourceGroupInfoWatcher; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Object>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable = interface(TypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable_Delegate_Base)
  ['{1EA21D71-2F41-5B6D-BCE2-D833782A47D7}']
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs
  IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs = interface(IInspectable)
  ['{1BDBEDD7-FEE6-4FD4-98DD-E92A2CC299F3}']
    function get_AppDiagnosticInfos: IVectorView_1__IAppDiagnosticInfo; safecall;
    function get_AppResourceGroupInfo: IAppResourceGroupInfo; safecall;
    property AppDiagnosticInfos: IVectorView_1__IAppDiagnosticInfo read get_AppDiagnosticInfos;
    property AppResourceGroupInfo: IAppResourceGroupInfo read get_AppResourceGroupInfo;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Windows.System.IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{93F9724C-17F8-5DF5-A6CF-2F0AB90C0A27}']
    procedure Invoke(sender: IAppResourceGroupInfoWatcher; args: IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppResourceGroupInfoWatcher,Windows.System.IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs>
  TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs = interface(TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs_Delegate_Base)
  ['{9BC01C3F-3C2C-5ABF-AF45-B4FF1B0229B6}']
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupInfoWatcher
  IAppResourceGroupInfoWatcher = interface(IInspectable)
  ['{D9B0A0FD-6E5A-4C72-8B17-09FEC4A212BD}']
    function add_Added(handler: TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs): EventRegistrationToken; safecall;
    procedure remove_Added(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherEventArgs): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    function add_EnumerationCompleted(handler: TypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(token: EventRegistrationToken); safecall;
    function add_Stopped(handler: TypedEventHandler_2__IAppResourceGroupInfoWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Stopped(token: EventRegistrationToken); safecall;
    function add_ExecutionStateChanged(handler: TypedEventHandler_2__IAppResourceGroupInfoWatcher__IAppResourceGroupInfoWatcherExecutionStateChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ExecutionStateChanged(token: EventRegistrationToken); safecall;
    function get_Status: AppResourceGroupInfoWatcherStatus; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    property Status: AppResourceGroupInfoWatcherStatus read get_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IAppDiagnosticInfo2
  IAppDiagnosticInfo2 = interface(IInspectable)
  ['{DF46FBD7-191A-446C-9473-8FBC2374A354}']
    function GetResourceGroups: IVector_1__IAppResourceGroupInfo; safecall;
    function CreateResourceGroupWatcher: IAppResourceGroupInfoWatcher; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IAppActivationResult>
  AsyncOperationCompletedHandler_1__IAppActivationResult = interface(IUnknown)
  ['{EBA1FBF3-1E05-5244-8C1E-0028555DBCC3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IAppActivationResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.IAppActivationResult>
  IAsyncOperation_1__IAppActivationResult = interface(IInspectable)
  ['{64381F53-AD0B-501D-A353-1C9B685B145B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IAppActivationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IAppActivationResult; safecall;
    function GetResults: IAppActivationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IAppActivationResult read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IAppDiagnosticInfo3
  IAppDiagnosticInfo3 = interface(IInspectable)
  ['{C895C63D-DD61-4C65-BABD-81A10B4F9815}']
    function LaunchAsync: IAsyncOperation_1__IAppActivationResult; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>>
  AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo_Delegate_Base = interface(IUnknown)
  ['{CC3F5F7E-4160-567F-A0F6-AA9AEB187AF3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>>
  AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo = interface(AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo_Delegate_Base)
  ['{DD0A962C-4758-5704-874F-F9A0B4569AB5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>>
  IAsyncOperation_1__IVector_1__IAppDiagnosticInfo_Base = interface(IInspectable)
  ['{D6A9D3B9-F63C-59BE-A096-3E9557C41182}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo; safecall;
    function GetResults: IVector_1__IAppDiagnosticInfo; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVector_1__IAppDiagnosticInfo read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppDiagnosticInfo>>
  IAsyncOperation_1__IVector_1__IAppDiagnosticInfo = interface(IAsyncOperation_1__IVector_1__IAppDiagnosticInfo_Base)
  ['{6D5AABF2-C335-5E91-B107-FF73B353250F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IAppDiagnosticInfoStatics
  [WinRTClassNameAttribute(SWindows_System_AppDiagnosticInfo)]
  IAppDiagnosticInfoStatics = interface(IInspectable)
  ['{CE6925BF-10CA-40C8-A9CA-C5C96501866E}']
    function RequestInfoAsync: IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.IAppDiagnosticInfoWatcherEventArgs
  IAppDiagnosticInfoWatcherEventArgs = interface(IInspectable)
  ['{7017C716-E1DA-4C65-99DF-046DFF5BE71A}']
    function get_AppDiagnosticInfo: IAppDiagnosticInfo; safecall;
    property AppDiagnosticInfo: IAppDiagnosticInfo read get_AppDiagnosticInfo;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppDiagnosticInfoWatcher,Windows.System.IAppDiagnosticInfoWatcherEventArgs>
  TypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs_Delegate_Base = interface(IUnknown)
  ['{7C8C6F9F-D6BF-5566-B013-39C141E0FF8C}']
    procedure Invoke(sender: IAppDiagnosticInfoWatcher; args: IAppDiagnosticInfoWatcherEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppDiagnosticInfoWatcher,Windows.System.IAppDiagnosticInfoWatcherEventArgs>
  TypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs = interface(TypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs_Delegate_Base)
  ['{04C9F500-14BC-5CF5-B34D-339156E07E21}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppDiagnosticInfoWatcher,Object>
  TypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable_Delegate_Base = interface(IUnknown)
  ['{895DEE2F-E0F2-5304-A40E-1C67A2C058AA}']
    procedure Invoke(sender: IAppDiagnosticInfoWatcher; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IAppDiagnosticInfoWatcher,Object>
  TypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable = interface(TypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable_Delegate_Base)
  ['{BFEA2810-7690-5913-A3AF-EC1B829F2DE3}']
  end;

  // UsedAPI Interface
  // Windows.System.IAppDiagnosticInfoWatcher
  IAppDiagnosticInfoWatcher = interface(IInspectable)
  ['{75575070-01D3-489A-9325-52F9CC6EDE0A}']
    function add_Added(handler: TypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs): EventRegistrationToken; safecall;
    procedure remove_Added(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__IAppDiagnosticInfoWatcher__IAppDiagnosticInfoWatcherEventArgs): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    function add_EnumerationCompleted(handler: TypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(token: EventRegistrationToken); safecall;
    function add_Stopped(handler: TypedEventHandler_2__IAppDiagnosticInfoWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Stopped(token: EventRegistrationToken); safecall;
    function get_Status: AppDiagnosticInfoWatcherStatus; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    property Status: AppDiagnosticInfoWatcherStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.DiagnosticAccessStatus>
  AsyncOperationCompletedHandler_1__DiagnosticAccessStatus_Delegate_Base = interface(IUnknown)
  ['{5D1302D7-5497-5A92-BF43-EB8B50679AAB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__DiagnosticAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.DiagnosticAccessStatus>
  AsyncOperationCompletedHandler_1__DiagnosticAccessStatus = interface(AsyncOperationCompletedHandler_1__DiagnosticAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.DiagnosticAccessStatus>
  IAsyncOperation_1__DiagnosticAccessStatus_Base = interface(IInspectable)
  ['{61C11BBE-2618-588A-A7CA-F60691272324}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__DiagnosticAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__DiagnosticAccessStatus; safecall;
    function GetResults: DiagnosticAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__DiagnosticAccessStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.DiagnosticAccessStatus>
  IAsyncOperation_1__DiagnosticAccessStatus = interface(IAsyncOperation_1__DiagnosticAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IAppDiagnosticInfoStatics2
  [WinRTClassNameAttribute(SWindows_System_AppDiagnosticInfo)]
  IAppDiagnosticInfoStatics2 = interface(IInspectable)
  ['{05B24B86-1000-4C90-BB9F-7235071C50FE}']
    function CreateWatcher: IAppDiagnosticInfoWatcher; safecall;
    function RequestAccessAsync: IAsyncOperation_1__DiagnosticAccessStatus; safecall;
    function RequestInfoForPackageAsync(packageFamilyName: HSTRING): IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; safecall;
    function RequestInfoForAppAsync: IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; overload; safecall;
    function RequestInfoForAppAsync(appUserModelId: HSTRING): IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.IAppExecutionStateChangeResult
  IAppExecutionStateChangeResult = interface(IInspectable)
  ['{6F039BF0-F91B-4DF8-AE77-3033CCB69114}']
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
  end;

  // UsedAPI Interface
  // Windows.System.IAppMemoryReport
  IAppMemoryReport = interface(IInspectable)
  ['{6D65339B-4D6F-45BC-9C5E-E49B3FF2758D}']
    function get_PrivateCommitUsage: UInt64; safecall;
    function get_PeakPrivateCommitUsage: UInt64; safecall;
    function get_TotalCommitUsage: UInt64; safecall;
    function get_TotalCommitLimit: UInt64; safecall;
    property PeakPrivateCommitUsage: UInt64 read get_PeakPrivateCommitUsage;
    property PrivateCommitUsage: UInt64 read get_PrivateCommitUsage;
    property TotalCommitLimit: UInt64 read get_TotalCommitLimit;
    property TotalCommitUsage: UInt64 read get_TotalCommitUsage;
  end;

  // UsedAPI Interface
  // Windows.System.IAppMemoryReport2
  IAppMemoryReport2 = interface(IInspectable)
  ['{5F7F3738-51B7-42DC-B7ED-79BA46D28857}']
    function get_ExpectedTotalCommitLimit: UInt64; safecall;
    property ExpectedTotalCommitLimit: UInt64 read get_ExpectedTotalCommitLimit;
  end;

  // UsedAPI Interface
  // Windows.System.IAppMemoryUsageLimitChangingEventArgs
  IAppMemoryUsageLimitChangingEventArgs = interface(IInspectable)
  ['{79F86664-FECA-4DA5-9E40-2BC63EFDC979}']
    function get_OldLimit: UInt64; safecall;
    function get_NewLimit: UInt64; safecall;
    property NewLimit: UInt64 read get_NewLimit;
    property OldLimit: UInt64 read get_OldLimit;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IAppExecutionStateChangeResult>
  AsyncOperationCompletedHandler_1__IAppExecutionStateChangeResult = interface(IUnknown)
  ['{90C0B271-7CB2-5B65-9EDA-AA9F7FA1321E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IAppExecutionStateChangeResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.IAppExecutionStateChangeResult>
  IAsyncOperation_1__IAppExecutionStateChangeResult = interface(IInspectable)
  ['{2B3D708D-2EEE-5A22-8147-B2032A8AF264}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IAppExecutionStateChangeResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IAppExecutionStateChangeResult; safecall;
    function GetResults: IAppExecutionStateChangeResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IAppExecutionStateChangeResult read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.System.IAppResourceGroupInfo2
  IAppResourceGroupInfo2 = interface(IInspectable)
  ['{EE9B236D-D305-4D6B-92F7-6AFDAD72DEDC}']
    function StartSuspendAsync: IAsyncOperation_1__IAppExecutionStateChangeResult; safecall;
    function StartResumeAsync: IAsyncOperation_1__IAppExecutionStateChangeResult; safecall;
    function StartTerminateAsync: IAsyncOperation_1__IAppExecutionStateChangeResult; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.IAppUriHandlerHost
  IAppUriHandlerHost = interface(IInspectable)
  ['{5D50CAC5-92D2-5409-B56F-7F73E10EA4C3}']
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    property Name: HSTRING read get_Name write put_Name;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IAppUriHandlerHost>
  IIterator_1__IAppUriHandlerHost = interface(IInspectable)
  ['{9431179C-CE04-57CE-9F9D-7C206310D816}']
    function get_Current: IAppUriHandlerHost; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIAppUriHandlerHost): Cardinal; safecall;
    property Current: IAppUriHandlerHost read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IAppUriHandlerHost>
  IIterable_1__IAppUriHandlerHost = interface(IInspectable)
  ['{C436A7C8-87EC-58B0-87EC-928FE5B39890}']
    function First: IIterator_1__IAppUriHandlerHost; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IAppUriHandlerHost>
  IVectorView_1__IAppUriHandlerHost = interface(IInspectable)
  ['{929BC521-A4E5-5253-BA8E-B9887CCFA7E2}']
    function GetAt(index: Cardinal): IAppUriHandlerHost; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IAppUriHandlerHost; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppUriHandlerHost): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.System.IAppUriHandlerHost>
  IVector_1__IAppUriHandlerHost = interface(IInspectable)
  ['{A0CBB390-22DA-5A6C-89DE-12C36DB4AA4A}']
    function GetAt(index: Cardinal): IAppUriHandlerHost; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IAppUriHandlerHost; safecall;
    function IndexOf(value: IAppUriHandlerHost; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IAppUriHandlerHost); safecall;
    procedure InsertAt(index: Cardinal; value: IAppUriHandlerHost); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IAppUriHandlerHost); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAppUriHandlerHost): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIAppUriHandlerHost); safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppUriHandlerHost>>
  AsyncOperationCompletedHandler_1__IVector_1__IAppUriHandlerHost = interface(IUnknown)
  ['{BAB97844-D5F5-556B-83F9-FA3491624B0A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVector_1__IAppUriHandlerHost; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVector`1<Windows.System.IAppUriHandlerHost>>
  IAsyncOperation_1__IVector_1__IAppUriHandlerHost = interface(IInspectable)
  ['{33A878A6-8A74-5956-A3D9-5ADE0F7D515B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVector_1__IAppUriHandlerHost); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVector_1__IAppUriHandlerHost; safecall;
    function GetResults: IVector_1__IAppUriHandlerHost; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVector_1__IAppUriHandlerHost read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.System.IAppUriHandlerRegistration
  IAppUriHandlerRegistration = interface(IInspectable)
  ['{6F73AEB1-4569-5C3F-9BA0-99123EEA32C3}']
    function get_Name: HSTRING; safecall;
    function get_User: IUser; safecall;
    function GetAppAddedHostsAsync: IAsyncOperation_1__IVector_1__IAppUriHandlerHost; safecall;
    function SetAppAddedHostsAsync(hosts: IIterable_1__IAppUriHandlerHost): IAsyncAction; safecall;
    property Name: HSTRING read get_Name;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.System.IAppUriHandlerRegistrationManager
  IAppUriHandlerRegistrationManager = interface(IInspectable)
  ['{E62C9A52-AC94-5750-AC1B-6CFB6F250263}']
    function get_User: IUser; safecall;
    function TryGetRegistration(name: HSTRING): IAppUriHandlerRegistration; safecall;
    property User: IUser read get_User;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IDateTimeSettingsStatics
  [WinRTClassNameAttribute(SWindows_System_DateTimeSettings)]
  IDateTimeSettingsStatics = interface(IInspectable)
  ['{5D2150D1-47EE-48AB-A52B-9F1954278D82}']
    procedure SetSystemDateTime(utcDateTime: DateTime); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IDispatcherQueue2
  IDispatcherQueue2 = interface(IInspectable)
  ['{C822C647-30EF-506E-BD1E-A647AE6675FF}']
    function get_HasThreadAccess: Boolean; safecall;
    property HasThreadAccess: Boolean read get_HasThreadAccess;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IDispatcherQueueController
  [WinRTClassNameAttribute(SWindows_System_DispatcherQueueController)]
  IDispatcherQueueController = interface(IInspectable)
  ['{22F34E66-50DB-4E36-A98D-61C01B384D20}']
    function get_DispatcherQueue: IDispatcherQueue; safecall;
    function ShutdownQueueAsync: IAsyncAction; safecall;
    property DispatcherQueue: IDispatcherQueue read get_DispatcherQueue;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IDispatcherQueueControllerStatics
  [WinRTClassNameAttribute(SWindows_System_DispatcherQueueController)]
  IDispatcherQueueControllerStatics = interface(IInspectable)
  ['{0A6C98E0-5198-49A2-A313-3F70D1F13C27}']
    function CreateOnDedicatedThread: IDispatcherQueueController; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IDispatcherQueueStatics
  [WinRTClassNameAttribute(SWindows_System_DispatcherQueue)]
  IDispatcherQueueStatics = interface(IInspectable)
  ['{A96D83D7-9371-4517-9245-D0824AC12C74}']
    function GetForCurrentThread: IDispatcherQueue; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.IFolderLauncherOptions
  IFolderLauncherOptions = interface(IInspectable)
  ['{BB91C27D-6B87-432A-BD04-776C6F5FB2AB}']
    function get_ItemsToSelect: IVector_1__IStorageItem; safecall;
    property ItemsToSelect: IVector_1__IStorageItem read get_ItemsToSelect;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IKnownUserPropertiesStatics
  [WinRTClassNameAttribute(SWindows_System_KnownUserProperties)]
  IKnownUserPropertiesStatics = interface(IInspectable)
  ['{7755911A-70C5-48E5-B637-5BA3441E4EE4}']
    function get_DisplayName: HSTRING; safecall;
    function get_FirstName: HSTRING; safecall;
    function get_LastName: HSTRING; safecall;
    function get_ProviderName: HSTRING; safecall;
    function get_AccountName: HSTRING; safecall;
    function get_GuestHost: HSTRING; safecall;
    function get_PrincipalName: HSTRING; safecall;
    function get_DomainName: HSTRING; safecall;
    function get_SessionInitiationProtocolUri: HSTRING; safecall;
    property AccountName: HSTRING read get_AccountName;
    property DisplayName: HSTRING read get_DisplayName;
    property DomainName: HSTRING read get_DomainName;
    property FirstName: HSTRING read get_FirstName;
    property GuestHost: HSTRING read get_GuestHost;
    property LastName: HSTRING read get_LastName;
    property PrincipalName: HSTRING read get_PrincipalName;
    property ProviderName: HSTRING read get_ProviderName;
    property SessionInitiationProtocolUri: HSTRING read get_SessionInitiationProtocolUri;
  end;

  // UsedAPI Interface
  // Windows.System.ILaunchUriResult
  ILaunchUriResult = interface(IInspectable)
  ['{EC27A8DF-F6D5-45CA-913A-70A40C5C8221}']
    function get_Status: LaunchUriStatus; safecall;
    function get_Result: IPropertySet; safecall;
    property Result: IPropertySet read get_Result;
    property Status: LaunchUriStatus read get_Status;
  end;

  // UsedAPI Interface
  // Windows.System.ILauncherUIOptions
  ILauncherUIOptions = interface(IInspectable)
  ['{1B25DA6E-8AA6-41E9-8251-4165F5985F49}']
    function get_InvocationPoint: IReference_1__Point; safecall;
    procedure put_InvocationPoint(value: IReference_1__Point); safecall;
    function get_SelectionRect: IReference_1__Rect; safecall;
    procedure put_SelectionRect(value: IReference_1__Rect); safecall;
    function get_PreferredPlacement: Popups_Placement; safecall;
    procedure put_PreferredPlacement(value: Popups_Placement); safecall;
    property InvocationPoint: IReference_1__Point read get_InvocationPoint write put_InvocationPoint;
    property PreferredPlacement: Popups_Placement read get_PreferredPlacement write put_PreferredPlacement;
    property SelectionRect: IReference_1__Rect read get_SelectionRect write put_SelectionRect;
  end;

  // UsedAPI Interface
  // Windows.System.ILauncherOptions
  ILauncherOptions = interface(IInspectable)
  ['{BAFA21D8-B071-4CD8-853E-341203E557D3}']
    function get_TreatAsUntrusted: Boolean; safecall;
    procedure put_TreatAsUntrusted(value: Boolean); safecall;
    function get_DisplayApplicationPicker: Boolean; safecall;
    procedure put_DisplayApplicationPicker(value: Boolean); safecall;
    function get_UI: ILauncherUIOptions; safecall;
    function get_PreferredApplicationPackageFamilyName: HSTRING; safecall;
    procedure put_PreferredApplicationPackageFamilyName(value: HSTRING); safecall;
    function get_PreferredApplicationDisplayName: HSTRING; safecall;
    procedure put_PreferredApplicationDisplayName(value: HSTRING); safecall;
    function get_FallbackUri: IUriRuntimeClass; safecall;
    procedure put_FallbackUri(value: IUriRuntimeClass); safecall;
    function get_ContentType: HSTRING; safecall;
    procedure put_ContentType(value: HSTRING); safecall;
    property ContentType: HSTRING read get_ContentType write put_ContentType;
    property DisplayApplicationPicker: Boolean read get_DisplayApplicationPicker write put_DisplayApplicationPicker;
    property FallbackUri: IUriRuntimeClass read get_FallbackUri write put_FallbackUri;
    property PreferredApplicationDisplayName: HSTRING read get_PreferredApplicationDisplayName write put_PreferredApplicationDisplayName;
    property PreferredApplicationPackageFamilyName: HSTRING read get_PreferredApplicationPackageFamilyName write put_PreferredApplicationPackageFamilyName;
    property TreatAsUntrusted: Boolean read get_TreatAsUntrusted write put_TreatAsUntrusted;
    property UI: ILauncherUIOptions read get_UI;
  end;

  // UsedAPI Interface
  // Windows.System.ILauncherOptions2
  ILauncherOptions2 = interface(IInspectable)
  ['{3BA08EB4-6E40-4DCE-A1A3-2F53950AFB49}']
    function get_TargetApplicationPackageFamilyName: HSTRING; safecall;
    procedure put_TargetApplicationPackageFamilyName(value: HSTRING); safecall;
    function get_NeighboringFilesQuery: Search_IStorageFileQueryResult; safecall;
    procedure put_NeighboringFilesQuery(value: Search_IStorageFileQueryResult); safecall;
    property NeighboringFilesQuery: Search_IStorageFileQueryResult read get_NeighboringFilesQuery write put_NeighboringFilesQuery;
    property TargetApplicationPackageFamilyName: HSTRING read get_TargetApplicationPackageFamilyName write put_TargetApplicationPackageFamilyName;
  end;

  // UsedAPI Interface
  // Windows.System.ILauncherOptions3
  ILauncherOptions3 = interface(IInspectable)
  ['{F0770655-4B63-4E3A-9107-4E687841923A}']
    function get_IgnoreAppUriHandlers: Boolean; safecall;
    procedure put_IgnoreAppUriHandlers(value: Boolean); safecall;
    property IgnoreAppUriHandlers: Boolean read get_IgnoreAppUriHandlers write put_IgnoreAppUriHandlers;
  end;

  // UsedAPI Interface
  // Windows.System.ILauncherOptions4
  ILauncherOptions4 = interface(IInspectable)
  ['{EF6FD10E-E6FB-4814-A44E-57E8B9D9A01B}']
    function get_LimitPickerToCurrentAppAndAppUriHandlers: Boolean; safecall;
    procedure put_LimitPickerToCurrentAppAndAppUriHandlers(value: Boolean); safecall;
    property LimitPickerToCurrentAppAndAppUriHandlers: Boolean read get_LimitPickerToCurrentAppAndAppUriHandlers write put_LimitPickerToCurrentAppAndAppUriHandlers;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.ILaunchUriResult>
  AsyncOperationCompletedHandler_1__ILaunchUriResult_Delegate_Base = interface(IUnknown)
  ['{70A97BF8-E0A5-59BB-9174-812A131D85A0}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__ILaunchUriResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.ILaunchUriResult>
  AsyncOperationCompletedHandler_1__ILaunchUriResult = interface(AsyncOperationCompletedHandler_1__ILaunchUriResult_Delegate_Base)
  ['{BD70A9E4-87D5-5255-959E-50CAB9725561}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.ILaunchUriResult>
  IAsyncOperation_1__ILaunchUriResult_Base = interface(IInspectable)
  ['{7F97FC15-1CD6-54B7-A290-ACB60DBA81A1}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__ILaunchUriResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__ILaunchUriResult; safecall;
    function GetResults: ILaunchUriResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__ILaunchUriResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.ILaunchUriResult>
  IAsyncOperation_1__ILaunchUriResult = interface(IAsyncOperation_1__ILaunchUriResult_Base)
  ['{82847C29-EA1C-5C0C-8FAB-362F9DF7BE07}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.LaunchQuerySupportStatus>
  AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus_Delegate_Base = interface(IUnknown)
  ['{198CAC52-ABCD-5529-933F-071CC93FD635}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__LaunchQuerySupportStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.LaunchQuerySupportStatus>
  AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus = interface(AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.LaunchQuerySupportStatus>
  IAsyncOperation_1__LaunchQuerySupportStatus_Base = interface(IInspectable)
  ['{E7539992-2220-5D2D-82C4-3D44F8750D91}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus; safecall;
    function GetResults: LaunchQuerySupportStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__LaunchQuerySupportStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.LaunchQuerySupportStatus>
  IAsyncOperation_1__LaunchQuerySupportStatus = interface(IAsyncOperation_1__LaunchQuerySupportStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.LaunchUriStatus>
  AsyncOperationCompletedHandler_1__LaunchUriStatus_Delegate_Base = interface(IUnknown)
  ['{520AA58E-40D6-5A57-A6DC-CB5FAEA5CCA5}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__LaunchUriStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.LaunchUriStatus>
  AsyncOperationCompletedHandler_1__LaunchUriStatus = interface(AsyncOperationCompletedHandler_1__LaunchUriStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.LaunchUriStatus>
  IAsyncOperation_1__LaunchUriStatus_Base = interface(IInspectable)
  ['{AB3D721B-A4F3-5861-B034-030B15233C52}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__LaunchUriStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__LaunchUriStatus; safecall;
    function GetResults: LaunchUriStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__LaunchUriStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.LaunchUriStatus>
  IAsyncOperation_1__LaunchUriStatus = interface(IAsyncOperation_1__LaunchUriStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.System.ILauncherViewOptions
  ILauncherViewOptions = interface(IInspectable)
  ['{8A9B29F1-7CA7-49DE-9BD3-3C5B7184F616}']
    function get_DesiredRemainingView: ViewSizePreference; safecall;
    procedure put_DesiredRemainingView(value: ViewSizePreference); safecall;
    property DesiredRemainingView: ViewSizePreference read get_DesiredRemainingView write put_DesiredRemainingView;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.System.IAppMemoryUsageLimitChangingEventArgs>
  EventHandler_1__IAppMemoryUsageLimitChangingEventArgs_Delegate_Base = interface(IUnknown)
  ['{6030E7C3-F93F-5E9C-9BA2-9A018D2B09C0}']
    procedure Invoke(sender: IInspectable; args: IAppMemoryUsageLimitChangingEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.System.IAppMemoryUsageLimitChangingEventArgs>
  EventHandler_1__IAppMemoryUsageLimitChangingEventArgs = interface(EventHandler_1__IAppMemoryUsageLimitChangingEventArgs_Delegate_Base)
  ['{12BD3887-6A66-56D5-9565-5E66B00BD683}']
  end;

  // UsedAPI Interface
  // Windows.System.IProcessMemoryReport
  IProcessMemoryReport = interface(IInspectable)
  ['{087305A8-9B70-4782-8741-3A982B6CE5E4}']
    function get_PrivateWorkingSetUsage: UInt64; safecall;
    function get_TotalWorkingSetUsage: UInt64; safecall;
    property PrivateWorkingSetUsage: UInt64 read get_PrivateWorkingSetUsage;
    property TotalWorkingSetUsage: UInt64 read get_TotalWorkingSetUsage;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IProcessLauncherOptions
  [WinRTClassNameAttribute(SWindows_System_ProcessLauncherOptions)]
  IProcessLauncherOptions = interface(IInspectable)
  ['{3080B9CF-F444-4A83-BEAF-A549A0F3229C}']
    function get_StandardInput: IInputStream; safecall;
    procedure put_StandardInput(value: IInputStream); safecall;
    function get_StandardOutput: IOutputStream; safecall;
    procedure put_StandardOutput(value: IOutputStream); safecall;
    function get_StandardError: IOutputStream; safecall;
    procedure put_StandardError(value: IOutputStream); safecall;
    function get_WorkingDirectory: HSTRING; safecall;
    procedure put_WorkingDirectory(value: HSTRING); safecall;
    property StandardError: IOutputStream read get_StandardError write put_StandardError;
    property StandardInput: IInputStream read get_StandardInput write put_StandardInput;
    property StandardOutput: IOutputStream read get_StandardOutput write put_StandardOutput;
    property WorkingDirectory: HSTRING read get_WorkingDirectory write put_WorkingDirectory;
  end;

  // UsedAPI Interface
  // Windows.System.IProcessLauncherResult
  IProcessLauncherResult = interface(IInspectable)
  ['{544C8934-86D8-4991-8E75-ECE8A43B6B6D}']
    function get_ExitCode: Cardinal; safecall;
    property ExitCode: Cardinal read get_ExitCode;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IProcessLauncherResult>
  AsyncOperationCompletedHandler_1__IProcessLauncherResult_Delegate_Base = interface(IUnknown)
  ['{8D787EE6-07E4-5DCE-8FE5-B503A1F6368C}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IProcessLauncherResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IProcessLauncherResult>
  AsyncOperationCompletedHandler_1__IProcessLauncherResult = interface(AsyncOperationCompletedHandler_1__IProcessLauncherResult_Delegate_Base)
  ['{512FE955-DDF9-558A-A619-D23B5829279F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.IProcessLauncherResult>
  IAsyncOperation_1__IProcessLauncherResult_Base = interface(IInspectable)
  ['{E6827240-7A8D-51BE-8D21-E093268CCC15}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IProcessLauncherResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IProcessLauncherResult; safecall;
    function GetResults: IProcessLauncherResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IProcessLauncherResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.IProcessLauncherResult>
  IAsyncOperation_1__IProcessLauncherResult = interface(IAsyncOperation_1__IProcessLauncherResult_Base)
  ['{57592837-AB2C-55CA-A796-9121A2902C39}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IProcessLauncherStatics
  [WinRTClassNameAttribute(SWindows_System_ProcessLauncher)]
  IProcessLauncherStatics = interface(IInspectable)
  ['{33AB66E7-2D0E-448B-A6A0-C13C3836D09C}']
    function RunToCompletionAsync(fileName: HSTRING; args: HSTRING): IAsyncOperation_1__IProcessLauncherResult; overload; safecall;
    function RunToCompletionAsync(fileName: HSTRING; args: HSTRING; options: IProcessLauncherOptions): IAsyncOperation_1__IProcessLauncherResult; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.IRemoteLauncherOptions
  IRemoteLauncherOptions = interface(IInspectable)
  ['{9E3A2788-2891-4CDF-A2D6-9DFF7D02E693}']
    function get_FallbackUri: IUriRuntimeClass; safecall;
    procedure put_FallbackUri(value: IUriRuntimeClass); safecall;
    function get_PreferredAppIds: IVector_1__HSTRING; safecall;
    property FallbackUri: IUriRuntimeClass read get_FallbackUri write put_FallbackUri;
    property PreferredAppIds: IVector_1__HSTRING read get_PreferredAppIds;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteLaunchUriStatus>
  AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus_Delegate_Base = interface(IUnknown)
  ['{3F8F4B1B-CD54-543C-817F-2630487F1878}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__RemoteLaunchUriStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteLaunchUriStatus>
  AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus = interface(AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteLaunchUriStatus>
  IAsyncOperation_1__RemoteLaunchUriStatus_Base = interface(IInspectable)
  ['{BB70E5CD-62C2-5F78-AC8D-F4B973981DA4}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus; safecall;
    function GetResults: RemoteLaunchUriStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__RemoteLaunchUriStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteLaunchUriStatus>
  IAsyncOperation_1__RemoteLaunchUriStatus = interface(IAsyncOperation_1__RemoteLaunchUriStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IShutdownManagerStatics
  [WinRTClassNameAttribute(SWindows_System_ShutdownManager)]
  IShutdownManagerStatics = interface(IInspectable)
  ['{72E247ED-DD5B-4D6C-B1D0-C57A7BBB5F94}']
    procedure BeginShutdown(shutdownKind: ShutdownKind; timeout: TimeSpan); safecall;
    procedure CancelShutdown; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IShutdownManagerStatics2
  [WinRTClassNameAttribute(SWindows_System_ShutdownManager)]
  IShutdownManagerStatics2 = interface(IInspectable)
  ['{0F69A02F-9C34-43C7-A8C3-70B30A7F7504}']
    function IsPowerStateSupported(powerState: PowerState): Boolean; safecall;
    procedure EnterPowerState(powerState: PowerState); overload; safecall;
    procedure EnterPowerState(powerState: PowerState; wakeUpAfter: TimeSpan); overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.AutoUpdateTimeZoneStatus>
  AsyncOperationCompletedHandler_1__AutoUpdateTimeZoneStatus = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__AutoUpdateTimeZoneStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.AutoUpdateTimeZoneStatus>
  IAsyncOperation_1__AutoUpdateTimeZoneStatus = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__AutoUpdateTimeZoneStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__AutoUpdateTimeZoneStatus; safecall;
    function GetResults: AutoUpdateTimeZoneStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__AutoUpdateTimeZoneStatus read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.System.IUserAuthenticationStatusChangeDeferral
  IUserAuthenticationStatusChangeDeferral = interface(IInspectable)
  ['{88B59568-BB30-42FB-A270-E9902E40EFA7}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.IUserAuthenticationStatusChangingEventArgs
  IUserAuthenticationStatusChangingEventArgs = interface(IInspectable)
  ['{8C030F28-A711-4C1E-AB48-04179C15938F}']
    function GetDeferral: IUserAuthenticationStatusChangeDeferral; safecall;
    function get_User: IUser; safecall;
    function get_NewStatus: UserAuthenticationStatus; safecall;
    function get_CurrentStatus: UserAuthenticationStatus; safecall;
    property CurrentStatus: UserAuthenticationStatus read get_CurrentStatus;
    property NewStatus: UserAuthenticationStatus read get_NewStatus;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.UserWatcherUpdateKind>
  IIterator_1__UserWatcherUpdateKind = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: UserWatcherUpdateKind; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PUserWatcherUpdateKind): Cardinal; safecall;
    property Current: UserWatcherUpdateKind read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.UserWatcherUpdateKind>
  IIterable_1__UserWatcherUpdateKind = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__UserWatcherUpdateKind; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.UserWatcherUpdateKind>
  IVectorView_1__UserWatcherUpdateKind = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): UserWatcherUpdateKind; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: UserWatcherUpdateKind; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PUserWatcherUpdateKind): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.System.IUserChangedEventArgs2
  IUserChangedEventArgs2 = interface(IInspectable)
  ['{6B2CCB44-6F01-560C-97AD-FC7F32EC581F}']
    function get_ChangedPropertyKinds: IVectorView_1__UserWatcherUpdateKind; safecall;
    property ChangedPropertyKinds: IVectorView_1__UserWatcherUpdateKind read get_ChangedPropertyKinds;
  end;

  // UsedAPI Interface
  // Windows.System.IUserDeviceAssociationChangedEventArgs
  IUserDeviceAssociationChangedEventArgs = interface(IInspectable)
  ['{BD1F6F6C-BB5D-4D7B-A5F0-C8CD11A38D42}']
    function get_DeviceId: HSTRING; safecall;
    function get_NewUser: IUser; safecall;
    function get_OldUser: IUser; safecall;
    property DeviceId: HSTRING read get_DeviceId;
    property NewUser: IUser read get_NewUser;
    property OldUser: IUser read get_OldUser;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.System.IUserDeviceAssociationChangedEventArgs>
  EventHandler_1__IUserDeviceAssociationChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{D3A3D4C5-D8B6-5A0E-88ED-A5499C377BE3}']
    procedure Invoke(sender: IInspectable; args: IUserDeviceAssociationChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.System.IUserDeviceAssociationChangedEventArgs>
  EventHandler_1__IUserDeviceAssociationChangedEventArgs = interface(EventHandler_1__IUserDeviceAssociationChangedEventArgs_Delegate_Base)
  ['{DCB776BF-9377-5EAA-A1E9-1663A74E8A82}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IUserDeviceAssociationStatics
  [WinRTClassNameAttribute(SWindows_System_UserDeviceAssociation)]
  IUserDeviceAssociationStatics = interface(IInspectable)
  ['{7E491E14-F85A-4C07-8DA9-7FE3D0542343}']
    function FindUserFromDeviceId(deviceId: HSTRING): IUser; safecall;
    function add_UserDeviceAssociationChanged(handler: EventHandler_1__IUserDeviceAssociationChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_UserDeviceAssociationChanged(token: EventRegistrationToken); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IUser>
  AsyncOperationCompletedHandler_1__IUser_Delegate_Base = interface(IUnknown)
  ['{F913E3A2-D1C5-5308-BECF-4C2D8167824A}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IUser; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.IUser>
  AsyncOperationCompletedHandler_1__IUser = interface(AsyncOperationCompletedHandler_1__IUser_Delegate_Base)
  ['{EF43E3B4-FD2D-5741-8316-4615C835932A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.IUser>
  IAsyncOperation_1__IUser_Base = interface(IInspectable)
  ['{A895D2F9-2399-5104-A532-5BA44AB4B165}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IUser); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IUser; safecall;
    function GetResults: IUser; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IUser read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.IUser>
  IAsyncOperation_1__IUser = interface(IAsyncOperation_1__IUser_Base)
  ['{0C897611-AD41-5100-B8E0-8E9A2A63DD93}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IUserPicker
  [WinRTClassNameAttribute(SWindows_System_UserPicker)]
  IUserPicker = interface(IInspectable)
  ['{7D548008-F1E3-4A6C-8DDC-A9BB0F488AED}']
    function get_AllowGuestAccounts: Boolean; safecall;
    procedure put_AllowGuestAccounts(value: Boolean); safecall;
    function get_SuggestedSelectedUser: IUser; safecall;
    procedure put_SuggestedSelectedUser(value: IUser); safecall;
    function PickSingleUserAsync: IAsyncOperation_1__IUser; safecall;
    property AllowGuestAccounts: Boolean read get_AllowGuestAccounts write put_AllowGuestAccounts;
    property SuggestedSelectedUser: IUser read get_SuggestedSelectedUser write put_SuggestedSelectedUser;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IUserPickerStatics
  [WinRTClassNameAttribute(SWindows_System_UserPicker)]
  IUserPickerStatics = interface(IInspectable)
  ['{DE3290DC-7E73-4DF6-A1AE-4D7ECA82B40D}']
    function IsSupported: Boolean; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Windows.System.IUserChangedEventArgs>
  TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{EB9D0454-25DB-5620-98B8-BE4C5D0DBC67}']
    procedure Invoke(sender: IUserWatcher; args: IUserChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Windows.System.IUserChangedEventArgs>
  TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs = interface(TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs_Delegate_Base)
  ['{86D02F04-FE8D-56E3-BF34-02086CE023C3}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Windows.System.IUserAuthenticationStatusChangingEventArgs>
  TypedEventHandler_2__IUserWatcher__IUserAuthenticationStatusChangingEventArgs_Delegate_Base = interface(IUnknown)
  ['{9EC3D9D5-B413-51DF-8C64-640E3356E351}']
    procedure Invoke(sender: IUserWatcher; args: IUserAuthenticationStatusChangingEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Windows.System.IUserAuthenticationStatusChangingEventArgs>
  TypedEventHandler_2__IUserWatcher__IUserAuthenticationStatusChangingEventArgs = interface(TypedEventHandler_2__IUserWatcher__IUserAuthenticationStatusChangingEventArgs_Delegate_Base)
  ['{8E52A522-D252-5C66-981D-5C4669CAA182}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Object>
  TypedEventHandler_2__IUserWatcher__IInspectable_Delegate_Base = interface(IUnknown)
  ['{F155E0FF-DBB5-5C34-AC0C-7E291E3300AB}']
    procedure Invoke(sender: IUserWatcher; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.IUserWatcher,Object>
  TypedEventHandler_2__IUserWatcher__IInspectable = interface(TypedEventHandler_2__IUserWatcher__IInspectable_Delegate_Base)
  ['{84CEB98E-C67B-5F00-9D4D-BAE9724A3F2C}']
  end;

  // UsedAPI Interface
  // Windows.System.IUserWatcher
  IUserWatcher = interface(IInspectable)
  ['{155EB23B-242A-45E0-A2E9-3171FC6A7FBB}']
    function get_Status: UserWatcherStatus; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    function add_Added(handler: TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Added(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    function add_Updated(handler: TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Updated(token: EventRegistrationToken); safecall;
    function add_AuthenticationStatusChanged(handler: TypedEventHandler_2__IUserWatcher__IUserChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AuthenticationStatusChanged(token: EventRegistrationToken); safecall;
    function add_AuthenticationStatusChanging(handler: TypedEventHandler_2__IUserWatcher__IUserAuthenticationStatusChangingEventArgs): EventRegistrationToken; safecall;
    procedure remove_AuthenticationStatusChanging(token: EventRegistrationToken); safecall;
    function add_EnumerationCompleted(handler: TypedEventHandler_2__IUserWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(token: EventRegistrationToken); safecall;
    function add_Stopped(handler: TypedEventHandler_2__IUserWatcher__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Stopped(token: EventRegistrationToken); safecall;
    property Status: UserWatcherStatus read get_Status;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IUser>
  IIterator_1__IUser_Base = interface(IInspectable)
  ['{326FE162-582B-5659-B8A4-68FF0F525745}']
    function get_Current: IUser; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIUser): Cardinal; safecall;
    property Current: IUser read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.IUser>
  IIterator_1__IUser = interface(IIterator_1__IUser_Base)
  ['{7CA609B7-BB87-5F99-93CE-9F66837F18B2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IUser>
  IIterable_1__IUser_Base = interface(IInspectable)
  ['{D1BACD1F-0376-5823-8C29-1D45B9F4C191}']
    function First: IIterator_1__IUser; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.IUser>
  IIterable_1__IUser = interface(IIterable_1__IUser_Base)
  ['{31EAD71A-B0E3-523D-ADC3-A3C6C4E566CD}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>
  IVectorView_1__IUser = interface(IInspectable)
  ['{5B679876-AAF7-5044-99B1-64E9A66CAD79}']
    function GetAt(index: Cardinal): IUser; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IUser; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIUser): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IUser_Delegate_Base = interface(IUnknown)
  ['{09870533-F7CB-569C-B797-DCB48DEBD709}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IUser; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IUser = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IUser_Delegate_Base)
  ['{5F64D434-639D-57F4-A6E7-351B8BBF48B3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>>
  IAsyncOperation_1__IVectorView_1__IUser_Base = interface(IInspectable)
  ['{E44EA1DF-BB85-5A8C-BDDC-C8E960C355C9}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IUser); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IUser; safecall;
    function GetResults: IVectorView_1__IUser; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IUser read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.IUser>>
  IAsyncOperation_1__IVectorView_1__IUser = interface(IAsyncOperation_1__IVectorView_1__IUser_Base)
  ['{A2CB77C4-6586-5D0A-B451-72EA90169873}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.IUserStatics
  [WinRTClassNameAttribute(SWindows_System_User)]
  IUserStatics = interface(IInspectable)
  ['{155EB23B-242A-45E0-A2E9-3171FC6A7FDD}']
    function CreateWatcher: IUserWatcher; safecall;
    function FindAllAsync: IAsyncOperation_1__IVectorView_1__IUser; overload; safecall;
    function FindAllAsync(&type: UserType): IAsyncOperation_1__IVectorView_1__IUser; overload; safecall;
    function FindAllAsync(&type: UserType; status: UserAuthenticationStatus): IAsyncOperation_1__IVectorView_1__IUser; overload; safecall;
    function GetFromId(nonRoamableId: HSTRING): IUser; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Inventory.IInstalledDesktopApp
  Inventory_IInstalledDesktopApp = interface(IInspectable)
  ['{75EAB8ED-C0BC-5364-4C28-166E0545167A}']
    function get_Id: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_Publisher: HSTRING; safecall;
    function get_DisplayVersion: HSTRING; safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property DisplayVersion: HSTRING read get_DisplayVersion;
    property Id: HSTRING read get_Id;
    property Publisher: HSTRING read get_Publisher;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.Inventory.IInstalledDesktopApp>
  IIterator_1__Inventory_IInstalledDesktopApp = interface(IInspectable)
  ['{52D9CA44-67C9-5789-8AD4-4B811DBFDF13}']
    function get_Current: Inventory_IInstalledDesktopApp; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInventory_IInstalledDesktopApp): Cardinal; safecall;
    property Current: Inventory_IInstalledDesktopApp read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.Inventory.IInstalledDesktopApp>
  IIterable_1__Inventory_IInstalledDesktopApp = interface(IInspectable)
  ['{933F7C4E-48FB-5F36-BADC-FC87D37D55C3}']
    function First: IIterator_1__Inventory_IInstalledDesktopApp; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Inventory.IInstalledDesktopApp>
  IVectorView_1__Inventory_IInstalledDesktopApp = interface(IInspectable)
  ['{BD5B5E23-7B3C-51FA-91E3-B14C01D4721D}']
    function GetAt(index: Cardinal): Inventory_IInstalledDesktopApp; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Inventory_IInstalledDesktopApp; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInventory_IInstalledDesktopApp): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.Inventory.IInstalledDesktopApp>>
  AsyncOperationCompletedHandler_1__IVectorView_1__Inventory_IInstalledDesktopApp = interface(IUnknown)
  ['{06990C85-9D06-52B0-958B-322F2B7B18BF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__Inventory_IInstalledDesktopApp; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.System.Inventory.IInstalledDesktopApp>>
  IAsyncOperation_1__IVectorView_1__Inventory_IInstalledDesktopApp = interface(IInspectable)
  ['{A171A8B3-F5F3-5199-91E5-47EE33C3863E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__Inventory_IInstalledDesktopApp); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Inventory_IInstalledDesktopApp; safecall;
    function GetResults: IVectorView_1__Inventory_IInstalledDesktopApp; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__Inventory_IInstalledDesktopApp read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReading
  Preview_ITwoPanelHingedDevicePosturePreviewReading = interface(IInspectable)
  ['{A0251452-4AD6-4B38-8426-C59A15493A7D}']
    function get_Timestamp: DateTime; safecall;
    function get_HingeState: Preview_HingeState; safecall;
    function get_Panel1Orientation: SimpleOrientation; safecall;
    function get_Panel1Id: HSTRING; safecall;
    function get_Panel2Orientation: SimpleOrientation; safecall;
    function get_Panel2Id: HSTRING; safecall;
    property HingeState: Preview_HingeState read get_HingeState;
    property Panel1Id: HSTRING read get_Panel1Id;
    property Panel1Orientation: SimpleOrientation read get_Panel1Orientation;
    property Panel2Id: HSTRING read get_Panel2Id;
    property Panel2Orientation: SimpleOrientation read get_Panel2Orientation;
    property Timestamp: DateTime read get_Timestamp;
  end deprecated;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReading>
  AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreviewReading = interface(IUnknown)
  ['{7A75D5C4-76F6-533E-9BFB-139D1A0494E0}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreviewReading; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReading>
  IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreviewReading = interface(IInspectable)
  ['{16C85870-DAF5-576C-9D12-DFE182C8CF9F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreviewReading); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreviewReading; safecall;
    function GetResults: Preview_ITwoPanelHingedDevicePosturePreviewReading; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreviewReading read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs
  Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs = interface(IInspectable)
  ['{2D2D1BC6-02CE-474A-A556-A75B1CF93A03}']
    function get_Reading: Preview_ITwoPanelHingedDevicePosturePreviewReading; safecall;
    property Reading: Preview_ITwoPanelHingedDevicePosturePreviewReading read get_Reading;
  end deprecated;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.Preview.ITwoPanelHingedDevicePosturePreview,Windows.System.Preview.ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs>
  TypedEventHandler_2__Preview_ITwoPanelHingedDevicePosturePreview__Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs = interface(IUnknown)
  ['{DE9ABE74-9D4F-5CA6-9580-4BB10A599F85}']
    procedure Invoke(sender: Preview_ITwoPanelHingedDevicePosturePreview; args: Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Preview.ITwoPanelHingedDevicePosturePreview
  Preview_ITwoPanelHingedDevicePosturePreview = interface(IInspectable)
  ['{72245C31-4B39-42A6-8E73-7235ADE16853}']
    function GetCurrentPostureAsync: IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreviewReading; safecall;
    function add_PostureChanged(handler: TypedEventHandler_2__Preview_ITwoPanelHingedDevicePosturePreview__Preview_ITwoPanelHingedDevicePosturePreviewReadingChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_PostureChanged(token: EventRegistrationToken); safecall;
  end deprecated;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreview>
  AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreview = interface(IUnknown)
  ['{49E6AB5E-D07C-5EC1-A38F-A51287F100F5}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreview; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.Preview.ITwoPanelHingedDevicePosturePreview>
  IAsyncOperation_1__Preview_ITwoPanelHingedDevicePosturePreview = interface(IInspectable)
  ['{25DF2446-C75B-534B-B146-B7BA917D6CDF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreview); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreview; safecall;
    function GetResults: Preview_ITwoPanelHingedDevicePosturePreview; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Preview_ITwoPanelHingedDevicePosturePreview read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.System.Profile.IAnalyticsVersionInfo
  Profile_IAnalyticsVersionInfo = interface(IInspectable)
  ['{926130B8-9955-4C74-BDC1-7CD0DECF9B03}']
    function get_DeviceFamily: HSTRING; safecall;
    function get_DeviceFamilyVersion: HSTRING; safecall;
    property DeviceFamily: HSTRING read get_DeviceFamily;
    property DeviceFamilyVersion: HSTRING read get_DeviceFamilyVersion;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,String>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__HSTRING = interface(IUnknown)
  ['{75E3182C-E6E1-589C-AB73-E8644BC285BF}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMapView_2__HSTRING__HSTRING; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,String>>
  IAsyncOperation_1__IMapView_2__HSTRING__HSTRING = interface(IInspectable)
  ['{817944B6-F046-5391-BB0B-4CC34D8040F3}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__HSTRING); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__HSTRING; safecall;
    function GetResults: IMapView_2__HSTRING__HSTRING; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__HSTRING read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.System.Profile.IUnsupportedAppRequirement
  Profile_IUnsupportedAppRequirement = interface(IInspectable)
  ['{6182445C-894B-5CBC-8976-A98E0A9B998D}']
    function get_Requirement: HSTRING; safecall;
    function get_Reasons: Profile_UnsupportedAppRequirementReasons; safecall;
    property Reasons: Profile_UnsupportedAppRequirementReasons read get_Reasons;
    property Requirement: HSTRING read get_Requirement;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.Profile.IUnsupportedAppRequirement>
  IIterator_1__Profile_IUnsupportedAppRequirement = interface(IInspectable)
  ['{BB976B54-C41E-5F1C-B2F6-9130D4842DF4}']
    function get_Current: Profile_IUnsupportedAppRequirement; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PProfile_IUnsupportedAppRequirement): Cardinal; safecall;
    property Current: Profile_IUnsupportedAppRequirement read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.Profile.IUnsupportedAppRequirement>
  IIterable_1__Profile_IUnsupportedAppRequirement = interface(IInspectable)
  ['{104C85B8-4E45-5A0E-B592-5CB858A3970D}']
    function First: IIterator_1__Profile_IUnsupportedAppRequirement; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Profile.IUnsupportedAppRequirement>
  IVectorView_1__Profile_IUnsupportedAppRequirement = interface(IInspectable)
  ['{498DEC57-3E09-54E7-A0E3-0261B7DF6F85}']
    function GetAt(index: Cardinal): Profile_IUnsupportedAppRequirement; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Profile_IUnsupportedAppRequirement; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PProfile_IUnsupportedAppRequirement): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.System.Profile.IHardwareToken
  Profile_IHardwareToken = interface(IInspectable)
  ['{28F6D4C0-FB12-40A4-8167-7F4E03D2724C}']
    function get_Id: IBuffer; safecall;
    function get_Signature: IBuffer; safecall;
    function get_Certificate: IBuffer; safecall;
    property Certificate: IBuffer read get_Certificate;
    property Id: IBuffer read get_Id;
    property Signature: IBuffer read get_Signature;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Profile.IPlatformDiagnosticsAndUsageDataSettingsStatics
  [WinRTClassNameAttribute(SWindows_System_Profile_PlatformDiagnosticsAndUsageDataSettings)]
  Profile_IPlatformDiagnosticsAndUsageDataSettingsStatics = interface(IInspectable)
  ['{B6E24C1B-7B1C-4B32-8C62-A66597CE723A}']
    function get_CollectionLevel: Profile_PlatformDataCollectionLevel; safecall;
    function add_CollectionLevelChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_CollectionLevelChanged(token: EventRegistrationToken); safecall;
    function CanCollectDiagnostics(level: Profile_PlatformDataCollectionLevel): Boolean; safecall;
    property CollectionLevel: Profile_PlatformDataCollectionLevel read get_CollectionLevel;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Profile.ISharedModeSettingsStatics
  [WinRTClassNameAttribute(SWindows_System_Profile_SharedModeSettings)]
  Profile_ISharedModeSettingsStatics = interface(IInspectable)
  ['{893DF40E-CAD6-4D50-8C49-6FCFC03EDB29}']
    function get_IsEnabled: Boolean; safecall;
    property IsEnabled: Boolean read get_IsEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Profile.ISharedModeSettingsStatics2
  [WinRTClassNameAttribute(SWindows_System_Profile_SharedModeSettings)]
  Profile_ISharedModeSettingsStatics2 = interface(IInspectable)
  ['{608988A4-CCF1-4EE8-A5E2-FD6A1D0CFAC8}']
    function get_ShouldAvoidLocalStorage: Boolean; safecall;
    property ShouldAvoidLocalStorage: Boolean read get_ShouldAvoidLocalStorage;
  end;

  // UsedAPI Interface
  // Windows.System.Profile.ISystemIdentificationInfo
  Profile_ISystemIdentificationInfo = interface(IInspectable)
  ['{0C659E7D-C3C2-4D33-A2DF-21BC41916EB3}']
    function get_Id: IBuffer; safecall;
    function get_Source: Profile_SystemIdentificationSource; safecall;
    property Id: IBuffer read get_Id;
    property Source: Profile_SystemIdentificationSource read get_Source;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Profile.ISystemSetupInfoStatics
  [WinRTClassNameAttribute(SWindows_System_Profile_SystemSetupInfo)]
  Profile_ISystemSetupInfoStatics = interface(IInspectable)
  ['{2C9620A8-1D88-5E2D-A324-A543AF4247EE}']
    function get_OutOfBoxExperienceState: Profile_SystemOutOfBoxExperienceState; safecall;
    function add_OutOfBoxExperienceStateChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_OutOfBoxExperienceStateChanged(token: EventRegistrationToken); safecall;
    property OutOfBoxExperienceState: Profile_SystemOutOfBoxExperienceState read get_OutOfBoxExperienceState;
  end;

  // UsedAPI Interface
  // Windows.System.Profile.SystemManufacturers.IOemSupportInfo
  Profile_SystemManufacturers_IOemSupportInfo = interface(IInspectable)
  ['{8D2EAE55-87EF-4266-86D0-C4AFBEB29BB9}']
    function get_SupportLink: IUriRuntimeClass; safecall;
    function get_SupportAppLink: IUriRuntimeClass; safecall;
    function get_SupportProvider: HSTRING; safecall;
    property SupportAppLink: IUriRuntimeClass read get_SupportAppLink;
    property SupportLink: IUriRuntimeClass read get_SupportLink;
    property SupportProvider: HSTRING read get_SupportProvider;
  end;

  // UsedAPI Interface
  // Windows.System.Profile.SystemManufacturers.ISystemSupportDeviceInfo
  Profile_SystemManufacturers_ISystemSupportDeviceInfo = interface(IInspectable)
  ['{05880B99-8247-441B-A996-A1784BAB79A8}']
    function get_OperatingSystem: HSTRING; safecall;
    function get_FriendlyName: HSTRING; safecall;
    function get_SystemManufacturer: HSTRING; safecall;
    function get_SystemProductName: HSTRING; safecall;
    function get_SystemSku: HSTRING; safecall;
    function get_SystemHardwareVersion: HSTRING; safecall;
    function get_SystemFirmwareVersion: HSTRING; safecall;
    property FriendlyName: HSTRING read get_FriendlyName;
    property OperatingSystem: HSTRING read get_OperatingSystem;
    property SystemFirmwareVersion: HSTRING read get_SystemFirmwareVersion;
    property SystemHardwareVersion: HSTRING read get_SystemHardwareVersion;
    property SystemManufacturer: HSTRING read get_SystemManufacturer;
    property SystemProductName: HSTRING read get_SystemProductName;
    property SystemSku: HSTRING read get_SystemSku;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Profile.SystemManufacturers.ISystemSupportInfoStatics
  [WinRTClassNameAttribute(SWindows_System_Profile_SystemManufacturers_SystemSupportInfo)]
  Profile_SystemManufacturers_ISystemSupportInfoStatics = interface(IInspectable)
  ['{EF750974-C422-45D7-A44D-5C1C0043A2B3}']
    function get_LocalSystemEdition: HSTRING; safecall;
    function get_OemSupportInfo: Profile_SystemManufacturers_IOemSupportInfo; safecall;
    property LocalSystemEdition: HSTRING read get_LocalSystemEdition;
    property OemSupportInfo: Profile_SystemManufacturers_IOemSupportInfo read get_OemSupportInfo;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.Profile.SystemManufacturers.ISystemSupportInfoStatics2
  [WinRTClassNameAttribute(SWindows_System_Profile_SystemManufacturers_SystemSupportInfo)]
  Profile_SystemManufacturers_ISystemSupportInfoStatics2 = interface(IInspectable)
  ['{33F349A4-3FA1-4986-AA4B-057420455E6D}']
    function get_LocalDeviceInfo: Profile_SystemManufacturers_ISystemSupportDeviceInfo; safecall;
    property LocalDeviceInfo: Profile_SystemManufacturers_ISystemSupportDeviceInfo read get_LocalDeviceInfo;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystem2
  RemoteSystems_IRemoteSystem2 = interface(IInspectable)
  ['{09DFE4EC-FB8B-4A08-A758-6876435D769E}']
    function get_IsAvailableBySpatialProximity: Boolean; safecall;
    function GetCapabilitySupportedAsync(capabilityName: HSTRING): IAsyncOperation_1__Boolean; safecall;
    property IsAvailableBySpatialProximity: Boolean read get_IsAvailableBySpatialProximity;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystem3
  RemoteSystems_IRemoteSystem3 = interface(IInspectable)
  ['{72B4B495-B7C6-40BE-831B-73562F12FFA8}']
    function get_ManufacturerDisplayName: HSTRING; safecall;
    function get_ModelDisplayName: HSTRING; safecall;
    property ManufacturerDisplayName: HSTRING read get_ManufacturerDisplayName;
    property ModelDisplayName: HSTRING read get_ModelDisplayName;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystem4
  RemoteSystems_IRemoteSystem4 = interface(IInspectable)
  ['{F164FFE5-B987-4CA5-9926-FA0438BE6273}']
    function get_Platform: RemoteSystems_RemoteSystemPlatform; safecall;
    property Platform: RemoteSystems_RemoteSystemPlatform read get_Platform;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemApp
  RemoteSystems_IRemoteSystemApp = interface(IInspectable)
  ['{80E5BCBD-D54D-41B1-9B16-6810A871ED4F}']
    function get_Id: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_IsAvailableByProximity: Boolean; safecall;
    function get_IsAvailableBySpatialProximity: Boolean; safecall;
    function get_Attributes: IMapView_2__HSTRING__HSTRING; safecall;
    property Attributes: IMapView_2__HSTRING__HSTRING read get_Attributes;
    property DisplayName: HSTRING read get_DisplayName;
    property Id: HSTRING read get_Id;
    property IsAvailableByProximity: Boolean read get_IsAvailableByProximity;
    property IsAvailableBySpatialProximity: Boolean read get_IsAvailableBySpatialProximity;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemApp>
  IIterator_1__RemoteSystems_IRemoteSystemApp = interface(IInspectable)
  ['{A5BE1264-7421-53B5-827A-CB1C73A758A6}']
    function get_Current: RemoteSystems_IRemoteSystemApp; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PRemoteSystems_IRemoteSystemApp): Cardinal; safecall;
    property Current: RemoteSystems_IRemoteSystemApp read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemApp>
  IIterable_1__RemoteSystems_IRemoteSystemApp = interface(IInspectable)
  ['{69FE5DEE-5716-53B5-BB92-EC133AA1BF15}']
    function First: IIterator_1__RemoteSystems_IRemoteSystemApp; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.RemoteSystems.IRemoteSystemApp>
  IVectorView_1__RemoteSystems_IRemoteSystemApp = interface(IInspectable)
  ['{A75948BC-608E-5569-A816-371CBE5D180E}']
    function GetAt(index: Cardinal): RemoteSystems_IRemoteSystemApp; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: RemoteSystems_IRemoteSystemApp; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PRemoteSystems_IRemoteSystemApp): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystem5
  RemoteSystems_IRemoteSystem5 = interface(IInspectable)
  ['{EB2AD723-E5E2-4AE2-A7A7-A1097A098E90}']
    function get_Apps: IVectorView_1__RemoteSystems_IRemoteSystemApp; safecall;
    property Apps: IVectorView_1__RemoteSystems_IRemoteSystemApp read get_Apps;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystem6
  RemoteSystems_IRemoteSystem6 = interface(IInspectable)
  ['{D4CDA942-C027-533E-9384-3A19B4F7EEF3}']
    function get_User: IUser; safecall;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemAddedEventArgs
  RemoteSystems_IRemoteSystemAddedEventArgs = interface(IInspectable)
  ['{8F39560F-E534-4697-8836-7ABEA151516E}']
    function get_RemoteSystem: RemoteSystems_IRemoteSystem; safecall;
    property RemoteSystem: RemoteSystems_IRemoteSystem read get_RemoteSystem;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemApp2
  RemoteSystems_IRemoteSystemApp2 = interface(IInspectable)
  ['{6369BF15-0A96-577A-8FF6-C35904DFA8F3}']
    function get_User: IUser; safecall;
    function get_ConnectionToken: HSTRING; safecall;
    property ConnectionToken: HSTRING read get_ConnectionToken;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemAppRegistration
  RemoteSystems_IRemoteSystemAppRegistration = interface(IInspectable)
  ['{B47947B5-7035-4A5A-B8DF-962D8F8431F4}']
    function get_User: IUser; safecall;
    function get_Attributes: IMap_2__HSTRING__HSTRING; safecall;
    function SaveAsync: IAsyncOperation_1__Boolean; safecall;
    property Attributes: IMap_2__HSTRING__HSTRING read get_Attributes;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemAuthorizationKindFilter
  RemoteSystems_IRemoteSystemAuthorizationKindFilter = interface(IInspectable)
  ['{6B0DDE8E-04D0-40F4-A27F-C2ACBBD6B734}']
    function get_RemoteSystemAuthorizationKind: RemoteSystems_RemoteSystemAuthorizationKind; safecall;
    property RemoteSystemAuthorizationKind: RemoteSystems_RemoteSystemAuthorizationKind read get_RemoteSystemAuthorizationKind;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemConnectionInfo
  RemoteSystems_IRemoteSystemConnectionInfo = interface(IInspectable)
  ['{23278BC3-0D09-52CB-9C6A-EED2940BEE43}']
    function get_IsProximal: Boolean; safecall;
    property IsProximal: Boolean read get_IsProximal;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemConnectionRequest2
  RemoteSystems_IRemoteSystemConnectionRequest2 = interface(IInspectable)
  ['{12DF6D6F-BFFC-483A-8ABE-D34A6C19F92B}']
    function get_RemoteSystemApp: RemoteSystems_IRemoteSystemApp; safecall;
    property RemoteSystemApp: RemoteSystems_IRemoteSystemApp read get_RemoteSystemApp;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemConnectionRequest3
  RemoteSystems_IRemoteSystemConnectionRequest3 = interface(IInspectable)
  ['{DE86C3E7-C9CC-5A50-B8D9-BA7B34BB8D0E}']
    function get_ConnectionToken: HSTRING; safecall;
    property ConnectionToken: HSTRING read get_ConnectionToken;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemDiscoveryTypeFilter
  RemoteSystems_IRemoteSystemDiscoveryTypeFilter = interface(IInspectable)
  ['{42D9041F-EE5A-43DA-AC6A-6FEE25460741}']
    function get_RemoteSystemDiscoveryType: RemoteSystems_RemoteSystemDiscoveryType; safecall;
    property RemoteSystemDiscoveryType: RemoteSystems_RemoteSystemDiscoveryType read get_RemoteSystemDiscoveryType;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemEnumerationCompletedEventArgs
  RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs = interface(IInspectable)
  ['{C6E83D5F-4030-4354-A060-14F1B22C545D}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemFilter
  RemoteSystems_IRemoteSystemFilter = interface(IInspectable)
  ['{4A3BA9E4-99EB-45EB-BA16-0367728FF374}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemKindFilter
  RemoteSystems_IRemoteSystemKindFilter = interface(IInspectable)
  ['{38E1C9EC-22C3-4EF6-901A-BBB1C7AAD4ED}']
    function get_RemoteSystemKinds: IVectorView_1__HSTRING; safecall;
    property RemoteSystemKinds: IVectorView_1__HSTRING read get_RemoteSystemKinds;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemRemovedEventArgs
  RemoteSystems_IRemoteSystemRemovedEventArgs = interface(IInspectable)
  ['{8B3D16BB-7306-49EA-B7DF-67D5714CB013}']
    function get_RemoteSystemId: HSTRING; safecall;
    property RemoteSystemId: HSTRING read get_RemoteSystemId;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Object>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable_Delegate_Base = interface(IUnknown)
  ['{D4CF5BDA-CC7A-52EF-A256-C4B36163BEEC}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionParticipantWatcher; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Object>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable_Delegate_Base)
  ['{EE7948C0-7384-5E23-A792-9EDC68234AED}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult
  RemoteSystems_IRemoteSystemSessionJoinResult = interface(IInspectable)
  ['{CE7B1F04-A03E-41A4-900B-1E79328C1267}']
    function get_Status: RemoteSystems_RemoteSystemSessionJoinStatus; safecall;
    function get_Session: RemoteSystems_IRemoteSystemSession; safecall;
    property Session: RemoteSystems_IRemoteSystemSession read get_Session;
    property Status: RemoteSystems_RemoteSystemSessionJoinStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult_Delegate_Base = interface(IUnknown)
  ['{379ADF35-4CB4-522F-91BE-913B5690568F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult = interface(AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult_Delegate_Base)
  ['{21900619-F845-51E1-B2CA-F5C02F69F8AC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult>
  IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult_Base = interface(IInspectable)
  ['{C58DBD1E-E300-55A8-ADA5-E25AAAA86667}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult; safecall;
    function GetResults: RemoteSystems_IRemoteSystemSessionJoinResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionJoinResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystemSessionJoinResult>
  IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult = interface(IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult_Base)
  ['{F09147BC-C4F3-5DC2-8838-008E7F5F612C}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionInfo
  RemoteSystems_IRemoteSystemSessionInfo = interface(IInspectable)
  ['{FF4DF648-8B0A-4E9A-9905-69E4B841C588}']
    function get_DisplayName: HSTRING; safecall;
    function get_ControllerDisplayName: HSTRING; safecall;
    function JoinAsync: IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionJoinResult; safecall;
    property ControllerDisplayName: HSTRING read get_ControllerDisplayName;
    property DisplayName: HSTRING read get_DisplayName;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionAddedEventArgs
  RemoteSystems_IRemoteSystemSessionAddedEventArgs = interface(IInspectable)
  ['{D585D754-BC97-4C39-99B4-BECA76E04C3F}']
    function get_SessionInfo: RemoteSystems_IRemoteSystemSessionInfo; safecall;
    property SessionInfo: RemoteSystems_IRemoteSystemSessionInfo read get_SessionInfo;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionJoinRequest
  RemoteSystems_IRemoteSystemSessionJoinRequest = interface(IInspectable)
  ['{20600068-7994-4331-86D1-D89D882585EE}']
    function get_Participant: RemoteSystems_IRemoteSystemSessionParticipant; safecall;
    procedure Accept; safecall;
    property Participant: RemoteSystems_IRemoteSystemSessionParticipant read get_Participant;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionJoinRequestedEventArgs
  RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs = interface(IInspectable)
  ['{DBCA4FC3-82B9-4816-9C24-E40E61774BD8}']
    function get_JoinRequest: RemoteSystems_IRemoteSystemSessionJoinRequest; safecall;
    function GetDeferral: IDeferral; safecall;
    property JoinRequest: RemoteSystems_IRemoteSystemSessionJoinRequest read get_JoinRequest;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionController,Windows.System.RemoteSystems.IRemoteSystemSessionJoinRequestedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionController__RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{D8E04916-B452-5322-AEC9-E3D4D581C772}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionController; args: RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionController,Windows.System.RemoteSystems.IRemoteSystemSessionJoinRequestedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionController__RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionController__RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs_Delegate_Base)
  ['{BAF77AF6-343C-5702-8706-BBBFEAF12675}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult
  RemoteSystems_IRemoteSystemSessionCreationResult = interface(IInspectable)
  ['{A79812C2-37DE-448C-8B83-A30AA3C4EAD6}']
    function get_Status: RemoteSystems_RemoteSystemSessionCreationStatus; safecall;
    function get_Session: RemoteSystems_IRemoteSystemSession; safecall;
    property Session: RemoteSystems_IRemoteSystemSession read get_Session;
    property Status: RemoteSystems_RemoteSystemSessionCreationStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult_Delegate_Base = interface(IUnknown)
  ['{6E72C549-73AA-5168-8560-C7236493B504}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult = interface(AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult_Delegate_Base)
  ['{3DFE2F14-E59E-5D1B-924C-64423E4742DE}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult>
  IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult_Base = interface(IInspectable)
  ['{90364BF5-D084-5F50-9729-82025326ABEF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult; safecall;
    function GetResults: RemoteSystems_IRemoteSystemSessionCreationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystemSessionCreationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystemSessionCreationResult>
  IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult = interface(IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult_Base)
  ['{1742DD31-0963-5C4D-9E37-B6EE48A2D2AF}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionController
  RemoteSystems_IRemoteSystemSessionController = interface(IInspectable)
  ['{E48B2DD2-6820-4867-B425-D89C0A3EF7BA}']
    function add_JoinRequested(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionController__RemoteSystems_IRemoteSystemSessionJoinRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_JoinRequested(token: EventRegistrationToken); safecall;
    function RemoveParticipantAsync(pParticipant: RemoteSystems_IRemoteSystemSessionParticipant): IAsyncOperation_1__Boolean; safecall;
    function CreateSessionAsync: IAsyncOperation_1__RemoteSystems_IRemoteSystemSessionCreationResult; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionOptions
  RemoteSystems_IRemoteSystemSessionOptions = interface(IInspectable)
  ['{740ED755-8418-4F01-9353-E21C9ECC6CFC}']
    function get_IsInviteOnly: Boolean; safecall;
    procedure put_IsInviteOnly(value: Boolean); safecall;
    property IsInviteOnly: Boolean read get_IsInviteOnly write put_IsInviteOnly;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionInvitation
  RemoteSystems_IRemoteSystemSessionInvitation = interface(IInspectable)
  ['{3E32CC91-51D7-4766-A121-25516C3B8294}']
    function get_Sender: RemoteSystems_IRemoteSystem; safecall;
    function get_SessionInfo: RemoteSystems_IRemoteSystemSessionInfo; safecall;
    property Sender: RemoteSystems_IRemoteSystem read get_Sender;
    property SessionInfo: RemoteSystems_IRemoteSystemSessionInfo read get_SessionInfo;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionInvitationReceivedEventArgs
  RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs = interface(IInspectable)
  ['{5E964A2D-A10D-4EDB-8DEA-54D20AC19543}']
    function get_Invitation: RemoteSystems_IRemoteSystemSessionInvitation; safecall;
    property Invitation: RemoteSystems_IRemoteSystemSessionInvitation read get_Invitation;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionInvitationListener,Windows.System.RemoteSystems.IRemoteSystemSessionInvitationReceivedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionInvitationListener__RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{18A242BB-D338-56C4-9559-568D5C2C3E93}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionInvitationListener; args: RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionInvitationListener,Windows.System.RemoteSystems.IRemoteSystemSessionInvitationReceivedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionInvitationListener__RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionInvitationListener__RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs_Delegate_Base)
  ['{3BA42A79-4ED0-55BE-9A6B-864E4D80A359}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionInvitationListener
  RemoteSystems_IRemoteSystemSessionInvitationListener = interface(IInspectable)
  ['{08F4003F-BC71-49E1-874A-31DDFF9A27B9}']
    function add_InvitationReceived(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionInvitationListener__RemoteSystems_IRemoteSystemSessionInvitationReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_InvitationReceived(token: EventRegistrationToken); safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemSessionParticipant>
  IIterator_1__RemoteSystems_IRemoteSystemSessionParticipant_Base = interface(IInspectable)
  ['{05FEC44B-3DD9-5CF1-A100-BEDC9233292D}']
    function get_Current: RemoteSystems_IRemoteSystemSessionParticipant; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PRemoteSystems_IRemoteSystemSessionParticipant): Cardinal; safecall;
    property Current: RemoteSystems_IRemoteSystemSessionParticipant read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemSessionParticipant>
  IIterator_1__RemoteSystems_IRemoteSystemSessionParticipant = interface(IIterator_1__RemoteSystems_IRemoteSystemSessionParticipant_Base)
  ['{707ECFB6-6C93-5E70-8DDD-13F82E7A5271}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemSessionParticipant>
  IIterable_1__RemoteSystems_IRemoteSystemSessionParticipant_Base = interface(IInspectable)
  ['{00189D10-16EC-5D1A-8369-4870C69E52B3}']
    function First: IIterator_1__RemoteSystems_IRemoteSystemSessionParticipant; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemSessionParticipant>
  IIterable_1__RemoteSystems_IRemoteSystemSessionParticipant = interface(IIterable_1__RemoteSystems_IRemoteSystemSessionParticipant_Base)
  ['{0A9C2978-E826-5FE2-92BA-1CF75BDBFA08}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionValueSetReceivedEventArgs
  RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs = interface(IInspectable)
  ['{06F31785-2DA5-4E58-A78F-9E8D0784EE25}']
    function get_Sender: RemoteSystems_IRemoteSystemSessionParticipant; safecall;
    function get_Message: IPropertySet; safecall;
    property &Message: IPropertySet read get_Message;
    property Sender: RemoteSystems_IRemoteSystemSessionParticipant read get_Sender;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionMessageChannel,Windows.System.RemoteSystems.IRemoteSystemSessionValueSetReceivedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionMessageChannel__RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{C476232D-8C76-5BA6-99F5-14557484C20D}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionMessageChannel; args: RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionMessageChannel,Windows.System.RemoteSystems.IRemoteSystemSessionValueSetReceivedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionMessageChannel__RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionMessageChannel__RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs_Delegate_Base)
  ['{988ADC1B-9F30-593E-AA1A-6C99958853C3}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionMessageChannel
  RemoteSystems_IRemoteSystemSessionMessageChannel = interface(IInspectable)
  ['{9524D12A-73D9-4C10-B751-C26784437127}']
    function get_Session: RemoteSystems_IRemoteSystemSession; safecall;
    function BroadcastValueSetAsync(messageData: IPropertySet): IAsyncOperation_1__Boolean; safecall;
    function SendValueSetAsync(messageData: IPropertySet; participant: RemoteSystems_IRemoteSystemSessionParticipant): IAsyncOperation_1__Boolean; safecall;
    function SendValueSetToParticipantsAsync(messageData: IPropertySet; participants: IIterable_1__RemoteSystems_IRemoteSystemSessionParticipant): IAsyncOperation_1__Boolean; safecall;
    function add_ValueSetReceived(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionMessageChannel__RemoteSystems_IRemoteSystemSessionValueSetReceivedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ValueSetReceived(token: EventRegistrationToken); safecall;
    property Session: RemoteSystems_IRemoteSystemSession read get_Session;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionRemovedEventArgs
  RemoteSystems_IRemoteSystemSessionRemovedEventArgs = interface(IInspectable)
  ['{AF82914E-39A1-4DEA-9D63-43798D5BBBD0}']
    function get_SessionInfo: RemoteSystems_IRemoteSystemSessionInfo; safecall;
    property SessionInfo: RemoteSystems_IRemoteSystemSessionInfo read get_SessionInfo;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionAddedEventArgs_Delegate_Base = interface(IUnknown)
  ['{1B036C4F-6B8F-55D6-B6DF-45E46A823B1D}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionWatcher; args: RemoteSystems_IRemoteSystemSessionAddedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionAddedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionAddedEventArgs_Delegate_Base)
  ['{F0EC5210-7411-5E32-BEC2-E232D23DB4D8}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionUpdatedEventArgs
  RemoteSystems_IRemoteSystemSessionUpdatedEventArgs = interface(IInspectable)
  ['{16875069-231E-4C91-8EC8-B3A39D9E55A3}']
    function get_SessionInfo: RemoteSystems_IRemoteSystemSessionInfo; safecall;
    property SessionInfo: RemoteSystems_IRemoteSystemSessionInfo read get_SessionInfo;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionUpdatedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{040F48B7-0D41-5AA2-85E8-6311666F0324}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionWatcher; args: RemoteSystems_IRemoteSystemSessionUpdatedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionUpdatedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionUpdatedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionUpdatedEventArgs_Delegate_Base)
  ['{B8B0C3CD-2DEF-5E52-96BB-404410E617D3}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionRemovedEventArgs_Delegate_Base = interface(IUnknown)
  ['{1CBC54F0-0C9D-59CC-8055-5E017A317812}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionWatcher; args: RemoteSystems_IRemoteSystemSessionRemovedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionWatcher,Windows.System.RemoteSystems.IRemoteSystemSessionRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionRemovedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionRemovedEventArgs_Delegate_Base)
  ['{221C1F5B-1C80-589C-82F3-96397A421EF2}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemSessionWatcher
  RemoteSystems_IRemoteSystemSessionWatcher = interface(IInspectable)
  ['{8003E340-0C41-4A62-B6D7-BDBE2B19BE2D}']
    procedure Start; safecall;
    procedure Stop; safecall;
    function get_Status: RemoteSystems_RemoteSystemSessionWatcherStatus; safecall;
    function add_Added(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionAddedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Added(token: EventRegistrationToken); safecall;
    function add_Updated(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Updated(token: EventRegistrationToken); safecall;
    function add_Removed(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionWatcher__RemoteSystems_IRemoteSystemSessionRemovedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Removed(token: EventRegistrationToken); safecall;
    property Status: RemoteSystems_RemoteSystemSessionWatcherStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystem>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem_Delegate_Base = interface(IUnknown)
  ['{3A0B522D-98D0-5D34-ACE6-2C7346613F1D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__RemoteSystems_IRemoteSystem; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.IRemoteSystem>
  AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem = interface(AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem_Delegate_Base)
  ['{C7AD9F11-8299-58C1-A4C5-8DDA4DCD7C4D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystem>
  IAsyncOperation_1__RemoteSystems_IRemoteSystem_Base = interface(IInspectable)
  ['{0D39F546-0ECA-5236-A5CA-7E3660658462}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem; safecall;
    function GetResults: RemoteSystems_IRemoteSystem; safecall;
    property Completed: AsyncOperationCompletedHandler_1__RemoteSystems_IRemoteSystem read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.IRemoteSystem>
  IAsyncOperation_1__RemoteSystems_IRemoteSystem = interface(IAsyncOperation_1__RemoteSystems_IRemoteSystem_Base)
  ['{362CDF3D-FCE5-5BD7-AD39-508002EF07B1}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemAddedEventArgs_Delegate_Base = interface(IUnknown)
  ['{A9B98F4A-B63D-5D07-92BB-E2ACB39455D1}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemWatcher; args: RemoteSystems_IRemoteSystemAddedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemAddedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemAddedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemAddedEventArgs_Delegate_Base)
  ['{72DA44DC-F0EA-5F1A-96C3-DAF4BE2C1E41}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemUpdatedEventArgs
  RemoteSystems_IRemoteSystemUpdatedEventArgs = interface(IInspectable)
  ['{7502FF0E-DBCB-4155-B4CA-B30A04F27627}']
    function get_RemoteSystem: RemoteSystems_IRemoteSystem; safecall;
    property RemoteSystem: RemoteSystems_IRemoteSystem read get_RemoteSystem;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemUpdatedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{88F9D23F-8946-5A1E-8DA1-82C66982A6D2}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemWatcher; args: RemoteSystems_IRemoteSystemUpdatedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemUpdatedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemUpdatedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemUpdatedEventArgs_Delegate_Base)
  ['{2B52C566-64E8-53FC-AB07-63625590A415}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemRemovedEventArgs_Delegate_Base = interface(IUnknown)
  ['{C290FB5A-3ED0-5858-AF19-F85553CB96B8}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemWatcher; args: RemoteSystems_IRemoteSystemRemovedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemRemovedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemRemovedEventArgs = interface(TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemRemovedEventArgs_Delegate_Base)
  ['{C61924BF-05D0-5750-8BDA-4447DF035B81}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemWatcher
  RemoteSystems_IRemoteSystemWatcher = interface(IInspectable)
  ['{5D600C7E-2C07-48C5-889C-455D2B099771}']
    procedure Start; safecall;
    procedure Stop; safecall;
    function add_RemoteSystemAdded(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemAddedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RemoteSystemAdded(token: EventRegistrationToken); safecall;
    function add_RemoteSystemUpdated(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RemoteSystemUpdated(token: EventRegistrationToken); safecall;
    function add_RemoteSystemRemoved(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemRemovedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RemoteSystemRemoved(token: EventRegistrationToken); safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemFilter>
  IIterator_1__RemoteSystems_IRemoteSystemFilter_Base = interface(IInspectable)
  ['{6A2C5AEF-9F30-58AE-A6CB-9AC9C8092A41}']
    function get_Current: RemoteSystems_IRemoteSystemFilter; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PRemoteSystems_IRemoteSystemFilter): Cardinal; safecall;
    property Current: RemoteSystems_IRemoteSystemFilter read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.RemoteSystems.IRemoteSystemFilter>
  IIterator_1__RemoteSystems_IRemoteSystemFilter = interface(IIterator_1__RemoteSystems_IRemoteSystemFilter_Base)
  ['{6A2C5AEF-9F30-58AE-A6CB-9AC9C8092A41}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemFilter>
  IIterable_1__RemoteSystems_IRemoteSystemFilter_Base = interface(IInspectable)
  ['{13966C92-A8DE-50C0-B16B-00C2C48F5F37}']
    function First: IIterator_1__RemoteSystems_IRemoteSystemFilter; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.RemoteSystems.IRemoteSystemFilter>
  IIterable_1__RemoteSystems_IRemoteSystemFilter = interface(IIterable_1__RemoteSystems_IRemoteSystemFilter_Base)
  ['{13966C92-A8DE-50C0-B16B-00C2C48F5F37}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.RemoteSystemAccessStatus>
  AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus_Delegate_Base = interface(IUnknown)
  ['{543A221D-EF39-57F5-9741-B052DBC29249}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__RemoteSystems_RemoteSystemAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.RemoteSystems.RemoteSystemAccessStatus>
  AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus = interface(AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.RemoteSystemAccessStatus>
  IAsyncOperation_1__RemoteSystems_RemoteSystemAccessStatus_Base = interface(IInspectable)
  ['{D76DA678-DD76-5460-8745-915B4410C905}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus; safecall;
    function GetResults: RemoteSystems_RemoteSystemAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__RemoteSystems_RemoteSystemAccessStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.RemoteSystems.RemoteSystemAccessStatus>
  IAsyncOperation_1__RemoteSystems_RemoteSystemAccessStatus = interface(IAsyncOperation_1__RemoteSystems_RemoteSystemAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemStatusTypeFilter
  RemoteSystems_IRemoteSystemStatusTypeFilter = interface(IInspectable)
  ['{0C39514E-CBB6-4777-8534-2E0C521AFFA2}']
    function get_RemoteSystemStatusType: RemoteSystems_RemoteSystemStatusType; safecall;
    property RemoteSystemStatusType: RemoteSystems_RemoteSystemStatusType read get_RemoteSystemStatusType;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemEnumerationCompletedEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs = interface(IUnknown)
  ['{D65C0831-D618-5EDC-9E76-E0C342F03609}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemWatcher; args: RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemWatcherErrorOccurredEventArgs
  RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs = interface(IInspectable)
  ['{74C5C6AF-5114-4426-9216-20D81F8519AE}']
    function get_Error: RemoteSystems_RemoteSystemWatcherError; safecall;
    property Error: RemoteSystems_RemoteSystemWatcherError read get_Error;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemWatcher,Windows.System.RemoteSystems.IRemoteSystemWatcherErrorOccurredEventArgs>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs = interface(IUnknown)
  ['{FB9DDAC4-24F9-5AAD-8DB1-66EA6AAB7147}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemWatcher; args: RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemWatcher2
  RemoteSystems_IRemoteSystemWatcher2 = interface(IInspectable)
  ['{73436700-19CA-48F9-A4CD-780F7AD58C71}']
    function add_EnumerationCompleted(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemEnumerationCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_EnumerationCompleted(token: EventRegistrationToken); safecall;
    function add_ErrorOccurred(handler: TypedEventHandler_2__RemoteSystems_IRemoteSystemWatcher__RemoteSystems_IRemoteSystemWatcherErrorOccurredEventArgs): EventRegistrationToken; safecall;
    procedure remove_ErrorOccurred(token: EventRegistrationToken); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemWatcher3
  RemoteSystems_IRemoteSystemWatcher3 = interface(IInspectable)
  ['{F79C0FCF-A913-55D3-8413-418FCF15BA54}']
    function get_User: IUser; safecall;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.System.RemoteSystems.IRemoteSystemWebAccountFilter
  RemoteSystems_IRemoteSystemWebAccountFilter = interface(IInspectable)
  ['{3FB75873-87C8-5D8F-977E-F69F96D67238}']
    function get_Account: IWebAccount; safecall;
    property Account: IWebAccount read get_Account;
  end;

  // UsedAPI Interface
  // Windows.System.Threading.Core.IPreallocatedWorkItem
  Threading_Core_IPreallocatedWorkItem = interface(IInspectable)
  ['{B6DAA9FC-BC5B-401A-A8B2-6E754D14DAA6}']
    function RunAsync: IAsyncAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Threading.WorkItemHandler
  Threading_WorkItemHandler = interface(IUnknown)
  ['{1D1A8B8B-FA66-414F-9CBD-B65FC99D17FA}']
    procedure Invoke(operation: IAsyncAction); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Threading.Core.ISignalNotifier
  Threading_Core_ISignalNotifier = interface(IInspectable)
  ['{14285E06-63A7-4713-B6D9-62F64B56FB8B}']
    procedure Enable; safecall;
    procedure Terminate; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Threading.Core.SignalHandler
  Threading_Core_SignalHandler = interface(IUnknown)
  ['{923C402E-4721-440E-9DDA-55B6F2E07710}']
    procedure Invoke(signalNotifier: Threading_Core_ISignalNotifier; timedOut: Boolean); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Threading.IThreadPoolTimer
  Threading_IThreadPoolTimer = interface(IInspectable)
  ['{594EBE78-55EA-4A88-A50D-3402AE1F9CF2}']
    function get_Period: TimeSpan; safecall;
    function get_Delay: TimeSpan; safecall;
    procedure Cancel; safecall;
    property Delay: TimeSpan read get_Delay;
    property Period: TimeSpan read get_Period;
  end;

  // UsedAPI Interface
  // Windows.System.Threading.TimerElapsedHandler
  Threading_TimerElapsedHandler = interface(IUnknown)
  ['{FAAEA667-FBEB-49CB-ADB2-71184C556E43}']
    procedure Invoke(timer: Threading_IThreadPoolTimer); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Threading.TimerDestroyedHandler
  Threading_TimerDestroyedHandler = interface(IUnknown)
  ['{34ED19FA-8384-4EB9-8209-FB5094EEEC35}']
    procedure Invoke(timer: Threading_IThreadPoolTimer); safecall;
  end;

  // UsedAPI Interface
  // Windows.System.Update.ISystemUpdateItem
  Update_ISystemUpdateItem = interface(IInspectable)
  ['{779740EB-5624-519E-A8E2-09E9173B3FB7}']
    function get_State: Update_SystemUpdateItemState; safecall;
    function get_Title: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_Id: HSTRING; safecall;
    function get_Revision: Cardinal; safecall;
    function get_DownloadProgress: Double; safecall;
    function get_InstallProgress: Double; safecall;
    function get_ExtendedError: HRESULT; safecall;
    property Description: HSTRING read get_Description;
    property DownloadProgress: Double read get_DownloadProgress;
    property ExtendedError: HRESULT read get_ExtendedError;
    property Id: HSTRING read get_Id;
    property InstallProgress: Double read get_InstallProgress;
    property Revision: Cardinal read get_Revision;
    property State: Update_SystemUpdateItemState read get_State;
    property Title: HSTRING read get_Title;
  end;

  // UsedAPI Interface
  // Windows.System.Update.ISystemUpdateLastErrorInfo
  Update_ISystemUpdateLastErrorInfo = interface(IInspectable)
  ['{7EE887F7-8A44-5B6E-BD07-7AECE4116EA9}']
    function get_State: Update_SystemUpdateManagerState; safecall;
    function get_ExtendedError: HRESULT; safecall;
    function get_IsInteractive: Boolean; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
    property IsInteractive: Boolean read get_IsInteractive;
    property State: Update_SystemUpdateManagerState read get_State;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.System.Update.ISystemUpdateItem>
  IIterator_1__Update_ISystemUpdateItem = interface(IInspectable)
  ['{B9D20D2D-9C3A-582F-BA35-51887753CFAF}']
    function get_Current: Update_ISystemUpdateItem; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PUpdate_ISystemUpdateItem): Cardinal; safecall;
    property Current: Update_ISystemUpdateItem read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.System.Update.ISystemUpdateItem>
  IIterable_1__Update_ISystemUpdateItem = interface(IInspectable)
  ['{FB0AC861-6D8C-5223-B783-72CB117AEF60}']
    function First: IIterator_1__Update_ISystemUpdateItem; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.System.Update.ISystemUpdateItem>
  IVectorView_1__Update_ISystemUpdateItem = interface(IInspectable)
  ['{13B2CCB4-C499-5E31-86C2-0C1704C66BCE}']
    function GetAt(index: Cardinal): Update_ISystemUpdateItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Update_ISystemUpdateItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PUpdate_ISystemUpdateItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.System.UserProfile.IAdvertisingManagerForUser
  UserProfile_IAdvertisingManagerForUser = interface(IInspectable)
  ['{928BF3D0-CF7C-4AB0-A7DC-6DC5BCD44252}']
    function get_AdvertisingId: HSTRING; safecall;
    function get_User: IUser; safecall;
    property AdvertisingId: HSTRING read get_AdvertisingId;
    property User: IUser read get_User;
  end;

  // UsedAPI Interface
  // Windows.System.UserProfile.IAssignedAccessSettings
  UserProfile_IAssignedAccessSettings = interface(IInspectable)
  ['{1BC57F1C-E971-5757-B8E0-512F8B8C46D2}']
    function get_IsEnabled: Boolean; safecall;
    function get_IsSingleAppKioskMode: Boolean; safecall;
    function get_User: IUser; safecall;
    property IsEnabled: Boolean read get_IsEnabled;
    property IsSingleAppKioskMode: Boolean read get_IsSingleAppKioskMode;
    property User: IUser read get_User;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IDiagnosticsSettings
  [WinRTClassNameAttribute(SWindows_System_UserProfile_DiagnosticsSettings)]
  UserProfile_IDiagnosticsSettings = interface(IInspectable)
  ['{E5E9ECCD-2711-44E0-973C-491D78048D24}']
    function get_CanUseDiagnosticsToTailorExperiences: Boolean; safecall;
    function get_User: IUser; safecall;
    property CanUseDiagnosticsToTailorExperiences: Boolean read get_CanUseDiagnosticsToTailorExperiences;
    property User: IUser read get_User;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IDiagnosticsSettingsStatics
  [WinRTClassNameAttribute(SWindows_System_UserProfile_DiagnosticsSettings)]
  UserProfile_IDiagnosticsSettingsStatics = interface(IInspectable)
  ['{72D2E80F-5390-4793-990B-3CCC7D6AC9C8}']
    function GetDefault: UserProfile_IDiagnosticsSettings; safecall;
    function GetForUser(user: IUser): UserProfile_IDiagnosticsSettings; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IFirstSignInSettings
  [WinRTClassNameAttribute(SWindows_System_UserProfile_FirstSignInSettings)]
  UserProfile_IFirstSignInSettings = interface(IInspectable)
  ['{3E945153-3A5E-452E-A601-F5BAAD2A4870}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IFirstSignInSettingsStatics
  [WinRTClassNameAttribute(SWindows_System_UserProfile_FirstSignInSettings)]
  UserProfile_IFirstSignInSettingsStatics = interface(IInspectable)
  ['{1CE18F0F-1C41-4EA0-B7A2-6F0C1C7E8438}']
    function GetDefault: UserProfile_IFirstSignInSettings; safecall;
  end;

  // UsedAPI Interface
  // Windows.System.UserProfile.IGlobalizationPreferencesForUser
  UserProfile_IGlobalizationPreferencesForUser = interface(IInspectable)
  ['{150F0795-4F6E-40BA-A010-E27D81BDA7F5}']
    function get_User: IUser; safecall;
    function get_Calendars: IVectorView_1__HSTRING; safecall;
    function get_Clocks: IVectorView_1__HSTRING; safecall;
    function get_Currencies: IVectorView_1__HSTRING; safecall;
    function get_Languages: IVectorView_1__HSTRING; safecall;
    function get_HomeGeographicRegion: HSTRING; safecall;
    function get_WeekStartsOn: DayOfWeek; safecall;
    property Calendars: IVectorView_1__HSTRING read get_Calendars;
    property Clocks: IVectorView_1__HSTRING read get_Clocks;
    property Currencies: IVectorView_1__HSTRING read get_Currencies;
    property HomeGeographicRegion: HSTRING read get_HomeGeographicRegion;
    property Languages: IVectorView_1__HSTRING read get_Languages;
    property User: IUser read get_User;
    property WeekStartsOn: DayOfWeek read get_WeekStartsOn;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IGlobalizationPreferencesStatics
  [WinRTClassNameAttribute(SWindows_System_UserProfile_GlobalizationPreferences)]
  UserProfile_IGlobalizationPreferencesStatics = interface(IInspectable)
  ['{01BF4326-ED37-4E96-B0E9-C1340D1EA158}']
    function get_Calendars: IVectorView_1__HSTRING; safecall;
    function get_Clocks: IVectorView_1__HSTRING; safecall;
    function get_Currencies: IVectorView_1__HSTRING; safecall;
    function get_Languages: IVectorView_1__HSTRING; safecall;
    function get_HomeGeographicRegion: HSTRING; safecall;
    function get_WeekStartsOn: DayOfWeek; safecall;
    property Calendars: IVectorView_1__HSTRING read get_Calendars;
    property Clocks: IVectorView_1__HSTRING read get_Clocks;
    property Currencies: IVectorView_1__HSTRING read get_Currencies;
    property HomeGeographicRegion: HSTRING read get_HomeGeographicRegion;
    property Languages: IVectorView_1__HSTRING read get_Languages;
    property WeekStartsOn: DayOfWeek read get_WeekStartsOn;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IGlobalizationPreferencesStatics2
  [WinRTClassNameAttribute(SWindows_System_UserProfile_GlobalizationPreferences)]
  UserProfile_IGlobalizationPreferencesStatics2 = interface(IInspectable)
  ['{FCCE85F1-4300-4CD0-9CAC-1A8E7B7E18F4}']
    function TrySetHomeGeographicRegion(region: HSTRING): Boolean; safecall;
    function TrySetLanguages(languageTags: IIterable_1__HSTRING): Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IGlobalizationPreferencesStatics3
  [WinRTClassNameAttribute(SWindows_System_UserProfile_GlobalizationPreferences)]
  UserProfile_IGlobalizationPreferencesStatics3 = interface(IInspectable)
  ['{1E059733-35F5-40D8-B9E8-AEF3EF856FCE}']
    function GetForUser(user: IUser): UserProfile_IGlobalizationPreferencesForUser; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.UserProfile.SetImageFeedResult>
  AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult_Delegate_Base = interface(IUnknown)
  ['{F214731A-1305-5B44-932C-AF9A1E4D78C9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__UserProfile_SetImageFeedResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.UserProfile.SetImageFeedResult>
  AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult = interface(AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.UserProfile.SetImageFeedResult>
  IAsyncOperation_1__UserProfile_SetImageFeedResult_Base = interface(IInspectable)
  ['{5361BFC9-0740-544A-9797-1FFE5E73C54E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult; safecall;
    function GetResults: UserProfile_SetImageFeedResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__UserProfile_SetImageFeedResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.UserProfile.SetImageFeedResult>
  IAsyncOperation_1__UserProfile_SetImageFeedResult = interface(IAsyncOperation_1__UserProfile_SetImageFeedResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.ILockScreenImageFeedStatics
  [WinRTClassNameAttribute(SWindows_System_UserProfile_LockScreen)]
  UserProfile_ILockScreenImageFeedStatics = interface(IInspectable)
  ['{2C0D73F6-03A9-41A6-9B01-495251FF51D5}']
    function RequestSetImageFeedAsync(syndicationFeedUri: IUriRuntimeClass): IAsyncOperation_1__UserProfile_SetImageFeedResult; safecall;
    function TryRemoveImageFeed: Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.ILockScreenStatics
  [WinRTClassNameAttribute(SWindows_System_UserProfile_LockScreen)]
  UserProfile_ILockScreenStatics = interface(IInspectable)
  ['{3EE9D3AD-B607-40AE-B426-7631D9821269}']
    function get_OriginalImageFile: IUriRuntimeClass; safecall;
    function GetImageStream: IRandomAccessStream; safecall;
    function SetImageFileAsync(value: IStorageFile): IAsyncAction; safecall;
    function SetImageStreamAsync(value: IRandomAccessStream): IAsyncAction; safecall;
    property OriginalImageFile: IUriRuntimeClass read get_OriginalImageFile;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.UserProfile.SetAccountPictureResult>
  AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult_Delegate_Base = interface(IUnknown)
  ['{603F3E31-7A51-518C-9280-C188EA4213D8}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__UserProfile_SetAccountPictureResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.System.UserProfile.SetAccountPictureResult>
  AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult = interface(AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.System.UserProfile.SetAccountPictureResult>
  IAsyncOperation_1__UserProfile_SetAccountPictureResult_Base = interface(IInspectable)
  ['{6809E406-6D3B-5164-8F32-B845B0781405}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult; safecall;
    function GetResults: UserProfile_SetAccountPictureResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__UserProfile_SetAccountPictureResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.System.UserProfile.SetAccountPictureResult>
  IAsyncOperation_1__UserProfile_SetAccountPictureResult = interface(IAsyncOperation_1__UserProfile_SetAccountPictureResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IUserInformationStatics
  [WinRTClassNameAttribute(SWindows_System_UserProfile_UserInformation)]
  UserProfile_IUserInformationStatics = interface(IInspectable)
  ['{77F3A910-48FA-489C-934E-2AE85BA8F772}']
    function get_AccountPictureChangeEnabled: Boolean; safecall;
    function get_NameAccessAllowed: Boolean; safecall;
    function GetAccountPicture(kind: UserProfile_AccountPictureKind): IStorageFile; safecall;
    function SetAccountPictureAsync(image: IStorageFile): IAsyncOperation_1__UserProfile_SetAccountPictureResult; safecall;
    function SetAccountPicturesAsync(smallImage: IStorageFile; largeImage: IStorageFile; video: IStorageFile): IAsyncOperation_1__UserProfile_SetAccountPictureResult; safecall;
    function SetAccountPictureFromStreamAsync(image: IRandomAccessStream): IAsyncOperation_1__UserProfile_SetAccountPictureResult; safecall;
    function SetAccountPicturesFromStreamsAsync(smallImage: IRandomAccessStream; largeImage: IRandomAccessStream; video: IRandomAccessStream): IAsyncOperation_1__UserProfile_SetAccountPictureResult; safecall;
    function add_AccountPictureChanged(changeHandler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_AccountPictureChanged(token: EventRegistrationToken); safecall;
    function GetDisplayNameAsync: IAsyncOperation_1__HSTRING; safecall;
    function GetFirstNameAsync: IAsyncOperation_1__HSTRING; safecall;
    function GetLastNameAsync: IAsyncOperation_1__HSTRING; safecall;
    function GetPrincipalNameAsync: IAsyncOperation_1__HSTRING; safecall;
    function GetSessionInitiationProtocolUriAsync: IAsyncOperation_1__IUriRuntimeClass; safecall;
    function GetDomainNameAsync: IAsyncOperation_1__HSTRING; safecall;
    property AccountPictureChangeEnabled: Boolean read get_AccountPictureChangeEnabled;
    property NameAccessAllowed: Boolean read get_NameAccessAllowed;
  end deprecated;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IUserProfilePersonalizationSettings
  [WinRTClassNameAttribute(SWindows_System_UserProfile_UserProfilePersonalizationSettings)]
  UserProfile_IUserProfilePersonalizationSettings = interface(IInspectable)
  ['{8CEDDAB4-7998-46D5-8DD3-184F1C5F9AB9}']
    function TrySetLockScreenImageAsync(imageFile: IStorageFile): IAsyncOperation_1__Boolean; safecall;
    function TrySetWallpaperImageAsync(imageFile: IStorageFile): IAsyncOperation_1__Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.System.UserProfile.IUserProfilePersonalizationSettingsStatics
  [WinRTClassNameAttribute(SWindows_System_UserProfile_UserProfilePersonalizationSettings)]
  UserProfile_IUserProfilePersonalizationSettingsStatics = interface(IInspectable)
  ['{91ACB841-5037-454B-9883-BB772D08DD16}']
    function get_Current: UserProfile_IUserProfilePersonalizationSettings; safecall;
    function IsSupported: Boolean; safecall;
    property Current: UserProfile_IUserProfilePersonalizationSettings read get_Current;
  end;

  // Windows.System.AppDiagnosticInfo
  // DualAPI
  // Implements: Windows.System.IAppDiagnosticInfo
  // Implements: Windows.System.IAppDiagnosticInfo2
  // Implements: Windows.System.IAppDiagnosticInfo3
  // Statics: "Windows.System.IAppDiagnosticInfoStatics"
  // Statics: "Windows.System.IAppDiagnosticInfoStatics2"
  TAppDiagnosticInfo = class(TWinRTGenericImportS2<IAppDiagnosticInfoStatics, IAppDiagnosticInfoStatics2>)
  public
    // -> IAppDiagnosticInfoStatics
    class function RequestInfoAsync: IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; static; inline;

    // -> IAppDiagnosticInfoStatics2
    class function CreateWatcher: IAppDiagnosticInfoWatcher; static; inline;
    class function RequestAccessAsync: IAsyncOperation_1__DiagnosticAccessStatus; static; inline;
    class function RequestInfoForPackageAsync(packageFamilyName: HSTRING): IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; static; inline;
    class function RequestInfoForAppAsync: IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; overload; static; inline;
    class function RequestInfoForAppAsync(appUserModelId: HSTRING): IAsyncOperation_1__IVector_1__IAppDiagnosticInfo; overload; static; inline;
  end;

  // Windows.System.DateTimeSettings
  // DualAPI
  // Statics: "Windows.System.IDateTimeSettingsStatics"
  TDateTimeSettings = class(TWinRTGenericImportS<IDateTimeSettingsStatics>)
  public
    // -> IDateTimeSettingsStatics
    class procedure SetSystemDateTime(utcDateTime: DateTime); static; inline;
  end;

  // Windows.System.Diagnostics.ProcessDiagnosticInfo
  // DualAPI
  // Implements: Windows.System.Diagnostics.IProcessDiagnosticInfo
  // Implements: Windows.System.Diagnostics.IProcessDiagnosticInfo2
  // Statics: "Windows.System.Diagnostics.IProcessDiagnosticInfoStatics"
  // Statics: "Windows.System.Diagnostics.IProcessDiagnosticInfoStatics2"
  TDiagnostics_ProcessDiagnosticInfo = class(TWinRTGenericImportS2<Diagnostics_IProcessDiagnosticInfoStatics, Diagnostics_IProcessDiagnosticInfoStatics2>)
  public
    // -> Diagnostics_IProcessDiagnosticInfoStatics
    class function GetForProcesses: IVectorView_1__Diagnostics_IProcessDiagnosticInfo; static; inline;
    class function GetForCurrentProcess: Diagnostics_IProcessDiagnosticInfo; static; inline;

    // -> Diagnostics_IProcessDiagnosticInfoStatics2
    class function TryGetForProcessId(processId: Cardinal): Diagnostics_IProcessDiagnosticInfo; static; inline;
  end;

  // Windows.System.Diagnostics.SystemDiagnosticInfo
  // DualAPI
  // Implements: Windows.System.Diagnostics.ISystemDiagnosticInfo
  // Statics: "Windows.System.Diagnostics.ISystemDiagnosticInfoStatics"
  TDiagnostics_SystemDiagnosticInfo = class(TWinRTGenericImportS<Diagnostics_ISystemDiagnosticInfoStatics>)
  public
    // -> Diagnostics_ISystemDiagnosticInfoStatics
    class function GetForCurrentSystem: Diagnostics_ISystemDiagnosticInfo; static; inline;
  end;

  // Windows.System.Diagnostics.Telemetry.PlatformTelemetryClient
  // DualAPI
  // Statics: "Windows.System.Diagnostics.Telemetry.IPlatformTelemetryClientStatics"
  TDiagnostics_Telemetry_PlatformTelemetryClient = class(TWinRTGenericImportS<Diagnostics_Telemetry_IPlatformTelemetryClientStatics>)
  public
    // -> Diagnostics_Telemetry_IPlatformTelemetryClientStatics
    class function Register(id: HSTRING): Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult; overload; static; inline;
    class function Register(id: HSTRING; settings: Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings): Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult; overload; static; inline;
  end;

  // Windows.System.Diagnostics.Telemetry.PlatformTelemetryRegistrationSettings
  // DualAPI
  // Implements: Windows.System.Diagnostics.Telemetry.IPlatformTelemetryRegistrationSettings
  // Instantiable: "Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings"
  TDiagnostics_Telemetry_PlatformTelemetryRegistrationSettings = class(TWinRTGenericImportI<Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings>) end;

  // Windows.System.Diagnostics.TraceReporting.PlatformDiagnosticActions
  // DualAPI
  // Statics: "Windows.System.Diagnostics.TraceReporting.IPlatformDiagnosticActionsStatics"
  TDiagnostics_TraceReporting_PlatformDiagnosticActions = class(TWinRTGenericImportS<Diagnostics_TraceReporting_IPlatformDiagnosticActionsStatics>)
  public
    // -> Diagnostics_TraceReporting_IPlatformDiagnosticActionsStatics
    class function IsScenarioEnabled(scenarioId: TGuid): Boolean; static; inline;
    class function TryEscalateScenario(scenarioId: TGuid; escalationType: Diagnostics_TraceReporting_PlatformDiagnosticEscalationType; outputDirectory: HSTRING; timestampOutputDirectory: Boolean; forceEscalationUpload: Boolean; triggers: IMapView_2__HSTRING__HSTRING): Boolean; static; inline;
    class function DownloadLatestSettingsForNamespace(partner: HSTRING; feature: HSTRING; isScenarioNamespace: Boolean; downloadOverCostedNetwork: Boolean; downloadOverBattery: Boolean): Diagnostics_TraceReporting_PlatformDiagnosticActionState; static; inline;
    class function GetActiveScenarioList: IVectorView_1__TGuid; static; inline;
    class function ForceUpload(latency: Diagnostics_TraceReporting_PlatformDiagnosticEventBufferLatencies; uploadOverCostedNetwork: Boolean; uploadOverBattery: Boolean): Diagnostics_TraceReporting_PlatformDiagnosticActionState; static; inline;
    class function IsTraceRunning(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType; scenarioId: TGuid; traceProfileHash: UInt64): Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotState; static; inline;
    class function GetActiveTraceRuntime(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType): Diagnostics_TraceReporting_IPlatformDiagnosticTraceRuntimeInfo; static; inline;
    class function GetKnownTraceList(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType): IVectorView_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo; static; inline;
  end;

  // Windows.System.DispatcherQueue
  // DualAPI
  // Implements: Windows.System.IDispatcherQueue
  // Implements: Windows.System.IDispatcherQueue2
  // Statics: "Windows.System.IDispatcherQueueStatics"
  TDispatcherQueue = class(TWinRTGenericImportS<IDispatcherQueueStatics>)
  public
    // -> IDispatcherQueueStatics
    class function GetForCurrentThread: IDispatcherQueue; static; inline;
  end;

  // Windows.System.DispatcherQueueController
  // DualAPI
  // Implements: Windows.System.IDispatcherQueueController
  // Statics: "Windows.System.IDispatcherQueueControllerStatics"
  TDispatcherQueueController = class(TWinRTGenericImportS<IDispatcherQueueControllerStatics>)
  public
    // -> IDispatcherQueueControllerStatics
    class function CreateOnDedicatedThread: IDispatcherQueueController; static; inline;
  end;

  // Windows.System.KnownUserProperties
  // DualAPI
  // Statics: "Windows.System.IKnownUserPropertiesStatics"
  TKnownUserProperties = class(TWinRTGenericImportS<IKnownUserPropertiesStatics>)
  public
    // -> IKnownUserPropertiesStatics
    class function get_DisplayName: HSTRING; static; inline;
    class function get_FirstName: HSTRING; static; inline;
    class function get_LastName: HSTRING; static; inline;
    class function get_ProviderName: HSTRING; static; inline;
    class function get_AccountName: HSTRING; static; inline;
    class function get_GuestHost: HSTRING; static; inline;
    class function get_PrincipalName: HSTRING; static; inline;
    class function get_DomainName: HSTRING; static; inline;
    class function get_SessionInitiationProtocolUri: HSTRING; static; inline;
    class property AccountName: HSTRING read get_AccountName;
    class property DisplayName: HSTRING read get_DisplayName;
    class property DomainName: HSTRING read get_DomainName;
    class property FirstName: HSTRING read get_FirstName;
    class property GuestHost: HSTRING read get_GuestHost;
    class property LastName: HSTRING read get_LastName;
    class property PrincipalName: HSTRING read get_PrincipalName;
    class property ProviderName: HSTRING read get_ProviderName;
    class property SessionInitiationProtocolUri: HSTRING read get_SessionInitiationProtocolUri;
  end;

  // Windows.System.ProcessLauncher
  // DualAPI
  // Statics: "Windows.System.IProcessLauncherStatics"
  TProcessLauncher = class(TWinRTGenericImportS<IProcessLauncherStatics>)
  public
    // -> IProcessLauncherStatics
    class function RunToCompletionAsync(fileName: HSTRING; args: HSTRING): IAsyncOperation_1__IProcessLauncherResult; overload; static; inline;
    class function RunToCompletionAsync(fileName: HSTRING; args: HSTRING; options: IProcessLauncherOptions): IAsyncOperation_1__IProcessLauncherResult; overload; static; inline;
  end;

  // Windows.System.ProcessLauncherOptions
  // DualAPI
  // Implements: Windows.System.IProcessLauncherOptions
  // Instantiable: "IProcessLauncherOptions"
  TProcessLauncherOptions = class(TWinRTGenericImportI<IProcessLauncherOptions>) end;

  // Windows.System.Profile.PlatformDiagnosticsAndUsageDataSettings
  // DualAPI
  // Statics: "Windows.System.Profile.IPlatformDiagnosticsAndUsageDataSettingsStatics"
  TProfile_PlatformDiagnosticsAndUsageDataSettings = class(TWinRTGenericImportS<Profile_IPlatformDiagnosticsAndUsageDataSettingsStatics>)
  public
    // -> Profile_IPlatformDiagnosticsAndUsageDataSettingsStatics
    class function get_CollectionLevel: Profile_PlatformDataCollectionLevel; static; inline;
    class function add_CollectionLevelChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_CollectionLevelChanged(token: EventRegistrationToken); static; inline;
    class function CanCollectDiagnostics(level: Profile_PlatformDataCollectionLevel): Boolean; static; inline;
    class property CollectionLevel: Profile_PlatformDataCollectionLevel read get_CollectionLevel;
  end;

  // Windows.System.Profile.SharedModeSettings
  // DualAPI
  // Statics: "Windows.System.Profile.ISharedModeSettingsStatics"
  // Statics: "Windows.System.Profile.ISharedModeSettingsStatics2"
  TProfile_SharedModeSettings = class(TWinRTGenericImportS2<Profile_ISharedModeSettingsStatics, Profile_ISharedModeSettingsStatics2>)
  public
    // -> Profile_ISharedModeSettingsStatics
    class function get_IsEnabled: Boolean; static; inline;
    class property IsEnabled: Boolean read get_IsEnabled;

    // -> Profile_ISharedModeSettingsStatics2
    class function get_ShouldAvoidLocalStorage: Boolean; static; inline;
    class property ShouldAvoidLocalStorage: Boolean read get_ShouldAvoidLocalStorage;
  end;

  // Windows.System.Profile.SystemManufacturers.SystemSupportInfo
  // DualAPI
  // Statics: "Windows.System.Profile.SystemManufacturers.ISystemSupportInfoStatics"
  // Statics: "Windows.System.Profile.SystemManufacturers.ISystemSupportInfoStatics2"
  TProfile_SystemManufacturers_SystemSupportInfo = class(TWinRTGenericImportS2<Profile_SystemManufacturers_ISystemSupportInfoStatics, Profile_SystemManufacturers_ISystemSupportInfoStatics2>)
  public
    // -> Profile_SystemManufacturers_ISystemSupportInfoStatics
    class function get_LocalSystemEdition: HSTRING; static; inline;
    class function get_OemSupportInfo: Profile_SystemManufacturers_IOemSupportInfo; static; inline;
    class property LocalSystemEdition: HSTRING read get_LocalSystemEdition;
    class property OemSupportInfo: Profile_SystemManufacturers_IOemSupportInfo read get_OemSupportInfo;

    // -> Profile_SystemManufacturers_ISystemSupportInfoStatics2
    class function get_LocalDeviceInfo: Profile_SystemManufacturers_ISystemSupportDeviceInfo; static; inline;
    class property LocalDeviceInfo: Profile_SystemManufacturers_ISystemSupportDeviceInfo read get_LocalDeviceInfo;
  end;

  // Windows.System.Profile.SystemSetupInfo
  // DualAPI
  // Statics: "Windows.System.Profile.ISystemSetupInfoStatics"
  TProfile_SystemSetupInfo = class(TWinRTGenericImportS<Profile_ISystemSetupInfoStatics>)
  public
    // -> Profile_ISystemSetupInfoStatics
    class function get_OutOfBoxExperienceState: Profile_SystemOutOfBoxExperienceState; static; inline;
    class function add_OutOfBoxExperienceStateChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_OutOfBoxExperienceStateChanged(token: EventRegistrationToken); static; inline;
    class property OutOfBoxExperienceState: Profile_SystemOutOfBoxExperienceState read get_OutOfBoxExperienceState;
  end;

  // Windows.System.ShutdownManager
  // DualAPI
  // Statics: "Windows.System.IShutdownManagerStatics"
  // Statics: "Windows.System.IShutdownManagerStatics2"
  TShutdownManager = class(TWinRTGenericImportS2<IShutdownManagerStatics, IShutdownManagerStatics2>)
  public
    // -> IShutdownManagerStatics
    class procedure BeginShutdown(shutdownKind: ShutdownKind; timeout: TimeSpan); static; inline;
    class procedure CancelShutdown; static; inline;

    // -> IShutdownManagerStatics2
    class function IsPowerStateSupported(powerState: PowerState): Boolean; static; inline;
    class procedure EnterPowerState(powerState: PowerState); overload; static; inline;
    class procedure EnterPowerState(powerState: PowerState; wakeUpAfter: TimeSpan); overload; static; inline;
  end;

  // Windows.System.User
  // DualAPI
  // Implements: Windows.System.IUser
  // Statics: "Windows.System.IUserStatics"
  TUser = class(TWinRTGenericImportS<IUserStatics>)
  public
    // -> IUserStatics
    class function CreateWatcher: IUserWatcher; static; inline;
    class function FindAllAsync: IAsyncOperation_1__IVectorView_1__IUser; overload; static; inline;
    class function FindAllAsync(&type: UserType): IAsyncOperation_1__IVectorView_1__IUser; overload; static; inline;
    class function FindAllAsync(&type: UserType; status: UserAuthenticationStatus): IAsyncOperation_1__IVectorView_1__IUser; overload; static; inline;
    class function GetFromId(nonRoamableId: HSTRING): IUser; static; inline;
  end;

  // Windows.System.UserDeviceAssociation
  // DualAPI
  // Statics: "Windows.System.IUserDeviceAssociationStatics"
  TUserDeviceAssociation = class(TWinRTGenericImportS<IUserDeviceAssociationStatics>)
  public
    // -> IUserDeviceAssociationStatics
    class function FindUserFromDeviceId(deviceId: HSTRING): IUser; static; inline;
    class function add_UserDeviceAssociationChanged(handler: EventHandler_1__IUserDeviceAssociationChangedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_UserDeviceAssociationChanged(token: EventRegistrationToken); static; inline;
  end;

  // Windows.System.UserPicker
  // DualAPI
  // Implements: Windows.System.IUserPicker
  // Statics: "Windows.System.IUserPickerStatics"
  // Instantiable: "IUserPicker"
  TUserPicker = class(TWinRTGenericImportSI<IUserPickerStatics, IUserPicker>)
  public
    // -> IUserPickerStatics
    class function IsSupported: Boolean; static; inline;
  end;

  // Windows.System.UserProfile.DiagnosticsSettings
  // DualAPI
  // Implements: Windows.System.UserProfile.IDiagnosticsSettings
  // Statics: "Windows.System.UserProfile.IDiagnosticsSettingsStatics"
  TUserProfile_DiagnosticsSettings = class(TWinRTGenericImportS<UserProfile_IDiagnosticsSettingsStatics>)
  public
    // -> UserProfile_IDiagnosticsSettingsStatics
    class function GetDefault: UserProfile_IDiagnosticsSettings; static; inline;
    class function GetForUser(user: IUser): UserProfile_IDiagnosticsSettings; static; inline;
  end;

  // Windows.System.UserProfile.FirstSignInSettings
  // DualAPI
  // Implements: Windows.System.UserProfile.IFirstSignInSettings
  // Implements: Windows.Foundation.Collections.IMapView`2<String,Object>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  // Statics: "Windows.System.UserProfile.IFirstSignInSettingsStatics"
  TUserProfile_FirstSignInSettings = class(TWinRTGenericImportS<UserProfile_IFirstSignInSettingsStatics>)
  public
    // -> UserProfile_IFirstSignInSettingsStatics
    class function GetDefault: UserProfile_IFirstSignInSettings; static; inline;
  end;

  // Windows.System.UserProfile.GlobalizationPreferences
  // DualAPI
  // Statics: "Windows.System.UserProfile.IGlobalizationPreferencesStatics"
  // Statics: "Windows.System.UserProfile.IGlobalizationPreferencesStatics2"
  // Statics: "Windows.System.UserProfile.IGlobalizationPreferencesStatics3"
  TUserProfile_GlobalizationPreferences = class(TWinRTGenericImportS3<UserProfile_IGlobalizationPreferencesStatics, UserProfile_IGlobalizationPreferencesStatics2, UserProfile_IGlobalizationPreferencesStatics3>)
  public
    // -> UserProfile_IGlobalizationPreferencesStatics
    class function get_Calendars: IVectorView_1__HSTRING; static; inline;
    class function get_Clocks: IVectorView_1__HSTRING; static; inline;
    class function get_Currencies: IVectorView_1__HSTRING; static; inline;
    class function get_Languages: IVectorView_1__HSTRING; static; inline;
    class function get_HomeGeographicRegion: HSTRING; static; inline;
    class function get_WeekStartsOn: DayOfWeek; static; inline;
    class property Calendars: IVectorView_1__HSTRING read get_Calendars;
    class property Clocks: IVectorView_1__HSTRING read get_Clocks;
    class property Currencies: IVectorView_1__HSTRING read get_Currencies;
    class property HomeGeographicRegion: HSTRING read get_HomeGeographicRegion;
    class property Languages: IVectorView_1__HSTRING read get_Languages;
    class property WeekStartsOn: DayOfWeek read get_WeekStartsOn;

    // -> UserProfile_IGlobalizationPreferencesStatics2
    class function TrySetHomeGeographicRegion(region: HSTRING): Boolean; static; inline;
    class function TrySetLanguages(languageTags: IIterable_1__HSTRING): Boolean; static; inline;

    // -> UserProfile_IGlobalizationPreferencesStatics3
    class function GetForUser(user: IUser): UserProfile_IGlobalizationPreferencesForUser; static; inline;
  end;

  // Windows.System.UserProfile.LockScreen
  // DualAPI
  // Statics: "Windows.System.UserProfile.ILockScreenImageFeedStatics"
  // Statics: "Windows.System.UserProfile.ILockScreenStatics"
  TUserProfile_LockScreen = class(TWinRTGenericImportS2<UserProfile_ILockScreenImageFeedStatics, UserProfile_ILockScreenStatics>)
  public
    // -> UserProfile_ILockScreenImageFeedStatics
    class function RequestSetImageFeedAsync(syndicationFeedUri: IUriRuntimeClass): IAsyncOperation_1__UserProfile_SetImageFeedResult; static; inline;
    class function TryRemoveImageFeed: Boolean; static; inline;

    // -> UserProfile_ILockScreenStatics
    class function get_OriginalImageFile: IUriRuntimeClass; static; inline;
    class function GetImageStream: IRandomAccessStream; static; inline;
    class function SetImageFileAsync(value: IStorageFile): IAsyncAction; static; inline;
    class function SetImageStreamAsync(value: IRandomAccessStream): IAsyncAction; static; inline;
    class property OriginalImageFile: IUriRuntimeClass read get_OriginalImageFile;
  end;

  // Windows.System.UserProfile.UserInformation
  // DualAPI
  // Statics: "Windows.System.UserProfile.IUserInformationStatics"
  TUserProfile_UserInformation = class(TWinRTGenericImportS<UserProfile_IUserInformationStatics>)
  public
    // -> UserProfile_IUserInformationStatics
    class function get_AccountPictureChangeEnabled: Boolean; static; inline;
    class function get_NameAccessAllowed: Boolean; static; inline;
    class function GetAccountPicture(kind: UserProfile_AccountPictureKind): IStorageFile; static; inline;
    class function SetAccountPictureAsync(image: IStorageFile): IAsyncOperation_1__UserProfile_SetAccountPictureResult; static; inline;
    class function SetAccountPicturesAsync(smallImage: IStorageFile; largeImage: IStorageFile; video: IStorageFile): IAsyncOperation_1__UserProfile_SetAccountPictureResult; static; inline;
    class function SetAccountPictureFromStreamAsync(image: IRandomAccessStream): IAsyncOperation_1__UserProfile_SetAccountPictureResult; static; inline;
    class function SetAccountPicturesFromStreamsAsync(smallImage: IRandomAccessStream; largeImage: IRandomAccessStream; video: IRandomAccessStream): IAsyncOperation_1__UserProfile_SetAccountPictureResult; static; inline;
    class function add_AccountPictureChanged(changeHandler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_AccountPictureChanged(token: EventRegistrationToken); static; inline;
    class function GetDisplayNameAsync: IAsyncOperation_1__HSTRING; static; inline;
    class function GetFirstNameAsync: IAsyncOperation_1__HSTRING; static; inline;
    class function GetLastNameAsync: IAsyncOperation_1__HSTRING; static; inline;
    class function GetPrincipalNameAsync: IAsyncOperation_1__HSTRING; static; inline;
    class function GetSessionInitiationProtocolUriAsync: IAsyncOperation_1__IUriRuntimeClass; static; inline;
    class function GetDomainNameAsync: IAsyncOperation_1__HSTRING; static; inline;
    class property AccountPictureChangeEnabled: Boolean read get_AccountPictureChangeEnabled;
    class property NameAccessAllowed: Boolean read get_NameAccessAllowed;
  end;

  // Windows.System.UserProfile.UserProfilePersonalizationSettings
  // DualAPI
  // Implements: Windows.System.UserProfile.IUserProfilePersonalizationSettings
  // Statics: "Windows.System.UserProfile.IUserProfilePersonalizationSettingsStatics"
  TUserProfile_UserProfilePersonalizationSettings = class(TWinRTGenericImportS<UserProfile_IUserProfilePersonalizationSettingsStatics>)
  public
    // -> UserProfile_IUserProfilePersonalizationSettingsStatics
    class function get_Current: UserProfile_IUserProfilePersonalizationSettings; static; inline;
    class function IsSupported: Boolean; static; inline;
    class property Current: UserProfile_IUserProfilePersonalizationSettings read get_Current;
  end;

implementation

{ TAppDiagnosticInfo }

class function TAppDiagnosticInfo.RequestInfoAsync: IAsyncOperation_1__IVector_1__IAppDiagnosticInfo;
begin
  Result := Statics.RequestInfoAsync;
end;


class function TAppDiagnosticInfo.CreateWatcher: IAppDiagnosticInfoWatcher;
begin
  Result := Statics2.CreateWatcher;
end;

class function TAppDiagnosticInfo.RequestAccessAsync: IAsyncOperation_1__DiagnosticAccessStatus;
begin
  Result := Statics2.RequestAccessAsync;
end;

class function TAppDiagnosticInfo.RequestInfoForPackageAsync(packageFamilyName: HSTRING): IAsyncOperation_1__IVector_1__IAppDiagnosticInfo;
begin
  Result := Statics2.RequestInfoForPackageAsync(packageFamilyName);
end;

class function TAppDiagnosticInfo.RequestInfoForAppAsync: IAsyncOperation_1__IVector_1__IAppDiagnosticInfo;
begin
  Result := Statics2.RequestInfoForAppAsync;
end;

class function TAppDiagnosticInfo.RequestInfoForAppAsync(appUserModelId: HSTRING): IAsyncOperation_1__IVector_1__IAppDiagnosticInfo;
begin
  Result := Statics2.RequestInfoForAppAsync(appUserModelId);
end;


{ TDateTimeSettings }

class procedure TDateTimeSettings.SetSystemDateTime(utcDateTime: DateTime);
begin
  Statics.SetSystemDateTime(utcDateTime);
end;


{ TDiagnostics_ProcessDiagnosticInfo }

class function TDiagnostics_ProcessDiagnosticInfo.GetForProcesses: IVectorView_1__Diagnostics_IProcessDiagnosticInfo;
begin
  Result := Statics.GetForProcesses;
end;

class function TDiagnostics_ProcessDiagnosticInfo.GetForCurrentProcess: Diagnostics_IProcessDiagnosticInfo;
begin
  Result := Statics.GetForCurrentProcess;
end;


class function TDiagnostics_ProcessDiagnosticInfo.TryGetForProcessId(processId: Cardinal): Diagnostics_IProcessDiagnosticInfo;
begin
  Result := Statics2.TryGetForProcessId(processId);
end;


{ TDiagnostics_SystemDiagnosticInfo }

class function TDiagnostics_SystemDiagnosticInfo.GetForCurrentSystem: Diagnostics_ISystemDiagnosticInfo;
begin
  Result := Statics.GetForCurrentSystem;
end;


{ TDiagnostics_Telemetry_PlatformTelemetryClient }

class function TDiagnostics_Telemetry_PlatformTelemetryClient.Register(id: HSTRING): Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult;
begin
  Result := Statics.Register(id);
end;

class function TDiagnostics_Telemetry_PlatformTelemetryClient.Register(id: HSTRING; settings: Diagnostics_Telemetry_IPlatformTelemetryRegistrationSettings): Diagnostics_Telemetry_IPlatformTelemetryRegistrationResult;
begin
  Result := Statics.Register(id, settings);
end;


{ TDiagnostics_Telemetry_PlatformTelemetryRegistrationSettings }

{ TDiagnostics_TraceReporting_PlatformDiagnosticActions }

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.IsScenarioEnabled(scenarioId: TGuid): Boolean;
begin
  Result := Statics.IsScenarioEnabled(scenarioId);
end;

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.TryEscalateScenario(scenarioId: TGuid; escalationType: Diagnostics_TraceReporting_PlatformDiagnosticEscalationType; outputDirectory: HSTRING; timestampOutputDirectory: Boolean; forceEscalationUpload: Boolean; triggers: IMapView_2__HSTRING__HSTRING): Boolean;
begin
  Result := Statics.TryEscalateScenario(scenarioId, escalationType, outputDirectory, timestampOutputDirectory, forceEscalationUpload, triggers);
end;

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.DownloadLatestSettingsForNamespace(partner: HSTRING; feature: HSTRING; isScenarioNamespace: Boolean; downloadOverCostedNetwork: Boolean; downloadOverBattery: Boolean): Diagnostics_TraceReporting_PlatformDiagnosticActionState;
begin
  Result := Statics.DownloadLatestSettingsForNamespace(partner, feature, isScenarioNamespace, downloadOverCostedNetwork, downloadOverBattery);
end;

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.GetActiveScenarioList: IVectorView_1__TGuid;
begin
  Result := Statics.GetActiveScenarioList;
end;

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.ForceUpload(latency: Diagnostics_TraceReporting_PlatformDiagnosticEventBufferLatencies; uploadOverCostedNetwork: Boolean; uploadOverBattery: Boolean): Diagnostics_TraceReporting_PlatformDiagnosticActionState;
begin
  Result := Statics.ForceUpload(latency, uploadOverCostedNetwork, uploadOverBattery);
end;

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.IsTraceRunning(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType; scenarioId: TGuid; traceProfileHash: UInt64): Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotState;
begin
  Result := Statics.IsTraceRunning(slotType, scenarioId, traceProfileHash);
end;

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.GetActiveTraceRuntime(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType): Diagnostics_TraceReporting_IPlatformDiagnosticTraceRuntimeInfo;
begin
  Result := Statics.GetActiveTraceRuntime(slotType);
end;

class function TDiagnostics_TraceReporting_PlatformDiagnosticActions.GetKnownTraceList(slotType: Diagnostics_TraceReporting_PlatformDiagnosticTraceSlotType): IVectorView_1__Diagnostics_TraceReporting_IPlatformDiagnosticTraceInfo;
begin
  Result := Statics.GetKnownTraceList(slotType);
end;


{ TDispatcherQueue }

class function TDispatcherQueue.GetForCurrentThread: IDispatcherQueue;
begin
  Result := Statics.GetForCurrentThread;
end;


{ TDispatcherQueueController }

class function TDispatcherQueueController.CreateOnDedicatedThread: IDispatcherQueueController;
begin
  Result := Statics.CreateOnDedicatedThread;
end;


{ TKnownUserProperties }

class function TKnownUserProperties.get_DisplayName: HSTRING;
begin
  Result := Statics.get_DisplayName;
end;

class function TKnownUserProperties.get_FirstName: HSTRING;
begin
  Result := Statics.get_FirstName;
end;

class function TKnownUserProperties.get_LastName: HSTRING;
begin
  Result := Statics.get_LastName;
end;

class function TKnownUserProperties.get_ProviderName: HSTRING;
begin
  Result := Statics.get_ProviderName;
end;

class function TKnownUserProperties.get_AccountName: HSTRING;
begin
  Result := Statics.get_AccountName;
end;

class function TKnownUserProperties.get_GuestHost: HSTRING;
begin
  Result := Statics.get_GuestHost;
end;

class function TKnownUserProperties.get_PrincipalName: HSTRING;
begin
  Result := Statics.get_PrincipalName;
end;

class function TKnownUserProperties.get_DomainName: HSTRING;
begin
  Result := Statics.get_DomainName;
end;

class function TKnownUserProperties.get_SessionInitiationProtocolUri: HSTRING;
begin
  Result := Statics.get_SessionInitiationProtocolUri;
end;


{ TProcessLauncher }

class function TProcessLauncher.RunToCompletionAsync(fileName: HSTRING; args: HSTRING): IAsyncOperation_1__IProcessLauncherResult;
begin
  Result := Statics.RunToCompletionAsync(fileName, args);
end;

class function TProcessLauncher.RunToCompletionAsync(fileName: HSTRING; args: HSTRING; options: IProcessLauncherOptions): IAsyncOperation_1__IProcessLauncherResult;
begin
  Result := Statics.RunToCompletionAsync(fileName, args, options);
end;


{ TProcessLauncherOptions }

{ TProfile_PlatformDiagnosticsAndUsageDataSettings }

class function TProfile_PlatformDiagnosticsAndUsageDataSettings.get_CollectionLevel: Profile_PlatformDataCollectionLevel;
begin
  Result := Statics.get_CollectionLevel;
end;

class function TProfile_PlatformDiagnosticsAndUsageDataSettings.add_CollectionLevelChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_CollectionLevelChanged(handler);
end;

class procedure TProfile_PlatformDiagnosticsAndUsageDataSettings.remove_CollectionLevelChanged(token: EventRegistrationToken);
begin
  Statics.remove_CollectionLevelChanged(token);
end;

class function TProfile_PlatformDiagnosticsAndUsageDataSettings.CanCollectDiagnostics(level: Profile_PlatformDataCollectionLevel): Boolean;
begin
  Result := Statics.CanCollectDiagnostics(level);
end;


{ TProfile_SharedModeSettings }

class function TProfile_SharedModeSettings.get_IsEnabled: Boolean;
begin
  Result := Statics.get_IsEnabled;
end;


class function TProfile_SharedModeSettings.get_ShouldAvoidLocalStorage: Boolean;
begin
  Result := Statics2.get_ShouldAvoidLocalStorage;
end;


{ TProfile_SystemManufacturers_SystemSupportInfo }

class function TProfile_SystemManufacturers_SystemSupportInfo.get_LocalSystemEdition: HSTRING;
begin
  Result := Statics.get_LocalSystemEdition;
end;

class function TProfile_SystemManufacturers_SystemSupportInfo.get_OemSupportInfo: Profile_SystemManufacturers_IOemSupportInfo;
begin
  Result := Statics.get_OemSupportInfo;
end;


class function TProfile_SystemManufacturers_SystemSupportInfo.get_LocalDeviceInfo: Profile_SystemManufacturers_ISystemSupportDeviceInfo;
begin
  Result := Statics2.get_LocalDeviceInfo;
end;


{ TProfile_SystemSetupInfo }

class function TProfile_SystemSetupInfo.get_OutOfBoxExperienceState: Profile_SystemOutOfBoxExperienceState;
begin
  Result := Statics.get_OutOfBoxExperienceState;
end;

class function TProfile_SystemSetupInfo.add_OutOfBoxExperienceStateChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_OutOfBoxExperienceStateChanged(handler);
end;

class procedure TProfile_SystemSetupInfo.remove_OutOfBoxExperienceStateChanged(token: EventRegistrationToken);
begin
  Statics.remove_OutOfBoxExperienceStateChanged(token);
end;


{ TShutdownManager }

class procedure TShutdownManager.BeginShutdown(shutdownKind: ShutdownKind; timeout: TimeSpan);
begin
  Statics.BeginShutdown(shutdownKind, timeout);
end;

class procedure TShutdownManager.CancelShutdown;
begin
  Statics.CancelShutdown;
end;


class function TShutdownManager.IsPowerStateSupported(powerState: PowerState): Boolean;
begin
  Result := Statics2.IsPowerStateSupported(powerState);
end;

class procedure TShutdownManager.EnterPowerState(powerState: PowerState);
begin
  Statics2.EnterPowerState(powerState);
end;

class procedure TShutdownManager.EnterPowerState(powerState: PowerState; wakeUpAfter: TimeSpan);
begin
  Statics2.EnterPowerState(powerState, wakeUpAfter);
end;


{ TUser }

class function TUser.CreateWatcher: IUserWatcher;
begin
  Result := Statics.CreateWatcher;
end;

class function TUser.FindAllAsync: IAsyncOperation_1__IVectorView_1__IUser;
begin
  Result := Statics.FindAllAsync;
end;

class function TUser.FindAllAsync(&type: UserType): IAsyncOperation_1__IVectorView_1__IUser;
begin
  Result := Statics.FindAllAsync(&type);
end;

class function TUser.FindAllAsync(&type: UserType; status: UserAuthenticationStatus): IAsyncOperation_1__IVectorView_1__IUser;
begin
  Result := Statics.FindAllAsync(&type, status);
end;

class function TUser.GetFromId(nonRoamableId: HSTRING): IUser;
begin
  Result := Statics.GetFromId(nonRoamableId);
end;


{ TUserDeviceAssociation }

class function TUserDeviceAssociation.FindUserFromDeviceId(deviceId: HSTRING): IUser;
begin
  Result := Statics.FindUserFromDeviceId(deviceId);
end;

class function TUserDeviceAssociation.add_UserDeviceAssociationChanged(handler: EventHandler_1__IUserDeviceAssociationChangedEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_UserDeviceAssociationChanged(handler);
end;

class procedure TUserDeviceAssociation.remove_UserDeviceAssociationChanged(token: EventRegistrationToken);
begin
  Statics.remove_UserDeviceAssociationChanged(token);
end;


{ TUserPicker }

class function TUserPicker.IsSupported: Boolean;
begin
  Result := Statics.IsSupported;
end;


{ TUserProfile_DiagnosticsSettings }

class function TUserProfile_DiagnosticsSettings.GetDefault: UserProfile_IDiagnosticsSettings;
begin
  Result := Statics.GetDefault;
end;

class function TUserProfile_DiagnosticsSettings.GetForUser(user: IUser): UserProfile_IDiagnosticsSettings;
begin
  Result := Statics.GetForUser(user);
end;


{ TUserProfile_FirstSignInSettings }

class function TUserProfile_FirstSignInSettings.GetDefault: UserProfile_IFirstSignInSettings;
begin
  Result := Statics.GetDefault;
end;


{ TUserProfile_GlobalizationPreferences }

class function TUserProfile_GlobalizationPreferences.get_Calendars: IVectorView_1__HSTRING;
begin
  Result := Statics.get_Calendars;
end;

class function TUserProfile_GlobalizationPreferences.get_Clocks: IVectorView_1__HSTRING;
begin
  Result := Statics.get_Clocks;
end;

class function TUserProfile_GlobalizationPreferences.get_Currencies: IVectorView_1__HSTRING;
begin
  Result := Statics.get_Currencies;
end;

class function TUserProfile_GlobalizationPreferences.get_Languages: IVectorView_1__HSTRING;
begin
  Result := Statics.get_Languages;
end;

class function TUserProfile_GlobalizationPreferences.get_HomeGeographicRegion: HSTRING;
begin
  Result := Statics.get_HomeGeographicRegion;
end;

class function TUserProfile_GlobalizationPreferences.get_WeekStartsOn: DayOfWeek;
begin
  Result := Statics.get_WeekStartsOn;
end;


class function TUserProfile_GlobalizationPreferences.TrySetHomeGeographicRegion(region: HSTRING): Boolean;
begin
  Result := Statics2.TrySetHomeGeographicRegion(region);
end;

class function TUserProfile_GlobalizationPreferences.TrySetLanguages(languageTags: IIterable_1__HSTRING): Boolean;
begin
  Result := Statics2.TrySetLanguages(languageTags);
end;


class function TUserProfile_GlobalizationPreferences.GetForUser(user: IUser): UserProfile_IGlobalizationPreferencesForUser;
begin
  Result := Statics3.GetForUser(user);
end;


{ TUserProfile_LockScreen }

class function TUserProfile_LockScreen.RequestSetImageFeedAsync(syndicationFeedUri: IUriRuntimeClass): IAsyncOperation_1__UserProfile_SetImageFeedResult;
begin
  Result := Statics.RequestSetImageFeedAsync(syndicationFeedUri);
end;

class function TUserProfile_LockScreen.TryRemoveImageFeed: Boolean;
begin
  Result := Statics.TryRemoveImageFeed;
end;


class function TUserProfile_LockScreen.get_OriginalImageFile: IUriRuntimeClass;
begin
  Result := Statics2.get_OriginalImageFile;
end;

class function TUserProfile_LockScreen.GetImageStream: IRandomAccessStream;
begin
  Result := Statics2.GetImageStream;
end;

class function TUserProfile_LockScreen.SetImageFileAsync(value: IStorageFile): IAsyncAction;
begin
  Result := Statics2.SetImageFileAsync(value);
end;

class function TUserProfile_LockScreen.SetImageStreamAsync(value: IRandomAccessStream): IAsyncAction;
begin
  Result := Statics2.SetImageStreamAsync(value);
end;


{ TUserProfile_UserInformation }

class function TUserProfile_UserInformation.get_AccountPictureChangeEnabled: Boolean;
begin
  Result := Statics.get_AccountPictureChangeEnabled;
end;

class function TUserProfile_UserInformation.get_NameAccessAllowed: Boolean;
begin
  Result := Statics.get_NameAccessAllowed;
end;

class function TUserProfile_UserInformation.GetAccountPicture(kind: UserProfile_AccountPictureKind): IStorageFile;
begin
  Result := Statics.GetAccountPicture(kind);
end;

class function TUserProfile_UserInformation.SetAccountPictureAsync(image: IStorageFile): IAsyncOperation_1__UserProfile_SetAccountPictureResult;
begin
  Result := Statics.SetAccountPictureAsync(image);
end;

class function TUserProfile_UserInformation.SetAccountPicturesAsync(smallImage: IStorageFile; largeImage: IStorageFile; video: IStorageFile): IAsyncOperation_1__UserProfile_SetAccountPictureResult;
begin
  Result := Statics.SetAccountPicturesAsync(smallImage, largeImage, video);
end;

class function TUserProfile_UserInformation.SetAccountPictureFromStreamAsync(image: IRandomAccessStream): IAsyncOperation_1__UserProfile_SetAccountPictureResult;
begin
  Result := Statics.SetAccountPictureFromStreamAsync(image);
end;

class function TUserProfile_UserInformation.SetAccountPicturesFromStreamsAsync(smallImage: IRandomAccessStream; largeImage: IRandomAccessStream; video: IRandomAccessStream): IAsyncOperation_1__UserProfile_SetAccountPictureResult;
begin
  Result := Statics.SetAccountPicturesFromStreamsAsync(smallImage, largeImage, video);
end;

class function TUserProfile_UserInformation.add_AccountPictureChanged(changeHandler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_AccountPictureChanged(changeHandler);
end;

class procedure TUserProfile_UserInformation.remove_AccountPictureChanged(token: EventRegistrationToken);
begin
  Statics.remove_AccountPictureChanged(token);
end;

class function TUserProfile_UserInformation.GetDisplayNameAsync: IAsyncOperation_1__HSTRING;
begin
  Result := Statics.GetDisplayNameAsync;
end;

class function TUserProfile_UserInformation.GetFirstNameAsync: IAsyncOperation_1__HSTRING;
begin
  Result := Statics.GetFirstNameAsync;
end;

class function TUserProfile_UserInformation.GetLastNameAsync: IAsyncOperation_1__HSTRING;
begin
  Result := Statics.GetLastNameAsync;
end;

class function TUserProfile_UserInformation.GetPrincipalNameAsync: IAsyncOperation_1__HSTRING;
begin
  Result := Statics.GetPrincipalNameAsync;
end;

class function TUserProfile_UserInformation.GetSessionInitiationProtocolUriAsync: IAsyncOperation_1__IUriRuntimeClass;
begin
  Result := Statics.GetSessionInitiationProtocolUriAsync;
end;

class function TUserProfile_UserInformation.GetDomainNameAsync: IAsyncOperation_1__HSTRING;
begin
  Result := Statics.GetDomainNameAsync;
end;


{ TUserProfile_UserProfilePersonalizationSettings }

class function TUserProfile_UserProfilePersonalizationSettings.get_Current: UserProfile_IUserProfilePersonalizationSettings;
begin
  Result := Statics.get_Current;
end;

class function TUserProfile_UserProfilePersonalizationSettings.IsSupported: Boolean;
begin
  Result := Statics.IsSupported;
end;


end.
