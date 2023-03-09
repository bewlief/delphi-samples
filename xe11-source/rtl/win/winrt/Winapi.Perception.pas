{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Perception;

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
  Winapi.SystemRT, 
  Winapi.GraphicsRT, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.Perception.Spatial.ISpatialCoordinateSystem
  Spatial_ISpatialCoordinateSystem = interface;
  PSpatial_ISpatialCoordinateSystem = ^Spatial_ISpatialCoordinateSystem;

  // Windows.Perception.IPerceptionTimestamp
  IPerceptionTimestamp = interface;
  PIPerceptionTimestamp = ^IPerceptionTimestamp;

  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialBoundingFrustum>
  IReference_1__Spatial_SpatialBoundingFrustum = interface;
  PIReference_1__Spatial_SpatialBoundingFrustum = ^IReference_1__Spatial_SpatialBoundingFrustum;

  // Windows.Perception.Automation.Core.ICorePerceptionAutomationStatics
  Automation_Core_ICorePerceptionAutomationStatics = interface;
  PAutomation_Core_ICorePerceptionAutomationStatics = ^Automation_Core_ICorePerceptionAutomationStatics;

  // Windows.Perception.IPerceptionTimestampHelperStatics
  IPerceptionTimestampHelperStatics = interface;
  PIPerceptionTimestampHelperStatics = ^IPerceptionTimestampHelperStatics;

  // Windows.Perception.IPerceptionTimestampHelperStatics2
  IPerceptionTimestampHelperStatics2 = interface;
  PIPerceptionTimestampHelperStatics2 = ^IPerceptionTimestampHelperStatics2;

  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialRay>
  IReference_1__Spatial_SpatialRay = interface;
  PIReference_1__Spatial_SpatialRay = ^IReference_1__Spatial_SpatialRay;

  // Windows.Perception.People.IHeadPose
  People_IHeadPose = interface;
  PPeople_IHeadPose = ^People_IHeadPose;

  // Windows.Perception.Spatial.ISpatialAnchorRawCoordinateSystemAdjustedEventArgs
  Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs = interface;
  PSpatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs = ^Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Perception.Spatial.ISpatialAnchor,Windows.Perception.Spatial.ISpatialAnchorRawCoordinateSystemAdjustedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialAnchor__Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialAnchor__Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialAnchor__Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs;

  // Windows.Perception.Spatial.ISpatialAnchor
  Spatial_ISpatialAnchor = interface;
  PSpatial_ISpatialAnchor = ^Spatial_ISpatialAnchor;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface;
  PAsyncOperationCompletedHandler_1__Boolean = ^AsyncOperationCompletedHandler_1__Boolean;

  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface;
  PIAsyncOperation_1__Boolean = ^IAsyncOperation_1__Boolean;

  // Windows.Foundation.AsyncOperationProgressHandler`2<UInt32,UInt32>
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationProgressHandler_2__Cardinal__Cardinal = ^AsyncOperationProgressHandler_2__Cardinal__Cardinal;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<UInt32,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal;

  // Windows.Foundation.IAsyncOperationWithProgress`2<UInt32,UInt32>
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = interface;
  PIAsyncOperationWithProgress_2__Cardinal__Cardinal = ^IAsyncOperationWithProgress_2__Cardinal__Cardinal;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.SpatialPerceptionAccessStatus>
  AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus = ^AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.SpatialPerceptionAccessStatus>
  IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus = interface;
  PIAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus = ^IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>
  IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = interface;
  PIKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = ^IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IIterator_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = ^IIterator_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IIterable_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = ^IIterable_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>
  IMapView_2__HSTRING__Spatial_ISpatialAnchor = interface;
  PIMapView_2__HSTRING__Spatial_ISpatialAnchor = ^IMapView_2__HSTRING__Spatial_ISpatialAnchor;

  // Windows.Perception.Spatial.ISpatialAnchorStore
  Spatial_ISpatialAnchorStore = interface;
  PSpatial_ISpatialAnchorStore = ^Spatial_ISpatialAnchorStore;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.ISpatialAnchorStore>
  AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore = interface;
  PAsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore = ^AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore;

  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.ISpatialAnchorStore>
  IAsyncOperation_1__Spatial_ISpatialAnchorStore = interface;
  PIAsyncOperation_1__Spatial_ISpatialAnchorStore = ^IAsyncOperation_1__Spatial_ISpatialAnchorStore;

  // Windows.Perception.Spatial.ISpatialAnchorManagerStatics
  Spatial_ISpatialAnchorManagerStatics = interface;
  PSpatial_ISpatialAnchorManagerStatics = ^Spatial_ISpatialAnchorManagerStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor = interface;
  PAsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor = ^AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IAsyncOperation_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor = interface;
  PIAsyncOperation_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor = ^IAsyncOperation_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationProgressHandler_2__IBuffer__Cardinal = ^AsyncOperationProgressHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface;
  PIAsyncOperationWithProgress_2__IBuffer__Cardinal = ^IAsyncOperationWithProgress_2__IBuffer__Cardinal;

  // Windows.Perception.Spatial.ISpatialBoundingVolume
  Spatial_ISpatialBoundingVolume = interface;
  PSpatial_ISpatialBoundingVolume = ^Spatial_ISpatialBoundingVolume;

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

  // Windows.Perception.Spatial.ISpatialEntity
  Spatial_ISpatialEntity = interface;
  PSpatial_ISpatialEntity = ^Spatial_ISpatialEntity;

  // Windows.Perception.Spatial.ISpatialEntityFactory
  Spatial_ISpatialEntityFactory = interface;
  PSpatial_ISpatialEntityFactory = ^Spatial_ISpatialEntityFactory;

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface;
  PIIterator_1__HSTRING = ^IIterator_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface;
  PIIterable_1__HSTRING = ^IIterable_1__HSTRING;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface;
  PIVectorView_1__HSTRING = ^IVectorView_1__HSTRING;

  // Windows.Foundation.IReference`1<UInt32>
  IReference_1__Cardinal = interface;
  PIReference_1__Cardinal = ^IReference_1__Cardinal;

  // Windows.Foundation.IReference`1<UInt64>
  IReference_1__UInt64 = interface;
  PIReference_1__UInt64 = ^IReference_1__UInt64;

  // Windows.Foundation.IReference`1<UInt8>
  IReference_1__Byte = interface;
  PIReference_1__Byte = ^IReference_1__Byte;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Object>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable = interface;
  PTypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable = ^TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable;

  // Windows.Foundation.IReference`1<Double>
  IReference_1__Double = interface;
  PIReference_1__Double = ^IReference_1__Double;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface;
  PEventHandler_1__IInspectable = ^EventHandler_1__IInspectable;

  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialBoundingOrientedBox>
  IReference_1__Spatial_SpatialBoundingOrientedBox = interface;
  PIReference_1__Spatial_SpatialBoundingOrientedBox = ^IReference_1__Spatial_SpatialBoundingOrientedBox;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshBuffer
  Spatial_Surfaces_ISpatialSurfaceMeshBuffer = interface;
  PSpatial_Surfaces_ISpatialSurfaceMeshBuffer = ^Spatial_Surfaces_ISpatialSurfaceMeshBuffer;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh
  Spatial_Surfaces_ISpatialSurfaceMesh = interface;
  PSpatial_Surfaces_ISpatialSurfaceMesh = ^Spatial_Surfaces_ISpatialSurfaceMesh;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh>
  AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh = interface;
  PAsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh = ^AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh;

  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh>
  IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh = interface;
  PIAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh = ^IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshOptions
  Spatial_Surfaces_ISpatialSurfaceMeshOptions = interface;
  PSpatial_Surfaces_ISpatialSurfaceMeshOptions = ^Spatial_Surfaces_ISpatialSurfaceMeshOptions;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo
  Spatial_Surfaces_ISpatialSurfaceInfo = interface;
  PSpatial_Surfaces_ISpatialSurfaceInfo = ^Spatial_Surfaces_ISpatialSurfaceInfo;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshOptionsStatics
  Spatial_Surfaces_ISpatialSurfaceMeshOptionsStatics = interface;
  PSpatial_Surfaces_ISpatialSurfaceMeshOptionsStatics = ^Spatial_Surfaces_ISpatialSurfaceMeshOptionsStatics;

  // Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>
  IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface;
  PIKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = ^IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>>
  IIterator_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface;
  PIIterator_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = ^IIterator_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>>
  IIterable_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface;
  PIIterable_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = ^IIterable_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo;

  // Windows.Foundation.Collections.IMapView`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>
  IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface;
  PIMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = ^IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.Perception.Spatial.ISpatialBoundingVolume>
  IIterator_1__Spatial_ISpatialBoundingVolume = interface;
  PIIterator_1__Spatial_ISpatialBoundingVolume = ^IIterator_1__Spatial_ISpatialBoundingVolume;

  // Windows.Foundation.Collections.IIterable`1<Windows.Perception.Spatial.ISpatialBoundingVolume>
  IIterable_1__Spatial_ISpatialBoundingVolume = interface;
  PIIterable_1__Spatial_ISpatialBoundingVolume = ^IIterable_1__Spatial_ISpatialBoundingVolume;

  // Windows.Foundation.TypedEventHandler`2<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserver,Object>
  TypedEventHandler_2__Spatial_Surfaces_ISpatialSurfaceObserver__IInspectable = interface;
  PTypedEventHandler_2__Spatial_Surfaces_ISpatialSurfaceObserver__IInspectable = ^TypedEventHandler_2__Spatial_Surfaces_ISpatialSurfaceObserver__IInspectable;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserver
  Spatial_Surfaces_ISpatialSurfaceObserver = interface;
  PSpatial_Surfaces_ISpatialSurfaceObserver = ^Spatial_Surfaces_ISpatialSurfaceObserver;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserverStatics
  Spatial_Surfaces_ISpatialSurfaceObserverStatics = interface;
  PSpatial_Surfaces_ISpatialSurfaceObserverStatics = ^Spatial_Surfaces_ISpatialSurfaceObserverStatics;

  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserverStatics2
  Spatial_Surfaces_ISpatialSurfaceObserverStatics2 = interface;
  PSpatial_Surfaces_ISpatialSurfaceObserverStatics2 = ^Spatial_Surfaces_ISpatialSurfaceObserverStatics2;

  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialBoundingBox>
  IReference_1__Spatial_SpatialBoundingBox = interface;
  PIReference_1__Spatial_SpatialBoundingBox = ^IReference_1__Spatial_SpatialBoundingBox;

  // Windows.Perception Enums

  // Windows.Perception.People.HandJointKind
  People_HandJointKind = (
    Palm = 0,
    Wrist = 1,
    ThumbMetacarpal = 2,
    ThumbProximal = 3,
    ThumbDistal = 4,
    ThumbTip = 5,
    IndexMetacarpal = 6,
    IndexProximal = 7,
    IndexIntermediate = 8,
    IndexDistal = 9,
    IndexTip = 10,
    MiddleMetacarpal = 11,
    MiddleProximal = 12,
    MiddleIntermediate = 13,
    MiddleDistal = 14,
    MiddleTip = 15,
    RingMetacarpal = 16,
    RingProximal = 17,
    RingIntermediate = 18,
    RingDistal = 19,
    RingTip = 20,
    LittleMetacarpal = 21,
    LittleProximal = 22,
    LittleIntermediate = 23,
    LittleDistal = 24,
    LittleTip = 25
  );
  PPeople_HandJointKind = ^People_HandJointKind;

  // Windows.Perception.People.JointPoseAccuracy
  People_JointPoseAccuracy = (
    High = 0,
    Approximate = 1
  );
  PPeople_JointPoseAccuracy = ^People_JointPoseAccuracy;

  // Windows.Perception.Spatial.SpatialAnchorExportPurpose
  Spatial_SpatialAnchorExportPurpose = (
    Relocalization = 0,
    Sharing = 1
  );
  PSpatial_SpatialAnchorExportPurpose = ^Spatial_SpatialAnchorExportPurpose;

  // Windows.Perception.Spatial.SpatialEntityWatcherStatus
  Spatial_SpatialEntityWatcherStatus = (
    Created = 0,
    Started = 1,
    EnumerationCompleted = 2,
    Stopping = 3,
    Stopped = 4,
    Aborted = 5
  );
  PSpatial_SpatialEntityWatcherStatus = ^Spatial_SpatialEntityWatcherStatus;

  // Windows.Perception.Spatial.SpatialLocatability
  Spatial_SpatialLocatability = (
    Unavailable = 0,
    OrientationOnly = 1,
    PositionalTrackingActivating = 2,
    PositionalTrackingActive = 3,
    PositionalTrackingInhibited = 4
  );
  PSpatial_SpatialLocatability = ^Spatial_SpatialLocatability;

  // Windows.Perception.Spatial.SpatialLookDirectionRange
  Spatial_SpatialLookDirectionRange = (
    ForwardOnly = 0,
    Omnidirectional = 1
  );
  PSpatial_SpatialLookDirectionRange = ^Spatial_SpatialLookDirectionRange;

  // Windows.Perception.Spatial.SpatialMovementRange
  Spatial_SpatialMovementRange = (
    NoMovement = 0,
    Bounded = 1
  );
  PSpatial_SpatialMovementRange = ^Spatial_SpatialMovementRange;

  // Windows.Perception.Spatial.SpatialPerceptionAccessStatus
  Spatial_SpatialPerceptionAccessStatus = (
    Unspecified = 0,
    Allowed = 1,
    DeniedByUser = 2,
    DeniedBySystem = 3
  );
  PSpatial_SpatialPerceptionAccessStatus = ^Spatial_SpatialPerceptionAccessStatus;

  // Windows.Perception Records
  // Windows.Perception.Automation.Core.PerceptionAutomationCoreContract
  Automation_Core_PerceptionAutomationCoreContract = record
  end;
  PAutomation_Core_PerceptionAutomationCoreContract = ^Automation_Core_PerceptionAutomationCoreContract;

  // Windows.Perception.People.HandMeshVertex
  People_HandMeshVertex = record
    Position: Numerics_Vector3;
    Normal: Numerics_Vector3;
  end;
  PPeople_HandMeshVertex = ^People_HandMeshVertex;

  // Windows.Perception.People.JointPose
  People_JointPose = record
    Orientation: Numerics_Quaternion;
    Position: Numerics_Vector3;
    Radius: Single;
    Accuracy: People_JointPoseAccuracy;
  end;
  PPeople_JointPose = ^People_JointPose;

  // Windows.Perception.Spatial.SpatialBoundingBox
  Spatial_SpatialBoundingBox = record
    Center: Numerics_Vector3;
    Extents: Numerics_Vector3;
  end;
  PSpatial_SpatialBoundingBox = ^Spatial_SpatialBoundingBox;

  // Windows.Perception.Spatial.SpatialBoundingFrustum
  Spatial_SpatialBoundingFrustum = record
    &Near: Numerics_Plane;
    &Far: Numerics_Plane;
    Right: Numerics_Plane;
    Left: Numerics_Plane;
    Top: Numerics_Plane;
    Bottom: Numerics_Plane;
  end;
  PSpatial_SpatialBoundingFrustum = ^Spatial_SpatialBoundingFrustum;

  // Windows.Perception.Spatial.SpatialBoundingOrientedBox
  Spatial_SpatialBoundingOrientedBox = record
    Center: Numerics_Vector3;
    Extents: Numerics_Vector3;
    Orientation: Numerics_Quaternion;
  end;
  PSpatial_SpatialBoundingOrientedBox = ^Spatial_SpatialBoundingOrientedBox;

  // Windows.Perception.Spatial.SpatialBoundingSphere
  Spatial_SpatialBoundingSphere = record
    Center: Numerics_Vector3;
    Radius: Single;
  end;
  PSpatial_SpatialBoundingSphere = ^Spatial_SpatialBoundingSphere;

  // Windows.Perception.Spatial.SpatialRay
  Spatial_SpatialRay = record
    Origin: Numerics_Vector3;
    Direction: Numerics_Vector3;
  end;
  PSpatial_SpatialRay = ^Spatial_SpatialRay;

  // Windows.Perception Interfaces

  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialCoordinateSystem
  Spatial_ISpatialCoordinateSystem = interface(IInspectable)
  ['{69EBCA4B-60A3-3586-A653-59A7BD676D07}']
    function TryGetTransformTo(target: Spatial_ISpatialCoordinateSystem): IReference_1__Numerics_Matrix4x4; safecall;
  end;

  // UsedAPI Interface
  // Windows.Perception.IPerceptionTimestamp
  IPerceptionTimestamp = interface(IInspectable)
  ['{87C24804-A22E-4ADB-BA26-D78EF639BCF4}']
    function get_TargetTime: DateTime; safecall;
    function get_PredictionAmount: TimeSpan; safecall;
    property PredictionAmount: TimeSpan read get_PredictionAmount;
    property TargetTime: DateTime read get_TargetTime;
  end;

  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialBoundingFrustum>
  IReference_1__Spatial_SpatialBoundingFrustum = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Spatial_SpatialBoundingFrustum; safecall;
    property Value: Spatial_SpatialBoundingFrustum read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Automation.Core.ICorePerceptionAutomationStatics
  [WinRTClassNameAttribute(SWindows_Perception_Automation_Core_CorePerceptionAutomation)]
  Automation_Core_ICorePerceptionAutomationStatics = interface(IInspectable)
  ['{0BB04541-4CE2-4923-9A76-8187ECC59112}']
    procedure SetActivationFactoryProvider(provider: IGetActivationFactory); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.IPerceptionTimestampHelperStatics
  [WinRTClassNameAttribute(SWindows_Perception_PerceptionTimestampHelper)]
  IPerceptionTimestampHelperStatics = interface(IInspectable)
  ['{47A611D4-A9DF-4EDC-855D-F4D339D967AC}']
    function FromHistoricalTargetTime(targetTime: DateTime): IPerceptionTimestamp; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.IPerceptionTimestampHelperStatics2
  [WinRTClassNameAttribute(SWindows_Perception_PerceptionTimestampHelper)]
  IPerceptionTimestampHelperStatics2 = interface(IInspectable)
  ['{73D1A7FE-3FB9-4571-87D4-3C920A5E86EB}']
    function FromSystemRelativeTargetTime(targetTime: TimeSpan): IPerceptionTimestamp; safecall;
  end;

  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialRay>
  IReference_1__Spatial_SpatialRay = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Spatial_SpatialRay; safecall;
    property Value: Spatial_SpatialRay read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Perception.People.IHeadPose
  People_IHeadPose = interface(IInspectable)
  ['{7F5AC5A5-49DB-379F-9429-32A2FAF34FA6}']
    function get_Position: Numerics_Vector3; safecall;
    function get_ForwardDirection: Numerics_Vector3; safecall;
    function get_UpDirection: Numerics_Vector3; safecall;
    property ForwardDirection: Numerics_Vector3 read get_ForwardDirection;
    property Position: Numerics_Vector3 read get_Position;
    property UpDirection: Numerics_Vector3 read get_UpDirection;
  end;

  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialAnchorRawCoordinateSystemAdjustedEventArgs
  Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs = interface(IInspectable)
  ['{A1E81EB8-56C7-3117-A2E4-81E0FCF28E00}']
    function get_OldRawCoordinateSystemToNewRawCoordinateSystemTransform: Numerics_Matrix4x4; safecall;
    property OldRawCoordinateSystemToNewRawCoordinateSystemTransform: Numerics_Matrix4x4 read get_OldRawCoordinateSystemToNewRawCoordinateSystemTransform;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Perception.Spatial.ISpatialAnchor,Windows.Perception.Spatial.ISpatialAnchorRawCoordinateSystemAdjustedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialAnchor__Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs_Delegate_Base = interface(IUnknown)
  ['{FA43F9E4-3558-59C8-9A77-6E8B765ADCC8}']
    procedure Invoke(sender: Spatial_ISpatialAnchor; args: Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Perception.Spatial.ISpatialAnchor,Windows.Perception.Spatial.ISpatialAnchorRawCoordinateSystemAdjustedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialAnchor__Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialAnchor__Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs_Delegate_Base)
  ['{F726A86C-0511-5864-AEFF-7618E64F384F}']
  end;

  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialAnchor
  Spatial_ISpatialAnchor = interface(IInspectable)
  ['{0529E5CE-1D34-3702-BCEC-EABFF578A869}']
    function get_CoordinateSystem: Spatial_ISpatialCoordinateSystem; safecall;
    function get_RawCoordinateSystem: Spatial_ISpatialCoordinateSystem; safecall;
    function add_RawCoordinateSystemAdjusted(handler: TypedEventHandler_2__Spatial_ISpatialAnchor__Spatial_ISpatialAnchorRawCoordinateSystemAdjustedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RawCoordinateSystemAdjusted(cookie: EventRegistrationToken); safecall;
    property CoordinateSystem: Spatial_ISpatialCoordinateSystem read get_CoordinateSystem;
    property RawCoordinateSystem: Spatial_ISpatialCoordinateSystem read get_RawCoordinateSystem;
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

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.SpatialPerceptionAccessStatus>
  AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus_Delegate_Base = interface(IUnknown)
  ['{6CED54C8-7689-525A-80E1-956A9D85CD83}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.SpatialPerceptionAccessStatus>
  AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus = interface(AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.SpatialPerceptionAccessStatus>
  IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus_Base = interface(IInspectable)
  ['{B425D126-1069-563F-A863-44A30A8F071D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus; safecall;
    function GetResults: Spatial_SpatialPerceptionAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Spatial_SpatialPerceptionAccessStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.SpatialPerceptionAccessStatus>
  IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus = interface(IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>
  IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = interface(IInspectable)
  ['{547791A5-EF39-5BD8-A5C3-33DE743A759A}']
    function get_Key: HSTRING; safecall;
    function get_Value: Spatial_ISpatialAnchor; safecall;
    property Key: HSTRING read get_Key;
    property Value: Spatial_ISpatialAnchor read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IIterator_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor_Base = interface(IInspectable)
  ['{67A5F318-0232-5900-AC7E-5C647D731CBC}']
    function get_Current: IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IIterator_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = interface(IIterator_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor_Base)
  ['{3267E04B-5FD8-543C-823C-2A90679C84C7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IIterable_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor_Base = interface(IInspectable)
  ['{55F0FA8A-AFD4-5541-A1C3-36F12147D606}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IIterable_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor = interface(IIterable_1__IKeyValuePair_2__HSTRING__Spatial_ISpatialAnchor_Base)
  ['{090FBEAE-A85B-539F-8BE0-707D8F633846}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>
  IMapView_2__HSTRING__Spatial_ISpatialAnchor_Base = interface(IInspectable)
  ['{2D344564-21B1-5470-B013-488CDDE45C48}']
    function Lookup(key: HSTRING): Spatial_ISpatialAnchor; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__Spatial_ISpatialAnchor; out second: IMapView_2__HSTRING__Spatial_ISpatialAnchor); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>
  IMapView_2__HSTRING__Spatial_ISpatialAnchor = interface(IMapView_2__HSTRING__Spatial_ISpatialAnchor_Base)
  ['{B2E424BB-8EF5-501D-A6DA-89072839A737}']
  end;

  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialAnchorStore
  Spatial_ISpatialAnchorStore = interface(IInspectable)
  ['{B0BC3636-486A-3CB0-9E6F-1245165C4DB6}']
    function GetAllSavedAnchors: IMapView_2__HSTRING__Spatial_ISpatialAnchor; safecall;
    function TrySave(id: HSTRING; anchor: Spatial_ISpatialAnchor): Boolean; safecall;
    procedure Remove(id: HSTRING); safecall;
    procedure Clear; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.ISpatialAnchorStore>
  AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore_Delegate_Base = interface(IUnknown)
  ['{84C21A3A-037A-503F-8006-AB577B7F6F66}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Spatial_ISpatialAnchorStore; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.ISpatialAnchorStore>
  AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore = interface(AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore_Delegate_Base)
  ['{64E39606-3372-5057-9C76-0DD0E4A35ACD}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.ISpatialAnchorStore>
  IAsyncOperation_1__Spatial_ISpatialAnchorStore_Base = interface(IInspectable)
  ['{1CD05E51-1457-5023-8F5D-FE5E5A953423}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore; safecall;
    function GetResults: Spatial_ISpatialAnchorStore; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Spatial_ISpatialAnchorStore read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.ISpatialAnchorStore>
  IAsyncOperation_1__Spatial_ISpatialAnchorStore = interface(IAsyncOperation_1__Spatial_ISpatialAnchorStore_Base)
  ['{53114995-FDB5-506B-9AA2-87715FFF4A5F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialAnchorManagerStatics
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_SpatialAnchorManager)]
  Spatial_ISpatialAnchorManagerStatics = interface(IInspectable)
  ['{88E30EAB-F3B7-420B-B086-8A80C07D910D}']
    function RequestStoreAsync: IAsyncOperation_1__Spatial_ISpatialAnchorStore; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor_Delegate_Base = interface(IUnknown)
  ['{3A950AA3-9C65-586E-AF75-1ACF07190E90}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor = interface(AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor_Delegate_Base)
  ['{270C36C6-8540-533B-97AE-9A5B17743C87}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IAsyncOperation_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor_Base = interface(IInspectable)
  ['{BBE07728-DA33-52C5-AAE0-A5E74CDF0471}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor; safecall;
    function GetResults: IMapView_2__HSTRING__Spatial_ISpatialAnchor; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IMapView`2<String,Windows.Perception.Spatial.ISpatialAnchor>>
  IAsyncOperation_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor = interface(IAsyncOperation_1__IMapView_2__HSTRING__Spatial_ISpatialAnchor_Base)
  ['{A3D7FBC2-7A16-52FD-B494-E92D234C464C}']
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

  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialBoundingVolume
  Spatial_ISpatialBoundingVolume = interface(IInspectable)
  ['{FB2065DA-68C3-33DF-B7AF-4C787207999C}']
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialEntity
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_SpatialEntity)]
  Spatial_ISpatialEntity = interface(IInspectable)
  ['{166DE955-E1EB-454C-BA08-E6C0668DDC65}']
    function get_Id: HSTRING; safecall;
    function get_Anchor: Spatial_ISpatialAnchor; safecall;
    function get_Properties: IPropertySet; safecall;
    property Anchor: Spatial_ISpatialAnchor read get_Anchor;
    property Id: HSTRING read get_Id;
    property Properties: IPropertySet read get_Properties;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.ISpatialEntityFactory
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_SpatialEntity)]
  Spatial_ISpatialEntityFactory = interface(IInspectable)
  ['{E1F1E325-349F-4225-A2F3-4B01C15FE056}']
    function CreateWithSpatialAnchor(spatialAnchor: Spatial_ISpatialAnchor): Spatial_ISpatialEntity; safecall;
    function CreateWithSpatialAnchorAndProperties(spatialAnchor: Spatial_ISpatialAnchor; propertySet: IPropertySet): Spatial_ISpatialEntity; safecall;
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

  // Windows.Foundation.TypedEventHandler`2<Windows.System.RemoteSystems.IRemoteSystemSessionParticipantWatcher,Object>
  TypedEventHandler_2__RemoteSystems_IRemoteSystemSessionParticipantWatcher__IInspectable = interface(IUnknown)
  ['{EE7948C0-7384-5E23-A792-9EDC68234AED}']
    procedure Invoke(sender: RemoteSystems_IRemoteSystemSessionParticipantWatcher; args: IInspectable); safecall;
  end;

  // Windows.Foundation.IReference`1<Double>
  IReference_1__Double = interface(IInspectable)
  ['{2F2D6C29-5473-5F3E-92E7-96572BB990E2}']
    function get_Value: Double; safecall;
    property Value: Double read get_Value;
  end;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface(IUnknown)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
    procedure Invoke(sender: IInspectable; args: IInspectable); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialBoundingOrientedBox>
  IReference_1__Spatial_SpatialBoundingOrientedBox = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Spatial_SpatialBoundingOrientedBox; safecall;
    property Value: Spatial_SpatialBoundingOrientedBox read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshBuffer
  Spatial_Surfaces_ISpatialSurfaceMeshBuffer = interface(IInspectable)
  ['{93CF59E0-871F-33F8-98B2-03D101458F6F}']
    function get_Format: DirectX_DirectXPixelFormat; safecall;
    function get_Stride: Cardinal; safecall;
    function get_ElementCount: Cardinal; safecall;
    function get_Data: IBuffer; safecall;
    property Data: IBuffer read get_Data;
    property ElementCount: Cardinal read get_ElementCount;
    property Format: DirectX_DirectXPixelFormat read get_Format;
    property Stride: Cardinal read get_Stride;
  end;

  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh
  Spatial_Surfaces_ISpatialSurfaceMesh = interface(IInspectable)
  ['{108F57D9-DF0D-3950-A0FD-F972C77C27B4}']
    function get_SurfaceInfo: Spatial_Surfaces_ISpatialSurfaceInfo; safecall;
    function get_CoordinateSystem: Spatial_ISpatialCoordinateSystem; safecall;
    function get_TriangleIndices: Spatial_Surfaces_ISpatialSurfaceMeshBuffer; safecall;
    function get_VertexPositions: Spatial_Surfaces_ISpatialSurfaceMeshBuffer; safecall;
    function get_VertexPositionScale: Numerics_Vector3; safecall;
    function get_VertexNormals: Spatial_Surfaces_ISpatialSurfaceMeshBuffer; safecall;
    property CoordinateSystem: Spatial_ISpatialCoordinateSystem read get_CoordinateSystem;
    property SurfaceInfo: Spatial_Surfaces_ISpatialSurfaceInfo read get_SurfaceInfo;
    property TriangleIndices: Spatial_Surfaces_ISpatialSurfaceMeshBuffer read get_TriangleIndices;
    property VertexNormals: Spatial_Surfaces_ISpatialSurfaceMeshBuffer read get_VertexNormals;
    property VertexPositionScale: Numerics_Vector3 read get_VertexPositionScale;
    property VertexPositions: Spatial_Surfaces_ISpatialSurfaceMeshBuffer read get_VertexPositions;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh>
  AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh_Delegate_Base = interface(IUnknown)
  ['{4680F7F6-44C5-5FC6-8D51-D6962915FA23}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh>
  AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh = interface(AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh_Delegate_Base)
  ['{8C04E539-C05E-5385-853F-03ED17E1A697}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh>
  IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh_Base = interface(IInspectable)
  ['{F5938FAD-A8A1-5F7E-9440-BDB781AD26B6}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh; safecall;
    function GetResults: Spatial_Surfaces_ISpatialSurfaceMesh; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Spatial_Surfaces_ISpatialSurfaceMesh read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMesh>
  IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh = interface(IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh_Base)
  ['{DBF56815-4BF9-591C-9E9B-FF5E22C53C47}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshOptions
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_Surfaces_SpatialSurfaceMeshOptions)]
  Spatial_Surfaces_ISpatialSurfaceMeshOptions = interface(IInspectable)
  ['{D2759F89-3572-3D2D-A10D-5FEE9394AA37}']
    function get_VertexPositionFormat: DirectX_DirectXPixelFormat; safecall;
    procedure put_VertexPositionFormat(value: DirectX_DirectXPixelFormat); safecall;
    function get_TriangleIndexFormat: DirectX_DirectXPixelFormat; safecall;
    procedure put_TriangleIndexFormat(value: DirectX_DirectXPixelFormat); safecall;
    function get_VertexNormalFormat: DirectX_DirectXPixelFormat; safecall;
    procedure put_VertexNormalFormat(value: DirectX_DirectXPixelFormat); safecall;
    function get_IncludeVertexNormals: Boolean; safecall;
    procedure put_IncludeVertexNormals(value: Boolean); safecall;
    property IncludeVertexNormals: Boolean read get_IncludeVertexNormals write put_IncludeVertexNormals;
    property TriangleIndexFormat: DirectX_DirectXPixelFormat read get_TriangleIndexFormat write put_TriangleIndexFormat;
    property VertexNormalFormat: DirectX_DirectXPixelFormat read get_VertexNormalFormat write put_VertexNormalFormat;
    property VertexPositionFormat: DirectX_DirectXPixelFormat read get_VertexPositionFormat write put_VertexPositionFormat;
  end;

  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo
  Spatial_Surfaces_ISpatialSurfaceInfo = interface(IInspectable)
  ['{F8E9EBE7-39B7-3962-BB03-57F56E1FB0A1}']
    function get_Id: TGuid; safecall;
    function get_UpdateTime: DateTime; safecall;
    function TryGetBounds(coordinateSystem: Spatial_ISpatialCoordinateSystem): IReference_1__Spatial_SpatialBoundingOrientedBox; safecall;
    function TryComputeLatestMeshAsync(maxTrianglesPerCubicMeter: Double): IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh; overload; safecall;
    function TryComputeLatestMeshAsync(maxTrianglesPerCubicMeter: Double; options: Spatial_Surfaces_ISpatialSurfaceMeshOptions): IAsyncOperation_1__Spatial_Surfaces_ISpatialSurfaceMesh; overload; safecall;
    property Id: TGuid read get_Id;
    property UpdateTime: DateTime read get_UpdateTime;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshOptionsStatics
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_Surfaces_SpatialSurfaceMeshOptions)]
  Spatial_Surfaces_ISpatialSurfaceMeshOptionsStatics = interface(IInspectable)
  ['{9B340ABF-9781-4505-8935-013575CAAE5E}']
    function get_SupportedVertexPositionFormats: IVectorView_1__DirectX_DirectXPixelFormat; safecall;
    function get_SupportedTriangleIndexFormats: IVectorView_1__DirectX_DirectXPixelFormat; safecall;
    function get_SupportedVertexNormalFormats: IVectorView_1__DirectX_DirectXPixelFormat; safecall;
    property SupportedTriangleIndexFormats: IVectorView_1__DirectX_DirectXPixelFormat read get_SupportedTriangleIndexFormats;
    property SupportedVertexNormalFormats: IVectorView_1__DirectX_DirectXPixelFormat read get_SupportedVertexNormalFormats;
    property SupportedVertexPositionFormats: IVectorView_1__DirectX_DirectXPixelFormat read get_SupportedVertexPositionFormats;
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>
  IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface(IInspectable)
  ['{6C8FA36F-82E6-5DDC-8F0E-7646469A4C4B}']
    function get_Key: TGuid; safecall;
    function get_Value: Spatial_Surfaces_ISpatialSurfaceInfo; safecall;
    property Key: TGuid read get_Key;
    property Value: Spatial_Surfaces_ISpatialSurfaceInfo read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>>
  IIterator_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo_Base = interface(IInspectable)
  ['{6D328390-F279-5F39-9682-BBA0CD81489B}']
    function get_Current: IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo): Cardinal; safecall;
    property Current: IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>>
  IIterator_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface(IIterator_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo_Base)
  ['{989514CE-C364-50AE-A457-661A09907428}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>>
  IIterable_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo_Base = interface(IInspectable)
  ['{868757D1-BE21-51D9-8DEE-A958B9DEEC71}']
    function First: IIterator_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>>
  IIterable_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface(IIterable_1__IKeyValuePair_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo_Base)
  ['{5A0E5C58-56C0-504D-823D-6B0D821E837B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>
  IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo_Base = interface(IInspectable)
  ['{EAA722B9-2859-593D-BB66-0C538E415E71}']
    function Lookup(key: TGuid): Spatial_Surfaces_ISpatialSurfaceInfo; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: TGuid): Boolean; safecall;
    procedure Split(out first: IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo; out second: IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IMapView`2<Guid,Windows.Perception.Spatial.Surfaces.ISpatialSurfaceInfo>
  IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo = interface(IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo_Base)
  ['{47C35879-AE7A-5414-B3F9-109D665128ED}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Perception.Spatial.ISpatialBoundingVolume>
  IIterator_1__Spatial_ISpatialBoundingVolume_Base = interface(IInspectable)
  ['{EB8385C5-0775-5415-8F76-327E6E388AC5}']
    function get_Current: Spatial_ISpatialBoundingVolume; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSpatial_ISpatialBoundingVolume): Cardinal; safecall;
    property Current: Spatial_ISpatialBoundingVolume read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Perception.Spatial.ISpatialBoundingVolume>
  IIterator_1__Spatial_ISpatialBoundingVolume = interface(IIterator_1__Spatial_ISpatialBoundingVolume_Base)
  ['{94ACF9EA-9717-5C6B-BBC9-4B80AB3E9A80}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Perception.Spatial.ISpatialBoundingVolume>
  IIterable_1__Spatial_ISpatialBoundingVolume_Base = interface(IInspectable)
  ['{89E8F1EE-3A2A-5B69-A786-CDDCF7456A3A}']
    function First: IIterator_1__Spatial_ISpatialBoundingVolume; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Perception.Spatial.ISpatialBoundingVolume>
  IIterable_1__Spatial_ISpatialBoundingVolume = interface(IIterable_1__Spatial_ISpatialBoundingVolume_Base)
  ['{3D91C0BB-1CD1-5A97-AE21-C5092529D6D5}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserver,Object>
  TypedEventHandler_2__Spatial_Surfaces_ISpatialSurfaceObserver__IInspectable_Delegate_Base = interface(IUnknown)
  ['{8B31274A-7693-52BE-9014-B0F5F65A3539}']
    procedure Invoke(sender: Spatial_Surfaces_ISpatialSurfaceObserver; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserver,Object>
  TypedEventHandler_2__Spatial_Surfaces_ISpatialSurfaceObserver__IInspectable = interface(TypedEventHandler_2__Spatial_Surfaces_ISpatialSurfaceObserver__IInspectable_Delegate_Base)
  ['{9EDA23A8-397E-5657-94D3-9769A212F376}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserver
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_Surfaces_SpatialSurfaceObserver)]
  Spatial_Surfaces_ISpatialSurfaceObserver = interface(IInspectable)
  ['{10B69819-DDCA-3483-AC3A-748FE8C86DF5}']
    function GetObservedSurfaces: IMapView_2__TGuid__Spatial_Surfaces_ISpatialSurfaceInfo; safecall;
    procedure SetBoundingVolume(bounds: Spatial_ISpatialBoundingVolume); safecall;
    procedure SetBoundingVolumes(bounds: IIterable_1__Spatial_ISpatialBoundingVolume); safecall;
    function add_ObservedSurfacesChanged(handler: TypedEventHandler_2__Spatial_Surfaces_ISpatialSurfaceObserver__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ObservedSurfacesChanged(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserverStatics
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_Surfaces_SpatialSurfaceObserver)]
  Spatial_Surfaces_ISpatialSurfaceObserverStatics = interface(IInspectable)
  ['{165951ED-2108-4168-9175-87E027BC9285}']
    function RequestAccessAsync: IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserverStatics2
  [WinRTClassNameAttribute(SWindows_Perception_Spatial_Surfaces_SpatialSurfaceObserver)]
  Spatial_Surfaces_ISpatialSurfaceObserverStatics2 = interface(IInspectable)
  ['{0F534261-C55D-4E6B-A895-A19DE69A42E3}']
    function IsSupported: Boolean; safecall;
  end;

  // Windows.Foundation.IReference`1<Windows.Perception.Spatial.SpatialBoundingBox>
  IReference_1__Spatial_SpatialBoundingBox = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Spatial_SpatialBoundingBox; safecall;
    property Value: Spatial_SpatialBoundingBox read get_Value;
  end;

  // Windows.Perception.Automation.Core.CorePerceptionAutomation
  // DualAPI
  // Statics: "Windows.Perception.Automation.Core.ICorePerceptionAutomationStatics"
  TAutomation_Core_CorePerceptionAutomation = class(TWinRTGenericImportS<Automation_Core_ICorePerceptionAutomationStatics>)
  public
    // -> Automation_Core_ICorePerceptionAutomationStatics
    class procedure SetActivationFactoryProvider(provider: IGetActivationFactory); static; inline;
  end;

  // Windows.Perception.PerceptionTimestampHelper
  // DualAPI
  // Statics: "Windows.Perception.IPerceptionTimestampHelperStatics"
  // Statics: "Windows.Perception.IPerceptionTimestampHelperStatics2"
  TPerceptionTimestampHelper = class(TWinRTGenericImportS2<IPerceptionTimestampHelperStatics, IPerceptionTimestampHelperStatics2>)
  public
    // -> IPerceptionTimestampHelperStatics
    class function FromHistoricalTargetTime(targetTime: DateTime): IPerceptionTimestamp; static; inline;

    // -> IPerceptionTimestampHelperStatics2
    class function FromSystemRelativeTargetTime(targetTime: TimeSpan): IPerceptionTimestamp; static; inline;
  end;

  // Windows.Perception.Spatial.SpatialAnchorManager
  // DualAPI
  // Statics: "Windows.Perception.Spatial.ISpatialAnchorManagerStatics"
  TSpatial_SpatialAnchorManager = class(TWinRTGenericImportS<Spatial_ISpatialAnchorManagerStatics>)
  public
    // -> Spatial_ISpatialAnchorManagerStatics
    class function RequestStoreAsync: IAsyncOperation_1__Spatial_ISpatialAnchorStore; static; inline;
  end;

  // Windows.Perception.Spatial.SpatialEntity
  // DualAPI
  // Implements: Windows.Perception.Spatial.ISpatialEntity
  // Factory: "Windows.Perception.Spatial.ISpatialEntityFactory"
  TSpatial_SpatialEntity = class(TWinRTGenericImportF<Spatial_ISpatialEntityFactory>)
  public
    // -> Spatial_ISpatialEntityFactory
    class function CreateWithSpatialAnchor(spatialAnchor: Spatial_ISpatialAnchor): Spatial_ISpatialEntity; static; inline;
    class function CreateWithSpatialAnchorAndProperties(spatialAnchor: Spatial_ISpatialAnchor; propertySet: IPropertySet): Spatial_ISpatialEntity; static; inline;
  end;

  // Windows.Perception.Spatial.Surfaces.SpatialSurfaceMeshOptions
  // DualAPI
  // Implements: Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshOptions
  // Statics: "Windows.Perception.Spatial.Surfaces.ISpatialSurfaceMeshOptionsStatics"
  // Instantiable: "Spatial_Surfaces_ISpatialSurfaceMeshOptions"
  TSpatial_Surfaces_SpatialSurfaceMeshOptions = class(TWinRTGenericImportSI<Spatial_Surfaces_ISpatialSurfaceMeshOptionsStatics, Spatial_Surfaces_ISpatialSurfaceMeshOptions>)
  public
    // -> Spatial_Surfaces_ISpatialSurfaceMeshOptionsStatics
    class function get_SupportedVertexPositionFormats: IVectorView_1__DirectX_DirectXPixelFormat; static; inline;
    class function get_SupportedTriangleIndexFormats: IVectorView_1__DirectX_DirectXPixelFormat; static; inline;
    class function get_SupportedVertexNormalFormats: IVectorView_1__DirectX_DirectXPixelFormat; static; inline;
    class property SupportedTriangleIndexFormats: IVectorView_1__DirectX_DirectXPixelFormat read get_SupportedTriangleIndexFormats;
    class property SupportedVertexNormalFormats: IVectorView_1__DirectX_DirectXPixelFormat read get_SupportedVertexNormalFormats;
    class property SupportedVertexPositionFormats: IVectorView_1__DirectX_DirectXPixelFormat read get_SupportedVertexPositionFormats;
  end;

  // Windows.Perception.Spatial.Surfaces.SpatialSurfaceObserver
  // DualAPI
  // Implements: Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserver
  // Statics: "Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserverStatics"
  // Statics: "Windows.Perception.Spatial.Surfaces.ISpatialSurfaceObserverStatics2"
  // Instantiable: "Spatial_Surfaces_ISpatialSurfaceObserver"
  TSpatial_Surfaces_SpatialSurfaceObserver = class(TWinRTGenericImportS2I<Spatial_Surfaces_ISpatialSurfaceObserverStatics, Spatial_Surfaces_ISpatialSurfaceObserverStatics2, Spatial_Surfaces_ISpatialSurfaceObserver>)
  public
    // -> Spatial_Surfaces_ISpatialSurfaceObserverStatics
    class function RequestAccessAsync: IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus; static; inline;

    // -> Spatial_Surfaces_ISpatialSurfaceObserverStatics2
    class function IsSupported: Boolean; static; inline;
  end;

implementation

{ TAutomation_Core_CorePerceptionAutomation }

class procedure TAutomation_Core_CorePerceptionAutomation.SetActivationFactoryProvider(provider: IGetActivationFactory);
begin
  Statics.SetActivationFactoryProvider(provider);
end;


{ TPerceptionTimestampHelper }

class function TPerceptionTimestampHelper.FromHistoricalTargetTime(targetTime: DateTime): IPerceptionTimestamp;
begin
  Result := Statics.FromHistoricalTargetTime(targetTime);
end;


class function TPerceptionTimestampHelper.FromSystemRelativeTargetTime(targetTime: TimeSpan): IPerceptionTimestamp;
begin
  Result := Statics2.FromSystemRelativeTargetTime(targetTime);
end;


{ TSpatial_SpatialAnchorManager }

class function TSpatial_SpatialAnchorManager.RequestStoreAsync: IAsyncOperation_1__Spatial_ISpatialAnchorStore;
begin
  Result := Statics.RequestStoreAsync;
end;


{ TSpatial_SpatialEntity }
// Factories for : "Spatial_SpatialEntity"
// Factory: "Windows.Perception.Spatial.ISpatialEntityFactory"
// -> Spatial_ISpatialEntityFactory

class function TSpatial_SpatialEntity.CreateWithSpatialAnchor(spatialAnchor: Spatial_ISpatialAnchor): Spatial_ISpatialEntity;
begin
  Result := Factory.CreateWithSpatialAnchor(spatialAnchor);
end;

class function TSpatial_SpatialEntity.CreateWithSpatialAnchorAndProperties(spatialAnchor: Spatial_ISpatialAnchor; propertySet: IPropertySet): Spatial_ISpatialEntity;
begin
  Result := Factory.CreateWithSpatialAnchorAndProperties(spatialAnchor, propertySet);
end;


{ TSpatial_Surfaces_SpatialSurfaceMeshOptions }

class function TSpatial_Surfaces_SpatialSurfaceMeshOptions.get_SupportedVertexPositionFormats: IVectorView_1__DirectX_DirectXPixelFormat;
begin
  Result := Statics.get_SupportedVertexPositionFormats;
end;

class function TSpatial_Surfaces_SpatialSurfaceMeshOptions.get_SupportedTriangleIndexFormats: IVectorView_1__DirectX_DirectXPixelFormat;
begin
  Result := Statics.get_SupportedTriangleIndexFormats;
end;

class function TSpatial_Surfaces_SpatialSurfaceMeshOptions.get_SupportedVertexNormalFormats: IVectorView_1__DirectX_DirectXPixelFormat;
begin
  Result := Statics.get_SupportedVertexNormalFormats;
end;


{ TSpatial_Surfaces_SpatialSurfaceObserver }

class function TSpatial_Surfaces_SpatialSurfaceObserver.RequestAccessAsync: IAsyncOperation_1__Spatial_SpatialPerceptionAccessStatus;
begin
  Result := Statics.RequestAccessAsync;
end;


class function TSpatial_Surfaces_SpatialSurfaceObserver.IsSupported: Boolean;
begin
  Result := Statics2.IsSupported;
end;


end.
