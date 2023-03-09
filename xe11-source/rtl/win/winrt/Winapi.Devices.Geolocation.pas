{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Devices.Geolocation;

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

  BasicGeoposition = Winapi.CommonTypes.BasicGeoposition;
  PBasicGeoposition = Winapi.CommonTypes.PBasicGeoposition;
  IGeoboundingBox = Winapi.CommonTypes.IGeoboundingBox;
  PIGeoboundingBox = Winapi.CommonTypes.PIGeoboundingBox;
  IGeopath = Winapi.CommonTypes.IGeopath;
  PIGeopath = Winapi.CommonTypes.PIGeopath;
  IGeopoint = Winapi.CommonTypes.IGeopoint;
  PIGeopoint = Winapi.CommonTypes.PIGeopoint;
  IVectorView_1__BasicGeoposition = Winapi.CommonTypes.IVectorView_1__BasicGeoposition;
  PIVectorView_1__BasicGeoposition = Winapi.CommonTypes.PIVectorView_1__BasicGeoposition;

  // Forward declarations for interfaces

  // Windows.Devices.Geolocation.IGeoshape
  IGeoshape = interface;
  PIGeoshape = ^IGeoshape;

  // Windows.Devices.Geolocation.IGeocoordinate
  IGeocoordinate = interface;
  PIGeocoordinate = ^IGeocoordinate;

  // Windows.Devices.Geolocation.ICivicAddress
  ICivicAddress = interface;
  PICivicAddress = ^ICivicAddress;

  // Windows.Devices.Geolocation.IGeoposition
  IGeoposition = interface;
  PIGeoposition = ^IGeoposition;

  // Windows.Devices.Geolocation.IGeoboundingBoxFactory
  IGeoboundingBoxFactory = interface;
  PIGeoboundingBoxFactory = ^IGeoboundingBoxFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.BasicGeoposition>
  IIterator_1__BasicGeoposition = interface;
  PIIterator_1__BasicGeoposition = ^IIterator_1__BasicGeoposition;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.BasicGeoposition>
  IIterable_1__BasicGeoposition = interface;
  PIIterable_1__BasicGeoposition = ^IIterable_1__BasicGeoposition;

  // Windows.Devices.Geolocation.IGeoboundingBoxStatics
  IGeoboundingBoxStatics = interface;
  PIGeoboundingBoxStatics = ^IGeoboundingBoxStatics;

  // Windows.Devices.Geolocation.IGeocircle
  IGeocircle = interface;
  PIGeocircle = ^IGeocircle;

  // Windows.Devices.Geolocation.IGeocircleFactory
  IGeocircleFactory = interface;
  PIGeocircleFactory = ^IGeocircleFactory;

  // Windows.Devices.Geolocation.IGeocoordinateSatelliteData
  IGeocoordinateSatelliteData = interface;
  PIGeocoordinateSatelliteData = ^IGeocoordinateSatelliteData;

  // Windows.Devices.Geolocation.IGeocoordinateWithPoint
  IGeocoordinateWithPoint = interface;
  PIGeocoordinateWithPoint = ^IGeocoordinateWithPoint;

  // Windows.Devices.Geolocation.IGeocoordinateWithPositionData
  IGeocoordinateWithPositionData = interface;
  PIGeocoordinateWithPositionData = ^IGeocoordinateWithPositionData;

  // Windows.Devices.Geolocation.IGeocoordinateWithPositionSourceTimestamp
  IGeocoordinateWithPositionSourceTimestamp = interface;
  PIGeocoordinateWithPositionSourceTimestamp = ^IGeocoordinateWithPositionSourceTimestamp;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeoposition>
  AsyncOperationCompletedHandler_1__IGeoposition = interface;
  PAsyncOperationCompletedHandler_1__IGeoposition = ^AsyncOperationCompletedHandler_1__IGeoposition;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeoposition>
  IAsyncOperation_1__IGeoposition = interface;
  PIAsyncOperation_1__IGeoposition = ^IAsyncOperation_1__IGeoposition;

  // Windows.Devices.Geolocation.IPositionChangedEventArgs
  IPositionChangedEventArgs = interface;
  PIPositionChangedEventArgs = ^IPositionChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeolocator,Windows.Devices.Geolocation.IPositionChangedEventArgs>
  TypedEventHandler_2__IGeolocator__IPositionChangedEventArgs = interface;
  PTypedEventHandler_2__IGeolocator__IPositionChangedEventArgs = ^TypedEventHandler_2__IGeolocator__IPositionChangedEventArgs;

  // Windows.Devices.Geolocation.IStatusChangedEventArgs
  IStatusChangedEventArgs = interface;
  PIStatusChangedEventArgs = ^IStatusChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeolocator,Windows.Devices.Geolocation.IStatusChangedEventArgs>
  TypedEventHandler_2__IGeolocator__IStatusChangedEventArgs = interface;
  PTypedEventHandler_2__IGeolocator__IStatusChangedEventArgs = ^TypedEventHandler_2__IGeolocator__IStatusChangedEventArgs;

  // Windows.Devices.Geolocation.IGeolocator
  IGeolocator = interface;
  PIGeolocator = ^IGeolocator;

  // Windows.Devices.Geolocation.IGeolocator2
  IGeolocator2 = interface;
  PIGeolocator2 = ^IGeolocator2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.GeolocationAccessStatus>
  AsyncOperationCompletedHandler_1__GeolocationAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__GeolocationAccessStatus = ^AsyncOperationCompletedHandler_1__GeolocationAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.GeolocationAccessStatus>
  IAsyncOperation_1__GeolocationAccessStatus = interface;
  PIAsyncOperation_1__GeolocationAccessStatus = ^IAsyncOperation_1__GeolocationAccessStatus;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeoposition>
  IIterator_1__IGeoposition = interface;
  PIIterator_1__IGeoposition = ^IIterator_1__IGeoposition;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeoposition>
  IIterable_1__IGeoposition = interface;
  PIIterable_1__IGeoposition = ^IIterable_1__IGeoposition;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>
  IVectorView_1__IGeoposition = interface;
  PIVectorView_1__IGeoposition = ^IVectorView_1__IGeoposition;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition = ^AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>>
  IAsyncOperation_1__IVectorView_1__IGeoposition = interface;
  PIAsyncOperation_1__IVectorView_1__IGeoposition = ^IAsyncOperation_1__IVectorView_1__IGeoposition;

  // Windows.Devices.Geolocation.IGeolocatorStatics
  IGeolocatorStatics = interface;
  PIGeolocatorStatics = ^IGeolocatorStatics;

  // Windows.Foundation.IReference`1<Windows.Devices.Geolocation.BasicGeoposition>
  IReference_1__BasicGeoposition = interface;
  PIReference_1__BasicGeoposition = ^IReference_1__BasicGeoposition;

  // Windows.Devices.Geolocation.IGeolocatorStatics2
  IGeolocatorStatics2 = interface;
  PIGeolocatorStatics2 = ^IGeolocatorStatics2;

  // Windows.Devices.Geolocation.IGeolocatorWithScalarAccuracy
  IGeolocatorWithScalarAccuracy = interface;
  PIGeolocatorWithScalarAccuracy = ^IGeolocatorWithScalarAccuracy;

  // Windows.Devices.Geolocation.IGeopathFactory
  IGeopathFactory = interface;
  PIGeopathFactory = ^IGeopathFactory;

  // Windows.Devices.Geolocation.IGeopointFactory
  IGeopointFactory = interface;
  PIGeopointFactory = ^IGeopointFactory;

  // Windows.Devices.Geolocation.IVenueData
  IVenueData = interface;
  PIVenueData = ^IVenueData;

  // Windows.Devices.Geolocation.IGeoposition2
  IGeoposition2 = interface;
  PIGeoposition2 = ^IGeoposition2;

  // Windows.Devices.Geolocation.IGeovisit
  IGeovisit = interface;
  PIGeovisit = ^IGeovisit;

  // Windows.Devices.Geolocation.IGeovisitStateChangedEventArgs
  IGeovisitStateChangedEventArgs = interface;
  PIGeovisitStateChangedEventArgs = ^IGeovisitStateChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeovisitMonitor,Windows.Devices.Geolocation.IGeovisitStateChangedEventArgs>
  TypedEventHandler_2__IGeovisitMonitor__IGeovisitStateChangedEventArgs = interface;
  PTypedEventHandler_2__IGeovisitMonitor__IGeovisitStateChangedEventArgs = ^TypedEventHandler_2__IGeovisitMonitor__IGeovisitStateChangedEventArgs;

  // Windows.Devices.Geolocation.IGeovisitMonitor
  IGeovisitMonitor = interface;
  PIGeovisitMonitor = ^IGeovisitMonitor;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeovisit>
  AsyncOperationCompletedHandler_1__IGeovisit = interface;
  PAsyncOperationCompletedHandler_1__IGeovisit = ^AsyncOperationCompletedHandler_1__IGeovisit;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeovisit>
  IAsyncOperation_1__IGeovisit = interface;
  PIAsyncOperation_1__IGeovisit = ^IAsyncOperation_1__IGeovisit;

  // Windows.Devices.Geolocation.IGeovisitMonitorStatics
  IGeovisitMonitorStatics = interface;
  PIGeovisitMonitorStatics = ^IGeovisitMonitorStatics;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeovisit>
  IIterator_1__IGeovisit = interface;
  PIIterator_1__IGeovisit = ^IIterator_1__IGeovisit;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeovisit>
  IIterable_1__IGeovisit = interface;
  PIIterable_1__IGeovisit = ^IIterable_1__IGeovisit;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeovisit>
  IVectorView_1__IGeovisit = interface;
  PIVectorView_1__IGeovisit = ^IVectorView_1__IGeovisit;

  // Windows.Devices.Geolocation.IGeovisitTriggerDetails
  IGeovisitTriggerDetails = interface;
  PIGeovisitTriggerDetails = ^IGeovisitTriggerDetails;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeopoint>
  IIterator_1__IGeopoint = interface;
  PIIterator_1__IGeopoint = ^IIterator_1__IGeopoint;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeopoint>
  IIterable_1__IGeopoint = interface;
  PIIterable_1__IGeopoint = ^IIterable_1__IGeopoint;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeopoint>
  AsyncOperationCompletedHandler_1__IGeopoint = interface;
  PAsyncOperationCompletedHandler_1__IGeopoint = ^AsyncOperationCompletedHandler_1__IGeopoint;

  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeopoint>
  IAsyncOperation_1__IGeopoint = interface;
  PIAsyncOperation_1__IGeopoint = ^IAsyncOperation_1__IGeopoint;

  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeopath>
  IIterator_1__IGeopath = interface;
  PIIterator_1__IGeopath = ^IIterator_1__IGeopath;

  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeopath>
  IIterable_1__IGeopath = interface;
  PIIterable_1__IGeopath = ^IIterable_1__IGeopath;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeopath>
  IVectorView_1__IGeopath = interface;
  PIVectorView_1__IGeopath = ^IVectorView_1__IGeopath;

  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Geolocation.IGeopath>
  IVector_1__IGeopath = interface;
  PIVector_1__IGeopath = ^IVector_1__IGeopath;

  // Windows.Devices.Geolocation Enums

  // Windows.Devices.Geolocation.AltitudeReferenceSystem
  AltitudeReferenceSystem = (
    Unspecified = 0,
    Terrain = 1,
    Ellipsoid = 2,
    Geoid = 3,
    Surface = 4
  );
  PAltitudeReferenceSystem = ^AltitudeReferenceSystem;

  // Windows.Devices.Geolocation.Geofencing.GeofenceMonitorStatus
  Geofencing_GeofenceMonitorStatus = (
    Ready = 0,
    Initializing = 1,
    NoData = 2,
    Disabled = 3,
    NotInitialized = 4,
    NotAvailable = 5
  );
  PGeofencing_GeofenceMonitorStatus = ^Geofencing_GeofenceMonitorStatus;

  // Windows.Devices.Geolocation.Geofencing.GeofenceRemovalReason
  Geofencing_GeofenceRemovalReason = (
    Used = 0,
    Expired = 1
  );
  PGeofencing_GeofenceRemovalReason = ^Geofencing_GeofenceRemovalReason;

  // Windows.Devices.Geolocation.Geofencing.GeofenceState
  Geofencing_GeofenceState = (
    None = 0,
    Entered = 1,
    Exited = 2,
    Removed = 4
  );
  PGeofencing_GeofenceState = ^Geofencing_GeofenceState;

  // Windows.Devices.Geolocation.Geofencing.MonitoredGeofenceStates
  Geofencing_MonitoredGeofenceStates = (
    None = 0,
    Entered = 1,
    Exited = 2,
    Removed = 4
  );
  PGeofencing_MonitoredGeofenceStates = ^Geofencing_MonitoredGeofenceStates;

  // Windows.Devices.Geolocation.GeolocationAccessStatus
  GeolocationAccessStatus = (
    Unspecified = 0,
    Allowed = 1,
    Denied = 2
  );
  PGeolocationAccessStatus = ^GeolocationAccessStatus;

  // Windows.Devices.Geolocation.GeoshapeType
  GeoshapeType = (
    Geopoint = 0,
    Geocircle = 1,
    Geopath = 2,
    GeoboundingBox = 3
  );
  PGeoshapeType = ^GeoshapeType;

  // Windows.Devices.Geolocation.PositionAccuracy
  PositionAccuracy = (
    Default = 0,
    High = 1
  );
  PPositionAccuracy = ^PositionAccuracy;

  // Windows.Devices.Geolocation.PositionSource
  PositionSource = (
    Cellular = 0,
    Satellite = 1,
    WiFi = 2,
    IPAddress = 3,
    Unknown = 4,
    Default = 5,
    Obfuscated = 6
  );
  PPositionSource = ^PositionSource;

  // Windows.Devices.Geolocation.PositionStatus
  PositionStatus = (
    Ready = 0,
    Initializing = 1,
    NoData = 2,
    Disabled = 3,
    NotInitialized = 4,
    NotAvailable = 5
  );
  PPositionStatus = ^PositionStatus;

  // Windows.Devices.Geolocation.VisitMonitoringScope
  VisitMonitoringScope = (
    Venue = 0,
    City = 1
  );
  PVisitMonitoringScope = ^VisitMonitoringScope;

  // Windows.Devices.Geolocation.VisitStateChange
  VisitStateChange = (
    TrackingLost = 0,
    Arrived = 1,
    Departed = 2,
    OtherMovement = 3
  );
  PVisitStateChange = ^VisitStateChange;

  // Windows.Devices.Geolocation Interfaces

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeoshape
  IGeoshape = interface(IInspectable)
  ['{C99CA2AF-C729-43C1-8FAB-D6DEC914DF7E}']
    function get_GeoshapeType: GeoshapeType; safecall;
    function get_SpatialReferenceId: Cardinal; safecall;
    function get_AltitudeReferenceSystem: AltitudeReferenceSystem; safecall;
    property AltitudeReferenceSystem_: AltitudeReferenceSystem read get_AltitudeReferenceSystem;
    property GeoshapeType_: GeoshapeType read get_GeoshapeType;
    property SpatialReferenceId: Cardinal read get_SpatialReferenceId;
  end;

  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeocoordinate
  IGeocoordinate = interface(IInspectable)
  ['{EE21A3AA-976A-4C70-803D-083EA55BCBC4}']
    function get_Latitude: Double; safecall;
    function get_Longitude: Double; safecall;
    function get_Altitude: IReference_1__Double; safecall;
    function get_Accuracy: Double; safecall;
    function get_AltitudeAccuracy: IReference_1__Double; safecall;
    function get_Heading: IReference_1__Double; safecall;
    function get_Speed: IReference_1__Double; safecall;
    function get_Timestamp: DateTime; safecall;
    property Accuracy: Double read get_Accuracy;
    property Altitude: IReference_1__Double read get_Altitude;
    property AltitudeAccuracy: IReference_1__Double read get_AltitudeAccuracy;
    property Heading: IReference_1__Double read get_Heading;
    property Latitude: Double read get_Latitude;
    property Longitude: Double read get_Longitude;
    property Speed: IReference_1__Double read get_Speed;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // UsedAPI Interface
  // Windows.Devices.Geolocation.ICivicAddress
  ICivicAddress = interface(IInspectable)
  ['{A8567A1A-64F4-4D48-BCEA-F6B008ECA34C}']
    function get_Country: HSTRING; safecall;
    function get_State: HSTRING; safecall;
    function get_City: HSTRING; safecall;
    function get_PostalCode: HSTRING; safecall;
    function get_Timestamp: DateTime; safecall;
    property City: HSTRING read get_City;
    property Country: HSTRING read get_Country;
    property PostalCode: HSTRING read get_PostalCode;
    property State: HSTRING read get_State;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeoposition
  IGeoposition = interface(IInspectable)
  ['{C18D0454-7D41-4FF7-A957-9DFFB4EF7F5B}']
    function get_Coordinate: IGeocoordinate; safecall;
    function get_CivicAddress: ICivicAddress; safecall;
    property CivicAddress: ICivicAddress read get_CivicAddress;
    property Coordinate: IGeocoordinate read get_Coordinate;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeoboundingBoxFactory
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_GeoboundingBox)]
  IGeoboundingBoxFactory = interface(IInspectable)
  ['{4DFBA589-0411-4ABC-B3B5-5BBCCB57D98C}']
    function Create(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition): IGeoboundingBox; safecall;
    function CreateWithAltitudeReference(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeoboundingBox; safecall;
    function CreateWithAltitudeReferenceAndSpatialReference(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeoboundingBox; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.BasicGeoposition>
  IIterator_1__BasicGeoposition_Base = interface(IInspectable)
  ['{1B4E26A1-88E4-5872-BB2D-4F31700828B2}']
    function get_Current: BasicGeoposition; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PBasicGeoposition): Cardinal; safecall;
    property Current: BasicGeoposition read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.BasicGeoposition>
  IIterator_1__BasicGeoposition = interface(IIterator_1__BasicGeoposition_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.BasicGeoposition>
  IIterable_1__BasicGeoposition_Base = interface(IInspectable)
  ['{922399A8-0093-5009-A8D2-F87B0EAE75F5}']
    function First: IIterator_1__BasicGeoposition; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.BasicGeoposition>
  IIterable_1__BasicGeoposition = interface(IIterable_1__BasicGeoposition_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeoboundingBoxStatics
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_GeoboundingBox)]
  IGeoboundingBoxStatics = interface(IInspectable)
  ['{67B80708-E61A-4CD0-841B-93233792B5CA}']
    function TryCompute(positions: IIterable_1__BasicGeoposition): IGeoboundingBox; overload; safecall;
    function TryCompute(positions: IIterable_1__BasicGeoposition; altitudeRefSystem: AltitudeReferenceSystem): IGeoboundingBox; overload; safecall;
    function TryCompute(positions: IIterable_1__BasicGeoposition; altitudeRefSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeoboundingBox; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeocircle
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geocircle)]
  IGeocircle = interface(IInspectable)
  ['{39E45843-A7F9-4E63-92A7-BA0C28D124B1}']
    function get_Center: BasicGeoposition; safecall;
    function get_Radius: Double; safecall;
    property Center: BasicGeoposition read get_Center;
    property Radius: Double read get_Radius;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeocircleFactory
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geocircle)]
  IGeocircleFactory = interface(IInspectable)
  ['{AFD6531F-72B1-4F7D-87CC-4ED4C9849C05}']
    function Create(position: BasicGeoposition; radius: Double): IGeocircle; safecall;
    function CreateWithAltitudeReferenceSystem(position: BasicGeoposition; radius: Double; altitudeReferenceSystem: AltitudeReferenceSystem): IGeocircle; safecall;
    function CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position: BasicGeoposition; radius: Double; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeocircle; safecall;
  end;

  // Windows.Devices.Geolocation.IGeocoordinateSatelliteData
  IGeocoordinateSatelliteData = interface(IInspectable)
  ['{C32A74D9-2608-474C-912C-06DD490F4AF7}']
    function get_PositionDilutionOfPrecision: IReference_1__Double; safecall;
    function get_HorizontalDilutionOfPrecision: IReference_1__Double; safecall;
    function get_VerticalDilutionOfPrecision: IReference_1__Double; safecall;
    property HorizontalDilutionOfPrecision: IReference_1__Double read get_HorizontalDilutionOfPrecision;
    property PositionDilutionOfPrecision: IReference_1__Double read get_PositionDilutionOfPrecision;
    property VerticalDilutionOfPrecision: IReference_1__Double read get_VerticalDilutionOfPrecision;
  end;

  // Windows.Devices.Geolocation.IGeocoordinateWithPoint
  IGeocoordinateWithPoint = interface(IInspectable)
  ['{FEEA0525-D22C-4D46-B527-0B96066FC7DB}']
    function get_Point: IGeopoint; safecall;
    property Point: IGeopoint read get_Point;
  end;

  // Windows.Devices.Geolocation.IGeocoordinateWithPositionData
  IGeocoordinateWithPositionData = interface(IInspectable)
  ['{95E634BE-DBD6-40AC-B8F2-A65C0340D9A6}']
    function get_PositionSource: PositionSource; safecall;
    function get_SatelliteData: IGeocoordinateSatelliteData; safecall;
    property PositionSource_: PositionSource read get_PositionSource;
    property SatelliteData: IGeocoordinateSatelliteData read get_SatelliteData;
  end;

  // Windows.Devices.Geolocation.IGeocoordinateWithPositionSourceTimestamp
  IGeocoordinateWithPositionSourceTimestamp = interface(IInspectable)
  ['{8543FC02-C9F1-4610-AFE0-8BC3A6A87036}']
    function get_PositionSourceTimestamp: IReference_1__DateTime; safecall;
    property PositionSourceTimestamp: IReference_1__DateTime read get_PositionSourceTimestamp;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeoposition>
  AsyncOperationCompletedHandler_1__IGeoposition_Delegate_Base = interface(IUnknown)
  ['{7668A704-244E-5E12-8DCB-92A3299EBA26}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IGeoposition; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeoposition>
  AsyncOperationCompletedHandler_1__IGeoposition = interface(AsyncOperationCompletedHandler_1__IGeoposition_Delegate_Base)
  ['{7B8AE52A-070D-5658-B3CA-1B394091EA48}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeoposition>
  IAsyncOperation_1__IGeoposition_Base = interface(IInspectable)
  ['{EE73ECF0-099D-57E5-8407-5B32E5AF1CC4}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IGeoposition); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IGeoposition; safecall;
    function GetResults: IGeoposition; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IGeoposition read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeoposition>
  IAsyncOperation_1__IGeoposition = interface(IAsyncOperation_1__IGeoposition_Base)
  ['{9223B88D-2C6D-50F5-BD0F-F2C9928EECD3}']
  end;

  // UsedAPI Interface
  // Windows.Devices.Geolocation.IPositionChangedEventArgs
  IPositionChangedEventArgs = interface(IInspectable)
  ['{37859CE5-9D1E-46C5-BF3B-6AD8CAC1A093}']
    function get_Position: IGeoposition; safecall;
    property Position: IGeoposition read get_Position;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeolocator,Windows.Devices.Geolocation.IPositionChangedEventArgs>
  TypedEventHandler_2__IGeolocator__IPositionChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{DF3C6164-4E7B-5E8E-9A7E-13DA059DEC1E}']
    procedure Invoke(sender: IGeolocator; args: IPositionChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeolocator,Windows.Devices.Geolocation.IPositionChangedEventArgs>
  TypedEventHandler_2__IGeolocator__IPositionChangedEventArgs = interface(TypedEventHandler_2__IGeolocator__IPositionChangedEventArgs_Delegate_Base)
  ['{04806A98-49FF-53B3-AF96-93269759D5C2}']
  end;

  // UsedAPI Interface
  // Windows.Devices.Geolocation.IStatusChangedEventArgs
  IStatusChangedEventArgs = interface(IInspectable)
  ['{3453D2DA-8C93-4111-A205-9AECFC9BE5C0}']
    function get_Status: PositionStatus; safecall;
    property Status: PositionStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeolocator,Windows.Devices.Geolocation.IStatusChangedEventArgs>
  TypedEventHandler_2__IGeolocator__IStatusChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{97FCF582-DE6B-5CD3-9690-E2ECBB66DA4D}']
    procedure Invoke(sender: IGeolocator; args: IStatusChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeolocator,Windows.Devices.Geolocation.IStatusChangedEventArgs>
  TypedEventHandler_2__IGeolocator__IStatusChangedEventArgs = interface(TypedEventHandler_2__IGeolocator__IStatusChangedEventArgs_Delegate_Base)
  ['{B9F039C6-759A-5E7D-BB48-6D1B1BBB9023}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeolocator
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geolocator)]
  IGeolocator = interface(IInspectable)
  ['{A9C3BF62-4524-4989-8AA9-DE019D2E551F}']
    function get_DesiredAccuracy: PositionAccuracy; safecall;
    procedure put_DesiredAccuracy(value: PositionAccuracy); safecall;
    function get_MovementThreshold: Double; safecall;
    procedure put_MovementThreshold(value: Double); safecall;
    function get_ReportInterval: Cardinal; safecall;
    procedure put_ReportInterval(value: Cardinal); safecall;
    function get_LocationStatus: PositionStatus; safecall;
    function GetGeopositionAsync: IAsyncOperation_1__IGeoposition; overload; safecall;
    function GetGeopositionAsync(maximumAge: TimeSpan; timeout: TimeSpan): IAsyncOperation_1__IGeoposition; overload; safecall;
    function add_PositionChanged(handler: TypedEventHandler_2__IGeolocator__IPositionChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_PositionChanged(token: EventRegistrationToken); safecall;
    function add_StatusChanged(handler: TypedEventHandler_2__IGeolocator__IStatusChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_StatusChanged(token: EventRegistrationToken); safecall;
    property DesiredAccuracy: PositionAccuracy read get_DesiredAccuracy write put_DesiredAccuracy;
    property LocationStatus: PositionStatus read get_LocationStatus;
    property MovementThreshold: Double read get_MovementThreshold write put_MovementThreshold;
    property ReportInterval: Cardinal read get_ReportInterval write put_ReportInterval;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeolocator2
  IGeolocator2 = interface(IInspectable)
  ['{D1B42E6D-8891-43B4-AD36-27C6FE9A97B1}']
    procedure AllowFallbackToConsentlessPositions; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.GeolocationAccessStatus>
  AsyncOperationCompletedHandler_1__GeolocationAccessStatus_Delegate_Base = interface(IUnknown)
  ['{F3524C93-E5C7-5B88-BEDB-D3E637CFF271}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GeolocationAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.GeolocationAccessStatus>
  AsyncOperationCompletedHandler_1__GeolocationAccessStatus = interface(AsyncOperationCompletedHandler_1__GeolocationAccessStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.GeolocationAccessStatus>
  IAsyncOperation_1__GeolocationAccessStatus_Base = interface(IInspectable)
  ['{DE2B24D0-B726-57B1-A7C5-E5A13932B7DE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GeolocationAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GeolocationAccessStatus; safecall;
    function GetResults: GeolocationAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GeolocationAccessStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.GeolocationAccessStatus>
  IAsyncOperation_1__GeolocationAccessStatus = interface(IAsyncOperation_1__GeolocationAccessStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeoposition>
  IIterator_1__IGeoposition_Base = interface(IInspectable)
  ['{A99B4206-263E-5308-82F2-31315C65135C}']
    function get_Current: IGeoposition; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIGeoposition): Cardinal; safecall;
    property Current: IGeoposition read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeoposition>
  IIterator_1__IGeoposition = interface(IIterator_1__IGeoposition_Base)
  ['{D414D676-75D3-52FB-AA22-5829AD9FC4FF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeoposition>
  IIterable_1__IGeoposition_Base = interface(IInspectable)
  ['{135ED72D-75B1-5881-BE41-6FFEAA202044}']
    function First: IIterator_1__IGeoposition; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeoposition>
  IIterable_1__IGeoposition = interface(IIterable_1__IGeoposition_Base)
  ['{FCB45D3B-0B5C-5C0D-82E9-4CFB295147B9}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>
  IVectorView_1__IGeoposition = interface(IInspectable)
  ['{C9FFE173-AC85-50AD-B262-90DCC4CA5165}']
    function GetAt(index: Cardinal): IGeoposition; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IGeoposition; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGeoposition): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition_Delegate_Base = interface(IUnknown)
  ['{6C67A1D1-9441-5AEE-B625-E3C1B5676A6D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IGeoposition; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition = interface(AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition_Delegate_Base)
  ['{272FE546-D773-5C98-867B-B7084A054EC5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>>
  IAsyncOperation_1__IVectorView_1__IGeoposition_Base = interface(IInspectable)
  ['{9454B533-EFB4-5385-8D3A-437FABC32D91}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition; safecall;
    function GetResults: IVectorView_1__IGeoposition; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IGeoposition read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeoposition>>
  IAsyncOperation_1__IVectorView_1__IGeoposition = interface(IAsyncOperation_1__IVectorView_1__IGeoposition_Base)
  ['{4E4265BF-AAF7-5109-A9E3-0CEDD1D201D0}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeolocatorStatics
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geolocator)]
  IGeolocatorStatics = interface(IInspectable)
  ['{9A8E7571-2DF5-4591-9F87-EB5FD894E9B7}']
    function RequestAccessAsync: IAsyncOperation_1__GeolocationAccessStatus; safecall;
    function GetGeopositionHistoryAsync(startTime: DateTime): IAsyncOperation_1__IVectorView_1__IGeoposition; overload; safecall;
    function GetGeopositionHistoryAsync(startTime: DateTime; duration: TimeSpan): IAsyncOperation_1__IVectorView_1__IGeoposition; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Devices.Geolocation.BasicGeoposition>
  IReference_1__BasicGeoposition = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: BasicGeoposition; safecall;
    property Value: BasicGeoposition read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeolocatorStatics2
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geolocator)]
  IGeolocatorStatics2 = interface(IInspectable)
  ['{993011A2-FA1C-4631-A71D-0DBEB1250D9C}']
    function get_IsDefaultGeopositionRecommended: Boolean; safecall;
    procedure put_DefaultGeoposition(value: IReference_1__BasicGeoposition); safecall;
    function get_DefaultGeoposition: IReference_1__BasicGeoposition; safecall;
    property DefaultGeoposition: IReference_1__BasicGeoposition read get_DefaultGeoposition write put_DefaultGeoposition;
    property IsDefaultGeopositionRecommended: Boolean read get_IsDefaultGeopositionRecommended;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeolocatorWithScalarAccuracy
  IGeolocatorWithScalarAccuracy = interface(IInspectable)
  ['{96F5D3C1-B80F-460A-994D-A96C47A51AA4}']
    function get_DesiredAccuracyInMeters: IReference_1__Cardinal; safecall;
    procedure put_DesiredAccuracyInMeters(value: IReference_1__Cardinal); safecall;
    property DesiredAccuracyInMeters: IReference_1__Cardinal read get_DesiredAccuracyInMeters write put_DesiredAccuracyInMeters;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeopathFactory
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geopath)]
  IGeopathFactory = interface(IInspectable)
  ['{27BEA9C8-C7E7-4359-9B9B-FCA3E05EF593}']
    function Create(positions: IIterable_1__BasicGeoposition): IGeopath; safecall;
    function CreateWithAltitudeReference(positions: IIterable_1__BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeopath; safecall;
    function CreateWithAltitudeReferenceAndSpatialReference(positions: IIterable_1__BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeopath; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeopointFactory
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_Geopoint)]
  IGeopointFactory = interface(IInspectable)
  ['{DB6B8D33-76BD-4E30-8AF7-A844DC37B7A0}']
    function Create(position: BasicGeoposition): IGeopoint; safecall;
    function CreateWithAltitudeReferenceSystem(position: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeopoint; safecall;
    function CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeopoint; safecall;
  end;

  // Windows.Devices.Geolocation.IVenueData
  IVenueData = interface(IInspectable)
  ['{66F39187-60E3-4B2F-B527-4F53F1C3C677}']
    function get_Id: HSTRING; safecall;
    function get_Level: HSTRING; safecall;
    property Id: HSTRING read get_Id;
    property Level: HSTRING read get_Level;
  end;

  // Windows.Devices.Geolocation.IGeoposition2
  IGeoposition2 = interface(IInspectable)
  ['{7F62F697-8671-4B0D-86F8-474A8496187C}']
    function get_VenueData: IVenueData; safecall;
    property VenueData: IVenueData read get_VenueData;
  end;

  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeovisit
  IGeovisit = interface(IInspectable)
  ['{B1877A76-9EF6-41AB-A0DD-793ECE76E2DE}']
    function get_Position: IGeoposition; safecall;
    function get_StateChange: VisitStateChange; safecall;
    function get_Timestamp: DateTime; safecall;
    property Position: IGeoposition read get_Position;
    property StateChange: VisitStateChange read get_StateChange;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeovisitStateChangedEventArgs
  IGeovisitStateChangedEventArgs = interface(IInspectable)
  ['{CEB4D1FF-8B53-4968-BEED-4CECD029CE15}']
    function get_Visit: IGeovisit; safecall;
    property Visit: IGeovisit read get_Visit;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeovisitMonitor,Windows.Devices.Geolocation.IGeovisitStateChangedEventArgs>
  TypedEventHandler_2__IGeovisitMonitor__IGeovisitStateChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{76ABC5EA-EE4F-5391-9B50-DECA5D4311C9}']
    procedure Invoke(sender: IGeovisitMonitor; args: IGeovisitStateChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Devices.Geolocation.IGeovisitMonitor,Windows.Devices.Geolocation.IGeovisitStateChangedEventArgs>
  TypedEventHandler_2__IGeovisitMonitor__IGeovisitStateChangedEventArgs = interface(TypedEventHandler_2__IGeovisitMonitor__IGeovisitStateChangedEventArgs_Delegate_Base)
  ['{6654804C-3E53-5184-B739-3424F1648A6F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeovisitMonitor
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_GeovisitMonitor)]
  IGeovisitMonitor = interface(IInspectable)
  ['{80118AAF-5944-4591-83C1-396647F54F2C}']
    function get_MonitoringScope: VisitMonitoringScope; safecall;
    procedure Start(value: VisitMonitoringScope); safecall;
    procedure Stop; safecall;
    function add_VisitStateChanged(handler: TypedEventHandler_2__IGeovisitMonitor__IGeovisitStateChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_VisitStateChanged(token: EventRegistrationToken); safecall;
    property MonitoringScope: VisitMonitoringScope read get_MonitoringScope;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeovisit>
  AsyncOperationCompletedHandler_1__IGeovisit_Delegate_Base = interface(IUnknown)
  ['{B9BCE8CB-2E04-5269-9B03-1614D0C00B01}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IGeovisit; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeovisit>
  AsyncOperationCompletedHandler_1__IGeovisit = interface(AsyncOperationCompletedHandler_1__IGeovisit_Delegate_Base)
  ['{B303AADF-C706-506A-96CC-A3668EA00702}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeovisit>
  IAsyncOperation_1__IGeovisit_Base = interface(IInspectable)
  ['{8D1C950A-EFB9-5440-A6A7-820A839BE07B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IGeovisit); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IGeovisit; safecall;
    function GetResults: IGeovisit; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IGeovisit read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeovisit>
  IAsyncOperation_1__IGeovisit = interface(IAsyncOperation_1__IGeovisit_Base)
  ['{0B367603-F91D-5148-94F8-036C96D33EB7}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Devices.Geolocation.IGeovisitMonitorStatics
  [WinRTClassNameAttribute(SWindows_Devices_Geolocation_GeovisitMonitor)]
  IGeovisitMonitorStatics = interface(IInspectable)
  ['{BCF976A7-BBF2-4CDD-95CF-554C82EDFB87}']
    function GetLastReportAsync: IAsyncOperation_1__IGeovisit; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeovisit>
  IIterator_1__IGeovisit_Base = interface(IInspectable)
  ['{F525FC34-B058-5345-8E28-3E69E5F59070}']
    function get_Current: IGeovisit; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIGeovisit): Cardinal; safecall;
    property Current: IGeovisit read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeovisit>
  IIterator_1__IGeovisit = interface(IIterator_1__IGeovisit_Base)
  ['{3B4BC333-AA44-553D-8941-1203922ECB46}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeovisit>
  IIterable_1__IGeovisit_Base = interface(IInspectable)
  ['{D5800189-0F3F-54A0-A749-6000C1E12E58}']
    function First: IIterator_1__IGeovisit; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeovisit>
  IIterable_1__IGeovisit = interface(IIterable_1__IGeovisit_Base)
  ['{CDB92F84-3EF7-54E8-B085-C64BBC02DA8E}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeovisit>
  IVectorView_1__IGeovisit = interface(IInspectable)
  ['{D80376A4-25FC-591D-A791-422EEE8C4414}']
    function GetAt(index: Cardinal): IGeovisit; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IGeovisit; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGeovisit): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Devices.Geolocation.IGeovisitTriggerDetails
  IGeovisitTriggerDetails = interface(IInspectable)
  ['{EA770D9E-D1C9-454B-99B7-B2F8CDD2482F}']
    function ReadReports: IVectorView_1__IGeovisit; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeopoint>
  IIterator_1__IGeopoint_Base = interface(IInspectable)
  ['{88225B39-8BE9-5C03-9714-8F1642D8A43F}']
    function get_Current: IGeopoint; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIGeopoint): Cardinal; safecall;
    property Current: IGeopoint read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeopoint>
  IIterator_1__IGeopoint = interface(IIterator_1__IGeopoint_Base)
  ['{A11ECDF0-F895-592E-992D-497BEAA6E362}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeopoint>
  IIterable_1__IGeopoint_Base = interface(IInspectable)
  ['{E7617FC9-2CC7-5BD1-BC5A-F47260834ED8}']
    function First: IIterator_1__IGeopoint; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeopoint>
  IIterable_1__IGeopoint = interface(IIterable_1__IGeopoint_Base)
  ['{768FFCFF-2CDA-53B3-ABE6-39C5CCE15163}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeopoint>
  AsyncOperationCompletedHandler_1__IGeopoint_Delegate_Base = interface(IUnknown)
  ['{4B5F2F60-19B1-5566-9DF6-92A42235CBF9}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IGeopoint; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Devices.Geolocation.IGeopoint>
  AsyncOperationCompletedHandler_1__IGeopoint = interface(AsyncOperationCompletedHandler_1__IGeopoint_Delegate_Base)
  ['{7B608E60-1D1C-54F7-AC4C-21521D88B0D0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeopoint>
  IAsyncOperation_1__IGeopoint_Base = interface(IInspectable)
  ['{3723E070-C2AE-538F-846E-0F9D280310C0}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IGeopoint); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IGeopoint; safecall;
    function GetResults: IGeopoint; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IGeopoint read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Devices.Geolocation.IGeopoint>
  IAsyncOperation_1__IGeopoint = interface(IAsyncOperation_1__IGeopoint_Base)
  ['{791AAF1D-97B7-58A5-BC49-226231EE1F08}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeopath>
  IIterator_1__IGeopath_Base = interface(IInspectable)
  ['{CF1D4402-4754-57E7-AAE0-69CA42CBD8F2}']
    function get_Current: IGeopath; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIGeopath): Cardinal; safecall;
    property Current: IGeopath read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Devices.Geolocation.IGeopath>
  IIterator_1__IGeopath = interface(IIterator_1__IGeopath_Base)
  ['{BAA6EA4D-009D-53B3-8E2C-150C91322020}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeopath>
  IIterable_1__IGeopath_Base = interface(IInspectable)
  ['{246FCFBD-A881-5E8E-9908-C1B9EBDFEC78}']
    function First: IIterator_1__IGeopath; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Devices.Geolocation.IGeopath>
  IIterable_1__IGeopath = interface(IIterable_1__IGeopath_Base)
  ['{E5DA8BA4-E9D2-513A-9047-4CBF50B2B110}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Devices.Geolocation.IGeopath>
  IVectorView_1__IGeopath = interface(IInspectable)
  ['{51CA54FA-7BF2-5ED8-AB08-E524017DCB41}']
    function GetAt(index: Cardinal): IGeopath; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IGeopath; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGeopath): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Geolocation.IGeopath>
  IVector_1__IGeopath_Base = interface(IInspectable)
  ['{7CA97E6D-6893-5672-A0C7-625C5090381E}']
    function GetAt(index: Cardinal): IGeopath; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IGeopath; safecall;
    function IndexOf(value: IGeopath; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IGeopath); safecall;
    procedure InsertAt(index: Cardinal; value: IGeopath); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IGeopath); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGeopath): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIGeopath); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Devices.Geolocation.IGeopath>
  IVector_1__IGeopath = interface(IVector_1__IGeopath_Base)
  ['{5FB15268-9174-52C4-BEB4-2BC8E7098895}']
  end;

  // Windows.Devices.Geolocation.GeoboundingBox
  // DualAPI
  // Implements: Windows.Devices.Geolocation.IGeoboundingBox
  // Implements: Windows.Devices.Geolocation.IGeoshape
  // Statics: "Windows.Devices.Geolocation.IGeoboundingBoxStatics"
  // Factory: "Windows.Devices.Geolocation.IGeoboundingBoxFactory"
  TGeoboundingBox = class(TWinRTGenericImportFS<IGeoboundingBoxFactory, IGeoboundingBoxStatics>)
  public
    // -> IGeoboundingBoxStatics
    class function TryCompute(positions: IIterable_1__BasicGeoposition): IGeoboundingBox; overload; static; inline;
    class function TryCompute(positions: IIterable_1__BasicGeoposition; altitudeRefSystem: AltitudeReferenceSystem): IGeoboundingBox; overload; static; inline;
    class function TryCompute(positions: IIterable_1__BasicGeoposition; altitudeRefSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeoboundingBox; overload; static; inline;

    // -> IGeoboundingBoxFactory
    class function Create(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition): IGeoboundingBox; static; inline;
    class function CreateWithAltitudeReference(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeoboundingBox; static; inline;
    class function CreateWithAltitudeReferenceAndSpatialReference(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeoboundingBox; static; inline;
  end;

  // Windows.Devices.Geolocation.Geocircle
  // DualAPI
  // Implements: Windows.Devices.Geolocation.IGeocircle
  // Implements: Windows.Devices.Geolocation.IGeoshape
  // Factory: "Windows.Devices.Geolocation.IGeocircleFactory"
  TGeocircle = class(TWinRTGenericImportF<IGeocircleFactory>)
  public
    // -> IGeocircleFactory
    class function Create(position: BasicGeoposition; radius: Double): IGeocircle; static; inline;
    class function CreateWithAltitudeReferenceSystem(position: BasicGeoposition; radius: Double; altitudeReferenceSystem: AltitudeReferenceSystem): IGeocircle; static; inline;
    class function CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position: BasicGeoposition; radius: Double; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeocircle; static; inline;
  end;

  // Windows.Devices.Geolocation.Geolocator
  // DualAPI
  // Implements: Windows.Devices.Geolocation.IGeolocator
  // Implements: Windows.Devices.Geolocation.IGeolocatorWithScalarAccuracy
  // Implements: Windows.Devices.Geolocation.IGeolocator2
  // Statics: "Windows.Devices.Geolocation.IGeolocatorStatics"
  // Statics: "Windows.Devices.Geolocation.IGeolocatorStatics2"
  // Instantiable: "IGeolocator"
  TGeolocator = class(TWinRTGenericImportS2I<IGeolocatorStatics, IGeolocatorStatics2, IGeolocator>)
  public
    // -> IGeolocatorStatics
    class function RequestAccessAsync: IAsyncOperation_1__GeolocationAccessStatus; static; inline;
    class function GetGeopositionHistoryAsync(startTime: DateTime): IAsyncOperation_1__IVectorView_1__IGeoposition; overload; static; inline;
    class function GetGeopositionHistoryAsync(startTime: DateTime; duration: TimeSpan): IAsyncOperation_1__IVectorView_1__IGeoposition; overload; static; inline;

    // -> IGeolocatorStatics2
    class function get_IsDefaultGeopositionRecommended: Boolean; static; inline;
    class procedure put_DefaultGeoposition(value: IReference_1__BasicGeoposition); static; inline;
    class function get_DefaultGeoposition: IReference_1__BasicGeoposition; static; inline;
    class property DefaultGeoposition: IReference_1__BasicGeoposition read get_DefaultGeoposition write put_DefaultGeoposition;
    class property IsDefaultGeopositionRecommended: Boolean read get_IsDefaultGeopositionRecommended;
  end;

  // Windows.Devices.Geolocation.Geopath
  // DualAPI
  // Implements: Windows.Devices.Geolocation.IGeopath
  // Implements: Windows.Devices.Geolocation.IGeoshape
  // Factory: "Windows.Devices.Geolocation.IGeopathFactory"
  TGeopath = class(TWinRTGenericImportF<IGeopathFactory>)
  public
    // -> IGeopathFactory
    class function Create(positions: IIterable_1__BasicGeoposition): IGeopath; static; inline;
    class function CreateWithAltitudeReference(positions: IIterable_1__BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeopath; static; inline;
    class function CreateWithAltitudeReferenceAndSpatialReference(positions: IIterable_1__BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeopath; static; inline;
  end;

  // Windows.Devices.Geolocation.Geopoint
  // DualAPI
  // Implements: Windows.Devices.Geolocation.IGeopoint
  // Implements: Windows.Devices.Geolocation.IGeoshape
  // Factory: "Windows.Devices.Geolocation.IGeopointFactory"
  TGeopoint = class(TWinRTGenericImportF<IGeopointFactory>)
  public
    // -> IGeopointFactory
    class function Create(position: BasicGeoposition): IGeopoint; static; inline;
    class function CreateWithAltitudeReferenceSystem(position: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeopoint; static; inline;
    class function CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeopoint; static; inline;
  end;

  // Windows.Devices.Geolocation.GeovisitMonitor
  // DualAPI
  // Implements: Windows.Devices.Geolocation.IGeovisitMonitor
  // Statics: "Windows.Devices.Geolocation.IGeovisitMonitorStatics"
  // Instantiable: "IGeovisitMonitor"
  TGeovisitMonitor = class(TWinRTGenericImportSI<IGeovisitMonitorStatics, IGeovisitMonitor>)
  public
    // -> IGeovisitMonitorStatics
    class function GetLastReportAsync: IAsyncOperation_1__IGeovisit; static; inline;
  end;

implementation

{ TGeoboundingBox }

class function TGeoboundingBox.TryCompute(positions: IIterable_1__BasicGeoposition): IGeoboundingBox;
begin
  Result := Statics.TryCompute(positions);
end;

class function TGeoboundingBox.TryCompute(positions: IIterable_1__BasicGeoposition; altitudeRefSystem: AltitudeReferenceSystem): IGeoboundingBox;
begin
  Result := Statics.TryCompute(positions, altitudeRefSystem);
end;

class function TGeoboundingBox.TryCompute(positions: IIterable_1__BasicGeoposition; altitudeRefSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeoboundingBox;
begin
  Result := Statics.TryCompute(positions, altitudeRefSystem, spatialReferenceId);
end;

// Factories for : "GeoboundingBox"
// Factory: "Windows.Devices.Geolocation.IGeoboundingBoxFactory"
// -> IGeoboundingBoxFactory

class function TGeoboundingBox.Create(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition): IGeoboundingBox;
begin
  Result := Factory.Create(northwestCorner, southeastCorner);
end;

class function TGeoboundingBox.CreateWithAltitudeReference(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeoboundingBox;
begin
  Result := Factory.CreateWithAltitudeReference(northwestCorner, southeastCorner, altitudeReferenceSystem);
end;

class function TGeoboundingBox.CreateWithAltitudeReferenceAndSpatialReference(northwestCorner: BasicGeoposition; southeastCorner: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeoboundingBox;
begin
  Result := Factory.CreateWithAltitudeReferenceAndSpatialReference(northwestCorner, southeastCorner, altitudeReferenceSystem, spatialReferenceId);
end;


{ TGeocircle }
// Factories for : "Geocircle"
// Factory: "Windows.Devices.Geolocation.IGeocircleFactory"
// -> IGeocircleFactory

class function TGeocircle.Create(position: BasicGeoposition; radius: Double): IGeocircle;
begin
  Result := Factory.Create(position, radius);
end;

class function TGeocircle.CreateWithAltitudeReferenceSystem(position: BasicGeoposition; radius: Double; altitudeReferenceSystem: AltitudeReferenceSystem): IGeocircle;
begin
  Result := Factory.CreateWithAltitudeReferenceSystem(position, radius, altitudeReferenceSystem);
end;

class function TGeocircle.CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position: BasicGeoposition; radius: Double; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeocircle;
begin
  Result := Factory.CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position, radius, altitudeReferenceSystem, spatialReferenceId);
end;


{ TGeolocator }

class function TGeolocator.RequestAccessAsync: IAsyncOperation_1__GeolocationAccessStatus;
begin
  Result := Statics.RequestAccessAsync;
end;

class function TGeolocator.GetGeopositionHistoryAsync(startTime: DateTime): IAsyncOperation_1__IVectorView_1__IGeoposition;
begin
  Result := Statics.GetGeopositionHistoryAsync(startTime);
end;

class function TGeolocator.GetGeopositionHistoryAsync(startTime: DateTime; duration: TimeSpan): IAsyncOperation_1__IVectorView_1__IGeoposition;
begin
  Result := Statics.GetGeopositionHistoryAsync(startTime, duration);
end;


class function TGeolocator.get_IsDefaultGeopositionRecommended: Boolean;
begin
  Result := Statics2.get_IsDefaultGeopositionRecommended;
end;

class procedure TGeolocator.put_DefaultGeoposition(value: IReference_1__BasicGeoposition);
begin
  Statics2.put_DefaultGeoposition(value);
end;

class function TGeolocator.get_DefaultGeoposition: IReference_1__BasicGeoposition;
begin
  Result := Statics2.get_DefaultGeoposition;
end;


{ TGeopath }
// Factories for : "Geopath"
// Factory: "Windows.Devices.Geolocation.IGeopathFactory"
// -> IGeopathFactory

class function TGeopath.Create(positions: IIterable_1__BasicGeoposition): IGeopath;
begin
  Result := Factory.Create(positions);
end;

class function TGeopath.CreateWithAltitudeReference(positions: IIterable_1__BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeopath;
begin
  Result := Factory.CreateWithAltitudeReference(positions, altitudeReferenceSystem);
end;

class function TGeopath.CreateWithAltitudeReferenceAndSpatialReference(positions: IIterable_1__BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeopath;
begin
  Result := Factory.CreateWithAltitudeReferenceAndSpatialReference(positions, altitudeReferenceSystem, spatialReferenceId);
end;


{ TGeopoint }
// Factories for : "Geopoint"
// Factory: "Windows.Devices.Geolocation.IGeopointFactory"
// -> IGeopointFactory

class function TGeopoint.Create(position: BasicGeoposition): IGeopoint;
begin
  Result := Factory.Create(position);
end;

class function TGeopoint.CreateWithAltitudeReferenceSystem(position: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem): IGeopoint;
begin
  Result := Factory.CreateWithAltitudeReferenceSystem(position, altitudeReferenceSystem);
end;

class function TGeopoint.CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position: BasicGeoposition; altitudeReferenceSystem: AltitudeReferenceSystem; spatialReferenceId: Cardinal): IGeopoint;
begin
  Result := Factory.CreateWithAltitudeReferenceSystemAndSpatialReferenceId(position, altitudeReferenceSystem, spatialReferenceId);
end;


{ TGeovisitMonitor }

class function TGeovisitMonitor.GetLastReportAsync: IAsyncOperation_1__IGeovisit;
begin
  Result := Statics.GetLastReportAsync;
end;


end.
