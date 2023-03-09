{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.PlayServices.Maps;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Location,
  Androidapi.JNI.Os,
  Androidapi.JNI.PlayServices,
  Androidapi.JNI.Util,
  Androidapi.JNI.Widget;

type
// ===== Forward declarations =====

  JCameraUpdate = interface;//com.google.android.gms.maps.CameraUpdate
  JCameraUpdateFactory = interface;//com.google.android.gms.maps.CameraUpdateFactory
  JGoogleMap = interface;//com.google.android.gms.maps.GoogleMap
  JGoogleMap_CancelableCallback = interface;//com.google.android.gms.maps.GoogleMap$CancelableCallback
  JGoogleMap_InfoWindowAdapter = interface;//com.google.android.gms.maps.GoogleMap$InfoWindowAdapter
  JGoogleMap_OnCameraChangeListener = interface;//com.google.android.gms.maps.GoogleMap$OnCameraChangeListener
  JGoogleMap_OnCameraIdleListener = interface;//com.google.android.gms.maps.GoogleMap$OnCameraIdleListener
  JGoogleMap_OnCameraMoveCanceledListener = interface;//com.google.android.gms.maps.GoogleMap$OnCameraMoveCanceledListener
  JGoogleMap_OnCameraMoveListener = interface;//com.google.android.gms.maps.GoogleMap$OnCameraMoveListener
  JGoogleMap_OnCameraMoveStartedListener = interface;//com.google.android.gms.maps.GoogleMap$OnCameraMoveStartedListener
  JGoogleMap_OnCircleClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnCircleClickListener
  JGoogleMap_OnGroundOverlayClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnGroundOverlayClickListener
  JGoogleMap_OnIndoorStateChangeListener = interface;//com.google.android.gms.maps.GoogleMap$OnIndoorStateChangeListener
  JGoogleMap_OnInfoWindowClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnInfoWindowClickListener
  JGoogleMap_OnInfoWindowCloseListener = interface;//com.google.android.gms.maps.GoogleMap$OnInfoWindowCloseListener
  JGoogleMap_OnInfoWindowLongClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnInfoWindowLongClickListener
  JGoogleMap_OnMapClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMapClickListener
  JGoogleMap_OnMapLoadedCallback = interface;//com.google.android.gms.maps.GoogleMap$OnMapLoadedCallback
  JGoogleMap_OnMapLongClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMapLongClickListener
  JGoogleMap_OnMarkerClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMarkerClickListener
  JGoogleMap_OnMarkerDragListener = interface;//com.google.android.gms.maps.GoogleMap$OnMarkerDragListener
  JGoogleMap_OnMyLocationButtonClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMyLocationButtonClickListener
  JGoogleMap_OnMyLocationChangeListener = interface;//com.google.android.gms.maps.GoogleMap$OnMyLocationChangeListener
  JGoogleMap_OnMyLocationClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnMyLocationClickListener
  JGoogleMap_OnPoiClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnPoiClickListener
  JGoogleMap_OnPolygonClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnPolygonClickListener
  JGoogleMap_OnPolylineClickListener = interface;//com.google.android.gms.maps.GoogleMap$OnPolylineClickListener
  JGoogleMap_SnapshotReadyCallback = interface;//com.google.android.gms.maps.GoogleMap$SnapshotReadyCallback
  JGoogleMapOptions = interface;//com.google.android.gms.maps.GoogleMapOptions
  JLocationSource = interface;//com.google.android.gms.maps.LocationSource
  JLocationSource_OnLocationChangedListener = interface;//com.google.android.gms.maps.LocationSource$OnLocationChangedListener
  JMapView = interface;//com.google.android.gms.maps.MapView
  JMapsInitializer = interface;//com.google.android.gms.maps.MapsInitializer
  JOnMapReadyCallback = interface;//com.google.android.gms.maps.OnMapReadyCallback
  JProjection = interface;//com.google.android.gms.maps.Projection
  JUiSettings = interface;//com.google.android.gms.maps.UiSettings
  JICameraUpdateFactoryDelegate = interface;//com.google.android.gms.maps.internal.ICameraUpdateFactoryDelegate
  JIGoogleMapDelegate = interface;//com.google.android.gms.maps.internal.IGoogleMapDelegate
  JILocationSourceDelegate = interface;//com.google.android.gms.maps.internal.ILocationSourceDelegate
  JIProjectionDelegate = interface;//com.google.android.gms.maps.internal.IProjectionDelegate
  JIUiSettingsDelegate = interface;//com.google.android.gms.maps.internal.IUiSettingsDelegate
  //Jinternal_zzab = interface;//com.google.android.gms.maps.internal.zzab
  //Jinternal_zzad = interface;//com.google.android.gms.maps.internal.zzad
  //Jinternal_zzaf = interface;//com.google.android.gms.maps.internal.zzaf
  //Jinternal_zzah = interface;//com.google.android.gms.maps.internal.zzah
  //Jinternal_zzaj = interface;//com.google.android.gms.maps.internal.zzaj
  //Jinternal_zzal = interface;//com.google.android.gms.maps.internal.zzal
  //Jinternal_zzan = interface;//com.google.android.gms.maps.internal.zzan
  //Jinternal_zzap = interface;//com.google.android.gms.maps.internal.zzap
  //Jinternal_zzar = interface;//com.google.android.gms.maps.internal.zzar
  //Jinternal_zzat = interface;//com.google.android.gms.maps.internal.zzat
  //Jinternal_zzav = interface;//com.google.android.gms.maps.internal.zzav
  //Jinternal_zzax = interface;//com.google.android.gms.maps.internal.zzax
  //Jinternal_zzaz = interface;//com.google.android.gms.maps.internal.zzaz
  //Jinternal_zzbb = interface;//com.google.android.gms.maps.internal.zzbb
  //Jinternal_zzbd = interface;//com.google.android.gms.maps.internal.zzbd
  //Jinternal_zzbf = interface;//com.google.android.gms.maps.internal.zzbf
  //Jinternal_zzbh = interface;//com.google.android.gms.maps.internal.zzbh
  //Jinternal_zzbu = interface;//com.google.android.gms.maps.internal.zzbu
  //Jmaps_internal_zzd = interface;//com.google.android.gms.maps.internal.zzd
  //Jmaps_internal_zzi = interface;//com.google.android.gms.maps.internal.zzi
  //Jmaps_internal_zzn = interface;//com.google.android.gms.maps.internal.zzn
  //Jmaps_internal_zzp = interface;//com.google.android.gms.maps.internal.zzp
  //Jmaps_internal_zzr = interface;//com.google.android.gms.maps.internal.zzr
  //Jinternal_zzt = interface;//com.google.android.gms.maps.internal.zzt
  //Jinternal_zzv = interface;//com.google.android.gms.maps.internal.zzv
  //Jinternal_zzx = interface;//com.google.android.gms.maps.internal.zzx
  //Jinternal_zzz = interface;//com.google.android.gms.maps.internal.zzz
  JBitmapDescriptor = interface;//com.google.android.gms.maps.model.BitmapDescriptor
  JBitmapDescriptorFactory = interface;//com.google.android.gms.maps.model.BitmapDescriptorFactory
  JCameraPosition = interface;//com.google.android.gms.maps.model.CameraPosition
  JCameraPosition_Builder = interface;//com.google.android.gms.maps.model.CameraPosition$Builder
  JCap = interface;//com.google.android.gms.maps.model.Cap
  JCircle = interface;//com.google.android.gms.maps.model.Circle
  JCircleOptions = interface;//com.google.android.gms.maps.model.CircleOptions
  JGroundOverlay = interface;//com.google.android.gms.maps.model.GroundOverlay
  JGroundOverlayOptions = interface;//com.google.android.gms.maps.model.GroundOverlayOptions
  JIndoorBuilding = interface;//com.google.android.gms.maps.model.IndoorBuilding
  JLatLng = interface;//com.google.android.gms.maps.model.LatLng
  JLatLngBounds = interface;//com.google.android.gms.maps.model.LatLngBounds
  JLatLngBounds_Builder = interface;//com.google.android.gms.maps.model.LatLngBounds$Builder
  JMapStyleOptions = interface;//com.google.android.gms.maps.model.MapStyleOptions
  JMarker = interface;//com.google.android.gms.maps.model.Marker
  JMarkerOptions = interface;//com.google.android.gms.maps.model.MarkerOptions
  JPointOfInterest = interface;//com.google.android.gms.maps.model.PointOfInterest
  JPolygon = interface;//com.google.android.gms.maps.model.Polygon
  JPolygonOptions = interface;//com.google.android.gms.maps.model.PolygonOptions
  JPolyline = interface;//com.google.android.gms.maps.model.Polyline
  JPolylineOptions = interface;//com.google.android.gms.maps.model.PolylineOptions
  JTile = interface;//com.google.android.gms.maps.model.Tile
  JTileOverlay = interface;//com.google.android.gms.maps.model.TileOverlay
  JTileOverlayOptions = interface;//com.google.android.gms.maps.model.TileOverlayOptions
  JTileProvider = interface;//com.google.android.gms.maps.model.TileProvider
  JVisibleRegion = interface;//com.google.android.gms.maps.model.VisibleRegion

// ===== Interface declarations =====

  JCameraUpdateClass = interface(JObjectClass)
    ['{7A21197F-BCBA-493B-B258-0631CADCB0FF}']
  end;

  [JavaSignature('com/google/android/gms/maps/CameraUpdate')]
  JCameraUpdate = interface(JObject)
    ['{11606994-4F45-4D5B-BA4E-22ABCAFE5A15}']
    function zza: JIObjectWrapper; cdecl;
  end;
  TJCameraUpdate = class(TJavaGenericImport<JCameraUpdateClass, JCameraUpdate>) end;

  JCameraUpdateFactoryClass = interface(JObjectClass)
    ['{67303D3C-CEA1-4D27-A15B-64C19309A076}']
    {class} function newCameraPosition(cameraPosition: JCameraPosition): JCameraUpdate; cdecl;
    {class} function newLatLng(latLng: JLatLng): JCameraUpdate; cdecl;
    {class} function newLatLngBounds(latLngBounds: JLatLngBounds; i: Integer): JCameraUpdate; cdecl; overload;
    {class} function newLatLngBounds(latLngBounds: JLatLngBounds; i: Integer; i1: Integer; i2: Integer): JCameraUpdate; cdecl; overload;
    {class} function newLatLngZoom(latLng: JLatLng; f: Single): JCameraUpdate; cdecl;
    {class} function scrollBy(f: Single; f1: Single): JCameraUpdate; cdecl;
    {class} function zoomBy(f: Single): JCameraUpdate; cdecl; overload;
    {class} function zoomBy(f: Single; point: JPoint): JCameraUpdate; cdecl; overload;
    {class} function zoomIn: JCameraUpdate; cdecl;
    {class} function zoomOut: JCameraUpdate; cdecl;
    {class} function zoomTo(f: Single): JCameraUpdate; cdecl;
    {class} procedure zza(iCameraUpdateFactoryDelegate: JICameraUpdateFactoryDelegate); cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/CameraUpdateFactory')]
  JCameraUpdateFactory = interface(JObject)
    ['{4E9D6497-D772-4A4C-9E34-48B7B1EED438}']
  end;
  TJCameraUpdateFactory = class(TJavaGenericImport<JCameraUpdateFactoryClass, JCameraUpdateFactory>) end;

  JGoogleMapClass = interface(JObjectClass)
    ['{400A62AC-F9F5-403A-91B0-CCA7F0FCFFA2}']
    {class} function _GetMAP_TYPE_HYBRID: Integer; cdecl;
    {class} function _GetMAP_TYPE_NONE: Integer; cdecl;
    {class} function _GetMAP_TYPE_NORMAL: Integer; cdecl;
    {class} function _GetMAP_TYPE_SATELLITE: Integer; cdecl;
    {class} function _GetMAP_TYPE_TERRAIN: Integer; cdecl;
    {class} function init(iGoogleMapDelegate: JIGoogleMapDelegate): JGoogleMap; cdecl;
    {class} property MAP_TYPE_HYBRID: Integer read _GetMAP_TYPE_HYBRID;
    {class} property MAP_TYPE_NONE: Integer read _GetMAP_TYPE_NONE;
    {class} property MAP_TYPE_NORMAL: Integer read _GetMAP_TYPE_NORMAL;
    {class} property MAP_TYPE_SATELLITE: Integer read _GetMAP_TYPE_SATELLITE;
    {class} property MAP_TYPE_TERRAIN: Integer read _GetMAP_TYPE_TERRAIN;
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap')]
  JGoogleMap = interface(JObject)
    ['{A525ED9B-BBC7-4848-9D2B-680E9D578195}']
    function addCircle(circleOptions: JCircleOptions): JCircle; cdecl;
    function addGroundOverlay(groundOverlayOptions: JGroundOverlayOptions): JGroundOverlay; cdecl;
    function addMarker(markerOptions: JMarkerOptions): JMarker; cdecl;
    function addPolygon(polygonOptions: JPolygonOptions): JPolygon; cdecl;
    function addPolyline(polylineOptions: JPolylineOptions): JPolyline; cdecl;
    function addTileOverlay(tileOverlayOptions: JTileOverlayOptions): JTileOverlay; cdecl;
    procedure animateCamera(cameraUpdate: JCameraUpdate); cdecl; overload;
    procedure animateCamera(cameraUpdate: JCameraUpdate; cancelableCallback: JGoogleMap_CancelableCallback); cdecl; overload;
    procedure animateCamera(cameraUpdate: JCameraUpdate; i: Integer; cancelableCallback: JGoogleMap_CancelableCallback); cdecl; overload;
    procedure clear; cdecl;
    function getCameraPosition: JCameraPosition; cdecl;
    function getFocusedBuilding: JIndoorBuilding; cdecl;
    function getMapType: Integer; cdecl;
    function getMaxZoomLevel: Single; cdecl;
    function getMinZoomLevel: Single; cdecl;
    function getMyLocation: JLocation; cdecl;
    function getProjection: JProjection; cdecl;
    function getUiSettings: JUiSettings; cdecl;
    function isBuildingsEnabled: Boolean; cdecl;
    function isIndoorEnabled: Boolean; cdecl;
    function isMyLocationEnabled: Boolean; cdecl;
    function isTrafficEnabled: Boolean; cdecl;
    procedure moveCamera(cameraUpdate: JCameraUpdate); cdecl;
    procedure resetMinMaxZoomPreference; cdecl;
    procedure setBuildingsEnabled(b: Boolean); cdecl;
    procedure setContentDescription(string_: JString); cdecl;
    function setIndoorEnabled(b: Boolean): Boolean; cdecl;
    procedure setInfoWindowAdapter(infoWindowAdapter: JGoogleMap_InfoWindowAdapter); cdecl;
    procedure setLatLngBoundsForCameraTarget(latLngBounds: JLatLngBounds); cdecl;
    procedure setLocationSource(locationSource: JLocationSource); cdecl;
    function setMapStyle(mapStyleOptions: JMapStyleOptions): Boolean; cdecl;
    procedure setMapType(i: Integer); cdecl;
    procedure setMaxZoomPreference(f: Single); cdecl;
    procedure setMinZoomPreference(f: Single); cdecl;
    procedure setMyLocationEnabled(b: Boolean); cdecl;
    procedure setOnCameraChangeListener(onCameraChangeListener: JGoogleMap_OnCameraChangeListener); cdecl;
    procedure setOnCameraIdleListener(onCameraIdleListener: JGoogleMap_OnCameraIdleListener); cdecl;
    procedure setOnCameraMoveCanceledListener(onCameraMoveCanceledListener: JGoogleMap_OnCameraMoveCanceledListener); cdecl;
    procedure setOnCameraMoveListener(onCameraMoveListener: JGoogleMap_OnCameraMoveListener); cdecl;
    procedure setOnCameraMoveStartedListener(onCameraMoveStartedListener: JGoogleMap_OnCameraMoveStartedListener); cdecl;
    procedure setOnCircleClickListener(onCircleClickListener: JGoogleMap_OnCircleClickListener); cdecl;
    procedure setOnGroundOverlayClickListener(onGroundOverlayClickListener: JGoogleMap_OnGroundOverlayClickListener); cdecl;
    procedure setOnIndoorStateChangeListener(onIndoorStateChangeListener: JGoogleMap_OnIndoorStateChangeListener); cdecl;
    procedure setOnInfoWindowClickListener(onInfoWindowClickListener: JGoogleMap_OnInfoWindowClickListener); cdecl;
    procedure setOnInfoWindowCloseListener(onInfoWindowCloseListener: JGoogleMap_OnInfoWindowCloseListener); cdecl;
    procedure setOnInfoWindowLongClickListener(onInfoWindowLongClickListener: JGoogleMap_OnInfoWindowLongClickListener); cdecl;
    procedure setOnMapClickListener(onMapClickListener: JGoogleMap_OnMapClickListener); cdecl;
    procedure setOnMapLoadedCallback(onMapLoadedCallback: JGoogleMap_OnMapLoadedCallback); cdecl;
    procedure setOnMapLongClickListener(onMapLongClickListener: JGoogleMap_OnMapLongClickListener); cdecl;
    procedure setOnMarkerClickListener(onMarkerClickListener: JGoogleMap_OnMarkerClickListener); cdecl;
    procedure setOnMarkerDragListener(onMarkerDragListener: JGoogleMap_OnMarkerDragListener); cdecl;
    procedure setOnMyLocationButtonClickListener(onMyLocationButtonClickListener: JGoogleMap_OnMyLocationButtonClickListener); cdecl;
    procedure setOnMyLocationChangeListener(onMyLocationChangeListener: JGoogleMap_OnMyLocationChangeListener); cdecl;
    procedure setOnMyLocationClickListener(onMyLocationClickListener: JGoogleMap_OnMyLocationClickListener); cdecl;
    procedure setOnPoiClickListener(onPoiClickListener: JGoogleMap_OnPoiClickListener); cdecl;
    procedure setOnPolygonClickListener(onPolygonClickListener: JGoogleMap_OnPolygonClickListener); cdecl;
    procedure setOnPolylineClickListener(onPolylineClickListener: JGoogleMap_OnPolylineClickListener); cdecl;
    procedure setPadding(i: Integer; i1: Integer; i2: Integer; i3: Integer); cdecl;
    procedure setTrafficEnabled(b: Boolean); cdecl;
    procedure snapshot(snapshotReadyCallback: JGoogleMap_SnapshotReadyCallback); cdecl; overload;
    procedure snapshot(snapshotReadyCallback: JGoogleMap_SnapshotReadyCallback; bitmap: JBitmap); cdecl; overload;
    procedure stopAnimation; cdecl;
  end;
  TJGoogleMap = class(TJavaGenericImport<JGoogleMapClass, JGoogleMap>) end;

  JGoogleMap_CancelableCallbackClass = interface(IJavaClass)
    ['{9614115C-26C0-42A4-BC6B-8C5C07FFE24B}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$CancelableCallback')]
  JGoogleMap_CancelableCallback = interface(IJavaInstance)
    ['{D1AC9707-AC15-4906-A687-B3A628DECC9D}']
    procedure onCancel; cdecl;
    procedure onFinish; cdecl;
  end;
  TJGoogleMap_CancelableCallback = class(TJavaGenericImport<JGoogleMap_CancelableCallbackClass, JGoogleMap_CancelableCallback>) end;

  JGoogleMap_InfoWindowAdapterClass = interface(IJavaClass)
    ['{4767E38C-50FA-43D2-A353-6562739F9B82}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$InfoWindowAdapter')]
  JGoogleMap_InfoWindowAdapter = interface(IJavaInstance)
    ['{DFF8DAEB-2C2B-44CC-9688-B88308CB1025}']
    function getInfoContents(marker: JMarker): JView; cdecl;
    function getInfoWindow(marker: JMarker): JView; cdecl;
  end;
  TJGoogleMap_InfoWindowAdapter = class(TJavaGenericImport<JGoogleMap_InfoWindowAdapterClass, JGoogleMap_InfoWindowAdapter>) end;

  JGoogleMap_OnCameraChangeListenerClass = interface(IJavaClass)
    ['{DCCF80DA-1231-4C0C-9161-93BA6A899D9A}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnCameraChangeListener')]
  JGoogleMap_OnCameraChangeListener = interface(IJavaInstance)
    ['{599287B7-6F74-4D70-822D-9C55486B2C89}']
    procedure onCameraChange(cameraPosition: JCameraPosition); cdecl;
  end;
  TJGoogleMap_OnCameraChangeListener = class(TJavaGenericImport<JGoogleMap_OnCameraChangeListenerClass, JGoogleMap_OnCameraChangeListener>) end;

  JGoogleMap_OnCameraIdleListenerClass = interface(IJavaClass)
    ['{CC208FBF-1F35-447B-AB28-45103280A32C}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnCameraIdleListener')]
  JGoogleMap_OnCameraIdleListener = interface(IJavaInstance)
    ['{E248D5E4-9FEF-47EC-A8B8-83F387591B7C}']
    procedure onCameraIdle; cdecl;
  end;
  TJGoogleMap_OnCameraIdleListener = class(TJavaGenericImport<JGoogleMap_OnCameraIdleListenerClass, JGoogleMap_OnCameraIdleListener>) end;

  JGoogleMap_OnCameraMoveCanceledListenerClass = interface(IJavaClass)
    ['{CB1C3E9C-AA1F-4CC5-84CB-01DED61A27AA}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnCameraMoveCanceledListener')]
  JGoogleMap_OnCameraMoveCanceledListener = interface(IJavaInstance)
    ['{02938C7D-4C39-49A1-ABAD-BF755A1F63C5}']
    procedure onCameraMoveCanceled; cdecl;
  end;
  TJGoogleMap_OnCameraMoveCanceledListener = class(TJavaGenericImport<JGoogleMap_OnCameraMoveCanceledListenerClass, JGoogleMap_OnCameraMoveCanceledListener>) end;

  JGoogleMap_OnCameraMoveListenerClass = interface(IJavaClass)
    ['{66C0C861-DFDD-457A-9DFC-C47EC44DFBB5}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnCameraMoveListener')]
  JGoogleMap_OnCameraMoveListener = interface(IJavaInstance)
    ['{76D455BE-93FF-43C1-85E0-2E18D7F432AB}']
    procedure onCameraMove; cdecl;
  end;
  TJGoogleMap_OnCameraMoveListener = class(TJavaGenericImport<JGoogleMap_OnCameraMoveListenerClass, JGoogleMap_OnCameraMoveListener>) end;

  JGoogleMap_OnCameraMoveStartedListenerClass = interface(IJavaClass)
    ['{B8FE659B-FD7C-4B26-A902-411E286BA74E}']
    {class} function _GetREASON_API_ANIMATION: Integer; cdecl;
    {class} function _GetREASON_DEVELOPER_ANIMATION: Integer; cdecl;
    {class} function _GetREASON_GESTURE: Integer; cdecl;
    {class} property REASON_API_ANIMATION: Integer read _GetREASON_API_ANIMATION;
    {class} property REASON_DEVELOPER_ANIMATION: Integer read _GetREASON_DEVELOPER_ANIMATION;
    {class} property REASON_GESTURE: Integer read _GetREASON_GESTURE;
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnCameraMoveStartedListener')]
  JGoogleMap_OnCameraMoveStartedListener = interface(IJavaInstance)
    ['{2D047FE1-DB2E-4C52-A4B9-3F497A38A7D0}']
    procedure onCameraMoveStarted(i: Integer); cdecl;
  end;
  TJGoogleMap_OnCameraMoveStartedListener = class(TJavaGenericImport<JGoogleMap_OnCameraMoveStartedListenerClass, JGoogleMap_OnCameraMoveStartedListener>) end;

  JGoogleMap_OnCircleClickListenerClass = interface(IJavaClass)
    ['{5420A4B2-2F9C-46CE-A7C6-3B26873F0F99}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnCircleClickListener')]
  JGoogleMap_OnCircleClickListener = interface(IJavaInstance)
    ['{63BC1847-5657-4BAD-B66B-EBF152F5FDAA}']
    procedure onCircleClick(circle: JCircle); cdecl;
  end;
  TJGoogleMap_OnCircleClickListener = class(TJavaGenericImport<JGoogleMap_OnCircleClickListenerClass, JGoogleMap_OnCircleClickListener>) end;

  JGoogleMap_OnGroundOverlayClickListenerClass = interface(IJavaClass)
    ['{D81916A6-D10D-474A-8963-7EAB58D68C7E}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnGroundOverlayClickListener')]
  JGoogleMap_OnGroundOverlayClickListener = interface(IJavaInstance)
    ['{9F7BF0D5-A76C-4097-926C-84A851DAD335}']
    procedure onGroundOverlayClick(groundOverlay: JGroundOverlay); cdecl;
  end;
  TJGoogleMap_OnGroundOverlayClickListener = class(TJavaGenericImport<JGoogleMap_OnGroundOverlayClickListenerClass, JGoogleMap_OnGroundOverlayClickListener>) end;

  JGoogleMap_OnIndoorStateChangeListenerClass = interface(IJavaClass)
    ['{C1CB6EA4-0F8B-4373-9950-D9F40A990131}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnIndoorStateChangeListener')]
  JGoogleMap_OnIndoorStateChangeListener = interface(IJavaInstance)
    ['{C684287F-B4AC-4F02-81B7-57BA3E55F2B0}']
    procedure onIndoorBuildingFocused; cdecl;
    procedure onIndoorLevelActivated(indoorBuilding: JIndoorBuilding); cdecl;
  end;
  TJGoogleMap_OnIndoorStateChangeListener = class(TJavaGenericImport<JGoogleMap_OnIndoorStateChangeListenerClass, JGoogleMap_OnIndoorStateChangeListener>) end;

  JGoogleMap_OnInfoWindowClickListenerClass = interface(IJavaClass)
    ['{020474F8-83E2-4E62-A37E-F07D54D4298A}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnInfoWindowClickListener')]
  JGoogleMap_OnInfoWindowClickListener = interface(IJavaInstance)
    ['{FB213E06-A38F-4C91-998D-AB98AF202C29}']
    procedure onInfoWindowClick(marker: JMarker); cdecl;
  end;
  TJGoogleMap_OnInfoWindowClickListener = class(TJavaGenericImport<JGoogleMap_OnInfoWindowClickListenerClass, JGoogleMap_OnInfoWindowClickListener>) end;

  JGoogleMap_OnInfoWindowCloseListenerClass = interface(IJavaClass)
    ['{A591A5F1-7389-44CC-97E3-C29A4F7E1851}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnInfoWindowCloseListener')]
  JGoogleMap_OnInfoWindowCloseListener = interface(IJavaInstance)
    ['{F3E1E561-6C2A-4D91-9637-4CC1497C58BF}']
    procedure onInfoWindowClose(marker: JMarker); cdecl;
  end;
  TJGoogleMap_OnInfoWindowCloseListener = class(TJavaGenericImport<JGoogleMap_OnInfoWindowCloseListenerClass, JGoogleMap_OnInfoWindowCloseListener>) end;

  JGoogleMap_OnInfoWindowLongClickListenerClass = interface(IJavaClass)
    ['{C87D0063-F309-41A9-A26D-2CB79742D3A5}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnInfoWindowLongClickListener')]
  JGoogleMap_OnInfoWindowLongClickListener = interface(IJavaInstance)
    ['{E838384E-5ADA-46CB-A1A6-F06BD97A7AF8}']
    procedure onInfoWindowLongClick(marker: JMarker); cdecl;
  end;
  TJGoogleMap_OnInfoWindowLongClickListener = class(TJavaGenericImport<JGoogleMap_OnInfoWindowLongClickListenerClass, JGoogleMap_OnInfoWindowLongClickListener>) end;

  JGoogleMap_OnMapClickListenerClass = interface(IJavaClass)
    ['{0964A15E-AF26-4F35-8AC8-BB937E0A68B5}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMapClickListener')]
  JGoogleMap_OnMapClickListener = interface(IJavaInstance)
    ['{B1E14548-02B7-4168-AEB6-26A64E23FC91}']
    procedure onMapClick(latLng: JLatLng); cdecl;
  end;
  TJGoogleMap_OnMapClickListener = class(TJavaGenericImport<JGoogleMap_OnMapClickListenerClass, JGoogleMap_OnMapClickListener>) end;

  JGoogleMap_OnMapLoadedCallbackClass = interface(IJavaClass)
    ['{9C276FB8-9468-4FD0-99B6-5C75B16448E0}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMapLoadedCallback')]
  JGoogleMap_OnMapLoadedCallback = interface(IJavaInstance)
    ['{0B425EB3-1B2A-4936-851A-0DD6BE9CDD88}']
    procedure onMapLoaded; cdecl;
  end;
  TJGoogleMap_OnMapLoadedCallback = class(TJavaGenericImport<JGoogleMap_OnMapLoadedCallbackClass, JGoogleMap_OnMapLoadedCallback>) end;

  JGoogleMap_OnMapLongClickListenerClass = interface(IJavaClass)
    ['{17AD8C33-1410-4CB4-8DCA-9E4FF3A872BA}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMapLongClickListener')]
  JGoogleMap_OnMapLongClickListener = interface(IJavaInstance)
    ['{F0789EA1-C5E2-4269-A67A-5FF1B0482CA5}']
    procedure onMapLongClick(latLng: JLatLng); cdecl;
  end;
  TJGoogleMap_OnMapLongClickListener = class(TJavaGenericImport<JGoogleMap_OnMapLongClickListenerClass, JGoogleMap_OnMapLongClickListener>) end;

  JGoogleMap_OnMarkerClickListenerClass = interface(IJavaClass)
    ['{0726010E-E833-4431-B205-F5D1978508F4}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMarkerClickListener')]
  JGoogleMap_OnMarkerClickListener = interface(IJavaInstance)
    ['{C3FD23CC-A959-4720-A91D-22074E303764}']
    function onMarkerClick(marker: JMarker): Boolean; cdecl;
  end;
  TJGoogleMap_OnMarkerClickListener = class(TJavaGenericImport<JGoogleMap_OnMarkerClickListenerClass, JGoogleMap_OnMarkerClickListener>) end;

  JGoogleMap_OnMarkerDragListenerClass = interface(IJavaClass)
    ['{6E1353C5-A89C-4DAD-9613-1029DA10D9E4}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMarkerDragListener')]
  JGoogleMap_OnMarkerDragListener = interface(IJavaInstance)
    ['{F4D0B32E-859B-47D6-8ECD-573590756CD9}']
    procedure onMarkerDrag(marker: JMarker); cdecl;
    procedure onMarkerDragEnd(marker: JMarker); cdecl;
    procedure onMarkerDragStart(marker: JMarker); cdecl;
  end;
  TJGoogleMap_OnMarkerDragListener = class(TJavaGenericImport<JGoogleMap_OnMarkerDragListenerClass, JGoogleMap_OnMarkerDragListener>) end;

  JGoogleMap_OnMyLocationButtonClickListenerClass = interface(IJavaClass)
    ['{69815418-ECFB-4F8B-8CCF-062E5BA9DCAD}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMyLocationButtonClickListener')]
  JGoogleMap_OnMyLocationButtonClickListener = interface(IJavaInstance)
    ['{BE4965DB-A640-42DA-9AD2-1F96425737F1}']
    function onMyLocationButtonClick: Boolean; cdecl;
  end;
  TJGoogleMap_OnMyLocationButtonClickListener = class(TJavaGenericImport<JGoogleMap_OnMyLocationButtonClickListenerClass, JGoogleMap_OnMyLocationButtonClickListener>) end;

  JGoogleMap_OnMyLocationChangeListenerClass = interface(IJavaClass)
    ['{A016AFD3-AE05-4306-80C4-DDED25000E54}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMyLocationChangeListener')]
  JGoogleMap_OnMyLocationChangeListener = interface(IJavaInstance)
    ['{44089694-6E57-4859-939D-97E59C7BAAAE}']
    procedure onMyLocationChange(location: JLocation); cdecl;
  end;
  TJGoogleMap_OnMyLocationChangeListener = class(TJavaGenericImport<JGoogleMap_OnMyLocationChangeListenerClass, JGoogleMap_OnMyLocationChangeListener>) end;

  JGoogleMap_OnMyLocationClickListenerClass = interface(IJavaClass)
    ['{F27AEAB1-A0DB-4D40-92AB-E209EA44CDC1}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnMyLocationClickListener')]
  JGoogleMap_OnMyLocationClickListener = interface(IJavaInstance)
    ['{80069797-79A1-481F-AA5D-6EEC22A9C5B0}']
    procedure onMyLocationClick(location: JLocation); cdecl;
  end;
  TJGoogleMap_OnMyLocationClickListener = class(TJavaGenericImport<JGoogleMap_OnMyLocationClickListenerClass, JGoogleMap_OnMyLocationClickListener>) end;

  JGoogleMap_OnPoiClickListenerClass = interface(IJavaClass)
    ['{707F3621-FE40-46E0-889F-EF2143839FFF}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnPoiClickListener')]
  JGoogleMap_OnPoiClickListener = interface(IJavaInstance)
    ['{E5BCCD09-DBB2-470C-A231-D42C662B04F6}']
    procedure onPoiClick(pointOfInterest: JPointOfInterest); cdecl;
  end;
  TJGoogleMap_OnPoiClickListener = class(TJavaGenericImport<JGoogleMap_OnPoiClickListenerClass, JGoogleMap_OnPoiClickListener>) end;

  JGoogleMap_OnPolygonClickListenerClass = interface(IJavaClass)
    ['{B583D43C-6616-4146-9CD9-F5E8223C490A}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnPolygonClickListener')]
  JGoogleMap_OnPolygonClickListener = interface(IJavaInstance)
    ['{7666A22B-58B2-41B9-97BD-C682B5D35B76}']
    procedure onPolygonClick(polygon: JPolygon); cdecl;
  end;
  TJGoogleMap_OnPolygonClickListener = class(TJavaGenericImport<JGoogleMap_OnPolygonClickListenerClass, JGoogleMap_OnPolygonClickListener>) end;

  JGoogleMap_OnPolylineClickListenerClass = interface(IJavaClass)
    ['{E2ED7D90-D888-4565-8CF3-FB00DCC4F619}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$OnPolylineClickListener')]
  JGoogleMap_OnPolylineClickListener = interface(IJavaInstance)
    ['{5E980422-D8A1-44EF-B3D5-7177578D1EB8}']
    procedure onPolylineClick(polyline: JPolyline); cdecl;
  end;
  TJGoogleMap_OnPolylineClickListener = class(TJavaGenericImport<JGoogleMap_OnPolylineClickListenerClass, JGoogleMap_OnPolylineClickListener>) end;

  JGoogleMap_SnapshotReadyCallbackClass = interface(IJavaClass)
    ['{61B0A354-3EDF-4C4C-AFA1-31C9ECD7E7CD}']
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMap$SnapshotReadyCallback')]
  JGoogleMap_SnapshotReadyCallback = interface(IJavaInstance)
    ['{53C58A5E-B8D3-4999-924B-8D053025EA12}']
    procedure onSnapshotReady(bitmap: JBitmap); cdecl;
  end;
  TJGoogleMap_SnapshotReadyCallback = class(TJavaGenericImport<JGoogleMap_SnapshotReadyCallbackClass, JGoogleMap_SnapshotReadyCallback>) end;

  JGoogleMapOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{6858A5C7-54A7-40DE-9A0C-1DC32ED24A12}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function createFromAttributes(context: JContext; attributeSet: JAttributeSet): JGoogleMapOptions; cdecl;
    {class} function init: JGoogleMapOptions; cdecl; overload;
    {class} function zza(context: JContext; attributeSet: JAttributeSet): JLatLngBounds; cdecl;
    {class} function zzb(context: JContext; attributeSet: JAttributeSet): JCameraPosition; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/GoogleMapOptions')]
  JGoogleMapOptions = interface(JAbstractSafeParcelable)
    ['{DC638BBD-E6D4-4F74-B531-53B24E101B06}']
    function ambientEnabled(b: Boolean): JGoogleMapOptions; cdecl;
    function camera(cameraPosition: JCameraPosition): JGoogleMapOptions; cdecl;
    function compassEnabled(b: Boolean): JGoogleMapOptions; cdecl;
    function getAmbientEnabled: JBoolean; cdecl;
    function getCamera: JCameraPosition; cdecl;
    function getCompassEnabled: JBoolean; cdecl;
    function getLatLngBoundsForCameraTarget: JLatLngBounds; cdecl;
    function getLiteMode: JBoolean; cdecl;
    function getMapToolbarEnabled: JBoolean; cdecl;
    function getMapType: Integer; cdecl;
    function getMaxZoomPreference: JFloat; cdecl;
    function getMinZoomPreference: JFloat; cdecl;
    function getRotateGesturesEnabled: JBoolean; cdecl;
    function getScrollGesturesEnabled: JBoolean; cdecl;
    function getScrollGesturesEnabledDuringRotateOrZoom: JBoolean; cdecl;
    function getTiltGesturesEnabled: JBoolean; cdecl;
    function getUseViewLifecycleInFragment: JBoolean; cdecl;
    function getZOrderOnTop: JBoolean; cdecl;
    function getZoomControlsEnabled: JBoolean; cdecl;
    function getZoomGesturesEnabled: JBoolean; cdecl;
    function latLngBoundsForCameraTarget(latLngBounds: JLatLngBounds): JGoogleMapOptions; cdecl;
    function liteMode(b: Boolean): JGoogleMapOptions; cdecl;
    function mapToolbarEnabled(b: Boolean): JGoogleMapOptions; cdecl;
    function mapType(i: Integer): JGoogleMapOptions; cdecl;
    function maxZoomPreference(f: Single): JGoogleMapOptions; cdecl;
    function minZoomPreference(f: Single): JGoogleMapOptions; cdecl;
    function rotateGesturesEnabled(b: Boolean): JGoogleMapOptions; cdecl;
    function scrollGesturesEnabled(b: Boolean): JGoogleMapOptions; cdecl;
    function scrollGesturesEnabledDuringRotateOrZoom(b: Boolean): JGoogleMapOptions; cdecl;
    function tiltGesturesEnabled(b: Boolean): JGoogleMapOptions; cdecl;
    function toString: JString; cdecl;
    function useViewLifecycleInFragment(b: Boolean): JGoogleMapOptions; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    function zOrderOnTop(b: Boolean): JGoogleMapOptions; cdecl;
    function zoomControlsEnabled(b: Boolean): JGoogleMapOptions; cdecl;
    function zoomGesturesEnabled(b: Boolean): JGoogleMapOptions; cdecl;
  end;
  TJGoogleMapOptions = class(TJavaGenericImport<JGoogleMapOptionsClass, JGoogleMapOptions>) end;

  JLocationSourceClass = interface(IJavaClass)
    ['{DD46DE05-8B4F-47D2-8910-3B19D7FD9939}']
  end;

  [JavaSignature('com/google/android/gms/maps/LocationSource')]
  JLocationSource = interface(IJavaInstance)
    ['{DE211F4A-D940-4E56-AAE3-21F7A885E2FE}']
    procedure activate(onLocationChangedListener: JLocationSource_OnLocationChangedListener); cdecl;
    procedure deactivate; cdecl;
  end;
  TJLocationSource = class(TJavaGenericImport<JLocationSourceClass, JLocationSource>) end;

  JLocationSource_OnLocationChangedListenerClass = interface(IJavaClass)
    ['{C1885854-F25B-4E28-8AB9-2052BFBAA973}']
  end;

  [JavaSignature('com/google/android/gms/maps/LocationSource$OnLocationChangedListener')]
  JLocationSource_OnLocationChangedListener = interface(IJavaInstance)
    ['{8A25D8FD-F6ED-4D09-A98C-BA569E82D7BB}']
    procedure onLocationChanged(location: JLocation); cdecl;
  end;
  TJLocationSource_OnLocationChangedListener = class(TJavaGenericImport<JLocationSource_OnLocationChangedListenerClass, JLocationSource_OnLocationChangedListener>) end;

  JMapViewClass = interface(JFrameLayoutClass)
    ['{1EE5FDF1-67E7-4FC9-AA41-D5B05E629B84}']
    {class} function init(context: JContext): JMapView; cdecl; overload;
    {class} function init(context: JContext; googleMapOptions: JGoogleMapOptions): JMapView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JMapView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet; i: Integer): JMapView; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/maps/MapView')]
  JMapView = interface(JFrameLayout)
    ['{FDC9DF68-5392-450C-A85C-E33754D8BFAD}']
    procedure getMapAsync(onMapReadyCallback: JOnMapReadyCallback); cdecl;
    procedure onCreate(bundle: JBundle); cdecl;
    procedure onDestroy; cdecl;
    procedure onEnterAmbient(bundle: JBundle); cdecl;
    procedure onExitAmbient; cdecl;
    procedure onLowMemory; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    procedure onSaveInstanceState(bundle: JBundle); cdecl;
    procedure onStart; cdecl;
    procedure onStop; cdecl;
  end;
  TJMapView = class(TJavaGenericImport<JMapViewClass, JMapView>) end;

  JMapsInitializerClass = interface(JObjectClass)
    ['{589D1A92-1661-448C-9560-8F20ABA1157C}']
    {class} function initialize(context: JContext): Integer; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/MapsInitializer')]
  JMapsInitializer = interface(JObject)
    ['{8708DF65-6068-4C6E-B345-603FC91F7C6A}']
  end;
  TJMapsInitializer = class(TJavaGenericImport<JMapsInitializerClass, JMapsInitializer>) end;

  JOnMapReadyCallbackClass = interface(IJavaClass)
    ['{B19B1560-053F-438D-BA83-9CCC414D09D6}']
  end;

  [JavaSignature('com/google/android/gms/maps/OnMapReadyCallback')]
  JOnMapReadyCallback = interface(IJavaInstance)
    ['{C5394813-868D-43FA-8778-71F37DFA699F}']
    procedure onMapReady(googleMap: JGoogleMap); cdecl;
  end;
  TJOnMapReadyCallback = class(TJavaGenericImport<JOnMapReadyCallbackClass, JOnMapReadyCallback>) end;

  JProjectionClass = interface(JObjectClass)
    ['{39569102-F14D-4DC9-9FC6-CEA9F4431A79}']
  end;

  [JavaSignature('com/google/android/gms/maps/Projection')]
  JProjection = interface(JObject)
    ['{6B32610E-3FC7-4956-AD4B-3472F4023DC7}']
    function fromScreenLocation(point: JPoint): JLatLng; cdecl;
    function getVisibleRegion: JVisibleRegion; cdecl;
    function toScreenLocation(latLng: JLatLng): JPoint; cdecl;
  end;
  TJProjection = class(TJavaGenericImport<JProjectionClass, JProjection>) end;

  JUiSettingsClass = interface(JObjectClass)
    ['{E177867D-1013-41DC-8CC9-61E764F41FC2}']
  end;

  [JavaSignature('com/google/android/gms/maps/UiSettings')]
  JUiSettings = interface(JObject)
    ['{C166F17B-4B44-4A09-8542-222C1668594A}']
    function isCompassEnabled: Boolean; cdecl;
    function isIndoorLevelPickerEnabled: Boolean; cdecl;
    function isMapToolbarEnabled: Boolean; cdecl;
    function isMyLocationButtonEnabled: Boolean; cdecl;
    function isRotateGesturesEnabled: Boolean; cdecl;
    function isScrollGesturesEnabled: Boolean; cdecl;
    function isScrollGesturesEnabledDuringRotateOrZoom: Boolean; cdecl;
    function isTiltGesturesEnabled: Boolean; cdecl;
    function isZoomControlsEnabled: Boolean; cdecl;
    function isZoomGesturesEnabled: Boolean; cdecl;
    procedure setAllGesturesEnabled(b: Boolean); cdecl;
    procedure setCompassEnabled(b: Boolean); cdecl;
    procedure setIndoorLevelPickerEnabled(b: Boolean); cdecl;
    procedure setMapToolbarEnabled(b: Boolean); cdecl;
    procedure setMyLocationButtonEnabled(b: Boolean); cdecl;
    procedure setRotateGesturesEnabled(b: Boolean); cdecl;
    procedure setScrollGesturesEnabled(b: Boolean); cdecl;
    procedure setScrollGesturesEnabledDuringRotateOrZoom(b: Boolean); cdecl;
    procedure setTiltGesturesEnabled(b: Boolean); cdecl;
    procedure setZoomControlsEnabled(b: Boolean); cdecl;
    procedure setZoomGesturesEnabled(b: Boolean); cdecl;
  end;
  TJUiSettings = class(TJavaGenericImport<JUiSettingsClass, JUiSettings>) end;

  JICameraUpdateFactoryDelegateClass = interface(JIInterfaceClass)
    ['{07D64056-52B1-4C96-8CFB-37F9B8858E1F}']
  end;

  [JavaSignature('com/google/android/gms/maps/internal/ICameraUpdateFactoryDelegate')]
  JICameraUpdateFactoryDelegate = interface(JIInterface)
    ['{8304CE07-A2B8-4DE3-AEDA-741DA2821D85}']
    function newCameraPosition(cameraPosition: JCameraPosition): JIObjectWrapper; cdecl;
    function newLatLng(latLng: JLatLng): JIObjectWrapper; cdecl;
    function newLatLngBounds(latLngBounds: JLatLngBounds; i: Integer): JIObjectWrapper; cdecl;
    function newLatLngBoundsWithSize(latLngBounds: JLatLngBounds; i: Integer; i1: Integer; i2: Integer): JIObjectWrapper; cdecl;
    function newLatLngZoom(latLng: JLatLng; f: Single): JIObjectWrapper; cdecl;
    function scrollBy(f: Single; f1: Single): JIObjectWrapper; cdecl;
    function zoomBy(f: Single): JIObjectWrapper; cdecl;
    function zoomByWithFocus(f: Single; i: Integer; i1: Integer): JIObjectWrapper; cdecl;
    function zoomIn: JIObjectWrapper; cdecl;
    function zoomOut: JIObjectWrapper; cdecl;
    function zoomTo(f: Single): JIObjectWrapper; cdecl;
  end;
  TJICameraUpdateFactoryDelegate = class(TJavaGenericImport<JICameraUpdateFactoryDelegateClass, JICameraUpdateFactoryDelegate>) end;

  JIGoogleMapDelegateClass = interface(JIInterfaceClass)
    ['{8F1C7FEB-7BD3-4EAA-B99B-59A74CC7B8C7}']
  end;

  [JavaSignature('com/google/android/gms/maps/internal/IGoogleMapDelegate')]
  JIGoogleMapDelegate = interface(JIInterface)
    ['{7154878F-75B5-4D54-8126-D0FF7F396E90}']
    //function addCircle(circleOptions: JCircleOptions): Jmaps_zzl; cdecl;
    //function addGroundOverlay(groundOverlayOptions: JGroundOverlayOptions): Jmaps_zzo; cdecl;
    //function addMarker(markerOptions: JMarkerOptions): Jmaps_zzx; cdecl;
    //function addPolygon(polygonOptions: JPolygonOptions): Jmaps_zzaa; cdecl;
    //function addPolyline(polylineOptions: JPolylineOptions): Jmaps_zzad; cdecl;
    //function addTileOverlay(tileOverlayOptions: JTileOverlayOptions): Jmaps_zzag; cdecl;
    procedure animateCamera(iObjectWrapper: JIObjectWrapper); cdecl;
    //procedure animateCameraWithCallback(iObjectWrapper: JIObjectWrapper; zzd: Jmaps_internal_zzd); cdecl;
    //procedure animateCameraWithDurationAndCallback(iObjectWrapper: JIObjectWrapper; i: Integer; zzd: Jmaps_internal_zzd); cdecl;
    procedure clear; cdecl;
    function getCameraPosition: JCameraPosition; cdecl;
    //function getFocusedBuilding: Jmaps_zzr; cdecl;
    //procedure getMapAsync(zzar: Jinternal_zzar); cdecl;
    function getMapType: Integer; cdecl;
    function getMaxZoomLevel: Single; cdecl;
    function getMinZoomLevel: Single; cdecl;
    function getMyLocation: JLocation; cdecl;
    function getProjection: JIProjectionDelegate; cdecl;
    function getUiSettings: JIUiSettingsDelegate; cdecl;
    function isBuildingsEnabled: Boolean; cdecl;
    function isIndoorEnabled: Boolean; cdecl;
    function isMyLocationEnabled: Boolean; cdecl;
    function isTrafficEnabled: Boolean; cdecl;
    procedure moveCamera(iObjectWrapper: JIObjectWrapper); cdecl;
    procedure onCreate(bundle: JBundle); cdecl;
    procedure onDestroy; cdecl;
    procedure onEnterAmbient(bundle: JBundle); cdecl;
    procedure onExitAmbient; cdecl;
    procedure onLowMemory; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    procedure onSaveInstanceState(bundle: JBundle); cdecl;
    procedure onStart; cdecl;
    procedure onStop; cdecl;
    procedure resetMinMaxZoomPreference; cdecl;
    procedure setBuildingsEnabled(b: Boolean); cdecl;
    procedure setContentDescription(string_: JString); cdecl;
    function setIndoorEnabled(b: Boolean): Boolean; cdecl;
    //procedure setInfoWindowAdapter(zzi: Jmaps_internal_zzi); cdecl;
    procedure setLatLngBoundsForCameraTarget(latLngBounds: JLatLngBounds); cdecl;
    procedure setLocationSource(iLocationSourceDelegate: JILocationSourceDelegate); cdecl;
    function setMapStyle(mapStyleOptions: JMapStyleOptions): Boolean; cdecl;
    procedure setMapType(i: Integer); cdecl;
    procedure setMaxZoomPreference(f: Single); cdecl;
    procedure setMinZoomPreference(f: Single); cdecl;
    procedure setMyLocationEnabled(b: Boolean); cdecl;
    //procedure setOnCameraChangeListener(zzn: Jmaps_internal_zzn); cdecl;
    //procedure setOnCameraIdleListener(zzp: Jmaps_internal_zzp); cdecl;
    //procedure setOnCameraMoveCanceledListener(zzr: Jmaps_internal_zzr); cdecl;
    //procedure setOnCameraMoveListener(zzt: Jinternal_zzt); cdecl;
    //procedure setOnCameraMoveStartedListener(zzv: Jinternal_zzv); cdecl;
    //procedure setOnCircleClickListener(zzx: Jinternal_zzx); cdecl;
    //procedure setOnGroundOverlayClickListener(zzz: Jinternal_zzz); cdecl;
    //procedure setOnIndoorStateChangeListener(zzab: Jinternal_zzab); cdecl;
    //procedure setOnInfoWindowClickListener(zzad: Jinternal_zzad); cdecl;
    //procedure setOnInfoWindowCloseListener(zzaf: Jinternal_zzaf); cdecl;
    //procedure setOnInfoWindowLongClickListener(zzah: Jinternal_zzah); cdecl;
    //procedure setOnMapClickListener(zzal: Jinternal_zzal); cdecl;
    //procedure setOnMapLoadedCallback(zzan: Jinternal_zzan); cdecl;
    //procedure setOnMapLongClickListener(zzap: Jinternal_zzap); cdecl;
    //procedure setOnMarkerClickListener(zzat: Jinternal_zzat); cdecl;
    //procedure setOnMarkerDragListener(zzav: Jinternal_zzav); cdecl;
    //procedure setOnMyLocationButtonClickListener(zzax: Jinternal_zzax); cdecl;
    //procedure setOnMyLocationChangeListener(zzaz: Jinternal_zzaz); cdecl;
    //procedure setOnMyLocationClickListener(zzbb: Jinternal_zzbb); cdecl;
    //procedure setOnPoiClickListener(zzbd: Jinternal_zzbd); cdecl;
    //procedure setOnPolygonClickListener(zzbf: Jinternal_zzbf); cdecl;
    //procedure setOnPolylineClickListener(zzbh: Jinternal_zzbh); cdecl;
    procedure setPadding(i: Integer; i1: Integer; i2: Integer; i3: Integer); cdecl;
    procedure setTrafficEnabled(b: Boolean); cdecl;
    procedure setWatermarkEnabled(b: Boolean); cdecl;
    //procedure snapshot(zzbu: Jinternal_zzbu; iObjectWrapper: JIObjectWrapper); cdecl;
    //procedure snapshotForTest(zzbu: Jinternal_zzbu); cdecl;
    procedure stopAnimation; cdecl;
    function useViewLifecycleWhenInFragment: Boolean; cdecl;
  end;
  TJIGoogleMapDelegate = class(TJavaGenericImport<JIGoogleMapDelegateClass, JIGoogleMapDelegate>) end;

  JILocationSourceDelegateClass = interface(JIInterfaceClass)
    ['{9E105F6A-9DDF-42A1-A1B9-05CA3D9A48C8}']
  end;

  [JavaSignature('com/google/android/gms/maps/internal/ILocationSourceDelegate')]
  JILocationSourceDelegate = interface(JIInterface)
    ['{33B90AE3-00D3-4025-BC1F-D0D25CCAE092}']
    //procedure activate(zzaj: Jinternal_zzaj); cdecl;
    procedure deactivate; cdecl;
  end;
  TJILocationSourceDelegate = class(TJavaGenericImport<JILocationSourceDelegateClass, JILocationSourceDelegate>) end;

  JIProjectionDelegateClass = interface(JIInterfaceClass)
    ['{198CE567-66D1-47CB-A9F9-EF5753805C59}']
  end;

  [JavaSignature('com/google/android/gms/maps/internal/IProjectionDelegate')]
  JIProjectionDelegate = interface(JIInterface)
    ['{93D6C3BF-8B87-48B0-BB99-0A216589089A}']
    function fromScreenLocation(iObjectWrapper: JIObjectWrapper): JLatLng; cdecl;
    function getVisibleRegion: JVisibleRegion; cdecl;
    function toScreenLocation(latLng: JLatLng): JIObjectWrapper; cdecl;
  end;
  TJIProjectionDelegate = class(TJavaGenericImport<JIProjectionDelegateClass, JIProjectionDelegate>) end;

  JIUiSettingsDelegateClass = interface(JIInterfaceClass)
    ['{6B0678EE-DEF6-4D18-B5A4-32AE60895ABA}']
  end;

  [JavaSignature('com/google/android/gms/maps/internal/IUiSettingsDelegate')]
  JIUiSettingsDelegate = interface(JIInterface)
    ['{D43A9F19-A008-4E96-9779-009F6373AC2D}']
    function isCompassEnabled: Boolean; cdecl;
    function isIndoorLevelPickerEnabled: Boolean; cdecl;
    function isMapToolbarEnabled: Boolean; cdecl;
    function isMyLocationButtonEnabled: Boolean; cdecl;
    function isRotateGesturesEnabled: Boolean; cdecl;
    function isScrollGesturesEnabled: Boolean; cdecl;
    function isScrollGesturesEnabledDuringRotateOrZoom: Boolean; cdecl;
    function isTiltGesturesEnabled: Boolean; cdecl;
    function isZoomControlsEnabled: Boolean; cdecl;
    function isZoomGesturesEnabled: Boolean; cdecl;
    procedure setAllGesturesEnabled(b: Boolean); cdecl;
    procedure setCompassEnabled(b: Boolean); cdecl;
    procedure setIndoorLevelPickerEnabled(b: Boolean); cdecl;
    procedure setMapToolbarEnabled(b: Boolean); cdecl;
    procedure setMyLocationButtonEnabled(b: Boolean); cdecl;
    procedure setRotateGesturesEnabled(b: Boolean); cdecl;
    procedure setScrollGesturesEnabled(b: Boolean); cdecl;
    procedure setScrollGesturesEnabledDuringRotateOrZoom(b: Boolean); cdecl;
    procedure setTiltGesturesEnabled(b: Boolean); cdecl;
    procedure setZoomControlsEnabled(b: Boolean); cdecl;
    procedure setZoomGesturesEnabled(b: Boolean); cdecl;
  end;
  TJIUiSettingsDelegate = class(TJavaGenericImport<JIUiSettingsDelegateClass, JIUiSettingsDelegate>) end;

  // com.google.android.gms.maps.internal.zzab
  // com.google.android.gms.maps.internal.zzad
  // com.google.android.gms.maps.internal.zzaf
  // com.google.android.gms.maps.internal.zzah
  // com.google.android.gms.maps.internal.zzaj
  // com.google.android.gms.maps.internal.zzal
  // com.google.android.gms.maps.internal.zzan
  // com.google.android.gms.maps.internal.zzap
  // com.google.android.gms.maps.internal.zzar
  // com.google.android.gms.maps.internal.zzat
  // com.google.android.gms.maps.internal.zzav
  // com.google.android.gms.maps.internal.zzax
  // com.google.android.gms.maps.internal.zzaz
  // com.google.android.gms.maps.internal.zzbb
  // com.google.android.gms.maps.internal.zzbd
  // com.google.android.gms.maps.internal.zzbf
  // com.google.android.gms.maps.internal.zzbh
  // com.google.android.gms.maps.internal.zzbu
  // com.google.android.gms.maps.internal.zzd
  // com.google.android.gms.maps.internal.zzi
  // com.google.android.gms.maps.internal.zzn
  // com.google.android.gms.maps.internal.zzp
  // com.google.android.gms.maps.internal.zzr
  // com.google.android.gms.maps.internal.zzt
  // com.google.android.gms.maps.internal.zzv
  // com.google.android.gms.maps.internal.zzx
  // com.google.android.gms.maps.internal.zzz
  JBitmapDescriptorClass = interface(JObjectClass)
    ['{B3AC0084-AD2D-4B7C-A2E6-46E1F05DB7F6}']
    {class} function init(iObjectWrapper: JIObjectWrapper): JBitmapDescriptor; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/BitmapDescriptor')]
  JBitmapDescriptor = interface(JObject)
    ['{650F87E1-F0BF-4270-8ADC-11941A7015AB}']
    function zza: JIObjectWrapper; cdecl;
  end;
  TJBitmapDescriptor = class(TJavaGenericImport<JBitmapDescriptorClass, JBitmapDescriptor>) end;

  JBitmapDescriptorFactoryClass = interface(JObjectClass)
    ['{88545E48-00BE-4AEB-BF98-78308F5AB256}']
    {class} function _GetHUE_AZURE: Single; cdecl;
    {class} function _GetHUE_BLUE: Single; cdecl;
    {class} function _GetHUE_CYAN: Single; cdecl;
    {class} function _GetHUE_GREEN: Single; cdecl;
    {class} function _GetHUE_MAGENTA: Single; cdecl;
    {class} function _GetHUE_ORANGE: Single; cdecl;
    {class} function _GetHUE_RED: Single; cdecl;
    {class} function _GetHUE_ROSE: Single; cdecl;
    {class} function _GetHUE_VIOLET: Single; cdecl;
    {class} function _GetHUE_YELLOW: Single; cdecl;
    {class} function defaultMarker: JBitmapDescriptor; cdecl; overload;
    {class} function defaultMarker(f: Single): JBitmapDescriptor; cdecl; overload;
    {class} function fromAsset(string_: JString): JBitmapDescriptor; cdecl;
    {class} function fromBitmap(bitmap: JBitmap): JBitmapDescriptor; cdecl;
    {class} function fromFile(string_: JString): JBitmapDescriptor; cdecl;
    {class} function fromPath(string_: JString): JBitmapDescriptor; cdecl;
    {class} function fromResource(i: Integer): JBitmapDescriptor; cdecl;
    {class} //procedure zza(zzi: Jmaps_zzi); cdecl;
    {class} property HUE_AZURE: Single read _GetHUE_AZURE;
    {class} property HUE_BLUE: Single read _GetHUE_BLUE;
    {class} property HUE_CYAN: Single read _GetHUE_CYAN;
    {class} property HUE_GREEN: Single read _GetHUE_GREEN;
    {class} property HUE_MAGENTA: Single read _GetHUE_MAGENTA;
    {class} property HUE_ORANGE: Single read _GetHUE_ORANGE;
    {class} property HUE_RED: Single read _GetHUE_RED;
    {class} property HUE_ROSE: Single read _GetHUE_ROSE;
    {class} property HUE_VIOLET: Single read _GetHUE_VIOLET;
    {class} property HUE_YELLOW: Single read _GetHUE_YELLOW;
  end;

  [JavaSignature('com/google/android/gms/maps/model/BitmapDescriptorFactory')]
  JBitmapDescriptorFactory = interface(JObject)
    ['{83007807-E8EE-43BD-9312-78943B089FD5}']
  end;
  TJBitmapDescriptorFactory = class(TJavaGenericImport<JBitmapDescriptorFactoryClass, JBitmapDescriptorFactory>) end;

  JCameraPositionClass = interface(JAbstractSafeParcelableClass)
    ['{BBC26A98-4E32-43A4-A67C-B0FF586E1071}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function builder: JCameraPosition_Builder; cdecl; overload;
    {class} function builder(cameraPosition: JCameraPosition): JCameraPosition_Builder; cdecl; overload;
    {class} function createFromAttributes(context: JContext; attributeSet: JAttributeSet): JCameraPosition; cdecl;
    {class} function fromLatLngZoom(latLng: JLatLng; f: Single): JCameraPosition; cdecl;
    {class} function init(latLng: JLatLng; f: Single; f1: Single; f2: Single): JCameraPosition; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/CameraPosition')]
  JCameraPosition = interface(JAbstractSafeParcelable)
    ['{CBD6FBEA-582B-473B-959B-C7EB300574A2}']
    function _Getbearing: Single; cdecl;
    function _Gettarget: JLatLng; cdecl;
    function _Gettilt: Single; cdecl;
    function _Getzoom: Single; cdecl;
    function equals(object_: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    property bearing: Single read _Getbearing;
    property target: JLatLng read _Gettarget;
    property tilt: Single read _Gettilt;
    property zoom: Single read _Getzoom;
  end;
  TJCameraPosition = class(TJavaGenericImport<JCameraPositionClass, JCameraPosition>) end;

  JCameraPosition_BuilderClass = interface(JObjectClass)
    ['{84517F2E-069C-457D-B297-C29BCEA99D39}']
    {class} function init: JCameraPosition_Builder; cdecl; overload;
    {class} function init(cameraPosition: JCameraPosition): JCameraPosition_Builder; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/maps/model/CameraPosition$Builder')]
  JCameraPosition_Builder = interface(JObject)
    ['{10539B19-24B1-414A-A8C9-09FF64A805A1}']
    function bearing(f: Single): JCameraPosition_Builder; cdecl;
    function build: JCameraPosition; cdecl;
    function target(latLng: JLatLng): JCameraPosition_Builder; cdecl;
    function tilt(f: Single): JCameraPosition_Builder; cdecl;
    function zoom(f: Single): JCameraPosition_Builder; cdecl;
  end;
  TJCameraPosition_Builder = class(TJavaGenericImport<JCameraPosition_BuilderClass, JCameraPosition_Builder>) end;

  JCapClass = interface(JAbstractSafeParcelableClass)
    ['{E4FF4EB9-32AD-4122-A174-12366D22DBD7}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/Cap')]
  JCap = interface(JAbstractSafeParcelable)
    ['{3C9469EF-6D13-4243-9DB3-DF335056086B}']
    function equals(object_: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJCap = class(TJavaGenericImport<JCapClass, JCap>) end;

  JCircleClass = interface(JObjectClass)
    ['{EEB20939-CDBA-4C8C-BB4E-EA9F3FB16A14}']
    {class} //function init(zzl: Jmaps_zzl): JCircle; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/Circle')]
  JCircle = interface(JObject)
    ['{D1408DBE-690F-45FD-A740-8D2FA4E10CED}']
    function equals(object_: JObject): Boolean; cdecl;
    function getCenter: JLatLng; cdecl;
    function getFillColor: Integer; cdecl;
    function getId: JString; cdecl;
    function getRadius: Double; cdecl;
    function getStrokeColor: Integer; cdecl;
    function getStrokePattern: JList; cdecl;
    function getStrokeWidth: Single; cdecl;
    function getTag: JObject; cdecl;
    function getZIndex: Single; cdecl;
    function hashCode: Integer; cdecl;
    function isClickable: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    procedure remove; cdecl;
    procedure setCenter(latLng: JLatLng); cdecl;
    procedure setClickable(b: Boolean); cdecl;
    procedure setFillColor(i: Integer); cdecl;
    procedure setRadius(d: Double); cdecl;
    procedure setStrokeColor(i: Integer); cdecl;
    procedure setStrokePattern(list: JList); cdecl;
    procedure setStrokeWidth(f: Single); cdecl;
    procedure setTag(object_: JObject); cdecl;
    procedure setVisible(b: Boolean); cdecl;
    procedure setZIndex(f: Single); cdecl;
  end;
  TJCircle = class(TJavaGenericImport<JCircleClass, JCircle>) end;

  JCircleOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{D055A38E-CD1E-41B4-B6A0-F6EDF0869544}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JCircleOptions; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/CircleOptions')]
  JCircleOptions = interface(JAbstractSafeParcelable)
    ['{5EB2D906-C70F-48BC-A34B-BA102162163A}']
    function center(latLng: JLatLng): JCircleOptions; cdecl;
    function clickable(b: Boolean): JCircleOptions; cdecl;
    function fillColor(i: Integer): JCircleOptions; cdecl;
    function getCenter: JLatLng; cdecl;
    function getFillColor: Integer; cdecl;
    function getRadius: Double; cdecl;
    function getStrokeColor: Integer; cdecl;
    function getStrokePattern: JList; cdecl;
    function getStrokeWidth: Single; cdecl;
    function getZIndex: Single; cdecl;
    function isClickable: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    function radius(d: Double): JCircleOptions; cdecl;
    function strokeColor(i: Integer): JCircleOptions; cdecl;
    function strokePattern(list: JList): JCircleOptions; cdecl;
    function strokeWidth(f: Single): JCircleOptions; cdecl;
    function visible(b: Boolean): JCircleOptions; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    function zIndex(f: Single): JCircleOptions; cdecl;
  end;
  TJCircleOptions = class(TJavaGenericImport<JCircleOptionsClass, JCircleOptions>) end;

  JGroundOverlayClass = interface(JObjectClass)
    ['{71A8228E-C56A-4DEE-B5DE-600E6FE8F24D}']
    {class} //function init(zzo: Jmaps_zzo): JGroundOverlay; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/GroundOverlay')]
  JGroundOverlay = interface(JObject)
    ['{D9C7E7CE-26CA-4B65-B2B3-5A1C57E3AE3F}']
    function equals(object_: JObject): Boolean; cdecl;
    function getBearing: Single; cdecl;
    function getBounds: JLatLngBounds; cdecl;
    function getHeight: Single; cdecl;
    function getId: JString; cdecl;
    function getPosition: JLatLng; cdecl;
    function getTag: JObject; cdecl;
    function getTransparency: Single; cdecl;
    function getWidth: Single; cdecl;
    function getZIndex: Single; cdecl;
    function hashCode: Integer; cdecl;
    function isClickable: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    procedure remove; cdecl;
    procedure setBearing(f: Single); cdecl;
    procedure setClickable(b: Boolean); cdecl;
    procedure setDimensions(f: Single); cdecl; overload;
    procedure setDimensions(f: Single; f1: Single); cdecl; overload;
    procedure setImage(bitmapDescriptor: JBitmapDescriptor); cdecl;
    procedure setPosition(latLng: JLatLng); cdecl;
    procedure setPositionFromBounds(latLngBounds: JLatLngBounds); cdecl;
    procedure setTag(object_: JObject); cdecl;
    procedure setTransparency(f: Single); cdecl;
    procedure setVisible(b: Boolean); cdecl;
    procedure setZIndex(f: Single); cdecl;
  end;
  TJGroundOverlay = class(TJavaGenericImport<JGroundOverlayClass, JGroundOverlay>) end;

  JGroundOverlayOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{BAAAFFA3-3D94-421F-A2A0-BAFEC6D6DB68}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetNO_DIMENSION: Single; cdecl;
    {class} function init: JGroundOverlayOptions; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property NO_DIMENSION: Single read _GetNO_DIMENSION;
  end;

  [JavaSignature('com/google/android/gms/maps/model/GroundOverlayOptions')]
  JGroundOverlayOptions = interface(JAbstractSafeParcelable)
    ['{9109D62E-A7BA-44ED-9BB3-9460A31F3B94}']
    function anchor(f: Single; f1: Single): JGroundOverlayOptions; cdecl;
    function bearing(f: Single): JGroundOverlayOptions; cdecl;
    function clickable(b: Boolean): JGroundOverlayOptions; cdecl;
    function getAnchorU: Single; cdecl;
    function getAnchorV: Single; cdecl;
    function getBearing: Single; cdecl;
    function getBounds: JLatLngBounds; cdecl;
    function getHeight: Single; cdecl;
    function getImage: JBitmapDescriptor; cdecl;
    function getLocation: JLatLng; cdecl;
    function getTransparency: Single; cdecl;
    function getWidth: Single; cdecl;
    function getZIndex: Single; cdecl;
    function image(bitmapDescriptor: JBitmapDescriptor): JGroundOverlayOptions; cdecl;
    function isClickable: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    function position(latLng: JLatLng; f: Single): JGroundOverlayOptions; cdecl; overload;
    function position(latLng: JLatLng; f: Single; f1: Single): JGroundOverlayOptions; cdecl; overload;
    function positionFromBounds(latLngBounds: JLatLngBounds): JGroundOverlayOptions; cdecl;
    function transparency(f: Single): JGroundOverlayOptions; cdecl;
    function visible(b: Boolean): JGroundOverlayOptions; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    function zIndex(f: Single): JGroundOverlayOptions; cdecl;
  end;
  TJGroundOverlayOptions = class(TJavaGenericImport<JGroundOverlayOptionsClass, JGroundOverlayOptions>) end;

  JIndoorBuildingClass = interface(JObjectClass)
    ['{57086D57-C8B8-451B-8502-399EDE2AC61A}']
    {class} //function init(zzr: Jmaps_zzr): JIndoorBuilding; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/IndoorBuilding')]
  JIndoorBuilding = interface(JObject)
    ['{35D9C33B-E716-406A-BC6B-1D760E6AF36E}']
    function equals(object_: JObject): Boolean; cdecl;
    function getActiveLevelIndex: Integer; cdecl;
    function getDefaultLevelIndex: Integer; cdecl;
    function getLevels: JList; cdecl;
    function hashCode: Integer; cdecl;
    function isUnderground: Boolean; cdecl;
  end;
  TJIndoorBuilding = class(TJavaGenericImport<JIndoorBuildingClass, JIndoorBuilding>) end;

  JLatLngClass = interface(JAbstractSafeParcelableClass)
    ['{009A2FD1-2E26-4E50-8D97-FF49B8D3E134}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(d: Double; d1: Double): JLatLng; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/LatLng')]
  JLatLng = interface(JAbstractSafeParcelable)
    ['{1612BD16-050A-4E8E-BBFF-1447D11BED15}']
    function _Getlatitude: Double; cdecl;
    function _Getlongitude: Double; cdecl;
    function equals(object_: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    property latitude: Double read _Getlatitude;
    property longitude: Double read _Getlongitude;
  end;
  TJLatLng = class(TJavaGenericImport<JLatLngClass, JLatLng>) end;

  JLatLngBoundsClass = interface(JAbstractSafeParcelableClass)
    ['{E18E597F-AE80-4248-A4D1-CE1F3202668C}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function builder: JLatLngBounds_Builder; cdecl;
    {class} function createFromAttributes(context: JContext; attributeSet: JAttributeSet): JLatLngBounds; cdecl;
    {class} function init(latLng: JLatLng; latLng1: JLatLng): JLatLngBounds; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/LatLngBounds')]
  JLatLngBounds = interface(JAbstractSafeParcelable)
    ['{65EF16BB-A68A-43E0-BC7D-0FBFE1321A73}']
    function _Getnortheast: JLatLng; cdecl;
    function _Getsouthwest: JLatLng; cdecl;
    function &contains(latLng: JLatLng): Boolean; cdecl;
    function equals(object_: JObject): Boolean; cdecl;
    function getCenter: JLatLng; cdecl;
    function hashCode: Integer; cdecl;
    function including(latLng: JLatLng): JLatLngBounds; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    property northeast: JLatLng read _Getnortheast;
    property southwest: JLatLng read _Getsouthwest;
  end;
  TJLatLngBounds = class(TJavaGenericImport<JLatLngBoundsClass, JLatLngBounds>) end;

  JLatLngBounds_BuilderClass = interface(JObjectClass)
    ['{FD670806-8BF8-4499-A135-6A46A4905B7C}']
    {class} function init: JLatLngBounds_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/LatLngBounds$Builder')]
  JLatLngBounds_Builder = interface(JObject)
    ['{BC10B895-4AAC-4B39-9F7A-207B7D80DFE4}']
    function build: JLatLngBounds; cdecl;
    function include(latLng: JLatLng): JLatLngBounds_Builder; cdecl;
  end;
  TJLatLngBounds_Builder = class(TJavaGenericImport<JLatLngBounds_BuilderClass, JLatLngBounds_Builder>) end;

  JMapStyleOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{3D755E6C-A829-4FCE-B660-B04260D7B12F}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(string_: JString): JMapStyleOptions; cdecl;
    {class} function loadRawResourceStyle(context: JContext; i: Integer): JMapStyleOptions; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/MapStyleOptions')]
  JMapStyleOptions = interface(JAbstractSafeParcelable)
    ['{5CE77666-7040-4F26-BA4F-1DA674E99C90}']
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJMapStyleOptions = class(TJavaGenericImport<JMapStyleOptionsClass, JMapStyleOptions>) end;

  JMarkerClass = interface(JObjectClass)
    ['{4A8555E9-4F3A-4C8A-A75E-17FB0015ABBB}']
    {class} //function init(zzx: Jmaps_zzx): JMarker; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/Marker')]
  JMarker = interface(JObject)
    ['{16BF016C-85CC-4E27-A86A-A066BDDCA0B7}']
    function equals(object_: JObject): Boolean; cdecl;
    function getAlpha: Single; cdecl;
    function getId: JString; cdecl;
    function getPosition: JLatLng; cdecl;
    function getRotation: Single; cdecl;
    function getSnippet: JString; cdecl;
    function getTag: JObject; cdecl;
    function getTitle: JString; cdecl;
    function getZIndex: Single; cdecl;
    function hashCode: Integer; cdecl;
    procedure hideInfoWindow; cdecl;
    function isDraggable: Boolean; cdecl;
    function isFlat: Boolean; cdecl;
    function isInfoWindowShown: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    procedure remove; cdecl;
    procedure setAlpha(f: Single); cdecl;
    procedure setAnchor(f: Single; f1: Single); cdecl;
    procedure setDraggable(b: Boolean); cdecl;
    procedure setFlat(b: Boolean); cdecl;
    procedure setIcon(bitmapDescriptor: JBitmapDescriptor); cdecl;
    procedure setInfoWindowAnchor(f: Single; f1: Single); cdecl;
    procedure setPosition(latLng: JLatLng); cdecl;
    procedure setRotation(f: Single); cdecl;
    procedure setSnippet(string_: JString); cdecl;
    procedure setTag(object_: JObject); cdecl;
    procedure setTitle(string_: JString); cdecl;
    procedure setVisible(b: Boolean); cdecl;
    procedure setZIndex(f: Single); cdecl;
    procedure showInfoWindow; cdecl;
  end;
  TJMarker = class(TJavaGenericImport<JMarkerClass, JMarker>) end;

  JMarkerOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{BDD0C13B-A5CE-4FD0-85E0-268A9170847F}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JMarkerOptions; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/MarkerOptions')]
  JMarkerOptions = interface(JAbstractSafeParcelable)
    ['{60B30661-9D1C-4E6F-999B-0F7F13556E9B}']
    function alpha(f: Single): JMarkerOptions; cdecl;
    function anchor(f: Single; f1: Single): JMarkerOptions; cdecl;
    function draggable(b: Boolean): JMarkerOptions; cdecl;
    function flat(b: Boolean): JMarkerOptions; cdecl;
    function getAlpha: Single; cdecl;
    function getAnchorU: Single; cdecl;
    function getAnchorV: Single; cdecl;
    function getIcon: JBitmapDescriptor; cdecl;
    function getInfoWindowAnchorU: Single; cdecl;
    function getInfoWindowAnchorV: Single; cdecl;
    function getPosition: JLatLng; cdecl;
    function getRotation: Single; cdecl;
    function getSnippet: JString; cdecl;
    function getTitle: JString; cdecl;
    function getZIndex: Single; cdecl;
    function icon(bitmapDescriptor: JBitmapDescriptor): JMarkerOptions; cdecl;
    function infoWindowAnchor(f: Single; f1: Single): JMarkerOptions; cdecl;
    function isDraggable: Boolean; cdecl;
    function isFlat: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    function position(latLng: JLatLng): JMarkerOptions; cdecl;
    function rotation(f: Single): JMarkerOptions; cdecl;
    function snippet(string_: JString): JMarkerOptions; cdecl;
    function title(string_: JString): JMarkerOptions; cdecl;
    function visible(b: Boolean): JMarkerOptions; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    function zIndex(f: Single): JMarkerOptions; cdecl;
  end;
  TJMarkerOptions = class(TJavaGenericImport<JMarkerOptionsClass, JMarkerOptions>) end;

  JPointOfInterestClass = interface(JAbstractSafeParcelableClass)
    ['{7B83FEBC-C361-47FE-B913-45AB942D4EBB}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(latLng: JLatLng; string_: JString; string_1: JString): JPointOfInterest; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/PointOfInterest')]
  JPointOfInterest = interface(JAbstractSafeParcelable)
    ['{3EDF407A-1827-4087-B218-B9586674DDC5}']
    function _GetlatLng: JLatLng; cdecl;
    function _Getname: JString; cdecl;
    function _GetplaceId: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    property latLng: JLatLng read _GetlatLng;
    property name: JString read _Getname;
    property placeId: JString read _GetplaceId;
  end;
  TJPointOfInterest = class(TJavaGenericImport<JPointOfInterestClass, JPointOfInterest>) end;

  JPolygonClass = interface(JObjectClass)
    ['{C32F88C3-3863-4316-AB9E-99F6501F709C}']
    {class} //function init(zzaa: Jmaps_zzaa): JPolygon; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/Polygon')]
  JPolygon = interface(JObject)
    ['{BECE8208-AA23-48F1-AC07-849EBCABB32E}']
    function equals(object_: JObject): Boolean; cdecl;
    function getFillColor: Integer; cdecl;
    function getHoles: JList; cdecl;
    function getId: JString; cdecl;
    function getPoints: JList; cdecl;
    function getStrokeColor: Integer; cdecl;
    function getStrokeJointType: Integer; cdecl;
    function getStrokePattern: JList; cdecl;
    function getStrokeWidth: Single; cdecl;
    function getTag: JObject; cdecl;
    function getZIndex: Single; cdecl;
    function hashCode: Integer; cdecl;
    function isClickable: Boolean; cdecl;
    function isGeodesic: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    procedure remove; cdecl;
    procedure setClickable(b: Boolean); cdecl;
    procedure setFillColor(i: Integer); cdecl;
    procedure setGeodesic(b: Boolean); cdecl;
    procedure setHoles(list: JList); cdecl;
    procedure setPoints(list: JList); cdecl;
    procedure setStrokeColor(i: Integer); cdecl;
    procedure setStrokeJointType(i: Integer); cdecl;
    procedure setStrokePattern(list: JList); cdecl;
    procedure setStrokeWidth(f: Single); cdecl;
    procedure setTag(object_: JObject); cdecl;
    procedure setVisible(b: Boolean); cdecl;
    procedure setZIndex(f: Single); cdecl;
  end;
  TJPolygon = class(TJavaGenericImport<JPolygonClass, JPolygon>) end;

  JPolygonOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{035017D5-E83D-4EBD-9159-872BB97687F8}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JPolygonOptions; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/PolygonOptions')]
  JPolygonOptions = interface(JAbstractSafeParcelable)
    ['{D44C6C92-3D47-4E5F-8BA2-0AF69743B24E}']
    function add(latLng: JLatLng): JPolygonOptions; cdecl; overload;
    function addAll(iterable: JIterable): JPolygonOptions; cdecl;
    function addHole(iterable: JIterable): JPolygonOptions; cdecl;
    function clickable(b: Boolean): JPolygonOptions; cdecl;
    function fillColor(i: Integer): JPolygonOptions; cdecl;
    function geodesic(b: Boolean): JPolygonOptions; cdecl;
    function getFillColor: Integer; cdecl;
    function getHoles: JList; cdecl;
    function getPoints: JList; cdecl;
    function getStrokeColor: Integer; cdecl;
    function getStrokeJointType: Integer; cdecl;
    function getStrokePattern: JList; cdecl;
    function getStrokeWidth: Single; cdecl;
    function getZIndex: Single; cdecl;
    function isClickable: Boolean; cdecl;
    function isGeodesic: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    function strokeColor(i: Integer): JPolygonOptions; cdecl;
    function strokeJointType(i: Integer): JPolygonOptions; cdecl;
    function strokePattern(list: JList): JPolygonOptions; cdecl;
    function strokeWidth(f: Single): JPolygonOptions; cdecl;
    function visible(b: Boolean): JPolygonOptions; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    function zIndex(f: Single): JPolygonOptions; cdecl;
  end;
  TJPolygonOptions = class(TJavaGenericImport<JPolygonOptionsClass, JPolygonOptions>) end;

  JPolylineClass = interface(JObjectClass)
    ['{E56467A2-5381-457A-B96F-E67567F6FABB}']
    {class} //function init(zzad: Jmaps_zzad): JPolyline; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/Polyline')]
  JPolyline = interface(JObject)
    ['{803D778F-39F8-4FF2-B9E5-8EC0BD513133}']
    function equals(object_: JObject): Boolean; cdecl;
    function getColor: Integer; cdecl;
    function getEndCap: JCap; cdecl;
    function getId: JString; cdecl;
    function getJointType: Integer; cdecl;
    function getPattern: JList; cdecl;
    function getPoints: JList; cdecl;
    function getStartCap: JCap; cdecl;
    function getTag: JObject; cdecl;
    function getWidth: Single; cdecl;
    function getZIndex: Single; cdecl;
    function hashCode: Integer; cdecl;
    function isClickable: Boolean; cdecl;
    function isGeodesic: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    procedure remove; cdecl;
    procedure setClickable(b: Boolean); cdecl;
    procedure setColor(i: Integer); cdecl;
    procedure setEndCap(cap: JCap); cdecl;
    procedure setGeodesic(b: Boolean); cdecl;
    procedure setJointType(i: Integer); cdecl;
    procedure setPattern(list: JList); cdecl;
    procedure setPoints(list: JList); cdecl;
    procedure setStartCap(cap: JCap); cdecl;
    procedure setTag(object_: JObject); cdecl;
    procedure setVisible(b: Boolean); cdecl;
    procedure setWidth(f: Single); cdecl;
    procedure setZIndex(f: Single); cdecl;
  end;
  TJPolyline = class(TJavaGenericImport<JPolylineClass, JPolyline>) end;

  JPolylineOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{73CA0FF0-3ECB-493C-822E-0D9E24EDD021}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JPolylineOptions; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/PolylineOptions')]
  JPolylineOptions = interface(JAbstractSafeParcelable)
    ['{029E45C1-C7B8-4FDB-B575-B222EC3DE59F}']
    function add(latLng: JLatLng): JPolylineOptions; cdecl; overload;
    function addAll(iterable: JIterable): JPolylineOptions; cdecl;
    function clickable(b: Boolean): JPolylineOptions; cdecl;
    function color(i: Integer): JPolylineOptions; cdecl;
    function endCap(cap: JCap): JPolylineOptions; cdecl;
    function geodesic(b: Boolean): JPolylineOptions; cdecl;
    function getColor: Integer; cdecl;
    function getEndCap: JCap; cdecl;
    function getJointType: Integer; cdecl;
    function getPattern: JList; cdecl;
    function getPoints: JList; cdecl;
    function getStartCap: JCap; cdecl;
    function getWidth: Single; cdecl;
    function getZIndex: Single; cdecl;
    function isClickable: Boolean; cdecl;
    function isGeodesic: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    function jointType(i: Integer): JPolylineOptions; cdecl;
    function pattern(list: JList): JPolylineOptions; cdecl;
    function startCap(cap: JCap): JPolylineOptions; cdecl;
    function visible(b: Boolean): JPolylineOptions; cdecl;
    function width(f: Single): JPolylineOptions; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    function zIndex(f: Single): JPolylineOptions; cdecl;
  end;
  TJPolylineOptions = class(TJavaGenericImport<JPolylineOptionsClass, JPolylineOptions>) end;

  JTileClass = interface(JAbstractSafeParcelableClass)
    ['{83027546-0EC6-4279-8EE9-9C7D5916EF94}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(i: Integer; i1: Integer; b: TJavaArray<Byte>): JTile; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/Tile')]
  JTile = interface(JAbstractSafeParcelable)
    ['{B5DD3D4F-FD9D-431B-BD30-919E328BE40C}']
    function _Getdata: TJavaArray<Byte>; cdecl;
    function _Getheight: Integer; cdecl;
    function _Getwidth: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    property data: TJavaArray<Byte> read _Getdata;
    property height: Integer read _Getheight;
    property width: Integer read _Getwidth;
  end;
  TJTile = class(TJavaGenericImport<JTileClass, JTile>) end;

  JTileOverlayClass = interface(JObjectClass)
    ['{048A54CE-D00A-4604-A37D-AD23D52380E8}']
    {class} //function init(zzag: Jmaps_zzag): JTileOverlay; cdecl;
  end;

  [JavaSignature('com/google/android/gms/maps/model/TileOverlay')]
  JTileOverlay = interface(JObject)
    ['{46B2D846-DF98-4A3A-9141-280BBB683586}']
    procedure clearTileCache; cdecl;
    function equals(object_: JObject): Boolean; cdecl;
    function getFadeIn: Boolean; cdecl;
    function getId: JString; cdecl;
    function getTransparency: Single; cdecl;
    function getZIndex: Single; cdecl;
    function hashCode: Integer; cdecl;
    function isVisible: Boolean; cdecl;
    procedure remove; cdecl;
    procedure setFadeIn(b: Boolean); cdecl;
    procedure setTransparency(f: Single); cdecl;
    procedure setVisible(b: Boolean); cdecl;
    procedure setZIndex(f: Single); cdecl;
  end;
  TJTileOverlay = class(TJavaGenericImport<JTileOverlayClass, JTileOverlay>) end;

  JTileOverlayOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{C3818656-1176-498F-A0E8-106091DEAE4B}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: JTileOverlayOptions; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/TileOverlayOptions')]
  JTileOverlayOptions = interface(JAbstractSafeParcelable)
    ['{514FFD3B-E0C2-4AD5-89EE-3EB07D1D03AF}']
    function fadeIn(b: Boolean): JTileOverlayOptions; cdecl;
    function getFadeIn: Boolean; cdecl;
    function getTileProvider: JTileProvider; cdecl;
    function getTransparency: Single; cdecl;
    function getZIndex: Single; cdecl;
    function isVisible: Boolean; cdecl;
    function tileProvider(tileProvider: JTileProvider): JTileOverlayOptions; cdecl;
    function transparency(f: Single): JTileOverlayOptions; cdecl;
    function visible(b: Boolean): JTileOverlayOptions; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    function zIndex(f: Single): JTileOverlayOptions; cdecl;
  end;
  TJTileOverlayOptions = class(TJavaGenericImport<JTileOverlayOptionsClass, JTileOverlayOptions>) end;

  JTileProviderClass = interface(IJavaClass)
    ['{D6128829-67DB-49E4-AB8A-D4ED01E19A11}']
    {class} function _GetNO_TILE: JTile; cdecl;
    {class} property NO_TILE: JTile read _GetNO_TILE;
  end;

  [JavaSignature('com/google/android/gms/maps/model/TileProvider')]
  JTileProvider = interface(IJavaInstance)
    ['{C1D85B3E-623F-4F8C-8CAA-8BFBFC944C61}']
    function getTile(i: Integer; i1: Integer; i2: Integer): JTile; cdecl;
  end;
  TJTileProvider = class(TJavaGenericImport<JTileProviderClass, JTileProvider>) end;

  JVisibleRegionClass = interface(JAbstractSafeParcelableClass)
    ['{C83C8AE5-7C11-407C-A342-3DE928A8E39B}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(latLng: JLatLng; latLng1: JLatLng; latLng2: JLatLng; latLng3: JLatLng; latLngBounds: JLatLngBounds): JVisibleRegion; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/maps/model/VisibleRegion')]
  JVisibleRegion = interface(JAbstractSafeParcelable)
    ['{669FB7E8-207E-4C0A-8882-F056813AB68C}']
    function _GetfarLeft: JLatLng; cdecl;
    function _GetfarRight: JLatLng; cdecl;
    function _GetlatLngBounds: JLatLngBounds; cdecl;
    function _GetnearLeft: JLatLng; cdecl;
    function _GetnearRight: JLatLng; cdecl;
    function equals(object_: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    property farLeft: JLatLng read _GetfarLeft;
    property farRight: JLatLng read _GetfarRight;
    property latLngBounds: JLatLngBounds read _GetlatLngBounds;
    property nearLeft: JLatLng read _GetnearLeft;
    property nearRight: JLatLng read _GetnearRight;
  end;
  TJVisibleRegion = class(TJavaGenericImport<JVisibleRegionClass, JVisibleRegion>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JCameraUpdate', TypeInfo(Androidapi.JNI.PlayServices.Maps.JCameraUpdate));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JCameraUpdateFactory', TypeInfo(Androidapi.JNI.PlayServices.Maps.JCameraUpdateFactory));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_CancelableCallback', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_CancelableCallback));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_InfoWindowAdapter', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_InfoWindowAdapter));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraChangeListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraChangeListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraIdleListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraIdleListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraMoveCanceledListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraMoveCanceledListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraMoveListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraMoveListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraMoveStartedListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCameraMoveStartedListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCircleClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnCircleClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnGroundOverlayClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnGroundOverlayClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnIndoorStateChangeListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnIndoorStateChangeListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnInfoWindowClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnInfoWindowClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnInfoWindowCloseListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnInfoWindowCloseListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnInfoWindowLongClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnInfoWindowLongClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMapClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMapClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMapLoadedCallback', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMapLoadedCallback));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMapLongClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMapLongClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMarkerClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMarkerClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMarkerDragListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMarkerDragListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMyLocationButtonClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMyLocationButtonClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMyLocationChangeListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMyLocationChangeListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMyLocationClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnMyLocationClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnPoiClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnPoiClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnPolygonClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnPolygonClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnPolylineClickListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_OnPolylineClickListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMap_SnapshotReadyCallback', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMap_SnapshotReadyCallback));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGoogleMapOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGoogleMapOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JLocationSource', TypeInfo(Androidapi.JNI.PlayServices.Maps.JLocationSource));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JLocationSource_OnLocationChangedListener', TypeInfo(Androidapi.JNI.PlayServices.Maps.JLocationSource_OnLocationChangedListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JMapView', TypeInfo(Androidapi.JNI.PlayServices.Maps.JMapView));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JMapsInitializer', TypeInfo(Androidapi.JNI.PlayServices.Maps.JMapsInitializer));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JOnMapReadyCallback', TypeInfo(Androidapi.JNI.PlayServices.Maps.JOnMapReadyCallback));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JProjection', TypeInfo(Androidapi.JNI.PlayServices.Maps.JProjection));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JUiSettings', TypeInfo(Androidapi.JNI.PlayServices.Maps.JUiSettings));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JICameraUpdateFactoryDelegate', TypeInfo(Androidapi.JNI.PlayServices.Maps.JICameraUpdateFactoryDelegate));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JIGoogleMapDelegate', TypeInfo(Androidapi.JNI.PlayServices.Maps.JIGoogleMapDelegate));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JILocationSourceDelegate', TypeInfo(Androidapi.JNI.PlayServices.Maps.JILocationSourceDelegate));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JIProjectionDelegate', TypeInfo(Androidapi.JNI.PlayServices.Maps.JIProjectionDelegate));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JIUiSettingsDelegate', TypeInfo(Androidapi.JNI.PlayServices.Maps.JIUiSettingsDelegate));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzab', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzab));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzad', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzad));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzaf', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzaf));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzah', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzah));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzaj', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzaj));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzal', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzal));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzan', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzan));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzap', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzap));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzar', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzar));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzat', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzat));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzav', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzav));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzax', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzax));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzaz', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzaz));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzbb', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzbb));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzbd', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzbd));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzbf', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzbf));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzbh', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzbh));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzbu', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzbu));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzd', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzd));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzi', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzi));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzn', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzn));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzp', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzp));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzr', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jmaps_internal_zzr));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzt', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzt));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzv', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzv));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzx', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzx));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.Jinternal_zzz', TypeInfo(Androidapi.JNI.PlayServices.Maps.Jinternal_zzz));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JBitmapDescriptor', TypeInfo(Androidapi.JNI.PlayServices.Maps.JBitmapDescriptor));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JBitmapDescriptorFactory', TypeInfo(Androidapi.JNI.PlayServices.Maps.JBitmapDescriptorFactory));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JCameraPosition', TypeInfo(Androidapi.JNI.PlayServices.Maps.JCameraPosition));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JCameraPosition_Builder', TypeInfo(Androidapi.JNI.PlayServices.Maps.JCameraPosition_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JCap', TypeInfo(Androidapi.JNI.PlayServices.Maps.JCap));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JCircle', TypeInfo(Androidapi.JNI.PlayServices.Maps.JCircle));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JCircleOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JCircleOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGroundOverlay', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGroundOverlay));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JGroundOverlayOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JGroundOverlayOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JIndoorBuilding', TypeInfo(Androidapi.JNI.PlayServices.Maps.JIndoorBuilding));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JLatLng', TypeInfo(Androidapi.JNI.PlayServices.Maps.JLatLng));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JLatLngBounds', TypeInfo(Androidapi.JNI.PlayServices.Maps.JLatLngBounds));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JLatLngBounds_Builder', TypeInfo(Androidapi.JNI.PlayServices.Maps.JLatLngBounds_Builder));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JMapStyleOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JMapStyleOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JMarker', TypeInfo(Androidapi.JNI.PlayServices.Maps.JMarker));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JMarkerOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JMarkerOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JPointOfInterest', TypeInfo(Androidapi.JNI.PlayServices.Maps.JPointOfInterest));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JPolygon', TypeInfo(Androidapi.JNI.PlayServices.Maps.JPolygon));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JPolygonOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JPolygonOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JPolyline', TypeInfo(Androidapi.JNI.PlayServices.Maps.JPolyline));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JPolylineOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JPolylineOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JTile', TypeInfo(Androidapi.JNI.PlayServices.Maps.JTile));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JTileOverlay', TypeInfo(Androidapi.JNI.PlayServices.Maps.JTileOverlay));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JTileOverlayOptions', TypeInfo(Androidapi.JNI.PlayServices.Maps.JTileOverlayOptions));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JTileProvider', TypeInfo(Androidapi.JNI.PlayServices.Maps.JTileProvider));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Maps.JVisibleRegion', TypeInfo(Androidapi.JNI.PlayServices.Maps.JVisibleRegion));
end;

initialization
  RegisterTypes;
end.


