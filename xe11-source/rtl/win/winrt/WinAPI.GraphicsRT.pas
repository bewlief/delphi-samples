{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.GraphicsRT;

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
  Winapi.Networking, 
  Winapi.ServicesRT, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  DirectX_DirectXAlphaMode = Winapi.CommonTypes.DirectX_DirectXAlphaMode;
  PDirectX_DirectXAlphaMode = Winapi.CommonTypes.PDirectX_DirectXAlphaMode;
  DirectX_DirectXPixelFormat = Winapi.CommonTypes.DirectX_DirectXPixelFormat;
  PDirectX_DirectXPixelFormat = Winapi.CommonTypes.PDirectX_DirectXPixelFormat;
  DirectX_DirectXPrimitiveTopology = Winapi.CommonTypes.DirectX_DirectXPrimitiveTopology;
  PDirectX_DirectXPrimitiveTopology = Winapi.CommonTypes.PDirectX_DirectXPrimitiveTopology;
  Effects_IGraphicsEffect = Winapi.CommonTypes.Effects_IGraphicsEffect;
  PEffects_IGraphicsEffect = Winapi.CommonTypes.PEffects_IGraphicsEffect;
  IGeometrySource2D = Winapi.CommonTypes.IGeometrySource2D;
  PIGeometrySource2D = Winapi.CommonTypes.PIGeometrySource2D;
  Imaging_BitmapAlphaMode = Winapi.CommonTypes.Imaging_BitmapAlphaMode;
  PImaging_BitmapAlphaMode = Winapi.CommonTypes.PImaging_BitmapAlphaMode;
  Imaging_BitmapBufferAccessMode = Winapi.CommonTypes.Imaging_BitmapBufferAccessMode;
  PImaging_BitmapBufferAccessMode = Winapi.CommonTypes.PImaging_BitmapBufferAccessMode;
  Imaging_BitmapPixelFormat = Winapi.CommonTypes.Imaging_BitmapPixelFormat;
  PImaging_BitmapPixelFormat = Winapi.CommonTypes.PImaging_BitmapPixelFormat;
  Imaging_BitmapPlaneDescription = Winapi.CommonTypes.Imaging_BitmapPlaneDescription;
  PImaging_BitmapPlaneDescription = Winapi.CommonTypes.PImaging_BitmapPlaneDescription;
  Imaging_IBitmapBuffer = Winapi.CommonTypes.Imaging_IBitmapBuffer;
  PImaging_IBitmapBuffer = Winapi.CommonTypes.PImaging_IBitmapBuffer;
  Imaging_ISoftwareBitmap = Winapi.CommonTypes.Imaging_ISoftwareBitmap;
  PImaging_ISoftwareBitmap = Winapi.CommonTypes.PImaging_ISoftwareBitmap;
  PointInt32 = Winapi.CommonTypes.PointInt32;
  PPointInt32 = Winapi.CommonTypes.PPointInt32;
  RectInt32 = Winapi.CommonTypes.RectInt32;
  PRectInt32 = Winapi.CommonTypes.PRectInt32;
  SizeInt32 = Winapi.CommonTypes.SizeInt32;
  PSizeInt32 = Winapi.CommonTypes.PSizeInt32;

  // Forward declarations for interfaces

  // Windows.Graphics.Effects.IGraphicsEffectSource
  Effects_IGraphicsEffectSource = interface;
  PEffects_IGraphicsEffectSource = ^Effects_IGraphicsEffectSource;

  // Windows.Graphics.Printing.IPrintTaskOptionsCore
  Printing_IPrintTaskOptionsCore = interface;
  PPrinting_IPrintTaskOptionsCore = ^Printing_IPrintTaskOptionsCore;

  // Windows.Graphics.Printing.IPrintDocumentSource
  Printing_IPrintDocumentSource = interface;
  PPrinting_IPrintDocumentSource = ^Printing_IPrintDocumentSource;

  // Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface
  DirectX_Direct3D11_IDirect3DSurface = interface;
  PDirectX_Direct3D11_IDirect3DSurface = ^DirectX_Direct3D11_IDirect3DSurface;

  // Windows.Graphics.DirectX.Direct3D11.IDirect3DDevice
  DirectX_Direct3D11_IDirect3DDevice = interface;
  PDirectX_Direct3D11_IDirect3DDevice = ^DirectX_Direct3D11_IDirect3DDevice;

  // Windows.Foundation.IReference`1<Windows.Graphics.SizeInt32>
  IReference_1__SizeInt32 = interface;
  PIReference_1__SizeInt32 = ^IReference_1__SizeInt32;

  // Windows.Graphics.Capture.IDirect3D11CaptureFrame
  Capture_IDirect3D11CaptureFrame = interface;
  PCapture_IDirect3D11CaptureFrame = ^Capture_IDirect3D11CaptureFrame;

  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Capture.IDirect3D11CaptureFramePool,Object>
  TypedEventHandler_2__Capture_IDirect3D11CaptureFramePool__IInspectable = interface;
  PTypedEventHandler_2__Capture_IDirect3D11CaptureFramePool__IInspectable = ^TypedEventHandler_2__Capture_IDirect3D11CaptureFramePool__IInspectable;

  // Windows.Graphics.Capture.IGraphicsCaptureSession
  Capture_IGraphicsCaptureSession = interface;
  PCapture_IGraphicsCaptureSession = ^Capture_IGraphicsCaptureSession;

  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Capture.IGraphicsCaptureItem,Object>
  TypedEventHandler_2__Capture_IGraphicsCaptureItem__IInspectable = interface;
  PTypedEventHandler_2__Capture_IGraphicsCaptureItem__IInspectable = ^TypedEventHandler_2__Capture_IGraphicsCaptureItem__IInspectable;

  // Windows.Graphics.Capture.IGraphicsCaptureItem
  Capture_IGraphicsCaptureItem = interface;
  PCapture_IGraphicsCaptureItem = ^Capture_IGraphicsCaptureItem;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueueTimer,Object>
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueueTimer__IInspectable = ^TypedEventHandler_2__IDispatcherQueueTimer__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Object>
  TypedEventHandler_2__IDispatcherQueue__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueue__IInspectable = ^TypedEventHandler_2__IDispatcherQueue__IInspectable;

  // Windows.Graphics.Capture.IDirect3D11CaptureFramePool
  Capture_IDirect3D11CaptureFramePool = interface;
  PCapture_IDirect3D11CaptureFramePool = ^Capture_IDirect3D11CaptureFramePool;

  // Windows.Graphics.Capture.IDirect3D11CaptureFramePoolStatics
  Capture_IDirect3D11CaptureFramePoolStatics = interface;
  PCapture_IDirect3D11CaptureFramePoolStatics = ^Capture_IDirect3D11CaptureFramePoolStatics;

  // Windows.Graphics.Capture.IDirect3D11CaptureFramePoolStatics2
  Capture_IDirect3D11CaptureFramePoolStatics2 = interface;
  PCapture_IDirect3D11CaptureFramePoolStatics2 = ^Capture_IDirect3D11CaptureFramePoolStatics2;

  // Windows.Graphics.Capture.IGraphicsCaptureItemStatics
  Capture_IGraphicsCaptureItemStatics = interface;
  PCapture_IGraphicsCaptureItemStatics = ^Capture_IGraphicsCaptureItemStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Capture.IGraphicsCaptureItem>
  AsyncOperationCompletedHandler_1__Capture_IGraphicsCaptureItem = interface;
  PAsyncOperationCompletedHandler_1__Capture_IGraphicsCaptureItem = ^AsyncOperationCompletedHandler_1__Capture_IGraphicsCaptureItem;

  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Capture.IGraphicsCaptureItem>
  IAsyncOperation_1__Capture_IGraphicsCaptureItem = interface;
  PIAsyncOperation_1__Capture_IGraphicsCaptureItem = ^IAsyncOperation_1__Capture_IGraphicsCaptureItem;

  // Windows.Graphics.Capture.IGraphicsCapturePicker
  Capture_IGraphicsCapturePicker = interface;
  PCapture_IGraphicsCapturePicker = ^Capture_IGraphicsCapturePicker;

  // Windows.Graphics.Capture.IGraphicsCaptureSession2
  Capture_IGraphicsCaptureSession2 = interface;
  PCapture_IGraphicsCaptureSession2 = ^Capture_IGraphicsCaptureSession2;

  // Windows.Graphics.Capture.IGraphicsCaptureSessionStatics
  Capture_IGraphicsCaptureSessionStatics = interface;
  PCapture_IGraphicsCaptureSessionStatics = ^Capture_IGraphicsCaptureSessionStatics;

  // Windows.Graphics.Display.Core.IHdmiDisplayMode
  Display_Core_IHdmiDisplayMode = interface;
  PDisplay_Core_IHdmiDisplayMode = ^Display_Core_IHdmiDisplayMode;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IIterator_1__Display_Core_IHdmiDisplayMode = interface;
  PIIterator_1__Display_Core_IHdmiDisplayMode = ^IIterator_1__Display_Core_IHdmiDisplayMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IIterable_1__Display_Core_IHdmiDisplayMode = interface;
  PIIterable_1__Display_Core_IHdmiDisplayMode = ^IIterable_1__Display_Core_IHdmiDisplayMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IVectorView_1__Display_Core_IHdmiDisplayMode = interface;
  PIVectorView_1__Display_Core_IHdmiDisplayMode = ^IVectorView_1__Display_Core_IHdmiDisplayMode;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Boolean>
  AsyncOperationCompletedHandler_1__Boolean = interface;
  PAsyncOperationCompletedHandler_1__Boolean = ^AsyncOperationCompletedHandler_1__Boolean;

  // Windows.Foundation.IAsyncOperation`1<Boolean>
  IAsyncOperation_1__Boolean = interface;
  PIAsyncOperation_1__Boolean = ^IAsyncOperation_1__Boolean;

  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Display.Core.IHdmiDisplayInformation,Object>
  TypedEventHandler_2__Display_Core_IHdmiDisplayInformation__IInspectable = interface;
  PTypedEventHandler_2__Display_Core_IHdmiDisplayInformation__IInspectable = ^TypedEventHandler_2__Display_Core_IHdmiDisplayInformation__IInspectable;

  // Windows.Graphics.Display.Core.IHdmiDisplayInformation
  Display_Core_IHdmiDisplayInformation = interface;
  PDisplay_Core_IHdmiDisplayInformation = ^Display_Core_IHdmiDisplayInformation;

  // Windows.Graphics.Display.Core.IHdmiDisplayInformationStatics
  Display_Core_IHdmiDisplayInformationStatics = interface;
  PDisplay_Core_IHdmiDisplayInformationStatics = ^Display_Core_IHdmiDisplayInformationStatics;

  // Windows.Graphics.Display.Core.IHdmiDisplayMode2
  Display_Core_IHdmiDisplayMode2 = interface;
  PDisplay_Core_IHdmiDisplayMode2 = ^Display_Core_IHdmiDisplayMode2;

  // Windows.Graphics.Display.DisplayPropertiesEventHandler
  Display_DisplayPropertiesEventHandler = interface;
  PDisplay_DisplayPropertiesEventHandler = ^Display_DisplayPropertiesEventHandler;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Display.NitRange>
  IIterator_1__Display_NitRange = interface;
  PIIterator_1__Display_NitRange = ^IIterator_1__Display_NitRange;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Display.NitRange>
  IIterable_1__Display_NitRange = interface;
  PIIterable_1__Display_NitRange = ^IIterable_1__Display_NitRange;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Display.NitRange>
  IVectorView_1__Display_NitRange = interface;
  PIVectorView_1__Display_NitRange = ^IVectorView_1__Display_NitRange;

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

  // Windows.Foundation.IReference`1<Double>
  IReference_1__Double = interface;
  PIReference_1__Double = ^IReference_1__Double;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IIterator_1__DirectX_DirectXPixelFormat = interface;
  PIIterator_1__DirectX_DirectXPixelFormat = ^IIterator_1__DirectX_DirectXPixelFormat;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IIterable_1__DirectX_DirectXPixelFormat = interface;
  PIIterable_1__DirectX_DirectXPixelFormat = ^IIterable_1__DirectX_DirectXPixelFormat;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IVectorView_1__DirectX_DirectXPixelFormat = interface;
  PIVectorView_1__DirectX_DirectXPixelFormat = ^IVectorView_1__DirectX_DirectXPixelFormat;

  // Windows.Foundation.IReference`1<Windows.Graphics.Holographic.HolographicStereoTransform>
  IReference_1__Holographic_HolographicStereoTransform = interface;
  PIReference_1__Holographic_HolographicStereoTransform = ^IReference_1__Holographic_HolographicStereoTransform;

  // Windows.Graphics.Holographic.IHolographicQuadLayer
  Holographic_IHolographicQuadLayer = interface;
  PHolographic_IHolographicQuadLayer = ^Holographic_IHolographicQuadLayer;

  // Windows.Graphics.Holographic.IHolographicQuadLayerFactory
  Holographic_IHolographicQuadLayerFactory = interface;
  PHolographic_IHolographicQuadLayerFactory = ^Holographic_IHolographicQuadLayerFactory;

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

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface;
  PEventHandler_1__IInspectable = ^EventHandler_1__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface;
  PTypedEventHandler_2__IMemoryBufferReference__IInspectable = ^TypedEventHandler_2__IMemoryBufferReference__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface;
  PIIterator_1__HSTRING = ^IIterator_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface;
  PIIterable_1__HSTRING = ^IIterable_1__HSTRING;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface;
  PIVectorView_1__HSTRING = ^IVectorView_1__HSTRING;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap = interface;
  PAsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap = ^AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap;

  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IAsyncOperation_1__Imaging_ISoftwareBitmap = interface;
  PIAsyncOperation_1__Imaging_ISoftwareBitmap = ^IAsyncOperation_1__Imaging_ISoftwareBitmap;

  // Windows.Graphics.Imaging.ISoftwareBitmapFactory
  Imaging_ISoftwareBitmapFactory = interface;
  PImaging_ISoftwareBitmapFactory = ^Imaging_ISoftwareBitmapFactory;

  // Windows.Graphics.Imaging.ISoftwareBitmapStatics
  Imaging_ISoftwareBitmapStatics = interface;
  PImaging_ISoftwareBitmapStatics = ^Imaging_ISoftwareBitmapStatics;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // Windows.Graphics.Printing.IPrintPageInfo
  Printing_IPrintPageInfo = interface;
  PPrinting_IPrintPageInfo = ^Printing_IPrintPageInfo;

  // Windows.Graphics.Printing.IPrintPageRange
  Printing_IPrintPageRange = interface;
  PPrinting_IPrintPageRange = ^Printing_IPrintPageRange;

  // Windows.Graphics.Printing.IPrintPageRangeFactory
  Printing_IPrintPageRangeFactory = interface;
  PPrinting_IPrintPageRangeFactory = ^Printing_IPrintPageRangeFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing.IPrintPageRange>
  IIterator_1__Printing_IPrintPageRange = interface;
  PIIterator_1__Printing_IPrintPageRange = ^IIterator_1__Printing_IPrintPageRange;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing.IPrintPageRange>
  IIterable_1__Printing_IPrintPageRange = interface;
  PIIterable_1__Printing_IPrintPageRange = ^IIterable_1__Printing_IPrintPageRange;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing.IPrintPageRange>
  IVectorView_1__Printing_IPrintPageRange = interface;
  PIVectorView_1__Printing_IPrintPageRange = ^IVectorView_1__Printing_IPrintPageRange;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing.IPrintPageRange>
  IVector_1__Printing_IPrintPageRange = interface;
  PIVector_1__Printing_IPrintPageRange = ^IVector_1__Printing_IPrintPageRange;

  // Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic
  Printing_IStandardPrintTaskOptionsStatic = interface;
  PPrinting_IStandardPrintTaskOptionsStatic = ^Printing_IStandardPrintTaskOptionsStatic;

  // Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic2
  Printing_IStandardPrintTaskOptionsStatic2 = interface;
  PPrinting_IStandardPrintTaskOptionsStatic2 = ^Printing_IStandardPrintTaskOptionsStatic2;

  // Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic3
  Printing_IStandardPrintTaskOptionsStatic3 = interface;
  PPrinting_IStandardPrintTaskOptionsStatic3 = ^Printing_IStandardPrintTaskOptionsStatic3;

  // Windows.Foundation.Collections.IIterator`1<Object>
  IIterator_1__IInspectable = interface;
  PIIterator_1__IInspectable = ^IIterator_1__IInspectable;

  // Windows.Foundation.Collections.IIterable`1<Object>
  IIterable_1__IInspectable = interface;
  PIIterable_1__IInspectable = ^IIterable_1__IInspectable;

  // Windows.Foundation.Collections.IVectorView`1<Object>
  IVectorView_1__IInspectable = interface;
  PIVectorView_1__IInspectable = ^IVectorView_1__IInspectable;

  // Windows.Graphics.Printing.OptionDetails.IPrintTextOptionDetails
  Printing_OptionDetails_IPrintTextOptionDetails = interface;
  PPrinting_OptionDetails_IPrintTextOptionDetails = ^Printing_OptionDetails_IPrintTextOptionDetails;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface;
  PTypedEventHandler_2__Activation_ISplashScreen__IInspectable = ^TypedEventHandler_2__Activation_ISplashScreen__IInspectable;

  // Windows.Graphics.Printing3D.IPrinting3DTextureResource
  Printing3D_IPrinting3DTextureResource = interface;
  PPrinting3D_IPrinting3DTextureResource = ^Printing3D_IPrinting3DTextureResource;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IIterator_1__Printing3D_IPrinting3DTextureResource = interface;
  PIIterator_1__Printing3D_IPrinting3DTextureResource = ^IIterator_1__Printing3D_IPrinting3DTextureResource;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IIterable_1__Printing3D_IPrinting3DTextureResource = interface;
  PIIterable_1__Printing3D_IPrinting3DTextureResource = ^IIterable_1__Printing3D_IPrinting3DTextureResource;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IVectorView_1__Printing3D_IPrinting3DTextureResource = interface;
  PIVectorView_1__Printing3D_IPrinting3DTextureResource = ^IVectorView_1__Printing3D_IPrinting3DTextureResource;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IVector_1__Printing3D_IPrinting3DTextureResource = interface;
  PIVector_1__Printing3D_IPrinting3DTextureResource = ^IVector_1__Printing3D_IPrinting3DTextureResource;

  // Windows.Graphics.Printing3D.IPrinting3DModelTexture
  Printing3D_IPrinting3DModelTexture = interface;
  PPrinting3D_IPrinting3DModelTexture = ^Printing3D_IPrinting3DModelTexture;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IIterator_1__Printing3D_IPrinting3DModelTexture = interface;
  PIIterator_1__Printing3D_IPrinting3DModelTexture = ^IIterator_1__Printing3D_IPrinting3DModelTexture;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IIterable_1__Printing3D_IPrinting3DModelTexture = interface;
  PIIterable_1__Printing3D_IPrinting3DModelTexture = ^IIterable_1__Printing3D_IPrinting3DModelTexture;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IVectorView_1__Printing3D_IPrinting3DModelTexture = interface;
  PIVectorView_1__Printing3D_IPrinting3DModelTexture = ^IVectorView_1__Printing3D_IPrinting3DModelTexture;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IVector_1__Printing3D_IPrinting3DModelTexture = interface;
  PIVector_1__Printing3D_IPrinting3DModelTexture = ^IVector_1__Printing3D_IPrinting3DModelTexture;

  // Windows.Foundation.Collections.IIterator`1<UInt32>
  IIterator_1__Cardinal = interface;
  PIIterator_1__Cardinal = ^IIterator_1__Cardinal;

  // Windows.Foundation.Collections.IIterable`1<UInt32>
  IIterable_1__Cardinal = interface;
  PIIterable_1__Cardinal = ^IIterable_1__Cardinal;

  // Windows.Foundation.Collections.IVectorView`1<UInt32>
  IVectorView_1__Cardinal = interface;
  PIVectorView_1__Cardinal = ^IVectorView_1__Cardinal;

  // Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult
  Printing3D_IPrinting3DMeshVerificationResult = interface;
  PPrinting3D_IPrinting3DMeshVerificationResult = ^Printing3D_IPrinting3DMeshVerificationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult = interface;
  PAsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult = ^AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult>
  IAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult = interface;
  PIAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult = ^IAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult;

  // Windows.Graphics.Printing3D.IPrinting3DMesh
  Printing3D_IPrinting3DMesh = interface;
  PPrinting3D_IPrinting3DMesh = ^Printing3D_IPrinting3DMesh;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IIterator_1__Printing3D_IPrinting3DMesh = interface;
  PIIterator_1__Printing3D_IPrinting3DMesh = ^IIterator_1__Printing3D_IPrinting3DMesh;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IIterable_1__Printing3D_IPrinting3DMesh = interface;
  PIIterable_1__Printing3D_IPrinting3DMesh = ^IIterable_1__Printing3D_IPrinting3DMesh;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IVectorView_1__Printing3D_IPrinting3DMesh = interface;
  PIVectorView_1__Printing3D_IPrinting3DMesh = ^IVectorView_1__Printing3D_IPrinting3DMesh;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IVector_1__Printing3D_IPrinting3DMesh = interface;
  PIVector_1__Printing3D_IPrinting3DMesh = ^IVector_1__Printing3D_IPrinting3DMesh;

  // Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix
  Printing3D_IPrinting3DComponentWithMatrix = interface;
  PPrinting3D_IPrinting3DComponentWithMatrix = ^Printing3D_IPrinting3DComponentWithMatrix;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IIterator_1__Printing3D_IPrinting3DComponentWithMatrix = interface;
  PIIterator_1__Printing3D_IPrinting3DComponentWithMatrix = ^IIterator_1__Printing3D_IPrinting3DComponentWithMatrix;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IIterable_1__Printing3D_IPrinting3DComponentWithMatrix = interface;
  PIIterable_1__Printing3D_IPrinting3DComponentWithMatrix = ^IIterable_1__Printing3D_IPrinting3DComponentWithMatrix;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IVectorView_1__Printing3D_IPrinting3DComponentWithMatrix = interface;
  PIVectorView_1__Printing3D_IPrinting3DComponentWithMatrix = ^IVectorView_1__Printing3D_IPrinting3DComponentWithMatrix;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IVector_1__Printing3D_IPrinting3DComponentWithMatrix = interface;
  PIVector_1__Printing3D_IPrinting3DComponentWithMatrix = ^IVector_1__Printing3D_IPrinting3DComponentWithMatrix;

  // Windows.Graphics.Printing3D.IPrinting3DComponent
  Printing3D_IPrinting3DComponent = interface;
  PPrinting3D_IPrinting3DComponent = ^Printing3D_IPrinting3DComponent;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IIterator_1__Printing3D_IPrinting3DComponent = interface;
  PIIterator_1__Printing3D_IPrinting3DComponent = ^IIterator_1__Printing3D_IPrinting3DComponent;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IIterable_1__Printing3D_IPrinting3DComponent = interface;
  PIIterable_1__Printing3D_IPrinting3DComponent = ^IIterable_1__Printing3D_IPrinting3DComponent;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IVectorView_1__Printing3D_IPrinting3DComponent = interface;
  PIVectorView_1__Printing3D_IPrinting3DComponent = ^IVectorView_1__Printing3D_IPrinting3DComponent;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IVector_1__Printing3D_IPrinting3DComponent = interface;
  PIVector_1__Printing3D_IPrinting3DComponent = ^IVector_1__Printing3D_IPrinting3DComponent;

  // Windows.Graphics.Printing3D.IPrinting3DColorMaterial
  Printing3D_IPrinting3DColorMaterial = interface;
  PPrinting3D_IPrinting3DColorMaterial = ^Printing3D_IPrinting3DColorMaterial;

  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterial
  Printing3D_IPrinting3DBaseMaterial = interface;
  PPrinting3D_IPrinting3DBaseMaterial = ^Printing3D_IPrinting3DBaseMaterial;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IIterator_1__Printing3D_IPrinting3DBaseMaterial = interface;
  PIIterator_1__Printing3D_IPrinting3DBaseMaterial = ^IIterator_1__Printing3D_IPrinting3DBaseMaterial;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IIterable_1__Printing3D_IPrinting3DBaseMaterial = interface;
  PIIterable_1__Printing3D_IPrinting3DBaseMaterial = ^IIterable_1__Printing3D_IPrinting3DBaseMaterial;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IVectorView_1__Printing3D_IPrinting3DBaseMaterial = interface;
  PIVectorView_1__Printing3D_IPrinting3DBaseMaterial = ^IVectorView_1__Printing3D_IPrinting3DBaseMaterial;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IVector_1__Printing3D_IPrinting3DBaseMaterial = interface;
  PIVector_1__Printing3D_IPrinting3DBaseMaterial = ^IVector_1__Printing3D_IPrinting3DBaseMaterial;

  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup
  Printing3D_IPrinting3DBaseMaterialGroup = interface;
  PPrinting3D_IPrinting3DBaseMaterialGroup = ^Printing3D_IPrinting3DBaseMaterialGroup;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DBaseMaterialGroup = interface;
  PIIterator_1__Printing3D_IPrinting3DBaseMaterialGroup = ^IIterator_1__Printing3D_IPrinting3DBaseMaterialGroup;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DBaseMaterialGroup = interface;
  PIIterable_1__Printing3D_IPrinting3DBaseMaterialGroup = ^IIterable_1__Printing3D_IPrinting3DBaseMaterialGroup;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DBaseMaterialGroup = interface;
  PIVectorView_1__Printing3D_IPrinting3DBaseMaterialGroup = ^IVectorView_1__Printing3D_IPrinting3DBaseMaterialGroup;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IVector_1__Printing3D_IPrinting3DBaseMaterialGroup = interface;
  PIVector_1__Printing3D_IPrinting3DBaseMaterialGroup = ^IVector_1__Printing3D_IPrinting3DBaseMaterialGroup;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IIterator_1__Printing3D_IPrinting3DColorMaterial = interface;
  PIIterator_1__Printing3D_IPrinting3DColorMaterial = ^IIterator_1__Printing3D_IPrinting3DColorMaterial;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IIterable_1__Printing3D_IPrinting3DColorMaterial = interface;
  PIIterable_1__Printing3D_IPrinting3DColorMaterial = ^IIterable_1__Printing3D_IPrinting3DColorMaterial;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IVectorView_1__Printing3D_IPrinting3DColorMaterial = interface;
  PIVectorView_1__Printing3D_IPrinting3DColorMaterial = ^IVectorView_1__Printing3D_IPrinting3DColorMaterial;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IVector_1__Printing3D_IPrinting3DColorMaterial = interface;
  PIVector_1__Printing3D_IPrinting3DColorMaterial = ^IVector_1__Printing3D_IPrinting3DColorMaterial;

  // Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup
  Printing3D_IPrinting3DColorMaterialGroup = interface;
  PPrinting3D_IPrinting3DColorMaterialGroup = ^Printing3D_IPrinting3DColorMaterialGroup;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DColorMaterialGroup = interface;
  PIIterator_1__Printing3D_IPrinting3DColorMaterialGroup = ^IIterator_1__Printing3D_IPrinting3DColorMaterialGroup;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DColorMaterialGroup = interface;
  PIIterable_1__Printing3D_IPrinting3DColorMaterialGroup = ^IIterable_1__Printing3D_IPrinting3DColorMaterialGroup;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DColorMaterialGroup = interface;
  PIVectorView_1__Printing3D_IPrinting3DColorMaterialGroup = ^IVectorView_1__Printing3D_IPrinting3DColorMaterialGroup;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IVector_1__Printing3D_IPrinting3DColorMaterialGroup = interface;
  PIVector_1__Printing3D_IPrinting3DColorMaterialGroup = ^IVector_1__Printing3D_IPrinting3DColorMaterialGroup;

  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial
  Printing3D_IPrinting3DTexture2CoordMaterial = interface;
  PPrinting3D_IPrinting3DTexture2CoordMaterial = ^Printing3D_IPrinting3DTexture2CoordMaterial;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface;
  PIIterator_1__Printing3D_IPrinting3DTexture2CoordMaterial = ^IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterial;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface;
  PIIterable_1__Printing3D_IPrinting3DTexture2CoordMaterial = ^IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterial;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface;
  PIVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterial = ^IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterial;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IVector_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface;
  PIVector_1__Printing3D_IPrinting3DTexture2CoordMaterial = ^IVector_1__Printing3D_IPrinting3DTexture2CoordMaterial;

  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup
  Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface;
  PPrinting3D_IPrinting3DTexture2CoordMaterialGroup = ^Printing3D_IPrinting3DTexture2CoordMaterialGroup;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface;
  PIIterator_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = ^IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface;
  PIIterable_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = ^IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface;
  PIVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = ^IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface;
  PIVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = ^IVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup;

  // Windows.Foundation.Collections.IIterator`1<Double>
  IIterator_1__Double = interface;
  PIIterator_1__Double = ^IIterator_1__Double;

  // Windows.Foundation.Collections.IIterable`1<Double>
  IIterable_1__Double = interface;
  PIIterable_1__Double = ^IIterable_1__Double;

  // Windows.Foundation.Collections.IVectorView`1<Double>
  IVectorView_1__Double = interface;
  PIVectorView_1__Double = ^IVectorView_1__Double;

  // Windows.Foundation.Collections.IVector`1<Double>
  IVector_1__Double = interface;
  PIVector_1__Double = ^IVector_1__Double;

  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial
  Printing3D_IPrinting3DCompositeMaterial = interface;
  PPrinting3D_IPrinting3DCompositeMaterial = ^Printing3D_IPrinting3DCompositeMaterial;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IIterator_1__Printing3D_IPrinting3DCompositeMaterial = interface;
  PIIterator_1__Printing3D_IPrinting3DCompositeMaterial = ^IIterator_1__Printing3D_IPrinting3DCompositeMaterial;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IIterable_1__Printing3D_IPrinting3DCompositeMaterial = interface;
  PIIterable_1__Printing3D_IPrinting3DCompositeMaterial = ^IIterable_1__Printing3D_IPrinting3DCompositeMaterial;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IVectorView_1__Printing3D_IPrinting3DCompositeMaterial = interface;
  PIVectorView_1__Printing3D_IPrinting3DCompositeMaterial = ^IVectorView_1__Printing3D_IPrinting3DCompositeMaterial;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IVector_1__Printing3D_IPrinting3DCompositeMaterial = interface;
  PIVector_1__Printing3D_IPrinting3DCompositeMaterial = ^IVector_1__Printing3D_IPrinting3DCompositeMaterial;

  // Windows.Foundation.Collections.IVector`1<UInt32>
  IVector_1__Cardinal = interface;
  PIVector_1__Cardinal = ^IVector_1__Cardinal;

  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup
  Printing3D_IPrinting3DCompositeMaterialGroup = interface;
  PPrinting3D_IPrinting3DCompositeMaterialGroup = ^Printing3D_IPrinting3DCompositeMaterialGroup;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface;
  PIIterator_1__Printing3D_IPrinting3DCompositeMaterialGroup = ^IIterator_1__Printing3D_IPrinting3DCompositeMaterialGroup;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface;
  PIIterable_1__Printing3D_IPrinting3DCompositeMaterialGroup = ^IIterable_1__Printing3D_IPrinting3DCompositeMaterialGroup;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface;
  PIVectorView_1__Printing3D_IPrinting3DCompositeMaterialGroup = ^IVectorView_1__Printing3D_IPrinting3DCompositeMaterialGroup;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IVector_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface;
  PIVector_1__Printing3D_IPrinting3DCompositeMaterialGroup = ^IVector_1__Printing3D_IPrinting3DCompositeMaterialGroup;

  // Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial
  Printing3D_IPrinting3DMultiplePropertyMaterial = interface;
  PPrinting3D_IPrinting3DMultiplePropertyMaterial = ^Printing3D_IPrinting3DMultiplePropertyMaterial;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface;
  PIIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterial = ^IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterial;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface;
  PIIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterial = ^IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterial;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface;
  PIVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterial = ^IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterial;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface;
  PIVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial = ^IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial;

  // Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup
  Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface;
  PPrinting3D_IPrinting3DMultiplePropertyMaterialGroup = ^Printing3D_IPrinting3DMultiplePropertyMaterialGroup;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface;
  PIIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = ^IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface;
  PIIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = ^IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface;
  PIVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = ^IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface;
  PIVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = ^IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup;

  // Windows.Graphics.Printing3D.IPrinting3DMaterial
  Printing3D_IPrinting3DMaterial = interface;
  PPrinting3D_IPrinting3DMaterial = ^Printing3D_IPrinting3DMaterial;

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

  // Windows.Graphics.Printing3D.IPrinting3DModel
  Printing3D_IPrinting3DModel = interface;
  PPrinting3D_IPrinting3DModel = ^Printing3D_IPrinting3DModel;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3DModel>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel = interface;
  PAsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel = ^AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel;

  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3DModel>
  IAsyncOperation_1__Printing3D_IPrinting3DModel = interface;
  PIAsyncOperation_1__Printing3D_IPrinting3DModel = ^IAsyncOperation_1__Printing3D_IPrinting3DModel;

  // Windows.Graphics.Printing3D.IPrinting3D3MFPackage
  Printing3D_IPrinting3D3MFPackage = interface;
  PPrinting3D_IPrinting3D3MFPackage = ^Printing3D_IPrinting3D3MFPackage;

  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Object>
  TypedEventHandler_2__Printing3D_IPrint3DTask__IInspectable = interface;
  PTypedEventHandler_2__Printing3D_IPrint3DTask__IInspectable = ^TypedEventHandler_2__Printing3D_IPrint3DTask__IInspectable;

  // Windows.Graphics.Printing3D.IPrint3DTaskCompletedEventArgs
  Printing3D_IPrint3DTaskCompletedEventArgs = interface;
  PPrinting3D_IPrint3DTaskCompletedEventArgs = ^Printing3D_IPrint3DTaskCompletedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Windows.Graphics.Printing3D.IPrint3DTaskCompletedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskCompletedEventArgs = interface;
  PTypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskCompletedEventArgs = ^TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskCompletedEventArgs;

  // Windows.Graphics.Printing3D.IPrint3DTaskSourceChangedEventArgs
  Printing3D_IPrint3DTaskSourceChangedEventArgs = interface;
  PPrinting3D_IPrint3DTaskSourceChangedEventArgs = ^Printing3D_IPrint3DTaskSourceChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Windows.Graphics.Printing3D.IPrint3DTaskSourceChangedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskSourceChangedEventArgs = interface;
  PTypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskSourceChangedEventArgs = ^TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskSourceChangedEventArgs;

  // Windows.Graphics.Printing3D.IPrint3DTask
  Printing3D_IPrint3DTask = interface;
  PPrinting3D_IPrint3DTask = ^Printing3D_IPrint3DTask;

  // Windows.Graphics.Printing3D.IPrint3DTaskSourceRequestedArgs
  Printing3D_IPrint3DTaskSourceRequestedArgs = interface;
  PPrinting3D_IPrint3DTaskSourceRequestedArgs = ^Printing3D_IPrint3DTaskSourceRequestedArgs;

  // Windows.Graphics.Printing3D.Print3DTaskSourceRequestedHandler
  Printing3D_Print3DTaskSourceRequestedHandler = interface;
  PPrinting3D_Print3DTaskSourceRequestedHandler = ^Printing3D_Print3DTaskSourceRequestedHandler;

  // Windows.Graphics.Printing3D.IPrint3DTaskRequest
  Printing3D_IPrint3DTaskRequest = interface;
  PPrinting3D_IPrint3DTaskRequest = ^Printing3D_IPrint3DTaskRequest;

  // Windows.Graphics.Printing3D.IPrint3DTaskRequestedEventArgs
  Printing3D_IPrint3DTaskRequestedEventArgs = interface;
  PPrinting3D_IPrint3DTaskRequestedEventArgs = ^Printing3D_IPrint3DTaskRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DManager,Windows.Graphics.Printing3D.IPrint3DTaskRequestedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DManager__Printing3D_IPrint3DTaskRequestedEventArgs = interface;
  PTypedEventHandler_2__Printing3D_IPrint3DManager__Printing3D_IPrint3DTaskRequestedEventArgs = ^TypedEventHandler_2__Printing3D_IPrint3DManager__Printing3D_IPrint3DTaskRequestedEventArgs;

  // Windows.Graphics.Printing3D.IPrint3DManager
  Printing3D_IPrint3DManager = interface;
  PPrinting3D_IPrint3DManager = ^Printing3D_IPrint3DManager;

  // Windows.Graphics.Printing3D.IPrint3DManagerStatics
  Printing3D_IPrint3DManagerStatics = interface;
  PPrinting3D_IPrint3DManagerStatics = ^Printing3D_IPrint3DManagerStatics;

  // Windows.Graphics.Printing3D.IPrinting3D3MFPackage2
  Printing3D_IPrinting3D3MFPackage2 = interface;
  PPrinting3D_IPrinting3D3MFPackage2 = ^Printing3D_IPrinting3D3MFPackage2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3D3MFPackage>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage = interface;
  PAsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage = ^AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage;

  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3D3MFPackage>
  IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage = interface;
  PIAsyncOperation_1__Printing3D_IPrinting3D3MFPackage = ^IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage;

  // Windows.Graphics.Printing3D.IPrinting3D3MFPackageStatics
  Printing3D_IPrinting3D3MFPackageStatics = interface;
  PPrinting3D_IPrinting3D3MFPackageStatics = ^Printing3D_IPrinting3D3MFPackageStatics;

  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroupFactory
  Printing3D_IPrinting3DBaseMaterialGroupFactory = interface;
  PPrinting3D_IPrinting3DBaseMaterialGroupFactory = ^Printing3D_IPrinting3DBaseMaterialGroupFactory;

  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterialStatics
  Printing3D_IPrinting3DBaseMaterialStatics = interface;
  PPrinting3D_IPrinting3DBaseMaterialStatics = ^Printing3D_IPrinting3DBaseMaterialStatics;

  // Windows.Graphics.Printing3D.IPrinting3DColorMaterial2
  Printing3D_IPrinting3DColorMaterial2 = interface;
  PPrinting3D_IPrinting3DColorMaterial2 = ^Printing3D_IPrinting3DColorMaterial2;

  // Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroupFactory
  Printing3D_IPrinting3DColorMaterialGroupFactory = interface;
  PPrinting3D_IPrinting3DColorMaterialGroupFactory = ^Printing3D_IPrinting3DColorMaterialGroupFactory;

  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup2
  Printing3D_IPrinting3DCompositeMaterialGroup2 = interface;
  PPrinting3D_IPrinting3DCompositeMaterialGroup2 = ^Printing3D_IPrinting3DCompositeMaterialGroup2;

  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroupFactory
  Printing3D_IPrinting3DCompositeMaterialGroupFactory = interface;
  PPrinting3D_IPrinting3DCompositeMaterialGroupFactory = ^Printing3D_IPrinting3DCompositeMaterialGroupFactory;

  // Windows.Graphics.Printing3D.IPrinting3DFaceReductionOptions
  Printing3D_IPrinting3DFaceReductionOptions = interface;
  PPrinting3D_IPrinting3DFaceReductionOptions = ^Printing3D_IPrinting3DFaceReductionOptions;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Boolean,Double>
  AsyncOperationProgressHandler_2__Boolean__Double = interface;
  PAsyncOperationProgressHandler_2__Boolean__Double = ^AsyncOperationProgressHandler_2__Boolean__Double;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Boolean,Double>
  AsyncOperationWithProgressCompletedHandler_2__Boolean__Double = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Boolean__Double = ^AsyncOperationWithProgressCompletedHandler_2__Boolean__Double;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Boolean,Double>
  IAsyncOperationWithProgress_2__Boolean__Double = interface;
  PIAsyncOperationWithProgress_2__Boolean__Double = ^IAsyncOperationWithProgress_2__Boolean__Double;

  // Windows.Graphics.Printing3D.IPrinting3DModel2
  Printing3D_IPrinting3DModel2 = interface;
  PPrinting3D_IPrinting3DModel2 = ^Printing3D_IPrinting3DModel2;

  // Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroupFactory
  Printing3D_IPrinting3DMultiplePropertyMaterialGroupFactory = interface;
  PPrinting3D_IPrinting3DMultiplePropertyMaterialGroupFactory = ^Printing3D_IPrinting3DMultiplePropertyMaterialGroupFactory;

  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup2
  Printing3D_IPrinting3DTexture2CoordMaterialGroup2 = interface;
  PPrinting3D_IPrinting3DTexture2CoordMaterialGroup2 = ^Printing3D_IPrinting3DTexture2CoordMaterialGroup2;

  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroupFactory
  Printing3D_IPrinting3DTexture2CoordMaterialGroupFactory = interface;
  PPrinting3D_IPrinting3DTexture2CoordMaterialGroupFactory = ^Printing3D_IPrinting3DTexture2CoordMaterialGroupFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IIterator_1__Imaging_BitmapPixelFormat = interface;
  PIIterator_1__Imaging_BitmapPixelFormat = ^IIterator_1__Imaging_BitmapPixelFormat;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IIterable_1__Imaging_BitmapPixelFormat = interface;
  PIIterable_1__Imaging_BitmapPixelFormat = ^IIterable_1__Imaging_BitmapPixelFormat;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IVectorView_1__Imaging_BitmapPixelFormat = interface;
  PIVectorView_1__Imaging_BitmapPixelFormat = ^IVectorView_1__Imaging_BitmapPixelFormat;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IIterator_1__Imaging_ISoftwareBitmap = interface;
  PIIterator_1__Imaging_ISoftwareBitmap = ^IIterator_1__Imaging_ISoftwareBitmap;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IIterable_1__Imaging_ISoftwareBitmap = interface;
  PIIterable_1__Imaging_ISoftwareBitmap = ^IIterable_1__Imaging_ISoftwareBitmap;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IIterator_1__DirectX_Direct3D11_IDirect3DSurface = interface;
  PIIterator_1__DirectX_Direct3D11_IDirect3DSurface = ^IIterator_1__DirectX_Direct3D11_IDirect3DSurface;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IIterable_1__DirectX_Direct3D11_IDirect3DSurface = interface;
  PIIterable_1__DirectX_Direct3D11_IDirect3DSurface = ^IIterable_1__DirectX_Direct3D11_IDirect3DSurface;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IVectorView_1__DirectX_Direct3D11_IDirect3DSurface = interface;
  PIVectorView_1__DirectX_Direct3D11_IDirect3DSurface = ^IVectorView_1__DirectX_Direct3D11_IDirect3DSurface;

  // Windows.Foundation.IReference`1<Windows.Graphics.Imaging.BitmapBounds>
  IReference_1__Imaging_BitmapBounds = interface;
  PIReference_1__Imaging_BitmapBounds = ^IReference_1__Imaging_BitmapBounds;

  // Windows.Graphics Enums

  // Windows.Graphics.DirectX.Direct3D11.Direct3DBindings
  DirectX_Direct3D11_Direct3DBindings = (
    VertexBuffer = 1,
    IndexBuffer = 2,
    ConstantBuffer = 4,
    ShaderResource = 8,
    StreamOutput = 16,
    RenderTarget = 32,
    DepthStencil = 64,
    UnorderedAccess = 128,
    Decoder = 512,
    VideoEncoder = 1024
  );
  PDirectX_Direct3D11_Direct3DBindings = ^DirectX_Direct3D11_Direct3DBindings;

  // Windows.Graphics.DirectX.Direct3D11.Direct3DUsage
  DirectX_Direct3D11_Direct3DUsage = (
    Default = 0,
    Immutable = 1,
    Dynamic = 2,
    Staging = 3
  );
  PDirectX_Direct3D11_Direct3DUsage = ^DirectX_Direct3D11_Direct3DUsage;

  // Windows.Graphics.DirectX.DirectXColorSpace
  DirectX_DirectXColorSpace = (
    RgbFullG22NoneP709 = 0,
    RgbFullG10NoneP709 = 1,
    RgbStudioG22NoneP709 = 2,
    RgbStudioG22NoneP2020 = 3,
    Reserved = 4,
    YccFullG22NoneP709X601 = 5,
    YccStudioG22LeftP601 = 6,
    YccFullG22LeftP601 = 7,
    YccStudioG22LeftP709 = 8,
    YccFullG22LeftP709 = 9,
    YccStudioG22LeftP2020 = 10,
    YccFullG22LeftP2020 = 11,
    RgbFullG2084NoneP2020 = 12,
    YccStudioG2084LeftP2020 = 13,
    RgbStudioG2084NoneP2020 = 14,
    YccStudioG22TopLeftP2020 = 15,
    YccStudioG2084TopLeftP2020 = 16,
    RgbFullG22NoneP2020 = 17,
    YccStudioGHlgTopLeftP2020 = 18,
    YccFullGHlgTopLeftP2020 = 19,
    RgbStudioG24NoneP709 = 20,
    RgbStudioG24NoneP2020 = 21,
    YccStudioG24LeftP709 = 22,
    YccStudioG24LeftP2020 = 23,
    YccStudioG24TopLeftP2020 = 24
  );
  PDirectX_DirectXColorSpace = ^DirectX_DirectXColorSpace;

  // Windows.Graphics.Display.AdvancedColorKind
  Display_AdvancedColorKind = (
    StandardDynamicRange = 0,
    WideColorGamut = 1,
    HighDynamicRange = 2
  );
  PDisplay_AdvancedColorKind = ^Display_AdvancedColorKind;

  // Windows.Graphics.Display.Core.HdmiDisplayColorSpace
  Display_Core_HdmiDisplayColorSpace = (
    RgbLimited = 0,
    RgbFull = 1,
    BT2020 = 2,
    BT709 = 3
  );
  PDisplay_Core_HdmiDisplayColorSpace = ^Display_Core_HdmiDisplayColorSpace;

  // Windows.Graphics.Display.Core.HdmiDisplayHdrOption
  Display_Core_HdmiDisplayHdrOption = (
    None = 0,
    EotfSdr = 1,
    Eotf2084 = 2,
    DolbyVisionLowLatency = 3
  );
  PDisplay_Core_HdmiDisplayHdrOption = ^Display_Core_HdmiDisplayHdrOption;

  // Windows.Graphics.Display.Core.HdmiDisplayPixelEncoding
  Display_Core_HdmiDisplayPixelEncoding = (
    Rgb444 = 0,
    Ycc444 = 1,
    Ycc422 = 2,
    Ycc420 = 3
  );
  PDisplay_Core_HdmiDisplayPixelEncoding = ^Display_Core_HdmiDisplayPixelEncoding;

  // Windows.Graphics.Display.DisplayBrightnessOverrideOptions
  Display_DisplayBrightnessOverrideOptions = (
    None = 0,
    UseDimmedPolicyWhenBatteryIsLow = 1
  );
  PDisplay_DisplayBrightnessOverrideOptions = ^Display_DisplayBrightnessOverrideOptions;

  // Windows.Graphics.Display.DisplayBrightnessOverrideScenario
  Display_DisplayBrightnessOverrideScenario = (
    IdleBrightness = 0,
    BarcodeReadingBrightness = 1,
    FullBrightness = 2
  );
  PDisplay_DisplayBrightnessOverrideScenario = ^Display_DisplayBrightnessOverrideScenario;

  // Windows.Graphics.Display.DisplayBrightnessScenario
  Display_DisplayBrightnessScenario = (
    DefaultBrightness = 0,
    IdleBrightness = 1,
    BarcodeReadingBrightness = 2,
    FullBrightness = 3
  );
  PDisplay_DisplayBrightnessScenario = ^Display_DisplayBrightnessScenario;

  // Windows.Graphics.Display.DisplayColorOverrideScenario
  Display_DisplayColorOverrideScenario = (
    Accurate = 0
  );
  PDisplay_DisplayColorOverrideScenario = ^Display_DisplayColorOverrideScenario;

  // Windows.Graphics.Display.DisplayOrientations
  Display_DisplayOrientations = (
    None = 0,
    Landscape = 1,
    Portrait = 2,
    LandscapeFlipped = 4,
    PortraitFlipped = 8
  );
  PDisplay_DisplayOrientations = ^Display_DisplayOrientations;

  // Windows.Graphics.Display.HdrMetadataFormat
  Display_HdrMetadataFormat = (
    Hdr10 = 0,
    Hdr10Plus = 1
  );
  PDisplay_HdrMetadataFormat = ^Display_HdrMetadataFormat;

  // Windows.Graphics.Display.ResolutionScale
  Display_ResolutionScale = (
    Invalid = 0,
    Scale100Percent = 100,
    Scale120Percent = 120,
    Scale125Percent = 125,
    Scale140Percent = 140,
    Scale150Percent = 150,
    Scale160Percent = 160,
    Scale175Percent = 175,
    Scale180Percent = 180,
    Scale200Percent = 200,
    Scale225Percent = 225,
    Scale250Percent = 250,
    Scale300Percent = 300,
    Scale350Percent = 350,
    Scale400Percent = 400,
    Scale450Percent = 450,
    Scale500Percent = 500
  );
  PDisplay_ResolutionScale = ^Display_ResolutionScale;

  // Windows.Graphics.Holographic.HolographicFramePresentResult
  Holographic_HolographicFramePresentResult = (
    Success = 0,
    DeviceRemoved = 1
  );
  PHolographic_HolographicFramePresentResult = ^Holographic_HolographicFramePresentResult;

  // Windows.Graphics.Holographic.HolographicFramePresentWaitBehavior
  Holographic_HolographicFramePresentWaitBehavior = (
    WaitForFrameToFinish = 0,
    DoNotWaitForFrameToFinish = 1
  );
  PHolographic_HolographicFramePresentWaitBehavior = ^Holographic_HolographicFramePresentWaitBehavior;

  // Windows.Graphics.Holographic.HolographicReprojectionMode
  Holographic_HolographicReprojectionMode = (
    PositionAndOrientation = 0,
    OrientationOnly = 1,
    Disabled = 2
  );
  PHolographic_HolographicReprojectionMode = ^Holographic_HolographicReprojectionMode;

  // Windows.Graphics.Holographic.HolographicSpaceUserPresence
  Holographic_HolographicSpaceUserPresence = (
    Absent = 0,
    PresentPassive = 1,
    PresentActive = 2
  );
  PHolographic_HolographicSpaceUserPresence = ^Holographic_HolographicSpaceUserPresence;

  // Windows.Graphics.Holographic.HolographicViewConfigurationKind
  Holographic_HolographicViewConfigurationKind = (
    Display = 0,
    PhotoVideoCamera = 1
  );
  PHolographic_HolographicViewConfigurationKind = ^Holographic_HolographicViewConfigurationKind;

  // Windows.Graphics.Imaging.BitmapFlip
  Imaging_BitmapFlip = (
    None = 0,
    Horizontal = 1,
    Vertical = 2
  );
  PImaging_BitmapFlip = ^Imaging_BitmapFlip;

  // Windows.Graphics.Imaging.BitmapInterpolationMode
  Imaging_BitmapInterpolationMode = (
    NearestNeighbor = 0,
    Linear = 1,
    Cubic = 2,
    Fant = 3
  );
  PImaging_BitmapInterpolationMode = ^Imaging_BitmapInterpolationMode;

  // Windows.Graphics.Imaging.BitmapRotation
  Imaging_BitmapRotation = (
    None = 0,
    Clockwise90Degrees = 1,
    Clockwise180Degrees = 2,
    Clockwise270Degrees = 3
  );
  PImaging_BitmapRotation = ^Imaging_BitmapRotation;

  // Windows.Graphics.Imaging.ColorManagementMode
  Imaging_ColorManagementMode = (
    DoNotColorManage = 0,
    ColorManageToSRgb = 1
  );
  PImaging_ColorManagementMode = ^Imaging_ColorManagementMode;

  // Windows.Graphics.Imaging.ExifOrientationMode
  Imaging_ExifOrientationMode = (
    IgnoreExifOrientation = 0,
    RespectExifOrientation = 1
  );
  PImaging_ExifOrientationMode = ^Imaging_ExifOrientationMode;

  // Windows.Graphics.Imaging.JpegSubsamplingMode
  Imaging_JpegSubsamplingMode = (
    Default = 0,
    Y4Cb2Cr0 = 1,
    Y4Cb2Cr2 = 2,
    Y4Cb4Cr4 = 3
  );
  PImaging_JpegSubsamplingMode = ^Imaging_JpegSubsamplingMode;

  // Windows.Graphics.Imaging.PngFilterMode
  Imaging_PngFilterMode = (
    Automatic = 0,
    None = 1,
    Sub = 2,
    Up = 3,
    Average = 4,
    Paeth = 5,
    Adaptive = 6
  );
  PImaging_PngFilterMode = ^Imaging_PngFilterMode;

  // Windows.Graphics.Imaging.TiffCompressionMode
  Imaging_TiffCompressionMode = (
    Automatic = 0,
    None = 1,
    Ccitt3 = 2,
    Ccitt4 = 3,
    Lzw = 4,
    Rle = 5,
    Zip = 6,
    LzwhDifferencing = 7
  );
  PImaging_TiffCompressionMode = ^Imaging_TiffCompressionMode;

  // Windows.Graphics.Printing.OptionDetails.PrintOptionStates
  Printing_OptionDetails_PrintOptionStates = (
    None = 0,
    Enabled = 1,
    Constrained = 2
  );
  PPrinting_OptionDetails_PrintOptionStates = ^Printing_OptionDetails_PrintOptionStates;

  // Windows.Graphics.Printing.OptionDetails.PrintOptionType
  Printing_OptionDetails_PrintOptionType = (
    Unknown = 0,
    Number = 1,
    Text = 2,
    ItemList = 3,
    Toggle = 4
  );
  PPrinting_OptionDetails_PrintOptionType = ^Printing_OptionDetails_PrintOptionType;

  // Windows.Graphics.Printing.PrintBinding
  Printing_PrintBinding = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    None = 3,
    Bale = 4,
    BindBottom = 5,
    BindLeft = 6,
    BindRight = 7,
    BindTop = 8,
    Booklet = 9,
    EdgeStitchBottom = 10,
    EdgeStitchLeft = 11,
    EdgeStitchRight = 12,
    EdgeStitchTop = 13,
    Fold = 14,
    JogOffset = 15,
    Trim = 16
  );
  PPrinting_PrintBinding = ^Printing_PrintBinding;

  // Windows.Graphics.Printing.PrintBordering
  Printing_PrintBordering = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    Bordered = 3,
    Borderless = 4
  );
  PPrinting_PrintBordering = ^Printing_PrintBordering;

  // Windows.Graphics.Printing.PrintCollation
  Printing_PrintCollation = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    Collated = 3,
    Uncollated = 4
  );
  PPrinting_PrintCollation = ^Printing_PrintCollation;

  // Windows.Graphics.Printing.PrintColorMode
  Printing_PrintColorMode = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    Color = 3,
    Grayscale = 4,
    Monochrome = 5
  );
  PPrinting_PrintColorMode = ^Printing_PrintColorMode;

  // Windows.Graphics.Printing.PrintDuplex
  Printing_PrintDuplex = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    OneSided = 3,
    TwoSidedShortEdge = 4,
    TwoSidedLongEdge = 5
  );
  PPrinting_PrintDuplex = ^Printing_PrintDuplex;

  // Windows.Graphics.Printing.PrintHolePunch
  Printing_PrintHolePunch = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    None = 3,
    LeftEdge = 4,
    RightEdge = 5,
    TopEdge = 6,
    BottomEdge = 7
  );
  PPrinting_PrintHolePunch = ^Printing_PrintHolePunch;

  // Windows.Graphics.Printing.PrintMediaSize
  Printing_PrintMediaSize = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    BusinessCard = 3,
    CreditCard = 4,
    IsoA0 = 5,
    IsoA1 = 6,
    IsoA10 = 7,
    IsoA2 = 8,
    IsoA3 = 9,
    IsoA3Extra = 10,
    IsoA3Rotated = 11,
    IsoA4 = 12,
    IsoA4Extra = 13,
    IsoA4Rotated = 14,
    IsoA5 = 15,
    IsoA5Extra = 16,
    IsoA5Rotated = 17,
    IsoA6 = 18,
    IsoA6Rotated = 19,
    IsoA7 = 20,
    IsoA8 = 21,
    IsoA9 = 22,
    IsoB0 = 23,
    IsoB1 = 24,
    IsoB10 = 25,
    IsoB2 = 26,
    IsoB3 = 27,
    IsoB4 = 28,
    IsoB4Envelope = 29,
    IsoB5Envelope = 30,
    IsoB5Extra = 31,
    IsoB7 = 32,
    IsoB8 = 33,
    IsoB9 = 34,
    IsoC0 = 35,
    IsoC1 = 36,
    IsoC10 = 37,
    IsoC2 = 38,
    IsoC3 = 39,
    IsoC3Envelope = 40,
    IsoC4 = 41,
    IsoC4Envelope = 42,
    IsoC5 = 43,
    IsoC5Envelope = 44,
    IsoC6 = 45,
    IsoC6C5Envelope = 46,
    IsoC6Envelope = 47,
    IsoC7 = 48,
    IsoC8 = 49,
    IsoC9 = 50,
    IsoDLEnvelope = 51,
    IsoDLEnvelopeRotated = 52,
    IsoSRA3 = 53,
    Japan2LPhoto = 54,
    JapanChou3Envelope = 55,
    JapanChou3EnvelopeRotated = 56,
    JapanChou4Envelope = 57,
    JapanChou4EnvelopeRotated = 58,
    JapanDoubleHagakiPostcard = 59,
    JapanDoubleHagakiPostcardRotated = 60,
    JapanHagakiPostcard = 61,
    JapanHagakiPostcardRotated = 62,
    JapanKaku2Envelope = 63,
    JapanKaku2EnvelopeRotated = 64,
    JapanKaku3Envelope = 65,
    JapanKaku3EnvelopeRotated = 66,
    JapanLPhoto = 67,
    JapanQuadrupleHagakiPostcard = 68,
    JapanYou1Envelope = 69,
    JapanYou2Envelope = 70,
    JapanYou3Envelope = 71,
    JapanYou4Envelope = 72,
    JapanYou4EnvelopeRotated = 73,
    JapanYou6Envelope = 74,
    JapanYou6EnvelopeRotated = 75,
    JisB0 = 76,
    JisB1 = 77,
    JisB10 = 78,
    JisB2 = 79,
    JisB3 = 80,
    JisB4 = 81,
    JisB4Rotated = 82,
    JisB5 = 83,
    JisB5Rotated = 84,
    JisB6 = 85,
    JisB6Rotated = 86,
    JisB7 = 87,
    JisB8 = 88,
    JisB9 = 89,
    NorthAmerica10x11 = 90,
    NorthAmerica10x12 = 91,
    NorthAmerica10x14 = 92,
    NorthAmerica11x17 = 93,
    NorthAmerica14x17 = 94,
    NorthAmerica4x6 = 95,
    NorthAmerica4x8 = 96,
    NorthAmerica5x7 = 97,
    NorthAmerica8x10 = 98,
    NorthAmerica9x11 = 99,
    NorthAmericaArchitectureASheet = 100,
    NorthAmericaArchitectureBSheet = 101,
    NorthAmericaArchitectureCSheet = 102,
    NorthAmericaArchitectureDSheet = 103,
    NorthAmericaArchitectureESheet = 104,
    NorthAmericaCSheet = 105,
    NorthAmericaDSheet = 106,
    NorthAmericaESheet = 107,
    NorthAmericaExecutive = 108,
    NorthAmericaGermanLegalFanfold = 109,
    NorthAmericaGermanStandardFanfold = 110,
    NorthAmericaLegal = 111,
    NorthAmericaLegalExtra = 112,
    NorthAmericaLetter = 113,
    NorthAmericaLetterExtra = 114,
    NorthAmericaLetterPlus = 115,
    NorthAmericaLetterRotated = 116,
    NorthAmericaMonarchEnvelope = 117,
    NorthAmericaNote = 118,
    NorthAmericaNumber10Envelope = 119,
    NorthAmericaNumber10EnvelopeRotated = 120,
    NorthAmericaNumber11Envelope = 121,
    NorthAmericaNumber12Envelope = 122,
    NorthAmericaNumber14Envelope = 123,
    NorthAmericaNumber9Envelope = 124,
    NorthAmericaPersonalEnvelope = 125,
    NorthAmericaQuarto = 126,
    NorthAmericaStatement = 127,
    NorthAmericaSuperA = 128,
    NorthAmericaSuperB = 129,
    NorthAmericaTabloid = 130,
    NorthAmericaTabloidExtra = 131,
    OtherMetricA3Plus = 132,
    OtherMetricA4Plus = 133,
    OtherMetricFolio = 134,
    OtherMetricInviteEnvelope = 135,
    OtherMetricItalianEnvelope = 136,
    Prc10Envelope = 137,
    Prc10EnvelopeRotated = 138,
    Prc16K = 139,
    Prc16KRotated = 140,
    Prc1Envelope = 141,
    Prc1EnvelopeRotated = 142,
    Prc2Envelope = 143,
    Prc2EnvelopeRotated = 144,
    Prc32K = 145,
    Prc32KBig = 146,
    Prc32KRotated = 147,
    Prc3Envelope = 148,
    Prc3EnvelopeRotated = 149,
    Prc4Envelope = 150,
    Prc4EnvelopeRotated = 151,
    Prc5Envelope = 152,
    Prc5EnvelopeRotated = 153,
    Prc6Envelope = 154,
    Prc6EnvelopeRotated = 155,
    Prc7Envelope = 156,
    Prc7EnvelopeRotated = 157,
    Prc8Envelope = 158,
    Prc8EnvelopeRotated = 159,
    Prc9Envelope = 160,
    Prc9EnvelopeRotated = 161,
    Roll04Inch = 162,
    Roll06Inch = 163,
    Roll08Inch = 164,
    Roll12Inch = 165,
    Roll15Inch = 166,
    Roll18Inch = 167,
    Roll22Inch = 168,
    Roll24Inch = 169,
    Roll30Inch = 170,
    Roll36Inch = 171,
    Roll54Inch = 172
  );
  PPrinting_PrintMediaSize = ^Printing_PrintMediaSize;

  // Windows.Graphics.Printing.PrintMediaType
  Printing_PrintMediaType = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    AutoSelect = 3,
    Archival = 4,
    BackPrintFilm = 5,
    Bond = 6,
    CardStock = 7,
    Continuous = 8,
    EnvelopePlain = 9,
    EnvelopeWindow = 10,
    Fabric = 11,
    HighResolution = 12,
    &Label = 13,
    MultiLayerForm = 14,
    MultiPartForm = 15,
    Photographic = 16,
    PhotographicFilm = 17,
    PhotographicGlossy = 18,
    PhotographicHighGloss = 19,
    PhotographicMatte = 20,
    PhotographicSatin = 21,
    PhotographicSemiGloss = 22,
    Plain = 23,
    Screen = 24,
    ScreenPaged = 25,
    Stationery = 26,
    TabStockFull = 27,
    TabStockPreCut = 28,
    Transparency = 29,
    TShirtTransfer = 30,
    None = 31
  );
  PPrinting_PrintMediaType = ^Printing_PrintMediaType;

  // Windows.Graphics.Printing.PrintOrientation
  Printing_PrintOrientation = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    Portrait = 3,
    PortraitFlipped = 4,
    Landscape = 5,
    LandscapeFlipped = 6
  );
  PPrinting_PrintOrientation = ^Printing_PrintOrientation;

  // Windows.Graphics.Printing.PrintQuality
  Printing_PrintQuality = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    Automatic = 3,
    Draft = 4,
    Fax = 5,
    High = 6,
    Normal = 7,
    Photographic = 8,
    Text = 9
  );
  PPrinting_PrintQuality = ^Printing_PrintQuality;

  // Windows.Graphics.Printing.PrintStaple
  Printing_PrintStaple = (
    Default = 0,
    NotAvailable = 1,
    PrinterCustom = 2,
    None = 3,
    StapleTopLeft = 4,
    StapleTopRight = 5,
    StapleBottomLeft = 6,
    StapleBottomRight = 7,
    StapleDualLeft = 8,
    StapleDualRight = 9,
    StapleDualTop = 10,
    StapleDualBottom = 11,
    SaddleStitch = 12
  );
  PPrinting_PrintStaple = ^Printing_PrintStaple;

  // Windows.Graphics.Printing.PrintTaskCompletion
  Printing_PrintTaskCompletion = (
    Abandoned = 0,
    Canceled = 1,
    Failed = 2,
    Submitted = 3
  );
  PPrinting_PrintTaskCompletion = ^Printing_PrintTaskCompletion;

  // Windows.Graphics.Printing.PrintTicket.PrintTicketFeatureSelectionType
  Printing_PrintTicket_PrintTicketFeatureSelectionType = (
    PickOne = 0,
    PickMany = 1
  );
  PPrinting_PrintTicket_PrintTicketFeatureSelectionType = ^Printing_PrintTicket_PrintTicketFeatureSelectionType;

  // Windows.Graphics.Printing.PrintTicket.PrintTicketParameterDataType
  Printing_PrintTicket_PrintTicketParameterDataType = (
    Integer = 0,
    NumericString = 1,
    &String = 2
  );
  PPrinting_PrintTicket_PrintTicketParameterDataType = ^Printing_PrintTicket_PrintTicketParameterDataType;

  // Windows.Graphics.Printing.PrintTicket.PrintTicketValueType
  Printing_PrintTicket_PrintTicketValueType = (
    Integer = 0,
    &String = 1,
    Unknown = 2
  );
  PPrinting_PrintTicket_PrintTicketValueType = ^Printing_PrintTicket_PrintTicketValueType;

  // Windows.Graphics.Printing.Workflow.PrintWorkflowSessionStatus
  Printing_Workflow_PrintWorkflowSessionStatus = (
    Started = 0,
    Completed = 1,
    Aborted = 2,
    Closed = 3
  );
  PPrinting_Workflow_PrintWorkflowSessionStatus = ^Printing_Workflow_PrintWorkflowSessionStatus;

  // Windows.Graphics.Printing.Workflow.PrintWorkflowSubmittedStatus
  Printing_Workflow_PrintWorkflowSubmittedStatus = (
    Succeeded = 0,
    Canceled = 1,
    Failed = 2
  );
  PPrinting_Workflow_PrintWorkflowSubmittedStatus = ^Printing_Workflow_PrintWorkflowSubmittedStatus;

  // Windows.Graphics.Printing3D.Print3DTaskCompletion
  Printing3D_Print3DTaskCompletion = (
    Abandoned = 0,
    Canceled = 1,
    Failed = 2,
    Slicing = 3,
    Submitted = 4
  );
  PPrinting3D_Print3DTaskCompletion = ^Printing3D_Print3DTaskCompletion;

  // Windows.Graphics.Printing3D.Print3DTaskDetail
  Printing3D_Print3DTaskDetail = (
    Unknown = 0,
    ModelExceedsPrintBed = 1,
    UploadFailed = 2,
    InvalidMaterialSelection = 3,
    InvalidModel = 4,
    ModelNotManifold = 5,
    InvalidPrintTicket = 6
  );
  PPrinting3D_Print3DTaskDetail = ^Printing3D_Print3DTaskDetail;

  // Windows.Graphics.Printing3D.Printing3DBufferFormat
  Printing3D_Printing3DBufferFormat = (
    Unknown = 0,
    R32G32B32A32Float = 2,
    R32G32B32A32UInt = 3,
    R32G32B32Float = 6,
    R32G32B32UInt = 7,
    Printing3DDouble = 500,
    Printing3DUInt = 501
  );
  PPrinting3D_Printing3DBufferFormat = ^Printing3D_Printing3DBufferFormat;

  // Windows.Graphics.Printing3D.Printing3DMeshVerificationMode
  Printing3D_Printing3DMeshVerificationMode = (
    FindFirstError = 0,
    FindAllErrors = 1
  );
  PPrinting3D_Printing3DMeshVerificationMode = ^Printing3D_Printing3DMeshVerificationMode;

  // Windows.Graphics.Printing3D.Printing3DModelUnit
  Printing3D_Printing3DModelUnit = (
    Meter = 0,
    Micron = 1,
    Millimeter = 2,
    Centimeter = 3,
    Inch = 4,
    Foot = 5
  );
  PPrinting3D_Printing3DModelUnit = ^Printing3D_Printing3DModelUnit;

  // Windows.Graphics.Printing3D.Printing3DObjectType
  Printing3D_Printing3DObjectType = (
    Model = 0,
    Support = 1,
    Others = 2
  );
  PPrinting3D_Printing3DObjectType = ^Printing3D_Printing3DObjectType;

  // Windows.Graphics.Printing3D.Printing3DPackageCompression
  Printing3D_Printing3DPackageCompression = (
    Low = 0,
    Medium = 1,
    High = 2
  );
  PPrinting3D_Printing3DPackageCompression = ^Printing3D_Printing3DPackageCompression;

  // Windows.Graphics.Printing3D.Printing3DTextureEdgeBehavior
  Printing3D_Printing3DTextureEdgeBehavior = (
    None = 0,
    Wrap = 1,
    Mirror = 2,
    Clamp = 3
  );
  PPrinting3D_Printing3DTextureEdgeBehavior = ^Printing3D_Printing3DTextureEdgeBehavior;

  // Windows.Graphics Records
  // Windows.Graphics.DirectX.Direct3D11.Direct3DMultisampleDescription
  DirectX_Direct3D11_Direct3DMultisampleDescription = record
    Count: Integer;
    Quality: Integer;
  end;
  PDirectX_Direct3D11_Direct3DMultisampleDescription = ^DirectX_Direct3D11_Direct3DMultisampleDescription;

  // Windows.Graphics.DirectX.Direct3D11.Direct3DSurfaceDescription
  DirectX_Direct3D11_Direct3DSurfaceDescription = record
    Width: Integer;
    Height: Integer;
    Format: DirectX_DirectXPixelFormat;
    MultisampleDescription: DirectX_Direct3D11_Direct3DMultisampleDescription;
  end;
  PDirectX_Direct3D11_Direct3DSurfaceDescription = ^DirectX_Direct3D11_Direct3DSurfaceDescription;

  // Windows.Graphics.Display.Core.HdmiDisplayHdr2086Metadata
  Display_Core_HdmiDisplayHdr2086Metadata = record
    RedPrimaryX: Word;
    RedPrimaryY: Word;
    GreenPrimaryX: Word;
    GreenPrimaryY: Word;
    BluePrimaryX: Word;
    BluePrimaryY: Word;
    WhitePointX: Word;
    WhitePointY: Word;
    MaxMasteringLuminance: Word;
    MinMasteringLuminance: Word;
    MaxContentLightLevel: Word;
    MaxFrameAverageLightLevel: Word;
  end;
  PDisplay_Core_HdmiDisplayHdr2086Metadata = ^Display_Core_HdmiDisplayHdr2086Metadata;

  // Windows.Graphics.Display.NitRange
  Display_NitRange = record
    MinNits: Single;
    MaxNits: Single;
    StepSizeNits: Single;
  end;
  PDisplay_NitRange = ^Display_NitRange;

  // Windows.Graphics.DisplayAdapterId
  DisplayAdapterId = record
    LowPart: Cardinal;
    HighPart: Integer;
  end;
  PDisplayAdapterId = ^DisplayAdapterId;

  // Windows.Graphics.Holographic.HolographicAdapterId
  Holographic_HolographicAdapterId = record
    LowPart: Cardinal;
    HighPart: Integer;
  end;
  PHolographic_HolographicAdapterId = ^Holographic_HolographicAdapterId;

  // Windows.Graphics.Holographic.HolographicFrameId
  Holographic_HolographicFrameId = record
    Value: UInt64;
  end;
  PHolographic_HolographicFrameId = ^Holographic_HolographicFrameId;

  // Windows.Graphics.Holographic.HolographicStereoTransform
  Holographic_HolographicStereoTransform = record
    Left: Numerics_Matrix4x4;
    Right: Numerics_Matrix4x4;
  end;
  PHolographic_HolographicStereoTransform = ^Holographic_HolographicStereoTransform;

  // Windows.Graphics.Imaging.BitmapBounds
  Imaging_BitmapBounds = record
    X: Cardinal;
    Y: Cardinal;
    Width: Cardinal;
    Height: Cardinal;
  end;
  PImaging_BitmapBounds = ^Imaging_BitmapBounds;

  // Windows.Graphics.Imaging.BitmapSize
  Imaging_BitmapSize = record
    Width: Cardinal;
    Height: Cardinal;
  end;
  PImaging_BitmapSize = ^Imaging_BitmapSize;

  // Windows.Graphics.Printing.PrintPageDescription
  Printing_PrintPageDescription = record
    PageSize: TSizeF;
    ImageableRect: TRectF;
    DpiX: Cardinal;
    DpiY: Cardinal;
  end;
  PPrinting_PrintPageDescription = ^Printing_PrintPageDescription;

  // Windows.Graphics.Printing3D.Printing3DBufferDescription
  Printing3D_Printing3DBufferDescription = record
    Format: Printing3D_Printing3DBufferFormat;
    Stride: Cardinal;
  end;
  PPrinting3D_Printing3DBufferDescription = ^Printing3D_Printing3DBufferDescription;

  // Windows.Graphics.Printing3D.Printing3DContract
  Printing3D_Printing3DContract = record
  end;
  PPrinting3D_Printing3DContract = ^Printing3D_Printing3DContract;

  // Windows.Graphics Interfaces

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Effects.IGraphicsEffectSource
  Effects_IGraphicsEffectSource = interface(IInspectable)
  ['{2D8F9DDC-4339-4EB9-9216-F9DEB75658A2}']
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing.IPrintTaskOptionsCore
  Printing_IPrintTaskOptionsCore = interface(IInspectable)
  ['{1BDBB474-4ED1-41EB-BE3C-72D18ED67337}']
    function GetPageDescription(jobPageNumber: Cardinal): Printing_PrintPageDescription; safecall;
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing.IPrintDocumentSource
  Printing_IPrintDocumentSource = interface(IInspectable)
  ['{DEDC0C30-F1EB-47DF-AAE6-ED5427511F01}']
  end;

  // UsedAPI Interface
  // Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface
  DirectX_Direct3D11_IDirect3DSurface = interface(IInspectable)
  ['{0BF4A146-13C1-4694-BEE3-7ABF15EAF586}']
    function get_Description: DirectX_Direct3D11_Direct3DSurfaceDescription; safecall;
    property Description: DirectX_Direct3D11_Direct3DSurfaceDescription read get_Description;
  end;

  // UsedAPI Interface
  // Windows.Graphics.DirectX.Direct3D11.IDirect3DDevice
  DirectX_Direct3D11_IDirect3DDevice = interface(IInspectable)
  ['{A37624AB-8D5F-4650-9D3E-9EAE3D9BC670}']
    procedure Trim; safecall;
  end;

  // Windows.Foundation.IReference`1<Windows.Graphics.SizeInt32>
  IReference_1__SizeInt32 = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: SizeInt32; safecall;
    property Value: SizeInt32 read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Graphics.Capture.IDirect3D11CaptureFrame
  Capture_IDirect3D11CaptureFrame = interface(IInspectable)
  ['{FA50C623-38DA-4B32-ACF3-FA9734AD800E}']
    function get_Surface: DirectX_Direct3D11_IDirect3DSurface; safecall;
    function get_SystemRelativeTime: TimeSpan; safecall;
    function get_ContentSize: SizeInt32; safecall;
    property ContentSize: SizeInt32 read get_ContentSize;
    property Surface: DirectX_Direct3D11_IDirect3DSurface read get_Surface;
    property SystemRelativeTime: TimeSpan read get_SystemRelativeTime;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Capture.IDirect3D11CaptureFramePool,Object>
  TypedEventHandler_2__Capture_IDirect3D11CaptureFramePool__IInspectable = interface(IUnknown)
  ['{A83B5962-C017-5413-B3DE-9CE3513A8D78}']
    procedure Invoke(sender: Capture_IDirect3D11CaptureFramePool; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IGraphicsCaptureSession
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_GraphicsCaptureSession)]
  Capture_IGraphicsCaptureSession = interface(IInspectable)
  ['{814E42A9-F70F-4AD7-939B-FDDCC6EB880D}']
    procedure StartCapture; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Capture.IGraphicsCaptureItem,Object>
  TypedEventHandler_2__Capture_IGraphicsCaptureItem__IInspectable = interface(IUnknown)
  ['{844BB80F-B1C4-5B66-9FB1-4B41C6313F90}']
    procedure Invoke(sender: Capture_IGraphicsCaptureItem; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IGraphicsCaptureItem
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_GraphicsCaptureItem)]
  Capture_IGraphicsCaptureItem = interface(IInspectable)
  ['{79C3F95B-31F7-4EC2-A464-632EF5D30760}']
    function get_DisplayName: HSTRING; safecall;
    function get_Size: SizeInt32; safecall;
    function add_Closed(handler: TypedEventHandler_2__Capture_IGraphicsCaptureItem__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property Size: SizeInt32 read get_Size;
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IDirect3D11CaptureFramePool
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_Direct3D11CaptureFramePool)]
  Capture_IDirect3D11CaptureFramePool = interface(IInspectable)
  ['{24EB6D22-1975-422E-82E7-780DBD8DDF24}']
    procedure Recreate(device: DirectX_Direct3D11_IDirect3DDevice; pixelFormat: DirectX_DirectXPixelFormat; numberOfBuffers: Integer; size: SizeInt32); safecall;
    function TryGetNextFrame: Capture_IDirect3D11CaptureFrame; safecall;
    function add_FrameArrived(handler: TypedEventHandler_2__Capture_IDirect3D11CaptureFramePool__IInspectable): EventRegistrationToken; safecall;
    procedure remove_FrameArrived(token: EventRegistrationToken); safecall;
    function CreateCaptureSession(item: Capture_IGraphicsCaptureItem): Capture_IGraphicsCaptureSession; safecall;
    function get_DispatcherQueue: IDispatcherQueue; safecall;
    property DispatcherQueue: IDispatcherQueue read get_DispatcherQueue;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IDirect3D11CaptureFramePoolStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_Direct3D11CaptureFramePool)]
  Capture_IDirect3D11CaptureFramePoolStatics = interface(IInspectable)
  ['{7784056A-67AA-4D53-AE54-1088D5A8CA21}']
    function Create(device: DirectX_Direct3D11_IDirect3DDevice; pixelFormat: DirectX_DirectXPixelFormat; numberOfBuffers: Integer; size: SizeInt32): Capture_IDirect3D11CaptureFramePool; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IDirect3D11CaptureFramePoolStatics2
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_Direct3D11CaptureFramePool)]
  Capture_IDirect3D11CaptureFramePoolStatics2 = interface(IInspectable)
  ['{589B103F-6BBC-5DF5-A991-02E28B3B66D5}']
    function CreateFreeThreaded(device: DirectX_Direct3D11_IDirect3DDevice; pixelFormat: DirectX_DirectXPixelFormat; numberOfBuffers: Integer; size: SizeInt32): Capture_IDirect3D11CaptureFramePool; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IGraphicsCaptureItemStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_GraphicsCaptureItem)]
  Capture_IGraphicsCaptureItemStatics = interface(IInspectable)
  ['{A87EBEA5-457C-5788-AB47-0CF1D3637E74}']
    function CreateFromVisual(visual: IVisual): Capture_IGraphicsCaptureItem; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Capture.IGraphicsCaptureItem>
  AsyncOperationCompletedHandler_1__Capture_IGraphicsCaptureItem = interface(IUnknown)
  ['{09B88462-7AB0-513F-83BF-4D327D69FE8D}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Capture_IGraphicsCaptureItem; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Capture.IGraphicsCaptureItem>
  IAsyncOperation_1__Capture_IGraphicsCaptureItem = interface(IInspectable)
  ['{AA355ABB-4690-51BA-98C0-37C7A9DF5C3E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Capture_IGraphicsCaptureItem); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Capture_IGraphicsCaptureItem; safecall;
    function GetResults: Capture_IGraphicsCaptureItem; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Capture_IGraphicsCaptureItem read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IGraphicsCapturePicker
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_GraphicsCapturePicker)]
  Capture_IGraphicsCapturePicker = interface(IInspectable)
  ['{5A1711B3-AD79-4B4A-9336-1318FDDE3539}']
    function PickSingleItemAsync: IAsyncOperation_1__Capture_IGraphicsCaptureItem; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IGraphicsCaptureSession2
  Capture_IGraphicsCaptureSession2 = interface(IInspectable)
  ['{2C39AE40-7D2E-5044-804E-8B6799D4CF9E}']
    function get_IsCursorCaptureEnabled: Boolean; safecall;
    procedure put_IsCursorCaptureEnabled(value: Boolean); safecall;
    property IsCursorCaptureEnabled: Boolean read get_IsCursorCaptureEnabled write put_IsCursorCaptureEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Capture.IGraphicsCaptureSessionStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Capture_GraphicsCaptureSession)]
  Capture_IGraphicsCaptureSessionStatics = interface(IInspectable)
  ['{2224A540-5974-49AA-B232-0882536F4CB5}']
    function IsSupported: Boolean; safecall;
  end;

  // UsedAPI Interface
  // Windows.Graphics.Display.Core.IHdmiDisplayMode
  Display_Core_IHdmiDisplayMode = interface(IInspectable)
  ['{0C06D5AD-1B90-4F51-9981-EF5A1C0DDF66}']
    function get_ResolutionWidthInRawPixels: Cardinal; safecall;
    function get_ResolutionHeightInRawPixels: Cardinal; safecall;
    function get_RefreshRate: Double; safecall;
    function get_StereoEnabled: Boolean; safecall;
    function get_BitsPerPixel: Word; safecall;
    function IsEqual(mode: Display_Core_IHdmiDisplayMode): Boolean; safecall;
    function get_ColorSpace: Display_Core_HdmiDisplayColorSpace; safecall;
    function get_PixelEncoding: Display_Core_HdmiDisplayPixelEncoding; safecall;
    function get_IsSdrLuminanceSupported: Boolean; safecall;
    function get_IsSmpte2084Supported: Boolean; safecall;
    function get_Is2086MetadataSupported: Boolean; safecall;
    property BitsPerPixel: Word read get_BitsPerPixel;
    property ColorSpace: Display_Core_HdmiDisplayColorSpace read get_ColorSpace;
    property Is2086MetadataSupported: Boolean read get_Is2086MetadataSupported;
    property IsSdrLuminanceSupported: Boolean read get_IsSdrLuminanceSupported;
    property IsSmpte2084Supported: Boolean read get_IsSmpte2084Supported;
    property PixelEncoding: Display_Core_HdmiDisplayPixelEncoding read get_PixelEncoding;
    property RefreshRate: Double read get_RefreshRate;
    property ResolutionHeightInRawPixels: Cardinal read get_ResolutionHeightInRawPixels;
    property ResolutionWidthInRawPixels: Cardinal read get_ResolutionWidthInRawPixels;
    property StereoEnabled: Boolean read get_StereoEnabled;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IIterator_1__Display_Core_IHdmiDisplayMode_Base = interface(IInspectable)
  ['{D66EB831-E22C-5EE3-AF45-E1C03DE4BC62}']
    function get_Current: Display_Core_IHdmiDisplayMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDisplay_Core_IHdmiDisplayMode): Cardinal; safecall;
    property Current: Display_Core_IHdmiDisplayMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IIterator_1__Display_Core_IHdmiDisplayMode = interface(IIterator_1__Display_Core_IHdmiDisplayMode_Base)
  ['{81D1A91F-6D91-5821-AD41-F6B8300AAA20}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IIterable_1__Display_Core_IHdmiDisplayMode_Base = interface(IInspectable)
  ['{497E3D51-0EA1-5BE0-8DBA-8F7F4CE4FB33}']
    function First: IIterator_1__Display_Core_IHdmiDisplayMode; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IIterable_1__Display_Core_IHdmiDisplayMode = interface(IIterable_1__Display_Core_IHdmiDisplayMode_Base)
  ['{70B05995-63D6-5764-A988-AEFB469490C7}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Display.Core.IHdmiDisplayMode>
  IVectorView_1__Display_Core_IHdmiDisplayMode = interface(IInspectable)
  ['{8F92D8FD-DC7B-5855-8A94-438C76A3F54B}']
    function GetAt(index: Cardinal): Display_Core_IHdmiDisplayMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Display_Core_IHdmiDisplayMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDisplay_Core_IHdmiDisplayMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
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

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Display.Core.IHdmiDisplayInformation,Object>
  TypedEventHandler_2__Display_Core_IHdmiDisplayInformation__IInspectable_Delegate_Base = interface(IUnknown)
  ['{D109932B-9CE1-5CDD-94C7-93C60C833AA3}']
    procedure Invoke(sender: Display_Core_IHdmiDisplayInformation; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Display.Core.IHdmiDisplayInformation,Object>
  TypedEventHandler_2__Display_Core_IHdmiDisplayInformation__IInspectable = interface(TypedEventHandler_2__Display_Core_IHdmiDisplayInformation__IInspectable_Delegate_Base)
  ['{5AD20A88-C02A-5D55-BAE6-FF9F947EF6F1}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Display.Core.IHdmiDisplayInformation
  [WinRTClassNameAttribute(SWindows_Graphics_Display_Core_HdmiDisplayInformation)]
  Display_Core_IHdmiDisplayInformation = interface(IInspectable)
  ['{130B3C0A-F565-476E-ABD5-EA05AEE74C69}']
    function GetSupportedDisplayModes: IVectorView_1__Display_Core_IHdmiDisplayMode; safecall;
    function GetCurrentDisplayMode: Display_Core_IHdmiDisplayMode; safecall;
    function SetDefaultDisplayModeAsync: IAsyncAction; safecall;
    function RequestSetCurrentDisplayModeAsync(mode: Display_Core_IHdmiDisplayMode): IAsyncOperation_1__Boolean; overload; safecall;
    function RequestSetCurrentDisplayModeAsync(mode: Display_Core_IHdmiDisplayMode; hdrOption: Display_Core_HdmiDisplayHdrOption): IAsyncOperation_1__Boolean; overload; safecall;
    function RequestSetCurrentDisplayModeAsync(mode: Display_Core_IHdmiDisplayMode; hdrOption: Display_Core_HdmiDisplayHdrOption; hdrMetadata: Display_Core_HdmiDisplayHdr2086Metadata): IAsyncOperation_1__Boolean; overload; safecall;
    function add_DisplayModesChanged(value: TypedEventHandler_2__Display_Core_IHdmiDisplayInformation__IInspectable): EventRegistrationToken; safecall;
    procedure remove_DisplayModesChanged(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Display.Core.IHdmiDisplayInformationStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Display_Core_HdmiDisplayInformation)]
  Display_Core_IHdmiDisplayInformationStatics = interface(IInspectable)
  ['{6CE6B260-F42A-4A15-914C-7B8E2A5A65DF}']
    function GetForCurrentView: Display_Core_IHdmiDisplayInformation; safecall;
  end;

  // Windows.Graphics.Display.Core.IHdmiDisplayMode2
  Display_Core_IHdmiDisplayMode2 = interface(IInspectable)
  ['{07CD4E9F-4B3C-42B8-84E7-895368718AF2}']
    function get_IsDolbyVisionLowLatencySupported: Boolean; safecall;
    property IsDolbyVisionLowLatencySupported: Boolean read get_IsDolbyVisionLowLatencySupported;
  end;

  // Windows.Graphics.Display.DisplayPropertiesEventHandler
  Display_DisplayPropertiesEventHandler = interface(IUnknown)
  ['{DBDD8B01-F1A1-46D1-9EE3-543BCC995980}']
    procedure Invoke(sender: IInspectable); safecall;
  end deprecated;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Display.NitRange>
  IIterator_1__Display_NitRange = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: Display_NitRange; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDisplay_NitRange): Cardinal; safecall;
    property Current: Display_NitRange read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Display.NitRange>
  IIterable_1__Display_NitRange = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__Display_NitRange; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Display.NitRange>
  IVectorView_1__Display_NitRange = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Display_NitRange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Display_NitRange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDisplay_NitRange): Cardinal; safecall;
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

  // Windows.Foundation.IReference`1<Double>
  IReference_1__Double = interface(IInspectable)
  ['{2F2D6C29-5473-5F3E-92E7-96572BB990E2}']
    function get_Value: Double; safecall;
    property Value: Double read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IIterator_1__DirectX_DirectXPixelFormat_Base = interface(IInspectable)
  ['{EA016190-AC80-5840-8F58-FF434C7B2907}']
    function get_Current: DirectX_DirectXPixelFormat; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDirectX_DirectXPixelFormat): Cardinal; safecall;
    property Current: DirectX_DirectXPixelFormat read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IIterator_1__DirectX_DirectXPixelFormat = interface(IIterator_1__DirectX_DirectXPixelFormat_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IIterable_1__DirectX_DirectXPixelFormat_Base = interface(IInspectable)
  ['{3908F2C6-1AEE-5129-B9A6-2A6E01D9507E}']
    function First: IIterator_1__DirectX_DirectXPixelFormat; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IIterable_1__DirectX_DirectXPixelFormat = interface(IIterable_1__DirectX_DirectXPixelFormat_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.DirectX.DirectXPixelFormat>
  IVectorView_1__DirectX_DirectXPixelFormat = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): DirectX_DirectXPixelFormat; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: DirectX_DirectXPixelFormat; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDirectX_DirectXPixelFormat): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.IReference`1<Windows.Graphics.Holographic.HolographicStereoTransform>
  IReference_1__Holographic_HolographicStereoTransform = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Holographic_HolographicStereoTransform; safecall;
    property Value: Holographic_HolographicStereoTransform read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Holographic.IHolographicQuadLayer
  [WinRTClassNameAttribute(SWindows_Graphics_Holographic_HolographicQuadLayer)]
  Holographic_IHolographicQuadLayer = interface(IInspectable)
  ['{903460C9-C9D9-5D5C-41AC-A2D5AB0FD331}']
    function get_PixelFormat: DirectX_DirectXPixelFormat; safecall;
    function get_Size: TSizeF; safecall;
    property PixelFormat: DirectX_DirectXPixelFormat read get_PixelFormat;
    property Size: TSizeF read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Holographic.IHolographicQuadLayerFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Holographic_HolographicQuadLayer)]
  Holographic_IHolographicQuadLayerFactory = interface(IInspectable)
  ['{A67538F3-5A14-5A10-489A-455065B37B76}']
    function Create(size: TSizeF): IClosable; safecall;
    function CreateWithPixelFormat(size: TSizeF; pixelFormat: DirectX_DirectXPixelFormat): IClosable; safecall;
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

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface(IUnknown)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
    procedure Invoke(sender: IInspectable; args: IInspectable); safecall;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface(IUnknown)
  ['{F4637D4A-0760-5431-BFC0-24EB1D4F6C4F}']
    procedure Invoke(sender: IMemoryBufferReference; args: IInspectable); safecall;
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
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap_Delegate_Base = interface(IUnknown)
  ['{B699B653-33ED-5E2D-A75F-02BF90E32619}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Imaging_ISoftwareBitmap; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap = interface(AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap_Delegate_Base)
  ['{17C5C9CA-A7E8-5D2D-A505-711CF7942368}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IAsyncOperation_1__Imaging_ISoftwareBitmap_Base = interface(IInspectable)
  ['{C4A10980-714B-5501-8DA2-DBDACCE70F73}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap; safecall;
    function GetResults: Imaging_ISoftwareBitmap; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Imaging_ISoftwareBitmap read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IAsyncOperation_1__Imaging_ISoftwareBitmap = interface(IAsyncOperation_1__Imaging_ISoftwareBitmap_Base)
  ['{B15AFB2D-7DE2-5C65-AE8D-4B68B90FE93E}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Imaging.ISoftwareBitmapFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Imaging_SoftwareBitmap)]
  Imaging_ISoftwareBitmapFactory = interface(IInspectable)
  ['{C99FEB69-2D62-4D47-A6B3-4FDB6A07FDF8}']
    function Create(format: Imaging_BitmapPixelFormat; width: Integer; height: Integer): Imaging_ISoftwareBitmap; safecall;
    function CreateWithAlpha(format: Imaging_BitmapPixelFormat; width: Integer; height: Integer; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Imaging.ISoftwareBitmapStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Imaging_SoftwareBitmap)]
  Imaging_ISoftwareBitmapStatics = interface(IInspectable)
  ['{DF0385DB-672F-4A9D-806E-C2442F343E86}']
    function Copy(source: Imaging_ISoftwareBitmap): Imaging_ISoftwareBitmap; safecall;
    function Convert(source: Imaging_ISoftwareBitmap; format: Imaging_BitmapPixelFormat): Imaging_ISoftwareBitmap; overload; safecall;
    function Convert(source: Imaging_ISoftwareBitmap; format: Imaging_BitmapPixelFormat; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap; overload; safecall;
    function CreateCopyFromBuffer(source: IBuffer; format: Imaging_BitmapPixelFormat; width: Integer; height: Integer): Imaging_ISoftwareBitmap; overload; safecall;
    function CreateCopyFromBuffer(source: IBuffer; format: Imaging_BitmapPixelFormat; width: Integer; height: Integer; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap; overload; safecall;
    function CreateCopyFromSurfaceAsync(surface: DirectX_Direct3D11_IDirect3DSurface): IAsyncOperation_1__Imaging_ISoftwareBitmap; overload; safecall;
    function CreateCopyFromSurfaceAsync(surface: DirectX_Direct3D11_IDirect3DSurface; alpha: Imaging_BitmapAlphaMode): IAsyncOperation_1__Imaging_ISoftwareBitmap; overload; safecall;
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing.IPrintPageInfo
  [WinRTClassNameAttribute(SWindows_Graphics_Printing_PrintPageInfo)]
  Printing_IPrintPageInfo = interface(IInspectable)
  ['{DD4BE9C9-A6A1-4ADA-930E-DA872A4F23D3}']
    procedure put_MediaSize(value: Printing_PrintMediaSize); safecall;
    function get_MediaSize: Printing_PrintMediaSize; safecall;
    procedure put_PageSize(value: TSizeF); safecall;
    function get_PageSize: TSizeF; safecall;
    procedure put_DpiX(value: Cardinal); safecall;
    function get_DpiX: Cardinal; safecall;
    procedure put_DpiY(value: Cardinal); safecall;
    function get_DpiY: Cardinal; safecall;
    procedure put_Orientation(value: Printing_PrintOrientation); safecall;
    function get_Orientation: Printing_PrintOrientation; safecall;
    property DpiX: Cardinal read get_DpiX write put_DpiX;
    property DpiY: Cardinal read get_DpiY write put_DpiY;
    property MediaSize: Printing_PrintMediaSize read get_MediaSize write put_MediaSize;
    property Orientation: Printing_PrintOrientation read get_Orientation write put_Orientation;
    property PageSize: TSizeF read get_PageSize write put_PageSize;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing.IPrintPageRange
  [WinRTClassNameAttribute(SWindows_Graphics_Printing_PrintPageRange)]
  Printing_IPrintPageRange = interface(IInspectable)
  ['{F8A06C54-6E7C-51C5-57FD-0660C2D71513}']
    function get_FirstPageNumber: Integer; safecall;
    function get_LastPageNumber: Integer; safecall;
    property FirstPageNumber: Integer read get_FirstPageNumber;
    property LastPageNumber: Integer read get_LastPageNumber;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing.IPrintPageRangeFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Printing_PrintPageRange)]
  Printing_IPrintPageRangeFactory = interface(IInspectable)
  ['{408FD45F-E047-5F85-7129-FB085A4FAD14}']
    function Create(firstPage: Integer; lastPage: Integer): Printing_IPrintPageRange; safecall;
    function CreateWithSinglePage(page: Integer): Printing_IPrintPageRange; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing.IPrintPageRange>
  IIterator_1__Printing_IPrintPageRange = interface(IInspectable)
  ['{06F58712-ACBE-5187-9D07-ECBC5EE7BAB7}']
    function get_Current: Printing_IPrintPageRange; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting_IPrintPageRange): Cardinal; safecall;
    property Current: Printing_IPrintPageRange read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing.IPrintPageRange>
  IIterable_1__Printing_IPrintPageRange = interface(IInspectable)
  ['{23BB7320-7E02-54D2-B71B-8C1672A87C23}']
    function First: IIterator_1__Printing_IPrintPageRange; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing.IPrintPageRange>
  IVectorView_1__Printing_IPrintPageRange = interface(IInspectable)
  ['{23D27B04-EB95-5CFE-9C5D-04DF158854C9}']
    function GetAt(index: Cardinal): Printing_IPrintPageRange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing_IPrintPageRange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting_IPrintPageRange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing.IPrintPageRange>
  IVector_1__Printing_IPrintPageRange = interface(IInspectable)
  ['{372099D9-D529-525A-B7E1-B77D7FF23DDB}']
    function GetAt(index: Cardinal): Printing_IPrintPageRange; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing_IPrintPageRange; safecall;
    function IndexOf(value: Printing_IPrintPageRange; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing_IPrintPageRange); safecall;
    procedure InsertAt(index: Cardinal; value: Printing_IPrintPageRange); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing_IPrintPageRange); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting_IPrintPageRange): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting_IPrintPageRange); safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic
  [WinRTClassNameAttribute(SWindows_Graphics_Printing_StandardPrintTaskOptions)]
  Printing_IStandardPrintTaskOptionsStatic = interface(IInspectable)
  ['{B4483D26-0DD0-4CD4-BAFF-930FC7D6A574}']
    function get_MediaSize: HSTRING; safecall;
    function get_MediaType: HSTRING; safecall;
    function get_Orientation: HSTRING; safecall;
    function get_PrintQuality: HSTRING; safecall;
    function get_ColorMode: HSTRING; safecall;
    function get_Duplex: HSTRING; safecall;
    function get_Collation: HSTRING; safecall;
    function get_Staple: HSTRING; safecall;
    function get_HolePunch: HSTRING; safecall;
    function get_Binding: HSTRING; safecall;
    function get_Copies: HSTRING; safecall;
    function get_NUp: HSTRING; safecall;
    function get_InputBin: HSTRING; safecall;
    property Binding: HSTRING read get_Binding;
    property Collation: HSTRING read get_Collation;
    property ColorMode: HSTRING read get_ColorMode;
    property Copies: HSTRING read get_Copies;
    property Duplex: HSTRING read get_Duplex;
    property HolePunch: HSTRING read get_HolePunch;
    property InputBin: HSTRING read get_InputBin;
    property MediaSize: HSTRING read get_MediaSize;
    property MediaType: HSTRING read get_MediaType;
    property NUp: HSTRING read get_NUp;
    property Orientation: HSTRING read get_Orientation;
    property PrintQuality: HSTRING read get_PrintQuality;
    property Staple: HSTRING read get_Staple;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic2
  [WinRTClassNameAttribute(SWindows_Graphics_Printing_StandardPrintTaskOptions)]
  Printing_IStandardPrintTaskOptionsStatic2 = interface(IInspectable)
  ['{3BE38BF4-7A44-4269-9A52-81261E289EE9}']
    function get_Bordering: HSTRING; safecall;
    property Bordering: HSTRING read get_Bordering;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic3
  [WinRTClassNameAttribute(SWindows_Graphics_Printing_StandardPrintTaskOptions)]
  Printing_IStandardPrintTaskOptionsStatic3 = interface(IInspectable)
  ['{BBF68E86-3858-41B3-A799-55DD9888D475}']
    function get_CustomPageRanges: HSTRING; safecall;
    property CustomPageRanges: HSTRING read get_CustomPageRanges;
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

  // Windows.Foundation.Collections.IVectorView`1<Object>
  IVectorView_1__IInspectable = interface(IInspectable)
  ['{A6487363-B074-5C60-AB16-866DCE4EE54D}']
    function GetAt(index: Cardinal): IInspectable; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IInspectable; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIInspectable): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Graphics.Printing.OptionDetails.IPrintTextOptionDetails
  Printing_OptionDetails_IPrintTextOptionDetails = interface(IInspectable)
  ['{AD75E563-5CE4-46BC-9918-AB9FAD144C5B}']
    function get_MaxCharacters: Cardinal; safecall;
    property MaxCharacters: Cardinal read get_MaxCharacters;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface(IUnknown)
  ['{359B8887-2FA6-5405-A4AF-642C9FDACC93}']
    procedure Invoke(sender: Activation_ISplashScreen; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DTextureResource
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DTextureResource)]
  Printing3D_IPrinting3DTextureResource = interface(IInspectable)
  ['{A70DF32D-6AB1-44AE-BC45-A27382C0D38C}']
    function get_TextureData: IRandomAccessStreamWithContentType; safecall;
    procedure put_TextureData(value: IRandomAccessStreamWithContentType); safecall;
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    property Name: HSTRING read get_Name write put_Name;
    property TextureData: IRandomAccessStreamWithContentType read get_TextureData write put_TextureData;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IIterator_1__Printing3D_IPrinting3DTextureResource_Base = interface(IInspectable)
  ['{0678D5DB-8FCA-5084-A851-7312FE53F735}']
    function get_Current: Printing3D_IPrinting3DTextureResource; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTextureResource): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DTextureResource read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IIterator_1__Printing3D_IPrinting3DTextureResource = interface(IIterator_1__Printing3D_IPrinting3DTextureResource_Base)
  ['{F1C54B31-8237-59FE-9DAF-1611C831299A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IIterable_1__Printing3D_IPrinting3DTextureResource_Base = interface(IInspectable)
  ['{54E3A71D-EAE0-5199-9728-FAC964850EBB}']
    function First: IIterator_1__Printing3D_IPrinting3DTextureResource; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IIterable_1__Printing3D_IPrinting3DTextureResource = interface(IIterable_1__Printing3D_IPrinting3DTextureResource_Base)
  ['{B82D7C82-F89C-5F53-B39E-4399A6BEF6AB}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IVectorView_1__Printing3D_IPrinting3DTextureResource = interface(IInspectable)
  ['{982A938E-5678-5378-87AB-8A35B5F31220}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DTextureResource; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DTextureResource; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTextureResource): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IVector_1__Printing3D_IPrinting3DTextureResource_Base = interface(IInspectable)
  ['{120948C9-AAA5-5EE5-A133-3215D0561404}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DTextureResource; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DTextureResource; safecall;
    function IndexOf(value: Printing3D_IPrinting3DTextureResource; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DTextureResource); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DTextureResource); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DTextureResource); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTextureResource): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTextureResource); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTextureResource>
  IVector_1__Printing3D_IPrinting3DTextureResource = interface(IVector_1__Printing3D_IPrinting3DTextureResource_Base)
  ['{8A343BBE-54C9-510C-96EA-C105A15B7CBD}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DModelTexture
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DModelTexture)]
  Printing3D_IPrinting3DModelTexture = interface(IInspectable)
  ['{5DAFCF01-B59D-483C-97BB-A4D546D1C75C}']
    function get_TextureResource: Printing3D_IPrinting3DTextureResource; safecall;
    procedure put_TextureResource(value: Printing3D_IPrinting3DTextureResource); safecall;
    function get_TileStyleU: Printing3D_Printing3DTextureEdgeBehavior; safecall;
    procedure put_TileStyleU(value: Printing3D_Printing3DTextureEdgeBehavior); safecall;
    function get_TileStyleV: Printing3D_Printing3DTextureEdgeBehavior; safecall;
    procedure put_TileStyleV(value: Printing3D_Printing3DTextureEdgeBehavior); safecall;
    property TextureResource: Printing3D_IPrinting3DTextureResource read get_TextureResource write put_TextureResource;
    property TileStyleU: Printing3D_Printing3DTextureEdgeBehavior read get_TileStyleU write put_TileStyleU;
    property TileStyleV: Printing3D_Printing3DTextureEdgeBehavior read get_TileStyleV write put_TileStyleV;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IIterator_1__Printing3D_IPrinting3DModelTexture_Base = interface(IInspectable)
  ['{3D473CA2-4A8C-5CBD-807F-49AF1580D2BA}']
    function get_Current: Printing3D_IPrinting3DModelTexture; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DModelTexture): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DModelTexture read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IIterator_1__Printing3D_IPrinting3DModelTexture = interface(IIterator_1__Printing3D_IPrinting3DModelTexture_Base)
  ['{80E1B7C4-0355-595C-9703-E796B91DBE8A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IIterable_1__Printing3D_IPrinting3DModelTexture_Base = interface(IInspectable)
  ['{94790870-6041-5D04-8699-17417117BB85}']
    function First: IIterator_1__Printing3D_IPrinting3DModelTexture; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IIterable_1__Printing3D_IPrinting3DModelTexture = interface(IIterable_1__Printing3D_IPrinting3DModelTexture_Base)
  ['{B7B865E8-DEAD-5A1C-810F-A2C8789043FB}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IVectorView_1__Printing3D_IPrinting3DModelTexture = interface(IInspectable)
  ['{DD4FA3CC-A965-51D3-9046-AF58EE17584A}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DModelTexture; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DModelTexture; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DModelTexture): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IVector_1__Printing3D_IPrinting3DModelTexture_Base = interface(IInspectable)
  ['{4E72578F-9BEA-5663-8699-E7FCAD3547A7}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DModelTexture; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DModelTexture; safecall;
    function IndexOf(value: Printing3D_IPrinting3DModelTexture; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DModelTexture); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DModelTexture); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DModelTexture); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DModelTexture): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DModelTexture); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DModelTexture>
  IVector_1__Printing3D_IPrinting3DModelTexture = interface(IVector_1__Printing3D_IPrinting3DModelTexture_Base)
  ['{26F8F853-4490-5DD2-B398-36E5783124FF}']
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

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult
  Printing3D_IPrinting3DMeshVerificationResult = interface(IInspectable)
  ['{195671BA-E93A-4E8A-A46F-DEA8E852197E}']
    function get_IsValid: Boolean; safecall;
    function get_NonmanifoldTriangles: IVectorView_1__Cardinal; safecall;
    function get_ReversedNormalTriangles: IVectorView_1__Cardinal; safecall;
    property IsValid: Boolean read get_IsValid;
    property NonmanifoldTriangles: IVectorView_1__Cardinal read get_NonmanifoldTriangles;
    property ReversedNormalTriangles: IVectorView_1__Cardinal read get_ReversedNormalTriangles;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult_Delegate_Base = interface(IUnknown)
  ['{186BAE17-5896-56DE-BFF4-4F176B3E6194}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult = interface(AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult_Delegate_Base)
  ['{CAB9430B-0AB4-56B9-81FF-7B276D015760}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult>
  IAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult_Base = interface(IInspectable)
  ['{0F9EB6C4-19F5-5BE9-9ADB-64F24AF115D8}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult; safecall;
    function GetResults: Printing3D_IPrinting3DMeshVerificationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DMeshVerificationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3DMeshVerificationResult>
  IAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult = interface(IAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult_Base)
  ['{810CF25C-2BEF-5233-BD26-57CE9FD5652E}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DMesh
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DMesh)]
  Printing3D_IPrinting3DMesh = interface(IInspectable)
  ['{192E90DC-0228-2E01-BC20-C5290CBF32C4}']
    function get_VertexCount: Cardinal; safecall;
    procedure put_VertexCount(value: Cardinal); safecall;
    function get_IndexCount: Cardinal; safecall;
    procedure put_IndexCount(value: Cardinal); safecall;
    function get_VertexPositionsDescription: Printing3D_Printing3DBufferDescription; safecall;
    procedure put_VertexPositionsDescription(value: Printing3D_Printing3DBufferDescription); safecall;
    function get_VertexNormalsDescription: Printing3D_Printing3DBufferDescription; safecall;
    procedure put_VertexNormalsDescription(value: Printing3D_Printing3DBufferDescription); safecall;
    function get_TriangleIndicesDescription: Printing3D_Printing3DBufferDescription; safecall;
    procedure put_TriangleIndicesDescription(value: Printing3D_Printing3DBufferDescription); safecall;
    function get_TriangleMaterialIndicesDescription: Printing3D_Printing3DBufferDescription; safecall;
    procedure put_TriangleMaterialIndicesDescription(value: Printing3D_Printing3DBufferDescription); safecall;
    function GetVertexPositions: IBuffer; safecall;
    procedure CreateVertexPositions(value: Cardinal); safecall;
    function GetVertexNormals: IBuffer; safecall;
    procedure CreateVertexNormals(value: Cardinal); safecall;
    function GetTriangleIndices: IBuffer; safecall;
    procedure CreateTriangleIndices(value: Cardinal); safecall;
    function GetTriangleMaterialIndices: IBuffer; safecall;
    procedure CreateTriangleMaterialIndices(value: Cardinal); safecall;
    function get_BufferDescriptionSet: IPropertySet; safecall;
    function get_BufferSet: IPropertySet; safecall;
    function VerifyAsync(value: Printing3D_Printing3DMeshVerificationMode): IAsyncOperation_1__Printing3D_IPrinting3DMeshVerificationResult; safecall;
    property BufferDescriptionSet: IPropertySet read get_BufferDescriptionSet;
    property BufferSet: IPropertySet read get_BufferSet;
    property IndexCount: Cardinal read get_IndexCount write put_IndexCount;
    property TriangleIndicesDescription: Printing3D_Printing3DBufferDescription read get_TriangleIndicesDescription write put_TriangleIndicesDescription;
    property TriangleMaterialIndicesDescription: Printing3D_Printing3DBufferDescription read get_TriangleMaterialIndicesDescription write put_TriangleMaterialIndicesDescription;
    property VertexCount: Cardinal read get_VertexCount write put_VertexCount;
    property VertexNormalsDescription: Printing3D_Printing3DBufferDescription read get_VertexNormalsDescription write put_VertexNormalsDescription;
    property VertexPositionsDescription: Printing3D_Printing3DBufferDescription read get_VertexPositionsDescription write put_VertexPositionsDescription;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IIterator_1__Printing3D_IPrinting3DMesh_Base = interface(IInspectable)
  ['{16DDF132-F80D-53B2-B09F-A42ED9689FC4}']
    function get_Current: Printing3D_IPrinting3DMesh; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMesh): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DMesh read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IIterator_1__Printing3D_IPrinting3DMesh = interface(IIterator_1__Printing3D_IPrinting3DMesh_Base)
  ['{E5AFC4A4-FBF2-5B6E-86D9-7947F0ACA0D7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IIterable_1__Printing3D_IPrinting3DMesh_Base = interface(IInspectable)
  ['{A8018FDA-DE4D-56FA-8609-FD2298BFB558}']
    function First: IIterator_1__Printing3D_IPrinting3DMesh; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IIterable_1__Printing3D_IPrinting3DMesh = interface(IIterable_1__Printing3D_IPrinting3DMesh_Base)
  ['{52F56826-8227-54C4-9830-117CE741F98B}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IVectorView_1__Printing3D_IPrinting3DMesh = interface(IInspectable)
  ['{98D57DB9-CE52-5440-B55B-4CD489403B5C}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DMesh; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DMesh; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMesh): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IVector_1__Printing3D_IPrinting3DMesh_Base = interface(IInspectable)
  ['{BB11BE6E-B592-5BC2-9A53-0127A9B32172}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DMesh; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DMesh; safecall;
    function IndexOf(value: Printing3D_IPrinting3DMesh; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DMesh); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DMesh); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DMesh); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMesh): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMesh); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMesh>
  IVector_1__Printing3D_IPrinting3DMesh = interface(IVector_1__Printing3D_IPrinting3DMesh_Base)
  ['{DEFEF6FF-A965-55A9-AD0A-3AC93191815F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DComponentWithMatrix)]
  Printing3D_IPrinting3DComponentWithMatrix = interface(IInspectable)
  ['{3279F335-0EF0-456B-9A21-49BEBE8B51C2}']
    function get_Component: Printing3D_IPrinting3DComponent; safecall;
    procedure put_Component(value: Printing3D_IPrinting3DComponent); safecall;
    function get_Matrix: Numerics_Matrix4x4; safecall;
    procedure put_Matrix(value: Numerics_Matrix4x4); safecall;
    property Component: Printing3D_IPrinting3DComponent read get_Component write put_Component;
    property Matrix: Numerics_Matrix4x4 read get_Matrix write put_Matrix;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IIterator_1__Printing3D_IPrinting3DComponentWithMatrix_Base = interface(IInspectable)
  ['{01D2CE44-8B63-571F-B92E-BF2CF7CC6D53}']
    function get_Current: Printing3D_IPrinting3DComponentWithMatrix; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponentWithMatrix): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DComponentWithMatrix read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IIterator_1__Printing3D_IPrinting3DComponentWithMatrix = interface(IIterator_1__Printing3D_IPrinting3DComponentWithMatrix_Base)
  ['{CC0FF882-740D-59A6-9456-3F61015E0CC0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IIterable_1__Printing3D_IPrinting3DComponentWithMatrix_Base = interface(IInspectable)
  ['{8A213648-0B81-5E23-A48E-AFE9F6691CC1}']
    function First: IIterator_1__Printing3D_IPrinting3DComponentWithMatrix; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IIterable_1__Printing3D_IPrinting3DComponentWithMatrix = interface(IIterable_1__Printing3D_IPrinting3DComponentWithMatrix_Base)
  ['{58521B5C-FAA5-5F45-9FDB-04E4DD1A62E5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IVectorView_1__Printing3D_IPrinting3DComponentWithMatrix = interface(IInspectable)
  ['{91544CF0-1B5B-5B15-81FC-067459089B9C}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DComponentWithMatrix; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DComponentWithMatrix; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponentWithMatrix): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IVector_1__Printing3D_IPrinting3DComponentWithMatrix_Base = interface(IInspectable)
  ['{F09F3DD7-61E6-5A8D-9DDF-57001F705DE7}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DComponentWithMatrix; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DComponentWithMatrix; safecall;
    function IndexOf(value: Printing3D_IPrinting3DComponentWithMatrix; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DComponentWithMatrix); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DComponentWithMatrix); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DComponentWithMatrix); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponentWithMatrix): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponentWithMatrix); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix>
  IVector_1__Printing3D_IPrinting3DComponentWithMatrix = interface(IVector_1__Printing3D_IPrinting3DComponentWithMatrix_Base)
  ['{7E199659-C85A-5AC9-AE7D-FC5C026EBD44}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DComponent
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DComponent)]
  Printing3D_IPrinting3DComponent = interface(IInspectable)
  ['{7E287845-BF7F-4CDB-A27F-30A01437FEDE}']
    function get_Mesh: Printing3D_IPrinting3DMesh; safecall;
    procedure put_Mesh(value: Printing3D_IPrinting3DMesh); safecall;
    function get_Components: IVector_1__Printing3D_IPrinting3DComponentWithMatrix; safecall;
    function get_Thumbnail: Printing3D_IPrinting3DTextureResource; safecall;
    procedure put_Thumbnail(value: Printing3D_IPrinting3DTextureResource); safecall;
    function get_Type: Printing3D_Printing3DObjectType; safecall;
    procedure put_Type(value: Printing3D_Printing3DObjectType); safecall;
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    function get_PartNumber: HSTRING; safecall;
    procedure put_PartNumber(value: HSTRING); safecall;
    property Components: IVector_1__Printing3D_IPrinting3DComponentWithMatrix read get_Components;
    property Mesh: Printing3D_IPrinting3DMesh read get_Mesh write put_Mesh;
    property Name: HSTRING read get_Name write put_Name;
    property PartNumber: HSTRING read get_PartNumber write put_PartNumber;
    property Thumbnail: Printing3D_IPrinting3DTextureResource read get_Thumbnail write put_Thumbnail;
    property &Type: Printing3D_Printing3DObjectType read get_Type write put_Type;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IIterator_1__Printing3D_IPrinting3DComponent_Base = interface(IInspectable)
  ['{2E9EABBA-184B-5C14-AE5F-EB634AA717E0}']
    function get_Current: Printing3D_IPrinting3DComponent; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponent): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DComponent read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IIterator_1__Printing3D_IPrinting3DComponent = interface(IIterator_1__Printing3D_IPrinting3DComponent_Base)
  ['{CDB6492A-B2EE-5707-BB49-F5E3DE91AECD}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IIterable_1__Printing3D_IPrinting3DComponent_Base = interface(IInspectable)
  ['{516556CA-F862-59F8-8241-E0F0C177DADD}']
    function First: IIterator_1__Printing3D_IPrinting3DComponent; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IIterable_1__Printing3D_IPrinting3DComponent = interface(IIterable_1__Printing3D_IPrinting3DComponent_Base)
  ['{974F07E1-E491-5006-BB68-3616409A7F8D}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IVectorView_1__Printing3D_IPrinting3DComponent = interface(IInspectable)
  ['{1CB57292-8550-5F87-BB54-1FC2A7199F19}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DComponent; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DComponent; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponent): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IVector_1__Printing3D_IPrinting3DComponent_Base = interface(IInspectable)
  ['{49E654C2-F372-582E-97CC-CB6B0FA3BA62}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DComponent; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DComponent; safecall;
    function IndexOf(value: Printing3D_IPrinting3DComponent; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DComponent); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DComponent); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DComponent); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponent): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DComponent); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DComponent>
  IVector_1__Printing3D_IPrinting3DComponent = interface(IVector_1__Printing3D_IPrinting3DComponent_Base)
  ['{A5E70DA4-FA6B-5E7B-A1AC-398FB8B7251C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DColorMaterial
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DColorMaterial)]
  Printing3D_IPrinting3DColorMaterial = interface(IInspectable)
  ['{E1899928-7CE7-4285-A35D-F145C9510C7B}']
    function get_Value: Cardinal; safecall;
    procedure put_Value(value: Cardinal); safecall;
    property Value: Cardinal read get_Value write put_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterial
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DBaseMaterial)]
  Printing3D_IPrinting3DBaseMaterial = interface(IInspectable)
  ['{D0F0E743-C50C-4BCB-9D04-FC16ADCEA2C9}']
    function get_Name: HSTRING; safecall;
    procedure put_Name(value: HSTRING); safecall;
    function get_Color: Printing3D_IPrinting3DColorMaterial; safecall;
    procedure put_Color(value: Printing3D_IPrinting3DColorMaterial); safecall;
    property Color: Printing3D_IPrinting3DColorMaterial read get_Color write put_Color;
    property Name: HSTRING read get_Name write put_Name;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IIterator_1__Printing3D_IPrinting3DBaseMaterial_Base = interface(IInspectable)
  ['{DAD4DD0D-59AB-501F-9D6B-A209C7D54649}']
    function get_Current: Printing3D_IPrinting3DBaseMaterial; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterial): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DBaseMaterial read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IIterator_1__Printing3D_IPrinting3DBaseMaterial = interface(IIterator_1__Printing3D_IPrinting3DBaseMaterial_Base)
  ['{554183C9-C06B-52F8-9312-DF17E5FC59B0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IIterable_1__Printing3D_IPrinting3DBaseMaterial_Base = interface(IInspectable)
  ['{9A6BD130-6F22-559C-B92C-14F9F8DDDA47}']
    function First: IIterator_1__Printing3D_IPrinting3DBaseMaterial; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IIterable_1__Printing3D_IPrinting3DBaseMaterial = interface(IIterable_1__Printing3D_IPrinting3DBaseMaterial_Base)
  ['{C5D08812-990E-5B4C-B9FE-CC64C2BA1E42}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IVectorView_1__Printing3D_IPrinting3DBaseMaterial = interface(IInspectable)
  ['{CD2E2B52-D8D5-5138-9958-A9434F1BEE9D}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DBaseMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DBaseMaterial; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterial): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IVector_1__Printing3D_IPrinting3DBaseMaterial_Base = interface(IInspectable)
  ['{6A5AA59F-FE10-517B-B1A9-C685ECCE1644}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DBaseMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DBaseMaterial; safecall;
    function IndexOf(value: Printing3D_IPrinting3DBaseMaterial; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DBaseMaterial); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DBaseMaterial); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DBaseMaterial); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterial): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterial); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterial>
  IVector_1__Printing3D_IPrinting3DBaseMaterial = interface(IVector_1__Printing3D_IPrinting3DBaseMaterial_Base)
  ['{2320DA4D-71C2-51A9-923E-596D62E9D50D}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DBaseMaterialGroup)]
  Printing3D_IPrinting3DBaseMaterialGroup = interface(IInspectable)
  ['{94F070B8-2515-4A8D-A1F0-D0FC13D06021}']
    function get_Bases: IVector_1__Printing3D_IPrinting3DBaseMaterial; safecall;
    function get_MaterialGroupId: Cardinal; safecall;
    property Bases: IVector_1__Printing3D_IPrinting3DBaseMaterial read get_Bases;
    property MaterialGroupId: Cardinal read get_MaterialGroupId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DBaseMaterialGroup_Base = interface(IInspectable)
  ['{A34DC709-E2A7-5254-9DC1-CD47E85E2504}']
    function get_Current: Printing3D_IPrinting3DBaseMaterialGroup; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterialGroup): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DBaseMaterialGroup read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DBaseMaterialGroup = interface(IIterator_1__Printing3D_IPrinting3DBaseMaterialGroup_Base)
  ['{56BDF2AE-AACB-5F77-870B-EA3D323D0BF7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DBaseMaterialGroup_Base = interface(IInspectable)
  ['{C08F8E70-F6EF-5469-806A-7CB601DDDB67}']
    function First: IIterator_1__Printing3D_IPrinting3DBaseMaterialGroup; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DBaseMaterialGroup = interface(IIterable_1__Printing3D_IPrinting3DBaseMaterialGroup_Base)
  ['{BD818304-E7A1-5BD6-A3AE-8756A0DB15CF}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DBaseMaterialGroup = interface(IInspectable)
  ['{604A4A76-2482-586A-98E0-817A8FDF8BD3}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DBaseMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DBaseMaterialGroup; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterialGroup): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IVector_1__Printing3D_IPrinting3DBaseMaterialGroup_Base = interface(IInspectable)
  ['{2B80D2CF-5449-5C81-8226-EBFC7D72F579}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DBaseMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DBaseMaterialGroup; safecall;
    function IndexOf(value: Printing3D_IPrinting3DBaseMaterialGroup; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DBaseMaterialGroup); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DBaseMaterialGroup); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DBaseMaterialGroup); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterialGroup): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DBaseMaterialGroup); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup>
  IVector_1__Printing3D_IPrinting3DBaseMaterialGroup = interface(IVector_1__Printing3D_IPrinting3DBaseMaterialGroup_Base)
  ['{BEBC9817-B94D-54ED-97C9-8FD53FE16064}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IIterator_1__Printing3D_IPrinting3DColorMaterial_Base = interface(IInspectable)
  ['{5A54A4A1-4D97-58D3-BDCC-1BF38B438D6D}']
    function get_Current: Printing3D_IPrinting3DColorMaterial; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterial): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DColorMaterial read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IIterator_1__Printing3D_IPrinting3DColorMaterial = interface(IIterator_1__Printing3D_IPrinting3DColorMaterial_Base)
  ['{6FA3C260-9ADD-5402-ACB8-20C6CF151786}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IIterable_1__Printing3D_IPrinting3DColorMaterial_Base = interface(IInspectable)
  ['{C77D4F28-7882-52B4-B3C9-7D58C8836573}']
    function First: IIterator_1__Printing3D_IPrinting3DColorMaterial; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IIterable_1__Printing3D_IPrinting3DColorMaterial = interface(IIterable_1__Printing3D_IPrinting3DColorMaterial_Base)
  ['{18277BF1-9FC4-5AFC-AEAD-75774B96371F}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IVectorView_1__Printing3D_IPrinting3DColorMaterial = interface(IInspectable)
  ['{CBD171C8-A1B5-5F88-AAFD-E5EE72B1306E}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DColorMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DColorMaterial; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterial): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IVector_1__Printing3D_IPrinting3DColorMaterial_Base = interface(IInspectable)
  ['{606166FD-6BF5-53A1-B1AE-C34892EF1663}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DColorMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DColorMaterial; safecall;
    function IndexOf(value: Printing3D_IPrinting3DColorMaterial; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DColorMaterial); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DColorMaterial); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DColorMaterial); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterial): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterial); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterial>
  IVector_1__Printing3D_IPrinting3DColorMaterial = interface(IVector_1__Printing3D_IPrinting3DColorMaterial_Base)
  ['{3EBF04EA-4993-53C0-987A-0D9F1AF74A55}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DColorMaterialGroup)]
  Printing3D_IPrinting3DColorMaterialGroup = interface(IInspectable)
  ['{001A6BD0-AADF-4226-AFE9-F369A0B45004}']
    function get_Colors: IVector_1__Printing3D_IPrinting3DColorMaterial; safecall;
    function get_MaterialGroupId: Cardinal; safecall;
    property Colors: IVector_1__Printing3D_IPrinting3DColorMaterial read get_Colors;
    property MaterialGroupId: Cardinal read get_MaterialGroupId;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DColorMaterialGroup_Base = interface(IInspectable)
  ['{498467BE-DE0E-552B-B24E-8EE25EC9A486}']
    function get_Current: Printing3D_IPrinting3DColorMaterialGroup; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterialGroup): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DColorMaterialGroup read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DColorMaterialGroup = interface(IIterator_1__Printing3D_IPrinting3DColorMaterialGroup_Base)
  ['{A040AFB1-5C30-56C7-8E56-05F4BE542920}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DColorMaterialGroup_Base = interface(IInspectable)
  ['{1BF32A86-26AB-5750-B54C-3BDA67867F8A}']
    function First: IIterator_1__Printing3D_IPrinting3DColorMaterialGroup; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DColorMaterialGroup = interface(IIterable_1__Printing3D_IPrinting3DColorMaterialGroup_Base)
  ['{2AF52DC1-D5C9-5823-85D8-49CF3FEE21C2}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DColorMaterialGroup = interface(IInspectable)
  ['{684C232E-F89D-5DDC-9B93-66D8ECDCD5B2}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DColorMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DColorMaterialGroup; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterialGroup): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IVector_1__Printing3D_IPrinting3DColorMaterialGroup_Base = interface(IInspectable)
  ['{7C8017F3-8365-5AA8-9FD0-A769F26E3FEF}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DColorMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DColorMaterialGroup; safecall;
    function IndexOf(value: Printing3D_IPrinting3DColorMaterialGroup; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DColorMaterialGroup); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DColorMaterialGroup); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DColorMaterialGroup); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterialGroup): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DColorMaterialGroup); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup>
  IVector_1__Printing3D_IPrinting3DColorMaterialGroup = interface(IVector_1__Printing3D_IPrinting3DColorMaterialGroup_Base)
  ['{E9D413D0-5D89-5C11-862B-92CC89C86A71}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DTexture2CoordMaterial)]
  Printing3D_IPrinting3DTexture2CoordMaterial = interface(IInspectable)
  ['{8D844BFB-07E9-4986-9833-8DD3D48C6859}']
    function get_Texture: Printing3D_IPrinting3DModelTexture; safecall;
    procedure put_Texture(value: Printing3D_IPrinting3DModelTexture); safecall;
    function get_U: Double; safecall;
    procedure put_U(value: Double); safecall;
    function get_V: Double; safecall;
    procedure put_V(value: Double); safecall;
    property Texture: Printing3D_IPrinting3DModelTexture read get_Texture write put_Texture;
    property U: Double read get_U write put_U;
    property V: Double read get_V write put_V;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterial_Base = interface(IInspectable)
  ['{72D80D63-3626-5A2F-A579-78E70AA86D46}']
    function get_Current: Printing3D_IPrinting3DTexture2CoordMaterial; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterial): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DTexture2CoordMaterial read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface(IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterial_Base)
  ['{3F41C68D-E03B-5FA8-8B60-551986FDA50C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterial_Base = interface(IInspectable)
  ['{28373276-483C-5BD0-99C7-01BFA04A57D4}']
    function First: IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterial; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface(IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterial_Base)
  ['{123F7245-99B4-5C91-BC24-69817A81724A}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface(IInspectable)
  ['{C75A5592-56A6-5F5E-9811-4467AA7BB799}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DTexture2CoordMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DTexture2CoordMaterial; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterial): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IVector_1__Printing3D_IPrinting3DTexture2CoordMaterial_Base = interface(IInspectable)
  ['{F16FBF2C-C783-5EDF-AD7B-7FB7EACF1501}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DTexture2CoordMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterial; safecall;
    function IndexOf(value: Printing3D_IPrinting3DTexture2CoordMaterial; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DTexture2CoordMaterial); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DTexture2CoordMaterial); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DTexture2CoordMaterial); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterial): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterial); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial>
  IVector_1__Printing3D_IPrinting3DTexture2CoordMaterial = interface(IVector_1__Printing3D_IPrinting3DTexture2CoordMaterial_Base)
  ['{13121B2B-F279-5445-A956-AEFF8032A442}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DTexture2CoordMaterialGroup)]
  Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface(IInspectable)
  ['{627D7CA7-6D90-4FB9-9FC4-9FEFF3DFA892}']
    function get_Texture2Coords: IVector_1__Printing3D_IPrinting3DTexture2CoordMaterial; safecall;
    function get_MaterialGroupId: Cardinal; safecall;
    property MaterialGroupId: Cardinal read get_MaterialGroupId;
    property Texture2Coords: IVector_1__Printing3D_IPrinting3DTexture2CoordMaterial read get_Texture2Coords;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup_Base = interface(IInspectable)
  ['{11EAECC4-6AC1-5697-9BF5-1EF617E1DFEB}']
    function get_Current: Printing3D_IPrinting3DTexture2CoordMaterialGroup; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterialGroup): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DTexture2CoordMaterialGroup read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface(IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup_Base)
  ['{7B8DD78A-E720-5310-AF5A-F507674268C4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup_Base = interface(IInspectable)
  ['{00017A1D-96BC-5C0E-B786-594FB4D077B6}']
    function First: IIterator_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface(IIterable_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup_Base)
  ['{2B58CE32-38EE-59BA-AA73-EA2B807C87E1}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface(IInspectable)
  ['{B02EEF03-B90A-5DCD-9EF3-D2550288765D}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DTexture2CoordMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DTexture2CoordMaterialGroup; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterialGroup): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup_Base = interface(IInspectable)
  ['{7DC68E96-2A62-5E7A-85D5-4864D03591EB}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DTexture2CoordMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup; safecall;
    function IndexOf(value: Printing3D_IPrinting3DTexture2CoordMaterialGroup; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DTexture2CoordMaterialGroup); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DTexture2CoordMaterialGroup); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DTexture2CoordMaterialGroup); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterialGroup): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DTexture2CoordMaterialGroup); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup>
  IVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup = interface(IVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup_Base)
  ['{756A81F3-DD00-51FE-A4EB-3F800462DA68}']
  end;

  // Windows.Foundation.Collections.IIterator`1<Double>
  IIterator_1__Double = interface(IInspectable)
  ['{638A2CF4-F474-5318-9055-141CB909AC4B}']
    function get_Current: Double; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDouble): Cardinal; safecall;
    property Current: Double read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Double>
  IIterable_1__Double = interface(IInspectable)
  ['{C738964E-9C64-5BCE-B5CE-61E9A282EC4A}']
    function First: IIterator_1__Double; safecall;
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
  // Windows.Foundation.Collections.IVector`1<Double>
  IVector_1__Double_Base = interface(IInspectable)
  ['{F452D23C-BF05-5F3E-88E7-D17A6716B911}']
    function GetAt(index: Cardinal): Double; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Double; safecall;
    function IndexOf(value: Double; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Double); safecall;
    procedure InsertAt(index: Cardinal; value: Double); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Double); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDouble): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDouble); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Double>
  [WinRTClassNameAttribute(SMicrosoft_UI_Xaml_Media_DoubleCollection)]
  IVector_1__Double = interface(IVector_1__Double_Base)
  ['{F452D23C-BF05-5F3E-88E7-D17A6716B911}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DCompositeMaterial)]
  Printing3D_IPrinting3DCompositeMaterial = interface(IInspectable)
  ['{462238DD-562E-4F6C-882D-F4D841FD63C7}']
    function get_Values: IVector_1__Double; safecall;
    property Values: IVector_1__Double read get_Values;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IIterator_1__Printing3D_IPrinting3DCompositeMaterial_Base = interface(IInspectable)
  ['{B7E6B17A-A885-5C97-B29E-BF261EB5DAD4}']
    function get_Current: Printing3D_IPrinting3DCompositeMaterial; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterial): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DCompositeMaterial read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IIterator_1__Printing3D_IPrinting3DCompositeMaterial = interface(IIterator_1__Printing3D_IPrinting3DCompositeMaterial_Base)
  ['{927C2496-40DC-56CD-A5B4-EF01B516A485}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IIterable_1__Printing3D_IPrinting3DCompositeMaterial_Base = interface(IInspectable)
  ['{A0AF2623-1B11-53CF-975D-64959386CDD3}']
    function First: IIterator_1__Printing3D_IPrinting3DCompositeMaterial; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IIterable_1__Printing3D_IPrinting3DCompositeMaterial = interface(IIterable_1__Printing3D_IPrinting3DCompositeMaterial_Base)
  ['{FCECFD8A-616D-5166-B72D-EBF4BAC48E6F}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IVectorView_1__Printing3D_IPrinting3DCompositeMaterial = interface(IInspectable)
  ['{70B2D562-E1BE-50EA-9698-8D4298D1B422}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DCompositeMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DCompositeMaterial; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterial): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IVector_1__Printing3D_IPrinting3DCompositeMaterial_Base = interface(IInspectable)
  ['{C3B27A95-5EFC-52C7-B5DE-E82E059A722E}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DCompositeMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DCompositeMaterial; safecall;
    function IndexOf(value: Printing3D_IPrinting3DCompositeMaterial; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DCompositeMaterial); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DCompositeMaterial); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DCompositeMaterial); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterial): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterial); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial>
  IVector_1__Printing3D_IPrinting3DCompositeMaterial = interface(IVector_1__Printing3D_IPrinting3DCompositeMaterial_Base)
  ['{71D196E7-9173-5B99-A01E-1B014A8448EE}']
  end;

  // Windows.Foundation.Collections.IVector`1<UInt32>
  IVector_1__Cardinal = interface(IInspectable)
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DCompositeMaterialGroup)]
  Printing3D_IPrinting3DCompositeMaterialGroup = interface(IInspectable)
  ['{8D946A5B-40F1-496D-A5FB-340A5A678E30}']
    function get_Composites: IVector_1__Printing3D_IPrinting3DCompositeMaterial; safecall;
    function get_MaterialGroupId: Cardinal; safecall;
    function get_MaterialIndices: IVector_1__Cardinal; safecall;
    property Composites: IVector_1__Printing3D_IPrinting3DCompositeMaterial read get_Composites;
    property MaterialGroupId: Cardinal read get_MaterialGroupId;
    property MaterialIndices: IVector_1__Cardinal read get_MaterialIndices;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DCompositeMaterialGroup_Base = interface(IInspectable)
  ['{AF86EEA4-DD9D-5AA9-AEE5-BE3892124742}']
    function get_Current: Printing3D_IPrinting3DCompositeMaterialGroup; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterialGroup): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DCompositeMaterialGroup read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface(IIterator_1__Printing3D_IPrinting3DCompositeMaterialGroup_Base)
  ['{7A3823C5-4890-5A00-B63A-A45265854B94}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DCompositeMaterialGroup_Base = interface(IInspectable)
  ['{F2FFEF61-C254-58C0-8206-B3B3096BE9CB}']
    function First: IIterator_1__Printing3D_IPrinting3DCompositeMaterialGroup; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface(IIterable_1__Printing3D_IPrinting3DCompositeMaterialGroup_Base)
  ['{66D9950E-0510-5A4C-8005-282E56C01361}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface(IInspectable)
  ['{4FF3642A-FE61-5E9A-99D6-8C58679A9FC8}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DCompositeMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DCompositeMaterialGroup; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterialGroup): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IVector_1__Printing3D_IPrinting3DCompositeMaterialGroup_Base = interface(IInspectable)
  ['{1E4CCD78-B6C0-51B1-AB2B-C3422F02C24E}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DCompositeMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DCompositeMaterialGroup; safecall;
    function IndexOf(value: Printing3D_IPrinting3DCompositeMaterialGroup; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DCompositeMaterialGroup); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DCompositeMaterialGroup); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DCompositeMaterialGroup); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterialGroup): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DCompositeMaterialGroup); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup>
  IVector_1__Printing3D_IPrinting3DCompositeMaterialGroup = interface(IVector_1__Printing3D_IPrinting3DCompositeMaterialGroup_Base)
  ['{91048A2E-951A-5523-8F41-E91A093C07BB}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DMultiplePropertyMaterial)]
  Printing3D_IPrinting3DMultiplePropertyMaterial = interface(IInspectable)
  ['{25A6254B-C6E9-484D-A214-A25E5776BA62}']
    function get_MaterialIndices: IVector_1__Cardinal; safecall;
    property MaterialIndices: IVector_1__Cardinal read get_MaterialIndices;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterial_Base = interface(IInspectable)
  ['{614C0A0A-BF75-56AD-A304-B79F60017B83}']
    function get_Current: Printing3D_IPrinting3DMultiplePropertyMaterial; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterial): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DMultiplePropertyMaterial read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface(IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterial_Base)
  ['{E70D5AD3-D569-5AD4-9050-E44FB8F1C41D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterial_Base = interface(IInspectable)
  ['{0DFC274E-AE4D-5BBB-93A8-7DC9F84DDAC3}']
    function First: IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterial; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface(IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterial_Base)
  ['{231E974E-34D5-5C64-B66E-C37FD8F5F606}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface(IInspectable)
  ['{668C7B77-E1D1-5B9C-B13F-8EE900C32BF4}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DMultiplePropertyMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DMultiplePropertyMaterial; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterial): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial_Base = interface(IInspectable)
  ['{E2196DA6-6A29-59A2-9DD6-93062F44BAAD}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DMultiplePropertyMaterial; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterial; safecall;
    function IndexOf(value: Printing3D_IPrinting3DMultiplePropertyMaterial; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DMultiplePropertyMaterial); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DMultiplePropertyMaterial); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DMultiplePropertyMaterial); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterial): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterial); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial>
  IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial = interface(IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial_Base)
  ['{A96C14BD-C789-56C6-90A6-E9EC18EEF181}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DMultiplePropertyMaterialGroup)]
  Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface(IInspectable)
  ['{F0950519-AEB9-4515-A39B-A088FBBB277C}']
    function get_MultipleProperties: IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial; safecall;
    function get_MaterialGroupIndices: IVector_1__Cardinal; safecall;
    function get_MaterialGroupId: Cardinal; safecall;
    property MaterialGroupId: Cardinal read get_MaterialGroupId;
    property MaterialGroupIndices: IVector_1__Cardinal read get_MaterialGroupIndices;
    property MultipleProperties: IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterial read get_MultipleProperties;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup_Base = interface(IInspectable)
  ['{A24BAB9A-D946-5036-B1C9-1C09B793F36C}']
    function get_Current: Printing3D_IPrinting3DMultiplePropertyMaterialGroup; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterialGroup): Cardinal; safecall;
    property Current: Printing3D_IPrinting3DMultiplePropertyMaterialGroup read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface(IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup_Base)
  ['{0007E339-CF16-5EA1-9ECF-72711DFDB933}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup_Base = interface(IInspectable)
  ['{23F7518E-2439-5573-A683-EFCA0C61A8D6}']
    function First: IIterator_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface(IIterable_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup_Base)
  ['{28A64E3D-5859-5FDF-8F7C-214CBD1FC8DD}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface(IInspectable)
  ['{1B1F556D-17F1-523C-8D31-B9747419CD2C}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DMultiplePropertyMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Printing3D_IPrinting3DMultiplePropertyMaterialGroup; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterialGroup): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup_Base = interface(IInspectable)
  ['{22585B94-34A1-5B6A-BAE3-BF44659812F3}']
    function GetAt(index: Cardinal): Printing3D_IPrinting3DMultiplePropertyMaterialGroup; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup; safecall;
    function IndexOf(value: Printing3D_IPrinting3DMultiplePropertyMaterialGroup; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Printing3D_IPrinting3DMultiplePropertyMaterialGroup); safecall;
    procedure InsertAt(index: Cardinal; value: Printing3D_IPrinting3DMultiplePropertyMaterialGroup); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Printing3D_IPrinting3DMultiplePropertyMaterialGroup); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterialGroup): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPrinting3D_IPrinting3DMultiplePropertyMaterialGroup); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup>
  IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup = interface(IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup_Base)
  ['{15874BEC-0852-5189-97BB-3C204648D709}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DMaterial
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DMaterial)]
  Printing3D_IPrinting3DMaterial = interface(IInspectable)
  ['{378DB256-ED62-4952-B85B-03567D7C465E}']
    function get_BaseGroups: IVector_1__Printing3D_IPrinting3DBaseMaterialGroup; safecall;
    function get_ColorGroups: IVector_1__Printing3D_IPrinting3DColorMaterialGroup; safecall;
    function get_Texture2CoordGroups: IVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup; safecall;
    function get_CompositeGroups: IVector_1__Printing3D_IPrinting3DCompositeMaterialGroup; safecall;
    function get_MultiplePropertyGroups: IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup; safecall;
    property BaseGroups: IVector_1__Printing3D_IPrinting3DBaseMaterialGroup read get_BaseGroups;
    property ColorGroups: IVector_1__Printing3D_IPrinting3DColorMaterialGroup read get_ColorGroups;
    property CompositeGroups: IVector_1__Printing3D_IPrinting3DCompositeMaterialGroup read get_CompositeGroups;
    property MultiplePropertyGroups: IVector_1__Printing3D_IPrinting3DMultiplePropertyMaterialGroup read get_MultiplePropertyGroups;
    property Texture2CoordGroups: IVector_1__Printing3D_IPrinting3DTexture2CoordMaterialGroup read get_Texture2CoordGroups;
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DModel
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DModel)]
  Printing3D_IPrinting3DModel = interface(IInspectable)
  ['{2D012EF0-52FB-919A-77B0-4B1A3B80324F}']
    function get_Unit: Printing3D_Printing3DModelUnit; safecall;
    procedure put_Unit(value: Printing3D_Printing3DModelUnit); safecall;
    function get_Textures: IVector_1__Printing3D_IPrinting3DModelTexture; safecall;
    function get_Meshes: IVector_1__Printing3D_IPrinting3DMesh; safecall;
    function get_Components: IVector_1__Printing3D_IPrinting3DComponent; safecall;
    function get_Material: Printing3D_IPrinting3DMaterial; safecall;
    procedure put_Material(value: Printing3D_IPrinting3DMaterial); safecall;
    function get_Build: Printing3D_IPrinting3DComponent; safecall;
    procedure put_Build(value: Printing3D_IPrinting3DComponent); safecall;
    function get_Version: HSTRING; safecall;
    procedure put_Version(value: HSTRING); safecall;
    function get_RequiredExtensions: IVector_1__HSTRING; safecall;
    function get_Metadata: IMap_2__HSTRING__HSTRING; safecall;
    function RepairAsync: IAsyncAction; safecall;
    function Clone: Printing3D_IPrinting3DModel; safecall;
    property Build: Printing3D_IPrinting3DComponent read get_Build write put_Build;
    property Components: IVector_1__Printing3D_IPrinting3DComponent read get_Components;
    property Material: Printing3D_IPrinting3DMaterial read get_Material write put_Material;
    property Meshes: IVector_1__Printing3D_IPrinting3DMesh read get_Meshes;
    property Metadata: IMap_2__HSTRING__HSTRING read get_Metadata;
    property RequiredExtensions: IVector_1__HSTRING read get_RequiredExtensions;
    property Textures: IVector_1__Printing3D_IPrinting3DModelTexture read get_Textures;
    property &Unit: Printing3D_Printing3DModelUnit read get_Unit write put_Unit;
    property Version: HSTRING read get_Version write put_Version;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3DModel>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel_Delegate_Base = interface(IUnknown)
  ['{26F4D34C-A11D-5B09-9908-ADE8B1B13555}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Printing3D_IPrinting3DModel; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3DModel>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel = interface(AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel_Delegate_Base)
  ['{CAE40DF1-F459-5C86-B28B-C5A0FC437AB9}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3DModel>
  IAsyncOperation_1__Printing3D_IPrinting3DModel_Base = interface(IInspectable)
  ['{1B27900B-10D5-53FF-9A34-4B31F31582B0}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel; safecall;
    function GetResults: Printing3D_IPrinting3DModel; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3DModel read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3DModel>
  IAsyncOperation_1__Printing3D_IPrinting3DModel = interface(IAsyncOperation_1__Printing3D_IPrinting3DModel_Base)
  ['{2E87092F-7EA2-5D57-A220-5B53AB96379E}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3D3MFPackage
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3D3MFPackage)]
  Printing3D_IPrinting3D3MFPackage = interface(IInspectable)
  ['{F64DD5C8-2AB7-45A9-A1B7-267E948D5B18}']
    function SaveAsync: IAsyncOperation_1__IRandomAccessStream; safecall;
    function get_PrintTicket: IRandomAccessStream; safecall;
    procedure put_PrintTicket(value: IRandomAccessStream); safecall;
    function get_ModelPart: IRandomAccessStream; safecall;
    procedure put_ModelPart(value: IRandomAccessStream); safecall;
    function get_Thumbnail: Printing3D_IPrinting3DTextureResource; safecall;
    procedure put_Thumbnail(value: Printing3D_IPrinting3DTextureResource); safecall;
    function get_Textures: IVector_1__Printing3D_IPrinting3DTextureResource; safecall;
    function LoadModelFromPackageAsync(value: IRandomAccessStream): IAsyncOperation_1__Printing3D_IPrinting3DModel; safecall;
    function SaveModelToPackageAsync(value: Printing3D_IPrinting3DModel): IAsyncAction; safecall;
    property ModelPart: IRandomAccessStream read get_ModelPart write put_ModelPart;
    property PrintTicket: IRandomAccessStream read get_PrintTicket write put_PrintTicket;
    property Textures: IVector_1__Printing3D_IPrinting3DTextureResource read get_Textures;
    property Thumbnail: Printing3D_IPrinting3DTextureResource read get_Thumbnail write put_Thumbnail;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Object>
  TypedEventHandler_2__Printing3D_IPrint3DTask__IInspectable_Delegate_Base = interface(IUnknown)
  ['{C0081611-7485-58A8-88BE-82E712D8C1BA}']
    procedure Invoke(sender: Printing3D_IPrint3DTask; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Object>
  TypedEventHandler_2__Printing3D_IPrint3DTask__IInspectable = interface(TypedEventHandler_2__Printing3D_IPrint3DTask__IInspectable_Delegate_Base)
  ['{1412284C-867A-5C17-BA5E-5F7BCD2098B5}']
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DTaskCompletedEventArgs
  Printing3D_IPrint3DTaskCompletedEventArgs = interface(IInspectable)
  ['{CC1914AF-2614-4F1D-ACCC-D6FC4FDA5455}']
    function get_Completion: Printing3D_Print3DTaskCompletion; safecall;
    function get_ExtendedStatus: Printing3D_Print3DTaskDetail; safecall;
    property Completion: Printing3D_Print3DTaskCompletion read get_Completion;
    property ExtendedStatus: Printing3D_Print3DTaskDetail read get_ExtendedStatus;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Windows.Graphics.Printing3D.IPrint3DTaskCompletedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{BCCF7095-BC8E-5FF5-83C0-D5691E0AA24D}']
    procedure Invoke(sender: Printing3D_IPrint3DTask; args: Printing3D_IPrint3DTaskCompletedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Windows.Graphics.Printing3D.IPrint3DTaskCompletedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskCompletedEventArgs = interface(TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskCompletedEventArgs_Delegate_Base)
  ['{E32B9E59-D059-58CF-BDAC-30D4BFC78FF5}']
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DTaskSourceChangedEventArgs
  Printing3D_IPrint3DTaskSourceChangedEventArgs = interface(IInspectable)
  ['{5BCD34AF-24E9-4C10-8D07-14C346BA3FCF}']
    function get_Source: Printing3D_IPrinting3D3MFPackage; safecall;
    property Source: Printing3D_IPrinting3D3MFPackage read get_Source;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Windows.Graphics.Printing3D.IPrint3DTaskSourceChangedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskSourceChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{58D36055-0241-555D-AF7B-9F05E5DAA412}']
    procedure Invoke(sender: Printing3D_IPrint3DTask; args: Printing3D_IPrint3DTaskSourceChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DTask,Windows.Graphics.Printing3D.IPrint3DTaskSourceChangedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskSourceChangedEventArgs = interface(TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskSourceChangedEventArgs_Delegate_Base)
  ['{4D259B83-5723-5ADA-95F3-152B37270E22}']
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DTask
  Printing3D_IPrint3DTask = interface(IInspectable)
  ['{8CE3D080-2118-4C28-80DE-F426D70191AE}']
    function get_Source: Printing3D_IPrinting3D3MFPackage; safecall;
    function add_Submitting(eventHandler: TypedEventHandler_2__Printing3D_IPrint3DTask__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Submitting(eventCookie: EventRegistrationToken); safecall;
    function add_Completed(eventHandler: TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Completed(eventCookie: EventRegistrationToken); safecall;
    function add_SourceChanged(eventHandler: TypedEventHandler_2__Printing3D_IPrint3DTask__Printing3D_IPrint3DTaskSourceChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_SourceChanged(eventCookie: EventRegistrationToken); safecall;
    property Source: Printing3D_IPrinting3D3MFPackage read get_Source;
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DTaskSourceRequestedArgs
  Printing3D_IPrint3DTaskSourceRequestedArgs = interface(IInspectable)
  ['{C77C9ABA-24AF-424D-A3BF-92250C355602}']
    procedure SetSource(source: Printing3D_IPrinting3D3MFPackage); safecall;
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.Print3DTaskSourceRequestedHandler
  Printing3D_Print3DTaskSourceRequestedHandler = interface(IUnknown)
  ['{E9175E70-C917-46DE-BB51-D9A94DB3711F}']
    procedure Invoke(args: Printing3D_IPrint3DTaskSourceRequestedArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DTaskRequest
  Printing3D_IPrint3DTaskRequest = interface(IInspectable)
  ['{2595C46F-2245-4C5A-8731-0D604DC6BC3C}']
    function CreateTask(title: HSTRING; printerId: HSTRING; handler: Printing3D_Print3DTaskSourceRequestedHandler): Printing3D_IPrint3DTask; safecall;
  end;

  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DTaskRequestedEventArgs
  Printing3D_IPrint3DTaskRequestedEventArgs = interface(IInspectable)
  ['{150CB77F-18C5-40D7-9F40-FAB3096E05A9}']
    function get_Request: Printing3D_IPrint3DTaskRequest; safecall;
    property Request: Printing3D_IPrint3DTaskRequest read get_Request;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DManager,Windows.Graphics.Printing3D.IPrint3DTaskRequestedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DManager__Printing3D_IPrint3DTaskRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{77C464A3-A7C6-5512-9859-412DB3F66AC4}']
    procedure Invoke(sender: Printing3D_IPrint3DManager; args: Printing3D_IPrint3DTaskRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Graphics.Printing3D.IPrint3DManager,Windows.Graphics.Printing3D.IPrint3DTaskRequestedEventArgs>
  TypedEventHandler_2__Printing3D_IPrint3DManager__Printing3D_IPrint3DTaskRequestedEventArgs = interface(TypedEventHandler_2__Printing3D_IPrint3DManager__Printing3D_IPrint3DTaskRequestedEventArgs_Delegate_Base)
  ['{1EF166C7-7F2F-513A-B9F6-554C6DFE89AD}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DManager
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Print3DManager)]
  Printing3D_IPrint3DManager = interface(IInspectable)
  ['{4D2FCB0A-7366-4971-8BD5-17C4E3E8C6C0}']
    function add_TaskRequested(eventHandler: TypedEventHandler_2__Printing3D_IPrint3DManager__Printing3D_IPrint3DTaskRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_TaskRequested(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrint3DManagerStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Print3DManager)]
  Printing3D_IPrint3DManagerStatics = interface(IInspectable)
  ['{0EF1CAFE-A9AD-4C08-A917-1D1F863EABCB}']
    function GetForCurrentView: Printing3D_IPrint3DManager; safecall;
    function ShowPrintUIAsync: IAsyncOperation_1__Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3D3MFPackage2
  Printing3D_IPrinting3D3MFPackage2 = interface(IInspectable)
  ['{965C7AC4-93CB-4430-92B8-789CD454F883}']
    function get_Compression: Printing3D_Printing3DPackageCompression; safecall;
    procedure put_Compression(value: Printing3D_Printing3DPackageCompression); safecall;
    property Compression: Printing3D_Printing3DPackageCompression read get_Compression write put_Compression;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3D3MFPackage>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage_Delegate_Base = interface(IUnknown)
  ['{28B6B208-85A7-53F1-83AE-577A7DE66A9B}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Graphics.Printing3D.IPrinting3D3MFPackage>
  AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage = interface(AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage_Delegate_Base)
  ['{41D023B3-7055-5679-9BEA-2763B728B8F7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3D3MFPackage>
  IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage_Base = interface(IInspectable)
  ['{6CF2EB38-E068-5558-94B0-0161192C5F19}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage; safecall;
    function GetResults: Printing3D_IPrinting3D3MFPackage; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Printing3D_IPrinting3D3MFPackage read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Graphics.Printing3D.IPrinting3D3MFPackage>
  IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage = interface(IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage_Base)
  ['{E8A14278-C484-5C8E-BEF1-DC025757B5E4}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3D3MFPackageStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3D3MFPackage)]
  Printing3D_IPrinting3D3MFPackageStatics = interface(IInspectable)
  ['{7058D9AF-7A9A-4787-B817-F6F459214823}']
    function LoadAsync(value: IRandomAccessStream): IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroupFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DBaseMaterialGroup)]
  Printing3D_IPrinting3DBaseMaterialGroupFactory = interface(IInspectable)
  ['{5C1546DC-8697-4193-976B-84BB4116E5BF}']
    function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DBaseMaterialGroup; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DBaseMaterialStatics
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DBaseMaterial)]
  Printing3D_IPrinting3DBaseMaterialStatics = interface(IInspectable)
  ['{815A47BC-374A-476D-BE92-3ECFD1CB9776}']
    function get_Abs: HSTRING; safecall;
    function get_Pla: HSTRING; safecall;
    property Abs: HSTRING read get_Abs;
    property Pla: HSTRING read get_Pla;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DColorMaterial2
  Printing3D_IPrinting3DColorMaterial2 = interface(IInspectable)
  ['{FAB0E852-0AEF-44E9-9DDD-36EEEA5ACD44}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    property Color_: Color read get_Color write put_Color;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroupFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DColorMaterialGroup)]
  Printing3D_IPrinting3DColorMaterialGroupFactory = interface(IInspectable)
  ['{71D38D6D-B1EA-4A5B-BC54-19C65F3DF044}']
    function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DColorMaterialGroup; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup2
  Printing3D_IPrinting3DCompositeMaterialGroup2 = interface(IInspectable)
  ['{06E86D62-7D3B-41E1-944C-BAFDE4555483}']
    function get_BaseMaterialGroup: Printing3D_IPrinting3DBaseMaterialGroup; safecall;
    procedure put_BaseMaterialGroup(value: Printing3D_IPrinting3DBaseMaterialGroup); safecall;
    property BaseMaterialGroup: Printing3D_IPrinting3DBaseMaterialGroup read get_BaseMaterialGroup write put_BaseMaterialGroup;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroupFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DCompositeMaterialGroup)]
  Printing3D_IPrinting3DCompositeMaterialGroupFactory = interface(IInspectable)
  ['{D08ECD13-92FF-43AA-A627-8D43C22C817E}']
    function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DCompositeMaterialGroup; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DFaceReductionOptions
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DFaceReductionOptions)]
  Printing3D_IPrinting3DFaceReductionOptions = interface(IInspectable)
  ['{BBFED397-2D74-46F7-BE85-99A67BBB6629}']
    function get_MaxReductionArea: Double; safecall;
    procedure put_MaxReductionArea(value: Double); safecall;
    function get_TargetTriangleCount: Cardinal; safecall;
    procedure put_TargetTriangleCount(value: Cardinal); safecall;
    function get_MaxEdgeLength: Double; safecall;
    procedure put_MaxEdgeLength(value: Double); safecall;
    property MaxEdgeLength: Double read get_MaxEdgeLength write put_MaxEdgeLength;
    property MaxReductionArea: Double read get_MaxReductionArea write put_MaxReductionArea;
    property TargetTriangleCount: Cardinal read get_TargetTriangleCount write put_TargetTriangleCount;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Boolean,Double>
  AsyncOperationProgressHandler_2__Boolean__Double_Delegate_Base = interface(IUnknown)
  ['{CADF3784-1200-5633-8280-163849914AB3}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Boolean__Double; progressInfo: Double); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Boolean,Double>
  AsyncOperationProgressHandler_2__Boolean__Double = interface(AsyncOperationProgressHandler_2__Boolean__Double_Delegate_Base)
  ['{CADF3784-1200-5633-8280-163849914AB3}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Boolean,Double>
  AsyncOperationWithProgressCompletedHandler_2__Boolean__Double_Delegate_Base = interface(IUnknown)
  ['{0EC5345B-B37A-5CD5-83D7-9590CDF445B5}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Boolean__Double; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Boolean,Double>
  AsyncOperationWithProgressCompletedHandler_2__Boolean__Double = interface(AsyncOperationWithProgressCompletedHandler_2__Boolean__Double_Delegate_Base)
  ['{0EC5345B-B37A-5CD5-83D7-9590CDF445B5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Boolean,Double>
  IAsyncOperationWithProgress_2__Boolean__Double_Base = interface(IInspectable)
  ['{AF873C66-2DF0-5A95-AB54-25634DA3FFA9}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__Boolean__Double); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__Boolean__Double; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__Boolean__Double); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__Boolean__Double; safecall;
    function GetResults: Boolean; safecall;
    property Progress: AsyncOperationProgressHandler_2__Boolean__Double read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__Boolean__Double read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Boolean,Double>
  IAsyncOperationWithProgress_2__Boolean__Double = interface(IAsyncOperationWithProgress_2__Boolean__Double_Base)
  ['{AF873C66-2DF0-5A95-AB54-25634DA3FFA9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DModel2
  Printing3D_IPrinting3DModel2 = interface(IInspectable)
  ['{C92069C7-C841-47F3-A84E-A149FD08B657}']
    function TryPartialRepairAsync: IAsyncOperation_1__Boolean; overload; safecall;
    function TryPartialRepairAsync(maxWaitTime: TimeSpan): IAsyncOperation_1__Boolean; overload; safecall;
    function TryReduceFacesAsync: IAsyncOperationWithProgress_2__Boolean__Double; overload; safecall;
    function TryReduceFacesAsync(printing3DFaceReductionOptions: Printing3D_IPrinting3DFaceReductionOptions): IAsyncOperationWithProgress_2__Boolean__Double; overload; safecall;
    function TryReduceFacesAsync(printing3DFaceReductionOptions: Printing3D_IPrinting3DFaceReductionOptions; maxWait: TimeSpan): IAsyncOperationWithProgress_2__Boolean__Double; overload; safecall;
    function RepairWithProgressAsync: IAsyncOperationWithProgress_2__Boolean__Double; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroupFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DMultiplePropertyMaterialGroup)]
  Printing3D_IPrinting3DMultiplePropertyMaterialGroupFactory = interface(IInspectable)
  ['{323E196E-D4C6-451E-A814-4D78A210FE53}']
    function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DMultiplePropertyMaterialGroup; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup2
  Printing3D_IPrinting3DTexture2CoordMaterialGroup2 = interface(IInspectable)
  ['{69FBDBBA-B12E-429B-8386-DF5284F6E80F}']
    function get_Texture: Printing3D_IPrinting3DModelTexture; safecall;
    procedure put_Texture(value: Printing3D_IPrinting3DModelTexture); safecall;
    property Texture: Printing3D_IPrinting3DModelTexture read get_Texture write put_Texture;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroupFactory
  [WinRTClassNameAttribute(SWindows_Graphics_Printing3D_Printing3DTexture2CoordMaterialGroup)]
  Printing3D_IPrinting3DTexture2CoordMaterialGroupFactory = interface(IInspectable)
  ['{CBB049B0-468A-4C6F-B2A2-8EB8BA8DEA48}']
    function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DTexture2CoordMaterialGroup; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IIterator_1__Imaging_BitmapPixelFormat_Base = interface(IInspectable)
  ['{7FC2E293-1084-5D45-B8B8-93E10692BCC8}']
    function get_Current: Imaging_BitmapPixelFormat; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PImaging_BitmapPixelFormat): Cardinal; safecall;
    property Current: Imaging_BitmapPixelFormat read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IIterator_1__Imaging_BitmapPixelFormat = interface(IIterator_1__Imaging_BitmapPixelFormat_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IIterable_1__Imaging_BitmapPixelFormat_Base = interface(IInspectable)
  ['{E924D9ED-A13E-5BDB-9ED8-65A1474DC274}']
    function First: IIterator_1__Imaging_BitmapPixelFormat; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IIterable_1__Imaging_BitmapPixelFormat = interface(IIterable_1__Imaging_BitmapPixelFormat_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.Imaging.BitmapPixelFormat>
  IVectorView_1__Imaging_BitmapPixelFormat = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): Imaging_BitmapPixelFormat; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Imaging_BitmapPixelFormat; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PImaging_BitmapPixelFormat): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IIterator_1__Imaging_ISoftwareBitmap_Base = interface(IInspectable)
  ['{CD12E4C3-8CA8-5BE6-B64B-204A014FC620}']
    function get_Current: Imaging_ISoftwareBitmap; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PImaging_ISoftwareBitmap): Cardinal; safecall;
    property Current: Imaging_ISoftwareBitmap read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IIterator_1__Imaging_ISoftwareBitmap = interface(IIterator_1__Imaging_ISoftwareBitmap_Base)
  ['{5D992AEB-ADA2-5B25-8E47-CB5A9DCCCA27}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IIterable_1__Imaging_ISoftwareBitmap_Base = interface(IInspectable)
  ['{22D3A30F-0898-5E94-99A3-AFA5951DFCD4}']
    function First: IIterator_1__Imaging_ISoftwareBitmap; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.Imaging.ISoftwareBitmap>
  IIterable_1__Imaging_ISoftwareBitmap = interface(IIterable_1__Imaging_ISoftwareBitmap_Base)
  ['{54E17562-ACE7-5210-B02C-C6A6679A0806}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IIterator_1__DirectX_Direct3D11_IDirect3DSurface_Base = interface(IInspectable)
  ['{BDFB6D0B-E785-5D5A-ABD2-FE1B18C43257}']
    function get_Current: DirectX_Direct3D11_IDirect3DSurface; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDirectX_Direct3D11_IDirect3DSurface): Cardinal; safecall;
    property Current: DirectX_Direct3D11_IDirect3DSurface read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IIterator_1__DirectX_Direct3D11_IDirect3DSurface = interface(IIterator_1__DirectX_Direct3D11_IDirect3DSurface_Base)
  ['{BDFB6D0B-E785-5D5A-ABD2-FE1B18C43257}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IIterable_1__DirectX_Direct3D11_IDirect3DSurface_Base = interface(IInspectable)
  ['{CC63BF9C-E16A-5A75-A5AA-2B53F975B0B0}']
    function First: IIterator_1__DirectX_Direct3D11_IDirect3DSurface; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IIterable_1__DirectX_Direct3D11_IDirect3DSurface = interface(IIterable_1__DirectX_Direct3D11_IDirect3DSurface_Base)
  ['{CC63BF9C-E16A-5A75-A5AA-2B53F975B0B0}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Graphics.DirectX.Direct3D11.IDirect3DSurface>
  IVectorView_1__DirectX_Direct3D11_IDirect3DSurface = interface(IInspectable)
  ['{1A81EC3E-5AFB-5E10-92BB-C843FEC70887}']
    function GetAt(index: Cardinal): DirectX_Direct3D11_IDirect3DSurface; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: DirectX_Direct3D11_IDirect3DSurface; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDirectX_Direct3D11_IDirect3DSurface): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Graphics.Imaging.BitmapBounds>
  IReference_1__Imaging_BitmapBounds = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Imaging_BitmapBounds; safecall;
    property Value: Imaging_BitmapBounds read get_Value;
  end;

  // Windows.Graphics.Capture.Direct3D11CaptureFramePool
  // DualAPI
  // Implements: Windows.Graphics.Capture.IDirect3D11CaptureFramePool
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Graphics.Capture.IDirect3D11CaptureFramePoolStatics"
  // Statics: "Windows.Graphics.Capture.IDirect3D11CaptureFramePoolStatics2"
  TCapture_Direct3D11CaptureFramePool = class(TWinRTGenericImportS2<Capture_IDirect3D11CaptureFramePoolStatics, Capture_IDirect3D11CaptureFramePoolStatics2>)
  public
    // -> Capture_IDirect3D11CaptureFramePoolStatics
    class function Create(device: DirectX_Direct3D11_IDirect3DDevice; pixelFormat: DirectX_DirectXPixelFormat; numberOfBuffers: Integer; size: SizeInt32): Capture_IDirect3D11CaptureFramePool; static; inline;

    // -> Capture_IDirect3D11CaptureFramePoolStatics2
    class function CreateFreeThreaded(device: DirectX_Direct3D11_IDirect3DDevice; pixelFormat: DirectX_DirectXPixelFormat; numberOfBuffers: Integer; size: SizeInt32): Capture_IDirect3D11CaptureFramePool; static; inline;
  end;

  // Windows.Graphics.Capture.GraphicsCaptureItem
  // DualAPI
  // Implements: Windows.Graphics.Capture.IGraphicsCaptureItem
  // Statics: "Windows.Graphics.Capture.IGraphicsCaptureItemStatics"
  TCapture_GraphicsCaptureItem = class(TWinRTGenericImportS<Capture_IGraphicsCaptureItemStatics>)
  public
    // -> Capture_IGraphicsCaptureItemStatics
    class function CreateFromVisual(visual: IVisual): Capture_IGraphicsCaptureItem; static; inline;
  end;

  // Windows.Graphics.Capture.GraphicsCapturePicker
  // DualAPI
  // Implements: Windows.Graphics.Capture.IGraphicsCapturePicker
  // Instantiable: "Capture_IGraphicsCapturePicker"
  TCapture_GraphicsCapturePicker = class(TWinRTGenericImportI<Capture_IGraphicsCapturePicker>) end;

  // Windows.Graphics.Capture.GraphicsCaptureSession
  // DualAPI
  // Implements: Windows.Graphics.Capture.IGraphicsCaptureSession
  // Implements: Windows.Graphics.Capture.IGraphicsCaptureSession2
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Graphics.Capture.IGraphicsCaptureSessionStatics"
  TCapture_GraphicsCaptureSession = class(TWinRTGenericImportS<Capture_IGraphicsCaptureSessionStatics>)
  public
    // -> Capture_IGraphicsCaptureSessionStatics
    class function IsSupported: Boolean; static; inline;
  end;

  // Windows.Graphics.Display.Core.HdmiDisplayInformation
  // DualAPI
  // Implements: Windows.Graphics.Display.Core.IHdmiDisplayInformation
  // Statics: "Windows.Graphics.Display.Core.IHdmiDisplayInformationStatics"
  TDisplay_Core_HdmiDisplayInformation = class(TWinRTGenericImportS<Display_Core_IHdmiDisplayInformationStatics>)
  public
    // -> Display_Core_IHdmiDisplayInformationStatics
    class function GetForCurrentView: Display_Core_IHdmiDisplayInformation; static; inline;
  end;

  // Windows.Graphics.Holographic.HolographicQuadLayer
  // DualAPI
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.Graphics.Holographic.IHolographicQuadLayer
  // Factory: "Windows.Graphics.Holographic.IHolographicQuadLayerFactory"
  THolographic_HolographicQuadLayer = class(TWinRTGenericImportF<Holographic_IHolographicQuadLayerFactory>)
  public
    // -> Holographic_IHolographicQuadLayerFactory
    class function Create(size: TSizeF): IClosable; static; inline;
    class function CreateWithPixelFormat(size: TSizeF; pixelFormat: DirectX_DirectXPixelFormat): IClosable; static; inline;
  end;

  // Windows.Graphics.Imaging.SoftwareBitmap
  // DualAPI
  // Implements: Windows.Graphics.Imaging.ISoftwareBitmap
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Graphics.Imaging.ISoftwareBitmapStatics"
  // Factory: "Windows.Graphics.Imaging.ISoftwareBitmapFactory"
  TImaging_SoftwareBitmap = class(TWinRTGenericImportFS<Imaging_ISoftwareBitmapFactory, Imaging_ISoftwareBitmapStatics>)
  public
    // -> Imaging_ISoftwareBitmapStatics
    class function Copy(source: Imaging_ISoftwareBitmap): Imaging_ISoftwareBitmap; static; inline;
    class function Convert(source: Imaging_ISoftwareBitmap; format: Imaging_BitmapPixelFormat): Imaging_ISoftwareBitmap; overload; static; inline;
    class function Convert(source: Imaging_ISoftwareBitmap; format: Imaging_BitmapPixelFormat; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap; overload; static; inline;
    class function CreateCopyFromBuffer(source: IBuffer; format: Imaging_BitmapPixelFormat; width: Integer; height: Integer): Imaging_ISoftwareBitmap; overload; static; inline;
    class function CreateCopyFromBuffer(source: IBuffer; format: Imaging_BitmapPixelFormat; width: Integer; height: Integer; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap; overload; static; inline;
    class function CreateCopyFromSurfaceAsync(surface: DirectX_Direct3D11_IDirect3DSurface): IAsyncOperation_1__Imaging_ISoftwareBitmap; overload; static; inline;
    class function CreateCopyFromSurfaceAsync(surface: DirectX_Direct3D11_IDirect3DSurface; alpha: Imaging_BitmapAlphaMode): IAsyncOperation_1__Imaging_ISoftwareBitmap; overload; static; inline;

    // -> Imaging_ISoftwareBitmapFactory
    class function Create(format: Imaging_BitmapPixelFormat; width: Integer; height: Integer): Imaging_ISoftwareBitmap; static; inline;
    class function CreateWithAlpha(format: Imaging_BitmapPixelFormat; width: Integer; height: Integer; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap; static; inline;
  end;

  // Windows.Graphics.Printing.PrintPageInfo
  // DualAPI
  // Implements: Windows.Graphics.Printing.IPrintPageInfo
  // Instantiable: "Printing_IPrintPageInfo"
  TPrinting_PrintPageInfo = class(TWinRTGenericImportI<Printing_IPrintPageInfo>) end;

  // Windows.Graphics.Printing.PrintPageRange
  // DualAPI
  // Implements: Windows.Graphics.Printing.IPrintPageRange
  // Factory: "Windows.Graphics.Printing.IPrintPageRangeFactory"
  TPrinting_PrintPageRange = class(TWinRTGenericImportF<Printing_IPrintPageRangeFactory>)
  public
    // -> Printing_IPrintPageRangeFactory
    class function Create(firstPage: Integer; lastPage: Integer): Printing_IPrintPageRange; static; inline;
    class function CreateWithSinglePage(page: Integer): Printing_IPrintPageRange; static; inline;
  end;

  // Windows.Graphics.Printing.StandardPrintTaskOptions
  // DualAPI
  // Statics: "Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic"
  // Statics: "Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic2"
  // Statics: "Windows.Graphics.Printing.IStandardPrintTaskOptionsStatic3"
  TPrinting_StandardPrintTaskOptions = class(TWinRTGenericImportS3<Printing_IStandardPrintTaskOptionsStatic, Printing_IStandardPrintTaskOptionsStatic2, Printing_IStandardPrintTaskOptionsStatic3>)
  public
    // -> Printing_IStandardPrintTaskOptionsStatic
    class function get_MediaSize: HSTRING; static; inline;
    class function get_MediaType: HSTRING; static; inline;
    class function get_Orientation: HSTRING; static; inline;
    class function get_PrintQuality: HSTRING; static; inline;
    class function get_ColorMode: HSTRING; static; inline;
    class function get_Duplex: HSTRING; static; inline;
    class function get_Collation: HSTRING; static; inline;
    class function get_Staple: HSTRING; static; inline;
    class function get_HolePunch: HSTRING; static; inline;
    class function get_Binding: HSTRING; static; inline;
    class function get_Copies: HSTRING; static; inline;
    class function get_NUp: HSTRING; static; inline;
    class function get_InputBin: HSTRING; static; inline;
    class property Binding: HSTRING read get_Binding;
    class property Collation: HSTRING read get_Collation;
    class property ColorMode: HSTRING read get_ColorMode;
    class property Copies: HSTRING read get_Copies;
    class property Duplex: HSTRING read get_Duplex;
    class property HolePunch: HSTRING read get_HolePunch;
    class property InputBin: HSTRING read get_InputBin;
    class property MediaSize: HSTRING read get_MediaSize;
    class property MediaType: HSTRING read get_MediaType;
    class property NUp: HSTRING read get_NUp;
    class property Orientation: HSTRING read get_Orientation;
    class property PrintQuality: HSTRING read get_PrintQuality;
    class property Staple: HSTRING read get_Staple;

    // -> Printing_IStandardPrintTaskOptionsStatic2
    class function get_Bordering: HSTRING; static; inline;
    class property Bordering: HSTRING read get_Bordering;

    // -> Printing_IStandardPrintTaskOptionsStatic3
    class function get_CustomPageRanges: HSTRING; static; inline;
    class property CustomPageRanges: HSTRING read get_CustomPageRanges;
  end;

  // Windows.Graphics.Printing3D.Print3DManager
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrint3DManager
  // Statics: "Windows.Graphics.Printing3D.IPrint3DManagerStatics"
  TPrinting3D_Print3DManager = class(TWinRTGenericImportS<Printing3D_IPrint3DManagerStatics>)
  public
    // -> Printing3D_IPrint3DManagerStatics
    class function GetForCurrentView: Printing3D_IPrint3DManager; static; inline;
    class function ShowPrintUIAsync: IAsyncOperation_1__Boolean; static; inline;
  end;

  // Windows.Graphics.Printing3D.Printing3D3MFPackage
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3D3MFPackage
  // Implements: Windows.Graphics.Printing3D.IPrinting3D3MFPackage2
  // Statics: "Windows.Graphics.Printing3D.IPrinting3D3MFPackageStatics"
  // Instantiable: "Printing3D_IPrinting3D3MFPackage"
  TPrinting3D_Printing3D3MFPackage = class(TWinRTGenericImportSI<Printing3D_IPrinting3D3MFPackageStatics, Printing3D_IPrinting3D3MFPackage>)
  public
    // -> Printing3D_IPrinting3D3MFPackageStatics
    class function LoadAsync(value: IRandomAccessStream): IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage; static; inline;
  end;

  // Windows.Graphics.Printing3D.Printing3DBaseMaterial
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DBaseMaterial
  // Statics: "Windows.Graphics.Printing3D.IPrinting3DBaseMaterialStatics"
  // Instantiable: "Printing3D_IPrinting3DBaseMaterial"
  TPrinting3D_Printing3DBaseMaterial = class(TWinRTGenericImportSI<Printing3D_IPrinting3DBaseMaterialStatics, Printing3D_IPrinting3DBaseMaterial>)
  public
    // -> Printing3D_IPrinting3DBaseMaterialStatics
    class function get_Abs: HSTRING; static; inline;
    class function get_Pla: HSTRING; static; inline;
    class property Abs: HSTRING read get_Abs;
    class property Pla: HSTRING read get_Pla;
  end;

  // Windows.Graphics.Printing3D.Printing3DBaseMaterialGroup
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroup
  // Factory: "Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroupFactory"
  TPrinting3D_Printing3DBaseMaterialGroup = class(TWinRTGenericImportF<Printing3D_IPrinting3DBaseMaterialGroupFactory>)
  public
    // -> Printing3D_IPrinting3DBaseMaterialGroupFactory
    class function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DBaseMaterialGroup; static; inline;
  end;

  // Windows.Graphics.Printing3D.Printing3DColorMaterial
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DColorMaterial
  // Implements: Windows.Graphics.Printing3D.IPrinting3DColorMaterial2
  // Instantiable: "Printing3D_IPrinting3DColorMaterial"
  TPrinting3D_Printing3DColorMaterial = class(TWinRTGenericImportI<Printing3D_IPrinting3DColorMaterial>) end;

  // Windows.Graphics.Printing3D.Printing3DColorMaterialGroup
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroup
  // Factory: "Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroupFactory"
  TPrinting3D_Printing3DColorMaterialGroup = class(TWinRTGenericImportF<Printing3D_IPrinting3DColorMaterialGroupFactory>)
  public
    // -> Printing3D_IPrinting3DColorMaterialGroupFactory
    class function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DColorMaterialGroup; static; inline;
  end;

  // Windows.Graphics.Printing3D.Printing3DComponent
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DComponent
  // Instantiable: "Printing3D_IPrinting3DComponent"
  TPrinting3D_Printing3DComponent = class(TWinRTGenericImportI<Printing3D_IPrinting3DComponent>) end;

  // Windows.Graphics.Printing3D.Printing3DComponentWithMatrix
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DComponentWithMatrix
  // Instantiable: "Printing3D_IPrinting3DComponentWithMatrix"
  TPrinting3D_Printing3DComponentWithMatrix = class(TWinRTGenericImportI<Printing3D_IPrinting3DComponentWithMatrix>) end;

  // Windows.Graphics.Printing3D.Printing3DCompositeMaterial
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DCompositeMaterial
  // Instantiable: "Printing3D_IPrinting3DCompositeMaterial"
  TPrinting3D_Printing3DCompositeMaterial = class(TWinRTGenericImportI<Printing3D_IPrinting3DCompositeMaterial>) end;

  // Windows.Graphics.Printing3D.Printing3DCompositeMaterialGroup
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup
  // Implements: Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroup2
  // Factory: "Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroupFactory"
  TPrinting3D_Printing3DCompositeMaterialGroup = class(TWinRTGenericImportF<Printing3D_IPrinting3DCompositeMaterialGroupFactory>)
  public
    // -> Printing3D_IPrinting3DCompositeMaterialGroupFactory
    class function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DCompositeMaterialGroup; static; inline;
  end;

  // Windows.Graphics.Printing3D.Printing3DFaceReductionOptions
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DFaceReductionOptions
  // Instantiable: "Printing3D_IPrinting3DFaceReductionOptions"
  TPrinting3D_Printing3DFaceReductionOptions = class(TWinRTGenericImportI<Printing3D_IPrinting3DFaceReductionOptions>) end;

  // Windows.Graphics.Printing3D.Printing3DMaterial
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DMaterial
  // Instantiable: "Printing3D_IPrinting3DMaterial"
  TPrinting3D_Printing3DMaterial = class(TWinRTGenericImportI<Printing3D_IPrinting3DMaterial>) end;

  // Windows.Graphics.Printing3D.Printing3DMesh
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DMesh
  // Instantiable: "Printing3D_IPrinting3DMesh"
  TPrinting3D_Printing3DMesh = class(TWinRTGenericImportI<Printing3D_IPrinting3DMesh>) end;

  // Windows.Graphics.Printing3D.Printing3DModel
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DModel
  // Implements: Windows.Graphics.Printing3D.IPrinting3DModel2
  // Instantiable: "Printing3D_IPrinting3DModel"
  TPrinting3D_Printing3DModel = class(TWinRTGenericImportI<Printing3D_IPrinting3DModel>) end;

  // Windows.Graphics.Printing3D.Printing3DModelTexture
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DModelTexture
  // Instantiable: "Printing3D_IPrinting3DModelTexture"
  TPrinting3D_Printing3DModelTexture = class(TWinRTGenericImportI<Printing3D_IPrinting3DModelTexture>) end;

  // Windows.Graphics.Printing3D.Printing3DMultiplePropertyMaterial
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterial
  // Instantiable: "Printing3D_IPrinting3DMultiplePropertyMaterial"
  TPrinting3D_Printing3DMultiplePropertyMaterial = class(TWinRTGenericImportI<Printing3D_IPrinting3DMultiplePropertyMaterial>) end;

  // Windows.Graphics.Printing3D.Printing3DMultiplePropertyMaterialGroup
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroup
  // Factory: "Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroupFactory"
  TPrinting3D_Printing3DMultiplePropertyMaterialGroup = class(TWinRTGenericImportF<Printing3D_IPrinting3DMultiplePropertyMaterialGroupFactory>)
  public
    // -> Printing3D_IPrinting3DMultiplePropertyMaterialGroupFactory
    class function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DMultiplePropertyMaterialGroup; static; inline;
  end;

  // Windows.Graphics.Printing3D.Printing3DTexture2CoordMaterial
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterial
  // Instantiable: "Printing3D_IPrinting3DTexture2CoordMaterial"
  TPrinting3D_Printing3DTexture2CoordMaterial = class(TWinRTGenericImportI<Printing3D_IPrinting3DTexture2CoordMaterial>) end;

  // Windows.Graphics.Printing3D.Printing3DTexture2CoordMaterialGroup
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup
  // Implements: Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroup2
  // Factory: "Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroupFactory"
  TPrinting3D_Printing3DTexture2CoordMaterialGroup = class(TWinRTGenericImportF<Printing3D_IPrinting3DTexture2CoordMaterialGroupFactory>)
  public
    // -> Printing3D_IPrinting3DTexture2CoordMaterialGroupFactory
    class function Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DTexture2CoordMaterialGroup; static; inline;
  end;

  // Windows.Graphics.Printing3D.Printing3DTextureResource
  // DualAPI
  // Implements: Windows.Graphics.Printing3D.IPrinting3DTextureResource
  // Instantiable: "Printing3D_IPrinting3DTextureResource"
  TPrinting3D_Printing3DTextureResource = class(TWinRTGenericImportI<Printing3D_IPrinting3DTextureResource>) end;

implementation

{ TCapture_Direct3D11CaptureFramePool }

class function TCapture_Direct3D11CaptureFramePool.Create(device: DirectX_Direct3D11_IDirect3DDevice; pixelFormat: DirectX_DirectXPixelFormat; numberOfBuffers: Integer; size: SizeInt32): Capture_IDirect3D11CaptureFramePool;
begin
  Result := Statics.Create(device, pixelFormat, numberOfBuffers, size);
end;


class function TCapture_Direct3D11CaptureFramePool.CreateFreeThreaded(device: DirectX_Direct3D11_IDirect3DDevice; pixelFormat: DirectX_DirectXPixelFormat; numberOfBuffers: Integer; size: SizeInt32): Capture_IDirect3D11CaptureFramePool;
begin
  Result := Statics2.CreateFreeThreaded(device, pixelFormat, numberOfBuffers, size);
end;


{ TCapture_GraphicsCaptureItem }

class function TCapture_GraphicsCaptureItem.CreateFromVisual(visual: IVisual): Capture_IGraphicsCaptureItem;
begin
  Result := Statics.CreateFromVisual(visual);
end;


{ TCapture_GraphicsCapturePicker }

{ TCapture_GraphicsCaptureSession }

class function TCapture_GraphicsCaptureSession.IsSupported: Boolean;
begin
  Result := Statics.IsSupported;
end;


{ TDisplay_Core_HdmiDisplayInformation }

class function TDisplay_Core_HdmiDisplayInformation.GetForCurrentView: Display_Core_IHdmiDisplayInformation;
begin
  Result := Statics.GetForCurrentView;
end;


{ THolographic_HolographicQuadLayer }
// Factories for : "Holographic_HolographicQuadLayer"
// Factory: "Windows.Graphics.Holographic.IHolographicQuadLayerFactory"
// -> Holographic_IHolographicQuadLayerFactory

class function THolographic_HolographicQuadLayer.Create(size: TSizeF): IClosable;
begin
  Result := Factory.Create(size);
end;

class function THolographic_HolographicQuadLayer.CreateWithPixelFormat(size: TSizeF; pixelFormat: DirectX_DirectXPixelFormat): IClosable;
begin
  Result := Factory.CreateWithPixelFormat(size, pixelFormat);
end;


{ TImaging_SoftwareBitmap }

class function TImaging_SoftwareBitmap.Copy(source: Imaging_ISoftwareBitmap): Imaging_ISoftwareBitmap;
begin
  Result := Statics.Copy(source);
end;

class function TImaging_SoftwareBitmap.Convert(source: Imaging_ISoftwareBitmap; format: Imaging_BitmapPixelFormat): Imaging_ISoftwareBitmap;
begin
  Result := Statics.Convert(source, format);
end;

class function TImaging_SoftwareBitmap.Convert(source: Imaging_ISoftwareBitmap; format: Imaging_BitmapPixelFormat; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap;
begin
  Result := Statics.Convert(source, format, alpha);
end;

class function TImaging_SoftwareBitmap.CreateCopyFromBuffer(source: IBuffer; format: Imaging_BitmapPixelFormat; width: Integer; height: Integer): Imaging_ISoftwareBitmap;
begin
  Result := Statics.CreateCopyFromBuffer(source, format, width, height);
end;

class function TImaging_SoftwareBitmap.CreateCopyFromBuffer(source: IBuffer; format: Imaging_BitmapPixelFormat; width: Integer; height: Integer; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap;
begin
  Result := Statics.CreateCopyFromBuffer(source, format, width, height, alpha);
end;

class function TImaging_SoftwareBitmap.CreateCopyFromSurfaceAsync(surface: DirectX_Direct3D11_IDirect3DSurface): IAsyncOperation_1__Imaging_ISoftwareBitmap;
begin
  Result := Statics.CreateCopyFromSurfaceAsync(surface);
end;

class function TImaging_SoftwareBitmap.CreateCopyFromSurfaceAsync(surface: DirectX_Direct3D11_IDirect3DSurface; alpha: Imaging_BitmapAlphaMode): IAsyncOperation_1__Imaging_ISoftwareBitmap;
begin
  Result := Statics.CreateCopyFromSurfaceAsync(surface, alpha);
end;

// Factories for : "Imaging_SoftwareBitmap"
// Factory: "Windows.Graphics.Imaging.ISoftwareBitmapFactory"
// -> Imaging_ISoftwareBitmapFactory

class function TImaging_SoftwareBitmap.Create(format: Imaging_BitmapPixelFormat; width: Integer; height: Integer): Imaging_ISoftwareBitmap;
begin
  Result := Factory.Create(format, width, height);
end;

class function TImaging_SoftwareBitmap.CreateWithAlpha(format: Imaging_BitmapPixelFormat; width: Integer; height: Integer; alpha: Imaging_BitmapAlphaMode): Imaging_ISoftwareBitmap;
begin
  Result := Factory.CreateWithAlpha(format, width, height, alpha);
end;


{ TPrinting_PrintPageInfo }

{ TPrinting_PrintPageRange }
// Factories for : "Printing_PrintPageRange"
// Factory: "Windows.Graphics.Printing.IPrintPageRangeFactory"
// -> Printing_IPrintPageRangeFactory

class function TPrinting_PrintPageRange.Create(firstPage: Integer; lastPage: Integer): Printing_IPrintPageRange;
begin
  Result := Factory.Create(firstPage, lastPage);
end;

class function TPrinting_PrintPageRange.CreateWithSinglePage(page: Integer): Printing_IPrintPageRange;
begin
  Result := Factory.CreateWithSinglePage(page);
end;


{ TPrinting_StandardPrintTaskOptions }

class function TPrinting_StandardPrintTaskOptions.get_MediaSize: HSTRING;
begin
  Result := Statics.get_MediaSize;
end;

class function TPrinting_StandardPrintTaskOptions.get_MediaType: HSTRING;
begin
  Result := Statics.get_MediaType;
end;

class function TPrinting_StandardPrintTaskOptions.get_Orientation: HSTRING;
begin
  Result := Statics.get_Orientation;
end;

class function TPrinting_StandardPrintTaskOptions.get_PrintQuality: HSTRING;
begin
  Result := Statics.get_PrintQuality;
end;

class function TPrinting_StandardPrintTaskOptions.get_ColorMode: HSTRING;
begin
  Result := Statics.get_ColorMode;
end;

class function TPrinting_StandardPrintTaskOptions.get_Duplex: HSTRING;
begin
  Result := Statics.get_Duplex;
end;

class function TPrinting_StandardPrintTaskOptions.get_Collation: HSTRING;
begin
  Result := Statics.get_Collation;
end;

class function TPrinting_StandardPrintTaskOptions.get_Staple: HSTRING;
begin
  Result := Statics.get_Staple;
end;

class function TPrinting_StandardPrintTaskOptions.get_HolePunch: HSTRING;
begin
  Result := Statics.get_HolePunch;
end;

class function TPrinting_StandardPrintTaskOptions.get_Binding: HSTRING;
begin
  Result := Statics.get_Binding;
end;

class function TPrinting_StandardPrintTaskOptions.get_Copies: HSTRING;
begin
  Result := Statics.get_Copies;
end;

class function TPrinting_StandardPrintTaskOptions.get_NUp: HSTRING;
begin
  Result := Statics.get_NUp;
end;

class function TPrinting_StandardPrintTaskOptions.get_InputBin: HSTRING;
begin
  Result := Statics.get_InputBin;
end;


class function TPrinting_StandardPrintTaskOptions.get_Bordering: HSTRING;
begin
  Result := Statics2.get_Bordering;
end;


class function TPrinting_StandardPrintTaskOptions.get_CustomPageRanges: HSTRING;
begin
  Result := Statics3.get_CustomPageRanges;
end;


{ TPrinting3D_Print3DManager }

class function TPrinting3D_Print3DManager.GetForCurrentView: Printing3D_IPrint3DManager;
begin
  Result := Statics.GetForCurrentView;
end;

class function TPrinting3D_Print3DManager.ShowPrintUIAsync: IAsyncOperation_1__Boolean;
begin
  Result := Statics.ShowPrintUIAsync;
end;


{ TPrinting3D_Printing3D3MFPackage }

class function TPrinting3D_Printing3D3MFPackage.LoadAsync(value: IRandomAccessStream): IAsyncOperation_1__Printing3D_IPrinting3D3MFPackage;
begin
  Result := Statics.LoadAsync(value);
end;


{ TPrinting3D_Printing3DBaseMaterial }

class function TPrinting3D_Printing3DBaseMaterial.get_Abs: HSTRING;
begin
  Result := Statics.get_Abs;
end;

class function TPrinting3D_Printing3DBaseMaterial.get_Pla: HSTRING;
begin
  Result := Statics.get_Pla;
end;


{ TPrinting3D_Printing3DBaseMaterialGroup }
// Factories for : "Printing3D_Printing3DBaseMaterialGroup"
// Factory: "Windows.Graphics.Printing3D.IPrinting3DBaseMaterialGroupFactory"
// -> Printing3D_IPrinting3DBaseMaterialGroupFactory

class function TPrinting3D_Printing3DBaseMaterialGroup.Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DBaseMaterialGroup;
begin
  Result := Factory.Create(MaterialGroupId);
end;


{ TPrinting3D_Printing3DColorMaterial }

{ TPrinting3D_Printing3DColorMaterialGroup }
// Factories for : "Printing3D_Printing3DColorMaterialGroup"
// Factory: "Windows.Graphics.Printing3D.IPrinting3DColorMaterialGroupFactory"
// -> Printing3D_IPrinting3DColorMaterialGroupFactory

class function TPrinting3D_Printing3DColorMaterialGroup.Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DColorMaterialGroup;
begin
  Result := Factory.Create(MaterialGroupId);
end;


{ TPrinting3D_Printing3DComponent }

{ TPrinting3D_Printing3DComponentWithMatrix }

{ TPrinting3D_Printing3DCompositeMaterial }

{ TPrinting3D_Printing3DCompositeMaterialGroup }
// Factories for : "Printing3D_Printing3DCompositeMaterialGroup"
// Factory: "Windows.Graphics.Printing3D.IPrinting3DCompositeMaterialGroupFactory"
// -> Printing3D_IPrinting3DCompositeMaterialGroupFactory

class function TPrinting3D_Printing3DCompositeMaterialGroup.Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DCompositeMaterialGroup;
begin
  Result := Factory.Create(MaterialGroupId);
end;


{ TPrinting3D_Printing3DFaceReductionOptions }

{ TPrinting3D_Printing3DMaterial }

{ TPrinting3D_Printing3DMesh }

{ TPrinting3D_Printing3DModel }

{ TPrinting3D_Printing3DModelTexture }

{ TPrinting3D_Printing3DMultiplePropertyMaterial }

{ TPrinting3D_Printing3DMultiplePropertyMaterialGroup }
// Factories for : "Printing3D_Printing3DMultiplePropertyMaterialGroup"
// Factory: "Windows.Graphics.Printing3D.IPrinting3DMultiplePropertyMaterialGroupFactory"
// -> Printing3D_IPrinting3DMultiplePropertyMaterialGroupFactory

class function TPrinting3D_Printing3DMultiplePropertyMaterialGroup.Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DMultiplePropertyMaterialGroup;
begin
  Result := Factory.Create(MaterialGroupId);
end;


{ TPrinting3D_Printing3DTexture2CoordMaterial }

{ TPrinting3D_Printing3DTexture2CoordMaterialGroup }
// Factories for : "Printing3D_Printing3DTexture2CoordMaterialGroup"
// Factory: "Windows.Graphics.Printing3D.IPrinting3DTexture2CoordMaterialGroupFactory"
// -> Printing3D_IPrinting3DTexture2CoordMaterialGroupFactory

class function TPrinting3D_Printing3DTexture2CoordMaterialGroup.Create(MaterialGroupId: Cardinal): Printing3D_IPrinting3DTexture2CoordMaterialGroup;
begin
  Result := Factory.Create(MaterialGroupId);
end;


{ TPrinting3D_Printing3DTextureResource }

end.
