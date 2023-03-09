{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI.Xaml.Media;

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
  Winapi.UI, 
  Winapi.UI.Composition, 
  Winapi.Foundation, 
  Winapi.Storage.Streams, 
  Winapi.ApplicationModel.Background, 
  Winapi.GraphicsRT, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  Animation_IConnectedAnimation = Winapi.CommonTypes.Animation_IConnectedAnimation;
  PAnimation_IConnectedAnimation = Winapi.CommonTypes.PAnimation_IConnectedAnimation;
  Animation_INavigationTransitionInfo = Winapi.CommonTypes.Animation_INavigationTransitionInfo;
  PAnimation_INavigationTransitionInfo = Winapi.CommonTypes.PAnimation_INavigationTransitionInfo;
  Animation_ITransition = Winapi.CommonTypes.Animation_ITransition;
  PAnimation_ITransition = Winapi.CommonTypes.PAnimation_ITransition;
  AudioCategory = Winapi.CommonTypes.AudioCategory;
  PAudioCategory = Winapi.CommonTypes.PAudioCategory;
  AudioDeviceType = Winapi.CommonTypes.AudioDeviceType;
  PAudioDeviceType = Winapi.CommonTypes.PAudioDeviceType;
  IBrush = Winapi.CommonTypes.IBrush;
  PIBrush = Winapi.CommonTypes.PIBrush;
  ICacheMode = Winapi.CommonTypes.ICacheMode;
  PICacheMode = Winapi.CommonTypes.PICacheMode;
  IGeneralTransform = Winapi.CommonTypes.IGeneralTransform;
  PIGeneralTransform = Winapi.CommonTypes.PIGeneralTransform;
  IImageSource = Winapi.CommonTypes.IImageSource;
  PIImageSource = Winapi.CommonTypes.PIImageSource;
  IProjection = Winapi.CommonTypes.IProjection;
  PIProjection = Winapi.CommonTypes.PIProjection;
  IRateChangedRoutedEventArgs = Winapi.CommonTypes.IRateChangedRoutedEventArgs;
  PIRateChangedRoutedEventArgs = Winapi.CommonTypes.PIRateChangedRoutedEventArgs;
  IRectangleGeometry = Winapi.CommonTypes.IRectangleGeometry;
  PIRectangleGeometry = Winapi.CommonTypes.PIRectangleGeometry;
  ITimelineMarker = Winapi.CommonTypes.ITimelineMarker;
  PITimelineMarker = Winapi.CommonTypes.PITimelineMarker;
  ITimelineMarkerRoutedEventArgs = Winapi.CommonTypes.ITimelineMarkerRoutedEventArgs;
  PITimelineMarkerRoutedEventArgs = Winapi.CommonTypes.PITimelineMarkerRoutedEventArgs;
  ITransform = Winapi.CommonTypes.ITransform;
  PITransform = Winapi.CommonTypes.PITransform;
  IVector_1__Animation_ITransition_Base = Winapi.CommonTypes.IVector_1__Animation_ITransition_Base;
  IVector_1__Animation_ITransition = Winapi.CommonTypes.IVector_1__Animation_ITransition;
  PIVector_1__Animation_ITransition = Winapi.CommonTypes.PIVector_1__Animation_ITransition;
  IVector_1__ITimelineMarker_Base = Winapi.CommonTypes.IVector_1__ITimelineMarker_Base;
  IVector_1__ITimelineMarker = Winapi.CommonTypes.IVector_1__ITimelineMarker;
  PIVector_1__ITimelineMarker = Winapi.CommonTypes.PIVector_1__ITimelineMarker;
  IVectorView_1__Animation_ITransition = Winapi.CommonTypes.IVectorView_1__Animation_ITransition;
  PIVectorView_1__Animation_ITransition = Winapi.CommonTypes.PIVectorView_1__Animation_ITransition;
  IVectorView_1__ITimelineMarker = Winapi.CommonTypes.IVectorView_1__ITimelineMarker;
  PIVectorView_1__ITimelineMarker = Winapi.CommonTypes.PIVectorView_1__ITimelineMarker;
  MediaCanPlayResponse = Winapi.CommonTypes.MediaCanPlayResponse;
  PMediaCanPlayResponse = Winapi.CommonTypes.PMediaCanPlayResponse;
  MediaElementState = Winapi.CommonTypes.MediaElementState;
  PMediaElementState = Winapi.CommonTypes.PMediaElementState;
  RateChangedRoutedEventHandler = Winapi.CommonTypes.RateChangedRoutedEventHandler;
  PRateChangedRoutedEventHandler = Winapi.CommonTypes.PRateChangedRoutedEventHandler;
  Stereo3DVideoPackingMode = Winapi.CommonTypes.Stereo3DVideoPackingMode;
  PStereo3DVideoPackingMode = Winapi.CommonTypes.PStereo3DVideoPackingMode;
  Stereo3DVideoRenderMode = Winapi.CommonTypes.Stereo3DVideoRenderMode;
  PStereo3DVideoRenderMode = Winapi.CommonTypes.PStereo3DVideoRenderMode;
  TimelineMarkerRoutedEventHandler = Winapi.CommonTypes.TimelineMarkerRoutedEventHandler;
  PTimelineMarkerRoutedEventHandler = Winapi.CommonTypes.PTimelineMarkerRoutedEventHandler;

  // Forward declarations for interfaces

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IIterator_1__Animation_ITransition = interface;
  PIIterator_1__Animation_ITransition = ^IIterator_1__Animation_ITransition;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IIterable_1__Animation_ITransition = interface;
  PIIterable_1__Animation_ITransition = ^IIterable_1__Animation_ITransition;

  // Windows.UI.Xaml.Media.IFontFamily
  IFontFamily = interface;
  PIFontFamily = ^IFontFamily;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IIterator_1__ITimelineMarker = interface;
  PIIterator_1__ITimelineMarker = ^IIterator_1__ITimelineMarker;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IIterable_1__ITimelineMarker = interface;
  PIIterable_1__ITimelineMarker = ^IIterable_1__ITimelineMarker;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IBrush>
  IIterator_1__IBrush = interface;
  PIIterator_1__IBrush = ^IIterator_1__IBrush;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IBrush>
  IIterable_1__IBrush = interface;
  PIIterable_1__IBrush = ^IIterable_1__IBrush;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IBrush>
  IVectorView_1__IBrush = interface;
  PIVectorView_1__IBrush = ^IVectorView_1__IBrush;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IBrush>
  IVector_1__IBrush = interface;
  PIVector_1__IBrush = ^IVector_1__IBrush;

  // Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs
  IPartialMediaFailureDetectedEventArgs = interface;
  PIPartialMediaFailureDetectedEventArgs = ^IPartialMediaFailureDetectedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IMediaElement,Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs>
  TypedEventHandler_2__IMediaElement__IPartialMediaFailureDetectedEventArgs = interface;
  PTypedEventHandler_2__IMediaElement__IPartialMediaFailureDetectedEventArgs = ^TypedEventHandler_2__IMediaElement__IPartialMediaFailureDetectedEventArgs;

  // Windows.UI.Xaml.Media.IMediaTransportControlsThumbnailRequestedEventArgs
  IMediaTransportControlsThumbnailRequestedEventArgs = interface;
  PIMediaTransportControlsThumbnailRequestedEventArgs = ^IMediaTransportControlsThumbnailRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IMediaTransportControls,Windows.UI.Xaml.Media.IMediaTransportControlsThumbnailRequestedEventArgs>
  TypedEventHandler_2__IMediaTransportControls__IMediaTransportControlsThumbnailRequestedEventArgs = interface;
  PTypedEventHandler_2__IMediaTransportControls__IMediaTransportControlsThumbnailRequestedEventArgs = ^TypedEventHandler_2__IMediaTransportControls__IMediaTransportControlsThumbnailRequestedEventArgs;

  // Windows.UI.Xaml.Media.ISolidColorBrush
  ISolidColorBrush = interface;
  PISolidColorBrush = ^ISolidColorBrush;

  // Windows.UI.Xaml.Media.IGeometry
  IGeometry = interface;
  PIGeometry = ^IGeometry;

  // Windows.UI.Xaml.Media.Imaging.IDownloadProgressEventArgs
  Imaging_IDownloadProgressEventArgs = interface;
  PImaging_IDownloadProgressEventArgs = ^Imaging_IDownloadProgressEventArgs;

  // Windows.UI.Xaml.Media.Imaging.DownloadProgressEventHandler
  Imaging_DownloadProgressEventHandler = interface;
  PImaging_DownloadProgressEventHandler = ^Imaging_DownloadProgressEventHandler;

  // Windows.UI.Xaml.Media.Imaging.IBitmapImage
  Imaging_IBitmapImage = interface;
  PImaging_IBitmapImage = ^Imaging_IBitmapImage;

  // Windows.UI.Xaml.Media.IShadow
  IShadow = interface;
  PIShadow = ^IShadow;

  // Windows.UI.Xaml.Media.Media3D.ITransform3D
  Media3D_ITransform3D = interface;
  PMedia3D_ITransform3D = ^Media3D_ITransform3D;

  // Windows.UI.Xaml.Media.IXamlLight
  IXamlLight = interface;
  PIXamlLight = ^IXamlLight;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IXamlLight>
  IIterator_1__IXamlLight = interface;
  PIIterator_1__IXamlLight = ^IIterator_1__IXamlLight;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IXamlLight>
  IIterable_1__IXamlLight = interface;
  PIIterable_1__IXamlLight = ^IIterable_1__IXamlLight;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IXamlLight>
  IVectorView_1__IXamlLight = interface;
  PIVectorView_1__IXamlLight = ^IVectorView_1__IXamlLight;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IXamlLight>
  IVector_1__IXamlLight = interface;
  PIVector_1__IXamlLight = ^IVector_1__IXamlLight;

  // Windows.UI.Xaml.Media.Animation.ITimeline
  Animation_ITimeline = interface;
  PAnimation_ITimeline = ^Animation_ITimeline;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IIterator_1__Animation_ITimeline = interface;
  PIIterator_1__Animation_ITimeline = ^IIterator_1__Animation_ITimeline;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IIterable_1__Animation_ITimeline = interface;
  PIIterable_1__Animation_ITimeline = ^IIterable_1__Animation_ITimeline;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IVectorView_1__Animation_ITimeline = interface;
  PIVectorView_1__Animation_ITimeline = ^IVectorView_1__Animation_ITimeline;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IVector_1__Animation_ITimeline = interface;
  PIVector_1__Animation_ITimeline = ^IVector_1__Animation_ITimeline;

  // Windows.UI.Xaml.Media.Animation.IStoryboard
  Animation_IStoryboard = interface;
  PAnimation_IStoryboard = ^Animation_IStoryboard;

  // Windows.UI.Xaml.Media.Animation.IEasingFunctionBase
  Animation_IEasingFunctionBase = interface;
  PAnimation_IEasingFunctionBase = ^Animation_IEasingFunctionBase;

  // Windows.UI.Xaml.Media.Animation.IAddDeleteThemeTransition
  Animation_IAddDeleteThemeTransition = interface;
  PAnimation_IAddDeleteThemeTransition = ^Animation_IAddDeleteThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IBackEase
  Animation_IBackEase = interface;
  PAnimation_IBackEase = ^Animation_IBackEase;

  // Windows.UI.Xaml.Media.Animation.IBackEaseStatics
  Animation_IBackEaseStatics = interface;
  PAnimation_IBackEaseStatics = ^Animation_IBackEaseStatics;

  // Windows.UI.Xaml.Media.Animation.IBasicConnectedAnimationConfiguration
  Animation_IBasicConnectedAnimationConfiguration = interface;
  PAnimation_IBasicConnectedAnimationConfiguration = ^Animation_IBasicConnectedAnimationConfiguration;

  // Windows.UI.Xaml.Media.Animation.IBasicConnectedAnimationConfigurationFactory
  Animation_IBasicConnectedAnimationConfigurationFactory = interface;
  PAnimation_IBasicConnectedAnimationConfigurationFactory = ^Animation_IBasicConnectedAnimationConfigurationFactory;

  // Windows.UI.Xaml.Media.Animation.IBeginStoryboard
  Animation_IBeginStoryboard = interface;
  PAnimation_IBeginStoryboard = ^Animation_IBeginStoryboard;

  // Windows.UI.Xaml.Media.Animation.IBeginStoryboardStatics
  Animation_IBeginStoryboardStatics = interface;
  PAnimation_IBeginStoryboardStatics = ^Animation_IBeginStoryboardStatics;

  // Windows.UI.Xaml.Media.Animation.IBounceEase
  Animation_IBounceEase = interface;
  PAnimation_IBounceEase = ^Animation_IBounceEase;

  // Windows.UI.Xaml.Media.Animation.IBounceEaseStatics
  Animation_IBounceEaseStatics = interface;
  PAnimation_IBounceEaseStatics = ^Animation_IBounceEaseStatics;

  // Windows.UI.Xaml.Media.Animation.ICircleEase
  Animation_ICircleEase = interface;
  PAnimation_ICircleEase = ^Animation_ICircleEase;

  // Windows.UI.Xaml.Media.Animation.IColorAnimation
  Animation_IColorAnimation = interface;
  PAnimation_IColorAnimation = ^Animation_IColorAnimation;

  // Windows.UI.Xaml.Media.Animation.IColorAnimationStatics
  Animation_IColorAnimationStatics = interface;
  PAnimation_IColorAnimationStatics = ^Animation_IColorAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IColorKeyFrame
  Animation_IColorKeyFrame = interface;
  PAnimation_IColorKeyFrame = ^Animation_IColorKeyFrame;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IIterator_1__Animation_IColorKeyFrame = interface;
  PIIterator_1__Animation_IColorKeyFrame = ^IIterator_1__Animation_IColorKeyFrame;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IIterable_1__Animation_IColorKeyFrame = interface;
  PIIterable_1__Animation_IColorKeyFrame = ^IIterable_1__Animation_IColorKeyFrame;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IVectorView_1__Animation_IColorKeyFrame = interface;
  PIVectorView_1__Animation_IColorKeyFrame = ^IVectorView_1__Animation_IColorKeyFrame;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IVector_1__Animation_IColorKeyFrame = interface;
  PIVector_1__Animation_IColorKeyFrame = ^IVector_1__Animation_IColorKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IColorAnimationUsingKeyFrames
  Animation_IColorAnimationUsingKeyFrames = interface;
  PAnimation_IColorAnimationUsingKeyFrames = ^Animation_IColorAnimationUsingKeyFrames;

  // Windows.UI.Xaml.Media.Animation.IColorAnimationUsingKeyFramesStatics
  Animation_IColorAnimationUsingKeyFramesStatics = interface;
  PAnimation_IColorAnimationUsingKeyFramesStatics = ^Animation_IColorAnimationUsingKeyFramesStatics;

  // Windows.UI.Xaml.Media.Animation.IColorKeyFrameFactory
  Animation_IColorKeyFrameFactory = interface;
  PAnimation_IColorKeyFrameFactory = ^Animation_IColorKeyFrameFactory;

  // Windows.UI.Xaml.Media.Animation.IColorKeyFrameStatics
  Animation_IColorKeyFrameStatics = interface;
  PAnimation_IColorKeyFrameStatics = ^Animation_IColorKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.ICommonNavigationTransitionInfo
  Animation_ICommonNavigationTransitionInfo = interface;
  PAnimation_ICommonNavigationTransitionInfo = ^Animation_ICommonNavigationTransitionInfo;

  // Windows.UI.Xaml.Media.Animation.ICommonNavigationTransitionInfoStatics
  Animation_ICommonNavigationTransitionInfoStatics = interface;
  PAnimation_ICommonNavigationTransitionInfoStatics = ^Animation_ICommonNavigationTransitionInfoStatics;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimation2
  Animation_IConnectedAnimation2 = interface;
  PAnimation_IConnectedAnimation2 = ^Animation_IConnectedAnimation2;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationConfiguration
  Animation_IConnectedAnimationConfiguration = interface;
  PAnimation_IConnectedAnimationConfiguration = ^Animation_IConnectedAnimationConfiguration;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimation3
  Animation_IConnectedAnimation3 = interface;
  PAnimation_IConnectedAnimation3 = ^Animation_IConnectedAnimation3;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationConfigurationFactory
  Animation_IConnectedAnimationConfigurationFactory = interface;
  PAnimation_IConnectedAnimationConfigurationFactory = ^Animation_IConnectedAnimationConfigurationFactory;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationService
  Animation_IConnectedAnimationService = interface;
  PAnimation_IConnectedAnimationService = ^Animation_IConnectedAnimationService;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationServiceStatics
  Animation_IConnectedAnimationServiceStatics = interface;
  PAnimation_IConnectedAnimationServiceStatics = ^Animation_IConnectedAnimationServiceStatics;

  // Windows.UI.Xaml.Media.Animation.IContentThemeTransition
  Animation_IContentThemeTransition = interface;
  PAnimation_IContentThemeTransition = ^Animation_IContentThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IContentThemeTransitionStatics
  Animation_IContentThemeTransitionStatics = interface;
  PAnimation_IContentThemeTransitionStatics = ^Animation_IContentThemeTransitionStatics;

  // Windows.UI.Xaml.Media.Animation.IContinuumNavigationTransitionInfo
  Animation_IContinuumNavigationTransitionInfo = interface;
  PAnimation_IContinuumNavigationTransitionInfo = ^Animation_IContinuumNavigationTransitionInfo;

  // Windows.UI.Xaml.Media.Animation.IContinuumNavigationTransitionInfoStatics
  Animation_IContinuumNavigationTransitionInfoStatics = interface;
  PAnimation_IContinuumNavigationTransitionInfoStatics = ^Animation_IContinuumNavigationTransitionInfoStatics;

  // Windows.UI.Xaml.Media.Animation.ICubicEase
  Animation_ICubicEase = interface;
  PAnimation_ICubicEase = ^Animation_ICubicEase;

  // Windows.UI.Xaml.Media.Animation.IDirectConnectedAnimationConfiguration
  Animation_IDirectConnectedAnimationConfiguration = interface;
  PAnimation_IDirectConnectedAnimationConfiguration = ^Animation_IDirectConnectedAnimationConfiguration;

  // Windows.UI.Xaml.Media.Animation.IDirectConnectedAnimationConfigurationFactory
  Animation_IDirectConnectedAnimationConfigurationFactory = interface;
  PAnimation_IDirectConnectedAnimationConfigurationFactory = ^Animation_IDirectConnectedAnimationConfigurationFactory;

  // Windows.UI.Xaml.Media.Animation.IDiscreteColorKeyFrame
  Animation_IDiscreteColorKeyFrame = interface;
  PAnimation_IDiscreteColorKeyFrame = ^Animation_IDiscreteColorKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IDiscreteDoubleKeyFrame
  Animation_IDiscreteDoubleKeyFrame = interface;
  PAnimation_IDiscreteDoubleKeyFrame = ^Animation_IDiscreteDoubleKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IDiscreteObjectKeyFrame
  Animation_IDiscreteObjectKeyFrame = interface;
  PAnimation_IDiscreteObjectKeyFrame = ^Animation_IDiscreteObjectKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IDiscretePointKeyFrame
  Animation_IDiscretePointKeyFrame = interface;
  PAnimation_IDiscretePointKeyFrame = ^Animation_IDiscretePointKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IDoubleAnimation
  Animation_IDoubleAnimation = interface;
  PAnimation_IDoubleAnimation = ^Animation_IDoubleAnimation;

  // Windows.UI.Xaml.Media.Animation.IDoubleAnimationStatics
  Animation_IDoubleAnimationStatics = interface;
  PAnimation_IDoubleAnimationStatics = ^Animation_IDoubleAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame
  Animation_IDoubleKeyFrame = interface;
  PAnimation_IDoubleKeyFrame = ^Animation_IDoubleKeyFrame;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IIterator_1__Animation_IDoubleKeyFrame = interface;
  PIIterator_1__Animation_IDoubleKeyFrame = ^IIterator_1__Animation_IDoubleKeyFrame;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IIterable_1__Animation_IDoubleKeyFrame = interface;
  PIIterable_1__Animation_IDoubleKeyFrame = ^IIterable_1__Animation_IDoubleKeyFrame;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IVectorView_1__Animation_IDoubleKeyFrame = interface;
  PIVectorView_1__Animation_IDoubleKeyFrame = ^IVectorView_1__Animation_IDoubleKeyFrame;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IVector_1__Animation_IDoubleKeyFrame = interface;
  PIVector_1__Animation_IDoubleKeyFrame = ^IVector_1__Animation_IDoubleKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IDoubleAnimationUsingKeyFrames
  Animation_IDoubleAnimationUsingKeyFrames = interface;
  PAnimation_IDoubleAnimationUsingKeyFrames = ^Animation_IDoubleAnimationUsingKeyFrames;

  // Windows.UI.Xaml.Media.Animation.IDoubleAnimationUsingKeyFramesStatics
  Animation_IDoubleAnimationUsingKeyFramesStatics = interface;
  PAnimation_IDoubleAnimationUsingKeyFramesStatics = ^Animation_IDoubleAnimationUsingKeyFramesStatics;

  // Windows.UI.Xaml.Media.Animation.IDoubleKeyFrameFactory
  Animation_IDoubleKeyFrameFactory = interface;
  PAnimation_IDoubleKeyFrameFactory = ^Animation_IDoubleKeyFrameFactory;

  // Windows.UI.Xaml.Media.Animation.IDoubleKeyFrameStatics
  Animation_IDoubleKeyFrameStatics = interface;
  PAnimation_IDoubleKeyFrameStatics = ^Animation_IDoubleKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.IDragItemThemeAnimation
  Animation_IDragItemThemeAnimation = interface;
  PAnimation_IDragItemThemeAnimation = ^Animation_IDragItemThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IDragItemThemeAnimationStatics
  Animation_IDragItemThemeAnimationStatics = interface;
  PAnimation_IDragItemThemeAnimationStatics = ^Animation_IDragItemThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IDragOverThemeAnimation
  Animation_IDragOverThemeAnimation = interface;
  PAnimation_IDragOverThemeAnimation = ^Animation_IDragOverThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IDragOverThemeAnimationStatics
  Animation_IDragOverThemeAnimationStatics = interface;
  PAnimation_IDragOverThemeAnimationStatics = ^Animation_IDragOverThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IDrillInNavigationTransitionInfo
  Animation_IDrillInNavigationTransitionInfo = interface;
  PAnimation_IDrillInNavigationTransitionInfo = ^Animation_IDrillInNavigationTransitionInfo;

  // Windows.UI.Xaml.Media.Animation.IDrillInThemeAnimation
  Animation_IDrillInThemeAnimation = interface;
  PAnimation_IDrillInThemeAnimation = ^Animation_IDrillInThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IDrillInThemeAnimationStatics
  Animation_IDrillInThemeAnimationStatics = interface;
  PAnimation_IDrillInThemeAnimationStatics = ^Animation_IDrillInThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IDrillOutThemeAnimation
  Animation_IDrillOutThemeAnimation = interface;
  PAnimation_IDrillOutThemeAnimation = ^Animation_IDrillOutThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IDrillOutThemeAnimationStatics
  Animation_IDrillOutThemeAnimationStatics = interface;
  PAnimation_IDrillOutThemeAnimationStatics = ^Animation_IDrillOutThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IDropTargetItemThemeAnimation
  Animation_IDropTargetItemThemeAnimation = interface;
  PAnimation_IDropTargetItemThemeAnimation = ^Animation_IDropTargetItemThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IDropTargetItemThemeAnimationStatics
  Animation_IDropTargetItemThemeAnimationStatics = interface;
  PAnimation_IDropTargetItemThemeAnimationStatics = ^Animation_IDropTargetItemThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IEasingColorKeyFrame
  Animation_IEasingColorKeyFrame = interface;
  PAnimation_IEasingColorKeyFrame = ^Animation_IEasingColorKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IEasingColorKeyFrameStatics
  Animation_IEasingColorKeyFrameStatics = interface;
  PAnimation_IEasingColorKeyFrameStatics = ^Animation_IEasingColorKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.IEasingDoubleKeyFrame
  Animation_IEasingDoubleKeyFrame = interface;
  PAnimation_IEasingDoubleKeyFrame = ^Animation_IEasingDoubleKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IEasingDoubleKeyFrameStatics
  Animation_IEasingDoubleKeyFrameStatics = interface;
  PAnimation_IEasingDoubleKeyFrameStatics = ^Animation_IEasingDoubleKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.IEasingFunctionBaseFactory
  Animation_IEasingFunctionBaseFactory = interface;
  PAnimation_IEasingFunctionBaseFactory = ^Animation_IEasingFunctionBaseFactory;

  // Windows.UI.Xaml.Media.Animation.IEasingFunctionBaseStatics
  Animation_IEasingFunctionBaseStatics = interface;
  PAnimation_IEasingFunctionBaseStatics = ^Animation_IEasingFunctionBaseStatics;

  // Windows.UI.Xaml.Media.Animation.IEasingPointKeyFrame
  Animation_IEasingPointKeyFrame = interface;
  PAnimation_IEasingPointKeyFrame = ^Animation_IEasingPointKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IEasingPointKeyFrameStatics
  Animation_IEasingPointKeyFrameStatics = interface;
  PAnimation_IEasingPointKeyFrameStatics = ^Animation_IEasingPointKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.IEdgeUIThemeTransition
  Animation_IEdgeUIThemeTransition = interface;
  PAnimation_IEdgeUIThemeTransition = ^Animation_IEdgeUIThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IEdgeUIThemeTransitionStatics
  Animation_IEdgeUIThemeTransitionStatics = interface;
  PAnimation_IEdgeUIThemeTransitionStatics = ^Animation_IEdgeUIThemeTransitionStatics;

  // Windows.UI.Xaml.Media.Animation.IElasticEase
  Animation_IElasticEase = interface;
  PAnimation_IElasticEase = ^Animation_IElasticEase;

  // Windows.UI.Xaml.Media.Animation.IElasticEaseStatics
  Animation_IElasticEaseStatics = interface;
  PAnimation_IElasticEaseStatics = ^Animation_IElasticEaseStatics;

  // Windows.UI.Xaml.Media.Animation.IEntranceNavigationTransitionInfo
  Animation_IEntranceNavigationTransitionInfo = interface;
  PAnimation_IEntranceNavigationTransitionInfo = ^Animation_IEntranceNavigationTransitionInfo;

  // Windows.UI.Xaml.Media.Animation.IEntranceNavigationTransitionInfoStatics
  Animation_IEntranceNavigationTransitionInfoStatics = interface;
  PAnimation_IEntranceNavigationTransitionInfoStatics = ^Animation_IEntranceNavigationTransitionInfoStatics;

  // Windows.UI.Xaml.Media.Animation.IEntranceThemeTransition
  Animation_IEntranceThemeTransition = interface;
  PAnimation_IEntranceThemeTransition = ^Animation_IEntranceThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IEntranceThemeTransitionStatics
  Animation_IEntranceThemeTransitionStatics = interface;
  PAnimation_IEntranceThemeTransitionStatics = ^Animation_IEntranceThemeTransitionStatics;

  // Windows.UI.Xaml.Media.Animation.IExponentialEase
  Animation_IExponentialEase = interface;
  PAnimation_IExponentialEase = ^Animation_IExponentialEase;

  // Windows.UI.Xaml.Media.Animation.IExponentialEaseStatics
  Animation_IExponentialEaseStatics = interface;
  PAnimation_IExponentialEaseStatics = ^Animation_IExponentialEaseStatics;

  // Windows.UI.Xaml.Media.Animation.IFadeInThemeAnimation
  Animation_IFadeInThemeAnimation = interface;
  PAnimation_IFadeInThemeAnimation = ^Animation_IFadeInThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IFadeInThemeAnimationStatics
  Animation_IFadeInThemeAnimationStatics = interface;
  PAnimation_IFadeInThemeAnimationStatics = ^Animation_IFadeInThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IFadeOutThemeAnimation
  Animation_IFadeOutThemeAnimation = interface;
  PAnimation_IFadeOutThemeAnimation = ^Animation_IFadeOutThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IFadeOutThemeAnimationStatics
  Animation_IFadeOutThemeAnimationStatics = interface;
  PAnimation_IFadeOutThemeAnimationStatics = ^Animation_IFadeOutThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfiguration
  Animation_IGravityConnectedAnimationConfiguration = interface;
  PAnimation_IGravityConnectedAnimationConfiguration = ^Animation_IGravityConnectedAnimationConfiguration;

  // Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfiguration2
  Animation_IGravityConnectedAnimationConfiguration2 = interface;
  PAnimation_IGravityConnectedAnimationConfiguration2 = ^Animation_IGravityConnectedAnimationConfiguration2;

  // Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfigurationFactory
  Animation_IGravityConnectedAnimationConfigurationFactory = interface;
  PAnimation_IGravityConnectedAnimationConfigurationFactory = ^Animation_IGravityConnectedAnimationConfigurationFactory;

  // Windows.UI.Xaml.Media.Animation.IKeySpline
  Animation_IKeySpline = interface;
  PAnimation_IKeySpline = ^Animation_IKeySpline;

  // Windows.UI.Xaml.Media.Animation.IKeyTimeHelper
  Animation_IKeyTimeHelper = interface;
  PAnimation_IKeyTimeHelper = ^Animation_IKeyTimeHelper;

  // Windows.UI.Xaml.Media.Animation.IKeyTimeHelperStatics
  Animation_IKeyTimeHelperStatics = interface;
  PAnimation_IKeyTimeHelperStatics = ^Animation_IKeyTimeHelperStatics;

  // Windows.UI.Xaml.Media.Animation.ILinearColorKeyFrame
  Animation_ILinearColorKeyFrame = interface;
  PAnimation_ILinearColorKeyFrame = ^Animation_ILinearColorKeyFrame;

  // Windows.UI.Xaml.Media.Animation.ILinearDoubleKeyFrame
  Animation_ILinearDoubleKeyFrame = interface;
  PAnimation_ILinearDoubleKeyFrame = ^Animation_ILinearDoubleKeyFrame;

  // Windows.UI.Xaml.Media.Animation.ILinearPointKeyFrame
  Animation_ILinearPointKeyFrame = interface;
  PAnimation_ILinearPointKeyFrame = ^Animation_ILinearPointKeyFrame;

  // Windows.UI.Xaml.Media.Animation.INavigationThemeTransition
  Animation_INavigationThemeTransition = interface;
  PAnimation_INavigationThemeTransition = ^Animation_INavigationThemeTransition;

  // Windows.UI.Xaml.Media.Animation.INavigationThemeTransitionStatics
  Animation_INavigationThemeTransitionStatics = interface;
  PAnimation_INavigationThemeTransitionStatics = ^Animation_INavigationThemeTransitionStatics;

  // Windows.UI.Xaml.Media.Animation.INavigationTransitionInfoFactory
  Animation_INavigationTransitionInfoFactory = interface;
  PAnimation_INavigationTransitionInfoFactory = ^Animation_INavigationTransitionInfoFactory;

  // Windows.UI.Xaml.Media.Animation.INavigationTransitionInfoOverrides
  Animation_INavigationTransitionInfoOverrides = interface;
  PAnimation_INavigationTransitionInfoOverrides = ^Animation_INavigationTransitionInfoOverrides;

  // Windows.UI.Xaml.Media.Animation.IObjectKeyFrame
  Animation_IObjectKeyFrame = interface;
  PAnimation_IObjectKeyFrame = ^Animation_IObjectKeyFrame;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IIterator_1__Animation_IObjectKeyFrame = interface;
  PIIterator_1__Animation_IObjectKeyFrame = ^IIterator_1__Animation_IObjectKeyFrame;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IIterable_1__Animation_IObjectKeyFrame = interface;
  PIIterable_1__Animation_IObjectKeyFrame = ^IIterable_1__Animation_IObjectKeyFrame;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IVectorView_1__Animation_IObjectKeyFrame = interface;
  PIVectorView_1__Animation_IObjectKeyFrame = ^IVectorView_1__Animation_IObjectKeyFrame;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IVector_1__Animation_IObjectKeyFrame = interface;
  PIVector_1__Animation_IObjectKeyFrame = ^IVector_1__Animation_IObjectKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IObjectAnimationUsingKeyFrames
  Animation_IObjectAnimationUsingKeyFrames = interface;
  PAnimation_IObjectAnimationUsingKeyFrames = ^Animation_IObjectAnimationUsingKeyFrames;

  // Windows.UI.Xaml.Media.Animation.IObjectAnimationUsingKeyFramesStatics
  Animation_IObjectAnimationUsingKeyFramesStatics = interface;
  PAnimation_IObjectAnimationUsingKeyFramesStatics = ^Animation_IObjectAnimationUsingKeyFramesStatics;

  // Windows.UI.Xaml.Media.Animation.IObjectKeyFrameFactory
  Animation_IObjectKeyFrameFactory = interface;
  PAnimation_IObjectKeyFrameFactory = ^Animation_IObjectKeyFrameFactory;

  // Windows.UI.Xaml.Media.Animation.IObjectKeyFrameStatics
  Animation_IObjectKeyFrameStatics = interface;
  PAnimation_IObjectKeyFrameStatics = ^Animation_IObjectKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.IPaneThemeTransition
  Animation_IPaneThemeTransition = interface;
  PAnimation_IPaneThemeTransition = ^Animation_IPaneThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IPaneThemeTransitionStatics
  Animation_IPaneThemeTransitionStatics = interface;
  PAnimation_IPaneThemeTransitionStatics = ^Animation_IPaneThemeTransitionStatics;

  // Windows.UI.Xaml.Media.Animation.IPointAnimation
  Animation_IPointAnimation = interface;
  PAnimation_IPointAnimation = ^Animation_IPointAnimation;

  // Windows.UI.Xaml.Media.Animation.IPointAnimationStatics
  Animation_IPointAnimationStatics = interface;
  PAnimation_IPointAnimationStatics = ^Animation_IPointAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IPointKeyFrame
  Animation_IPointKeyFrame = interface;
  PAnimation_IPointKeyFrame = ^Animation_IPointKeyFrame;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IIterator_1__Animation_IPointKeyFrame = interface;
  PIIterator_1__Animation_IPointKeyFrame = ^IIterator_1__Animation_IPointKeyFrame;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IIterable_1__Animation_IPointKeyFrame = interface;
  PIIterable_1__Animation_IPointKeyFrame = ^IIterable_1__Animation_IPointKeyFrame;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IVectorView_1__Animation_IPointKeyFrame = interface;
  PIVectorView_1__Animation_IPointKeyFrame = ^IVectorView_1__Animation_IPointKeyFrame;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IVector_1__Animation_IPointKeyFrame = interface;
  PIVector_1__Animation_IPointKeyFrame = ^IVector_1__Animation_IPointKeyFrame;

  // Windows.UI.Xaml.Media.Animation.IPointAnimationUsingKeyFrames
  Animation_IPointAnimationUsingKeyFrames = interface;
  PAnimation_IPointAnimationUsingKeyFrames = ^Animation_IPointAnimationUsingKeyFrames;

  // Windows.UI.Xaml.Media.Animation.IPointAnimationUsingKeyFramesStatics
  Animation_IPointAnimationUsingKeyFramesStatics = interface;
  PAnimation_IPointAnimationUsingKeyFramesStatics = ^Animation_IPointAnimationUsingKeyFramesStatics;

  // Windows.UI.Xaml.Media.Animation.IPointKeyFrameFactory
  Animation_IPointKeyFrameFactory = interface;
  PAnimation_IPointKeyFrameFactory = ^Animation_IPointKeyFrameFactory;

  // Windows.UI.Xaml.Media.Animation.IPointKeyFrameStatics
  Animation_IPointKeyFrameStatics = interface;
  PAnimation_IPointKeyFrameStatics = ^Animation_IPointKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.IPointerDownThemeAnimation
  Animation_IPointerDownThemeAnimation = interface;
  PAnimation_IPointerDownThemeAnimation = ^Animation_IPointerDownThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IPointerDownThemeAnimationStatics
  Animation_IPointerDownThemeAnimationStatics = interface;
  PAnimation_IPointerDownThemeAnimationStatics = ^Animation_IPointerDownThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IPointerUpThemeAnimation
  Animation_IPointerUpThemeAnimation = interface;
  PAnimation_IPointerUpThemeAnimation = ^Animation_IPointerUpThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IPointerUpThemeAnimationStatics
  Animation_IPointerUpThemeAnimationStatics = interface;
  PAnimation_IPointerUpThemeAnimationStatics = ^Animation_IPointerUpThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IPopInThemeAnimation
  Animation_IPopInThemeAnimation = interface;
  PAnimation_IPopInThemeAnimation = ^Animation_IPopInThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IPopInThemeAnimationStatics
  Animation_IPopInThemeAnimationStatics = interface;
  PAnimation_IPopInThemeAnimationStatics = ^Animation_IPopInThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IPopOutThemeAnimation
  Animation_IPopOutThemeAnimation = interface;
  PAnimation_IPopOutThemeAnimation = ^Animation_IPopOutThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IPopOutThemeAnimationStatics
  Animation_IPopOutThemeAnimationStatics = interface;
  PAnimation_IPopOutThemeAnimationStatics = ^Animation_IPopOutThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IPopupThemeTransition
  Animation_IPopupThemeTransition = interface;
  PAnimation_IPopupThemeTransition = ^Animation_IPopupThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IPopupThemeTransitionStatics
  Animation_IPopupThemeTransitionStatics = interface;
  PAnimation_IPopupThemeTransitionStatics = ^Animation_IPopupThemeTransitionStatics;

  // Windows.UI.Xaml.Media.Animation.IPowerEase
  Animation_IPowerEase = interface;
  PAnimation_IPowerEase = ^Animation_IPowerEase;

  // Windows.UI.Xaml.Media.Animation.IPowerEaseStatics
  Animation_IPowerEaseStatics = interface;
  PAnimation_IPowerEaseStatics = ^Animation_IPowerEaseStatics;

  // Windows.UI.Xaml.Media.Animation.IQuadraticEase
  Animation_IQuadraticEase = interface;
  PAnimation_IQuadraticEase = ^Animation_IQuadraticEase;

  // Windows.UI.Xaml.Media.Animation.IQuarticEase
  Animation_IQuarticEase = interface;
  PAnimation_IQuarticEase = ^Animation_IQuarticEase;

  // Windows.UI.Xaml.Media.Animation.IQuinticEase
  Animation_IQuinticEase = interface;
  PAnimation_IQuinticEase = ^Animation_IQuinticEase;

  // Windows.UI.Xaml.Media.Animation.IReorderThemeTransition
  Animation_IReorderThemeTransition = interface;
  PAnimation_IReorderThemeTransition = ^Animation_IReorderThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IRepeatBehaviorHelper
  Animation_IRepeatBehaviorHelper = interface;
  PAnimation_IRepeatBehaviorHelper = ^Animation_IRepeatBehaviorHelper;

  // Windows.UI.Xaml.Media.Animation.IRepeatBehaviorHelperStatics
  Animation_IRepeatBehaviorHelperStatics = interface;
  PAnimation_IRepeatBehaviorHelperStatics = ^Animation_IRepeatBehaviorHelperStatics;

  // Windows.UI.Xaml.Media.Animation.IRepositionThemeAnimation
  Animation_IRepositionThemeAnimation = interface;
  PAnimation_IRepositionThemeAnimation = ^Animation_IRepositionThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.IRepositionThemeAnimationStatics
  Animation_IRepositionThemeAnimationStatics = interface;
  PAnimation_IRepositionThemeAnimationStatics = ^Animation_IRepositionThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IRepositionThemeTransition
  Animation_IRepositionThemeTransition = interface;
  PAnimation_IRepositionThemeTransition = ^Animation_IRepositionThemeTransition;

  // Windows.UI.Xaml.Media.Animation.IRepositionThemeTransition2
  Animation_IRepositionThemeTransition2 = interface;
  PAnimation_IRepositionThemeTransition2 = ^Animation_IRepositionThemeTransition2;

  // Windows.UI.Xaml.Media.Animation.IRepositionThemeTransitionStatics2
  Animation_IRepositionThemeTransitionStatics2 = interface;
  PAnimation_IRepositionThemeTransitionStatics2 = ^Animation_IRepositionThemeTransitionStatics2;

  // Windows.UI.Xaml.Media.Animation.ISineEase
  Animation_ISineEase = interface;
  PAnimation_ISineEase = ^Animation_ISineEase;

  // Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfo
  Animation_ISlideNavigationTransitionInfo = interface;
  PAnimation_ISlideNavigationTransitionInfo = ^Animation_ISlideNavigationTransitionInfo;

  // Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfo2
  Animation_ISlideNavigationTransitionInfo2 = interface;
  PAnimation_ISlideNavigationTransitionInfo2 = ^Animation_ISlideNavigationTransitionInfo2;

  // Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfoStatics2
  Animation_ISlideNavigationTransitionInfoStatics2 = interface;
  PAnimation_ISlideNavigationTransitionInfoStatics2 = ^Animation_ISlideNavigationTransitionInfoStatics2;

  // Windows.UI.Xaml.Media.Animation.ISplineColorKeyFrame
  Animation_ISplineColorKeyFrame = interface;
  PAnimation_ISplineColorKeyFrame = ^Animation_ISplineColorKeyFrame;

  // Windows.UI.Xaml.Media.Animation.ISplineColorKeyFrameStatics
  Animation_ISplineColorKeyFrameStatics = interface;
  PAnimation_ISplineColorKeyFrameStatics = ^Animation_ISplineColorKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.ISplineDoubleKeyFrame
  Animation_ISplineDoubleKeyFrame = interface;
  PAnimation_ISplineDoubleKeyFrame = ^Animation_ISplineDoubleKeyFrame;

  // Windows.UI.Xaml.Media.Animation.ISplineDoubleKeyFrameStatics
  Animation_ISplineDoubleKeyFrameStatics = interface;
  PAnimation_ISplineDoubleKeyFrameStatics = ^Animation_ISplineDoubleKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.ISplinePointKeyFrame
  Animation_ISplinePointKeyFrame = interface;
  PAnimation_ISplinePointKeyFrame = ^Animation_ISplinePointKeyFrame;

  // Windows.UI.Xaml.Media.Animation.ISplinePointKeyFrameStatics
  Animation_ISplinePointKeyFrameStatics = interface;
  PAnimation_ISplinePointKeyFrameStatics = ^Animation_ISplinePointKeyFrameStatics;

  // Windows.UI.Xaml.Media.Animation.ISplitCloseThemeAnimation
  Animation_ISplitCloseThemeAnimation = interface;
  PAnimation_ISplitCloseThemeAnimation = ^Animation_ISplitCloseThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.ISplitCloseThemeAnimationStatics
  Animation_ISplitCloseThemeAnimationStatics = interface;
  PAnimation_ISplitCloseThemeAnimationStatics = ^Animation_ISplitCloseThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.ISplitOpenThemeAnimation
  Animation_ISplitOpenThemeAnimation = interface;
  PAnimation_ISplitOpenThemeAnimation = ^Animation_ISplitOpenThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.ISplitOpenThemeAnimationStatics
  Animation_ISplitOpenThemeAnimationStatics = interface;
  PAnimation_ISplitOpenThemeAnimationStatics = ^Animation_ISplitOpenThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.IStoryboardStatics
  Animation_IStoryboardStatics = interface;
  PAnimation_IStoryboardStatics = ^Animation_IStoryboardStatics;

  // Windows.UI.Xaml.Media.Animation.ISuppressNavigationTransitionInfo
  Animation_ISuppressNavigationTransitionInfo = interface;
  PAnimation_ISuppressNavigationTransitionInfo = ^Animation_ISuppressNavigationTransitionInfo;

  // Windows.UI.Xaml.Media.Animation.ISwipeBackThemeAnimation
  Animation_ISwipeBackThemeAnimation = interface;
  PAnimation_ISwipeBackThemeAnimation = ^Animation_ISwipeBackThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.ISwipeBackThemeAnimationStatics
  Animation_ISwipeBackThemeAnimationStatics = interface;
  PAnimation_ISwipeBackThemeAnimationStatics = ^Animation_ISwipeBackThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.ISwipeHintThemeAnimation
  Animation_ISwipeHintThemeAnimation = interface;
  PAnimation_ISwipeHintThemeAnimation = ^Animation_ISwipeHintThemeAnimation;

  // Windows.UI.Xaml.Media.Animation.ISwipeHintThemeAnimationStatics
  Animation_ISwipeHintThemeAnimationStatics = interface;
  PAnimation_ISwipeHintThemeAnimationStatics = ^Animation_ISwipeHintThemeAnimationStatics;

  // Windows.UI.Xaml.Media.Animation.ITimelineFactory
  Animation_ITimelineFactory = interface;
  PAnimation_ITimelineFactory = ^Animation_ITimelineFactory;

  // Windows.UI.Xaml.Media.Animation.ITimelineStatics
  Animation_ITimelineStatics = interface;
  PAnimation_ITimelineStatics = ^Animation_ITimelineStatics;

  // Windows.UI.Xaml.Media.Animation.ITransitionFactory
  Animation_ITransitionFactory = interface;
  PAnimation_ITransitionFactory = ^Animation_ITransitionFactory;

  // Windows.UI.Xaml.Media.IAcrylicBrush
  IAcrylicBrush = interface;
  PIAcrylicBrush = ^IAcrylicBrush;

  // Windows.UI.Xaml.Media.IAcrylicBrush2
  IAcrylicBrush2 = interface;
  PIAcrylicBrush2 = ^IAcrylicBrush2;

  // Windows.UI.Xaml.Media.IAcrylicBrushFactory
  IAcrylicBrushFactory = interface;
  PIAcrylicBrushFactory = ^IAcrylicBrushFactory;

  // Windows.UI.Xaml.Media.IAcrylicBrushStatics
  IAcrylicBrushStatics = interface;
  PIAcrylicBrushStatics = ^IAcrylicBrushStatics;

  // Windows.UI.Xaml.Media.IAcrylicBrushStatics2
  IAcrylicBrushStatics2 = interface;
  PIAcrylicBrushStatics2 = ^IAcrylicBrushStatics2;

  // Windows.UI.Xaml.Media.IArcSegment
  IArcSegment = interface;
  PIArcSegment = ^IArcSegment;

  // Windows.UI.Xaml.Media.IArcSegmentStatics
  IArcSegmentStatics = interface;
  PIArcSegmentStatics = ^IArcSegmentStatics;

  // Windows.UI.Xaml.Media.IBezierSegment
  IBezierSegment = interface;
  PIBezierSegment = ^IBezierSegment;

  // Windows.UI.Xaml.Media.IBezierSegmentStatics
  IBezierSegmentStatics = interface;
  PIBezierSegmentStatics = ^IBezierSegmentStatics;

  // Windows.UI.Xaml.Media.IBitmapCache
  IBitmapCache = interface;
  PIBitmapCache = ^IBitmapCache;

  // Windows.UI.Xaml.Media.IBrushFactory
  IBrushFactory = interface;
  PIBrushFactory = ^IBrushFactory;

  // Windows.UI.Xaml.Media.IBrushOverrides2
  IBrushOverrides2 = interface;
  PIBrushOverrides2 = ^IBrushOverrides2;

  // Windows.UI.Xaml.Media.IBrushStatics
  IBrushStatics = interface;
  PIBrushStatics = ^IBrushStatics;

  // Windows.UI.Xaml.Media.ICacheModeFactory
  ICacheModeFactory = interface;
  PICacheModeFactory = ^ICacheModeFactory;

  // Windows.UI.Xaml.Media.ICompositeTransform
  ICompositeTransform = interface;
  PICompositeTransform = ^ICompositeTransform;

  // Windows.UI.Xaml.Media.ICompositeTransformStatics
  ICompositeTransformStatics = interface;
  PICompositeTransformStatics = ^ICompositeTransformStatics;

  // Windows.UI.Xaml.Media.ICompositionTarget
  ICompositionTarget = interface;
  PICompositionTarget = ^ICompositionTarget;

  // Windows.UI.Xaml.Media.ICompositionTargetStatics
  ICompositionTargetStatics = interface;
  PICompositionTargetStatics = ^ICompositionTargetStatics;

  // Windows.UI.Xaml.Media.IRenderedEventArgs
  IRenderedEventArgs = interface;
  PIRenderedEventArgs = ^IRenderedEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.UI.Xaml.Media.IRenderedEventArgs>
  EventHandler_1__IRenderedEventArgs = interface;
  PEventHandler_1__IRenderedEventArgs = ^EventHandler_1__IRenderedEventArgs;

  // Windows.UI.Xaml.Media.ICompositionTargetStatics3
  ICompositionTargetStatics3 = interface;
  PICompositionTargetStatics3 = ^ICompositionTargetStatics3;

  // Windows.UI.Xaml.Media.IEllipseGeometry
  IEllipseGeometry = interface;
  PIEllipseGeometry = ^IEllipseGeometry;

  // Windows.UI.Xaml.Media.IEllipseGeometryStatics
  IEllipseGeometryStatics = interface;
  PIEllipseGeometryStatics = ^IEllipseGeometryStatics;

  // Windows.UI.Xaml.Media.IFontFamilyFactory
  IFontFamilyFactory = interface;
  PIFontFamilyFactory = ^IFontFamilyFactory;

  // Windows.UI.Xaml.Media.IFontFamilyStatics2
  IFontFamilyStatics2 = interface;
  PIFontFamilyStatics2 = ^IFontFamilyStatics2;

  // Windows.UI.Xaml.Media.IGeneralTransformFactory
  IGeneralTransformFactory = interface;
  PIGeneralTransformFactory = ^IGeneralTransformFactory;

  // Windows.UI.Xaml.Media.IGeneralTransformOverrides
  IGeneralTransformOverrides = interface;
  PIGeneralTransformOverrides = ^IGeneralTransformOverrides;

  // Windows.UI.Xaml.Media.IGeometryFactory
  IGeometryFactory = interface;
  PIGeometryFactory = ^IGeometryFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IGeometry>
  IIterator_1__IGeometry = interface;
  PIIterator_1__IGeometry = ^IIterator_1__IGeometry;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGeometry>
  IIterable_1__IGeometry = interface;
  PIIterable_1__IGeometry = ^IIterable_1__IGeometry;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IGeometry>
  IVectorView_1__IGeometry = interface;
  PIVectorView_1__IGeometry = ^IVectorView_1__IGeometry;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGeometry>
  IVector_1__IGeometry = interface;
  PIVector_1__IGeometry = ^IVector_1__IGeometry;

  // Windows.UI.Xaml.Media.IGeometryGroup
  IGeometryGroup = interface;
  PIGeometryGroup = ^IGeometryGroup;

  // Windows.UI.Xaml.Media.IGeometryGroupStatics
  IGeometryGroupStatics = interface;
  PIGeometryGroupStatics = ^IGeometryGroupStatics;

  // Windows.UI.Xaml.Media.IGeometryStatics
  IGeometryStatics = interface;
  PIGeometryStatics = ^IGeometryStatics;

  // Windows.UI.Xaml.Media.IGradientStop
  IGradientStop = interface;
  PIGradientStop = ^IGradientStop;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IGradientStop>
  IIterator_1__IGradientStop = interface;
  PIIterator_1__IGradientStop = ^IIterator_1__IGradientStop;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGradientStop>
  IIterable_1__IGradientStop = interface;
  PIIterable_1__IGradientStop = ^IIterable_1__IGradientStop;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IGradientStop>
  IVectorView_1__IGradientStop = interface;
  PIVectorView_1__IGradientStop = ^IVectorView_1__IGradientStop;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGradientStop>
  IVector_1__IGradientStop = interface;
  PIVector_1__IGradientStop = ^IVector_1__IGradientStop;

  // Windows.UI.Xaml.Media.IGradientBrush
  IGradientBrush = interface;
  PIGradientBrush = ^IGradientBrush;

  // Windows.UI.Xaml.Media.IGradientBrushFactory
  IGradientBrushFactory = interface;
  PIGradientBrushFactory = ^IGradientBrushFactory;

  // Windows.UI.Xaml.Media.IGradientBrushStatics
  IGradientBrushStatics = interface;
  PIGradientBrushStatics = ^IGradientBrushStatics;

  // Windows.UI.Xaml.Media.IGradientStopStatics
  IGradientStopStatics = interface;
  PIGradientStopStatics = ^IGradientStopStatics;

  // Windows.UI.Xaml.Media.IImageBrush
  IImageBrush = interface;
  PIImageBrush = ^IImageBrush;

  // Windows.UI.Xaml.Media.IImageBrushStatics
  IImageBrushStatics = interface;
  PIImageBrushStatics = ^IImageBrushStatics;

  // Windows.UI.Xaml.Media.IImageSourceFactory
  IImageSourceFactory = interface;
  PIImageSourceFactory = ^IImageSourceFactory;

  // Windows.UI.Xaml.Media.ILineGeometry
  ILineGeometry = interface;
  PILineGeometry = ^ILineGeometry;

  // Windows.UI.Xaml.Media.ILineGeometryStatics
  ILineGeometryStatics = interface;
  PILineGeometryStatics = ^ILineGeometryStatics;

  // Windows.UI.Xaml.Media.ILineSegment
  ILineSegment = interface;
  PILineSegment = ^ILineSegment;

  // Windows.UI.Xaml.Media.ILineSegmentStatics
  ILineSegmentStatics = interface;
  PILineSegmentStatics = ^ILineSegmentStatics;

  // Windows.UI.Xaml.Media.ILinearGradientBrush
  ILinearGradientBrush = interface;
  PILinearGradientBrush = ^ILinearGradientBrush;

  // Windows.UI.Xaml.Media.ILinearGradientBrushFactory
  ILinearGradientBrushFactory = interface;
  PILinearGradientBrushFactory = ^ILinearGradientBrushFactory;

  // Windows.UI.Xaml.Media.ILinearGradientBrushStatics
  ILinearGradientBrushStatics = interface;
  PILinearGradientBrushStatics = ^ILinearGradientBrushStatics;

  // Windows.UI.Xaml.Media.ILoadedImageSourceLoadCompletedEventArgs
  ILoadedImageSourceLoadCompletedEventArgs = interface;
  PILoadedImageSourceLoadCompletedEventArgs = ^ILoadedImageSourceLoadCompletedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.ILoadedImageSurface,Windows.UI.Xaml.Media.ILoadedImageSourceLoadCompletedEventArgs>
  TypedEventHandler_2__ILoadedImageSurface__ILoadedImageSourceLoadCompletedEventArgs = interface;
  PTypedEventHandler_2__ILoadedImageSurface__ILoadedImageSourceLoadCompletedEventArgs = ^TypedEventHandler_2__ILoadedImageSurface__ILoadedImageSourceLoadCompletedEventArgs;

  // Windows.UI.Xaml.Media.ILoadedImageSurface
  ILoadedImageSurface = interface;
  PILoadedImageSurface = ^ILoadedImageSurface;

  // Windows.UI.Xaml.Media.ILoadedImageSurfaceStatics
  ILoadedImageSurfaceStatics = interface;
  PILoadedImageSurfaceStatics = ^ILoadedImageSurfaceStatics;

  // Windows.UI.Xaml.Media.IMatrix3DProjection
  IMatrix3DProjection = interface;
  PIMatrix3DProjection = ^IMatrix3DProjection;

  // Windows.UI.Xaml.Media.IMatrix3DProjectionStatics
  IMatrix3DProjectionStatics = interface;
  PIMatrix3DProjectionStatics = ^IMatrix3DProjectionStatics;

  // Windows.UI.Xaml.Media.IMatrixHelper
  IMatrixHelper = interface;
  PIMatrixHelper = ^IMatrixHelper;

  // Windows.UI.Xaml.Media.IMatrixHelperStatics
  IMatrixHelperStatics = interface;
  PIMatrixHelperStatics = ^IMatrixHelperStatics;

  // Windows.UI.Xaml.Media.IMatrixTransform
  IMatrixTransform = interface;
  PIMatrixTransform = ^IMatrixTransform;

  // Windows.UI.Xaml.Media.IMatrixTransformStatics
  IMatrixTransformStatics = interface;
  PIMatrixTransformStatics = ^IMatrixTransformStatics;

  // Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs2
  IPartialMediaFailureDetectedEventArgs2 = interface;
  PIPartialMediaFailureDetectedEventArgs2 = ^IPartialMediaFailureDetectedEventArgs2;

  // Windows.UI.Xaml.Media.IPathSegment
  IPathSegment = interface;
  PIPathSegment = ^IPathSegment;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IPathSegment>
  IIterator_1__IPathSegment = interface;
  PIIterator_1__IPathSegment = ^IIterator_1__IPathSegment;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathSegment>
  IIterable_1__IPathSegment = interface;
  PIIterable_1__IPathSegment = ^IIterable_1__IPathSegment;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IPathSegment>
  IVectorView_1__IPathSegment = interface;
  PIVectorView_1__IPathSegment = ^IVectorView_1__IPathSegment;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathSegment>
  IVector_1__IPathSegment = interface;
  PIVector_1__IPathSegment = ^IVector_1__IPathSegment;

  // Windows.UI.Xaml.Media.IPathFigure
  IPathFigure = interface;
  PIPathFigure = ^IPathFigure;

  // Windows.UI.Xaml.Media.IPathFigureStatics
  IPathFigureStatics = interface;
  PIPathFigureStatics = ^IPathFigureStatics;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IPathFigure>
  IIterator_1__IPathFigure = interface;
  PIIterator_1__IPathFigure = ^IIterator_1__IPathFigure;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathFigure>
  IIterable_1__IPathFigure = interface;
  PIIterable_1__IPathFigure = ^IIterable_1__IPathFigure;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IPathFigure>
  IVectorView_1__IPathFigure = interface;
  PIVectorView_1__IPathFigure = ^IVectorView_1__IPathFigure;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathFigure>
  IVector_1__IPathFigure = interface;
  PIVector_1__IPathFigure = ^IVector_1__IPathFigure;

  // Windows.UI.Xaml.Media.IPathGeometry
  IPathGeometry = interface;
  PIPathGeometry = ^IPathGeometry;

  // Windows.UI.Xaml.Media.IPathGeometryStatics
  IPathGeometryStatics = interface;
  PIPathGeometryStatics = ^IPathGeometryStatics;

  // Windows.UI.Xaml.Media.IPathSegmentFactory
  IPathSegmentFactory = interface;
  PIPathSegmentFactory = ^IPathSegmentFactory;

  // Windows.UI.Xaml.Media.IPlaneProjection
  IPlaneProjection = interface;
  PIPlaneProjection = ^IPlaneProjection;

  // Windows.UI.Xaml.Media.IPlaneProjectionStatics
  IPlaneProjectionStatics = interface;
  PIPlaneProjectionStatics = ^IPlaneProjectionStatics;

  // Windows.UI.Xaml.Media.IPolyBezierSegment
  IPolyBezierSegment = interface;
  PIPolyBezierSegment = ^IPolyBezierSegment;

  // Windows.UI.Xaml.Media.IPolyBezierSegmentStatics
  IPolyBezierSegmentStatics = interface;
  PIPolyBezierSegmentStatics = ^IPolyBezierSegmentStatics;

  // Windows.UI.Xaml.Media.IPolyLineSegment
  IPolyLineSegment = interface;
  PIPolyLineSegment = ^IPolyLineSegment;

  // Windows.UI.Xaml.Media.IPolyLineSegmentStatics
  IPolyLineSegmentStatics = interface;
  PIPolyLineSegmentStatics = ^IPolyLineSegmentStatics;

  // Windows.UI.Xaml.Media.IPolyQuadraticBezierSegment
  IPolyQuadraticBezierSegment = interface;
  PIPolyQuadraticBezierSegment = ^IPolyQuadraticBezierSegment;

  // Windows.UI.Xaml.Media.IPolyQuadraticBezierSegmentStatics
  IPolyQuadraticBezierSegmentStatics = interface;
  PIPolyQuadraticBezierSegmentStatics = ^IPolyQuadraticBezierSegmentStatics;

  // Windows.UI.Xaml.Media.IProjectionFactory
  IProjectionFactory = interface;
  PIProjectionFactory = ^IProjectionFactory;

  // Windows.UI.Xaml.Media.IQuadraticBezierSegment
  IQuadraticBezierSegment = interface;
  PIQuadraticBezierSegment = ^IQuadraticBezierSegment;

  // Windows.UI.Xaml.Media.IQuadraticBezierSegmentStatics
  IQuadraticBezierSegmentStatics = interface;
  PIQuadraticBezierSegmentStatics = ^IQuadraticBezierSegmentStatics;

  // Windows.UI.Xaml.Media.IRectangleGeometryStatics
  IRectangleGeometryStatics = interface;
  PIRectangleGeometryStatics = ^IRectangleGeometryStatics;

  // Windows.UI.Xaml.Media.IRenderingEventArgs
  IRenderingEventArgs = interface;
  PIRenderingEventArgs = ^IRenderingEventArgs;

  // Windows.UI.Xaml.Media.IRevealBackgroundBrush
  IRevealBackgroundBrush = interface;
  PIRevealBackgroundBrush = ^IRevealBackgroundBrush;

  // Windows.UI.Xaml.Media.IRevealBackgroundBrushFactory
  IRevealBackgroundBrushFactory = interface;
  PIRevealBackgroundBrushFactory = ^IRevealBackgroundBrushFactory;

  // Windows.UI.Xaml.Media.IRevealBorderBrush
  IRevealBorderBrush = interface;
  PIRevealBorderBrush = ^IRevealBorderBrush;

  // Windows.UI.Xaml.Media.IRevealBorderBrushFactory
  IRevealBorderBrushFactory = interface;
  PIRevealBorderBrushFactory = ^IRevealBorderBrushFactory;

  // Windows.UI.Xaml.Media.IRevealBrush
  IRevealBrush = interface;
  PIRevealBrush = ^IRevealBrush;

  // Windows.UI.Xaml.Media.IRevealBrushFactory
  IRevealBrushFactory = interface;
  PIRevealBrushFactory = ^IRevealBrushFactory;

  // Windows.UI.Xaml.Media.IRevealBrushStatics
  IRevealBrushStatics = interface;
  PIRevealBrushStatics = ^IRevealBrushStatics;

  // Windows.UI.Xaml.Media.IRotateTransform
  IRotateTransform = interface;
  PIRotateTransform = ^IRotateTransform;

  // Windows.UI.Xaml.Media.IRotateTransformStatics
  IRotateTransformStatics = interface;
  PIRotateTransformStatics = ^IRotateTransformStatics;

  // Windows.UI.Xaml.Media.IScaleTransform
  IScaleTransform = interface;
  PIScaleTransform = ^IScaleTransform;

  // Windows.UI.Xaml.Media.IScaleTransformStatics
  IScaleTransformStatics = interface;
  PIScaleTransformStatics = ^IScaleTransformStatics;

  // Windows.UI.Xaml.Media.IShadowFactory
  IShadowFactory = interface;
  PIShadowFactory = ^IShadowFactory;

  // Windows.UI.Xaml.Media.ISkewTransform
  ISkewTransform = interface;
  PISkewTransform = ^ISkewTransform;

  // Windows.UI.Xaml.Media.ISkewTransformStatics
  ISkewTransformStatics = interface;
  PISkewTransformStatics = ^ISkewTransformStatics;

  // Windows.UI.Xaml.Media.ISolidColorBrushFactory
  ISolidColorBrushFactory = interface;
  PISolidColorBrushFactory = ^ISolidColorBrushFactory;

  // Windows.UI.Xaml.Media.ISolidColorBrushStatics
  ISolidColorBrushStatics = interface;
  PISolidColorBrushStatics = ^ISolidColorBrushStatics;

  // Windows.UI.Xaml.Media.IThemeShadow
  IThemeShadow = interface;
  PIThemeShadow = ^IThemeShadow;

  // Windows.UI.Xaml.Media.IThemeShadowFactory
  IThemeShadowFactory = interface;
  PIThemeShadowFactory = ^IThemeShadowFactory;

  // Windows.UI.Xaml.Media.ITileBrush
  ITileBrush = interface;
  PITileBrush = ^ITileBrush;

  // Windows.UI.Xaml.Media.ITileBrushFactory
  ITileBrushFactory = interface;
  PITileBrushFactory = ^ITileBrushFactory;

  // Windows.UI.Xaml.Media.ITileBrushStatics
  ITileBrushStatics = interface;
  PITileBrushStatics = ^ITileBrushStatics;

  // Windows.UI.Xaml.Media.ITimelineMarkerStatics
  ITimelineMarkerStatics = interface;
  PITimelineMarkerStatics = ^ITimelineMarkerStatics;

  // Windows.UI.Xaml.Media.ITransformFactory
  ITransformFactory = interface;
  PITransformFactory = ^ITransformFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.ITransform>
  IIterator_1__ITransform = interface;
  PIIterator_1__ITransform = ^IIterator_1__ITransform;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITransform>
  IIterable_1__ITransform = interface;
  PIIterable_1__ITransform = ^IIterable_1__ITransform;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.ITransform>
  IVectorView_1__ITransform = interface;
  PIVectorView_1__ITransform = ^IVectorView_1__ITransform;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITransform>
  IVector_1__ITransform = interface;
  PIVector_1__ITransform = ^IVector_1__ITransform;

  // Windows.UI.Xaml.Media.ITransformGroup
  ITransformGroup = interface;
  PITransformGroup = ^ITransformGroup;

  // Windows.UI.Xaml.Media.ITransformGroupStatics
  ITransformGroupStatics = interface;
  PITransformGroupStatics = ^ITransformGroupStatics;

  // Windows.UI.Xaml.Media.ITranslateTransform
  ITranslateTransform = interface;
  PITranslateTransform = ^ITranslateTransform;

  // Windows.UI.Xaml.Media.ITranslateTransformStatics
  ITranslateTransformStatics = interface;
  PITranslateTransformStatics = ^ITranslateTransformStatics;

  // Windows.UI.Xaml.Media.IVisualTreeHelper
  IVisualTreeHelper = interface;
  PIVisualTreeHelper = ^IVisualTreeHelper;

  // Windows.UI.Xaml.Media.IVisualTreeHelperStatics
  IVisualTreeHelperStatics = interface;
  PIVisualTreeHelperStatics = ^IVisualTreeHelperStatics;

  // Windows.UI.Xaml.Media.IVisualTreeHelperStatics2
  IVisualTreeHelperStatics2 = interface;
  PIVisualTreeHelperStatics2 = ^IVisualTreeHelperStatics2;

  // Windows.UI.Xaml.Media.IVisualTreeHelperStatics3
  IVisualTreeHelperStatics3 = interface;
  PIVisualTreeHelperStatics3 = ^IVisualTreeHelperStatics3;

  // Windows.UI.Xaml.Media.IXamlCompositionBrushBase
  IXamlCompositionBrushBase = interface;
  PIXamlCompositionBrushBase = ^IXamlCompositionBrushBase;

  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseFactory
  IXamlCompositionBrushBaseFactory = interface;
  PIXamlCompositionBrushBaseFactory = ^IXamlCompositionBrushBaseFactory;

  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseOverrides
  IXamlCompositionBrushBaseOverrides = interface;
  PIXamlCompositionBrushBaseOverrides = ^IXamlCompositionBrushBaseOverrides;

  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseProtected
  IXamlCompositionBrushBaseProtected = interface;
  PIXamlCompositionBrushBaseProtected = ^IXamlCompositionBrushBaseProtected;

  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseStatics
  IXamlCompositionBrushBaseStatics = interface;
  PIXamlCompositionBrushBaseStatics = ^IXamlCompositionBrushBaseStatics;

  // Windows.UI.Xaml.Media.IXamlLightFactory
  IXamlLightFactory = interface;
  PIXamlLightFactory = ^IXamlLightFactory;

  // Windows.UI.Xaml.Media.IXamlLightOverrides
  IXamlLightOverrides = interface;
  PIXamlLightOverrides = ^IXamlLightOverrides;

  // Windows.UI.Xaml.Media.IXamlLightProtected
  IXamlLightProtected = interface;
  PIXamlLightProtected = ^IXamlLightProtected;

  // Windows.UI.Xaml.Media.IXamlLightStatics
  IXamlLightStatics = interface;
  PIXamlLightStatics = ^IXamlLightStatics;

  // Windows.UI.Xaml.Media.Imaging.IBitmapImage2
  Imaging_IBitmapImage2 = interface;
  PImaging_IBitmapImage2 = ^Imaging_IBitmapImage2;

  // Windows.UI.Xaml.Media.Imaging.IBitmapImage3
  Imaging_IBitmapImage3 = interface;
  PImaging_IBitmapImage3 = ^Imaging_IBitmapImage3;

  // Windows.UI.Xaml.Media.Imaging.IBitmapImageFactory
  Imaging_IBitmapImageFactory = interface;
  PImaging_IBitmapImageFactory = ^Imaging_IBitmapImageFactory;

  // Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics
  Imaging_IBitmapImageStatics = interface;
  PImaging_IBitmapImageStatics = ^Imaging_IBitmapImageStatics;

  // Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics2
  Imaging_IBitmapImageStatics2 = interface;
  PImaging_IBitmapImageStatics2 = ^Imaging_IBitmapImageStatics2;

  // Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics3
  Imaging_IBitmapImageStatics3 = interface;
  PImaging_IBitmapImageStatics3 = ^Imaging_IBitmapImageStatics3;

  // Windows.UI.Xaml.Media.Imaging.IBitmapSource
  Imaging_IBitmapSource = interface;
  PImaging_IBitmapSource = ^Imaging_IBitmapSource;

  // Windows.UI.Xaml.Media.Imaging.IBitmapSourceFactory
  Imaging_IBitmapSourceFactory = interface;
  PImaging_IBitmapSourceFactory = ^Imaging_IBitmapSourceFactory;

  // Windows.UI.Xaml.Media.Imaging.IBitmapSourceStatics
  Imaging_IBitmapSourceStatics = interface;
  PImaging_IBitmapSourceStatics = ^Imaging_IBitmapSourceStatics;

  // Windows.UI.Xaml.Media.Imaging.IRenderTargetBitmap
  Imaging_IRenderTargetBitmap = interface;
  PImaging_IRenderTargetBitmap = ^Imaging_IRenderTargetBitmap;

  // Windows.UI.Xaml.Media.Imaging.IRenderTargetBitmapStatics
  Imaging_IRenderTargetBitmapStatics = interface;
  PImaging_IRenderTargetBitmapStatics = ^Imaging_IRenderTargetBitmapStatics;

  // Windows.UI.Xaml.Media.Imaging.ISoftwareBitmapSource
  Imaging_ISoftwareBitmapSource = interface;
  PImaging_ISoftwareBitmapSource = ^Imaging_ISoftwareBitmapSource;

  // Windows.UI.Xaml.Media.Imaging.ISurfaceImageSource
  Imaging_ISurfaceImageSource = interface;
  PImaging_ISurfaceImageSource = ^Imaging_ISurfaceImageSource;

  // Windows.UI.Xaml.Media.Imaging.ISurfaceImageSourceFactory
  Imaging_ISurfaceImageSourceFactory = interface;
  PImaging_ISurfaceImageSourceFactory = ^Imaging_ISurfaceImageSourceFactory;

  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceOpenedEventArgs
  Imaging_ISvgImageSourceOpenedEventArgs = interface;
  PImaging_ISvgImageSourceOpenedEventArgs = ^Imaging_ISvgImageSourceOpenedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Imaging.ISvgImageSource,Windows.UI.Xaml.Media.Imaging.ISvgImageSourceOpenedEventArgs>
  TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceOpenedEventArgs = interface;
  PTypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceOpenedEventArgs = ^TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceOpenedEventArgs;

  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFailedEventArgs
  Imaging_ISvgImageSourceFailedEventArgs = interface;
  PImaging_ISvgImageSourceFailedEventArgs = ^Imaging_ISvgImageSourceFailedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Imaging.ISvgImageSource,Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFailedEventArgs>
  TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceFailedEventArgs = interface;
  PTypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceFailedEventArgs = ^TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceFailedEventArgs;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Xaml.Media.Imaging.SvgImageSourceLoadStatus>
  AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus = interface;
  PAsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus = ^AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Xaml.Media.Imaging.SvgImageSourceLoadStatus>
  IAsyncOperation_1__Imaging_SvgImageSourceLoadStatus = interface;
  PIAsyncOperation_1__Imaging_SvgImageSourceLoadStatus = ^IAsyncOperation_1__Imaging_SvgImageSourceLoadStatus;

  // Windows.UI.Xaml.Media.Imaging.ISvgImageSource
  Imaging_ISvgImageSource = interface;
  PImaging_ISvgImageSource = ^Imaging_ISvgImageSource;

  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFactory
  Imaging_ISvgImageSourceFactory = interface;
  PImaging_ISvgImageSourceFactory = ^Imaging_ISvgImageSourceFactory;

  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceStatics
  Imaging_ISvgImageSourceStatics = interface;
  PImaging_ISvgImageSourceStatics = ^Imaging_ISvgImageSourceStatics;

  // Windows.UI.Xaml.Media.Imaging.IVirtualSurfaceImageSource
  Imaging_IVirtualSurfaceImageSource = interface;
  PImaging_IVirtualSurfaceImageSource = ^Imaging_IVirtualSurfaceImageSource;

  // Windows.UI.Xaml.Media.Imaging.IVirtualSurfaceImageSourceFactory
  Imaging_IVirtualSurfaceImageSourceFactory = interface;
  PImaging_IVirtualSurfaceImageSourceFactory = ^Imaging_IVirtualSurfaceImageSourceFactory;

  // Windows.UI.Xaml.Media.Imaging.IWriteableBitmap
  Imaging_IWriteableBitmap = interface;
  PImaging_IWriteableBitmap = ^Imaging_IWriteableBitmap;

  // Windows.UI.Xaml.Media.Imaging.IWriteableBitmapFactory
  Imaging_IWriteableBitmapFactory = interface;
  PImaging_IWriteableBitmapFactory = ^Imaging_IWriteableBitmapFactory;

  // Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTask
  Imaging_IXamlRenderingBackgroundTask = interface;
  PImaging_IXamlRenderingBackgroundTask = ^Imaging_IXamlRenderingBackgroundTask;

  // Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTaskFactory
  Imaging_IXamlRenderingBackgroundTaskFactory = interface;
  PImaging_IXamlRenderingBackgroundTaskFactory = ^Imaging_IXamlRenderingBackgroundTaskFactory;

  // Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTaskOverrides
  Imaging_IXamlRenderingBackgroundTaskOverrides = interface;
  PImaging_IXamlRenderingBackgroundTaskOverrides = ^Imaging_IXamlRenderingBackgroundTaskOverrides;

  // Windows.UI.Xaml.Media.Media3D.ICompositeTransform3D
  Media3D_ICompositeTransform3D = interface;
  PMedia3D_ICompositeTransform3D = ^Media3D_ICompositeTransform3D;

  // Windows.UI.Xaml.Media.Media3D.ICompositeTransform3DStatics
  Media3D_ICompositeTransform3DStatics = interface;
  PMedia3D_ICompositeTransform3DStatics = ^Media3D_ICompositeTransform3DStatics;

  // Windows.UI.Xaml.Media.Media3D.IMatrix3DHelper
  Media3D_IMatrix3DHelper = interface;
  PMedia3D_IMatrix3DHelper = ^Media3D_IMatrix3DHelper;

  // Windows.UI.Xaml.Media.Media3D.IMatrix3DHelperStatics
  Media3D_IMatrix3DHelperStatics = interface;
  PMedia3D_IMatrix3DHelperStatics = ^Media3D_IMatrix3DHelperStatics;

  // Windows.UI.Xaml.Media.Media3D.IPerspectiveTransform3D
  Media3D_IPerspectiveTransform3D = interface;
  PMedia3D_IPerspectiveTransform3D = ^Media3D_IPerspectiveTransform3D;

  // Windows.UI.Xaml.Media.Media3D.IPerspectiveTransform3DStatics
  Media3D_IPerspectiveTransform3DStatics = interface;
  PMedia3D_IPerspectiveTransform3DStatics = ^Media3D_IPerspectiveTransform3DStatics;

  // Windows.UI.Xaml.Media.Media3D.ITransform3DFactory
  Media3D_ITransform3DFactory = interface;
  PMedia3D_ITransform3DFactory = ^Media3D_ITransform3DFactory;

  // Windows.UI.Xaml.Media Enums

  // Windows.UI.Xaml.Media.AcrylicBackgroundSource
  AcrylicBackgroundSource = (
    HostBackdrop = 0,
    Backdrop = 1
  );
  PAcrylicBackgroundSource = ^AcrylicBackgroundSource;

  // Windows.UI.Xaml.Media.AlignmentX
  AlignmentX = (
    Left = 0,
    Center = 1,
    Right = 2
  );
  PAlignmentX = ^AlignmentX;

  // Windows.UI.Xaml.Media.AlignmentY
  AlignmentY = (
    Top = 0,
    Center = 1,
    Bottom = 2
  );
  PAlignmentY = ^AlignmentY;

  // Windows.UI.Xaml.Media.Animation.ClockState
  Animation_ClockState = (
    Active = 0,
    Filling = 1,
    Stopped = 2
  );
  PAnimation_ClockState = ^Animation_ClockState;

  // Windows.UI.Xaml.Media.Animation.ConnectedAnimationComponent
  Animation_ConnectedAnimationComponent = (
    OffsetX = 0,
    OffsetY = 1,
    CrossFade = 2,
    Scale = 3
  );
  PAnimation_ConnectedAnimationComponent = ^Animation_ConnectedAnimationComponent;

  // Windows.UI.Xaml.Media.Animation.EasingMode
  Animation_EasingMode = (
    EaseOut = 0,
    EaseIn = 1,
    EaseInOut = 2
  );
  PAnimation_EasingMode = ^Animation_EasingMode;

  // Windows.UI.Xaml.Media.Animation.FillBehavior
  Animation_FillBehavior = (
    HoldEnd = 0,
    Stop = 1
  );
  PAnimation_FillBehavior = ^Animation_FillBehavior;

  // Windows.UI.Xaml.Media.Animation.RepeatBehaviorType
  Animation_RepeatBehaviorType = (
    Count = 0,
    Duration = 1,
    Forever = 2
  );
  PAnimation_RepeatBehaviorType = ^Animation_RepeatBehaviorType;

  // Windows.UI.Xaml.Media.Animation.SlideNavigationTransitionEffect
  Animation_SlideNavigationTransitionEffect = (
    FromBottom = 0,
    FromLeft = 1,
    FromRight = 2
  );
  PAnimation_SlideNavigationTransitionEffect = ^Animation_SlideNavigationTransitionEffect;

  // Windows.UI.Xaml.Media.BrushMappingMode
  BrushMappingMode = (
    Absolute = 0,
    RelativeToBoundingBox = 1
  );
  PBrushMappingMode = ^BrushMappingMode;

  // Windows.UI.Xaml.Media.ColorInterpolationMode
  ColorInterpolationMode = (
    ScRgbLinearInterpolation = 0,
    SRgbLinearInterpolation = 1
  );
  PColorInterpolationMode = ^ColorInterpolationMode;

  // Windows.UI.Xaml.Media.ElementCompositeMode
  ElementCompositeMode = (
    Inherit = 0,
    SourceOver = 1,
    MinBlend = 2
  );
  PElementCompositeMode = ^ElementCompositeMode;

  // Windows.UI.Xaml.Media.FastPlayFallbackBehaviour
  FastPlayFallbackBehaviour = (
    Skip = 0,
    Hide = 1,
    Disable = 2
  );
  PFastPlayFallbackBehaviour = ^FastPlayFallbackBehaviour;

  // Windows.UI.Xaml.Media.FillRule
  FillRule = (
    EvenOdd = 0,
    Nonzero = 1
  );
  PFillRule = ^FillRule;

  // Windows.UI.Xaml.Media.GradientSpreadMethod
  GradientSpreadMethod = (
    Pad = 0,
    Reflect = 1,
    &Repeat = 2
  );
  PGradientSpreadMethod = ^GradientSpreadMethod;

  // Windows.UI.Xaml.Media.Imaging.BitmapCreateOptions
  Imaging_BitmapCreateOptions = (
    None = 0,
    IgnoreImageCache = 8
  );
  PImaging_BitmapCreateOptions = ^Imaging_BitmapCreateOptions;

  // Windows.UI.Xaml.Media.Imaging.DecodePixelType
  Imaging_DecodePixelType = (
    Physical = 0,
    Logical = 1
  );
  PImaging_DecodePixelType = ^Imaging_DecodePixelType;

  // Windows.UI.Xaml.Media.Imaging.SvgImageSourceLoadStatus
  Imaging_SvgImageSourceLoadStatus = (
    Success = 0,
    NetworkError = 1,
    InvalidFormat = 2,
    Other = 3
  );
  PImaging_SvgImageSourceLoadStatus = ^Imaging_SvgImageSourceLoadStatus;

  // Windows.UI.Xaml.Media.LoadedImageSourceLoadStatus
  LoadedImageSourceLoadStatus = (
    Success = 0,
    NetworkError = 1,
    InvalidFormat = 2,
    Other = 3
  );
  PLoadedImageSourceLoadStatus = ^LoadedImageSourceLoadStatus;

  // Windows.UI.Xaml.Media.PenLineCap
  PenLineCap = (
    Flat = 0,
    Square = 1,
    Round = 2,
    Triangle = 3
  );
  PPenLineCap = ^PenLineCap;

  // Windows.UI.Xaml.Media.PenLineJoin
  PenLineJoin = (
    Miter = 0,
    Bevel = 1,
    Round = 2
  );
  PPenLineJoin = ^PenLineJoin;

  // Windows.UI.Xaml.Media.RevealBrushState
  RevealBrushState = (
    Normal = 0,
    PointerOver = 1,
    Pressed = 2
  );
  PRevealBrushState = ^RevealBrushState;

  // Windows.UI.Xaml.Media.Stretch
  Stretch = (
    None = 0,
    Fill = 1,
    Uniform = 2,
    UniformToFill = 3
  );
  PStretch = ^Stretch;

  // Windows.UI.Xaml.Media.StyleSimulations
  StyleSimulations = (
    None = 0,
    BoldSimulation = 1,
    ItalicSimulation = 2,
    BoldItalicSimulation = 3
  );
  PStyleSimulations = ^StyleSimulations;

  // Windows.UI.Xaml.Media.SweepDirection
  SweepDirection = (
    Counterclockwise = 0,
    Clockwise = 1
  );
  PSweepDirection = ^SweepDirection;

  // Windows.UI.Xaml.Media Records
  // Windows.UI.Xaml.Media.Animation.KeyTime
  Animation_KeyTime = record
    TimeSpan: TimeSpan;
  end;
  PAnimation_KeyTime = ^Animation_KeyTime;

  // Windows.UI.Xaml.Media.Animation.RepeatBehavior
  Animation_RepeatBehavior = record
    Count: Double;
    Duration: TimeSpan;
    &Type: Animation_RepeatBehaviorType;
  end;
  PAnimation_RepeatBehavior = ^Animation_RepeatBehavior;

  // Windows.UI.Xaml.Media.Matrix
  Matrix = record
    M11: Double;
    M12: Double;
    M21: Double;
    M22: Double;
    OffsetX: Double;
    OffsetY: Double;
  end;
  PMatrix = ^Matrix;

  // Windows.UI.Xaml.Media.Media3D.Matrix3D
  Media3D_Matrix3D = record
    M11: Double;
    M12: Double;
    M13: Double;
    M14: Double;
    M21: Double;
    M22: Double;
    M23: Double;
    M24: Double;
    M31: Double;
    M32: Double;
    M33: Double;
    M34: Double;
    OffsetX: Double;
    OffsetY: Double;
    OffsetZ: Double;
    M44: Double;
  end;
  PMedia3D_Matrix3D = ^Media3D_Matrix3D;

  // Windows.UI.Xaml.Media Interfaces

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IIterator_1__Animation_ITransition_Base = interface(IInspectable)
  ['{0F149913-D622-5DDE-B6E3-7835C2AB54B7}']
    function get_Current: Animation_ITransition; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAnimation_ITransition): Cardinal; safecall;
    property Current: Animation_ITransition read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IIterator_1__Animation_ITransition = interface(IIterator_1__Animation_ITransition_Base)
  ['{F74BC733-47FC-58D8-B415-FAFACC549900}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IIterable_1__Animation_ITransition_Base = interface(IInspectable)
  ['{FEB51398-4FDB-5112-8A9B-6A8786CA01CE}']
    function First: IIterator_1__Animation_ITransition; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITransition>
  IIterable_1__Animation_ITransition = interface(IIterable_1__Animation_ITransition_Base)
  ['{CA24DB0E-3CED-5D24-9AA4-7ED85BDF3A01}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IFontFamily
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_FontFamily)]
  IFontFamily = interface(IInspectable)
  ['{92467E64-D66A-4CF4-9322-3D23B3C0C361}']
    function get_Source: HSTRING; safecall;
    property Source: HSTRING read get_Source;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IIterator_1__ITimelineMarker_Base = interface(IInspectable)
  ['{63E1E513-FCA3-51C0-8C2B-09DB5F8F4C40}']
    function get_Current: ITimelineMarker; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PITimelineMarker): Cardinal; safecall;
    property Current: ITimelineMarker read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IIterator_1__ITimelineMarker = interface(IIterator_1__ITimelineMarker_Base)
  ['{3ADD1E76-8C51-5063-A778-DF1CB3B4A033}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IIterable_1__ITimelineMarker_Base = interface(IInspectable)
  ['{946C5AF5-4C09-5784-9CF2-506947378E8E}']
    function First: IIterator_1__ITimelineMarker; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITimelineMarker>
  IIterable_1__ITimelineMarker = interface(IIterable_1__ITimelineMarker_Base)
  ['{7C3980E7-2D55-5C2C-9585-8A1CA26048B3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IBrush>
  IIterator_1__IBrush_Base = interface(IInspectable)
  ['{5D4853D4-3F43-5ABA-A074-F21B30927BF6}']
    function get_Current: IBrush; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIBrush): Cardinal; safecall;
    property Current: IBrush read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IBrush>
  IIterator_1__IBrush = interface(IIterator_1__IBrush_Base)
  ['{B6E49D83-3240-56D6-A991-C36C50CF19CF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IBrush>
  IIterable_1__IBrush_Base = interface(IInspectable)
  ['{B07E92D5-06B3-5DD2-8D49-349FFBD84634}']
    function First: IIterator_1__IBrush; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IBrush>
  IIterable_1__IBrush = interface(IIterable_1__IBrush_Base)
  ['{DA99F188-EB7C-5015-9BEE-EEC72E7A3724}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IBrush>
  IVectorView_1__IBrush = interface(IInspectable)
  ['{F1578261-3822-59E8-A020-3F86B6970E8D}']
    function GetAt(index: Cardinal): IBrush; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IBrush; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBrush): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IBrush>
  IVector_1__IBrush_Base = interface(IInspectable)
  ['{A8CD84E4-3EDF-5172-8F77-10E910E5DC5D}']
    function GetAt(index: Cardinal): IBrush; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IBrush; safecall;
    function IndexOf(value: IBrush; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IBrush); safecall;
    procedure InsertAt(index: Cardinal; value: IBrush); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IBrush); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBrush): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIBrush); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IBrush>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_BrushCollection)]
  IVector_1__IBrush = interface(IVector_1__IBrush_Base)
  ['{BB4C3F64-25AD-5091-8470-D5F79B4ECCDA}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PartialMediaFailureDetectedEventArgs)]
  IPartialMediaFailureDetectedEventArgs = interface(IInspectable)
  ['{02B65A91-E5A1-442B-88D3-2DC127BFC59B}']
    function get_StreamKind: Playback_FailedMediaStreamKind; safecall;
    property StreamKind: Playback_FailedMediaStreamKind read get_StreamKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IMediaElement,Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs>
  TypedEventHandler_2__IMediaElement__IPartialMediaFailureDetectedEventArgs_Delegate_Base = interface(IUnknown)
  ['{B402B0CA-BECA-5537-8192-8F30F7CB5D0E}']
    procedure Invoke(sender: IMediaElement; args: IPartialMediaFailureDetectedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IMediaElement,Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs>
  TypedEventHandler_2__IMediaElement__IPartialMediaFailureDetectedEventArgs = interface(TypedEventHandler_2__IMediaElement__IPartialMediaFailureDetectedEventArgs_Delegate_Base)
  ['{5A94EDA6-0BE6-5347-BC7E-6D598C0BB834}']
  end;

  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IMediaTransportControlsThumbnailRequestedEventArgs
  IMediaTransportControlsThumbnailRequestedEventArgs = interface(IInspectable)
  ['{E4A8B21C-E3C2-485C-AE69-F1537B76755A}']
    procedure SetThumbnailImage(source: IInputStream); safecall;
    function GetDeferral: IDeferral; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IMediaTransportControls,Windows.UI.Xaml.Media.IMediaTransportControlsThumbnailRequestedEventArgs>
  TypedEventHandler_2__IMediaTransportControls__IMediaTransportControlsThumbnailRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{C2925FF8-71F0-59D3-BA13-862B226EEBA2}']
    procedure Invoke(sender: IMediaTransportControls; args: IMediaTransportControlsThumbnailRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Controls.IMediaTransportControls,Windows.UI.Xaml.Media.IMediaTransportControlsThumbnailRequestedEventArgs>
  TypedEventHandler_2__IMediaTransportControls__IMediaTransportControlsThumbnailRequestedEventArgs = interface(TypedEventHandler_2__IMediaTransportControls__IMediaTransportControlsThumbnailRequestedEventArgs_Delegate_Base)
  ['{90565EC5-98A4-5206-B038-EC3E98D786D8}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ISolidColorBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_SolidColorBrush)]
  ISolidColorBrush = interface(IInspectable)
  ['{9D850850-66F3-48DF-9A8F-824BD5E070AF}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    property Color_: Color read get_Color write put_Color;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGeometry
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Geometry)]
  IGeometry = interface(IInspectable)
  ['{FA123889-0ACD-417B-B62D-5CA1BF4DFC0E}']
    function get_Transform: ITransform; safecall;
    procedure put_Transform(value: ITransform); safecall;
    function get_Bounds: TRectF; safecall;
    property Bounds: TRectF read get_Bounds;
    property Transform: ITransform read get_Transform write put_Transform;
  end;

  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IDownloadProgressEventArgs
  Imaging_IDownloadProgressEventArgs = interface(IInspectable)
  ['{7311E0D4-FE94-4E70-9B90-CDD47AC23AFB}']
    function get_Progress: Integer; safecall;
    procedure put_Progress(value: Integer); safecall;
    property Progress: Integer read get_Progress write put_Progress;
  end;

  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.DownloadProgressEventHandler
  Imaging_DownloadProgressEventHandler = interface(IUnknown)
  ['{1ABAEE23-74EE-4CC7-99BA-B171E3CDA61E}']
    procedure Invoke(sender: IInspectable; e: Imaging_IDownloadProgressEventArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapImage
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapImage)]
  Imaging_IBitmapImage = interface(IInspectable)
  ['{31AF3271-E3B4-442D-A341-4C0226B2725B}']
    function get_CreateOptions: Imaging_BitmapCreateOptions; safecall;
    procedure put_CreateOptions(value: Imaging_BitmapCreateOptions); safecall;
    function get_UriSource: IUriRuntimeClass; safecall;
    procedure put_UriSource(value: IUriRuntimeClass); safecall;
    function get_DecodePixelWidth: Integer; safecall;
    procedure put_DecodePixelWidth(value: Integer); safecall;
    function get_DecodePixelHeight: Integer; safecall;
    procedure put_DecodePixelHeight(value: Integer); safecall;
    function add_DownloadProgress(handler: Imaging_DownloadProgressEventHandler): EventRegistrationToken; safecall;
    procedure remove_DownloadProgress(token: EventRegistrationToken); safecall;
    function add_ImageOpened(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ImageOpened(token: EventRegistrationToken); safecall;
    function add_ImageFailed(handler: ExceptionRoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ImageFailed(token: EventRegistrationToken); safecall;
    property CreateOptions: Imaging_BitmapCreateOptions read get_CreateOptions write put_CreateOptions;
    property DecodePixelHeight: Integer read get_DecodePixelHeight write put_DecodePixelHeight;
    property DecodePixelWidth: Integer read get_DecodePixelWidth write put_DecodePixelWidth;
    property UriSource: IUriRuntimeClass read get_UriSource write put_UriSource;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IShadow
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Shadow)]
  IShadow = interface(IInspectable)
  ['{6813A583-F3B4-5FCF-8694-2CD0AEFC2FAD}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.ITransform3D
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_Transform3D)]
  Media3D_ITransform3D = interface(IInspectable)
  ['{AE3ED43A-A9FC-4C31-86CD-56D9CA251A69}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlLight
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_XamlLight)]
  IXamlLight = interface(IInspectable)
  ['{0CC3FC1F-B327-4A18-9648-7C84DB26CE22}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IXamlLight>
  IIterator_1__IXamlLight_Base = interface(IInspectable)
  ['{AF1B4914-A3E9-5F74-B04D-E086CE23ABA6}']
    function get_Current: IXamlLight; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIXamlLight): Cardinal; safecall;
    property Current: IXamlLight read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IXamlLight>
  IIterator_1__IXamlLight = interface(IIterator_1__IXamlLight_Base)
  ['{7517D16D-AA87-53C8-82B0-B34F1F140377}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IXamlLight>
  IIterable_1__IXamlLight_Base = interface(IInspectable)
  ['{03229EAD-2BA2-5101-9324-A2649DB7E61D}']
    function First: IIterator_1__IXamlLight; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IXamlLight>
  IIterable_1__IXamlLight = interface(IIterable_1__IXamlLight_Base)
  ['{9A552E06-4984-574D-8B9F-9542117D2DD7}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IXamlLight>
  IVectorView_1__IXamlLight = interface(IInspectable)
  ['{CCC95943-F1D7-5B54-877A-1F734CC5C36B}']
    function GetAt(index: Cardinal): IXamlLight; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IXamlLight; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIXamlLight): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IXamlLight>
  IVector_1__IXamlLight_Base = interface(IInspectable)
  ['{883F90FF-8F3E-5DD2-947C-D7B305DAA504}']
    function GetAt(index: Cardinal): IXamlLight; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IXamlLight; safecall;
    function IndexOf(value: IXamlLight; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IXamlLight); safecall;
    procedure InsertAt(index: Cardinal; value: IXamlLight); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IXamlLight); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIXamlLight): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIXamlLight); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IXamlLight>
  IVector_1__IXamlLight = interface(IVector_1__IXamlLight_Base)
  ['{55FDE18A-0E7A-520E-A7C8-575CF843C4F9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ITimeline
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_Timeline)]
  Animation_ITimeline = interface(IInspectable)
  ['{0BC465DC-BE4D-4D0D-9549-2208B715F40D}']
    function get_AutoReverse: Boolean; safecall;
    procedure put_AutoReverse(value: Boolean); safecall;
    function get_BeginTime: IReference_1__TimeSpan; safecall;
    procedure put_BeginTime(value: IReference_1__TimeSpan); safecall;
    function get_Duration: Duration; safecall;
    procedure put_Duration(value: Duration); safecall;
    function get_SpeedRatio: Double; safecall;
    procedure put_SpeedRatio(value: Double); safecall;
    function get_FillBehavior: Animation_FillBehavior; safecall;
    procedure put_FillBehavior(value: Animation_FillBehavior); safecall;
    function get_RepeatBehavior: Animation_RepeatBehavior; safecall;
    procedure put_RepeatBehavior(value: Animation_RepeatBehavior); safecall;
    function add_Completed(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Completed(token: EventRegistrationToken); safecall;
    property AutoReverse: Boolean read get_AutoReverse write put_AutoReverse;
    property BeginTime: IReference_1__TimeSpan read get_BeginTime write put_BeginTime;
    property Duration_: Duration read get_Duration write put_Duration;
    property FillBehavior: Animation_FillBehavior read get_FillBehavior write put_FillBehavior;
    property RepeatBehavior: Animation_RepeatBehavior read get_RepeatBehavior write put_RepeatBehavior;
    property SpeedRatio: Double read get_SpeedRatio write put_SpeedRatio;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IIterator_1__Animation_ITimeline_Base = interface(IInspectable)
  ['{B0C22C25-AE1A-5326-BF81-394246CC3076}']
    function get_Current: Animation_ITimeline; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAnimation_ITimeline): Cardinal; safecall;
    property Current: Animation_ITimeline read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IIterator_1__Animation_ITimeline = interface(IIterator_1__Animation_ITimeline_Base)
  ['{B63978B8-F348-5AD1-9B5C-10BF7366312F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IIterable_1__Animation_ITimeline_Base = interface(IInspectable)
  ['{A122A346-E6D5-5C54-857D-038E60F5D9C1}']
    function First: IIterator_1__Animation_ITimeline; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IIterable_1__Animation_ITimeline = interface(IIterable_1__Animation_ITimeline_Base)
  ['{94688718-170E-55B4-A769-6EC9485E5871}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IVectorView_1__Animation_ITimeline = interface(IInspectable)
  ['{207820A4-87D8-531D-8874-8A36AB5B3278}']
    function GetAt(index: Cardinal): Animation_ITimeline; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Animation_ITimeline; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_ITimeline): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  IVector_1__Animation_ITimeline_Base = interface(IInspectable)
  ['{2005C7F9-C7D7-521B-A81C-2C9DAD77A9E8}']
    function GetAt(index: Cardinal): Animation_ITimeline; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Animation_ITimeline; safecall;
    function IndexOf(value: Animation_ITimeline; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Animation_ITimeline); safecall;
    procedure InsertAt(index: Cardinal; value: Animation_ITimeline); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Animation_ITimeline); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_ITimeline): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PAnimation_ITimeline); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_TimelineCollection)]
  IVector_1__Animation_ITimeline = interface(IVector_1__Animation_ITimeline_Base)
  ['{687E07ED-B91E-5171-8929-527FBB0E4726}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IStoryboard
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_Storyboard)]
  Animation_IStoryboard = interface(IInspectable)
  ['{D45C1E6E-3594-460E-981A-32271BD3AA06}']
    function get_Children: IVector_1__Animation_ITimeline; safecall;
    procedure Seek(offset: TimeSpan); safecall;
    procedure Stop; safecall;
    procedure &Begin; safecall;
    procedure Pause; safecall;
    procedure Resume; safecall;
    function GetCurrentState: Animation_ClockState; safecall;
    function GetCurrentTime_: TimeSpan; safecall;
    procedure SeekAlignedToLastTick(offset: TimeSpan); safecall;
    procedure SkipToFill; safecall;
    property Children: IVector_1__Animation_ITimeline read get_Children;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingFunctionBase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingFunctionBase)]
  Animation_IEasingFunctionBase = interface(IInspectable)
  ['{C108383F-2C02-4151-8ECD-68DDAA3F0D9B}']
    function get_EasingMode: Animation_EasingMode; safecall;
    procedure put_EasingMode(value: Animation_EasingMode); safecall;
    function Ease(normalizedTime: Double): Double; safecall;
    property EasingMode: Animation_EasingMode read get_EasingMode write put_EasingMode;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IAddDeleteThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_AddDeleteThemeTransition)]
  Animation_IAddDeleteThemeTransition = interface(IInspectable)
  ['{ADEC852E-4424-4DAB-99C1-3A04E36A3C48}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBackEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BackEase)]
  Animation_IBackEase = interface(IInspectable)
  ['{E47796E7-F805-4A8F-81C9-38E6472CAA94}']
    function get_Amplitude: Double; safecall;
    procedure put_Amplitude(value: Double); safecall;
    property Amplitude: Double read get_Amplitude write put_Amplitude;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBackEaseStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BackEase)]
  Animation_IBackEaseStatics = interface(IInspectable)
  ['{3C70A2FF-A0A0-4786-926C-22321F8F25B7}']
    function get_AmplitudeProperty: IDependencyProperty; safecall;
    property AmplitudeProperty: IDependencyProperty read get_AmplitudeProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBasicConnectedAnimationConfiguration
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BasicConnectedAnimationConfiguration)]
  Animation_IBasicConnectedAnimationConfiguration = interface(IInspectable)
  ['{E675F9B5-A4D6-5353-83E6-C89E7CF8D456}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBasicConnectedAnimationConfigurationFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BasicConnectedAnimationConfiguration)]
  Animation_IBasicConnectedAnimationConfigurationFactory = interface(IInspectable)
  ['{95E6844A-4377-503C-BEE2-11DFCD5570E6}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IBasicConnectedAnimationConfiguration; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBeginStoryboard
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BeginStoryboard)]
  Animation_IBeginStoryboard = interface(IInspectable)
  ['{64189FCD-49EC-4E52-A6F6-55324C921053}']
    function get_Storyboard: Animation_IStoryboard; safecall;
    procedure put_Storyboard(value: Animation_IStoryboard); safecall;
    property Storyboard: Animation_IStoryboard read get_Storyboard write put_Storyboard;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBeginStoryboardStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BeginStoryboard)]
  Animation_IBeginStoryboardStatics = interface(IInspectable)
  ['{12CFF18C-AA91-4C4A-B82F-DF34FC57F94B}']
    function get_StoryboardProperty: IDependencyProperty; safecall;
    property StoryboardProperty: IDependencyProperty read get_StoryboardProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBounceEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BounceEase)]
  Animation_IBounceEase = interface(IInspectable)
  ['{2BF1464E-FC71-47ED-85A1-3BA9577718B4}']
    function get_Bounces: Integer; safecall;
    procedure put_Bounces(value: Integer); safecall;
    function get_Bounciness: Double; safecall;
    procedure put_Bounciness(value: Double); safecall;
    property Bounces: Integer read get_Bounces write put_Bounces;
    property Bounciness: Double read get_Bounciness write put_Bounciness;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IBounceEaseStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_BounceEase)]
  Animation_IBounceEaseStatics = interface(IInspectable)
  ['{C0701DA2-4F73-41C9-B2CB-2EA3105107FF}']
    function get_BouncesProperty: IDependencyProperty; safecall;
    function get_BouncinessProperty: IDependencyProperty; safecall;
    property BouncesProperty: IDependencyProperty read get_BouncesProperty;
    property BouncinessProperty: IDependencyProperty read get_BouncinessProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ICircleEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_CircleEase)]
  Animation_ICircleEase = interface(IInspectable)
  ['{53A3BDB2-9177-4E6E-A043-5082D889AB1F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IColorAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorAnimation)]
  Animation_IColorAnimation = interface(IInspectable)
  ['{B8AE8A15-0F63-4694-9467-BDAFAC1253EA}']
    function get_From: IReference_1__Color; safecall;
    procedure put_From(value: IReference_1__Color); safecall;
    function get_To: IReference_1__Color; safecall;
    procedure put_To(value: IReference_1__Color); safecall;
    function get_By: IReference_1__Color; safecall;
    procedure put_By(value: IReference_1__Color); safecall;
    function get_EasingFunction: Animation_IEasingFunctionBase; safecall;
    procedure put_EasingFunction(value: Animation_IEasingFunctionBase); safecall;
    function get_EnableDependentAnimation: Boolean; safecall;
    procedure put_EnableDependentAnimation(value: Boolean); safecall;
    property By: IReference_1__Color read get_By write put_By;
    property EasingFunction: Animation_IEasingFunctionBase read get_EasingFunction write put_EasingFunction;
    property EnableDependentAnimation: Boolean read get_EnableDependentAnimation write put_EnableDependentAnimation;
    property From: IReference_1__Color read get_From write put_From;
    property &To: IReference_1__Color read get_To write put_To;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IColorAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorAnimation)]
  Animation_IColorAnimationStatics = interface(IInspectable)
  ['{55EAF6E2-87E3-4F48-958F-855B2F9EA9EC}']
    function get_FromProperty: IDependencyProperty; safecall;
    function get_ToProperty: IDependencyProperty; safecall;
    function get_ByProperty: IDependencyProperty; safecall;
    function get_EasingFunctionProperty: IDependencyProperty; safecall;
    function get_EnableDependentAnimationProperty: IDependencyProperty; safecall;
    property ByProperty: IDependencyProperty read get_ByProperty;
    property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
    property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
    property FromProperty: IDependencyProperty read get_FromProperty;
    property ToProperty: IDependencyProperty read get_ToProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IColorKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorKeyFrame)]
  Animation_IColorKeyFrame = interface(IInspectable)
  ['{B51D82D9-0910-4589-A284-B0C9205858E9}']
    function get_Value: Color; safecall;
    procedure put_Value(value: Color); safecall;
    function get_KeyTime: Animation_KeyTime; safecall;
    procedure put_KeyTime(value: Animation_KeyTime); safecall;
    property KeyTime: Animation_KeyTime read get_KeyTime write put_KeyTime;
    property Value: Color read get_Value write put_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IIterator_1__Animation_IColorKeyFrame_Base = interface(IInspectable)
  ['{36A6440D-AA86-5BCE-8DF2-5DE99206F151}']
    function get_Current: Animation_IColorKeyFrame; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAnimation_IColorKeyFrame): Cardinal; safecall;
    property Current: Animation_IColorKeyFrame read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IIterator_1__Animation_IColorKeyFrame = interface(IIterator_1__Animation_IColorKeyFrame_Base)
  ['{6F401756-EE50-5758-AF39-162F92F72397}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IIterable_1__Animation_IColorKeyFrame_Base = interface(IInspectable)
  ['{1859DD08-582D-51DC-82A1-466111CAF944}']
    function First: IIterator_1__Animation_IColorKeyFrame; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IIterable_1__Animation_IColorKeyFrame = interface(IIterable_1__Animation_IColorKeyFrame_Base)
  ['{153210AD-06C7-537C-AD97-EB3040EB58C6}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IVectorView_1__Animation_IColorKeyFrame = interface(IInspectable)
  ['{1326AD03-C6D3-5BD1-9F0D-4967C58FCF2F}']
    function GetAt(index: Cardinal): Animation_IColorKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Animation_IColorKeyFrame; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IColorKeyFrame): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  IVector_1__Animation_IColorKeyFrame_Base = interface(IInspectable)
  ['{92D24FB8-AF54-5180-9888-5756566A13FF}']
    function GetAt(index: Cardinal): Animation_IColorKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Animation_IColorKeyFrame; safecall;
    function IndexOf(value: Animation_IColorKeyFrame; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Animation_IColorKeyFrame); safecall;
    procedure InsertAt(index: Cardinal; value: Animation_IColorKeyFrame); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Animation_IColorKeyFrame); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IColorKeyFrame): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PAnimation_IColorKeyFrame); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorKeyFrameCollection)]
  IVector_1__Animation_IColorKeyFrame = interface(IVector_1__Animation_IColorKeyFrame_Base)
  ['{FDF2163C-0939-5FA5-A73C-D21399812E61}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IColorAnimationUsingKeyFrames
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorAnimationUsingKeyFrames)]
  Animation_IColorAnimationUsingKeyFrames = interface(IInspectable)
  ['{F5C82640-13C3-42AA-9AE2-7E6B51C92F95}']
    function get_KeyFrames: IVector_1__Animation_IColorKeyFrame; safecall;
    function get_EnableDependentAnimation: Boolean; safecall;
    procedure put_EnableDependentAnimation(value: Boolean); safecall;
    property EnableDependentAnimation: Boolean read get_EnableDependentAnimation write put_EnableDependentAnimation;
    property KeyFrames: IVector_1__Animation_IColorKeyFrame read get_KeyFrames;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IColorAnimationUsingKeyFramesStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorAnimationUsingKeyFrames)]
  Animation_IColorAnimationUsingKeyFramesStatics = interface(IInspectable)
  ['{B4723CDC-96E9-48F9-8D92-9B648B2F1CC6}']
    function get_EnableDependentAnimationProperty: IDependencyProperty; safecall;
    property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IColorKeyFrameFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorKeyFrame)]
  Animation_IColorKeyFrameFactory = interface(IInspectable)
  ['{769BD88A-9CFB-4A7D-96C4-A1E7DE6FDB4B}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IColorKeyFrame; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IColorKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ColorKeyFrame)]
  Animation_IColorKeyFrameStatics = interface(IInspectable)
  ['{C043AE99-210C-430F-9DA5-DF1082692055}']
    function get_ValueProperty: IDependencyProperty; safecall;
    function get_KeyTimeProperty: IDependencyProperty; safecall;
    property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    property ValueProperty: IDependencyProperty read get_ValueProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ICommonNavigationTransitionInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_CommonNavigationTransitionInfo)]
  Animation_ICommonNavigationTransitionInfo = interface(IInspectable)
  ['{50345692-A555-4624-A361-0A91C1706473}']
    function get_IsStaggeringEnabled: Boolean; safecall;
    procedure put_IsStaggeringEnabled(value: Boolean); safecall;
    property IsStaggeringEnabled: Boolean read get_IsStaggeringEnabled write put_IsStaggeringEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ICommonNavigationTransitionInfoStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_CommonNavigationTransitionInfo)]
  Animation_ICommonNavigationTransitionInfoStatics = interface(IInspectable)
  ['{1E3EFE33-50BE-4443-883C-E5627201C2E5}']
    function get_IsStaggeringEnabledProperty: IDependencyProperty; safecall;
    function get_IsStaggerElementProperty: IDependencyProperty; safecall;
    function GetIsStaggerElement(element: IUIElement): Boolean; safecall;
    procedure SetIsStaggerElement(element: IUIElement; value: Boolean); safecall;
    property IsStaggerElementProperty: IDependencyProperty read get_IsStaggerElementProperty;
    property IsStaggeringEnabledProperty: IDependencyProperty read get_IsStaggeringEnabledProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimation2
  Animation_IConnectedAnimation2 = interface(IInspectable)
  ['{5D2F8E5C-584B-4DDD-B668-973891431459}']
    function get_IsScaleAnimationEnabled: Boolean; safecall;
    procedure put_IsScaleAnimationEnabled(value: Boolean); safecall;
    function TryStart(destination: IUIElement; coordinatedElements: IIterable_1__IUIElement): Boolean; safecall;
    procedure SetAnimationComponent(component: Animation_ConnectedAnimationComponent; animation: ICompositionAnimationBase); safecall;
    property IsScaleAnimationEnabled: Boolean read get_IsScaleAnimationEnabled write put_IsScaleAnimationEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationConfiguration
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ConnectedAnimationConfiguration)]
  Animation_IConnectedAnimationConfiguration = interface(IInspectable)
  ['{00218AAE-CD8C-5651-92A0-C1DB95C03998}']
  end;

  // Windows.UI.Xaml.Media.Animation.IConnectedAnimation3
  Animation_IConnectedAnimation3 = interface(IInspectable)
  ['{6E3040C6-0430-59C0-A80C-CCEED2E778DD}']
    function get_Configuration: Animation_IConnectedAnimationConfiguration; safecall;
    procedure put_Configuration(value: Animation_IConnectedAnimationConfiguration); safecall;
    property Configuration: Animation_IConnectedAnimationConfiguration read get_Configuration write put_Configuration;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationConfigurationFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ConnectedAnimationConfiguration)]
  Animation_IConnectedAnimationConfigurationFactory = interface(IInspectable)
  ['{30F9B84B-DD7E-593E-BF75-E959DC0EC52A}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationService
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ConnectedAnimationService)]
  Animation_IConnectedAnimationService = interface(IInspectable)
  ['{1C6875C9-19BB-4D47-B9AA-66C802DCB9FF}']
    function get_DefaultDuration: TimeSpan; safecall;
    procedure put_DefaultDuration(value: TimeSpan); safecall;
    function get_DefaultEasingFunction: ICompositionEasingFunction; safecall;
    procedure put_DefaultEasingFunction(value: ICompositionEasingFunction); safecall;
    function PrepareToAnimate(key: HSTRING; source: IUIElement): Animation_IConnectedAnimation; safecall;
    function GetAnimation(key: HSTRING): Animation_IConnectedAnimation; safecall;
    property DefaultDuration: TimeSpan read get_DefaultDuration write put_DefaultDuration;
    property DefaultEasingFunction: ICompositionEasingFunction read get_DefaultEasingFunction write put_DefaultEasingFunction;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IConnectedAnimationServiceStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ConnectedAnimationService)]
  Animation_IConnectedAnimationServiceStatics = interface(IInspectable)
  ['{C7078EA5-D688-40E8-8F90-96A6279273D2}']
    function GetForCurrentView: Animation_IConnectedAnimationService; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IContentThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ContentThemeTransition)]
  Animation_IContentThemeTransition = interface(IInspectable)
  ['{F66FC5C3-5915-437D-8E3B-ADF8E7F0AB57}']
    function get_HorizontalOffset: Double; safecall;
    procedure put_HorizontalOffset(value: Double); safecall;
    function get_VerticalOffset: Double; safecall;
    procedure put_VerticalOffset(value: Double); safecall;
    property HorizontalOffset: Double read get_HorizontalOffset write put_HorizontalOffset;
    property VerticalOffset: Double read get_VerticalOffset write put_VerticalOffset;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IContentThemeTransitionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ContentThemeTransition)]
  Animation_IContentThemeTransitionStatics = interface(IInspectable)
  ['{0E8EE385-9A42-4459-AFA9-337DC41E1587}']
    function get_HorizontalOffsetProperty: IDependencyProperty; safecall;
    function get_VerticalOffsetProperty: IDependencyProperty; safecall;
    property HorizontalOffsetProperty: IDependencyProperty read get_HorizontalOffsetProperty;
    property VerticalOffsetProperty: IDependencyProperty read get_VerticalOffsetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IContinuumNavigationTransitionInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ContinuumNavigationTransitionInfo)]
  Animation_IContinuumNavigationTransitionInfo = interface(IInspectable)
  ['{4BE1DBAD-8BA6-4004-8438-8A9017978543}']
    function get_ExitElement: IUIElement; safecall;
    procedure put_ExitElement(value: IUIElement); safecall;
    property ExitElement: IUIElement read get_ExitElement write put_ExitElement;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IContinuumNavigationTransitionInfoStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ContinuumNavigationTransitionInfo)]
  Animation_IContinuumNavigationTransitionInfoStatics = interface(IInspectable)
  ['{3E25DD53-B18F-4BF1-B3BC-92F516F29903}']
    function get_ExitElementProperty: IDependencyProperty; safecall;
    function get_IsEntranceElementProperty: IDependencyProperty; safecall;
    function GetIsEntranceElement(element: IUIElement): Boolean; safecall;
    procedure SetIsEntranceElement(element: IUIElement; value: Boolean); safecall;
    function get_IsExitElementProperty: IDependencyProperty; safecall;
    function GetIsExitElement(element: IUIElement): Boolean; safecall;
    procedure SetIsExitElement(element: IUIElement; value: Boolean); safecall;
    function get_ExitElementContainerProperty: IDependencyProperty; safecall;
    function GetExitElementContainer(element: IListViewBase): Boolean; safecall;
    procedure SetExitElementContainer(element: IListViewBase; value: Boolean); safecall;
    property ExitElementContainerProperty: IDependencyProperty read get_ExitElementContainerProperty;
    property ExitElementProperty: IDependencyProperty read get_ExitElementProperty;
    property IsEntranceElementProperty: IDependencyProperty read get_IsEntranceElementProperty;
    property IsExitElementProperty: IDependencyProperty read get_IsExitElementProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ICubicEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_CubicEase)]
  Animation_ICubicEase = interface(IInspectable)
  ['{1B94FC76-DAD7-4354-B1A2-7969FBF6A70D}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDirectConnectedAnimationConfiguration
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DirectConnectedAnimationConfiguration)]
  Animation_IDirectConnectedAnimationConfiguration = interface(IInspectable)
  ['{EE5D736F-5738-5D86-B770-151948CF365E}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDirectConnectedAnimationConfigurationFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DirectConnectedAnimationConfiguration)]
  Animation_IDirectConnectedAnimationConfigurationFactory = interface(IInspectable)
  ['{059263E9-D2B3-5A77-9CF4-E26D8B542608}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IDirectConnectedAnimationConfiguration; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDiscreteColorKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DiscreteColorKeyFrame)]
  Animation_IDiscreteColorKeyFrame = interface(IInspectable)
  ['{230C08F4-E062-4CB1-8E2A-14093D73ED8C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDiscreteDoubleKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DiscreteDoubleKeyFrame)]
  Animation_IDiscreteDoubleKeyFrame = interface(IInspectable)
  ['{F5F51F3A-AD11-49CE-8E1C-08FDF1447446}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDiscreteObjectKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DiscreteObjectKeyFrame)]
  Animation_IDiscreteObjectKeyFrame = interface(IInspectable)
  ['{C7DCDE89-F12D-4A9C-8199-E7A9ECE3A473}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDiscretePointKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DiscretePointKeyFrame)]
  Animation_IDiscretePointKeyFrame = interface(IInspectable)
  ['{E0A9070D-4C42-4A90-983A-75F5A83A2FBE}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDoubleAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleAnimation)]
  Animation_IDoubleAnimation = interface(IInspectable)
  ['{7E9F3D59-0F07-4BC9-977D-03763FF8154F}']
    function get_From: IReference_1__Double; safecall;
    procedure put_From(value: IReference_1__Double); safecall;
    function get_To: IReference_1__Double; safecall;
    procedure put_To(value: IReference_1__Double); safecall;
    function get_By: IReference_1__Double; safecall;
    procedure put_By(value: IReference_1__Double); safecall;
    function get_EasingFunction: Animation_IEasingFunctionBase; safecall;
    procedure put_EasingFunction(value: Animation_IEasingFunctionBase); safecall;
    function get_EnableDependentAnimation: Boolean; safecall;
    procedure put_EnableDependentAnimation(value: Boolean); safecall;
    property By: IReference_1__Double read get_By write put_By;
    property EasingFunction: Animation_IEasingFunctionBase read get_EasingFunction write put_EasingFunction;
    property EnableDependentAnimation: Boolean read get_EnableDependentAnimation write put_EnableDependentAnimation;
    property From: IReference_1__Double read get_From write put_From;
    property &To: IReference_1__Double read get_To write put_To;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDoubleAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleAnimation)]
  Animation_IDoubleAnimationStatics = interface(IInspectable)
  ['{E27A935D-F111-43B7-B824-832B58D7786B}']
    function get_FromProperty: IDependencyProperty; safecall;
    function get_ToProperty: IDependencyProperty; safecall;
    function get_ByProperty: IDependencyProperty; safecall;
    function get_EasingFunctionProperty: IDependencyProperty; safecall;
    function get_EnableDependentAnimationProperty: IDependencyProperty; safecall;
    property ByProperty: IDependencyProperty read get_ByProperty;
    property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
    property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
    property FromProperty: IDependencyProperty read get_FromProperty;
    property ToProperty: IDependencyProperty read get_ToProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleKeyFrame)]
  Animation_IDoubleKeyFrame = interface(IInspectable)
  ['{674456FD-E81E-4F4E-B4AD-0ACFED9ECD68}']
    function get_Value: Double; safecall;
    procedure put_Value(value: Double); safecall;
    function get_KeyTime: Animation_KeyTime; safecall;
    procedure put_KeyTime(value: Animation_KeyTime); safecall;
    property KeyTime: Animation_KeyTime read get_KeyTime write put_KeyTime;
    property Value: Double read get_Value write put_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IIterator_1__Animation_IDoubleKeyFrame_Base = interface(IInspectable)
  ['{CA007BBD-84A1-512F-977F-9BD728E1E73F}']
    function get_Current: Animation_IDoubleKeyFrame; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAnimation_IDoubleKeyFrame): Cardinal; safecall;
    property Current: Animation_IDoubleKeyFrame read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IIterator_1__Animation_IDoubleKeyFrame = interface(IIterator_1__Animation_IDoubleKeyFrame_Base)
  ['{C9150ECD-4460-5FD9-9675-B0B072AB4E97}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IIterable_1__Animation_IDoubleKeyFrame_Base = interface(IInspectable)
  ['{5F1676DA-A405-5B7A-BAF1-968DE4391FB7}']
    function First: IIterator_1__Animation_IDoubleKeyFrame; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IIterable_1__Animation_IDoubleKeyFrame = interface(IIterable_1__Animation_IDoubleKeyFrame_Base)
  ['{A07E52A7-554F-5651-AFD5-7FE13C41E018}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IVectorView_1__Animation_IDoubleKeyFrame = interface(IInspectable)
  ['{6A06AAC0-20B2-54CE-A2F0-D9DA051588F9}']
    function GetAt(index: Cardinal): Animation_IDoubleKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Animation_IDoubleKeyFrame; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IDoubleKeyFrame): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  IVector_1__Animation_IDoubleKeyFrame_Base = interface(IInspectable)
  ['{D7CBDE22-86BF-572F-8473-079D15076C3E}']
    function GetAt(index: Cardinal): Animation_IDoubleKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Animation_IDoubleKeyFrame; safecall;
    function IndexOf(value: Animation_IDoubleKeyFrame; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Animation_IDoubleKeyFrame); safecall;
    procedure InsertAt(index: Cardinal; value: Animation_IDoubleKeyFrame); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Animation_IDoubleKeyFrame); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IDoubleKeyFrame): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PAnimation_IDoubleKeyFrame); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleKeyFrameCollection)]
  IVector_1__Animation_IDoubleKeyFrame = interface(IVector_1__Animation_IDoubleKeyFrame_Base)
  ['{CBD60DFD-DDD0-500C-B656-DAFE026D3551}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDoubleAnimationUsingKeyFrames
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleAnimationUsingKeyFrames)]
  Animation_IDoubleAnimationUsingKeyFrames = interface(IInspectable)
  ['{4FEE628F-BFEE-4F75-83C2-A93B39488473}']
    function get_KeyFrames: IVector_1__Animation_IDoubleKeyFrame; safecall;
    function get_EnableDependentAnimation: Boolean; safecall;
    procedure put_EnableDependentAnimation(value: Boolean); safecall;
    property EnableDependentAnimation: Boolean read get_EnableDependentAnimation write put_EnableDependentAnimation;
    property KeyFrames: IVector_1__Animation_IDoubleKeyFrame read get_KeyFrames;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDoubleAnimationUsingKeyFramesStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleAnimationUsingKeyFrames)]
  Animation_IDoubleAnimationUsingKeyFramesStatics = interface(IInspectable)
  ['{109BF2F6-C60F-49AA-ABF6-F696D492116B}']
    function get_EnableDependentAnimationProperty: IDependencyProperty; safecall;
    property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDoubleKeyFrameFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleKeyFrame)]
  Animation_IDoubleKeyFrameFactory = interface(IInspectable)
  ['{AC97DEC3-7538-40B9-B152-696F7FBF4722}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IDoubleKeyFrame; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDoubleKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DoubleKeyFrame)]
  Animation_IDoubleKeyFrameStatics = interface(IInspectable)
  ['{324641B0-7D37-427A-ADEB-43F38BB61A4D}']
    function get_ValueProperty: IDependencyProperty; safecall;
    function get_KeyTimeProperty: IDependencyProperty; safecall;
    property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    property ValueProperty: IDependencyProperty read get_ValueProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDragItemThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DragItemThemeAnimation)]
  Animation_IDragItemThemeAnimation = interface(IInspectable)
  ['{0C7D5DB5-7ED6-4949-B4E6-A78C9F4F978D}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDragItemThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DragItemThemeAnimation)]
  Animation_IDragItemThemeAnimationStatics = interface(IInspectable)
  ['{6218B9F5-013A-4FB1-86FC-92BC4E8D0241}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDragOverThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DragOverThemeAnimation)]
  Animation_IDragOverThemeAnimation = interface(IInspectable)
  ['{72F762F7-7E51-4A6B-B937-DC4B4C1C5458}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    function get_ToOffset: Double; safecall;
    procedure put_ToOffset(value: Double); safecall;
    function get_Direction: Primitives_AnimationDirection; safecall;
    procedure put_Direction(value: Primitives_AnimationDirection); safecall;
    property Direction: Primitives_AnimationDirection read get_Direction write put_Direction;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
    property ToOffset: Double read get_ToOffset write put_ToOffset;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDragOverThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DragOverThemeAnimation)]
  Animation_IDragOverThemeAnimationStatics = interface(IInspectable)
  ['{146FFE57-3C9D-41D9-A5FF-8D7239516810}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    function get_ToOffsetProperty: IDependencyProperty; safecall;
    function get_DirectionProperty: IDependencyProperty; safecall;
    property DirectionProperty: IDependencyProperty read get_DirectionProperty;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
    property ToOffsetProperty: IDependencyProperty read get_ToOffsetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDrillInNavigationTransitionInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DrillInNavigationTransitionInfo)]
  Animation_IDrillInNavigationTransitionInfo = interface(IInspectable)
  ['{3B86201A-45D3-463B-939E-C8595F439BCC}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDrillInThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DrillInThemeAnimation)]
  Animation_IDrillInThemeAnimation = interface(IInspectable)
  ['{B090B824-F1D2-41B8-87BA-78034126594C}']
    function get_EntranceTargetName: HSTRING; safecall;
    procedure put_EntranceTargetName(value: HSTRING); safecall;
    function get_EntranceTarget: IDependencyObject; safecall;
    procedure put_EntranceTarget(value: IDependencyObject); safecall;
    function get_ExitTargetName: HSTRING; safecall;
    procedure put_ExitTargetName(value: HSTRING); safecall;
    function get_ExitTarget: IDependencyObject; safecall;
    procedure put_ExitTarget(value: IDependencyObject); safecall;
    property EntranceTarget: IDependencyObject read get_EntranceTarget write put_EntranceTarget;
    property EntranceTargetName: HSTRING read get_EntranceTargetName write put_EntranceTargetName;
    property ExitTarget: IDependencyObject read get_ExitTarget write put_ExitTarget;
    property ExitTargetName: HSTRING read get_ExitTargetName write put_ExitTargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDrillInThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DrillInThemeAnimation)]
  Animation_IDrillInThemeAnimationStatics = interface(IInspectable)
  ['{C61FE488-A17A-4B11-B53B-A4F1A07D4BA9}']
    function get_EntranceTargetNameProperty: IDependencyProperty; safecall;
    function get_EntranceTargetProperty: IDependencyProperty; safecall;
    function get_ExitTargetNameProperty: IDependencyProperty; safecall;
    function get_ExitTargetProperty: IDependencyProperty; safecall;
    property EntranceTargetNameProperty: IDependencyProperty read get_EntranceTargetNameProperty;
    property EntranceTargetProperty: IDependencyProperty read get_EntranceTargetProperty;
    property ExitTargetNameProperty: IDependencyProperty read get_ExitTargetNameProperty;
    property ExitTargetProperty: IDependencyProperty read get_ExitTargetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDrillOutThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DrillOutThemeAnimation)]
  Animation_IDrillOutThemeAnimation = interface(IInspectable)
  ['{D890CCDF-06D3-4F7E-8E4A-4FB76E256139}']
    function get_EntranceTargetName: HSTRING; safecall;
    procedure put_EntranceTargetName(value: HSTRING); safecall;
    function get_EntranceTarget: IDependencyObject; safecall;
    procedure put_EntranceTarget(value: IDependencyObject); safecall;
    function get_ExitTargetName: HSTRING; safecall;
    procedure put_ExitTargetName(value: HSTRING); safecall;
    function get_ExitTarget: IDependencyObject; safecall;
    procedure put_ExitTarget(value: IDependencyObject); safecall;
    property EntranceTarget: IDependencyObject read get_EntranceTarget write put_EntranceTarget;
    property EntranceTargetName: HSTRING read get_EntranceTargetName write put_EntranceTargetName;
    property ExitTarget: IDependencyObject read get_ExitTarget write put_ExitTarget;
    property ExitTargetName: HSTRING read get_ExitTargetName write put_ExitTargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDrillOutThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DrillOutThemeAnimation)]
  Animation_IDrillOutThemeAnimationStatics = interface(IInspectable)
  ['{BEB5DB9B-2617-4888-80DD-72FA7BB6FAC3}']
    function get_EntranceTargetNameProperty: IDependencyProperty; safecall;
    function get_EntranceTargetProperty: IDependencyProperty; safecall;
    function get_ExitTargetNameProperty: IDependencyProperty; safecall;
    function get_ExitTargetProperty: IDependencyProperty; safecall;
    property EntranceTargetNameProperty: IDependencyProperty read get_EntranceTargetNameProperty;
    property EntranceTargetProperty: IDependencyProperty read get_EntranceTargetProperty;
    property ExitTargetNameProperty: IDependencyProperty read get_ExitTargetNameProperty;
    property ExitTargetProperty: IDependencyProperty read get_ExitTargetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDropTargetItemThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DropTargetItemThemeAnimation)]
  Animation_IDropTargetItemThemeAnimation = interface(IInspectable)
  ['{1881C968-1824-462B-87E8-C357212B977B}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IDropTargetItemThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_DropTargetItemThemeAnimation)]
  Animation_IDropTargetItemThemeAnimationStatics = interface(IInspectable)
  ['{AE80F486-2E56-4513-BF18-D77470164AE5}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingColorKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingColorKeyFrame)]
  Animation_IEasingColorKeyFrame = interface(IInspectable)
  ['{C733D630-F4B9-4934-9BDD-27AC5ED1CFD8}']
    function get_EasingFunction: Animation_IEasingFunctionBase; safecall;
    procedure put_EasingFunction(value: Animation_IEasingFunctionBase); safecall;
    property EasingFunction: Animation_IEasingFunctionBase read get_EasingFunction write put_EasingFunction;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingColorKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingColorKeyFrame)]
  Animation_IEasingColorKeyFrameStatics = interface(IInspectable)
  ['{6F3837FC-8E3D-4522-9B0F-003DB8609851}']
    function get_EasingFunctionProperty: IDependencyProperty; safecall;
    property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingDoubleKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingDoubleKeyFrame)]
  Animation_IEasingDoubleKeyFrame = interface(IInspectable)
  ['{965ADB8D-9A54-4108-B4FF-B5A5212CB338}']
    function get_EasingFunction: Animation_IEasingFunctionBase; safecall;
    procedure put_EasingFunction(value: Animation_IEasingFunctionBase); safecall;
    property EasingFunction: Animation_IEasingFunctionBase read get_EasingFunction write put_EasingFunction;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingDoubleKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingDoubleKeyFrame)]
  Animation_IEasingDoubleKeyFrameStatics = interface(IInspectable)
  ['{C8D3D845-DBAE-4E5B-8B84-D9537398E5B1}']
    function get_EasingFunctionProperty: IDependencyProperty; safecall;
    property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingFunctionBaseFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingFunctionBase)]
  Animation_IEasingFunctionBaseFactory = interface(IInspectable)
  ['{1830FE6A-F01B-43E0-B61F-B452A1C66FD2}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingFunctionBaseStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingFunctionBase)]
  Animation_IEasingFunctionBaseStatics = interface(IInspectable)
  ['{2A5031AA-2C50-4A1D-BB04-D75E07B71548}']
    function get_EasingModeProperty: IDependencyProperty; safecall;
    property EasingModeProperty: IDependencyProperty read get_EasingModeProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingPointKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingPointKeyFrame)]
  Animation_IEasingPointKeyFrame = interface(IInspectable)
  ['{B3C91380-6868-4225-A70B-3981CC0B2947}']
    function get_EasingFunction: Animation_IEasingFunctionBase; safecall;
    procedure put_EasingFunction(value: Animation_IEasingFunctionBase); safecall;
    property EasingFunction: Animation_IEasingFunctionBase read get_EasingFunction write put_EasingFunction;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEasingPointKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EasingPointKeyFrame)]
  Animation_IEasingPointKeyFrameStatics = interface(IInspectable)
  ['{E22DBFC4-080C-402C-A6B5-F48D0A98116B}']
    function get_EasingFunctionProperty: IDependencyProperty; safecall;
    property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEdgeUIThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EdgeUIThemeTransition)]
  Animation_IEdgeUIThemeTransition = interface(IInspectable)
  ['{5C86C19B-49D7-19EC-CF19-83A73C6DE75E}']
    function get_Edge: Primitives_EdgeTransitionLocation; safecall;
    procedure put_Edge(value: Primitives_EdgeTransitionLocation); safecall;
    property Edge: Primitives_EdgeTransitionLocation read get_Edge write put_Edge;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEdgeUIThemeTransitionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EdgeUIThemeTransition)]
  Animation_IEdgeUIThemeTransitionStatics = interface(IInspectable)
  ['{16A2B13B-4705-302B-27C6-2AAC92F645AC}']
    function get_EdgeProperty: IDependencyProperty; safecall;
    property EdgeProperty: IDependencyProperty read get_EdgeProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IElasticEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ElasticEase)]
  Animation_IElasticEase = interface(IInspectable)
  ['{EF5BA58C-B0B6-4A6C-9CA8-FB4233F12459}']
    function get_Oscillations: Integer; safecall;
    procedure put_Oscillations(value: Integer); safecall;
    function get_Springiness: Double; safecall;
    procedure put_Springiness(value: Double); safecall;
    property Oscillations: Integer read get_Oscillations write put_Oscillations;
    property Springiness: Double read get_Springiness write put_Springiness;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IElasticEaseStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ElasticEase)]
  Animation_IElasticEaseStatics = interface(IInspectable)
  ['{A9F566EC-FE9C-4B2B-8E52-BB785D562185}']
    function get_OscillationsProperty: IDependencyProperty; safecall;
    function get_SpringinessProperty: IDependencyProperty; safecall;
    property OscillationsProperty: IDependencyProperty read get_OscillationsProperty;
    property SpringinessProperty: IDependencyProperty read get_SpringinessProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEntranceNavigationTransitionInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EntranceNavigationTransitionInfo)]
  Animation_IEntranceNavigationTransitionInfo = interface(IInspectable)
  ['{720A256B-1C8A-41EE-82EC-8A87C0CF47DA}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEntranceNavigationTransitionInfoStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EntranceNavigationTransitionInfo)]
  Animation_IEntranceNavigationTransitionInfoStatics = interface(IInspectable)
  ['{F948C27A-40C9-469F-8F33-BF45C8811F21}']
    function get_IsTargetElementProperty: IDependencyProperty; safecall;
    function GetIsTargetElement(element: IUIElement): Boolean; safecall;
    procedure SetIsTargetElement(element: IUIElement; value: Boolean); safecall;
    property IsTargetElementProperty: IDependencyProperty read get_IsTargetElementProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEntranceThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EntranceThemeTransition)]
  Animation_IEntranceThemeTransition = interface(IInspectable)
  ['{07698C09-A8E3-419A-A01D-7410A0AE8EC8}']
    function get_FromHorizontalOffset: Double; safecall;
    procedure put_FromHorizontalOffset(value: Double); safecall;
    function get_FromVerticalOffset: Double; safecall;
    procedure put_FromVerticalOffset(value: Double); safecall;
    function get_IsStaggeringEnabled: Boolean; safecall;
    procedure put_IsStaggeringEnabled(value: Boolean); safecall;
    property FromHorizontalOffset: Double read get_FromHorizontalOffset write put_FromHorizontalOffset;
    property FromVerticalOffset: Double read get_FromVerticalOffset write put_FromVerticalOffset;
    property IsStaggeringEnabled: Boolean read get_IsStaggeringEnabled write put_IsStaggeringEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IEntranceThemeTransitionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_EntranceThemeTransition)]
  Animation_IEntranceThemeTransitionStatics = interface(IInspectable)
  ['{37CC0577-FF98-4AED-B86E-5EC23702F877}']
    function get_FromHorizontalOffsetProperty: IDependencyProperty; safecall;
    function get_FromVerticalOffsetProperty: IDependencyProperty; safecall;
    function get_IsStaggeringEnabledProperty: IDependencyProperty; safecall;
    property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    property IsStaggeringEnabledProperty: IDependencyProperty read get_IsStaggeringEnabledProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IExponentialEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ExponentialEase)]
  Animation_IExponentialEase = interface(IInspectable)
  ['{7CB9E41D-F0BB-4BCA-9DA5-9BA3A11734C4}']
    function get_Exponent: Double; safecall;
    procedure put_Exponent(value: Double); safecall;
    property Exponent: Double read get_Exponent write put_Exponent;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IExponentialEaseStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ExponentialEase)]
  Animation_IExponentialEaseStatics = interface(IInspectable)
  ['{F37EE7E3-A761-4352-9AD6-70794567581A}']
    function get_ExponentProperty: IDependencyProperty; safecall;
    property ExponentProperty: IDependencyProperty read get_ExponentProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IFadeInThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_FadeInThemeAnimation)]
  Animation_IFadeInThemeAnimation = interface(IInspectable)
  ['{6D4BC8F5-A918-4477-8078-554C68812AB8}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IFadeInThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_FadeInThemeAnimation)]
  Animation_IFadeInThemeAnimationStatics = interface(IInspectable)
  ['{7F0117E1-BEA9-4923-B23A-0DDF4D7B8737}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IFadeOutThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_FadeOutThemeAnimation)]
  Animation_IFadeOutThemeAnimation = interface(IInspectable)
  ['{89276BA9-FFD4-45B6-9B9A-CED48951E712}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IFadeOutThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_FadeOutThemeAnimation)]
  Animation_IFadeOutThemeAnimationStatics = interface(IInspectable)
  ['{FE17A81A-4168-4F68-A28C-E5DD98CF680F}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfiguration
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_GravityConnectedAnimationConfiguration)]
  Animation_IGravityConnectedAnimationConfiguration = interface(IInspectable)
  ['{C751A4B7-0459-5142-B891-AEAAC1D41822}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfiguration2
  Animation_IGravityConnectedAnimationConfiguration2 = interface(IInspectable)
  ['{62333ADD-AED4-5FED-95FF-D128ACCE8BE4}']
    function get_IsShadowEnabled: Boolean; safecall;
    procedure put_IsShadowEnabled(value: Boolean); safecall;
    property IsShadowEnabled: Boolean read get_IsShadowEnabled write put_IsShadowEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfigurationFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_GravityConnectedAnimationConfiguration)]
  Animation_IGravityConnectedAnimationConfigurationFactory = interface(IInspectable)
  ['{E822C41F-3656-5090-92F5-C217EAACB682}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IGravityConnectedAnimationConfiguration; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IKeySpline
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_KeySpline)]
  Animation_IKeySpline = interface(IInspectable)
  ['{77A163BB-D5CA-4A32-BA0B-7DFF988E58A0}']
    function get_ControlPoint1: TPointF; safecall;
    procedure put_ControlPoint1(value: TPointF); safecall;
    function get_ControlPoint2: TPointF; safecall;
    procedure put_ControlPoint2(value: TPointF); safecall;
    property ControlPoint1: TPointF read get_ControlPoint1 write put_ControlPoint1;
    property ControlPoint2: TPointF read get_ControlPoint2 write put_ControlPoint2;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IKeyTimeHelper
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_KeyTimeHelper)]
  Animation_IKeyTimeHelper = interface(IInspectable)
  ['{3643E480-4823-466A-ABE5-5E79C8ED77ED}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IKeyTimeHelperStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_KeyTimeHelper)]
  Animation_IKeyTimeHelperStatics = interface(IInspectable)
  ['{7FA2612C-22A9-45E9-9AF7-C7416EFFF7A5}']
    function FromTimeSpan(timeSpan: TimeSpan): Animation_KeyTime; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ILinearColorKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_LinearColorKeyFrame)]
  Animation_ILinearColorKeyFrame = interface(IInspectable)
  ['{66FDB6EF-AC81-4611-B1D2-61F545983F03}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ILinearDoubleKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_LinearDoubleKeyFrame)]
  Animation_ILinearDoubleKeyFrame = interface(IInspectable)
  ['{8EFDF265-9A7B-431D-8F0C-14C56B5EA4D9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ILinearPointKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_LinearPointKeyFrame)]
  Animation_ILinearPointKeyFrame = interface(IInspectable)
  ['{E7C9B8EF-AF24-49EE-84F1-A86600A4E319}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.INavigationThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_NavigationThemeTransition)]
  Animation_INavigationThemeTransition = interface(IInspectable)
  ['{8833848C-4EB7-41F2-8799-9EEF0A213B73}']
    function get_DefaultNavigationTransitionInfo: Animation_INavigationTransitionInfo; safecall;
    procedure put_DefaultNavigationTransitionInfo(value: Animation_INavigationTransitionInfo); safecall;
    property DefaultNavigationTransitionInfo: Animation_INavigationTransitionInfo read get_DefaultNavigationTransitionInfo write put_DefaultNavigationTransitionInfo;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.INavigationThemeTransitionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_NavigationThemeTransition)]
  Animation_INavigationThemeTransitionStatics = interface(IInspectable)
  ['{EA2F06E0-5E60-4F8E-BCAF-431487A294AB}']
    function get_DefaultNavigationTransitionInfoProperty: IDependencyProperty; safecall;
    property DefaultNavigationTransitionInfoProperty: IDependencyProperty read get_DefaultNavigationTransitionInfoProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.INavigationTransitionInfoFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_NavigationTransitionInfo)]
  Animation_INavigationTransitionInfoFactory = interface(IInspectable)
  ['{EDF4F8D5-AF63-4FAB-9D4A-87927F82DD6B}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_INavigationTransitionInfo; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.INavigationTransitionInfoOverrides
  Animation_INavigationTransitionInfoOverrides = interface(IInspectable)
  ['{D9517E6A-A9D0-4BF7-9DB0-4633A69DAFF2}']
    function GetNavigationStateCore: HSTRING; safecall;
    procedure SetNavigationStateCore(navigationState: HSTRING); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IObjectKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ObjectKeyFrame)]
  Animation_IObjectKeyFrame = interface(IInspectable)
  ['{9852A851-8593-48EE-A6A4-D5D4720F029A}']
    function get_Value: IInspectable; safecall;
    procedure put_Value(value: IInspectable); safecall;
    function get_KeyTime: Animation_KeyTime; safecall;
    procedure put_KeyTime(value: Animation_KeyTime); safecall;
    property KeyTime: Animation_KeyTime read get_KeyTime write put_KeyTime;
    property Value: IInspectable read get_Value write put_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IIterator_1__Animation_IObjectKeyFrame_Base = interface(IInspectable)
  ['{69C69834-8951-54A7-A2CA-922BACF9FC70}']
    function get_Current: Animation_IObjectKeyFrame; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAnimation_IObjectKeyFrame): Cardinal; safecall;
    property Current: Animation_IObjectKeyFrame read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IIterator_1__Animation_IObjectKeyFrame = interface(IIterator_1__Animation_IObjectKeyFrame_Base)
  ['{5EC17499-36FF-539C-9055-CD4C9ED7E620}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IIterable_1__Animation_IObjectKeyFrame_Base = interface(IInspectable)
  ['{9084A8E1-8F4D-5DE3-B1C6-E5F05724CAED}']
    function First: IIterator_1__Animation_IObjectKeyFrame; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IIterable_1__Animation_IObjectKeyFrame = interface(IIterable_1__Animation_IObjectKeyFrame_Base)
  ['{01EFF557-C828-558B-8722-084AE4D8FD76}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IVectorView_1__Animation_IObjectKeyFrame = interface(IInspectable)
  ['{07F37759-481C-58FB-9181-D4CFD27B68B6}']
    function GetAt(index: Cardinal): Animation_IObjectKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Animation_IObjectKeyFrame; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IObjectKeyFrame): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  IVector_1__Animation_IObjectKeyFrame_Base = interface(IInspectable)
  ['{5F733D3F-72A6-5303-9727-2B03EAE7DD4C}']
    function GetAt(index: Cardinal): Animation_IObjectKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Animation_IObjectKeyFrame; safecall;
    function IndexOf(value: Animation_IObjectKeyFrame; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Animation_IObjectKeyFrame); safecall;
    procedure InsertAt(index: Cardinal; value: Animation_IObjectKeyFrame); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Animation_IObjectKeyFrame); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IObjectKeyFrame): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PAnimation_IObjectKeyFrame); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ObjectKeyFrameCollection)]
  IVector_1__Animation_IObjectKeyFrame = interface(IVector_1__Animation_IObjectKeyFrame_Base)
  ['{1D86A4FC-E993-5E3F-A2B2-9D5CC2DA098D}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IObjectAnimationUsingKeyFrames
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ObjectAnimationUsingKeyFrames)]
  Animation_IObjectAnimationUsingKeyFrames = interface(IInspectable)
  ['{334A2D92-B74A-4C64-B9A6-58BCFA314F22}']
    function get_KeyFrames: IVector_1__Animation_IObjectKeyFrame; safecall;
    function get_EnableDependentAnimation: Boolean; safecall;
    procedure put_EnableDependentAnimation(value: Boolean); safecall;
    property EnableDependentAnimation: Boolean read get_EnableDependentAnimation write put_EnableDependentAnimation;
    property KeyFrames: IVector_1__Animation_IObjectKeyFrame read get_KeyFrames;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IObjectAnimationUsingKeyFramesStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ObjectAnimationUsingKeyFrames)]
  Animation_IObjectAnimationUsingKeyFramesStatics = interface(IInspectable)
  ['{EB736182-6AF1-49A3-97B6-783ED97400FE}']
    function get_EnableDependentAnimationProperty: IDependencyProperty; safecall;
    property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IObjectKeyFrameFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ObjectKeyFrame)]
  Animation_IObjectKeyFrameFactory = interface(IInspectable)
  ['{1626143E-3E6D-44D8-9B9A-04AEA70F8492}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IObjectKeyFrame; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IObjectKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ObjectKeyFrame)]
  Animation_IObjectKeyFrameStatics = interface(IInspectable)
  ['{2CD6AB00-5319-4286-8EED-4E755EA0CF9C}']
    function get_ValueProperty: IDependencyProperty; safecall;
    function get_KeyTimeProperty: IDependencyProperty; safecall;
    property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    property ValueProperty: IDependencyProperty read get_ValueProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPaneThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PaneThemeTransition)]
  Animation_IPaneThemeTransition = interface(IInspectable)
  ['{4708EB8E-4BFC-EE46-D4F9-708DEF3FBB2B}']
    function get_Edge: Primitives_EdgeTransitionLocation; safecall;
    procedure put_Edge(value: Primitives_EdgeTransitionLocation); safecall;
    property Edge: Primitives_EdgeTransitionLocation read get_Edge write put_Edge;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPaneThemeTransitionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PaneThemeTransition)]
  Animation_IPaneThemeTransitionStatics = interface(IInspectable)
  ['{316B382F-4BE4-1797-B45C-CD900BBE0CAA}']
    function get_EdgeProperty: IDependencyProperty; safecall;
    property EdgeProperty: IDependencyProperty read get_EdgeProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointAnimation)]
  Animation_IPointAnimation = interface(IInspectable)
  ['{30F04312-7726-4F88-B8E2-2FA54518963B}']
    function get_From: IReference_1__Point; safecall;
    procedure put_From(value: IReference_1__Point); safecall;
    function get_To: IReference_1__Point; safecall;
    procedure put_To(value: IReference_1__Point); safecall;
    function get_By: IReference_1__Point; safecall;
    procedure put_By(value: IReference_1__Point); safecall;
    function get_EasingFunction: Animation_IEasingFunctionBase; safecall;
    procedure put_EasingFunction(value: Animation_IEasingFunctionBase); safecall;
    function get_EnableDependentAnimation: Boolean; safecall;
    procedure put_EnableDependentAnimation(value: Boolean); safecall;
    property By: IReference_1__Point read get_By write put_By;
    property EasingFunction: Animation_IEasingFunctionBase read get_EasingFunction write put_EasingFunction;
    property EnableDependentAnimation: Boolean read get_EnableDependentAnimation write put_EnableDependentAnimation;
    property From: IReference_1__Point read get_From write put_From;
    property &To: IReference_1__Point read get_To write put_To;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointAnimation)]
  Animation_IPointAnimationStatics = interface(IInspectable)
  ['{2F99B356-E737-408B-A0FD-327826D32255}']
    function get_FromProperty: IDependencyProperty; safecall;
    function get_ToProperty: IDependencyProperty; safecall;
    function get_ByProperty: IDependencyProperty; safecall;
    function get_EasingFunctionProperty: IDependencyProperty; safecall;
    function get_EnableDependentAnimationProperty: IDependencyProperty; safecall;
    property ByProperty: IDependencyProperty read get_ByProperty;
    property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
    property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
    property FromProperty: IDependencyProperty read get_FromProperty;
    property ToProperty: IDependencyProperty read get_ToProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointKeyFrame)]
  Animation_IPointKeyFrame = interface(IInspectable)
  ['{FCC88D01-7F82-4DAE-8026-7B7E086878B3}']
    function get_Value: TPointF; safecall;
    procedure put_Value(value: TPointF); safecall;
    function get_KeyTime: Animation_KeyTime; safecall;
    procedure put_KeyTime(value: Animation_KeyTime); safecall;
    property KeyTime: Animation_KeyTime read get_KeyTime write put_KeyTime;
    property Value: TPointF read get_Value write put_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IIterator_1__Animation_IPointKeyFrame_Base = interface(IInspectable)
  ['{E55A65AD-E742-5F78-876B-64D1681FC9E2}']
    function get_Current: Animation_IPointKeyFrame; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAnimation_IPointKeyFrame): Cardinal; safecall;
    property Current: Animation_IPointKeyFrame read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IIterator_1__Animation_IPointKeyFrame = interface(IIterator_1__Animation_IPointKeyFrame_Base)
  ['{E52249E5-E527-55EB-A2EC-28DF30496003}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IIterable_1__Animation_IPointKeyFrame_Base = interface(IInspectable)
  ['{B8DE4DC9-8C69-55F3-AF58-040F1319649C}']
    function First: IIterator_1__Animation_IPointKeyFrame; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IIterable_1__Animation_IPointKeyFrame = interface(IIterable_1__Animation_IPointKeyFrame_Base)
  ['{C21D2C13-72EE-56CC-9054-870C4608FBCA}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IVectorView_1__Animation_IPointKeyFrame = interface(IInspectable)
  ['{8A38494B-CA26-54B7-B7AC-C5FE60B85729}']
    function GetAt(index: Cardinal): Animation_IPointKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Animation_IPointKeyFrame; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IPointKeyFrame): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  IVector_1__Animation_IPointKeyFrame_Base = interface(IInspectable)
  ['{B17E26DE-9CD7-5456-BEE0-49882A87F945}']
    function GetAt(index: Cardinal): Animation_IPointKeyFrame; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Animation_IPointKeyFrame; safecall;
    function IndexOf(value: Animation_IPointKeyFrame; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Animation_IPointKeyFrame); safecall;
    procedure InsertAt(index: Cardinal; value: Animation_IPointKeyFrame); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Animation_IPointKeyFrame); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimation_IPointKeyFrame): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PAnimation_IPointKeyFrame); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointKeyFrameCollection)]
  IVector_1__Animation_IPointKeyFrame = interface(IVector_1__Animation_IPointKeyFrame_Base)
  ['{97B26EC0-72C6-5577-A99E-715507A5CE94}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointAnimationUsingKeyFrames
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointAnimationUsingKeyFrames)]
  Animation_IPointAnimationUsingKeyFrames = interface(IInspectable)
  ['{9B944F72-446A-41D0-A129-41A620F4595D}']
    function get_KeyFrames: IVector_1__Animation_IPointKeyFrame; safecall;
    function get_EnableDependentAnimation: Boolean; safecall;
    procedure put_EnableDependentAnimation(value: Boolean); safecall;
    property EnableDependentAnimation: Boolean read get_EnableDependentAnimation write put_EnableDependentAnimation;
    property KeyFrames: IVector_1__Animation_IPointKeyFrame read get_KeyFrames;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointAnimationUsingKeyFramesStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointAnimationUsingKeyFrames)]
  Animation_IPointAnimationUsingKeyFramesStatics = interface(IInspectable)
  ['{5F454C87-2390-46EA-BAA7-762F4BC30D04}']
    function get_EnableDependentAnimationProperty: IDependencyProperty; safecall;
    property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointKeyFrameFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointKeyFrame)]
  Animation_IPointKeyFrameFactory = interface(IInspectable)
  ['{CB214BDF-426A-4392-8355-C2AE52852623}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IPointKeyFrame; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointKeyFrame)]
  Animation_IPointKeyFrameStatics = interface(IInspectable)
  ['{95CF1B27-7965-4BEC-B9FB-FBE94B65518E}']
    function get_ValueProperty: IDependencyProperty; safecall;
    function get_KeyTimeProperty: IDependencyProperty; safecall;
    property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    property ValueProperty: IDependencyProperty read get_ValueProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointerDownThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointerDownThemeAnimation)]
  Animation_IPointerDownThemeAnimation = interface(IInspectable)
  ['{B58E714E-C49D-4788-A233-0AE85D99DD5A}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointerDownThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointerDownThemeAnimation)]
  Animation_IPointerDownThemeAnimationStatics = interface(IInspectable)
  ['{63A7CB7B-6D46-4494-B94A-E72F3B492A61}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointerUpThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointerUpThemeAnimation)]
  Animation_IPointerUpThemeAnimation = interface(IInspectable)
  ['{E9E9D07D-6340-4828-AD12-690694B9910B}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPointerUpThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PointerUpThemeAnimation)]
  Animation_IPointerUpThemeAnimationStatics = interface(IInspectable)
  ['{7C618F9C-7992-4139-8BFC-0883B9727A7E}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPopInThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PopInThemeAnimation)]
  Animation_IPopInThemeAnimation = interface(IInspectable)
  ['{196938C1-1C07-4C28-8847-F9F055B32855}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    function get_FromHorizontalOffset: Double; safecall;
    procedure put_FromHorizontalOffset(value: Double); safecall;
    function get_FromVerticalOffset: Double; safecall;
    procedure put_FromVerticalOffset(value: Double); safecall;
    property FromHorizontalOffset: Double read get_FromHorizontalOffset write put_FromHorizontalOffset;
    property FromVerticalOffset: Double read get_FromVerticalOffset write put_FromVerticalOffset;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPopInThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PopInThemeAnimation)]
  Animation_IPopInThemeAnimationStatics = interface(IInspectable)
  ['{EFAA99D3-218A-4701-977F-F1BFAE8BA649}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    function get_FromHorizontalOffsetProperty: IDependencyProperty; safecall;
    function get_FromVerticalOffsetProperty: IDependencyProperty; safecall;
    property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPopOutThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PopOutThemeAnimation)]
  Animation_IPopOutThemeAnimation = interface(IInspectable)
  ['{4786AB49-0E48-4E81-A2E5-CC5AA19E48D3}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPopOutThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PopOutThemeAnimation)]
  Animation_IPopOutThemeAnimationStatics = interface(IInspectable)
  ['{1D492C09-03C1-4490-99DC-909FEAB357FB}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPopupThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PopupThemeTransition)]
  Animation_IPopupThemeTransition = interface(IInspectable)
  ['{47843552-4283-545E-C791-268DCA22CE4B}']
    function get_FromHorizontalOffset: Double; safecall;
    procedure put_FromHorizontalOffset(value: Double); safecall;
    function get_FromVerticalOffset: Double; safecall;
    procedure put_FromVerticalOffset(value: Double); safecall;
    property FromHorizontalOffset: Double read get_FromHorizontalOffset write put_FromHorizontalOffset;
    property FromVerticalOffset: Double read get_FromVerticalOffset write put_FromVerticalOffset;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPopupThemeTransitionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PopupThemeTransition)]
  Animation_IPopupThemeTransitionStatics = interface(IInspectable)
  ['{E5A1640E-490D-1505-9F6B-8FAFC044DEC5}']
    function get_FromHorizontalOffsetProperty: IDependencyProperty; safecall;
    function get_FromVerticalOffsetProperty: IDependencyProperty; safecall;
    property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPowerEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PowerEase)]
  Animation_IPowerEase = interface(IInspectable)
  ['{69C80579-EEDF-405B-8680-D9606880C937}']
    function get_Power: Double; safecall;
    procedure put_Power(value: Double); safecall;
    property Power: Double read get_Power write put_Power;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IPowerEaseStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_PowerEase)]
  Animation_IPowerEaseStatics = interface(IInspectable)
  ['{A5955103-91A2-460C-9C41-D28F6A939BDA}']
    function get_PowerProperty: IDependencyProperty; safecall;
    property PowerProperty: IDependencyProperty read get_PowerProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IQuadraticEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_QuadraticEase)]
  Animation_IQuadraticEase = interface(IInspectable)
  ['{E1510E91-EF6D-44F0-803D-68D16DE0DDFC}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IQuarticEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_QuarticEase)]
  Animation_IQuarticEase = interface(IInspectable)
  ['{E8698814-FE42-4A05-B5B8-081F41157815}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IQuinticEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_QuinticEase)]
  Animation_IQuinticEase = interface(IInspectable)
  ['{92EE793B-3C49-4108-AA11-AB786603DA21}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IReorderThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_ReorderThemeTransition)]
  Animation_IReorderThemeTransition = interface(IInspectable)
  ['{F2065C6C-D052-4AD1-8362-B71B36DF7497}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IRepeatBehaviorHelper
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_RepeatBehaviorHelper)]
  Animation_IRepeatBehaviorHelper = interface(IInspectable)
  ['{6863AB72-4997-47F9-87AD-37EFB75993EA}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IRepeatBehaviorHelperStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_RepeatBehaviorHelper)]
  Animation_IRepeatBehaviorHelperStatics = interface(IInspectable)
  ['{7A795033-79F3-4DD9-B267-9CF50FB51F84}']
    function get_Forever: Animation_RepeatBehavior; safecall;
    function FromCount(count: Double): Animation_RepeatBehavior; safecall;
    function FromDuration(duration: TimeSpan): Animation_RepeatBehavior; safecall;
    function GetHasCount(target: Animation_RepeatBehavior): Boolean; safecall;
    function GetHasDuration(target: Animation_RepeatBehavior): Boolean; safecall;
    function Equals(target: Animation_RepeatBehavior; value: Animation_RepeatBehavior): Boolean; safecall;
    property Forever: Animation_RepeatBehavior read get_Forever;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IRepositionThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_RepositionThemeAnimation)]
  Animation_IRepositionThemeAnimation = interface(IInspectable)
  ['{ECDA24E8-8945-4949-A1BF-62109965A7E9}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    function get_FromHorizontalOffset: Double; safecall;
    procedure put_FromHorizontalOffset(value: Double); safecall;
    function get_FromVerticalOffset: Double; safecall;
    procedure put_FromVerticalOffset(value: Double); safecall;
    property FromHorizontalOffset: Double read get_FromHorizontalOffset write put_FromHorizontalOffset;
    property FromVerticalOffset: Double read get_FromVerticalOffset write put_FromVerticalOffset;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IRepositionThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_RepositionThemeAnimation)]
  Animation_IRepositionThemeAnimationStatics = interface(IInspectable)
  ['{4D92B1B1-860B-4BF9-A59D-1EB1CCBE8FE0}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    function get_FromHorizontalOffsetProperty: IDependencyProperty; safecall;
    function get_FromVerticalOffsetProperty: IDependencyProperty; safecall;
    property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IRepositionThemeTransition
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_RepositionThemeTransition)]
  Animation_IRepositionThemeTransition = interface(IInspectable)
  ['{88329B82-98F3-455A-AC53-2E7083B6E22C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IRepositionThemeTransition2
  Animation_IRepositionThemeTransition2 = interface(IInspectable)
  ['{CEBFE864-DBEA-4404-8E6E-DE55ADA75239}']
    function get_IsStaggeringEnabled: Boolean; safecall;
    procedure put_IsStaggeringEnabled(value: Boolean); safecall;
    property IsStaggeringEnabled: Boolean read get_IsStaggeringEnabled write put_IsStaggeringEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IRepositionThemeTransitionStatics2
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_RepositionThemeTransition)]
  Animation_IRepositionThemeTransitionStatics2 = interface(IInspectable)
  ['{9240E930-0A19-468B-8C2A-68FAB4500027}']
    function get_IsStaggeringEnabledProperty: IDependencyProperty; safecall;
    property IsStaggeringEnabledProperty: IDependencyProperty read get_IsStaggeringEnabledProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISineEase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SineEase)]
  Animation_ISineEase = interface(IInspectable)
  ['{A9382962-230B-49DA-9E0D-664987892343}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SlideNavigationTransitionInfo)]
  Animation_ISlideNavigationTransitionInfo = interface(IInspectable)
  ['{D6AC9D77-2E03-405F-80ED-E62BEEF3668F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfo2
  Animation_ISlideNavigationTransitionInfo2 = interface(IInspectable)
  ['{90E2D9C0-5C81-5001-8013-4FBFEA4BF139}']
    function get_Effect: Animation_SlideNavigationTransitionEffect; safecall;
    procedure put_Effect(value: Animation_SlideNavigationTransitionEffect); safecall;
    property Effect: Animation_SlideNavigationTransitionEffect read get_Effect write put_Effect;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfoStatics2
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SlideNavigationTransitionInfo)]
  Animation_ISlideNavigationTransitionInfoStatics2 = interface(IInspectable)
  ['{8A861BAA-981A-5ACE-9F85-CB7FDE648A67}']
    function get_EffectProperty: IDependencyProperty; safecall;
    property EffectProperty: IDependencyProperty read get_EffectProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplineColorKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplineColorKeyFrame)]
  Animation_ISplineColorKeyFrame = interface(IInspectable)
  ['{1A4A5941-1FE0-473A-8EFE-4316D8C86229}']
    function get_KeySpline: Animation_IKeySpline; safecall;
    procedure put_KeySpline(value: Animation_IKeySpline); safecall;
    property KeySpline: Animation_IKeySpline read get_KeySpline write put_KeySpline;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplineColorKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplineColorKeyFrame)]
  Animation_ISplineColorKeyFrameStatics = interface(IInspectable)
  ['{61D1D997-8589-4F2F-8FBB-7D03EDC98DD3}']
    function get_KeySplineProperty: IDependencyProperty; safecall;
    property KeySplineProperty: IDependencyProperty read get_KeySplineProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplineDoubleKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplineDoubleKeyFrame)]
  Animation_ISplineDoubleKeyFrame = interface(IInspectable)
  ['{00D72D38-6B2B-4843-838E-C8B115EEC801}']
    function get_KeySpline: Animation_IKeySpline; safecall;
    procedure put_KeySpline(value: Animation_IKeySpline); safecall;
    property KeySpline: Animation_IKeySpline read get_KeySpline write put_KeySpline;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplineDoubleKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplineDoubleKeyFrame)]
  Animation_ISplineDoubleKeyFrameStatics = interface(IInspectable)
  ['{060A8FFC-975F-4E4E-9EC7-13C5AEE02062}']
    function get_KeySplineProperty: IDependencyProperty; safecall;
    property KeySplineProperty: IDependencyProperty read get_KeySplineProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplinePointKeyFrame
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplinePointKeyFrame)]
  Animation_ISplinePointKeyFrame = interface(IInspectable)
  ['{0F19F306-7036-494F-BC3C-780DF0CC524A}']
    function get_KeySpline: Animation_IKeySpline; safecall;
    procedure put_KeySpline(value: Animation_IKeySpline); safecall;
    property KeySpline: Animation_IKeySpline read get_KeySpline write put_KeySpline;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplinePointKeyFrameStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplinePointKeyFrame)]
  Animation_ISplinePointKeyFrameStatics = interface(IInspectable)
  ['{E97A32C2-0A7A-4766-95CB-0D692611CB4C}']
    function get_KeySplineProperty: IDependencyProperty; safecall;
    property KeySplineProperty: IDependencyProperty read get_KeySplineProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplitCloseThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplitCloseThemeAnimation)]
  Animation_ISplitCloseThemeAnimation = interface(IInspectable)
  ['{4F799518-FF39-4E90-BB74-2ABD56027402}']
    function get_OpenedTargetName: HSTRING; safecall;
    procedure put_OpenedTargetName(value: HSTRING); safecall;
    function get_OpenedTarget: IDependencyObject; safecall;
    procedure put_OpenedTarget(value: IDependencyObject); safecall;
    function get_ClosedTargetName: HSTRING; safecall;
    procedure put_ClosedTargetName(value: HSTRING); safecall;
    function get_ClosedTarget: IDependencyObject; safecall;
    procedure put_ClosedTarget(value: IDependencyObject); safecall;
    function get_ContentTargetName: HSTRING; safecall;
    procedure put_ContentTargetName(value: HSTRING); safecall;
    function get_ContentTarget: IDependencyObject; safecall;
    procedure put_ContentTarget(value: IDependencyObject); safecall;
    function get_OpenedLength: Double; safecall;
    procedure put_OpenedLength(value: Double); safecall;
    function get_ClosedLength: Double; safecall;
    procedure put_ClosedLength(value: Double); safecall;
    function get_OffsetFromCenter: Double; safecall;
    procedure put_OffsetFromCenter(value: Double); safecall;
    function get_ContentTranslationDirection: Primitives_AnimationDirection; safecall;
    procedure put_ContentTranslationDirection(value: Primitives_AnimationDirection); safecall;
    function get_ContentTranslationOffset: Double; safecall;
    procedure put_ContentTranslationOffset(value: Double); safecall;
    property ClosedLength: Double read get_ClosedLength write put_ClosedLength;
    property ClosedTarget: IDependencyObject read get_ClosedTarget write put_ClosedTarget;
    property ClosedTargetName: HSTRING read get_ClosedTargetName write put_ClosedTargetName;
    property ContentTarget: IDependencyObject read get_ContentTarget write put_ContentTarget;
    property ContentTargetName: HSTRING read get_ContentTargetName write put_ContentTargetName;
    property ContentTranslationDirection: Primitives_AnimationDirection read get_ContentTranslationDirection write put_ContentTranslationDirection;
    property ContentTranslationOffset: Double read get_ContentTranslationOffset write put_ContentTranslationOffset;
    property OffsetFromCenter: Double read get_OffsetFromCenter write put_OffsetFromCenter;
    property OpenedLength: Double read get_OpenedLength write put_OpenedLength;
    property OpenedTarget: IDependencyObject read get_OpenedTarget write put_OpenedTarget;
    property OpenedTargetName: HSTRING read get_OpenedTargetName write put_OpenedTargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplitCloseThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplitCloseThemeAnimation)]
  Animation_ISplitCloseThemeAnimationStatics = interface(IInspectable)
  ['{7AA94DE9-CC9B-4E90-A11A-0050A2216A9E}']
    function get_OpenedTargetNameProperty: IDependencyProperty; safecall;
    function get_OpenedTargetProperty: IDependencyProperty; safecall;
    function get_ClosedTargetNameProperty: IDependencyProperty; safecall;
    function get_ClosedTargetProperty: IDependencyProperty; safecall;
    function get_ContentTargetNameProperty: IDependencyProperty; safecall;
    function get_ContentTargetProperty: IDependencyProperty; safecall;
    function get_OpenedLengthProperty: IDependencyProperty; safecall;
    function get_ClosedLengthProperty: IDependencyProperty; safecall;
    function get_OffsetFromCenterProperty: IDependencyProperty; safecall;
    function get_ContentTranslationDirectionProperty: IDependencyProperty; safecall;
    function get_ContentTranslationOffsetProperty: IDependencyProperty; safecall;
    property ClosedLengthProperty: IDependencyProperty read get_ClosedLengthProperty;
    property ClosedTargetNameProperty: IDependencyProperty read get_ClosedTargetNameProperty;
    property ClosedTargetProperty: IDependencyProperty read get_ClosedTargetProperty;
    property ContentTargetNameProperty: IDependencyProperty read get_ContentTargetNameProperty;
    property ContentTargetProperty: IDependencyProperty read get_ContentTargetProperty;
    property ContentTranslationDirectionProperty: IDependencyProperty read get_ContentTranslationDirectionProperty;
    property ContentTranslationOffsetProperty: IDependencyProperty read get_ContentTranslationOffsetProperty;
    property OffsetFromCenterProperty: IDependencyProperty read get_OffsetFromCenterProperty;
    property OpenedLengthProperty: IDependencyProperty read get_OpenedLengthProperty;
    property OpenedTargetNameProperty: IDependencyProperty read get_OpenedTargetNameProperty;
    property OpenedTargetProperty: IDependencyProperty read get_OpenedTargetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplitOpenThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplitOpenThemeAnimation)]
  Animation_ISplitOpenThemeAnimation = interface(IInspectable)
  ['{785FD7AA-5456-4639-8FD2-26BAE6A5FFE4}']
    function get_OpenedTargetName: HSTRING; safecall;
    procedure put_OpenedTargetName(value: HSTRING); safecall;
    function get_OpenedTarget: IDependencyObject; safecall;
    procedure put_OpenedTarget(value: IDependencyObject); safecall;
    function get_ClosedTargetName: HSTRING; safecall;
    procedure put_ClosedTargetName(value: HSTRING); safecall;
    function get_ClosedTarget: IDependencyObject; safecall;
    procedure put_ClosedTarget(value: IDependencyObject); safecall;
    function get_ContentTargetName: HSTRING; safecall;
    procedure put_ContentTargetName(value: HSTRING); safecall;
    function get_ContentTarget: IDependencyObject; safecall;
    procedure put_ContentTarget(value: IDependencyObject); safecall;
    function get_OpenedLength: Double; safecall;
    procedure put_OpenedLength(value: Double); safecall;
    function get_ClosedLength: Double; safecall;
    procedure put_ClosedLength(value: Double); safecall;
    function get_OffsetFromCenter: Double; safecall;
    procedure put_OffsetFromCenter(value: Double); safecall;
    function get_ContentTranslationDirection: Primitives_AnimationDirection; safecall;
    procedure put_ContentTranslationDirection(value: Primitives_AnimationDirection); safecall;
    function get_ContentTranslationOffset: Double; safecall;
    procedure put_ContentTranslationOffset(value: Double); safecall;
    property ClosedLength: Double read get_ClosedLength write put_ClosedLength;
    property ClosedTarget: IDependencyObject read get_ClosedTarget write put_ClosedTarget;
    property ClosedTargetName: HSTRING read get_ClosedTargetName write put_ClosedTargetName;
    property ContentTarget: IDependencyObject read get_ContentTarget write put_ContentTarget;
    property ContentTargetName: HSTRING read get_ContentTargetName write put_ContentTargetName;
    property ContentTranslationDirection: Primitives_AnimationDirection read get_ContentTranslationDirection write put_ContentTranslationDirection;
    property ContentTranslationOffset: Double read get_ContentTranslationOffset write put_ContentTranslationOffset;
    property OffsetFromCenter: Double read get_OffsetFromCenter write put_OffsetFromCenter;
    property OpenedLength: Double read get_OpenedLength write put_OpenedLength;
    property OpenedTarget: IDependencyObject read get_OpenedTarget write put_OpenedTarget;
    property OpenedTargetName: HSTRING read get_OpenedTargetName write put_OpenedTargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISplitOpenThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SplitOpenThemeAnimation)]
  Animation_ISplitOpenThemeAnimationStatics = interface(IInspectable)
  ['{8D4CFA89-3A91-458D-B0FB-4CAD625CBF8D}']
    function get_OpenedTargetNameProperty: IDependencyProperty; safecall;
    function get_OpenedTargetProperty: IDependencyProperty; safecall;
    function get_ClosedTargetNameProperty: IDependencyProperty; safecall;
    function get_ClosedTargetProperty: IDependencyProperty; safecall;
    function get_ContentTargetNameProperty: IDependencyProperty; safecall;
    function get_ContentTargetProperty: IDependencyProperty; safecall;
    function get_OpenedLengthProperty: IDependencyProperty; safecall;
    function get_ClosedLengthProperty: IDependencyProperty; safecall;
    function get_OffsetFromCenterProperty: IDependencyProperty; safecall;
    function get_ContentTranslationDirectionProperty: IDependencyProperty; safecall;
    function get_ContentTranslationOffsetProperty: IDependencyProperty; safecall;
    property ClosedLengthProperty: IDependencyProperty read get_ClosedLengthProperty;
    property ClosedTargetNameProperty: IDependencyProperty read get_ClosedTargetNameProperty;
    property ClosedTargetProperty: IDependencyProperty read get_ClosedTargetProperty;
    property ContentTargetNameProperty: IDependencyProperty read get_ContentTargetNameProperty;
    property ContentTargetProperty: IDependencyProperty read get_ContentTargetProperty;
    property ContentTranslationDirectionProperty: IDependencyProperty read get_ContentTranslationDirectionProperty;
    property ContentTranslationOffsetProperty: IDependencyProperty read get_ContentTranslationOffsetProperty;
    property OffsetFromCenterProperty: IDependencyProperty read get_OffsetFromCenterProperty;
    property OpenedLengthProperty: IDependencyProperty read get_OpenedLengthProperty;
    property OpenedTargetNameProperty: IDependencyProperty read get_OpenedTargetNameProperty;
    property OpenedTargetProperty: IDependencyProperty read get_OpenedTargetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.IStoryboardStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_Storyboard)]
  Animation_IStoryboardStatics = interface(IInspectable)
  ['{D82F07D8-73D5-4379-BD48-7E05184A8BAD}']
    function get_TargetPropertyProperty: IDependencyProperty; safecall;
    function GetTargetProperty(element: Animation_ITimeline): HSTRING; safecall;
    procedure SetTargetProperty(element: Animation_ITimeline; path: HSTRING); safecall;
    function get_TargetNameProperty: IDependencyProperty; safecall;
    function GetTargetName(element: Animation_ITimeline): HSTRING; safecall;
    procedure SetTargetName(element: Animation_ITimeline; name: HSTRING); safecall;
    procedure SetTarget(timeline: Animation_ITimeline; target: IDependencyObject); safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
    property TargetPropertyProperty: IDependencyProperty read get_TargetPropertyProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISuppressNavigationTransitionInfo
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SuppressNavigationTransitionInfo)]
  Animation_ISuppressNavigationTransitionInfo = interface(IInspectable)
  ['{244D7B0C-B1B7-4871-9D3E-D56203A3A5B4}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISwipeBackThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SwipeBackThemeAnimation)]
  Animation_ISwipeBackThemeAnimation = interface(IInspectable)
  ['{A38A4214-0BCA-4D2D-95F7-CEBA57FBAF60}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    function get_FromHorizontalOffset: Double; safecall;
    procedure put_FromHorizontalOffset(value: Double); safecall;
    function get_FromVerticalOffset: Double; safecall;
    procedure put_FromVerticalOffset(value: Double); safecall;
    property FromHorizontalOffset: Double read get_FromHorizontalOffset write put_FromHorizontalOffset;
    property FromVerticalOffset: Double read get_FromVerticalOffset write put_FromVerticalOffset;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISwipeBackThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SwipeBackThemeAnimation)]
  Animation_ISwipeBackThemeAnimationStatics = interface(IInspectable)
  ['{693F31BF-4DA6-468A-8CE0-996C9AAD42E0}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    function get_FromHorizontalOffsetProperty: IDependencyProperty; safecall;
    function get_FromVerticalOffsetProperty: IDependencyProperty; safecall;
    property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISwipeHintThemeAnimation
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SwipeHintThemeAnimation)]
  Animation_ISwipeHintThemeAnimation = interface(IInspectable)
  ['{CDD067C0-580E-4E40-BE98-F202D3D84365}']
    function get_TargetName: HSTRING; safecall;
    procedure put_TargetName(value: HSTRING); safecall;
    function get_ToHorizontalOffset: Double; safecall;
    procedure put_ToHorizontalOffset(value: Double); safecall;
    function get_ToVerticalOffset: Double; safecall;
    procedure put_ToVerticalOffset(value: Double); safecall;
    property TargetName: HSTRING read get_TargetName write put_TargetName;
    property ToHorizontalOffset: Double read get_ToHorizontalOffset write put_ToHorizontalOffset;
    property ToVerticalOffset: Double read get_ToVerticalOffset write put_ToVerticalOffset;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ISwipeHintThemeAnimationStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_SwipeHintThemeAnimation)]
  Animation_ISwipeHintThemeAnimationStatics = interface(IInspectable)
  ['{23D61A57-9115-4D63-B04A-B89F1C744DC0}']
    function get_TargetNameProperty: IDependencyProperty; safecall;
    function get_ToHorizontalOffsetProperty: IDependencyProperty; safecall;
    function get_ToVerticalOffsetProperty: IDependencyProperty; safecall;
    property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
    property ToHorizontalOffsetProperty: IDependencyProperty read get_ToHorizontalOffsetProperty;
    property ToVerticalOffsetProperty: IDependencyProperty read get_ToVerticalOffsetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ITimelineFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_Timeline)]
  Animation_ITimelineFactory = interface(IInspectable)
  ['{1D56BB07-BDA4-478B-8ADA-EB04D580CD5E}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_ITimeline; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ITimelineStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_Timeline)]
  Animation_ITimelineStatics = interface(IInspectable)
  ['{A902ED4E-EF10-4D6F-9A40-93CB8895F4E5}']
    function get_AllowDependentAnimations: Boolean; safecall;
    procedure put_AllowDependentAnimations(value: Boolean); safecall;
    function get_AutoReverseProperty: IDependencyProperty; safecall;
    function get_BeginTimeProperty: IDependencyProperty; safecall;
    function get_DurationProperty: IDependencyProperty; safecall;
    function get_SpeedRatioProperty: IDependencyProperty; safecall;
    function get_FillBehaviorProperty: IDependencyProperty; safecall;
    function get_RepeatBehaviorProperty: IDependencyProperty; safecall;
    property AllowDependentAnimations: Boolean read get_AllowDependentAnimations write put_AllowDependentAnimations;
    property AutoReverseProperty: IDependencyProperty read get_AutoReverseProperty;
    property BeginTimeProperty: IDependencyProperty read get_BeginTimeProperty;
    property DurationProperty: IDependencyProperty read get_DurationProperty;
    property FillBehaviorProperty: IDependencyProperty read get_FillBehaviorProperty;
    property RepeatBehaviorProperty: IDependencyProperty read get_RepeatBehaviorProperty;
    property SpeedRatioProperty: IDependencyProperty read get_SpeedRatioProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Animation.ITransitionFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Animation_Transition)]
  Animation_ITransitionFactory = interface(IInspectable)
  ['{DC9AB2CF-3BC9-44AA-B3FC-883A83233A2C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IAcrylicBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_AcrylicBrush)]
  IAcrylicBrush = interface(IInspectable)
  ['{79BBCF4E-CD66-4F1B-A8B6-CD6D2977C18D}']
    function get_BackgroundSource: AcrylicBackgroundSource; safecall;
    procedure put_BackgroundSource(value: AcrylicBackgroundSource); safecall;
    function get_TintColor: Color; safecall;
    procedure put_TintColor(value: Color); safecall;
    function get_TintOpacity: Double; safecall;
    procedure put_TintOpacity(value: Double); safecall;
    function get_TintTransitionDuration: TimeSpan; safecall;
    procedure put_TintTransitionDuration(value: TimeSpan); safecall;
    function get_AlwaysUseFallback: Boolean; safecall;
    procedure put_AlwaysUseFallback(value: Boolean); safecall;
    property AlwaysUseFallback: Boolean read get_AlwaysUseFallback write put_AlwaysUseFallback;
    property BackgroundSource: AcrylicBackgroundSource read get_BackgroundSource write put_BackgroundSource;
    property TintColor: Color read get_TintColor write put_TintColor;
    property TintOpacity: Double read get_TintOpacity write put_TintOpacity;
    property TintTransitionDuration: TimeSpan read get_TintTransitionDuration write put_TintTransitionDuration;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IAcrylicBrush2
  IAcrylicBrush2 = interface(IInspectable)
  ['{C9645383-B19E-5AC0-86FF-3D90506DBCDA}']
    function get_TintLuminosityOpacity: IReference_1__Double; safecall;
    procedure put_TintLuminosityOpacity(value: IReference_1__Double); safecall;
    property TintLuminosityOpacity: IReference_1__Double read get_TintLuminosityOpacity write put_TintLuminosityOpacity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IAcrylicBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_AcrylicBrush)]
  IAcrylicBrushFactory = interface(IInspectable)
  ['{81A32568-F6CC-4013-8363-928AE23B7A61}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IAcrylicBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IAcrylicBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_AcrylicBrush)]
  IAcrylicBrushStatics = interface(IInspectable)
  ['{2787FD79-A3DA-423F-B81A-599147971523}']
    function get_BackgroundSourceProperty: IDependencyProperty; safecall;
    function get_TintColorProperty: IDependencyProperty; safecall;
    function get_TintOpacityProperty: IDependencyProperty; safecall;
    function get_TintTransitionDurationProperty: IDependencyProperty; safecall;
    function get_AlwaysUseFallbackProperty: IDependencyProperty; safecall;
    property AlwaysUseFallbackProperty: IDependencyProperty read get_AlwaysUseFallbackProperty;
    property BackgroundSourceProperty: IDependencyProperty read get_BackgroundSourceProperty;
    property TintColorProperty: IDependencyProperty read get_TintColorProperty;
    property TintOpacityProperty: IDependencyProperty read get_TintOpacityProperty;
    property TintTransitionDurationProperty: IDependencyProperty read get_TintTransitionDurationProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IAcrylicBrushStatics2
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_AcrylicBrush)]
  IAcrylicBrushStatics2 = interface(IInspectable)
  ['{129188A8-BF11-5BBC-8445-8C510E5926C0}']
    function get_TintLuminosityOpacityProperty: IDependencyProperty; safecall;
    property TintLuminosityOpacityProperty: IDependencyProperty read get_TintLuminosityOpacityProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IArcSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ArcSegment)]
  IArcSegment = interface(IInspectable)
  ['{07940C5F-63FB-4469-91BE-F1097C168052}']
    function get_Point: TPointF; safecall;
    procedure put_Point(value: TPointF); safecall;
    function get_Size: TSizeF; safecall;
    procedure put_Size(value: TSizeF); safecall;
    function get_RotationAngle: Double; safecall;
    procedure put_RotationAngle(value: Double); safecall;
    function get_IsLargeArc: Boolean; safecall;
    procedure put_IsLargeArc(value: Boolean); safecall;
    function get_SweepDirection: SweepDirection; safecall;
    procedure put_SweepDirection(value: SweepDirection); safecall;
    property IsLargeArc: Boolean read get_IsLargeArc write put_IsLargeArc;
    property Point: TPointF read get_Point write put_Point;
    property RotationAngle: Double read get_RotationAngle write put_RotationAngle;
    property Size: TSizeF read get_Size write put_Size;
    property SweepDirection_: SweepDirection read get_SweepDirection write put_SweepDirection;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IArcSegmentStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ArcSegment)]
  IArcSegmentStatics = interface(IInspectable)
  ['{82348F6E-8A69-4204-9C12-7207DF317643}']
    function get_PointProperty: IDependencyProperty; safecall;
    function get_SizeProperty: IDependencyProperty; safecall;
    function get_RotationAngleProperty: IDependencyProperty; safecall;
    function get_IsLargeArcProperty: IDependencyProperty; safecall;
    function get_SweepDirectionProperty: IDependencyProperty; safecall;
    property IsLargeArcProperty: IDependencyProperty read get_IsLargeArcProperty;
    property PointProperty: IDependencyProperty read get_PointProperty;
    property RotationAngleProperty: IDependencyProperty read get_RotationAngleProperty;
    property SizeProperty: IDependencyProperty read get_SizeProperty;
    property SweepDirectionProperty: IDependencyProperty read get_SweepDirectionProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IBezierSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_BezierSegment)]
  IBezierSegment = interface(IInspectable)
  ['{AF4BB9EE-8984-49B7-81DF-3F35994B95EB}']
    function get_Point1: TPointF; safecall;
    procedure put_Point1(value: TPointF); safecall;
    function get_Point2: TPointF; safecall;
    procedure put_Point2(value: TPointF); safecall;
    function get_Point3: TPointF; safecall;
    procedure put_Point3(value: TPointF); safecall;
    property Point1: TPointF read get_Point1 write put_Point1;
    property Point2: TPointF read get_Point2 write put_Point2;
    property Point3: TPointF read get_Point3 write put_Point3;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IBezierSegmentStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_BezierSegment)]
  IBezierSegmentStatics = interface(IInspectable)
  ['{C0287BAC-1410-4530-8452-1C9D0AD1F341}']
    function get_Point1Property: IDependencyProperty; safecall;
    function get_Point2Property: IDependencyProperty; safecall;
    function get_Point3Property: IDependencyProperty; safecall;
    property Point1Property: IDependencyProperty read get_Point1Property;
    property Point2Property: IDependencyProperty read get_Point2Property;
    property Point3Property: IDependencyProperty read get_Point3Property;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IBitmapCache
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_BitmapCache)]
  IBitmapCache = interface(IInspectable)
  ['{79C2219E-44D2-4610-9735-9BEC83809ECF}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Brush)]
  IBrushFactory = interface(IInspectable)
  ['{399658A2-14FB-4B8F-83E6-6E3DAB12069B}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IBrushOverrides2
  IBrushOverrides2 = interface(IInspectable)
  ['{D092B151-D83B-5A81-A71E-A1C7F8AD6963}']
    procedure PopulatePropertyInfoOverride(propertyName: HSTRING; animationPropertyInfo: IAnimationPropertyInfo); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Brush)]
  IBrushStatics = interface(IInspectable)
  ['{E70C3102-0225-47F5-B22E-0467619F6A22}']
    function get_OpacityProperty: IDependencyProperty; safecall;
    function get_TransformProperty: IDependencyProperty; safecall;
    function get_RelativeTransformProperty: IDependencyProperty; safecall;
    property OpacityProperty: IDependencyProperty read get_OpacityProperty;
    property RelativeTransformProperty: IDependencyProperty read get_RelativeTransformProperty;
    property TransformProperty: IDependencyProperty read get_TransformProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ICacheModeFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_CacheMode)]
  ICacheModeFactory = interface(IInspectable)
  ['{EB1F8C5B-0ABB-4E70-B8A8-620D0D953AB2}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): ICacheMode; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ICompositeTransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_CompositeTransform)]
  ICompositeTransform = interface(IInspectable)
  ['{C8A4385B-F24A-4701-A265-A78846F142B9}']
    function get_CenterX: Double; safecall;
    procedure put_CenterX(value: Double); safecall;
    function get_CenterY: Double; safecall;
    procedure put_CenterY(value: Double); safecall;
    function get_ScaleX: Double; safecall;
    procedure put_ScaleX(value: Double); safecall;
    function get_ScaleY: Double; safecall;
    procedure put_ScaleY(value: Double); safecall;
    function get_SkewX: Double; safecall;
    procedure put_SkewX(value: Double); safecall;
    function get_SkewY: Double; safecall;
    procedure put_SkewY(value: Double); safecall;
    function get_Rotation: Double; safecall;
    procedure put_Rotation(value: Double); safecall;
    function get_TranslateX: Double; safecall;
    procedure put_TranslateX(value: Double); safecall;
    function get_TranslateY: Double; safecall;
    procedure put_TranslateY(value: Double); safecall;
    property CenterX: Double read get_CenterX write put_CenterX;
    property CenterY: Double read get_CenterY write put_CenterY;
    property Rotation: Double read get_Rotation write put_Rotation;
    property ScaleX: Double read get_ScaleX write put_ScaleX;
    property ScaleY: Double read get_ScaleY write put_ScaleY;
    property SkewX: Double read get_SkewX write put_SkewX;
    property SkewY: Double read get_SkewY write put_SkewY;
    property TranslateX: Double read get_TranslateX write put_TranslateX;
    property TranslateY: Double read get_TranslateY write put_TranslateY;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ICompositeTransformStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_CompositeTransform)]
  ICompositeTransformStatics = interface(IInspectable)
  ['{2F190C08-8266-496F-9653-A18BD4F836AA}']
    function get_CenterXProperty: IDependencyProperty; safecall;
    function get_CenterYProperty: IDependencyProperty; safecall;
    function get_ScaleXProperty: IDependencyProperty; safecall;
    function get_ScaleYProperty: IDependencyProperty; safecall;
    function get_SkewXProperty: IDependencyProperty; safecall;
    function get_SkewYProperty: IDependencyProperty; safecall;
    function get_RotationProperty: IDependencyProperty; safecall;
    function get_TranslateXProperty: IDependencyProperty; safecall;
    function get_TranslateYProperty: IDependencyProperty; safecall;
    property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    property CenterYProperty: IDependencyProperty read get_CenterYProperty;
    property RotationProperty: IDependencyProperty read get_RotationProperty;
    property ScaleXProperty: IDependencyProperty read get_ScaleXProperty;
    property ScaleYProperty: IDependencyProperty read get_ScaleYProperty;
    property SkewXProperty: IDependencyProperty read get_SkewXProperty;
    property SkewYProperty: IDependencyProperty read get_SkewYProperty;
    property TranslateXProperty: IDependencyProperty read get_TranslateXProperty;
    property TranslateYProperty: IDependencyProperty read get_TranslateYProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ICompositionTarget
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_CompositionTarget)]
  ICompositionTarget = interface(IInspectable)
  ['{26CFBFF0-713C-4BEC-8803-E101F7B14ED3}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ICompositionTargetStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_CompositionTarget)]
  ICompositionTargetStatics = interface(IInspectable)
  ['{2B1AF03D-1ED2-4B59-BD00-7594EE92832B}']
    function add_Rendering(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Rendering(token: EventRegistrationToken); safecall;
    function add_SurfaceContentsLost(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_SurfaceContentsLost(token: EventRegistrationToken); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRenderedEventArgs
  IRenderedEventArgs = interface(IInspectable)
  ['{E349817D-81C7-4938-828C-A7E2797B35A6}']
    function get_FrameDuration: TimeSpan; safecall;
    property FrameDuration: TimeSpan read get_FrameDuration;
  end;

  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.UI.Xaml.Media.IRenderedEventArgs>
  EventHandler_1__IRenderedEventArgs = interface(IUnknown)
  ['{CB1D5F8C-DC90-5572-A032-28E33E3AF167}']
    procedure Invoke(sender: IInspectable; args: IRenderedEventArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ICompositionTargetStatics3
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_CompositionTarget)]
  ICompositionTargetStatics3 = interface(IInspectable)
  ['{BC0A7CD9-6750-4708-994C-2028E0312AC8}']
    function add_Rendered(handler: EventHandler_1__IRenderedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Rendered(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IEllipseGeometry
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_EllipseGeometry)]
  IEllipseGeometry = interface(IInspectable)
  ['{D4F61BBA-4EA2-40D6-AA6C-8D38AA87651F}']
    function get_Center: TPointF; safecall;
    procedure put_Center(value: TPointF); safecall;
    function get_RadiusX: Double; safecall;
    procedure put_RadiusX(value: Double); safecall;
    function get_RadiusY: Double; safecall;
    procedure put_RadiusY(value: Double); safecall;
    property Center: TPointF read get_Center write put_Center;
    property RadiusX: Double read get_RadiusX write put_RadiusX;
    property RadiusY: Double read get_RadiusY write put_RadiusY;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IEllipseGeometryStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_EllipseGeometry)]
  IEllipseGeometryStatics = interface(IInspectable)
  ['{1744DB47-F635-4B16-AEE6-E052A65DEFB2}']
    function get_CenterProperty: IDependencyProperty; safecall;
    function get_RadiusXProperty: IDependencyProperty; safecall;
    function get_RadiusYProperty: IDependencyProperty; safecall;
    property CenterProperty: IDependencyProperty read get_CenterProperty;
    property RadiusXProperty: IDependencyProperty read get_RadiusXProperty;
    property RadiusYProperty: IDependencyProperty read get_RadiusYProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IFontFamilyFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_FontFamily)]
  IFontFamilyFactory = interface(IInspectable)
  ['{D5603377-3DAE-4DCD-AF09-F9498E9EC659}']
    function CreateInstanceWithName(familyName: HSTRING; baseInterface: IInspectable; out innerInterface: IInspectable): IFontFamily; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IFontFamilyStatics2
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_FontFamily)]
  IFontFamilyStatics2 = interface(IInspectable)
  ['{52AD7AF9-37E6-4297-A238-97FB6A408D9E}']
    function get_XamlAutoFontFamily: IFontFamily; safecall;
    property XamlAutoFontFamily: IFontFamily read get_XamlAutoFontFamily;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGeneralTransformFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GeneralTransform)]
  IGeneralTransformFactory = interface(IInspectable)
  ['{7A25C930-29C4-4E31-B6F9-DEDD52E4DF1B}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IGeneralTransform; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGeneralTransformOverrides
  IGeneralTransformOverrides = interface(IInspectable)
  ['{4F121083-24CF-4524-90AD-8A42B1C12783}']
    function get_InverseCore: IGeneralTransform; safecall;
    function TryTransformCore(inPoint: TPointF; out outPoint: TPointF): Boolean; safecall;
    function TransformBoundsCore(rect: TRectF): TRectF; safecall;
    property InverseCore: IGeneralTransform read get_InverseCore;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGeometryFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Geometry)]
  IGeometryFactory = interface(IInspectable)
  ['{F65DAF23-D5FD-42F9-B32A-929C5A4B54E1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IGeometry>
  IIterator_1__IGeometry_Base = interface(IInspectable)
  ['{8726DCBF-961D-527A-A7CB-2DB47E1C9092}']
    function get_Current: IGeometry; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIGeometry): Cardinal; safecall;
    property Current: IGeometry read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IGeometry>
  IIterator_1__IGeometry = interface(IIterator_1__IGeometry_Base)
  ['{27E7DDCC-F613-5B9A-B9E0-110891C29A25}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGeometry>
  IIterable_1__IGeometry_Base = interface(IInspectable)
  ['{35340039-0DFC-52B4-8748-0D9A755DB8A8}']
    function First: IIterator_1__IGeometry; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGeometry>
  IIterable_1__IGeometry = interface(IIterable_1__IGeometry_Base)
  ['{ACAD11D1-6C32-5439-A5F5-5FD158EAABDC}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IGeometry>
  IVectorView_1__IGeometry = interface(IInspectable)
  ['{91C765D4-558E-584F-8518-81604FD414A0}']
    function GetAt(index: Cardinal): IGeometry; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IGeometry; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGeometry): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGeometry>
  IVector_1__IGeometry_Base = interface(IInspectable)
  ['{84C6AC3A-8207-5599-9583-606AC2139DDD}']
    function GetAt(index: Cardinal): IGeometry; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IGeometry; safecall;
    function IndexOf(value: IGeometry; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IGeometry); safecall;
    procedure InsertAt(index: Cardinal; value: IGeometry); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IGeometry); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGeometry): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIGeometry); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGeometry>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GeometryCollection)]
  IVector_1__IGeometry = interface(IVector_1__IGeometry_Base)
  ['{06B8DB4B-0D9E-5441-9C10-EC821F81E29A}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGeometryGroup
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GeometryGroup)]
  IGeometryGroup = interface(IInspectable)
  ['{55225A61-8677-4C8C-8E46-EE3DC355114B}']
    function get_FillRule: FillRule; safecall;
    procedure put_FillRule(value: FillRule); safecall;
    function get_Children: IVector_1__IGeometry; safecall;
    procedure put_Children(value: IVector_1__IGeometry); safecall;
    property Children: IVector_1__IGeometry read get_Children write put_Children;
    property FillRule_: FillRule read get_FillRule write put_FillRule;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGeometryGroupStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GeometryGroup)]
  IGeometryGroupStatics = interface(IInspectable)
  ['{56C955F4-8496-4BB6-ABF0-617B1FE78B45}']
    function get_FillRuleProperty: IDependencyProperty; safecall;
    function get_ChildrenProperty: IDependencyProperty; safecall;
    property ChildrenProperty: IDependencyProperty read get_ChildrenProperty;
    property FillRuleProperty: IDependencyProperty read get_FillRuleProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGeometryStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Geometry)]
  IGeometryStatics = interface(IInspectable)
  ['{7A70AA8C-0B06-465F-B637-9A47E5A70111}']
    function get_Empty: IGeometry; safecall;
    function get_StandardFlatteningTolerance: Double; safecall;
    function get_TransformProperty: IDependencyProperty; safecall;
    property Empty: IGeometry read get_Empty;
    property StandardFlatteningTolerance: Double read get_StandardFlatteningTolerance;
    property TransformProperty: IDependencyProperty read get_TransformProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGradientStop
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GradientStop)]
  IGradientStop = interface(IInspectable)
  ['{665F44FE-2E59-4C4A-AB53-076A100CCD81}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    function get_Offset: Double; safecall;
    procedure put_Offset(value: Double); safecall;
    property Color_: Color read get_Color write put_Color;
    property Offset: Double read get_Offset write put_Offset;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IGradientStop>
  IIterator_1__IGradientStop_Base = interface(IInspectable)
  ['{01DCA32B-0938-5E4A-94AD-BB4906F651ED}']
    function get_Current: IGradientStop; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIGradientStop): Cardinal; safecall;
    property Current: IGradientStop read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IGradientStop>
  IIterator_1__IGradientStop = interface(IIterator_1__IGradientStop_Base)
  ['{AC65BBD9-7A3C-5408-9EB7-7C1737D7E7D2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGradientStop>
  IIterable_1__IGradientStop_Base = interface(IInspectable)
  ['{9105BB93-AC26-5BAE-8C1E-DF8ECF00DEE6}']
    function First: IIterator_1__IGradientStop; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGradientStop>
  IIterable_1__IGradientStop = interface(IIterable_1__IGradientStop_Base)
  ['{18A97692-A91A-508E-90B5-EB933D98960D}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IGradientStop>
  IVectorView_1__IGradientStop = interface(IInspectable)
  ['{14A9FAF8-A167-53A1-82C3-B45933DB29DE}']
    function GetAt(index: Cardinal): IGradientStop; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IGradientStop; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGradientStop): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGradientStop>
  IVector_1__IGradientStop_Base = interface(IInspectable)
  ['{33422DA4-24F3-5B52-9E8D-11DD71F08BB1}']
    function GetAt(index: Cardinal): IGradientStop; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IGradientStop; safecall;
    function IndexOf(value: IGradientStop; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IGradientStop); safecall;
    procedure InsertAt(index: Cardinal; value: IGradientStop); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IGradientStop); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIGradientStop): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIGradientStop); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGradientStop>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GradientStopCollection)]
  IVector_1__IGradientStop = interface(IVector_1__IGradientStop_Base)
  ['{4F041AD3-D933-5139-A296-1E0F2C35C1E5}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGradientBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GradientBrush)]
  IGradientBrush = interface(IInspectable)
  ['{2166E69F-935A-4191-8E3C-1C8DFDFCDC78}']
    function get_SpreadMethod: GradientSpreadMethod; safecall;
    procedure put_SpreadMethod(value: GradientSpreadMethod); safecall;
    function get_MappingMode: BrushMappingMode; safecall;
    procedure put_MappingMode(value: BrushMappingMode); safecall;
    function get_ColorInterpolationMode: ColorInterpolationMode; safecall;
    procedure put_ColorInterpolationMode(value: ColorInterpolationMode); safecall;
    function get_GradientStops: IVector_1__IGradientStop; safecall;
    procedure put_GradientStops(value: IVector_1__IGradientStop); safecall;
    property ColorInterpolationMode_: ColorInterpolationMode read get_ColorInterpolationMode write put_ColorInterpolationMode;
    property GradientStops: IVector_1__IGradientStop read get_GradientStops write put_GradientStops;
    property MappingMode: BrushMappingMode read get_MappingMode write put_MappingMode;
    property SpreadMethod: GradientSpreadMethod read get_SpreadMethod write put_SpreadMethod;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGradientBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GradientBrush)]
  IGradientBrushFactory = interface(IInspectable)
  ['{ED4779CA-45BD-4131-B625-BE86E07C6112}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IGradientBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGradientBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GradientBrush)]
  IGradientBrushStatics = interface(IInspectable)
  ['{961661F9-8BB4-4E6C-B923-B5D787E0F1A9}']
    function get_SpreadMethodProperty: IDependencyProperty; safecall;
    function get_MappingModeProperty: IDependencyProperty; safecall;
    function get_ColorInterpolationModeProperty: IDependencyProperty; safecall;
    function get_GradientStopsProperty: IDependencyProperty; safecall;
    property ColorInterpolationModeProperty: IDependencyProperty read get_ColorInterpolationModeProperty;
    property GradientStopsProperty: IDependencyProperty read get_GradientStopsProperty;
    property MappingModeProperty: IDependencyProperty read get_MappingModeProperty;
    property SpreadMethodProperty: IDependencyProperty read get_SpreadMethodProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IGradientStopStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_GradientStop)]
  IGradientStopStatics = interface(IInspectable)
  ['{602A6D75-6193-4FE5-8E82-C7C6F6FEBAFD}']
    function get_ColorProperty: IDependencyProperty; safecall;
    function get_OffsetProperty: IDependencyProperty; safecall;
    property ColorProperty: IDependencyProperty read get_ColorProperty;
    property OffsetProperty: IDependencyProperty read get_OffsetProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IImageBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ImageBrush)]
  IImageBrush = interface(IInspectable)
  ['{9FD11377-C12A-4493-BF7D-F3A8AD74B554}']
    function get_ImageSource: IImageSource; safecall;
    procedure put_ImageSource(value: IImageSource); safecall;
    function add_ImageFailed(handler: ExceptionRoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ImageFailed(token: EventRegistrationToken); safecall;
    function add_ImageOpened(handler: RoutedEventHandler): EventRegistrationToken; safecall;
    procedure remove_ImageOpened(token: EventRegistrationToken); safecall;
    property ImageSource: IImageSource read get_ImageSource write put_ImageSource;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IImageBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ImageBrush)]
  IImageBrushStatics = interface(IInspectable)
  ['{1255B1B2-DD18-42E5-892C-EAE30C305B8C}']
    function get_ImageSourceProperty: IDependencyProperty; safecall;
    property ImageSourceProperty: IDependencyProperty read get_ImageSourceProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IImageSourceFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ImageSource)]
  IImageSourceFactory = interface(IInspectable)
  ['{297EC001-2540-4E5A-AB66-88035DD3DDB5}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILineGeometry
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LineGeometry)]
  ILineGeometry = interface(IInspectable)
  ['{30EDD4A2-8FC5-40AF-A7A2-C27FE7AA1363}']
    function get_StartPoint: TPointF; safecall;
    procedure put_StartPoint(value: TPointF); safecall;
    function get_EndPoint: TPointF; safecall;
    procedure put_EndPoint(value: TPointF); safecall;
    property EndPoint: TPointF read get_EndPoint write put_EndPoint;
    property StartPoint: TPointF read get_StartPoint write put_StartPoint;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILineGeometryStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LineGeometry)]
  ILineGeometryStatics = interface(IInspectable)
  ['{578AE763-5562-4EE4-8703-EA4036D891E3}']
    function get_StartPointProperty: IDependencyProperty; safecall;
    function get_EndPointProperty: IDependencyProperty; safecall;
    property EndPointProperty: IDependencyProperty read get_EndPointProperty;
    property StartPointProperty: IDependencyProperty read get_StartPointProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILineSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LineSegment)]
  ILineSegment = interface(IInspectable)
  ['{EF6A2E25-3FF0-4420-A411-7182A4CECB15}']
    function get_Point: TPointF; safecall;
    procedure put_Point(value: TPointF); safecall;
    property Point: TPointF read get_Point write put_Point;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILineSegmentStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LineSegment)]
  ILineSegmentStatics = interface(IInspectable)
  ['{9FCAB141-04C0-4AFB-87B3-E800B969B894}']
    function get_PointProperty: IDependencyProperty; safecall;
    property PointProperty: IDependencyProperty read get_PointProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILinearGradientBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LinearGradientBrush)]
  ILinearGradientBrush = interface(IInspectable)
  ['{8E96D16B-BB84-4C6F-9DBF-9D6C5C6D9C39}']
    function get_StartPoint: TPointF; safecall;
    procedure put_StartPoint(value: TPointF); safecall;
    function get_EndPoint: TPointF; safecall;
    procedure put_EndPoint(value: TPointF); safecall;
    property EndPoint: TPointF read get_EndPoint write put_EndPoint;
    property StartPoint: TPointF read get_StartPoint write put_StartPoint;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILinearGradientBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LinearGradientBrush)]
  ILinearGradientBrushFactory = interface(IInspectable)
  ['{0AE0861C-1E7A-4FED-9857-EA8CAA798490}']
    function CreateInstanceWithGradientStopCollectionAndAngle(gradientStopCollection: IVector_1__IGradientStop; angle: Double): ILinearGradientBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILinearGradientBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LinearGradientBrush)]
  ILinearGradientBrushStatics = interface(IInspectable)
  ['{7AF6E504-2DC3-40E3-BE0B-B314C13CB991}']
    function get_StartPointProperty: IDependencyProperty; safecall;
    function get_EndPointProperty: IDependencyProperty; safecall;
    property EndPointProperty: IDependencyProperty read get_EndPointProperty;
    property StartPointProperty: IDependencyProperty read get_StartPointProperty;
  end;

  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILoadedImageSourceLoadCompletedEventArgs
  ILoadedImageSourceLoadCompletedEventArgs = interface(IInspectable)
  ['{1AC60B1E-7837-4489-B3E5-D0D5AD0A56C4}']
    function get_Status: LoadedImageSourceLoadStatus; safecall;
    property Status: LoadedImageSourceLoadStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.ILoadedImageSurface,Windows.UI.Xaml.Media.ILoadedImageSourceLoadCompletedEventArgs>
  TypedEventHandler_2__ILoadedImageSurface__ILoadedImageSourceLoadCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{0AA0C46E-1DB6-5850-AE17-EC310EA3FD2D}']
    procedure Invoke(sender: ILoadedImageSurface; args: ILoadedImageSourceLoadCompletedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.ILoadedImageSurface,Windows.UI.Xaml.Media.ILoadedImageSourceLoadCompletedEventArgs>
  TypedEventHandler_2__ILoadedImageSurface__ILoadedImageSourceLoadCompletedEventArgs = interface(TypedEventHandler_2__ILoadedImageSurface__ILoadedImageSourceLoadCompletedEventArgs_Delegate_Base)
  ['{A0E5578B-FE3E-57EE-9CF1-D8ED7F887DB3}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILoadedImageSurface
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LoadedImageSurface)]
  ILoadedImageSurface = interface(IInspectable)
  ['{050C8313-6737-45BA-8531-33094FEBEF55}']
    function get_DecodedPhysicalSize: TSizeF; safecall;
    function get_DecodedSize: TSizeF; safecall;
    function get_NaturalSize: TSizeF; safecall;
    function add_LoadCompleted(handler: TypedEventHandler_2__ILoadedImageSurface__ILoadedImageSourceLoadCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_LoadCompleted(token: EventRegistrationToken); safecall;
    property DecodedPhysicalSize: TSizeF read get_DecodedPhysicalSize;
    property DecodedSize: TSizeF read get_DecodedSize;
    property NaturalSize: TSizeF read get_NaturalSize;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ILoadedImageSurfaceStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_LoadedImageSurface)]
  ILoadedImageSurfaceStatics = interface(IInspectable)
  ['{22B8EDF6-84AD-40AB-937D-4871613E765D}']
    function StartLoadFromUri(uri: IUriRuntimeClass; desiredMaxSize: TSizeF): ILoadedImageSurface; overload; safecall;
    function StartLoadFromUri(uri: IUriRuntimeClass): ILoadedImageSurface; overload; safecall;
    function StartLoadFromStream(stream: IRandomAccessStream; desiredMaxSize: TSizeF): ILoadedImageSurface; overload; safecall;
    function StartLoadFromStream(stream: IRandomAccessStream): ILoadedImageSurface; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IMatrix3DProjection
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Matrix3DProjection)]
  IMatrix3DProjection = interface(IInspectable)
  ['{6F03E149-BFC9-4C01-B578-50338CEC97FC}']
    function get_ProjectionMatrix: Media3D_Matrix3D; safecall;
    procedure put_ProjectionMatrix(value: Media3D_Matrix3D); safecall;
    property ProjectionMatrix: Media3D_Matrix3D read get_ProjectionMatrix write put_ProjectionMatrix;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IMatrix3DProjectionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Matrix3DProjection)]
  IMatrix3DProjectionStatics = interface(IInspectable)
  ['{AE9D5895-41EC-4E37-ABAA-69F41D2F876B}']
    function get_ProjectionMatrixProperty: IDependencyProperty; safecall;
    property ProjectionMatrixProperty: IDependencyProperty read get_ProjectionMatrixProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IMatrixHelper
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_MatrixHelper)]
  IMatrixHelper = interface(IInspectable)
  ['{F3CF4882-06B5-48C8-9EB2-1763E9364038}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IMatrixHelperStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_MatrixHelper)]
  IMatrixHelperStatics = interface(IInspectable)
  ['{C18606A6-39F4-4B8A-8403-28E5E5F033B4}']
    function get_Identity: Matrix; safecall;
    function FromElements(m11: Double; m12: Double; m21: Double; m22: Double; offsetX: Double; offsetY: Double): Matrix; safecall;
    function GetIsIdentity(target: Matrix): Boolean; safecall;
    function Transform(target: Matrix; point: TPointF): TPointF; safecall;
    property Identity: Matrix read get_Identity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IMatrixTransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_MatrixTransform)]
  IMatrixTransform = interface(IInspectable)
  ['{EDFDD551-5FED-45FC-AE62-92A4B6CF9707}']
    function get_Matrix: Matrix; safecall;
    procedure put_Matrix(value: Matrix); safecall;
    property Matrix_: Matrix read get_Matrix write put_Matrix;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IMatrixTransformStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_MatrixTransform)]
  IMatrixTransformStatics = interface(IInspectable)
  ['{43E02E47-15B8-4758-BB97-7D52420ACC5B}']
    function get_MatrixProperty: IDependencyProperty; safecall;
    property MatrixProperty: IDependencyProperty read get_MatrixProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs2
  IPartialMediaFailureDetectedEventArgs2 = interface(IInspectable)
  ['{73074875-890D-416B-B9AE-E84DFD9C4B1B}']
    function get_ExtendedError: HRESULT; safecall;
    property ExtendedError: HRESULT read get_ExtendedError;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPathSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathSegment)]
  IPathSegment = interface(IInspectable)
  ['{FCFA71CF-9CE3-474F-8157-10B6435A616B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IPathSegment>
  IIterator_1__IPathSegment_Base = interface(IInspectable)
  ['{3CFEDCFB-7A75-5BC7-A7E2-95EE9BC6DD32}']
    function get_Current: IPathSegment; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIPathSegment): Cardinal; safecall;
    property Current: IPathSegment read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IPathSegment>
  IIterator_1__IPathSegment = interface(IIterator_1__IPathSegment_Base)
  ['{AA7BAA74-2699-5B9C-A760-5E46A08D132A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathSegment>
  IIterable_1__IPathSegment_Base = interface(IInspectable)
  ['{37E2CB21-B9C9-5006-BED7-4C328981B551}']
    function First: IIterator_1__IPathSegment; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathSegment>
  IIterable_1__IPathSegment = interface(IIterable_1__IPathSegment_Base)
  ['{66020054-E595-5443-9073-D287709F3BA7}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IPathSegment>
  IVectorView_1__IPathSegment = interface(IInspectable)
  ['{5835BC7E-1D95-5C94-BA19-80A10C279030}']
    function GetAt(index: Cardinal): IPathSegment; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IPathSegment; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPathSegment): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathSegment>
  IVector_1__IPathSegment_Base = interface(IInspectable)
  ['{11FDD506-B21D-564D-B6A8-FF991057B8F3}']
    function GetAt(index: Cardinal): IPathSegment; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IPathSegment; safecall;
    function IndexOf(value: IPathSegment; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IPathSegment); safecall;
    procedure InsertAt(index: Cardinal; value: IPathSegment); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IPathSegment); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPathSegment): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIPathSegment); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathSegment>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathSegmentCollection)]
  IVector_1__IPathSegment = interface(IVector_1__IPathSegment_Base)
  ['{817764B4-3BF8-5B8B-916A-A7230929DBE9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPathFigure
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathFigure)]
  IPathFigure = interface(IInspectable)
  ['{5D955C8C-5FA9-4DDA-A3CC-10FCDCAA20D7}']
    function get_Segments: IVector_1__IPathSegment; safecall;
    procedure put_Segments(value: IVector_1__IPathSegment); safecall;
    function get_StartPoint: TPointF; safecall;
    procedure put_StartPoint(value: TPointF); safecall;
    function get_IsClosed: Boolean; safecall;
    procedure put_IsClosed(value: Boolean); safecall;
    function get_IsFilled: Boolean; safecall;
    procedure put_IsFilled(value: Boolean); safecall;
    property IsClosed: Boolean read get_IsClosed write put_IsClosed;
    property IsFilled: Boolean read get_IsFilled write put_IsFilled;
    property Segments: IVector_1__IPathSegment read get_Segments write put_Segments;
    property StartPoint: TPointF read get_StartPoint write put_StartPoint;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPathFigureStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathFigure)]
  IPathFigureStatics = interface(IInspectable)
  ['{B60591D9-2395-4317-9552-3A58526F8C7B}']
    function get_SegmentsProperty: IDependencyProperty; safecall;
    function get_StartPointProperty: IDependencyProperty; safecall;
    function get_IsClosedProperty: IDependencyProperty; safecall;
    function get_IsFilledProperty: IDependencyProperty; safecall;
    property IsClosedProperty: IDependencyProperty read get_IsClosedProperty;
    property IsFilledProperty: IDependencyProperty read get_IsFilledProperty;
    property SegmentsProperty: IDependencyProperty read get_SegmentsProperty;
    property StartPointProperty: IDependencyProperty read get_StartPointProperty;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IPathFigure>
  IIterator_1__IPathFigure_Base = interface(IInspectable)
  ['{71BD579B-088E-5E59-8876-80613DCFF83E}']
    function get_Current: IPathFigure; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIPathFigure): Cardinal; safecall;
    property Current: IPathFigure read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.IPathFigure>
  IIterator_1__IPathFigure = interface(IIterator_1__IPathFigure_Base)
  ['{77CBEBB2-8DCA-58A5-8DB8-FAF18983F9EB}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathFigure>
  IIterable_1__IPathFigure_Base = interface(IInspectable)
  ['{CD1DC421-D6E5-5B3F-A8C7-9938F28B2995}']
    function First: IIterator_1__IPathFigure; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathFigure>
  IIterable_1__IPathFigure = interface(IIterable_1__IPathFigure_Base)
  ['{C5971062-709B-52FA-B4FC-49C4B5F27B5E}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.IPathFigure>
  IVectorView_1__IPathFigure = interface(IInspectable)
  ['{7EB8CC17-F9E8-5C0B-A5F4-EA77EF844721}']
    function GetAt(index: Cardinal): IPathFigure; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IPathFigure; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPathFigure): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathFigure>
  IVector_1__IPathFigure_Base = interface(IInspectable)
  ['{DE29D405-26DD-5E72-8BCB-495199D4E5B5}']
    function GetAt(index: Cardinal): IPathFigure; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IPathFigure; safecall;
    function IndexOf(value: IPathFigure; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IPathFigure); safecall;
    procedure InsertAt(index: Cardinal; value: IPathFigure); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IPathFigure); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIPathFigure): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIPathFigure); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathFigure>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathFigureCollection)]
  IVector_1__IPathFigure = interface(IVector_1__IPathFigure_Base)
  ['{B7CA298E-9486-5821-9893-3498E0A39EE3}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPathGeometry
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathGeometry)]
  IPathGeometry = interface(IInspectable)
  ['{081B9DF8-BAE6-4BCB-813C-BDE0E46DC8B7}']
    function get_FillRule: FillRule; safecall;
    procedure put_FillRule(value: FillRule); safecall;
    function get_Figures: IVector_1__IPathFigure; safecall;
    procedure put_Figures(value: IVector_1__IPathFigure); safecall;
    property Figures: IVector_1__IPathFigure read get_Figures write put_Figures;
    property FillRule_: FillRule read get_FillRule write put_FillRule;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPathGeometryStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathGeometry)]
  IPathGeometryStatics = interface(IInspectable)
  ['{D9E58BBA-2CBA-4741-8F8D-3198CF5186B9}']
    function get_FillRuleProperty: IDependencyProperty; safecall;
    function get_FiguresProperty: IDependencyProperty; safecall;
    property FiguresProperty: IDependencyProperty read get_FiguresProperty;
    property FillRuleProperty: IDependencyProperty read get_FillRuleProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPathSegmentFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PathSegment)]
  IPathSegmentFactory = interface(IInspectable)
  ['{2A1C0AAE-ECCD-4464-A148-6FFDB3AA281F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPlaneProjection
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PlaneProjection)]
  IPlaneProjection = interface(IInspectable)
  ['{E6F82BFA-6726-469A-B259-A5188347CA8F}']
    function get_LocalOffsetX: Double; safecall;
    procedure put_LocalOffsetX(value: Double); safecall;
    function get_LocalOffsetY: Double; safecall;
    procedure put_LocalOffsetY(value: Double); safecall;
    function get_LocalOffsetZ: Double; safecall;
    procedure put_LocalOffsetZ(value: Double); safecall;
    function get_RotationX: Double; safecall;
    procedure put_RotationX(value: Double); safecall;
    function get_RotationY: Double; safecall;
    procedure put_RotationY(value: Double); safecall;
    function get_RotationZ: Double; safecall;
    procedure put_RotationZ(value: Double); safecall;
    function get_CenterOfRotationX: Double; safecall;
    procedure put_CenterOfRotationX(value: Double); safecall;
    function get_CenterOfRotationY: Double; safecall;
    procedure put_CenterOfRotationY(value: Double); safecall;
    function get_CenterOfRotationZ: Double; safecall;
    procedure put_CenterOfRotationZ(value: Double); safecall;
    function get_GlobalOffsetX: Double; safecall;
    procedure put_GlobalOffsetX(value: Double); safecall;
    function get_GlobalOffsetY: Double; safecall;
    procedure put_GlobalOffsetY(value: Double); safecall;
    function get_GlobalOffsetZ: Double; safecall;
    procedure put_GlobalOffsetZ(value: Double); safecall;
    function get_ProjectionMatrix: Media3D_Matrix3D; safecall;
    property CenterOfRotationX: Double read get_CenterOfRotationX write put_CenterOfRotationX;
    property CenterOfRotationY: Double read get_CenterOfRotationY write put_CenterOfRotationY;
    property CenterOfRotationZ: Double read get_CenterOfRotationZ write put_CenterOfRotationZ;
    property GlobalOffsetX: Double read get_GlobalOffsetX write put_GlobalOffsetX;
    property GlobalOffsetY: Double read get_GlobalOffsetY write put_GlobalOffsetY;
    property GlobalOffsetZ: Double read get_GlobalOffsetZ write put_GlobalOffsetZ;
    property LocalOffsetX: Double read get_LocalOffsetX write put_LocalOffsetX;
    property LocalOffsetY: Double read get_LocalOffsetY write put_LocalOffsetY;
    property LocalOffsetZ: Double read get_LocalOffsetZ write put_LocalOffsetZ;
    property ProjectionMatrix: Media3D_Matrix3D read get_ProjectionMatrix;
    property RotationX: Double read get_RotationX write put_RotationX;
    property RotationY: Double read get_RotationY write put_RotationY;
    property RotationZ: Double read get_RotationZ write put_RotationZ;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPlaneProjectionStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PlaneProjection)]
  IPlaneProjectionStatics = interface(IInspectable)
  ['{AD919C67-3BDC-4855-8969-D1F9A3ADC27D}']
    function get_LocalOffsetXProperty: IDependencyProperty; safecall;
    function get_LocalOffsetYProperty: IDependencyProperty; safecall;
    function get_LocalOffsetZProperty: IDependencyProperty; safecall;
    function get_RotationXProperty: IDependencyProperty; safecall;
    function get_RotationYProperty: IDependencyProperty; safecall;
    function get_RotationZProperty: IDependencyProperty; safecall;
    function get_CenterOfRotationXProperty: IDependencyProperty; safecall;
    function get_CenterOfRotationYProperty: IDependencyProperty; safecall;
    function get_CenterOfRotationZProperty: IDependencyProperty; safecall;
    function get_GlobalOffsetXProperty: IDependencyProperty; safecall;
    function get_GlobalOffsetYProperty: IDependencyProperty; safecall;
    function get_GlobalOffsetZProperty: IDependencyProperty; safecall;
    function get_ProjectionMatrixProperty: IDependencyProperty; safecall;
    property CenterOfRotationXProperty: IDependencyProperty read get_CenterOfRotationXProperty;
    property CenterOfRotationYProperty: IDependencyProperty read get_CenterOfRotationYProperty;
    property CenterOfRotationZProperty: IDependencyProperty read get_CenterOfRotationZProperty;
    property GlobalOffsetXProperty: IDependencyProperty read get_GlobalOffsetXProperty;
    property GlobalOffsetYProperty: IDependencyProperty read get_GlobalOffsetYProperty;
    property GlobalOffsetZProperty: IDependencyProperty read get_GlobalOffsetZProperty;
    property LocalOffsetXProperty: IDependencyProperty read get_LocalOffsetXProperty;
    property LocalOffsetYProperty: IDependencyProperty read get_LocalOffsetYProperty;
    property LocalOffsetZProperty: IDependencyProperty read get_LocalOffsetZProperty;
    property ProjectionMatrixProperty: IDependencyProperty read get_ProjectionMatrixProperty;
    property RotationXProperty: IDependencyProperty read get_RotationXProperty;
    property RotationYProperty: IDependencyProperty read get_RotationYProperty;
    property RotationZProperty: IDependencyProperty read get_RotationZProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPolyBezierSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PolyBezierSegment)]
  IPolyBezierSegment = interface(IInspectable)
  ['{36805271-38C4-4BCF-96CD-028A6D38AF25}']
    function get_Points: IVector_1__Point; safecall;
    procedure put_Points(value: IVector_1__Point); safecall;
    property Points: IVector_1__Point read get_Points write put_Points;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPolyBezierSegmentStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PolyBezierSegment)]
  IPolyBezierSegmentStatics = interface(IInspectable)
  ['{1D91A6DA-1492-4ACC-BD66-A496F3D829D6}']
    function get_PointsProperty: IDependencyProperty; safecall;
    property PointsProperty: IDependencyProperty read get_PointsProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPolyLineSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PolyLineSegment)]
  IPolyLineSegment = interface(IInspectable)
  ['{4B397F87-A2E6-479D-BDC8-6F4464646887}']
    function get_Points: IVector_1__Point; safecall;
    procedure put_Points(value: IVector_1__Point); safecall;
    property Points: IVector_1__Point read get_Points write put_Points;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPolyLineSegmentStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PolyLineSegment)]
  IPolyLineSegmentStatics = interface(IInspectable)
  ['{D64A2C87-33F1-4E70-A47F-B4981EF648A2}']
    function get_PointsProperty: IDependencyProperty; safecall;
    property PointsProperty: IDependencyProperty read get_PointsProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPolyQuadraticBezierSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PolyQuadraticBezierSegment)]
  IPolyQuadraticBezierSegment = interface(IInspectable)
  ['{DD5CED7D-E6DB-4C96-B6A1-3FCE96E987A6}']
    function get_Points: IVector_1__Point; safecall;
    procedure put_Points(value: IVector_1__Point); safecall;
    property Points: IVector_1__Point read get_Points write put_Points;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IPolyQuadraticBezierSegmentStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_PolyQuadraticBezierSegment)]
  IPolyQuadraticBezierSegmentStatics = interface(IInspectable)
  ['{FDF5EB75-7AD5-4C89-8169-8C9786ABD9EB}']
    function get_PointsProperty: IDependencyProperty; safecall;
    property PointsProperty: IDependencyProperty read get_PointsProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IProjectionFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Projection)]
  IProjectionFactory = interface(IInspectable)
  ['{C4F29CAB-60AD-4F24-BD27-9D69C3127C9A}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IProjection; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IQuadraticBezierSegment
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_QuadraticBezierSegment)]
  IQuadraticBezierSegment = interface(IInspectable)
  ['{2C509A5B-BF18-455A-A078-914B5232D8AF}']
    function get_Point1: TPointF; safecall;
    procedure put_Point1(value: TPointF); safecall;
    function get_Point2: TPointF; safecall;
    procedure put_Point2(value: TPointF); safecall;
    property Point1: TPointF read get_Point1 write put_Point1;
    property Point2: TPointF read get_Point2 write put_Point2;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IQuadraticBezierSegmentStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_QuadraticBezierSegment)]
  IQuadraticBezierSegmentStatics = interface(IInspectable)
  ['{69C78278-3C0B-4B4F-B7A2-F003DED41BB0}']
    function get_Point1Property: IDependencyProperty; safecall;
    function get_Point2Property: IDependencyProperty; safecall;
    property Point1Property: IDependencyProperty read get_Point1Property;
    property Point2Property: IDependencyProperty read get_Point2Property;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRectangleGeometryStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RectangleGeometry)]
  IRectangleGeometryStatics = interface(IInspectable)
  ['{377F8DBA-7902-48E3-83BE-7C8002A6653C}']
    function get_RectProperty: IDependencyProperty; safecall;
    property RectProperty: IDependencyProperty read get_RectProperty;
  end;

  // Windows.UI.Xaml.Media.IRenderingEventArgs
  IRenderingEventArgs = interface(IInspectable)
  ['{5BF7D30D-9748-4AED-8380-D7890EB776A0}']
    function get_RenderingTime: TimeSpan; safecall;
    property RenderingTime: TimeSpan read get_RenderingTime;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRevealBackgroundBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RevealBackgroundBrush)]
  IRevealBackgroundBrush = interface(IInspectable)
  ['{261DCC0E-1991-4CDF-AEE0-6350A3F90BB9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRevealBackgroundBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RevealBackgroundBrush)]
  IRevealBackgroundBrushFactory = interface(IInspectable)
  ['{8C56BCAA-02A5-4F45-8506-8D39228F5D3F}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBackgroundBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRevealBorderBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RevealBorderBrush)]
  IRevealBorderBrush = interface(IInspectable)
  ['{060BA115-C542-483C-8202-5F03331866C9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRevealBorderBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RevealBorderBrush)]
  IRevealBorderBrushFactory = interface(IInspectable)
  ['{94C25298-F5F8-4482-A25C-6758501A8626}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBorderBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRevealBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RevealBrush)]
  IRevealBrush = interface(IInspectable)
  ['{2036A0ED-8271-4398-9019-25872093F13F}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    function get_TargetTheme: ApplicationTheme; safecall;
    procedure put_TargetTheme(value: ApplicationTheme); safecall;
    function get_AlwaysUseFallback: Boolean; safecall;
    procedure put_AlwaysUseFallback(value: Boolean); safecall;
    property AlwaysUseFallback: Boolean read get_AlwaysUseFallback write put_AlwaysUseFallback;
    property Color_: Color read get_Color write put_Color;
    property TargetTheme: ApplicationTheme read get_TargetTheme write put_TargetTheme;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRevealBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RevealBrush)]
  IRevealBrushFactory = interface(IInspectable)
  ['{9D9379CE-E3A0-4AAF-BE37-EA9D9DD43105}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRevealBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RevealBrush)]
  IRevealBrushStatics = interface(IInspectable)
  ['{190F2625-7209-4D42-A847-1AC4BBBB3499}']
    function get_ColorProperty: IDependencyProperty; safecall;
    function get_TargetThemeProperty: IDependencyProperty; safecall;
    function get_AlwaysUseFallbackProperty: IDependencyProperty; safecall;
    function get_StateProperty: IDependencyProperty; safecall;
    procedure SetState(element: IUIElement; value: RevealBrushState); safecall;
    function GetState(element: IUIElement): RevealBrushState; safecall;
    property AlwaysUseFallbackProperty: IDependencyProperty read get_AlwaysUseFallbackProperty;
    property ColorProperty: IDependencyProperty read get_ColorProperty;
    property StateProperty: IDependencyProperty read get_StateProperty;
    property TargetThemeProperty: IDependencyProperty read get_TargetThemeProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRotateTransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RotateTransform)]
  IRotateTransform = interface(IInspectable)
  ['{688EA9B9-1E4E-4596-86E3-428B27334FAF}']
    function get_CenterX: Double; safecall;
    procedure put_CenterX(value: Double); safecall;
    function get_CenterY: Double; safecall;
    procedure put_CenterY(value: Double); safecall;
    function get_Angle: Double; safecall;
    procedure put_Angle(value: Double); safecall;
    property Angle: Double read get_Angle write put_Angle;
    property CenterX: Double read get_CenterX write put_CenterX;
    property CenterY: Double read get_CenterY write put_CenterY;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IRotateTransformStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_RotateTransform)]
  IRotateTransformStatics = interface(IInspectable)
  ['{A131EB8A-51A3-41B6-B9D3-A10E429054AB}']
    function get_CenterXProperty: IDependencyProperty; safecall;
    function get_CenterYProperty: IDependencyProperty; safecall;
    function get_AngleProperty: IDependencyProperty; safecall;
    property AngleProperty: IDependencyProperty read get_AngleProperty;
    property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    property CenterYProperty: IDependencyProperty read get_CenterYProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IScaleTransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ScaleTransform)]
  IScaleTransform = interface(IInspectable)
  ['{ED67F18D-936E-43AB-929A-E9CD0A511E52}']
    function get_CenterX: Double; safecall;
    procedure put_CenterX(value: Double); safecall;
    function get_CenterY: Double; safecall;
    procedure put_CenterY(value: Double); safecall;
    function get_ScaleX: Double; safecall;
    procedure put_ScaleX(value: Double); safecall;
    function get_ScaleY: Double; safecall;
    procedure put_ScaleY(value: Double); safecall;
    property CenterX: Double read get_CenterX write put_CenterX;
    property CenterY: Double read get_CenterY write put_CenterY;
    property ScaleX: Double read get_ScaleX write put_ScaleX;
    property ScaleY: Double read get_ScaleY write put_ScaleY;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IScaleTransformStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ScaleTransform)]
  IScaleTransformStatics = interface(IInspectable)
  ['{9D9436F4-40A7-46DD-975A-07D337CD852E}']
    function get_CenterXProperty: IDependencyProperty; safecall;
    function get_CenterYProperty: IDependencyProperty; safecall;
    function get_ScaleXProperty: IDependencyProperty; safecall;
    function get_ScaleYProperty: IDependencyProperty; safecall;
    property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    property CenterYProperty: IDependencyProperty read get_CenterYProperty;
    property ScaleXProperty: IDependencyProperty read get_ScaleXProperty;
    property ScaleYProperty: IDependencyProperty read get_ScaleYProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IShadowFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Shadow)]
  IShadowFactory = interface(IInspectable)
  ['{19899F25-D28B-51E6-94B0-D7E709686305}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ISkewTransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_SkewTransform)]
  ISkewTransform = interface(IInspectable)
  ['{4E8A3B15-7A0F-4617-9E98-1E65BDC92115}']
    function get_CenterX: Double; safecall;
    procedure put_CenterX(value: Double); safecall;
    function get_CenterY: Double; safecall;
    procedure put_CenterY(value: Double); safecall;
    function get_AngleX: Double; safecall;
    procedure put_AngleX(value: Double); safecall;
    function get_AngleY: Double; safecall;
    procedure put_AngleY(value: Double); safecall;
    property AngleX: Double read get_AngleX write put_AngleX;
    property AngleY: Double read get_AngleY write put_AngleY;
    property CenterX: Double read get_CenterX write put_CenterX;
    property CenterY: Double read get_CenterY write put_CenterY;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ISkewTransformStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_SkewTransform)]
  ISkewTransformStatics = interface(IInspectable)
  ['{ECD11D73-5614-4B31-B6AF-BEAE10105624}']
    function get_CenterXProperty: IDependencyProperty; safecall;
    function get_CenterYProperty: IDependencyProperty; safecall;
    function get_AngleXProperty: IDependencyProperty; safecall;
    function get_AngleYProperty: IDependencyProperty; safecall;
    property AngleXProperty: IDependencyProperty read get_AngleXProperty;
    property AngleYProperty: IDependencyProperty read get_AngleYProperty;
    property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    property CenterYProperty: IDependencyProperty read get_CenterYProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ISolidColorBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_SolidColorBrush)]
  ISolidColorBrushFactory = interface(IInspectable)
  ['{D935CE0C-86F5-4DA6-8A27-B1619EF7F92B}']
    function CreateInstanceWithColor(color: Color): ISolidColorBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ISolidColorBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_SolidColorBrush)]
  ISolidColorBrushStatics = interface(IInspectable)
  ['{E1A65EFA-2B23-41BA-B9BA-7094EC8E4E9F}']
    function get_ColorProperty: IDependencyProperty; safecall;
    property ColorProperty: IDependencyProperty read get_ColorProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IThemeShadow
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ThemeShadow)]
  IThemeShadow = interface(IInspectable)
  ['{3ECCAD09-7985-5F39-8B62-6C10696DCA6F}']
    function get_Receivers: IUIElementWeakCollection; safecall;
    property Receivers: IUIElementWeakCollection read get_Receivers;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IThemeShadowFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_ThemeShadow)]
  IThemeShadowFactory = interface(IInspectable)
  ['{2E71465D-0F67-590E-831B-7E5E2A32B778}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IThemeShadow; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITileBrush
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TileBrush)]
  ITileBrush = interface(IInspectable)
  ['{C201CF06-CD84-48A5-9607-664D7361CD61}']
    function get_AlignmentX: AlignmentX; safecall;
    procedure put_AlignmentX(value: AlignmentX); safecall;
    function get_AlignmentY: AlignmentY; safecall;
    procedure put_AlignmentY(value: AlignmentY); safecall;
    function get_Stretch: Stretch; safecall;
    procedure put_Stretch(value: Stretch); safecall;
    property AlignmentX_: AlignmentX read get_AlignmentX write put_AlignmentX;
    property AlignmentY_: AlignmentY read get_AlignmentY write put_AlignmentY;
    property Stretch_: Stretch read get_Stretch write put_Stretch;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITileBrushFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TileBrush)]
  ITileBrushFactory = interface(IInspectable)
  ['{AA159F7C-ED6A-4FB3-B014-B5C7E379A4DE}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): ITileBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITileBrushStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TileBrush)]
  ITileBrushStatics = interface(IInspectable)
  ['{3497C25B-B562-4E68-8435-2399F6EB94D5}']
    function get_AlignmentXProperty: IDependencyProperty; safecall;
    function get_AlignmentYProperty: IDependencyProperty; safecall;
    function get_StretchProperty: IDependencyProperty; safecall;
    property AlignmentXProperty: IDependencyProperty read get_AlignmentXProperty;
    property AlignmentYProperty: IDependencyProperty read get_AlignmentYProperty;
    property StretchProperty: IDependencyProperty read get_StretchProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITimelineMarkerStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TimelineMarker)]
  ITimelineMarkerStatics = interface(IInspectable)
  ['{C4AEF0C6-16A3-484B-87F5-6528B8F04A47}']
    function get_TimeProperty: IDependencyProperty; safecall;
    function get_TypeProperty: IDependencyProperty; safecall;
    function get_TextProperty: IDependencyProperty; safecall;
    property TextProperty: IDependencyProperty read get_TextProperty;
    property TimeProperty: IDependencyProperty read get_TimeProperty;
    property TypeProperty: IDependencyProperty read get_TypeProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITransformFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Transform)]
  ITransformFactory = interface(IInspectable)
  ['{1A955A66-7CF4-4320-B416-6181192FCC6D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.ITransform>
  IIterator_1__ITransform_Base = interface(IInspectable)
  ['{2CDBD536-4598-5F0F-A236-11EC053C8E4B}']
    function get_Current: ITransform; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PITransform): Cardinal; safecall;
    property Current: ITransform read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Xaml.Media.ITransform>
  IIterator_1__ITransform = interface(IIterator_1__ITransform_Base)
  ['{2823D9F4-C497-5F56-9B6F-B03608ADDF4C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITransform>
  IIterable_1__ITransform_Base = interface(IInspectable)
  ['{FB8CACBC-D3EC-51B8-9778-99EDE8463611}']
    function First: IIterator_1__ITransform; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITransform>
  IIterable_1__ITransform = interface(IIterable_1__ITransform_Base)
  ['{1F5A9FA9-5015-5288-BD9E-BA94588AB354}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Xaml.Media.ITransform>
  IVectorView_1__ITransform = interface(IInspectable)
  ['{3DFCEE58-7D16-57D4-BDEB-60FB4F41D8CA}']
    function GetAt(index: Cardinal): ITransform; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ITransform; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PITransform): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITransform>
  IVector_1__ITransform_Base = interface(IInspectable)
  ['{62D7BBC1-71F5-5611-A984-E571C2DE7BAA}']
    function GetAt(index: Cardinal): ITransform; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ITransform; safecall;
    function IndexOf(value: ITransform; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ITransform); safecall;
    procedure InsertAt(index: Cardinal; value: ITransform); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ITransform); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PITransform): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PITransform); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITransform>
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TransformCollection)]
  IVector_1__ITransform = interface(IVector_1__ITransform_Base)
  ['{2F1BB563-47B5-57C6-AA0B-01C6DB05D9F6}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITransformGroup
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TransformGroup)]
  ITransformGroup = interface(IInspectable)
  ['{63418CCC-8D2D-4737-B951-2AFCE1DDC4C4}']
    function get_Children: IVector_1__ITransform; safecall;
    procedure put_Children(value: IVector_1__ITransform); safecall;
    function get_Value: Matrix; safecall;
    property Children: IVector_1__ITransform read get_Children write put_Children;
    property Value: Matrix read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITransformGroupStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TransformGroup)]
  ITransformGroupStatics = interface(IInspectable)
  ['{25312F2A-CFAB-4B24-9713-5BDEAD1929C0}']
    function get_ChildrenProperty: IDependencyProperty; safecall;
    property ChildrenProperty: IDependencyProperty read get_ChildrenProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITranslateTransform
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TranslateTransform)]
  ITranslateTransform = interface(IInspectable)
  ['{C975905C-3C36-4229-817B-178F64C0E113}']
    function get_X: Double; safecall;
    procedure put_X(value: Double); safecall;
    function get_Y: Double; safecall;
    procedure put_Y(value: Double); safecall;
    property X: Double read get_X write put_X;
    property Y: Double read get_Y write put_Y;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.ITranslateTransformStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_TranslateTransform)]
  ITranslateTransformStatics = interface(IInspectable)
  ['{F419AA91-E042-4111-9C2F-D201304123DD}']
    function get_XProperty: IDependencyProperty; safecall;
    function get_YProperty: IDependencyProperty; safecall;
    property XProperty: IDependencyProperty read get_XProperty;
    property YProperty: IDependencyProperty read get_YProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IVisualTreeHelper
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_VisualTreeHelper)]
  IVisualTreeHelper = interface(IInspectable)
  ['{24B935E3-52C7-4141-8BAC-A73D06130569}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IVisualTreeHelperStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_VisualTreeHelper)]
  IVisualTreeHelperStatics = interface(IInspectable)
  ['{E75758C4-D25D-4B1D-971F-596F17F12BAA}']
    function FindElementsInHostCoordinates(intersectingPoint: TPointF; subtree: IUIElement): IIterable_1__IUIElement; overload; safecall;
    function FindElementsInHostCoordinates(intersectingRect: TRectF; subtree: IUIElement): IIterable_1__IUIElement; overload; safecall;
    function FindElementsInHostCoordinates(intersectingPoint: TPointF; subtree: IUIElement; includeAllElements: Boolean): IIterable_1__IUIElement; overload; safecall;
    function FindElementsInHostCoordinates(intersectingRect: TRectF; subtree: IUIElement; includeAllElements: Boolean): IIterable_1__IUIElement; overload; safecall;
    function GetChild(reference: IDependencyObject; childIndex: Integer): IDependencyObject; safecall;
    function GetChildrenCount(reference: IDependencyObject): Integer; safecall;
    function GetParent(reference: IDependencyObject): IDependencyObject; safecall;
    procedure DisconnectChildrenRecursive(element: IUIElement); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IVisualTreeHelperStatics2
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_VisualTreeHelper)]
  IVisualTreeHelperStatics2 = interface(IInspectable)
  ['{07BCD176-869F-44A7-8797-2103A4C3E47A}']
    function GetOpenPopups(window: IWindow): IVectorView_1__Primitives_IPopup; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IVisualTreeHelperStatics3
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_VisualTreeHelper)]
  IVisualTreeHelperStatics3 = interface(IInspectable)
  ['{40420D50-CA16-57DA-8AAC-944C8AF577FD}']
    function GetOpenPopupsForXamlRoot(xamlRoot: IXamlRoot): IVectorView_1__Primitives_IPopup; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlCompositionBrushBase
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_XamlCompositionBrushBase)]
  IXamlCompositionBrushBase = interface(IInspectable)
  ['{03E432D9-B35C-4A79-811C-C5652004DA0E}']
    function get_FallbackColor: Color; safecall;
    procedure put_FallbackColor(value: Color); safecall;
    property FallbackColor: Color read get_FallbackColor write put_FallbackColor;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_XamlCompositionBrushBase)]
  IXamlCompositionBrushBaseFactory = interface(IInspectable)
  ['{394F0823-2451-4ED8-BD24-488149B3428D}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IXamlCompositionBrushBase; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseOverrides
  IXamlCompositionBrushBaseOverrides = interface(IInspectable)
  ['{D19127F1-38B4-4EA1-8F33-849629A4C9C1}']
    procedure OnConnected; safecall;
    procedure OnDisconnected; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseProtected
  IXamlCompositionBrushBaseProtected = interface(IInspectable)
  ['{1513F3D8-0457-4E1C-AD77-11C1D9879743}']
    function get_CompositionBrush: ICompositionBrush; safecall;
    procedure put_CompositionBrush(value: ICompositionBrush); safecall;
    property CompositionBrush: ICompositionBrush read get_CompositionBrush write put_CompositionBrush;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlCompositionBrushBaseStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_XamlCompositionBrushBase)]
  IXamlCompositionBrushBaseStatics = interface(IInspectable)
  ['{4FD49B06-061A-441F-B97A-ADFBD41AE681}']
    function get_FallbackColorProperty: IDependencyProperty; safecall;
    property FallbackColorProperty: IDependencyProperty read get_FallbackColorProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlLightFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_XamlLight)]
  IXamlLightFactory = interface(IInspectable)
  ['{87DED768-3055-43B8-8EF6-798DC4C2329A}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IXamlLight; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlLightOverrides
  IXamlLightOverrides = interface(IInspectable)
  ['{7C6296C7-0173-48E1-B73D-7FA216A9AC28}']
    function GetId: HSTRING; safecall;
    procedure OnConnected(newElement: IUIElement); safecall;
    procedure OnDisconnected(oldElement: IUIElement); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlLightProtected
  IXamlLightProtected = interface(IInspectable)
  ['{5ECF220B-1252-43D0-9729-6EA692046838}']
    function get_CompositionLight: ICompositionLight; safecall;
    procedure put_CompositionLight(value: ICompositionLight); safecall;
    property CompositionLight: ICompositionLight read get_CompositionLight write put_CompositionLight;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.IXamlLightStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_XamlLight)]
  IXamlLightStatics = interface(IInspectable)
  ['{B5EA9D69-B508-4E9C-BD27-6B044B5F78A0}']
    procedure AddTargetElement(lightId: HSTRING; element: IUIElement); safecall;
    procedure RemoveTargetElement(lightId: HSTRING; element: IUIElement); safecall;
    procedure AddTargetBrush(lightId: HSTRING; brush: IBrush); safecall;
    procedure RemoveTargetBrush(lightId: HSTRING; brush: IBrush); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapImage2
  Imaging_IBitmapImage2 = interface(IInspectable)
  ['{1069C1B6-8C9B-4762-BE3D-759F5698F2B3}']
    function get_DecodePixelType: Imaging_DecodePixelType; safecall;
    procedure put_DecodePixelType(value: Imaging_DecodePixelType); safecall;
    property DecodePixelType: Imaging_DecodePixelType read get_DecodePixelType write put_DecodePixelType;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapImage3
  Imaging_IBitmapImage3 = interface(IInspectable)
  ['{F1DE6F26-3C73-453F-A7BA-9B85C18B3733}']
    function get_IsAnimatedBitmap: Boolean; safecall;
    function get_IsPlaying: Boolean; safecall;
    function get_AutoPlay: Boolean; safecall;
    procedure put_AutoPlay(value: Boolean); safecall;
    procedure Play; safecall;
    procedure Stop; safecall;
    property AutoPlay: Boolean read get_AutoPlay write put_AutoPlay;
    property IsAnimatedBitmap: Boolean read get_IsAnimatedBitmap;
    property IsPlaying: Boolean read get_IsPlaying;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapImageFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapImage)]
  Imaging_IBitmapImageFactory = interface(IInspectable)
  ['{C9132978-4810-4E5E-8087-03671EE60D85}']
    function CreateInstanceWithUriSource(uriSource: IUriRuntimeClass): Imaging_IBitmapImage; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapImage)]
  Imaging_IBitmapImageStatics = interface(IInspectable)
  ['{9E282143-70E8-437C-9FA4-2CBF295CFF84}']
    function get_CreateOptionsProperty: IDependencyProperty; safecall;
    function get_UriSourceProperty: IDependencyProperty; safecall;
    function get_DecodePixelWidthProperty: IDependencyProperty; safecall;
    function get_DecodePixelHeightProperty: IDependencyProperty; safecall;
    property CreateOptionsProperty: IDependencyProperty read get_CreateOptionsProperty;
    property DecodePixelHeightProperty: IDependencyProperty read get_DecodePixelHeightProperty;
    property DecodePixelWidthProperty: IDependencyProperty read get_DecodePixelWidthProperty;
    property UriSourceProperty: IDependencyProperty read get_UriSourceProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics2
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapImage)]
  Imaging_IBitmapImageStatics2 = interface(IInspectable)
  ['{C5F5576A-75AF-41A4-B893-8FE91FEE2882}']
    function get_DecodePixelTypeProperty: IDependencyProperty; safecall;
    property DecodePixelTypeProperty: IDependencyProperty read get_DecodePixelTypeProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics3
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapImage)]
  Imaging_IBitmapImageStatics3 = interface(IInspectable)
  ['{2B44E30D-F6D5-4411-A8CD-BF7603C4FAA0}']
    function get_IsAnimatedBitmapProperty: IDependencyProperty; safecall;
    function get_IsPlayingProperty: IDependencyProperty; safecall;
    function get_AutoPlayProperty: IDependencyProperty; safecall;
    property AutoPlayProperty: IDependencyProperty read get_AutoPlayProperty;
    property IsAnimatedBitmapProperty: IDependencyProperty read get_IsAnimatedBitmapProperty;
    property IsPlayingProperty: IDependencyProperty read get_IsPlayingProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapSource)]
  Imaging_IBitmapSource = interface(IInspectable)
  ['{23D86411-202F-41B2-8C5B-A8A3B333800B}']
    function get_PixelWidth: Integer; safecall;
    function get_PixelHeight: Integer; safecall;
    procedure SetSource(streamSource: IRandomAccessStream); safecall;
    function SetSourceAsync(streamSource: IRandomAccessStream): IAsyncAction; safecall;
    property PixelHeight: Integer read get_PixelHeight;
    property PixelWidth: Integer read get_PixelWidth;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapSourceFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapSource)]
  Imaging_IBitmapSourceFactory = interface(IInspectable)
  ['{E240420E-D4A7-49A4-A0B4-A59FDD77E508}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_IBitmapSource; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IBitmapSourceStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_BitmapSource)]
  Imaging_IBitmapSourceStatics = interface(IInspectable)
  ['{9A9C9981-827B-4E51-891B-8A15B511842D}']
    function get_PixelWidthProperty: IDependencyProperty; safecall;
    function get_PixelHeightProperty: IDependencyProperty; safecall;
    property PixelHeightProperty: IDependencyProperty read get_PixelHeightProperty;
    property PixelWidthProperty: IDependencyProperty read get_PixelWidthProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IRenderTargetBitmap
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_RenderTargetBitmap)]
  Imaging_IRenderTargetBitmap = interface(IInspectable)
  ['{500DEE81-893C-4C0A-8FEC-4678AC717589}']
    function get_PixelWidth: Integer; safecall;
    function get_PixelHeight: Integer; safecall;
    function RenderAsync(element: IUIElement): IAsyncAction; overload; safecall;
    function RenderAsync(element: IUIElement; scaledWidth: Integer; scaledHeight: Integer): IAsyncAction; overload; safecall;
    function GetPixelsAsync: IAsyncOperation_1__IBuffer; safecall;
    property PixelHeight: Integer read get_PixelHeight;
    property PixelWidth: Integer read get_PixelWidth;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IRenderTargetBitmapStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_RenderTargetBitmap)]
  Imaging_IRenderTargetBitmapStatics = interface(IInspectable)
  ['{F0A1EFEE-C131-4D40-9C47-F7D7CF2B077F}']
    function get_PixelWidthProperty: IDependencyProperty; safecall;
    function get_PixelHeightProperty: IDependencyProperty; safecall;
    property PixelHeightProperty: IDependencyProperty read get_PixelHeightProperty;
    property PixelWidthProperty: IDependencyProperty read get_PixelWidthProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISoftwareBitmapSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_SoftwareBitmapSource)]
  Imaging_ISoftwareBitmapSource = interface(IInspectable)
  ['{D2DD9ED0-D3C5-4056-91B5-B7C1D1E8130E}']
    function SetBitmapAsync(softwareBitmap: Imaging_ISoftwareBitmap): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISurfaceImageSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_SurfaceImageSource)]
  Imaging_ISurfaceImageSource = interface(IInspectable)
  ['{62F7D416-C714-4C4C-8273-F839BC58135C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISurfaceImageSourceFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_SurfaceImageSource)]
  Imaging_ISurfaceImageSourceFactory = interface(IInspectable)
  ['{3AB2212A-EF65-4A5F-BFAC-73993E8C12C9}']
    function CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISurfaceImageSource; safecall;
    function CreateInstanceWithDimensionsAndOpacity(pixelWidth: Integer; pixelHeight: Integer; isOpaque: Boolean; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISurfaceImageSource; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceOpenedEventArgs
  Imaging_ISvgImageSourceOpenedEventArgs = interface(IInspectable)
  ['{85EF4C16-748E-4008-95C7-6A23DD7316DB}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Imaging.ISvgImageSource,Windows.UI.Xaml.Media.Imaging.ISvgImageSourceOpenedEventArgs>
  TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceOpenedEventArgs_Delegate_Base = interface(IUnknown)
  ['{9E321151-8B27-5E18-9EA3-50520ACEB7EF}']
    procedure Invoke(sender: Imaging_ISvgImageSource; args: Imaging_ISvgImageSourceOpenedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Imaging.ISvgImageSource,Windows.UI.Xaml.Media.Imaging.ISvgImageSourceOpenedEventArgs>
  TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceOpenedEventArgs = interface(TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceOpenedEventArgs_Delegate_Base)
  ['{6D2E34D3-EF01-537B-BE8F-A74322ABF597}']
  end;

  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFailedEventArgs
  Imaging_ISvgImageSourceFailedEventArgs = interface(IInspectable)
  ['{68BB3170-3CCC-4035-AC01-9834543D744E}']
    function get_Status: Imaging_SvgImageSourceLoadStatus; safecall;
    property Status: Imaging_SvgImageSourceLoadStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Imaging.ISvgImageSource,Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFailedEventArgs>
  TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceFailedEventArgs_Delegate_Base = interface(IUnknown)
  ['{D723938D-EFEE-5A0C-AB3E-1E1DB3C9A216}']
    procedure Invoke(sender: Imaging_ISvgImageSource; args: Imaging_ISvgImageSourceFailedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Xaml.Media.Imaging.ISvgImageSource,Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFailedEventArgs>
  TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceFailedEventArgs = interface(TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceFailedEventArgs_Delegate_Base)
  ['{FE77B7BF-D6E9-59EC-A465-B20E5FBB9D38}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Xaml.Media.Imaging.SvgImageSourceLoadStatus>
  AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus_Delegate_Base = interface(IUnknown)
  ['{7C8BC668-4E0B-5924-B7E7-234A11D63D61}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Imaging_SvgImageSourceLoadStatus; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Xaml.Media.Imaging.SvgImageSourceLoadStatus>
  AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus = interface(AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Xaml.Media.Imaging.SvgImageSourceLoadStatus>
  IAsyncOperation_1__Imaging_SvgImageSourceLoadStatus_Base = interface(IInspectable)
  ['{F19DF5C2-2B78-53A9-8D38-5CA8DBB5DBC6}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus; safecall;
    function GetResults: Imaging_SvgImageSourceLoadStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Imaging_SvgImageSourceLoadStatus read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Xaml.Media.Imaging.SvgImageSourceLoadStatus>
  IAsyncOperation_1__Imaging_SvgImageSourceLoadStatus = interface(IAsyncOperation_1__Imaging_SvgImageSourceLoadStatus_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISvgImageSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_SvgImageSource)]
  Imaging_ISvgImageSource = interface(IInspectable)
  ['{03E1CEC3-0CA8-4A4E-8D7C-C808A0838586}']
    function get_UriSource: IUriRuntimeClass; safecall;
    procedure put_UriSource(value: IUriRuntimeClass); safecall;
    function get_RasterizePixelWidth: Double; safecall;
    procedure put_RasterizePixelWidth(value: Double); safecall;
    function get_RasterizePixelHeight: Double; safecall;
    procedure put_RasterizePixelHeight(value: Double); safecall;
    function add_Opened(handler: TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceOpenedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Opened(token: EventRegistrationToken); safecall;
    function add_OpenFailed(handler: TypedEventHandler_2__Imaging_ISvgImageSource__Imaging_ISvgImageSourceFailedEventArgs): EventRegistrationToken; safecall;
    procedure remove_OpenFailed(token: EventRegistrationToken); safecall;
    function SetSourceAsync(streamSource: IRandomAccessStream): IAsyncOperation_1__Imaging_SvgImageSourceLoadStatus; safecall;
    property RasterizePixelHeight: Double read get_RasterizePixelHeight write put_RasterizePixelHeight;
    property RasterizePixelWidth: Double read get_RasterizePixelWidth write put_RasterizePixelWidth;
    property UriSource: IUriRuntimeClass read get_UriSource write put_UriSource;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_SvgImageSource)]
  Imaging_ISvgImageSourceFactory = interface(IInspectable)
  ['{C794E9E7-CF23-4D72-BF1A-DFAA16D8EA52}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISvgImageSource; safecall;
    function CreateInstanceWithUriSource(uriSource: IUriRuntimeClass; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISvgImageSource; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.ISvgImageSourceStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_SvgImageSource)]
  Imaging_ISvgImageSourceStatics = interface(IInspectable)
  ['{9C6638CE-BED1-4AAB-ACBB-D3E2185D315A}']
    function get_UriSourceProperty: IDependencyProperty; safecall;
    function get_RasterizePixelWidthProperty: IDependencyProperty; safecall;
    function get_RasterizePixelHeightProperty: IDependencyProperty; safecall;
    property RasterizePixelHeightProperty: IDependencyProperty read get_RasterizePixelHeightProperty;
    property RasterizePixelWidthProperty: IDependencyProperty read get_RasterizePixelWidthProperty;
    property UriSourceProperty: IDependencyProperty read get_UriSourceProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IVirtualSurfaceImageSource
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_VirtualSurfaceImageSource)]
  Imaging_IVirtualSurfaceImageSource = interface(IInspectable)
  ['{4A711FEA-BFAC-11E0-A06A-9DE44724019B}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IVirtualSurfaceImageSourceFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_VirtualSurfaceImageSource)]
  Imaging_IVirtualSurfaceImageSourceFactory = interface(IInspectable)
  ['{3AB2212A-BFAC-11E0-8A92-69E44724019B}']
    function CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer): Imaging_IVirtualSurfaceImageSource; safecall;
    function CreateInstanceWithDimensionsAndOpacity(pixelWidth: Integer; pixelHeight: Integer; isOpaque: Boolean): Imaging_IVirtualSurfaceImageSource; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IWriteableBitmap
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_WriteableBitmap)]
  Imaging_IWriteableBitmap = interface(IInspectable)
  ['{BF0B7E6F-DF7C-4A85-8413-A1216285835C}']
    function get_PixelBuffer: IBuffer; safecall;
    procedure Invalidate; safecall;
    property PixelBuffer: IBuffer read get_PixelBuffer;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IWriteableBitmapFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_WriteableBitmap)]
  Imaging_IWriteableBitmapFactory = interface(IInspectable)
  ['{5563EBB1-3EF2-42C5-9C6D-1CF5DCC041FF}']
    function CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer): Imaging_IWriteableBitmap; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTask
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_XamlRenderingBackgroundTask)]
  Imaging_IXamlRenderingBackgroundTask = interface(IInspectable)
  ['{5D5FE9AA-533E-44B8-A975-FC5F1E3BFF52}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTaskFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Imaging_XamlRenderingBackgroundTask)]
  Imaging_IXamlRenderingBackgroundTaskFactory = interface(IInspectable)
  ['{A3D1BB63-38F8-4DA3-9FCA-FD8128A2CBF9}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_IXamlRenderingBackgroundTask; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTaskOverrides
  Imaging_IXamlRenderingBackgroundTaskOverrides = interface(IInspectable)
  ['{9C2A6997-A908-4711-B4B2-A960DB3D8E5A}']
    procedure OnRun(taskInstance: IBackgroundTaskInstance); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.ICompositeTransform3D
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_CompositeTransform3D)]
  Media3D_ICompositeTransform3D = interface(IInspectable)
  ['{8977CB01-AF8D-4AF5-B084-C08EB9704ABE}']
    function get_CenterX: Double; safecall;
    procedure put_CenterX(value: Double); safecall;
    function get_CenterY: Double; safecall;
    procedure put_CenterY(value: Double); safecall;
    function get_CenterZ: Double; safecall;
    procedure put_CenterZ(value: Double); safecall;
    function get_RotationX: Double; safecall;
    procedure put_RotationX(value: Double); safecall;
    function get_RotationY: Double; safecall;
    procedure put_RotationY(value: Double); safecall;
    function get_RotationZ: Double; safecall;
    procedure put_RotationZ(value: Double); safecall;
    function get_ScaleX: Double; safecall;
    procedure put_ScaleX(value: Double); safecall;
    function get_ScaleY: Double; safecall;
    procedure put_ScaleY(value: Double); safecall;
    function get_ScaleZ: Double; safecall;
    procedure put_ScaleZ(value: Double); safecall;
    function get_TranslateX: Double; safecall;
    procedure put_TranslateX(value: Double); safecall;
    function get_TranslateY: Double; safecall;
    procedure put_TranslateY(value: Double); safecall;
    function get_TranslateZ: Double; safecall;
    procedure put_TranslateZ(value: Double); safecall;
    property CenterX: Double read get_CenterX write put_CenterX;
    property CenterY: Double read get_CenterY write put_CenterY;
    property CenterZ: Double read get_CenterZ write put_CenterZ;
    property RotationX: Double read get_RotationX write put_RotationX;
    property RotationY: Double read get_RotationY write put_RotationY;
    property RotationZ: Double read get_RotationZ write put_RotationZ;
    property ScaleX: Double read get_ScaleX write put_ScaleX;
    property ScaleY: Double read get_ScaleY write put_ScaleY;
    property ScaleZ: Double read get_ScaleZ write put_ScaleZ;
    property TranslateX: Double read get_TranslateX write put_TranslateX;
    property TranslateY: Double read get_TranslateY write put_TranslateY;
    property TranslateZ: Double read get_TranslateZ write put_TranslateZ;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.ICompositeTransform3DStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_CompositeTransform3D)]
  Media3D_ICompositeTransform3DStatics = interface(IInspectable)
  ['{DDBF4D67-2A25-48F3-9808-C51EC3D55BEC}']
    function get_CenterXProperty: IDependencyProperty; safecall;
    function get_CenterYProperty: IDependencyProperty; safecall;
    function get_CenterZProperty: IDependencyProperty; safecall;
    function get_RotationXProperty: IDependencyProperty; safecall;
    function get_RotationYProperty: IDependencyProperty; safecall;
    function get_RotationZProperty: IDependencyProperty; safecall;
    function get_ScaleXProperty: IDependencyProperty; safecall;
    function get_ScaleYProperty: IDependencyProperty; safecall;
    function get_ScaleZProperty: IDependencyProperty; safecall;
    function get_TranslateXProperty: IDependencyProperty; safecall;
    function get_TranslateYProperty: IDependencyProperty; safecall;
    function get_TranslateZProperty: IDependencyProperty; safecall;
    property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    property CenterYProperty: IDependencyProperty read get_CenterYProperty;
    property CenterZProperty: IDependencyProperty read get_CenterZProperty;
    property RotationXProperty: IDependencyProperty read get_RotationXProperty;
    property RotationYProperty: IDependencyProperty read get_RotationYProperty;
    property RotationZProperty: IDependencyProperty read get_RotationZProperty;
    property ScaleXProperty: IDependencyProperty read get_ScaleXProperty;
    property ScaleYProperty: IDependencyProperty read get_ScaleYProperty;
    property ScaleZProperty: IDependencyProperty read get_ScaleZProperty;
    property TranslateXProperty: IDependencyProperty read get_TranslateXProperty;
    property TranslateYProperty: IDependencyProperty read get_TranslateYProperty;
    property TranslateZProperty: IDependencyProperty read get_TranslateZProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.IMatrix3DHelper
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_Matrix3DHelper)]
  Media3D_IMatrix3DHelper = interface(IInspectable)
  ['{E48C10EF-9927-4C9B-8213-07775512BA04}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.IMatrix3DHelperStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_Matrix3DHelper)]
  Media3D_IMatrix3DHelperStatics = interface(IInspectable)
  ['{9264545E-E158-4E74-8899-689160BD2F8C}']
    function get_Identity: Media3D_Matrix3D; safecall;
    function Multiply(matrix1: Media3D_Matrix3D; matrix2: Media3D_Matrix3D): Media3D_Matrix3D; safecall;
    function FromElements(m11: Double; m12: Double; m13: Double; m14: Double; m21: Double; m22: Double; m23: Double; m24: Double; m31: Double; m32: Double; m33: Double; m34: Double; offsetX: Double; offsetY: Double; offsetZ: Double; m44: Double): Media3D_Matrix3D; safecall;
    function GetHasInverse(target: Media3D_Matrix3D): Boolean; safecall;
    function GetIsIdentity(target: Media3D_Matrix3D): Boolean; safecall;
    function Invert(target: Media3D_Matrix3D): Media3D_Matrix3D; safecall;
    property Identity: Media3D_Matrix3D read get_Identity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.IPerspectiveTransform3D
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_PerspectiveTransform3D)]
  Media3D_IPerspectiveTransform3D = interface(IInspectable)
  ['{9A7B532A-30F9-40A1-96C9-C59D87F95AC3}']
    function get_Depth: Double; safecall;
    procedure put_Depth(value: Double); safecall;
    function get_OffsetX: Double; safecall;
    procedure put_OffsetX(value: Double); safecall;
    function get_OffsetY: Double; safecall;
    procedure put_OffsetY(value: Double); safecall;
    property Depth: Double read get_Depth write put_Depth;
    property OffsetX: Double read get_OffsetX write put_OffsetX;
    property OffsetY: Double read get_OffsetY write put_OffsetY;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.IPerspectiveTransform3DStatics
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_PerspectiveTransform3D)]
  Media3D_IPerspectiveTransform3DStatics = interface(IInspectable)
  ['{8E6F6400-620C-48C7-844D-3F0984DA5B17}']
    function get_DepthProperty: IDependencyProperty; safecall;
    function get_OffsetXProperty: IDependencyProperty; safecall;
    function get_OffsetYProperty: IDependencyProperty; safecall;
    property DepthProperty: IDependencyProperty read get_DepthProperty;
    property OffsetXProperty: IDependencyProperty read get_OffsetXProperty;
    property OffsetYProperty: IDependencyProperty read get_OffsetYProperty;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Xaml.Media.Media3D.ITransform3DFactory
  [WinRTClassNameAttribute(SWindows_UI_Xaml_Media_Media3D_Transform3D)]
  Media3D_ITransform3DFactory = interface(IInspectable)
  ['{052C1F7A-8D73-48CD-BBB8-D00434CAAE5D}']
    function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Media3D_ITransform3D; safecall;
  end;

  // Windows.UI.Xaml.Media.Brush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IBrush
  // Implements: Windows.UI.Xaml.Media.IBrushOverrides2
  // Implements: Windows.UI.Composition.IAnimationObject
  // Statics: "Windows.UI.Xaml.Media.IBrushStatics"
  // Factory: "Windows.UI.Xaml.Media.IBrushFactory"
  // Instantiable: "IBrush"
  TBrush = class(TWinRTGenericImportFSI<IBrushFactory, IBrushStatics, IBrush>)
  public
    // -> IBrushStatics
    class function get_OpacityProperty: IDependencyProperty; static; inline;
    class function get_TransformProperty: IDependencyProperty; static; inline;
    class function get_RelativeTransformProperty: IDependencyProperty; static; inline;
    class property OpacityProperty: IDependencyProperty read get_OpacityProperty;
    class property RelativeTransformProperty: IDependencyProperty read get_RelativeTransformProperty;
    class property TransformProperty: IDependencyProperty read get_TransformProperty;

    // -> IBrushFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.TileBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ITileBrush
  // Statics: "Windows.UI.Xaml.Media.ITileBrushStatics"
  // Factory: "Windows.UI.Xaml.Media.ITileBrushFactory"
  // Instantiable: "ITileBrush"
  TTileBrush = class(TWinRTGenericImportFSI<ITileBrushFactory, ITileBrushStatics, ITileBrush>)
  public
    // -> ITileBrushStatics
    class function get_AlignmentXProperty: IDependencyProperty; static; inline;
    class function get_AlignmentYProperty: IDependencyProperty; static; inline;
    class function get_StretchProperty: IDependencyProperty; static; inline;
    class property AlignmentXProperty: IDependencyProperty read get_AlignmentXProperty;
    class property AlignmentYProperty: IDependencyProperty read get_AlignmentYProperty;
    class property StretchProperty: IDependencyProperty read get_StretchProperty;

    // -> ITileBrushFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): ITileBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.XamlCompositionBrushBase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IXamlCompositionBrushBase
  // Implements: Windows.UI.Xaml.Media.IXamlCompositionBrushBaseProtected
  // Implements: Windows.UI.Xaml.Media.IXamlCompositionBrushBaseOverrides
  // Statics: "Windows.UI.Xaml.Media.IXamlCompositionBrushBaseStatics"
  // Factory: "Windows.UI.Xaml.Media.IXamlCompositionBrushBaseFactory"
  // Instantiable: "IXamlCompositionBrushBase"
  TXamlCompositionBrushBase = class(TWinRTGenericImportFSI<IXamlCompositionBrushBaseFactory, IXamlCompositionBrushBaseStatics, IXamlCompositionBrushBase>)
  public
    // -> IXamlCompositionBrushBaseStatics
    class function get_FallbackColorProperty: IDependencyProperty; static; inline;
    class property FallbackColorProperty: IDependencyProperty read get_FallbackColorProperty;

    // -> IXamlCompositionBrushBaseFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IXamlCompositionBrushBase; static; inline;
  end;

  // Windows.UI.Xaml.Media.AcrylicBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IAcrylicBrush
  // Implements: Windows.UI.Xaml.Media.IAcrylicBrush2
  // Statics: "Windows.UI.Xaml.Media.IAcrylicBrushStatics"
  // Statics: "Windows.UI.Xaml.Media.IAcrylicBrushStatics2"
  // Factory: "Windows.UI.Xaml.Media.IAcrylicBrushFactory"
  // Instantiable: "IAcrylicBrush"
  TAcrylicBrush = class(TWinRTGenericImportFS2I<IAcrylicBrushFactory, IAcrylicBrushStatics, IAcrylicBrushStatics2, IAcrylicBrush>)
  public
    // -> IAcrylicBrushStatics
    class function get_BackgroundSourceProperty: IDependencyProperty; static; inline;
    class function get_TintColorProperty: IDependencyProperty; static; inline;
    class function get_TintOpacityProperty: IDependencyProperty; static; inline;
    class function get_TintTransitionDurationProperty: IDependencyProperty; static; inline;
    class function get_AlwaysUseFallbackProperty: IDependencyProperty; static; inline;
    class property AlwaysUseFallbackProperty: IDependencyProperty read get_AlwaysUseFallbackProperty;
    class property BackgroundSourceProperty: IDependencyProperty read get_BackgroundSourceProperty;
    class property TintColorProperty: IDependencyProperty read get_TintColorProperty;
    class property TintOpacityProperty: IDependencyProperty read get_TintOpacityProperty;
    class property TintTransitionDurationProperty: IDependencyProperty read get_TintTransitionDurationProperty;

    // -> IAcrylicBrushStatics2
    class function get_TintLuminosityOpacityProperty: IDependencyProperty; static; inline;
    class property TintLuminosityOpacityProperty: IDependencyProperty read get_TintLuminosityOpacityProperty;

    // -> IAcrylicBrushFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IAcrylicBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.Transition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ITransition
  // Factory: "Windows.UI.Xaml.Media.Animation.ITransitionFactory"
  // Instantiable: "Animation_ITransition"
  TAnimation_Transition = class(TWinRTGenericImportFI<Animation_ITransitionFactory, Animation_ITransition>)
  public
    // -> Animation_ITransitionFactory
  end;

  // Windows.UI.Xaml.Media.Animation.AddDeleteThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IAddDeleteThemeTransition
  // Instantiable: "Animation_IAddDeleteThemeTransition"
  TAnimation_AddDeleteThemeTransition = class(TWinRTGenericImportI<Animation_IAddDeleteThemeTransition>) end;

  // Windows.UI.Xaml.Media.Animation.EasingFunctionBase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IEasingFunctionBase
  // Statics: "Windows.UI.Xaml.Media.Animation.IEasingFunctionBaseStatics"
  // Factory: "Windows.UI.Xaml.Media.Animation.IEasingFunctionBaseFactory"
  // Instantiable: "Animation_IEasingFunctionBase"
  TAnimation_EasingFunctionBase = class(TWinRTGenericImportFSI<Animation_IEasingFunctionBaseFactory, Animation_IEasingFunctionBaseStatics, Animation_IEasingFunctionBase>)
  public
    // -> Animation_IEasingFunctionBaseStatics
    class function get_EasingModeProperty: IDependencyProperty; static; inline;
    class property EasingModeProperty: IDependencyProperty read get_EasingModeProperty;

    // -> Animation_IEasingFunctionBaseFactory
  end;

  // Windows.UI.Xaml.Media.Animation.BackEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IBackEase
  // Statics: "Windows.UI.Xaml.Media.Animation.IBackEaseStatics"
  // Instantiable: "Animation_IBackEase"
  TAnimation_BackEase = class(TWinRTGenericImportSI<Animation_IBackEaseStatics, Animation_IBackEase>)
  public
    // -> Animation_IBackEaseStatics
    class function get_AmplitudeProperty: IDependencyProperty; static; inline;
    class property AmplitudeProperty: IDependencyProperty read get_AmplitudeProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ConnectedAnimationConfiguration
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IConnectedAnimationConfiguration
  // Factory: "Windows.UI.Xaml.Media.Animation.IConnectedAnimationConfigurationFactory"
  // Instantiable: "Animation_IConnectedAnimationConfiguration"
  TAnimation_ConnectedAnimationConfiguration = class(TWinRTGenericImportFI<Animation_IConnectedAnimationConfigurationFactory, Animation_IConnectedAnimationConfiguration>)
  public
    // -> Animation_IConnectedAnimationConfigurationFactory
  end;

  // Windows.UI.Xaml.Media.Animation.BasicConnectedAnimationConfiguration
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IBasicConnectedAnimationConfiguration
  // Factory: "Windows.UI.Xaml.Media.Animation.IBasicConnectedAnimationConfigurationFactory"
  // Instantiable: "Animation_IBasicConnectedAnimationConfiguration"
  TAnimation_BasicConnectedAnimationConfiguration = class(TWinRTGenericImportFI<Animation_IBasicConnectedAnimationConfigurationFactory, Animation_IBasicConnectedAnimationConfiguration>)
  public
    // -> Animation_IBasicConnectedAnimationConfigurationFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IBasicConnectedAnimationConfiguration; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.BeginStoryboard
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IBeginStoryboard
  // Statics: "Windows.UI.Xaml.Media.Animation.IBeginStoryboardStatics"
  // Instantiable: "Animation_IBeginStoryboard"
  TAnimation_BeginStoryboard = class(TWinRTGenericImportSI<Animation_IBeginStoryboardStatics, Animation_IBeginStoryboard>)
  public
    // -> Animation_IBeginStoryboardStatics
    class function get_StoryboardProperty: IDependencyProperty; static; inline;
    class property StoryboardProperty: IDependencyProperty read get_StoryboardProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.BounceEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IBounceEase
  // Statics: "Windows.UI.Xaml.Media.Animation.IBounceEaseStatics"
  // Instantiable: "Animation_IBounceEase"
  TAnimation_BounceEase = class(TWinRTGenericImportSI<Animation_IBounceEaseStatics, Animation_IBounceEase>)
  public
    // -> Animation_IBounceEaseStatics
    class function get_BouncesProperty: IDependencyProperty; static; inline;
    class function get_BouncinessProperty: IDependencyProperty; static; inline;
    class property BouncesProperty: IDependencyProperty read get_BouncesProperty;
    class property BouncinessProperty: IDependencyProperty read get_BouncinessProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.CircleEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ICircleEase
  // Instantiable: "Animation_ICircleEase"
  TAnimation_CircleEase = class(TWinRTGenericImportI<Animation_ICircleEase>) end;

  // Windows.UI.Xaml.Media.Animation.Timeline
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ITimeline
  // Statics: "Windows.UI.Xaml.Media.Animation.ITimelineStatics"
  // Factory: "Windows.UI.Xaml.Media.Animation.ITimelineFactory"
  // Instantiable: "Animation_ITimeline"
  TAnimation_Timeline = class(TWinRTGenericImportFSI<Animation_ITimelineFactory, Animation_ITimelineStatics, Animation_ITimeline>)
  public
    // -> Animation_ITimelineStatics
    class function get_AllowDependentAnimations: Boolean; static; inline;
    class procedure put_AllowDependentAnimations(value: Boolean); static; inline;
    class function get_AutoReverseProperty: IDependencyProperty; static; inline;
    class function get_BeginTimeProperty: IDependencyProperty; static; inline;
    class function get_DurationProperty: IDependencyProperty; static; inline;
    class function get_SpeedRatioProperty: IDependencyProperty; static; inline;
    class function get_FillBehaviorProperty: IDependencyProperty; static; inline;
    class function get_RepeatBehaviorProperty: IDependencyProperty; static; inline;
    class property AllowDependentAnimations: Boolean read get_AllowDependentAnimations write put_AllowDependentAnimations;
    class property AutoReverseProperty: IDependencyProperty read get_AutoReverseProperty;
    class property BeginTimeProperty: IDependencyProperty read get_BeginTimeProperty;
    class property DurationProperty: IDependencyProperty read get_DurationProperty;
    class property FillBehaviorProperty: IDependencyProperty read get_FillBehaviorProperty;
    class property RepeatBehaviorProperty: IDependencyProperty read get_RepeatBehaviorProperty;
    class property SpeedRatioProperty: IDependencyProperty read get_SpeedRatioProperty;

    // -> Animation_ITimelineFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_ITimeline; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.ColorAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IColorAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IColorAnimationStatics"
  // Instantiable: "Animation_IColorAnimation"
  TAnimation_ColorAnimation = class(TWinRTGenericImportSI<Animation_IColorAnimationStatics, Animation_IColorAnimation>)
  public
    // -> Animation_IColorAnimationStatics
    class function get_FromProperty: IDependencyProperty; static; inline;
    class function get_ToProperty: IDependencyProperty; static; inline;
    class function get_ByProperty: IDependencyProperty; static; inline;
    class function get_EasingFunctionProperty: IDependencyProperty; static; inline;
    class function get_EnableDependentAnimationProperty: IDependencyProperty; static; inline;
    class property ByProperty: IDependencyProperty read get_ByProperty;
    class property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
    class property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
    class property FromProperty: IDependencyProperty read get_FromProperty;
    class property ToProperty: IDependencyProperty read get_ToProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ColorAnimationUsingKeyFrames
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IColorAnimationUsingKeyFrames
  // Statics: "Windows.UI.Xaml.Media.Animation.IColorAnimationUsingKeyFramesStatics"
  // Instantiable: "Animation_IColorAnimationUsingKeyFrames"
  TAnimation_ColorAnimationUsingKeyFrames = class(TWinRTGenericImportSI<Animation_IColorAnimationUsingKeyFramesStatics, Animation_IColorAnimationUsingKeyFrames>)
  public
    // -> Animation_IColorAnimationUsingKeyFramesStatics
    class function get_EnableDependentAnimationProperty: IDependencyProperty; static; inline;
    class property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ColorKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IColorKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.IColorKeyFrameStatics"
  // Factory: "Windows.UI.Xaml.Media.Animation.IColorKeyFrameFactory"
  // Instantiable: "Animation_IColorKeyFrame"
  TAnimation_ColorKeyFrame = class(TWinRTGenericImportFSI<Animation_IColorKeyFrameFactory, Animation_IColorKeyFrameStatics, Animation_IColorKeyFrame>)
  public
    // -> Animation_IColorKeyFrameStatics
    class function get_ValueProperty: IDependencyProperty; static; inline;
    class function get_KeyTimeProperty: IDependencyProperty; static; inline;
    class property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    class property ValueProperty: IDependencyProperty read get_ValueProperty;

    // -> Animation_IColorKeyFrameFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IColorKeyFrame; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.ColorKeyFrameCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IColorKeyFrame>
  // Instantiable: "IVector_1__Animation_IColorKeyFrame"
  TAnimation_ColorKeyFrameCollection = class(TWinRTGenericImportI<IVector_1__Animation_IColorKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.NavigationTransitionInfo
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.INavigationTransitionInfo
  // Implements: Windows.UI.Xaml.Media.Animation.INavigationTransitionInfoOverrides
  // Factory: "Windows.UI.Xaml.Media.Animation.INavigationTransitionInfoFactory"
  // Instantiable: "Animation_INavigationTransitionInfo"
  TAnimation_NavigationTransitionInfo = class(TWinRTGenericImportFI<Animation_INavigationTransitionInfoFactory, Animation_INavigationTransitionInfo>)
  public
    // -> Animation_INavigationTransitionInfoFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_INavigationTransitionInfo; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.CommonNavigationTransitionInfo
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ICommonNavigationTransitionInfo
  // Statics: "Windows.UI.Xaml.Media.Animation.ICommonNavigationTransitionInfoStatics"
  // Instantiable: "Animation_ICommonNavigationTransitionInfo"
  TAnimation_CommonNavigationTransitionInfo = class(TWinRTGenericImportSI<Animation_ICommonNavigationTransitionInfoStatics, Animation_ICommonNavigationTransitionInfo>)
  public
    // -> Animation_ICommonNavigationTransitionInfoStatics
    class function get_IsStaggeringEnabledProperty: IDependencyProperty; static; inline;
    class function get_IsStaggerElementProperty: IDependencyProperty; static; inline;
    class function GetIsStaggerElement(element: IUIElement): Boolean; static; inline;
    class procedure SetIsStaggerElement(element: IUIElement; value: Boolean); static; inline;
    class property IsStaggerElementProperty: IDependencyProperty read get_IsStaggerElementProperty;
    class property IsStaggeringEnabledProperty: IDependencyProperty read get_IsStaggeringEnabledProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ConnectedAnimationService
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IConnectedAnimationService
  // Statics: "Windows.UI.Xaml.Media.Animation.IConnectedAnimationServiceStatics"
  TAnimation_ConnectedAnimationService = class(TWinRTGenericImportS<Animation_IConnectedAnimationServiceStatics>)
  public
    // -> Animation_IConnectedAnimationServiceStatics
    class function GetForCurrentView: Animation_IConnectedAnimationService; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.ContentThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IContentThemeTransition
  // Statics: "Windows.UI.Xaml.Media.Animation.IContentThemeTransitionStatics"
  // Instantiable: "Animation_IContentThemeTransition"
  TAnimation_ContentThemeTransition = class(TWinRTGenericImportSI<Animation_IContentThemeTransitionStatics, Animation_IContentThemeTransition>)
  public
    // -> Animation_IContentThemeTransitionStatics
    class function get_HorizontalOffsetProperty: IDependencyProperty; static; inline;
    class function get_VerticalOffsetProperty: IDependencyProperty; static; inline;
    class property HorizontalOffsetProperty: IDependencyProperty read get_HorizontalOffsetProperty;
    class property VerticalOffsetProperty: IDependencyProperty read get_VerticalOffsetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ContinuumNavigationTransitionInfo
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IContinuumNavigationTransitionInfo
  // Statics: "Windows.UI.Xaml.Media.Animation.IContinuumNavigationTransitionInfoStatics"
  // Instantiable: "Animation_IContinuumNavigationTransitionInfo"
  TAnimation_ContinuumNavigationTransitionInfo = class(TWinRTGenericImportSI<Animation_IContinuumNavigationTransitionInfoStatics, Animation_IContinuumNavigationTransitionInfo>)
  public
    // -> Animation_IContinuumNavigationTransitionInfoStatics
    class function get_ExitElementProperty: IDependencyProperty; static; inline;
    class function get_IsEntranceElementProperty: IDependencyProperty; static; inline;
    class function GetIsEntranceElement(element: IUIElement): Boolean; static; inline;
    class procedure SetIsEntranceElement(element: IUIElement; value: Boolean); static; inline;
    class function get_IsExitElementProperty: IDependencyProperty; static; inline;
    class function GetIsExitElement(element: IUIElement): Boolean; static; inline;
    class procedure SetIsExitElement(element: IUIElement; value: Boolean); static; inline;
    class function get_ExitElementContainerProperty: IDependencyProperty; static; inline;
    class function GetExitElementContainer(element: IListViewBase): Boolean; static; inline;
    class procedure SetExitElementContainer(element: IListViewBase; value: Boolean); static; inline;
    class property ExitElementContainerProperty: IDependencyProperty read get_ExitElementContainerProperty;
    class property ExitElementProperty: IDependencyProperty read get_ExitElementProperty;
    class property IsEntranceElementProperty: IDependencyProperty read get_IsEntranceElementProperty;
    class property IsExitElementProperty: IDependencyProperty read get_IsExitElementProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.CubicEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ICubicEase
  // Instantiable: "Animation_ICubicEase"
  TAnimation_CubicEase = class(TWinRTGenericImportI<Animation_ICubicEase>) end;

  // Windows.UI.Xaml.Media.Animation.DirectConnectedAnimationConfiguration
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDirectConnectedAnimationConfiguration
  // Factory: "Windows.UI.Xaml.Media.Animation.IDirectConnectedAnimationConfigurationFactory"
  // Instantiable: "Animation_IDirectConnectedAnimationConfiguration"
  TAnimation_DirectConnectedAnimationConfiguration = class(TWinRTGenericImportFI<Animation_IDirectConnectedAnimationConfigurationFactory, Animation_IDirectConnectedAnimationConfiguration>)
  public
    // -> Animation_IDirectConnectedAnimationConfigurationFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IDirectConnectedAnimationConfiguration; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.DiscreteColorKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDiscreteColorKeyFrame
  // Instantiable: "Animation_IDiscreteColorKeyFrame"
  TAnimation_DiscreteColorKeyFrame = class(TWinRTGenericImportI<Animation_IDiscreteColorKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.DoubleKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.IDoubleKeyFrameStatics"
  // Factory: "Windows.UI.Xaml.Media.Animation.IDoubleKeyFrameFactory"
  // Instantiable: "Animation_IDoubleKeyFrame"
  TAnimation_DoubleKeyFrame = class(TWinRTGenericImportFSI<Animation_IDoubleKeyFrameFactory, Animation_IDoubleKeyFrameStatics, Animation_IDoubleKeyFrame>)
  public
    // -> Animation_IDoubleKeyFrameStatics
    class function get_ValueProperty: IDependencyProperty; static; inline;
    class function get_KeyTimeProperty: IDependencyProperty; static; inline;
    class property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    class property ValueProperty: IDependencyProperty read get_ValueProperty;

    // -> Animation_IDoubleKeyFrameFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IDoubleKeyFrame; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.DiscreteDoubleKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDiscreteDoubleKeyFrame
  // Instantiable: "Animation_IDiscreteDoubleKeyFrame"
  TAnimation_DiscreteDoubleKeyFrame = class(TWinRTGenericImportI<Animation_IDiscreteDoubleKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.ObjectKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IObjectKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.IObjectKeyFrameStatics"
  // Factory: "Windows.UI.Xaml.Media.Animation.IObjectKeyFrameFactory"
  // Instantiable: "Animation_IObjectKeyFrame"
  TAnimation_ObjectKeyFrame = class(TWinRTGenericImportFSI<Animation_IObjectKeyFrameFactory, Animation_IObjectKeyFrameStatics, Animation_IObjectKeyFrame>)
  public
    // -> Animation_IObjectKeyFrameStatics
    class function get_ValueProperty: IDependencyProperty; static; inline;
    class function get_KeyTimeProperty: IDependencyProperty; static; inline;
    class property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    class property ValueProperty: IDependencyProperty read get_ValueProperty;

    // -> Animation_IObjectKeyFrameFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IObjectKeyFrame; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.DiscreteObjectKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDiscreteObjectKeyFrame
  // Instantiable: "Animation_IDiscreteObjectKeyFrame"
  TAnimation_DiscreteObjectKeyFrame = class(TWinRTGenericImportI<Animation_IDiscreteObjectKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.PointKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPointKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.IPointKeyFrameStatics"
  // Factory: "Windows.UI.Xaml.Media.Animation.IPointKeyFrameFactory"
  // Instantiable: "Animation_IPointKeyFrame"
  TAnimation_PointKeyFrame = class(TWinRTGenericImportFSI<Animation_IPointKeyFrameFactory, Animation_IPointKeyFrameStatics, Animation_IPointKeyFrame>)
  public
    // -> Animation_IPointKeyFrameStatics
    class function get_ValueProperty: IDependencyProperty; static; inline;
    class function get_KeyTimeProperty: IDependencyProperty; static; inline;
    class property KeyTimeProperty: IDependencyProperty read get_KeyTimeProperty;
    class property ValueProperty: IDependencyProperty read get_ValueProperty;

    // -> Animation_IPointKeyFrameFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IPointKeyFrame; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.DiscretePointKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDiscretePointKeyFrame
  // Instantiable: "Animation_IDiscretePointKeyFrame"
  TAnimation_DiscretePointKeyFrame = class(TWinRTGenericImportI<Animation_IDiscretePointKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.DoubleAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDoubleAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IDoubleAnimationStatics"
  // Instantiable: "Animation_IDoubleAnimation"
  TAnimation_DoubleAnimation = class(TWinRTGenericImportSI<Animation_IDoubleAnimationStatics, Animation_IDoubleAnimation>)
  public
    // -> Animation_IDoubleAnimationStatics
    class function get_FromProperty: IDependencyProperty; static; inline;
    class function get_ToProperty: IDependencyProperty; static; inline;
    class function get_ByProperty: IDependencyProperty; static; inline;
    class function get_EasingFunctionProperty: IDependencyProperty; static; inline;
    class function get_EnableDependentAnimationProperty: IDependencyProperty; static; inline;
    class property ByProperty: IDependencyProperty read get_ByProperty;
    class property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
    class property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
    class property FromProperty: IDependencyProperty read get_FromProperty;
    class property ToProperty: IDependencyProperty read get_ToProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.DoubleAnimationUsingKeyFrames
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDoubleAnimationUsingKeyFrames
  // Statics: "Windows.UI.Xaml.Media.Animation.IDoubleAnimationUsingKeyFramesStatics"
  // Instantiable: "Animation_IDoubleAnimationUsingKeyFrames"
  TAnimation_DoubleAnimationUsingKeyFrames = class(TWinRTGenericImportSI<Animation_IDoubleAnimationUsingKeyFramesStatics, Animation_IDoubleAnimationUsingKeyFrames>)
  public
    // -> Animation_IDoubleAnimationUsingKeyFramesStatics
    class function get_EnableDependentAnimationProperty: IDependencyProperty; static; inline;
    class property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.DoubleKeyFrameCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IDoubleKeyFrame>
  // Instantiable: "IVector_1__Animation_IDoubleKeyFrame"
  TAnimation_DoubleKeyFrameCollection = class(TWinRTGenericImportI<IVector_1__Animation_IDoubleKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.DragItemThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDragItemThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IDragItemThemeAnimationStatics"
  // Instantiable: "Animation_IDragItemThemeAnimation"
  TAnimation_DragItemThemeAnimation = class(TWinRTGenericImportSI<Animation_IDragItemThemeAnimationStatics, Animation_IDragItemThemeAnimation>)
  public
    // -> Animation_IDragItemThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.DragOverThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDragOverThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IDragOverThemeAnimationStatics"
  // Instantiable: "Animation_IDragOverThemeAnimation"
  TAnimation_DragOverThemeAnimation = class(TWinRTGenericImportSI<Animation_IDragOverThemeAnimationStatics, Animation_IDragOverThemeAnimation>)
  public
    // -> Animation_IDragOverThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class function get_ToOffsetProperty: IDependencyProperty; static; inline;
    class function get_DirectionProperty: IDependencyProperty; static; inline;
    class property DirectionProperty: IDependencyProperty read get_DirectionProperty;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
    class property ToOffsetProperty: IDependencyProperty read get_ToOffsetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.DrillInNavigationTransitionInfo
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDrillInNavigationTransitionInfo
  // Instantiable: "Animation_IDrillInNavigationTransitionInfo"
  TAnimation_DrillInNavigationTransitionInfo = class(TWinRTGenericImportI<Animation_IDrillInNavigationTransitionInfo>) end;

  // Windows.UI.Xaml.Media.Animation.DrillInThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDrillInThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IDrillInThemeAnimationStatics"
  // Instantiable: "Animation_IDrillInThemeAnimation"
  TAnimation_DrillInThemeAnimation = class(TWinRTGenericImportSI<Animation_IDrillInThemeAnimationStatics, Animation_IDrillInThemeAnimation>)
  public
    // -> Animation_IDrillInThemeAnimationStatics
    class function get_EntranceTargetNameProperty: IDependencyProperty; static; inline;
    class function get_EntranceTargetProperty: IDependencyProperty; static; inline;
    class function get_ExitTargetNameProperty: IDependencyProperty; static; inline;
    class function get_ExitTargetProperty: IDependencyProperty; static; inline;
    class property EntranceTargetNameProperty: IDependencyProperty read get_EntranceTargetNameProperty;
    class property EntranceTargetProperty: IDependencyProperty read get_EntranceTargetProperty;
    class property ExitTargetNameProperty: IDependencyProperty read get_ExitTargetNameProperty;
    class property ExitTargetProperty: IDependencyProperty read get_ExitTargetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.DrillOutThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDrillOutThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IDrillOutThemeAnimationStatics"
  // Instantiable: "Animation_IDrillOutThemeAnimation"
  TAnimation_DrillOutThemeAnimation = class(TWinRTGenericImportSI<Animation_IDrillOutThemeAnimationStatics, Animation_IDrillOutThemeAnimation>)
  public
    // -> Animation_IDrillOutThemeAnimationStatics
    class function get_EntranceTargetNameProperty: IDependencyProperty; static; inline;
    class function get_EntranceTargetProperty: IDependencyProperty; static; inline;
    class function get_ExitTargetNameProperty: IDependencyProperty; static; inline;
    class function get_ExitTargetProperty: IDependencyProperty; static; inline;
    class property EntranceTargetNameProperty: IDependencyProperty read get_EntranceTargetNameProperty;
    class property EntranceTargetProperty: IDependencyProperty read get_EntranceTargetProperty;
    class property ExitTargetNameProperty: IDependencyProperty read get_ExitTargetNameProperty;
    class property ExitTargetProperty: IDependencyProperty read get_ExitTargetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.DropTargetItemThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IDropTargetItemThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IDropTargetItemThemeAnimationStatics"
  // Instantiable: "Animation_IDropTargetItemThemeAnimation"
  TAnimation_DropTargetItemThemeAnimation = class(TWinRTGenericImportSI<Animation_IDropTargetItemThemeAnimationStatics, Animation_IDropTargetItemThemeAnimation>)
  public
    // -> Animation_IDropTargetItemThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.EasingColorKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IEasingColorKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.IEasingColorKeyFrameStatics"
  // Instantiable: "Animation_IEasingColorKeyFrame"
  TAnimation_EasingColorKeyFrame = class(TWinRTGenericImportSI<Animation_IEasingColorKeyFrameStatics, Animation_IEasingColorKeyFrame>)
  public
    // -> Animation_IEasingColorKeyFrameStatics
    class function get_EasingFunctionProperty: IDependencyProperty; static; inline;
    class property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.EasingDoubleKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IEasingDoubleKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.IEasingDoubleKeyFrameStatics"
  // Instantiable: "Animation_IEasingDoubleKeyFrame"
  TAnimation_EasingDoubleKeyFrame = class(TWinRTGenericImportSI<Animation_IEasingDoubleKeyFrameStatics, Animation_IEasingDoubleKeyFrame>)
  public
    // -> Animation_IEasingDoubleKeyFrameStatics
    class function get_EasingFunctionProperty: IDependencyProperty; static; inline;
    class property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.EasingPointKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IEasingPointKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.IEasingPointKeyFrameStatics"
  // Instantiable: "Animation_IEasingPointKeyFrame"
  TAnimation_EasingPointKeyFrame = class(TWinRTGenericImportSI<Animation_IEasingPointKeyFrameStatics, Animation_IEasingPointKeyFrame>)
  public
    // -> Animation_IEasingPointKeyFrameStatics
    class function get_EasingFunctionProperty: IDependencyProperty; static; inline;
    class property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.EdgeUIThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IEdgeUIThemeTransition
  // Statics: "Windows.UI.Xaml.Media.Animation.IEdgeUIThemeTransitionStatics"
  // Instantiable: "Animation_IEdgeUIThemeTransition"
  TAnimation_EdgeUIThemeTransition = class(TWinRTGenericImportSI<Animation_IEdgeUIThemeTransitionStatics, Animation_IEdgeUIThemeTransition>)
  public
    // -> Animation_IEdgeUIThemeTransitionStatics
    class function get_EdgeProperty: IDependencyProperty; static; inline;
    class property EdgeProperty: IDependencyProperty read get_EdgeProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ElasticEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IElasticEase
  // Statics: "Windows.UI.Xaml.Media.Animation.IElasticEaseStatics"
  // Instantiable: "Animation_IElasticEase"
  TAnimation_ElasticEase = class(TWinRTGenericImportSI<Animation_IElasticEaseStatics, Animation_IElasticEase>)
  public
    // -> Animation_IElasticEaseStatics
    class function get_OscillationsProperty: IDependencyProperty; static; inline;
    class function get_SpringinessProperty: IDependencyProperty; static; inline;
    class property OscillationsProperty: IDependencyProperty read get_OscillationsProperty;
    class property SpringinessProperty: IDependencyProperty read get_SpringinessProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.EntranceNavigationTransitionInfo
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IEntranceNavigationTransitionInfo
  // Statics: "Windows.UI.Xaml.Media.Animation.IEntranceNavigationTransitionInfoStatics"
  // Instantiable: "Animation_IEntranceNavigationTransitionInfo"
  TAnimation_EntranceNavigationTransitionInfo = class(TWinRTGenericImportSI<Animation_IEntranceNavigationTransitionInfoStatics, Animation_IEntranceNavigationTransitionInfo>)
  public
    // -> Animation_IEntranceNavigationTransitionInfoStatics
    class function get_IsTargetElementProperty: IDependencyProperty; static; inline;
    class function GetIsTargetElement(element: IUIElement): Boolean; static; inline;
    class procedure SetIsTargetElement(element: IUIElement; value: Boolean); static; inline;
    class property IsTargetElementProperty: IDependencyProperty read get_IsTargetElementProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.EntranceThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IEntranceThemeTransition
  // Statics: "Windows.UI.Xaml.Media.Animation.IEntranceThemeTransitionStatics"
  // Instantiable: "Animation_IEntranceThemeTransition"
  TAnimation_EntranceThemeTransition = class(TWinRTGenericImportSI<Animation_IEntranceThemeTransitionStatics, Animation_IEntranceThemeTransition>)
  public
    // -> Animation_IEntranceThemeTransitionStatics
    class function get_FromHorizontalOffsetProperty: IDependencyProperty; static; inline;
    class function get_FromVerticalOffsetProperty: IDependencyProperty; static; inline;
    class function get_IsStaggeringEnabledProperty: IDependencyProperty; static; inline;
    class property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    class property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    class property IsStaggeringEnabledProperty: IDependencyProperty read get_IsStaggeringEnabledProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ExponentialEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IExponentialEase
  // Statics: "Windows.UI.Xaml.Media.Animation.IExponentialEaseStatics"
  // Instantiable: "Animation_IExponentialEase"
  TAnimation_ExponentialEase = class(TWinRTGenericImportSI<Animation_IExponentialEaseStatics, Animation_IExponentialEase>)
  public
    // -> Animation_IExponentialEaseStatics
    class function get_ExponentProperty: IDependencyProperty; static; inline;
    class property ExponentProperty: IDependencyProperty read get_ExponentProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.FadeInThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IFadeInThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IFadeInThemeAnimationStatics"
  // Instantiable: "Animation_IFadeInThemeAnimation"
  TAnimation_FadeInThemeAnimation = class(TWinRTGenericImportSI<Animation_IFadeInThemeAnimationStatics, Animation_IFadeInThemeAnimation>)
  public
    // -> Animation_IFadeInThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.FadeOutThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IFadeOutThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IFadeOutThemeAnimationStatics"
  // Instantiable: "Animation_IFadeOutThemeAnimation"
  TAnimation_FadeOutThemeAnimation = class(TWinRTGenericImportSI<Animation_IFadeOutThemeAnimationStatics, Animation_IFadeOutThemeAnimation>)
  public
    // -> Animation_IFadeOutThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.GravityConnectedAnimationConfiguration
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfiguration
  // Implements: Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfiguration2
  // Factory: "Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfigurationFactory"
  // Instantiable: "Animation_IGravityConnectedAnimationConfiguration"
  TAnimation_GravityConnectedAnimationConfiguration = class(TWinRTGenericImportFI<Animation_IGravityConnectedAnimationConfigurationFactory, Animation_IGravityConnectedAnimationConfiguration>)
  public
    // -> Animation_IGravityConnectedAnimationConfigurationFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IGravityConnectedAnimationConfiguration; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.KeySpline
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IKeySpline
  // Instantiable: "Animation_IKeySpline"
  TAnimation_KeySpline = class(TWinRTGenericImportI<Animation_IKeySpline>) end;

  // Windows.UI.Xaml.Media.Animation.KeyTimeHelper
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IKeyTimeHelper
  // Statics: "Windows.UI.Xaml.Media.Animation.IKeyTimeHelperStatics"
  TAnimation_KeyTimeHelper = class(TWinRTGenericImportS<Animation_IKeyTimeHelperStatics>)
  public
    // -> Animation_IKeyTimeHelperStatics
    class function FromTimeSpan(timeSpan: TimeSpan): Animation_KeyTime; static; inline;
  end;

  // Windows.UI.Xaml.Media.Animation.LinearColorKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ILinearColorKeyFrame
  // Instantiable: "Animation_ILinearColorKeyFrame"
  TAnimation_LinearColorKeyFrame = class(TWinRTGenericImportI<Animation_ILinearColorKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.LinearDoubleKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ILinearDoubleKeyFrame
  // Instantiable: "Animation_ILinearDoubleKeyFrame"
  TAnimation_LinearDoubleKeyFrame = class(TWinRTGenericImportI<Animation_ILinearDoubleKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.LinearPointKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ILinearPointKeyFrame
  // Instantiable: "Animation_ILinearPointKeyFrame"
  TAnimation_LinearPointKeyFrame = class(TWinRTGenericImportI<Animation_ILinearPointKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.NavigationThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.INavigationThemeTransition
  // Statics: "Windows.UI.Xaml.Media.Animation.INavigationThemeTransitionStatics"
  // Instantiable: "Animation_INavigationThemeTransition"
  TAnimation_NavigationThemeTransition = class(TWinRTGenericImportSI<Animation_INavigationThemeTransitionStatics, Animation_INavigationThemeTransition>)
  public
    // -> Animation_INavigationThemeTransitionStatics
    class function get_DefaultNavigationTransitionInfoProperty: IDependencyProperty; static; inline;
    class property DefaultNavigationTransitionInfoProperty: IDependencyProperty read get_DefaultNavigationTransitionInfoProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ObjectAnimationUsingKeyFrames
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IObjectAnimationUsingKeyFrames
  // Statics: "Windows.UI.Xaml.Media.Animation.IObjectAnimationUsingKeyFramesStatics"
  // Instantiable: "Animation_IObjectAnimationUsingKeyFrames"
  TAnimation_ObjectAnimationUsingKeyFrames = class(TWinRTGenericImportSI<Animation_IObjectAnimationUsingKeyFramesStatics, Animation_IObjectAnimationUsingKeyFrames>)
  public
    // -> Animation_IObjectAnimationUsingKeyFramesStatics
    class function get_EnableDependentAnimationProperty: IDependencyProperty; static; inline;
    class property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.ObjectKeyFrameCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IObjectKeyFrame>
  // Instantiable: "IVector_1__Animation_IObjectKeyFrame"
  TAnimation_ObjectKeyFrameCollection = class(TWinRTGenericImportI<IVector_1__Animation_IObjectKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.PaneThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPaneThemeTransition
  // Statics: "Windows.UI.Xaml.Media.Animation.IPaneThemeTransitionStatics"
  // Instantiable: "Animation_IPaneThemeTransition"
  TAnimation_PaneThemeTransition = class(TWinRTGenericImportSI<Animation_IPaneThemeTransitionStatics, Animation_IPaneThemeTransition>)
  public
    // -> Animation_IPaneThemeTransitionStatics
    class function get_EdgeProperty: IDependencyProperty; static; inline;
    class property EdgeProperty: IDependencyProperty read get_EdgeProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PointAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPointAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IPointAnimationStatics"
  // Instantiable: "Animation_IPointAnimation"
  TAnimation_PointAnimation = class(TWinRTGenericImportSI<Animation_IPointAnimationStatics, Animation_IPointAnimation>)
  public
    // -> Animation_IPointAnimationStatics
    class function get_FromProperty: IDependencyProperty; static; inline;
    class function get_ToProperty: IDependencyProperty; static; inline;
    class function get_ByProperty: IDependencyProperty; static; inline;
    class function get_EasingFunctionProperty: IDependencyProperty; static; inline;
    class function get_EnableDependentAnimationProperty: IDependencyProperty; static; inline;
    class property ByProperty: IDependencyProperty read get_ByProperty;
    class property EasingFunctionProperty: IDependencyProperty read get_EasingFunctionProperty;
    class property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
    class property FromProperty: IDependencyProperty read get_FromProperty;
    class property ToProperty: IDependencyProperty read get_ToProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PointAnimationUsingKeyFrames
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPointAnimationUsingKeyFrames
  // Statics: "Windows.UI.Xaml.Media.Animation.IPointAnimationUsingKeyFramesStatics"
  // Instantiable: "Animation_IPointAnimationUsingKeyFrames"
  TAnimation_PointAnimationUsingKeyFrames = class(TWinRTGenericImportSI<Animation_IPointAnimationUsingKeyFramesStatics, Animation_IPointAnimationUsingKeyFrames>)
  public
    // -> Animation_IPointAnimationUsingKeyFramesStatics
    class function get_EnableDependentAnimationProperty: IDependencyProperty; static; inline;
    class property EnableDependentAnimationProperty: IDependencyProperty read get_EnableDependentAnimationProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PointKeyFrameCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.IPointKeyFrame>
  // Instantiable: "IVector_1__Animation_IPointKeyFrame"
  TAnimation_PointKeyFrameCollection = class(TWinRTGenericImportI<IVector_1__Animation_IPointKeyFrame>) end;

  // Windows.UI.Xaml.Media.Animation.PointerDownThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPointerDownThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IPointerDownThemeAnimationStatics"
  // Instantiable: "Animation_IPointerDownThemeAnimation"
  TAnimation_PointerDownThemeAnimation = class(TWinRTGenericImportSI<Animation_IPointerDownThemeAnimationStatics, Animation_IPointerDownThemeAnimation>)
  public
    // -> Animation_IPointerDownThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PointerUpThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPointerUpThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IPointerUpThemeAnimationStatics"
  // Instantiable: "Animation_IPointerUpThemeAnimation"
  TAnimation_PointerUpThemeAnimation = class(TWinRTGenericImportSI<Animation_IPointerUpThemeAnimationStatics, Animation_IPointerUpThemeAnimation>)
  public
    // -> Animation_IPointerUpThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PopInThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPopInThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IPopInThemeAnimationStatics"
  // Instantiable: "Animation_IPopInThemeAnimation"
  TAnimation_PopInThemeAnimation = class(TWinRTGenericImportSI<Animation_IPopInThemeAnimationStatics, Animation_IPopInThemeAnimation>)
  public
    // -> Animation_IPopInThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class function get_FromHorizontalOffsetProperty: IDependencyProperty; static; inline;
    class function get_FromVerticalOffsetProperty: IDependencyProperty; static; inline;
    class property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    class property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PopOutThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPopOutThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IPopOutThemeAnimationStatics"
  // Instantiable: "Animation_IPopOutThemeAnimation"
  TAnimation_PopOutThemeAnimation = class(TWinRTGenericImportSI<Animation_IPopOutThemeAnimationStatics, Animation_IPopOutThemeAnimation>)
  public
    // -> Animation_IPopOutThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PopupThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPopupThemeTransition
  // Statics: "Windows.UI.Xaml.Media.Animation.IPopupThemeTransitionStatics"
  // Instantiable: "Animation_IPopupThemeTransition"
  TAnimation_PopupThemeTransition = class(TWinRTGenericImportSI<Animation_IPopupThemeTransitionStatics, Animation_IPopupThemeTransition>)
  public
    // -> Animation_IPopupThemeTransitionStatics
    class function get_FromHorizontalOffsetProperty: IDependencyProperty; static; inline;
    class function get_FromVerticalOffsetProperty: IDependencyProperty; static; inline;
    class property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    class property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.PowerEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IPowerEase
  // Statics: "Windows.UI.Xaml.Media.Animation.IPowerEaseStatics"
  // Instantiable: "Animation_IPowerEase"
  TAnimation_PowerEase = class(TWinRTGenericImportSI<Animation_IPowerEaseStatics, Animation_IPowerEase>)
  public
    // -> Animation_IPowerEaseStatics
    class function get_PowerProperty: IDependencyProperty; static; inline;
    class property PowerProperty: IDependencyProperty read get_PowerProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.QuadraticEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IQuadraticEase
  // Instantiable: "Animation_IQuadraticEase"
  TAnimation_QuadraticEase = class(TWinRTGenericImportI<Animation_IQuadraticEase>) end;

  // Windows.UI.Xaml.Media.Animation.QuarticEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IQuarticEase
  // Instantiable: "Animation_IQuarticEase"
  TAnimation_QuarticEase = class(TWinRTGenericImportI<Animation_IQuarticEase>) end;

  // Windows.UI.Xaml.Media.Animation.QuinticEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IQuinticEase
  // Instantiable: "Animation_IQuinticEase"
  TAnimation_QuinticEase = class(TWinRTGenericImportI<Animation_IQuinticEase>) end;

  // Windows.UI.Xaml.Media.Animation.ReorderThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IReorderThemeTransition
  // Instantiable: "Animation_IReorderThemeTransition"
  TAnimation_ReorderThemeTransition = class(TWinRTGenericImportI<Animation_IReorderThemeTransition>) end;

  // Windows.UI.Xaml.Media.Animation.RepeatBehaviorHelper
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IRepeatBehaviorHelper
  // Statics: "Windows.UI.Xaml.Media.Animation.IRepeatBehaviorHelperStatics"
  TAnimation_RepeatBehaviorHelper = class(TWinRTGenericImportS<Animation_IRepeatBehaviorHelperStatics>)
  public
    // -> Animation_IRepeatBehaviorHelperStatics
    class function get_Forever: Animation_RepeatBehavior; static; inline;
    class function FromCount(count: Double): Animation_RepeatBehavior; static; inline;
    class function FromDuration(duration: TimeSpan): Animation_RepeatBehavior; static; inline;
    class function GetHasCount(target: Animation_RepeatBehavior): Boolean; static; inline;
    class function GetHasDuration(target: Animation_RepeatBehavior): Boolean; static; inline;
    class function Equals(target: Animation_RepeatBehavior; value: Animation_RepeatBehavior): Boolean; reintroduce; static; inline;
    class property Forever: Animation_RepeatBehavior read get_Forever;
  end;

  // Windows.UI.Xaml.Media.Animation.RepositionThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IRepositionThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.IRepositionThemeAnimationStatics"
  // Instantiable: "Animation_IRepositionThemeAnimation"
  TAnimation_RepositionThemeAnimation = class(TWinRTGenericImportSI<Animation_IRepositionThemeAnimationStatics, Animation_IRepositionThemeAnimation>)
  public
    // -> Animation_IRepositionThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class function get_FromHorizontalOffsetProperty: IDependencyProperty; static; inline;
    class function get_FromVerticalOffsetProperty: IDependencyProperty; static; inline;
    class property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    class property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.RepositionThemeTransition
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IRepositionThemeTransition
  // Implements: Windows.UI.Xaml.Media.Animation.IRepositionThemeTransition2
  // Statics: "Windows.UI.Xaml.Media.Animation.IRepositionThemeTransitionStatics2"
  // Instantiable: "Animation_IRepositionThemeTransition"
  TAnimation_RepositionThemeTransition = class(TWinRTGenericImportSI<Animation_IRepositionThemeTransitionStatics2, Animation_IRepositionThemeTransition>)
  public
    // -> Animation_IRepositionThemeTransitionStatics2
    class function get_IsStaggeringEnabledProperty: IDependencyProperty; static; inline;
    class property IsStaggeringEnabledProperty: IDependencyProperty read get_IsStaggeringEnabledProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SineEase
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISineEase
  // Instantiable: "Animation_ISineEase"
  TAnimation_SineEase = class(TWinRTGenericImportI<Animation_ISineEase>) end;

  // Windows.UI.Xaml.Media.Animation.SlideNavigationTransitionInfo
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfo
  // Implements: Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfo2
  // Statics: "Windows.UI.Xaml.Media.Animation.ISlideNavigationTransitionInfoStatics2"
  // Instantiable: "Animation_ISlideNavigationTransitionInfo"
  TAnimation_SlideNavigationTransitionInfo = class(TWinRTGenericImportSI<Animation_ISlideNavigationTransitionInfoStatics2, Animation_ISlideNavigationTransitionInfo>)
  public
    // -> Animation_ISlideNavigationTransitionInfoStatics2
    class function get_EffectProperty: IDependencyProperty; static; inline;
    class property EffectProperty: IDependencyProperty read get_EffectProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SplineColorKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISplineColorKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.ISplineColorKeyFrameStatics"
  // Instantiable: "Animation_ISplineColorKeyFrame"
  TAnimation_SplineColorKeyFrame = class(TWinRTGenericImportSI<Animation_ISplineColorKeyFrameStatics, Animation_ISplineColorKeyFrame>)
  public
    // -> Animation_ISplineColorKeyFrameStatics
    class function get_KeySplineProperty: IDependencyProperty; static; inline;
    class property KeySplineProperty: IDependencyProperty read get_KeySplineProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SplineDoubleKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISplineDoubleKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.ISplineDoubleKeyFrameStatics"
  // Instantiable: "Animation_ISplineDoubleKeyFrame"
  TAnimation_SplineDoubleKeyFrame = class(TWinRTGenericImportSI<Animation_ISplineDoubleKeyFrameStatics, Animation_ISplineDoubleKeyFrame>)
  public
    // -> Animation_ISplineDoubleKeyFrameStatics
    class function get_KeySplineProperty: IDependencyProperty; static; inline;
    class property KeySplineProperty: IDependencyProperty read get_KeySplineProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SplinePointKeyFrame
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISplinePointKeyFrame
  // Statics: "Windows.UI.Xaml.Media.Animation.ISplinePointKeyFrameStatics"
  // Instantiable: "Animation_ISplinePointKeyFrame"
  TAnimation_SplinePointKeyFrame = class(TWinRTGenericImportSI<Animation_ISplinePointKeyFrameStatics, Animation_ISplinePointKeyFrame>)
  public
    // -> Animation_ISplinePointKeyFrameStatics
    class function get_KeySplineProperty: IDependencyProperty; static; inline;
    class property KeySplineProperty: IDependencyProperty read get_KeySplineProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SplitCloseThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISplitCloseThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.ISplitCloseThemeAnimationStatics"
  // Instantiable: "Animation_ISplitCloseThemeAnimation"
  TAnimation_SplitCloseThemeAnimation = class(TWinRTGenericImportSI<Animation_ISplitCloseThemeAnimationStatics, Animation_ISplitCloseThemeAnimation>)
  public
    // -> Animation_ISplitCloseThemeAnimationStatics
    class function get_OpenedTargetNameProperty: IDependencyProperty; static; inline;
    class function get_OpenedTargetProperty: IDependencyProperty; static; inline;
    class function get_ClosedTargetNameProperty: IDependencyProperty; static; inline;
    class function get_ClosedTargetProperty: IDependencyProperty; static; inline;
    class function get_ContentTargetNameProperty: IDependencyProperty; static; inline;
    class function get_ContentTargetProperty: IDependencyProperty; static; inline;
    class function get_OpenedLengthProperty: IDependencyProperty; static; inline;
    class function get_ClosedLengthProperty: IDependencyProperty; static; inline;
    class function get_OffsetFromCenterProperty: IDependencyProperty; static; inline;
    class function get_ContentTranslationDirectionProperty: IDependencyProperty; static; inline;
    class function get_ContentTranslationOffsetProperty: IDependencyProperty; static; inline;
    class property ClosedLengthProperty: IDependencyProperty read get_ClosedLengthProperty;
    class property ClosedTargetNameProperty: IDependencyProperty read get_ClosedTargetNameProperty;
    class property ClosedTargetProperty: IDependencyProperty read get_ClosedTargetProperty;
    class property ContentTargetNameProperty: IDependencyProperty read get_ContentTargetNameProperty;
    class property ContentTargetProperty: IDependencyProperty read get_ContentTargetProperty;
    class property ContentTranslationDirectionProperty: IDependencyProperty read get_ContentTranslationDirectionProperty;
    class property ContentTranslationOffsetProperty: IDependencyProperty read get_ContentTranslationOffsetProperty;
    class property OffsetFromCenterProperty: IDependencyProperty read get_OffsetFromCenterProperty;
    class property OpenedLengthProperty: IDependencyProperty read get_OpenedLengthProperty;
    class property OpenedTargetNameProperty: IDependencyProperty read get_OpenedTargetNameProperty;
    class property OpenedTargetProperty: IDependencyProperty read get_OpenedTargetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SplitOpenThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISplitOpenThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.ISplitOpenThemeAnimationStatics"
  // Instantiable: "Animation_ISplitOpenThemeAnimation"
  TAnimation_SplitOpenThemeAnimation = class(TWinRTGenericImportSI<Animation_ISplitOpenThemeAnimationStatics, Animation_ISplitOpenThemeAnimation>)
  public
    // -> Animation_ISplitOpenThemeAnimationStatics
    class function get_OpenedTargetNameProperty: IDependencyProperty; static; inline;
    class function get_OpenedTargetProperty: IDependencyProperty; static; inline;
    class function get_ClosedTargetNameProperty: IDependencyProperty; static; inline;
    class function get_ClosedTargetProperty: IDependencyProperty; static; inline;
    class function get_ContentTargetNameProperty: IDependencyProperty; static; inline;
    class function get_ContentTargetProperty: IDependencyProperty; static; inline;
    class function get_OpenedLengthProperty: IDependencyProperty; static; inline;
    class function get_ClosedLengthProperty: IDependencyProperty; static; inline;
    class function get_OffsetFromCenterProperty: IDependencyProperty; static; inline;
    class function get_ContentTranslationDirectionProperty: IDependencyProperty; static; inline;
    class function get_ContentTranslationOffsetProperty: IDependencyProperty; static; inline;
    class property ClosedLengthProperty: IDependencyProperty read get_ClosedLengthProperty;
    class property ClosedTargetNameProperty: IDependencyProperty read get_ClosedTargetNameProperty;
    class property ClosedTargetProperty: IDependencyProperty read get_ClosedTargetProperty;
    class property ContentTargetNameProperty: IDependencyProperty read get_ContentTargetNameProperty;
    class property ContentTargetProperty: IDependencyProperty read get_ContentTargetProperty;
    class property ContentTranslationDirectionProperty: IDependencyProperty read get_ContentTranslationDirectionProperty;
    class property ContentTranslationOffsetProperty: IDependencyProperty read get_ContentTranslationOffsetProperty;
    class property OffsetFromCenterProperty: IDependencyProperty read get_OffsetFromCenterProperty;
    class property OpenedLengthProperty: IDependencyProperty read get_OpenedLengthProperty;
    class property OpenedTargetNameProperty: IDependencyProperty read get_OpenedTargetNameProperty;
    class property OpenedTargetProperty: IDependencyProperty read get_OpenedTargetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.Storyboard
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.IStoryboard
  // Statics: "Windows.UI.Xaml.Media.Animation.IStoryboardStatics"
  // Instantiable: "Animation_IStoryboard"
  TAnimation_Storyboard = class(TWinRTGenericImportSI<Animation_IStoryboardStatics, Animation_IStoryboard>)
  public
    // -> Animation_IStoryboardStatics
    class function get_TargetPropertyProperty: IDependencyProperty; static; inline;
    class function GetTargetProperty(element: Animation_ITimeline): HSTRING; static; inline;
    class procedure SetTargetProperty(element: Animation_ITimeline; path: HSTRING); static; inline;
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class function GetTargetName(element: Animation_ITimeline): HSTRING; static; inline;
    class procedure SetTargetName(element: Animation_ITimeline; name: HSTRING); static; inline;
    class procedure SetTarget(timeline: Animation_ITimeline; target: IDependencyObject); static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
    class property TargetPropertyProperty: IDependencyProperty read get_TargetPropertyProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SuppressNavigationTransitionInfo
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISuppressNavigationTransitionInfo
  // Instantiable: "Animation_ISuppressNavigationTransitionInfo"
  TAnimation_SuppressNavigationTransitionInfo = class(TWinRTGenericImportI<Animation_ISuppressNavigationTransitionInfo>) end;

  // Windows.UI.Xaml.Media.Animation.SwipeBackThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISwipeBackThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.ISwipeBackThemeAnimationStatics"
  // Instantiable: "Animation_ISwipeBackThemeAnimation"
  TAnimation_SwipeBackThemeAnimation = class(TWinRTGenericImportSI<Animation_ISwipeBackThemeAnimationStatics, Animation_ISwipeBackThemeAnimation>)
  public
    // -> Animation_ISwipeBackThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class function get_FromHorizontalOffsetProperty: IDependencyProperty; static; inline;
    class function get_FromVerticalOffsetProperty: IDependencyProperty; static; inline;
    class property FromHorizontalOffsetProperty: IDependencyProperty read get_FromHorizontalOffsetProperty;
    class property FromVerticalOffsetProperty: IDependencyProperty read get_FromVerticalOffsetProperty;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.SwipeHintThemeAnimation
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Animation.ISwipeHintThemeAnimation
  // Statics: "Windows.UI.Xaml.Media.Animation.ISwipeHintThemeAnimationStatics"
  // Instantiable: "Animation_ISwipeHintThemeAnimation"
  TAnimation_SwipeHintThemeAnimation = class(TWinRTGenericImportSI<Animation_ISwipeHintThemeAnimationStatics, Animation_ISwipeHintThemeAnimation>)
  public
    // -> Animation_ISwipeHintThemeAnimationStatics
    class function get_TargetNameProperty: IDependencyProperty; static; inline;
    class function get_ToHorizontalOffsetProperty: IDependencyProperty; static; inline;
    class function get_ToVerticalOffsetProperty: IDependencyProperty; static; inline;
    class property TargetNameProperty: IDependencyProperty read get_TargetNameProperty;
    class property ToHorizontalOffsetProperty: IDependencyProperty read get_ToHorizontalOffsetProperty;
    class property ToVerticalOffsetProperty: IDependencyProperty read get_ToVerticalOffsetProperty;
  end;

  // Windows.UI.Xaml.Media.Animation.TimelineCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITimeline>
  // Instantiable: "IVector_1__Animation_ITimeline"
  TAnimation_TimelineCollection = class(TWinRTGenericImportI<IVector_1__Animation_ITimeline>) end;

  // Windows.UI.Xaml.Media.Animation.TransitionCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.Animation.ITransition>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.Animation.ITransition>
  // Instantiable: "IVector_1__Animation_ITransition"
  TAnimation_TransitionCollection = class(TWinRTGenericImportI<IVector_1__Animation_ITransition>) end;

  // Windows.UI.Xaml.Media.PathSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPathSegment
  // Factory: "Windows.UI.Xaml.Media.IPathSegmentFactory"
  // Instantiable: "IPathSegment"
  TPathSegment = class(TWinRTGenericImportFI<IPathSegmentFactory, IPathSegment>)
  public
    // -> IPathSegmentFactory
  end;

  // Windows.UI.Xaml.Media.ArcSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IArcSegment
  // Statics: "Windows.UI.Xaml.Media.IArcSegmentStatics"
  // Instantiable: "IArcSegment"
  TArcSegment = class(TWinRTGenericImportSI<IArcSegmentStatics, IArcSegment>)
  public
    // -> IArcSegmentStatics
    class function get_PointProperty: IDependencyProperty; static; inline;
    class function get_SizeProperty: IDependencyProperty; static; inline;
    class function get_RotationAngleProperty: IDependencyProperty; static; inline;
    class function get_IsLargeArcProperty: IDependencyProperty; static; inline;
    class function get_SweepDirectionProperty: IDependencyProperty; static; inline;
    class property IsLargeArcProperty: IDependencyProperty read get_IsLargeArcProperty;
    class property PointProperty: IDependencyProperty read get_PointProperty;
    class property RotationAngleProperty: IDependencyProperty read get_RotationAngleProperty;
    class property SizeProperty: IDependencyProperty read get_SizeProperty;
    class property SweepDirectionProperty: IDependencyProperty read get_SweepDirectionProperty;
  end;

  // Windows.UI.Xaml.Media.BezierSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IBezierSegment
  // Statics: "Windows.UI.Xaml.Media.IBezierSegmentStatics"
  // Instantiable: "IBezierSegment"
  TBezierSegment = class(TWinRTGenericImportSI<IBezierSegmentStatics, IBezierSegment>)
  public
    // -> IBezierSegmentStatics
    class function get_Point1Property: IDependencyProperty; static; inline;
    class function get_Point2Property: IDependencyProperty; static; inline;
    class function get_Point3Property: IDependencyProperty; static; inline;
    class property Point1Property: IDependencyProperty read get_Point1Property;
    class property Point2Property: IDependencyProperty read get_Point2Property;
    class property Point3Property: IDependencyProperty read get_Point3Property;
  end;

  // Windows.UI.Xaml.Media.CacheMode
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ICacheMode
  // Factory: "Windows.UI.Xaml.Media.ICacheModeFactory"
  // Instantiable: "ICacheMode"
  TCacheMode = class(TWinRTGenericImportFI<ICacheModeFactory, ICacheMode>)
  public
    // -> ICacheModeFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): ICacheMode; static; inline;
  end;

  // Windows.UI.Xaml.Media.BitmapCache
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IBitmapCache
  // Instantiable: "IBitmapCache"
  TBitmapCache = class(TWinRTGenericImportI<IBitmapCache>) end;

  // Windows.UI.Xaml.Media.BrushCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IBrush>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IBrush>
  // Instantiable: "IVector_1__IBrush"
  TBrushCollection = class(TWinRTGenericImportI<IVector_1__IBrush>) end;

  // Windows.UI.Xaml.Media.GeneralTransform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IGeneralTransform
  // Implements: Windows.UI.Xaml.Media.IGeneralTransformOverrides
  // Factory: "Windows.UI.Xaml.Media.IGeneralTransformFactory"
  // Instantiable: "IGeneralTransform"
  TGeneralTransform = class(TWinRTGenericImportFI<IGeneralTransformFactory, IGeneralTransform>)
  public
    // -> IGeneralTransformFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IGeneralTransform; static; inline;
  end;

  // Windows.UI.Xaml.Media.Transform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ITransform
  // Factory: "Windows.UI.Xaml.Media.ITransformFactory"
  // Instantiable: "ITransform"
  TTransform = class(TWinRTGenericImportFI<ITransformFactory, ITransform>)
  public
    // -> ITransformFactory
  end;

  // Windows.UI.Xaml.Media.CompositeTransform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ICompositeTransform
  // Statics: "Windows.UI.Xaml.Media.ICompositeTransformStatics"
  // Instantiable: "ICompositeTransform"
  TCompositeTransform = class(TWinRTGenericImportSI<ICompositeTransformStatics, ICompositeTransform>)
  public
    // -> ICompositeTransformStatics
    class function get_CenterXProperty: IDependencyProperty; static; inline;
    class function get_CenterYProperty: IDependencyProperty; static; inline;
    class function get_ScaleXProperty: IDependencyProperty; static; inline;
    class function get_ScaleYProperty: IDependencyProperty; static; inline;
    class function get_SkewXProperty: IDependencyProperty; static; inline;
    class function get_SkewYProperty: IDependencyProperty; static; inline;
    class function get_RotationProperty: IDependencyProperty; static; inline;
    class function get_TranslateXProperty: IDependencyProperty; static; inline;
    class function get_TranslateYProperty: IDependencyProperty; static; inline;
    class property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    class property CenterYProperty: IDependencyProperty read get_CenterYProperty;
    class property RotationProperty: IDependencyProperty read get_RotationProperty;
    class property ScaleXProperty: IDependencyProperty read get_ScaleXProperty;
    class property ScaleYProperty: IDependencyProperty read get_ScaleYProperty;
    class property SkewXProperty: IDependencyProperty read get_SkewXProperty;
    class property SkewYProperty: IDependencyProperty read get_SkewYProperty;
    class property TranslateXProperty: IDependencyProperty read get_TranslateXProperty;
    class property TranslateYProperty: IDependencyProperty read get_TranslateYProperty;
  end;

  // Windows.UI.Xaml.Media.CompositionTarget
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ICompositionTarget
  // Statics: "Windows.UI.Xaml.Media.ICompositionTargetStatics"
  // Statics: "Windows.UI.Xaml.Media.ICompositionTargetStatics3"
  TCompositionTarget = class(TWinRTGenericImportS2<ICompositionTargetStatics, ICompositionTargetStatics3>)
  public
    // -> ICompositionTargetStatics
    class function add_Rendering(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_Rendering(token: EventRegistrationToken); static; inline;
    class function add_SurfaceContentsLost(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_SurfaceContentsLost(token: EventRegistrationToken); static; inline;

    // -> ICompositionTargetStatics3
    class function add_Rendered(handler: EventHandler_1__IRenderedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_Rendered(token: EventRegistrationToken); static; inline;
  end;

  // Windows.UI.Xaml.Media.DoubleCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Double>
  // Implements: Windows.Foundation.Collections.IIterable`1<Double>
  // Instantiable: "IVector_1__Double"
  TDoubleCollection = class(TWinRTGenericImportI<IVector_1__Double>) end;

  // Windows.UI.Xaml.Media.Geometry
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IGeometry
  // Statics: "Windows.UI.Xaml.Media.IGeometryStatics"
  // Factory: "Windows.UI.Xaml.Media.IGeometryFactory"
  // Instantiable: "IGeometry"
  TGeometry = class(TWinRTGenericImportFSI<IGeometryFactory, IGeometryStatics, IGeometry>)
  public
    // -> IGeometryStatics
    class function get_Empty: IGeometry; static; inline;
    class function get_StandardFlatteningTolerance: Double; static; inline;
    class function get_TransformProperty: IDependencyProperty; static; inline;
    class property Empty: IGeometry read get_Empty;
    class property StandardFlatteningTolerance: Double read get_StandardFlatteningTolerance;
    class property TransformProperty: IDependencyProperty read get_TransformProperty;

    // -> IGeometryFactory
  end;

  // Windows.UI.Xaml.Media.EllipseGeometry
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IEllipseGeometry
  // Statics: "Windows.UI.Xaml.Media.IEllipseGeometryStatics"
  // Instantiable: "IEllipseGeometry"
  TEllipseGeometry = class(TWinRTGenericImportSI<IEllipseGeometryStatics, IEllipseGeometry>)
  public
    // -> IEllipseGeometryStatics
    class function get_CenterProperty: IDependencyProperty; static; inline;
    class function get_RadiusXProperty: IDependencyProperty; static; inline;
    class function get_RadiusYProperty: IDependencyProperty; static; inline;
    class property CenterProperty: IDependencyProperty read get_CenterProperty;
    class property RadiusXProperty: IDependencyProperty read get_RadiusXProperty;
    class property RadiusYProperty: IDependencyProperty read get_RadiusYProperty;
  end;

  // Windows.UI.Xaml.Media.FontFamily
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IFontFamily
  // Statics: "Windows.UI.Xaml.Media.IFontFamilyStatics2"
  // Factory: "Windows.UI.Xaml.Media.IFontFamilyFactory"
  // Instantiable: "IFontFamily"
  TFontFamily = class(TWinRTGenericImportFSI<IFontFamilyFactory, IFontFamilyStatics2, IFontFamily>)
  public
    // -> IFontFamilyStatics2
    class function get_XamlAutoFontFamily: IFontFamily; static; inline;
    class property XamlAutoFontFamily: IFontFamily read get_XamlAutoFontFamily;

    // -> IFontFamilyFactory
    class function CreateInstanceWithName(familyName: HSTRING; baseInterface: IInspectable; out innerInterface: IInspectable): IFontFamily; static; inline;
  end;

  // Windows.UI.Xaml.Media.GeometryCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGeometry>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGeometry>
  // Instantiable: "IVector_1__IGeometry"
  TGeometryCollection = class(TWinRTGenericImportI<IVector_1__IGeometry>) end;

  // Windows.UI.Xaml.Media.GeometryGroup
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IGeometryGroup
  // Statics: "Windows.UI.Xaml.Media.IGeometryGroupStatics"
  // Instantiable: "IGeometryGroup"
  TGeometryGroup = class(TWinRTGenericImportSI<IGeometryGroupStatics, IGeometryGroup>)
  public
    // -> IGeometryGroupStatics
    class function get_FillRuleProperty: IDependencyProperty; static; inline;
    class function get_ChildrenProperty: IDependencyProperty; static; inline;
    class property ChildrenProperty: IDependencyProperty read get_ChildrenProperty;
    class property FillRuleProperty: IDependencyProperty read get_FillRuleProperty;
  end;

  // Windows.UI.Xaml.Media.GradientBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IGradientBrush
  // Statics: "Windows.UI.Xaml.Media.IGradientBrushStatics"
  // Factory: "Windows.UI.Xaml.Media.IGradientBrushFactory"
  // Instantiable: "IGradientBrush"
  TGradientBrush = class(TWinRTGenericImportFSI<IGradientBrushFactory, IGradientBrushStatics, IGradientBrush>)
  public
    // -> IGradientBrushStatics
    class function get_SpreadMethodProperty: IDependencyProperty; static; inline;
    class function get_MappingModeProperty: IDependencyProperty; static; inline;
    class function get_ColorInterpolationModeProperty: IDependencyProperty; static; inline;
    class function get_GradientStopsProperty: IDependencyProperty; static; inline;
    class property ColorInterpolationModeProperty: IDependencyProperty read get_ColorInterpolationModeProperty;
    class property GradientStopsProperty: IDependencyProperty read get_GradientStopsProperty;
    class property MappingModeProperty: IDependencyProperty read get_MappingModeProperty;
    class property SpreadMethodProperty: IDependencyProperty read get_SpreadMethodProperty;

    // -> IGradientBrushFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IGradientBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.GradientStop
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IGradientStop
  // Statics: "Windows.UI.Xaml.Media.IGradientStopStatics"
  // Instantiable: "IGradientStop"
  TGradientStop = class(TWinRTGenericImportSI<IGradientStopStatics, IGradientStop>)
  public
    // -> IGradientStopStatics
    class function get_ColorProperty: IDependencyProperty; static; inline;
    class function get_OffsetProperty: IDependencyProperty; static; inline;
    class property ColorProperty: IDependencyProperty read get_ColorProperty;
    class property OffsetProperty: IDependencyProperty read get_OffsetProperty;
  end;

  // Windows.UI.Xaml.Media.GradientStopCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IGradientStop>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IGradientStop>
  // Instantiable: "IVector_1__IGradientStop"
  TGradientStopCollection = class(TWinRTGenericImportI<IVector_1__IGradientStop>) end;

  // Windows.UI.Xaml.Media.ImageBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IImageBrush
  // Statics: "Windows.UI.Xaml.Media.IImageBrushStatics"
  // Instantiable: "IImageBrush"
  TImageBrush = class(TWinRTGenericImportSI<IImageBrushStatics, IImageBrush>)
  public
    // -> IImageBrushStatics
    class function get_ImageSourceProperty: IDependencyProperty; static; inline;
    class property ImageSourceProperty: IDependencyProperty read get_ImageSourceProperty;
  end;

  // Windows.UI.Xaml.Media.ImageSource
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IImageSource
  // Factory: "Windows.UI.Xaml.Media.IImageSourceFactory"
  // Instantiable: "IImageSource"
  TImageSource = class(TWinRTGenericImportFI<IImageSourceFactory, IImageSource>)
  public
    // -> IImageSourceFactory
  end;

  // Windows.UI.Xaml.Media.Imaging.BitmapSource
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.IBitmapSource
  // Statics: "Windows.UI.Xaml.Media.Imaging.IBitmapSourceStatics"
  // Factory: "Windows.UI.Xaml.Media.Imaging.IBitmapSourceFactory"
  // Instantiable: "Imaging_IBitmapSource"
  TImaging_BitmapSource = class(TWinRTGenericImportFSI<Imaging_IBitmapSourceFactory, Imaging_IBitmapSourceStatics, Imaging_IBitmapSource>)
  public
    // -> Imaging_IBitmapSourceStatics
    class function get_PixelWidthProperty: IDependencyProperty; static; inline;
    class function get_PixelHeightProperty: IDependencyProperty; static; inline;
    class property PixelHeightProperty: IDependencyProperty read get_PixelHeightProperty;
    class property PixelWidthProperty: IDependencyProperty read get_PixelWidthProperty;

    // -> Imaging_IBitmapSourceFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_IBitmapSource; static; inline;
  end;

  // Windows.UI.Xaml.Media.Imaging.BitmapImage
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.IBitmapImage
  // Implements: Windows.UI.Xaml.Media.Imaging.IBitmapImage2
  // Implements: Windows.UI.Xaml.Media.Imaging.IBitmapImage3
  // Statics: "Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics"
  // Statics: "Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics2"
  // Statics: "Windows.UI.Xaml.Media.Imaging.IBitmapImageStatics3"
  // Factory: "Windows.UI.Xaml.Media.Imaging.IBitmapImageFactory"
  // Instantiable: "Imaging_IBitmapImage"
  TImaging_BitmapImage = class(TWinRTGenericImportFS3I<Imaging_IBitmapImageFactory, Imaging_IBitmapImageStatics, Imaging_IBitmapImageStatics2, Imaging_IBitmapImageStatics3, Imaging_IBitmapImage>)
  public
    // -> Imaging_IBitmapImageStatics
    class function get_CreateOptionsProperty: IDependencyProperty; static; inline;
    class function get_UriSourceProperty: IDependencyProperty; static; inline;
    class function get_DecodePixelWidthProperty: IDependencyProperty; static; inline;
    class function get_DecodePixelHeightProperty: IDependencyProperty; static; inline;
    class property CreateOptionsProperty: IDependencyProperty read get_CreateOptionsProperty;
    class property DecodePixelHeightProperty: IDependencyProperty read get_DecodePixelHeightProperty;
    class property DecodePixelWidthProperty: IDependencyProperty read get_DecodePixelWidthProperty;
    class property UriSourceProperty: IDependencyProperty read get_UriSourceProperty;

    // -> Imaging_IBitmapImageStatics2
    class function get_DecodePixelTypeProperty: IDependencyProperty; static; inline;
    class property DecodePixelTypeProperty: IDependencyProperty read get_DecodePixelTypeProperty;

    // -> Imaging_IBitmapImageStatics3
    class function get_IsAnimatedBitmapProperty: IDependencyProperty; static; inline;
    class function get_IsPlayingProperty: IDependencyProperty; static; inline;
    class function get_AutoPlayProperty: IDependencyProperty; static; inline;
    class property AutoPlayProperty: IDependencyProperty read get_AutoPlayProperty;
    class property IsAnimatedBitmapProperty: IDependencyProperty read get_IsAnimatedBitmapProperty;
    class property IsPlayingProperty: IDependencyProperty read get_IsPlayingProperty;

    // -> Imaging_IBitmapImageFactory
    class function CreateInstanceWithUriSource(uriSource: IUriRuntimeClass): Imaging_IBitmapImage; static; inline;
  end;

  // Windows.UI.Xaml.Media.Imaging.RenderTargetBitmap
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.IRenderTargetBitmap
  // Statics: "Windows.UI.Xaml.Media.Imaging.IRenderTargetBitmapStatics"
  // Instantiable: "Imaging_IRenderTargetBitmap"
  TImaging_RenderTargetBitmap = class(TWinRTGenericImportSI<Imaging_IRenderTargetBitmapStatics, Imaging_IRenderTargetBitmap>)
  public
    // -> Imaging_IRenderTargetBitmapStatics
    class function get_PixelWidthProperty: IDependencyProperty; static; inline;
    class function get_PixelHeightProperty: IDependencyProperty; static; inline;
    class property PixelHeightProperty: IDependencyProperty read get_PixelHeightProperty;
    class property PixelWidthProperty: IDependencyProperty read get_PixelWidthProperty;
  end;

  // Windows.UI.Xaml.Media.Imaging.SoftwareBitmapSource
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.ISoftwareBitmapSource
  // Implements: Windows.Foundation.IClosable
  // Instantiable: "Imaging_ISoftwareBitmapSource"
  TImaging_SoftwareBitmapSource = class(TWinRTGenericImportI<Imaging_ISoftwareBitmapSource>) end;

  // Windows.UI.Xaml.Media.Imaging.SurfaceImageSource
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.ISurfaceImageSource
  // Factory: "Windows.UI.Xaml.Media.Imaging.ISurfaceImageSourceFactory"
  // Instantiable: "Imaging_ISurfaceImageSource"
  TImaging_SurfaceImageSource = class(TWinRTGenericImportFI<Imaging_ISurfaceImageSourceFactory, Imaging_ISurfaceImageSource>)
  public
    // -> Imaging_ISurfaceImageSourceFactory
    class function CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISurfaceImageSource; static; inline;
    class function CreateInstanceWithDimensionsAndOpacity(pixelWidth: Integer; pixelHeight: Integer; isOpaque: Boolean; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISurfaceImageSource; static; inline;
  end;

  // Windows.UI.Xaml.Media.Imaging.SvgImageSource
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.ISvgImageSource
  // Statics: "Windows.UI.Xaml.Media.Imaging.ISvgImageSourceStatics"
  // Factory: "Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFactory"
  // Instantiable: "Imaging_ISvgImageSource"
  TImaging_SvgImageSource = class(TWinRTGenericImportFSI<Imaging_ISvgImageSourceFactory, Imaging_ISvgImageSourceStatics, Imaging_ISvgImageSource>)
  public
    // -> Imaging_ISvgImageSourceStatics
    class function get_UriSourceProperty: IDependencyProperty; static; inline;
    class function get_RasterizePixelWidthProperty: IDependencyProperty; static; inline;
    class function get_RasterizePixelHeightProperty: IDependencyProperty; static; inline;
    class property RasterizePixelHeightProperty: IDependencyProperty read get_RasterizePixelHeightProperty;
    class property RasterizePixelWidthProperty: IDependencyProperty read get_RasterizePixelWidthProperty;
    class property UriSourceProperty: IDependencyProperty read get_UriSourceProperty;

    // -> Imaging_ISvgImageSourceFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISvgImageSource; static; inline;
    class function CreateInstanceWithUriSource(uriSource: IUriRuntimeClass; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISvgImageSource; static; inline;
  end;

  // Windows.UI.Xaml.Media.Imaging.VirtualSurfaceImageSource
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.IVirtualSurfaceImageSource
  // Factory: "Windows.UI.Xaml.Media.Imaging.IVirtualSurfaceImageSourceFactory"
  TImaging_VirtualSurfaceImageSource = class(TWinRTGenericImportF<Imaging_IVirtualSurfaceImageSourceFactory>)
  public
    // -> Imaging_IVirtualSurfaceImageSourceFactory
    class function CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer): Imaging_IVirtualSurfaceImageSource; static; inline;
    class function CreateInstanceWithDimensionsAndOpacity(pixelWidth: Integer; pixelHeight: Integer; isOpaque: Boolean): Imaging_IVirtualSurfaceImageSource; static; inline;
  end;

  // Windows.UI.Xaml.Media.Imaging.WriteableBitmap
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.IWriteableBitmap
  // Factory: "Windows.UI.Xaml.Media.Imaging.IWriteableBitmapFactory"
  TImaging_WriteableBitmap = class(TWinRTGenericImportF<Imaging_IWriteableBitmapFactory>)
  public
    // -> Imaging_IWriteableBitmapFactory
    class function CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer): Imaging_IWriteableBitmap; static; inline;
  end;

  // Windows.UI.Xaml.Media.Imaging.XamlRenderingBackgroundTask
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTask
  // Implements: Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTaskOverrides
  // Factory: "Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTaskFactory"
  // Instantiable: "Imaging_IXamlRenderingBackgroundTask"
  TImaging_XamlRenderingBackgroundTask = class(TWinRTGenericImportFI<Imaging_IXamlRenderingBackgroundTaskFactory, Imaging_IXamlRenderingBackgroundTask>)
  public
    // -> Imaging_IXamlRenderingBackgroundTaskFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_IXamlRenderingBackgroundTask; static; inline;
  end;

  // Windows.UI.Xaml.Media.LineGeometry
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ILineGeometry
  // Statics: "Windows.UI.Xaml.Media.ILineGeometryStatics"
  // Instantiable: "ILineGeometry"
  TLineGeometry = class(TWinRTGenericImportSI<ILineGeometryStatics, ILineGeometry>)
  public
    // -> ILineGeometryStatics
    class function get_StartPointProperty: IDependencyProperty; static; inline;
    class function get_EndPointProperty: IDependencyProperty; static; inline;
    class property EndPointProperty: IDependencyProperty read get_EndPointProperty;
    class property StartPointProperty: IDependencyProperty read get_StartPointProperty;
  end;

  // Windows.UI.Xaml.Media.LineSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ILineSegment
  // Statics: "Windows.UI.Xaml.Media.ILineSegmentStatics"
  // Instantiable: "ILineSegment"
  TLineSegment = class(TWinRTGenericImportSI<ILineSegmentStatics, ILineSegment>)
  public
    // -> ILineSegmentStatics
    class function get_PointProperty: IDependencyProperty; static; inline;
    class property PointProperty: IDependencyProperty read get_PointProperty;
  end;

  // Windows.UI.Xaml.Media.LinearGradientBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ILinearGradientBrush
  // Statics: "Windows.UI.Xaml.Media.ILinearGradientBrushStatics"
  // Factory: "Windows.UI.Xaml.Media.ILinearGradientBrushFactory"
  // Instantiable: "ILinearGradientBrush"
  TLinearGradientBrush = class(TWinRTGenericImportFSI<ILinearGradientBrushFactory, ILinearGradientBrushStatics, ILinearGradientBrush>)
  public
    // -> ILinearGradientBrushStatics
    class function get_StartPointProperty: IDependencyProperty; static; inline;
    class function get_EndPointProperty: IDependencyProperty; static; inline;
    class property EndPointProperty: IDependencyProperty read get_EndPointProperty;
    class property StartPointProperty: IDependencyProperty read get_StartPointProperty;

    // -> ILinearGradientBrushFactory
    class function CreateInstanceWithGradientStopCollectionAndAngle(gradientStopCollection: IVector_1__IGradientStop; angle: Double): ILinearGradientBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.LoadedImageSurface
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ILoadedImageSurface
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.UI.Composition.ICompositionSurface
  // Statics: "Windows.UI.Xaml.Media.ILoadedImageSurfaceStatics"
  TLoadedImageSurface = class(TWinRTGenericImportS<ILoadedImageSurfaceStatics>)
  public
    // -> ILoadedImageSurfaceStatics
    class function StartLoadFromUri(uri: IUriRuntimeClass; desiredMaxSize: TSizeF): ILoadedImageSurface; overload; static; inline;
    class function StartLoadFromUri(uri: IUriRuntimeClass): ILoadedImageSurface; overload; static; inline;
    class function StartLoadFromStream(stream: IRandomAccessStream; desiredMaxSize: TSizeF): ILoadedImageSurface; overload; static; inline;
    class function StartLoadFromStream(stream: IRandomAccessStream): ILoadedImageSurface; overload; static; inline;
  end;

  // Windows.UI.Xaml.Media.Projection
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IProjection
  // Factory: "Windows.UI.Xaml.Media.IProjectionFactory"
  // Instantiable: "IProjection"
  TProjection = class(TWinRTGenericImportFI<IProjectionFactory, IProjection>)
  public
    // -> IProjectionFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IProjection; static; inline;
  end;

  // Windows.UI.Xaml.Media.Matrix3DProjection
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IMatrix3DProjection
  // Statics: "Windows.UI.Xaml.Media.IMatrix3DProjectionStatics"
  // Instantiable: "IMatrix3DProjection"
  TMatrix3DProjection = class(TWinRTGenericImportSI<IMatrix3DProjectionStatics, IMatrix3DProjection>)
  public
    // -> IMatrix3DProjectionStatics
    class function get_ProjectionMatrixProperty: IDependencyProperty; static; inline;
    class property ProjectionMatrixProperty: IDependencyProperty read get_ProjectionMatrixProperty;
  end;

  // Windows.UI.Xaml.Media.MatrixHelper
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IMatrixHelper
  // Statics: "Windows.UI.Xaml.Media.IMatrixHelperStatics"
  TMatrixHelper = class(TWinRTGenericImportS<IMatrixHelperStatics>)
  public
    // -> IMatrixHelperStatics
    class function get_Identity: Matrix; static; inline;
    class function FromElements(m11: Double; m12: Double; m21: Double; m22: Double; offsetX: Double; offsetY: Double): Matrix; static; inline;
    class function GetIsIdentity(target: Matrix): Boolean; static; inline;
    class function Transform(target: Matrix; point: TPointF): TPointF; static; inline;
    class property Identity: Matrix read get_Identity;
  end;

  // Windows.UI.Xaml.Media.MatrixTransform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IMatrixTransform
  // Statics: "Windows.UI.Xaml.Media.IMatrixTransformStatics"
  // Instantiable: "IMatrixTransform"
  TMatrixTransform = class(TWinRTGenericImportSI<IMatrixTransformStatics, IMatrixTransform>)
  public
    // -> IMatrixTransformStatics
    class function get_MatrixProperty: IDependencyProperty; static; inline;
    class property MatrixProperty: IDependencyProperty read get_MatrixProperty;
  end;

  // Windows.UI.Xaml.Media.Media3D.Transform3D
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Media3D.ITransform3D
  // Factory: "Windows.UI.Xaml.Media.Media3D.ITransform3DFactory"
  // Instantiable: "Media3D_ITransform3D"
  TMedia3D_Transform3D = class(TWinRTGenericImportFI<Media3D_ITransform3DFactory, Media3D_ITransform3D>)
  public
    // -> Media3D_ITransform3DFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Media3D_ITransform3D; static; inline;
  end;

  // Windows.UI.Xaml.Media.Media3D.CompositeTransform3D
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Media3D.ICompositeTransform3D
  // Statics: "Windows.UI.Xaml.Media.Media3D.ICompositeTransform3DStatics"
  // Instantiable: "Media3D_ICompositeTransform3D"
  TMedia3D_CompositeTransform3D = class(TWinRTGenericImportSI<Media3D_ICompositeTransform3DStatics, Media3D_ICompositeTransform3D>)
  public
    // -> Media3D_ICompositeTransform3DStatics
    class function get_CenterXProperty: IDependencyProperty; static; inline;
    class function get_CenterYProperty: IDependencyProperty; static; inline;
    class function get_CenterZProperty: IDependencyProperty; static; inline;
    class function get_RotationXProperty: IDependencyProperty; static; inline;
    class function get_RotationYProperty: IDependencyProperty; static; inline;
    class function get_RotationZProperty: IDependencyProperty; static; inline;
    class function get_ScaleXProperty: IDependencyProperty; static; inline;
    class function get_ScaleYProperty: IDependencyProperty; static; inline;
    class function get_ScaleZProperty: IDependencyProperty; static; inline;
    class function get_TranslateXProperty: IDependencyProperty; static; inline;
    class function get_TranslateYProperty: IDependencyProperty; static; inline;
    class function get_TranslateZProperty: IDependencyProperty; static; inline;
    class property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    class property CenterYProperty: IDependencyProperty read get_CenterYProperty;
    class property CenterZProperty: IDependencyProperty read get_CenterZProperty;
    class property RotationXProperty: IDependencyProperty read get_RotationXProperty;
    class property RotationYProperty: IDependencyProperty read get_RotationYProperty;
    class property RotationZProperty: IDependencyProperty read get_RotationZProperty;
    class property ScaleXProperty: IDependencyProperty read get_ScaleXProperty;
    class property ScaleYProperty: IDependencyProperty read get_ScaleYProperty;
    class property ScaleZProperty: IDependencyProperty read get_ScaleZProperty;
    class property TranslateXProperty: IDependencyProperty read get_TranslateXProperty;
    class property TranslateYProperty: IDependencyProperty read get_TranslateYProperty;
    class property TranslateZProperty: IDependencyProperty read get_TranslateZProperty;
  end;

  // Windows.UI.Xaml.Media.Media3D.Matrix3DHelper
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Media3D.IMatrix3DHelper
  // Statics: "Windows.UI.Xaml.Media.Media3D.IMatrix3DHelperStatics"
  TMedia3D_Matrix3DHelper = class(TWinRTGenericImportS<Media3D_IMatrix3DHelperStatics>)
  public
    // -> Media3D_IMatrix3DHelperStatics
    class function get_Identity: Media3D_Matrix3D; static; inline;
    class function Multiply(matrix1: Media3D_Matrix3D; matrix2: Media3D_Matrix3D): Media3D_Matrix3D; static; inline;
    class function FromElements(m11: Double; m12: Double; m13: Double; m14: Double; m21: Double; m22: Double; m23: Double; m24: Double; m31: Double; m32: Double; m33: Double; m34: Double; offsetX: Double; offsetY: Double; offsetZ: Double; m44: Double): Media3D_Matrix3D; static; inline;
    class function GetHasInverse(target: Media3D_Matrix3D): Boolean; static; inline;
    class function GetIsIdentity(target: Media3D_Matrix3D): Boolean; static; inline;
    class function Invert(target: Media3D_Matrix3D): Media3D_Matrix3D; static; inline;
    class property Identity: Media3D_Matrix3D read get_Identity;
  end;

  // Windows.UI.Xaml.Media.Media3D.PerspectiveTransform3D
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.Media3D.IPerspectiveTransform3D
  // Statics: "Windows.UI.Xaml.Media.Media3D.IPerspectiveTransform3DStatics"
  // Instantiable: "Media3D_IPerspectiveTransform3D"
  TMedia3D_PerspectiveTransform3D = class(TWinRTGenericImportSI<Media3D_IPerspectiveTransform3DStatics, Media3D_IPerspectiveTransform3D>)
  public
    // -> Media3D_IPerspectiveTransform3DStatics
    class function get_DepthProperty: IDependencyProperty; static; inline;
    class function get_OffsetXProperty: IDependencyProperty; static; inline;
    class function get_OffsetYProperty: IDependencyProperty; static; inline;
    class property DepthProperty: IDependencyProperty read get_DepthProperty;
    class property OffsetXProperty: IDependencyProperty read get_OffsetXProperty;
    class property OffsetYProperty: IDependencyProperty read get_OffsetYProperty;
  end;

  // Windows.UI.Xaml.Media.PartialMediaFailureDetectedEventArgs
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs
  // Implements: Windows.UI.Xaml.Media.IPartialMediaFailureDetectedEventArgs2
  // Instantiable: "IPartialMediaFailureDetectedEventArgs"
  TPartialMediaFailureDetectedEventArgs = class(TWinRTGenericImportI<IPartialMediaFailureDetectedEventArgs>) end;

  // Windows.UI.Xaml.Media.PathFigure
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPathFigure
  // Statics: "Windows.UI.Xaml.Media.IPathFigureStatics"
  // Instantiable: "IPathFigure"
  TPathFigure = class(TWinRTGenericImportSI<IPathFigureStatics, IPathFigure>)
  public
    // -> IPathFigureStatics
    class function get_SegmentsProperty: IDependencyProperty; static; inline;
    class function get_StartPointProperty: IDependencyProperty; static; inline;
    class function get_IsClosedProperty: IDependencyProperty; static; inline;
    class function get_IsFilledProperty: IDependencyProperty; static; inline;
    class property IsClosedProperty: IDependencyProperty read get_IsClosedProperty;
    class property IsFilledProperty: IDependencyProperty read get_IsFilledProperty;
    class property SegmentsProperty: IDependencyProperty read get_SegmentsProperty;
    class property StartPointProperty: IDependencyProperty read get_StartPointProperty;
  end;

  // Windows.UI.Xaml.Media.PathFigureCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathFigure>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathFigure>
  // Instantiable: "IVector_1__IPathFigure"
  TPathFigureCollection = class(TWinRTGenericImportI<IVector_1__IPathFigure>) end;

  // Windows.UI.Xaml.Media.PathGeometry
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPathGeometry
  // Statics: "Windows.UI.Xaml.Media.IPathGeometryStatics"
  // Instantiable: "IPathGeometry"
  TPathGeometry = class(TWinRTGenericImportSI<IPathGeometryStatics, IPathGeometry>)
  public
    // -> IPathGeometryStatics
    class function get_FillRuleProperty: IDependencyProperty; static; inline;
    class function get_FiguresProperty: IDependencyProperty; static; inline;
    class property FiguresProperty: IDependencyProperty read get_FiguresProperty;
    class property FillRuleProperty: IDependencyProperty read get_FillRuleProperty;
  end;

  // Windows.UI.Xaml.Media.PathSegmentCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.IPathSegment>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.IPathSegment>
  // Instantiable: "IVector_1__IPathSegment"
  TPathSegmentCollection = class(TWinRTGenericImportI<IVector_1__IPathSegment>) end;

  // Windows.UI.Xaml.Media.PlaneProjection
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPlaneProjection
  // Statics: "Windows.UI.Xaml.Media.IPlaneProjectionStatics"
  // Instantiable: "IPlaneProjection"
  TPlaneProjection = class(TWinRTGenericImportSI<IPlaneProjectionStatics, IPlaneProjection>)
  public
    // -> IPlaneProjectionStatics
    class function get_LocalOffsetXProperty: IDependencyProperty; static; inline;
    class function get_LocalOffsetYProperty: IDependencyProperty; static; inline;
    class function get_LocalOffsetZProperty: IDependencyProperty; static; inline;
    class function get_RotationXProperty: IDependencyProperty; static; inline;
    class function get_RotationYProperty: IDependencyProperty; static; inline;
    class function get_RotationZProperty: IDependencyProperty; static; inline;
    class function get_CenterOfRotationXProperty: IDependencyProperty; static; inline;
    class function get_CenterOfRotationYProperty: IDependencyProperty; static; inline;
    class function get_CenterOfRotationZProperty: IDependencyProperty; static; inline;
    class function get_GlobalOffsetXProperty: IDependencyProperty; static; inline;
    class function get_GlobalOffsetYProperty: IDependencyProperty; static; inline;
    class function get_GlobalOffsetZProperty: IDependencyProperty; static; inline;
    class function get_ProjectionMatrixProperty: IDependencyProperty; static; inline;
    class property CenterOfRotationXProperty: IDependencyProperty read get_CenterOfRotationXProperty;
    class property CenterOfRotationYProperty: IDependencyProperty read get_CenterOfRotationYProperty;
    class property CenterOfRotationZProperty: IDependencyProperty read get_CenterOfRotationZProperty;
    class property GlobalOffsetXProperty: IDependencyProperty read get_GlobalOffsetXProperty;
    class property GlobalOffsetYProperty: IDependencyProperty read get_GlobalOffsetYProperty;
    class property GlobalOffsetZProperty: IDependencyProperty read get_GlobalOffsetZProperty;
    class property LocalOffsetXProperty: IDependencyProperty read get_LocalOffsetXProperty;
    class property LocalOffsetYProperty: IDependencyProperty read get_LocalOffsetYProperty;
    class property LocalOffsetZProperty: IDependencyProperty read get_LocalOffsetZProperty;
    class property ProjectionMatrixProperty: IDependencyProperty read get_ProjectionMatrixProperty;
    class property RotationXProperty: IDependencyProperty read get_RotationXProperty;
    class property RotationYProperty: IDependencyProperty read get_RotationYProperty;
    class property RotationZProperty: IDependencyProperty read get_RotationZProperty;
  end;

  // Windows.UI.Xaml.Media.PointCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.Foundation.Point>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>
  // Instantiable: "IVector_1__Point"
  TPointCollection = class(TWinRTGenericImportI<IVector_1__Point>) end;

  // Windows.UI.Xaml.Media.PolyBezierSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPolyBezierSegment
  // Statics: "Windows.UI.Xaml.Media.IPolyBezierSegmentStatics"
  // Instantiable: "IPolyBezierSegment"
  TPolyBezierSegment = class(TWinRTGenericImportSI<IPolyBezierSegmentStatics, IPolyBezierSegment>)
  public
    // -> IPolyBezierSegmentStatics
    class function get_PointsProperty: IDependencyProperty; static; inline;
    class property PointsProperty: IDependencyProperty read get_PointsProperty;
  end;

  // Windows.UI.Xaml.Media.PolyLineSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPolyLineSegment
  // Statics: "Windows.UI.Xaml.Media.IPolyLineSegmentStatics"
  // Instantiable: "IPolyLineSegment"
  TPolyLineSegment = class(TWinRTGenericImportSI<IPolyLineSegmentStatics, IPolyLineSegment>)
  public
    // -> IPolyLineSegmentStatics
    class function get_PointsProperty: IDependencyProperty; static; inline;
    class property PointsProperty: IDependencyProperty read get_PointsProperty;
  end;

  // Windows.UI.Xaml.Media.PolyQuadraticBezierSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IPolyQuadraticBezierSegment
  // Statics: "Windows.UI.Xaml.Media.IPolyQuadraticBezierSegmentStatics"
  // Instantiable: "IPolyQuadraticBezierSegment"
  TPolyQuadraticBezierSegment = class(TWinRTGenericImportSI<IPolyQuadraticBezierSegmentStatics, IPolyQuadraticBezierSegment>)
  public
    // -> IPolyQuadraticBezierSegmentStatics
    class function get_PointsProperty: IDependencyProperty; static; inline;
    class property PointsProperty: IDependencyProperty read get_PointsProperty;
  end;

  // Windows.UI.Xaml.Media.QuadraticBezierSegment
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IQuadraticBezierSegment
  // Statics: "Windows.UI.Xaml.Media.IQuadraticBezierSegmentStatics"
  // Instantiable: "IQuadraticBezierSegment"
  TQuadraticBezierSegment = class(TWinRTGenericImportSI<IQuadraticBezierSegmentStatics, IQuadraticBezierSegment>)
  public
    // -> IQuadraticBezierSegmentStatics
    class function get_Point1Property: IDependencyProperty; static; inline;
    class function get_Point2Property: IDependencyProperty; static; inline;
    class property Point1Property: IDependencyProperty read get_Point1Property;
    class property Point2Property: IDependencyProperty read get_Point2Property;
  end;

  // Windows.UI.Xaml.Media.RateChangedRoutedEventArgs
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IRateChangedRoutedEventArgs
  // Instantiable: "IRateChangedRoutedEventArgs"
  TRateChangedRoutedEventArgs = class(TWinRTGenericImportI<IRateChangedRoutedEventArgs>) end;

  // Windows.UI.Xaml.Media.RectangleGeometry
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IRectangleGeometry
  // Statics: "Windows.UI.Xaml.Media.IRectangleGeometryStatics"
  // Instantiable: "IRectangleGeometry"
  TRectangleGeometry = class(TWinRTGenericImportSI<IRectangleGeometryStatics, IRectangleGeometry>)
  public
    // -> IRectangleGeometryStatics
    class function get_RectProperty: IDependencyProperty; static; inline;
    class property RectProperty: IDependencyProperty read get_RectProperty;
  end;

  // Windows.UI.Xaml.Media.RevealBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IRevealBrush
  // Statics: "Windows.UI.Xaml.Media.IRevealBrushStatics"
  // Factory: "Windows.UI.Xaml.Media.IRevealBrushFactory"
  // Instantiable: "IRevealBrush"
  TRevealBrush = class(TWinRTGenericImportFSI<IRevealBrushFactory, IRevealBrushStatics, IRevealBrush>)
  public
    // -> IRevealBrushStatics
    class function get_ColorProperty: IDependencyProperty; static; inline;
    class function get_TargetThemeProperty: IDependencyProperty; static; inline;
    class function get_AlwaysUseFallbackProperty: IDependencyProperty; static; inline;
    class function get_StateProperty: IDependencyProperty; static; inline;
    class procedure SetState(element: IUIElement; value: RevealBrushState); static; inline;
    class function GetState(element: IUIElement): RevealBrushState; static; inline;
    class property AlwaysUseFallbackProperty: IDependencyProperty read get_AlwaysUseFallbackProperty;
    class property ColorProperty: IDependencyProperty read get_ColorProperty;
    class property StateProperty: IDependencyProperty read get_StateProperty;
    class property TargetThemeProperty: IDependencyProperty read get_TargetThemeProperty;

    // -> IRevealBrushFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.RevealBackgroundBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IRevealBackgroundBrush
  // Factory: "Windows.UI.Xaml.Media.IRevealBackgroundBrushFactory"
  // Instantiable: "IRevealBackgroundBrush"
  TRevealBackgroundBrush = class(TWinRTGenericImportFI<IRevealBackgroundBrushFactory, IRevealBackgroundBrush>)
  public
    // -> IRevealBackgroundBrushFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBackgroundBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.RevealBorderBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IRevealBorderBrush
  // Factory: "Windows.UI.Xaml.Media.IRevealBorderBrushFactory"
  // Instantiable: "IRevealBorderBrush"
  TRevealBorderBrush = class(TWinRTGenericImportFI<IRevealBorderBrushFactory, IRevealBorderBrush>)
  public
    // -> IRevealBorderBrushFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBorderBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.RotateTransform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IRotateTransform
  // Statics: "Windows.UI.Xaml.Media.IRotateTransformStatics"
  // Instantiable: "IRotateTransform"
  TRotateTransform = class(TWinRTGenericImportSI<IRotateTransformStatics, IRotateTransform>)
  public
    // -> IRotateTransformStatics
    class function get_CenterXProperty: IDependencyProperty; static; inline;
    class function get_CenterYProperty: IDependencyProperty; static; inline;
    class function get_AngleProperty: IDependencyProperty; static; inline;
    class property AngleProperty: IDependencyProperty read get_AngleProperty;
    class property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    class property CenterYProperty: IDependencyProperty read get_CenterYProperty;
  end;

  // Windows.UI.Xaml.Media.ScaleTransform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IScaleTransform
  // Statics: "Windows.UI.Xaml.Media.IScaleTransformStatics"
  // Instantiable: "IScaleTransform"
  TScaleTransform = class(TWinRTGenericImportSI<IScaleTransformStatics, IScaleTransform>)
  public
    // -> IScaleTransformStatics
    class function get_CenterXProperty: IDependencyProperty; static; inline;
    class function get_CenterYProperty: IDependencyProperty; static; inline;
    class function get_ScaleXProperty: IDependencyProperty; static; inline;
    class function get_ScaleYProperty: IDependencyProperty; static; inline;
    class property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    class property CenterYProperty: IDependencyProperty read get_CenterYProperty;
    class property ScaleXProperty: IDependencyProperty read get_ScaleXProperty;
    class property ScaleYProperty: IDependencyProperty read get_ScaleYProperty;
  end;

  // Windows.UI.Xaml.Media.Shadow
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IShadow
  // Factory: "Windows.UI.Xaml.Media.IShadowFactory"
  // Instantiable: "IShadow"
  TShadow = class(TWinRTGenericImportFI<IShadowFactory, IShadow>)
  public
    // -> IShadowFactory
  end;

  // Windows.UI.Xaml.Media.SkewTransform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ISkewTransform
  // Statics: "Windows.UI.Xaml.Media.ISkewTransformStatics"
  // Instantiable: "ISkewTransform"
  TSkewTransform = class(TWinRTGenericImportSI<ISkewTransformStatics, ISkewTransform>)
  public
    // -> ISkewTransformStatics
    class function get_CenterXProperty: IDependencyProperty; static; inline;
    class function get_CenterYProperty: IDependencyProperty; static; inline;
    class function get_AngleXProperty: IDependencyProperty; static; inline;
    class function get_AngleYProperty: IDependencyProperty; static; inline;
    class property AngleXProperty: IDependencyProperty read get_AngleXProperty;
    class property AngleYProperty: IDependencyProperty read get_AngleYProperty;
    class property CenterXProperty: IDependencyProperty read get_CenterXProperty;
    class property CenterYProperty: IDependencyProperty read get_CenterYProperty;
  end;

  // Windows.UI.Xaml.Media.SolidColorBrush
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ISolidColorBrush
  // Statics: "Windows.UI.Xaml.Media.ISolidColorBrushStatics"
  // Factory: "Windows.UI.Xaml.Media.ISolidColorBrushFactory"
  // Instantiable: "ISolidColorBrush"
  TSolidColorBrush = class(TWinRTGenericImportFSI<ISolidColorBrushFactory, ISolidColorBrushStatics, ISolidColorBrush>)
  public
    // -> ISolidColorBrushStatics
    class function get_ColorProperty: IDependencyProperty; static; inline;
    class property ColorProperty: IDependencyProperty read get_ColorProperty;

    // -> ISolidColorBrushFactory
    class function CreateInstanceWithColor(color: Color): ISolidColorBrush; static; inline;
  end;

  // Windows.UI.Xaml.Media.ThemeShadow
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IThemeShadow
  // Factory: "Windows.UI.Xaml.Media.IThemeShadowFactory"
  // Instantiable: "IThemeShadow"
  TThemeShadow = class(TWinRTGenericImportFI<IThemeShadowFactory, IThemeShadow>)
  public
    // -> IThemeShadowFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IThemeShadow; static; inline;
  end;

  // Windows.UI.Xaml.Media.TimelineMarker
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ITimelineMarker
  // Statics: "Windows.UI.Xaml.Media.ITimelineMarkerStatics"
  // Instantiable: "ITimelineMarker"
  TTimelineMarker = class(TWinRTGenericImportSI<ITimelineMarkerStatics, ITimelineMarker>)
  public
    // -> ITimelineMarkerStatics
    class function get_TimeProperty: IDependencyProperty; static; inline;
    class function get_TypeProperty: IDependencyProperty; static; inline;
    class function get_TextProperty: IDependencyProperty; static; inline;
    class property TextProperty: IDependencyProperty read get_TextProperty;
    class property TimeProperty: IDependencyProperty read get_TimeProperty;
    class property TypeProperty: IDependencyProperty read get_TypeProperty;
  end;

  // Windows.UI.Xaml.Media.TimelineMarkerCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITimelineMarker>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITimelineMarker>
  // Instantiable: "IVector_1__ITimelineMarker"
  TTimelineMarkerCollection = class(TWinRTGenericImportI<IVector_1__ITimelineMarker>) end;

  // Windows.UI.Xaml.Media.TimelineMarkerRoutedEventArgs
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ITimelineMarkerRoutedEventArgs
  // Instantiable: "ITimelineMarkerRoutedEventArgs"
  TTimelineMarkerRoutedEventArgs = class(TWinRTGenericImportI<ITimelineMarkerRoutedEventArgs>) end;

  // Windows.UI.Xaml.Media.TransformCollection
  // Explicitly imported
  // Implements: Windows.Foundation.Collections.IVector`1<Windows.UI.Xaml.Media.ITransform>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.UI.Xaml.Media.ITransform>
  // Instantiable: "IVector_1__ITransform"
  TTransformCollection = class(TWinRTGenericImportI<IVector_1__ITransform>) end;

  // Windows.UI.Xaml.Media.TransformGroup
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ITransformGroup
  // Statics: "Windows.UI.Xaml.Media.ITransformGroupStatics"
  // Instantiable: "ITransformGroup"
  TTransformGroup = class(TWinRTGenericImportSI<ITransformGroupStatics, ITransformGroup>)
  public
    // -> ITransformGroupStatics
    class function get_ChildrenProperty: IDependencyProperty; static; inline;
    class property ChildrenProperty: IDependencyProperty read get_ChildrenProperty;
  end;

  // Windows.UI.Xaml.Media.TranslateTransform
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.ITranslateTransform
  // Statics: "Windows.UI.Xaml.Media.ITranslateTransformStatics"
  // Instantiable: "ITranslateTransform"
  TTranslateTransform = class(TWinRTGenericImportSI<ITranslateTransformStatics, ITranslateTransform>)
  public
    // -> ITranslateTransformStatics
    class function get_XProperty: IDependencyProperty; static; inline;
    class function get_YProperty: IDependencyProperty; static; inline;
    class property XProperty: IDependencyProperty read get_XProperty;
    class property YProperty: IDependencyProperty read get_YProperty;
  end;

  // Windows.UI.Xaml.Media.VisualTreeHelper
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IVisualTreeHelper
  // Statics: "Windows.UI.Xaml.Media.IVisualTreeHelperStatics"
  // Statics: "Windows.UI.Xaml.Media.IVisualTreeHelperStatics2"
  // Statics: "Windows.UI.Xaml.Media.IVisualTreeHelperStatics3"
  TVisualTreeHelper = class(TWinRTGenericImportS3<IVisualTreeHelperStatics, IVisualTreeHelperStatics2, IVisualTreeHelperStatics3>)
  public
    // -> IVisualTreeHelperStatics
    class function FindElementsInHostCoordinates(intersectingPoint: TPointF; subtree: IUIElement): IIterable_1__IUIElement; overload; static; inline;
    class function FindElementsInHostCoordinates(intersectingRect: TRectF; subtree: IUIElement): IIterable_1__IUIElement; overload; static; inline;
    class function FindElementsInHostCoordinates(intersectingPoint: TPointF; subtree: IUIElement; includeAllElements: Boolean): IIterable_1__IUIElement; overload; static; inline;
    class function FindElementsInHostCoordinates(intersectingRect: TRectF; subtree: IUIElement; includeAllElements: Boolean): IIterable_1__IUIElement; overload; static; inline;
    class function GetChild(reference: IDependencyObject; childIndex: Integer): IDependencyObject; static; inline;
    class function GetChildrenCount(reference: IDependencyObject): Integer; static; inline;
    class function GetParent(reference: IDependencyObject): IDependencyObject; static; inline;
    class procedure DisconnectChildrenRecursive(element: IUIElement); static; inline;

    // -> IVisualTreeHelperStatics2
    class function GetOpenPopups(window: IWindow): IVectorView_1__Primitives_IPopup; static; inline;

    // -> IVisualTreeHelperStatics3
    class function GetOpenPopupsForXamlRoot(xamlRoot: IXamlRoot): IVectorView_1__Primitives_IPopup; static; inline;
  end;

  // Windows.UI.Xaml.Media.XamlLight
  // Explicitly imported
  // Implements: Windows.UI.Xaml.Media.IXamlLight
  // Implements: Windows.UI.Xaml.Media.IXamlLightProtected
  // Implements: Windows.UI.Xaml.Media.IXamlLightOverrides
  // Statics: "Windows.UI.Xaml.Media.IXamlLightStatics"
  // Factory: "Windows.UI.Xaml.Media.IXamlLightFactory"
  // Instantiable: "IXamlLight"
  TXamlLight = class(TWinRTGenericImportFSI<IXamlLightFactory, IXamlLightStatics, IXamlLight>)
  public
    // -> IXamlLightStatics
    class procedure AddTargetElement(lightId: HSTRING; element: IUIElement); static; inline;
    class procedure RemoveTargetElement(lightId: HSTRING; element: IUIElement); static; inline;
    class procedure AddTargetBrush(lightId: HSTRING; brush: IBrush); static; inline;
    class procedure RemoveTargetBrush(lightId: HSTRING; brush: IBrush); static; inline;

    // -> IXamlLightFactory
    class function CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IXamlLight; static; inline;
  end;

implementation

{ TBrush }

class function TBrush.get_OpacityProperty: IDependencyProperty;
begin
  Result := Statics.get_OpacityProperty;
end;

class function TBrush.get_TransformProperty: IDependencyProperty;
begin
  Result := Statics.get_TransformProperty;
end;

class function TBrush.get_RelativeTransformProperty: IDependencyProperty;
begin
  Result := Statics.get_RelativeTransformProperty;
end;

// Factories for : "Brush"
// Factory: "Windows.UI.Xaml.Media.IBrushFactory"
// -> IBrushFactory

class function TBrush.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IBrush;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TTileBrush }

class function TTileBrush.get_AlignmentXProperty: IDependencyProperty;
begin
  Result := Statics.get_AlignmentXProperty;
end;

class function TTileBrush.get_AlignmentYProperty: IDependencyProperty;
begin
  Result := Statics.get_AlignmentYProperty;
end;

class function TTileBrush.get_StretchProperty: IDependencyProperty;
begin
  Result := Statics.get_StretchProperty;
end;

// Factories for : "TileBrush"
// Factory: "Windows.UI.Xaml.Media.ITileBrushFactory"
// -> ITileBrushFactory

class function TTileBrush.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): ITileBrush;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TXamlCompositionBrushBase }

class function TXamlCompositionBrushBase.get_FallbackColorProperty: IDependencyProperty;
begin
  Result := Statics.get_FallbackColorProperty;
end;

// Factories for : "XamlCompositionBrushBase"
// Factory: "Windows.UI.Xaml.Media.IXamlCompositionBrushBaseFactory"
// -> IXamlCompositionBrushBaseFactory

class function TXamlCompositionBrushBase.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IXamlCompositionBrushBase;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAcrylicBrush }

class function TAcrylicBrush.get_BackgroundSourceProperty: IDependencyProperty;
begin
  Result := Statics.get_BackgroundSourceProperty;
end;

class function TAcrylicBrush.get_TintColorProperty: IDependencyProperty;
begin
  Result := Statics.get_TintColorProperty;
end;

class function TAcrylicBrush.get_TintOpacityProperty: IDependencyProperty;
begin
  Result := Statics.get_TintOpacityProperty;
end;

class function TAcrylicBrush.get_TintTransitionDurationProperty: IDependencyProperty;
begin
  Result := Statics.get_TintTransitionDurationProperty;
end;

class function TAcrylicBrush.get_AlwaysUseFallbackProperty: IDependencyProperty;
begin
  Result := Statics.get_AlwaysUseFallbackProperty;
end;


class function TAcrylicBrush.get_TintLuminosityOpacityProperty: IDependencyProperty;
begin
  Result := Statics2.get_TintLuminosityOpacityProperty;
end;

// Factories for : "AcrylicBrush"
// Factory: "Windows.UI.Xaml.Media.IAcrylicBrushFactory"
// -> IAcrylicBrushFactory

class function TAcrylicBrush.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IAcrylicBrush;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_Transition }
// Factories for : "Animation_Transition"
// Factory: "Windows.UI.Xaml.Media.Animation.ITransitionFactory"
// -> Animation_ITransitionFactory


{ TAnimation_AddDeleteThemeTransition }

{ TAnimation_EasingFunctionBase }

class function TAnimation_EasingFunctionBase.get_EasingModeProperty: IDependencyProperty;
begin
  Result := Statics.get_EasingModeProperty;
end;

// Factories for : "Animation_EasingFunctionBase"
// Factory: "Windows.UI.Xaml.Media.Animation.IEasingFunctionBaseFactory"
// -> Animation_IEasingFunctionBaseFactory


{ TAnimation_BackEase }

class function TAnimation_BackEase.get_AmplitudeProperty: IDependencyProperty;
begin
  Result := Statics.get_AmplitudeProperty;
end;


{ TAnimation_ConnectedAnimationConfiguration }
// Factories for : "Animation_ConnectedAnimationConfiguration"
// Factory: "Windows.UI.Xaml.Media.Animation.IConnectedAnimationConfigurationFactory"
// -> Animation_IConnectedAnimationConfigurationFactory


{ TAnimation_BasicConnectedAnimationConfiguration }
// Factories for : "Animation_BasicConnectedAnimationConfiguration"
// Factory: "Windows.UI.Xaml.Media.Animation.IBasicConnectedAnimationConfigurationFactory"
// -> Animation_IBasicConnectedAnimationConfigurationFactory

class function TAnimation_BasicConnectedAnimationConfiguration.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IBasicConnectedAnimationConfiguration;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_BeginStoryboard }

class function TAnimation_BeginStoryboard.get_StoryboardProperty: IDependencyProperty;
begin
  Result := Statics.get_StoryboardProperty;
end;


{ TAnimation_BounceEase }

class function TAnimation_BounceEase.get_BouncesProperty: IDependencyProperty;
begin
  Result := Statics.get_BouncesProperty;
end;

class function TAnimation_BounceEase.get_BouncinessProperty: IDependencyProperty;
begin
  Result := Statics.get_BouncinessProperty;
end;


{ TAnimation_CircleEase }

{ TAnimation_Timeline }

class function TAnimation_Timeline.get_AllowDependentAnimations: Boolean;
begin
  Result := Statics.get_AllowDependentAnimations;
end;

class procedure TAnimation_Timeline.put_AllowDependentAnimations(value: Boolean);
begin
  Statics.put_AllowDependentAnimations(value);
end;

class function TAnimation_Timeline.get_AutoReverseProperty: IDependencyProperty;
begin
  Result := Statics.get_AutoReverseProperty;
end;

class function TAnimation_Timeline.get_BeginTimeProperty: IDependencyProperty;
begin
  Result := Statics.get_BeginTimeProperty;
end;

class function TAnimation_Timeline.get_DurationProperty: IDependencyProperty;
begin
  Result := Statics.get_DurationProperty;
end;

class function TAnimation_Timeline.get_SpeedRatioProperty: IDependencyProperty;
begin
  Result := Statics.get_SpeedRatioProperty;
end;

class function TAnimation_Timeline.get_FillBehaviorProperty: IDependencyProperty;
begin
  Result := Statics.get_FillBehaviorProperty;
end;

class function TAnimation_Timeline.get_RepeatBehaviorProperty: IDependencyProperty;
begin
  Result := Statics.get_RepeatBehaviorProperty;
end;

// Factories for : "Animation_Timeline"
// Factory: "Windows.UI.Xaml.Media.Animation.ITimelineFactory"
// -> Animation_ITimelineFactory

class function TAnimation_Timeline.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_ITimeline;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_ColorAnimation }

class function TAnimation_ColorAnimation.get_FromProperty: IDependencyProperty;
begin
  Result := Statics.get_FromProperty;
end;

class function TAnimation_ColorAnimation.get_ToProperty: IDependencyProperty;
begin
  Result := Statics.get_ToProperty;
end;

class function TAnimation_ColorAnimation.get_ByProperty: IDependencyProperty;
begin
  Result := Statics.get_ByProperty;
end;

class function TAnimation_ColorAnimation.get_EasingFunctionProperty: IDependencyProperty;
begin
  Result := Statics.get_EasingFunctionProperty;
end;

class function TAnimation_ColorAnimation.get_EnableDependentAnimationProperty: IDependencyProperty;
begin
  Result := Statics.get_EnableDependentAnimationProperty;
end;


{ TAnimation_ColorAnimationUsingKeyFrames }

class function TAnimation_ColorAnimationUsingKeyFrames.get_EnableDependentAnimationProperty: IDependencyProperty;
begin
  Result := Statics.get_EnableDependentAnimationProperty;
end;


{ TAnimation_ColorKeyFrame }

class function TAnimation_ColorKeyFrame.get_ValueProperty: IDependencyProperty;
begin
  Result := Statics.get_ValueProperty;
end;

class function TAnimation_ColorKeyFrame.get_KeyTimeProperty: IDependencyProperty;
begin
  Result := Statics.get_KeyTimeProperty;
end;

// Factories for : "Animation_ColorKeyFrame"
// Factory: "Windows.UI.Xaml.Media.Animation.IColorKeyFrameFactory"
// -> Animation_IColorKeyFrameFactory

class function TAnimation_ColorKeyFrame.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IColorKeyFrame;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_ColorKeyFrameCollection }

{ TAnimation_NavigationTransitionInfo }
// Factories for : "Animation_NavigationTransitionInfo"
// Factory: "Windows.UI.Xaml.Media.Animation.INavigationTransitionInfoFactory"
// -> Animation_INavigationTransitionInfoFactory

class function TAnimation_NavigationTransitionInfo.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_INavigationTransitionInfo;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_CommonNavigationTransitionInfo }

class function TAnimation_CommonNavigationTransitionInfo.get_IsStaggeringEnabledProperty: IDependencyProperty;
begin
  Result := Statics.get_IsStaggeringEnabledProperty;
end;

class function TAnimation_CommonNavigationTransitionInfo.get_IsStaggerElementProperty: IDependencyProperty;
begin
  Result := Statics.get_IsStaggerElementProperty;
end;

class function TAnimation_CommonNavigationTransitionInfo.GetIsStaggerElement(element: IUIElement): Boolean;
begin
  Result := Statics.GetIsStaggerElement(element);
end;

class procedure TAnimation_CommonNavigationTransitionInfo.SetIsStaggerElement(element: IUIElement; value: Boolean);
begin
  Statics.SetIsStaggerElement(element, value);
end;


{ TAnimation_ConnectedAnimationService }

class function TAnimation_ConnectedAnimationService.GetForCurrentView: Animation_IConnectedAnimationService;
begin
  Result := Statics.GetForCurrentView;
end;


{ TAnimation_ContentThemeTransition }

class function TAnimation_ContentThemeTransition.get_HorizontalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_HorizontalOffsetProperty;
end;

class function TAnimation_ContentThemeTransition.get_VerticalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_VerticalOffsetProperty;
end;


{ TAnimation_ContinuumNavigationTransitionInfo }

class function TAnimation_ContinuumNavigationTransitionInfo.get_ExitElementProperty: IDependencyProperty;
begin
  Result := Statics.get_ExitElementProperty;
end;

class function TAnimation_ContinuumNavigationTransitionInfo.get_IsEntranceElementProperty: IDependencyProperty;
begin
  Result := Statics.get_IsEntranceElementProperty;
end;

class function TAnimation_ContinuumNavigationTransitionInfo.GetIsEntranceElement(element: IUIElement): Boolean;
begin
  Result := Statics.GetIsEntranceElement(element);
end;

class procedure TAnimation_ContinuumNavigationTransitionInfo.SetIsEntranceElement(element: IUIElement; value: Boolean);
begin
  Statics.SetIsEntranceElement(element, value);
end;

class function TAnimation_ContinuumNavigationTransitionInfo.get_IsExitElementProperty: IDependencyProperty;
begin
  Result := Statics.get_IsExitElementProperty;
end;

class function TAnimation_ContinuumNavigationTransitionInfo.GetIsExitElement(element: IUIElement): Boolean;
begin
  Result := Statics.GetIsExitElement(element);
end;

class procedure TAnimation_ContinuumNavigationTransitionInfo.SetIsExitElement(element: IUIElement; value: Boolean);
begin
  Statics.SetIsExitElement(element, value);
end;

class function TAnimation_ContinuumNavigationTransitionInfo.get_ExitElementContainerProperty: IDependencyProperty;
begin
  Result := Statics.get_ExitElementContainerProperty;
end;

class function TAnimation_ContinuumNavigationTransitionInfo.GetExitElementContainer(element: IListViewBase): Boolean;
begin
  Result := Statics.GetExitElementContainer(element);
end;

class procedure TAnimation_ContinuumNavigationTransitionInfo.SetExitElementContainer(element: IListViewBase; value: Boolean);
begin
  Statics.SetExitElementContainer(element, value);
end;


{ TAnimation_CubicEase }

{ TAnimation_DirectConnectedAnimationConfiguration }
// Factories for : "Animation_DirectConnectedAnimationConfiguration"
// Factory: "Windows.UI.Xaml.Media.Animation.IDirectConnectedAnimationConfigurationFactory"
// -> Animation_IDirectConnectedAnimationConfigurationFactory

class function TAnimation_DirectConnectedAnimationConfiguration.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IDirectConnectedAnimationConfiguration;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_DiscreteColorKeyFrame }

{ TAnimation_DoubleKeyFrame }

class function TAnimation_DoubleKeyFrame.get_ValueProperty: IDependencyProperty;
begin
  Result := Statics.get_ValueProperty;
end;

class function TAnimation_DoubleKeyFrame.get_KeyTimeProperty: IDependencyProperty;
begin
  Result := Statics.get_KeyTimeProperty;
end;

// Factories for : "Animation_DoubleKeyFrame"
// Factory: "Windows.UI.Xaml.Media.Animation.IDoubleKeyFrameFactory"
// -> Animation_IDoubleKeyFrameFactory

class function TAnimation_DoubleKeyFrame.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IDoubleKeyFrame;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_DiscreteDoubleKeyFrame }

{ TAnimation_ObjectKeyFrame }

class function TAnimation_ObjectKeyFrame.get_ValueProperty: IDependencyProperty;
begin
  Result := Statics.get_ValueProperty;
end;

class function TAnimation_ObjectKeyFrame.get_KeyTimeProperty: IDependencyProperty;
begin
  Result := Statics.get_KeyTimeProperty;
end;

// Factories for : "Animation_ObjectKeyFrame"
// Factory: "Windows.UI.Xaml.Media.Animation.IObjectKeyFrameFactory"
// -> Animation_IObjectKeyFrameFactory

class function TAnimation_ObjectKeyFrame.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IObjectKeyFrame;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_DiscreteObjectKeyFrame }

{ TAnimation_PointKeyFrame }

class function TAnimation_PointKeyFrame.get_ValueProperty: IDependencyProperty;
begin
  Result := Statics.get_ValueProperty;
end;

class function TAnimation_PointKeyFrame.get_KeyTimeProperty: IDependencyProperty;
begin
  Result := Statics.get_KeyTimeProperty;
end;

// Factories for : "Animation_PointKeyFrame"
// Factory: "Windows.UI.Xaml.Media.Animation.IPointKeyFrameFactory"
// -> Animation_IPointKeyFrameFactory

class function TAnimation_PointKeyFrame.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IPointKeyFrame;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_DiscretePointKeyFrame }

{ TAnimation_DoubleAnimation }

class function TAnimation_DoubleAnimation.get_FromProperty: IDependencyProperty;
begin
  Result := Statics.get_FromProperty;
end;

class function TAnimation_DoubleAnimation.get_ToProperty: IDependencyProperty;
begin
  Result := Statics.get_ToProperty;
end;

class function TAnimation_DoubleAnimation.get_ByProperty: IDependencyProperty;
begin
  Result := Statics.get_ByProperty;
end;

class function TAnimation_DoubleAnimation.get_EasingFunctionProperty: IDependencyProperty;
begin
  Result := Statics.get_EasingFunctionProperty;
end;

class function TAnimation_DoubleAnimation.get_EnableDependentAnimationProperty: IDependencyProperty;
begin
  Result := Statics.get_EnableDependentAnimationProperty;
end;


{ TAnimation_DoubleAnimationUsingKeyFrames }

class function TAnimation_DoubleAnimationUsingKeyFrames.get_EnableDependentAnimationProperty: IDependencyProperty;
begin
  Result := Statics.get_EnableDependentAnimationProperty;
end;


{ TAnimation_DoubleKeyFrameCollection }

{ TAnimation_DragItemThemeAnimation }

class function TAnimation_DragItemThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;


{ TAnimation_DragOverThemeAnimation }

class function TAnimation_DragOverThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;

class function TAnimation_DragOverThemeAnimation.get_ToOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_ToOffsetProperty;
end;

class function TAnimation_DragOverThemeAnimation.get_DirectionProperty: IDependencyProperty;
begin
  Result := Statics.get_DirectionProperty;
end;


{ TAnimation_DrillInNavigationTransitionInfo }

{ TAnimation_DrillInThemeAnimation }

class function TAnimation_DrillInThemeAnimation.get_EntranceTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_EntranceTargetNameProperty;
end;

class function TAnimation_DrillInThemeAnimation.get_EntranceTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_EntranceTargetProperty;
end;

class function TAnimation_DrillInThemeAnimation.get_ExitTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_ExitTargetNameProperty;
end;

class function TAnimation_DrillInThemeAnimation.get_ExitTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_ExitTargetProperty;
end;


{ TAnimation_DrillOutThemeAnimation }

class function TAnimation_DrillOutThemeAnimation.get_EntranceTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_EntranceTargetNameProperty;
end;

class function TAnimation_DrillOutThemeAnimation.get_EntranceTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_EntranceTargetProperty;
end;

class function TAnimation_DrillOutThemeAnimation.get_ExitTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_ExitTargetNameProperty;
end;

class function TAnimation_DrillOutThemeAnimation.get_ExitTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_ExitTargetProperty;
end;


{ TAnimation_DropTargetItemThemeAnimation }

class function TAnimation_DropTargetItemThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;


{ TAnimation_EasingColorKeyFrame }

class function TAnimation_EasingColorKeyFrame.get_EasingFunctionProperty: IDependencyProperty;
begin
  Result := Statics.get_EasingFunctionProperty;
end;


{ TAnimation_EasingDoubleKeyFrame }

class function TAnimation_EasingDoubleKeyFrame.get_EasingFunctionProperty: IDependencyProperty;
begin
  Result := Statics.get_EasingFunctionProperty;
end;


{ TAnimation_EasingPointKeyFrame }

class function TAnimation_EasingPointKeyFrame.get_EasingFunctionProperty: IDependencyProperty;
begin
  Result := Statics.get_EasingFunctionProperty;
end;


{ TAnimation_EdgeUIThemeTransition }

class function TAnimation_EdgeUIThemeTransition.get_EdgeProperty: IDependencyProperty;
begin
  Result := Statics.get_EdgeProperty;
end;


{ TAnimation_ElasticEase }

class function TAnimation_ElasticEase.get_OscillationsProperty: IDependencyProperty;
begin
  Result := Statics.get_OscillationsProperty;
end;

class function TAnimation_ElasticEase.get_SpringinessProperty: IDependencyProperty;
begin
  Result := Statics.get_SpringinessProperty;
end;


{ TAnimation_EntranceNavigationTransitionInfo }

class function TAnimation_EntranceNavigationTransitionInfo.get_IsTargetElementProperty: IDependencyProperty;
begin
  Result := Statics.get_IsTargetElementProperty;
end;

class function TAnimation_EntranceNavigationTransitionInfo.GetIsTargetElement(element: IUIElement): Boolean;
begin
  Result := Statics.GetIsTargetElement(element);
end;

class procedure TAnimation_EntranceNavigationTransitionInfo.SetIsTargetElement(element: IUIElement; value: Boolean);
begin
  Statics.SetIsTargetElement(element, value);
end;


{ TAnimation_EntranceThemeTransition }

class function TAnimation_EntranceThemeTransition.get_FromHorizontalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromHorizontalOffsetProperty;
end;

class function TAnimation_EntranceThemeTransition.get_FromVerticalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromVerticalOffsetProperty;
end;

class function TAnimation_EntranceThemeTransition.get_IsStaggeringEnabledProperty: IDependencyProperty;
begin
  Result := Statics.get_IsStaggeringEnabledProperty;
end;


{ TAnimation_ExponentialEase }

class function TAnimation_ExponentialEase.get_ExponentProperty: IDependencyProperty;
begin
  Result := Statics.get_ExponentProperty;
end;


{ TAnimation_FadeInThemeAnimation }

class function TAnimation_FadeInThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;


{ TAnimation_FadeOutThemeAnimation }

class function TAnimation_FadeOutThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;


{ TAnimation_GravityConnectedAnimationConfiguration }
// Factories for : "Animation_GravityConnectedAnimationConfiguration"
// Factory: "Windows.UI.Xaml.Media.Animation.IGravityConnectedAnimationConfigurationFactory"
// -> Animation_IGravityConnectedAnimationConfigurationFactory

class function TAnimation_GravityConnectedAnimationConfiguration.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Animation_IGravityConnectedAnimationConfiguration;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TAnimation_KeySpline }

{ TAnimation_KeyTimeHelper }

class function TAnimation_KeyTimeHelper.FromTimeSpan(timeSpan: TimeSpan): Animation_KeyTime;
begin
  Result := Statics.FromTimeSpan(timeSpan);
end;


{ TAnimation_LinearColorKeyFrame }

{ TAnimation_LinearDoubleKeyFrame }

{ TAnimation_LinearPointKeyFrame }

{ TAnimation_NavigationThemeTransition }

class function TAnimation_NavigationThemeTransition.get_DefaultNavigationTransitionInfoProperty: IDependencyProperty;
begin
  Result := Statics.get_DefaultNavigationTransitionInfoProperty;
end;


{ TAnimation_ObjectAnimationUsingKeyFrames }

class function TAnimation_ObjectAnimationUsingKeyFrames.get_EnableDependentAnimationProperty: IDependencyProperty;
begin
  Result := Statics.get_EnableDependentAnimationProperty;
end;


{ TAnimation_ObjectKeyFrameCollection }

{ TAnimation_PaneThemeTransition }

class function TAnimation_PaneThemeTransition.get_EdgeProperty: IDependencyProperty;
begin
  Result := Statics.get_EdgeProperty;
end;


{ TAnimation_PointAnimation }

class function TAnimation_PointAnimation.get_FromProperty: IDependencyProperty;
begin
  Result := Statics.get_FromProperty;
end;

class function TAnimation_PointAnimation.get_ToProperty: IDependencyProperty;
begin
  Result := Statics.get_ToProperty;
end;

class function TAnimation_PointAnimation.get_ByProperty: IDependencyProperty;
begin
  Result := Statics.get_ByProperty;
end;

class function TAnimation_PointAnimation.get_EasingFunctionProperty: IDependencyProperty;
begin
  Result := Statics.get_EasingFunctionProperty;
end;

class function TAnimation_PointAnimation.get_EnableDependentAnimationProperty: IDependencyProperty;
begin
  Result := Statics.get_EnableDependentAnimationProperty;
end;


{ TAnimation_PointAnimationUsingKeyFrames }

class function TAnimation_PointAnimationUsingKeyFrames.get_EnableDependentAnimationProperty: IDependencyProperty;
begin
  Result := Statics.get_EnableDependentAnimationProperty;
end;


{ TAnimation_PointKeyFrameCollection }

{ TAnimation_PointerDownThemeAnimation }

class function TAnimation_PointerDownThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;


{ TAnimation_PointerUpThemeAnimation }

class function TAnimation_PointerUpThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;


{ TAnimation_PopInThemeAnimation }

class function TAnimation_PopInThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;

class function TAnimation_PopInThemeAnimation.get_FromHorizontalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromHorizontalOffsetProperty;
end;

class function TAnimation_PopInThemeAnimation.get_FromVerticalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromVerticalOffsetProperty;
end;


{ TAnimation_PopOutThemeAnimation }

class function TAnimation_PopOutThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;


{ TAnimation_PopupThemeTransition }

class function TAnimation_PopupThemeTransition.get_FromHorizontalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromHorizontalOffsetProperty;
end;

class function TAnimation_PopupThemeTransition.get_FromVerticalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromVerticalOffsetProperty;
end;


{ TAnimation_PowerEase }

class function TAnimation_PowerEase.get_PowerProperty: IDependencyProperty;
begin
  Result := Statics.get_PowerProperty;
end;


{ TAnimation_QuadraticEase }

{ TAnimation_QuarticEase }

{ TAnimation_QuinticEase }

{ TAnimation_ReorderThemeTransition }

{ TAnimation_RepeatBehaviorHelper }

class function TAnimation_RepeatBehaviorHelper.get_Forever: Animation_RepeatBehavior;
begin
  Result := Statics.get_Forever;
end;

class function TAnimation_RepeatBehaviorHelper.FromCount(count: Double): Animation_RepeatBehavior;
begin
  Result := Statics.FromCount(count);
end;

class function TAnimation_RepeatBehaviorHelper.FromDuration(duration: TimeSpan): Animation_RepeatBehavior;
begin
  Result := Statics.FromDuration(duration);
end;

class function TAnimation_RepeatBehaviorHelper.GetHasCount(target: Animation_RepeatBehavior): Boolean;
begin
  Result := Statics.GetHasCount(target);
end;

class function TAnimation_RepeatBehaviorHelper.GetHasDuration(target: Animation_RepeatBehavior): Boolean;
begin
  Result := Statics.GetHasDuration(target);
end;

class function TAnimation_RepeatBehaviorHelper.Equals(target: Animation_RepeatBehavior; value: Animation_RepeatBehavior): Boolean;
begin
  Result := Statics.Equals(target, value);
end;


{ TAnimation_RepositionThemeAnimation }

class function TAnimation_RepositionThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;

class function TAnimation_RepositionThemeAnimation.get_FromHorizontalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromHorizontalOffsetProperty;
end;

class function TAnimation_RepositionThemeAnimation.get_FromVerticalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromVerticalOffsetProperty;
end;


{ TAnimation_RepositionThemeTransition }

class function TAnimation_RepositionThemeTransition.get_IsStaggeringEnabledProperty: IDependencyProperty;
begin
  Result := Statics.get_IsStaggeringEnabledProperty;
end;


{ TAnimation_SineEase }

{ TAnimation_SlideNavigationTransitionInfo }

class function TAnimation_SlideNavigationTransitionInfo.get_EffectProperty: IDependencyProperty;
begin
  Result := Statics.get_EffectProperty;
end;


{ TAnimation_SplineColorKeyFrame }

class function TAnimation_SplineColorKeyFrame.get_KeySplineProperty: IDependencyProperty;
begin
  Result := Statics.get_KeySplineProperty;
end;


{ TAnimation_SplineDoubleKeyFrame }

class function TAnimation_SplineDoubleKeyFrame.get_KeySplineProperty: IDependencyProperty;
begin
  Result := Statics.get_KeySplineProperty;
end;


{ TAnimation_SplinePointKeyFrame }

class function TAnimation_SplinePointKeyFrame.get_KeySplineProperty: IDependencyProperty;
begin
  Result := Statics.get_KeySplineProperty;
end;


{ TAnimation_SplitCloseThemeAnimation }

class function TAnimation_SplitCloseThemeAnimation.get_OpenedTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_OpenedTargetNameProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_OpenedTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_OpenedTargetProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_ClosedTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_ClosedTargetNameProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_ClosedTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_ClosedTargetProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_ContentTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTargetNameProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_ContentTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTargetProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_OpenedLengthProperty: IDependencyProperty;
begin
  Result := Statics.get_OpenedLengthProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_ClosedLengthProperty: IDependencyProperty;
begin
  Result := Statics.get_ClosedLengthProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_OffsetFromCenterProperty: IDependencyProperty;
begin
  Result := Statics.get_OffsetFromCenterProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_ContentTranslationDirectionProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTranslationDirectionProperty;
end;

class function TAnimation_SplitCloseThemeAnimation.get_ContentTranslationOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTranslationOffsetProperty;
end;


{ TAnimation_SplitOpenThemeAnimation }

class function TAnimation_SplitOpenThemeAnimation.get_OpenedTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_OpenedTargetNameProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_OpenedTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_OpenedTargetProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_ClosedTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_ClosedTargetNameProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_ClosedTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_ClosedTargetProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_ContentTargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTargetNameProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_ContentTargetProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTargetProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_OpenedLengthProperty: IDependencyProperty;
begin
  Result := Statics.get_OpenedLengthProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_ClosedLengthProperty: IDependencyProperty;
begin
  Result := Statics.get_ClosedLengthProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_OffsetFromCenterProperty: IDependencyProperty;
begin
  Result := Statics.get_OffsetFromCenterProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_ContentTranslationDirectionProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTranslationDirectionProperty;
end;

class function TAnimation_SplitOpenThemeAnimation.get_ContentTranslationOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_ContentTranslationOffsetProperty;
end;


{ TAnimation_Storyboard }

class function TAnimation_Storyboard.get_TargetPropertyProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetPropertyProperty;
end;

class function TAnimation_Storyboard.GetTargetProperty(element: Animation_ITimeline): HSTRING;
begin
  Result := Statics.GetTargetProperty(element);
end;

class procedure TAnimation_Storyboard.SetTargetProperty(element: Animation_ITimeline; path: HSTRING);
begin
  Statics.SetTargetProperty(element, path);
end;

class function TAnimation_Storyboard.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;

class function TAnimation_Storyboard.GetTargetName(element: Animation_ITimeline): HSTRING;
begin
  Result := Statics.GetTargetName(element);
end;

class procedure TAnimation_Storyboard.SetTargetName(element: Animation_ITimeline; name: HSTRING);
begin
  Statics.SetTargetName(element, name);
end;

class procedure TAnimation_Storyboard.SetTarget(timeline: Animation_ITimeline; target: IDependencyObject);
begin
  Statics.SetTarget(timeline, target);
end;


{ TAnimation_SuppressNavigationTransitionInfo }

{ TAnimation_SwipeBackThemeAnimation }

class function TAnimation_SwipeBackThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;

class function TAnimation_SwipeBackThemeAnimation.get_FromHorizontalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromHorizontalOffsetProperty;
end;

class function TAnimation_SwipeBackThemeAnimation.get_FromVerticalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_FromVerticalOffsetProperty;
end;


{ TAnimation_SwipeHintThemeAnimation }

class function TAnimation_SwipeHintThemeAnimation.get_TargetNameProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetNameProperty;
end;

class function TAnimation_SwipeHintThemeAnimation.get_ToHorizontalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_ToHorizontalOffsetProperty;
end;

class function TAnimation_SwipeHintThemeAnimation.get_ToVerticalOffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_ToVerticalOffsetProperty;
end;


{ TAnimation_TimelineCollection }

{ TAnimation_TransitionCollection }

{ TPathSegment }
// Factories for : "PathSegment"
// Factory: "Windows.UI.Xaml.Media.IPathSegmentFactory"
// -> IPathSegmentFactory


{ TArcSegment }

class function TArcSegment.get_PointProperty: IDependencyProperty;
begin
  Result := Statics.get_PointProperty;
end;

class function TArcSegment.get_SizeProperty: IDependencyProperty;
begin
  Result := Statics.get_SizeProperty;
end;

class function TArcSegment.get_RotationAngleProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationAngleProperty;
end;

class function TArcSegment.get_IsLargeArcProperty: IDependencyProperty;
begin
  Result := Statics.get_IsLargeArcProperty;
end;

class function TArcSegment.get_SweepDirectionProperty: IDependencyProperty;
begin
  Result := Statics.get_SweepDirectionProperty;
end;


{ TBezierSegment }

class function TBezierSegment.get_Point1Property: IDependencyProperty;
begin
  Result := Statics.get_Point1Property;
end;

class function TBezierSegment.get_Point2Property: IDependencyProperty;
begin
  Result := Statics.get_Point2Property;
end;

class function TBezierSegment.get_Point3Property: IDependencyProperty;
begin
  Result := Statics.get_Point3Property;
end;


{ TCacheMode }
// Factories for : "CacheMode"
// Factory: "Windows.UI.Xaml.Media.ICacheModeFactory"
// -> ICacheModeFactory

class function TCacheMode.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): ICacheMode;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TBitmapCache }

{ TBrushCollection }

{ TGeneralTransform }
// Factories for : "GeneralTransform"
// Factory: "Windows.UI.Xaml.Media.IGeneralTransformFactory"
// -> IGeneralTransformFactory

class function TGeneralTransform.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IGeneralTransform;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TTransform }
// Factories for : "Transform"
// Factory: "Windows.UI.Xaml.Media.ITransformFactory"
// -> ITransformFactory


{ TCompositeTransform }

class function TCompositeTransform.get_CenterXProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterXProperty;
end;

class function TCompositeTransform.get_CenterYProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterYProperty;
end;

class function TCompositeTransform.get_ScaleXProperty: IDependencyProperty;
begin
  Result := Statics.get_ScaleXProperty;
end;

class function TCompositeTransform.get_ScaleYProperty: IDependencyProperty;
begin
  Result := Statics.get_ScaleYProperty;
end;

class function TCompositeTransform.get_SkewXProperty: IDependencyProperty;
begin
  Result := Statics.get_SkewXProperty;
end;

class function TCompositeTransform.get_SkewYProperty: IDependencyProperty;
begin
  Result := Statics.get_SkewYProperty;
end;

class function TCompositeTransform.get_RotationProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationProperty;
end;

class function TCompositeTransform.get_TranslateXProperty: IDependencyProperty;
begin
  Result := Statics.get_TranslateXProperty;
end;

class function TCompositeTransform.get_TranslateYProperty: IDependencyProperty;
begin
  Result := Statics.get_TranslateYProperty;
end;


{ TCompositionTarget }

class function TCompositionTarget.add_Rendering(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_Rendering(handler);
end;

class procedure TCompositionTarget.remove_Rendering(token: EventRegistrationToken);
begin
  Statics.remove_Rendering(token);
end;

class function TCompositionTarget.add_SurfaceContentsLost(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_SurfaceContentsLost(handler);
end;

class procedure TCompositionTarget.remove_SurfaceContentsLost(token: EventRegistrationToken);
begin
  Statics.remove_SurfaceContentsLost(token);
end;


class function TCompositionTarget.add_Rendered(handler: EventHandler_1__IRenderedEventArgs): EventRegistrationToken;
begin
  Result := Statics2.add_Rendered(handler);
end;

class procedure TCompositionTarget.remove_Rendered(token: EventRegistrationToken);
begin
  Statics2.remove_Rendered(token);
end;


{ TDoubleCollection }

{ TGeometry }

class function TGeometry.get_Empty: IGeometry;
begin
  Result := Statics.get_Empty;
end;

class function TGeometry.get_StandardFlatteningTolerance: Double;
begin
  Result := Statics.get_StandardFlatteningTolerance;
end;

class function TGeometry.get_TransformProperty: IDependencyProperty;
begin
  Result := Statics.get_TransformProperty;
end;

// Factories for : "Geometry"
// Factory: "Windows.UI.Xaml.Media.IGeometryFactory"
// -> IGeometryFactory


{ TEllipseGeometry }

class function TEllipseGeometry.get_CenterProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterProperty;
end;

class function TEllipseGeometry.get_RadiusXProperty: IDependencyProperty;
begin
  Result := Statics.get_RadiusXProperty;
end;

class function TEllipseGeometry.get_RadiusYProperty: IDependencyProperty;
begin
  Result := Statics.get_RadiusYProperty;
end;


{ TFontFamily }

class function TFontFamily.get_XamlAutoFontFamily: IFontFamily;
begin
  Result := Statics.get_XamlAutoFontFamily;
end;

// Factories for : "FontFamily"
// Factory: "Windows.UI.Xaml.Media.IFontFamilyFactory"
// -> IFontFamilyFactory

class function TFontFamily.CreateInstanceWithName(familyName: HSTRING; baseInterface: IInspectable; out innerInterface: IInspectable): IFontFamily;
begin
  Result := Factory.CreateInstanceWithName(familyName, baseInterface, innerInterface);
end;


{ TGeometryCollection }

{ TGeometryGroup }

class function TGeometryGroup.get_FillRuleProperty: IDependencyProperty;
begin
  Result := Statics.get_FillRuleProperty;
end;

class function TGeometryGroup.get_ChildrenProperty: IDependencyProperty;
begin
  Result := Statics.get_ChildrenProperty;
end;


{ TGradientBrush }

class function TGradientBrush.get_SpreadMethodProperty: IDependencyProperty;
begin
  Result := Statics.get_SpreadMethodProperty;
end;

class function TGradientBrush.get_MappingModeProperty: IDependencyProperty;
begin
  Result := Statics.get_MappingModeProperty;
end;

class function TGradientBrush.get_ColorInterpolationModeProperty: IDependencyProperty;
begin
  Result := Statics.get_ColorInterpolationModeProperty;
end;

class function TGradientBrush.get_GradientStopsProperty: IDependencyProperty;
begin
  Result := Statics.get_GradientStopsProperty;
end;

// Factories for : "GradientBrush"
// Factory: "Windows.UI.Xaml.Media.IGradientBrushFactory"
// -> IGradientBrushFactory

class function TGradientBrush.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IGradientBrush;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TGradientStop }

class function TGradientStop.get_ColorProperty: IDependencyProperty;
begin
  Result := Statics.get_ColorProperty;
end;

class function TGradientStop.get_OffsetProperty: IDependencyProperty;
begin
  Result := Statics.get_OffsetProperty;
end;


{ TGradientStopCollection }

{ TImageBrush }

class function TImageBrush.get_ImageSourceProperty: IDependencyProperty;
begin
  Result := Statics.get_ImageSourceProperty;
end;


{ TImageSource }
// Factories for : "ImageSource"
// Factory: "Windows.UI.Xaml.Media.IImageSourceFactory"
// -> IImageSourceFactory


{ TImaging_BitmapSource }

class function TImaging_BitmapSource.get_PixelWidthProperty: IDependencyProperty;
begin
  Result := Statics.get_PixelWidthProperty;
end;

class function TImaging_BitmapSource.get_PixelHeightProperty: IDependencyProperty;
begin
  Result := Statics.get_PixelHeightProperty;
end;

// Factories for : "Imaging_BitmapSource"
// Factory: "Windows.UI.Xaml.Media.Imaging.IBitmapSourceFactory"
// -> Imaging_IBitmapSourceFactory

class function TImaging_BitmapSource.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_IBitmapSource;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TImaging_BitmapImage }

class function TImaging_BitmapImage.get_CreateOptionsProperty: IDependencyProperty;
begin
  Result := Statics.get_CreateOptionsProperty;
end;

class function TImaging_BitmapImage.get_UriSourceProperty: IDependencyProperty;
begin
  Result := Statics.get_UriSourceProperty;
end;

class function TImaging_BitmapImage.get_DecodePixelWidthProperty: IDependencyProperty;
begin
  Result := Statics.get_DecodePixelWidthProperty;
end;

class function TImaging_BitmapImage.get_DecodePixelHeightProperty: IDependencyProperty;
begin
  Result := Statics.get_DecodePixelHeightProperty;
end;


class function TImaging_BitmapImage.get_DecodePixelTypeProperty: IDependencyProperty;
begin
  Result := Statics2.get_DecodePixelTypeProperty;
end;


class function TImaging_BitmapImage.get_IsAnimatedBitmapProperty: IDependencyProperty;
begin
  Result := Statics3.get_IsAnimatedBitmapProperty;
end;

class function TImaging_BitmapImage.get_IsPlayingProperty: IDependencyProperty;
begin
  Result := Statics3.get_IsPlayingProperty;
end;

class function TImaging_BitmapImage.get_AutoPlayProperty: IDependencyProperty;
begin
  Result := Statics3.get_AutoPlayProperty;
end;

// Factories for : "Imaging_BitmapImage"
// Factory: "Windows.UI.Xaml.Media.Imaging.IBitmapImageFactory"
// -> Imaging_IBitmapImageFactory

class function TImaging_BitmapImage.CreateInstanceWithUriSource(uriSource: IUriRuntimeClass): Imaging_IBitmapImage;
begin
  Result := Factory.CreateInstanceWithUriSource(uriSource);
end;


{ TImaging_RenderTargetBitmap }

class function TImaging_RenderTargetBitmap.get_PixelWidthProperty: IDependencyProperty;
begin
  Result := Statics.get_PixelWidthProperty;
end;

class function TImaging_RenderTargetBitmap.get_PixelHeightProperty: IDependencyProperty;
begin
  Result := Statics.get_PixelHeightProperty;
end;


{ TImaging_SoftwareBitmapSource }

{ TImaging_SurfaceImageSource }
// Factories for : "Imaging_SurfaceImageSource"
// Factory: "Windows.UI.Xaml.Media.Imaging.ISurfaceImageSourceFactory"
// -> Imaging_ISurfaceImageSourceFactory

class function TImaging_SurfaceImageSource.CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISurfaceImageSource;
begin
  Result := Factory.CreateInstanceWithDimensions(pixelWidth, pixelHeight, baseInterface, innerInterface);
end;

class function TImaging_SurfaceImageSource.CreateInstanceWithDimensionsAndOpacity(pixelWidth: Integer; pixelHeight: Integer; isOpaque: Boolean; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISurfaceImageSource;
begin
  Result := Factory.CreateInstanceWithDimensionsAndOpacity(pixelWidth, pixelHeight, isOpaque, baseInterface, innerInterface);
end;


{ TImaging_SvgImageSource }

class function TImaging_SvgImageSource.get_UriSourceProperty: IDependencyProperty;
begin
  Result := Statics.get_UriSourceProperty;
end;

class function TImaging_SvgImageSource.get_RasterizePixelWidthProperty: IDependencyProperty;
begin
  Result := Statics.get_RasterizePixelWidthProperty;
end;

class function TImaging_SvgImageSource.get_RasterizePixelHeightProperty: IDependencyProperty;
begin
  Result := Statics.get_RasterizePixelHeightProperty;
end;

// Factories for : "Imaging_SvgImageSource"
// Factory: "Windows.UI.Xaml.Media.Imaging.ISvgImageSourceFactory"
// -> Imaging_ISvgImageSourceFactory

class function TImaging_SvgImageSource.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISvgImageSource;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;

class function TImaging_SvgImageSource.CreateInstanceWithUriSource(uriSource: IUriRuntimeClass; baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_ISvgImageSource;
begin
  Result := Factory.CreateInstanceWithUriSource(uriSource, baseInterface, innerInterface);
end;


{ TImaging_VirtualSurfaceImageSource }
// Factories for : "Imaging_VirtualSurfaceImageSource"
// Factory: "Windows.UI.Xaml.Media.Imaging.IVirtualSurfaceImageSourceFactory"
// -> Imaging_IVirtualSurfaceImageSourceFactory

class function TImaging_VirtualSurfaceImageSource.CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer): Imaging_IVirtualSurfaceImageSource;
begin
  Result := Factory.CreateInstanceWithDimensions(pixelWidth, pixelHeight);
end;

class function TImaging_VirtualSurfaceImageSource.CreateInstanceWithDimensionsAndOpacity(pixelWidth: Integer; pixelHeight: Integer; isOpaque: Boolean): Imaging_IVirtualSurfaceImageSource;
begin
  Result := Factory.CreateInstanceWithDimensionsAndOpacity(pixelWidth, pixelHeight, isOpaque);
end;


{ TImaging_WriteableBitmap }
// Factories for : "Imaging_WriteableBitmap"
// Factory: "Windows.UI.Xaml.Media.Imaging.IWriteableBitmapFactory"
// -> Imaging_IWriteableBitmapFactory

class function TImaging_WriteableBitmap.CreateInstanceWithDimensions(pixelWidth: Integer; pixelHeight: Integer): Imaging_IWriteableBitmap;
begin
  Result := Factory.CreateInstanceWithDimensions(pixelWidth, pixelHeight);
end;


{ TImaging_XamlRenderingBackgroundTask }
// Factories for : "Imaging_XamlRenderingBackgroundTask"
// Factory: "Windows.UI.Xaml.Media.Imaging.IXamlRenderingBackgroundTaskFactory"
// -> Imaging_IXamlRenderingBackgroundTaskFactory

class function TImaging_XamlRenderingBackgroundTask.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Imaging_IXamlRenderingBackgroundTask;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TLineGeometry }

class function TLineGeometry.get_StartPointProperty: IDependencyProperty;
begin
  Result := Statics.get_StartPointProperty;
end;

class function TLineGeometry.get_EndPointProperty: IDependencyProperty;
begin
  Result := Statics.get_EndPointProperty;
end;


{ TLineSegment }

class function TLineSegment.get_PointProperty: IDependencyProperty;
begin
  Result := Statics.get_PointProperty;
end;


{ TLinearGradientBrush }

class function TLinearGradientBrush.get_StartPointProperty: IDependencyProperty;
begin
  Result := Statics.get_StartPointProperty;
end;

class function TLinearGradientBrush.get_EndPointProperty: IDependencyProperty;
begin
  Result := Statics.get_EndPointProperty;
end;

// Factories for : "LinearGradientBrush"
// Factory: "Windows.UI.Xaml.Media.ILinearGradientBrushFactory"
// -> ILinearGradientBrushFactory

class function TLinearGradientBrush.CreateInstanceWithGradientStopCollectionAndAngle(gradientStopCollection: IVector_1__IGradientStop; angle: Double): ILinearGradientBrush;
begin
  Result := Factory.CreateInstanceWithGradientStopCollectionAndAngle(gradientStopCollection, angle);
end;


{ TLoadedImageSurface }

class function TLoadedImageSurface.StartLoadFromUri(uri: IUriRuntimeClass; desiredMaxSize: TSizeF): ILoadedImageSurface;
begin
  Result := Statics.StartLoadFromUri(uri, desiredMaxSize);
end;

class function TLoadedImageSurface.StartLoadFromUri(uri: IUriRuntimeClass): ILoadedImageSurface;
begin
  Result := Statics.StartLoadFromUri(uri);
end;

class function TLoadedImageSurface.StartLoadFromStream(stream: IRandomAccessStream; desiredMaxSize: TSizeF): ILoadedImageSurface;
begin
  Result := Statics.StartLoadFromStream(stream, desiredMaxSize);
end;

class function TLoadedImageSurface.StartLoadFromStream(stream: IRandomAccessStream): ILoadedImageSurface;
begin
  Result := Statics.StartLoadFromStream(stream);
end;


{ TProjection }
// Factories for : "Projection"
// Factory: "Windows.UI.Xaml.Media.IProjectionFactory"
// -> IProjectionFactory

class function TProjection.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IProjection;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TMatrix3DProjection }

class function TMatrix3DProjection.get_ProjectionMatrixProperty: IDependencyProperty;
begin
  Result := Statics.get_ProjectionMatrixProperty;
end;


{ TMatrixHelper }

class function TMatrixHelper.get_Identity: Matrix;
begin
  Result := Statics.get_Identity;
end;

class function TMatrixHelper.FromElements(m11: Double; m12: Double; m21: Double; m22: Double; offsetX: Double; offsetY: Double): Matrix;
begin
  Result := Statics.FromElements(m11, m12, m21, m22, offsetX, offsetY);
end;

class function TMatrixHelper.GetIsIdentity(target: Matrix): Boolean;
begin
  Result := Statics.GetIsIdentity(target);
end;

class function TMatrixHelper.Transform(target: Matrix; point: TPointF): TPointF;
begin
  Result := Statics.Transform(target, point);
end;


{ TMatrixTransform }

class function TMatrixTransform.get_MatrixProperty: IDependencyProperty;
begin
  Result := Statics.get_MatrixProperty;
end;


{ TMedia3D_Transform3D }
// Factories for : "Media3D_Transform3D"
// Factory: "Windows.UI.Xaml.Media.Media3D.ITransform3DFactory"
// -> Media3D_ITransform3DFactory

class function TMedia3D_Transform3D.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): Media3D_ITransform3D;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TMedia3D_CompositeTransform3D }

class function TMedia3D_CompositeTransform3D.get_CenterXProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterXProperty;
end;

class function TMedia3D_CompositeTransform3D.get_CenterYProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterYProperty;
end;

class function TMedia3D_CompositeTransform3D.get_CenterZProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterZProperty;
end;

class function TMedia3D_CompositeTransform3D.get_RotationXProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationXProperty;
end;

class function TMedia3D_CompositeTransform3D.get_RotationYProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationYProperty;
end;

class function TMedia3D_CompositeTransform3D.get_RotationZProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationZProperty;
end;

class function TMedia3D_CompositeTransform3D.get_ScaleXProperty: IDependencyProperty;
begin
  Result := Statics.get_ScaleXProperty;
end;

class function TMedia3D_CompositeTransform3D.get_ScaleYProperty: IDependencyProperty;
begin
  Result := Statics.get_ScaleYProperty;
end;

class function TMedia3D_CompositeTransform3D.get_ScaleZProperty: IDependencyProperty;
begin
  Result := Statics.get_ScaleZProperty;
end;

class function TMedia3D_CompositeTransform3D.get_TranslateXProperty: IDependencyProperty;
begin
  Result := Statics.get_TranslateXProperty;
end;

class function TMedia3D_CompositeTransform3D.get_TranslateYProperty: IDependencyProperty;
begin
  Result := Statics.get_TranslateYProperty;
end;

class function TMedia3D_CompositeTransform3D.get_TranslateZProperty: IDependencyProperty;
begin
  Result := Statics.get_TranslateZProperty;
end;


{ TMedia3D_Matrix3DHelper }

class function TMedia3D_Matrix3DHelper.get_Identity: Media3D_Matrix3D;
begin
  Result := Statics.get_Identity;
end;

class function TMedia3D_Matrix3DHelper.Multiply(matrix1: Media3D_Matrix3D; matrix2: Media3D_Matrix3D): Media3D_Matrix3D;
begin
  Result := Statics.Multiply(matrix1, matrix2);
end;

class function TMedia3D_Matrix3DHelper.FromElements(m11: Double; m12: Double; m13: Double; m14: Double; m21: Double; m22: Double; m23: Double; m24: Double; m31: Double; m32: Double; m33: Double; m34: Double; offsetX: Double; offsetY: Double; offsetZ: Double; m44: Double): Media3D_Matrix3D;
begin
  Result := Statics.FromElements(m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34, offsetX, offsetY, offsetZ, m44);
end;

class function TMedia3D_Matrix3DHelper.GetHasInverse(target: Media3D_Matrix3D): Boolean;
begin
  Result := Statics.GetHasInverse(target);
end;

class function TMedia3D_Matrix3DHelper.GetIsIdentity(target: Media3D_Matrix3D): Boolean;
begin
  Result := Statics.GetIsIdentity(target);
end;

class function TMedia3D_Matrix3DHelper.Invert(target: Media3D_Matrix3D): Media3D_Matrix3D;
begin
  Result := Statics.Invert(target);
end;


{ TMedia3D_PerspectiveTransform3D }

class function TMedia3D_PerspectiveTransform3D.get_DepthProperty: IDependencyProperty;
begin
  Result := Statics.get_DepthProperty;
end;

class function TMedia3D_PerspectiveTransform3D.get_OffsetXProperty: IDependencyProperty;
begin
  Result := Statics.get_OffsetXProperty;
end;

class function TMedia3D_PerspectiveTransform3D.get_OffsetYProperty: IDependencyProperty;
begin
  Result := Statics.get_OffsetYProperty;
end;


{ TPartialMediaFailureDetectedEventArgs }

{ TPathFigure }

class function TPathFigure.get_SegmentsProperty: IDependencyProperty;
begin
  Result := Statics.get_SegmentsProperty;
end;

class function TPathFigure.get_StartPointProperty: IDependencyProperty;
begin
  Result := Statics.get_StartPointProperty;
end;

class function TPathFigure.get_IsClosedProperty: IDependencyProperty;
begin
  Result := Statics.get_IsClosedProperty;
end;

class function TPathFigure.get_IsFilledProperty: IDependencyProperty;
begin
  Result := Statics.get_IsFilledProperty;
end;


{ TPathFigureCollection }

{ TPathGeometry }

class function TPathGeometry.get_FillRuleProperty: IDependencyProperty;
begin
  Result := Statics.get_FillRuleProperty;
end;

class function TPathGeometry.get_FiguresProperty: IDependencyProperty;
begin
  Result := Statics.get_FiguresProperty;
end;


{ TPathSegmentCollection }

{ TPlaneProjection }

class function TPlaneProjection.get_LocalOffsetXProperty: IDependencyProperty;
begin
  Result := Statics.get_LocalOffsetXProperty;
end;

class function TPlaneProjection.get_LocalOffsetYProperty: IDependencyProperty;
begin
  Result := Statics.get_LocalOffsetYProperty;
end;

class function TPlaneProjection.get_LocalOffsetZProperty: IDependencyProperty;
begin
  Result := Statics.get_LocalOffsetZProperty;
end;

class function TPlaneProjection.get_RotationXProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationXProperty;
end;

class function TPlaneProjection.get_RotationYProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationYProperty;
end;

class function TPlaneProjection.get_RotationZProperty: IDependencyProperty;
begin
  Result := Statics.get_RotationZProperty;
end;

class function TPlaneProjection.get_CenterOfRotationXProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterOfRotationXProperty;
end;

class function TPlaneProjection.get_CenterOfRotationYProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterOfRotationYProperty;
end;

class function TPlaneProjection.get_CenterOfRotationZProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterOfRotationZProperty;
end;

class function TPlaneProjection.get_GlobalOffsetXProperty: IDependencyProperty;
begin
  Result := Statics.get_GlobalOffsetXProperty;
end;

class function TPlaneProjection.get_GlobalOffsetYProperty: IDependencyProperty;
begin
  Result := Statics.get_GlobalOffsetYProperty;
end;

class function TPlaneProjection.get_GlobalOffsetZProperty: IDependencyProperty;
begin
  Result := Statics.get_GlobalOffsetZProperty;
end;

class function TPlaneProjection.get_ProjectionMatrixProperty: IDependencyProperty;
begin
  Result := Statics.get_ProjectionMatrixProperty;
end;


{ TPointCollection }

{ TPolyBezierSegment }

class function TPolyBezierSegment.get_PointsProperty: IDependencyProperty;
begin
  Result := Statics.get_PointsProperty;
end;


{ TPolyLineSegment }

class function TPolyLineSegment.get_PointsProperty: IDependencyProperty;
begin
  Result := Statics.get_PointsProperty;
end;


{ TPolyQuadraticBezierSegment }

class function TPolyQuadraticBezierSegment.get_PointsProperty: IDependencyProperty;
begin
  Result := Statics.get_PointsProperty;
end;


{ TQuadraticBezierSegment }

class function TQuadraticBezierSegment.get_Point1Property: IDependencyProperty;
begin
  Result := Statics.get_Point1Property;
end;

class function TQuadraticBezierSegment.get_Point2Property: IDependencyProperty;
begin
  Result := Statics.get_Point2Property;
end;


{ TRateChangedRoutedEventArgs }

{ TRectangleGeometry }

class function TRectangleGeometry.get_RectProperty: IDependencyProperty;
begin
  Result := Statics.get_RectProperty;
end;


{ TRevealBrush }

class function TRevealBrush.get_ColorProperty: IDependencyProperty;
begin
  Result := Statics.get_ColorProperty;
end;

class function TRevealBrush.get_TargetThemeProperty: IDependencyProperty;
begin
  Result := Statics.get_TargetThemeProperty;
end;

class function TRevealBrush.get_AlwaysUseFallbackProperty: IDependencyProperty;
begin
  Result := Statics.get_AlwaysUseFallbackProperty;
end;

class function TRevealBrush.get_StateProperty: IDependencyProperty;
begin
  Result := Statics.get_StateProperty;
end;

class procedure TRevealBrush.SetState(element: IUIElement; value: RevealBrushState);
begin
  Statics.SetState(element, value);
end;

class function TRevealBrush.GetState(element: IUIElement): RevealBrushState;
begin
  Result := Statics.GetState(element);
end;

// Factories for : "RevealBrush"
// Factory: "Windows.UI.Xaml.Media.IRevealBrushFactory"
// -> IRevealBrushFactory

class function TRevealBrush.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBrush;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TRevealBackgroundBrush }
// Factories for : "RevealBackgroundBrush"
// Factory: "Windows.UI.Xaml.Media.IRevealBackgroundBrushFactory"
// -> IRevealBackgroundBrushFactory

class function TRevealBackgroundBrush.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBackgroundBrush;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TRevealBorderBrush }
// Factories for : "RevealBorderBrush"
// Factory: "Windows.UI.Xaml.Media.IRevealBorderBrushFactory"
// -> IRevealBorderBrushFactory

class function TRevealBorderBrush.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IRevealBorderBrush;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TRotateTransform }

class function TRotateTransform.get_CenterXProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterXProperty;
end;

class function TRotateTransform.get_CenterYProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterYProperty;
end;

class function TRotateTransform.get_AngleProperty: IDependencyProperty;
begin
  Result := Statics.get_AngleProperty;
end;


{ TScaleTransform }

class function TScaleTransform.get_CenterXProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterXProperty;
end;

class function TScaleTransform.get_CenterYProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterYProperty;
end;

class function TScaleTransform.get_ScaleXProperty: IDependencyProperty;
begin
  Result := Statics.get_ScaleXProperty;
end;

class function TScaleTransform.get_ScaleYProperty: IDependencyProperty;
begin
  Result := Statics.get_ScaleYProperty;
end;


{ TShadow }
// Factories for : "Shadow"
// Factory: "Windows.UI.Xaml.Media.IShadowFactory"
// -> IShadowFactory


{ TSkewTransform }

class function TSkewTransform.get_CenterXProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterXProperty;
end;

class function TSkewTransform.get_CenterYProperty: IDependencyProperty;
begin
  Result := Statics.get_CenterYProperty;
end;

class function TSkewTransform.get_AngleXProperty: IDependencyProperty;
begin
  Result := Statics.get_AngleXProperty;
end;

class function TSkewTransform.get_AngleYProperty: IDependencyProperty;
begin
  Result := Statics.get_AngleYProperty;
end;


{ TSolidColorBrush }

class function TSolidColorBrush.get_ColorProperty: IDependencyProperty;
begin
  Result := Statics.get_ColorProperty;
end;

// Factories for : "SolidColorBrush"
// Factory: "Windows.UI.Xaml.Media.ISolidColorBrushFactory"
// -> ISolidColorBrushFactory

class function TSolidColorBrush.CreateInstanceWithColor(color: Color): ISolidColorBrush;
begin
  Result := Factory.CreateInstanceWithColor(color);
end;


{ TThemeShadow }
// Factories for : "ThemeShadow"
// Factory: "Windows.UI.Xaml.Media.IThemeShadowFactory"
// -> IThemeShadowFactory

class function TThemeShadow.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IThemeShadow;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


{ TTimelineMarker }

class function TTimelineMarker.get_TimeProperty: IDependencyProperty;
begin
  Result := Statics.get_TimeProperty;
end;

class function TTimelineMarker.get_TypeProperty: IDependencyProperty;
begin
  Result := Statics.get_TypeProperty;
end;

class function TTimelineMarker.get_TextProperty: IDependencyProperty;
begin
  Result := Statics.get_TextProperty;
end;


{ TTimelineMarkerCollection }

{ TTimelineMarkerRoutedEventArgs }

{ TTransformCollection }

{ TTransformGroup }

class function TTransformGroup.get_ChildrenProperty: IDependencyProperty;
begin
  Result := Statics.get_ChildrenProperty;
end;


{ TTranslateTransform }

class function TTranslateTransform.get_XProperty: IDependencyProperty;
begin
  Result := Statics.get_XProperty;
end;

class function TTranslateTransform.get_YProperty: IDependencyProperty;
begin
  Result := Statics.get_YProperty;
end;


{ TVisualTreeHelper }

class function TVisualTreeHelper.FindElementsInHostCoordinates(intersectingPoint: TPointF; subtree: IUIElement): IIterable_1__IUIElement;
begin
  Result := Statics.FindElementsInHostCoordinates(intersectingPoint, subtree);
end;

class function TVisualTreeHelper.FindElementsInHostCoordinates(intersectingRect: TRectF; subtree: IUIElement): IIterable_1__IUIElement;
begin
  Result := Statics.FindElementsInHostCoordinates(intersectingRect, subtree);
end;

class function TVisualTreeHelper.FindElementsInHostCoordinates(intersectingPoint: TPointF; subtree: IUIElement; includeAllElements: Boolean): IIterable_1__IUIElement;
begin
  Result := Statics.FindElementsInHostCoordinates(intersectingPoint, subtree, includeAllElements);
end;

class function TVisualTreeHelper.FindElementsInHostCoordinates(intersectingRect: TRectF; subtree: IUIElement; includeAllElements: Boolean): IIterable_1__IUIElement;
begin
  Result := Statics.FindElementsInHostCoordinates(intersectingRect, subtree, includeAllElements);
end;

class function TVisualTreeHelper.GetChild(reference: IDependencyObject; childIndex: Integer): IDependencyObject;
begin
  Result := Statics.GetChild(reference, childIndex);
end;

class function TVisualTreeHelper.GetChildrenCount(reference: IDependencyObject): Integer;
begin
  Result := Statics.GetChildrenCount(reference);
end;

class function TVisualTreeHelper.GetParent(reference: IDependencyObject): IDependencyObject;
begin
  Result := Statics.GetParent(reference);
end;

class procedure TVisualTreeHelper.DisconnectChildrenRecursive(element: IUIElement);
begin
  Statics.DisconnectChildrenRecursive(element);
end;


class function TVisualTreeHelper.GetOpenPopups(window: IWindow): IVectorView_1__Primitives_IPopup;
begin
  Result := Statics2.GetOpenPopups(window);
end;


class function TVisualTreeHelper.GetOpenPopupsForXamlRoot(xamlRoot: IXamlRoot): IVectorView_1__Primitives_IPopup;
begin
  Result := Statics3.GetOpenPopupsForXamlRoot(xamlRoot);
end;


{ TXamlLight }

class procedure TXamlLight.AddTargetElement(lightId: HSTRING; element: IUIElement);
begin
  Statics.AddTargetElement(lightId, element);
end;

class procedure TXamlLight.RemoveTargetElement(lightId: HSTRING; element: IUIElement);
begin
  Statics.RemoveTargetElement(lightId, element);
end;

class procedure TXamlLight.AddTargetBrush(lightId: HSTRING; brush: IBrush);
begin
  Statics.AddTargetBrush(lightId, brush);
end;

class procedure TXamlLight.RemoveTargetBrush(lightId: HSTRING; brush: IBrush);
begin
  Statics.RemoveTargetBrush(lightId, brush);
end;

// Factories for : "XamlLight"
// Factory: "Windows.UI.Xaml.Media.IXamlLightFactory"
// -> IXamlLightFactory

class function TXamlLight.CreateInstance(baseInterface: IInspectable; out innerInterface: IInspectable): IXamlLight;
begin
  Result := Factory.CreateInstance(baseInterface, innerInterface);
end;


end.
