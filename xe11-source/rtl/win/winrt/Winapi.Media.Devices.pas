{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Media.Devices;

{$HPPEMIT NOUSINGNAMESPACE}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$MINENUMSIZE 4}

uses 
  Winapi.Windows, 
  Winapi.WinRT, 
  System.Types, 
  System.Win.WinRT, 
  Winapi.Media.MediaProperties, 
  Winapi.CommonTypes, 
  Winapi.Perception, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type

  // Forward declarations for interfaces

  // Windows.Media.Devices.IMediaDeviceController
  IMediaDeviceController = interface;
  PIMediaDeviceController = ^IMediaDeviceController;

  // Windows.Media.Devices.IAudioDeviceController
  IAudioDeviceController = interface;
  PIAudioDeviceController = ^IAudioDeviceController;

  // Windows.Media.Devices.IMediaDeviceControlCapabilities
  IMediaDeviceControlCapabilities = interface;
  PIMediaDeviceControlCapabilities = ^IMediaDeviceControlCapabilities;

  // Windows.Media.Devices.IMediaDeviceControl
  IMediaDeviceControl = interface;
  PIMediaDeviceControl = ^IMediaDeviceControl;

  // Windows.Media.Devices.IVideoDeviceController
  IVideoDeviceController = interface;
  PIVideoDeviceController = ^IVideoDeviceController;

  // Windows.Media.Devices.Core.ICameraIntrinsics
  Core_ICameraIntrinsics = interface;
  PCore_ICameraIntrinsics = ^Core_ICameraIntrinsics;

  // Windows.Foundation.IReference`1<Windows.Media.Devices.CaptureSceneMode>
  IReference_1__CaptureSceneMode = interface;
  PIReference_1__CaptureSceneMode = ^IReference_1__CaptureSceneMode;

  // Windows.Media.Devices.Core.IDepthCorrelatedCoordinateMapper
  Core_IDepthCorrelatedCoordinateMapper = interface;
  PCore_IDepthCorrelatedCoordinateMapper = ^Core_IDepthCorrelatedCoordinateMapper;

  // Windows.Foundation.IReference`1<Windows.Media.Devices.MediaCaptureFocusState>
  IReference_1__MediaCaptureFocusState = interface;
  PIReference_1__MediaCaptureFocusState = ^IReference_1__MediaCaptureFocusState;

  // Windows.Media.Devices.Core.IFrameExposureControl
  Core_IFrameExposureControl = interface;
  PCore_IFrameExposureControl = ^Core_IFrameExposureControl;

  // Windows.Media.Devices.Core.IFrameExposureCompensationControl
  Core_IFrameExposureCompensationControl = interface;
  PCore_IFrameExposureCompensationControl = ^Core_IFrameExposureCompensationControl;

  // Windows.Media.Devices.Core.IFrameIsoSpeedControl
  Core_IFrameIsoSpeedControl = interface;
  PCore_IFrameIsoSpeedControl = ^Core_IFrameIsoSpeedControl;

  // Windows.Media.Devices.Core.IFrameFocusControl
  Core_IFrameFocusControl = interface;
  PCore_IFrameFocusControl = ^Core_IFrameFocusControl;

  // Windows.Media.Devices.Core.IFrameController
  Core_IFrameController = interface;
  PCore_IFrameController = ^Core_IFrameController;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.Core.IFrameController>
  IIterator_1__Core_IFrameController = interface;
  PIIterator_1__Core_IFrameController = ^IIterator_1__Core_IFrameController;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.Core.IFrameController>
  IIterable_1__Core_IFrameController = interface;
  PIIterable_1__Core_IFrameController = ^IIterable_1__Core_IFrameController;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.Core.IFrameController>
  IVectorView_1__Core_IFrameController = interface;
  PIVectorView_1__Core_IFrameController = ^IVectorView_1__Core_IFrameController;

  // Windows.Media.Devices.IDialRequestedEventArgs
  IDialRequestedEventArgs = interface;
  PIDialRequestedEventArgs = ^IDialRequestedEventArgs;

  // Windows.Media.Devices.DialRequestedEventHandler
  DialRequestedEventHandler = interface;
  PDialRequestedEventHandler = ^DialRequestedEventHandler;

  // Windows.Media.Devices.IRedialRequestedEventArgs
  IRedialRequestedEventArgs = interface;
  PIRedialRequestedEventArgs = ^IRedialRequestedEventArgs;

  // Windows.Media.Devices.RedialRequestedEventHandler
  RedialRequestedEventHandler = interface;
  PRedialRequestedEventHandler = ^RedialRequestedEventHandler;

  // Windows.Media.Devices.IKeypadPressedEventArgs
  IKeypadPressedEventArgs = interface;
  PIKeypadPressedEventArgs = ^IKeypadPressedEventArgs;

  // Windows.Media.Devices.KeypadPressedEventHandler
  KeypadPressedEventHandler = interface;
  PKeypadPressedEventHandler = ^KeypadPressedEventHandler;

  // Windows.Media.Devices.ICallControl
  ICallControl = interface;
  PICallControl = ^ICallControl;

  // Windows.Media.Devices.CallControlEventHandler
  CallControlEventHandler = interface;
  PCallControlEventHandler = ^CallControlEventHandler;

  // Windows.Media.Devices.Core.ICameraIntrinsics2
  Core_ICameraIntrinsics2 = interface;
  PCore_ICameraIntrinsics2 = ^Core_ICameraIntrinsics2;

  // Windows.Media.Devices.Core.ICameraIntrinsicsFactory
  Core_ICameraIntrinsicsFactory = interface;
  PCore_ICameraIntrinsicsFactory = ^Core_ICameraIntrinsicsFactory;

  // Windows.Media.Devices.Core.IFrameExposureCapabilities
  Core_IFrameExposureCapabilities = interface;
  PCore_IFrameExposureCapabilities = ^Core_IFrameExposureCapabilities;

  // Windows.Media.Devices.Core.IFrameExposureCompensationCapabilities
  Core_IFrameExposureCompensationCapabilities = interface;
  PCore_IFrameExposureCompensationCapabilities = ^Core_IFrameExposureCompensationCapabilities;

  // Windows.Media.Devices.Core.IFrameIsoSpeedCapabilities
  Core_IFrameIsoSpeedCapabilities = interface;
  PCore_IFrameIsoSpeedCapabilities = ^Core_IFrameIsoSpeedCapabilities;

  // Windows.Media.Devices.Core.IFrameFocusCapabilities
  Core_IFrameFocusCapabilities = interface;
  PCore_IFrameFocusCapabilities = ^Core_IFrameFocusCapabilities;

  // Windows.Media.Devices.Core.IFrameControlCapabilities
  Core_IFrameControlCapabilities = interface;
  PCore_IFrameControlCapabilities = ^Core_IFrameControlCapabilities;

  // Windows.Media.Devices.Core.IFrameFlashCapabilities
  Core_IFrameFlashCapabilities = interface;
  PCore_IFrameFlashCapabilities = ^Core_IFrameFlashCapabilities;

  // Windows.Media.Devices.Core.IFrameControlCapabilities2
  Core_IFrameControlCapabilities2 = interface;
  PCore_IFrameControlCapabilities2 = ^Core_IFrameControlCapabilities2;

  // Windows.Media.Devices.Core.IFrameFlashControl
  Core_IFrameFlashControl = interface;
  PCore_IFrameFlashControl = ^Core_IFrameFlashControl;

  // Windows.Media.Devices.Core.IFrameController2
  Core_IFrameController2 = interface;
  PCore_IFrameController2 = ^Core_IFrameController2;

  // Windows.Foundation.Collections.IVector`1<Windows.Media.Devices.Core.IFrameController>
  IVector_1__Core_IFrameController = interface;
  PIVector_1__Core_IFrameController = ^IVector_1__Core_IFrameController;

  // Windows.Media.Devices.Core.IVariablePhotoSequenceController
  Core_IVariablePhotoSequenceController = interface;
  PCore_IVariablePhotoSequenceController = ^Core_IVariablePhotoSequenceController;

  // Windows.Media.Devices.IAdvancedPhotoCaptureSettings
  IAdvancedPhotoCaptureSettings = interface;
  PIAdvancedPhotoCaptureSettings = ^IAdvancedPhotoCaptureSettings;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.AdvancedPhotoMode>
  IIterator_1__AdvancedPhotoMode = interface;
  PIIterator_1__AdvancedPhotoMode = ^IIterator_1__AdvancedPhotoMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.AdvancedPhotoMode>
  IIterable_1__AdvancedPhotoMode = interface;
  PIIterable_1__AdvancedPhotoMode = ^IIterable_1__AdvancedPhotoMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.AdvancedPhotoMode>
  IVectorView_1__AdvancedPhotoMode = interface;
  PIVectorView_1__AdvancedPhotoMode = ^IVectorView_1__AdvancedPhotoMode;

  // Windows.Media.Devices.IAdvancedPhotoControl
  IAdvancedPhotoControl = interface;
  PIAdvancedPhotoControl = ^IAdvancedPhotoControl;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController
  IAdvancedVideoCaptureDeviceController = interface;
  PIAdvancedVideoCaptureDeviceController = ^IAdvancedVideoCaptureDeviceController;

  // Windows.Media.Devices.ILowLagPhotoSequenceControl
  ILowLagPhotoSequenceControl = interface;
  PILowLagPhotoSequenceControl = ^ILowLagPhotoSequenceControl;

  // Windows.Media.Devices.ILowLagPhotoControl
  ILowLagPhotoControl = interface;
  PILowLagPhotoControl = ^ILowLagPhotoControl;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.CaptureSceneMode>
  IIterator_1__CaptureSceneMode = interface;
  PIIterator_1__CaptureSceneMode = ^IIterator_1__CaptureSceneMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.CaptureSceneMode>
  IIterable_1__CaptureSceneMode = interface;
  PIIterable_1__CaptureSceneMode = ^IIterable_1__CaptureSceneMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.CaptureSceneMode>
  IVectorView_1__CaptureSceneMode = interface;
  PIVectorView_1__CaptureSceneMode = ^IVectorView_1__CaptureSceneMode;

  // Windows.Media.Devices.ISceneModeControl
  ISceneModeControl = interface;
  PISceneModeControl = ^ISceneModeControl;

  // Windows.Media.Devices.ITorchControl
  ITorchControl = interface;
  PITorchControl = ^ITorchControl;

  // Windows.Media.Devices.IFlashControl
  IFlashControl = interface;
  PIFlashControl = ^IFlashControl;

  // Windows.Media.Devices.IWhiteBalanceControl
  IWhiteBalanceControl = interface;
  PIWhiteBalanceControl = ^IWhiteBalanceControl;

  // Windows.Media.Devices.IExposureControl
  IExposureControl = interface;
  PIExposureControl = ^IExposureControl;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.FocusPreset>
  IIterator_1__FocusPreset = interface;
  PIIterator_1__FocusPreset = ^IIterator_1__FocusPreset;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.FocusPreset>
  IIterable_1__FocusPreset = interface;
  PIIterable_1__FocusPreset = ^IIterable_1__FocusPreset;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.FocusPreset>
  IVectorView_1__FocusPreset = interface;
  PIVectorView_1__FocusPreset = ^IVectorView_1__FocusPreset;

  // Windows.Media.Devices.IFocusControl
  IFocusControl = interface;
  PIFocusControl = ^IFocusControl;

  // Windows.Media.Devices.IExposureCompensationControl
  IExposureCompensationControl = interface;
  PIExposureCompensationControl = ^IExposureCompensationControl;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IsoSpeedPreset>
  IIterator_1__IsoSpeedPreset = interface;
  PIIterator_1__IsoSpeedPreset = ^IIterator_1__IsoSpeedPreset;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IsoSpeedPreset>
  IIterable_1__IsoSpeedPreset = interface;
  PIIterable_1__IsoSpeedPreset = ^IIterable_1__IsoSpeedPreset;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.IsoSpeedPreset>
  IVectorView_1__IsoSpeedPreset = interface;
  PIVectorView_1__IsoSpeedPreset = ^IVectorView_1__IsoSpeedPreset;

  // Windows.Media.Devices.IIsoSpeedControl
  IIsoSpeedControl = interface;
  PIIsoSpeedControl = ^IIsoSpeedControl;

  // Windows.Media.Devices.IRegionOfInterest
  IRegionOfInterest = interface;
  PIRegionOfInterest = ^IRegionOfInterest;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IRegionOfInterest>
  IIterator_1__IRegionOfInterest = interface;
  PIIterator_1__IRegionOfInterest = ^IIterator_1__IRegionOfInterest;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IRegionOfInterest>
  IIterable_1__IRegionOfInterest = interface;
  PIIterable_1__IRegionOfInterest = ^IIterable_1__IRegionOfInterest;

  // Windows.Media.Devices.IRegionsOfInterestControl
  IRegionsOfInterestControl = interface;
  PIRegionsOfInterestControl = ^IRegionsOfInterestControl;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController2
  IAdvancedVideoCaptureDeviceController2 = interface;
  PIAdvancedVideoCaptureDeviceController2 = ^IAdvancedVideoCaptureDeviceController2;

  // Windows.Media.Devices.IPhotoConfirmationControl
  IPhotoConfirmationControl = interface;
  PIPhotoConfirmationControl = ^IPhotoConfirmationControl;

  // Windows.Media.Devices.IZoomControl
  IZoomControl = interface;
  PIZoomControl = ^IZoomControl;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController3
  IAdvancedVideoCaptureDeviceController3 = interface;
  PIAdvancedVideoCaptureDeviceController3 = ^IAdvancedVideoCaptureDeviceController3;

  // Windows.Media.Devices.IExposurePriorityVideoControl
  IExposurePriorityVideoControl = interface;
  PIExposurePriorityVideoControl = ^IExposurePriorityVideoControl;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.HdrVideoMode>
  IIterator_1__HdrVideoMode = interface;
  PIIterator_1__HdrVideoMode = ^IIterator_1__HdrVideoMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.HdrVideoMode>
  IIterable_1__HdrVideoMode = interface;
  PIIterable_1__HdrVideoMode = ^IIterable_1__HdrVideoMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.HdrVideoMode>
  IVectorView_1__HdrVideoMode = interface;
  PIVectorView_1__HdrVideoMode = ^IVectorView_1__HdrVideoMode;

  // Windows.Media.Devices.IHdrVideoControl
  IHdrVideoControl = interface;
  PIHdrVideoControl = ^IHdrVideoControl;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IIterator_1__OpticalImageStabilizationMode = interface;
  PIIterator_1__OpticalImageStabilizationMode = ^IIterator_1__OpticalImageStabilizationMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IIterable_1__OpticalImageStabilizationMode = interface;
  PIIterable_1__OpticalImageStabilizationMode = ^IIterable_1__OpticalImageStabilizationMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IVectorView_1__OpticalImageStabilizationMode = interface;
  PIVectorView_1__OpticalImageStabilizationMode = ^IVectorView_1__OpticalImageStabilizationMode;

  // Windows.Media.Devices.IOpticalImageStabilizationControl
  IOpticalImageStabilizationControl = interface;
  PIOpticalImageStabilizationControl = ^IOpticalImageStabilizationControl;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController4
  IAdvancedVideoCaptureDeviceController4 = interface;
  PIAdvancedVideoCaptureDeviceController4 = ^IAdvancedVideoCaptureDeviceController4;

  // Windows.Media.Devices.IVideoDeviceControllerGetDevicePropertyResult
  IVideoDeviceControllerGetDevicePropertyResult = interface;
  PIVideoDeviceControllerGetDevicePropertyResult = ^IVideoDeviceControllerGetDevicePropertyResult;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController5
  IAdvancedVideoCaptureDeviceController5 = interface;
  PIAdvancedVideoCaptureDeviceController5 = ^IAdvancedVideoCaptureDeviceController5;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.VideoTemporalDenoisingMode>
  IIterator_1__VideoTemporalDenoisingMode = interface;
  PIIterator_1__VideoTemporalDenoisingMode = ^IIterator_1__VideoTemporalDenoisingMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.VideoTemporalDenoisingMode>
  IIterable_1__VideoTemporalDenoisingMode = interface;
  PIIterable_1__VideoTemporalDenoisingMode = ^IIterable_1__VideoTemporalDenoisingMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.VideoTemporalDenoisingMode>
  IVectorView_1__VideoTemporalDenoisingMode = interface;
  PIVectorView_1__VideoTemporalDenoisingMode = ^IVectorView_1__VideoTemporalDenoisingMode;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.InfraredTorchMode>
  IIterator_1__InfraredTorchMode = interface;
  PIIterator_1__InfraredTorchMode = ^IIterator_1__InfraredTorchMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.InfraredTorchMode>
  IIterable_1__InfraredTorchMode = interface;
  PIIterable_1__InfraredTorchMode = ^IIterable_1__InfraredTorchMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.InfraredTorchMode>
  IVectorView_1__InfraredTorchMode = interface;
  PIVectorView_1__InfraredTorchMode = ^IVectorView_1__InfraredTorchMode;

  // Windows.Media.Devices.IPanelBasedOptimizationControl
  IPanelBasedOptimizationControl = interface;
  PIPanelBasedOptimizationControl = ^IPanelBasedOptimizationControl;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController8
  IAdvancedVideoCaptureDeviceController8 = interface;
  PIAdvancedVideoCaptureDeviceController8 = ^IAdvancedVideoCaptureDeviceController8;

  // Windows.Media.Devices.IModuleCommandResult
  IModuleCommandResult = interface;
  PIModuleCommandResult = ^IModuleCommandResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Devices.IModuleCommandResult>
  AsyncOperationCompletedHandler_1__IModuleCommandResult = interface;
  PAsyncOperationCompletedHandler_1__IModuleCommandResult = ^AsyncOperationCompletedHandler_1__IModuleCommandResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Devices.IModuleCommandResult>
  IAsyncOperation_1__IModuleCommandResult = interface;
  PIAsyncOperation_1__IModuleCommandResult = ^IAsyncOperation_1__IModuleCommandResult;

  // Windows.Media.Devices.IAudioDeviceModule
  IAudioDeviceModule = interface;
  PIAudioDeviceModule = ^IAudioDeviceModule;

  // Windows.Media.Devices.IAudioDeviceModuleNotificationEventArgs
  IAudioDeviceModuleNotificationEventArgs = interface;
  PIAudioDeviceModuleNotificationEventArgs = ^IAudioDeviceModuleNotificationEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Devices.IAudioDeviceModulesManager,Windows.Media.Devices.IAudioDeviceModuleNotificationEventArgs>
  TypedEventHandler_2__IAudioDeviceModulesManager__IAudioDeviceModuleNotificationEventArgs = interface;
  PTypedEventHandler_2__IAudioDeviceModulesManager__IAudioDeviceModuleNotificationEventArgs = ^TypedEventHandler_2__IAudioDeviceModulesManager__IAudioDeviceModuleNotificationEventArgs;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IAudioDeviceModule>
  IIterator_1__IAudioDeviceModule = interface;
  PIIterator_1__IAudioDeviceModule = ^IIterator_1__IAudioDeviceModule;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IAudioDeviceModule>
  IIterable_1__IAudioDeviceModule = interface;
  PIIterable_1__IAudioDeviceModule = ^IIterable_1__IAudioDeviceModule;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.IAudioDeviceModule>
  IVectorView_1__IAudioDeviceModule = interface;
  PIVectorView_1__IAudioDeviceModule = ^IVectorView_1__IAudioDeviceModule;

  // Windows.Media.Devices.IAudioDeviceModulesManager
  IAudioDeviceModulesManager = interface;
  PIAudioDeviceModulesManager = ^IAudioDeviceModulesManager;

  // Windows.Media.Devices.IAudioDeviceModulesManagerFactory
  IAudioDeviceModulesManagerFactory = interface;
  PIAudioDeviceModulesManagerFactory = ^IAudioDeviceModulesManagerFactory;

  // Windows.Media.Devices.ICallControlStatics
  ICallControlStatics = interface;
  PICallControlStatics = ^ICallControlStatics;

  // Windows.Media.Devices.IDefaultAudioDeviceChangedEventArgs
  IDefaultAudioDeviceChangedEventArgs = interface;
  PIDefaultAudioDeviceChangedEventArgs = ^IDefaultAudioDeviceChangedEventArgs;

  // Windows.Media.Devices.IFlashControl2
  IFlashControl2 = interface;
  PIFlashControl2 = ^IFlashControl2;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.FocusMode>
  IIterator_1__FocusMode = interface;
  PIIterator_1__FocusMode = ^IIterator_1__FocusMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.FocusMode>
  IIterable_1__FocusMode = interface;
  PIIterable_1__FocusMode = ^IIterable_1__FocusMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.FocusMode>
  IVectorView_1__FocusMode = interface;
  PIVectorView_1__FocusMode = ^IVectorView_1__FocusMode;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.ManualFocusDistance>
  IIterator_1__ManualFocusDistance = interface;
  PIIterator_1__ManualFocusDistance = ^IIterator_1__ManualFocusDistance;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.ManualFocusDistance>
  IIterable_1__ManualFocusDistance = interface;
  PIIterable_1__ManualFocusDistance = ^IIterable_1__ManualFocusDistance;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.ManualFocusDistance>
  IVectorView_1__ManualFocusDistance = interface;
  PIVectorView_1__ManualFocusDistance = ^IVectorView_1__ManualFocusDistance;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.AutoFocusRange>
  IIterator_1__AutoFocusRange = interface;
  PIIterator_1__AutoFocusRange = ^IIterator_1__AutoFocusRange;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.AutoFocusRange>
  IIterable_1__AutoFocusRange = interface;
  PIIterable_1__AutoFocusRange = ^IIterable_1__AutoFocusRange;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.AutoFocusRange>
  IVectorView_1__AutoFocusRange = interface;
  PIVectorView_1__AutoFocusRange = ^IVectorView_1__AutoFocusRange;

  // Windows.Foundation.IReference`1<Windows.Media.Devices.ManualFocusDistance>
  IReference_1__ManualFocusDistance = interface;
  PIReference_1__ManualFocusDistance = ^IReference_1__ManualFocusDistance;

  // Windows.Media.Devices.IFocusSettings
  IFocusSettings = interface;
  PIFocusSettings = ^IFocusSettings;

  // Windows.Media.Devices.IFocusControl2
  IFocusControl2 = interface;
  PIFocusControl2 = ^IFocusControl2;

  // Windows.Media.Devices.IIsoSpeedControl2
  IIsoSpeedControl2 = interface;
  PIIsoSpeedControl2 = ^IIsoSpeedControl2;

  // Windows.Foundation.TypedEventHandler`2<Object,Windows.Media.Devices.IDefaultAudioDeviceChangedEventArgs>
  TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs = interface;
  PTypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs = ^TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs;

  // Windows.Media.Devices.IMediaDeviceStatics
  IMediaDeviceStatics = interface;
  PIMediaDeviceStatics = ^IMediaDeviceStatics;

  // Windows.Media.Devices.IRegionOfInterest2
  IRegionOfInterest2 = interface;
  PIRegionOfInterest2 = ^IRegionOfInterest2;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.ZoomTransitionMode>
  IIterator_1__ZoomTransitionMode = interface;
  PIIterator_1__ZoomTransitionMode = ^IIterator_1__ZoomTransitionMode;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.ZoomTransitionMode>
  IIterable_1__ZoomTransitionMode = interface;
  PIIterable_1__ZoomTransitionMode = ^IIterable_1__ZoomTransitionMode;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.ZoomTransitionMode>
  IVectorView_1__ZoomTransitionMode = interface;
  PIVectorView_1__ZoomTransitionMode = ^IVectorView_1__ZoomTransitionMode;

  // Windows.Media.Devices.IZoomSettings
  IZoomSettings = interface;
  PIZoomSettings = ^IZoomSettings;

  // Windows.Media.Devices.IZoomControl2
  IZoomControl2 = interface;
  PIZoomControl2 = ^IZoomControl2;

  // Windows.Media.Devices Enums

  // Windows.Media.Devices.AdvancedPhotoMode
  AdvancedPhotoMode = (
    Auto = 0,
    Standard = 1,
    Hdr = 2,
    LowLight = 3
  );
  PAdvancedPhotoMode = ^AdvancedPhotoMode;

  // Windows.Media.Devices.AudioDeviceRole
  AudioDeviceRole = (
    Default = 0,
    Communications = 1
  );
  PAudioDeviceRole = ^AudioDeviceRole;

  // Windows.Media.Devices.AutoFocusRange
  AutoFocusRange = (
    FullRange = 0,
    Macro = 1,
    Normal = 2
  );
  PAutoFocusRange = ^AutoFocusRange;

  // Windows.Media.Devices.CameraStreamState
  CameraStreamState = (
    NotStreaming = 0,
    Streaming = 1,
    BlockedForPrivacy = 2,
    Shutdown = 3
  );
  PCameraStreamState = ^CameraStreamState;

  // Windows.Media.Devices.CaptureSceneMode
  CaptureSceneMode = (
    Auto = 0,
    Manual = 1,
    Macro = 2,
    Portrait = 3,
    Sport = 4,
    Snow = 5,
    Night = 6,
    Beach = 7,
    Sunset = 8,
    Candlelight = 9,
    Landscape = 10,
    NightPortrait = 11,
    Backlit = 12
  );
  PCaptureSceneMode = ^CaptureSceneMode;

  // Windows.Media.Devices.CaptureUse
  CaptureUse = (
    None = 0,
    Photo = 1,
    Video = 2
  );
  PCaptureUse = ^CaptureUse;

  // Windows.Media.Devices.ColorTemperaturePreset
  ColorTemperaturePreset = (
    Auto = 0,
    Manual = 1,
    Cloudy = 2,
    Daylight = 3,
    Flash = 4,
    Fluorescent = 5,
    Tungsten = 6,
    Candlelight = 7
  );
  PColorTemperaturePreset = ^ColorTemperaturePreset;

  // Windows.Media.Devices.Core.FrameFlashMode
  Core_FrameFlashMode = (
    Disable = 0,
    Enable = 1,
    Global = 2
  );
  PCore_FrameFlashMode = ^Core_FrameFlashMode;

  // Windows.Media.Devices.FocusMode
  FocusMode = (
    Auto = 0,
    Single = 1,
    Continuous = 2,
    Manual = 3
  );
  PFocusMode = ^FocusMode;

  // Windows.Media.Devices.FocusPreset
  FocusPreset = (
    Auto = 0,
    Manual = 1,
    AutoMacro = 2,
    AutoNormal = 3,
    AutoInfinity = 4,
    AutoHyperfocal = 5
  );
  PFocusPreset = ^FocusPreset;

  // Windows.Media.Devices.HdrVideoMode
  HdrVideoMode = (
    Off = 0,
    &On = 1,
    Auto = 2
  );
  PHdrVideoMode = ^HdrVideoMode;

  // Windows.Media.Devices.InfraredTorchMode
  InfraredTorchMode = (
    Off = 0,
    &On = 1,
    AlternatingFrameIllumination = 2
  );
  PInfraredTorchMode = ^InfraredTorchMode;

  // Windows.Media.Devices.IsoSpeedPreset
  IsoSpeedPreset = (
    Auto = 0,
    Iso50 = 1,
    Iso80 = 2,
    Iso100 = 3,
    Iso200 = 4,
    Iso400 = 5,
    Iso800 = 6,
    Iso1600 = 7,
    Iso3200 = 8,
    Iso6400 = 9,
    Iso12800 = 10,
    Iso25600 = 11
  );
  PIsoSpeedPreset = ^IsoSpeedPreset;

  // Windows.Media.Devices.ManualFocusDistance
  ManualFocusDistance = (
    Infinity = 0,
    Hyperfocal = 1,
    Nearest = 2
  );
  PManualFocusDistance = ^ManualFocusDistance;

  // Windows.Media.Devices.MediaCaptureFocusState
  MediaCaptureFocusState = (
    Uninitialized = 0,
    Lost = 1,
    Searching = 2,
    Focused = 3,
    Failed = 4
  );
  PMediaCaptureFocusState = ^MediaCaptureFocusState;

  // Windows.Media.Devices.MediaCaptureOptimization
  MediaCaptureOptimization = (
    Default = 0,
    Quality = 1,
    Latency = 2,
    Power = 3,
    LatencyThenQuality = 4,
    LatencyThenPower = 5,
    PowerAndQuality = 6
  );
  PMediaCaptureOptimization = ^MediaCaptureOptimization;

  // Windows.Media.Devices.MediaCapturePauseBehavior
  MediaCapturePauseBehavior = (
    RetainHardwareResources = 0,
    ReleaseHardwareResources = 1
  );
  PMediaCapturePauseBehavior = ^MediaCapturePauseBehavior;

  // Windows.Media.Devices.OpticalImageStabilizationMode
  OpticalImageStabilizationMode = (
    Off = 0,
    &On = 1,
    Auto = 2
  );
  POpticalImageStabilizationMode = ^OpticalImageStabilizationMode;

  // Windows.Media.Devices.RegionOfInterestType
  RegionOfInterestType = (
    Unknown = 0,
    Face = 1
  );
  PRegionOfInterestType = ^RegionOfInterestType;

  // Windows.Media.Devices.SendCommandStatus
  SendCommandStatus = (
    Success = 0,
    DeviceNotAvailable = 1
  );
  PSendCommandStatus = ^SendCommandStatus;

  // Windows.Media.Devices.TelephonyKey
  TelephonyKey = (
    D0 = 0,
    D1 = 1,
    D2 = 2,
    D3 = 3,
    D4 = 4,
    D5 = 5,
    D6 = 6,
    D7 = 7,
    D8 = 8,
    D9 = 9,
    Star = 10,
    Pound = 11,
    A = 12,
    B = 13,
    C = 14,
    D = 15
  );
  PTelephonyKey = ^TelephonyKey;

  // Windows.Media.Devices.VideoDeviceControllerGetDevicePropertyStatus
  VideoDeviceControllerGetDevicePropertyStatus = (
    Success = 0,
    UnknownFailure = 1,
    BufferTooSmall = 2,
    NotSupported = 3,
    DeviceNotAvailable = 4,
    MaxPropertyValueSizeTooSmall = 5,
    MaxPropertyValueSizeRequired = 6
  );
  PVideoDeviceControllerGetDevicePropertyStatus = ^VideoDeviceControllerGetDevicePropertyStatus;

  // Windows.Media.Devices.VideoDeviceControllerSetDevicePropertyStatus
  VideoDeviceControllerSetDevicePropertyStatus = (
    Success = 0,
    UnknownFailure = 1,
    NotSupported = 2,
    InvalidValue = 3,
    DeviceNotAvailable = 4,
    NotInControl = 5
  );
  PVideoDeviceControllerSetDevicePropertyStatus = ^VideoDeviceControllerSetDevicePropertyStatus;

  // Windows.Media.Devices.VideoTemporalDenoisingMode
  VideoTemporalDenoisingMode = (
    Off = 0,
    &On = 1,
    Auto = 2
  );
  PVideoTemporalDenoisingMode = ^VideoTemporalDenoisingMode;

  // Windows.Media.Devices.ZoomTransitionMode
  ZoomTransitionMode = (
    Auto = 0,
    Direct = 1,
    Smooth = 2
  );
  PZoomTransitionMode = ^ZoomTransitionMode;

  // Windows.Media.Devices Records
  // Windows.Media.Devices.CallControlContract
  CallControlContract = record
  end;
  PCallControlContract = ^CallControlContract;

  // Windows.Media.Devices Interfaces

  // Windows.Media.Devices.IMediaDeviceController
  IMediaDeviceController = interface(IInspectable)
  ['{F6F8F5CE-209A-48FB-86FC-D44578F317E6}']
    function GetAvailableMediaStreamProperties(mediaStreamType: Capture_MediaStreamType): IVectorView_1__IMediaEncodingProperties; safecall;
    function GetMediaStreamProperties(mediaStreamType: Capture_MediaStreamType): IMediaEncodingProperties; safecall;
    function SetMediaStreamPropertiesAsync(mediaStreamType: Capture_MediaStreamType; mediaEncodingProperties: IMediaEncodingProperties): IAsyncAction; safecall;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IAudioDeviceController
  IAudioDeviceController = interface(IInspectable)
  ['{EDD4A388-79C7-4F7C-90E8-EF934B21580A}']
    procedure put_Muted(value: Boolean); safecall;
    function get_Muted: Boolean; safecall;
    procedure put_VolumePercent(value: Single); safecall;
    function get_VolumePercent: Single; safecall;
    property Muted: Boolean read get_Muted write put_Muted;
    property VolumePercent: Single read get_VolumePercent write put_VolumePercent;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IMediaDeviceControlCapabilities
  IMediaDeviceControlCapabilities = interface(IInspectable)
  ['{23005816-EB85-43E2-B92B-8240D5EE70EC}']
    function get_Supported: Boolean; safecall;
    function get_Min: Double; safecall;
    function get_Max: Double; safecall;
    function get_Step: Double; safecall;
    function get_Default: Double; safecall;
    function get_AutoModeSupported: Boolean; safecall;
    property AutoModeSupported: Boolean read get_AutoModeSupported;
    property Default: Double read get_Default;
    property Max: Double read get_Max;
    property Min: Double read get_Min;
    property Step: Double read get_Step;
    property Supported: Boolean read get_Supported;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IMediaDeviceControl
  IMediaDeviceControl = interface(IInspectable)
  ['{EFA8DFA9-6F75-4863-BA0B-583F3036B4DE}']
    function get_Capabilities: IMediaDeviceControlCapabilities; safecall;
    function TryGetValue(out value: Double): Boolean; safecall;
    function TrySetValue(value: Double): Boolean; safecall;
    function TryGetAuto(out value: Boolean): Boolean; safecall;
    function TrySetAuto(value: Boolean): Boolean; safecall;
    property Capabilities: IMediaDeviceControlCapabilities read get_Capabilities;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IVideoDeviceController
  IVideoDeviceController = interface(IInspectable)
  ['{99555575-2E2E-40B8-B6C7-F82D10013210}']
    function get_Brightness: IMediaDeviceControl; safecall;
    function get_Contrast: IMediaDeviceControl; safecall;
    function get_Hue: IMediaDeviceControl; safecall;
    function get_WhiteBalance: IMediaDeviceControl; safecall;
    function get_BacklightCompensation: IMediaDeviceControl; safecall;
    function get_Pan: IMediaDeviceControl; safecall;
    function get_Tilt: IMediaDeviceControl; safecall;
    function get_Zoom: IMediaDeviceControl; safecall;
    function get_Roll: IMediaDeviceControl; safecall;
    function get_Exposure: IMediaDeviceControl; safecall;
    function get_Focus: IMediaDeviceControl; safecall;
    function TrySetPowerlineFrequency(value: Capture_PowerlineFrequency): Boolean; safecall;
    function TryGetPowerlineFrequency(out value: Capture_PowerlineFrequency): Boolean; safecall;
    property BacklightCompensation: IMediaDeviceControl read get_BacklightCompensation;
    property Brightness: IMediaDeviceControl read get_Brightness;
    property Contrast: IMediaDeviceControl read get_Contrast;
    property Exposure: IMediaDeviceControl read get_Exposure;
    property Focus: IMediaDeviceControl read get_Focus;
    property Hue: IMediaDeviceControl read get_Hue;
    property Pan: IMediaDeviceControl read get_Pan;
    property Roll: IMediaDeviceControl read get_Roll;
    property Tilt: IMediaDeviceControl read get_Tilt;
    property WhiteBalance: IMediaDeviceControl read get_WhiteBalance;
    property Zoom: IMediaDeviceControl read get_Zoom;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.Core.ICameraIntrinsics
  [WinRTClassNameAttribute(SWindows_Media_Devices_Core_CameraIntrinsics)]
  Core_ICameraIntrinsics = interface(IInspectable)
  ['{0AA6ED32-6589-49DA-AFDE-594270CA0AAC}']
    function get_FocalLength: Numerics_Vector2; safecall;
    function get_PrincipalPoint: Numerics_Vector2; safecall;
    function get_RadialDistortion: Numerics_Vector3; safecall;
    function get_TangentialDistortion: Numerics_Vector2; safecall;
    function get_ImageWidth: Cardinal; safecall;
    function get_ImageHeight: Cardinal; safecall;
    function ProjectOntoFrame(coordinate: Numerics_Vector3): TPointF; safecall;
    function UnprojectAtUnitDepth(pixelCoordinate: TPointF): Numerics_Vector2; safecall;
    procedure ProjectManyOntoFrame(coordinatesSize: Cardinal; coordinates: PNumerics_Vector3; resultsSize: Cardinal; results: PPointF); safecall;
    procedure UnprojectPixelsAtUnitDepth(pixelCoordinatesSize: Cardinal; pixelCoordinates: PPointF; resultsSize: Cardinal; results: PNumerics_Vector2); safecall;
    property FocalLength: Numerics_Vector2 read get_FocalLength;
    property ImageHeight: Cardinal read get_ImageHeight;
    property ImageWidth: Cardinal read get_ImageWidth;
    property PrincipalPoint: Numerics_Vector2 read get_PrincipalPoint;
    property RadialDistortion: Numerics_Vector3 read get_RadialDistortion;
    property TangentialDistortion: Numerics_Vector2 read get_TangentialDistortion;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Media.Devices.CaptureSceneMode>
  IReference_1__CaptureSceneMode = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: CaptureSceneMode; safecall;
    property Value: CaptureSceneMode read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.Core.IDepthCorrelatedCoordinateMapper
  Core_IDepthCorrelatedCoordinateMapper = interface(IInspectable)
  ['{F95D89FB-8AF0-4CB0-926D-696866E5046A}']
    function UnprojectPoint(sourcePoint: TPointF; targetCoordinateSystem: Spatial_ISpatialCoordinateSystem): Numerics_Vector3; safecall;
    procedure UnprojectPoints(sourcePointsSize: Cardinal; sourcePoints: PPointF; targetCoordinateSystem: Spatial_ISpatialCoordinateSystem; resultsSize: Cardinal; results: PNumerics_Vector3); safecall;
    function MapPoint(sourcePoint: TPointF; targetCoordinateSystem: Spatial_ISpatialCoordinateSystem; targetCameraIntrinsics: Core_ICameraIntrinsics): TPointF; safecall;
    procedure MapPoints(sourcePointsSize: Cardinal; sourcePoints: PPointF; targetCoordinateSystem: Spatial_ISpatialCoordinateSystem; targetCameraIntrinsics: Core_ICameraIntrinsics; resultsSize: Cardinal; results: PPointF); safecall;
  end;

  // Windows.Foundation.IReference`1<Windows.Media.Devices.MediaCaptureFocusState>
  IReference_1__MediaCaptureFocusState = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: MediaCaptureFocusState; safecall;
    property Value: MediaCaptureFocusState read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.Core.IFrameExposureControl
  Core_IFrameExposureControl = interface(IInspectable)
  ['{B1605A61-FFAF-4752-B621-F5B6F117F432}']
    function get_Auto: Boolean; safecall;
    procedure put_Auto(value: Boolean); safecall;
    function get_Value: IReference_1__TimeSpan; safecall;
    procedure put_Value(value: IReference_1__TimeSpan); safecall;
    property Auto: Boolean read get_Auto write put_Auto;
    property Value: IReference_1__TimeSpan read get_Value write put_Value;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.Core.IFrameExposureCompensationControl
  Core_IFrameExposureCompensationControl = interface(IInspectable)
  ['{E95896C9-F7F9-48CA-8591-A26531CB1578}']
    function get_Value: IReference_1__Single; safecall;
    procedure put_Value(value: IReference_1__Single); safecall;
    property Value: IReference_1__Single read get_Value write put_Value;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.Core.IFrameIsoSpeedControl
  Core_IFrameIsoSpeedControl = interface(IInspectable)
  ['{1A03EFED-786A-4C75-A557-7AB9A85F588C}']
    function get_Auto: Boolean; safecall;
    procedure put_Auto(value: Boolean); safecall;
    function get_Value: IReference_1__Cardinal; safecall;
    procedure put_Value(value: IReference_1__Cardinal); safecall;
    property Auto: Boolean read get_Auto write put_Auto;
    property Value: IReference_1__Cardinal read get_Value write put_Value;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.Core.IFrameFocusControl
  Core_IFrameFocusControl = interface(IInspectable)
  ['{272DF1D0-D912-4214-A67B-E38A8D48D8C6}']
    function get_Value: IReference_1__Cardinal; safecall;
    procedure put_Value(value: IReference_1__Cardinal); safecall;
    property Value: IReference_1__Cardinal read get_Value write put_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.Core.IFrameController
  [WinRTClassNameAttribute(SWindows_Media_Devices_Core_FrameController)]
  Core_IFrameController = interface(IInspectable)
  ['{C16459D9-BAEF-4052-9177-48AFF2AF7522}']
    function get_ExposureControl: Core_IFrameExposureControl; safecall;
    function get_ExposureCompensationControl: Core_IFrameExposureCompensationControl; safecall;
    function get_IsoSpeedControl: Core_IFrameIsoSpeedControl; safecall;
    function get_FocusControl: Core_IFrameFocusControl; safecall;
    function get_PhotoConfirmationEnabled: IReference_1__Boolean; safecall;
    procedure put_PhotoConfirmationEnabled(value: IReference_1__Boolean); safecall;
    property ExposureCompensationControl: Core_IFrameExposureCompensationControl read get_ExposureCompensationControl;
    property ExposureControl: Core_IFrameExposureControl read get_ExposureControl;
    property FocusControl: Core_IFrameFocusControl read get_FocusControl;
    property IsoSpeedControl: Core_IFrameIsoSpeedControl read get_IsoSpeedControl;
    property PhotoConfirmationEnabled: IReference_1__Boolean read get_PhotoConfirmationEnabled write put_PhotoConfirmationEnabled;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.Core.IFrameController>
  IIterator_1__Core_IFrameController_Base = interface(IInspectable)
  ['{1440DC88-63FF-5A01-BB93-390C76742488}']
    function get_Current: Core_IFrameController; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCore_IFrameController): Cardinal; safecall;
    property Current: Core_IFrameController read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.Core.IFrameController>
  IIterator_1__Core_IFrameController = interface(IIterator_1__Core_IFrameController_Base)
  ['{B2B33445-0C84-5AA7-AF4A-C56B687CA08A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.Core.IFrameController>
  IIterable_1__Core_IFrameController_Base = interface(IInspectable)
  ['{BD8EEADC-2DD9-5AD8-AC5D-F3B13B94B9C2}']
    function First: IIterator_1__Core_IFrameController; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.Core.IFrameController>
  IIterable_1__Core_IFrameController = interface(IIterable_1__Core_IFrameController_Base)
  ['{8AA6E634-4356-5E6C-84B1-77741B100001}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.Core.IFrameController>
  IVectorView_1__Core_IFrameController = interface(IInspectable)
  ['{D7EBCF7F-B0F6-5389-A44D-FCDA72ACC22E}']
    function GetAt(index: Cardinal): Core_IFrameController; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Core_IFrameController; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IFrameController): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IDialRequestedEventArgs
  IDialRequestedEventArgs = interface(IInspectable)
  ['{037B929E-953C-4286-8866-4F0F376C855A}']
    procedure Handled; safecall;
    function get_Contact: IInspectable; safecall;
    property Contact: IInspectable read get_Contact;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.DialRequestedEventHandler
  DialRequestedEventHandler = interface(IUnknown)
  ['{5ABBFFDB-C21F-4BC4-891B-257E28C1B1A4}']
    procedure Invoke(sender: ICallControl; e: IDialRequestedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IRedialRequestedEventArgs
  IRedialRequestedEventArgs = interface(IInspectable)
  ['{7EB55209-76AB-4C31-B40E-4B58379D580C}']
    procedure Handled; safecall;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.RedialRequestedEventHandler
  RedialRequestedEventHandler = interface(IUnknown)
  ['{BAF257D1-4EBD-4B84-9F47-6EC43D75D8B1}']
    procedure Invoke(sender: ICallControl; e: IRedialRequestedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IKeypadPressedEventArgs
  IKeypadPressedEventArgs = interface(IInspectable)
  ['{D3A43900-B4FA-49CD-9442-89AF6568F601}']
    function get_TelephonyKey: TelephonyKey; safecall;
    property TelephonyKey_: TelephonyKey read get_TelephonyKey;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.KeypadPressedEventHandler
  KeypadPressedEventHandler = interface(IUnknown)
  ['{E637A454-C527-422C-8926-C9AF83B559A0}']
    procedure Invoke(sender: ICallControl; e: IKeypadPressedEventArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.ICallControl
  [WinRTClassNameAttribute(SWindows_Media_Devices_CallControl)]
  ICallControl = interface(IInspectable)
  ['{A520D0D6-AE8D-45DB-8011-CA49D3B3E578}']
    function IndicateNewIncomingCall(enableRinger: Boolean; callerId: HSTRING): UInt64; safecall;
    function IndicateNewOutgoingCall: UInt64; safecall;
    procedure IndicateActiveCall(callToken: UInt64); safecall;
    procedure EndCall(callToken: UInt64); safecall;
    function get_HasRinger: Boolean; safecall;
    function add_AnswerRequested(handler: CallControlEventHandler): EventRegistrationToken; safecall;
    procedure remove_AnswerRequested(token: EventRegistrationToken); safecall;
    function add_HangUpRequested(handler: CallControlEventHandler): EventRegistrationToken; safecall;
    procedure remove_HangUpRequested(token: EventRegistrationToken); safecall;
    function add_DialRequested(handler: DialRequestedEventHandler): EventRegistrationToken; safecall;
    procedure remove_DialRequested(token: EventRegistrationToken); safecall;
    function add_RedialRequested(handler: RedialRequestedEventHandler): EventRegistrationToken; safecall;
    procedure remove_RedialRequested(token: EventRegistrationToken); safecall;
    function add_KeypadPressed(handler: KeypadPressedEventHandler): EventRegistrationToken; safecall;
    procedure remove_KeypadPressed(token: EventRegistrationToken); safecall;
    function add_AudioTransferRequested(handler: CallControlEventHandler): EventRegistrationToken; safecall;
    procedure remove_AudioTransferRequested(token: EventRegistrationToken); safecall;
    property HasRinger: Boolean read get_HasRinger;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.CallControlEventHandler
  CallControlEventHandler = interface(IUnknown)
  ['{596F759F-50DF-4454-BC63-4D3D01B61958}']
    procedure Invoke(sender: ICallControl); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.Core.ICameraIntrinsics2
  Core_ICameraIntrinsics2 = interface(IInspectable)
  ['{0CDAA447-0798-4B4D-839F-C5EC414DB27A}']
    function get_UndistortedProjectionTransform: Numerics_Matrix4x4; safecall;
    function DistortPoint(input: TPointF): TPointF; safecall;
    procedure DistortPoints(inputsSize: Cardinal; inputs: PPointF; resultsSize: Cardinal; results: PPointF); safecall;
    function UndistortPoint(input: TPointF): TPointF; safecall;
    procedure UndistortPoints(inputsSize: Cardinal; inputs: PPointF; resultsSize: Cardinal; results: PPointF); safecall;
    property UndistortedProjectionTransform: Numerics_Matrix4x4 read get_UndistortedProjectionTransform;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.Core.ICameraIntrinsicsFactory
  [WinRTClassNameAttribute(SWindows_Media_Devices_Core_CameraIntrinsics)]
  Core_ICameraIntrinsicsFactory = interface(IInspectable)
  ['{C0DDC486-2132-4A34-A659-9BFE2A055712}']
    function Create(focalLength: Numerics_Vector2; principalPoint: Numerics_Vector2; radialDistortion: Numerics_Vector3; tangentialDistortion: Numerics_Vector2; imageWidth: Cardinal; imageHeight: Cardinal): Core_ICameraIntrinsics; safecall;
  end;

  // Windows.Media.Devices.Core.IFrameExposureCapabilities
  Core_IFrameExposureCapabilities = interface(IInspectable)
  ['{BDBE9CE3-3985-4E72-97C2-0590D61307A1}']
    function get_Supported: Boolean; safecall;
    function get_Min: TimeSpan; safecall;
    function get_Max: TimeSpan; safecall;
    function get_Step: TimeSpan; safecall;
    property Max: TimeSpan read get_Max;
    property Min: TimeSpan read get_Min;
    property Step: TimeSpan read get_Step;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.Core.IFrameExposureCompensationCapabilities
  Core_IFrameExposureCompensationCapabilities = interface(IInspectable)
  ['{B988A823-8065-41EE-B04F-722265954500}']
    function get_Supported: Boolean; safecall;
    function get_Min: Single; safecall;
    function get_Max: Single; safecall;
    function get_Step: Single; safecall;
    property Max: Single read get_Max;
    property Min: Single read get_Min;
    property Step: Single read get_Step;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.Core.IFrameIsoSpeedCapabilities
  Core_IFrameIsoSpeedCapabilities = interface(IInspectable)
  ['{16BDFF61-6DF6-4AC9-B92A-9F6ECD1AD2FA}']
    function get_Supported: Boolean; safecall;
    function get_Min: Cardinal; safecall;
    function get_Max: Cardinal; safecall;
    function get_Step: Cardinal; safecall;
    property Max: Cardinal read get_Max;
    property Min: Cardinal read get_Min;
    property Step: Cardinal read get_Step;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.Core.IFrameFocusCapabilities
  Core_IFrameFocusCapabilities = interface(IInspectable)
  ['{7B25CD58-01C0-4065-9C40-C1A721425C1A}']
    function get_Supported: Boolean; safecall;
    function get_Min: Cardinal; safecall;
    function get_Max: Cardinal; safecall;
    function get_Step: Cardinal; safecall;
    property Max: Cardinal read get_Max;
    property Min: Cardinal read get_Min;
    property Step: Cardinal read get_Step;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.Core.IFrameControlCapabilities
  Core_IFrameControlCapabilities = interface(IInspectable)
  ['{A8FFAE60-4E9E-4377-A789-E24C4AE7E544}']
    function get_Exposure: Core_IFrameExposureCapabilities; safecall;
    function get_ExposureCompensation: Core_IFrameExposureCompensationCapabilities; safecall;
    function get_IsoSpeed: Core_IFrameIsoSpeedCapabilities; safecall;
    function get_Focus: Core_IFrameFocusCapabilities; safecall;
    function get_PhotoConfirmationSupported: Boolean; safecall;
    property Exposure: Core_IFrameExposureCapabilities read get_Exposure;
    property ExposureCompensation: Core_IFrameExposureCompensationCapabilities read get_ExposureCompensation;
    property Focus: Core_IFrameFocusCapabilities read get_Focus;
    property IsoSpeed: Core_IFrameIsoSpeedCapabilities read get_IsoSpeed;
    property PhotoConfirmationSupported: Boolean read get_PhotoConfirmationSupported;
  end;

  // Windows.Media.Devices.Core.IFrameFlashCapabilities
  Core_IFrameFlashCapabilities = interface(IInspectable)
  ['{BB9341A2-5EBE-4F62-8223-0E2B05BFBBD0}']
    function get_Supported: Boolean; safecall;
    function get_RedEyeReductionSupported: Boolean; safecall;
    function get_PowerSupported: Boolean; safecall;
    property PowerSupported: Boolean read get_PowerSupported;
    property RedEyeReductionSupported: Boolean read get_RedEyeReductionSupported;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.Core.IFrameControlCapabilities2
  Core_IFrameControlCapabilities2 = interface(IInspectable)
  ['{CE9B0464-4730-440F-BD3E-EFE8A8F230A8}']
    function get_Flash: Core_IFrameFlashCapabilities; safecall;
    property Flash: Core_IFrameFlashCapabilities read get_Flash;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.Core.IFrameFlashControl
  Core_IFrameFlashControl = interface(IInspectable)
  ['{75D5F6C7-BD45-4FAB-9375-45AC04B332C2}']
    function get_Mode: Core_FrameFlashMode; safecall;
    procedure put_Mode(value: Core_FrameFlashMode); safecall;
    function get_Auto: Boolean; safecall;
    procedure put_Auto(value: Boolean); safecall;
    function get_RedEyeReduction: Boolean; safecall;
    procedure put_RedEyeReduction(value: Boolean); safecall;
    function get_PowerPercent: Single; safecall;
    procedure put_PowerPercent(value: Single); safecall;
    property Auto: Boolean read get_Auto write put_Auto;
    property Mode: Core_FrameFlashMode read get_Mode write put_Mode;
    property PowerPercent: Single read get_PowerPercent write put_PowerPercent;
    property RedEyeReduction: Boolean read get_RedEyeReduction write put_RedEyeReduction;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.Core.IFrameController2
  Core_IFrameController2 = interface(IInspectable)
  ['{00D3BC75-D87C-485B-8A09-5C358568B427}']
    function get_FlashControl: Core_IFrameFlashControl; safecall;
    property FlashControl: Core_IFrameFlashControl read get_FlashControl;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Devices.Core.IFrameController>
  IVector_1__Core_IFrameController_Base = interface(IInspectable)
  ['{1C8F8276-B89A-5093-A1ED-AF49DFB72A89}']
    function GetAt(index: Cardinal): Core_IFrameController; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Core_IFrameController; safecall;
    function IndexOf(value: Core_IFrameController; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Core_IFrameController); safecall;
    procedure InsertAt(index: Cardinal; value: Core_IFrameController); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Core_IFrameController); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCore_IFrameController): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PCore_IFrameController); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Media.Devices.Core.IFrameController>
  IVector_1__Core_IFrameController = interface(IVector_1__Core_IFrameController_Base)
  ['{717903F3-0AAD-5328-B195-8F974C549255}']
  end;

  // Windows.Media.Devices.Core.IVariablePhotoSequenceController
  Core_IVariablePhotoSequenceController = interface(IInspectable)
  ['{7FBFF880-ED8C-43FD-A7C3-B35809E4229A}']
    function get_Supported: Boolean; safecall;
    function get_MaxPhotosPerSecond: Single; safecall;
    function get_PhotosPerSecondLimit: Single; safecall;
    procedure put_PhotosPerSecondLimit(value: Single); safecall;
    function GetHighestConcurrentFrameRate(captureProperties: IMediaEncodingProperties): IMediaRatio; safecall;
    function GetCurrentFrameRate: IMediaRatio; safecall;
    function get_FrameCapabilities: Core_IFrameControlCapabilities; safecall;
    function get_DesiredFrameControllers: IVector_1__Core_IFrameController; safecall;
    property DesiredFrameControllers: IVector_1__Core_IFrameController read get_DesiredFrameControllers;
    property FrameCapabilities: Core_IFrameControlCapabilities read get_FrameCapabilities;
    property MaxPhotosPerSecond: Single read get_MaxPhotosPerSecond;
    property PhotosPerSecondLimit: Single read get_PhotosPerSecondLimit write put_PhotosPerSecondLimit;
    property Supported: Boolean read get_Supported;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IAdvancedPhotoCaptureSettings
  [WinRTClassNameAttribute(SWindows_Media_Devices_AdvancedPhotoCaptureSettings)]
  IAdvancedPhotoCaptureSettings = interface(IInspectable)
  ['{08F3863A-0018-445B-93D2-646D1C5ED05C}']
    function get_Mode: AdvancedPhotoMode; safecall;
    procedure put_Mode(value: AdvancedPhotoMode); safecall;
    property Mode: AdvancedPhotoMode read get_Mode write put_Mode;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.AdvancedPhotoMode>
  IIterator_1__AdvancedPhotoMode_Base = interface(IInspectable)
  ['{E6D0BC9D-E1CB-5ED0-8EDE-7D037BCC2E07}']
    function get_Current: AdvancedPhotoMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAdvancedPhotoMode): Cardinal; safecall;
    property Current: AdvancedPhotoMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.AdvancedPhotoMode>
  IIterator_1__AdvancedPhotoMode = interface(IIterator_1__AdvancedPhotoMode_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.AdvancedPhotoMode>
  IIterable_1__AdvancedPhotoMode_Base = interface(IInspectable)
  ['{7D090784-70A9-570C-BE82-0D0890318975}']
    function First: IIterator_1__AdvancedPhotoMode; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.AdvancedPhotoMode>
  IIterable_1__AdvancedPhotoMode = interface(IIterable_1__AdvancedPhotoMode_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.AdvancedPhotoMode>
  IVectorView_1__AdvancedPhotoMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): AdvancedPhotoMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: AdvancedPhotoMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAdvancedPhotoMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Devices.IAdvancedPhotoControl
  IAdvancedPhotoControl = interface(IInspectable)
  ['{C5B15486-9001-4682-9309-68EAE0080EEC}']
    function get_Supported: Boolean; safecall;
    function get_SupportedModes: IVectorView_1__AdvancedPhotoMode; safecall;
    function get_Mode: AdvancedPhotoMode; safecall;
    procedure Configure(settings: IAdvancedPhotoCaptureSettings); safecall;
    property Mode: AdvancedPhotoMode read get_Mode;
    property Supported: Boolean read get_Supported;
    property SupportedModes: IVectorView_1__AdvancedPhotoMode read get_SupportedModes;
  end;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController
  IAdvancedVideoCaptureDeviceController = interface(IInspectable)
  ['{DE6FF4D3-2B96-4583-80AB-B5B01DC6A8D7}']
    procedure SetDeviceProperty(propertyId: HSTRING; propertyValue: IInspectable); safecall;
    function GetDeviceProperty(propertyId: HSTRING): IInspectable; safecall;
  end;

  // Windows.Media.Devices.ILowLagPhotoSequenceControl
  ILowLagPhotoSequenceControl = interface(IInspectable)
  ['{3DCF909D-6D16-409C-BAFE-B9A594C6FDE6}']
    function get_Supported: Boolean; safecall;
    function get_MaxPastPhotos: Cardinal; safecall;
    function get_MaxPhotosPerSecond: Single; safecall;
    function get_PastPhotoLimit: Cardinal; safecall;
    procedure put_PastPhotoLimit(value: Cardinal); safecall;
    function get_PhotosPerSecondLimit: Single; safecall;
    procedure put_PhotosPerSecondLimit(value: Single); safecall;
    function GetHighestConcurrentFrameRate(captureProperties: IMediaEncodingProperties): IMediaRatio; safecall;
    function GetCurrentFrameRate: IMediaRatio; safecall;
    function get_ThumbnailEnabled: Boolean; safecall;
    procedure put_ThumbnailEnabled(value: Boolean); safecall;
    function get_ThumbnailFormat: MediaThumbnailFormat; safecall;
    procedure put_ThumbnailFormat(value: MediaThumbnailFormat); safecall;
    function get_DesiredThumbnailSize: Cardinal; safecall;
    procedure put_DesiredThumbnailSize(value: Cardinal); safecall;
    function get_HardwareAcceleratedThumbnailSupported: Cardinal; safecall;
    property DesiredThumbnailSize: Cardinal read get_DesiredThumbnailSize write put_DesiredThumbnailSize;
    property HardwareAcceleratedThumbnailSupported: Cardinal read get_HardwareAcceleratedThumbnailSupported;
    property MaxPastPhotos: Cardinal read get_MaxPastPhotos;
    property MaxPhotosPerSecond: Single read get_MaxPhotosPerSecond;
    property PastPhotoLimit: Cardinal read get_PastPhotoLimit write put_PastPhotoLimit;
    property PhotosPerSecondLimit: Single read get_PhotosPerSecondLimit write put_PhotosPerSecondLimit;
    property Supported: Boolean read get_Supported;
    property ThumbnailEnabled: Boolean read get_ThumbnailEnabled write put_ThumbnailEnabled;
    property ThumbnailFormat: MediaThumbnailFormat read get_ThumbnailFormat write put_ThumbnailFormat;
  end;

  // Windows.Media.Devices.ILowLagPhotoControl
  ILowLagPhotoControl = interface(IInspectable)
  ['{6D5C4DD0-FADF-415D-AEE6-3BAA529300C9}']
    function GetHighestConcurrentFrameRate(captureProperties: IMediaEncodingProperties): IMediaRatio; safecall;
    function GetCurrentFrameRate: IMediaRatio; safecall;
    function get_ThumbnailEnabled: Boolean; safecall;
    procedure put_ThumbnailEnabled(value: Boolean); safecall;
    function get_ThumbnailFormat: MediaThumbnailFormat; safecall;
    procedure put_ThumbnailFormat(value: MediaThumbnailFormat); safecall;
    function get_DesiredThumbnailSize: Cardinal; safecall;
    procedure put_DesiredThumbnailSize(value: Cardinal); safecall;
    function get_HardwareAcceleratedThumbnailSupported: Cardinal; safecall;
    property DesiredThumbnailSize: Cardinal read get_DesiredThumbnailSize write put_DesiredThumbnailSize;
    property HardwareAcceleratedThumbnailSupported: Cardinal read get_HardwareAcceleratedThumbnailSupported;
    property ThumbnailEnabled: Boolean read get_ThumbnailEnabled write put_ThumbnailEnabled;
    property ThumbnailFormat: MediaThumbnailFormat read get_ThumbnailFormat write put_ThumbnailFormat;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.CaptureSceneMode>
  IIterator_1__CaptureSceneMode_Base = interface(IInspectable)
  ['{AAFA6D7A-2F7F-5DD7-AA0A-265731A2B3B3}']
    function get_Current: CaptureSceneMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PCaptureSceneMode): Cardinal; safecall;
    property Current: CaptureSceneMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.CaptureSceneMode>
  IIterator_1__CaptureSceneMode = interface(IIterator_1__CaptureSceneMode_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.CaptureSceneMode>
  IIterable_1__CaptureSceneMode_Base = interface(IInspectable)
  ['{16D26B98-2CBC-52F0-AB64-1723714418E9}']
    function First: IIterator_1__CaptureSceneMode; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.CaptureSceneMode>
  IIterable_1__CaptureSceneMode = interface(IIterable_1__CaptureSceneMode_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.CaptureSceneMode>
  IVectorView_1__CaptureSceneMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): CaptureSceneMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: CaptureSceneMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PCaptureSceneMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Devices.ISceneModeControl
  ISceneModeControl = interface(IInspectable)
  ['{D48E5AF7-8D59-4854-8C62-12C70BA89B7C}']
    function get_SupportedModes: IVectorView_1__CaptureSceneMode; safecall;
    function get_Value: CaptureSceneMode; safecall;
    function SetValueAsync(sceneMode: CaptureSceneMode): IAsyncAction; safecall;
    property SupportedModes: IVectorView_1__CaptureSceneMode read get_SupportedModes;
    property Value: CaptureSceneMode read get_Value;
  end;

  // Windows.Media.Devices.ITorchControl
  ITorchControl = interface(IInspectable)
  ['{A6053665-8250-416C-919A-724296AFA306}']
    function get_Supported: Boolean; safecall;
    function get_PowerSupported: Boolean; safecall;
    function get_Enabled: Boolean; safecall;
    procedure put_Enabled(value: Boolean); safecall;
    function get_PowerPercent: Single; safecall;
    procedure put_PowerPercent(value: Single); safecall;
    property Enabled: Boolean read get_Enabled write put_Enabled;
    property PowerPercent: Single read get_PowerPercent write put_PowerPercent;
    property PowerSupported: Boolean read get_PowerSupported;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.IFlashControl
  IFlashControl = interface(IInspectable)
  ['{DEF41DBE-7D68-45E3-8C0F-BE7BB32837D0}']
    function get_Supported: Boolean; safecall;
    function get_PowerSupported: Boolean; safecall;
    function get_RedEyeReductionSupported: Boolean; safecall;
    function get_Enabled: Boolean; safecall;
    procedure put_Enabled(value: Boolean); safecall;
    function get_Auto: Boolean; safecall;
    procedure put_Auto(value: Boolean); safecall;
    function get_RedEyeReduction: Boolean; safecall;
    procedure put_RedEyeReduction(value: Boolean); safecall;
    function get_PowerPercent: Single; safecall;
    procedure put_PowerPercent(value: Single); safecall;
    property Auto: Boolean read get_Auto write put_Auto;
    property Enabled: Boolean read get_Enabled write put_Enabled;
    property PowerPercent: Single read get_PowerPercent write put_PowerPercent;
    property PowerSupported: Boolean read get_PowerSupported;
    property RedEyeReduction: Boolean read get_RedEyeReduction write put_RedEyeReduction;
    property RedEyeReductionSupported: Boolean read get_RedEyeReductionSupported;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.IWhiteBalanceControl
  IWhiteBalanceControl = interface(IInspectable)
  ['{781F047E-7162-49C8-A8F9-9481C565363E}']
    function get_Supported: Boolean; safecall;
    function get_Preset: ColorTemperaturePreset; safecall;
    function SetPresetAsync(preset: ColorTemperaturePreset): IAsyncAction; safecall;
    function get_Min: Cardinal; safecall;
    function get_Max: Cardinal; safecall;
    function get_Step: Cardinal; safecall;
    function get_Value: Cardinal; safecall;
    function SetValueAsync(temperature: Cardinal): IAsyncAction; safecall;
    property Max: Cardinal read get_Max;
    property Min: Cardinal read get_Min;
    property Preset: ColorTemperaturePreset read get_Preset;
    property Step: Cardinal read get_Step;
    property Supported: Boolean read get_Supported;
    property Value: Cardinal read get_Value;
  end;

  // Windows.Media.Devices.IExposureControl
  IExposureControl = interface(IInspectable)
  ['{09E8CBE2-AD96-4F28-A0E0-96ED7E1B5FD2}']
    function get_Supported: Boolean; safecall;
    function get_Auto: Boolean; safecall;
    function SetAutoAsync(value: Boolean): IAsyncAction; safecall;
    function get_Min: TimeSpan; safecall;
    function get_Max: TimeSpan; safecall;
    function get_Step: TimeSpan; safecall;
    function get_Value: TimeSpan; safecall;
    function SetValueAsync(shutterDuration: TimeSpan): IAsyncAction; safecall;
    property Auto: Boolean read get_Auto;
    property Max: TimeSpan read get_Max;
    property Min: TimeSpan read get_Min;
    property Step: TimeSpan read get_Step;
    property Supported: Boolean read get_Supported;
    property Value: TimeSpan read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.FocusPreset>
  IIterator_1__FocusPreset_Base = interface(IInspectable)
  ['{D3EBC8E9-F0C5-51C0-BB86-BDEA0A6946FB}']
    function get_Current: FocusPreset; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PFocusPreset): Cardinal; safecall;
    property Current: FocusPreset read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.FocusPreset>
  IIterator_1__FocusPreset = interface(IIterator_1__FocusPreset_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.FocusPreset>
  IIterable_1__FocusPreset_Base = interface(IInspectable)
  ['{26BA711B-3A32-5216-BC34-61ECAFBEBDC1}']
    function First: IIterator_1__FocusPreset; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.FocusPreset>
  IIterable_1__FocusPreset = interface(IIterable_1__FocusPreset_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.FocusPreset>
  IVectorView_1__FocusPreset = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): FocusPreset; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: FocusPreset; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PFocusPreset): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Devices.IFocusControl
  IFocusControl = interface(IInspectable)
  ['{C0D889F6-5228-4453-B153-85606592B238}']
    function get_Supported: Boolean; safecall;
    function get_SupportedPresets: IVectorView_1__FocusPreset; safecall;
    function get_Preset: FocusPreset; safecall;
    function SetPresetAsync(preset: FocusPreset): IAsyncAction; overload; safecall;
    function SetPresetAsync(preset: FocusPreset; completeBeforeFocus: Boolean): IAsyncAction; overload; safecall;
    function get_Min: Cardinal; safecall;
    function get_Max: Cardinal; safecall;
    function get_Step: Cardinal; safecall;
    function get_Value: Cardinal; safecall;
    function SetValueAsync(focus: Cardinal): IAsyncAction; safecall;
    function FocusAsync: IAsyncAction; safecall;
    property Max: Cardinal read get_Max;
    property Min: Cardinal read get_Min;
    property Preset: FocusPreset read get_Preset;
    property Step: Cardinal read get_Step;
    property Supported: Boolean read get_Supported;
    property SupportedPresets: IVectorView_1__FocusPreset read get_SupportedPresets;
    property Value: Cardinal read get_Value;
  end;

  // Windows.Media.Devices.IExposureCompensationControl
  IExposureCompensationControl = interface(IInspectable)
  ['{81C8E834-DCEC-4011-A610-1F3847E64ACA}']
    function get_Supported: Boolean; safecall;
    function get_Min: Single; safecall;
    function get_Max: Single; safecall;
    function get_Step: Single; safecall;
    function get_Value: Single; safecall;
    function SetValueAsync(value: Single): IAsyncAction; safecall;
    property Max: Single read get_Max;
    property Min: Single read get_Min;
    property Step: Single read get_Step;
    property Supported: Boolean read get_Supported;
    property Value: Single read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IsoSpeedPreset>
  IIterator_1__IsoSpeedPreset_Base = interface(IInspectable)
  ['{1B33AF76-980B-5348-916A-793F61B555A0}']
    function get_Current: IsoSpeedPreset; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIsoSpeedPreset): Cardinal; safecall;
    property Current: IsoSpeedPreset read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IsoSpeedPreset>
  IIterator_1__IsoSpeedPreset = interface(IIterator_1__IsoSpeedPreset_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IsoSpeedPreset>
  IIterable_1__IsoSpeedPreset_Base = interface(IInspectable)
  ['{94839ABE-9712-545A-A94D-A567A3E8DFB7}']
    function First: IIterator_1__IsoSpeedPreset; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IsoSpeedPreset>
  IIterable_1__IsoSpeedPreset = interface(IIterable_1__IsoSpeedPreset_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.IsoSpeedPreset>
  IVectorView_1__IsoSpeedPreset = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): IsoSpeedPreset; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IsoSpeedPreset; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIsoSpeedPreset): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Devices.IIsoSpeedControl
  IIsoSpeedControl = interface(IInspectable)
  ['{27B6C322-25AD-4F1B-AAAB-524AB376CA33}']
    function get_Supported: Boolean; safecall;
    function get_SupportedPresets: IVectorView_1__IsoSpeedPreset; safecall;
    function get_Preset: IsoSpeedPreset; safecall;
    function SetPresetAsync(preset: IsoSpeedPreset): IAsyncAction; safecall;
    property Preset: IsoSpeedPreset read get_Preset;
    property Supported: Boolean read get_Supported;
    property SupportedPresets: IVectorView_1__IsoSpeedPreset read get_SupportedPresets;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IRegionOfInterest
  [WinRTClassNameAttribute(SWindows_Media_Devices_RegionOfInterest)]
  IRegionOfInterest = interface(IInspectable)
  ['{E5ECC834-CE66-4E05-A78F-CF391A5EC2D1}']
    function get_AutoFocusEnabled: Boolean; safecall;
    procedure put_AutoFocusEnabled(value: Boolean); safecall;
    function get_AutoWhiteBalanceEnabled: Boolean; safecall;
    procedure put_AutoWhiteBalanceEnabled(value: Boolean); safecall;
    function get_AutoExposureEnabled: Boolean; safecall;
    procedure put_AutoExposureEnabled(value: Boolean); safecall;
    function get_Bounds: TRectF; safecall;
    procedure put_Bounds(value: TRectF); safecall;
    property AutoExposureEnabled: Boolean read get_AutoExposureEnabled write put_AutoExposureEnabled;
    property AutoFocusEnabled: Boolean read get_AutoFocusEnabled write put_AutoFocusEnabled;
    property AutoWhiteBalanceEnabled: Boolean read get_AutoWhiteBalanceEnabled write put_AutoWhiteBalanceEnabled;
    property Bounds: TRectF read get_Bounds write put_Bounds;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IRegionOfInterest>
  IIterator_1__IRegionOfInterest_Base = interface(IInspectable)
  ['{8EB80E4E-9691-594F-8B3D-F52ECC0F7837}']
    function get_Current: IRegionOfInterest; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIRegionOfInterest): Cardinal; safecall;
    property Current: IRegionOfInterest read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IRegionOfInterest>
  IIterator_1__IRegionOfInterest = interface(IIterator_1__IRegionOfInterest_Base)
  ['{1FC1158F-A90B-583F-803A-71B3C22D2686}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IRegionOfInterest>
  IIterable_1__IRegionOfInterest_Base = interface(IInspectable)
  ['{D73144C7-9D75-5DFB-8040-626202DCF454}']
    function First: IIterator_1__IRegionOfInterest; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IRegionOfInterest>
  IIterable_1__IRegionOfInterest = interface(IIterable_1__IRegionOfInterest_Base)
  ['{69260AC6-4C37-527E-A1AB-2EBCF083DE2B}']
  end;

  // Windows.Media.Devices.IRegionsOfInterestControl
  IRegionsOfInterestControl = interface(IInspectable)
  ['{C323F527-AB0B-4558-8B5B-DF5693DB0378}']
    function get_MaxRegions: Cardinal; safecall;
    function SetRegionsAsync(regions: IIterable_1__IRegionOfInterest): IAsyncAction; overload; safecall;
    function SetRegionsAsync(regions: IIterable_1__IRegionOfInterest; lockValues: Boolean): IAsyncAction; overload; safecall;
    function ClearRegionsAsync: IAsyncAction; safecall;
    function get_AutoFocusSupported: Boolean; safecall;
    function get_AutoWhiteBalanceSupported: Boolean; safecall;
    function get_AutoExposureSupported: Boolean; safecall;
    property AutoExposureSupported: Boolean read get_AutoExposureSupported;
    property AutoFocusSupported: Boolean read get_AutoFocusSupported;
    property AutoWhiteBalanceSupported: Boolean read get_AutoWhiteBalanceSupported;
    property MaxRegions: Cardinal read get_MaxRegions;
  end;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController2
  IAdvancedVideoCaptureDeviceController2 = interface(IInspectable)
  ['{8BB94F8F-F11A-43DB-B402-11930B80AE56}']
    function get_LowLagPhotoSequence: ILowLagPhotoSequenceControl; safecall;
    function get_LowLagPhoto: ILowLagPhotoControl; safecall;
    function get_SceneModeControl: ISceneModeControl; safecall;
    function get_TorchControl: ITorchControl; safecall;
    function get_FlashControl: IFlashControl; safecall;
    function get_WhiteBalanceControl: IWhiteBalanceControl; safecall;
    function get_ExposureControl: IExposureControl; safecall;
    function get_FocusControl: IFocusControl; safecall;
    function get_ExposureCompensationControl: IExposureCompensationControl; safecall;
    function get_IsoSpeedControl: IIsoSpeedControl; safecall;
    function get_RegionsOfInterestControl: IRegionsOfInterestControl; safecall;
    function get_PrimaryUse: CaptureUse; safecall;
    procedure put_PrimaryUse(value: CaptureUse); safecall;
    property ExposureCompensationControl: IExposureCompensationControl read get_ExposureCompensationControl;
    property ExposureControl: IExposureControl read get_ExposureControl;
    property FlashControl: IFlashControl read get_FlashControl;
    property FocusControl: IFocusControl read get_FocusControl;
    property IsoSpeedControl: IIsoSpeedControl read get_IsoSpeedControl;
    property LowLagPhoto: ILowLagPhotoControl read get_LowLagPhoto;
    property LowLagPhotoSequence: ILowLagPhotoSequenceControl read get_LowLagPhotoSequence;
    property PrimaryUse: CaptureUse read get_PrimaryUse write put_PrimaryUse;
    property RegionsOfInterestControl: IRegionsOfInterestControl read get_RegionsOfInterestControl;
    property SceneModeControl: ISceneModeControl read get_SceneModeControl;
    property TorchControl: ITorchControl read get_TorchControl;
    property WhiteBalanceControl: IWhiteBalanceControl read get_WhiteBalanceControl;
  end;

  // Windows.Media.Devices.IPhotoConfirmationControl
  IPhotoConfirmationControl = interface(IInspectable)
  ['{C8F3F363-FF5E-4582-A9A8-0550F85A4A76}']
    function get_Supported: Boolean; safecall;
    function get_Enabled: Boolean; safecall;
    procedure put_Enabled(value: Boolean); safecall;
    function get_PixelFormat: MediaPixelFormat; safecall;
    procedure put_PixelFormat(format: MediaPixelFormat); safecall;
    property Enabled: Boolean read get_Enabled write put_Enabled;
    property PixelFormat: MediaPixelFormat read get_PixelFormat write put_PixelFormat;
    property Supported: Boolean read get_Supported;
  end;

  // Windows.Media.Devices.IZoomControl
  IZoomControl = interface(IInspectable)
  ['{3A1E0B12-32DA-4C17-BFD7-8D0C73C8F5A5}']
    function get_Supported: Boolean; safecall;
    function get_Min: Single; safecall;
    function get_Max: Single; safecall;
    function get_Step: Single; safecall;
    function get_Value: Single; safecall;
    procedure put_Value(value: Single); safecall;
    property Max: Single read get_Max;
    property Min: Single read get_Min;
    property Step: Single read get_Step;
    property Supported: Boolean read get_Supported;
    property Value: Single read get_Value write put_Value;
  end;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController3
  IAdvancedVideoCaptureDeviceController3 = interface(IInspectable)
  ['{A98B8F34-EE0D-470C-B9F0-4229C4BBD089}']
    function get_VariablePhotoSequenceController: Core_IVariablePhotoSequenceController; safecall;
    function get_PhotoConfirmationControl: IPhotoConfirmationControl; safecall;
    function get_ZoomControl: IZoomControl; safecall;
    property PhotoConfirmationControl: IPhotoConfirmationControl read get_PhotoConfirmationControl;
    property VariablePhotoSequenceController: Core_IVariablePhotoSequenceController read get_VariablePhotoSequenceController;
    property ZoomControl: IZoomControl read get_ZoomControl;
  end;

  // Windows.Media.Devices.IExposurePriorityVideoControl
  IExposurePriorityVideoControl = interface(IInspectable)
  ['{2CB240A3-5168-4271-9EA5-47621A98A352}']
    function get_Supported: Boolean; safecall;
    function get_Enabled: Boolean; safecall;
    procedure put_Enabled(value: Boolean); safecall;
    property Enabled: Boolean read get_Enabled write put_Enabled;
    property Supported: Boolean read get_Supported;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.HdrVideoMode>
  IIterator_1__HdrVideoMode_Base = interface(IInspectable)
  ['{3DB61D13-0F30-5D2D-99CB-30C7B9009878}']
    function get_Current: HdrVideoMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PHdrVideoMode): Cardinal; safecall;
    property Current: HdrVideoMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.HdrVideoMode>
  IIterator_1__HdrVideoMode = interface(IIterator_1__HdrVideoMode_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.HdrVideoMode>
  IIterable_1__HdrVideoMode_Base = interface(IInspectable)
  ['{1D9679A7-2D06-5294-AC67-F9CD3561DCB8}']
    function First: IIterator_1__HdrVideoMode; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.HdrVideoMode>
  IIterable_1__HdrVideoMode = interface(IIterable_1__HdrVideoMode_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.HdrVideoMode>
  IVectorView_1__HdrVideoMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): HdrVideoMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: HdrVideoMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PHdrVideoMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Devices.IHdrVideoControl
  IHdrVideoControl = interface(IInspectable)
  ['{55D8E2D0-30C0-43BF-9B9A-9799D70CED94}']
    function get_Supported: Boolean; safecall;
    function get_SupportedModes: IVectorView_1__HdrVideoMode; safecall;
    function get_Mode: HdrVideoMode; safecall;
    procedure put_Mode(value: HdrVideoMode); safecall;
    property Mode: HdrVideoMode read get_Mode write put_Mode;
    property Supported: Boolean read get_Supported;
    property SupportedModes: IVectorView_1__HdrVideoMode read get_SupportedModes;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IIterator_1__OpticalImageStabilizationMode_Base = interface(IInspectable)
  ['{4A165D46-CF19-5A03-BB54-63FC2B4ED39B}']
    function get_Current: OpticalImageStabilizationMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: POpticalImageStabilizationMode): Cardinal; safecall;
    property Current: OpticalImageStabilizationMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IIterator_1__OpticalImageStabilizationMode = interface(IIterator_1__OpticalImageStabilizationMode_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IIterable_1__OpticalImageStabilizationMode_Base = interface(IInspectable)
  ['{323D7734-94C2-544D-A560-56560FE68819}']
    function First: IIterator_1__OpticalImageStabilizationMode; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IIterable_1__OpticalImageStabilizationMode = interface(IIterable_1__OpticalImageStabilizationMode_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.OpticalImageStabilizationMode>
  IVectorView_1__OpticalImageStabilizationMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): OpticalImageStabilizationMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: OpticalImageStabilizationMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: POpticalImageStabilizationMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Devices.IOpticalImageStabilizationControl
  IOpticalImageStabilizationControl = interface(IInspectable)
  ['{BFAD9C1D-00BC-423B-8EB2-A0178CA94247}']
    function get_Supported: Boolean; safecall;
    function get_SupportedModes: IVectorView_1__OpticalImageStabilizationMode; safecall;
    function get_Mode: OpticalImageStabilizationMode; safecall;
    procedure put_Mode(value: OpticalImageStabilizationMode); safecall;
    property Mode: OpticalImageStabilizationMode read get_Mode write put_Mode;
    property Supported: Boolean read get_Supported;
    property SupportedModes: IVectorView_1__OpticalImageStabilizationMode read get_SupportedModes;
  end;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController4
  IAdvancedVideoCaptureDeviceController4 = interface(IInspectable)
  ['{EA9FBFAF-D371-41C3-9A17-824A87EBDFD2}']
    function get_ExposurePriorityVideoControl: IExposurePriorityVideoControl; safecall;
    function get_DesiredOptimization: MediaCaptureOptimization; safecall;
    procedure put_DesiredOptimization(value: MediaCaptureOptimization); safecall;
    function get_HdrVideoControl: IHdrVideoControl; safecall;
    function get_OpticalImageStabilizationControl: IOpticalImageStabilizationControl; safecall;
    function get_AdvancedPhotoControl: IAdvancedPhotoControl; safecall;
    property AdvancedPhotoControl: IAdvancedPhotoControl read get_AdvancedPhotoControl;
    property DesiredOptimization: MediaCaptureOptimization read get_DesiredOptimization write put_DesiredOptimization;
    property ExposurePriorityVideoControl: IExposurePriorityVideoControl read get_ExposurePriorityVideoControl;
    property HdrVideoControl: IHdrVideoControl read get_HdrVideoControl;
    property OpticalImageStabilizationControl: IOpticalImageStabilizationControl read get_OpticalImageStabilizationControl;
  end;

  // Windows.Media.Devices.IVideoDeviceControllerGetDevicePropertyResult
  IVideoDeviceControllerGetDevicePropertyResult = interface(IInspectable)
  ['{C5D88395-6ED5-4790-8B5D-0EF13935D0F8}']
    function get_Status: VideoDeviceControllerGetDevicePropertyStatus; safecall;
    function get_Value: IInspectable; safecall;
    property Status: VideoDeviceControllerGetDevicePropertyStatus read get_Status;
    property Value: IInspectable read get_Value;
  end;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController5
  IAdvancedVideoCaptureDeviceController5 = interface(IInspectable)
  ['{33512B17-B9CB-4A23-B875-F9EAAB535492}']
    function get_Id: HSTRING; safecall;
    function GetDevicePropertyById(propertyId: HSTRING; maxPropertyValueSize: IReference_1__Cardinal): IVideoDeviceControllerGetDevicePropertyResult; safecall;
    function SetDevicePropertyById(propertyId: HSTRING; propertyValue: IInspectable): VideoDeviceControllerSetDevicePropertyStatus; safecall;
    function GetDevicePropertyByExtendedId(extendedPropertyIdSize: Cardinal; extendedPropertyId: PByte; maxPropertyValueSize: IReference_1__Cardinal): IVideoDeviceControllerGetDevicePropertyResult; safecall;
    function SetDevicePropertyByExtendedId(extendedPropertyIdSize: Cardinal; extendedPropertyId: PByte; propertyValueSize: Cardinal; propertyValue: PByte): VideoDeviceControllerSetDevicePropertyStatus; safecall;
    property Id: HSTRING read get_Id;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.VideoTemporalDenoisingMode>
  IIterator_1__VideoTemporalDenoisingMode = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: VideoTemporalDenoisingMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PVideoTemporalDenoisingMode): Cardinal; safecall;
    property Current: VideoTemporalDenoisingMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.VideoTemporalDenoisingMode>
  IIterable_1__VideoTemporalDenoisingMode = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__VideoTemporalDenoisingMode; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.VideoTemporalDenoisingMode>
  IVectorView_1__VideoTemporalDenoisingMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): VideoTemporalDenoisingMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: VideoTemporalDenoisingMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PVideoTemporalDenoisingMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.InfraredTorchMode>
  IIterator_1__InfraredTorchMode = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: InfraredTorchMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInfraredTorchMode): Cardinal; safecall;
    property Current: InfraredTorchMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.InfraredTorchMode>
  IIterable_1__InfraredTorchMode = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__InfraredTorchMode; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.InfraredTorchMode>
  IVectorView_1__InfraredTorchMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): InfraredTorchMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: InfraredTorchMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInfraredTorchMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Media.Devices.IPanelBasedOptimizationControl
  IPanelBasedOptimizationControl = interface(IInspectable)
  ['{33323223-6247-5419-A5A4-3D808645D917}']
    function get_IsSupported: Boolean; safecall;
    function get_Panel: Panel; safecall;
    procedure put_Panel(value: Panel); safecall;
    property IsSupported: Boolean read get_IsSupported;
    property Panel_: Panel read get_Panel write put_Panel;
  end;

  // Windows.Media.Devices.IAdvancedVideoCaptureDeviceController8
  IAdvancedVideoCaptureDeviceController8 = interface(IInspectable)
  ['{D843F010-E7FB-595B-9A78-0E54C4532B43}']
    function get_PanelBasedOptimizationControl: IPanelBasedOptimizationControl; safecall;
    property PanelBasedOptimizationControl: IPanelBasedOptimizationControl read get_PanelBasedOptimizationControl;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IModuleCommandResult
  IModuleCommandResult = interface(IInspectable)
  ['{520D1EB4-1374-4C7D-B1E4-39DCDF3EAE4E}']
    function get_Status: SendCommandStatus; safecall;
    function get_Result: IBuffer; safecall;
    property Result: IBuffer read get_Result;
    property Status: SendCommandStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Devices.IModuleCommandResult>
  AsyncOperationCompletedHandler_1__IModuleCommandResult_Delegate_Base = interface(IUnknown)
  ['{CB786404-F2E8-5E0B-BF12-39E31483CFAE}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IModuleCommandResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Media.Devices.IModuleCommandResult>
  AsyncOperationCompletedHandler_1__IModuleCommandResult = interface(AsyncOperationCompletedHandler_1__IModuleCommandResult_Delegate_Base)
  ['{AF62931E-6AE4-5655-9FDC-4E5F25C939DF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Devices.IModuleCommandResult>
  IAsyncOperation_1__IModuleCommandResult_Base = interface(IInspectable)
  ['{2E1F3D72-A58D-5B0A-B42D-3660C04CFEEB}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IModuleCommandResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IModuleCommandResult; safecall;
    function GetResults: IModuleCommandResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IModuleCommandResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Media.Devices.IModuleCommandResult>
  IAsyncOperation_1__IModuleCommandResult = interface(IAsyncOperation_1__IModuleCommandResult_Base)
  ['{2C6BF221-7556-53C2-A367-F1A5BB3819BD}']
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IAudioDeviceModule
  IAudioDeviceModule = interface(IInspectable)
  ['{86CFAC36-47C1-4B33-9852-8773EC4BE123}']
    function get_ClassId: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_InstanceId: Cardinal; safecall;
    function get_MajorVersion: Cardinal; safecall;
    function get_MinorVersion: Cardinal; safecall;
    function SendCommandAsync(Command: IBuffer): IAsyncOperation_1__IModuleCommandResult; safecall;
    property ClassId: HSTRING read get_ClassId;
    property DisplayName: HSTRING read get_DisplayName;
    property InstanceId: Cardinal read get_InstanceId;
    property MajorVersion: Cardinal read get_MajorVersion;
    property MinorVersion: Cardinal read get_MinorVersion;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IAudioDeviceModuleNotificationEventArgs
  IAudioDeviceModuleNotificationEventArgs = interface(IInspectable)
  ['{E3E3CCAF-224C-48BE-956B-9A13134E96E8}']
    function get_Module: IAudioDeviceModule; safecall;
    function get_NotificationData: IBuffer; safecall;
    property Module: IAudioDeviceModule read get_Module;
    property NotificationData: IBuffer read get_NotificationData;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Devices.IAudioDeviceModulesManager,Windows.Media.Devices.IAudioDeviceModuleNotificationEventArgs>
  TypedEventHandler_2__IAudioDeviceModulesManager__IAudioDeviceModuleNotificationEventArgs_Delegate_Base = interface(IUnknown)
  ['{B2F6B1FD-7092-5724-B2CE-91B1176E80E1}']
    procedure Invoke(sender: IAudioDeviceModulesManager; args: IAudioDeviceModuleNotificationEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Media.Devices.IAudioDeviceModulesManager,Windows.Media.Devices.IAudioDeviceModuleNotificationEventArgs>
  TypedEventHandler_2__IAudioDeviceModulesManager__IAudioDeviceModuleNotificationEventArgs = interface(TypedEventHandler_2__IAudioDeviceModulesManager__IAudioDeviceModuleNotificationEventArgs_Delegate_Base)
  ['{DBAEA1CD-E1B1-5484-96FC-8EFF3A2AEEB8}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IAudioDeviceModule>
  IIterator_1__IAudioDeviceModule_Base = interface(IInspectable)
  ['{B4CBBFB7-9851-56C9-839D-A10A8B1BB234}']
    function get_Current: IAudioDeviceModule; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIAudioDeviceModule): Cardinal; safecall;
    property Current: IAudioDeviceModule read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.IAudioDeviceModule>
  IIterator_1__IAudioDeviceModule = interface(IIterator_1__IAudioDeviceModule_Base)
  ['{97A6CDC2-0590-5937-8D32-9A68D3D90DCA}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IAudioDeviceModule>
  IIterable_1__IAudioDeviceModule_Base = interface(IInspectable)
  ['{7EEB51C3-D70E-548A-85C2-3CF71B4A124C}']
    function First: IIterator_1__IAudioDeviceModule; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.IAudioDeviceModule>
  IIterable_1__IAudioDeviceModule = interface(IIterable_1__IAudioDeviceModule_Base)
  ['{574EF793-9855-53DC-883D-F358ADA53082}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.IAudioDeviceModule>
  IVectorView_1__IAudioDeviceModule = interface(IInspectable)
  ['{CECC9411-B8C8-5BDD-9F4E-447D433667F2}']
    function GetAt(index: Cardinal): IAudioDeviceModule; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IAudioDeviceModule; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIAudioDeviceModule): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IAudioDeviceModulesManager
  [WinRTClassNameAttribute(SWindows_Media_Devices_AudioDeviceModulesManager)]
  IAudioDeviceModulesManager = interface(IInspectable)
  ['{6AA40C4D-960A-4D1C-B318-0022604547ED}']
    function add_ModuleNotificationReceived(handler: TypedEventHandler_2__IAudioDeviceModulesManager__IAudioDeviceModuleNotificationEventArgs): EventRegistrationToken; safecall;
    procedure remove_ModuleNotificationReceived(token: EventRegistrationToken); safecall;
    function FindAllById(moduleId: HSTRING): IVectorView_1__IAudioDeviceModule; safecall;
    function FindAll: IVectorView_1__IAudioDeviceModule; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IAudioDeviceModulesManagerFactory
  [WinRTClassNameAttribute(SWindows_Media_Devices_AudioDeviceModulesManager)]
  IAudioDeviceModulesManagerFactory = interface(IInspectable)
  ['{8DB03670-E64D-4773-96C0-BC7EBF0E063F}']
    function Create(deviceId: HSTRING): IAudioDeviceModulesManager; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.ICallControlStatics
  [WinRTClassNameAttribute(SWindows_Media_Devices_CallControl)]
  ICallControlStatics = interface(IInspectable)
  ['{03945AD5-85AB-40E1-AF19-56C94303B019}']
    function GetDefault: ICallControl; safecall;
    function FromId(deviceId: HSTRING): ICallControl; safecall;
  end;

  // UsedAPI Interface
  // Windows.Media.Devices.IDefaultAudioDeviceChangedEventArgs
  IDefaultAudioDeviceChangedEventArgs = interface(IInspectable)
  ['{110F882F-1C05-4657-A18E-47C9B69F07AB}']
    function get_Id: HSTRING; safecall;
    function get_Role: AudioDeviceRole; safecall;
    property Id: HSTRING read get_Id;
    property Role: AudioDeviceRole read get_Role;
  end;

  // Windows.Media.Devices.IFlashControl2
  IFlashControl2 = interface(IInspectable)
  ['{7D29CC9E-75E1-4AF7-BD7D-4E38E1C06CD6}']
    function get_AssistantLightSupported: Boolean; safecall;
    function get_AssistantLightEnabled: Boolean; safecall;
    procedure put_AssistantLightEnabled(value: Boolean); safecall;
    property AssistantLightEnabled: Boolean read get_AssistantLightEnabled write put_AssistantLightEnabled;
    property AssistantLightSupported: Boolean read get_AssistantLightSupported;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.FocusMode>
  IIterator_1__FocusMode_Base = interface(IInspectable)
  ['{F9A43CD4-B300-541F-AF79-3DE3400E16AF}']
    function get_Current: FocusMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PFocusMode): Cardinal; safecall;
    property Current: FocusMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.FocusMode>
  IIterator_1__FocusMode = interface(IIterator_1__FocusMode_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.FocusMode>
  IIterable_1__FocusMode_Base = interface(IInspectable)
  ['{561BC21F-4AE2-580A-A216-0AD48F373A4C}']
    function First: IIterator_1__FocusMode; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.FocusMode>
  IIterable_1__FocusMode = interface(IIterable_1__FocusMode_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.FocusMode>
  IVectorView_1__FocusMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): FocusMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: FocusMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PFocusMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.ManualFocusDistance>
  IIterator_1__ManualFocusDistance_Base = interface(IInspectable)
  ['{B02944E1-F649-511E-80DD-2E2B20379DEB}']
    function get_Current: ManualFocusDistance; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PManualFocusDistance): Cardinal; safecall;
    property Current: ManualFocusDistance read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.ManualFocusDistance>
  IIterator_1__ManualFocusDistance = interface(IIterator_1__ManualFocusDistance_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.ManualFocusDistance>
  IIterable_1__ManualFocusDistance_Base = interface(IInspectable)
  ['{CF8CBEB1-2A4C-522D-962F-84C31A598D68}']
    function First: IIterator_1__ManualFocusDistance; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.ManualFocusDistance>
  IIterable_1__ManualFocusDistance = interface(IIterable_1__ManualFocusDistance_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.ManualFocusDistance>
  IVectorView_1__ManualFocusDistance = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): ManualFocusDistance; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ManualFocusDistance; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PManualFocusDistance): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.AutoFocusRange>
  IIterator_1__AutoFocusRange_Base = interface(IInspectable)
  ['{07489AC5-3C71-59C6-B7DC-7F21341C2F71}']
    function get_Current: AutoFocusRange; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAutoFocusRange): Cardinal; safecall;
    property Current: AutoFocusRange read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.AutoFocusRange>
  IIterator_1__AutoFocusRange = interface(IIterator_1__AutoFocusRange_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.AutoFocusRange>
  IIterable_1__AutoFocusRange_Base = interface(IInspectable)
  ['{751664C6-F8D6-50A3-AB80-137C6D908C55}']
    function First: IIterator_1__AutoFocusRange; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.AutoFocusRange>
  IIterable_1__AutoFocusRange = interface(IIterable_1__AutoFocusRange_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.AutoFocusRange>
  IVectorView_1__AutoFocusRange = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): AutoFocusRange; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: AutoFocusRange; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAutoFocusRange): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Media.Devices.ManualFocusDistance>
  IReference_1__ManualFocusDistance = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: ManualFocusDistance; safecall;
    property Value: ManualFocusDistance read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IFocusSettings
  [WinRTClassNameAttribute(SWindows_Media_Devices_FocusSettings)]
  IFocusSettings = interface(IInspectable)
  ['{79958F6B-3263-4275-85D6-AEAE891C96EE}']
    function get_Mode: FocusMode; safecall;
    procedure put_Mode(value: FocusMode); safecall;
    function get_AutoFocusRange: AutoFocusRange; safecall;
    procedure put_AutoFocusRange(value: AutoFocusRange); safecall;
    function get_Value: IReference_1__Cardinal; safecall;
    procedure put_Value(value: IReference_1__Cardinal); safecall;
    function get_Distance: IReference_1__ManualFocusDistance; safecall;
    procedure put_Distance(value: IReference_1__ManualFocusDistance); safecall;
    function get_WaitForFocus: Boolean; safecall;
    procedure put_WaitForFocus(value: Boolean); safecall;
    function get_DisableDriverFallback: Boolean; safecall;
    procedure put_DisableDriverFallback(value: Boolean); safecall;
    property AutoFocusRange_: AutoFocusRange read get_AutoFocusRange write put_AutoFocusRange;
    property DisableDriverFallback: Boolean read get_DisableDriverFallback write put_DisableDriverFallback;
    property Distance: IReference_1__ManualFocusDistance read get_Distance write put_Distance;
    property Mode: FocusMode read get_Mode write put_Mode;
    property Value: IReference_1__Cardinal read get_Value write put_Value;
    property WaitForFocus: Boolean read get_WaitForFocus write put_WaitForFocus;
  end;

  // Windows.Media.Devices.IFocusControl2
  IFocusControl2 = interface(IInspectable)
  ['{3F7CFF48-C534-4E9E-94C3-52EF2AFD5D07}']
    function get_FocusChangedSupported: Boolean; safecall;
    function get_WaitForFocusSupported: Boolean; safecall;
    function get_SupportedFocusModes: IVectorView_1__FocusMode; safecall;
    function get_SupportedFocusDistances: IVectorView_1__ManualFocusDistance; safecall;
    function get_SupportedFocusRanges: IVectorView_1__AutoFocusRange; safecall;
    function get_Mode: FocusMode; safecall;
    function get_FocusState: MediaCaptureFocusState; safecall;
    function UnlockAsync: IAsyncAction; safecall;
    function LockAsync: IAsyncAction; safecall;
    procedure Configure(settings: IFocusSettings); safecall;
    property FocusChangedSupported: Boolean read get_FocusChangedSupported;
    property FocusState: MediaCaptureFocusState read get_FocusState;
    property Mode: FocusMode read get_Mode;
    property SupportedFocusDistances: IVectorView_1__ManualFocusDistance read get_SupportedFocusDistances;
    property SupportedFocusModes: IVectorView_1__FocusMode read get_SupportedFocusModes;
    property SupportedFocusRanges: IVectorView_1__AutoFocusRange read get_SupportedFocusRanges;
    property WaitForFocusSupported: Boolean read get_WaitForFocusSupported;
  end;

  // Windows.Media.Devices.IIsoSpeedControl2
  IIsoSpeedControl2 = interface(IInspectable)
  ['{6F1578F2-6D77-4F8A-8C2F-6130B6395053}']
    function get_Min: Cardinal; safecall;
    function get_Max: Cardinal; safecall;
    function get_Step: Cardinal; safecall;
    function get_Value: Cardinal; safecall;
    function SetValueAsync(isoSpeed: Cardinal): IAsyncAction; safecall;
    function get_Auto: Boolean; safecall;
    function SetAutoAsync: IAsyncAction; safecall;
    property Auto: Boolean read get_Auto;
    property Max: Cardinal read get_Max;
    property Min: Cardinal read get_Min;
    property Step: Cardinal read get_Step;
    property Value: Cardinal read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.Media.Devices.IDefaultAudioDeviceChangedEventArgs>
  TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs = interface(IUnknown)
  ['{D600194A-0919-5223-A074-C52D5C5177DC}']
    procedure Invoke(sender: IInspectable; args: IDefaultAudioDeviceChangedEventArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IMediaDeviceStatics
  [WinRTClassNameAttribute(SWindows_Media_Devices_MediaDevice)]
  IMediaDeviceStatics = interface(IInspectable)
  ['{AA2D9A40-909F-4BBA-BF8B-0C0D296F14F0}']
    function GetAudioCaptureSelector: HSTRING; safecall;
    function GetAudioRenderSelector: HSTRING; safecall;
    function GetVideoCaptureSelector: HSTRING; safecall;
    function GetDefaultAudioCaptureId(role: AudioDeviceRole): HSTRING; safecall;
    function GetDefaultAudioRenderId(role: AudioDeviceRole): HSTRING; safecall;
    function add_DefaultAudioCaptureDeviceChanged(handler: TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_DefaultAudioCaptureDeviceChanged(cookie: EventRegistrationToken); safecall;
    function add_DefaultAudioRenderDeviceChanged(handler: TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_DefaultAudioRenderDeviceChanged(cookie: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IRegionOfInterest2
  IRegionOfInterest2 = interface(IInspectable)
  ['{19FE2A91-73AA-4D51-8A9D-56CCF7DB7F54}']
    function get_Type: RegionOfInterestType; safecall;
    procedure put_Type(value: RegionOfInterestType); safecall;
    function get_BoundsNormalized: Boolean; safecall;
    procedure put_BoundsNormalized(value: Boolean); safecall;
    function get_Weight: Cardinal; safecall;
    procedure put_Weight(value: Cardinal); safecall;
    property BoundsNormalized: Boolean read get_BoundsNormalized write put_BoundsNormalized;
    property &Type: RegionOfInterestType read get_Type write put_Type;
    property Weight: Cardinal read get_Weight write put_Weight;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.ZoomTransitionMode>
  IIterator_1__ZoomTransitionMode_Base = interface(IInspectable)
  ['{80EB468A-FDC4-5C89-99B8-8D476264E211}']
    function get_Current: ZoomTransitionMode; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PZoomTransitionMode): Cardinal; safecall;
    property Current: ZoomTransitionMode read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Media.Devices.ZoomTransitionMode>
  IIterator_1__ZoomTransitionMode = interface(IIterator_1__ZoomTransitionMode_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.ZoomTransitionMode>
  IIterable_1__ZoomTransitionMode_Base = interface(IInspectable)
  ['{DB656915-8FAC-5FB2-98E0-0E97421656C5}']
    function First: IIterator_1__ZoomTransitionMode; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Media.Devices.ZoomTransitionMode>
  IIterable_1__ZoomTransitionMode = interface(IIterable_1__ZoomTransitionMode_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Media.Devices.ZoomTransitionMode>
  IVectorView_1__ZoomTransitionMode = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): ZoomTransitionMode; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ZoomTransitionMode; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PZoomTransitionMode): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Media.Devices.IZoomSettings
  [WinRTClassNameAttribute(SWindows_Media_Devices_ZoomSettings)]
  IZoomSettings = interface(IInspectable)
  ['{6AD66B24-14B4-4BFD-B18F-88FE24463B52}']
    function get_Mode: ZoomTransitionMode; safecall;
    procedure put_Mode(value: ZoomTransitionMode); safecall;
    function get_Value: Single; safecall;
    procedure put_Value(value: Single); safecall;
    property Mode: ZoomTransitionMode read get_Mode write put_Mode;
    property Value: Single read get_Value write put_Value;
  end;

  // Windows.Media.Devices.IZoomControl2
  IZoomControl2 = interface(IInspectable)
  ['{69843DB0-2E99-4641-8529-184F319D1671}']
    function get_SupportedModes: IVectorView_1__ZoomTransitionMode; safecall;
    function get_Mode: ZoomTransitionMode; safecall;
    procedure Configure(settings: IZoomSettings); safecall;
    property Mode: ZoomTransitionMode read get_Mode;
    property SupportedModes: IVectorView_1__ZoomTransitionMode read get_SupportedModes;
  end;

  // Windows.Media.Devices.AdvancedPhotoCaptureSettings
  // DualAPI
  // Implements: Windows.Media.Devices.IAdvancedPhotoCaptureSettings
  // Instantiable: "IAdvancedPhotoCaptureSettings"
  TAdvancedPhotoCaptureSettings = class(TWinRTGenericImportI<IAdvancedPhotoCaptureSettings>) end;

  // Windows.Media.Devices.AudioDeviceModulesManager
  // DualAPI
  // Implements: Windows.Media.Devices.IAudioDeviceModulesManager
  // Factory: "Windows.Media.Devices.IAudioDeviceModulesManagerFactory"
  TAudioDeviceModulesManager = class(TWinRTGenericImportF<IAudioDeviceModulesManagerFactory>)
  public
    // -> IAudioDeviceModulesManagerFactory
    class function Create(deviceId: HSTRING): IAudioDeviceModulesManager; static; inline;
  end;

  // Windows.Media.Devices.CallControl
  // DualAPI
  // Implements: Windows.Media.Devices.ICallControl
  // Statics: "Windows.Media.Devices.ICallControlStatics"
  TCallControl = class(TWinRTGenericImportS<ICallControlStatics>)
  public
    // -> ICallControlStatics
    class function GetDefault: ICallControl; static; inline;
    class function FromId(deviceId: HSTRING): ICallControl; static; inline;
  end;

  // Windows.Media.Devices.Core.CameraIntrinsics
  // DualAPI
  // Implements: Windows.Media.Devices.Core.ICameraIntrinsics
  // Implements: Windows.Media.Devices.Core.ICameraIntrinsics2
  // Factory: "Windows.Media.Devices.Core.ICameraIntrinsicsFactory"
  TCore_CameraIntrinsics = class(TWinRTGenericImportF<Core_ICameraIntrinsicsFactory>)
  public
    // -> Core_ICameraIntrinsicsFactory
    class function Create(focalLength: Numerics_Vector2; principalPoint: Numerics_Vector2; radialDistortion: Numerics_Vector3; tangentialDistortion: Numerics_Vector2; imageWidth: Cardinal; imageHeight: Cardinal): Core_ICameraIntrinsics; static; inline;
  end;

  // Windows.Media.Devices.Core.FrameController
  // DualAPI
  // Implements: Windows.Media.Devices.Core.IFrameController
  // Implements: Windows.Media.Devices.Core.IFrameController2
  // Instantiable: "Core_IFrameController"
  TCore_FrameController = class(TWinRTGenericImportI<Core_IFrameController>) end;

  // Windows.Media.Devices.FocusSettings
  // DualAPI
  // Implements: Windows.Media.Devices.IFocusSettings
  // Instantiable: "IFocusSettings"
  TFocusSettings = class(TWinRTGenericImportI<IFocusSettings>) end;

  // Windows.Media.Devices.MediaDevice
  // DualAPI
  // Statics: "Windows.Media.Devices.IMediaDeviceStatics"
  TMediaDevice = class(TWinRTGenericImportS<IMediaDeviceStatics>)
  public
    // -> IMediaDeviceStatics
    class function GetAudioCaptureSelector: HSTRING; static; inline;
    class function GetAudioRenderSelector: HSTRING; static; inline;
    class function GetVideoCaptureSelector: HSTRING; static; inline;
    class function GetDefaultAudioCaptureId(role: AudioDeviceRole): HSTRING; static; inline;
    class function GetDefaultAudioRenderId(role: AudioDeviceRole): HSTRING; static; inline;
    class function add_DefaultAudioCaptureDeviceChanged(handler: TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_DefaultAudioCaptureDeviceChanged(cookie: EventRegistrationToken); static; inline;
    class function add_DefaultAudioRenderDeviceChanged(handler: TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_DefaultAudioRenderDeviceChanged(cookie: EventRegistrationToken); static; inline;
  end;

  // Windows.Media.Devices.RegionOfInterest
  // DualAPI
  // Implements: Windows.Media.Devices.IRegionOfInterest
  // Implements: Windows.Media.Devices.IRegionOfInterest2
  // Instantiable: "IRegionOfInterest"
  TRegionOfInterest = class(TWinRTGenericImportI<IRegionOfInterest>) end;

  // Windows.Media.Devices.ZoomSettings
  // DualAPI
  // Implements: Windows.Media.Devices.IZoomSettings
  // Instantiable: "IZoomSettings"
  TZoomSettings = class(TWinRTGenericImportI<IZoomSettings>) end;

implementation

{ TAdvancedPhotoCaptureSettings }

{ TAudioDeviceModulesManager }
// Factories for : "AudioDeviceModulesManager"
// Factory: "Windows.Media.Devices.IAudioDeviceModulesManagerFactory"
// -> IAudioDeviceModulesManagerFactory

class function TAudioDeviceModulesManager.Create(deviceId: HSTRING): IAudioDeviceModulesManager;
begin
  Result := Factory.Create(deviceId);
end;


{ TCallControl }

class function TCallControl.GetDefault: ICallControl;
begin
  Result := Statics.GetDefault;
end;

class function TCallControl.FromId(deviceId: HSTRING): ICallControl;
begin
  Result := Statics.FromId(deviceId);
end;


{ TCore_CameraIntrinsics }
// Factories for : "Core_CameraIntrinsics"
// Factory: "Windows.Media.Devices.Core.ICameraIntrinsicsFactory"
// -> Core_ICameraIntrinsicsFactory

class function TCore_CameraIntrinsics.Create(focalLength: Numerics_Vector2; principalPoint: Numerics_Vector2; radialDistortion: Numerics_Vector3; tangentialDistortion: Numerics_Vector2; imageWidth: Cardinal; imageHeight: Cardinal): Core_ICameraIntrinsics;
begin
  Result := Factory.Create(focalLength, principalPoint, radialDistortion, tangentialDistortion, imageWidth, imageHeight);
end;


{ TCore_FrameController }

{ TFocusSettings }

{ TMediaDevice }

class function TMediaDevice.GetAudioCaptureSelector: HSTRING;
begin
  Result := Statics.GetAudioCaptureSelector;
end;

class function TMediaDevice.GetAudioRenderSelector: HSTRING;
begin
  Result := Statics.GetAudioRenderSelector;
end;

class function TMediaDevice.GetVideoCaptureSelector: HSTRING;
begin
  Result := Statics.GetVideoCaptureSelector;
end;

class function TMediaDevice.GetDefaultAudioCaptureId(role: AudioDeviceRole): HSTRING;
begin
  Result := Statics.GetDefaultAudioCaptureId(role);
end;

class function TMediaDevice.GetDefaultAudioRenderId(role: AudioDeviceRole): HSTRING;
begin
  Result := Statics.GetDefaultAudioRenderId(role);
end;

class function TMediaDevice.add_DefaultAudioCaptureDeviceChanged(handler: TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_DefaultAudioCaptureDeviceChanged(handler);
end;

class procedure TMediaDevice.remove_DefaultAudioCaptureDeviceChanged(cookie: EventRegistrationToken);
begin
  Statics.remove_DefaultAudioCaptureDeviceChanged(cookie);
end;

class function TMediaDevice.add_DefaultAudioRenderDeviceChanged(handler: TypedEventHandler_2__IInspectable__IDefaultAudioDeviceChangedEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_DefaultAudioRenderDeviceChanged(handler);
end;

class procedure TMediaDevice.remove_DefaultAudioRenderDeviceChanged(cookie: EventRegistrationToken);
begin
  Statics.remove_DefaultAudioRenderDeviceChanged(cookie);
end;


{ TRegionOfInterest }

{ TZoomSettings }

end.
