{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI.Composition;

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
  Winapi.GraphicsRT, 
  Winapi.Foundation, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  CompositionBackfaceVisibility = Winapi.CommonTypes.CompositionBackfaceVisibility;
  PCompositionBackfaceVisibility = Winapi.CommonTypes.PCompositionBackfaceVisibility;
  CompositionBatchTypes = Winapi.CommonTypes.CompositionBatchTypes;
  PCompositionBatchTypes = Winapi.CommonTypes.PCompositionBatchTypes;
  CompositionBitmapInterpolationMode = Winapi.CommonTypes.CompositionBitmapInterpolationMode;
  PCompositionBitmapInterpolationMode = Winapi.CommonTypes.PCompositionBitmapInterpolationMode;
  CompositionBorderMode = Winapi.CommonTypes.CompositionBorderMode;
  PCompositionBorderMode = Winapi.CommonTypes.PCompositionBorderMode;
  CompositionColorSpace = Winapi.CommonTypes.CompositionColorSpace;
  PCompositionColorSpace = Winapi.CommonTypes.PCompositionColorSpace;
  CompositionCompositeMode = Winapi.CommonTypes.CompositionCompositeMode;
  PCompositionCompositeMode = Winapi.CommonTypes.PCompositionCompositeMode;
  CompositionEffectFactoryLoadStatus = Winapi.CommonTypes.CompositionEffectFactoryLoadStatus;
  PCompositionEffectFactoryLoadStatus = Winapi.CommonTypes.PCompositionEffectFactoryLoadStatus;
  CompositionGetValueStatus = Winapi.CommonTypes.CompositionGetValueStatus;
  PCompositionGetValueStatus = Winapi.CommonTypes.PCompositionGetValueStatus;
  CompositionStretch = Winapi.CommonTypes.CompositionStretch;
  PCompositionStretch = Winapi.CommonTypes.PCompositionStretch;
  Core_ICompositorController = Winapi.CommonTypes.Core_ICompositorController;
  PCore_ICompositorController = Winapi.CommonTypes.PCore_ICompositorController;
  IColorKeyFrameAnimation = Winapi.CommonTypes.IColorKeyFrameAnimation;
  PIColorKeyFrameAnimation = Winapi.CommonTypes.PIColorKeyFrameAnimation;
  ICompositionBatchCompletedEventArgs = Winapi.CommonTypes.ICompositionBatchCompletedEventArgs;
  PICompositionBatchCompletedEventArgs = Winapi.CommonTypes.PICompositionBatchCompletedEventArgs;
  ICompositionBrush = Winapi.CommonTypes.ICompositionBrush;
  PICompositionBrush = Winapi.CommonTypes.PICompositionBrush;
  ICompositionCapabilities = Winapi.CommonTypes.ICompositionCapabilities;
  PICompositionCapabilities = Winapi.CommonTypes.PICompositionCapabilities;
  ICompositionClip = Winapi.CommonTypes.ICompositionClip;
  PICompositionClip = Winapi.CommonTypes.PICompositionClip;
  ICompositionColorBrush = Winapi.CommonTypes.ICompositionColorBrush;
  PICompositionColorBrush = Winapi.CommonTypes.PICompositionColorBrush;
  ICompositionCommitBatch = Winapi.CommonTypes.ICompositionCommitBatch;
  PICompositionCommitBatch = Winapi.CommonTypes.PICompositionCommitBatch;
  ICompositionEasingFunction = Winapi.CommonTypes.ICompositionEasingFunction;
  PICompositionEasingFunction = Winapi.CommonTypes.PICompositionEasingFunction;
  ICompositionEffectBrush = Winapi.CommonTypes.ICompositionEffectBrush;
  PICompositionEffectBrush = Winapi.CommonTypes.PICompositionEffectBrush;
  ICompositionEffectFactory = Winapi.CommonTypes.ICompositionEffectFactory;
  PICompositionEffectFactory = Winapi.CommonTypes.PICompositionEffectFactory;
  ICompositionPropertySet = Winapi.CommonTypes.ICompositionPropertySet;
  PICompositionPropertySet = Winapi.CommonTypes.PICompositionPropertySet;
  ICompositionScopedBatch = Winapi.CommonTypes.ICompositionScopedBatch;
  PICompositionScopedBatch = Winapi.CommonTypes.PICompositionScopedBatch;
  ICompositionSurface = Winapi.CommonTypes.ICompositionSurface;
  PICompositionSurface = Winapi.CommonTypes.PICompositionSurface;
  ICompositionSurfaceBrush = Winapi.CommonTypes.ICompositionSurfaceBrush;
  PICompositionSurfaceBrush = Winapi.CommonTypes.PICompositionSurfaceBrush;
  ICompositionTarget = Winapi.CommonTypes.ICompositionTarget;
  PICompositionTarget = Winapi.CommonTypes.PICompositionTarget;
  ICompositor = Winapi.CommonTypes.ICompositor;
  PICompositor = Winapi.CommonTypes.PICompositor;
  IContainerVisual = Winapi.CommonTypes.IContainerVisual;
  PIContainerVisual = Winapi.CommonTypes.PIContainerVisual;
  ICubicBezierEasingFunction = Winapi.CommonTypes.ICubicBezierEasingFunction;
  PICubicBezierEasingFunction = Winapi.CommonTypes.PICubicBezierEasingFunction;
  IExpressionAnimation = Winapi.CommonTypes.IExpressionAnimation;
  PIExpressionAnimation = Winapi.CommonTypes.PIExpressionAnimation;
  IInsetClip = Winapi.CommonTypes.IInsetClip;
  PIInsetClip = Winapi.CommonTypes.PIInsetClip;
  ILinearEasingFunction = Winapi.CommonTypes.ILinearEasingFunction;
  PILinearEasingFunction = Winapi.CommonTypes.PILinearEasingFunction;
  IQuaternionKeyFrameAnimation = Winapi.CommonTypes.IQuaternionKeyFrameAnimation;
  PIQuaternionKeyFrameAnimation = Winapi.CommonTypes.PIQuaternionKeyFrameAnimation;
  IScalarKeyFrameAnimation = Winapi.CommonTypes.IScalarKeyFrameAnimation;
  PIScalarKeyFrameAnimation = Winapi.CommonTypes.PIScalarKeyFrameAnimation;
  ISpriteVisual = Winapi.CommonTypes.ISpriteVisual;
  PISpriteVisual = Winapi.CommonTypes.PISpriteVisual;
  IVector2KeyFrameAnimation = Winapi.CommonTypes.IVector2KeyFrameAnimation;
  PIVector2KeyFrameAnimation = Winapi.CommonTypes.PIVector2KeyFrameAnimation;
  IVector3KeyFrameAnimation = Winapi.CommonTypes.IVector3KeyFrameAnimation;
  PIVector3KeyFrameAnimation = Winapi.CommonTypes.PIVector3KeyFrameAnimation;
  IVector4KeyFrameAnimation = Winapi.CommonTypes.IVector4KeyFrameAnimation;
  PIVector4KeyFrameAnimation = Winapi.CommonTypes.PIVector4KeyFrameAnimation;
  IVisual = Winapi.CommonTypes.IVisual;
  PIVisual = Winapi.CommonTypes.PIVisual;
  IVisualCollection = Winapi.CommonTypes.IVisualCollection;
  PIVisualCollection = Winapi.CommonTypes.PIVisualCollection;
  TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs_Delegate_Base;
  TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs;
  PTypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IInspectable__ICompositionBatchCompletedEventArgs;

  // Forward declarations for interfaces

  // Windows.UI.Composition.ICompositionAnimationBase
  ICompositionAnimationBase = interface;
  PICompositionAnimationBase = ^ICompositionAnimationBase;

  // Windows.UI.Composition.IAnimationPropertyInfo
  IAnimationPropertyInfo = interface;
  PIAnimationPropertyInfo = ^IAnimationPropertyInfo;

  // Windows.UI.Composition.IVisualUnorderedCollection
  IVisualUnorderedCollection = interface;
  PIVisualUnorderedCollection = ^IVisualUnorderedCollection;

  // Windows.UI.Composition.ICompositionLight
  ICompositionLight = interface;
  PICompositionLight = ^ICompositionLight;

  // Windows.UI.Composition.Diagnostics.ICompositionDebugHeatMaps
  Diagnostics_ICompositionDebugHeatMaps = interface;
  PDiagnostics_ICompositionDebugHeatMaps = ^Diagnostics_ICompositionDebugHeatMaps;

  // Windows.UI.Composition.Diagnostics.ICompositionDebugSettings
  Diagnostics_ICompositionDebugSettings = interface;
  PDiagnostics_ICompositionDebugSettings = ^Diagnostics_ICompositionDebugSettings;

  // Windows.UI.Composition.Diagnostics.ICompositionDebugSettingsStatics
  Diagnostics_ICompositionDebugSettingsStatics = interface;
  PDiagnostics_ICompositionDebugSettingsStatics = ^Diagnostics_ICompositionDebugSettingsStatics;

  // Windows.UI.Composition.Effects.ISceneLightingEffect
  Effects_ISceneLightingEffect = interface;
  PEffects_ISceneLightingEffect = ^Effects_ISceneLightingEffect;

  // Windows.UI.Composition.Effects.ISceneLightingEffect2
  Effects_ISceneLightingEffect2 = interface;
  PEffects_ISceneLightingEffect2 = ^Effects_ISceneLightingEffect2;

  // Windows.UI.Composition.IAmbientLight
  IAmbientLight = interface;
  PIAmbientLight = ^IAmbientLight;

  // Windows.UI.Composition.IAmbientLight2
  IAmbientLight2 = interface;
  PIAmbientLight2 = ^IAmbientLight2;

  // Windows.UI.Composition.IAnimationController
  IAnimationController = interface;
  PIAnimationController = ^IAnimationController;

  // Windows.UI.Composition.IAnimationControllerStatics
  IAnimationControllerStatics = interface;
  PIAnimationControllerStatics = ^IAnimationControllerStatics;

  // Windows.UI.Composition.IAnimationObject
  IAnimationObject = interface;
  PIAnimationObject = ^IAnimationObject;

  // Windows.UI.Composition.IBooleanKeyFrameAnimation
  IBooleanKeyFrameAnimation = interface;
  PIBooleanKeyFrameAnimation = ^IBooleanKeyFrameAnimation;

  // Windows.UI.Composition.IBounceScalarNaturalMotionAnimation
  IBounceScalarNaturalMotionAnimation = interface;
  PIBounceScalarNaturalMotionAnimation = ^IBounceScalarNaturalMotionAnimation;

  // Windows.UI.Composition.IBounceVector2NaturalMotionAnimation
  IBounceVector2NaturalMotionAnimation = interface;
  PIBounceVector2NaturalMotionAnimation = ^IBounceVector2NaturalMotionAnimation;

  // Windows.UI.Composition.IBounceVector3NaturalMotionAnimation
  IBounceVector3NaturalMotionAnimation = interface;
  PIBounceVector3NaturalMotionAnimation = ^IBounceVector3NaturalMotionAnimation;

  // Windows.UI.Composition.ICompositionObject
  ICompositionObject = interface;
  PICompositionObject = ^ICompositionObject;

  // Windows.UI.Composition.ICompositionAnimation
  ICompositionAnimation = interface;
  PICompositionAnimation = ^ICompositionAnimation;

  // Windows.UI.Composition.ICompositionAnimation2
  ICompositionAnimation2 = interface;
  PICompositionAnimation2 = ^ICompositionAnimation2;

  // Windows.UI.Composition.ICompositionAnimation3
  ICompositionAnimation3 = interface;
  PICompositionAnimation3 = ^ICompositionAnimation3;

  // Windows.UI.Composition.ICompositionAnimation4
  ICompositionAnimation4 = interface;
  PICompositionAnimation4 = ^ICompositionAnimation4;

  // Windows.UI.Composition.ICompositionAnimationFactory
  ICompositionAnimationFactory = interface;
  PICompositionAnimationFactory = ^ICompositionAnimationFactory;

  // Windows.UI.Composition.ICompositionAnimationGroup
  ICompositionAnimationGroup = interface;
  PICompositionAnimationGroup = ^ICompositionAnimationGroup;

  // Windows.UI.Composition.ICompositionBackdropBrush
  ICompositionBackdropBrush = interface;
  PICompositionBackdropBrush = ^ICompositionBackdropBrush;

  // Windows.UI.Composition.ICompositionBrushFactory
  ICompositionBrushFactory = interface;
  PICompositionBrushFactory = ^ICompositionBrushFactory;

  // Windows.UI.Composition.ICompositionCapabilitiesStatics
  ICompositionCapabilitiesStatics = interface;
  PICompositionCapabilitiesStatics = ^ICompositionCapabilitiesStatics;

  // Windows.UI.Composition.ICompositionClip2
  ICompositionClip2 = interface;
  PICompositionClip2 = ^ICompositionClip2;

  // Windows.UI.Composition.ICompositionClipFactory
  ICompositionClipFactory = interface;
  PICompositionClipFactory = ^ICompositionClipFactory;

  // Windows.UI.Composition.ICompositionColorGradientStop
  ICompositionColorGradientStop = interface;
  PICompositionColorGradientStop = ^ICompositionColorGradientStop;

  // Windows.UI.Composition.ICompositionShape
  ICompositionShape = interface;
  PICompositionShape = ^ICompositionShape;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.ICompositionShape>
  IIterator_1__ICompositionShape = interface;
  PIIterator_1__ICompositionShape = ^IIterator_1__ICompositionShape;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.ICompositionShape>
  IIterable_1__ICompositionShape = interface;
  PIIterable_1__ICompositionShape = ^IIterable_1__ICompositionShape;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Composition.ICompositionShape>
  IVectorView_1__ICompositionShape = interface;
  PIVectorView_1__ICompositionShape = ^IVectorView_1__ICompositionShape;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Composition.ICompositionShape>
  IVector_1__ICompositionShape = interface;
  PIVector_1__ICompositionShape = ^IVector_1__ICompositionShape;

  // Windows.UI.Composition.ICompositionContainerShape
  ICompositionContainerShape = interface;
  PICompositionContainerShape = ^ICompositionContainerShape;

  // Windows.UI.Composition.ICompositionDrawingSurface
  ICompositionDrawingSurface = interface;
  PICompositionDrawingSurface = ^ICompositionDrawingSurface;

  // Windows.UI.Composition.ICompositionDrawingSurface2
  ICompositionDrawingSurface2 = interface;
  PICompositionDrawingSurface2 = ^ICompositionDrawingSurface2;

  // Windows.UI.Composition.ICompositionDrawingSurfaceFactory
  ICompositionDrawingSurfaceFactory = interface;
  PICompositionDrawingSurfaceFactory = ^ICompositionDrawingSurfaceFactory;

  // Windows.UI.Composition.ICompositionEasingFunctionFactory
  ICompositionEasingFunctionFactory = interface;
  PICompositionEasingFunctionFactory = ^ICompositionEasingFunctionFactory;

  // Windows.UI.Composition.ICompositionEffectSourceParameter
  ICompositionEffectSourceParameter = interface;
  PICompositionEffectSourceParameter = ^ICompositionEffectSourceParameter;

  // Windows.UI.Composition.ICompositionEffectSourceParameterFactory
  ICompositionEffectSourceParameterFactory = interface;
  PICompositionEffectSourceParameterFactory = ^ICompositionEffectSourceParameterFactory;

  // Windows.UI.Composition.ICompositionEllipseGeometry
  ICompositionEllipseGeometry = interface;
  PICompositionEllipseGeometry = ^ICompositionEllipseGeometry;

  // Windows.UI.Composition.ICompositionGeometry
  ICompositionGeometry = interface;
  PICompositionGeometry = ^ICompositionGeometry;

  // Windows.UI.Composition.ICompositionViewBox
  ICompositionViewBox = interface;
  PICompositionViewBox = ^ICompositionViewBox;

  // Windows.UI.Composition.ICompositionGeometricClip
  ICompositionGeometricClip = interface;
  PICompositionGeometricClip = ^ICompositionGeometricClip;

  // Windows.UI.Composition.ICompositionGeometryFactory
  ICompositionGeometryFactory = interface;
  PICompositionGeometryFactory = ^ICompositionGeometryFactory;

  // Windows.UI.Composition.ICompositionGradientBrushFactory
  ICompositionGradientBrushFactory = interface;
  PICompositionGradientBrushFactory = ^ICompositionGradientBrushFactory;

  // Windows.UI.Composition.IRenderingDeviceReplacedEventArgs
  IRenderingDeviceReplacedEventArgs = interface;
  PIRenderingDeviceReplacedEventArgs = ^IRenderingDeviceReplacedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.ICompositionGraphicsDevice,Windows.UI.Composition.IRenderingDeviceReplacedEventArgs>
  TypedEventHandler_2__ICompositionGraphicsDevice__IRenderingDeviceReplacedEventArgs = interface;
  PTypedEventHandler_2__ICompositionGraphicsDevice__IRenderingDeviceReplacedEventArgs = ^TypedEventHandler_2__ICompositionGraphicsDevice__IRenderingDeviceReplacedEventArgs;

  // Windows.UI.Composition.ICompositionGraphicsDevice
  ICompositionGraphicsDevice = interface;
  PICompositionGraphicsDevice = ^ICompositionGraphicsDevice;

  // Windows.UI.Composition.ICompositionVirtualDrawingSurface
  ICompositionVirtualDrawingSurface = interface;
  PICompositionVirtualDrawingSurface = ^ICompositionVirtualDrawingSurface;

  // Windows.UI.Composition.ICompositionGraphicsDevice2
  ICompositionGraphicsDevice2 = interface;
  PICompositionGraphicsDevice2 = ^ICompositionGraphicsDevice2;

  // Windows.UI.Composition.ICompositionMipmapSurface
  ICompositionMipmapSurface = interface;
  PICompositionMipmapSurface = ^ICompositionMipmapSurface;

  // Windows.UI.Composition.ICompositionGraphicsDevice3
  ICompositionGraphicsDevice3 = interface;
  PICompositionGraphicsDevice3 = ^ICompositionGraphicsDevice3;

  // Windows.UI.Composition.ICompositionLight2
  ICompositionLight2 = interface;
  PICompositionLight2 = ^ICompositionLight2;

  // Windows.UI.Composition.ICompositionLight3
  ICompositionLight3 = interface;
  PICompositionLight3 = ^ICompositionLight3;

  // Windows.UI.Composition.ICompositionLightFactory
  ICompositionLightFactory = interface;
  PICompositionLightFactory = ^ICompositionLightFactory;

  // Windows.UI.Composition.ICompositionLineGeometry
  ICompositionLineGeometry = interface;
  PICompositionLineGeometry = ^ICompositionLineGeometry;

  // Windows.UI.Composition.ICompositionLinearGradientBrush
  ICompositionLinearGradientBrush = interface;
  PICompositionLinearGradientBrush = ^ICompositionLinearGradientBrush;

  // Windows.UI.Composition.ICompositionMaskBrush
  ICompositionMaskBrush = interface;
  PICompositionMaskBrush = ^ICompositionMaskBrush;

  // Windows.UI.Composition.ICompositionNineGridBrush
  ICompositionNineGridBrush = interface;
  PICompositionNineGridBrush = ^ICompositionNineGridBrush;

  // Windows.UI.Composition.IImplicitAnimationCollection
  IImplicitAnimationCollection = interface;
  PIImplicitAnimationCollection = ^IImplicitAnimationCollection;

  // Windows.UI.Composition.ICompositionObject2
  ICompositionObject2 = interface;
  PICompositionObject2 = ^ICompositionObject2;

  // Windows.UI.Composition.ICompositionObject3
  ICompositionObject3 = interface;
  PICompositionObject3 = ^ICompositionObject3;

  // Windows.UI.Composition.ICompositionObject4
  ICompositionObject4 = interface;
  PICompositionObject4 = ^ICompositionObject4;

  // Windows.UI.Composition.ICompositionObjectFactory
  ICompositionObjectFactory = interface;
  PICompositionObjectFactory = ^ICompositionObjectFactory;

  // Windows.UI.Composition.ICompositionObjectStatics
  ICompositionObjectStatics = interface;
  PICompositionObjectStatics = ^ICompositionObjectStatics;

  // Windows.UI.Composition.ICompositionPath
  ICompositionPath = interface;
  PICompositionPath = ^ICompositionPath;

  // Windows.UI.Composition.ICompositionPathFactory
  ICompositionPathFactory = interface;
  PICompositionPathFactory = ^ICompositionPathFactory;

  // Windows.UI.Composition.ICompositionPathGeometry
  ICompositionPathGeometry = interface;
  PICompositionPathGeometry = ^ICompositionPathGeometry;

  // Windows.UI.Composition.ICompositionProjectedShadowCaster
  ICompositionProjectedShadowCaster = interface;
  PICompositionProjectedShadowCaster = ^ICompositionProjectedShadowCaster;

  // Windows.UI.Composition.ICompositionProjectedShadowCasterCollection
  ICompositionProjectedShadowCasterCollection = interface;
  PICompositionProjectedShadowCasterCollection = ^ICompositionProjectedShadowCasterCollection;

  // Windows.UI.Composition.ICompositionProjectedShadowReceiver
  ICompositionProjectedShadowReceiver = interface;
  PICompositionProjectedShadowReceiver = ^ICompositionProjectedShadowReceiver;

  // Windows.UI.Composition.ICompositionProjectedShadowReceiverUnorderedCollection
  ICompositionProjectedShadowReceiverUnorderedCollection = interface;
  PICompositionProjectedShadowReceiverUnorderedCollection = ^ICompositionProjectedShadowReceiverUnorderedCollection;

  // Windows.UI.Composition.ICompositionProjectedShadow
  ICompositionProjectedShadow = interface;
  PICompositionProjectedShadow = ^ICompositionProjectedShadow;

  // Windows.UI.Composition.ICompositionPropertySet2
  ICompositionPropertySet2 = interface;
  PICompositionPropertySet2 = ^ICompositionPropertySet2;

  // Windows.UI.Composition.ICompositionRadialGradientBrush
  ICompositionRadialGradientBrush = interface;
  PICompositionRadialGradientBrush = ^ICompositionRadialGradientBrush;

  // Windows.UI.Composition.ICompositionRectangleGeometry
  ICompositionRectangleGeometry = interface;
  PICompositionRectangleGeometry = ^ICompositionRectangleGeometry;

  // Windows.UI.Composition.ICompositionRoundedRectangleGeometry
  ICompositionRoundedRectangleGeometry = interface;
  PICompositionRoundedRectangleGeometry = ^ICompositionRoundedRectangleGeometry;

  // Windows.UI.Composition.ICompositionShadow
  ICompositionShadow = interface;
  PICompositionShadow = ^ICompositionShadow;

  // Windows.UI.Composition.ICompositionShadowFactory
  ICompositionShadowFactory = interface;
  PICompositionShadowFactory = ^ICompositionShadowFactory;

  // Windows.UI.Composition.ICompositionShapeFactory
  ICompositionShapeFactory = interface;
  PICompositionShapeFactory = ^ICompositionShapeFactory;

  // Windows.UI.Composition.ICompositionSpriteShape
  ICompositionSpriteShape = interface;
  PICompositionSpriteShape = ^ICompositionSpriteShape;

  // Windows.UI.Composition.ICompositionSurfaceBrush2
  ICompositionSurfaceBrush2 = interface;
  PICompositionSurfaceBrush2 = ^ICompositionSurfaceBrush2;

  // Windows.UI.Composition.ICompositionSurfaceBrush3
  ICompositionSurfaceBrush3 = interface;
  PICompositionSurfaceBrush3 = ^ICompositionSurfaceBrush3;

  // Windows.UI.Composition.ICompositionTargetFactory
  ICompositionTargetFactory = interface;
  PICompositionTargetFactory = ^ICompositionTargetFactory;

  // Windows.UI.Composition.ICompositionTransform
  ICompositionTransform = interface;
  PICompositionTransform = ^ICompositionTransform;

  // Windows.UI.Composition.ICompositionTransformFactory
  ICompositionTransformFactory = interface;
  PICompositionTransformFactory = ^ICompositionTransformFactory;

  // Windows.UI.Composition.ICompositionVirtualDrawingSurfaceFactory
  ICompositionVirtualDrawingSurfaceFactory = interface;
  PICompositionVirtualDrawingSurfaceFactory = ^ICompositionVirtualDrawingSurfaceFactory;

  // Windows.UI.Composition.ICompositionVisualSurface
  ICompositionVisualSurface = interface;
  PICompositionVisualSurface = ^ICompositionVisualSurface;

  // Windows.UI.Composition.IDistantLight
  IDistantLight = interface;
  PIDistantLight = ^IDistantLight;

  // Windows.UI.Composition.IDropShadow
  IDropShadow = interface;
  PIDropShadow = ^IDropShadow;

  // Windows.UI.Composition.ILayerVisual
  ILayerVisual = interface;
  PILayerVisual = ^ILayerVisual;

  // Windows.UI.Composition.IPointLight
  IPointLight = interface;
  PIPointLight = ^IPointLight;

  // Windows.UI.Composition.ISpotLight
  ISpotLight = interface;
  PISpotLight = ^ISpotLight;

  // Windows.UI.Composition.IStepEasingFunction
  IStepEasingFunction = interface;
  PIStepEasingFunction = ^IStepEasingFunction;

  // Windows.UI.Composition.ICompositor2
  ICompositor2 = interface;
  PICompositor2 = ^ICompositor2;

  // Windows.UI.Composition.ICompositor3
  ICompositor3 = interface;
  PICompositor3 = ^ICompositor3;

  // Windows.UI.Composition.ISpringScalarNaturalMotionAnimation
  ISpringScalarNaturalMotionAnimation = interface;
  PISpringScalarNaturalMotionAnimation = ^ISpringScalarNaturalMotionAnimation;

  // Windows.UI.Composition.ISpringVector2NaturalMotionAnimation
  ISpringVector2NaturalMotionAnimation = interface;
  PISpringVector2NaturalMotionAnimation = ^ISpringVector2NaturalMotionAnimation;

  // Windows.UI.Composition.ISpringVector3NaturalMotionAnimation
  ISpringVector3NaturalMotionAnimation = interface;
  PISpringVector3NaturalMotionAnimation = ^ISpringVector3NaturalMotionAnimation;

  // Windows.UI.Composition.ICompositor4
  ICompositor4 = interface;
  PICompositor4 = ^ICompositor4;

  // Windows.UI.Composition.IPathKeyFrameAnimation
  IPathKeyFrameAnimation = interface;
  PIPathKeyFrameAnimation = ^IPathKeyFrameAnimation;

  // Windows.UI.Composition.IShapeVisual
  IShapeVisual = interface;
  PIShapeVisual = ^IShapeVisual;

  // Windows.UI.Composition.ICompositor5
  ICompositor5 = interface;
  PICompositor5 = ^ICompositor5;

  // Windows.UI.Composition.IRedirectVisual
  IRedirectVisual = interface;
  PIRedirectVisual = ^IRedirectVisual;

  // Windows.UI.Composition.ICompositor6
  ICompositor6 = interface;
  PICompositor6 = ^ICompositor6;

  // Windows.UI.Composition.ICompositorStatics
  ICompositorStatics = interface;
  PICompositorStatics = ^ICompositorStatics;

  // Windows.UI.Composition.ICompositorWithProjectedShadow
  ICompositorWithProjectedShadow = interface;
  PICompositorWithProjectedShadow = ^ICompositorWithProjectedShadow;

  // Windows.UI.Composition.ICompositorWithRadialGradient
  ICompositorWithRadialGradient = interface;
  PICompositorWithRadialGradient = ^ICompositorWithRadialGradient;

  // Windows.UI.Composition.ICompositorWithVisualSurface
  ICompositorWithVisualSurface = interface;
  PICompositorWithVisualSurface = ^ICompositorWithVisualSurface;

  // Windows.UI.Composition.IContainerVisualFactory
  IContainerVisualFactory = interface;
  PIContainerVisualFactory = ^IContainerVisualFactory;

  // Windows.UI.Composition.IDistantLight2
  IDistantLight2 = interface;
  PIDistantLight2 = ^IDistantLight2;

  // Windows.UI.Composition.IDropShadow2
  IDropShadow2 = interface;
  PIDropShadow2 = ^IDropShadow2;

  // Windows.UI.Composition.IKeyFrameAnimation
  IKeyFrameAnimation = interface;
  PIKeyFrameAnimation = ^IKeyFrameAnimation;

  // Windows.UI.Composition.IKeyFrameAnimation2
  IKeyFrameAnimation2 = interface;
  PIKeyFrameAnimation2 = ^IKeyFrameAnimation2;

  // Windows.UI.Composition.IKeyFrameAnimation3
  IKeyFrameAnimation3 = interface;
  PIKeyFrameAnimation3 = ^IKeyFrameAnimation3;

  // Windows.UI.Composition.IKeyFrameAnimationFactory
  IKeyFrameAnimationFactory = interface;
  PIKeyFrameAnimationFactory = ^IKeyFrameAnimationFactory;

  // Windows.UI.Composition.ILayerVisual2
  ILayerVisual2 = interface;
  PILayerVisual2 = ^ILayerVisual2;

  // Windows.UI.Composition.INaturalMotionAnimation
  INaturalMotionAnimation = interface;
  PINaturalMotionAnimation = ^INaturalMotionAnimation;

  // Windows.UI.Composition.INaturalMotionAnimationFactory
  INaturalMotionAnimationFactory = interface;
  PINaturalMotionAnimationFactory = ^INaturalMotionAnimationFactory;

  // Windows.UI.Composition.IPointLight2
  IPointLight2 = interface;
  PIPointLight2 = ^IPointLight2;

  // Windows.UI.Composition.IPointLight3
  IPointLight3 = interface;
  PIPointLight3 = ^IPointLight3;

  // Windows.UI.Composition.IScalarNaturalMotionAnimation
  IScalarNaturalMotionAnimation = interface;
  PIScalarNaturalMotionAnimation = ^IScalarNaturalMotionAnimation;

  // Windows.UI.Composition.IScalarNaturalMotionAnimationFactory
  IScalarNaturalMotionAnimationFactory = interface;
  PIScalarNaturalMotionAnimationFactory = ^IScalarNaturalMotionAnimationFactory;

  // Windows.UI.Composition.ISpotLight2
  ISpotLight2 = interface;
  PISpotLight2 = ^ISpotLight2;

  // Windows.UI.Composition.ISpotLight3
  ISpotLight3 = interface;
  PISpotLight3 = ^ISpotLight3;

  // Windows.UI.Composition.ISpriteVisual2
  ISpriteVisual2 = interface;
  PISpriteVisual2 = ^ISpriteVisual2;

  // Windows.UI.Composition.IVector2NaturalMotionAnimation
  IVector2NaturalMotionAnimation = interface;
  PIVector2NaturalMotionAnimation = ^IVector2NaturalMotionAnimation;

  // Windows.UI.Composition.IVector2NaturalMotionAnimationFactory
  IVector2NaturalMotionAnimationFactory = interface;
  PIVector2NaturalMotionAnimationFactory = ^IVector2NaturalMotionAnimationFactory;

  // Windows.UI.Composition.IVector3NaturalMotionAnimation
  IVector3NaturalMotionAnimation = interface;
  PIVector3NaturalMotionAnimation = ^IVector3NaturalMotionAnimation;

  // Windows.UI.Composition.IVector3NaturalMotionAnimationFactory
  IVector3NaturalMotionAnimationFactory = interface;
  PIVector3NaturalMotionAnimationFactory = ^IVector3NaturalMotionAnimationFactory;

  // Windows.UI.Composition.IVisual2
  IVisual2 = interface;
  PIVisual2 = ^IVisual2;

  // Windows.UI.Composition.IVisual3
  IVisual3 = interface;
  PIVisual3 = ^IVisual3;

  // Windows.UI.Composition.IVisualElement
  IVisualElement = interface;
  PIVisualElement = ^IVisualElement;

  // Windows.UI.Composition.IVisualFactory
  IVisualFactory = interface;
  PIVisualFactory = ^IVisualFactory;

  // Windows.UI.Composition.Interactions.ICompositionConditionalValue
  Interactions_ICompositionConditionalValue = interface;
  PInteractions_ICompositionConditionalValue = ^Interactions_ICompositionConditionalValue;

  // Windows.UI.Composition.Interactions.ICompositionConditionalValueStatics
  Interactions_ICompositionConditionalValueStatics = interface;
  PInteractions_ICompositionConditionalValueStatics = ^Interactions_ICompositionConditionalValueStatics;

  // Windows.UI.Composition.Interactions.ICompositionInteractionSource
  Interactions_ICompositionInteractionSource = interface;
  PInteractions_ICompositionInteractionSource = ^Interactions_ICompositionInteractionSource;

  // Windows.UI.Composition.Interactions.ICompositionInteractionSourceCollection
  Interactions_ICompositionInteractionSourceCollection = interface;
  PInteractions_ICompositionInteractionSourceCollection = ^Interactions_ICompositionInteractionSourceCollection;

  // Windows.UI.Composition.Interactions.IInteractionSourceConfiguration
  Interactions_IInteractionSourceConfiguration = interface;
  PInteractions_IInteractionSourceConfiguration = ^Interactions_IInteractionSourceConfiguration;

  // Windows.UI.Composition.Interactions.IInteractionTrackerCustomAnimationStateEnteredArgs
  Interactions_IInteractionTrackerCustomAnimationStateEnteredArgs = interface;
  PInteractions_IInteractionTrackerCustomAnimationStateEnteredArgs = ^Interactions_IInteractionTrackerCustomAnimationStateEnteredArgs;

  // Windows.UI.Composition.Interactions.IInteractionTrackerIdleStateEnteredArgs
  Interactions_IInteractionTrackerIdleStateEnteredArgs = interface;
  PInteractions_IInteractionTrackerIdleStateEnteredArgs = ^Interactions_IInteractionTrackerIdleStateEnteredArgs;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaStateEnteredArgs
  Interactions_IInteractionTrackerInertiaStateEnteredArgs = interface;
  PInteractions_IInteractionTrackerInertiaStateEnteredArgs = ^Interactions_IInteractionTrackerInertiaStateEnteredArgs;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInteractingStateEnteredArgs
  Interactions_IInteractionTrackerInteractingStateEnteredArgs = interface;
  PInteractions_IInteractionTrackerInteractingStateEnteredArgs = ^Interactions_IInteractionTrackerInteractingStateEnteredArgs;

  // Windows.UI.Composition.Interactions.IInteractionTrackerRequestIgnoredArgs
  Interactions_IInteractionTrackerRequestIgnoredArgs = interface;
  PInteractions_IInteractionTrackerRequestIgnoredArgs = ^Interactions_IInteractionTrackerRequestIgnoredArgs;

  // Windows.UI.Composition.Interactions.IInteractionTrackerValuesChangedArgs
  Interactions_IInteractionTrackerValuesChangedArgs = interface;
  PInteractions_IInteractionTrackerValuesChangedArgs = ^Interactions_IInteractionTrackerValuesChangedArgs;

  // Windows.UI.Composition.Interactions.IInteractionTrackerOwner
  Interactions_IInteractionTrackerOwner = interface;
  PInteractions_IInteractionTrackerOwner = ^Interactions_IInteractionTrackerOwner;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier
  Interactions_IInteractionTrackerInertiaModifier = interface;
  PInteractions_IInteractionTrackerInertiaModifier = ^Interactions_IInteractionTrackerInertiaModifier;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier>
  IIterator_1__Interactions_IInteractionTrackerInertiaModifier = interface;
  PIIterator_1__Interactions_IInteractionTrackerInertiaModifier = ^IIterator_1__Interactions_IInteractionTrackerInertiaModifier;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier>
  IIterable_1__Interactions_IInteractionTrackerInertiaModifier = interface;
  PIIterable_1__Interactions_IInteractionTrackerInertiaModifier = ^IIterable_1__Interactions_IInteractionTrackerInertiaModifier;

  // Windows.UI.Composition.Interactions.IInteractionTracker
  Interactions_IInteractionTracker = interface;
  PInteractions_IInteractionTracker = ^Interactions_IInteractionTracker;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.ICompositionConditionalValue>
  IIterator_1__Interactions_ICompositionConditionalValue = interface;
  PIIterator_1__Interactions_ICompositionConditionalValue = ^IIterator_1__Interactions_ICompositionConditionalValue;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.ICompositionConditionalValue>
  IIterable_1__Interactions_ICompositionConditionalValue = interface;
  PIIterable_1__Interactions_ICompositionConditionalValue = ^IIterable_1__Interactions_ICompositionConditionalValue;

  // Windows.UI.Composition.Interactions.IInteractionTracker2
  Interactions_IInteractionTracker2 = interface;
  PInteractions_IInteractionTracker2 = ^Interactions_IInteractionTracker2;

  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier
  Interactions_IInteractionTrackerVector2InertiaModifier = interface;
  PInteractions_IInteractionTrackerVector2InertiaModifier = ^Interactions_IInteractionTrackerVector2InertiaModifier;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier>
  IIterator_1__Interactions_IInteractionTrackerVector2InertiaModifier = interface;
  PIIterator_1__Interactions_IInteractionTrackerVector2InertiaModifier = ^IIterator_1__Interactions_IInteractionTrackerVector2InertiaModifier;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier>
  IIterable_1__Interactions_IInteractionTrackerVector2InertiaModifier = interface;
  PIIterable_1__Interactions_IInteractionTrackerVector2InertiaModifier = ^IIterable_1__Interactions_IInteractionTrackerVector2InertiaModifier;

  // Windows.UI.Composition.Interactions.IInteractionTracker3
  Interactions_IInteractionTracker3 = interface;
  PInteractions_IInteractionTracker3 = ^Interactions_IInteractionTracker3;

  // Windows.UI.Composition.Interactions.IInteractionTracker4
  Interactions_IInteractionTracker4 = interface;
  PInteractions_IInteractionTracker4 = ^Interactions_IInteractionTracker4;

  // Windows.UI.Composition.Interactions.IInteractionTracker5
  Interactions_IInteractionTracker5 = interface;
  PInteractions_IInteractionTracker5 = ^Interactions_IInteractionTracker5;

  // Windows.UI.Composition.Interactions.IInteractionTrackerCustomAnimationStateEnteredArgs2
  Interactions_IInteractionTrackerCustomAnimationStateEnteredArgs2 = interface;
  PInteractions_IInteractionTrackerCustomAnimationStateEnteredArgs2 = ^Interactions_IInteractionTrackerCustomAnimationStateEnteredArgs2;

  // Windows.UI.Composition.Interactions.IInteractionTrackerIdleStateEnteredArgs2
  Interactions_IInteractionTrackerIdleStateEnteredArgs2 = interface;
  PInteractions_IInteractionTrackerIdleStateEnteredArgs2 = ^Interactions_IInteractionTrackerIdleStateEnteredArgs2;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifierFactory
  Interactions_IInteractionTrackerInertiaModifierFactory = interface;
  PInteractions_IInteractionTrackerInertiaModifierFactory = ^Interactions_IInteractionTrackerInertiaModifierFactory;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaMotion
  Interactions_IInteractionTrackerInertiaMotion = interface;
  PInteractions_IInteractionTrackerInertiaMotion = ^Interactions_IInteractionTrackerInertiaMotion;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaMotionStatics
  Interactions_IInteractionTrackerInertiaMotionStatics = interface;
  PInteractions_IInteractionTrackerInertiaMotionStatics = ^Interactions_IInteractionTrackerInertiaMotionStatics;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaNaturalMotion
  Interactions_IInteractionTrackerInertiaNaturalMotion = interface;
  PInteractions_IInteractionTrackerInertiaNaturalMotion = ^Interactions_IInteractionTrackerInertiaNaturalMotion;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaNaturalMotionStatics
  Interactions_IInteractionTrackerInertiaNaturalMotionStatics = interface;
  PInteractions_IInteractionTrackerInertiaNaturalMotionStatics = ^Interactions_IInteractionTrackerInertiaNaturalMotionStatics;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaRestingValue
  Interactions_IInteractionTrackerInertiaRestingValue = interface;
  PInteractions_IInteractionTrackerInertiaRestingValue = ^Interactions_IInteractionTrackerInertiaRestingValue;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaRestingValueStatics
  Interactions_IInteractionTrackerInertiaRestingValueStatics = interface;
  PInteractions_IInteractionTrackerInertiaRestingValueStatics = ^Interactions_IInteractionTrackerInertiaRestingValueStatics;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaStateEnteredArgs2
  Interactions_IInteractionTrackerInertiaStateEnteredArgs2 = interface;
  PInteractions_IInteractionTrackerInertiaStateEnteredArgs2 = ^Interactions_IInteractionTrackerInertiaStateEnteredArgs2;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaStateEnteredArgs3
  Interactions_IInteractionTrackerInertiaStateEnteredArgs3 = interface;
  PInteractions_IInteractionTrackerInertiaStateEnteredArgs3 = ^Interactions_IInteractionTrackerInertiaStateEnteredArgs3;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInteractingStateEnteredArgs2
  Interactions_IInteractionTrackerInteractingStateEnteredArgs2 = interface;
  PInteractions_IInteractionTrackerInteractingStateEnteredArgs2 = ^Interactions_IInteractionTrackerInteractingStateEnteredArgs2;

  // Windows.UI.Composition.Interactions.IInteractionTrackerStatics
  Interactions_IInteractionTrackerStatics = interface;
  PInteractions_IInteractionTrackerStatics = ^Interactions_IInteractionTrackerStatics;

  // Windows.UI.Composition.Interactions.IInteractionTrackerStatics2
  Interactions_IInteractionTrackerStatics2 = interface;
  PInteractions_IInteractionTrackerStatics2 = ^Interactions_IInteractionTrackerStatics2;

  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifierFactory
  Interactions_IInteractionTrackerVector2InertiaModifierFactory = interface;
  PInteractions_IInteractionTrackerVector2InertiaModifierFactory = ^Interactions_IInteractionTrackerVector2InertiaModifierFactory;

  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaNaturalMotion
  Interactions_IInteractionTrackerVector2InertiaNaturalMotion = interface;
  PInteractions_IInteractionTrackerVector2InertiaNaturalMotion = ^Interactions_IInteractionTrackerVector2InertiaNaturalMotion;

  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaNaturalMotionStatics
  Interactions_IInteractionTrackerVector2InertiaNaturalMotionStatics = interface;
  PInteractions_IInteractionTrackerVector2InertiaNaturalMotionStatics = ^Interactions_IInteractionTrackerVector2InertiaNaturalMotionStatics;

  // Windows.UI.Composition.Interactions.IVisualInteractionSource
  Interactions_IVisualInteractionSource = interface;
  PInteractions_IVisualInteractionSource = ^Interactions_IVisualInteractionSource;

  // Windows.UI.Composition.Interactions.IVisualInteractionSource2
  Interactions_IVisualInteractionSource2 = interface;
  PInteractions_IVisualInteractionSource2 = ^Interactions_IVisualInteractionSource2;

  // Windows.UI.Composition.Interactions.IVisualInteractionSource3
  Interactions_IVisualInteractionSource3 = interface;
  PInteractions_IVisualInteractionSource3 = ^Interactions_IVisualInteractionSource3;

  // Windows.UI.Composition.Interactions.IVisualInteractionSourceObjectFactory
  Interactions_IVisualInteractionSourceObjectFactory = interface;
  PInteractions_IVisualInteractionSourceObjectFactory = ^Interactions_IVisualInteractionSourceObjectFactory;

  // Windows.UI.Composition.Interactions.IVisualInteractionSourceStatics
  Interactions_IVisualInteractionSourceStatics = interface;
  PInteractions_IVisualInteractionSourceStatics = ^Interactions_IVisualInteractionSourceStatics;

  // Windows.UI.Composition.Interactions.IVisualInteractionSourceStatics2
  Interactions_IVisualInteractionSourceStatics2 = interface;
  PInteractions_IVisualInteractionSourceStatics2 = ^Interactions_IVisualInteractionSourceStatics2;

  // Windows.UI.Composition.Scenes.ISceneBoundingBox
  Scenes_ISceneBoundingBox = interface;
  PScenes_ISceneBoundingBox = ^Scenes_ISceneBoundingBox;

  // Windows.UI.Composition.Scenes.ISceneComponent
  Scenes_ISceneComponent = interface;
  PScenes_ISceneComponent = ^Scenes_ISceneComponent;

  // Windows.UI.Composition.Scenes.ISceneComponentCollection
  Scenes_ISceneComponentCollection = interface;
  PScenes_ISceneComponentCollection = ^Scenes_ISceneComponentCollection;

  // Windows.UI.Composition.Scenes.ISceneComponentFactory
  Scenes_ISceneComponentFactory = interface;
  PScenes_ISceneComponentFactory = ^Scenes_ISceneComponentFactory;

  // Windows.UI.Composition.Scenes.ISceneMaterial
  Scenes_ISceneMaterial = interface;
  PScenes_ISceneMaterial = ^Scenes_ISceneMaterial;

  // Windows.UI.Composition.Scenes.ISceneMaterialFactory
  Scenes_ISceneMaterialFactory = interface;
  PScenes_ISceneMaterialFactory = ^Scenes_ISceneMaterialFactory;

  // Windows.UI.Composition.Scenes.ISceneMaterialInput
  Scenes_ISceneMaterialInput = interface;
  PScenes_ISceneMaterialInput = ^Scenes_ISceneMaterialInput;

  // Windows.UI.Composition.Scenes.ISceneMaterialInputFactory
  Scenes_ISceneMaterialInputFactory = interface;
  PScenes_ISceneMaterialInputFactory = ^Scenes_ISceneMaterialInputFactory;

  // Windows.UI.Composition.Scenes.ISceneMesh
  Scenes_ISceneMesh = interface;
  PScenes_ISceneMesh = ^Scenes_ISceneMesh;

  // Windows.UI.Composition.Scenes.ISceneMeshMaterialAttributeMap
  Scenes_ISceneMeshMaterialAttributeMap = interface;
  PScenes_ISceneMeshMaterialAttributeMap = ^Scenes_ISceneMeshMaterialAttributeMap;

  // Windows.UI.Composition.Scenes.ISceneMeshRendererComponent
  Scenes_ISceneMeshRendererComponent = interface;
  PScenes_ISceneMeshRendererComponent = ^Scenes_ISceneMeshRendererComponent;

  // Windows.UI.Composition.Scenes.ISceneMeshRendererComponentStatics
  Scenes_ISceneMeshRendererComponentStatics = interface;
  PScenes_ISceneMeshRendererComponentStatics = ^Scenes_ISceneMeshRendererComponentStatics;

  // Windows.UI.Composition.Scenes.ISceneMeshStatics
  Scenes_ISceneMeshStatics = interface;
  PScenes_ISceneMeshStatics = ^Scenes_ISceneMeshStatics;

  // Windows.UI.Composition.Scenes.ISceneMetallicRoughnessMaterial
  Scenes_ISceneMetallicRoughnessMaterial = interface;
  PScenes_ISceneMetallicRoughnessMaterial = ^Scenes_ISceneMetallicRoughnessMaterial;

  // Windows.UI.Composition.Scenes.ISceneMetallicRoughnessMaterialStatics
  Scenes_ISceneMetallicRoughnessMaterialStatics = interface;
  PScenes_ISceneMetallicRoughnessMaterialStatics = ^Scenes_ISceneMetallicRoughnessMaterialStatics;

  // Windows.UI.Composition.Scenes.ISceneModelTransform
  Scenes_ISceneModelTransform = interface;
  PScenes_ISceneModelTransform = ^Scenes_ISceneModelTransform;

  // Windows.UI.Composition.Scenes.ISceneNodeCollection
  Scenes_ISceneNodeCollection = interface;
  PScenes_ISceneNodeCollection = ^Scenes_ISceneNodeCollection;

  // Windows.UI.Composition.Scenes.ISceneNode
  Scenes_ISceneNode = interface;
  PScenes_ISceneNode = ^Scenes_ISceneNode;

  // Windows.UI.Composition.Scenes.ISceneNodeStatics
  Scenes_ISceneNodeStatics = interface;
  PScenes_ISceneNodeStatics = ^Scenes_ISceneNodeStatics;

  // Windows.UI.Composition.Scenes.ISceneObject
  Scenes_ISceneObject = interface;
  PScenes_ISceneObject = ^Scenes_ISceneObject;

  // Windows.UI.Composition.Scenes.ISceneObjectFactory
  Scenes_ISceneObjectFactory = interface;
  PScenes_ISceneObjectFactory = ^Scenes_ISceneObjectFactory;

  // Windows.UI.Composition.Scenes.IScenePbrMaterial
  Scenes_IScenePbrMaterial = interface;
  PScenes_IScenePbrMaterial = ^Scenes_IScenePbrMaterial;

  // Windows.UI.Composition.Scenes.IScenePbrMaterialFactory
  Scenes_IScenePbrMaterialFactory = interface;
  PScenes_IScenePbrMaterialFactory = ^Scenes_IScenePbrMaterialFactory;

  // Windows.UI.Composition.Scenes.ISceneRendererComponent
  Scenes_ISceneRendererComponent = interface;
  PScenes_ISceneRendererComponent = ^Scenes_ISceneRendererComponent;

  // Windows.UI.Composition.Scenes.ISceneRendererComponentFactory
  Scenes_ISceneRendererComponentFactory = interface;
  PScenes_ISceneRendererComponentFactory = ^Scenes_ISceneRendererComponentFactory;

  // Windows.UI.Composition.Scenes.ISceneSurfaceMaterialInput
  Scenes_ISceneSurfaceMaterialInput = interface;
  PScenes_ISceneSurfaceMaterialInput = ^Scenes_ISceneSurfaceMaterialInput;

  // Windows.UI.Composition.Scenes.ISceneSurfaceMaterialInputStatics
  Scenes_ISceneSurfaceMaterialInputStatics = interface;
  PScenes_ISceneSurfaceMaterialInputStatics = ^Scenes_ISceneSurfaceMaterialInputStatics;

  // Windows.UI.Composition.Scenes.ISceneVisual
  Scenes_ISceneVisual = interface;
  PScenes_ISceneVisual = ^Scenes_ISceneVisual;

  // Windows.UI.Composition.Scenes.ISceneVisualStatics
  Scenes_ISceneVisualStatics = interface;
  PScenes_ISceneVisualStatics = ^Scenes_ISceneVisualStatics;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.IVisualElement>
  IIterator_1__IVisualElement = interface;
  PIIterator_1__IVisualElement = ^IIterator_1__IVisualElement;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.IVisualElement>
  IIterable_1__IVisualElement = interface;
  PIIterable_1__IVisualElement = ^IIterable_1__IVisualElement;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Composition.IVisualElement>
  IVectorView_1__IVisualElement = interface;
  PIVectorView_1__IVisualElement = ^IVectorView_1__IVisualElement;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Composition.IVisualElement>
  IVector_1__IVisualElement = interface;
  PIVector_1__IVisualElement = ^IVector_1__IVisualElement;

  // Windows.UI.Composition Enums

  // Windows.UI.Composition.AnimationControllerProgressBehavior
  AnimationControllerProgressBehavior = (
    Default = 0,
    IncludesDelayTime = 1
  );
  PAnimationControllerProgressBehavior = ^AnimationControllerProgressBehavior;

  // Windows.UI.Composition.AnimationDelayBehavior
  AnimationDelayBehavior = (
    SetInitialValueAfterDelay = 0,
    SetInitialValueBeforeDelay = 1
  );
  PAnimationDelayBehavior = ^AnimationDelayBehavior;

  // Windows.UI.Composition.AnimationDirection
  AnimationDirection = (
    Normal = 0,
    Reverse = 1,
    Alternate = 2,
    AlternateReverse = 3
  );
  PAnimationDirection = ^AnimationDirection;

  // Windows.UI.Composition.AnimationIterationBehavior
  AnimationIterationBehavior = (
    Count = 0,
    Forever = 1
  );
  PAnimationIterationBehavior = ^AnimationIterationBehavior;

  // Windows.UI.Composition.AnimationPropertyAccessMode
  AnimationPropertyAccessMode = (
    None = 0,
    ReadOnly = 1,
    WriteOnly = 2,
    ReadWrite = 3
  );
  PAnimationPropertyAccessMode = ^AnimationPropertyAccessMode;

  // Windows.UI.Composition.AnimationStopBehavior
  AnimationStopBehavior = (
    LeaveCurrentValue = 0,
    SetToInitialValue = 1,
    SetToFinalValue = 2
  );
  PAnimationStopBehavior = ^AnimationStopBehavior;

  // Windows.UI.Composition.CompositionDropShadowSourcePolicy
  CompositionDropShadowSourcePolicy = (
    Default = 0,
    InheritFromVisualContent = 1
  );
  PCompositionDropShadowSourcePolicy = ^CompositionDropShadowSourcePolicy;

  // Windows.UI.Composition.CompositionGradientExtendMode
  CompositionGradientExtendMode = (
    Clamp = 0,
    Wrap = 1,
    Mirror = 2
  );
  PCompositionGradientExtendMode = ^CompositionGradientExtendMode;

  // Windows.UI.Composition.CompositionMappingMode
  CompositionMappingMode = (
    Absolute = 0,
    Relative = 1
  );
  PCompositionMappingMode = ^CompositionMappingMode;

  // Windows.UI.Composition.CompositionStrokeCap
  CompositionStrokeCap = (
    Flat = 0,
    Square = 1,
    Round = 2,
    Triangle = 3
  );
  PCompositionStrokeCap = ^CompositionStrokeCap;

  // Windows.UI.Composition.CompositionStrokeLineJoin
  CompositionStrokeLineJoin = (
    Miter = 0,
    Bevel = 1,
    Round = 2,
    MiterOrBevel = 3
  );
  PCompositionStrokeLineJoin = ^CompositionStrokeLineJoin;

  // Windows.UI.Composition.Diagnostics.CompositionDebugOverdrawContentKinds
  Diagnostics_CompositionDebugOverdrawContentKinds = (
    None = 0,
    OffscreenRendered = 1,
    Colors = 2,
    Effects = 4,
    Shadows = 8,
    Lights = 16,
    Surfaces = 32,
    SwapChains = 64,
    All = -1
  );
  PDiagnostics_CompositionDebugOverdrawContentKinds = ^Diagnostics_CompositionDebugOverdrawContentKinds;

  // Windows.UI.Composition.Effects.SceneLightingEffectReflectanceModel
  Effects_SceneLightingEffectReflectanceModel = (
    BlinnPhong = 0,
    PhysicallyBasedBlinnPhong = 1
  );
  PEffects_SceneLightingEffectReflectanceModel = ^Effects_SceneLightingEffectReflectanceModel;

  // Windows.UI.Composition.Interactions.InteractionBindingAxisModes
  Interactions_InteractionBindingAxisModes = (
    None = 0,
    PositionX = 1,
    PositionY = 2,
    Scale = 4
  );
  PInteractions_InteractionBindingAxisModes = ^Interactions_InteractionBindingAxisModes;

  // Windows.UI.Composition.Interactions.InteractionChainingMode
  Interactions_InteractionChainingMode = (
    Auto = 0,
    Always = 1,
    Never = 2
  );
  PInteractions_InteractionChainingMode = ^Interactions_InteractionChainingMode;

  // Windows.UI.Composition.Interactions.InteractionSourceMode
  Interactions_InteractionSourceMode = (
    Disabled = 0,
    EnabledWithInertia = 1,
    EnabledWithoutInertia = 2
  );
  PInteractions_InteractionSourceMode = ^Interactions_InteractionSourceMode;

  // Windows.UI.Composition.Interactions.InteractionSourceRedirectionMode
  Interactions_InteractionSourceRedirectionMode = (
    Disabled = 0,
    Enabled = 1
  );
  PInteractions_InteractionSourceRedirectionMode = ^Interactions_InteractionSourceRedirectionMode;

  // Windows.UI.Composition.Interactions.InteractionTrackerClampingOption
  Interactions_InteractionTrackerClampingOption = (
    Auto = 0,
    Disabled = 1
  );
  PInteractions_InteractionTrackerClampingOption = ^Interactions_InteractionTrackerClampingOption;

  // Windows.UI.Composition.Interactions.InteractionTrackerPositionUpdateOption
  Interactions_InteractionTrackerPositionUpdateOption = (
    Default = 0,
    AllowActiveCustomScaleAnimation = 1
  );
  PInteractions_InteractionTrackerPositionUpdateOption = ^Interactions_InteractionTrackerPositionUpdateOption;

  // Windows.UI.Composition.Interactions.VisualInteractionSourceRedirectionMode
  Interactions_VisualInteractionSourceRedirectionMode = (
    Off = 0,
    CapableTouchpadOnly = 1,
    PointerWheelOnly = 2,
    CapableTouchpadAndPointerWheel = 3
  );
  PInteractions_VisualInteractionSourceRedirectionMode = ^Interactions_VisualInteractionSourceRedirectionMode;

  // Windows.UI.Composition.Scenes.SceneAlphaMode
  Scenes_SceneAlphaMode = (
    Opaque = 0,
    AlphaTest = 1,
    Blend = 2
  );
  PScenes_SceneAlphaMode = ^Scenes_SceneAlphaMode;

  // Windows.UI.Composition.Scenes.SceneAttributeSemantic
  Scenes_SceneAttributeSemantic = (
    Index = 0,
    Vertex = 1,
    Normal = 2,
    TexCoord0 = 3,
    TexCoord1 = 4,
    Color = 5,
    Tangent = 6
  );
  PScenes_SceneAttributeSemantic = ^Scenes_SceneAttributeSemantic;

  // Windows.UI.Composition.Scenes.SceneComponentType
  Scenes_SceneComponentType = (
    MeshRendererComponent = 0
  );
  PScenes_SceneComponentType = ^Scenes_SceneComponentType;

  // Windows.UI.Composition.Scenes.SceneWrappingMode
  Scenes_SceneWrappingMode = (
    ClampToEdge = 0,
    MirroredRepeat = 1,
    &Repeat = 2
  );
  PScenes_SceneWrappingMode = ^Scenes_SceneWrappingMode;

  // Windows.UI.Composition Interfaces

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionAnimationBase
  ICompositionAnimationBase = interface(IInspectable)
  ['{1C2C2999-E818-48D3-A6DD-D78C82F8ACE9}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IAnimationPropertyInfo
  IAnimationPropertyInfo = interface(IInspectable)
  ['{F4716F05-ED77-4E3C-B328-5C3985B3738F}']
    function get_AccessMode: AnimationPropertyAccessMode; safecall;
    procedure put_AccessMode(value: AnimationPropertyAccessMode); safecall;
    property AccessMode: AnimationPropertyAccessMode read get_AccessMode write put_AccessMode;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IVisualUnorderedCollection
  IVisualUnorderedCollection = interface(IInspectable)
  ['{338FAA70-54C8-40A7-8029-C9CEEB0AA250}']
    function get_Count: Integer; safecall;
    procedure Add(newVisual: IVisual); safecall;
    procedure Remove(visual: IVisual); safecall;
    procedure RemoveAll; safecall;
    property Count: Integer read get_Count;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionLight
  ICompositionLight = interface(IInspectable)
  ['{41A6D7C2-2E5D-4BC1-B09E-8F0A03E3D8D3}']
    function get_Targets: IVisualUnorderedCollection; safecall;
    property Targets: IVisualUnorderedCollection read get_Targets;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Diagnostics.ICompositionDebugHeatMaps
  Diagnostics_ICompositionDebugHeatMaps = interface(IInspectable)
  ['{E49C90AC-2FF3-5805-718C-B725EE07650F}']
    procedure Hide(subtree: IVisual); safecall;
    procedure ShowMemoryUsage(subtree: IVisual); safecall;
    procedure ShowOverdraw(subtree: IVisual; contentKinds: Diagnostics_CompositionDebugOverdrawContentKinds); safecall;
    procedure ShowRedraw(subtree: IVisual); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Diagnostics.ICompositionDebugSettings
  [WinRTClassNameAttribute(SWindows_UI_Composition_Diagnostics_CompositionDebugSettings)]
  Diagnostics_ICompositionDebugSettings = interface(IInspectable)
  ['{2831987E-1D82-4D38-B7B7-EFD11C7BC3D1}']
    function get_HeatMaps: Diagnostics_ICompositionDebugHeatMaps; safecall;
    property HeatMaps: Diagnostics_ICompositionDebugHeatMaps read get_HeatMaps;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Diagnostics.ICompositionDebugSettingsStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Diagnostics_CompositionDebugSettings)]
  Diagnostics_ICompositionDebugSettingsStatics = interface(IInspectable)
  ['{64EC1F1E-6AF8-4AF8-B814-C870FD5A9505}']
    function TryGetSettings(compositor: ICompositor): Diagnostics_ICompositionDebugSettings; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Effects.ISceneLightingEffect
  [WinRTClassNameAttribute(SWindows_UI_Composition_Effects_SceneLightingEffect)]
  Effects_ISceneLightingEffect = interface(IInspectable)
  ['{91BB5E52-95D1-4F8B-9A5A-6408B24B8C6A}']
    function get_AmbientAmount: Single; safecall;
    procedure put_AmbientAmount(value: Single); safecall;
    function get_DiffuseAmount: Single; safecall;
    procedure put_DiffuseAmount(value: Single); safecall;
    function get_NormalMapSource: Effects_IGraphicsEffectSource; safecall;
    procedure put_NormalMapSource(value: Effects_IGraphicsEffectSource); safecall;
    function get_SpecularAmount: Single; safecall;
    procedure put_SpecularAmount(value: Single); safecall;
    function get_SpecularShine: Single; safecall;
    procedure put_SpecularShine(value: Single); safecall;
    property AmbientAmount: Single read get_AmbientAmount write put_AmbientAmount;
    property DiffuseAmount: Single read get_DiffuseAmount write put_DiffuseAmount;
    property NormalMapSource: Effects_IGraphicsEffectSource read get_NormalMapSource write put_NormalMapSource;
    property SpecularAmount: Single read get_SpecularAmount write put_SpecularAmount;
    property SpecularShine: Single read get_SpecularShine write put_SpecularShine;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Effects.ISceneLightingEffect2
  Effects_ISceneLightingEffect2 = interface(IInspectable)
  ['{9E270E81-72F0-4C5C-95F8-8A6E0024F409}']
    function get_ReflectanceModel: Effects_SceneLightingEffectReflectanceModel; safecall;
    procedure put_ReflectanceModel(value: Effects_SceneLightingEffectReflectanceModel); safecall;
    property ReflectanceModel: Effects_SceneLightingEffectReflectanceModel read get_ReflectanceModel write put_ReflectanceModel;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IAmbientLight
  IAmbientLight = interface(IInspectable)
  ['{A48130A1-B7C4-46F7-B9BF-DAF43A44E6EE}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    property Color_: Color read get_Color write put_Color;
  end;

  // Windows.UI.Composition.IAmbientLight2
  IAmbientLight2 = interface(IInspectable)
  ['{3B64A6BF-5F97-4C94-86E5-042DD386B27D}']
    function get_Intensity: Single; safecall;
    procedure put_Intensity(value: Single); safecall;
    property Intensity: Single read get_Intensity write put_Intensity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.IAnimationController
  [WinRTClassNameAttribute(SWindows_UI_Composition_AnimationController)]
  IAnimationController = interface(IInspectable)
  ['{C934EFD2-0722-4F5F-A4E2-9510F3D43BF7}']
    function get_PlaybackRate: Single; safecall;
    procedure put_PlaybackRate(value: Single); safecall;
    function get_Progress: Single; safecall;
    procedure put_Progress(value: Single); safecall;
    function get_ProgressBehavior: AnimationControllerProgressBehavior; safecall;
    procedure put_ProgressBehavior(value: AnimationControllerProgressBehavior); safecall;
    procedure Pause; safecall;
    procedure Resume; safecall;
    property PlaybackRate: Single read get_PlaybackRate write put_PlaybackRate;
    property Progress: Single read get_Progress write put_Progress;
    property ProgressBehavior: AnimationControllerProgressBehavior read get_ProgressBehavior write put_ProgressBehavior;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.IAnimationControllerStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_AnimationController)]
  IAnimationControllerStatics = interface(IInspectable)
  ['{E71164DF-651B-4800-B9E5-6A3BCFED3365}']
    function get_MaxPlaybackRate: Single; safecall;
    function get_MinPlaybackRate: Single; safecall;
    property MaxPlaybackRate: Single read get_MaxPlaybackRate;
    property MinPlaybackRate: Single read get_MinPlaybackRate;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.IAnimationObject
  IAnimationObject = interface(IInspectable)
  ['{E7141E0A-04B8-4FC5-A4DC-195392E57807}']
    procedure PopulatePropertyInfo(propertyName: HSTRING; propertyInfo: IAnimationPropertyInfo); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IBooleanKeyFrameAnimation
  IBooleanKeyFrameAnimation = interface(IInspectable)
  ['{95E23A08-D1F4-4972-9770-3EFE68D82E14}']
    procedure InsertKeyFrame(normalizedProgressKey: Single; value: Boolean); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IBounceScalarNaturalMotionAnimation
  IBounceScalarNaturalMotionAnimation = interface(IInspectable)
  ['{BAA30DCC-A633-4618-9B06-7F7C72C87CFF}']
    function get_Acceleration: Single; safecall;
    procedure put_Acceleration(value: Single); safecall;
    function get_Restitution: Single; safecall;
    procedure put_Restitution(value: Single); safecall;
    property Acceleration: Single read get_Acceleration write put_Acceleration;
    property Restitution: Single read get_Restitution write put_Restitution;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IBounceVector2NaturalMotionAnimation
  IBounceVector2NaturalMotionAnimation = interface(IInspectable)
  ['{DA344196-2154-4B3C-88AA-47361204ECCD}']
    function get_Acceleration: Single; safecall;
    procedure put_Acceleration(value: Single); safecall;
    function get_Restitution: Single; safecall;
    procedure put_Restitution(value: Single); safecall;
    property Acceleration: Single read get_Acceleration write put_Acceleration;
    property Restitution: Single read get_Restitution write put_Restitution;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IBounceVector3NaturalMotionAnimation
  IBounceVector3NaturalMotionAnimation = interface(IInspectable)
  ['{47DABC31-10D3-4518-86F1-09CAF742D113}']
    function get_Acceleration: Single; safecall;
    procedure put_Acceleration(value: Single); safecall;
    function get_Restitution: Single; safecall;
    procedure put_Restitution(value: Single); safecall;
    property Acceleration: Single read get_Acceleration write put_Acceleration;
    property Restitution: Single read get_Restitution write put_Restitution;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionObject
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionObject)]
  ICompositionObject = interface(IInspectable)
  ['{BCB4AD45-7609-4550-934F-16002A68FDED}']
    function get_Compositor: ICompositor; safecall;
    function get_Dispatcher: ICoreDispatcher; safecall;
    function get_Properties: ICompositionPropertySet; safecall;
    procedure StartAnimation(propertyName: HSTRING; animation: ICompositionAnimation); safecall;
    procedure StopAnimation(propertyName: HSTRING); safecall;
    property Compositor: ICompositor read get_Compositor;
    property Dispatcher: ICoreDispatcher read get_Dispatcher;
    property Properties: ICompositionPropertySet read get_Properties;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionAnimation
  ICompositionAnimation = interface(IInspectable)
  ['{464C4C2C-1CAA-4061-9B40-E13FDE1503CA}']
    procedure ClearAllParameters; safecall;
    procedure ClearParameter(key: HSTRING); safecall;
    procedure SetColorParameter(key: HSTRING; value: Color); safecall;
    procedure SetMatrix3x2Parameter(key: HSTRING; value: Numerics_Matrix3x2); safecall;
    procedure SetMatrix4x4Parameter(key: HSTRING; value: Numerics_Matrix4x4); safecall;
    procedure SetQuaternionParameter(key: HSTRING; value: Numerics_Quaternion); safecall;
    procedure SetReferenceParameter(key: HSTRING; compositionObject: ICompositionObject); safecall;
    procedure SetScalarParameter(key: HSTRING; value: Single); safecall;
    procedure SetVector2Parameter(key: HSTRING; value: Numerics_Vector2); safecall;
    procedure SetVector3Parameter(key: HSTRING; value: Numerics_Vector3); safecall;
    procedure SetVector4Parameter(key: HSTRING; value: Numerics_Vector4); safecall;
  end;

  // Windows.UI.Composition.ICompositionAnimation2
  ICompositionAnimation2 = interface(IInspectable)
  ['{369B603E-A80F-4948-93E3-ED23FB38C6CB}']
    procedure SetBooleanParameter(key: HSTRING; value: Boolean); safecall;
    function get_Target: HSTRING; safecall;
    procedure put_Target(value: HSTRING); safecall;
    property Target: HSTRING read get_Target write put_Target;
  end;

  // Windows.UI.Composition.ICompositionAnimation3
  ICompositionAnimation3 = interface(IInspectable)
  ['{D51E030D-7DA4-4BD7-BC2D-F4517529F43A}']
    function get_InitialValueExpressions: IMap_2__HSTRING__HSTRING; safecall;
    property InitialValueExpressions: IMap_2__HSTRING__HSTRING read get_InitialValueExpressions;
  end;

  // Windows.UI.Composition.ICompositionAnimation4
  ICompositionAnimation4 = interface(IInspectable)
  ['{770137BE-76BC-4E23-BFED-FE9CC20F6EC9}']
    procedure SetExpressionReferenceParameter(parameterName: HSTRING; source: IAnimationObject); safecall;
  end;

  // Windows.UI.Composition.ICompositionAnimationFactory
  ICompositionAnimationFactory = interface(IInspectable)
  ['{10F6C4FB-6E51-4C25-BBD3-586A9BEC3EF4}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionAnimationGroup
  ICompositionAnimationGroup = interface(IInspectable)
  ['{5E7CC90C-CD14-4E07-8A55-C72527AABDAC}']
    function get_Count: Integer; safecall;
    procedure Add(value: ICompositionAnimation); safecall;
    procedure Remove(value: ICompositionAnimation); safecall;
    procedure RemoveAll; safecall;
    property Count: Integer read get_Count;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionBackdropBrush
  ICompositionBackdropBrush = interface(IInspectable)
  ['{C5ACAE58-3898-499E-8D7F-224E91286A5D}']
  end;

  // Windows.UI.Composition.ICompositionBrushFactory
  ICompositionBrushFactory = interface(IInspectable)
  ['{DA53FB4C-4650-47C4-AD76-765379607ED6}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionCapabilitiesStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionCapabilities)]
  ICompositionCapabilitiesStatics = interface(IInspectable)
  ['{F7B7A86E-6416-49E5-8DDF-AFE949E20562}']
    function GetForCurrentView: ICompositionCapabilities; safecall;
  end;

  // Windows.UI.Composition.ICompositionClip2
  ICompositionClip2 = interface(IInspectable)
  ['{5893E069-3516-40E1-89E0-5BA924927235}']
    function get_AnchorPoint: Numerics_Vector2; safecall;
    procedure put_AnchorPoint(value: Numerics_Vector2); safecall;
    function get_CenterPoint: Numerics_Vector2; safecall;
    procedure put_CenterPoint(value: Numerics_Vector2); safecall;
    function get_Offset: Numerics_Vector2; safecall;
    procedure put_Offset(value: Numerics_Vector2); safecall;
    function get_RotationAngle: Single; safecall;
    procedure put_RotationAngle(value: Single); safecall;
    function get_RotationAngleInDegrees: Single; safecall;
    procedure put_RotationAngleInDegrees(value: Single); safecall;
    function get_Scale: Numerics_Vector2; safecall;
    procedure put_Scale(value: Numerics_Vector2); safecall;
    function get_TransformMatrix: Numerics_Matrix3x2; safecall;
    procedure put_TransformMatrix(value: Numerics_Matrix3x2); safecall;
    property AnchorPoint: Numerics_Vector2 read get_AnchorPoint write put_AnchorPoint;
    property CenterPoint: Numerics_Vector2 read get_CenterPoint write put_CenterPoint;
    property Offset: Numerics_Vector2 read get_Offset write put_Offset;
    property RotationAngle: Single read get_RotationAngle write put_RotationAngle;
    property RotationAngleInDegrees: Single read get_RotationAngleInDegrees write put_RotationAngleInDegrees;
    property Scale: Numerics_Vector2 read get_Scale write put_Scale;
    property TransformMatrix: Numerics_Matrix3x2 read get_TransformMatrix write put_TransformMatrix;
  end;

  // Windows.UI.Composition.ICompositionClipFactory
  ICompositionClipFactory = interface(IInspectable)
  ['{B9484CAF-20C7-4AED-AC4A-9C78BA1302CF}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionColorGradientStop
  ICompositionColorGradientStop = interface(IInspectable)
  ['{6F00CA92-C801-4E41-9A8F-A53E20F57778}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    function get_Offset: Single; safecall;
    procedure put_Offset(value: Single); safecall;
    property Color_: Color read get_Color write put_Color;
    property Offset: Single read get_Offset write put_Offset;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionShape
  ICompositionShape = interface(IInspectable)
  ['{B47CE2F7-9A88-42C4-9E87-2E500CA8688C}']
    function get_CenterPoint: Numerics_Vector2; safecall;
    procedure put_CenterPoint(value: Numerics_Vector2); safecall;
    function get_Offset: Numerics_Vector2; safecall;
    procedure put_Offset(value: Numerics_Vector2); safecall;
    function get_RotationAngle: Single; safecall;
    procedure put_RotationAngle(value: Single); safecall;
    function get_RotationAngleInDegrees: Single; safecall;
    procedure put_RotationAngleInDegrees(value: Single); safecall;
    function get_Scale: Numerics_Vector2; safecall;
    procedure put_Scale(value: Numerics_Vector2); safecall;
    function get_TransformMatrix: Numerics_Matrix3x2; safecall;
    procedure put_TransformMatrix(value: Numerics_Matrix3x2); safecall;
    property CenterPoint: Numerics_Vector2 read get_CenterPoint write put_CenterPoint;
    property Offset: Numerics_Vector2 read get_Offset write put_Offset;
    property RotationAngle: Single read get_RotationAngle write put_RotationAngle;
    property RotationAngleInDegrees: Single read get_RotationAngleInDegrees write put_RotationAngleInDegrees;
    property Scale: Numerics_Vector2 read get_Scale write put_Scale;
    property TransformMatrix: Numerics_Matrix3x2 read get_TransformMatrix write put_TransformMatrix;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.ICompositionShape>
  IIterator_1__ICompositionShape = interface(IInspectable)
  ['{2BAC9158-8A06-5720-B631-4E249A387B79}']
    function get_Current: ICompositionShape; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PICompositionShape): Cardinal; safecall;
    property Current: ICompositionShape read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.ICompositionShape>
  IIterable_1__ICompositionShape = interface(IInspectable)
  ['{BA63621A-77DD-5EDE-A226-4C1CE58B6A2C}']
    function First: IIterator_1__ICompositionShape; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Composition.ICompositionShape>
  IVectorView_1__ICompositionShape = interface(IInspectable)
  ['{23554431-EC22-5AD4-B4F9-6AD7B96FD19D}']
    function GetAt(index: Cardinal): ICompositionShape; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ICompositionShape; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PICompositionShape): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Composition.ICompositionShape>
  IVector_1__ICompositionShape = interface(IInspectable)
  ['{D7C4F337-A66B-5FFB-B38A-01DC6AF1CA06}']
    function GetAt(index: Cardinal): ICompositionShape; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ICompositionShape; safecall;
    function IndexOf(value: ICompositionShape; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ICompositionShape); safecall;
    procedure InsertAt(index: Cardinal; value: ICompositionShape); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ICompositionShape); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PICompositionShape): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PICompositionShape); safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionContainerShape
  ICompositionContainerShape = interface(IInspectable)
  ['{4F5E859B-2E5B-44A8-982C-AA0F69C16059}']
    function get_Shapes: IVector_1__ICompositionShape; safecall;
    property Shapes: IVector_1__ICompositionShape read get_Shapes;
  end;

  // Windows.UI.Composition.ICompositionDrawingSurface
  ICompositionDrawingSurface = interface(IInspectable)
  ['{A166C300-FAD0-4D11-9E67-E433162FF49E}']
    function get_AlphaMode: DirectX_DirectXAlphaMode; safecall;
    function get_PixelFormat: DirectX_DirectXPixelFormat; safecall;
    function get_Size: TSizeF; safecall;
    property AlphaMode: DirectX_DirectXAlphaMode read get_AlphaMode;
    property PixelFormat: DirectX_DirectXPixelFormat read get_PixelFormat;
    property Size: TSizeF read get_Size;
  end;

  // Windows.UI.Composition.ICompositionDrawingSurface2
  ICompositionDrawingSurface2 = interface(IInspectable)
  ['{FAD0E88B-E354-44E8-8E3D-C4880D5A213F}']
    function get_SizeInt32: SizeInt32; safecall;
    procedure Resize(sizePixels: SizeInt32); safecall;
    procedure Scroll(offset: PointInt32); overload; safecall;
    procedure Scroll(offset: PointInt32; scrollRect: RectInt32); overload; safecall;
    procedure ScrollWithClip(offset: PointInt32; clipRect: RectInt32); overload; safecall;
    procedure ScrollWithClip(offset: PointInt32; clipRect: RectInt32; scrollRect: RectInt32); overload; safecall;
    property SizeInt32_: SizeInt32 read get_SizeInt32;
  end;

  // Windows.UI.Composition.ICompositionDrawingSurfaceFactory
  ICompositionDrawingSurfaceFactory = interface(IInspectable)
  ['{9497B00A-312D-46B9-9DB3-412FD79464C8}']
  end;

  // Windows.UI.Composition.ICompositionEasingFunctionFactory
  ICompositionEasingFunctionFactory = interface(IInspectable)
  ['{60840774-3DA0-4949-8200-7206C00190A0}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionEffectSourceParameter
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionEffectSourceParameter)]
  ICompositionEffectSourceParameter = interface(IInspectable)
  ['{858AB13A-3292-4E4E-B3BB-2B6C6544A6EE}']
    function get_Name: HSTRING; safecall;
    property Name: HSTRING read get_Name;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionEffectSourceParameterFactory
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionEffectSourceParameter)]
  ICompositionEffectSourceParameterFactory = interface(IInspectable)
  ['{B3D9F276-ABA3-4724-ACF3-D0397464DB1C}']
    function Create(name: HSTRING): ICompositionEffectSourceParameter; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionEllipseGeometry
  ICompositionEllipseGeometry = interface(IInspectable)
  ['{4801F884-F6AD-4B93-AFA9-897B64E57B1F}']
    function get_Center: Numerics_Vector2; safecall;
    procedure put_Center(value: Numerics_Vector2); safecall;
    function get_Radius: Numerics_Vector2; safecall;
    procedure put_Radius(value: Numerics_Vector2); safecall;
    property Center: Numerics_Vector2 read get_Center write put_Center;
    property Radius: Numerics_Vector2 read get_Radius write put_Radius;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionGeometry
  ICompositionGeometry = interface(IInspectable)
  ['{E985217C-6A17-4207-ABD8-5FD3DD612A9D}']
    function get_TrimEnd: Single; safecall;
    procedure put_TrimEnd(value: Single); safecall;
    function get_TrimOffset: Single; safecall;
    procedure put_TrimOffset(value: Single); safecall;
    function get_TrimStart: Single; safecall;
    procedure put_TrimStart(value: Single); safecall;
    property TrimEnd: Single read get_TrimEnd write put_TrimEnd;
    property TrimOffset: Single read get_TrimOffset write put_TrimOffset;
    property TrimStart: Single read get_TrimStart write put_TrimStart;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionViewBox
  ICompositionViewBox = interface(IInspectable)
  ['{B440BF07-068F-4537-84C6-4ECBE019E1F4}']
    function get_HorizontalAlignmentRatio: Single; safecall;
    procedure put_HorizontalAlignmentRatio(value: Single); safecall;
    function get_Offset: Numerics_Vector2; safecall;
    procedure put_Offset(value: Numerics_Vector2); safecall;
    function get_Size: Numerics_Vector2; safecall;
    procedure put_Size(value: Numerics_Vector2); safecall;
    function get_Stretch: CompositionStretch; safecall;
    procedure put_Stretch(value: CompositionStretch); safecall;
    function get_VerticalAlignmentRatio: Single; safecall;
    procedure put_VerticalAlignmentRatio(value: Single); safecall;
    property HorizontalAlignmentRatio: Single read get_HorizontalAlignmentRatio write put_HorizontalAlignmentRatio;
    property Offset: Numerics_Vector2 read get_Offset write put_Offset;
    property Size: Numerics_Vector2 read get_Size write put_Size;
    property Stretch: CompositionStretch read get_Stretch write put_Stretch;
    property VerticalAlignmentRatio: Single read get_VerticalAlignmentRatio write put_VerticalAlignmentRatio;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionGeometricClip
  ICompositionGeometricClip = interface(IInspectable)
  ['{C840B581-81C9-4444-A2C1-CCAECE3A50E5}']
    function get_Geometry: ICompositionGeometry; safecall;
    procedure put_Geometry(value: ICompositionGeometry); safecall;
    function get_ViewBox: ICompositionViewBox; safecall;
    procedure put_ViewBox(value: ICompositionViewBox); safecall;
    property Geometry: ICompositionGeometry read get_Geometry write put_Geometry;
    property ViewBox: ICompositionViewBox read get_ViewBox write put_ViewBox;
  end;

  // Windows.UI.Composition.ICompositionGeometryFactory
  ICompositionGeometryFactory = interface(IInspectable)
  ['{BFFEBFE1-8C25-480B-9F56-FED6B288055D}']
  end;

  // Windows.UI.Composition.ICompositionGradientBrushFactory
  ICompositionGradientBrushFactory = interface(IInspectable)
  ['{56D765D7-F189-48C9-9C8D-94DAF1BEC010}']
  end;

  // Windows.UI.Composition.IRenderingDeviceReplacedEventArgs
  IRenderingDeviceReplacedEventArgs = interface(IInspectable)
  ['{3A31AC7D-28BF-4E7A-8524-71679D480F38}']
    function get_GraphicsDevice: ICompositionGraphicsDevice; safecall;
    property GraphicsDevice: ICompositionGraphicsDevice read get_GraphicsDevice;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.ICompositionGraphicsDevice,Windows.UI.Composition.IRenderingDeviceReplacedEventArgs>
  TypedEventHandler_2__ICompositionGraphicsDevice__IRenderingDeviceReplacedEventArgs_Delegate_Base = interface(IUnknown)
  ['{259B32BE-BD06-53C2-BD51-89E8E0F9E239}']
    procedure Invoke(sender: ICompositionGraphicsDevice; args: IRenderingDeviceReplacedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Composition.ICompositionGraphicsDevice,Windows.UI.Composition.IRenderingDeviceReplacedEventArgs>
  TypedEventHandler_2__ICompositionGraphicsDevice__IRenderingDeviceReplacedEventArgs = interface(TypedEventHandler_2__ICompositionGraphicsDevice__IRenderingDeviceReplacedEventArgs_Delegate_Base)
  ['{876392D2-70D0-550F-AFD3-6E2824D1073B}']
  end;

  // Windows.UI.Composition.ICompositionGraphicsDevice
  ICompositionGraphicsDevice = interface(IInspectable)
  ['{FB22C6E1-80A2-4667-9936-DBEAF6EEFE95}']
    function CreateDrawingSurface(sizePixels: TSizeF; pixelFormat: DirectX_DirectXPixelFormat; alphaMode: DirectX_DirectXAlphaMode): ICompositionDrawingSurface; safecall;
    function add_RenderingDeviceReplaced(handler: TypedEventHandler_2__ICompositionGraphicsDevice__IRenderingDeviceReplacedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RenderingDeviceReplaced(token: EventRegistrationToken); safecall;
  end;

  // Windows.UI.Composition.ICompositionVirtualDrawingSurface
  ICompositionVirtualDrawingSurface = interface(IInspectable)
  ['{A9C384DB-8740-4F94-8B9D-B68521E7863D}']
    procedure Trim(rectsSize: Cardinal; rects: PRectInt32); safecall;
  end;

  // Windows.UI.Composition.ICompositionGraphicsDevice2
  ICompositionGraphicsDevice2 = interface(IInspectable)
  ['{0FB8BDF6-C0F0-4BCC-9FB8-084982490D7D}']
    function CreateDrawingSurface2(sizePixels: SizeInt32; pixelFormat: DirectX_DirectXPixelFormat; alphaMode: DirectX_DirectXAlphaMode): ICompositionDrawingSurface; safecall;
    function CreateVirtualDrawingSurface(sizePixels: SizeInt32; pixelFormat: DirectX_DirectXPixelFormat; alphaMode: DirectX_DirectXAlphaMode): ICompositionVirtualDrawingSurface; safecall;
  end;

  // Windows.UI.Composition.ICompositionMipmapSurface
  ICompositionMipmapSurface = interface(IInspectable)
  ['{4863675C-CF4A-4B1C-9ECE-C5EC0C2B2FE6}']
    function get_LevelCount: Cardinal; safecall;
    function get_AlphaMode: DirectX_DirectXAlphaMode; safecall;
    function get_PixelFormat: DirectX_DirectXPixelFormat; safecall;
    function get_SizeInt32: SizeInt32; safecall;
    function GetDrawingSurfaceForLevel(level: Cardinal): ICompositionDrawingSurface; safecall;
    property AlphaMode: DirectX_DirectXAlphaMode read get_AlphaMode;
    property LevelCount: Cardinal read get_LevelCount;
    property PixelFormat: DirectX_DirectXPixelFormat read get_PixelFormat;
    property SizeInt32_: SizeInt32 read get_SizeInt32;
  end;

  // Windows.UI.Composition.ICompositionGraphicsDevice3
  ICompositionGraphicsDevice3 = interface(IInspectable)
  ['{37F67514-D3EF-49D1-B69D-0D8EABEB3626}']
    function CreateMipmapSurface(sizePixels: SizeInt32; pixelFormat: DirectX_DirectXPixelFormat; alphaMode: DirectX_DirectXAlphaMode): ICompositionMipmapSurface; safecall;
    procedure Trim; safecall;
  end;

  // Windows.UI.Composition.ICompositionLight2
  ICompositionLight2 = interface(IInspectable)
  ['{A7BCDA72-F35D-425D-9B98-23F4205F6669}']
    function get_ExclusionsFromTargets: IVisualUnorderedCollection; safecall;
    property ExclusionsFromTargets: IVisualUnorderedCollection read get_ExclusionsFromTargets;
  end;

  // Windows.UI.Composition.ICompositionLight3
  ICompositionLight3 = interface(IInspectable)
  ['{4B0B00E4-DF07-4959-B7A4-4F7E4233F838}']
    function get_IsEnabled: Boolean; safecall;
    procedure put_IsEnabled(value: Boolean); safecall;
    property IsEnabled: Boolean read get_IsEnabled write put_IsEnabled;
  end;

  // Windows.UI.Composition.ICompositionLightFactory
  ICompositionLightFactory = interface(IInspectable)
  ['{069CF306-DA3C-4B44-838A-5E03D51ACE55}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionLineGeometry
  ICompositionLineGeometry = interface(IInspectable)
  ['{DD7615A4-0C9A-4B67-8DCE-440A5BF9CDEC}']
    function get_Start: Numerics_Vector2; safecall;
    procedure put_Start(value: Numerics_Vector2); safecall;
    function get_End: Numerics_Vector2; safecall;
    procedure put_End(value: Numerics_Vector2); safecall;
    property &End: Numerics_Vector2 read get_End write put_End;
    property Start: Numerics_Vector2 read get_Start write put_Start;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionLinearGradientBrush
  ICompositionLinearGradientBrush = interface(IInspectable)
  ['{983BC519-A9DB-413C-A2D8-2A9056FC525E}']
    function get_EndPoint: Numerics_Vector2; safecall;
    procedure put_EndPoint(value: Numerics_Vector2); safecall;
    function get_StartPoint: Numerics_Vector2; safecall;
    procedure put_StartPoint(value: Numerics_Vector2); safecall;
    property EndPoint: Numerics_Vector2 read get_EndPoint write put_EndPoint;
    property StartPoint: Numerics_Vector2 read get_StartPoint write put_StartPoint;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionMaskBrush
  ICompositionMaskBrush = interface(IInspectable)
  ['{522CF09E-BE6B-4F41-BE49-F9226D471B4A}']
    function get_Mask: ICompositionBrush; safecall;
    procedure put_Mask(value: ICompositionBrush); safecall;
    function get_Source: ICompositionBrush; safecall;
    procedure put_Source(value: ICompositionBrush); safecall;
    property Mask: ICompositionBrush read get_Mask write put_Mask;
    property Source: ICompositionBrush read get_Source write put_Source;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionNineGridBrush
  ICompositionNineGridBrush = interface(IInspectable)
  ['{F25154E4-BC8C-4BE7-B80F-8685B83C0186}']
    function get_BottomInset: Single; safecall;
    procedure put_BottomInset(value: Single); safecall;
    function get_BottomInsetScale: Single; safecall;
    procedure put_BottomInsetScale(value: Single); safecall;
    function get_IsCenterHollow: Boolean; safecall;
    procedure put_IsCenterHollow(value: Boolean); safecall;
    function get_LeftInset: Single; safecall;
    procedure put_LeftInset(value: Single); safecall;
    function get_LeftInsetScale: Single; safecall;
    procedure put_LeftInsetScale(value: Single); safecall;
    function get_RightInset: Single; safecall;
    procedure put_RightInset(value: Single); safecall;
    function get_RightInsetScale: Single; safecall;
    procedure put_RightInsetScale(value: Single); safecall;
    function get_Source: ICompositionBrush; safecall;
    procedure put_Source(value: ICompositionBrush); safecall;
    function get_TopInset: Single; safecall;
    procedure put_TopInset(value: Single); safecall;
    function get_TopInsetScale: Single; safecall;
    procedure put_TopInsetScale(value: Single); safecall;
    procedure SetInsets(inset: Single); overload; safecall;
    procedure SetInsets(left: Single; top: Single; right: Single; bottom: Single); overload; safecall;
    procedure SetInsetScales(scale: Single); overload; safecall;
    procedure SetInsetScales(left: Single; top: Single; right: Single; bottom: Single); overload; safecall;
    property BottomInset: Single read get_BottomInset write put_BottomInset;
    property BottomInsetScale: Single read get_BottomInsetScale write put_BottomInsetScale;
    property IsCenterHollow: Boolean read get_IsCenterHollow write put_IsCenterHollow;
    property LeftInset: Single read get_LeftInset write put_LeftInset;
    property LeftInsetScale: Single read get_LeftInsetScale write put_LeftInsetScale;
    property RightInset: Single read get_RightInset write put_RightInset;
    property RightInsetScale: Single read get_RightInsetScale write put_RightInsetScale;
    property Source: ICompositionBrush read get_Source write put_Source;
    property TopInset: Single read get_TopInset write put_TopInset;
    property TopInsetScale: Single read get_TopInsetScale write put_TopInsetScale;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IImplicitAnimationCollection
  IImplicitAnimationCollection = interface(IInspectable)
  ['{0598A3FF-0A92-4C9D-A427-B25519250DBF}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionObject2
  ICompositionObject2 = interface(IInspectable)
  ['{EF874EA1-5CFF-4B68-9E30-A1519D08BA03}']
    function get_Comment: HSTRING; safecall;
    procedure put_Comment(value: HSTRING); safecall;
    function get_ImplicitAnimations: IImplicitAnimationCollection; safecall;
    procedure put_ImplicitAnimations(value: IImplicitAnimationCollection); safecall;
    procedure StartAnimationGroup(value: ICompositionAnimationBase); safecall;
    procedure StopAnimationGroup(value: ICompositionAnimationBase); safecall;
    property Comment: HSTRING read get_Comment write put_Comment;
    property ImplicitAnimations: IImplicitAnimationCollection read get_ImplicitAnimations write put_ImplicitAnimations;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionObject3
  ICompositionObject3 = interface(IInspectable)
  ['{4BC27925-DACD-4CF2-98B1-986B76E7EBE6}']
    function get_DispatcherQueue: IDispatcherQueue; safecall;
    property DispatcherQueue: IDispatcherQueue read get_DispatcherQueue;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionObject4
  ICompositionObject4 = interface(IInspectable)
  ['{0BB3784C-346B-4A7C-966B-7310966553D5}']
    function TryGetAnimationController(propertyName: HSTRING): IAnimationController; safecall;
  end;

  // Windows.UI.Composition.ICompositionObjectFactory
  ICompositionObjectFactory = interface(IInspectable)
  ['{51205C5E-558A-4F2A-8D39-37BFE1E20DDD}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionObjectStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionObject)]
  ICompositionObjectStatics = interface(IInspectable)
  ['{C1ED052F-1BA2-44BA-A904-6A882A0A5ADB}']
    procedure StartAnimationWithIAnimationObject(target: IAnimationObject; propertyName: HSTRING; animation: ICompositionAnimation); safecall;
    procedure StartAnimationGroupWithIAnimationObject(target: IAnimationObject; animation: ICompositionAnimationBase); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionPath
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionPath)]
  ICompositionPath = interface(IInspectable)
  ['{66DA1D5F-2E10-4F22-8A06-0A8151919E60}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionPathFactory
  [WinRTClassNameAttribute(SWindows_UI_Composition_CompositionPath)]
  ICompositionPathFactory = interface(IInspectable)
  ['{9C1E8C6A-0F33-4751-9437-EB3FB9D3AB07}']
    function Create(source: IGeometrySource2D): ICompositionPath; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionPathGeometry
  ICompositionPathGeometry = interface(IInspectable)
  ['{0B6A417E-2C77-4C23-AF5E-6304C147BB61}']
    function get_Path: ICompositionPath; safecall;
    procedure put_Path(value: ICompositionPath); safecall;
    property Path: ICompositionPath read get_Path write put_Path;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionProjectedShadowCaster
  ICompositionProjectedShadowCaster = interface(IInspectable)
  ['{B1D7D426-1E36-5A62-BE56-A16112FDD148}']
    function get_Brush: ICompositionBrush; safecall;
    procedure put_Brush(value: ICompositionBrush); safecall;
    function get_CastingVisual: IVisual; safecall;
    procedure put_CastingVisual(value: IVisual); safecall;
    property Brush: ICompositionBrush read get_Brush write put_Brush;
    property CastingVisual: IVisual read get_CastingVisual write put_CastingVisual;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionProjectedShadowCasterCollection
  ICompositionProjectedShadowCasterCollection = interface(IInspectable)
  ['{D2525C0C-E07F-58A3-AC91-37F73EE91740}']
    function get_Count: Integer; safecall;
    procedure InsertAbove(newCaster: ICompositionProjectedShadowCaster; reference: ICompositionProjectedShadowCaster); safecall;
    procedure InsertAtBottom(newCaster: ICompositionProjectedShadowCaster); safecall;
    procedure InsertAtTop(newCaster: ICompositionProjectedShadowCaster); safecall;
    procedure InsertBelow(newCaster: ICompositionProjectedShadowCaster; reference: ICompositionProjectedShadowCaster); safecall;
    procedure Remove(caster: ICompositionProjectedShadowCaster); safecall;
    procedure RemoveAll; safecall;
    property Count: Integer read get_Count;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionProjectedShadowReceiver
  ICompositionProjectedShadowReceiver = interface(IInspectable)
  ['{1377985A-6A49-536A-9BE4-A96A8E5298A9}']
    function get_ReceivingVisual: IVisual; safecall;
    procedure put_ReceivingVisual(value: IVisual); safecall;
    property ReceivingVisual: IVisual read get_ReceivingVisual write put_ReceivingVisual;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionProjectedShadowReceiverUnorderedCollection
  ICompositionProjectedShadowReceiverUnorderedCollection = interface(IInspectable)
  ['{02B3E3B7-27D2-599F-AC4B-AB787CDDE6FD}']
    procedure Add(value: ICompositionProjectedShadowReceiver); safecall;
    function get_Count: Integer; safecall;
    procedure Remove(value: ICompositionProjectedShadowReceiver); safecall;
    procedure RemoveAll; safecall;
    property Count: Integer read get_Count;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionProjectedShadow
  ICompositionProjectedShadow = interface(IInspectable)
  ['{285B8E72-4328-523F-BCF2-5557C52C3B25}']
    function get_BlurRadiusMultiplier: Single; safecall;
    procedure put_BlurRadiusMultiplier(value: Single); safecall;
    function get_Casters: ICompositionProjectedShadowCasterCollection; safecall;
    function get_LightSource: ICompositionLight; safecall;
    procedure put_LightSource(value: ICompositionLight); safecall;
    function get_MaxBlurRadius: Single; safecall;
    procedure put_MaxBlurRadius(value: Single); safecall;
    function get_MinBlurRadius: Single; safecall;
    procedure put_MinBlurRadius(value: Single); safecall;
    function get_Receivers: ICompositionProjectedShadowReceiverUnorderedCollection; safecall;
    property BlurRadiusMultiplier: Single read get_BlurRadiusMultiplier write put_BlurRadiusMultiplier;
    property Casters: ICompositionProjectedShadowCasterCollection read get_Casters;
    property LightSource: ICompositionLight read get_LightSource write put_LightSource;
    property MaxBlurRadius: Single read get_MaxBlurRadius write put_MaxBlurRadius;
    property MinBlurRadius: Single read get_MinBlurRadius write put_MinBlurRadius;
    property Receivers: ICompositionProjectedShadowReceiverUnorderedCollection read get_Receivers;
  end;

  // Windows.UI.Composition.ICompositionPropertySet2
  ICompositionPropertySet2 = interface(IInspectable)
  ['{DE80731E-A211-4455-8880-7D0F3F6A44FD}']
    procedure InsertBoolean(propertyName: HSTRING; value: Boolean); safecall;
    function TryGetBoolean(propertyName: HSTRING; out value: Boolean): CompositionGetValueStatus; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionRadialGradientBrush
  ICompositionRadialGradientBrush = interface(IInspectable)
  ['{3D3B50C5-E3FA-4CE2-B9FC-3EE12561788F}']
    function get_EllipseCenter: Numerics_Vector2; safecall;
    procedure put_EllipseCenter(value: Numerics_Vector2); safecall;
    function get_EllipseRadius: Numerics_Vector2; safecall;
    procedure put_EllipseRadius(value: Numerics_Vector2); safecall;
    function get_GradientOriginOffset: Numerics_Vector2; safecall;
    procedure put_GradientOriginOffset(value: Numerics_Vector2); safecall;
    property EllipseCenter: Numerics_Vector2 read get_EllipseCenter write put_EllipseCenter;
    property EllipseRadius: Numerics_Vector2 read get_EllipseRadius write put_EllipseRadius;
    property GradientOriginOffset: Numerics_Vector2 read get_GradientOriginOffset write put_GradientOriginOffset;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionRectangleGeometry
  ICompositionRectangleGeometry = interface(IInspectable)
  ['{0CD51428-5356-4246-AECF-7A0B76975400}']
    function get_Offset: Numerics_Vector2; safecall;
    procedure put_Offset(value: Numerics_Vector2); safecall;
    function get_Size: Numerics_Vector2; safecall;
    procedure put_Size(value: Numerics_Vector2); safecall;
    property Offset: Numerics_Vector2 read get_Offset write put_Offset;
    property Size: Numerics_Vector2 read get_Size write put_Size;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionRoundedRectangleGeometry
  ICompositionRoundedRectangleGeometry = interface(IInspectable)
  ['{8770C822-1D50-4B8B-B013-7C9A0E46935F}']
    function get_CornerRadius: Numerics_Vector2; safecall;
    procedure put_CornerRadius(value: Numerics_Vector2); safecall;
    function get_Offset: Numerics_Vector2; safecall;
    procedure put_Offset(value: Numerics_Vector2); safecall;
    function get_Size: Numerics_Vector2; safecall;
    procedure put_Size(value: Numerics_Vector2); safecall;
    property CornerRadius: Numerics_Vector2 read get_CornerRadius write put_CornerRadius;
    property Offset: Numerics_Vector2 read get_Offset write put_Offset;
    property Size: Numerics_Vector2 read get_Size write put_Size;
  end;

  // Windows.UI.Composition.ICompositionShadow
  ICompositionShadow = interface(IInspectable)
  ['{329E52E2-4335-49CC-B14A-37782D10F0C4}']
  end;

  // Windows.UI.Composition.ICompositionShadowFactory
  ICompositionShadowFactory = interface(IInspectable)
  ['{221F492F-DCBA-4B91-999E-1DC217A01530}']
  end;

  // Windows.UI.Composition.ICompositionShapeFactory
  ICompositionShapeFactory = interface(IInspectable)
  ['{1DFC36D0-B05A-44EF-82B0-12118BCD4CD0}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionSpriteShape
  ICompositionSpriteShape = interface(IInspectable)
  ['{401B61BB-0007-4363-B1F3-6BCC003FB83E}']
    function get_FillBrush: ICompositionBrush; safecall;
    procedure put_FillBrush(value: ICompositionBrush); safecall;
    function get_Geometry: ICompositionGeometry; safecall;
    procedure put_Geometry(value: ICompositionGeometry); safecall;
    function get_IsStrokeNonScaling: Boolean; safecall;
    procedure put_IsStrokeNonScaling(value: Boolean); safecall;
    function get_StrokeBrush: ICompositionBrush; safecall;
    procedure put_StrokeBrush(value: ICompositionBrush); safecall;
    function get_StrokeDashArray: IVector_1__Single; safecall;
    function get_StrokeDashCap: CompositionStrokeCap; safecall;
    procedure put_StrokeDashCap(value: CompositionStrokeCap); safecall;
    function get_StrokeDashOffset: Single; safecall;
    procedure put_StrokeDashOffset(value: Single); safecall;
    function get_StrokeEndCap: CompositionStrokeCap; safecall;
    procedure put_StrokeEndCap(value: CompositionStrokeCap); safecall;
    function get_StrokeLineJoin: CompositionStrokeLineJoin; safecall;
    procedure put_StrokeLineJoin(value: CompositionStrokeLineJoin); safecall;
    function get_StrokeMiterLimit: Single; safecall;
    procedure put_StrokeMiterLimit(value: Single); safecall;
    function get_StrokeStartCap: CompositionStrokeCap; safecall;
    procedure put_StrokeStartCap(value: CompositionStrokeCap); safecall;
    function get_StrokeThickness: Single; safecall;
    procedure put_StrokeThickness(value: Single); safecall;
    property FillBrush: ICompositionBrush read get_FillBrush write put_FillBrush;
    property Geometry: ICompositionGeometry read get_Geometry write put_Geometry;
    property IsStrokeNonScaling: Boolean read get_IsStrokeNonScaling write put_IsStrokeNonScaling;
    property StrokeBrush: ICompositionBrush read get_StrokeBrush write put_StrokeBrush;
    property StrokeDashArray: IVector_1__Single read get_StrokeDashArray;
    property StrokeDashCap: CompositionStrokeCap read get_StrokeDashCap write put_StrokeDashCap;
    property StrokeDashOffset: Single read get_StrokeDashOffset write put_StrokeDashOffset;
    property StrokeEndCap: CompositionStrokeCap read get_StrokeEndCap write put_StrokeEndCap;
    property StrokeLineJoin: CompositionStrokeLineJoin read get_StrokeLineJoin write put_StrokeLineJoin;
    property StrokeMiterLimit: Single read get_StrokeMiterLimit write put_StrokeMiterLimit;
    property StrokeStartCap: CompositionStrokeCap read get_StrokeStartCap write put_StrokeStartCap;
    property StrokeThickness: Single read get_StrokeThickness write put_StrokeThickness;
  end;

  // Windows.UI.Composition.ICompositionSurfaceBrush2
  ICompositionSurfaceBrush2 = interface(IInspectable)
  ['{D27174D5-64F5-4692-9DC7-71B61D7E5880}']
    function get_AnchorPoint: Numerics_Vector2; safecall;
    procedure put_AnchorPoint(value: Numerics_Vector2); safecall;
    function get_CenterPoint: Numerics_Vector2; safecall;
    procedure put_CenterPoint(value: Numerics_Vector2); safecall;
    function get_Offset: Numerics_Vector2; safecall;
    procedure put_Offset(value: Numerics_Vector2); safecall;
    function get_RotationAngle: Single; safecall;
    procedure put_RotationAngle(value: Single); safecall;
    function get_RotationAngleInDegrees: Single; safecall;
    procedure put_RotationAngleInDegrees(value: Single); safecall;
    function get_Scale: Numerics_Vector2; safecall;
    procedure put_Scale(value: Numerics_Vector2); safecall;
    function get_TransformMatrix: Numerics_Matrix3x2; safecall;
    procedure put_TransformMatrix(value: Numerics_Matrix3x2); safecall;
    property AnchorPoint: Numerics_Vector2 read get_AnchorPoint write put_AnchorPoint;
    property CenterPoint: Numerics_Vector2 read get_CenterPoint write put_CenterPoint;
    property Offset: Numerics_Vector2 read get_Offset write put_Offset;
    property RotationAngle: Single read get_RotationAngle write put_RotationAngle;
    property RotationAngleInDegrees: Single read get_RotationAngleInDegrees write put_RotationAngleInDegrees;
    property Scale: Numerics_Vector2 read get_Scale write put_Scale;
    property TransformMatrix: Numerics_Matrix3x2 read get_TransformMatrix write put_TransformMatrix;
  end;

  // Windows.UI.Composition.ICompositionSurfaceBrush3
  ICompositionSurfaceBrush3 = interface(IInspectable)
  ['{550BB289-1FE0-42E5-8195-1EEFA87FF08E}']
    function get_SnapToPixels: Boolean; safecall;
    procedure put_SnapToPixels(value: Boolean); safecall;
    property SnapToPixels: Boolean read get_SnapToPixels write put_SnapToPixels;
  end;

  // Windows.UI.Composition.ICompositionTargetFactory
  ICompositionTargetFactory = interface(IInspectable)
  ['{93CD9D2B-8516-4B14-A8CE-F49E2119EC42}']
  end;

  // Windows.UI.Composition.ICompositionTransform
  ICompositionTransform = interface(IInspectable)
  ['{7CD54529-FBED-4112-ABC5-185906DD927C}']
  end;

  // Windows.UI.Composition.ICompositionTransformFactory
  ICompositionTransformFactory = interface(IInspectable)
  ['{AAAECA26-C149-517A-8F72-6BFF7A65CE08}']
  end;

  // Windows.UI.Composition.ICompositionVirtualDrawingSurfaceFactory
  ICompositionVirtualDrawingSurfaceFactory = interface(IInspectable)
  ['{6766106C-D56B-4A49-B1DF-5076A0620768}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ICompositionVisualSurface
  ICompositionVisualSurface = interface(IInspectable)
  ['{B224D803-4F6E-4A3F-8CAE-3DC1CDA74FC6}']
    function get_SourceVisual: IVisual; safecall;
    procedure put_SourceVisual(value: IVisual); safecall;
    function get_SourceOffset: Numerics_Vector2; safecall;
    procedure put_SourceOffset(value: Numerics_Vector2); safecall;
    function get_SourceSize: Numerics_Vector2; safecall;
    procedure put_SourceSize(value: Numerics_Vector2); safecall;
    property SourceOffset: Numerics_Vector2 read get_SourceOffset write put_SourceOffset;
    property SourceSize: Numerics_Vector2 read get_SourceSize write put_SourceSize;
    property SourceVisual: IVisual read get_SourceVisual write put_SourceVisual;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IDistantLight
  IDistantLight = interface(IInspectable)
  ['{318CFAFC-5CE3-4B55-AB5D-07A00353AC99}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    function get_CoordinateSpace: IVisual; safecall;
    procedure put_CoordinateSpace(value: IVisual); safecall;
    function get_Direction: Numerics_Vector3; safecall;
    procedure put_Direction(value: Numerics_Vector3); safecall;
    property Color_: Color read get_Color write put_Color;
    property CoordinateSpace: IVisual read get_CoordinateSpace write put_CoordinateSpace;
    property Direction: Numerics_Vector3 read get_Direction write put_Direction;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IDropShadow
  IDropShadow = interface(IInspectable)
  ['{CB977C07-A154-4851-85E7-A8924C84FAD8}']
    function get_BlurRadius: Single; safecall;
    procedure put_BlurRadius(value: Single); safecall;
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    function get_Mask: ICompositionBrush; safecall;
    procedure put_Mask(value: ICompositionBrush); safecall;
    function get_Offset: Numerics_Vector3; safecall;
    procedure put_Offset(value: Numerics_Vector3); safecall;
    function get_Opacity: Single; safecall;
    procedure put_Opacity(value: Single); safecall;
    property BlurRadius: Single read get_BlurRadius write put_BlurRadius;
    property Color_: Color read get_Color write put_Color;
    property Mask: ICompositionBrush read get_Mask write put_Mask;
    property Offset: Numerics_Vector3 read get_Offset write put_Offset;
    property Opacity: Single read get_Opacity write put_Opacity;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ILayerVisual
  ILayerVisual = interface(IInspectable)
  ['{AF843985-0444-4887-8E83-B40B253F822C}']
    function get_Effect: ICompositionEffectBrush; safecall;
    procedure put_Effect(value: ICompositionEffectBrush); safecall;
    property Effect: ICompositionEffectBrush read get_Effect write put_Effect;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IPointLight
  IPointLight = interface(IInspectable)
  ['{B18545B3-0C5A-4AB0-BEDC-4F3546948272}']
    function get_Color: Color; safecall;
    procedure put_Color(value: Color); safecall;
    function get_ConstantAttenuation: Single; safecall;
    procedure put_ConstantAttenuation(value: Single); safecall;
    function get_CoordinateSpace: IVisual; safecall;
    procedure put_CoordinateSpace(value: IVisual); safecall;
    function get_LinearAttenuation: Single; safecall;
    procedure put_LinearAttenuation(value: Single); safecall;
    function get_Offset: Numerics_Vector3; safecall;
    procedure put_Offset(value: Numerics_Vector3); safecall;
    function get_QuadraticAttenuation: Single; safecall;
    procedure put_QuadraticAttenuation(value: Single); safecall;
    property Color_: Color read get_Color write put_Color;
    property ConstantAttenuation: Single read get_ConstantAttenuation write put_ConstantAttenuation;
    property CoordinateSpace: IVisual read get_CoordinateSpace write put_CoordinateSpace;
    property LinearAttenuation: Single read get_LinearAttenuation write put_LinearAttenuation;
    property Offset: Numerics_Vector3 read get_Offset write put_Offset;
    property QuadraticAttenuation: Single read get_QuadraticAttenuation write put_QuadraticAttenuation;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ISpotLight
  ISpotLight = interface(IInspectable)
  ['{5A9FE273-44A1-4F95-A422-8FA5116BDB44}']
    function get_ConstantAttenuation: Single; safecall;
    procedure put_ConstantAttenuation(value: Single); safecall;
    function get_CoordinateSpace: IVisual; safecall;
    procedure put_CoordinateSpace(value: IVisual); safecall;
    function get_Direction: Numerics_Vector3; safecall;
    procedure put_Direction(value: Numerics_Vector3); safecall;
    function get_InnerConeAngle: Single; safecall;
    procedure put_InnerConeAngle(value: Single); safecall;
    function get_InnerConeAngleInDegrees: Single; safecall;
    procedure put_InnerConeAngleInDegrees(value: Single); safecall;
    function get_InnerConeColor: Color; safecall;
    procedure put_InnerConeColor(value: Color); safecall;
    function get_LinearAttenuation: Single; safecall;
    procedure put_LinearAttenuation(value: Single); safecall;
    function get_Offset: Numerics_Vector3; safecall;
    procedure put_Offset(value: Numerics_Vector3); safecall;
    function get_OuterConeAngle: Single; safecall;
    procedure put_OuterConeAngle(value: Single); safecall;
    function get_OuterConeAngleInDegrees: Single; safecall;
    procedure put_OuterConeAngleInDegrees(value: Single); safecall;
    function get_OuterConeColor: Color; safecall;
    procedure put_OuterConeColor(value: Color); safecall;
    function get_QuadraticAttenuation: Single; safecall;
    procedure put_QuadraticAttenuation(value: Single); safecall;
    property ConstantAttenuation: Single read get_ConstantAttenuation write put_ConstantAttenuation;
    property CoordinateSpace: IVisual read get_CoordinateSpace write put_CoordinateSpace;
    property Direction: Numerics_Vector3 read get_Direction write put_Direction;
    property InnerConeAngle: Single read get_InnerConeAngle write put_InnerConeAngle;
    property InnerConeAngleInDegrees: Single read get_InnerConeAngleInDegrees write put_InnerConeAngleInDegrees;
    property InnerConeColor: Color read get_InnerConeColor write put_InnerConeColor;
    property LinearAttenuation: Single read get_LinearAttenuation write put_LinearAttenuation;
    property Offset: Numerics_Vector3 read get_Offset write put_Offset;
    property OuterConeAngle: Single read get_OuterConeAngle write put_OuterConeAngle;
    property OuterConeAngleInDegrees: Single read get_OuterConeAngleInDegrees write put_OuterConeAngleInDegrees;
    property OuterConeColor: Color read get_OuterConeColor write put_OuterConeColor;
    property QuadraticAttenuation: Single read get_QuadraticAttenuation write put_QuadraticAttenuation;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IStepEasingFunction
  IStepEasingFunction = interface(IInspectable)
  ['{D0CAA74B-560C-4A0B-A5F6-206CA8C3ECD6}']
    function get_FinalStep: Integer; safecall;
    procedure put_FinalStep(value: Integer); safecall;
    function get_InitialStep: Integer; safecall;
    procedure put_InitialStep(value: Integer); safecall;
    function get_IsFinalStepSingleFrame: Boolean; safecall;
    procedure put_IsFinalStepSingleFrame(value: Boolean); safecall;
    function get_IsInitialStepSingleFrame: Boolean; safecall;
    procedure put_IsInitialStepSingleFrame(value: Boolean); safecall;
    function get_StepCount: Integer; safecall;
    procedure put_StepCount(value: Integer); safecall;
    property FinalStep: Integer read get_FinalStep write put_FinalStep;
    property InitialStep: Integer read get_InitialStep write put_InitialStep;
    property IsFinalStepSingleFrame: Boolean read get_IsFinalStepSingleFrame write put_IsFinalStepSingleFrame;
    property IsInitialStepSingleFrame: Boolean read get_IsInitialStepSingleFrame write put_IsInitialStepSingleFrame;
    property StepCount: Integer read get_StepCount write put_StepCount;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositor2
  ICompositor2 = interface(IInspectable)
  ['{735081DC-5E24-45DA-A38F-E32CC349A9A0}']
    function CreateAmbientLight: IAmbientLight; safecall;
    function CreateAnimationGroup: ICompositionAnimationGroup; safecall;
    function CreateBackdropBrush: ICompositionBackdropBrush; safecall;
    function CreateDistantLight: IDistantLight; safecall;
    function CreateDropShadow: IDropShadow; safecall;
    function CreateImplicitAnimationCollection: IImplicitAnimationCollection; safecall;
    function CreateLayerVisual: ILayerVisual; safecall;
    function CreateMaskBrush: ICompositionMaskBrush; safecall;
    function CreateNineGridBrush: ICompositionNineGridBrush; safecall;
    function CreatePointLight: IPointLight; safecall;
    function CreateSpotLight: ISpotLight; safecall;
    function CreateStepEasingFunction: IStepEasingFunction; overload; safecall;
    function CreateStepEasingFunction(stepCount: Integer): IStepEasingFunction; overload; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositor3
  ICompositor3 = interface(IInspectable)
  ['{C9DD8EF0-6EB1-4E3C-A658-675D9C64D4AB}']
    function CreateHostBackdropBrush: ICompositionBackdropBrush; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ISpringScalarNaturalMotionAnimation
  ISpringScalarNaturalMotionAnimation = interface(IInspectable)
  ['{0572A95F-37F9-4FBE-B87B-5CD03A89501C}']
    function get_DampingRatio: Single; safecall;
    procedure put_DampingRatio(value: Single); safecall;
    function get_Period: TimeSpan; safecall;
    procedure put_Period(value: TimeSpan); safecall;
    property DampingRatio: Single read get_DampingRatio write put_DampingRatio;
    property Period: TimeSpan read get_Period write put_Period;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ISpringVector2NaturalMotionAnimation
  ISpringVector2NaturalMotionAnimation = interface(IInspectable)
  ['{23F494B5-EE73-4F0F-A423-402B946DF4B3}']
    function get_DampingRatio: Single; safecall;
    procedure put_DampingRatio(value: Single); safecall;
    function get_Period: TimeSpan; safecall;
    procedure put_Period(value: TimeSpan); safecall;
    property DampingRatio: Single read get_DampingRatio write put_DampingRatio;
    property Period: TimeSpan read get_Period write put_Period;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.ISpringVector3NaturalMotionAnimation
  ISpringVector3NaturalMotionAnimation = interface(IInspectable)
  ['{6C8749DF-D57B-4794-8E2D-CECB11E194E5}']
    function get_DampingRatio: Single; safecall;
    procedure put_DampingRatio(value: Single); safecall;
    function get_Period: TimeSpan; safecall;
    procedure put_Period(value: TimeSpan); safecall;
    property DampingRatio: Single read get_DampingRatio write put_DampingRatio;
    property Period: TimeSpan read get_Period write put_Period;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositor4
  ICompositor4 = interface(IInspectable)
  ['{AE47E78A-7910-4425-A482-A05B758ADCE9}']
    function CreateColorGradientStop: ICompositionColorGradientStop; overload; safecall;
    function CreateColorGradientStop(offset: Single; color: Color): ICompositionColorGradientStop; overload; safecall;
    function CreateLinearGradientBrush: ICompositionLinearGradientBrush; safecall;
    function CreateSpringScalarAnimation: ISpringScalarNaturalMotionAnimation; safecall;
    function CreateSpringVector2Animation: ISpringVector2NaturalMotionAnimation; safecall;
    function CreateSpringVector3Animation: ISpringVector3NaturalMotionAnimation; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IPathKeyFrameAnimation
  IPathKeyFrameAnimation = interface(IInspectable)
  ['{9D0D18C9-1576-4B3F-BE60-1D5031F5E71B}']
    procedure InsertKeyFrame(normalizedProgressKey: Single; path: ICompositionPath); overload; safecall;
    procedure InsertKeyFrame(normalizedProgressKey: Single; path: ICompositionPath; easingFunction: ICompositionEasingFunction); overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IShapeVisual
  IShapeVisual = interface(IInspectable)
  ['{F2BD13C3-BA7E-4B0F-9126-FFB7536B8176}']
    function get_Shapes: IVector_1__ICompositionShape; safecall;
    function get_ViewBox: ICompositionViewBox; safecall;
    procedure put_ViewBox(value: ICompositionViewBox); safecall;
    property Shapes: IVector_1__ICompositionShape read get_Shapes;
    property ViewBox: ICompositionViewBox read get_ViewBox write put_ViewBox;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositor5
  ICompositor5 = interface(IInspectable)
  ['{48EA31AD-7FCD-4076-A79C-90CC4B852C9B}']
    function get_Comment: HSTRING; safecall;
    procedure put_Comment(value: HSTRING); safecall;
    function get_GlobalPlaybackRate: Single; safecall;
    procedure put_GlobalPlaybackRate(value: Single); safecall;
    function CreateBounceScalarAnimation: IBounceScalarNaturalMotionAnimation; safecall;
    function CreateBounceVector2Animation: IBounceVector2NaturalMotionAnimation; safecall;
    function CreateBounceVector3Animation: IBounceVector3NaturalMotionAnimation; safecall;
    function CreateContainerShape: ICompositionContainerShape; safecall;
    function CreateEllipseGeometry: ICompositionEllipseGeometry; safecall;
    function CreateLineGeometry: ICompositionLineGeometry; safecall;
    function CreatePathGeometry: ICompositionPathGeometry; overload; safecall;
    function CreatePathGeometry(path: ICompositionPath): ICompositionPathGeometry; overload; safecall;
    function CreatePathKeyFrameAnimation: IPathKeyFrameAnimation; safecall;
    function CreateRectangleGeometry: ICompositionRectangleGeometry; safecall;
    function CreateRoundedRectangleGeometry: ICompositionRoundedRectangleGeometry; safecall;
    function CreateShapeVisual: IShapeVisual; safecall;
    function CreateSpriteShape: ICompositionSpriteShape; overload; safecall;
    function CreateSpriteShape(geometry: ICompositionGeometry): ICompositionSpriteShape; overload; safecall;
    function CreateViewBox: ICompositionViewBox; safecall;
    function RequestCommitAsync: IAsyncAction; safecall;
    property Comment: HSTRING read get_Comment write put_Comment;
    property GlobalPlaybackRate: Single read get_GlobalPlaybackRate write put_GlobalPlaybackRate;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IRedirectVisual
  IRedirectVisual = interface(IInspectable)
  ['{8CC6E340-8B75-5422-B06F-09FFE9F8617E}']
    function get_Source: IVisual; safecall;
    procedure put_Source(value: IVisual); safecall;
    property Source: IVisual read get_Source write put_Source;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositor6
  ICompositor6 = interface(IInspectable)
  ['{7A38B2BD-CEC8-4EEB-830F-D8D07AEDEBC3}']
    function CreateGeometricClip: ICompositionGeometricClip; overload; safecall;
    function CreateGeometricClip(geometry: ICompositionGeometry): ICompositionGeometricClip; overload; safecall;
    function CreateRedirectVisual: IRedirectVisual; overload; safecall;
    function CreateRedirectVisual(source: IVisual): IRedirectVisual; overload; safecall;
    function CreateBooleanKeyFrameAnimation: IBooleanKeyFrameAnimation; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositorStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Compositor)]
  ICompositorStatics = interface(IInspectable)
  ['{080DB93E-121E-4D97-8B74-1DFCF91987EA}']
    function get_MaxGlobalPlaybackRate: Single; safecall;
    function get_MinGlobalPlaybackRate: Single; safecall;
    property MaxGlobalPlaybackRate: Single read get_MaxGlobalPlaybackRate;
    property MinGlobalPlaybackRate: Single read get_MinGlobalPlaybackRate;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositorWithProjectedShadow
  ICompositorWithProjectedShadow = interface(IInspectable)
  ['{A2E6330E-8A60-5A38-BB85-B44EA901677C}']
    function CreateProjectedShadowCaster: ICompositionProjectedShadowCaster; safecall;
    function CreateProjectedShadow: ICompositionProjectedShadow; safecall;
    function CreateProjectedShadowReceiver: ICompositionProjectedShadowReceiver; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositorWithRadialGradient
  ICompositorWithRadialGradient = interface(IInspectable)
  ['{98B9C1A7-8E71-4B53-B4A8-69BA5D19DC5B}']
    function CreateRadialGradientBrush: ICompositionRadialGradientBrush; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.ICompositorWithVisualSurface
  ICompositorWithVisualSurface = interface(IInspectable)
  ['{CFA1658B-0123-4551-8891-89BDCC40322B}']
    function CreateVisualSurface: ICompositionVisualSurface; safecall;
  end;

  // Windows.UI.Composition.IContainerVisualFactory
  IContainerVisualFactory = interface(IInspectable)
  ['{0363A65B-C7DA-4D9A-95F4-69B5C8DF670B}']
  end;

  // Windows.UI.Composition.IDistantLight2
  IDistantLight2 = interface(IInspectable)
  ['{DBCDAA1C-294B-48D7-B60E-76DF64AA392B}']
    function get_Intensity: Single; safecall;
    procedure put_Intensity(value: Single); safecall;
    property Intensity: Single read get_Intensity write put_Intensity;
  end;

  // Windows.UI.Composition.IDropShadow2
  IDropShadow2 = interface(IInspectable)
  ['{6C4218BC-15B9-4C2D-8D4A-0767DF11977A}']
    function get_SourcePolicy: CompositionDropShadowSourcePolicy; safecall;
    procedure put_SourcePolicy(value: CompositionDropShadowSourcePolicy); safecall;
    property SourcePolicy: CompositionDropShadowSourcePolicy read get_SourcePolicy write put_SourcePolicy;
  end;

  // Windows.UI.Composition.IKeyFrameAnimation
  IKeyFrameAnimation = interface(IInspectable)
  ['{126E7F22-3AE9-4540-9A8A-DEAE8A4A4A84}']
    function get_DelayTime: TimeSpan; safecall;
    procedure put_DelayTime(value: TimeSpan); safecall;
    function get_Duration: TimeSpan; safecall;
    procedure put_Duration(value: TimeSpan); safecall;
    function get_IterationBehavior: AnimationIterationBehavior; safecall;
    procedure put_IterationBehavior(value: AnimationIterationBehavior); safecall;
    function get_IterationCount: Integer; safecall;
    procedure put_IterationCount(value: Integer); safecall;
    function get_KeyFrameCount: Integer; safecall;
    function get_StopBehavior: AnimationStopBehavior; safecall;
    procedure put_StopBehavior(value: AnimationStopBehavior); safecall;
    procedure InsertExpressionKeyFrame(normalizedProgressKey: Single; value: HSTRING); overload; safecall;
    procedure InsertExpressionKeyFrame(normalizedProgressKey: Single; value: HSTRING; easingFunction: ICompositionEasingFunction); overload; safecall;
    property DelayTime: TimeSpan read get_DelayTime write put_DelayTime;
    property Duration: TimeSpan read get_Duration write put_Duration;
    property IterationBehavior: AnimationIterationBehavior read get_IterationBehavior write put_IterationBehavior;
    property IterationCount: Integer read get_IterationCount write put_IterationCount;
    property KeyFrameCount: Integer read get_KeyFrameCount;
    property StopBehavior: AnimationStopBehavior read get_StopBehavior write put_StopBehavior;
  end;

  // Windows.UI.Composition.IKeyFrameAnimation2
  IKeyFrameAnimation2 = interface(IInspectable)
  ['{F4B488BB-2940-4EC0-A41A-EB6D801A2F18}']
    function get_Direction: AnimationDirection; safecall;
    procedure put_Direction(value: AnimationDirection); safecall;
    property Direction: AnimationDirection read get_Direction write put_Direction;
  end;

  // Windows.UI.Composition.IKeyFrameAnimation3
  IKeyFrameAnimation3 = interface(IInspectable)
  ['{845BF0B4-D8DE-462F-8753-C80D43C6FF5A}']
    function get_DelayBehavior: AnimationDelayBehavior; safecall;
    procedure put_DelayBehavior(value: AnimationDelayBehavior); safecall;
    property DelayBehavior: AnimationDelayBehavior read get_DelayBehavior write put_DelayBehavior;
  end;

  // Windows.UI.Composition.IKeyFrameAnimationFactory
  IKeyFrameAnimationFactory = interface(IInspectable)
  ['{BF0803F8-712A-4FC1-8C87-970859ED8D2E}']
  end;

  // Windows.UI.Composition.ILayerVisual2
  ILayerVisual2 = interface(IInspectable)
  ['{98F9AEEB-6F23-49F1-90B1-1F59A14FBCE3}']
    function get_Shadow: ICompositionShadow; safecall;
    procedure put_Shadow(value: ICompositionShadow); safecall;
    property Shadow: ICompositionShadow read get_Shadow write put_Shadow;
  end;

  // Windows.UI.Composition.INaturalMotionAnimation
  INaturalMotionAnimation = interface(IInspectable)
  ['{438DE12D-769B-4821-A949-284A6547E873}']
    function get_DelayBehavior: AnimationDelayBehavior; safecall;
    procedure put_DelayBehavior(value: AnimationDelayBehavior); safecall;
    function get_DelayTime: TimeSpan; safecall;
    procedure put_DelayTime(value: TimeSpan); safecall;
    function get_StopBehavior: AnimationStopBehavior; safecall;
    procedure put_StopBehavior(value: AnimationStopBehavior); safecall;
    property DelayBehavior: AnimationDelayBehavior read get_DelayBehavior write put_DelayBehavior;
    property DelayTime: TimeSpan read get_DelayTime write put_DelayTime;
    property StopBehavior: AnimationStopBehavior read get_StopBehavior write put_StopBehavior;
  end;

  // Windows.UI.Composition.INaturalMotionAnimationFactory
  INaturalMotionAnimationFactory = interface(IInspectable)
  ['{F53ACB06-CF6A-4387-A3FE-5221F3E7E0E0}']
  end;

  // Windows.UI.Composition.IPointLight2
  IPointLight2 = interface(IInspectable)
  ['{EFE98F2C-0678-4F69-B164-A810D995BCB7}']
    function get_Intensity: Single; safecall;
    procedure put_Intensity(value: Single); safecall;
    property Intensity: Single read get_Intensity write put_Intensity;
  end;

  // Windows.UI.Composition.IPointLight3
  IPointLight3 = interface(IInspectable)
  ['{4C0A8367-D4E9-468A-87AE-7BA43AB29485}']
    function get_MinAttenuationCutoff: Single; safecall;
    procedure put_MinAttenuationCutoff(value: Single); safecall;
    function get_MaxAttenuationCutoff: Single; safecall;
    procedure put_MaxAttenuationCutoff(value: Single); safecall;
    property MaxAttenuationCutoff: Single read get_MaxAttenuationCutoff write put_MaxAttenuationCutoff;
    property MinAttenuationCutoff: Single read get_MinAttenuationCutoff write put_MinAttenuationCutoff;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IScalarNaturalMotionAnimation
  IScalarNaturalMotionAnimation = interface(IInspectable)
  ['{94A94581-BF92-495B-B5BD-D2C659430737}']
    function get_FinalValue: IReference_1__Single; safecall;
    procedure put_FinalValue(value: IReference_1__Single); safecall;
    function get_InitialValue: IReference_1__Single; safecall;
    procedure put_InitialValue(value: IReference_1__Single); safecall;
    function get_InitialVelocity: Single; safecall;
    procedure put_InitialVelocity(value: Single); safecall;
    property FinalValue: IReference_1__Single read get_FinalValue write put_FinalValue;
    property InitialValue: IReference_1__Single read get_InitialValue write put_InitialValue;
    property InitialVelocity: Single read get_InitialVelocity write put_InitialVelocity;
  end;

  // Windows.UI.Composition.IScalarNaturalMotionAnimationFactory
  IScalarNaturalMotionAnimationFactory = interface(IInspectable)
  ['{835AA4FC-671C-41DD-AF48-AE8DEF8B1529}']
  end;

  // Windows.UI.Composition.ISpotLight2
  ISpotLight2 = interface(IInspectable)
  ['{64EE615E-0686-4DEA-A9E8-BC3A8C701459}']
    function get_InnerConeIntensity: Single; safecall;
    procedure put_InnerConeIntensity(value: Single); safecall;
    function get_OuterConeIntensity: Single; safecall;
    procedure put_OuterConeIntensity(value: Single); safecall;
    property InnerConeIntensity: Single read get_InnerConeIntensity write put_InnerConeIntensity;
    property OuterConeIntensity: Single read get_OuterConeIntensity write put_OuterConeIntensity;
  end;

  // Windows.UI.Composition.ISpotLight3
  ISpotLight3 = interface(IInspectable)
  ['{E4D03EEA-131F-480E-859E-B82705B74360}']
    function get_MinAttenuationCutoff: Single; safecall;
    procedure put_MinAttenuationCutoff(value: Single); safecall;
    function get_MaxAttenuationCutoff: Single; safecall;
    procedure put_MaxAttenuationCutoff(value: Single); safecall;
    property MaxAttenuationCutoff: Single read get_MaxAttenuationCutoff write put_MaxAttenuationCutoff;
    property MinAttenuationCutoff: Single read get_MinAttenuationCutoff write put_MinAttenuationCutoff;
  end;

  // Windows.UI.Composition.ISpriteVisual2
  ISpriteVisual2 = interface(IInspectable)
  ['{588C9664-997A-4850-91FE-53CB58F81CE9}']
    function get_Shadow: ICompositionShadow; safecall;
    procedure put_Shadow(value: ICompositionShadow); safecall;
    property Shadow: ICompositionShadow read get_Shadow write put_Shadow;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.IVector2NaturalMotionAnimation
  IVector2NaturalMotionAnimation = interface(IInspectable)
  ['{0F3E0B7D-E512-479D-A00C-77C93A30A395}']
    function get_FinalValue: IReference_1__Numerics_Vector2; safecall;
    procedure put_FinalValue(value: IReference_1__Numerics_Vector2); safecall;
    function get_InitialValue: IReference_1__Numerics_Vector2; safecall;
    procedure put_InitialValue(value: IReference_1__Numerics_Vector2); safecall;
    function get_InitialVelocity: Numerics_Vector2; safecall;
    procedure put_InitialVelocity(value: Numerics_Vector2); safecall;
    property FinalValue: IReference_1__Numerics_Vector2 read get_FinalValue write put_FinalValue;
    property InitialValue: IReference_1__Numerics_Vector2 read get_InitialValue write put_InitialValue;
    property InitialVelocity: Numerics_Vector2 read get_InitialVelocity write put_InitialVelocity;
  end;

  // Windows.UI.Composition.IVector2NaturalMotionAnimationFactory
  IVector2NaturalMotionAnimationFactory = interface(IInspectable)
  ['{8C74FF61-0761-48A2-BDDB-6AFCC52B89D8}']
  end;

  // Windows.UI.Composition.IVector3NaturalMotionAnimation
  IVector3NaturalMotionAnimation = interface(IInspectable)
  ['{9C17042C-E2CA-45AD-969E-4E78B7B9AD41}']
    function get_FinalValue: IReference_1__Numerics_Vector3; safecall;
    procedure put_FinalValue(value: IReference_1__Numerics_Vector3); safecall;
    function get_InitialValue: IReference_1__Numerics_Vector3; safecall;
    procedure put_InitialValue(value: IReference_1__Numerics_Vector3); safecall;
    function get_InitialVelocity: Numerics_Vector3; safecall;
    procedure put_InitialVelocity(value: Numerics_Vector3); safecall;
    property FinalValue: IReference_1__Numerics_Vector3 read get_FinalValue write put_FinalValue;
    property InitialValue: IReference_1__Numerics_Vector3 read get_InitialValue write put_InitialValue;
    property InitialVelocity: Numerics_Vector3 read get_InitialVelocity write put_InitialVelocity;
  end;

  // Windows.UI.Composition.IVector3NaturalMotionAnimationFactory
  IVector3NaturalMotionAnimationFactory = interface(IInspectable)
  ['{21A81D2F-0880-457B-AC87-B609018C876D}']
  end;

  // Windows.UI.Composition.IVisual2
  IVisual2 = interface(IInspectable)
  ['{3052B611-56C3-4C3E-8BF3-F6E1AD473F06}']
    function get_ParentForTransform: IVisual; safecall;
    procedure put_ParentForTransform(value: IVisual); safecall;
    function get_RelativeOffsetAdjustment: Numerics_Vector3; safecall;
    procedure put_RelativeOffsetAdjustment(value: Numerics_Vector3); safecall;
    function get_RelativeSizeAdjustment: Numerics_Vector2; safecall;
    procedure put_RelativeSizeAdjustment(value: Numerics_Vector2); safecall;
    property ParentForTransform: IVisual read get_ParentForTransform write put_ParentForTransform;
    property RelativeOffsetAdjustment: Numerics_Vector3 read get_RelativeOffsetAdjustment write put_RelativeOffsetAdjustment;
    property RelativeSizeAdjustment: Numerics_Vector2 read get_RelativeSizeAdjustment write put_RelativeSizeAdjustment;
  end;

  // Windows.UI.Composition.IVisual3
  IVisual3 = interface(IInspectable)
  ['{30BE580D-F4B6-4AB7-80DD-3738CBAC9F2C}']
    function get_IsHitTestVisible: Boolean; safecall;
    procedure put_IsHitTestVisible(value: Boolean); safecall;
    property IsHitTestVisible: Boolean read get_IsHitTestVisible write put_IsHitTestVisible;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.IVisualElement
  IVisualElement = interface(IInspectable)
  ['{01E64612-1D82-42F4-8E3F-A722DED33FC7}']
  end;

  // Windows.UI.Composition.IVisualFactory
  IVisualFactory = interface(IInspectable)
  ['{AD0FF93E-B502-4EB5-87B4-9A38A71D0137}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.ICompositionConditionalValue
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_CompositionConditionalValue)]
  Interactions_ICompositionConditionalValue = interface(IInspectable)
  ['{43250538-EB73-4561-A71D-1A43EAEB7A9B}']
    function get_Condition: IExpressionAnimation; safecall;
    procedure put_Condition(value: IExpressionAnimation); safecall;
    function get_Value: IExpressionAnimation; safecall;
    procedure put_Value(value: IExpressionAnimation); safecall;
    property Condition: IExpressionAnimation read get_Condition write put_Condition;
    property Value: IExpressionAnimation read get_Value write put_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.ICompositionConditionalValueStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_CompositionConditionalValue)]
  Interactions_ICompositionConditionalValueStatics = interface(IInspectable)
  ['{090C4B72-8467-4D0A-9065-AC46B80A5522}']
    function Create(compositor: ICompositor): Interactions_ICompositionConditionalValue; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.ICompositionInteractionSource
  Interactions_ICompositionInteractionSource = interface(IInspectable)
  ['{043B2431-06E3-495A-BA54-409F0017FAC0}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.ICompositionInteractionSourceCollection
  Interactions_ICompositionInteractionSourceCollection = interface(IInspectable)
  ['{1B468E4B-A5BF-47D8-A547-3894155A158C}']
    function get_Count: Integer; safecall;
    procedure Add(value: Interactions_ICompositionInteractionSource); safecall;
    procedure Remove(value: Interactions_ICompositionInteractionSource); safecall;
    procedure RemoveAll; safecall;
    property Count: Integer read get_Count;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionSourceConfiguration
  Interactions_IInteractionSourceConfiguration = interface(IInspectable)
  ['{A78347E5-A9D1-4D02-985E-B930CD0B9DA4}']
    function get_PositionXSourceMode: Interactions_InteractionSourceRedirectionMode; safecall;
    procedure put_PositionXSourceMode(value: Interactions_InteractionSourceRedirectionMode); safecall;
    function get_PositionYSourceMode: Interactions_InteractionSourceRedirectionMode; safecall;
    procedure put_PositionYSourceMode(value: Interactions_InteractionSourceRedirectionMode); safecall;
    function get_ScaleSourceMode: Interactions_InteractionSourceRedirectionMode; safecall;
    procedure put_ScaleSourceMode(value: Interactions_InteractionSourceRedirectionMode); safecall;
    property PositionXSourceMode: Interactions_InteractionSourceRedirectionMode read get_PositionXSourceMode write put_PositionXSourceMode;
    property PositionYSourceMode: Interactions_InteractionSourceRedirectionMode read get_PositionYSourceMode write put_PositionYSourceMode;
    property ScaleSourceMode: Interactions_InteractionSourceRedirectionMode read get_ScaleSourceMode write put_ScaleSourceMode;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerCustomAnimationStateEnteredArgs
  Interactions_IInteractionTrackerCustomAnimationStateEnteredArgs = interface(IInspectable)
  ['{8D1C8CF1-D7B0-434C-A5D2-2D7611864834}']
    function get_RequestId: Integer; safecall;
    property RequestId: Integer read get_RequestId;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerIdleStateEnteredArgs
  Interactions_IInteractionTrackerIdleStateEnteredArgs = interface(IInspectable)
  ['{50012FAA-1510-4142-A1A5-019B09F8857B}']
    function get_RequestId: Integer; safecall;
    property RequestId: Integer read get_RequestId;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaStateEnteredArgs
  Interactions_IInteractionTrackerInertiaStateEnteredArgs = interface(IInspectable)
  ['{87108CF2-E7FF-4F7D-9FFD-D72F1E409B63}']
    function get_ModifiedRestingPosition: IReference_1__Numerics_Vector3; safecall;
    function get_ModifiedRestingScale: IReference_1__Single; safecall;
    function get_NaturalRestingPosition: Numerics_Vector3; safecall;
    function get_NaturalRestingScale: Single; safecall;
    function get_PositionVelocityInPixelsPerSecond: Numerics_Vector3; safecall;
    function get_RequestId: Integer; safecall;
    function get_ScaleVelocityInPercentPerSecond: Single; safecall;
    property ModifiedRestingPosition: IReference_1__Numerics_Vector3 read get_ModifiedRestingPosition;
    property ModifiedRestingScale: IReference_1__Single read get_ModifiedRestingScale;
    property NaturalRestingPosition: Numerics_Vector3 read get_NaturalRestingPosition;
    property NaturalRestingScale: Single read get_NaturalRestingScale;
    property PositionVelocityInPixelsPerSecond: Numerics_Vector3 read get_PositionVelocityInPixelsPerSecond;
    property RequestId: Integer read get_RequestId;
    property ScaleVelocityInPercentPerSecond: Single read get_ScaleVelocityInPercentPerSecond;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInteractingStateEnteredArgs
  Interactions_IInteractionTrackerInteractingStateEnteredArgs = interface(IInspectable)
  ['{A7263939-A17B-4011-99FD-B5C24F143748}']
    function get_RequestId: Integer; safecall;
    property RequestId: Integer read get_RequestId;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerRequestIgnoredArgs
  Interactions_IInteractionTrackerRequestIgnoredArgs = interface(IInspectable)
  ['{80DD82F1-CE25-488F-91DD-CB6455CCFF2E}']
    function get_RequestId: Integer; safecall;
    property RequestId: Integer read get_RequestId;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerValuesChangedArgs
  Interactions_IInteractionTrackerValuesChangedArgs = interface(IInspectable)
  ['{CF1578EF-D3DF-4501-B9E6-F02FB22F73D0}']
    function get_Position: Numerics_Vector3; safecall;
    function get_RequestId: Integer; safecall;
    function get_Scale: Single; safecall;
    property Position: Numerics_Vector3 read get_Position;
    property RequestId: Integer read get_RequestId;
    property Scale: Single read get_Scale;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerOwner
  Interactions_IInteractionTrackerOwner = interface(IInspectable)
  ['{DB2E8AF3-4DEB-4E53-B29C-B06C9F96D651}']
    procedure CustomAnimationStateEntered(sender: Interactions_IInteractionTracker; args: Interactions_IInteractionTrackerCustomAnimationStateEnteredArgs); safecall;
    procedure IdleStateEntered(sender: Interactions_IInteractionTracker; args: Interactions_IInteractionTrackerIdleStateEnteredArgs); safecall;
    procedure InertiaStateEntered(sender: Interactions_IInteractionTracker; args: Interactions_IInteractionTrackerInertiaStateEnteredArgs); safecall;
    procedure InteractingStateEntered(sender: Interactions_IInteractionTracker; args: Interactions_IInteractionTrackerInteractingStateEnteredArgs); safecall;
    procedure RequestIgnored(sender: Interactions_IInteractionTracker; args: Interactions_IInteractionTrackerRequestIgnoredArgs); safecall;
    procedure ValuesChanged(sender: Interactions_IInteractionTracker; args: Interactions_IInteractionTrackerValuesChangedArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier
  Interactions_IInteractionTrackerInertiaModifier = interface(IInspectable)
  ['{A0E2C920-26B4-4DA2-8B61-5E683979BBE2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier>
  IIterator_1__Interactions_IInteractionTrackerInertiaModifier_Base = interface(IInspectable)
  ['{46617D87-2CD2-5E31-9A30-EA86F8AA7CA1}']
    function get_Current: Interactions_IInteractionTrackerInertiaModifier; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInteractions_IInteractionTrackerInertiaModifier): Cardinal; safecall;
    property Current: Interactions_IInteractionTrackerInertiaModifier read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier>
  IIterator_1__Interactions_IInteractionTrackerInertiaModifier = interface(IIterator_1__Interactions_IInteractionTrackerInertiaModifier_Base)
  ['{D26AD81E-4175-5D12-8048-42995F22AC85}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier>
  IIterable_1__Interactions_IInteractionTrackerInertiaModifier_Base = interface(IInspectable)
  ['{9A245C40-AAE6-59FB-87F5-4BB05599F0B1}']
    function First: IIterator_1__Interactions_IInteractionTrackerInertiaModifier; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifier>
  IIterable_1__Interactions_IInteractionTrackerInertiaModifier = interface(IIterable_1__Interactions_IInteractionTrackerInertiaModifier_Base)
  ['{0A807270-A641-50B9-A240-40FEDEC0FCFD}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTracker
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTracker)]
  Interactions_IInteractionTracker = interface(IInspectable)
  ['{2A8E8CB1-1000-4416-8363-CC27FB877308}']
    function get_InteractionSources: Interactions_ICompositionInteractionSourceCollection; safecall;
    function get_IsPositionRoundingSuggested: Boolean; safecall;
    function get_MaxPosition: Numerics_Vector3; safecall;
    procedure put_MaxPosition(value: Numerics_Vector3); safecall;
    function get_MaxScale: Single; safecall;
    procedure put_MaxScale(value: Single); safecall;
    function get_MinPosition: Numerics_Vector3; safecall;
    procedure put_MinPosition(value: Numerics_Vector3); safecall;
    function get_MinScale: Single; safecall;
    procedure put_MinScale(value: Single); safecall;
    function get_NaturalRestingPosition: Numerics_Vector3; safecall;
    function get_NaturalRestingScale: Single; safecall;
    function get_Owner: Interactions_IInteractionTrackerOwner; safecall;
    function get_Position: Numerics_Vector3; safecall;
    function get_PositionInertiaDecayRate: IReference_1__Numerics_Vector3; safecall;
    procedure put_PositionInertiaDecayRate(value: IReference_1__Numerics_Vector3); safecall;
    function get_PositionVelocityInPixelsPerSecond: Numerics_Vector3; safecall;
    function get_Scale: Single; safecall;
    function get_ScaleInertiaDecayRate: IReference_1__Single; safecall;
    procedure put_ScaleInertiaDecayRate(value: IReference_1__Single); safecall;
    function get_ScaleVelocityInPercentPerSecond: Single; safecall;
    procedure AdjustPositionXIfGreaterThanThreshold(adjustment: Single; positionThreshold: Single); safecall;
    procedure AdjustPositionYIfGreaterThanThreshold(adjustment: Single; positionThreshold: Single); safecall;
    procedure ConfigurePositionXInertiaModifiers(modifiers: IIterable_1__Interactions_IInteractionTrackerInertiaModifier); safecall;
    procedure ConfigurePositionYInertiaModifiers(modifiers: IIterable_1__Interactions_IInteractionTrackerInertiaModifier); safecall;
    procedure ConfigureScaleInertiaModifiers(modifiers: IIterable_1__Interactions_IInteractionTrackerInertiaModifier); safecall;
    function TryUpdatePosition(value: Numerics_Vector3): Integer; safecall;
    function TryUpdatePositionBy(amount: Numerics_Vector3): Integer; safecall;
    function TryUpdatePositionWithAnimation(animation: ICompositionAnimation): Integer; safecall;
    function TryUpdatePositionWithAdditionalVelocity(velocityInPixelsPerSecond: Numerics_Vector3): Integer; safecall;
    function TryUpdateScale(value: Single; centerPoint: Numerics_Vector3): Integer; safecall;
    function TryUpdateScaleWithAnimation(animation: ICompositionAnimation; centerPoint: Numerics_Vector3): Integer; safecall;
    function TryUpdateScaleWithAdditionalVelocity(velocityInPercentPerSecond: Single; centerPoint: Numerics_Vector3): Integer; safecall;
    property InteractionSources: Interactions_ICompositionInteractionSourceCollection read get_InteractionSources;
    property IsPositionRoundingSuggested: Boolean read get_IsPositionRoundingSuggested;
    property MaxPosition: Numerics_Vector3 read get_MaxPosition write put_MaxPosition;
    property MaxScale: Single read get_MaxScale write put_MaxScale;
    property MinPosition: Numerics_Vector3 read get_MinPosition write put_MinPosition;
    property MinScale: Single read get_MinScale write put_MinScale;
    property NaturalRestingPosition: Numerics_Vector3 read get_NaturalRestingPosition;
    property NaturalRestingScale: Single read get_NaturalRestingScale;
    property Owner: Interactions_IInteractionTrackerOwner read get_Owner;
    property Position: Numerics_Vector3 read get_Position;
    property PositionInertiaDecayRate: IReference_1__Numerics_Vector3 read get_PositionInertiaDecayRate write put_PositionInertiaDecayRate;
    property PositionVelocityInPixelsPerSecond: Numerics_Vector3 read get_PositionVelocityInPixelsPerSecond;
    property Scale: Single read get_Scale;
    property ScaleInertiaDecayRate: IReference_1__Single read get_ScaleInertiaDecayRate write put_ScaleInertiaDecayRate;
    property ScaleVelocityInPercentPerSecond: Single read get_ScaleVelocityInPercentPerSecond;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.ICompositionConditionalValue>
  IIterator_1__Interactions_ICompositionConditionalValue_Base = interface(IInspectable)
  ['{8A75B02D-3991-55A6-BFE2-82CB7DD65B98}']
    function get_Current: Interactions_ICompositionConditionalValue; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInteractions_ICompositionConditionalValue): Cardinal; safecall;
    property Current: Interactions_ICompositionConditionalValue read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.ICompositionConditionalValue>
  IIterator_1__Interactions_ICompositionConditionalValue = interface(IIterator_1__Interactions_ICompositionConditionalValue_Base)
  ['{0E0EA8B0-30B9-55DD-BF50-C971E1007661}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.ICompositionConditionalValue>
  IIterable_1__Interactions_ICompositionConditionalValue_Base = interface(IInspectable)
  ['{B268447B-F519-5CE5-89CD-B7E1BC5652EE}']
    function First: IIterator_1__Interactions_ICompositionConditionalValue; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.ICompositionConditionalValue>
  IIterable_1__Interactions_ICompositionConditionalValue = interface(IIterable_1__Interactions_ICompositionConditionalValue_Base)
  ['{E2082853-3D1B-5F73-BD55-E02BC606FA01}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTracker2
  Interactions_IInteractionTracker2 = interface(IInspectable)
  ['{25769A3E-CE6D-448C-8386-92620D240756}']
    procedure ConfigureCenterPointXInertiaModifiers(conditionalValues: IIterable_1__Interactions_ICompositionConditionalValue); safecall;
    procedure ConfigureCenterPointYInertiaModifiers(conditionalValues: IIterable_1__Interactions_ICompositionConditionalValue); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier
  Interactions_IInteractionTrackerVector2InertiaModifier = interface(IInspectable)
  ['{87E08AB0-3086-4853-A4B7-77882AD5D7E3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier>
  IIterator_1__Interactions_IInteractionTrackerVector2InertiaModifier_Base = interface(IInspectable)
  ['{7762CAAB-5B42-5958-9F49-06AEFD43AD75}']
    function get_Current: Interactions_IInteractionTrackerVector2InertiaModifier; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInteractions_IInteractionTrackerVector2InertiaModifier): Cardinal; safecall;
    property Current: Interactions_IInteractionTrackerVector2InertiaModifier read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier>
  IIterator_1__Interactions_IInteractionTrackerVector2InertiaModifier = interface(IIterator_1__Interactions_IInteractionTrackerVector2InertiaModifier_Base)
  ['{7F56D2B7-9A86-541E-97AC-ACD9A71C1220}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier>
  IIterable_1__Interactions_IInteractionTrackerVector2InertiaModifier_Base = interface(IInspectable)
  ['{3AEACFD8-C7F1-580C-A23B-99666E42E62B}']
    function First: IIterator_1__Interactions_IInteractionTrackerVector2InertiaModifier; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifier>
  IIterable_1__Interactions_IInteractionTrackerVector2InertiaModifier = interface(IIterable_1__Interactions_IInteractionTrackerVector2InertiaModifier_Base)
  ['{4C721963-D0EA-5416-A314-FE28F5061E50}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTracker3
  Interactions_IInteractionTracker3 = interface(IInspectable)
  ['{E6C5D7A2-5C4B-42C6-84B7-F69441B18091}']
    procedure ConfigureVector2PositionInertiaModifiers(modifiers: IIterable_1__Interactions_IInteractionTrackerVector2InertiaModifier); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTracker4
  Interactions_IInteractionTracker4 = interface(IInspectable)
  ['{EBD222BC-04AF-4AC7-847D-06EA36E80A16}']
    function TryUpdatePosition(value: Numerics_Vector3; option: Interactions_InteractionTrackerClampingOption): Integer; safecall;
    function TryUpdatePositionBy(amount: Numerics_Vector3; option: Interactions_InteractionTrackerClampingOption): Integer; safecall;
    function get_IsInertiaFromImpulse: Boolean; safecall;
    property IsInertiaFromImpulse: Boolean read get_IsInertiaFromImpulse;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTracker5
  Interactions_IInteractionTracker5 = interface(IInspectable)
  ['{D3EF5DA2-A254-40E4-88D5-44E4E16B5809}']
    function TryUpdatePosition(value: Numerics_Vector3; option: Interactions_InteractionTrackerClampingOption; posUpdateOption: Interactions_InteractionTrackerPositionUpdateOption): Integer; safecall;
  end;

  // Windows.UI.Composition.Interactions.IInteractionTrackerCustomAnimationStateEnteredArgs2
  Interactions_IInteractionTrackerCustomAnimationStateEnteredArgs2 = interface(IInspectable)
  ['{47D579B7-0985-5E99-B024-2F32C380C1A4}']
    function get_IsFromBinding: Boolean; safecall;
    property IsFromBinding: Boolean read get_IsFromBinding;
  end;

  // Windows.UI.Composition.Interactions.IInteractionTrackerIdleStateEnteredArgs2
  Interactions_IInteractionTrackerIdleStateEnteredArgs2 = interface(IInspectable)
  ['{F2E771ED-B803-5137-9435-1C96E48721E9}']
    function get_IsFromBinding: Boolean; safecall;
    property IsFromBinding: Boolean read get_IsFromBinding;
  end;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaModifierFactory
  Interactions_IInteractionTrackerInertiaModifierFactory = interface(IInspectable)
  ['{993818FE-C94E-4B86-87F3-922665BA46B9}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaMotion
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerInertiaMotion)]
  Interactions_IInteractionTrackerInertiaMotion = interface(IInspectable)
  ['{04922FDC-F154-4CB8-BF33-CC1BA611E6DB}']
    function get_Condition: IExpressionAnimation; safecall;
    procedure put_Condition(value: IExpressionAnimation); safecall;
    function get_Motion: IExpressionAnimation; safecall;
    procedure put_Motion(value: IExpressionAnimation); safecall;
    property Condition: IExpressionAnimation read get_Condition write put_Condition;
    property Motion: IExpressionAnimation read get_Motion write put_Motion;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaMotionStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerInertiaMotion)]
  Interactions_IInteractionTrackerInertiaMotionStatics = interface(IInspectable)
  ['{8CC83DD6-BA7B-431A-844B-6EAC9130F99A}']
    function Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaMotion; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaNaturalMotion
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerInertiaNaturalMotion)]
  Interactions_IInteractionTrackerInertiaNaturalMotion = interface(IInspectable)
  ['{70ACDAAE-27DC-48ED-A3C3-6D61C9A029D2}']
    function get_Condition: IExpressionAnimation; safecall;
    procedure put_Condition(value: IExpressionAnimation); safecall;
    function get_NaturalMotion: IScalarNaturalMotionAnimation; safecall;
    procedure put_NaturalMotion(value: IScalarNaturalMotionAnimation); safecall;
    property Condition: IExpressionAnimation read get_Condition write put_Condition;
    property NaturalMotion: IScalarNaturalMotionAnimation read get_NaturalMotion write put_NaturalMotion;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaNaturalMotionStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerInertiaNaturalMotion)]
  Interactions_IInteractionTrackerInertiaNaturalMotionStatics = interface(IInspectable)
  ['{CFDA55B0-5E3E-4289-932D-EE5F50E74283}']
    function Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaNaturalMotion; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaRestingValue
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerInertiaRestingValue)]
  Interactions_IInteractionTrackerInertiaRestingValue = interface(IInspectable)
  ['{86F7EC09-5096-4170-9CC8-DF2FE101BB93}']
    function get_Condition: IExpressionAnimation; safecall;
    procedure put_Condition(value: IExpressionAnimation); safecall;
    function get_RestingValue: IExpressionAnimation; safecall;
    procedure put_RestingValue(value: IExpressionAnimation); safecall;
    property Condition: IExpressionAnimation read get_Condition write put_Condition;
    property RestingValue: IExpressionAnimation read get_RestingValue write put_RestingValue;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaRestingValueStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerInertiaRestingValue)]
  Interactions_IInteractionTrackerInertiaRestingValueStatics = interface(IInspectable)
  ['{18ED4699-0745-4096-BCAB-3A4E99569BCF}']
    function Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaRestingValue; safecall;
  end;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaStateEnteredArgs2
  Interactions_IInteractionTrackerInertiaStateEnteredArgs2 = interface(IInspectable)
  ['{B1EB32F6-C26C-41F6-A189-FABC22B323CC}']
    function get_IsInertiaFromImpulse: Boolean; safecall;
    property IsInertiaFromImpulse: Boolean read get_IsInertiaFromImpulse;
  end;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInertiaStateEnteredArgs3
  Interactions_IInteractionTrackerInertiaStateEnteredArgs3 = interface(IInspectable)
  ['{48AC1C2F-47BD-59AF-A58C-79BD2EB9EF71}']
    function get_IsFromBinding: Boolean; safecall;
    property IsFromBinding: Boolean read get_IsFromBinding;
  end;

  // Windows.UI.Composition.Interactions.IInteractionTrackerInteractingStateEnteredArgs2
  Interactions_IInteractionTrackerInteractingStateEnteredArgs2 = interface(IInspectable)
  ['{509652D6-D488-59CD-819F-F52310295B11}']
    function get_IsFromBinding: Boolean; safecall;
    property IsFromBinding: Boolean read get_IsFromBinding;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTracker)]
  Interactions_IInteractionTrackerStatics = interface(IInspectable)
  ['{BBA5D7B7-6590-4498-8D6C-EB62B514C92A}']
    function Create(compositor: ICompositor): Interactions_IInteractionTracker; safecall;
    function CreateWithOwner(compositor: ICompositor; owner: Interactions_IInteractionTrackerOwner): Interactions_IInteractionTracker; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerStatics2
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTracker)]
  Interactions_IInteractionTrackerStatics2 = interface(IInspectable)
  ['{35E53720-46B7-5CB0-B505-F3D6884A6163}']
    procedure SetBindingMode(boundTracker1: Interactions_IInteractionTracker; boundTracker2: Interactions_IInteractionTracker; axisMode: Interactions_InteractionBindingAxisModes); safecall;
    function GetBindingMode(boundTracker1: Interactions_IInteractionTracker; boundTracker2: Interactions_IInteractionTracker): Interactions_InteractionBindingAxisModes; safecall;
  end;

  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaModifierFactory
  Interactions_IInteractionTrackerVector2InertiaModifierFactory = interface(IInspectable)
  ['{7401D6C4-6C6D-48DF-BC3E-171E227E7D7F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaNaturalMotion
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerVector2InertiaNaturalMotion)]
  Interactions_IInteractionTrackerVector2InertiaNaturalMotion = interface(IInspectable)
  ['{5F17695C-162D-4C07-9400-C282B28276CA}']
    function get_Condition: IExpressionAnimation; safecall;
    procedure put_Condition(value: IExpressionAnimation); safecall;
    function get_NaturalMotion: IVector2NaturalMotionAnimation; safecall;
    procedure put_NaturalMotion(value: IVector2NaturalMotionAnimation); safecall;
    property Condition: IExpressionAnimation read get_Condition write put_Condition;
    property NaturalMotion: IVector2NaturalMotionAnimation read get_NaturalMotion write put_NaturalMotion;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaNaturalMotionStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_InteractionTrackerVector2InertiaNaturalMotion)]
  Interactions_IInteractionTrackerVector2InertiaNaturalMotionStatics = interface(IInspectable)
  ['{82001A48-09C0-434F-8189-141C66DF362F}']
    function Create(compositor: ICompositor): Interactions_IInteractionTrackerVector2InertiaNaturalMotion; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IVisualInteractionSource
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_VisualInteractionSource)]
  Interactions_IVisualInteractionSource = interface(IInspectable)
  ['{CA0E8A86-D8D6-4111-B088-70347BD2B0ED}']
    function get_IsPositionXRailsEnabled: Boolean; safecall;
    procedure put_IsPositionXRailsEnabled(value: Boolean); safecall;
    function get_IsPositionYRailsEnabled: Boolean; safecall;
    procedure put_IsPositionYRailsEnabled(value: Boolean); safecall;
    function get_ManipulationRedirectionMode: Interactions_VisualInteractionSourceRedirectionMode; safecall;
    procedure put_ManipulationRedirectionMode(value: Interactions_VisualInteractionSourceRedirectionMode); safecall;
    function get_PositionXChainingMode: Interactions_InteractionChainingMode; safecall;
    procedure put_PositionXChainingMode(value: Interactions_InteractionChainingMode); safecall;
    function get_PositionXSourceMode: Interactions_InteractionSourceMode; safecall;
    procedure put_PositionXSourceMode(value: Interactions_InteractionSourceMode); safecall;
    function get_PositionYChainingMode: Interactions_InteractionChainingMode; safecall;
    procedure put_PositionYChainingMode(value: Interactions_InteractionChainingMode); safecall;
    function get_PositionYSourceMode: Interactions_InteractionSourceMode; safecall;
    procedure put_PositionYSourceMode(value: Interactions_InteractionSourceMode); safecall;
    function get_ScaleChainingMode: Interactions_InteractionChainingMode; safecall;
    procedure put_ScaleChainingMode(value: Interactions_InteractionChainingMode); safecall;
    function get_ScaleSourceMode: Interactions_InteractionSourceMode; safecall;
    procedure put_ScaleSourceMode(value: Interactions_InteractionSourceMode); safecall;
    function get_Source: IVisual; safecall;
    procedure TryRedirectForManipulation(pointerPoint: IPointerPoint); safecall;
    property IsPositionXRailsEnabled: Boolean read get_IsPositionXRailsEnabled write put_IsPositionXRailsEnabled;
    property IsPositionYRailsEnabled: Boolean read get_IsPositionYRailsEnabled write put_IsPositionYRailsEnabled;
    property ManipulationRedirectionMode: Interactions_VisualInteractionSourceRedirectionMode read get_ManipulationRedirectionMode write put_ManipulationRedirectionMode;
    property PositionXChainingMode: Interactions_InteractionChainingMode read get_PositionXChainingMode write put_PositionXChainingMode;
    property PositionXSourceMode: Interactions_InteractionSourceMode read get_PositionXSourceMode write put_PositionXSourceMode;
    property PositionYChainingMode: Interactions_InteractionChainingMode read get_PositionYChainingMode write put_PositionYChainingMode;
    property PositionYSourceMode: Interactions_InteractionSourceMode read get_PositionYSourceMode write put_PositionYSourceMode;
    property ScaleChainingMode: Interactions_InteractionChainingMode read get_ScaleChainingMode write put_ScaleChainingMode;
    property ScaleSourceMode: Interactions_InteractionSourceMode read get_ScaleSourceMode write put_ScaleSourceMode;
    property Source: IVisual read get_Source;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IVisualInteractionSource2
  Interactions_IVisualInteractionSource2 = interface(IInspectable)
  ['{AA914893-A73C-414D-80D0-249BAD2FBD93}']
    function get_DeltaPosition: Numerics_Vector3; safecall;
    function get_DeltaScale: Single; safecall;
    function get_Position: Numerics_Vector3; safecall;
    function get_PositionVelocity: Numerics_Vector3; safecall;
    function get_Scale: Single; safecall;
    function get_ScaleVelocity: Single; safecall;
    procedure ConfigureCenterPointXModifiers(conditionalValues: IIterable_1__Interactions_ICompositionConditionalValue); safecall;
    procedure ConfigureCenterPointYModifiers(conditionalValues: IIterable_1__Interactions_ICompositionConditionalValue); safecall;
    procedure ConfigureDeltaPositionXModifiers(conditionalValues: IIterable_1__Interactions_ICompositionConditionalValue); safecall;
    procedure ConfigureDeltaPositionYModifiers(conditionalValues: IIterable_1__Interactions_ICompositionConditionalValue); safecall;
    procedure ConfigureDeltaScaleModifiers(conditionalValues: IIterable_1__Interactions_ICompositionConditionalValue); safecall;
    property DeltaPosition: Numerics_Vector3 read get_DeltaPosition;
    property DeltaScale: Single read get_DeltaScale;
    property Position: Numerics_Vector3 read get_Position;
    property PositionVelocity: Numerics_Vector3 read get_PositionVelocity;
    property Scale: Single read get_Scale;
    property ScaleVelocity: Single read get_ScaleVelocity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IVisualInteractionSource3
  Interactions_IVisualInteractionSource3 = interface(IInspectable)
  ['{D941EF2A-0D5C-4057-92D7-C9711533204F}']
    function get_PointerWheelConfig: Interactions_IInteractionSourceConfiguration; safecall;
    property PointerWheelConfig: Interactions_IInteractionSourceConfiguration read get_PointerWheelConfig;
  end;

  // Windows.UI.Composition.Interactions.IVisualInteractionSourceObjectFactory
  Interactions_IVisualInteractionSourceObjectFactory = interface(IInspectable)
  ['{B2CA917C-E98A-41F2-B3C9-891C9266C8F6}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IVisualInteractionSourceStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_VisualInteractionSource)]
  Interactions_IVisualInteractionSourceStatics = interface(IInspectable)
  ['{369965E1-8645-4F75-BA00-6479CD10C8E6}']
    function Create(source: IVisual): Interactions_IVisualInteractionSource; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Interactions.IVisualInteractionSourceStatics2
  [WinRTClassNameAttribute(SWindows_UI_Composition_Interactions_VisualInteractionSource)]
  Interactions_IVisualInteractionSourceStatics2 = interface(IInspectable)
  ['{A979C032-5764-55E0-BC1F-0778786DCFDE}']
    function CreateFromIVisualElement(source: IVisualElement): Interactions_IVisualInteractionSource; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneBoundingBox
  Scenes_ISceneBoundingBox = interface(IInspectable)
  ['{5D8FFC70-C618-4083-8251-9962593114AA}']
    function get_Center: Numerics_Vector3; safecall;
    function get_Extents: Numerics_Vector3; safecall;
    function get_Max: Numerics_Vector3; safecall;
    function get_Min: Numerics_Vector3; safecall;
    function get_Size: Numerics_Vector3; safecall;
    property Center: Numerics_Vector3 read get_Center;
    property Extents: Numerics_Vector3 read get_Extents;
    property Max: Numerics_Vector3 read get_Max;
    property Min: Numerics_Vector3 read get_Min;
    property Size: Numerics_Vector3 read get_Size;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneComponent
  Scenes_ISceneComponent = interface(IInspectable)
  ['{AE20FC96-226C-44BD-95CB-DD5ED9EBE9A5}']
    function get_ComponentType: Scenes_SceneComponentType; safecall;
    property ComponentType: Scenes_SceneComponentType read get_ComponentType;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneComponentCollection
  Scenes_ISceneComponentCollection = interface(IInspectable)
  ['{C483791C-5F46-45E4-B666-A3D2259F9B2E}']
  end;

  // Windows.UI.Composition.Scenes.ISceneComponentFactory
  Scenes_ISceneComponentFactory = interface(IInspectable)
  ['{5FBC5574-DDD8-5889-AB5B-D8FA716E7C9E}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMaterial
  Scenes_ISceneMaterial = interface(IInspectable)
  ['{8CA74B7C-30DF-4E07-9490-37875AF1A123}']
  end;

  // Windows.UI.Composition.Scenes.ISceneMaterialFactory
  Scenes_ISceneMaterialFactory = interface(IInspectable)
  ['{67536C19-A707-5254-A495-7FDC799893B9}']
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMaterialInput
  Scenes_ISceneMaterialInput = interface(IInspectable)
  ['{422A1642-1EF1-485C-97E9-AE6F95AD812F}']
  end;

  // Windows.UI.Composition.Scenes.ISceneMaterialInputFactory
  Scenes_ISceneMaterialInputFactory = interface(IInspectable)
  ['{A88FEB74-7D0A-5E4C-A748-1015AF9CA74F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMesh
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneMesh)]
  Scenes_ISceneMesh = interface(IInspectable)
  ['{EE9A1530-1155-4C0C-92BD-40020CF78347}']
    function get_Bounds: Scenes_ISceneBoundingBox; safecall;
    function get_PrimitiveTopology: DirectX_DirectXPrimitiveTopology; safecall;
    procedure put_PrimitiveTopology(value: DirectX_DirectXPrimitiveTopology); safecall;
    procedure FillMeshAttribute(semantic: Scenes_SceneAttributeSemantic; format: DirectX_DirectXPixelFormat; memory: IMemoryBuffer); safecall;
    property Bounds: Scenes_ISceneBoundingBox read get_Bounds;
    property PrimitiveTopology: DirectX_DirectXPrimitiveTopology read get_PrimitiveTopology write put_PrimitiveTopology;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMeshMaterialAttributeMap
  Scenes_ISceneMeshMaterialAttributeMap = interface(IInspectable)
  ['{CE843171-3D43-4855-AA69-31FF988D049D}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMeshRendererComponent
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneMeshRendererComponent)]
  Scenes_ISceneMeshRendererComponent = interface(IInspectable)
  ['{9929F7E3-6364-477E-98FE-74ED9FD4C2DE}']
    function get_Material: Scenes_ISceneMaterial; safecall;
    procedure put_Material(value: Scenes_ISceneMaterial); safecall;
    function get_Mesh: Scenes_ISceneMesh; safecall;
    procedure put_Mesh(value: Scenes_ISceneMesh); safecall;
    function get_UVMappings: Scenes_ISceneMeshMaterialAttributeMap; safecall;
    property Material: Scenes_ISceneMaterial read get_Material write put_Material;
    property Mesh: Scenes_ISceneMesh read get_Mesh write put_Mesh;
    property UVMappings: Scenes_ISceneMeshMaterialAttributeMap read get_UVMappings;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMeshRendererComponentStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneMeshRendererComponent)]
  Scenes_ISceneMeshRendererComponentStatics = interface(IInspectable)
  ['{4954F37A-4459-4521-BD6E-2B38B8D711EA}']
    function Create(compositor: ICompositor): Scenes_ISceneMeshRendererComponent; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMeshStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneMesh)]
  Scenes_ISceneMeshStatics = interface(IInspectable)
  ['{8412316C-7B57-473F-966B-81DC277B1751}']
    function Create(compositor: ICompositor): Scenes_ISceneMesh; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMetallicRoughnessMaterial
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneMetallicRoughnessMaterial)]
  Scenes_ISceneMetallicRoughnessMaterial = interface(IInspectable)
  ['{C1D91446-799C-429E-A4E4-5DA645F18E61}']
    function get_BaseColorInput: Scenes_ISceneMaterialInput; safecall;
    procedure put_BaseColorInput(value: Scenes_ISceneMaterialInput); safecall;
    function get_BaseColorFactor: Numerics_Vector4; safecall;
    procedure put_BaseColorFactor(value: Numerics_Vector4); safecall;
    function get_MetallicFactor: Single; safecall;
    procedure put_MetallicFactor(value: Single); safecall;
    function get_MetallicRoughnessInput: Scenes_ISceneMaterialInput; safecall;
    procedure put_MetallicRoughnessInput(value: Scenes_ISceneMaterialInput); safecall;
    function get_RoughnessFactor: Single; safecall;
    procedure put_RoughnessFactor(value: Single); safecall;
    property BaseColorFactor: Numerics_Vector4 read get_BaseColorFactor write put_BaseColorFactor;
    property BaseColorInput: Scenes_ISceneMaterialInput read get_BaseColorInput write put_BaseColorInput;
    property MetallicFactor: Single read get_MetallicFactor write put_MetallicFactor;
    property MetallicRoughnessInput: Scenes_ISceneMaterialInput read get_MetallicRoughnessInput write put_MetallicRoughnessInput;
    property RoughnessFactor: Single read get_RoughnessFactor write put_RoughnessFactor;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneMetallicRoughnessMaterialStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneMetallicRoughnessMaterial)]
  Scenes_ISceneMetallicRoughnessMaterialStatics = interface(IInspectable)
  ['{3BDDCA50-6D9D-4531-8DC4-B27E3E49B7AB}']
    function Create(compositor: ICompositor): Scenes_ISceneMetallicRoughnessMaterial; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneModelTransform
  Scenes_ISceneModelTransform = interface(IInspectable)
  ['{C05576C2-32B1-4269-980D-B98537100AE4}']
    function get_Orientation: Numerics_Quaternion; safecall;
    procedure put_Orientation(value: Numerics_Quaternion); safecall;
    function get_RotationAngle: Single; safecall;
    procedure put_RotationAngle(value: Single); safecall;
    function get_RotationAngleInDegrees: Single; safecall;
    procedure put_RotationAngleInDegrees(value: Single); safecall;
    function get_RotationAxis: Numerics_Vector3; safecall;
    procedure put_RotationAxis(value: Numerics_Vector3); safecall;
    function get_Scale: Numerics_Vector3; safecall;
    procedure put_Scale(value: Numerics_Vector3); safecall;
    function get_Translation: Numerics_Vector3; safecall;
    procedure put_Translation(value: Numerics_Vector3); safecall;
    property Orientation: Numerics_Quaternion read get_Orientation write put_Orientation;
    property RotationAngle: Single read get_RotationAngle write put_RotationAngle;
    property RotationAngleInDegrees: Single read get_RotationAngleInDegrees write put_RotationAngleInDegrees;
    property RotationAxis: Numerics_Vector3 read get_RotationAxis write put_RotationAxis;
    property Scale: Numerics_Vector3 read get_Scale write put_Scale;
    property Translation: Numerics_Vector3 read get_Translation write put_Translation;
  end;

  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneNodeCollection
  Scenes_ISceneNodeCollection = interface(IInspectable)
  ['{29ADA101-2DD9-4332-BE63-60D2CF4269F2}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneNode
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneNode)]
  Scenes_ISceneNode = interface(IInspectable)
  ['{ACF2C247-F307-4581-9C41-AF2E29C3B016}']
    function get_Children: Scenes_ISceneNodeCollection; safecall;
    function get_Components: Scenes_ISceneComponentCollection; safecall;
    function get_Parent: Scenes_ISceneNode; safecall;
    function get_Transform: Scenes_ISceneModelTransform; safecall;
    function FindFirstComponentOfType(value: Scenes_SceneComponentType): Scenes_ISceneComponent; safecall;
    property Children: Scenes_ISceneNodeCollection read get_Children;
    property Components: Scenes_ISceneComponentCollection read get_Components;
    property Parent: Scenes_ISceneNode read get_Parent;
    property Transform: Scenes_ISceneModelTransform read get_Transform;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneNodeStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneNode)]
  Scenes_ISceneNodeStatics = interface(IInspectable)
  ['{579A0FAA-BE9D-4210-908C-93D15FEED0B7}']
    function Create(compositor: ICompositor): Scenes_ISceneNode; safecall;
  end;

  // Windows.UI.Composition.Scenes.ISceneObject
  Scenes_ISceneObject = interface(IInspectable)
  ['{1E94249B-0F1B-49EB-A819-877D8450005B}']
  end;

  // Windows.UI.Composition.Scenes.ISceneObjectFactory
  Scenes_ISceneObjectFactory = interface(IInspectable)
  ['{14FE799A-33E4-52EF-956C-44229D21F2C1}']
  end;

  // Windows.UI.Composition.Scenes.IScenePbrMaterial
  Scenes_IScenePbrMaterial = interface(IInspectable)
  ['{AAB6EBBE-D680-46DF-8294-B6800A9F95E7}']
    function get_AlphaCutoff: Single; safecall;
    procedure put_AlphaCutoff(value: Single); safecall;
    function get_AlphaMode: Scenes_SceneAlphaMode; safecall;
    procedure put_AlphaMode(value: Scenes_SceneAlphaMode); safecall;
    function get_EmissiveInput: Scenes_ISceneMaterialInput; safecall;
    procedure put_EmissiveInput(value: Scenes_ISceneMaterialInput); safecall;
    function get_EmissiveFactor: Numerics_Vector3; safecall;
    procedure put_EmissiveFactor(value: Numerics_Vector3); safecall;
    function get_IsDoubleSided: Boolean; safecall;
    procedure put_IsDoubleSided(value: Boolean); safecall;
    function get_NormalInput: Scenes_ISceneMaterialInput; safecall;
    procedure put_NormalInput(value: Scenes_ISceneMaterialInput); safecall;
    function get_NormalScale: Single; safecall;
    procedure put_NormalScale(value: Single); safecall;
    function get_OcclusionInput: Scenes_ISceneMaterialInput; safecall;
    procedure put_OcclusionInput(value: Scenes_ISceneMaterialInput); safecall;
    function get_OcclusionStrength: Single; safecall;
    procedure put_OcclusionStrength(value: Single); safecall;
    property AlphaCutoff: Single read get_AlphaCutoff write put_AlphaCutoff;
    property AlphaMode: Scenes_SceneAlphaMode read get_AlphaMode write put_AlphaMode;
    property EmissiveFactor: Numerics_Vector3 read get_EmissiveFactor write put_EmissiveFactor;
    property EmissiveInput: Scenes_ISceneMaterialInput read get_EmissiveInput write put_EmissiveInput;
    property IsDoubleSided: Boolean read get_IsDoubleSided write put_IsDoubleSided;
    property NormalInput: Scenes_ISceneMaterialInput read get_NormalInput write put_NormalInput;
    property NormalScale: Single read get_NormalScale write put_NormalScale;
    property OcclusionInput: Scenes_ISceneMaterialInput read get_OcclusionInput write put_OcclusionInput;
    property OcclusionStrength: Single read get_OcclusionStrength write put_OcclusionStrength;
  end;

  // Windows.UI.Composition.Scenes.IScenePbrMaterialFactory
  Scenes_IScenePbrMaterialFactory = interface(IInspectable)
  ['{2E3F3DFE-0B85-5727-B5BE-B7D3CBAC37FA}']
  end;

  // Windows.UI.Composition.Scenes.ISceneRendererComponent
  Scenes_ISceneRendererComponent = interface(IInspectable)
  ['{F1ACB857-CF4F-4025-9B25-A2D1944CF507}']
  end;

  // Windows.UI.Composition.Scenes.ISceneRendererComponentFactory
  Scenes_ISceneRendererComponentFactory = interface(IInspectable)
  ['{1DB6ED6C-AA2C-5967-9035-56352DC69658}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneSurfaceMaterialInput
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneSurfaceMaterialInput)]
  Scenes_ISceneSurfaceMaterialInput = interface(IInspectable)
  ['{9937DA5C-A9CA-4CFC-B3AA-088356518742}']
    function get_BitmapInterpolationMode: CompositionBitmapInterpolationMode; safecall;
    procedure put_BitmapInterpolationMode(value: CompositionBitmapInterpolationMode); safecall;
    function get_Surface: ICompositionSurface; safecall;
    procedure put_Surface(value: ICompositionSurface); safecall;
    function get_WrappingUMode: Scenes_SceneWrappingMode; safecall;
    procedure put_WrappingUMode(value: Scenes_SceneWrappingMode); safecall;
    function get_WrappingVMode: Scenes_SceneWrappingMode; safecall;
    procedure put_WrappingVMode(value: Scenes_SceneWrappingMode); safecall;
    property BitmapInterpolationMode: CompositionBitmapInterpolationMode read get_BitmapInterpolationMode write put_BitmapInterpolationMode;
    property Surface: ICompositionSurface read get_Surface write put_Surface;
    property WrappingUMode: Scenes_SceneWrappingMode read get_WrappingUMode write put_WrappingUMode;
    property WrappingVMode: Scenes_SceneWrappingMode read get_WrappingVMode write put_WrappingVMode;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneSurfaceMaterialInputStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneSurfaceMaterialInput)]
  Scenes_ISceneSurfaceMaterialInputStatics = interface(IInspectable)
  ['{5A2394D3-6429-4589-BBCF-B84F4F3CFBFE}']
    function Create(compositor: ICompositor): Scenes_ISceneSurfaceMaterialInput; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneVisual
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneVisual)]
  Scenes_ISceneVisual = interface(IInspectable)
  ['{8E672C1E-D734-47B1-BE14-3D694FFA4301}']
    function get_Root: Scenes_ISceneNode; safecall;
    procedure put_Root(value: Scenes_ISceneNode); safecall;
    property Root: Scenes_ISceneNode read get_Root write put_Root;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Composition.Scenes.ISceneVisualStatics
  [WinRTClassNameAttribute(SWindows_UI_Composition_Scenes_SceneVisual)]
  Scenes_ISceneVisualStatics = interface(IInspectable)
  ['{B8347E9A-50AA-4527-8D34-DE4CB8EA88B4}']
    function Create(compositor: ICompositor): Scenes_ISceneVisual; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Composition.IVisualElement>
  IIterator_1__IVisualElement = interface(IInspectable)
  ['{3142FDB3-4110-5819-B966-9C2A172E209F}']
    function get_Current: IVisualElement; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIVisualElement): Cardinal; safecall;
    property Current: IVisualElement read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Composition.IVisualElement>
  IIterable_1__IVisualElement = interface(IInspectable)
  ['{A0394077-1A66-589C-997D-2ACBA9051F77}']
    function First: IIterator_1__IVisualElement; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Composition.IVisualElement>
  IVectorView_1__IVisualElement = interface(IInspectable)
  ['{48E605A6-1FA6-5B4B-A802-17F54C4BCCCC}']
    function GetAt(index: Cardinal): IVisualElement; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IVisualElement; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIVisualElement): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Composition.IVisualElement>
  IVector_1__IVisualElement = interface(IInspectable)
  ['{CF59C70B-6D34-55FB-9313-9781433E778A}']
    function GetAt(index: Cardinal): IVisualElement; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IVisualElement; safecall;
    function IndexOf(value: IVisualElement; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IVisualElement); safecall;
    procedure InsertAt(index: Cardinal; value: IVisualElement); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IVisualElement); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIVisualElement): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIVisualElement); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Composition.CompositionObject
  // DualAPI
  // Implements: Windows.UI.Composition.ICompositionObject
  // Implements: Windows.UI.Composition.ICompositionObject2
  // Implements: Windows.UI.Composition.ICompositionObject3
  // Implements: Windows.UI.Composition.ICompositionObject4
  // Implements: Windows.Foundation.IClosable
  // Implements: Windows.UI.Composition.IAnimationObject
  // Statics: "Windows.UI.Composition.ICompositionObjectStatics"
  TCompositionObject = class(TWinRTGenericImportS<ICompositionObjectStatics>)
  public
    // -> ICompositionObjectStatics
    class procedure StartAnimationWithIAnimationObject(target: IAnimationObject; propertyName: HSTRING; animation: ICompositionAnimation); static; inline;
    class procedure StartAnimationGroupWithIAnimationObject(target: IAnimationObject; animation: ICompositionAnimationBase); static; inline;
  end;

  // Windows.UI.Composition.AnimationController
  // DualAPI
  // Implements: Windows.UI.Composition.IAnimationController
  // Statics: "Windows.UI.Composition.IAnimationControllerStatics"
  TAnimationController = class(TWinRTGenericImportS<IAnimationControllerStatics>)
  public
    // -> IAnimationControllerStatics
    class function get_MaxPlaybackRate: Single; static; inline;
    class function get_MinPlaybackRate: Single; static; inline;
    class property MaxPlaybackRate: Single read get_MaxPlaybackRate;
    class property MinPlaybackRate: Single read get_MinPlaybackRate;
  end;

  // Windows.UI.Composition.CompositionCapabilities
  // DualAPI
  // Implements: Windows.UI.Composition.ICompositionCapabilities
  // Statics: "Windows.UI.Composition.ICompositionCapabilitiesStatics"
  TCompositionCapabilities = class(TWinRTGenericImportS<ICompositionCapabilitiesStatics>)
  public
    // -> ICompositionCapabilitiesStatics
    class function GetForCurrentView: ICompositionCapabilities; static; inline;
  end;

  // Windows.UI.Composition.CompositionEffectSourceParameter
  // DualAPI
  // Implements: Windows.UI.Composition.ICompositionEffectSourceParameter
  // Implements: Windows.Graphics.Effects.IGraphicsEffectSource
  // Factory: "Windows.UI.Composition.ICompositionEffectSourceParameterFactory"
  TCompositionEffectSourceParameter = class(TWinRTGenericImportF<ICompositionEffectSourceParameterFactory>)
  public
    // -> ICompositionEffectSourceParameterFactory
    class function Create(name: HSTRING): ICompositionEffectSourceParameter; static; inline;
  end;

  // Windows.UI.Composition.CompositionPath
  // DualAPI
  // Implements: Windows.UI.Composition.ICompositionPath
  // Implements: Windows.Graphics.IGeometrySource2D
  // Factory: "Windows.UI.Composition.ICompositionPathFactory"
  TCompositionPath = class(TWinRTGenericImportF<ICompositionPathFactory>)
  public
    // -> ICompositionPathFactory
    class function Create(source: IGeometrySource2D): ICompositionPath; static; inline;
  end;

  // Windows.UI.Composition.Compositor
  // DualAPI
  // Implements: Windows.UI.Composition.ICompositor
  // Implements: Windows.UI.Composition.ICompositor2
  // Implements: Windows.UI.Composition.ICompositor3
  // Implements: Windows.UI.Composition.ICompositor4
  // Implements: Windows.UI.Composition.ICompositor5
  // Implements: Windows.UI.Composition.ICompositor6
  // Implements: Windows.UI.Composition.ICompositorWithProjectedShadow
  // Implements: Windows.UI.Composition.ICompositorWithRadialGradient
  // Implements: Windows.UI.Composition.ICompositorWithVisualSurface
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.UI.Composition.ICompositorStatics"
  // Instantiable: "ICompositor"
  // Interop Intf: "ICompositorInterop"
  ICompositorInterop = interface(IUnknown)
    ['{25297D5C-3AD4-4C9C-B5CF-E36A38512330}']
    function CreateCompositionSurfaceForHandle(swapChain: THandle): ICompositionSurface; safecall;
    function CreateCompositionSurfaceForSwapChain(const swapChain: IUnknown): ICompositionSurface; safecall;
    function CreateGraphicsDevice(const renderingDevice: IUnknown): ICompositionGraphicsDevice; safecall;
  end;
  TCompositor = class(TWinRTGenericImportSIO<ICompositorStatics, ICompositor, ICompositorInterop>)
  public
    // -> ICompositorStatics
    class function get_MaxGlobalPlaybackRate: Single; static; inline;
    class function get_MinGlobalPlaybackRate: Single; static; inline;
    class property MaxGlobalPlaybackRate: Single read get_MaxGlobalPlaybackRate;
    class property MinGlobalPlaybackRate: Single read get_MinGlobalPlaybackRate;
  end;

  // Windows.UI.Composition.Core.CompositorController
  // DualAPI
  // Implements: Windows.UI.Composition.Core.ICompositorController
  // Implements: Windows.Foundation.IClosable
  // Instantiable: "Core_ICompositorController"
  TCore_CompositorController = class(TWinRTGenericImportI<Core_ICompositorController>) end;

  // Windows.UI.Composition.Diagnostics.CompositionDebugSettings
  // DualAPI
  // Implements: Windows.UI.Composition.Diagnostics.ICompositionDebugSettings
  // Statics: "Windows.UI.Composition.Diagnostics.ICompositionDebugSettingsStatics"
  TDiagnostics_CompositionDebugSettings = class(TWinRTGenericImportS<Diagnostics_ICompositionDebugSettingsStatics>)
  public
    // -> Diagnostics_ICompositionDebugSettingsStatics
    class function TryGetSettings(compositor: ICompositor): Diagnostics_ICompositionDebugSettings; static; inline;
  end;

  // Windows.UI.Composition.Effects.SceneLightingEffect
  // DualAPI
  // Implements: Windows.UI.Composition.Effects.ISceneLightingEffect
  // Implements: Windows.UI.Composition.Effects.ISceneLightingEffect2
  // Implements: Windows.Graphics.Effects.IGraphicsEffect
  // Implements: Windows.Graphics.Effects.IGraphicsEffectSource
  // Instantiable: "Effects_ISceneLightingEffect"
  TEffects_SceneLightingEffect = class(TWinRTGenericImportI<Effects_ISceneLightingEffect>) end;

  // Windows.UI.Composition.Interactions.CompositionConditionalValue
  // DualAPI
  // Implements: Windows.UI.Composition.Interactions.ICompositionConditionalValue
  // Statics: "Windows.UI.Composition.Interactions.ICompositionConditionalValueStatics"
  TInteractions_CompositionConditionalValue = class(TWinRTGenericImportS<Interactions_ICompositionConditionalValueStatics>)
  public
    // -> Interactions_ICompositionConditionalValueStatics
    class function Create(compositor: ICompositor): Interactions_ICompositionConditionalValue; static; inline;
  end;

  // Windows.UI.Composition.Interactions.InteractionTracker
  // DualAPI
  // Implements: Windows.UI.Composition.Interactions.IInteractionTracker
  // Implements: Windows.UI.Composition.Interactions.IInteractionTracker2
  // Implements: Windows.UI.Composition.Interactions.IInteractionTracker3
  // Implements: Windows.UI.Composition.Interactions.IInteractionTracker4
  // Implements: Windows.UI.Composition.Interactions.IInteractionTracker5
  // Statics: "Windows.UI.Composition.Interactions.IInteractionTrackerStatics"
  // Statics: "Windows.UI.Composition.Interactions.IInteractionTrackerStatics2"
  TInteractions_InteractionTracker = class(TWinRTGenericImportS2<Interactions_IInteractionTrackerStatics, Interactions_IInteractionTrackerStatics2>)
  public
    // -> Interactions_IInteractionTrackerStatics
    class function Create(compositor: ICompositor): Interactions_IInteractionTracker; static; inline;
    class function CreateWithOwner(compositor: ICompositor; owner: Interactions_IInteractionTrackerOwner): Interactions_IInteractionTracker; static; inline;

    // -> Interactions_IInteractionTrackerStatics2
    class procedure SetBindingMode(boundTracker1: Interactions_IInteractionTracker; boundTracker2: Interactions_IInteractionTracker; axisMode: Interactions_InteractionBindingAxisModes); static; inline;
    class function GetBindingMode(boundTracker1: Interactions_IInteractionTracker; boundTracker2: Interactions_IInteractionTracker): Interactions_InteractionBindingAxisModes; static; inline;
  end;

  // Windows.UI.Composition.Interactions.InteractionTrackerInertiaMotion
  // DualAPI
  // Implements: Windows.UI.Composition.Interactions.IInteractionTrackerInertiaMotion
  // Statics: "Windows.UI.Composition.Interactions.IInteractionTrackerInertiaMotionStatics"
  TInteractions_InteractionTrackerInertiaMotion = class(TWinRTGenericImportS<Interactions_IInteractionTrackerInertiaMotionStatics>)
  public
    // -> Interactions_IInteractionTrackerInertiaMotionStatics
    class function Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaMotion; static; inline;
  end;

  // Windows.UI.Composition.Interactions.InteractionTrackerInertiaNaturalMotion
  // DualAPI
  // Implements: Windows.UI.Composition.Interactions.IInteractionTrackerInertiaNaturalMotion
  // Statics: "Windows.UI.Composition.Interactions.IInteractionTrackerInertiaNaturalMotionStatics"
  TInteractions_InteractionTrackerInertiaNaturalMotion = class(TWinRTGenericImportS<Interactions_IInteractionTrackerInertiaNaturalMotionStatics>)
  public
    // -> Interactions_IInteractionTrackerInertiaNaturalMotionStatics
    class function Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaNaturalMotion; static; inline;
  end;

  // Windows.UI.Composition.Interactions.InteractionTrackerInertiaRestingValue
  // DualAPI
  // Implements: Windows.UI.Composition.Interactions.IInteractionTrackerInertiaRestingValue
  // Statics: "Windows.UI.Composition.Interactions.IInteractionTrackerInertiaRestingValueStatics"
  TInteractions_InteractionTrackerInertiaRestingValue = class(TWinRTGenericImportS<Interactions_IInteractionTrackerInertiaRestingValueStatics>)
  public
    // -> Interactions_IInteractionTrackerInertiaRestingValueStatics
    class function Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaRestingValue; static; inline;
  end;

  // Windows.UI.Composition.Interactions.InteractionTrackerVector2InertiaNaturalMotion
  // DualAPI
  // Implements: Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaNaturalMotion
  // Statics: "Windows.UI.Composition.Interactions.IInteractionTrackerVector2InertiaNaturalMotionStatics"
  TInteractions_InteractionTrackerVector2InertiaNaturalMotion = class(TWinRTGenericImportS<Interactions_IInteractionTrackerVector2InertiaNaturalMotionStatics>)
  public
    // -> Interactions_IInteractionTrackerVector2InertiaNaturalMotionStatics
    class function Create(compositor: ICompositor): Interactions_IInteractionTrackerVector2InertiaNaturalMotion; static; inline;
  end;

  // Windows.UI.Composition.Interactions.VisualInteractionSource
  // DualAPI
  // Implements: Windows.UI.Composition.Interactions.IVisualInteractionSource
  // Implements: Windows.UI.Composition.Interactions.IVisualInteractionSource2
  // Implements: Windows.UI.Composition.Interactions.IVisualInteractionSource3
  // Implements: Windows.UI.Composition.Interactions.ICompositionInteractionSource
  // Statics: "Windows.UI.Composition.Interactions.IVisualInteractionSourceStatics"
  // Statics: "Windows.UI.Composition.Interactions.IVisualInteractionSourceStatics2"
  TInteractions_VisualInteractionSource = class(TWinRTGenericImportS2<Interactions_IVisualInteractionSourceStatics, Interactions_IVisualInteractionSourceStatics2>)
  public
    // -> Interactions_IVisualInteractionSourceStatics
    class function Create(source: IVisual): Interactions_IVisualInteractionSource; static; inline;

    // -> Interactions_IVisualInteractionSourceStatics2
    class function CreateFromIVisualElement(source: IVisualElement): Interactions_IVisualInteractionSource; static; inline;
  end;

  // Windows.UI.Composition.Scenes.SceneMesh
  // DualAPI
  // Implements: Windows.UI.Composition.Scenes.ISceneMesh
  // Statics: "Windows.UI.Composition.Scenes.ISceneMeshStatics"
  TScenes_SceneMesh = class(TWinRTGenericImportS<Scenes_ISceneMeshStatics>)
  public
    // -> Scenes_ISceneMeshStatics
    class function Create(compositor: ICompositor): Scenes_ISceneMesh; static; inline;
  end;

  // Windows.UI.Composition.Scenes.SceneMeshRendererComponent
  // DualAPI
  // Implements: Windows.UI.Composition.Scenes.ISceneMeshRendererComponent
  // Statics: "Windows.UI.Composition.Scenes.ISceneMeshRendererComponentStatics"
  TScenes_SceneMeshRendererComponent = class(TWinRTGenericImportS<Scenes_ISceneMeshRendererComponentStatics>)
  public
    // -> Scenes_ISceneMeshRendererComponentStatics
    class function Create(compositor: ICompositor): Scenes_ISceneMeshRendererComponent; static; inline;
  end;

  // Windows.UI.Composition.Scenes.SceneMetallicRoughnessMaterial
  // DualAPI
  // Implements: Windows.UI.Composition.Scenes.ISceneMetallicRoughnessMaterial
  // Statics: "Windows.UI.Composition.Scenes.ISceneMetallicRoughnessMaterialStatics"
  TScenes_SceneMetallicRoughnessMaterial = class(TWinRTGenericImportS<Scenes_ISceneMetallicRoughnessMaterialStatics>)
  public
    // -> Scenes_ISceneMetallicRoughnessMaterialStatics
    class function Create(compositor: ICompositor): Scenes_ISceneMetallicRoughnessMaterial; static; inline;
  end;

  // Windows.UI.Composition.Scenes.SceneNode
  // DualAPI
  // Implements: Windows.UI.Composition.Scenes.ISceneNode
  // Statics: "Windows.UI.Composition.Scenes.ISceneNodeStatics"
  TScenes_SceneNode = class(TWinRTGenericImportS<Scenes_ISceneNodeStatics>)
  public
    // -> Scenes_ISceneNodeStatics
    class function Create(compositor: ICompositor): Scenes_ISceneNode; static; inline;
  end;

  // Windows.UI.Composition.Scenes.SceneSurfaceMaterialInput
  // DualAPI
  // Implements: Windows.UI.Composition.Scenes.ISceneSurfaceMaterialInput
  // Statics: "Windows.UI.Composition.Scenes.ISceneSurfaceMaterialInputStatics"
  TScenes_SceneSurfaceMaterialInput = class(TWinRTGenericImportS<Scenes_ISceneSurfaceMaterialInputStatics>)
  public
    // -> Scenes_ISceneSurfaceMaterialInputStatics
    class function Create(compositor: ICompositor): Scenes_ISceneSurfaceMaterialInput; static; inline;
  end;

  // Windows.UI.Composition.Scenes.SceneVisual
  // DualAPI
  // Implements: Windows.UI.Composition.Scenes.ISceneVisual
  // Statics: "Windows.UI.Composition.Scenes.ISceneVisualStatics"
  TScenes_SceneVisual = class(TWinRTGenericImportS<Scenes_ISceneVisualStatics>)
  public
    // -> Scenes_ISceneVisualStatics
    class function Create(compositor: ICompositor): Scenes_ISceneVisual; static; inline;
  end;

implementation

{ TCompositionObject }

class procedure TCompositionObject.StartAnimationWithIAnimationObject(target: IAnimationObject; propertyName: HSTRING; animation: ICompositionAnimation);
begin
  Statics.StartAnimationWithIAnimationObject(target, propertyName, animation);
end;

class procedure TCompositionObject.StartAnimationGroupWithIAnimationObject(target: IAnimationObject; animation: ICompositionAnimationBase);
begin
  Statics.StartAnimationGroupWithIAnimationObject(target, animation);
end;


{ TAnimationController }

class function TAnimationController.get_MaxPlaybackRate: Single;
begin
  Result := Statics.get_MaxPlaybackRate;
end;

class function TAnimationController.get_MinPlaybackRate: Single;
begin
  Result := Statics.get_MinPlaybackRate;
end;


{ TCompositionCapabilities }

class function TCompositionCapabilities.GetForCurrentView: ICompositionCapabilities;
begin
  Result := Statics.GetForCurrentView;
end;


{ TCompositionEffectSourceParameter }
// Factories for : "CompositionEffectSourceParameter"
// Factory: "Windows.UI.Composition.ICompositionEffectSourceParameterFactory"
// -> ICompositionEffectSourceParameterFactory

class function TCompositionEffectSourceParameter.Create(name: HSTRING): ICompositionEffectSourceParameter;
begin
  Result := Factory.Create(name);
end;


{ TCompositionPath }
// Factories for : "CompositionPath"
// Factory: "Windows.UI.Composition.ICompositionPathFactory"
// -> ICompositionPathFactory

class function TCompositionPath.Create(source: IGeometrySource2D): ICompositionPath;
begin
  Result := Factory.Create(source);
end;


{ TCompositor }

class function TCompositor.get_MaxGlobalPlaybackRate: Single;
begin
  Result := Statics.get_MaxGlobalPlaybackRate;
end;

class function TCompositor.get_MinGlobalPlaybackRate: Single;
begin
  Result := Statics.get_MinGlobalPlaybackRate;
end;


{ TCore_CompositorController }

{ TDiagnostics_CompositionDebugSettings }

class function TDiagnostics_CompositionDebugSettings.TryGetSettings(compositor: ICompositor): Diagnostics_ICompositionDebugSettings;
begin
  Result := Statics.TryGetSettings(compositor);
end;


{ TEffects_SceneLightingEffect }

{ TInteractions_CompositionConditionalValue }

class function TInteractions_CompositionConditionalValue.Create(compositor: ICompositor): Interactions_ICompositionConditionalValue;
begin
  Result := Statics.Create(compositor);
end;


{ TInteractions_InteractionTracker }

class function TInteractions_InteractionTracker.Create(compositor: ICompositor): Interactions_IInteractionTracker;
begin
  Result := Statics.Create(compositor);
end;

class function TInteractions_InteractionTracker.CreateWithOwner(compositor: ICompositor; owner: Interactions_IInteractionTrackerOwner): Interactions_IInteractionTracker;
begin
  Result := Statics.CreateWithOwner(compositor, owner);
end;


class procedure TInteractions_InteractionTracker.SetBindingMode(boundTracker1: Interactions_IInteractionTracker; boundTracker2: Interactions_IInteractionTracker; axisMode: Interactions_InteractionBindingAxisModes);
begin
  Statics2.SetBindingMode(boundTracker1, boundTracker2, axisMode);
end;

class function TInteractions_InteractionTracker.GetBindingMode(boundTracker1: Interactions_IInteractionTracker; boundTracker2: Interactions_IInteractionTracker): Interactions_InteractionBindingAxisModes;
begin
  Result := Statics2.GetBindingMode(boundTracker1, boundTracker2);
end;


{ TInteractions_InteractionTrackerInertiaMotion }

class function TInteractions_InteractionTrackerInertiaMotion.Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaMotion;
begin
  Result := Statics.Create(compositor);
end;


{ TInteractions_InteractionTrackerInertiaNaturalMotion }

class function TInteractions_InteractionTrackerInertiaNaturalMotion.Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaNaturalMotion;
begin
  Result := Statics.Create(compositor);
end;


{ TInteractions_InteractionTrackerInertiaRestingValue }

class function TInteractions_InteractionTrackerInertiaRestingValue.Create(compositor: ICompositor): Interactions_IInteractionTrackerInertiaRestingValue;
begin
  Result := Statics.Create(compositor);
end;


{ TInteractions_InteractionTrackerVector2InertiaNaturalMotion }

class function TInteractions_InteractionTrackerVector2InertiaNaturalMotion.Create(compositor: ICompositor): Interactions_IInteractionTrackerVector2InertiaNaturalMotion;
begin
  Result := Statics.Create(compositor);
end;


{ TInteractions_VisualInteractionSource }

class function TInteractions_VisualInteractionSource.Create(source: IVisual): Interactions_IVisualInteractionSource;
begin
  Result := Statics.Create(source);
end;


class function TInteractions_VisualInteractionSource.CreateFromIVisualElement(source: IVisualElement): Interactions_IVisualInteractionSource;
begin
  Result := Statics2.CreateFromIVisualElement(source);
end;


{ TScenes_SceneMesh }

class function TScenes_SceneMesh.Create(compositor: ICompositor): Scenes_ISceneMesh;
begin
  Result := Statics.Create(compositor);
end;


{ TScenes_SceneMeshRendererComponent }

class function TScenes_SceneMeshRendererComponent.Create(compositor: ICompositor): Scenes_ISceneMeshRendererComponent;
begin
  Result := Statics.Create(compositor);
end;


{ TScenes_SceneMetallicRoughnessMaterial }

class function TScenes_SceneMetallicRoughnessMaterial.Create(compositor: ICompositor): Scenes_ISceneMetallicRoughnessMaterial;
begin
  Result := Statics.Create(compositor);
end;


{ TScenes_SceneNode }

class function TScenes_SceneNode.Create(compositor: ICompositor): Scenes_ISceneNode;
begin
  Result := Statics.Create(compositor);
end;


{ TScenes_SceneSurfaceMaterialInput }

class function TScenes_SceneSurfaceMaterialInput.Create(compositor: ICompositor): Scenes_ISceneSurfaceMaterialInput;
begin
  Result := Statics.Create(compositor);
end;


{ TScenes_SceneVisual }

class function TScenes_SceneVisual.Create(compositor: ICompositor): Scenes_ISceneVisual;
begin
  Result := Statics.Create(compositor);
end;


end.
