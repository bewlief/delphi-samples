{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI.Input;

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
  Winapi.Perception, 
  Winapi.Foundation, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  HoldingState = Winapi.CommonTypes.HoldingState;
  PHoldingState = Winapi.CommonTypes.PHoldingState;
  IPointerPoint = Winapi.CommonTypes.IPointerPoint;
  PIPointerPoint = Winapi.CommonTypes.PIPointerPoint;
  IPointerPointProperties = Winapi.CommonTypes.IPointerPointProperties;
  PIPointerPointProperties = Winapi.CommonTypes.PIPointerPointProperties;
  IRadialController = Winapi.CommonTypes.IRadialController;
  PIRadialController = Winapi.CommonTypes.PIRadialController;
  IRadialControllerButtonClickedEventArgs = Winapi.CommonTypes.IRadialControllerButtonClickedEventArgs;
  PIRadialControllerButtonClickedEventArgs = Winapi.CommonTypes.PIRadialControllerButtonClickedEventArgs;
  IRadialControllerControlAcquiredEventArgs = Winapi.CommonTypes.IRadialControllerControlAcquiredEventArgs;
  PIRadialControllerControlAcquiredEventArgs = Winapi.CommonTypes.PIRadialControllerControlAcquiredEventArgs;
  IRadialControllerMenu = Winapi.CommonTypes.IRadialControllerMenu;
  PIRadialControllerMenu = Winapi.CommonTypes.PIRadialControllerMenu;
  IRadialControllerMenuItem = Winapi.CommonTypes.IRadialControllerMenuItem;
  PIRadialControllerMenuItem = Winapi.CommonTypes.PIRadialControllerMenuItem;
  IRadialControllerRotationChangedEventArgs = Winapi.CommonTypes.IRadialControllerRotationChangedEventArgs;
  PIRadialControllerRotationChangedEventArgs = Winapi.CommonTypes.PIRadialControllerRotationChangedEventArgs;
  IRadialControllerScreenContact = Winapi.CommonTypes.IRadialControllerScreenContact;
  PIRadialControllerScreenContact = Winapi.CommonTypes.PIRadialControllerScreenContact;
  IRadialControllerScreenContactContinuedEventArgs = Winapi.CommonTypes.IRadialControllerScreenContactContinuedEventArgs;
  PIRadialControllerScreenContactContinuedEventArgs = Winapi.CommonTypes.PIRadialControllerScreenContactContinuedEventArgs;
  IRadialControllerScreenContactStartedEventArgs = Winapi.CommonTypes.IRadialControllerScreenContactStartedEventArgs;
  PIRadialControllerScreenContactStartedEventArgs = Winapi.CommonTypes.PIRadialControllerScreenContactStartedEventArgs;
  IVector_1__IPointerPoint_Base = Winapi.CommonTypes.IVector_1__IPointerPoint_Base;
  IVector_1__IPointerPoint = Winapi.CommonTypes.IVector_1__IPointerPoint;
  PIVector_1__IPointerPoint = Winapi.CommonTypes.PIVector_1__IPointerPoint;
  IVector_1__IRadialControllerMenuItem_Base = Winapi.CommonTypes.IVector_1__IRadialControllerMenuItem_Base;
  IVector_1__IRadialControllerMenuItem = Winapi.CommonTypes.IVector_1__IRadialControllerMenuItem;
  PIVector_1__IRadialControllerMenuItem = Winapi.CommonTypes.PIVector_1__IRadialControllerMenuItem;
  IVectorView_1__IPointerPoint = Winapi.CommonTypes.IVectorView_1__IPointerPoint;
  PIVectorView_1__IPointerPoint = Winapi.CommonTypes.PIVectorView_1__IPointerPoint;
  IVectorView_1__IRadialControllerMenuItem = Winapi.CommonTypes.IVectorView_1__IRadialControllerMenuItem;
  PIVectorView_1__IRadialControllerMenuItem = Winapi.CommonTypes.PIVectorView_1__IRadialControllerMenuItem;
  ManipulationDelta = Winapi.CommonTypes.ManipulationDelta;
  PManipulationDelta = Winapi.CommonTypes.PManipulationDelta;
  ManipulationVelocities = Winapi.CommonTypes.ManipulationVelocities;
  PManipulationVelocities = Winapi.CommonTypes.PManipulationVelocities;
  PointerUpdateKind = Winapi.CommonTypes.PointerUpdateKind;
  PPointerUpdateKind = Winapi.CommonTypes.PPointerUpdateKind;
  TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs_Delegate_Base;
  TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs;
  PTypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IRadialController__IRadialControllerButtonClickedEventArgs;
  TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs_Delegate_Base;
  TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs;
  PTypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IRadialController__IRadialControllerControlAcquiredEventArgs;
  TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs_Delegate_Base;
  TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs;
  PTypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IRadialController__IRadialControllerRotationChangedEventArgs;
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs_Delegate_Base;
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs;
  PTypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IRadialController__IRadialControllerScreenContactContinuedEventArgs;
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs_Delegate_Base;
  TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs;
  PTypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IRadialController__IRadialControllerScreenContactStartedEventArgs;

  // Forward declarations for interfaces

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.IPointerPoint>
  IIterator_1__IPointerPoint = interface;
  PIIterator_1__IPointerPoint = ^IIterator_1__IPointerPoint;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.IPointerPoint>
  IIterable_1__IPointerPoint = interface;
  PIIterable_1__IPointerPoint = ^IIterable_1__IPointerPoint;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Input.GazeInputAccessStatus>
  AsyncOperationCompletedHandler_1__GazeInputAccessStatus = interface;
  PAsyncOperationCompletedHandler_1__GazeInputAccessStatus = ^AsyncOperationCompletedHandler_1__GazeInputAccessStatus;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Input.GazeInputAccessStatus>
  IAsyncOperation_1__GazeInputAccessStatus = interface;
  PIAsyncOperation_1__GazeInputAccessStatus = ^IAsyncOperation_1__GazeInputAccessStatus;

  // Windows.UI.Input.Spatial.ISpatialInteractionSource
  Spatial_ISpatialInteractionSource = interface;
  PSpatial_ISpatialInteractionSource = ^Spatial_ISpatialInteractionSource;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.IRadialControllerMenuItem>
  IIterator_1__IRadialControllerMenuItem = interface;
  PIIterator_1__IRadialControllerMenuItem = ^IIterator_1__IRadialControllerMenuItem;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.IRadialControllerMenuItem>
  IIterable_1__IRadialControllerMenuItem = interface;
  PIIterable_1__IRadialControllerMenuItem = ^IIterable_1__IRadialControllerMenuItem;

  // Windows.UI.Input.Core.IRadialControllerIndependentInputSource
  Core_IRadialControllerIndependentInputSource = interface;
  PCore_IRadialControllerIndependentInputSource = ^Core_IRadialControllerIndependentInputSource;

  // Windows.UI.Input.Core.IRadialControllerIndependentInputSource2
  Core_IRadialControllerIndependentInputSource2 = interface;
  PCore_IRadialControllerIndependentInputSource2 = ^Core_IRadialControllerIndependentInputSource2;

  // Windows.UI.Input.Core.IRadialControllerIndependentInputSourceStatics
  Core_IRadialControllerIndependentInputSourceStatics = interface;
  PCore_IRadialControllerIndependentInputSourceStatics = ^Core_IRadialControllerIndependentInputSourceStatics;

  // Windows.UI.Input.IAttachableInputObjectFactory
  IAttachableInputObjectFactory = interface;
  PIAttachableInputObjectFactory = ^IAttachableInputObjectFactory;

  // Windows.UI.Input.IPointerPointTransform
  IPointerPointTransform = interface;
  PIPointerPointTransform = ^IPointerPointTransform;

  // Windows.UI.Input.IRadialControllerButtonPressedEventArgs
  IRadialControllerButtonPressedEventArgs = interface;
  PIRadialControllerButtonPressedEventArgs = ^IRadialControllerButtonPressedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonPressedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonPressedEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerButtonPressedEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerButtonPressedEventArgs;

  // Windows.UI.Input.IRadialControllerButtonHoldingEventArgs
  IRadialControllerButtonHoldingEventArgs = interface;
  PIRadialControllerButtonHoldingEventArgs = ^IRadialControllerButtonHoldingEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonHoldingEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonHoldingEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerButtonHoldingEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerButtonHoldingEventArgs;

  // Windows.UI.Input.IRadialControllerButtonReleasedEventArgs
  IRadialControllerButtonReleasedEventArgs = interface;
  PIRadialControllerButtonReleasedEventArgs = ^IRadialControllerButtonReleasedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonReleasedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonReleasedEventArgs = interface;
  PTypedEventHandler_2__IRadialController__IRadialControllerButtonReleasedEventArgs = ^TypedEventHandler_2__IRadialController__IRadialControllerButtonReleasedEventArgs;

  // Windows.UI.Input.IRadialController2
  IRadialController2 = interface;
  PIRadialController2 = ^IRadialController2;

  // Windows.UI.Input.IRadialControllerButtonClickedEventArgs2
  IRadialControllerButtonClickedEventArgs2 = interface;
  PIRadialControllerButtonClickedEventArgs2 = ^IRadialControllerButtonClickedEventArgs2;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.RadialControllerSystemMenuItemKind>
  IIterator_1__RadialControllerSystemMenuItemKind = interface;
  PIIterator_1__RadialControllerSystemMenuItemKind = ^IIterator_1__RadialControllerSystemMenuItemKind;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.RadialControllerSystemMenuItemKind>
  IIterable_1__RadialControllerSystemMenuItemKind = interface;
  PIIterable_1__RadialControllerSystemMenuItemKind = ^IIterable_1__RadialControllerSystemMenuItemKind;

  // Windows.UI.Input.IRadialControllerConfiguration
  IRadialControllerConfiguration = interface;
  PIRadialControllerConfiguration = ^IRadialControllerConfiguration;

  // Windows.UI.Input.IRadialControllerConfiguration2
  IRadialControllerConfiguration2 = interface;
  PIRadialControllerConfiguration2 = ^IRadialControllerConfiguration2;

  // Windows.UI.Input.IRadialControllerConfigurationStatics
  IRadialControllerConfigurationStatics = interface;
  PIRadialControllerConfigurationStatics = ^IRadialControllerConfigurationStatics;

  // Windows.UI.Input.IRadialControllerConfigurationStatics2
  IRadialControllerConfigurationStatics2 = interface;
  PIRadialControllerConfigurationStatics2 = ^IRadialControllerConfigurationStatics2;

  // Windows.UI.Input.IRadialControllerControlAcquiredEventArgs2
  IRadialControllerControlAcquiredEventArgs2 = interface;
  PIRadialControllerControlAcquiredEventArgs2 = ^IRadialControllerControlAcquiredEventArgs2;

  // Windows.UI.Input.IRadialControllerMenuItemStatics
  IRadialControllerMenuItemStatics = interface;
  PIRadialControllerMenuItemStatics = ^IRadialControllerMenuItemStatics;

  // Windows.UI.Input.IRadialControllerMenuItemStatics2
  IRadialControllerMenuItemStatics2 = interface;
  PIRadialControllerMenuItemStatics2 = ^IRadialControllerMenuItemStatics2;

  // Windows.UI.Input.IRadialControllerRotationChangedEventArgs2
  IRadialControllerRotationChangedEventArgs2 = interface;
  PIRadialControllerRotationChangedEventArgs2 = ^IRadialControllerRotationChangedEventArgs2;

  // Windows.UI.Input.IRadialControllerScreenContactContinuedEventArgs2
  IRadialControllerScreenContactContinuedEventArgs2 = interface;
  PIRadialControllerScreenContactContinuedEventArgs2 = ^IRadialControllerScreenContactContinuedEventArgs2;

  // Windows.UI.Input.IRadialControllerScreenContactEndedEventArgs
  IRadialControllerScreenContactEndedEventArgs = interface;
  PIRadialControllerScreenContactEndedEventArgs = ^IRadialControllerScreenContactEndedEventArgs;

  // Windows.UI.Input.IRadialControllerScreenContactStartedEventArgs2
  IRadialControllerScreenContactStartedEventArgs2 = interface;
  PIRadialControllerScreenContactStartedEventArgs2 = ^IRadialControllerScreenContactStartedEventArgs2;

  // Windows.UI.Input.IRadialControllerStatics
  IRadialControllerStatics = interface;
  PIRadialControllerStatics = ^IRadialControllerStatics;

  // Windows.UI.Input.Spatial.ISpatialPointerPose
  Spatial_ISpatialPointerPose = interface;
  PSpatial_ISpatialPointerPose = ^Spatial_ISpatialPointerPose;

  // Windows.UI.Input.Spatial.ISpatialRecognitionStartedEventArgs
  Spatial_ISpatialRecognitionStartedEventArgs = interface;
  PSpatial_ISpatialRecognitionStartedEventArgs = ^Spatial_ISpatialRecognitionStartedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialRecognitionStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionStartedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionStartedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionStartedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialRecognitionEndedEventArgs
  Spatial_ISpatialRecognitionEndedEventArgs = interface;
  PSpatial_ISpatialRecognitionEndedEventArgs = ^Spatial_ISpatialRecognitionEndedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialRecognitionEndedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionEndedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionEndedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionEndedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialTappedEventArgs
  Spatial_ISpatialTappedEventArgs = interface;
  PSpatial_ISpatialTappedEventArgs = ^Spatial_ISpatialTappedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialTappedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialTappedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialTappedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialTappedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialHoldStartedEventArgs
  Spatial_ISpatialHoldStartedEventArgs = interface;
  PSpatial_ISpatialHoldStartedEventArgs = ^Spatial_ISpatialHoldStartedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldStartedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldStartedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldStartedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialHoldCompletedEventArgs
  Spatial_ISpatialHoldCompletedEventArgs = interface;
  PSpatial_ISpatialHoldCompletedEventArgs = ^Spatial_ISpatialHoldCompletedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCompletedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCompletedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCompletedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialHoldCanceledEventArgs
  Spatial_ISpatialHoldCanceledEventArgs = interface;
  PSpatial_ISpatialHoldCanceledEventArgs = ^Spatial_ISpatialHoldCanceledEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCanceledEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCanceledEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCanceledEventArgs;

  // Windows.UI.Input.Spatial.ISpatialManipulationStartedEventArgs
  Spatial_ISpatialManipulationStartedEventArgs = interface;
  PSpatial_ISpatialManipulationStartedEventArgs = ^Spatial_ISpatialManipulationStartedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationStartedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationStartedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationStartedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialManipulationDelta
  Spatial_ISpatialManipulationDelta = interface;
  PSpatial_ISpatialManipulationDelta = ^Spatial_ISpatialManipulationDelta;

  // Windows.UI.Input.Spatial.ISpatialManipulationUpdatedEventArgs
  Spatial_ISpatialManipulationUpdatedEventArgs = interface;
  PSpatial_ISpatialManipulationUpdatedEventArgs = ^Spatial_ISpatialManipulationUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationUpdatedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationUpdatedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationUpdatedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationUpdatedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialManipulationCompletedEventArgs
  Spatial_ISpatialManipulationCompletedEventArgs = interface;
  PSpatial_ISpatialManipulationCompletedEventArgs = ^Spatial_ISpatialManipulationCompletedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCompletedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCompletedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCompletedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialManipulationCanceledEventArgs
  Spatial_ISpatialManipulationCanceledEventArgs = interface;
  PSpatial_ISpatialManipulationCanceledEventArgs = ^Spatial_ISpatialManipulationCanceledEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCanceledEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCanceledEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCanceledEventArgs;

  // Windows.UI.Input.Spatial.ISpatialNavigationStartedEventArgs
  Spatial_ISpatialNavigationStartedEventArgs = interface;
  PSpatial_ISpatialNavigationStartedEventArgs = ^Spatial_ISpatialNavigationStartedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationStartedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationStartedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationStartedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialNavigationUpdatedEventArgs
  Spatial_ISpatialNavigationUpdatedEventArgs = interface;
  PSpatial_ISpatialNavigationUpdatedEventArgs = ^Spatial_ISpatialNavigationUpdatedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationUpdatedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationUpdatedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationUpdatedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationUpdatedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialNavigationCompletedEventArgs
  Spatial_ISpatialNavigationCompletedEventArgs = interface;
  PSpatial_ISpatialNavigationCompletedEventArgs = ^Spatial_ISpatialNavigationCompletedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCompletedEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCompletedEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCompletedEventArgs;

  // Windows.UI.Input.Spatial.ISpatialNavigationCanceledEventArgs
  Spatial_ISpatialNavigationCanceledEventArgs = interface;
  PSpatial_ISpatialNavigationCanceledEventArgs = ^Spatial_ISpatialNavigationCanceledEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCanceledEventArgs = interface;
  PTypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCanceledEventArgs = ^TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCanceledEventArgs;

  // Windows.UI.Input.Spatial.ISpatialInteractionSourceLocation
  Spatial_ISpatialInteractionSourceLocation = interface;
  PSpatial_ISpatialInteractionSourceLocation = ^Spatial_ISpatialInteractionSourceLocation;

  // Windows.UI.Input.Spatial.ISpatialInteractionSourceProperties
  Spatial_ISpatialInteractionSourceProperties = interface;
  PSpatial_ISpatialInteractionSourceProperties = ^Spatial_ISpatialInteractionSourceProperties;

  // Windows.UI.Input.Spatial.ISpatialInteractionSourceState
  Spatial_ISpatialInteractionSourceState = interface;
  PSpatial_ISpatialInteractionSourceState = ^Spatial_ISpatialInteractionSourceState;

  // Windows.UI.Input.Spatial.ISpatialInteraction
  Spatial_ISpatialInteraction = interface;
  PSpatial_ISpatialInteraction = ^Spatial_ISpatialInteraction;

  // Windows.UI.Input.Spatial.ISpatialGestureRecognizer
  Spatial_ISpatialGestureRecognizer = interface;
  PSpatial_ISpatialGestureRecognizer = ^Spatial_ISpatialGestureRecognizer;

  // Windows.UI.Input.Spatial.ISpatialGestureRecognizerFactory
  Spatial_ISpatialGestureRecognizerFactory = interface;
  PSpatial_ISpatialGestureRecognizerFactory = ^Spatial_ISpatialGestureRecognizerFactory;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IIterator_1__Spatial_ISpatialInteractionSourceState = interface;
  PIIterator_1__Spatial_ISpatialInteractionSourceState = ^IIterator_1__Spatial_ISpatialInteractionSourceState;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IIterable_1__Spatial_ISpatialInteractionSourceState = interface;
  PIIterable_1__Spatial_ISpatialInteractionSourceState = ^IIterable_1__Spatial_ISpatialInteractionSourceState;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IVectorView_1__Spatial_ISpatialInteractionSourceState = interface;
  PIVectorView_1__Spatial_ISpatialInteractionSourceState = ^IVectorView_1__Spatial_ISpatialInteractionSourceState;

  // Windows.UI.Input Enums

  // Windows.UI.Input.CrossSlidingState
  CrossSlidingState = (
    Started = 0,
    Dragging = 1,
    Selecting = 2,
    SelectSpeedBumping = 3,
    SpeedBumping = 4,
    Rearranging = 5,
    Completed = 6
  );
  PCrossSlidingState = ^CrossSlidingState;

  // Windows.UI.Input.DraggingState
  DraggingState = (
    Started = 0,
    Continuing = 1,
    Completed = 2
  );
  PDraggingState = ^DraggingState;

  // Windows.UI.Input.EdgeGestureKind
  EdgeGestureKind = (
    Touch = 0,
    Keyboard = 1,
    Mouse = 2
  );
  PEdgeGestureKind = ^EdgeGestureKind;

  // Windows.UI.Input.GazeInputAccessStatus
  GazeInputAccessStatus = (
    Unspecified = 0,
    Allowed = 1,
    DeniedByUser = 2,
    DeniedBySystem = 3
  );
  PGazeInputAccessStatus = ^GazeInputAccessStatus;

  // Windows.UI.Input.GestureSettings
  GestureSettings = (
    None = 0,
    Tap = 1,
    DoubleTap = 2,
    Hold = 4,
    HoldWithMouse = 8,
    RightTap = 16,
    Drag = 32,
    ManipulationTranslateX = 64,
    ManipulationTranslateY = 128,
    ManipulationTranslateRailsX = 256,
    ManipulationTranslateRailsY = 512,
    ManipulationRotate = 1024,
    ManipulationScale = 2048,
    ManipulationTranslateInertia = 4096,
    ManipulationRotateInertia = 8192,
    ManipulationScaleInertia = 16384,
    CrossSlide = 32768,
    ManipulationMultipleFingerPanning = 65536
  );
  PGestureSettings = ^GestureSettings;

  // Windows.UI.Input.InputActivationState
  InputActivationState = (
    None = 0,
    Deactivated = 1,
    ActivatedNotForeground = 2,
    ActivatedInForeground = 3
  );
  PInputActivationState = ^InputActivationState;

  // Windows.UI.Input.Preview.Injection.InjectedInputButtonChangeKind
  Preview_Injection_InjectedInputButtonChangeKind = (
    None = 0,
    FirstButtonDown = 1,
    FirstButtonUp = 2,
    SecondButtonDown = 3,
    SecondButtonUp = 4,
    ThirdButtonDown = 5,
    ThirdButtonUp = 6,
    FourthButtonDown = 7,
    FourthButtonUp = 8,
    FifthButtonDown = 9,
    FifthButtonUp = 10
  );
  PPreview_Injection_InjectedInputButtonChangeKind = ^Preview_Injection_InjectedInputButtonChangeKind;

  // Windows.UI.Input.Preview.Injection.InjectedInputKeyOptions
  Preview_Injection_InjectedInputKeyOptions = (
    None = 0,
    ExtendedKey = 1,
    KeyUp = 2,
    ScanCode = 8,
    Unicode = 4
  );
  PPreview_Injection_InjectedInputKeyOptions = ^Preview_Injection_InjectedInputKeyOptions;

  // Windows.UI.Input.Preview.Injection.InjectedInputMouseOptions
  Preview_Injection_InjectedInputMouseOptions = (
    None = 0,
    Move = 1,
    LeftDown = 2,
    LeftUp = 4,
    RightDown = 8,
    RightUp = 16,
    MiddleDown = 32,
    MiddleUp = 64,
    XDown = 128,
    XUp = 256,
    Wheel = 2048,
    HWheel = 4096,
    MoveNoCoalesce = 8192,
    VirtualDesk = 16384,
    Absolute = 32768
  );
  PPreview_Injection_InjectedInputMouseOptions = ^Preview_Injection_InjectedInputMouseOptions;

  // Windows.UI.Input.Preview.Injection.InjectedInputPenButtons
  Preview_Injection_InjectedInputPenButtons = (
    None = 0,
    Barrel = 1,
    Inverted = 2,
    Eraser = 4
  );
  PPreview_Injection_InjectedInputPenButtons = ^Preview_Injection_InjectedInputPenButtons;

  // Windows.UI.Input.Preview.Injection.InjectedInputPenParameters
  Preview_Injection_InjectedInputPenParameters = (
    None = 0,
    Pressure = 1,
    Rotation = 2,
    TiltX = 4,
    TiltY = 8
  );
  PPreview_Injection_InjectedInputPenParameters = ^Preview_Injection_InjectedInputPenParameters;

  // Windows.UI.Input.Preview.Injection.InjectedInputPointerOptions
  Preview_Injection_InjectedInputPointerOptions = (
    None = 0,
    New = 1,
    InRange = 2,
    InContact = 4,
    FirstButton = 16,
    SecondButton = 32,
    Primary = 8192,
    Confidence = 16384,
    Canceled = 32768,
    PointerDown = 65536,
    Update = 131072,
    PointerUp = 262144,
    CaptureChanged = 2097152
  );
  PPreview_Injection_InjectedInputPointerOptions = ^Preview_Injection_InjectedInputPointerOptions;

  // Windows.UI.Input.Preview.Injection.InjectedInputShortcut
  Preview_Injection_InjectedInputShortcut = (
    Back = 0,
    Start = 1,
    Search = 2
  );
  PPreview_Injection_InjectedInputShortcut = ^Preview_Injection_InjectedInputShortcut;

  // Windows.UI.Input.Preview.Injection.InjectedInputTouchParameters
  Preview_Injection_InjectedInputTouchParameters = (
    None = 0,
    Contact = 1,
    Orientation = 2,
    Pressure = 4
  );
  PPreview_Injection_InjectedInputTouchParameters = ^Preview_Injection_InjectedInputTouchParameters;

  // Windows.UI.Input.Preview.Injection.InjectedInputVisualizationMode
  Preview_Injection_InjectedInputVisualizationMode = (
    None = 0,
    Default = 1,
    Indirect = 2
  );
  PPreview_Injection_InjectedInputVisualizationMode = ^Preview_Injection_InjectedInputVisualizationMode;

  // Windows.UI.Input.RadialControllerMenuKnownIcon
  RadialControllerMenuKnownIcon = (
    Scroll = 0,
    Zoom = 1,
    UndoRedo = 2,
    Volume = 3,
    NextPreviousTrack = 4,
    Ruler = 5,
    InkColor = 6,
    InkThickness = 7,
    PenType = 8
  );
  PRadialControllerMenuKnownIcon = ^RadialControllerMenuKnownIcon;

  // Windows.UI.Input.RadialControllerSystemMenuItemKind
  RadialControllerSystemMenuItemKind = (
    Scroll = 0,
    Zoom = 1,
    UndoRedo = 2,
    Volume = 3,
    NextPreviousTrack = 4
  );
  PRadialControllerSystemMenuItemKind = ^RadialControllerSystemMenuItemKind;

  // Windows.UI.Input.Spatial.SpatialGestureSettings
  Spatial_SpatialGestureSettings = (
    None = 0,
    Tap = 1,
    DoubleTap = 2,
    Hold = 4,
    ManipulationTranslate = 8,
    NavigationX = 16,
    NavigationY = 32,
    NavigationZ = 64,
    NavigationRailsX = 128,
    NavigationRailsY = 256,
    NavigationRailsZ = 512
  );
  PSpatial_SpatialGestureSettings = ^Spatial_SpatialGestureSettings;

  // Windows.UI.Input.Spatial.SpatialInteractionPressKind
  Spatial_SpatialInteractionPressKind = (
    None = 0,
    Select = 1,
    Menu = 2,
    Grasp = 3,
    Touchpad = 4,
    Thumbstick = 5
  );
  PSpatial_SpatialInteractionPressKind = ^Spatial_SpatialInteractionPressKind;

  // Windows.UI.Input.Spatial.SpatialInteractionSourceHandedness
  Spatial_SpatialInteractionSourceHandedness = (
    Unspecified = 0,
    Left = 1,
    Right = 2
  );
  PSpatial_SpatialInteractionSourceHandedness = ^Spatial_SpatialInteractionSourceHandedness;

  // Windows.UI.Input.Spatial.SpatialInteractionSourceKind
  Spatial_SpatialInteractionSourceKind = (
    Other = 0,
    Hand = 1,
    Voice = 2,
    Controller = 3
  );
  PSpatial_SpatialInteractionSourceKind = ^Spatial_SpatialInteractionSourceKind;

  // Windows.UI.Input.Spatial.SpatialInteractionSourcePositionAccuracy
  Spatial_SpatialInteractionSourcePositionAccuracy = (
    High = 0,
    Approximate = 1
  );
  PSpatial_SpatialInteractionSourcePositionAccuracy = ^Spatial_SpatialInteractionSourcePositionAccuracy;

  // Windows.UI.Input Records
  // Windows.UI.Input.CrossSlideThresholds
  CrossSlideThresholds = record
    SelectionStart: Single;
    SpeedBumpStart: Single;
    SpeedBumpEnd: Single;
    RearrangeStart: Single;
  end;
  PCrossSlideThresholds = ^CrossSlideThresholds;

  // Windows.UI.Input.Preview.Injection.InjectedInputPoint
  Preview_Injection_InjectedInputPoint = record
    PositionX: Integer;
    PositionY: Integer;
  end;
  PPreview_Injection_InjectedInputPoint = ^Preview_Injection_InjectedInputPoint;

  // Windows.UI.Input.Preview.Injection.InjectedInputPointerInfo
  Preview_Injection_InjectedInputPointerInfo = record
    PointerId: Cardinal;
    PointerOptions: Preview_Injection_InjectedInputPointerOptions;
    PixelLocation: Preview_Injection_InjectedInputPoint;
    TimeOffsetInMilliseconds: Cardinal;
    PerformanceCount: UInt64;
  end;
  PPreview_Injection_InjectedInputPointerInfo = ^Preview_Injection_InjectedInputPointerInfo;

  // Windows.UI.Input.Preview.Injection.InjectedInputRectangle
  Preview_Injection_InjectedInputRectangle = record
    Left: Integer;
    Top: Integer;
    Bottom: Integer;
    Right: Integer;
  end;
  PPreview_Injection_InjectedInputRectangle = ^Preview_Injection_InjectedInputRectangle;

  // Windows.UI.Input Interfaces

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.IPointerPoint>
  IIterator_1__IPointerPoint_Base = interface(IInspectable)
  ['{721FE01C-5AD4-5262-B078-3AB345105DB8}']
    function get_Current: IPointerPoint; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIPointerPoint): Cardinal; safecall;
    property Current: IPointerPoint read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.IPointerPoint>
  IIterator_1__IPointerPoint = interface(IIterator_1__IPointerPoint_Base)
  ['{2C506FAB-54BD-5007-8847-46FB9494DFB3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.IPointerPoint>
  IIterable_1__IPointerPoint_Base = interface(IInspectable)
  ['{F6F2CBA6-7076-5B59-9631-F6AC32B57695}']
    function First: IIterator_1__IPointerPoint; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.IPointerPoint>
  IIterable_1__IPointerPoint = interface(IIterable_1__IPointerPoint_Base)
  ['{69A48EED-AF6C-5AE9-ACAD-A403EB13C0DA}']
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Input.GazeInputAccessStatus>
  AsyncOperationCompletedHandler_1__GazeInputAccessStatus = interface(IUnknown)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__GazeInputAccessStatus; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Input.GazeInputAccessStatus>
  IAsyncOperation_1__GazeInputAccessStatus = interface(IInspectable)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__GazeInputAccessStatus); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__GazeInputAccessStatus; safecall;
    function GetResults: GazeInputAccessStatus; safecall;
    property Completed: AsyncOperationCompletedHandler_1__GazeInputAccessStatus read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialInteractionSource
  Spatial_ISpatialInteractionSource = interface(IInspectable)
  ['{FB5433BA-B0B3-3148-9F3B-E9F5DE568F5D}']
    function get_Id: Cardinal; safecall;
    function get_Kind: Spatial_SpatialInteractionSourceKind; safecall;
    property Id: Cardinal read get_Id;
    property Kind: Spatial_SpatialInteractionSourceKind read get_Kind;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.IRadialControllerMenuItem>
  IIterator_1__IRadialControllerMenuItem_Base = interface(IInspectable)
  ['{5A773E24-D968-535E-969A-76CE3602A637}']
    function get_Current: IRadialControllerMenuItem; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIRadialControllerMenuItem): Cardinal; safecall;
    property Current: IRadialControllerMenuItem read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.IRadialControllerMenuItem>
  IIterator_1__IRadialControllerMenuItem = interface(IIterator_1__IRadialControllerMenuItem_Base)
  ['{1A32FA4D-B3ED-5B91-AC5D-F5CC1845E696}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.IRadialControllerMenuItem>
  IIterable_1__IRadialControllerMenuItem_Base = interface(IInspectable)
  ['{1AA752B3-DC11-5BCE-B2B9-CD1BF8F235BE}']
    function First: IIterator_1__IRadialControllerMenuItem; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.IRadialControllerMenuItem>
  IIterable_1__IRadialControllerMenuItem = interface(IIterable_1__IRadialControllerMenuItem_Base)
  ['{7C62A666-8F21-5875-AA98-951AC77DAEB3}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.Core.IRadialControllerIndependentInputSource
  [WinRTClassNameAttribute(SWindows_UI_Input_Core_RadialControllerIndependentInputSource)]
  Core_IRadialControllerIndependentInputSource = interface(IInspectable)
  ['{3D577EF6-4CEE-11E6-B535-001BDC06AB3B}']
    function get_Controller: IRadialController; safecall;
    function get_Dispatcher: ICoreDispatcher; safecall;
    property Controller: IRadialController read get_Controller;
    property Dispatcher: ICoreDispatcher read get_Dispatcher;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.Core.IRadialControllerIndependentInputSource2
  Core_IRadialControllerIndependentInputSource2 = interface(IInspectable)
  ['{7073AAD8-35F3-4EEB-8751-BE4D0A66FAF4}']
    function get_DispatcherQueue: IDispatcherQueue; safecall;
    property DispatcherQueue: IDispatcherQueue read get_DispatcherQueue;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.Core.IRadialControllerIndependentInputSourceStatics
  [WinRTClassNameAttribute(SWindows_UI_Input_Core_RadialControllerIndependentInputSource)]
  Core_IRadialControllerIndependentInputSourceStatics = interface(IInspectable)
  ['{3D577EF5-4CEE-11E6-B535-001BDC06AB3B}']
    function CreateForView(view: ICoreApplicationView): Core_IRadialControllerIndependentInputSource; safecall;
  end;

  // Windows.UI.Input.IAttachableInputObjectFactory
  IAttachableInputObjectFactory = interface(IInspectable)
  ['{A4C54C4E-42BC-58FA-A640-EA1516F4C06B}']
  end;

  // Windows.UI.Input.IPointerPointTransform
  IPointerPointTransform = interface(IInspectable)
  ['{4D5FE14F-B87C-4028-BC9C-59E9947FB056}']
    function get_Inverse: IPointerPointTransform; safecall;
    function TryTransform(inPoint: TPointF; out outPoint: TPointF): Boolean; safecall;
    function TransformBounds(rect: TRectF): TRectF; safecall;
    property Inverse: IPointerPointTransform read get_Inverse;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerButtonPressedEventArgs
  IRadialControllerButtonPressedEventArgs = interface(IInspectable)
  ['{3D577EED-4CEE-11E6-B535-001BDC06AB3B}']
    function get_Contact: IRadialControllerScreenContact; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonPressedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonPressedEventArgs_Delegate_Base = interface(IUnknown)
  ['{660BEE4A-4FED-5A62-AA5D-8113B477BC69}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerButtonPressedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonPressedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonPressedEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerButtonPressedEventArgs_Delegate_Base)
  ['{A84E58BE-27BD-5B94-BD1C-AC5319506931}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerButtonHoldingEventArgs
  IRadialControllerButtonHoldingEventArgs = interface(IInspectable)
  ['{3D577EEE-3CEE-11E6-B535-001BDC06AB3B}']
    function get_Contact: IRadialControllerScreenContact; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonHoldingEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonHoldingEventArgs_Delegate_Base = interface(IUnknown)
  ['{4C44F2FF-3A4A-51BA-A01F-9F2002471F59}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerButtonHoldingEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonHoldingEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonHoldingEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerButtonHoldingEventArgs_Delegate_Base)
  ['{1DD1E759-DDBD-5FCC-AE7B-9DCFE3B28F7B}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerButtonReleasedEventArgs
  IRadialControllerButtonReleasedEventArgs = interface(IInspectable)
  ['{3D577EEF-3CEE-11E6-B535-001BDC06AB3B}']
    function get_Contact: IRadialControllerScreenContact; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property Contact: IRadialControllerScreenContact read get_Contact;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonReleasedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonReleasedEventArgs_Delegate_Base = interface(IUnknown)
  ['{C22FF62C-C642-5D50-9340-FE163122720C}']
    procedure Invoke(sender: IRadialController; args: IRadialControllerButtonReleasedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.IRadialController,Windows.UI.Input.IRadialControllerButtonReleasedEventArgs>
  TypedEventHandler_2__IRadialController__IRadialControllerButtonReleasedEventArgs = interface(TypedEventHandler_2__IRadialController__IRadialControllerButtonReleasedEventArgs_Delegate_Base)
  ['{16547458-2C91-5F76-AB77-69CBECB5085F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialController2
  IRadialController2 = interface(IInspectable)
  ['{3D577EFF-4CEE-11E6-B535-001BDC06AB3B}']
    function add_ButtonPressed(handler: TypedEventHandler_2__IRadialController__IRadialControllerButtonPressedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ButtonPressed(token: EventRegistrationToken); safecall;
    function add_ButtonHolding(handler: TypedEventHandler_2__IRadialController__IRadialControllerButtonHoldingEventArgs): EventRegistrationToken; safecall;
    procedure remove_ButtonHolding(token: EventRegistrationToken); safecall;
    function add_ButtonReleased(handler: TypedEventHandler_2__IRadialController__IRadialControllerButtonReleasedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ButtonReleased(token: EventRegistrationToken); safecall;
  end;

  // Windows.UI.Input.IRadialControllerButtonClickedEventArgs2
  IRadialControllerButtonClickedEventArgs2 = interface(IInspectable)
  ['{3D577EF3-3CEE-11E6-B535-001BDC06AB3B}']
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.RadialControllerSystemMenuItemKind>
  IIterator_1__RadialControllerSystemMenuItemKind_Base = interface(IInspectable)
  ['{3FBC1858-A43E-54DC-B0E2-8B098BDDACF6}']
    function get_Current: RadialControllerSystemMenuItemKind; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PRadialControllerSystemMenuItemKind): Cardinal; safecall;
    property Current: RadialControllerSystemMenuItemKind read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.RadialControllerSystemMenuItemKind>
  IIterator_1__RadialControllerSystemMenuItemKind = interface(IIterator_1__RadialControllerSystemMenuItemKind_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.RadialControllerSystemMenuItemKind>
  IIterable_1__RadialControllerSystemMenuItemKind_Base = interface(IInspectable)
  ['{4516010F-FD98-5E1D-BF3F-AEAF79F1F3DA}']
    function First: IIterator_1__RadialControllerSystemMenuItemKind; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.RadialControllerSystemMenuItemKind>
  IIterable_1__RadialControllerSystemMenuItemKind = interface(IIterable_1__RadialControllerSystemMenuItemKind_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerConfiguration
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialControllerConfiguration)]
  IRadialControllerConfiguration = interface(IInspectable)
  ['{A6B79ECB-6A52-4430-910C-56370A9D6B42}']
    procedure SetDefaultMenuItems(buttons: IIterable_1__RadialControllerSystemMenuItemKind); safecall;
    procedure ResetToDefaultMenuItems; safecall;
    function TrySelectDefaultMenuItem(&type: RadialControllerSystemMenuItemKind): Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerConfiguration2
  IRadialControllerConfiguration2 = interface(IInspectable)
  ['{3D577EF7-3CEE-11E6-B535-001BDC06AB3B}']
    procedure put_ActiveControllerWhenMenuIsSuppressed(value: IRadialController); safecall;
    function get_ActiveControllerWhenMenuIsSuppressed: IRadialController; safecall;
    procedure put_IsMenuSuppressed(value: Boolean); safecall;
    function get_IsMenuSuppressed: Boolean; safecall;
    property ActiveControllerWhenMenuIsSuppressed: IRadialController read get_ActiveControllerWhenMenuIsSuppressed write put_ActiveControllerWhenMenuIsSuppressed;
    property IsMenuSuppressed: Boolean read get_IsMenuSuppressed write put_IsMenuSuppressed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerConfigurationStatics
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialControllerConfiguration)]
  IRadialControllerConfigurationStatics = interface(IInspectable)
  ['{79B6B0E5-069A-4486-A99D-8DB772B9642F}']
    function GetForCurrentView: IRadialControllerConfiguration; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerConfigurationStatics2
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialControllerConfiguration)]
  IRadialControllerConfigurationStatics2 = interface(IInspectable)
  ['{53E08B17-E205-48D3-9CAF-80FF47C4D7C7}']
    procedure put_AppController(value: IRadialController); safecall;
    function get_AppController: IRadialController; safecall;
    procedure put_IsAppControllerEnabled(value: Boolean); safecall;
    function get_IsAppControllerEnabled: Boolean; safecall;
    property AppController: IRadialController read get_AppController write put_AppController;
    property IsAppControllerEnabled: Boolean read get_IsAppControllerEnabled write put_IsAppControllerEnabled;
  end;

  // Windows.UI.Input.IRadialControllerControlAcquiredEventArgs2
  IRadialControllerControlAcquiredEventArgs2 = interface(IInspectable)
  ['{3D577EF4-3CEE-11E6-B535-001BDC06AB3B}']
    function get_IsButtonPressed: Boolean; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property IsButtonPressed: Boolean read get_IsButtonPressed;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerMenuItemStatics
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialControllerMenuItem)]
  IRadialControllerMenuItemStatics = interface(IInspectable)
  ['{249E0887-D842-4524-9DF8-E0D647EDC887}']
    function CreateFromIcon(displayText: HSTRING; icon: IRandomAccessStreamReference): IRadialControllerMenuItem; safecall;
    function CreateFromKnownIcon(displayText: HSTRING; value: RadialControllerMenuKnownIcon): IRadialControllerMenuItem; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerMenuItemStatics2
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialControllerMenuItem)]
  IRadialControllerMenuItemStatics2 = interface(IInspectable)
  ['{0CBB70BE-7E3E-48BD-BE04-2C7FCAA9C1FF}']
    function CreateFromFontGlyph(displayText: HSTRING; glyph: HSTRING; fontFamily: HSTRING): IRadialControllerMenuItem; overload; safecall;
    function CreateFromFontGlyph(displayText: HSTRING; glyph: HSTRING; fontFamily: HSTRING; fontUri: IUriRuntimeClass): IRadialControllerMenuItem; overload; safecall;
  end;

  // Windows.UI.Input.IRadialControllerRotationChangedEventArgs2
  IRadialControllerRotationChangedEventArgs2 = interface(IInspectable)
  ['{3D577EEC-4CEE-11E6-B535-001BDC06AB3B}']
    function get_IsButtonPressed: Boolean; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property IsButtonPressed: Boolean read get_IsButtonPressed;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // Windows.UI.Input.IRadialControllerScreenContactContinuedEventArgs2
  IRadialControllerScreenContactContinuedEventArgs2 = interface(IInspectable)
  ['{3D577EF1-3CEE-11E6-B535-001BDC06AB3B}']
    function get_IsButtonPressed: Boolean; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property IsButtonPressed: Boolean read get_IsButtonPressed;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // Windows.UI.Input.IRadialControllerScreenContactEndedEventArgs
  IRadialControllerScreenContactEndedEventArgs = interface(IInspectable)
  ['{3D577EF2-3CEE-11E6-B535-001BDC06AB3B}']
    function get_IsButtonPressed: Boolean; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property IsButtonPressed: Boolean read get_IsButtonPressed;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // Windows.UI.Input.IRadialControllerScreenContactStartedEventArgs2
  IRadialControllerScreenContactStartedEventArgs2 = interface(IInspectable)
  ['{3D577EF0-3CEE-11E6-B535-001BDC06AB3B}']
    function get_IsButtonPressed: Boolean; safecall;
    function get_SimpleHapticsController: Haptics_ISimpleHapticsController; safecall;
    property IsButtonPressed: Boolean read get_IsButtonPressed;
    property SimpleHapticsController: Haptics_ISimpleHapticsController read get_SimpleHapticsController;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.IRadialControllerStatics
  [WinRTClassNameAttribute(SWindows_UI_Input_RadialController)]
  IRadialControllerStatics = interface(IInspectable)
  ['{FADED0B7-B84C-4894-87AA-8F25AA5F288B}']
    function IsSupported: Boolean; safecall;
    function CreateForCurrentView: IRadialController; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialPointerPose
  Spatial_ISpatialPointerPose = interface(IInspectable)
  ['{6953A42E-C17E-357D-97A1-7269D0ED2D10}']
    function get_Timestamp: IPerceptionTimestamp; safecall;
    function get_Head: People_IHeadPose; safecall;
    property Head: People_IHeadPose read get_Head;
    property Timestamp: IPerceptionTimestamp read get_Timestamp;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialRecognitionStartedEventArgs
  Spatial_ISpatialRecognitionStartedEventArgs = interface(IInspectable)
  ['{24DA128F-0008-4A6D-AA50-2A76F9CFB264}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function TryGetPointerPose(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialPointerPose; safecall;
    function IsGesturePossible(gesture: Spatial_SpatialGestureSettings): Boolean; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialRecognitionStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionStartedEventArgs_Delegate_Base = interface(IUnknown)
  ['{F2BD99D6-99FA-5599-A14A-1F7A7A92E3D7}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialRecognitionStartedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialRecognitionStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionStartedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionStartedEventArgs_Delegate_Base)
  ['{6BB028C6-A13C-58D6-8DA3-CFEC391DF2D2}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialRecognitionEndedEventArgs
  Spatial_ISpatialRecognitionEndedEventArgs = interface(IInspectable)
  ['{0E35F5CB-3F75-43F3-AC81-D1DC2DF9B1FB}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialRecognitionEndedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionEndedEventArgs_Delegate_Base = interface(IUnknown)
  ['{AFA7FAD7-3FD7-5C81-8802-195C523F6E7B}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialRecognitionEndedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialRecognitionEndedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionEndedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionEndedEventArgs_Delegate_Base)
  ['{08989675-7333-5C33-9310-7309365EFB0C}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialTappedEventArgs
  Spatial_ISpatialTappedEventArgs = interface(IInspectable)
  ['{296D83DE-F444-4AA1-B2BF-9DC88D567DA6}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function TryGetPointerPose(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialPointerPose; safecall;
    function get_TapCount: Cardinal; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
    property TapCount: Cardinal read get_TapCount;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialTappedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialTappedEventArgs_Delegate_Base = interface(IUnknown)
  ['{630205A4-2F93-5022-B7B8-C43ED428498A}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialTappedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialTappedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialTappedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialTappedEventArgs_Delegate_Base)
  ['{4F851809-DC24-54E3-BF44-4E22A87B3D0A}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialHoldStartedEventArgs
  Spatial_ISpatialHoldStartedEventArgs = interface(IInspectable)
  ['{8E343D79-ACB6-4144-8615-2CFBA8A3CB3F}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function TryGetPointerPose(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialPointerPose; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldStartedEventArgs_Delegate_Base = interface(IUnknown)
  ['{7537DD12-02B8-5132-8FF7-90C80EF454D1}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialHoldStartedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldStartedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldStartedEventArgs_Delegate_Base)
  ['{3CCF2703-6B88-5320-BF77-44D00823D336}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialHoldCompletedEventArgs
  Spatial_ISpatialHoldCompletedEventArgs = interface(IInspectable)
  ['{3F64470B-4CFD-43DA-8DC4-E64552173971}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{E425D80E-8C18-5375-B600-1DD7A11DBA5E}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialHoldCompletedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCompletedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCompletedEventArgs_Delegate_Base)
  ['{3AC2696B-FE32-5FE1-80DC-7641CFB0EF62}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialHoldCanceledEventArgs
  Spatial_ISpatialHoldCanceledEventArgs = interface(IInspectable)
  ['{5DFCB667-4CAA-4093-8C35-B601A839F31B}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCanceledEventArgs_Delegate_Base = interface(IUnknown)
  ['{6A715963-C49F-5F3D-BFEC-952700308860}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialHoldCanceledEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialHoldCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCanceledEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCanceledEventArgs_Delegate_Base)
  ['{30C8A7E7-55A1-5FA3-A379-876B137C2F44}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialManipulationStartedEventArgs
  Spatial_ISpatialManipulationStartedEventArgs = interface(IInspectable)
  ['{A1D6BBCE-42A5-377B-ADA6-D28E3D384737}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function TryGetPointerPose(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialPointerPose; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationStartedEventArgs_Delegate_Base = interface(IUnknown)
  ['{A123252B-CFE8-5CFB-B61C-F2DC011EB12E}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialManipulationStartedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationStartedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationStartedEventArgs_Delegate_Base)
  ['{6FC3387D-4449-55E6-8C80-E825F67AC5B4}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialManipulationDelta
  Spatial_ISpatialManipulationDelta = interface(IInspectable)
  ['{A7EC967A-D123-3A81-A15B-992923DCBE91}']
    function get_Translation: Numerics_Vector3; safecall;
    property Translation: Numerics_Vector3 read get_Translation;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialManipulationUpdatedEventArgs
  Spatial_ISpatialManipulationUpdatedEventArgs = interface(IInspectable)
  ['{5F230B9B-60C6-4DC6-BDC9-9F4A6F15FE49}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function TryGetCumulativeDelta(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialManipulationDelta; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationUpdatedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{CECBE023-3C49-530E-A010-8C0C3CBD3088}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialManipulationUpdatedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationUpdatedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationUpdatedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationUpdatedEventArgs_Delegate_Base)
  ['{623E4FD9-E1D3-5AFA-8527-C91BCC2F0510}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialManipulationCompletedEventArgs
  Spatial_ISpatialManipulationCompletedEventArgs = interface(IInspectable)
  ['{05086802-F301-4343-9250-2FBAA5F87A37}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function TryGetCumulativeDelta(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialManipulationDelta; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{10DAC914-BEA2-5D67-8607-DC470EA1DCD8}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialManipulationCompletedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCompletedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCompletedEventArgs_Delegate_Base)
  ['{98DA95B2-41EB-5070-990A-CC5145AEF0B6}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialManipulationCanceledEventArgs
  Spatial_ISpatialManipulationCanceledEventArgs = interface(IInspectable)
  ['{2D40D1CB-E7DA-4220-B0BF-819301674780}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCanceledEventArgs_Delegate_Base = interface(IUnknown)
  ['{B92A6754-3946-5353-B8F9-2A07EB0E8886}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialManipulationCanceledEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialManipulationCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCanceledEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCanceledEventArgs_Delegate_Base)
  ['{C7CC67FC-9E5C-5731-8C74-15F7DFF2347D}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialNavigationStartedEventArgs
  Spatial_ISpatialNavigationStartedEventArgs = interface(IInspectable)
  ['{754A348A-FB64-4656-8EBD-9DEECAAFE475}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function TryGetPointerPose(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialPointerPose; safecall;
    function get_IsNavigatingX: Boolean; safecall;
    function get_IsNavigatingY: Boolean; safecall;
    function get_IsNavigatingZ: Boolean; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
    property IsNavigatingX: Boolean read get_IsNavigatingX;
    property IsNavigatingY: Boolean read get_IsNavigatingY;
    property IsNavigatingZ: Boolean read get_IsNavigatingZ;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationStartedEventArgs_Delegate_Base = interface(IUnknown)
  ['{5419CADC-2600-527B-9398-1744318D0618}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialNavigationStartedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationStartedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationStartedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationStartedEventArgs_Delegate_Base)
  ['{DFF8C7A2-7F59-5074-B6E2-75281D1C4B04}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialNavigationUpdatedEventArgs
  Spatial_ISpatialNavigationUpdatedEventArgs = interface(IInspectable)
  ['{9B713FD7-839D-4A74-8732-45466FC044B5}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function get_NormalizedOffset: Numerics_Vector3; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
    property NormalizedOffset: Numerics_Vector3 read get_NormalizedOffset;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationUpdatedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationUpdatedEventArgs_Delegate_Base = interface(IUnknown)
  ['{A7216B90-C175-5A28-B500-A445F0751C5B}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialNavigationUpdatedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationUpdatedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationUpdatedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationUpdatedEventArgs_Delegate_Base)
  ['{4D6C39A6-236B-52C8-A763-FD05D5795EAF}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialNavigationCompletedEventArgs
  Spatial_ISpatialNavigationCompletedEventArgs = interface(IInspectable)
  ['{012E80B7-AF3B-42C2-9E41-BAAA0E721F3A}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    function get_NormalizedOffset: Numerics_Vector3; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
    property NormalizedOffset: Numerics_Vector3 read get_NormalizedOffset;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{F0D459DF-EBD3-5388-B0A9-5F44BCD6F58F}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialNavigationCompletedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationCompletedEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCompletedEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCompletedEventArgs_Delegate_Base)
  ['{4F7F5901-0DE8-5C8C-9E4A-2C415FBE3B05}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialNavigationCanceledEventArgs
  Spatial_ISpatialNavigationCanceledEventArgs = interface(IInspectable)
  ['{CE503EDC-E8A5-46F0-92D4-3C122B35112A}']
    function get_InteractionSourceKind: Spatial_SpatialInteractionSourceKind; safecall;
    property InteractionSourceKind: Spatial_SpatialInteractionSourceKind read get_InteractionSourceKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCanceledEventArgs_Delegate_Base = interface(IUnknown)
  ['{9947EA8E-EB4D-5F93-9FD9-2ADE6470BC5D}']
    procedure Invoke(sender: Spatial_ISpatialGestureRecognizer; args: Spatial_ISpatialNavigationCanceledEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Input.Spatial.ISpatialGestureRecognizer,Windows.UI.Input.Spatial.ISpatialNavigationCanceledEventArgs>
  TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCanceledEventArgs = interface(TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCanceledEventArgs_Delegate_Base)
  ['{CD45B698-83F2-533E-A14A-C991E53CE489}']
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialInteractionSourceLocation
  Spatial_ISpatialInteractionSourceLocation = interface(IInspectable)
  ['{EA4696C4-7E8B-30CA-BCC5-C77189CEA30A}']
    function get_Position: IReference_1__Numerics_Vector3; safecall;
    function get_Velocity: IReference_1__Numerics_Vector3; safecall;
    property Position: IReference_1__Numerics_Vector3 read get_Position;
    property Velocity: IReference_1__Numerics_Vector3 read get_Velocity;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialInteractionSourceProperties
  Spatial_ISpatialInteractionSourceProperties = interface(IInspectable)
  ['{05604542-3EF7-3222-9F53-63C9CB7E3BC7}']
    function TryGetSourceLossMitigationDirection(coordinateSystem: Spatial_ISpatialCoordinateSystem): IReference_1__Numerics_Vector3; safecall;
    function get_SourceLossRisk: Double; safecall;
    function TryGetLocation(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialInteractionSourceLocation; safecall;
    property SourceLossRisk: Double read get_SourceLossRisk;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialInteractionSourceState
  Spatial_ISpatialInteractionSourceState = interface(IInspectable)
  ['{D5C475EF-4B63-37EC-98B9-9FC652B9D2F2}']
    function get_Source: Spatial_ISpatialInteractionSource; safecall;
    function get_Properties: Spatial_ISpatialInteractionSourceProperties; safecall;
    function get_IsPressed: Boolean; safecall;
    function get_Timestamp: IPerceptionTimestamp; safecall;
    function TryGetPointerPose(coordinateSystem: Spatial_ISpatialCoordinateSystem): Spatial_ISpatialPointerPose; safecall;
    property IsPressed: Boolean read get_IsPressed;
    property Properties: Spatial_ISpatialInteractionSourceProperties read get_Properties;
    property Source: Spatial_ISpatialInteractionSource read get_Source;
    property Timestamp: IPerceptionTimestamp read get_Timestamp;
  end;

  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialInteraction
  Spatial_ISpatialInteraction = interface(IInspectable)
  ['{FC967639-88E6-4646-9112-4344AAEC9DFA}']
    function get_SourceState: Spatial_ISpatialInteractionSourceState; safecall;
    property SourceState: Spatial_ISpatialInteractionSourceState read get_SourceState;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialGestureRecognizer
  [WinRTClassNameAttribute(SWindows_UI_Input_Spatial_SpatialGestureRecognizer)]
  Spatial_ISpatialGestureRecognizer = interface(IInspectable)
  ['{71605BCC-0C35-4673-ADBD-CC04CAA6EF45}']
    function add_RecognitionStarted(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionStartedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RecognitionStarted(token: EventRegistrationToken); safecall;
    function add_RecognitionEnded(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialRecognitionEndedEventArgs): EventRegistrationToken; safecall;
    procedure remove_RecognitionEnded(token: EventRegistrationToken); safecall;
    function add_Tapped(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialTappedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Tapped(token: EventRegistrationToken); safecall;
    function add_HoldStarted(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldStartedEventArgs): EventRegistrationToken; safecall;
    procedure remove_HoldStarted(token: EventRegistrationToken); safecall;
    function add_HoldCompleted(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_HoldCompleted(token: EventRegistrationToken); safecall;
    function add_HoldCanceled(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialHoldCanceledEventArgs): EventRegistrationToken; safecall;
    procedure remove_HoldCanceled(token: EventRegistrationToken); safecall;
    function add_ManipulationStarted(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationStartedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ManipulationStarted(token: EventRegistrationToken); safecall;
    function add_ManipulationUpdated(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ManipulationUpdated(token: EventRegistrationToken); safecall;
    function add_ManipulationCompleted(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ManipulationCompleted(token: EventRegistrationToken); safecall;
    function add_ManipulationCanceled(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialManipulationCanceledEventArgs): EventRegistrationToken; safecall;
    procedure remove_ManipulationCanceled(token: EventRegistrationToken); safecall;
    function add_NavigationStarted(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationStartedEventArgs): EventRegistrationToken; safecall;
    procedure remove_NavigationStarted(token: EventRegistrationToken); safecall;
    function add_NavigationUpdated(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationUpdatedEventArgs): EventRegistrationToken; safecall;
    procedure remove_NavigationUpdated(token: EventRegistrationToken); safecall;
    function add_NavigationCompleted(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_NavigationCompleted(token: EventRegistrationToken); safecall;
    function add_NavigationCanceled(handler: TypedEventHandler_2__Spatial_ISpatialGestureRecognizer__Spatial_ISpatialNavigationCanceledEventArgs): EventRegistrationToken; safecall;
    procedure remove_NavigationCanceled(token: EventRegistrationToken); safecall;
    procedure CaptureInteraction(interaction: Spatial_ISpatialInteraction); safecall;
    procedure CancelPendingGestures; safecall;
    function TrySetGestureSettings(settings: Spatial_SpatialGestureSettings): Boolean; safecall;
    function get_GestureSettings: Spatial_SpatialGestureSettings; safecall;
    property GestureSettings: Spatial_SpatialGestureSettings read get_GestureSettings;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Input.Spatial.ISpatialGestureRecognizerFactory
  [WinRTClassNameAttribute(SWindows_UI_Input_Spatial_SpatialGestureRecognizer)]
  Spatial_ISpatialGestureRecognizerFactory = interface(IInspectable)
  ['{77214186-57B9-3150-8382-698B24E264D0}']
    function Create(settings: Spatial_SpatialGestureSettings): Spatial_ISpatialGestureRecognizer; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IIterator_1__Spatial_ISpatialInteractionSourceState_Base = interface(IInspectable)
  ['{23F50032-907E-5D73-BBDC-F7563DE655C4}']
    function get_Current: Spatial_ISpatialInteractionSourceState; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSpatial_ISpatialInteractionSourceState): Cardinal; safecall;
    property Current: Spatial_ISpatialInteractionSourceState read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IIterator_1__Spatial_ISpatialInteractionSourceState = interface(IIterator_1__Spatial_ISpatialInteractionSourceState_Base)
  ['{C161CBD4-CFE8-5B50-A71F-70FFB7B0982F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IIterable_1__Spatial_ISpatialInteractionSourceState_Base = interface(IInspectable)
  ['{7ECCE0F5-0874-506F-8D42-4C9519156407}']
    function First: IIterator_1__Spatial_ISpatialInteractionSourceState; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IIterable_1__Spatial_ISpatialInteractionSourceState = interface(IIterable_1__Spatial_ISpatialInteractionSourceState_Base)
  ['{BC513341-0EFC-5C29-8942-D0C78951315F}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Input.Spatial.ISpatialInteractionSourceState>
  IVectorView_1__Spatial_ISpatialInteractionSourceState = interface(IInspectable)
  ['{F9E91AB1-3E81-534D-A56E-9936B3D33F3C}']
    function GetAt(index: Cardinal): Spatial_ISpatialInteractionSourceState; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Spatial_ISpatialInteractionSourceState; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSpatial_ISpatialInteractionSourceState): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.Input.Core.RadialControllerIndependentInputSource
  // DualAPI
  // Implements: Windows.UI.Input.Core.IRadialControllerIndependentInputSource
  // Implements: Windows.UI.Input.Core.IRadialControllerIndependentInputSource2
  // Statics: "Windows.UI.Input.Core.IRadialControllerIndependentInputSourceStatics"
  // Interop Intf: "Core_IRadialControllerIndependentInputSourceInterop"
  Core_IRadialControllerIndependentInputSourceInterop = interface(IInspectable)
    ['{3D577EFF-4CEE-11E6-B535-001BDC06AB3B}']
    function CreateForWindow(wnd: THandle; const riid: TGUID): Core_IRadialControllerIndependentInputSource; safecall;
  end;
  TCore_RadialControllerIndependentInputSource = class(TWinRTGenericImportSO<Core_IRadialControllerIndependentInputSourceStatics, Core_IRadialControllerIndependentInputSourceInterop>)
  public
    // -> Core_IRadialControllerIndependentInputSourceStatics
    class function CreateForView(view: ICoreApplicationView): Core_IRadialControllerIndependentInputSource; static; inline;
  end;

  // Windows.UI.Input.RadialController
  // DualAPI
  // Implements: Windows.UI.Input.IRadialController
  // Implements: Windows.UI.Input.IRadialController2
  // Statics: "Windows.UI.Input.IRadialControllerStatics"
  // Interop Intf: "IRadialControllerInterop"
  IRadialControllerInterop = interface(IInspectable)
    ['{1B0535C9-57AD-45C1-9D79-AD5C34360513}']
    function CreateForWindow(wnd: THandle; const riid: TGUID): IRadialController; safecall;
  end;
  TRadialController = class(TWinRTGenericImportSO<IRadialControllerStatics, IRadialControllerInterop>)
  public
    // -> IRadialControllerStatics
    class function IsSupported: Boolean; static; inline;
    class function CreateForCurrentView: IRadialController; static; inline;
  end;

  // Windows.UI.Input.RadialControllerConfiguration
  // DualAPI
  // Implements: Windows.UI.Input.IRadialControllerConfiguration
  // Implements: Windows.UI.Input.IRadialControllerConfiguration2
  // Statics: "Windows.UI.Input.IRadialControllerConfigurationStatics"
  // Statics: "Windows.UI.Input.IRadialControllerConfigurationStatics2"
  // Interop Intf: "IRadialControllerConfigurationInterop"
  IRadialControllerConfigurationInterop = interface(IInspectable)
    ['{787cdaac-3186-476d-87e4-b9374a7b9970}']
    function GetForWindow(wnd: THandle; const riid: TGUID): IRadialControllerConfiguration; safecall;
  end;
  TRadialControllerConfiguration = class(TWinRTGenericImportS2O<IRadialControllerConfigurationStatics, IRadialControllerConfigurationStatics2, IRadialControllerConfigurationInterop>)
  public
    // -> IRadialControllerConfigurationStatics
    class function GetForCurrentView: IRadialControllerConfiguration; static; inline;

    // -> IRadialControllerConfigurationStatics2
    class procedure put_AppController(value: IRadialController); static; inline;
    class function get_AppController: IRadialController; static; inline;
    class procedure put_IsAppControllerEnabled(value: Boolean); static; inline;
    class function get_IsAppControllerEnabled: Boolean; static; inline;
    class property AppController: IRadialController read get_AppController write put_AppController;
    class property IsAppControllerEnabled: Boolean read get_IsAppControllerEnabled write put_IsAppControllerEnabled;
  end;

  // Windows.UI.Input.RadialControllerMenuItem
  // DualAPI
  // Implements: Windows.UI.Input.IRadialControllerMenuItem
  // Statics: "Windows.UI.Input.IRadialControllerMenuItemStatics"
  // Statics: "Windows.UI.Input.IRadialControllerMenuItemStatics2"
  TRadialControllerMenuItem = class(TWinRTGenericImportS2<IRadialControllerMenuItemStatics, IRadialControllerMenuItemStatics2>)
  public
    // -> IRadialControllerMenuItemStatics
    class function CreateFromIcon(displayText: HSTRING; icon: IRandomAccessStreamReference): IRadialControllerMenuItem; static; inline;
    class function CreateFromKnownIcon(displayText: HSTRING; value: RadialControllerMenuKnownIcon): IRadialControllerMenuItem; static; inline;

    // -> IRadialControllerMenuItemStatics2
    class function CreateFromFontGlyph(displayText: HSTRING; glyph: HSTRING; fontFamily: HSTRING): IRadialControllerMenuItem; overload; static; inline;
    class function CreateFromFontGlyph(displayText: HSTRING; glyph: HSTRING; fontFamily: HSTRING; fontUri: IUriRuntimeClass): IRadialControllerMenuItem; overload; static; inline;
  end;

  // Windows.UI.Input.Spatial.SpatialGestureRecognizer
  // DualAPI
  // Implements: Windows.UI.Input.Spatial.ISpatialGestureRecognizer
  // Factory: "Windows.UI.Input.Spatial.ISpatialGestureRecognizerFactory"
  TSpatial_SpatialGestureRecognizer = class(TWinRTGenericImportF<Spatial_ISpatialGestureRecognizerFactory>)
  public
    // -> Spatial_ISpatialGestureRecognizerFactory
    class function Create(settings: Spatial_SpatialGestureSettings): Spatial_ISpatialGestureRecognizer; static; inline;
  end;

implementation

{ TCore_RadialControllerIndependentInputSource }

class function TCore_RadialControllerIndependentInputSource.CreateForView(view: ICoreApplicationView): Core_IRadialControllerIndependentInputSource;
begin
  Result := Statics.CreateForView(view);
end;


{ TRadialController }

class function TRadialController.IsSupported: Boolean;
begin
  Result := Statics.IsSupported;
end;

class function TRadialController.CreateForCurrentView: IRadialController;
begin
  Result := Statics.CreateForCurrentView;
end;


{ TRadialControllerConfiguration }

class function TRadialControllerConfiguration.GetForCurrentView: IRadialControllerConfiguration;
begin
  Result := Statics.GetForCurrentView;
end;


class procedure TRadialControllerConfiguration.put_AppController(value: IRadialController);
begin
  Statics2.put_AppController(value);
end;

class function TRadialControllerConfiguration.get_AppController: IRadialController;
begin
  Result := Statics2.get_AppController;
end;

class procedure TRadialControllerConfiguration.put_IsAppControllerEnabled(value: Boolean);
begin
  Statics2.put_IsAppControllerEnabled(value);
end;

class function TRadialControllerConfiguration.get_IsAppControllerEnabled: Boolean;
begin
  Result := Statics2.get_IsAppControllerEnabled;
end;


{ TRadialControllerMenuItem }

class function TRadialControllerMenuItem.CreateFromIcon(displayText: HSTRING; icon: IRandomAccessStreamReference): IRadialControllerMenuItem;
begin
  Result := Statics.CreateFromIcon(displayText, icon);
end;

class function TRadialControllerMenuItem.CreateFromKnownIcon(displayText: HSTRING; value: RadialControllerMenuKnownIcon): IRadialControllerMenuItem;
begin
  Result := Statics.CreateFromKnownIcon(displayText, value);
end;


class function TRadialControllerMenuItem.CreateFromFontGlyph(displayText: HSTRING; glyph: HSTRING; fontFamily: HSTRING): IRadialControllerMenuItem;
begin
  Result := Statics2.CreateFromFontGlyph(displayText, glyph, fontFamily);
end;

class function TRadialControllerMenuItem.CreateFromFontGlyph(displayText: HSTRING; glyph: HSTRING; fontFamily: HSTRING; fontUri: IUriRuntimeClass): IRadialControllerMenuItem;
begin
  Result := Statics2.CreateFromFontGlyph(displayText, glyph, fontFamily, fontUri);
end;


{ TSpatial_SpatialGestureRecognizer }
// Factories for : "Spatial_SpatialGestureRecognizer"
// Factory: "Windows.UI.Input.Spatial.ISpatialGestureRecognizerFactory"
// -> Spatial_ISpatialGestureRecognizerFactory

class function TSpatial_SpatialGestureRecognizer.Create(settings: Spatial_SpatialGestureSettings): Spatial_ISpatialGestureRecognizer;
begin
  Result := Factory.Create(settings);
end;


end.
