{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Gaming;

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

  Input_IGameController = Winapi.CommonTypes.Input_IGameController;
  PInput_IGameController = Winapi.CommonTypes.PInput_IGameController;
  Input_IHeadset = Winapi.CommonTypes.Input_IHeadset;
  PInput_IHeadset = Winapi.CommonTypes.PInput_IHeadset;
  TypedEventHandler_2__Input_IGameController__Input_IHeadset_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Input_IGameController__Input_IHeadset_Delegate_Base;
  TypedEventHandler_2__Input_IGameController__Input_IHeadset = Winapi.CommonTypes.TypedEventHandler_2__Input_IGameController__Input_IHeadset;
  PTypedEventHandler_2__Input_IGameController__Input_IHeadset = Winapi.CommonTypes.PTypedEventHandler_2__Input_IGameController__Input_IHeadset;

  // Forward declarations for interfaces

  // Windows.Gaming.Input.Custom.IGameControllerProvider
  Input_Custom_IGameControllerProvider = interface;
  PInput_Custom_IGameControllerProvider = ^Input_Custom_IGameControllerProvider;

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

  // Windows.Gaming.Input.Custom.ICustomGameControllerFactory
  Input_Custom_ICustomGameControllerFactory = interface;
  PInput_Custom_ICustomGameControllerFactory = ^Input_Custom_ICustomGameControllerFactory;

  // Windows.Gaming.Input.Custom.IGameControllerFactoryManagerStatics
  Input_Custom_IGameControllerFactoryManagerStatics = interface;
  PInput_Custom_IGameControllerFactoryManagerStatics = ^Input_Custom_IGameControllerFactoryManagerStatics;

  // Windows.Gaming.Input.Custom.IGameControllerFactoryManagerStatics2
  Input_Custom_IGameControllerFactoryManagerStatics2 = interface;
  PInput_Custom_IGameControllerFactoryManagerStatics2 = ^Input_Custom_IGameControllerFactoryManagerStatics2;

  // Windows.Gaming.Input.Custom.IGameControllerInputSink
  Input_Custom_IGameControllerInputSink = interface;
  PInput_Custom_IGameControllerInputSink = ^Input_Custom_IGameControllerInputSink;

  // Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult
  Input_Custom_IGipFirmwareUpdateResult = interface;
  PInput_Custom_IGipFirmwareUpdateResult = ^Input_Custom_IGipFirmwareUpdateResult;

  // Windows.Gaming.Input.Custom.IGipGameControllerInputSink
  Input_Custom_IGipGameControllerInputSink = interface;
  PInput_Custom_IGipGameControllerInputSink = ^Input_Custom_IGipGameControllerInputSink;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = interface;
  PAsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = ^AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = interface;
  PAsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = ^AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = interface;
  PIAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = ^IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress;

  // Windows.Gaming.Input.Custom.IGipGameControllerProvider
  Input_Custom_IGipGameControllerProvider = interface;
  PInput_Custom_IGipGameControllerProvider = ^Input_Custom_IGipGameControllerProvider;

  // Windows.Gaming.Input.Custom.IHidGameControllerInputSink
  Input_Custom_IHidGameControllerInputSink = interface;
  PInput_Custom_IHidGameControllerInputSink = ^Input_Custom_IHidGameControllerInputSink;

  // Windows.Gaming.Input.Custom.IHidGameControllerProvider
  Input_Custom_IHidGameControllerProvider = interface;
  PInput_Custom_IHidGameControllerProvider = ^Input_Custom_IHidGameControllerProvider;

  // Windows.Gaming.Input.Custom.IXusbGameControllerInputSink
  Input_Custom_IXusbGameControllerInputSink = interface;
  PInput_Custom_IXusbGameControllerInputSink = ^Input_Custom_IXusbGameControllerInputSink;

  // Windows.Gaming.Input.Custom.IXusbGameControllerProvider
  Input_Custom_IXusbGameControllerProvider = interface;
  PInput_Custom_IXusbGameControllerProvider = ^Input_Custom_IXusbGameControllerProvider;

  // Windows.Gaming.Input.ForceFeedback.IForceFeedbackEffect
  Input_ForceFeedback_IForceFeedbackEffect = interface;
  PInput_ForceFeedback_IForceFeedbackEffect = ^Input_ForceFeedback_IForceFeedbackEffect;

  // Windows.Gaming.Input.ForceFeedback.IConditionForceEffect
  Input_ForceFeedback_IConditionForceEffect = interface;
  PInput_ForceFeedback_IConditionForceEffect = ^Input_ForceFeedback_IConditionForceEffect;

  // Windows.Gaming.Input.ForceFeedback.IConditionForceEffectFactory
  Input_ForceFeedback_IConditionForceEffectFactory = interface;
  PInput_ForceFeedback_IConditionForceEffectFactory = ^Input_ForceFeedback_IConditionForceEffectFactory;

  // Windows.Gaming.Input.ForceFeedback.IConstantForceEffect
  Input_ForceFeedback_IConstantForceEffect = interface;
  PInput_ForceFeedback_IConstantForceEffect = ^Input_ForceFeedback_IConstantForceEffect;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.Input.ForceFeedback.ForceFeedbackLoadEffectResult>
  AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult = interface;
  PAsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult = ^AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.Input.ForceFeedback.ForceFeedbackLoadEffectResult>
  IAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult = interface;
  PIAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult = ^IAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult;

  // Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor
  Input_ForceFeedback_IForceFeedbackMotor = interface;
  PInput_ForceFeedback_IForceFeedbackMotor = ^Input_ForceFeedback_IForceFeedbackMotor;

  // Windows.Gaming.Input.ForceFeedback.IPeriodicForceEffect
  Input_ForceFeedback_IPeriodicForceEffect = interface;
  PInput_ForceFeedback_IPeriodicForceEffect = ^Input_ForceFeedback_IPeriodicForceEffect;

  // Windows.Gaming.Input.ForceFeedback.IPeriodicForceEffectFactory
  Input_ForceFeedback_IPeriodicForceEffectFactory = interface;
  PInput_ForceFeedback_IPeriodicForceEffectFactory = ^Input_ForceFeedback_IPeriodicForceEffectFactory;

  // Windows.Gaming.Input.ForceFeedback.IRampForceEffect
  Input_ForceFeedback_IRampForceEffect = interface;
  PInput_ForceFeedback_IRampForceEffect = ^Input_ForceFeedback_IRampForceEffect;

  // Windows.Gaming.Input.IArcadeStick
  Input_IArcadeStick = interface;
  PInput_IArcadeStick = ^Input_IArcadeStick;

  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IArcadeStick>
  EventHandler_1__Input_IArcadeStick = interface;
  PEventHandler_1__Input_IArcadeStick = ^EventHandler_1__Input_IArcadeStick;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IArcadeStick>
  IIterator_1__Input_IArcadeStick = interface;
  PIIterator_1__Input_IArcadeStick = ^IIterator_1__Input_IArcadeStick;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IArcadeStick>
  IIterable_1__Input_IArcadeStick = interface;
  PIIterable_1__Input_IArcadeStick = ^IIterable_1__Input_IArcadeStick;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IArcadeStick>
  IVectorView_1__Input_IArcadeStick = interface;
  PIVectorView_1__Input_IArcadeStick = ^IVectorView_1__Input_IArcadeStick;

  // Windows.Gaming.Input.IArcadeStickStatics
  Input_IArcadeStickStatics = interface;
  PInput_IArcadeStickStatics = ^Input_IArcadeStickStatics;

  // Windows.Gaming.Input.IArcadeStickStatics2
  Input_IArcadeStickStatics2 = interface;
  PInput_IArcadeStickStatics2 = ^Input_IArcadeStickStatics2;

  // Windows.Gaming.Input.IFlightStick
  Input_IFlightStick = interface;
  PInput_IFlightStick = ^Input_IFlightStick;

  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IFlightStick>
  EventHandler_1__Input_IFlightStick = interface;
  PEventHandler_1__Input_IFlightStick = ^EventHandler_1__Input_IFlightStick;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IFlightStick>
  IIterator_1__Input_IFlightStick = interface;
  PIIterator_1__Input_IFlightStick = ^IIterator_1__Input_IFlightStick;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IFlightStick>
  IIterable_1__Input_IFlightStick = interface;
  PIIterable_1__Input_IFlightStick = ^IIterable_1__Input_IFlightStick;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IFlightStick>
  IVectorView_1__Input_IFlightStick = interface;
  PIVectorView_1__Input_IFlightStick = ^IVectorView_1__Input_IFlightStick;

  // Windows.Gaming.Input.IFlightStickStatics
  Input_IFlightStickStatics = interface;
  PInput_IFlightStickStatics = ^Input_IFlightStickStatics;

  // Windows.Foundation.IReference`1<Int32>
  IReference_1__Integer = interface;
  PIReference_1__Integer = ^IReference_1__Integer;

  // Windows.Gaming.Input.IGameControllerBatteryInfo
  Input_IGameControllerBatteryInfo = interface;
  PInput_IGameControllerBatteryInfo = ^Input_IGameControllerBatteryInfo;

  // Windows.Gaming.Input.IGamepad
  Input_IGamepad = interface;
  PInput_IGamepad = ^Input_IGamepad;

  // Windows.Gaming.Input.IGamepad2
  Input_IGamepad2 = interface;
  PInput_IGamepad2 = ^Input_IGamepad2;

  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IGamepad>
  EventHandler_1__Input_IGamepad = interface;
  PEventHandler_1__Input_IGamepad = ^EventHandler_1__Input_IGamepad;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IGamepad>
  IIterator_1__Input_IGamepad = interface;
  PIIterator_1__Input_IGamepad = ^IIterator_1__Input_IGamepad;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IGamepad>
  IIterable_1__Input_IGamepad = interface;
  PIIterable_1__Input_IGamepad = ^IIterable_1__Input_IGamepad;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IGamepad>
  IVectorView_1__Input_IGamepad = interface;
  PIVectorView_1__Input_IGamepad = ^IVectorView_1__Input_IGamepad;

  // Windows.Gaming.Input.IGamepadStatics
  Input_IGamepadStatics = interface;
  PInput_IGamepadStatics = ^Input_IGamepadStatics;

  // Windows.Gaming.Input.IGamepadStatics2
  Input_IGamepadStatics2 = interface;
  PInput_IGamepadStatics2 = ^Input_IGamepadStatics2;

  // Windows.Gaming.Input.IRacingWheel
  Input_IRacingWheel = interface;
  PInput_IRacingWheel = ^Input_IRacingWheel;

  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IRacingWheel>
  EventHandler_1__Input_IRacingWheel = interface;
  PEventHandler_1__Input_IRacingWheel = ^EventHandler_1__Input_IRacingWheel;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IRacingWheel>
  IIterator_1__Input_IRacingWheel = interface;
  PIIterator_1__Input_IRacingWheel = ^IIterator_1__Input_IRacingWheel;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IRacingWheel>
  IIterable_1__Input_IRacingWheel = interface;
  PIIterable_1__Input_IRacingWheel = ^IIterable_1__Input_IRacingWheel;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IRacingWheel>
  IVectorView_1__Input_IRacingWheel = interface;
  PIVectorView_1__Input_IRacingWheel = ^IVectorView_1__Input_IRacingWheel;

  // Windows.Gaming.Input.IRacingWheelStatics
  Input_IRacingWheelStatics = interface;
  PInput_IRacingWheelStatics = ^Input_IRacingWheelStatics;

  // Windows.Gaming.Input.IRacingWheelStatics2
  Input_IRacingWheelStatics2 = interface;
  PInput_IRacingWheelStatics2 = ^Input_IRacingWheelStatics2;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IIterator_1__Input_ForceFeedback_IForceFeedbackMotor = interface;
  PIIterator_1__Input_ForceFeedback_IForceFeedbackMotor = ^IIterator_1__Input_ForceFeedback_IForceFeedbackMotor;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IIterable_1__Input_ForceFeedback_IForceFeedbackMotor = interface;
  PIIterable_1__Input_ForceFeedback_IForceFeedbackMotor = ^IIterable_1__Input_ForceFeedback_IForceFeedbackMotor;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IVectorView_1__Input_ForceFeedback_IForceFeedbackMotor = interface;
  PIVectorView_1__Input_ForceFeedback_IForceFeedbackMotor = ^IVectorView_1__Input_ForceFeedback_IForceFeedbackMotor;

  // Windows.Gaming.Input.IRawGameController
  Input_IRawGameController = interface;
  PInput_IRawGameController = ^Input_IRawGameController;

  // Windows.Gaming.Input.IRawGameController2
  Input_IRawGameController2 = interface;
  PInput_IRawGameController2 = ^Input_IRawGameController2;

  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IRawGameController>
  EventHandler_1__Input_IRawGameController = interface;
  PEventHandler_1__Input_IRawGameController = ^EventHandler_1__Input_IRawGameController;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IRawGameController>
  IIterator_1__Input_IRawGameController = interface;
  PIIterator_1__Input_IRawGameController = ^IIterator_1__Input_IRawGameController;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IRawGameController>
  IIterable_1__Input_IRawGameController = interface;
  PIIterable_1__Input_IRawGameController = ^IIterable_1__Input_IRawGameController;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IRawGameController>
  IVectorView_1__Input_IRawGameController = interface;
  PIVectorView_1__Input_IRawGameController = ^IVectorView_1__Input_IRawGameController;

  // Windows.Gaming.Input.IRawGameControllerStatics
  Input_IRawGameControllerStatics = interface;
  PInput_IRawGameControllerStatics = ^Input_IRawGameControllerStatics;

  // Windows.Gaming.Input.IUINavigationController
  Input_IUINavigationController = interface;
  PInput_IUINavigationController = ^Input_IUINavigationController;

  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IUINavigationController>
  EventHandler_1__Input_IUINavigationController = interface;
  PEventHandler_1__Input_IUINavigationController = ^EventHandler_1__Input_IUINavigationController;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IUINavigationController>
  IIterator_1__Input_IUINavigationController = interface;
  PIIterator_1__Input_IUINavigationController = ^IIterator_1__Input_IUINavigationController;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IUINavigationController>
  IIterable_1__Input_IUINavigationController = interface;
  PIIterable_1__Input_IUINavigationController = ^IIterable_1__Input_IUINavigationController;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IUINavigationController>
  IVectorView_1__Input_IUINavigationController = interface;
  PIVectorView_1__Input_IUINavigationController = ^IVectorView_1__Input_IUINavigationController;

  // Windows.Gaming.Input.IUINavigationControllerStatics
  Input_IUINavigationControllerStatics = interface;
  PInput_IUINavigationControllerStatics = ^Input_IUINavigationControllerStatics;

  // Windows.Gaming.Input.IUINavigationControllerStatics2
  Input_IUINavigationControllerStatics2 = interface;
  PInput_IUINavigationControllerStatics2 = ^Input_IUINavigationControllerStatics2;

  // Windows.Gaming.Input.Preview.IGameControllerProviderInfoStatics
  Input_Preview_IGameControllerProviderInfoStatics = interface;
  PInput_Preview_IGameControllerProviderInfoStatics = ^Input_Preview_IGameControllerProviderInfoStatics;

  // Windows.Gaming.Preview.GamesEnumeration.GameListRemovedEventHandler
  Preview_GamesEnumeration_GameListRemovedEventHandler = interface;
  PPreview_GamesEnumeration_GameListRemovedEventHandler = ^Preview_GamesEnumeration_GameListRemovedEventHandler;

  // Windows.Foundation.Collections.IVector`1<String>
  IVector_1__HSTRING = interface;
  PIVector_1__HSTRING = ^IVector_1__HSTRING;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface;
  PEventHandler_1__IInspectable = ^EventHandler_1__IInspectable;

  // Windows.Gaming.UI.IGameBarStatics
  UI_IGameBarStatics = interface;
  PUI_IGameBarStatics = ^UI_IGameBarStatics;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface;
  PTypedEventHandler_2__Activation_ISplashScreen__IInspectable = ^TypedEventHandler_2__Activation_ISplashScreen__IInspectable;

  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult
  XboxLive_Storage_IGameSaveBlobGetResult = interface;
  PXboxLive_Storage_IGameSaveBlobGetResult = ^XboxLive_Storage_IGameSaveBlobGetResult;

  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo
  XboxLive_Storage_IGameSaveBlobInfo = interface;
  PXboxLive_Storage_IGameSaveBlobInfo = ^XboxLive_Storage_IGameSaveBlobInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IIterator_1__XboxLive_Storage_IGameSaveBlobInfo = interface;
  PIIterator_1__XboxLive_Storage_IGameSaveBlobInfo = ^IIterator_1__XboxLive_Storage_IGameSaveBlobInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IIterable_1__XboxLive_Storage_IGameSaveBlobInfo = interface;
  PIIterable_1__XboxLive_Storage_IGameSaveBlobInfo = ^IIterable_1__XboxLive_Storage_IGameSaveBlobInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IVectorView_1__XboxLive_Storage_IGameSaveBlobInfo = interface;
  PIVectorView_1__XboxLive_Storage_IGameSaveBlobInfo = ^IVectorView_1__XboxLive_Storage_IGameSaveBlobInfo;

  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult
  XboxLive_Storage_IGameSaveBlobInfoGetResult = interface;
  PXboxLive_Storage_IGameSaveBlobInfoGetResult = ^XboxLive_Storage_IGameSaveBlobInfoGetResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult = interface;
  PAsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult = ^AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult = interface;
  PIAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult = ^IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  AsyncOperationCompletedHandler_1__Cardinal = interface;
  PAsyncOperationCompletedHandler_1__Cardinal = ^AsyncOperationCompletedHandler_1__Cardinal;

  // Windows.Foundation.IAsyncOperation`1<UInt32>
  IAsyncOperation_1__Cardinal = interface;
  PIAsyncOperation_1__Cardinal = ^IAsyncOperation_1__Cardinal;

  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoQuery
  XboxLive_Storage_IGameSaveBlobInfoQuery = interface;
  PXboxLive_Storage_IGameSaveBlobInfoQuery = ^XboxLive_Storage_IGameSaveBlobInfoQuery;

  // Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult
  XboxLive_Storage_IGameSaveOperationResult = interface;
  PXboxLive_Storage_IGameSaveOperationResult = ^XboxLive_Storage_IGameSaveOperationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult = interface;
  PAsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult = ^AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult = interface;
  PIAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult = ^IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult;

  // Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo
  XboxLive_Storage_IGameSaveContainerInfo = interface;
  PXboxLive_Storage_IGameSaveContainerInfo = ^XboxLive_Storage_IGameSaveContainerInfo;

  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IIterator_1__XboxLive_Storage_IGameSaveContainerInfo = interface;
  PIIterator_1__XboxLive_Storage_IGameSaveContainerInfo = ^IIterator_1__XboxLive_Storage_IGameSaveContainerInfo;

  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IIterable_1__XboxLive_Storage_IGameSaveContainerInfo = interface;
  PIIterable_1__XboxLive_Storage_IGameSaveContainerInfo = ^IIterable_1__XboxLive_Storage_IGameSaveContainerInfo;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IVectorView_1__XboxLive_Storage_IGameSaveContainerInfo = interface;
  PIVectorView_1__XboxLive_Storage_IGameSaveContainerInfo = ^IVectorView_1__XboxLive_Storage_IGameSaveContainerInfo;

  // Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult
  XboxLive_Storage_IGameSaveContainerInfoGetResult = interface;
  PXboxLive_Storage_IGameSaveContainerInfoGetResult = ^XboxLive_Storage_IGameSaveContainerInfoGetResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult = interface;
  PAsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult = ^AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult = interface;
  PIAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult = ^IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult;

  // Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoQuery
  XboxLive_Storage_IGameSaveContainerInfoQuery = interface;
  PXboxLive_Storage_IGameSaveContainerInfoQuery = ^XboxLive_Storage_IGameSaveContainerInfoQuery;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Int64>
  AsyncOperationCompletedHandler_1__Int64 = interface;
  PAsyncOperationCompletedHandler_1__Int64 = ^AsyncOperationCompletedHandler_1__Int64;

  // Windows.Foundation.IAsyncOperation`1<Int64>
  IAsyncOperation_1__Int64 = interface;
  PIAsyncOperation_1__Int64 = ^IAsyncOperation_1__Int64;

  // Windows.Gaming.XboxLive.Storage.IGameSaveProvider
  XboxLive_Storage_IGameSaveProvider = interface;
  PXboxLive_Storage_IGameSaveProvider = ^XboxLive_Storage_IGameSaveProvider;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult = interface;
  PAsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult = ^AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult = interface;
  PIAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult = ^IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult;

  // Windows.Gaming.XboxLive.Storage.IGameSaveContainer
  XboxLive_Storage_IGameSaveContainer = interface;
  PXboxLive_Storage_IGameSaveContainer = ^XboxLive_Storage_IGameSaveContainer;

  // Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult
  XboxLive_Storage_IGameSaveProviderGetResult = interface;
  PXboxLive_Storage_IGameSaveProviderGetResult = ^XboxLive_Storage_IGameSaveProviderGetResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult = interface;
  PAsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult = ^AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult = interface;
  PIAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult = ^IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult;

  // Windows.Gaming.XboxLive.Storage.IGameSaveProviderStatics
  XboxLive_Storage_IGameSaveProviderStatics = interface;
  PXboxLive_Storage_IGameSaveProviderStatics = ^XboxLive_Storage_IGameSaveProviderStatics;

  // Windows.Gaming Enums

  // Windows.Gaming.Input.ArcadeStickButtons
  Input_ArcadeStickButtons = (
    None = 0,
    StickUp = 1,
    StickDown = 2,
    StickLeft = 4,
    StickRight = 8,
    Action1 = 16,
    Action2 = 32,
    Action3 = 64,
    Action4 = 128,
    Action5 = 256,
    Action6 = 512,
    Special1 = 1024,
    Special2 = 2048
  );
  PInput_ArcadeStickButtons = ^Input_ArcadeStickButtons;

  // Windows.Gaming.Input.Custom.GipFirmwareUpdateStatus
  Input_Custom_GipFirmwareUpdateStatus = (
    Completed = 0,
    UpToDate = 1,
    Failed = 2
  );
  PInput_Custom_GipFirmwareUpdateStatus = ^Input_Custom_GipFirmwareUpdateStatus;

  // Windows.Gaming.Input.Custom.GipMessageClass
  Input_Custom_GipMessageClass = (
    Command = 0,
    LowLatency = 1,
    StandardLatency = 2
  );
  PInput_Custom_GipMessageClass = ^Input_Custom_GipMessageClass;

  // Windows.Gaming.Input.Custom.XusbDeviceSubtype
  Input_Custom_XusbDeviceSubtype = (
    Unknown = 0,
    Gamepad = 1,
    ArcadePad = 2,
    ArcadeStick = 3,
    FlightStick = 4,
    Wheel = 5,
    Guitar = 6,
    GuitarAlternate = 7,
    GuitarBass = 8,
    DrumKit = 9,
    DancePad = 10
  );
  PInput_Custom_XusbDeviceSubtype = ^Input_Custom_XusbDeviceSubtype;

  // Windows.Gaming.Input.Custom.XusbDeviceType
  Input_Custom_XusbDeviceType = (
    Unknown = 0,
    Gamepad = 1
  );
  PInput_Custom_XusbDeviceType = ^Input_Custom_XusbDeviceType;

  // Windows.Gaming.Input.FlightStickButtons
  Input_FlightStickButtons = (
    None = 0,
    FirePrimary = 1,
    FireSecondary = 2
  );
  PInput_FlightStickButtons = ^Input_FlightStickButtons;

  // Windows.Gaming.Input.ForceFeedback.ConditionForceEffectKind
  Input_ForceFeedback_ConditionForceEffectKind = (
    Spring = 0,
    Damper = 1,
    Inertia = 2,
    Friction = 3
  );
  PInput_ForceFeedback_ConditionForceEffectKind = ^Input_ForceFeedback_ConditionForceEffectKind;

  // Windows.Gaming.Input.ForceFeedback.ForceFeedbackEffectAxes
  Input_ForceFeedback_ForceFeedbackEffectAxes = (
    None = 0,
    X = 1,
    Y = 2,
    Z = 4
  );
  PInput_ForceFeedback_ForceFeedbackEffectAxes = ^Input_ForceFeedback_ForceFeedbackEffectAxes;

  // Windows.Gaming.Input.ForceFeedback.ForceFeedbackEffectState
  Input_ForceFeedback_ForceFeedbackEffectState = (
    Stopped = 0,
    Running = 1,
    Paused = 2,
    Faulted = 3
  );
  PInput_ForceFeedback_ForceFeedbackEffectState = ^Input_ForceFeedback_ForceFeedbackEffectState;

  // Windows.Gaming.Input.ForceFeedback.ForceFeedbackLoadEffectResult
  Input_ForceFeedback_ForceFeedbackLoadEffectResult = (
    Succeeded = 0,
    EffectStorageFull = 1,
    EffectNotSupported = 2
  );
  PInput_ForceFeedback_ForceFeedbackLoadEffectResult = ^Input_ForceFeedback_ForceFeedbackLoadEffectResult;

  // Windows.Gaming.Input.ForceFeedback.PeriodicForceEffectKind
  Input_ForceFeedback_PeriodicForceEffectKind = (
    SquareWave = 0,
    SineWave = 1,
    TriangleWave = 2,
    SawtoothWaveUp = 3,
    SawtoothWaveDown = 4
  );
  PInput_ForceFeedback_PeriodicForceEffectKind = ^Input_ForceFeedback_PeriodicForceEffectKind;

  // Windows.Gaming.Input.GameControllerButtonLabel
  Input_GameControllerButtonLabel = (
    None = 0,
    XboxBack = 1,
    XboxStart = 2,
    XboxMenu = 3,
    XboxView = 4,
    XboxUp = 5,
    XboxDown = 6,
    XboxLeft = 7,
    XboxRight = 8,
    XboxA = 9,
    XboxB = 10,
    XboxX = 11,
    XboxY = 12,
    XboxLeftBumper = 13,
    XboxLeftTrigger = 14,
    XboxLeftStickButton = 15,
    XboxRightBumper = 16,
    XboxRightTrigger = 17,
    XboxRightStickButton = 18,
    XboxPaddle1 = 19,
    XboxPaddle2 = 20,
    XboxPaddle3 = 21,
    XboxPaddle4 = 22,
    Mode = 23,
    Select = 24,
    Menu = 25,
    View = 26,
    Back = 27,
    Start = 28,
    Options = 29,
    Share = 30,
    Up = 31,
    Down = 32,
    Left = 33,
    Right = 34,
    LetterA = 35,
    LetterB = 36,
    LetterC = 37,
    LetterL = 38,
    LetterR = 39,
    LetterX = 40,
    LetterY = 41,
    LetterZ = 42,
    Cross = 43,
    Circle = 44,
    Square = 45,
    Triangle = 46,
    LeftBumper = 47,
    LeftTrigger = 48,
    LeftStickButton = 49,
    Left1 = 50,
    Left2 = 51,
    Left3 = 52,
    RightBumper = 53,
    RightTrigger = 54,
    RightStickButton = 55,
    Right1 = 56,
    Right2 = 57,
    Right3 = 58,
    Paddle1 = 59,
    Paddle2 = 60,
    Paddle3 = 61,
    Paddle4 = 62,
    Plus = 63,
    Minus = 64,
    DownLeftArrow = 65,
    DialLeft = 66,
    DialRight = 67,
    Suspension = 68
  );
  PInput_GameControllerButtonLabel = ^Input_GameControllerButtonLabel;

  // Windows.Gaming.Input.GameControllerSwitchKind
  Input_GameControllerSwitchKind = (
    TwoWay = 0,
    FourWay = 1,
    EightWay = 2
  );
  PInput_GameControllerSwitchKind = ^Input_GameControllerSwitchKind;

  // Windows.Gaming.Input.GameControllerSwitchPosition
  Input_GameControllerSwitchPosition = (
    Center = 0,
    Up = 1,
    UpRight = 2,
    Right = 3,
    DownRight = 4,
    Down = 5,
    DownLeft = 6,
    Left = 7,
    UpLeft = 8
  );
  PInput_GameControllerSwitchPosition = ^Input_GameControllerSwitchPosition;

  // Windows.Gaming.Input.GamepadButtons
  Input_GamepadButtons = (
    None = 0,
    Menu = 1,
    View = 2,
    A = 4,
    B = 8,
    X = 16,
    Y = 32,
    DPadUp = 64,
    DPadDown = 128,
    DPadLeft = 256,
    DPadRight = 512,
    LeftShoulder = 1024,
    RightShoulder = 2048,
    LeftThumbstick = 4096,
    RightThumbstick = 8192,
    Paddle1 = 16384,
    Paddle2 = 32768,
    Paddle3 = 65536,
    Paddle4 = 131072
  );
  PInput_GamepadButtons = ^Input_GamepadButtons;

  // Windows.Gaming.Input.OptionalUINavigationButtons
  Input_OptionalUINavigationButtons = (
    None = 0,
    Context1 = 1,
    Context2 = 2,
    Context3 = 4,
    Context4 = 8,
    PageUp = 16,
    PageDown = 32,
    PageLeft = 64,
    PageRight = 128,
    ScrollUp = 256,
    ScrollDown = 512,
    ScrollLeft = 1024,
    ScrollRight = 2048
  );
  PInput_OptionalUINavigationButtons = ^Input_OptionalUINavigationButtons;

  // Windows.Gaming.Input.RacingWheelButtons
  Input_RacingWheelButtons = (
    None = 0,
    PreviousGear = 1,
    NextGear = 2,
    DPadUp = 4,
    DPadDown = 8,
    DPadLeft = 16,
    DPadRight = 32,
    Button1 = 64,
    Button2 = 128,
    Button3 = 256,
    Button4 = 512,
    Button5 = 1024,
    Button6 = 2048,
    Button7 = 4096,
    Button8 = 8192,
    Button9 = 16384,
    Button10 = 32768,
    Button11 = 65536,
    Button12 = 131072,
    Button13 = 262144,
    Button14 = 524288,
    Button15 = 1048576,
    Button16 = 2097152
  );
  PInput_RacingWheelButtons = ^Input_RacingWheelButtons;

  // Windows.Gaming.Input.RequiredUINavigationButtons
  Input_RequiredUINavigationButtons = (
    None = 0,
    Menu = 1,
    View = 2,
    Accept = 4,
    Cancel = 8,
    Up = 16,
    Down = 32,
    Left = 64,
    Right = 128
  );
  PInput_RequiredUINavigationButtons = ^Input_RequiredUINavigationButtons;

  // Windows.Gaming.Preview.GamesEnumeration.GameListCategory
  Preview_GamesEnumeration_GameListCategory = (
    Candidate = 0,
    ConfirmedBySystem = 1,
    ConfirmedByUser = 2
  );
  PPreview_GamesEnumeration_GameListCategory = ^Preview_GamesEnumeration_GameListCategory;

  // Windows.Gaming.Preview.GamesEnumeration.GameListEntryLaunchableState
  Preview_GamesEnumeration_GameListEntryLaunchableState = (
    NotLaunchable = 0,
    ByLastRunningFullPath = 1,
    ByUserProvidedPath = 2,
    ByTile = 3
  );
  PPreview_GamesEnumeration_GameListEntryLaunchableState = ^Preview_GamesEnumeration_GameListEntryLaunchableState;

  // Windows.Gaming.UI.GameChatMessageOrigin
  UI_GameChatMessageOrigin = (
    Voice = 0,
    Text = 1
  );
  PUI_GameChatMessageOrigin = ^UI_GameChatMessageOrigin;

  // Windows.Gaming.UI.GameChatOverlayPosition
  UI_GameChatOverlayPosition = (
    BottomCenter = 0,
    BottomLeft = 1,
    BottomRight = 2,
    MiddleRight = 3,
    MiddleLeft = 4,
    TopCenter = 5,
    TopLeft = 6,
    TopRight = 7
  );
  PUI_GameChatOverlayPosition = ^UI_GameChatOverlayPosition;

  // Windows.Gaming.XboxLive.Storage.GameSaveErrorStatus
  XboxLive_Storage_GameSaveErrorStatus = (
    Ok = 0,
    Abort = -2147467260,
    InvalidContainerName = -2138898431,
    NoAccess = -2138898430,
    OutOfLocalStorage = -2138898429,
    UserCanceled = -2138898428,
    UpdateTooBig = -2138898427,
    QuotaExceeded = -2138898426,
    ProvidedBufferTooSmall = -2138898425,
    BlobNotFound = -2138898424,
    NoXboxLiveInfo = -2138898423,
    ContainerNotInSync = -2138898422,
    ContainerSyncFailed = -2138898421,
    UserHasNoXboxLiveInfo = -2138898420,
    ObjectExpired = -2138898419
  );
  PXboxLive_Storage_GameSaveErrorStatus = ^XboxLive_Storage_GameSaveErrorStatus;

  // Windows.Gaming Records
  // Windows.Gaming.Input.ArcadeStickReading
  Input_ArcadeStickReading = record
    Timestamp: UInt64;
    Buttons: Input_ArcadeStickButtons;
  end;
  PInput_ArcadeStickReading = ^Input_ArcadeStickReading;

  // Windows.Gaming.Input.Custom.GameControllerVersionInfo
  Input_Custom_GameControllerVersionInfo = record
    Major: Word;
    Minor: Word;
    Build: Word;
    Revision: Word;
  end;
  PInput_Custom_GameControllerVersionInfo = ^Input_Custom_GameControllerVersionInfo;

  // Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress
  Input_Custom_GipFirmwareUpdateProgress = record
    PercentCompleted: Double;
    CurrentComponentId: Cardinal;
  end;
  PInput_Custom_GipFirmwareUpdateProgress = ^Input_Custom_GipFirmwareUpdateProgress;

  // Windows.Gaming.Input.FlightStickReading
  Input_FlightStickReading = record
    Timestamp: UInt64;
    Buttons: Input_FlightStickButtons;
    HatSwitch: Input_GameControllerSwitchPosition;
    Roll: Double;
    Pitch: Double;
    Yaw: Double;
    Throttle: Double;
  end;
  PInput_FlightStickReading = ^Input_FlightStickReading;

  // Windows.Gaming.Input.GamepadReading
  Input_GamepadReading = record
    Timestamp: UInt64;
    Buttons: Input_GamepadButtons;
    LeftTrigger: Double;
    RightTrigger: Double;
    LeftThumbstickX: Double;
    LeftThumbstickY: Double;
    RightThumbstickX: Double;
    RightThumbstickY: Double;
  end;
  PInput_GamepadReading = ^Input_GamepadReading;

  // Windows.Gaming.Input.GamepadVibration
  Input_GamepadVibration = record
    LeftMotor: Double;
    RightMotor: Double;
    LeftTrigger: Double;
    RightTrigger: Double;
  end;
  PInput_GamepadVibration = ^Input_GamepadVibration;

  // Windows.Gaming.Input.GamingInputPreviewContract
  Input_GamingInputPreviewContract = record
  end;
  PInput_GamingInputPreviewContract = ^Input_GamingInputPreviewContract;

  // Windows.Gaming.Input.RacingWheelReading
  Input_RacingWheelReading = record
    Timestamp: UInt64;
    Buttons: Input_RacingWheelButtons;
    PatternShifterGear: Integer;
    Wheel: Double;
    Throttle: Double;
    Brake: Double;
    Clutch: Double;
    Handbrake: Double;
  end;
  PInput_RacingWheelReading = ^Input_RacingWheelReading;

  // Windows.Gaming.Input.UINavigationReading
  Input_UINavigationReading = record
    Timestamp: UInt64;
    RequiredButtons: Input_RequiredUINavigationButtons;
    OptionalButtons: Input_OptionalUINavigationButtons;
  end;
  PInput_UINavigationReading = ^Input_UINavigationReading;

  // Windows.Gaming.Preview.GamesEnumerationContract
  Preview_GamesEnumerationContract = record
  end;
  PPreview_GamesEnumerationContract = ^Preview_GamesEnumerationContract;

  // Windows.Gaming.UI.GameChatOverlayContract
  UI_GameChatOverlayContract = record
  end;
  PUI_GameChatOverlayContract = ^UI_GameChatOverlayContract;

  // Windows.Gaming.UI.GamingUIProviderContract
  UI_GamingUIProviderContract = record
  end;
  PUI_GamingUIProviderContract = ^UI_GamingUIProviderContract;

  // Windows.Gaming.XboxLive.StorageApiContract
  XboxLive_StorageApiContract = record
  end;
  PXboxLive_StorageApiContract = ^XboxLive_StorageApiContract;

  // Windows.Gaming Interfaces

  // UsedAPI Interface
  // Windows.Gaming.Input.Custom.IGameControllerProvider
  Input_Custom_IGameControllerProvider = interface(IInspectable)
  ['{E6D73982-2996-4559-B16C-3E57D46E58D6}']
    function get_FirmwareVersionInfo: Input_Custom_GameControllerVersionInfo; safecall;
    function get_HardwareProductId: Word; safecall;
    function get_HardwareVendorId: Word; safecall;
    function get_HardwareVersionInfo: Input_Custom_GameControllerVersionInfo; safecall;
    function get_IsConnected: Boolean; safecall;
    property FirmwareVersionInfo: Input_Custom_GameControllerVersionInfo read get_FirmwareVersionInfo;
    property HardwareProductId: Word read get_HardwareProductId;
    property HardwareVendorId: Word read get_HardwareVendorId;
    property HardwareVersionInfo: Input_Custom_GameControllerVersionInfo read get_HardwareVersionInfo;
    property IsConnected: Boolean read get_IsConnected;
  end;

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

  // UsedAPI Interface
  // Windows.Gaming.Input.Custom.ICustomGameControllerFactory
  Input_Custom_ICustomGameControllerFactory = interface(IInspectable)
  ['{69A0AE5E-758E-4CBE-ACE6-62155FE9126F}']
    function CreateGameController(provider: Input_Custom_IGameControllerProvider): IInspectable; safecall;
    procedure OnGameControllerAdded(value: Input_IGameController); safecall;
    procedure OnGameControllerRemoved(value: Input_IGameController); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.Custom.IGameControllerFactoryManagerStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_Custom_GameControllerFactoryManager)]
  Input_Custom_IGameControllerFactoryManagerStatics = interface(IInspectable)
  ['{36CB66E3-D0A1-4986-A24C-40B137DEBA9E}']
    procedure RegisterCustomFactoryForGipInterface(factory: Input_Custom_ICustomGameControllerFactory; interfaceId: TGuid); safecall;
    procedure RegisterCustomFactoryForHardwareId(factory: Input_Custom_ICustomGameControllerFactory; hardwareVendorId: Word; hardwareProductId: Word); safecall;
    procedure RegisterCustomFactoryForXusbType(factory: Input_Custom_ICustomGameControllerFactory; xusbType: Input_Custom_XusbDeviceType; xusbSubtype: Input_Custom_XusbDeviceSubtype); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.Custom.IGameControllerFactoryManagerStatics2
  [WinRTClassNameAttribute(SWindows_Gaming_Input_Custom_GameControllerFactoryManager)]
  Input_Custom_IGameControllerFactoryManagerStatics2 = interface(IInspectable)
  ['{EACE5644-19DF-4115-B32A-2793E2AEA3BB}']
    function TryGetFactoryControllerFromGameController(factory: Input_Custom_ICustomGameControllerFactory; gameController: Input_IGameController): Input_IGameController; safecall;
  end;

  // Windows.Gaming.Input.Custom.IGameControllerInputSink
  Input_Custom_IGameControllerInputSink = interface(IInspectable)
  ['{1FF6F922-C640-4C78-A820-9A715C558BCB}']
    procedure OnInputResumed(timestamp: UInt64); safecall;
    procedure OnInputSuspended(timestamp: UInt64); safecall;
  end;

  // Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult
  Input_Custom_IGipFirmwareUpdateResult = interface(IInspectable)
  ['{6B794D32-8553-4292-8E03-E16651A2F8BC}']
    function get_ExtendedErrorCode: Cardinal; safecall;
    function get_FinalComponentId: Cardinal; safecall;
    function get_Status: Input_Custom_GipFirmwareUpdateStatus; safecall;
    property ExtendedErrorCode: Cardinal read get_ExtendedErrorCode;
    property FinalComponentId: Cardinal read get_FinalComponentId;
    property Status: Input_Custom_GipFirmwareUpdateStatus read get_Status;
  end;

  // Windows.Gaming.Input.Custom.IGipGameControllerInputSink
  Input_Custom_IGipGameControllerInputSink = interface(IInspectable)
  ['{A2108ABF-09F1-43BC-A140-80F899EC36FB}']
    procedure OnKeyReceived(timestamp: UInt64; keyCode: Byte; isPressed: Boolean); safecall;
    procedure OnMessageReceived(timestamp: UInt64; messageClass: Input_Custom_GipMessageClass; messageId: Byte; sequenceId: Byte; messageBufferSize: Cardinal; messageBuffer: PByte); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress_Delegate_Base = interface(IUnknown)
  ['{065C16AF-49DC-5C94-AFE2-9385937FACC9}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress; progressInfo: Input_Custom_GipFirmwareUpdateProgress); safecall;
  end;
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = interface(AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress_Delegate_Base)
  ['{534AC4CE-A902-5627-AEF9-3FA985AAC4FB}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress_Delegate_Base = interface(IUnknown)
  ['{61B95949-A027-51D8-9F33-37927451502B}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = interface(AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress_Delegate_Base)
  ['{A0AB214A-F835-5769-A534-251D008FA3EF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress_Base = interface(IInspectable)
  ['{BFAA48BD-155F-5112-BD86-E01D6F7CD405}']
    procedure put_Progress(handler: AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress); safecall;
    function get_Progress: AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress; safecall;
    procedure put_Completed(handler: AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress); safecall;
    function get_Completed: AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress; safecall;
    function GetResults: Input_Custom_IGipFirmwareUpdateResult; safecall;
    property Progress: AsyncOperationProgressHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress read get_Progress write put_Progress;
    property Completed: AsyncOperationWithProgressCompletedHandler_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Gaming.Input.Custom.IGipFirmwareUpdateResult,Windows.Gaming.Input.Custom.GipFirmwareUpdateProgress>
  IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress = interface(IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress_Base)
  ['{FEB8360A-2497-5ABA-A1C9-518FFFDE150D}']
  end;

  // Windows.Gaming.Input.Custom.IGipGameControllerProvider
  Input_Custom_IGipGameControllerProvider = interface(IInspectable)
  ['{DBCF1E19-1AF5-45A8-BF02-A0EE50C823FC}']
    procedure SendMessage(messageClass: Input_Custom_GipMessageClass; messageId: Byte; messageBufferSize: Cardinal; messageBuffer: PByte); safecall;
    procedure SendReceiveMessage(messageClass: Input_Custom_GipMessageClass; messageId: Byte; requestMessageBufferSize: Cardinal; requestMessageBuffer: PByte; responseMessageBufferSize: Cardinal; responseMessageBuffer: PByte); safecall;
    function UpdateFirmwareAsync(firmwareImage: IInputStream): IAsyncOperationWithProgress_2__Input_Custom_IGipFirmwareUpdateResult__Input_Custom_GipFirmwareUpdateProgress; safecall;
  end;

  // Windows.Gaming.Input.Custom.IHidGameControllerInputSink
  Input_Custom_IHidGameControllerInputSink = interface(IInspectable)
  ['{F754C322-182D-40E4-A126-FCEE4FFA1E31}']
    procedure OnInputReportReceived(timestamp: UInt64; reportId: Byte; reportBufferSize: Cardinal; reportBuffer: PByte); safecall;
  end;

  // Windows.Gaming.Input.Custom.IHidGameControllerProvider
  Input_Custom_IHidGameControllerProvider = interface(IInspectable)
  ['{95CE3AF4-ABF0-4B68-A081-3B7DE73FF0E7}']
    function get_UsageId: Word; safecall;
    function get_UsagePage: Word; safecall;
    procedure GetFeatureReport(reportId: Byte; reportBufferSize: Cardinal; reportBuffer: PByte); safecall;
    procedure SendFeatureReport(reportId: Byte; reportBufferSize: Cardinal; reportBuffer: PByte); safecall;
    procedure SendOutputReport(reportId: Byte; reportBufferSize: Cardinal; reportBuffer: PByte); safecall;
    property UsageId: Word read get_UsageId;
    property UsagePage: Word read get_UsagePage;
  end;

  // Windows.Gaming.Input.Custom.IXusbGameControllerInputSink
  Input_Custom_IXusbGameControllerInputSink = interface(IInspectable)
  ['{B2AC1D95-6ECB-42B3-8AAB-025401CA4712}']
    procedure OnInputReceived(timestamp: UInt64; reportId: Byte; inputBufferSize: Cardinal; inputBuffer: PByte); safecall;
  end;

  // Windows.Gaming.Input.Custom.IXusbGameControllerProvider
  Input_Custom_IXusbGameControllerProvider = interface(IInspectable)
  ['{6E2971EB-0EFB-48B4-808B-837643B2F216}']
    procedure SetVibration(lowFrequencyMotorSpeed: Double; highFrequencyMotorSpeed: Double); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IForceFeedbackEffect
  Input_ForceFeedback_IForceFeedbackEffect = interface(IInspectable)
  ['{A17FBA0C-2AE4-48C2-8063-EABD0777CB89}']
    function get_Gain: Double; safecall;
    procedure put_Gain(value: Double); safecall;
    function get_State: Input_ForceFeedback_ForceFeedbackEffectState; safecall;
    procedure Start; safecall;
    procedure Stop; safecall;
    property Gain: Double read get_Gain write put_Gain;
    property State: Input_ForceFeedback_ForceFeedbackEffectState read get_State;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IConditionForceEffect
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ForceFeedback_ConditionForceEffect)]
  Input_ForceFeedback_IConditionForceEffect = interface(IInspectable)
  ['{32D1EA68-3695-4E69-85C0-CD1944189140}']
    function get_Kind: Input_ForceFeedback_ConditionForceEffectKind; safecall;
    procedure SetParameters(direction: Numerics_Vector3; positiveCoefficient: Single; negativeCoefficient: Single; maxPositiveMagnitude: Single; maxNegativeMagnitude: Single; deadZone: Single; bias: Single); safecall;
    property Kind: Input_ForceFeedback_ConditionForceEffectKind read get_Kind;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IConditionForceEffectFactory
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ForceFeedback_ConditionForceEffect)]
  Input_ForceFeedback_IConditionForceEffectFactory = interface(IInspectable)
  ['{91A99264-1810-4EB6-A773-BFD3B8CDDBAB}']
    function CreateInstance(effectKind: Input_ForceFeedback_ConditionForceEffectKind): Input_ForceFeedback_IForceFeedbackEffect; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IConstantForceEffect
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ForceFeedback_ConstantForceEffect)]
  Input_ForceFeedback_IConstantForceEffect = interface(IInspectable)
  ['{9BFA0140-F3C7-415C-B068-0F068734BCE0}']
    procedure SetParameters(vector: Numerics_Vector3; duration: TimeSpan); safecall;
    procedure SetParametersWithEnvelope(vector: Numerics_Vector3; attackGain: Single; sustainGain: Single; releaseGain: Single; startDelay: TimeSpan; attackDuration: TimeSpan; sustainDuration: TimeSpan; releaseDuration: TimeSpan; repeatCount: Cardinal); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.Input.ForceFeedback.ForceFeedbackLoadEffectResult>
  AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult_Delegate_Base = interface(IUnknown)
  ['{F8220A41-F738-51E8-89BA-76BBD66158CB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.Input.ForceFeedback.ForceFeedbackLoadEffectResult>
  AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult = interface(AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult_Delegate_Base)
  ['{E08EC1E0-E4AE-55A1-9A15-180859E0FA0F}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.Input.ForceFeedback.ForceFeedbackLoadEffectResult>
  IAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult_Base = interface(IInspectable)
  ['{21F834FC-E845-5AB9-BF85-9534E2397798}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult; safecall;
    function GetResults: Input_ForceFeedback_ForceFeedbackLoadEffectResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.Input.ForceFeedback.ForceFeedbackLoadEffectResult>
  IAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult = interface(IAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult_Base)
  ['{CA76FC11-A2C1-513E-B837-B4E39C42DC6B}']
  end;

  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor
  Input_ForceFeedback_IForceFeedbackMotor = interface(IInspectable)
  ['{8D3D417C-A5EA-4516-8026-2B00F74EF6E5}']
    function get_AreEffectsPaused: Boolean; safecall;
    function get_MasterGain: Double; safecall;
    procedure put_MasterGain(value: Double); safecall;
    function get_IsEnabled: Boolean; safecall;
    function get_SupportedAxes: Input_ForceFeedback_ForceFeedbackEffectAxes; safecall;
    function LoadEffectAsync(effect: Input_ForceFeedback_IForceFeedbackEffect): IAsyncOperation_1__Input_ForceFeedback_ForceFeedbackLoadEffectResult; safecall;
    procedure PauseAllEffects; safecall;
    procedure ResumeAllEffects; safecall;
    procedure StopAllEffects; safecall;
    function TryDisableAsync: IAsyncOperation_1__Boolean; safecall;
    function TryEnableAsync: IAsyncOperation_1__Boolean; safecall;
    function TryResetAsync: IAsyncOperation_1__Boolean; safecall;
    function TryUnloadEffectAsync(effect: Input_ForceFeedback_IForceFeedbackEffect): IAsyncOperation_1__Boolean; safecall;
    property AreEffectsPaused: Boolean read get_AreEffectsPaused;
    property IsEnabled: Boolean read get_IsEnabled;
    property MasterGain: Double read get_MasterGain write put_MasterGain;
    property SupportedAxes: Input_ForceFeedback_ForceFeedbackEffectAxes read get_SupportedAxes;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IPeriodicForceEffect
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ForceFeedback_PeriodicForceEffect)]
  Input_ForceFeedback_IPeriodicForceEffect = interface(IInspectable)
  ['{5C5138D7-FC75-4D52-9A0A-EFE4CAB5FE64}']
    function get_Kind: Input_ForceFeedback_PeriodicForceEffectKind; safecall;
    procedure SetParameters(vector: Numerics_Vector3; frequency: Single; phase: Single; bias: Single; duration: TimeSpan); safecall;
    procedure SetParametersWithEnvelope(vector: Numerics_Vector3; frequency: Single; phase: Single; bias: Single; attackGain: Single; sustainGain: Single; releaseGain: Single; startDelay: TimeSpan; attackDuration: TimeSpan; sustainDuration: TimeSpan; releaseDuration: TimeSpan; repeatCount: Cardinal); safecall;
    property Kind: Input_ForceFeedback_PeriodicForceEffectKind read get_Kind;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IPeriodicForceEffectFactory
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ForceFeedback_PeriodicForceEffect)]
  Input_ForceFeedback_IPeriodicForceEffectFactory = interface(IInspectable)
  ['{6F62EB1A-9851-477B-B318-35ECAA15070F}']
    function CreateInstance(effectKind: Input_ForceFeedback_PeriodicForceEffectKind): Input_ForceFeedback_IForceFeedbackEffect; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.ForceFeedback.IRampForceEffect
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ForceFeedback_RampForceEffect)]
  Input_ForceFeedback_IRampForceEffect = interface(IInspectable)
  ['{F1F81259-1CA6-4080-B56D-B43F3354D052}']
    procedure SetParameters(startVector: Numerics_Vector3; endVector: Numerics_Vector3; duration: TimeSpan); safecall;
    procedure SetParametersWithEnvelope(startVector: Numerics_Vector3; endVector: Numerics_Vector3; attackGain: Single; sustainGain: Single; releaseGain: Single; startDelay: TimeSpan; attackDuration: TimeSpan; sustainDuration: TimeSpan; releaseDuration: TimeSpan; repeatCount: Cardinal); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IArcadeStick
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ArcadeStick)]
  Input_IArcadeStick = interface(IInspectable)
  ['{B14A539D-BEFB-4C81-8051-15ECF3B13036}']
    function GetButtonLabel(button: Input_ArcadeStickButtons): Input_GameControllerButtonLabel; safecall;
    function GetCurrentReading: Input_ArcadeStickReading; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IArcadeStick>
  EventHandler_1__Input_IArcadeStick_Delegate_Base = interface(IUnknown)
  ['{6AFB8188-D28D-539B-BB69-EA1763FB9920}']
    procedure Invoke(sender: IInspectable; args: Input_IArcadeStick); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IArcadeStick>
  EventHandler_1__Input_IArcadeStick = interface(EventHandler_1__Input_IArcadeStick_Delegate_Base)
  ['{4B147AFA-683A-5538-AC27-4B6304038AF5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IArcadeStick>
  IIterator_1__Input_IArcadeStick_Base = interface(IInspectable)
  ['{D30629AF-CC9D-52E1-8B1F-0FFA9629AFEE}']
    function get_Current: Input_IArcadeStick; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInput_IArcadeStick): Cardinal; safecall;
    property Current: Input_IArcadeStick read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IArcadeStick>
  IIterator_1__Input_IArcadeStick = interface(IIterator_1__Input_IArcadeStick_Base)
  ['{F3B8B1A9-08A5-505C-8524-6BB9CB670C4B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IArcadeStick>
  IIterable_1__Input_IArcadeStick_Base = interface(IInspectable)
  ['{9376F457-2DA5-544A-A409-C636F5D81C35}']
    function First: IIterator_1__Input_IArcadeStick; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IArcadeStick>
  IIterable_1__Input_IArcadeStick = interface(IIterable_1__Input_IArcadeStick_Base)
  ['{F44D0BBD-2861-53DA-9BF3-E08BB438BED2}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IArcadeStick>
  IVectorView_1__Input_IArcadeStick = interface(IInspectable)
  ['{BF4692FE-9C66-56C3-8FFD-FD46B1D31982}']
    function GetAt(index: Cardinal): Input_IArcadeStick; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IArcadeStick; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IArcadeStick): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IArcadeStickStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ArcadeStick)]
  Input_IArcadeStickStatics = interface(IInspectable)
  ['{5C37B8C8-37B1-4AD8-9458-200F1A30018E}']
    function add_ArcadeStickAdded(value: EventHandler_1__Input_IArcadeStick): EventRegistrationToken; safecall;
    procedure remove_ArcadeStickAdded(token: EventRegistrationToken); safecall;
    function add_ArcadeStickRemoved(value: EventHandler_1__Input_IArcadeStick): EventRegistrationToken; safecall;
    procedure remove_ArcadeStickRemoved(token: EventRegistrationToken); safecall;
    function get_ArcadeSticks: IVectorView_1__Input_IArcadeStick; safecall;
    property ArcadeSticks: IVectorView_1__Input_IArcadeStick read get_ArcadeSticks;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IArcadeStickStatics2
  [WinRTClassNameAttribute(SWindows_Gaming_Input_ArcadeStick)]
  Input_IArcadeStickStatics2 = interface(IInspectable)
  ['{52B5D744-BB86-445A-B59C-596F0E2A49DF}']
    function FromGameController(gameController: Input_IGameController): Input_IArcadeStick; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IFlightStick
  [WinRTClassNameAttribute(SWindows_Gaming_Input_FlightStick)]
  Input_IFlightStick = interface(IInspectable)
  ['{B4A2C01C-B83B-4459-A1A9-97B03C33DA7C}']
    function get_HatSwitchKind: Input_GameControllerSwitchKind; safecall;
    function GetButtonLabel(button: Input_FlightStickButtons): Input_GameControllerButtonLabel; safecall;
    function GetCurrentReading: Input_FlightStickReading; safecall;
    property HatSwitchKind: Input_GameControllerSwitchKind read get_HatSwitchKind;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IFlightStick>
  EventHandler_1__Input_IFlightStick_Delegate_Base = interface(IUnknown)
  ['{D57470B1-CC22-5A43-8E18-5CA064AAFE21}']
    procedure Invoke(sender: IInspectable; args: Input_IFlightStick); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IFlightStick>
  EventHandler_1__Input_IFlightStick = interface(EventHandler_1__Input_IFlightStick_Delegate_Base)
  ['{3FEDCF43-4D61-520C-8A48-D9BB1AE7C5FE}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IFlightStick>
  IIterator_1__Input_IFlightStick_Base = interface(IInspectable)
  ['{F5FA1919-3F18-5560-BB13-CF7018AC41D5}']
    function get_Current: Input_IFlightStick; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInput_IFlightStick): Cardinal; safecall;
    property Current: Input_IFlightStick read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IFlightStick>
  IIterator_1__Input_IFlightStick = interface(IIterator_1__Input_IFlightStick_Base)
  ['{2C67490F-3325-54D5-B002-99EA2100CD77}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IFlightStick>
  IIterable_1__Input_IFlightStick_Base = interface(IInspectable)
  ['{3B7FC175-BEBE-52EF-A3E9-DDA75EA1ACFC}']
    function First: IIterator_1__Input_IFlightStick; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IFlightStick>
  IIterable_1__Input_IFlightStick = interface(IIterable_1__Input_IFlightStick_Base)
  ['{676E26DA-0C68-5802-9996-F5D2061094E5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IFlightStick>
  IVectorView_1__Input_IFlightStick = interface(IInspectable)
  ['{6625E1F5-2BE6-55E4-8D29-E0A845C7F1E5}']
    function GetAt(index: Cardinal): Input_IFlightStick; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IFlightStick; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IFlightStick): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IFlightStickStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_FlightStick)]
  Input_IFlightStickStatics = interface(IInspectable)
  ['{5514924A-FECC-435E-83DC-5CEC8A18A520}']
    function add_FlightStickAdded(value: EventHandler_1__Input_IFlightStick): EventRegistrationToken; safecall;
    procedure remove_FlightStickAdded(token: EventRegistrationToken); safecall;
    function add_FlightStickRemoved(value: EventHandler_1__Input_IFlightStick): EventRegistrationToken; safecall;
    procedure remove_FlightStickRemoved(token: EventRegistrationToken); safecall;
    function get_FlightSticks: IVectorView_1__Input_IFlightStick; safecall;
    function FromGameController(gameController: Input_IGameController): Input_IFlightStick; safecall;
    property FlightSticks: IVectorView_1__Input_IFlightStick read get_FlightSticks;
  end;

  // Windows.Foundation.IReference`1<Int32>
  IReference_1__Integer = interface(IInspectable)
  ['{548CEFBD-BC8A-5FA0-8DF2-957440FC8BF4}']
    function get_Value: Integer; safecall;
    property Value: Integer read get_Value;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IGameControllerBatteryInfo
  Input_IGameControllerBatteryInfo = interface(IInspectable)
  ['{DCECC681-3963-4DA6-955D-553F3B6F6161}']
    function TryGetBatteryReport: Power_IBatteryReport; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IGamepad
  [WinRTClassNameAttribute(SWindows_Gaming_Input_Gamepad)]
  Input_IGamepad = interface(IInspectable)
  ['{BC7BB43C-0A69-3903-9E9D-A50F86A45DE5}']
    function get_Vibration: Input_GamepadVibration; safecall;
    procedure put_Vibration(value: Input_GamepadVibration); safecall;
    function GetCurrentReading: Input_GamepadReading; safecall;
    property Vibration: Input_GamepadVibration read get_Vibration write put_Vibration;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IGamepad2
  Input_IGamepad2 = interface(IInspectable)
  ['{3C1689BD-5915-4245-B0C0-C89FAE0308FF}']
    function GetButtonLabel(button: Input_GamepadButtons): Input_GameControllerButtonLabel; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IGamepad>
  EventHandler_1__Input_IGamepad_Delegate_Base = interface(IUnknown)
  ['{8A7639EE-624A-501A-BB53-562D1EC11B52}']
    procedure Invoke(sender: IInspectable; args: Input_IGamepad); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IGamepad>
  EventHandler_1__Input_IGamepad = interface(EventHandler_1__Input_IGamepad_Delegate_Base)
  ['{211C945C-F4C7-5499-8CDE-C2743AB66C7A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IGamepad>
  IIterator_1__Input_IGamepad_Base = interface(IInspectable)
  ['{246737E8-12BC-5C64-AF52-06DB4B13FA2F}']
    function get_Current: Input_IGamepad; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInput_IGamepad): Cardinal; safecall;
    property Current: Input_IGamepad read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IGamepad>
  IIterator_1__Input_IGamepad = interface(IIterator_1__Input_IGamepad_Base)
  ['{2A491677-2704-568C-811F-69E1DB9CF299}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IGamepad>
  IIterable_1__Input_IGamepad_Base = interface(IInspectable)
  ['{47132BA0-6B17-5CD2-A8BD-B5D3443CCB13}']
    function First: IIterator_1__Input_IGamepad; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IGamepad>
  IIterable_1__Input_IGamepad = interface(IIterable_1__Input_IGamepad_Base)
  ['{CDFC722B-193D-522F-A4F7-CB683ADD1D56}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IGamepad>
  IVectorView_1__Input_IGamepad = interface(IInspectable)
  ['{C4141A0C-2804-5C97-BACE-87A9751E957F}']
    function GetAt(index: Cardinal): Input_IGamepad; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IGamepad; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IGamepad): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IGamepadStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_Gamepad)]
  Input_IGamepadStatics = interface(IInspectable)
  ['{8BBCE529-D49C-39E9-9560-E47DDE96B7C8}']
    function add_GamepadAdded(value: EventHandler_1__Input_IGamepad): EventRegistrationToken; safecall;
    procedure remove_GamepadAdded(token: EventRegistrationToken); safecall;
    function add_GamepadRemoved(value: EventHandler_1__Input_IGamepad): EventRegistrationToken; safecall;
    procedure remove_GamepadRemoved(token: EventRegistrationToken); safecall;
    function get_Gamepads: IVectorView_1__Input_IGamepad; safecall;
    property Gamepads: IVectorView_1__Input_IGamepad read get_Gamepads;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IGamepadStatics2
  [WinRTClassNameAttribute(SWindows_Gaming_Input_Gamepad)]
  Input_IGamepadStatics2 = interface(IInspectable)
  ['{42676DC5-0856-47C4-9213-B395504C3A3C}']
    function FromGameController(gameController: Input_IGameController): Input_IGamepad; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IRacingWheel
  [WinRTClassNameAttribute(SWindows_Gaming_Input_RacingWheel)]
  Input_IRacingWheel = interface(IInspectable)
  ['{F546656F-E106-4C82-A90F-554012904B85}']
    function get_HasClutch: Boolean; safecall;
    function get_HasHandbrake: Boolean; safecall;
    function get_HasPatternShifter: Boolean; safecall;
    function get_MaxPatternShifterGear: Integer; safecall;
    function get_MaxWheelAngle: Double; safecall;
    function get_WheelMotor: Input_ForceFeedback_IForceFeedbackMotor; safecall;
    function GetButtonLabel(button: Input_RacingWheelButtons): Input_GameControllerButtonLabel; safecall;
    function GetCurrentReading: Input_RacingWheelReading; safecall;
    property HasClutch: Boolean read get_HasClutch;
    property HasHandbrake: Boolean read get_HasHandbrake;
    property HasPatternShifter: Boolean read get_HasPatternShifter;
    property MaxPatternShifterGear: Integer read get_MaxPatternShifterGear;
    property MaxWheelAngle: Double read get_MaxWheelAngle;
    property WheelMotor: Input_ForceFeedback_IForceFeedbackMotor read get_WheelMotor;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IRacingWheel>
  EventHandler_1__Input_IRacingWheel_Delegate_Base = interface(IUnknown)
  ['{352EC824-F64B-5353-80EA-7FF58E3B92A4}']
    procedure Invoke(sender: IInspectable; args: Input_IRacingWheel); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IRacingWheel>
  EventHandler_1__Input_IRacingWheel = interface(EventHandler_1__Input_IRacingWheel_Delegate_Base)
  ['{6D14150F-773D-5282-8FFA-413290302B14}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IRacingWheel>
  IIterator_1__Input_IRacingWheel_Base = interface(IInspectable)
  ['{23D735B8-4D36-5377-A245-69DF97C9FCD9}']
    function get_Current: Input_IRacingWheel; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInput_IRacingWheel): Cardinal; safecall;
    property Current: Input_IRacingWheel read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IRacingWheel>
  IIterator_1__Input_IRacingWheel = interface(IIterator_1__Input_IRacingWheel_Base)
  ['{D8DFB54F-D536-592D-B684-2FD0BDFD060E}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IRacingWheel>
  IIterable_1__Input_IRacingWheel_Base = interface(IInspectable)
  ['{9A7C3830-9A87-5287-A1E2-8A2AF29CF68C}']
    function First: IIterator_1__Input_IRacingWheel; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IRacingWheel>
  IIterable_1__Input_IRacingWheel = interface(IIterable_1__Input_IRacingWheel_Base)
  ['{EAF9E22B-DD7B-523A-A8B0-3BE4B2727112}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IRacingWheel>
  IVectorView_1__Input_IRacingWheel = interface(IInspectable)
  ['{F815F5EB-E0AA-50DC-8A74-C23A370985B9}']
    function GetAt(index: Cardinal): Input_IRacingWheel; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IRacingWheel; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IRacingWheel): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IRacingWheelStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_RacingWheel)]
  Input_IRacingWheelStatics = interface(IInspectable)
  ['{3AC12CD5-581B-4936-9F94-69F1E6514C7D}']
    function add_RacingWheelAdded(value: EventHandler_1__Input_IRacingWheel): EventRegistrationToken; safecall;
    procedure remove_RacingWheelAdded(token: EventRegistrationToken); safecall;
    function add_RacingWheelRemoved(value: EventHandler_1__Input_IRacingWheel): EventRegistrationToken; safecall;
    procedure remove_RacingWheelRemoved(token: EventRegistrationToken); safecall;
    function get_RacingWheels: IVectorView_1__Input_IRacingWheel; safecall;
    property RacingWheels: IVectorView_1__Input_IRacingWheel read get_RacingWheels;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IRacingWheelStatics2
  [WinRTClassNameAttribute(SWindows_Gaming_Input_RacingWheel)]
  Input_IRacingWheelStatics2 = interface(IInspectable)
  ['{E666BCAA-EDFD-4323-A9F6-3C384048D1ED}']
    function FromGameController(gameController: Input_IGameController): Input_IRacingWheel; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IIterator_1__Input_ForceFeedback_IForceFeedbackMotor_Base = interface(IInspectable)
  ['{64CF69E0-5464-5B72-BD4B-82F7C3D0386D}']
    function get_Current: Input_ForceFeedback_IForceFeedbackMotor; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInput_ForceFeedback_IForceFeedbackMotor): Cardinal; safecall;
    property Current: Input_ForceFeedback_IForceFeedbackMotor read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IIterator_1__Input_ForceFeedback_IForceFeedbackMotor = interface(IIterator_1__Input_ForceFeedback_IForceFeedbackMotor_Base)
  ['{AD6193CE-C0B8-5967-B0A3-09F657CB0E49}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IIterable_1__Input_ForceFeedback_IForceFeedbackMotor_Base = interface(IInspectable)
  ['{C14440D1-FEA0-5147-AED8-9B85239DA882}']
    function First: IIterator_1__Input_ForceFeedback_IForceFeedbackMotor; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IIterable_1__Input_ForceFeedback_IForceFeedbackMotor = interface(IIterable_1__Input_ForceFeedback_IForceFeedbackMotor_Base)
  ['{42F320D3-FFE7-585C-B493-7F989F85B9CC}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.ForceFeedback.IForceFeedbackMotor>
  IVectorView_1__Input_ForceFeedback_IForceFeedbackMotor = interface(IInspectable)
  ['{E59BD26B-2669-59C3-B136-933A7598F2E1}']
    function GetAt(index: Cardinal): Input_ForceFeedback_IForceFeedbackMotor; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_ForceFeedback_IForceFeedbackMotor; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_ForceFeedback_IForceFeedbackMotor): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IRawGameController
  [WinRTClassNameAttribute(SWindows_Gaming_Input_RawGameController)]
  Input_IRawGameController = interface(IInspectable)
  ['{7CAD6D91-A7E1-4F71-9A78-33E9C5DFEA62}']
    function get_AxisCount: Integer; safecall;
    function get_ButtonCount: Integer; safecall;
    function get_ForceFeedbackMotors: IVectorView_1__Input_ForceFeedback_IForceFeedbackMotor; safecall;
    function get_HardwareProductId: Word; safecall;
    function get_HardwareVendorId: Word; safecall;
    function get_SwitchCount: Integer; safecall;
    function GetButtonLabel(buttonIndex: Integer): Input_GameControllerButtonLabel; safecall;
    function GetCurrentReading(buttonArraySize: Cardinal; buttonArray: PBoolean; switchArraySize: Cardinal; switchArray: PInput_GameControllerSwitchPosition; axisArraySize: Cardinal; axisArray: PDouble): UInt64; safecall;
    function GetSwitchKind(switchIndex: Integer): Input_GameControllerSwitchKind; safecall;
    property AxisCount: Integer read get_AxisCount;
    property ButtonCount: Integer read get_ButtonCount;
    property ForceFeedbackMotors: IVectorView_1__Input_ForceFeedback_IForceFeedbackMotor read get_ForceFeedbackMotors;
    property HardwareProductId: Word read get_HardwareProductId;
    property HardwareVendorId: Word read get_HardwareVendorId;
    property SwitchCount: Integer read get_SwitchCount;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IRawGameController2
  Input_IRawGameController2 = interface(IInspectable)
  ['{43C0C035-BB73-4756-A787-3ED6BEA617BD}']
    function get_SimpleHapticsControllers: IVectorView_1__Haptics_ISimpleHapticsController; safecall;
    function get_NonRoamableId: HSTRING; safecall;
    function get_DisplayName: HSTRING; safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property NonRoamableId: HSTRING read get_NonRoamableId;
    property SimpleHapticsControllers: IVectorView_1__Haptics_ISimpleHapticsController read get_SimpleHapticsControllers;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IRawGameController>
  EventHandler_1__Input_IRawGameController_Delegate_Base = interface(IUnknown)
  ['{00621C22-42E8-529F-9270-836B32931D72}']
    procedure Invoke(sender: IInspectable; args: Input_IRawGameController); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IRawGameController>
  EventHandler_1__Input_IRawGameController = interface(EventHandler_1__Input_IRawGameController_Delegate_Base)
  ['{673B36CA-0AE7-5784-BF2C-7B090B784A37}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IRawGameController>
  IIterator_1__Input_IRawGameController_Base = interface(IInspectable)
  ['{51CC88DC-66FB-55EA-9A1B-AADCD71CC08E}']
    function get_Current: Input_IRawGameController; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInput_IRawGameController): Cardinal; safecall;
    property Current: Input_IRawGameController read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IRawGameController>
  IIterator_1__Input_IRawGameController = interface(IIterator_1__Input_IRawGameController_Base)
  ['{0742A503-16BC-5E4E-AA88-F96EFD04AEF4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IRawGameController>
  IIterable_1__Input_IRawGameController_Base = interface(IInspectable)
  ['{8F2F08CC-F4F4-5539-9357-1F07334D381F}']
    function First: IIterator_1__Input_IRawGameController; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IRawGameController>
  IIterable_1__Input_IRawGameController = interface(IIterable_1__Input_IRawGameController_Base)
  ['{0012D7B5-D0D9-57C6-ADC2-A1467840562A}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IRawGameController>
  IVectorView_1__Input_IRawGameController = interface(IInspectable)
  ['{7EF80DB4-B308-510E-8047-09763FDF8569}']
    function GetAt(index: Cardinal): Input_IRawGameController; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IRawGameController; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IRawGameController): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IRawGameControllerStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_RawGameController)]
  Input_IRawGameControllerStatics = interface(IInspectable)
  ['{EB8D0792-E95A-4B19-AFC7-0A59F8BF759E}']
    function add_RawGameControllerAdded(value: EventHandler_1__Input_IRawGameController): EventRegistrationToken; safecall;
    procedure remove_RawGameControllerAdded(token: EventRegistrationToken); safecall;
    function add_RawGameControllerRemoved(value: EventHandler_1__Input_IRawGameController): EventRegistrationToken; safecall;
    procedure remove_RawGameControllerRemoved(token: EventRegistrationToken); safecall;
    function get_RawGameControllers: IVectorView_1__Input_IRawGameController; safecall;
    function FromGameController(gameController: Input_IGameController): Input_IRawGameController; safecall;
    property RawGameControllers: IVectorView_1__Input_IRawGameController read get_RawGameControllers;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IUINavigationController
  [WinRTClassNameAttribute(SWindows_Gaming_Input_UINavigationController)]
  Input_IUINavigationController = interface(IInspectable)
  ['{E5AEEFDD-F50E-4A55-8CDC-D33229548175}']
    function GetCurrentReading: Input_UINavigationReading; safecall;
    function GetOptionalButtonLabel(button: Input_OptionalUINavigationButtons): Input_GameControllerButtonLabel; safecall;
    function GetRequiredButtonLabel(button: Input_RequiredUINavigationButtons): Input_GameControllerButtonLabel; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IUINavigationController>
  EventHandler_1__Input_IUINavigationController_Delegate_Base = interface(IUnknown)
  ['{9EAEC424-75C1-5871-8DA9-CE590C653045}']
    procedure Invoke(sender: IInspectable; args: Input_IUINavigationController); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Gaming.Input.IUINavigationController>
  EventHandler_1__Input_IUINavigationController = interface(EventHandler_1__Input_IUINavigationController_Delegate_Base)
  ['{AA0C60BE-5A29-5DEF-BCD9-FB19873F0B5A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IUINavigationController>
  IIterator_1__Input_IUINavigationController_Base = interface(IInspectable)
  ['{C10B2696-64A3-5262-BC4F-B741E5D5AFAB}']
    function get_Current: Input_IUINavigationController; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInput_IUINavigationController): Cardinal; safecall;
    property Current: Input_IUINavigationController read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.Input.IUINavigationController>
  IIterator_1__Input_IUINavigationController = interface(IIterator_1__Input_IUINavigationController_Base)
  ['{11ABA575-8631-5640-B441-6135C2B542C6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IUINavigationController>
  IIterable_1__Input_IUINavigationController_Base = interface(IInspectable)
  ['{8DEA85A0-0204-57DD-ABAD-90E37C0EF240}']
    function First: IIterator_1__Input_IUINavigationController; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.Input.IUINavigationController>
  IIterable_1__Input_IUINavigationController = interface(IIterable_1__Input_IUINavigationController_Base)
  ['{46480DAC-84C3-583B-A9ED-664EBB4D8B80}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.Input.IUINavigationController>
  IVectorView_1__Input_IUINavigationController = interface(IInspectable)
  ['{AAA98D20-C011-584C-82F2-1D0D361E164E}']
    function GetAt(index: Cardinal): Input_IUINavigationController; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Input_IUINavigationController; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInput_IUINavigationController): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IUINavigationControllerStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_UINavigationController)]
  Input_IUINavigationControllerStatics = interface(IInspectable)
  ['{2F14930A-F6F8-4A48-8D89-94786CCA0C2E}']
    function add_UINavigationControllerAdded(value: EventHandler_1__Input_IUINavigationController): EventRegistrationToken; safecall;
    procedure remove_UINavigationControllerAdded(token: EventRegistrationToken); safecall;
    function add_UINavigationControllerRemoved(value: EventHandler_1__Input_IUINavigationController): EventRegistrationToken; safecall;
    procedure remove_UINavigationControllerRemoved(token: EventRegistrationToken); safecall;
    function get_UINavigationControllers: IVectorView_1__Input_IUINavigationController; safecall;
    property UINavigationControllers: IVectorView_1__Input_IUINavigationController read get_UINavigationControllers;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.IUINavigationControllerStatics2
  [WinRTClassNameAttribute(SWindows_Gaming_Input_UINavigationController)]
  Input_IUINavigationControllerStatics2 = interface(IInspectable)
  ['{E0CB28E3-B20B-4B0B-9ED4-F3D53CEC0DE4}']
    function FromGameController(gameController: Input_IGameController): Input_IUINavigationController; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.Input.Preview.IGameControllerProviderInfoStatics
  [WinRTClassNameAttribute(SWindows_Gaming_Input_Preview_GameControllerProviderInfo)]
  Input_Preview_IGameControllerProviderInfoStatics = interface(IInspectable)
  ['{0BE1E6C5-D9BD-44EE-8362-488B2E464BFB}']
    function GetParentProviderId(provider: Input_Custom_IGameControllerProvider): HSTRING; safecall;
    function GetProviderId(provider: Input_Custom_IGameControllerProvider): HSTRING; safecall;
  end;

  // Windows.Gaming.Preview.GamesEnumeration.GameListRemovedEventHandler
  Preview_GamesEnumeration_GameListRemovedEventHandler = interface(IUnknown)
  ['{10C5648F-6C8F-4712-9B38-474BC22E76D8}']
    procedure Invoke(identifier: HSTRING); safecall;
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

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface(IUnknown)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
    procedure Invoke(sender: IInspectable; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.UI.IGameBarStatics
  [WinRTClassNameAttribute(SWindows_Gaming_UI_GameBar)]
  UI_IGameBarStatics = interface(IInspectable)
  ['{1DB9A292-CC78-4173-BE45-B61E67283EA7}']
    function add_VisibilityChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_VisibilityChanged(token: EventRegistrationToken); safecall;
    function add_IsInputRedirectedChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_IsInputRedirectedChanged(token: EventRegistrationToken); safecall;
    function get_Visible: Boolean; safecall;
    function get_IsInputRedirected: Boolean; safecall;
    property IsInputRedirected: Boolean read get_IsInputRedirected;
    property Visible: Boolean read get_Visible;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface(IUnknown)
  ['{359B8887-2FA6-5405-A4AF-642C9FDACC93}']
    procedure Invoke(sender: Activation_ISplashScreen; args: IInspectable); safecall;
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult
  XboxLive_Storage_IGameSaveBlobGetResult = interface(IInspectable)
  ['{917281E0-7201-4953-AA2C-4008F03AEF45}']
    function get_Status: XboxLive_Storage_GameSaveErrorStatus; safecall;
    function get_Value: IMapView_2__HSTRING__IBuffer; safecall;
    property Status: XboxLive_Storage_GameSaveErrorStatus read get_Status;
    property Value: IMapView_2__HSTRING__IBuffer read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo
  XboxLive_Storage_IGameSaveBlobInfo = interface(IInspectable)
  ['{ADD38034-BAF0-4645-B6D0-46EDAFFB3C2B}']
    function get_Name: HSTRING; safecall;
    function get_Size: Cardinal; safecall;
    property Name: HSTRING read get_Name;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IIterator_1__XboxLive_Storage_IGameSaveBlobInfo_Base = interface(IInspectable)
  ['{AAAF545B-F5E9-5DA6-AF70-9D904C7A4D37}']
    function get_Current: XboxLive_Storage_IGameSaveBlobInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PXboxLive_Storage_IGameSaveBlobInfo): Cardinal; safecall;
    property Current: XboxLive_Storage_IGameSaveBlobInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IIterator_1__XboxLive_Storage_IGameSaveBlobInfo = interface(IIterator_1__XboxLive_Storage_IGameSaveBlobInfo_Base)
  ['{A0D56059-EF17-5B74-974D-5F94D12B10EA}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IIterable_1__XboxLive_Storage_IGameSaveBlobInfo_Base = interface(IInspectable)
  ['{A7C456D7-FA9F-536F-8ED2-459545811ED4}']
    function First: IIterator_1__XboxLive_Storage_IGameSaveBlobInfo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IIterable_1__XboxLive_Storage_IGameSaveBlobInfo = interface(IIterable_1__XboxLive_Storage_IGameSaveBlobInfo_Base)
  ['{43514C9B-BF01-5719-80A3-6D7A4D536D62}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfo>
  IVectorView_1__XboxLive_Storage_IGameSaveBlobInfo = interface(IInspectable)
  ['{DA049282-6A34-541E-AFC2-DC31D40227D4}']
    function GetAt(index: Cardinal): XboxLive_Storage_IGameSaveBlobInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: XboxLive_Storage_IGameSaveBlobInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PXboxLive_Storage_IGameSaveBlobInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult
  XboxLive_Storage_IGameSaveBlobInfoGetResult = interface(IInspectable)
  ['{C7578582-3697-42BF-989C-665D923B5231}']
    function get_Status: XboxLive_Storage_GameSaveErrorStatus; safecall;
    function get_Value: IVectorView_1__XboxLive_Storage_IGameSaveBlobInfo; safecall;
    property Status: XboxLive_Storage_GameSaveErrorStatus read get_Status;
    property Value: IVectorView_1__XboxLive_Storage_IGameSaveBlobInfo read get_Value;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult_Delegate_Base = interface(IUnknown)
  ['{9331E53A-A414-51E7-BFBC-7784DF83DC8E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult = interface(AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult_Delegate_Base)
  ['{F3ABCDCD-1681-59C7-9916-5B862B81716D}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult_Base = interface(IInspectable)
  ['{D7B7F3B4-6028-522F-849D-A69495E4DCD0}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult; safecall;
    function GetResults: XboxLive_Storage_IGameSaveBlobInfoGetResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobInfoGetResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult = interface(IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult_Base)
  ['{C7C52A18-3BB2-59F3-BE9E-23F4A7221782}']
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<UInt32>
  AsyncOperationCompletedHandler_1__Cardinal = interface(IUnknown)
  ['{9343B6E7-E3D2-5E4A-AB2D-2BCE4919A6A4}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<UInt32>
  IAsyncOperation_1__Cardinal = interface(IInspectable)
  ['{EF60385F-BE78-584B-AAEF-7829ADA2B0DE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Cardinal); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Cardinal; safecall;
    function GetResults: Cardinal; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Cardinal read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveBlobInfoQuery
  XboxLive_Storage_IGameSaveBlobInfoQuery = interface(IInspectable)
  ['{9FDD74B2-EEEE-447B-A9D2-7F96C0F83208}']
    function GetBlobInfoAsync: IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult; overload; safecall;
    function GetBlobInfoAsync(startIndex: Cardinal; maxNumberOfItems: Cardinal): IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobInfoGetResult; overload; safecall;
    function GetItemCountAsync: IAsyncOperation_1__Cardinal; safecall;
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult
  XboxLive_Storage_IGameSaveOperationResult = interface(IInspectable)
  ['{CF0F1A05-24A0-4582-9A55-B1BBBB9388D8}']
    function get_Status: XboxLive_Storage_GameSaveErrorStatus; safecall;
    property Status: XboxLive_Storage_GameSaveErrorStatus read get_Status;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult_Delegate_Base = interface(IUnknown)
  ['{EE53E64F-5319-56FD-A28A-2C474FC42E48}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult = interface(AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult_Delegate_Base)
  ['{B8E836FA-5951-51E5-97B3-20877A33E797}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult_Base = interface(IInspectable)
  ['{1C27FB97-1E1A-516F-ABB2-12C18E18218D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult; safecall;
    function GetResults: XboxLive_Storage_IGameSaveOperationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveOperationResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveOperationResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult = interface(IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult_Base)
  ['{C35050AC-3B37-5C2F-855B-F7A59172F12E}']
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo
  XboxLive_Storage_IGameSaveContainerInfo = interface(IInspectable)
  ['{B7E27300-155D-4BB4-B2BA-930306F391B5}']
    function get_Name: HSTRING; safecall;
    function get_TotalSize: UInt64; safecall;
    function get_DisplayName: HSTRING; safecall;
    function get_LastModifiedTime: DateTime; safecall;
    function get_NeedsSync: Boolean; safecall;
    property DisplayName: HSTRING read get_DisplayName;
    property LastModifiedTime: DateTime read get_LastModifiedTime;
    property Name: HSTRING read get_Name;
    property NeedsSync: Boolean read get_NeedsSync;
    property TotalSize: UInt64 read get_TotalSize;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IIterator_1__XboxLive_Storage_IGameSaveContainerInfo_Base = interface(IInspectable)
  ['{0ECD9756-3E0D-523F-A549-2B6504DB5202}']
    function get_Current: XboxLive_Storage_IGameSaveContainerInfo; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PXboxLive_Storage_IGameSaveContainerInfo): Cardinal; safecall;
    property Current: XboxLive_Storage_IGameSaveContainerInfo read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IIterator_1__XboxLive_Storage_IGameSaveContainerInfo = interface(IIterator_1__XboxLive_Storage_IGameSaveContainerInfo_Base)
  ['{AF3513D4-D746-5063-B0D7-41037770DEC4}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IIterable_1__XboxLive_Storage_IGameSaveContainerInfo_Base = interface(IInspectable)
  ['{55E4D98F-0889-5C06-A857-7DD168C2D852}']
    function First: IIterator_1__XboxLive_Storage_IGameSaveContainerInfo; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IIterable_1__XboxLive_Storage_IGameSaveContainerInfo = interface(IIterable_1__XboxLive_Storage_IGameSaveContainerInfo_Base)
  ['{69260C3E-3765-5A0A-B8DC-5ECAC3E59562}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfo>
  IVectorView_1__XboxLive_Storage_IGameSaveContainerInfo = interface(IInspectable)
  ['{6F745F09-1B7B-5C89-BB0E-27615D2533B7}']
    function GetAt(index: Cardinal): XboxLive_Storage_IGameSaveContainerInfo; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: XboxLive_Storage_IGameSaveContainerInfo; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PXboxLive_Storage_IGameSaveContainerInfo): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult
  XboxLive_Storage_IGameSaveContainerInfoGetResult = interface(IInspectable)
  ['{FFC50D74-C581-4F9D-9E39-30A10C1E4C50}']
    function get_Status: XboxLive_Storage_GameSaveErrorStatus; safecall;
    function get_Value: IVectorView_1__XboxLive_Storage_IGameSaveContainerInfo; safecall;
    property Status: XboxLive_Storage_GameSaveErrorStatus read get_Status;
    property Value: IVectorView_1__XboxLive_Storage_IGameSaveContainerInfo read get_Value;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult_Delegate_Base = interface(IUnknown)
  ['{05F86A80-BE5B-5E7E-B977-8257C5E48ACC}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult = interface(AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult_Delegate_Base)
  ['{5F16D0D4-91FD-5E75-957F-14C998E2A7F0}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult_Base = interface(IInspectable)
  ['{CFF8AFEB-5A18-5F51-B61B-943887F729EE}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult; safecall;
    function GetResults: XboxLive_Storage_IGameSaveContainerInfoGetResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveContainerInfoGetResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult = interface(IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult_Base)
  ['{EC65EC4B-68E5-5026-B2D7-F94BD7485976}']
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveContainerInfoQuery
  XboxLive_Storage_IGameSaveContainerInfoQuery = interface(IInspectable)
  ['{3C94E863-6F80-4327-9327-FFC11AFD42B3}']
    function GetContainerInfoAsync: IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult; overload; safecall;
    function GetContainerInfoAsync(startIndex: Cardinal; maxNumberOfItems: Cardinal): IAsyncOperation_1__XboxLive_Storage_IGameSaveContainerInfoGetResult; overload; safecall;
    function GetItemCountAsync: IAsyncOperation_1__Cardinal; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Int64>
  AsyncOperationCompletedHandler_1__Int64_Delegate_Base = interface(IUnknown)
  ['{D3EF5872-7D4E-59BB-95ED-79FE0F0DBE89}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Int64; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Int64>
  AsyncOperationCompletedHandler_1__Int64 = interface(AsyncOperationCompletedHandler_1__Int64_Delegate_Base)
  ['{D3EF5872-7D4E-59BB-95ED-79FE0F0DBE89}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Int64>
  IAsyncOperation_1__Int64_Base = interface(IInspectable)
  ['{CC468085-4BEF-5584-907C-9223D2679019}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Int64); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Int64; safecall;
    function GetResults: Int64; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Int64 read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Int64>
  IAsyncOperation_1__Int64 = interface(IAsyncOperation_1__Int64_Base)
  ['{CC468085-4BEF-5584-907C-9223D2679019}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveProvider
  [WinRTClassNameAttribute(SWindows_Gaming_XboxLive_Storage_GameSaveProvider)]
  XboxLive_Storage_IGameSaveProvider = interface(IInspectable)
  ['{90A60394-80FE-4211-97F8-A5DE14DD95D2}']
    function get_User: IUser; safecall;
    function CreateContainer(name: HSTRING): XboxLive_Storage_IGameSaveContainer; safecall;
    function DeleteContainerAsync(name: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult; safecall;
    function CreateContainerInfoQuery: XboxLive_Storage_IGameSaveContainerInfoQuery; overload; safecall;
    function CreateContainerInfoQuery(containerNamePrefix: HSTRING): XboxLive_Storage_IGameSaveContainerInfoQuery; overload; safecall;
    function GetRemainingBytesInQuotaAsync: IAsyncOperation_1__Int64; safecall;
    function get_ContainersChangedSinceLastSync: IVectorView_1__HSTRING; safecall;
    property ContainersChangedSinceLastSync: IVectorView_1__HSTRING read get_ContainersChangedSinceLastSync;
    property User: IUser read get_User;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult_Delegate_Base = interface(IUnknown)
  ['{9D96282C-B6AB-5CD3-991B-A358C531BCB6}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult = interface(AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult_Delegate_Base)
  ['{5401C820-05B6-52E3-8C90-7CCE3D7448AF}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult_Base = interface(IInspectable)
  ['{7023B023-7AED-526C-B3BC-BE12E35CE1CF}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult; safecall;
    function GetResults: XboxLive_Storage_IGameSaveBlobGetResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveBlobGetResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveBlobGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult = interface(IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult_Base)
  ['{FF5A0BFE-BD7C-5695-8F49-EE175CDFAF0C}']
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveContainer
  XboxLive_Storage_IGameSaveContainer = interface(IInspectable)
  ['{C3C08F89-563F-4ECD-9C6F-33FD0E323D10}']
    function get_Name: HSTRING; safecall;
    function get_Provider: XboxLive_Storage_IGameSaveProvider; safecall;
    function SubmitUpdatesAsync(blobsToWrite: IMapView_2__HSTRING__IBuffer; blobsToDelete: IIterable_1__HSTRING; displayName: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult; safecall;
    function ReadAsync(blobsToRead: IMapView_2__HSTRING__IBuffer): IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult; safecall;
    function GetAsync(blobsToRead: IIterable_1__HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveBlobGetResult; safecall;
    function SubmitPropertySetUpdatesAsync(blobsToWrite: IPropertySet; blobsToDelete: IIterable_1__HSTRING; displayName: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveOperationResult; safecall;
    function CreateBlobInfoQuery(blobNamePrefix: HSTRING): XboxLive_Storage_IGameSaveBlobInfoQuery; safecall;
    property Name: HSTRING read get_Name;
    property Provider: XboxLive_Storage_IGameSaveProvider read get_Provider;
  end;

  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult
  XboxLive_Storage_IGameSaveProviderGetResult = interface(IInspectable)
  ['{3AB90816-D393-4D65-AC16-41C3E67AB945}']
    function get_Status: XboxLive_Storage_GameSaveErrorStatus; safecall;
    function get_Value: XboxLive_Storage_IGameSaveProvider; safecall;
    property Status: XboxLive_Storage_GameSaveErrorStatus read get_Status;
    property Value: XboxLive_Storage_IGameSaveProvider read get_Value;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult_Delegate_Base = interface(IUnknown)
  ['{7617548D-8E60-50CB-A11E-120FA2082E5B}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult>
  AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult = interface(AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult_Delegate_Base)
  ['{999C389C-8CF8-502E-BE10-9CA7449B0DAA}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult_Base = interface(IInspectable)
  ['{3DC36085-5FEC-541B-96CF-627B2AD80D36}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult; safecall;
    function GetResults: XboxLive_Storage_IGameSaveProviderGetResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__XboxLive_Storage_IGameSaveProviderGetResult read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Gaming.XboxLive.Storage.IGameSaveProviderGetResult>
  IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult = interface(IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult_Base)
  ['{8A84D67A-C1B5-5040-AE17-8818BB713E9C}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Gaming.XboxLive.Storage.IGameSaveProviderStatics
  [WinRTClassNameAttribute(SWindows_Gaming_XboxLive_Storage_GameSaveProvider)]
  XboxLive_Storage_IGameSaveProviderStatics = interface(IInspectable)
  ['{D01D3ED0-7B03-449D-8CBD-3402842A1048}']
    function GetForUserAsync(user: IUser; serviceConfigId: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult; safecall;
    function GetSyncOnDemandForUserAsync(user: IUser; serviceConfigId: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult; safecall;
  end;

  // Windows.Gaming.Input.ArcadeStick
  // DualAPI
  // Implements: Windows.Gaming.Input.IArcadeStick
  // Implements: Windows.Gaming.Input.IGameController
  // Implements: Windows.Gaming.Input.IGameControllerBatteryInfo
  // Statics: "Windows.Gaming.Input.IArcadeStickStatics"
  // Statics: "Windows.Gaming.Input.IArcadeStickStatics2"
  TInput_ArcadeStick = class(TWinRTGenericImportS2<Input_IArcadeStickStatics, Input_IArcadeStickStatics2>)
  public
    // -> Input_IArcadeStickStatics
    class function add_ArcadeStickAdded(value: EventHandler_1__Input_IArcadeStick): EventRegistrationToken; static; inline;
    class procedure remove_ArcadeStickAdded(token: EventRegistrationToken); static; inline;
    class function add_ArcadeStickRemoved(value: EventHandler_1__Input_IArcadeStick): EventRegistrationToken; static; inline;
    class procedure remove_ArcadeStickRemoved(token: EventRegistrationToken); static; inline;
    class function get_ArcadeSticks: IVectorView_1__Input_IArcadeStick; static; inline;
    class property ArcadeSticks: IVectorView_1__Input_IArcadeStick read get_ArcadeSticks;

    // -> Input_IArcadeStickStatics2
    class function FromGameController(gameController: Input_IGameController): Input_IArcadeStick; static; inline;
  end;

  // Windows.Gaming.Input.Custom.GameControllerFactoryManager
  // DualAPI
  // Statics: "Windows.Gaming.Input.Custom.IGameControllerFactoryManagerStatics"
  // Statics: "Windows.Gaming.Input.Custom.IGameControllerFactoryManagerStatics2"
  TInput_Custom_GameControllerFactoryManager = class(TWinRTGenericImportS2<Input_Custom_IGameControllerFactoryManagerStatics, Input_Custom_IGameControllerFactoryManagerStatics2>)
  public
    // -> Input_Custom_IGameControllerFactoryManagerStatics
    class procedure RegisterCustomFactoryForGipInterface(factory: Input_Custom_ICustomGameControllerFactory; interfaceId: TGuid); static; inline;
    class procedure RegisterCustomFactoryForHardwareId(factory: Input_Custom_ICustomGameControllerFactory; hardwareVendorId: Word; hardwareProductId: Word); static; inline;
    class procedure RegisterCustomFactoryForXusbType(factory: Input_Custom_ICustomGameControllerFactory; xusbType: Input_Custom_XusbDeviceType; xusbSubtype: Input_Custom_XusbDeviceSubtype); static; inline;

    // -> Input_Custom_IGameControllerFactoryManagerStatics2
    class function TryGetFactoryControllerFromGameController(factory: Input_Custom_ICustomGameControllerFactory; gameController: Input_IGameController): Input_IGameController; static; inline;
  end;

  // Windows.Gaming.Input.FlightStick
  // DualAPI
  // Implements: Windows.Gaming.Input.IFlightStick
  // Implements: Windows.Gaming.Input.IGameController
  // Implements: Windows.Gaming.Input.IGameControllerBatteryInfo
  // Statics: "Windows.Gaming.Input.IFlightStickStatics"
  TInput_FlightStick = class(TWinRTGenericImportS<Input_IFlightStickStatics>)
  public
    // -> Input_IFlightStickStatics
    class function add_FlightStickAdded(value: EventHandler_1__Input_IFlightStick): EventRegistrationToken; static; inline;
    class procedure remove_FlightStickAdded(token: EventRegistrationToken); static; inline;
    class function add_FlightStickRemoved(value: EventHandler_1__Input_IFlightStick): EventRegistrationToken; static; inline;
    class procedure remove_FlightStickRemoved(token: EventRegistrationToken); static; inline;
    class function get_FlightSticks: IVectorView_1__Input_IFlightStick; static; inline;
    class function FromGameController(gameController: Input_IGameController): Input_IFlightStick; static; inline;
    class property FlightSticks: IVectorView_1__Input_IFlightStick read get_FlightSticks;
  end;

  // Windows.Gaming.Input.ForceFeedback.ConditionForceEffect
  // DualAPI
  // Implements: Windows.Gaming.Input.ForceFeedback.IForceFeedbackEffect
  // Implements: Windows.Gaming.Input.ForceFeedback.IConditionForceEffect
  // Factory: "Windows.Gaming.Input.ForceFeedback.IConditionForceEffectFactory"
  TInput_ForceFeedback_ConditionForceEffect = class(TWinRTGenericImportF<Input_ForceFeedback_IConditionForceEffectFactory>)
  public
    // -> Input_ForceFeedback_IConditionForceEffectFactory
    class function CreateInstance(effectKind: Input_ForceFeedback_ConditionForceEffectKind): Input_ForceFeedback_IForceFeedbackEffect; static; inline;
  end;

  // Windows.Gaming.Input.ForceFeedback.ConstantForceEffect
  // DualAPI
  // Implements: Windows.Gaming.Input.ForceFeedback.IForceFeedbackEffect
  // Implements: Windows.Gaming.Input.ForceFeedback.IConstantForceEffect
  // Instantiable: "Input_ForceFeedback_IConstantForceEffect"
  TInput_ForceFeedback_ConstantForceEffect = class(TWinRTGenericImportI<Input_ForceFeedback_IConstantForceEffect>) end;

  // Windows.Gaming.Input.ForceFeedback.PeriodicForceEffect
  // DualAPI
  // Implements: Windows.Gaming.Input.ForceFeedback.IForceFeedbackEffect
  // Implements: Windows.Gaming.Input.ForceFeedback.IPeriodicForceEffect
  // Factory: "Windows.Gaming.Input.ForceFeedback.IPeriodicForceEffectFactory"
  TInput_ForceFeedback_PeriodicForceEffect = class(TWinRTGenericImportF<Input_ForceFeedback_IPeriodicForceEffectFactory>)
  public
    // -> Input_ForceFeedback_IPeriodicForceEffectFactory
    class function CreateInstance(effectKind: Input_ForceFeedback_PeriodicForceEffectKind): Input_ForceFeedback_IForceFeedbackEffect; static; inline;
  end;

  // Windows.Gaming.Input.ForceFeedback.RampForceEffect
  // DualAPI
  // Implements: Windows.Gaming.Input.ForceFeedback.IForceFeedbackEffect
  // Implements: Windows.Gaming.Input.ForceFeedback.IRampForceEffect
  // Instantiable: "Input_ForceFeedback_IRampForceEffect"
  TInput_ForceFeedback_RampForceEffect = class(TWinRTGenericImportI<Input_ForceFeedback_IRampForceEffect>) end;

  // Windows.Gaming.Input.Gamepad
  // DualAPI
  // Implements: Windows.Gaming.Input.IGamepad
  // Implements: Windows.Gaming.Input.IGameController
  // Implements: Windows.Gaming.Input.IGamepad2
  // Implements: Windows.Gaming.Input.IGameControllerBatteryInfo
  // Statics: "Windows.Gaming.Input.IGamepadStatics"
  // Statics: "Windows.Gaming.Input.IGamepadStatics2"
  TInput_Gamepad = class(TWinRTGenericImportS2<Input_IGamepadStatics, Input_IGamepadStatics2>)
  public
    // -> Input_IGamepadStatics
    class function add_GamepadAdded(value: EventHandler_1__Input_IGamepad): EventRegistrationToken; static; inline;
    class procedure remove_GamepadAdded(token: EventRegistrationToken); static; inline;
    class function add_GamepadRemoved(value: EventHandler_1__Input_IGamepad): EventRegistrationToken; static; inline;
    class procedure remove_GamepadRemoved(token: EventRegistrationToken); static; inline;
    class function get_Gamepads: IVectorView_1__Input_IGamepad; static; inline;
    class property Gamepads: IVectorView_1__Input_IGamepad read get_Gamepads;

    // -> Input_IGamepadStatics2
    class function FromGameController(gameController: Input_IGameController): Input_IGamepad; static; inline;
  end;

  // Windows.Gaming.Input.Preview.GameControllerProviderInfo
  // DualAPI
  // Statics: "Windows.Gaming.Input.Preview.IGameControllerProviderInfoStatics"
  TInput_Preview_GameControllerProviderInfo = class(TWinRTGenericImportS<Input_Preview_IGameControllerProviderInfoStatics>)
  public
    // -> Input_Preview_IGameControllerProviderInfoStatics
    class function GetParentProviderId(provider: Input_Custom_IGameControllerProvider): HSTRING; static; inline;
    class function GetProviderId(provider: Input_Custom_IGameControllerProvider): HSTRING; static; inline;
  end;

  // Windows.Gaming.Input.RacingWheel
  // DualAPI
  // Implements: Windows.Gaming.Input.IRacingWheel
  // Implements: Windows.Gaming.Input.IGameController
  // Implements: Windows.Gaming.Input.IGameControllerBatteryInfo
  // Statics: "Windows.Gaming.Input.IRacingWheelStatics"
  // Statics: "Windows.Gaming.Input.IRacingWheelStatics2"
  TInput_RacingWheel = class(TWinRTGenericImportS2<Input_IRacingWheelStatics, Input_IRacingWheelStatics2>)
  public
    // -> Input_IRacingWheelStatics
    class function add_RacingWheelAdded(value: EventHandler_1__Input_IRacingWheel): EventRegistrationToken; static; inline;
    class procedure remove_RacingWheelAdded(token: EventRegistrationToken); static; inline;
    class function add_RacingWheelRemoved(value: EventHandler_1__Input_IRacingWheel): EventRegistrationToken; static; inline;
    class procedure remove_RacingWheelRemoved(token: EventRegistrationToken); static; inline;
    class function get_RacingWheels: IVectorView_1__Input_IRacingWheel; static; inline;
    class property RacingWheels: IVectorView_1__Input_IRacingWheel read get_RacingWheels;

    // -> Input_IRacingWheelStatics2
    class function FromGameController(gameController: Input_IGameController): Input_IRacingWheel; static; inline;
  end;

  // Windows.Gaming.Input.RawGameController
  // DualAPI
  // Implements: Windows.Gaming.Input.IRawGameController
  // Implements: Windows.Gaming.Input.IGameController
  // Implements: Windows.Gaming.Input.IGameControllerBatteryInfo
  // Implements: Windows.Gaming.Input.IRawGameController2
  // Statics: "Windows.Gaming.Input.IRawGameControllerStatics"
  TInput_RawGameController = class(TWinRTGenericImportS<Input_IRawGameControllerStatics>)
  public
    // -> Input_IRawGameControllerStatics
    class function add_RawGameControllerAdded(value: EventHandler_1__Input_IRawGameController): EventRegistrationToken; static; inline;
    class procedure remove_RawGameControllerAdded(token: EventRegistrationToken); static; inline;
    class function add_RawGameControllerRemoved(value: EventHandler_1__Input_IRawGameController): EventRegistrationToken; static; inline;
    class procedure remove_RawGameControllerRemoved(token: EventRegistrationToken); static; inline;
    class function get_RawGameControllers: IVectorView_1__Input_IRawGameController; static; inline;
    class function FromGameController(gameController: Input_IGameController): Input_IRawGameController; static; inline;
    class property RawGameControllers: IVectorView_1__Input_IRawGameController read get_RawGameControllers;
  end;

  // Windows.Gaming.Input.UINavigationController
  // DualAPI
  // Implements: Windows.Gaming.Input.IUINavigationController
  // Implements: Windows.Gaming.Input.IGameController
  // Implements: Windows.Gaming.Input.IGameControllerBatteryInfo
  // Statics: "Windows.Gaming.Input.IUINavigationControllerStatics"
  // Statics: "Windows.Gaming.Input.IUINavigationControllerStatics2"
  TInput_UINavigationController = class(TWinRTGenericImportS2<Input_IUINavigationControllerStatics, Input_IUINavigationControllerStatics2>)
  public
    // -> Input_IUINavigationControllerStatics
    class function add_UINavigationControllerAdded(value: EventHandler_1__Input_IUINavigationController): EventRegistrationToken; static; inline;
    class procedure remove_UINavigationControllerAdded(token: EventRegistrationToken); static; inline;
    class function add_UINavigationControllerRemoved(value: EventHandler_1__Input_IUINavigationController): EventRegistrationToken; static; inline;
    class procedure remove_UINavigationControllerRemoved(token: EventRegistrationToken); static; inline;
    class function get_UINavigationControllers: IVectorView_1__Input_IUINavigationController; static; inline;
    class property UINavigationControllers: IVectorView_1__Input_IUINavigationController read get_UINavigationControllers;

    // -> Input_IUINavigationControllerStatics2
    class function FromGameController(gameController: Input_IGameController): Input_IUINavigationController; static; inline;
  end;

  // Windows.Gaming.UI.GameBar
  // DualAPI
  // Statics: "Windows.Gaming.UI.IGameBarStatics"
  TUI_GameBar = class(TWinRTGenericImportS<UI_IGameBarStatics>)
  public
    // -> UI_IGameBarStatics
    class function add_VisibilityChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_VisibilityChanged(token: EventRegistrationToken); static; inline;
    class function add_IsInputRedirectedChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_IsInputRedirectedChanged(token: EventRegistrationToken); static; inline;
    class function get_Visible: Boolean; static; inline;
    class function get_IsInputRedirected: Boolean; static; inline;
    class property IsInputRedirected: Boolean read get_IsInputRedirected;
    class property Visible: Boolean read get_Visible;
  end;

  // Windows.Gaming.XboxLive.Storage.GameSaveProvider
  // DualAPI
  // Implements: Windows.Gaming.XboxLive.Storage.IGameSaveProvider
  // Statics: "Windows.Gaming.XboxLive.Storage.IGameSaveProviderStatics"
  TXboxLive_Storage_GameSaveProvider = class(TWinRTGenericImportS<XboxLive_Storage_IGameSaveProviderStatics>)
  public
    // -> XboxLive_Storage_IGameSaveProviderStatics
    class function GetForUserAsync(user: IUser; serviceConfigId: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult; static; inline;
    class function GetSyncOnDemandForUserAsync(user: IUser; serviceConfigId: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult; static; inline;
  end;

implementation

{ TInput_ArcadeStick }

class function TInput_ArcadeStick.add_ArcadeStickAdded(value: EventHandler_1__Input_IArcadeStick): EventRegistrationToken;
begin
  Result := Statics.add_ArcadeStickAdded(value);
end;

class procedure TInput_ArcadeStick.remove_ArcadeStickAdded(token: EventRegistrationToken);
begin
  Statics.remove_ArcadeStickAdded(token);
end;

class function TInput_ArcadeStick.add_ArcadeStickRemoved(value: EventHandler_1__Input_IArcadeStick): EventRegistrationToken;
begin
  Result := Statics.add_ArcadeStickRemoved(value);
end;

class procedure TInput_ArcadeStick.remove_ArcadeStickRemoved(token: EventRegistrationToken);
begin
  Statics.remove_ArcadeStickRemoved(token);
end;

class function TInput_ArcadeStick.get_ArcadeSticks: IVectorView_1__Input_IArcadeStick;
begin
  Result := Statics.get_ArcadeSticks;
end;


class function TInput_ArcadeStick.FromGameController(gameController: Input_IGameController): Input_IArcadeStick;
begin
  Result := Statics2.FromGameController(gameController);
end;


{ TInput_Custom_GameControllerFactoryManager }

class procedure TInput_Custom_GameControllerFactoryManager.RegisterCustomFactoryForGipInterface(factory: Input_Custom_ICustomGameControllerFactory; interfaceId: TGuid);
begin
  Statics.RegisterCustomFactoryForGipInterface(factory, interfaceId);
end;

class procedure TInput_Custom_GameControllerFactoryManager.RegisterCustomFactoryForHardwareId(factory: Input_Custom_ICustomGameControllerFactory; hardwareVendorId: Word; hardwareProductId: Word);
begin
  Statics.RegisterCustomFactoryForHardwareId(factory, hardwareVendorId, hardwareProductId);
end;

class procedure TInput_Custom_GameControllerFactoryManager.RegisterCustomFactoryForXusbType(factory: Input_Custom_ICustomGameControllerFactory; xusbType: Input_Custom_XusbDeviceType; xusbSubtype: Input_Custom_XusbDeviceSubtype);
begin
  Statics.RegisterCustomFactoryForXusbType(factory, xusbType, xusbSubtype);
end;


class function TInput_Custom_GameControllerFactoryManager.TryGetFactoryControllerFromGameController(factory: Input_Custom_ICustomGameControllerFactory; gameController: Input_IGameController): Input_IGameController;
begin
  Result := Statics2.TryGetFactoryControllerFromGameController(factory, gameController);
end;


{ TInput_FlightStick }

class function TInput_FlightStick.add_FlightStickAdded(value: EventHandler_1__Input_IFlightStick): EventRegistrationToken;
begin
  Result := Statics.add_FlightStickAdded(value);
end;

class procedure TInput_FlightStick.remove_FlightStickAdded(token: EventRegistrationToken);
begin
  Statics.remove_FlightStickAdded(token);
end;

class function TInput_FlightStick.add_FlightStickRemoved(value: EventHandler_1__Input_IFlightStick): EventRegistrationToken;
begin
  Result := Statics.add_FlightStickRemoved(value);
end;

class procedure TInput_FlightStick.remove_FlightStickRemoved(token: EventRegistrationToken);
begin
  Statics.remove_FlightStickRemoved(token);
end;

class function TInput_FlightStick.get_FlightSticks: IVectorView_1__Input_IFlightStick;
begin
  Result := Statics.get_FlightSticks;
end;

class function TInput_FlightStick.FromGameController(gameController: Input_IGameController): Input_IFlightStick;
begin
  Result := Statics.FromGameController(gameController);
end;


{ TInput_ForceFeedback_ConditionForceEffect }
// Factories for : "Input_ForceFeedback_ConditionForceEffect"
// Factory: "Windows.Gaming.Input.ForceFeedback.IConditionForceEffectFactory"
// -> Input_ForceFeedback_IConditionForceEffectFactory

class function TInput_ForceFeedback_ConditionForceEffect.CreateInstance(effectKind: Input_ForceFeedback_ConditionForceEffectKind): Input_ForceFeedback_IForceFeedbackEffect;
begin
  Result := Factory.CreateInstance(effectKind);
end;


{ TInput_ForceFeedback_ConstantForceEffect }

{ TInput_ForceFeedback_PeriodicForceEffect }
// Factories for : "Input_ForceFeedback_PeriodicForceEffect"
// Factory: "Windows.Gaming.Input.ForceFeedback.IPeriodicForceEffectFactory"
// -> Input_ForceFeedback_IPeriodicForceEffectFactory

class function TInput_ForceFeedback_PeriodicForceEffect.CreateInstance(effectKind: Input_ForceFeedback_PeriodicForceEffectKind): Input_ForceFeedback_IForceFeedbackEffect;
begin
  Result := Factory.CreateInstance(effectKind);
end;


{ TInput_ForceFeedback_RampForceEffect }

{ TInput_Gamepad }

class function TInput_Gamepad.add_GamepadAdded(value: EventHandler_1__Input_IGamepad): EventRegistrationToken;
begin
  Result := Statics.add_GamepadAdded(value);
end;

class procedure TInput_Gamepad.remove_GamepadAdded(token: EventRegistrationToken);
begin
  Statics.remove_GamepadAdded(token);
end;

class function TInput_Gamepad.add_GamepadRemoved(value: EventHandler_1__Input_IGamepad): EventRegistrationToken;
begin
  Result := Statics.add_GamepadRemoved(value);
end;

class procedure TInput_Gamepad.remove_GamepadRemoved(token: EventRegistrationToken);
begin
  Statics.remove_GamepadRemoved(token);
end;

class function TInput_Gamepad.get_Gamepads: IVectorView_1__Input_IGamepad;
begin
  Result := Statics.get_Gamepads;
end;


class function TInput_Gamepad.FromGameController(gameController: Input_IGameController): Input_IGamepad;
begin
  Result := Statics2.FromGameController(gameController);
end;


{ TInput_Preview_GameControllerProviderInfo }

class function TInput_Preview_GameControllerProviderInfo.GetParentProviderId(provider: Input_Custom_IGameControllerProvider): HSTRING;
begin
  Result := Statics.GetParentProviderId(provider);
end;

class function TInput_Preview_GameControllerProviderInfo.GetProviderId(provider: Input_Custom_IGameControllerProvider): HSTRING;
begin
  Result := Statics.GetProviderId(provider);
end;


{ TInput_RacingWheel }

class function TInput_RacingWheel.add_RacingWheelAdded(value: EventHandler_1__Input_IRacingWheel): EventRegistrationToken;
begin
  Result := Statics.add_RacingWheelAdded(value);
end;

class procedure TInput_RacingWheel.remove_RacingWheelAdded(token: EventRegistrationToken);
begin
  Statics.remove_RacingWheelAdded(token);
end;

class function TInput_RacingWheel.add_RacingWheelRemoved(value: EventHandler_1__Input_IRacingWheel): EventRegistrationToken;
begin
  Result := Statics.add_RacingWheelRemoved(value);
end;

class procedure TInput_RacingWheel.remove_RacingWheelRemoved(token: EventRegistrationToken);
begin
  Statics.remove_RacingWheelRemoved(token);
end;

class function TInput_RacingWheel.get_RacingWheels: IVectorView_1__Input_IRacingWheel;
begin
  Result := Statics.get_RacingWheels;
end;


class function TInput_RacingWheel.FromGameController(gameController: Input_IGameController): Input_IRacingWheel;
begin
  Result := Statics2.FromGameController(gameController);
end;


{ TInput_RawGameController }

class function TInput_RawGameController.add_RawGameControllerAdded(value: EventHandler_1__Input_IRawGameController): EventRegistrationToken;
begin
  Result := Statics.add_RawGameControllerAdded(value);
end;

class procedure TInput_RawGameController.remove_RawGameControllerAdded(token: EventRegistrationToken);
begin
  Statics.remove_RawGameControllerAdded(token);
end;

class function TInput_RawGameController.add_RawGameControllerRemoved(value: EventHandler_1__Input_IRawGameController): EventRegistrationToken;
begin
  Result := Statics.add_RawGameControllerRemoved(value);
end;

class procedure TInput_RawGameController.remove_RawGameControllerRemoved(token: EventRegistrationToken);
begin
  Statics.remove_RawGameControllerRemoved(token);
end;

class function TInput_RawGameController.get_RawGameControllers: IVectorView_1__Input_IRawGameController;
begin
  Result := Statics.get_RawGameControllers;
end;

class function TInput_RawGameController.FromGameController(gameController: Input_IGameController): Input_IRawGameController;
begin
  Result := Statics.FromGameController(gameController);
end;


{ TInput_UINavigationController }

class function TInput_UINavigationController.add_UINavigationControllerAdded(value: EventHandler_1__Input_IUINavigationController): EventRegistrationToken;
begin
  Result := Statics.add_UINavigationControllerAdded(value);
end;

class procedure TInput_UINavigationController.remove_UINavigationControllerAdded(token: EventRegistrationToken);
begin
  Statics.remove_UINavigationControllerAdded(token);
end;

class function TInput_UINavigationController.add_UINavigationControllerRemoved(value: EventHandler_1__Input_IUINavigationController): EventRegistrationToken;
begin
  Result := Statics.add_UINavigationControllerRemoved(value);
end;

class procedure TInput_UINavigationController.remove_UINavigationControllerRemoved(token: EventRegistrationToken);
begin
  Statics.remove_UINavigationControllerRemoved(token);
end;

class function TInput_UINavigationController.get_UINavigationControllers: IVectorView_1__Input_IUINavigationController;
begin
  Result := Statics.get_UINavigationControllers;
end;


class function TInput_UINavigationController.FromGameController(gameController: Input_IGameController): Input_IUINavigationController;
begin
  Result := Statics2.FromGameController(gameController);
end;


{ TUI_GameBar }

class function TUI_GameBar.add_VisibilityChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_VisibilityChanged(handler);
end;

class procedure TUI_GameBar.remove_VisibilityChanged(token: EventRegistrationToken);
begin
  Statics.remove_VisibilityChanged(token);
end;

class function TUI_GameBar.add_IsInputRedirectedChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_IsInputRedirectedChanged(handler);
end;

class procedure TUI_GameBar.remove_IsInputRedirectedChanged(token: EventRegistrationToken);
begin
  Statics.remove_IsInputRedirectedChanged(token);
end;

class function TUI_GameBar.get_Visible: Boolean;
begin
  Result := Statics.get_Visible;
end;

class function TUI_GameBar.get_IsInputRedirected: Boolean;
begin
  Result := Statics.get_IsInputRedirected;
end;


{ TXboxLive_Storage_GameSaveProvider }

class function TXboxLive_Storage_GameSaveProvider.GetForUserAsync(user: IUser; serviceConfigId: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult;
begin
  Result := Statics.GetForUserAsync(user, serviceConfigId);
end;

class function TXboxLive_Storage_GameSaveProvider.GetSyncOnDemandForUserAsync(user: IUser; serviceConfigId: HSTRING): IAsyncOperation_1__XboxLive_Storage_IGameSaveProviderGetResult;
begin
  Result := Statics.GetSyncOnDemandForUserAsync(user, serviceConfigId);
end;


end.
