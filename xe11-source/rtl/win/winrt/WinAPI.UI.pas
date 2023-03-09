{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI;

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
  Winapi.Security.Credentials, 
  Winapi.Foundation, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  AsyncActionProgressHandler_1__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncActionProgressHandler_1__UInt64_Delegate_Base;
  AsyncActionProgressHandler_1__UInt64 = Winapi.CommonTypes.AsyncActionProgressHandler_1__UInt64;
  PAsyncActionProgressHandler_1__UInt64 = Winapi.CommonTypes.PAsyncActionProgressHandler_1__UInt64;
  AsyncActionWithProgressCompletedHandler_1__UInt64_Delegate_Base = Winapi.CommonTypes.AsyncActionWithProgressCompletedHandler_1__UInt64_Delegate_Base;
  AsyncActionWithProgressCompletedHandler_1__UInt64 = Winapi.CommonTypes.AsyncActionWithProgressCompletedHandler_1__UInt64;
  PAsyncActionWithProgressCompletedHandler_1__UInt64 = Winapi.CommonTypes.PAsyncActionWithProgressCompletedHandler_1__UInt64;
  Color = Winapi.CommonTypes.Color;
  PColor = Winapi.CommonTypes.PColor;
  IAsyncActionWithProgress_1__UInt64 = Winapi.CommonTypes.IAsyncActionWithProgress_1__UInt64;
  PIAsyncActionWithProgress_1__UInt64 = Winapi.CommonTypes.PIAsyncActionWithProgress_1__UInt64;
  IUIContext = Winapi.CommonTypes.IUIContext;
  PIUIContext = Winapi.CommonTypes.PIUIContext;
  IVector_1__Single_Base = Winapi.CommonTypes.IVector_1__Single_Base;
  IVector_1__Single = Winapi.CommonTypes.IVector_1__Single;
  PIVector_1__Single = Winapi.CommonTypes.PIVector_1__Single;
  IVectorView_1__WindowManagement_IDisplayRegion = Winapi.CommonTypes.IVectorView_1__WindowManagement_IDisplayRegion;
  PIVectorView_1__WindowManagement_IDisplayRegion = Winapi.CommonTypes.PIVectorView_1__WindowManagement_IDisplayRegion;
  Popups_Placement = Winapi.CommonTypes.Popups_Placement;
  PPopups_Placement = Winapi.CommonTypes.PPopups_Placement;
  TypedEventHandler_2__Core_ICompositorController__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__Core_ICompositorController__IInspectable;
  PTypedEventHandler_2__Core_ICompositorController__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__Core_ICompositorController__IInspectable;
  TypedEventHandler_2__Core_ICoreInputView__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__Core_ICoreInputView__IInspectable;
  PTypedEventHandler_2__Core_ICoreInputView__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__Core_ICoreInputView__IInspectable;
  TypedEventHandler_2__ICompositionCapabilities__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICompositionCapabilities__IInspectable_Delegate_Base;
  TypedEventHandler_2__ICompositionCapabilities__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__ICompositionCapabilities__IInspectable;
  PTypedEventHandler_2__ICompositionCapabilities__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__ICompositionCapabilities__IInspectable;
  TypedEventHandler_2__IRadialController__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IInspectable_Delegate_Base;
  TypedEventHandler_2__IRadialController__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IRadialController__IInspectable;
  PTypedEventHandler_2__IRadialController__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IRadialController__IInspectable;
  TypedEventHandler_2__IRadialControllerMenuItem__IInspectable_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IRadialControllerMenuItem__IInspectable_Delegate_Base;
  TypedEventHandler_2__IRadialControllerMenuItem__IInspectable = Winapi.CommonTypes.TypedEventHandler_2__IRadialControllerMenuItem__IInspectable;
  PTypedEventHandler_2__IRadialControllerMenuItem__IInspectable = Winapi.CommonTypes.PTypedEventHandler_2__IRadialControllerMenuItem__IInspectable;
  TypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs;
  PTypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__WindowManagement_IWindowingEnvironment__WindowManagement_IWindowingEnvironmentChangedEventArgs;
  WindowManagement_IDisplayRegion = Winapi.CommonTypes.WindowManagement_IDisplayRegion;
  PWindowManagement_IDisplayRegion = Winapi.CommonTypes.PWindowManagement_IDisplayRegion;
  WindowManagement_IWindowingEnvironment = Winapi.CommonTypes.WindowManagement_IWindowingEnvironment;
  PWindowManagement_IWindowingEnvironment = Winapi.CommonTypes.PWindowManagement_IWindowingEnvironment;
  WindowManagement_IWindowingEnvironmentChangedEventArgs = Winapi.CommonTypes.WindowManagement_IWindowingEnvironmentChangedEventArgs;
  PWindowManagement_IWindowingEnvironmentChangedEventArgs = Winapi.CommonTypes.PWindowManagement_IWindowingEnvironmentChangedEventArgs;
  WindowManagement_WindowingEnvironmentKind = Winapi.CommonTypes.WindowManagement_WindowingEnvironmentKind;
  PWindowManagement_WindowingEnvironmentKind = Winapi.CommonTypes.PWindowManagement_WindowingEnvironmentKind;

  // Forward declarations for interfaces

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Color>
  IIterator_1__Color = interface;
  PIIterator_1__Color = ^IIterator_1__Color;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Color>
  IIterable_1__Color = interface;
  PIIterable_1__Color = ^IIterable_1__Color;

  // Windows.Foundation.IReference`1<Windows.UI.Color>
  IReference_1__Color = interface;
  PIReference_1__Color = ^IReference_1__Color;

  // Windows.UI.Shell.IAdaptiveCard
  Shell_IAdaptiveCard = interface;
  PShell_IAdaptiveCard = ^Shell_IAdaptiveCard;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.WindowManagement.IDisplayRegion>
  IIterator_1__WindowManagement_IDisplayRegion = interface;
  PIIterator_1__WindowManagement_IDisplayRegion = ^IIterator_1__WindowManagement_IDisplayRegion;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.WindowManagement.IDisplayRegion>
  IIterable_1__WindowManagement_IDisplayRegion = interface;
  PIIterable_1__WindowManagement_IDisplayRegion = ^IIterable_1__WindowManagement_IDisplayRegion;

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

  // Windows.UI.ApplicationSettings.ICredentialCommand
  ApplicationSettings_ICredentialCommand = interface;
  PApplicationSettings_ICredentialCommand = ^ApplicationSettings_ICredentialCommand;

  // Windows.UI.ApplicationSettings.CredentialCommandCredentialDeletedHandler
  ApplicationSettings_CredentialCommandCredentialDeletedHandler = interface;
  PApplicationSettings_CredentialCommandCredentialDeletedHandler = ^ApplicationSettings_CredentialCommandCredentialDeletedHandler;

  // Windows.UI.ApplicationSettings.WebAccountProviderCommandInvokedHandler
  ApplicationSettings_WebAccountProviderCommandInvokedHandler = interface;
  PApplicationSettings_WebAccountProviderCommandInvokedHandler = ^ApplicationSettings_WebAccountProviderCommandInvokedHandler;

  // Windows.UI.ApplicationSettings.IWebAccountProviderCommand
  ApplicationSettings_IWebAccountProviderCommand = interface;
  PApplicationSettings_IWebAccountProviderCommand = ^ApplicationSettings_IWebAccountProviderCommand;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IIterator_1__ApplicationSettings_IWebAccountProviderCommand = interface;
  PIIterator_1__ApplicationSettings_IWebAccountProviderCommand = ^IIterator_1__ApplicationSettings_IWebAccountProviderCommand;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IIterable_1__ApplicationSettings_IWebAccountProviderCommand = interface;
  PIIterable_1__ApplicationSettings_IWebAccountProviderCommand = ^IIterable_1__ApplicationSettings_IWebAccountProviderCommand;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IVectorView_1__ApplicationSettings_IWebAccountProviderCommand = interface;
  PIVectorView_1__ApplicationSettings_IWebAccountProviderCommand = ^IVectorView_1__ApplicationSettings_IWebAccountProviderCommand;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IVector_1__ApplicationSettings_IWebAccountProviderCommand = interface;
  PIVector_1__ApplicationSettings_IWebAccountProviderCommand = ^IVector_1__ApplicationSettings_IWebAccountProviderCommand;

  // Windows.UI.ApplicationSettings.IWebAccountInvokedArgs
  ApplicationSettings_IWebAccountInvokedArgs = interface;
  PApplicationSettings_IWebAccountInvokedArgs = ^ApplicationSettings_IWebAccountInvokedArgs;

  // Windows.UI.ApplicationSettings.WebAccountCommandInvokedHandler
  ApplicationSettings_WebAccountCommandInvokedHandler = interface;
  PApplicationSettings_WebAccountCommandInvokedHandler = ^ApplicationSettings_WebAccountCommandInvokedHandler;

  // Windows.UI.ApplicationSettings.IWebAccountCommand
  ApplicationSettings_IWebAccountCommand = interface;
  PApplicationSettings_IWebAccountCommand = ^ApplicationSettings_IWebAccountCommand;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IIterator_1__ApplicationSettings_IWebAccountCommand = interface;
  PIIterator_1__ApplicationSettings_IWebAccountCommand = ^IIterator_1__ApplicationSettings_IWebAccountCommand;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IIterable_1__ApplicationSettings_IWebAccountCommand = interface;
  PIIterable_1__ApplicationSettings_IWebAccountCommand = ^IIterable_1__ApplicationSettings_IWebAccountCommand;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IVectorView_1__ApplicationSettings_IWebAccountCommand = interface;
  PIVectorView_1__ApplicationSettings_IWebAccountCommand = ^IVectorView_1__ApplicationSettings_IWebAccountCommand;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IVector_1__ApplicationSettings_IWebAccountCommand = interface;
  PIVector_1__ApplicationSettings_IWebAccountCommand = ^IVector_1__ApplicationSettings_IWebAccountCommand;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IIterator_1__ApplicationSettings_ICredentialCommand = interface;
  PIIterator_1__ApplicationSettings_ICredentialCommand = ^IIterator_1__ApplicationSettings_ICredentialCommand;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IIterable_1__ApplicationSettings_ICredentialCommand = interface;
  PIIterable_1__ApplicationSettings_ICredentialCommand = ^IIterable_1__ApplicationSettings_ICredentialCommand;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IVectorView_1__ApplicationSettings_ICredentialCommand = interface;
  PIVectorView_1__ApplicationSettings_ICredentialCommand = ^IVectorView_1__ApplicationSettings_ICredentialCommand;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IVector_1__ApplicationSettings_ICredentialCommand = interface;
  PIVector_1__ApplicationSettings_ICredentialCommand = ^IVector_1__ApplicationSettings_ICredentialCommand;

  // Windows.UI.Popups.UICommandInvokedHandler
  Popups_UICommandInvokedHandler = interface;
  PPopups_UICommandInvokedHandler = ^Popups_UICommandInvokedHandler;

  // Windows.UI.Popups.IUICommand
  Popups_IUICommand = interface;
  PPopups_IUICommand = ^Popups_IUICommand;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Popups.IUICommand>
  IIterator_1__Popups_IUICommand = interface;
  PIIterator_1__Popups_IUICommand = ^IIterator_1__Popups_IUICommand;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Popups.IUICommand>
  IIterable_1__Popups_IUICommand = interface;
  PIIterable_1__Popups_IUICommand = ^IIterable_1__Popups_IUICommand;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Popups.IUICommand>
  IVectorView_1__Popups_IUICommand = interface;
  PIVectorView_1__Popups_IUICommand = ^IVectorView_1__Popups_IUICommand;

  // Windows.Foundation.Collections.IVector`1<Windows.UI.Popups.IUICommand>
  IVector_1__Popups_IUICommand = interface;
  PIVector_1__Popups_IUICommand = ^IVector_1__Popups_IUICommand;

  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneEventDeferral
  ApplicationSettings_IAccountsSettingsPaneEventDeferral = interface;
  PApplicationSettings_IAccountsSettingsPaneEventDeferral = ^ApplicationSettings_IAccountsSettingsPaneEventDeferral;

  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneCommandsRequestedEventArgs
  ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs = interface;
  PApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs = ^ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.ApplicationSettings.IAccountsSettingsPane,Windows.UI.ApplicationSettings.IAccountsSettingsPaneCommandsRequestedEventArgs>
  TypedEventHandler_2__ApplicationSettings_IAccountsSettingsPane__ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs = interface;
  PTypedEventHandler_2__ApplicationSettings_IAccountsSettingsPane__ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs = ^TypedEventHandler_2__ApplicationSettings_IAccountsSettingsPane__ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs;

  // Windows.UI.ApplicationSettings.IAccountsSettingsPane
  ApplicationSettings_IAccountsSettingsPane = interface;
  PApplicationSettings_IAccountsSettingsPane = ^ApplicationSettings_IAccountsSettingsPane;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Object>
  AsyncOperationCompletedHandler_1__IInspectable = interface;
  PAsyncOperationCompletedHandler_1__IInspectable = ^AsyncOperationCompletedHandler_1__IInspectable;

  // Windows.Foundation.IAsyncOperation`1<Object>
  IAsyncOperation_1__IInspectable = interface;
  PIAsyncOperation_1__IInspectable = ^IAsyncOperation_1__IInspectable;

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

  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneCommandsRequestedEventArgs2
  ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs2 = interface;
  PApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs2 = ^ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs2;

  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics
  ApplicationSettings_IAccountsSettingsPaneStatics = interface;
  PApplicationSettings_IAccountsSettingsPaneStatics = ^ApplicationSettings_IAccountsSettingsPaneStatics;

  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics2
  ApplicationSettings_IAccountsSettingsPaneStatics2 = interface;
  PApplicationSettings_IAccountsSettingsPaneStatics2 = ^ApplicationSettings_IAccountsSettingsPaneStatics2;

  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics3
  ApplicationSettings_IAccountsSettingsPaneStatics3 = interface;
  PApplicationSettings_IAccountsSettingsPaneStatics3 = ^ApplicationSettings_IAccountsSettingsPaneStatics3;

  // Windows.UI.ApplicationSettings.ISettingsCommandFactory
  ApplicationSettings_ISettingsCommandFactory = interface;
  PApplicationSettings_ISettingsCommandFactory = ^ApplicationSettings_ISettingsCommandFactory;

  // Windows.UI.ApplicationSettings.ISettingsCommandStatics
  ApplicationSettings_ISettingsCommandStatics = interface;
  PApplicationSettings_ISettingsCommandStatics = ^ApplicationSettings_ISettingsCommandStatics;

  // Windows.UI.ApplicationSettings.IWebAccountCommandFactory
  ApplicationSettings_IWebAccountCommandFactory = interface;
  PApplicationSettings_IWebAccountCommandFactory = ^ApplicationSettings_IWebAccountCommandFactory;

  // Windows.UI.ApplicationSettings.IWebAccountProviderCommandFactory
  ApplicationSettings_IWebAccountProviderCommandFactory = interface;
  PApplicationSettings_IWebAccountProviderCommandFactory = ^ApplicationSettings_IWebAccountProviderCommandFactory;

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

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueueTimer,Object>
  TypedEventHandler_2__IDispatcherQueueTimer__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueueTimer__IInspectable = ^TypedEventHandler_2__IDispatcherQueueTimer__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.System.IDispatcherQueue,Object>
  TypedEventHandler_2__IDispatcherQueue__IInspectable = interface;
  PTypedEventHandler_2__IDispatcherQueue__IInspectable = ^TypedEventHandler_2__IDispatcherQueue__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<Single>
  IIterator_1__Single = interface;
  PIIterator_1__Single = ^IIterator_1__Single;

  // Windows.Foundation.Collections.IIterable`1<Single>
  IIterable_1__Single = interface;
  PIIterable_1__Single = ^IIterable_1__Single;

  // Windows.Foundation.Collections.IVectorView`1<Single>
  IVectorView_1__Single = interface;
  PIVectorView_1__Single = ^IVectorView_1__Single;

  // Windows.Foundation.IReference`1<Single>
  IReference_1__Single = interface;
  PIReference_1__Single = ^IReference_1__Single;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface;
  PTypedEventHandler_2__IMemoryBufferReference__IInspectable = ^TypedEventHandler_2__IMemoryBufferReference__IInspectable;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Object>
  TypedEventHandler_2__ICoreWindow__IInspectable = interface;
  PTypedEventHandler_2__ICoreWindow__IInspectable = ^TypedEventHandler_2__ICoreWindow__IInspectable;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Popups.IUICommand>
  AsyncOperationCompletedHandler_1__Popups_IUICommand = interface;
  PAsyncOperationCompletedHandler_1__Popups_IUICommand = ^AsyncOperationCompletedHandler_1__Popups_IUICommand;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Popups.IUICommand>
  IAsyncOperation_1__Popups_IUICommand = interface;
  PIAsyncOperation_1__Popups_IUICommand = ^IAsyncOperation_1__Popups_IUICommand;

  // Windows.UI.IUIContentRoot
  IUIContentRoot = interface;
  PIUIContentRoot = ^IUIContentRoot;

  // Windows.UI.WindowManagement.IAppWindowFrameStyle
  WindowManagement_IAppWindowFrameStyle = interface;
  PWindowManagement_IAppWindowFrameStyle = ^WindowManagement_IAppWindowFrameStyle;

  // Windows.UI.WindowManagement.IAppWindowPresentationConfiguration
  WindowManagement_IAppWindowPresentationConfiguration = interface;
  PWindowManagement_IAppWindowPresentationConfiguration = ^WindowManagement_IAppWindowPresentationConfiguration;

  // Windows.UI.WindowManagement.IAppWindowPresenter
  WindowManagement_IAppWindowPresenter = interface;
  PWindowManagement_IAppWindowPresenter = ^WindowManagement_IAppWindowPresenter;

  // Windows.UI.WindowManagement.IAppWindowTitleBarVisibility
  WindowManagement_IAppWindowTitleBarVisibility = interface;
  PWindowManagement_IAppWindowTitleBarVisibility = ^WindowManagement_IAppWindowTitleBarVisibility;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IDisplayRegion,Object>
  TypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable = interface;
  PTypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable = ^TypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable;

  // Windows.UI.WindowManagement.IAppWindowPlacement
  WindowManagement_IAppWindowPlacement = interface;
  PWindowManagement_IAppWindowPlacement = ^WindowManagement_IAppWindowPlacement;

  // Windows.UI.WindowManagement.IAppWindowChangedEventArgs
  WindowManagement_IAppWindowChangedEventArgs = interface;
  PWindowManagement_IAppWindowChangedEventArgs = ^WindowManagement_IAppWindowChangedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IAppWindow,Windows.UI.WindowManagement.IAppWindowChangedEventArgs>
  TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowChangedEventArgs = interface;
  PTypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowChangedEventArgs = ^TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowChangedEventArgs;

  // Windows.UI.WindowManagement.IAppWindowClosedEventArgs
  WindowManagement_IAppWindowClosedEventArgs = interface;
  PWindowManagement_IAppWindowClosedEventArgs = ^WindowManagement_IAppWindowClosedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IAppWindow,Windows.UI.WindowManagement.IAppWindowClosedEventArgs>
  TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowClosedEventArgs = interface;
  PTypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowClosedEventArgs = ^TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowClosedEventArgs;

  // Windows.UI.WindowManagement.IAppWindowCloseRequestedEventArgs
  WindowManagement_IAppWindowCloseRequestedEventArgs = interface;
  PWindowManagement_IAppWindowCloseRequestedEventArgs = ^WindowManagement_IAppWindowCloseRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IAppWindow,Windows.UI.WindowManagement.IAppWindowCloseRequestedEventArgs>
  TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowCloseRequestedEventArgs = interface;
  PTypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowCloseRequestedEventArgs = ^TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowCloseRequestedEventArgs;

  // Windows.UI.WindowManagement.IAppWindow
  WindowManagement_IAppWindow = interface;
  PWindowManagement_IAppWindow = ^WindowManagement_IAppWindow;

  // Windows.UI.IColorHelper
  IColorHelper = interface;
  PIColorHelper = ^IColorHelper;

  // Windows.UI.IColorHelperStatics
  IColorHelperStatics = interface;
  PIColorHelperStatics = ^IColorHelperStatics;

  // Windows.UI.IColorHelperStatics2
  IColorHelperStatics2 = interface;
  PIColorHelperStatics2 = ^IColorHelperStatics2;

  // Windows.UI.IColors
  IColors = interface;
  PIColors = ^IColors;

  // Windows.UI.IColorsStatics
  IColorsStatics = interface;
  PIColorsStatics = ^IColorsStatics;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface;
  PTypedEventHandler_2__Activation_ISplashScreen__IInspectable = ^TypedEventHandler_2__Activation_ISplashScreen__IInspectable;

  // Windows.Foundation.Collections.IIterator`1<UInt32>
  IIterator_1__Cardinal = interface;
  PIIterator_1__Cardinal = ^IIterator_1__Cardinal;

  // Windows.Foundation.Collections.IIterable`1<UInt32>
  IIterable_1__Cardinal = interface;
  PIIterable_1__Cardinal = ^IIterable_1__Cardinal;

  // Windows.Foundation.Collections.IVectorView`1<UInt32>
  IVectorView_1__Cardinal = interface;
  PIVectorView_1__Cardinal = ^IVectorView_1__Cardinal;

  // Windows.Foundation.IReference`1<Int32>
  IReference_1__Integer = interface;
  PIReference_1__Integer = ^IReference_1__Integer;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Object>
  TypedEventHandler_2__IToastNotification__IInspectable = interface;
  PTypedEventHandler_2__IToastNotification__IInspectable = ^TypedEventHandler_2__IToastNotification__IInspectable;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface;
  PEventHandler_1__IInspectable = ^EventHandler_1__IInspectable;

  // Windows.UI.WindowManagement.IAppWindowPresentationConfigurationFactory
  WindowManagement_IAppWindowPresentationConfigurationFactory = interface;
  PWindowManagement_IAppWindowPresentationConfigurationFactory = ^WindowManagement_IAppWindowPresentationConfigurationFactory;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.WindowManagement.IAppWindow>
  AsyncOperationCompletedHandler_1__WindowManagement_IAppWindow = interface;
  PAsyncOperationCompletedHandler_1__WindowManagement_IAppWindow = ^AsyncOperationCompletedHandler_1__WindowManagement_IAppWindow;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.WindowManagement.IAppWindow>
  IAsyncOperation_1__WindowManagement_IAppWindow = interface;
  PIAsyncOperation_1__WindowManagement_IAppWindow = ^IAsyncOperation_1__WindowManagement_IAppWindow;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.WindowManagement.IWindowingEnvironment>
  IIterator_1__WindowManagement_IWindowingEnvironment = interface;
  PIIterator_1__WindowManagement_IWindowingEnvironment = ^IIterator_1__WindowManagement_IWindowingEnvironment;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.WindowManagement.IWindowingEnvironment>
  IIterable_1__WindowManagement_IWindowingEnvironment = interface;
  PIIterable_1__WindowManagement_IWindowingEnvironment = ^IIterable_1__WindowManagement_IWindowingEnvironment;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.WindowManagement.IWindowingEnvironment>
  IVectorView_1__WindowManagement_IWindowingEnvironment = interface;
  PIVectorView_1__WindowManagement_IWindowingEnvironment = ^IVectorView_1__WindowManagement_IWindowingEnvironment;

  // Windows.UI Enums

  // Windows.UI.ApplicationSettings.SettingsEdgeLocation
  ApplicationSettings_SettingsEdgeLocation = (
    Right = 0,
    Left = 1
  );
  PApplicationSettings_SettingsEdgeLocation = ^ApplicationSettings_SettingsEdgeLocation;

  // Windows.UI.ApplicationSettings.SupportedWebAccountActions
  ApplicationSettings_SupportedWebAccountActions = (
    None = 0,
    Reconnect = 1,
    Remove = 2,
    ViewDetails = 4,
    Manage = 8,
    More = 16
  );
  PApplicationSettings_SupportedWebAccountActions = ^ApplicationSettings_SupportedWebAccountActions;

  // Windows.UI.ApplicationSettings.WebAccountAction
  ApplicationSettings_WebAccountAction = (
    Reconnect = 0,
    Remove = 1,
    ViewDetails = 2,
    Manage = 3,
    More = 4
  );
  PApplicationSettings_WebAccountAction = ^ApplicationSettings_WebAccountAction;

  // Windows.UI.Popups.MessageDialogOptions
  Popups_MessageDialogOptions = (
    None = 0,
    AcceptUserInputAfterDelay = 1
  );
  PPopups_MessageDialogOptions = ^Popups_MessageDialogOptions;

  // Windows.UI.Shell.SecurityAppKind
  Shell_SecurityAppKind = (
    WebProtection = 0
  );
  PShell_SecurityAppKind = ^Shell_SecurityAppKind;

  // Windows.UI.Shell.SecurityAppState
  Shell_SecurityAppState = (
    Disabled = 0,
    Enabled = 1
  );
  PShell_SecurityAppState = ^Shell_SecurityAppState;

  // Windows.UI.Shell.SecurityAppSubstatus
  Shell_SecurityAppSubstatus = (
    Undetermined = 0,
    NoActionNeeded = 1,
    ActionRecommended = 2,
    ActionNeeded = 3
  );
  PShell_SecurityAppSubstatus = ^Shell_SecurityAppSubstatus;

  // Windows.UI.StartScreen.ForegroundText
  StartScreen_ForegroundText = (
    Dark = 0,
    Light = 1
  );
  PStartScreen_ForegroundText = ^StartScreen_ForegroundText;

  // Windows.UI.StartScreen.JumpListItemKind
  StartScreen_JumpListItemKind = (
    Arguments = 0,
    Separator = 1
  );
  PStartScreen_JumpListItemKind = ^StartScreen_JumpListItemKind;

  // Windows.UI.StartScreen.JumpListSystemGroupKind
  StartScreen_JumpListSystemGroupKind = (
    None = 0,
    Frequent = 1,
    Recent = 2
  );
  PStartScreen_JumpListSystemGroupKind = ^StartScreen_JumpListSystemGroupKind;

  // Windows.UI.StartScreen.TileMixedRealityModelActivationBehavior
  StartScreen_TileMixedRealityModelActivationBehavior = (
    Default = 0,
    None = 1
  );
  PStartScreen_TileMixedRealityModelActivationBehavior = ^StartScreen_TileMixedRealityModelActivationBehavior;

  // Windows.UI.StartScreen.TileOptions
  StartScreen_TileOptions = (
    None = 0,
    ShowNameOnLogo = 1,
    ShowNameOnWideLogo = 2,
    CopyOnDeployment = 4
  );
  PStartScreen_TileOptions = ^StartScreen_TileOptions;

  // Windows.UI.StartScreen.TileSize
  StartScreen_TileSize = (
    Default = 0,
    Square30x30 = 1,
    Square70x70 = 2,
    Square150x150 = 3,
    Wide310x150 = 4,
    Square310x310 = 5,
    Square71x71 = 6,
    Square44x44 = 7
  );
  PStartScreen_TileSize = ^StartScreen_TileSize;

  // Windows.UI.WindowManagement.AppWindowClosedReason
  WindowManagement_AppWindowClosedReason = (
    Other = 0,
    AppInitiated = 1,
    UserInitiated = 2
  );
  PWindowManagement_AppWindowClosedReason = ^WindowManagement_AppWindowClosedReason;

  // Windows.UI.WindowManagement.AppWindowFrameStyle
  WindowManagement_AppWindowFrameStyle = (
    Default = 0,
    NoFrame = 1
  );
  PWindowManagement_AppWindowFrameStyle = ^WindowManagement_AppWindowFrameStyle;

  // Windows.UI.WindowManagement.AppWindowPresentationKind
  WindowManagement_AppWindowPresentationKind = (
    Default = 0,
    CompactOverlay = 1,
    FullScreen = 2
  );
  PWindowManagement_AppWindowPresentationKind = ^WindowManagement_AppWindowPresentationKind;

  // Windows.UI.WindowManagement.AppWindowTitleBarVisibility
  WindowManagement_AppWindowTitleBarVisibility = (
    Default = 0,
    AlwaysHidden = 1
  );
  PWindowManagement_AppWindowTitleBarVisibility = ^WindowManagement_AppWindowTitleBarVisibility;

  // Windows.UI Records
  // Windows.UI.ApplicationSettings.ApplicationsSettingsContract
  ApplicationSettings_ApplicationsSettingsContract = record
  end;
  PApplicationSettings_ApplicationsSettingsContract = ^ApplicationSettings_ApplicationsSettingsContract;

  // Windows.UI.Shell.SecurityAppManagerContract
  Shell_SecurityAppManagerContract = record
  end;
  PShell_SecurityAppManagerContract = ^Shell_SecurityAppManagerContract;

  // Windows.UI Interfaces

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Color>
  IIterator_1__Color_Base = interface(IInspectable)
  ['{C4310B12-7AC2-5E5B-B511-E546EEA473B4}']
    function get_Current: Color; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PColor): Cardinal; safecall;
    property Current: Color read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Color>
  IIterator_1__Color = interface(IIterator_1__Color_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Color>
  IIterable_1__Color_Base = interface(IInspectable)
  ['{932EEF5E-2C2F-5EAE-929A-74E973B57C27}']
    function First: IIterator_1__Color; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Color>
  IIterable_1__Color = interface(IIterable_1__Color_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.UI.Color>
  IReference_1__Color = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Color; safecall;
    property Value: Color read get_Value;
  end;

  // Windows.UI.Shell.IAdaptiveCard
  Shell_IAdaptiveCard = interface(IInspectable)
  ['{72D0568C-A274-41CD-82A8-989D40B9B05E}']
    function ToJson: HSTRING; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.WindowManagement.IDisplayRegion>
  IIterator_1__WindowManagement_IDisplayRegion = interface(IInspectable)
  ['{F4670424-1F7D-5265-BE8A-4877DEC91C8A}']
    function get_Current: WindowManagement_IDisplayRegion; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PWindowManagement_IDisplayRegion): Cardinal; safecall;
    property Current: WindowManagement_IDisplayRegion read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.WindowManagement.IDisplayRegion>
  IIterable_1__WindowManagement_IDisplayRegion = interface(IInspectable)
  ['{58DDAF6B-10D3-54AA-A224-C6A33A685119}']
    function First: IIterator_1__WindowManagement_IDisplayRegion; safecall;
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

  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.ICredentialCommand
  ApplicationSettings_ICredentialCommand = interface(IInspectable)
  ['{A5F665E6-6143-4A7A-A971-B017BA978CE2}']
    function get_PasswordCredential: IPasswordCredential; safecall;
    function get_CredentialDeleted: ApplicationSettings_CredentialCommandCredentialDeletedHandler; safecall;
    property CredentialDeleted: ApplicationSettings_CredentialCommandCredentialDeletedHandler read get_CredentialDeleted;
    property PasswordCredential: IPasswordCredential read get_PasswordCredential;
  end;

  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.CredentialCommandCredentialDeletedHandler
  ApplicationSettings_CredentialCommandCredentialDeletedHandler = interface(IUnknown)
  ['{61C0E185-0977-4678-B4E2-98727AFBEED9}']
    procedure Invoke(command: ApplicationSettings_ICredentialCommand); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.WebAccountProviderCommandInvokedHandler
  ApplicationSettings_WebAccountProviderCommandInvokedHandler = interface(IUnknown)
  ['{B7DE5527-4C8F-42DD-84DA-5EC493ABDB9A}']
    procedure Invoke(command: ApplicationSettings_IWebAccountProviderCommand); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IWebAccountProviderCommand
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_WebAccountProviderCommand)]
  ApplicationSettings_IWebAccountProviderCommand = interface(IInspectable)
  ['{D69BDD9A-A0A6-4E9B-88DC-C71E757A3501}']
    function get_WebAccountProvider: IWebAccountProvider; safecall;
    function get_Invoked: ApplicationSettings_WebAccountProviderCommandInvokedHandler; safecall;
    property Invoked: ApplicationSettings_WebAccountProviderCommandInvokedHandler read get_Invoked;
    property WebAccountProvider: IWebAccountProvider read get_WebAccountProvider;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IIterator_1__ApplicationSettings_IWebAccountProviderCommand_Base = interface(IInspectable)
  ['{82D7CD74-8E33-5F06-92FC-915138AACBDE}']
    function get_Current: ApplicationSettings_IWebAccountProviderCommand; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PApplicationSettings_IWebAccountProviderCommand): Cardinal; safecall;
    property Current: ApplicationSettings_IWebAccountProviderCommand read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IIterator_1__ApplicationSettings_IWebAccountProviderCommand = interface(IIterator_1__ApplicationSettings_IWebAccountProviderCommand_Base)
  ['{0589F3D7-4C7B-5DCF-8B69-521E998840E3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IIterable_1__ApplicationSettings_IWebAccountProviderCommand_Base = interface(IInspectable)
  ['{15165367-2E93-59A6-B5C7-16D3B58FD2E7}']
    function First: IIterator_1__ApplicationSettings_IWebAccountProviderCommand; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IIterable_1__ApplicationSettings_IWebAccountProviderCommand = interface(IIterable_1__ApplicationSettings_IWebAccountProviderCommand_Base)
  ['{5D3ADB92-B711-52EB-9CEE-0EF716E42707}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IVectorView_1__ApplicationSettings_IWebAccountProviderCommand = interface(IInspectable)
  ['{6D1C614A-F7CB-5051-8AF9-79B46848FE39}']
    function GetAt(index: Cardinal): ApplicationSettings_IWebAccountProviderCommand; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ApplicationSettings_IWebAccountProviderCommand; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PApplicationSettings_IWebAccountProviderCommand): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IVector_1__ApplicationSettings_IWebAccountProviderCommand_Base = interface(IInspectable)
  ['{D376ABF3-F0C1-5233-9F42-DE531884963E}']
    function GetAt(index: Cardinal): ApplicationSettings_IWebAccountProviderCommand; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ApplicationSettings_IWebAccountProviderCommand; safecall;
    function IndexOf(value: ApplicationSettings_IWebAccountProviderCommand; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ApplicationSettings_IWebAccountProviderCommand); safecall;
    procedure InsertAt(index: Cardinal; value: ApplicationSettings_IWebAccountProviderCommand); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ApplicationSettings_IWebAccountProviderCommand); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PApplicationSettings_IWebAccountProviderCommand): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PApplicationSettings_IWebAccountProviderCommand); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.IWebAccountProviderCommand>
  IVector_1__ApplicationSettings_IWebAccountProviderCommand = interface(IVector_1__ApplicationSettings_IWebAccountProviderCommand_Base)
  ['{5E4F3A00-0619-55EB-BDC5-B5AC43E5E153}']
  end;

  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IWebAccountInvokedArgs
  ApplicationSettings_IWebAccountInvokedArgs = interface(IInspectable)
  ['{E7ABCC40-A1D8-4C5D-9A7F-1D34B2F90AD2}']
    function get_Action: ApplicationSettings_WebAccountAction; safecall;
    property Action: ApplicationSettings_WebAccountAction read get_Action;
  end;

  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.WebAccountCommandInvokedHandler
  ApplicationSettings_WebAccountCommandInvokedHandler = interface(IUnknown)
  ['{1EE6E459-1705-4A9A-B599-A0C3D6921973}']
    procedure Invoke(command: ApplicationSettings_IWebAccountCommand; args: ApplicationSettings_IWebAccountInvokedArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IWebAccountCommand
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_WebAccountCommand)]
  ApplicationSettings_IWebAccountCommand = interface(IInspectable)
  ['{CAA39398-9CFA-4246-B0C4-A913A3896541}']
    function get_WebAccount: IWebAccount; safecall;
    function get_Invoked: ApplicationSettings_WebAccountCommandInvokedHandler; safecall;
    function get_Actions: ApplicationSettings_SupportedWebAccountActions; safecall;
    property Actions: ApplicationSettings_SupportedWebAccountActions read get_Actions;
    property Invoked: ApplicationSettings_WebAccountCommandInvokedHandler read get_Invoked;
    property WebAccount: IWebAccount read get_WebAccount;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IIterator_1__ApplicationSettings_IWebAccountCommand_Base = interface(IInspectable)
  ['{8CBB62B6-BD9C-5486-9D14-9CC4627B32D4}']
    function get_Current: ApplicationSettings_IWebAccountCommand; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PApplicationSettings_IWebAccountCommand): Cardinal; safecall;
    property Current: ApplicationSettings_IWebAccountCommand read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IIterator_1__ApplicationSettings_IWebAccountCommand = interface(IIterator_1__ApplicationSettings_IWebAccountCommand_Base)
  ['{E620D41D-D00D-54D0-B860-4A421577D0B1}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IIterable_1__ApplicationSettings_IWebAccountCommand_Base = interface(IInspectable)
  ['{BD0D999C-B2BA-51B2-BCC0-D4A5CD821555}']
    function First: IIterator_1__ApplicationSettings_IWebAccountCommand; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IIterable_1__ApplicationSettings_IWebAccountCommand = interface(IIterable_1__ApplicationSettings_IWebAccountCommand_Base)
  ['{3629C358-6F4E-58FD-80EE-0E5F1F4082E8}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IVectorView_1__ApplicationSettings_IWebAccountCommand = interface(IInspectable)
  ['{B927432C-66BF-5A20-ADFE-25DB09CA2B30}']
    function GetAt(index: Cardinal): ApplicationSettings_IWebAccountCommand; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ApplicationSettings_IWebAccountCommand; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PApplicationSettings_IWebAccountCommand): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IVector_1__ApplicationSettings_IWebAccountCommand_Base = interface(IInspectable)
  ['{64E864C8-7FEF-5DF5-A624-50B577F48554}']
    function GetAt(index: Cardinal): ApplicationSettings_IWebAccountCommand; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ApplicationSettings_IWebAccountCommand; safecall;
    function IndexOf(value: ApplicationSettings_IWebAccountCommand; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ApplicationSettings_IWebAccountCommand); safecall;
    procedure InsertAt(index: Cardinal; value: ApplicationSettings_IWebAccountCommand); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ApplicationSettings_IWebAccountCommand); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PApplicationSettings_IWebAccountCommand): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PApplicationSettings_IWebAccountCommand); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.IWebAccountCommand>
  IVector_1__ApplicationSettings_IWebAccountCommand = interface(IVector_1__ApplicationSettings_IWebAccountCommand_Base)
  ['{58120E09-4156-5FAB-B9BB-8909C18470EE}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IIterator_1__ApplicationSettings_ICredentialCommand_Base = interface(IInspectable)
  ['{9F1177F1-85BB-5CD0-9B08-A0B47A764C75}']
    function get_Current: ApplicationSettings_ICredentialCommand; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PApplicationSettings_ICredentialCommand): Cardinal; safecall;
    property Current: ApplicationSettings_ICredentialCommand read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IIterator_1__ApplicationSettings_ICredentialCommand = interface(IIterator_1__ApplicationSettings_ICredentialCommand_Base)
  ['{B8A25360-140B-5563-8918-58B6E53E057A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IIterable_1__ApplicationSettings_ICredentialCommand_Base = interface(IInspectable)
  ['{883ED18D-4DBB-58F2-8FD2-E4B018509553}']
    function First: IIterator_1__ApplicationSettings_ICredentialCommand; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IIterable_1__ApplicationSettings_ICredentialCommand = interface(IIterable_1__ApplicationSettings_ICredentialCommand_Base)
  ['{E36B7DB3-5D4F-501A-AAD3-062A4D8927A5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IVectorView_1__ApplicationSettings_ICredentialCommand = interface(IInspectable)
  ['{CFE85330-C245-5639-B839-4E548E5E6B17}']
    function GetAt(index: Cardinal): ApplicationSettings_ICredentialCommand; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: ApplicationSettings_ICredentialCommand; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PApplicationSettings_ICredentialCommand): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IVector_1__ApplicationSettings_ICredentialCommand_Base = interface(IInspectable)
  ['{B6AF1CB5-F60E-5B08-B312-2EB51135CFC6}']
    function GetAt(index: Cardinal): ApplicationSettings_ICredentialCommand; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__ApplicationSettings_ICredentialCommand; safecall;
    function IndexOf(value: ApplicationSettings_ICredentialCommand; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: ApplicationSettings_ICredentialCommand); safecall;
    procedure InsertAt(index: Cardinal; value: ApplicationSettings_ICredentialCommand); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: ApplicationSettings_ICredentialCommand); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PApplicationSettings_ICredentialCommand): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PApplicationSettings_ICredentialCommand); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.ApplicationSettings.ICredentialCommand>
  IVector_1__ApplicationSettings_ICredentialCommand = interface(IVector_1__ApplicationSettings_ICredentialCommand_Base)
  ['{458D5549-557E-5756-B5CA-13209BA7D91E}']
  end;

  // UsedAPI Interface
  // Windows.UI.Popups.UICommandInvokedHandler
  Popups_UICommandInvokedHandler = interface(IUnknown)
  ['{DAF77A4F-C27A-4298-9AC6-2922C45E7DA6}']
    procedure Invoke(command: Popups_IUICommand); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Popups.IUICommand
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_SettingsCommand)]
  Popups_IUICommand = interface(IInspectable)
  ['{4FF93A75-4145-47FF-AC7F-DFF1C1FA5B0F}']
    function get_Label: HSTRING; safecall;
    procedure put_Label(value: HSTRING); safecall;
    function get_Invoked: Popups_UICommandInvokedHandler; safecall;
    procedure put_Invoked(value: Popups_UICommandInvokedHandler); safecall;
    function get_Id: IInspectable; safecall;
    procedure put_Id(value: IInspectable); safecall;
    property Id: IInspectable read get_Id write put_Id;
    property Invoked: Popups_UICommandInvokedHandler read get_Invoked write put_Invoked;
    property &Label: HSTRING read get_Label write put_Label;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Popups.IUICommand>
  IIterator_1__Popups_IUICommand_Base = interface(IInspectable)
  ['{2F071C24-4A58-5A00-A294-C7162E98C2A0}']
    function get_Current: Popups_IUICommand; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PPopups_IUICommand): Cardinal; safecall;
    property Current: Popups_IUICommand read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Popups.IUICommand>
  IIterator_1__Popups_IUICommand = interface(IIterator_1__Popups_IUICommand_Base)
  ['{F45DB3D3-7299-57CE-A73E-297CF0AF3083}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Popups.IUICommand>
  IIterable_1__Popups_IUICommand_Base = interface(IInspectable)
  ['{6308E7E8-CB85-5339-A3E9-9A7500D19C68}']
    function First: IIterator_1__Popups_IUICommand; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Popups.IUICommand>
  IIterable_1__Popups_IUICommand = interface(IIterable_1__Popups_IUICommand_Base)
  ['{E63DE42B-53C3-5E07-90D3-98172D545412}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Popups.IUICommand>
  IVectorView_1__Popups_IUICommand = interface(IInspectable)
  ['{ED1165E6-F377-5B00-8172-93C1BD04DEB4}']
    function GetAt(index: Cardinal): Popups_IUICommand; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Popups_IUICommand; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPopups_IUICommand): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Popups.IUICommand>
  IVector_1__Popups_IUICommand_Base = interface(IInspectable)
  ['{10BD9CDD-3767-5E96-9022-F00F9CBD6241}']
    function GetAt(index: Cardinal): Popups_IUICommand; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Popups_IUICommand; safecall;
    function IndexOf(value: Popups_IUICommand; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: Popups_IUICommand); safecall;
    procedure InsertAt(index: Cardinal; value: Popups_IUICommand); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: Popups_IUICommand); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPopups_IUICommand): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPopups_IUICommand); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.UI.Popups.IUICommand>
  IVector_1__Popups_IUICommand = interface(IVector_1__Popups_IUICommand_Base)
  ['{105139A1-DCB8-5F65-97EF-CB1BF0B75F9D}']
  end;

  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneEventDeferral
  ApplicationSettings_IAccountsSettingsPaneEventDeferral = interface(IInspectable)
  ['{CBF25D3F-E5BA-40EF-93DA-65E096E5FB04}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneCommandsRequestedEventArgs
  ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs = interface(IInspectable)
  ['{3B68C099-DB19-45D0-9ABF-95D3773C9330}']
    function get_WebAccountProviderCommands: IVector_1__ApplicationSettings_IWebAccountProviderCommand; safecall;
    function get_WebAccountCommands: IVector_1__ApplicationSettings_IWebAccountCommand; safecall;
    function get_CredentialCommands: IVector_1__ApplicationSettings_ICredentialCommand; safecall;
    function get_Commands: IVector_1__Popups_IUICommand; safecall;
    function get_HeaderText: HSTRING; safecall;
    procedure put_HeaderText(value: HSTRING); safecall;
    function GetDeferral: ApplicationSettings_IAccountsSettingsPaneEventDeferral; safecall;
    property Commands: IVector_1__Popups_IUICommand read get_Commands;
    property CredentialCommands: IVector_1__ApplicationSettings_ICredentialCommand read get_CredentialCommands;
    property HeaderText: HSTRING read get_HeaderText write put_HeaderText;
    property WebAccountCommands: IVector_1__ApplicationSettings_IWebAccountCommand read get_WebAccountCommands;
    property WebAccountProviderCommands: IVector_1__ApplicationSettings_IWebAccountProviderCommand read get_WebAccountProviderCommands;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.ApplicationSettings.IAccountsSettingsPane,Windows.UI.ApplicationSettings.IAccountsSettingsPaneCommandsRequestedEventArgs>
  TypedEventHandler_2__ApplicationSettings_IAccountsSettingsPane__ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{69B8847E-7D72-5A15-BC1C-4CA39C93B162}']
    procedure Invoke(sender: ApplicationSettings_IAccountsSettingsPane; args: ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.ApplicationSettings.IAccountsSettingsPane,Windows.UI.ApplicationSettings.IAccountsSettingsPaneCommandsRequestedEventArgs>
  TypedEventHandler_2__ApplicationSettings_IAccountsSettingsPane__ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs = interface(TypedEventHandler_2__ApplicationSettings_IAccountsSettingsPane__ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs_Delegate_Base)
  ['{F355D7FC-61F8-5271-A250-80C756A1DE3A}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IAccountsSettingsPane
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_AccountsSettingsPane)]
  ApplicationSettings_IAccountsSettingsPane = interface(IInspectable)
  ['{81EA942C-4F09-4406-A538-838D9B14B7E6}']
    function add_AccountCommandsRequested(handler: TypedEventHandler_2__ApplicationSettings_IAccountsSettingsPane__ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_AccountCommandsRequested(cookie: EventRegistrationToken); safecall;
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

  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneCommandsRequestedEventArgs2
  ApplicationSettings_IAccountsSettingsPaneCommandsRequestedEventArgs2 = interface(IInspectable)
  ['{362F7BAD-4E37-4967-8C40-E78EE7A1E5BB}']
    function get_User: IUser; safecall;
    property User: IUser read get_User;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_AccountsSettingsPane)]
  ApplicationSettings_IAccountsSettingsPaneStatics = interface(IInspectable)
  ['{561F8B60-B0EC-4150-A8DC-208EE44B068A}']
    function GetForCurrentView: ApplicationSettings_IAccountsSettingsPane; safecall;
    procedure Show; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics2
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_AccountsSettingsPane)]
  ApplicationSettings_IAccountsSettingsPaneStatics2 = interface(IInspectable)
  ['{D21DF7C2-CE0D-484F-B8E8-E823C215765E}']
    function ShowManageAccountsAsync: IAsyncAction; safecall;
    function ShowAddAccountAsync: IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics3
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_AccountsSettingsPane)]
  ApplicationSettings_IAccountsSettingsPaneStatics3 = interface(IInspectable)
  ['{08410458-A2BA-4C6F-B4AC-48F514331216}']
    function ShowManageAccountsForUserAsync(user: IUser): IAsyncAction; safecall;
    function ShowAddAccountForUserAsync(user: IUser): IAsyncAction; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.ISettingsCommandFactory
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_SettingsCommand)]
  ApplicationSettings_ISettingsCommandFactory = interface(IInspectable)
  ['{68E15B33-1C83-433A-AA5A-CEEEA5BD4764}']
    function CreateSettingsCommand(settingsCommandId: IInspectable; &label: HSTRING; handler: Popups_UICommandInvokedHandler): Popups_IUICommand; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.ISettingsCommandStatics
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_SettingsCommand)]
  ApplicationSettings_ISettingsCommandStatics = interface(IInspectable)
  ['{749AE954-2F69-4B17-8ABA-D05CE5778E46}']
    function get_AccountsCommand: Popups_IUICommand; safecall;
    property AccountsCommand: Popups_IUICommand read get_AccountsCommand;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IWebAccountCommandFactory
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_WebAccountCommand)]
  ApplicationSettings_IWebAccountCommandFactory = interface(IInspectable)
  ['{BFA6CDFF-2F2D-42F5-81DE-1D56BAFC496D}']
    function CreateWebAccountCommand(webAccount: IWebAccount; invoked: ApplicationSettings_WebAccountCommandInvokedHandler; actions: ApplicationSettings_SupportedWebAccountActions): ApplicationSettings_IWebAccountCommand; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.ApplicationSettings.IWebAccountProviderCommandFactory
  [WinRTClassNameAttribute(SWindows_UI_ApplicationSettings_WebAccountProviderCommand)]
  ApplicationSettings_IWebAccountProviderCommandFactory = interface(IInspectable)
  ['{D5658A1B-B176-4776-8469-A9D3FF0B3F59}']
    function CreateWebAccountProviderCommand(webAccountProvider: IWebAccountProvider; invoked: ApplicationSettings_WebAccountProviderCommandInvokedHandler): ApplicationSettings_IWebAccountProviderCommand; safecall;
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

  // Windows.Foundation.Collections.IIterator`1<Single>
  IIterator_1__Single = interface(IInspectable)
  ['{42614E61-B0AA-5E72-9354-2771DB20B7A8}']
    function get_Current: Single; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSingle): Cardinal; safecall;
    property Current: Single read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Single>
  IIterable_1__Single = interface(IInspectable)
  ['{B01BEE51-063A-5FDA-BD72-D76637BB8CB8}']
    function First: IIterator_1__Single; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Single>
  IVectorView_1__Single = interface(IInspectable)
  ['{7BCA64FD-150C-5D50-B56B-9F4F474C5930}']
    function GetAt(index: Cardinal): Single; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Single; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSingle): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.IReference`1<Single>
  IReference_1__Single = interface(IInspectable)
  ['{719CC2BA-3E76-5DEF-9F1A-38D85A145EA8}']
    function get_Value: Single; safecall;
    property Value: Single read get_Value;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface(IUnknown)
  ['{F4637D4A-0760-5431-BFC0-24EB1D4F6C4F}']
    procedure Invoke(sender: IMemoryBufferReference; args: IInspectable); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Object>
  TypedEventHandler_2__ICoreWindow__IInspectable_Delegate_Base = interface(IUnknown)
  ['{6368AE3D-52D4-5290-B936-717A9ACF5BEA}']
    procedure Invoke(sender: ICoreWindow; args: IInspectable); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Core.ICoreWindow,Object>
  TypedEventHandler_2__ICoreWindow__IInspectable = interface(TypedEventHandler_2__ICoreWindow__IInspectable_Delegate_Base)
  ['{B36B7AF8-9A47-5035-B15C-4B124BDFC849}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Popups.IUICommand>
  AsyncOperationCompletedHandler_1__Popups_IUICommand_Delegate_Base = interface(IUnknown)
  ['{DD33FD5B-A24D-5A44-91FE-DD6441770103}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__Popups_IUICommand; asyncStatus: AsyncStatus); safecall;
  end;
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.Popups.IUICommand>
  AsyncOperationCompletedHandler_1__Popups_IUICommand = interface(AsyncOperationCompletedHandler_1__Popups_IUICommand_Delegate_Base)
  ['{DD33FD5B-A24D-5A44-91FE-DD6441770103}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Popups.IUICommand>
  IAsyncOperation_1__Popups_IUICommand_Base = interface(IInspectable)
  ['{B8770535-6A4B-52B1-B578-F3CDC5007A1F}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__Popups_IUICommand); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__Popups_IUICommand; safecall;
    function GetResults: Popups_IUICommand; safecall;
    property Completed: AsyncOperationCompletedHandler_1__Popups_IUICommand read get_Completed write put_Completed;
  end;
  // Windows.Foundation.IAsyncOperation`1<Windows.UI.Popups.IUICommand>
  IAsyncOperation_1__Popups_IUICommand = interface(IAsyncOperation_1__Popups_IUICommand_Base)
  ['{B8770535-6A4B-52B1-B578-F3CDC5007A1F}']
  end;

  // UsedAPI Interface
  // Windows.UI.IUIContentRoot
  IUIContentRoot = interface(IInspectable)
  ['{1DFCBAC6-B36B-5CB9-9BC5-2B7A0EDDC378}']
    function get_UIContext: IUIContext; safecall;
    property UIContext: IUIContext read get_UIContext;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowFrameStyle
  WindowManagement_IAppWindowFrameStyle = interface(IInspectable)
  ['{AC412946-E1AC-5230-944A-C60873DCF4A9}']
    function GetFrameStyle: WindowManagement_AppWindowFrameStyle; safecall;
    procedure SetFrameStyle(frameStyle: WindowManagement_AppWindowFrameStyle); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowPresentationConfiguration
  WindowManagement_IAppWindowPresentationConfiguration = interface(IInspectable)
  ['{B5A43EE3-DF33-5E67-BD31-1072457300DF}']
    function get_Kind: WindowManagement_AppWindowPresentationKind; safecall;
    property Kind: WindowManagement_AppWindowPresentationKind read get_Kind;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowPresenter
  WindowManagement_IAppWindowPresenter = interface(IInspectable)
  ['{5AE9ED73-E1FD-5317-AD78-5A3ED271BBDE}']
    function GetConfiguration: WindowManagement_IAppWindowPresentationConfiguration; safecall;
    function IsPresentationSupported(presentationKind: WindowManagement_AppWindowPresentationKind): Boolean; safecall;
    function RequestPresentation(configuration: WindowManagement_IAppWindowPresentationConfiguration): Boolean; overload; safecall;
    function RequestPresentation(presentationKind: WindowManagement_AppWindowPresentationKind): Boolean; overload; safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowTitleBarVisibility
  WindowManagement_IAppWindowTitleBarVisibility = interface(IInspectable)
  ['{A215A4E3-6E7E-5651-8C3B-624819528154}']
    function GetPreferredVisibility: WindowManagement_AppWindowTitleBarVisibility; safecall;
    procedure SetPreferredVisibility(visibilityMode: WindowManagement_AppWindowTitleBarVisibility); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IDisplayRegion,Object>
  TypedEventHandler_2__WindowManagement_IDisplayRegion__IInspectable = interface(IUnknown)
  ['{8FF84463-41EF-50C0-B82E-A713DC7EDF18}']
    procedure Invoke(sender: WindowManagement_IDisplayRegion; args: IInspectable); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowPlacement
  WindowManagement_IAppWindowPlacement = interface(IInspectable)
  ['{03DC815E-E7A9-5857-9C03-7D670594410E}']
    function get_DisplayRegion: WindowManagement_IDisplayRegion; safecall;
    function get_Offset: TPointF; safecall;
    function get_Size: TSizeF; safecall;
    property DisplayRegion: WindowManagement_IDisplayRegion read get_DisplayRegion;
    property Offset: TPointF read get_Offset;
    property Size: TSizeF read get_Size;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowChangedEventArgs
  WindowManagement_IAppWindowChangedEventArgs = interface(IInspectable)
  ['{1DE1F3BE-A655-55AD-B2B6-EB240F880356}']
    function get_DidAvailableWindowPresentationsChange: Boolean; safecall;
    function get_DidDisplayRegionsChange: Boolean; safecall;
    function get_DidFrameChange: Boolean; safecall;
    function get_DidSizeChange: Boolean; safecall;
    function get_DidTitleBarChange: Boolean; safecall;
    function get_DidVisibilityChange: Boolean; safecall;
    function get_DidWindowingEnvironmentChange: Boolean; safecall;
    function get_DidWindowPresentationChange: Boolean; safecall;
    property DidAvailableWindowPresentationsChange: Boolean read get_DidAvailableWindowPresentationsChange;
    property DidDisplayRegionsChange: Boolean read get_DidDisplayRegionsChange;
    property DidFrameChange: Boolean read get_DidFrameChange;
    property DidSizeChange: Boolean read get_DidSizeChange;
    property DidTitleBarChange: Boolean read get_DidTitleBarChange;
    property DidVisibilityChange: Boolean read get_DidVisibilityChange;
    property DidWindowPresentationChange: Boolean read get_DidWindowPresentationChange;
    property DidWindowingEnvironmentChange: Boolean read get_DidWindowingEnvironmentChange;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IAppWindow,Windows.UI.WindowManagement.IAppWindowChangedEventArgs>
  TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowChangedEventArgs = interface(IUnknown)
  ['{1F0FD3BC-2705-54FD-BBDB-30D2F650E1F1}']
    procedure Invoke(sender: WindowManagement_IAppWindow; args: WindowManagement_IAppWindowChangedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowClosedEventArgs
  WindowManagement_IAppWindowClosedEventArgs = interface(IInspectable)
  ['{CC7DF816-9520-5A06-821E-456AD8B358AA}']
    function get_Reason: WindowManagement_AppWindowClosedReason; safecall;
    property Reason: WindowManagement_AppWindowClosedReason read get_Reason;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IAppWindow,Windows.UI.WindowManagement.IAppWindowClosedEventArgs>
  TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowClosedEventArgs = interface(IUnknown)
  ['{E9005A2D-77D9-586D-9592-BF6D53499DA7}']
    procedure Invoke(sender: WindowManagement_IAppWindow; args: WindowManagement_IAppWindowClosedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindowCloseRequestedEventArgs
  WindowManagement_IAppWindowCloseRequestedEventArgs = interface(IInspectable)
  ['{E9FF01DA-E7A2-57A8-8B5E-39C4003AFDBB}']
    function get_Cancel: Boolean; safecall;
    procedure put_Cancel(value: Boolean); safecall;
    function GetDeferral: IDeferral; safecall;
    property Cancel: Boolean read get_Cancel write put_Cancel;
  end;

  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.UI.WindowManagement.IAppWindow,Windows.UI.WindowManagement.IAppWindowCloseRequestedEventArgs>
  TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowCloseRequestedEventArgs = interface(IUnknown)
  ['{5DA4A3A6-62AD-512F-921C-F54622B3594D}']
    procedure Invoke(sender: WindowManagement_IAppWindow; args: WindowManagement_IAppWindowCloseRequestedEventArgs); safecall;
  end;

  // UsedAPI Interface
  // Windows.UI.WindowManagement.IAppWindow
  WindowManagement_IAppWindow = interface(IInspectable)
  ['{663014A6-B75E-5DBD-995C-F0117FA3FB61}']
    function get_Content: IUIContentRoot; safecall;
    function get_DispatcherQueue: IDispatcherQueue; safecall;
    function get_Frame: WindowManagement_IAppWindowFrameStyle; safecall;
    function get_IsVisible: Boolean; safecall;
    function get_PersistedStateId: HSTRING; safecall;
    procedure put_PersistedStateId(value: HSTRING); safecall;
    function get_Presenter: WindowManagement_IAppWindowPresenter; safecall;
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_TitleBar: WindowManagement_IAppWindowTitleBarVisibility; safecall;
    function get_UIContext: IUIContext; safecall;
    function get_WindowingEnvironment: WindowManagement_IWindowingEnvironment; safecall;
    function CloseAsync: IAsyncAction; safecall;
    function GetPlacement: WindowManagement_IAppWindowPlacement; safecall;
    function GetDisplayRegions: IVectorView_1__WindowManagement_IDisplayRegion; safecall;
    procedure RequestMoveToDisplayRegion(displayRegion: WindowManagement_IDisplayRegion); safecall;
    procedure RequestMoveAdjacentToCurrentView; safecall;
    procedure RequestMoveAdjacentToWindow(anchorWindow: WindowManagement_IAppWindow); safecall;
    procedure RequestMoveRelativeToWindowContent(anchorWindow: WindowManagement_IAppWindow; contentOffset: TPointF); safecall;
    procedure RequestMoveRelativeToCurrentViewContent(contentOffset: TPointF); safecall;
    procedure RequestMoveRelativeToDisplayRegion(displayRegion: WindowManagement_IDisplayRegion; displayRegionOffset: TPointF); safecall;
    procedure RequestSize(frameSize: TSizeF); safecall;
    function TryShowAsync: IAsyncOperation_1__Boolean; safecall;
    function add_Changed(handler: TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Changed(token: EventRegistrationToken); safecall;
    function add_Closed(handler: TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowClosedEventArgs): EventRegistrationToken; safecall;
    procedure remove_Closed(token: EventRegistrationToken); safecall;
    function add_CloseRequested(handler: TypedEventHandler_2__WindowManagement_IAppWindow__WindowManagement_IAppWindowCloseRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_CloseRequested(token: EventRegistrationToken); safecall;
    property Content: IUIContentRoot read get_Content;
    property DispatcherQueue: IDispatcherQueue read get_DispatcherQueue;
    property Frame: WindowManagement_IAppWindowFrameStyle read get_Frame;
    property IsVisible: Boolean read get_IsVisible;
    property PersistedStateId: HSTRING read get_PersistedStateId write put_PersistedStateId;
    property Presenter: WindowManagement_IAppWindowPresenter read get_Presenter;
    property Title: HSTRING read get_Title write put_Title;
    property TitleBar: WindowManagement_IAppWindowTitleBarVisibility read get_TitleBar;
    property UIContext: IUIContext read get_UIContext;
    property WindowingEnvironment: WindowManagement_IWindowingEnvironment read get_WindowingEnvironment;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.IColorHelper
  [WinRTClassNameAttribute(SWindows_UI_ColorHelper)]
  IColorHelper = interface(IInspectable)
  ['{193CFBE7-65C7-4540-AD08-6283BA76879A}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.IColorHelperStatics
  [WinRTClassNameAttribute(SWindows_UI_ColorHelper)]
  IColorHelperStatics = interface(IInspectable)
  ['{8504DBEA-FB6A-4144-A6C2-33499C9284F5}']
    function FromArgb(a: Byte; r: Byte; g: Byte; b: Byte): Color; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.IColorHelperStatics2
  [WinRTClassNameAttribute(SWindows_UI_ColorHelper)]
  IColorHelperStatics2 = interface(IInspectable)
  ['{24D9AF02-6EB0-4B94-855C-FCF0818D9A16}']
    function ToDisplayName(color: Color): HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.IColors
  [WinRTClassNameAttribute(SWindows_UI_Colors)]
  IColors = interface(IInspectable)
  ['{9B8C9326-4CA6-4CE5-8994-9EFF65CABDCC}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.IColorsStatics
  [WinRTClassNameAttribute(SWindows_UI_Colors)]
  IColorsStatics = interface(IInspectable)
  ['{CFF52E04-CCA6-4614-A17E-754910C84A99}']
    function get_AliceBlue: Color; safecall;
    function get_AntiqueWhite: Color; safecall;
    function get_Aqua: Color; safecall;
    function get_Aquamarine: Color; safecall;
    function get_Azure: Color; safecall;
    function get_Beige: Color; safecall;
    function get_Bisque: Color; safecall;
    function get_Black: Color; safecall;
    function get_BlanchedAlmond: Color; safecall;
    function get_Blue: Color; safecall;
    function get_BlueViolet: Color; safecall;
    function get_Brown: Color; safecall;
    function get_BurlyWood: Color; safecall;
    function get_CadetBlue: Color; safecall;
    function get_Chartreuse: Color; safecall;
    function get_Chocolate: Color; safecall;
    function get_Coral: Color; safecall;
    function get_CornflowerBlue: Color; safecall;
    function get_Cornsilk: Color; safecall;
    function get_Crimson: Color; safecall;
    function get_Cyan: Color; safecall;
    function get_DarkBlue: Color; safecall;
    function get_DarkCyan: Color; safecall;
    function get_DarkGoldenrod: Color; safecall;
    function get_DarkGray: Color; safecall;
    function get_DarkGreen: Color; safecall;
    function get_DarkKhaki: Color; safecall;
    function get_DarkMagenta: Color; safecall;
    function get_DarkOliveGreen: Color; safecall;
    function get_DarkOrange: Color; safecall;
    function get_DarkOrchid: Color; safecall;
    function get_DarkRed: Color; safecall;
    function get_DarkSalmon: Color; safecall;
    function get_DarkSeaGreen: Color; safecall;
    function get_DarkSlateBlue: Color; safecall;
    function get_DarkSlateGray: Color; safecall;
    function get_DarkTurquoise: Color; safecall;
    function get_DarkViolet: Color; safecall;
    function get_DeepPink: Color; safecall;
    function get_DeepSkyBlue: Color; safecall;
    function get_DimGray: Color; safecall;
    function get_DodgerBlue: Color; safecall;
    function get_Firebrick: Color; safecall;
    function get_FloralWhite: Color; safecall;
    function get_ForestGreen: Color; safecall;
    function get_Fuchsia: Color; safecall;
    function get_Gainsboro: Color; safecall;
    function get_GhostWhite: Color; safecall;
    function get_Gold: Color; safecall;
    function get_Goldenrod: Color; safecall;
    function get_Gray: Color; safecall;
    function get_Green: Color; safecall;
    function get_GreenYellow: Color; safecall;
    function get_Honeydew: Color; safecall;
    function get_HotPink: Color; safecall;
    function get_IndianRed: Color; safecall;
    function get_Indigo: Color; safecall;
    function get_Ivory: Color; safecall;
    function get_Khaki: Color; safecall;
    function get_Lavender: Color; safecall;
    function get_LavenderBlush: Color; safecall;
    function get_LawnGreen: Color; safecall;
    function get_LemonChiffon: Color; safecall;
    function get_LightBlue: Color; safecall;
    function get_LightCoral: Color; safecall;
    function get_LightCyan: Color; safecall;
    function get_LightGoldenrodYellow: Color; safecall;
    function get_LightGreen: Color; safecall;
    function get_LightGray: Color; safecall;
    function get_LightPink: Color; safecall;
    function get_LightSalmon: Color; safecall;
    function get_LightSeaGreen: Color; safecall;
    function get_LightSkyBlue: Color; safecall;
    function get_LightSlateGray: Color; safecall;
    function get_LightSteelBlue: Color; safecall;
    function get_LightYellow: Color; safecall;
    function get_Lime: Color; safecall;
    function get_LimeGreen: Color; safecall;
    function get_Linen: Color; safecall;
    function get_Magenta: Color; safecall;
    function get_Maroon: Color; safecall;
    function get_MediumAquamarine: Color; safecall;
    function get_MediumBlue: Color; safecall;
    function get_MediumOrchid: Color; safecall;
    function get_MediumPurple: Color; safecall;
    function get_MediumSeaGreen: Color; safecall;
    function get_MediumSlateBlue: Color; safecall;
    function get_MediumSpringGreen: Color; safecall;
    function get_MediumTurquoise: Color; safecall;
    function get_MediumVioletRed: Color; safecall;
    function get_MidnightBlue: Color; safecall;
    function get_MintCream: Color; safecall;
    function get_MistyRose: Color; safecall;
    function get_Moccasin: Color; safecall;
    function get_NavajoWhite: Color; safecall;
    function get_Navy: Color; safecall;
    function get_OldLace: Color; safecall;
    function get_Olive: Color; safecall;
    function get_OliveDrab: Color; safecall;
    function get_Orange: Color; safecall;
    function get_OrangeRed: Color; safecall;
    function get_Orchid: Color; safecall;
    function get_PaleGoldenrod: Color; safecall;
    function get_PaleGreen: Color; safecall;
    function get_PaleTurquoise: Color; safecall;
    function get_PaleVioletRed: Color; safecall;
    function get_PapayaWhip: Color; safecall;
    function get_PeachPuff: Color; safecall;
    function get_Peru: Color; safecall;
    function get_Pink: Color; safecall;
    function get_Plum: Color; safecall;
    function get_PowderBlue: Color; safecall;
    function get_Purple: Color; safecall;
    function get_Red: Color; safecall;
    function get_RosyBrown: Color; safecall;
    function get_RoyalBlue: Color; safecall;
    function get_SaddleBrown: Color; safecall;
    function get_Salmon: Color; safecall;
    function get_SandyBrown: Color; safecall;
    function get_SeaGreen: Color; safecall;
    function get_SeaShell: Color; safecall;
    function get_Sienna: Color; safecall;
    function get_Silver: Color; safecall;
    function get_SkyBlue: Color; safecall;
    function get_SlateBlue: Color; safecall;
    function get_SlateGray: Color; safecall;
    function get_Snow: Color; safecall;
    function get_SpringGreen: Color; safecall;
    function get_SteelBlue: Color; safecall;
    function get_Tan: Color; safecall;
    function get_Teal: Color; safecall;
    function get_Thistle: Color; safecall;
    function get_Tomato: Color; safecall;
    function get_Transparent: Color; safecall;
    function get_Turquoise: Color; safecall;
    function get_Violet: Color; safecall;
    function get_Wheat: Color; safecall;
    function get_White: Color; safecall;
    function get_WhiteSmoke: Color; safecall;
    function get_Yellow: Color; safecall;
    function get_YellowGreen: Color; safecall;
    property AliceBlue: Color read get_AliceBlue;
    property AntiqueWhite: Color read get_AntiqueWhite;
    property Aqua: Color read get_Aqua;
    property Aquamarine: Color read get_Aquamarine;
    property Azure: Color read get_Azure;
    property Beige: Color read get_Beige;
    property Bisque: Color read get_Bisque;
    property Black: Color read get_Black;
    property BlanchedAlmond: Color read get_BlanchedAlmond;
    property Blue: Color read get_Blue;
    property BlueViolet: Color read get_BlueViolet;
    property Brown: Color read get_Brown;
    property BurlyWood: Color read get_BurlyWood;
    property CadetBlue: Color read get_CadetBlue;
    property Chartreuse: Color read get_Chartreuse;
    property Chocolate: Color read get_Chocolate;
    property Coral: Color read get_Coral;
    property CornflowerBlue: Color read get_CornflowerBlue;
    property Cornsilk: Color read get_Cornsilk;
    property Crimson: Color read get_Crimson;
    property Cyan: Color read get_Cyan;
    property DarkBlue: Color read get_DarkBlue;
    property DarkCyan: Color read get_DarkCyan;
    property DarkGoldenrod: Color read get_DarkGoldenrod;
    property DarkGray: Color read get_DarkGray;
    property DarkGreen: Color read get_DarkGreen;
    property DarkKhaki: Color read get_DarkKhaki;
    property DarkMagenta: Color read get_DarkMagenta;
    property DarkOliveGreen: Color read get_DarkOliveGreen;
    property DarkOrange: Color read get_DarkOrange;
    property DarkOrchid: Color read get_DarkOrchid;
    property DarkRed: Color read get_DarkRed;
    property DarkSalmon: Color read get_DarkSalmon;
    property DarkSeaGreen: Color read get_DarkSeaGreen;
    property DarkSlateBlue: Color read get_DarkSlateBlue;
    property DarkSlateGray: Color read get_DarkSlateGray;
    property DarkTurquoise: Color read get_DarkTurquoise;
    property DarkViolet: Color read get_DarkViolet;
    property DeepPink: Color read get_DeepPink;
    property DeepSkyBlue: Color read get_DeepSkyBlue;
    property DimGray: Color read get_DimGray;
    property DodgerBlue: Color read get_DodgerBlue;
    property Firebrick: Color read get_Firebrick;
    property FloralWhite: Color read get_FloralWhite;
    property ForestGreen: Color read get_ForestGreen;
    property Fuchsia: Color read get_Fuchsia;
    property Gainsboro: Color read get_Gainsboro;
    property GhostWhite: Color read get_GhostWhite;
    property Gold: Color read get_Gold;
    property Goldenrod: Color read get_Goldenrod;
    property Gray: Color read get_Gray;
    property Green: Color read get_Green;
    property GreenYellow: Color read get_GreenYellow;
    property Honeydew: Color read get_Honeydew;
    property HotPink: Color read get_HotPink;
    property IndianRed: Color read get_IndianRed;
    property Indigo: Color read get_Indigo;
    property Ivory: Color read get_Ivory;
    property Khaki: Color read get_Khaki;
    property Lavender: Color read get_Lavender;
    property LavenderBlush: Color read get_LavenderBlush;
    property LawnGreen: Color read get_LawnGreen;
    property LemonChiffon: Color read get_LemonChiffon;
    property LightBlue: Color read get_LightBlue;
    property LightCoral: Color read get_LightCoral;
    property LightCyan: Color read get_LightCyan;
    property LightGoldenrodYellow: Color read get_LightGoldenrodYellow;
    property LightGray: Color read get_LightGray;
    property LightGreen: Color read get_LightGreen;
    property LightPink: Color read get_LightPink;
    property LightSalmon: Color read get_LightSalmon;
    property LightSeaGreen: Color read get_LightSeaGreen;
    property LightSkyBlue: Color read get_LightSkyBlue;
    property LightSlateGray: Color read get_LightSlateGray;
    property LightSteelBlue: Color read get_LightSteelBlue;
    property LightYellow: Color read get_LightYellow;
    property Lime: Color read get_Lime;
    property LimeGreen: Color read get_LimeGreen;
    property Linen: Color read get_Linen;
    property Magenta: Color read get_Magenta;
    property Maroon: Color read get_Maroon;
    property MediumAquamarine: Color read get_MediumAquamarine;
    property MediumBlue: Color read get_MediumBlue;
    property MediumOrchid: Color read get_MediumOrchid;
    property MediumPurple: Color read get_MediumPurple;
    property MediumSeaGreen: Color read get_MediumSeaGreen;
    property MediumSlateBlue: Color read get_MediumSlateBlue;
    property MediumSpringGreen: Color read get_MediumSpringGreen;
    property MediumTurquoise: Color read get_MediumTurquoise;
    property MediumVioletRed: Color read get_MediumVioletRed;
    property MidnightBlue: Color read get_MidnightBlue;
    property MintCream: Color read get_MintCream;
    property MistyRose: Color read get_MistyRose;
    property Moccasin: Color read get_Moccasin;
    property NavajoWhite: Color read get_NavajoWhite;
    property Navy: Color read get_Navy;
    property OldLace: Color read get_OldLace;
    property Olive: Color read get_Olive;
    property OliveDrab: Color read get_OliveDrab;
    property Orange: Color read get_Orange;
    property OrangeRed: Color read get_OrangeRed;
    property Orchid: Color read get_Orchid;
    property PaleGoldenrod: Color read get_PaleGoldenrod;
    property PaleGreen: Color read get_PaleGreen;
    property PaleTurquoise: Color read get_PaleTurquoise;
    property PaleVioletRed: Color read get_PaleVioletRed;
    property PapayaWhip: Color read get_PapayaWhip;
    property PeachPuff: Color read get_PeachPuff;
    property Peru: Color read get_Peru;
    property Pink: Color read get_Pink;
    property Plum: Color read get_Plum;
    property PowderBlue: Color read get_PowderBlue;
    property Purple: Color read get_Purple;
    property Red: Color read get_Red;
    property RosyBrown: Color read get_RosyBrown;
    property RoyalBlue: Color read get_RoyalBlue;
    property SaddleBrown: Color read get_SaddleBrown;
    property Salmon: Color read get_Salmon;
    property SandyBrown: Color read get_SandyBrown;
    property SeaGreen: Color read get_SeaGreen;
    property SeaShell: Color read get_SeaShell;
    property Sienna: Color read get_Sienna;
    property Silver: Color read get_Silver;
    property SkyBlue: Color read get_SkyBlue;
    property SlateBlue: Color read get_SlateBlue;
    property SlateGray: Color read get_SlateGray;
    property Snow: Color read get_Snow;
    property SpringGreen: Color read get_SpringGreen;
    property SteelBlue: Color read get_SteelBlue;
    property Tan: Color read get_Tan;
    property Teal: Color read get_Teal;
    property Thistle: Color read get_Thistle;
    property Tomato: Color read get_Tomato;
    property Transparent: Color read get_Transparent;
    property Turquoise: Color read get_Turquoise;
    property Violet: Color read get_Violet;
    property Wheat: Color read get_Wheat;
    property White: Color read get_White;
    property WhiteSmoke: Color read get_WhiteSmoke;
    property Yellow: Color read get_Yellow;
    property YellowGreen: Color read get_YellowGreen;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.Activation.ISplashScreen,Object>
  TypedEventHandler_2__Activation_ISplashScreen__IInspectable = interface(IUnknown)
  ['{359B8887-2FA6-5405-A4AF-642C9FDACC93}']
    procedure Invoke(sender: Activation_ISplashScreen; args: IInspectable); safecall;
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

  // Windows.Foundation.IReference`1<Int32>
  IReference_1__Integer = interface(IInspectable)
  ['{548CEFBD-BC8A-5FA0-8DF2-957440FC8BF4}']
    function get_Value: Integer; safecall;
    property Value: Integer read get_Value;
  end;

  // Windows.Foundation.TypedEventHandler`2<Windows.UI.Notifications.IToastNotification,Object>
  TypedEventHandler_2__IToastNotification__IInspectable = interface(IUnknown)
  ['{93621AAC-6E87-5F7A-AA83-927B2D905518}']
    procedure Invoke(sender: IToastNotification; args: IInspectable); safecall;
  end;

  // Windows.Foundation.EventHandler`1<Object>
  EventHandler_1__IInspectable = interface(IUnknown)
  ['{C50898F6-C536-5F47-8583-8B2C2438A13B}']
    procedure Invoke(sender: IInspectable; args: IInspectable); safecall;
  end;

  // Windows.UI.WindowManagement.IAppWindowPresentationConfigurationFactory
  WindowManagement_IAppWindowPresentationConfigurationFactory = interface(IInspectable)
  ['{FD3606A6-7875-5DE8-84FF-6351EE13DD0D}']
  end;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.UI.WindowManagement.IAppWindow>
  AsyncOperationCompletedHandler_1__WindowManagement_IAppWindow = interface(IUnknown)
  ['{B9804EF8-64CB-5FAA-B83B-018AAE52A63E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__WindowManagement_IAppWindow; asyncStatus: AsyncStatus); safecall;
  end;

  // Windows.Foundation.IAsyncOperation`1<Windows.UI.WindowManagement.IAppWindow>
  IAsyncOperation_1__WindowManagement_IAppWindow = interface(IInspectable)
  ['{0794B92B-B957-5CAC-B79A-8F293E6E9D11}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__WindowManagement_IAppWindow); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__WindowManagement_IAppWindow; safecall;
    function GetResults: WindowManagement_IAppWindow; safecall;
    property Completed: AsyncOperationCompletedHandler_1__WindowManagement_IAppWindow read get_Completed write put_Completed;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.WindowManagement.IWindowingEnvironment>
  IIterator_1__WindowManagement_IWindowingEnvironment = interface(IInspectable)
  ['{83484D4A-D906-5DBD-AF5A-026F987E8F90}']
    function get_Current: WindowManagement_IWindowingEnvironment; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PWindowManagement_IWindowingEnvironment): Cardinal; safecall;
    property Current: WindowManagement_IWindowingEnvironment read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.WindowManagement.IWindowingEnvironment>
  IIterable_1__WindowManagement_IWindowingEnvironment = interface(IInspectable)
  ['{808F55C7-C44C-57A1-B1B4-BB27E28A6876}']
    function First: IIterator_1__WindowManagement_IWindowingEnvironment; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.WindowManagement.IWindowingEnvironment>
  IVectorView_1__WindowManagement_IWindowingEnvironment = interface(IInspectable)
  ['{BE259166-990A-546C-8676-7F705D11756E}']
    function GetAt(index: Cardinal): WindowManagement_IWindowingEnvironment; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: WindowManagement_IWindowingEnvironment; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PWindowManagement_IWindowingEnvironment): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.UI.ApplicationSettings.AccountsSettingsPane
  // DualAPI
  // Implements: Windows.UI.ApplicationSettings.IAccountsSettingsPane
  // Statics: "Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics"
  // Statics: "Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics2"
  // Statics: "Windows.UI.ApplicationSettings.IAccountsSettingsPaneStatics3"
  TApplicationSettings_AccountsSettingsPane = class(TWinRTGenericImportS3<ApplicationSettings_IAccountsSettingsPaneStatics, ApplicationSettings_IAccountsSettingsPaneStatics2, ApplicationSettings_IAccountsSettingsPaneStatics3>)
  public
    // -> ApplicationSettings_IAccountsSettingsPaneStatics
    class function GetForCurrentView: ApplicationSettings_IAccountsSettingsPane; static; inline;
    class procedure Show; static; inline;

    // -> ApplicationSettings_IAccountsSettingsPaneStatics2
    class function ShowManageAccountsAsync: IAsyncAction; static; inline;
    class function ShowAddAccountAsync: IAsyncAction; static; inline;

    // -> ApplicationSettings_IAccountsSettingsPaneStatics3
    class function ShowManageAccountsForUserAsync(user: IUser): IAsyncAction; static; inline;
    class function ShowAddAccountForUserAsync(user: IUser): IAsyncAction; static; inline;
  end;

  // Windows.UI.ApplicationSettings.SettingsCommand
  // DualAPI
  // Implements: Windows.UI.Popups.IUICommand
  // Statics: "Windows.UI.ApplicationSettings.ISettingsCommandStatics"
  // Factory: "Windows.UI.ApplicationSettings.ISettingsCommandFactory"
  TApplicationSettings_SettingsCommand = class(TWinRTGenericImportFS<ApplicationSettings_ISettingsCommandFactory, ApplicationSettings_ISettingsCommandStatics>)
  public
    // -> ApplicationSettings_ISettingsCommandStatics
    class function get_AccountsCommand: Popups_IUICommand; static; inline;
    class property AccountsCommand: Popups_IUICommand read get_AccountsCommand;

    // -> ApplicationSettings_ISettingsCommandFactory
    class function CreateSettingsCommand(settingsCommandId: IInspectable; &label: HSTRING; handler: Popups_UICommandInvokedHandler): Popups_IUICommand; static; inline;
  end;

  // Windows.UI.ApplicationSettings.WebAccountCommand
  // DualAPI
  // Implements: Windows.UI.ApplicationSettings.IWebAccountCommand
  // Factory: "Windows.UI.ApplicationSettings.IWebAccountCommandFactory"
  TApplicationSettings_WebAccountCommand = class(TWinRTGenericImportF<ApplicationSettings_IWebAccountCommandFactory>)
  public
    // -> ApplicationSettings_IWebAccountCommandFactory
    class function CreateWebAccountCommand(webAccount: IWebAccount; invoked: ApplicationSettings_WebAccountCommandInvokedHandler; actions: ApplicationSettings_SupportedWebAccountActions): ApplicationSettings_IWebAccountCommand; static; inline;
  end;

  // Windows.UI.ApplicationSettings.WebAccountProviderCommand
  // DualAPI
  // Implements: Windows.UI.ApplicationSettings.IWebAccountProviderCommand
  // Factory: "Windows.UI.ApplicationSettings.IWebAccountProviderCommandFactory"
  TApplicationSettings_WebAccountProviderCommand = class(TWinRTGenericImportF<ApplicationSettings_IWebAccountProviderCommandFactory>)
  public
    // -> ApplicationSettings_IWebAccountProviderCommandFactory
    class function CreateWebAccountProviderCommand(webAccountProvider: IWebAccountProvider; invoked: ApplicationSettings_WebAccountProviderCommandInvokedHandler): ApplicationSettings_IWebAccountProviderCommand; static; inline;
  end;

  // Windows.UI.ColorHelper
  // Explicitly imported
  // Implements: Windows.UI.IColorHelper
  // Statics: "Windows.UI.IColorHelperStatics"
  // Statics: "Windows.UI.IColorHelperStatics2"
  TColorHelper = class(TWinRTGenericImportS2<IColorHelperStatics, IColorHelperStatics2>)
  public
    // -> IColorHelperStatics
    class function FromArgb(a: Byte; r: Byte; g: Byte; b: Byte): Color; static; inline;

    // -> IColorHelperStatics2
    class function ToDisplayName(color: Color): HSTRING; static; inline;
  end;

  // Windows.UI.Colors
  // Explicitly imported
  // Implements: Windows.UI.IColors
  // Statics: "Windows.UI.IColorsStatics"
  TColors = class(TWinRTGenericImportS<IColorsStatics>)
  public
    // -> IColorsStatics
    class function get_AliceBlue: Color; static; inline;
    class function get_AntiqueWhite: Color; static; inline;
    class function get_Aqua: Color; static; inline;
    class function get_Aquamarine: Color; static; inline;
    class function get_Azure: Color; static; inline;
    class function get_Beige: Color; static; inline;
    class function get_Bisque: Color; static; inline;
    class function get_Black: Color; static; inline;
    class function get_BlanchedAlmond: Color; static; inline;
    class function get_Blue: Color; static; inline;
    class function get_BlueViolet: Color; static; inline;
    class function get_Brown: Color; static; inline;
    class function get_BurlyWood: Color; static; inline;
    class function get_CadetBlue: Color; static; inline;
    class function get_Chartreuse: Color; static; inline;
    class function get_Chocolate: Color; static; inline;
    class function get_Coral: Color; static; inline;
    class function get_CornflowerBlue: Color; static; inline;
    class function get_Cornsilk: Color; static; inline;
    class function get_Crimson: Color; static; inline;
    class function get_Cyan: Color; static; inline;
    class function get_DarkBlue: Color; static; inline;
    class function get_DarkCyan: Color; static; inline;
    class function get_DarkGoldenrod: Color; static; inline;
    class function get_DarkGray: Color; static; inline;
    class function get_DarkGreen: Color; static; inline;
    class function get_DarkKhaki: Color; static; inline;
    class function get_DarkMagenta: Color; static; inline;
    class function get_DarkOliveGreen: Color; static; inline;
    class function get_DarkOrange: Color; static; inline;
    class function get_DarkOrchid: Color; static; inline;
    class function get_DarkRed: Color; static; inline;
    class function get_DarkSalmon: Color; static; inline;
    class function get_DarkSeaGreen: Color; static; inline;
    class function get_DarkSlateBlue: Color; static; inline;
    class function get_DarkSlateGray: Color; static; inline;
    class function get_DarkTurquoise: Color; static; inline;
    class function get_DarkViolet: Color; static; inline;
    class function get_DeepPink: Color; static; inline;
    class function get_DeepSkyBlue: Color; static; inline;
    class function get_DimGray: Color; static; inline;
    class function get_DodgerBlue: Color; static; inline;
    class function get_Firebrick: Color; static; inline;
    class function get_FloralWhite: Color; static; inline;
    class function get_ForestGreen: Color; static; inline;
    class function get_Fuchsia: Color; static; inline;
    class function get_Gainsboro: Color; static; inline;
    class function get_GhostWhite: Color; static; inline;
    class function get_Gold: Color; static; inline;
    class function get_Goldenrod: Color; static; inline;
    class function get_Gray: Color; static; inline;
    class function get_Green: Color; static; inline;
    class function get_GreenYellow: Color; static; inline;
    class function get_Honeydew: Color; static; inline;
    class function get_HotPink: Color; static; inline;
    class function get_IndianRed: Color; static; inline;
    class function get_Indigo: Color; static; inline;
    class function get_Ivory: Color; static; inline;
    class function get_Khaki: Color; static; inline;
    class function get_Lavender: Color; static; inline;
    class function get_LavenderBlush: Color; static; inline;
    class function get_LawnGreen: Color; static; inline;
    class function get_LemonChiffon: Color; static; inline;
    class function get_LightBlue: Color; static; inline;
    class function get_LightCoral: Color; static; inline;
    class function get_LightCyan: Color; static; inline;
    class function get_LightGoldenrodYellow: Color; static; inline;
    class function get_LightGreen: Color; static; inline;
    class function get_LightGray: Color; static; inline;
    class function get_LightPink: Color; static; inline;
    class function get_LightSalmon: Color; static; inline;
    class function get_LightSeaGreen: Color; static; inline;
    class function get_LightSkyBlue: Color; static; inline;
    class function get_LightSlateGray: Color; static; inline;
    class function get_LightSteelBlue: Color; static; inline;
    class function get_LightYellow: Color; static; inline;
    class function get_Lime: Color; static; inline;
    class function get_LimeGreen: Color; static; inline;
    class function get_Linen: Color; static; inline;
    class function get_Magenta: Color; static; inline;
    class function get_Maroon: Color; static; inline;
    class function get_MediumAquamarine: Color; static; inline;
    class function get_MediumBlue: Color; static; inline;
    class function get_MediumOrchid: Color; static; inline;
    class function get_MediumPurple: Color; static; inline;
    class function get_MediumSeaGreen: Color; static; inline;
    class function get_MediumSlateBlue: Color; static; inline;
    class function get_MediumSpringGreen: Color; static; inline;
    class function get_MediumTurquoise: Color; static; inline;
    class function get_MediumVioletRed: Color; static; inline;
    class function get_MidnightBlue: Color; static; inline;
    class function get_MintCream: Color; static; inline;
    class function get_MistyRose: Color; static; inline;
    class function get_Moccasin: Color; static; inline;
    class function get_NavajoWhite: Color; static; inline;
    class function get_Navy: Color; static; inline;
    class function get_OldLace: Color; static; inline;
    class function get_Olive: Color; static; inline;
    class function get_OliveDrab: Color; static; inline;
    class function get_Orange: Color; static; inline;
    class function get_OrangeRed: Color; static; inline;
    class function get_Orchid: Color; static; inline;
    class function get_PaleGoldenrod: Color; static; inline;
    class function get_PaleGreen: Color; static; inline;
    class function get_PaleTurquoise: Color; static; inline;
    class function get_PaleVioletRed: Color; static; inline;
    class function get_PapayaWhip: Color; static; inline;
    class function get_PeachPuff: Color; static; inline;
    class function get_Peru: Color; static; inline;
    class function get_Pink: Color; static; inline;
    class function get_Plum: Color; static; inline;
    class function get_PowderBlue: Color; static; inline;
    class function get_Purple: Color; static; inline;
    class function get_Red: Color; static; inline;
    class function get_RosyBrown: Color; static; inline;
    class function get_RoyalBlue: Color; static; inline;
    class function get_SaddleBrown: Color; static; inline;
    class function get_Salmon: Color; static; inline;
    class function get_SandyBrown: Color; static; inline;
    class function get_SeaGreen: Color; static; inline;
    class function get_SeaShell: Color; static; inline;
    class function get_Sienna: Color; static; inline;
    class function get_Silver: Color; static; inline;
    class function get_SkyBlue: Color; static; inline;
    class function get_SlateBlue: Color; static; inline;
    class function get_SlateGray: Color; static; inline;
    class function get_Snow: Color; static; inline;
    class function get_SpringGreen: Color; static; inline;
    class function get_SteelBlue: Color; static; inline;
    class function get_Tan: Color; static; inline;
    class function get_Teal: Color; static; inline;
    class function get_Thistle: Color; static; inline;
    class function get_Tomato: Color; static; inline;
    class function get_Transparent: Color; static; inline;
    class function get_Turquoise: Color; static; inline;
    class function get_Violet: Color; static; inline;
    class function get_Wheat: Color; static; inline;
    class function get_White: Color; static; inline;
    class function get_WhiteSmoke: Color; static; inline;
    class function get_Yellow: Color; static; inline;
    class function get_YellowGreen: Color; static; inline;
    class property AliceBlue: Color read get_AliceBlue;
    class property AntiqueWhite: Color read get_AntiqueWhite;
    class property Aqua: Color read get_Aqua;
    class property Aquamarine: Color read get_Aquamarine;
    class property Azure: Color read get_Azure;
    class property Beige: Color read get_Beige;
    class property Bisque: Color read get_Bisque;
    class property Black: Color read get_Black;
    class property BlanchedAlmond: Color read get_BlanchedAlmond;
    class property Blue: Color read get_Blue;
    class property BlueViolet: Color read get_BlueViolet;
    class property Brown: Color read get_Brown;
    class property BurlyWood: Color read get_BurlyWood;
    class property CadetBlue: Color read get_CadetBlue;
    class property Chartreuse: Color read get_Chartreuse;
    class property Chocolate: Color read get_Chocolate;
    class property Coral: Color read get_Coral;
    class property CornflowerBlue: Color read get_CornflowerBlue;
    class property Cornsilk: Color read get_Cornsilk;
    class property Crimson: Color read get_Crimson;
    class property Cyan: Color read get_Cyan;
    class property DarkBlue: Color read get_DarkBlue;
    class property DarkCyan: Color read get_DarkCyan;
    class property DarkGoldenrod: Color read get_DarkGoldenrod;
    class property DarkGray: Color read get_DarkGray;
    class property DarkGreen: Color read get_DarkGreen;
    class property DarkKhaki: Color read get_DarkKhaki;
    class property DarkMagenta: Color read get_DarkMagenta;
    class property DarkOliveGreen: Color read get_DarkOliveGreen;
    class property DarkOrange: Color read get_DarkOrange;
    class property DarkOrchid: Color read get_DarkOrchid;
    class property DarkRed: Color read get_DarkRed;
    class property DarkSalmon: Color read get_DarkSalmon;
    class property DarkSeaGreen: Color read get_DarkSeaGreen;
    class property DarkSlateBlue: Color read get_DarkSlateBlue;
    class property DarkSlateGray: Color read get_DarkSlateGray;
    class property DarkTurquoise: Color read get_DarkTurquoise;
    class property DarkViolet: Color read get_DarkViolet;
    class property DeepPink: Color read get_DeepPink;
    class property DeepSkyBlue: Color read get_DeepSkyBlue;
    class property DimGray: Color read get_DimGray;
    class property DodgerBlue: Color read get_DodgerBlue;
    class property Firebrick: Color read get_Firebrick;
    class property FloralWhite: Color read get_FloralWhite;
    class property ForestGreen: Color read get_ForestGreen;
    class property Fuchsia: Color read get_Fuchsia;
    class property Gainsboro: Color read get_Gainsboro;
    class property GhostWhite: Color read get_GhostWhite;
    class property Gold: Color read get_Gold;
    class property Goldenrod: Color read get_Goldenrod;
    class property Gray: Color read get_Gray;
    class property Green: Color read get_Green;
    class property GreenYellow: Color read get_GreenYellow;
    class property Honeydew: Color read get_Honeydew;
    class property HotPink: Color read get_HotPink;
    class property IndianRed: Color read get_IndianRed;
    class property Indigo: Color read get_Indigo;
    class property Ivory: Color read get_Ivory;
    class property Khaki: Color read get_Khaki;
    class property Lavender: Color read get_Lavender;
    class property LavenderBlush: Color read get_LavenderBlush;
    class property LawnGreen: Color read get_LawnGreen;
    class property LemonChiffon: Color read get_LemonChiffon;
    class property LightBlue: Color read get_LightBlue;
    class property LightCoral: Color read get_LightCoral;
    class property LightCyan: Color read get_LightCyan;
    class property LightGoldenrodYellow: Color read get_LightGoldenrodYellow;
    class property LightGray: Color read get_LightGray;
    class property LightGreen: Color read get_LightGreen;
    class property LightPink: Color read get_LightPink;
    class property LightSalmon: Color read get_LightSalmon;
    class property LightSeaGreen: Color read get_LightSeaGreen;
    class property LightSkyBlue: Color read get_LightSkyBlue;
    class property LightSlateGray: Color read get_LightSlateGray;
    class property LightSteelBlue: Color read get_LightSteelBlue;
    class property LightYellow: Color read get_LightYellow;
    class property Lime: Color read get_Lime;
    class property LimeGreen: Color read get_LimeGreen;
    class property Linen: Color read get_Linen;
    class property Magenta: Color read get_Magenta;
    class property Maroon: Color read get_Maroon;
    class property MediumAquamarine: Color read get_MediumAquamarine;
    class property MediumBlue: Color read get_MediumBlue;
    class property MediumOrchid: Color read get_MediumOrchid;
    class property MediumPurple: Color read get_MediumPurple;
    class property MediumSeaGreen: Color read get_MediumSeaGreen;
    class property MediumSlateBlue: Color read get_MediumSlateBlue;
    class property MediumSpringGreen: Color read get_MediumSpringGreen;
    class property MediumTurquoise: Color read get_MediumTurquoise;
    class property MediumVioletRed: Color read get_MediumVioletRed;
    class property MidnightBlue: Color read get_MidnightBlue;
    class property MintCream: Color read get_MintCream;
    class property MistyRose: Color read get_MistyRose;
    class property Moccasin: Color read get_Moccasin;
    class property NavajoWhite: Color read get_NavajoWhite;
    class property Navy: Color read get_Navy;
    class property OldLace: Color read get_OldLace;
    class property Olive: Color read get_Olive;
    class property OliveDrab: Color read get_OliveDrab;
    class property Orange: Color read get_Orange;
    class property OrangeRed: Color read get_OrangeRed;
    class property Orchid: Color read get_Orchid;
    class property PaleGoldenrod: Color read get_PaleGoldenrod;
    class property PaleGreen: Color read get_PaleGreen;
    class property PaleTurquoise: Color read get_PaleTurquoise;
    class property PaleVioletRed: Color read get_PaleVioletRed;
    class property PapayaWhip: Color read get_PapayaWhip;
    class property PeachPuff: Color read get_PeachPuff;
    class property Peru: Color read get_Peru;
    class property Pink: Color read get_Pink;
    class property Plum: Color read get_Plum;
    class property PowderBlue: Color read get_PowderBlue;
    class property Purple: Color read get_Purple;
    class property Red: Color read get_Red;
    class property RosyBrown: Color read get_RosyBrown;
    class property RoyalBlue: Color read get_RoyalBlue;
    class property SaddleBrown: Color read get_SaddleBrown;
    class property Salmon: Color read get_Salmon;
    class property SandyBrown: Color read get_SandyBrown;
    class property SeaGreen: Color read get_SeaGreen;
    class property SeaShell: Color read get_SeaShell;
    class property Sienna: Color read get_Sienna;
    class property Silver: Color read get_Silver;
    class property SkyBlue: Color read get_SkyBlue;
    class property SlateBlue: Color read get_SlateBlue;
    class property SlateGray: Color read get_SlateGray;
    class property Snow: Color read get_Snow;
    class property SpringGreen: Color read get_SpringGreen;
    class property SteelBlue: Color read get_SteelBlue;
    class property Tan: Color read get_Tan;
    class property Teal: Color read get_Teal;
    class property Thistle: Color read get_Thistle;
    class property Tomato: Color read get_Tomato;
    class property Transparent: Color read get_Transparent;
    class property Turquoise: Color read get_Turquoise;
    class property Violet: Color read get_Violet;
    class property Wheat: Color read get_Wheat;
    class property White: Color read get_White;
    class property WhiteSmoke: Color read get_WhiteSmoke;
    class property Yellow: Color read get_Yellow;
    class property YellowGreen: Color read get_YellowGreen;
  end;

implementation

{ TApplicationSettings_AccountsSettingsPane }

class function TApplicationSettings_AccountsSettingsPane.GetForCurrentView: ApplicationSettings_IAccountsSettingsPane;
begin
  Result := Statics.GetForCurrentView;
end;

class procedure TApplicationSettings_AccountsSettingsPane.Show;
begin
  Statics.Show;
end;


class function TApplicationSettings_AccountsSettingsPane.ShowManageAccountsAsync: IAsyncAction;
begin
  Result := Statics2.ShowManageAccountsAsync;
end;

class function TApplicationSettings_AccountsSettingsPane.ShowAddAccountAsync: IAsyncAction;
begin
  Result := Statics2.ShowAddAccountAsync;
end;


class function TApplicationSettings_AccountsSettingsPane.ShowManageAccountsForUserAsync(user: IUser): IAsyncAction;
begin
  Result := Statics3.ShowManageAccountsForUserAsync(user);
end;

class function TApplicationSettings_AccountsSettingsPane.ShowAddAccountForUserAsync(user: IUser): IAsyncAction;
begin
  Result := Statics3.ShowAddAccountForUserAsync(user);
end;


{ TApplicationSettings_SettingsCommand }

class function TApplicationSettings_SettingsCommand.get_AccountsCommand: Popups_IUICommand;
begin
  Result := Statics.get_AccountsCommand;
end;

// Factories for : "ApplicationSettings_SettingsCommand"
// Factory: "Windows.UI.ApplicationSettings.ISettingsCommandFactory"
// -> ApplicationSettings_ISettingsCommandFactory

class function TApplicationSettings_SettingsCommand.CreateSettingsCommand(settingsCommandId: IInspectable; &label: HSTRING; handler: Popups_UICommandInvokedHandler): Popups_IUICommand;
begin
  Result := Factory.CreateSettingsCommand(settingsCommandId, &label, handler);
end;


{ TApplicationSettings_WebAccountCommand }
// Factories for : "ApplicationSettings_WebAccountCommand"
// Factory: "Windows.UI.ApplicationSettings.IWebAccountCommandFactory"
// -> ApplicationSettings_IWebAccountCommandFactory

class function TApplicationSettings_WebAccountCommand.CreateWebAccountCommand(webAccount: IWebAccount; invoked: ApplicationSettings_WebAccountCommandInvokedHandler; actions: ApplicationSettings_SupportedWebAccountActions): ApplicationSettings_IWebAccountCommand;
begin
  Result := Factory.CreateWebAccountCommand(webAccount, invoked, actions);
end;


{ TApplicationSettings_WebAccountProviderCommand }
// Factories for : "ApplicationSettings_WebAccountProviderCommand"
// Factory: "Windows.UI.ApplicationSettings.IWebAccountProviderCommandFactory"
// -> ApplicationSettings_IWebAccountProviderCommandFactory

class function TApplicationSettings_WebAccountProviderCommand.CreateWebAccountProviderCommand(webAccountProvider: IWebAccountProvider; invoked: ApplicationSettings_WebAccountProviderCommandInvokedHandler): ApplicationSettings_IWebAccountProviderCommand;
begin
  Result := Factory.CreateWebAccountProviderCommand(webAccountProvider, invoked);
end;


{ TColorHelper }

class function TColorHelper.FromArgb(a: Byte; r: Byte; g: Byte; b: Byte): Color;
begin
  Result := Statics.FromArgb(a, r, g, b);
end;


class function TColorHelper.ToDisplayName(color: Color): HSTRING;
begin
  Result := Statics2.ToDisplayName(color);
end;


{ TColors }

class function TColors.get_AliceBlue: Color;
begin
  Result := Statics.get_AliceBlue;
end;

class function TColors.get_AntiqueWhite: Color;
begin
  Result := Statics.get_AntiqueWhite;
end;

class function TColors.get_Aqua: Color;
begin
  Result := Statics.get_Aqua;
end;

class function TColors.get_Aquamarine: Color;
begin
  Result := Statics.get_Aquamarine;
end;

class function TColors.get_Azure: Color;
begin
  Result := Statics.get_Azure;
end;

class function TColors.get_Beige: Color;
begin
  Result := Statics.get_Beige;
end;

class function TColors.get_Bisque: Color;
begin
  Result := Statics.get_Bisque;
end;

class function TColors.get_Black: Color;
begin
  Result := Statics.get_Black;
end;

class function TColors.get_BlanchedAlmond: Color;
begin
  Result := Statics.get_BlanchedAlmond;
end;

class function TColors.get_Blue: Color;
begin
  Result := Statics.get_Blue;
end;

class function TColors.get_BlueViolet: Color;
begin
  Result := Statics.get_BlueViolet;
end;

class function TColors.get_Brown: Color;
begin
  Result := Statics.get_Brown;
end;

class function TColors.get_BurlyWood: Color;
begin
  Result := Statics.get_BurlyWood;
end;

class function TColors.get_CadetBlue: Color;
begin
  Result := Statics.get_CadetBlue;
end;

class function TColors.get_Chartreuse: Color;
begin
  Result := Statics.get_Chartreuse;
end;

class function TColors.get_Chocolate: Color;
begin
  Result := Statics.get_Chocolate;
end;

class function TColors.get_Coral: Color;
begin
  Result := Statics.get_Coral;
end;

class function TColors.get_CornflowerBlue: Color;
begin
  Result := Statics.get_CornflowerBlue;
end;

class function TColors.get_Cornsilk: Color;
begin
  Result := Statics.get_Cornsilk;
end;

class function TColors.get_Crimson: Color;
begin
  Result := Statics.get_Crimson;
end;

class function TColors.get_Cyan: Color;
begin
  Result := Statics.get_Cyan;
end;

class function TColors.get_DarkBlue: Color;
begin
  Result := Statics.get_DarkBlue;
end;

class function TColors.get_DarkCyan: Color;
begin
  Result := Statics.get_DarkCyan;
end;

class function TColors.get_DarkGoldenrod: Color;
begin
  Result := Statics.get_DarkGoldenrod;
end;

class function TColors.get_DarkGray: Color;
begin
  Result := Statics.get_DarkGray;
end;

class function TColors.get_DarkGreen: Color;
begin
  Result := Statics.get_DarkGreen;
end;

class function TColors.get_DarkKhaki: Color;
begin
  Result := Statics.get_DarkKhaki;
end;

class function TColors.get_DarkMagenta: Color;
begin
  Result := Statics.get_DarkMagenta;
end;

class function TColors.get_DarkOliveGreen: Color;
begin
  Result := Statics.get_DarkOliveGreen;
end;

class function TColors.get_DarkOrange: Color;
begin
  Result := Statics.get_DarkOrange;
end;

class function TColors.get_DarkOrchid: Color;
begin
  Result := Statics.get_DarkOrchid;
end;

class function TColors.get_DarkRed: Color;
begin
  Result := Statics.get_DarkRed;
end;

class function TColors.get_DarkSalmon: Color;
begin
  Result := Statics.get_DarkSalmon;
end;

class function TColors.get_DarkSeaGreen: Color;
begin
  Result := Statics.get_DarkSeaGreen;
end;

class function TColors.get_DarkSlateBlue: Color;
begin
  Result := Statics.get_DarkSlateBlue;
end;

class function TColors.get_DarkSlateGray: Color;
begin
  Result := Statics.get_DarkSlateGray;
end;

class function TColors.get_DarkTurquoise: Color;
begin
  Result := Statics.get_DarkTurquoise;
end;

class function TColors.get_DarkViolet: Color;
begin
  Result := Statics.get_DarkViolet;
end;

class function TColors.get_DeepPink: Color;
begin
  Result := Statics.get_DeepPink;
end;

class function TColors.get_DeepSkyBlue: Color;
begin
  Result := Statics.get_DeepSkyBlue;
end;

class function TColors.get_DimGray: Color;
begin
  Result := Statics.get_DimGray;
end;

class function TColors.get_DodgerBlue: Color;
begin
  Result := Statics.get_DodgerBlue;
end;

class function TColors.get_Firebrick: Color;
begin
  Result := Statics.get_Firebrick;
end;

class function TColors.get_FloralWhite: Color;
begin
  Result := Statics.get_FloralWhite;
end;

class function TColors.get_ForestGreen: Color;
begin
  Result := Statics.get_ForestGreen;
end;

class function TColors.get_Fuchsia: Color;
begin
  Result := Statics.get_Fuchsia;
end;

class function TColors.get_Gainsboro: Color;
begin
  Result := Statics.get_Gainsboro;
end;

class function TColors.get_GhostWhite: Color;
begin
  Result := Statics.get_GhostWhite;
end;

class function TColors.get_Gold: Color;
begin
  Result := Statics.get_Gold;
end;

class function TColors.get_Goldenrod: Color;
begin
  Result := Statics.get_Goldenrod;
end;

class function TColors.get_Gray: Color;
begin
  Result := Statics.get_Gray;
end;

class function TColors.get_Green: Color;
begin
  Result := Statics.get_Green;
end;

class function TColors.get_GreenYellow: Color;
begin
  Result := Statics.get_GreenYellow;
end;

class function TColors.get_Honeydew: Color;
begin
  Result := Statics.get_Honeydew;
end;

class function TColors.get_HotPink: Color;
begin
  Result := Statics.get_HotPink;
end;

class function TColors.get_IndianRed: Color;
begin
  Result := Statics.get_IndianRed;
end;

class function TColors.get_Indigo: Color;
begin
  Result := Statics.get_Indigo;
end;

class function TColors.get_Ivory: Color;
begin
  Result := Statics.get_Ivory;
end;

class function TColors.get_Khaki: Color;
begin
  Result := Statics.get_Khaki;
end;

class function TColors.get_Lavender: Color;
begin
  Result := Statics.get_Lavender;
end;

class function TColors.get_LavenderBlush: Color;
begin
  Result := Statics.get_LavenderBlush;
end;

class function TColors.get_LawnGreen: Color;
begin
  Result := Statics.get_LawnGreen;
end;

class function TColors.get_LemonChiffon: Color;
begin
  Result := Statics.get_LemonChiffon;
end;

class function TColors.get_LightBlue: Color;
begin
  Result := Statics.get_LightBlue;
end;

class function TColors.get_LightCoral: Color;
begin
  Result := Statics.get_LightCoral;
end;

class function TColors.get_LightCyan: Color;
begin
  Result := Statics.get_LightCyan;
end;

class function TColors.get_LightGoldenrodYellow: Color;
begin
  Result := Statics.get_LightGoldenrodYellow;
end;

class function TColors.get_LightGreen: Color;
begin
  Result := Statics.get_LightGreen;
end;

class function TColors.get_LightGray: Color;
begin
  Result := Statics.get_LightGray;
end;

class function TColors.get_LightPink: Color;
begin
  Result := Statics.get_LightPink;
end;

class function TColors.get_LightSalmon: Color;
begin
  Result := Statics.get_LightSalmon;
end;

class function TColors.get_LightSeaGreen: Color;
begin
  Result := Statics.get_LightSeaGreen;
end;

class function TColors.get_LightSkyBlue: Color;
begin
  Result := Statics.get_LightSkyBlue;
end;

class function TColors.get_LightSlateGray: Color;
begin
  Result := Statics.get_LightSlateGray;
end;

class function TColors.get_LightSteelBlue: Color;
begin
  Result := Statics.get_LightSteelBlue;
end;

class function TColors.get_LightYellow: Color;
begin
  Result := Statics.get_LightYellow;
end;

class function TColors.get_Lime: Color;
begin
  Result := Statics.get_Lime;
end;

class function TColors.get_LimeGreen: Color;
begin
  Result := Statics.get_LimeGreen;
end;

class function TColors.get_Linen: Color;
begin
  Result := Statics.get_Linen;
end;

class function TColors.get_Magenta: Color;
begin
  Result := Statics.get_Magenta;
end;

class function TColors.get_Maroon: Color;
begin
  Result := Statics.get_Maroon;
end;

class function TColors.get_MediumAquamarine: Color;
begin
  Result := Statics.get_MediumAquamarine;
end;

class function TColors.get_MediumBlue: Color;
begin
  Result := Statics.get_MediumBlue;
end;

class function TColors.get_MediumOrchid: Color;
begin
  Result := Statics.get_MediumOrchid;
end;

class function TColors.get_MediumPurple: Color;
begin
  Result := Statics.get_MediumPurple;
end;

class function TColors.get_MediumSeaGreen: Color;
begin
  Result := Statics.get_MediumSeaGreen;
end;

class function TColors.get_MediumSlateBlue: Color;
begin
  Result := Statics.get_MediumSlateBlue;
end;

class function TColors.get_MediumSpringGreen: Color;
begin
  Result := Statics.get_MediumSpringGreen;
end;

class function TColors.get_MediumTurquoise: Color;
begin
  Result := Statics.get_MediumTurquoise;
end;

class function TColors.get_MediumVioletRed: Color;
begin
  Result := Statics.get_MediumVioletRed;
end;

class function TColors.get_MidnightBlue: Color;
begin
  Result := Statics.get_MidnightBlue;
end;

class function TColors.get_MintCream: Color;
begin
  Result := Statics.get_MintCream;
end;

class function TColors.get_MistyRose: Color;
begin
  Result := Statics.get_MistyRose;
end;

class function TColors.get_Moccasin: Color;
begin
  Result := Statics.get_Moccasin;
end;

class function TColors.get_NavajoWhite: Color;
begin
  Result := Statics.get_NavajoWhite;
end;

class function TColors.get_Navy: Color;
begin
  Result := Statics.get_Navy;
end;

class function TColors.get_OldLace: Color;
begin
  Result := Statics.get_OldLace;
end;

class function TColors.get_Olive: Color;
begin
  Result := Statics.get_Olive;
end;

class function TColors.get_OliveDrab: Color;
begin
  Result := Statics.get_OliveDrab;
end;

class function TColors.get_Orange: Color;
begin
  Result := Statics.get_Orange;
end;

class function TColors.get_OrangeRed: Color;
begin
  Result := Statics.get_OrangeRed;
end;

class function TColors.get_Orchid: Color;
begin
  Result := Statics.get_Orchid;
end;

class function TColors.get_PaleGoldenrod: Color;
begin
  Result := Statics.get_PaleGoldenrod;
end;

class function TColors.get_PaleGreen: Color;
begin
  Result := Statics.get_PaleGreen;
end;

class function TColors.get_PaleTurquoise: Color;
begin
  Result := Statics.get_PaleTurquoise;
end;

class function TColors.get_PaleVioletRed: Color;
begin
  Result := Statics.get_PaleVioletRed;
end;

class function TColors.get_PapayaWhip: Color;
begin
  Result := Statics.get_PapayaWhip;
end;

class function TColors.get_PeachPuff: Color;
begin
  Result := Statics.get_PeachPuff;
end;

class function TColors.get_Peru: Color;
begin
  Result := Statics.get_Peru;
end;

class function TColors.get_Pink: Color;
begin
  Result := Statics.get_Pink;
end;

class function TColors.get_Plum: Color;
begin
  Result := Statics.get_Plum;
end;

class function TColors.get_PowderBlue: Color;
begin
  Result := Statics.get_PowderBlue;
end;

class function TColors.get_Purple: Color;
begin
  Result := Statics.get_Purple;
end;

class function TColors.get_Red: Color;
begin
  Result := Statics.get_Red;
end;

class function TColors.get_RosyBrown: Color;
begin
  Result := Statics.get_RosyBrown;
end;

class function TColors.get_RoyalBlue: Color;
begin
  Result := Statics.get_RoyalBlue;
end;

class function TColors.get_SaddleBrown: Color;
begin
  Result := Statics.get_SaddleBrown;
end;

class function TColors.get_Salmon: Color;
begin
  Result := Statics.get_Salmon;
end;

class function TColors.get_SandyBrown: Color;
begin
  Result := Statics.get_SandyBrown;
end;

class function TColors.get_SeaGreen: Color;
begin
  Result := Statics.get_SeaGreen;
end;

class function TColors.get_SeaShell: Color;
begin
  Result := Statics.get_SeaShell;
end;

class function TColors.get_Sienna: Color;
begin
  Result := Statics.get_Sienna;
end;

class function TColors.get_Silver: Color;
begin
  Result := Statics.get_Silver;
end;

class function TColors.get_SkyBlue: Color;
begin
  Result := Statics.get_SkyBlue;
end;

class function TColors.get_SlateBlue: Color;
begin
  Result := Statics.get_SlateBlue;
end;

class function TColors.get_SlateGray: Color;
begin
  Result := Statics.get_SlateGray;
end;

class function TColors.get_Snow: Color;
begin
  Result := Statics.get_Snow;
end;

class function TColors.get_SpringGreen: Color;
begin
  Result := Statics.get_SpringGreen;
end;

class function TColors.get_SteelBlue: Color;
begin
  Result := Statics.get_SteelBlue;
end;

class function TColors.get_Tan: Color;
begin
  Result := Statics.get_Tan;
end;

class function TColors.get_Teal: Color;
begin
  Result := Statics.get_Teal;
end;

class function TColors.get_Thistle: Color;
begin
  Result := Statics.get_Thistle;
end;

class function TColors.get_Tomato: Color;
begin
  Result := Statics.get_Tomato;
end;

class function TColors.get_Transparent: Color;
begin
  Result := Statics.get_Transparent;
end;

class function TColors.get_Turquoise: Color;
begin
  Result := Statics.get_Turquoise;
end;

class function TColors.get_Violet: Color;
begin
  Result := Statics.get_Violet;
end;

class function TColors.get_Wheat: Color;
begin
  Result := Statics.get_Wheat;
end;

class function TColors.get_White: Color;
begin
  Result := Statics.get_White;
end;

class function TColors.get_WhiteSmoke: Color;
begin
  Result := Statics.get_WhiteSmoke;
end;

class function TColors.get_Yellow: Color;
begin
  Result := Statics.get_Yellow;
end;

class function TColors.get_YellowGreen: Color;
begin
  Result := Statics.get_YellowGreen;
end;


end.
