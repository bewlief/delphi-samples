{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.UI.Core;

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

  CoreCursorType = Winapi.CommonTypes.CoreCursorType;
  PCoreCursorType = Winapi.CommonTypes.PCoreCursorType;
  CoreDispatcherPriority = Winapi.CommonTypes.CoreDispatcherPriority;
  PCoreDispatcherPriority = Winapi.CommonTypes.PCoreDispatcherPriority;
  CoreInputDeviceTypes = Winapi.CommonTypes.CoreInputDeviceTypes;
  PCoreInputDeviceTypes = Winapi.CommonTypes.PCoreInputDeviceTypes;
  CorePhysicalKeyStatus = Winapi.CommonTypes.CorePhysicalKeyStatus;
  PCorePhysicalKeyStatus = Winapi.CommonTypes.PCorePhysicalKeyStatus;
  CoreProcessEventsOption = Winapi.CommonTypes.CoreProcessEventsOption;
  PCoreProcessEventsOption = Winapi.CommonTypes.PCoreProcessEventsOption;
  CoreProximityEvaluation = Winapi.CommonTypes.CoreProximityEvaluation;
  PCoreProximityEvaluation = Winapi.CommonTypes.PCoreProximityEvaluation;
  CoreVirtualKeyStates = Winapi.CommonTypes.CoreVirtualKeyStates;
  PCoreVirtualKeyStates = Winapi.CommonTypes.PCoreVirtualKeyStates;
  CoreWindowActivationState = Winapi.CommonTypes.CoreWindowActivationState;
  PCoreWindowActivationState = Winapi.CommonTypes.PCoreWindowActivationState;
  CoreWindowFlowDirection = Winapi.CommonTypes.CoreWindowFlowDirection;
  PCoreWindowFlowDirection = Winapi.CommonTypes.PCoreWindowFlowDirection;
  DispatchedHandler = Winapi.CommonTypes.DispatchedHandler;
  PDispatchedHandler = Winapi.CommonTypes.PDispatchedHandler;
  IAutomationProviderRequestedEventArgs = Winapi.CommonTypes.IAutomationProviderRequestedEventArgs;
  PIAutomationProviderRequestedEventArgs = Winapi.CommonTypes.PIAutomationProviderRequestedEventArgs;
  ICharacterReceivedEventArgs = Winapi.CommonTypes.ICharacterReceivedEventArgs;
  PICharacterReceivedEventArgs = Winapi.CommonTypes.PICharacterReceivedEventArgs;
  ICoreCursor = Winapi.CommonTypes.ICoreCursor;
  PICoreCursor = Winapi.CommonTypes.PICoreCursor;
  ICoreDispatcher = Winapi.CommonTypes.ICoreDispatcher;
  PICoreDispatcher = Winapi.CommonTypes.PICoreDispatcher;
  ICoreInputSourceBase = Winapi.CommonTypes.ICoreInputSourceBase;
  PICoreInputSourceBase = Winapi.CommonTypes.PICoreInputSourceBase;
  ICoreWindow = Winapi.CommonTypes.ICoreWindow;
  PICoreWindow = Winapi.CommonTypes.PICoreWindow;
  ICoreWindowEventArgs = Winapi.CommonTypes.ICoreWindowEventArgs;
  PICoreWindowEventArgs = Winapi.CommonTypes.PICoreWindowEventArgs;
  IdleDispatchedHandler = Winapi.CommonTypes.IdleDispatchedHandler;
  PIdleDispatchedHandler = Winapi.CommonTypes.PIdleDispatchedHandler;
  IIdleDispatchedHandlerArgs = Winapi.CommonTypes.IIdleDispatchedHandlerArgs;
  PIIdleDispatchedHandlerArgs = Winapi.CommonTypes.PIIdleDispatchedHandlerArgs;
  IInputEnabledEventArgs = Winapi.CommonTypes.IInputEnabledEventArgs;
  PIInputEnabledEventArgs = Winapi.CommonTypes.PIInputEnabledEventArgs;
  IKeyEventArgs = Winapi.CommonTypes.IKeyEventArgs;
  PIKeyEventArgs = Winapi.CommonTypes.PIKeyEventArgs;
  IPointerEventArgs = Winapi.CommonTypes.IPointerEventArgs;
  PIPointerEventArgs = Winapi.CommonTypes.PIPointerEventArgs;
  ITouchHitTestingEventArgs = Winapi.CommonTypes.ITouchHitTestingEventArgs;
  PITouchHitTestingEventArgs = Winapi.CommonTypes.PITouchHitTestingEventArgs;
  IVisibilityChangedEventArgs = Winapi.CommonTypes.IVisibilityChangedEventArgs;
  PIVisibilityChangedEventArgs = Winapi.CommonTypes.PIVisibilityChangedEventArgs;
  IWindowActivatedEventArgs = Winapi.CommonTypes.IWindowActivatedEventArgs;
  PIWindowActivatedEventArgs = Winapi.CommonTypes.PIWindowActivatedEventArgs;
  IWindowSizeChangedEventArgs = Winapi.CommonTypes.IWindowSizeChangedEventArgs;
  PIWindowSizeChangedEventArgs = Winapi.CommonTypes.PIWindowSizeChangedEventArgs;
  TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs_Delegate_Base;
  TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs = Winapi.CommonTypes.TypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs;
  PTypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__Core_ICoreInkIndependentInputSource__IPointerEventArgs;
  TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs;
  PTypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__IAutomationProviderRequestedEventArgs;
  TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs;
  PTypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__ICharacterReceivedEventArgs;
  TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs;
  PTypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__ICoreWindowEventArgs;
  TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs;
  PTypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__IInputEnabledEventArgs;
  TypedEventHandler_2__ICoreWindow__IKeyEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IKeyEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__IKeyEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IKeyEventArgs;
  PTypedEventHandler_2__ICoreWindow__IKeyEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__IKeyEventArgs;
  TypedEventHandler_2__ICoreWindow__IPointerEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IPointerEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__IPointerEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IPointerEventArgs;
  PTypedEventHandler_2__ICoreWindow__IPointerEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__IPointerEventArgs;
  TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs;
  PTypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__ITouchHitTestingEventArgs;
  TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs;
  PTypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__IVisibilityChangedEventArgs;
  TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs;
  PTypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__IWindowActivatedEventArgs;
  TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs_Delegate_Base;
  TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs;
  PTypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__ICoreWindow__IWindowSizeChangedEventArgs;
  TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs_Delegate_Base;
  TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IInkStrokeInput__IPointerEventArgs;
  PTypedEventHandler_2__IInkStrokeInput__IPointerEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IInkStrokeInput__IPointerEventArgs;
  TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs_Delegate_Base;
  TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs;
  PTypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IInkUnprocessedInput__IPointerEventArgs;
  TypedEventHandler_2__IInspectable__IInputEnabledEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IInspectable__IInputEnabledEventArgs_Delegate_Base;
  TypedEventHandler_2__IInspectable__IInputEnabledEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IInspectable__IInputEnabledEventArgs;
  PTypedEventHandler_2__IInspectable__IInputEnabledEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IInspectable__IInputEnabledEventArgs;

  // Forward declarations for interfaces

  // Windows.UI.Core.AnimationMetrics.IPropertyAnimation
  AnimationMetrics_IPropertyAnimation = interface;
  PAnimationMetrics_IPropertyAnimation = ^AnimationMetrics_IPropertyAnimation;

  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IIterator_1__AnimationMetrics_IPropertyAnimation = interface;
  PIIterator_1__AnimationMetrics_IPropertyAnimation = ^IIterator_1__AnimationMetrics_IPropertyAnimation;

  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IIterable_1__AnimationMetrics_IPropertyAnimation = interface;
  PIIterable_1__AnimationMetrics_IPropertyAnimation = ^IIterable_1__AnimationMetrics_IPropertyAnimation;

  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IVectorView_1__AnimationMetrics_IPropertyAnimation = interface;
  PIVectorView_1__AnimationMetrics_IPropertyAnimation = ^IVectorView_1__AnimationMetrics_IPropertyAnimation;

  // Windows.UI.Core.AnimationMetrics.IAnimationDescription
  AnimationMetrics_IAnimationDescription = interface;
  PAnimationMetrics_IAnimationDescription = ^AnimationMetrics_IAnimationDescription;

  // Windows.UI.Core.AnimationMetrics.IAnimationDescriptionFactory
  AnimationMetrics_IAnimationDescriptionFactory = interface;
  PAnimationMetrics_IAnimationDescriptionFactory = ^AnimationMetrics_IAnimationDescriptionFactory;

  // Windows.UI.Core.AnimationMetrics.IOpacityAnimation
  AnimationMetrics_IOpacityAnimation = interface;
  PAnimationMetrics_IOpacityAnimation = ^AnimationMetrics_IOpacityAnimation;

  // Windows.UI.Core.AnimationMetrics.IScaleAnimation
  AnimationMetrics_IScaleAnimation = interface;
  PAnimationMetrics_IScaleAnimation = ^AnimationMetrics_IScaleAnimation;

  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ICoreWindowEventArgs>
  TypedEventHandler_2__IInspectable__ICoreWindowEventArgs = interface;
  PTypedEventHandler_2__IInspectable__ICoreWindowEventArgs = ^TypedEventHandler_2__IInspectable__ICoreWindowEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ICharacterReceivedEventArgs>
  TypedEventHandler_2__IInspectable__ICharacterReceivedEventArgs = interface;
  PTypedEventHandler_2__IInspectable__ICharacterReceivedEventArgs = ^TypedEventHandler_2__IInspectable__ICharacterReceivedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IKeyEventArgs>
  TypedEventHandler_2__IInspectable__IKeyEventArgs = interface;
  PTypedEventHandler_2__IInspectable__IKeyEventArgs = ^TypedEventHandler_2__IInspectable__IKeyEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__IInspectable__IPointerEventArgs = interface;
  PTypedEventHandler_2__IInspectable__IPointerEventArgs = ^TypedEventHandler_2__IInspectable__IPointerEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ITouchHitTestingEventArgs>
  TypedEventHandler_2__IInspectable__ITouchHitTestingEventArgs = interface;
  PTypedEventHandler_2__IInspectable__ITouchHitTestingEventArgs = ^TypedEventHandler_2__IInspectable__ITouchHitTestingEventArgs;

  // Windows.UI.Core.IInitializeWithCoreWindow
  IInitializeWithCoreWindow = interface;
  PIInitializeWithCoreWindow = ^IInitializeWithCoreWindow;

  // Windows.UI.Core Enums

  // Windows.UI.Core.AnimationMetrics.AnimationEffect
  AnimationMetrics_AnimationEffect = (
    Expand = 0,
    Collapse = 1,
    Reposition = 2,
    FadeIn = 3,
    FadeOut = 4,
    AddToList = 5,
    DeleteFromList = 6,
    AddToGrid = 7,
    DeleteFromGrid = 8,
    AddToSearchGrid = 9,
    DeleteFromSearchGrid = 10,
    AddToSearchList = 11,
    DeleteFromSearchList = 12,
    ShowEdgeUI = 13,
    ShowPanel = 14,
    HideEdgeUI = 15,
    HidePanel = 16,
    ShowPopup = 17,
    HidePopup = 18,
    PointerDown = 19,
    PointerUp = 20,
    DragSourceStart = 21,
    DragSourceEnd = 22,
    TransitionContent = 23,
    Reveal = 24,
    Hide = 25,
    DragBetweenEnter = 26,
    DragBetweenLeave = 27,
    SwipeSelect = 28,
    SwipeDeselect = 29,
    SwipeReveal = 30,
    EnterPage = 31,
    TransitionPage = 32,
    CrossFade = 33,
    Peek = 34,
    UpdateBadge = 35
  );
  PAnimationMetrics_AnimationEffect = ^AnimationMetrics_AnimationEffect;

  // Windows.UI.Core.AnimationMetrics.AnimationEffectTarget
  AnimationMetrics_AnimationEffectTarget = (
    Primary = 0,
    Added = 1,
    Affected = 2,
    Background = 3,
    Content = 4,
    Deleted = 5,
    Deselected = 6,
    DragSource = 7,
    Hidden = 8,
    Incoming = 9,
    Outgoing = 10,
    Outline = 11,
    Remaining = 12,
    Revealed = 13,
    RowIn = 14,
    RowOut = 15,
    Selected = 16,
    Selection = 17,
    Shown = 18,
    Tapped = 19
  );
  PAnimationMetrics_AnimationEffectTarget = ^AnimationMetrics_AnimationEffectTarget;

  // Windows.UI.Core.AnimationMetrics.PropertyAnimationType
  AnimationMetrics_PropertyAnimationType = (
    Scale = 0,
    Translation = 1,
    Opacity = 2
  );
  PAnimationMetrics_PropertyAnimationType = ^AnimationMetrics_PropertyAnimationType;

  // Windows.UI.Core.AppViewBackButtonVisibility
  AppViewBackButtonVisibility = (
    Visible = 0,
    Collapsed = 1,
    Disabled = 2
  );
  PAppViewBackButtonVisibility = ^AppViewBackButtonVisibility;

  // Windows.UI.Core.CoreAcceleratorKeyEventType
  CoreAcceleratorKeyEventType = (
    Character = 2,
    DeadCharacter = 3,
    KeyDown = 0,
    KeyUp = 1,
    SystemCharacter = 6,
    SystemDeadCharacter = 7,
    SystemKeyDown = 4,
    SystemKeyUp = 5,
    UnicodeCharacter = 8
  );
  PCoreAcceleratorKeyEventType = ^CoreAcceleratorKeyEventType;

  // Windows.UI.Core.CoreProximityEvaluationScore
  CoreProximityEvaluationScore = (
    Closest = 0,
    Farthest = 2147483647
  );
  PCoreProximityEvaluationScore = ^CoreProximityEvaluationScore;

  // Windows.UI.Core.CoreWindowActivationMode
  CoreWindowActivationMode = (
    None = 0,
    Deactivated = 1,
    ActivatedNotForeground = 2,
    ActivatedInForeground = 3
  );
  PCoreWindowActivationMode = ^CoreWindowActivationMode;

  // Windows.UI.Core Records
  // Windows.UI.Core.AnimationMetrics.AnimationMetricsContract
  AnimationMetrics_AnimationMetricsContract = record
  end;
  PAnimationMetrics_AnimationMetricsContract = ^AnimationMetrics_AnimationMetricsContract;

  // Windows.UI.Core.CoreWindowDialogsContract
  CoreWindowDialogsContract = record
  end;
  PCoreWindowDialogsContract = ^CoreWindowDialogsContract;

  // Windows.UI.Core Interfaces

  // UsedAPI Interface
  // Windows.UI.Core.AnimationMetrics.IPropertyAnimation
  AnimationMetrics_IPropertyAnimation = interface(IInspectable)
  ['{3A01B4DA-4D8C-411E-B615-1ADE683A9903}']
    function get_Type: AnimationMetrics_PropertyAnimationType; safecall;
    function get_Delay: TimeSpan; safecall;
    function get_Duration: TimeSpan; safecall;
    function get_Control1: TPointF; safecall;
    function get_Control2: TPointF; safecall;
    property Control1: TPointF read get_Control1;
    property Control2: TPointF read get_Control2;
    property Delay: TimeSpan read get_Delay;
    property Duration: TimeSpan read get_Duration;
    property &Type: AnimationMetrics_PropertyAnimationType read get_Type;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IIterator_1__AnimationMetrics_IPropertyAnimation_Base = interface(IInspectable)
  ['{BB6799D3-9F1A-5A4E-A940-945F1AB8C4FE}']
    function get_Current: AnimationMetrics_IPropertyAnimation; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PAnimationMetrics_IPropertyAnimation): Cardinal; safecall;
    property Current: AnimationMetrics_IPropertyAnimation read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IIterator_1__AnimationMetrics_IPropertyAnimation = interface(IIterator_1__AnimationMetrics_IPropertyAnimation_Base)
  ['{BB6799D3-9F1A-5A4E-A940-945F1AB8C4FE}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IIterable_1__AnimationMetrics_IPropertyAnimation_Base = interface(IInspectable)
  ['{C75F1BD1-A3C1-5881-9DA0-1ECDB8E51BC3}']
    function First: IIterator_1__AnimationMetrics_IPropertyAnimation; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IIterable_1__AnimationMetrics_IPropertyAnimation = interface(IIterable_1__AnimationMetrics_IPropertyAnimation_Base)
  ['{C75F1BD1-A3C1-5881-9DA0-1ECDB8E51BC3}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.UI.Core.AnimationMetrics.IPropertyAnimation>
  IVectorView_1__AnimationMetrics_IPropertyAnimation = interface(IInspectable)
  ['{3A6ED95D-6A50-5EAD-A4C6-09F8BABC632C}']
    function GetAt(index: Cardinal): AnimationMetrics_IPropertyAnimation; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: AnimationMetrics_IPropertyAnimation; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PAnimationMetrics_IPropertyAnimation): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Core.AnimationMetrics.IAnimationDescription
  [WinRTClassNameAttribute(SWindows_UI_Core_AnimationMetrics_AnimationDescription)]
  AnimationMetrics_IAnimationDescription = interface(IInspectable)
  ['{7D11A549-BE3D-41DE-B081-05C149962F9B}']
    function get_Animations: IVectorView_1__AnimationMetrics_IPropertyAnimation; safecall;
    function get_StaggerDelay: TimeSpan; safecall;
    function get_StaggerDelayFactor: Single; safecall;
    function get_DelayLimit: TimeSpan; safecall;
    function get_ZOrder: Integer; safecall;
    property Animations: IVectorView_1__AnimationMetrics_IPropertyAnimation read get_Animations;
    property DelayLimit: TimeSpan read get_DelayLimit;
    property StaggerDelay: TimeSpan read get_StaggerDelay;
    property StaggerDelayFactor: Single read get_StaggerDelayFactor;
    property ZOrder: Integer read get_ZOrder;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.UI.Core.AnimationMetrics.IAnimationDescriptionFactory
  [WinRTClassNameAttribute(SWindows_UI_Core_AnimationMetrics_AnimationDescription)]
  AnimationMetrics_IAnimationDescriptionFactory = interface(IInspectable)
  ['{C6E27ABE-C1FB-48B5-9271-ECC70AC86EF0}']
    function CreateInstance(effect: AnimationMetrics_AnimationEffect; target: AnimationMetrics_AnimationEffectTarget): AnimationMetrics_IAnimationDescription; safecall;
  end;

  // Windows.UI.Core.AnimationMetrics.IOpacityAnimation
  AnimationMetrics_IOpacityAnimation = interface(IInspectable)
  ['{803AABE5-EE7E-455F-84E9-2506AFB8D2B4}']
    function get_InitialOpacity: IReference_1__Single; safecall;
    function get_FinalOpacity: Single; safecall;
    property FinalOpacity: Single read get_FinalOpacity;
    property InitialOpacity: IReference_1__Single read get_InitialOpacity;
  end;

  // Windows.UI.Core.AnimationMetrics.IScaleAnimation
  AnimationMetrics_IScaleAnimation = interface(IInspectable)
  ['{023552C7-71AB-428C-9C9F-D31780964995}']
    function get_InitialScaleX: IReference_1__Single; safecall;
    function get_InitialScaleY: IReference_1__Single; safecall;
    function get_FinalScaleX: Single; safecall;
    function get_FinalScaleY: Single; safecall;
    function get_NormalizedOrigin: TPointF; safecall;
    property FinalScaleX: Single read get_FinalScaleX;
    property FinalScaleY: Single read get_FinalScaleY;
    property InitialScaleX: IReference_1__Single read get_InitialScaleX;
    property InitialScaleY: IReference_1__Single read get_InitialScaleY;
    property NormalizedOrigin: TPointF read get_NormalizedOrigin;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ICoreWindowEventArgs>
  TypedEventHandler_2__IInspectable__ICoreWindowEventArgs_Delegate_Base = interface(IUnknown)
  ['{1A8AC270-A777-50F7-88A1-E34E56C09449}']
    procedure Invoke(sender: IInspectable; args: ICoreWindowEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ICoreWindowEventArgs>
  TypedEventHandler_2__IInspectable__ICoreWindowEventArgs = interface(TypedEventHandler_2__IInspectable__ICoreWindowEventArgs_Delegate_Base)
  ['{F520E86D-9D9B-5024-8BAE-1DCBDDEFC67E}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ICharacterReceivedEventArgs>
  TypedEventHandler_2__IInspectable__ICharacterReceivedEventArgs_Delegate_Base = interface(IUnknown)
  ['{5AA4A848-86B2-506B-89AB-5EB5786420C6}']
    procedure Invoke(sender: IInspectable; args: ICharacterReceivedEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ICharacterReceivedEventArgs>
  TypedEventHandler_2__IInspectable__ICharacterReceivedEventArgs = interface(TypedEventHandler_2__IInspectable__ICharacterReceivedEventArgs_Delegate_Base)
  ['{B1626D26-8C50-5526-947F-76E5652A527F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IKeyEventArgs>
  TypedEventHandler_2__IInspectable__IKeyEventArgs_Delegate_Base = interface(IUnknown)
  ['{EADFFDF7-D70E-5688-906C-C2B1229EA16D}']
    procedure Invoke(sender: IInspectable; args: IKeyEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IKeyEventArgs>
  TypedEventHandler_2__IInspectable__IKeyEventArgs = interface(TypedEventHandler_2__IInspectable__IKeyEventArgs_Delegate_Base)
  ['{DEC68719-2C61-5E75-8DCB-AEA742273701}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__IInspectable__IPointerEventArgs_Delegate_Base = interface(IUnknown)
  ['{26AABF41-A0FD-5E66-B188-6C74182D00CD}']
    procedure Invoke(sender: IInspectable; args: IPointerEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.IPointerEventArgs>
  TypedEventHandler_2__IInspectable__IPointerEventArgs = interface(TypedEventHandler_2__IInspectable__IPointerEventArgs_Delegate_Base)
  ['{698383FB-4816-51E2-B784-F1EFD30A0A7F}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ITouchHitTestingEventArgs>
  TypedEventHandler_2__IInspectable__ITouchHitTestingEventArgs_Delegate_Base = interface(IUnknown)
  ['{C76E9D25-6A96-58FD-874F-AE52BD603AF8}']
    procedure Invoke(sender: IInspectable; args: ITouchHitTestingEventArgs); safecall;
  end;
  // Windows.Foundation.TypedEventHandler`2<Object,Windows.UI.Core.ITouchHitTestingEventArgs>
  TypedEventHandler_2__IInspectable__ITouchHitTestingEventArgs = interface(TypedEventHandler_2__IInspectable__ITouchHitTestingEventArgs_Delegate_Base)
  ['{202CDEA7-C8ED-5F65-9E1A-9FD2606E6851}']
  end;

  // Windows.UI.Core.IInitializeWithCoreWindow
  IInitializeWithCoreWindow = interface(IInspectable)
  ['{188F20D6-9873-464A-ACE5-57E010F465E6}']
    procedure Initialize(window: ICoreWindow); safecall;
  end;

  // Windows.UI.Core.AnimationMetrics.AnimationDescription
  // DualAPI
  // Implements: Windows.UI.Core.AnimationMetrics.IAnimationDescription
  // Factory: "Windows.UI.Core.AnimationMetrics.IAnimationDescriptionFactory"
  TAnimationMetrics_AnimationDescription = class(TWinRTGenericImportF<AnimationMetrics_IAnimationDescriptionFactory>)
  public
    // -> AnimationMetrics_IAnimationDescriptionFactory
    class function CreateInstance(effect: AnimationMetrics_AnimationEffect; target: AnimationMetrics_AnimationEffectTarget): AnimationMetrics_IAnimationDescription; static; inline;
  end;

implementation

{ TAnimationMetrics_AnimationDescription }
// Factories for : "AnimationMetrics_AnimationDescription"
// Factory: "Windows.UI.Core.AnimationMetrics.IAnimationDescriptionFactory"
// -> AnimationMetrics_IAnimationDescriptionFactory

class function TAnimationMetrics_AnimationDescription.CreateInstance(effect: AnimationMetrics_AnimationEffect; target: AnimationMetrics_AnimationEffectTarget): AnimationMetrics_IAnimationDescription;
begin
  Result := Factory.CreateInstance(effect, target);
end;


end.
