{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Foundation;

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

  AsyncActionCompletedHandler = Winapi.CommonTypes.AsyncActionCompletedHandler;
  PAsyncActionCompletedHandler = Winapi.CommonTypes.PAsyncActionCompletedHandler;
  AsyncOperationCompletedHandler_1__Boolean_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__Boolean_Delegate_Base;
  AsyncOperationCompletedHandler_1__Boolean = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__Boolean;
  PAsyncOperationCompletedHandler_1__Boolean = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__Boolean;
  AsyncOperationCompletedHandler_1__IUriRuntimeClass_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IUriRuntimeClass_Delegate_Base;
  AsyncOperationCompletedHandler_1__IUriRuntimeClass = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IUriRuntimeClass;
  PAsyncOperationCompletedHandler_1__IUriRuntimeClass = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IUriRuntimeClass;
  AsyncOperationProgressHandler_2__Cardinal__Cardinal_Delegate_Base = Winapi.CommonTypes.AsyncOperationProgressHandler_2__Cardinal__Cardinal_Delegate_Base;
  AsyncOperationProgressHandler_2__Cardinal__Cardinal = Winapi.CommonTypes.AsyncOperationProgressHandler_2__Cardinal__Cardinal;
  PAsyncOperationProgressHandler_2__Cardinal__Cardinal = Winapi.CommonTypes.PAsyncOperationProgressHandler_2__Cardinal__Cardinal;
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal_Delegate_Base = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal_Delegate_Base;
  AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = Winapi.CommonTypes.AsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal;
  PAsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal = Winapi.CommonTypes.PAsyncOperationWithProgressCompletedHandler_2__Cardinal__Cardinal;
  AsyncStatus = Winapi.CommonTypes.AsyncStatus;
  PAsyncStatus = Winapi.CommonTypes.PAsyncStatus;
  DateTime = Winapi.CommonTypes.DateTime;
  PDateTime = Winapi.CommonTypes.PDateTime;
  EventRegistrationToken = Winapi.CommonTypes.EventRegistrationToken;
  PEventRegistrationToken = Winapi.CommonTypes.PEventRegistrationToken;
  IAsyncAction = Winapi.CommonTypes.IAsyncAction;
  PIAsyncAction = Winapi.CommonTypes.PIAsyncAction;
  IAsyncOperation_1__Boolean_Base = Winapi.CommonTypes.IAsyncOperation_1__Boolean_Base;
  IAsyncOperation_1__Boolean = Winapi.CommonTypes.IAsyncOperation_1__Boolean;
  PIAsyncOperation_1__Boolean = Winapi.CommonTypes.PIAsyncOperation_1__Boolean;
  IAsyncOperation_1__IUriRuntimeClass_Base = Winapi.CommonTypes.IAsyncOperation_1__IUriRuntimeClass_Base;
  IAsyncOperation_1__IUriRuntimeClass = Winapi.CommonTypes.IAsyncOperation_1__IUriRuntimeClass;
  PIAsyncOperation_1__IUriRuntimeClass = Winapi.CommonTypes.PIAsyncOperation_1__IUriRuntimeClass;
  IAsyncOperationWithProgress_2__Cardinal__Cardinal_Base = Winapi.CommonTypes.IAsyncOperationWithProgress_2__Cardinal__Cardinal_Base;
  IAsyncOperationWithProgress_2__Cardinal__Cardinal = Winapi.CommonTypes.IAsyncOperationWithProgress_2__Cardinal__Cardinal;
  PIAsyncOperationWithProgress_2__Cardinal__Cardinal = Winapi.CommonTypes.PIAsyncOperationWithProgress_2__Cardinal__Cardinal;
  IDeferral = Winapi.CommonTypes.IDeferral;
  PIDeferral = Winapi.CommonTypes.PIDeferral;
  IIterable_1__Point_Base = Winapi.CommonTypes.IIterable_1__Point_Base;
  IIterable_1__Point = Winapi.CommonTypes.IIterable_1__Point;
  PIIterable_1__Point = Winapi.CommonTypes.PIIterable_1__Point;
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable_Base = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__HSTRING__IInspectable_Base;
  IIterator_1__IKeyValuePair_2__HSTRING__IInspectable = Winapi.CommonTypes.IIterator_1__IKeyValuePair_2__HSTRING__IInspectable;
  PIIterator_1__IKeyValuePair_2__HSTRING__IInspectable = Winapi.CommonTypes.PIIterator_1__IKeyValuePair_2__HSTRING__IInspectable;
  IIterator_1__Point_Base = Winapi.CommonTypes.IIterator_1__Point_Base;
  IIterator_1__Point = Winapi.CommonTypes.IIterator_1__Point;
  PIIterator_1__Point = Winapi.CommonTypes.PIIterator_1__Point;
  IKeyValuePair_2__HSTRING__IInspectable = Winapi.CommonTypes.IKeyValuePair_2__HSTRING__IInspectable;
  PIKeyValuePair_2__HSTRING__IInspectable = Winapi.CommonTypes.PIKeyValuePair_2__HSTRING__IInspectable;
  IMapChangedEventArgs_1__HSTRING = Winapi.CommonTypes.IMapChangedEventArgs_1__HSTRING;
  PIMapChangedEventArgs_1__HSTRING = Winapi.CommonTypes.PIMapChangedEventArgs_1__HSTRING;
  IMapView_2__HSTRING__IInspectable_Base = Winapi.CommonTypes.IMapView_2__HSTRING__IInspectable_Base;
  IMapView_2__HSTRING__IInspectable = Winapi.CommonTypes.IMapView_2__HSTRING__IInspectable;
  PIMapView_2__HSTRING__IInspectable = Winapi.CommonTypes.PIMapView_2__HSTRING__IInspectable;
  IObservableMap_2__HSTRING__IInspectable = Winapi.CommonTypes.IObservableMap_2__HSTRING__IInspectable;
  PIObservableMap_2__HSTRING__IInspectable = Winapi.CommonTypes.PIObservableMap_2__HSTRING__IInspectable;
  IReference_1__DateTime = Winapi.CommonTypes.IReference_1__DateTime;
  PIReference_1__DateTime = Winapi.CommonTypes.PIReference_1__DateTime;
  IReference_1__Numerics_Vector2 = Winapi.CommonTypes.IReference_1__Numerics_Vector2;
  PIReference_1__Numerics_Vector2 = Winapi.CommonTypes.PIReference_1__Numerics_Vector2;
  IReference_1__Point = Winapi.CommonTypes.IReference_1__Point;
  PIReference_1__Point = Winapi.CommonTypes.PIReference_1__Point;
  IReference_1__Rect = Winapi.CommonTypes.IReference_1__Rect;
  PIReference_1__Rect = Winapi.CommonTypes.PIReference_1__Rect;
  IReference_1__TimeSpan = Winapi.CommonTypes.IReference_1__TimeSpan;
  PIReference_1__TimeSpan = Winapi.CommonTypes.PIReference_1__TimeSpan;
  IUriRuntimeClass = Winapi.CommonTypes.IUriRuntimeClass;
  PIUriRuntimeClass = Winapi.CommonTypes.PIUriRuntimeClass;
  IVector_1__IUriRuntimeClass_Base = Winapi.CommonTypes.IVector_1__IUriRuntimeClass_Base;
  IVector_1__IUriRuntimeClass = Winapi.CommonTypes.IVector_1__IUriRuntimeClass;
  PIVector_1__IUriRuntimeClass = Winapi.CommonTypes.PIVector_1__IUriRuntimeClass;
  IVectorView_1__IUriRuntimeClass = Winapi.CommonTypes.IVectorView_1__IUriRuntimeClass;
  PIVectorView_1__IUriRuntimeClass = Winapi.CommonTypes.PIVectorView_1__IUriRuntimeClass;
  IWwwFormUrlDecoderRuntimeClass = Winapi.CommonTypes.IWwwFormUrlDecoderRuntimeClass;
  PIWwwFormUrlDecoderRuntimeClass = Winapi.CommonTypes.PIWwwFormUrlDecoderRuntimeClass;
  MapChangedEventHandler_2__HSTRING__IInspectable_Delegate_Base = Winapi.CommonTypes.MapChangedEventHandler_2__HSTRING__IInspectable_Delegate_Base;
  MapChangedEventHandler_2__HSTRING__IInspectable = Winapi.CommonTypes.MapChangedEventHandler_2__HSTRING__IInspectable;
  PMapChangedEventHandler_2__HSTRING__IInspectable = Winapi.CommonTypes.PMapChangedEventHandler_2__HSTRING__IInspectable;
  Numerics_Matrix3x2 = Winapi.CommonTypes.Numerics_Matrix3x2;
  PNumerics_Matrix3x2 = Winapi.CommonTypes.PNumerics_Matrix3x2;
  Numerics_Matrix4x4 = Winapi.CommonTypes.Numerics_Matrix4x4;
  PNumerics_Matrix4x4 = Winapi.CommonTypes.PNumerics_Matrix4x4;
  Numerics_Quaternion = Winapi.CommonTypes.Numerics_Quaternion;
  PNumerics_Quaternion = Winapi.CommonTypes.PNumerics_Quaternion;
  Numerics_Vector2 = Winapi.CommonTypes.Numerics_Vector2;
  PNumerics_Vector2 = Winapi.CommonTypes.PNumerics_Vector2;
  Numerics_Vector3 = Winapi.CommonTypes.Numerics_Vector3;
  PNumerics_Vector3 = Winapi.CommonTypes.PNumerics_Vector3;
  Numerics_Vector4 = Winapi.CommonTypes.Numerics_Vector4;
  PNumerics_Vector4 = Winapi.CommonTypes.PNumerics_Vector4;
  TimeSpan = Winapi.CommonTypes.TimeSpan;
  PTimeSpan = Winapi.CommonTypes.PTimeSpan;

  // Forward declarations for interfaces

  // Windows.Foundation.IClosable
  IClosable = interface;
  PIClosable = ^IClosable;

  // Windows.Foundation.IAsyncInfo
  IAsyncInfo = interface;
  PIAsyncInfo = ^IAsyncInfo;

  // Windows.Foundation.IPropertyValue
  IPropertyValue = interface;
  PIPropertyValue = ^IPropertyValue;

  // Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Vector3>
  IReference_1__Numerics_Vector3 = interface;
  PIReference_1__Numerics_Vector3 = ^IReference_1__Numerics_Vector3;

  // Windows.Foundation.IMemoryBufferReference
  IMemoryBufferReference = interface;
  PIMemoryBufferReference = ^IMemoryBufferReference;

  // Windows.Foundation.IMemoryBuffer
  IMemoryBuffer = interface;
  PIMemoryBuffer = ^IMemoryBuffer;

  // Windows.Foundation.IWwwFormUrlDecoderEntry
  IWwwFormUrlDecoderEntry = interface;
  PIWwwFormUrlDecoderEntry = ^IWwwFormUrlDecoderEntry;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  IIterator_1__IWwwFormUrlDecoderEntry = interface;
  PIIterator_1__IWwwFormUrlDecoderEntry = ^IIterator_1__IWwwFormUrlDecoderEntry;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  IIterable_1__IWwwFormUrlDecoderEntry = interface;
  PIIterable_1__IWwwFormUrlDecoderEntry = ^IIterable_1__IWwwFormUrlDecoderEntry;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.DateTime>
  IIterator_1__DateTime = interface;
  PIIterator_1__DateTime = ^IIterator_1__DateTime;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.DateTime>
  IIterable_1__DateTime = interface;
  PIIterable_1__DateTime = ^IIterable_1__DateTime;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.DateTime>
  IVectorView_1__DateTime = interface;
  PIVectorView_1__DateTime = ^IVectorView_1__DateTime;

  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.DateTime>
  IVector_1__DateTime = interface;
  PIVector_1__DateTime = ^IVector_1__DateTime;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Rect>
  IIterator_1__Rect = interface;
  PIIterator_1__Rect = ^IIterator_1__Rect;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Rect>
  IIterable_1__Rect = interface;
  PIIterable_1__Rect = ^IIterable_1__Rect;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Rect>
  IVectorView_1__Rect = interface;
  PIVectorView_1__Rect = ^IVectorView_1__Rect;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IReference`1<Windows.Foundation.DateTime>>
  AsyncOperationCompletedHandler_1__IReference_1__DateTime = interface;
  PAsyncOperationCompletedHandler_1__IReference_1__DateTime = ^AsyncOperationCompletedHandler_1__IReference_1__DateTime;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IReference`1<Windows.Foundation.DateTime>>
  IAsyncOperation_1__IReference_1__DateTime = interface;
  PIAsyncOperation_1__IReference_1__DateTime = ^IAsyncOperation_1__IReference_1__DateTime;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>>
  AsyncOperationCompletedHandler_1__IReference_1__TimeSpan = interface;
  PAsyncOperationCompletedHandler_1__IReference_1__TimeSpan = ^AsyncOperationCompletedHandler_1__IReference_1__TimeSpan;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>>
  IAsyncOperation_1__IReference_1__TimeSpan = interface;
  PIAsyncOperation_1__IReference_1__TimeSpan = ^IAsyncOperation_1__IReference_1__TimeSpan;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>>
  IIterator_1__IIterable_1__Point = interface;
  PIIterator_1__IIterable_1__Point = ^IIterator_1__IIterable_1__Point;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>>
  IIterable_1__IIterable_1__Point = interface;
  PIIterable_1__IIterable_1__Point = ^IIterable_1__IIterable_1__Point;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Point>
  IVectorView_1__Point = interface;
  PIVectorView_1__Point = ^IVectorView_1__Point;

  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.Point>
  IVector_1__Point = interface;
  PIVector_1__Point = ^IVector_1__Point;

  // Windows.Foundation.IGetActivationFactory
  IGetActivationFactory = interface;
  PIGetActivationFactory = ^IGetActivationFactory;

  // Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Matrix4x4>
  IReference_1__Numerics_Matrix4x4 = interface;
  PIReference_1__Numerics_Matrix4x4 = ^IReference_1__Numerics_Matrix4x4;

  // Windows.Foundation.IReference`1<Windows.Foundation.Size>
  IReference_1__Size = interface;
  PIReference_1__Size = ^IReference_1__Size;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Size>
  IIterator_1__Size = interface;
  PIIterator_1__Size = ^IIterator_1__Size;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Size>
  IIterable_1__Size = interface;
  PIIterable_1__Size = ^IIterable_1__Size;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Size>
  IVectorView_1__Size = interface;
  PIVectorView_1__Size = ^IVectorView_1__Size;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IInspectable = ^IIterable_1__IKeyValuePair_2__HSTRING__IInspectable;

  // Windows.Foundation.Collections.IMap`2<String,Object>
  IMap_2__HSTRING__IInspectable = interface;
  PIMap_2__HSTRING__IInspectable = ^IMap_2__HSTRING__IInspectable;

  // Windows.Foundation.DeferralCompletedHandler
  DeferralCompletedHandler = interface;
  PDeferralCompletedHandler = ^DeferralCompletedHandler;

  // Windows.Foundation.Diagnostics.ITracingStatusChangedEventArgs
  Diagnostics_ITracingStatusChangedEventArgs = interface;
  PDiagnostics_ITracingStatusChangedEventArgs = ^Diagnostics_ITracingStatusChangedEventArgs;

  // Windows.Foundation.EventHandler`1<Windows.Foundation.Diagnostics.ITracingStatusChangedEventArgs>
  EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs = interface;
  PEventHandler_1__Diagnostics_ITracingStatusChangedEventArgs = ^EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs;

  // Windows.Foundation.Diagnostics.IAsyncCausalityTracerStatics
  Diagnostics_IAsyncCausalityTracerStatics = interface;
  PDiagnostics_IAsyncCausalityTracerStatics = ^Diagnostics_IAsyncCausalityTracerStatics;

  // Windows.Foundation.Diagnostics.IErrorReportingSettings
  Diagnostics_IErrorReportingSettings = interface;
  PDiagnostics_IErrorReportingSettings = ^Diagnostics_IErrorReportingSettings;

  // Windows.Foundation.IDeferralFactory
  IDeferralFactory = interface;
  PIDeferralFactory = ^IDeferralFactory;

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface;
  PTypedEventHandler_2__IMemoryBufferReference__IInspectable = ^TypedEventHandler_2__IMemoryBufferReference__IInspectable;

  // Windows.Foundation.IMemoryBufferFactory
  IMemoryBufferFactory = interface;
  PIMemoryBufferFactory = ^IMemoryBufferFactory;

  // Windows.Foundation.IPropertyValueStatics
  IPropertyValueStatics = interface;
  PIPropertyValueStatics = ^IPropertyValueStatics;

  // Windows.Foundation.IStringable
  IStringable = interface;
  PIStringable = ^IStringable;

  // Windows.Foundation.IUriEscapeStatics
  IUriEscapeStatics = interface;
  PIUriEscapeStatics = ^IUriEscapeStatics;

  // Windows.Foundation.IUriRuntimeClassFactory
  IUriRuntimeClassFactory = interface;
  PIUriRuntimeClassFactory = ^IUriRuntimeClassFactory;

  // Windows.Foundation.IUriRuntimeClassWithAbsoluteCanonicalUri
  IUriRuntimeClassWithAbsoluteCanonicalUri = interface;
  PIUriRuntimeClassWithAbsoluteCanonicalUri = ^IUriRuntimeClassWithAbsoluteCanonicalUri;

  // Windows.Foundation.IWwwFormUrlDecoderRuntimeClassFactory
  IWwwFormUrlDecoderRuntimeClassFactory = interface;
  PIWwwFormUrlDecoderRuntimeClassFactory = ^IWwwFormUrlDecoderRuntimeClassFactory;

  // Windows.Foundation.Metadata.IApiInformationStatics
  Metadata_IApiInformationStatics = interface;
  PMetadata_IApiInformationStatics = ^Metadata_IApiInformationStatics;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IClosable>
  IIterator_1__IClosable = interface;
  PIIterator_1__IClosable = ^IIterator_1__IClosable;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IClosable>
  IIterable_1__IClosable = interface;
  PIIterable_1__IClosable = ^IIterable_1__IClosable;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.IClosable>
  IVectorView_1__IClosable = interface;
  PIVectorView_1__IClosable = ^IVectorView_1__IClosable;

  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.IClosable>
  IVector_1__IClosable = interface;
  PIVector_1__IClosable = ^IVector_1__IClosable;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IUriRuntimeClass>
  IIterator_1__IUriRuntimeClass = interface;
  PIIterator_1__IUriRuntimeClass = ^IIterator_1__IUriRuntimeClass;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IUriRuntimeClass>
  IIterable_1__IUriRuntimeClass = interface;
  PIIterable_1__IUriRuntimeClass = ^IIterable_1__IUriRuntimeClass;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.TimeSpan>
  IIterator_1__TimeSpan = interface;
  PIIterator_1__TimeSpan = ^IIterator_1__TimeSpan;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.TimeSpan>
  IIterable_1__TimeSpan = interface;
  PIIterable_1__TimeSpan = ^IIterable_1__TimeSpan;

  // Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Quaternion>
  IReference_1__Numerics_Quaternion = interface;
  PIReference_1__Numerics_Quaternion = ^IReference_1__Numerics_Quaternion;

  // Windows.Foundation Enums

  // Windows.Foundation.Diagnostics.CausalityRelation
  Diagnostics_CausalityRelation = (
    AssignDelegate = 0,
    Join = 1,
    Choice = 2,
    Cancel = 3,
    Error = 4
  );
  PDiagnostics_CausalityRelation = ^Diagnostics_CausalityRelation;

  // Windows.Foundation.Diagnostics.CausalitySource
  Diagnostics_CausalitySource = (
    Application = 0,
    &Library = 1,
    System = 2
  );
  PDiagnostics_CausalitySource = ^Diagnostics_CausalitySource;

  // Windows.Foundation.Diagnostics.CausalitySynchronousWork
  Diagnostics_CausalitySynchronousWork = (
    CompletionNotification = 0,
    ProgressNotification = 1,
    Execution = 2
  );
  PDiagnostics_CausalitySynchronousWork = ^Diagnostics_CausalitySynchronousWork;

  // Windows.Foundation.Diagnostics.CausalityTraceLevel
  Diagnostics_CausalityTraceLevel = (
    Required = 0,
    Important = 1,
    Verbose = 2
  );
  PDiagnostics_CausalityTraceLevel = ^Diagnostics_CausalityTraceLevel;

  // Windows.Foundation.Diagnostics.ErrorOptions
  Diagnostics_ErrorOptions = (
    None = 0,
    SuppressExceptions = 1,
    ForceExceptions = 2,
    UseSetErrorInfo = 4,
    SuppressSetErrorInfo = 8
  );
  PDiagnostics_ErrorOptions = ^Diagnostics_ErrorOptions;

  // Windows.Foundation.Diagnostics.LoggingFieldFormat
  Diagnostics_LoggingFieldFormat = (
    Default = 0,
    Hidden = 1,
    &String = 2,
    Boolean = 3,
    Hexadecimal = 4,
    ProcessId = 5,
    ThreadId = 6,
    Port = 7,
    Ipv4Address = 8,
    Ipv6Address = 9,
    SocketAddress = 10,
    Xml = 11,
    Json = 12,
    Win32Error = 13,
    NTStatus = 14,
    HResult = 15,
    FileTime = 16,
    Signed = 17,
    Unsigned = 18
  );
  PDiagnostics_LoggingFieldFormat = ^Diagnostics_LoggingFieldFormat;

  // Windows.Foundation.Diagnostics.LoggingLevel
  Diagnostics_LoggingLevel = (
    Verbose = 0,
    Information = 1,
    Warning = 2,
    Error = 3,
    Critical = 4
  );
  PDiagnostics_LoggingLevel = ^Diagnostics_LoggingLevel;

  // Windows.Foundation.Diagnostics.LoggingOpcode
  Diagnostics_LoggingOpcode = (
    Info = 0,
    Start = 1,
    Stop = 2,
    Reply = 6,
    Resume = 7,
    Suspend = 8,
    Send = 9
  );
  PDiagnostics_LoggingOpcode = ^Diagnostics_LoggingOpcode;

  // Windows.Foundation.Metadata.AttributeTargets
  Metadata_AttributeTargets = (
    All = -1,
    Delegate = 1,
    Enum = 2,
    Event = 4,
    Field = 8,
    &Interface = 16,
    Method = 64,
    Parameter = 128,
    &Property = 256,
    RuntimeClass = 512,
    Struct = 1024,
    InterfaceImpl = 2048,
    ApiContract = 8192
  );
  PMetadata_AttributeTargets = ^Metadata_AttributeTargets;

  // Windows.Foundation.Metadata.CompositionType
  Metadata_CompositionType = (
    &Protected = 1,
    &Public = 2
  );
  PMetadata_CompositionType = ^Metadata_CompositionType;

  // Windows.Foundation.Metadata.DeprecationType
  Metadata_DeprecationType = (
    Deprecate = 0,
    Remove = 1
  );
  PMetadata_DeprecationType = ^Metadata_DeprecationType;

  // Windows.Foundation.Metadata.FeatureStage
  Metadata_FeatureStage = (
    AlwaysDisabled = 0,
    DisabledByDefault = 1,
    EnabledByDefault = 2,
    AlwaysEnabled = 3
  );
  PMetadata_FeatureStage = ^Metadata_FeatureStage;

  // Windows.Foundation.Metadata.GCPressureAmount
  Metadata_GCPressureAmount = (
    Low = 0,
    Medium = 1,
    High = 2
  );
  PMetadata_GCPressureAmount = ^Metadata_GCPressureAmount;

  // Windows.Foundation.Metadata.MarshalingType
  Metadata_MarshalingType = (
    None = 1,
    Agile = 2,
    Standard = 3,
    InvalidMarshaling = 0
  );
  PMetadata_MarshalingType = ^Metadata_MarshalingType;

  // Windows.Foundation.Metadata.Platform
  Metadata_Platform = (
    Windows = 0,
    WindowsPhone = 1
  );
  PMetadata_Platform = ^Metadata_Platform;

  // Windows.Foundation.Metadata.ThreadingModel
  Metadata_ThreadingModel = (
    STA = 1,
    MTA = 2,
    Both = 3,
    InvalidThreading = 0
  );
  PMetadata_ThreadingModel = ^Metadata_ThreadingModel;

  // Windows.Foundation.PropertyType
  PropertyType = (
    Empty = 0,
    UInt8 = 1,
    Int16 = 2,
    UInt16 = 3,
    Int32 = 4,
    UInt32 = 5,
    Int64 = 6,
    UInt64 = 7,
    Single = 8,
    Double = 9,
    Char16 = 10,
    Boolean = 11,
    &String = 12,
    Inspectable = 13,
    DateTime = 14,
    TimeSpan = 15,
    Guid = 16,
    Point = 17,
    Size = 18,
    Rect = 19,
    OtherType = 20,
    UInt8Array = 1025,
    Int16Array = 1026,
    UInt16Array = 1027,
    Int32Array = 1028,
    UInt32Array = 1029,
    Int64Array = 1030,
    UInt64Array = 1031,
    SingleArray = 1032,
    DoubleArray = 1033,
    Char16Array = 1034,
    BooleanArray = 1035,
    StringArray = 1036,
    InspectableArray = 1037,
    DateTimeArray = 1038,
    TimeSpanArray = 1039,
    GuidArray = 1040,
    PointArray = 1041,
    SizeArray = 1042,
    RectArray = 1043,
    OtherTypeArray = 1044
  );
  PPropertyType = ^PropertyType;

  // Windows.Foundation Records
  // Windows.Foundation.FoundationContract
  FoundationContract = record
  end;
  PFoundationContract = ^FoundationContract;

  // Windows.Foundation.Numerics.Plane
  Numerics_Plane = record
    Normal: Numerics_Vector3;
    D: Single;
  end;
  PNumerics_Plane = ^Numerics_Plane;

  // Windows.Foundation.Numerics.Rational
  Numerics_Rational = record
    Numerator: Cardinal;
    Denominator: Cardinal;
  end;
  PNumerics_Rational = ^Numerics_Rational;

  // Windows.Foundation.UniversalApiContract
  UniversalApiContract = record
  end;
  PUniversalApiContract = ^UniversalApiContract;

  // Windows.Foundation Interfaces

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IClosable
  IClosable = interface(IInspectable)
  ['{30D5A829-7FA4-4026-83BB-D75BAE4EA99E}']
    procedure Close; safecall;
  end;

  // Windows.Foundation.IAsyncInfo
  IAsyncInfo = interface(IInspectable)
  ['{00000036-0000-0000-C000-000000000046}']
    function get_Id: Cardinal; safecall;
    function get_Status: AsyncStatus; safecall;
    function get_ErrorCode: HRESULT; safecall;
    procedure Cancel; safecall;
    procedure Close; safecall;
    property ErrorCode: HRESULT read get_ErrorCode;
    property Id: Cardinal read get_Id;
    property Status: AsyncStatus read get_Status;
  end;

  // Windows.Foundation.IPropertyValue
  IPropertyValue = interface(IInspectable)
  ['{4BD682DD-7554-40E9-9A9B-82654EDE7E62}']
    function get_Type: PropertyType; safecall;
    function get_IsNumericScalar: Boolean; safecall;
    function GetUInt8: Byte; safecall;
    function GetInt16: SmallInt; safecall;
    function GetUInt16: Word; safecall;
    function GetInt32: Integer; safecall;
    function GetUInt32: Cardinal; safecall;
    function GetInt64: Int64; safecall;
    function GetUInt64: UInt64; safecall;
    function GetSingle: Single; safecall;
    function GetDouble: Double; safecall;
    function GetChar16: Char; safecall;
    function GetBoolean: Boolean; safecall;
    function GetString: HSTRING; safecall;
    function GetGuid: TGuid; safecall;
    function GetDateTime: DateTime; safecall;
    function GetTimeSpan: TimeSpan; safecall;
    function GetPoint: TPointF; safecall;
    function GetSize: TSizeF; safecall;
    function GetRect: TRectF; safecall;
    procedure GetUInt8Array(valueSize: Cardinal; value: PByte); safecall;
    procedure GetInt16Array(valueSize: Cardinal; value: PSmallInt); safecall;
    procedure GetUInt16Array(valueSize: Cardinal; value: PWord); safecall;
    procedure GetInt32Array(valueSize: Cardinal; value: PInteger); safecall;
    procedure GetUInt32Array(valueSize: Cardinal; value: PCardinal); safecall;
    procedure GetInt64Array(valueSize: Cardinal; value: PInt64); safecall;
    procedure GetUInt64Array(valueSize: Cardinal; value: PUInt64); safecall;
    procedure GetSingleArray(valueSize: Cardinal; value: PSingle); safecall;
    procedure GetDoubleArray(valueSize: Cardinal; value: PDouble); safecall;
    procedure GetChar16Array(valueSize: Cardinal; value: PChar); safecall;
    procedure GetBooleanArray(valueSize: Cardinal; value: PBoolean); safecall;
    procedure GetStringArray(valueSize: Cardinal; value: PHSTRING); safecall;
    procedure GetInspectableArray(valueSize: Cardinal; value: PIInspectable); safecall;
    procedure GetGuidArray(valueSize: Cardinal; value: PGuid); safecall;
    procedure GetDateTimeArray(valueSize: Cardinal; value: PDateTime); safecall;
    procedure GetTimeSpanArray(valueSize: Cardinal; value: PTimeSpan); safecall;
    procedure GetPointArray(valueSize: Cardinal; value: PPointF); safecall;
    procedure GetSizeArray(valueSize: Cardinal; value: PSizeF); safecall;
    procedure GetRectArray(valueSize: Cardinal; value: PRectF); safecall;
    property IsNumericScalar: Boolean read get_IsNumericScalar;
    property &Type: PropertyType read get_Type;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Vector3>
  IReference_1__Numerics_Vector3 = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Numerics_Vector3; safecall;
    property Value: Numerics_Vector3 read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IMemoryBufferReference
  IMemoryBufferReference = interface(IInspectable)
  ['{FBC4DD29-245B-11E4-AF98-689423260CF8}']
    function get_Capacity: Cardinal; safecall;
    function add_Closed(handler: TypedEventHandler_2__IMemoryBufferReference__IInspectable): EventRegistrationToken; safecall;
    procedure remove_Closed(cookie: EventRegistrationToken); safecall;
    property Capacity: Cardinal read get_Capacity;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IMemoryBuffer
  [WinRTClassNameAttribute(SWindows_Foundation_MemoryBuffer)]
  IMemoryBuffer = interface(IInspectable)
  ['{FBC4DD2A-245B-11E4-AF98-689423260CF8}']
    function CreateReference: IMemoryBufferReference; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IWwwFormUrlDecoderEntry
  IWwwFormUrlDecoderEntry = interface(IInspectable)
  ['{125E7431-F678-4E8E-B670-20A9B06C512D}']
    function get_Name: HSTRING; safecall;
    function get_Value: HSTRING; safecall;
    property Name: HSTRING read get_Name;
    property Value: HSTRING read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  IIterator_1__IWwwFormUrlDecoderEntry_Base = interface(IInspectable)
  ['{32E54295-373C-50CB-80A1-468A990CA780}']
    function get_Current: IWwwFormUrlDecoderEntry; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIWwwFormUrlDecoderEntry): Cardinal; safecall;
    property Current: IWwwFormUrlDecoderEntry read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  IIterator_1__IWwwFormUrlDecoderEntry = interface(IIterator_1__IWwwFormUrlDecoderEntry_Base)
  ['{32E54295-373C-50CB-80A1-468A990CA780}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  IIterable_1__IWwwFormUrlDecoderEntry_Base = interface(IInspectable)
  ['{876BE83B-7218-5BFB-A169-83152EF7E146}']
    function First: IIterator_1__IWwwFormUrlDecoderEntry; safecall;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  IIterable_1__IWwwFormUrlDecoderEntry = interface(IIterable_1__IWwwFormUrlDecoderEntry_Base)
  ['{876BE83B-7218-5BFB-A169-83152EF7E146}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.DateTime>
  IIterator_1__DateTime_Base = interface(IInspectable)
  ['{F56158DF-8947-5480-96ED-36C1057877EA}']
    function get_Current: DateTime; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PDateTime): Cardinal; safecall;
    property Current: DateTime read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.DateTime>
  IIterator_1__DateTime = interface(IIterator_1__DateTime_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.DateTime>
  IIterable_1__DateTime_Base = interface(IInspectable)
  ['{576A207D-977C-5B36-B54D-624EC86C53A3}']
    function First: IIterator_1__DateTime; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.DateTime>
  IIterable_1__DateTime = interface(IIterable_1__DateTime_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.DateTime>
  IVectorView_1__DateTime = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): DateTime; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: DateTime; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDateTime): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.DateTime>
  IVector_1__DateTime_Base = interface(IInspectable)
  ['{94390DC5-E442-5870-88B6-007E232F902C}']
    function GetAt(index: Cardinal): DateTime; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__DateTime; safecall;
    function IndexOf(value: DateTime; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: DateTime); safecall;
    procedure InsertAt(index: Cardinal; value: DateTime); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: DateTime); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PDateTime): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PDateTime); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.DateTime>
  IVector_1__DateTime = interface(IVector_1__DateTime_Base)
  ['{A4739064-B54E-55D4-8012-317E2B6A807B}']
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Rect>
  IIterator_1__Rect = interface(IInspectable)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
    function get_Current: TRectF; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PRectF): Cardinal; safecall;
    property Current: TRectF read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Rect>
  IIterable_1__Rect = interface(IInspectable)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
    function First: IIterator_1__Rect; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Rect>
  IVectorView_1__Rect = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): TRectF; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TRectF; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PRectF): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IReference`1<Windows.Foundation.DateTime>>
  AsyncOperationCompletedHandler_1__IReference_1__DateTime_Delegate_Base = interface(IUnknown)
  ['{C4225D5E-1B7C-571E-9B88-2AB2EEFA8C8F}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IReference_1__DateTime; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IReference`1<Windows.Foundation.DateTime>>
  AsyncOperationCompletedHandler_1__IReference_1__DateTime = interface(AsyncOperationCompletedHandler_1__IReference_1__DateTime_Delegate_Base)
  ['{1AD5D0F6-8398-587D-AA9A-978252B1BBA2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IReference`1<Windows.Foundation.DateTime>>
  IAsyncOperation_1__IReference_1__DateTime_Base = interface(IInspectable)
  ['{2025B34F-4214-56AB-ABFE-2FBE6595DA9D}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IReference_1__DateTime); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IReference_1__DateTime; safecall;
    function GetResults: IReference_1__DateTime; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IReference_1__DateTime read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IReference`1<Windows.Foundation.DateTime>>
  IAsyncOperation_1__IReference_1__DateTime = interface(IAsyncOperation_1__IReference_1__DateTime_Base)
  ['{00BEDA26-245F-5D19-B775-4DE00BBDC644}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>>
  AsyncOperationCompletedHandler_1__IReference_1__TimeSpan_Delegate_Base = interface(IUnknown)
  ['{E137B677-BFEF-54B0-B200-95C5C2902A25}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IReference_1__TimeSpan; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>>
  AsyncOperationCompletedHandler_1__IReference_1__TimeSpan = interface(AsyncOperationCompletedHandler_1__IReference_1__TimeSpan_Delegate_Base)
  ['{1AD5D0F6-8398-587D-AA9A-978252B1BBA2}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>>
  IAsyncOperation_1__IReference_1__TimeSpan_Base = interface(IInspectable)
  ['{24A901AD-910F-5C0F-B23C-67007577A558}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IReference_1__TimeSpan); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IReference_1__TimeSpan; safecall;
    function GetResults: IReference_1__TimeSpan; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IReference_1__TimeSpan read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.IReference`1<Windows.Foundation.TimeSpan>>
  IAsyncOperation_1__IReference_1__TimeSpan = interface(IAsyncOperation_1__IReference_1__TimeSpan_Base)
  ['{00BEDA26-245F-5D19-B775-4DE00BBDC644}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>>
  IIterator_1__IIterable_1__Point_Base = interface(IInspectable)
  ['{377F6162-6E4D-574E-BF01-77F4FD021D0E}']
    function get_Current: IIterable_1__Point; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIIterable_1__Point): Cardinal; safecall;
    property Current: IIterable_1__Point read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>>
  IIterator_1__IIterable_1__Point = interface(IIterator_1__IIterable_1__Point_Base)
  ['{66C2C906-6BCA-5085-BBA5-C7D445D25EE6}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>>
  IIterable_1__IIterable_1__Point_Base = interface(IInspectable)
  ['{AE44597E-D411-5B7F-BBEC-6A96C94A107A}']
    function First: IIterator_1__IIterable_1__Point; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Point>>
  IIterable_1__IIterable_1__Point = interface(IIterable_1__IIterable_1__Point_Base)
  ['{0FB49B66-3A39-5E05-B730-60ADDFB9795F}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Point>
  IVectorView_1__Point = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): TPointF; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TPointF; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPointF): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.Point>
  IVector_1__Point_Base = interface(IInspectable)
  ['{C0D513A9-EC4A-5A5D-B6D5-B707DEFDB9F7}']
    function GetAt(index: Cardinal): TPointF; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__Point; safecall;
    function IndexOf(value: TPointF; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: TPointF); safecall;
    procedure InsertAt(index: Cardinal; value: TPointF); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: TPointF); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PPointF): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PPointF); safecall;
    property Size: Cardinal read get_Size;
  end;
  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.Point>
  [WinRTClassNameAttribute(SMicrosoft_UI_Xaml_Media_PointCollection)]
  IVector_1__Point = interface(IVector_1__Point_Base)
  ['{A4739064-B54E-55D4-8012-317E2B6A807B}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.IGetActivationFactory
  IGetActivationFactory = interface(IInspectable)
  ['{4EDB8EE2-96DD-49A7-94F7-4607DDAB8E3C}']
    function GetActivationFactory(activatableClassId: HSTRING): IInspectable; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Matrix4x4>
  IReference_1__Numerics_Matrix4x4 = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Numerics_Matrix4x4; safecall;
    property Value: Numerics_Matrix4x4 read get_Value;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IReference`1<Windows.Foundation.Size>
  IReference_1__Size = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: TSizeF; safecall;
    property Value: TSizeF read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Size>
  IIterator_1__Size_Base = interface(IInspectable)
  ['{A3508EE0-3527-5144-894D-422EADEF43D7}']
    function get_Current: TSizeF; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSizeF): Cardinal; safecall;
    property Current: TSizeF read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Size>
  IIterator_1__Size = interface(IIterator_1__Size_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Size>
  IIterable_1__Size_Base = interface(IInspectable)
  ['{C9DF55C3-4D41-5E90-BA76-E89ED564446B}']
    function First: IIterator_1__Size; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Size>
  IIterable_1__Size = interface(IIterable_1__Size_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.Size>
  IVectorView_1__Size = interface(IInspectable)
  ['{86D0B56E-CB4E-58F0-B9A2-1528619DCD26}']
    function GetAt(index: Cardinal): TSizeF; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: TSizeF; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSizeF): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable_Base = interface(IInspectable)
  ['{FE2F3D47-5D47-5499-8374-430C7CDA0204}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IInspectable; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  IIterable_1__IKeyValuePair_2__HSTRING__IInspectable = interface(IIterable_1__IKeyValuePair_2__HSTRING__IInspectable_Base)
  ['{FE2F3D47-5D47-5499-8374-430C7CDA0204}']
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

  // UsedAPI Interface
  // Windows.Foundation.DeferralCompletedHandler
  DeferralCompletedHandler = interface(IUnknown)
  ['{ED32A372-F3C8-4FAA-9CFB-470148DA3888}']
    procedure Invoke; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Diagnostics.ITracingStatusChangedEventArgs
  Diagnostics_ITracingStatusChangedEventArgs = interface(IInspectable)
  ['{410B7711-FF3B-477F-9C9A-D2EFDA302DC3}']
    function get_Enabled: Boolean; safecall;
    function get_TraceLevel: Diagnostics_CausalityTraceLevel; safecall;
    property Enabled: Boolean read get_Enabled;
    property TraceLevel: Diagnostics_CausalityTraceLevel read get_TraceLevel;
  end;

  // Generic Delegate for:
  // Windows.Foundation.EventHandler`1<Windows.Foundation.Diagnostics.ITracingStatusChangedEventArgs>
  EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs_Delegate_Base = interface(IUnknown)
  ['{2BF27008-2EB4-5675-B1CD-E9906CC5CE64}']
    procedure Invoke(sender: IInspectable; args: Diagnostics_ITracingStatusChangedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.Foundation.Diagnostics.ITracingStatusChangedEventArgs>
  EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs = interface(EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs_Delegate_Base)
  ['{B505DAE1-062C-506F-BB6A-3E1DE7ED6E76}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Diagnostics.IAsyncCausalityTracerStatics
  [WinRTClassNameAttribute(SWindows_Foundation_Diagnostics_AsyncCausalityTracer)]
  Diagnostics_IAsyncCausalityTracerStatics = interface(IInspectable)
  ['{50850B26-267E-451B-A890-AB6A370245EE}']
    procedure TraceOperationCreation(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; operationName: HSTRING; relatedContext: UInt64); safecall;
    procedure TraceOperationCompletion(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; status: AsyncStatus); safecall;
    procedure TraceOperationRelation(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; relation: Diagnostics_CausalityRelation); safecall;
    procedure TraceSynchronousWorkStart(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; work: Diagnostics_CausalitySynchronousWork); safecall;
    procedure TraceSynchronousWorkCompletion(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; work: Diagnostics_CausalitySynchronousWork); safecall;
    function add_TracingStatusChanged(handler: EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_TracingStatusChanged(cookie: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Diagnostics.IErrorReportingSettings
  [WinRTClassNameAttribute(SWindows_Foundation_Diagnostics_RuntimeBrokerErrorSettings)]
  Diagnostics_IErrorReportingSettings = interface(IInspectable)
  ['{16369792-B03E-4BA1-8BB8-D28F4AB4D2C0}']
    procedure SetErrorOptions(value: Diagnostics_ErrorOptions); safecall;
    function GetErrorOptions: Diagnostics_ErrorOptions; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IDeferralFactory
  [WinRTClassNameAttribute(SWindows_Foundation_Deferral)]
  IDeferralFactory = interface(IInspectable)
  ['{65A1ECC5-3FB5-4832-8CA9-F061B281D13A}']
    function Create(handler: DeferralCompletedHandler): IDeferral; safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable_Delegate_Base = interface(IUnknown)
  ['{F4637D4A-0760-5431-BFC0-24EB1D4F6C4F}']
    procedure Invoke(sender: IMemoryBufferReference; args: IInspectable); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface(TypedEventHandler_2__IMemoryBufferReference__IInspectable_Delegate_Base)
  ['{F4637D4A-0760-5431-BFC0-24EB1D4F6C4F}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IMemoryBufferFactory
  [WinRTClassNameAttribute(SWindows_Foundation_MemoryBuffer)]
  IMemoryBufferFactory = interface(IInspectable)
  ['{FBC4DD2B-245B-11E4-AF98-689423260CF8}']
    function Create(capacity: Cardinal): IMemoryBuffer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IPropertyValueStatics
  [WinRTClassNameAttribute(SWindows_Foundation_PropertyValue)]
  IPropertyValueStatics = interface(IInspectable)
  ['{629BDBC8-D932-4FF4-96B9-8D96C5C1E858}']
    function CreateEmpty: IInspectable; safecall;
    function CreateUInt8(value: Byte): IInspectable; safecall;
    function CreateInt16(value: SmallInt): IInspectable; safecall;
    function CreateUInt16(value: Word): IInspectable; safecall;
    function CreateInt32(value: Integer): IInspectable; safecall;
    function CreateUInt32(value: Cardinal): IInspectable; safecall;
    function CreateInt64(value: Int64): IInspectable; safecall;
    function CreateUInt64(value: UInt64): IInspectable; safecall;
    function CreateSingle(value: Single): IInspectable; safecall;
    function CreateDouble(value: Double): IInspectable; safecall;
    function CreateChar16(value: Char): IInspectable; safecall;
    function CreateBoolean(value: Boolean): IInspectable; safecall;
    function CreateString(value: HSTRING): IInspectable; safecall;
    function CreateInspectable(value: IInspectable): IInspectable; safecall;
    function CreateGuid(value: TGuid): IInspectable; safecall;
    function CreateDateTime(value: DateTime): IInspectable; safecall;
    function CreateTimeSpan(value: TimeSpan): IInspectable; safecall;
    function CreatePoint(value: TPointF): IInspectable; safecall;
    function CreateSize(value: TSizeF): IInspectable; safecall;
    function CreateRect(value: TRectF): IInspectable; safecall;
    function CreateUInt8Array(valueSize: Cardinal; value: PByte): IInspectable; safecall;
    function CreateInt16Array(valueSize: Cardinal; value: PSmallInt): IInspectable; safecall;
    function CreateUInt16Array(valueSize: Cardinal; value: PWord): IInspectable; safecall;
    function CreateInt32Array(valueSize: Cardinal; value: PInteger): IInspectable; safecall;
    function CreateUInt32Array(valueSize: Cardinal; value: PCardinal): IInspectable; safecall;
    function CreateInt64Array(valueSize: Cardinal; value: PInt64): IInspectable; safecall;
    function CreateUInt64Array(valueSize: Cardinal; value: PUInt64): IInspectable; safecall;
    function CreateSingleArray(valueSize: Cardinal; value: PSingle): IInspectable; safecall;
    function CreateDoubleArray(valueSize: Cardinal; value: PDouble): IInspectable; safecall;
    function CreateChar16Array(valueSize: Cardinal; value: PChar): IInspectable; safecall;
    function CreateBooleanArray(valueSize: Cardinal; value: PBoolean): IInspectable; safecall;
    function CreateStringArray(valueSize: Cardinal; value: PHSTRING): IInspectable; safecall;
    function CreateInspectableArray(valueSize: Cardinal; value: PIInspectable): IInspectable; safecall;
    function CreateGuidArray(valueSize: Cardinal; value: PGuid): IInspectable; safecall;
    function CreateDateTimeArray(valueSize: Cardinal; value: PDateTime): IInspectable; safecall;
    function CreateTimeSpanArray(valueSize: Cardinal; value: PTimeSpan): IInspectable; safecall;
    function CreatePointArray(valueSize: Cardinal; value: PPointF): IInspectable; safecall;
    function CreateSizeArray(valueSize: Cardinal; value: PSizeF): IInspectable; safecall;
    function CreateRectArray(valueSize: Cardinal; value: PRectF): IInspectable; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IStringable
  IStringable = interface(IInspectable)
  ['{96369F54-8EB6-48F0-ABCE-C1B211E627C3}']
    function ToString: HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IUriEscapeStatics
  [WinRTClassNameAttribute(SWindows_Foundation_Uri)]
  IUriEscapeStatics = interface(IInspectable)
  ['{C1D432BA-C824-4452-A7FD-512BC3BBE9A1}']
    function UnescapeComponent(toUnescape: HSTRING): HSTRING; safecall;
    function EscapeComponent(toEscape: HSTRING): HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IUriRuntimeClassFactory
  [WinRTClassNameAttribute(SWindows_Foundation_Uri)]
  IUriRuntimeClassFactory = interface(IInspectable)
  ['{44A9796F-723E-4FDF-A218-033E75B0C084}']
    function CreateUri(uri: HSTRING): IUriRuntimeClass; safecall;
    function CreateWithRelativeUri(baseUri: HSTRING; relativeUri: HSTRING): IUriRuntimeClass; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IUriRuntimeClassWithAbsoluteCanonicalUri
  IUriRuntimeClassWithAbsoluteCanonicalUri = interface(IInspectable)
  ['{758D9661-221C-480F-A339-50656673F46F}']
    function get_AbsoluteCanonicalUri: HSTRING; safecall;
    function get_DisplayIri: HSTRING; safecall;
    property AbsoluteCanonicalUri: HSTRING read get_AbsoluteCanonicalUri;
    property DisplayIri: HSTRING read get_DisplayIri;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.IWwwFormUrlDecoderRuntimeClassFactory
  [WinRTClassNameAttribute(SWindows_Foundation_WwwFormUrlDecoder)]
  IWwwFormUrlDecoderRuntimeClassFactory = interface(IInspectable)
  ['{5B8C6B3D-24AE-41B5-A1BF-F0C3D544845B}']
    function CreateWwwFormUrlDecoder(query: HSTRING): IWwwFormUrlDecoderRuntimeClass; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Foundation.Metadata.IApiInformationStatics
  [WinRTClassNameAttribute(SWindows_Foundation_Metadata_ApiInformation)]
  Metadata_IApiInformationStatics = interface(IInspectable)
  ['{997439FE-F681-4A11-B416-C13A47E8BA36}']
    function IsTypePresent(typeName: HSTRING): Boolean; safecall;
    function IsMethodPresent(typeName: HSTRING; methodName: HSTRING): Boolean; overload; safecall;
    function IsMethodPresent(typeName: HSTRING; methodName: HSTRING; inputParameterCount: Cardinal): Boolean; overload; safecall;
    function IsEventPresent(typeName: HSTRING; eventName: HSTRING): Boolean; safecall;
    function IsPropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean; safecall;
    function IsReadOnlyPropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean; safecall;
    function IsWriteablePropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean; safecall;
    function IsEnumNamedValuePresent(enumTypeName: HSTRING; valueName: HSTRING): Boolean; safecall;
    function IsApiContractPresent(contractName: HSTRING; majorVersion: Word): Boolean; overload; safecall;
    function IsApiContractPresent(contractName: HSTRING; majorVersion: Word; minorVersion: Word): Boolean; overload; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IClosable>
  IIterator_1__IClosable = interface(IInspectable)
  ['{C9463957-E47D-5649-9874-4B13AE23061B}']
    function get_Current: IClosable; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIClosable): Cardinal; safecall;
    property Current: IClosable read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IClosable>
  IIterable_1__IClosable = interface(IInspectable)
  ['{44DA7ECF-B8CF-5DEF-8BF1-664578A8FB16}']
    function First: IIterator_1__IClosable; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.IClosable>
  IVectorView_1__IClosable = interface(IInspectable)
  ['{26DEBA5E-F73B-5181-94DB-2FCBC1DBAF8F}']
    function GetAt(index: Cardinal): IClosable; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IClosable; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIClosable): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Windows.Foundation.Collections.IVector`1<Windows.Foundation.IClosable>
  IVector_1__IClosable = interface(IInspectable)
  ['{1BFCA4F6-2C4E-5174-9869-B39D35848FCC}']
    function GetAt(index: Cardinal): IClosable; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IClosable; safecall;
    function IndexOf(value: IClosable; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IClosable); safecall;
    procedure InsertAt(index: Cardinal; value: IClosable); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IClosable); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIClosable): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIClosable); safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IUriRuntimeClass>
  IIterator_1__IUriRuntimeClass_Base = interface(IInspectable)
  ['{1C157D0F-5EFE-5CEC-BBD6-0C6CE9AF07A5}']
    function get_Current: IUriRuntimeClass; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIUriRuntimeClass): Cardinal; safecall;
    property Current: IUriRuntimeClass read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.IUriRuntimeClass>
  IIterator_1__IUriRuntimeClass = interface(IIterator_1__IUriRuntimeClass_Base)
  ['{E070225C-CB16-5FE3-8CC4-CAB4CE987C97}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IUriRuntimeClass>
  IIterable_1__IUriRuntimeClass_Base = interface(IInspectable)
  ['{B0D63B78-78AD-5E31-B6D8-E32A0E16C447}']
    function First: IIterator_1__IUriRuntimeClass; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IUriRuntimeClass>
  IIterable_1__IUriRuntimeClass = interface(IIterable_1__IUriRuntimeClass_Base)
  ['{EA0329A4-F285-5263-A1BA-D87F0BF8D236}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.TimeSpan>
  IIterator_1__TimeSpan_Base = interface(IInspectable)
  ['{67E9EADB-324B-5661-A405-DED8445B1EEA}']
    function get_Current: TimeSpan; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PTimeSpan): Cardinal; safecall;
    property Current: TimeSpan read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.TimeSpan>
  IIterator_1__TimeSpan = interface(IIterator_1__TimeSpan_Base)
  ['{752850B9-5ED2-5655-8DE2-262EFC26CF39}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.TimeSpan>
  IIterable_1__TimeSpan_Base = interface(IInspectable)
  ['{E9F78726-829A-5F67-8D19-95EF154B7742}']
    function First: IIterator_1__TimeSpan; safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.TimeSpan>
  IIterable_1__TimeSpan = interface(IIterable_1__TimeSpan_Base)
  ['{1B6614A1-8FC5-567D-9157-410A9E0ECBC5}']
  end;

  // Windows.Foundation.IReference`1<Windows.Foundation.Numerics.Quaternion>
  IReference_1__Numerics_Quaternion = interface(IInspectable)
  ['{455ACF7B-8F11-5BB9-93BE-7A214CD5A134}']
    function get_Value: Numerics_Quaternion; safecall;
    property Value: Numerics_Quaternion read get_Value;
  end;

  // Windows.Foundation.Deferral
  // DualAPI
  // Implements: Windows.Foundation.IDeferral
  // Implements: Windows.Foundation.IClosable
  // Factory: "Windows.Foundation.IDeferralFactory"
  TDeferral = class(TWinRTGenericImportF<IDeferralFactory>)
  public
    // -> IDeferralFactory
    class function Create(handler: DeferralCompletedHandler): IDeferral; static; inline;
  end;

  // Windows.Foundation.Diagnostics.AsyncCausalityTracer
  // DualAPI
  // Statics: "Windows.Foundation.Diagnostics.IAsyncCausalityTracerStatics"
  TDiagnostics_AsyncCausalityTracer = class(TWinRTGenericImportS<Diagnostics_IAsyncCausalityTracerStatics>)
  public
    // -> Diagnostics_IAsyncCausalityTracerStatics
    class procedure TraceOperationCreation(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; operationName: HSTRING; relatedContext: UInt64); static; inline;
    class procedure TraceOperationCompletion(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; status: AsyncStatus); static; inline;
    class procedure TraceOperationRelation(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; relation: Diagnostics_CausalityRelation); static; inline;
    class procedure TraceSynchronousWorkStart(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; work: Diagnostics_CausalitySynchronousWork); static; inline;
    class procedure TraceSynchronousWorkCompletion(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; work: Diagnostics_CausalitySynchronousWork); static; inline;
    class function add_TracingStatusChanged(handler: EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_TracingStatusChanged(cookie: EventRegistrationToken); static; inline;
  end;

  // Windows.Foundation.Diagnostics.RuntimeBrokerErrorSettings
  // DualAPI
  // Implements: Windows.Foundation.Diagnostics.IErrorReportingSettings
  // Instantiable: "Diagnostics_IErrorReportingSettings"
  TDiagnostics_RuntimeBrokerErrorSettings = class(TWinRTGenericImportI<Diagnostics_IErrorReportingSettings>) end;

  // Windows.Foundation.MemoryBuffer
  // DualAPI
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Factory: "Windows.Foundation.IMemoryBufferFactory"
  TMemoryBuffer = class(TWinRTGenericImportF<IMemoryBufferFactory>)
  public
    // -> IMemoryBufferFactory
    class function Create(capacity: Cardinal): IMemoryBuffer; static; inline;
  end;

  // Windows.Foundation.Metadata.ApiInformation
  // DualAPI
  // Statics: "Windows.Foundation.Metadata.IApiInformationStatics"
  TMetadata_ApiInformation = class(TWinRTGenericImportS<Metadata_IApiInformationStatics>)
  public
    // -> Metadata_IApiInformationStatics
    class function IsTypePresent(typeName: HSTRING): Boolean; static; inline;
    class function IsMethodPresent(typeName: HSTRING; methodName: HSTRING): Boolean; overload; static; inline;
    class function IsMethodPresent(typeName: HSTRING; methodName: HSTRING; inputParameterCount: Cardinal): Boolean; overload; static; inline;
    class function IsEventPresent(typeName: HSTRING; eventName: HSTRING): Boolean; static; inline;
    class function IsPropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean; static; inline;
    class function IsReadOnlyPropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean; static; inline;
    class function IsWriteablePropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean; static; inline;
    class function IsEnumNamedValuePresent(enumTypeName: HSTRING; valueName: HSTRING): Boolean; static; inline;
    class function IsApiContractPresent(contractName: HSTRING; majorVersion: Word): Boolean; overload; static; inline;
    class function IsApiContractPresent(contractName: HSTRING; majorVersion: Word; minorVersion: Word): Boolean; overload; static; inline;
  end;

  // Windows.Foundation.PropertyValue
  // DualAPI
  // Statics: "Windows.Foundation.IPropertyValueStatics"
  TPropertyValue = class(TWinRTGenericImportS<IPropertyValueStatics>)
  public
    // -> IPropertyValueStatics
    class function CreateEmpty: IInspectable; static; inline;
    class function CreateUInt8(value: Byte): IInspectable; static; inline;
    class function CreateInt16(value: SmallInt): IInspectable; static; inline;
    class function CreateUInt16(value: Word): IInspectable; static; inline;
    class function CreateInt32(value: Integer): IInspectable; static; inline;
    class function CreateUInt32(value: Cardinal): IInspectable; static; inline;
    class function CreateInt64(value: Int64): IInspectable; static; inline;
    class function CreateUInt64(value: UInt64): IInspectable; static; inline;
    class function CreateSingle(value: Single): IInspectable; static; inline;
    class function CreateDouble(value: Double): IInspectable; static; inline;
    class function CreateChar16(value: Char): IInspectable; static; inline;
    class function CreateBoolean(value: Boolean): IInspectable; static; inline;
    class function CreateString(value: HSTRING): IInspectable; static; inline;
    class function CreateInspectable(value: IInspectable): IInspectable; static; inline;
    class function CreateGuid(value: TGuid): IInspectable; static; inline;
    class function CreateDateTime(value: DateTime): IInspectable; static; inline;
    class function CreateTimeSpan(value: TimeSpan): IInspectable; static; inline;
    class function CreatePoint(value: TPointF): IInspectable; static; inline;
    class function CreateSize(value: TSizeF): IInspectable; static; inline;
    class function CreateRect(value: TRectF): IInspectable; static; inline;
    class function CreateUInt8Array(valueSize: Cardinal; value: PByte): IInspectable; static; inline;
    class function CreateInt16Array(valueSize: Cardinal; value: PSmallInt): IInspectable; static; inline;
    class function CreateUInt16Array(valueSize: Cardinal; value: PWord): IInspectable; static; inline;
    class function CreateInt32Array(valueSize: Cardinal; value: PInteger): IInspectable; static; inline;
    class function CreateUInt32Array(valueSize: Cardinal; value: PCardinal): IInspectable; static; inline;
    class function CreateInt64Array(valueSize: Cardinal; value: PInt64): IInspectable; static; inline;
    class function CreateUInt64Array(valueSize: Cardinal; value: PUInt64): IInspectable; static; inline;
    class function CreateSingleArray(valueSize: Cardinal; value: PSingle): IInspectable; static; inline;
    class function CreateDoubleArray(valueSize: Cardinal; value: PDouble): IInspectable; static; inline;
    class function CreateChar16Array(valueSize: Cardinal; value: PChar): IInspectable; static; inline;
    class function CreateBooleanArray(valueSize: Cardinal; value: PBoolean): IInspectable; static; inline;
    class function CreateStringArray(valueSize: Cardinal; value: PHSTRING): IInspectable; static; inline;
    class function CreateInspectableArray(valueSize: Cardinal; value: PIInspectable): IInspectable; static; inline;
    class function CreateGuidArray(valueSize: Cardinal; value: PGuid): IInspectable; static; inline;
    class function CreateDateTimeArray(valueSize: Cardinal; value: PDateTime): IInspectable; static; inline;
    class function CreateTimeSpanArray(valueSize: Cardinal; value: PTimeSpan): IInspectable; static; inline;
    class function CreatePointArray(valueSize: Cardinal; value: PPointF): IInspectable; static; inline;
    class function CreateSizeArray(valueSize: Cardinal; value: PSizeF): IInspectable; static; inline;
    class function CreateRectArray(valueSize: Cardinal; value: PRectF): IInspectable; static; inline;
  end;

  // Windows.Foundation.Uri
  // DualAPI
  // Implements: Windows.Foundation.IUriRuntimeClass
  // Implements: Windows.Foundation.IUriRuntimeClassWithAbsoluteCanonicalUri
  // Implements: Windows.Foundation.IStringable
  // Statics: "Windows.Foundation.IUriEscapeStatics"
  // Factory: "Windows.Foundation.IUriRuntimeClassFactory"
  TUri = class(TWinRTGenericImportFS<IUriRuntimeClassFactory, IUriEscapeStatics>)
  public
    // -> IUriEscapeStatics
    class function UnescapeComponent(toUnescape: HSTRING): HSTRING; static; inline;
    class function EscapeComponent(toEscape: HSTRING): HSTRING; static; inline;

    // -> IUriRuntimeClassFactory
    class function CreateUri(uri: HSTRING): IUriRuntimeClass; static; inline;
    class function CreateWithRelativeUri(baseUri: HSTRING; relativeUri: HSTRING): IUriRuntimeClass; static; inline;
  end;

  // Windows.Foundation.WwwFormUrlDecoder
  // DualAPI
  // Implements: Windows.Foundation.IWwwFormUrlDecoderRuntimeClass
  // Implements: Windows.Foundation.Collections.IVectorView`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.IWwwFormUrlDecoderEntry>
  // Factory: "Windows.Foundation.IWwwFormUrlDecoderRuntimeClassFactory"
  TWwwFormUrlDecoder = class(TWinRTGenericImportF<IWwwFormUrlDecoderRuntimeClassFactory>)
  public
    // -> IWwwFormUrlDecoderRuntimeClassFactory
    class function CreateWwwFormUrlDecoder(query: HSTRING): IWwwFormUrlDecoderRuntimeClass; static; inline;
  end;

implementation

{ TDeferral }
// Factories for : "Deferral"
// Factory: "Windows.Foundation.IDeferralFactory"
// -> IDeferralFactory

class function TDeferral.Create(handler: DeferralCompletedHandler): IDeferral;
begin
  Result := Factory.Create(handler);
end;


{ TDiagnostics_AsyncCausalityTracer }

class procedure TDiagnostics_AsyncCausalityTracer.TraceOperationCreation(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; operationName: HSTRING; relatedContext: UInt64);
begin
  Statics.TraceOperationCreation(traceLevel, source, platformId, operationId, operationName, relatedContext);
end;

class procedure TDiagnostics_AsyncCausalityTracer.TraceOperationCompletion(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; status: AsyncStatus);
begin
  Statics.TraceOperationCompletion(traceLevel, source, platformId, operationId, status);
end;

class procedure TDiagnostics_AsyncCausalityTracer.TraceOperationRelation(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; relation: Diagnostics_CausalityRelation);
begin
  Statics.TraceOperationRelation(traceLevel, source, platformId, operationId, relation);
end;

class procedure TDiagnostics_AsyncCausalityTracer.TraceSynchronousWorkStart(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; platformId: TGuid; operationId: UInt64; work: Diagnostics_CausalitySynchronousWork);
begin
  Statics.TraceSynchronousWorkStart(traceLevel, source, platformId, operationId, work);
end;

class procedure TDiagnostics_AsyncCausalityTracer.TraceSynchronousWorkCompletion(traceLevel: Diagnostics_CausalityTraceLevel; source: Diagnostics_CausalitySource; work: Diagnostics_CausalitySynchronousWork);
begin
  Statics.TraceSynchronousWorkCompletion(traceLevel, source, work);
end;

class function TDiagnostics_AsyncCausalityTracer.add_TracingStatusChanged(handler: EventHandler_1__Diagnostics_ITracingStatusChangedEventArgs): EventRegistrationToken;
begin
  Result := Statics.add_TracingStatusChanged(handler);
end;

class procedure TDiagnostics_AsyncCausalityTracer.remove_TracingStatusChanged(cookie: EventRegistrationToken);
begin
  Statics.remove_TracingStatusChanged(cookie);
end;


{ TDiagnostics_RuntimeBrokerErrorSettings }

{ TMemoryBuffer }
// Factories for : "MemoryBuffer"
// Factory: "Windows.Foundation.IMemoryBufferFactory"
// -> IMemoryBufferFactory

class function TMemoryBuffer.Create(capacity: Cardinal): IMemoryBuffer;
begin
  Result := Factory.Create(capacity);
end;


{ TMetadata_ApiInformation }

class function TMetadata_ApiInformation.IsTypePresent(typeName: HSTRING): Boolean;
begin
  Result := Statics.IsTypePresent(typeName);
end;

class function TMetadata_ApiInformation.IsMethodPresent(typeName: HSTRING; methodName: HSTRING): Boolean;
begin
  Result := Statics.IsMethodPresent(typeName, methodName);
end;

class function TMetadata_ApiInformation.IsMethodPresent(typeName: HSTRING; methodName: HSTRING; inputParameterCount: Cardinal): Boolean;
begin
  Result := Statics.IsMethodPresent(typeName, methodName, inputParameterCount);
end;

class function TMetadata_ApiInformation.IsEventPresent(typeName: HSTRING; eventName: HSTRING): Boolean;
begin
  Result := Statics.IsEventPresent(typeName, eventName);
end;

class function TMetadata_ApiInformation.IsPropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean;
begin
  Result := Statics.IsPropertyPresent(typeName, propertyName);
end;

class function TMetadata_ApiInformation.IsReadOnlyPropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean;
begin
  Result := Statics.IsReadOnlyPropertyPresent(typeName, propertyName);
end;

class function TMetadata_ApiInformation.IsWriteablePropertyPresent(typeName: HSTRING; propertyName: HSTRING): Boolean;
begin
  Result := Statics.IsWriteablePropertyPresent(typeName, propertyName);
end;

class function TMetadata_ApiInformation.IsEnumNamedValuePresent(enumTypeName: HSTRING; valueName: HSTRING): Boolean;
begin
  Result := Statics.IsEnumNamedValuePresent(enumTypeName, valueName);
end;

class function TMetadata_ApiInformation.IsApiContractPresent(contractName: HSTRING; majorVersion: Word): Boolean;
begin
  Result := Statics.IsApiContractPresent(contractName, majorVersion);
end;

class function TMetadata_ApiInformation.IsApiContractPresent(contractName: HSTRING; majorVersion: Word; minorVersion: Word): Boolean;
begin
  Result := Statics.IsApiContractPresent(contractName, majorVersion, minorVersion);
end;


{ TPropertyValue }

class function TPropertyValue.CreateEmpty: IInspectable;
begin
  Result := Statics.CreateEmpty;
end;

class function TPropertyValue.CreateUInt8(value: Byte): IInspectable;
begin
  Result := Statics.CreateUInt8(value);
end;

class function TPropertyValue.CreateInt16(value: SmallInt): IInspectable;
begin
  Result := Statics.CreateInt16(value);
end;

class function TPropertyValue.CreateUInt16(value: Word): IInspectable;
begin
  Result := Statics.CreateUInt16(value);
end;

class function TPropertyValue.CreateInt32(value: Integer): IInspectable;
begin
  Result := Statics.CreateInt32(value);
end;

class function TPropertyValue.CreateUInt32(value: Cardinal): IInspectable;
begin
  Result := Statics.CreateUInt32(value);
end;

class function TPropertyValue.CreateInt64(value: Int64): IInspectable;
begin
  Result := Statics.CreateInt64(value);
end;

class function TPropertyValue.CreateUInt64(value: UInt64): IInspectable;
begin
  Result := Statics.CreateUInt64(value);
end;

class function TPropertyValue.CreateSingle(value: Single): IInspectable;
begin
  Result := Statics.CreateSingle(value);
end;

class function TPropertyValue.CreateDouble(value: Double): IInspectable;
begin
  Result := Statics.CreateDouble(value);
end;

class function TPropertyValue.CreateChar16(value: Char): IInspectable;
begin
  Result := Statics.CreateChar16(value);
end;

class function TPropertyValue.CreateBoolean(value: Boolean): IInspectable;
begin
  Result := Statics.CreateBoolean(value);
end;

class function TPropertyValue.CreateString(value: HSTRING): IInspectable;
begin
  Result := Statics.CreateString(value);
end;

class function TPropertyValue.CreateInspectable(value: IInspectable): IInspectable;
begin
  Result := Statics.CreateInspectable(value);
end;

class function TPropertyValue.CreateGuid(value: TGuid): IInspectable;
begin
  Result := Statics.CreateGuid(value);
end;

class function TPropertyValue.CreateDateTime(value: DateTime): IInspectable;
begin
  Result := Statics.CreateDateTime(value);
end;

class function TPropertyValue.CreateTimeSpan(value: TimeSpan): IInspectable;
begin
  Result := Statics.CreateTimeSpan(value);
end;

class function TPropertyValue.CreatePoint(value: TPointF): IInspectable;
begin
  Result := Statics.CreatePoint(value);
end;

class function TPropertyValue.CreateSize(value: TSizeF): IInspectable;
begin
  Result := Statics.CreateSize(value);
end;

class function TPropertyValue.CreateRect(value: TRectF): IInspectable;
begin
  Result := Statics.CreateRect(value);
end;

class function TPropertyValue.CreateUInt8Array(valueSize: Cardinal; value: PByte): IInspectable;
begin
  Result := Statics.CreateUInt8Array(valueSize, value);
end;

class function TPropertyValue.CreateInt16Array(valueSize: Cardinal; value: PSmallInt): IInspectable;
begin
  Result := Statics.CreateInt16Array(valueSize, value);
end;

class function TPropertyValue.CreateUInt16Array(valueSize: Cardinal; value: PWord): IInspectable;
begin
  Result := Statics.CreateUInt16Array(valueSize, value);
end;

class function TPropertyValue.CreateInt32Array(valueSize: Cardinal; value: PInteger): IInspectable;
begin
  Result := Statics.CreateInt32Array(valueSize, value);
end;

class function TPropertyValue.CreateUInt32Array(valueSize: Cardinal; value: PCardinal): IInspectable;
begin
  Result := Statics.CreateUInt32Array(valueSize, value);
end;

class function TPropertyValue.CreateInt64Array(valueSize: Cardinal; value: PInt64): IInspectable;
begin
  Result := Statics.CreateInt64Array(valueSize, value);
end;

class function TPropertyValue.CreateUInt64Array(valueSize: Cardinal; value: PUInt64): IInspectable;
begin
  Result := Statics.CreateUInt64Array(valueSize, value);
end;

class function TPropertyValue.CreateSingleArray(valueSize: Cardinal; value: PSingle): IInspectable;
begin
  Result := Statics.CreateSingleArray(valueSize, value);
end;

class function TPropertyValue.CreateDoubleArray(valueSize: Cardinal; value: PDouble): IInspectable;
begin
  Result := Statics.CreateDoubleArray(valueSize, value);
end;

class function TPropertyValue.CreateChar16Array(valueSize: Cardinal; value: PChar): IInspectable;
begin
  Result := Statics.CreateChar16Array(valueSize, value);
end;

class function TPropertyValue.CreateBooleanArray(valueSize: Cardinal; value: PBoolean): IInspectable;
begin
  Result := Statics.CreateBooleanArray(valueSize, value);
end;

class function TPropertyValue.CreateStringArray(valueSize: Cardinal; value: PHSTRING): IInspectable;
begin
  Result := Statics.CreateStringArray(valueSize, value);
end;

class function TPropertyValue.CreateInspectableArray(valueSize: Cardinal; value: PIInspectable): IInspectable;
begin
  Result := Statics.CreateInspectableArray(valueSize, value);
end;

class function TPropertyValue.CreateGuidArray(valueSize: Cardinal; value: PGuid): IInspectable;
begin
  Result := Statics.CreateGuidArray(valueSize, value);
end;

class function TPropertyValue.CreateDateTimeArray(valueSize: Cardinal; value: PDateTime): IInspectable;
begin
  Result := Statics.CreateDateTimeArray(valueSize, value);
end;

class function TPropertyValue.CreateTimeSpanArray(valueSize: Cardinal; value: PTimeSpan): IInspectable;
begin
  Result := Statics.CreateTimeSpanArray(valueSize, value);
end;

class function TPropertyValue.CreatePointArray(valueSize: Cardinal; value: PPointF): IInspectable;
begin
  Result := Statics.CreatePointArray(valueSize, value);
end;

class function TPropertyValue.CreateSizeArray(valueSize: Cardinal; value: PSizeF): IInspectable;
begin
  Result := Statics.CreateSizeArray(valueSize, value);
end;

class function TPropertyValue.CreateRectArray(valueSize: Cardinal; value: PRectF): IInspectable;
begin
  Result := Statics.CreateRectArray(valueSize, value);
end;


{ TUri }

class function TUri.UnescapeComponent(toUnescape: HSTRING): HSTRING;
begin
  Result := Statics.UnescapeComponent(toUnescape);
end;

class function TUri.EscapeComponent(toEscape: HSTRING): HSTRING;
begin
  Result := Statics.EscapeComponent(toEscape);
end;

// Factories for : "Uri"
// Factory: "Windows.Foundation.IUriRuntimeClassFactory"
// -> IUriRuntimeClassFactory

class function TUri.CreateUri(uri: HSTRING): IUriRuntimeClass;
begin
  Result := Factory.CreateUri(uri);
end;

class function TUri.CreateWithRelativeUri(baseUri: HSTRING; relativeUri: HSTRING): IUriRuntimeClass;
begin
  Result := Factory.CreateWithRelativeUri(baseUri, relativeUri);
end;


{ TWwwFormUrlDecoder }
// Factories for : "WwwFormUrlDecoder"
// Factory: "Windows.Foundation.IWwwFormUrlDecoderRuntimeClassFactory"
// -> IWwwFormUrlDecoderRuntimeClassFactory

class function TWwwFormUrlDecoder.CreateWwwFormUrlDecoder(query: HSTRING): IWwwFormUrlDecoderRuntimeClass;
begin
  Result := Factory.CreateWwwFormUrlDecoder(query);
end;


end.
