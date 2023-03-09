{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.AI;

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
  Winapi.Media, 
  Winapi.GraphicsRT, 
  Winapi.ServicesRT, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  IIterable_1__Cardinal_Base = Winapi.CommonTypes.IIterable_1__Cardinal_Base;
  IIterable_1__Cardinal = Winapi.CommonTypes.IIterable_1__Cardinal;
  PIIterable_1__Cardinal = Winapi.CommonTypes.PIIterable_1__Cardinal;
  IIterator_1__Byte_Base = Winapi.CommonTypes.IIterator_1__Byte_Base;
  IIterator_1__Byte = Winapi.CommonTypes.IIterator_1__Byte;
  PIIterator_1__Byte = Winapi.CommonTypes.PIIterator_1__Byte;
  IIterator_1__Cardinal_Base = Winapi.CommonTypes.IIterator_1__Cardinal_Base;
  IIterator_1__Cardinal = Winapi.CommonTypes.IIterator_1__Cardinal;
  PIIterator_1__Cardinal = Winapi.CommonTypes.PIIterator_1__Cardinal;
  IIterator_1__Integer_Base = Winapi.CommonTypes.IIterator_1__Integer_Base;
  IIterator_1__Integer = Winapi.CommonTypes.IIterator_1__Integer;
  PIIterator_1__Integer = Winapi.CommonTypes.PIIterator_1__Integer;
  IIterator_1__Single_Base = Winapi.CommonTypes.IIterator_1__Single_Base;
  IIterator_1__Single = Winapi.CommonTypes.IIterator_1__Single;
  PIIterator_1__Single = Winapi.CommonTypes.PIIterator_1__Single;
  IVectorView_1__Cardinal = Winapi.CommonTypes.IVectorView_1__Cardinal;
  PIVectorView_1__Cardinal = Winapi.CommonTypes.PIVectorView_1__Cardinal;
  IVectorView_1__Integer = Winapi.CommonTypes.IVectorView_1__Integer;
  PIVectorView_1__Integer = Winapi.CommonTypes.PIVectorView_1__Integer;
  IVectorView_1__Single = Winapi.CommonTypes.IVectorView_1__Single;
  PIVectorView_1__Single = Winapi.CommonTypes.PIVectorView_1__Single;

  // Forward declarations for interfaces

  // Windows.AI.MachineLearning.IImageFeatureDescriptor
  MachineLearning_IImageFeatureDescriptor = interface;
  PMachineLearning_IImageFeatureDescriptor = ^MachineLearning_IImageFeatureDescriptor;

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

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface;
  PTypedEventHandler_2__IMemoryBufferReference__IInspectable = ^TypedEventHandler_2__IMemoryBufferReference__IInspectable;

  // Windows.AI.MachineLearning.IImageFeatureValue
  MachineLearning_IImageFeatureValue = interface;
  PMachineLearning_IImageFeatureValue = ^MachineLearning_IImageFeatureValue;

  // Windows.AI.MachineLearning.IImageFeatureValueStatics
  MachineLearning_IImageFeatureValueStatics = interface;
  PMachineLearning_IImageFeatureValueStatics = ^MachineLearning_IImageFeatureValueStatics;

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

  // Windows.AI.MachineLearning.ILearningModelFeatureDescriptor
  MachineLearning_ILearningModelFeatureDescriptor = interface;
  PMachineLearning_ILearningModelFeatureDescriptor = ^MachineLearning_ILearningModelFeatureDescriptor;

  // Windows.Foundation.Collections.IIterator`1<Windows.AI.MachineLearning.ILearningModelFeatureDescriptor>
  IIterator_1__MachineLearning_ILearningModelFeatureDescriptor = interface;
  PIIterator_1__MachineLearning_ILearningModelFeatureDescriptor = ^IIterator_1__MachineLearning_ILearningModelFeatureDescriptor;

  // Windows.Foundation.Collections.IIterable`1<Windows.AI.MachineLearning.ILearningModelFeatureDescriptor>
  IIterable_1__MachineLearning_ILearningModelFeatureDescriptor = interface;
  PIIterable_1__MachineLearning_ILearningModelFeatureDescriptor = ^IIterable_1__MachineLearning_ILearningModelFeatureDescriptor;

  // Windows.Foundation.Collections.IVectorView`1<Windows.AI.MachineLearning.ILearningModelFeatureDescriptor>
  IVectorView_1__MachineLearning_ILearningModelFeatureDescriptor = interface;
  PIVectorView_1__MachineLearning_ILearningModelFeatureDescriptor = ^IVectorView_1__MachineLearning_ILearningModelFeatureDescriptor;

  // Windows.AI.MachineLearning.ILearningModel
  MachineLearning_ILearningModel = interface;
  PMachineLearning_ILearningModel = ^MachineLearning_ILearningModel;

  // Windows.AI.MachineLearning.ILearningModelBinding
  MachineLearning_ILearningModelBinding = interface;
  PMachineLearning_ILearningModelBinding = ^MachineLearning_ILearningModelBinding;

  // Windows.AI.MachineLearning.ILearningModelDevice
  MachineLearning_ILearningModelDevice = interface;
  PMachineLearning_ILearningModelDevice = ^MachineLearning_ILearningModelDevice;

  // Windows.AI.MachineLearning.ILearningModelEvaluationResult
  MachineLearning_ILearningModelEvaluationResult = interface;
  PMachineLearning_ILearningModelEvaluationResult = ^MachineLearning_ILearningModelEvaluationResult;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.AI.MachineLearning.ILearningModelEvaluationResult>
  AsyncOperationCompletedHandler_1__MachineLearning_ILearningModelEvaluationResult = interface;
  PAsyncOperationCompletedHandler_1__MachineLearning_ILearningModelEvaluationResult = ^AsyncOperationCompletedHandler_1__MachineLearning_ILearningModelEvaluationResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.AI.MachineLearning.ILearningModelEvaluationResult>
  IAsyncOperation_1__MachineLearning_ILearningModelEvaluationResult = interface;
  PIAsyncOperation_1__MachineLearning_ILearningModelEvaluationResult = ^IAsyncOperation_1__MachineLearning_ILearningModelEvaluationResult;

  // Windows.AI.MachineLearning.ILearningModelSession
  MachineLearning_ILearningModelSession = interface;
  PMachineLearning_ILearningModelSession = ^MachineLearning_ILearningModelSession;

  // Windows.AI.MachineLearning.ILearningModelBindingFactory
  MachineLearning_ILearningModelBindingFactory = interface;
  PMachineLearning_ILearningModelBindingFactory = ^MachineLearning_ILearningModelBindingFactory;

  // Windows.AI.MachineLearning.ILearningModelDeviceFactory
  MachineLearning_ILearningModelDeviceFactory = interface;
  PMachineLearning_ILearningModelDeviceFactory = ^MachineLearning_ILearningModelDeviceFactory;

  // Windows.AI.MachineLearning.ILearningModelDeviceStatics
  MachineLearning_ILearningModelDeviceStatics = interface;
  PMachineLearning_ILearningModelDeviceStatics = ^MachineLearning_ILearningModelDeviceStatics;

  // Windows.AI.MachineLearning.ILearningModelFeatureValue
  MachineLearning_ILearningModelFeatureValue = interface;
  PMachineLearning_ILearningModelFeatureValue = ^MachineLearning_ILearningModelFeatureValue;

  // Windows.AI.MachineLearning.ILearningModelOperatorProvider
  MachineLearning_ILearningModelOperatorProvider = interface;
  PMachineLearning_ILearningModelOperatorProvider = ^MachineLearning_ILearningModelOperatorProvider;

  // Windows.AI.MachineLearning.ILearningModelSessionFactory
  MachineLearning_ILearningModelSessionFactory = interface;
  PMachineLearning_ILearningModelSessionFactory = ^MachineLearning_ILearningModelSessionFactory;

  // Windows.AI.MachineLearning.ILearningModelSessionOptions
  MachineLearning_ILearningModelSessionOptions = interface;
  PMachineLearning_ILearningModelSessionOptions = ^MachineLearning_ILearningModelSessionOptions;

  // Windows.AI.MachineLearning.ILearningModelSessionFactory2
  MachineLearning_ILearningModelSessionFactory2 = interface;
  PMachineLearning_ILearningModelSessionFactory2 = ^MachineLearning_ILearningModelSessionFactory2;

  // Windows.AI.MachineLearning.ILearningModelSessionOptions2
  MachineLearning_ILearningModelSessionOptions2 = interface;
  PMachineLearning_ILearningModelSessionOptions2 = ^MachineLearning_ILearningModelSessionOptions2;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.AI.MachineLearning.ILearningModel>
  AsyncOperationCompletedHandler_1__MachineLearning_ILearningModel = interface;
  PAsyncOperationCompletedHandler_1__MachineLearning_ILearningModel = ^AsyncOperationCompletedHandler_1__MachineLearning_ILearningModel;

  // Windows.Foundation.IAsyncOperation`1<Windows.AI.MachineLearning.ILearningModel>
  IAsyncOperation_1__MachineLearning_ILearningModel = interface;
  PIAsyncOperation_1__MachineLearning_ILearningModel = ^IAsyncOperation_1__MachineLearning_ILearningModel;

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

  // Windows.AI.MachineLearning.ILearningModelStatics
  MachineLearning_ILearningModelStatics = interface;
  PMachineLearning_ILearningModelStatics = ^MachineLearning_ILearningModelStatics;

  // Windows.AI.MachineLearning.IMapFeatureDescriptor
  MachineLearning_IMapFeatureDescriptor = interface;
  PMachineLearning_IMapFeatureDescriptor = ^MachineLearning_IMapFeatureDescriptor;

  // Windows.AI.MachineLearning.ISequenceFeatureDescriptor
  MachineLearning_ISequenceFeatureDescriptor = interface;
  PMachineLearning_ISequenceFeatureDescriptor = ^MachineLearning_ISequenceFeatureDescriptor;

  // Windows.Foundation.Collections.IIterator`1<Int64>
  IIterator_1__Int64 = interface;
  PIIterator_1__Int64 = ^IIterator_1__Int64;

  // Windows.Foundation.Collections.IIterable`1<Int64>
  IIterable_1__Int64 = interface;
  PIIterable_1__Int64 = ^IIterable_1__Int64;

  // Windows.Foundation.Collections.IVectorView`1<Int64>
  IVectorView_1__Int64 = interface;
  PIVectorView_1__Int64 = ^IVectorView_1__Int64;

  // Windows.AI.MachineLearning.ITensor
  MachineLearning_ITensor = interface;
  PMachineLearning_ITensor = ^MachineLearning_ITensor;

  // Windows.Foundation.Collections.IIterator`1<Boolean>
  IIterator_1__Boolean = interface;
  PIIterator_1__Boolean = ^IIterator_1__Boolean;

  // Windows.Foundation.Collections.IIterable`1<Boolean>
  IIterable_1__Boolean = interface;
  PIIterable_1__Boolean = ^IIterable_1__Boolean;

  // Windows.Foundation.Collections.IVectorView`1<Boolean>
  IVectorView_1__Boolean = interface;
  PIVectorView_1__Boolean = ^IVectorView_1__Boolean;

  // Windows.AI.MachineLearning.ITensorBoolean
  MachineLearning_ITensorBoolean = interface;
  PMachineLearning_ITensorBoolean = ^MachineLearning_ITensorBoolean;

  // Windows.AI.MachineLearning.ITensorBooleanStatics
  MachineLearning_ITensorBooleanStatics = interface;
  PMachineLearning_ITensorBooleanStatics = ^MachineLearning_ITensorBooleanStatics;

  // Windows.AI.MachineLearning.ITensorBooleanStatics2
  MachineLearning_ITensorBooleanStatics2 = interface;
  PMachineLearning_ITensorBooleanStatics2 = ^MachineLearning_ITensorBooleanStatics2;

  // Windows.Foundation.Collections.IIterator`1<Double>
  IIterator_1__Double = interface;
  PIIterator_1__Double = ^IIterator_1__Double;

  // Windows.Foundation.Collections.IIterable`1<Double>
  IIterable_1__Double = interface;
  PIIterable_1__Double = ^IIterable_1__Double;

  // Windows.Foundation.Collections.IVectorView`1<Double>
  IVectorView_1__Double = interface;
  PIVectorView_1__Double = ^IVectorView_1__Double;

  // Windows.AI.MachineLearning.ITensorDouble
  MachineLearning_ITensorDouble = interface;
  PMachineLearning_ITensorDouble = ^MachineLearning_ITensorDouble;

  // Windows.AI.MachineLearning.ITensorDoubleStatics
  MachineLearning_ITensorDoubleStatics = interface;
  PMachineLearning_ITensorDoubleStatics = ^MachineLearning_ITensorDoubleStatics;

  // Windows.AI.MachineLearning.ITensorDoubleStatics2
  MachineLearning_ITensorDoubleStatics2 = interface;
  PMachineLearning_ITensorDoubleStatics2 = ^MachineLearning_ITensorDoubleStatics2;

  // Windows.AI.MachineLearning.ITensorFeatureDescriptor
  MachineLearning_ITensorFeatureDescriptor = interface;
  PMachineLearning_ITensorFeatureDescriptor = ^MachineLearning_ITensorFeatureDescriptor;

  // Windows.Foundation.Collections.IIterable`1<Single>
  IIterable_1__Single = interface;
  PIIterable_1__Single = ^IIterable_1__Single;

  // Windows.AI.MachineLearning.ITensorFloat
  MachineLearning_ITensorFloat = interface;
  PMachineLearning_ITensorFloat = ^MachineLearning_ITensorFloat;

  // Windows.AI.MachineLearning.ITensorFloat16Bit
  MachineLearning_ITensorFloat16Bit = interface;
  PMachineLearning_ITensorFloat16Bit = ^MachineLearning_ITensorFloat16Bit;

  // Windows.AI.MachineLearning.ITensorFloat16BitStatics
  MachineLearning_ITensorFloat16BitStatics = interface;
  PMachineLearning_ITensorFloat16BitStatics = ^MachineLearning_ITensorFloat16BitStatics;

  // Windows.AI.MachineLearning.ITensorFloat16BitStatics2
  MachineLearning_ITensorFloat16BitStatics2 = interface;
  PMachineLearning_ITensorFloat16BitStatics2 = ^MachineLearning_ITensorFloat16BitStatics2;

  // Windows.AI.MachineLearning.ITensorFloatStatics
  MachineLearning_ITensorFloatStatics = interface;
  PMachineLearning_ITensorFloatStatics = ^MachineLearning_ITensorFloatStatics;

  // Windows.AI.MachineLearning.ITensorFloatStatics2
  MachineLearning_ITensorFloatStatics2 = interface;
  PMachineLearning_ITensorFloatStatics2 = ^MachineLearning_ITensorFloatStatics2;

  // Windows.Foundation.Collections.IIterator`1<Int16>
  IIterator_1__SmallInt = interface;
  PIIterator_1__SmallInt = ^IIterator_1__SmallInt;

  // Windows.Foundation.Collections.IIterable`1<Int16>
  IIterable_1__SmallInt = interface;
  PIIterable_1__SmallInt = ^IIterable_1__SmallInt;

  // Windows.Foundation.Collections.IVectorView`1<Int16>
  IVectorView_1__SmallInt = interface;
  PIVectorView_1__SmallInt = ^IVectorView_1__SmallInt;

  // Windows.AI.MachineLearning.ITensorInt16Bit
  MachineLearning_ITensorInt16Bit = interface;
  PMachineLearning_ITensorInt16Bit = ^MachineLearning_ITensorInt16Bit;

  // Windows.AI.MachineLearning.ITensorInt16BitStatics
  MachineLearning_ITensorInt16BitStatics = interface;
  PMachineLearning_ITensorInt16BitStatics = ^MachineLearning_ITensorInt16BitStatics;

  // Windows.AI.MachineLearning.ITensorInt16BitStatics2
  MachineLearning_ITensorInt16BitStatics2 = interface;
  PMachineLearning_ITensorInt16BitStatics2 = ^MachineLearning_ITensorInt16BitStatics2;

  // Windows.Foundation.Collections.IIterable`1<Int32>
  IIterable_1__Integer = interface;
  PIIterable_1__Integer = ^IIterable_1__Integer;

  // Windows.AI.MachineLearning.ITensorInt32Bit
  MachineLearning_ITensorInt32Bit = interface;
  PMachineLearning_ITensorInt32Bit = ^MachineLearning_ITensorInt32Bit;

  // Windows.AI.MachineLearning.ITensorInt32BitStatics
  MachineLearning_ITensorInt32BitStatics = interface;
  PMachineLearning_ITensorInt32BitStatics = ^MachineLearning_ITensorInt32BitStatics;

  // Windows.AI.MachineLearning.ITensorInt32BitStatics2
  MachineLearning_ITensorInt32BitStatics2 = interface;
  PMachineLearning_ITensorInt32BitStatics2 = ^MachineLearning_ITensorInt32BitStatics2;

  // Windows.AI.MachineLearning.ITensorInt64Bit
  MachineLearning_ITensorInt64Bit = interface;
  PMachineLearning_ITensorInt64Bit = ^MachineLearning_ITensorInt64Bit;

  // Windows.AI.MachineLearning.ITensorInt64BitStatics
  MachineLearning_ITensorInt64BitStatics = interface;
  PMachineLearning_ITensorInt64BitStatics = ^MachineLearning_ITensorInt64BitStatics;

  // Windows.AI.MachineLearning.ITensorInt64BitStatics2
  MachineLearning_ITensorInt64BitStatics2 = interface;
  PMachineLearning_ITensorInt64BitStatics2 = ^MachineLearning_ITensorInt64BitStatics2;

  // Windows.Foundation.Collections.IIterable`1<UInt8>
  IIterable_1__Byte = interface;
  PIIterable_1__Byte = ^IIterable_1__Byte;

  // Windows.Foundation.Collections.IVectorView`1<UInt8>
  IVectorView_1__Byte = interface;
  PIVectorView_1__Byte = ^IVectorView_1__Byte;

  // Windows.AI.MachineLearning.ITensorInt8Bit
  MachineLearning_ITensorInt8Bit = interface;
  PMachineLearning_ITensorInt8Bit = ^MachineLearning_ITensorInt8Bit;

  // Windows.AI.MachineLearning.ITensorInt8BitStatics
  MachineLearning_ITensorInt8BitStatics = interface;
  PMachineLearning_ITensorInt8BitStatics = ^MachineLearning_ITensorInt8BitStatics;

  // Windows.AI.MachineLearning.ITensorInt8BitStatics2
  MachineLearning_ITensorInt8BitStatics2 = interface;
  PMachineLearning_ITensorInt8BitStatics2 = ^MachineLearning_ITensorInt8BitStatics2;

  // Windows.Foundation.Collections.IIterator`1<String>
  IIterator_1__HSTRING = interface;
  PIIterator_1__HSTRING = ^IIterator_1__HSTRING;

  // Windows.Foundation.Collections.IIterable`1<String>
  IIterable_1__HSTRING = interface;
  PIIterable_1__HSTRING = ^IIterable_1__HSTRING;

  // Windows.Foundation.Collections.IVectorView`1<String>
  IVectorView_1__HSTRING = interface;
  PIVectorView_1__HSTRING = ^IVectorView_1__HSTRING;

  // Windows.AI.MachineLearning.ITensorString
  MachineLearning_ITensorString = interface;
  PMachineLearning_ITensorString = ^MachineLearning_ITensorString;

  // Windows.AI.MachineLearning.ITensorStringStatics
  MachineLearning_ITensorStringStatics = interface;
  PMachineLearning_ITensorStringStatics = ^MachineLearning_ITensorStringStatics;

  // Windows.AI.MachineLearning.ITensorStringStatics2
  MachineLearning_ITensorStringStatics2 = interface;
  PMachineLearning_ITensorStringStatics2 = ^MachineLearning_ITensorStringStatics2;

  // Windows.Foundation.Collections.IIterator`1<UInt16>
  IIterator_1__Word = interface;
  PIIterator_1__Word = ^IIterator_1__Word;

  // Windows.Foundation.Collections.IIterable`1<UInt16>
  IIterable_1__Word = interface;
  PIIterable_1__Word = ^IIterable_1__Word;

  // Windows.Foundation.Collections.IVectorView`1<UInt16>
  IVectorView_1__Word = interface;
  PIVectorView_1__Word = ^IVectorView_1__Word;

  // Windows.AI.MachineLearning.ITensorUInt16Bit
  MachineLearning_ITensorUInt16Bit = interface;
  PMachineLearning_ITensorUInt16Bit = ^MachineLearning_ITensorUInt16Bit;

  // Windows.AI.MachineLearning.ITensorUInt16BitStatics
  MachineLearning_ITensorUInt16BitStatics = interface;
  PMachineLearning_ITensorUInt16BitStatics = ^MachineLearning_ITensorUInt16BitStatics;

  // Windows.AI.MachineLearning.ITensorUInt16BitStatics2
  MachineLearning_ITensorUInt16BitStatics2 = interface;
  PMachineLearning_ITensorUInt16BitStatics2 = ^MachineLearning_ITensorUInt16BitStatics2;

  // Windows.AI.MachineLearning.ITensorUInt32Bit
  MachineLearning_ITensorUInt32Bit = interface;
  PMachineLearning_ITensorUInt32Bit = ^MachineLearning_ITensorUInt32Bit;

  // Windows.AI.MachineLearning.ITensorUInt32BitStatics
  MachineLearning_ITensorUInt32BitStatics = interface;
  PMachineLearning_ITensorUInt32BitStatics = ^MachineLearning_ITensorUInt32BitStatics;

  // Windows.AI.MachineLearning.ITensorUInt32BitStatics2
  MachineLearning_ITensorUInt32BitStatics2 = interface;
  PMachineLearning_ITensorUInt32BitStatics2 = ^MachineLearning_ITensorUInt32BitStatics2;

  // Windows.Foundation.Collections.IIterator`1<UInt64>
  IIterator_1__UInt64 = interface;
  PIIterator_1__UInt64 = ^IIterator_1__UInt64;

  // Windows.Foundation.Collections.IIterable`1<UInt64>
  IIterable_1__UInt64 = interface;
  PIIterable_1__UInt64 = ^IIterable_1__UInt64;

  // Windows.Foundation.Collections.IVectorView`1<UInt64>
  IVectorView_1__UInt64 = interface;
  PIVectorView_1__UInt64 = ^IVectorView_1__UInt64;

  // Windows.AI.MachineLearning.ITensorUInt64Bit
  MachineLearning_ITensorUInt64Bit = interface;
  PMachineLearning_ITensorUInt64Bit = ^MachineLearning_ITensorUInt64Bit;

  // Windows.AI.MachineLearning.ITensorUInt64BitStatics
  MachineLearning_ITensorUInt64BitStatics = interface;
  PMachineLearning_ITensorUInt64BitStatics = ^MachineLearning_ITensorUInt64BitStatics;

  // Windows.AI.MachineLearning.ITensorUInt64BitStatics2
  MachineLearning_ITensorUInt64BitStatics2 = interface;
  PMachineLearning_ITensorUInt64BitStatics2 = ^MachineLearning_ITensorUInt64BitStatics2;

  // Windows.AI.MachineLearning.ITensorUInt8Bit
  MachineLearning_ITensorUInt8Bit = interface;
  PMachineLearning_ITensorUInt8Bit = ^MachineLearning_ITensorUInt8Bit;

  // Windows.AI.MachineLearning.ITensorUInt8BitStatics
  MachineLearning_ITensorUInt8BitStatics = interface;
  PMachineLearning_ITensorUInt8BitStatics = ^MachineLearning_ITensorUInt8BitStatics;

  // Windows.AI.MachineLearning.ITensorUInt8BitStatics2
  MachineLearning_ITensorUInt8BitStatics2 = interface;
  PMachineLearning_ITensorUInt8BitStatics2 = ^MachineLearning_ITensorUInt8BitStatics2;

  // Windows.AI Enums

  // Windows.AI.MachineLearning.LearningModelDeviceKind
  MachineLearning_LearningModelDeviceKind = (
    Default = 0,
    Cpu = 1,
    DirectX = 2,
    DirectXHighPerformance = 3,
    DirectXMinPower = 4
  );
  PMachineLearning_LearningModelDeviceKind = ^MachineLearning_LearningModelDeviceKind;

  // Windows.AI.MachineLearning.LearningModelFeatureKind
  MachineLearning_LearningModelFeatureKind = (
    Tensor = 0,
    Sequence = 1,
    Map = 2,
    Image = 3
  );
  PMachineLearning_LearningModelFeatureKind = ^MachineLearning_LearningModelFeatureKind;

  // Windows.AI.MachineLearning.Preview.FeatureElementKindPreview
  MachineLearning_Preview_FeatureElementKindPreview = (
    Undefined = 0,
    Float = 1,
    UInt8 = 2,
    Int8 = 3,
    UInt16 = 4,
    Int16 = 5,
    Int32 = 6,
    Int64 = 7,
    &String = 8,
    Boolean = 9,
    Float16 = 10,
    Double = 11,
    UInt32 = 12,
    UInt64 = 13,
    Complex64 = 14,
    Complex128 = 15
  );
  PMachineLearning_Preview_FeatureElementKindPreview = ^MachineLearning_Preview_FeatureElementKindPreview;

  // Windows.AI.MachineLearning.Preview.LearningModelDeviceKindPreview
  MachineLearning_Preview_LearningModelDeviceKindPreview = (
    LearningDeviceAny = 0,
    LearningDeviceCpu = 1,
    LearningDeviceGpu = 2,
    LearningDeviceNpu = 3,
    LearningDeviceDsp = 4,
    LearningDeviceFpga = 5
  );
  PMachineLearning_Preview_LearningModelDeviceKindPreview = ^MachineLearning_Preview_LearningModelDeviceKindPreview;

  // Windows.AI.MachineLearning.Preview.LearningModelFeatureKindPreview
  MachineLearning_Preview_LearningModelFeatureKindPreview = (
    Undefined = 0,
    Tensor = 1,
    Sequence = 2,
    Map = 3,
    Image = 4
  );
  PMachineLearning_Preview_LearningModelFeatureKindPreview = ^MachineLearning_Preview_LearningModelFeatureKindPreview;

  // Windows.AI.MachineLearning.TensorKind
  MachineLearning_TensorKind = (
    Undefined = 0,
    Float = 1,
    UInt8 = 2,
    Int8 = 3,
    UInt16 = 4,
    Int16 = 5,
    Int32 = 6,
    Int64 = 7,
    &String = 8,
    Boolean = 9,
    Float16 = 10,
    Double = 11,
    UInt32 = 12,
    UInt64 = 13,
    Complex64 = 14,
    Complex128 = 15
  );
  PMachineLearning_TensorKind = ^MachineLearning_TensorKind;

  // Windows.AI Records
  // Windows.AI.MachineLearning.MachineLearningContract
  MachineLearning_MachineLearningContract = record
  end;
  PMachineLearning_MachineLearningContract = ^MachineLearning_MachineLearningContract;

  // Windows.AI.MachineLearning.Preview.MachineLearningPreviewContract
  MachineLearning_Preview_MachineLearningPreviewContract = record
  end;
  PMachineLearning_Preview_MachineLearningPreviewContract = ^MachineLearning_Preview_MachineLearningPreviewContract;

  // Windows.AI Interfaces

  // Windows.AI.MachineLearning.IImageFeatureDescriptor
  MachineLearning_IImageFeatureDescriptor = interface(IInspectable)
  ['{365585A5-171A-4A2A-985F-265159D3895A}']
    function get_BitmapPixelFormat: Imaging_BitmapPixelFormat; safecall;
    function get_BitmapAlphaMode: Imaging_BitmapAlphaMode; safecall;
    function get_Width: Cardinal; safecall;
    function get_Height: Cardinal; safecall;
    property BitmapAlphaMode: Imaging_BitmapAlphaMode read get_BitmapAlphaMode;
    property BitmapPixelFormat: Imaging_BitmapPixelFormat read get_BitmapPixelFormat;
    property Height: Cardinal read get_Height;
    property Width: Cardinal read get_Width;
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

  // Windows.Foundation.TypedEventHandler`2<Windows.Foundation.IMemoryBufferReference,Object>
  TypedEventHandler_2__IMemoryBufferReference__IInspectable = interface(IUnknown)
  ['{F4637D4A-0760-5431-BFC0-24EB1D4F6C4F}']
    procedure Invoke(sender: IMemoryBufferReference; args: IInspectable); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.IImageFeatureValue
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_ImageFeatureValue)]
  MachineLearning_IImageFeatureValue = interface(IInspectable)
  ['{F0414FD9-C9AA-4405-B7FB-94F87C8A3037}']
    function get_VideoFrame: IVideoFrame; safecall;
    property VideoFrame: IVideoFrame read get_VideoFrame;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.IImageFeatureValueStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_ImageFeatureValue)]
  MachineLearning_IImageFeatureValueStatics = interface(IInspectable)
  ['{1BC317FD-23CB-4610-B085-C8E1C87EBAA0}']
    function CreateFromVideoFrame(image: IVideoFrame): MachineLearning_IImageFeatureValue; safecall;
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

  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelFeatureDescriptor
  MachineLearning_ILearningModelFeatureDescriptor = interface(IInspectable)
  ['{BC08CF7C-6ED0-4004-97BA-B9A2EECD2B4F}']
    function get_Name: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_Kind: MachineLearning_LearningModelFeatureKind; safecall;
    function get_IsRequired: Boolean; safecall;
    property Description: HSTRING read get_Description;
    property IsRequired: Boolean read get_IsRequired;
    property Kind: MachineLearning_LearningModelFeatureKind read get_Kind;
    property Name: HSTRING read get_Name;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.AI.MachineLearning.ILearningModelFeatureDescriptor>
  IIterator_1__MachineLearning_ILearningModelFeatureDescriptor = interface(IInspectable)
  ['{0EF412A8-A1E6-593A-97F2-0D699CA6A567}']
    function get_Current: MachineLearning_ILearningModelFeatureDescriptor; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PMachineLearning_ILearningModelFeatureDescriptor): Cardinal; safecall;
    property Current: MachineLearning_ILearningModelFeatureDescriptor read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.AI.MachineLearning.ILearningModelFeatureDescriptor>
  IIterable_1__MachineLearning_ILearningModelFeatureDescriptor = interface(IInspectable)
  ['{0FA50877-6792-56B7-AF46-430A8901894A}']
    function First: IIterator_1__MachineLearning_ILearningModelFeatureDescriptor; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.AI.MachineLearning.ILearningModelFeatureDescriptor>
  IVectorView_1__MachineLearning_ILearningModelFeatureDescriptor = interface(IInspectable)
  ['{53E53120-A6E1-527F-AF8A-C812902E175E}']
    function GetAt(index: Cardinal): MachineLearning_ILearningModelFeatureDescriptor; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: MachineLearning_ILearningModelFeatureDescriptor; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PMachineLearning_ILearningModelFeatureDescriptor): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModel
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModel)]
  MachineLearning_ILearningModel = interface(IInspectable)
  ['{5B8E4920-489F-4E86-9128-265A327B78FA}']
    function get_Author: HSTRING; safecall;
    function get_Name: HSTRING; safecall;
    function get_Domain: HSTRING; safecall;
    function get_Description: HSTRING; safecall;
    function get_Version: Int64; safecall;
    function get_Metadata: IMapView_2__HSTRING__HSTRING; safecall;
    function get_InputFeatures: IVectorView_1__MachineLearning_ILearningModelFeatureDescriptor; safecall;
    function get_OutputFeatures: IVectorView_1__MachineLearning_ILearningModelFeatureDescriptor; safecall;
    property Author: HSTRING read get_Author;
    property Description: HSTRING read get_Description;
    property Domain: HSTRING read get_Domain;
    property InputFeatures: IVectorView_1__MachineLearning_ILearningModelFeatureDescriptor read get_InputFeatures;
    property Metadata: IMapView_2__HSTRING__HSTRING read get_Metadata;
    property Name: HSTRING read get_Name;
    property OutputFeatures: IVectorView_1__MachineLearning_ILearningModelFeatureDescriptor read get_OutputFeatures;
    property Version: Int64 read get_Version;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelBinding
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelBinding)]
  MachineLearning_ILearningModelBinding = interface(IInspectable)
  ['{EA312F20-168F-4F8C-94FE-2E7AC31B4AA8}']
    procedure Bind(name: HSTRING; value: IInspectable); overload; safecall;
    procedure Bind(name: HSTRING; value: IInspectable; props: IPropertySet); overload; safecall;
    procedure Clear; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelDevice
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelDevice)]
  MachineLearning_ILearningModelDevice = interface(IInspectable)
  ['{F5C2C8FE-3F56-4A8C-AC5F-FDB92D8B8252}']
    function get_AdapterId: DisplayAdapterId; safecall;
    function get_Direct3D11Device: DirectX_Direct3D11_IDirect3DDevice; safecall;
    property AdapterId: DisplayAdapterId read get_AdapterId;
    property Direct3D11Device: DirectX_Direct3D11_IDirect3DDevice read get_Direct3D11Device;
  end;

  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelEvaluationResult
  MachineLearning_ILearningModelEvaluationResult = interface(IInspectable)
  ['{B2F9BFCD-960E-49C0-8593-EB190AE3EEE2}']
    function get_CorrelationId: HSTRING; safecall;
    function get_ErrorStatus: Integer; safecall;
    function get_Succeeded: Boolean; safecall;
    function get_Outputs: IMapView_2__HSTRING__IInspectable; safecall;
    property CorrelationId: HSTRING read get_CorrelationId;
    property ErrorStatus: Integer read get_ErrorStatus;
    property Outputs: IMapView_2__HSTRING__IInspectable read get_Outputs;
    property Succeeded: Boolean read get_Succeeded;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.AI.MachineLearning.ILearningModelEvaluationResult>
  AsyncOperationCompletedHandler_1__MachineLearning_ILearningModelEvaluationResult = interface(IUnknown)
  ['{24706802-4FF0-5E0F-8B75-1C58935E4D57}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__MachineLearning_ILearningModelEvaluationResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.AI.MachineLearning.ILearningModelEvaluationResult>
  IAsyncOperation_1__MachineLearning_ILearningModelEvaluationResult = interface(IInspectable)
  ['{E0C16162-C8D8-5F87-B52B-1CE9BA76EB57}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__MachineLearning_ILearningModelEvaluationResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__MachineLearning_ILearningModelEvaluationResult; safecall;
    function GetResults: MachineLearning_ILearningModelEvaluationResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__MachineLearning_ILearningModelEvaluationResult read get_Completed write put_Completed;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelSession
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelSession)]
  MachineLearning_ILearningModelSession = interface(IInspectable)
  ['{8E58F8F6-B787-4C11-90F0-7129AECA74A9}']
    function get_Model: MachineLearning_ILearningModel; safecall;
    function get_Device: MachineLearning_ILearningModelDevice; safecall;
    function get_EvaluationProperties: IPropertySet; safecall;
    function EvaluateAsync(bindings: MachineLearning_ILearningModelBinding; correlationId: HSTRING): IAsyncOperation_1__MachineLearning_ILearningModelEvaluationResult; safecall;
    function EvaluateFeaturesAsync(features: IMap_2__HSTRING__IInspectable; correlationId: HSTRING): IAsyncOperation_1__MachineLearning_ILearningModelEvaluationResult; safecall;
    function Evaluate(bindings: MachineLearning_ILearningModelBinding; correlationId: HSTRING): MachineLearning_ILearningModelEvaluationResult; safecall;
    function EvaluateFeatures(features: IMap_2__HSTRING__IInspectable; correlationId: HSTRING): MachineLearning_ILearningModelEvaluationResult; safecall;
    property Device: MachineLearning_ILearningModelDevice read get_Device;
    property EvaluationProperties: IPropertySet read get_EvaluationProperties;
    property Model: MachineLearning_ILearningModel read get_Model;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelBindingFactory
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelBinding)]
  MachineLearning_ILearningModelBindingFactory = interface(IInspectable)
  ['{C95F7A7A-E788-475E-8917-23AA381FAF0B}']
    function CreateFromSession(session: MachineLearning_ILearningModelSession): MachineLearning_ILearningModelBinding; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelDeviceFactory
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelDevice)]
  MachineLearning_ILearningModelDeviceFactory = interface(IInspectable)
  ['{9CFFD74D-B1E5-4F20-80AD-0A56690DB06B}']
    function Create(deviceKind: MachineLearning_LearningModelDeviceKind): MachineLearning_ILearningModelDevice; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelDeviceStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelDevice)]
  MachineLearning_ILearningModelDeviceStatics = interface(IInspectable)
  ['{49F32107-A8BF-42BB-92C7-10B12DC5D21F}']
    function CreateFromDirect3D11Device(device: DirectX_Direct3D11_IDirect3DDevice): MachineLearning_ILearningModelDevice; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelFeatureValue
  MachineLearning_ILearningModelFeatureValue = interface(IInspectable)
  ['{F51005DB-4085-4DFE-9FED-95EB0C0CF75C}']
    function get_Kind: MachineLearning_LearningModelFeatureKind; safecall;
    property Kind: MachineLearning_LearningModelFeatureKind read get_Kind;
  end;

  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelOperatorProvider
  MachineLearning_ILearningModelOperatorProvider = interface(IInspectable)
  ['{2A222E5D-AFB1-47ED-BFAD-B5B3A459EC04}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelSessionFactory
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelSession)]
  MachineLearning_ILearningModelSessionFactory = interface(IInspectable)
  ['{0F6B881D-1C9B-47B6-BFE0-F1CF62A67579}']
    function CreateFromModel(model: MachineLearning_ILearningModel): MachineLearning_ILearningModelSession; safecall;
    function CreateFromModelOnDevice(model: MachineLearning_ILearningModel; deviceToRunOn: MachineLearning_ILearningModelDevice): MachineLearning_ILearningModelSession; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelSessionOptions
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelSessionOptions)]
  MachineLearning_ILearningModelSessionOptions = interface(IInspectable)
  ['{B8F63FA1-134D-5133-8CFF-3A5C3C263BEB}']
    function get_BatchSizeOverride: Cardinal; safecall;
    procedure put_BatchSizeOverride(value: Cardinal); safecall;
    property BatchSizeOverride: Cardinal read get_BatchSizeOverride write put_BatchSizeOverride;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelSessionFactory2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModelSession)]
  MachineLearning_ILearningModelSessionFactory2 = interface(IInspectable)
  ['{4E5C88BF-0A1F-5FEC-ADE0-2FD91E4EF29B}']
    function CreateFromModelOnDeviceWithSessionOptions(model: MachineLearning_ILearningModel; deviceToRunOn: MachineLearning_ILearningModelDevice; learningModelSessionOptions: MachineLearning_ILearningModelSessionOptions): MachineLearning_ILearningModelSession; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelSessionOptions2
  MachineLearning_ILearningModelSessionOptions2 = interface(IInspectable)
  ['{6FCD1DC4-175F-5BD2-8DE5-2F2006A25ADF}']
    function get_CloseModelOnSessionCreation: Boolean; safecall;
    procedure put_CloseModelOnSessionCreation(value: Boolean); safecall;
    property CloseModelOnSessionCreation: Boolean read get_CloseModelOnSessionCreation write put_CloseModelOnSessionCreation;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.AI.MachineLearning.ILearningModel>
  AsyncOperationCompletedHandler_1__MachineLearning_ILearningModel = interface(IUnknown)
  ['{0AF09637-D1B5-5CDE-863C-933F647DF151}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__MachineLearning_ILearningModel; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.AI.MachineLearning.ILearningModel>
  IAsyncOperation_1__MachineLearning_ILearningModel = interface(IInspectable)
  ['{B23C1AA2-62B8-56AD-B968-58CBB138555A}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__MachineLearning_ILearningModel); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__MachineLearning_ILearningModel; safecall;
    function GetResults: MachineLearning_ILearningModel; safecall;
    property Completed: AsyncOperationCompletedHandler_1__MachineLearning_ILearningModel read get_Completed write put_Completed;
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ILearningModelStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_LearningModel)]
  MachineLearning_ILearningModelStatics = interface(IInspectable)
  ['{E3B977E8-6952-4E47-8EF4-1F7F07897C6D}']
    function LoadFromStorageFileAsync(modelFile: IStorageFile): IAsyncOperation_1__MachineLearning_ILearningModel; overload; safecall;
    function LoadFromStreamAsync(modelStream: IRandomAccessStreamReference): IAsyncOperation_1__MachineLearning_ILearningModel; overload; safecall;
    function LoadFromFilePath(filePath: HSTRING): MachineLearning_ILearningModel; overload; safecall;
    function LoadFromStream(modelStream: IRandomAccessStreamReference): MachineLearning_ILearningModel; overload; safecall;
    function LoadFromStorageFileAsync(modelFile: IStorageFile; operatorProvider: MachineLearning_ILearningModelOperatorProvider): IAsyncOperation_1__MachineLearning_ILearningModel; overload; safecall;
    function LoadFromStreamAsync(modelStream: IRandomAccessStreamReference; operatorProvider: MachineLearning_ILearningModelOperatorProvider): IAsyncOperation_1__MachineLearning_ILearningModel; overload; safecall;
    function LoadFromFilePath(filePath: HSTRING; operatorProvider: MachineLearning_ILearningModelOperatorProvider): MachineLearning_ILearningModel; overload; safecall;
    function LoadFromStream(modelStream: IRandomAccessStreamReference; operatorProvider: MachineLearning_ILearningModelOperatorProvider): MachineLearning_ILearningModel; overload; safecall;
  end;

  // Windows.AI.MachineLearning.IMapFeatureDescriptor
  MachineLearning_IMapFeatureDescriptor = interface(IInspectable)
  ['{530424BD-A257-436D-9E60-C2981F7CC5C4}']
    function get_KeyKind: MachineLearning_TensorKind; safecall;
    function get_ValueDescriptor: MachineLearning_ILearningModelFeatureDescriptor; safecall;
    property KeyKind: MachineLearning_TensorKind read get_KeyKind;
    property ValueDescriptor: MachineLearning_ILearningModelFeatureDescriptor read get_ValueDescriptor;
  end;

  // Windows.AI.MachineLearning.ISequenceFeatureDescriptor
  MachineLearning_ISequenceFeatureDescriptor = interface(IInspectable)
  ['{84F6945A-562B-4D62-A851-739ACED96668}']
    function get_ElementDescriptor: MachineLearning_ILearningModelFeatureDescriptor; safecall;
    property ElementDescriptor: MachineLearning_ILearningModelFeatureDescriptor read get_ElementDescriptor;
  end;

  // Windows.Foundation.Collections.IIterator`1<Int64>
  IIterator_1__Int64 = interface(IInspectable)
  ['{FB98034C-86B7-581F-8CD9-5AD0692201A9}']
    function get_Current: Int64; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PInt64): Cardinal; safecall;
    property Current: Int64 read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Int64>
  IIterable_1__Int64 = interface(IInspectable)
  ['{7784427E-F9CC-518D-964B-E50D5CE727F1}']
    function First: IIterator_1__Int64; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Int64>
  IVectorView_1__Int64 = interface(IInspectable)
  ['{8221AA0E-D1D2-5B22-A918-05672812D12F}']
    function GetAt(index: Cardinal): Int64; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Int64; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PInt64): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensor
  MachineLearning_ITensor = interface(IInspectable)
  ['{05489593-A305-4A25-AD09-440119B4B7F6}']
    function get_TensorKind: MachineLearning_TensorKind; safecall;
    function get_Shape: IVectorView_1__Int64; safecall;
    property Shape: IVectorView_1__Int64 read get_Shape;
    property TensorKind: MachineLearning_TensorKind read get_TensorKind;
  end;

  // Windows.Foundation.Collections.IIterator`1<Boolean>
  IIterator_1__Boolean = interface(IInspectable)
  ['{740A0296-A535-572A-BF0B-17C18FF71FE6}']
    function get_Current: Boolean; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PBoolean): Cardinal; safecall;
    property Current: Boolean read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Boolean>
  IIterable_1__Boolean = interface(IInspectable)
  ['{30160817-1D7D-54E9-99DB-D7636266A476}']
    function First: IIterator_1__Boolean; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Boolean>
  IVectorView_1__Boolean = interface(IInspectable)
  ['{243A09CB-6F40-56AF-A442-FE81431FBEF5}']
    function GetAt(index: Cardinal): Boolean; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Boolean; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PBoolean): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorBoolean
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorBoolean)]
  MachineLearning_ITensorBoolean = interface(IInspectable)
  ['{50F311ED-29E9-4A5C-A44D-8FC512584EED}']
    function GetAsVectorView: IVectorView_1__Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorBooleanStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorBoolean)]
  MachineLearning_ITensorBooleanStatics = interface(IInspectable)
  ['{2796862C-2357-49A7-B476-D0AA3DFE6866}']
    function Create: MachineLearning_ITensorBoolean; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorBoolean; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PBoolean): MachineLearning_ITensorBoolean; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Boolean): MachineLearning_ITensorBoolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorBooleanStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorBoolean)]
  MachineLearning_ITensorBooleanStatics2 = interface(IInspectable)
  ['{A3A4A501-6A2D-52D7-B04B-C435BAEE0115}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PBoolean): MachineLearning_ITensorBoolean; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorBoolean; safecall;
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorDouble
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorDouble)]
  MachineLearning_ITensorDouble = interface(IInspectable)
  ['{91E41252-7A8F-4F0E-A28F-9637FFC8A3D0}']
    function GetAsVectorView: IVectorView_1__Double; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorDoubleStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorDouble)]
  MachineLearning_ITensorDoubleStatics = interface(IInspectable)
  ['{A86693C5-9538-44E7-A3CA-5DF374A5A70C}']
    function Create: MachineLearning_ITensorDouble; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorDouble; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PDouble): MachineLearning_ITensorDouble; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Double): MachineLearning_ITensorDouble; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorDoubleStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorDouble)]
  MachineLearning_ITensorDoubleStatics2 = interface(IInspectable)
  ['{93A570DE-5E9A-5094-85C8-592C655E68AC}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PDouble): MachineLearning_ITensorDouble; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorDouble; safecall;
  end;

  // Windows.AI.MachineLearning.ITensorFeatureDescriptor
  MachineLearning_ITensorFeatureDescriptor = interface(IInspectable)
  ['{74455C80-946A-4310-A19C-EE0AF028FCE4}']
    function get_TensorKind: MachineLearning_TensorKind; safecall;
    function get_Shape: IVectorView_1__Int64; safecall;
    property Shape: IVectorView_1__Int64 read get_Shape;
    property TensorKind: MachineLearning_TensorKind read get_TensorKind;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Single>
  IIterable_1__Single_Base = interface(IInspectable)
  ['{B01BEE51-063A-5FDA-BD72-D76637BB8CB8}']
    function First: IIterator_1__Single; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Single>
  IIterable_1__Single = interface(IIterable_1__Single_Base)
  ['{B01BEE51-063A-5FDA-BD72-D76637BB8CB8}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorFloat
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorFloat)]
  MachineLearning_ITensorFloat = interface(IInspectable)
  ['{F2282D82-AA02-42C8-A0C8-DF1EFC9676E1}']
    function GetAsVectorView: IVectorView_1__Single; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorFloat16Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorFloat16Bit)]
  MachineLearning_ITensorFloat16Bit = interface(IInspectable)
  ['{0AB994FC-5B89-4C3C-B5E4-5282A5316C0A}']
    function GetAsVectorView: IVectorView_1__Single; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorFloat16BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorFloat16Bit)]
  MachineLearning_ITensorFloat16BitStatics = interface(IInspectable)
  ['{A52DB6F5-318A-44D4-820B-0CDC7054A84A}']
    function Create: MachineLearning_ITensorFloat16Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorFloat16Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat16Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Single): MachineLearning_ITensorFloat16Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorFloat16BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorFloat16Bit)]
  MachineLearning_ITensorFloat16BitStatics2 = interface(IInspectable)
  ['{68545726-2DC7-51BF-B470-0B344CC2A1BC}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat16Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorFloat16Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorFloatStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorFloat)]
  MachineLearning_ITensorFloatStatics = interface(IInspectable)
  ['{DBCD395B-3BA3-452F-B10D-3C135E573FA9}']
    function Create: MachineLearning_ITensorFloat; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorFloat; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Single): MachineLearning_ITensorFloat; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorFloatStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorFloat)]
  MachineLearning_ITensorFloatStatics2 = interface(IInspectable)
  ['{24610BC1-5E44-5713-B281-8F4AD4D555E8}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorFloat; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<Int16>
  IIterator_1__SmallInt = interface(IInspectable)
  ['{5409069F-E7C1-5732-BB69-E5736F03F9A9}']
    function get_Current: SmallInt; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PSmallInt): Cardinal; safecall;
    property Current: SmallInt read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Int16>
  IIterable_1__SmallInt = interface(IInspectable)
  ['{72FF2923-4B4E-53BB-8FEB-41EC5F2BB734}']
    function First: IIterator_1__SmallInt; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<Int16>
  IVectorView_1__SmallInt = interface(IInspectable)
  ['{E53056AD-8A0E-5C41-A62D-C92E3AC2DE58}']
    function GetAt(index: Cardinal): SmallInt; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: SmallInt; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PSmallInt): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt16Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt16Bit)]
  MachineLearning_ITensorInt16Bit = interface(IInspectable)
  ['{98A32D39-E6D6-44AF-8AFA-BAEBC44DC020}']
    function GetAsVectorView: IVectorView_1__SmallInt; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt16BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt16Bit)]
  MachineLearning_ITensorInt16BitStatics = interface(IInspectable)
  ['{98646293-266E-4B1A-821F-E60D70898B91}']
    function Create: MachineLearning_ITensorInt16Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt16Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSmallInt): MachineLearning_ITensorInt16Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__SmallInt): MachineLearning_ITensorInt16Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt16BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt16Bit)]
  MachineLearning_ITensorInt16BitStatics2 = interface(IInspectable)
  ['{0CD70CF4-696C-5E5F-95D8-5EBF9670148B}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSmallInt): MachineLearning_ITensorInt16Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt16Bit; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Int32>
  IIterable_1__Integer_Base = interface(IInspectable)
  ['{81A643FB-F51C-5565-83C4-F96425777B66}']
    function First: IIterator_1__Integer; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Int32>
  IIterable_1__Integer = interface(IIterable_1__Integer_Base)
  ['{81A643FB-F51C-5565-83C4-F96425777B66}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt32Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt32Bit)]
  MachineLearning_ITensorInt32Bit = interface(IInspectable)
  ['{2C0C28D3-207C-4486-A7D2-884522C5E589}']
    function GetAsVectorView: IVectorView_1__Integer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt32BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt32Bit)]
  MachineLearning_ITensorInt32BitStatics = interface(IInspectable)
  ['{6539864B-52FA-4E35-907C-834CAC417B50}']
    function Create: MachineLearning_ITensorInt32Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt32Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PInteger): MachineLearning_ITensorInt32Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Integer): MachineLearning_ITensorInt32Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt32BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt32Bit)]
  MachineLearning_ITensorInt32BitStatics2 = interface(IInspectable)
  ['{7C4B079A-E956-5CE0-A3BD-157D9D79B5EC}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PInteger): MachineLearning_ITensorInt32Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt32Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt64Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt64Bit)]
  MachineLearning_ITensorInt64Bit = interface(IInspectable)
  ['{499665BA-1FA2-45AD-AF25-A0BD9BDA4C87}']
    function GetAsVectorView: IVectorView_1__Int64; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt64BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt64Bit)]
  MachineLearning_ITensorInt64BitStatics = interface(IInspectable)
  ['{9648AD9D-1198-4D74-9517-783AB62B9CC2}']
    function Create: MachineLearning_ITensorInt64Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt64Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PInt64): MachineLearning_ITensorInt64Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Int64): MachineLearning_ITensorInt64Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt64BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt64Bit)]
  MachineLearning_ITensorInt64BitStatics2 = interface(IInspectable)
  ['{6D3D9DCB-FF40-5EC2-89FE-084E2B6BC6DB}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PInt64): MachineLearning_ITensorInt64Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt64Bit; safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<UInt8>
  IIterable_1__Byte_Base = interface(IInspectable)
  ['{88318266-F3FD-50FC-8F08-B823A41B60C1}']
    function First: IIterator_1__Byte; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<UInt8>
  IIterable_1__Byte = interface(IIterable_1__Byte_Base)
  ['{88318266-F3FD-50FC-8F08-B823A41B60C1}']
  end;

  // Windows.Foundation.Collections.IVectorView`1<UInt8>
  IVectorView_1__Byte = interface(IInspectable)
  ['{6D05FB29-7885-544E-9382-A1AD391A3FA4}']
    function GetAt(index: Cardinal): Byte; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Byte; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PByte): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt8Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt8Bit)]
  MachineLearning_ITensorInt8Bit = interface(IInspectable)
  ['{CDDD97C5-FFD8-4FEF-AEFB-30E1A485B2EE}']
    function GetAsVectorView: IVectorView_1__Byte; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt8BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt8Bit)]
  MachineLearning_ITensorInt8BitStatics = interface(IInspectable)
  ['{B1A12284-095C-4C76-A661-AC4CEE1F3E8B}']
    function Create: MachineLearning_ITensorInt8Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt8Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorInt8Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Byte): MachineLearning_ITensorInt8Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorInt8BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorInt8Bit)]
  MachineLearning_ITensorInt8BitStatics2 = interface(IInspectable)
  ['{C0D59637-C468-56FB-9535-C052BDB93DC0}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorInt8Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt8Bit; safecall;
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

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorString
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorString)]
  MachineLearning_ITensorString = interface(IInspectable)
  ['{582335C8-BDB1-4610-BC75-35E9CBF009B7}']
    function GetAsVectorView: IVectorView_1__HSTRING; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorStringStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorString)]
  MachineLearning_ITensorStringStatics = interface(IInspectable)
  ['{83623324-CF26-4F17-A2D4-20EF8D097D53}']
    function Create: MachineLearning_ITensorString; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorString; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PHSTRING): MachineLearning_ITensorString; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__HSTRING): MachineLearning_ITensorString; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorStringStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorString)]
  MachineLearning_ITensorStringStatics2 = interface(IInspectable)
  ['{9E355ED0-C8E2-5254-9137-0193A3668FD8}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PHSTRING): MachineLearning_ITensorString; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<UInt16>
  IIterator_1__Word = interface(IInspectable)
  ['{5738FC25-402B-5FC1-B1E4-0AA24EF652F1}']
    function get_Current: Word; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PWord): Cardinal; safecall;
    property Current: Word read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<UInt16>
  IIterable_1__Word = interface(IInspectable)
  ['{ECFA9A6F-FA2E-5345-B297-EFB4E8C6BE87}']
    function First: IIterator_1__Word; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<UInt16>
  IVectorView_1__Word = interface(IInspectable)
  ['{9D0D0D9F-6A82-55A3-98C5-228499DF38F9}']
    function GetAt(index: Cardinal): Word; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: Word; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PWord): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt16Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt16Bit)]
  MachineLearning_ITensorUInt16Bit = interface(IInspectable)
  ['{68140F4B-23C0-42F3-81F6-A891C011BC3F}']
    function GetAsVectorView: IVectorView_1__Word; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt16BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt16Bit)]
  MachineLearning_ITensorUInt16BitStatics = interface(IInspectable)
  ['{5DF745DD-028A-481A-A27C-C7E6435E52DD}']
    function Create: MachineLearning_ITensorUInt16Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt16Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PWord): MachineLearning_ITensorUInt16Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Word): MachineLearning_ITensorUInt16Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt16BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt16Bit)]
  MachineLearning_ITensorUInt16BitStatics2 = interface(IInspectable)
  ['{8AF40C64-D69F-5315-9348-490877BBD642}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PWord): MachineLearning_ITensorUInt16Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt16Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt32Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt32Bit)]
  MachineLearning_ITensorUInt32Bit = interface(IInspectable)
  ['{D8C9C2FF-7511-45A3-BFAC-C38F370D2237}']
    function GetAsVectorView: IVectorView_1__Cardinal; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt32BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt32Bit)]
  MachineLearning_ITensorUInt32BitStatics = interface(IInspectable)
  ['{417C3837-E773-4378-8E7F-0CC33DBEA697}']
    function Create: MachineLearning_ITensorUInt32Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt32Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PCardinal): MachineLearning_ITensorUInt32Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Cardinal): MachineLearning_ITensorUInt32Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt32BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt32Bit)]
  MachineLearning_ITensorUInt32BitStatics2 = interface(IInspectable)
  ['{EF1A1F1C-314E-569D-B496-5C8447D20CD2}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PCardinal): MachineLearning_ITensorUInt32Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt32Bit; safecall;
  end;

  // Windows.Foundation.Collections.IIterator`1<UInt64>
  IIterator_1__UInt64 = interface(IInspectable)
  ['{C473ED96-76E3-5FF2-9435-47FEEBFE9539}']
    function get_Current: UInt64; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PUInt64): Cardinal; safecall;
    property Current: UInt64 read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<UInt64>
  IIterable_1__UInt64 = interface(IInspectable)
  ['{4B3A3229-7995-5F3C-B248-6C1F7E664F01}']
    function First: IIterator_1__UInt64; safecall;
  end;

  // Windows.Foundation.Collections.IVectorView`1<UInt64>
  IVectorView_1__UInt64 = interface(IInspectable)
  ['{23D156C7-7EF9-5096-AABA-1E6C9AB5CEB4}']
    function GetAt(index: Cardinal): UInt64; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: UInt64; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PUInt64): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt64Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt64Bit)]
  MachineLearning_ITensorUInt64Bit = interface(IInspectable)
  ['{2E70FFAD-04BF-4825-839A-82BAEF8C7886}']
    function GetAsVectorView: IVectorView_1__UInt64; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt64BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt64Bit)]
  MachineLearning_ITensorUInt64BitStatics = interface(IInspectable)
  ['{7A7E20EB-242F-47CB-A9C6-F602ECFBFEE4}']
    function Create: MachineLearning_ITensorUInt64Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt64Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PUInt64): MachineLearning_ITensorUInt64Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__UInt64): MachineLearning_ITensorUInt64Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt64BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt64Bit)]
  MachineLearning_ITensorUInt64BitStatics2 = interface(IInspectable)
  ['{085A687D-67E1-5B1E-B232-4FABE9CA20B3}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PUInt64): MachineLearning_ITensorUInt64Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt64Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt8Bit
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt8Bit)]
  MachineLearning_ITensorUInt8Bit = interface(IInspectable)
  ['{58E1AE27-622B-48E3-BE22-D867AED1DAAC}']
    function GetAsVectorView: IVectorView_1__Byte; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt8BitStatics
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt8Bit)]
  MachineLearning_ITensorUInt8BitStatics = interface(IInspectable)
  ['{05F67583-BC24-4220-8A41-2DCD8C5ED33C}']
    function Create: MachineLearning_ITensorUInt8Bit; overload; safecall;
    function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt8Bit; overload; safecall;
    function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorUInt8Bit; safecall;
    function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Byte): MachineLearning_ITensorUInt8Bit; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.AI.MachineLearning.ITensorUInt8BitStatics2
  [WinRTClassNameAttribute(SWindows_AI_MachineLearning_TensorUInt8Bit)]
  MachineLearning_ITensorUInt8BitStatics2 = interface(IInspectable)
  ['{2BA042D6-373E-5A3A-A2FC-A6C41BD52789}']
    function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorUInt8Bit; safecall;
    function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt8Bit; safecall;
  end;

  // Windows.AI.MachineLearning.ImageFeatureValue
  // DualAPI
  // Implements: Windows.AI.MachineLearning.IImageFeatureValue
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Statics: "Windows.AI.MachineLearning.IImageFeatureValueStatics"
  TMachineLearning_ImageFeatureValue = class(TWinRTGenericImportS<MachineLearning_IImageFeatureValueStatics>)
  public
    // -> MachineLearning_IImageFeatureValueStatics
    class function CreateFromVideoFrame(image: IVideoFrame): MachineLearning_IImageFeatureValue; static; inline;
  end;

  // Windows.AI.MachineLearning.LearningModel
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ILearningModel
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ILearningModelStatics"
  TMachineLearning_LearningModel = class(TWinRTGenericImportS<MachineLearning_ILearningModelStatics>)
  public
    // -> MachineLearning_ILearningModelStatics
    class function LoadFromStorageFileAsync(modelFile: IStorageFile): IAsyncOperation_1__MachineLearning_ILearningModel; overload; static; inline;
    class function LoadFromStreamAsync(modelStream: IRandomAccessStreamReference): IAsyncOperation_1__MachineLearning_ILearningModel; overload; static; inline;
    class function LoadFromFilePath(filePath: HSTRING): MachineLearning_ILearningModel; overload; static; inline;
    class function LoadFromStream(modelStream: IRandomAccessStreamReference): MachineLearning_ILearningModel; overload; static; inline;
    class function LoadFromStorageFileAsync(modelFile: IStorageFile; operatorProvider: MachineLearning_ILearningModelOperatorProvider): IAsyncOperation_1__MachineLearning_ILearningModel; overload; static; inline;
    class function LoadFromStreamAsync(modelStream: IRandomAccessStreamReference; operatorProvider: MachineLearning_ILearningModelOperatorProvider): IAsyncOperation_1__MachineLearning_ILearningModel; overload; static; inline;
    class function LoadFromFilePath(filePath: HSTRING; operatorProvider: MachineLearning_ILearningModelOperatorProvider): MachineLearning_ILearningModel; overload; static; inline;
    class function LoadFromStream(modelStream: IRandomAccessStreamReference; operatorProvider: MachineLearning_ILearningModelOperatorProvider): MachineLearning_ILearningModel; overload; static; inline;
  end;

  // Windows.AI.MachineLearning.LearningModelBinding
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ILearningModelBinding
  // Implements: Windows.Foundation.Collections.IMapView`2<String,Object>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  // Factory: "Windows.AI.MachineLearning.ILearningModelBindingFactory"
  TMachineLearning_LearningModelBinding = class(TWinRTGenericImportF<MachineLearning_ILearningModelBindingFactory>)
  public
    // -> MachineLearning_ILearningModelBindingFactory
    class function CreateFromSession(session: MachineLearning_ILearningModelSession): MachineLearning_ILearningModelBinding; static; inline;
  end;

  // Windows.AI.MachineLearning.LearningModelDevice
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ILearningModelDevice
  // Statics: "Windows.AI.MachineLearning.ILearningModelDeviceStatics"
  // Factory: "Windows.AI.MachineLearning.ILearningModelDeviceFactory"
  TMachineLearning_LearningModelDevice = class(TWinRTGenericImportFS<MachineLearning_ILearningModelDeviceFactory, MachineLearning_ILearningModelDeviceStatics>)
  public
    // -> MachineLearning_ILearningModelDeviceStatics
    class function CreateFromDirect3D11Device(device: DirectX_Direct3D11_IDirect3DDevice): MachineLearning_ILearningModelDevice; static; inline;

    // -> MachineLearning_ILearningModelDeviceFactory
    class function Create(deviceKind: MachineLearning_LearningModelDeviceKind): MachineLearning_ILearningModelDevice; static; inline;
  end;

  // Windows.AI.MachineLearning.LearningModelSession
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ILearningModelSession
  // Implements: Windows.Foundation.IClosable
  // Factory: "Windows.AI.MachineLearning.ILearningModelSessionFactory"
  // Factory: "Windows.AI.MachineLearning.ILearningModelSessionFactory2"
  TMachineLearning_LearningModelSession = class(TWinRTGenericImportF2<MachineLearning_ILearningModelSessionFactory, MachineLearning_ILearningModelSessionFactory2>)
  public
    // -> MachineLearning_ILearningModelSessionFactory
    class function CreateFromModel(model: MachineLearning_ILearningModel): MachineLearning_ILearningModelSession; static; inline;
    class function CreateFromModelOnDevice(model: MachineLearning_ILearningModel; deviceToRunOn: MachineLearning_ILearningModelDevice): MachineLearning_ILearningModelSession; static; inline;

    // -> MachineLearning_ILearningModelSessionFactory2
    class function CreateFromModelOnDeviceWithSessionOptions(model: MachineLearning_ILearningModel; deviceToRunOn: MachineLearning_ILearningModelDevice; learningModelSessionOptions: MachineLearning_ILearningModelSessionOptions): MachineLearning_ILearningModelSession; static; inline;
  end;

  // Windows.AI.MachineLearning.LearningModelSessionOptions
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ILearningModelSessionOptions
  // Implements: Windows.AI.MachineLearning.ILearningModelSessionOptions2
  // Instantiable: "MachineLearning_ILearningModelSessionOptions"
  TMachineLearning_LearningModelSessionOptions = class(TWinRTGenericImportI<MachineLearning_ILearningModelSessionOptions>) end;

  // Windows.AI.MachineLearning.TensorBoolean
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorBoolean
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorBooleanStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorBooleanStatics2"
  TMachineLearning_TensorBoolean = class(TWinRTGenericImportS2<MachineLearning_ITensorBooleanStatics, MachineLearning_ITensorBooleanStatics2>)
  public
    // -> MachineLearning_ITensorBooleanStatics
    class function Create: MachineLearning_ITensorBoolean; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorBoolean; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PBoolean): MachineLearning_ITensorBoolean; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Boolean): MachineLearning_ITensorBoolean; static; inline;

    // -> MachineLearning_ITensorBooleanStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PBoolean): MachineLearning_ITensorBoolean; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorBoolean; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorDouble
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorDouble
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorDoubleStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorDoubleStatics2"
  TMachineLearning_TensorDouble = class(TWinRTGenericImportS2<MachineLearning_ITensorDoubleStatics, MachineLearning_ITensorDoubleStatics2>)
  public
    // -> MachineLearning_ITensorDoubleStatics
    class function Create: MachineLearning_ITensorDouble; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorDouble; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PDouble): MachineLearning_ITensorDouble; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Double): MachineLearning_ITensorDouble; static; inline;

    // -> MachineLearning_ITensorDoubleStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PDouble): MachineLearning_ITensorDouble; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorDouble; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorFloat
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorFloat
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorFloatStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorFloatStatics2"
  TMachineLearning_TensorFloat = class(TWinRTGenericImportS2<MachineLearning_ITensorFloatStatics, MachineLearning_ITensorFloatStatics2>)
  public
    // -> MachineLearning_ITensorFloatStatics
    class function Create: MachineLearning_ITensorFloat; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorFloat; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Single): MachineLearning_ITensorFloat; static; inline;

    // -> MachineLearning_ITensorFloatStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorFloat; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorFloat16Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorFloat16Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorFloat16BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorFloat16BitStatics2"
  TMachineLearning_TensorFloat16Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorFloat16BitStatics, MachineLearning_ITensorFloat16BitStatics2>)
  public
    // -> MachineLearning_ITensorFloat16BitStatics
    class function Create: MachineLearning_ITensorFloat16Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorFloat16Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat16Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Single): MachineLearning_ITensorFloat16Bit; static; inline;

    // -> MachineLearning_ITensorFloat16BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat16Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorFloat16Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorInt16Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorInt16Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorInt16BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorInt16BitStatics2"
  TMachineLearning_TensorInt16Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorInt16BitStatics, MachineLearning_ITensorInt16BitStatics2>)
  public
    // -> MachineLearning_ITensorInt16BitStatics
    class function Create: MachineLearning_ITensorInt16Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt16Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSmallInt): MachineLearning_ITensorInt16Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__SmallInt): MachineLearning_ITensorInt16Bit; static; inline;

    // -> MachineLearning_ITensorInt16BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSmallInt): MachineLearning_ITensorInt16Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt16Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorInt32Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorInt32Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorInt32BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorInt32BitStatics2"
  TMachineLearning_TensorInt32Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorInt32BitStatics, MachineLearning_ITensorInt32BitStatics2>)
  public
    // -> MachineLearning_ITensorInt32BitStatics
    class function Create: MachineLearning_ITensorInt32Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt32Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PInteger): MachineLearning_ITensorInt32Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Integer): MachineLearning_ITensorInt32Bit; static; inline;

    // -> MachineLearning_ITensorInt32BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PInteger): MachineLearning_ITensorInt32Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt32Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorInt64Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorInt64Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorInt64BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorInt64BitStatics2"
  TMachineLearning_TensorInt64Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorInt64BitStatics, MachineLearning_ITensorInt64BitStatics2>)
  public
    // -> MachineLearning_ITensorInt64BitStatics
    class function Create: MachineLearning_ITensorInt64Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt64Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PInt64): MachineLearning_ITensorInt64Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Int64): MachineLearning_ITensorInt64Bit; static; inline;

    // -> MachineLearning_ITensorInt64BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PInt64): MachineLearning_ITensorInt64Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt64Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorInt8Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorInt8Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorInt8BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorInt8BitStatics2"
  TMachineLearning_TensorInt8Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorInt8BitStatics, MachineLearning_ITensorInt8BitStatics2>)
  public
    // -> MachineLearning_ITensorInt8BitStatics
    class function Create: MachineLearning_ITensorInt8Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt8Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorInt8Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Byte): MachineLearning_ITensorInt8Bit; static; inline;

    // -> MachineLearning_ITensorInt8BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorInt8Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt8Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorString
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorString
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorStringStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorStringStatics2"
  TMachineLearning_TensorString = class(TWinRTGenericImportS2<MachineLearning_ITensorStringStatics, MachineLearning_ITensorStringStatics2>)
  public
    // -> MachineLearning_ITensorStringStatics
    class function Create: MachineLearning_ITensorString; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorString; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PHSTRING): MachineLearning_ITensorString; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__HSTRING): MachineLearning_ITensorString; static; inline;

    // -> MachineLearning_ITensorStringStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PHSTRING): MachineLearning_ITensorString; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorUInt16Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorUInt16Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorUInt16BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorUInt16BitStatics2"
  TMachineLearning_TensorUInt16Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorUInt16BitStatics, MachineLearning_ITensorUInt16BitStatics2>)
  public
    // -> MachineLearning_ITensorUInt16BitStatics
    class function Create: MachineLearning_ITensorUInt16Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt16Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PWord): MachineLearning_ITensorUInt16Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Word): MachineLearning_ITensorUInt16Bit; static; inline;

    // -> MachineLearning_ITensorUInt16BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PWord): MachineLearning_ITensorUInt16Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt16Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorUInt32Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorUInt32Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorUInt32BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorUInt32BitStatics2"
  TMachineLearning_TensorUInt32Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorUInt32BitStatics, MachineLearning_ITensorUInt32BitStatics2>)
  public
    // -> MachineLearning_ITensorUInt32BitStatics
    class function Create: MachineLearning_ITensorUInt32Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt32Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PCardinal): MachineLearning_ITensorUInt32Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Cardinal): MachineLearning_ITensorUInt32Bit; static; inline;

    // -> MachineLearning_ITensorUInt32BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PCardinal): MachineLearning_ITensorUInt32Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt32Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorUInt64Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorUInt64Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorUInt64BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorUInt64BitStatics2"
  TMachineLearning_TensorUInt64Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorUInt64BitStatics, MachineLearning_ITensorUInt64BitStatics2>)
  public
    // -> MachineLearning_ITensorUInt64BitStatics
    class function Create: MachineLearning_ITensorUInt64Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt64Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PUInt64): MachineLearning_ITensorUInt64Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__UInt64): MachineLearning_ITensorUInt64Bit; static; inline;

    // -> MachineLearning_ITensorUInt64BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PUInt64): MachineLearning_ITensorUInt64Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt64Bit; static; inline;
  end;

  // Windows.AI.MachineLearning.TensorUInt8Bit
  // DualAPI
  // Implements: Windows.AI.MachineLearning.ITensorUInt8Bit
  // Implements: Windows.AI.MachineLearning.ITensor
  // Implements: Windows.AI.MachineLearning.ILearningModelFeatureValue
  // Implements: Windows.Foundation.IMemoryBuffer
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.AI.MachineLearning.ITensorUInt8BitStatics"
  // Statics: "Windows.AI.MachineLearning.ITensorUInt8BitStatics2"
  TMachineLearning_TensorUInt8Bit = class(TWinRTGenericImportS2<MachineLearning_ITensorUInt8BitStatics, MachineLearning_ITensorUInt8BitStatics2>)
  public
    // -> MachineLearning_ITensorUInt8BitStatics
    class function Create: MachineLearning_ITensorUInt8Bit; overload; static; inline;
    class function Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt8Bit; overload; static; inline;
    class function CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorUInt8Bit; static; inline;
    class function CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Byte): MachineLearning_ITensorUInt8Bit; static; inline;

    // -> MachineLearning_ITensorUInt8BitStatics2
    class function CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorUInt8Bit; static; inline;
    class function CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt8Bit; static; inline;
  end;

implementation

{ TMachineLearning_ImageFeatureValue }

class function TMachineLearning_ImageFeatureValue.CreateFromVideoFrame(image: IVideoFrame): MachineLearning_IImageFeatureValue;
begin
  Result := Statics.CreateFromVideoFrame(image);
end;


{ TMachineLearning_LearningModel }

class function TMachineLearning_LearningModel.LoadFromStorageFileAsync(modelFile: IStorageFile): IAsyncOperation_1__MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromStorageFileAsync(modelFile);
end;

class function TMachineLearning_LearningModel.LoadFromStreamAsync(modelStream: IRandomAccessStreamReference): IAsyncOperation_1__MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromStreamAsync(modelStream);
end;

class function TMachineLearning_LearningModel.LoadFromFilePath(filePath: HSTRING): MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromFilePath(filePath);
end;

class function TMachineLearning_LearningModel.LoadFromStream(modelStream: IRandomAccessStreamReference): MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromStream(modelStream);
end;

class function TMachineLearning_LearningModel.LoadFromStorageFileAsync(modelFile: IStorageFile; operatorProvider: MachineLearning_ILearningModelOperatorProvider): IAsyncOperation_1__MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromStorageFileAsync(modelFile, operatorProvider);
end;

class function TMachineLearning_LearningModel.LoadFromStreamAsync(modelStream: IRandomAccessStreamReference; operatorProvider: MachineLearning_ILearningModelOperatorProvider): IAsyncOperation_1__MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromStreamAsync(modelStream, operatorProvider);
end;

class function TMachineLearning_LearningModel.LoadFromFilePath(filePath: HSTRING; operatorProvider: MachineLearning_ILearningModelOperatorProvider): MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromFilePath(filePath, operatorProvider);
end;

class function TMachineLearning_LearningModel.LoadFromStream(modelStream: IRandomAccessStreamReference; operatorProvider: MachineLearning_ILearningModelOperatorProvider): MachineLearning_ILearningModel;
begin
  Result := Statics.LoadFromStream(modelStream, operatorProvider);
end;


{ TMachineLearning_LearningModelBinding }
// Factories for : "MachineLearning_LearningModelBinding"
// Factory: "Windows.AI.MachineLearning.ILearningModelBindingFactory"
// -> MachineLearning_ILearningModelBindingFactory

class function TMachineLearning_LearningModelBinding.CreateFromSession(session: MachineLearning_ILearningModelSession): MachineLearning_ILearningModelBinding;
begin
  Result := Factory.CreateFromSession(session);
end;


{ TMachineLearning_LearningModelDevice }

class function TMachineLearning_LearningModelDevice.CreateFromDirect3D11Device(device: DirectX_Direct3D11_IDirect3DDevice): MachineLearning_ILearningModelDevice;
begin
  Result := Statics.CreateFromDirect3D11Device(device);
end;

// Factories for : "MachineLearning_LearningModelDevice"
// Factory: "Windows.AI.MachineLearning.ILearningModelDeviceFactory"
// -> MachineLearning_ILearningModelDeviceFactory

class function TMachineLearning_LearningModelDevice.Create(deviceKind: MachineLearning_LearningModelDeviceKind): MachineLearning_ILearningModelDevice;
begin
  Result := Factory.Create(deviceKind);
end;


{ TMachineLearning_LearningModelSession }
// Factories for : "MachineLearning_LearningModelSession"
// Factory: "Windows.AI.MachineLearning.ILearningModelSessionFactory"
// -> MachineLearning_ILearningModelSessionFactory

class function TMachineLearning_LearningModelSession.CreateFromModel(model: MachineLearning_ILearningModel): MachineLearning_ILearningModelSession;
begin
  Result := Factory.CreateFromModel(model);
end;

class function TMachineLearning_LearningModelSession.CreateFromModelOnDevice(model: MachineLearning_ILearningModel; deviceToRunOn: MachineLearning_ILearningModelDevice): MachineLearning_ILearningModelSession;
begin
  Result := Factory.CreateFromModelOnDevice(model, deviceToRunOn);
end;

// Factory: "Windows.AI.MachineLearning.ILearningModelSessionFactory2"
// -> MachineLearning_ILearningModelSessionFactory2

class function TMachineLearning_LearningModelSession.CreateFromModelOnDeviceWithSessionOptions(model: MachineLearning_ILearningModel; deviceToRunOn: MachineLearning_ILearningModelDevice; learningModelSessionOptions: MachineLearning_ILearningModelSessionOptions): MachineLearning_ILearningModelSession;
begin
  Result := Factory2.CreateFromModelOnDeviceWithSessionOptions(model, deviceToRunOn, learningModelSessionOptions);
end;


{ TMachineLearning_LearningModelSessionOptions }

{ TMachineLearning_TensorBoolean }

class function TMachineLearning_TensorBoolean.Create: MachineLearning_ITensorBoolean;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorBoolean.Create(shape: IIterable_1__Int64): MachineLearning_ITensorBoolean;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorBoolean.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PBoolean): MachineLearning_ITensorBoolean;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorBoolean.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Boolean): MachineLearning_ITensorBoolean;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorBoolean.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PBoolean): MachineLearning_ITensorBoolean;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorBoolean.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorBoolean;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorDouble }

class function TMachineLearning_TensorDouble.Create: MachineLearning_ITensorDouble;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorDouble.Create(shape: IIterable_1__Int64): MachineLearning_ITensorDouble;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorDouble.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PDouble): MachineLearning_ITensorDouble;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorDouble.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Double): MachineLearning_ITensorDouble;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorDouble.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PDouble): MachineLearning_ITensorDouble;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorDouble.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorDouble;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorFloat }

class function TMachineLearning_TensorFloat.Create: MachineLearning_ITensorFloat;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorFloat.Create(shape: IIterable_1__Int64): MachineLearning_ITensorFloat;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorFloat.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorFloat.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Single): MachineLearning_ITensorFloat;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorFloat.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorFloat.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorFloat;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorFloat16Bit }

class function TMachineLearning_TensorFloat16Bit.Create: MachineLearning_ITensorFloat16Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorFloat16Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorFloat16Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorFloat16Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat16Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorFloat16Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Single): MachineLearning_ITensorFloat16Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorFloat16Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSingle): MachineLearning_ITensorFloat16Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorFloat16Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorFloat16Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorInt16Bit }

class function TMachineLearning_TensorInt16Bit.Create: MachineLearning_ITensorInt16Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorInt16Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt16Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorInt16Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PSmallInt): MachineLearning_ITensorInt16Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorInt16Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__SmallInt): MachineLearning_ITensorInt16Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorInt16Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PSmallInt): MachineLearning_ITensorInt16Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorInt16Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt16Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorInt32Bit }

class function TMachineLearning_TensorInt32Bit.Create: MachineLearning_ITensorInt32Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorInt32Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt32Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorInt32Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PInteger): MachineLearning_ITensorInt32Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorInt32Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Integer): MachineLearning_ITensorInt32Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorInt32Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PInteger): MachineLearning_ITensorInt32Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorInt32Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt32Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorInt64Bit }

class function TMachineLearning_TensorInt64Bit.Create: MachineLearning_ITensorInt64Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorInt64Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt64Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorInt64Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PInt64): MachineLearning_ITensorInt64Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorInt64Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Int64): MachineLearning_ITensorInt64Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorInt64Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PInt64): MachineLearning_ITensorInt64Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorInt64Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt64Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorInt8Bit }

class function TMachineLearning_TensorInt8Bit.Create: MachineLearning_ITensorInt8Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorInt8Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorInt8Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorInt8Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorInt8Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorInt8Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Byte): MachineLearning_ITensorInt8Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorInt8Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorInt8Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorInt8Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorInt8Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorString }

class function TMachineLearning_TensorString.Create: MachineLearning_ITensorString;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorString.Create(shape: IIterable_1__Int64): MachineLearning_ITensorString;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorString.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PHSTRING): MachineLearning_ITensorString;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorString.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__HSTRING): MachineLearning_ITensorString;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorString.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PHSTRING): MachineLearning_ITensorString;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;


{ TMachineLearning_TensorUInt16Bit }

class function TMachineLearning_TensorUInt16Bit.Create: MachineLearning_ITensorUInt16Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorUInt16Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt16Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorUInt16Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PWord): MachineLearning_ITensorUInt16Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt16Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Word): MachineLearning_ITensorUInt16Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorUInt16Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PWord): MachineLearning_ITensorUInt16Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt16Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt16Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorUInt32Bit }

class function TMachineLearning_TensorUInt32Bit.Create: MachineLearning_ITensorUInt32Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorUInt32Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt32Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorUInt32Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PCardinal): MachineLearning_ITensorUInt32Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt32Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Cardinal): MachineLearning_ITensorUInt32Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorUInt32Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PCardinal): MachineLearning_ITensorUInt32Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt32Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt32Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorUInt64Bit }

class function TMachineLearning_TensorUInt64Bit.Create: MachineLearning_ITensorUInt64Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorUInt64Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt64Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorUInt64Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PUInt64): MachineLearning_ITensorUInt64Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt64Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__UInt64): MachineLearning_ITensorUInt64Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorUInt64Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PUInt64): MachineLearning_ITensorUInt64Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt64Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt64Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


{ TMachineLearning_TensorUInt8Bit }

class function TMachineLearning_TensorUInt8Bit.Create: MachineLearning_ITensorUInt8Bit;
begin
  Result := Statics.Create;
end;

class function TMachineLearning_TensorUInt8Bit.Create(shape: IIterable_1__Int64): MachineLearning_ITensorUInt8Bit;
begin
  Result := Statics.Create(shape);
end;

class function TMachineLearning_TensorUInt8Bit.CreateFromArray(shape: IIterable_1__Int64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorUInt8Bit;
begin
  Result := Statics.CreateFromArray(shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt8Bit.CreateFromIterable(shape: IIterable_1__Int64; data: IIterable_1__Byte): MachineLearning_ITensorUInt8Bit;
begin
  Result := Statics.CreateFromIterable(shape, data);
end;


class function TMachineLearning_TensorUInt8Bit.CreateFromShapeArrayAndDataArray(shapeSize: Cardinal; shape: PInt64; dataSize: Cardinal; data: PByte): MachineLearning_ITensorUInt8Bit;
begin
  Result := Statics2.CreateFromShapeArrayAndDataArray(shapeSize, shape, dataSize, data);
end;

class function TMachineLearning_TensorUInt8Bit.CreateFromBuffer(shapeSize: Cardinal; shape: PInt64; buffer: IBuffer): MachineLearning_ITensorUInt8Bit;
begin
  Result := Statics2.CreateFromBuffer(shapeSize, shape, buffer);
end;


end.
