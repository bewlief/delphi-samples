{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Storage.Streams;

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
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer_Delegate_Base;
  AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer;
  PAsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IMapView_2__Cardinal__IBuffer;
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Delegate_Base;
  AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference;
  PAsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IMapView_2__HSTRING__IRandomAccessStreamReference;
  AsyncOperationCompletedHandler_1__IRandomAccessStream_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IRandomAccessStream_Delegate_Base;
  AsyncOperationCompletedHandler_1__IRandomAccessStream = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IRandomAccessStream;
  PAsyncOperationCompletedHandler_1__IRandomAccessStream = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IRandomAccessStream;
  AsyncOperationCompletedHandler_1__IRandomAccessStreamReference_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IRandomAccessStreamReference_Delegate_Base;
  AsyncOperationCompletedHandler_1__IRandomAccessStreamReference = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IRandomAccessStreamReference;
  PAsyncOperationCompletedHandler_1__IRandomAccessStreamReference = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IRandomAccessStreamReference;
  AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType_Delegate_Base;
  AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType;
  PAsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IRandomAccessStreamWithContentType;
  IAsyncOperation_1__IMapView_2__Cardinal__IBuffer_Base = Winapi.CommonTypes.IAsyncOperation_1__IMapView_2__Cardinal__IBuffer_Base;
  IAsyncOperation_1__IMapView_2__Cardinal__IBuffer = Winapi.CommonTypes.IAsyncOperation_1__IMapView_2__Cardinal__IBuffer;
  PIAsyncOperation_1__IMapView_2__Cardinal__IBuffer = Winapi.CommonTypes.PIAsyncOperation_1__IMapView_2__Cardinal__IBuffer;
  IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Base = Winapi.CommonTypes.IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference_Base;
  IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.IAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference;
  PIAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.PIAsyncOperation_1__IMapView_2__HSTRING__IRandomAccessStreamReference;
  IAsyncOperation_1__IRandomAccessStream_Base = Winapi.CommonTypes.IAsyncOperation_1__IRandomAccessStream_Base;
  IAsyncOperation_1__IRandomAccessStream = Winapi.CommonTypes.IAsyncOperation_1__IRandomAccessStream;
  PIAsyncOperation_1__IRandomAccessStream = Winapi.CommonTypes.PIAsyncOperation_1__IRandomAccessStream;
  IAsyncOperation_1__IRandomAccessStreamReference_Base = Winapi.CommonTypes.IAsyncOperation_1__IRandomAccessStreamReference_Base;
  IAsyncOperation_1__IRandomAccessStreamReference = Winapi.CommonTypes.IAsyncOperation_1__IRandomAccessStreamReference;
  PIAsyncOperation_1__IRandomAccessStreamReference = Winapi.CommonTypes.PIAsyncOperation_1__IRandomAccessStreamReference;
  IAsyncOperation_1__IRandomAccessStreamWithContentType_Base = Winapi.CommonTypes.IAsyncOperation_1__IRandomAccessStreamWithContentType_Base;
  IAsyncOperation_1__IRandomAccessStreamWithContentType = Winapi.CommonTypes.IAsyncOperation_1__IRandomAccessStreamWithContentType;
  PIAsyncOperation_1__IRandomAccessStreamWithContentType = Winapi.CommonTypes.PIAsyncOperation_1__IRandomAccessStreamWithContentType;
  IBuffer = Winapi.CommonTypes.IBuffer;
  PIBuffer = Winapi.CommonTypes.PIBuffer;
  IInputStream = Winapi.CommonTypes.IInputStream;
  PIInputStream = Winapi.CommonTypes.PIInputStream;
  IMap_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.IMap_2__HSTRING__IRandomAccessStreamReference;
  PIMap_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.PIMap_2__HSTRING__IRandomAccessStreamReference;
  IMapView_2__Cardinal__IBuffer_Base = Winapi.CommonTypes.IMapView_2__Cardinal__IBuffer_Base;
  IMapView_2__Cardinal__IBuffer = Winapi.CommonTypes.IMapView_2__Cardinal__IBuffer;
  PIMapView_2__Cardinal__IBuffer = Winapi.CommonTypes.PIMapView_2__Cardinal__IBuffer;
  IMapView_2__HSTRING__IRandomAccessStreamReference_Base = Winapi.CommonTypes.IMapView_2__HSTRING__IRandomAccessStreamReference_Base;
  IMapView_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.IMapView_2__HSTRING__IRandomAccessStreamReference;
  PIMapView_2__HSTRING__IRandomAccessStreamReference = Winapi.CommonTypes.PIMapView_2__HSTRING__IRandomAccessStreamReference;
  InputStreamOptions = Winapi.CommonTypes.InputStreamOptions;
  PInputStreamOptions = Winapi.CommonTypes.PInputStreamOptions;
  IOutputStream = Winapi.CommonTypes.IOutputStream;
  PIOutputStream = Winapi.CommonTypes.PIOutputStream;
  IRandomAccessStream = Winapi.CommonTypes.IRandomAccessStream;
  PIRandomAccessStream = Winapi.CommonTypes.PIRandomAccessStream;
  IRandomAccessStreamReference = Winapi.CommonTypes.IRandomAccessStreamReference;
  PIRandomAccessStreamReference = Winapi.CommonTypes.PIRandomAccessStreamReference;
  IRandomAccessStreamWithContentType = Winapi.CommonTypes.IRandomAccessStreamWithContentType;
  PIRandomAccessStreamWithContentType = Winapi.CommonTypes.PIRandomAccessStreamWithContentType;
  IVectorView_1__IBuffer = Winapi.CommonTypes.IVectorView_1__IBuffer;
  PIVectorView_1__IBuffer = Winapi.CommonTypes.PIVectorView_1__IBuffer;

  // Forward declarations for interfaces

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>
  IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = interface;
  PIKeyValuePair_2__HSTRING__IRandomAccessStreamReference = ^IKeyValuePair_2__HSTRING__IRandomAccessStreamReference;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IIterator_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = ^IIterator_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IIterable_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = ^IIterable_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStream>
  IIterator_1__IRandomAccessStream = interface;
  PIIterator_1__IRandomAccessStream = ^IIterator_1__IRandomAccessStream;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStream>
  IIterable_1__IRandomAccessStream = interface;
  PIIterable_1__IRandomAccessStream = ^IIterable_1__IRandomAccessStream;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStream>
  IVectorView_1__IRandomAccessStream = interface;
  PIVectorView_1__IRandomAccessStream = ^IVectorView_1__IRandomAccessStream;

  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Streams.IRandomAccessStream>
  IVector_1__IRandomAccessStream = interface;
  PIVector_1__IRandomAccessStream = ^IVector_1__IRandomAccessStream;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IBuffer>
  AsyncOperationCompletedHandler_1__IBuffer = interface;
  PAsyncOperationCompletedHandler_1__IBuffer = ^AsyncOperationCompletedHandler_1__IBuffer;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IBuffer>
  IAsyncOperation_1__IBuffer = interface;
  PIAsyncOperation_1__IBuffer = ^IAsyncOperation_1__IBuffer;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IInputStream>
  AsyncOperationCompletedHandler_1__IInputStream = interface;
  PAsyncOperationCompletedHandler_1__IInputStream = ^AsyncOperationCompletedHandler_1__IInputStream;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IInputStream>
  IAsyncOperation_1__IInputStream = interface;
  PIAsyncOperation_1__IInputStream = ^IAsyncOperation_1__IInputStream;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IBuffer>
  IIterator_1__IBuffer = interface;
  PIIterator_1__IBuffer = ^IIterator_1__IBuffer;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IBuffer>
  IIterable_1__IBuffer = interface;
  PIIterable_1__IBuffer = ^IIterable_1__IBuffer;

  // Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>
  IKeyValuePair_2__Cardinal__IBuffer = interface;
  PIKeyValuePair_2__Cardinal__IBuffer = ^IKeyValuePair_2__Cardinal__IBuffer;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IIterator_1__IKeyValuePair_2__Cardinal__IBuffer = interface;
  PIIterator_1__IKeyValuePair_2__Cardinal__IBuffer = ^IIterator_1__IKeyValuePair_2__Cardinal__IBuffer;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IIterable_1__IKeyValuePair_2__Cardinal__IBuffer = interface;
  PIIterable_1__IKeyValuePair_2__Cardinal__IBuffer = ^IIterable_1__IKeyValuePair_2__Cardinal__IBuffer;

  // Windows.Foundation.Collections.IMap`2<UInt32,Windows.Storage.Streams.IBuffer>
  IMap_2__Cardinal__IBuffer = interface;
  PIMap_2__Cardinal__IBuffer = ^IMap_2__Cardinal__IBuffer;

  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Streams.IBuffer>
  IVector_1__IBuffer = interface;
  PIVector_1__IBuffer = ^IVector_1__IBuffer;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IOutputStream>
  AsyncOperationCompletedHandler_1__IOutputStream = interface;
  PAsyncOperationCompletedHandler_1__IOutputStream = ^AsyncOperationCompletedHandler_1__IOutputStream;

  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IOutputStream>
  IAsyncOperation_1__IOutputStream = interface;
  PIAsyncOperation_1__IOutputStream = ^IAsyncOperation_1__IOutputStream;

  // Windows.Storage.Streams.IDataReader
  IDataReader = interface;
  PIDataReader = ^IDataReader;

  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationProgressHandler_2__IBuffer__Cardinal = ^AsyncOperationProgressHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface;
  PAsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = ^AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal;

  // Windows.Foundation.IAsyncOperationWithProgress`2<Windows.Storage.Streams.IBuffer,UInt32>
  IAsyncOperationWithProgress_2__IBuffer__Cardinal = interface;
  PIAsyncOperationWithProgress_2__IBuffer__Cardinal = ^IAsyncOperationWithProgress_2__IBuffer__Cardinal;

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>
  IKeyValuePair_2__HSTRING__IBuffer = interface;
  PIKeyValuePair_2__HSTRING__IBuffer = ^IKeyValuePair_2__HSTRING__IBuffer;

  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>>
  IIterator_1__IKeyValuePair_2__HSTRING__IBuffer = interface;
  PIIterator_1__IKeyValuePair_2__HSTRING__IBuffer = ^IIterator_1__IKeyValuePair_2__HSTRING__IBuffer;

  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>>
  IIterable_1__IKeyValuePair_2__HSTRING__IBuffer = interface;
  PIIterable_1__IKeyValuePair_2__HSTRING__IBuffer = ^IIterable_1__IKeyValuePair_2__HSTRING__IBuffer;

  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IBuffer>
  IMapView_2__HSTRING__IBuffer = interface;
  PIMapView_2__HSTRING__IBuffer = ^IMapView_2__HSTRING__IBuffer;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IIterator_1__IRandomAccessStreamWithContentType = interface;
  PIIterator_1__IRandomAccessStreamWithContentType = ^IIterator_1__IRandomAccessStreamWithContentType;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IIterable_1__IRandomAccessStreamWithContentType = interface;
  PIIterable_1__IRandomAccessStreamWithContentType = ^IIterable_1__IRandomAccessStreamWithContentType;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IVectorView_1__IRandomAccessStreamWithContentType = interface;
  PIVectorView_1__IRandomAccessStreamWithContentType = ^IVectorView_1__IRandomAccessStreamWithContentType;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IRandomAccessStreamWithContentType = interface;
  PAsyncOperationCompletedHandler_1__IVectorView_1__IRandomAccessStreamWithContentType = ^AsyncOperationCompletedHandler_1__IVectorView_1__IRandomAccessStreamWithContentType;

  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>>
  IAsyncOperation_1__IVectorView_1__IRandomAccessStreamWithContentType = interface;
  PIAsyncOperation_1__IVectorView_1__IRandomAccessStreamWithContentType = ^IAsyncOperation_1__IVectorView_1__IRandomAccessStreamWithContentType;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IIterator_1__IRandomAccessStreamReference = interface;
  PIIterator_1__IRandomAccessStreamReference = ^IIterator_1__IRandomAccessStreamReference;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IIterable_1__IRandomAccessStreamReference = interface;
  PIIterable_1__IRandomAccessStreamReference = ^IIterable_1__IRandomAccessStreamReference;

  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IVectorView_1__IRandomAccessStreamReference = interface;
  PIVectorView_1__IRandomAccessStreamReference = ^IVectorView_1__IRandomAccessStreamReference;

  // Windows.Storage.Streams.IBufferFactory
  IBufferFactory = interface;
  PIBufferFactory = ^IBufferFactory;

  // Windows.Storage.Streams.IBufferStatics
  IBufferStatics = interface;
  PIBufferStatics = ^IBufferStatics;

  // Windows.Storage.Streams.IDataReaderFactory
  IDataReaderFactory = interface;
  PIDataReaderFactory = ^IDataReaderFactory;

  // Windows.Storage.Streams.IDataReaderStatics
  IDataReaderStatics = interface;
  PIDataReaderStatics = ^IDataReaderStatics;

  // Windows.Storage.Streams.IDataWriter
  IDataWriter = interface;
  PIDataWriter = ^IDataWriter;

  // Windows.Storage.Streams.IDataWriterFactory
  IDataWriterFactory = interface;
  PIDataWriterFactory = ^IDataWriterFactory;

  // Windows.Storage.Streams.IInputStreamReference
  IInputStreamReference = interface;
  PIInputStreamReference = ^IInputStreamReference;

  // Windows.Storage.Streams.IRandomAccessStreamReferenceStatics
  IRandomAccessStreamReferenceStatics = interface;
  PIRandomAccessStreamReferenceStatics = ^IRandomAccessStreamReferenceStatics;

  // Windows.Storage.Streams Enums

  // Windows.Storage.Streams.ByteOrder
  ByteOrder = (
    LittleEndian = 0,
    BigEndian = 1
  );
  PByteOrder = ^ByteOrder;

  // Windows.Storage.Streams.FileOpenDisposition
  FileOpenDisposition = (
    OpenExisting = 0,
    OpenAlways = 1,
    CreateNew = 2,
    CreateAlways = 3,
    TruncateExisting = 4
  );
  PFileOpenDisposition = ^FileOpenDisposition;

  // Windows.Storage.Streams.UnicodeEncoding
  UnicodeEncoding = (
    Utf8 = 0,
    Utf16LE = 1,
    Utf16BE = 2
  );
  PUnicodeEncoding = ^UnicodeEncoding;

  // Windows.Storage.Streams Interfaces

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>
  IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = interface(IInspectable)
  ['{02C75FC1-0806-5B2B-A690-DA0F3E03EF45}']
    function get_Key: HSTRING; safecall;
    function get_Value: IRandomAccessStreamReference; safecall;
    property Key: HSTRING read get_Key;
    property Value: IRandomAccessStreamReference read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IIterator_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference_Base = interface(IInspectable)
  ['{9419AF53-ACB8-5328-8853-70BA87EB6AD5}']
    function get_Current: IKeyValuePair_2__HSTRING__IRandomAccessStreamReference; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IRandomAccessStreamReference): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IRandomAccessStreamReference read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IIterator_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = interface(IIterator_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference_Base)
  ['{F68346CB-785C-5C53-A795-4A270766A47A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IIterable_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference_Base = interface(IInspectable)
  ['{C9729BA7-5E20-569D-A3D1-97A4E653E5BB}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IRandomAccessStreamReference>>
  IIterable_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference = interface(IIterable_1__IKeyValuePair_2__HSTRING__IRandomAccessStreamReference_Base)
  ['{8D579E1F-0FF6-5010-A14B-6C7DE8433955}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStream>
  IIterator_1__IRandomAccessStream_Base = interface(IInspectable)
  ['{C875446A-587F-58DA-897E-3BBE5EC7C30B}']
    function get_Current: IRandomAccessStream; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIRandomAccessStream): Cardinal; safecall;
    property Current: IRandomAccessStream read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStream>
  IIterator_1__IRandomAccessStream = interface(IIterator_1__IRandomAccessStream_Base)
  ['{C875446A-587F-58DA-897E-3BBE5EC7C30B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStream>
  IIterable_1__IRandomAccessStream_Base = interface(IInspectable)
  ['{BA666A00-1555-5DF4-81A5-07D23F7FFCEB}']
    function First: IIterator_1__IRandomAccessStream; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStream>
  IIterable_1__IRandomAccessStream = interface(IIterable_1__IRandomAccessStream_Base)
  ['{BA666A00-1555-5DF4-81A5-07D23F7FFCEB}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStream>
  IVectorView_1__IRandomAccessStream = interface(IInspectable)
  ['{92CD0A46-2266-5CD6-9293-E111299F2793}']
    function GetAt(index: Cardinal): IRandomAccessStream; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IRandomAccessStream; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIRandomAccessStream): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Streams.IRandomAccessStream>
  IVector_1__IRandomAccessStream_Base = interface(IInspectable)
  ['{2736B66B-DAA3-5E0C-9842-6A0F44B5440B}']
    function GetAt(index: Cardinal): IRandomAccessStream; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IRandomAccessStream; safecall;
    function IndexOf(value: IRandomAccessStream; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IRandomAccessStream); safecall;
    procedure InsertAt(index: Cardinal; value: IRandomAccessStream); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IRandomAccessStream); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIRandomAccessStream): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIRandomAccessStream); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Streams.IRandomAccessStream>
  IVector_1__IRandomAccessStream = interface(IVector_1__IRandomAccessStream_Base)
  ['{2736B66B-DAA3-5E0C-9842-6A0F44B5440B}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IBuffer>
  AsyncOperationCompletedHandler_1__IBuffer_Delegate_Base = interface(IUnknown)
  ['{51C3D2FD-B8A1-5620-B746-7EE6D533ACA3}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IBuffer; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IBuffer>
  AsyncOperationCompletedHandler_1__IBuffer = interface(AsyncOperationCompletedHandler_1__IBuffer_Delegate_Base)
  ['{51C3D2FD-B8A1-5620-B746-7EE6D533ACA3}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IBuffer>
  IAsyncOperation_1__IBuffer_Base = interface(IInspectable)
  ['{3BEE8834-B9A7-5A80-A746-5EF097227878}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IBuffer); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IBuffer; safecall;
    function GetResults: IBuffer; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IBuffer read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IBuffer>
  IAsyncOperation_1__IBuffer = interface(IAsyncOperation_1__IBuffer_Base)
  ['{3BEE8834-B9A7-5A80-A746-5EF097227878}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IInputStream>
  AsyncOperationCompletedHandler_1__IInputStream_Delegate_Base = interface(IUnknown)
  ['{D0BD0125-9049-57A3-BD66-E2525D98C814}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IInputStream; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IInputStream>
  AsyncOperationCompletedHandler_1__IInputStream = interface(AsyncOperationCompletedHandler_1__IInputStream_Delegate_Base)
  ['{D0BD0125-9049-57A3-BD66-E2525D98C814}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IInputStream>
  IAsyncOperation_1__IInputStream_Base = interface(IInspectable)
  ['{A8FE0732-556D-5841-B7EE-B3450FB52666}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IInputStream); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IInputStream; safecall;
    function GetResults: IInputStream; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IInputStream read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IInputStream>
  IAsyncOperation_1__IInputStream = interface(IAsyncOperation_1__IInputStream_Base)
  ['{A8FE0732-556D-5841-B7EE-B3450FB52666}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IBuffer>
  IIterator_1__IBuffer_Base = interface(IInspectable)
  ['{AFEE38E0-F882-5F10-9655-1FC98CC8CCE5}']
    function get_Current: IBuffer; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIBuffer): Cardinal; safecall;
    property Current: IBuffer read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IBuffer>
  IIterator_1__IBuffer = interface(IIterator_1__IBuffer_Base)
  ['{AFEE38E0-F882-5F10-9655-1FC98CC8CCE5}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IBuffer>
  IIterable_1__IBuffer_Base = interface(IInspectable)
  ['{902972BF-A984-5443-B1C5-2F04A99E1FCA}']
    function First: IIterator_1__IBuffer; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IBuffer>
  IIterable_1__IBuffer = interface(IIterable_1__IBuffer_Base)
  ['{902972BF-A984-5443-B1C5-2F04A99E1FCA}']
  end;

  // Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>
  IKeyValuePair_2__Cardinal__IBuffer = interface(IInspectable)
  ['{82A3A3B7-E04A-5395-8487-7F94F1508CE7}']
    function get_Key: Cardinal; safecall;
    function get_Value: IBuffer; safecall;
    property Key: Cardinal read get_Key;
    property Value: IBuffer read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IIterator_1__IKeyValuePair_2__Cardinal__IBuffer_Base = interface(IInspectable)
  ['{A295FA0C-C99F-5109-8AB9-91534BB48C9B}']
    function get_Current: IKeyValuePair_2__Cardinal__IBuffer; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__Cardinal__IBuffer): Cardinal; safecall;
    property Current: IKeyValuePair_2__Cardinal__IBuffer read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IIterator_1__IKeyValuePair_2__Cardinal__IBuffer = interface(IIterator_1__IKeyValuePair_2__Cardinal__IBuffer_Base)
  ['{A295FA0C-C99F-5109-8AB9-91534BB48C9B}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IIterable_1__IKeyValuePair_2__Cardinal__IBuffer_Base = interface(IInspectable)
  ['{4FE7FE23-22B1-528C-881D-A4ECEAEF0F11}']
    function First: IIterator_1__IKeyValuePair_2__Cardinal__IBuffer; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<UInt32,Windows.Storage.Streams.IBuffer>>
  IIterable_1__IKeyValuePair_2__Cardinal__IBuffer = interface(IIterable_1__IKeyValuePair_2__Cardinal__IBuffer_Base)
  ['{4FE7FE23-22B1-528C-881D-A4ECEAEF0F11}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IMap`2<UInt32,Windows.Storage.Streams.IBuffer>
  IMap_2__Cardinal__IBuffer = interface(IInspectable)
  ['{5D2591DF-48C5-5734-9EF1-BD639B032007}']
    function Lookup(key: Cardinal): IBuffer; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: Cardinal): Boolean; safecall;
    function GetView: IMapView_2__Cardinal__IBuffer; safecall;
    function Insert(key: Cardinal; value: IBuffer): Boolean; safecall;
    procedure Remove(key: Cardinal); safecall;
    procedure Clear; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Streams.IBuffer>
  IVector_1__IBuffer_Base = interface(IInspectable)
  ['{308FE894-CC06-5007-BC85-CBE94AC1A70C}']
    function GetAt(index: Cardinal): IBuffer; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IBuffer; safecall;
    function IndexOf(value: IBuffer; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IBuffer); safecall;
    procedure InsertAt(index: Cardinal; value: IBuffer); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IBuffer); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIBuffer): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIBuffer); safecall;
    property Size: Cardinal read get_Size;
  end;
  // Windows.Foundation.Collections.IVector`1<Windows.Storage.Streams.IBuffer>
  IVector_1__IBuffer = interface(IVector_1__IBuffer_Base)
  ['{308FE894-CC06-5007-BC85-CBE94AC1A70C}']
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IOutputStream>
  AsyncOperationCompletedHandler_1__IOutputStream_Delegate_Base = interface(IUnknown)
  ['{BCB37F4F-3AF4-561C-A9E3-EEF1738494D7}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IOutputStream; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Storage.Streams.IOutputStream>
  AsyncOperationCompletedHandler_1__IOutputStream = interface(AsyncOperationCompletedHandler_1__IOutputStream_Delegate_Base)
  ['{BCB37F4F-3AF4-561C-A9E3-EEF1738494D7}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IOutputStream>
  IAsyncOperation_1__IOutputStream_Base = interface(IInspectable)
  ['{E8736833-D013-5361-977D-C5E99934680E}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IOutputStream); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IOutputStream; safecall;
    function GetResults: IOutputStream; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IOutputStream read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Storage.Streams.IOutputStream>
  IAsyncOperation_1__IOutputStream = interface(IAsyncOperation_1__IOutputStream_Base)
  ['{E8736833-D013-5361-977D-C5E99934680E}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IDataReader
  [WinRTClassNameAttribute(SWindows_Storage_Streams_DataReader)]
  IDataReader = interface(IInspectable)
  ['{E2B50029-B4C1-4314-A4B8-FB813A2F275E}']
    function get_UnconsumedBufferLength: Cardinal; safecall;
    function get_UnicodeEncoding: UnicodeEncoding; safecall;
    procedure put_UnicodeEncoding(value: UnicodeEncoding); safecall;
    function get_ByteOrder: ByteOrder; safecall;
    procedure put_ByteOrder(value: ByteOrder); safecall;
    function get_InputStreamOptions: InputStreamOptions; safecall;
    procedure put_InputStreamOptions(value: InputStreamOptions); safecall;
    function ReadByte: Byte; safecall;
    procedure ReadBytes(valueSize: Cardinal; value: PByte); safecall;
    function ReadBuffer(length: Cardinal): IBuffer; safecall;
    function ReadBoolean: Boolean; safecall;
    function ReadGuid: TGuid; safecall;
    function ReadInt16: SmallInt; safecall;
    function ReadInt32: Integer; safecall;
    function ReadInt64: Int64; safecall;
    function ReadUInt16: Word; safecall;
    function ReadUInt32: Cardinal; safecall;
    function ReadUInt64: UInt64; safecall;
    function ReadSingle: Single; safecall;
    function ReadDouble: Double; safecall;
    function ReadString(codeUnitCount: Cardinal): HSTRING; safecall;
    function ReadDateTime: DateTime; safecall;
    function ReadTimeSpan: TimeSpan; safecall;
    function LoadAsync(count: Cardinal): IAsyncOperation_1__Cardinal; safecall;
    function DetachBuffer: IBuffer; safecall;
    function DetachStream: IInputStream; safecall;
    property ByteOrder_: ByteOrder read get_ByteOrder write put_ByteOrder;
    property InputStreamOptions_: InputStreamOptions read get_InputStreamOptions write put_InputStreamOptions;
    property UnconsumedBufferLength: Cardinal read get_UnconsumedBufferLength;
    property UnicodeEncoding_: UnicodeEncoding read get_UnicodeEncoding write put_UnicodeEncoding;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationProgressHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationProgressHandler_2__IBuffer__Cardinal = interface(IUnknown)
  ['{BF666554-7605-5D9A-B14E-18D8C8472AFE}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; progressInfo: Cardinal); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationWithProgressCompletedHandler`2<Windows.Storage.Streams.IBuffer,UInt32>
  AsyncOperationWithProgressCompletedHandler_2__IBuffer__Cardinal = interface(IUnknown)
  ['{06386A7A-E009-5B0B-AB68-A8E48B516647}']
    procedure Invoke(asyncInfo: IAsyncOperationWithProgress_2__IBuffer__Cardinal; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
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

  // Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>
  IKeyValuePair_2__HSTRING__IBuffer = interface(IInspectable)
  ['{9114F794-2CEB-5B03-9B22-36884E1F58B3}']
    function get_Key: HSTRING; safecall;
    function get_Value: IBuffer; safecall;
    property Key: HSTRING read get_Key;
    property Value: IBuffer read get_Value;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>>
  IIterator_1__IKeyValuePair_2__HSTRING__IBuffer_Base = interface(IInspectable)
  ['{790ACB62-C4B3-57EA-A152-9E219371C6DC}']
    function get_Current: IKeyValuePair_2__HSTRING__IBuffer; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIKeyValuePair_2__HSTRING__IBuffer): Cardinal; safecall;
    property Current: IKeyValuePair_2__HSTRING__IBuffer read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>>
  IIterator_1__IKeyValuePair_2__HSTRING__IBuffer = interface(IIterator_1__IKeyValuePair_2__HSTRING__IBuffer_Base)
  ['{790ACB62-C4B3-57EA-A152-9E219371C6DC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>>
  IIterable_1__IKeyValuePair_2__HSTRING__IBuffer_Base = interface(IInspectable)
  ['{3C9FFA92-5123-5AC4-B111-03C215F0C51C}']
    function First: IIterator_1__IKeyValuePair_2__HSTRING__IBuffer; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Windows.Storage.Streams.IBuffer>>
  IIterable_1__IKeyValuePair_2__HSTRING__IBuffer = interface(IIterable_1__IKeyValuePair_2__HSTRING__IBuffer_Base)
  ['{3C9FFA92-5123-5AC4-B111-03C215F0C51C}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IBuffer>
  IMapView_2__HSTRING__IBuffer_Base = interface(IInspectable)
  ['{2CFEEC4F-E261-5F4C-AEE1-C78518E9D5B9}']
    function Lookup(key: HSTRING): IBuffer; safecall;
    function get_Size: Cardinal; safecall;
    function HasKey(key: HSTRING): Boolean; safecall;
    procedure Split(out first: IMapView_2__HSTRING__IBuffer; out second: IMapView_2__HSTRING__IBuffer); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IMapView`2<String,Windows.Storage.Streams.IBuffer>
  IMapView_2__HSTRING__IBuffer = interface(IMapView_2__HSTRING__IBuffer_Base)
  ['{2CFEEC4F-E261-5F4C-AEE1-C78518E9D5B9}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IIterator_1__IRandomAccessStreamWithContentType_Base = interface(IInspectable)
  ['{4A10752D-6B1A-5FEC-A59C-70389BF162A2}']
    function get_Current: IRandomAccessStreamWithContentType; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIRandomAccessStreamWithContentType): Cardinal; safecall;
    property Current: IRandomAccessStreamWithContentType read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IIterator_1__IRandomAccessStreamWithContentType = interface(IIterator_1__IRandomAccessStreamWithContentType_Base)
  ['{F225500A-EE51-56E4-BAC4-C835CF10C316}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IIterable_1__IRandomAccessStreamWithContentType_Base = interface(IInspectable)
  ['{034EA0C4-C20E-5C0C-BA31-64212F28E650}']
    function First: IIterator_1__IRandomAccessStreamWithContentType; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IIterable_1__IRandomAccessStreamWithContentType = interface(IIterable_1__IRandomAccessStreamWithContentType_Base)
  ['{C6B83736-1F57-5E24-9238-702C585CC3BB}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>
  IVectorView_1__IRandomAccessStreamWithContentType = interface(IInspectable)
  ['{20A82375-0F34-55AF-AE7E-AE77BC3D542F}']
    function GetAt(index: Cardinal): IRandomAccessStreamWithContentType; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IRandomAccessStreamWithContentType; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIRandomAccessStreamWithContentType): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>>
  AsyncOperationCompletedHandler_1__IVectorView_1__IRandomAccessStreamWithContentType = interface(IUnknown)
  ['{4572C48A-AAC4-5B60-AB1E-8561D15763BE}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IVectorView_1__IRandomAccessStreamWithContentType; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamWithContentType>>
  IAsyncOperation_1__IVectorView_1__IRandomAccessStreamWithContentType = interface(IInspectable)
  ['{93177A05-73F1-563E-9CB7-99C2A51D8E63}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IVectorView_1__IRandomAccessStreamWithContentType); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IRandomAccessStreamWithContentType; safecall;
    function GetResults: IVectorView_1__IRandomAccessStreamWithContentType; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IVectorView_1__IRandomAccessStreamWithContentType read get_Completed write put_Completed;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IIterator_1__IRandomAccessStreamReference = interface(IInspectable)
  ['{712E1447-A302-58CA-8CF7-C3B9B4BA67EC}']
    function get_Current: IRandomAccessStreamReference; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIRandomAccessStreamReference): Cardinal; safecall;
    property Current: IRandomAccessStreamReference read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IIterable_1__IRandomAccessStreamReference = interface(IInspectable)
  ['{AE075AFA-2F06-5011-9FE1-1AF33410E707}']
    function First: IIterator_1__IRandomAccessStreamReference; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.Storage.Streams.IRandomAccessStreamReference>
  IVectorView_1__IRandomAccessStreamReference = interface(IInspectable)
  ['{4D904A36-6418-5C09-A622-A1CC8C27EEFF}']
    function GetAt(index: Cardinal): IRandomAccessStreamReference; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IRandomAccessStreamReference; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIRandomAccessStreamReference): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IBufferFactory
  [WinRTClassNameAttribute(SWindows_Storage_Streams_Buffer)]
  IBufferFactory = interface(IInspectable)
  ['{71AF914D-C10F-484B-BC50-14BC623B3A27}']
    function Create(capacity: Cardinal): IBuffer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IBufferStatics
  [WinRTClassNameAttribute(SWindows_Storage_Streams_Buffer)]
  IBufferStatics = interface(IInspectable)
  ['{E901E65B-D716-475A-A90A-AF7229B1E741}']
    function CreateCopyFromMemoryBuffer(input: IMemoryBuffer): IBuffer; safecall;
    function CreateMemoryBufferOverIBuffer(input: IBuffer): IMemoryBuffer; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IDataReaderFactory
  [WinRTClassNameAttribute(SWindows_Storage_Streams_DataReader)]
  IDataReaderFactory = interface(IInspectable)
  ['{D7527847-57DA-4E15-914C-06806699A098}']
    function CreateDataReader(inputStream: IInputStream): IDataReader; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IDataReaderStatics
  [WinRTClassNameAttribute(SWindows_Storage_Streams_DataReader)]
  IDataReaderStatics = interface(IInspectable)
  ['{11FCBFC8-F93A-471B-B121-F379E349313C}']
    function FromBuffer(buffer: IBuffer): IDataReader; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IDataWriter
  [WinRTClassNameAttribute(SWindows_Storage_Streams_DataWriter)]
  IDataWriter = interface(IInspectable)
  ['{64B89265-D341-4922-B38A-DD4AF8808C4E}']
    function get_UnstoredBufferLength: Cardinal; safecall;
    function get_UnicodeEncoding: UnicodeEncoding; safecall;
    procedure put_UnicodeEncoding(value: UnicodeEncoding); safecall;
    function get_ByteOrder: ByteOrder; safecall;
    procedure put_ByteOrder(value: ByteOrder); safecall;
    procedure WriteByte(value: Byte); safecall;
    procedure WriteBytes(valueSize: Cardinal; value: PByte); safecall;
    procedure WriteBuffer(buffer: IBuffer); overload; safecall;
    procedure WriteBuffer(buffer: IBuffer; start: Cardinal; count: Cardinal); overload; safecall;
    procedure WriteBoolean(value: Boolean); safecall;
    procedure WriteGuid(value: TGuid); safecall;
    procedure WriteInt16(value: SmallInt); safecall;
    procedure WriteInt32(value: Integer); safecall;
    procedure WriteInt64(value: Int64); safecall;
    procedure WriteUInt16(value: Word); safecall;
    procedure WriteUInt32(value: Cardinal); safecall;
    procedure WriteUInt64(value: UInt64); safecall;
    procedure WriteSingle(value: Single); safecall;
    procedure WriteDouble(value: Double); safecall;
    procedure WriteDateTime(value: DateTime); safecall;
    procedure WriteTimeSpan(value: TimeSpan); safecall;
    function WriteString(value: HSTRING): Cardinal; safecall;
    function MeasureString(value: HSTRING): Cardinal; safecall;
    function StoreAsync: IAsyncOperation_1__Cardinal; safecall;
    function FlushAsync: IAsyncOperation_1__Boolean; safecall;
    function DetachBuffer: IBuffer; safecall;
    function DetachStream: IOutputStream; safecall;
    property ByteOrder_: ByteOrder read get_ByteOrder write put_ByteOrder;
    property UnicodeEncoding_: UnicodeEncoding read get_UnicodeEncoding write put_UnicodeEncoding;
    property UnstoredBufferLength: Cardinal read get_UnstoredBufferLength;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IDataWriterFactory
  [WinRTClassNameAttribute(SWindows_Storage_Streams_DataWriter)]
  IDataWriterFactory = interface(IInspectable)
  ['{338C67C2-8B84-4C2B-9C50-7B8767847A1F}']
    function CreateDataWriter(outputStream: IOutputStream): IDataWriter; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IInputStreamReference
  IInputStreamReference = interface(IInspectable)
  ['{43929D18-5EC9-4B5A-919C-4205B0C804B6}']
    function OpenSequentialReadAsync: IAsyncOperation_1__IInputStream; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.Storage.Streams.IRandomAccessStreamReferenceStatics
  [WinRTClassNameAttribute(SWindows_Storage_Streams_RandomAccessStreamReference)]
  IRandomAccessStreamReferenceStatics = interface(IInspectable)
  ['{857309DC-3FBF-4E7D-986F-EF3B1A07A964}']
    function CreateFromFile(&file: IStorageFile): IRandomAccessStreamReference; safecall;
    function CreateFromUri(uri: IUriRuntimeClass): IRandomAccessStreamReference; safecall;
    function CreateFromStream(stream: IRandomAccessStream): IRandomAccessStreamReference; safecall;
  end;

  // Windows.Storage.Streams.Buffer
  // DualAPI
  // Implements: Windows.Storage.Streams.IBuffer
  // Statics: "Windows.Storage.Streams.IBufferStatics"
  // Factory: "Windows.Storage.Streams.IBufferFactory"
  TBuffer = class(TWinRTGenericImportFS<IBufferFactory, IBufferStatics>)
  public
    // -> IBufferStatics
    class function CreateCopyFromMemoryBuffer(input: IMemoryBuffer): IBuffer; static; inline;
    class function CreateMemoryBufferOverIBuffer(input: IBuffer): IMemoryBuffer; static; inline;

    // -> IBufferFactory
    class function Create(capacity: Cardinal): IBuffer; static; inline;
  end;

  // Windows.Storage.Streams.DataReader
  // DualAPI
  // Explicitly imported
  // Implements: Windows.Storage.Streams.IDataReader
  // Implements: Windows.Foundation.IClosable
  // Statics: "Windows.Storage.Streams.IDataReaderStatics"
  // Factory: "Windows.Storage.Streams.IDataReaderFactory"
  TDataReader = class(TWinRTGenericImportFS<IDataReaderFactory, IDataReaderStatics>)
  public
    // -> IDataReaderStatics
    class function FromBuffer(buffer: IBuffer): IDataReader; static; inline;

    // -> IDataReaderFactory
    class function CreateDataReader(inputStream: IInputStream): IDataReader; static; inline;
  end;

  // Windows.Storage.Streams.DataWriter
  // DualAPI
  // Explicitly imported
  // Implements: Windows.Storage.Streams.IDataWriter
  // Implements: Windows.Foundation.IClosable
  // Factory: "Windows.Storage.Streams.IDataWriterFactory"
  // Instantiable: "IDataWriter"
  TDataWriter = class(TWinRTGenericImportFI<IDataWriterFactory, IDataWriter>)
  public
    // -> IDataWriterFactory
    class function CreateDataWriter(outputStream: IOutputStream): IDataWriter; static; inline;
  end;

  // Windows.Storage.Streams.RandomAccessStreamReference
  // Explicitly imported
  // Implements: Windows.Storage.Streams.IRandomAccessStreamReference
  // Statics: "Windows.Storage.Streams.IRandomAccessStreamReferenceStatics"
  TRandomAccessStreamReference = class(TWinRTGenericImportS<IRandomAccessStreamReferenceStatics>)
  public
    // -> IRandomAccessStreamReferenceStatics
    class function CreateFromFile(&file: IStorageFile): IRandomAccessStreamReference; static; inline;
    class function CreateFromUri(uri: IUriRuntimeClass): IRandomAccessStreamReference; static; inline;
    class function CreateFromStream(stream: IRandomAccessStream): IRandomAccessStreamReference; static; inline;
  end;

implementation

{ TBuffer }

class function TBuffer.CreateCopyFromMemoryBuffer(input: IMemoryBuffer): IBuffer;
begin
  Result := Statics.CreateCopyFromMemoryBuffer(input);
end;

class function TBuffer.CreateMemoryBufferOverIBuffer(input: IBuffer): IMemoryBuffer;
begin
  Result := Statics.CreateMemoryBufferOverIBuffer(input);
end;

// Factories for : "Buffer"
// Factory: "Windows.Storage.Streams.IBufferFactory"
// -> IBufferFactory

class function TBuffer.Create(capacity: Cardinal): IBuffer;
begin
  Result := Factory.Create(capacity);
end;


{ TDataReader }

class function TDataReader.FromBuffer(buffer: IBuffer): IDataReader;
begin
  Result := Statics.FromBuffer(buffer);
end;

// Factories for : "DataReader"
// Factory: "Windows.Storage.Streams.IDataReaderFactory"
// -> IDataReaderFactory

class function TDataReader.CreateDataReader(inputStream: IInputStream): IDataReader;
begin
  Result := Factory.CreateDataReader(inputStream);
end;


{ TDataWriter }
// Factories for : "DataWriter"
// Factory: "Windows.Storage.Streams.IDataWriterFactory"
// -> IDataWriterFactory

class function TDataWriter.CreateDataWriter(outputStream: IOutputStream): IDataWriter;
begin
  Result := Factory.CreateDataWriter(outputStream);
end;


{ TRandomAccessStreamReference }

class function TRandomAccessStreamReference.CreateFromFile(&file: IStorageFile): IRandomAccessStreamReference;
begin
  Result := Statics.CreateFromFile(&file);
end;

class function TRandomAccessStreamReference.CreateFromUri(uri: IUriRuntimeClass): IRandomAccessStreamReference;
begin
  Result := Statics.CreateFromUri(uri);
end;

class function TRandomAccessStreamReference.CreateFromStream(stream: IRandomAccessStream): IRandomAccessStreamReference;
begin
  Result := Statics.CreateFromStream(stream);
end;


end.
