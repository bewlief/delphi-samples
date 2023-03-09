{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.Foundation.Collections;

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

  AsyncOperationCompletedHandler_1__IPropertySet_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IPropertySet_Delegate_Base;
  AsyncOperationCompletedHandler_1__IPropertySet = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__IPropertySet;
  PAsyncOperationCompletedHandler_1__IPropertySet = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__IPropertySet;
  CollectionChange = Winapi.CommonTypes.CollectionChange;
  PCollectionChange = Winapi.CommonTypes.PCollectionChange;
  IAsyncOperation_1__IPropertySet_Base = Winapi.CommonTypes.IAsyncOperation_1__IPropertySet_Base;
  IAsyncOperation_1__IPropertySet = Winapi.CommonTypes.IAsyncOperation_1__IPropertySet;
  PIAsyncOperation_1__IPropertySet = Winapi.CommonTypes.PIAsyncOperation_1__IPropertySet;
  IPropertySet = Winapi.CommonTypes.IPropertySet;
  PIPropertySet = Winapi.CommonTypes.PIPropertySet;
  IVectorChangedEventArgs = Winapi.CommonTypes.IVectorChangedEventArgs;
  PIVectorChangedEventArgs = Winapi.CommonTypes.PIVectorChangedEventArgs;
  TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs_Delegate_Base;
  TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs;
  PTypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__Playback_IMediaPlaybackItem__IVectorChangedEventArgs;

  // Windows.Foundation.Collections Interfaces

  // Windows.Foundation.Collections.PropertySet
  // DualAPI
  // Implements: Windows.Foundation.Collections.IPropertySet
  // Implements: Windows.Foundation.Collections.IObservableMap`2<String,Object>
  // Implements: Windows.Foundation.Collections.IMap`2<String,Object>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  // Instantiable: "IPropertySet"
  TPropertySet = class(TWinRTGenericImportI<IPropertySet>) end;

  // Windows.Foundation.Collections.StringMap
  // DualAPI
  // Implements: Windows.Foundation.Collections.IMap`2<String,String>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,String>>
  // Implements: Windows.Foundation.Collections.IObservableMap`2<String,String>
  // Instantiable: "IMap_2__HSTRING__HSTRING"
  TStringMap = class(TWinRTGenericImportI<IMap_2__HSTRING__HSTRING>) end;

  // Windows.Foundation.Collections.ValueSet
  // DualAPI
  // Implements: Windows.Foundation.Collections.IPropertySet
  // Implements: Windows.Foundation.Collections.IObservableMap`2<String,Object>
  // Implements: Windows.Foundation.Collections.IMap`2<String,Object>
  // Implements: Windows.Foundation.Collections.IIterable`1<Windows.Foundation.Collections.IKeyValuePair`2<String,Object>>
  // Instantiable: "IPropertySet"
  TValueSet = class(TWinRTGenericImportI<IPropertySet>) end;

implementation

{ TPropertySet }

{ TStringMap }

{ TValueSet }

end.
