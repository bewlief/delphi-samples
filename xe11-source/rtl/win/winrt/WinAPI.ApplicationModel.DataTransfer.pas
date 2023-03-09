{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 2020-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Winapi.ApplicationModel.DataTransfer;

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
  Winapi.Security, 
  Winapi.CommonNames;

{$SCOPEDENUMS ON}

type
  // Alias type definitions for types moved from this unit

  AsyncOperationCompletedHandler_1__DataPackageOperation_Delegate_Base = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__DataPackageOperation_Delegate_Base;
  AsyncOperationCompletedHandler_1__DataPackageOperation = Winapi.CommonTypes.AsyncOperationCompletedHandler_1__DataPackageOperation;
  PAsyncOperationCompletedHandler_1__DataPackageOperation = Winapi.CommonTypes.PAsyncOperationCompletedHandler_1__DataPackageOperation;
  DataPackageOperation = Winapi.CommonTypes.DataPackageOperation;
  PDataPackageOperation = Winapi.CommonTypes.PDataPackageOperation;
  DataProviderHandler = Winapi.CommonTypes.DataProviderHandler;
  PDataProviderHandler = Winapi.CommonTypes.PDataProviderHandler;
  DragDrop_DragDropModifiers = Winapi.CommonTypes.DragDrop_DragDropModifiers;
  PDragDrop_DragDropModifiers = Winapi.CommonTypes.PDragDrop_DragDropModifiers;
  IAsyncOperation_1__DataPackageOperation_Base = Winapi.CommonTypes.IAsyncOperation_1__DataPackageOperation_Base;
  IAsyncOperation_1__DataPackageOperation = Winapi.CommonTypes.IAsyncOperation_1__DataPackageOperation;
  PIAsyncOperation_1__DataPackageOperation = Winapi.CommonTypes.PIAsyncOperation_1__DataPackageOperation;
  IDataPackage = Winapi.CommonTypes.IDataPackage;
  PIDataPackage = Winapi.CommonTypes.PIDataPackage;
  IDataPackagePropertySet = Winapi.CommonTypes.IDataPackagePropertySet;
  PIDataPackagePropertySet = Winapi.CommonTypes.PIDataPackagePropertySet;
  IDataPackagePropertySetView = Winapi.CommonTypes.IDataPackagePropertySetView;
  PIDataPackagePropertySetView = Winapi.CommonTypes.PIDataPackagePropertySetView;
  IDataPackageView = Winapi.CommonTypes.IDataPackageView;
  PIDataPackageView = Winapi.CommonTypes.PIDataPackageView;
  IDataProviderDeferral = Winapi.CommonTypes.IDataProviderDeferral;
  PIDataProviderDeferral = Winapi.CommonTypes.PIDataProviderDeferral;
  IDataProviderRequest = Winapi.CommonTypes.IDataProviderRequest;
  PIDataProviderRequest = Winapi.CommonTypes.PIDataProviderRequest;
  IOperationCompletedEventArgs = Winapi.CommonTypes.IOperationCompletedEventArgs;
  PIOperationCompletedEventArgs = Winapi.CommonTypes.PIOperationCompletedEventArgs;
  TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs_Delegate_Base = Winapi.CommonTypes.TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs_Delegate_Base;
  TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs = Winapi.CommonTypes.TypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs;
  PTypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs = Winapi.CommonTypes.PTypedEventHandler_2__IDataPackage__IOperationCompletedEventArgs;

  // Forward declarations for interfaces

  // Windows.ApplicationModel.DataTransfer.ShareTarget.IQuickLink
  ShareTarget_IQuickLink = interface;
  PShareTarget_IQuickLink = ^ShareTarget_IQuickLink;

  // Windows.ApplicationModel.DataTransfer.ShareTarget.IShareOperation
  ShareTarget_IShareOperation = interface;
  PShareTarget_IShareOperation = ^ShareTarget_IShareOperation;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragInfo
  DragDrop_Core_ICoreDragInfo = interface;
  PDragDrop_Core_ICoreDragInfo = ^DragDrop_Core_ICoreDragInfo;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragUIOverride
  DragDrop_Core_ICoreDragUIOverride = interface;
  PDragDrop_Core_ICoreDragUIOverride = ^DragDrop_Core_ICoreDragUIOverride;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDropOperationTarget
  DragDrop_Core_ICoreDropOperationTarget = interface;
  PDragDrop_Core_ICoreDropOperationTarget = ^DragDrop_Core_ICoreDropOperationTarget;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDropOperationTargetRequestedEventArgs
  DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs = interface;
  PDragDrop_Core_ICoreDropOperationTargetRequestedEventArgs = ^DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManager,Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDropOperationTargetRequestedEventArgs>
  TypedEventHandler_2__DragDrop_Core_ICoreDragDropManager__DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs = interface;
  PTypedEventHandler_2__DragDrop_Core_ICoreDragDropManager__DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs = ^TypedEventHandler_2__DragDrop_Core_ICoreDragDropManager__DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManager
  DragDrop_Core_ICoreDragDropManager = interface;
  PDragDrop_Core_ICoreDragDropManager = ^DragDrop_Core_ICoreDragDropManager;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManagerStatics
  DragDrop_Core_ICoreDragDropManagerStatics = interface;
  PDragDrop_Core_ICoreDragDropManagerStatics = ^DragDrop_Core_ICoreDragDropManagerStatics;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragInfo2
  DragDrop_Core_ICoreDragInfo2 = interface;
  PDragDrop_Core_ICoreDragInfo2 = ^DragDrop_Core_ICoreDragInfo2;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragOperation
  DragDrop_Core_ICoreDragOperation = interface;
  PDragDrop_Core_ICoreDragOperation = ^DragDrop_Core_ICoreDragOperation;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragOperation2
  DragDrop_Core_ICoreDragOperation2 = interface;
  PDragDrop_Core_ICoreDragOperation2 = ^DragDrop_Core_ICoreDragOperation2;

  // Windows.ApplicationModel.DataTransfer.IClipboardContentOptions
  IClipboardContentOptions = interface;
  PIClipboardContentOptions = ^IClipboardContentOptions;

  // Windows.ApplicationModel.DataTransfer.IClipboardHistoryChangedEventArgs
  IClipboardHistoryChangedEventArgs = interface;
  PIClipboardHistoryChangedEventArgs = ^IClipboardHistoryChangedEventArgs;

  // Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem
  IClipboardHistoryItem = interface;
  PIClipboardHistoryItem = ^IClipboardHistoryItem;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem>
  IIterator_1__IClipboardHistoryItem = interface;
  PIIterator_1__IClipboardHistoryItem = ^IIterator_1__IClipboardHistoryItem;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem>
  IIterable_1__IClipboardHistoryItem = interface;
  PIIterable_1__IClipboardHistoryItem = ^IIterable_1__IClipboardHistoryItem;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem>
  IVectorView_1__IClipboardHistoryItem = interface;
  PIVectorView_1__IClipboardHistoryItem = ^IVectorView_1__IClipboardHistoryItem;

  // Windows.ApplicationModel.DataTransfer.IClipboardHistoryItemsResult
  IClipboardHistoryItemsResult = interface;
  PIClipboardHistoryItemsResult = ^IClipboardHistoryItemsResult;

  // Windows.ApplicationModel.DataTransfer.IClipboardStatics
  IClipboardStatics = interface;
  PIClipboardStatics = ^IClipboardStatics;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItemsResult>
  AsyncOperationCompletedHandler_1__IClipboardHistoryItemsResult = interface;
  PAsyncOperationCompletedHandler_1__IClipboardHistoryItemsResult = ^AsyncOperationCompletedHandler_1__IClipboardHistoryItemsResult;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItemsResult>
  IAsyncOperation_1__IClipboardHistoryItemsResult = interface;
  PIAsyncOperation_1__IClipboardHistoryItemsResult = ^IAsyncOperation_1__IClipboardHistoryItemsResult;

  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryChangedEventArgs>
  EventHandler_1__IClipboardHistoryChangedEventArgs = interface;
  PEventHandler_1__IClipboardHistoryChangedEventArgs = ^EventHandler_1__IClipboardHistoryChangedEventArgs;

  // Windows.ApplicationModel.DataTransfer.IClipboardStatics2
  IClipboardStatics2 = interface;
  PIClipboardStatics2 = ^IClipboardStatics2;

  // Windows.ApplicationModel.DataTransfer.IDataPackage2
  IDataPackage2 = interface;
  PIDataPackage2 = ^IDataPackage2;

  // Windows.ApplicationModel.DataTransfer.IShareProvider
  IShareProvider = interface;
  PIShareProvider = ^IShareProvider;

  // Windows.ApplicationModel.DataTransfer.IShareTargetInfo
  IShareTargetInfo = interface;
  PIShareTargetInfo = ^IShareTargetInfo;

  // Windows.ApplicationModel.DataTransfer.IShareCompletedEventArgs
  IShareCompletedEventArgs = interface;
  PIShareCompletedEventArgs = ^IShareCompletedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Windows.ApplicationModel.DataTransfer.IShareCompletedEventArgs>
  TypedEventHandler_2__IDataPackage__IShareCompletedEventArgs = interface;
  PTypedEventHandler_2__IDataPackage__IShareCompletedEventArgs = ^TypedEventHandler_2__IDataPackage__IShareCompletedEventArgs;

  // Windows.ApplicationModel.DataTransfer.IDataPackage3
  IDataPackage3 = interface;
  PIDataPackage3 = ^IDataPackage3;

  // Windows.ApplicationModel.DataTransfer.IDataPackage4
  IDataPackage4 = interface;
  PIDataPackage4 = ^IDataPackage4;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet2
  IDataPackagePropertySet2 = interface;
  PIDataPackagePropertySet2 = ^IDataPackagePropertySet2;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet3
  IDataPackagePropertySet3 = interface;
  PIDataPackagePropertySet3 = ^IDataPackagePropertySet3;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet4
  IDataPackagePropertySet4 = interface;
  PIDataPackagePropertySet4 = ^IDataPackagePropertySet4;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView2
  IDataPackagePropertySetView2 = interface;
  PIDataPackagePropertySetView2 = ^IDataPackagePropertySetView2;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView3
  IDataPackagePropertySetView3 = interface;
  PIDataPackagePropertySetView3 = ^IDataPackagePropertySetView3;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView4
  IDataPackagePropertySetView4 = interface;
  PIDataPackagePropertySetView4 = ^IDataPackagePropertySetView4;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView5
  IDataPackagePropertySetView5 = interface;
  PIDataPackagePropertySetView5 = ^IDataPackagePropertySetView5;

  // Windows.ApplicationModel.DataTransfer.IDataPackageView2
  IDataPackageView2 = interface;
  PIDataPackageView2 = ^IDataPackageView2;

  // Windows.ApplicationModel.DataTransfer.IDataPackageView3
  IDataPackageView3 = interface;
  PIDataPackageView3 = ^IDataPackageView3;

  // Windows.ApplicationModel.DataTransfer.IDataPackageView4
  IDataPackageView4 = interface;
  PIDataPackageView4 = ^IDataPackageView4;

  // Windows.ApplicationModel.DataTransfer.IDataRequestDeferral
  IDataRequestDeferral = interface;
  PIDataRequestDeferral = ^IDataRequestDeferral;

  // Windows.ApplicationModel.DataTransfer.IDataRequest
  IDataRequest = interface;
  PIDataRequest = ^IDataRequest;

  // Windows.ApplicationModel.DataTransfer.IDataRequestedEventArgs
  IDataRequestedEventArgs = interface;
  PIDataRequestedEventArgs = ^IDataRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.IDataRequestedEventArgs>
  TypedEventHandler_2__IDataTransferManager__IDataRequestedEventArgs = interface;
  PTypedEventHandler_2__IDataTransferManager__IDataRequestedEventArgs = ^TypedEventHandler_2__IDataTransferManager__IDataRequestedEventArgs;

  // Windows.ApplicationModel.DataTransfer.ITargetApplicationChosenEventArgs
  ITargetApplicationChosenEventArgs = interface;
  PITargetApplicationChosenEventArgs = ^ITargetApplicationChosenEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.ITargetApplicationChosenEventArgs>
  TypedEventHandler_2__IDataTransferManager__ITargetApplicationChosenEventArgs = interface;
  PTypedEventHandler_2__IDataTransferManager__ITargetApplicationChosenEventArgs = ^TypedEventHandler_2__IDataTransferManager__ITargetApplicationChosenEventArgs;

  // Windows.ApplicationModel.DataTransfer.IDataTransferManager
  IDataTransferManager = interface;
  PIDataTransferManager = ^IDataTransferManager;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IIterator_1__IShareProvider = interface;
  PIIterator_1__IShareProvider = ^IIterator_1__IShareProvider;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IIterable_1__IShareProvider = interface;
  PIIterable_1__IShareProvider = ^IIterable_1__IShareProvider;

  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IVectorView_1__IShareProvider = interface;
  PIVectorView_1__IShareProvider = ^IVectorView_1__IShareProvider;

  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IVector_1__IShareProvider = interface;
  PIVector_1__IShareProvider = ^IVector_1__IShareProvider;

  // Windows.ApplicationModel.DataTransfer.IShareProvidersRequestedEventArgs
  IShareProvidersRequestedEventArgs = interface;
  PIShareProvidersRequestedEventArgs = ^IShareProvidersRequestedEventArgs;

  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.IShareProvidersRequestedEventArgs>
  TypedEventHandler_2__IDataTransferManager__IShareProvidersRequestedEventArgs = interface;
  PTypedEventHandler_2__IDataTransferManager__IShareProvidersRequestedEventArgs = ^TypedEventHandler_2__IDataTransferManager__IShareProvidersRequestedEventArgs;

  // Windows.ApplicationModel.DataTransfer.IDataTransferManager2
  IDataTransferManager2 = interface;
  PIDataTransferManager2 = ^IDataTransferManager2;

  // Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics
  IDataTransferManagerStatics = interface;
  PIDataTransferManagerStatics = ^IDataTransferManagerStatics;

  // Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics2
  IDataTransferManagerStatics2 = interface;
  PIDataTransferManagerStatics2 = ^IDataTransferManagerStatics2;

  // Windows.ApplicationModel.DataTransfer.IShareUIOptions
  IShareUIOptions = interface;
  PIShareUIOptions = ^IShareUIOptions;

  // Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics3
  IDataTransferManagerStatics3 = interface;
  PIDataTransferManagerStatics3 = ^IDataTransferManagerStatics3;

  // Windows.ApplicationModel.DataTransfer.IHtmlFormatHelperStatics
  IHtmlFormatHelperStatics = interface;
  PIHtmlFormatHelperStatics = ^IHtmlFormatHelperStatics;

  // Windows.ApplicationModel.DataTransfer.IOperationCompletedEventArgs2
  IOperationCompletedEventArgs2 = interface;
  PIOperationCompletedEventArgs2 = ^IOperationCompletedEventArgs2;

  // Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics
  IStandardDataFormatsStatics = interface;
  PIStandardDataFormatsStatics = ^IStandardDataFormatsStatics;

  // Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics2
  IStandardDataFormatsStatics2 = interface;
  PIStandardDataFormatsStatics2 = ^IStandardDataFormatsStatics2;

  // Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics3
  IStandardDataFormatsStatics3 = interface;
  PIStandardDataFormatsStatics3 = ^IStandardDataFormatsStatics3;

  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.IDataPackage>
  AsyncOperationCompletedHandler_1__IDataPackage = interface;
  PAsyncOperationCompletedHandler_1__IDataPackage = ^AsyncOperationCompletedHandler_1__IDataPackage;

  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.IDataPackage>
  IAsyncOperation_1__IDataPackage = interface;
  PIAsyncOperation_1__IDataPackage = ^IAsyncOperation_1__IDataPackage;

  // Windows.ApplicationModel.DataTransfer Enums

  // Windows.ApplicationModel.DataTransfer.ClipboardHistoryItemsResultStatus
  ClipboardHistoryItemsResultStatus = (
    Success = 0,
    AccessDenied = 1,
    ClipboardHistoryDisabled = 2
  );
  PClipboardHistoryItemsResultStatus = ^ClipboardHistoryItemsResultStatus;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.CoreDragUIContentMode
  DragDrop_Core_CoreDragUIContentMode = (
    Auto = 0,
    Deferred = 1
  );
  PDragDrop_Core_CoreDragUIContentMode = ^DragDrop_Core_CoreDragUIContentMode;

  // Windows.ApplicationModel.DataTransfer.SetHistoryItemAsContentStatus
  SetHistoryItemAsContentStatus = (
    Success = 0,
    AccessDenied = 1,
    ItemDeleted = 2
  );
  PSetHistoryItemAsContentStatus = ^SetHistoryItemAsContentStatus;

  // Windows.ApplicationModel.DataTransfer.ShareUITheme
  ShareUITheme = (
    Default = 0,
    Light = 1,
    Dark = 2
  );
  PShareUITheme = ^ShareUITheme;

  // Windows.ApplicationModel.DataTransfer Interfaces

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.ShareTarget.IQuickLink
  ShareTarget_IQuickLink = interface(IInspectable)
  ['{603E4308-F0BE-4ADC-ACC9-8B27AB9CF556}']
    function get_Title: HSTRING; safecall;
    procedure put_Title(value: HSTRING); safecall;
    function get_Thumbnail: IRandomAccessStreamReference; safecall;
    procedure put_Thumbnail(value: IRandomAccessStreamReference); safecall;
    function get_Id: HSTRING; safecall;
    procedure put_Id(value: HSTRING); safecall;
    function get_SupportedDataFormats: IVector_1__HSTRING; safecall;
    function get_SupportedFileTypes: IVector_1__HSTRING; safecall;
    property Id: HSTRING read get_Id write put_Id;
    property SupportedDataFormats: IVector_1__HSTRING read get_SupportedDataFormats;
    property SupportedFileTypes: IVector_1__HSTRING read get_SupportedFileTypes;
    property Thumbnail: IRandomAccessStreamReference read get_Thumbnail write put_Thumbnail;
    property Title: HSTRING read get_Title write put_Title;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.ShareTarget.IShareOperation
  ShareTarget_IShareOperation = interface(IInspectable)
  ['{2246BAB8-D0F8-41C1-A82A-4137DB6504FB}']
    function get_Data: IDataPackageView; safecall;
    function get_QuickLinkId: HSTRING; safecall;
    procedure RemoveThisQuickLink; safecall;
    procedure ReportStarted; safecall;
    procedure ReportDataRetrieved; safecall;
    procedure ReportSubmittedBackgroundTask; safecall;
    procedure ReportCompleted(quicklink: ShareTarget_IQuickLink); overload; safecall;
    procedure ReportCompleted; overload; safecall;
    procedure ReportError(value: HSTRING); safecall;
    property Data: IDataPackageView read get_Data;
    property QuickLinkId: HSTRING read get_QuickLinkId;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragInfo
  DragDrop_Core_ICoreDragInfo = interface(IInspectable)
  ['{48353A8B-CB50-464E-9575-CD4E3A7AB028}']
    function get_Data: IDataPackageView; safecall;
    function get_Modifiers: DragDrop_DragDropModifiers; safecall;
    function get_Position: TPointF; safecall;
    property Data: IDataPackageView read get_Data;
    property Modifiers: DragDrop_DragDropModifiers read get_Modifiers;
    property Position: TPointF read get_Position;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragUIOverride
  DragDrop_Core_ICoreDragUIOverride = interface(IInspectable)
  ['{89A85064-3389-4F4F-8897-7E8A3FFB3C93}']
    procedure SetContentFromSoftwareBitmap(softwareBitmap: Imaging_ISoftwareBitmap); overload; safecall;
    procedure SetContentFromSoftwareBitmap(softwareBitmap: Imaging_ISoftwareBitmap; anchorPoint: TPointF); overload; safecall;
    function get_IsContentVisible: Boolean; safecall;
    procedure put_IsContentVisible(value: Boolean); safecall;
    function get_Caption: HSTRING; safecall;
    procedure put_Caption(value: HSTRING); safecall;
    function get_IsCaptionVisible: Boolean; safecall;
    procedure put_IsCaptionVisible(value: Boolean); safecall;
    function get_IsGlyphVisible: Boolean; safecall;
    procedure put_IsGlyphVisible(value: Boolean); safecall;
    procedure Clear; safecall;
    property Caption: HSTRING read get_Caption write put_Caption;
    property IsCaptionVisible: Boolean read get_IsCaptionVisible write put_IsCaptionVisible;
    property IsContentVisible: Boolean read get_IsContentVisible write put_IsContentVisible;
    property IsGlyphVisible: Boolean read get_IsGlyphVisible write put_IsGlyphVisible;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDropOperationTarget
  DragDrop_Core_ICoreDropOperationTarget = interface(IInspectable)
  ['{D9126196-4C5B-417D-BB37-76381DEF8DB4}']
    function EnterAsync(dragInfo: DragDrop_Core_ICoreDragInfo; dragUIOverride: DragDrop_Core_ICoreDragUIOverride): IAsyncOperation_1__DataPackageOperation; safecall;
    function OverAsync(dragInfo: DragDrop_Core_ICoreDragInfo; dragUIOverride: DragDrop_Core_ICoreDragUIOverride): IAsyncOperation_1__DataPackageOperation; safecall;
    function LeaveAsync(dragInfo: DragDrop_Core_ICoreDragInfo): IAsyncAction; safecall;
    function DropAsync(dragInfo: DragDrop_Core_ICoreDragInfo): IAsyncOperation_1__DataPackageOperation; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDropOperationTargetRequestedEventArgs
  DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs = interface(IInspectable)
  ['{2ACA929A-5E28-4EA6-829E-29134E665D6D}']
    procedure SetTarget(target: DragDrop_Core_ICoreDropOperationTarget); safecall;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManager,Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDropOperationTargetRequestedEventArgs>
  TypedEventHandler_2__DragDrop_Core_ICoreDragDropManager__DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{A4C3B1C1-B8AD-58CB-ACC0-8EF37EAE4ED4}']
    procedure Invoke(sender: DragDrop_Core_ICoreDragDropManager; args: DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManager,Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDropOperationTargetRequestedEventArgs>
  TypedEventHandler_2__DragDrop_Core_ICoreDragDropManager__DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs = interface(TypedEventHandler_2__DragDrop_Core_ICoreDragDropManager__DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs_Delegate_Base)
  ['{79E5B8A1-F24A-5D72-9E29-0503937617EB}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManager
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DragDrop_Core_CoreDragDropManager)]
  DragDrop_Core_ICoreDragDropManager = interface(IInspectable)
  ['{7D56D344-8464-4FAF-AA49-37EA6E2D7BD1}']
    function add_TargetRequested(value: TypedEventHandler_2__DragDrop_Core_ICoreDragDropManager__DragDrop_Core_ICoreDropOperationTargetRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_TargetRequested(value: EventRegistrationToken); safecall;
    function get_AreConcurrentOperationsEnabled: Boolean; safecall;
    procedure put_AreConcurrentOperationsEnabled(value: Boolean); safecall;
    property AreConcurrentOperationsEnabled: Boolean read get_AreConcurrentOperationsEnabled write put_AreConcurrentOperationsEnabled;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManagerStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DragDrop_Core_CoreDragDropManager)]
  DragDrop_Core_ICoreDragDropManagerStatics = interface(IInspectable)
  ['{9542FDCA-DA12-4C1C-8D06-041DB29733C3}']
    function GetForCurrentView: DragDrop_Core_ICoreDragDropManager; safecall;
  end;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragInfo2
  DragDrop_Core_ICoreDragInfo2 = interface(IInspectable)
  ['{C54691E5-E6FB-4D74-B4B1-8A3C17F25E9E}']
    function get_AllowedOperations: DataPackageOperation; safecall;
    property AllowedOperations: DataPackageOperation read get_AllowedOperations;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragOperation
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DragDrop_Core_CoreDragOperation)]
  DragDrop_Core_ICoreDragOperation = interface(IInspectable)
  ['{CC06DE4F-6DB0-4E62-AB1B-A74A02DC6D85}']
    function get_Data: IDataPackage; safecall;
    procedure SetPointerId(pointerId: Cardinal); safecall;
    procedure SetDragUIContentFromSoftwareBitmap(softwareBitmap: Imaging_ISoftwareBitmap); overload; safecall;
    procedure SetDragUIContentFromSoftwareBitmap(softwareBitmap: Imaging_ISoftwareBitmap; anchorPoint: TPointF); overload; safecall;
    function get_DragUIContentMode: DragDrop_Core_CoreDragUIContentMode; safecall;
    procedure put_DragUIContentMode(value: DragDrop_Core_CoreDragUIContentMode); safecall;
    function StartAsync: IAsyncOperation_1__DataPackageOperation; safecall;
    property Data: IDataPackage read get_Data;
    property DragUIContentMode: DragDrop_Core_CoreDragUIContentMode read get_DragUIContentMode write put_DragUIContentMode;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragOperation2
  DragDrop_Core_ICoreDragOperation2 = interface(IInspectable)
  ['{824B1E2C-D99A-4FC3-8507-6C182F33B46A}']
    function get_AllowedOperations: DataPackageOperation; safecall;
    procedure put_AllowedOperations(value: DataPackageOperation); safecall;
    property AllowedOperations: DataPackageOperation read get_AllowedOperations write put_AllowedOperations;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IClipboardContentOptions
  IClipboardContentOptions = interface(IInspectable)
  ['{E888A98C-AD4B-5447-A056-AB3556276D2B}']
    function get_IsRoamable: Boolean; safecall;
    procedure put_IsRoamable(value: Boolean); safecall;
    function get_IsAllowedInHistory: Boolean; safecall;
    procedure put_IsAllowedInHistory(value: Boolean); safecall;
    function get_RoamingFormats: IVector_1__HSTRING; safecall;
    function get_HistoryFormats: IVector_1__HSTRING; safecall;
    property HistoryFormats: IVector_1__HSTRING read get_HistoryFormats;
    property IsAllowedInHistory: Boolean read get_IsAllowedInHistory write put_IsAllowedInHistory;
    property IsRoamable: Boolean read get_IsRoamable write put_IsRoamable;
    property RoamingFormats: IVector_1__HSTRING read get_RoamingFormats;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IClipboardHistoryChangedEventArgs
  IClipboardHistoryChangedEventArgs = interface(IInspectable)
  ['{C0BE453F-8EA2-53CE-9ABA-8D2212573452}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem
  IClipboardHistoryItem = interface(IInspectable)
  ['{0173BD8A-AFFF-5C50-AB92-3D19F481EC58}']
    function get_Id: HSTRING; safecall;
    function get_Timestamp: DateTime; safecall;
    function get_Content: IDataPackageView; safecall;
    property Content: IDataPackageView read get_Content;
    property Id: HSTRING read get_Id;
    property Timestamp: DateTime read get_Timestamp;
  end;

  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem>
  IIterator_1__IClipboardHistoryItem = interface(IInspectable)
  ['{B58D4FCF-1C73-5399-BCB9-AD7290638B73}']
    function get_Current: IClipboardHistoryItem; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIClipboardHistoryItem): Cardinal; safecall;
    property Current: IClipboardHistoryItem read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;

  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem>
  IIterable_1__IClipboardHistoryItem = interface(IInspectable)
  ['{D2AA2AD5-C1D8-5982-9CC7-D091E88EC698}']
    function First: IIterator_1__IClipboardHistoryItem; safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItem>
  IVectorView_1__IClipboardHistoryItem = interface(IInspectable)
  ['{21C1FC02-1580-518D-B9EB-485EB5ED3F5E}']
    function GetAt(index: Cardinal): IClipboardHistoryItem; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IClipboardHistoryItem; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIClipboardHistoryItem): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IClipboardHistoryItemsResult
  IClipboardHistoryItemsResult = interface(IInspectable)
  ['{E6DFDEE6-0EE2-52E3-852B-F295DB65939A}']
    function get_Status: ClipboardHistoryItemsResultStatus; safecall;
    function get_Items: IVectorView_1__IClipboardHistoryItem; safecall;
    property Items: IVectorView_1__IClipboardHistoryItem read get_Items;
    property Status: ClipboardHistoryItemsResultStatus read get_Status;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IClipboardStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_Clipboard)]
  IClipboardStatics = interface(IInspectable)
  ['{C627E291-34E2-4963-8EED-93CBB0EA3D70}']
    function GetContent: IDataPackageView; safecall;
    procedure SetContent(content: IDataPackage); safecall;
    procedure Flush; safecall;
    procedure Clear; safecall;
    function add_ContentChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ContentChanged(token: EventRegistrationToken); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItemsResult>
  AsyncOperationCompletedHandler_1__IClipboardHistoryItemsResult = interface(IUnknown)
  ['{83194ABC-2E91-57AF-90F1-FAB58A0591CB}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IClipboardHistoryItemsResult; asyncStatus: AsyncStatus); safecall;
  end;

  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryItemsResult>
  IAsyncOperation_1__IClipboardHistoryItemsResult = interface(IInspectable)
  ['{3F40CA0A-E46F-54CC-9BA3-10DD5878FCA2}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IClipboardHistoryItemsResult); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IClipboardHistoryItemsResult; safecall;
    function GetResults: IClipboardHistoryItemsResult; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IClipboardHistoryItemsResult read get_Completed write put_Completed;
  end;

  // UsedAPI Interface
  // Windows.Foundation.EventHandler`1<Windows.ApplicationModel.DataTransfer.IClipboardHistoryChangedEventArgs>
  EventHandler_1__IClipboardHistoryChangedEventArgs = interface(IUnknown)
  ['{D85CBE94-0252-5179-9230-12DB3555B268}']
    procedure Invoke(sender: IInspectable; args: IClipboardHistoryChangedEventArgs); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IClipboardStatics2
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_Clipboard)]
  IClipboardStatics2 = interface(IInspectable)
  ['{D2AC1B6A-D29F-554B-B303-F0452345FE02}']
    function GetHistoryItemsAsync: IAsyncOperation_1__IClipboardHistoryItemsResult; safecall;
    function ClearHistory: Boolean; safecall;
    function DeleteItemFromHistory(item: IClipboardHistoryItem): Boolean; safecall;
    function SetHistoryItemAsContent(item: IClipboardHistoryItem): SetHistoryItemAsContentStatus; safecall;
    function IsHistoryEnabled: Boolean; safecall;
    function IsRoamingEnabled: Boolean; safecall;
    function SetContentWithOptions(content: IDataPackage; options: IClipboardContentOptions): Boolean; safecall;
    function add_HistoryChanged(handler: EventHandler_1__IClipboardHistoryChangedEventArgs): EventRegistrationToken; safecall;
    procedure remove_HistoryChanged(token: EventRegistrationToken); safecall;
    function add_RoamingEnabledChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_RoamingEnabledChanged(token: EventRegistrationToken); safecall;
    function add_HistoryEnabledChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; safecall;
    procedure remove_HistoryEnabledChanged(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataPackage2
  IDataPackage2 = interface(IInspectable)
  ['{041C1FE9-2409-45E1-A538-4C53EEEE04A7}']
    procedure SetApplicationLink(value: IUriRuntimeClass); safecall;
    procedure SetWebLink(value: IUriRuntimeClass); safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IShareProvider
  IShareProvider = interface(IInspectable)
  ['{2FABE026-443E-4CDA-AF25-8D81070EFD80}']
    function get_Title: HSTRING; safecall;
    function get_DisplayIcon: IRandomAccessStreamReference; safecall;
    function get_BackgroundColor: Color; safecall;
    function get_Tag: IInspectable; safecall;
    procedure put_Tag(value: IInspectable); safecall;
    property BackgroundColor: Color read get_BackgroundColor;
    property DisplayIcon: IRandomAccessStreamReference read get_DisplayIcon;
    property Tag: IInspectable read get_Tag write put_Tag;
    property Title: HSTRING read get_Title;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IShareTargetInfo
  IShareTargetInfo = interface(IInspectable)
  ['{385BE607-C6E8-4114-B294-28F3BB6F9904}']
    function get_AppUserModelId: HSTRING; safecall;
    function get_ShareProvider: IShareProvider; safecall;
    property AppUserModelId: HSTRING read get_AppUserModelId;
    property ShareProvider: IShareProvider read get_ShareProvider;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IShareCompletedEventArgs
  IShareCompletedEventArgs = interface(IInspectable)
  ['{4574C442-F913-4F60-9DF7-CC4060AB1916}']
    function get_ShareTarget: IShareTargetInfo; safecall;
    property ShareTarget: IShareTargetInfo read get_ShareTarget;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Windows.ApplicationModel.DataTransfer.IShareCompletedEventArgs>
  TypedEventHandler_2__IDataPackage__IShareCompletedEventArgs_Delegate_Base = interface(IUnknown)
  ['{F8F7E24A-56FE-58DF-BC15-2365AEC03966}']
    procedure Invoke(sender: IDataPackage; args: IShareCompletedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataPackage,Windows.ApplicationModel.DataTransfer.IShareCompletedEventArgs>
  TypedEventHandler_2__IDataPackage__IShareCompletedEventArgs = interface(TypedEventHandler_2__IDataPackage__IShareCompletedEventArgs_Delegate_Base)
  ['{2B4C35E5-96FD-58E2-82A0-3B40DF89065E}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataPackage3
  IDataPackage3 = interface(IInspectable)
  ['{88F31F5D-787B-4D32-965A-A9838105A056}']
    function add_ShareCompleted(handler: TypedEventHandler_2__IDataPackage__IShareCompletedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ShareCompleted(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataPackage4
  IDataPackage4 = interface(IInspectable)
  ['{13A24EC8-9382-536F-852A-3045E1B29A3B}']
    function add_ShareCanceled(handler: TypedEventHandler_2__IDataPackage__IInspectable): EventRegistrationToken; safecall;
    procedure remove_ShareCanceled(token: EventRegistrationToken); safecall;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet2
  IDataPackagePropertySet2 = interface(IInspectable)
  ['{EB505D4A-9800-46AA-B181-7B6F0F2B919A}']
    function get_ContentSourceWebLink: IUriRuntimeClass; safecall;
    procedure put_ContentSourceWebLink(value: IUriRuntimeClass); safecall;
    function get_ContentSourceApplicationLink: IUriRuntimeClass; safecall;
    procedure put_ContentSourceApplicationLink(value: IUriRuntimeClass); safecall;
    function get_PackageFamilyName: HSTRING; safecall;
    procedure put_PackageFamilyName(value: HSTRING); safecall;
    function get_Square30x30Logo: IRandomAccessStreamReference; safecall;
    procedure put_Square30x30Logo(value: IRandomAccessStreamReference); safecall;
    function get_LogoBackgroundColor: Color; safecall;
    procedure put_LogoBackgroundColor(value: Color); safecall;
    property ContentSourceApplicationLink: IUriRuntimeClass read get_ContentSourceApplicationLink write put_ContentSourceApplicationLink;
    property ContentSourceWebLink: IUriRuntimeClass read get_ContentSourceWebLink write put_ContentSourceWebLink;
    property LogoBackgroundColor: Color read get_LogoBackgroundColor write put_LogoBackgroundColor;
    property PackageFamilyName: HSTRING read get_PackageFamilyName write put_PackageFamilyName;
    property Square30x30Logo: IRandomAccessStreamReference read get_Square30x30Logo write put_Square30x30Logo;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet3
  IDataPackagePropertySet3 = interface(IInspectable)
  ['{9E87FD9B-5205-401B-874A-455653BD39E8}']
    function get_EnterpriseId: HSTRING; safecall;
    procedure put_EnterpriseId(value: HSTRING); safecall;
    property EnterpriseId: HSTRING read get_EnterpriseId write put_EnterpriseId;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySet4
  IDataPackagePropertySet4 = interface(IInspectable)
  ['{6390EBF5-1739-4C74-B22F-865FAB5E8545}']
    function get_ContentSourceUserActivityJson: HSTRING; safecall;
    procedure put_ContentSourceUserActivityJson(value: HSTRING); safecall;
    property ContentSourceUserActivityJson: HSTRING read get_ContentSourceUserActivityJson write put_ContentSourceUserActivityJson;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView2
  IDataPackagePropertySetView2 = interface(IInspectable)
  ['{6054509B-8EBE-4FEB-9C1E-75E69DE54B84}']
    function get_PackageFamilyName: HSTRING; safecall;
    function get_ContentSourceWebLink: IUriRuntimeClass; safecall;
    function get_ContentSourceApplicationLink: IUriRuntimeClass; safecall;
    function get_Square30x30Logo: IRandomAccessStreamReference; safecall;
    function get_LogoBackgroundColor: Color; safecall;
    property ContentSourceApplicationLink: IUriRuntimeClass read get_ContentSourceApplicationLink;
    property ContentSourceWebLink: IUriRuntimeClass read get_ContentSourceWebLink;
    property LogoBackgroundColor: Color read get_LogoBackgroundColor;
    property PackageFamilyName: HSTRING read get_PackageFamilyName;
    property Square30x30Logo: IRandomAccessStreamReference read get_Square30x30Logo;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView3
  IDataPackagePropertySetView3 = interface(IInspectable)
  ['{DB764CE5-D174-495C-84FC-1A51F6AB45D7}']
    function get_EnterpriseId: HSTRING; safecall;
    property EnterpriseId: HSTRING read get_EnterpriseId;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView4
  IDataPackagePropertySetView4 = interface(IInspectable)
  ['{4474C80D-D16F-40AE-9580-6F8562B94235}']
    function get_ContentSourceUserActivityJson: HSTRING; safecall;
    property ContentSourceUserActivityJson: HSTRING read get_ContentSourceUserActivityJson;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackagePropertySetView5
  IDataPackagePropertySetView5 = interface(IInspectable)
  ['{6F0A9445-3760-50BB-8523-C4202DED7D78}']
    function get_IsFromRoamingClipboard: Boolean; safecall;
    property IsFromRoamingClipboard: Boolean read get_IsFromRoamingClipboard;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackageView2
  IDataPackageView2 = interface(IInspectable)
  ['{40ECBA95-2450-4C1D-B6B4-ED45463DEE9C}']
    function GetApplicationLinkAsync: IAsyncOperation_1__IUriRuntimeClass; safecall;
    function GetWebLinkAsync: IAsyncOperation_1__IUriRuntimeClass; safecall;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackageView3
  IDataPackageView3 = interface(IInspectable)
  ['{D37771A8-DDAD-4288-8428-D1CAE394128B}']
    function RequestAccessAsync: IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function RequestAccessAsync(enterpriseId: HSTRING): IAsyncOperation_1__EnterpriseData_ProtectionPolicyEvaluationResult; overload; safecall;
    function UnlockAndAssumeEnterpriseIdentity: EnterpriseData_ProtectionPolicyEvaluationResult; safecall;
  end;

  // Windows.ApplicationModel.DataTransfer.IDataPackageView4
  IDataPackageView4 = interface(IInspectable)
  ['{DFE96F1F-E042-4433-A09F-26D6FFDA8B85}']
    procedure SetAcceptedFormatId(formatId: HSTRING); safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataRequestDeferral
  IDataRequestDeferral = interface(IInspectable)
  ['{6DC4B89F-0386-4263-87C1-ED7DCE30890E}']
    procedure Complete; safecall;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataRequest
  IDataRequest = interface(IInspectable)
  ['{4341AE3B-FC12-4E53-8C02-AC714C415A27}']
    function get_Data: IDataPackage; safecall;
    procedure put_Data(value: IDataPackage); safecall;
    function get_Deadline: DateTime; safecall;
    procedure FailWithDisplayText(value: HSTRING); safecall;
    function GetDeferral: IDataRequestDeferral; safecall;
    property Data: IDataPackage read get_Data write put_Data;
    property Deadline: DateTime read get_Deadline;
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataRequestedEventArgs
  IDataRequestedEventArgs = interface(IInspectable)
  ['{CB8BA807-6AC5-43C9-8AC5-9BA232163182}']
    function get_Request: IDataRequest; safecall;
    property Request: IDataRequest read get_Request;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.IDataRequestedEventArgs>
  TypedEventHandler_2__IDataTransferManager__IDataRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{EC6F9CC8-46D0-5E0E-B4D2-7D7773AE37A0}']
    procedure Invoke(sender: IDataTransferManager; args: IDataRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.IDataRequestedEventArgs>
  TypedEventHandler_2__IDataTransferManager__IDataRequestedEventArgs = interface(TypedEventHandler_2__IDataTransferManager__IDataRequestedEventArgs_Delegate_Base)
  ['{041062FC-4655-55C7-A13F-1153F9CA6E51}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.ITargetApplicationChosenEventArgs
  ITargetApplicationChosenEventArgs = interface(IInspectable)
  ['{CA6FB8AC-2987-4EE3-9C54-D8AFBCB86C1D}']
    function get_ApplicationName: HSTRING; safecall;
    property ApplicationName: HSTRING read get_ApplicationName;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.ITargetApplicationChosenEventArgs>
  TypedEventHandler_2__IDataTransferManager__ITargetApplicationChosenEventArgs_Delegate_Base = interface(IUnknown)
  ['{C4AC1BA2-7851-5A44-BC8D-3D7C713F1F41}']
    procedure Invoke(sender: IDataTransferManager; args: ITargetApplicationChosenEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.ITargetApplicationChosenEventArgs>
  TypedEventHandler_2__IDataTransferManager__ITargetApplicationChosenEventArgs = interface(TypedEventHandler_2__IDataTransferManager__ITargetApplicationChosenEventArgs_Delegate_Base)
  ['{A3639038-7CA2-5BF9-9657-DDBB0B7C6142}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataTransferManager
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DataTransferManager)]
  IDataTransferManager = interface(IInspectable)
  ['{A5CAEE9B-8708-49D1-8D36-67D25A8DA00C}']
    function add_DataRequested(eventHandler: TypedEventHandler_2__IDataTransferManager__IDataRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_DataRequested(eventCookie: EventRegistrationToken); safecall;
    function add_TargetApplicationChosen(eventHandler: TypedEventHandler_2__IDataTransferManager__ITargetApplicationChosenEventArgs): EventRegistrationToken; safecall;
    procedure remove_TargetApplicationChosen(eventCookie: EventRegistrationToken); safecall;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IIterator_1__IShareProvider_Base = interface(IInspectable)
  ['{886F5642-E9F9-573B-9213-5840B5062B40}']
    function get_Current: IShareProvider; safecall;
    function get_HasCurrent: Boolean; safecall;
    function MoveNext: Boolean; safecall;
    function GetMany(itemsSize: Cardinal; items: PIShareProvider): Cardinal; safecall;
    property Current: IShareProvider read get_Current;
    property HasCurrent: Boolean read get_HasCurrent;
  end;
  // Windows.Foundation.Collections.IIterator`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IIterator_1__IShareProvider = interface(IIterator_1__IShareProvider_Base)
  ['{D7A2A7A2-254A-55DA-AE8F-EDF525A5036A}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IIterable_1__IShareProvider_Base = interface(IInspectable)
  ['{0903B218-5CAD-53E6-9A21-6F4B31C4A409}']
    function First: IIterator_1__IShareProvider; safecall;
  end;
  // Windows.Foundation.Collections.IIterable`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IIterable_1__IShareProvider = interface(IIterable_1__IShareProvider_Base)
  ['{3C8AA09E-5CC9-5F78-AE2E-04FB0E8C6C48}']
  end;

  // UsedAPI Interface
  // Windows.Foundation.Collections.IVectorView`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IVectorView_1__IShareProvider = interface(IInspectable)
  ['{82017F36-CFE0-5FE3-8AB0-6F0055957FB4}']
    function GetAt(index: Cardinal): IShareProvider; safecall;
    function get_Size: Cardinal; safecall;
    function IndexOf(value: IShareProvider; out index: Cardinal): Boolean; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIShareProvider): Cardinal; safecall;
    property Size: Cardinal read get_Size;
  end;

  // Generic Ancestor for:
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IVector_1__IShareProvider_Base = interface(IInspectable)
  ['{A1687865-31E2-5536-97EC-292269A78046}']
    function GetAt(index: Cardinal): IShareProvider; safecall;
    function get_Size: Cardinal; safecall;
    function GetView: IVectorView_1__IShareProvider; safecall;
    function IndexOf(value: IShareProvider; out index: Cardinal): Boolean; safecall;
    procedure SetAt(index: Cardinal; value: IShareProvider); safecall;
    procedure InsertAt(index: Cardinal; value: IShareProvider); safecall;
    procedure RemoveAt(index: Cardinal); safecall;
    procedure Append(value: IShareProvider); safecall;
    procedure RemoveAtEnd; safecall;
    procedure Clear; safecall;
    function GetMany(startIndex: Cardinal; itemsSize: Cardinal; items: PIShareProvider): Cardinal; safecall;
    procedure ReplaceAll(itemsSize: Cardinal; items: PIShareProvider); safecall;
    property Size: Cardinal read get_Size;
  end;
  // UsedAPI Interface
  // Windows.Foundation.Collections.IVector`1<Windows.ApplicationModel.DataTransfer.IShareProvider>
  IVector_1__IShareProvider = interface(IVector_1__IShareProvider_Base)
  ['{2F775C24-EE1E-509C-A4D0-CCC66B046507}']
  end;

  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IShareProvidersRequestedEventArgs
  IShareProvidersRequestedEventArgs = interface(IInspectable)
  ['{F888F356-A3F8-4FCE-85E4-8826E63BE799}']
    function get_Providers: IVector_1__IShareProvider; safecall;
    function get_Data: IDataPackageView; safecall;
    function GetDeferral: IDeferral; safecall;
    property Data: IDataPackageView read get_Data;
    property Providers: IVector_1__IShareProvider read get_Providers;
  end;

  // Generic Delegate for:
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.IShareProvidersRequestedEventArgs>
  TypedEventHandler_2__IDataTransferManager__IShareProvidersRequestedEventArgs_Delegate_Base = interface(IUnknown)
  ['{147E2860-7397-582F-80DB-B8685383A937}']
    procedure Invoke(sender: IDataTransferManager; args: IShareProvidersRequestedEventArgs); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.TypedEventHandler`2<Windows.ApplicationModel.DataTransfer.IDataTransferManager,Windows.ApplicationModel.DataTransfer.IShareProvidersRequestedEventArgs>
  TypedEventHandler_2__IDataTransferManager__IShareProvidersRequestedEventArgs = interface(TypedEventHandler_2__IDataTransferManager__IShareProvidersRequestedEventArgs_Delegate_Base)
  ['{AD4E35F1-51EC-5CC1-854F-87BDBB80C810}']
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataTransferManager2
  IDataTransferManager2 = interface(IInspectable)
  ['{30AE7D71-8BA8-4C02-8E3F-DDB23B388715}']
    function add_ShareProvidersRequested(handler: TypedEventHandler_2__IDataTransferManager__IShareProvidersRequestedEventArgs): EventRegistrationToken; safecall;
    procedure remove_ShareProvidersRequested(token: EventRegistrationToken); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DataTransferManager)]
  IDataTransferManagerStatics = interface(IInspectable)
  ['{A9DA01AA-E00E-4CFE-AA44-2DD932DCA3D8}']
    procedure ShowShareUI; safecall;
    function GetForCurrentView: IDataTransferManager; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics2
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DataTransferManager)]
  IDataTransferManagerStatics2 = interface(IInspectable)
  ['{C54EC2EC-9F97-4D63-9868-395E271AD8F5}']
    function IsSupported: Boolean; safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IShareUIOptions
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_ShareUIOptions)]
  IShareUIOptions = interface(IInspectable)
  ['{72FA8A80-342F-4D90-9551-2AE04E37680C}']
    function get_Theme: ShareUITheme; safecall;
    procedure put_Theme(value: ShareUITheme); safecall;
    function get_SelectionRect: IReference_1__Rect; safecall;
    procedure put_SelectionRect(value: IReference_1__Rect); safecall;
    property SelectionRect: IReference_1__Rect read get_SelectionRect write put_SelectionRect;
    property Theme: ShareUITheme read get_Theme write put_Theme;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics3
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_DataTransferManager)]
  IDataTransferManagerStatics3 = interface(IInspectable)
  ['{05845473-6C82-4F5C-AC23-62E458361FAC}']
    procedure ShowShareUI(options: IShareUIOptions); safecall;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IHtmlFormatHelperStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_HtmlFormatHelper)]
  IHtmlFormatHelperStatics = interface(IInspectable)
  ['{E22E7749-DD70-446F-AEFC-61CEE59F655E}']
    function GetStaticFragment(htmlFormat: HSTRING): HSTRING; safecall;
    function CreateHtmlFormat(htmlFragment: HSTRING): HSTRING; safecall;
  end;

  // Windows.ApplicationModel.DataTransfer.IOperationCompletedEventArgs2
  IOperationCompletedEventArgs2 = interface(IInspectable)
  ['{858FA073-1E19-4105-B2F7-C8478808D562}']
    function get_AcceptedFormatId: HSTRING; safecall;
    property AcceptedFormatId: HSTRING read get_AcceptedFormatId;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_StandardDataFormats)]
  IStandardDataFormatsStatics = interface(IInspectable)
  ['{7ED681A1-A880-40C9-B4ED-0BEE1E15F549}']
    function get_Text: HSTRING; safecall;
    function get_Uri: HSTRING; safecall;
    function get_Html: HSTRING; safecall;
    function get_Rtf: HSTRING; safecall;
    function get_Bitmap: HSTRING; safecall;
    function get_StorageItems: HSTRING; safecall;
    property Bitmap: HSTRING read get_Bitmap;
    property Html: HSTRING read get_Html;
    property Rtf: HSTRING read get_Rtf;
    property StorageItems: HSTRING read get_StorageItems;
    property Text: HSTRING read get_Text;
    property Uri: HSTRING read get_Uri;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics2
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_StandardDataFormats)]
  IStandardDataFormatsStatics2 = interface(IInspectable)
  ['{42A254F4-9D76-42E8-861B-47C25DD0CF71}']
    function get_WebLink: HSTRING; safecall;
    function get_ApplicationLink: HSTRING; safecall;
    property ApplicationLink: HSTRING read get_ApplicationLink;
    property WebLink: HSTRING read get_WebLink;
  end;

  // DualAPI Interface
  // UsedAPI Interface
  // Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics3
  [WinRTClassNameAttribute(SWindows_ApplicationModel_DataTransfer_StandardDataFormats)]
  IStandardDataFormatsStatics3 = interface(IInspectable)
  ['{3B57B069-01D4-474C-8B5F-BC8E27F38B21}']
    function get_UserActivityJsonArray: HSTRING; safecall;
    property UserActivityJsonArray: HSTRING read get_UserActivityJsonArray;
  end;

  // Generic Delegate for:
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.IDataPackage>
  AsyncOperationCompletedHandler_1__IDataPackage_Delegate_Base = interface(IUnknown)
  ['{A93A3B99-E946-57CE-AAD9-C23D138C353E}']
    procedure Invoke(asyncInfo: IAsyncOperation_1__IDataPackage; asyncStatus: AsyncStatus); safecall;
  end;
  // UsedAPI Interface
  // Windows.Foundation.AsyncOperationCompletedHandler`1<Windows.ApplicationModel.DataTransfer.IDataPackage>
  AsyncOperationCompletedHandler_1__IDataPackage = interface(AsyncOperationCompletedHandler_1__IDataPackage_Delegate_Base)
  ['{926269A5-BCC6-5C70-90E0-79EAF8ECBABC}']
  end;

  // Generic Ancestor for:
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.IDataPackage>
  IAsyncOperation_1__IDataPackage_Base = interface(IInspectable)
  ['{A16F2D07-EAD3-53E4-9490-75BDBAEB7A5B}']
    procedure put_Completed(handler: AsyncOperationCompletedHandler_1__IDataPackage); safecall;
    function get_Completed: AsyncOperationCompletedHandler_1__IDataPackage; safecall;
    function GetResults: IDataPackage; safecall;
    property Completed: AsyncOperationCompletedHandler_1__IDataPackage read get_Completed write put_Completed;
  end;
  // UsedAPI Interface
  // Windows.Foundation.IAsyncOperation`1<Windows.ApplicationModel.DataTransfer.IDataPackage>
  IAsyncOperation_1__IDataPackage = interface(IAsyncOperation_1__IDataPackage_Base)
  ['{F5B89375-4322-58CD-9F18-DBCF55147BF3}']
  end;

  // Windows.ApplicationModel.DataTransfer.Clipboard
  // DualAPI
  // Statics: "Windows.ApplicationModel.DataTransfer.IClipboardStatics"
  // Statics: "Windows.ApplicationModel.DataTransfer.IClipboardStatics2"
  TClipboard = class(TWinRTGenericImportS2<IClipboardStatics, IClipboardStatics2>)
  public
    // -> IClipboardStatics
    class function GetContent: IDataPackageView; static; inline;
    class procedure SetContent(content: IDataPackage); static; inline;
    class procedure Flush; static; inline;
    class procedure Clear; static; inline;
    class function add_ContentChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_ContentChanged(token: EventRegistrationToken); static; inline;

    // -> IClipboardStatics2
    class function GetHistoryItemsAsync: IAsyncOperation_1__IClipboardHistoryItemsResult; static; inline;
    class function ClearHistory: Boolean; static; inline;
    class function DeleteItemFromHistory(item: IClipboardHistoryItem): Boolean; static; inline;
    class function SetHistoryItemAsContent(item: IClipboardHistoryItem): SetHistoryItemAsContentStatus; static; inline;
    class function IsHistoryEnabled: Boolean; static; inline;
    class function IsRoamingEnabled: Boolean; static; inline;
    class function SetContentWithOptions(content: IDataPackage; options: IClipboardContentOptions): Boolean; static; inline;
    class function add_HistoryChanged(handler: EventHandler_1__IClipboardHistoryChangedEventArgs): EventRegistrationToken; static; inline;
    class procedure remove_HistoryChanged(token: EventRegistrationToken); static; inline;
    class function add_RoamingEnabledChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_RoamingEnabledChanged(token: EventRegistrationToken); static; inline;
    class function add_HistoryEnabledChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken; static; inline;
    class procedure remove_HistoryEnabledChanged(token: EventRegistrationToken); static; inline;
  end;

  // Windows.ApplicationModel.DataTransfer.DataPackage
  // DualAPI
  // Implements: Windows.ApplicationModel.DataTransfer.IDataPackage
  // Implements: Windows.ApplicationModel.DataTransfer.IDataPackage2
  // Implements: Windows.ApplicationModel.DataTransfer.IDataPackage3
  // Implements: Windows.ApplicationModel.DataTransfer.IDataPackage4
  // Instantiable: "IDataPackage"
  TDataPackage = class(TWinRTGenericImportI<IDataPackage>) end;

  // Windows.ApplicationModel.DataTransfer.DataTransferManager
  // Explicitly imported
  // Implements: Windows.ApplicationModel.DataTransfer.IDataTransferManager
  // Implements: Windows.ApplicationModel.DataTransfer.IDataTransferManager2
  // Statics: "Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics"
  // Statics: "Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics2"
  // Statics: "Windows.ApplicationModel.DataTransfer.IDataTransferManagerStatics3"
  // Interop Intf: "IDataTransferManagerInterop"
  IDataTransferManagerInterop = interface(IUnknown)
    ['{3A3DCD6C-3EAB-43DC-BCDE-45671CE800C8}']
    function GetForWindow(appWindow: THandle; const riid: TGUID): IDataTransferManager; safecall;
    procedure ShowShareUIForWindow(appWindow: THandle); safecall;
  end;
  TDataTransferManager = class(TWinRTGenericImportS3O<IDataTransferManagerStatics, IDataTransferManagerStatics2, IDataTransferManagerStatics3, IDataTransferManagerInterop>)
  public
    // -> IDataTransferManagerStatics
    class procedure ShowShareUI; overload; static; inline;
    class function GetForCurrentView: IDataTransferManager; static; inline;

    // -> IDataTransferManagerStatics2
    class function IsSupported: Boolean; static; inline;

    // -> IDataTransferManagerStatics3
    class procedure ShowShareUI(options: IShareUIOptions); overload; static; inline;
  end;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.CoreDragDropManager
  // DualAPI
  // Implements: Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManager
  // Statics: "Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragDropManagerStatics"
  TDragDrop_Core_CoreDragDropManager = class(TWinRTGenericImportS<DragDrop_Core_ICoreDragDropManagerStatics>)
  public
    // -> DragDrop_Core_ICoreDragDropManagerStatics
    class function GetForCurrentView: DragDrop_Core_ICoreDragDropManager; static; inline;
  end;

  // Windows.ApplicationModel.DataTransfer.DragDrop.Core.CoreDragOperation
  // DualAPI
  // Implements: Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragOperation
  // Implements: Windows.ApplicationModel.DataTransfer.DragDrop.Core.ICoreDragOperation2
  // Instantiable: "DragDrop_Core_ICoreDragOperation"
  TDragDrop_Core_CoreDragOperation = class(TWinRTGenericImportI<DragDrop_Core_ICoreDragOperation>) end;

  // Windows.ApplicationModel.DataTransfer.HtmlFormatHelper
  // DualAPI
  // Statics: "Windows.ApplicationModel.DataTransfer.IHtmlFormatHelperStatics"
  THtmlFormatHelper = class(TWinRTGenericImportS<IHtmlFormatHelperStatics>)
  public
    // -> IHtmlFormatHelperStatics
    class function GetStaticFragment(htmlFormat: HSTRING): HSTRING; static; inline;
    class function CreateHtmlFormat(htmlFragment: HSTRING): HSTRING; static; inline;
  end;

  // Windows.ApplicationModel.DataTransfer.ShareUIOptions
  // DualAPI
  // Implements: Windows.ApplicationModel.DataTransfer.IShareUIOptions
  // Instantiable: "IShareUIOptions"
  TShareUIOptions = class(TWinRTGenericImportI<IShareUIOptions>) end;

  // Windows.ApplicationModel.DataTransfer.StandardDataFormats
  // DualAPI
  // Statics: "Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics"
  // Statics: "Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics2"
  // Statics: "Windows.ApplicationModel.DataTransfer.IStandardDataFormatsStatics3"
  TStandardDataFormats = class(TWinRTGenericImportS3<IStandardDataFormatsStatics, IStandardDataFormatsStatics2, IStandardDataFormatsStatics3>)
  public
    // -> IStandardDataFormatsStatics
    class function get_Text: HSTRING; static; inline;
    class function get_Uri: HSTRING; static; inline;
    class function get_Html: HSTRING; static; inline;
    class function get_Rtf: HSTRING; static; inline;
    class function get_Bitmap: HSTRING; static; inline;
    class function get_StorageItems: HSTRING; static; inline;
    class property Bitmap: HSTRING read get_Bitmap;
    class property Html: HSTRING read get_Html;
    class property Rtf: HSTRING read get_Rtf;
    class property StorageItems: HSTRING read get_StorageItems;
    class property Text: HSTRING read get_Text;
    class property Uri: HSTRING read get_Uri;

    // -> IStandardDataFormatsStatics2
    class function get_WebLink: HSTRING; static; inline;
    class function get_ApplicationLink: HSTRING; static; inline;
    class property ApplicationLink: HSTRING read get_ApplicationLink;
    class property WebLink: HSTRING read get_WebLink;

    // -> IStandardDataFormatsStatics3
    class function get_UserActivityJsonArray: HSTRING; static; inline;
    class property UserActivityJsonArray: HSTRING read get_UserActivityJsonArray;
  end;

implementation

{ TClipboard }

class function TClipboard.GetContent: IDataPackageView;
begin
  Result := Statics.GetContent;
end;

class procedure TClipboard.SetContent(content: IDataPackage);
begin
  Statics.SetContent(content);
end;

class procedure TClipboard.Flush;
begin
  Statics.Flush;
end;

class procedure TClipboard.Clear;
begin
  Statics.Clear;
end;

class function TClipboard.add_ContentChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics.add_ContentChanged(handler);
end;

class procedure TClipboard.remove_ContentChanged(token: EventRegistrationToken);
begin
  Statics.remove_ContentChanged(token);
end;


class function TClipboard.GetHistoryItemsAsync: IAsyncOperation_1__IClipboardHistoryItemsResult;
begin
  Result := Statics2.GetHistoryItemsAsync;
end;

class function TClipboard.ClearHistory: Boolean;
begin
  Result := Statics2.ClearHistory;
end;

class function TClipboard.DeleteItemFromHistory(item: IClipboardHistoryItem): Boolean;
begin
  Result := Statics2.DeleteItemFromHistory(item);
end;

class function TClipboard.SetHistoryItemAsContent(item: IClipboardHistoryItem): SetHistoryItemAsContentStatus;
begin
  Result := Statics2.SetHistoryItemAsContent(item);
end;

class function TClipboard.IsHistoryEnabled: Boolean;
begin
  Result := Statics2.IsHistoryEnabled;
end;

class function TClipboard.IsRoamingEnabled: Boolean;
begin
  Result := Statics2.IsRoamingEnabled;
end;

class function TClipboard.SetContentWithOptions(content: IDataPackage; options: IClipboardContentOptions): Boolean;
begin
  Result := Statics2.SetContentWithOptions(content, options);
end;

class function TClipboard.add_HistoryChanged(handler: EventHandler_1__IClipboardHistoryChangedEventArgs): EventRegistrationToken;
begin
  Result := Statics2.add_HistoryChanged(handler);
end;

class procedure TClipboard.remove_HistoryChanged(token: EventRegistrationToken);
begin
  Statics2.remove_HistoryChanged(token);
end;

class function TClipboard.add_RoamingEnabledChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics2.add_RoamingEnabledChanged(handler);
end;

class procedure TClipboard.remove_RoamingEnabledChanged(token: EventRegistrationToken);
begin
  Statics2.remove_RoamingEnabledChanged(token);
end;

class function TClipboard.add_HistoryEnabledChanged(handler: EventHandler_1__IInspectable): EventRegistrationToken;
begin
  Result := Statics2.add_HistoryEnabledChanged(handler);
end;

class procedure TClipboard.remove_HistoryEnabledChanged(token: EventRegistrationToken);
begin
  Statics2.remove_HistoryEnabledChanged(token);
end;


{ TDataPackage }

{ TDataTransferManager }

class procedure TDataTransferManager.ShowShareUI;
begin
  Statics.ShowShareUI;
end;

class function TDataTransferManager.GetForCurrentView: IDataTransferManager;
begin
  Result := Statics.GetForCurrentView;
end;


class function TDataTransferManager.IsSupported: Boolean;
begin
  Result := Statics2.IsSupported;
end;


class procedure TDataTransferManager.ShowShareUI(options: IShareUIOptions);
begin
  Statics3.ShowShareUI(options);
end;


{ TDragDrop_Core_CoreDragDropManager }

class function TDragDrop_Core_CoreDragDropManager.GetForCurrentView: DragDrop_Core_ICoreDragDropManager;
begin
  Result := Statics.GetForCurrentView;
end;


{ TDragDrop_Core_CoreDragOperation }

{ THtmlFormatHelper }

class function THtmlFormatHelper.GetStaticFragment(htmlFormat: HSTRING): HSTRING;
begin
  Result := Statics.GetStaticFragment(htmlFormat);
end;

class function THtmlFormatHelper.CreateHtmlFormat(htmlFragment: HSTRING): HSTRING;
begin
  Result := Statics.CreateHtmlFormat(htmlFragment);
end;


{ TShareUIOptions }

{ TStandardDataFormats }

class function TStandardDataFormats.get_Text: HSTRING;
begin
  Result := Statics.get_Text;
end;

class function TStandardDataFormats.get_Uri: HSTRING;
begin
  Result := Statics.get_Uri;
end;

class function TStandardDataFormats.get_Html: HSTRING;
begin
  Result := Statics.get_Html;
end;

class function TStandardDataFormats.get_Rtf: HSTRING;
begin
  Result := Statics.get_Rtf;
end;

class function TStandardDataFormats.get_Bitmap: HSTRING;
begin
  Result := Statics.get_Bitmap;
end;

class function TStandardDataFormats.get_StorageItems: HSTRING;
begin
  Result := Statics.get_StorageItems;
end;


class function TStandardDataFormats.get_WebLink: HSTRING;
begin
  Result := Statics2.get_WebLink;
end;

class function TStandardDataFormats.get_ApplicationLink: HSTRING;
begin
  Result := Statics2.get_ApplicationLink;
end;


class function TStandardDataFormats.get_UserActivityJsonArray: HSTRING;
begin
  Result := Statics3.get_UserActivityJsonArray;
end;


end.
