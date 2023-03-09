{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit ToolsAPI.WelcomePage;

interface

uses
  System.SysUtils, System.Classes, System.Types, Vcl.Forms, Vcl.Controls, Vcl.Menus,
  Vcl.ImageCollection, Vcl.Graphics;

(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageNotifier280,0xCD8AA3EB,0xAB59,0x47C2,0x92,0x64,0xA6,0x14,0x73,0x69,0xB9,0xA6);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageNotifier,0x1007377B,0x1266,0x47D5,0xAB,0xEA,0xAC,0xDE,0x94,0x2C,0xD6,0xE9);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePagePluginNotifier280,0xB157D07D,0x3332,0x41E6,0x85,0xA9,0x5E,0x01,0x21,0x0F,0x38,0x56);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePagePluginNotifier,0x38B3EA34,0xD832,0x48D0,0xAF,0x84,0x74,0x49,0xEB,0x28,0xD2,0x21);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageModelItemData,0x7800EF34,0xDBFC,0x487E,0x87,0x9C,0x9E,0xFC,0xB6,0xEA,0x42,0xB1);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageModelAdjustedItemData280,0xB28DD62B,0x2FF8,0x4D28,0x88,0xB0,0x7E,0x8A,0x04,0xD1,0xFF,0x33);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageModelAdjustedItemData,0x7DA748EA,0x7E34,0x4D58,0x80,0xDF,0x48,0x07,0xE5,0x76,0x3B,0xAD);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageModelItemStateData,0x5F3B4229,0x0E29,0x4867,0x9B,0x6C,0x56,0x8B,0x63,0x72,0x09,0xC4);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageModelItemAdditionalData, 0x6BC68517,0xA109,0x4D1D,0xA5,0xA9,0xB6,0x5F,0x46,0x39,0x45,0xFF);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePagePluginModel,0x75357ABC,0xF773,0x498C,0x99,0x70,0xCC,0x63,0x57,0x71,0xE6,0xDA);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePagePluginOnlineModel,0xDE75BB76,0xA656,0x4C76,0x9C,0xD0,0xBD,0x02,0xCC,0xB6,0x36,0x95);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePagePlugin,0x60AB2EA1,0x9521,0x4A7C,0xBE,0x7F,0x12,0x53,0x08,0xAF,0x20,0xE5);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageDataPlugin,0xA4E90C8F,0x9AA0,0x481E,0xB4,0x02,0x36,0x32,0x9B,0x38,0xF8,0xBC);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageDataPluginControlState,0xCD335CEE,0xA7E9,0x4E2B,0xB0,0xCC,0xA6,0x26,0xEA,0xED,0xE8,0xCB);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageCaptionFrame,0x2E6BCE6D,0x60BB,0x42C5,0xB2,0x4A,0x30,0xC4,0x09,0x76,0xF2,0x02);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageDataPluginListView,0x6213F973,0xE077,0x4647,0x9E,0x4E,0xD1,0x06,0x66,0xC3,0x0B,0x26);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageContentPluginCreator,0x1E2E6E47,0xB4C5,0x49A6,0x80,0x18,0x93,0x8B,0x5C,0xC1,0xED,0xFE);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageControlPluginCreator ,0xED9D44C5,0x0131,0x4C7C,0xB0,0xEE,0x19,0x20,0x91,0x12,0xA1,0xAE);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePagePluginService280,0xAABD143E,0x44EB,0x4070,0x83,0x4B,0xD0,0x00,0x2E,0x12,0xEE,0x01);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePagePluginService,0xCFDFE3BD,0xB3BA,0x4D27,0xBE,0xC0,0x81,0x30,0xE4,0xA7,0x87,0x4B);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageSettings280,0xD5935926,0x8F43,0x4F49,0x9E,0xDB,0x27,0xF4,0x92,0x13,0x52,0x54);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageSettings,0xB10CD61C,0x4137,0x430F,0xA6,0x67,0xFA,0xCA,0x16,0x5B,0xE8,0x16);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageBackgroundService280,0x7AA097E9,0x5932,0x4224,0xA9,0x96,0x4F,0x27,0x17,0x79,0xB0,0xFB);'*)
(*$HPPEMIT 'DEFINE_GUID(IID_INTAWelcomePageBackgroundService,0x996AC521,0x40D2,0x4FA4,0x94,0xA8,0xE9,0xF2,0xEB,0x16,0x19,0x9D);'*)

type
  TOnPluginItemClickEvent = procedure(Sender: TObject; ItemIndex: Integer) of object;
  TOnSetDataEvent = procedure(const AData: IInterfaceList) of object;
  TOnReceiveDataEvent = procedure(var Abort: Boolean) of object;
  TOnPluginRegisteredEvent = procedure(const PluginID: string) of object;

  EWelcomePageException = class(Exception);

  /// <summary>
  /// Mode of Plugin View:
  ///  vmNone - without view (use for control handlers)
  ///  vmListShort - default list with Image, Title and Description
  ///  vmListButton - the same as vmListShort and Button on right side
  ///  vmListDropDown - the same as vmListShort and Button with DropDown on right side
  ///  vmListFull - the same as vmListShort and DropDown Button and Button on right side
  /// </summary>
  TWelcomePageViewMode = (vmNone, vmListShort, vmListButton, vmListDropDown, vmListFull);

  /// <summary>
  /// Interface for notify Welcome Page about closing or destroying plugins
  /// </summary>
  INTAWelcomePageNotifier280 = interface(IUnknown)
    ['{CD8AA3EB-AB59-47C2-9264-A6147369B9A6}']
    /// <summary>
    /// Event before Welcome Page is about to free
    /// </summary>
    procedure BeforeWelcomePageDestroy;
    /// <summary>
    /// Event before Plugin is about to close
    /// </summary>
    procedure BeforePluginClose(const PluginID: string);
    /// <summary>
    /// Event before Plugin is about to free
    /// </summary>
    procedure BeforePluginDestroy(const PluginID: string);
  end;

  INTAWelcomePageNotifier = interface(INTAWelcomePageNotifier280)
    ['{1007377B-1266-47D5-ABEA-ACDE942CD6E9}']
  end;

  /// <summary>
  /// Interface for notify Welcome Page Plugin about showing, closing or resizing
  /// </summary>
  INTAWelcomePagePluginNotifier280 = interface(IUnknown)
    ['{B157D07D-3332-41E6-85A9-5E01210F3856}']
    /// <summary>
    /// Event to display main Plugin View (the equivalent of OnShow)
    /// </summary>
    procedure ViewShow(Sender: TWinControl);
    /// <summary>
    /// Event to hide/close the main Plugin View
    /// </summary>
    procedure ViewHide;
    /// <summary>
    /// Event when the main Plugin View size change
    /// </summary>
    procedure ViewResize(AColumnSpan, ARowSpan: Integer);
  end;

  INTAWelcomePagePluginNotifier = interface(INTAWelcomePagePluginNotifier280)
    ['{38B3EA34-D832-48D0-AF84-7449EB28D221}']
  end;

  /// <summary>
  /// Interface to provide access to main info of item data
  /// </summary>
  INTAWelcomePageModelItemData = interface
    ['{7800EF34-DBFC-487E-879C-9EFCB6EA42B1}']
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);

    /// <summary>
    /// Title on plugin data item
    /// </summary>
    property Title: string read GetTitle write SetTitle;
    /// <summary>
    /// Description on plugin data item
    /// </summary>
    property Description: string read GetDescription write SetDescription;
    /// <summary>
    /// Image index on main image of plugin data item
    /// </summary>
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
  end;

  /// <summary>
  /// Interface to adjust item data before access
  /// </summary>
  INTAWelcomePageModelAdjustedItemData280 = interface
    ['{B28DD62B-2FF8-4D28-88B0-7E8A04D1FF33}']
    /// <summary>
    /// Provide access to adjusted description on plugin data item
    /// </summary>
    function GetAdjustedDescription(ACanvas: TCanvas; ARect: TRect): string;
  end;

  INTAWelcomePageModelAdjustedItemData = interface(INTAWelcomePageModelAdjustedItemData280)
    ['{7DA748EA-7E34-4D58-80DF-4807E5763BAD}']
  end;

  /// <summary>
  /// Interface to provide state of item data
  /// </summary>
  INTAWelcomePageModelItemStateData = interface
    ['{5F3B4229-0E29-4867-9B6C-568B637209C4}']
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);

    /// <summary>
    /// Enabled state of plugin data item
    /// </summary>
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  /// <summary>
  /// Interface to provide access to additional info of item data
  /// </summary>
  INTAWelcomePageModelItemAdditionalData = interface
    ['{6BC68517-A109-4D1D-A5A9-B65F463945FF}']
    function GetAdditionalImageIndex: Integer;
    procedure SetAdditionalImageIndex(const Value: Integer);

    /// <summary>
    /// Image index for additional button on plugin data item
    /// </summary>
    property AdditionalImageIndex: Integer read GetAdditionalImageIndex write SetAdditionalImageIndex;
  end;

  /// <summary>
  /// Interface to provide access to data model
  /// </summary>
  INTAWelcomePagePluginModel = interface
    ['{75357ABC-F773-498C-9970-CC635771E6DA}']
    function GetData: IInterfaceList;
    function GetStatusMessage: string;
    function GetImageCollection: TImageCollection;
    function GetIsDataLoaded: Boolean;

    /// <summary>
    /// Method to load data
    /// </summary>
    procedure LoadData;
    /// <summary>
    /// Method to refresh data
    /// </summary>
    procedure RefreshData;
    /// <summary>
    /// Method to stop loading data
    /// </summary>
    procedure StopLoading;
    /// <summary>
    /// Method to set callback procedure after loading data
    /// </summary>
    procedure SetOnLoadingFinished(AProc: TProc);

    /// <summary>
    /// Returns model data
    /// </summary>
    property Data: IInterfaceList read GetData;
    /// <summary>
    /// Returns last status message
    /// </summary>
    property StatusMessage: string read GetStatusMessage;
    /// <summary>
    /// Returns ImageCollection with images linked with items
    /// </summary>
    property ImageCollection: TImageCollection read GetImageCollection;
    /// <summary>
    /// Returns the completion status of loading data
    /// </summary>
    property IsDataLoaded: Boolean read GetIsDataLoaded;
  end;

  /// <summary>
  /// Extends INTAWelcomePagePluginModel interface to provide access to data model
  ///  with data from online services
  /// </summary>
  INTAWelcomePagePluginOnlineModel = interface(INTAWelcomePagePluginModel)
    ['{DE75BB76-A656-4C76-9CD0-BD02CCB63695}']
    function GetServiceUrl: string;
    procedure SetServiceUrl(const Value: string);

    /// <summary>
    /// Contains URL of online service
    /// </summary>
    property ServiceUrl: string read GetServiceUrl write SetServiceUrl;
  end;

  /// <summary>
  /// Interface to provide access to Welcome Page Plugin ID and Name
  /// </summary>
  INTAWelcomePagePlugin = interface
    ['{60AB2EA1-9521-4A7C-BE7F-125308AF20E5}']
    /// <summary>
    /// Returns an identifier of the Welcome Page Plugin
    /// </summary>
    function GetPluginID: string;
    /// <summary>
    /// Returns a display name of the Welcome Page Plugin
    /// </summary>
    function GetPluginName: string;
    /// <summary>
    /// Returns True if the plugin should be shown at the startup.
    /// Returns False if the plugin should shown in the  "Available Plugins" area of the Welcome Page Layout editor.
    /// </summary>
    function GetPluginVisible: Boolean;

    property PluginID: string read GetPluginID;
    property PluginName: string read GetPluginName;
    property PluginVisible: Boolean read GetPluginVisible;
  end;

  /// <summary>
  /// Interface for data model dependent plugins
  /// </summary>
  INTAWelcomePageDataPlugin = interface(INTAWelcomePagePlugin)
    ['{A4E90C8F-9AA0-481E-B402-36329B38F8BC}']
    function GetModel: INTAWelcomePagePluginModel;
    procedure SetModel(const Value: INTAWelcomePagePluginModel);

    /// <summary>
    /// Access to data model
    /// </summary>
    property Model: INTAWelcomePagePluginModel read GetModel write SetModel;
  end;

  /// <summary>
  /// Interface for plugins that manage the state of controls
  /// </summary>
  INTAWelcomePageDataPluginControlState = interface(INTAWelcomePageDataPlugin)
    ['{CD335CEE-A7E9-4E2B-B0CC-A626EAEDE8CB}']
    function GetControl: TControl;
    procedure SetControl(const Value: TControl);

    /// <summary>
    /// Access to control, the state of which is managed
    /// </summary>
    property Control: TControl read GetControl write SetControl;
  end;

  /// <summary>
  /// Interface for plugins that display list of data
  /// </summary>
  INTAWelcomePageCaptionFrame = interface(INTAWelcomePageDataPlugin)
    ['{2E6BCE6D-60BB-42C5-B24A-30C40976F202}']
    /// <summary>
    /// Get custom caption frame
    /// </summary>
    function GetCaptionFrame: TFrame;
    /// <summary>
    /// Set custom Caption frame
    /// </summary>
    procedure SetCaptionFrame(const ACaptionFrame: TFrame);
    /// <summary>
    /// Get custom client frame
    /// </summary>
    function GetClientFrame: TFrame;
    /// <summary>
    /// Set custom client frame
    /// </summary>
    procedure SetClientFrame(const AClientFrame: TFrame);
    property CaptionFrame: TFrame read GetCaptionFrame write SetCaptionFrame;
    property ClientFrame: TFrame read GetClientFrame write SetClientFrame;
  end;

  /// <summary>
  /// Interface for plugins that display list of data
  /// </summary>
  INTAWelcomePageDataPluginListView = interface(INTAWelcomePageCaptionFrame)
    ['{6213F973-E077-4647-9E4E-D10666C30B26}']
    /// <summary>
    /// Get height of list item
    /// </summary>
    function GetItemHeight: Integer;
    /// <summary>
    /// Set height of list item
    /// </summary>
    procedure SetItemHeight(const Value: Integer);
    /// <summary>
    /// Get view mode
    /// </summary>
    function GetViewMode: TWelcomePageViewMode;
    /// <summary>
    /// Set view mode
    /// </summary>
    procedure SetViewMode(const Value: TWelcomePageViewMode);

    /// <summary>
    /// Set event of double click to data item
    /// </summary>
    procedure SetOnItemDblClick(AProc: TOnPluginItemClickEvent);
    /// <summary>
    /// Set event of click to additional button of data item
    /// </summary>
    procedure SetOnItemAdditionalClick(AProc: TOnPluginItemClickEvent);
    /// <summary>
    /// Set PopupMenu to display on click to DropDown button of data item
    /// </summary>
    procedure SetPopupMenu(AMenu: TPopupMenu);

    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ViewMode: TWelcomePageViewMode read GetViewMode write SetViewMode;
  end;

  /// <summary>
  /// Interface to create or destroy the Welcome Page Plugin with View
  /// </summary>
  INTAWelcomePageContentPluginCreator = interface(INTAWelcomePagePlugin)
    ['{1E2E6E47-B4C5-49A6-8018-938B5CC1EDFE}']
    /// <summary>
    /// Returns the main view of the Welcome Page Plugin
    /// </summary>
    function GetView: TFrame;
    /// <summary>
    /// Creates and returns View of the Welcome Page Plugin
    /// </summary>
    function CreateView: TFrame;
    /// <summary>
    /// Destroy the view
    /// </summary>
    procedure DestroyView;
    /// <summary>
    /// Returns set of plugin icon for display in caption (w/ High DPI support)
    /// </summary>
    function GetIcon: TGraphicArray;
    /// <summary>
    /// Returns index of plugin icon
    /// </summary>
    function GetIconIndex: Integer;
    /// <summary>
    /// Sets index of plugin icon
    /// </summary>
    procedure SetIconIndex(const Value: Integer);

    /// <summary>
    /// Property to access to the main view
    /// </summary>
    property View: TFrame read GetView;
    /// <summary>
    /// Property to access to the plugin icon index
    /// </summary>
    property IconIndex: Integer read GetIconIndex write SetIconIndex;
  end;

  /// <summary>
  /// Interface to plugin with View
  /// </summary>
  INTAWelcomePageControlPluginCreator = interface(INTAWelcomePagePlugin)
    ['{ED9D44C5-0131-4C7C-B0EE-19209112A1AE}']
    /// <summary>
    /// Returns control object that manage control
    /// </summary>
    function GetControlObject: TInterfacedObject;
    /// <summary>
    /// Creates and returns the instance of the Welcome Page Control Plugin
    /// </summary>
    function CreateControlPlugin(AControl: TControl): TInterfacedObject;
    /// <summary>
    /// Destroy the instance of the Welcome Page Control Plugin
    /// </summary>
    procedure DestroyControlPlugin;
    /// <summary>
    /// Property to access to control object
    /// </summary>
    property ControlObject: TInterfacedObject read GetControlObject;
  end;

  /// <summary>
  /// Interface of service of the Welcome Page Service
  /// </summary>
  INTAWelcomePagePluginService280 = interface
    ['{AABD143E-44EB-4070-834B-D0002E12EE01}']
    /// <summary>
    /// Return count of registered plugins.
    /// </summary>
    function GetPluginCount: Integer;
    /// <summary>
    /// Return registered plugin ID by Index.
    /// </summary>
    function GetPluginID(Index: Integer): string;
    /// <summary>
    /// Return registered plugin interface by Index.
    /// </summary>
    function GetPluginByIndex(Index: Integer): INTAWelcomePagePlugin;
    /// <summary>
    /// Return registered plugin interface by ID.
    /// </summary>
    function GetPluginByID(const ID: string): INTAWelcomePagePlugin;
    /// <summary>
    /// Return view of plugin by Plugin ID.
    /// </summary>
    function GetPluginView(const ID: string): TFrame;

    /// <summary>
    /// Getting Index of plugin icon
    /// </summary>
    function GetPluginIconIndex(const PluginID: string): Integer;
    /// <summary>
    /// Set Event to initialize plugins
    /// </summary>
    procedure SetOnInitPluginsEvent(const AProc: TProc);
    /// <summary>
    /// Set Event to registered in Welcome Page Service plguin
    /// </summary>
    procedure SetOnPluginRegisteredEvent(const AProc: TOnPluginRegisteredEvent);

    /// <summary>
    /// Registers plugin in the Welcome Page Service
    /// </summary>
    procedure RegisterPluginCreator(const WPPluginCreator: INTAWelcomePagePlugin);
    /// <summary>
    /// Destroys and unregisters plugin from Welcome Page Service
    /// </summary>
    procedure UnRegisterPlugin(const PluginID: string);

    /// <summary>
    /// Creates (if it's not exist) registered in Welcome Page Service plugin
    /// </summary>
    function CreatePlugin(const PluginID: string): TFrame; overload;
    /// <summary>
    /// Creates (if it's not extist) registered in Welcome Page Service control plugin
    /// </summary>
    function CreatePlugin(const PluginID: string; const Control: TControl): TInterfacedObject; overload;
    /// <summary>
    /// Destroy registered in Welcome Page Service plugin
    /// </summary>
    procedure DestroyPlugin(const PluginID: string);

    /// <summary>
    /// Create a new instance for a view w/ caption frame
    /// </summary>
    function CreateCaptionFrame(const APluginID, APluginName: string; const AModel: INTAWelcomePagePluginModel): TFrame;
    /// <summary>
    /// Create a new instance for a view w/ a Listview frame
    /// </summary>
    function CreateListViewFrame(const APluginID, APluginName: string;
      const AViewMode: TWelcomePageViewMode; const AModel: INTAWelcomePagePluginModel): TFrame;

    // List of registered Plugins
    property PluginCount: Integer read GetPluginCount;
    property PluginID[Index: Integer]: string read GetPluginID;
    property Plugins[Index: Integer]: INTAWelcomePagePlugin read GetPluginByIndex; default;
    property PluginsByID[const ID: string]: INTAWelcomePagePlugin read GetPluginByID;
    property PluginView[const ID: string]: TFrame read GetPluginView;
  end;

  INTAWelcomePagePluginService = interface(INTAWelcomePagePluginService280)
    ['{CFDFE3BD-B3BA-4D27-BEC0-8130E4A7874B}']
  end;

  /// <summary>
  /// Interface to save or read the Welcome Page registry data
  /// </summary>
  INTAWelcomePageSettings280 = interface
    ['{D5935926-8F43-4F49-9EDB-27F492135254}']
    /// <summary>
    /// Returns true if setting with Name exists
    /// </summary>
    function IsSettingExists(const Name: string): Boolean;
    /// <summary>
    /// Set and get settings section name. Default is Root of base key
    /// </summary>
    procedure SetSettingsSection(const ASection: string = '');
    function GetSettingsSection: string;
    /// <summary>
    /// Save settings to registry
    /// </summary>
    procedure SaveSetting(const Name: string; Value: TStream); overload;
    procedure SaveSetting(const Name: string; Value: Boolean); overload;
    procedure SaveSetting(const Name: string; Value: TDateTime); overload;
    procedure SaveSetting(const Name: string; Value: Double); overload;
    procedure SaveSetting(const Name: string; Value: Integer); overload;
    procedure SaveSetting(const Name, Value: string; AFile: Boolean = false); overload;
    /// <summary>
    /// Read settings from registry
    /// </summary>
    procedure ReadSetting(const Name: string; Value: TStream); overload;
    procedure ReadSetting(const Name: string; var Value: Boolean); overload;
    procedure ReadSetting(const Name: string; var Value: TDateTime); overload;
    procedure ReadSetting(const Name: string; var Value: Double); overload;
    procedure ReadSetting(const Name: string; var Value: Integer); overload;
    procedure ReadSetting(const Name: string; var Value: string); overload;
    /// <summary>
    /// Delete settings from registry
    /// </summary>
    procedure DeleteSetting(const Name: string);
  end;

  INTAWelcomePageSettings = interface(INTAWelcomePageSettings280)
    ['{B10CD61C-4137-430F-A667-FACA165BE816}']
  end;

  /// <summary>
  /// Interface to implement the Welcome Page background
  /// </summary>
  INTAWelcomePageBackgroundService280 = interface
    ['{7AA097E9-5932-4224-A996-4F271779B0FB}']
    /// <summary>
    /// Painting a rectangle of background to any canvas (AColor - specific color to blend)
    /// </summary>
    procedure PaintBackgroundTo(ACanvas: TCanvas; AControl: TControl; ARect: TRect; AColor: TColor; AOpacity: Byte = 255); overload;
    procedure PaintBackgroundTo(ACanvas: TCanvas; AControl: TControl; AColor: TColor; AOpacity: Byte = 255); overload;
    procedure PaintBackgroundTo(ACanvas: TCanvas; AControl: TControl; AOpacity: Byte = 255); overload;
    /// <summary>
    /// Getting a rectangle relative of background owner component
    /// </summary>
    function GetRelativeControlRect(AControl: TControl): TRect;
    /// <summary>
    /// Getting a size of text with specific font
    /// </summary>
    function GetTextSize(const AText: string; AFont: TFont): TSize;
    /// <summary>
    /// Painting text to a rectangle of canvas
    /// </summary>
    procedure PaintTextTo(ACanvas: TCanvas; const AText: string; var ARect: TRect; AFont: TFont; ATextFormat: TTextFormat);
  end;

  INTAWelcomePageBackgroundService = interface(INTAWelcomePageBackgroundService280)
    ['{996AC521-40D2-4FA4-94A8-E9F2EB16199D}']
  end;

  TWelcomePageMetrics = class
    type
      ListView = record
      const
        SmallItemHeight = 32;
        MediumItemHeight = 56;
        LargeItemHeight = 80;
      end;
      Background = record
      const
        Opacity = 64;
      end;
  end;


var
  WelcomePagePluginService: INTAWelcomePagePluginService;
  WelcomePageSettings: INTAWelcomePageSettings;

implementation

end.


