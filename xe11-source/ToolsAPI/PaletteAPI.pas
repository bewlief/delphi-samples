{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit PaletteAPI;

{ Component Palette / ToolBox API }

interface

uses System.Classes, 
{$IFNDEF PARSER_TEST}
  ToolsApi,
{$ELSE}
  ToolsAPI4Testing,
{$ENDIF}
  Vcl.Graphics, Winapi.ActiveX, Vcl.Menus, Vcl.GraphUtil, VCL.ImageCollection;

type
  TNTAPaintIconSize = (pi16x16, pi24x24, pi32x32, pi48x48, pi64x64, pi128x128);

  /// <summary>
  /// INTAPalettePaintIcon
  /// Palette items which implement INTAPalettePaintIcon will be called to paint
  /// their icon to the given Canvas. 
  /// </summary>
  INTAPalettePaintIcon = interface
    ['{D9BAD01A-99D9-4661-A470-90C7BC743DC9}']
    procedure Paint(const Canvas: TCanvas; const X, Y: Integer;
      const IconSize: TNTAPaintIconSize);
  end;

  INTAPalettePaintIcon160 = interface(INTAPalettePaintIcon)
    ['{EAF90D7D-0C59-4B90-AF44-EE525719EEFC}']
    procedure Paint(const Canvas: TCanvas; const X, Y: Integer;
      const IconSize: TNTAPaintIconSize; Enabled: Boolean); overload;
  end;

  INTAPalettePaintIcon280 = interface(INTAPalettePaintIcon160)
    ['{C268AB62-C01C-4205-80AC-72F92CEA90DB}']
    function LoadIconFromResource: Integer;
    procedure Paint(const Canvas: TCanvas; const X, Y, AHeight: Integer;
      Enabled, AThreadLoading: Boolean); overload;
  end;

  /// <summary>
  /// TPalDragState: same as QControls.TDragState and Controls.TDragState.
  /// Added here for a more generic TDragState. 
  /// </summary>
  TPalDragState = (pdsDragEnter, pdsDragLeave, pdsDragMove);

  /// <summary>
  /// IOTAPaletteDragAcceptor
  /// Register your IOTAPaletteDragAcceptor to mark that window
  /// as acceptable for a drop target from the Palette. Handle
  /// is topmost parent handle for your accepting window.
  /// 
  /// You can either use this, or register your window as an OLE drop target
  /// and implement the IOTAPaletteOleDragDropOp interface to fill in the
  /// OLE IDataObject drag and drop interface that will be "dragged over" the window.
  ///    
  /// </summary>
  IOTAPaletteDragAcceptor = interface
    ['{44E0BDCA-EEDD-45A5-8170-A764D9E26056}']
    function GetHandle: THandle;
    property Handle: THandle read GetHandle;
  end;

  /// <summary>
  /// IOTADesignerDragAcceptor
  /// Use this interface to see if your item is being dragged over
  /// the VCL designer. 
  /// </summary>
  IOTADesignerDragAcceptor = interface(IOTAPaletteDragAcceptor)
    ['{0047B6A0-D238-4627-8EBD-9F66A57CF2F5}']
  end;


  /// <summary>
  /// IOTACodeEditorDragAcceptor
  /// Use this interface to see if your item is being dragged over the code
  /// editor. 
  /// </summary>
  IOTACodeEditorDragAcceptor = interface(IOTAPaletteDragAcceptor)
    ['{7A8E3301-46A6-4226-9B8F-0BCEC7E9E801}']
    function GetEditorControl: TObject;
    /// <summary>
    /// Returns the EditorControl object for this acceptor. You can only
    /// ccess this if you are NTA, however, the interface query is valid for OTA. 
    /// </summary>
    property EditorControl: TObject read GetEditorControl;
  end;
  
  /// <summary>
  /// TOTAPaletteDragDropOp
  /// If your Palette Item implements this interface, DragOver will
  /// be called when the item is dragged over something that supports
  /// items being dropped on it, such as the Code Editor or
  /// Designer.  If the item was droped there, DragDrop will be called
  /// for the item. 
  /// </summary>
  IOTAPaletteDragDropOp = interface
    ['{A6364D92-37AB-4C39-ACA3-4CB1F8BD0C94}']
    procedure DragOver(Target: IOTAPaletteDragAcceptor; X, Y: Integer;
      State: TPalDragState; var Accept: Boolean);
    procedure DragDrop(Target: IOTAPaletteDragAcceptor; X, Y: Integer);
  end;

  /// <summary>
  /// IOTAPaletteOleDragDropOp
  /// If your Palette Item implementes this interface, it will be called
  /// to fill in the OLE IDataObject that will be dragged around.
  /// This is required for designers or windows that use OLE drag and drop. 
  /// </summary>
  IOTAPaletteOleDragDropOp = interface
    ['{5B524357-BB75-4C8A-B88F-954075A60FFD}']
    /// <summary>
    /// Provides the TFormatEtc indicating the data that the IDataObject
    /// will contain. 
    /// </summary>
    function GetFormatRec: TFormatEtc;
    /// <summary>
    /// Allows you to return true if you return data in the SourceFormat 
    /// </summary>
    function QueryGetData(const SourceFormat: TFormatEtc): Boolean;
    /// <summary>
    /// Allows you to fill in the Medium with the requested SourceFormat  
    /// </summary>
    function GetData(const SourceFormat: TFormatEtc; out Medium: TStgMedium): Boolean;
  end;

  /// <summary>
  /// IOTAPaletteCursor
  /// Implement this interface to set the cursor when the palette item is dragged onto a designer.
  /// WebForms uses this interface. 
  /// </summary>
  IOTAPaletteCursor = interface
    ['{26DDEB04-1FFC-45B0-98E7-02AD8706AF70}']
    function SetCursor: Boolean;
  end;

  /// <summary>
  /// IOTABasePaletteItem
  /// A base bare minimum item on the component palette. Base interface
  /// for other items. 
  /// </summary>
  IOTABasePaletteItem270 = interface
    ['{72808CFB-B7F9-4598-A3FA-310DA25B700D}']
    function GetCanDelete: Boolean;
    function GetEnabled: Boolean;
    function GetHelpName: string;
    function GetHintText: string;
    function GetIDString: string;
    function GetName: string;
    function GetVisible: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetHelpName(const Value: string);
    procedure SetName(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure SetHintText(const Value: string);
    /// <summary>
    /// If this item can be deleted (permanently), return true.
    /// When Delete is called, you should remove the item (permanently) 
    /// </summary>
    property CanDelete: Boolean read GetCanDelete;
    property Enabled: Boolean read GetEnabled;
    /// <summary>
    /// HelpName: used for displaying a help page for this item 
    /// </summary>
    property HelpName: string read GetHelpName write SetHelpName;
    /// <summary>
    /// HintText: The tool tip that is displayed for this item 
    /// </summary>
    property HintText: string read GetHintText write SetHintText;
    /// <summary>
    /// IDString: uniquely identifies the item, and never changes 
    /// </summary>
    property IDString: string read GetIDString;
    /// <summary>
    /// Name: the display name for a given item 
    /// </summary>
    property Name: string read GetName write SetName;
    /// <summary>
    /// Visible: if the item should be shown or not. Items should
    /// be hidden when they are not valid for the current designer. Hidden
    /// items may be shown when customizing the palette. 
    /// </summary>
    property Visible: Boolean read GetVisible write SetVisible;
    /// <summary>
    /// Execute: call it when you want this item to create itself
    /// or execute its action. Groups will do nothing. Palette items
    /// will typically create themselves. 
    /// </summary>
    procedure Execute;
    /// <summary>
    /// Delete is called when an item is removed from the IDE permanently.
    /// CanDelete should be checked before calling Delete. 
    /// </summary>
    procedure Delete;
  end;

  /// <summary>
  ///  This interface contains methods and properties to work with ImageIndex.
  /// </summary>
  IOTABasePaletteItem280 = interface(IOTABasePaletteItem270)
    ['{6FC107FF-449B-4D65-A163-513843770F87}']
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
  end;

  IOTABasePaletteItem = interface(IOTABasePaletteItem280)
    ['{9CAEBEBF-6BFB-4E9A-B5E7-C3146DEFA6E8}']
  end;

  /// <summary>
  /// IOTAGetPaletteItem
  /// This interface is used to get a palette item from an IDataObject during
  /// ole drag drop processing 
  /// </summary>
  IOTAGetPaletteItem = interface
    ['{ED77C99B-5994-4C8B-92BB-CB72B3BEEBD9}']
    function GetPaletteItem: IOTABasePaletteItem;
  end;

  /// <summary>
  /// IOTAExecutePaletteItem
  /// Modules implement this interface if they support execution of a palette item.
  /// This interface is used when a snippet is double clicked. 
  /// </summary>
  IOTAExecutePaletteItem = interface
    ['{70FAFCB8-EE3D-487F-B546-00E002AA7027}']
    procedure ExecutePaletteItem(Item: IOTABasePaletteItem);
  end;

  /// <summary>
  /// IOTAComponentPaletteItem 
  /// </summary>
  IOTAComponentPaletteItem = interface(IOTABasePaletteItem)
    ['{5A1A13FD-48B6-4A76-BB3F-374102172767}']
    procedure SetPackageName(const Value: string);
    procedure SetClassName(const Value: string);
    procedure SetUnitName(const Value: string);

    function GetClassName: string;
    function GetPackageName: string;
    function GetUnitName: string;
    /// <summary>
    /// ClassName: the classname of the item. May be different, or the
    /// same thing as the Name. 
    /// </summary>
    property ClassName: string read GetClassName write SetClassName;
    /// <summary>
    /// PackageName: the base package that this item exists in. 
    /// </summary>
    property PackageName: string read GetPackageName write SetPackageName;
    /// <summary>
    /// UnitName: the full unit name that contains the given item.
    /// It may be blank for items that don't have units (such as templates) 
    /// </summary>
    property UnitName: string read GetUnitName write SetUnitName;
  end;

  /// <summary>
  /// IOTAPaletteGroup
  /// Each group can contain regular palette items and/or other
  /// groups. If the Item is a IOTAPaletteGroup, it is a subgroup. 
  /// </summary>
  IOTAPaletteGroup = interface(IOTABasePaletteItem)
    ['{C7593CE8-1A0B-409F-B21F-84D983593F77}']
    function GetCount: Integer;
    function GetItem(const Index: Integer): IOTABasePaletteItem;

    property Count: Integer read GetCount;
    property Items[const Index: Integer]: IOTABasePaletteItem read GetItem; default;

    /// <summary>
    /// AddGroup: adds a new sub-group to this group, or returns an existing
    /// group if IDString was already found. 
    /// </summary>
    function AddGroup(const Name, IDString: string): IOTAPaletteGroup;
    /// <summary>
    /// AddItem: adds an item and returns its current index. The current
    /// index may change if an item is deleted before it. 
    /// </summary>
    function AddItem(const Item: IOTABasePaletteItem): Integer;
    procedure Clear;
    /// <summary>
    /// FindItem*: for locating particular items. If Recurse is True, it will
    /// iterate through subgroups. 
    /// </summary>
    function FindItem(const IDString: string; Recurse: Boolean): IOTABasePaletteItem;
    function FindItemByName(const Name: string; Recurse: Boolean): IOTABasePaletteItem;
    /// <summary>
    /// FindItemGroup: Finds the group for which the given item is contained in.
    /// It should always be recursive, and can return the current group,
    /// if it contains the item. Returns nil if it could not find the item. 
    /// </summary>
    function FindItemGroup(const IDString: string): IOTAPaletteGroup;
    function FindItemGroupByName(const Name: string): IOTAPaletteGroup;
    /// <summary>
    /// InsertItem: Add an item at a particular index 
    /// </summary>
    procedure InsertItem(const Index: Integer; const Item: IOTABasePaletteItem);
    /// <summary>
    /// IndexOf: Returns the index of an Item in the group, or -1 if not found 
    /// </summary>
    function IndexOf(const Item: IOTABasePaletteItem): Integer;
    /// <summary>
    /// Move: Moves the item at CurIndex to NewIndex.  If NewIndex < 0 then the item
    /// is deleted. 
    /// </summary>
    procedure Move(const CurIndex, NewIndex: Integer);
    /// <summary>
    /// RemoveItem: remove the Item from the Group, searching all the
    /// subgroups if Recursive is True. 
    /// </summary>
    function RemoveItem(const IDString: string; Recurse: Boolean): Boolean;
  end;

  /// <summary>
  /// IOTAPaletteNotifier
  /// Add this notifier to the IOTAPaletteServices to be notified when
  /// the palette has changed.
  /// 
  /// Modifed: called when you should refresh all your items.
  /// 
  /// Destroyed: Release your hold on the IOTAPaletteServices.
  /// 
  /// ***NOTE:*** When ItemAdded or ItemRemoved are called, Modified will not
  /// be called. This allows you to keep your updates to a minimum.
  ///    
  /// </summary>
  IOTAPaletteNotifier = interface(IOTANotifier)
    ['{6A3D2F2D-19BD-487E-A715-F1E7FECF8791}']
    /// <summary>
    /// ItemAdded: called when a particular item is added to a group. 
    /// </summary>
    procedure ItemAdded(const Group: IOTAPaletteGroup;
      const Item: IOTABasePaletteItem);
    /// <summary>
    /// ItemRemoved: called when a particular item is deleted from the
    /// palette. You should release your references to the item at this time. 
    /// </summary>
    procedure ItemRemoved(const Group: IOTAPaletteGroup;
      const Item: IOTABasePaletteItem);
    /// <summary>
    /// SelectedToolChanged: a new tool was selected. If you show a selected
    /// tool, you should "unselect" it, and display this Tool as selected. 
    /// </summary>
    procedure SelectedToolChanged(const Tool: IOTABasePaletteItem);
    /// <summary>
    /// BeginUpdate/EndUpdate: will be called before multiple items are added
    /// or removed. You should refresh your UI and list of items on the
    /// last EndUpdate. 
    /// </summary>
    procedure BeginUpdate;
    procedure EndUpdate;
  end;

  TOTAPaletteColorItem = record
    StartColor: TColor;
    EndColor: TColor;
    TextColor: TColor;
  end;

  TOTAPaletteButtonColors = record
    RegularColor, SelectedColor, HotColor: TColor;
  end;

  TOTAPaletteOptions = record
    BackgroundColor, BackgroundGradientColor: TColor;
    BoldCaptions: Boolean;
    CaptionOnlyBorder: Boolean;
    UsePlusMinus: Boolean;
    CategoryGradientDirection: TGradientDirection;
    BackgroundGradientDirection: TGradientDirection;
  end;

  /// <summary>
  /// IOTAPaletteColorScheme
  /// A Color scheme used by the default palette. Can be used by other
  /// palette via the IOTAPaletteServices, if you wish 
  /// </summary>
  IOTAPaletteColorScheme = interface
    ['{75D3424A-6518-4465-B940-E004D7FFBB0C}']
    function GetButtonColors: TOTAPaletteButtonColors;
    function GetColor(const Index: Integer): TOTAPaletteColorItem;
    function GetCount: Integer;
    function GetIDString: string;
    function GetName: string;
    function GetOptions: TOTAPaletteOptions;

    property ButtonColors: TOTAPaletteButtonColors read GetButtonColors;
    property Options: TOTAPaletteOptions read GetOptions;
    /// <summary>
    /// Colors are the colors to iterate through for each category when
    /// using a Scheme 
    /// </summary>
    property Colors[const Index: Integer]: TOTAPaletteColorItem read GetColor; default;
    property Count: Integer read GetCount;

    property IDString: string read GetIDString;
    property Name: string read GetName;
  end;

  /// <summary>
  /// IOTAPaletteServices
  /// Query BorlandIDEServices for IOTAPaletteServices.
  /// Use IOTAPaletteServices to iterate through all the top level groups,
  /// add a notifier, and notify when you add/remove items. 
  /// </summary>
  IOTAPaletteServices270 = interface
    ['{DB22AF92-DFC1-40DD-B7A4-B1FE77158B74}']
    procedure SetSelectedTool(const Value: IOTABasePaletteItem);
    function GetBaseGroup: IOTAPaletteGroup;
    function GetSelectedTool: IOTABasePaletteItem;

    function AddNotifier(const Notifier: IOTAPaletteNotifier): Integer;
    /// <summary>
    /// BaseGroup: the first group in the palette which contains all of the
    /// other groups. Iterate through it to find all the items. 
    /// </summary>
    property BaseGroup: IOTAPaletteGroup read GetBaseGroup;
    /// <summary>
    /// BeginUpdate: Call when you are adding or removing multiple items.
    /// Always match with an EndUpdate. 
    /// </summary>
    procedure BeginUpdate;
    procedure EndUpdate;
    /// <summary>
    /// Call ItemAdded when you add an item to a group. 
    /// </summary>
    procedure ItemAdded(const Group: IOTAPaletteGroup;
      const Item: IOTABasePaletteItem);
    /// <summary>
    /// Call ItemRemoved when you delete an item. 
    /// </summary>
    procedure ItemRemoved(const Group: IOTAPaletteGroup;
      const Item: IOTABasePaletteItem);
    /// <summary>
    /// Call Modifed when one or more items state has changed. 
    /// </summary>
    procedure Modified;

    /// <summary>
    /// RegisterDragAcceptor: returns the index of a newly registered acceptor 
    /// </summary>
    function RegisterDragAcceptor(const Acceptor: IOTAPaletteDragAcceptor): Integer;
    /// <summary>
    /// RemoveDragAcceptor: removes the acceptor at Index 
    /// </summary>
    procedure UnRegisterDragAcceptor(const Index: Integer);
    /// <summary>
    /// GetDragAcceptors: returns a list of all acceptors.
    /// Each item in the list is a IOTAPaletteDragAcceptor 
    /// </summary>
    function GetDragAcceptors: IInterfaceList;

    function RegisterColorScheme(const ColorScheme: IOTAPaletteColorScheme): Integer;
    procedure UnRegisterColorScheme(const Index: Integer);
    function GetColorSchemes: IInterfaceList;

    procedure RemoveNotifier(const Index: Integer);
    /// <summary>
    /// SelectedTool: The current selected tool (may be nil). When you
    /// visually change what the selected tool is, you must set this
    /// property. It will notify each Notifier. 
    /// </summary>
    property SelectedTool: IOTABasePaletteItem read GetSelectedTool write SetSelectedTool;

    /// <summary>
    /// PaletteState: a string which contains multiple "states" that the IDE
    /// is in. This allows palette items to determine if they should
    /// be visible or not, based on what is in the state. 
    /// </summary>
    procedure AddPaletteState(const State: string);
    procedure RemovePaletteState(const State: string);
    /// <summary>
    /// If your state is a "designer" state, use this method to add your state.
    /// The "cPSDesignerActive" state will be set when at least one
    /// designer state is in the PaletteState string. 
    /// </summary>
    procedure AddPaletteDesignerState(const State: string);

    function ContainsPaletteState(const State: string): Boolean;
    function GetPaletteState: string;

  end;

  IOTAPaletteServices280 = interface(IOTAPaletteServices270)
    ['{968F2C45-81D4-4B86-9AB7-F70FBEF51973}']
    procedure SetImageCollection(const AValue: TImageCollection);
    function GetImageCollection: TImageCollection;

    property ImageCollection: TImageCollection read GetImageCollection write SetImageCollection;
  end;

  IOTAPaletteServices = interface(IOTAPaletteServices280)
    ['{6963F71C-0A3F-4598-B6F6-9574CE0E6847}']
  end;

  /// <summary>
  /// INTAPaletteContextMenu
  /// Query the BorlandIDEServices for this service. It allows you to dynamically
  /// add context menus to the palette. Consumers can use menu items from the
  /// interface, producers can add menu items to the interface. 
  /// </summary>
  INTAPaletteContextMenu = interface
    ['{B58639BC-4332-45BF-A79E-D4977EE7E024}']
    function GetCount: Integer;
    function GetContextMenuItem(const Index: Integer): TMenuItem;

    /// <summary>
    /// Adds a context menu to the internal list. 
    /// </summary>
    procedure AddContextMenuItem(const MenuItem: TMenuItem);
    /// <summary>
    /// Remove a context menu item previously added 
    /// </summary>
    procedure RemoveContextMenuItem(const MenuItem: TMenuItem);
    /// <summary>
    /// Retrieves the menu items. Since the item may have already been used
    /// by someone else, you should remove it from the parent before
    /// using it:
    /// 
    /// NewMenuItem := ContextMenuItems[I];
    /// if NewMenuItem.Parent <> nil then
    ///   NewMenuItem.Parent.Remove(NewMenuItem);
    /// 
    ///     
    /// </summary>
    property ContextMenuItems[const Index: Integer]: TMenuItem read GetContextMenuItem; default;
    property Count: Integer read GetCount;
  end;

(*
    Note: snipplets have been removed in favor of code templates.
    Please see CodeTemplateAPI.pas.

    /// <summary>
    /// IOTASnippletManager
    /// Allows you to add/remove/query for code snipplets 
    /// </summary>
    IOTASnippletManager = interface
      ['{8E96E78C-1153-4CCA-B8D2-CF5F4DBA7329}']
      function GetCount: Integer;

      /// <summary>
      /// Adds a snipplet to the palette; returns the IDString for the new snipplet as UTF8 
      /// </summary>
      function AddSnipplet(const Text: string): string; overload;
      /// <summary>
      /// Remove a snipplet with the given IDString
      /// </summary>
      function RemoveSnipplet(const IDString: string): Boolean;
      /// <summary>
      /// Remove all snipplet's 
      /// </summary>
      procedure RemoveAllSnipplets;
      /// <summary>
      /// The count of all snipplets 
      /// </summary>
      property Count: Integer read GetCount;
      /// <summary>
      /// Gets a particular id string at a given index 
      /// </summary>
      function GetIDString(const Index: Integer): string;
      /// <summary>
      /// Retrieves the snipplet text at a particular index 
      /// </summary>
      function GetSnipplet(const Index: Integer): string; overload;
    end deprecated;
  
    /// <summary>
    /// IOTAWideSnippletManager
    /// Wide string support 
    /// </summary>
    IOTAWideSnippletManager = interface
      ['{D30FBAC1-653C-405F-AF8A-6048A8B79ED2}']
      /// <summary>
      /// AddSnipplet: Wide string version 
      /// </summary>
      function AddSnippletW(const Text: Widestring): string;
      /// <summary>
      /// GetSnipplet: Wide string version 
      /// </summary>
      function GetSnippletW(const Index: Integer): Widestring;
    end deprecated;
*)

const
  // Some predefined palette states
  cPSCodeEditorActive = 'CodeEditorActive';
  /// <summary>
  /// The cPSDesignerActive state is special. If you remove it,
  /// it clears all states added with AddDesignerState.
  /// If you add a state with AddDesignerState, it implicitly adds
  /// the cPSDesignerActive state. When you remove the last designer state
  /// previously added with AddDesignerState, then cPSDesignerActive is
  /// automatically removed.
  /// </summary>
  cPSDesignerActive = 'DesignerActive';
  cNTAPaintIconSizes: array[TNTAPaintIconSize] of Integer = (16, 24, 32, 48, 64, 128);

implementation

end.
