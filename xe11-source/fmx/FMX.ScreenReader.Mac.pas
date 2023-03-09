{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.ScreenReader.Mac;

interface

uses
  System.Classes, System.UITypes, FMX.Forms, FMX.Types, System.SysUtils, System.TypInfo, FMX.ScreenReader, Macapi.AppKit,
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.ObjcRuntime, System.Generics.Collections, Macapi.ObjectiveC,
  Macapi.QuartzCore, FMX.Controls, FMX.TreeView, FMX.Grid, System.Types, System.Character, FMX.StdCtrls, FMX.Graphics;

{$SCOPEDENUMS ON}

type
  //Edit state for voiceover of keyboard character key(s)
  TEditState = (Focus, LastKey, LastWord);

  //Base form class to inherit from and support Accessibility in an application
  TAccForm = class(TForm)
  private
    FFocusInit: Boolean;
    FPrevIdx: Integer;
    FPrevCell: TPoint;
    FPrevSel: TTreeViewItem;
    FPrevEx: Double;
    FPrevStr: string;
    FEditKey: Char;
    FEditState: TEditState;
    FEditTimer, FEditMouseUp: TTimer;
    FOldActive: Boolean;
  protected
    procedure EditTimerChanged(Sender: TObject);
    procedure EditMouseUpChanged(Sender: TObject);
    procedure SendNotification; virtual;
    procedure DoShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoPaint(const Canvas: TCanvas; const ARect: TRectF); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFocusedControl;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoClick: Boolean = True); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  end;

  //NSObject interface declaration of NSAccessibility API procedures and functions
  NSObjectClass = interface(IObjectiveCClass)
    ['{84CDD025-E02A-4128-B1AC-35A7A5A4643B}']
  end;
  NSObject = interface(IObjectiveCInstance)
    ['{C8CC567E-50C3-461C-BAA7-AD96D2CDC5C6}']
    function accessibilityAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer): Pointer; overload; cdecl;
    function accessibilityIsAttributeSettable(attribute: Pointer): Boolean; cdecl;
    procedure accessibilitySetValue(value: Pointer; attribute: Pointer); cdecl;
    function accessibilitySetOverrideValue(value: Pointer; attribute: Pointer): Boolean; cdecl;
    function accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger; cdecl;
    function accessibilityArrayAttributeValues(attribute: Pointer; index: NSUInteger; maxCount: NSUInteger): NSArray; cdecl;
    function accessibilityIndexOfChild(child: Pointer): NSUInteger; cdecl;
    function accessibilityParameterizedAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer; parameter: Pointer): Pointer; overload; cdecl;
    function accessibilityActionNames: NSArray; cdecl;
    function accessibilityActionDescription(action: Pointer): NSString; cdecl;
    procedure accessibilityPerformAction(action: Pointer); cdecl;
    function accessibilityIsIgnored: Boolean; cdecl;
    function accessibilityHitTest(point: NSPoint): Pointer; cdecl;
    function accessibilityFocusedUIElement: Pointer; cdecl;
    procedure retain; cdecl;
    procedure release; cdecl;
    function retainCount: NSUInteger; cdecl;
    function isKindOfClass(cls: Pointer): Boolean; cdecl;
    function isMemberOfClass(cls: Pointer): Boolean; cdecl;
  end;
  TNSObject = class(TOCGenericImport<NSObjectClass, NSObject>) end;

  //NSView interface declaration of NSAccessibility API procedures and functions
  NSViewClass = interface(NSResponderClass)
    ['{B5D7C145-77C2-4826-B957-B8F2D7C01C9C}']
    function defaultFocusRingType: NSFocusRingType; cdecl;
    function defaultMenu: NSMenu; cdecl;
    function focusView: Pointer; cdecl;
  end;
  NSView = interface(NSResponder)
    ['{607BF1EF-39B3-4440-B84F-C4EA8902ED43}']
    function accessibilityAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer): Pointer; overload; cdecl;
    function accessibilityIsAttributeSettable(attribute: Pointer): Boolean; cdecl;
    procedure accessibilitySetValue(value: Pointer; attribute: Pointer); cdecl;
    function accessibilitySetOverrideValue(value: Pointer; attribute: Pointer): Boolean; cdecl;
    function accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger; cdecl;
    function accessibilityArrayAttributeValues(attribute: Pointer; index: NSUInteger; maxCount: NSUInteger): NSArray; cdecl;
    function accessibilityIndexOfChild(child: Pointer): NSUInteger; cdecl;
    function accessibilityParameterizedAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer; parameter: Pointer): Pointer; overload; cdecl;
    function accessibilityActionNames: NSArray; cdecl;
    function accessibilityActionDescription(action: Pointer): NSString; cdecl;
    procedure accessibilityPerformAction(action: Pointer); cdecl;
    function accessibilityIsIgnored: Boolean; cdecl;
    function accessibilityHitTest(point: NSPoint): Pointer; cdecl;
    function accessibilityFocusedUIElement: Pointer; cdecl;
    function acceptsFirstMouse(theEvent: NSEvent): Boolean; cdecl;
    function acceptsTouchEvents: Boolean; cdecl;
    procedure addCursorRect(aRect: NSRect; cursor: NSCursor); cdecl;
    procedure addSubview(aView: NSView); cdecl; overload;
    procedure addSubview(aView: NSView; positioned: NSWindowOrderingMode; relativeTo: NSView); cdecl; overload;
    function addToolTipRect(aRect: NSRect; owner: Pointer; userData: Pointer): NSToolTipTag; cdecl;
    procedure addTrackingArea(trackingArea: NSTrackingArea); cdecl;
    function addTrackingRect(aRect: NSRect; owner: Pointer; userData: Pointer; assumeInside: Boolean): NSTrackingRectTag; cdecl;
    procedure adjustPageHeightNew(newBottom: PSingle; top: Single; bottom: Single; limit: Single); cdecl;
    procedure adjustPageWidthNew(newRight: PSingle; left: Single; right: Single; limit: Single); cdecl;
    function adjustScroll(newVisible: NSRect): NSRect; cdecl;
    procedure allocateGState; cdecl;
    function alphaValue: Single; cdecl;
    function ancestorSharedWithView(aView: NSView): NSView; cdecl;
    function autoresizesSubviews: Boolean; cdecl;
    function autoresizingMask: NSUInteger; cdecl;
    function autoscroll(theEvent: NSEvent): Boolean; cdecl;
    function backgroundFilters: NSArray; cdecl;
    procedure beginDocument; cdecl;
    procedure beginPageInRect(aRect: NSRect; atPlacement: NSPoint); cdecl;
    function bitmapImageRepForCachingDisplayInRect(rect: NSRect): NSBitmapImageRep; cdecl;
    function bounds: NSRect; cdecl;
    function boundsRotation: Single; cdecl;
    procedure cacheDisplayInRect(rect: NSRect; toBitmapImageRep: NSBitmapImageRep); cdecl;
    function canBecomeKeyView: Boolean; cdecl;
    function canDraw: Boolean; cdecl;
    function canDrawConcurrently: Boolean; cdecl;
    function centerScanRect(aRect: NSRect): NSRect; cdecl;
    function compositingFilter: CIFilter; cdecl;
    function contentFilters: NSArray; cdecl;
    function convertPoint(aPoint: NSPoint; fromView: NSView): NSPoint; cdecl; overload;
    function convertPointFromBase(aPoint: NSPoint): NSPoint; cdecl;
    function convertPointToBase(aPoint: NSPoint): NSPoint; cdecl;
    function convertRect(aRect: NSRect; fromView: NSView): NSRect; cdecl; overload;
    function convertRectFromBase(aRect: NSRect): NSRect; cdecl;
    function convertRectToBase(aRect: NSRect): NSRect; cdecl;
    function convertSize(aSize: NSSize; fromView: NSView): NSSize; cdecl; overload;
    function convertSizeFromBase(aSize: NSSize): NSSize; cdecl;
    function convertSizeToBase(aSize: NSSize): NSSize; cdecl;
    function dataWithEPSInsideRect(rect: NSRect): NSData; cdecl;
    function dataWithPDFInsideRect(rect: NSRect): NSData; cdecl;
    procedure didAddSubview(subview: NSView); cdecl;
    procedure discardCursorRects; cdecl;
    procedure display; cdecl;
    procedure displayIfNeeded; cdecl;
    procedure displayIfNeededIgnoringOpacity; cdecl;
    procedure displayIfNeededInRect(rect: NSRect); cdecl;
    procedure displayIfNeededInRectIgnoringOpacity(rect: NSRect); cdecl;
    procedure displayRect(rect: NSRect); cdecl;
    procedure displayRectIgnoringOpacity(rect: NSRect); cdecl; overload;
    procedure displayRectIgnoringOpacity(aRect: NSRect; inContext: NSGraphicsContext); cdecl; overload;
    function dragFile(filename: NSString; fromRect: NSRect; slideBack: Boolean; event: NSEvent): Boolean; cdecl;
    procedure dragImage(anImage: NSImage; at: NSPoint; offset: NSSize; event: NSEvent; pasteboard: NSPasteboard; source: Pointer; slideBack: Boolean); cdecl;
    function dragPromisedFilesOfTypes(typeArray: NSArray; fromRect: NSRect; source: Pointer; slideBack: Boolean; event: NSEvent): Boolean; cdecl;
    procedure drawPageBorderWithSize(borderSize: NSSize); cdecl;
    procedure drawRect(dirtyRect: NSRect); cdecl;
    procedure drawSheetBorderWithSize(borderSize: NSSize); cdecl;
    function enclosingMenuItem: NSMenuItem; cdecl;
    function enclosingScrollView: NSScrollView; cdecl;
    procedure endDocument; cdecl;
    procedure endPage; cdecl;
    function enterFullScreenMode(screen: NSScreen; withOptions: NSDictionary): Boolean; cdecl;
    procedure exitFullScreenModeWithOptions(options: NSDictionary); cdecl;
    function focusRingType: NSFocusRingType; cdecl;
    function frame: NSRect; cdecl;
    function frameCenterRotation: Single; cdecl;
    function frameRotation: Single; cdecl;
    function gState: NSInteger; cdecl;
    procedure getRectsBeingDrawn(rects: Pointer; count: PNSInteger); cdecl;
    procedure getRectsExposedDuringLiveResize(exposedRects: NSRect; count: PNSInteger); cdecl;
    function heightAdjustLimit: Single; cdecl;
    function hitTest(aPoint: NSPoint): NSView; cdecl;
    function inLiveResize: Boolean; cdecl;
    function initWithFrame(frameRect: NSRect): Pointer; cdecl;
    function inputContext: NSTextInputContext; cdecl;
    function isDescendantOf(aView: NSView): Boolean; cdecl;
    function isFlipped: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isHiddenOrHasHiddenAncestor: Boolean; cdecl;
    function isInFullScreenMode: Boolean; cdecl;
    function isOpaque: Boolean; cdecl;
    function isRotatedFromBase: Boolean; cdecl;
    function isRotatedOrScaledFromBase: Boolean; cdecl;
    function knowsPageRange(range: PNSRange): Boolean; cdecl;
    function layer: CALayer; cdecl;
    function layerContentsPlacement: NSViewLayerContentsPlacement; cdecl;
    function layerContentsRedrawPolicy: NSViewLayerContentsRedrawPolicy; cdecl;
    function locationOfPrintRect(aRect: NSRect): NSPoint; cdecl;
    procedure lockFocus; cdecl;
    function lockFocusIfCanDraw: Boolean; cdecl;
    function lockFocusIfCanDrawInContext(context: NSGraphicsContext): Boolean; cdecl;
    function makeBackingLayer: CALayer; cdecl;
    function menuForEvent(event: NSEvent): NSMenu; cdecl;
    function mouse(aPoint: NSPoint; inRect: NSRect): Boolean; cdecl;
    function mouseDownCanMoveWindow: Boolean; cdecl;
    function needsDisplay: Boolean; cdecl;
    function needsPanelToBecomeKey: Boolean; cdecl;
    function needsToDrawRect(aRect: NSRect): Boolean; cdecl;
    function nextKeyView: NSView; cdecl;
    function nextValidKeyView: NSView; cdecl;
    function opaqueAncestor: NSView; cdecl;
    function pageFooter: NSAttributedString; cdecl;
    function pageHeader: NSAttributedString; cdecl;
    function performKeyEquivalent(theEvent: NSEvent): Boolean; cdecl;
    function performMnemonic(theString: NSString): Boolean; cdecl;
    function postsBoundsChangedNotifications: Boolean; cdecl;
    function postsFrameChangedNotifications: Boolean; cdecl;
    function preservesContentDuringLiveResize: Boolean; cdecl;
    function previousKeyView: NSView; cdecl;
    function previousValidKeyView: NSView; cdecl;
    procedure print(sender: Pointer); cdecl;
    function printJobTitle: NSString; cdecl;
    function rectForPage(page: NSInteger): NSRect; cdecl;
    function rectPreservedDuringLiveResize: NSRect; cdecl;
    procedure reflectScrolledClipView(aClipView: NSClipView); cdecl;
    procedure registerForDraggedTypes(newTypes: NSArray); cdecl;
    function registeredDraggedTypes: NSArray; cdecl;
    procedure releaseGState; cdecl;
    procedure removeAllToolTips; cdecl;
    procedure removeCursorRect(aRect: NSRect; cursor: NSCursor); cdecl;
    procedure removeFromSuperview; cdecl;
    procedure removeFromSuperviewWithoutNeedingDisplay; cdecl;
    procedure removeToolTip(tag: NSToolTipTag); cdecl;
    procedure removeTrackingArea(trackingArea: NSTrackingArea); cdecl;
    procedure removeTrackingRect(tag: NSTrackingRectTag); cdecl;
    procedure renewGState; cdecl;
    procedure resetCursorRects; cdecl;
    procedure resizeSubviewsWithOldSize(oldSize: NSSize); cdecl;
    procedure resizeWithOldSuperviewSize(oldSize: NSSize); cdecl;
    procedure rotateByAngle(angle: Single); cdecl;
    procedure rulerView(ruler: NSRulerView; didAddMarker: NSRulerMarker); cdecl; overload;
    procedure rulerView(ruler: NSRulerView; handleMouseDown: NSEvent); cdecl; overload;
    function rulerView(ruler: NSRulerView; willAddMarker: NSRulerMarker; atLocation: Single): Single; cdecl; overload;
    procedure rulerView(ruler: NSRulerView; willSetClientView: NSView); cdecl; overload;
    procedure scaleUnitSquareToSize(newUnitSize: NSSize); cdecl;
    procedure scrollClipView(aClipView: NSClipView; toPoint: NSPoint); cdecl;
    procedure scrollPoint(aPoint: NSPoint); cdecl;
    procedure scrollRect(aRect: NSRect; by: NSSize); cdecl;
    function scrollRectToVisible(aRect: NSRect): Boolean; cdecl;
    procedure setAcceptsTouchEvents(flag: Boolean); cdecl;
    procedure setAlphaValue(viewAlpha: Single); cdecl;
    procedure setAutoresizesSubviews(flag: Boolean); cdecl;
    procedure setAutoresizingMask(mask: NSUInteger); cdecl;
    procedure setBackgroundFilters(filters: NSArray); cdecl;
    procedure setBounds(aRect: NSRect); cdecl;
    procedure setBoundsOrigin(newOrigin: NSPoint); cdecl;
    procedure setBoundsRotation(angle: Single); cdecl;
    procedure setBoundsSize(newSize: NSSize); cdecl;
    procedure setCanDrawConcurrently(flag: Boolean); cdecl;
    procedure setCompositingFilter(filter: CIFilter); cdecl;
    procedure setContentFilters(filters: NSArray); cdecl;
    procedure setFocusRingType(focusRingType: NSFocusRingType); cdecl;
    procedure setFrame(frameRect: NSRect); cdecl;
    procedure setFrameCenterRotation(angle: Single); cdecl;
    procedure setFrameOrigin(newOrigin: NSPoint); cdecl;
    procedure setFrameRotation(angle: Single); cdecl;
    procedure setFrameSize(newSize: NSSize); cdecl;
    procedure setHidden(flag: Boolean); cdecl;
    procedure setKeyboardFocusRingNeedsDisplayInRect(rect: NSRect); cdecl;
    procedure setLayer(newLayer: CALayer); cdecl;
    procedure setLayerContentsPlacement(newPlacement: NSViewLayerContentsPlacement); cdecl;
    procedure setLayerContentsRedrawPolicy(newPolicy: NSViewLayerContentsRedrawPolicy); cdecl;
    procedure setNeedsDisplay(flag: Boolean); cdecl;
    procedure setNeedsDisplayInRect(invalidRect: NSRect); cdecl;
    procedure setNextKeyView(next: NSView); cdecl;
    procedure setPostsBoundsChangedNotifications(flag: Boolean); cdecl;
    procedure setPostsFrameChangedNotifications(flag: Boolean); cdecl;
    procedure setShadow(shadow: NSShadow); cdecl;
    procedure setSubviews(newSubviews: NSArray); cdecl;
    procedure setToolTip(string_: NSString); cdecl;
    procedure setUpGState; cdecl;
    procedure setWantsLayer(flag: Boolean); cdecl;
    procedure setWantsRestingTouches(flag: Boolean); cdecl;
    function shadow: NSShadow; cdecl;
    function shouldDelayWindowOrderingForEvent(theEvent: NSEvent): Boolean; cdecl;
    function shouldDrawColor: Boolean; cdecl;
    procedure showDefinitionForAttributedString(attrString: NSAttributedString; atPoint: NSPoint); cdecl; overload;
    function subviews: NSArray; cdecl;
    function superview: Pointer; cdecl;
    function tag: NSInteger; cdecl;
    function toolTip: NSString; cdecl;
    function trackingAreas: NSArray; cdecl;
    procedure translateOriginToPoint(translation: NSPoint); cdecl;
    procedure translateRectsNeedingDisplayInRect(clipRect: NSRect; by: NSSize); cdecl;
    procedure unlockFocus; cdecl;
    procedure unregisterDraggedTypes; cdecl;
    procedure updateTrackingAreas; cdecl;
    procedure viewDidEndLiveResize; cdecl;
    procedure viewDidHide; cdecl;
    procedure viewDidMoveToSuperview; cdecl;
    procedure viewDidMoveToWindow; cdecl;
    procedure viewDidUnhide; cdecl;
    procedure viewWillDraw; cdecl;
    procedure viewWillMoveToSuperview(newSuperview: NSView); cdecl;
    procedure viewWillMoveToWindow(newWindow: NSWindow); cdecl;
    procedure viewWillStartLiveResize; cdecl;
    function viewWithTag(aTag: NSInteger): Pointer; cdecl;
    function visibleRect: NSRect; cdecl;
    function wantsDefaultClipping: Boolean; cdecl;
    function wantsLayer: Boolean; cdecl;
    function wantsRestingTouches: Boolean; cdecl;
    function widthAdjustLimit: Single; cdecl;
    procedure willRemoveSubview(subview: NSView); cdecl;
    function window: NSWindow; cdecl;
    procedure writeEPSInsideRect(rect: NSRect; toPasteboard: NSPasteboard); cdecl;
    procedure writePDFInsideRect(rect: NSRect; toPasteboard: NSPasteboard); cdecl;
  end;
  TNSView = class(TOCGenericImport<NSViewClass, NSView>)  end;

  //NSViewX interface declaration of NSAccessibility API procedures and functions
  NSViewX = interface(NSView)
    ['{92E4A9ED-8E18-4E58-B4E0-D72B4F9B87A5}']
    function accessibilityAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer): Pointer; overload; cdecl;
    function accessibilityIsAttributeSettable(attribute: Pointer): Boolean; cdecl;
    procedure accessibilitySetValue(value: Pointer; attribute: Pointer); cdecl;
    function accessibilitySetOverrideValue(value: Pointer; attribute: Pointer): Boolean; cdecl;
    function accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger; cdecl;
    function accessibilityArrayAttributeValues(attribute: Pointer; index: NSUInteger; maxCount: NSUInteger): NSArray; cdecl;
    function accessibilityIndexOfChild(child: Pointer): NSUInteger; cdecl;
    function accessibilityParameterizedAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer; parameter: Pointer): Pointer; overload; cdecl;
    function accessibilityActionNames: NSArray; cdecl;
    function accessibilityActionDescription(action: Pointer): NSString; cdecl;
    procedure accessibilityPerformAction(action: Pointer); cdecl;
    function accessibilityIsIgnored: Boolean; cdecl;
    function accessibilityHitTest(point: NSPoint): Pointer; cdecl;
    function accessibilityFocusedUIElement: Pointer; cdecl;
  end;

  //NSObjectX interface declaration of NSAccessibility API procedures and functions
  NSObjectX = interface(NSObject)
    ['{92E4A9ED-8E18-4E58-B4E0-D72B4F9B87A5}']
    function accessibilityAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer): Pointer; overload; cdecl;
    function accessibilityIsAttributeSettable(attribute: Pointer): Boolean; cdecl;
    procedure accessibilitySetValue(value: Pointer; attribute: Pointer); cdecl;
    function accessibilitySetOverrideValue(value: Pointer; attribute: Pointer): Boolean; cdecl;
    function accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger; cdecl;
    function accessibilityArrayAttributeValues(attribute: Pointer; index: NSUInteger; maxCount: NSUInteger): NSArray; cdecl;
    function accessibilityIndexOfChild(child: Pointer): NSUInteger; cdecl;
    function accessibilityParameterizedAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer; parameter: Pointer): Pointer; overload; cdecl;
    function accessibilityActionNames: NSArray; cdecl;
    function accessibilityActionDescription(action: Pointer): NSString; cdecl;
    procedure accessibilityPerformAction(action: Pointer); cdecl;
    function accessibilityIsIgnored: Boolean; cdecl;
    function accessibilityHitTest(point: NSPoint): Pointer; cdecl;
    function accessibilityFocusedUIElement: Pointer; cdecl;
  end;

  //NSObject interface implementation of NSAccessibility API procedures and functions
  TFMXNSObject = class(TOCLocal)
  private
    FViewControl: TControl;
    FParent: NSView;
    FBounds: NSRect;
    FObjectArr: NSMutableArray;
    FText: string;
  public
    constructor Create;
    function NativeObject: NSObject;
    function GetObjectiveCClass: PTypeInfo; override;
    function accessibilityAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer): Pointer; overload; cdecl;
    function accessibilityIsAttributeSettable(attribute: Pointer): Boolean; cdecl;
    procedure accessibilitySetValue(value: Pointer; attribute: Pointer); cdecl;
    function accessibilitySetOverrideValue(value: Pointer; attribute: Pointer): Boolean; cdecl;
    function accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger; cdecl;
    function accessibilityArrayAttributeValues(attribute: Pointer; index: NSUInteger; maxCount: NSUInteger): NSArray; cdecl;
    function accessibilityIndexOfChild(child: Pointer): NSUInteger; cdecl;
    function accessibilityParameterizedAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer; parameter: Pointer): Pointer; overload; cdecl;
    function accessibilityActionNames: NSArray; cdecl;
    function accessibilityActionDescription(action: Pointer): NSString; cdecl;
    procedure accessibilityPerformAction(action: Pointer); cdecl;
    function accessibilityIsIgnored: Boolean; cdecl;
    function accessibilityHitTest(point: NSPoint): Pointer; cdecl;
    function accessibilityFocusedUIElement: Pointer; cdecl;
    property ViewControl: TControl read FViewControl write FViewControl;
    property Parent: NSView read FParent write FParent;
    property ObjectArr: NSMutableArray read FObjectArr write FObjectArr;
    property Text: string read FText write FText;
    property Bounds: NSRect read FBounds write FBounds;
  end;

  //NSView interface implementation of NSAccessibility API procedures and functions
  TFMXNSView = class(TOCLocal)
  private
    FViewControl: TControl;
    FObjectArr: NSMutableArray;
    FList: TList<TFMXNSObject>;
    FForm: TAccForm;
  public
    function GetRoleText(ARole: TFMXRole): String;
    constructor Create;
    destructor Destroy; override;
    function NativeView: NSView;
    function GetObjectiveCClass: PTypeInfo; override;
    function accessibilityAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer): Pointer; overload; cdecl;
    function accessibilityIsAttributeSettable(attribute: Pointer): Boolean; cdecl;
    procedure accessibilitySetValue(value: Pointer; attribute: Pointer); cdecl;
    function accessibilitySetOverrideValue(value: Pointer; attribute: Pointer): Boolean; cdecl;
    function accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger; cdecl;
    function accessibilityArrayAttributeValues(attribute: Pointer; index: NSUInteger; maxCount: NSUInteger): NSArray; cdecl;
    function accessibilityIndexOfChild(child: Pointer): NSUInteger; cdecl;
    function accessibilityParameterizedAttributeNames: NSArray; cdecl;
    function accessibilityAttributeValue(attribute: Pointer; parameter: Pointer): Pointer; overload; cdecl;
    function accessibilityActionNames: NSArray; cdecl;
    function accessibilityActionDescription(action: Pointer): NSString; cdecl;
    procedure accessibilityPerformAction(action: Pointer); cdecl;
    function accessibilityIsIgnored: Boolean; cdecl;
    function accessibilityHitTest(point: NSPoint): Pointer; cdecl;
    function accessibilityFocusedUIElement: Pointer; cdecl;
    property ViewControl: TControl read FViewControl write FViewControl;
    property ObjectArr: NSMutableArray read FObjectArr write FObjectArr;
    property List: TList<TFMXNSObject> read FList write FList;
    property Form: TAccForm read FForm write FForm;
  end;

const
  AppKitf = '/System/Library/Frameworks/AppKit.framework/AppKit';

var
  FocusedObj: TFMXNSView;
  FocusedCtrl: TControl;
  PrevCtrl: TControl;

//Sends notification to any assistive applications from the user interface
//element in your application
procedure NSAccessibilityPostNotification(element: Pointer; notification: Pointer); cdecl; external AppKitf name _PU + 'NSAccessibilityPostNotification';
//Returns a standard description for a role and subrole
function NSAccessibilityRoleDescription(role, subrole: Pointer): Pointer; cdecl; external AppKitf name _PU + 'NSAccessibilityRoleDescription';
//Returns a list of unignored accessibility objects, descending the hierarchy if necessary
function NSAccessibilityUnignoredChildren(originalChildren: Pointer): Pointer; cdecl; external AppKitf name _PU + 'NSAccessibilityUnignoredChildren';
//Returns an unignored accessibility object, ascending the hierarchy if necessary
function NSAccessibilityUnignoredAncestor(element: Pointer): Pointer; cdecl; external AppKitf name _PU + 'NSAccessibilityUnignoredAncestor';
//Returns a standard description for an action
function NSAccessibilityActionDescription(action: Pointer): Pointer; cdecl; external AppKitf name _PU + 'NSAccessibilityActionDescription';

implementation

uses
  Macapi.Helpers,
  FMX.Edit, FMX.Memo, System.Rtti, FMX.ListBox, FMX.PlatForm.Mac, FMX.ComboEdit, FMX.ComboEdit.Style, FMX.SpinBox;

//Returns the correct position based on the parent of a control
function GetParentPosition(AControl: TControl): TPointF;
var
  CurParentPos, ParentPos: TPointF;
begin
  Result.X := 0;
  Result.Y := 0;
  if (AControl <> nil) and not (AControl.Parent is TForm) then
  begin
    if (AControl.Parent is TControl) then
    begin
      CurParentPos.X := TControl(AControl.Parent).Position.X;
      CurParentPos.Y := TControl(AControl.Parent).Position.Y;
      ParentPos := GetParentPosition(TControl(AControl.Parent));
      Result.X := CurParentPos.X + ParentPos.X;
      Result.Y := CurParentPos.Y + ParentPos.Y;
    end;
  end;
end;

//constructor and setup of key character voiceover and form activate timers
constructor TAccForm.Create(AOwner: TComponent);
begin
  inherited;
  FEditKey := #0;
  FEditState := TEditState.Focus;
  FEditTimer := TTimer.Create(Self);
  FEditTimer.Interval := 1000;
  FEditTimer.OnTimer := EditTimerChanged;

  FEditMouseUp := TTimer.Create(Self);
  FEditMouseUp.Interval := 100;
  FEditMouseUp.OnTimer := EditMouseUpChanged;
end;

destructor TAccForm.Destroy;
begin
  FEditTimer.Free;
  FEditMouseUp.Free;
  inherited;
end;

//Update focused control on the main form when a accessibility enabled
//application closes
procedure TAccForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited;
  if (Application.MainForm <> nil) and (Application.MainForm is TAccForm) then
    TAccForm(Application.MainForm).UpdateFocusedControl;
end;

//Update focused control on form activation
procedure TAccForm.DoPaint(const Canvas: TCanvas; const ARect: TRectF);
begin
  inherited;
  if FOldActive <> Active then
  begin
    FOldActive := Active;
    if FOldActive then
      FEditMouseUp.Enabled := True;
  end;
end;

//update focused control on form show
procedure TAccForm.DoShow;
begin
  inherited;
  if not FFocusInit then
  begin
    UpdateFocusedControl;
    FFocusInit := True;
  end;
end;

//update focused control on form activate
procedure TAccForm.EditMouseUpChanged(Sender: TObject);
begin
  FEditMouseUp.Enabled := False;
  UpdateFocusedControl;
end;

//send notification when keyboard interaction ends and read the last typed word
procedure TAccForm.EditTimerChanged(Sender: TObject);
begin
  FEditTimer.Enabled := False;
  if ((FocusedCtrl is TEdit) or (FocusedCtrl is TComboEdit) or (FocusedCtrl is TMemo)) and
    (FEditState = TEditState.LastKey) then
  begin
    FEditState := TEditState.LastWord;
    SendNotification;
  end;
end;

//send notification on keydown when KeyChar is a letter or digit and the focused control is a TEdit or TMemo
procedure TAccForm.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  Ch: Char;
begin
  Ch := KeyChar;
  inherited;
  if Assigned(FocusedCtrl) and ((FocusedCtrl is TEdit) or (FocusedCtrl is TComboEdit) or (FocusedCtrl is TMemo)) then
  begin
    FEditState := TEditState.Focus;
    if Ch.IsLetterOrDigit then
    begin
      FEditState := TEditState.LastKey;
      FEditKey := Ch;
      SendNotification;
    end;
  end;
  FEditTimer.Enabled := False;
  FEditTimer.Enabled := True;
end;

//send notification or update focused control on keyup
procedure TAccForm.KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  Ch: Char;
begin
  Ch := Chr(Key);
  inherited;
  if Ch.IsLetterOrDigit or (FocusedCtrl = nil) then
    Exit;

  if ((FocusedCtrl is TCheckBox) or (FocusedCtrl is TRadioButton)) and (Ch = ' ') then
    SendNotification
  else if not (Ch = ' ') then
  begin
    UpdateFocusedControl;
    if ((FocusedCtrl is TEdit) or (FocusedCtrl is TComboEdit) or (FocusedCtrl is TMemo)) and (Key = vkTab) then
      FEditTimer.Enabled := False;
  end;
end;

//update focused control on mouse up
procedure TAccForm.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoClick: Boolean = True);
begin
  inherited;
  FEditTimer.Enabled := False;
  if ((Focused is TEdit) or (FocusedCtrl is TComboEdit) or (Focused is TMemo)) then
    FEditMouseUp.Enabled := True
  else
    UpdateFocusedControl;
end;

procedure TAccForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FocusedCtrl) then
    FocusedCtrl := nil;
end;

//send a notification that the value or title has changed (current and previous selected index change or keyboard character input)
//or a notification that the focused control has changed
procedure TAccForm.SendNotification;
var
  Idx: Integer;
  ListItem: TListBoxItem;
  TreeItem: TTreeViewItem;
  ComboEdit: TComboEdit;
  Obj: Pointer;
begin
  if FocusedCtrl <> nil then
  begin
    Idx := -1;
    if (FocusedCtrl is TListBox) then
      Idx := TListBox(FocusedCtrl).ItemIndex
    else if (FocusedCtrl is TComboBox) then
      Idx := TComboBox(FocusedCtrl).ItemIndex
    else if (FocusedCtrl is TComboEdit) then
      Idx := TComboEdit(FocusedCtrl).ItemIndex
    else if (FocusedCtrl is TTreeView) and ((FocusedCtrl as TTreeView).Selected <> nil) then
      Idx := TTreeView(FocusedCtrl).Selected.Index;

    if (FocusedObj.ObjectArr <> nil) and (Idx >= 0) and (Idx <= Integer(FocusedObj.ObjectArr.count) - 1) then
    begin
      if (FocusedCtrl is TListBox) then
      begin
        ListItem := TListBox(FocusedCtrl).Selected;
        if ListItem <> nil then
          FocusedObj.FList[Idx].Bounds := MakeNSRect(ListItem.Position.X + ListItem.ParentControl.Position.X,
            ListItem.Position.Y + ListItem.ParentControl.Position.Y, ListItem.Width, ListItem.Height);
      end
      else if (FocusedCtrl is TComboBox) then
      begin
        ListItem := TComboBox(FocusedCtrl).Selected;
        if ListItem <> nil then
          FocusedObj.FList[Idx].Bounds := MakeNSRect(ListItem.Position.X + ListItem.ParentControl.Position.X,
            ListItem.Position.Y + ListItem.ParentControl.Position.Y, ListItem.Width, ListItem.Height);
      end
      else if (FocusedCtrl is TComboEdit) then
      begin
        ComboEdit := TComboEdit(FocusedCtrl);
        if ComboEdit.HasPresentationProxy and (ComboEdit.PresentationProxy.Receiver is TStyledComboEdit) then
          ListItem := TStyledComboEdit(ComboEdit.PresentationProxy.Receiver).ListBox.Selected
        else
          ListItem := nil;
        if ListItem <> nil then
          FocusedObj.FList[Idx].Bounds := MakeNSRect(ListItem.Position.X + ListItem.ParentControl.Position.X,
            ListItem.Position.Y + ListItem.ParentControl.Position.Y, ListItem.Width, ListItem.Height);
      end
      else if (FocusedCtrl is TTreeView) and ((FocusedCtrl as TTreeView).Selected <> nil) then
      begin
        TreeItem := TTreeView(FocusedCtrl).Selected;
        if TreeItem <> nil then
          FocusedObj.FList[Idx].Bounds := MakeNSRect(TreeItem.Position.X + TreeItem.ParentControl.Position.X,
            TreeItem.Position.Y + TreeItem.ParentControl.Position.Y, TreeItem.Width, TreeItem.Height);
      end;
      Obj := FocusedObj.ObjectArr.objectAtIndex(Idx);
    end
    else
      Obj := FocusedObj.GetObjectID;

    NSAccessibilityPostNotification(Obj, (CocoaNSStringConst(AppKitf,
      'NSAccessibilityFocusedUIElementChangedNotification') as ILocalObject).GetObjectID);
    if ((FocusedCtrl is TEdit) or (FocusedCtrl is TComboEdit) or (FocusedCtrl is TMemo)) and
      (FEditState <> TEditState.Focus) then
      NSAccessibilityPostNotification(FocusedObj.GetObjectID,
        (CocoaNSStringConst(AppKitf, 'NSAccessibilityTitleChangedNotification') as ILocalObject).GetObjectID)
    else
      NSAccessibilityPostNotification(FocusedObj.GetObjectID,
        (CocoaNSStringConst(AppKitf, 'NSAccessibilityValueChangedNotification') as ILocalObject).GetObjectID);
  end;
end;

//creates a native NSView instance that supports the NSAccessibility protocol based on the focused control
//and adds it to the form
procedure TAccForm.UpdateFocusedControl;
var
  Win: NSWindow;
  View: NSView;
  R: NSRect;
  ListItem: TListBoxItem;
  TreeItem: TTreeViewItem;
  TreeItemParent: TTreeViewItem;
  AccessibilityObj: TFMXNSObject;
  SGrid: TCustomGrid;
  SCol: TColumn;
  ParentPos: TPointF;
begin
  Win := WindowHandleToPlatform(Self.Handle).Wnd;
  if Win <> nil then
  begin
    View := TNSView.Wrap(TNSView.Wrap(Win.contentView).subviews.objectAtIndex(0));
    if (View <> nil) and (Focused <> nil) then
    begin
      if FocusedCtrl = Focused as TControl then
      begin
        if (Focused.GetObject.Name = '') and (Focused.Parent <> nil) and (Focused.Parent is TColumn) then
        begin
          SCol := TColumn(Focused.Parent);
          if SCol.PresentedControl is TCustomGrid then
            SGrid := TCustomGrid(SCol.PresentedControl)
          else
            SGrid := nil;

          if (SGrid.ColumnIndex <> FPrevCell.X) or (SGrid.Selected <> FPrevCell.y) then
          begin
            FPrevCell := Point(SGrid.ColumnIndex, SGrid.Selected);
            SendNotification;
            Exit;
          end;
        end;


        if (FocusedCtrl is TCustomGrid) then
        begin
          SGrid := TCustomGrid(FocusedCtrl);
          if (SGrid.ColumnIndex <> FPrevCell.X) or (SGrid.Selected <> FPrevCell.y) then
          begin
            FPrevCell := Point(SGrid.ColumnIndex, SGrid.Selected);
            SendNotification;
            Exit;
          end;
        end
        else if (FocusedCtrl is TListBox) then
        begin
          if FPrevIdx <> TListBox(FocusedCtrl).ItemIndex then
          begin
            FPrevIdx := TListBox(FocusedCtrl).ItemIndex;
            SendNotification;
          end;
        end
        else if (FocusedCtrl is TComboBox) then
        begin
          if FPrevIdx <> TComboBox(FocusedCtrl).ItemIndex then
          begin
            FPrevIdx := TComboBox(FocusedCtrl).ItemIndex;
            SendNotification;
          end;
        end
        else if (FocusedCtrl is TComboEdit) then
        begin
          if FPrevIdx <> TComboEdit(FocusedCtrl).ItemIndex then
          begin
            FPrevIdx := TComboEdit(FocusedCtrl).ItemIndex;
            SendNotification;
          end;
        end
        else if (FocusedCtrl is TSpinBox) then
        begin
          if FPrevStr <> TSpinBox(FocusedCtrl).Text then
          begin
            FPrevStr := TSpinBox(FocusedCtrl).Text;
            SendNotification;
            Exit;
          end;
        end
        else if ((FocusedCtrl is TEdit) or (FocusedCtrl is TComboEdit) or (FocusedCtrl is TMemo)) then
        begin
          SendNotification;
          Exit;
        end
        else if (FocusedCtrl is TTreeView) then
        begin
          if FPrevSel <> TTreeView(FocusedCtrl).Selected then
          begin
            FPrevSel := TTreeView(FocusedCtrl).Selected;
            SendNotification;
          end;
        end;
      end
      else
      begin
        FPrevIdx := -1;
        FPrevSel := nil;
        FPrevStr := '';
        FPrevEx := 0;
        FPrevCell := Point(-1,-1);
      end;

      PrevCtrl := FocusedCtrl;

      if (Focused is TControl) and (TControl(Focused).Owner is TColumn) then
        FocusedCtrl := TColumn(TControl(Focused).Owner).PresentedControl
      else
        FocusedCtrl := Focused as TControl;

      if FocusedCtrl <> nil then
      begin
        if (FocusedObj <> nil) and (FocusedCtrl <> PrevCtrl) then
        begin
          FocusedObj.NativeView.removeFromSuperview;
          FocusedObj.NativeView.release;
          FocusedObj.Free;
          FocusedObj := nil;
        end;

        ParentPos := GetParentPosition(FocusedCtrl);
        R := MakeNSRect(FocusedCtrl.Position.X + ParentPos.X, -ParentPos.Y -FocusedCtrl.Position.Y + ClientHeight - FocusedCtrl.Height,
          FocusedCtrl.Width, FocusedCtrl.Height);

        FocusedObj := TFMXNSView.Create;
        FocusedObj.Form := Self;
        FocusedObj.NativeView.setFrame(R);
        FocusedObj.ViewControl := FocusedCtrl;

        if FocusedObj.ViewControl is TCustomGrid then
        begin
          SGrid := TCustomGrid(FocusedObj.ViewControl);
          FocusedObj.FObjectArr.removeAllObjects;
          FocusedObj.FList.Clear;
          AccessibilityObj := TFMXNSObject.Create;
          AccessibilityObj.Parent := FocusedObj.NativeView;
          AccessibilityObj.ObjectArr := FocusedObj.FObjectArr;
          AccessibilityObj.ViewControl := SGrid;
          if (SGrid is TStringGrid) and (SGrid.ColumnIndex >= 0) and (SGrid.ColumnIndex < SGrid.ColumnCount) and
            (SGrid.Selected >= 0) and (SGrid.Selected < SGrid.RowCount) then
            AccessibilityObj.Text := TStringGrid(SGrid).Cells[SGrid.ColumnIndex, SGrid.Selected]
          else
            AccessibilityObj.Text := Format('Col: %d; Row: %d', [SGrid.ColumnIndex, SGrid.Selected]);
          FocusedObj.FObjectArr.addObject(AccessibilityObj.GetObjectID);
          FocusedObj.FList.Add(AccessibilityObj);
        end;

        if FocusedObj.ViewControl is TListBox then
        begin
          FocusedObj.FObjectArr.removeAllObjects;
          FocusedObj.FList.Clear;
          ListItem := TListBox(FocusedObj.ViewControl).Selected;
          if ListItem <> nil then
          begin
            R := MakeNSRect(R.origin.x + ListItem.Position.X + ListItem.ParentControl.Position.X,
              R.origin.y - ListItem.Position.Y - ListItem.ParentControl.Position.Y + FocusedObj.ViewControl.Height - ListItem.Height,
              ListItem.Width, ListItem.Height);
            FocusedObj.NativeView.setFrame(R);
          end;
        end;

        if FocusedObj.ViewControl is TComboBox then
        begin
          FocusedObj.FObjectArr.removeAllObjects;
          FocusedObj.FList.Clear;
        end;

        if FocusedObj.ViewControl is TComboEdit then
        begin
          FocusedObj.FObjectArr.removeAllObjects;
          FocusedObj.FList.Clear;
        end;

        if FocusedObj.ViewControl is TTreeView then
        begin
          TreeItem := TTreeView(FocusedObj.ViewControl).Selected;
          if TreeItem <> nil then
          begin
            TreeItemParent := TreeItem.ParentItem;
            if TreeItemParent <> nil then
              R := MakeNSRect(R.origin.x + TreeItem.Position.X + TreeItem.ParentControl.Position.X + TreeItemParent.Position.X,
                R.origin.y - TreeItem.Position.Y - TreeItem.ParentControl.Position.Y - TreeItemParent.Position.Y + FocusedObj.ViewControl.Height - TreeItem.Height,
                TreeItem.Width, TreeItem.Height)
            else
              R := MakeNSRect(R.origin.x + TreeItem.Position.X + TreeItem.ParentControl.Position.X,
                R.origin.y - TreeItem.Position.Y - TreeItem.ParentControl.Position.Y + FocusedObj.ViewControl.Height - TreeItem.Height,
                TreeItem.Width, TreeItem.Height);
            FocusedObj.NativeView.setFrame(R);
          end;
          FocusedObj.FObjectArr.removeAllObjects;
          FocusedObj.FList.Clear;
        end;

        if (FocusedCtrl is TListBox) then
          FPrevIdx := TListBox(FocusedCtrl).ItemIndex
        else if (FocusedCtrl is TComboBox) then
          FPrevIdx := TComboBox(FocusedCtrl).ItemIndex
        else if (FocusedCtrl is TComboEdit) then
          FPrevIdx := TComboEdit(FocusedCtrl).ItemIndex
        else if (FocusedCtrl is TSpinBox) then
          FPrevStr := TSpinBox(FocusedCtrl).Text
        else if (FocusedCtrl is TTreeView) then
          FPrevSel := TTreeView(FocusedCtrl).Selected
        else if (FocusedCtrl is TCustomGrid) then
          FPrevCell := Point(TCustomGrid(FocusedCtrl).ColumnIndex, TCustomGrid(FocusedCtrl).Selected);

        View.addSubview(FocusedObj.NativeView);
        Win.makeFirstResponder(FocusedObj.NativeView);
      end;
    end;
  end;
end;

{ TFMXNSView }

//returns a standard description for an action
function TFMXNSView.accessibilityActionDescription(action: Pointer): NSString;
begin
  Result := NSView(super).accessibilityActionDescription(action);
end;

//returns an array of supported action names
function TFMXNSView.accessibilityActionNames: NSArray;
begin
  Result := NSView(super).accessibilityActionNames;
end;

//returns the count of the specified accessibility array attribute
function TFMXNSView.accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger;
begin
  Result := NSView(super).accessibilityArrayAttributeCount(attribute)
end;

//returns a subarray of values of an accessibility array attribute
function TFMXNSView.accessibilityArrayAttributeValues(attribute: Pointer; index, maxCount: NSUInteger): NSArray;
begin
  Result := NSView(super).accessibilityArrayAttributeValues(attribute, index, maxCount);
end;

//returns an array of supported attribute names
function TFMXNSView.accessibilityAttributeNames: NSArray;
var
  AccAttribs: array of Pointer;
begin
  SetLength(AccAttribs, 19);
  AccAttribs[0] := (StrToNSStr('AXRole') as ILocalObject).GetObjectID;
  AccAttribs[1] := (StrToNSStr('AXRoleDescription') as ILocalObject).GetObjectID;
  AccAttribs[2] := (StrToNSStr('AXParent') as ILocalObject).GetObjectID;
  AccAttribs[3] := (StrToNSStr('AXPosition') as ILocalObject).GetObjectID;
  AccAttribs[4] := (StrToNSStr('AXSize') as ILocalObject).GetObjectID;
  AccAttribs[5] := (StrToNSStr('AXWindow') as ILocalObject).GetObjectID;
  AccAttribs[6] := (StrToNSStr('AXTopLevelUIElement') as ILocalObject).GetObjectID;
  AccAttribs[7] := (StrToNSStr('AXValue') as ILocalObject).GetObjectID;
  AccAttribs[8] := (StrToNSStr('AXEnabled') as ILocalObject).GetObjectID;
  AccAttribs[9] := (StrToNSStr('AXFocused') as ILocalObject).GetObjectID;
  AccAttribs[10] := (StrToNSStr('AXDescription') as ILocalObject).GetObjectID;
  AccAttribs[11] := (StrToNSStr('AXChildren') as ILocalObject).GetObjectID;
  AccAttribs[12] := (StrToNSStr('AXVisibleChildren') as ILocalObject).GetObjectID;
  AccAttribs[13] := (StrToNSStr('AXSelectedChildren') as ILocalObject).GetObjectID;
  AccAttribs[14] := (StrToNSStr('AXRowIndexRange') as ILocalObject).GetObjectID;
  AccAttribs[15] := (StrToNSStr('AXColumnIndexRange') as ILocalObject).GetObjectID;
  AccAttribs[16] := (StrToNSStr('AXSelectedText') as ILocalObject).GetObjectID;
  AccAttribs[17] := (StrToNSStr('AXSelectedTextRange') as ILocalObject).GetObjectID;
  AccAttribs[18] := (StrToNSStr('AXTitle') as ILocalObject).GetObjectID;

  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@AccAttribs[0], Length(AccAttribs)));
end;

// Returns the value of the parameterized attribute corresponding to the specified attribute name and parameter
function TFMXNSView.accessibilityAttributeValue(attribute, parameter: Pointer): Pointer;
begin
  Result := NSView(Super).accessibilityAttributeValue(attribute, parameter);
end;

// Returns the value of the specified attribute
function TFMXNSView.accessibilityAttributeValue(attribute: Pointer): Pointer;

  function GetSelectedItemFromComboEdit(const AComboEdit: TComboEdit): TListBoxItem;
  begin
    if AComboEdit.HasPresentationProxy and (AComboEdit.PresentationProxy.Receiver is TStyledComboEdit) then
      Result := TStyledComboEdit(AComboEdit.PresentationProxy.Receiver).ListBox.Selected
    else
      Result := nil;
  end;

var
  AttributeValue: Pointer;
  Idx: Integer;
  FmxAcc: IFMXAccessibility;
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
  RttiPropText: TRttiProperty;
  RttiPropVal: TRttiProperty;
  ValCtrl: Boolean;
  Ex: Double;
  Range: NSRange;
  Str: string;
  Selection: Integer;
  Ch: string;
  AttribString: string;
begin
  AttributeValue := nil;
  AttribString := UTF8ToString(TNSString.Wrap(attribute).UTF8String);

  if AttribString = 'AXRowIndexRange' then
  begin
    if ViewControl is TCustomGrid then
    begin
      Range.location := TCustomGrid(ViewControl).Selected;
      Range.length := 1;
      AttributeValue := TNSValue.OCClass.valueWithRange(Range);
    end;
  end
  else if AttribString = 'AXColumnIndexRange' then
  begin
    if ViewControl is TCustomGrid then
    begin
      Range.location := TCustomGrid(ViewControl).ColumnIndex;
      Range.length := 1;
      AttributeValue := TNSValue.OCClass.valueWithRange(Range);
    end;
  end
  else if AttribString = 'AXSelectedText' then
  begin
    if ViewControl is TEdit then
    begin
      if TEdit(ViewControl).SelText <> '' then
        AttributeValue := (StrToNSStr(TEdit(ViewControl).SelText) as ILocalObject).GetObjectID;
    end
    else if ViewControl is TComboEdit then
    begin
      if TComboEdit(ViewControl).SelText <> '' then
        AttributeValue := (StrToNSStr(TComboEdit(ViewControl).SelText) as ILocalObject).GetObjectID;
    end
    else if ViewControl is TMemo then
    begin
      if TMemo(ViewControl).SelText <> '' then
        AttributeValue := (StrToNSStr(TMemo(ViewControl).SelText) as ILocalObject).GetObjectID;
    end;
  end
  else if AttribString = 'AXSelectedTextRange' then
  begin
    if ViewControl is TEdit then
    begin
      if TEdit(ViewControl).SelText <> '' then
      begin
        Range.location := TEdit(ViewControl).SelStart;
        Range.length := TEdit(ViewControl).SelLength;
        AttributeValue := TNSValue.OCClass.valueWithRange(Range);
      end;
    end
    else if ViewControl is TComboEdit then
    begin
      if TComboEdit(ViewControl).SelText <> '' then
      begin
        Range.location := TComboEdit(ViewControl).SelStart;
        Range.length := TComboEdit(ViewControl).SelLength;
        AttributeValue := TNSValue.OCClass.valueWithRange(Range);
      end;
    end
    else if ViewControl is TMemo then
    begin
      if TMemo(ViewControl).SelText <> '' then
      begin
        Range.location := TMemo(ViewControl).SelStart;
        Range.length := TMemo(ViewControl).SelLength;
        AttributeValue := TNSValue.OCClass.valueWithRange(Range);
      end;
    end;
  end
  else if AttribString = 'AXRole' then
  begin
    AttributeValue := (StrToNSStr('AXStaticText') as ILocalObject).GetObjectID;
    if ViewControl.GetInterface(IFMXAccessibility, FmxAcc) then
      AttributeValue := (StrToNSStr(GetRoleText(FmxAcc.GetRole)) as ILocalObject).GetObjectID
    else if (ViewControl is TButton) then
      AttributeValue := (StrToNSStr('AXButton') as ILocalObject).GetObjectID
    else if (ViewControl is TCustomGrid) then
      AttributeValue := (StrToNSStr('AXCell') as ILocalObject).GetObjectID
    else if (ViewControl is TEdit) and not TEdit(ViewControl).ReadOnly then
      AttributeValue := (StrToNSStr('AXTextField') as ILocalObject).GetObjectID
    else if (ViewControl is TMemo) then
      AttributeValue := (StrToNSStr('AXTextArea') as ILocalObject).GetObjectID
    else if ViewControl is TTreeView then
      AttributeValue := (StrToNSStr('AXContentList') as ILocalObject).GetObjectID
    else if ViewControl is TListBox then
      AttributeValue := (StrToNSStr('AXContentList') as ILocalObject).GetObjectID
    else if (ViewControl is TCombobox) or (ViewControl is TComboEdit) then
      AttributeValue := (StrToNSStr('AXComboBox') as ILocalObject).GetObjectID
    else if ViewControl is TCheckBox then
      AttributeValue := (StrToNSStr('AXCheckBox') as ILocalObject).GetObjectID
    else if ViewControl is TTrackBar then
      AttributeValue := (StrToNSStr('AXSlider') as ILocalObject).GetObjectID
    else if ViewControl is TSpinBox then
      AttributeValue := (StrToNSStr('AXIncrementor') as ILocalObject).GetObjectID
    else if ViewControl is TRadioButton then
      AttributeValue := (StrToNSStr('AXRadioButton') as ILocalObject).GetObjectID;
  end
  else if AttribString = 'AXRoleDescription' then
  begin
    AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXStaticText') as ILocalObject).GetObjectID, nil);
    if ViewControl.GetInterface(IFMXAccessibility, FmxAcc) then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr(GetRoleText(FmxAcc.GetRole)) as ILocalObject).GetObjectID, nil)
    else if (ViewControl is TCustomGrid) then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXCell') as ILocalObject).GetObjectID, nil)
    else if (ViewControl is TButton) then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXButton') as ILocalObject).GetObjectID, nil)
    else if (ViewControl is TEdit) and not TEdit(ViewControl).ReadOnly then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXTextField') as ILocalObject).GetObjectID, nil)
    else if (ViewControl is TMemo) then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXTextArea') as ILocalObject).GetObjectID, nil)
    else if ViewControl is TTreeView then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXList') as ILocalObject).GetObjectID, nil)
    else if ViewControl is TListBox then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXList') as ILocalObject).GetObjectID, nil)
    else if (ViewControl is TComboBox) or (ViewControl is TComboEdit) then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXComboBox') as ILocalObject).GetObjectID, nil)
    else if ViewControl is TCheckBox then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXCheckBox') as ILocalObject).GetObjectID, nil)
    else if ViewControl is TTrackBar then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXSlider') as ILocalObject).GetObjectID, nil)
    else if ViewControl is TSpinBox then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXIncrementor') as ILocalObject).GetObjectID, nil)
    else if ViewControl is TRadioButton then
      AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXRadioButton') as ILocalObject).GetObjectID, nil);
  end
  else if AttribString = 'AXVisibleChildren' then
  begin
    if FObjectArr.count > 0 then
      AttributeValue := NSAccessibilityUnignoredChildren((FObjectArr as ILocalObject).GetObjectID)
    else
      AttributeValue := NSAccessibilityUnignoredChildren(objc_msgSend((TNSArray.OCClass as ILocalObject).GetObjectID, sel_getUid(MarshaledAString('array'))));
  end
  else if AttribString = 'AXTitle' then
  begin
    if (ViewControl is TEdit) then
    begin
      if Form <> nil then
      begin
        case Form.FEditState of
          TEditState.LastKey:
            AttributeValue := (StrToNSStr(Form.FEditKey) as ILocalObject).GetObjectID;
          TEditState.LastWord:
            begin
              Str := '';
              Selection := TEdit(ViewControl).SelStart;
              while (Selection > 0) do
              begin
                Ch := TEdit(ViewControl).Text.Substring(Selection - 1, 1);
                if Ch <> ' ' then
                begin
                  Str := Ch + Str;
                  Selection := Selection - 1;
                end
                else
                  Break;
              end;
            AttributeValue := (StrToNSStr(Str) as ILocalObject).GetObjectID;
            Form.FEditState := TEditState.Focus;
          end;
        end;
      end;
    end
    else if (ViewControl is TComboEdit) then
    begin
      if Form <> nil then
      begin
        case Form.FEditState of
          TEditState.LastKey:
            AttributeValue := (StrToNSStr(Form.FEditKey) as ILocalObject).GetObjectID;
          TEditState.LastWord:
            begin
              Str := '';
              Selection := TComboEdit(ViewControl).SelStart;
              while (Selection > 0) do
              begin
                Ch := TComboEdit(ViewControl).Text.Substring(Selection - 1, 1);
                if Ch <> ' ' then
                begin
                  Str := Ch + Str;
                  Selection := Selection - 1;
                end
                else
                  Break;
              end;
            AttributeValue := (StrToNSStr(Str) as ILocalObject).GetObjectID;
            Form.FEditState := TEditState.Focus;
          end;
        end;
      end;
    end
    else if (ViewControl is TMemo) then
    begin
      if Form <> nil then
      begin
        case Form.FEditState of
          TEditState.Focus:
            AttributeValue := (StrToNSStr((ViewControl as TMemo).Text) as ILocalObject).GetObjectID;
          TEditState.LastKey:
            AttributeValue := (StrToNSStr(Form.FEditKey) as ILocalObject).GetObjectID;
          TEditState.LastWord:
            begin
              Str := '';
              Selection := TMemo(ViewControl).SelStart;
              while (Selection > 0) do
              begin
                Ch := TMemo(ViewControl).Text.Substring(Selection - 1, 1);
                if Ch <> ' ' then
                begin
                  Str := Ch + Str;
                  Selection := Selection - 1;
                end
                else
                  Break;
              end;
              AttributeValue := (StrToNSStr(Str) as ILocalObject).GetObjectID;
              Form.FEditState := TEditState.Focus;
            end;
        end;
      end;
    end;
  end
  else if AttribString = 'AXDescription' then
  begin
    if ViewControl.GetInterface(IFMXAccessibility, FmxAcc) then
    begin
      if not (FmxAcc.GetRole = TFMXRole.StaticText) then
        AttributeValue := (StrToNSStr(FmxAcc.GetControlText) as ILocalObject).GetObjectID
    end
    else if (ViewControl is TButton) then
      AttributeValue := (StrToNSStr(TButton(ViewControl).Text) as ILocalObject).GetObjectID
    else if ViewControl is TMemo then
      AttributeValue := nil
    else if (ViewControl is TCheckBox) then
      AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(TCheckBox(ViewControl).Text))
    else if (ViewControl is TTreeView) and (TTreeView(ViewControl).Selected <> nil) then
      AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(TTreeView(ViewControl).Selected.Text))
    else if (ViewControl is TListBox) and (TListBox(ViewControl).Selected <> nil) then
      AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(TListBox(ViewControl).Selected.Text))
    else if (ViewControl is TComboBox) and (TComboBox(ViewControl).Selected <> nil) then
      AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(TComboBox(ViewControl).Selected.Text))
    else if (ViewControl is TComboEdit) and (GetSelectedItemFromComboEdit(TComboEdit(ViewControl)) <> nil) then
      AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(GetSelectedItemFromComboEdit(TComboEdit(ViewControl)).Text))
    else if (ViewControl is TRadioButton) then
      AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(TRadioButton(ViewControl).Text))
    else if (ViewControl is TTrackBar) then
      AttributeValue := nil
    else if (ViewControl is TSpinBox) then
      AttributeValue := nil
    else if (ViewControl is TEdit) then
      AttributeValue := nil
    else
    begin
      RttiCtx := TRttiContext.Create;
      try
        RttiType := RttiCtx.GetType(ViewControl.ClassInfo);
        RttiPropText := RttiType.GetProperty('Text');
        RttiPropVal := RttiType.GetProperty('Value');

        ValCtrl := (ViewControl is TScrollBar);

        if not ValCtrl and (RttiPropText <> nil) then
           AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(RttiPropText.GetValue(ViewControl).AsString))
        else
        begin
          if RttiPropVal <> nil then
          begin
            Ex := RttiPropVal.GetValue(ViewControl).AsExtended;
            AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(Format('%.2g',[Ex])));
          end;
        end;
      finally
        RttiCtx.Free;
      end;
    end;
  end
  else if AttribString = 'AXValue' then
  begin
    if ViewControl.GetInterface(IFMXAccessibility, FmxAcc) then
    begin
      if FmxAcc.GetRole = TFMXRole.StaticText then
        AttributeValue := (StrToNSStr(FmxAcc.GetControlText) as ILocalObject).GetObjectID
    end
    else if (ViewControl is TEdit) then
    begin
      AttributeValue := (StrToNSStr(TEdit(ViewControl).Text) as ILocalObject).GetObjectID;
    end
    else if ViewControl is TCheckBox then
    begin
      if TCheckBox(ViewControl).IsChecked then
        AttributeValue := TNSNumber.OCClass.numberWithInt(1)
      else
        AttributeValue := TNSNumber.OCClass.numberWithInt(0);
    end
    else if ViewControl is TRadioButton then
    begin
      if TRadioButton(ViewControl).IsChecked then
        AttributeValue := TNSNumber.OCClass.numberWithInt(1)
      else
        AttributeValue := TNSNumber.OCClass.numberWithInt(0);
    end
    else if ViewControl is TTrackBar then
      AttributeValue := TNSNumber.OCClass.numberWithFloat(TTrackBar(ViewControl).Value)
    else if ViewControl is TSpinBox then
      AttributeValue := TNSNumber.OCClass.numberWithFloat(TSpinBox(ViewControl).Value);
  end
  else if AttribString = 'AXEnabled' then
  begin
    AttributeValue := TNSNumber.OCClass.numberWithBool(True)
  end
  else if AttribString = 'AXFocused' then
  begin
    AttributeValue := TNSNumber.OCClass.numberWithBool(False);
  end
  else if AttribString = 'AXSelectedChildren' then
  begin
    if FObjectArr.count > 0 then
    begin
      Idx := -1;
      if ViewControl is TCustomGrid then
        Idx := 0;

      if (Idx >= 0) and (Idx <= Integer(FObjectArr.count) - 1) then
        AttributeValue := NSAccessibilityUnignoredChildren(TNSArray.OCClass.arrayWithObject(FObjectArr.objectAtIndex(Idx)))
    end
    else
      AttributeValue := NSAccessibilityUnignoredChildren(objc_msgSend((TNSArray.OCClass as ILocalObject).GetObjectID, sel_getUid(MarshaledAString('array'))));
  end
  else if AttribString = 'AXChildren' then
  begin
    if (ViewControl is TComboBox) or (ViewControl is TComboEdit) or (ViewControl is TListBox) or (ViewControl is TTreeView) then
      AttributeValue := NSView(super).accessibilityAttributeValue(attribute)
    else
    begin
      if FObjectArr.count > 0 then
        AttributeValue := NSAccessibilityUnignoredChildren((FObjectArr as ILocalObject).GetObjectID)
      else
        AttributeValue := NSAccessibilityUnignoredChildren(objc_msgSend((TNSArray.OCClass as ILocalObject).GetObjectID, sel_getUid(MarshaledAString('array'))));
    end;
  end
  else
    AttributeValue :=  NSView(super).accessibilityAttributeValue(attribute);

  Result := AttributeValue;
end;

//returns the deepest descendant of the accessibility hierarchy that has the focus
function TFMXNSView.accessibilityFocusedUIElement: Pointer;
var
  Idx: Integer;
begin
  Idx := -1;
  if (Idx >= 0) and (Idx <= FList.Count - 1) then
    Result := TNSObject.Wrap(FList[Idx].GetObjectID).accessibilityFocusedUIElement
  else
    Result := GetObjectID;
end;

//returns the deepest descendant of the accessibility hierarchy that contains the specified point
function TFMXNSView.accessibilityHitTest(point: NSPoint): Pointer;
var
  HitTestResult: Pointer;
  WindowPoint, LocalPoint: NSPoint;
  I: Integer;
  Pth: NSBezierPath;
  Obj: NSObject;
  AC: TFMXNSObject;
begin
  HitTestResult := GetObjectID;
  WindowPoint := NativeView.window.convertScreenToBase(point);
  LocalPoint := NativeView.convertPoint(WindowPoint, nil);
  LocalPoint.y := -LocalPoint.y + NativeView.frame.size.height;
  for I := 0 to FObjectArr.count - 1 do
  begin
    Obj := TNSObject.Wrap(FObjectArr.objectAtIndex(I));
    AC := FList[I];
    Pth := TNSBezierPath.Wrap(TNSBezierPath.OCClass.bezierPathWithRect(AC.Bounds));
    if Pth.containsPoint(LocalPoint) then
    begin
      HitTestResult := TNSObject.Wrap(AC.GetObjectID).accessibilityHitTest(point);
      Break;
    end;
  end;

  Result := HitTestResult;
end;

//returns the index of the specified accessibility child in the parent
function TFMXNSView.accessibilityIndexOfChild(child: Pointer): NSUInteger;
begin
  if Assigned(FObjectArr) then
    Result := FObjectArr.indexOfObject(child)
  else
    Result := NSView(Super).accessibilityIndexOfChild(child);
end;

//returns a Boolean value that indicates whether the value for the specified attribute can be set
function TFMXNSView.accessibilityIsAttributeSettable(attribute: Pointer): Boolean;
begin
  Result := NSView(Super).accessibilityIsAttributeSettable(attribute);
end;

//returns a Boolean value indicating whether the view should be ignored in the parent-child accessibility hierarchy
function TFMXNSView.accessibilityIsIgnored: Boolean;
begin
  Result := False;
end;

//returns a list of supported parameterized attribute names
function TFMXNSView.accessibilityParameterizedAttributeNames: NSArray;
begin
  Result := NSView(Super).accessibilityParameterizedAttributeNames;
end;

//performs the action associated with the specified action
procedure TFMXNSView.accessibilityPerformAction(action: Pointer);
begin
  NSView(Super).accessibilityPerformAction(action);
end;

//overrides the specified attribute, or adds it if it does not exist, and sets its value to the specified value
function TFMXNSView.accessibilitySetOverrideValue(value, attribute: Pointer): Boolean;
begin
  Result := NSView(Super).accessibilitySetOverrideValue(value, attribute);
end;

//sets the value of the specified attribute to the specified value
procedure TFMXNSView.accessibilitySetValue(value, attribute: Pointer);
begin
  NSView(Super).accessibilitySetValue(value, attribute);
end;

//creates an instance of NSView and an array to hold children of type NSObject
constructor TFMXNSView.Create;
var
  V: Pointer;
  AutoReleasePool: NSAutoreleasePool;
begin
  FObjectArr := TNSMutableArray.Wrap(TNSMutableArray.Wrap(TNSMutableArray.OCClass.alloc).init);
  FList := TList<TFMXNSObject>.Create;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    inherited Create;
    V := NSView(Super).init;
    if GetObjectID <> V then
      UpdateObjectID(V);
  finally
    AutoReleasePool.release;
  end;
end;

destructor TFMXNSView.Destroy;
begin
  FList.Free;
  FList := nil;
  if FObjectArr <> nil then
  begin
    FObjectArr.release;
    FObjectArr := nil;
  end;
  inherited;
end;

function TFMXNSView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(NSViewX);
end;

//used to return the role of a focused control that is not supported by default
function TFMXNSView.GetRoleText(ARole: TFMXRole): String;
begin
  case ARole of
    TFMXRole.Cell: Result := 'AXCell';
    TFMXRole.StaticText: Result := 'AXStaticText';
    TFMXRole.Text: Result := 'AXTextField';
    TFMXRole.Grid: Result := 'AXGrid';
    TFMXRole.Button: Result := 'AXButton';
    TFMXRole.CheckBox: Result := 'AXCheckBox';
    TFMXRole.RadioButton: Result := 'AXRadioButton';
    TFMXRole.List: Result := 'AXContentList';
    TFMXRole.Slider: Result := 'AXSlider';
    TFMXRole.ComboBox: Result := 'AXComboBox';
    TFMXRole.TabGroup: Result := 'AXTabGroup'
  end;
end;

//returns a reference to the native NSView instance
function TFMXNSView.NativeView: NSView;
begin
  Result := NSView(Super);
end;

{ TFMXNSObject }

//returns a standard description for an action
function TFMXNSObject.accessibilityActionDescription(action: Pointer): NSString;
begin
  Result := TNSString.Wrap(NSAccessibilityActionDescription(action));
end;

//returns an array of supported action names
function TFMXNSObject.accessibilityActionNames: NSArray;
begin
  Result  := TNSArray.Wrap(TNSArray.OCClass.arrayWithObject((StrToNSStr('AXPress') as ILocalObject).GetObjectID));
end;

//returns the count of the specified accessibility array attribute
function TFMXNSObject.accessibilityArrayAttributeCount(attribute: Pointer): NSUInteger;
begin
  Result := NSObject(Super).accessibilityArrayAttributeCount(attribute)
end;

//returns a subarray of values of an accessibility array attribute
function TFMXNSObject.accessibilityArrayAttributeValues(attribute: Pointer; index, maxCount: NSUInteger): NSArray;
begin
  Result := NSObject(Super).accessibilityArrayAttributeValues(attribute, index, maxCount);
end;

//returns an array of supported attribute names
function TFMXNSObject.accessibilityAttributeNames: NSArray;
var
  AttribNames: array of pointer;
begin
  SetLength(AttribNames, 20);
  AttribNames[0] := (StrToNSStr('AXRole') as ILocalObject).GetObjectID;
  AttribNames[1] := (StrToNSStr('AXSubRole') as ILocalObject).GetObjectID;
  AttribNames[2] := (StrToNSStr('AXRoleDescription') as ILocalObject).GetObjectID;
  AttribNames[3] := (StrToNSStr('AXChildren') as ILocalObject).GetObjectID;
  AttribNames[4] := (StrToNSStr('AXHelp') as ILocalObject).GetObjectID;
  AttribNames[5] := (StrToNSStr('AXParent') as ILocalObject).GetObjectID;
  AttribNames[6] := (StrToNSStr('AXPosition') as ILocalObject).GetObjectID;
  AttribNames[7] := (StrToNSStr('AXSize') as ILocalObject).GetObjectID;
  AttribNames[8] := (StrToNSStr('AXTitle') as ILocalObject).GetObjectID;
  AttribNames[9] := (StrToNSStr('AXDescription') as ILocalObject).GetObjectID;
  AttribNames[10] := (StrToNSStr('AXValue') as ILocalObject).GetObjectID;
  AttribNames[11] := (StrToNSStr('AXFocused') as ILocalObject).GetObjectID;
  AttribNames[12] := (StrToNSStr('AXEnabled') as ILocalObject).GetObjectID;
  AttribNames[13] := (StrToNSStr('AXWindow') as ILocalObject).GetObjectID;
  AttribNames[14] := (StrToNSStr('AXVisited') as ILocalObject).GetObjectID;
  AttribNames[15] := (StrToNSStr('AXLinkedUIElements') as ILocalObject).GetObjectID;
  AttribNames[16] := (StrToNSStr('AXSelected') as ILocalObject).GetObjectID;
  AttribNames[17] := (StrToNSStr('AXBlockQuoteLevel') as ILocalObject).GetObjectID;
  AttribNames[18] := (StrToNSStr('AXTopLevelUIElement') as ILocalObject).GetObjectID;
  AttribNames[19] := (StrToNSStr('AXAriaBusy') as ILocalObject).GetObjectID;
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@AttribNames[0], Length(AttribNames)));
end;

//returns the value of the parameterized attribute corresponding to the specified attribute name and parameter
function TFMXNSObject.accessibilityAttributeValue(attribute, parameter: Pointer): Pointer;
begin
  Result := NSObject(Super).accessibilityAttributeValue(attribute, parameter);
end;

//returns the value of the specified attribute
function TFMXNSObject.accessibilityAttributeValue(attribute: Pointer): Pointer;
var
  AttributeValue: Pointer;
  Pt: NSPoint;
  Idx, count: Integer;
  Str: string;
  Pos: TPointF;
  AttribStr: string;
begin
  AttributeValue := nil;
  AttribStr := UTF8ToString(TNSString.Wrap(attribute).UTF8String);
  if AttribStr = 'AXSelected' then
    AttributeValue := TNSNumber.OCClass.numberWithBool(False)
  else if AttribStr = 'AXRole' then
    AttributeValue := (StrToNSStr('AXStaticText') as ILocalObject).GetObjectID
  else if AttribStr = 'AXRoleDescription' then
    AttributeValue := NSAccessibilityRoleDescription((StrToNSStr('AXStaticText') as ILocalObject).GetObjectID, nil)
  else if AttribStr = 'AXParent' then
    AttributeValue := (Parent as ILocalObject).GetObjectID
  else if AttribStr = 'AXWindow' then
    AttributeValue := Parent.accessibilityAttributeValue(attribute)
  else if AttribStr = 'AXTopLevelUIElement' then
    AttributeValue := Parent.accessibilityAttributeValue(attribute)
  else if AttribStr = 'AXValue' then
    AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(Text))
  else if AttribStr = 'AXEnabled' then
    AttributeValue := TNSNumber.OCClass.numberWithBool(True)
  else if AttribStr = 'AXHelp' then
    AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(''))
  else if AttribStr = 'AXFocused' then
    AttributeValue := TNSNumber.OCClass.numberWithBool(False)
  else if AttribStr = 'AXAriaBusy' then
    AttributeValue := TNSNumber.OCClass.numberWithBool(False)
  else if AttribStr = 'AXVisited' then
    AttributeValue := TNSNumber.OCClass.numberWithBool(False)
  else if AttribStr = 'AXBlockQuoteLevel' then
    AttributeValue := TNSNumber.OCClass.numberWithInteger(0)
  else if AttribStr = 'AXDescription' then
    AttributeValue := nil
  else if AttribStr = 'AXTitle' then
  begin
    if not (ViewControl is TCustomGrid) then
    begin
      if Assigned(ObjectArr) then
      begin
        Idx := -1;
        count := ObjectArr.count;
        Str := '( '+ IntToStr(Idx) + ' of ' + IntToStr(count) + ' )';
        AttributeValue := TNSString.OCClass.stringWithString(StrToNSStr(Str));
      end;
    end;
  end
  else if AttribStr = 'AXPosition' then
  begin
    Pt := Bounds.origin;
    Pt.x := Pt.x + Parent.frame.origin.x;
    Pt.y := -Pt.y - ViewControl.Position.Y + TNSView.Wrap(Parent.window.contentView).frame.size.height - Bounds.size.height;
    Pos := GetParentPosition(ViewControl);
    Pt.y := Pt.y - Pos.Y;
    AttributeValue := TNSValue.OCClass.valueWithPoint(Parent.window.convertBaseToScreen(Pt));
  end
  else if AttribStr = 'AXSize' then
    AttributeValue := TNSValue.OCClass.valueWithSize(Parent.convertSize(Bounds.size, nil));

  Result := AttributeValue;
end;

//returns the deepest descendant of the accessibility hierarchy that has the focus
function TFMXNSObject.accessibilityFocusedUIElement: Pointer;
begin
  Result := NSAccessibilityUnignoredAncestor(GetObjectID);
end;

//returns the deepest descendant of the accessibility hierarchy that contains the specified point
function TFMXNSObject.accessibilityHitTest(point: NSPoint): Pointer;
begin
  Result := NSAccessibilityUnignoredAncestor(GetObjectID);
end;

//returns the index of the specified accessibility child in the parent
function TFMXNSObject.accessibilityIndexOfChild(child: Pointer): NSUInteger;
begin
  Result := NSObject(Super).accessibilityIndexOfChild(child);
end;

//returns a Boolean value that indicates whether the value for the specified attribute can be set
function TFMXNSObject.accessibilityIsAttributeSettable(attribute: Pointer): Boolean;
begin
  if TNSString.Wrap(attribute).UTF8String = 'AXFocused' then
    Result := True
  else if TNSString.Wrap(attribute).UTF8String = 'AXValue' then
    Result := True
  else
    Result := False;
end;

//returns a Boolean value indicating whether the view should be ignored in the parent-child accessibility hierarchy
function TFMXNSObject.accessibilityIsIgnored: Boolean;
begin
  Result := False;
end;

//returns a list of supported parameterized attribute names
function TFMXNSObject.accessibilityParameterizedAttributeNames: NSArray;
begin
  Result := NSObject(Super).accessibilityParameterizedAttributeNames;
end;

//performs the action associated with the specified action
procedure TFMXNSObject.accessibilityPerformAction(action: Pointer);
begin
  NSObject(Super).accessibilityPerformAction(action);
end;

//overrides the specified attribute, or adds it if it does not exist, and sets its value to the specified value
function TFMXNSObject.accessibilitySetOverrideValue(value, attribute: Pointer): Boolean;
begin
  Result := NSObject(Super).accessibilitySetOverrideValue(value, attribute);
end;

//sets the value of the specified attribute to the specified value
procedure TFMXNSObject.accessibilitySetValue(value, attribute: Pointer);
begin
  NSObject(Super).accessibilitySetValue(value, attribute);
end;

//creates an instance of NSObject
constructor TFMXNSObject.Create;
var
  V: Pointer;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    inherited Create;
    V := NSObject(Super).init;
    if GetObjectID <> V then
      UpdateObjectID(V);
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXNSObject.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(NSObjectX);
end;

//returns a reference to the native NSObject instance
function TFMXNSObject.NativeObject: NSObject;
begin
  Result := TNSObject.Wrap(GetObjectID);
end;

end.
