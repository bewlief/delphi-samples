{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.VirtualKeyboard.iOS;

interface

{$SCOPEDENUMS ON}

procedure RegisterVirtualKeyboardServices;
procedure UnregisterVirtualKeyboardServices;

implementation

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Generics.Collections, System.UITypes, System.Types, 
  System.Messaging, System.Math, Macapi.ObjectiveC, Macapi.ObjCRuntime, Macapi.Helpers,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  FMX.Types, FMX.VirtualKeyboard, FMX.Platform, FMX.Forms, FMX.Platform.iOS, FMX.Consts, FMX.Helpers.iOS,
  FMX.Controls.Presentation, FMX.Controls;

const
  ToolbarHeight = 44;

type
  TStoredActiveForm = class;

  IKeyboardEvents = interface(NSObject)
  ['{72D3A7FD-DDE3-473D-9750-46C072E7B3B7}']
    { Keyboard notifications }
    procedure KeyboardWillShow(notification: Pointer); cdecl;
    procedure KeyboardWillHide(notification: Pointer); cdecl;
    procedure KeyboardDidHide(notification: Pointer); cdecl;
    procedure KeyboardDidShow(notification: Pointer); cdecl;
    procedure KeyboardWillChangeFrame(notification: Pointer); cdecl;
    procedure KeyboardDidChangeFrame(notification: Pointer); cdecl;
    { Actions }
    procedure HideVirtualKeyboard; cdecl;
    procedure CustomButtonAction(sender: Pointer); cdecl;
  end;

  TKeyboardEventHandler = class(TOCLocal)
  strict private type
    TKeyboardState = (Shown, Hidden);
  private
    FKeepFocus: Boolean;
    FVisible: Boolean;
    { Keyborad Notifications }
    procedure SendNotificationAboutKeyboardEvent(const AVKRect: TRect; const AKeyboardState: TKeyboardState);
    function GetKeyboardRect(const Notification: Pointer): TRect;
    function GetKeyboardFrame(const Notification: Pointer): NSRect;
    function InvertFrame(const AFrame: NSRect): NSRect;
  protected
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    { IKeyboardEvents }
    procedure KeyboardWillShow(notification: Pointer); cdecl;
    procedure KeyboardWillHide(notification: Pointer); cdecl;
    procedure KeyboardDidHide(notification: Pointer); cdecl;
    procedure KeyboardWillChangeFrame(notification: Pointer); cdecl;
    procedure KeyboardDidChangeFrame(notification: Pointer); cdecl;
    procedure KeyboardDidShow(notification: Pointer); cdecl;
    procedure HideVirtualKeyboard; cdecl;
    procedure CustomButtonAction(sender: Pointer); cdecl;

    property Visible: Boolean read FVisible;
  end;

  TiOSVirtualKeyboardService = class(TInterfacedObject, IFMXVirtualKeyboardService, IFMXVirtualKeyboardToolbarService)
  private const
    ToolbarAnimationDuration = 0.25;
  private
    FKeyboardHandler: TKeyboardEventHandler;
    FTransient: Boolean;
    FToolbarVisible: Boolean;
    FToolBar: UIToolBar;
    FToolBarButtons: NSMutableArray;
    FFlexibleSepararator: UIBarButtonItem;
    FHideButton: UIBarButtonItem;
    FButtons: TList<TVirtualKeyboardToolButton>;
    FUpdatingButtons: Boolean;
    FToolbarEnabled: Boolean;
    FHideButtonVisible: Boolean;
    FStoredActiveForm: TStoredActiveForm;
    FKeyboardFrame: NSRect;
    FAnimationDuration: NSTimeInterval;
    procedure SetToolbarVisible(const Value: Boolean);
    procedure SetToolbarFrame(const Frame: NSRect; const UseAnimation: Boolean);
    function GetToolbarFrame: NSRect;
    procedure RefreshToolbarButtons;
    procedure RefreshToolbarPosition;
    procedure CreateToolbar;
    procedure ChangeToolbarOrientation;
    procedure SetKeyboardFrame(const Rect: NSRect);
    procedure SetKeyboardAnimationDuration(const AInterval: NSTimeInterval);
    property ToolbarVisible: Boolean read FToolbarVisible write SetToolbarVisible;
  private
    function IsNativeControl(AControl: TFmxObject): Boolean;
    function ExtractForm(const AControl: TFmxObject): TCommonCustomForm;
    function GetFormView(const AControl: TFmxObject): UIView;
    function GetView(const AFocusedControl: TFmxObject): UIView;
    { Messaging }
    procedure ApplicationEventHandler(const Sender: TObject; const M: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXVirtualKeyboardService }
    function ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
    function HideVirtualKeyboard: Boolean;
    procedure SetTransientState(Value: Boolean);
    function GetVirtualKeyboardState: TVirtualKeyboardStates;
    property VirtualKeyboardState: TVirtualKeyboardStates read GetVirtualKeyboardState;
    { IFMXVirtualKeyboardToolbarService }
    procedure SetToolbarEnabled(const Value: Boolean);
    function IsToolbarEnabled: Boolean;
    procedure SetHideKeyboardButtonVisibility(const Value: Boolean);
    function IsHideKeyboardButtonVisible: Boolean;
    function AddButton(const Title: string; AOnClick: TNotifyEvent): TVirtualKeyboardToolButton;
    procedure DeleteButton(const Index: Integer);
    function ButtonsCount: Integer;
    function GetButtonByIndex(const Index: Integer): TVirtualKeyboardToolButton;
    procedure ClearButtons;
  end;

  TiOSVKToolbarButton = class(TVirtualKeyboardToolButton)
  protected
    procedure DoChanged; override;
  end;

  TStoredActiveForm = class(TComponent)
  private
    [Weak] FForm: TCommonCustomForm;
    procedure SetForm(const Value: TCommonCustomForm);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Form: TCommonCustomForm read FForm write SetForm;
  end;

var
  KeyboardService: TiOSVirtualKeyboardService;

procedure RegisterVirtualKeyboardServices;
begin
  KeyboardService := TiOSVirtualKeyboardService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, KeyboardService);
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardToolbarService, KeyboardService);
end;

procedure UnregisterVirtualKeyboardServices;
begin
  if TPlatformServices.Current <> nil then
  begin
    TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardService);
    TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardToolbarService);
  end;
  FreeAndNil(KeyboardService);
end;

{ TKeyboardEventHandler }

constructor TKeyboardEventHandler.Create;
var
  NotificationCenter: NSNotificationCenter;
begin
  inherited;
  NotificationCenter := TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter);
  NotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardWillShow:'), NSObjectToID(UIKeyboardWillShowNotification), nil);
  NotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardWillHide:'), NSObjectToID(UIKeyboardWillHideNotification), nil);
  NotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardDidHide:'), NSObjectToID(UIKeyboardDidHideNotification), nil);
  NotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardDidShow:'), NSObjectToID(UIKeyboardDidShowNotification), nil);
  NotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardWillChangeFrame:'), NSObjectToID(UIKeyboardWillChangeFrameNotification), nil);
  NotificationCenter.addObserver(GetObjectID, sel_getUid('KeyboardDidChangeFrame:'), NSObjectToID(UIKeyboardDidChangeFrameNotification), nil);
end;

procedure TKeyboardEventHandler.CustomButtonAction(sender: Pointer);
var
  Index: Integer;
begin
  Index := TUIBarButtonItem.Wrap(sender).tag - 1;
  if Index >= 0 then
    TiOSVKToolbarButton(KeyboardService.FButtons[Index]).DoExecute;
end;

destructor TKeyboardEventHandler.Destroy;
begin
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(GetObjectID);
  inherited;
end;

function TKeyboardEventHandler.GetKeyboardFrame(const Notification: Pointer): NSRect;
var
  ScreenService: IFMXScreenService;
  Orientation: TScreenOrientation;
begin
  Result := iOSapi.UIKit.TNSValue.Wrap(TNSNotification.Wrap(Notification).userInfo.valueForKey(UIKeyboardFrameEndUserInfoKey)).CGRectValue;

  if not TOSVersion.Check(8) then
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
      Orientation := ScreenService.GetScreenOrientation
    else
      Orientation := TScreenOrientation.Portrait;

    case Orientation of
      TScreenOrientation.InvertedPortrait:
        Result.origin.y := Screen.Size.Height - Result.origin.y - Result.size.height;
      TScreenOrientation.InvertedLandscape:
      begin
        Result := InvertFrame(Result);
        Result.origin.y := Screen.Size.Height - Result.origin.y - Result.size.height;
      end;
      TScreenOrientation.Landscape:
        Result := InvertFrame(Result);
    end;
  end;
end;

function TKeyboardEventHandler.GetKeyboardRect(const Notification: Pointer): TRect;
var
  OCRect: NSRect;
begin
  OCRect := GetKeyboardFrame(Notification);
  Result := TRect.Create(OCRect.origin.ToPointF.Round, Round(OCRect.size.width), Round(OCRect.size.height));
end;

function TKeyboardEventHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IKeyboardEvents);
end;

procedure TKeyboardEventHandler.HideVirtualKeyboard;
var
  VKService: IFMXVirtualKeyboardService;
begin
  try
    Screen.ActiveForm.Focused := nil;
    if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, VKService) then
      VKService.HideVirtualKeyboard;
  except
    Application.HandleException(Screen.ActiveForm);
  end;
end;

function TKeyboardEventHandler.InvertFrame(const AFrame: NSRect): NSRect;
begin
  Result.origin.x := AFrame.origin.y;
  Result.origin.y := AFrame.origin.x;
  Result.size.width := AFrame.size.height;
  Result.size.height := AFrame.size.width;
end;

procedure TKeyboardEventHandler.KeyboardDidChangeFrame(notification: Pointer);
begin
  KeyboardService.ChangeToolbarOrientation;
end;

procedure TKeyboardEventHandler.KeyboardDidHide(notification: Pointer);
begin
  FVisible := False;
  KeyboardService.ToolbarVisible := False;
end;

procedure TKeyboardEventHandler.KeyboardDidShow(notification: Pointer);
begin
  FVisible := True;
end;

procedure TKeyboardEventHandler.KeyboardWillChangeFrame(notification: Pointer);
var
  KeyboardFrame: NSRect;
begin
  KeyboardFrame := GetKeyboardFrame(Notification);
  KeyboardService.SetKeyboardFrame(KeyboardFrame);
  if KeyboardService.IsToolbarEnabled then
    KeyboardService.RefreshToolbarPosition
end;

procedure TKeyboardEventHandler.KeyboardWillHide(notification: Pointer);
var
  VKRect: TRect;
  Duration: Double;
begin
  if (TVirtualKeyboardState.Visible in KeyboardService.VirtualKeyboardState) or KeyboardService.ToolbarVisible then
  begin
    KeyboardService.ToolbarVisible := False;
    Duration := TNSNumber.Wrap(TNSNotification.Wrap(Notification).userInfo.valueForKey(UIKeyboardAnimationDurationUserInfoKey)).doubleValue;
    KeyboardService.SetKeyboardAnimationDuration(Duration);
    VKRect := GetKeyboardRect(notification);
    if KeyboardService.IsToolbarEnabled then
      VKRect.Bottom := VKRect.Bottom + ToolbarHeight;
    if not FKeepFocus then
      HideVirtualKeyboard;
    SendNotificationAboutKeyboardEvent(VKRect, TKeyboardState.Hidden);
  end;
end;

procedure TKeyboardEventHandler.KeyboardWillShow(notification: Pointer);
var
  VKRect: TRect;
  Duration: NSTimeInterval;
begin
  VKRect := GetKeyboardRect(notification);
  Duration := TNSNumber.Wrap(TNSNotification.Wrap(Notification).userInfo.valueForKey(UIKeyboardAnimationDurationUserInfoKey)).doubleValue;
  KeyboardService.SetKeyboardAnimationDuration(Duration);
  KeyboardService.SetKeyboardFrame(GetKeyboardFrame(Notification));
  KeyboardService.CreateToolbar;
  KeyboardService.ToolbarVisible := True;
  if KeyboardService.IsToolbarEnabled then
    VKRect.Top := VKRect.Top - ToolbarHeight;
  SendNotificationAboutKeyboardEvent(VKRect, TKeyboardState.Shown);
end;

procedure TKeyboardEventHandler.SendNotificationAboutKeyboardEvent(const AVKRect: TRect;
  const AKeyboardState: TKeyboardState);
var
  Message: TVKStateChangeMessage;
begin
  Message := TVKStateChangeMessage.Create(AKeyboardState = TKeyboardState.Shown, AVKRect);
  TMessageManager.DefaultManager.SendMessage(Self, Message, True);
end;

{ TStoredActiveForm }

procedure TStoredActiveForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FForm) then
    FForm := nil;
end;

procedure TStoredActiveForm.SetForm(const Value: TCommonCustomForm);
begin
  if FForm <> Value then
  begin
    if FForm <> nil then
    begin
      TComponent(FForm).RemoveFreeNotification(self);
      FForm := nil;
    end;
    FForm := Value;
    if FForm <> nil then
      TComponent(FForm).FreeNotification(Self);
  end;
end;

{ TiOSVirtualKeyboardService }

function TiOSVirtualKeyboardService.AddButton(const Title: string; AOnClick: TNotifyEvent): TVirtualKeyboardToolButton;
begin
  FUpdatingButtons := True;
  Result := TiOSVKToolbarButton.Create;
  Result.Title := Title;
  Result.OnExecute := AOnClick;
  FUpdatingButtons := False;
  FButtons.Add(Result);
  RefreshToolbarButtons;
end;

function TiOSVirtualKeyboardService.ButtonsCount: Integer;
begin
  Result := FButtons.Count;
end;

procedure TiOSVirtualKeyboardService.ChangeToolbarOrientation;
begin
  //Need to change orientation without animation
  if FToolBar <> nil then
    SetToolbarFrame(GetToolbarFrame, False);
end;

procedure TiOSVirtualKeyboardService.ClearButtons;
begin
  if FButtons.Count > 0 then
  begin
    FUpdatingButtons := True;
    FButtons.Clear;
    FUpdatingButtons := False;
    RefreshToolbarButtons;
  end;
end;

constructor TiOSVirtualKeyboardService.Create;
begin
  inherited Create;
  FStoredActiveForm := TStoredActiveForm.Create(nil);
  FKeyboardHandler := TKeyboardEventHandler.Create;

  // In this event we detect type of interface idiom (iPad or iPhone), because while application is not initialized,
  // we cannot get correct value for it.
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);

  FUpdatingButtons := False;
  FToolbarVisible := False;
  FButtons := TList<TVirtualKeyboardToolButton>.Create;
  FToolBarButtons := TNSMutableArray.Create;
  FAnimationDuration := ToolbarAnimationDuration;
  FToolbarEnabled := True;
  FHideButtonVisible := True;
end;

procedure TiOSVirtualKeyboardService.CreateToolbar;
var
  KeyWindow: UIWindow;
begin
  KeyWindow := SharedApplication.keyWindow;
  if (KeyWindow <> nil) and (KeyWindow.rootViewController <> nil) then
  begin
    if (FToolBar = nil) and FToolbarEnabled then
    begin
      FToolBar := TUIToolbar.Create;
      if TOSVersion.Check(7) then
        FToolBar.setBarStyle(UIBarStyleDefault)
      else
        FToolBar.setBarStyle(UIBarStyleBlackOpaque);
      FToolBar.setAlpha(0.8);
      SetToolbarFrame(GetToolbarFrame, False);
      RefreshToolbarButtons;
      KeyWindow.rootViewController.view.addSubview(FToolbar);
    end
    else
      KeyWindow.rootViewController.view.bringSubviewToFront(FToolbar);
  end;
end;

procedure TiOSVirtualKeyboardService.DeleteButton(const Index: Integer);
begin
  if (Index >= 0) and (Index < FButtons.Count) then
  begin
    FButtons.Delete(Index);
    RefreshToolbarButtons;
  end;
end;

destructor TiOSVirtualKeyboardService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
  FreeAndNil(FKeyboardHandler);
  FUpdatingButtons := True;
  FreeAndNil(FButtons);
  if FToolBar <> nil then
  begin
    if FToolBar.items <> nil then
      FToolBar.items.release;
    FToolBar.release;
    FToolBar := nil;
  end;
  if FFlexibleSepararator <> nil then
  begin
    FFlexibleSepararator.release;
    FFlexibleSepararator := nil;
  end;
  if FHideButton <> nil then
  begin
    FHideButton.release;
    FHideButton := nil;
  end;
  FreeAndNil(FStoredActiveForm);
  FToolBarButtons.release;
  inherited;
end;

function TiOSVirtualKeyboardService.ExtractForm(const AControl: TFmxObject): TCommonCustomForm;
var
  RootObj: TFmxObject;
begin
  if AControl = nil then
    Exit(nil);

  if AControl is TCommonCustomForm then
    Exit(TCommonCustomForm(AControl));

  RootObj := AControl.Root.GetObject;
  if RootObj is TCommonCustomForm then
    Result := TCommonCustomForm(RootObj)
  else
    Result := nil;
end;

procedure TiOSVirtualKeyboardService.ApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if (M is TApplicationEventMessage) and (TApplicationEventMessage(M).Value.Event = TApplicationEvent.FinishedLaunching) then
  begin
    FToolbarEnabled := not IsPad;
    if FToolbarEnabled then
    begin
      CreateToolbar;
      ToolbarVisible := False;
    end;
    FHideButtonVisible := FToolbarEnabled;
  end;
end;

function TiOSVirtualKeyboardService.GetButtonByIndex(const Index: Integer): TVirtualKeyboardToolButton;
begin
  if (Index >= 0) and (Index < FButtons.Count) then
    Result := FButtons[Index]
  else
    Result := nil;
end;

function TiOSVirtualKeyboardService.GetFormView(const AControl: TFmxObject): UIView;
var
  Form: TCommonCustomForm;
begin
  Form := ExtractForm(AControl);

  if (Form = nil) or not Form.IsHandleAllocated then
    Result := nil
  else
    Result := WindowHandleToPlatform(Form.Handle).View;
end;

function TiOSVirtualKeyboardService.GetToolbarFrame: NSRect;
var
  ScreenRect: NSRect;
  InterfaceOrientation: TScreenOrientation;
  ScreenService: IFMXScreenService;
begin
  ScreenRect := MainScreen.bounds;
  Result.origin.x := 0;
  Result.size.height := ToolbarHeight;

  if TOSVersion.Check(8) then
    InterfaceOrientation := TScreenOrientation.Portrait
  else if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
    InterfaceOrientation := ScreenService.GetScreenOrientation
  else
    InterfaceOrientation := TScreenOrientation.Portrait;

  case InterfaceOrientation of
    TScreenOrientation.Portrait,
    TScreenOrientation.InvertedPortrait:
      begin
        if (FToolBar <> nil) and ToolbarVisible and IsToolbarEnabled then
          Result.origin.y := ScreenRect.size.height - FKeyboardFrame.size.height - FToolBar.bounds.size.height
        else
          Result.origin.y := ScreenRect.size.height;
        Result.size.width := ScreenRect.size.width;
      end;
    TScreenOrientation.Landscape,
    TScreenOrientation.InvertedLandscape:
      begin
        if (FToolBar <> nil) and ToolbarVisible and IsToolbarEnabled then
          Result.origin.y := ScreenRect.size.width - FKeyboardFrame.size.height - FToolBar.bounds.size.height
        else
          Result.origin.y := ScreenRect.size.width;
        Result.size.width := ScreenRect.size.height;
      end;
  end;
end;

function TiOSVirtualKeyboardService.GetView(const AFocusedControl: TFmxObject): UIView;
begin
  if IsNativeControl(AFocusedControl) then
    Result := UIView(TPresentedControl(AFocusedControl).PresentationProxy.NativeObject)
  else
    Result := GetFormView(AFocusedControl);
end;

function TiOSVirtualKeyboardService.GetVirtualKeyboardState: TVirtualKeyboardStates;
begin
  Result := [];
  if VKAutoShowMode in [TVKAutoShowMode.Always, TVKAutoShowMode.DefinedBySystem] then
    Include(Result, TVirtualKeyboardState.AutoShow);
  if FTransient then
    Include(Result, TVirtualKeyboardState.Transient);
  if FKeyboardHandler.Visible then
    Include(Result, TVirtualKeyboardState.Visible);
end;

procedure TiOSVirtualKeyboardService.SetTransientState(Value: Boolean);
begin
  FTransient := Value;
end;

function TiOSVirtualKeyboardService.IsHideKeyboardButtonVisible: Boolean;
begin
  Result := FHideButtonVisible;
end;

function TiOSVirtualKeyboardService.IsNativeControl(AControl: TFmxObject): Boolean;
var
  ControlTypeSupportable: IControlTypeSupportable;
begin
  Result := Supports(AControl, IControlTypeSupportable, ControlTypeSupportable) and
            (ControlTypeSupportable.ControlType = TControlType.Platform);
end;

function TiOSVirtualKeyboardService.IsToolbarEnabled: Boolean;
begin
  Result := FToolbarEnabled;
end;

procedure TiOSVirtualKeyboardService.RefreshToolbarButtons;
var
  I: Integer;
  B: UIBarButtonItem;
  AutoReleasePool: NSAutoReleasePool;
begin
  if not FUpdatingButtons and (FToolBar <> nil) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      if FToolBar.items <> nil then
      begin
        FToolBar.setItems(nil);
        FFlexibleSepararator := nil;
        FHideButton := nil;
      end;

      FToolBarButtons.removeAllObjects;
    finally
      AutoReleasePool.release;
    end;

    //Custom buttons
    for I := 0 to FButtons.Count - 1 do
    begin
      B := TUIBarButtonItem.Create;
      B.setTitle(StrToNSStr(FButtons[I].Title));
      B.setStyle(UIBarButtonItemStyleBordered);
      B.setTag(I + 1);
      B.setTarget(FKeyboardHandler.GetObjectID);
      B.setAction(sel_getUid('CustomButtonAction:'));
      FToolBarButtons.addObject(NSObjectToID(B));
    end;

    if FHideButtonVisible then
    begin
      //Separator
      if FFlexibleSepararator = nil then
      begin
        FFlexibleSepararator := TUIBarButtonItem.Create;
        FFlexibleSepararator.initWithBarButtonSystemItem(UIBarButtonSystemItemFlexibleSpace, nil, nil);
      end;
      FToolBarButtons.addObject(NSObjectToID(FFlexibleSepararator));

      //Hide button
      if FHideButton = nil then
      begin
        FHideButton := TUIBarButtonItem.Create;
        FHideButton.setTitle(StrToNSStr(SEditorDone));
        FHideButton.setStyle(UIBarButtonItemStyleDone);
        FHideButton.setTarget(FKeyboardHandler.GetObjectID);
        FHideButton.setAction(sel_getUid('HideVirtualKeyboard'));
      end;
      FToolBarButtons.addObject(NSObjectToID(FHideButton));
    end;

    FToolBar.setItems(FToolBarButtons);
  end;
end;

procedure TiOSVirtualKeyboardService.RefreshToolbarPosition;
begin
  SetToolbarFrame(GetToolbarFrame, False);
end;

procedure TiOSVirtualKeyboardService.SetHideKeyboardButtonVisibility(const Value: Boolean);
begin
  if FHideButtonVisible <> Value then
  begin
    FHideButtonVisible := Value;
    RefreshToolbarButtons;
  end;
end;

procedure TiOSVirtualKeyboardService.SetKeyboardAnimationDuration(const AInterval: NSTimeInterval);
begin
  FAnimationDuration := AInterval;
end;

procedure TiOSVirtualKeyboardService.SetKeyboardFrame(const Rect: NSRect);
begin
  FKeyboardFrame := Rect;
end;

procedure TiOSVirtualKeyboardService.SetToolbarEnabled(const Value: Boolean);
begin
  if FToolbarEnabled <> Value then
  begin
    if not Value then
      ToolbarVisible := False;
    FToolbarEnabled := Value;
  end;
end;

procedure TiOSVirtualKeyboardService.SetToolbarFrame(const Frame: NSRect; const UseAnimation: Boolean);
begin
  if FToolBar = nil then
    Exit;

  if UseAnimation then
  begin
    TUIView.OCClass.beginAnimations(nil, nil);
    try
      TUIView.OCClass.setAnimationDuration(FAnimationDuration);
      TUIView.OCClass.setAnimationBeginsFromCurrentState(True);
      FToolBar.setFrame(Frame);
    finally
      TUIView.OCClass.commitAnimations;
    end;
  end
  else
    FToolBar.setFrame(Frame);
end;

procedure TiOSVirtualKeyboardService.SetToolbarVisible(const Value: Boolean);
begin
  if FToolbarVisible <> Value then
  begin
    FToolbarVisible := Value;
    if FToolBar <> nil then
    begin
      if FToolbarEnabled then
        SetToolbarFrame(GetToolbarFrame, True);
      FToolBar.setHidden(not (FToolbarEnabled and Value));
    end;
  end;
end;

function TiOSVirtualKeyboardService.ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
var
  View: UIView;
begin
  if (AControl = nil) or (AControl.Root = nil) then
    Exit(False);

  FStoredActiveForm.Form := ExtractForm(AControl);

  View := GetView(AControl);
  if View = nil then
    Result := False
  else
  begin
    Result := View.becomeFirstResponder;
    // If user switches focus between two text-input controls, we use "transient" switching mode.
    // In this mode we don't hide virtual keyboard and just change focus between native UIView.
    // However, it may not change keyboard type, so we should reload input view.
    View.reloadInputViews;
  end;
end;

function TiOSVirtualKeyboardService.HideVirtualKeyboard: Boolean;
begin
  if FTransient then
    Exit(False);

  if SharedApplication.keyWindow = nil then
    Result := False
  else
  begin
    FKeyboardHandler.FKeepFocus := True;
    try
      Result := SharedApplication.keyWindow.endEditing(True);
    finally
      FKeyboardHandler.FKeepFocus := False;
    end;
  end;
end;

{ TiOSVKToolbarButton }

procedure TiOSVKToolbarButton.DoChanged;
begin
  inherited;
  KeyboardService.RefreshToolbarButtons;
end;

initialization
  RegisterVirtualKeyboardServices;
finalization
  UnregisterVirtualKeyboardServices;
end.
