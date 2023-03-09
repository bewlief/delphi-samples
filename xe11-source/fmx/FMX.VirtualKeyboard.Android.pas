{*******************************************************}
{                                                       }
{             Delphi FireMonkey Platform                }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.VirtualKeyboard.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.Messaging, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero,
  Androidapi.JNIBridge, FMX.Types, FMX.VirtualKeyboard;

type
  TAndroidVirtualKeyboard = class;

  TKeyboardStateChangedListener = class(TJavaLocal, JOnKeyboardStateChangedListener)
  private
    [Weak] FKeyboardService: TAndroidVirtualKeyboard;
    FNeedNotifyAboutFrameChanges: Boolean;
    FPreviousVKRect: TRect;
  public
    constructor Create(const AService: TAndroidVirtualKeyboard);
    { JOnKeyboardStateChangedListener }
    procedure onVirtualKeyboardWillShown; cdecl;
    procedure onVirtualKeyboardFrameChanged(newFrame: JRect); cdecl;
    procedure onVirtualKeyboardWillHidden; cdecl;
  end;

  TAndroidVirtualKeyboard = class(TInterfacedObject, IFMXVirtualKeyboardService)
  private
    FKeyboardStateListener: TKeyboardStateChangedListener;
    FTransient: Boolean;
    procedure RegisterService;
    procedure UnregisterService;
  protected
    function IsAutoShow: Boolean;
    function DefineNativeView(const AObject: TFmxObject): JView;
    procedure SendNotificationAboutKeyboardEvent(const AVKRect: TRect);
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXVirtualKeyboardService }
    function ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
    function HideVirtualKeyboard: Boolean;
    function GetVirtualKeyboardState: TVirtualKeyboardStates;
    procedure SetTransientState(Value: Boolean);

    property VirtualKeyboardState: TVirtualKeyboardStates read GetVirtualKeyboardState;
  end;
  TVirtualKeyboardAndroid = TAndroidVirtualKeyboard;

implementation

uses
  System.SysUtils, FMX.Forms, FMX.Controls, FMX.Controls.Presentation, FMX.Platform, FMX.Platform.Android,
  FMX.Platform.UI.Android, FMX.Presentation.Android.Style, FMX.Text;

{ TAndroidVirtualKeyboardService }

function TAndroidVirtualKeyboard.IsAutoShow: Boolean;
begin
  Result := VKAutoShowMode in [TVKAutoShowMode.Always, TVKAutoShowMode.DefinedBySystem];
end;

procedure TAndroidVirtualKeyboard.RegisterService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService) then
    TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, Self);
end;

procedure TAndroidVirtualKeyboard.UnregisterService;
begin
  if TPlatformServices.Current <> nil then
    TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardService);
end;

constructor TAndroidVirtualKeyboard.Create;
begin
  inherited;
  RegisterService;
  _AddRef;
  FKeyboardStateListener := TKeyboardStateChangedListener.Create(Self);
  MainActivity.getVirtualKeyboard.addOnKeyboardStateChangedListener(FKeyboardStateListener);
end;

function TAndroidVirtualKeyboard.DefineNativeView(const AObject: TFmxObject): JView;

  function IsNativeControl: Boolean;
  begin
    Result := (AObject is TPresentedControl) and (TPresentedControl(AObject).ControlType = TControlType.Platform);
  end;

  function GetNativeView: JView;
  begin
    Result := JView(TPresentedControl(AObject).PresentationProxy.NativeObject);
  end;

  function GetFormView: JView;
  begin
    Result := WindowHandleToPlatform(TCommonCustomForm(TControl(AObject).Root.GetObject).Handle).View;
  end;

  function IsNativeStyledControl: Boolean;
  begin
    Result := (AObject is TPresentedControl) and (TPresentedControl(AObject).Presentation is TAndroidStyledPresentation);
  end;

  function IsFormAvailable: Boolean;
  begin
    Result := (AObject is TControl) and (TControl(AObject).Root <> nil) and
              TCommonCustomForm(TControl(AObject).Root.GetObject).IsHandleAllocated;
  end;

begin
  // For Styled-native controls we redirect text-input system to form. Form will deliver key event to focused fmx control.
  if IsNativeStyledControl and IsFormAvailable then
    Result := GetFormView
  else if IsNativeControl then
    Result := GetNativeView
  else if Supports(AObject, ITextInput) then
    Result := MainActivity.getEditText
  else if IsFormAvailable then
    Result := GetFormView
  else
    Result := nil;
end;

destructor TAndroidVirtualKeyboard.Destroy;
begin
  MainActivity.getVirtualKeyboard.removeOnKeyboardStateChangedListener(FKeyboardStateListener);
  FreeAndNil(FKeyboardStateListener);
  UnregisterService;
  inherited;
end;

function TAndroidVirtualKeyboard.GetVirtualKeyboardState: TVirtualKeyboardStates;
begin
  Result := [];
  if IsAutoShow then
    Include(Result, TVirtualKeyboardState.AutoShow);
  if FTransient then
    Include(Result, TVirtualKeyboardState.Transient);
  if MainActivity.getVirtualKeyboard.isVirtualKeyboardShown then
    Include(Result, TVirtualKeyboardState.Visible);
end;

function TAndroidVirtualKeyboard.HideVirtualKeyboard: Boolean;
begin
  Result := False;
  try
    if not FTransient then
      Result := MainActivity.getVirtualKeyboard.hide;
  except
    Application.HandleException(Screen.ActiveForm);
  end;
end;

function TAndroidVirtualKeyboard.ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;

  function IsNotFocused(const AControl: TFmxObject): Boolean;
  begin
    Result := (AControl is TControl) and not TControl(AControl).IsFocused;
  end;

var
  View: JView;
begin
  View := DefineNativeView(AControl);
  // The Android native text input system requires setting focus for working with Soft Keyboard.
  if IsNotFocused(AControl) then
    TControl(AControl).SetFocus;
  Result := MainActivity.getVirtualKeyboard.showFor(View);
end;

procedure TAndroidVirtualKeyboard.SendNotificationAboutKeyboardEvent(const AVKRect: TRect);
var
  Message: TVKStateChangeMessage;
begin
  Message := TVKStateChangeMessage.Create(TVirtualKeyboardState.Visible in VirtualKeyboardState, AVKRect);
  TMessageManager.DefaultManager.SendMessage(Self, Message, True);
end;

procedure TAndroidVirtualKeyboard.SetTransientState(Value: Boolean);
begin
  FTransient := Value;
end;

{ TKeyboardStateChangedListener }

constructor TKeyboardStateChangedListener.Create(const AService: TAndroidVirtualKeyboard);
begin
  inherited Create;
  FKeyboardService := AService;
  FNeedNotifyAboutFrameChanges := False;
  FPreviousVKRect := TRect.Empty;
end;

procedure TKeyboardStateChangedListener.onVirtualKeyboardWillShown;
begin
  FNeedNotifyAboutFrameChanges := FNeedNotifyAboutFrameChanges or
                                  not (TVirtualKeyboardState.Visible in FKeyboardService.VirtualKeyboardState);
end;

procedure TKeyboardStateChangedListener.onVirtualKeyboardFrameChanged(newFrame: JRect);
var
  VKRect: TRect;
begin
  VKRect.TopLeft := ConvertPixelToPoint(TPointF.Create(newFrame.Left, newFrame.Top)).Round;
  VKRect.BottomRight := ConvertPixelToPoint(TPointF.Create(newFrame.Right, newFrame.Bottom)).Round;

  if (FNeedNotifyAboutFrameChanges or ((MainActivity.getVirtualKeyboard.isVirtualKeyboardShown) and (VKRect.Height > 0)))
     and (FPreviousVKRect <> VKRect) then
    try
      FKeyboardService.SendNotificationAboutKeyboardEvent(VKRect);
      FPreviousVKRect := VKRect;
    finally
      FNeedNotifyAboutFrameChanges := False;
    end;
end;

procedure TKeyboardStateChangedListener.onVirtualKeyboardWillHidden;
begin
  FNeedNotifyAboutFrameChanges := FNeedNotifyAboutFrameChanges or
                                  (TVirtualKeyboardState.Visible in FKeyboardService.VirtualKeyboardState);
end;

end.

