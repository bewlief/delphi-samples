{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.VirtualKeyboard.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, Winapi.Windows, FMX.Types, FMX.Pickers, FMX.Platform, FMX.VirtualKeyboard;

type
  TWinVirtualKeyboard = class(TInterfacedObject, IFMXVirtualKeyboardService)
  private type
    TvkbState = (None, Hidden, Shown);
  private
    FPath: string;
    FExeName: string;
    FWndClassName: string;
    FHTmerLang: TFmxHandle;
    FHTmerVisible: TFmxHandle;
    FKBPresent: Boolean;
    FFormHandle: HWND;
    FInst: HINST;
    FError: Boolean;
    FLastvkbState: TvkbState;
    FLastHandle: HWND;
    FLastTime: TDateTime;
    FNewvkbState: TvkbState;
    FWait: Boolean;
    FStepActivate: Integer;
    FCodeKeyboard: HKL;
    FTimerService: IFMXTimerService;
    procedure KillTimerLang;
    procedure TimerLangProc;
    procedure StartTimerLang;
    procedure KillTimerVisible;
    procedure TimerVisibleProc;
    procedure StartTimerVisible;
    function FindKeyValue(const Key: HKey; const Name, Value, SubKeyName, SubValueName: string): string;
    function GetVirtualKeyboardState: TVirtualKeyboardStates;
    procedure vkbExecute(FormHandle: HWND);
    function vkbHandle: HWND;
    function vkbState: TvkbState;
    function GetVKBounds: TRect;
  protected
    procedure Clear;
    function IsAutoShow: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
    function HideVirtualKeyboard: Boolean;
    procedure SetTransientState(Value: Boolean);
    property VirtualKeyboardState: TVirtualKeyboardStates read GetVirtualKeyboardState;
    property ExeName: string read FExeName write FExeName;
    property Path: string read FPath write FPath;
    property WndClassName: string read FWndClassName write FWndClassName;
  end;

implementation

uses
  System.Messaging, System.SysUtils, System.IOUtils, Winapi.ShellAPI, Winapi.Messages, FMX.Forms, FMX.Helpers.Win,
  FMX.Platform.Win;

{ TWinVirtualKeyboard }

constructor TWinVirtualKeyboard.Create;
var
  L: Integer;
  S: string;
  HID: HKey;
begin
  S := string.Empty;
  inherited Create;
  SetLength(S, MAX_PATH);
  L := GetSystemDirectory(PChar(S), MAX_PATH);
  SetLength(S, L);
  FPath := S;
  FExeName := 'osk.exe';
  FWndClassName := 'OSKMainClass';
  FKBPresent := True;
  if not TOSVersion.Check(6, 2) and (Winapi.Windows.RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Enum', 0,
    KEY_READ, HID) = ERROR_SUCCESS) then
  try
    S := FindKeyValue(HID, 'ClassGUID', '{4D36E96B-E325-11CE-BFC1-08002BE10318}', 'Control',
      'ActiveService');
    FKBPresent := S <> '';
  finally
    RegCloseKey(HID);
  end;
  FNewvkbState := vkbState;
  StartTimerLang;
end;

procedure TWinVirtualKeyboard.Clear;
var
  H: HWND;
begin
  H := vkbHandle;
  if (H <> 0) and (FInst > 32) then
    PostMessage(H, WM_SYSCOMMAND, SC_CLOSE, 0);
  KillTimerVisible;
  KillTimerLang;
  FInst := 0;
  FError := False;
  FLastTime := 0;
  FLastHandle := 0;
end;

destructor TWinVirtualKeyboard.Destroy;
begin
  Clear;
  inherited;
end;

function TWinVirtualKeyboard.FindKeyValue(const Key: HKey; const Name, Value, SubKeyName, SubValueName: string): string;
var
  Buf, Val: string;
  R, I, J: Integer;
  SubKey: HKey;
  BufSize, T, ValSize: Cardinal;
begin
  Result := string.Empty;
  I := 0;
  Buf := string.Empty;
  Val := string.Empty;
  BufSize := 2048;
  SetLength(Buf, BufSize);
  ValSize := BufSize;
  SetLength(Val, ValSize);
  repeat
    BufSize := Length(Buf);
    ValSize := Length(Val);
    R := Winapi.Windows.RegEnumValue(Key, I, @Buf[1], BufSize, nil, @T, @Val[1], @ValSize);
    if (R = ERROR_SUCCESS) then
    begin
      if (string(PChar(Buf)) = Name) and (T = REG_SZ) and (SameText(string(PChar(Val)), Value)) then
      begin
        if Winapi.Windows.RegOpenKeyEx(Key, PChar(SubKeyName), 0, KEY_READ, SubKey) = ERROR_SUCCESS
        then
          try
            J := 0;
            repeat
              BufSize := Length(Buf);
              ValSize := Length(Val);
              R := Winapi.Windows.RegEnumValue(SubKey, J, @Buf[1], BufSize, nil, @T, @Val[1],
                @ValSize);
              if (R = ERROR_SUCCESS) and (string(PChar(Buf)) = SubValueName) and (T = REG_SZ) and
                (string(PChar(Val)) <> '') then
              begin
                Result := string(PChar(Val));
              end;
              Inc(J);
            until (Result <> '') or (R <> ERROR_SUCCESS);
          finally
            RegCloseKey(SubKey);
          end;
      end;
      Inc(I);
    end;
  until not Result.IsEmpty or (R <> ERROR_SUCCESS);

  if Result.IsEmpty then
  begin
    I := 0;
    repeat
      R := Winapi.Windows.RegEnumKey(Key, I, PChar(Buf), BufSize);
      if R = ERROR_SUCCESS then
      begin
        if Winapi.Windows.RegOpenKeyEx(Key, PChar(Buf), 0, KEY_READ, SubKey) = ERROR_SUCCESS then
          try
            Result := FindKeyValue(SubKey, Name, Value, SubKeyName, SubValueName);
          finally
            RegCloseKey(SubKey);
          end;
        Inc(I);
      end;
    until (Result <> '') or (R <> ERROR_SUCCESS);
  end;
end;

function TWinVirtualKeyboard.GetVirtualKeyboardState: TVirtualKeyboardStates;
var
  LState: TvkbState;
begin
  if FError then
    Result := [TVirtualKeyboardState.Error]
  else
    Result := [];
  if IsAutoShow then
    Result := Result + [TVirtualKeyboardState.AutoShow];
  if not FError then
  begin
    if Abs(Now - FLastTime) > 1 / SecsPerDay then
      LState := vkbState
    else
      LState := FLastvkbState;
    if LState = TvkbState.Shown then
      Result := Result + [TVirtualKeyboardState.Visible];
  end;
end;

function TWinVirtualKeyboard.GetVKBounds: TRect;
begin
  if FLastHandle <> 0 then
    GetWindowRect(FLastHandle, Result);
end;

function TWinVirtualKeyboard.HideVirtualKeyboard: Boolean;
begin
  Result := not FError;
  if (not FError) then
  begin
    if IsAutoShow then
      FNewvkbState := TvkbState.Hidden
    else
      FNewvkbState := TvkbState.None;
    if FNewvkbState <> vkbState then
    begin
      StartTimerVisible;
    end;
  end;
end;

procedure TWinVirtualKeyboard.SetTransientState(Value: Boolean);
begin
end;

function TWinVirtualKeyboard.ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
var
  Root: IRoot;
begin
  Result := not FError;
  if (not FError) then
  begin
    FNewvkbState := TvkbState.Shown;
    if FNewvkbState <> vkbState then
      StartTimerVisible;
    FWait := True;
    FFormHandle := 0;
    if (AControl <> nil) then
    begin
      Root := AControl.Root;
      if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
        FFormHandle := FormToHWND(TCommonCustomForm(Root.GetObject));
    end;
  end;
end;

function TWinVirtualKeyboard.IsAutoShow: Boolean;
begin
  Result := (VKAutoShowMode = TVKAutoShowMode.Always) or ((VKAutoShowMode = TVKAutoShowMode.DefinedBySystem) and
    (not FKBPresent));
end;

procedure TWinVirtualKeyboard.KillTimerLang;
begin
  if FHTmerLang <> 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FTimerService.DestroyTimer(FHTmerLang);
      FHTmerLang := 0;
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TWinVirtualKeyboard.StartTimerLang;
begin
  if FHTmerLang = 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FHTmerLang := FTimerService.CreateTimer(250, TimerLangProc);
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TWinVirtualKeyboard.TimerLangProc;
var
  LCodeKeyboard: HKL;
begin
  if FStepActivate > 0 then
  begin
    FLastHandle := vkbHandle;
    case FStepActivate of
      1:
        begin
          SetActiveWindow(FLastHandle);
          SetFocus(FLastHandle);
        end;
      4:
        begin
          SetActiveWindow(FFormHandle);
        end;
      5:
        begin
          SetFocus(FFormHandle);
          FCodeKeyboard := GetKeyboardLayout(0);
        end;
    end;
    if FStepActivate = 5 then
      FStepActivate := 0
    else
    begin
      Inc(FStepActivate);
      Exit;
    end;
  end
  else
  begin
    if vkbState = TvkbState.Shown then
    begin
      LCodeKeyboard := GetKeyboardLayout(0);
      if FCodeKeyboard <> LCodeKeyboard then
      begin
        SetActiveWindow(0);
        SetActiveWindow(FFormHandle);
        SetFocus(FFormHandle);
        FCodeKeyboard := LCodeKeyboard;
      end;
    end;
  end;
end;

procedure TWinVirtualKeyboard.KillTimerVisible;
begin
  if FHTmerVisible <> 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FTimerService.DestroyTimer(FHTmerVisible);
      FHTmerVisible := 0;
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TWinVirtualKeyboard.StartTimerVisible;
begin
  if FHTmerVisible = 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FHTmerVisible := FTimerService.CreateTimer(100, TimerVisibleProc);
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TWinVirtualKeyboard.TimerVisibleProc;
var
  LState: TvkbState;
  procedure Quit;
  begin
    if FLastHandle <> 0 then
      PostMessage(FLastHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
    Sleep(40);
    FLastHandle := 0;
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(False, TRect.Empty), True);
  end;
  procedure Restore;
  begin
    if FLastHandle <> 0 then
    begin
      if Winapi.Windows.GetActiveWindow <> FLastHandle then
      begin
        SendMessage(FLastHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
        TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(True, GetVKBounds), True);
      end;
    end;
  end;
  procedure Hide;
  begin
    if FLastHandle <> 0 then
      PostMessage(FLastHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
    FWait := True;
    FLastHandle := 0;
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(False, TRect.Empty), True);
  end;

begin
  if FWait then
  begin
    FLastHandle := vkbHandle;
    FWait := False;
    Exit;
  end;
  FWait := True;
  LState := vkbState;
  if LState <> FNewvkbState then
  begin
    case LState of
      TvkbState.None:
        case FNewvkbState of
          TvkbState.Hidden: { none }
            ;
          TvkbState.Shown:
            begin
              vkbExecute(FFormHandle);
              FWait := False;
              FStepActivate := 1;
              Exit;
            end;
        end;
      TvkbState.Hidden:
        case FNewvkbState of
          TvkbState.None:
            Quit;
          TvkbState.Shown:
            Restore;
        end;
      TvkbState.Shown:
        case FNewvkbState of
          TvkbState.None:
            Quit;
          TvkbState.Hidden:
            Hide;
        end;
    end;
    FNewvkbState := vkbState;
  end
  else if (FNewvkbState = TvkbState.Shown) and (FStepActivate = 1) then
    // Here we are sending a deferred message, otherwise there will be incorrect coordinates
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(True, GetVKBounds), True);
  KillTimerVisible;
end;

procedure TWinVirtualKeyboard.vkbExecute(FormHandle: HWND);

  function IsFileExisted(const AFileName: string): Boolean;
  begin
    if FileExists(AFileName) then
      Exit(True);

    TWow64Redirection.Current.Disable;
    try
      Result := FileExists(AFileName);
    finally
      TWow64Redirection.Current.Restore;
    end;
  end;

  function LaunchVirtualKeyboardApp(const AFileName: string): HINST;
  begin
    TWow64Redirection.Current.Disable;
    try
      Result := ShellExecute(FormHandle, 'open', PChar(AFileName), nil, PChar(ExtractFileDir(AFileName)), SW_SHOWNOACTIVATE);
    finally
      TWow64Redirection.Current.Restore;
    end;
  end;

  function WaitLaunchingVirtualKeyboardApp: Boolean;
  const
    StepPause = 40; //msec
    MaxAttemptsCount = 100;
  var
    AttemptsCount: Integer;
  begin
    AttemptsCount := 0;
    while (AttemptsCount < MaxAttemptsCount) and (vkbState = TvkbState.None) do
    begin
      Inc(AttemptsCount);
      Sleep(StepPause);
    end;
    Result := AttemptsCount < MaxAttemptsCount;
  end;

var
  VKAppFileName: string;
  H: HWND;
begin
  if FError then
    Exit;
  H := vkbHandle;
  if H = 0 then
  begin
    VKAppFileName := TPath.Combine(Path, ExeName);

    if IsFileExisted(VKAppFileName) then
      FInst := LaunchVirtualKeyboardApp(VKAppFileName)
    else
      FInst := 0;

    if FInst <= 32 then
      FError := True
    else if not WaitLaunchingVirtualKeyboardApp then
    begin
      FInst := 0;
      FError := True;
    end;
  end;
end;

function TWinVirtualKeyboard.vkbHandle: HWND;
begin
  Result := Winapi.Windows.FindWindow(PChar(FWndClassName), nil);
end;

function TWinVirtualKeyboard.vkbState: TvkbState;
var
  H: HWND;
begin
  H := vkbHandle;
  if (H <> INVALID_HANDLE_VALUE) and (H <> 0) then
  begin
    if (not IsWindowVisible(H)) or (IsIconic(H)) then
      Result := TvkbState.Hidden
    else
      Result := TvkbState.Shown;
    FLastHandle := H;
  end
  else
  begin
    Result := TvkbState.None;
    FLastHandle := 0;
  end;
  FLastvkbState := Result;
  FLastTime := Now;
end;

end.
