{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Menu.Win;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Generics.Collections, Winapi.Windows, Winapi.Messages, FMX.Menus, FMX.Types, FMX.Forms;

type
  TWinMenuLooper = class
  private
    FView: IMenuView;
    function IsItemSelectable(const Item: TFmxObject): Boolean;
    procedure SelectFirstMenuItem(const AView: IMenuView);
    procedure SelectLastMenuItem(const AView: IMenuView);
    procedure SelectNextMenuItem(const AView: IMenuView; const ABackward: Boolean);
    function BackwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
    function ForwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
  public
    procedure StartLoop(const AView: IMenuView);
    procedure EndLoop(const AView: IMenuView);
  end;

  TWinMenuService = class(TInterfacedObject, IFMXMenuService)
  private type
    TState = (CreatingOSMenu, DestroyingMenuItem);
    TStates = set of TState;
    TMenuId = Integer;
    TWinMenuInfo = record
      MenuID: Integer;
      FMXMenuItem: TMenuItem;
      constructor Create(const AMenuId: Integer; const AnItem: TMenuItem);
    end;
  private
    FHMenuMap: TDictionary<TFmxHandle, TWinMenuInfo>;
    FHMenuIdMap: TDictionary<TMenuId, TFmxHandle>;
    FStates: TStates;
    FMenuLooper: TWinMenuLooper;
    { Menu Item Id}
    function GenerateMenuId: TMenuId;
    function AssignNewIdToMenu(const AParentMenu, AMenu: HMENU): TMenuId;
    function FindMenuInfoById(const AMenuItemId: TMenuId; var AMenuInfo: TWinMenuInfo): Boolean;
    { Removing }
    procedure DestroysAllMenuItems(const AMenu: IItemsContainer);
    procedure RemoveMenuFromMaps(const AMenuHandle: TFmxHandle);
    { Menu Item Bitmap }
    procedure AddBitmapToMenu(const AParentMenu: HMENU; const AMenuItemId: TMenuId; const ABitmap: HBITMAP);
    procedure RemoveBitmapFromMenu(const AParentMenu, AMenu: HMENU);
  protected
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMInitMenuPopup(var Message: TWMInitMenuPopup); message WM_INITMENUPOPUP;
    procedure WMMenuSelect(var Message: TWMMenuSelect); message WM_MENUSELECT;
  public
    constructor Create;
    destructor Destroy; override;

    { IFMXMenuService }

    procedure StartMenuLoop(const AView: IMenuView);
    function ShortCutToText(ShortCut: TShortCut): string;
    procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
    function TextToShortCut(Text: string): Integer;
    procedure CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);
    procedure UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);
    procedure DestroyMenuItem(const AItem: IItemsContainer);
    function IsMenuBarOnWindowBorder: Boolean;
    procedure UpdateMenuBar;
  end;

implementation

uses
  System.Types, System.SysUtils, System.RTLConsts, System.UITypes, FMX.Platform, FMX.Platform.Win, FMX.Helpers.Win,
  FMX.Forms.Border.Win, FMX.Consts;

type
  TOpenMenuItem = class(TMenuItem);
  EUnavailableMenuId = class(Exception);

{ TWinMenuLooper }

function TWinMenuLooper.BackwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AView = nil then
    Exit;

  for I := AStartInd downto AEndInd do
    if IsItemSelectable(AView.GetItem(I)) then
    begin
      AView.Selected := TMenuItem(AView.GetItem(I));
      Result := True;
      Break;
    end;
end;

procedure TWinMenuLooper.EndLoop(const AView: IMenuView);
var
  View: IMenuView;
begin
  View := AView;
  while View <> nil do
  begin
    View.Loop := False;
    View.Selected := nil;
    View := View.ParentView;
  end;
end;

function TWinMenuLooper.ForwardSelectNextMenuItem(const AView: IMenuView; const AStartInd, AEndInd: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AView = nil then
    Exit;

  for I := AStartInd to AEndInd do
    if IsItemSelectable(AView.GetItem(I)) then
    begin
      AView.Selected := TMenuItem(AView.GetItem(I));
      Result := True;
      Break;
    end;
end;

function TWinMenuLooper.IsItemSelectable(const Item: TFmxObject): Boolean;
begin
  Result := (Item is TMenuItem) and TMenuItem(Item).Visible and (TMenuItem(Item).Text <> SMenuSeparator);
end;

procedure TWinMenuLooper.SelectFirstMenuItem(const AView: IMenuView);
var
  I: Integer;
begin
  if AView = nil then
    Exit;

  I := 0;
  while (I < AView.GetItemsCount) and not IsItemSelectable(AView.GetItem(I)) do
    Inc(I);
  if I < AView.GetItemsCount then
    AView.Selected := TMenuItem(AView.GetItem(I));
end;

procedure TWinMenuLooper.SelectLastMenuItem(const AView: IMenuView);
var
  I: Integer;
begin
  if AView = nil then
    Exit;

  I := AView.GetItemsCount - 1;
  while (I >= 0) and not IsItemSelectable(AView.GetItem(I)) do
    Dec(I);
  if I >= 0 then
    AView.Selected := TMenuItem(AView.GetItem(I));
end;

procedure TWinMenuLooper.SelectNextMenuItem(const AView: IMenuView; const ABackward: Boolean);
begin
  if ABackward then
  begin
    if (AView.Selected = nil) or not BackwardSelectNextMenuItem(AView, AView.Selected.Index - 1, 0) then
      SelectLastMenuItem(AView);
    // otherwise nothing
  end
  else
  begin
    if (AView.Selected = nil) or not ForwardSelectNextMenuItem(AView, AView.Selected.Index + 1,  AView.GetItemsCount - 1) then
      SelectFirstMenuItem(AView);
    // otherwise nothing
  end;
end;

procedure TWinMenuLooper.StartLoop(const AView: IMenuView);
var
  FirstLoop: Boolean;

  function ContinueLoop: Boolean;
  begin
    Result := AView.Loop;
  end;

  function ParentWindow(const AView: IMenuView): HWND;
  var
    Obj: TFmxObject;
    Form: TCommonCustomForm;
  begin
    Result := 0;
    if (AView <> nil) and (AView.Parent <> nil) then
    begin
      Obj := AView.Parent;
      while (Obj <> nil) and not (Obj is TCommonCustomForm) do
        Obj := Obj.Parent;
      if Obj is TCommonCustomForm then
      begin
        Form := TCommonCustomForm(Obj);
        while TCommonCustomForm.IsPopupForm(Form) do
          Form := Form.ParentForm;
        if (Form <> nil) and (Form.Handle <> nil) then
          Result := WindowHandleToPlatform(Form.Handle).Wnd;
      end;
    end;
  end;

  function MsgToTMessage(const AMsg: TMsg): TMessage;
  begin
    Result.Msg := AMsg.message;
    Result.WParam := AMsg.wParam;
    Result.LParam := AMsg.lParam;
    Result.Result := 0;
  end;

var
  Msg: TMsg;
  Message: TMessage;
  WP: TPoint;
  ParentWnd: HWND;
  ScreenPoint: TPointF;
  InMenus: Boolean;
  CurrentView, NewView: IMenuView;
  Obj: IControl;
  TimerID: THandle;
  MenuItem: TMenuItem;
begin
  FView := AView;
  AView.Loop := True;
  TimerID := SetTimer(0, 0, 50, nil);
  try
    FirstLoop := True;
    while ContinueLoop do
    begin
                                                         
      if FirstLoop then
        FirstLoop := False
      else
        WaitMessage;

      while ContinueLoop and PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) do
      begin
        Message := MsgToTMessage(Msg);
        case Msg.Message of
          WM_WINDOWPOSCHANGING:
            begin
              EndLoop(AView);
              Exit;
            end;
          WM_QUIT { , WM_NCLBUTTONDOWN..WM_NCMBUTTONDBLCLK } :
            begin
              EndLoop(AView);
              Continue;
            end;
          WM_TIMER:
            begin
              TranslateMessage(Msg);
            end;
        end;
        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
        begin
          Message := MsgToTMessage(Msg);
          case Msg.Message of
            WM_CLOSEMENU:
              EndLoop(AView);
            WM_NCMOUSEMOVE, WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCLBUTTONUP:
              begin
                case Msg.Message of
                  WM_NCMOUSEMOVE:
                    begin
                      { Handle MouseOver }
                      WP := TWMMouse(Message).Pos; // window px
                      Winapi.Windows.ClientToScreen(Msg.HWND, WP); // screen px
                      ScreenPoint := PxToDp(WP); // dp
                      Obj := AView.ObjectAtPoint(ScreenPoint);
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                      { Find top level menu }
                      CurrentView := AView;
                      while CurrentView.ParentView <> nil do
                        CurrentView := CurrentView.ParentView;
                      { Check all items }
                      while CurrentView <> nil do
                      begin
                        Obj := CurrentView.ObjectAtPoint(ScreenPoint);
                        if (Obj <> nil) and (Obj.GetObject is TMenuItem) and not TMenuItem(Obj.GetObject).IsSelected then
                        begin
                          if CurrentView <> AView then
                          begin
                            NewView := AView;
                            while NewView <> CurrentView do
                            begin
                              NewView.Loop := False;
                              NewView := NewView.ParentView;
                            end;
                            TMenuItem(Obj.GetObject).NeedPopup;
                            Exit;
                          end;
                        end;
                        CurrentView := CurrentView.ChildView;
                      end;
                      Continue;
                    end;
                  WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN:
                    begin
                      { Handle MouseOver if mouse over not menuitem }
                      WP := TWMMouse(Message).Pos; // window px
                      Winapi.Windows.ClientToScreen(Msg.HWND, WP); // screen px
                      ScreenPoint := PxToDp(WP); // dp
                      Obj := AView.ObjectAtPoint(ScreenPoint);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                      { Menus }
                      if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
                      begin
                        MenuItem := TMenuItem(Obj.GetObject);
                        if not MenuItem.IsSelected and MenuItem.HavePopup then
                          TMenuItem(MenuItem).NeedPopup
                        else if MenuItem.CanBeClicked then
                        begin
                          EndLoop(AView);
                          TOpenMenuItem(Obj.GetObject).Click;
                        end;
                      end
                      else
                      begin
                        CurrentView := AView;
                        InMenus := False;
                        while (CurrentView <> nil) and not InMenus do
                        begin
                          if not (CurrentView.IsMenuBar) and (CurrentView.ObjectAtPoint(ScreenPoint) <> nil) then
                            InMenus := True;
                          CurrentView := CurrentView.ParentView;
                        end;
                        if not InMenus then
                          EndLoop(AView);
                      end;
                    end;
                  WM_NCLBUTTONUP:
                    begin
                      { Handle MouseOver if mouse over not menuitem }
                      WP := TWMMouse(Message).Pos; // window px
                      Winapi.Windows.ClientToScreen(Msg.HWND, WP); // screen px
                      ScreenPoint := PxToDp(WP); // screen dp
                      Obj := AView.ObjectAtPoint(ScreenPoint);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                    end;
                end;
              end;
            WM_MOUSEFIRST..WM_MOUSELAST:
              begin
                { Handle MouseOver if mouse over not menuitem }
                WP := TWMMouse(Message).Pos; // window px
                Winapi.Windows.ClientToScreen(Msg.HWND, WP); // screen px
                case Msg.Message of
                  WM_MOUSEMOVE:
                    begin
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                      Continue;
                    end;
                  WM_LBUTTONDOWN, WM_RBUTTONDOWN:
                    begin
                      ScreenPoint := PxToDp(WP); // screen dp
                      Obj := AView.ObjectAtPoint(ScreenPoint);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                      { Menus }
                      if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
                      begin
                        MenuItem := TMenuItem(Obj.GetObject);
                        if not MenuItem.IsSelected and MenuItem.HavePopup then
                          TMenuItem(MenuItem).NeedPopup
                        else if MenuItem.CanBeClicked then
                        begin
                          EndLoop(AView);
                          TOpenMenuItem(MenuItem).Click;
                        end;
                      end
                      else
                      begin
                        CurrentView := AView;
                        InMenus := False;
                        while (CurrentView <> nil) and not InMenus do
                        begin
                          if not (CurrentView.IsMenuBar) and (CurrentView.ObjectAtPoint(ScreenPoint) <> nil) then
                            InMenus := True;
                          CurrentView := CurrentView.ParentView;
                        end;
                        if not InMenus then
                        begin
                          EndLoop(AView);
                          ParentWnd := ParentWindow(AView);
                          if ParentWnd <> 0 then
                          begin
                            // Redirect messages to the parent form
                            Winapi.Windows.ScreenToClient(ParentWnd, WP);
                            Msg.LPARAM := PointToLParam(WP);
                            PostMessage(ParentWnd, Msg.Message, Msg.wParam, Msg.LPARAM);
                          end;
                        end;
                      end;
                    end;
                  WM_LBUTTONUP, WM_RBUTTONUP:
                    begin
                      ScreenPoint := PxToDp(WP); // screen dp
                      Obj := AView.ObjectAtPoint(ScreenPoint);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                    end;
                end;
              end;
            WM_KEYFIRST .. WM_KEYLAST:
              if GetKeyState(VK_LBUTTON) >= 0 then
                case Msg.Message of
                  WM_KEYDOWN, WM_SYSKEYDOWN:
                    case Msg.wParam of
                      VK_RETURN:
                        begin
                          if AView.Selected <> nil then
                          begin
                            if AView.Selected.HavePopup then
                              AView.Selected.NeedPopup
                            else if AView.Selected.CanBeClicked then
                            begin
                              TOpenMenuItem(AView.Selected).Click;
                              EndLoop(AView);
                            end;
                          end
                          else
                            EndLoop(AView);
                        end;
                      VK_ESCAPE:
                        begin
                          AView.Selected := nil;
                          Exit;
                        end;
                      VK_MENU, VK_F10:
                        EndLoop(AView);
                      VK_LEFT, VK_RIGHT:
                          if AView.IsMenuBar then
                            SelectNextMenuItem(AView, Msg.wParam = VK_LEFT)
                          else if AView.ParentView <> nil then
                            if (AView.Selected <> nil) and AView.Selected.HavePopup and (Msg.wParam = VK_RIGHT) then
                              AView.Selected.NeedPopup
                            else if AView.ParentView.IsMenuBar then
                            begin
                              AView.Loop := False;
                              SelectNextMenuItem(AView.ParentView, Msg.wParam = VK_LEFT);
                              if AView.ParentView.Selected <> nil then
                                AView.ParentView.Selected.NeedPopup;
                              Exit;
                            end
                            else
                              AView.Loop := False;
                      VK_UP, VK_DOWN:
                          if AView.IsMenuBar then
                          begin
                            if (AView.Selected <> nil) and (Msg.wParam = VK_DOWN) then
                              AView.Selected.NeedPopup;
                          end
                          else
                            SelectNextMenuItem(AView, Msg.wParam = VK_UP);
                    end;
                end;
          else
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        end;
      end;
    end;
  finally
    KillTimer(0, TimerID);
    AView.Loop := False;
    Winapi.Windows.ReleaseCapture;
    FView := nil;
  end;
end;

{ TWinMenuService }

procedure TWinMenuService.AddBitmapToMenu(const AParentMenu: HMENU; const AMenuItemId: TMenuId; const ABitmap: HBITMAP);
var
  MenuItemInfo: tagMENUITEMINFOW;
begin
  if ABitmap <> 0 then
  begin
    FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
    MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
    MenuItemInfo.fMask := MIIM_BITMAP;
    if not GetMenuItemInfo(AParentMenu, AMenuItemId, False, MenuItemInfo) then
      RaiseLastOSError;
    MenuItemInfo.hbmpItem := ABitmap;
    try
      if not SetMenuItemInfo(AParentMenu, AMenuItemId, False, MenuItemInfo) then
        RaiseLastOSError;
    except
      MenuItemInfo.hbmpItem := 0;
      SetMenuItemInfo(AParentMenu, AMenuItemId, False, MenuItemInfo);
      raise;
    end;
  end;
end;

function TWinMenuService.AssignNewIdToMenu(const AParentMenu, AMenu: HMENU): TMenuId;
var
  MenuItemInfo: tagMENUITEMINFOW;
begin
  Result := GenerateMenuId;

  FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
  MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
  MenuItemInfo.fMask := MIIM_ID;
  if not GetMenuItemInfo(AParentMenu, AMenu, False, MenuItemInfo) then
    RaiseLastOSError;
  MenuItemInfo.wID := Result;
  try
    if not SetMenuItemInfo(AParentMenu, AMenu, False, MenuItemInfo) then
      RaiseLastOSError;
    FHMenuIdMap.Add(Result, AMenu);
  except
    MenuItemInfo.wID := 0;
    SetMenuItemInfo(AParentMenu, AMenu, False, MenuItemInfo);
    raise;
  end;
end;

constructor TWinMenuService.Create;
begin
  FHMenuMap := TDictionary<TFmxHandle, TWinMenuInfo>.Create;
  FHMenuIdMap := TDictionary<TMenuId, TFmxHandle>.Create;
  FMenuLooper := TWinMenuLooper.Create;
end;

procedure TWinMenuService.CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);

  function CalculateItemFlags(const AMenuItem: TMenuItem): Integer;
  begin
    Result := 0;
    if AMenuItem.Text = SMenuSeparator then
      Result := Result or MF_SEPARATOR
    else
    begin
      if AMenuItem.IsChecked then
        Result := Result or MF_CHECKED;

      if not AMenuItem.Enabled then
        Result := Result or MF_DISABLED;

      Result := Result or MF_STRING;
    end;
  end;

  function CalculateVisibleCount(const AMenu: IItemsContainer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to AMenu.GetItemsCount - 1 do
      if (AMenu.GetItem(I) is TMenuItem) and TMenuItem(AMenu.GetItem(I)).Visible then
        Inc(Result);
  end;

  function AssignNewIdToLastMenuItem(const AParentMenu: HMENU): TMenuId;
  var
    MenuItemInfo: tagMENUITEMINFOW;
  begin
    Result := GenerateMenuId;
    FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
    MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
    MenuItemInfo.fMask := MIIM_ID;
    if not GetMenuItemInfo(AParentMenu, GetMenuItemCount(AParentMenu) - 1, True, MenuItemInfo) then
      RaiseLastOSError;
    MenuItemInfo.wID := Result;
    if not SetMenuItemInfo(AParentMenu, GetMenuItemCount(AParentMenu) - 1, True, MenuItemInfo) then
      RaiseLastOSError;
  end;

  procedure InsertItems(const AParent: HMENU; AChild: IItemsContainer; const AMenuLevel: Integer);
  var
    I, Flags, VisibleCount: Integer;
    PopupMenu: HMENU;
    Native: INativeControl;
    MenuItem: TMenuItem;
    SubChild: IItemsContainer;
    S: string;
    Bitmap: HBITMAP;
    Scale: Single;
    PopupMenuId: TMenuId;
  begin
    if (AChild <> nil) and (AChild.GetObject is TMenuItem) then
    begin
      Bitmap := 0;
      MenuItem := TMenuItem(AChild.GetObject);
      if MenuItem.Visible and Supports(MenuItem, INativeControl, Native) then
      begin
        Flags := CalculateItemFlags(MenuItem);
        VisibleCount := CalculateVisibleCount(AChild);
        if VisibleCount > 0 then
          Flags := Flags or MF_POPUP;

        Native.Handle := 0;
        PopupMenu := CreateMenu;
        if PopupMenu = 0 then
          RaiseLastOSError;
        try
          if AMenuLevel > 0 then
            S := ShortCutToText(MenuItem.ShortCut)
          else
            S := string.Empty;
          if not S.IsEmpty then
            S := #9 + S;
          S := MenuItem.Text + S;
          if AppendMenu(AParent, Flags, PopupMenu, PChar(S)) then
          begin
            PopupMenuId := AssignNewIdToLastMenuItem(AParent);
            FHMenuIdMap.Add(PopupMenuId, PopupMenu);
            FHMenuMap.Add(PopupMenu, TWinMenuInfo.Create(PopupMenuId, MenuItem));

            Scale := AForm.Handle.Scale;
            Bitmap := TWinMenuHelper.ImageListToMenuBitmap(AMenuLevel = 0, MenuItem.Images, MenuItem.ImageIndex, Scale);
            if Bitmap = 0 then
              Bitmap := TWinMenuHelper.BitmapToMenuBitmap(AMenuLevel = 0, MenuItem.Bitmap, Scale);
            if Bitmap <> 0 then
              try
                AddBitmapToMenu(AParent, PopupMenuId, Bitmap);
              except
                DeleteObject(Bitmap);
                Bitmap := 0;
                raise;
              end;
            if VisibleCount > 0 then
              for I := 0 to AChild.GetItemsCount - 1 do
                if Supports(AChild.GetItem(I), IItemsContainer, SubChild) then
                  InsertItems(PopupMenu, SubChild, AMenuLevel + 1);
            Native.Handle := PopupMenu;
          end
          else
            RaiseLastOSError;
        except
          if Bitmap <> 0 then
            RemoveBitmapFromMenu(AParent, PopupMenu);
          DestroyMenu(PopupMenu);
          raise;
        end;
      end;
    end;
  end;

var
  Handle: HMENU;
  Wnd: HWND;
  I, VisibleCount: Integer;
  WindowBorder: TWindowBorderWin;
  Native: INativeControl;
  SavedWindowState: TWindowState;
begin
  if TState.CreatingOSMenu in FStates then
    Exit;
  SavedWindowState := AForm.WindowState;
  try
    Include(FStates, TState.CreatingOSMenu);
    Wnd := FormToHWND(AForm);
    if Wnd = 0 then
      Exit;

    VisibleCount := 0;
    Native := nil;
    if AMenu <> nil then
    begin
      DestroysAllMenuItems(AMenu);
      Supports(AMenu.GetObject, INativeControl, Native);
      if not (csDesigning in AForm.ComponentState) and AForm.Border.IsSupported then
      begin
        WindowBorder := TWindowBorderWin(AForm.Border.WindowBorder);
        WindowBorder.CreateOSMenu(AMenu);
      end
      else if (Native <> nil) and Native.HandleSupported then
        VisibleCount := CalculateVisibleCount(AMenu);
    end;
    Handle := GetMenu(Wnd);
    if VisibleCount > 0 then
    begin
      if Handle = 0 then
      begin
        Native.Handle := 0;
        Handle := CreateMenu;
        if Handle = 0 then
          RaiseLastOSError;
      end;
      try
        for I := 0 to AMenu.GetItemsCount - 1 do
          if AMenu.GetItem(I) is TMenuItem and TMenuItem(AMenu.GetItem(I)).Visible then
            InsertItems(Handle, TMenuItem(AMenu.GetItem(I)), 0);
      except
        DestroyMenu(Handle);
        raise;
      end;
      SetMenu(Wnd, Handle);
    end
    else if Handle <> 0 then
    begin
      if not DestroyMenu(Handle) then
        RaiseLastOSError;
      SetMenu(Wnd, 0);
      Handle := 0;
    end;
    if Native <> nil then
      Native.Handle := Handle;
  finally
    AForm.WindowState := SavedWindowState;
    Exclude(FStates, TState.CreatingOSMenu);
  end;
end;

destructor TWinMenuService.Destroy;
begin
  FreeAndNil(FMenuLooper);
  FreeAndNil(FHMenuMap);
  FreeAndNil(FHMenuIdMap);

  inherited;
end;

procedure TWinMenuService.DestroyMenuItem(const AItem: IItemsContainer);

  function FindParentMenuHandle(const AMenuItem: TFmxObject): TFmxHandle;
  var
    ParentMenuItem: TFmxObject;
    Native: INativeControl;
  begin
    if AMenuItem is TMenuItem then
      ParentMenuItem := TMenuItem(AMenuItem).ParentMenuItem
    else
      ParentMenuItem := AMenuItem.Parent;
    if Supports(ParentMenuItem, INativeControl, Native) then
      Result := Native.Handle
    else
      Result := 0;
  end;

  procedure UnlinkSubmenuItems(const AParentHandle: HMENU);
  var
    I: Integer;
  begin
    for I := GetMenuItemCount(AParentHandle) - 1 downto 0 do
      CheckWinapiResult(RemoveMenu(AParentHandle, I, MF_BYPOSITION));
  end;

var
  Form: TCommonCustomForm;
  MenuItem: TFmxObject;
  Wnd: HWND;
  ParentHandle, Handle: TFmxHandle;
  Native: INativeControl;
  Root: IRoot;
begin
  if not (TState.DestroyingMenuItem in FStates) and (AItem <> nil) and (AItem.GetObject is TFmxObject) then
    MenuItem := AItem.GetObject
  else
    Exit;
  Include(FStates, TState.DestroyingMenuItem);
  try
    Root := MenuItem.Root;
    if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
    begin
      Form := TCommonCustomForm(Root.GetObject);
      if (MenuItem <> nil) and Supports(MenuItem, INativeControl, Native) and (Native.Handle <> 0) then
      begin
        Handle := Native.Handle;
        if not (csDesigning in Form.ComponentState) and (Form.Border.WindowBorder is TWindowBorderWin) and
          TWindowBorderWin(Form.Border.WindowBorder).HandleExists(Handle) then
        begin
          TWindowBorderWin(Form.Border.WindowBorder).RemoveHandle(Handle);
        end
        else
        begin
          if Handle <> INVALID_HANDLE_VALUE then
          begin
            Wnd := FormToHWND(Form);
            ParentHandle := FindParentMenuHandle(MenuItem);
            if Handle = GetMenu(Wnd) then
            begin
              UnlinkSubmenuItems(Handle);
              CreateOSMenu(Form, nil);
              SetMenu(Wnd, 0);
            end
            else if ParentHandle <> 0 then
              RemoveBitmapFromMenu(ParentHandle, Handle);

            if IsMenu(Handle) then
            begin
              UnlinkSubmenuItems(Handle);
              CheckWinapiResult(DestroyMenu(Handle));
            end;
          end;
          RemoveMenuFromMaps(Handle);
        end;
        Native.Handle := 0
      end;
    end;
  finally
    Exclude(FStates, TState.DestroyingMenuItem);
  end;
end;

function TWinMenuService.FindMenuInfoById(const AMenuItemId: TMenuId; var AMenuInfo: TWinMenuInfo): Boolean;
var
  FmxHandle: TFmxHandle;
begin
  Result := FHMenuIdMap.TryGetValue(AMenuItemId, FmxHandle) and FHMenuMap.TryGetValue(FmxHandle, AMenuInfo);
end;

function TWinMenuService.GenerateMenuId: TMenuId;
begin
  Result := NewUniqueMenuCommand;
  if Result > 65535 then
  begin
    FreeUniqueMenuCommand(Result);
    raise EUnavailableMenuId.Create(SUnavailableMenuId);
  end;
end;

function TWinMenuService.IsMenuBarOnWindowBorder: Boolean;
begin
  Result := True;
end;

procedure TWinMenuService.RemoveBitmapFromMenu(const AParentMenu, AMenu: HMENU);
var
  MenuItemInfo: tagMENUITEMINFOW;
  MenuInfo: TWinMenuInfo;
  MenuId: TMenuId;
begin
  if FHMenuMap.TryGetValue(AMenu, MenuInfo) then
    MenuId := MenuInfo.MenuID
  else
    Exit;
  FillChar(MenuItemInfo, SizeOf(MenuItemInfo), 0);
  MenuItemInfo.cbSize := SizeOf(MenuItemInfo);
  MenuItemInfo.fMask := MIIM_BITMAP;
  if not GetMenuItemInfo(AParentMenu, MenuId, False, MenuItemInfo) then
    RaiseLastOSError;
  if (MenuItemInfo.hbmpItem <> HBITMAP(-1)) and (MenuItemInfo.hbmpItem > HBMMENU_POPUP_MINIMIZE) then
  begin
    if not DeleteObject(MenuItemInfo.hbmpItem) then
      RaiseLastOSError;

    MenuItemInfo.hbmpItem := HBITMAP(-1);
    if not SetMenuItemInfo(AParentMenu, MenuId, False, MenuItemInfo) then
      RaiseLastOSError;
  end;
end;

procedure TWinMenuService.DestroysAllMenuItems(const AMenu: IItemsContainer);
var
  I: Integer;
  ItemsContainer: IItemsContainer;
begin
  for I := 0 to AMenu.GetItemsCount - 1 do
    if Supports(AMenu.GetItem(I), IItemsContainer, ItemsContainer) then
      DestroysAllMenuItems(ItemsContainer);
  DestroyMenuItem(AMenu);
end;

procedure TWinMenuService.RemoveMenuFromMaps(const AMenuHandle: TFmxHandle);
var
  Pair: TPair<TMenuId, TFmxHandle>;
begin
  FHMenuMap.Remove(AMenuHandle);

  for Pair in FHMenuIdMap do
    if Pair.Value = AMenuHandle then
    begin
      FreeUniqueMenuCommand(Pair.Key);
      FHMenuIdMap.Remove(Pair.Key);
      Break;
    end;
end;

procedure TWinMenuService.ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  FMX.Helpers.Win.ShortCutToKey(ShortCut, Key, Shift);
end;

function TWinMenuService.ShortCutToText(ShortCut: TShortCut): string;
begin
  Result := FMX.Helpers.Win.ShortCutToText(ShortCut);
end;

procedure TWinMenuService.StartMenuLoop(const AView: IMenuView);
begin
  FMenuLooper.StartLoop(AView);
end;

function TWinMenuService.TextToShortCut(Text: string): Integer;
begin
  Result := FMX.Helpers.Win.TextToShortCut(Text);
end;

procedure TWinMenuService.UpdateMenuBar;
begin
end;

procedure TWinMenuService.UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);

  function GetParentMenuItemHandle(const AMenuItem: TMenuItem): TFmxHandle;
  var
    NativeControl: INativeControl;
  begin
    if Supports(AMenuItem.ParentMenuItem, INativeControl, NativeControl) or Supports(AMenuItem.MainMenu, INativeControl, NativeControl) then
      Result := NativeControl.Handle
    else
      Result := 0;
  end;

  function GetBitmap(const AMenuItem: TMenuItem): HBITMAP;
  var
    IsTopLevelItem: Boolean;
    Scale: Single;
  begin
    IsTopLevelItem := AMenuItem.Parent is TMainMenu;
    if (AMenuItem.Root <> nil) and (AMenuItem.Root.GetObject is TCommonCustomForm) then
      Scale := TCommonCustomForm(AMenuItem.Root.GetObject).Handle.Scale
    else
      Scale := 1;
    Result := TWinMenuHelper.ImageListToMenuBitmap(IsTopLevelItem, AMenuItem.Images, AMenuItem.ImageIndex, Scale);
    if Result = 0 then
      Result := TWinMenuHelper.BitmapToMenuBitmap(IsTopLevelItem, AMenuItem.Bitmap, Scale);
  end;

  procedure UpdateBitmap(const AMenuItem: TMenuItem);
  var
    ParentHandle: TFmxHandle;
    Bitmap: HBITMAP;
    MenuItemHandle: TFmxHandle;
  begin
    MenuItemHandle := (AMenuItem as INativeControl).Handle;
    ParentHandle := GetParentMenuItemHandle(AMenuItem);
    if ParentHandle = 0 then
      Exit;

    Bitmap := GetBitmap(AMenuItem);
    if Bitmap <> 0 then
      try
        RemoveBitmapFromMenu(ParentHandle, MenuItemHandle);
        AddBitmapToMenu(ParentHandle, FHMenuMap[MenuItemHandle].MenuID, Bitmap);
      except
        DeleteObject(Bitmap);
        raise;
      end
    else
      RemoveBitmapFromMenu(ParentHandle, MenuItemHandle);
  end;

  procedure UpdateEnabled(const AMenuItem: TMenuItem);
  const
    Enables: array[Boolean] of DWORD = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  var
    ParentHandle: TFmxHandle;
    MenuItemHandle: TFmxHandle;
  begin
    MenuItemHandle := (AMenuItem as INativeControl).Handle;
    ParentHandle := GetParentMenuItemHandle(AMenuItem);
    if ParentHandle = 0 then
      Exit;

    EnableMenuItem(ParentHandle, FHMenuMap[MenuItemHandle].Menuid, MF_BYCOMMAND or Enables[AMenuItem.enabled]);
  end;

  procedure UpdateChecked(const AMenuItem: TMenuItem);
  const
    Checks: array[Boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
  var
    ParentHandle: TFmxHandle;
    MenuItemHandle: TFmxHandle;
  begin
    MenuItemHandle := (AMenuItem as INativeControl).Handle;
    ParentHandle := GetParentMenuItemHandle(AMenuItem);
    if ParentHandle = 0 then
      Exit;

    CheckMenuItem(ParentHandle, FHMenuMap[MenuItemHandle].Menuid, MF_BYCOMMAND or Checks[AMenuItem.IsChecked]);
  end;

  procedure RecreateMainMenu(const AMenuItem: TMenuItem);
  var
    MainMenu: TMainMenu;
  begin
    MainMenu := AMenuItem.MainMenu;
    if MainMenu <> nil then
      MainMenu.RecreateOSMenu;
  end;

  function IsStyledBorder(const AMenuItem: TMenuItem): Boolean;
  var
    Form: TCommonCustomForm;
  begin
    if AMenuItem.Root = nil then
      Exit(False);

    Form := AMenuItem.Root.GetObject as TCommonCustomForm;
    Result := Form.Border.IsSupported;
  end;

var
  MenuItem: TMenuItem;
  MenuItemHandle: TFmxHandle;
begin
  if not (AItem.GetObject is TMenuItem) then
    Exit;

  MenuItem := TMenuItem(AItem.GetObject);
  MenuItemHandle := (MenuItem as INativeControl).Handle;
  if (MenuItemHandle = 0) or IsStyledBorder(MenuItem) then
  begin
    RecreateMainMenu(MenuItem);
    Exit;
  end;

  if MenuItemHandle = INVALID_HANDLE_VALUE then
    raise EInvalidFmxHandle.CreateFMT(SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(MenuItemHandle) * 2, MenuItemHandle]);

  if AChange = [TMenuItemChange.Bitmap] then
    UpdateBitmap(MenuItem)
  else if AChange = [TMenuItemChange.Enabled] then
    UpdateEnabled(MenuItem)
  else if AChange = [TMenuItemChange.Checked] then
    UpdateChecked(MenuItem)
  else
    // If Visible, Text or Shortcut are changed, then we need to rebuild menu. Because WinApi doesn't support
    RecreateMainMenu(MenuItem);
end;

procedure TWinMenuService.WMCommand(var Message: TWMCommand);
var
  MenuInfo: TWinMenuInfo;
begin
  if FindMenuInfoById(Message.ItemId, MenuInfo) then
  begin
    TOpenMenuItem(MenuInfo.FMXMenuItem).Click;
    Message.Result := 0;
  end;
end;

procedure TWinMenuService.WMInitMenuPopup(var Message: TWMInitMenuPopup);
var
  MenuInfo: TWinMenuInfo;
begin
  if FHMenuMap.TryGetValue(Message.MenuPopup, MenuInfo) then
    TOpenMenuItem(MenuInfo.FMXMenuItem).Click;
end;

procedure TWinMenuService.WMMenuSelect(var Message: TWMMenuSelect);
var
  MenuInfo: TWinMenuInfo;
  MenuHandle: HMENU;
begin
  MenuHandle := GetSubMenu(Message.Menu, Message.IDItem);
  if MenuHandle = 0 then
    Exit;

  if FindMenuInfoById(Message.IDItem, MenuInfo) then
    Application.Hint := MenuInfo.FMXMenuItem.Hint
  else
    Application.Hint := string.Empty;
end;

{ TWin32MenuInfo }

constructor TWinMenuService.TWinMenuInfo.Create(const AMenuId: Integer; const AnItem: TMenuItem);
begin
  MenuID := AMenuId;
  FMXMenuItem := AnItem;
end;

end.
