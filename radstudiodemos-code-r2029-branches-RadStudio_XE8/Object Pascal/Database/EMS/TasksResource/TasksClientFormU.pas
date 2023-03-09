unit TasksClientFormU;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  FMX.ListView.Types, Data.Bind.GenData, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.Layouts, FMX.Memo, FMX.StdCtrls, FMX.Edit, Data.Bind.ObjectScope,
  FMX.TabControl, FMX.ListView, REST.Backend.KinveyProvider,
  System.Generics.Collections,REST.Backend.ServiceTypes,
  System.Actions, FMX.ActnList,
  FMX.Controls.Presentation, FMX.MobilePreview, FMX.ScrollBox, System.ImageList,
  FMX.ImgList, Fmx.Bind.GenData, FMX.ListBox, FMX.Ani, FMX.MultiView,
  REST.Backend.PushTypes;

type
  TTasksClientForm = class(TForm)
    ToolBar1: TToolBar;
    TabControl1: TTabControl;
    ListViewTasks: TListView;
    TabItemList: TTabItem;
    TabItemDetails: TTabItem;
    EditTitle: TEdit;
    ButtonAdd: TButton;
    TabItemAdd: TTabItem;
    MemoAddContent: TMemo;
    EditAddTitle: TEdit;
    ButtonSaveAdd: TButton;
    ButtonCancelAdd: TButton;
    TabItemEdit: TTabItem;
    EditEditTitle: TEdit;
    ButtonEditSave: TButton;
    ButtonEditCancel: TButton;
    MemoEditContent: TMemo;
    ActionList1: TActionList;
    ActionAdd: TAction;
    ActionAddSave: TAction;
    ActionAddCancel: TAction;
    ActionEditSave: TAction;
    ActionEditCancel: TAction;
    ButtonBack: TButton;
    ActionEdit: TAction;
    ActionBack: TAction;
    ButtonDelete: TButton;
    ActionDelete: TAction;
    LabelToolBarCaption: TLabel;
    ActionToolBarCaption: TAction;
    ActionRefresh: TAction;
    ActionNext: TAction;
    ActionPrior: TAction;
    SpeedButtonNext: TSpeedButton;
    SpeedButtonPrior: TSpeedButton;
    ButtonEdit: TButton;
    Layout5: TLayout;
    RefreshList: TButton;
    ToolBar4: TToolBar;
    BindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkControlToFieldTitle: TLinkControlToField;
    LinkListControlToField1: TLinkListControlToField;
    LinkControlToField1: TLinkControlToField;
    ActionLogin: TAction;
    ActionLogout: TAction;
    ActionSignup: TAction;
    ButtonNext: TButton;
    ActionForward: TAction;
    LayoutTabControl: TLayout;
    ToolBar2: TToolBar;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    Layout1: TLayout;
    Layout2: TLayout;
    EditCreatedBy: TEdit;
    LabelCreatedBy: TLabel;
    LinkControlToFieldCreatedBy: TLinkControlToField;
    EditAssignedTo: TEdit;
    LabelAssignedTo: TLabel;
    LinkControlToFieldAssignedTo: TLinkControlToField;
    LabelStatus: TLabel;
    Layout3: TLayout;
    Layout4: TLayout;
    Label5: TLabel;
    ComboBoxAssignTo: TComboBox;
    EditViewStatus: TEdit;
    LinkControlToField5: TLinkControlToField;
    ActionTaskMessages: TAction;
    ActionTestConnection: TAction;
    ActionRefreshTaskMessages: TAction;
    GridLayout1: TGridLayout;
    Layout10: TLayout;
    Layout11: TLayout;
    Layout12: TLayout;
    TabItemMessages: TTabItem;
    ListViewMessages: TListView;
    ToolBar6: TToolBar;
    Button2: TButton;
    ActionRefreshMessages: TAction;
    TaskMessagesBindSource: TPrototypeBindSource;
    LinkFillControlToField3: TLinkFillControlToField;
    Layout17: TLayout;
    Layout18: TLayout;
    Label10: TLabel;
    ComboBoxEditStatus: TComboBox;
    LinkFillControlToField1: TLinkFillControlToField;
    LinkFillControlToField4: TLinkFillControlToField;
    Button3: TButton;
    LayoutNotification: TLayout;
    ButtonGotoNotification: TButton;
    LayoutNotificationAnimation: TFloatAnimation;
    Layout13: TLayout;
    LabelNotificationsCaption: TLabel;
    TabItemMessageDetails: TTabItem;
    MemoMessageDetails: TMemo;
    LinkControlToField7: TLinkControlToField;
    TabItemEditMessage: TTabItem;
    MemoEditMessage: TMemo;
    LinkControlToField8: TLinkControlToField;
    ToolBar7: TToolBar;
    ButtonEditMessage: TButton;
    ButtonDeleteMessage: TButton;
    Button6: TButton;
    TabItemAddMessage: TTabItem;
    Layout22: TLayout;
    Label15: TLabel;
    EditMessageCreatedBy: TEdit;
    LinkControlToField9: TLinkControlToField;
    MemoTask: TMemo;
    LinkControlToFieldContent: TLinkControlToField;
    LinkListControlToField2: TLinkListControlToField;
    MemoAddMessage: TMemo;
    LinkControlToFieldMessage: TLinkControlToField;
    MultiView1: TMultiView;
    ListViewNotifications: TListView;
    Layout23: TLayout;
    Label16: TLabel;
    ButtonClearNotifications: TButton;
    ActionClearNotifications: TAction;
    TimerShowNotificationPanel: TTimer;
    LayoutPriorNext: TLayout;
    Layout6: TLayout;
    EditTaskCreatedAt: TEdit;
    Label1: TLabel;
    Layout7: TLayout;
    EditTaskUpdatedAt: TEdit;
    Label7: TLabel;
    LinkControlToFieldCreatedAt: TLinkControlToField;
    LinkControlToFieldUpdatedAt: TLinkControlToField;
    GridLayout2: TGridLayout;
    Layout9: TLayout;
    Layout14: TLayout;
    EditMessageCreatedAt: TEdit;
    Label8: TLabel;
    Layout15: TLayout;
    Edit1: TEdit;
    Label9: TLabel;
    LinkControlToFieldCreatedAt2: TLinkControlToField;
    LinkControlToFieldUpdatedAt2: TLinkControlToField;
    Layout16: TLayout;
    EditMessageKind: TEdit;
    Label14: TLabel;
    LinkControlToFieldKind: TLinkControlToField;
    ImageList1: TImageList;
    Layout21: TLayout;
    EditTaskTitle: TEdit;
    Label18: TLabel;
    LinkControlToFieldTaskTitle: TLinkControlToField;
    Layout24: TLayout;
    EditCommentFor: TEdit;
    Label19: TLabel;
    LinkControlToFieldTaskTitle2: TLinkControlToField;
    Layout25: TLayout;
    EditAddCommentFor: TEdit;
    Label20: TLabel;
    LinkControlToFieldTaskTitle3: TLinkControlToField;
    Layout26: TLayout;
    EditMessagesFor: TEdit;
    Label21: TLabel;
    LinkControlToFieldTaskTitle4: TLinkControlToField;
    TabItemSettings: TTabItem;
    EditHostName1: TEdit;
    EditURLPort1: TEdit;
    EditURLPath1: TEdit;
    ComboBoxProtocol: TComboBox;
    EditUserName1: TEdit;
    EditUserPassword1: TEdit;
    ButtonLogout: TButton;
    ButtonSignup: TButton;
    Button8: TButton;
    ButtonLogin: TButton;
    Layout32: TLayout;
    Glyph1: TGlyph;
    LinkPropertyToFieldStatusImageIndex: TLinkPropertyToField;
    GlyphEditTask: TGlyph;
    Layout33: TLayout;
    Layout34: TLayout;
    Glyph3: TGlyph;
    LinkPropertyToFieldStatusImageIndex3: TLinkPropertyToField;
    ListBoxSettings: TListBox;
    Settings: TListBoxGroupHeader;
    ListBoxItemHost: TListBoxItem;
    ListBoxItemPath: TListBoxItem;
    ListBoxItemPort: TListBoxItem;
    ListBoxItemProtocol: TListBoxItem;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItemUserName: TListBoxItem;
    ListBoxItemUserPassword: TListBoxItem;
    Layout27: TLayout;
    Layout28: TLayout;
    Layout30: TLayout;
    Layout35: TLayout;
    Layout36: TLayout;
    Layout37: TLayout;
    ListBoxItemLoggedInName: TListBoxItem;
    ListBoxItemLoginLogout: TListBoxItem;
    Layout19: TLayout;
    EditLoggedInUserName: TEdit;
    ListBoxItemLogout: TListBoxItem;
    ListBoxUnusedSettings: TListBox;
    Layout8: TLayout;
    Layout29: TLayout;
    Layout31: TLayout;
    Layout38: TLayout;
    LayoutMessageDetailsMemo: TLayout;
    procedure ListViewTasksItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure FormCreate(Sender: TObject);
    procedure ActionAddExecute(Sender: TObject);
    procedure ActionAddUpdate(Sender: TObject);
    procedure ActionAddCancelExecute(Sender: TObject);
    procedure ActionAddSaveExecute(Sender: TObject);
    procedure ActionAddSaveUpdate(Sender: TObject);
    procedure ActionEditSaveExecute(Sender: TObject);
    procedure ActionEditCancelExecute(Sender: TObject);
    procedure ActionEditExecute(Sender: TObject);
    procedure ActionEditUpdate(Sender: TObject);
    procedure ActionBackExecute(Sender: TObject);
    procedure ActionBackUpdate(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionToolBarCaptionUpdate(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionNextExecute(Sender: TObject);
    procedure ActionNextUpdate(Sender: TObject);
    procedure ActionPriorExecute(Sender: TObject);
    procedure ActionPriorUpdate(Sender: TObject);
    procedure ActionEditSaveUpdate(Sender: TObject);
    procedure ActionToolBarCaptionExecute(Sender: TObject);
    procedure BindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure ActionLoginExecute(Sender: TObject);
    procedure ActionLoginUpdate(Sender: TObject);
    procedure ActionSignupExecute(Sender: TObject);
    procedure ActionSignupUpdate(Sender: TObject);
    procedure ActionLogoutExecute(Sender: TObject);
    procedure ActionLogoutUpdate(Sender: TObject);
    procedure ActionForwardExecute(Sender: TObject);
    procedure ActionForwardUpdate(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure ActionAddCancelUpdate(Sender: TObject);
    procedure ActionEditCancelUpdate(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionTaskMessagesExecute(Sender: TObject);
    procedure ActionTestConnectionExecute(Sender: TObject);
    procedure ActionTestConnectionUpdate(Sender: TObject);
    procedure ActionRefreshTaskMessagesExecute(Sender: TObject);
    procedure GridLayout1Resize(Sender: TObject);
    procedure TaskMessagesBindSourceCreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure ListViewMessagesClick(Sender: TObject);
    procedure ButtonCloseNotificationClick(Sender: TObject);
    procedure ListViewDetailMessagesPullRefresh(Sender: TObject);
    procedure ButtonGotoNotificationClick(Sender: TObject);
    procedure ButtonClearNotificationsClick(Sender: TObject);
    procedure ActionClearNotificationsExecute(Sender: TObject);
    procedure ActionClearNotificationsUpdate(Sender: TObject);
    procedure TimerShowNotificationPanelTimer(Sender: TObject);
    procedure ActionTaskMessagesUpdate(Sender: TObject);
    procedure GridLayout2Resize(Sender: TObject);
    procedure ListViewNotificationsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  public
    type
      TView = (Settings, Login, List, Details, Add, Edit, Messages, MessageDetails, AddMessage, EditMessage);
  private
    FTasksBindSourceAdapter: TBindSourceAdapter;
    FTaskMessagesBindSourceAdapter: TBindSourceAdapter;
    FViewStack: TStack<TView>;
    FSaving: Boolean;
    FLayoutNotificationHeight: Single;
    procedure ShowView(AView: TView);
    function CurrentView: TView;
    function GetTitleField: TBindSourceAdapterField;
    function GetContentField: TBindSourceAdapterField;
    procedure PopView;
    procedure PushView(const AView: TView);
    procedure PushViews(const AViews: array of TView);
    procedure ShowPriorNext;
    function GetAdapter: TBindSourceAdapter;
    procedure UpdateLoginPage;
    procedure RefreshTasks;
    procedure RefreshTaskMessages;
    procedure PushRegister;
    procedure ShowPushNotification(const ARefresh: Boolean; const AData: TPushData);
    procedure HideNotificationLayout;
    procedure ShowNotificationLayout;
    procedure RefreshUserNames;
    procedure OnIdle(Sender: TObject; var ADone: Boolean);
    procedure ShowMessage(const ATaskID, AMessageID: string);
    procedure ShowTask(const ATaskID: string);
    procedure DoLogin;
    procedure DoAfterLogin;
    procedure DoSignup;
    procedure DoAfterConnectionChange;
    procedure UpdateConnectionSettings;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  TasksClientForm: TTasksClientForm;

implementation

{$R *.fmx}

uses TasksAdapterModuleU, TasksClientModuleU, FMX.VirtualKeyboard, FMX.Platform, System.Math,
  REST.Backend.EMSProvider, TasksTypesU,
  System.PushNotification, TasksClientSettingsU;

function TTasksClientForm.GetAdapter: TBindSourceAdapter;
begin
  Result := Self.BindSource1.InternalAdapter;
end;

procedure TTasksClientForm.ListViewTasksItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  PushView(TView.Details);
end;

procedure TTasksClientForm.ListViewDetailMessagesPullRefresh(Sender: TObject);
begin
  RefreshTaskMessages;
end;

procedure TTasksClientForm.ListViewMessagesClick(Sender: TObject);
begin
  PushView(TView.MessageDetails);
end;

procedure TTasksClientForm.ListViewNotificationsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  LMessageID: string;
  LTaskID: string;
  LKind: string;
begin
  if AItem <> nil then
  begin
    AItem.Data['taskid'].TryAsType<string>(LTaskID);
    AItem.Data['messagekind'].TryAsType<string>(LKind);
    case TTasksUtil.StringToKind(LKind) of
      TMessage.TKind.Comment:
        if (LTaskID <> '') and AItem.Data['messageid'].TryAsType<string>(LMessageID) then
          ShowMessage(LTaskID, LMessageID);
      TMessage.TKind.TaskCreated,
      TMessage.TKind.TaskUpdated:
        if LTaskID <> '' then
          ShowTask(LTaskID)
    else
      Assert(False);
    end;
  end;
end;

procedure TTasksClientForm.ShowMessage(const ATaskID, AMessageID: string);
begin
  if MultiView1.Visible then
    MultiView1.Presenter.Close;
  if BindSource1.Locate('ID', ATaskID) then
  begin
    RefreshTaskMessages;
    if TaskMessagesBindSource.Locate('ID', AMessageID) then
      PushViews([TView.List, TView.Details, TView.Messages, TView.MessageDetails])
    else
      MessageDlg(Format('Unable to locate message id "%s".', [AMessageID]),
        TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0,
          procedure(const AResult: TModalResult)
            begin
            end);

  end
  else
    MessageDlg(Format('Unable to locate task id "%s".', [ATaskID]),
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0,
        procedure(const AResult: TModalResult)
          begin
          end);

end;

procedure TTasksClientForm.ShowTask(const ATaskID: string);
begin
  if MultiView1.Visible then
    MultiView1.Presenter.Close;
  if BindSource1.Locate('ID', ATaskID) then
    PushViews([TView.List, TView.Details])
  else
    MessageDlg(Format('Unable to locate task id %s".', [ATaskID]),
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0,
        procedure(const AResult: TModalResult)
          begin
          end);
end;

procedure TTasksClientForm.UpdateLoginPage;
  procedure ShowItem(const AListBoxItem: TListBoxItem; AVisible: Boolean);
  begin
    if AVisible then
      if not ListBoxSettings.Content.ContainsObject(AListBoxItem) then
        ListBoxSettings.AddObject(AListBoxItem);

    if not AVisible then
      if not ListBoxUnusedSettings.Content.ContainsObject(AListBoxItem) then
        ListBoxUnusedSettings.AddObject(AListBoxItem);



  end;

begin
  EditLoggedInUserName.Text := TasksClientModule.LoggedInUserName;
  ShowItem(ListBoxItemUserName, not TasksClientModule.LoggedIn);
  ShowItem(ListBoxItemUserPassword, not TasksClientModule.LoggedIn);
  ShowItem(ListBoxItemLoggedInName, TasksClientModule.LoggedIn);
  ShowItem(ListboxItemLoginLogout, not TasksClientModule.LoggedIn);
  ShowItem(ListBoxItemLogout, TasksClientModule.LoggedIn);
end;

procedure TTasksClientForm.RefreshTasks;
begin
  TasksAdapterModule.RefreshTasksAdapter;
end;

procedure TTasksClientForm.RefreshTaskMessages;
begin
  TasksAdapterModule.RefreshTaskMessagesAdapter;
end;

procedure TTasksClientForm.RefreshUserNames;
var
  LUserNames: TArray<string>;
  S: string;
begin
  LUserNames := TasksClientModule.GetUserNames;
  ComboBoxAssignTo.BeginUpdate;
  try
    ComboBoxAssignTo.Clear;
    ComboBoxAssignTo.Items.Add('');
    for S in LUserNames do
      ComboBoxAssignTo.Items.Add(S);
  finally
    ComboBoxAssignTo.EndUpdate;
  end;

end;

procedure TTasksClientForm.TaskMessagesBindSourceCreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  Assert(FTaskMessagesBindSourceAdapter = nil);
  FTaskMessagesBindSourceAdapter := TasksAdapterModule.TaskMessagesBindSourceAdapter;
  ABindSourceAdapter := FTaskMessagesBindSourceAdapter;
end;

procedure TTasksClientForm.TimerShowNotificationPanelTimer(Sender: TObject);
begin
  if not FSaving then
  begin
    TimerShowNotificationPanel.Enabled := False;
    ShowNotificationLayout;
  end;
end;

procedure TTasksClientForm.ShowPushNotification(const ARefresh: Boolean; const AData: TPushData);
var
  I: Integer;
  LItem: TListViewItem;
  LMessageId: string;
  LTaskId: string;
  LKind: string;
begin
  if ARefresh then
  begin
    RefreshTasks;
    case CurrentView of
      TView.Edit, TView.EditMessage:;
    else
      RefreshTasks;
      case CurrentView of
        TView.Messages,
        TView.MessageDetails:
          RefreshTaskMessages;
      end;
    end;
  end;

  // TODO: If item is already in list, just select it
  I := AData.Extras.IndexOf('taskid');
  if I >= 0 then
    LTaskID := AData.Extras.Items[I].Value;
  I := AData.Extras.IndexOf('messageid');
  if I >= 0 then
    LMessageID := AData.Extras.Items[I].Value;
  I := AData.Extras.IndexOf('messagekind');
  if I >= 0 then
    LKind := AData.Extras.Items[I].Value;
  LItem := ListViewNotifications.Items.Add;
  LItem.Data['taskid'] := LTaskID;
  LItem.Data['messageid'] := LMessageID;
  LItem.Data['messagekind'] := LKind;

  LItem.Text := AData.Message;
  LItem.ImageIndex := TMessageWrapper.GetKindImageIndex(TTasksUtil.StringToKind(LKind));
  ShowNotificationLayout;
end;

procedure TTasksClientForm.PushRegister;
begin
  // Enable push if there are any push services
  if TPushServiceManager.Instance.Count > 0 then
  begin

    TasksClientModule.OnDeviceTokenReceived :=
      procedure
      begin
        TasksClientModule.PushRegister;
      end;

    if not TasksClientModule.PushEvents1.Active then
    try
      TasksClientModule.PushEvents1.Active := True;
    except
      // TODO
    end;

    if TasksClientModule.PushEvents1.StartupNotification <> nil then
      ShowPushNotification(False, TasksClientModule.PushEvents1.StartupNotification);
  end;
end;

procedure TTasksClientForm.DoLogin;
begin
  TasksClientModule.Login(EditUserName1.Text, EditUserPassword1.Text);
  DoAfterLogin;
end;

procedure TTasksClientForm.DoSignup;
begin
  TasksClientModule.Signup(EditUserName1.Text, EditUserPassword1.Text);
  DoAfterLogin;
end;

procedure TTasksClientForm.DoAfterLogin;
begin
  TTasksClientSettings.Instance.Update(
    procedure(const AWriter: TTasksClientSettings.TWriter) begin AWriter.WriteUserName(TasksClientModule.BackendAuth1.UserName); end);
  UpdateLoginPage;
  DoAfterConnectionChange;
  PushRegister;
end;

procedure TTasksClientForm.DoAfterConnectionChange;
begin
  ListViewTasks.Items.Clear;
  RefreshTasks;
  RefreshUserNames;
end;

procedure TTasksClientForm.ActionLoginExecute(Sender: TObject);
begin
  DoLogin;
end;

procedure TTasksClientForm.ActionLoginUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := not TasksClientModule.BackendAuth1.LoggedIn;
end;

procedure TTasksClientForm.ActionLogoutExecute(Sender: TObject);
begin
  TasksClientModule.BackendAuth1.Logout;
  UpdateLoginPage;
end;

procedure TTasksClientForm.ActionLogoutUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := TasksClientModule.BackendAuth1.LoggedIn;
end;

constructor TTasksClientForm.Create(AOwner: TComponent);
begin
  inherited;
  FViewStack := TStack<TView>.Create;
end;

function TTasksClientForm.CurrentView: TView;
begin
 if Self.TabControl1.ActiveTab = TabItemAdd then
   Result := TView.Add
 else if Self.TabControl1.ActiveTab = TabItemAddMessage then
   Result := TView.AddMessage
 else if Self.TabControl1.ActiveTab = TabItemList then
   Result := TView.List
 else if Self.TabControl1.ActiveTab = TabItemEdit then
   Result := TView.Edit
 else if Self.TabControl1.ActiveTab = TabItemEditMessage then
   Result := TView.EditMessage
 else if Self.TabControl1.ActiveTab = TabItemDetails then
   Result := TView.Details
 else if Self.TabControl1.ActiveTab = TabItemMessageDetails then
   Result := TView.MessageDetails
 else if Self.TabControl1.ActiveTab = TabItemSettings then
   Result := TView.Settings
 else if Self.TabControl1.ActiveTab = TabItemMessages then
   Result := TView.Messages
 else
   raise Exception.Create('Unexpected');
end;

destructor TTasksClientForm.Destroy;
begin
  FViewStack.Free;
  inherited;
end;

procedure TTasksClientForm.PushView(const AView: TView);
begin
  FViewStack.Push(CurrentView);
  ShowView(AView);
end;

procedure TTasksClientForm.PushViews(const AViews: array of TView);
var
  I: Integer;
begin
  FViewStack.Clear;
  Assert(Length(AViews) >= 2);
  for I := 0 to Length(AViews) - 1 do
    FViewStack.Push(AViews[I]);
  ShowView(AViews[Length(AViews)-1]);
end;

procedure TTasksClientForm.PopView;
begin
  if FViewStack.Count > 0 then
    ShowView(FViewStack.Pop);
end;

procedure TTasksClientForm.UpdateConnectionSettings;
begin
  TTasksClientSettings.Instance.Update(
    procedure(const AWriter: TTasksClientSettings.TWriter)
    begin
      AWriter.WriteHost(TasksClientModule.EMSProvider1.URLHost);
      AWriter.WritePort(TasksClientModule.EMSProvider1.URLPort);
      AWriter.WritePath(TasksClientModule.EMSProvider1.URLBasePath);
      AWriter.WriteProtocol(TasksClientModule.EMSProvider1.URLProtocol);
    end);
end;

procedure TTasksClientForm.FormCreate(Sender: TObject);
var
  FService: IFMXVirtualKeyboardToolbarService;
  LProtocol: string;
  LPort: Integer;
  LStatus: TTask.TStatus;
begin

  UpdateLoginPage;
  FLayoutNotificationHeight := LayoutNotification.Height;
  LayoutNotification.Height := 0;
  ShowView(TView.Settings);
  TabControl1.TabPosition := TTabPosition.None;
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardToolbarService, FService) then
  begin
    FService.SetToolbarEnabled(False);
    FService.SetHideKeyboardButtonVisibility(False);
  end;

  // Populate combobox with TTask.TStatus string
  for LStatus := Low(TTask.TStatus) to High(TTask.TStatus) do
    ComboBoxEditStatus.Items.Add(TTasksUtil.StatusToString(LStatus));

  // Load settings from ini file (if any)
  TTasksClientSettings.Instance.Load;
  if not TTasksClientSettings.Instance.ConfigFileExists then
    UpdateConnectionSettings;  // Write properties from EMSProvider1
  EditHostName1.Text := TTasksClientSettings.Instance.Host;
  EditURLPort1.Text := TTasksClientSettings.Instance.Port.ToString;
  EditURLPath1.Text := TTasksClientSettings.Instance.Path;
  if SameText(TTasksClientSettings.Instance.Protocol, 'https') then
    ComboBoxProtocol.ItemIndex := 1
  else
    ComboBoxProtocol.ItemIndex := 0;
  EditUserName1.Text := TTasksClientSettings.Instance.Username;

  // Update connection from controls
  TasksClientModule.OnUpdateConnection :=
    procedure(const AProvider: TEMSProvider; var AChanged: Boolean)
    begin
      if AProvider.URLHost <> EditHostName1.Text then
      begin
        AProvider.URLHost := EditHostName1.Text;
        AChanged := True;
      end;
      if AProvider.URLBasePath <> EditURLPath1.Text then
      begin
        AProvider.URLBasePath := EditURLPath1.Text;
        AChanged := True;
      end;
      LPort := StrToInt(EditURLPort1.Text);
      if AProvider.URLPort <> LPort then
      begin
        AProvider.URLPort := LPort;
        AChanged := True;
      end;
      if ComboBoxProtocol.ItemIndex = 1 then
        LProtocol := 'https'
      else
        LProtocol := 'http';
      if LProtocol <> AProvider.URLProtocol then
      begin
        AChanged := True;
        AProvider.URLProtocol := LProtocol;
      end;
      if AChanged then
        // Update .ini file
        UpdateConnectionSettings;
    end;

  TasksClientModule.OnPushReceived :=
    procedure(const APushData: TPushData)
    begin
      ShowPushNotification(True, APushData);
    end;

  Assert(TasksClientModule.PushEvents1.Active = False); // Design time state
  Application.OnIdle := OnIdle;
end;


procedure TTasksClientForm.OnIdle(Sender: TObject; var ADone: Boolean);
begin
  ShowPriorNext;
  if ListViewNotifications.Items.Count = 0 then
  begin
    // Hide notifications when list is empty
    if MultiView1.Visible then
      MultiView1.Presenter.Close;
    if LayoutNotification.Height <> 0 then
      HideNotificationLayout;
  end;

  case CurrentView of
    TView.Edit:
    begin
      // Keep Glyph synchronized with combobox when editing a task
      // LiveBindings won't update the image until post
      GlyphEditTask.ImageIndex := TasksAdapterModule.CurrentTaskStatusImageIndex;
    end;
    TView.MessageDetails:
    begin
      // Hide memo unless message is a comment
      LayoutMessageDetailsMemo.Visible := TasksAdapterModule.CurrentMessageKind = TMessage.TKind.Comment;
    end;
  end;
end;

resourcestring
  sFieldNotFound = 'Field not found.';

function TTasksClientForm.GetContentField: TBindSourceAdapterField;
begin
  Result := GetAdapter.FindField('Content');
  if Result = nil then
    raise Exception.Create(sFieldNotFound);
end;

function TTasksClientForm.GetTitleField: TBindSourceAdapterField;
begin
  Result := GetAdapter.FindField('Title');
  if Result = nil then
    raise Exception.Create(sFieldNotFound);
end;

procedure TTasksClientForm.GridLayout1Resize(Sender: TObject);
begin
  GridLayout1.ItemWidth := GridLayout1.Width;
end;

procedure TTasksClientForm.GridLayout2Resize(Sender: TObject);
begin
  GridLayout2.ItemWidth := GridLayout2.Width;
end;

procedure TTasksClientForm.ActionAddCancelExecute(Sender: TObject);
begin
  case CurrentView of
    TView.Add:
    begin
      PopView;
      if FTasksBindSourceAdapter.State in seEditModes then
      begin
        FTasksBindSourceAdapter.Cancel;
        FTasksBindSourceAdapter.Delete;
      end;
    end;
    TView.AddMessage:
    begin
      PopView;
      if FTaskMessagesBindSourceAdapter.State in seEditModes then
      begin
        FTaskMessagesBindSourceAdapter.Cancel;
        FTaskMessagesBindSourceAdapter.Delete;
      end;
    end;
  else
    Assert(False);
  end;
end;

procedure TTasksClientForm.ActionAddCancelUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Add,
    TView.AddMessage:
      (Sender as TAction).Visible := True;
  else
    (Sender as TAction).Visible := False;
  end;
end;

procedure TTasksClientForm.ActionAddExecute(Sender: TObject);
begin
  case CurrentView of
    TView.List:
    begin
      PushView(TView.Add);
      GetAdapter.Append;
    end;
    TView.Messages:
    begin
      PushView(TView.AddMessage);
      FTaskMessagesBindSourceAdapter.Append;
    end;
  else
    Assert(False);
  end;
end;


procedure TTasksClientForm.ActionAddSaveExecute(Sender: TObject);
begin
  case CurrentView of
    TView.Add:
    begin
      FSaving := True;
      try
        FTasksBindSourceAdapter.Post;
      except
        FSaving := False;
        raise;
      end;
      FSaving := False;
      PopView;
    end;
    TView.AddMessage:
    begin
      FSaving := True;
      try
        FTaskMessagesBindSourceAdapter.Post;
      except
        FSaving := False;
        raise;
      end;
      FSaving := False;
      PopView;
    end;
  end;
end;

procedure TTasksClientForm.ActionAddSaveUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Add,
    TView.AddMessage:
      (Sender as TAction).Visible := True;
  else
      (Sender as TAction).Visible := False;
  end;
end;

procedure TTasksClientForm.ActionAddUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.List,
    TView.Messages:
      (Sender as TAction).Visible := True;
  else
      (Sender as TAction).Visible := False;
  end;
end;

procedure TTasksClientForm.ActionBackExecute(Sender: TObject);
begin
  if CurrentView = TView.List then
  begin
    ShowView(TView.Settings);
    FViewStack.Clear;
  end
  else if FViewStack.Count > 0 then
    PopView
  else
    ShowView(TView.List);
end;

procedure TTasksClientForm.ActionBackUpdate(Sender: TObject);
begin
        (Sender as TAction).Enabled := True;

  case CurrentView of
    TView.List,
    TView.Details:
    begin
      (Sender as TAction).Text := '';   // List or Messages
      (Sender as TAction).Visible := True;
    end;
    TView.MessageDetails:
    begin
      (Sender as TAction).Text := '';   // List or Messages
      (Sender as TAction).Visible := True;
    end;
    TView.Messages:
    begin
      (Sender as TAction).Text := '';
      (Sender as TAction).Visible := True;
    end
  else
    (Sender as TAction).Visible := False
  end;

end;

procedure TTasksClientForm.ActionClearNotificationsExecute(Sender: TObject);
begin
  ListViewNotifications.Items.Clear;
end;

procedure TTasksClientForm.ActionClearNotificationsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := ListViewNotifications.Items.Count > 0;
end;

procedure TTasksClientForm.ActionDeleteExecute(Sender: TObject);
var
  LTitle: string;
begin
  case CurrentView of
    TView.Details:
      LTitle := Format('Delete "%s"?', [GetTitleField.GetTValue.ToString]);
    TView.MessageDetails:
      LTitle := 'Delete message?';
  else
    Assert(False);
  end;
  MessageDlg(LTitle,
    TMsgDlgType.mtConfirmation,
    mbOKCancel,
    0,
    procedure(const AResult: TModalResult)
    begin
      if AResult <> mrCancel then
        case CurrentView of
          TView.Details:
          begin
            FTasksBindSourceAdapter.Delete;
            if FTaskMessagesBindSourceAdapter.ItemCount = 0 then
              // No more records
              ShowView(TView.List);
          end;
          TView.MessageDetails:
          begin
            FTaskMessagesBindSourceAdapter.Delete;
            if FTaskMessagesBindSourceAdapter.ItemCount = 0 then
              // No more records
              ShowView(TView.Messages);
          end;
        end;
    end
  );
end;

procedure TTasksClientForm.ActionDeleteUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details,
    TView.MessageDetails:
    begin
      case TasksAdapterModule.CurrentMessageKind of
        TMessage.TKind.Comment:
          (Sender as TAction).Visible := True;
      else
        (Sender as TAction).Visible := False;
      end;
    end
  else
      (Sender as TAction).Visible := False
  end;
end;

procedure TTasksClientForm.ActionEditCancelExecute(Sender: TObject);
begin
  MessageDlg('Cancel changes?', TMsgDlgType.mtConfirmation, mbOKCancel, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult <> mrCancel then
        case CurrentView of
          TView.Edit:
          begin
            if FTasksBindSourceAdapter.Modified then
              FTasksBindSourceAdapter.Cancel;
            PopView;
          end;
          TView.EditMessage:
          begin
            if FTaskMessagesBindSourceAdapter.Modified then
              FTaskMessagesBindSourceAdapter.Cancel;
            PopView;
          end
        else
          Assert(False);
        end;
    end);
end;

procedure TTasksClientForm.ActionEditCancelUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Edit,
    TView.EditMessage:
      (Sender as TAction).Visible := True
  else
      (Sender as TAction).Visible := False
  end;
end;

procedure TTasksClientForm.ActionEditExecute(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
      PushView(TView.Edit);
    TView.MessageDetails:
      PushView(TView.EditMessage);
  else
    Assert(False);
  end;
end;

procedure TTasksClientForm.ActionEditSaveExecute(Sender: TObject);
begin
  case CurrentView of
    TView.Edit:
    begin
      FSaving := True;
      try
        FTasksBindSourceAdapter.Post;
      except
        FSaving := False;
        raise;
      end;
      FSaving := False;
      PopView;
    end;
    TView.EditMessage:
    begin
      FSaving := True;
      try
        FTaskMessagesBindSourceAdapter.Post;
      except
        FSaving := False;
        raise;
      end;
      FSaving := False;
      PopView;
    end;
  end;
end;

procedure TTasksClientForm.ActionEditSaveUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Edit:
    begin
      (Sender as TAction).Visible := True;
      (Sender as TAction).Enabled := GetAdapter.Modified;
    end;
    TView.EditMessage:
    begin
      (Sender as TAction).Visible := True;
      (Sender as TAction).Enabled := FTaskMessagesBindSourceAdapter.Modified;
    end;
  else
   (Sender as TAction).Visible := False;
  end;
end;

procedure TTasksClientForm.ActionEditUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
      (Sender as TAction).Visible := True;
    TView.MessageDetails:
    begin
      case TasksAdapterModule.CurrentMessageKind of
        TMessage.TKind.Comment:
          (Sender as TAction).Visible := True;
      else
        (Sender as TAction).Visible := False;
      end;
    end
  else
    (Sender as TAction).Visible := False;
  end;
end;

procedure TTasksClientForm.ActionForwardExecute(Sender: TObject);
var
  LChanged: Boolean;
begin
  if not TasksClientModule.LoggedIn then
    DoLogin
  else
  begin
    TasksClientModule.UpdateConnection(LChanged);
    if LChanged then
      // Refresh lists if connection was changed.
      // Don't logout (could be the same server)
      DoAfterConnectionChange;
  end;

  ShowView(TView.List);
end;

procedure TTasksClientForm.ActionForwardUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := (CurrentView = TView.Settings);
  if  TAction(Sender).Visible then
    TAction(Sender).Text := 'Tasks';
end;

procedure TTasksClientForm.ActionToolBarCaptionExecute(Sender: TObject);
begin
//
end;

procedure TTasksClientForm.ActionToolBarCaptionUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.List: (Sender as TAction).Text := 'Tasks';
    TView.Details: (Sender as TAction).Text := Format('Task %d of %d',
      [FTasksBindSourceAdapter.ItemIndex+1, FTasksBindSourceAdapter.ItemCount]);
    TView.MessageDetails: (Sender as TAction).Text := Format('Message %d of %d',
      [FTaskMessagesBindSourceAdapter.ItemIndex+1, FTaskMessagesBindSourceAdapter.ItemCount]);
    TView.Add: (Sender as TAction).Text := 'Add Task';
    TView.Edit: (Sender as TAction).Text := 'Edit Task';
    TView.Login: (Sender as TAction).Text := 'User';
    TView.Settings: (Sender as TAction).Text := 'Settings';
    TView.Messages: (Sender as TAction).Text := 'Messages';
    TView.AddMessage: (Sender as TAction).Text := 'Add Comment';
    TView.EditMessage: (Sender as TAction).Text := 'Edit Comment';
  end;
end;

procedure TTasksClientForm.ShowPriorNext;
begin
  case CurrentView of
    TView.Details,
    TView.MessageDetails:
      LayoutPriorNext.Visible := True;  // Show container of prior,next speed buttons
  else
    LayoutPriorNext.Visible := False;
  end;
end;

procedure TTasksClientForm.ActionNextExecute(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
    begin
     BindSource1.Next;
     RefreshTaskMessages;
    end;
    TView.MessageDetails:
      TaskMessagesBindSource.Next;
  else
    Assert(False);
  end;
end;

procedure TTasksClientForm.ActionNextUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details:  TAction(Sender).Enabled := not BindSource1.EOF;
    TView.MessageDetails:  TAction(Sender).Enabled := not TaskMessagesBindSource.EOF;
  end;
end;

procedure TTasksClientForm.ActionPriorExecute(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
    begin
      BindSource1.Prior;
      RefreshTaskMessages;
    end;
    TView.MessageDetails:
      TaskMessagesBindSource.Prior;
  else
    Assert(False);
  end;
end;

procedure TTasksClientForm.ActionPriorUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details:  TAction(Sender).Enabled := not BindSource1.BOF;
    TView.MessageDetails:  TAction(Sender).Enabled := not TaskMessagesBindSource.BOF;
  end;
end;

procedure TTasksClientForm.ActionRefreshExecute(Sender: TObject);
begin
  RefreshTasks;
  RefreshUserNames;
end;

procedure TTasksClientForm.ActionRefreshTaskMessagesExecute(Sender: TObject);
begin
  RefreshTaskMessages;
  RefreshUserNames;
end;

procedure TTasksClientForm.ActionTaskMessagesExecute(Sender: TObject);
begin
  PushView(TView.Messages);
end;

procedure TTasksClientForm.ActionTaskMessagesUpdate(Sender: TObject);
begin
  case CurrentView of
    TView.Details:
    begin
      TAction(Sender).Visible := True;
      TAction(Sender).Text := Format('Messages(%d)', [FTaskMessagesBindSourceAdapter.ItemCount]);
    end
  else
    TAction(Sender).Visible := False;
  end;
end;

procedure TTasksClientForm.ActionSignupExecute(Sender: TObject);
begin
  DoSignup;
end;

procedure TTasksClientForm.ActionSignupUpdate(Sender: TObject);
begin
  TAction(Sender).Visible := not TasksClientModule.BackendAuth1.LoggedIn;
end;

procedure TTasksClientForm.ActionTestConnectionExecute(Sender: TObject);
begin
  try
    TasksClientModule.TestConnection;
    MessageDlg('Connection OK',
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0,
        procedure(const AResult: TModalResult)
          begin

          end
      );
  except
     on E: Exception do
       MessageDlg(Format('Unable to connect to %0:s.  %1:s', [TasksClientModule.EMSProvider1.BaseURL, E.Message]),
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0,
          procedure(const AResult: TModalResult)
            begin

            end
        );
  end;

end;

procedure TTasksClientForm.ActionTestConnectionUpdate(Sender: TObject);
begin
  //
end;

procedure TTasksClientForm.BindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  Assert(FTasksBindSourceAdapter = nil);
  FTasksBindSourceAdapter := BindSource1.InternalAdapter;
  FTasksBindSourceAdapter := TasksAdapterModule.TasksBindSourceAdapter;
  ABindSourceAdapter := FTasksBindSourceAdapter;
end;

procedure TTasksClientForm.ButtonClearNotificationsClick(Sender: TObject);
begin
//
end;

procedure TTasksClientForm.ButtonCloseNotificationClick(Sender: TObject);
begin
  HideNotificationLayout;
end;

procedure TTasksClientForm.ButtonGotoNotificationClick(Sender: TObject);
begin
  MultiView1.Visible := not MultiView1.Visible;
end;

procedure TTasksClientForm.ShowView(AView: TView);
begin
  case AView of
    TView.List:    Self.TabControl1.ActiveTab := TabItemList;
    TView.Details:
    begin
      RefreshTaskMessages;
      Self.TabControl1.ActiveTab := TabItemDetails;
    end;
    TView.MessageDetails: Self.TabControl1.ActiveTab := TabItemMessageDetails;
    TView.Add:     Self.TabControl1.ActiveTab := TabItemAdd;
    TView.AddMessage:     Self.TabControl1.ActiveTab := TabItemAddMessage;
    TView.Edit:    Self.TabControl1.ActiveTab := TabItemEdit;
    TView.EditMessage:    Self.TabControl1.ActiveTab := TabItemEditMessage;
    TView.Settings:   Self.TabControl1.ActiveTab := TabItemSettings;
    TView.Messages:
    begin
      Self.TabControl1.ActiveTab := TabItemMessages;
      RefreshTaskMessages;
    end;
  else
    raise Exception.Create('Unexpected');
  end;
end;

procedure TTasksClientForm.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  TabControl1.Align := TAlignLayout.Client;
end;

procedure TTasksClientForm.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  TabControl1.Align := TAlignLayout.Horizontal;
  TabControl1.Height := LayoutTabControl.Height - Bounds.Height;
end;

procedure TTasksClientForm.ShowNotificationLayout;
begin
  if FSaving then
  begin
    // Wait until done adding or editing, and message box is not shown.
    TimerShowNotificationPanel.Enabled := True;
  end
  else
  begin
    LayoutNotificationAnimation.Stop;
    LayoutNotificationAnimation.StopValue := FLayoutNotificationHeight;
    LayoutNotificationAnimation.StartValue := 0;
    LayoutNotificationAnimation.Start;
  end;
end;

procedure TTasksClientForm.HideNotificationLayout;
begin
  LayoutNotificationAnimation.Stop;
  LayoutNotificationAnimation.StopValue := 0;
  LayoutNotificationAnimation.StartValue := FLayoutNotificationHeight;
  LayoutNotificationAnimation.Start;
end;

end.
