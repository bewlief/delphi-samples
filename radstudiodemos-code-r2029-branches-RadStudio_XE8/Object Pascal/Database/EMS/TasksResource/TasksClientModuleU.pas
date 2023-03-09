//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit TasksClientModuleU;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Backend.ServiceTypes,
  REST.Backend.MetaTypes, REST.Backend.EMSServices, System.JSON,
  REST.Backend.EMSProvider, REST.Client, REST.Backend.EndPoint,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.ServiceComponents, TasksTypesU, System.Actions, FMX.ActnList,
  REST.Backend.Providers, REST.Backend.EMSPushDevice, System.PushNotification,
  REST.Backend.PushTypes, REST.Backend.PushDevice;

type
  TTasksClientModule = class(TDataModule)
    BackendAuth1: TBackendAuth;
    BackendEndpointDeleteNote: TBackendEndpoint;
    BackendEndpointUpdateNote: TBackendEndpoint;
    BackendEndpointGetNotes: TBackendEndpoint;
    EMSProvider1: TEMSProvider;
    BackendEndpointGetNote: TBackendEndpoint;
    BackendEndpointAddNote: TBackendEndpoint;
    BackendUsers1: TBackendUsers;
    BackendEndpointGetTaskMessages: TBackendEndpoint;
    BackendEndpointAddTaskMessage: TBackendEndpoint;
    PushEvents1: TPushEvents;
    BackendEndpointUpdateTaskMessage: TBackendEndpoint;
    BackendEndpointDeleteTaskMessage: TBackendEndpoint;
    procedure PushEvents1DeviceTokenReceived(Sender: TObject);
    procedure PushEvents1PushReceived(Sender: TObject; const AData: TPushData);
  public type
    TUpdateConnectionCallback = reference to procedure(const AProvider: TEMSProvider; var AChanged: Boolean);
    TPushDataCallback = reference to procedure(const APushData: TPushData);
  private
    FOnUpdateConnection: TUpdateConnectionCallback;
    FOnPushReceived: TPushDataCallback;
    FOnDeviceTokenReceived: TProc;
    function GetJSON<T>(const AResponse: TCustomRESTResponse): T;
    function GetJSONArray(const AResponse: TCustomRESTResponse): TJSONArray;
    function GetJSONObject(const AResponse: TCustomRESTResponse): TJSONObject;
    function GetLoggedIn: Boolean;
    function GetLoggedInUserName: string;
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateConnection; overload;
    procedure UpdateConnection(out AChanged: Boolean); overload;
    procedure Login(const AUserName, APassword: string);
    procedure Logout;
    procedure Signup(const AUserName, APassword: string);
    procedure AddTask(const ATask: TTask;
      out ACreatedBy: string; out ACreatedAt: TTaskDateTime; out AID: string);
    function FindTask(const ATitle: string; out ATask: TTask): Boolean;
    function DeleteTask(const AID: string): Boolean;
    function GetTask(const AID: string; out ATask: TTask): Boolean;
    procedure UpdateTask(const ATask: TTask; out AUpdatedAt: TTaskDateTime);
    function GetTasks: TArray<TTask>;
    function GetUserNames: TArray<string>;
    procedure PushRegister;
    // Add message to an item
    procedure AddMessage(const AMessage: TMessage;
      out ACreatedBy: string; out ACreatedAt: TTaskDatetime; out AMessageID: string); overload;
    procedure UpdateMessage(const AMessage: TMessage; out AUpdatedAt: TTaskDateTime);
    function DeleteMessage(const ATaskID, AMessageID: string): Boolean;
    function GetTaskMessages(const AID: string): TArray<TMessage>; overload;
    procedure TestConnection;
    property LoggedIn: Boolean read GetLoggedIn;
    property LoggedInUserName: string read GetLoggedInUserName;
    property OnUpdateConnection: TUpdateConnectionCallback read FOnUpdateConnection write FOnUpdateConnection;
    property OnPushReceived: TPushDataCallback read FOnPushReceived write FOnPushReceived;
    property OnDeviceTokenReceived: TProc read FOnDeviceTokenReceived write FOnDeviceTokenReceived;
  end;

var
  TasksClientModule: TTasksClientModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses FMX.Dialogs, REST.Types;

{$R *.dfm}

function TTasksClientModule.GetJSON<T>(const AResponse: TCustomRESTResponse): T;
begin
  if AResponse.JSONValue = nil then
    raise Exception.Create('JSON Expected');
  Result := AResponse.JSONValue.GetValue<T>('');
end;

function TTasksClientModule.GetJSONObject(const AResponse: TCustomRESTResponse): TJSONObject;
begin
  Result := GetJSON<TJSONObject>(AResponse);
end;

function TTasksClientModule.GetLoggedIn: Boolean;
begin
  Result := BackendAuth1.LoggedIn;
end;

function TTasksClientModule.GetLoggedInUserName: string;
begin
  Result := BackendAuth1.LoggedInUserName;
end;

function TTasksClientModule.GetJSONArray(const AResponse: TCustomRESTResponse): TJSONArray;
begin
  Result := GetJSON<TJSONArray>(AResponse);
end;

procedure TTasksClientModule.AddTask(const ATask: TTask;
  out ACreatedBy: string; out ACreatedAt: TTaskDatetime; out AID: string);
var
  LJSONObject: TJSONObject;
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointAddNote;
  LJSONObject := TTaskJSON.TaskToJSON(ATask);
  try
    LEndpoint.ClearBody;
    LEndpoint.AddBody(LJSONObject);
    LEndpoint.Execute;
    with GetJSONObject(LEndpoint.Response) do
    begin
      AID := GetValue<string>('id');
      ACreatedBy := GetValue<string>('createdby');
      ACreatedAt := TTaskDateTime.CreateUTCString(GetValue<string>('createdat'));
    end;
  finally
    LJSONObject.Free;
  end;
end;

procedure TTasksClientModule.AddMessage(const AMessage: TMessage;
  out ACreatedBy: string; out ACreatedAt: TTaskDatetime; out AMessageID: string);
var
  LJSONObject: TJSONObject;
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointAddTaskMessage;
  Assert(AMessage.TaskID <> '');
  LEndpoint.Params.Items[0].Value := AMessage.TaskID;
  LJSONObject := TMessageJSON.MessageToJSON(AMessage);
  try
    LEndpoint.ClearBody;
    LEndpoint.AddBody(LJSONObject);
    LEndpoint.Execute;
    with GetJSONObject(LEndpoint.Response) do
    begin
      AMessageID := GetValue<string>(TMessageJSON.TNames.Id);
      ACreatedBy := GetValue<string>(TMessageJSON.TNames.CreatedBy);
      ACreatedAt := TTaskDateTime.CreateUTCString(GetValue<string>(TMessageJSON.TNames.CreatedAt, ''));
    end;
  finally
    LJSONObject.Free;
  end;
end;

function TTasksClientModule.DeleteTask(const AID: string): Boolean;
var
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointDeleteNote;
  LEndpoint.Params.Items[0].Value := AID;
  // Don't raise exception on 404
  LEndpoint.AllowHTTPErrors := TBackendEndpoint.TAllowHTTPErrors.ClientErrorNotFound_404;
  LEndpoint.Execute;
  Result := LEndpoint.Response.Status.Success;
end;

function TTasksClientModule.GetTask(const AID: string; out ATask: TTask): Boolean;
var
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointGetNote;
  LEndpoint.Params.Items[0].Value := AID;
  // Don't raise exception on 404
  LEndpoint.AllowHTTPErrors := TBackendEndpoint.TAllowHTTPErrors.ClientErrorNotFound_404;
  LEndpoint.Execute;
  Result := LEndpoint.Response.Status.Success;
  if Result then
  begin
    ATask := TTaskJSON.JSONToTask(
      GetJSONObject(LEndpoint.Response));
  end;
end;

function TTasksClientModule.FindTask(const ATitle: string; out ATask: TTask): Boolean;
var
  LNotes: TArray<TTask>;
  LParam: TRESTRequestParameter;
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointGetNotes;
  LParam := LEndpoint.Params.AddItem;
  try
    // Add a parameter to get note request
    LParam.Kind := TRESTRequestParameterKind.pkGETorPOST;
    LParam.Name := 'title';
    LParam.Value := ATitle;
    BackendEndpointGetNote.Execute;
    LNotes := TTaskJSON.JSONToTasks(
      GetJSONArray(LEndpoint.Response));
    if Length(LNotes) = 0 then
      Result := False
    else
    begin
      Assert(Length(LNotes) = 1);
      ATask := LNotes[0];
      Result := True;
    end;
  finally
    // Delete the parameter
    LEndpoint.Params.Delete(LParam);
  end;
end;

function TTasksClientModule.GetTasks: TArray<TTask>;
var
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointGetNotes;
  LEndpoint.Execute;
  Result := TTaskJSON.JSONToTasks(
    GetJSONArray(LEndpoint.Response));
end;

function TTasksClientModule.GetTaskMessages(const AID: string): TArray<TMessage>;
var
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointGetTaskMessages;
  LEndpoint.Params.Items[0].Value := AID;
  LEndpoint.Execute;
  Result := TMessageJSON.JSONToMessages(
    GetJSONArray(LEndpoint.Response));
end;

function TTasksClientModule.GetUserNames: TArray<string>;
var
  LUsers: TArray<TBackendEntityValue>;
  LUser: TBackendEntityValue;
begin
  UpdateConnection;
  BackendUsers1.Users.QueryUsers(['order=username'], nil, LUsers);
  for LUser in LUsers do
    Result := Result + [LUser.UserName];
end;

procedure TTasksClientModule.Login(const AUserName, APassword: string);
begin
  UpdateConnection;
  BackendAuth1.UserName := AUserName;
  BackendAuth1.Password := APassword;
  BackendAuth1.Login;
end;

procedure TTasksClientModule.Logout;
begin
  BackendAuth1.Logout;
end;

procedure TTasksClientModule.PushEvents1DeviceTokenReceived(Sender: TObject);
begin
  if Assigned(FOnDeviceTokenReceived) then
    FOnDeviceTokenReceived;


end;

procedure TTasksClientModule.PushEvents1PushReceived(Sender: TObject;
  const AData: TPushData);
begin
  if Assigned(FOnPushReceived) then
    FOnPushReceived(AData);
end;

procedure TTasksClientModule.PushRegister;
var
  LJSONObject: TJSONObject;
begin
  Assert(BackendAuth1.UserName <> ''); // Should be logged in
  if PushEvents1.DeviceToken <> '' then
  begin
    LJSONObject := TJSONObject.Create;
    try
      // Associate installation with a username
      LJSONObject.AddPair('tasksusername', BackendAuth1.UserName);
      PushEvents1.RegisterDevice(LJSONObject);
    finally
      LJSONObject.Free;
    end;
  end;

end;

procedure TTasksClientModule.Signup(const AUserName, APassword: string);
begin
  UpdateConnection;
  BackendAuth1.UserName := AUserName;
  BackendAuth1.Password := APassword;
  BackendAuth1.Signup;
end;

procedure TTasksClientModule.TestConnection;
begin
  UpdateConnection;
  EMSProvider1.AppHandshake(   // Raises exception
    procedure(const AJSON: TJSONObject)
    begin
    end);
end;

procedure TTasksClientModule.UpdateConnection;
var
  LChanged: Boolean;
begin
  UpdateConnection(LChanged);
end;

procedure TTasksClientModule.UpdateConnection(out AChanged: Boolean);
begin
  AChanged := False;
  if Assigned(FOnUpdateConnection) then
    FOnUpdateConnection(EMSProvider1, AChanged);
end;

procedure TTasksClientModule.UpdateTask(const ATask: TTask; out AUpdatedAt: TTaskDateTime);
var
  LJSONObject: TJSONObject;
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointUpdateNote;
  LEndpoint.Params.Items[0].Value := ATask.ID;
  LJSONObject := TTaskJSON.TaskToJSON(ATask);
  try
    LEndpoint.ClearBody;
    LEndpoint.AddBody(LJSONObject);
    LEndpoint.Execute;
    with GetJSONObject(LEndpoint.Response) do
    begin
      AUpdatedAt := TTaskDateTime.CreateUTCString(GetValue<string>('updatedat'));
    end;
  finally
    LJSONObject.Free;
  end;
end;

procedure TTasksClientModule.UpdateMessage(const AMessage: TMessage; out AUpdatedAt: TTaskDateTime);
var
  LJSONObject: TJSONObject;
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointUpdateTaskMessage;
  LEndpoint.Params.Items[0].Value := AMessage.TaskID;
  LEndpoint.Params.Items[1].Value := AMessage.ID;
  LJSONObject := TMessageJSON.MessageToJSON(AMessage);
  try
    LEndpoint.ClearBody;
    LEndpoint.AddBody(LJSONObject);
    LEndpoint.Execute;
    with GetJSONObject(LEndpoint.Response) do
    begin
      AUpdatedAt := TTaskDateTime.CreateUTCString(GetValue<string>('updatedat'));
    end;
  finally
    LJSONObject.Free;
  end;
end;

function TTasksClientModule.DeleteMessage(const ATaskID, AMessageID: string): Boolean;
var
  LEndpoint: TBackendEndpoint;
begin
  UpdateConnection;
  LEndpoint := BackendEndpointDeleteTaskMessage;
  LEndpoint.Params.Items[0].Value := ATaskID;
  LEndpoint.Params.Items[1].Value := AMessageID;
  // Don't raise exception on 404
  LEndpoint.AllowHTTPErrors := TBackendEndpoint.TAllowHTTPErrors.ClientErrorNotFound_404;
  LEndpoint.Execute;
  Result := LEndpoint.Response.Status.Success;
end;


end.
