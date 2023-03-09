//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit TasksResourceU;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes, TasksStorageU,
  TasksTypesU;

type
  [ResourceName('Tasks')]
  TTasksResource = class(TDataModule)
  private
    FTasksStorage: TTasksStorage;
    procedure CheckTasksStorage(const AContext: TEndpointContext);
    procedure HandleException;
    procedure CheckAuthorized(const AContext: TEndpointContext);
    procedure PushMessage(const AContext: TEndpointContext; const AAlert: string; const ATask: TTask;
      const AMessage: TMessage);
  public
    destructor Destroy; override;
  published
    // Tasks
    procedure GetTask(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [ResourceSuffix('{item}')]
    procedure GetTaskItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    // Add
    procedure PostTask(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    // Update
    [ResourceSuffix('{item}')]
    procedure PutTaskItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [ResourceSuffix('{item}')]
    procedure DeleteTaskItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    // Task Messages
    [ResourceSuffix('{item}/messages')]
    procedure GetTaskMessages(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    // Add
    [ResourceSuffix('{item}/messages')]
    procedure PostTaskMessages(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    [ResourceSuffix('{task}/messages/{message}')]
    procedure DeleteTaskMessage(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

    // Update
    [ResourceSuffix('{task}/messages/{message}')]
    procedure PutTaskMessage(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);

  end;

procedure Register;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses System.StrUtils;

destructor TTasksResource.Destroy;
begin
  FTasksStorage.Free;
  inherited;
end;

// Call this from within an exception block
procedure TTasksResource.HandleException;
var
  LException: TObject;
  LMessage: string;
begin
  LException := ExceptObject;
  Assert(LException <> nil); // should be within an except block
  if LException is Exception then
  begin
    LMessage := Exception(LException).Message;
    if LException is ETaskDuplicate then
      EEMSHTTPError.RaiseDuplicate(LMessage)
    else if LException is ETaskNotFound then
      EEMSHTTPError.RaiseNotFound(LMessage)
    else if LException is ETaskMissingTitle then
      EEMSHTTPError.RaiseBadRequest(LMessage)
    else
    begin
      LException := TObject(AcquireExceptionObject);
      Assert(LException <> nil);  // should be within an except block
      raise LException;
    end;
  end;
end;

procedure TTasksResource.GetTask(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LTasks: TArray<TTask>;
  LJSON: TJSONArray;
  LTitle: string;
  LTask: TTask;
begin
  LJSON := nil;
  try
    CheckTasksStorage(AContext);
    if ARequest.Params.TryGetValue('title', LTitle) then
    begin
      // Find a tast with a particular title
      if FTasksStorage.FindTask(LTitle, LTask) then
        LTasks := TArray<TTask>.Create(LTask)
      else
        LTasks := nil;
    end
    else
      LTasks := FTasksStorage.GetTasks;
    LJSON := TJSONArray.Create;
    TTaskJSON.TasksToJSON(LTasks, LJSON);
    AResponse.Body.SetValue(LJSON, True)  // AResponse owns LJSONArray and will free it
  except
    LJSON.Free;
    HandleException;
  end;
end;

procedure TTasksResource.GetTaskMessages(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
  LMessages: TArray<TMessage>;
  LJSON: TJSONArray;
begin
  try
    LItem := ARequest.Params.Values['item'];
    CheckTasksStorage(AContext);
    if FTasksStorage.GetTaskMessages(LItem, LMessages) then
    begin
      LJSON := TJSONArray.Create;
      TMessageJSON.MessagesToJSON(LMessages, LJSON);
      AResponse.Body.SetValue(LJSON, True);   // AResponse owns LJSONObject and will free it
    end
    else
      AResponse.RaiseNotFound(Format('"%s" not found',[LItem]));
  except
    HandleException;
  end;
end;

procedure TTasksResource.GetTaskItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
  LTask: TTask;
  LJSON: TJSONObject;
begin
  try
    LItem := ARequest.Params.Values['item'];
    CheckTasksStorage(AContext);
    if FTasksStorage.GetTask(LItem, LTask) then
    begin
      LJSON := TTaskJSON.TaskToJSON(LTask);
      AResponse.Body.SetValue(LJSON, True);   // AResponse owns LJSONObject and will free it
    end
    else
      AResponse.RaiseNotFound(Format('"%s" not found',[LItem]));
  except
    HandleException;
  end;
end;

// New task
procedure TTasksResource.PostTask(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LJSON: TJSONObject;
  LTask: TTask;
  LID: string;
  LMessageID: string;
  LMessage: TMessage;
begin
  CheckAuthorized(AContext);
  try
    if ARequest.Body.TryGetObject(LJSON) then
    begin
      CheckTasksStorage(AContext);
      LTask := TTaskJSON.JSONToTask(LJSON, AContext.User.UserName, TTask.TStatus.New, ''); // TODO: Dates
      if LTask.AssignedTo = '' then
        AResponse.RaiseBadRequest('', 'AssignedTo value is blank');
      FTasksStorage.AddTask(LTask, LID);
      // Add "new" message
      LMessage := TMessage.Create(LID, TMessage.TKind.TaskCreated, '', AContext.User.UserName, '', '');  // TODO: Dates
      FTasksStorage.AddMessage(LID, LMessage, LMessageID);
      LMessage.ID := LMessageID;
      PushMessage(AContext, Format('Created: %s', [LTask.Title]), LTask, LMessage);

      LJSON := TJSONObject.Create;
      LJSON.AddPair(TTaskJSON.TNames.ID, LID);
      LJSON.AddPair(TTaskJSON.TNames.CreatedBy, AContext.User.UserName);
      LJSON.AddPair(TTaskJSON.TNames.CreatedAt, LTask.CreatedAt.AsUTCString);
      AResponse.Body.SetValue(LJSON, True);
    end
    else
      AResponse.RaiseBadRequest('JSON expected');
  except
    HandleException;
  end;
end;

procedure TTasksResource.PostTaskMessages(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LJSON: TJSONObject;
  LMessage: TMessage;
  LMessageID: string;
  LItem: string;
  LTask: TTask;
begin
  CheckAuthorized(AContext);
  try
    LItem := ARequest.Params.Values['item'];

    CheckTasksStorage(AContext);
    if FTasksStorage.GetTask(LItem, LTask) then
    begin
      if ARequest.Body.TryGetObject(LJSON) then
      begin
        LMessage := TMessageJSON.JSONToMessage(LJSON, AContext.User.UserName, ''); // TODO: Dates
        Assert(LItem = LMessage.TaskID); // Message should already contain task id
        case LMessage.Kind of
          TMessage.TKind.TaskCreated,
          TMessage.TKind.TaskUpdated:
            AResponse.RaiseBadRequest('Invalid message kind');
        end;

        FTasksStorage.AddMessage(LItem, LMessage, LMessageID);
        LMessage.ID := LMessageID;
        PushMessage(AContext, Format('Comment: %s', [LMessage.Comment]),  LTask, LMessage);

        LJSON := TJSONObject.Create;
        LJSON.AddPair(TMessageJSON.TNames.ID, LMessageID);
        LJSON.AddPair(TTaskJSON.TNames.CreatedBy, AContext.User.UserName);
        LJSON.AddPair(TTaskJSON.TNames.CreatedAt, LMessage.CreatedAt.AsUTCString);
        AResponse.Body.SetValue(LJSON, True);
      end
      else
        AResponse.RaiseBadRequest('JSON expected');
    end
    else
      AResponse.RaiseNotFound(Format('"%s" not found',[LItem]));

  except
    HandleException;
  end;
end;

procedure TTasksResource.PushMessage(const AContext: TEndpointContext; const AAlert: string; const ATask: TTask; const AMessage: TMessage);
var
  LEMSAPI: TEMSInternalAPI;
  LData: TJSONObject;
  LWhere: TJSONObject;
  LJSON: TJSONObject;
  LAlert: string;

  function MakeAlert(const AValue: string): string;
  var
    I: Integer;
  begin
    Result := ReplaceStr(AValue, #10, ' ');
    Result := ReplaceStr(Result, #13, ' ');
    if Result.Length > 30 then
      Result := Trim(Result.Substring(0, 30)) + '...';
    if Result = '' then
      Result := '(empty)';

  end;
begin
  // Create in-process EMS API
  LEMSAPI := TEMSInternalAPI.Create(AContext);
  LWhere := nil;
  LJSON := nil;
  try
    LAlert := MakeAlert(AAlert);
    LJSON := TJSONObject.Create;
    // Gcm
    LData := TJSONObject.Create;
    LJSON.AddPair('gcm', LData);
    LData.AddPair('message', LAlert);
    // IOS
    LData := TJSONObject.Create;
    LJSON.AddPair('aps', LData);
    LData.AddPair('alert', LAlert);

    // Extras
    LData := TJSONObject.Create;
    LJSON.AddPair('extras', LData);
    Assert(AMessage.TaskID <> '');
    LData.AddPair('taskid', AMessage.TaskID);
    Assert(AMessage.ID <> '');
    LData.AddPair('messageid', AMessage.ID);
    LData.AddPair('messagekind', TTasksUtil.KindToString(AMessage.Kind));

    // TODO: Send to particular users, based on user preferences
//    LWhere := TJSONObject.Create;
//    LWhere.AddPair('tasksusername', AAssignedTo);  // Target assignee
//    LEMSAPI.PushWhere(LData, LWhere);
    LEMSAPI.PushBroadcast(LJSON);
  finally
    LEMSAPI.Free;
    LWhere.Free;
    LJSON.Free;
  end;
end;

procedure TTasksResource.PutTaskItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
  LJSON: TJSONObject;
  LTask: TTask;
  LMessage: TMessage;
  LMessageID: string;
begin
  CheckAuthorized(AContext);
  try
    LItem := ARequest.Params.Values['item'];
    if ARequest.Body.TryGetObject(LJSON) then
    begin
      CheckTasksStorage(AContext);
      LTask := TTaskJSON.JSONToTask(LJSON);
      LTask.Update; // change UpdatedAt
      // Add message
      LMessage := TMessage.Create(LItem, TMessage.TKind.TaskUpdated, '', AContext.User.UserName, LTask.CreatedAt.AsUTCString, ''); // TODO: Dates
      FTasksStorage.AddMessage(LItem, LMessage, LMessageID);
      LMessage.ID := LMessageID;
      PushMessage(AContext, 'Task Updated', LTask, LMessage);

      // TODO: Only owner can
      FTasksStorage.UpdateTask(LItem, LTask);
      // JSON response
      LJSON := TJSONObject.Create;
      LJSON.AddPair(TTaskJSON.TNames.UpdatedAt, LTask.UpdatedAt.AsUTCString);
      AResponse.Body.SetValue(LJSON, True);
    end
    else
      AResponse.RaiseBadRequest('JSON expected');
  except
    HandleException;
  end;
end;

procedure TTasksResource.PutTaskMessage(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LTaskID: string;
  LMessageID: string;
  LJSON: TJSONObject;
  LMessage: TMessage;
begin
  CheckAuthorized(AContext);
  try
    LMessageID := ARequest.Params.Values['message'];
    LTaskID := ARequest.Params.Values['task'];
    if ARequest.Body.TryGetObject(LJSON) then
    begin
      CheckTasksStorage(AContext);
      LMessage := TMessageJSON.JSONToMessage(LJSON);
      Assert(LTaskID = LMessage.TaskID); // Message should contain task id
      case LMessage.Kind of
        TMessage.TKind.TaskCreated,
        TMessage.TKind.TaskUpdated:
          AResponse.RaiseBadRequest('Invalid message kind');
      end;
      // TODO: Only owner can
      LMessage.Update; // change UpdatedAt
      FTasksStorage.UpdateMessage(LTaskID, LMessageID, LMessage);
      // JSON response
      LJSON := TJSONObject.Create;
      LJSON.AddPair(TTaskJSON.TNames.UpdatedAt, LMessage.UpdatedAt.AsUTCString);
      AResponse.Body.SetValue(LJSON, True);
    end
    else
      AResponse.RaiseBadRequest('JSON expected');
  except
    HandleException;
  end;
end;

function GetModuleDirectory: string;
begin
  Result := ExtractFilePath(StringReplace(GetModuleName(HInstance),'\\?\','',[rfReplaceAll]));
end;

procedure TTasksResource.CheckTasksStorage(const AContext: TEndpointContext);
begin
  if FTasksStorage = nil then
    FTasksStorage := TTasksStorage.Create(GetModuleDirectory);
end;

procedure TTasksResource.CheckAuthorized(const AContext: TEndpointContext);
begin
  if AContext.User = nil then
    AContext.Response.RaiseUnauthorized('The operation is only permitted for logged in users');
end;

procedure TTasksResource.DeleteTaskItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  CheckAuthorized(AContext);
  try
    LItem := ARequest.Params.Values['item'];
    CheckTasksStorage(AContext);
    // TODO: Only owner can delete message
    FTasksStorage.DeleteTask(LItem);
  except
    HandleException;
  end;
end;

procedure TTasksResource.DeleteTaskMessage(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LTaskID: string;
  LMessageID: string;
begin
  CheckAuthorized(AContext);
  try
    LMessageID := ARequest.Params.Values['message'];
    LTaskID := ARequest.Params.Values['task'];
    CheckTasksStorage(AContext);
    // TODO: Only owner can delete message
    FTasksStorage.DeleteMessage(LTaskID, LMessageID);
  except
    HandleException;
  end;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TTasksResource));
end;

end.


