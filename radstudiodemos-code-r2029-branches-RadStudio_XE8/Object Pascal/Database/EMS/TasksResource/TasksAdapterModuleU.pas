//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit TasksAdapterModuleU;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes, Data.Bind.ObjectScope,
  TasksTypesU;

type
  // Expose data binding properties
  TTaskWrapper = class
  private
    FTask: TTask;
    function GetContent: string;
    function GetID: string;
    function GetTitle: string;
    procedure SetContent(const Value: string);
    procedure SetTitle(const Value: string);
    function GetCreatedBy: string;
    function GetAssignedTo: string;
    function GetStatus: string;
    function GetStatusImageIndex: Integer; overload;
    class function GetStatusImageIndex(AStatus: TTask.TStatus): Integer; overload; static;
    procedure SetAssignedTo(const Value: string);
    procedure SetStatus(const Value: string);
    function GetCreatedAt: string;
    function GetUpdatedAt: string;
  public
    constructor Create(const ANote: TTask);
    function GetData: TTask;
    procedure SetID(const AID: string);
    procedure SetCreatedBy(const ACreatedBy: string);
    procedure SetCreatedAt(const ACreatedAt: TTaskDatetime);
    procedure SetUpdatedAt(const AUpdatedAt: TTaskDatetime);
    property Title: string read GetTitle write SetTitle;
    property Content: string read GetContent write SetContent;
    property CreatedBy: string read GetCreatedBy;
    property AssignedTo: string read GetAssignedTo write SetAssignedTo;
    property Status: string read GetStatus write SetStatus;
    property StatusImageIndex: Integer read GetStatusImageIndex;
    property ID: string read GetID;
    property CreatedAt: string read GetCreatedAt;
    property UpdatedAt: string read GetUpdatedAt;
  end;

  TMessageWrapper = class
  private
    FData: TMessage;
    function GetComment: string;
    function GetID: string;
    procedure SetComment(const Value: string);
    function GetCreatedBy: string;
    function GetTaskID: string;
    function GetCreatedAt: string;
    function GetUpdatedAt: string;
    function GetKind: string;
    function GetKindImageIndex: Integer; overload;
    function GetTitle: string;
    function GetTaskTitle: string;
  public
    constructor Create(const AData: TMessage);
    function GetData: TMessage;
    class function GetKindImageIndex(AKind: TMessage.TKind): Integer; overload; static;
    procedure SetID(const AID: string);
    procedure SetTaskID(const ATaskID: string);
    procedure SetCreatedBy(const ACreatedBy: string);
    procedure SetCreatedAt(const ACreatedAt: TTaskDatetime);
    procedure SetUpdatedAt(const AUpdatedAt: TTaskDatetime);
    property Comment: string read GetComment write SetComment;
    property ID: string read GetID;
    property TaskID: string read GetTaskID;
    property CreatedBy: string read GetCreatedBy;
    property CreatedAt: string read GetCreatedAt;
    property UpdatedAt: string read GetUpdatedAt;
    property Kind: string read GetKind;
    property KindImageIndex: Integer read GetKindImageIndex;
    property Title: string read GetTitle;
    property TaskTitle: string read GetTaskTitle;
  end;

  TTasksAdapterModule = class(TDataModule)
  private
    FTasksBindSourceAdapter: TListBindSourceAdapter<TTaskWrapper>;
    FTaskMessagesBindSourceAdapter: TListBindSourceAdapter<TMessageWrapper>;
    procedure TasksAfterPost(Sender: TBindSourceAdapter);
    procedure TasksBeforeDelete(Sender: TBindSourceAdapter);
    procedure TaskMessagesAfterPost(Sender: TBindSourceAdapter);
    procedure TaskMessagesBeforeDelete(Sender: TBindSourceAdapter);
    function GetCurrentTaskID: string;
    function GetCurrentTaskAssignedTo: string;
    function GetCurrentTaskCreatedBy: string;
    function GetCurrentTaskTitle: string;
    function GetCurrentMessageKind: TMessage.TKind;
    function GetCurrentMessageKindImageIndex: Integer;
  public
    function GetTasksBindSourceAdapter: TBindSourceAdapter;
    function GetTaskMessagesBindSourceAdapter: TBindSourceAdapter;
    procedure UpdateTasksAdapter(const ANotes: TArray<TTask>);
    procedure UpdateTaskMessagesAdapter(const AMessages: TArray<TMessage>);
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshTasksAdapter; overload;
    procedure RefreshTaskMessagesAdapter; overload;
    property TasksBindSourceAdapter: TBindSourceAdapter read GetTasksBindSourceAdapter;
    property TaskMessagesBindSourceAdapter: TBindSourceAdapter read GetTaskMessagesBindSourceAdapter;
    property CurrentTaskID: string read GetCurrentTaskID;
    property CurrentTaskCreatedBy: string read GetCurrentTaskCreatedBy;
    property CurrentTaskAssignedTo: string read GetCurrentTaskAssignedTo;
    property CurrentTaskTitle: string read GetCurrentTaskTitle;
    property CurrentMessageKind: TMessage.TKind read GetCurrentMessageKind;
    property CurrentTaskStatusImageIndex: Integer read GetCurrentMessageKindImageIndex;
  end;

var
  TasksAdapterModule: TTasksAdapterModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

uses TasksClientModuleU, System.DateUtils, System.StrUtils;

{$R *.dfm}

procedure TTasksAdapterModule.RefreshTasksAdapter;
var
  LNotes: TArray<TTask>;
begin
  LNotes := TasksClientModule.GetTasks;
  UpdateTasksAdapter(LNotes);
end;

procedure TTasksAdapterModule.RefreshTaskMessagesAdapter;
var
  LMessages: TArray<TMessage>;
begin
  LMessages := TasksClientModule.GetTaskMessages(CurrentTaskID);
  UpdateTaskMessagesAdapter(LMessages);
end;

procedure TTasksAdapterModule.UpdateTasksAdapter(const ANotes: TArray<TTask>);
var
  LList: TList<TTaskWrapper>;
  LNote: TTask;
begin
  if csDestroying in ComponentState then
    Exit;
  LList := TObjectList<TTaskWrapper>.Create;
  try
    for LNote in ANotes do
      LList.Add(TTaskWrapper.Create(LNote));
    if FTasksBindSourceAdapter = nil then
      FTasksBindSourceAdapter := TListBindSourceAdapter<TTaskWrapper>.Create(Self, LList, True);
    FTasksBindSourceAdapter.SetList(LList, True);
    FTasksBindSourceAdapter.Active := True;
  except
    LList.Free;
    raise;
  end;
end;

procedure TTasksAdapterModule.UpdateTaskMessagesAdapter(const AMessages: TArray<TMessage>);
var
  LList: TList<TMessageWrapper>;
  LMessage: TMessage;
begin
  if csDestroying in ComponentState then
    Exit;
  LList := TObjectList<TMessageWrapper>.Create;
  try
    for LMessage in AMessages do
      LList.Add(TMessageWrapper.Create(LMessage));
    if FTaskMessagesBindSourceAdapter = nil then
      FTaskMessagesBindSourceAdapter := TListBindSourceAdapter<TMessageWrapper>.Create(Self, LList, True);
    FTaskMessagesBindSourceAdapter.SetList(LList, True);
    FTaskMessagesBindSourceAdapter.Active := True;
  except
    LList.Free;
    raise;
  end;
end;

function TTasksAdapterModule.GetCurrentTaskID: string;
begin
  if FTasksBindSourceAdapter <> nil then
     // Create empty adapter
     Result := FTasksBindSourceAdapter.FindField('ID').GetTValue.AsString;
end;

function TTasksAdapterModule.GetCurrentTaskTitle: string;
begin
  if FTasksBindSourceAdapter <> nil then
     // Create empty adapter
     Result := FTasksBindSourceAdapter.FindField('Title').GetTValue.AsString;
end;

function TTasksAdapterModule.GetCurrentTaskCreatedBy: string;
begin
  if FTasksBindSourceAdapter <> nil then
     // Create empty adapter
     Result := FTasksBindSourceAdapter.FindField('CreatedBy').GetTValue.AsString;
end;

function TTasksAdapterModule.GetCurrentMessageKind: TMessage.TKind;
begin
  if (FTaskMessagesBindSourceAdapter <> nil) and
     (FTaskMessagesBindSourceAdapter.ItemIndex >= 0) and
     (FTaskMessagesBindSourceAdapter.List.Count > FTaskMessagesBindSourceAdapter.ItemIndex) then
    // This will not be correct if user is editing task.
    // If we want the correct value while using is editing then should get it from the field.
    Result := FTaskMessagesBindSourceAdapter.List[FTaskMessagesBindSourceAdapter.ItemIndex].FData.Kind
  else
    Result := Low(TMessage.TKind);
end;

function TTasksAdapterModule.GetCurrentMessageKindImageIndex: Integer;
var
  LStatus: TTask.TStatus;
begin
  if FTasksBindSourceAdapter <> nil then
  begin
    // Calculate the image index using Status, in case the user is editing Status
    LStatus := TTasksUtil.StringToStatus(FTasksBindSourceAdapter.FindField('Status').GetTValue.AsString);
    Result := TTaskWrapper.GetStatusImageIndex(LStatus);
  end
  else
    Result := -1;
end;

function TTasksAdapterModule.GetCurrentTaskAssignedTo: string;
begin
  if FTasksBindSourceAdapter <> nil then
     // Create empty adapter
     Result := FTasksBindSourceAdapter.FindField('AssignedTo').GetTValue.AsString;
end;

function TTasksAdapterModule.GetTasksBindSourceAdapter: TBindSourceAdapter;
begin
  if FTasksBindSourceAdapter = nil then
  begin
     // Create empty adapter
     FTasksBindSourceAdapter := TListBindSourceAdapter<TTaskWrapper>.Create(Self, TList<TTaskWrapper>.Create, True);
     FTasksBindSourceAdapter.AfterPost := TasksAfterPost;
     FTasksBindSourceAdapter.BeforeDelete := TasksBeforeDelete;
  end;
  Result := FTasksBindSourceAdapter;
end;

function TTasksAdapterModule.GetTaskMessagesBindSourceAdapter: TBindSourceAdapter;
begin
  if FTaskMessagesBindSourceAdapter = nil then
  begin
     // Create empty adapter
     FTaskMessagesBindSourceAdapter := TListBindSourceAdapter<TMessageWrapper>.Create(Self, TList<TMessageWrapper>.Create, True);
     FTaskMessagesBindSourceAdapter.AfterPost := TaskMessagesAfterPost;
     FTaskMessagesBindSourceAdapter.BeforeDelete := TaskMessagesBeforeDelete;
  end;
  Result := FTaskMessagesBindSourceAdapter;
end;

procedure TTasksAdapterModule.TasksBeforeDelete(Sender: TBindSourceAdapter);
var
  LWrapper: TTaskWrapper;
begin
  LWrapper := FTasksBindSourceAdapter.List[FTasksBindSourceAdapter.ItemIndex];
  if LWrapper.ID <> '' then
    TasksClientModule.DeleteTask(LWrapper.ID);
end;

procedure TTasksAdapterModule.TasksAfterPost(Sender: TBindSourceAdapter);
var
  LWrapper: TTaskWrapper;
  LID: string;
  LCreatedBy: string;
  LTaskDateTime: TTaskDateTime;
begin
  try
    LWrapper := FTasksBindSourceAdapter.List[FTasksBindSourceAdapter.ItemIndex];
    if LWrapper.ID = '' then
    begin
      TasksClientModule.AddTask(LWrapper.GetData, LCreatedBy, LTaskDateTime, LID);
      LWrapper.SetID(LID);
      LWrapper.SetCreatedBy(LCreatedBy);
      LWrapper.SetCreatedAt(LTaskDateTime);
      FTasksBindSourceAdapter.Refresh;
    end
    else
    begin
      TasksClientModule.UpdateTask(LWrapper.GetData, LTaskDateTime);
      LWrapper.SetUpdatedAt(LTaskDateTime);
      FTasksBindSourceAdapter.Refresh;
    end;
  except
    FTasksBindSourceAdapter.Edit;  // Return to edit mode
    raise;
  end;
end;

procedure TTasksAdapterModule.TaskMessagesBeforeDelete(Sender: TBindSourceAdapter);
var
  LWrapper: TMessageWrapper;
begin
  LWrapper := FTaskMessagesBindSourceAdapter.List[FTaskMessagesBindSourceAdapter.ItemIndex];
  if LWrapper.ID <> '' then
    TasksClientModule.DeleteMessage(LWrapper.TaskID, LWrapper.ID);
end;

procedure TTasksAdapterModule.TaskMessagesAfterPost(Sender: TBindSourceAdapter);
var
  LWrapper: TMessageWrapper;
  LID: string;
  LCreatedBy: string;
  LTaskDateTime: TTaskDateTime;
begin
  try
    LWrapper := FTaskMessagesBindSourceAdapter.List[FTaskMessagesBindSourceAdapter.ItemIndex];
    if LWrapper.ID = '' then
    begin
      LWrapper.SetTaskID(CurrentTaskID);
      TasksClientModule.AddMessage(LWrapper.GetData, LCreatedBy, LTaskDateTime, LID);
      LWrapper.SetID(LID);
      LWrapper.SetCreatedBy(LCreatedBy);
      LWrapper.SetCreatedAt(LTaskDateTime);
      FTaskMessagesBindSourceAdapter.Refresh;
    end
    else
    begin
      TasksClientModule.UpdateMessage(LWrapper.GetData, LTaskDateTime);
      LWrapper.SetUpdatedAt(LTaskDateTime);
      FTaskMessagesBindSourceAdapter.Refresh;
    end;
  except
    FTaskMessagesBindSourceAdapter.Edit;  // Return to edit mode
    raise;
  end;
end;

function FormatTaskDateTime(const AValue: TTaskDateTime): string;
begin
  // TODO: Provide readable time
  // If today, just show time
  // If this week, just show day of week and time
  // If this year, just show day and time
  // Otherwise, show entire date
  Result := AValue.AsLocalString;
end;

{ TTaskWrapper }

constructor TTaskWrapper.Create(const ANote: TTask);
begin
  FTask := ANote;
end;

function TTaskWrapper.GetContent: string;
begin
  Result := FTask.Content;
end;

function TTaskWrapper.GetID: string;
begin
  Result := FTask.ID;
end;

function TTaskWrapper.GetData: TTask;
begin
  Result := FTask;
end;

function TTaskWrapper.GetTitle: string;
begin
  Result := FTask.Title;

end;

function TTaskWrapper.GetCreatedAt: string;
begin
  Result := FormatTaskDateTime(FTask.CreatedAt);
end;

function TTaskWrapper.GetUpdatedAt: string;
begin
  Result := FormatTaskDateTime(FTask.UpdatedAt);
end;

function TTaskWrapper.GetCreatedBy: string;
begin
  Result := FTask.CreatedBy;
end;

function TTaskWrapper.GetAssignedTo: string;
begin
  Result := FTask.AssignedTo;
end;

function TTaskWrapper.GetStatus: string;
begin
  Result := TTasksUtil.StatusToString(FTask.Status)
end;

function TTaskWrapper.GetStatusImageIndex: Integer;
begin
  Result := TTaskWrapper.GetStatusImageIndex(FTask.Status);
end;

class function TTaskWrapper.GetStatusImageIndex(AStatus: TTask.TStatus): Integer;
begin
  // TODO
  case AStatus of
    TTask.TStatus.New:
      Result := 2;
    TTask.TStatus.InProgress:
      Result := 1;
    TTask.TStatus.Done:
      Result := 0;
    TTask.TStatus.WontDo:
      Result := 3;
  else
    Result := -1;
    //Assert(False);
  end;
end;

procedure TTaskWrapper.SetAssignedTo(const Value: string);
begin
  FTask.AssignedTo := Value;
end;

procedure TTaskWrapper.SetContent(const Value: string);
begin
  FTask.Content := Value;
end;

procedure TTaskWrapper.SetID(const AID: string);
begin
  FTask.ID := AID;
end;

procedure TTaskWrapper.SetStatus(const Value: string);
begin
  FTask.Status := TTasksUtil.StringToStatus(Value);
end;

procedure TTaskWrapper.SetCreatedAt(const ACreatedAt: TTaskDatetime);
begin
  FTask.CreatedAt := ACreatedAt;
  FTask.UpdatedAt := ACreatedAt; // Synch
end;

procedure TTaskWrapper.SetCreatedBy(const ACreatedBy: string);
begin
  FTask.CreatedBy := ACreatedBy;
end;

procedure TTaskWrapper.SetTitle(const Value: string);
begin
  FTask.Title := Value;
end;

procedure TTaskWrapper.SetUpdatedAt(const AUpdatedAt: TTaskDatetime);
begin
  FTask.UpdatedAt := AUpdatedAt;
end;

{ TMessageWrapper }

constructor TMessageWrapper.Create(const AData: TMessage);
begin
  FData := AData;
end;

function TMessageWrapper.GetID: string;
begin
  Result := FData.ID;
end;

function TMessageWrapper.GetKind: string;
begin
  case FData.Kind of
    TMessage.TKind.TaskCreated:
      Result := 'Create';
    TMessage.TKind.TaskUpdated:
      Result := 'Update';
    TMessage.TKind.Comment:
      Result := 'Comment';
  else
    Assert(False);
  end;
end;

function TMessageWrapper.GetKindImageIndex: Integer;
begin
  Result := GetKindImageIndex(FData.Kind);
end;

class function TMessageWrapper.GetKindImageIndex(AKind: TMessage.TKind): Integer;
begin
  case AKind of
    TMessage.TKind.TaskCreated:
      Result := 4;
    TMessage.TKind.TaskUpdated:
      Result := 5;
    TMessage.TKind.Comment:
      Result := 6;
  else
    Result := -1;
  end;
end;


function TMessageWrapper.GetComment: string;
begin
  Result := FData.Comment;
end;

function TMessageWrapper.GetTaskID: string;
begin
  Result := FData.TaskID;
end;

function TMessageWrapper.GetTaskTitle: string;
begin
  Result := TasksAdapterModule.GetCurrentTaskTitle;
end;

function TMessageWrapper.GetTitle: string;
  function SingleLine(const AValue: string): string;
  begin
    Result := ReplaceStr(FData.Comment, #13, ' ');
    Result := ReplaceStr(Result, #10, ' ');
  end;
begin
  case FData.Kind of
    TMessage.TKind.Comment:
      Result := 'Comment: ' +  SingleLine(FData.Comment);
    TMessage.TKind.TaskCreated:
      Result := 'Created';
    TMessage.TKind.TaskUpdated:
      Result := 'Updated';
  else
    Assert(False);
  end;
end;

function TMessageWrapper.GetUpdatedAt: string;
begin
  Result := FormatTaskDateTime(FData.UpdatedAt);
end;

function TMessageWrapper.GetData: TMessage;
begin
  Result := FData;
end;

procedure TMessageWrapper.SetCreatedBy(const ACreatedBy: string);
begin
  FData.CreatedBy := ACreatedBy;
end;

procedure TMessageWrapper.SetCreatedAt(const ACreatedAt: TTaskDatetime);
begin
  FData.CreatedAt := ACreatedAt;
  FData.UpdatedAt := ACreatedAt;   // Synch
end;

procedure TMessageWrapper.SetUpdatedAt(const AUpdatedAt: TTaskDatetime);
begin
  FData.UpdatedAt := AUpdatedAt;
end;

function TMessageWrapper.GetCreatedAt: string;
begin
  Result := FormatTaskDateTime(FData.CreatedAt);
end;

function TMessageWrapper.GetCreatedBy: string;
begin
  Result := FData.CreatedBy;
end;

procedure TMessageWrapper.SetID(const AID: string);
begin
  FData.ID := AID;
end;

procedure TMessageWrapper.SetComment(const Value: string);
begin
  FData.Comment := Value;
end;

procedure TMessageWrapper.SetTaskID(const ATaskID: string);
begin
  Assert(FData.TaskID = '');  // Shouldn't be changing id
  FData.TaskID := ATaskID;
end;

end.
