//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit TasksTypesU;

interface

{$SCOPEDENUMS ON}

uses System.JSON;

type

  TTaskDateTime = record
  private
    FUTCTime: TDateTime;
    function GetLocalDateTime: TDateTime;
    function GetLocalString: string;
    function GetUTCDateTime: TDateTime;
    function GetUTCString: string;
  public
    constructor CreateUTCString(const AUTCString: string);
    property AsUTCString: string read GetUTCString;
    property AsLocalString: string read GetLocalString;
    property AsUTCDateTime: TDateTime read GetUTCDateTime;
    property AsLocalDateTime: TDateTime read GetLocalDateTime;
  end;

  TTask = record
  public type
    TStatus = (New, InProgress, Done, WontDo);
  private
    FTitle: string;
    FContent: string;
    FID: string;
    FCreatedBy: string;
    FStatus: TStatus;
    FAssignedTo: string;
    FUpdatedAt: TTaskDateTime;
    FCreatedAt: TTaskDateTime;
  public
    constructor Create(const ATitle, AContent, ACreatedBy, AAssignedTo: string; AStatus: TStatus; const ACreatedAt, AUpdatedAt: string);
    constructor CreateWithID(const ATitle, AContent, ACreatedBy, AAssignedTo: string; AStatus: TStatus; const ACreatedAt, AUpdatedAt: string;
      const AID: string);
    procedure Update;
    property Title: string read FTitle write FTitle;
    property Content: string read FContent write FContent;
    property ID: string read FID write FID;
    property Status: TStatus read FStatus write FStatus;
    property CreatedBy: string read FCreatedBy write FCreatedBy;
    property AssignedTo: string read FAssignedTo write FAssignedTo;
    property CreatedAt: TTaskDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TTaskDateTime read FUpdatedAt write FUpdatedAt;
  end;

  TTaskJSON = class
  public type
    TNames = record
    public const
      Title = 'title';
      Content = 'content';
      Id = 'id';
      CreatedBy = 'createdby';
      AssignedTo = 'assignedto';
      Status = 'status';
      CreatedAt = 'createdat';
      UpdatedAt = 'updatedat';
    end;
    TStatusValues = record
    public const
      New = 'new';
      InProgress = 'inprogress';
      Done = 'done';
      WontDo = 'wontdo';
    end;
  public
    class function JSONToTask(const AJSON: TJSONValue): TTask; overload; static;
    class function JSONToTask(const AJSON: TJSONValue; const ACreatedBy: string; AStatus: TTask.TStatus; const ACreatedAt: string): TTask; overload; static;
    class function JSONToTasks(const AJSON: TJSONArray): TArray<TTask>; static;
    class procedure TasksToJSON(const TTasks: TArray<TTask>; const AJSON: TJSONArray); static;
    class function TaskToJSON(const ATask: TTask): TJSONObject; static;
  end;

  // Associated with a task
  TMessage = record
  public type
    TKind = (Comment, TaskCreated, TaskUpdated);
  private
    FComment: string;
    FID: string;
    FKind: TKind;
    FTaskID: string;
    FCreatedBy: string;
    FUpdatedAt: TTaskDateTime;
    FCreatedAt: TTaskDateTime;
  public
    constructor Create(const ATaskID: string; AKind: TKind; const AMessage: string; const ACreatedBy: string; const ACreatedAt, AUpdatedAt: string);
    constructor CreateWithID(const ATaskID: string; AKind: TKind; const AMessage: string; const ACreatedBy: string; const ACreatedAt, AUpdatedAt: string; const AID: string);
    procedure Update;
    property Comment: string read FComment write FComment;
    property Kind: TKind read FKind write FKind;
    property ID: string read FID write FID;
    property TaskID: string read FTaskID write FTaskID;
    property CreatedBy: string read FCreatedBy write FCreatedBy;
    property CreatedAt: TTaskDateTime read FCreatedAt write FCreatedAt;
    property UpdatedAt: TTaskDateTime read FUpdatedAt write FUpdatedAt;
  end;

  TMessageJSON = class
  public type
    TNames = record
    public const
      Comment = 'comment';
      TaskId = 'taskid';
      Id = 'id';
      CreatedBy = 'createdby';
      CreatedAt = 'createdat';
      UpdatedAt = 'updatedat';
      Kind = 'kind';
    end;
  public
    class function JSONToMessage(const AJSON: TJSONValue): TMessage; overload; static;
    class function JSONToMessage(const AJSON: TJSONValue; const ACreatedBy: string; const ACreatedAt: string): TMessage; overload; static;
    class function JSONToMessages(const AJSON: TJSONArray): TArray<TMessage>; static;
    class procedure MessagesToJSON(const AMessages: TArray<TMessage>; const AJSON: TJSONArray); static;
    class function MessageToJSON(const AMessage: TMessage): TJSONObject; static;
  end;

  TTasksUtil = class
  public
    class function StringToStatus(const AValue: string): TTask.TStatus; static;
    class function StatusToString(AValue: TTask.TStatus): string; static;
    class function StringToKind(const AValue: string): TMessage.TKind; static;
    class function KindToString(AValue: TMessage.TKind): string; static;
  end;

implementation

uses System.Generics.Collections, System.TypInfo, System.SysUtils, System.DateUtils;

{ TNote }

constructor TTask.Create(const ATitle, AContent, ACreatedBy, AAssignedTo: string; AStatus: TStatus;
  const ACreatedAt, AUpdatedAt: string);
begin
  FContent := AContent;
  FTitle := ATitle;
  FStatus := AStatus;
  FCreatedBy := ACreatedBy;
  FAssignedTo := AAssignedTo;
  FCreatedAt := TTaskDateTime.CreateUTCString(ACreatedAt);
  if AUpdatedAt = '' then
    FUpdatedAt := FCreatedAt
  else
    FUpdatedAt := TTaskDateTime.CreateUTCString(AUpdatedAt)
end;

class function TTaskJSON.JSONToTask(const AJSON: TJSONValue; const ACreatedBy: string; AStatus: TTask.TStatus;
  const ACreatedAt: string): TTask;
begin
  Result := TTask.CreateWithID(
    AJSON.GetValue<string>(TNames.Title, ''),
    AJSON.GetValue<string>(TNames.Content, ''),
    ACreatedBy,
    AJSON.GetValue<string>(TNames.AssignedTo, ''),
    AStatus,
    ACreatedAt,
    ACreatedAt,  // UpdatedAt
    AJSON.GetValue<string>(TNames.ID, ''));
end;

class function TTaskJSON.JSONToTask(const AJSON: TJSONValue): TTask;
var
  LStatus: string;
begin
  LStatus := AJSON.GetValue<string>(TNames.Status, '');
  Result := TTask.CreateWithID(
    AJSON.GetValue<string>(TNames.Title, ''),
    AJSON.GetValue<string>(TNames.Content, ''),
    AJSON.GetValue<string>(TNames.CreatedBy, ''),
    AJSON.GetValue<string>(TNames.AssignedTo, ''),
    TTasksUtil.StringToStatus(LStatus),
    AJSON.GetValue<string>(TNames.CreatedAt, ''),
    AJSON.GetValue<string>(TNames.UpdatedAt, ''),
    AJSON.GetValue<string>(TNames.ID, ''));
end;

class function TTaskJSON.TaskToJSON(const ATask: TTask): TJSONObject; // Caller must free result
begin
  Result := TJSONObject.Create;
  Result.AddPair(TNames.Title, ATask.Title);
  Result.AddPair(TNames.Content, ATask.Content);
  Result.AddPair(TNames.CreatedBy, ATask.CreatedBy);
  Result.AddPair(TNames.AssignedTo, ATask.AssignedTo);
  Result.AddPair(TNames.Status, TTasksUtil.StatusToString(ATask.Status));
  Result.AddPair(TNames.CreatedAt, ATask.CreatedAt.AsUTCString);
  Result.AddPair(TNames.UpdatedAt, ATask.UpdatedAt.AsUTCString);
  Result.AddPair(TNames.ID, ATask.ID);
end;

class function TTaskJSON.JSONToTasks(const AJSON: TJSONArray): TArray<TTask>;
var
  LValue: TJSONValue;
  LList: TList<TTask>;
begin
  LList := TList<TTask>.Create;
  try
    for LValue in AJSON do
      LList.Add(TTaskJSON.JSONToTask(LValue));
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

class procedure TTaskJSON.TasksToJSON(const TTasks: TArray<TTask>;
  const AJSON: TJSONArray);
var
  LNote: TTask;
  LJSON: TJSONObject;
begin
  for LNote in TTasks do
  begin
    LJSON := TaskToJSON(LNote);
    AJSON.Add(LJSON);
  end;
end;

constructor TTask.CreateWithID(const ATitle, AContent, ACreatedBy, AAssignedTo: string; AStatus: TStatus;
  const ACreatedAt, AUpdatedAt: string; const AID: string);
begin
  Create(ATitle, AContent, ACreatedBy, AAssignedTo, AStatus, ACreatedAt, AUpdatedAt);
  FID := AID;
end;

procedure TTask.Update;
begin
  FUpdatedAt := TTaskDateTime.CreateUTCString('');
end;

{ TTasksUtil }

class function TTasksUtil.StringToStatus(const AValue: string): TTask.TStatus;
var
  LValue: Integer;
begin
  LValue := System.TypInfo.GetEnumValue(TypeInfo(TTask.TStatus), AValue);
  if LValue < Integer(Low(TTask.TStatus)) then
    Result := Low(TTask.TStatus)
  else
    Result := TTask.TStatus(LValue);
end;

class function TTasksUtil.StatusToString(AValue: TTask.TStatus): string;
begin
  Result := System.TypInfo.GetEnumName(TypeInfo(TTask.TStatus), Integer(AValue));
end;

class function TTasksUtil.StringToKind(const AValue: string): TMessage.TKind;
var
  LValue: Integer;
begin
  LValue := System.TypInfo.GetEnumValue(TypeInfo(TMessage.TKind), AValue);
  if LValue < Integer(Low(TMessage.TKind)) then
    Result := TMessage.TKind.Comment
  else
    Result := TMessage.TKind(LValue);
end;

class function TTasksUtil.KindToString(AValue: TMessage.TKind): string;
begin
  Result := System.TypInfo.GetEnumName(TypeInfo(TMessage.TKind), Integer(AValue));
end;


{ TMessage }

constructor TMessage.Create(const ATaskID: string; AKind: TKind; const AMessage, ACreatedBy: string; const ACreatedAt, AUpdatedAt: string);
begin
  FTaskID := ATaskID;
  FKind := AKind;
  FComment := AMessage;
  FCreatedBy := ACreatedBy;
  FCreatedAt := TTaskDateTime.CreateUTCString(ACreatedAt);
  if AUpdatedAt = '' then
    FUpdatedAt := FCreatedAt
  else
    FUpdatedAt := TTaskDateTime.CreateUTCString(AUpdatedAt)
end;

class function TMessageJSON.JSONToMessage(const AJSON: TJSONValue): TMessage;
begin
  Result := TMessage.CreateWithID(
    AJSON.GetValue<string>(TNames.TaskID, ''),
    TTasksUtil.StringToKind(AJSON.GetValue<string>(TNames.Kind, '')),
    AJSON.GetValue<string>(TNames.Comment, ''),
    AJSON.GetValue<string>(TNames.CreatedBy, ''),
    AJSON.GetValue<string>(TNames.CreatedAt, ''),
    AJSON.GetValue<string>(TNames.UpdatedAt, ''),
    AJSON.GetValue<string>(TNames.ID, ''));
end;

class function TMessageJSON.JSONToMessage(const AJSON: TJSONValue; const ACreatedBy: string; const ACreatedAt: string): TMessage;
begin
  Result := TMessage.CreateWithID(
    AJSON.GetValue<string>(TNames.TaskID, ''),
    TTasksUtil.StringToKind(AJSON.GetValue<string>(TNames.Kind, '')),
    AJSON.GetValue<string>(TNames.Comment, ''),
    ACreatedBy,
    ACreatedAt,
    ACreatedAt,// UpdatedAt
    AJSON.GetValue<string>(TNames.ID, ''));
end;

class function TMessageJSON.MessageToJSON(const AMessage: TMessage): TJSONObject; // Caller must free result
begin
  Result := TJSONObject.Create;
  Result.AddPair(TNames.TaskID, AMessage.TaskID);
  Result.AddPair(TNames.Kind, TTasksUtil.KindToString(AMessage.Kind));
  Result.AddPair(TNames.Comment, AMessage.Comment);
  Result.AddPair(TNames.CreatedBy, AMessage.CreatedBy);
  Result.AddPair(TNames.CreatedAt, AMessage.CreatedAt.AsUTCString);
  Result.AddPair(TNames.UpdatedAt, AMessage.UpdatedAt.AsUTCString);
  Result.AddPair(TNames.ID, AMessage.ID);
end;

class function TMessageJSON.JSONToMessages(const AJSON: TJSONArray): TArray<TMessage>;
var
  LValue: TJSONValue;
  LList: TList<TMessage>;
begin
  LList := TList<TMessage>.Create;
  try
    for LValue in AJSON do
      LList.Add(TMessageJSON.JSONToMessage(LValue));
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

class procedure TMessageJSON.MessagesToJSON(const AMessages: TArray<TMessage>;
  const AJSON: TJSONArray);
var
  LValue: TMessage;
  LJSON: TJSONObject;
begin
  for LValue in AMessages do
  begin
    LJSON := MessageToJSON(LValue);
    AJSON.Add(LJSON);
  end;
end;

constructor TMessage.CreateWithID(const ATaskID: string; AKind: TKind; const AMessage, ACreatedBy: string; const ACreatedAt, AUpdatedAt: string; const AID: string);
begin
  Create(ATaskID, AKind, AMessage, ACreatedBy, ACreatedAt, AUpdatedAt);
  FID := AID;
end;

procedure TMessage.Update;
begin
  FUpdatedAt := TTaskDateTime.CreateUTCString('');
end;

{ TTaskDateTime }

constructor TTaskDateTime.CreateUTCString(const AUTCString: string);
begin
  if AUTCString <> '' then
    FUTCTime := ISO8601ToDate(AUTCString)
  else
    FUTCTime := TTimeZone.Local.ToUniversalTime(Now);
end;

function TTaskDateTime.GetLocalDateTime: TDateTime;
begin
  Result := TTimeZone.Local.ToLocalTime(FUTCTime);
end;

function TTaskDateTime.GetLocalString: string;
begin
  Result := DateTimeToStr(GetLocalDateTime)
end;

function TTaskDateTime.GetUTCDateTime: TDateTime;
begin
  Result := FUTCTime;
end;

function TTaskDateTime.GetUTCString: string;
begin
  Result := DateToISO8601(FUTCTime);
end;

end.
