//---------------------------------------------------------------------------

// This software is Copyright (c) 2014 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit TasksStorageU;

interface

uses System.Classes, System.SysUtils,  System.IniFiles, System.Generics.Collections,
  TasksTypesU;

type

  TTasksStorage = class
  private type
    TSuffix = record
      const
        Title = '.title';
        Text = '.text';
        CreatedBy = '.createdby';
        AssignedTo = '.assignedto';
        Status = '.status';
        Updated = '.updated';
        CreatedAt = '.createdat';
        UpdatedAt = '.updatedat';
        Kind = '.kind';

        Task = '.task';
        TaskMessages = '.taskmessages';

        // Messages
        Comment = '.comment';
    end;
  private
    FIniFile: TIniFile;
    class function TaskSection(const AID: string): string; static;
    class function TaskMessageSection(const AID: string): string; static;
    function DecodeMultilineText(const AText: string): string;
    function EncodeMultilineText(const AText: string): string;
    procedure ReadTask(const AID: string; out ATask: TTask);
    procedure ReadIDs(const ASection, ASuffix: string; const AList: TList<string>); overload;
    function ReadIDs(const ASection, ASuffix: string): TArray<string>; overload;
    procedure ReadMessage(const ATaskID, AMessageID: string;
      out AMessage: TMessage);
    function MessageExists(const ATaskID, AMessageID: string): Boolean;
  public
    constructor Create(const ADirectory: string); // ; const AUserID: string);
    destructor Destroy; override;
    function GetTasks: TArray<TTask>;
    function GetTask(const AID: string; out ATask: TTask): Boolean;
    procedure UpdateTask(const AID: string; const ATask: TTask);
    procedure AddTask(const ATask: TTask; out AID: string);
    procedure AddMessage(const AID: string; const AMessage: TMessage; out AMessageID: string);
    procedure UpdateMessage(const ATaskID, AMessageID: string; const AMessage: TMessage);
    function DeleteMessage(const ATaskID, AMessageID: string): Boolean;
    function GetTaskMessages(const AID: string; out AMessages: TArray<TMessage>): Boolean; overload;
    function DeleteTask(const AID: string): Boolean;
    function TaskTitleExists(const ATitle: string): Boolean;
    function FindTask(const ATitle: string; out ATask: TTask): Boolean;
    function TaskIDExists(const AID: string): Boolean;
//    procedure GetUserMessages(const AUserName: string; out AMessages: TArray<TMessage>); overload;
  end;

  ETaskError = class(Exception);
  ETaskNotFound = class(ETaskError);
  ETaskDuplicate = class(ETaskError);
  ETaskMissingTitle = class(ETaskError);

implementation

uses System.TypInfo;

{ TTasksStorage }

procedure TTasksStorage.AddTask(const ATask: TTask; out AID: string);

  function NextID: string;
  var
    LList: TList<string>;
    I: Integer;
  begin
    LList := TList<string>.Create;
    try
      ReadIDs('', TSuffix.Task, LList);
      I := LList.Count;
      while LList.Contains(IntToStr(I)) do
        Inc(I);
      Result := IntToStr(I);
    finally
      LList.Free;
    end;
  end;

begin
  if ATask.Title = '' then
    raise ETaskMissingTitle.Create('Title required');
  if TaskTitleExists(ATask.Title) then
    raise ETaskDuplicate.CreateFmt('"%s" already exists', [ATask.Title]);
  AID := NextID;
  FIniFile.WriteString(TaskSection(AID), TSuffix.Title, ATask.Title);
  FIniFile.WriteString(TaskSection(AID), TSuffix.Text, EncodeMultilineText( ATask.Content));
  FIniFile.WriteString(TaskSection(AID), TSuffix.AssignedTo, ATask.AssignedTo);
  FIniFile.WriteString(TaskSection(AID), TSuffix.CreatedBy, ATask.CreatedBy);
  FIniFile.WriteString(TaskSection(AID), TSuffix.CreatedAt, ATask.CreatedAt.AsUTCString);
  FIniFile.WriteString(TaskSection(AID), TSuffix.Status, TTasksUtil.StatusToString(ATask.Status));
end;

procedure TTasksStorage.AddMessage(const AID: string; const AMessage: TMessage; out AMessageID: string);

  function NextID: string;
  var
    LList: TList<string>;
    I: Integer;
  begin
    LList := TList<string>.Create;
    try
      ReadIDs(TaskMessageSection(AID), TSuffix.Comment, LList);
      I := LList.Count;
      while LList.Contains(IntToStr(I)) do
        Inc(I);
      Result := IntToStr(I);
    finally
      LList.Free;
    end;
  end;

begin
  AMessageID := NextID; // NOTE: Race condition if there are simultaneous Adds
  FIniFile.WriteString(TaskMessageSection(AID), AMessageID + TSuffix.Comment,
    EncodeMultilineText(AMessage.Comment));
  FIniFile.WriteString(TaskMessageSection(AID), AMessageID + TSuffix.CreatedBy,
    AMessage.CreatedBy);
  FIniFile.WriteString(TaskMessageSection(AID), AMessageID + TSuffix.CreatedAt,
    AMessage.CreatedAt.AsUTCString);
  FIniFile.WriteString(TaskMessageSection(AID), AMessageID + TSuffix.Kind,
    TTasksUtil.KindToString(AMessage.Kind));
end;

constructor TTasksStorage.Create(const ADirectory: string);
var
  LPath: string;
begin
  LPath := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory)) + 'tasks.ini';
  FIniFile := TIniFile.Create(LPath);
end;

function TTasksStorage.TaskIDExists(const AID: string): Boolean;
begin
  Result := FIniFile.SectionExists(TaskMessageSection(AID));
end;

class function TTasksStorage.TaskMessageSection(const AID: string): string;
begin
  Result := AID + TSuffix.TaskMessages;
end;

class function TTasksStorage.TaskSection(const AID: string): string;
begin
  Result := AID + TSuffix.Task;
end;

function TTasksStorage.MessageExists(const ATaskID, AMessageID: string): Boolean;
begin
  Result := FIniFile.ValueExists(TaskMessageSection(ATaskID), AMessageID + TSuffix.Comment);
end;

function TTasksStorage.TaskTitleExists(const ATitle: string): Boolean;
var
  LTask: TTask;
begin
  Result := False;
  for LTask in GetTasks do
    if LTask.Title = ATitle then
    begin
      Result := True;
      break;
    end;
end;

function TTasksStorage.FindTask(const ATitle: string; out ATask: TTask): Boolean;
var
  LTask: TTask;
begin
  Result := False;
  for LTask in GetTasks do
    if LTask.Title = ATitle then
    begin
      Result := True;
      ReadTask(LTask.ID, ATask);
      break;
    end;
end;

function TTasksStorage.DeleteMessage(const ATaskID,
  AMessageID: string): Boolean;
begin
  Result := MessageExists(ATaskID, AMessageID);
  if Result then
  begin
    FIniFile.DeleteKey(TaskMessageSection(ATaskID), AMessageID + TSuffix.Comment);
    FIniFile.DeleteKey(TaskMessageSection(ATaskID), AMessageID + TSuffix.CreatedBy);
    FIniFile.DeleteKey(TaskMessageSection(ATaskID), AMessageID + TSuffix.UpdatedAt);
    FIniFile.DeleteKey(TaskMessageSection(ATaskID), AMessageID + TSuffix.CreatedAt);
    FIniFile.DeleteKey(TaskMessageSection(ATaskID), AMessageID + TSuffix.Kind);

  end;
end;

function TTasksStorage.DeleteTask(const AID: string): Boolean;
begin
  Result := TaskIDExists(AID);
  if Result then
  begin
    FIniFile.EraseSection(TaskSection(AID));
    FIniFile.EraseSection(TaskMessageSection(AID));
  end;
end;

destructor TTasksStorage.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TTasksStorage.EncodeMultilineText(const AText: string): string;
var
  LBuilder: TStringBuilder;
  S: Char;
begin
  LBuilder := TStringBuilder.Create;
  try
    for S in AText do
    begin
      case S of
      #10:
        LBuilder.Append('\n');
      #13:
        LBuilder.Append('\r');
      '\':
        LBuilder.Append('\\');
      else
        LBuilder.Append(S);
      end;
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TTasksStorage.DecodeMultilineText(const AText: string): string;
var
  LBuilder: TStringBuilder;
  I: Integer;
  S: Char;
begin
  LBuilder := TStringBuilder.Create;
  try
    I := 0;
    while I < AText.Length do
    begin
      S := AText.Chars[I];
      if (S = '\') and (I+1 < AText.Length) then
        case AText.Chars[I+1] of
          'n':
            begin
              Inc(I);
              S := #10;
            end;
          'r':
            begin
              Inc(I);
              S := #13;
            end;
          '\':
            begin
              Inc(I);
              S := '\';
            end
        end;
      LBuilder.Append(S);
      Inc(I);
    end;
    Result := LBuilder.ToString;
  finally
    LBuilder.Free;
  end;
end;

function TTasksStorage.GetTask(const AID: string; out ATask: TTask): Boolean;
begin
  Result := TaskIDExists(AID);
  if Result then
    ReadTask(AID, ATask);
end;

procedure TTasksStorage.ReadTask(const AID: string; out ATask: TTask);
var
  LText: string;
  LTitle: string;
  LCreatedBy: string;
  LAssignedTo: string;
  LStatus: string;
  LCreatedAt: string;
  LUpdatedAt: string;
begin
  LTitle := FIniFile.ReadString(TaskSection(AID), TSuffix.Title, '');
  LText := DecodeMultilineText(FIniFile.ReadString(TaskSection(AID), TSuffix.Text, ''));
  LCreatedBy := FIniFile.ReadString(TaskSection(AID), TSuffix.CreatedBy, '');
  LAssignedTo := FIniFile.ReadString(TaskSection(AID), TSuffix.AssignedTo, '');
  LStatus := FIniFile.ReadString(TaskSection(AID), TSuffix.Status, '');
  LCreatedAt := FIniFile.ReadString(TaskSection(AID), TSuffix.CreatedAt, '');
  LUpdatedAt := FIniFile.ReadString(TaskSection(AID), TSuffix.UpdatedAt, '');
  ATask := TTask.CreateWithID(LTitle, LText, LCreatedBy, LAssignedTo, TTasksUtil.StringToStatus(LStatus), LCreatedAt, LUpdatedAt, AID);
end;

procedure TTasksStorage.ReadMessage(const ATaskID, AMessageID: string; out AMessage: TMessage);
var
  LMessage: string;
  LCreatedBy: string;
  LCreatedAt: string;
  LUpdatedAt: string;
  LKind: TMessage.TKind;
begin
  LMessage := FIniFile.ReadString(TaskMessageSection(ATaskID), AMessageID + TSuffix.Comment, '');
  LMessage := DecodeMultilineText(LMessage);
  LCreatedBy := FIniFile.ReadString(TaskMessageSection(ATaskID), AMessageID + TSuffix.CreatedBy, '');
  LCreatedAt := FIniFile.ReadString(TaskMessageSection(ATaskID), AMessageID + TSuffix.CreatedAt, '');
  LUpdatedAt := FIniFile.ReadString(TaskMessageSection(ATaskID), AMessageID + TSuffix.UpdatedAt, '');
  LKind :=
    TTasksUtil.StringToKind(FIniFile.ReadString(TaskMessageSection(ATaskID), AMessageID + TSuffix.Kind, ''));

  AMessage := TMessage.CreateWithID(ATaskID, LKind, LMessage, LCreatedBy, LCreatedAt, LUpdatedAt, AMessageID);
end;

function TTasksStorage.GetTasks: TArray<TTask>;
var
  LList: TList<TTask>;
  LTask: TTask;
  LID: string;
begin
  LList := nil;
  try
    LList := TList<TTask>.Create;
    for LID in ReadIDs('', TSuffix.Task) do
    begin
      ReadTask(LID, LTask);
      LList.Add(LTask);
    end;
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;


function TTasksStorage.GetTaskMessages(const AID: string; out AMessages: TArray<TMessage>): Boolean;
var
  LList: TList<TMessage>;
  LMessage: TMessage;
  LMessageID: string;
begin
  Result := TaskIDExists(AID);
  if Result then
  begin
    LList := nil;
    try
      LList := TList<TMessage>.Create;
      for LMessageID in ReadIDs(TaskMessageSection(AID), TSuffix.Comment) do
      begin
        ReadMessage(AID, LMessageID, LMessage);
        LList.Add(LMessage);
      end;
      AMessages := LList.ToArray;
    finally
      LList.Free;
    end;
  end;
end;

function TTasksStorage.ReadIDs(const ASection, ASuffix: string): TArray<string>;
var
  LList: TList<string>;
begin
  LList := TList<string>.Create;
  try
    ReadIDs(ASection, ASuffix, LList);
    Result := LList.ToArray;
  finally
    LList.Free;
  end;
end;

procedure TTasksStorage.ReadIDs(const ASection, ASuffix: string; const AList: TList<string>);
var
  LSection: TStrings;
  I: Integer;
  LID: string;
  LKey: string;
begin
  LSection := nil;
  try
    LSection := TStringList.Create;
    if ASection <> '' then
      FIniFile.ReadSection(ASection, LSection)
    else
      FIniFile.ReadSections(LSection);
    for I := 0 to LSection.Count - 1 do
    begin
      LKey := LSection[I];
      if LKey.EndsWith(ASuffix) then   // e.g.; '.title'
      begin
        LID := LKey.Substring(0, Length(LKey) - Length(ASuffix));
        Assert(not AList.Contains(LID));
        Assert(LID <> '');
        AList.Add(LID);
      end;
    end;
  finally
    LSection.Free;
  end;
end;

procedure TTasksStorage.UpdateMessage(const ATaskID, AMessageID: string;
  const AMessage: TMessage);
begin
  if not MessageExists(ATaskID, AMessageID) then
    raise ETaskNotFound.CreateFmt('"%s\messages\%s" not found', [ATaskID, AMessageID]);
  FIniFile.WriteString(TaskMessageSection(ATaskID), AMessageID + TSuffix.Comment, AMessage.Comment);
  FIniFile.WriteString(TaskMessageSection(ATaskID), AMessageID + TSuffix.UpdatedAt, AMessage.UpdatedAt.AsUTCString);
  FIniFile.WriteString(TaskMessageSection(ATaskID), AMessageID + TSuffix.Kind,
    TTasksUtil.KindToString(AMessage.Kind));
  // Do not update createdby or createdat
end;

procedure TTasksStorage.UpdateTask(const AID: string; const ATask: TTask);
begin
  if not TaskIDExists(AID) then
    raise ETaskNotFound.CreateFmt('"%s" not found', [AID]);
  FIniFile.WriteString(TaskSection(AID), TSuffix.Title, ATask.Title);
  FIniFile.WriteString(TaskSection(AID), TSuffix.Text, EncodeMultilineText(ATask.Content));
  FIniFile.WriteString(TaskSection(AID), TSuffix.Status, TTasksUtil.StatusToString(ATask.Status));
  FIniFile.WriteString(TaskSection(AID), TSuffix.UpdatedAt, ATask.UpdatedAt.AsUTCString);
  FIniFile.WriteString(TaskSection(AID), TSuffix.AssignedTo, ATask.AssignedTo);
  // Do not update createdby or createdat
end;


end.
