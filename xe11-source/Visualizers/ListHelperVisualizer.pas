{*******************************************************}
{                                                       }
{            RadStudio Debugger Visualizer Sample       }
{ Copyright(c) 2009-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit ListHelperVisualizer;

interface

procedure Register;

implementation

uses
  System.SysUtils, System.Generics.Collections, ToolsAPI;

resourcestring
  sListHelperVisualizerName = 'TListHelper/TList<T>/TQueue<T>/TStack<T> Visualizer for Delphi';
  sListHelperVisualizerDescription = 'Displays TListHelper/TList<T>/TQueue<T>/TStack<T> instances with a human-readable FItems format rather than as a pointer value';

type
  TTypeReader = reference to procedure (AStr: TStringBuilder; APtr: Pointer);
  TItemInfo = record
    FSpace: string;
    FType: string;
    FPtrType: string;
    FSize: NativeInt;
    FReader: TTypeReader;
  end;

  TDebuggerListHelperVisualizer = class(TInterfacedObject, IOTADebuggerVisualizer,
    IOTADebuggerVisualizer250, IOTADebuggerVisualizerValueReplacer, IOTAThreadNotifier,
    IOTAThreadNotifier160, IOTADebuggerNotifier)
  private
    FNotifierIndex: Integer;
    FCompleted: Boolean;
    FDeferredResult: string;
    FItemInfos: TDictionary<string, TItemInfo>;
    class function GetTypeReader(const AProcess: IOTAProcess; 
      const ATypeName: string): TTypeReader; static;
  public
    constructor Create;
    destructor Destroy; override;
    { IOTADebuggerVisualizer }
    function GetSupportedTypeCount: Integer;
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean); overload;
    function GetVisualizerIdentifier: string;
    function GetVisualizerName: string;
    function GetVisualizerDescription: string;
    { IOTADebuggerVisualizer250 }
    procedure GetSupportedType(Index: Integer; var TypeName: string;
      var AllDescendants: Boolean; var IsGeneric: Boolean); overload;
    { IOTADebuggerVisualizerValueReplacer }
    function GetReplacementValue(const Expression, TypeName, EvalResult: string): string;
    { IOTAThreadNotifier }
    procedure EvaluateComplete(const ExprStr: string; const ResultStr: string;
      CanModify: Boolean; ResultAddress: Cardinal; ResultSize: Cardinal;
      ReturnCode: Integer); overload;
    procedure ModifyComplete(const ExprStr: string; const ResultStr: string;
      ReturnCode: Integer);
    procedure ThreadNotify(Reason: TOTANotifyReason);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAThreadNotifier160 }
    procedure EvaluateComplete(const ExprStr: string; const ResultStr: string;
      CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
      ReturnCode: Integer); overload;
    { IOTADebuggerNotifier }
    procedure ProcessCreated(Const Process: IOTAProcess);
    procedure ProcessDestroyed(Const Process: IOTAProcess);
    procedure BreakpointAdded(Const Breakpoint: IOTABreakpoint);
    procedure BreakpointDeleted(Const Breakpoint: IOTABreakpoint);
  end;

  TListHelperVisualizerType = record
    TypeName: string;
    AllDesc: Boolean;
    IsGen: Boolean;
    ListHlprExp: string
  end;

const
  ListHelperVisualizerTypes: array[0..8] of TListHelperVisualizerType =
  (
    (TypeName: 'TListHelper'; AllDesc: False; IsGen: False; ListHlprExp: '';),
    (TypeName: 'TQueueHelper'; AllDesc: False; IsGen: False; ListHlprExp: '.FLH';),
    (TypeName: 'TStackHelper'; AllDesc: False; IsGen: False; ListHlprExp: '.FLH';),
    (TypeName: 'System.Generics.Collections.TList<T>'; AllDesc: False; IsGen: True; ListHlprExp: '.FListHelper';),
    (TypeName: 'System.Generics.Collections.TQueue<T>'; AllDesc: False; IsGen: True; ListHlprExp: '.FQueueHelper.FLH';),
    (TypeName: 'System.Generics.Collections.TStack<T>'; AllDesc: False; IsGen: True; ListHlprExp: '.FStackHelper.FLH';),
    (TypeName: 'System.Generics.Collections.TObjectList<T>'; AllDesc: False; IsGen: True; ListHlprExp: '.FListHelper';),
    (TypeName: 'System.Generics.Collections.TObjectQueue<T>'; AllDesc: False; IsGen: True; ListHlprExp: '.FQueueHelper.FLH';),
    (TypeName: 'System.Generics.Collections.TObjectStack<T>'; AllDesc: False; IsGen: True; ListHlprExp: '.FStackHelper.FLH';)
  );

  MaxItemsToDump: Integer = 500;

{ TDebuggerListHelperVisualizer }

constructor TDebuggerListHelperVisualizer.Create;
begin
  inherited Create;
  FItemInfos := TDictionary<string, TItemInfo>.Create;
end;

destructor TDebuggerListHelperVisualizer.Destroy;
begin
  FItemInfos.Free;
  inherited Destroy;
end;

procedure TDebuggerListHelperVisualizer.ProcessCreated(
  const Process: IOTAProcess);
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.ProcessDestroyed(
  const Process: IOTAProcess);
begin
  TMonitor.Enter(FItemInfos);
  try
    FItemInfos.Clear;
  finally
    TMonitor.Exit(FItemInfos);
  end;
end;

procedure TDebuggerListHelperVisualizer.BreakpointAdded(
  const Breakpoint: IOTABreakpoint);
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.BreakpointDeleted(
  const Breakpoint: IOTABreakpoint);
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.AfterSave;
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.BeforeSave;
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.Destroyed;
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.Modified;
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.ModifyComplete(const ExprStr,
  ResultStr: string; ReturnCode: Integer);
begin
  // don't care about this notification
end;

procedure TDebuggerListHelperVisualizer.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress, ResultSize: Cardinal;
  ReturnCode: Integer);
begin
  EvaluateComplete(ExprStr, ResultStr, CanModify, TOTAAddress(ResultAddress),
    LongWord(ResultSize), ReturnCode);
end;

procedure TDebuggerListHelperVisualizer.EvaluateComplete(const ExprStr,
  ResultStr: string; CanModify: Boolean; ResultAddress: TOTAAddress; ResultSize: LongWord;
  ReturnCode: Integer);
begin
  FCompleted := True;
  if ReturnCode = 0 then
    FDeferredResult := ResultStr;
end;

procedure TDebuggerListHelperVisualizer.ThreadNotify(Reason: TOTANotifyReason);
begin
  // don't care about this notification
end;

class function TDebuggerListHelperVisualizer.GetTypeReader(const AProcess: IOTAProcess; const ATypeName: string): TTypeReader;
begin
  Result := nil;
  if SameText(ATypeName, 'ShortInt') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PShortInt(APtr)^); end
  else if SameText(ATypeName, 'Byte') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PByte(APtr)^); end
  else if SameText(ATypeName, 'SmallInt') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PSmallInt(APtr)^); end
  else if SameText(ATypeName, 'Word') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PWord(APtr)^); end
  else if SameText(ATypeName, 'Integer') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PInteger(APtr)^); end
  else if SameText(ATypeName, 'Cardinal') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PCardinal(APtr)^); end
  else if SameText(ATypeName, 'Int64') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PInt64(APtr)^); end
  else if SameText(ATypeName, 'UInt64') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PUInt64(APtr)^); end
  else if SameText(ATypeName, 'Boolean') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PBoolean(APtr)^); end
  else if SameText(ATypeName, 'Single') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PSingle(APtr)^); end
  else if SameText(ATypeName, 'Double') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PDouble(APtr)^); end
  else if SameText(ATypeName, 'Extended') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(Double(PExtended(APtr)^)); end
  else if SameText(ATypeName, 'Currency') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PCurrency(APtr)^); end
  else if SameText(ATypeName, 'Char') then
    Result := procedure (AStr: TStringBuilder; APtr: Pointer) begin AStr.Append(PChar(APtr)^); end
  else if SameText(ATypeName, 'Pointer') then
    if AProcess.GetProcessType in [optWin32, optOSX32, optiOS32, optAndroid] then
      Result := procedure (AStr: TStringBuilder; APtr: Pointer)
        begin
          if PCardinal(APtr)^ = 0 then
            AStr.Append('nil')
          else
          begin
            AStr.Append('$');
            AStr.Append(IntToHex(PCardinal(APtr)^, 0));
          end
        end
    else
      Result := procedure (AStr: TStringBuilder; APtr: Pointer)
        begin
          if PUInt64(APtr)^ = 0 then
            AStr.Append('nil')
          else
          begin
            AStr.Append('$');
            AStr.Append(IntToHex(PUInt64(APtr)^, 0));
          end;
        end;
end;

function TDebuggerListHelperVisualizer.GetReplacementValue(
  const Expression, TypeName, EvalResult: string): string;
var
  DebugSvcs: IOTADebuggerServices;
  CurProcess: IOTAProcess;
  CurThread: IOTAThread;
  LExp: string;
  LAsRec: Boolean;
  s: string;
  I, i1, i2: Integer;
  LItemInfo: TItemInfo;
  LItemCount: NativeInt;
  LItemAddr: NativeUInt;
  LStr: TStringBuilder;
  LModifiers: string;
  LBytes: TBytes;

  function GetListHlprExp(const ATypeName: string): string;
  var
    I, Len: Integer;
  begin
    Len := ATypeName.IndexOf('<') + 1;
    if Len = 0 then
      Len := Length(ATypeName);
    for I := Low(ListHelperVisualizerTypes) to High(ListHelperVisualizerTypes) do
      if StrLIComp(PChar(ListHelperVisualizerTypes[I].TypeName), PChar(ATypeName), Len) = 0 then
        Exit(ListHelperVisualizerTypes[I].ListHlprExp);
  end;

  function EvalExp(const AExp: string): string;
  var
    EvalRes: TOTAEvaluateResult;
    ResultStr: array[0..4095] of Char;
    CanModify: Boolean;
    ResultAddr, ResultSize, ResultVal: LongWord;
  begin
    EvalRes := CurThread.Evaluate(AExp, @ResultStr, Length(ResultStr),
      CanModify, eseAll, '', ResultAddr, ResultSize, ResultVal, '', 0);
    if EvalRes = erOK then
      Result := ResultStr
    else if EvalRes = erDeferred then
    begin
      FCompleted := False;
      FDeferredResult := '';
      FNotifierIndex := CurThread.AddNotifier(Self);
      while not FCompleted do
        DebugSvcs.ProcessDebugEvents;
      CurThread.RemoveNotifier(FNotifierIndex);
      FNotifierIndex := -1;
      if FDeferredResult <> '' then
        Result := FDeferredResult;
    end;
  end;

  function PrepExp(const AExp, ARes: string; var AAsRec: Boolean; var AModifiers: string): string;
  var
    I: Integer;
    LAsRec: Boolean;
  begin
    LAsRec := False;
    I := AExp.Length;
    while (I >= 1) and CharInSet(AExp[I], [' ', 'h', 'c', 'd', 'm', 'p', 'r', 's',
                                                'H', 'C', 'D', 'M', 'P', 'R', 'S' ]) do
    begin
      if CharInSet(AExp[I], ['r', 'R']) then
        LAsRec := True;
      Dec(I);
    end;
    if (I >= 1) and (AExp[I] = ',') then
    begin
      AAsRec := LAsRec;
      AModifiers := AExp.Substring(I - 1);
      Result := AExp.Substring(0, I - 1).Trim;
    end
    // Tooltip Expression Evaluation does not use modifiers, but asks for ",r" evaluation format.
    // Then result string will look like "(field: ...".
    else if (Length(ARes) >= 2) and (ARes[1] = '(') and IsValidIdent(ARes[2]) then
    begin
      AAsRec := True;
      AModifiers := ',r';
      Result := AExp;
    end
    else
    begin
      AAsRec := False;
      AModifiers := '';
      Result := AExp;
    end;
  end;

begin
  Result := EvalResult;
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebugSvcs) then
    CurProcess := DebugSvcs.CurrentProcess;
  if CurProcess = nil then
    Exit;
  CurThread := CurProcess.CurrentThread;
  if CurThread = nil then
    Exit;

  // Detect and remove debugger format specifiers
  LExp := PrepExp(Expression, EvalResult, LAsRec, LModifiers);
  // Append an expression returning TListHelper
  LExp := LExp + GetListHlprExp(TypeName);

  TMonitor.Enter(FItemInfos);
  try
    if not FItemInfos.TryGetValue(TypeName, LItemInfo) then
      try
        // Get element type name and space from 'TList<T>.arrayofT'
        s := EvalExp('PTypeInfo(' + LExp + '.FTypeInfo)^.Name');
        i1 := s.IndexOf('<');
        i2 := s.LastIndexOf('>');
        if (i1 = -1) or (i2 = -1) then
          Exit;
        s := s.Substring(i1 + 1, i2 - i1 - 1);
        i1 := s.LastIndexOf('.');
        if i1 >= 0 then
        begin
          LItemInfo.FSpace := s.Substring(0, i1 + 1);
          LItemInfo.FType := s.Substring(i1 + 1);
          // Evaluating of "SizeOf(string)" or "SizeOf(System.string)" returns error
          // "System unit out of date or corrupted: missing 'UnicodeString'"
          if SameText(LItemInfo.FSpace, 'System.') then
          begin
            LItemInfo.FSpace := '';
            if SameText(LItemInfo.FType, 'string') then
              LItemInfo.FType := 'UnicodeString';
          end;
        end
        else
        begin
          LItemInfo.FSpace := '';
          LItemInfo.FType := s;
        end;
        if LItemInfo.FType.IsEmpty then
          Exit;

        // Get element type size
        s := EvalExp('SizeOf(' + LItemInfo.FSpace + LItemInfo.FType +')');
        LItemInfo.FSize := StrToInt64Def(s, -1);
        if (LItemInfo.FSize = -1) and not LItemInfo.FSpace.IsEmpty then
        begin
          // Try without namespace, debugger can fail to evaluate expression with
          // namespace, for example System.Types.TPoint
          LItemInfo.FSpace := '';
          s := EvalExp('SizeOf(' + LItemInfo.FType +')');
          LItemInfo.FSize := StrToInt64Def(s, -1);
          if LItemInfo.FSize = -1 then
            Exit;
        end;

        // Try to get element pointer type
        LItemInfo.FPtrType := LItemInfo.FType;
        if LItemInfo.FPtrType.StartsWith('T', True) then
          LItemInfo.FPtrType := LItemInfo.FPtrType.Substring(1);
        LItemInfo.FPtrType := 'P' + LItemInfo.FPtrType;
        s := EvalExp('SizeOf(' + LItemInfo.FSpace + LItemInfo.FPtrType +')');
        if s.IsEmpty then
          LItemInfo.FPtrType := '';

        LItemInfo.FReader := GetTypeReader(CurProcess, LItemInfo.FType);
      finally
        FItemInfos.Add(TypeName, LItemInfo);
      end
    else
    if LItemInfo.FType.IsEmpty or (LItemInfo.FSize = -1) then
      Exit;
  finally
    TMonitor.Exit(FItemInfos);
  end;

  // Get items pointer
  s := EvalExp(LExp + '.FItems');
  if SameText(s, 'nil') then
  begin
    LItemAddr := 0;
    LItemCount := 0;
  end
  else
  begin
    LItemAddr := StrToUInt64Def(s, 0);
    // Get items count
    LItemCount := StrToInt64Def(EvalExp('PNativeInt(PByte(' + LExp + '.FItems) - SizeOf(NativeInt))^'), -1);
    if (LItemCount = -1) or (LItemCount > 0) and (LItemAddr = 0) then
      Exit;
  end;

  LStr := TStringBuilder.Create(512);
  try
    // When reader is assigned, then read full array and use reader to format items
    if (LItemInfo.FReader <> nil) and (LItemCount > 0) then
    begin
      SetLength(LBytes, LItemInfo.FSize * LItemCount);
      CurProcess.ReadProcessMemory(LItemAddr, LItemInfo.FSize * LItemCount, LBytes[0]);
      LItemAddr := NativeUInt(@LBytes[0]);
    end;

    // Produce string with element values
    LStr.Append('(');
    for I := 0 to LItemCount - 1 do
    begin
      if I > 0 then
        LStr.Append(', ');
      if (MaxItemsToDump > 0) and (I > MaxItemsToDump) then
      begin
        LStr.Append('...');
        Break;
      end;

      if LItemInfo.FReader <> nil then
        LItemInfo.FReader(LStr, Pointer(LItemAddr))
      else
      begin
        if LItemInfo.FPtrType.IsEmpty then
          s := EvalExp(LItemInfo.FSpace + LItemInfo.FType + '(Pointer(' + LItemAddr.ToString + ')^)' + LModifiers)
        else
          s := EvalExp(LItemInfo.FSpace + LItemInfo.FPtrType + '(Pointer(' + LItemAddr.ToString + '))^' + LModifiers);
        if s = '' then
          Exit;
        LStr.Append(s);
      end;
      LItemAddr := LItemAddr + NativeUInt(LItemInfo.FSize);
    end;
    LStr.Append(')');

    // Replace FItems value by new element values string
    if LAsRec then
    begin
      i1 := Result.IndexOf('(FItems:');
      if i1 <> -1 then
        Inc(i1, 7);
      i2 := Result.IndexOf(';', i1);
    end
    else
    begin
      // first address value is FItems
      i1 := Result.IndexOf('($');
      i2 := Result.IndexOf(',', i1);
    end;
    if (i1 = -1) or (i2 = -1) then
      Exit;
    Result := Result.Substring(0, i1 + 1) + LStr.ToString(True) + Result.Substring(i2);
  finally
    LStr.Free;
  end;
end;

function TDebuggerListHelperVisualizer.GetSupportedTypeCount: Integer;
begin
  Result := Length(ListHelperVisualizerTypes);
end;

procedure TDebuggerListHelperVisualizer.GetSupportedType(Index: Integer; var TypeName: string;
  var AllDescendants: Boolean);
begin
  TypeName := ListHelperVisualizerTypes[Index].TypeName;
  AllDescendants := ListHelperVisualizerTypes[Index].AllDesc;
end;

procedure TDebuggerListHelperVisualizer.GetSupportedType(Index: Integer;
  var TypeName: string; var AllDescendants, IsGeneric: Boolean);
begin
  TypeName := ListHelperVisualizerTypes[Index].TypeName;
  AllDescendants := ListHelperVisualizerTypes[Index].AllDesc;
  IsGeneric := ListHelperVisualizerTypes[Index].IsGen;
end;

function TDebuggerListHelperVisualizer.GetVisualizerDescription: string;
begin
  Result := sListHelperVisualizerDescription;
end;

function TDebuggerListHelperVisualizer.GetVisualizerIdentifier: string;
begin
  Result := ClassName;
end;

function TDebuggerListHelperVisualizer.GetVisualizerName: string;
begin
  Result := sListHelperVisualizerName;
end;

var
  ListHelperVis: IOTADebuggerVisualizer = nil;
  DebugNotifier: Integer = -1;

procedure Register;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    ListHelperVis := TDebuggerListHelperVisualizer.Create;
    DebuggerServices.RegisterDebugVisualizer(ListHelperVis);
    DebugNotifier := DebuggerServices.AddNotifier(ListHelperVis as IOTADebuggerNotifier);
  end;
end;

procedure RemoveVisualizer;
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) then
  begin
    if DebugNotifier <> -1 then
    begin
      DebuggerServices.RemoveNotifier(DebugNotifier);
      DebugNotifier := -1;
    end;
    if ListHelperVis <> nil then
    begin
      DebuggerServices.UnregisterDebugVisualizer(ListHelperVis);
      ListHelperVis := nil;
    end;
  end;
end;

initialization
finalization
  RemoveVisualizer;
end.
