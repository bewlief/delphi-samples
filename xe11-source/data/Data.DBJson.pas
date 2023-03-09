{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

       
                                        
                                         
                                           
                                         
                                                                                   
                                                                       
 

unit Data.DBJson;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Generics.Collections, System.JSON, System.JSON.Readers,
  System.JSON.Builders, System.JSON.Writers, Data.DB;

type
  TJSONMetaMergeMode = (None, Merge, Add, Update, AddOrError, Error);
  TJSONTypesMode = (Rich, JSONOnly, StringOnly);
  TJSONDataSetArea = (All, AllFromCurrent, Current, CurrentAsArray);

  /// <summary>
  /// TJSONToDataSetBridge serves as a bridge between dataset and JSON value. And performs the tasks:
  /// * Define method - scans JSON value structure and defines FieldDefs according to this structure;
  /// * Append method - imports JSON value into DataSet, where JSON object corresponds to single record
  ///                   and JSON array to set of records;
  /// * Produce method - exports Dataset into JSON value.
  /// </summary>
  TJSONToDataSetBridge = class(TObject)
  public type
    IAdaptor = interface (IUnknown)
      function GetDefaultFieldName(const AJSON: TJSONValue): string;
      function GetScanDepth: Integer;
    end;

  private
    FAdaptor: IAdaptor;
    FDataSet: TDataSet;
    FFieldDefs: TFieldDefs;
    FFieldPaths: TDictionary<string, string>;
    FStringFieldSize: Integer;
    FMetaMergeMode: TJSONMetaMergeMode;
    FObjectView: Boolean;
    FSampleObjects: Integer;
    FTypesMode: TJSONTypesMode;
    FPKFields: string;

    function GetDefaultFieldName(const AJSON: TJSONValue): string;
    function GetScanDepth: Integer;
    class function ContainsArrayOfObjects(AReader: TJSONReader): Boolean; static;
  public
    constructor Create(const AAdaptor: IAdaptor);
    destructor Destroy; override;

    /// <summary>
    /// Resets internal state of bridge, including FieldDefs and PKFields.
    /// </summary>
    procedure Reset;

    /// <summary>
    /// Defines FieldDefs using TJSONIterator current level JSON value.
    /// This method normally is used with JSON object, where each element of object will be
    /// represented by dataset field. Multiple calls of Define depending on MetaMergeMode can
    /// lead to more precise definition of FieldDefs.
    /// </summary>
    procedure Define(AIter: TJSONIterator); overload;
    /// <summary>
    /// Defines FieldDefs using TJSONReader current level JSON value.
    /// This method normally is used with JSON array of JSON objects. It will call Define(AIter)
    /// for each array item up to SampleObjects array items.
    /// </summary>
    procedure Define(AReader: TJSONReader); overload;
    /// <summary>
    /// Defines FieldDefs using TJSONValue.
    /// This method normally is used with JSON array of JSON objects. It will call Define(AIter)
    /// for each array item up to SampleObjects array items.
    /// </summary>
    procedure Define(AJSON: TJSONValue); overload;

    /// <summary>
    /// Appends new record to Dataset and populates it fields using TJSONReader current level JSON value.
    /// This method normally is used with JSON object. JSON elements are matched to Dataset Fields
    /// using JSON element names.
    /// </summary>
    procedure Append(AIter: TJSONIterator); overload;
    /// <summary>
    /// Appends new record to Dataset and populates it fields using TJSONReader current level JSON value.
    /// This method normally is used with JSON array. For each array item will be appended new Dataset record.
    /// </summary>
    procedure Append(AReader: TJSONReader); overload;
    /// <summary>
    /// Appends new record to Dataset and populates it fields using TJSONValue.
    /// This method normally is used with JSON array. For each array item will be appended new Dataset record.
    /// </summary>
    procedure Append(AValue: TJSONValue); overload;

    /// <summary>
    /// Returns unique identifying field after calling Define. This is used only with
    /// MongoDB datasets, where unique identifying field is Oid.
    /// </summary>
    property PKFields: string read FPKFields;

    /// <summary>
    /// Specifies the dataset to work with.
    /// </summary>
    property Dataset: TDataSet read FDataSet write FDataSet;
    /// <summary>
    /// Specifies the field defs to work with. Note, setting Dataset does not set FieldDefs.
    /// </summary>
    property FieldDefs: TFieldDefs read FFieldDefs write FFieldDefs;

    /// <summary>
    /// Specifies the column definitiob mode.
    /// When it is False, then JSON elements will be represented using flat field list.
    /// When it is True, then JSON objects will be represented using ftADT, arrays - ftDataSet,
    /// and simple fields - using a type derived from field contents. This mode is more slow and
    /// may be not supported by some dataset classes.
    /// Default value is False.
    /// </summary>
    property ObjectView: Boolean read FObjectView write FObjectView default False;
    /// <summary>
    /// Specifies the column definitions merging mode. Default value is TJSONMetaMergeMode.Merge.
    /// </summary>
    property MetaMergeMode: TJSONMetaMergeMode read FMetaMergeMode write FMetaMergeMode default TJSONMetaMergeMode.Merge;
    /// <summary>
    /// Specifies the number of objects in JSON dataset to scan to determine the structure.
    /// Default value is MaxInt, what means all dataset objects.
    /// </summary>
    property SampleObjects: Integer read FSampleObjects write FSampleObjects default MaxInt;
    /// <summary>
    /// Specifies the field type definition mode. When it is:
    /// * TJSONTypesMode.Rich, then adapter will try to derive field type from the JSON content.
    /// * TJSONTypesMode.JSONOnly, then adapter will use only JSON value types.
    /// * TJSONTypesMode.StringOnly, then all fields will be represented by ftWideString.
    /// Default value is TJSONTypesMode.Rich.
    /// </summary>
    property TypesMode: TJSONTypesMode read FTypesMode write FTypesMode default TJSONTypesMode.Rich;
    /// <summary>
    /// Specifies a string field size when TypesMode = TJSONTypesMode.StringOnly. Default value is 255.
    /// </summary>
    property StringFieldSize: Integer read FStringFieldSize write FStringFieldSize default 255;
  end;

  TDataSetToJSONBridge = class(TObject)
  private
    FDataSet: TDataSet;
    FFieldNames: TStrings;
    FArea: TJSONDataSetArea;
    FOnFilterRecord: TFilterRecordEvent;
    FIncludeNulls: Boolean;
    procedure WriteFieldValues(AFields: TFields; APairs: TJSONCollectionBuilder.TPairs);
    procedure WriteDataSet(ADataSet: TDataSet; AElems: TJSONCollectionBuilder.TElements;
      AArea: TJSONDataSetArea);
  protected
    function DoFilterRecord(ADataSet: TDataSet): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Produces JSON array with JSON objects using all Dataset records, where one object
    /// corresponds to one Dataset record. Uses TJSONWriter to build JSON array.
    /// </summary>
    procedure Produce(AWriter: TJSONWriter); overload;
    /// <summary>
    /// Produces JSON array with JSON objects using all Dataset records, where one object
    /// corresponds to one Dataset record. Returns JSON array.
    /// </summary>
    function Produce: TJSONAncestor; overload;

    /// <summary>
    /// Specifies the dataset to work with.
    /// </summary>
    property Dataset: TDataSet read FDataSet write FDataSet;
    /// <summary>
    /// Specifies the field name to work with. Empty FieldNames means all fields.
    /// </summary>
    property FieldNames: TStrings read FFieldNames;
    /// <summary>
    /// Specifies set of dataset records to include into JSON.
    /// </summary>
    property Area: TJSONDataSetArea read FArea write FArea default TJSONDataSetArea.All;
    /// <summary>
    /// Specifies should or not field with null values include into JSON.
    /// </summary>
    property IncludeNulls: Boolean read FIncludeNulls write FIncludeNulls default False;
    /// <summary>
    /// Event allowing to filter dataset records.
    /// </summary>
    property OnFilterRecord: TFilterRecordEvent read FOnFilterRecord write FOnFilterRecord;
  end;

implementation

uses
  System.SysUtils, System.Variants, System.DateUtils, System.Math, System.JSON.Types,
  System.Rtti;

{ TJSONToDataSetBridge }

const
  CArrayItem: string = 'Array';

constructor TJSONToDataSetBridge.Create(const AAdaptor: IAdaptor);
begin
  inherited Create;
  FAdaptor := AAdaptor;
  FFieldPaths := TDictionary<string, string>.Create;
  FStringFieldSize := 255;
  FObjectView := False;
  FMetaMergeMode := TJSONMetaMergeMode.Merge;
  FSampleObjects := MaxInt;
  FTypesMode := TJSONTypesMode.Rich;
end;

destructor TJSONToDataSetBridge.Destroy;
begin
  FFieldPaths.Free;
  inherited Destroy;
end;

function TJSONToDataSetBridge.GetDefaultFieldName(const AJSON: TJSONValue): string;
begin
  if Assigned(FAdaptor) then
    Result := FAdaptor.GetDefaultFieldName(AJSON)
  else if AJSON <> nil then
    Result := AJSON.ClassName.Substring(1)
  else
    Result := 'value';
end;

function TJSONToDataSetBridge.GetScanDepth: Integer;
begin
  if Assigned(FAdaptor) then
    Result := FAdaptor.GetScanDepth
  else
    Result := 1;
end;

procedure TJSONToDataSetBridge.Reset;
begin
  FFieldPaths.Clear;
  FFieldDefs.Clear;
  FPKFields := '';
end;

procedure TJSONToDataSetBridge.Define(AIter: TJSONIterator);
var
  LDefs: TFieldDefs;
  LDef: TFieldDef;
  I: Integer;
  LStr: string;
  LBool: Boolean;
  LInt64: Int64;
  LFloat: Extended;
  LDateTime: TDateTime;
  LDepth: Integer;

  function GetKey: String;
  begin
    if AIter.ParentType = TJsonToken.StartArray then
      Result := CArrayItem
    else if not ObjectView then
      Result := Copy(AIter.GetPath(LDepth), 1)
    else
      Result := AIter.Key;
    if Result = '' then
      Result := GetDefaultFieldName(nil);
  end;

  procedure DataTypeMismatch(ACurType, ANewType: TFieldType);
  begin
    DatabaseErrorFmt('Field [%s] data type mismatch. Current type [%s], new type [%s]',
      [AIter.Path, FieldTypeNames[ACurType], FieldTypeNames[ANewType]], Dataset);
  end;

  procedure FieldIsNotFound;
  begin
    DatabaseErrorFmt('Field [%s] is not found',
      [AIter.Path], Dataset);
  end;

  function AddFieldDef: TFieldDef;
  begin
    Result := LDefs.AddFieldDef;
    Result.Name := GetKey;
    FFieldPaths.Add(AIter.Path, Result.Name);
  end;

  procedure SetDataType(ADef: TFieldDef; ADataType: TFieldType); inline;
  begin
    if ADef.DataType <> ADataType then
      ADef.DataType := ADataType;
  end;

  procedure DefineSimple(ADataType: TFieldType; ASize: Integer);
  var
    LAdded: Boolean;
  begin
    if (LDef <> nil) and (LDef.DataType <> ftUnknown) and (LDef.DataType <> ADataType) and
       (MetaMergeMode = TJSONMetaMergeMode.AddOrError) then
      DataTypeMismatch(LDef.DataType, ADataType);
    LAdded := False;
    if LDef = nil then
      if MetaMergeMode = TJSONMetaMergeMode.Error then
        FieldIsNotFound
      else if MetaMergeMode in [TJSONMetaMergeMode.Merge, TJSONMetaMergeMode.Add, TJSONMetaMergeMode.AddOrError] then
      begin
        LDef := AddFieldDef;
        LAdded := True;
      end;
    if (LDef <> nil) and (LAdded or (MetaMergeMode in [TJSONMetaMergeMode.Merge, TJSONMetaMergeMode.Update])) then
    begin
      if ADataType in [ftString, ftWideString] then
        SetDataType(LDef, ADataType);
      case LDef.DataType of
        ftUnknown:
          SetDataType(LDef, ADataType);
        ftSmallint,
        ftInteger,
        ftWord,
        ftAutoInc,
        ftLargeint,
        ftLongWord,
        ftShortint,
        ftByte:
          if ADataType in [ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftSingle, ftExtended] then
            SetDataType(LDef, ADataType)
          else if not (ADataType in [ftSmallint, ftInteger, ftWord, ftAutoInc,
                                     ftLargeint, ftLongWord, ftShortint, ftByte]) then
            SetDataType(LDef, ftWideString);
        ftFloat,
        ftCurrency,
        ftBCD,
        ftFMTBcd,
        ftSingle,
        ftExtended:
          if not (ADataType in [ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint,
                                ftLongWord, ftShortint, ftByte, ftFloat, ftCurrency,
                                ftBCD, ftFMTBcd, ftSingle, ftExtended]) then
            SetDataType(LDef, ftWideString);
        ftDate,
        ftTime:
          if ADataType in [ftDateTime, ftTimeStamp, ftOraTimeStamp, ftTimeStampOffset] then
            SetDataType(LDef, ADataType)
          else if not (ADataType in [ftDate, ftTime]) then
            SetDataType(LDef, ftWideString);
        ftDateTime,
        ftTimeStamp,
        ftOraTimeStamp,
        ftTimeStampOffset:
          if not (ADataType in [ftDateTime, ftTimeStamp, ftOraTimeStamp, ftTimeStampOffset]) then
            SetDataType(LDef, ftWideString);
        ftBoolean:
          if not (ADataType in [ftBoolean]) then
            SetDataType(LDef, ftWideString);
        ftADT: ;
        ftDataSet: ;
      end;
      if LDef.DataType in [ftBytes, ftVarBytes, ftFixedChar, ftString, ftFixedWideChar, ftWideString] then
        if LAdded then
          LDef.Size := ASize
        else
          LDef.Size := Max(ASize, LDef.Size)
      else
        LDef.Size := 0;
      if ADataType in [ftFixedChar, ftFixedWideChar, ftBytes] then
        LDef.Attributes := LDef.Attributes + [faFixed];
    end;
  end;

  procedure DefineAdt(ADataType: TFieldType);
  begin
    if (LDef <> nil) and (LDef.DataType <> ADataType) then
      DataTypeMismatch(LDef.DataType, ADataType)
    else if LDef = nil then
    begin
      LDef := AddFieldDef;
      LDef.DataType := ADataType;
    end;
  end;

  procedure DefineUndefined(ADefs: TFieldDefs);
  var
    i: Integer;
    LDef: TFieldDef;
  begin
    for i := 0 to ADefs.Count - 1 do
    begin
      LDef := ADefs[i];
      if LDef.DataType = ftUnknown then
      begin
        LDef.DataType := ftWideString;
        LDef.Size := StringFieldSize;
      end
      else if LDef.DataType in [ftADT, ftDataSet] then
      begin
        if (LDef.DataType = ftDataSet) and (LDef.ChildDefs.Count = 0) then
          LDef.ChildDefs.Add(CArrayItem, ftUnknown);
        DefineUndefined(LDef.ChildDefs);
      end;
    end;
  end;

begin
  FPKFields := '';
  LDefs := FFieldDefs;
  LDepth := AIter.Depth;

  while True do begin
    while AIter.Next do
      if not ((LDepth < AIter.Depth) and (AIter.ParentType = TJsonToken.StartArray) and
              (AIter.&Index >= SampleObjects)) then
      begin
        I := LDefs.IndexOf(GetKey);
        if I = -1 then
          LDef := nil
        else
          LDef := LDefs[I];

        if (TypesMode = TJSONTypesMode.StringOnly) and not (AIter.&Type in [TJsonToken.StartObject, TJsonToken.StartArray]) then
        begin
          DefineSimple(ftWideString, StringFieldSize);
          Continue;
        end;

        case AIter.&Type of
          TJsonToken.StartObject:
            begin
              if ObjectView then
              begin
                DefineAdt(ftADT);
                LDefs := LDef.ChildDefs;
              end
              else
                DefineSimple(ftWideString, StringFieldSize);
              AIter.Recurse;
              if AIter.Depth - LDepth >= GetScanDepth then
              begin
                AIter.Return;
                if ObjectView then
                  LDefs := TFieldDefs(LDefs.ParentDef.Collection);
              end;
            end;
          TJsonToken.StartArray:
            begin
              if ObjectView then
              begin
                DefineAdt(ftDataSet);
                LDefs := LDef.ChildDefs;
              end
              else
                DefineSimple(ftWideString, StringFieldSize);
              AIter.Recurse;
              if not ObjectView or (AIter.Depth - LDepth >= GetScanDepth) then
              begin
                AIter.Return;
                if ObjectView then
                  LDefs := TFieldDefs(LDefs.ParentDef.Collection);
              end;
            end;
          TJsonToken.Raw,
          TJsonToken.Bytes:
            DefineSimple(ftVarBytes, Length(AIter.AsBytes));
          TJsonToken.Integer:
            DefineSimple(ftInteger, 11);
          TJsonToken.Float:
            DefineSimple(ftFloat, 18);
          TJsonToken.&String:
            begin
              LStr := AIter.AsString;
              if TypesMode = TJSONTypesMode.JSONOnly then
                DefineSimple(ftWideString, Length(LStr))
              else if (LDef <> nil) and (LDef.DataType in [ftString, ftWideString]) then
                DefineSimple(LDef.DataType, Length(LStr))
              else if TryStrToInt64(LStr, LInt64) then
                if (LInt64 >= -MaxInt - 1) and (LInt64 <= MaxInt) then
                  DefineSimple(ftInteger, 11)
                else
                  DefineSimple(ftLargeint, 20)
              else if TryStrToFloat(LStr, LFloat, JSONFormatSettings) then
                DefineSimple(ftFloat, 18)
              else if TryStrToBool(LStr, LBool) then
                DefineSimple(ftBoolean, 5)
              else if TryISO8601ToDate(LStr, LDateTime) then
                DefineSimple(ftDateTime, 27)
              else
                DefineSimple(ftWideString, Length(LStr));
            end;
          TJsonToken.Boolean:
            DefineSimple(ftBoolean, 5);
          TJsonToken.Date:
            DefineSimple(ftDateTime, 27);

          TJsonToken.Null,
          TJsonToken.Undefined,
          TJsonToken.MinKey,
          TJsonToken.MaxKey:
            DefineSimple(ftUnknown, 0);

          TJsonToken.Oid:
            begin
              DefineSimple(ftFixedWideChar, Length(AIter.AsOid.AsString));
              if (LDefs.ParentDef = nil) and not ((LDepth < AIter.Depth) and (AIter.ParentType = TJsonToken.StartArray)) then
                FPKFields := AIter.Key;
            end;
          TJsonToken.RegEx:
            DefineSimple(ftWideString, Length(AIter.AsRegEx.AsString));
          TJsonToken.DBRef:
            DefineSimple(ftWideString, Length(AIter.AsDBRef.AsString));
          TJsonToken.CodeWScope:
            DefineSimple(ftWideMemo, 0);
        end;
      end;

    if LDepth < AIter.Depth then
    begin
      AIter.Return;
      if ObjectView then
        LDefs := TFieldDefs(LDefs.ParentDef.Collection);
    end
    else
      Break;
  end;
  DefineUndefined(LDefs);
end;

class function TJSONToDataSetBridge.ContainsArrayOfObjects(AReader: TJSONReader): Boolean;
begin
  if AReader.TokenType = TJsonToken.None then
  begin
    Result := AReader.Read and (AReader.TokenType = TJsonToken.StartArray);
    if Result then
      Result := AReader.Read and (AReader.TokenType in [TJsonToken.StartObject, TJsonToken.EndArray]);
    AReader.Rewind;
  end
  else
    Result := AReader.TokenType = TJsonToken.StartArray;
end;

procedure TJSONToDataSetBridge.Define(AReader: TJSONReader);
var
  LIter: TJSONIterator;
  I: Integer;
  LRecurse: Boolean;
begin
  LIter := TJSONIterator.Create(AReader);
  try
    if not ContainsArrayOfObjects(AReader) then
      Define(LIter)
    else
      for I := 1 to SampleObjects do
      begin
        if not LIter.Next then
          Break;
        LRecurse := (LIter.&Type = TJsonToken.StartObject) and LIter.Recurse;
        Define(LIter);
        if LRecurse then
          LIter.Return;
      end;
  finally
    LIter.Free;
  end;
end;

procedure TJSONToDataSetBridge.Define(AJSON: TJSONValue);
var
  LRdr: TJsonObjectReader;
begin
  LRdr := TJsonObjectReader.Create(AJSON);
  try
    Define(LRdr);
  finally
    LRdr.Free;
  end;
end;

procedure TJSONToDataSetBridge.Append(AIter: TJSONIterator);
var
  LDataSet: TDataSet;
  LFields: TFields;
  LField: TField;
  LStack: TStack<TPair<TDataSet, TFields>>;
  LPair: TPair<TDataSet, TFields>;
  LExtended: Extended;
  LDateTime: TDateTime;
  LDepth: Integer;
  LBool: Boolean;

  function GetKey: String;
  begin
    if not ObjectView then
      Result := AIter.GetPath(LDepth)
    else
      if AIter.ParentType = TJsonToken.StartArray then
        Result := CArrayItem
      else
        Result := AIter.Key;
    if Result = '' then
      Result := GetDefaultFieldName(nil);
  end;

begin
  LStack := TStack<TPair<TDataSet, TFields>>.Create;
  try
    LDepth := AIter.Depth;
    LDataSet := Dataset;
    LFields := LDataSet.Fields;
    LDataSet.Append;
    while True do
    begin
      while AIter.Next do
      begin
        if (LDepth < AIter.Depth) and (AIter.ParentType = TJsonToken.StartArray) then
        begin
          LDataSet.Append;
          if LFields.Count > 0 then
            LField := LFields[0]
          else
            LField := nil;
        end
        else
          LField := LFields.FindField(GetKey);
        if LField <> nil then
          case AIter.&Type of
            TJsonToken.StartObject:
              if ObjectView and (LField is TADTField) then
              begin
                LStack.Push(TPair<TDataSet, TFields>.Create(LDataSet, LFields));
                LFields := TADTField(LField).Fields;
                AIter.Recurse;
              end
              else
              begin
                if LField.DataType in [ftFixedChar, ftString, ftFixedWideChar, ftWideString, ftMemo, ftWideMemo] then
                begin
                  if AIter.Reader is TJsonObjectReader then
                    LField.AsWideString := TJsonObjectReader(AIter.Reader).Current.ToString;
                end;
                AIter.Recurse;
                if ObjectView then
                  AIter.Return;
              end;
            TJsonToken.StartArray:
              if ObjectView and (LField is TDataSetField) then
              begin
                LStack.Push(TPair<TDataSet, TFields>.Create(LDataSet, LFields));
                LDataSet := TDataSetField(LField).NestedDataSet;
                LFields := LDataSet.Fields;
                LDataSet.Append;
                AIter.Recurse;
              end
              else
              begin
                if LField.DataType in [ftFixedChar, ftString, ftFixedWideChar, ftWideString, ftMemo, ftWideMemo] then
                begin
                  if AIter.Reader is TJsonObjectReader then
                    LField.AsWideString := TJsonObjectReader(AIter.Reader).Current.ToString;
                end;
                AIter.Recurse;
                AIter.Return;
              end;

            TJsonToken.Raw,
            TJsonToken.Bytes:
              LField.AsBytes := AIter.AsBytes;
            TJsonToken.Integer:
              if LField.DataType = ftBoolean then
                LField.AsBoolean := AIter.AsInteger <> 0
              else
                LField.AsInteger := AIter.AsInteger;
            TJsonToken.Float:
              if LField.DataType = ftBoolean then
                LField.AsBoolean := SameValue(AIter.AsDouble, 0)
              else
                LField.AsFloat := AIter.AsDouble;
            TJsonToken.&String:
              if (LField.DataType in [ftSingle, ftFloat, ftExtended, ftBCD, ftCurrency, ftFMTBcd]) and
                 TryStrToFloat(AIter.AsString, LExtended, JSONFormatSettings) then
                LField.AsExtended := LExtended
              else if (LField.DataType = ftBoolean) and
                      TryStrToBool(AIter.AsString, LBool) then
                LField.AsBoolean := LBool
              else if (LField.DataType = ftTime) and
                      TryStrToTime(AIter.AsString, LDateTime, JSONFormatSettings) then
                  LField.AsDateTime := LDateTime
              else if (LField.DataType = ftDate) and
                      TryStrToDate(AIter.AsString, LDateTime, JSONFormatSettings) then
                  LField.AsDateTime := LDateTime
              else if (LField.DataType in [ftDateTime, ftTime, ftDate, ftTimeStamp, ftTimeStampOffset]) and
                      TryISO8601ToDate(AIter.AsString, LDateTime) then
                if LField.DataType = ftTime then
                  LField.AsDateTime := TimeOf(LDateTime)
                else if LField.DataType = ftDate then
                  LField.AsDateTime := DateOf(LDateTime)
                else
                  LField.AsDateTime := LDateTime
              else
                LField.AsWideString := AIter.AsString;
            TJsonToken.Boolean:
              LField.AsBoolean := AIter.AsBoolean;
            TJsonToken.Date:
              LField.AsDateTime := AIter.AsDateTime;

            TJsonToken.Null,
            TJsonToken.Undefined:
              LField.Clear;
            TJsonToken.MinKey:
              if LField.DataType in [ftBlob, ftMemo, ftWideMemo, ftBytes, ftVarBytes, ftString, ftWideString] then
                LField.AsString := '<MINKEY>'
              else
                LField.Clear;
            TJsonToken.MaxKey:
              if LField.DataType in [ftBlob, ftMemo, ftWideMemo, ftBytes, ftVarBytes, ftString, ftWideString] then
                LField.AsString := '<MAXKEY>'
              else
                LField.Clear;

            TJsonToken.Oid:
              LField.AsWideString := AIter.AsOid.AsString;
            TJsonToken.RegEx:
              LField.AsWideString := AIter.AsRegEx.AsString;
            TJsonToken.DBRef:
              LField.AsWideString := AIter.AsDBRef.AsString;
            TJsonToken.CodeWScope:
              LField.AsWideString := AIter.AsCodeWScope.Code;
          end;
      end;

      if LDepth < AIter.Depth then
      begin
        AIter.Return;
        if ObjectView then
        begin
          LPair := LStack.Pop;
          LDataSet := LPair.Key;
          LFields := LPair.Value;
        end;
      end
      else
        Break;
    end;
  finally
    LStack.Free;
  end;
  if DataSet.State in dsEditModes then
    Dataset.Post;
end;

procedure TJSONToDataSetBridge.Append(AReader: TJSONReader);
var
  LIter: TJSONIterator;
  LRecurse: Boolean;
begin
  LIter := TJSONIterator.Create(AReader);
  try
    if not ContainsArrayOfObjects(AReader) then
      Append(LIter)
    else
      while LIter.Next do
      begin
        LRecurse := (LIter.&Type = TJsonToken.StartObject) and LIter.Recurse;
        Append(LIter);
        if LRecurse then
          LIter.Return;
      end;
  finally
    LIter.Free;
  end;
end;

procedure TJSONToDataSetBridge.Append(AValue: TJSONValue);
var
  LRdr: TJsonObjectReader;
begin
  LRdr := TJsonObjectReader.Create(AValue);
  try
    Append(LRdr);
  finally
    LRdr.Free;
  end;
end;

{ TDataSetToJSONBridge }

constructor TDataSetToJSONBridge.Create;
begin
  inherited Create;
  FFieldNames := TStringList.Create(dupAccept, False, False);
  FArea := TJSONDataSetArea.All;
end;

destructor TDataSetToJSONBridge.Destroy;
begin
  FreeAndNil(FFieldNames);
  inherited Destroy;
end;

procedure TDataSetToJSONBridge.WriteFieldValues(AFields: TFields; APairs: TJSONCollectionBuilder.TPairs);
var
  i: Integer;
  LFld: TField;
  LElems: TJSONCollectionBuilder.TElements;
  LPairs: TJSONCollectionBuilder.TPairs;
  LName: string;
  j: Integer;
begin
  for i := 0 to AFields.Count - 1 do begin
    LFld := AFields[i];

    if FieldNames.Count > 0 then
    begin
      LName := LFld.FullName;
      j := FieldNames.IndexOfName(LName);
      if j >= 0 then
        LName := FieldNames.ValueFromIndex[j]
      else
      begin
        j := FieldNames.IndexOf(LName);
        if j >= 0 then
          LName := LFld.FieldName
        else
          Continue;
      end;
    end
    else
      LName := LFld.FieldName;

    if IncludeNulls or not LFld.IsNull then
      case LFld.DataType of
      ftADT:
        begin
          LPairs := APairs.BeginObject(LName);
          WriteFieldValues(TADTField(LFld).Fields, LPairs);
          LPairs.EndObject;
        end;
      ftDataSet:
        begin
          LElems := APairs.BeginArray(LName);
          WriteDataSet(TDataSetField(LFld).NestedDataSet, LElems, TJSONDataSetArea.All);
          LElems.EndArray;
        end;
      else
        APairs.Add(LName, LFld.Value);
      end;
  end;
end;

function TDataSetToJSONBridge.DoFilterRecord(ADataSet: TDataSet): Boolean;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
    OnFilterRecord(ADataSet, Result);
end;

procedure TDataSetToJSONBridge.WriteDataSet(ADataSet: TDataSet;
  AElems: TJSONCollectionBuilder.TElements; AArea: TJSONDataSetArea);
var
  LPairs: TJSONCollectionBuilder.TPairs;
  LBmk: TBookmark;
begin
  ADataset.DisableControls;
  if not DataSet.IsUniDirectional then
    LBmk := DataSet.Bookmark;
  try
    if AArea = TJSONDataSetArea.All then
      ADataset.First;
    while not ADataset.Eof do
    begin
      if DoFilterRecord(ADataSet) then
      begin
        LPairs := AElems.BeginObject;
        try
          WriteFieldValues(ADataSet.Fields, LPairs);
        finally
          LPairs.EndObject;
        end;
      end;
      ADataset.Next;
    end;
  finally
    if not DataSet.IsUniDirectional then
      DataSet.Bookmark := LBmk;
    ADataset.EnableControls;
  end;
end;

procedure TDataSetToJSONBridge.Produce(AWriter: TJSONWriter);
var
  LArrBldr: TJSONArrayBuilder;
  LObjBldr: TJSONObjectBuilder;
  LElems: TJSONCollectionBuilder.TElements;
  LPairs: TJSONCollectionBuilder.TPairs;
begin
  Dataset.Active := True;

  case Area of
    TJSONDataSetArea.All,
    TJSONDataSetArea.AllFromCurrent:
      begin
        LArrBldr := TJSONArrayBuilder.Create(AWriter);
        LElems := LArrBldr.BeginArray;
        try
          WriteDataSet(Dataset, LElems, Area);
        finally
          LElems.EndArray;
          LArrBldr.Free;
        end;
      end;
    TJSONDataSetArea.Current:
      begin
        LObjBldr := TJSONObjectBuilder.Create(AWriter);
        LPairs := LObjBldr.BeginObject;
        try
          WriteFieldValues(DataSet.Fields, LPairs);
        finally
          LPairs.EndObject;
          LObjBldr.Free;
        end;
      end;
    TJSONDataSetArea.CurrentAsArray:
      begin
        LArrBldr := TJSONArrayBuilder.Create(AWriter);
        LElems := LArrBldr.BeginArray;
        try
          if DoFilterRecord(DataSet) then
          begin
            LPairs := LElems.BeginObject;
            try
              WriteFieldValues(DataSet.Fields, LPairs);
            finally
              LPairs.EndObject;
            end;
          end;
        finally
          LElems.EndArray;
          LArrBldr.Free;
        end;
      end;
  end;
end;

function TDataSetToJSONBridge.Produce: TJSONAncestor;
var
  LWriter: TJsonObjectWriter;
begin
  LWriter := TJsonObjectWriter.Create(False);
  try
    Produce(LWriter);
    Result := LWriter.JSON;
  finally
    LWriter.Free;
  end;
end;

end.
