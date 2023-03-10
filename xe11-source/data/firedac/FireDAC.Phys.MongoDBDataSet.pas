{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{               FireDAC MongoDB datasets                }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$SCOPEDENUMS ON}

                  
                                                                           
                                
                                  
                                   
                         
                            
                                                   
                                           
                                                      
                                              
                            
                      
 

unit FireDAC.Phys.MongoDBDataSet;

interface

uses
  System.Classes, System.Generics.Collections, System.JSON.Types, System.JSON,
    Data.DB, Data.DBJson,
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.DatS,
  FireDAC.Phys.MongoDBWrapper, FireDAC.Phys.MongoDB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TFDMongoCustomMemTable = class;
  TFDMongoCustomDataSet = class;
  TFDMongoDataSet = class;
  TFDMongoQuery = class;
  TFDMongoPipeline = class;

  /// <summary> An action when a fetched string value is greater than TField.Size:
  ///  * Strict - exception will raised
  ///  * Clip - the value will be clipped
  ///  * Full - the value will be fetched in full
  /// </summary>
  TFDMongoStringsMode = (Strict, Clip, Full);

  /// <summary> An internal base class for all MongoDB dataset classes. </summary>
  TFDMongoCustomMemTable = class(TFDCustomMemTable)
  protected
    // TDataSet
    procedure DataConvert(Field: TField; Source: TValueBuffer; var Dest: TValueBuffer; ToNative: Boolean); override;
{$IFNDEF NEXTGEN}
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
{$ENDIF !NEXTGEN}
  public
    constructor Create(AOwner: TComponent); override;
  end;

  /// <summary> A base class for all MongoDB dataset classes. This is an
  ///  abstract class, responsible for dataset structure defining, documents
  ///  fetching, updates applying. This class is not capable to provide a
  ///  MongoDB cursor. Descendant classes are responsible for that. To enable
  ///  updates an application should specify Connection, DatabaseName,
  ///  CollecionName properties. </summary>
  TFDMongoCustomDataSet = class (TFDMongoCustomMemTable, TJSONToDataSetBridge.IAdaptor)
  private
    FConnection: TFDCustomConnection;
    FMongoConnection: TMongoConnection;
    FCollectionName: String;
    FDatabaseName: String;
    FCursor: IMongoCursor;
    FExternalCursor: Boolean;
    FPrefetchedDocs: TObjectList<TMongoDocument>;
    FRowsFetched: Integer;
    FBridge: TJSONToDataSetBridge;
    procedure SetConnection(const AValue: TFDCustomConnection);
    procedure CheckConnectionActive;
    procedure CheckConnection;
    procedure CheckNames;
    procedure SetCollectionName(const AValue: String);
    procedure SetDatabaseName(const AValue: String);
    procedure SetCursor(const AValue: IMongoCursor);
    class function IsOidField(AField: TField; out AOid: TJsonOid): Boolean;
    procedure WriteFieldValues(AFields: TFields; ADoc: TMongoDocument);
    procedure BuildInsertDoc(AIns: TMongoInsert.TExpression);
    procedure BuildModifyDoc(AMod: TMongoUpdate.TModifier);
    procedure DefineFromDoc(ADoc: TMongoDocument; ASampleDocs: Integer);
    procedure FetchFromDoc(ADoc: TMongoDocument);
    procedure ApplyToDoc(ARequest: TFDUpdateRequest; AColl: TMongoCollection);
    procedure DoConnectChanged(Sender: TObject; Connecting: Boolean);
    function GetCollection: TMongoCollection;
    function GetStringsMode: TFDMongoStringsMode;
    procedure SetStringsMode(AValue: TFDMongoStringsMode);
    function GetMetaMergeMode: TFDMergeMetaMode;
    procedure SetMetaMergeMode(const AValue: TFDMergeMetaMode);
  protected
    // TJSONToDataSetBridge.IAdaptor
    function GetDefaultFieldName(const AJSON: TJSONValue): string;
    function GetScanDepth: Integer;
    // TComponent
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    // TDataSet
    function GetCanRefresh: Boolean; override;
    function GetCanModify: Boolean; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure InternalClose; override;
    // TFDDataSet
    procedure DoDefineDatSManager; override;
    procedure DoOpenSource(ABlocked, AInfoQuery, AStructQuery: Boolean); override;
    function DoFetch(ATable: TFDDatSTable; AAll: Boolean;
      ADirection: TFDFetchDirection): Integer; override;
    procedure DoCloseSource; override;
    procedure DoResetDatSManager; override;
    procedure DoUpdateRecordHandler(ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
      AOptions: TFDUpdateRowOptions; var AAction: TFDErrorAction); override;
    // TFDMongoCustomDataSet
    function GetCursor(ALimit, ASkip: Integer): IMongoCursor; virtual;
    /// <summary> Specifies the MongoDB cursor. TFDMongoDataSet allows to assign
    ///  a custom cursor obtained from MongoDB API wrapping classes. Other datasets,
    ///  like TFDMongoQuery, automatically create a cursor. </summary>
    property Cursor: IMongoCursor read FCursor write SetCursor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ServerDeleteAll(ANoUndo: Boolean = False); override;
    /// <summary> Returns reference to MongoDB collection object specified by
    ///  DatabaseName and CollectionName properties. </summary>
    property Collection: TMongoCollection read GetCollection;
    /// <summary> Specifies the MongoDB connection. </summary>
    property Connection: TFDCustomConnection read FConnection write SetConnection;
    /// <summary> Specifies the MongoDB database name. </summary>
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    /// <summary> Specifies the MongoDB collection name. </summary>
    property CollectionName: String read FCollectionName write SetCollectionName;
    /// <summary> Specifies the column definitions merging mode. </summary>
    property MetaMergeMode: TFDMergeMetaMode read GetMetaMergeMode
      write SetMetaMergeMode default mmMerge;
    /// <summary> Specifies an action when a fetched string value is greater
    ///  than TField.Size. </summary>
    property StringsMode: TFDMongoStringsMode read GetStringsMode
      write SetStringsMode stored False default TFDMongoStringsMode.Strict;
  end;

  /// <summary> TFDMongoDataSet class allows to attach the dataset to existing
  ///  MongoDB cursor. The cursor may be get from different MongoDB API wrapping
  ///  class methods, for example TFDMongoDatabase.ListCollections. To open
  ///  dataset assign a cursor to TFDMongoDataSet.Cursor property and call Open
  ///  method. To enable updates an application should specify Connection,
  ///  DatabaseName, CollecionName properties. </summary>
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]
  TFDMongoDataSet = class (TFDMongoCustomDataSet)
  public
    property Cursor;
  published
    property ActiveStoredUsage;
    { TDataSet }
    property Active;
    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property FieldOptions;
    property Filtered;
    property FilterOptions;
    property Filter;
    property OnFilterRecord;
    property ObjectView default True;
    property Constraints;
    { TFDDataSet }
    property CachedUpdates;
    property FilterChanges;
    property Indexes;
    property IndexesActive;
    property IndexName;
    property IndexFieldNames;
    property Aggregates;
    property AggregatesActive;
    property ConstraintsEnabled;
    property MasterSource;
    property MasterFields;
    property DetailFields;
    property OnUpdateRecord;
    property OnUpdateError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property AfterGetRecord;
    property FetchOptions;
    property FormatOptions;
    property ResourceOptions;
    property UpdateOptions;
    { TFDAdaptedDataSet }
    property LocalSQL;
    { TFDMongoCustomDataSet }
    property Connection;
    property DatabaseName;
    property CollectionName;
    property MetaMergeMode;
    property StringsMode;
  end;

  /// <summary> TFDMongoQuery class allows execute a MongoDB Query. The query
  ///  may be specified using QProject, QMatch, QSort properties at design-time
  ///  or run-time. Or using Query Builder provided by Query property at run-time.
  ///  To execute a query the Connection, DatabaseName, CollecionName properties
  ///  must be specified. </summary>
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]
  TFDMongoQuery = class (TFDMongoCustomDataSet)
  private
    FQuery: TMongoQuery;
    FQProject: String;
    FQMatch: String;
    FQSort: String;
    function GetQuery: TMongoQuery;
    procedure SetQProject(const AValue: String);
    procedure SetQMatch(const AValue: String);
    procedure SetQSort(const AValue: String);
    procedure PrepareQuery(ALimit, ASkip: Integer);
  protected
    // TFDDataSet
    procedure UpdateRecordCount; override;
    // TFDMongoCustomDataSet
    function GetCursor(ALimit, ASkip: Integer): IMongoCursor; override;
  public
    destructor Destroy; override;
    /// <summary> Returns a reference to MongoDB Query builder. An application
    ///  may use Query builder at run-time to compose a MongoDB query. Other options
    ///  are to use QProject, QMatch, QSort properties at design-time or run-time.
    ///  Finally, application may use both options. </summary>
    property Query: TMongoQuery read GetQuery;
  published
    property ActiveStoredUsage;
    { TDataSet }
    property Active;
    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property FieldOptions;
    property Filtered;
    property FilterOptions;
    property Filter;
    property OnFilterRecord;
    property ObjectView default True;
    property Constraints;
    { TFDDataSet }
    property CachedUpdates;
    property FilterChanges;
    property Indexes;
    property IndexesActive;
    property IndexName;
    property IndexFieldNames;
    property Aggregates;
    property AggregatesActive;
    property ConstraintsEnabled;
    property MasterSource;
    property MasterFields;
    property DetailFields;
    property OnUpdateRecord;
    property OnUpdateError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property AfterGetRecord;
    property FetchOptions;
    property FormatOptions;
    property ResourceOptions;
    property UpdateOptions;
    { TFDAdaptedDataSet }
    property LocalSQL;
    { TFDMongoCustomDataSet }
    property Connection;
    property DatabaseName;
    property CollectionName;
    property MetaMergeMode;
    property StringsMode;
    /// <summary> Specifies the MongoDB Query projection JSON. </summary>
    property QProject: String read FQProject write SetQProject;
    /// <summary> Specifies the MongoDB Query match JSON. </summary>
    property QMatch: String read FQMatch write SetQMatch;
    /// <summary> Specifies the MongoDB Query sort JSON. </summary>
    property QSort: String read FQSort write SetQSort;
  end;

  /// <summary> TFDMongoPipeline class allows execute a MongoDB Pipeline. The
  ///  pipeline may be specified using PProject, PMatch, PRedact, PUnwind, PGroup,
  ///  PSort properties at design-time or run-time. Or using Pipeline Builder
  ///  provided by Pipeline property at run-time. To execute a pipeline the
  ///  Connection, DatabaseName, CollecionName properties must be specified. </summary>
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]
  TFDMongoPipeline = class (TFDMongoCustomDataSet)
  private
    FPipeline: TMongoPipeline;
    FPUnwind: String;
    FPSort: String;
    FPMatch: String;
    FPGroup: String;
    FPRedact: String;
    FPProject: String;
    function GetPipeline: TMongoPipeline;
    procedure SetPProject(const AValue: String);
    procedure SetPMatch(const AValue: String);
    procedure SetPRedact(const AValue: String);
    procedure SetPUnwind(const AValue: String);
    procedure SetPGroup(const AValue: String);
    procedure SetPSort(const AValue: String);
    procedure PreparePipeline(ALimit, ASkip: Integer);
  protected
    // TFDMongoCustomDataSet
    function GetCursor(ALimit, ASkip: Integer): IMongoCursor; override;
  public
    destructor Destroy; override;
    /// <summary> Returns a reference to MongoDB Pipeline builder. An application
    ///  may use Pipeline builder at run-time to compose a MongoDB pipeline. Other
    ///  options are to use PProject, PMatch, etc properties at design-time or
    ///  run-time. Finally, application may use both options. </summary>
    property Pipeline: TMongoPipeline read GetPipeline;
  published
    property ActiveStoredUsage;
    { TDataSet }
    property Active;
    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
    property FieldOptions;
    property Filtered;
    property FilterOptions;
    property Filter;
    property OnFilterRecord;
    property ObjectView default True;
    property Constraints;
    { TFDDataSet }
    property CachedUpdates;
    property FilterChanges;
    property Indexes;
    property IndexesActive;
    property IndexName;
    property IndexFieldNames;
    property Aggregates;
    property AggregatesActive;
    property ConstraintsEnabled;
    property MasterSource;
    property MasterFields;
    property DetailFields;
    property OnUpdateRecord;
    property OnUpdateError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property AfterGetRecord;
    property FetchOptions;
    property FormatOptions;
    property ResourceOptions;
    property UpdateOptions;
    { TFDAdaptedDataSet }
    property LocalSQL;
    { TFDMongoCustomDataSet }
    property Connection;
    property DatabaseName;
    property CollectionName;
    property MetaMergeMode;
    property StringsMode;
    /// <summary> Specifies the MongoDB Pipeline project JSON. </summary>
    property PProject: String read FPProject write SetPProject;
    /// <summary> Specifies the MongoDB Pipeline match JSON. </summary>
    property PMatch: String read FPMatch write SetPMatch;
    /// <summary> Specifies the MongoDB Pipeline redact JSON. </summary>
    property PRedact: String read FPRedact write SetPRedact;
    /// <summary> Specifies the MongoDB Pipeline unwind JSON. </summary>
    property PUnwind: String read FPUnwind write SetPUnwind;
    /// <summary> Specifies the MongoDB Pipeline group JSON. </summary>
    property PGroup: String read FPGroup write SetPGroup;
    /// <summary> Specifies the MongoDB Pipeline sort JSON. </summary>
    property PSort: String read FPSort write SetPSort;
  end;

implementation

uses
  System.Variants, System.SysUtils, System.Math, System.Rtti,
{$IFNDEF NEXTGEN}
    System.AnsiStrings,
{$ENDIF}
  System.JSON.Readers, System.JSON.Writers, System.JSON.Builders,
  FireDAC.Stan.Util, FireDAC.Stan.Error, FireDAC.Stan.Consts,
    FireDAC.Stan.ResStrs;

type
  __TFDConn = class (TFDCustomConnection)
  end;

{-------------------------------------------------------------------------------}
{ TFDMongoCustomMemTable                                                        }
{-------------------------------------------------------------------------------}
constructor TFDMongoCustomMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NestedDataSetClass := TFDMongoCustomMemTable;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomMemTable.DataConvert(Field: TField; Source: TValueBuffer;
  var Dest: TValueBuffer; ToNative: Boolean);
var
  iLen: Integer;
begin
  case Field.DataType of
    ftWideString, ftFixedWideChar:
      begin
        if FormatOptions.StrsTrim2Len then
          iLen := Field.Size + 1
        else
          iLen := (IOBufferSize - 1) div SizeOf(WideChar);
        StrLCopy(PChar(Dest), PChar(Source), iLen);
      end;
{$IFNDEF NEXTGEN}
    ftString, ftFixedChar:
      begin
        if FormatOptions.StrsTrim2Len then
          iLen := Field.Size + 1
        else
          iLen := IOBufferSize - 1;
        System.AnsiStrings.StrLCopy(PAnsiChar(Dest), PAnsiChar(Source), iLen);
      end;
{$ENDIF}
    else
      inherited DataConvert(Field, Source, Dest, ToNative);
  end;
end;

{-------------------------------------------------------------------------------}
{$IFNDEF NEXTGEN}
{$WARNINGS OFF}
procedure TFDMongoCustomMemTable.DataConvert(Field: TField; Source,
  Dest: Pointer; ToNative: Boolean);
var
  iLen: Integer;
begin
  case Field.DataType of
    ftWideString, ftFixedWideChar:
      begin
        if FormatOptions.StrsTrim2Len then
          iLen := Field.Size + 1
        else
          iLen := (IOBufferSize - 1) div SizeOf(WideChar);
        StrLCopy(PChar(Dest), PChar(Source), iLen);
      end;
    ftString, ftFixedChar:
      begin
        if FormatOptions.StrsTrim2Len then
          iLen := Field.Size + 1
        else
          iLen := IOBufferSize - 1;
        System.AnsiStrings.StrLCopy(PAnsiChar(Dest), PAnsiChar(Source), iLen);
      end;
    else
      inherited DataConvert(Field, Source, Dest, ToNative);
  end;
end;
{$WARNINGS ON}
{$ENDIF}

{-------------------------------------------------------------------------------}
{ TFDMongoCustomDataSet                                                         }
{-------------------------------------------------------------------------------}
constructor TFDMongoCustomDataSet.Create(AOwner: TComponent);
var
  oConn: TFDCustomConnection;
begin
  inherited Create(AOwner);
  FetchOptions.RestoreDefaults;
  ResourceOptions.RestoreDefaults;
  UpdateOptions.RestoreDefaults;
  FormatOptions.RestoreDefaults;
  FPrefetchedDocs := TObjectList<TMongoDocument>.Create(True);
  FBridge := TJSONToDataSetBridge.Create(Self);
  FBridge.Dataset := Self;
  FBridge.FieldDefs := FieldDefs;
  FBridge.ObjectView := True;
  FBridge.MetaMergeMode := TJSONMetaMergeMode.Merge;
  FBridge.TypesMode := TJSONTypesMode.JSONOnly;
  if FDIsDesigning(Self) then begin
    oConn := FDFindDefaultConnection(Self);
    if (oConn <> nil) and
       ((oConn.RDBMSKind = TFDRDBMSKinds.Unknown) or (oConn.RDBMSKind = TFDRDBMSKinds.MongoDB)) then
      Connection := oConn;
  end;
end;

{-------------------------------------------------------------------------------}
destructor TFDMongoCustomDataSet.Destroy;
begin
  Destroying;
  Disconnect(True);
  Connection := nil;
  FDFreeAndNil(FBridge);
  inherited Destroy;
  FDFreeAndNil(FPrefetchedDocs);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
    if AComponent = Connection then
      Connection := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.SetConnection(const AValue: TFDCustomConnection);
begin
  if Connection <> AValue then begin
    if Connection <> nil then begin
      CheckInactive;
      __TFDConn(Connection).UnRegisterClient(Self);
    end;
    FConnection := AValue;
    FieldDefs.Clear;
    FieldDefs.Updated := False;
    if Connection <> nil then begin
      Connection.FreeNotification(Self);
      __TFDConn(Connection).RegisterClient(Self, DoConnectChanged);
      if Connection.Connected then
        DoConnectChanged(nil, True);
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.CheckConnectionActive;
begin
  CheckConnection;
  Connection.CheckActive;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.CheckConnection;
begin
  if (Connection = nil) or (Connection.RDBMSKind <> TFDRDBMSKinds.MongoDB) then
    FDException(Self, [S_FD_LComp, S_FD_MongoId], er_FD_MongoCannotOpenDataSet,
      [GetDisplayName, 'Connection']);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.CheckNames;
begin
  if DatabaseName = '' then
    FDException(Self, [S_FD_LComp, S_FD_MongoId], er_FD_MongoCannotOpenDataSet,
      [GetDisplayName, 'DatabaseName']);
  if CollectionName = '' then
    FDException(Self, [S_FD_LComp, S_FD_MongoId], er_FD_MongoCannotOpenDataSet,
      [GetDisplayName, 'CollectionName']);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.DoConnectChanged(Sender: TObject; Connecting: Boolean);
begin
  if Connecting then
    FMongoConnection := TMongoConnection(Connection.CliObj)
  else
    FMongoConnection := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.SetDatabaseName(const AValue: String);
begin
  if AnsiCompareText(FDatabaseName, AValue) <> 0 then begin
    CheckInactive;
    FDatabaseName := AValue;
    FieldDefs.Clear;
    FieldDefs.Updated := False;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.SetCollectionName(const AValue: String);
begin
  if AnsiCompareText(FCollectionName, AValue) <> 0 then begin
    CheckInactive;
    FCollectionName := AValue;
    FieldDefs.Clear;
    FieldDefs.Updated := False;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.SetCursor(const AValue: IMongoCursor);
begin
  if Cursor <> AValue then begin
    CheckInactive;
    FCursor := AValue;
    FExternalCursor := Cursor <> nil;
    FieldDefs.Clear;
    FieldDefs.Updated := False;
  end;
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoCustomDataSet.GetCollection: TMongoCollection;
begin
  CheckConnectionActive;
  CheckNames;
  Result := FMongoConnection[DatabaseName][CollectionName];
  if Connection.ConnectionIntf.Transaction.Active then
    Result.Session := TMongoSession(Connection.ConnectionIntf.Transaction.CliObj)
  else
    Result.Session := nil;
end;

{-------------------------------------------------------------------------------}
function TFDMongoCustomDataSet.GetStringsMode: TFDMongoStringsMode;
begin
  if not FormatOptions.StrsTrim2Len and (FormatOptions.InlineDataSize > 0) then
    Result := TFDMongoStringsMode.Strict
  else if not FormatOptions.StrsTrim2Len and (FormatOptions.InlineDataSize = 0) then
    Result := TFDMongoStringsMode.Full
  else if FormatOptions.StrsTrim2Len then
    Result := TFDMongoStringsMode.Clip
  else
    Result := TFDMongoStringsMode.Strict;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.SetStringsMode(AValue: TFDMongoStringsMode);
begin
  case AValue of
    TFDMongoStringsMode.Strict:
      begin
        FormatOptions.StrsTrim2Len := False;
        FormatOptions.InlineDataSize := C_FD_DefInlineDataSize;
      end;
    TFDMongoStringsMode.Clip:
      begin
        FormatOptions.StrsTrim2Len := True;
        FormatOptions.InlineDataSize := C_FD_DefInlineDataSize;
      end;
    TFDMongoStringsMode.Full:
      begin
        FormatOptions.StrsTrim2Len := False;
        FormatOptions.InlineDataSize := 0;
      end;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDMongoCustomDataSet.GetMetaMergeMode: TFDMergeMetaMode;
begin
  Result := TFDMergeMetaMode(FBridge.MetaMergeMode);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.SetMetaMergeMode(const AValue: TFDMergeMetaMode);
begin
  FBridge.MetaMergeMode := TJSONMetaMergeMode(AValue);
end;

{-------------------------------------------------------------------------------}
function TFDMongoCustomDataSet.GetScanDepth: Integer;
begin
  Result := MaxInt;
end;

{-------------------------------------------------------------------------------}
function TFDMongoCustomDataSet.GetDefaultFieldName(const AJSON: TJSONValue): string;
begin
  Result := AJSON.ClassName.Substring(1);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.DefineFromDoc(ADoc: TMongoDocument; ASampleDocs: Integer);
var
  oIter: TJSONIterator;
begin
  oIter := ADoc.Iterator;
  try
    FBridge.SampleObjects := ASampleDocs;
    FBridge.Define(oIter);
    if FBridge.PKFields <> '' then
      UpdateOptions.KeyFields := FBridge.PKFields;
  finally
    oIter.Free;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.FetchFromDoc(ADoc: TMongoDocument);
var
  oIter: TJSONIterator;
  oTab: TFDDatSTable;
  oNestTab: TFDDatSTable;
  oParentRow: TFDDatSRow;
  oRow: TFDDatSRow;
  iCol: Integer;
  oStack: TStack<TFDDatSTable>;

  function AddRow: TFDDatSRow;
  begin
    Result := oTab.NewRow(True);
    oTab.Rows.Add(Result);
    if oParentRow <> nil then
      Result.ParentRow := oParentRow;
  end;

begin
  oStack := TStack<TFDDatSTable>.Create;
  try
    oStack.Push(oTab);
    oTab := Table;
    oParentRow := nil;
    oRow := AddRow;
    oIter := ADoc.Iterator;
    try
      while True do begin
        while oIter.Next do begin
          if oIter.ParentType = TJsonToken.StartArray then begin
            oRow := AddRow;
            iCol := 0;
          end
          else
            iCol := oTab.Columns.IndexOfName(oIter.Key);
          if iCol <> -1 then
            case oIter.&Type of
              TJsonToken.StartObject,
              TJsonToken.StartArray:
                begin
                  oNestTab := oTab.Columns[iCol].NestedTable;
                  if oNestTab = nil then begin
                    oIter.Recurse;
                    oIter.Return;
                  end
                  else begin
                    oStack.Push(oTab);
                    oTab := oNestTab;
                    oParentRow := oRow;
                    if oIter.&Type = TJsonToken.StartObject then
                      oRow := AddRow;
                    oIter.Recurse;
                  end;
                end;

              TJsonToken.Raw,
              TJsonToken.Bytes:
                oRow.SetData(iCol, oIter.AsBytes);
              TJsonToken.Integer:
                oRow.SetData(iCol, oIter.AsInteger);
              TJsonToken.Float:
                oRow.SetData(iCol, oIter.AsDouble);
              TJsonToken.&String:
                oRow.SetData(iCol, oIter.AsString);
              TJsonToken.Boolean:
                oRow.SetData(iCol, oIter.AsBoolean);
              TJsonToken.Date:
                oRow.SetData(iCol, oIter.AsDateTime);

              TJsonToken.Null,
              TJsonToken.Undefined:
                oRow.SetData(iCol, Null);
              TJsonToken.MinKey:
                if oTab.Columns[iCol].DataType in C_FD_VarLenTypes then
                  oRow.SetData(iCol, '<MINKEY>')
                else
                  oRow.SetData(iCol, Null);
              TJsonToken.MaxKey:
                if oTab.Columns[iCol].DataType in C_FD_VarLenTypes then
                  oRow.SetData(iCol, '<MAXKEY>')
                else
                  oRow.SetData(iCol, Null);

              TJsonToken.Oid:
                oRow.SetData(iCol, oIter.AsOid.AsString);
              TJsonToken.RegEx:
                oRow.SetData(iCol, oIter.AsRegEx.AsString);
              TJsonToken.DBRef:
                oRow.SetData(iCol, oIter.AsDBRef.AsString);
              TJsonToken.CodeWScope:
                oRow.SetData(iCol, oIter.AsCodeWScope.Code);
            end;
        end;

        if oIter.InRecurse then begin
          oIter.Return;
          oTab := oStack.Pop;
          oRow := oParentRow;
          if oRow.Table.Columns.ParentRowRefCol = -1 then
            oParentRow := nil
          else
            oParentRow := oParentRow.ParentRow;
        end
        else
          Break;
      end;
    finally
      oIter.Free;
    end;
  finally
    oStack.Free;
  end;
end;

{-------------------------------------------------------------------------------}
class function TFDMongoCustomDataSet.IsOidField(AField: TField; out AOid: TJsonOid): Boolean;
begin
  Result := SameText(AField.FieldName, '_id') and (
      (AField.DataType in [ftFixedChar, ftFixedWideChar]) or
      (AField.DataType in [ftString, ftWideString]) and TStringField(AField).FixedChar
    ) and (AField.Size = 24);
  if Result then
    try
      AOid.AsString := AField.AsString;
    except
      Result := False;
    end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.WriteFieldValues(AFields: TFields; ADoc: TMongoDocument);
var
  i: Integer;
  oFld: TField;
  oDS: TDataSet;
  rOid: TJsonOid;
  lUpdChng: Boolean;
begin
  lUpdChng := UpdateOptions.UpdateChangedFields;
  for i := 0 to AFields.Count - 1 do begin
    oFld := AFields[i];
    if (not lUpdChng and not SameText(oFld.FieldName, '_id') or not oFld.IsNull) and
       (pfInUpdate in oFld.ProviderFlags) then
      case oFld.DataType of
      ftADT:
        begin
          ADoc.BeginObject(oFld.FieldName);
          WriteFieldValues(TADTField(oFld).Fields, ADoc);
          ADoc.EndObject;
        end;
      ftDataSet:
        begin
          ADoc.BeginArray(oFld.FieldName);
          oDS := TDataSetField(oFld).NestedDataSet;
          oDS.First;
          while not oDS.Eof do begin
            WriteFieldValues(oDS.Fields, ADoc);
            oDS.Next;
          end;
          ADoc.EndArray;
        end;
      else
        if IsOidField(oFld, rOid) then
          ADoc.Add(oFld.FieldName, rOid)
        else
          ADoc.Add(oFld.FieldName, oFld.Value);
      end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.BuildInsertDoc(AIns: TMongoInsert.TExpression);
begin
  WriteFieldValues(Fields, AIns.Doc);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.BuildModifyDoc(AMod: TMongoUpdate.TModifier);

  procedure WriteUpdates(AFields: TFields; AMod: TMongoUpdate.TModifier);
  var
    i: Integer;
    oFld: TField;
    oDS: TFDDataSet;
    oDoc: TMongoDocument;
    lUpdChng: Boolean;
  begin
    lUpdChng := UpdateOptions.UpdateChangedFields;
    for i := 0 to AFields.Count - 1 do begin
      oFld := AFields[i];
      if pfInUpdate in oFld.ProviderFlags then
        case oFld.DataType of
        ftADT:
          WriteUpdates(TADTField(oFld).Fields, AMod);
        ftDataSet:
          begin
            oDS := TDataSetField(oFld).NestedDataSet as TFDDataSet;
            if oDS.UpdatesPending then begin
              oDoc := AMod.&Set.Writer;
              oDoc.BeginArray(oFld.FullName);
              oDS.First;
              while not oDS.Eof do begin
                WriteFieldValues(oDS.Fields, oDoc);
                oDS.Next;
              end;
              oDoc.EndArray;
            end;
          end;
        else
          if not lUpdChng or not VarSameValue(oFld.CurValue, oFld.OldValue) then
            AMod.&Set.Field(oFld.FullName, oFld.Value);
        end;
    end;
  end;

begin
  WriteUpdates(Fields, AMod);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.ApplyToDoc(ARequest: TFDUpdateRequest; AColl: TMongoCollection);
var
  oIDField: TField;
  oUpd: TMongoUpdate;
  oIns: TMongoInsert;
  oDel: TMongoSelector;
  rOid: TJsonOid;

  procedure ErrorRecordIsDeleted(const AOper: String; ARowsCount: Integer);
  var
    s: String;
  begin
    s := LowerCase(AOper);
    if s[Length(s)] = 'e' then
      s := s + 'd'
    else
      s := s + 'ed';
    FDException(Self, [S_FD_LDApt], er_FD_DAptRecordIsDeleted,
      [AOper, s, ARowsCount, S_FD_DAptRecordIsDeletedReasons]);
  end;

begin
  case ARequest of
  arUpdate:
    begin
      oUpd := TMongoUpdate.Create(AColl.Env);
      try
        oIDField := FieldByName('_id');
        if IsOidField(oIDField, rOid) then
          oUpd.Match().Add('_id', rOid)
        else
          oUpd.Match().Add('_id', oIDField.AsString);
        BuildModifyDoc(oUpd.Modify());
        AColl.Update(oUpd, []);
        if UpdateOptions.CountUpdatedRecords and (AColl.DocsModified <> 1) then
          ErrorRecordIsDeleted('Update', AColl.DocsModified);
      finally
        oUpd.Free;
      end;
    end;

  arInsert:
    begin
      oIns := TMongoInsert.Create(AColl.Env);
      try
        BuildInsertDoc(oIns.Values());
        AColl.Insert(oIns, []);
        if UpdateOptions.CountUpdatedRecords and (AColl.DocsInserted <> 1) then
          ErrorRecordIsDeleted('Insert', AColl.DocsModified);
      finally
        oIns.Free;
      end;
    end;

  arDelete:
    begin
      oDel := TMongoSelector.Create(AColl.Env);
      try
        oIDField := FieldByName('_id');
        if IsOidField(oIDField, rOid) then
          oDel.Match().Add('_id', rOid)
        else
          oDel.Match().Add('_id', oIDField.AsString);
        AColl.Remove(oDel, []);
        if UpdateOptions.CountUpdatedRecords and (AColl.DocsRemoved <> 1) then
          ErrorRecordIsDeleted('Delete', AColl.DocsRemoved);
      finally
        oDel.Free;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.OpenCursor(InfoQuery: Boolean);
begin
  CheckConnection;
  if Cursor = nil then
    CheckNames;
  __TFDConn(Connection).AttachClient(Self);
  try
    inherited OpenCursor(InfoQuery);
  except
    __TFDConn(Connection).DetachClient(Self);
    raise;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.DoDefineDatSManager;
begin
  DoOpenSource(True, True, True);
  inherited DoDefineDatSManager;
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoCustomDataSet.GetCursor(ALimit, ASkip: Integer): IMongoCursor;
begin
  Result := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.DoOpenSource(ABlocked, AInfoQuery, AStructQuery: Boolean);
var
  iLimit: Integer;
  iSkip: Integer;
  oDoc: TMongoDocument;
  iSampleDocs, i: Integer;
  oDef: TFieldDef;
begin
  FRowsFetched := 0;
  if Cursor = nil then begin
    iLimit := FetchOptions.RecsMax;
    if iLimit >= 0 then begin
      if FetchOptions.Mode = fmExactRecsMax then
        Inc(iLimit);
    end;
    iSkip := FetchOptions.RecsSkip;
    FCursor := GetCursor(iLimit, iSkip);
    FExternalCursor := False;
  end;
  if Cursor = nil then
    FDException(Self, [S_FD_LComp, S_FD_MongoId], er_FD_MongoCannotOpenDataSet,
      [GetDisplayName, 'Cursor']);

  if AInfoQuery then begin
    if MetaMergeMode <> mmNone then begin
      FBridge.Reset;
      iSampleDocs := FetchOptions.RowsetSize * 2;
      for i := 1 to iSampleDocs do begin
        oDoc := TMongoDocument.Create(FMongoConnection.Env);
        if not Cursor.Next(oDoc) then begin
          FDFree(oDoc);
          Break;
        end;
        FPrefetchedDocs.Add(oDoc);
        DefineFromDoc(oDoc, iSampleDocs);
      end;
      if FieldDefs.Count = 0 then begin
        oDef := FieldDefs.AddFieldDef;
        oDef.Name := '_id';
        oDef.DataType := ftFixedWideChar;
        oDef.Size := 24;
        oDef.Attributes := oDef.Attributes + [faFixed];
      end;
    end;
    FieldDefList.Update;
    FieldDefs.Updated := True;
  end;
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoCustomDataSet.DoFetch(ATable: TFDDatSTable; AAll: Boolean;
  ADirection: TFDFetchDirection): Integer;
var
  iRecsMax, iRecsPrior: Integer;
  rState: TFDDatSLoadState;
begin
  ASSERT(ATable = Table);
  if not ((ADirection = fdDown) and ((Cursor <> nil) or (FPrefetchedDocs.Count > 0))) then begin
    Result := 0;
    Exit;
  end;
  if AAll or (FetchOptions.Mode = fmAll) then
    if FetchOptions.RecsMax = -1 then
      iRecsMax := MaxInt
    else if FRowsFetched < FetchOptions.RecsMax then
      iRecsMax := FetchOptions.RecsMax - FRowsFetched
    else
      iRecsMax := 0
  else
    if FetchOptions.RecsMax = -1 then
      iRecsMax := FetchOptions.RowsetSize
    else if FRowsFetched < FetchOptions.RecsMax then begin
      iRecsMax := FetchOptions.RecsMax - FRowsFetched;
      if iRecsMax > FetchOptions.RowsetSize then
        iRecsMax := FetchOptions.RowsetSize;
    end
    else
      iRecsMax := 0;
  iRecsPrior := FRowsFetched;
  Table.BeginLoadData(rState, lmFetching);
  try
    while iRecsMax > 0 do begin
      if FRowsFetched < FPrefetchedDocs.Count then
        FetchFromDoc(FPrefetchedDocs[FRowsFetched])
      else if Cursor.Next then
        FetchFromDoc(Cursor.Doc)
      else begin
        FSourceEOF := True;
        Break;
      end;
      Inc(FRowsFetched);
      Dec(iRecsMax);
    end;
  finally
    if FSourceEOF or (FRowsFetched >= FPrefetchedDocs.Count) then
      FPrefetchedDocs.Clear;
    Table.EndLoadData(rState);
  end;
  Result := FRowsFetched - iRecsPrior;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoCustomDataSet.InternalClose;
begin
  try
    inherited InternalClose;
  finally
    if Connection <> nil then
      __TFDConn(Connection).DetachClient(Self);
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoCustomDataSet.DoCloseSource;
begin
  inherited DoCloseSource;
  FCursor := nil;
  FExternalCursor := False;
  FPrefetchedDocs.Clear;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoCustomDataSet.DoResetDatSManager;
begin
  inherited DoResetDatSManager;
  ClearColumnMap;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoCustomDataSet.DoUpdateRecordHandler(ARow: TFDDatSRow;
  ARequest: TFDUpdateRequest; AOptions: TFDUpdateRowOptions;
  var AAction: TFDErrorAction);
var
  sCollName: String;
  oColl: TMongoCollection;
begin
  inherited DoUpdateRecordHandler(ARow, ARequest, AOptions, AAction);
  if (AAction = eaDefault) and (ARequest in [arInsert, arUpdate, arDelete]) then begin
    sCollName := UpdateOptions.UpdateTableName;
    if sCollName = '' then
      sCollName := CollectionName;
    oColl := FMongoConnection[DatabaseName][sCollName];
    if Connection.ConnectionIntf.Transaction.Active then
      oColl.Session := TMongoSession(Connection.ConnectionIntf.Transaction.CliObj)
    else
      oColl.Session := nil;
    ApplyToDoc(ARequest, oColl);
    AAction := eaApplied;
  end;
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoCustomDataSet.GetCanRefresh: Boolean;
begin
  Result := inherited GetCanRefresh and not FExternalCursor;
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoCustomDataSet.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and (DatabaseName <> '') and (CollectionName <> '');
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoCustomDataSet.ServerDeleteAll(ANoUndo: Boolean);
begin
  Collection.RemoveAll;
end;

{ ----------------------------------------------------------------------------- }
{ TFDMongoQuery                                                                 }
{ ----------------------------------------------------------------------------- }
destructor TFDMongoQuery.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FQuery);
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoQuery.GetQuery: TMongoQuery;
begin
  CheckConnectionActive;
  if FQuery = nil then
    FQuery := TMongoQuery.Create(FMongoConnection.Env);
  Result := FQuery;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoQuery.SetQProject(const AValue: String);
begin
  if QProject <> AValue then begin
    CheckInactive;
    if FQuery <> nil then
      Query.Project.Clear;
    FQProject := AValue;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoQuery.SetQMatch(const AValue: String);
begin
  if QMatch <> AValue then begin
    CheckInactive;
    if FQuery <> nil then
      Query.Match.Clear;
    FQMatch := AValue;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoQuery.SetQSort(const AValue: String);
begin
  if QSort <> AValue then begin
    CheckInactive;
    if FQuery <> nil then
      Query.Sort.Clear;
    FQSort := AValue;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoQuery.PrepareQuery(ALimit, ASkip: Integer);
begin
  if QProject <> '' then
    Query
      .Project
        .Clear
        .Append(QProject)
      .&End;
  if QMatch <> '' then
    Query
      .Match
        .Clear
        .Append(QMatch)
      .&End;
  if QSort <> '' then
    Query
      .Sort
        .Clear
        .Append(QSort)
      .&End;
  if ALimit >= 0 then
    Query.Limit(ALimit);
  if ASkip >= 0 then
    Query.Skip(ASkip);
end;

{-------------------------------------------------------------------------------}
function TFDMongoQuery.GetCursor(ALimit, ASkip: Integer): IMongoCursor;
begin
  PrepareQuery(ALimit, ASkip);
  Result := Collection.Find(Query, []);
end;

{-------------------------------------------------------------------------------}
procedure TFDMongoQuery.UpdateRecordCount;
begin
  PrepareQuery(FetchOptions.RecsMax, FetchOptions.RecsSkip);
  FRecordCount := Collection.Count(Query, []);
end;

{ ----------------------------------------------------------------------------- }
{ TFDMongoPipeline                                                              }
{ ----------------------------------------------------------------------------- }
destructor TFDMongoPipeline.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FPipeline);
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoPipeline.GetPipeline: TMongoPipeline;
begin
  CheckConnectionActive;
  if FPipeline = nil then
    FPipeline := TMongoPipeline.Create(FMongoConnection.Env);
  Result := FPipeline;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoPipeline.SetPProject(const AValue: String);
begin
  if PProject <> AValue then begin
    CheckInactive;
    if FPipeline <> nil then
      Pipeline.Project.Clear;
    FPProject := AValue;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoPipeline.SetPMatch(const AValue: String);
begin
  if PMatch <> AValue then begin
    CheckInactive;
    if FPipeline <> nil then
      Pipeline.Match.Clear;
    FPMatch := AValue;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoPipeline.SetPRedact(const AValue: String);
begin
  if PRedact <> AValue then begin
    CheckInactive;
    if FPipeline <> nil then
      Pipeline.Redact.Clear;
    FPRedact := AValue;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoPipeline.SetPUnwind(const AValue: String);
begin
  if PUnwind <> AValue then begin
    CheckInactive;
    if FPipeline <> nil then
      Pipeline.Unwind('');
    FPUnwind := AValue;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoPipeline.SetPGroup(const AValue: String);
begin
  if PGroup <> AValue then begin
    CheckInactive;
    if FPipeline <> nil then
      Pipeline.Group.Clear;
    FPGroup := AValue;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoPipeline.SetPSort(const AValue: String);
begin
  if PSort <> AValue then begin
    CheckInactive;
    if FPipeline <> nil then
      Pipeline.Sort.Clear;
    FPSort := AValue;
  end;
end;

{ ----------------------------------------------------------------------------- }
procedure TFDMongoPipeline.PreparePipeline(ALimit, ASkip: Integer);
begin
  if PProject <> '' then
    Pipeline
      .Project
        .Clear
        .Append(PProject)
      .&End;
  if PMatch <> '' then
    Pipeline
      .Match
        .Clear
        .Append(PMatch)
      .&End;
  if PRedact <> '' then
    Pipeline
      .Redact
        .Clear
        .Append(PRedact)
      .&End;
  if PUnwind <> '' then
    Pipeline.Unwind(PUnwind);
  if PGroup <> '' then
    Pipeline
      .Group
        .Clear
        .Append(PGroup)
      .&End;
  if PSort <> '' then
    Pipeline
      .Sort
        .Clear
        .Append(PSort)
      .&End;
  if ALimit >= 0 then
    Pipeline.Limit(ALimit);
  if ASkip >= 0 then
    Pipeline.Skip(ASkip);
end;

{ ----------------------------------------------------------------------------- }
function TFDMongoPipeline.GetCursor(ALimit, ASkip: Integer): IMongoCursor;
begin
  PreparePipeline(ALimit, ASkip);
  Result := Collection.Aggregate(Pipeline);
end;

end.
