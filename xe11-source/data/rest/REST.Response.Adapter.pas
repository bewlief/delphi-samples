{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
unit REST.Response.Adapter;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.DateUtils, System.Generics.Collections, System.Rtti,
    System.JSON, System.JSON.Builders,
  Data.DB, Data.DBJson, Data.Bind.Components,
  REST.Client, REST.Types;

type
  TOnBeforeOpenDataSetEvent = procedure(Sender: TObject) of object;
  TJSONParserObjectCallback = procedure(const AJSONObject: TJSONObject) of object;

  /// <summary>
  /// Updates a dataset with the content of a TJSONValue. Can dynamically generate field definitions or
  /// use user-specified field definitions.
  /// </summary>
  TCustomJSONDataSetAdapter = class(TComponent, TJSONToDataSetBridge.IAdaptor)
  strict private
    FBridge: TJSONToDataSetBridge;
    FFieldDefs: TFieldDefs;

    procedure SetFieldDefs(AValue: TFieldDefs);
    function GetDataSet: TDataSet;
    procedure SetDataSet(const AValue: TDataSet);
    procedure TransferJSONData(const AJSON: TJSONValue);
  private
    function GetMetaMergeMode: TJSONMetaMergeMode;
    function GetObjectView: Boolean;
    function GetSampleObjects: Integer;
    function GetTypesMode: TJSONTypesMode;
    function GetStringFieldSize: Integer;
    procedure SetMetaMergeMode(const Value: TJSONMetaMergeMode);
    procedure SetObjectView(const Value: Boolean);
    procedure SetSampleObjects(const Value: Integer);
    procedure SetTypesMode(const Value: TJSONTypesMode);
    procedure SetStringFieldSize(const Value: Integer);
  protected
    procedure DoBeforeOpenDataSet; virtual;
    function GetFieldDefsClass: TFieldDefsClass; virtual;

    // TJSONToDataSetBridge.IAdaptor
    function GetDefaultFieldName(const AJSON: TJSONValue): string; virtual;
    function GetScanDepth: Integer; virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Clears all field definitions.
    /// </summary>
    procedure ResetToDefaults;
    /// <summary>
    /// Activates and loads the attached dataset.  First, the field definitions are determined.
    /// If the FieldDefs list is empty, then the JSON value will
    /// be scanned for field definitions.  Otherwise, the definitions in the FieldDefs list
    /// will be used.  Next, the field definitions are assigned to the dataset so that the dataset can create fields.
    /// Finally, the dataset is loaded from the elements in the JSON value.
    /// </summary>
    procedure UpdateDataSet(const AJSON: TJSONValue);
    /// <summary>
    /// Closes the dataset and clears field definitions from the dataset.
    /// </summary>
    procedure ClearDataSet;

    /// <summary>
    /// Dataset that will be filled with values from a JSON value.
    /// </summary>
    property Dataset: TDataSet read GetDataSet write SetDataSet;
    /// <summary>
    /// Optional list of field definitions.  Add to this list to explicitly define the names and types of fields
    /// to add to the dataset.  The Name property of each field definition should correspond to a member of the JSON value.
    /// Adding a field definition is one way to customize the dataset.  Another way is to
    /// add fields to the dataset.  When adding fields to the dataset, the FieldName property of each field should correspond to a member of the JSON value.
    /// </summary>
    property FieldDefs: TFieldDefs read FFieldDefs write SetFieldDefs;

    /// <summary>
    /// Specifies the column definitiob mode.
    /// When it is False, then JSON elements will be represented using flat field list.
    /// When it is True, then JSON objects will be represented using ftADT, arrays - ftDataSet,
    /// and simple fields - type depending on TypesMode and derived from field contents.
    /// This mode is more slow and may be not supported by some dataset classes.
    /// </summary>
    property ObjectView: Boolean read GetObjectView write SetObjectView default False;
    /// <summary>
    /// Specifies the column definitions merging mode. Default value is TJSONMetaMergeMode.Merge.
    /// </summary>
    property MetaMergeMode: TJSONMetaMergeMode read GetMetaMergeMode write SetMetaMergeMode default TJSONMetaMergeMode.Merge;
    /// <summary>
    /// Specifies the number of objects in JSON dataset to scan to determine the structure.
    /// Default value is 1. This is the fastest, but useful only for TJSONTypesMode.StringOnly mode.
    /// </summary>
    property SampleObjects: Integer read GetSampleObjects write SetSampleObjects default 1;
    /// <summary>
    /// Specifies the field type definition mode. When it is:
    /// * TJSONTypesMode.Rich, then adapter will try to derive field type from the JSON content.
    /// * TJSONTypesMode.JSONOnly, then adapter will use only JSON value types.
    /// * TJSONTypesMode.StringOnly, then all fields will be represented by ftWideString.
    /// Default value is TJSONTypesMode.StringOnly.
    /// </summary>
    property TypesMode: TJSONTypesMode read GetTypesMode write SetTypesMode default TJSONTypesMode.StringOnly;
    /// <summary>
    /// Specifies a string field size when TypesMode = TJSONTypesMode.StringOnly. Default value is 255.
    /// </summary>
    property StringFieldSize: Integer read GetStringFieldSize write SetStringFieldSize default 255;
  end;

  /// <summary>
  /// Updates a dataset with the JSONValue from a REST response.
  /// </summary>
  TCustomRESTResponseDataSetAdapter = class(TComponent)
  private type
    /// <summary>
    /// Declare adapter responsible for writing the JSONValue to the dataset
    /// </summary>
    TAdapter = class(TCustomJSONDataSetAdapter)
    private
      FOwner: TCustomRESTResponseDataSetAdapter;
    protected
      procedure DoBeforeOpenDataSet; override;
      function GetDefaultFieldName(const AJSON: TJSONValue): string; override;
      function GetScanDepth: Integer; override;
    end;

    TNotify = class(TCustomRESTResponse.TNotify)
    private
      FOwner: TCustomRESTResponseDataSetAdapter;
      constructor Create(const AOwner: TCustomRESTResponseDataSetAdapter);
    public
      procedure JSONValueChanged(ASender: TObject); override;
    end;

  public type
    TJSONValueError = (NoContent, NoJSON, NoResponseComponent, InvalidRootElement);
    TJSONValueErrorEvent = procedure(Sender: TObject; AUpdateError: TJSONValueError; const AMessage: string) of object;
    EJSONValueError = class(ERESTException)
    private
      FError: TJSONValueError;
    public
      constructor Create(AError: TJSONValueError; const AMessage: string);
      property Error: TJSONValueError read FError;
    end;

  strict private
    FOnBeforeOpenDataSet: TOnBeforeOpenDataSetEvent;
    FBridge: TAdapter;
    FResponse: TCustomRESTResponse;
    FAutoUpdate: boolean;
    FNotify: TNotify;
    FResponseJSONIntf: IRESTResponseJSON;
    FNestedElementsDepth: Integer;
    FNestedElements: Boolean;
    FDeferActivate: Boolean;
    procedure SetResponse(const AValue: TCustomRESTResponse);
    function GetDataSet: TDataSet;
    function GetFieldDefs: TFieldDefs;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetFieldDefs(const Value: TFieldDefs);
    procedure SetActive(const Value: boolean);
    function GetActive: boolean;
    procedure SetAutoUpdate(const Value: boolean);
    procedure SetResponseJSONIntf(const Value: IRESTResponseJSON);
    procedure ResponseJSONChanged(Sender: TObject);
    procedure SetNestedElementsDepth(const Value: Integer);
    procedure InvalidateJSONValue;
    procedure DoJSONValueChanged;
  private
    FRootElement: string;
    FJSONValue: TJSONValue;
    FOnUpdateError: TJSONValueErrorEvent;
    procedure SetNestedElements(const Value: Boolean);
    function CreateJSONValue: TJSONValue;
    function GetJSONValue: TJSONValue;
    function GetResponseComponent: IRESTResponseJSON;
    function GetRootFieldName: string;
    function GetStringFieldSize: Integer;
    procedure SetStringFieldSize(const Value: Integer);
    function GetMetaMergeMode: TJSONMetaMergeMode;
    function GetObjectView: Boolean;
    function GetSampleObjects: Integer;
    function GetTypesMode: TJSONTypesMode;
    procedure SetMetaMergeMode(const Value: TJSONMetaMergeMode);
    procedure SetObjectView(const Value: Boolean);
    procedure SetSampleObjects(const Value: Integer);
    procedure SetTypesMode(const Value: TJSONTypesMode);
  protected
    procedure DoBeforeOpenDataSet; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetRootElement(const AValue: string);
    procedure Loaded; override;
    procedure DoUpdateError(AError: TJSONValueError; const AMessage: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultFieldName(const AJSON: TJSONValue): string; deprecated;
    /// <summary>
    /// Set Active True to fill the dataset.  Set Active False to close the dataset.
    /// </summary>
    property Active: boolean read GetActive write SetActive default False;
    /// <summary>
    /// Set AutoUpdate to True to automatically update the dataset when the JSON changes. For example,
    /// if the Response property is set, then the dataset will be updated when the corresponding
    /// request is executed.
    /// </summary>
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate default True;
    /// <summary>
    /// Associate the adapter with a REST response. When the corresponding request is execute,
    /// the dataset may be automatically filled.
    /// </summary>
    property Response: TCustomRESTResponse read FResponse write SetResponse;
    /// <summary>
    /// Associate the adapter with an object that has a JSON value. When the JSON of the object
    /// is updated, the dataset may be automatically filled.
    /// </summary>
    property ResponseJSON: IRESTResponseJSON read FResponseJSONIntf write SetResponseJSONIntf;
    /// <summary>
    /// Clears all field definitions.
    /// </summary>
    procedure ResetToDefaults;
    /// <summary>
    /// Activates and loads the attached dataset.  First, the field definitions are determined.
    /// If the FieldDefs list is empty, then the JSON value will
    /// be scanned for field definitions.  Otherwise, the definitions in the FieldDefs list
    /// will be used.  Next, the field definitions are assigned to the dataset so that the dataset can create fields.
    /// Finally, the dataset is loaded from the elements in the JSON value.
    /// </summary>
    procedure UpdateDataSet(AJSONValue: TJSONValue = nil);
    /// <summary>
    /// Clears an attached dataset.
    /// </summary>
    procedure ClearDataSet;
    property Dataset: TDataSet read GetDataSet write SetDataSet;
    /// <summary>
    /// Optional list of field definitions.  Add to this list to explicitly define the names and types of fields
    /// to add to the dataset.  The Name property of each field definition should correspond to a member of the JSON value.
    /// Adding a field definition is one way to customize the dataset.  Another way is to
    /// add fields to the dataset.  When adding fields to the dataset, the FieldName property of each field should correspond to a member of the JSON value.
    /// </summary>
    property FieldDefs: TFieldDefs read GetFieldDefs write SetFieldDefs;
    /// <summary>
    /// Optional path into the JSON value.  The path identifies a starting point for loading the dataset.
    /// This RootElement is applied after an optionally set RootElement of the corresponding TRESTResponse was applied.
    /// A Response object may also have a RootElement.  In this case, the RootElement of the response object is applied before
    /// the RootElement of the adapter.
    /// Sample RootElement paths: "foo", "foo.bar", "[0].foo", "foo.bar[0]".
    /// </summary>
    property RootElement: string read FRootElement write SetRootElement;

    /// <summary>
    /// Specifies the column definitiob mode.
    /// When it is False, then JSON elements will be represented using flat field list.
    /// When it is True, then JSON objects will be represented using ftADT, arrays - ftDataSet,
    /// and simple fields - type depending on TypesMode and derived from field contents.
    /// This mode is more slow and may be not supported by some dataset classes.
    /// </summary>
    property ObjectView: Boolean read GetObjectView write SetObjectView default False;
    /// <summary>
    /// Specifies the column definitions merging mode. Default value is TJSONMetaMergeMode.Merge.
    /// </summary>
    property MetaMergeMode: TJSONMetaMergeMode read GetMetaMergeMode write SetMetaMergeMode default TJSONMetaMergeMode.Merge;
    /// <summary>
    /// Specifies the number of objects in JSON dataset to scan to determine the structure.
    /// Default value is 1. This is the fastest, but useful only for TJSONTypesMode.StringOnly mode.
    /// </summary>
    property SampleObjects: Integer read GetSampleObjects write SetSampleObjects default 1;
    /// <summary>
    /// Specifies the field type definition mode. When it is:
    /// * TJSONTypesMode.Rich, then adapter will try to derive field type from the JSON content.
    /// * TJSONTypesMode.JSONOnly, then adapter will use only JSON value types.
    /// * TJSONTypesMode.StringOnly, then all fields will be represented by ftWideString.
    /// Default value is TJSONTypesMode.StringOnly.
    /// </summary>
    property TypesMode: TJSONTypesMode read GetTypesMode write SetTypesMode default TJSONTypesMode.StringOnly;
    /// <summary>
    /// Specifies a string field size when TypesMode = TJSONTypesMode.StringOnly. Default value is 255.
    /// </summary>
    property StringFieldSize: Integer read GetStringFieldSize write SetStringFieldSize default 255;

    /// <summary>
    /// When NestedElements is true, JSON child elements will be added to flat field list.
    /// Used when ObjectView=False.
    /// </summary>
    property NestedElements: Boolean read FNestedElements write SetNestedElements default False;
    /// <summary>
    /// NestedElementsDepth is used when NestedElements = true or ObjectView = true. NestedElementsDepth
    /// indicate how deep to scan JSON child elements for fields. The value 0 indicates a deep scan.
    /// </summary>
    property NestedElementsDepth: Integer read FNestedElementsDepth write SetNestedElementsDepth default 0;

    property OnBeforeOpenDataSet: TOnBeforeOpenDataSetEvent read FOnBeforeOpenDataSet write FOnBeforeOpenDataSet;
    property OnUpdateError: TJSONValueErrorEvent read FOnUpdateError write FOnUpdateError;
  end;

  TRESTResponseDataSetAdapter = class(TCustomRESTResponseDataSetAdapter)
  published
    property Active;
    property AutoUpdate;
    property Dataset;
    property FieldDefs;
    property Response;
    property ResponseJSON;
    property RootElement;
    property ObjectView;
    property MetaMergeMode;
    property SampleObjects;
    property TypesMode;
    property NestedElementsDepth;
    property NestedElements;
    property StringFieldSize;
    property OnBeforeOpenDataSet;
    property OnUpdateError;
  end;

  /// <summary>JSON value holder. Allows TRESTResponseDataSetAdapter to consume a JSON value
  /// created by a means other than TRESTResponse.
  /// To use, assign an instance of this class to TRESTREsponseDataSetAdapter.ResponseJSON.
  /// </summary>
  TRESTResponseJSON = class(TComponent, IRESTResponseJSON)
  private
    FValue: TJSONValue;
    FJSONNotifyList: TList<TNotifyEvent>;
    FOwnsValue: Boolean;
  protected
    /// <summary>Indicate that the JSON value has changed.  Subscribers will be notified.</summary>
    procedure ValueChanged(Sender: TObject);
    { IRESTResponseJSON }
    /// <summary>Add a change event for subscribing to a change to the JSON value</summary>
    procedure AddJSONChangedEvent(const ANotify: TNotifyEvent);
    /// <summary>Remove a change event</summary>
    procedure RemoveJSONChangedEvent(const ANotify: TNotifyEvent);
    /// <summary>Get the current JSON value</summary>
    procedure GetJSONResponse(out AValue: TJSONValue; out AHasOwner: Boolean);
    /// <summary>Indicate if there is a JSON value</summary>
    function HasJSONResponse: Boolean;
    /// <summary>Indicate if there is some content.  In this class, the content is always JSON</summary>
    function HasResponseContent: Boolean;
  public
    /// <summary>Create the class with no JSON</summary>
    constructor Create(AOwner: TComponent); overload; override;
    /// <summary>Create the class with JSON.  If AOwnsValue is true then the JSON will be freed automatically.</summary>
    constructor Create(AOwner: TComponent; const AValue: TJSONValue; AOwnsValue: Boolean); reintroduce; overload;
    destructor Destroy; override;
    /// <summary>Set the JSON value.  Subscribers will be notified.  If AOwnsValue is true then the JSON will be freed automatically.</summary>
    procedure SetJSONValue(const AValue: TJSONValue; AOwnsValue: Boolean);
  end;

  /// <summary>
  /// Updates a REST request with the JSONValue built from a dataset.
  /// </summary>
  TCustomRESTRequestDataSetAdapter = class(TComponent)
  private type
    TNotify = class(TCustomRESTRequest.TNotify)
    private
      FOwner: TCustomRESTRequestDataSetAdapter;
      constructor Create(const AOwner: TCustomRESTRequestDataSetAdapter);
    public
      procedure ParameterValuesNeeded(Sender: TObject); override;
    end;

  strict private
    FBridge: TDataSetToJSONBridge;
    FNotify: TNotify;
    FRequest: TCustomRESTRequest;
    FAutoUpdate: Boolean;
    FAutoCreateParam: Boolean;
    FParamName: string;
    function GetDataSet: TDataSet;
    procedure SetDataSet(const AValue: TDataSet);
    function GetFieldNames: TStrings;
    procedure SetFieldNames(const AValue: TStrings);
    procedure SetRequest(const AValue: TCustomRESTRequest);
    function GetArea: TJSONDataSetArea;
    procedure SetArea(const AValue: TJSONDataSetArea);
    function GetOnFilterRecord: TFilterRecordEvent;
    procedure SetOnFilterRecord(const AValue: TFilterRecordEvent);
    function GetIncludeNulls: Boolean;
    procedure SetIncludeNulls(const AValue: Boolean);
    procedure SetParamName(const AValue: string);
    procedure ParameterValuesNeeded;
    function GetParameter: TRESTRequestParameter;
    procedure CheckRequest;
    procedure CheckDataSet;
    function GetActualParameter: string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// Resets associated request parameter.
    /// </summary>
    procedure ClearParameter;
    /// <summary>
    /// Produces JSON using attached dataset and assigns it to request parameter.
    /// </summary>
    procedure UpdateParameter;

    /// <summary>
    /// Returns REST request parameter to use. The parameter is determined by ParamName.
    /// When ParamName is empty, then "body" is used as a name.
    /// </summary>
    property Parameter: TRESTRequestParameter read GetParameter;

    /// <summary>
    /// Set AutoUpdate to True to automatically produce JSON and assign it to request
    /// parameter when it is needed. For example, if the DataSet property is set, then
    /// the JSON will be produced and assigned to parameter when the request is executed.
    /// </summary>
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate default True;
    /// <summary>
    /// Set AutoCreateParam to True to automatically create REST request body parameter
    /// when it does not exist.
    /// </summary>
    property AutoCreateParam: Boolean read FAutoCreateParam write FAutoCreateParam default True;
    /// <summary>
    /// Associate the adapter with a REST request. When the request is execute,
    /// the JSON may be automatically produce.
    /// </summary>
    property Request: TCustomRESTRequest read FRequest write SetRequest;
    property Dataset: TDataSet read GetDataSet write SetDataSet;
    /// <summary>
    /// Optional list of field names. Add to this list to explicitly define the names of fields
    /// to use in the dataset.
    /// </summary>
    property FieldNames: TStrings read GetFieldNames write SetFieldNames;
    /// <summary>
    /// Specifies REST request parameter name to set dataset JSON value.
    /// </summary>
    property ParamName: string read FParamName write SetParamName;
    /// <summary>
    /// Specifies set of dataset records to include into JSON.
    /// </summary>
    property Area: TJSONDataSetArea read GetArea write SetArea default TJSONDataSetArea.All;
    /// <summary>
    /// Specifies should or not field with null values include into JSON.
    /// </summary>
    property IncludeNulls: Boolean read GetIncludeNulls write SetIncludeNulls default False;
    /// <summary>
    /// Event allowing to filter dataset records.
    /// </summary>
    property OnFilterRecord: TFilterRecordEvent read GetOnFilterRecord write SetOnFilterRecord;
  end;

  TRESTRequestDataSetAdapter = class(TCustomRESTRequestDataSetAdapter)
  published
    property AutoUpdate;
    property AutoCreateParam;
    property Request;
    property Dataset;
    property FieldNames;
    property ParamName;
    property Area;
    property IncludeNulls;
    property OnFilterRecord;
  end;

implementation

uses
  System.SysUtils, System.StrUtils, System.Types, System.JSON.Types,
    System.JSON.Readers, System.JSON.Writers,
  REST.Utils, System.Math, REST.Consts,
  System.Variants;

function FindDefaultDataSet(AComp: TComponent): TDataSet;
begin
  Result := TRESTFindDefaultComponent.FindDefaultT<TDataSet>(AComp);
end;

{ TCustomJSONDataSetAdapter }

constructor TCustomJSONDataSetAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldDefs := GetFieldDefsClass.Create(nil);
  FBridge := TJSONToDataSetBridge.Create(Self);
  FBridge.FieldDefs := GetFieldDefsClass.Create(nil);
  FBridge.ObjectView := False;
  FBridge.MetaMergeMode := TJSONMetaMergeMode.Merge;
  FBridge.SampleObjects := 1;
  FBridge.TypesMode := TJSONTypesMode.StringOnly;
  FBridge.StringFieldSize := 255;
end;

destructor TCustomJSONDataSetAdapter.Destroy;
begin
  FreeAndNil(FFieldDefs);
  FBridge.FieldDefs.Free;
  inherited Destroy;
  FreeAndNil(FBridge);
end;

procedure TCustomJSONDataSetAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = DataSet then
      DataSet := nil;
end;

function TCustomJSONDataSetAdapter.GetFieldDefsClass: TFieldDefsClass;
begin
  Result := DefaultFieldDefsClass;
end;

function TCustomJSONDataSetAdapter.GetDefaultFieldName(const AJSON: TJSONValue): string;
begin
  if AJSON <> nil then
    Result := AJSON.ClassName.Substring(1)
  else
    Result := 'value';
end;

function TCustomJSONDataSetAdapter.GetScanDepth: Integer;
begin
  Result := 1;
end;

function TCustomRESTResponseDataSetAdapter.GetActive: boolean;
begin
  Result := (Dataset <> nil) and Dataset.Active;
end;

function TCustomJSONDataSetAdapter.GetMetaMergeMode: TJSONMetaMergeMode;
begin
  Result := FBridge.MetaMergeMode;
end;

function TCustomJSONDataSetAdapter.GetObjectView: Boolean;
begin
  Result := FBridge.ObjectView;
end;

function TCustomJSONDataSetAdapter.GetSampleObjects: Integer;
begin
  Result := FBridge.SampleObjects;
end;

function TCustomJSONDataSetAdapter.GetTypesMode: TJSONTypesMode;
begin
  Result := FBridge.TypesMode;
end;

function TCustomJSONDataSetAdapter.GetStringFieldSize: Integer;
begin
  Result := FBridge.StringFieldSize;
end;

function TCustomJSONDataSetAdapter.GetDataSet: TDataSet;
begin
  Result := FBridge.Dataset;
end;

procedure TCustomJSONDataSetAdapter.SetDataSet(const AValue: TDataSet);
begin
  if Assigned(DataSet) then
    DataSet.RemoveFreeNotification(self);

  FBridge.Dataset := AValue;

  if Assigned(DataSet) then
    DataSet.FreeNotification(self);
end;

procedure TCustomJSONDataSetAdapter.SetFieldDefs(AValue: TFieldDefs);
begin
  FieldDefs.Assign(AValue);
end;

procedure TCustomJSONDataSetAdapter.SetMetaMergeMode(const Value: TJSONMetaMergeMode);
begin
  FBridge.MetaMergeMode := Value;
end;

procedure TCustomJSONDataSetAdapter.SetObjectView(const Value: Boolean);
begin
  FBridge.ObjectView := Value;
end;

procedure TCustomJSONDataSetAdapter.SetSampleObjects(const Value: Integer);
begin
  FBridge.SampleObjects := Value;
end;

procedure TCustomJSONDataSetAdapter.SetTypesMode(const Value: TJSONTypesMode);
begin
  FBridge.TypesMode := Value;
end;

procedure TCustomJSONDataSetAdapter.SetStringFieldSize(const Value: Integer);
begin
  FBridge.StringFieldSize := Value;
end;

procedure TCustomJSONDataSetAdapter.ResetToDefaults;
begin
  FFieldDefs.Clear;
  FBridge.FieldDefs.Clear;
end;

procedure TCustomJSONDataSetAdapter.ClearDataSet;
begin
  if Assigned(DataSet) then
  begin
    DataSet.Close;
    DataSet.FieldDefs.Clear;
  end;
end;

procedure TCustomJSONDataSetAdapter.DoBeforeOpenDataSet;
begin
  //
end;

procedure TCustomJSONDataSetAdapter.TransferJSONData(const AJSON: TJSONValue);
var
  LContext: TRTTIContext;
  LType: TRTTIType;

  procedure CreateDataSet;
  var
    LMethod: TRTTIMethod;
  begin
    if LType <> nil then
    begin
      // Support TClientDataSet
      LMethod := LType.GetMethod('CreateDataSet');
      if (LMethod <> nil) and (Length(LMethod.GetParameters) = 0) then
        LMethod.Invoke(DataSet, []);
    end;
  end;

  procedure BeginBatch;
  var
    LMethod: TRTTIMethod;
  begin
    if LType <> nil then
    begin
      // Support TFDDataSet
      LMethod := LType.GetMethod('BeginBatch');
      if (LMethod <> nil) and (Length(LMethod.GetParameters) = 1) then
        LMethod.Invoke(DataSet, [True]);
    end;
  end;

  procedure EndBatch;
  var
    LMethod: TRTTIMethod;
  begin
    if LType <> nil then
    begin
      // Support TFDDataSet
      LMethod := LType.GetMethod('EndBatch');
      if (LMethod <> nil) and (Length(LMethod.GetParameters) = 0) then
        LMethod.Invoke(DataSet, []);
    end;
  end;

  procedure MergeChangeLog;
  var
    LMethod: TRTTIMethod;
    LProp: TRttiProperty;
  begin
    if LType <> nil then
    begin
      // Support TClientDataSet and TFDMemTable
      LProp := LType.GetProperty('LogChanges');
      if (LProp <> nil) and LProp.GetValue(DataSet).AsBoolean then
      begin
        LMethod := LType.GetMethod('MergeChangeLog');
        if (LMethod <> nil) and (Length(LMethod.GetParameters) = 0) then
          LMethod.Invoke(DataSet, []);
      end;
    end;
  end;

begin
  Assert(Assigned(DataSet));
  Assert(Assigned(AJSON));

  // dataset MUST have fields before we can continue
  if (DataSet.Fields.Count = 0) and (DataSet.FieldDefs.Count = 0) then
    Exit;

  LType := LContext.GetType(DataSet.ClassType);
  // Disable controls while creating dataset so that
  // activate notification is sent after populate
  DataSet.DisableControls;
  try
    if not DataSet.Active then
    begin
      DoBeforeOpenDataSet;
      if not DataSet.Active then
        CreateDataSet;
      if not DataSet.Active then
        DataSet.Open;
    end;

    BeginBatch;
    try
      FBridge.Append(AJSON);
    finally
      EndBatch;
      MergeChangeLog;
    end;

    // just for convenience we relocate
    // the dataset to the first record
    DataSet.First;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TCustomJSONDataSetAdapter.UpdateDataSet(const AJSON: TJSONValue);
var
  I: Integer;
  LField: TField;
  LFieldDef: TFieldDef;
begin
  // this code might get triggered even without a tdataset
  // being attached. so we should check, if we have a dataset
  // or not - both is okay. really.
  if Assigned(DataSet) and (not (csDestroying in DataSet.ComponentState)) and
    (not (csLoading in DataSet.ComponentState)) then
  begin
    FBridge.Reset;
    try
      // if we do not have any predefined fields-defs,
      // we scan the JSON for available fields
      if FFieldDefs.Count = 0 then
      begin
        FBridge.FieldDefs.Clear;
        // The following may raise an exception
        FBridge.Define(AJSON);
      end
      else
        FBridge.FieldDefs.Assign(FFieldDefs);
    finally
      if DataSet.Active then
        DataSet.Close;
    end;

    // Synchronize fielddefs and persistent fields
    if DataSet.Fields.Count > 0 then
    begin
      for I := 0 to FBridge.FieldDefs.Count - 1 do
      begin
        // Be sure fields and fields defs are compatible
        LField := DataSet.FindField(FBridge.FieldDefs[I].Name);
        if LField <> nil then
          LField.SetFieldDefProps(FBridge.FieldDefs[I]);
      end;
      for I := 0 to DataSet.Fields.Count - 1 do
      begin
        LField := DataSet.Fields[I];
        if (FBridge.FieldDefs as TDefCollection).Find(LField.FieldName) = nil then
        begin
          // Be sure there is a fielddef for every field
          LFieldDef := FBridge.FieldDefs.AddFieldDef;
          LFieldDef.Name := LField.FieldName;
          LField.SetFieldDefProps(LFieldDef);
        end;
      end;
    end;

    DataSet.DisableControls;
    try
      DataSet.FieldDefs := FBridge.FieldDefs;
    finally
      DataSet.EnableControls;
    end;

    TransferJSONData(AJSON);
  end;
end;

{ TCustomRESTResponseDataSetAdapter }

procedure TCustomRESTResponseDataSetAdapter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Response then
    begin
      FResponse := nil;
      if not (csDestroying in ComponentState) then
        DoJSONValueChanged;
    end
    else if (FResponseJSONIntf is TComponent) and (TComponent(FResponseJSONIntf) = AComponent) then
    begin
      FResponseJSONIntf := nil;
      if not (csDestroying in ComponentState) then
        DoJSONValueChanged;
    end;
end;

procedure TCustomRESTResponseDataSetAdapter.ResetToDefaults;
begin
  FBridge.ResetToDefaults;
end;

function TCustomRESTResponseDataSetAdapter.GetResponseComponent: IRESTResponseJSON;
begin
  Result := nil;
  if Response <> nil then
    Result := Response
  else if ResponseJSON <> nil then
    Result := ResponseJSON;
end;

function TCustomRESTResponseDataSetAdapter.CreateJSONValue: TJSONValue;
var
  LJsonValue: TJSONValue;
  LHasOwner: Boolean;
  LPathValue: TJSONValue;
  LRESTResponseJSON: IRESTResponseJSON;
begin
  LRESTResponseJSON := GetResponseComponent;
  if LRESTResponseJSON = nil then
    raise EJSONValueError.Create(TJSONValueError.NoResponseComponent, sAdapterResponseComponentIsNil);
  if LRESTResponseJSON.HasResponseContent then
    if not LRESTResponseJSON.HasJSONResponse then
      raise EJSONValueError.Create(TJSONValueError.NoJSON, sResponseContentIsNotJSON);

  if LRESTResponseJSON.HasJSONResponse then
  begin
    LRESTResponseJSON.GetJSONResponse(LJSONValue, LHasOwner);
    try
      Assert(LJSONValue <> nil);
      if LHasOwner then
        LJSONValue := LJSONValue.Clone as TJSONValue;
      LHasOwner := False;
      if (RootElement <> '') and (LJsonValue <> nil) then
      begin
        if LJSONValue.TryGetValue<TJSONValue>(RootElement, LPathValue) then
        begin
          LPathValue.Owned := False; // Need to decouple from parent, to avoid memleak
          LJsonValue.Free;
          LJsonValue := LPathValue;
        end
        else
          raise EJSONValueError.Create(TJSONValueError.InvalidRootElement, Format(sAdapterInvalidRootElement, [RootElement]));
      end;
    except
      if not LHasOwner then
        LJSONValue.Free;
      raise;
    end;
  end
  else
    LJSONValue := TJSONArray.Create; // Empty value
  Result := LJSONValue;
end;

function TCustomRESTResponseDataSetAdapter.GetJSONValue: TJSONValue;
begin
  if FJSONValue = nil then
    FJSONValue := CreateJSONValue;
  Assert(FJSONValue <> nil);
  Result := FJSONValue;
end;

procedure TCustomRESTResponseDataSetAdapter.SetActive(const Value: boolean);
begin
  if Value <> Active then
    if csLoading in ComponentState then
      FDeferActivate := Value
    else
      if Value then
        UpdateDataSet
      else
        ClearDataSet;
end;

procedure TCustomRESTResponseDataSetAdapter.SetAutoUpdate(const Value: boolean);
begin
  // Changing autoupdate does not cause an update.
  FAutoUpdate := Value;
end;

procedure TCustomRESTResponseDataSetAdapter.ClearDataSet;
begin
  if Assigned(FBridge) then
    FBridge.ClearDataSet;
end;

constructor TCustomRESTResponseDataSetAdapter.Create(AOwner: TComponent);
begin
  inherited;
  FAutoUpdate := True;
  FNotify := TNotify.Create(Self);
  FBridge := TAdapter.Create(Self);
  FBridge.FOwner := Self;
  Dataset := FindDefaultDataSet(Self);
end;

destructor TCustomRESTResponseDataSetAdapter.Destroy;
begin
  FBridge.Free;
  inherited;
  if FResponse <> nil then
    if FResponse.NotifyList <> nil then
      FResponse.NotifyList.RemoveNotify(FNotify);
  FNotify.Free;
  FJSONValue.Free;
end;

procedure TCustomRESTResponseDataSetAdapter.DoBeforeOpenDataSet;
begin
  if Assigned(FOnBeforeOpenDataSet) then
    FOnBeforeOpenDataSet(self)
end;

procedure TCustomRESTResponseDataSetAdapter.DoJSONValueChanged;
begin
  InvalidateJSONValue;
  if not (csLoading in ComponentState) then
    if AutoUpdate then
      if (GetResponseComponent <> nil) and (GetResponseComponent.HasResponseContent) then
        UpdateDataSet
      else
        ClearDataSet;
end;

procedure TCustomRESTResponseDataSetAdapter.DoUpdateError(AError: TJSONValueError; const AMessage: string);
begin
  if Assigned(FOnUpdateError) then
    FOnUpdateError(Self, AError, AMessage);
end;

function TCustomRESTResponseDataSetAdapter.GetDataSet: TDataSet;
begin
  Result := FBridge.Dataset;
end;

function TCustomRESTResponseDataSetAdapter.GetDefaultFieldName(
  const AJSON: TJSONValue): string;
begin
  Result := FBridge.GetDefaultFieldName(AJSON);
end;

function TCustomRESTResponseDataSetAdapter.GetRootFieldName: string;
var
  I: Integer;
  LRootElement: string;
begin
  LRootElement := RootElement;
  if LRootElement = '' then
  begin
    if FResponse <> nil then
      LRootElement := FResponse.RootElement;
  end;

  if LRootElement <> '' then
  begin
    I := LRootElement.LastDelimiter('.');
    if I >= 0 then
      Result := LRootElement.Substring(I+1)
    else
      Result := LRootElement;
  end;
end;

function TCustomRESTResponseDataSetAdapter.GetFieldDefs: TFieldDefs;
begin
  Result := FBridge.FieldDefs;
end;

function TCustomRESTResponseDataSetAdapter.GetObjectView: Boolean;
begin
  Result := FBridge.ObjectView;
end;

function TCustomRESTResponseDataSetAdapter.GetMetaMergeMode: TJSONMetaMergeMode;
begin
  Result := FBridge.MetaMergeMode;
end;

function TCustomRESTResponseDataSetAdapter.GetSampleObjects: Integer;
begin
  Result := FBridge.SampleObjects;
end;

function TCustomRESTResponseDataSetAdapter.GetTypesMode: TJSONTypesMode;
begin
  Result := FBridge.TypesMode;
end;

function TCustomRESTResponseDataSetAdapter.GetStringFieldSize: Integer;
begin
  Result := FBridge.StringFieldSize;
end;

procedure TCustomRESTResponseDataSetAdapter.InvalidateJSONValue;
begin
  FreeAndNil(FJSONValue);
end;

procedure TCustomRESTResponseDataSetAdapter.Loaded;
begin
  inherited;
  if FDeferActivate then
    if (GetResponseComponent <> nil) and (GetResponseComponent.HasResponseContent) then
    begin
      FDeferActivate := False;
      Active := True;
    end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetDataSet(const Value: TDataSet);
begin
  FBridge.Dataset := Value;
end;

procedure TCustomRESTResponseDataSetAdapter.SetFieldDefs(const Value: TFieldDefs);
begin
  FBridge.FieldDefs := Value;
end;

procedure TCustomRESTResponseDataSetAdapter.SetResponse(const AValue: TCustomRESTResponse);
begin
  if AValue <> FResponse then
  begin
    if Assigned(FResponse) then
    begin
      FResponse.RemoveFreeNotification(self);
      if FResponse.NotifyList <> nil then
        FResponse.NotifyList.RemoveNotify(FNotify);
    end;

    FResponse := AValue;

    if Assigned(FResponse) then
    begin
      ResponseJSON := nil;
      FResponse.FreeNotification(self);
      if FResponse.NotifyList <> nil then
        FResponse.NotifyList.AddNotify(FNotify);
    end;
    if AutoUpdate then
      DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.ResponseJSONChanged(Sender: TObject);
begin
  DoJSONValueChanged;
end;

procedure TCustomRESTResponseDataSetAdapter.SetResponseJSONIntf(
  const Value: IRESTResponseJSON);
var
  LDoChange: Boolean;
begin
  if FResponseJSONIntf <> Value then
  begin
    LDoChange := True;
    if FResponseJSONIntf <> nil then
    begin
      if FResponseJSONIntf is TComponent then
        TComponent(FResponseJSONIntf).RemoveFreeNotification(Self);
      FResponseJSONIntf.RemoveJSONChangedEvent(ResponseJSONChanged);
    end;
    FResponseJSONIntf := Value;
    if FResponseJSONIntf <> nil then
    begin
      LDoChange := Response = nil; // Following line will DoChange if Response <> nil
      Response := nil;
      if FResponseJSONIntf is TComponent then
        TComponent(FResponseJSONIntf).FreeNotification(Self);
      FResponseJSONIntf.AddJSONChangedEvent(ResponseJSONChanged);
    end;
    if LDoChange then
      DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetRootElement(const AValue: string);
begin
  if FRootElement <> AValue then
  begin
    FRootElement := AValue;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetNestedElements(
  const Value: Boolean);
begin
  if FNestedElements <> Value then
  begin
    FNestedElements := Value;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetNestedElementsDepth(const Value: Integer);
begin
  if FNestedElementsDepth <> Value then
  begin
    FNestedElementsDepth := Value;
    if FNestedElements then
      DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetObjectView(const Value: Boolean);
begin
  if ObjectView <> Value then
  begin
    FBridge.ObjectView := Value;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetMetaMergeMode(const Value: TJSONMetaMergeMode);
begin
  if MetaMergeMode <> Value then
  begin
    FBridge.MetaMergeMode := Value;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetSampleObjects(const Value: Integer);
begin
  if SampleObjects <> Value then
  begin
    FBridge.SampleObjects := Value;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetTypesMode(const Value: TJSONTypesMode);
begin
  if TypesMode <> Value then
  begin
    FBridge.TypesMode := Value;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.SetStringFieldSize(const Value: Integer);
begin
  if StringFieldSize <> Value then
  begin
    FBridge.StringFieldSize := Value;
    DoJSONValueChanged;
  end;
end;

procedure TCustomRESTResponseDataSetAdapter.UpdateDataSet(AJSONValue: TJSONValue);
begin
  try
    if AJSONValue = nil then
      AJSONValue := GetJSONValue;
    FBridge.UpdateDataSet(AJSONValue);
  except
    on E: Exception do
      if (E is EJSONValueError) and Assigned(FOnUpdateError) then
        FOnUpdateError(Self, EJSONValueError(E).Error, E.Message)
      else
        raise;
  end;
end;

{ TCustomRESTResponseDataSetAdapter.TAdapter }

procedure TCustomRESTResponseDataSetAdapter.TAdapter.DoBeforeOpenDataSet;
begin
  FOwner.DoBeforeOpenDataSet;
end;

function TCustomRESTResponseDataSetAdapter.TAdapter.GetDefaultFieldName(const AJSON: TJSONValue): string;
begin
  Result := FOwner.GetRootFieldName;
  if Result = '' then
    Result := inherited;
end;

function TCustomRESTResponseDataSetAdapter.TAdapter.GetScanDepth: Integer;
begin
  if FOwner.NestedElements or FOwner.ObjectView then
  begin
    Result := FOwner.NestedElementsDepth;
    if Result <= 0 then
      Result := MaxInt;  // No limit
  end
  else
    Result := 1;
end;

{ TCustomRESTResponseDataSetAdapter.TNotify }

constructor TCustomRESTResponseDataSetAdapter.TNotify.Create(
  const AOwner: TCustomRESTResponseDataSetAdapter);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TCustomRESTResponseDataSetAdapter.TNotify.JSONValueChanged(ASender: TObject);
begin
  FOwner.DoJSONValueChanged;
end;

{ TCustomRESTResponseDataSetAdapter.EJSONValueError }

constructor TCustomRESTResponseDataSetAdapter.EJSONValueError.Create(
  AError: TJSONValueError; const AMessage: string);
begin
  inherited Create(AMessage);
end;

{ TRESTResponseJSON }

procedure TRESTResponseJSON.AddJSONChangedEvent(const ANotify: TNotifyEvent);
begin
  Assert(not FJSONNotifyList.Contains(ANotify));
  if not FJSONNotifyList.Contains(ANotify) then
    FJSONNotifyList.Add(ANotify);
end;

constructor TRESTResponseJSON.Create(AOwner: TComponent);
begin
  inherited;
  FJSONNotifyList := TList<TNotifyEvent>.Create;
end;

constructor TRESTResponseJSON.Create(AOwner: TComponent; const AValue: TJSONValue; AOwnsValue: Boolean);
begin
  Create(AOwner);
  FValue := AValue;
  FOwnsValue := AOwnsValue;
end;

destructor TRESTResponseJSON.Destroy;
begin
  // We own the JSONValue, so free it.
  if FOwnsValue then
    FValue.Free;
  FJSONNotifyList.Free;
  inherited;
end;

procedure TRESTResponseJSON.GetJSONResponse(out AValue: TJSONValue;
  out AHasOwner: Boolean);
begin
  AValue := FValue;
  AHasOwner := FOwnsValue; // We own this object
end;

function TRESTResponseJSON.HasJSONResponse: Boolean;
begin
  Result := FValue <> nil;
end;

function TRESTResponseJSON.HasResponseContent: Boolean;
begin
  Result := FValue <> nil;
end;

procedure TRESTResponseJSON.RemoveJSONChangedEvent(const ANotify: TNotifyEvent);
begin
  Assert(FJSONNotifyList.Contains(ANotify));
  FJSONNotifyList.Remove(ANotify);
end;

procedure TRESTResponseJSON.SetJSONValue(const AValue: TJSONValue;
  AOwnsValue: Boolean);
begin
  if FOwnsValue then
    FreeAndNil(FValue);
  FValue := AValue;
  FOwnsValue := AOwnsValue;
  ValueChanged(self);
end;

procedure TRESTResponseJSON.ValueChanged(Sender: TObject);
var
  LNotifyEvent: TNotifyEvent;
begin
  for LNotifyEvent in FJSONNotifyList do
    LNotifyEvent(Self);
end;

{ TCustomRESTRequestDataSetAdapter.TNotify }

constructor TCustomRESTRequestDataSetAdapter.TNotify.Create(
  const AOwner: TCustomRESTRequestDataSetAdapter);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TCustomRESTRequestDataSetAdapter.TNotify.ParameterValuesNeeded(
  Sender: TObject);
begin
  FOwner.ParameterValuesNeeded;
end;

{ TCustomRESTRequestDataSetAdapter }

constructor TCustomRESTRequestDataSetAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoUpdate := True;
  FAutoCreateParam := True;
  FNotify := TNotify.Create(Self);
  FBridge := TDataSetToJSONBridge.Create;
  Dataset := FindDefaultDataSet(Self);
end;

destructor TCustomRESTRequestDataSetAdapter.Destroy;
begin
  if FRequest <> nil then
    ClearParameter;
  FBridge.Free;
  inherited;
  if FRequest <> nil then
    if FRequest.NotifyList <> nil then
      FRequest.NotifyList.RemoveNotify(FNotify);
  FNotify.Free;
end;

function TCustomRESTRequestDataSetAdapter.GetDataSet: TDataSet;
begin
  Result := FBridge.Dataset;
end;

function TCustomRESTRequestDataSetAdapter.GetFieldNames: TStrings;
begin
  Result := FBridge.FieldNames;
end;

function TCustomRESTRequestDataSetAdapter.GetArea: TJSONDataSetArea;
begin
  Result := FBridge.Area;
end;

function TCustomRESTRequestDataSetAdapter.GetOnFilterRecord: TFilterRecordEvent;
begin
  Result := FBridge.OnFilterRecord;
end;

function TCustomRESTRequestDataSetAdapter.GetIncludeNulls: Boolean;
begin
  Result := FBridge.IncludeNulls;
end;

procedure TCustomRESTRequestDataSetAdapter.SetDataSet(const AValue: TDataSet);
begin
  if FBridge.Dataset <> AValue then
  begin
    if FBridge.Dataset <> nil then
      FBridge.Dataset.RemoveFreeNotification(Self);
    FBridge.Dataset := AValue;
    if FBridge.Dataset <> nil then
      FBridge.Dataset.FreeNotification(Self);
  end;
end;

procedure TCustomRESTRequestDataSetAdapter.SetFieldNames(const AValue: TStrings);
begin
  FBridge.FieldNames.Assign(AValue);
end;

procedure TCustomRESTRequestDataSetAdapter.SetRequest(const AValue: TCustomRESTRequest);
begin
  if AValue <> FRequest then
  begin
    if Assigned(FRequest) then
    begin
      ClearParameter;
      FRequest.RemoveFreeNotification(self);
      if FRequest.NotifyList <> nil then
        FRequest.NotifyList.RemoveNotify(FNotify);
    end;

    FRequest := AValue;

    if Assigned(FRequest) then
    begin
      FRequest.FreeNotification(self);
      if FRequest.NotifyList <> nil then
        FRequest.NotifyList.AddNotify(FNotify);
    end;
  end;
end;

procedure TCustomRESTRequestDataSetAdapter.SetArea(const AValue: TJSONDataSetArea);
begin
  FBridge.Area := AValue
end;

procedure TCustomRESTRequestDataSetAdapter.SetOnFilterRecord(const AValue: TFilterRecordEvent);
begin
  FBridge.OnFilterRecord := AValue;
end;

procedure TCustomRESTRequestDataSetAdapter.SetIncludeNulls(const AValue: Boolean);
begin
  FBridge.IncludeNulls := AValue;
end;

procedure TCustomRESTRequestDataSetAdapter.SetParamName(const AValue: string);
begin
  if FParamName <> AValue then
  begin
    ClearParameter;
    FParamName := AValue;
  end;
end;

procedure TCustomRESTRequestDataSetAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = Request then
      Request := nil
    else if AComponent = Dataset then
      DataSet := nil;
end;

procedure TCustomRESTRequestDataSetAdapter.ParameterValuesNeeded;
begin
  if not (csLoading in ComponentState) then
    if AutoUpdate then
      if DataSet <> nil then
        UpdateParameter
      else
        ClearParameter;
end;

procedure TCustomRESTRequestDataSetAdapter.CheckRequest;
begin
  if Request = nil then
    raise ERESTException.CreateRes(@sAdapterRequestComponentIsNil);
end;

procedure TCustomRESTRequestDataSetAdapter.CheckDataSet;
begin
  if DataSet = nil then
    raise ERESTException.CreateRes(@sAdapterDatasetComponentIsNil);
end;

function TCustomRESTRequestDataSetAdapter.GetActualParameter: string;
begin
  if ParamName = '' then
    Result := REST.Client.sBody
  else
    Result := ParamName;
end;

function TCustomRESTRequestDataSetAdapter.GetParameter: TRESTRequestParameter;
var
  LName: string;
begin
  CheckRequest;

  LName := GetActualParameter;
  Result := Request.Params.ParameterByName(LName);
  if Result = nil then
  begin
    if not AutoCreateParam then
      raise ERESTException.CreateResFmt(@sAdapterParamIsNotFound, [LName]);

    Result := Request.Params.AddItem;
    try
      Result.Kind := TRESTRequestParameterKind.pkREQUESTBODY;
      Result.Name := LName;
      Result.ContentType := TRESTContentType.ctAPPLICATION_JSON;
      Result.Options := Result.Options + [TRESTRequestParameterOption.poTransient];
    except
      Result.Free;
      raise;
    end;
  end;
end;

procedure TCustomRESTRequestDataSetAdapter.ClearParameter;
var
  LParam: TRESTRequestParameter;
  LName: string;
begin
  if (Request = nil) or (csDestroying in Request.ComponentState) then
    Exit;

  LName := GetActualParameter;
  LParam := Request.Params.ParameterByName(LName);
  if LParam <> nil then
  begin
    if TRESTRequestParameterOption.poTransient in LParam.Options then
      LParam.Free
    else
      LParam.Clear;
  end;
end;

procedure TCustomRESTRequestDataSetAdapter.UpdateParameter;
var
  LParam: TRESTRequestParameter;
  LStream: TStringStream;
  LWriter: TJsonTextWriter;
begin
  CheckRequest;
  CheckDataSet;

  LParam := Parameter;

  LStream := TStringStream.Create('', TEncoding.UTF8);
  try
    LWriter := TJsonTextWriter.Create(LStream);
    try
      FBridge.Produce(LWriter);
    finally
      LWriter.Free;
    end;

    if not (csDesigning in ComponentState) and (
         (LParam.Kind in [TRESTRequestParameterKind.pkREQUESTBODY, TRESTRequestParameterKind.pkFILE]) or
         (LParam.Kind = TRESTRequestParameterKind.pkGETorPOST) and
           (Request.Method in [TRESTRequestMethod.rmPOST, TRESTRequestMethod.rmPUT, TRESTRequestMethod.rmPATCH])) then
    begin
      LStream.Position := 0;
      LParam.SetStream(LStream, TRESTObjectOwnership.ooREST);
      LStream := nil;
    end
    else
      LParam.Value := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

end.
