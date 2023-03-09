{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{             FireDAC ADO Migration Helpers             }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$WARN SYMBOL_DEPRECATED OFF}

unit FireDAC.ADO.Migrate;

interface

uses
  System.Classes, Data.DB,
  FireDAC.Stan.Param, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  /// <summary>The TFDADOParametersHelper is a helper class that emulates
  /// ADO TParameters.</summary>
  TFDADOParametersHelper = class helper for TFDParams
  public
    /// <summary> The AddParameter method emulates TParameters.AddParameter.
    ///  Use TFDParams.Add method instead. </summary>
    function AddParameter: TFDParam;
      deprecated 'Use TFDParams.Add instead';
    /// <summary> The CreateParameter method emulates TParameters.CreateParameter.
    ///  Use TFDParams.Add or CreateParam methods instead. </summary>
    function CreateParameter(const Name: string; DataType: TFieldType;
      Direction: TParamType; Size: Integer; Value: Variant): TFDParam;
      deprecated 'Use TFDParams.Add or CreateParam instead';
  end;

  /// <summary>The TFDADOConnectionHelper is a helper class that emulates
  /// TADOConnection.</summary>
  TFDADOConnectionHelper = class helper for TFDCustomConnection
  private
    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
  public
    /// <summary> The GetFieldNames method emulates TADOConnection.GetFieldNames.
    ///  Use TFDConnection.GetFieldNames method instead. </summary>
    procedure GetFieldNames(const TableName: string; List: TStrings); overload;
      deprecated 'Use TFDConnection.GetFieldNames instead';
    /// <summary> The GetProcedureNames method emulates TADOConnection.GetProcedureNames.
    ///  Use TFDConnection.GetStoredProcNames method instead. </summary>
    procedure GetProcedureNames(List: TStrings); overload;
      deprecated 'Use TFDConnection.GetStoredProcNames instead';
    /// <summary> The GetTableNames method emulates TADOConnection.GetTableNames.
    ///  Use TFDConnection.GetTableNames method instead. </summary>
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False); overload;
      deprecated 'Use TFDConnection.GetTableNames instead';
    /// <summary> The Execute method emulates TADOConnection.Execute.
    ///  Use TFDConnection.ExecSQL methods instead. </summary>
    procedure Execute(const CommandText: string; var RecordsAffected: Integer); overload;
      deprecated 'Use TFDConnection.ExecSQL instead';
    /// <summary> The ConnectionTimeout property emulates TADOConnection.ConnectionTimeout.
    ///  Use TFDConnection.Params property instead. </summary>
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;
  end;

  TFDADOAffectRecords = (arCurrent, arFiltered, arAll, arAllChapters);
  TFDADORecordStatus = (rsOK, rsNew, rsModified, rsDeleted, rsUnmodified, rsInvalid,
    rsMultipleChanges, rsPendingChanges, rsCanceled, rsCantRelease,
    rsConcurrencyViolation, rsIntegrityViolation, rsMaxChangesExceeded,
    rsObjectOpen, rsOutOfMemory, rsPermissionDenied, rsSchemaViolation,
    rsDBDeleted);
  TFDADORecordStatusSet = set of TFDADORecordStatus;
  TFDADOSeekOption = (soFirstEQ, soLastEQ, soAfterEQ, soAfter, soBeforeEQ, soBefore);
  TFDADOLockType = (ltUnspecified, ltReadOnly, ltPessimistic, ltOptimistic,
    ltBatchOptimistic);

  /// <summary>Emulates TCustomADODataSet.</summary>
  TFDADODataSetHelper = class helper for TFDRDBMSDataSet
  private
    function GetRecordStatus: TFDADORecordStatusSet;
      deprecated 'Use TFDDataSet.UpdateStatus and RowError instead';
    function GetEnableBCD: Boolean;
      deprecated 'Use TFDDataSet.FormatOptions instead';
    procedure SetEnableBCD(const Value: Boolean);
      deprecated 'Use TFDDataSet.FormatOptions instead';
    function GetSort: string;
      deprecated 'Use TFDDataSet.IndexFieldNames instead';
    procedure SetSort(const Value: string);
      deprecated 'Use TFDDataSet.IndexFieldNames instead';
  public
    /// <summary> The CancelBatch method emulates TCustomADODataSet.CancelBatch.
    ///  Use TFDDataSet.CancelUpdates method instead. </summary>
    procedure CancelBatch(AffectRecords: TFDADOAffectRecords = arAll);
      deprecated 'Use TFDDataSet.CancelUpdates instead';
    /// <summary> The UpdateBatch method emulates TCustomADODataSet.UpdateBatch.
    ///  Use TFDDataSet.ApplyUpdates and CommitUpdates methods instead. </summary>
    procedure UpdateBatch(AffectRecords: TFDADOAffectRecords = arAll);
      deprecated 'Use TFDDataSet.ApplyUpdates and CommitUpdates instead';
    /// <summary> The Clone method emulates TCustomADODataSet.Clone.
    ///  Use TFDMemTable.CloneCursor method instead. </summary>
    procedure Clone(Source: TFDDataSet; LockType: TFDADOLockType = ltUnspecified);
      deprecated 'Use TFDMemTable.CloneCursor instead';
    /// <summary> The DeleteRecords method emulates TCustomADODataSet.DeleteRecords.
    ///  Use TFDDataSet.Delete and ServerDeleteAll methods instead. </summary>
    procedure DeleteRecords(AffectRecords: TFDADOAffectRecords = arAll);
      deprecated 'Use TFDDataSet.Delete and ServerDeleteAll instead';
    /// <summary> The DeleteRecords method emulates TCustomADODataSet.Seek.
    ///  Use TFDDataSet.Locate or LocateEx methods instead. </summary>
    function Seek(const KeyValues: Variant; SeekOption: TFDADOSeekOption = soFirstEQ): Boolean;
      deprecated 'Use TFDDataSet.Locate or LocateEx instead';
    /// <summary> The RecordStatus property emulates TCustomADODataSet.RecordStatus.
    ///  Use TFDDataSet.UpdateStatus and RowError properties instead. </summary>
    property RecordStatus: TFDADORecordStatusSet read GetRecordStatus;
    /// <summary> The EnableBCD property emulates TCustomADODataSet.EnableBCD.
    ///  Use TFDDataSet.FormatOptions property instead. </summary>
    property EnableBCD: Boolean read GetEnableBCD write SetEnableBCD default False;
    /// <summary> The Sort property emulates TCustomADODataSet.Sort.
    ///  Use TFDDataSet.IndexFieldNames property instead. </summary>
    property Sort: string read GetSort write SetSort;
  end;

  /// <summary>Emulates TADOCommand.</summary>
  TFDADOCommandHelper = class helper for TFDCommand
  public
    /// <summary> The Execute method emulates TADOCommand.Execute.
    ///  Use TFDCommand.Execute method instead. </summary>
    procedure Execute(const Parameters: Variant); overload;
      deprecated 'Use TFDCommand.Execute instead';
    /// <summary> The Execute method emulates TADOCommand.Execute.
    ///  Use TFDCommand.Execute method instead. </summary>
    procedure Execute(var RecordsAffected: Integer; const Parameters: Variant); overload;
      deprecated 'Use TFDCommand.Execute instead';
  end;

implementation

uses
  System.Variants, System.SysUtils,
  FireDAC.Stan.Option, FireDAC.Stan.Consts, FireDAC.Stan.Error, FireDAC.Stan.Intf,
  FireDAC.Phys.Intf;

{-------------------------------------------------------------------------------}
{ TFDADOParametersHelper                                                        }
{-------------------------------------------------------------------------------}
function TFDADOParametersHelper.AddParameter: TFDParam;
begin
  Result := Add;
end;

{-------------------------------------------------------------------------------}
function TFDADOParametersHelper.CreateParameter(const Name: string;
  DataType: TFieldType; Direction: TParamType; Size: Integer; Value: Variant): TFDParam;
begin
  Result := Add(Name, DataType, Size, Direction);
  Result.Value := Value;
end;

{-------------------------------------------------------------------------------}
{ TFDADOConnectionHelper                                                        }
{-------------------------------------------------------------------------------}
procedure TFDADOConnectionHelper.GetFieldNames(const TableName: string; List: TStrings);
begin
  GetFieldNames('', '', TableName, '', List);
end;

{-------------------------------------------------------------------------------}
procedure TFDADOConnectionHelper.GetProcedureNames(List: TStrings);
begin
  GetStoredProcNames('', '', '', '', List);
end;

{-------------------------------------------------------------------------------}
procedure TFDADOConnectionHelper.GetTableNames(List: TStrings; SystemTables: Boolean);
var
  eScopes: TFDPhysObjectScopes;
begin
  eScopes := [osMy, osOther];
  if SystemTables then
    eScopes := eScopes + [osSystem];
  GetTableNames('', '', '', List, eScopes, [tkTable], SystemTables);
end;

{-------------------------------------------------------------------------------}
procedure TFDADOConnectionHelper.Execute(const CommandText: string;
  var RecordsAffected: Integer);
begin
  RecordsAffected := ExecSQL(CommandText);
end;

{-------------------------------------------------------------------------------}
function TFDADOConnectionHelper.GetConnectionTimeout: Integer;
begin
  Result := ResultConnectionDef.AsInteger[S_FD_ConnParam_Common_LoginTimeout];
end;

{-------------------------------------------------------------------------------}
procedure TFDADOConnectionHelper.SetConnectionTimeout(const Value: Integer);
begin
  ResultConnectionDef.AsInteger[S_FD_ConnParam_Common_LoginTimeout] := Value;
end;

{-------------------------------------------------------------------------------}
{ TFDADODataSetHelper                                                           }
{-------------------------------------------------------------------------------}
function TFDADODataSetHelper.GetEnableBCD: Boolean;
var
  i: Integer;
begin
  for i := 0 to FormatOptions.MapRules.Count - 1 do
    if (FormatOptions.MapRules[i].SourceDataType = dtFmtBCD) and
       (FormatOptions.MapRules[i].TargetDataType = dtDouble) then
      Exit(True);
  Result := False;
end;

{-------------------------------------------------------------------------------}
procedure TFDADODataSetHelper.SetEnableBCD(const Value: Boolean);
var
  i: Integer;
begin
  if Value then begin
    FormatOptions.OwnMapRules := True;
    for i := 0 to FormatOptions.MapRules.Count - 1 do
      if (FormatOptions.MapRules[i].SourceDataType = dtFmtBCD) and
         (FormatOptions.MapRules[i].TargetDataType = dtDouble) then
        Exit;
    FormatOptions.MapRules.Add(dtFMTBcd, dtDouble);
  end
  else begin
    for i := 0 to FormatOptions.MapRules.Count - 1 do
      if (FormatOptions.MapRules[i].SourceDataType = dtFmtBCD) and
         (FormatOptions.MapRules[i].TargetDataType = dtDouble) then
      begin
        FormatOptions.MapRules.Delete(i);
        Break;
      end;
    FormatOptions.OwnMapRules := FormatOptions.MapRules.Count <> 0;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADODataSetHelper.CancelBatch(AffectRecords: TFDADOAffectRecords);
begin
  case AffectRecords of
    arCurrent:
      RevertRecord;
    arFiltered:
              ;
    arAll,
    arAllChapters:
      CancelUpdates;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADODataSetHelper.UpdateBatch(AffectRecords: TFDADOAffectRecords);
begin
  case AffectRecords of
    arCurrent,
    arFiltered:
              ;
    arAll,
    arAllChapters:
      begin
        ApplyUpdates(0);
        CommitUpdates;
      end;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDADODataSetHelper.GetRecordStatus: TFDADORecordStatusSet;
var
  oErr: EFDException;
begin
  Result := [];
  case UpdateStatus of
  usUnmodified: Result := [rsUnmodified]; // rsOK ?
  usModified:   Result := [rsModified];
  usInserted:   Result := [rsNew];
  usDeleted:    Result := [rsDeleted];
  end;
  oErr := RowError;
  if oErr is EFDDBEngineException then
    case EFDDBEngineException(oErr).Kind of
    ekRecordLocked: Result := Result + [rsCantRelease];
    ekNoDataFound:  Result := Result + [rsDBDeleted];
    ekTooManyRows:  Result := Result + [rsMultipleChanges];
    ekUKViolated,
    ekFKViolated:   Result := Result + [rsIntegrityViolation];
    ekObjNotExists: Result := Result + [rsSchemaViolation];
    ekCmdAborted:   Result := Result + [rsCanceled];
    end;
end;

{-------------------------------------------------------------------------------}
procedure TFDADODataSetHelper.DeleteRecords(AffectRecords: TFDADOAffectRecords);
begin
  case AffectRecords of
    arCurrent:
      Delete;
    arFiltered:
      begin
        First;
        while not IsEmpty do
          Delete;
      end;
    arAll,
    arAllChapters:
      ServerDeleteAll;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDADODataSetHelper.Seek(const KeyValues: Variant; SeekOption: TFDADOSeekOption): Boolean;
var
  i: Integer;
  sFields: string;
begin
  sFields := '';
  for i := 0 to IndexFieldCount - 1 do begin
    if sFields <> '' then
      sFields := sFields + ';';
    sFields := sFields + IndexFields[i].FieldName;
  end;

  case SeekOption of
    soFirstEQ:
      begin
        Result := LocateEx(sFields, KeyValues, []);
        if not Result then
          Last;
      end;
    soLastEQ:
      begin
        Result := LocateEx(sFields, KeyValues, [lxoBackward]);
        if not Result then
          Last;
      end;
    soAfterEQ:
      begin
        SetKey;
        FieldValues[sFields] := KeyValues;
        GotoNearest;
        Result := Found;
      end;
    else
              
//    soBeforeEQ,
//    soAfter,
//    soBefore:
      Result := False;
  end;
end;

{-------------------------------------------------------------------------------}
function TFDADODataSetHelper.GetSort: string;
begin
  Result := StringReplace(IndexFieldNames, ':A', ' ASC', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':D', ' DESC', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ';', ', ', [rfReplaceAll]);
end;

{-------------------------------------------------------------------------------}
procedure TFDADODataSetHelper.SetSort(const Value: string);
var
  sSort: string;
begin
  sSort := StringReplace(Value, ',', ';', [rfReplaceAll]);
  sSort := StringReplace(sSort, ' ASC', ':A', [rfReplaceAll, rfIgnoreCase]);
  sSort := StringReplace(sSort, ' DESC', ':D', [rfReplaceAll, rfIgnoreCase]);
  sSort := StringReplace(sSort, ' ', '', [rfReplaceAll]);
  IndexFieldNames := sSort;
end;

{-------------------------------------------------------------------------------}
procedure TFDADODataSetHelper.Clone(Source: TFDDataSet; LockType: TFDADOLockType);
begin
  CloneCursor(Source);
  case LockType of
  ltUnspecified: UpdateOptions.LockMode := lmNone;
  ltReadOnly:    UpdateOptions.ReadOnly := True;
  ltPessimistic: UpdateOptions.LockMode := lmPessimistic;
  ltOptimistic:  UpdateOptions.LockMode := lmOptimistic;
  ltBatchOptimistic: CachedUpdates := True;
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDADOCommandHelper                                                           }
{-------------------------------------------------------------------------------}
procedure TFDADOCommandHelper.Execute(const Parameters: Variant);
var
  RecordsAffected: Integer;
begin
  RecordsAffected := 0;
  Execute(RecordsAffected, Parameters);
end;

{-------------------------------------------------------------------------------}
procedure TFDADOCommandHelper.Execute(var RecordsAffected: Integer;
  const Parameters: Variant);
var
  i: Integer;
begin
  if VarIsArray(Parameters) then begin
    for i := 0 to VarArrayHighBound(Parameters, 1) do
      if VarType(Parameters[i]) <> varError then
        Params[i].Value := Parameters[i];
  end
  else if VarType(Parameters) <> varError then
    Params[0].Value := Parameters;
  Execute();
  RecordsAffected := RowsAffected;
end;

end.
