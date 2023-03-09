{*******************************************************}
{                                                       }
{               Delphi FireDAC Framework                }
{                FireDAC MongoDB driver                 }
{                                                       }
{ Copyright(c) 2004-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
{$I FireDAC.inc}
{$SCOPEDENUMS ON}
{$HPPEMIT LINKUNIT}

unit FireDAC.Phys.MongoDB;

interface

uses
  System.Classes,
  FireDAC.Phys;

type
  /// <summary> Use the TFDPhysMongoDriverLink component to link the MongoDB driver
  ///  to an application and to set it up. Generally, it is enough to include the
  ///  FireDAC.Phys.MongoDB unit in your application uses clause. </summary>
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux)]
  TFDPhysMongoDriverLink = class(TFDPhysDriverLink)
  protected
    function GetBaseDriverID: String; override;
  end;

{-------------------------------------------------------------------------------}
implementation

uses
  System.Variants, System.SysUtils, System.SyncObjs, System.JSON.Types,
    System.JSON.Builders, System.JSON.Readers, System.Rtti,
  FireDAC.Stan.Consts, FireDAC.Stan.Intf, FireDAC.Stan.Util, FireDAC.Stan.Error,
    FireDAC.Stan.Option,
  FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.Phys.SQLGenerator,
  FireDAC.Phys.MongoDBCli, FireDAC.Phys.MongoDBWrapper, FireDAC.Phys.MongoDBDef,
    FireDAC.Phys.MongoDBMeta;

const
  S_TZLocal = 'Local';
  S_TZUTC = 'UTC';

type
  TFDPhysMongoDriver = class;
  TFDPhysMongoConnection = class;
  TFDPhysMongoEventAlerter = class;
  TFDPhysMongoTransaction = class;

  TFDPhysMongoDriver = class(TFDPhysDriver)
  private
    FCLib: TMongoClientLib;
    FBLib: TMongoBSONLib;
  protected
    class function GetBaseDriverID: String; override;
    class function GetBaseDriverDesc: String; override;
    class function GetRDBMSKind: TFDRDBMSKind; override;
    class function GetConnectionDefParamsClass: TFDConnectionDefParamsClass; override;
    procedure InternalLoad; override;
    procedure InternalUnload; override;
    function InternalCreateConnection(AConnHost: TFDPhysConnectionHost): TFDPhysConnection; override;
    function GetCliObj: Pointer; override;
    function GetConnParams(AKeys: TStrings; AParams: TFDDatSTable): TFDDatSTable; override;
  public
    constructor Create(AManager: TFDPhysManager; const ADriverDef: IFDStanDefinition); override;
    destructor Destroy; override;
  end;

  TFDPhysMongoConnection = class(TFDPhysConnection)
  private
    FEnv: TMongoEnv;
    FConnection: TMongoConnection;
    FServerVersion: TFDVersion;
    function BuildMongoConnectString(const AConnectionDef: IFDStanConnectionDef): String;
  protected
    procedure InternalConnect; override;
    procedure InternalSetMeta; override;
    procedure InternalDisconnect; override;
    procedure InternalPing; override;
    function InternalCreateTransaction: TFDPhysTransaction; override;
    function InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter; override;
    function InternalCreateCommand: TFDPhysCommand; override;
    function InternalCreateMetadata: TObject; override;
    function InternalCreateCommandGenerator(const ACommand: IFDPhysCommand): TFDPhysCommandGenerator; override;
{$IFDEF FireDAC_MONITOR}
    procedure InternalTracingChanged; override;
{$ENDIF}
    procedure GetItem(AIndex: Integer; out AName: String;
      out AValue: Variant; out AKind: TFDMoniAdapterItemKind); override;
    function GetItemCount: Integer; override;
    procedure InternalExecuteDirect(const ASQL: String; ATransaction: TFDPhysTransaction); override;
    function GetCliObj: Pointer; override;
    function InternalGetCliHandle: Pointer; override;
  public
    constructor Create(ADriverObj: TFDPhysDriver; AConnHost: TFDPhysConnectionHost); override;
    property MongoConnection: TMongoConnection read FConnection;
  end;

  TFDPhysMongoEventAlerter = class(TFDPhysEventAlerter)
  private
    FTailThread: TThread;
    FEventsConnection: IFDPhysConnection;
    FColl: String;
    FDB: String;
    FSize: Integer;
    procedure DoFired(ADoc: TMongoDocument);
  protected
    // TFDPhysEventAlerter
    procedure InternalAllocHandle; override;
    procedure InternalRegister; override;
    procedure InternalHandle(AEventMessage: TFDPhysEventMessage); override;
    procedure InternalAbortJob; override;
    procedure InternalReleaseHandle; override;
    procedure InternalSignal(const AEvent: String; const AArgument: Variant); override;
  end;

  TFDPhysMongoTransaction = class(TFDPhysTransaction)
  private
    FSession: TMongoSession;
  protected
    procedure InternalAllocHandle; override;
    procedure InternalReleaseHandle; override;
    procedure InternalChanged; override;
    procedure InternalStartTransaction(ATxID: LongWord); override;
    procedure InternalCommit(ATxID: LongWord); override;
    procedure InternalRollback(ATxID: LongWord); override;
    function GetCliObj: Pointer; override;
  end;

{-------------------------------------------------------------------------------}
{ TFDPhysMongoDriverLink                                                        }
{-------------------------------------------------------------------------------}
function TFDPhysMongoDriverLink.GetBaseDriverID: String;
begin
  Result := S_FD_MongoId;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMongoDriver                                                            }
{-------------------------------------------------------------------------------}
constructor TFDPhysMongoDriver.Create(AManager: TFDPhysManager;
  const ADriverDef: IFDStanDefinition);
begin
  inherited Create(AManager, ADriverDef);
  FCLib := TMongoClientLibExt2.Create(FDPhysManagerObj);
  FBLib := TMongoBSONLib.Create(FCLib);
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysMongoDriver.Destroy;
begin
  inherited Destroy;
  FDFreeAndNil(FBLib);
  FDFreeAndNil(FCLib);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoDriver.InternalLoad;
var
  sHome, sLib: String;
begin
  sHome := '';
  sLib := '';
  GetVendorParams(sHome, sLib);
  FCLib.Load(sHome, sLib);
  FBLib.Load;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoDriver.InternalUnload;
begin
  FBLib.Unload;
  FCLib.Unload;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoDriver.InternalCreateConnection(
  AConnHost: TFDPhysConnectionHost): TFDPhysConnection;
begin
  Result := TFDPhysMongoConnection.Create(Self, AConnHost);
end;

{-------------------------------------------------------------------------------}
class function TFDPhysMongoDriver.GetBaseDriverID: String;
begin
  Result := S_FD_MongoId;
end;

{-------------------------------------------------------------------------------}
class function TFDPhysMongoDriver.GetBaseDriverDesc: String;
begin
  Result := 'MongoDB';
end;

{-------------------------------------------------------------------------------}
class function TFDPhysMongoDriver.GetRDBMSKind: TFDRDBMSKind;
begin
  Result := TFDRDBMSKinds.MongoDB;
end;

{-------------------------------------------------------------------------------}
class function TFDPhysMongoDriver.GetConnectionDefParamsClass: TFDConnectionDefParamsClass;
begin
  Result := TFDPhysMongoConnectionDefParams;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoDriver.GetCliObj: Pointer;
begin
  Result := FBLib;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoDriver.GetConnParams(AKeys: TStrings; AParams: TFDDatSTable): TFDDatSTable;
var
  oView: TFDDatSView;
begin
  Result := inherited GetConnParams(AKeys, AParams);
  oView := Result.Select('Name=''' + S_FD_ConnParam_Common_Database + '''');
  if oView.Rows.Count = 1 then begin
    oView.Rows[0].BeginEdit;
    oView.Rows[0].SetValues('LoginIndex', 4);
    oView.Rows[0].EndEdit;
  end;

  Result.Rows.Add([Unassigned, S_FD_ConnParam_Common_Server, S_FD_Local, S_FD_Local, S_FD_ConnParam_Common_Server, 2]);
  Result.Rows.Add([Unassigned, S_FD_ConnParam_Common_Port, '@I', '27017', S_FD_ConnParam_Common_Port, 3]);
  Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_MoreHosts, '@S', '', S_FD_ConnParam_Mongo_MoreHosts, -1]);
  Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_UseSSL, '@L', S_FD_False, S_FD_ConnParam_Mongo_UseSSL, -1]);
  Result.Rows.Add([Unassigned, S_FD_ConnParam_Common_LoginTimeout, '@I', '0', S_FD_ConnParam_Common_LoginTimeout, -1]);
  Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_ReadTimeout, '@I', '300', S_FD_ConnParam_Mongo_ReadTimeout, -1]);
  Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_TimeZone, S_TZLocal + ';' + S_TZUTC, S_TZLocal, S_FD_ConnParam_Mongo_TimeZone, -1]);
  Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_MongoAdvanced, '@S', '', S_FD_ConnParam_Mongo_MongoAdvanced, -1]);
                                                      
  // Result.Rows.Add([Unassigned, S_FD_ConnParam_Common_MetaDefCatalog, '@S', 'test', S_FD_ConnParam_Common_MetaDefCatalog, -1]);
  // Result.Rows.Add([Unassigned, S_FD_ConnParam_Common_MetaCurCatalog, '@S', '', S_FD_ConnParam_Common_MetaCurCatalog, -1]);
  if (AKeys <> nil) and
     (CompareText(AKeys.Values[S_FD_ConnParam_Mongo_UseSSL], S_FD_True) = 0) then begin
    Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_SSL_PEMFile, '@F:PEM File|*.pem', '', S_FD_ConnParam_Mongo_SSL_PEMFile, -1]);
    Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_SSL_PEMPassword, '@P', '', S_FD_ConnParam_Mongo_SSL_PEMPassword, -1]);
    Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_SSL_CAFile, '@F:CRT File|*.crt', '', S_FD_ConnParam_Mongo_SSL_CAFile, -1]);
    Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_SSL_WeakValidation, '@L', S_FD_False, S_FD_ConnParam_Mongo_SSL_WeakValidation, -1]);
    Result.Rows.Add([Unassigned, S_FD_ConnParam_Mongo_SSL_AllowInvHost, '@L', S_FD_False, S_FD_ConnParam_Mongo_SSL_AllowInvHost, -1]);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMongoConnection                                                        }
{-------------------------------------------------------------------------------}
constructor TFDPhysMongoConnection.Create(ADriverObj: TFDPhysDriver;
  AConnHost: TFDPhysConnectionHost);
begin
  inherited Create(ADriverObj, AConnHost);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.InternalCreateTransaction: TFDPhysTransaction;
begin
  Result := TFDPhysMongoTransaction.Create(Self);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.InternalCreateEvent(const AEventKind: String): TFDPhysEventAlerter;
begin
  if CompareText(AEventKind, S_FD_EventKind_Mongo_Tail) = 0 then
    Result := TFDPhysMongoEventAlerter.Create(Self, S_FD_EventKind_Mongo_Tail)
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.InternalCreateCommand: TFDPhysCommand;
begin
                      
  Result := nil;
  FDCapabilityNotSupported(Self, [S_FD_LPhys, DriverObj.DriverID]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.InternalCreateMetadata: TObject;
begin
  Result := TFDPhysMongoMetadata.Create(Self, FServerVersion,
    TFDPhysMongoDriver(DriverObj).FCLib.Version, True);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.InternalCreateCommandGenerator(
  const ACommand: IFDPhysCommand): TFDPhysCommandGenerator;
begin
                      
  Result := nil;
  FDCapabilityNotSupported(Self, [S_FD_LPhys, DriverObj.DriverID]);
end;

{$IFDEF FireDAC_MONITOR}
{-------------------------------------------------------------------------------}
procedure TFDPhysMongoConnection.InternalTracingChanged;
begin
  if FEnv <> nil then begin
    FEnv.Monitor := FMonitor;
    FEnv.Tracing := FTracing;
  end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.BuildMongoConnectString(
  const AConnectionDef: IFDStanConnectionDef): String;
var
  oParams: TFDPhysMongoConnectionDefParams;
  lWasOpt: Boolean;
  i: Integer;
  s: String;

  procedure AddOpt(const AOpt: String);
  begin
    if not lWasOpt then
      Result := Result + '?'
    else
      Result := Result + '&';
    lWasOpt := True;
    Result := Result + AOpt;
  end;

begin
  oParams := AConnectionDef.Params as TFDPhysMongoConnectionDefParams;
  Result := 'mongodb://';
  if AConnectionDef.HasValue(S_FD_ConnParam_Common_UserName) or
     AConnectionDef.HasValue(S_FD_ConnParam_Common_Password) then
    Result := Result + oParams.UserName + ':' + oParams.Password + '@';
  if AConnectionDef.HasValue(S_FD_ConnParam_Common_Server) then
    Result := Result + oParams.Server
  else
    Result := Result + '127.0.0.1';
  if AConnectionDef.HasValue(S_FD_ConnParam_Common_Port) then
    Result := Result + ':' + IntToStr(oParams.Port);
  if AConnectionDef.HasValue(S_FD_ConnParam_Mongo_MoreHosts) then begin
      i := 1;
      s := oParams.MoreHosts;
      while i <= Length(s) do
        Result := Result + ',' + FDExtractFieldName(s, i);
  end;
  Result := Result + '/';
  if AConnectionDef.HasValue(S_FD_ConnParam_Common_LoginTimeout) or
     AConnectionDef.HasValue(S_FD_ConnParam_Mongo_ReadTimeout) or
     AConnectionDef.HasValue(S_FD_ConnParam_Mongo_MongoAdvanced) or
     AConnectionDef.HasValue(S_FD_ConnParam_Common_Database) then begin
    if AConnectionDef.HasValue(S_FD_ConnParam_Common_Database) then
      Result := Result + oParams.Database;
    lWasOpt := False;
    if AConnectionDef.HasValue(S_FD_ConnParam_Common_LoginTimeout) then
      AddOpt('connectTimeoutMS=' + IntToStr(oParams.LoginTimeout * 1000));
    if AConnectionDef.HasValue(S_FD_ConnParam_Mongo_ReadTimeout) then
      AddOpt('socketTimeoutMS=' + IntToStr(oParams.ReadTimeout * 1000));
    if AConnectionDef.HasValue(S_FD_ConnParam_Mongo_MongoAdvanced) then begin
      i := 1;
      s := oParams.MongoAdvanced;
      while i <= Length(s) do
        AddOpt(FDExtractFieldName(s, i));
    end;
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoConnection.InternalConnect;
begin
  // TMongoEnv creation moved to InternalConnect from Create, because on ARC
  // platforms destruction of FEnv in Destroy leaded to Self->FEnv->FError->Self
  // circular destruction sequence
  FEnv := TMongoEnv.Create(TFDPhysMongoDriver(DriverObj).FCLib,
    TFDPhysMongoDriver(DriverObj).FBLib, Self);
  FEnv.DateTimeZoneHandling := (ConnectionDef.Params as TFDPhysMongoConnectionDefParams).TimeZone;

  if InternalGetSharedCliHandle() <> nil then
    FConnection := TMongoConnection.CreateUsingHandle(FEnv,
      Pmongoc_handle_t(InternalGetSharedCliHandle()), Self)
  else
    FConnection := TMongoConnection.Create(FEnv, Self);
{$IFDEF FireDAC_MONITOR}
  InternalTracingChanged;
{$ENDIF}

  if InternalGetSharedCliHandle() = nil then begin
    FConnection.Open(BuildMongoConnectString(ConnectionDef));
    if ConnectionDef.AsBoolean[S_FD_ConnParam_Mongo_UseSSL] then
      FConnection.SSLInit(
        ConnectionDef.AsString[S_FD_ConnParam_Mongo_SSL_PEMFile],
        ConnectionDef.AsString[S_FD_ConnParam_Mongo_SSL_PEMPassword],
        ConnectionDef.AsString[S_FD_ConnParam_Mongo_SSL_CAFile],
        '', '',
        ConnectionDef.AsBoolean[S_FD_ConnParam_Mongo_SSL_WeakValidation],
        ConnectionDef.AsBoolean[S_FD_ConnParam_Mongo_SSL_AllowInvHost]);

    // Open only initializes connection parameters. The connection will be
    // really open on first command (communication) to the server. To handle
    // incorrect credentials, server inavailability, etc perform Ping command.
    FConnection.Ping;
  end;

  FServerVersion := FConnection.ServerVersion;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoConnection.InternalSetMeta;
begin
  inherited InternalSetMeta;
  if FDefaultCatalog = '' then
    FDefaultCatalog := 'test';
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoConnection.InternalDisconnect;
begin
  FDFreeAndNil(FConnection);
  FDFreeAndNil(FEnv);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoConnection.InternalPing;
begin
  FConnection.Ping;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoConnection.InternalExecuteDirect(const ASQL: String;
  ATransaction: TFDPhysTransaction);
begin
                      
  FDCapabilityNotSupported(Self, [S_FD_LPhys, DriverObj.DriverID]);
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.GetItemCount: Integer;
begin
  Result := inherited GetItemCount;
  if DriverObj.State in [drsLoaded, drsActive] then begin
    Inc(Result, 1);
    if FConnection <> nil then
      Inc(Result, 1);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoConnection.GetItem(AIndex: Integer; out AName: String;
  out AValue: Variant; out AKind: TFDMoniAdapterItemKind);
begin
  if AIndex < inherited GetItemCount then
    inherited GetItem(AIndex, AName, AValue, AKind)
  else
    case AIndex - inherited GetItemCount of
    0:
      begin
        AName := 'Client version';
        AKind := ikClientInfo;
        if TFDPhysMongoDriver(DriverObj).FCLib.VersionStr <> '' then
          AValue := TFDPhysMongoDriver(DriverObj).FCLib.VersionStr
        else
          AValue := '<unknown>';
      end;
    1:
      begin
        AName := 'Server version';
        AKind := ikSessionInfo;
        if (FConnection <> nil) and (FConnection.Handle <> nil) then
          AValue := FDVerInt2Str(FServerVersion)
        else
          AValue := '<unknown>';
      end;
    end;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.GetCliObj: Pointer;
begin
  Result := FConnection;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoConnection.InternalGetCliHandle: Pointer;
begin
  if FConnection <> nil then
    Result := FConnection.Handle
  else
    Result := nil;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMongoTailThread                                                        }
{-------------------------------------------------------------------------------}
type
  TFDPhysMongoTailThread = class(TThread)
  private
    [weak] FAlerter: TFDPhysMongoEventAlerter;
  protected
    procedure Execute; override;
  public
    constructor Create(AAlerter: TFDPhysMongoEventAlerter);
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysMongoTailThread.Create(AAlerter: TFDPhysMongoEventAlerter);
begin
  inherited Create;
  FAlerter := AAlerter;
  FreeOnTerminate := True;
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysMongoTailThread.Destroy;
begin
  FAlerter.FTailThread := nil;
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoTailThread.Execute;
var
  oConn: TMongoConnection;
  oCrs: IMongoCursor;
  rLastId: TJsonOid;
  oIter: TJSONIterator;
  oDoc: TMongoDocument;
begin
  oConn := TMongoConnection(FAlerter.FEventsConnection.CliObj);
  oDoc := nil;
  try
    try
      oCrs := oConn[FAlerter.FDB][FAlerter.FColl].Aggregate([])
        .Group()
          .Add('_id', '1')
          .BeginObject('lastId')
            .Add('$max', '$_id')
          .EndObject
        .&End;
      if oCrs.Next then begin
        oIter := oCrs.Doc.Iterator;
        try
          oIter.Next('lastId');
          rLastId := oIter.AsOid;
        finally
          FDFree(oIter);
        end;
      end;

      while not Terminated and FAlerter.IsRunning do begin
        oCrs := oConn[FAlerter.FDB][FAlerter.FColl]
          .Find([TMongoQueryFlag.TailableCursors, TMongoQueryFlag.AwaitData])
          .Match()
            .BeginObject('_id')
              .Add('$gt', rLastId)
            .EndObject
          .&End
          .Sort()
            .Ascending(['$natural'])
          .&End;

        while not Terminated and FAlerter.IsRunning do begin
          if oDoc = nil then
            oDoc := oConn.Env.NewDoc;
          if not oCrs.Next(oDoc) then begin
            if not oCrs.IsAlive then begin
              Sleep(100);
              Break;
            end;
            Continue;
          end;

          oIter := oDoc.Iterator;
          try
            oIter.Next('_id');
            rLastId := oIter.AsOid;
          finally
            FDFree(oIter);
          end;
          FAlerter.DoFired(oDoc);
          oDoc := nil;
        end;
      end;
    except
      on E: EFDDBEngineException do
        if E.Kind <> ekCmdAborted then begin
          Terminate;
          FAlerter.AbortJob;
        end;
    end;
  finally
    if oDoc <> nil then
      FDFree(oDoc);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMongoEventMessage                                                      }
{-------------------------------------------------------------------------------}
type
  TFDPhysMongoEventMessage = class(TFDPhysEventMessage)
  private
    FName: String;
    FDoc: TMongoDocument;
  public
    constructor Create(const AName: String; ADoc: TMongoDocument);
    destructor Destroy; override;
  end;

{-------------------------------------------------------------------------------}
constructor TFDPhysMongoEventMessage.Create(const AName: String; ADoc: TMongoDocument);
begin
  inherited Create;
  FName := AName;
  FDoc := ADoc;
end;

{-------------------------------------------------------------------------------}
destructor TFDPhysMongoEventMessage.Destroy;
begin
  FDFreeAndNil(FDoc);
  inherited Destroy;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMongoEventAlerter                                                      }
{-------------------------------------------------------------------------------}
const
  C_WakeUpEvent = C_FD_SysNamePrefix + 'WAKEUP';
  C_DefDBName = 'test';
  C_DefCollSize = 10000;

procedure TFDPhysMongoEventAlerter.InternalAllocHandle;
var
  i: Integer;
begin
  if GetNames().Count <> 1 then
    FDException(Self, [S_FD_LPhys, S_FD_MongoId], er_FD_MongoAlertToMany, []);

  FColl := GetNames()[0];

  i := Pos('=', FColl);
  if i > 0 then begin
    FSize := StrToIntDef(Copy(FColl, i + 1, MaxInt), C_DefCollSize);
    FColl := Copy(FColl, 1, i - 1);
  end
  else
    FSize := C_DefCollSize;

  i := Pos('.', FColl);
  if i > 0 then begin
    FDB := Copy(FColl, 1, i - 1);
    FColl := Copy(FColl, i + 1, MaxInt);
  end
  else begin
    FDB := GetConnection.ConnectionDef.Params.Database;
    if FDB = '' then
      FDB := C_DefDBName;
  end;

  FEventsConnection := GetConnection.Clone;
  if FEventsConnection.State = csDisconnected then
    FEventsConnection.Open;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoEventAlerter.InternalRegister;
var
  oConn: TMongoConnection;
  oOpts: TMongoDocument;
begin
  oConn := TMongoConnection(FEventsConnection.CliObj);

  if not oConn[FDB].HasCollection(FColl) then begin
    oOpts := oConn.Env.NewDoc;
    try
      oOpts.Add('capped', True);
      oOpts.Add('size', FSize);
      oConn[FDB].CreateCollection(FColl, oOpts);
      oConn[FDB][FColl]
        .Insert()
          .Values()
            .Add('name', C_WakeUpEvent)
          .&End
        .Exec;
    finally
      FDFree(oOpts);
    end;
  end;

  FTailThread := TFDPhysMongoTailThread.Create(Self);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoEventAlerter.DoFired(ADoc: TMongoDocument);
var
  oIter: TJSONIterator;
  lEnqueue: Boolean;
  sName: String;
begin
  lEnqueue := True;
  sName := '';
  oIter := ADoc.Iterator;
  try
    if oIter.Find('name') then begin
      sName := oIter.AsString;
      lEnqueue := CompareText(sName, C_WakeUpEvent) <> 0;
    end;
  finally
    FDFree(oIter);
  end;
  if lEnqueue then
    FMsgThread.EnqueueMsg(TFDPhysMongoEventMessage.Create(sName, ADoc))
  else
    FDFree(ADoc);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoEventAlerter.InternalHandle(AEventMessage: TFDPhysEventMessage);
var
  oMsg: TFDPhysMongoEventMessage;
begin
  oMsg := TFDPhysMongoEventMessage(AEventMessage);
  InternalHandleEvent(oMsg.FName, oMsg.FDoc.AsJSON);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoEventAlerter.InternalAbortJob;
begin
  if FTailThread <> nil then begin
    FTailThread.Terminate;
    InternalSignal(C_WakeUpEvent, Null);
    while (FTailThread <> nil) and (FTailThread.ThreadID <> TThread.Current.ThreadID) do
      Sleep(1);
  end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoEventAlerter.InternalReleaseHandle;
begin
  FTailThread := nil;
  FEventsConnection := nil;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoEventAlerter.InternalSignal(const AEvent: String;
  const AArgument: Variant);
var
  oConn: TMongoConnection;
  oIns: TMongoInsert;
  s: String;
begin
  oConn := TMongoConnection(GetConnection.CliObj);
  oIns := TMongoInsert.Create(oConn.Env);
  try
    oIns.Values().Add('name', AEvent);
    if not (VarIsNull(AArgument) or VarIsEmpty(AArgument)) then begin
      s := VarToStr(AArgument);
      if (Length(s) > 0) and (s[1] = '{') and (s[Length(s)] = '}') then
        oIns.Values().Append(s)
      else
        oIns.Values().Add('arg', AArgument);
    end;
    oConn[FDB][FColl].Insert(oIns);
  finally
    FDFree(oIns);
  end;
end;

{-------------------------------------------------------------------------------}
{ TFDPhysMongoTransaction                                                       }
{-------------------------------------------------------------------------------}
procedure TFDPhysMongoTransaction.InternalAllocHandle;
var
  oLib: TMongoClientLib;
begin
  oLib := TMongoConnection(GetConnection.CliObj).CLib;
  if (oLib is TMongoClientLibExt) and
     Assigned(TMongoClientLibExt(oLib).Fmongoc_client_start_session) and
     (TFDPhysMongoConnection(ConnectionObj).FServerVersion >= mvMongo030600) then
    FSession := TMongoConnection(GetConnection.CliObj).NewSession;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoTransaction.InternalReleaseHandle;
begin
  FDFreeAndNil(FSession);
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoTransaction.InternalChanged;
var
  eLevel: TMongoReadConcern.TReadLevel;
begin
  if FSession <> nil then
    if xoIsolation in GetOptions.Changed then begin
      case GetOptions.Isolation of
      xiDirtyRead,
      xiReadCommitted,
      xiRepeatableRead: eLevel := TMongoReadConcern.TReadLevel.Local;
      xiSnapshot:       eLevel := TMongoReadConcern.TReadLevel.Snapshot;
      xiSerializible:   eLevel := TMongoReadConcern.TReadLevel.Linearizable;
      else              eLevel := TMongoReadConcern.TReadLevel.Default;
      end;
      FSession.Options.ReadConcern.Level := eLevel;
    end;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoTransaction.InternalStartTransaction(ATxID: LongWord);
begin
  if FSession <> nil then
    FSession.StartTransaction;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoTransaction.InternalCommit(ATxID: LongWord);
begin
  if FSession <> nil then
    FSession.CommitTransaction;
end;

{-------------------------------------------------------------------------------}
procedure TFDPhysMongoTransaction.InternalRollback(ATxID: LongWord);
begin
  if FSession <> nil then
    FSession.AbortTransaction;
end;

{-------------------------------------------------------------------------------}
function TFDPhysMongoTransaction.GetCliObj: Pointer;
begin
  Result := FSession;
end;

{-------------------------------------------------------------------------------}
initialization
  FDRegisterDriverClass(TFDPhysMongoDriver);

finalization
  FDUnregisterDriverClass(TFDPhysMongoDriver);

end.
