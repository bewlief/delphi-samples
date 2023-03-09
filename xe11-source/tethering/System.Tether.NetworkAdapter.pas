{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Tether.NetworkAdapter;

interface

{$SCOPEDENUMS ON}

uses
  System.SysUtils, System.SyncObjs, System.Tether.Manager, System.Tether.Comm,
  System.Generics.Collections,
  IPPeerAPI;

type
  TTetheringNetworkAdapterCommon = class;

  TTetheringNetworkManagerCommunicationThread = class(TTetheringManagerCommunicationThread)
  private const
    TetheringNewManager            = 'TetheringNewManager';
    TetheringShutdown              = 'TetheringShutdown';
    TetheringRemoteConnection      = 'TetheringRemoteConnection';
  private
    FIPVersion: TCommIPVersion;
    FLastConnection: string;
    FClientComm: TTetheringNetworkComm;
    FTarget: string;
    FSubnetTable: TArray<TIPv4Subnet>;

    FServerComm: TTetheringNetworkServerComm;
    FClientConnections: TObjectDictionary<string, TTetheringNetworkComm>;

    procedure DoOnExecute(const AConnection: TTetheringCustomComm);
    procedure DoOnConnect(const AConnection: TTetheringCustomComm);
    procedure DoOnDisconnect(const AConnection: TTetheringCustomComm);

    procedure PrepareClientConnection(const AConnection: string);
  protected
    /// <summary>Callback used to receive data from ServerComm</summary>
    procedure DoOnReceiveData(const AConnection: string; const AData: TBytes); virtual;

    /// <summary>Callback used to handle socket exceptions in ServerComm</summary>
    procedure DoOnReceiveException(const AMessage : String; const AExceptionClass: TClass);

    /// <summary>Merges two connection strings updating the first one with the port of the second.</summary>
    function MergeConnectionString(const AConnectionWithIP, AConnectionWithPort: string): string;

    function IsListening: Boolean; override;
    /// <summary>Starts listening using a specific target port</summary>
    function StartListening(const ATarget: string): Boolean; virtual;
    /// <summary>Stops listening</summary>
    procedure StopListening; virtual;

    function GetRemoteConnectionString(const AConnection: string): string; override;
    function ProcessResponse(const AData: string; var RemoteConnectionString: string): string; override;
    procedure DoSendCommand(const AConnection, AData: string); override;
    /// <summary>Broacasts a command to others managers</summary>
    /// <param name="AData">Data to be sent</param>
    /// <param name="AHost">If specified, the data is sent to a specific host</param>
    procedure BroadcastCommand(const AData: string; const AHost: string = ''); virtual; abstract;
  public
    constructor Create(const AnAdapter: TTetheringAdapter; AIPVersion: TCommIPVersion; const ABindToAddress: string); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  /// <summary>Class that handles manager communications using UDP</summary>
  TTetheringNetworkManagerCommunicationThread_UDP = class(TTetheringNetworkManagerCommunicationThread)
  private
    FServerCommUDP: TTetheringNetworkServerCommUDP;
  protected
    function IsListening: Boolean; override;
    /// <summary>Starts listening using a specific target port</summary>
    function StartListening(const ATarget: string): Boolean; override;
    /// <summary>Stops listening</summary>
    procedure StopListening; override;

    procedure BroadcastCommand(const AData: string; const AHost: string = ''); override;
  public
    constructor Create(const AnAdapter: TTetheringAdapter; AIPVersion: TCommIPVersion; const ABindToAddress: string);
    destructor Destroy; override;
  end;

  /// <summary>Class that handles manager communications using Multicast</summary>
  TTetheringNetworkManagerCommunicationThread_Multicast = class(TTetheringNetworkManagerCommunicationThread)
  private
    FServerCommMulticast: TTetheringNetworkServerCommMulticast;
  protected
    function IsListening: Boolean; override;
    /// <summary>Starts listening using a specific target port</summary>
    function StartListening(const ATarget: string): Boolean; override;
    /// <summary>Stops listening</summary>
    procedure StopListening; override;

    procedure BroadcastCommand(const AData: string; const AHost: string = ''); override;
  public
    constructor Create(const AnAdapter: TTetheringAdapter; AIPVersion: TCommIPVersion; const ABindToAddress: string);
    destructor Destroy; override;
  end;

  /// <summary>Class that implements common features for Network Adapters</summary>
  TTetheringNetworkAdapterCommon = class(TTetheringAdapter)
  public const
    FirstManagerPort = 2020;
    LastManagerPort = 2039;
    FirstAvPort = 2040;
    LastAvPort  = 2110;
  private
    [Weak] FCommunicationThread: TTetheringNetworkManagerCommunicationThread;

    procedure SendManagerCommand(const ATarget: string; const ACommand: TTetheringManagerCommand);
  protected
    /// <summary>IP Version that is going to be used by the NetworkAdapter</summary>
    FIPVersion: TCommIPVersion;

    /// <summary>Checks that if specified target is an IP Address</summary>
    function IsIP(const ATarget: string): Boolean; virtual;
    /// <summary>Checks that if specified target is a range of IP Addresses</summary>
    function IsIPRange(const ATarget: string): Boolean; virtual;

    /// <summary>Sends a Manager Command to a specific range of targets </summary>
    procedure SendManagerCommandToRange(const ATarget: string; const ACommand: TTetheringManagerCommand); virtual;

    function GetAutomaticTimeout: Cardinal; override;

    procedure DoDiscoverManagers(Timeout: Cardinal; const ATargetList: TTetheringTargetHosts;
      const AProfileGroups, AProfileTexts: TArray<string>); override;
    procedure DoStopListening; override;
  public
    constructor Create(AIPVersion: TCommIPVersion); reintroduce;
    destructor Destroy; override;

    function GetClientPeer(const ProtocolId: TTetheringProtocolType): TTetheringCustomComm; override;
    function GetServerPeer(const ProtocolId: TTetheringProtocolType): TTetheringCustomServerComm; override;

    function MaxConnections: Integer; override;
    function GetTargetConnection(const Port, Offset: Integer): string; override;

    procedure NotifyShutdown; override;
  end;

  /// <summary>Class that implements an IP_V4 Network Adapter that uses UDP</summary>
  TTetheringNetworkAdapterV4_UDP = class(TTetheringNetworkAdapterCommon)
  public const
    AdapterID = 'Network';      // do not localize
  protected
    function GetAdapterType: TTetheringAdapterType; override;

    function DoCreateCommunicationThread(const AnAdapter: TTetheringAdapter; const ABindToAddress: string): TTetheringManagerCommunicationThread; override;
  public
    constructor Create(AIPVersion: TCommIPVersion);
    class function CreateInstance: TTetheringAdapter; override;
  end;

  /// <summary>Class that implements common features for Mulicast Network Adapters</summary>
  TTetheringNetworkAdapterMulticast = class(TTetheringNetworkAdapterCommon)
  protected
    function DoCreateCommunicationThread(const AnAdapter: TTetheringAdapter; const ABindToAddress: string): TTetheringManagerCommunicationThread; override;
  public
    constructor Create(AIPVersion: TCommIPVersion);
  end;

  /// <summary>Class that implements an IP_V4 Network Adapter that uses Multicast</summary>
  TTetheringNetworkAdapterMulticast_V4 = class(TTetheringNetworkAdapterMulticast)
  public const
    AdapterID = 'Network_V4';      // do not localize
  protected
    function GetAdapterType: TTetheringAdapterType; override;
  public
    class function CreateInstance: TTetheringAdapter; override;
  end;

  /// <summary>Class that implements an IP_V6 Network Adapter that uses Multicast</summary>
  TTetheringNetworkAdapterMulticast_V6 = class(TTetheringNetworkAdapterMulticast)
  public const
    AdapterID = 'Network_V6';      // do not localize
  protected
    function IsIP(const ATarget: string): Boolean; override;
    function IsIPRange(const ATarget: string): Boolean; override;
    procedure SendManagerCommandToRange(const ATarget: string; const ACommand: TTetheringManagerCommand); override;
    function GetAdapterType: TTetheringAdapterType; override;
  public
    class function CreateInstance: TTetheringAdapter; override;
  end;

implementation

uses
  System.Classes, System.Rtti, System.TypInfo, System.StrUtils, System.Tether.Consts;

const
  TetheringExceptionMarker = #27#27#27'Exception';

type
  TTetheringAdapterFriend = class(TTetheringAdapter)
    // Needed to access protected class method Log
  end;

function CheckExceptionName(const E: Exception; const AName: string): Boolean;
var
  LClass: TClass;
begin
  LClass := E.ClassType;
  while LClass <> nil do
  begin
     if LClass.ClassNameIs(AName) then
       Break;
     LClass := LClass.ClassParent;
  end;
  Result := LClass <> nil;
end;


{ TTetheringNetworkAdapterCommon }

constructor TTetheringNetworkAdapterCommon.Create(AIPVersion: TCommIPVersion);
begin
  inherited Create;
  FIPVersion := AIPVersion;
end;

destructor TTetheringNetworkAdapterCommon.Destroy;
begin
  inherited;
end;

procedure TTetheringNetworkAdapterCommon.DoDiscoverManagers(Timeout: Cardinal;
  const ATargetList: TTetheringTargetHosts; const AProfileGroups, AProfileTexts: TArray<string>);
var
  LTarget: string;
  LCommand: TTetheringManagerCommand;
  LGroups, LTexts: string;
begin
  inherited;
  if (Length(AProfileGroups) > 0) or (Length(AProfileTexts) > 0) then
  begin
    if Length(AProfileGroups) > 0 then
      LGroups := TTetheringManagerCommand.Join(';', AProfileGroups)
    else
      LGroups := TTetheringNetworkManagerCommunicationThread.EMPTYTOKEN;

    if Length(AProfileTexts) > 0 then
      LTexts := TTetheringManagerCommand.Join(';', AProfileTexts)
    else
      LTexts := TTetheringNetworkManagerCommunicationThread.EMPTYTOKEN;

    LCommand := TTetheringManagerCommand.Create(TTetheringNetworkManagerCommunicationThread.TetheringDiscoverManagers,
      FAdapterConnectionString, Manager.Version, [FCommunicationThread.FTarget, LGroups, LTexts]);
  end
  else
    LCommand := TTetheringManagerCommand.Create(TTetheringNetworkManagerCommunicationThread.TetheringDiscoverManagers,
      FAdapterConnectionString, Manager.Version, [FCommunicationThread.FTarget]);

  if Length(ATargetList) > 0 then
  begin
    for LTarget in ATargetList do
    begin
      if IsIP(LTarget) then
      begin
        if IsIPRange(LTarget) then
          SendManagerCommandToRange(LTarget, LCommand)
        else
          SendManagerCommand(LTarget, LCommand);
      end
      else
        // Can get an exception if hostname is not resolved
        try
          SendManagerCommand(LTarget, LCommand);
        except
          on E: Exception do
            if not CheckExceptionName(E, 'EIdSocketError') then // Do not translate
              raise;
        end;
    end;
  end
  else
    SendManagerCommand('', LCommand);
end;

procedure TTetheringNetworkAdapterCommon.DoStopListening;
begin
  NotifyShutdown;
  inherited DoStopListening;
end;

function TTetheringNetworkAdapterCommon.GetAutomaticTimeout: Cardinal;
begin
  // Default Timeout for Network Adapters
  Result := 2900;
end;

function TTetheringNetworkAdapterCommon.GetClientPeer(const ProtocolId: TTetheringProtocolType): TTetheringCustomComm;
begin
  Result := TTetheringNetworkComm.Create(nil, '', FIPVersion);
end;

function TTetheringNetworkAdapterCommon.GetServerPeer(const ProtocolId: TTetheringProtocolType): TTetheringCustomServerComm;
begin
  Result := TTetheringNetworkServerComm.Create(FIPVersion);
end;

function TTetheringNetworkAdapterCommon.GetTargetConnection(const Port, Offset: Integer): string;
begin
  Result := (FirstAvPort + Port).ToString;
end;

function TTetheringNetworkAdapterCommon.IsIP(const ATarget: string): Boolean;
var
  LIPByteStrings: TArray<string>;
  LIPByteStr: string;
  LIPByte: Integer;
begin
  Result := True;
  LIPByteStrings := ATarget.Split(['.']);
  if (Length(LIPByteStrings) = 4) then
  begin
    for LIPByteStr in LIPByteStrings do
    begin
      LIPByte := StrToIntDef(LIPByteStr, -1);
      if (LIPByte < 0) or (LIPByte > 255) then
        Exit(False);
    end;
  end
  else
    Result := False;
end;

function TTetheringNetworkAdapterCommon.IsIPRange(const ATarget: string): Boolean;
var
  LPos: integer;
begin
  LPos := ATarget.LastIndexOf('.');
  Result := (LPos > 0) and (ATarget.Substring(LPos + 1).ToInteger = 0);
end;

function TTetheringNetworkAdapterCommon.MaxConnections: Integer;
begin
  Result := LastAvPort - FirstAvPort + 1;
end;

procedure TTetheringNetworkAdapterCommon.NotifyShutdown;
var
  LCommand: string;
begin
  LCommand := TTetheringManagerCommand.Create(TTetheringNetworkManagerCommunicationThread.TetheringShutdown,
    FAdapterConnectionString, Manager.Version, [Manager.Identifier]).ToString;

  FCommunicationThread.BroadcastCommand(LCommand);
end;

procedure TTetheringNetworkAdapterCommon.SendManagerCommandToRange(const ATarget: string;
  const ACommand: TTetheringManagerCommand);
var
  I: integer;
  LNet: string;
begin
  LNet := ATarget.Substring(0, ATarget.LastIndexOf('.') + 1);
  for I := 1 to 254 do
    SendManagerCommand(LNet + I.ToString, ACommand);
end;

procedure TTetheringNetworkAdapterCommon.SendManagerCommand(const ATarget: string; const ACommand: TTetheringManagerCommand);
begin
  FCommunicationThread.BroadcastCommand(ACommand.ToString, ATarget);
end;


{ TTetheringNetworkManagerCommunicationThread }

constructor TTetheringNetworkManagerCommunicationThread.Create(const AnAdapter: TTetheringAdapter; AIPVersion: TCommIPVersion;
  const ABindToAddress: string);
begin
  inherited Create(AnAdapter);
  FIPVersion := AIPVersion;
  FServerComm := TTetheringNetworkServerComm.Create(AIPVersion);
  FServerComm.OnExecute := DoOnExecute;
  FServerComm.OnConnect := DoOnConnect;
  FServerComm.OnDisconnect := DoOnDisconnect;

  FClientConnections := TObjectDictionary<string, TTetheringNetworkComm>.Create([doOwnsValues], 10);

  if ABindToAddress = '' then
  begin
    FSubnetTable := GStackPeers.GetIPv4Subnets;
    if Length(FSubnetTable) < 2 then
      FSubnetTable := nil;
  end;
end;

destructor TTetheringNetworkManagerCommunicationThread.Destroy;
begin
  inherited;
  FClientConnections.Free;
  FServerComm.Free;
end;

procedure TTetheringNetworkManagerCommunicationThread.Execute;
var
  I: Integer;
  LRemoteConnectionString: string;
  LResp: string;
  LCommandParts: TArray<string>;
  LListening: Boolean;
  LPort: string;
begin
  inherited;
  try
    try
      try
        for I := TTetheringNetworkAdapterCommon.FirstManagerPort to TTetheringNetworkAdapterCommon.LastManagerPort do
        begin
          if StartListening(I.ToString) then
            Break;
          StopListening;
        end;

        LPort := I.ToString;
        LListening := IsListening;
        if not LListening then
          raise ETetheringException.Create(SManagerNetworkCreation);

        LRemoteConnectionString := EMPTYTOKEN + TetheringConnectionSeparator + FTarget;
        TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString := LRemoteConnectionString;

        LResp := TTetheringManagerCommand.Create(TTetheringNetworkManagerCommunicationThread.TetheringNewManager,
          LRemoteConnectionString, FAdapter.Manager.Version,
          [FAdapter.Manager.Identifier, FAdapter.Manager.Name, FAdapter.Manager.Text, FTarget]).ToString;

        BroadcastCommand(LResp)
      finally
        CommunicationThreadInitialized;
      end;

      while not Terminated do
      begin
        FCommandQueue.WaitCommand;
        while not Terminated and (FCommandQueue.Count > 0) do
        begin
          try
            LCommandParts := TTetheringManagerCommand.Split(FCommandQueue.Dequeue, TetheringSeparator);
            if LCommandParts[0] = TetheringExceptionMarker then
            begin
              if LCommandParts[1].Contains('# 57') then // Socket error # 57 Socket not connected
              begin
                StopListening;
                StartListening(LPort);
              end;
            end;
            LRemoteConnectionString := LCommandParts[0];
            LResp := ProcessResponse(LCommandParts[1], LRemoteConnectionString);
            if (not Terminated) and (LResp <> TetheringEmpty) then
              SendCommand(LRemoteConnectionString, LResp);
          except
            on E: Exception do
              if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
                TTetheringAdapterFriend.Log(Format('TTetheringNetworkManagerCommunicationThread.Execute(%s): %s %s', [FAdapter.AdapterType, E.ClassName, E.Message]));
          end;
        end;
      end;

    finally
      Sleep(50);
      StopListening;
    end;
  except
    on E: Exception do
      if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
        TTetheringAdapterFriend.Log(Format('TTetheringNetworkManagerCommunicationThread.Execute(%s): %s %s', [FAdapter.AdapterType, E.ClassName, E.Message]));
  end;
end;

function TTetheringNetworkManagerCommunicationThread.GetRemoteConnectionString(const AConnection: string): string;
begin
  Result := AConnection;
end;

function TTetheringNetworkManagerCommunicationThread.IsListening: Boolean;
begin
  Result := FServerComm.Active;
end;

procedure TTetheringNetworkManagerCommunicationThread.DoOnConnect(const AConnection: TTetheringCustomComm);
begin

end;

procedure TTetheringNetworkManagerCommunicationThread.DoOnDisconnect(const AConnection: TTetheringCustomComm);
begin

end;

procedure TTetheringNetworkManagerCommunicationThread.DoOnExecute(const AConnection: TTetheringCustomComm);
var
  LCommand: TBytes;
  LStrCommand: string;
  LTmpCommand: string;
  LAux: TBytes;
  LCommands: TArray<string>;
  LRemoteConnection: string;
  LCount: Integer;
begin
  LCommand := AConnection.ReadData;
  if Length(LCommand) > 0 then
  begin
    LCount := 0;
    while LCommand[High(LCommand)] <> Ord(TetheringCommandSeparator) do
    begin
      if LCount > 5000 then
        Exit;
      LAux := AConnection.ReadData;
      if Length(LAux) = 0 then
      begin
        Inc(LCount);
        Sleep(1);
      end
      else
      begin
        LCount := 0;
        LCommand := LCommand + LAux;
      end;
    end;

    try
      LStrCommand := TEncoding.UTF8.GetString(LCommand);
    except
      Exit;
    end;

    LRemoteConnection := TTetheringNetworkComm(AConnection).RemoteConnectionString;
    LCommands := TTetheringManagerCommand.Split(LStrCommand, TetheringCommandSeparator);
    for LStrCommand in LCommands do
      if LStrCommand <> '' then
      begin
        LTmpCommand := TTetheringManagerCommand.Join(TetheringSeparator, [LRemoteConnection, LStrCommand]);
        if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
            TTetheringNetworkAdapterCommon.Log('TCP OnExecute "' + LTmpCommand + '"');
        FCommandQueue.Enqueue(LTmpCommand);
      end;

  end;
end;

procedure TTetheringNetworkManagerCommunicationThread.DoOnReceiveData(const AConnection: string; const AData: TBytes);
var
  Data: string;
  Host: string;
  Port: string;
  LConnection: string;
  LParts: TArray<string>;
  LCommand: TTetheringManagerCommand;
  LCommands: TArray<string>;
  LTmpCommand: string;
begin
  if not Terminated then
  begin
    LParts := AConnection.Split([TetheringConnectionSeparator]);
    Host := LParts[0];
    Port := LParts[1];
    LConnection := AConnection;

    try
      Data := TEncoding.UTF8.GetString(TBytes(AData));
    except
      Exit;
    end;

    if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
      TTetheringNetworkAdapterCommon.Log('DoOnReceiveData("' + AConnection + '") Data: "' + Data + '"');
    LCommands := TTetheringManagerCommand.Split(Data, TetheringCommandSeparator);
    for Data in LCommands do
      if Data <> '' then
      begin
        LCommand.DecodeCommand(Data);
        if not LCommand.ValidCommand then
          Exit;

        if (LCommand.CommandText = TetheringNewManager) and (LCommand.NumParams >= 4) then
        begin
          if LCommand.ConnectionString.StartsWith(EMPTYTOKEN) then
          begin
            // If command comes from an older Network Adapter we preprocess the connection string...
            if LCommand.ConnectionString = EMPTYTOKEN then
              LCommand.ConnectionString := LCommand.ConnectionString + TetheringConnectionSeparator + LCommand.Params[3];

            if (TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString.StartsWith(EMPTYTOKEN) or
                (TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString = '')) and
               (LCommand.Params[0] = FAdapter.Manager.Identifier) then
            begin
              TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString :=
                MergeConnectionString(IfThen(Assigned(FSubnetTable), TetheringMULTIHOMED, LConnection), TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString);
              LCommand.ConnectionString := TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString;
              LConnection := LCommand.ConnectionString;
              if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
                TTetheringNetworkAdapterCommon.Log('DoOnReceiveData Updating Connection string to: "' + LCommand.ConnectionString + '"');
            end;
          end;
        end;

        LConnection := MergeConnectionString(LConnection, LCommand.ConnectionString);

        if LConnection <> TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString then  // To avoid echo
        begin
          LCommands := TTetheringManagerCommand.Split(LCommand.ToString, TetheringCommandSeparator);

          LTmpCommand := TTetheringManagerCommand.Join(TetheringSeparator, [LConnection, LCommands[0]]);
          if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
            TTetheringNetworkAdapterCommon.Log('DoOnReceiveData - Enqueue Command: "' + LTmpCommand + '"');
          FCommandQueue.Enqueue(LTmpCommand);
        end;
      end;
  end;
end;

procedure TTetheringNetworkManagerCommunicationThread.DoOnReceiveException(
  const AMessage: String; const AExceptionClass: TClass);
begin
  FCommandQueue.Enqueue(TetheringExceptionMarker + TetheringSeparator + AMessage);
end;

procedure TTetheringNetworkManagerCommunicationThread.DoSendCommand(const AConnection, AData: string);
var
  I: Integer;
  LConnectionI32: UInt32;
  LData: string;
begin
  if AData <> TetheringEmpty then
  begin
    if AData.StartsWith(TetheringRequestPair) then
      FLastConnection := '';
    TMonitor.Enter(FClientConnections);
    try
      PrepareClientConnection(AConnection);

      LData := AData;
      if Assigned(FSubnetTable) then
      begin
        LConnectionI32 := GStackPeers.IPv4ToUInt32(AConnection);
        for I := High(FSubnetTable) downto Low(FSubnetTable) do
          if (I = Low(FSubnetTable)) or ((LConnectionI32 and FSubnetTable[I].Mask) = FSubnetTable[I].Subnet) then
          begin
            LData := StringReplace(LData, '|' + TetheringMULTIHOMED + '$', '|' + FSubnetTable[I].Address + '$', [rfReplaceAll]);
            LData := StringReplace(LData, ',' + TetheringMULTIHOMED + '$', ',' + FSubnetTable[I].Address + '$', [rfReplaceAll]);
            Break;
          end;
      end;

      if FClientComm.Connected then
      begin
        try
          FClientComm.WriteData(TEncoding.UTF8.GetBytes(LData));
        except
          FLastConnection := '';
                                                                     
          raise ETetheringException.CreateResFmt(@SNoConnections, [AConnection]);
        end;
      end
      else
      begin
        FLastConnection := '';
                                                                   
        raise ETetheringException.CreateResFmt(@SNoConnections, [AConnection]);
      end;
    finally
      TMonitor.Exit(FClientConnections);
    end;
  end;
end;

function TTetheringNetworkManagerCommunicationThread.MergeConnectionString(const AConnectionWithIP, AConnectionWithPort: string): string;
var
  LParts: TArray<string>;
begin
  LParts := TTetheringManagerCommand.Split(AConnectionWithIP, TetheringConnectionSeparator);
  Result := LParts[0];
  LParts := TTetheringManagerCommand.Split(AConnectionWithPort, TetheringConnectionSeparator);
  Result := Result + TetheringConnectionSeparator + LParts[1]
end;

procedure TTetheringNetworkManagerCommunicationThread.PrepareClientConnection(const AConnection: string);
var
  LConnInfo: TPair<string, TTetheringNetworkComm>;
  LRemoveConn: TList<string>;
  LKey: string;
begin
  if AConnection <> FLastConnection then
  begin
    if not FClientConnections.TryGetValue(AConnection, FClientComm) then
    begin
      FClientComm := TTetheringNetworkComm.Create(nil, '', FIPVersion);
      FClientConnections.Add(AConnection, FClientComm);
    end;
    FLastConnection := AConnection;
  end;
  if not FClientComm.Connected then
    FClientComm.Connect(AConnection, FReconnectTimeout);

  LRemoveConn := nil;
  try
    for LConnInfo in FClientConnections do
    begin
      if (LConnInfo.Value <> FClientComm) and (not LConnInfo.Value.Connected) then
      begin
        if not assigned(LRemoveConn) then
          LRemoveConn := TList<string>.Create;
        LRemoveConn.Add(LConnInfo.Key);
      end;
    end;
    if Assigned(LRemoveConn) then
      for LKey in LRemoveConn do
        FClientConnections.Remove(LKey);
  finally
    LRemoveConn.Free;
  end;
end;

function TTetheringNetworkManagerCommunicationThread.ProcessResponse(const AData: string; var RemoteConnectionString: string): string;
var
  State: TPairingState;
  ManagerInfo: TTetheringManagerInfo;
  LNetworkInfo: TNetworkInfo;
  LCommand: TTetheringManagerCommand;
begin
  if Terminated then // Avoid process any message if communication thread is terminated
    Exit;

  Result := TetheringEmpty;

  LCommand.DecodeCommand(AData);
  if not LCommand.ValidCommand then
    Exit;

  if LCommand.ConnectionString = '' then
    LCommand.ConnectionString := TTetheringNetworkAdapterCommon(FAdapter).FAdapterConnectionString;

  RemoteConnectionString := MergeConnectionString(RemoteConnectionString, LCommand.ConnectionString);

  LNetworkInfo := GetNetworkInfo(RemoteConnectionString);
  State := LNetworkInfo.State;

  if (LCommand.CommandText = TetheringDiscoverManagers) or (LCommand.CommandText = TetheringRequestPair) then
    FLastConnection := '';

  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.Adapter) then
    TTetheringNetworkAdapterCommon.Log('Net-In('+RemoteConnectionString+'): ' + FAdapter.Manager.Text + ': ' + GetEnumName(TypeInfo(TPairingState), Ord(State)) + ' -> ' + AData);
  case State of
    TPairingState.Waiting:
      begin
        // TetheringNewManager
        if (LCommand.CommandText = TetheringNewManager) and (LCommand.NumParams = 4) then
        begin
          if LCommand.Params[0] <> FAdapter.Manager.Identifier then
          begin
            LNetworkInfo.ManagerVersion := LCommand.Version;
            ManagerInfo := CreateManagerInfo(LCommand.Params[0], LCommand.Params[1], LCommand.Params[2], RemoteConnectionString, LNetworkInfo.ManagerVersion);
            ManagerInfo.ManagerAdapters := FAdapter.AdapterType;
            TTetheringNetworkAdapterCommon(FAdapter).DoNewManager(ManagerInfo);
          end
        end
        // TetheringShutdown
        else if (LCommand.CommandText = TetheringShutdown) and (LCommand.NumParams = 1) then
        begin
          if FAdapter.Manager.Identifier <> LCommand.Params[0] then
          begin
            LNetworkInfo := GetNetworkInfoByManager(LCommand.Params[0]);
            if LNetworkInfo <> nil then
              SetState(RemoteConnectionString, TPairingState.Waiting);
            TTetheringNetworkAdapterCommon(FAdapter).DoRemoteManagerShutdown(LCommand.Params[0]);
          end;
        end
        else
          Result := inherited ProcessResponse(AData, RemoteConnectionString);
      end;
    TPairingState.Paired:
      begin
        // TetheringShutdown
        if (LCommand.CommandText = TetheringShutdown) and (LCommand.NumParams = 1) then
        begin
          LNetworkInfo := GetNetworkInfoByManager(LCommand.Params[0]);
          if LNetworkInfo <> nil then
            SetState(RemoteConnectionString, TPairingState.Waiting);
          TTetheringNetworkAdapterCommon(FAdapter).DoRemoteManagerShutdown(LCommand.Params[0]);
        end
          else
            Result := inherited ProcessResponse(AData, RemoteConnectionString);
      end;
    else
      Result := inherited ProcessResponse(AData, RemoteConnectionString);
  end;

  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.Adapter) then
    TTetheringNetworkAdapterCommon.Log('Net-Out('+RemoteConnectionString+'): state: ' + GetEnumName(TypeInfo(TPairingState), Ord(GetState(RemoteConnectionString))) + '; Resp: ' + Result);
end;

function TTetheringNetworkManagerCommunicationThread.StartListening(const ATarget: string): Boolean;
begin
  FTarget := ATarget;
  FServerComm.Target := ATarget;
  Result := FServerComm.StartServer;
end;

procedure TTetheringNetworkManagerCommunicationThread.StopListening;
begin
  FServerComm.StopServer;
end;


{ TTetheringNetworkManagerCommunicationThread_UDP }

constructor TTetheringNetworkManagerCommunicationThread_UDP.Create(const AnAdapter: TTetheringAdapter;
  AIPVersion: TCommIPVersion; const ABindToAddress: string);
begin
  inherited Create(AnAdapter, AIPVersion, ABindToAddress);
  FServerCommUDP := TTetheringNetworkServerCommUDP.Create(AIPVersion, ABindToAddress);
  FServerCommUDP.OnUDPData := DoOnReceiveData;
  FServerCommUDP.OnUDPException := DoOnReceiveException;
end;

destructor TTetheringNetworkManagerCommunicationThread_UDP.Destroy;
begin
  inherited;
  FServerCommUDP.Free;
end;

procedure TTetheringNetworkManagerCommunicationThread_UDP.BroadcastCommand(const AData, AHost: string);
begin
  FServerCommUDP.BroadcastData(TEncoding.UTF8.GetBytes(AData), AHost, TTetheringNetworkAdapterCommon.FirstManagerPort,
    TTetheringNetworkAdapterCommon.LastManagerPort);
end;

function TTetheringNetworkManagerCommunicationThread_UDP.IsListening: Boolean;
begin
  Result := (inherited IsListening) and FServerCommUDP.Active;
end;

function TTetheringNetworkManagerCommunicationThread_UDP.StartListening(const ATarget: string): Boolean;
begin
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
    TTetheringNetworkAdapterCommon.Log('TTetheringNetworkManagerCommunicationThread_UDP.StartListening');
  FServerCommUDP.Target := ATarget;
  Result := (inherited StartListening(ATarget)) and FServerCommUDP.StartServer;
end;

procedure TTetheringNetworkManagerCommunicationThread_UDP.StopListening;
begin
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
    TTetheringNetworkAdapterCommon.Log('TTetheringNetworkManagerCommunicationThread_UDP.StopListening');
  FServerCommUDP.StopServer;
  inherited StopListening;
end;


{ TTetheringNetworkManagerCommunicationThread_Multicast }

constructor TTetheringNetworkManagerCommunicationThread_Multicast.Create(const AnAdapter: TTetheringAdapter;
  AIPVersion: TCommIPVersion; const ABindToAddress: string);
begin
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
    TTetheringNetworkAdapterCommon.Log('Creating TTetheringNetworkManagerCommunicationThread_Multicast(' +
      TRttiEnumerationType.GetName<TCommIPVersion>(AIPVersion) + ')');
  inherited;
  FServerCommMulticast := TTetheringNetworkServerCommMulticast.Create(AIPVersion);
  FServerCommMulticast.OnMulticastData := DoOnReceiveData;
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
    TTetheringNetworkAdapterCommon.Log('Created TTetheringNetworkManagerCommunicationThread_Multicast(' +
      TRttiEnumerationType.GetName<TCommIPVersion>(AIPVersion) + ')');
end;

destructor TTetheringNetworkManagerCommunicationThread_Multicast.Destroy;
begin
  inherited;
  FServerCommMulticast.Free;
end;

procedure TTetheringNetworkManagerCommunicationThread_Multicast.BroadcastCommand(const AData, AHost: string);
begin
  FServerCommMulticast.MulticastData(TEncoding.UTF8.GetBytes(AData), AHost, TTetheringNetworkAdapterCommon.FirstManagerPort,
    TTetheringNetworkAdapterCommon.LastManagerPort);
end;

function TTetheringNetworkManagerCommunicationThread_Multicast.IsListening: Boolean;
begin
  Result := (inherited IsListening) and FServerCommMulticast.Active;
end;

function TTetheringNetworkManagerCommunicationThread_Multicast.StartListening(const ATarget: string): Boolean;
begin
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
    TTetheringNetworkAdapterCommon.Log('TTetheringNetworkManagerCommunicationThread_Multicast.StartListening');
  FServerCommMulticast.Target := ATarget;
  Result := (inherited StartListening(ATarget)) and FServerCommMulticast.StartServer;
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
    TTetheringNetworkAdapterCommon.Log('TTetheringNetworkManagerCommunicationThread_Multicast.StartListening: ' + BoolToStr(Result, True));
end;

procedure TTetheringNetworkManagerCommunicationThread_Multicast.StopListening;
begin
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.CommThread) then
    TTetheringNetworkAdapterCommon.Log('TTetheringNetworkManagerCommunicationThread_Multicast.StopListening');
  FServerCommMulticast.StopServer;
  inherited StopListening;
end;


{ TTetheringNetworkAdapterV4_UDP }

class function TTetheringNetworkAdapterV4_UDP.CreateInstance: TTetheringAdapter;
begin
  Result := TTetheringNetworkAdapterV4_UDP.Create(TCommIPVersion.IP_IPv4);
end;

constructor TTetheringNetworkAdapterV4_UDP.Create(AIPVersion: TCommIPVersion);
begin
  inherited;
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.Adapter) then
    TTetheringNetworkAdapterCommon.Log('Created TTetheringNetworkAdapterV4(' + TRttiEnumerationType.GetName<TCommIPVersion>(AIPVersion) + ')');
end;

function TTetheringNetworkAdapterV4_UDP.DoCreateCommunicationThread(
  const AnAdapter: TTetheringAdapter; const ABindToAddress: string): TTetheringManagerCommunicationThread;
begin
  Result := TTetheringNetworkManagerCommunicationThread_UDP.Create(Self, FIPVersion, ABindToAddress);
  FCommunicationThread := TTetheringNetworkManagerCommunicationThread(Result);
end;

function TTetheringNetworkAdapterV4_UDP.GetAdapterType: TTetheringAdapterType;
begin
  Result := AdapterID;
end;


{ TTetheringNetworkAdapterMulticast }

constructor TTetheringNetworkAdapterMulticast.Create(AIPVersion: TCommIPVersion);
begin
  inherited Create(AIPVersion);
  if TTetheringAdapter.IsLoggingItem(TTetheringAdapter.TTetheringLogItem.Adapter) then
    TTetheringNetworkAdapterCommon.Log('Created TTetheringNetworkAdapterMulticast(' + TRttiEnumerationType.GetName<TCommIPVersion>(AIPVersion) + ')');
end;

function TTetheringNetworkAdapterMulticast.DoCreateCommunicationThread(
  const AnAdapter: TTetheringAdapter; const ABindToAddress: string): TTetheringManagerCommunicationThread;
begin
  Result := TTetheringNetworkManagerCommunicationThread_Multicast.Create(Self, FIPVersion, ABindToAddress);
  FCommunicationThread := TTetheringNetworkManagerCommunicationThread(Result);
end;


{ TTetheringNetworkAdapterMulticast_V4 }

class function TTetheringNetworkAdapterMulticast_V4.CreateInstance: TTetheringAdapter;
begin
  Result := TTetheringNetworkAdapterMulticast_V4.Create(TCommIPVersion.IP_IPv4);
end;

function TTetheringNetworkAdapterMulticast_V4.GetAdapterType: TTetheringAdapterType;
begin
  Result := AdapterID;
end;


{ TTetheringNetworkAdapterMulticast_V6 }

class function TTetheringNetworkAdapterMulticast_V6.CreateInstance: TTetheringAdapter;
begin
  Result := TTetheringNetworkAdapterMulticast_V6.Create(TCommIPVersion.IP_IPv6);
end;

function TTetheringNetworkAdapterMulticast_V6.GetAdapterType: TTetheringAdapterType;
begin
  Result := AdapterID;
end;

function TTetheringNetworkAdapterMulticast_V6.IsIP(const ATarget: string): Boolean;
begin
                                                                       
  Result := ATarget.IndexOf(':') <> -1;
end;

function TTetheringNetworkAdapterMulticast_V6.IsIPRange(const ATarget: string): Boolean;
begin
                                                                               
  Result := False;
end;

procedure TTetheringNetworkAdapterMulticast_V6.SendManagerCommandToRange(const ATarget: string;
  const ACommand: TTetheringManagerCommand);
begin
                                                                               
  // Now, we do not manage network ranges. Avoid any command sending.
end;

initialization
  TTetheringAdapters.RegisterAdapter(TTetheringNetworkAdapterV4_UDP, TTetheringNetworkAdapterV4_UDP.AdapterID);
  TTetheringAdapters.RegisterAdapter(TTetheringNetworkAdapterMulticast_V4, TTetheringNetworkAdapterMulticast_V4.AdapterID);
  TTetheringAdapters.RegisterAdapter(TTetheringNetworkAdapterMulticast_V6, TTetheringNetworkAdapterMulticast_V6.AdapterID);

finalization
  TTetheringAdapters.UnRegisterAdapter(TTetheringNetworkAdapterMulticast_V6.AdapterID);
  TTetheringAdapters.UnRegisterAdapter(TTetheringNetworkAdapterMulticast_V4.AdapterID);
  TTetheringAdapters.UnRegisterAdapter(TTetheringNetworkAdapterV4_UDP.AdapterID);
end.

