unit TasksClientSettingsU;

interface

uses System.Classes, System.IniFiles;

type
  TTasksClientSettings = class
  public type
    TWriter = class
    private
      FIniFile: TIniFile;
      FSettings: TTasksClientSettings;
    private
      constructor Create(const ASettings: TTasksClientSettings; const AIniFile: TIniFile);
    public
      procedure WriteHost(const AValue: string);
      procedure WritePort(AValue: Integer);
      procedure WritePath(const AValue: string);
      procedure WriteUserName(const AValue: string);
      procedure WriteProtocol(const AValue: string);
    end;
    TUpdateCallback = reference to procedure(const AWriter: TWriter);
  private class var
    FInstance: TTasksClientSettings;
  private
    FHost: string;
    FLoaded: Boolean;
    FPath: string;
    FProtocol: string;
    FPort: Integer;
    FUsername: string;
    function FindSettingsFileName(out AFileName: string): Boolean;
    function GetSettingsFileExists: Boolean;
    function GetSettingsFileName: string;
  public
    class constructor Create;
    class destructor Destroy;
    procedure Load;
    procedure Update(const ACallback: TUpdateCallback);
    property ConfigFileExists: Boolean read GetSettingsFileExists;
    property ConfigFileName: string read GetSettingsFileName;
    property Loaded: Boolean read FLoaded;
    property Host: string read FHost;
    property Port: Integer read FPort;
    property Path: string read FPath;
    property Protocol: string read FProtocol;
    property Username: string read FUsername;
    class property Instance: TTasksClientSettings read FInstance;
  end;

implementation

uses System.StrUtils, System.SysUtils, System.IOUtils;

type
  TSettingsSections = record
  public const
    Connection = 'Connection';
    User = 'User';
  end;

  TSettingsNames = record
  public const
    Host = 'Host';
    Port = 'Port';
    UserName = 'UserName';
    Path = 'Path';
    Protocol = 'Protocol';
  end;

  TSettingsDefaults = record
  const
    Port = 8080;
  end;


procedure LoadSettings(const ASettings: TTasksClientSettings);
var
  LINI: TIniFile;

  function ReadString(const ASection, AKey: string; const ADefault: string = ''): string;
  begin
    Result := LINI.ReadString(ASection, AKey, '');
    if Result = '' then
      Result := ADefault;
  end;

  function ReadBool(const ASection, AKey: string; const ADefault: Boolean = False): Boolean;
  begin
    Result := LINI.ReadBool(ASection, AKey, ADefault);
  end;

  function ReadInteger(const ASection, AKey: string; const ADefault: Integer): Integer;
  begin
    Result := LINI.ReadInteger(ASection, AKey, ADefault);
  end;

begin
  LINI := TIniFile.Create(ASettings.ConfigFileName);
  try
    ASettings.FHost := ReadString
      (TSettingsSections.Connection, TSettingsNames.Host, '');
    ASettings.FPort := ReadInteger
      (TSettingsSections.Connection, TSettingsNames.Port, TSettingsDefaults.Port);
    ASettings.FPath := ReadString
      (TSettingsSections.Connection, TSettingsNames.Path, '');
    ASettings.FProtocol := ReadString
      (TSettingsSections.Connection, TSettingsNames.Protocol, '');
    ASettings.FUserName := ReadString
      (TSettingsSections.User, TSettingsNames.UserName, '');
  finally
    LINI.Free;
  end;
end;

{ TTasksClientSettings }

class constructor TTasksClientSettings.Create;
begin
  FInstance := TTasksClientSettings.Create;
end;

//function CurrentDirectory: string;
//begin
//  Result := ExtractFilePath(StringReplace(GetModuleName(HInstance),'\\?\','',[rfReplaceAll, rfIgnoreCase]));       //ExtractFileName(FileName)
//end;

class destructor TTasksClientSettings.Destroy;
begin
  FInstance.Free;
end;

function TTasksClientSettings.FindSettingsFileName(out AFileName: string): Boolean;
begin
  AFileName := TPath.Combine(TPath.GetDocumentsPath, 'tasksclient');
  //AFileName := CurrentDirectory + 'tasksclient.ini';
  ForceDirectories(AFileName);
  AFileName := TPath.Combine(AFileName, 'tasksclient.ini');
  Result := FileExists(AFileName);
end;

function TTasksClientSettings.GetSettingsFileExists: Boolean;
var
  LFileName: string;
begin
  Result := FindSettingsFileName(LFileName);
end;

function TTasksClientSettings.GetSettingsFileName: string;
begin
  FindSettingsFileName(Result);
end;

procedure TTasksClientSettings.Load;
begin
  if not FLoaded then
  begin
    LoadSettings(Self);
    FLoaded := True;
  end;
end;

procedure TTasksClientSettings.Update(const ACallback: TUpdateCallback);
var
  LWriter: TWriter;
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(Self.ConfigFileName);
  LWriter := nil;
  try
    LWriter := TWriter.Create(Self, LIniFile);
    ACallback(LWriter);
  finally
    LIniFile.Free;
    LWriter.Free;
  end;
end;

{ TTasksClientSettings.TSettingsWriter }

constructor TTasksClientSettings.TWriter.Create(const ASettings: TTasksClientSettings;
  const AIniFile: TIniFile);
begin
  FSettings := ASettings;
  FIniFile := AIniFile;
end;

procedure TTasksClientSettings.TWriter.WriteHost(
  const AValue: string);
begin
  FSettings.FHost := AValue;
  FIniFile.WriteString(TSettingsSections.Connection, TSettingsNames.Host, AValue);
end;

procedure TTasksClientSettings.TWriter.WritePath(
  const AValue: string);
begin
  FSettings.FPath := AValue;
  FIniFile.WriteString(TSettingsSections.Connection, TSettingsNames.Path, AValue);
end;

procedure TTasksClientSettings.TWriter.WritePort(AValue: Integer);
begin
  FSettings.FPort := AValue;
  FIniFile.WriteInteger(TSettingsSections.Connection, TSettingsNames.Port, AValue);
end;

procedure TTasksClientSettings.TWriter.WriteProtocol(
  const AValue: string);
begin
  FSettings.FProtocol := AValue;
  FIniFile.WriteString(TSettingsSections.Connection, TSettingsNames.Protocol, AValue);
end;

procedure TTasksClientSettings.TWriter.WriteUserName(
  const AValue: string);
begin
  FSettings.FUsername := AValue;
  FIniFile.WriteString(TSettingsSections.User, TSettingsNames.UserName, AValue);
end;

end.
