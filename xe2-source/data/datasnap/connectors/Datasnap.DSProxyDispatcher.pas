{*******************************************************}
{                                                       }
{               Delphi DataSnap Framework               }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit Datasnap.DSProxyDispatcher;

interface

uses
  Datasnap.DSClientMetadata,
  System.Classes,
  System.Generics.Collections,
  Web.HTTPApp;

type
  /// <summary>
  /// The proxy dispatcher for WebBroker based datasnap servers
  /// </summary>
  TDSProxyDispatcher = class(TCustomWebFileDispatcher)
  strict private
    FAvailableProxies: TDictionary<string, string>;
    FDSProxyGenerator: TDSProxyGenerator;
    procedure OnBeforeDispatch(Sender: TObject; const AFileName: string;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure BuildAvailableProxies;
    procedure SetDSProxyGenerator(const Value: TDSProxyGenerator);
    function GetLanguage(const AFileName: String): String;
  private
    FRequiredProxyFilesPath: String;
    // IWebDispatch
    {$HINTS OFF}
    function DispatchMethodType: TMethodType;
    {$HINTS ON}
    procedure SetRequiredProxyFilesPath(const Value: String);
  protected
    procedure CreateZipFile(LanguageRequiredFiles, Filename: String);
    function isSupportedProxy(const Language: String;
      out WriterID: String): Boolean;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AfterDispatch;
    /// <summary>
    ///   Represents the full path where are the required proxy files
    /// </summary>
    property RequiredProxyFilesPath: String read FRequiredProxyFilesPath
      write SetRequiredProxyFilesPath;
      /// <summary>
      ///   The TDSProxyGenerator instance to use
      /// </summary>
    property DSProxyGenerator: TDSProxyGenerator read FDSProxyGenerator
      write SetDSProxyGenerator;
  end;

implementation

uses
  Datasnap.DSProxyWriter,
  Datasnap.DSProxyUtils,
  System.StrUtils,
  System.SysUtils,
  System.Types,
  Web.WebFileDispatcher;

{ TDSProxyDipatcher }

procedure TDSProxyDispatcher.BuildAvailableProxies;
var
  writer: String;
  WriterProxy: TDSProxyWriter;
begin
  if not assigned(FAvailableProxies) then
  begin
    FAvailableProxies := TDictionary<string, string>.Create;
    for writer in TDSProxyWriterFactory.RegisteredWritersList do
    begin
      WriterProxy := TDSProxyWriterFactory.GetWriter(writer);
      try
        if not FAvailableProxies.ContainsKey(WriterProxy.Properties.Language) then
          FAvailableProxies.Add(WriterProxy.Properties.Language, writer);
      finally
        WriterProxy.Free;
      end;
    end;
  end;
end;

constructor TDSProxyDispatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootDirectory := '.';
  WebDirectories.Clear;
  WebFileExtensions.Clear;
  TWebDirectoryItem.Create(WebDirectories, dirInclude, '\proxy\*');
  TWebDirectoryItem.Create(WebDirectories, dirExclude, '\proxy\*\*');
  RequiredProxyFilesPath := 'proxy';
  TWebFileExtensionItem.Create(WebFileExtensions,
    'application/x-zip-compressed', 'zip');
  BeforeDispatch := OnBeforeDispatch;
end;

procedure TDSProxyDispatcher.CreateZipFile(LanguageRequiredFiles,
  Filename: String);
begin
  TDSProxyUtils.CompressDirectory(LanguageRequiredFiles, Filename);
end;

destructor TDSProxyDispatcher.Destroy;
begin
  FreeAndNil(FAvailableProxies);
  inherited;
end;

function TDSProxyDispatcher.DispatchMethodType: TMethodType;
begin
  Result := mtGet;
end;

function TDSProxyDispatcher.GetLanguage(const AFileName: String): String;
var
  a: TStringDynArray;
begin
  a := SplitString(LowerCase(AFileName), '.');
  Result := a[0];
end;

function TDSProxyDispatcher.isSupportedProxy(const Language: String;
  out WriterID: String): Boolean;
begin
  BuildAvailableProxies;
  Result := FAvailableProxies.TryGetValue(Language, WriterID);
end;

procedure TDSProxyDispatcher.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDSProxyGenerator) then
    FDSProxyGenerator := nil;
end;

procedure TDSProxyDispatcher.OnBeforeDispatch(Sender: TObject;
  const AFileName: string; Request: TWebRequest; Response: TWebResponse;
  var Handled: Boolean);
var
  D1, D2: TDateTime;
  Language, WriterID: String;
  Filename, LanguageRequiredFiles, OutputProxyDir: String;
  MustGenerateProxy: Boolean;
begin
  Handled := False;
  try
    Filename := WebApplicationDirectory + RequiredProxyFilesPath + '\' +
      ExtractFileName(AFileName);
    if assigned(FDSProxyGenerator) then
    begin
      Language := GetLanguage(ExtractFileName(Filename));
      if isSupportedProxy(Language, WriterID) then
      begin
        MustGenerateProxy := False;

        if not FileExists(Filename) then
          MustGenerateProxy := True;

        if not MustGenerateProxy then // the file exists
          // check for older generated proxy
          MustGenerateProxy := FileAge(Filename, D1) and
            FileAge(WebApplicationFileName, D2) and (D1 < D2);

        if MustGenerateProxy then
        begin
          FDSProxyGenerator.writer := WriterID;
          if DirectoryExists(FRequiredProxyFilesPath) then // full path
            LanguageRequiredFiles := IncludeTrailingPathDelimiter
              (FRequiredProxyFilesPath) + Language
          else // try with a relative path
            LanguageRequiredFiles := WebApplicationDirectory +
              FRequiredProxyFilesPath + '\' + Language;
          OutputProxyDir := LanguageRequiredFiles + '\' +
            FDSProxyGenerator.WriterProperties.Comment;
          FDSProxyGenerator.TargetDirectory := OutputProxyDir;
          FDSProxyGenerator.TargetUnitName := 'DSProxy' +
            FDSProxyGenerator.FileDescriptions[0].DefaultFileExt;
          FDSProxyGenerator.Write;
          DeleteFile(Filename);
          CreateZipFile(LanguageRequiredFiles, Filename);
        end;
      end;
    end;
  except
    on E: Exception do
      raise;
  end;
end;

procedure TDSProxyDispatcher.SetDSProxyGenerator(const Value
  : TDSProxyGenerator);
begin
  FDSProxyGenerator := Value;
  FreeAndNil(FAvailableProxies);
end;

procedure TDSProxyDispatcher.SetRequiredProxyFilesPath(const Value: String);
begin
  FRequiredProxyFilesPath := Value;
end;

end.
