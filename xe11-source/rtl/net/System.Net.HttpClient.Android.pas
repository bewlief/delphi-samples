{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Net.HttpClient.Android;

interface

implementation

uses
  AndroidApi.JNI, Androidapi.JNIBridge, Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Net,
  Androidapi.JNI.Java.Net,
  Androidapi.JNI.Java.Security,
  Androidapi.JNI.Android.Security,
  Androidapi.Looper,
  Androidapi.AppGlue,
  Androidapi.NativeActivity,

  System.Classes,
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.DateUtils,
  System.SyncObjs,
  System.Hash,
  System.NetEncoding,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.NetConsts,
  System.Types,
  System.Net.Mime;

type
  TAndroidHTTPRequest = class;

  TX509TrustManager = class(TJavaLocal, JX509TrustManager)
  private
    class procedure JX509CertToDCert(AJCert: JX509Certificate;
      var ADCert: TCertificate); static;
  protected
    FJOrigOldTrustManager: JX509TrustManager;
    [Weak] FRequest: TAndroidHTTPRequest;
  public
    procedure checkClientTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString); cdecl;
    procedure checkServerTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString); cdecl;
    function getAcceptedIssuers: TJavaObjectArray<JX509Certificate>; cdecl;
    constructor Create(ATrustManager: JX509TrustManager; const ARequest: TAndroidHTTPRequest);
  end;

  TAliasCallback = class(TJavaLocal, JKeyChainAliasCallback)
  protected
    [Weak] FRequest: TAndroidHTTPRequest;
  public
    procedure alias(alias: JString); cdecl;
    constructor Create(const ARequest: TAndroidHTTPRequest);
  end;

  TJHostnameVerifier = class(TJavaLocal, JHostnameVerifier)
  protected
    [Weak] FRequest: TAndroidHTTPRequest;
  public
    function verify(hostname: JString; session: JSSLSession): Boolean; cdecl;
    constructor Create(const ARequest: TAndroidHTTPRequest);
  end;

  TAndroidHTTPClient = class(THTTPClient)
  private type
    // To cache Server/Proxy Auth
    TAuthInfo = record
      AuthType: TCredentialsStorage.TAuthSchemeType;
      URL: TURI;
      Username: string;
      Password: string;
      realm: string;
      nonce: string;
      opaque: string;
      qop: string;
      algorithm: string;
    end;

    TAuthArray = TArray<TAuthInfo>;

    TAuthURIComparer = class(TComparer<TAuthInfo>)
    public
      function Compare(const Left, Right: TAuthInfo): Integer; override;
    end;

  private type
    // To cache Server certificates
    TServerCertArray = TArray<JX509Certificate>;

  private type
    // To cache Client certificates
    TClientCertInfo = record
      URL: TURI;
      Alias: JString;
      PrivateKey: JPrivateKey;
      CertificateChain: TJavaObjectArray<JX509Certificate>;
    end;

    TClientCertArray = TArray<TClientCertInfo>;

  private
    // Cache Server/Proxy Auth
    FAuthServerCache: TAuthArray;
    FAuthProxyCache: TAuthArray;
    FAuthURIComparer: TAuthURIComparer;

    // Cache Server certificates
    FServerCertCache: TServerCertArray;

    // Cache Client certificates
    FClientCertCache: TClientCertArray;

    procedure SetAuth(const AuthTargetType: TAuthTargetType; const AuthInfo: TAuthInfo;
      const ARequest: TAndroidHTTPRequest);
    procedure SetAuthResponse(const ARequest: TAndroidHTTPRequest);
    function GetAuthInfo(const HeaderValue: string; const ARequest: TAndroidHTTPRequest): TAuthInfo;
    function GenerateResponse(const AuthInfo: TAuthInfo; const AURL: TURI; const AMethodString: string): string;
    function GenerateDigestResponse(const AuthInfo: TAuthInfo; const AURL: TURI; const AMethodString: string): string;
    function GenerateBasicResponse(const AuthInfo: TAuthInfo): string;


    function FindAuthInfo(const AnAuthArray: TAuthArray; const AURL: TURI): TAuthInfo;
    procedure SetPreemptiveServerAuth(const ARequest: TAndroidHTTPRequest);
    procedure SetPreemptiveProxyAuth(const ARequest: TAndroidHTTPRequest);

    // cache Server certificates functions
    function ExistServerCertInfo(AServerCert: JX509Certificate): Boolean;
    procedure AddServerCertInfo(AServerCert: JX509Certificate);

    // cache Client certificates functions
    procedure AddClientCertInfo(const AURL: TURI; AnAlias: JString; APrivateKey: JPrivateKey;
      ACertificateChain: TJavaObjectArray<JX509Certificate>);

    procedure UpdateSecureFailureReasons(E: EJNIException);

  protected
    function DoExecuteRequest(const ARequest: THTTPRequest; var AResponse: THTTPResponse;
      const AContentStream: TStream): TAndroidHTTPClient.TExecutionResult; override;
    procedure ExecuteHTTP(const ARequest: IHTTPRequest;
      const AContentStream: TStream; const AResponse: IHTTPResponse); override;

    procedure DoGetClientCertificates(const ARequest: THTTPRequest; const ACertificateList: TList<TCertificate>); override;
    function DoClientCertificateAccepted(const ARequest: THTTPRequest; const AnIndex: Integer): Boolean; override;
    function DoGetSSLCertificateFromServer(const ARequest: THTTPRequest): TCertificate; override;
    procedure DoServerCertificateAccepted(const ARequest: THTTPRequest); override;
    function DoSetCredential(AnAuthTargetType: TAuthTargetType; const ARequest: THTTPRequest;
      const ACredential: TCredentialsStorage.TCredential): Boolean; override;

    function DoGetHTTPRequestInstance(const AClient: THTTPClient; const ARequestMethod: string;
      const AURI: TURI): IHTTPRequest; override;

    function DoGetResponseInstance(const AContext: TObject; const AProc: TProc;
      const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent;
      const ARequest: IURLRequest; const AContentStream: TStream): IAsyncResult; override;

    function DoProcessStatus(const ARequest: IHTTPRequest; const  AResponse: IHTTPResponse): Boolean; override;

    class function CreateInstance: TURLClient; override;

  public
    constructor Create;
    destructor Destroy; override;

  end;


  TX509KeyManager = class(TJavaLocal, JX509KeyManager)
  protected
    FUsingKeyChain: Boolean;
    FUsingCache: Boolean;
    FJOrigOldKeyManager: JX509KeyManager;
    [Weak] FRequest: TAndroidHTTPRequest;
    FClientCertInfo: TAndroidHTTPClient.TClientCertInfo;
  public
    function chooseClientAlias(keyType: TJavaObjectArray<JString>; issuers: TJavaObjectArray<JPrincipal>; socket: JSocket): JString; cdecl;
    function chooseServerAlias(keyType: JString; issuers: TJavaObjectArray<JPrincipal>; socket: JSocket): JString; cdecl;
    function getCertificateChain(alias: JString): TJavaObjectArray<JX509Certificate>; cdecl;
    function getClientAliases(keyType: JString; issuers: TJavaObjectArray<JPrincipal>): TJavaObjectArray<JString>; cdecl;
    function getServerAliases(keyType: JString; issuers: TJavaObjectArray<JPrincipal>): TJavaObjectArray<JString>; cdecl;
    function getPrivateKey(alias: JString): JPrivateKey; cdecl;

    constructor Create(AKeyManager: JX509KeyManager; const ARequest: TAndroidHTTPRequest);
  end;


  TAndroidHTTPResponse = class;
  TAndroidHTTPRequest = class(THTTPRequest)
  private type
    TClientCertURIComparer = class(TComparer<TAndroidHTTPClient.TClientCertInfo>)
    public
      function Compare(const Left, Right: TAndroidHTTPClient.TClientCertInfo): Integer; override;
    end;

    IClientCertURIComparer = IComparer<TAndroidHTTPClient.TClientCertInfo>;

  private
    FJURLConnection: JHttpURLConnection;

    FJTrustManager: JX509TrustManager;
    FJKeyManager: JX509KeyManager;
    FJHostVerifier: JHostnameVerifier;
    FJClientCertAlias: JString;
    FClientCertEvent: TEvent;
    FServerCertificate: TCertificate;
    FJServerCertificate: JX509Certificate;
    FClientCertAliasCallback: JKeyChainAliasCallback;

    [Weak] FResponse: TAndroidHTTPResponse;

    FHeaders: TDictionary<string, string>;

    FAuthUserName: string;
    FAuthPassword: string;

    FClientCertCache: TList<TAndroidHTTPClient.TClientCertInfo>;
    FMoreClientCert: Boolean;

    procedure WriteData;
    procedure SetPreemptiveAuth;
    function FindClientCertInfo(const AURL: TURI): TAndroidHTTPClient.TClientCertInfo;
    procedure SetSecureProtocols(const AJSSLContext: JSSLContext);
    procedure DoSetConnectionTimeout(AValue: Integer);
    procedure DoSetResponseTimeout(AValue: Integer);
  protected
    function GetHeaders: TNetHeaders; override;

    procedure AddHeader(const AName, AValue: string); override;
    function RemoveHeader(const AName: string): Boolean; override;

    function GetHeaderValue(const AName: string): string; override;
    procedure SetHeaderValue(const AName, Value: string); override;

    procedure DoPrepare; override;

    /// <summary> Setter for the ConnectionTimeout property.</summary>
    procedure SetConnectionTimeout(const Value: Integer); override;
    /// <summary> Setter for the ResponseTimeout property.</summary>
    procedure SetResponseTimeout(const Value: Integer); override;

  public
    constructor Create(const AClient: THTTPClient; const ARequestMethod: string; const AURI: TURI);
    destructor Destroy; override;
  end;

  TAndroidHTTPResponse = class(THTTPResponse)
  private
    [Weak] FRequest: TAndroidHTTPRequest;
    FResponseCode: Integer;
    FStatusText: string;
    FVersion: THTTPProtocolVersion;
    procedure UpdateHeaders;

  protected
    function GetDecompressResponse: Boolean; override;
    procedure DoReadData(const AStream: TStream); override;
    function GetHeaders: TNetHeaders; override;

    function GetStatusCode: Integer; override;
    function GetStatusText: string; override;
    function GetVersion: THTTPProtocolVersion; override;
  public
    constructor Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback;
      const AAsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: TAndroidHTTPRequest; const AContentStream: TStream);
    destructor Destroy; override;
  end;

{ TX509TrustManager }

procedure TX509TrustManager.checkClientTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString);
begin
  try
    FJOrigOldTrustManager.checkClientTrusted(chain, authType);
  except
    on E: EJNIException do
    begin
    end;
  end;
end;

procedure TX509TrustManager.checkServerTrusted(chain: TJavaObjectArray<JX509Certificate>; authType: JString);
var
  LTrustManagerExtensions: JX509TrustManagerExtensions;
  LTryTrustManagerExtensions: Boolean;
begin
  try
    if chain.Length > 0 then
    begin
      FRequest.FJServerCertificate := chain[0];
      JX509CertToDCert(FRequest.FJServerCertificate, FRequest.FServerCertificate);
      if TAndroidHTTPClient(FRequest.FClient).ExistServerCertInfo(FRequest.FJServerCertificate) then
        Exit;

      FRequest.FJServerCertificate.checkValidity;
    end
    else
    begin
      FRequest.FJServerCertificate := nil;
      FRequest.FServerCertificate := Default(TCertificate);
    end;

    LTryTrustManagerExtensions := False;
    try
      FJOrigOldTrustManager.checkServerTrusted(chain, authType);
    except
      on E: EJNIException do
        if Pos('Domain specific configurations require that hostname aware checkServerTrusted', E.Message) > 0 then
          LTryTrustManagerExtensions := True
        else
          raise;
    end;
    if LTryTrustManagerExtensions then
    begin
      LTrustManagerExtensions := TJX509TrustManagerExtensions.JavaClass.init(FJOrigOldTrustManager);
      LTrustManagerExtensions.checkServerTrusted(chain, authType, FRequest.FJURLConnection.getURL.getHost);
    end;
  except
    on E: EJNIException do
      TAndroidHTTPClient(FRequest.FClient).UpdateSecureFailureReasons(E);
  end;
end;

class procedure TX509TrustManager.JX509CertToDCert(AJCert: JX509Certificate; var ADCert: TCertificate);
var
  LName: string;

  function NormalizeDomainName(const AName: string; var ACommonName: string): string;
  var
    LList: TStringList;
    LItem: string;
    I: Integer;
  begin
    Result := '';
    ACommonName := '';
    LList := TStringList.Create;
    try
      LList.Delimiter := ',';
      LList.StrictDelimiter := True;
      LList.DelimitedText := AName;
      for I := LList.Count - 1 downto 0 do
      begin
        if Result <> '' then
          Result := Result + ';';
        LItem := LList.ValueFromIndex[I].Trim;
        Result := Result + LItem;
        if I = 0 then
          ACommonName := LItem;
      end;
    finally
      LList.Free;
    end;
  end;

begin
  ADCert.Start := UnixToDateTime(Trunc(AJCert.getNotBefore.getTime / 1000));
  ADCert.Expiry := UnixToDateTime(Trunc(AJCert.getNotAfter.getTime / 1000));
  ADCert.Subject := NormalizeDomainName(JStringToString(AJCert.getSubjectDN.getName), LName);
  ADCert.CertName := LName;
  ADCert.Issuer := NormalizeDomainName(JStringToString(AJCert.getIssuerDN.getName), LName);
  ADCert.SerialNum := JStringToString(AJCert.getSerialNumber.toString(16)).ToUpper;
                                                                                                              
                                            
end;

constructor TX509TrustManager.Create(ATrustManager: JX509TrustManager; const ARequest: TAndroidHTTPRequest);
begin
  inherited Create;
  FJOrigOldTrustManager := ATrustManager;
  FRequest := ARequest;
end;

function TX509TrustManager.getAcceptedIssuers: TJavaObjectArray<JX509Certificate>;
begin
  Result := FJOrigOldTrustManager.getAcceptedIssuers;
end;

{ TAndroidHTTPClient }

procedure TAndroidHTTPClient.AddClientCertInfo(const AURL: TURI; AnAlias: JString; APrivateKey: JPrivateKey;
  ACertificateChain: TJavaObjectArray<JX509Certificate>);
var
  Len: Integer;
  Info: TClientCertInfo;
begin
  Len := Length(FClientCertCache);
  SetLength(FClientCertCache, Len + 1);
  Info.URL := AURL;
  Info.Alias := AnAlias;
  Info.PrivateKey := APrivateKey;
  Info.CertificateChain := ACertificateChain;
  FClientCertCache[Len] := Info;
end;

procedure TAndroidHTTPClient.AddServerCertInfo(AServerCert: JX509Certificate);
var
  Len: Integer;
begin
  if not ExistServerCertInfo(AServerCert) then
  begin
    Len := Length(FServerCertCache);
    SetLength(FServerCertCache, Len + 1);
    FServerCertCache[Len] := AServerCert;
  end;
end;

constructor TAndroidHTTPClient.Create;
begin
  inherited Initializer;
  FAuthURIComparer := TAuthURIComparer.Create;
end;

destructor TAndroidHTTPClient.Destroy;
var
  LClientCert: TClientCertInfo;
begin
  for LClientCert in FClientCertCache do
    LClientCert.CertificateChain.Free;
  FAuthURIComparer.Free;
  inherited;
end;

class function TAndroidHTTPClient.CreateInstance: TURLClient;
begin
  Result := TAndroidHTTPClient.Create;
end;

function TAndroidHTTPClient.DoClientCertificateAccepted(const ARequest: THTTPRequest; const AnIndex: Integer): Boolean;
begin
  Result := False;
end;

procedure TAndroidHTTPClient.UpdateSecureFailureReasons(E: EJNIException);
var
  LReasons: THTTPSecureFailureReasons;
begin
  LReasons := SecureFailureReasons;
  if E.Message.Contains('java.security.cert.CertPathValidatorException') then
    Include(LReasons, THTTPSecureFailureReason.InvalidCA)
  else if E.Message.Contains('javax.net.ssl.SSLPeerUnverifiedException') then
    Include(LReasons, THTTPSecureFailureReason.CertCNInvalid)
  else if E.Message.Contains('java.security.cert.CertificateException: Unacceptable certificate') then
    Include(LReasons, THTTPSecureFailureReason.InvalidCert)
  else if E.Message.Contains('java.security.cert.CertificateExpiredException') or
         E.Message.Contains('CertificateNotYetValidException') then
    Include(LReasons, THTTPSecureFailureReason.CertDateInvalid)
  else if E.Message.Contains('javax.net.ssl.SSLProtocolException') or
          E.Message.Contains('javax.net.ssl.SSLException') then
    Include(LReasons, THTTPSecureFailureReason.SecurityChannelError);
  FSecureFailureReasons := LReasons;
end;

function TAndroidHTTPClient.DoExecuteRequest(const ARequest: THTTPRequest; var AResponse: THTTPResponse;
  const AContentStream: TStream): TAndroidHTTPClient.TExecutionResult;
var
  LRequest: TAndroidHTTPRequest;
  LResponse: TAndroidHTTPResponse;
  Value: TPair<string, string>;
  LServerCertAccepted: Boolean;
  LMessage: string;
begin
  Result := TExecutionResult.Success;
  LRequest := TAndroidHTTPRequest(ARequest);
  LResponse := TAndroidHTTPResponse(AResponse);

  // Request headers
  for Value in LRequest.FHeaders do
    LRequest.FJURLConnection.setRequestProperty(StringToJString(Value.Key), StringToJString(Value.Value));
  LRequest.WriteData;

  LMessage := '';
  try
    LRequest.FJURLConnection.connect;
  except
    on E: EJNIException do
    begin
      UpdateSecureFailureReasons(E);
      LMessage := E.Message;
    end;
  end;

  if LMessage <> '' then
  begin
    LRequest.FJURLConnection.disconnect;
    if (SecureFailureReasons <> []) and
       not (THTTPSecureFailureReason.SecurityChannelError in SecureFailureReasons) and
       not LRequest.FServerCertificate.IsEmpty and
       not ExistServerCertInfo(LRequest.FJServerCertificate) then
      Exit(TExecutionResult.ServerCertificateInvalid)
    else
      raise ENetHTTPCertificateException.Create(LMessage);
  end
  else if (Assigned(FValidateServerCertificateCallback) or
           Assigned(FValidateServerCertificateEvent)) and
       not LRequest.FServerCertificate.IsEmpty and
       not ExistServerCertInfo(LRequest.FJServerCertificate) then
  begin
    LServerCertAccepted := FSecureFailureReasons = [];
    if Assigned(FValidateServerCertificateCallback) then
      FValidateServerCertificateCallback(Self, LRequest, LRequest.FServerCertificate, LServerCertAccepted)
    else if Assigned(FValidateServerCertificateEvent) then
      FValidateServerCertificateEvent(Self, LRequest, LRequest.FServerCertificate, LServerCertAccepted);
    if not LServerCertAccepted then
    begin
      Include(FSecureFailureReasons, THTTPSecureFailureReason.CertNotAccepted);
      LRequest.FJURLConnection.disconnect;
      raise ENetHTTPCertificateException.CreateRes(@SNetHttpServerCertificateNotAccepted);
    end
    else
      AddServerCertInfo(LRequest.FJServerCertificate);
  end;

  // Update Headers & Cookies
  LResponse.UpdateHeaders;
end;

procedure InternalProcessMessages;
begin
                                                                                    
  TThread.Sleep(1);
end;

procedure TAndroidHTTPClient.ExecuteHTTP(const ARequest: IHTTPRequest; const AContentStream: TStream;
  const AResponse: IHTTPResponse);
var
  LTerminated: Boolean;
  LException: Exception;
begin
  // Android has a limitation when we are in main thread with http framework. To force the use of http framework in main thread
  // in android we have to change setThreadPolicy, but if you use client certificates the application gets a Force Close
  // Events will not be triggered in main thread so messages are not processed using KeyChain classes
  // To avoid this problem we launch the http request in a thread and we process the messages while the http is processed
  try
    if TThread.CurrentThread.ThreadID = MainThreadID then
    begin
      // Executing HTTPS request from main thread, when server requires client certificate,
      // will block application at TJKeyChain.JavaClass.getPrivateKey call.
      // Always call HTTPS requests asynchronously.
      LTerminated := False;
      LException := nil;
      TThread.CreateAnonymousThread(procedure
        begin
          try
            inherited ExecuteHTTP(ARequest, AContentStream, AResponse);
          except
            on E: Exception do
              LException := Exception(AcquireExceptionObject);
          end;
          LTerminated := True;
        end).Start;

      while not LTerminated do
        InternalProcessMessages;

      if LException <> nil then
        raise LException;
    end
    else
      inherited ExecuteHTTP(ARequest, AContentStream, AResponse);
  except
    on E: EJNIException do
      raise ENetHTTPClientException.Create(E.Message);
  end;
end;

procedure TAndroidHTTPClient.DoGetClientCertificates(const ARequest: THTTPRequest; const ACertificateList: TList<TCertificate>);
begin
  inherited;

end;

function TAndroidHTTPClient.DoGetHTTPRequestInstance(const AClient: THTTPClient; const ARequestMethod: string;
  const AURI: TURI): IHTTPRequest;
begin
  Result := TAndroidHTTPRequest.Create(TAndroidHTTPClient(AClient), ARequestMethod, AURI);
end;

function TAndroidHTTPClient.DoGetResponseInstance(const AContext: TObject; const AProc: TProc;
  const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: IURLRequest;
  const AContentStream: TStream): IAsyncResult;
begin
  Result := TAndroidHTTPResponse.Create(AContext, AProc, AsyncCallback, AsyncCallbackEvent, ARequest as TAndroidHttpRequest,
    AContentStream);
end;

function TAndroidHTTPClient.DoGetSSLCertificateFromServer(const ARequest: THTTPRequest): TCertificate;
begin
  Result := (ARequest as TAndroidHTTPRequest).FServerCertificate;
end;

function TAndroidHTTPClient.DoProcessStatus(const ARequest: IHTTPRequest; const AResponse: IHTTPResponse): Boolean;
var
  LRequest: TAndroidHTTPRequest;
  LResponse: TAndroidHTTPResponse;
begin
  LRequest := ARequest as TAndroidHTTPRequest;
  LResponse := AResponse as TAndroidHTTPResponse;
  // If the result is true then the while ends
  Result := True;
  if IsAutoRedirect(LResponse) then
  begin
    LRequest.FURL := ComposeRedirectURL(LRequest, LResponse);
    if IsAutoRedirectWithGET(LRequest, LResponse) then
    begin
      LRequest.FMethodString := sHTTPMethodGet; // Change to GET
      LRequest.FSourceStream := nil;            // Dont send any data
      LRequest.RemoveHeader(sContentLength);
      LRequest.SetHeaderValue(sContentType, '');// Dont set content type
    end;

    Result := False;
  end
  else
    if (AResponse.StatusCode = 403) and LRequest.FMoreClientCert then // client certificate needed
      Result := False;
end;

procedure TAndroidHTTPClient.DoServerCertificateAccepted(const ARequest: THTTPRequest);
begin
  inherited;
  if not TAndroidHTTPRequest(ARequest).FServerCertificate.IsEmpty then
    AddServerCertInfo(TAndroidHTTPRequest(ARequest).FJServerCertificate);
end;

function TAndroidHTTPClient.DoSetCredential(AnAuthTargetType: TAuthTargetType; const ARequest: THTTPRequest;
  const ACredential: TCredentialsStorage.TCredential): Boolean;
var
  LRequest: TAndroidHTTPRequest;
begin
   LRequest := TAndroidHTTPRequest(ARequest);

   LRequest.FAuthUserName := ACredential.UserName;
   LRequest.FAuthPassword := ACredential.Password;

   SetAuthResponse(LRequest);

   Result := True;
end;

function TAndroidHTTPClient.ExistServerCertInfo(AServerCert: JX509Certificate): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FServerCertCache) do
    if FServerCertCache[I].equals(AServerCert) then
      Exit(True);
end;

function TAndroidHTTPClient.FindAuthInfo(const AnAuthArray: TAuthArray; const AURL: TURI): TAuthInfo;

  function GetURL(const AURI: TURI): string;
  begin
    Result := AURI.Host + ':' + AURI.Port.ToString + AURI.Path;
  end;

var
  LAuthArray: TAuthArray;
  I: Integer;
  Pos: Integer;
  URL: string;
  LHost: string;
begin
  Result := Default(TAuthInfo);
  if Length(AnAuthArray) > 0 then
  begin
    LAuthArray := AnAuthArray;
    TArray.Sort<TAuthInfo>(LAuthArray, FAuthURIComparer);

    URL := GetURL(AURL);
    LHost := AURL.Host + ':' + AURL.Port.ToString + '/';

    while URL <> LHost do
    begin
      for I := 0 to High(LAuthArray) do
      begin
        if URL.StartsWith(GetURL(LAuthArray[I].URL), True) then
          Exit(LAuthArray[I]);
      end;

      Pos := URL.Substring(0, URL.Length - 1).LastIndexOf('/');
      if Pos < 0 then
        Break;

      URL := URL.Substring(0, Pos + 1);
    end;

    for I := High(LAuthArray) downto 0 do
      if  GetURL(LAuthArray[I].URL).StartsWith(LHost, True) then
        Exit(LAuthArray[I]);
  end;
end;

function TAndroidHTTPClient.GenerateBasicResponse(const AuthInfo: TAuthInfo): string;
begin
  Result := 'Basic ' + TNetEncoding.Base64String.Encode(AuthInfo.Username + ':' + AuthInfo.Password); // do not translate
end;

function TAndroidHTTPClient.GetAuthInfo(const HeaderValue: string; const ARequest: TAndroidHTTPRequest): TAuthInfo;
var
  LList: THeaderValueList;
begin
  Result := Default(TAuthInfo);
  LList := THeaderValueList.Create(HeaderValue);
  try
    if LList.Subject = 'Digest' then     // do not translate
      Result.AuthType := TCredentialsStorage.TAuthSchemeType.Digest
    else if LList.Subject = 'Basic' then // do not translate
      Result.AuthType := TCredentialsStorage.TAuthSchemeType.Basic
    else
      Exit;

    Result.Username := ARequest.FAuthUserName;
    Result.Password := ARequest.FAuthPassword;
    Result.URL := ARequest.FURL;

    Result.realm := LList['realm'];      // do not translate
    if Result.AuthType = TCredentialsStorage.TAuthSchemeType.Digest then
    begin
      Result.algorithm := LList['algorithm']; // do not translate
      if Result.algorithm = '' then
        Result.algorithm := 'MD5';      // do not translate
      Result.nonce := LList['nonce'];   // do not translate
      Result.opaque := LList['opaque']; // do not translate
      Result.qop := LList['qop'];       // do not translate
    end;
  finally
    LList.Free;
  end;
end;

function TAndroidHTTPClient.GenerateDigestResponse(const AuthInfo: TAuthInfo; const AURL: TURI; const AMethodString: string): string;

  function AddDigestValue(const AKey, AValue: string; Quoted, IsLast: Boolean): string;
  begin
    Result := '';
    if AValue <> '' then
    begin
      if Quoted then
        Result := '"' + AValue + '"'
      else
        Result := AValue;

      Result := AKey + '=' + Result;
      if not IsLast then
        Result := Result + ', ';
    end;
  end;

  function GetURI(const AURI: TURI): string;
  begin
    Result := AURI.Path;
    if AURI.Query <> '' then
      Result := Result + '?' + AURI.Query;
  end;

var
  response: string;
  cnonce: string;
  HA1, HA2: string;
begin
  //Create nonce
  cnonce := THashMD5.GetHashString(Double(Now()).toString);

  //Create response
  HA1 := THashMD5.GetHashString(AuthInfo.Username + ':' + AuthInfo.realm + ':' + AuthInfo.Password);
  HA2 := THashMD5.GetHashString(AMethodString + ':' + GetURI(AURL));

  if AuthInfo.qop = '' then
    response := THashMD5.GetHashString(HA1 + ':' + AuthInfo.nonce + ':' + HA2)
  else
    response := THashMD5.GetHashString(HA1 + ':' + AuthInfo.nonce + ':00000001:' + cnonce + ':' + AuthInfo.qop + ':' + HA2);

  Result := 'Digest ' + AddDigestValue('username', AuthInfo.Username, True, False) +   // do not translate
    AddDigestValue('realm', AuthInfo.realm, True, False) +    // do not translate
    AddDigestValue('nonce', AuthInfo.nonce, True, False) +   // do not translate
    AddDigestValue('uri', GetURI(AURL), True, False) +   // do not translate
    AddDigestValue('algorithm', AuthInfo.algorithm, False, False) +   // do not translate
    AddDigestValue('response', response, True, False) +    // do not translate
    AddDigestValue('opaque', AuthInfo.opaque, True, False) +  // do not translate
    AddDigestValue('qop', AuthInfo.qop, False, False) +   // do not translate
    AddDigestValue('nc', '00000001', False, False) +   // do not translate
    AddDigestValue('cnonce', cnonce, True, True);   // do not translate
end;

function TAndroidHTTPClient.GenerateResponse(const AuthInfo: TAuthInfo; const AURL: TURI; const AMethodString: string): string;
begin
  if AuthInfo.Username = '' then
    Result := ''
  else
    if AuthInfo.AuthType = TCredentialsStorage.TAuthSchemeType.Digest then
      Result := GenerateDigestResponse(AuthInfo, AURL, AMethodString)
    else if AuthInfo.AuthType = TCredentialsStorage.TAuthSchemeType.Basic then
      Result := GenerateBasicResponse(AuthInfo);
end;

procedure TAndroidHTTPClient.SetAuth(const AuthTargetType: TAuthTargetType; const AuthInfo: TAuthInfo;
  const ARequest: TAndroidHTTPRequest);
var
  Response: string;
begin
  Response := GenerateResponse(AuthInfo, ARequest.FURL, ARequest.FMethodString);
  if Response <> '' then
    if AuthTargetType = TAuthTargetType.Server then
      ARequest.AddHeader(sAuthorization, Response)
    else
      ARequest.AddHeader(sProxyAuthorization, Response);
end;

procedure TAndroidHTTPClient.SetAuthResponse(const ARequest: TAndroidHTTPRequest);
var
  HeaderValue: string;
  AuthInfo: TAuthInfo;
  AuthTargetType: TAuthTargetType;
  Len: Integer;
begin
  // It is preemptive authentication. Do not call here GetHeaderValue, it will call
  // FRequest.FJURLConnection.getHeaderFieldKey, which will open connection. So,
  // TAndroidHTTPClient.DoExecuteRequest will fail to set headers with:
  // java.lang.IllegalStateException: Cannot set request property after connection is made
  if PreemptiveAuthentication and (Length(ARequest.FResponse.FHeaders) = 0) then
    HeaderValue := 'Basic' // do not translate
  else
    HeaderValue := ARequest.FResponse.GetHeaderValue(sWWWAuthenticate);
  if HeaderValue <> '' then
    AuthTargetType := TAuthTargetType.Server
  else
  begin
    HeaderValue := ARequest.FResponse.GetHeaderValue(sProxyAuthenticate);
    AuthTargetType := TAuthTargetType.Proxy;
  end;

  if HeaderValue <> '' then
  begin
    AuthInfo := GetAuthInfo(HeaderValue, ARequest);
    SetAuth(AuthTargetType, AuthInfo, ARequest);
    if AuthTargetType = TAuthTargetType.Server then
    begin
      Len := Length(FAuthServerCache);
      SetLength(FAuthServerCache, Len + 1);
      FAuthServerCache[Len] := AuthInfo;
    end
    else
    begin
      Len := Length(FAuthProxyCache);
      SetLength(FAuthProxyCache, Len + 1);
      FAuthProxyCache[Len] := AuthInfo;
    end
  end;
end;

procedure TAndroidHTTPClient.SetPreemptiveProxyAuth(const ARequest: TAndroidHTTPRequest);
var
  AuthInfo: TAuthInfo;
begin
  if (ProxySettings.Host <> '') and (ProxySettings.UserName <> '') then
  begin
    AuthInfo := Default(TAuthInfo);
    AuthInfo.AuthType := TCredentialsStorage.TAuthSchemeType.Basic;
    AuthInfo.Username := ProxySettings.UserName;
    AuthInfo.Password := ProxySettings.Password;
  end
  else
    AuthInfo := FindAuthInfo(FAuthProxyCache, ARequest.FURL);
  SetAuth(TAuthTargetType.Proxy, AuthInfo, ARequest);
end;

procedure TAndroidHTTPClient.SetPreemptiveServerAuth(const ARequest: TAndroidHTTPRequest);
var
  AuthInfo: TAuthInfo;
begin
  AuthInfo := FindAuthInfo(FAuthServerCache, ARequest.FURL);
  SetAuth(TAuthTargetType.Server, AuthInfo, ARequest);
end;

{ TAndroidHTTPRequest }

constructor TAndroidHTTPRequest.Create(const AClient: THTTPClient; const ARequestMethod: string; const AURI: TURI);
var
  LClient: TAndroidHTTPClient;
  I: Integer;
  LClientCertURIComparer: TClientCertURIComparer;
begin
  inherited Create(AClient, ARequestMethod, AURI);
  FHeaders := TDictionary<string, string>.Create;
  FClientCertEvent := TEvent.Create;

  FClientCertCache := TList<TAndroidHTTPClient.TClientCertInfo>.Create;
  LClient := TAndroidHTTPClient(AClient);
  for I := 0 to High(LClient.FClientCertCache) do
    FClientCertCache.Add(LClient.FClientCertCache[I]);
  LClientCertURIComparer := TClientCertURIComparer.Create;
  FClientCertCache.Sort(LClientCertURIComparer);
  LClientCertURIComparer.Free;
  FMoreClientCert := False;
end;

destructor TAndroidHTTPRequest.Destroy;
begin
  FClientCertCache.Free;
                                                                                  
  if FJTrustManager <> nil then
  begin
    FJTrustManager._Release;
    FJTrustManager := nil;
  end;
  if FJKeyManager <> nil then
  begin
    FJKeyManager._Release;
    FJKeyManager := nil;
  end;
  if FClientCertAliasCallback <> nil then
  begin
    FClientCertAliasCallback._Release;
    FClientCertAliasCallback := nil;
  end;
  if FJHostVerifier <> nil then
  begin
    FJHostVerifier._Release;
    FJHostVerifier := nil;
  end;
  FJClientCertAlias := nil;
  FJServerCertificate := nil;
  FResponse := nil;
  FJURLConnection := nil;
  FClientCertEvent.Free;
  FHeaders.Free;
  inherited;
end;

procedure TAndroidHTTPRequest.SetSecureProtocols(const AJSSLContext: JSSLContext);
var
  LJSSLEngine: JSSLEngine;
  LJProtocols: TJavaObjectArray<JString>;
  LNewProtocols: TStringList;
  i: Integer;
  s: string;

  procedure CheckProtocol(AProt: THTTPSecureProtocol; const AName: string);
  begin
    if (AProt in TAndroidHTTPClient(FClient).SecureProtocols) and SameText(s, AName) then
      LNewProtocols.Add(AName);
  end;

begin
  LJSSLEngine := AJSSLContext.createSSLEngine;
  LNewProtocols := TStringList.Create;
  try
    LJProtocols := LJSSLEngine.getSupportedProtocols;
    try
      for i := 0 to LJProtocols.Length - 1 do
      begin
        s := JStringToString(LJProtocols[i]);
        CheckProtocol(THTTPSecureProtocol.SSL2, 'SSLv2');
        CheckProtocol(THTTPSecureProtocol.SSL3, 'SSLv3');
        CheckProtocol(THTTPSecureProtocol.TLS1, 'TLSv1');
        CheckProtocol(THTTPSecureProtocol.TLS11, 'TLSv1.1');
        CheckProtocol(THTTPSecureProtocol.TLS12, 'TLSv1.2');
        CheckProtocol(THTTPSecureProtocol.TLS13, 'TLSv1.3');
      end;
    finally
      LJProtocols.Free;
    end;
    if LNewProtocols.Count > 0 then
    begin
      LJProtocols := TJavaObjectArray<JString>.Create(LNewProtocols.Count);
      try
        for i := 0 to LNewProtocols.Count - 1 do
          LJProtocols[i] := StringToJString(LNewProtocols[i]);
        LJSSLEngine.setEnabledProtocols(LJProtocols);
      finally
        LJProtocols.Free;
      end;
    end;
  finally
    LNewProtocols.Free;
  end;
end;

procedure TAndroidHTTPRequest.DoSetConnectionTimeout(AValue: Integer);
begin
  if (FJURLConnection <> nil) and (AValue <> 0) then
    if AValue < 0 then
      FJURLConnection.setConnectTimeout(0)
    else
      FJURLConnection.setConnectTimeout(AValue);
end;

procedure TAndroidHTTPRequest.DoSetResponseTimeout(AValue: Integer);
begin
  if (FJURLConnection <> nil) and (AValue <> 0) then
    if AValue < 0 then
      FJURLConnection.setReadTimeout(0)
    else
      FJURLConnection.setReadTimeout(AValue);
end;

procedure TAndroidHTTPRequest.DoPrepare;
var
  LJURL: JURL;
  LJProxy: JProxy;
  LJSSLContext: JSSLContext;
  LArrayTrustManager: TJavaObjectArray<JTrustManager>;
  LArrayKeyManager: TJavaObjectArray<JKeyManager>;
  LOldTrustManager: JX509TrustManager;
  LOldKeyManager: JX509KeyManager;
  LManagerFactory: JTrustManagerFactory;
  LTrustManagers: TJavaObjectArray<JTrustManager>;
  LJKeyManagerFactory: JKeyManagerFactory;
  LKeyManagers: TJavaObjectArray<JKeyManager>;
begin
  inherited;
  if FJURLConnection <> nil  then
    FJURLConnection.disconnect;


  // Set proxy if needed
  if FClient.ProxySettings.Host <> '' then
  begin
    LJProxy := TJProxy.JavaClass.init(TJProxy_Type.JavaClass.HTTP,
      TJInetSocketAddress.JavaClass.init(StringToJString(FClient.ProxySettings.Host), FClient.ProxySettings.Port));
  end;

  LJURL := TJURL.JavaClass.init(StringToJString(FURL.ToString));

  if LJProxy <> nil then
    FJURLConnection := TJHttpURLConnection.Wrap(LJURL.openConnection(LJProxy))
  else
    FJURLConnection := TJHttpURLConnection.Wrap(LJURL.openConnection);

  DoSetConnectionTimeout(ConnectionTimeout);
  DoSetResponseTimeout(ResponseTimeout);

  FJURLConnection.setInstanceFollowRedirects(False);

  if FURL.Scheme = TURI.SCHEME_HTTPS then
  begin
    LJSSLContext := TJSSLContext.JavaClass.getInstance(StringToJString('TLS'));

    // TrustManager
    LManagerFactory := TJTrustManagerFactory.JavaClass.getInstance(TJTrustManagerFactory.JavaClass.getDefaultAlgorithm());
    LManagerFactory.init(TJKeyStore.Wrap(nil));
    LTrustManagers := LManagerFactory.getTrustManagers();
    try
      LOldTrustManager := TJX509TrustManager.Wrap(LTrustManagers[0]); // Get Current Trust Manager.
    finally
      LTrustManagers.Free;
    end;

    FJTrustManager := TX509TrustManager.Create(LOldTrustManager, Self);
    LArrayTrustManager := TJavaObjectArray<JTrustManager>.Create(1);
    LKeyManagers := nil;
    LArrayKeyManager := nil;
    try
      LArrayTrustManager.Items[0] := TJTrustManager.Wrap(FJTrustManager);

      // KeyManager
      LJKeyManagerFactory := TJKeyManagerFactory.JavaClass.getInstance(TJKeyManagerFactory.JavaClass.getDefaultAlgorithm());
      LJKeyManagerFactory.init(nil, nil);
      LKeyManagers := LJKeyManagerFactory.getKeyManagers;
      LOldKeyManager := TJX509KeyManager.Wrap(LKeyManagers[0]); // Get Current Key Manager.

      FJKeyManager := TX509KeyManager.Create(LOldKeyManager, Self);
      LArrayKeyManager := TJavaObjectArray<JKeyManager>.Create(1);
      LArrayKeyManager.Items[0] := TJKeyManager.Wrap(FJKeyManager);

      // KeyManager KeyChainAliasCallback
      FClientCertAliasCallback := TAliasCallback.Create(Self);

      // Trick at now to load TJKeyChain class to get it working in proxys. We can not load classes inside proxys, need investigation
      TJKeyChain.Create;

      LJSSLContext.init(LArrayKeyManager, LArrayTrustManager, nil);
    finally
      LArrayKeyManager.Free;
      LArrayTrustManager.Free;
      LKeyManagers.Free;
    end;

    if TAndroidHTTPClient(FClient).SecureProtocols <> [] then
      SetSecureProtocols(LJSSLContext);
    TJHttpsURLConnection.Wrap(FJURLConnection).setSSLSocketFactory(LJSSLContext.getSocketFactory);

    FJHostVerifier := TJHostnameVerifier.Create(Self);
    TJHttpsURLConnection.Wrap(FJURLConnection).setHostnameVerifier(FJHostVerifier);
  end;

  FJURLConnection.setRequestMethod(StringToJString(FMethodString));
  SetPreemptiveAuth;
end;

function TAndroidHTTPRequest.FindClientCertInfo(const AURL: TURI): TAndroidHTTPClient.TClientCertInfo;

  function GetURL(const AURI: TURI): string;
  begin
    Result := AURI.Host + ':' + AURI.Port.ToString + AURI.Path;
  end;

var
  I: Integer;
  Pos: Integer;
  URL: string;
  LHost: string;
begin
  Result := Default(TAndroidHTTPClient.TClientCertInfo);

  if FClientCertCache.Count > 0 then
  begin
    URL := GetURL(AURL);
    LHost := AURL.Host + ':' + AURL.Port.ToString + '/';

    while URL <> LHost do
    begin
      for I := 0 to FClientCertCache.Count - 1 do
      begin
        if URL.StartsWith(GetURL(FClientCertCache[I].URL), True) then
        begin
          Result := FClientCertCache[I];
          FClientCertCache.Delete(I);
          Exit;
        end;
      end;

      Pos := URL.Substring(0, URL.Length - 1).LastIndexOf('/');
      if Pos < 0 then
        Break;

      URL := URL.Substring(0, Pos + 1);
    end;

    for I := FClientCertCache.Count - 1 downto 0 do
      if  GetURL(FClientCertCache[I].URL).StartsWith(LHost, True) then
      begin
        Result := FClientCertCache[I];
        FClientCertCache.Delete(I);
        Exit;
      end;
  end;
end;

function TAndroidHTTPRequest.GetHeaders: TNetHeaders;
var
  Value: TPair<string, string>;
  CntHeader: Integer;
begin
  SetLength(Result, 500); // Max 500 headers
  CntHeader := 0;
  for Value in FHeaders do
  begin
    Result[CntHeader].Create(Value.Key, Value.Value);
    Inc(CntHeader);
  end;
  SetLength(Result, CntHeader);
end;

function TAndroidHTTPRequest.GetHeaderValue(const AName: string): string;
begin
  Result := '';
  FHeaders.TryGetValue(AName, Result);
end;

procedure TAndroidHTTPRequest.AddHeader(const AName, AValue: string);
begin
  inherited;
  BaseAddHeader(AName, AValue);
end;

function TAndroidHTTPRequest.RemoveHeader(const AName: string): Boolean;
begin
  Result := True;
  if GetHeaderValue(AName) = '' then
    Result := False
  else
    FHeaders.Remove(AName);
end;

procedure TAndroidHTTPRequest.SetHeaderValue(const AName, Value: string);
begin
  inherited;
  FHeaders.AddOrSetValue(AName, Value);
end;

procedure TAndroidHTTPRequest.SetConnectionTimeout(const Value: Integer);
begin
  inherited;
  DoSetConnectionTimeout(Value);
end;

procedure TAndroidHTTPRequest.SetResponseTimeout(const Value: Integer);
begin
  inherited;
  DoSetResponseTimeout(Value);
end;

procedure TAndroidHTTPRequest.SetPreemptiveAuth;
var
  LClient: TAndroidHTTPClient;
begin
  LClient := TAndroidHTTPClient(FClient);
  LClient.SetPreemptiveServerAuth(Self);
  LClient.SetPreemptiveProxyAuth(Self);
end;

procedure TAndroidHTTPRequest.WriteData;
const
  BUFFERSIZE = 64 * 1024;
var
  LDataLength: Int64;
  LWritten: Int64;
  LToWrite: LongInt;
  LJStream: JOutputStream;
  LJBuffer: TJavaArray<System.Byte>;
  LAbort: Boolean;
begin
  if FSourceStream <> nil then
  begin
    LDataLength := FSourceStream.Size - FSourceStream.Position;
    if not TOSVersion.Check(4, 4) then // api < 19
      LDataLength := Integer(LDataLength);
    if LDataLength > 0 then
    begin
      DoSendDataProgress(LDataLength, 0, LAbort, True);
      if not LAbort then
      begin
        LJBuffer := TJavaArray<System.Byte>.Create(BUFFERSIZE);
        try
          FJURLConnection.setDoOutput(True);
          if TOSVersion.Check(4, 4) then
            FJURLConnection.setFixedLengthStreamingMode(LDataLength)
          else
            FJURLConnection.setFixedLengthStreamingMode(Integer(LDataLength));
          LJStream := FJURLConnection.getOutputStream;
          LWritten := 0;
          while not LAbort and (LWritten < LDataLength) do
          begin
            LToWrite := BUFFERSIZE;
            if LDataLength - LWritten < LToWrite then
              LToWrite := LDataLength - LWritten;
            FSourceStream.ReadBuffer(LJBuffer.Data^, LToWrite);
            LJStream.write(LJBuffer, 0, LToWrite);
            LWritten := LWritten + LToWrite;
            DoSendDataProgress(LDataLength, LWritten, LAbort, True);
          end;
        finally
          LJBuffer.Free;
        end;
      end;
    end;
  end;
end;

{ TAndroidHTTPResponse }

constructor TAndroidHTTPResponse.Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback; const AAsyncCallbackEvent: TAsyncCallbackEvent;
      const ARequest: TAndroidHTTPRequest; const AContentStream: TStream);
begin
  inherited Create(AContext, AProc, AAsyncCallback, AAsyncCallbackEvent, ARequest as TAndroidHTTPRequest, AContentStream);
  FRequest := ARequest;
  FRequest.FResponse := Self;
end;

destructor TAndroidHTTPResponse.Destroy;
begin
  FRequest := nil;
  inherited;
end;

function TAndroidHTTPResponse.GetDecompressResponse: Boolean;
var
  LContEnc: string;
begin
  LContEnc := GetContentEncoding;
  Result := ((LContEnc = 'gzip') or (LContEnc = 'deflate')); // do not translate
end;

procedure TAndroidHTTPResponse.DoReadData(const AStream: TStream);
const
  BUFFERSIZE = 64 * 1024;  // Usual TCP Window Size
var
  LJBuffer: TJavaArray<System.Byte>;
  LJIStream: JInputStream;
  LExpected, LTotalReaded: Int64;
  LReaded: Integer;
  LStatusCode: Integer;
  LAbort: Boolean;
begin
  inherited;
  LTotalReaded := 0;
  LStatusCode := GetStatusCode;
  LExpected := GetContentLength;
  if LExpected = 0 then
    LExpected := -1;
  FRequest.DoReceiveDataProgress(LStatusCode, LExpected, LTotalReaded, LAbort);
  if not LAbort then
  begin
    LJBuffer := TJavaArray<System.Byte>.Create(BUFFERSIZE);
    try
      try
        if GetStatusCode < 300 then
          LJIStream := FRequest.FJURLConnection.getInputStream
        else
          LJIStream := FRequest.FJURLConnection.getErrorStream;
      except
        LJIStream := FRequest.FJURLConnection.getErrorStream;
      end;

      LReaded := 0;
      while not LAbort and (LJIStream <> nil) and (LReaded >= 0) do
      begin
        LReaded := LJIStream.read(LJBuffer);
        if LReaded > 0 then
        begin
          AStream.WriteData(LJBuffer.Data, LReaded);
          LTotalReaded := LTotalReaded + LReaded;
          FRequest.DoReceiveDataProgress(LStatusCode, LExpected, LTotalReaded, LAbort);
        end;
      end;
    finally
      if LJIStream <> nil then
        LJIStream.close;
      LJBuffer.Free;
    end;
  end;
end;

procedure TAndroidHTTPResponse.UpdateHeaders;
begin
  SetLength(FHeaders, 0);
  FResponseCode := 0;
  FStatusText := '';
  FVersion := THTTPProtocolVersion.UNKNOWN_HTTP;

  GetHeaders;
  GetStatusCode;
  GetStatusText;
  GetVersion;
end;

function TAndroidHTTPResponse.GetHeaders: TNetHeaders;
var
  LHeader: string;
  LValue: string;
  I: Integer;
  P: Integer;

  procedure AddOrSetHeader;
  var
    J: Integer;
  begin
    for J := 0 to P - 1 do
    begin
      if SameText(FHeaders[J].Name, LHeader) then
      begin
        FHeaders[J].Value := FHeaders[J].Value + ', ' + LValue;
        Exit;
      end;
    end;
    FHeaders[P].Create(LHeader, LValue);
    Inc(P);
  end;

begin
  if Length(FHeaders) = 0 then
  begin
    SetLength(FHeaders, 500); // Max 500 headers
    I := 0;
    P := 0;
    while True do
    begin
      LHeader := JStringToString(FRequest.FJURLConnection.getHeaderFieldKey(I));
      if LHeader = '' then
        Break;
      LValue := JStringToString(FRequest.FJURLConnection.getHeaderField(I));
      if SameText(LHeader, sSetCookie) then
        InternalAddCookie(LValue.Trim)
      else
        AddOrSetHeader;
      Inc(I);
    end;
    SetLength(FHeaders, P);
  end;
  Result := FHeaders;
end;

function TAndroidHTTPResponse.GetStatusCode: Integer;
begin
  if FResponseCode = 0 then
    FResponseCode := FRequest.FJURLConnection.getResponseCode;
  Result := FResponseCode;
end;

function TAndroidHTTPResponse.GetStatusText: string;
begin
  if FStatusText = '' then
    FStatusText := JStringToString(FRequest.FJURLConnection.getResponseMessage);
  Result := FStatusText;
end;

function TAndroidHTTPResponse.GetVersion: THTTPProtocolVersion;
var
  LStatusLine: string;
  Version: string;
  LValues: TArray<string>;
  LURLConnection: JURLConnection;
begin
  if FVersion = THTTPProtocolVersion.UNKNOWN_HTTP then
  begin
    LURLConnection := TJURLConnection.Wrap(FRequest.FJURLConnection);
    LStatusLine := JStringToString(LURLConnection.getHeaderField(nil));  // Get the status line
    if LStatusLine <> '' then
    begin
      LValues := LStatusLine.Split([' ']);
      Version := LValues[0];
      if string.CompareText(Version, 'HTTP/1.0') = 0 then
        FVersion := THTTPProtocolVersion.HTTP_1_0
      else if string.CompareText(Version, 'HTTP/1.1') = 0 then
        FVersion := THTTPProtocolVersion.HTTP_1_1
      else if string.CompareText(Version, 'HTTP/2.0') = 0 then
        FVersion := THTTPProtocolVersion.HTTP_2_0
      else
        FVersion := THTTPProtocolVersion.UNKNOWN_HTTP;
    end
    else
      FVersion := THTTPProtocolVersion.UNKNOWN_HTTP;
  end;
  Result := FVersion;
end;

{ TJHostnameVerifier }

constructor TJHostnameVerifier.Create(const ARequest: TAndroidHTTPRequest);
begin
  inherited Create;
  FRequest := ARequest;
end;

function TJHostnameVerifier.verify(hostname: JString; session: JSSLSession): Boolean;
begin
  if not FRequest.FServerCertificate.IsEmpty and
     TAndroidHTTPClient(FRequest.FClient).ExistServerCertInfo(FRequest.FJServerCertificate) then
    Result := True
  else
    Result := TJHttpsURLConnection.JavaClass.getDefaultHostnameVerifier.verify(hostname, session);
end;

{ TX509KeyManager }

constructor TX509KeyManager.Create(AKeyManager: JX509KeyManager; const ARequest: TAndroidHTTPRequest);
begin
  inherited Create;
  FJOrigOldKeyManager := AKeyManager;
  FRequest := ARequest;
end;

function TX509KeyManager.chooseClientAlias(keyType: TJavaObjectArray<JString>; issuers: TJavaObjectArray<JPrincipal>;
  socket: JSocket): JString;
begin
  if not FRequest.FClientCertPath.IsEmpty then
  begin
    FUsingCache := True;
    FUsingKeyChain := True;
    FRequest.FMoreClientCert := False;
    Exit(StringToJString(FRequest.FClientCertPath));
  end;

  FRequest.FMoreClientCert := True;
  Result := FJOrigOldKeyManager.chooseClientAlias (keyType, issuers, socket);

  if Result = nil then
  begin
    FUsingCache := True;
    FClientCertInfo := FRequest.FindClientCertInfo(FRequest.FURL);
    if FClientCertInfo.URL.Host = '' then
    begin
      FUsingKeyChain := True;
      TJKeyChain.JavaClass.choosePrivateKeyAlias(TAndroidHelper.Activity, FRequest.FClientCertAliasCallback, keyType, issuers, nil, -1, nil);
      FRequest.FClientCertEvent.WaitFor(INFINITE);
      FRequest.FClientCertEvent.ResetEvent;
      Result := FRequest.FJClientCertAlias;
    end
    else
      Result := FClientCertInfo.Alias;
  end;
  if Result = nil then // we can not find a valid client certificate
    FRequest.FMoreClientCert := False;
end;

function TX509KeyManager.chooseServerAlias(keyType: JString; issuers: TJavaObjectArray<JPrincipal>;
  socket: JSocket): JString;
begin
  Result := FJOrigOldKeyManager.chooseServerAlias(keyType, issuers, socket);
end;

function TX509KeyManager.getCertificateChain(alias: JString): TJavaObjectArray<JX509Certificate>;
begin
  if FUsingKeyChain then
  begin
    Result := TJKeyChain.JavaClass.getCertificateChain(TAndroidHelper.Context, alias);
    TAndroidHTTPClient(FRequest.FClient).AddClientCertInfo(FRequest.FURL, alias,
      TJKeyChain.JavaClass.getPrivateKey(TAndroidHelper.Context, alias), Result);
  end
  else if FUsingCache then
    Result := FClientCertInfo.CertificateChain
  else
    Result := FJOrigOldKeyManager.getCertificateChain(alias);
end;

function TX509KeyManager.getClientAliases(keyType: JString;
  issuers: TJavaObjectArray<JPrincipal>): TJavaObjectArray<JString>;
begin
  Result := FJOrigOldKeyManager.getClientAliases(keyType, issuers);
end;

function TX509KeyManager.getPrivateKey(alias: JString): JPrivateKey;
begin
  if FUsingKeyChain then
    Result := TJKeyChain.JavaClass.getPrivateKey(TAndroidHelper.Context, alias)
  else if FUsingCache then
    Result := FClientCertInfo.PrivateKey
  else
    Result := FJOrigOldKeyManager.getPrivateKey(alias);
end;

function TX509KeyManager.getServerAliases(keyType: JString;
  issuers: TJavaObjectArray<JPrincipal>): TJavaObjectArray<JString>;
begin
  Result := FJOrigOldKeyManager.getServerAliases(keyType, issuers);
end;

{ TAliasCallback }

procedure TAliasCallback.alias(alias: JString);
begin
  FRequest.FJClientCertAlias := alias;
  FRequest.FClientCertEvent.SetEvent;
end;

constructor TAliasCallback.Create(const ARequest: TAndroidHTTPRequest);
begin
  inherited Create;
  FRequest := ARequest;
end;

{ TAndroidHTTPClient.TAuthURIComparer }

function TAndroidHTTPClient.TAuthURIComparer.Compare(const Left, Right: TAuthInfo): Integer;
begin
  Result := string.CompareText(Left.URL.Host + ':' + Left.URL.Port.ToString, Right.URL.Host + ':' + Right.URL.Port.ToString);
  if Result = 0 then
    Result := - string.CompareText(Left.URL.Path, Right.URL.Path);
end;

{ TAndroidHTTPRequest.TClientCertURIComparer }

function TAndroidHTTPRequest.TClientCertURIComparer.Compare(const Left,
  Right: TAndroidHTTPClient.TClientCertInfo): Integer;
begin
  Result := string.CompareText(Left.URL.Host + ':' + Left.URL.Port.ToString, Right.URL.Host + ':' + Right.URL.Port.ToString);
  if Result = 0 then
    Result := - string.CompareText(Left.URL.Path, Right.URL.Path);
end;

initialization
  TURLSchemes.RegisterURLClientScheme(TAndroidHTTPClient, 'HTTP');
  TURLSchemes.RegisterURLClientScheme(TAndroidHTTPClient, 'HTTPS');

finalization
  TURLSchemes.UnRegisterURLClientScheme('HTTP');
  TURLSchemes.UnRegisterURLClientScheme('HTTPS');

end.
