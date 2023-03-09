{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                 SOAP HTTP Transport                   }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Soap.SOAPHTTPTrans;

{$LEGACYIFEND ON}

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Net.URLClient, System.Net.HttpClient,
  Soap.IntfInfo, Soap.SOAPAttachIntf, Soap.WebNode, Soap.WSDLIntf, Soap.WSDLNode;

type
  ESOAPHTTPException = class(ENetException)
  private
    FStatusCode: Integer;
    FStatusText: string;
    FURL: string;
  public
    constructor Create(AStatusCode: Integer; const AStatusText, AURL: string); overload;
    property StatusCode: Integer read FStatusCode;
    property StatusText: string read FStatusText;
    property URL: string read FURL;
  end;

  SOAPInvokeOptions = (soNoValueForEmptySOAPAction,   { Send "" or absolutely no value for empty SOAPAction }
                       soIgnoreInvalidCerts,          { Handle Invalid Server Cert and ask HTTP runtime to ignore }
                       soNoSOAPActionHeader,          { Don't send SOAPAction - use very very carefully!! }
                       soAutoCheckAccessPointViaUDDI, { If we get a status code 404/405/410 - contact UDDI }
                       soPickFirstClientCertificate   { If certificate info is not specified - use first }
                       );
  TSOAPInvokeOptions = set of SOAPInvokeOptions;
  TSOAPHttpErrorAction = (heaSuccess, heaError, heaAbort, heaRetry);

  THTTPReqResp = class;

  { Provides access to HTTPReqResp component }
  IHTTPReqResp = interface
  ['{5FA6A197-32DE-4225-BC85-216CB80D1561}']
    function GetHTTPReqResp: THTTPReqResp;
  end;

  TBeforePostEvent = procedure(const HTTPReqResp: THTTPReqResp; Client: THTTPClient) of object;
  TPostingDataEvent = procedure(Sent: Integer; Total: Integer) of object;
  TReceivingDataEvent = procedure(Read: Integer; Total: Integer) of object;
  TSOAPHttpErrorEvent = procedure(const HTTPReqResp: THTTPReqResp; const HTTPResponse: IHTTPResponse;
    const Error: ESOAPHTTPException; var Action: TSOAPHttpErrorAction) of object;

  TClientCert = class(TComponent)
  private
    FCert: TCertificate;
  published
    property SerialNum: string read FCert.SerialNum write FCert.SerialNum;
    property CertName: string read FCert.CertName write FCert.CertName;
    property Subject: string read FCert.Subject write FCert.Subject;
    property Issuer: string read FCert.Issuer write FCert.Issuer;
    property ProtocolName: string read FCert.ProtocolName write FCert.ProtocolName;
  end;

                                                                                
  TClientCertExt = class(TClientCert)
  private
    FStream: TStream;
    FFileName: TFileName;
    FPassword: string;
  public
    procedure Assign(ASource: TPersistent); override;
    property Stream: TStream read FStream write FStream;
  published
    property FileName: TFileName read FFileName write FFileName;
    property Password: string read FPassword write FPassword;
  end;

  THTTPReqResp = class(TComponent, IInterface, IWebNode, IHTTPReqResp, IStreamLoaderCustomizer)
  private
    FHTTP: THTTPClient;
    FUserSetURL: Boolean;
{$IFNDEF AUTOREFCOUNT}
    FRefCount: Integer;
{$ENDIF !AUTOREFCOUNT}
    FOwnerIsComponent: Boolean;
    FURL: string;
    FBindingType: TWebServiceBindingType;
    FMimeBoundary: string;
    FWebNodeOptions: WebNodeOptions;
    FUserName: string;
    FPassword: string;
    FProxy: string;
    FWSDLView: TWSDLView;
    FSoapAction: string;
    FUseUTF8InHeader: Boolean;
    FInvokeOptions: TSOAPInvokeOptions;
    FUDDIBindingKey: string;
    FUDDIOperator: String;
    FOnBeforePost: TBeforePostEvent;
    FOnPostingData: TPostingDataEvent;
    FOnReceivingData: TReceivingDataEvent;
    FClientCertificate: TClientCert;
    FNeedClientCertificateEvent: TNeedClientCertificateEvent;
    FOnHttpError: TSOAPHttpErrorEvent;
    procedure SetURL(const Value: string);
    function  GetSOAPAction: string;
    procedure SetSOAPAction(const SOAPAction: string);
    function IsSOAPActionStored: Boolean;
    procedure SetWSDLView(const WSDLVIew: TWSDLView);
    function  GetContentType: string;
    function  GetSOAPActionHeader: string;
    procedure SetUsername(const NameValue: string);
    procedure SetPassword(const PasswordValue: string);
    procedure SetProxy(const ProxyValue: string);
    function  GetAgentIsStored:Boolean;
    procedure SetupHttp(Http: THttpClient);
    function GetConnectTimeout: Integer;
    function GetSendTimeout: Integer;
    function GetReceiveTimeout: Integer;
    procedure SetConnectTimeout(const Value: Integer);
    procedure SetSendTimeout(const Value: Integer);
    procedure SetReceiveTimeout(const Value: Integer);
    function GetAgent: string;
    procedure SetAgent(const Value: string);
    function GetHTTP: THTTPClient;
    function GetSecureProtocols: THTTPSecureProtocols;
    procedure SetSecureProtocols(const AValue: THTTPSecureProtocols);
    function GetPreemptiveAuthentication: Boolean;
    procedure SetPreemptiveAuthentication(const AValue: Boolean);
    function GetAutomaticDecompression: THTTPCompressionMethods;
    procedure SetAutomaticDecompression(const AValue: THTTPCompressionMethods);
    function GetUseDefaultCredentials: Boolean;
    procedure SetUseDefaultCredentials(const AValue: Boolean);
    procedure DoValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const Certificate: TCertificate;
      var Accepted: Boolean);
    procedure DoNeedClientCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificateList: TCertificateList;
      var AnIndex: Integer);
    procedure DoSendData(const Sender: TObject; AContentLength,
      AWriteCount: Int64; var AAbort: Boolean);
    procedure DoReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var AAbort: Boolean);
    function CheckResponseError(const AHTTPResponse: IHTTPResponse; AFailOn500: Boolean): TSOAPHttpErrorAction;
  protected
{$IFNDEF AUTOREFCOUNT}
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
{$ENDIF !AUTOREFCOUNT}
    function GetMimeBoundary: string;
    procedure SetMimeBoundary(const Value: string);
    function  GetWebNodeOptions: WebNodeOptions;
    procedure SetWebNodeOptions(Value: WebNodeOptions);
  public
    constructor Create(Owner: TComponent); override;
{$IFNDEF AUTOREFCOUNT}
    class function NewInstance: TObject; override;
{$ENDIF !AUTOREFCOUNT}
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function  GetHTTPReqResp: THTTPReqResp;
    procedure Get(Resp: TStream); virtual;
    {IWebNode}
    procedure BeforeExecute(const IntfMD: TIntfMetaData;
                            const MethMD: TIntfMethEntry;
                            MethodIndex: Integer;
                            AttachHandler: IMimeAttachmentHandler);
    procedure Execute(const DataMsg: String; Resp: TStream); overload; virtual;
    procedure Execute(const Request: TStream; Response: TStream); overload; virtual;
    function  Execute(const Request: TStream): TStream; overload; virtual;
    {IStreamLoaderCustomizer}
    procedure Customize(const ATransport: IInterface);

    property HTTP: THTTPClient read GetHTTP;
  published
    property  URL: string read FURL write SetURL;
    property  SoapAction: string read GetSOAPAction write SetSOAPAction
      stored IsSOAPActionStored;
    property  WSDLView: TWSDLView read FWSDLView write SetWSDLView;
    property  Agent: string read GetAgent write SetAgent stored GetAgentIsStored;
    property  UserName: string read FUserName write SetUserName;
    property  Password: string read FPassword write SetPassword;
    property  Proxy: string read FProxy write SetProxy;
    property  ClientCertificate: TClientCert read FClientCertificate;
    property  UseUTF8InHeader: Boolean read FUseUTF8InHeader write FUseUTF8InHeader
      default True;
    property  InvokeOptions: TSOAPInvokeOptions read FInvokeOptions write FInvokeOptions
      default [soIgnoreInvalidCerts, soAutoCheckAccessPointViaUDDI];
    property  WebNodeOptions: WebNodeOptions read GetWebNodeOptions write SetWebNodeOptions
      default [];
    property  UDDIBindingKey: string read FUDDIBindingKey write FUDDIBindingKey;
    property  UDDIOperator: string read FUDDIOperator write FUDDIOperator;
    property  ConnectTimeout: Integer read GetConnectTimeout write SetConnectTimeout
      default TURLClient.DefaultConnectionTimeout;
    property  SendTimeout: Integer read GetSendTimeout write SetSendTimeout
      default TURLClient.DefaultSendTimeout;
    property  ReceiveTimeout: Integer read GetReceiveTimeout write SetReceiveTimeout
      default TURLClient.DefaultResponseTimeout;
    /// <summary>Property controls which secure protocols may be used for
    /// HTTPS communication. By default will be used all protocols supported
    /// by the operating system. </summary>
    property SecureProtocols: THTTPSecureProtocols read GetSecureProtocols
      write SetSecureProtocols default CHTTPDefSecureProtocols;
    /// <summary>Property controls automatic decompression of response body.
    /// It is platform dependent and currently supported on Windows and Linux.
    /// When set, then corresponding "Accept-Encoding" header will be included
    /// into request, and response body will be automatically decoded. On iOS,
    /// macOS and Android platforms decoding is performed automatically. </summary>
    property AutomaticDecompression: THTTPCompressionMethods
      read GetAutomaticDecompression write SetAutomaticDecompression
      default [];
    /// <summary>Property controls preemptive authentication. When set to True, then
    /// basic authentication will be provided before the server gives an unauthorized response. </summary>
    property PreemptiveAuthentication: Boolean read GetPreemptiveAuthentication
      write SetPreemptiveAuthentication default True;
    /// <summary>Property controls automatic usage of logged user credentials
    /// for NTLM and Negotiate authentication schemas. It is platform dependent and
    /// currently supported on Windows and Linux. </summary>
    property UseDefaultCredentials: Boolean read GetUseDefaultCredentials
      write SetUseDefaultCredentials default True;

    { Events }
    property  OnBeforePost: TBeforePostEvent read FOnBeforePost write FOnBeforePost;
    property  OnPostingData: TPostingDataEvent read FOnPostingData write FOnPostingData;
    property  OnReceivingData: TReceivingDataEvent read FOnReceivingData write FOnReceivingData;
    property  OnNeedClientCertificate: TNeedClientCertificateEvent read FNeedClientCertificateEvent
      write FNeedClientCertificateEvent;
    property  OnHttpError: TSOAPHttpErrorEvent read FOnHttpError write FOnHttpError;
  end;

implementation

uses
  System.SyncObjs, System.NetConsts,
{$IFDEF MSWINDOWS}
  Winapi.Windows, System.Net.HttpClient.Win,
{$ENDIF}
  Soap.InvokeRegistry, Soap.SOAPAttach, Soap.SOAPConst, Soap.UDDIHelper, Soap.WSDLItems;

const
  SOAP_AGENT = 'Embarcadero SOAP 1.4'; { Do not localize }

{ ESOAPHTTPException }

constructor ESOAPHTTPException.Create(AStatusCode: Integer; const AStatusText, AURL: string);
begin
  inherited CreateFmt('%s (%d) - ''%s''', [AStatusText, AStatusCode, AURL]);
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
  FURL := AURL;
end;

{ TClientCertExt }

procedure TClientCertExt.Assign(ASource: TPersistent);
begin
  if ASource is TClientCert then
  begin
    FCert := TClientCert(ASource).FCert;
    if ASource is TClientCertExt then
    begin
      FileName := TClientCertExt(ASource).FileName;
      Stream := TClientCertExt(ASource).Stream;
      Password := TClientCertExt(ASource).Password;
    end;
  end
  else
    inherited Assign(ASource);
end;

{ THTTPReqResp }

constructor THTTPReqResp.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FHTTP := THTTPClient.Create;
  FHTTP.PreemptiveAuthentication := True;
  FHTTP.UseDefaultCredentials := True;
  FHTTP.RedirectsWithGET := [];

  InvokeOptions := [soIgnoreInvalidCerts, soAutoCheckAccessPointViaUDDI];
  Agent := SOAP_AGENT;
  { Default this to true to allow Clients to send International Characters without having to
    explicit set this.
    NOTE: This is a change from previous versions but it seems better based on the number of
          reports whose ultimate solution is related to not having enabled this property
          The property still specifies the default as False as we cannot break interfaces for
          this release. We'll reconsider the 'default' in a subsequent release. }
  UseUTF8InHeader := True;

  FClientCertificate := TClientCertExt.Create(Self);
  FClientCertificate.Name := 'ClientCert1'; {Do not localize }
  FClientCertificate.SetSubComponent(True);
end;

destructor THTTPReqResp.Destroy;
begin
  WSDLView := nil;
  FreeAndNil(FClientCertificate);
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

{$IFNDEF AUTOREFCOUNT}
class function THTTPReqResp.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  THTTPReqResp(Result).FRefCount := 1;
end;
{$ENDIF !AUTOREFCOUNT}

{ $IFNDEF AUTOREFCOUNT}
procedure THTTPReqResp.AfterConstruction;
begin
  inherited;
  FOwnerIsComponent := Assigned(Owner) and (Owner is TComponent);
{$IFNDEF AUTOREFCOUNT}
  TInterlocked.Decrement(FRefCount);
{$ENDIF !AUTOREFCOUNT}
end;
{ $ENDIF !AUTOREFCOUNT}

{ IInterface }

{$IFNDEF AUTOREFCOUNT}
function THTTPReqResp._AddRef: Integer;
begin
  Result := TInterlocked.Increment(FRefCount)
end;

function THTTPReqResp._Release: Integer;
begin
  Result := TInterlocked.Decrement(FRefCount);
  { If we are not being used as a TComponent, then use refcount to manage our
    lifetime as with TInterfacedObject. }
  if (Result = 0) and not FOwnerIsComponent then
    Destroy;
end;
{$ENDIF !AUTOREFCOUNT}

function THTTPReqResp.GetHTTPReqResp: THTTPReqResp;
begin
  Result := Self;
end;

function THTTPReqResp.GetSOAPAction: string;
begin
  if (FSoapAction = '') and not (soNoValueForEmptySOAPAction in InvokeOptions) then
    Result := '""'
  else
    Result := FSoapAction;
end;

procedure THTTPReqResp.SetSOAPAction(const SOAPAction: string);
begin
  FSoapAction := SOAPAction;
end;

function THTTPReqResp.IsSOAPActionStored: Boolean;
begin
  Result := FSoapAction <> '';
end;

procedure THTTPReqResp.SetWSDLView(const WSDLVIew: TWSDLView);
begin
  if FWSDLView <> WSDLView then
  begin
    if Assigned(FWSDLView) and Assigned(FWSDLView.WSDL) then
      FWSDLView.WSDL.StreamLoaderCustomizer := nil;
    FWSDLView := WSDLView;
    if Assigned(FWSDLView) then
    begin
      FWSDLView.UserName := Username;
      FWSDLView.Password := Password;
      FWSDLView.Proxy := Proxy;
      if Assigned(FWSDLView.WSDL) then
        FWSDLView.WSDL.StreamLoaderCustomizer := Self;
    end;
  end;
end;

procedure THTTPReqResp.SetURL(const Value: string);
begin
  FUserSetURL := Value <> '';
  FURL := Value;
end;

procedure THTTPReqResp.SetMimeBoundary(const Value: string);
begin
  FMimeBoundary := Value;
end;

function THTTPReqResp.GetMimeBoundary: string;
begin
  Result := FMimeBoundary;
end;

function THTTPReqResp.GetWebNodeOptions: WebNodeOptions;
begin
  Result := FWebNodeOptions;
end;

procedure THTTPReqResp.SetWebNodeOptions(Value: WebNodeOptions);
begin
  FWebNodeOptions := Value;
end;

procedure THTTPReqResp.SetUsername(const NameValue: string);
begin
  FUserName := NameValue;
  UseDefaultCredentials := FUserName.IsEmpty and FPassword.IsEmpty;
  if Assigned(WSDLView) then
    WSDLView.UserName := NameValue;
end;

procedure THTTPReqResp.SetPassword(const PasswordValue: string);
begin
  FPassword := PasswordValue;
  UseDefaultCredentials := FUserName.IsEmpty and FPassword.IsEmpty;
  if Assigned(WSDLView) then
    WSDLView.Password := PasswordValue;
end;

procedure THTTPReqResp.SetProxy(const ProxyValue: string);
begin
  FProxy := ProxyValue;
  if Assigned(WSDLView) then
    WSDLView.Proxy := ProxyValue;
end;

function THTTPReqResp.GetSOAPActionHeader: string;
begin
  if (SoapAction = '') then
    Result := SHTTPSoapAction + ':'
  else if (SoapAction = '""') then
    Result := SHTTPSoapAction + ': ""'
  else
    Result := SHTTPSoapAction + ': ' + '"' + SoapAction + '"';
end;

function THTTPReqResp.GetReceiveTimeout: Integer;
begin
  Result := FHTTP.ResponseTimeout;
end;

procedure THTTPReqResp.SetReceiveTimeout(const Value: Integer);
begin
  FHTTP.ResponseTimeout := Value;
end;

function THTTPReqResp.GetConnectTimeout: Integer;
begin
  Result := FHTTP.ConnectionTimeout;
end;

procedure THTTPReqResp.SetConnectTimeout(const Value: Integer);
begin
  FHTTP.ConnectionTimeout := Value;
end;

function THTTPReqResp.GetSendTimeout: Integer;
begin
  Result := FHTTP.SendTimeout;
end;

procedure THTTPReqResp.SetSendTimeout(const Value: Integer);
begin
  FHTTP.SendTimeout := Value;
end;

function THTTPReqResp.GetAgent: string;
begin
  Result := FHTTP.UserAgent;
end;

procedure THTTPReqResp.SetAgent(const Value: string);
begin
  FHTTP.UserAgent := Value;
end;

function THTTPReqResp.GetAgentIsStored: Boolean;
begin
  Result := Agent <> SOAP_AGENT;
end;

function THTTPReqResp.GetHTTP: THTTPClient;
begin
  Result := FHTTP;
end;

function THTTPReqResp.GetSecureProtocols: THTTPSecureProtocols;
begin
  Result := FHTTP.SecureProtocols;
end;

procedure THTTPReqResp.SetSecureProtocols(const AValue: THTTPSecureProtocols);
begin
  FHTTP.SecureProtocols := AValue;
end;

function THTTPReqResp.GetPreemptiveAuthentication: Boolean;
begin
  Result := FHTTP.PreemptiveAuthentication;
end;

procedure THTTPReqResp.SetPreemptiveAuthentication(const AValue: Boolean);
begin
  FHTTP.PreemptiveAuthentication := AValue;
end;

function THTTPReqResp.GetAutomaticDecompression: THTTPCompressionMethods;
begin
  Result := FHTTP.AutomaticDecompression;
end;

procedure THTTPReqResp.SetAutomaticDecompression(const AValue: THTTPCompressionMethods);
begin
  FHTTP.AutomaticDecompression := AValue;
end;

function THTTPReqResp.GetUseDefaultCredentials: Boolean;
begin
  Result := FHTTP.UseDefaultCredentials;
end;

procedure THTTPReqResp.SetUseDefaultCredentials(const AValue: Boolean);
begin
  FHTTP.UseDefaultCredentials := AValue;
end;

function THTTPReqResp.GetContentType: string;
begin
  Result := '';
  if not (wnoSOAP12 in WebNodeOptions) then
  begin
    if UseUTF8InHeader then
      Result := ContentTypeUTF8
    else
      Result := ContentTypeNoUTF8;
  end
  else
  begin
    if UseUTF8InHeader then
      Result := Format(ContentTypeWithActionNoLabelFmt, [ContentType12UTF8, GetSOAPAction])
    else
      Result := Format(ContentTypeWithActionNoLabelFmt, [ContentType12NoUTF8, GetSOAPAction]);
  end;
end;

procedure THTTPReqResp.DoValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
begin
  if not Accepted and (soIgnoreInvalidCerts in InvokeOptions) then
    Accepted := True;
end;

procedure THTTPReqResp.DoNeedClientCertificate(const Sender: TObject; const ARequest: TURLRequest;
  const ACertificateList: TCertificateList; var AnIndex: Integer);
var
  Cert: TCertificate;
  i: Integer;

  procedure UISelectClientCertificate;
{$IFDEF MSWINDOWS}
  var
    LCert: TCertificate;
    I: Integer;
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    if not IsConsole then
      if ShowSelectCertificateDialog(GetForegroundWindow(), '', '', LCert) then
        for I := 0 to ACertificateList.Count - 1 do
          if LCert.SerialNum = ACertificateList[I].SerialNum then
          begin
            AnIndex := I;
            Exit;
          end;
{$ENDIF}
  end;

begin
  if Assigned(OnNeedClientCertificate) then
    OnNeedClientCertificate(Sender, ARequest, ACertificateList, AnIndex)
  else if (ClientCertificate.Subject <> '') or (ClientCertificate.Issuer <> '') or
     (ClientCertificate.ProtocolName <> '') or (ClientCertificate.SerialNum <> '') or
     (ClientCertificate.CertName <> '') then
  begin
    for i := 0 to ACertificateList.Count - 1 do
    begin
      Cert := ACertificateList[i];
      if ((ClientCertificate.Subject = '') or (ClientCertificate.Subject = Cert.Subject)) and
         ((ClientCertificate.Issuer = '') or (ClientCertificate.Issuer = Cert.Issuer)) and
         ((ClientCertificate.ProtocolName = '') or (ClientCertificate.ProtocolName = Cert.ProtocolName)) and
         ((ClientCertificate.SerialNum = '') or (ClientCertificate.SerialNum = Cert.SerialNum)) and
         ((ClientCertificate.CertName = '') or (ClientCertificate.CertName = Cert.CertName)) then
      begin
        AnIndex := i;
        Break;
      end;
    end;
    if AnIndex < 0 then
      UISelectClientCertificate;
  end
  else if (soPickFirstClientCertificate in InvokeOptions) and (ACertificateList.Count > 0) then
    AnIndex := 0
  else
    UISelectClientCertificate;
end;

procedure THTTPReqResp.DoSendData(const Sender: TObject; AContentLength, AWriteCount: Int64; var AAbort: Boolean);
begin
  if Assigned(OnPostingData) then
    OnPostingData(AWriteCount, AContentLength);
end;

procedure THTTPReqResp.DoReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  if Assigned(OnReceivingData) then
    OnReceivingData(AReadCount, AContentLength);
end;

procedure THTTPReqResp.SetupHttp(Http: THttpClient);
var
  LProxy: TProxySettings;
  LCredential: TCredentialsStorage.TCredential;
begin
  if (Proxy <> '') or (UserName <> '') or (Password <> '') then
    FHTTP.CredentialsStorage.ClearCredentials;

  { Proxy support configuration }
  if Proxy <> '' then
  begin
    LProxy := TProxySettings.Create(Proxy);
    if (LProxy.UserName = '') and (LProxy.Password = '') then
    begin
      { If name/password is used in conjunction with proxy, it's passed
        along for proxy authentication }
      LProxy.UserName := UserName;
      LProxy.Password := Password;
    end;
    FHTTP.ProxySettings := LProxy;
  end;

  { no proxy with Username/Password implies basic authentication }
  if (Proxy = '') and ((UserName <> '') or (Password <> '')) then
  begin
    LCredential := TCredentialsStorage.TCredential.Create(TAuthTargetType.Server,
      '', URL, UserName, Password);
    FHTTP.CredentialsStorage.AddCredential(LCredential);
  end;

  FHTTP.OnValidateServerCertificate := DoValidateServerCertificate;
  FHTTP.OnNeedClientCertificate := DoNeedClientCertificate;
  FHTTP.OnSendData := DoSendData;
  FHTTP.OnReceiveData := DoReceiveData;
end;

                                                                                   
procedure SetupHttpReq(Self: THTTPReqResp; AHttpRequest: IHTTPRequest);
begin
  with Self do
  begin
    if TClientCertExt(ClientCertificate).FileName <> '' then
      AHttpRequest.SetClientCertificate(
        TClientCertExt(ClientCertificate).FileName,
        TClientCertExt(ClientCertificate).Password)
    else if TClientCertExt(ClientCertificate).Stream <> nil then
      AHttpRequest.SetClientCertificate(
        TClientCertExt(ClientCertificate).Stream,
        TClientCertExt(ClientCertificate).Password);
  end;
end;

function THTTPReqResp.CheckResponseError(const AHTTPResponse: IHTTPResponse; AFailOn500: Boolean): TSOAPHttpErrorAction;
var
  LExc: ESOAPHTTPException;
  LStatus: Integer;
begin
  Result := heaSuccess;
  LStatus := AHTTPResponse.StatusCode;
  if (LStatus >= 300) and ((LStatus <> 500) or (LStatus = 500) and AFailOn500) then
  begin
    Result := heaError;
    LExc := ESOAPHTTPException.Create(AHTTPResponse.StatusCode, AHTTPResponse.StatusText, URL);
    if Assigned(FOnHttpError) then
      FOnHttpError(Self, AHTTPResponse, LExc, Result);
    case Result of
      heaSuccess,
      heaRetry:
        LExc.Free;
      heaError:
        raise LExc;
      heaAbort:
        begin
          LExc.Free;
          System.SysUtils.Abort;
        end;
    end;
  end;
end;

procedure THTTPReqResp.Get(Resp: TStream);
var
  HTTPRequest: IHTTPRequest;
  HTTPResponse: IHTTPResponse;
begin
  { GETs require a URL }
  if URL = '' then
    raise ESOAPHTTPException.Create(SEmptyURL);

  SetupHttp(FHTTP);

  FHTTP.Accept := '*/*';
  FHTTP.ContentType := sTextXml;
  Resp.Position := 0;

  repeat
    HTTPRequest := FHTTP.GetRequest(sHTTPMethodGet, URL);
    SetupHttpReq(Self, HTTPRequest);
    HTTPResponse := FHTTP.Execute(HTTPRequest, Resp);
  until CheckResponseError(HTTPResponse, True) = heaSuccess;
end;

{ Here the RIO can perform any transports specific setup before call - XML serialization is done }
procedure THTTPReqResp.BeforeExecute(const IntfMD: TIntfMetaData;
                                     const MethMD: TIntfMethEntry;
                                     MethodIndex: Integer;
                                     AttachHandler: IMimeAttachmentHandler);
var
  MethName: InvString;
  Binding: InvString;
  QBinding: IQualifiedName;
  SOAPVersion: TSOAPVersion;
begin
  if FUserSetURL then
  begin
    MethName := InvRegistry.GetMethExternalName(IntfMD.Info, MethMD.Name);
    FSoapAction := InvRegistry.GetActionURIOfInfo(IntfMD.Info, MethName, MethodIndex);
  end
  else
  begin
    { User did *NOT* set a URL }
    if WSDLView <> nil then
    begin
      if ioSOAP12 in InvRegistry.GetIntfInvokeOptions(IntfMD.Info) then
        SOAPVersion := svSOAP12
      else
        SOAPVersion := svSOAP11;

      { Make sure WSDL is active }
      WSDLView.Activate;
      QBinding := WSDLView.WSDL.GetBindingForServicePort(WSDLView.Service, WSDLView.Port);
      if QBinding <> nil then
      begin
        Binding := QBinding.Name;
        MethName:= InvRegistry.GetMethExternalName(WSDLView.IntfInfo, WSDLView.Operation);
                                                                                                    
        FSoapAction := WSDLView.WSDL.GetSoapAction(Binding, MethName, 0, SOAPVersion);
      end;

      {NOTE: In case we can't get the SOAPAction - see if we have something in the registry }
      {      It can't hurt:) }
      if FSoapAction = '' then
        InvRegistry.GetActionURIOfInfo(IntfMD.Info, MethName, MethodIndex);

      { Retrieve URL }
      FURL := WSDLView.WSDL.GetSoapAddressForServicePort(WSDLView.Service, WSDLView.Port, SOAPVersion);
      if URL = '' then
        raise ESOAPHTTPException.CreateFmt(sCantGetURL,
                                           [WSDLView.Service, WSDLView.Port, WSDLView.WSDL.FileName]);
    end
    else
      raise ESOAPHTTPException.Create(sNoWSDLURL);
  end;

  { Are we sending attachments?? }
  if AttachHandler <> nil then
  begin
    FBindingType := btMIME;
    { If yes, ask MIME handler what MIME boundary it's using to build the Multipart
      packet }
    FMimeBoundary := AttachHandler.MIMEBoundary;

    { Also customize the MIME packet for transport specific items }
    if UseUTF8InHeader then
      AttachHandler.AddSoapHeader(Format(ContentTypeTemplate, [ContentTypeUTF8]))
    else
      AttachHandler.AddSoapHeader(Format(ContentTypeTemplate, [ContentTypeNoUTF8]));
    AttachHandler.AddSoapHeader(GetSOAPActionHeader);
  end else
    FBindingType := btSOAP;
end;

procedure THTTPReqResp.Execute(const DataMsg: String; Resp: TStream);
var
  Stream: TMemoryStream;
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(DataMsg);
  Stream := TMemoryStream.Create;
  try
    Stream.Size := Length(Bytes);
    Stream.Write(Bytes, 0, Length(Bytes));
    Execute(Stream, Resp);
  finally
    Stream.Free;
  end;
end;

function THTTPReqResp.Execute(const Request: TStream): TStream;
begin
  Result := TMemoryStream.Create;
  try
    Execute(Request, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure THTTPReqResp.Execute(const Request: TStream; Response: TStream);

  function IsErrorStatusCode(Code: Integer): Boolean;
  begin
    case Code of
      404, 405, 410:
        Result := True;
      else
        Result := False;
    end;
  end;

var
  LookUpUDDI, CanRetry: Boolean;
  AccessPoint: string;
  PrevError: string;
  HTTPRequest: IHTTPRequest;
  HTTPResponse: IHTTPResponse;
  ContType: string;
begin
  LookUpUDDI := False;
  CanRetry := (soAutoCheckAccessPointViaUDDI in InvokeOptions) and
              (Length(UDDIBindingKey) > 0) and
              (Length(UDDIOperator) > 0);
  while (True) do
  begin
    { Look up URL from UDDI?? }
    if LookUpUDDI and CanRetry then
    begin
      try
        CanRetry := False;
        AccessPoint := '';
        AccessPoint := GetBindingkeyAccessPoint(UDDIOperator, UDDIBindingKey);
      except
        { Ignore UDDI lookup error }
      end;
      { If UDDI lookup failed or we got back the same URL we used...
        raise the previous execption message }
      if (AccessPoint = '') or SameText(AccessPoint, URL) then
        raise ESOAPHTTPException.Create(PrevError);
      FURL := AccessPoint;
    end;

    HTTPResponse := nil;
    try
      SetupHttp(FHTTP);

      { Setup packet based on Content-Type/Binding }
      if FBindingType = btMIME then
      begin
        FHTTP.ContentType := Format(ContentHeaderMIME, [FMimeBoundary]);
        FHTTP.CustomHeaders[MimeVersionName] := MimeVersionValue;
      end else { Assume btSOAP }
        FHTTP.ContentType := GetContentType;

      { Action header }
      if (FBindingType = btMIME) or
         (not (soNoSOAPActionHeader in FInvokeOptions) and not (wnoSOAP12 in GetWebNodeOptions)) then
      begin
        { NOTE: It's not really clear whether this should be sent in the case
                of MIME Binding. Investigate interoperability ?? }
        FHTTP.CustomHeaders[SHTTPSoapAction] := SoapAction;
      end;

      FHTTP.Accept := '*/*';
      Request.Position := 0;

      if Assigned(OnBeforePost) then
        OnBeforePost(Self, FHTTP);

      repeat
        HTTPRequest := FHTTP.GetRequest(sHTTPMethodPost, URL);
        HTTPRequest.SourceStream := Request;
        SetupHttpReq(Self, HTTPRequest);
        HTTPResponse := FHTTP.Execute(HTTPRequest, Response);
      until CheckResponseError(HTTPResponse, False) = heaSuccess;

      if Response.Size = 0 then
        raise ESOAPHTTPException.Create(SInvalidHTTPResponse);

      ContType := HTTPResponse.MimeType;
      FMimeBoundary := GetMimeBoundaryFromType(ContType);
      { NOTE: Content-Types are case insensitive! }
      {       And here we're not validating that we
              have a valid content-type; rather
              we're checking for some common invalid
              ones }
      if SameText(ContType, ContentTypeTextPlain) or
         SameText(ContType, STextHtml) then
        raise ESOAPHTTPException.CreateFmt(SInvalidContentType, [ContType]);

      Exit;
    except
      on Ex: ESOAPHTTPException do
      begin
        if not CanRetry or not IsErrorStatusCode(HTTPResponse.StatusCode) then
          raise;
        { Trigger UDDI Lookup }
        LookUpUDDI := True;
        PrevError := Ex.Message;
      end;
    end;
  end;
end;

procedure THTTPReqResp.Customize(const ATransport: IInterface);
var
  LIntf: IHTTPReqResp;
  LReqResp: THTTPReqResp;
begin
  LReqResp := nil;
  if Supports(ATransport, IHTTPReqResp, LIntf) then
    LReqResp := LIntf.GetHTTPReqResp;
  if LReqResp = nil then
    Exit;

  LReqResp.Agent := Agent;
  LReqResp.UserName := UserName;
  LReqResp.Password := Password;
  LReqResp.Proxy := Proxy;
  LReqResp.ClientCertificate.Assign(ClientCertificate);
  LReqResp.InvokeOptions := InvokeOptions;
  LReqResp.ConnectTimeout := ConnectTimeout;
  LReqResp.SendTimeout := SendTimeout;
  LReqResp.ReceiveTimeout := ReceiveTimeout;
  LReqResp.SecureProtocols := SecureProtocols;
  LReqResp.AutomaticDecompression := AutomaticDecompression;
  LReqResp.PreemptiveAuthentication := PreemptiveAuthentication;
  LReqResp.UseDefaultCredentials := UseDefaultCredentials;
  LReqResp.OnPostingData := OnPostingData;
  LReqResp.OnReceivingData := OnReceivingData;
  LReqResp.OnNeedClientCertificate := OnNeedClientCertificate;
  LReqResp.OnHttpError := OnHttpError;
end;

end.
