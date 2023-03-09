{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2014-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Net.HttpClient.Mac;

interface

var
  GCacheSession: Boolean = True;

implementation

uses
  System.Classes, System.Generics.Collections, System.SysUtils, System.Net.URLClient, System.NetConsts,
{$IF defined(IOS)}
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.Security,
{$ELSE}
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.Security,
{$ENDIF !IOS}
  Macapi.ObjCRuntime, Macapi.Helpers, Macapi.ObjectiveC, Macapi.CoreFoundation, System.Net.HttpClient,
  System.DateUtils, System.NetEncoding, System.Types, System.SyncObjs;

type
  TMacCertList = TList<SecCertificateRef>;
  TMacIdentityList = TList<SecIdentityRef>;
  TMacHTTPRequest = class;
  TMacHTTPResponse = class;
  TMacConnectionDataDelegate = class;

                                                                                                                    
  NSURLSessionTaskDelegate = interface(IObjectiveC)
    ['{C48E0AED-64F3-45A4-8D42-E3DB12F668E7}']
    [MethodName('URLSession:task:willPerformHTTPRedirection:newRequest:completionHandler:')]
    procedure URLSessionTaskWillPerformHTTPRedirectionNewRequestCompletionHandler(session: NSURLSession;
      task: NSURLSessionTask; willPerformHTTPRedirection: NSHTTPURLResponse; newRequest: NSURLRequest;
      completionHandler: Pointer {TFoundationCompletionHandler7}); cdecl;
    [MethodName('URLSession:task:didReceiveChallenge:completionHandler:')]
    procedure URLSessionTaskDidReceiveChallengeCompletionHandler(session: NSURLSession; task: NSURLSessionTask;
      didReceiveChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer {TFoundationCompletionHandler6}); cdecl;
    [MethodName('URLSession:task:needNewBodyStream:')]
    procedure URLSessionTaskNeedNewBodyStream(session: NSURLSession; task: NSURLSessionTask;
      needNewBodyStream: TFoundationNeedNewBodyStream); cdecl;
    [MethodName('URLSession:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend:')]
    procedure URLSessionTaskDidSendBodyDataTotalBytesSentTotalBytesExpectedToSend(session: NSURLSession;
      task: NSURLSessionTask; didSendBodyData: Int64; totalBytesSent: Int64; totalBytesExpectedToSend: Int64); cdecl;
    [MethodName('URLSession:task:didCompleteWithError:')]
    procedure URLSessionTaskDidCompleteWithError(session: NSURLSession; task: NSURLSessionTask;
      didCompleteWithError: NSError); cdecl;
  end;

  NSURLSessionDataDelegate = interface(IObjectiveC)
    ['{9B66A569-04EB-4B0C-9043-BCF97E891CBD}']
    [MethodName('URLSession:dataTask:didReceiveResponse:completionHandler:')]
    procedure URLSessionDataTaskDidReceiveResponseCompletionHandler(session: NSURLSession;
      dataTask: NSURLSessionDataTask; didReceiveResponse: NSURLResponse;
      completionHandler: Pointer {TFoundationCompletionHandler8}); cdecl;
    [MethodName('URLSession:dataTask:didBecomeDownloadTask:')]
    procedure URLSessionDataTaskDidBecomeDownloadTask(session: NSURLSession; dataTask: NSURLSessionDataTask;
      didBecomeDownloadTask: NSURLSessionDownloadTask); cdecl;
    [MethodName('URLSession:dataTask:didReceiveData:')]
    procedure URLSessionDataTaskDidReceiveData(session: NSURLSession; dataTask: NSURLSessionDataTask;
      didReceiveData: NSData); cdecl;
    [MethodName('URLSession:dataTask:willCacheResponse:completionHandler:')]
    procedure URLSessionDataTaskWillCacheResponseCompletionHandler(session: NSURLSession;
      dataTask: NSURLSessionDataTask; willCacheResponse: NSCachedURLResponse;
      completionHandler: Pointer {TFoundationCompletionHandler9}); cdecl;
  end;

  TDataTasks = class(TObject)
  private
    FList: TDictionary<Pointer, TMacHTTPResponse>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(const ATask: NSURLSessionTask; const AResponse: TMacHTTPResponse);
    procedure RemoveTask(const ATask: NSURLSessionTask);
    function FindResponse(const ATask: NSURLSessionTask; var AResponse: TMacHTTPResponse): Boolean;
    procedure HandleException(const ATask: NSURLSessionTask);
  end;

  TMacHTTPClient = class(THTTPClient)
  private
    FDataTasks: TDataTasks;
    FDelegate: TMacConnectionDataDelegate;
    FConfig: NSURLSessionConfiguration;
    FSession: NSURLSession;
    FLastSecureProtocols: THTTPSecureProtocols;
    FLastProxySettings: TProxySettings;
    FLastSendTimeout: Integer;
    FLastResponseTimeout: Integer;
    procedure SetMacProxySettings;
    procedure SetMacSecureProtocols;
    procedure SetMacTimeouts;
    procedure UpdateSession;
    procedure ReleaseSession;
  protected
    function DoExecuteRequest(const ARequest: THTTPRequest; var AResponse: THTTPResponse;
      const AContentStream: TStream): TMacHTTPClient.TExecutionResult; override;
    function DoSetCredential(AnAuthTargetType: TAuthTargetType; const ARequest: THTTPRequest;
      const ACredential: TCredentialsStorage.TCredential): Boolean; override;
    function DoGetHTTPRequestInstance(const AClient: THTTPClient; const ARequestMethod: string;
      const AURI: TURI): IHTTPRequest; override;
    function DoProcessStatus(const ARequest: IHTTPRequest; const  AResponse: IHTTPResponse): Boolean; override;
    function DoGetSSLCertificateFromServer(const ARequest: THTTPRequest): TCertificate; override;
    procedure DoServerCertificateAccepted(const ARequest: THTTPRequest); override;
    procedure DoGetClientCertificates(const ARequest: THTTPRequest; const ACertificateList: TList<TCertificate>); override;
    function DoClientCertificateAccepted(const ARequest: THTTPRequest; const AnIndex: Integer): Boolean; override;
    class function CreateInstance: TURLClient; override;

    function DoGetResponseInstance(const AContext: TObject; const AProc: TProc;
      const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent;
      const ARequest: IURLRequest; const AContentStream: TStream): IAsyncResult; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMacConnectionDataDelegate = class(TOCLocal, NSURLSessionTaskDelegate, NSURLSessionDataDelegate)
  private
    [Weak] FDataTasks: TDataTasks;
    [Weak] FMacHTTPClient: TMacHTTPClient;
    procedure ExtractCertificateInfo(ACertificate: SecCertificateRef; var CertInfo: TCertificate);
{$If defined(IOS)}
    /// <summary>Import a client certificate from an stream that contains raw certificate data</summary>
    function ImportP12Certificate(var AIdentity: SecIdentityRef; var ACertificates: NSArray;
      const ARequest: TMacHTTPRequest): Boolean;
{$ELSE !IOS}
    /// <summary>Obtain from the keychain the certificates that match the issuer</summary>
    procedure FilterCertificates(const AIssuers: NSArray; const AMacCertList: TMacCertList;
      const ACertList: TCertificateList);
    /// <summary>For client certificate authentication challenge we need the certificate and the identity</summary>
    procedure GetKeychainIdentities(const AIdentityList: TMacIdentityList);
    /// <summary>Get the identity of the provied certificate. This is needed to create the
    /// NSURLCredential instance to reply to a client certificate authentication challenge</summary>
    function FindIdentityForCertificate(const AIdentityList: TMacIdentityList; ACertificate: SecCertificateRef): SecIdentityRef;
{$ENDIF}
  public
    constructor Create(const ADataTasks: TDataTasks; const AClient: TMacHTTPClient);

    // NSURLSessionTaskDelegate
    [MethodName('URLSession:task:didCompleteWithError:')]
    procedure URLSessionTaskDidCompleteWithError(session: NSURLSession; task: NSURLSessionTask;
      didCompleteWithError: NSError); cdecl;
    [MethodName('URLSession:task:didReceiveChallenge:completionHandler:')]
    procedure URLSessionTaskDidReceiveChallengeCompletionHandler(session: NSURLSession; task: NSURLSessionTask;
      didReceiveChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer {TFoundationCompletionHandler6}); cdecl;
    [MethodName('URLSession:task:willPerformHTTPRedirection:newRequest:completionHandler:')]
    procedure URLSessionTaskWillPerformHTTPRedirectionNewRequestCompletionHandler(session: NSURLSession;
      task: NSURLSessionTask; willPerformHTTPRedirection: NSHTTPURLResponse; newRequest: NSURLRequest;
      completionHandler: Pointer {TFoundationCompletionHandler7}); cdecl;
    [MethodName('URLSession:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend:')]
    procedure URLSessionTaskDidSendBodyDataTotalBytesSentTotalBytesExpectedToSend(session: NSURLSession;
      task: NSURLSessionTask; didSendBodyData: Int64; totalBytesSent: Int64; totalBytesExpectedToSend: Int64); cdecl;
    [MethodName('URLSession:task:needNewBodyStream:')]
    procedure URLSessionTaskNeedNewBodyStream(session: NSURLSession; task: NSURLSessionTask;
      needNewBodyStream: TFoundationNeedNewBodyStream); cdecl;

    // NSURLSessionDataDelegate
    [MethodName('URLSession:dataTask:didReceiveResponse:completionHandler:')]
    procedure URLSessionDataTaskDidReceiveResponseCompletionHandler(session: NSURLSession;
      dataTask: NSURLSessionDataTask; didReceiveResponse: NSURLResponse;
      completionHandler: Pointer {TFoundationCompletionHandler8}); cdecl;
    [MethodName('URLSession:dataTask:didReceiveData:')]
    procedure URLSessionDataTaskDidReceiveData(session: NSURLSession; dataTask: NSURLSessionDataTask;
      didReceiveData: NSData); cdecl;
    [MethodName('URLSession:dataTask:willCacheResponse:completionHandler:')]
    procedure URLSessionDataTaskWillCacheResponseCompletionHandler(session: NSURLSession;
      dataTask: NSURLSessionDataTask; willCacheResponse: NSCachedURLResponse;
      completionHandler: Pointer {TFoundationCompletionHandler9}); cdecl;
    [MethodName('URLSession:dataTask:didBecomeDownloadTask:')]
    procedure URLSessionDataTaskDidBecomeDownloadTask(session: NSURLSession; dataTask: NSURLSessionDataTask;
      didBecomeDownloadTask: NSURLSessionDownloadTask); cdecl;
  end;

  TMacHTTPRequest = class(THTTPRequest)
  private
    FRequest: NSMutableURLRequest;
    FDataTask: NSURLSessionDataTask;
    FRedirects: Integer;
    FProxyCredential: TCredentialsStorage.TCredential;
    FServerCredential: TCredentialsStorage.TCredential;
    FProxyCredentialUsed: Boolean;
    FServerCredentialUsed: Boolean;
    procedure SetMacDecompression;
    procedure SetMacRequestParams;
  protected
    procedure DoPrepare; override;
    procedure DoCancel; override;

    function GetHeaders: TNetHeaders; override;

    procedure AddHeader(const AName, AValue: string); override;
    function RemoveHeader(const AName: string): Boolean; override;

    function GetHeaderValue(const AName: string): string; override;
    procedure SetHeaderValue(const AName, Value: string); override;

    /// <summary> Setter for the ConnectionTimeout property.</summary>
    procedure SetConnectionTimeout(const Value: Integer); override;
    /// <summary> Setter for the ResponseTimeout property.</summary>
    procedure SetResponseTimeout(const Value: Integer); override;
  public
    constructor Create(const AClient: THTTPClient; const ARequestMethod: string; const AURI: TURI);
    destructor Destroy; override;
    function AddRedirect: Integer; inline;
    property Redirects: Integer read FRedirects;
  end;

  TMacHTTPResponse = class(THTTPResponse)
  private
    [Weak] FRequest: TMacHTTPRequest;
    FDone: Boolean;
    FError: NSError;
    FException: Exception;
    FResponse: NSHTTPURLResponse;
    FReaded: Int64;
    function GetIsCancelled: Boolean;
    procedure FinishRequest(const AError: NSError);
  protected
    procedure DoReadData(const AStream: TStream); override;

    function GetHeaders: TNetHeaders; override;
    function GetStatusCode: Integer; override;
    function GetStatusText: string; override;
    function GetVersion: THTTPProtocolVersion; override;
  public
    procedure CreateNativeData(const ANativeResponse: NSURLResponse);
    constructor Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback; const AAsyncCallbackEvent: TAsyncCallbackEvent;
      const ARequest: TMacHTTPRequest; const AContentStream: TStream);
    destructor Destroy; override;
  end;

type
  TSecTrustEvaluateWithErrorProc = function (trust: SecTrustRef; var error: CFErrorRef): Boolean; cdecl;
  TSecTrustEvaluateProc = function (trust: SecTrustRef; var result: uint32): OSStatus; cdecl;
  TSecCertificateCopyCommonNameProc = function (certificate: SecCertificateRef; var commonName: CFStringRef): OSStatus; cdecl;
  TSecCertificateCopySerialNumberDataProc = function (certificate: SecCertificateRef; error: CFErrorRef): CFDataRef; cdecl;

var
  SecTrustEvaluateWithErrorProc: TSecTrustEvaluateWithErrorProc = nil;
  SecTrustEvaluateProc: TSecTrustEvaluateProc = nil;
  SecCertificateCopyCommonNameProc: TSecCertificateCopyCommonNameProc = nil;
  SecCertificateCopySerialNumberDataProc: TSecCertificateCopySerialNumberDataProc = nil;

                                                                              
const
  NetworkLib = '/System/Library/Frameworks/Network.framework/Network';

  kSSLProtocol2 = 1;
  kSSLProtocol3 = 2;
  kTLSProtocol1 = 4;
  kTLSProtocolAll = 6;
  kTLSProtocol11 = 7;
  kTLSProtocol12 = 8;
  kTLSProtocol13 = 10;

function kCFNetworkProxiesHTTPEnable: NSString;
begin
  Result := CocoaNSStringConst(NetworkLib, 'kCFNetworkProxiesHTTPEnable');
end;

function kCFNetworkProxiesHTTPProxy: NSString;
begin
  Result := CocoaNSStringConst(NetworkLib, 'kCFNetworkProxiesHTTPProxy');
end;

function kCFNetworkProxiesHTTPPort: NSString;
begin
  Result := CocoaNSStringConst(NetworkLib, 'kCFNetworkProxiesHTTPPort');
end;

function kCFNetworkProxiesHTTPSEnable: NSString;
begin
  Result := CocoaNSStringConst(NetworkLib, 'kCFNetworkProxiesHTTPSEnable');
end;

function kCFNetworkProxiesHTTPSProxy: NSString;
begin
  Result := CocoaNSStringConst(NetworkLib, 'kCFNetworkProxiesHTTPSProxy');
end;

function kCFNetworkProxiesHTTPSPort: NSString;
begin
  Result := CocoaNSStringConst(NetworkLib, 'kCFNetworkProxiesHTTPSPort');
end;

procedure InitSecProcs;
var
  LhSec: HMODULE;
begin
  LhSec := LoadLibrary(libSecurity);
  if LhSec <> 0 then
    try
      SecTrustEvaluateWithErrorProc := GetProcAddress(LhSec, 'SecTrustEvaluateWithError');
      SecTrustEvaluateProc := GetProcAddress(LhSec, 'SecTrustEvaluate');
      SecCertificateCopyCommonNameProc := GetProcAddress(LhSec, 'SecCertificateCopyCommonName');
      SecCertificateCopySerialNumberDataProc := GetProcAddress(LhSec, 'SecCertificateCopySerialNumberData');
    finally
      FreeLibrary(LhSec);
    end;
end;

procedure InternalWaitMessage(AInterval: Single);
var
  TimeoutDate: NSDate;
begin
  TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(AInterval));
  TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop).runMode(NSDefaultRunLoopMode, TimeoutDate);
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    Sleep(Trunc(AInterval * 1000));
end;

{ TMacHTTPClient }

constructor TMacHTTPClient.Create;
begin
  inherited Initializer;
  FDataTasks := TDataTasks.Create;
  FDelegate := TMacConnectionDataDelegate.Create(FDataTasks, Self);
end;

destructor TMacHTTPClient.Destroy;
begin
  ReleaseSession;
  FDataTasks.Free;
  FDelegate.Free;
  inherited;
end;

class function TMacHTTPClient.CreateInstance: TURLClient;
begin
  Result := TMacHTTPClient.Create;
end;

function TMacHTTPClient.DoClientCertificateAccepted(const ARequest: THTTPRequest; const AnIndex: Integer): Boolean;
begin
  inherited;
  Result := True;
end;

procedure TMacHTTPClient.SetMacProxySettings;
var
  LDict: NSMutableDictionary;
begin
  if ProxySettings.Host <> '' then
  begin
    LDict := TNSMutableDictionary.Create;
    if (ProxySettings.Scheme = '') or SameText(ProxySettings.Scheme, TURI.SCHEME_HTTP) then
    begin
      LDict.setValue(TNSNumber.OCClass.numberWithInt(1), kCFNetworkProxiesHTTPEnable);
      LDict.setValue(StringToID(ProxySettings.Host), kCFNetworkProxiesHTTPProxy);
      LDict.setValue(TNSNumber.OCClass.numberWithInt(ProxySettings.Port), kCFNetworkProxiesHTTPPort);
    end;
    if (ProxySettings.Scheme = '') or SameText(ProxySettings.Scheme, TURI.SCHEME_HTTPS) then
    begin
      LDict.setValue(TNSNumber.OCClass.numberWithInt(1), kCFNetworkProxiesHTTPSEnable);
      LDict.setValue(StringToID(ProxySettings.Host), kCFNetworkProxiesHTTPSProxy);
      LDict.setValue(TNSNumber.OCClass.numberWithInt(ProxySettings.Port), kCFNetworkProxiesHTTPSPort);
    end;
    FConfig.setConnectionProxyDictionary(LDict);
  end;
  FLastProxySettings := ProxySettings;
end;

procedure TMacHTTPClient.SetMacSecureProtocols;
const
  CSecProts: array [THTTPSecureProtocol] of Integer = (
    kSSLProtocol2, kSSLProtocol3, kTLSProtocol1, kTLSProtocol11,
    kTLSProtocol12, kTLSProtocol13);
var
  LMinProt: Integer;
  LMaxProt: Integer;
  LProt: THTTPSecureProtocol;
begin
  if SecureProtocols <> CHTTPDefSecureProtocols then
  begin
    LMinProt := -1;
    LMaxProt := -1;
    for LProt := Low(THTTPSecureProtocol) to High(THTTPSecureProtocol) do
      if LProt in SecureProtocols then
      begin
        if LMinProt = -1 then
          LMinProt := CSecProts[LProt];
        LMaxProt := CSecProts[LProt];
      end;
    if LMinProt <> -1 then
      FConfig.setTLSMinimumSupportedProtocol(Cardinal(LMinProt));
    if LMaxProt <> -1 then
      FConfig.setTLSMaximumSupportedProtocol(Cardinal(LMaxProt));
  end;
  FLastSecureProtocols := SecureProtocols;
end;

procedure TMacHTTPClient.SetMacTimeouts;
var
  LReqTimeout,
  LResTimeout: NSTimeInterval;
begin
  // The timer of timeoutIntervalForRequest is reset whenever data arrives,
  // no matter how much data (just a single byte arriving would reset it),
  // it only causes a timeout if absolutely no data arrives for this interval.
  // The timer for timeoutIntervalForResource is never reset, it produces a timeout
  // unless the task fully completed as that will stop that timer from firing.
  FLastSendTimeout := SendTimeout;
  FLastResponseTimeout := ResponseTimeout;
  if SendTimeout <> 0 then
  begin
    if SendTimeout < 0 then
      LReqTimeout := SecsPerDay * 365
    else
      LReqTimeout := SendTimeout / 1000;
    FConfig.setTimeoutIntervalForRequest(LReqTimeout);
  end;
  if ResponseTimeout <> 0 then
  begin
    if ResponseTimeout < 0 then
      LResTimeout := SecsPerDay * 365
    else
      LResTimeout := ResponseTimeout / 1000;
    FConfig.setTimeoutIntervalForResource(LResTimeout);
  end;
end;

procedure TMacHTTPClient.ReleaseSession;
begin
  if FSession <> nil then
  begin
    FSession.release;
    FSession := nil;
  end;
  if FConfig <> nil then
  begin
    FConfig.release;
    FConfig := nil;
  end;
end;

procedure TMacHTTPClient.UpdateSession;
begin
  TMonitor.Enter(Self);
  try
    if not GCacheSession or
       (FSession = nil) or
       (FLastSecureProtocols <> SecureProtocols) or
       (FLastProxySettings.Host <> ProxySettings.Host) or (FLastProxySettings.Port <> ProxySettings.Port) or
         (FLastProxySettings.Scheme <> ProxySettings.Scheme) or (FLastProxySettings.UserName <> ProxySettings.UserName) or
         (FLastProxySettings.Password <> ProxySettings.Password) or
       (FLastSendTimeout <> SendTimeout) or (FLastResponseTimeout <> ResponseTimeout) then
    begin
      ReleaseSession;

      FConfig := TNSURLSessionConfiguration.OCClass.ephemeralSessionConfiguration;
      FConfig.retain;
      SetMacProxySettings;
      SetMacSecureProtocols;
      SetMacTimeouts;

      FSession := TNSURLSession.OCClass.sessionWithConfigurationDelegateDelegateQueue(
        FConfig, FDelegate.GetObjectID, nil);
      FSession.retain;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TMacHTTPClient.DoExecuteRequest(const ARequest: THTTPRequest; var AResponse: THTTPResponse;
  const AContentStream: TStream): TMacHTTPClient.TExecutionResult;
var
  LRequest: TMacHTTPRequest;
  LResponse: TMacHTTPResponse;
  LExc: Exception;
begin
  Result := TExecutionResult.Success;
  LRequest := TMacHTTPRequest(ARequest);
  if LRequest.FCancelled then
    Exit;

  LRequest.FDataTask := FSession.dataTaskWithRequest(LRequest.FRequest);
  LRequest.FDataTask.retain;

  LResponse := TMacHTTPResponse(AResponse);
  LResponse.FDone := False;
  FDataTasks.AddTask(LRequest.FDataTask, LResponse);
  try
    LRequest.FDataTask.resume;
    while not LResponse.FDone do
      InternalWaitMessage(0.01);

    if LResponse.FException <> nil then
    begin
      LExc := LResponse.FException;
      LResponse.FException := nil;
      raise LExc;
    end
    else if LResponse.FError <> nil then
      raise ENetHTTPClientException.CreateResFmt(@SNetHttpClientErrorAccessing,
        [LResponse.FError.code, LRequest.FURL.ToString, NSStrToStr(LResponse.FError.localizedDescription)])
    else if THTTPSecureFailureReason.CertNotAccepted in FSecureFailureReasons then
      raise ENetHTTPCertificateException.CreateRes(@SNetHttpServerCertificateNotAccepted);

  finally
    FDataTasks.RemoveTask(LRequest.FDataTask);
    TMonitor.Enter(LRequest);
    try
      LRequest.FDataTask.release;
      LRequest.FDataTask := nil;
    finally
      TMonitor.Exit(LRequest);
    end;
  end;
end;

procedure TMacHTTPClient.DoGetClientCertificates(const ARequest: THTTPRequest;
  const ACertificateList: TList<TCertificate>);
begin
                                                                 
end;

function TMacHTTPClient.DoGetHTTPRequestInstance(const AClient: THTTPClient; const ARequestMethod: string; const AURI: TURI): IHTTPRequest;
begin
  Result := TMacHTTPRequest.Create(AClient, ARequestMethod, AURI);
end;

function TMacHTTPClient.DoGetResponseInstance(const AContext: TObject; const AProc: TProc;
  const AsyncCallback: TAsyncCallback; const AsyncCallbackEvent: TAsyncCallbackEvent; const ARequest: IURLRequest;
  const AContentStream: TStream): IAsyncResult;
begin
  Result := TMacHTTPResponse.Create(AContext, AProc, AsyncCallback, AsyncCallbackEvent, ARequest as TMacHttpRequest, AContentStream);
end;

function TMacHTTPClient.DoGetSSLCertificateFromServer(const ARequest: THTTPRequest): TCertificate;
begin
  Result := Default(TCertificate);
                                                                 
end;

function TMacHTTPClient.DoProcessStatus(const ARequest: IHTTPRequest; const AResponse: IHTTPResponse): Boolean;
begin
  Result := True;
end;

procedure TMacHTTPClient.DoServerCertificateAccepted(const ARequest: THTTPRequest);
begin
                                                                 
end;

function TMacHTTPClient.DoSetCredential(AnAuthTargetType: TAuthTargetType;
  const ARequest: THTTPRequest; const ACredential: TCredentialsStorage.TCredential): Boolean;
begin
  case AnAuthTargetType of
    TAuthTargetType.Proxy:
      begin
        TMacHTTPRequest(ARequest).FProxyCredential := ACredential;
        TMacHTTPRequest(ARequest).FProxyCredentialUsed := False;
      end;
    TAuthTargetType.Server:
      begin
        TMacHTTPRequest(ARequest).FServerCredential := ACredential;
        TMacHTTPRequest(ARequest).FServerCredentialUsed := False;
      end;
  end;
  Result := True;
end;

{ TMacHTTPResponse }

constructor TMacHTTPResponse.Create(const AContext: TObject; const AProc: TProc; const AAsyncCallback: TAsyncCallback; const AAsyncCallbackEvent: TAsyncCallbackEvent;
      const ARequest: TMacHTTPRequest; const AContentStream: TStream);
begin
  inherited Create(AContext, AProc, AAsyncCallback, AAsyncCallbackEvent, ARequest as TMacHTTPRequest, AContentStream);
  FRequest := TMacHTTPRequest(ARequest);
end;

procedure TMacHTTPResponse.CreateNativeData(const ANativeResponse: NSURLResponse);
begin
  if FResponse <> nil then
  begin
    FResponse.release;
    FResponse := nil;
  end;
  SetLength(FHeaders, 0);

  FResponse := TNSHTTPURLResponse.Wrap((ANativeResponse as ILocalObject).GetObjectID);
  FResponse.retain;
end;

function TMacHTTPResponse.GetIsCancelled: Boolean;
begin
  Result := (FRequest <> nil) and FRequest.GetIsCancelled;
end;

destructor TMacHTTPResponse.Destroy;
begin
  FreeAndNil(FException);
  if FError <> nil then
  begin
    FError.release;
    FError := nil;
  end;
  if FResponse <> nil then
  begin
    FResponse.release;
    FResponse := nil;
  end;
  inherited Destroy;
end;

procedure TMacHTTPResponse.DoReadData(const AStream: TStream);
begin
  // Do nothing
end;

procedure TMacHTTPResponse.FinishRequest(const AError: NSError);
begin
  if (AError <> nil) and (AError.code <> 0) then
    if (AError.code = NSURLErrorCancelled) and (FRequest <> nil) then
    begin
      if FException = nil then
        FRequest.FCancelled := True;
    end
    else
    begin
      FError := AError;
      FError.retain;
    end;
  FDone := True;
end;

procedure KeyValueToNetHeaders(const AKeys: NSArray; const AValues: NSArray; var AHeaders: TNetHeaders);
var
  I, J: Integer;
  LName: string;
  LValue: string;
begin
  SetLength(AHeaders, AKeys.count);
  J := 0;
  for I := 0 to NativeInt(AKeys.count) - 1 do
  begin
    LName := NSStrToStr(TNSString.Wrap(AKeys.objectAtIndex(I)));
    LValue := NSStrToStr(TNSString.Wrap(AValues.objectAtIndex(I)));
    if not SameText(LName, sSetCookie) then
    begin
      AHeaders[J].Name := LName;
      AHeaders[J].Value := LValue;
      Inc(J);
    end;
  end;
  SetLength(AHeaders, J);
end;

function TMacHTTPResponse.GetHeaders: TNetHeaders;
var
  LAllCookies: NSArray;
  LNSCookie: NSHTTPCookie;
  LCookie: TCookie;
  I: Integer;
begin
  if GetIsCancelled or (FResponse = nil) then
    Result := nil
  else
  begin
    if Length(FHeaders) = 0 then
    begin
      LAllCookies := TNSHTTPCookie.OCClass.cookiesWithResponseHeaderFields(FResponse.allHeaderFields, FRequest.FRequest.URL);
      if LAllCookies.count > 0 then
        for I := 0 to LAllCookies.count - 1 do
        begin
          LNSCookie := TNSHTTPCookie.Wrap(LAllCookies.objectAtIndex(I));
          LCookie.Name := TNetEncoding.URL.Decode(NSStrToStr(LNSCookie.name), [TURLEncoding.TDecodeOption.PlusAsSpaces]);
          LCookie.Value := TNetEncoding.URL.Decode(NSStrToStr(LNSCookie.value), [TURLEncoding.TDecodeOption.PlusAsSpaces]);
          LCookie.Expires := NSDateToDateTime(LNSCookie.expiresDate);
          LCookie.Domain := NSStrToStr(LNSCookie.domain);
          if (LCookie.Domain <> '') and (LCookie.Domain.Chars[0] <> '.') then
            LCookie.Domain := '.' + LCookie.Domain;
          LCookie.Path := NSStrToStr(LNSCookie.path);
          LCookie.Secure := LNSCookie.isSecure;
          LCookie.HttpOnly := LNSCookie.isHTTPOnly;
          FCookies.Add(LCookie);
        end;

      KeyValueToNetHeaders(FResponse.allHeaderFields.allKeys, FResponse.allHeaderFields.allValues, FHeaders);
    end;
    Result := FHeaders;
  end;
end;

function TMacHTTPResponse.GetStatusCode: Integer;
begin
  if GetIsCancelled or (FResponse = nil) then
    Result := 0
  else
    Result := FResponse.statusCode;
end;

function TMacHTTPResponse.GetStatusText: string;
begin
  if GetIsCancelled or (FResponse = nil) then
    Result := ''
  else if FResponse.statusCode = 200 then
    Result := 'OK' // do not localize
  else
    Result := NSStrToStr(TNSHTTPURLResponse.OCClass.localizedStringForStatusCode(FResponse.statusCode));
end;

function TMacHTTPResponse.GetVersion: THTTPProtocolVersion;
begin
  Result := THTTPProtocolVersion.HTTP_1_1;
end;

{ TMacHTTPRequest }

function TMacHTTPRequest.AddRedirect: Integer;
begin
  Inc(FRedirects);
  Result := FRedirects;
end;

constructor TMacHTTPRequest.Create(const AClient: THTTPClient; const ARequestMethod: string; const AURI: TURI);
begin
  inherited Create(AClient, ARequestMethod, AURI);
  FRequest := TNSMutableURLRequest.Create;
  FRequest.setTimeoutInterval(ResponseTimeout / 1000);
  FRequest.setHTTPShouldHandleCookies(AClient.AllowCookies);
end;

destructor TMacHTTPRequest.Destroy;
begin
  FRequest.release;
  FRequest := nil;
  inherited;
end;

procedure TMacHTTPRequest.DoPrepare;
var
  LClient: TMacHTTPClient;
begin
  if FURL.Username <> '' then
    SetCredential(TCredentialsStorage.TCredential.Create(TAuthTargetType.Server, '', FURL.ToString,
      FURL.Username, FURL.Password));

  LClient := TMacHTTPClient(FClient);
  LClient.UpdateSession;
  SetMacDecompression;
  SetMacRequestParams;
end;

procedure TMacHTTPRequest.SetMacDecompression;
var
  LClient: TMacHTTPClient;
  LDecompress: THTTPCompressionMethods;
  LEncodings: string;
begin
  LClient := TMacHTTPClient(FClient);
  LDecompress := LClient.AutomaticDecompression;
  if (LDecompress <> []) and not (THTTPCompressionMethod.Any in LDecompress) then
  begin
    LEncodings := '';
    if THTTPCompressionMethod.Deflate in LDecompress then
      LEncodings := LEncodings + ', deflate';  // do not translate
    if THTTPCompressionMethod.GZip in LDecompress then
      LEncodings := LEncodings + ', gzip';  // do not translate
    if THTTPCompressionMethod.Brotli in LDecompress then
      LEncodings := LEncodings + ', br';  // do not translate
    LEncodings := Copy(LEncodings, 3, MaxInt);
    SetHeaderValue('Accept-Encoding', LEncodings);
  end;
end;

procedure TMacHTTPRequest.SetMacRequestParams;
var
  LBody: NSData;
  LBytes: TBytes;
  LDataLength: Int64;
  LAbort: Boolean;
begin
  FRequest.setHTTPMethod(StrToNSStr(FMethodString));
  FRequest.setURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(FURL.ToString))));
  if FSourceStream <> nil then
  begin
    LDataLength := FSourceStream.Size - FSourceStream.Position;
    if LDataLength > 0 then
    begin
      DoSendDataProgress(LDataLength, 0, LAbort, True);
      if not LAbort then
      begin
        SetLength(LBytes, LDataLength);
        FSourceStream.ReadBuffer(LBytes, LDataLength);
        LBody := TNSData.Wrap(TNSData.OCClass.dataWithBytes(@LBytes[0], LDataLength));
        FRequest.setHTTPBody(LBody);
      end;
    end;
  end;
end;

procedure TMacHTTPRequest.DoCancel;
begin
  TMonitor.Enter(Self);
  try
    if FDataTask <> nil then
      FDataTask.cancel;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TMacHTTPRequest.AddHeader(const AName, AValue: string);
begin
  inherited;
  FRequest.addValue(StrToNSStr(AValue), StrToNSStr(AName));
end;

function TMacHTTPRequest.GetHeaders: TNetHeaders;
begin
  Result := [];
  if (FRequest.allHTTPHeaderFields <> nil) and (FRequest.allHTTPHeaderFields.allValues <> nil) and
     (FRequest.allHTTPHeaderFields.allValues.count > 0) then
    KeyValueToNetHeaders(FRequest.allHTTPHeaderFields.allKeys, FRequest.allHTTPHeaderFields.allValues, Result)
end;

function TMacHTTPRequest.GetHeaderValue(const AName: string): string;
begin
  Result := NSStrToStr(FRequest.valueForHTTPHeaderField(StrToNSStr(AName)));
end;

function TMacHTTPRequest.RemoveHeader(const AName: string): Boolean;
begin
  FRequest.setValue(nil, StrToNSStr(AName));
  Result := True;
end;

procedure TMacHTTPRequest.SetConnectionTimeout(const Value: Integer);
begin
  inherited;
  // Not Available in MacOS/iOS. The Connection Timeout has no effect...
end;

procedure TMacHTTPRequest.SetHeaderValue(const AName, Value: string);
begin
  FRequest.setValue(StrToNSStr(Value), StrToNSStr(AName));
end;

procedure TMacHTTPRequest.SetResponseTimeout(const Value: Integer);
begin
  inherited;
  FRequest.setTimeoutInterval(Value / 1000);
end;

{ TMacConnectionDataDelegate }

procedure TMacConnectionDataDelegate.URLSessionTaskDidCompleteWithError(
  session: NSURLSession; task: NSURLSessionTask; didCompleteWithError: NSError);
var
  LResponse: TMacHTTPResponse;
begin
  if not FDataTasks.FindResponse(task, LResponse) then
    raise ENetHTTPClientException.CreateRes(@SNetHttpClientResponseError);
  LResponse.FinishRequest(didCompleteWithError);
end;

procedure TMacConnectionDataDelegate.URLSessionDataTaskDidBecomeDownloadTask(
  session: NSURLSession; dataTask: NSURLSessionDataTask;
  didBecomeDownloadTask: NSURLSessionDownloadTask);
begin
  // nothing
end;

procedure TMacConnectionDataDelegate.URLSessionDataTaskDidReceiveData(
  session: NSURLSession; dataTask: NSURLSessionDataTask; didReceiveData: NSData);
var
  LResponse: TMacHTTPResponse;
  LAbort: Boolean;
begin
  if not FDataTasks.FindResponse(dataTask, LResponse) then
  begin
    dataTask.cancel;
    raise ENetHTTPClientException.CreateRes(@SNetHttpClientResponseError);
  end;
  try
    LResponse.FReaded := LResponse.FReaded + Int64(didReceiveData.length);
    LResponse.FStream.Write(didReceiveData.bytes^, didReceiveData.length);
    LResponse.FRequest.DoReceiveDataProgress(LResponse.GetStatusCode, LResponse.GetContentLength,
      LResponse.FReaded, LAbort);
    if LAbort then
      dataTask.cancel;
  except
    FDataTasks.HandleException(dataTask);
  end;
end;

procedure TMacConnectionDataDelegate.URLSessionDataTaskDidReceiveResponseCompletionHandler(
  session: NSURLSession; dataTask: NSURLSessionDataTask; didReceiveResponse: NSURLResponse;
  completionHandler: Pointer);
var
  LCompletionHandlerImpl: procedure(param1: NSURLSessionResponseDisposition); cdecl;
  LResponse: TMacHTTPResponse;
  LAbort: Boolean;
begin
  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    if not FDataTasks.FindResponse(dataTask, LResponse) then
    begin
      LCompletionHandlerImpl(NSURLSessionResponseCancel);
      raise ENetHTTPClientException.CreateRes(@SNetHttpClientResponseError);
    end;
    try
      LResponse.CreateNativeData(didReceiveResponse);
      LResponse.FRequest.DoReceiveDataProgress(LResponse.GetStatusCode, LResponse.GetContentLength, 0, LAbort);
      if LAbort then
        LCompletionHandlerImpl(NSURLSessionResponseCancel)
      else
        LCompletionHandlerImpl(NSURLSessionResponseAllow);
    except
      LCompletionHandlerImpl(NSURLSessionResponseCancel);
      FDataTasks.HandleException(dataTask);
    end;
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;
end;

procedure TMacConnectionDataDelegate.URLSessionTaskDidSendBodyDataTotalBytesSentTotalBytesExpectedToSend(
  session: NSURLSession; task: NSURLSessionTask; didSendBodyData: Int64; totalBytesSent: Int64;
  totalBytesExpectedToSend: Int64);
var
  LResponse: TMacHTTPResponse;
  LAbort: Boolean;
begin
  if not FDataTasks.FindResponse(task, LResponse) then
  begin
    task.cancel;
    raise ENetHTTPClientException.CreateRes(@SNetHttpClientResponseError);
  end;
  try
    LResponse.FRequest.DoSendDataProgress(totalBytesExpectedToSend, totalBytesSent, LAbort, True);
    if LAbort then
      task.cancel;
  except
    FDataTasks.HandleException(task);
  end;
end;

procedure TMacConnectionDataDelegate.URLSessionTaskNeedNewBodyStream(
  session: NSURLSession; task: NSURLSessionTask; needNewBodyStream: TFoundationNeedNewBodyStream);
begin
  // nothing
end;

procedure TMacConnectionDataDelegate.URLSessionDataTaskWillCacheResponseCompletionHandler(
  session: NSURLSession; dataTask: NSURLSessionDataTask; willCacheResponse: NSCachedURLResponse;
  completionHandler: Pointer {TFoundationCompletionHandler9}); cdecl;
var
  LCompletionHandlerImpl: procedure(param1: Pointer); cdecl;
begin
  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    LCompletionHandlerImpl(nil);
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;
end;

function CFStringRefToString(const Value: CFStringRef; Release: Boolean = False): string;
var
  Range: CFRange;
  Tmp: TCharArray;
begin
  if Value = nil then Exit('');
  try
    Range := CFRangeMake(0, CFStringGetLength(Value));
    if Range.Length > 0 then
    begin
      SetLength(Tmp, Range.Length);
      CFStringGetCharacters(Value, Range, MarshaledString(Tmp));
      Result := string.Create(Tmp);
    end
    else
      Result := EmptyStr;
  finally
    if Release then
      CFRelease(Value);
  end;
end;

procedure TMacConnectionDataDelegate.ExtractCertificateInfo(ACertificate: SecCertificateRef; var CertInfo: TCertificate);
{$IF defined(IOS)}
var
  LStr: CFStringRef;
  LData: CFDataRef;
  I: Integer;
  s: string;
begin
  CertInfo := Default(TCertificate);

  if Assigned(SecCertificateCopyCommonNameProc) then
  begin
    SecCertificateCopyCommonNameProc(ACertificate, LStr);  // 10.3
    CertInfo.CertName := CFStringRefToString(LStr, True);
  end;

  LStr := SecCertificateCopySubjectSummary(ACertificate); // 2.0
  CertInfo.Subject := CFStringRefToString(LStr, True);
  if CertInfo.CertName.IsEmpty then
    CertInfo.CertName := CertInfo.Subject;

  if Assigned(SecCertificateCopySerialNumberDataProc) then
  begin
    LData := SecCertificateCopySerialNumberDataProc(ACertificate, nil); // 11.0
    I := CFDataGetLength(LData);
    SetLength(s, I * 2);
    BinToHex(CFDataGetBytePtr(LData), PChar(s), I);
    CertInfo.SerialNum := s;
  end;
end;
{$ELSE !IOS}
const
  kIssuer = 'Issuer Name'; // Do not translate
  kSubjectName = 'Subject Name';
  kNotValidBefore = 'Not Valid Before';
  kNotValidAfter = 'Not Valid After';
  kSerialNum = 'Serial Number';
  kPublicKey = 'Public Key Data';
var
  LError: CFErrorRef;
  LCFCertData: CFDictionaryRef;
  LDict: NSDictionary;
  I: Integer;
  LDataDict: NSDictionary;
  LValue: NSString;
  LName, LText: string;
  LArrayValues: NSArray;
  LValuesDict: NSDictionary;
  J: Integer;
  LNumValue: NSNumber;
  LData: NSData;
  L: Integer;
  P: PByte;

  function MacToDateTime(const AbsTime: CFAbsoluteTime): TDateTime;
  var
    LDate: CFGregorianDate;
  begin
    LDate := CFAbsoluteTimeGetGregorianDate(AbsTime, nil);
    with LDate do
      Result := EncodeDateTime(year, month, day, hour, minute, Round(second), 0);
  end;

begin
  CertInfo := Default(TCertificate);
  LError := nil;
  LCFCertData := SecCertificateCopyValues(ACertificate, nil, LError);
  try
    LDict := TNSDictionary.Wrap(LCFCertData);
    for I := 0 to LDict.allKeys.count - 1 do
    begin
      LValue := TNSString.Wrap(LDict.allKeys.objectAtIndex(I));
      LDataDict := TNSDictionary.Wrap(LDict.allValues.objectAtIndex(I));

      LValue := TNSString.Wrap(LDataDict.valueForKey(kSecPropertyKeyLabel));
      LName := NSStrToStr(LValue);
      if (CompareText(LName, kIssuer) = 0) or (CompareText(LName, kSubjectName) = 0) then
      begin
        LText := '';
        LValue := nil;
        LArrayValues := TNSArray.Wrap(LDataDict.valueForKey(kSecPropertyKeyValue));
        if LArrayValues.count > 0 then
        begin
          for J := 0 to LArrayValues.count - 1 do
          begin
            LValuesDict := TNSDictionary.Wrap(LArrayValues.objectAtIndex(J));
            LValue := TNSString.Wrap(LValuesDict.valueForKey(kSecPropertyKeyValue));
            if LText <> '' then
              LText := LText + ';';
            LText := LText + NSStrToStr(LValue);
          end;
        end;
        if CompareText(LName, kIssuer) = 0 then
          CertInfo.Issuer := LText
        else
        begin
          if LValue <> nil then
            CertInfo.CertName := NSStrToStr(LValue);
          CertInfo.Subject := LText;
        end;
      end
      else if CompareText(LName, kNotValidBefore) = 0 then
      begin
        LNumValue := TNSNumber.Wrap(LDataDict.valueForKey(kSecPropertyKeyValue));
        CertInfo.Start := MacToDateTime(CFAbsoluteTime(LNumValue.intValue));
      end
      else if CompareText(LName, kNotValidAfter) = 0 then
      begin
        LNumValue := TNSNumber.Wrap(LDataDict.valueForKey(kSecPropertyKeyValue));
        CertInfo.Expiry := MacToDateTime(CFAbsoluteTime(LNumValue.intValue));
      end
      else if CompareText(LName, kSerialNum) = 0 then
      begin
        LValue := TNSString.Wrap(LDataDict.valueForKey(kSecPropertyKeyValue));
        LText := NSStrToStr(LValue);
        CertInfo.SerialNum := LText.Replace(' ', '', [rfReplaceAll]);
      end
      else if CompareText(LName, kPublicKey) = 0 then
      begin
                                                                                                                                  
        LData := TNSData.Wrap(LDataDict.valueForKey(kSecPropertyKeyValue));
        L := LData.length;
        P := LData.bytes;
        if ((P + 0)^ = $30) and ((P + 1)^ = $82) and ((P + 2)^ = $01) and ((P + 3)^ in [$0A, $00]) and
           ((P + 4)^ = $02) and ((P + 5)^ = $82) and ((P + 6)^ = $01) and ((P + 7)^ in [$01, $00]) and
           ((P + L - 5)^ = $02) and ((P + L - 4)^ = $03) then
        begin
          Dec(L, 8); Inc(P, 8); Dec(L, 5);
          if P^ = 0 then
          begin
            Inc(P);
            Dec(L);
          end;
          SetLength(LText, L * 2);
          BinToHex(P, PChar(LText), L);
          CertInfo.PublicKey := LText;
          CertInfo.KeySize := L;
        end;
      end;
    end;
  finally
    CFRelease(LCFCertData);
  end;
end;
{$ENDIF !IOS}

{$IF not defined(IOS)}
procedure TMacConnectionDataDelegate.GetKeychainIdentities(const AIdentityList: TMacIdentityList);
var
  LIdentDict: NSMutableDictionary;
  LRes: OSStatus;
  I: Integer;
  LIdents: Pointer;
  LAllIdents: NSArray;
begin
  LIdentDict := TNSMutableDictionary.Create;
  try
    LIdentDict.setValue((kSecClassIdentity as ILocalObject).GetObjectID, kSecClass);
    LIdentDict.setValue(kCFBooleanTrue, kSecReturnRef);
    LIdentDict.setValue((kSecMatchLimitAll as ILocalObject).GetObjectID, KSecMatchLimit);
    LIdents := nil;
    LRes := SecItemCopyMatching((LIdentDict as ILocalObject).GetObjectID, @LIdents);
    if (LRes <> errSecSuccess) then
      raise ENetHTTPCertificateException.CreateRes(@SNetHttpIdentitiesError);

    LAllIdents := TNSArray.Wrap(LIdents);
    if LAllIdents.count > 0 then
      for I := 0 to LAllIdents.count - 1 do
        AIdentityList.Add(LAllIdents.objectAtIndex(I));

  finally
    LIdentDict.release;
  end;
end;

procedure TMacConnectionDataDelegate.FilterCertificates(const AIssuers: NSArray; const AMacCertList: TMacCertList;
  const ACertList: TCertificateList);
var
  LDict: NSMutableDictionary;
  LRes: OSStatus;
  LCerts: Pointer;
  LAllCerts: NSArray;
  I: Integer;
  LCert: SecCertificateRef;
  LCertInfo: TCertificate;
begin
  LDict := TNSMutableDictionary.Create;
  try
    LDict.setValue((kSecClassCertificate as ILocalObject).GetObjectID, kSecClass);
    LDict.setValue(kCFBooleanTrue, kSecReturnRef);
    if (AIssuers <> nil) and (AIssuers.count > 0) then
      LDict.setValue(AIssuers.objectAtIndex(0), kSecAttrIssuer);
    LDict.setValue(kCFBooleanFalse, kSecMatchTrustedOnly);
    LDict.setValue(kCFBooleanTrue, kSecMatchCaseInsensitive);
    LDict.setValue((kSecMatchLimitAll as ILocalObject).GetObjectID, KSecMatchLimit);

    LCerts := nil;
    LRes := SecItemCopyMatching((LDict as ILocalObject).GetObjectID, @LCerts);
    if (LRes <> errSecSuccess) and (LRes <> errSecItemNotFound) then
      raise ENetHTTPCertificateException.CreateRes(@SNetHttpCertificatesError);

    LAllCerts := TNSArray.Wrap(LCerts);
    if LAllCerts.count > 0 then
    begin
      for I := 0 to LAllCerts.count - 1 do
      begin
        LCert := LAllCerts.objectAtIndex(I);
        ExtractCertificateInfo(LCert, LCertInfo);
        AMacCertList.Add(LCert);
        ACertList.Add(LCertInfo);
      end;
    end;

  finally
    LDict.release;
  end;
end;

function TMacConnectionDataDelegate.FindIdentityForCertificate(const AIdentityList: TMacIdentityList;
  ACertificate: SecCertificateRef): SecIdentityRef;
var
  LNewCert: SecCertificateRef;
  I: Integer;
  LRes: OSStatus;
begin
  LNewCert := nil;
  for I := 0 to AIdentityList.Count - 1 do
  begin
    LRes := SecIdentityCopyCertificate(AIdentityList[I], @LNewCert);
    try
      if (LRes = errSecSuccess) and CFEqual(LNewCert, ACertificate) then
         Exit(AIdentityList[I]);
    finally
      CFRelease(LNewCert);
    end;
  end;
  Result := nil;
end;
{$ENDIF !IOS}

{$IF defined(IOS)}
function TMacConnectionDataDelegate.ImportP12Certificate(var AIdentity: SecIdentityRef; var ACertificates: NSArray;
  const ARequest: TMacHTTPRequest): Boolean;
var
  LCertData: NSData;
  LDict: NSMutableDictionary;
  LPassword: NSString;
  LImported: Pointer;
  LImportedData: NSArray;
  LRes: OSStatus;
  LIdentityDict: NSDictionary;
  LCert: SecCertificateRef;
  LRawCert: TBytes;
begin
  Result := False;
  if ARequest.FClientCertificate <> nil then
  begin
    SetLength(LRawCert, ARequest.FClientCertificate.Size);
    ARequest.FClientCertificate.Position := 0;
    ARequest.FClientCertificate.Read(LRawCert, Length(LRawCert));
    LCertData := TNSData.Wrap(TNSData.OCClass.dataWithBytes(@LRawCert[0], Length(LRawCert)));
    LDict := TNSMutableDictionary.Create;
    try
      LPassword := StrToNSStr(ARequest.FClientCertPassword);
      LDict.setValue((LPassword as ILocalObject).GetObjectID, kSecImportExportPassphrase);
      LRes := SecPKCS12Import((LCertData as ILocalObject).GetObjectID, (LDict as ILocalObject).GetObjectID, @LImported);
      if LRes <> errSecSuccess then
        raise ENetHTTPCertificateException.CreateRes(@SNetHttpCertificateImportError);
      LImportedData := TNSArray.Wrap(LImported);
      LIdentityDict := TNSDictionary.Wrap(LImportedData.objectAtIndex(0));
      AIdentity := LIdentityDict.valueForKey(kSecImportItemIdentity);
      LRes := SecIdentityCopyCertificate(AIdentity, @LCert);
      if LRes <> errSecSuccess then
        raise ENetHTTPCertificateException.CreateRes(@SNetHttpCertificateImportError);

      ACertificates := TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(LCert));
      Result := True;

    finally
      LDict.release;
    end;
  end;
end;
{$ENDIF IOS}

procedure TMacConnectionDataDelegate.URLSessionTaskDidReceiveChallengeCompletionHandler(
  session: NSURLSession; task: NSURLSessionTask; didReceiveChallenge: NSURLAuthenticationChallenge;
  completionHandler: Pointer);
const
  errSecHostNameMismatch = -67602;
  errSecCertificateExpired = -67818;
  errSecCertificateNotValidYet = -67819;
  errSecCertificateRevoked = -67820;
  errSecInvalidCertAuthority = -67826;
  errSecCRLExpired = -67613;
  errSecCRLNotValidYet = -67614;
var
  LCompletionHandlerImpl: procedure(param1: NSURLSessionAuthChallengeDisposition; ignored: Pointer; param2: Pointer); cdecl;
  LProtSpace: NSURLProtectionSpace;
  LAuthMethod: NSString;
  LResponse: TMacHTTPResponse;
  LNewCredential: NSURLCredential;
  LAccepted: Boolean;
  LCertificate: TCertificate;
  LCertRef: SecCertificateRef;
{$IF not defined(IOS)}
  LMacCertList: TMacCertList;
  LCertList: TCertificateList;
  LIdentityList: TMacIdentityList;
  LClientCertIndex: Integer;
{$ENDIF !IOS}
  LIdentity: SecIdentityRef;
  LCertArray: NSArray;
  LErrorRef: CFErrorRef;
  LError: NSError;
  LCode: NSInteger;
  LReasons: THTTPSecureFailureReasons;
  LTrustRes: uint32;
  LCredential: TCredentialsStorage.TCredential;
begin
  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    if not FDataTasks.FindResponse(task, LResponse) then
    begin
      LCompletionHandlerImpl(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
      raise ENetHTTPClientException.CreateRes(@SNetHttpClientResponseError);
    end;
    try
      LProtSpace := didReceiveChallenge.protectionSpace;
      LAuthMethod := LProtSpace.authenticationMethod;
      if LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPBasic) or
         LAuthMethod.isEqualToString(NSURLAuthenticationMethodHTTPDigest) or
         LAuthMethod.isEqualToString(NSURLAuthenticationMethodNTLM) then
      begin
        LCredential := Default(TCredentialsStorage.TCredential);
        if LProtSpace.isProxy then
        begin
          if not LResponse.FRequest.FProxyCredentialUsed then
          begin
            LCredential := LResponse.FRequest.FProxyCredential;
            LResponse.FRequest.FProxyCredentialUsed := True;
          end;
        end
        else
        begin
          if not LResponse.FRequest.FServerCredentialUsed then
          begin
            LCredential := LResponse.FRequest.FServerCredential;
            LResponse.FRequest.FServerCredentialUsed := True;
          end;
        end;
        if not LCredential.IsEmpty then
        begin
          LNewCredential := TNSURLCredential.Wrap(TNSURLCredential.OCClass.credentialWithUser(
            StrToNSStr(LCredential.UserName), StrToNSStr(LCredential.Password),
            NSURLCredentialPersistenceNone));
          LCompletionHandlerImpl(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(LNewCredential));
        end
        else
          LCompletionHandlerImpl(NSURLSessionAuthChallengeRejectProtectionSpace, nil, nil);
      end
      else
      begin
        if LAuthMethod.isEqualToString(NSURLAuthenticationMethodServerTrust) then
        begin
          LAccepted := True;
          if SecTrustGetCertificateCount(LProtSpace.serverTrust) > 0 then
          begin
            LCertRef := SecTrustGetCertificateAtIndex(LProtSpace.serverTrust, 0);
            ExtractCertificateInfo(LCertRef, LCertificate);

            LReasons := [];
            if Assigned(SecTrustEvaluateWithErrorProc) then
            begin
              SecTrustEvaluateWithErrorProc(LProtSpace.serverTrust, LErrorRef);
              if LErrorRef <> nil then
              begin
                LError := TNSError.Wrap(LErrorRef);
                LCode := LError.code;
                case LCode of
                errSecSuccess:
                  ;
                errSecHostNameMismatch:
                  Include(LReasons, THTTPSecureFailureReason.CertCNInvalid);
                errSecCertificateExpired,
                errSecCertificateNotValidYet,
                errSecCRLExpired,
                errSecCRLNotValidYet:
                  Include(LReasons, THTTPSecureFailureReason.CertDateInvalid);
                errSecCertificateRevoked:
                  Include(LReasons, THTTPSecureFailureReason.CertRevoked);
                errSecInvalidCertAuthority:
                  Include(LReasons, THTTPSecureFailureReason.InvalidCA);
                                                                   
                else
                  Include(LReasons, THTTPSecureFailureReason.SecurityChannelError);
                end;
              end;
            end
            else if Assigned(SecTrustEvaluateProc) then
            begin
              SecTrustEvaluateProc(LProtSpace.serverTrust, LTrustRes);
              case LTrustRes of
              kSecTrustResultUnspecified:
                ;
              kSecTrustResultProceed,
              kSecTrustResultConfirm:
                Include(LReasons, THTTPSecureFailureReason.InvalidCert);
              kSecTrustResultDeny,
              kSecTrustResultRecoverableTrustFailure,
              kSecTrustResultOtherError:
                Include(LReasons, THTTPSecureFailureReason.CertNotAccepted);
              kSecTrustResultFatalTrustFailure:
                Include(LReasons, THTTPSecureFailureReason.InvalidCert);
              end;
            end;
            FMacHTTPClient.FSecureFailureReasons := LReasons;

            if not Assigned(FMacHTTPClient.FValidateServerCertificateCallback) and
               not Assigned(FMacHTTPClient.FValidateServerCertificateEvent) then
              LCompletionHandlerImpl(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil)
            else
            begin
              LAccepted := LReasons = [];
              if Assigned(FMacHTTPClient.FValidateServerCertificateCallback) then
                FMacHTTPClient.FValidateServerCertificateCallback(FMacHTTPClient, LResponse.FRequest, LCertificate, LAccepted)
              else if Assigned(FMacHTTPClient.FValidateServerCertificateEvent) then
                FMacHTTPClient.FValidateServerCertificateEvent(FMacHTTPClient, LResponse.FRequest, LCertificate, LAccepted);
              if LAccepted then
              begin
                LNewCredential := TNSURLCredential.Wrap(TNSURLCredential.OCClass.credentialForTrust(LProtSpace.serverTrust));
                LCompletionHandlerImpl(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(LNewCredential));
              end
              else
              begin
                FMacHTTPClient.FSecureFailureReasons := FMacHTTPClient.SecureFailureReasons + [THTTPSecureFailureReason.CertNotAccepted];
                LCompletionHandlerImpl(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
              end;
            end;
          end
          else
          begin
            FMacHTTPClient.FSecureFailureReasons := [THTTPSecureFailureReason.InvalidCert];
            LCompletionHandlerImpl(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
          end;
        end
        else
        begin
          if LAuthMethod.isEqualToString(NSURLAuthenticationMethodClientCertificate) then
          begin
{$IF defined(IOS)}
            if ImportP12Certificate(LIdentity, LCertArray, LResponse.FRequest) then
            begin
              try
                LNewCredential := TNSURLCredential.Wrap(TNSURLCredential.OCClass.credentialWithIdentity(LIdentity, LCertArray, 1));
                LCompletionHandlerImpl(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(LNewCredential));
              finally
                CFRelease(LCertArray.objectAtIndex(0));
              end;
            end
            else
              LCompletionHandlerImpl(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
{$ELSE}
            LMacCertList := TMacCertList.Create;
            LCertList := TCertificateList.Create;
            LIdentityList := TMacIdentityList.Create;
            try
              if Assigned(FMacHTTPClient.FNeedClientCertificateCallback) or Assigned(FMacHTTPClient.FNeedClientCertificateEvent) then
              begin
                LClientCertIndex := -1;
                FilterCertificates(LProtSpace.distinguishedNames, LMacCertList, LCertList);
                GetKeyChainIdentities(LIdentityList);

                if Assigned(FMacHTTPClient.FNeedClientCertificateCallback) then
                  FMacHTTPClient.FNeedClientCertificateCallback(Self, LResponse.FRequest, LCertList, LClientCertIndex)
                else
                  FMacHTTPClient.FNeedClientCertificateEvent(Self, LResponse.FRequest, LCertList, LClientCertIndex);

                if LClientCertIndex >= 0 then
                begin
                  LIdentity := FindIdentityForCertificate(LIdentityList, LMacCertList[LClientCertIndex]);

                  if LIdentity <> nil then
                  begin
                    LCertArray := TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(LMacCertList[LClientCertIndex]));
                    LNewCredential := TNSURLCredential.Wrap(TNSURLCredential.OCClass.credentialWithIdentity(LIdentity, LCertArray, 1));
                    LCompletionHandlerImpl(NSURLSessionAuthChallengeUseCredential, nil, NSObjectToID(LNewCredential));
                  end
                  else
                    raise ENetHTTPCertificateException.CreateRes(@SNetHttpIdentityCertError);
                end
                else
                  LCompletionHandlerImpl(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil);
              end
              else
                LCompletionHandlerImpl(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil);

            finally
              LMacCertList.Free;
              LCertList.Free;
              LIdentityList.Free;
            end;
{$ENDIF !IOS}
          end
          else
            LCompletionHandlerImpl(NSURLSessionAuthChallengePerformDefaultHandling, nil, nil);
        end;
      end;
    except
      LCompletionHandlerImpl(NSURLSessionAuthChallengeCancelAuthenticationChallenge, nil, nil);
      FDataTasks.HandleException(task);
    end;
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;
end;

procedure TMacConnectionDataDelegate.URLSessionTaskWillPerformHTTPRedirectionNewRequestCompletionHandler(
  session: NSURLSession; task: NSURLSessionTask; willPerformHTTPRedirection: NSHTTPURLResponse;
  newRequest: NSURLRequest; completionHandler: Pointer);
var
  LCompletionHandlerImpl: procedure(param1: Pointer); cdecl;
  LResponse: TMacHTTPResponse;
  LRedirectRequest: NSMutableURLRequest;
  LResult: NSURLRequest;
begin
  @LCompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  try
    if not FDataTasks.FindResponse(task, LResponse) then
    begin
      task.cancel;
      raise ENetHTTPClientException.CreateRes(@SNetHttpClientResponseError);
    end;
    try
      LResult := newRequest;
      if willPerformHTTPRedirection <> nil then
      begin
        LResponse.CreateNativeData(willPerformHTTPRedirection);

        if LResponse.GetStatusCode <> 0 then
          if FMacHTTPClient.IsAutoRedirect(LResponse) then
          begin
            if LResponse.FRequest.AddRedirect > FMacHTTPClient.FMaxRedirects then
              raise ENetHTTPRequestException.CreateResFmt(@SNetHttpMaxRedirections, [FMacHTTPClient.FMaxRedirects]);

            // NSURLSession by default redirects from POST to GET on 302/303.
            // http://tewha.net/2012/05/handling-302303-redirects/
            if not FMacHTTPClient.IsAutoRedirectWithGET(LResponse.FRequest, LResponse) and
               (SameText(LResponse.FRequest.GetMethodString, sHTTPMethodPost) or
                SameText(LResponse.FRequest.GetMethodString, sHTTPMethodPut) or
                SameText(LResponse.FRequest.GetMethodString, sHTTPMethodDelete)) and
               SameText(NSStrToStr(newRequest.HTTPMethod), sHTTPMethodGet) then
            begin
              LRedirectRequest := LResponse.FRequest.FRequest.mutableCopy;
              LRedirectRequest.setURL(newRequest.URL);
              LResult := LRedirectRequest;
            end;

            // Sync request props with actual redirect request values
            LResponse.FRequest.FURL := TURI.Create(NSStrToStr(LResult.URL.absoluteString));
            LResponse.FRequest.FMethodString := NSStrToStr(LResult.HTTPMethod);

            // Update headers and cookies
            if FMacHTTPClient.AllowCookies then
              LResponse.GetHeaders;
          end
          else
            LResult := nil;
      end;
      LCompletionHandlerImpl(NSObjectToID(LResult));
    except
      FDataTasks.HandleException(task);
    end;
  finally
    imp_removeBlock(@LCompletionHandlerImpl);
  end;
end;

constructor TMacConnectionDataDelegate.Create(const ADataTasks: TDataTasks; const AClient: TMacHTTPClient);
begin
  Inherited Create;
  FDataTasks := ADataTasks;
  FMacHTTPClient := AClient;
end;

{ TDataTasks }

constructor TDataTasks.Create;
begin
  inherited Create;
  FList := TDictionary<Pointer, TMacHTTPResponse>.Create;
end;

destructor TDataTasks.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TDataTasks.AddTask(const ATask: NSURLSessionTask; const AResponse: TMacHTTPResponse);
var
  LObjectID: Pointer;
begin
  TMonitor.Enter(Self);
  try
    LObjectID := (ATask as ILocalObject).GetObjectID;
    FList.Add(LObjectID, AResponse);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TDataTasks.RemoveTask(const ATask: NSURLSessionTask);
var
  LObjectID: Pointer;
begin
  TMonitor.Enter(Self);
  try
    LObjectID := (ATask as ILocalObject).GetObjectID;
    FList.Remove(LObjectID);
  finally
    TMonitor.Exit(Self);
  end;
end;

function TDataTasks.FindResponse(const ATask: NSURLSessionTask; var AResponse: TMacHTTPResponse): Boolean;
var
  LObjectID: Pointer;
begin
  TMonitor.Enter(Self);
  try
    LObjectID := (ATask as ILocalObject).GetObjectID;
    Result := FList.TryGetValue(LObjectID, AResponse);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TDataTasks.HandleException(const ATask: NSURLSessionTask);
var
  LObjectID: Pointer;
  LResponse: TMacHTTPResponse;
begin
  ATask.cancel;
  TMonitor.Enter(Self);
  try
    LObjectID := (ATask as ILocalObject).GetObjectID;
    if FList.TryGetValue(LObjectID, LResponse) then
    begin
      FreeAndNil(LResponse.FException);
      LResponse.FException := Exception(AcquireExceptionObject);
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

initialization
  TURLSchemes.RegisterURLClientScheme(TMacHTTPClient, 'HTTP');
  TURLSchemes.RegisterURLClientScheme(TMacHTTPClient, 'HTTPS');
  InitSecProcs;

finalization
  TURLSchemes.UnRegisterURLClientScheme('HTTP');
  TURLSchemes.UnRegisterURLClientScheme('HTTPS');
end.

