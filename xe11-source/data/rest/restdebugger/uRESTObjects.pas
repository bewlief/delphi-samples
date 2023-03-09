{*******************************************************}
{                                                       }
{             Delphi REST Client Framework              }
{                                                       }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
unit uRESTObjects;

interface

uses
  System.Classes, System.Generics.Collections, System.JSON,
  REST.Types, REST.Client;

const
  AUTH_CRYPTO_VALUE: word = 47011;

type
  TRESTAuthMethod = (amNONE, amSIMPLE, amBASIC, amOAUTH, amOAUTH2);
  TRESTAuthMethods = set of TRESTAuthMethod;

  TRESTRequestParams = class(TObject)
  private
    FMethod: TRESTRequestMethod;
    FURL: string;
    FResource: string;
    FContentType: string;

    FAuthMethod: TRESTAuthMethod;
    FAuthUsername: string;
    FAuthUsernameKey: string;
    FAuthPassword: string;
    FAuthPasswordKey: string;

    /// oauth1 & oauth2
    FClientID: string;
    FClientSecret: string;
    FAuthCode: string;
    FAccessToken: string;
    FAccessTokenSecret: string;
    FRequestToken: string;
    FRequestTokenSecret: string;
    FRefreshToken: string;
    FOAuth1SignatureMethod: string;
    FOAuth2ResponseType: string;

    FEndpointAuth: string;
    FEndpointAccessToken: string;
    FEndpointRequestToken: string;
    FEndpointRedirect: string;

    FAuthScope: string;

    FCustomParams: TRESTRequestParameterList;
    FCustomBody: TMemoryStream;

    FRootElement: string;
    FNestedFields: Boolean;
    FDataSetView: Integer;
  protected
    procedure SetResource(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetToDefaults;

    procedure Assign(ASource: TRESTRequestParams);

    function AsJSONObject: TJSONObject;
    procedure FromJSONObject(const AJSONObject: TJSONObject);

    procedure SaveToFile(const AFilename: string);
    procedure LoadFromFile(const AFilename: string);

    function ToString: string; override;

    property URL: string read FURL write FURL;
    property Resource: string read FResource write SetResource;
    property Method: TRESTRequestMethod read FMethod write FMethod;
    property ContentType: string read FContentType write FContentType;

    property AuthMethod: TRESTAuthMethod read FAuthMethod write FAuthMethod;
    property AuthUsername: string read FAuthUsername write FAuthUsername;
    property AuthUsernameKey: string read FAuthUsernameKey write FAuthUsernameKey;
    property AuthPassword: string read FAuthPassword write FAuthPassword;
    property AuthPasswordKey: string read FAuthPasswordKey write FAuthPasswordKey;

    property ClientID: string read FClientID write FClientID;
    property ClientSecret: string read FClientSecret write FClientSecret;
    property AuthCode: string read FAuthCode write FAuthCode;
    property AccessToken: string read FAccessToken write FAccessToken;
    property AccessTokenSecret: string read FAccessTokenSecret write FAccessTokenSecret;
    property RequestToken: string read FRequestToken write FRequestToken;
    property RequestTokenSecret: string read FRequestTokenSecret write FRequestTokenSecret;
    property RefreshToken: string read FRefreshToken write FRefreshToken;
    property OAuth1SignatureMethod: string read FOAuth1SignatureMethod write FOAuth1SignatureMethod;
    property OAuth2ResponseType: string read FOAuth2ResponseType write FOAuth2ResponseType;

    property EndpointAuth: string read FEndpointAuth write FEndpointAuth;
    property EndpointAccessToken: string read FEndpointAccessToken write FEndpointAccessToken;
    property EndpointRequestToken: string read FEndpointRequestToken write FEndpointRequestToken;
    property EndpointRedirect: string read FEndpointRedirect write FEndpointRedirect;

    property AuthScope: string read FAuthScope write FAuthScope;

    property CustomBody: TMemoryStream read FCustomBody;
    property CustomParams: TRESTRequestParameterList read FCustomParams;

    property RootElement: string read FRootElement write FRootElement;
    property NestedFields: Boolean read FNestedFields write FNestedFields;
    property DataSetView: Integer read FDataSetView write FDataSetView;
  end;

  TRESTRequestParamsList = TList<TRESTRequestParams>;
  TRESTRequestParamsObjectList = TObjectList<TRESTRequestParams>;

var
  DefaultRESTAuthMethod: TRESTAuthMethod = amNONE;

function RESTRequestMethodFromString(const ARequestMethod: string): TRESTRequestMethod;

function RESTAuthMethodToString(const AAuthMethod: TRESTAuthMethod): string;
function RESTAuthMethodFromString(const AAuthMethod: string): TRESTAuthMethod;

function SimpleEncryptStr(const S: String; Key: word): string;
function SimpleDecryptStr(const S: String; Key: word): string;

implementation

uses
  System.StrUtils, System.SysUtils,
  IdGlobal, IdCoderMIME,
  REST.Consts;

function RESTRequestMethodFromString(const ARequestMethod: string): TRESTRequestMethod;
begin
  if SameText(ARequestMethod, 'GET') then
    Result := TRESTRequestMethod.rmGET
  else if SameText(ARequestMethod, 'POST') then
    Result := TRESTRequestMethod.rmPOST
  else if SameText(ARequestMethod, 'PUT') then
    Result := TRESTRequestMethod.rmPUT
  else if SameText(ARequestMethod, 'DELETE') then
    Result := TRESTRequestMethod.rmDELETE
  else if SameText(ARequestMethod, 'PATCH') then
    Result := TRESTRequestMethod.rmPATCH
  else
    raise Exception.Create(sRESTUnsupportedRequestMethod);
end;

function RESTAuthMethodToString(const AAuthMethod: TRESTAuthMethod): string;
begin
  case AAuthMethod of
    amNONE:
      Result := 'NONE';
    amSIMPLE:
      Result := 'SIMPLE';
    amBASIC:
      Result := 'BASIC';
    amOAUTH:
      Result := 'OAUTH';
    amOAUTH2:
      Result := 'OAUTH2';
  else
    Result := '--unknown--';
  end;
end;

function RESTAuthMethodFromString(const AAuthMethod: string): TRESTAuthMethod;
begin
  if SameText(AAuthMethod, 'NONE') then
    Result := amNONE
  else if SameText(AAuthMethod, 'SIMPLE') then
    Result := amSIMPLE
  else if SameText(AAuthMethod, 'BASIC') then
    Result := amBASIC
  else if SameText(AAuthMethod, 'OAUTH') then
    Result := amOAUTH
  else if SameText(AAuthMethod, 'OAUTH2') then
    Result := amOAUTH2
  else
    raise Exception.Create(sRESTUnsupportedAuthMethod);
end;

const
  CKEY1 = 53761;
  CKEY2 = 32618;

function SimpleEncryptStr(const S: string; Key: word): string;
var
  i: Integer;
  RStr: RawByteString;
  RStrB: TBytes absolute RStr;
begin
  Result := '';
  RStr := UTF8Encode(S);
  for i := 0 to Length(RStr) - 1 do
  begin
    RStrB[i] := RStrB[i] xor (Key shr 8);
    Key := (RStrB[i] + Key) * CKEY1 + CKEY2;
  end;
  for i := 0 to Length(RStr) - 1 do
    Result := Result + IntToHex(RStrB[i], 2);
end;

function SimpleDecryptStr(const S: String; Key: word): string;
var
  i, tmpKey: Integer;
  RStr: RawByteString;
  RStrB: TBytes absolute RStr;
  tmpStr: string;
begin
  tmpStr := UpperCase(S);
  SetLength(RStr, Length(tmpStr) div 2);
  i := 1;
  try
    while (i < Length(tmpStr)) do
    begin
      RStrB[i div 2] := StrToInt('$' + tmpStr[i] + tmpStr[i + 1]);
      Inc(i, 2);
    end;
  except
    Result := '';
    Exit;
  end;
  for i := 0 to Length(RStr) - 1 do
  begin
    tmpKey := RStrB[i];
    RStrB[i] := RStrB[i] xor (Key shr 8);
    Key := (tmpKey + Key) * CKEY1 + CKEY2;
  end;
  Result := UTF8ToString(RStr);
end;

{ TRESTRequestParams }

constructor TRESTRequestParams.Create;
begin
  inherited;

  FCustomParams := TRESTRequestParameterList.Create(nil);
  FCustomBody := TMemoryStream.Create;

  ResetToDefaults;
end;

destructor TRESTRequestParams.Destroy;
begin
  ResetToDefaults;

  FreeAndNIL(FCustomBody);
  FreeAndNIL(FCustomParams);

  inherited;
end;

procedure TRESTRequestParams.ResetToDefaults;
begin
  FURL := '';
  FResource := '';
  FMethod := DefaultRESTRequestMethod;

  FContentType := '';

  /// do NOT reset the proxy-settings!

  FAuthMethod := DefaultRESTAuthMethod;
  FAuthUsername := '';
  FAuthUsernameKey := '';
  FAuthPassword := '';
  FAuthPasswordKey := '';

  FClientID := '';
  FClientSecret := '';
  FAuthCode := '';
  FAccessToken := '';
  FAccessTokenSecret := '';
  FRequestToken := '';
  FRequestTokenSecret := '';
  FRefreshToken := '';
  FOAuth1SignatureMethod := '';
  FOAuth2ResponseType := '';

  FEndpointAuth := '';
  FEndpointAccessToken := '';
  FEndpointRequestToken := '';
  FEndpointRedirect := '';

  FAuthScope := '';

  FCustomParams.Clear;
  FCustomBody.Clear;

  FRootElement := '';
  FNestedFields := False;
  FDataSetView := 0;
end;

procedure TRESTRequestParams.Assign(ASource: TRESTRequestParams);
var
  LParam: TRESTRequestParameter;
  LParam2: TRESTRequestParameter;
  LPosition: int64;
begin
  Assert(Assigned(ASource));

  FMethod := ASource.Method;
  FURL := ASource.URL;
  FResource := ASource.Resource;
  FContentType := ASource.ContentType;

  FAuthMethod := ASource.AuthMethod;
  FAuthUsername := ASource.AuthUsername;
  FAuthUsernameKey := ASource.AuthUsernameKey;
  FAuthPassword := ASource.AuthPassword;
  FAuthPasswordKey := ASource.AuthPasswordKey;

  FClientID := ASource.ClientID;
  FClientSecret := ASource.ClientSecret;
  FAuthCode := ASource.AuthCode;
  FAccessToken := ASource.AccessToken;
  FAccessTokenSecret := ASource.AccessTokenSecret;
  FRequestToken := ASource.RequestToken;
  FRequestTokenSecret := ASource.RequestTokenSecret;
  FRefreshToken := ASource.RefreshToken;
  FOAuth1SignatureMethod := ASource.OAuth1SignatureMethod;
  FOAuth2ResponseType := ASource.OAuth2ResponseType;

  FEndpointAuth := ASource.EndpointAuth;
  FEndpointAccessToken := ASource.EndpointAccessToken;
  FEndpointRequestToken := ASource.EndpointRequestToken;
  FEndpointRedirect := ASource.EndpointRedirect;

  FAuthScope := ASource.AuthScope;

  FCustomParams.Clear;
  for LParam in ASource.CustomParams do
  begin
    LParam2 := FCustomParams.AddItem;
    LParam2.Assign(LParam);
  end;

  FCustomBody.Clear;
  /// save stream-position
  LPosition := ASource.CustomBody.Position;
  try
    ASource.CustomBody.Position := 0;
    FCustomBody.CopyFrom(ASource.CustomBody, ASource.CustomBody.Size);
  finally
    ASource.CustomBody.Position := LPosition;
  end;

  FRootElement := ASource.FRootElement;
  FNestedFields := ASource.FNestedFields;
  FDataSetView := ASource.FDataSetView;
end;

function TRESTRequestParams.AsJSONObject: TJSONObject;
var
  LRoot: TJSONObject;
  LAuth: TJSONObject;
  LParams: TJSONArray;
  LParameter: TRESTRequestParameter;
  LObject: TJSONObject;
begin
  LRoot := TJSONObject.Create;
  LRoot.AddPair(TJSONPair.Create('method', TJSONString.Create(RESTRequestMethodToString(FMethod))));
  LRoot.AddPair(TJSONPair.Create('url', TJSONString.Create(FURL)));
  LRoot.AddPair(TJSONPair.Create('resource', TJSONString.Create(FResource)));
  LRoot.AddPair(TJSONPair.Create('contenttype', TJSONString.Create(FContentType)));

  /// write auth-info as a object ("dictionary")
  LAuth := TJSONObject.Create;
  LAuth.AddPair(TJSONPair.Create('method', TJSONString.Create(RESTAuthMethodToString(FAuthMethod))));
  LAuth.AddPair(TJSONPair.Create('username', TJSONString.Create(FAuthUsername)));
  LAuth.AddPair(TJSONPair.Create('usernamekey', TJSONString.Create(FAuthUsernameKey)));
  LAuth.AddPair(TJSONPair.Create('password', TJSONString.Create(FAuthPassword)));
  LAuth.AddPair(TJSONPair.Create('passwordkey', TJSONString.Create(FAuthPasswordKey)));
  LAuth.AddPair(TJSONPair.Create('clientid', TJSONString.Create(FClientID)));
  LAuth.AddPair(TJSONPair.Create('clientsecret', TJSONString.Create(FClientSecret)));
  LAuth.AddPair(TJSONPair.Create('authcode', TJSONString.Create(FAuthCode)));
  LAuth.AddPair(TJSONPair.Create('accesstoken', TJSONString.Create(FAccessToken)));
  LAuth.AddPair(TJSONPair.Create('accesstokensecret', TJSONString.Create(FAccessTokenSecret)));
  LAuth.AddPair(TJSONPair.Create('requesttoken', TJSONString.Create(FRequestToken)));
  LAuth.AddPair(TJSONPair.Create('requesttokensecret', TJSONString.Create(FRequestTokenSecret)));
  LAuth.AddPair(TJSONPair.Create('refreshtoken', TJSONString.Create(FRefreshToken)));
  LAuth.AddPair(TJSONPair.Create('signaturemethod', TJSONString.Create(FOAuth1SignatureMethod)));
  LAuth.AddPair(TJSONPair.Create('responsetype', TJSONString.Create(FOAuth2ResponseType)));
  LAuth.AddPair(TJSONPair.Create('endpointauth', TJSONString.Create(FEndpointAuth)));
  LAuth.AddPair(TJSONPair.Create('endpointaccesstoken', TJSONString.Create(FEndpointAccessToken)));
  LAuth.AddPair(TJSONPair.Create('endpointrequesttoken', TJSONString.Create(FEndpointRequestToken)));
  LAuth.AddPair(TJSONPair.Create('endpointredirect', TJSONString.Create(FEndpointRedirect)));
  LAuth.AddPair(TJSONPair.Create('authscope', TJSONString.Create(FAuthScope)));

  // LRoot.AddPair(TJSONPair.Create('auth', SimpleEncryptStr(LAuth.ToString, AUTH_CRYPTO_VALUE)));
  LRoot.AddPair(TJSONPair.Create('auth', LAuth));

  (*
    --> Proxy-Settings are no more saved in the context of a single request.
    instead we treat them as part of the app-config for the rest-debugger.
  *)

  /// write custom params as an array of objects
  LParams := TJSONArray.Create;
  for LParameter in FCustomParams do
  begin
    LObject := TJSONObject.Create;
    LObject.AddPair(TJSONPair.Create('name', LParameter.Name));
    LObject.AddPair(TJSONPair.Create('value', LParameter.Value));
    LObject.AddPair(TJSONPair.Create('kind', RESTRequestParameterKindToString(LParameter.Kind)));
    LObject.AddPair(TJSONPair.Create('encode', TJSONBool.Create(not (poDoNotEncode in LParameter.Options))));
    LParams.Add(LObject);
  end;
  LRoot.AddPair(TJSONPair.Create('parameters', LParams));

  /// write custom body
  FCustomBody.Seek(0, soFromBeginning);
  LRoot.AddPair(TJSONPair.Create('body', TJSONString.Create(TIdEncoderMIME.EncodeStream(FCustomBody))));

  LRoot.AddPair(TJSONPair.Create('datasetview', FDataSetView));
  LRoot.AddPair(TJSONPair.Create('rootelement', FRootElement));
  LRoot.AddPair(TJSONPair.Create('nestedfields', FNestedFields));

  Result := LRoot;
end;

procedure TRESTRequestParams.FromJSONObject(const AJSONObject: TJSONObject);
var
  auth: TJSONObject;
  pair: TJSONPair;
  i: Integer;
  LParams: TJSONArray;
  LObject: TJSONObject;
  LParameter: TRESTRequestParameter;
  LPair: TJSONPair;
  LBool: TJSONBool;
  // S: string;

  function ExtractObject(const Ident: string; const Container: TJSONObject): TJSONObject; overload;
  var
    p: TJSONPair;
  begin
    Result := nil;
    p := Container.Get(Ident);
    if Assigned(p) then
      if (p.JsonValue is TJSONObject) then
        Result := p.JsonValue as TJSONObject;
  end;

  function ExtractObject(const Index: Integer; const Container: TJSONArray): TJSONObject; overload;
  var
    v: TJSONValue;
  begin
    Result := nil;
    v := Container.Items[Index];
    if Assigned(v) then
      if (v IS TJSONObject) then
        Result := v as TJSONObject;
  end;

  function ExtractArray(const Ident: string; const Container: TJSONObject): TJSONArray;
  var
    p: TJSONPair;
  begin
    Result := nil;
    p := Container.Get(Ident);
    if Assigned(p) then
      if (p.JsonValue is TJSONArray) then
        Result := p.JsonValue as TJSONArray;
  end;

begin
  ResetToDefaults;

  pair := AJSONObject.Get('method');
  if Assigned(pair) then
    FMethod := RESTRequestMethodFromString(pair.JsonValue.Value);

  pair := AJSONObject.Get('url');
  if Assigned(pair) then
    FURL := pair.JsonValue.Value;

  pair := AJSONObject.Get('resource');
  if Assigned(pair) then
    FResource := pair.JsonValue.Value;

  pair := AJSONObject.Get('contenttype');
  if Assigned(pair) then
    FContentType := pair.JsonValue.Value;

  // pair := AJSONObject.Get('auth');
  // if Assigned(pair) then
  auth := ExtractObject('auth', AJSONObject);
  if Assigned(auth) then
  begin
    // S := SimpleDecryptStr(pair.JsonValue.Value, AUTH_CRYPTO_VALUE);
    // S:= pair.JsonValue.Value;
    // auth := (TJSONObject.ParseJSONValue(S) AS TJSONObject);

    pair := auth.Get('method');
    if Assigned(pair) then
      FAuthMethod := RESTAuthMethodFromString(pair.JsonValue.Value);
    pair := auth.Get('username');
    if Assigned(pair) then
      FAuthUsername := pair.JsonValue.Value;
    pair := auth.Get('usernamekey');
    if Assigned(pair) then
      FAuthUsernameKey := pair.JsonValue.Value;
    pair := auth.Get('password');
    if Assigned(pair) then
      FAuthPassword := pair.JsonValue.Value;
    pair := auth.Get('passwordkey');
    if Assigned(pair) then
      FAuthPasswordKey := pair.JsonValue.Value;
    pair := auth.Get('clientid');
    if Assigned(pair) then
      FClientID := pair.JsonValue.Value;
    pair := auth.Get('clientsecret');
    if Assigned(pair) then
      FClientSecret := pair.JsonValue.Value;
    pair := auth.Get('authcode');
    if Assigned(pair) then
      FAuthCode := pair.JsonValue.Value;
    pair := auth.Get('accesstoken');
    if Assigned(pair) then
      FAccessToken := pair.JsonValue.Value;
    pair := auth.Get('accesstokensecret');
    if Assigned(pair) then
      FAccessTokenSecret := pair.JsonValue.Value;
    pair := auth.Get('requesttoken');
    if Assigned(pair) then
      FRequestToken := pair.JsonValue.Value;
    pair := auth.Get('requesttokensecret');
    if Assigned(pair) then
      FRequestTokenSecret := pair.JsonValue.Value;
    pair := auth.Get('refreshtoken');
    if Assigned(pair) then
      FRefreshToken := pair.JsonValue.Value;
    pair := auth.Get('signaturemethod');
    if Assigned(pair) then
      FOAuth1SignatureMethod := pair.JsonValue.Value;
    pair := auth.Get('responsetype');
    if Assigned(pair) then
      FOAuth2ResponseType := pair.JsonValue.Value;
    pair := auth.Get('endpointauth');
    if Assigned(pair) then
      FEndpointAuth := pair.JsonValue.Value;
    pair := auth.Get('endpointaccesstoken');
    if Assigned(pair) then
      FEndpointAccessToken := pair.JsonValue.Value;
    pair := auth.Get('endpointrequesttoken');
    if Assigned(pair) then
      FEndpointRequestToken := pair.JsonValue.Value;
    pair := auth.Get('endpointredirect');
    if Assigned(pair) then
      FEndpointRedirect := pair.JsonValue.Value;
    pair := auth.Get('authscope');
    if Assigned(pair) then
      FAuthScope := pair.JsonValue.Value;
  end;

  (*
    --> Proxy-Settings are no more saved in the context of a single request.
    instead we treat them as part of the app-config for the rest-debugger.
  *)

  LParams := ExtractArray('parameters', AJSONObject);
  if Assigned(LParams) then
  begin
    for i := 0 to LParams.Count - 1 do
    begin
      LObject := ExtractObject(i, LParams);
      if Assigned(LObject) then
      begin
        LParameter := FCustomParams.AddItem;

        LPair := LObject.Get('name');
        if Assigned(LPair) then
          LParameter.Name := LPair.JsonValue.Value;
        LPair := LObject.Get('value');
        if Assigned(LPair) then
          LParameter.Value := LPair.JsonValue.Value;
        LPair := LObject.Get('kind');
        if Assigned(LPair) then
          LParameter.Kind := RESTRequestParameterKindFromString(LPair.JsonValue.Value);
        LPair := LObject.Get('encode');
        if Assigned(LPair) and LPair.JsonValue.TryGetValue<TJSONBool>(LBool) then
          if LBool.AsBoolean then
            LParameter.Options := LParameter.Options - [poDoNotEncode]
          else
            LParameter.Options := LParameter.Options + [poDoNotEncode];
      end;
    end;
  end;

  pair := AJSONObject.Get('body');
  if Assigned(pair) then
    TIdDecoderMIME.DecodeStream(pair.JsonValue.Value, FCustomBody);

  pair := AJSONObject.Get('datasetview');
  if Assigned(pair) then
    FDataSetView := StrToIntDef(pair.JsonValue.Value, 0);

  pair := AJSONObject.Get('rootelement');
  if Assigned(pair) then
    FRootElement := pair.JsonValue.Value;

  pair := AJSONObject.Get('nestedfields');
  if Assigned(pair) then
    FNestedFields := StrToBoolDef(pair.JsonValue.Value, False);
end;

procedure TRESTRequestParams.SaveToFile(const AFilename: string);
var
  LStream: TStringStream;
  LJSONObj: TJSONObject;
begin
  LJSONObj := AsJSONObject;
  try
    LStream := TStringStream.Create(LJSONObj.ToString);
    try
      LStream.SaveToFile(AFilename);
    finally
      LStream.Free;
    end;
  finally
    LJSONObj.Free;
  end;
end;

procedure TRESTRequestParams.LoadFromFile(const AFilename: string);
var
  LStream: TStringStream;
  LJSONObj: TJSONObject;
begin
  LStream := TStringStream.Create;
  try
    LStream.LoadFromFile(AFilename);
    LJSONObj := TJSONObject.ParseJSONValue(LStream.DataString) AS TJSONObject;
    try
      FromJSONObject(LJSONObj);
    finally
      LJSONObj.Free;
    end;
  finally
    LStream.Free;
  end;
end;

procedure TRESTRequestParams.SetResource(const AValue: string);
begin
  if (AValue <> FResource) then
  begin
    // FCustomParams.FromString(FResource, AValue);
    FCustomParams.CreateURLSegmentsFromString(AValue);

    FResource := AValue;
  end;
end;

function TRESTRequestParams.ToString: string;
var
  S: string;
begin
  Result := FURL;
  if (FResource <> '') then
  begin
    /// just a minimal fixing of the data for display
    while EndsText('/', Result) do
      Delete(Result, Length(Result), 1);
    S := FResource;
    while StartsText('/', S) do
      Delete(S, 1, 1);

    Result := Result + ' + --> ' + S;
  end;
end;

end.
